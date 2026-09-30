# BLS Quarterly Census of Employment and Wages -- industry-slice connector.
#
# Replaces the county-by-county design that never ran. See
# docs/bls_qcew_options.md for the measurements behind every choice here; the
# short version is that the old approach made ~6,200 requests for ~440 MB,
# depended on a package that does not exist (`blsQCEW`) plus one archived from
# CRAN (`blsAPI`), and rolled county data up to state, which recovers only
# about 45% of the employment the same industry reports at state level.
#
# This fetches one file per 4-digit NAICS code -- each carries every geography
# for that industry -- and reads the state rows directly.
#
# Three decisions were taken by the index owner on 2026-09-17 and are encoded
# here rather than left implicit:
#
#   1. Suppressed cells are NA, not zero. QCEW withholds any cell that would
#      disclose an individual employer, flags it `disclosure_code == "N"`, and
#      then reports the value as literal `0`. Summing that with na.rm = TRUE
#      reads "withheld" as "none".
#   2. The bundle is read at 4-digit depth. State-level suppression across the
#      bundle is 16% at 4-digit (measured 2025 annual, 72 of 459 cells) against
#      84% at county level.
#   3. PEA-level workforce is not attempted. 181 of 410 PEAs would have no
#      disclosed bundle employment at all, so PEAs keep inheriting their
#      state's value by join.

QCEW_BASE_URL <- "https://data.bls.gov/cew/data/api"
QCEW_AREA_TITLES_URL <- "https://data.bls.gov/cew/doc/titles/area/area_titles.csv"

# Private ownership. QCEW splits federal/state/local/private government
# ownership into separate rows; the index measures private employment.
QCEW_OWN_PRIVATE <- "5"

# Aggregation levels, from QCEW's agglvl_titles. These are the two the
# connector reads, and they are asserted rather than inferred because picking
# the wrong one silently changes the geography or the industry depth.
QCEW_AGGLVL_STATE_NAICS4 <- "56" # state x 4-digit NAICS x ownership
QCEW_AGGLVL_STATE_TOTAL <- "51" # state x all industries x ownership

# QCEW's pseudo-code for "all industries", used as the share denominator.
QCEW_ALL_INDUSTRIES <- "10"

#' URL of one QCEW industry slice
#'
#' An industry slice contains every geography for that NAICS code -- national,
#' state, MSA and county -- so one request serves a state-level need that the
#' previous design met with ~3,100 county requests per quarter.
#'
#' @param year Four-digit year, e.g. `"2025"`.
#' @param qtr Quarter `"1"`..`"4"`, or `"a"` for the annual averages file.
#' @param industry_code NAICS code, or `"10"` for all industries.
#' @return The URL as a string.
#' @export
qcew_industry_url <- function(year, qtr, industry_code) {
  sprintf(
    "%s/%s/%s/industry/%s.csv",
    QCEW_BASE_URL, as.character(year), as.character(qtr), as.character(industry_code)
  )
}

#' Derive the 4-digit NAICS parents of a 6-digit bundle
#'
#' The bundle is defined at 6-digit detail in `scripts/07_process_data.R`, but
#' read at 4-digit depth -- decision (2) above. Sorted so the fetch order, and
#' therefore the manifest, is deterministic.
#'
#' @param codes Character vector of 6-digit NAICS codes.
#' @return Sorted unique 4-digit parents.
#' @export
qcew_naics4 <- function(codes) {
  codes <- as.character(codes)
  if (any(is.na(codes) | nchar(codes) < 4)) {
    rlang::abort("qcew_naics4(): every code must be at least 4 characters.")
  }
  sort(unique(substr(codes, 1, 4)))
}

#' Read QCEW's published area titles
#'
#' @param path Local copy of `area_titles.csv`.
#' @return A tibble of `area_fips`, `area_title`, deduplicated.
#' @export
qcew_read_area_titles <- function(path) {
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  ) %>%
    dplyr::distinct(.data$area_fips, .data$area_title)
}

#' Map statewide `area_fips` to the 50 state names
#'
#' Derived from the publisher's own titles rather than a hard-coded FIPS list,
#' so the mapping cannot drift from what QCEW actually serves. The join to
#' `state.name` is also what excludes the non-states that share the statewide
#' FIPS shape: `11000` District of Columbia (excluded index-wide, see
#' refactor_plan.md F-15), `72000` Puerto Rico, `78000` Virgin Islands, and
#' `57000` "Federal Bureau of Investigation -- undesignated".
#'
#' @param area_titles As returned by [qcew_read_area_titles()].
#' @return A tibble of `area_fips`, `state`, `abbr`, one row per state.
#' @export
qcew_state_lookup <- function(area_titles) {
  lookup <- area_titles %>%
    dplyr::filter(grepl("^[0-9]{2}000$", .data$area_fips)) %>%
    dplyr::mutate(state = trimws(sub("\\s*--\\s*Statewide$", "", .data$area_title))) %>%
    dplyr::filter(.data$state %in% state.name) %>%
    dplyr::transmute(
      area_fips = .data$area_fips,
      state = .data$state,
      abbr = state.abb[match(.data$state, state.name)]
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$area_fips)

  if (nrow(lookup) != 50L) {
    rlang::abort(glue::glue(
      "qcew_state_lookup(): expected 50 states, resolved {nrow(lookup)}. ",
      "QCEW's area titles have changed shape; refusing to guess the mapping."
    ))
  }
  lookup
}

#' Fetch one industry slice, tolerating an absent one
#'
#' Not every code has a slice: `335911` and `335912` return 404 at 6-digit, and
#' before the NAICS 2022 remap nine of the bundle's ten telecom codes did too.
#' A 404 is a fact about the code, not a failure, so it returns `NA` and the
#' caller records the absence.
#'
#' @param year,qtr,industry_code Passed to [qcew_industry_url()].
#' @param cache_dir Directory for the cached CSV.
#' @param refresh Re-download even when a cached copy exists.
#' @param offline Never touch the network: return a cached copy if there is one
#'   and `NA` otherwise. This is what `SKIP_DATA_DOWNLOADS=TRUE` sets, and it is
#'   what keeps CI hermetic -- see the brief's no-network rule and
#'   docs/refactor_plan.md F-11.
#' @return Path to the cached file, or `NA_character_` if the slice is absent.
#' @export
qcew_fetch_slice <- function(year, qtr, industry_code, cache_dir, refresh = FALSE,
                             offline = FALSE) {
  dest <- fs::path(cache_dir, sprintf("%s_%s_%s.csv", year, qtr, industry_code))

  if (fs::file_exists(dest) && !isTRUE(refresh)) {
    return(as.character(dest))
  }
  if (isTRUE(offline)) {
    return(NA_character_)
  }
  fs::dir_create(cache_dir, recurse = TRUE)

  url <- qcew_industry_url(year, qtr, industry_code)
  ok <- tryCatch(
    {
      utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )

  # BLS serves an HTML error page rather than a 404 status for some absent
  # slices, so size is checked as well as the download result.
  if (!ok || !fs::file_exists(dest) || fs::file_size(dest) < 100) {
    if (fs::file_exists(dest)) fs::file_delete(dest)
    return(NA_character_)
  }
  as.character(dest)
}

#' Read a QCEW slice
#'
#' Everything is read as character and coerced explicitly. QCEW mixes blank,
#' `"0"` and flagged values in the same column, and letting `readr` guess makes
#' the parse depend on which rows happen to appear first in a given vintage.
#'
#' @param path Path to a slice CSV.
#' @return A tibble.
#' @export
qcew_read_slice <- function(path) {
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )
}

#' State-level employment rows from a slice, with suppression honoured
#'
#' This is where decision (1) lives. `disclosure_code == "N"` marks a withheld
#' cell, and QCEW reports those as `0`; they become `NA` here so that downstream
#' code cannot mistake "withheld" for "none".
#'
#' @param slice A slice as returned by [qcew_read_slice()].
#' @param agglvl Aggregation level to keep -- one of the `QCEW_AGGLVL_*`
#'   constants.
#' @param own_code Ownership code to keep.
#' @return A tibble of `area_fips`, `industry_code`, `employment`, `suppressed`.
#' @export
qcew_state_rows <- function(slice, agglvl, own_code = QCEW_OWN_PRIVATE) {
  required <- c(
    "area_fips", "own_code", "industry_code", "agglvl_code",
    "disclosure_code", "annual_avg_emplvl"
  )
  missing <- setdiff(required, names(slice))
  if (length(missing) > 0) {
    rlang::abort(glue::glue(
      "qcew_state_rows(): slice is missing {paste(missing, collapse = ', ')}. ",
      "QCEW's schema has changed; refusing to guess which column carries ",
      "employment."
    ))
  }

  slice %>%
    dplyr::filter(
      .data$own_code == !!own_code,
      .data$agglvl_code == !!agglvl
    ) %>%
    dplyr::mutate(
      suppressed = !is.na(.data$disclosure_code) & .data$disclosure_code == "N",
      employment = suppressWarnings(as.numeric(.data$annual_avg_emplvl)),
      # Withheld is unknown, not zero.
      employment = dplyr::if_else(.data$suppressed, NA_real_, .data$employment)
    ) %>%
    dplyr::select(
      dplyr::all_of(c("area_fips", "industry_code", "employment", "suppressed"))
    )
}

#' Aggregate bundle slices to one row per state
#'
#' A state can be missing from a slice in two different ways, and they are not
#' the same thing:
#'
#' * **suppressed** -- the row exists and is flagged `N`. The employment is
#'   unknown, and is counted in `n_suppressed`.
#' * **absent** -- no row at all, which in QCEW means no establishments were
#'   reported in that industry-state. Counted in `n_absent` and contributes
#'   zero.
#'
#' A third gap is deliberately *not* counted here: a NAICS code QCEW does not
#' publish at any geography. That is a property of the period rather than of a
#' state, so [qcew_fetch_period()] reports it separately in `absent_codes` and
#' this function only ever sees codes that were published. `n_disclosed +
#' n_suppressed + n_absent` therefore equals `length(slices)`, not the size of
#' the bundle as defined.
#'
#' `employment` sums only what is disclosed, and is `NA` when a state has no
#' disclosed cell at all -- otherwise a wholly-suppressed state would report 0
#' and read as "no electro-industrial employment".
#'
#' @param slices Named list of slices, names being the NAICS codes.
#' @param lookup As returned by [qcew_state_lookup()].
#' @return One row per state: `state`, `abbr`, `employment`, `n_disclosed`,
#'   `n_suppressed`, `n_absent`.
#' @export
#' One row per state x NAICS code, absences made explicit
#'
#' The long intermediate behind [qcew_bundle_state_employment()], exported in
#' its own right because [qcew_matched_growth()] needs to compare two periods
#' cell by cell rather than on their totals.
#'
#' @param slices Named list of slices, names being the NAICS codes.
#' @param lookup As returned by [qcew_state_lookup()].
#' @return `state`, `abbr`, `code`, `employment` (NA if withheld or absent),
#'   `suppressed`, `absent`.
#' @export
qcew_bundle_cells <- function(slices, lookup) {
  codes <- names(slices)
  if (is.null(codes) || any(!nzchar(codes))) {
    rlang::abort("qcew_bundle_cells(): `slices` must be named by NAICS code.")
  }

  cells <- purrr::imap_dfr(slices, function(slice, code) {
    qcew_state_rows(slice, QCEW_AGGLVL_STATE_NAICS4) %>%
      dplyr::mutate(code = code)
  })

  # Expand to every state x code pair so absences are explicit rather than
  # simply missing from the sum.
  tidyr::expand_grid(area_fips = lookup$area_fips, code = codes) %>%
    dplyr::left_join(
      cells %>% dplyr::select(dplyr::all_of(c("area_fips", "code", "employment", "suppressed"))),
      by = c("area_fips", "code")
    ) %>%
    # `absent` must be derived before `suppressed` is filled: an unmatched row
    # is what leaves `suppressed` NA. Filling it to FALSE afterwards makes
    # disclosed / suppressed / absent mutually exclusive, so callers do not
    # need `na.rm` to count them and cannot conflate "no row" with "withheld".
    dplyr::mutate(
      absent = is.na(.data$suppressed),
      suppressed = !is.na(.data$suppressed) & .data$suppressed
    ) %>%
    dplyr::inner_join(lookup, by = "area_fips") %>%
    dplyr::select(dplyr::all_of(c(
      "state", "abbr", "code", "employment", "suppressed", "absent"
    ))) %>%
    dplyr::arrange(.data$state, .data$code)
}

#' @rdname qcew_bundle_cells
#' @export
qcew_bundle_state_employment <- function(slices, lookup) {
  qcew_bundle_cells(slices, lookup) %>%
    dplyr::group_by(.data$state, .data$abbr) %>%
    dplyr::summarize(
      n_disclosed = sum(!is.na(.data$employment)),
      n_suppressed = sum(.data$suppressed),
      n_absent = sum(.data$absent),
      employment = dplyr::if_else(
        sum(!is.na(.data$employment)) == 0L,
        NA_real_,
        sum(.data$employment, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    dplyr::select(dplyr::all_of(c(
      "state", "abbr", "employment", "n_disclosed", "n_suppressed", "n_absent"
    ))) %>%
    dplyr::arrange(.data$state)
}

#' Growth in bundle employment on a like-for-like basket
#'
#' Comparing two periods' bundle *totals* silently compares different baskets,
#' because QCEW's suppression is decided per period. Nevada is the worked
#' example: NAICS 3359 reported 12,513 in 2022 and was withheld in 2025, so the
#' naive comparison shows a 62% collapse while every other Nevada code is flat
#' or rising. Measured over the 2022-2025 pair, 17 of 50 states change their
#' disclosure pattern, and those states show 2.6x the spread of the 33 that do
#' not -- so the naive figure partly measures disclosure rather than employment.
#'
#' This restricts each state to the codes disclosed in **both** periods and
#' computes growth from those. `n_matched` and `n_dropped` record the size of
#' the basket, so a growth rate resting on very few codes is visible rather than
#' implied.
#'
#' Note this fixes comparability, not completeness: a matched basket still omits
#' whatever was withheld in either period.
#'
#' @param current,baseline Cell tables from [qcew_bundle_cells()].
#' @return `state`, `workforce_growth`, `matched_employment`,
#'   `matched_employment_baseline`, `n_matched`, `n_dropped`.
#' @export
qcew_matched_growth <- function(current, baseline) {
  dplyr::inner_join(
    current %>% dplyr::select(dplyr::all_of(c("state", "code", "employment"))),
    baseline %>% dplyr::select(dplyr::all_of(c("state", "code", "employment"))),
    by = c("state", "code"), suffix = c("_cur", "_base")
  ) %>%
    dplyr::group_by(.data$state) %>%
    dplyr::summarize(
      n_matched = sum(!is.na(.data$employment_cur) & !is.na(.data$employment_base)),
      n_dropped = sum(is.na(.data$employment_cur) | is.na(.data$employment_base)),
      matched_employment = sum(
        .data$employment_cur[!is.na(.data$employment_cur) & !is.na(.data$employment_base)]
      ),
      matched_employment_baseline = sum(
        .data$employment_base[!is.na(.data$employment_cur) & !is.na(.data$employment_base)]
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # Growth needs a positive base and at least one matched code.
      workforce_growth = dplyr::if_else(
        .data$n_matched > 0L & .data$matched_employment_baseline > 0,
        (.data$matched_employment - .data$matched_employment_baseline) /
          .data$matched_employment_baseline,
        NA_real_
      ),
      matched_employment = dplyr::if_else(.data$n_matched > 0L, .data$matched_employment, NA_real_),
      matched_employment_baseline = dplyr::if_else(
        .data$n_matched > 0L, .data$matched_employment_baseline, NA_real_
      )
    ) %>%
    dplyr::select(dplyr::all_of(c(
      "state", "workforce_growth", "matched_employment",
      "matched_employment_baseline", "n_matched", "n_dropped"
    ))) %>%
    dplyr::arrange(.data$state)
}

#' Total private employment per state, the share denominator
#'
#' @param slice The `industry_code == "10"` slice.
#' @param lookup As returned by [qcew_state_lookup()].
#' @return A tibble of `state`, `total_employment`.
#' @export
qcew_state_totals <- function(slice, lookup) {
  qcew_state_rows(slice, QCEW_AGGLVL_STATE_TOTAL) %>%
    dplyr::filter(.data$industry_code == QCEW_ALL_INDUSTRIES) %>%
    dplyr::inner_join(lookup, by = "area_fips") %>%
    dplyr::transmute(state = .data$state, total_employment = .data$employment) %>%
    dplyr::arrange(.data$state)
}

#' Build `workforce_share` and `workforce_growth`
#'
#' `workforce_share` is bundle employment as a percentage of total private
#' employment.
#'
#' `workforce_growth` is the **proportional change in bundle employment over
#' the baseline span**, computed on a like-for-like basket by
#' [qcew_matched_growth()]. A value of `0.12` means the electro-industrial
#' bundle grew 12% over the span.
#'
#' This corrects the upstream arithmetic, which divided the same numerator by
#' *current total* employment. That produced a percentage-point change in
#' `workforce_share`'s numerator rather than a growth rate -- a coherent
#' quantity, but not the one the indicator is named for, and one whose
#' magnitude was governed by the size of a state's whole private economy rather
#' than by how fast its electro-industrial base was growing. Changed on
#' 2026-09-29 at the index owner's explicit instruction; see
#' docs/methodology.md and docs/refactor_plan.md F-23.
#'
#' @param bundle Current-period bundle employment, from
#'   [qcew_bundle_state_employment()].
#' @param totals Current-period totals, from [qcew_state_totals()].
#' @param growth Matched growth from [qcew_matched_growth()], or `NULL` to leave
#'   `workforce_growth` as `NA`.
#' @return A tibble of `state`, `workforce_share`, `workforce_growth` and the
#'   per-state coverage counts.
#' @export
qcew_workforce_indicators <- function(bundle, totals, growth = NULL) {
  out <- bundle %>%
    dplyr::left_join(totals, by = "state") %>%
    dplyr::mutate(
      workforce_share = dplyr::if_else(
        !is.na(.data$total_employment) & .data$total_employment > 0,
        .data$employment / .data$total_employment * 100,
        NA_real_
      )
    )

  growth_cols <- c(
    "workforce_growth", "matched_employment",
    "matched_employment_baseline", "n_matched", "n_dropped"
  )
  if (is.null(growth)) {
    for (col in growth_cols) {
      out[[col]] <- if (col %in% c("n_matched", "n_dropped")) NA_integer_ else NA_real_
    }
  } else {
    out <- out %>%
      dplyr::left_join(
        growth %>% dplyr::select(dplyr::all_of(c("state", growth_cols))),
        by = "state"
      )
  }

  out %>%
    dplyr::select(dplyr::all_of(c(
      "state", "workforce_share", "workforce_growth",
      "employment", "total_employment",
      "matched_employment", "matched_employment_baseline",
      "n_matched", "n_dropped",
      "n_disclosed", "n_suppressed", "n_absent"
    )))
}

#' Find the most recent published annual period
#'
#' Deliberately discovered rather than hard-coded. A pinned year is exactly the
#' defect F-08 turned out to be for BNEF: it goes stale silently, and once the
#' pinned period stops existing the filter matches nothing without erroring.
#' Probes the small all-industries slice, newest first.
#'
#' @param cache_dir Directory for cached slices.
#' @param from Newest year to consider. Defaults to the current calendar year.
#' @param max_back How many years to walk back before giving up.
#' @param offline Passed to [qcew_fetch_slice()].
#' @return The year as a string, or `NA_character_` if none is reachable.
#' @export
qcew_latest_annual_year <- function(cache_dir, from = as.integer(format(Sys.Date(), "%Y")),
                                    max_back = 5L, offline = FALSE) {
  for (y in seq(from, from - max_back)) {
    path <- qcew_fetch_slice(
      as.character(y), "a", QCEW_ALL_INDUSTRIES, cache_dir,
      offline = offline
    )
    if (!is.na(path)) {
      return(as.character(y))
    }
  }
  NA_character_
}

#' Fetch and assemble the whole bundle for one period
#'
#' @param codes4 4-digit NAICS codes, from [qcew_naics4()].
#' @param year,qtr Period to fetch.
#' @param cache_dir Directory for cached slices.
#' @param lookup As returned by [qcew_state_lookup()].
#' @param refresh,offline Passed to [qcew_fetch_slice()].
#' @return A list of `bundle`, `totals`, `absent_codes`.
#' @export
qcew_fetch_period <- function(codes4, year, qtr, cache_dir, lookup, refresh = FALSE,
                              offline = FALSE) {
  paths <- vapply(
    codes4,
    function(code) {
      qcew_fetch_slice(year, qtr, code, cache_dir, refresh = refresh, offline = offline)
    },
    character(1)
  )
  absent <- names(paths)[is.na(paths)]
  present <- paths[!is.na(paths)]

  if (length(present) == 0) {
    rlang::abort(glue::glue(
      "qcew_fetch_period(): no bundle slice could be retrieved for {year}/{qtr}. ",
      "Every one of {length(codes4)} codes was absent or unreachable."
    ))
  }

  slices <- lapply(present, qcew_read_slice)
  names(slices) <- names(present)

  total_path <- qcew_fetch_slice(
    year, qtr, QCEW_ALL_INDUSTRIES, cache_dir,
    refresh = refresh, offline = offline
  )
  if (is.na(total_path)) {
    rlang::abort(glue::glue(
      "qcew_fetch_period(): the all-industries denominator slice is missing for ",
      "{year}/{qtr}; workforce_share cannot be computed."
    ))
  }

  cells <- qcew_bundle_cells(slices, lookup)
  list(
    cells = cells,
    bundle = qcew_bundle_state_employment(slices, lookup),
    totals = qcew_state_totals(qcew_read_slice(total_path), lookup),
    absent_codes = absent
  )
}
