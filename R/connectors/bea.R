# BEA regional GDP -- SAGDP (annual) and SQGDP (quarterly).
#
# Replaces two readers that each matched a zip member by its exact filename,
# vintage included:
#
#   "SAGDP9__ALL_AREAS_1997_2024.csv"
#   "SQGDP9__ALL_AREAS_2005_2025.csv"
#
# BEA puts the data's year range in the filename, so both strings change with
# every release. When they stopped matching, `[1]` returned `NA`, the readers
# returned an empty tibble, `safe_left_join()` silently dropped the join, and
# `gdp_growth_index` plus the `incentives_gdp` denominator went NA with no
# warning (docs/refactor_plan.md F-06).
#
# That is not a future risk. Measured against live BEA on 2026-10-08, both
# filenames are ALREADY stale:
#
#   staged SAGDP9__ALL_AREAS_1997_2024.csv  ->  live ..._1997_2025.csv
#   staged SQGDP9__ALL_AREAS_2005_2025.csv  ->  live ..._2005_2026.csv
#
# Nothing had broken yet only because the committed zips are the old vintage.
# The next refresh would have emptied both indicators.
#
# A third hard-coding sat alongside them: the annual reader selected the column
# `x2024`, which is `janitor::clean_names()`'s rendering of BEA's `2024`. The
# column name carried the vintage too, so the file and the column had to be
# updated in lockstep -- exactly the coupling that hid the staleness of the
# Census population denominator in F-11.

BEA_ZIP_BASE_URL <- "https://apps.bea.gov/regional/zip"

#' URL of a BEA regional zip
#'
#' Verified live 2026-10-08 for both `SAGDP.zip` and `SQGDP.zip`. SQGDP was
#' previously read from disk but registered nowhere, so it had no refresh path
#' at all.
#'
#' @param name Zip filename, e.g. `"SAGDP.zip"`.
#' @return The URL as a string.
#' @export
bea_zip_url <- function(name) {
  paste0(BEA_ZIP_BASE_URL, "/", name)
}

#' Locate one member of a BEA zip by prefix
#'
#' Matches on the table prefix (`SAGDP9__ALL_AREAS_`) and ignores the year range
#' that follows it. The double underscore matters: `SAGDP1__` and `SAGDP11__`
#' are different tables that would otherwise collide with a looser pattern.
#'
#' Ambiguity is an error rather than a silent `[1]`. Two members matching means
#' BEA has changed the layout, and picking the first would be a guess.
#'
#' @param zip_path Path to the zip.
#' @param prefix Member filename prefix.
#' @return The member's name inside the archive.
#' @export
bea_zip_member <- function(zip_path, prefix) {
  members <- utils::unzip(zip_path, list = TRUE)$Name
  hits <- members[startsWith(basename(members), prefix)]
  hits <- hits[grepl("[.]csv$", hits)]

  if (length(hits) == 0) {
    rlang::abort(c(
      glue::glue("No member starting with `{prefix}` in {basename(zip_path)}."),
      i = glue::glue("The archive holds {length(members)} members."),
      i = "BEA has changed its table layout; refusing to guess which file to read."
    ))
  }
  if (length(hits) > 1) {
    rlang::abort(c(
      glue::glue("{length(hits)} members start with `{prefix}` in {basename(zip_path)}."),
      x = paste(hits, collapse = ", "),
      i = "Picking the first would be a guess; the prefix needs narrowing."
    ))
  }
  hits[[1]]
}

#' Read one member of a BEA zip
#'
#' @param zip_path Path to the zip.
#' @param prefix Member filename prefix.
#' @return A tibble with `janitor`-cleaned names.
#' @export
bea_read_member <- function(zip_path, prefix) {
  member <- bea_zip_member(zip_path, prefix)
  temp_dir <- tempfile()
  fs::dir_create(temp_dir)
  on.exit(fs::dir_delete(temp_dir), add = TRUE)
  utils::unzip(zip_path, files = member, exdir = temp_dir, junkpaths = TRUE)

  path <- fs::path(temp_dir, basename(member))
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE) %>%
    janitor::clean_names() %>%
    tibble::as_tibble()
}

#' Parse a BEA value column, honouring its footnote codes
#'
#' BEA marks cells it will not give a number for with a parenthesised code
#' rather than leaving them blank: `(D)` withheld to avoid disclosure, `(L)`
#' below a reporting threshold, `(N)` / `(NA)` not available, `(NM)` not
#' meaningful, `(T)` not shown.
#'
#' Those become `NA` deliberately. The previous code relied on
#' `readr::parse_number()` *failing* on them, which produced the right answer
#' by accident while emitting dozens of parse warnings per run -- noise that
#' would hide a real parse problem. Suppressed is unknown, not zero, which is
#' the same rule the QCEW connector applies.
#'
#' @param x A character vector of BEA values.
#' @return A numeric vector.
#' @export
bea_parse_values <- function(x) {
  chr <- trimws(as.character(x))
  footnote <- grepl("^\\(.*\\)$", chr)
  chr[footnote] <- NA_character_
  suppressWarnings(readr::parse_number(chr))
}

#' The most recent annual column in a BEA table
#'
#' BEA labels year columns with the bare year, which `clean_names()` renders as
#' `x1997` ... `x2025`. Discovered rather than pinned, so the column cannot
#' drift away from the file that supplies it.
#'
#' @param column_names Cleaned column names.
#' @return A list of `column` and `year`.
#' @export
bea_latest_year_column <- function(column_names) {
  year_cols <- grep("^x[0-9]{4}$", column_names, value = TRUE)
  if (length(year_cols) == 0) {
    rlang::abort(
      "No year columns (x<year>) in the BEA table; its layout has changed."
    )
  }
  years <- as.integer(sub("^x", "", year_cols))
  idx <- which.max(years)
  list(column = year_cols[[idx]], year = years[[idx]])
}

#' Annual state GDP, all-industry total, latest year
#'
#' @param raw_dir Raw data directory.
#' @return A tibble of `GeoFIPS`, `GeoName`, `Unit`, `Description`, `gdp`,
#'   `gdp_year`; or `NULL` if the zip is not staged.
#' @export
load_bea_annual_gdp <- function(raw_dir) {
  zip_path <- fs::path(raw_dir, "remote", "SAGDP.zip")
  if (!fs::file_exists(zip_path)) {
    return(NULL)
  }

  gdp <- bea_read_member(zip_path, "SAGDP9__ALL_AREAS_")
  latest <- bea_latest_year_column(names(gdp))

  # BEA pads this description with a trailing space. Trimmed rather than
  # matched literally, so a tidy-up on their side does not empty the table.
  out <- gdp %>%
    dplyr::filter(trimws(.data$description) == "All industry total") %>%
    dplyr::transmute(
      GeoFIPS = .data$geo_fips,
      GeoName = .data$geo_name,
      Unit = .data$unit,
      Description = trimws(.data$description),
      gdp = bea_parse_values(.data[[latest$column]]),
      gdp_year = latest$year
    )

  if (nrow(out) == 0) {
    rlang::abort(c(
      "BEA annual GDP: no rows matched `All industry total`.",
      i = "The Description values have changed; the denominator would silently go NA."
    ))
  }
  out
}

#' Quarterly state GDP growth over one, five and ten years
#'
#' The quarter columns were already discovered rather than pinned, so only the
#' member lookup changes here.
#'
#' @param raw_dir Raw data directory.
#' @param states State lookup with `state` and `abbr`.
#' @return A tibble of `state` and `gdp_growth_index`; or `NULL` if the zip is
#'   not staged.
#' @export
load_bea_quarterly_gdp_growth <- function(raw_dir, states) {
  zip_path <- fs::path(raw_dir, "remote", "SQGDP.zip")
  if (!fs::file_exists(zip_path)) {
    return(NULL)
  }

  gdp_raw <- bea_read_member(zip_path, "SQGDP9__ALL_AREAS_")

  quarter_cols <- names(gdp_raw)[grepl("^x[0-9]{4}_q[1-4]$", names(gdp_raw))]
  if (length(quarter_cols) == 0) {
    rlang::abort(
      "BEA quarterly GDP: no quarter columns (x<year>_q<n>); the layout has changed."
    )
  }

  gdp_raw <- gdp_raw %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(quarter_cols), bea_parse_values))

  quarter_info <- stringr::str_match(quarter_cols, "^x([0-9]{4})_q([1-4])$")
  years <- as.integer(quarter_info[, 2])
  quarters <- as.integer(quarter_info[, 3])
  latest_idx <- order(years, quarters, decreasing = TRUE)[1]
  latest_col <- quarter_cols[latest_idx]
  latest_year <- years[latest_idx]
  latest_quarter <- quarters[latest_idx]

  prior_col <- function(offset) {
    target <- paste0("x", latest_year - offset, "_q", latest_quarter)
    if (target %in% quarter_cols) target else NA_character_
  }
  col_1yr <- prior_col(1)
  col_5yr <- prior_col(5)
  col_10yr <- prior_col(10)

  # Durable-goods manufacturing. Carried over verbatim; if BEA ever restates
  # this code the filter empties, which the check below turns into an error
  # rather than a silently NA indicator.
  gdp_ind <- gdp_raw %>%
    dplyr::filter(.data$industry_classification == "321,327-339") %>%
    dplyr::mutate(
      gdp_growth_1yr = if (is.na(col_1yr)) NA_real_ else .data[[latest_col]] / .data[[col_1yr]] - 1,
      gdp_growth_5yr = if (is.na(col_5yr)) NA_real_ else .data[[latest_col]] / .data[[col_5yr]] - 1,
      gdp_growth_10yr = if (is.na(col_10yr)) NA_real_ else .data[[latest_col]] / .data[[col_10yr]] - 1
    ) %>%
    dplyr::select(dplyr::all_of(c(
      "geo_name", "gdp_growth_1yr", "gdp_growth_5yr", "gdp_growth_10yr"
    ))) %>%
    dplyr::inner_join(states, by = c("geo_name" = "state"))

  if (nrow(gdp_ind) == 0) {
    rlang::abort(c(
      "BEA quarterly GDP: no rows matched industry classification `321,327-339`.",
      i = glue::glue("Latest period read was {latest_year} Q{latest_quarter}."),
      i = "gdp_growth_index would silently go NA for every state."
    ))
  }

  growth_cols <- c("gdp_growth_1yr", "gdp_growth_5yr", "gdp_growth_10yr")

  gdp_ind %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(growth_cols), scale_minmax)) %>%
    dplyr::mutate(gdp_growth_index = rowmean_index(dplyr::select(., dplyr::all_of(growth_cols)))) %>%
    dplyr::transmute(state = .data$geo_name, gdp_growth_index)
}
