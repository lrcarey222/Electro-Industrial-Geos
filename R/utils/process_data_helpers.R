empty_facility_tbl <- function() {
  tibble::tibble(
    name = character(),
    tech = character(),
    cat = character(),
    size = numeric(),
    Latitude = numeric(),
    Longitude = numeric(),
    state_abbr = character(),
    unit = character()
  )
}

median_scurve <- function(x, gamma = 0.5) {
  r <- dplyr::percent_rank(x)
  (r^gamma) / (r^gamma + (1 - r)^gamma)
}

build_state_facility_rollup <- function(facility_data) {
  facility_data %>%
    dplyr::filter(!is.na(.data$state_abbr)) %>%
    dplyr::group_by(.data$state_abbr, .data$cat, .data$unit) %>%
    dplyr::summarize(size = sum(.data$size, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = .data$cat, values_from = .data$size)
}

build_pea_facility_rollup <- function(facility_data, pea_sf, pea_pop) {
  facility_data %>%
    dplyr::filter(!is.na(.data$Latitude), !is.na(.data$Longitude)) %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
    sf::st_join(pea_sf, join = sf::st_intersects, left = FALSE) %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(!is.na(.data$economic_area), !is.na(.data$state_abbr)) %>%
    dplyr::group_by(.data$economic_area, .data$cat, .data$unit) %>%
    dplyr::summarize(size = sum(.data$size, na.rm = TRUE), .groups = "drop") %>%
    dplyr::left_join(pea_pop, by = c("economic_area" = "PEA_Name")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(size_pop = .data$size / .data$pop) %>%
    dplyr::group_by(.data$cat) %>%
    dplyr::mutate(
      size_perc = scale_minmax(.data$size_pop),
      size_perc_scurve = median_scurve(.data$size_pop)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$economic_area, .data$cat, .data$size_perc_scurve) %>%
    tidyr::pivot_wider(names_from = .data$cat, values_from = .data$size_perc_scurve)
}

#' Locate the Cents/kWh column for a customer sector in an EIA-861M sheet
#'
#' The sheet repeats a four-column block -- Revenue, Sales, Customers, Price --
#' once per customer class, and names the class only in a merged cell on row 1:
#'
#'   row 1: . . . . RESIDENTIAL . . . COMMERCIAL . . . INDUSTRIAL . . . TOTAL ...
#'   row 3: Year Month State Data-Status Thousand-Dollars Megawatthours Count Cents/kWh ...
#'
#' So there are five identically named Cents/kWh columns and taking the first
#' silently yields RESIDENTIAL. That is exactly what happened -- see
#' docs/refactor_plan.md F-04. Resolve the sector from row 1 instead, and fail
#' loudly rather than guess if the layout changes.
#'
#' @param path Path to the workbook.
#' @param sheet Sheet index or name.
#' @param sector Customer class as it appears on row 1.
#' @return Integer column index of that sector's Cents/kWh column.
#' @export
eia_price_column_index <- function(path, sheet = 1, sector = "INDUSTRIAL") {
  # Anchor the range at column 1. `cell_rows(1)` lets readxl trim leading empty
  # columns, and row 1 of this sheet begins with four blanks -- which silently
  # shifted every index left by four and selected COMMERCIAL instead of
  # INDUSTRIAL. cell_limits() with an explicit start column keeps the blanks, so
  # the position of a label is its true spreadsheet column.
  header <- suppressWarnings(readxl::read_excel(
    path,
    sheet = sheet,
    range = readxl::cell_limits(c(1L, 1L), c(1L, NA)),
    col_names = FALSE,
    .name_repair = "minimal"
  ))
  labels <- toupper(trimws(as.character(unlist(header[1, ], use.names = FALSE))))
  hit <- which(!is.na(labels) & labels == toupper(sector))

  if (length(hit) != 1) {
    rlang::abort(glue::glue(
      "Could not locate a unique '{sector}' sector header on row 1 of ",
      "{basename(path)} sheet {sheet} ({length(hit)} match(es) found). ",
      "The EIA-861M layout may have changed; refusing to guess a price column."
    ))
  }

  # Price is the fourth column of the sector's block.
  as.integer(hit[1] + 3L)
}

load_remote_eia_sales <- function(paths, raw_dir) {
  eia_remote_path <- fs::path(raw_dir, "remote", "sales_revenue.xlsx")
  if (fs::file_exists(eia_remote_path)) {
    return(readxl::read_excel(eia_remote_path, sheet = 1, skip = 2))
  }

  cached <- tryCatch(
    download_with_cache(
      url = "https://www.eia.gov/electricity/data/eia861m/xls/sales_revenue.xlsx",
      dest_dir = paths$cache_dir,
      snapshot_date = paths$snapshot_date,
      filename = "sales_revenue.xlsx"
    ),
    error = function(e) NULL
  )

  if (is.null(cached)) {
    return(NULL)
  }

  fs::dir_create(fs::path(raw_dir, "remote"), recurse = TRUE)
  fs::file_copy(cached, eia_remote_path, overwrite = TRUE)
  readxl::read_excel(eia_remote_path, sheet = 1, skip = 2)
}

#' Most recent BloombergNEF data-centre export in a directory
#'
#' The filename carries the export date and, since 1.2.0, a version suffix --
#' "2026-07-03 - Global Data Center Live IT Capacity Database (1.5.0).xlsx".
#' Hard-coding one filename means a steward's newer drop is silently ignored,
#' which is half of docs/refactor_plan.md F-08.
#'
#' @param dir Directory holding the exports.
#' @return Path to the newest export, or NA if there is none.
#' @export
latest_bnef_export <- function(dir) {
  if (!fs::dir_exists(dir)) {
    return(NA_character_)
  }
  files <- fs::dir_ls(
    dir,
    regexp = "Global Data Center Live IT Capacity Database.*[.]xlsx$",
    type = "file",
    fail = FALSE
  )
  if (length(files) == 0) {
    return(NA_character_)
  }
  stamps <- as.Date(stringr::str_extract(basename(files), "^[0-9]{4}-[0-9]{2}-[0-9]{2}"))
  if (all(is.na(stamps))) {
    # No dated filenames: fall back to mtime rather than picking arbitrarily.
    return(as.character(files[which.max(fs::file_info(files)$modification_time)]))
  }
  as.character(files[which.max(stamps)])
}

#' Normalise a BNEF `Date` column
#'
#' Older exports carry text dates; 1.5.0 carries Excel serial numbers. Reading
#' one format and filtering on the other yields zero rows without erroring,
#' which is the other half of F-08.
#'
#' @param x The raw Date column.
#' @return A Date vector.
#' @export
bnef_normalise_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  chr <- as.character(x)
  out <- suppressWarnings(as.Date(chr))
  num <- suppressWarnings(as.numeric(chr))
  fill <- is.na(out) & !is.na(num)
  if (any(fill)) {
    out[fill] <- as.Date(num[fill], origin = "1899-12-30")
  }
  out
}

#' Resolve the forward-pipeline capacity column in a BNEF export
#'
#' Up to the 2025-08-08 export this was `Committed.Capacity.(MW)`. The 1.5.0
#' COVER sheet records that BNEF dropped it: "Dataset now includes projects
#' which are under construction but not yet online. Redundant columns
#' 'Committed Capacity' and 'Early Stage Capacity' were removed."
#' `Under.Construction.Capacity.(MW)` is the only forward-looking capacity
#' field left, and it was present in the older vintages too.
#'
#' `Other.Pipeline.Capacity.(MW)` is deliberately *excluded*. In 1.5.0 it is
#' the exact arithmetic negation of `Under.Construction.Capacity.(MW)` on all
#' 388,484 rows (verified with both openxlsx and readxl), i.e. a mirror-bar
#' chart helper rather than a data field. Min-max scaling it inverts
#' `datacenter_index` -- Virginia lands at the bottom and Vermont at the top.
#'
#' `Committed.Capacity.(MW)` is preferred where it still exists so that older
#' vintages keep reading exactly as before; this is a fallback, not a
#' redefinition. Note the two are not the same concept, so a series spanning
#' the 1.5.0 boundary is not continuous -- see docs/refactor_plan.md F-08.
#'
#' @param data A BNEF "Data Centers" sheet.
#' @return The column name, or NA if no usable candidate is present.
#' @export
bnef_pipeline_column <- function(data) {
  candidates <- c("Committed.Capacity.(MW)", "Under.Construction.Capacity.(MW)")
  for (candidate in candidates) {
    if (!candidate %in% names(data)) {
      next
    }
    values <- suppressWarnings(as.numeric(data[[candidate]]))
    if (any(values < 0, na.rm = TRUE)) {
      # A capacity column holding negative values is a chart helper or a
      # schema change, not capacity. Skip it rather than invert the index.
      next
    }
    return(candidate)
  }
  NA_character_
}
