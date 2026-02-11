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
