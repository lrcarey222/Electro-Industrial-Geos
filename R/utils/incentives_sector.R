#' Build incentives-by-sector by state and year (since 2020)
#'
#' @param gjf Good Jobs First data frame (raw or cleaned names).
#' @param state_gdp Optional state GDP table with columns GeoName and gdp_2024.
#' @return Tibble with state, year, sector, subs_m, incentives_gdp.
#' @export
build_incentives_by_sector_year <- function(gjf = NULL, state_gdp = NULL) {
  out_cols <- c("state", "year", "sector", "subs_m", "incentives_gdp")
  if (is.null(gjf) || nrow(gjf) == 0) {
    return(tibble::as_tibble(stats::setNames(replicate(length(out_cols), logical(0), simplify = FALSE), out_cols)))
  }

  gjf <- janitor::clean_names(gjf)
  needed <- c("location", "year", "subs_m", "specific_industry_of_parent", "major_industry_of_parent")
  if (!all(needed %in% names(gjf))) {
    return(tibble::as_tibble(stats::setNames(replicate(length(out_cols), logical(0), simplify = FALSE), out_cols)))
  }

  classify_sector <- function(specific, major) {
    txt <- stringr::str_to_lower(paste(specific %||% "", major %||% ""))
    dplyr::case_when(
      stringr::str_detect(txt, "semiconductor|\\bchip\\b") ~ "Semiconductors",
      stringr::str_detect(txt, "battery") ~ "Batteries",
      stringr::str_detect(txt, "electric vehicle|\\bev\\b|automotive") ~ "EVs",
      stringr::str_detect(txt, "data\\s*center|datacenter") ~ "Data Centers",
      TRUE ~ NA_character_
    )
  }

  out <- gjf %>%
    dplyr::mutate(
      year = suppressWarnings(as.integer(.data$year)),
      sector = classify_sector(.data$specific_industry_of_parent, .data$major_industry_of_parent)
    ) %>%
    dplyr::filter(!is.na(.data$sector), !is.na(.data$year), .data$year >= 2020) %>%
    dplyr::group_by(state = .data$location, .data$year, .data$sector) %>%
    dplyr::summarize(subs_m = sum(as.numeric(.data$subs_m), na.rm = TRUE), .groups = "drop")

  if (!is.null(state_gdp) && nrow(state_gdp) > 0 && all(c("GeoName", "gdp_2024") %in% names(state_gdp))) {
    out <- out %>%
      dplyr::left_join(state_gdp %>% dplyr::transmute(state = .data$GeoName, gdp_2024 = as.numeric(.data$gdp_2024)), by = "state") %>%
      dplyr::mutate(incentives_gdp = dplyr::if_else(is.na(.data$gdp_2024), NA_real_, .data$subs_m / .data$gdp_2024 * 100)) %>%
      dplyr::select(-.data$gdp_2024)
  } else {
    out <- out %>% dplyr::mutate(incentives_gdp = NA_real_)
  }

  out %>% dplyr::select(dplyr::all_of(out_cols))
}
