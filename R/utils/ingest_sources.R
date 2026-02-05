#' Ingest legacy raw sources
#'
#' @param paths Paths list.
#' @param snapshot_date Snapshot date string.
#' @param skip_downloads Whether to skip URL downloads.
#' @return Tibble of source inventory.
#' @export
ingest_legacy_sources <- function(paths, snapshot_date, skip_downloads = FALSE) {
  raw_dir <- paths$raw_dir
  cache_dir <- paths$cache_dir

  legacy_files <- c(
    "Good Jobs First/gjf_complete.csv",
    "dbo_Program.csv",
    "climate_leg.csv",
    "CPCN_Requirements_and_Enactment_Years_by_State_GPT.csv",
    "Regdata_subnational.csv",
    "Solar Ordinances.csv",
    "state_sepa.csv",
    "50 State Gap Analysis.xlsx",
    "cnbc_bus_rankings.csv",
    "BNEF/2025-08-08 - Global Data Center Live IT Capacity Database.xlsx",
    "clean_investment_monitor_q2_2025/quarterly_actual_investment.csv",
    "clean_investment_monitor_q2_2025/socioeconomics.csv",
    "semiconductor_man.csv",
    "Downloads/state_business_cycle_status.csv",
    "Downloads/us_drone_facility_announcements_2022_2025.csv",
    "egrid2023_data_metric_rev2.xlsx",
    "table_8.xlsx"
  )

  snapshot_date_parsed <- suppressWarnings(as.Date(snapshot_date))
  if (is.na(snapshot_date_parsed)) {
    snapshot_date_parsed <- Sys.Date()
  }
  generator_date <- seq(snapshot_date_parsed, length.out = 2, by = "-1 month")[2]
  make_generator_source <- function(date) {
    filename <- paste0(
      tolower(format(date, "%B")),
      "_generator",
      format(date, "%Y"),
      ".xlsx"
    )
    list(
      url = paste0("https://www.eia.gov/electricity/data/eia860m/xls/", filename),
      filename = filename
    )
  }
  generator_source <- make_generator_source(generator_date)

  is_valid_xlsx <- function(path) {
    if (!fs::file_exists(path) || fs::file_size(path) <= 0) {
      return(FALSE)
    }
    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)
    sig <- readBin(con, what = "raw", n = 2)
    identical(sig, charToRaw("PK"))
  }

  download_sources <- list(
    list(
      name = "bea_sagdp",
      url = "https://apps.bea.gov/regional/zip/SAGDP.zip",
      filename = "SAGDP.zip"
    ),
    list(
      name = "eig_dynamism",
      url = "https://eig.org/state-dynamism-2025/assets/Downloadable-Data-EIG-Index-of-State-Dynamism-2022.xlsx",
      filename = "Downloadable-Data-EIG-Index-of-State-Dynamism-2022.xlsx"
    ),
    list(
      name = "afdc_station_counts",
      url = "https://afdc.energy.gov/files/docs/historical-station-counts.xlsx?year=2024",
      filename = "historical-station-counts.xlsx"
    ),
    list(
      name = "lbnl_interconnection",
      url = "https://emp.lbl.gov/sites/default/files/2025-08/LBNL_Ix_Queue_Data_File_thru2024_v2.xlsx",
      filename = "LBNL_Ix_Queue_Data_File_thru2024_v2.xlsx"
    ),
    list(
      name = "eia_sales_revenue",
      url = "https://www.eia.gov/electricity/data/eia861m/xls/sales_revenue.xlsx",
      filename = "sales_revenue.xlsx"
    ),
    list(
      name = "afdc_ev_registrations",
      url = "https://afdc.energy.gov/files/u/data/data_source/10962/10962-ev-registration-counts-by-state_9-06-24.xlsx?12518e7893",
      filename = "10962-ev-registration-counts-by-state_9-06-24.xlsx"
    ),
    list(
      name = "eia_generator_capacity",
      url = generator_source$url,
      filename = generator_source$filename
    )
  )

  fs::dir_create(raw_dir, recurse = TRUE)
  fs::dir_create(cache_dir, recurse = TRUE)

  local_inventory <- purrr::map_dfr(legacy_files, function(rel_path) {
    full_path <- fs::path(raw_dir, rel_path)
    fs::dir_create(fs::path_dir(full_path), recurse = TRUE)
    tibble::tibble(
      source = rel_path,
      type = "local",
      path = full_path,
      exists = fs::file_exists(full_path)
    )
  })

  download_inventory <- purrr::map_dfr(download_sources, function(src) {
    dest_dir <- fs::path(raw_dir, "remote")
    fs::dir_create(dest_dir, recurse = TRUE)
    resolved_src <- src
    dest_path <- fs::path(dest_dir, resolved_src$filename)

    if (!isTRUE(skip_downloads)) {
      if (identical(src$name, "eia_generator_capacity")) {
        download_result <- NULL
        for (months_back in 0:12) {
          candidate_date <- seq(generator_date, length.out = months_back + 1, by = "-1 month")[months_back + 1]
          candidate_src <- make_generator_source(candidate_date)
          candidate_cached <- tryCatch(
            download_with_cache(candidate_src$url, cache_dir, snapshot_date, filename = candidate_src$filename),
            error = function(e) NULL
          )
          if (!is.null(candidate_cached) && is_valid_xlsx(candidate_cached)) {
            download_result <- candidate_cached
            resolved_src <- modifyList(src, candidate_src)
            break
          }
        }
        if (is.null(download_result)) {
          rlang::warn("Unable to download EIA generator workbook for previous-month candidates (0-12 months back).")
        } else {
          dest_path <- fs::path(dest_dir, resolved_src$filename)
          fs::file_copy(download_result, dest_path, overwrite = TRUE)
        }
      } else {
        cached <- download_with_cache(resolved_src$url, cache_dir, snapshot_date, filename = resolved_src$filename)
        fs::file_copy(cached, dest_path, overwrite = TRUE)
      }
    }

    tibble::tibble(
      source = resolved_src$name,
      type = "remote",
      path = dest_path,
      exists = fs::file_exists(dest_path)
    )
  })

  dplyr::bind_rows(local_inventory, download_inventory)
}
