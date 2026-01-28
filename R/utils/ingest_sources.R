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

  download_sources <- list(
    list(
      name = "bea_sqgdp",
      url = "https://apps.bea.gov/regional/zip/SQGDP.zip",
      filename = "SQGDP.zip"
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
    dest_path <- fs::path(dest_dir, src$filename)

    if (!isTRUE(skip_downloads)) {
      cached <- download_with_cache(src$url, cache_dir, snapshot_date, filename = src$filename)
      fs::file_copy(cached, dest_path, overwrite = TRUE)
    }

    tibble::tibble(
      source = src$name,
      type = "remote",
      path = dest_path,
      exists = fs::file_exists(dest_path)
    )
  })

  dplyr::bind_rows(local_inventory, download_inventory)
}
