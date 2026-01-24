#' Export output tables and metadata
#'
#' @param outputs Named list of data frames.
#' @param paths Paths list from resolve_paths.
#' @return Invisible TRUE.
#' @export
export_outputs <- function(outputs, paths) {
  readr::write_csv(outputs$electrotech, fs::path(paths$output_dir, "electrotech.csv"))
  readr::write_csv(outputs$deployment, fs::path(paths$output_dir, "deployment.csv"))
  readr::write_csv(outputs$regulatory, fs::path(paths$output_dir, "reg_friction.csv"))
  readr::write_csv(outputs$cluster, fs::path(paths$output_dir, "cluster_index.csv"))

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Policy_Intent")
  openxlsx::writeData(wb, "Policy_Intent", outputs$policy)
  openxlsx::addWorksheet(wb, "Regulatory_Ease")
  openxlsx::writeData(wb, "Regulatory_Ease", outputs$regulatory)
  openxlsx::addWorksheet(wb, "Deployment")
  openxlsx::writeData(wb, "Deployment", outputs$deployment)
  openxlsx::addWorksheet(wb, "Economic_Capabilities")
  openxlsx::writeData(wb, "Economic_Capabilities", outputs$economic)
  openxlsx::addWorksheet(wb, "Infrastructure")
  openxlsx::writeData(wb, "Infrastructure", outputs$infrastructure)
  openxlsx::addWorksheet(wb, "Cluster_Index")
  openxlsx::writeData(wb, "Cluster_Index", outputs$cluster)
  openxlsx::addWorksheet(wb, "Electrotech_Combined")
  openxlsx::writeData(wb, "Electrotech_Combined", outputs$electrotech)

  openxlsx::saveWorkbook(
    wb,
    fs::path(paths$output_dir, "Electrotech_Index_Tables.xlsx"),
    overwrite = TRUE
  )

  metadata <- tibble::tibble(
    snapshot_date = paths$snapshot_date,
    run_timestamp = as.character(Sys.time()),
    use_sample_data = paths$use_sample_data
  )
  readr::write_csv(metadata, fs::path(paths$metadata_dir, "run_metadata.csv"))

  invisible(TRUE)
}
