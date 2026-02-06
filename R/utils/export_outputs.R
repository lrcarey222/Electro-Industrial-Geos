#' Export output tables and metadata
#'
#' @param outputs Named list of data frames.
#' @param paths Paths list from resolve_paths.
#' @param raw_inputs Raw input data frame.
#' @param index_definition Optional index definition.
#' @return Invisible TRUE.
#' @export
export_outputs <- function(outputs, paths, raw_inputs = NULL, index_definition = NULL) {
  readr::write_csv(outputs[["Electro-Industrial"]], fs::path(paths$output_dir, "Electro-Industrial.csv"))
  readr::write_csv(outputs[["Electro-Industrial"]], fs::path(paths$output_dir, "Electro-Industrial_state.csv"))
  readr::write_csv(outputs[["Electro-Industrial-PEA"]], fs::path(paths$output_dir, "Electro-Industrial_pea.csv"))
  readr::write_csv(outputs$deployment, fs::path(paths$output_dir, "deployment.csv"))
  readr::write_csv(outputs$regulatory, fs::path(paths$output_dir, "reg_friction.csv"))
  readr::write_csv(outputs$cluster, fs::path(paths$output_dir, "cluster_index.csv"))
  readr::write_csv(outputs$cluster_pea, fs::path(paths$output_dir, "cluster_index_pea.csv"))

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
  openxlsx::addWorksheet(wb, "Cluster_Index_PEA")
  openxlsx::writeData(wb, "Cluster_Index_PEA", outputs$cluster_pea)
  openxlsx::addWorksheet(wb, "Electro-Industrial_Combined")
  openxlsx::writeData(wb, "Electro-Industrial_Combined", outputs[["Electro-Industrial"]])
  openxlsx::addWorksheet(wb, "Electro-Industrial_PEA")
  openxlsx::writeData(wb, "Electro-Industrial_PEA", outputs[["Electro-Industrial-PEA"]])

  openxlsx::saveWorkbook(
    wb,
    fs::path(paths$output_dir, "Electro-Industrial_Index_Tables.xlsx"),
    overwrite = TRUE
  )

  metadata <- tibble::tibble(
    snapshot_date = paths$snapshot_date,
    run_timestamp = as.character(Sys.time()),
    use_sample_data = paths$use_sample_data
  )
  readr::write_csv(metadata, fs::path(paths$metadata_dir, "run_metadata.csv"))

  if (!is.null(raw_inputs) && isTRUE(paths$write_audit)) {
    audit_dir <- fs::path(paths$output_dir, "audit")
    fs::dir_create(audit_dir, recurse = TRUE)
    audit_table <- build_audit_table(
      raw_inputs,
      indices = outputs,
      definition = index_definition
    )
    readr::write_csv(audit_table, fs::path(audit_dir, "Electro-Industrial_audit_long.csv"))
  }

  invisible(TRUE)
}
