#' Build PEA Electro-Industrial index
#'
#' @param cluster_pea PEA-level cluster index data frame.
#' @return Data frame with PEA Electro-Industrial index columns.
#' @export
build_Electro_Industrial_pea_index <- function(cluster_pea) {
  required <- c("economic_area", "state", "cluster_index")
  missing <- setdiff(required, names(cluster_pea))
  if (length(missing) > 0) {
    rlang::abort(glue::glue("PEA index inputs missing required columns: {paste(missing, collapse = ', ')}"))
  }

  cluster_pea %>%
    dplyr::transmute(
      economic_area,
      state,
      cluster_index,
      Electro_Industrial_index = cluster_index,
      Electro_Industrial_index_w = cluster_index
    ) %>%
    dplyr::arrange(dplyr::desc(Electro_Industrial_index_w))
}
