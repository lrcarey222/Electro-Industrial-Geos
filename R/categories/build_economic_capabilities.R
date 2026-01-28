#' Build economic capabilities index
#'
#' @param inputs Data frame with economic inputs.
#' @return Data frame with econ_index.
#' @export
build_economic_capabilities_index <- function(inputs) {
  inputs %>%
    scale_dataframe_by_polarity(
      positive = c("employment_lq", "gdp_growth_index", "feasibility_index", "economic_dynamism"),
      negative = character()
    ) %>%
    dplyr::mutate(econ_index = rowmean_index(dplyr::select(., employment_lq, gdp_growth_index, feasibility_index, economic_dynamism)))
}
