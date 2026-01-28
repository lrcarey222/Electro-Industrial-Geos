#' Build regulatory ease index
#'
#' @param inputs Data frame with regulatory inputs.
#' @return Data frame with ease_index.
#' @export
build_regulatory_ease_index <- function(inputs) {
  inputs %>%
    scale_dataframe_by_polarity(
      positive = character(),
      negative = c("cpcn", "regdata_index", "ordinance", "sepa")
    ) %>%
    dplyr::mutate(ease_index = rowmean_index(dplyr::select(., cpcn, regdata_index, ordinance, sepa)))
}
