#' Build policy intent index
#'
#' @param inputs Data frame with policy intent inputs.
#' @return Data frame with intent_index.
#' @export
build_policy_intent_index <- function(inputs) {
  inputs %>%
    dplyr::mutate(
      incentives_gdp = dplyr::coalesce(incentives_gdp, 0),
      legislation_index = dplyr::coalesce(legislation_index, 0)
    ) %>%
    scale_dataframe_by_polarity(
      positive = c("incentives_gdp", "spot_score", "dsire_policy_count", "dev_policy_count", "legislation_index"),
      negative = character()
    ) %>%
    dplyr::mutate(intent_index = rowmean_index(dplyr::select(., incentives_gdp, spot_score, dsire_policy_count, dev_policy_count, legislation_index)))
}
