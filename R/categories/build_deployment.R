#' Build deployment index
#'
#' @param inputs Data frame with deployment inputs.
#' @return Data frame with deployment_index.
#' @export
build_deployment_index <- function(inputs) {
  inputs %>%
    scale_dataframe_by_polarity(
      positive = c(
        "clean_tech_investment",
        "datacenter_index",
        "electric_capacity_growth",
        "semiconductor_investment",
        "evs_per_capita"
      ),
      negative = character()
    ) %>%
    dplyr::mutate(
      deployment_index = rowmean_index(
        dplyr::select(
          ., clean_tech_investment, datacenter_index, electric_capacity_growth,
          semiconductor_investment, evs_per_capita
        )
      )
    )
}
