#' Build infrastructure index
#'
#' @param inputs Data frame with infrastructure inputs.
#' @return Data frame with infra_index and infra_index_w.
#' @export
build_infrastructure_index <- function(inputs) {
  positive <- c("renewable_potential", "ev_stations_cap", "electricity_price")
  negative <- c("interconnection_queue", "cnbc_rank")
  weights_cfg <- getOption("Electro_Industrial.weights")
  weights <- weights_cfg$infrastructure %||% list(
    renewable_potential = 0.2,
    ev_stations_cap = 0.2,
    interconnection_queue = 0.2,
    electricity_price = 0.2,
    cnbc_rank = 0.2
  )
  weights <- unlist(weights, use.names = TRUE)

  scaled <- scale_dataframe_by_polarity(inputs, positive, negative)

  scaled %>%
    dplyr::mutate(
      infra_index = rowmean_index(dplyr::select(., dplyr::all_of(c(positive, negative)))),
      infra_index_w = weighted_index(., weights)
    )
}
