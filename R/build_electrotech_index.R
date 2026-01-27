#' Build electrotech index
#'
#' @param deployment Deployment data frame.
#' @param infrastructure Infrastructure data frame.
#' @param economic Economic capabilities data frame.
#' @param policy Policy intent data frame.
#' @param regulatory Regulatory ease data frame.
#' @param cluster Cluster index data frame.
#' @return Data frame with electrotech_index and electrotech_index_w.
#' @export
build_electrotech_index <- function(
  deployment,
  infrastructure,
  economic,
  policy,
  regulatory,
  cluster
) {
  weights <- c(
    deployment_index = 0.4,
    infra_index = 0.15,
    econ_index = 0.15,
    intent_index = 0.2,
    cluster_index = 0.2,
    ease_index = 0.2
  )

  combined <- deployment %>%
    dplyr::select(state, abbr, deployment_index) %>%
    dplyr::left_join(infrastructure %>% dplyr::select(state, infra_index), by = "state") %>%
    dplyr::left_join(economic %>% dplyr::select(state, econ_index), by = "state") %>%
    dplyr::left_join(policy %>% dplyr::select(state, intent_index), by = "state") %>%
    dplyr::left_join(regulatory %>% dplyr::select(state, ease_index), by = "state") %>%
    dplyr::left_join(cluster %>% dplyr::select(state, cluster_index), by = "state") %>%
    dplyr::mutate(cluster_index = dplyr::coalesce(cluster_index, 0))

  scaled <- scale_dataframe_by_polarity(
    combined,
    positive = c("deployment_index", "infra_index", "econ_index", "intent_index", "cluster_index", "ease_index"),
    negative = character()
  )

  scaled %>%
    dplyr::mutate(
      electrotech_index = rowSums(dplyr::select(., deployment_index, infra_index, econ_index, intent_index, cluster_index, ease_index), na.rm = TRUE),
      electrotech_index_w = weighted_index(., weights)
    )
}
