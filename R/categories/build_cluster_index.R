#' Build cluster index
#'
#' @param inputs Data frame with cluster inputs.
#' @return Data frame with cluster_index.
#' @export
build_cluster_index <- function(inputs) {
  anchor_vars <- c(
    "datacenter_mw",
    "semiconductor_manufacturing",
    "battery_manufacturing",
    "solar_manufacturing",
    "ev_manufacturing"
  )

  positive <- c(
    "workforce_share",
    "workforce_growth",
    "industry_feasibility",
    "clean_electric_capacity_growth",
    anchor_vars
  )
  negative <- c("industrial_electricity_price")

  base_cols <- c(setdiff(positive, anchor_vars), negative)
  base_n <- length(base_cols)

  scaled <- scale_dataframe_by_polarity(inputs, positive, negative)

  scaled %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      max_anchor = max(dplyr::c_across(dplyr::all_of(anchor_vars)), na.rm = TRUE),
      dominant_anchor = anchor_vars[which.max(dplyr::c_across(dplyr::all_of(anchor_vars)))],
      positive = rowMeans(dplyr::pick(dplyr::all_of(positive)), na.rm = TRUE),
      negative = rowMeans(dplyr::pick(dplyr::all_of(negative)), na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      base_sum = rowSums(dplyr::across(dplyr::all_of(base_cols)), na.rm = TRUE),
      cluster_index = (base_sum + max_anchor) / (base_n + 1),
      cluster_index = scale_minmax(cluster_index),
      cluster_top = dplyr::if_else(cluster_index > 0.5, state, "")
    ) %>%
    dplyr::arrange(dplyr::desc(cluster_index))
}
