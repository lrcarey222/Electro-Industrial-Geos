#' Build cluster index
#'
#' @param inputs Data frame with cluster inputs.
#' @param geo_col Geography column to score (defaults to state).
#' @param top_label_col Column used for `cluster_top` labels.
#' @return Data frame with cluster_index.
#' @export
build_cluster_index <- function(inputs, geo_col = "state", top_label_col = geo_col) {
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

  if (!geo_col %in% names(inputs)) {
    rlang::abort(glue::glue("Cluster inputs must include `{geo_col}`."))
  }
  if (!top_label_col %in% names(inputs)) {
    rlang::abort(glue::glue("Cluster inputs must include `{top_label_col}`."))
  }

  scaled <- scale_dataframe_by_polarity(inputs, positive, negative)

  scaled %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # A geography can legitimately have no anchor data at all -- every
      # manufacturing and datacenter value missing. `which.max()` returns
      # integer(0) for an all-NA row, which makes `dominant_anchor` size 0 and
      # aborts the whole index; `max(na.rm = TRUE)` returns -Inf with a warning.
      # Neither is a scoring decision, so handle the empty case explicitly.
      #
      # This preserves the resulting index: -Inf previously became NA in
      # scale_minmax(), which drops non-finite values, and NA stays NA. The
      # composite then coalesces a missing cluster_index to 0 as before.
      max_anchor = {
        anchors <- dplyr::c_across(dplyr::all_of(anchor_vars))
        if (all(is.na(anchors))) NA_real_ else max(anchors, na.rm = TRUE)
      },
      dominant_anchor = {
        anchors <- dplyr::c_across(dplyr::all_of(anchor_vars))
        if (all(is.na(anchors))) NA_character_ else anchor_vars[which.max(anchors)]
      },
      positive = rowMeans(dplyr::pick(dplyr::all_of(positive)), na.rm = TRUE),
      negative = rowMeans(dplyr::pick(dplyr::all_of(negative)), na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      base_sum = rowSums(dplyr::across(dplyr::all_of(base_cols)), na.rm = TRUE),
      cluster_index = (base_sum + max_anchor) / (base_n + 1),
      cluster_index = scale_minmax(cluster_index),
      cluster_top = dplyr::if_else(
        cluster_index > 0.5,
        as.character(.data[[top_label_col]]),
        ""
      )
    ) %>%
    dplyr::arrange(dplyr::desc(cluster_index))
}

#' Build state cluster rollup from PEA cluster index
#'
#' @param cluster_pea PEA cluster index data frame.
#' @return State-level cluster table using the top-ranked PEA per state.
#' @export
build_state_cluster_from_pea <- function(cluster_pea, membership = NULL) {
  required_cols <- c("state", "cluster_index", "cluster_top")
  missing_cols <- setdiff(required_cols, names(cluster_pea))
  if (length(missing_cols) > 0) {
    rlang::abort(glue::glue("PEA cluster index missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }

  # A state inherits from the best PEA that OVERLAPS it, which is not the same
  # as the best PEA it dominates. Connecticut, New Jersey and Rhode Island
  # dominate no PEA at all -- they are absorbed into larger multi-state metros --
  # so grouping on the PEA's own `state` column alone silently scores them 0.
  #
  # Before F-21 was fixed this worked by accident: a PEA emitted one row per
  # state it touched, so membership was implicit in the duplication. With one
  # row per PEA it has to be passed in explicitly.
  if (!is.null(membership) && nrow(membership) > 0) {
    needed <- c("economic_area", "state")
    if (!all(needed %in% names(membership))) {
      rlang::abort(glue::glue(
        "PEA state membership needs {paste(needed, collapse = ', ')}."
      ))
    }
    if (!"economic_area" %in% names(cluster_pea)) {
      rlang::abort("PEA cluster index needs `economic_area` to expand membership.")
    }
    cluster_pea <- cluster_pea %>%
      dplyr::select(-dplyr::all_of("state")) %>%
      dplyr::inner_join(
        membership %>% dplyr::distinct(.data$economic_area, .data$state),
        by = "economic_area",
        relationship = "many-to-many"
      )
  }

  cluster_pea %>%
    dplyr::group_by(state) %>%
    dplyr::slice_max(order_by = cluster_index, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
}
