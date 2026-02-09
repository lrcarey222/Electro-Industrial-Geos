#' Load output tables needed for state profile reports
#'
#' @param paths Path list from resolve_paths().
#' @return Named list of tibbles.
#' @export
load_outputs <- function(paths = getOption("Electro_Industrial.paths")) {
  if (is.null(paths)) {
    rlang::abort("Electro_Industrial.paths is not configured.")
  }
  out <- paths$output_dir
  read_if_exists <- function(name) {
    path <- fs::path(out, name)
    if (fs::file_exists(path)) readr::read_csv(path, show_col_types = FALSE) else tibble::tibble()
  }

  list(
    overall = read_if_exists("Electro-Industrial_state.csv"),
    policy = read_if_exists("policy_intent.csv"),
    regulatory = read_if_exists("reg_friction.csv"),
    economic = read_if_exists("economic_capabilities.csv"),
    infrastructure = read_if_exists("infrastructure.csv"),
    deployment = read_if_exists("deployment.csv"),
    cluster = read_if_exists("cluster_index.csv"),
    incentives_by_sector_year = read_if_exists("incentives_by_sector_year.csv"),
    processed_inputs = if (fs::file_exists(fs::path(paths$processed_dir, "inputs_processed.csv"))) {
      readr::read_csv(fs::path(paths$processed_dir, "inputs_processed.csv"), show_col_types = FALSE)
    } else tibble::tibble()
  )
}

#' Compute ranks with NA preserved
#' @export
compute_ranks <- function(df, value_col) {
  vals <- df[[value_col]]
  ranks <- dplyr::if_else(is.na(vals), NA_real_, as.numeric(dplyr::min_rank(dplyr::desc(vals))))
  dplyr::mutate(df, rank = ranks)
}

#' Get category definitions from index config
#' @export
get_category_definition <- function(root = getOption("Electro_Industrial.root", find_repo_root())) {
  def <- yaml::read_yaml(fs::path(root, "config", "index_definition.yml"))
  purrr::imap_dfr(def$categories, ~ tibble::tibble(
    category = .y,
    variable = unlist(.x$variables),
    polarity = .x$polarity %||% NA_character_
  ))
}

#' Build component table for one category and state
#' @export
build_category_component_table <- function(state_name, category_df, category_vars, category_index_col) {
  paths <- getOption("Electro_Industrial.paths")
  raw_df <- if (!is.null(paths) && fs::file_exists(fs::path(paths$processed_dir, "inputs_processed.csv"))) {
    readr::read_csv(fs::path(paths$processed_dir, "inputs_processed.csv"), show_col_types = FALSE)
  } else {
    tibble::tibble(state = character())
  }

  scaled_long <- category_df %>%
    dplyr::select(dplyr::all_of(c("state", category_vars, category_index_col))) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(category_vars), names_to = "variable", values_to = "scaled_value")

  raw_long <- raw_df %>%
    dplyr::select(dplyr::any_of(c("state", category_vars))) %>%
    tidyr::pivot_longer(cols = dplyr::any_of(category_vars), names_to = "variable", values_to = "raw_value")

  rank_long <- scaled_long %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::mutate(rank = dplyr::if_else(is.na(.data$scaled_value), NA_real_, as.numeric(dplyr::min_rank(dplyr::desc(.data$scaled_value))))) %>%
    dplyr::ungroup()

  rank_long %>%
    dplyr::left_join(raw_long, by = c("state", "variable")) %>%
    dplyr::filter(.data$state == state_name) %>%
    dplyr::select(.data$variable, .data$raw_value, .data$scaled_value, .data$rank)
}

#' Build top and bottom peers for a category
#' @export
build_top_bottom_peers <- function(category_df, category_index_col, top_n = 10) {
  ranked <- compute_ranks(category_df, category_index_col) %>%
    dplyr::filter(!is.na(.data[[category_index_col]]))
  list(
    top = ranked %>% dplyr::arrange(dplyr::desc(.data[[category_index_col]])) %>% dplyr::slice_head(n = top_n),
    bottom = ranked %>% dplyr::arrange(.data[[category_index_col]]) %>% dplyr::slice_head(n = top_n)
  )
}

#' Plot top/bottom comparison for a selected state
#' @export
plot_top_bottom_comparison <- function(category_df, category_index_col, state_name, top_n = 10) {
  peers <- build_top_bottom_peers(category_df, category_index_col, top_n)
  compare <- dplyr::bind_rows(
    dplyr::mutate(peers$top, group = "Top"),
    dplyr::mutate(peers$bottom, group = "Bottom")
  ) %>%
    dplyr::bind_rows(category_df %>% dplyr::filter(.data$state == state_name) %>% dplyr::mutate(group = "Selected")) %>%
    dplyr::distinct(.data$state, .keep_all = TRUE) %>%
    dplyr::mutate(state = reorder(.data$state, .data[[category_index_col]]), selected = .data$state == state_name)

  ggplot2::ggplot(compare, ggplot2::aes(x = .data$state, y = .data[[category_index_col]], fill = .data$selected)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("TRUE" = "#d95f02", "FALSE" = "#1b9e77"), guide = "none") +
    ggplot2::labs(x = NULL, y = category_index_col)
}
