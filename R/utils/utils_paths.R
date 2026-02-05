#' Load configuration for the pipeline
#'
#' @param config_path Path to the config file.
#' @param root Repo root path.
#' @return A list with configuration values.
#' @export
load_config <- function(config_path = NULL, root = find_repo_root()) {
  if (is.null(config_path)) {
    config_path <- Sys.getenv("Electro-Industrial_CONFIG", fs::path(root, "config", "config.yml"))
  }
  if (!file.exists(config_path)) {
    rlang::abort(paste("Config file not found:", config_path))
  }
  yaml::read_yaml(config_path)
}

#' Resolve configured paths and ensure output directories exist
#'
#' @param cfg Config list.
#' @param root Repo root path.
#' @return A list with resolved directories.
#' @export
resolve_paths <- function(cfg, root = find_repo_root()) {
  data_dir <- Sys.getenv("Electro-Industrial_DATA_DIR", cfg$data_dir %||% "data")
  cache_dir <- Sys.getenv("Electro-Industrial_CACHE_DIR", cfg$cache_dir %||% "data/raw_cache")
  output_dir <- Sys.getenv("Electro-Industrial_OUTPUT_DIR", cfg$output_dir %||% "outputs")
  metadata_dir <- fs::path(output_dir, "metadata")

  data_dir <- root_path(root, data_dir)
  cache_dir <- root_path(root, cache_dir)
  output_dir <- root_path(root, output_dir)
  metadata_dir <- root_path(root, metadata_dir)

  fs::dir_create(cache_dir, recurse = TRUE)
  fs::dir_create(output_dir, recurse = TRUE)
  fs::dir_create(metadata_dir, recurse = TRUE)

  list(
    data_dir = data_dir,
    cache_dir = cache_dir,
    output_dir = output_dir,
    metadata_dir = metadata_dir,
    processed_dir = root_path(root, cfg$processed_dir %||% "data/processed"),
    raw_dir = root_path(root, cfg$raw_dir %||% "data/raw"),
    examples_dir = root_path(root, cfg$examples_dir %||% "data/examples"),
    snapshot_date = Sys.getenv("Electro-Industrial_SNAPSHOT_DATE", cfg$snapshot_date %||% "2025-01-01"),
    use_sample_data = as.logical(Sys.getenv("Electro-Industrial_USE_SAMPLE_DATA", cfg$use_sample_data %||% TRUE)),
    write_audit = as.logical(Sys.getenv("Electro-Industrial_WRITE_AUDIT", cfg$write_audit %||% FALSE))
  )
}
