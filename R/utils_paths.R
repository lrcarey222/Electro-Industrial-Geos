#' Load configuration for the pipeline
#'
#' @param config_path Path to the config file.
#' @return A list with configuration values.
#' @export
load_config <- function(config_path = "config.yml") {
  if (!file.exists(config_path)) {
    rlang::abort(paste("Config file not found:", config_path))
  }
  config::get(file = config_path)
}

#' Resolve configured paths and ensure output directories exist
#'
#' @param cfg Config list.
#' @return A list with resolved directories.
#' @export
resolve_paths <- function(cfg) {
  data_dir <- Sys.getenv("ELECTROTECH_DATA_DIR", cfg$data_dir %||% "data")
  cache_dir <- Sys.getenv("ELECTROTECH_CACHE_DIR", cfg$cache_dir %||% "data/raw_cache")
  output_dir <- Sys.getenv("ELECTROTECH_OUTPUT_DIR", cfg$output_dir %||% "outputs")
  metadata_dir <- fs::path(output_dir, "metadata")

  fs::dir_create(cache_dir, recurse = TRUE)
  fs::dir_create(output_dir, recurse = TRUE)
  fs::dir_create(metadata_dir, recurse = TRUE)

  list(
    data_dir = data_dir,
    cache_dir = cache_dir,
    output_dir = output_dir,
    metadata_dir = metadata_dir,
    snapshot_date = Sys.getenv("ELECTROTECH_SNAPSHOT_DATE", cfg$snapshot_date %||% "2025-01-01"),
    use_sample_data = as.logical(Sys.getenv("ELECTROTECH_USE_SAMPLE_DATA", cfg$use_sample_data %||% TRUE))
  )
}
