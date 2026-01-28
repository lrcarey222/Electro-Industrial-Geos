#' Read a YAML file
#'
#' @param path Path to YAML.
#' @param required Whether to error if missing.
#' @return Parsed YAML list or NULL.
#' @export
read_yaml_file <- function(path, required = TRUE) {
  if (!file.exists(path)) {
    if (required) {
      rlang::abort(glue::glue("Required config not found: {path}"))
    }
    return(NULL)
  }
  yaml::read_yaml(path)
}

#' Load Electrotech configuration
#'
#' @param root Repo root.
#' @return Config list.
#' @export
load_electrotech_config <- function(root) {
  config_path <- Sys.getenv(
    "ELECTROTECH_CONFIG",
    fs::path(root, "config", "config.yml")
  )
  read_yaml_file(config_path, required = TRUE)
}

#' Load Electrotech weights
#'
#' @param root Repo root.
#' @return Weights list.
#' @export
load_electrotech_weights <- function(root) {
  weights_path <- Sys.getenv(
    "ELECTROTECH_WEIGHTS",
    fs::path(root, "config", "weights.yml")
  )
  read_yaml_file(weights_path, required = TRUE)
}

#' Load index definition config
#'
#' @param root Repo root.
#' @return Index definition list.
#' @export
load_index_definition <- function(root) {
  definition_path <- Sys.getenv(
    "ELECTROTECH_INDEX_DEFINITION",
    fs::path(root, "config", "index_definition.yml")
  )
  read_yaml_file(definition_path, required = FALSE)
}

#' Load missing data config
#'
#' @param root Repo root.
#' @return Missing data list.
#' @export
load_missing_data <- function(root) {
  missing_path <- Sys.getenv(
    "ELECTROTECH_MISSING_DATA",
    fs::path(root, "config", "missing_data.yml")
  )
  read_yaml_file(missing_path, required = FALSE)
}

#' Set Electrotech options
#'
#' @param cfg Config list.
#' @param weights Weights list.
#' @param index_definition Index definition list.
#' @param missing_data Missing data list.
#' @return Invisible TRUE.
#' @export
set_electrotech_options <- function(cfg, weights, index_definition = NULL, missing_data = NULL) {
  options(
    electrotech.config = cfg,
    electrotech.weights = weights,
    electrotech.index_definition = index_definition,
    electrotech.missing_data = missing_data
  )
  invisible(TRUE)
}
