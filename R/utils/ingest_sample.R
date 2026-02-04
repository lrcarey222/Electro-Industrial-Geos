#' Load sample inputs
#'
#' @param paths Paths list.
#' @return Data frame of sample inputs.
#' @export
load_sample_inputs <- function(paths = getOption("Electro-Industrial.paths")) {
  if (is.null(paths)) {
    rlang::abort("Paths not configured. Run scripts/00_setup.R first.")
  }
  sample_path <- fs::path(paths$examples_dir, "sample_inputs.csv")
  if (!fs::file_exists(sample_path)) {
    sample_path <- system.file("extdata", "sample_inputs.csv", package = "Electro-Industrialindex")
  }
  if (sample_path == "" || !fs::file_exists(sample_path)) {
    rlang::abort("Sample inputs not found in data/examples or inst/extdata.")
  }
  readr::read_csv(sample_path, show_col_types = FALSE)
}
