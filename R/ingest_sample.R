#' Load sample inputs bundled with the package
#'
#' @return Data frame of sample inputs.
#' @export
load_sample_inputs <- function() {
  sample_path <- system.file("extdata", "sample_inputs.csv", package = "electrotechindex")
  if (sample_path == "") {
    rlang::abort("Sample inputs not found in inst/extdata.")
  }
  readr::read_csv(sample_path, show_col_types = FALSE)
}
