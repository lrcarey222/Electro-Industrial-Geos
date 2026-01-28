#' Load inputs from data directory
#'
#' @param paths Paths list.
#' @return Data frame of inputs.
#' @export
load_inputs <- function(paths = getOption("electrotech.paths")) {
  if (is.null(paths)) {
    rlang::abort("Paths not configured. Run scripts/00_setup.R first.")
  }
  input_path <- fs::path(paths$data_dir, "inputs.csv")
  if (!fs::file_exists(input_path)) {
    rlang::abort("Required inputs.csv not found. See data/README.md for schema.")
  }
  readr::read_csv(input_path, show_col_types = FALSE)
}
