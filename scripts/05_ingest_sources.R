paths <- getOption("electrotech.paths")

if (is.null(paths)) {
  rlang::abort("Paths not configured. Ensure scripts/00_setup.R ran successfully.")
}

skip_downloads <- as.logical(Sys.getenv("SKIP_DATA_DOWNLOADS", "TRUE"))
if (isTRUE(skip_downloads)) {
  message("Skipping data downloads (SKIP_DATA_DOWNLOADS=TRUE).")
} else {
  message("No automated downloads configured; ensure data/raw is populated as needed.")
}
