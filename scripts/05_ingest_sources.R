paths <- getOption("electrotech.paths")

if (is.null(paths)) {
  rlang::abort("Paths not configured. Ensure scripts/00_setup.R ran successfully.")
}

skip_downloads <- as.logical(Sys.getenv("SKIP_DATA_DOWNLOADS", "FALSE"))

inventory <- ingest_legacy_sources(
  paths = paths,
  snapshot_date = paths$snapshot_date,
  skip_downloads = skip_downloads
)

inventory_path <- fs::path(paths$processed_dir, "source_inventory.csv")
readr::write_csv(inventory, inventory_path)

missing_local <- inventory %>%
  dplyr::filter(type == "local", !exists) %>%
  dplyr::pull(source)

if (length(missing_local) > 0) {
  message("Missing local raw files (place them under ", paths$raw_dir, "): ")
  message(paste0("- ", missing_local, collapse = "\n"))
}

if (isTRUE(skip_downloads)) {
  message("Skipping URL downloads (SKIP_DATA_DOWNLOADS=TRUE).")
}
