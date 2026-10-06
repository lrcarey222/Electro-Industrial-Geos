# Coverage gate: runs between building the indices and writing outputs, so an
# index that has silently fallen back to sample data cannot reach a published
# file. See R/coverage.R for why this is a ratchet rather than a floor.

if (is.null(paths) || is.null(index_definition)) {
  rlang::abort("Configuration not loaded. Run scripts/00_setup.R first.")
}

coverage_config <- local({
  path <- fs::path(repo_root, "config", "validation.yml")
  if (!fs::file_exists(path)) {
    return(list())
  }
  (yaml::read_yaml(path)$coverage) %||% list()
})

sample_inputs_for_coverage <- local({
  path <- fs::path(paths$examples_dir, "sample_inputs.csv")
  if (!fs::file_exists(path)) {
    return(NULL)
  }
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
})

coverage_indicators <- definition_indicators(index_definition)
coverage_table <- indicator_coverage(
  processed_inputs,
  sample_inputs_for_coverage,
  coverage_indicators,
  min_geographies = coverage_config$min_geographies %||% 10L
)

# Inputs the run could not reach are a different thing from inputs that do not
# exist. SKIP_DATA_DOWNLOADS is what CI sets, and in that mode the licensed
# payloads are absent by design and the connectors cannot fetch -- so their
# indicators are expected to be on sample data and must not fail the build.
inputs_unavailable <- isTRUE(as.logical(Sys.getenv("SKIP_DATA_DOWNLOADS", "FALSE")))
declared_bleed <- as.character(coverage_config$allowed_sample_bleed %||% character(0))
if (inputs_unavailable) {
  declared_bleed <- c(
    declared_bleed,
    as.character(coverage_config$allowed_when_inputs_unavailable %||% character(0))
  )
}

coverage_result <- coverage_regressions(coverage_table, declared_bleed)

readr::write_csv(
  coverage_table %>%
    dplyr::mutate(
      declared = .data$indicator %in% declared_bleed,
      inputs_unavailable = inputs_unavailable
    ),
  fs::path(paths$processed_dir, "indicator_coverage.csv")
)

n_bleed <- sum(coverage_table$status == "sample_bleed")
message(sprintf(
  "coverage: %d of %d indicators on sample data%s",
  n_bleed, nrow(coverage_table),
  if (inputs_unavailable) " (inputs unavailable: SKIP_DATA_DOWNLOADS is set)" else ""
))

if (length(coverage_result$sparse) > 0) {
  message(sprintf(
    "coverage: thin but real -- %s",
    paste(coverage_result$sparse, collapse = ", ")
  ))
}

# A declared indicator that has recovered is good news, and the list should
# shrink to match. Said out loud so the ratchet actually tightens.
if (length(coverage_result$recovered) > 0) {
  message(sprintf(
    "coverage: no longer on sample data, remove from config/validation.yml -- %s",
    paste(coverage_result$recovered, collapse = ", ")
  ))
}

if (length(coverage_result$empty) > 0) {
  rlang::abort(c(
    "Coverage gate: an indicator has no values at all.",
    x = paste(coverage_result$empty, collapse = ", "),
    i = "An empty indicator cannot be scaled, so the index built on it is not meaningful."
  ))
}

if (length(coverage_result$regressions) > 0) {
  rlang::abort(c(
    glue::glue(
      "Coverage gate: {length(coverage_result$regressions)} indicator(s) fell back to ",
      "sample data and are not declared in config/validation.yml."
    ),
    x = paste(coverage_result$regressions, collapse = ", "),
    i = "Each holds only California, Texas and New York, copied from data/examples/sample_inputs.csv.",
    i = "Fix the producer, or -- if this is expected -- add it to coverage.allowed_sample_bleed with a reason.",
    i = "Outputs were NOT written."
  ))
}
