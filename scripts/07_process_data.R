paths <- getOption("electrotech.paths")
config <- getOption("electrotech.config")

if (is.null(paths) || is.null(config)) {
  rlang::abort("Configuration not loaded. Run scripts/00_setup.R first.")
}

inputs <- if (isTRUE(paths$use_sample_data)) {
  load_sample_inputs(paths)
} else {
  load_inputs(paths)
}

validated_inputs <- validate_inputs_schema(inputs, required_input_columns())

processed_inputs <- validated_inputs

processed_path <- fs::path(paths$processed_dir, "inputs_processed.csv")
readr::write_csv(processed_inputs, processed_path)
