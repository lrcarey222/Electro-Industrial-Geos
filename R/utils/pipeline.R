#' Run the Electrotech pipeline
#'
#' @param root Repo root path.
#' @return Invisible TRUE.
#' @export
run_electrotech_pipeline <- function(root = NULL) {
  if (is.null(root)) {
    root <- find_repo_root()
  }

  scripts_dir <- fs::path(root, "scripts")
  if (!fs::dir_exists(scripts_dir)) {
    rlang::abort("scripts/ directory not found; cannot run pipeline.")
  }

  pipeline_env <- new.env(parent = globalenv())
  pipeline_env$repo_root <- root

  scripts <- c(
    "00_setup.R",
    "05_ingest_sources.R",
    "07_process_data.R",
    "10_build_themes.R",
    "20_build_indices.R",
    "80_write_outputs.R"
  )

  for (script in scripts) {
    script_path <- fs::path(scripts_dir, script)
    if (!fs::file_exists(script_path)) {
      rlang::abort(glue::glue("Required script missing: {script_path}"))
    }
    sys.source(script_path, envir = pipeline_env)
  }

  invisible(TRUE)
}
