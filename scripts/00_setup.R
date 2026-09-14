suppressPackageStartupMessages({
  library(dplyr)
  library(fs)
  library(glue)
  library(janitor)
  library(purrr)
  library(readr)
  library(rlang)
  library(sf)
  library(stringr)
  library(tidyr)
  library(tibble)
  library(tigris)
  library(yaml)
  library(openxlsx)
  library(readxl)
})

# blsAPI is deliberately not attached here. It was archived from CRAN on
# 2021-07-05, so `library(blsAPI)` made the whole pipeline un-installable from a
# clean checkout. The only call site uses the fully qualified `blsAPI::blsQCEW()`
# inside a block that is currently guarded off, so attaching it was redundant.
# Sourcing this package is a decision for the work that reinstates the QCEW path
# -- see docs/refactor_plan.md F-05 and F-16.

if (!exists("repo_root", inherits = FALSE) || is.null(repo_root)) {
  repo_root <- tryCatch(find_repo_root(), error = function(e) NULL)
  if (is.null(repo_root)) {
    repo_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  }
}

r_files <- list.files(fs::path(repo_root, "R"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
for (f in r_files) {
  source(f)
}

config <- load_Electro_Industrial_config(repo_root)
weights <- load_Electro_Industrial_weights(repo_root)
index_definition <- load_index_definition(repo_root)
missing_data <- load_missing_data(repo_root)
set_Electro_Industrial_options(config, weights, index_definition, missing_data)

paths <- resolve_paths(config, repo_root)
options(Electro_Industrial.paths = paths, Electro_Industrial.root = repo_root)

fs::dir_create(paths$processed_dir, recurse = TRUE)
fs::dir_create(paths$raw_dir, recurse = TRUE)
fs::dir_create(paths$examples_dir, recurse = TRUE)
