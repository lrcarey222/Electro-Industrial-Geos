suppressPackageStartupMessages({
  library(dplyr)
  library(fs)
  library(glue)
  library(janitor)
  library(purrr)
  library(readr)
  library(rlang)
  library(stringr)
  library(tidyr)
  library(tibble)
  library(yaml)
  library(openxlsx)
})

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

config <- load_Electro-Industrial_config(repo_root)
weights <- load_Electro-Industrial_weights(repo_root)
index_definition <- load_index_definition(repo_root)
missing_data <- load_missing_data(repo_root)
set_Electro-Industrial_options(config, weights, index_definition, missing_data)

paths <- resolve_paths(config, repo_root)
options(Electro-Industrial.paths = paths, Electro-Industrial.root = repo_root)

fs::dir_create(paths$processed_dir, recurse = TRUE)
fs::dir_create(paths$raw_dir, recurse = TRUE)
fs::dir_create(paths$examples_dir, recurse = TRUE)
