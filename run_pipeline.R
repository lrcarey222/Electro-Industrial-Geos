#!/usr/bin/env Rscript

find_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(current, ".git")) ||
      length(list.files(current, pattern = "\\.Rproj$")) > 0) {
      return(current)
    }
    parent <- dirname(current)
    if (parent == current) {
      break
    }
    current <- parent
  }
  stop("Unable to locate repository root.")
}

repo_root <- find_root()
source(file.path(repo_root, "R", "utils", "path_helpers.R"))
source(file.path(repo_root, "R", "utils", "pipeline.R"))

run_Electro_Industrial_pipeline(repo_root)
