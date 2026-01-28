#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(script_arg) > 0) {
  sub("^--file=", "", script_arg[1])
} else {
  ""
}

if (script_path == "") {
  stop("Unable to determine script path; run via Rscript scripts/run_pipeline.R")
}

repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
runner <- file.path(repo_root, "run_pipeline.R")
if (!file.exists(runner)) {
  stop("run_pipeline.R not found at repository root.")
}

source(runner)
