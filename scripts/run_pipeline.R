#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(targets)
})

if (!file.exists("_targets.R")) {
  stop("Run this script from the repository root.")
}

tar_make()
