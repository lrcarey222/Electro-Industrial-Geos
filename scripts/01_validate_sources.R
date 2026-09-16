#!/usr/bin/env Rscript
#
# Validate config/sources.yml against config/index_definition.yml.
#
# Runs in CI with no network and no data. The check that matters is coverage:
# every indicator the index consumes must be claimed by exactly one source. An
# indicator with no source has no provenance, no owner and no staleness SLA,
# which is precisely how eight indicators came to hold nothing but three rows of
# sample data while the pipeline reported success.
#
# Usage:
#   Rscript scripts/01_validate_sources.R
#
# Exit codes: 0 = valid, 1 = invalid.

# Deliberately light: yaml, fs, glue and rlang only. No geospatial stack, no
# data reads, no network -- so this can run as soon as dependencies are
# installed and well before anything touches the pipeline.
suppressPackageStartupMessages({
  library(fs)
  library(glue)
  library(rlang)
  library(yaml)
})

repo_root <- normalizePath(
  file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), ".."),
  winslash = "/",
  mustWork = FALSE
)
if (is.na(repo_root) || !dir.exists(repo_root)) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

`%||%` <- function(x, y) if (!is.null(x)) x else y
source(file.path(repo_root, "R", "sources_registry.R"))

registry <- load_sources_registry(repo_root)
index_definition <- yaml::read_yaml(file.path(repo_root, "config", "index_definition.yml"))

problems <- validate_sources_registry(registry, index_definition)

n_sources <- length(registry$sources)
declared <- unique(unlist(
  lapply(index_definition$categories %||% list(), function(d) d$variables %||% character(0)),
  use.names = FALSE
))

if (length(problems) > 0) {
  cat(sprintf("validate_sources: %d problem(s) in config/sources.yml\n\n", length(problems)))
  for (p in problems) {
    cat("  - ", p, "\n", sep = "")
  }
  cat("\n")
  quit(status = 1)
}

cat(sprintf(
  "validate_sources: %d sources, all %d indicators claimed exactly once\n",
  n_sources, length(declared)
))

# Visibility, not a failure: count the decisions still outstanding. These are
# conservative defaults awaiting a human, and the registry is honest about them
# rather than silently asserting a publisher or a licence.
count_if <- function(f) sum(vapply(registry$sources, f, logical(1)))

todo_confirm <- count_if(function(s) any(grepl("TODO-CONFIRM", unlist(s, use.names = FALSE))))
no_steward <- count_if(function(s) identical(as.character(s$steward %||% ""), "TODO"))
unknown <- count_if(function(s) identical(s$access_class, "unknown"))
no_endpoint <- count_if(function(s) {
  is.null(s$endpoint) && s$access_class %in% c("api", "file_url")
})

cat("validate_sources: outstanding decisions (not failures)\n")
cat(sprintf("  %2d source(s) carry a TODO-CONFIRM\n", todo_confirm))
cat(sprintf("  %2d source(s) have no steward assigned\n", no_steward))
cat(sprintf("  %2d source(s) are access_class 'unknown' (origin untraced)\n", unknown))
if (no_endpoint > 0) {
  cat(sprintf("  %2d automatable source(s) have no endpoint recorded\n", no_endpoint))
}
if (no_steward > 0) {
  cat("  Populate config/stewards.yml from stewards.example.yml, then set `steward:` per source.\n")
}

quit(status = 0)
