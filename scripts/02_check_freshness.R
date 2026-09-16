#!/usr/bin/env Rscript
#
# Freshness check: how old is every input, and is anyone accountable for it?
#
# Joins config/sources.yml to data/_state/manifest.csv, measures each source
# against its own SLA, and writes:
#
#   outputs/metadata/data_status.json   machine-readable, for the dashboard
#   docs/data_status.md                 human-readable, committed so status is
#                                       visible without opening Actions
#
# Needs no network and no pipeline data.
#
# Usage:
#   Rscript scripts/02_check_freshness.R [--as-of YYYY-MM-DD] [--quiet]
#                                        [--no-write] [--markdown <path>]
#
# Exit codes: 0 all fresh, 1 one or more warn/pending, 2 one or more fail.
# The non-zero codes are informational for a human and are what the scheduled
# workflow keys its issue filing off; they are deliberately NOT wired into the
# main CI build, because a stale source is a fact to be reported, not a reason
# to block a code change.

suppressPackageStartupMessages({
  library(dplyr)
  library(fs)
  library(glue)
  library(readr)
  library(rlang)
  library(tibble)
  library(yaml)
})

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (length(hit) == 0 || hit[1] == length(args)) default else args[hit[1] + 1]
}
has_flag <- function(flag) flag %in% args

repo_root <- normalizePath(
  file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), ".."),
  winslash = "/",
  mustWork = FALSE
)
if (is.na(repo_root) || !dir.exists(repo_root)) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

`%||%` <- function(x, y) if (!is.null(x)) x else y
for (f in c("path_helpers.R", "sources_registry.R", "manifest.R", "freshness.R")) {
  candidate <- file.path(repo_root, "R", f)
  if (!file.exists(candidate)) {
    candidate <- file.path(repo_root, "R", "utils", f)
  }
  source(candidate)
}

as_of <- as.Date(arg_value("--as-of", as.character(Sys.Date())))
if (is.na(as_of)) {
  stop("--as-of must be a YYYY-MM-DD date", call. = FALSE)
}

registry <- load_sources_registry(repo_root)
index_definition <- yaml::read_yaml(file.path(repo_root, "config", "index_definition.yml"))

# Refuse to report on a registry that does not describe the index. A freshness
# board that silently omits an indicator is worse than no board.
problems <- validate_sources_registry(registry, index_definition)
if (length(problems) > 0) {
  cat("check_freshness: config/sources.yml is invalid; run scripts/01_validate_sources.R\n")
  for (p in problems) cat("  - ", p, "\n", sep = "")
  quit(status = 2)
}

manifest <- manifest_read(manifest_path(repo_root))
freshness <- compute_freshness(registry, manifest, as_of = as_of)
code <- freshness_exit_code(freshness)

if (!has_flag("--no-write")) {
  json_path <- file.path(repo_root, "outputs", "metadata", "data_status.json")
  fs::dir_create(fs::path_dir(json_path), recurse = TRUE)
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to write data_status.json", call. = FALSE)
  }
  jsonlite::write_json(
    freshness_summary(freshness, as_of = as_of),
    json_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )

  md_path <- arg_value("--markdown", file.path(repo_root, "docs", "data_status.md"))
  fs::dir_create(fs::path_dir(md_path), recurse = TRUE)
  writeLines(freshness_markdown(freshness, as_of = as_of), md_path)
}

if (!has_flag("--quiet")) {
  counts <- table(factor(freshness$status, levels = c("ok", "warn", "fail", "pending")))
  cat(sprintf(
    "check_freshness: %d sources -- %d ok, %d warn, %d fail, %d pending (as of %s)\n",
    nrow(freshness), counts[["ok"]], counts[["warn"]], counts[["fail"]],
    counts[["pending"]], as.character(as_of)
  ))

  flagged <- freshness[freshness$status != "ok", ]
  if (nrow(flagged) > 0) {
    cat("\n")
    for (i in seq_len(nrow(flagged))) {
      r <- flagged[i, ]
      age <- if (is.na(r$age_days)) "no vintage recorded" else paste0(r$age_days, "d old")
      owner <- if (is.na(r$steward) || r$steward == "TODO") "UNASSIGNED" else r$steward
      cat(sprintf(
        "  [%-7s] %-32s %-22s warn@%s fail@%s  steward=%s\n",
        toupper(r$status), r$source_id, age,
        r$warn_after_days, r$fail_after_days, owner
      ))
    }
    cat("\n")
  }
}

quit(status = code)
