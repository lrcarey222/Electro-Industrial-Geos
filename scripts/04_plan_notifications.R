#!/usr/bin/env Rscript
#
# Build the notification plan for stale sources.
#
# Pure content generation: no network, no GitHub API. Writes a plan the
# workflow reconciles against the issues that already exist. Keeping the two
# apart means the issue bodies are unit-testable offline, which the brief
# requires of everything in CI.
#
# Writes to <out-dir> (default outputs/metadata/notifications/):
#   plan.json          one entry per source needing attention
#   <source_id>.md     the rendered issue body for that source
#
# Usage:
#   Rscript scripts/04_plan_notifications.R [--as-of YYYY-MM-DD]
#                                           [--out-dir <path>]
#                                           [--allow-unassigned]
#                                           [--dry-run]
#
# Exit codes:
#   0  nothing needs attention
#   1  at least one source needs an issue opened or updated
#   3  sources need attention but have no steward, and --allow-unassigned was
#      not passed. Filing an issue nobody is assigned to is how a team learns
#      to ignore the board, so this fails loudly instead.

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
for (f in c("sources_registry.R", "manifest.R", "freshness.R", "notify.R")) {
  source(file.path(repo_root, "R", f))
}

as_of <- as.Date(arg_value("--as-of", as.character(Sys.Date())))
if (is.na(as_of)) stop("--as-of must be a YYYY-MM-DD date", call. = FALSE)
out_dir <- arg_value("--out-dir", file.path(repo_root, "outputs", "metadata", "notifications"))

registry <- load_sources_registry(repo_root)
index_definition <- yaml::read_yaml(file.path(repo_root, "config", "index_definition.yml"))
problems <- validate_sources_registry(registry, index_definition)
if (length(problems) > 0) {
  cat("plan_notifications: config/sources.yml is invalid; refusing to notify\n")
  for (p in problems) cat("  - ", p, "\n", sep = "")
  quit(status = 2)
}

manifest <- manifest_read(manifest_path(repo_root))
freshness <- compute_freshness(registry, manifest, as_of = as_of)
plan <- notification_plan(registry, freshness, as_of = as_of)

if (length(plan) == 0) {
  cat("plan_notifications: every source is within its SLA; nothing to notify\n")
  quit(status = 0)
}

unassigned <- vapply(plan, function(p) is.na(p$assignee), logical(1))

if (!has_flag("--dry-run")) {
  fs::dir_create(out_dir, recurse = TRUE)
  # Clear stale bodies so a source that has recovered does not leave one behind.
  old <- fs::dir_ls(out_dir, glob = "*.md", fail = FALSE)
  if (length(old) > 0) fs::file_delete(old)

  for (p in plan) {
    writeLines(p$body, file.path(out_dir, paste0(p$source_id, ".md")))
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to write plan.json", call. = FALSE)
  }
  jsonlite::write_json(
    lapply(plan, function(p) {
      p$body <- NULL # bodies live in their own files; keep the plan readable
      p$body_file <- paste0(p$source_id, ".md")
      p
    }),
    file.path(out_dir, "plan.json"),
    auto_unbox = TRUE, pretty = TRUE, na = "null"
  )
}

cat(sprintf(
  "plan_notifications: %d source(s) need attention (as of %s)\n",
  length(plan), as.character(as_of)
))
for (p in plan) {
  cat(sprintf(
    "  [%-7s] %-32s overdue=%-5s escalate=%-4s assignee=%s\n",
    toupper(p$status), p$source_id, p$days_overdue,
    ifelse(is.na(p$escalation), "-", p$escalation),
    ifelse(is.na(p$assignee), "UNASSIGNED", p$assignee)
  ))
}

if (any(unassigned) && !has_flag("--allow-unassigned")) {
  cat(sprintf(
    paste0(
      "\nplan_notifications: %d of %d source(s) needing attention have no steward.\n",
      "Refusing to file issues nobody is assigned to -- an unassigned issue is how a\n",
      "team learns to ignore the board.\n\n",
      "Fix: copy config/stewards.example.yml to config/stewards.yml, then set\n",
      "`steward:` on each source in config/sources.yml.\n\n",
      "Override deliberately with --allow-unassigned.\n"
    ),
    sum(unassigned), length(plan)
  ))
  quit(status = 3)
}

quit(status = 1)
