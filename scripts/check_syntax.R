#!/usr/bin/env Rscript
#
# Parse every R source file in the repository and fail if any of them is
# syntactically invalid.
#
# This is not a pipeline stage -- it is a guard. `run_Electro_Industrial_pipeline()`
# loads each stage with `sys.source()`, which parses a whole file before
# evaluating any of it, so a single unbalanced brace takes the entire pipeline
# down. That is exactly what happened: scripts/07_process_data.R stopped parsing
# at commit a17d212 and stayed broken for 13 commits, because nothing in CI ever
# checked. Keep this running ahead of the test and smoke-test steps so the
# failure is reported in one line instead of buried in a stack trace.
#
# Usage:
#   Rscript scripts/check_syntax.R
#
# Exit codes: 0 = all files parse, 1 = one or more failed.

repo_root <- normalizePath(
  file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), ".."),
  winslash = "/",
  mustWork = FALSE
)
if (is.na(repo_root) || !dir.exists(repo_root)) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

# Directories searched for `.R` / `.Rprofile` sources, plus root-level scripts.
search_dirs <- c("R", "scripts", "tests")

# Deliberate exclusions. Each needs a reason and a tracking reference; do not
# add to this list to silence a new failure.
#
#   Legacy Script/ -- Electro-Industrial_State.R does not parse: a global
#     find-and-replace put hyphens into ~61 bare R identifiers, and there is a
#     second incomplete expression at line 403. It is retained as the
#     methodology record of reference and is never sourced by the pipeline.
#     See docs/refactor_plan.md F-02, which must be resolved before it is either
#     repaired or retired.
exclude_dirs <- c("Legacy Script", "renv")

files <- character(0)
for (d in search_dirs) {
  dir_path <- file.path(repo_root, d)
  if (dir.exists(dir_path)) {
    files <- c(files, list.files(dir_path, pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE))
  }
}
files <- c(files, list.files(repo_root, pattern = "[.][Rr]$", full.names = TRUE))

rel <- sub(paste0("^", gsub("([.|()\\^{}+$*?\\[\\]])", "\\\\\\1", repo_root), "/?"), "", files)
keep <- !vapply(
  rel,
  function(p) any(startsWith(p, paste0(exclude_dirs, "/"))),
  logical(1)
)
files <- files[keep]
rel <- rel[keep]

ord <- order(rel)
files <- files[ord]
rel <- rel[ord]

if (length(files) == 0) {
  cat("check_syntax: no R files found under", repo_root, "\n")
  quit(status = 1)
}

failures <- character(0)
for (i in seq_along(files)) {
  err <- tryCatch(
    {
      parse(files[i])
      NA_character_
    },
    error = function(e) sub("\n.*", "", conditionMessage(e))
  )
  if (!is.na(err)) {
    failures <- c(failures, sprintf("  %s\n    %s", rel[i], err))
  }
}

if (length(failures) > 0) {
  cat(sprintf(
    "check_syntax: %d of %d R files failed to parse\n\n%s\n\n",
    length(failures), length(files), paste(failures, collapse = "\n")
  ))
  cat("A file that does not parse cannot be run at all -- sys.source() parses\n")
  cat("before it evaluates, so this breaks the whole pipeline, not one stage.\n")
  quit(status = 1)
}

# DESCRIPTION must be readable as a Debian Control File. It is how CI resolves
# dependencies (`setup-r-dependencies` reads it), so a malformed continuation
# line silently costs you every declared package. This is not hypothetical: a
# bare `)` at column 0 closing `Authors@R` made the file unreadable, and pak
# failed with "Line starting ') ...' is malformed!" long before any R code ran.
# DCF continuation lines must be indented.
desc_path <- file.path(repo_root, "DESCRIPTION")
if (file.exists(desc_path)) {
  desc_err <- tryCatch(
    {
      read.dcf(desc_path)
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )
  if (!is.na(desc_err)) {
    cat("check_syntax: DESCRIPTION is not a readable control file\n")
    cat("    ", desc_err, "\n\n", sep = "")
    cat("Continuation lines in DESCRIPTION must be indented. CI resolves\n")
    cat("dependencies from this file, so nothing installs while it is malformed.\n")
    quit(status = 1)
  }
}

cat(sprintf("check_syntax: %d R files parse cleanly; DESCRIPTION is well formed\n", length(files)))
quit(status = 0)
