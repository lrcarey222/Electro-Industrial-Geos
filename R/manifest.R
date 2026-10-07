#' Manifest columns, in order
#'
#' The manifest is the per-source provenance record: what was retrieved, when,
#' what the publisher released, and what it looked like. `run_metadata.csv`
#' records the run; this records the inputs.
#'
#' @return Character vector of column names.
#' @export
manifest_columns <- function() {
  c(
    "source_id",
    "retrieved_at_utc",
    "publisher_release_date",
    "vintage_label",
    "sha256",
    "n_rows",
    "n_geographies",
    "schema_fingerprint",
    "ingest_method",
    "ingested_by",
    "status",
    "notes"
  )
}

#' Allowed manifest status values
#'
#' `pending` means no vintage has ever been recorded. It is deliberately not the
#' same as `ok`: an unknown vintage must never read as fresh.
#'
#' @return Character vector.
#' @export
manifest_statuses <- function() {
  c("ok", "stale", "failed", "pending")
}

#' Default manifest path
#'
#' @param root Repo root.
#' @return Path to the manifest.
#' @export
manifest_path <- function(root = find_repo_root()) {
  Sys.getenv("EIG_MANIFEST", fs::path(root, "data", "_state", "manifest.csv"))
}

#' Empty manifest tibble with the correct column types
#'
#' @return Tibble with zero rows.
#' @export
manifest_empty <- function() {
  tibble::tibble(
    source_id = character(),
    retrieved_at_utc = character(),
    publisher_release_date = character(),
    vintage_label = character(),
    sha256 = character(),
    n_rows = integer(),
    n_geographies = integer(),
    schema_fingerprint = character(),
    ingest_method = character(),
    ingested_by = character(),
    status = character(),
    notes = character()
  )
}

#' Read the manifest
#'
#' A missing manifest is not an error: it means nothing has been ingested yet,
#' and every source will read as `pending`.
#'
#' @param path Manifest path.
#' @return Tibble.
#' @export
manifest_read <- function(path = manifest_path()) {
  if (!fs::file_exists(path)) {
    return(manifest_empty())
  }
  parsed <- readr::read_csv(
    path,
    col_types = readr::cols(
      source_id = readr::col_character(),
      retrieved_at_utc = readr::col_character(),
      publisher_release_date = readr::col_character(),
      vintage_label = readr::col_character(),
      sha256 = readr::col_character(),
      n_rows = readr::col_integer(),
      n_geographies = readr::col_integer(),
      schema_fingerprint = readr::col_character(),
      ingest_method = readr::col_character(),
      ingested_by = readr::col_character(),
      status = readr::col_character(),
      notes = readr::col_character()
    ),
    progress = FALSE
  )
  missing <- setdiff(manifest_columns(), names(parsed))
  if (length(missing) > 0) {
    rlang::abort(glue::glue(
      "Manifest at {path} is missing column(s): {paste(missing, collapse = ', ')}"
    ))
  }
  parsed[manifest_columns()]
}

#' Write the manifest
#'
#' Sorted by `source_id` so the committed file produces a readable diff when a
#' single source is refreshed, rather than reordering wholesale.
#'
#' @param manifest Tibble.
#' @param path Destination.
#' @return Invisibly, the manifest.
#' @export
manifest_write <- function(manifest, path = manifest_path()) {
  fs::dir_create(fs::path_dir(path), recurse = TRUE)
  manifest <- manifest[order(manifest$source_id), manifest_columns()]
  readr::write_csv(manifest, path, na = "")
  invisible(manifest)
}

#' Insert or replace a single source's manifest row
#'
#' One row per source: the manifest records the current vintage, while prior
#' vintages are retained in the raw archive.
#'
#' @param manifest Tibble.
#' @param entry Named list with at least `source_id`.
#' @return Updated tibble.
#' @export
manifest_upsert <- function(manifest, entry) {
  if (is.null(entry$source_id) || !nzchar(entry$source_id)) {
    rlang::abort("manifest_upsert() requires a non-empty source_id.")
  }
  if (!is.null(entry$status) && !entry$status %in% manifest_statuses()) {
    rlang::abort(glue::glue(
      "status '{entry$status}' is not one of {paste(manifest_statuses(), collapse = '/')}"
    ))
  }

  row <- manifest_empty()
  row[1, ] <- NA
  row$source_id <- entry$source_id
  for (nm in intersect(names(entry), manifest_columns())) {
    value <- entry[[nm]]
    row[[nm]] <- if (is.null(value)) NA else value
  }

  manifest <- manifest[manifest$source_id != entry$source_id, , drop = FALSE]
  dplyr::bind_rows(manifest, row)
}

#' Is this file binary?
#'
#' Decided by content, not extension: a NUL byte in the first chunk means the
#' file is not text. Extension-based guessing would misclassify the unfamiliar
#' formats this pipeline reads, and getting it wrong in the binary direction
#' would corrupt a digest.
#'
#' @param path File path.
#' @param n Bytes to sniff.
#' @return `TRUE` or `FALSE`.
#' @keywords internal
is_binary_file <- function(path, n = 8192L) {
  if (!fs::file_exists(path) || fs::file_size(path) == 0) {
    return(FALSE)
  }
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  bytes <- readBin(con, what = "raw", n = n)
  any(bytes == as.raw(0))
}

#' SHA-256 of a file's *content*, insensitive to line endings
#'
#' The recorded hash answers "is this the same data the published number came
#' from". Hashing the file as it sits on disk answers a different and less
#' useful question, because git rewrites line endings on checkout: re-seeding
#' the manifest on Windows changed `gjf_subsidy_tracker`'s digest while its row
#' count, geography count and schema fingerprint were byte-identical. A hash
#' that flips on checkout either raises false alarms about a publisher changing
#' a file, or teaches people to ignore the field -- which costs the manifest its
#' main purpose (docs/refactor_plan.md F-24).
#'
#' Text files are therefore normalised to LF before hashing. Binary files are
#' hashed exactly as they are, because normalising bytes inside a ZIP or
#' shapefile would be corruption, not normalisation.
#'
#' Decision 2026-10-05: fingerprint the content and ignore line endings.
#'
#' @param path File path.
#' @return Lowercase hex digest, or `NA` if the file is absent.
#' @export
file_sha256 <- function(path) {
  if (!fs::file_exists(path)) {
    return(NA_character_)
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    rlang::abort("Package 'digest' is required to fingerprint files.")
  }

  if (is_binary_file(path)) {
    return(digest::digest(path, algo = "sha256", file = TRUE))
  }

  size <- fs::file_size(path)
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  bytes <- readBin(con, what = "raw", n = as.numeric(size))

  # CRLF -> LF, then any remaining lone CR -> LF, so a file written on any of
  # the three conventions digests identically.
  cr <- as.raw(13L)
  lf <- as.raw(10L)
  if (length(bytes) > 0) {
    crlf <- which(bytes == cr & c(bytes[-1], as.raw(255L)) == lf)
    if (length(crlf) > 0) {
      bytes <- bytes[-crlf]
    }
    bytes[bytes == cr] <- lf
  }
  digest::digest(bytes, algo = "sha256", serialize = FALSE)
}

#' Schema fingerprint for a data frame
#'
#' Hash of sorted `name:type` pairs. A changed fingerprint means the publisher
#' changed the file on you, which is the single most common way a pipeline like
#' this breaks. Sorting makes the fingerprint insensitive to column reordering,
#' which is a cosmetic change, while remaining sensitive to a renamed, added,
#' removed or retyped column, which is not.
#'
#' @param data A data frame.
#' @return Lowercase hex digest, or NA for a NULL input.
#' @export
schema_fingerprint <- function(data) {
  if (is.null(data)) {
    return(NA_character_)
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    rlang::abort("Package 'digest' is required to fingerprint schemas.")
  }
  pairs <- paste0(names(data), ":", vapply(data, function(x) class(x)[1], character(1)))
  digest::digest(paste(sort(pairs), collapse = "|"), algo = "sha256")
}

#' Count distinct geographies in a data frame
#'
#' Coverage is the cheapest real check there is. Eight indicators in the
#' published vintage held three states' worth of sample data while the pipeline
#' reported success; a recorded geography count makes that visible at ingest.
#'
#' @param data A data frame.
#' @param candidates Column names to try, in order of preference.
#' @return Integer count, or NA if no candidate column is present.
#' @export
count_geographies <- function(data,
                              candidates = c(
                                "state", "State", "STATE", "abbr", "state_abbr",
                                "Location", "jurisdiction", "Jurisdiction",
                                "geo_name", "GeoName", "economic_area", "PEA_Name"
                              )) {
  if (is.null(data)) {
    return(NA_integer_)
  }
  hit <- intersect(candidates, names(data))
  if (length(hit) == 0) {
    return(NA_integer_)
  }
  as.integer(dplyr::n_distinct(data[[hit[1]]], na.rm = TRUE))
}
