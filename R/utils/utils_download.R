# Downloading with validation.
#
# The previous implementation was four lines and checked nothing:
#
#   if (!fs::file_exists(dest_path)) {
#     utils::download.file(url, destfile = dest_path, mode = "wb", quiet = TRUE)
#   }
#   dest_path
#
# No status check, no content-type check, no magic-byte check, and no atomic
# write. An HTTP error body was written straight to the destination path, and
# `fs::file_exists()` then reported it as present -- so the next run
# short-circuited on existence and never retried. The poison persisted.
#
# That is not a hypothetical: 19 of the 22 `*_generator*.xlsx` files staged in
# data/raw/remote/ are HTML error pages saved under an .xlsx extension, three of
# them committed to git, while all three genuine workbooks are untracked. A
# fresh clone got no usable EIA-860M data at all. See docs/refactor_plan.md
# F-12.

#' First bytes of a file, as raw
#'
#' Reads *up to* `n` bytes. Requiring exactly `n` would mean a short response
#' returned nothing and sailed past every check -- and error pages are short:
#' the EIA 404 body is about 150 bytes, well under any sensible sniff length.
#'
#' @param path File path.
#' @param n Maximum number of bytes to read.
#' @return A raw vector, empty if the file is absent or unreadable.
#' @keywords internal
file_head_bytes <- function(path, n = 2L) {
  if (!fs::file_exists(path) || fs::file_size(path) == 0) {
    return(raw(0))
  }
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  readBin(con, what = "raw", n = n)
}

#' Does this file look like a ZIP container?
#'
#' `.xlsx` and `.zip` are both ZIP containers, so the `PK` signature is what
#' separates a real workbook from an error page with the right extension.
#'
#' Compares raw bytes. Converting to a string first would be wrong: a real
#' workbook's header is binary, and `rawToChar()` on it produces an invalid
#' multibyte string that makes downstream string functions throw.
#'
#' @param path File path.
#' @return `TRUE` or `FALSE`.
#' @export
is_valid_xlsx <- function(path) {
  identical(file_head_bytes(path, 2L), charToRaw("PK"))
}

#' Does this file look like an HTML document?
#'
#' Publishers serve human-readable error pages with a 200 status far more often
#' than they 404 cleanly, so this is the check that actually catches them.
#'
#' Matching is done with `useBytes = TRUE` and NULs are dropped first, because
#' the input may be arbitrary binary. Without that, sniffing a genuine workbook
#' raises "invalid multibyte string" and the validator rejects a perfectly good
#' file -- which is worse than the bug it is trying to catch.
#'
#' @param path File path.
#' @return `TRUE` or `FALSE`.
#' @export
looks_like_html <- function(path) {
  head_bytes <- file_head_bytes(path, 1024L)
  if (length(head_bytes) == 0) {
    return(FALSE)
  }
  text <- tryCatch(
    rawToChar(head_bytes[head_bytes != as.raw(0)]),
    error = function(e) ""
  )
  if (!nzchar(text)) {
    return(FALSE)
  }
  isTRUE(tryCatch(
    grepl("^[[:space:]]*(<!doctype html|<html|<head|<\\?xml)", text,
          ignore.case = TRUE, useBytes = TRUE) ||
      grepl("<html[ >]|<title>[^<]*(404|not found|error)", text,
            ignore.case = TRUE, useBytes = TRUE),
    error = function(e) FALSE
  ))
}

#' Default validator for a downloaded file, inferred from its name
#'
#' Returns a function of one argument (the path) giving `list(ok, reason)`.
#' Anything that looks like HTML is rejected unless HTML was asked for, because
#' that is the shape every one of the poisoned files took.
#'
#' @param filename Name the file will be stored under.
#' @return A validator function.
#' @export
default_download_validator <- function(filename) {
  ext <- tolower(tools::file_ext(filename %||% ""))
  function(path) {
    if (!fs::file_exists(path)) {
      return(list(ok = FALSE, reason = "no file was written"))
    }
    if (fs::file_size(path) == 0) {
      return(list(ok = FALSE, reason = "file is empty"))
    }
    if (!ext %in% c("html", "htm") && looks_like_html(path)) {
      return(list(ok = FALSE, reason = sprintf(
        "server returned an HTML document (%s bytes), not a .%s file",
        format(fs::file_size(path)), ext
      )))
    }
    if (ext %in% c("xlsx", "xlsm", "zip") && !is_valid_xlsx(path)) {
      return(list(ok = FALSE, reason = sprintf(
        "expected a ZIP container (.%s) but the file does not start with 'PK'", ext
      )))
    }
    list(ok = TRUE, reason = NA_character_)
  }
}

#' Download a file with caching, validation and an atomic write
#'
#' Three things the previous version did not do:
#'
#' * **Validate.** A response that is not the file that was asked for is
#'   rejected rather than cached.
#' * **Heal a poisoned cache.** An existing file that fails validation is
#'   deleted and re-fetched, instead of being short-circuited on existence
#'   forever.
#' * **Write atomically.** The download lands in a temporary file beside the
#'   destination and is renamed only once it validates, so an interrupted or
#'   failed download can never leave a half-file that looks present.
#'
#' @param url URL to download.
#' @param dest_dir Directory to store cached files.
#' @param snapshot_date Snapshot date used in the default cache key.
#' @param filename Optional filename override.
#' @param validate Validator function; defaults to one inferred from the
#'   filename. Pass `FALSE` to skip validation entirely.
#' @param retries Attempts before giving up.
#' @param backoff_seconds Base delay between attempts, doubled each retry.
#' @param downloader Function called as `downloader(url, destfile)`. Injectable
#'   so the retry and validation logic is testable without network.
#' @return Path to the validated cached file.
#' @export
download_with_cache <- function(url, dest_dir, snapshot_date, filename = NULL,
                                validate = NULL, retries = 3L,
                                backoff_seconds = 1, downloader = NULL) {
  fs::dir_create(dest_dir, recurse = TRUE)
  safe_name <- filename %||% paste0(gsub("[^A-Za-z0-9]+", "_", basename(url)), "_", snapshot_date)
  dest_path <- fs::path(dest_dir, safe_name)

  validator <- if (isFALSE(validate)) {
    function(path) list(ok = TRUE, reason = NA_character_)
  } else {
    validate %||% default_download_validator(safe_name)
  }
  fetch <- downloader %||% function(url, destfile) {
    utils::download.file(url, destfile = destfile, mode = "wb", quiet = TRUE)
  }

  if (fs::file_exists(dest_path)) {
    cached <- validator(dest_path)
    if (isTRUE(cached$ok)) {
      return(as.character(dest_path))
    }
    # The cached copy is poison. Leaving it would mean this function keeps
    # returning it forever, which is exactly how 19 HTML error pages came to be
    # staged as workbooks.
    rlang::warn(glue::glue(
      "Discarding cached {safe_name}: {cached$reason}. Re-fetching."
    ))
    fs::file_delete(dest_path)
  }

  # A validator that throws must be treated as a failed validation, not allowed
  # to escape: an exception here would skip the cleanup below and leave a .part
  # file behind, which is the same class of litter this function exists to stop.
  safe_validate <- function(path) {
    tryCatch(
      {
        out <- validator(path)
        if (!isTRUE(out$ok)) out$ok <- FALSE
        out
      },
      error = function(e) list(ok = FALSE, reason = paste("validator failed:", conditionMessage(e)))
    )
  }

  last_reason <- "no attempt was made"
  for (attempt in seq_len(max(1L, as.integer(retries)))) {
    tmp <- fs::file_temp(pattern = "dl_", tmp_dir = dest_dir, ext = "part")
    # Cleanup is registered before anything can fail, so no path out of this
    # iteration leaves the partial file behind.
    on.exit(if (fs::file_exists(tmp)) fs::file_delete(tmp), add = TRUE)

    ok <- tryCatch(
      {
        fetch(url, tmp)
        TRUE
      },
      error = function(e) {
        last_reason <<- conditionMessage(e)
        FALSE
      },
      warning = function(w) {
        last_reason <<- conditionMessage(w)
        FALSE
      }
    )

    if (ok) {
      checked <- safe_validate(tmp)
      if (isTRUE(checked$ok)) {
        # Rename only once valid: the destination never exists in a bad state.
        fs::file_move(tmp, dest_path)
        return(as.character(dest_path))
      }
      last_reason <- checked$reason
    }

    if (fs::file_exists(tmp)) {
      fs::file_delete(tmp)
    }
    if (attempt < retries && backoff_seconds > 0) {
      Sys.sleep(backoff_seconds * (2^(attempt - 1)))
    }
  }

  rlang::abort(glue::glue(
    "Failed to download {safe_name} after {retries} attempt(s): {last_reason} ({url})"
  ))
}
