library(testthat)

# Implementation comes from tests/testthat/setup.R.
#
# Guards docs/refactor_plan.md F-12: download_with_cache() checked nothing, so
# an HTTP error body was written to the destination path and then treated as a
# valid cached file forever. 19 of 22 staged EIA workbooks are HTML error pages
# because of it.
#
# `downloader` is injected throughout, so none of this touches the network.

HTML_ERROR <- paste0(
  "<!DOCTYPE html>\n<html><head><title>404 - File or directory not found.",
  "</title></head><body><h2>404 - File or directory not found.</h2></body></html>"
)

writes_html <- function(url, destfile) writeLines(HTML_ERROR, destfile)
writes_xlsx <- function(url, destfile) {
  con <- file(destfile, "wb")
  on.exit(close(con), add = TRUE)
  writeBin(c(charToRaw("PK"), as.raw(c(0x03, 0x04)), as.raw(rep(0, 200))), con)
}
writes_nothing <- function(url, destfile) invisible(NULL)
always_fails <- function(url, destfile) stop("connection timed out")

test_that("magic bytes separate a workbook from an error page", {
  d <- withr::local_tempdir()
  xlsx <- file.path(d, "real.xlsx"); writes_xlsx(NULL, xlsx)
  html <- file.path(d, "fake.xlsx"); writes_html(NULL, html)

  expect_true(is_valid_xlsx(xlsx))
  expect_false(is_valid_xlsx(html))
  expect_true(looks_like_html(html))
  expect_false(looks_like_html(xlsx))

  # A file too short to have a signature is not a workbook.
  empty <- file.path(d, "empty.xlsx"); file.create(empty)
  expect_false(is_valid_xlsx(empty))
  expect_false(looks_like_html(empty))

  # A SHORT error page must still be caught. Sniffing by requiring a fixed
  # number of bytes misses these, and error bodies are short by nature -- the
  # EIA 404 is about 150 bytes against a 512-byte sniff.
  tiny <- file.path(d, "tiny.xlsx")
  writeLines("<html><body>404</body></html>", tiny)
  expect_lt(file.size(tiny), 512)
  expect_true(looks_like_html(tiny))
  expect_false(is_valid_xlsx(tiny))
  expect_false(default_download_validator("tiny.xlsx")(tiny)$ok)
})

test_that("the default validator rejects what poisoned the cache", {
  d <- withr::local_tempdir()
  v <- default_download_validator("august_generator2024.xlsx")

  html <- file.path(d, "a.xlsx"); writes_html(NULL, html)
  res <- v(html)
  expect_false(res$ok)
  expect_match(res$reason, "HTML document")

  good <- file.path(d, "b.xlsx"); writes_xlsx(NULL, good)
  expect_true(v(good)$ok)

  empty <- file.path(d, "c.xlsx"); file.create(empty)
  expect_false(v(empty)$ok)
  expect_match(v(empty)$reason, "empty")

  expect_false(v(file.path(d, "missing.xlsx"))$ok)
})

test_that("an HTML page is still rejected when the extension is .zip", {
  d <- withr::local_tempdir()
  html <- file.path(d, "SAGDP.zip"); writes_html(NULL, html)
  expect_false(default_download_validator("SAGDP.zip")(html)$ok)
})

test_that("HTML is accepted when HTML is what was asked for", {
  d <- withr::local_tempdir()
  page <- file.path(d, "p.html"); writes_html(NULL, page)
  expect_true(default_download_validator("p.html")(page)$ok)
})

# --- The behaviour that matters --------------------------------------------

test_that("a bad response is never written to the destination", {
  d <- withr::local_tempdir()
  expect_error(
    download_with_cache("http://x/august_generator2024.xlsx", d, "2026-01-01",
                        filename = "august_generator2024.xlsx",
                        retries = 2L, backoff_seconds = 0, downloader = writes_html),
    "Failed to download"
  )
  # The whole point: no poisoned file is left behind, and no .part either.
  expect_false(file.exists(file.path(d, "august_generator2024.xlsx")))
  expect_length(list.files(d), 0L)
})

test_that("a poisoned cache heals instead of being returned forever", {
  d <- withr::local_tempdir()
  dest <- file.path(d, "gen.xlsx")
  writes_html(NULL, dest)
  expect_true(file.exists(dest))

  expect_warning(
    got <- download_with_cache("http://x/gen.xlsx", d, "2026-01-01", filename = "gen.xlsx",
                               retries = 1L, backoff_seconds = 0, downloader = writes_xlsx),
    "Discarding cached"
  )
  expect_equal(normalizePath(got), normalizePath(dest))
  expect_true(is_valid_xlsx(dest))
  expect_false(looks_like_html(dest))
})

test_that("a valid cached file is returned without downloading again", {
  d <- withr::local_tempdir()
  dest <- file.path(d, "gen.xlsx")
  writes_xlsx(NULL, dest)
  before <- file.mtime(dest)

  exploding <- function(url, destfile) stop("must not be called")
  got <- download_with_cache("http://x/gen.xlsx", d, "2026-01-01", filename = "gen.xlsx",
                             downloader = exploding)
  expect_equal(normalizePath(got), normalizePath(dest))
  expect_equal(file.mtime(dest), before)
})

test_that("a download that writes nothing fails rather than caching a ghost", {
  d <- withr::local_tempdir()
  expect_error(
    download_with_cache("http://x/gen.xlsx", d, "2026-01-01", filename = "gen.xlsx",
                        retries = 1L, backoff_seconds = 0, downloader = writes_nothing),
    "Failed to download"
  )
  expect_length(list.files(d), 0L)
})

test_that("transient failures are retried, and a late success is kept", {
  d <- withr::local_tempdir()
  attempts <- 0L
  flaky <- function(url, destfile) {
    attempts <<- attempts + 1L
    if (attempts < 3L) stop("connection reset")
    writes_xlsx(url, destfile)
  }
  got <- download_with_cache("http://x/gen.xlsx", d, "2026-01-01", filename = "gen.xlsx",
                             retries = 3L, backoff_seconds = 0, downloader = flaky)
  expect_equal(attempts, 3L)
  expect_true(is_valid_xlsx(got))
})

test_that("retries are bounded and the final error names the cause", {
  d <- withr::local_tempdir()
  expect_error(
    download_with_cache("http://x/gen.xlsx", d, "2026-01-01", filename = "gen.xlsx",
                        retries = 2L, backoff_seconds = 0, downloader = always_fails),
    "connection timed out"
  )
})

test_that("validation can be switched off for a file with no signature", {
  d <- withr::local_tempdir()
  writes_text <- function(url, destfile) writeLines("a,b\n1,2", destfile)
  got <- download_with_cache("http://x/data.csv", d, "2026-01-01", filename = "data.csv",
                             validate = FALSE, retries = 1L, backoff_seconds = 0,
                             downloader = writes_text)
  expect_true(file.exists(got))
})

test_that("the staged EIA workbooks are the problem F-12 describes", {
  # Reads whatever is on disk; skipped where nothing is staged. This is the
  # check that would have caught the poisoning in the first place.
  dir <- file.path(repo_root, "data", "raw", "remote")
  skip_if_not(dir.exists(dir), "no remote directory staged")
  files <- list.files(dir, pattern = "generator.*[.]xlsx$", full.names = TRUE)
  skip_if(length(files) == 0, "no generator workbooks staged")

  valid <- vapply(files, is_valid_xlsx, logical(1))
  # Whatever the ratio locally, any file that fails must be HTML rather than
  # some other corruption -- that is the signature of the bug.
  for (f in files[!valid]) {
    expect_true(looks_like_html(f), info = basename(f))
  }
  # And at least one real workbook must exist, or the indicator has no source.
  expect_gt(sum(valid), 0)
})
