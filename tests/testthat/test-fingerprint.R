library(testthat)

# Implementation comes from tests/testthat/setup.R.
#
# Guards docs/refactor_plan.md F-24. The manifest's sha256 answers "is this the
# same data the published number came from". Hashing the file as it sits on disk
# answered a different question: re-seeding on Windows changed
# gjf_subsidy_tracker's digest while its row count, geography count and schema
# fingerprint were byte-identical, because git had rewritten the line endings.

write_bytes <- function(path, text) {
  con <- file(path, "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(text), con)
}

test_that("the same content digests the same whatever the line endings", {
  d <- withr::local_tempdir()
  lf   <- file.path(d, "lf.csv");   write_bytes(lf,   "state,value\nCA,1\nTX,2\n")
  crlf <- file.path(d, "crlf.csv"); write_bytes(crlf, "state,value\r\nCA,1\r\nTX,2\r\n")
  cr   <- file.path(d, "cr.csv");   write_bytes(cr,   "state,value\rCA,1\rTX,2\r")

  expect_equal(file_sha256(lf), file_sha256(crlf))
  expect_equal(file_sha256(lf), file_sha256(cr))

  # And the file on disk really is different, so this is not a vacuous test.
  expect_false(identical(
    tools::md5sum(lf)[[1]], tools::md5sum(crlf)[[1]]
  ))
})

test_that("different content still digests differently", {
  d <- withr::local_tempdir()
  a <- file.path(d, "a.csv"); write_bytes(a, "state,value\nCA,1\n")
  b <- file.path(d, "b.csv"); write_bytes(b, "state,value\nCA,2\n")
  expect_false(identical(file_sha256(a), file_sha256(b)))
})

test_that("a trailing-newline difference is still a difference", {
  # Normalising line endings must not also normalise their presence.
  d <- withr::local_tempdir()
  a <- file.path(d, "a.csv"); write_bytes(a, "x\ny\n")
  b <- file.path(d, "b.csv"); write_bytes(b, "x\ny")
  expect_false(identical(file_sha256(a), file_sha256(b)))
})

test_that("binary files are hashed exactly, not normalised", {
  d <- withr::local_tempdir()
  # A CR byte inside binary content must be left alone: rewriting bytes inside
  # a ZIP or shapefile would be corruption, not normalisation.
  p1 <- file.path(d, "a.bin")
  con <- file(p1, "wb"); writeBin(as.raw(c(0x50, 0x4B, 0x00, 0x0D, 0x0A, 0x01)), con); close(con)
  p2 <- file.path(d, "b.bin")
  con <- file(p2, "wb"); writeBin(as.raw(c(0x50, 0x4B, 0x00, 0x0A, 0x01)), con); close(con)

  expect_true(is_binary_file(p1))
  expect_false(identical(file_sha256(p1), file_sha256(p2)))
  # Matches the plain file digest, since nothing is rewritten.
  expect_equal(file_sha256(p1), digest::digest(p1, algo = "sha256", file = TRUE))
})

test_that("binary detection is by content, not extension", {
  d <- withr::local_tempdir()
  text_with_odd_ext <- file.path(d, "data.xlsx")
  write_bytes(text_with_odd_ext, "not,really\r\na,workbook\r\n")
  expect_false(is_binary_file(text_with_odd_ext))

  binary_with_text_ext <- file.path(d, "data.csv")
  con <- file(binary_with_text_ext, "wb"); writeBin(as.raw(c(1, 0, 2)), con); close(con)
  expect_true(is_binary_file(binary_with_text_ext))
})

test_that("absent and empty files behave sensibly", {
  d <- withr::local_tempdir()
  expect_true(is.na(file_sha256(file.path(d, "nope.csv"))))

  empty <- file.path(d, "empty.csv"); file.create(empty)
  expect_false(is_binary_file(empty))
  expect_match(file_sha256(empty), "^[0-9a-f]{64}$")
})

test_that("the digest is a stable 64-character hex string", {
  d <- withr::local_tempdir()
  p <- file.path(d, "x.csv"); write_bytes(p, "a,b\n1,2\n")
  first <- file_sha256(p)
  expect_match(first, "^[0-9a-f]{64}$")
  expect_equal(first, file_sha256(p))
})

test_that("real staged text and binary sources both digest without error", {
  dir <- file.path(repo_root, "data", "raw")
  skip_if_not(dir.exists(dir), "no raw directory staged")
  candidates <- c(
    file.path(dir, "Regdata_subnational.csv"),
    file.path(dir, "FCC_PEA_website.xlsx")
  )
  candidates <- candidates[file.exists(candidates)]
  skip_if(length(candidates) == 0, "no staged sources to fingerprint")

  for (p in candidates) {
    expect_match(file_sha256(p), "^[0-9a-f]{64}$", info = basename(p))
  }
})
