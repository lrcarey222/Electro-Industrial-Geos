library(testthat)

# Implementation and repo_root come from tests/testthat/setup.R.

fake_registry <- function(..., warn = 100, fail = 200) {
  overrides <- list(...)
  base <- list(
    id = "src_a", label = "Source A", feeds = c("ind_one"),
    publisher = "Publisher", access_class = "file_url", endpoint = "https://example.invalid/x.csv",
    auth_env = NULL, geography = "state", cadence = "annual",
    typical_release_lag_days = 30, refresh_cron = NULL,
    staleness = list(warn_after_days = warn, fail_after_days = fail),
    steward = "steward-a", can_commit_raw = TRUE, license = "Public domain", notes = ""
  )
  list(sources = list(utils::modifyList(base, overrides)), path = "<memory>")
}

entry <- function(...) {
  manifest_upsert(manifest_empty(), utils::modifyList(
    list(
      source_id = "src_a", retrieved_at_utc = NA_character_,
      publisher_release_date = NA_character_, vintage_label = "v1",
      sha256 = NA_character_, n_rows = 10L, n_geographies = 50L,
      schema_fingerprint = NA_character_, ingest_method = "auto",
      ingested_by = "test", status = "ok", notes = ""
    ),
    list(...)
  ))
}

# ---- manifest ------------------------------------------------------------

test_that("manifest_empty has the documented columns", {
  expect_equal(names(manifest_empty()), manifest_columns())
  expect_equal(nrow(manifest_empty()), 0L)
})

test_that("manifest_upsert replaces rather than appends for the same source", {
  m <- manifest_upsert(manifest_empty(), list(source_id = "a", vintage_label = "v1", status = "ok"))
  m <- manifest_upsert(m, list(source_id = "b", vintage_label = "v1", status = "ok"))
  m <- manifest_upsert(m, list(source_id = "a", vintage_label = "v2", status = "ok"))

  expect_equal(nrow(m), 2L)
  expect_equal(m$vintage_label[m$source_id == "a"], "v2")
})

test_that("manifest_upsert rejects a bad status and a missing id", {
  expect_error(manifest_upsert(manifest_empty(), list(source_id = "a", status = "nonsense")), "not one of")
  expect_error(manifest_upsert(manifest_empty(), list(vintage_label = "v1")), "source_id")
})

test_that("manifest survives a write/read round trip", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  m <- manifest_upsert(manifest_empty(), list(
    source_id = "a", vintage_label = "v1", n_rows = 5L,
    n_geographies = 50L, status = "ok", notes = "hello"
  ))
  manifest_write(m, tmp)
  back <- manifest_read(tmp)

  expect_equal(back$source_id, "a")
  expect_equal(back$n_rows, 5L)
  expect_equal(back$n_geographies, 50L)
  expect_equal(back$notes, "hello")
})

test_that("a missing manifest reads as empty rather than erroring", {
  expect_equal(nrow(manifest_read(file.path(tempdir(), "definitely-not-here.csv"))), 0L)
})

test_that("schema_fingerprint is order-insensitive but change-sensitive", {
  a <- data.frame(state = "CA", value = 1)
  reordered <- data.frame(value = 1, state = "CA")
  renamed <- data.frame(state = "CA", amount = 1)
  retyped <- data.frame(state = "CA", value = "1")
  added <- data.frame(state = "CA", value = 1, extra = TRUE)

  expect_equal(schema_fingerprint(a), schema_fingerprint(reordered))
  expect_false(schema_fingerprint(a) == schema_fingerprint(renamed))
  expect_false(schema_fingerprint(a) == schema_fingerprint(retyped))
  expect_false(schema_fingerprint(a) == schema_fingerprint(added))
  expect_true(is.na(schema_fingerprint(NULL)))
})

test_that("count_geographies finds a key column or returns NA", {
  expect_equal(count_geographies(data.frame(state = c("CA", "TX", "CA"))), 2L)
  expect_equal(count_geographies(data.frame(Jurisdiction = c("a", "b"))), 2L)
  expect_true(is.na(count_geographies(data.frame(x = 1:3))))
  expect_true(is.na(count_geographies(NULL)))
})

# ---- freshness -----------------------------------------------------------

test_that("a source with no manifest row is pending, never ok", {
  f <- compute_freshness(fake_registry(), manifest_empty(), as_of = as.Date("2026-01-01"))
  expect_equal(f$status, "pending")
  expect_true(is.na(f$age_days))
  expect_equal(freshness_exit_code(f), 1L)
})

test_that("a recent vintage is ok", {
  f <- compute_freshness(
    fake_registry(),
    entry(publisher_release_date = "2025-12-01"),
    as_of = as.Date("2026-01-01")
  )
  expect_equal(f$status, "ok")
  expect_equal(f$age_days, 31L)
  expect_equal(f$age_basis, "publisher_release_date")
  expect_equal(freshness_exit_code(f), 0L)
})

test_that("crossing warn and fail thresholds changes status and exit code", {
  warn_row <- compute_freshness(
    fake_registry(warn = 100, fail = 200),
    entry(publisher_release_date = "2025-09-01"),
    as_of = as.Date("2026-01-01")
  )
  expect_equal(warn_row$status, "warn")
  expect_equal(freshness_exit_code(warn_row), 1L)

  fail_row <- compute_freshness(
    fake_registry(warn = 100, fail = 200),
    entry(publisher_release_date = "2025-01-01"),
    as_of = as.Date("2026-01-01")
  )
  expect_equal(fail_row$status, "fail")
  expect_equal(freshness_exit_code(fail_row), 2L)
})

test_that("the threshold boundary is inclusive", {
  exactly_warn <- compute_freshness(
    fake_registry(warn = 100, fail = 200),
    entry(publisher_release_date = as.character(as.Date("2026-01-01") - 100)),
    as_of = as.Date("2026-01-01")
  )
  expect_equal(exactly_warn$status, "warn")
})

test_that("age falls back to retrieval date when the release date is unknown", {
  f <- compute_freshness(
    fake_registry(),
    entry(publisher_release_date = NA_character_, retrieved_at_utc = "2025-12-01T00:00:00Z"),
    as_of = as.Date("2026-01-01")
  )
  expect_equal(f$age_basis, "retrieved_at_utc")
  expect_equal(f$age_days, 31L)
})

test_that("a row with no usable date at all is pending, not ok", {
  f <- compute_freshness(
    fake_registry(),
    entry(publisher_release_date = NA_character_, retrieved_at_utc = NA_character_),
    as_of = as.Date("2026-01-01")
  )
  expect_equal(f$status, "pending")
})

test_that("a failed ingest stays failing however recent it is", {
  f <- compute_freshness(
    fake_registry(),
    entry(publisher_release_date = "2025-12-31", status = "failed"),
    as_of = as.Date("2026-01-01")
  )
  expect_equal(f$status, "fail")
  expect_equal(freshness_exit_code(f), 2L)
})

test_that("days_overdue is zero before the warn threshold and positive after", {
  fresh <- compute_freshness(
    fake_registry(warn = 100, fail = 200),
    entry(publisher_release_date = "2025-12-01"),
    as_of = as.Date("2026-01-01")
  )
  expect_equal(fresh$days_overdue, 0L)

  late <- compute_freshness(
    fake_registry(warn = 100, fail = 200),
    entry(publisher_release_date = "2025-08-01"),
    as_of = as.Date("2026-01-01")
  )
  expect_gt(late$days_overdue, 0L)
})

# ---- the shipped registry and manifest -----------------------------------

test_that("the committed manifest covers every registered source", {
  registry <- load_sources_registry(repo_root)
  manifest <- manifest_read(manifest_path(repo_root))
  skip_if(nrow(manifest) == 0, "manifest not seeded")

  registry_ids <- vapply(registry$sources, function(s) s$id, character(1))
  expect_setequal(manifest$source_id, registry_ids)
  expect_true(all(manifest$status %in% manifest_statuses()))
})

test_that("freshness runs against the shipped configuration", {
  registry <- load_sources_registry(repo_root)
  manifest <- manifest_read(manifest_path(repo_root))
  f <- compute_freshness(registry, manifest, as_of = as.Date("2026-09-16"))

  expect_equal(nrow(f), length(registry$sources))
  expect_true(all(f$status %in% c("ok", "warn", "fail", "pending")))
  expect_true(freshness_exit_code(f) %in% 0:2)

  md <- freshness_markdown(f, as_of = as.Date("2026-09-16"))
  expect_true(any(grepl("^# Data status", md)))
  expect_true(any(grepl("Do not edit by hand", md)))
})
