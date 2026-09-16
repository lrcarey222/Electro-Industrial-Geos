library(testthat)

# Implementation and repo_root come from tests/testthat/setup.R.

src <- function(...) {
  utils::modifyList(
    list(
      id = "src_a", label = "Source A", feeds = c("ind_one", "ind_two"),
      publisher = "Publisher", access_class = "file_url",
      endpoint = "https://example.invalid/data.csv", auth_env = NULL,
      geography = "state", cadence = "annual", staleness = list(warn_after_days = 100, fail_after_days = 200),
      steward = "steward-a", can_commit_raw = TRUE, license = "Public domain", notes = ""
    ),
    list(...)
  )
}

row <- function(...) {
  utils::modifyList(
    list(
      source_id = "src_a", label = "Source A", publisher = "Publisher",
      access_class = "file_url", feeds = "ind_one, ind_two", n_indicators = 2L,
      cadence = "annual", steward = "steward-a", vintage_label = "v1",
      vintage_date = "2025-01-01", age_basis = "publisher_release_date",
      age_days = 150L, warn_after_days = 100L, fail_after_days = 200L,
      days_to_warn = -50L, days_overdue = 50L, n_geographies = 50L,
      manifest_status = "ok", status = "warn", can_commit_raw = TRUE
    ),
    list(...)
  ) |> tibble::as_tibble()
}

test_that("the title is stable as a source gets staler", {
  s <- src()
  a <- issue_title(s)
  b <- issue_title(s)
  expect_identical(a, b)
  expect_match(a, "^\\[data-refresh\\] src_a")
  # Nothing volatile may leak into the dedup key.
  expect_false(grepl("[0-9]+d|days|warn|fail", a))
})

test_that("labels reflect access class and severity", {
  expect_setequal(issue_labels(src(access_class = "api"), row()), c("data-refresh", "automated"))
  expect_setequal(issue_labels(src(access_class = "file_url"), row()), c("data-refresh", "automated"))
  expect_setequal(
    issue_labels(src(access_class = "licensed"), row()),
    c("data-refresh", "manual-upload")
  )
  expect_true("blocking" %in% issue_labels(src(), row(status = "fail")))
  expect_false("blocking" %in% issue_labels(src(), row(status = "warn")))
})

test_that("escalation fires only at 14 and 30 days", {
  expect_true(is.na(escalation_milestone(NA)))
  expect_true(is.na(escalation_milestone(0)))
  expect_true(is.na(escalation_milestone(13)))
  expect_equal(escalation_milestone(14), 14L)
  expect_equal(escalation_milestone(29), 14L)
  expect_equal(escalation_milestone(30), 30L)
  expect_equal(escalation_milestone(400), 30L)
})

test_that("the drop path follows the documented convention", {
  p <- manual_drop_path(src(), vintage = "2026-Q1")
  expect_match(p, "^data/manual/src_a/src_a_2026-Q1_[0-9]{8}\\.csv$")
  # The placeholder must survive intact when no vintage is known.
  expect_match(manual_drop_path(src()), "src_a_<vintage>_", fixed = FALSE)
  expect_match(manual_drop_path(src(), vintage = NA), "<vintage>", fixed = TRUE)
  # A messy real label gets sanitised, not passed through.
  expect_match(manual_drop_path(src(), vintage = "2025 Q2/final"), "2025-Q2-final", fixed = TRUE)
})

test_that("the body carries everything a steward needs", {
  body <- issue_body(src(access_class = "licensed", can_commit_raw = FALSE,
                         manual_instructions = "Log in and export the thing.",
                         column_contract = list(tbl = c("state", "value"))),
                     row(status = "fail"))

  expect_match(body, "Log in and export the thing", fixed = TRUE)
  expect_match(body, "Do not commit this file", fixed = TRUE)
  expect_match(body, "EIG_MANUAL_DIR", fixed = TRUE)
  expect_match(body, "scripts/06_validate_manual.R --source src_a", fixed = TRUE)
  expect_match(body, "Past the failure threshold", fixed = TRUE)
  expect_match(body, "ind_one", fixed = TRUE)
  expect_match(body, "<!-- eig-source:src_a -->", fixed = TRUE)
  # The column contract must be rendered, not just referenced.
  expect_match(body, "must contain these columns", fixed = TRUE)
})

test_that("a committable source is told to commit, not to use EIG_MANUAL_DIR", {
  body <- issue_body(src(can_commit_raw = TRUE), row())
  expect_match(body, "data/manual/src_a/", fixed = TRUE)
  expect_false(grepl("Do not commit this file", body, fixed = TRUE))
})

test_that("an unassigned steward reads as unassigned, not as a steward named TODO", {
  body <- issue_body(src(steward = "TODO"), row(steward = "TODO"))
  expect_match(body, "steward `unassigned`", fixed = TRUE)
  expect_false(grepl("steward `TODO`", body, fixed = TRUE))
})

test_that("a manual source with no instructions says so rather than staying silent", {
  body <- issue_body(src(access_class = "manual", manual_instructions = NULL), row())
  expect_match(body, "No `manual_instructions` are recorded", fixed = TRUE)
  expect_match(body, "gap in the registry", fixed = TRUE)
})

test_that("a never-recorded vintage is described as such", {
  body <- issue_body(src(), row(status = "pending", age_days = NA_integer_,
                                days_overdue = NA_integer_, vintage_label = NA_character_,
                                vintage_date = NA_character_))
  expect_match(body, "never recorded", fixed = TRUE)
  expect_match(body, "_not recorded_", fixed = TRUE)
})

test_that("a support-only source says so instead of claiming zero indicators", {
  body <- issue_body(src(feeds = character(0)), row(n_indicators = 0L))
  expect_match(body, "denominator or geography", fixed = TRUE)
})

# ---- plan ----------------------------------------------------------------

test_that("the plan covers exactly the sources outside their SLA", {
  registry <- load_sources_registry(repo_root)
  manifest <- manifest_read(manifest_path(repo_root))
  freshness <- compute_freshness(registry, manifest, as_of = as.Date("2026-09-16"))
  plan <- notification_plan(registry, freshness, as_of = as.Date("2026-09-16"))

  flagged <- freshness$source_id[freshness$status %in% c("warn", "fail", "pending")]
  expect_setequal(vapply(plan, function(p) p$source_id, character(1)), flagged)

  # Titles must be unique, or the workflow would collapse two sources onto one issue.
  titles <- vapply(plan, function(p) p$title, character(1))
  expect_equal(anyDuplicated(titles), 0L)

  for (p in plan) {
    expect_true("data-refresh" %in% p$labels)
    expect_true(nzchar(p$body))
  }
})

test_that("a fresh source generates no notification", {
  # Synthetic rather than derived from the shipped manifest: every real source
  # is currently stale, so this is the only way to exercise the empty-plan path.
  registry <- list(sources = list(src()), path = "<memory>")
  manifest <- manifest_upsert(manifest_empty(), list(
    source_id = "src_a", publisher_release_date = "2026-01-01",
    vintage_label = "v2", status = "ok"
  ))
  freshness <- compute_freshness(registry, manifest, as_of = as.Date("2026-02-01"))

  expect_equal(freshness$status, "ok")
  expect_equal(freshness_exit_code(freshness), 0L)
  expect_length(notification_plan(registry, freshness, as_of = as.Date("2026-02-01")), 0)
})

test_that("recovery is detectable: the same source drops out of the plan once refreshed", {
  registry <- list(sources = list(src()), path = "<memory>")

  stale <- manifest_upsert(manifest_empty(), list(
    source_id = "src_a", publisher_release_date = "2025-01-01",
    vintage_label = "v1", status = "ok"
  ))
  refreshed <- manifest_upsert(stale, list(
    source_id = "src_a", publisher_release_date = "2026-01-15",
    vintage_label = "v2", status = "ok"
  ))

  as_of <- as.Date("2026-02-01")
  plan_before <- notification_plan(registry, compute_freshness(registry, stale, as_of), as_of)
  plan_after <- notification_plan(registry, compute_freshness(registry, refreshed, as_of), as_of)

  expect_length(plan_before, 1)
  expect_length(plan_after, 0)
  # One row per source, so refreshing replaces rather than accumulating.
  expect_equal(nrow(refreshed), 1L)
})
