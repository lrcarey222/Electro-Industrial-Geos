library(testthat)

# Implementation comes from tests/testthat/setup.R.
#
# Guards docs/refactor_plan.md F-08: the BNEF reader hard-coded both the export
# filename and the snapshot date, so a steward's newer drop was ignored and,
# once the publisher switched Date to an Excel serial, the filter would have
# matched zero rows without erroring.

test_that("the newest dated export wins", {
  d <- withr::local_tempdir()
  for (f in c(
    "2025-06-25 - Global Data Center Live IT Capacity Database.xlsx",
    "2025-08-08 - Global Data Center Live IT Capacity Database.xlsx",
    "2026-07-03 - Global Data Center Live IT Capacity Database (1.5.0).xlsx",
    "2026-01-08 - Global Data Center Live IT Capacity Database (1.2.0).xlsx"
  )) {
    file.create(file.path(d, f))
  }
  expect_match(basename(latest_bnef_export(d)), "^2026-07-03")
})

test_that("unrelated files in the directory are ignored", {
  d <- withr::local_tempdir()
  file.create(file.path(d, "2026-09-01 - Global Wind Turbine Nacelle Manufacturing Capacity.xlsx"))
  file.create(file.path(d, "2025-08-08 - Global Data Center Live IT Capacity Database.xlsx"))
  file.create(file.path(d, "Global Data Center Live IT Capacity Database _ Full Report.pdf"))
  expect_match(basename(latest_bnef_export(d)), "^2025-08-08")
  expect_match(basename(latest_bnef_export(d)), "[.]xlsx$")
})

test_that("a missing or empty directory returns NA rather than erroring", {
  expect_true(is.na(latest_bnef_export(file.path(tempdir(), "definitely-not-here"))))
  expect_true(is.na(latest_bnef_export(withr::local_tempdir())))
})

test_that("Date is normalised from both text and Excel serial", {
  # 46112 is 2026-03-31, the latest quarter in the 1.5.0 export.
  expect_equal(bnef_normalise_date(46112), as.Date("2026-03-31"))
  expect_equal(bnef_normalise_date("2025-03-31"), as.Date("2025-03-31"))
  expect_equal(bnef_normalise_date(as.Date("2025-03-31")), as.Date("2025-03-31"))

  # A mixed character column, which is what a part-converted sheet looks like.
  mixed <- bnef_normalise_date(c("2025-03-31", "46112", NA))
  expect_equal(mixed[1], as.Date("2025-03-31"))
  expect_equal(mixed[2], as.Date("2026-03-31"))
  expect_true(is.na(mixed[3]))
})

test_that("the pipeline column keeps old vintages on Committed and falls back", {
  old_shape <- data.frame(
    check.names = FALSE,
    `Committed.Capacity.(MW)` = c(0, 200),
    `Under.Construction.Capacity.(MW)` = c(0, 126)
  )
  new_shape <- data.frame(
    check.names = FALSE,
    `Under.Construction.Capacity.(MW)` = c(0, 1000)
  )

  # Where Committed survives, nothing about how it is read may change.
  expect_equal(bnef_pipeline_column(old_shape), "Committed.Capacity.(MW)")
  expect_equal(bnef_pipeline_column(new_shape), "Under.Construction.Capacity.(MW)")
  expect_true(is.na(bnef_pipeline_column(data.frame(x = 1))))
})

test_that("Other.Pipeline is never selected, even when it is the only mirror", {
  # BNEF 1.5.0 ships Other.Pipeline as the exact negation of Under.Construction
  # -- a mirror-bar chart helper. Selecting it inverts datacenter_index.
  v <- c(0, 4762.90, 3218.51)
  sheet <- data.frame(
    check.names = FALSE,
    `Under.Construction.Capacity.(MW)` = v,
    `Other.Pipeline.Capacity.(MW)` = -v
  )
  expect_equal(bnef_pipeline_column(sheet), "Under.Construction.Capacity.(MW)")

  # And if some future export leaves only a negative column, refuse it rather
  # than silently publishing an inverted index.
  only_mirror <- data.frame(check.names = FALSE, `Other.Pipeline.Capacity.(MW)` = -v)
  expect_true(is.na(bnef_pipeline_column(only_mirror)))
})

test_that("a candidate carrying negative capacity is rejected", {
  poisoned <- data.frame(check.names = FALSE, `Committed.Capacity.(MW)` = c(1, -1))
  expect_true(is.na(bnef_pipeline_column(poisoned)))

  # NA is not negative, and must not trip the guard.
  with_na <- data.frame(check.names = FALSE, `Committed.Capacity.(MW)` = c(1, NA))
  expect_equal(bnef_pipeline_column(with_na), "Committed.Capacity.(MW)")
})

test_that("against the staged exports, discovery and schema resolution agree", {
  dir <- file.path(repo_root, "data", "raw", "BNEF")
  skip_if_not(dir.exists(dir), "no BNEF directory")
  latest <- latest_bnef_export(dir)
  skip_if(is.na(latest), "no BNEF export staged")

  # Whatever is staged, the reader must find a usable pipeline column and a
  # Date it can parse -- those are the two things that broke.
  d <- suppressWarnings(openxlsx::read.xlsx(latest, sheet = "Data Centers", startRow = 8))
  expect_true("Date" %in% names(d))

  col <- bnef_pipeline_column(d)
  expect_false(is.na(col))
  # Whatever gets selected must be a capacity, not a mirror of one.
  expect_false(any(suppressWarnings(as.numeric(d[[col]])) < 0, na.rm = TRUE))

  dates <- bnef_normalise_date(d$Date)
  expect_s3_class(dates, "Date")
  expect_true(any(!is.na(dates)))
  # The hard-coded date must no longer be assumed present.
  expect_true(max(dates, na.rm = TRUE) >= as.Date("2025-03-31"))
})
