library(testthat)

# Implementation comes from tests/testthat/setup.R.
#
# Guards docs/refactor_plan.md F-06. Both BEA readers matched a zip member by
# its exact filename with the vintage baked in, and the annual reader also named
# the column `x2024`. When BEA's next release changed either, `[1]` returned NA,
# the reader returned an empty tibble, and gdp_growth_index plus the
# incentives_gdp denominator went NA with no warning.
#
# Measured 2026-10-08: both hard-coded filenames were ALREADY stale against live
# BEA. Nothing here touches the network; the zips are built in a temp dir.

make_zip <- function(dir, members) {
  # members: named list of filename -> data frame (or character lines)
  paths <- character(0)
  for (nm in names(members)) {
    p <- file.path(dir, nm)
    if (is.data.frame(members[[nm]])) {
      utils::write.csv(members[[nm]], p, row.names = FALSE)
    } else {
      writeLines(members[[nm]], p)
    }
    paths <- c(paths, p)
  }
  zip_path <- file.path(dir, "bundle.zip")
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  suppressWarnings(utils::zip(zip_path, basename(paths), flags = "-q"))
  zip_path
}

annual_table <- function(latest = 2025) {
  years <- 1997:latest
  df <- data.frame(
    GeoFIPS = c("01000", "06000"),
    GeoName = c("Alabama", "California"),
    Region = c(5L, 8L),
    TableName = "SAGDP9",
    LineCode = 1L,
    IndustryClassification = "...",
    # BEA pads this with a trailing space.
    Description = c("All industry total ", "All industry total "),
    Unit = "Millions of current dollars",
    check.names = FALSE, stringsAsFactors = FALSE
  )
  for (y in years) df[[as.character(y)]] <- c(1000 + y, 5000 + y)
  df
}

test_that("the zip URL matches what the registry downloads", {
  expect_equal(bea_zip_url("SAGDP.zip"), "https://apps.bea.gov/regional/zip/SAGDP.zip")
  expect_equal(bea_zip_url("SQGDP.zip"), "https://apps.bea.gov/regional/zip/SQGDP.zip")
})

test_that("a member is found by prefix whatever year range follows it", {
  d <- withr::local_tempdir()
  # The two filenames the old code pinned, and the two live today.
  for (nm in c("SAGDP9__ALL_AREAS_1997_2024.csv", "SAGDP9__ALL_AREAS_1997_2025.csv")) {
    dd <- withr::local_tempdir()
    z <- make_zip(dd, setNames(list(annual_table()), nm))
    expect_equal(bea_zip_member(z, "SAGDP9__ALL_AREAS_"), nm)
  }
})

test_that("sibling tables with similar prefixes do not collide", {
  d <- withr::local_tempdir()
  z <- make_zip(d, list(
    "SAGDP1__ALL_AREAS_1997_2025.csv"  = annual_table(),
    "SAGDP11__ALL_AREAS_1998_2025.csv" = annual_table(),
    "SAGDP9__ALL_AREAS_1997_2025.csv"  = annual_table()
  ))
  # The double underscore is what separates SAGDP9 from SAGDP1 and SAGDP11.
  expect_equal(bea_zip_member(z, "SAGDP9__ALL_AREAS_"), "SAGDP9__ALL_AREAS_1997_2025.csv")
  expect_equal(bea_zip_member(z, "SAGDP1__ALL_AREAS_"), "SAGDP1__ALL_AREAS_1997_2025.csv")
  expect_equal(bea_zip_member(z, "SAGDP11__ALL_AREAS_"), "SAGDP11__ALL_AREAS_1998_2025.csv")
})

test_that("a missing member is an error, not an empty tibble", {
  d <- withr::local_tempdir()
  z <- make_zip(d, list("SOMETHING_ELSE.csv" = annual_table()))
  expect_error(bea_zip_member(z, "SAGDP9__ALL_AREAS_"), "No member starting with")
})

test_that("an ambiguous prefix is refused rather than taking the first", {
  d <- withr::local_tempdir()
  z <- make_zip(d, list(
    "SAGDP9__ALL_AREAS_1997_2024.csv" = annual_table(),
    "SAGDP9__ALL_AREAS_1997_2025.csv" = annual_table()
  ))
  expect_error(bea_zip_member(z, "SAGDP9__ALL_AREAS_"), "2 members start with")
})

test_that("BEA footnote codes become NA deliberately, without parse warnings", {
  # (D) withheld, (L) below threshold, (N)/(NA) not available, (NM) not
  # meaningful. The old code relied on parse_number() failing on these, which
  # was right by accident and emitted 75 warnings a run.
  expect_silent(got <- bea_parse_values(c("1,234", "(D)", "(L)", "(NA)", "(NM)", " 56 ", "")))
  expect_equal(got[[1]], 1234)
  expect_true(all(is.na(got[2:5])))
  expect_equal(got[[6]], 56)
  expect_true(is.na(got[[7]]))

  # Suppressed is unknown, not zero.
  expect_false(isTRUE(all.equal(bea_parse_values("(D)"), 0)))
})

# --- The year column must track the file ----------------------------------

test_that("the latest year column is discovered, not named", {
  expect_equal(bea_latest_year_column(c("geo_name", "x1997", "x2024", "x2025"))$column, "x2025")
  expect_equal(bea_latest_year_column(c("geo_name", "x1997", "x2024", "x2025"))$year, 2025L)
  # Order in the vector must not matter.
  expect_equal(bea_latest_year_column(c("x2025", "x1997"))$column, "x2025")
  expect_error(bea_latest_year_column(c("geo_name", "unit")), "No year columns")
})

test_that("annual GDP reads the latest year and records which year that is", {
  d <- withr::local_tempdir()
  make_zip(d, list("SAGDP9__ALL_AREAS_1997_2025.csv" = annual_table(latest = 2025)))
  raw <- withr::local_tempdir()
  fs::dir_create(fs::path(raw, "remote"))
  file.copy(file.path(d, "bundle.zip"), fs::path(raw, "remote", "SAGDP.zip"))

  got <- load_bea_annual_gdp(raw)
  expect_equal(nrow(got), 2L)
  expect_true(all(c("GeoName", "gdp", "gdp_year") %in% names(got)))
  expect_equal(unique(got$gdp_year), 2025L)
  # 2025 values, not 2024 -- proving the column followed the file.
  expect_equal(got$gdp[got$GeoName == "Alabama"], 1000 + 2025)
  expect_equal(got$gdp[got$GeoName == "California"], 5000 + 2025)
  # The trailing space BEA pads Description with is trimmed.
  expect_equal(unique(got$Description), "All industry total")
})

test_that("a renamed Description is an error, not a silently empty denominator", {
  d <- withr::local_tempdir()
  tbl <- annual_table()
  tbl$Description <- "Total for all industries"
  make_zip(d, list("SAGDP9__ALL_AREAS_1997_2025.csv" = tbl))
  raw <- withr::local_tempdir()
  fs::dir_create(fs::path(raw, "remote"))
  file.copy(file.path(d, "bundle.zip"), fs::path(raw, "remote", "SAGDP.zip"))

  expect_error(load_bea_annual_gdp(raw), "All industry total")
})

test_that("an unstaged zip is NULL, which the caller reports rather than aborting", {
  raw <- withr::local_tempdir()
  fs::dir_create(fs::path(raw, "remote"))
  expect_null(load_bea_annual_gdp(raw))
  expect_null(load_bea_quarterly_gdp_growth(raw, tibble::tibble(state = character(), abbr = character())))
})

# --- Against the real staged archives -------------------------------------

test_that("the committed zips resolve and give 50 states a GDP", {
  raw <- file.path(repo_root, "data", "raw")
  skip_if_not(file.exists(file.path(raw, "remote", "SAGDP.zip")), "SAGDP.zip not staged")

  got <- load_bea_annual_gdp(raw)
  expect_false(is.null(got))
  expect_gte(sum(got$GeoName %in% state.name), 50L)
  expect_true(all(got$gdp > 0, na.rm = TRUE))
  # A plausible recent vintage rather than a pinned one.
  expect_gte(unique(got$gdp_year)[[1]], 2024L)
})

test_that("the committed quarterly zip yields a growth index for the 50 states", {
  raw <- file.path(repo_root, "data", "raw")
  skip_if_not(file.exists(file.path(raw, "remote", "SQGDP.zip")), "SQGDP.zip not staged")
  states <- tibble::tibble(state = state.name, abbr = state.abb)

  got <- load_bea_quarterly_gdp_growth(raw, states)
  expect_false(is.null(got))
  expect_equal(nrow(got), 50L)
  expect_true(all(got$gdp_growth_index >= 0 & got$gdp_growth_index <= 1, na.rm = TRUE))
})
