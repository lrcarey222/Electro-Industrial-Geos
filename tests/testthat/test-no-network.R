library(testthat)

# Implementation comes from tests/testthat/setup.R.
#
# Guards docs/refactor_plan.md F-11: two network reads sat at the top level of
# scripts/07_process_data.R with no cache, no tryCatch and no regard for
# SKIP_DATA_DOWNLOADS, so they broke the brief's no-network CI rule and could
# abort the pipeline before a single indicator was built.
#
# Nothing here touches the network. The Census fixture is a trimmed real file;
# see tests/fixtures/census/README.md.

census_fixture <- function() fixture_path("census", "co-est2025-alldata.csv")

test_that("the vintage URL follows the pattern verified against Census", {
  expect_equal(
    census_county_pop_url(2025),
    paste0(
      "https://www2.census.gov/programs-surveys/popest/datasets/",
      "2020-2025/counties/totals/co-est2025-alldata.csv"
    )
  )
  # The decade start is fixed at 2020; only the vintage moves.
  expect_match(census_county_pop_url(2023), "2020-2023/counties/totals/co-est2023-alldata\\.csv$")
})

test_that("the registry endpoint template and the builder agree", {
  # sources.yml is the registry of record, so a builder that drifts from it
  # would make the registry a lie.
  registry <- load_sources_registry(repo_root)
  entry <- Filter(function(s) identical(s$id, "census_county_population"), registry$sources)[[1]]

  expect_match(entry$endpoint, "\\{year\\}", info = "endpoint should be a template, not a pinned vintage")
  substituted <- gsub("{year}", "2025", entry$endpoint, fixed = TRUE)
  expect_equal(census_county_pop_url(2025), substituted)
})

# --- Offline is the default CI condition -----------------------------------

test_that("offline with nothing cached returns NULL rather than reaching out", {
  cache <- withr::local_tempdir()
  expect_null(load_census_county_population(cache, offline = TRUE, from = 2025))
  # And it must not have created the directory as a side effect of trying.
  expect_length(list.files(cache), 0L)
})

test_that("offline reads a cached vintage, newest first", {
  cache <- withr::local_tempdir()
  file.copy(census_fixture(), file.path(cache, "co-est2025-alldata.csv"))
  file.copy(census_fixture(), file.path(cache, "co-est2024-alldata.csv"))

  # 2024's copy is the 2025 file, so its POPESTIMATE2024 column exists too --
  # what matters is that the newest year is preferred.
  got <- load_census_county_population(cache, offline = TRUE, from = 2026)
  expect_equal(got$year, "2025")

  # Walks back when the newest years are not published.
  expect_equal(load_census_county_population(cache, offline = TRUE, from = 2024)$year, "2024")
  # Gives up rather than reaching for an implausibly old vintage.
  expect_null(load_census_county_population(cache, offline = TRUE, from = 2030, max_back = 2L))
})

test_that("the population column is derived from the vintage, not hard-coded", {
  cache <- withr::local_tempdir()
  file.copy(census_fixture(), file.path(cache, "co-est2025-alldata.csv"))
  got <- load_census_county_population(cache, offline = TRUE, from = 2025)

  expect_named(got$data, c("FIPS", "population"))
  expect_type(got$data$population, "double")
  expect_true(all(!is.na(got$data$population)))
  # FIPS is the zero-padded state+county concatenation.
  expect_true(all(grepl("^[0-9]{5}$", got$data$FIPS)))

  # The old code read POPESTIMATE2023 from a co-est2023 file. Reading the 2025
  # file must pick POPESTIMATE2025, which differs -- otherwise the column and
  # the vintage have drifted apart again.
  raw <- readr::read_csv(census_fixture(), show_col_types = FALSE, progress = FALSE)
  expect_equal(got$data$population, as.numeric(raw$POPESTIMATE2025))
  expect_false(isTRUE(all.equal(
    as.numeric(raw$POPESTIMATE2025), as.numeric(raw$POPESTIMATE2023)
  )))
})

test_that("a vintage whose own estimate column is missing is skipped, not guessed", {
  cache <- withr::local_tempdir()
  raw <- readr::read_csv(census_fixture(), show_col_types = FALSE, progress = FALSE)
  # A file labelled 2025 that does not actually carry POPESTIMATE2025.
  readr::write_csv(
    raw[, setdiff(names(raw), "POPESTIMATE2025")],
    file.path(cache, "co-est2025-alldata.csv")
  )
  expect_null(load_census_county_population(cache, offline = TRUE, from = 2025, max_back = 0L))

  # With an older usable vintage present it falls back rather than failing.
  readr::write_csv(raw, file.path(cache, "co-est2024-alldata.csv"))
  expect_equal(
    load_census_county_population(cache, offline = TRUE, from = 2025, max_back = 1L)$year,
    "2024"
  )
})

# --- State boundaries ------------------------------------------------------

test_that("state boundaries come from cache when offline", {
  cache <- withr::local_tempdir()
  expect_null(load_state_boundaries(cache, offline = TRUE, from = 2025))

  fake <- structure(
    list(STATEFP = "06", STUSPS = "CA", STATE = "California"),
    class = c("tbl_df", "tbl", "data.frame"), row.names = 1L
  )
  saveRDS(fake, file.path(cache, "census_tiger_states_2024.rds"))

  got <- load_state_boundaries(cache, offline = TRUE, from = 2025)
  expect_equal(got$year, "2024")
  expect_equal(got$data$STUSPS, "CA")
})

test_that("a corrupt cached geometry is skipped rather than crashing the run", {
  cache <- withr::local_tempdir()
  writeLines("not an rds file", file.path(cache, "census_tiger_states_2025.rds"))
  expect_null(load_state_boundaries(cache, offline = TRUE, from = 2025, max_back = 0L))
})

# --- The whole point: a cold offline run must not abort --------------------

test_that("both loaders degrade to NULL together without erroring", {
  cache <- withr::local_tempdir()
  expect_silent({
    pop <- load_census_county_population(cache, offline = TRUE, from = 2025)
    geo <- load_state_boundaries(cache, offline = TRUE, from = 2025)
  })
  expect_null(pop)
  expect_null(geo)
})
