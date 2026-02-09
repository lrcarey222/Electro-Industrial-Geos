library(testthat)
library(dplyr)

source("R/utils/utils_helpers.R")
source("R/utils/utils_index.R")
source("R/utils/incentives_sector.R")
source("R/visualization/state_profile_helpers.R")

test_that("rowmean_index returns NA for all-NA rows", {
  x <- tibble::tibble(a = c(NA, 1), b = c(NA, 3))
  out <- rowmean_index(x)
  expect_true(is.na(out[1]))
  expect_equal(out[2], 2)
})

test_that("compute_ranks keeps NA rank as NA", {
  df <- tibble::tibble(state = c("A", "B", "C"), score = c(5, NA, 1))
  ranked <- compute_ranks(df, "score")
  expect_equal(ranked$rank[ranked$state == "A"], 1)
  expect_true(is.na(ranked$rank[ranked$state == "B"]))
})

test_that("incentives by sector handles missing gjf", {
  out <- build_incentives_by_sector_year(NULL, NULL)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
  expect_true(all(c("state", "year", "sector", "subs_m", "incentives_gdp") %in% names(out)))
})
