library(testthat)
library(dplyr)

source("R/utils/utils_helpers.R")
source("R/utils/utils_scale.R")
source("R/utils/utils_index.R")
source("R/utils/ingest_sample.R")
source("R/categories/build_policy_intent.R")

options(Electro_Industrial.paths = list(examples_dir = "tests/fixtures"))


test_that("scale_minmax handles NA and Inf", {
  x <- c(1, 2, NA, Inf)
  scaled <- scale_minmax(x)
  expect_equal(length(scaled), 4)
  expect_true(all(is.finite(scaled[1:2])))
  expect_true(is.na(scaled[3]))
})

test_that("scale_reverse_minmax reverses direction", {
  x <- c(1, 2, 3)
  expect_equal(scale_reverse_minmax(x), c(1, 0.5, 0))
})

test_that("weighted_index handles missing weights", {
  df <- data.frame(a = c(1, 2), b = c(2, 4))
  weights <- c(a = 0.5, b = 0.5)
  expect_equal(weighted_index(df, weights), c(1.5, 3))
})

test_that("policy intent index computes", {
  inputs <- load_sample_inputs()
  out <- build_policy_intent_index(inputs)
  expect_true("intent_index" %in% names(out))
  expect_equal(nrow(out), nrow(inputs))
})
