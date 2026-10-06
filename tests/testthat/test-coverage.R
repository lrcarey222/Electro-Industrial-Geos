library(testthat)

# Implementation comes from tests/testthat/setup.R.
#
# Guards docs/refactor_plan.md F-25: an indicator whose producer goes missing
# keeps whatever base_inputs supplied -- the three-state sample fixture -- and
# the pipeline still reports success.

sample_fixture <- function() {
  tibble::tibble(
    state = c("California", "Texas", "New York"),
    alpha = c(1, 2, 3),
    beta  = c(10, 20, 30),
    gamma = c(5, 6, 7)
  )
}

# A run where `alpha` is real (50 states) and `beta` is still the fixture.
run_inputs <- function() {
  tibble::tibble(
    state = c(state.name),
    alpha = seq_along(state.name) + 100,
    beta  = NA_real_,
    gamma = NA_real_
  ) %>%
    dplyr::mutate(
      beta = dplyr::if_else(.data$state == "California", 10,
             dplyr::if_else(.data$state == "Texas", 20,
             dplyr::if_else(.data$state == "New York", 30, NA_real_))),
      gamma = dplyr::if_else(.data$state == "California", 5,
              dplyr::if_else(.data$state == "Texas", 6,
              dplyr::if_else(.data$state == "New York", 7, NA_real_)))
    )
}

test_that("indicators are read out of the index definition", {
  definition <- list(categories = list(
    a = list(variables = c("beta", "alpha")),
    b = list(variables = c("gamma", "alpha"))
  ))
  expect_equal(definition_indicators(definition), c("alpha", "beta", "gamma"))
})

test_that("sample bleed is detected only when coverage AND values both match", {
  cov <- indicator_coverage(run_inputs(), sample_fixture(), c("alpha", "beta", "gamma"))

  expect_equal(cov$status[cov$indicator == "alpha"], "ok")
  expect_equal(cov$status[cov$indicator == "beta"], "sample_bleed")
  expect_equal(cov$status[cov$indicator == "gamma"], "sample_bleed")
  expect_equal(cov$n_values[cov$indicator == "alpha"], 50L)
  expect_equal(cov$n_values[cov$indicator == "beta"], 3L)
})

test_that("three real states that happen to differ are not called bleed", {
  inputs <- run_inputs()
  # Same three states, different values: a genuinely sparse real indicator.
  inputs$beta[inputs$state == "California"] <- 11
  cov <- indicator_coverage(inputs, sample_fixture(), "beta")
  expect_false(cov$status == "sample_bleed")
  expect_equal(cov$status, "sparse")
})

test_that("a full-coverage indicator matching the fixture on 3 states is not bleed", {
  # Value-matching alone must not be enough, or any real indicator that agrees
  # with the fixture on California/Texas/New York would be flagged.
  inputs <- run_inputs()
  inputs$beta <- seq_along(state.name) + 500
  inputs$beta[inputs$state == "California"] <- 10
  inputs$beta[inputs$state == "Texas"] <- 20
  inputs$beta[inputs$state == "New York"] <- 30
  cov <- indicator_coverage(inputs, sample_fixture(), "beta")
  expect_equal(cov$status, "ok")
  expect_equal(cov$n_values, 50L)
})

test_that("missing and empty indicators are distinguished", {
  inputs <- run_inputs()
  inputs$delta <- NA_real_
  cov <- indicator_coverage(inputs, sample_fixture(), c("delta", "epsilon"))
  expect_equal(cov$status[cov$indicator == "delta"], "empty")
  expect_equal(cov$status[cov$indicator == "epsilon"], "absent")
  expect_equal(cov$n_values[cov$indicator == "epsilon"], 0L)
})

test_that("a missing geography key is refused rather than guessed", {
  expect_error(
    indicator_coverage(tibble::tibble(x = 1), sample_fixture(), "alpha"),
    "must contain `state`"
  )
})

# --- The ratchet -----------------------------------------------------------

test_that("declared bleed passes, undeclared bleed is a regression", {
  cov <- indicator_coverage(run_inputs(), sample_fixture(), c("alpha", "beta", "gamma"))

  both <- coverage_regressions(cov, declared = c("beta", "gamma"))
  expect_equal(both$regressions, character(0))
  expect_equal(coverage_exit_code(both), 0L)

  partial <- coverage_regressions(cov, declared = "beta")
  expect_equal(partial$regressions, "gamma")
  expect_equal(coverage_exit_code(partial), 2L)

  none <- coverage_regressions(cov, declared = character(0))
  expect_setequal(none$regressions, c("beta", "gamma"))
  expect_equal(coverage_exit_code(none), 2L)
})

test_that("an indicator that recovers is reported so the list can shrink", {
  inputs <- run_inputs()
  inputs$beta <- seq_along(state.name) + 900
  cov <- indicator_coverage(inputs, sample_fixture(), c("alpha", "beta", "gamma"))
  res <- coverage_regressions(cov, declared = c("beta", "gamma"))

  expect_equal(res$recovered, "beta")
  expect_equal(res$regressions, character(0))
  # Recovery is good news, not a failure.
  expect_equal(coverage_exit_code(res), 0L)
})

test_that("an empty indicator fails even when declared", {
  inputs <- run_inputs()
  inputs$beta <- NA_real_
  cov <- indicator_coverage(inputs, sample_fixture(), "beta")
  res <- coverage_regressions(cov, declared = "beta")
  expect_equal(res$empty, "beta")
  expect_equal(coverage_exit_code(res), 2L)
})

test_that("sparse coverage warns but does not fail", {
  inputs <- run_inputs()
  inputs$beta <- NA_real_
  inputs$beta[1:4] <- c(1, 2, 3, 4)
  cov <- indicator_coverage(inputs, sample_fixture(), "beta", min_geographies = 10L)
  res <- coverage_regressions(cov, declared = character(0))
  expect_equal(res$sparse, "beta")
  expect_equal(coverage_exit_code(res), 1L)
})

# --- The declared lists must describe the real pipeline --------------------

test_that("config/validation.yml declares the indicators it claims to", {
  cfg <- yaml::read_yaml(file.path(repo_root, "config", "validation.yml"))$coverage
  definition <- yaml::read_yaml(file.path(repo_root, "config", "index_definition.yml"))
  known <- definition_indicators(definition)

  declared <- c(cfg$allowed_sample_bleed, cfg$allowed_when_inputs_unavailable)
  # A declared name that is not an indicator is a typo that would silently
  # weaken the ratchet, because it would never match anything.
  expect_true(all(declared %in% known), info = paste(setdiff(declared, known), collapse = ", "))

  # The two lists must not overlap: an indicator is either unproduceable or
  # merely unreachable, and conflating them hides which.
  expect_length(
    intersect(cfg$allowed_sample_bleed, cfg$allowed_when_inputs_unavailable), 0L
  )
  expect_gt(cfg$min_geographies, 3L)
})
