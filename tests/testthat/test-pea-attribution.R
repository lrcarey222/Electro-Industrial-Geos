library(testthat)

# Implementation comes from tests/testthat/setup.R.
#
# Guards docs/refactor_plan.md F-21. A PEA spanning a state border used to emit
# one row per state, so `Yuma, AZ` appeared as both California and Arizona, the
# PEA output carried 402 rows for 416 PEAs, and any diff keyed on economic_area
# matched duplicates against each other.
#
# Decision 2026-10-05: a PEA stays a geography in its own right, and state-level
# variables come from the state holding the largest share of it.

crosswalk <- function() {
  tibble::tibble(
    FCC_PEA_Number = c(1, 1, 1, 2, 2, 3),
    FIPS           = c("06001", "06003", "32001", "04001", "06005", "36001"),
    State          = c("CA", "CA", "NV", "AZ", "CA", "NY")
  )
}

populations <- function(ca1 = 100, ca2 = 50, nv = 400, az = 10, ca3 = 90, ny = 7) {
  tibble::tibble(
    FIPS = c("06001", "06003", "32001", "04001", "06005", "36001"),
    population = c(ca1, ca2, nv, az, ca3, ny)
  )
}

test_that("the dominant state is the one with the most people, not the most counties", {
  # PEA 1: California has two counties (150 people), Nevada has one (400).
  # Population must win, or a metro would lose to two rural counties.
  res <- pea_dominant_state(crosswalk(), populations())

  expect_equal(nrow(res), 3L)
  expect_equal(res$abbr[res$FCC_PEA_Number == 1], "NV")
  expect_equal(res$n_states[res$FCC_PEA_Number == 1], 2L)
  expect_equal(res$dominant_population[res$FCC_PEA_Number == 1], 400)
  expect_equal(res$dominant_share[res$FCC_PEA_Number == 1], 400 / 550)
})

test_that("the dominant state flips when the population flips", {
  res <- pea_dominant_state(crosswalk(), populations(ca1 = 900))
  expect_equal(res$abbr[res$FCC_PEA_Number == 1], "CA")
  expect_equal(res$dominant_population[res$FCC_PEA_Number == 1], 950)
})

test_that("a single-state PEA has share 1 and n_states 1", {
  res <- pea_dominant_state(crosswalk(), populations())
  pea3 <- res[res$FCC_PEA_Number == 3, ]
  expect_equal(pea3$abbr, "NY")
  expect_equal(pea3$n_states, 1L)
  expect_equal(pea3$dominant_share, 1)
})

test_that("exactly one row per PEA -- this is the whole point", {
  res <- pea_dominant_state(crosswalk(), populations())
  expect_equal(nrow(res), dplyr::n_distinct(crosswalk()$FCC_PEA_Number))
  expect_false(any(duplicated(res$FCC_PEA_Number)))
})

test_that("with no population at all it falls back to county count, not an error", {
  # An offline run has no Census data. The result must still be deterministic.
  res <- pea_dominant_state(crosswalk(), tibble::tibble(FIPS = character(), population = numeric()))
  expect_equal(nrow(res), 3L)
  # PEA 1: California has two counties to Nevada's one.
  expect_equal(res$abbr[res$FCC_PEA_Number == 1], "CA")
  expect_true(all(is.na(res$dominant_share)))
})

test_that("ties resolve deterministically rather than by row order", {
  tied <- tibble::tibble(
    FCC_PEA_Number = c(7, 7),
    FIPS = c("06001", "04001"),
    State = c("CA", "AZ")
  )
  pop <- tibble::tibble(FIPS = c("06001", "04001"), population = c(100, 100))

  first <- pea_dominant_state(tied, pop)
  reversed <- pea_dominant_state(tied[rev(seq_len(nrow(tied))), ], pop)
  expect_equal(first$abbr, reversed$abbr)
  # Equal population and equal county count, so alphabetical settles it.
  expect_equal(first$abbr, "AZ")
})

test_that("non-state territories are dropped rather than winning a PEA", {
  with_pr <- dplyr::bind_rows(
    crosswalk(),
    tibble::tibble(FCC_PEA_Number = 3, FIPS = "72001", State = "PR")
  )
  pop <- dplyr::bind_rows(
    populations(),
    tibble::tibble(FIPS = "72001", population = 1e6)
  )
  res <- pea_dominant_state(with_pr, pop)
  # Puerto Rico has far more people here, but is not one of the 50 states.
  expect_equal(res$abbr[res$FCC_PEA_Number == 3], "NY")
  expect_true(all(res$abbr %in% state.abb))
})

# --- State inheritance needs membership, not dominance ---------------------

pea_clusters <- function() {
  tibble::tibble(
    economic_area = c("New York, NY", "Providence, RI", "Albany, NY"),
    state         = c("New York", "Rhode Island", "New York"),
    cluster_index = c(0.9, 0.1, 0.4),
    cluster_top   = c(TRUE, FALSE, FALSE)
  )
}

# New Jersey and Connecticut sit inside the New York PEA but dominate nothing.
pea_membership <- function() {
  tibble::tibble(
    economic_area = c("New York, NY", "New York, NY", "New York, NY",
                      "Providence, RI", "Albany, NY"),
    state = c("New York", "New Jersey", "Connecticut", "Rhode Island", "New York")
  )
}

test_that("without membership, a state that dominates no PEA disappears", {
  # Documents the trap rather than endorsing it: this is what one row per PEA
  # does to the old dominance-only grouping.
  res <- build_state_cluster_from_pea(pea_clusters())
  expect_false("New Jersey" %in% res$state)
  expect_false("Connecticut" %in% res$state)
})

test_that("with membership, a state inherits the best PEA that overlaps it", {
  res <- build_state_cluster_from_pea(pea_clusters(), membership = pea_membership())

  expect_true(all(c("New York", "New Jersey", "Connecticut", "Rhode Island") %in% res$state))
  expect_equal(nrow(res), 4L)
  # All three New York PEA members inherit its 0.9, not Albany's 0.4.
  expect_equal(res$cluster_index[res$state == "New Jersey"], 0.9)
  expect_equal(res$cluster_index[res$state == "Connecticut"], 0.9)
  expect_equal(res$cluster_index[res$state == "New York"], 0.9)
  expect_equal(res$cluster_index[res$state == "Rhode Island"], 0.1)
  # One row per state, whatever the overlap count.
  expect_false(any(duplicated(res$state)))
})

test_that("membership requires the columns it needs", {
  expect_error(
    build_state_cluster_from_pea(pea_clusters(), membership = tibble::tibble(economic_area = "x")),
    "needs economic_area, state"
  )
  expect_error(
    build_state_cluster_from_pea(
      pea_clusters() %>% dplyr::select(-economic_area),
      membership = pea_membership()
    ),
    "needs `economic_area`"
  )
})

test_that("an empty membership falls back rather than emptying the index", {
  res <- build_state_cluster_from_pea(
    pea_clusters(),
    membership = tibble::tibble(economic_area = character(), state = character())
  )
  expect_gt(nrow(res), 0)
})

test_that("a malformed crosswalk is refused rather than guessed at", {
  expect_error(
    pea_dominant_state(tibble::tibble(FCC_PEA_Number = 1), populations()),
    "missing FIPS, State"
  )
})

test_that("against the real crosswalk, every PEA resolves to exactly one state", {
  path <- file.path(repo_root, "data", "raw", "FCC_PEA_website.xlsx")
  skip_if_not(file.exists(path), "FCC crosswalk not staged")
  cw <- suppressMessages(readxl::read_excel(path, 3))

  res <- pea_dominant_state(cw, tibble::tibble(FIPS = character(), population = numeric()))
  expect_false(any(duplicated(res$FCC_PEA_Number)))
  expect_true(all(res$abbr %in% state.abb))
  # The finding's premise: a substantial minority of PEAs cross a state line.
  expect_gt(sum(res$n_states > 1), 50)
  expect_lt(sum(res$n_states > 1), nrow(res))
})
