# Shared setup for the test suite.
#
# testthat runs `setup*.R` with the working directory set to tests/testthat/.
# That is precisely why the previous arrangement executed zero assertions: each
# test file called `source("R/utils/utils_helpers.R")` and friends, which are
# repo-root-relative, so all three files errored at load with
# "cannot open file 'R/utils/utils_helpers.R'" before reaching a single
# `test_that()`. Locate the repository root explicitly, then load the
# implementation once, here, for every test file.
#
# This deliberately does not `library()` the package, and still sources R/
# directly. The name was illegal until 2026-10-07 -- `Electro-Industrialindex`,
# with a hyphen -- so the package could not be installed or attached at all
# (F-18). It is now `electroindustrial`, which is legal.
#
# Sourcing R/ remains the right loader regardless, because the tests exercise
# functions the package does not export. NAMESPACE exports 38 names; the suite
# relies on many more, including every connector added since -- qcew_*, bnef_*,
# indicator_coverage(), pea_dominant_state(), file_sha256() -- none of which is
# exported. `library()` would load a package that does not contain most of what
# is under test.
#
# Bringing NAMESPACE back in line with R/ is real outstanding work and is not a
# rename. Until then this file is the loader, and `tests/testthat.R` -- the
# `R CMD check` entry point, which does call `library()` -- stays aspirational.

find_repo_root_for_tests <- function(start = getwd()) {
  dir <- normalizePath(start, winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(dir, "DESCRIPTION"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) {
      stop(
        "tests/testthat/setup.R: could not locate the repository root from ",
        start,
        call. = FALSE
      )
    }
    dir <- parent
  }
  dir
}

repo_root <- find_repo_root_for_tests()

r_files <- list.files(
  file.path(repo_root, "R"),
  pattern = "[.]R$",
  recursive = TRUE,
  full.names = TRUE
)
if (length(r_files) == 0) {
  stop("tests/testthat/setup.R: no R sources found under ", file.path(repo_root, "R"), call. = FALSE)
}
for (f in r_files) {
  source(f)
}

# Absolute path to a file under tests/fixtures/, so tests do not depend on the
# working directory either.
fixture_path <- function(...) file.path(repo_root, "tests", "fixtures", ...)

# Index weights are pinned here rather than read from config/weights.yml, so the
# expected-value fixture stays hermetic: changing published weights should
# require an explicit, reviewed fixture update rather than silently reshaping the
# baseline a regression test is measured against. test-indices.R asserts these
# still agree with config/weights.yml, so the two cannot drift apart unnoticed.
test_weights <- list(
  `Electro-Industrial` = list(
    deployment_index = 0.4,
    infra_index = 0.15,
    econ_index = 0.15,
    intent_index = 0.2,
    cluster_index = 0.2,
    ease_index = 0.2
  ),
  infrastructure = list(
    renewable_potential = 0.2,
    ev_stations_cap = 0.2,
    interconnection_queue = 0.2,
    electricity_price = 0.2,
    cnbc_rank = 0.2
  )
)

options(
  Electro_Industrial.paths = list(examples_dir = fixture_path()),
  Electro_Industrial.weights = test_weights
)
