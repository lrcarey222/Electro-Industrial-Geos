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
# This deliberately does not `library()` the package. `Package:` in DESCRIPTION
# is `Electro-Industrialindex`, which is not a legal R package name -- hyphens
# are not permitted -- so the package cannot be installed and cannot be attached.
# See docs/refactor_plan.md F-18.

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
