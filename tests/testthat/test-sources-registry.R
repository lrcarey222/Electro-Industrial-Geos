library(testthat)

# Implementation and repo_root come from tests/testthat/setup.R.

index_definition <- yaml::read_yaml(file.path(repo_root, "config", "index_definition.yml"))

declared_indicators <- unique(unlist(
  lapply(index_definition$categories, function(d) d$variables),
  use.names = FALSE
))

test_that("the shipped registry is valid", {
  registry <- load_sources_registry(repo_root)
  expect_gt(length(registry$sources), 0)
  expect_equal(validate_sources_registry(registry, index_definition), character(0))
})

test_that("every indicator is claimed by exactly one source", {
  registry <- load_sources_registry(repo_root)
  claims <- unlist(lapply(registry$sources, function(s) s$feeds), use.names = FALSE)

  expect_setequal(intersect(claims, declared_indicators), declared_indicators)
  expect_equal(anyDuplicated(claims), 0L)
  # No source may claim an indicator the index does not define.
  expect_equal(setdiff(claims, declared_indicators), character(0))
})

test_that("an orphaned indicator fails validation", {
  registry <- load_sources_registry(repo_root)
  widened <- index_definition
  widened$categories$policy_intent$variables <- c(
    widened$categories$policy_intent$variables,
    "indicator_with_no_source"
  )

  problems <- validate_sources_registry(registry, widened)
  expect_true(any(grepl("Orphaned indicator", problems)))
  expect_true(any(grepl("indicator_with_no_source", problems)))
})

test_that("a double-claimed indicator fails validation", {
  registry <- load_sources_registry(repo_root)
  # Make a second source also claim an indicator that is already owned.
  registry$sources[[2]]$feeds <- c(registry$sources[[2]]$feeds, registry$sources[[1]]$feeds[[1]])

  problems <- validate_sources_registry(registry, index_definition)
  expect_true(any(grepl("more than one source", problems)))
})

test_that("manual and licensed sources must carry retrieval detail", {
  registry <- load_sources_registry(repo_root)
  for (s in registry$sources) {
    if (identical(s$access_class, "manual") || identical(s$access_class, "licensed")) {
      for (field in sources_manual_fields()) {
        expect_true(
          field %in% names(s),
          info = paste0(s$id, " (", s$access_class, ") is missing ", field)
        )
      }
    }
  }
})

test_that("stripping retrieval detail from a licensed source fails validation", {
  registry <- load_sources_registry(repo_root)
  idx <- which(vapply(
    registry$sources,
    function(s) identical(s$access_class, "licensed"),
    logical(1)
  ))
  skip_if(length(idx) == 0, "no licensed sources in the registry")

  registry$sources[[idx[1]]]$manual_instructions <- NULL
  problems <- validate_sources_registry(registry, index_definition)
  expect_true(any(grepl("manual_instructions", problems)))
})

test_that("warn threshold may not exceed fail threshold", {
  registry <- load_sources_registry(repo_root)
  registry$sources[[1]]$staleness$warn_after_days <- 9999

  problems <- validate_sources_registry(registry, index_definition)
  expect_true(any(grepl("must not exceed", problems)))
})

test_that("auth_env holds a variable name, never a value", {
  registry <- load_sources_registry(repo_root)

  # Nothing in the shipped registry should look like a literal secret.
  for (s in registry$sources) {
    if (!is.null(s$auth_env)) {
      expect_match(as.character(s$auth_env), "^[A-Z][A-Z0-9_]*$", info = s$id)
    }
  }

  registry$sources[[1]]$auth_env <- "sk-live-abc123"
  problems <- validate_sources_registry(registry, index_definition)
  expect_true(any(grepl("environment variable NAME", problems)))
})

test_that("every access_class is one of the documented values", {
  registry <- load_sources_registry(repo_root)
  classes <- vapply(registry$sources, function(s) s$access_class, character(1))
  expect_true(all(classes %in% sources_access_classes()))
})

test_that("source ids are unique", {
  registry <- load_sources_registry(repo_root)
  ids <- vapply(registry$sources, function(s) s$id, character(1))
  expect_equal(anyDuplicated(ids), 0L)
})
