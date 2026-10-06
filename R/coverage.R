# Indicator coverage gate.
#
# The pipeline builds every indicator from `base_inputs` and then coalesces real
# data over the top. When a producer is missing -- an unreadable file, a dead
# URL, a licensed input a public clone does not have -- the indicator keeps
# whatever `base_inputs` supplied and the run still reports success. With
# `use_sample_data: true` in config/config.yml, and no data/inputs.csv in the
# repo, `base_inputs` is the three-state fixture in data/examples. So a missing
# producer silently publishes California, Texas and New York as if they were the
# whole country.
#
# That is not hypothetical: it is how eight of 33 indicators came to sit on
# three states' worth of sample values in the published vintage while the
# pipeline reported success (docs/refactor_plan.md F-05, F-25).
#
# A hard floor cannot be the gate, because sample data is currently the declared
# default -- every run would fail. What works from day one is a RATCHET: the
# indicators known to be on sample data are declared in config/validation.yml,
# and the gate fails when that set GROWS. An indicator that regresses onto
# sample data is then a build failure rather than a number nobody questions.

#' Indicators the index is defined over
#'
#' @param index_definition Parsed `config/index_definition.yml`.
#' @return Sorted unique indicator names.
#' @export
definition_indicators <- function(index_definition) {
  vars <- unlist(lapply(index_definition$categories, function(cat) cat$variables), use.names = FALSE)
  sort(unique(as.character(vars)))
}

#' Per-indicator coverage, and whether the values are still the sample fixture
#'
#' An indicator counts as `sample_bleed` when it has no more values than the
#' fixture has rows **and** every value it does have is the fixture's value for
#' that geography. Both conditions are required: coverage alone would flag a
#' genuinely sparse real indicator, and value-matching alone would flag a real
#' indicator that happens to agree with the fixture on three states.
#'
#' @param inputs The processed inputs table, one row per geography.
#' @param sample The sample fixture (`data/examples/sample_inputs.csv`).
#' @param indicators Indicator names to assess.
#' @param min_geographies Below this, a non-bleeding indicator is `sparse`.
#' @param key Geography key column.
#' @return A tibble of `indicator`, `n_values`, `n_sample_matches`,
#'   `sample_rows`, `status`.
#' @export
indicator_coverage <- function(inputs, sample, indicators,
                               min_geographies = 10L, key = "state") {
  if (!key %in% names(inputs)) {
    rlang::abort(glue::glue("indicator_coverage(): `inputs` must contain `{key}`."))
  }
  sample_rows <- if (is.null(sample)) 0L else nrow(sample)

  rows <- lapply(indicators, function(ind) {
    if (!ind %in% names(inputs)) {
      return(tibble::tibble(
        indicator = ind, n_values = 0L, n_sample_matches = NA_integer_,
        sample_rows = sample_rows, status = "absent"
      ))
    }
    values <- inputs[[ind]]
    n_values <- sum(!is.na(values))

    n_match <- NA_integer_
    if (!is.null(sample) && ind %in% names(sample) && n_values > 0L) {
      joined <- merge(
        data.frame(k = inputs[[key]], v = values, stringsAsFactors = FALSE),
        data.frame(k = sample[[key]], s = sample[[ind]], stringsAsFactors = FALSE),
        by = "k", all.x = TRUE
      )
      joined <- joined[!is.na(joined$v), , drop = FALSE]
      n_match <- sum(
        !is.na(joined$s) & abs(joined$v - joined$s) <= 1e-9,
        na.rm = TRUE
      )
    }

    bleeding <- !is.na(n_match) &&
      n_values > 0L &&
      n_values <= sample_rows &&
      n_match == n_values

    status <- if (n_values == 0L) {
      "empty"
    } else if (bleeding) {
      "sample_bleed"
    } else if (n_values < min_geographies) {
      "sparse"
    } else {
      "ok"
    }

    tibble::tibble(
      indicator = ind, n_values = as.integer(n_values),
      n_sample_matches = as.integer(n_match),
      sample_rows = as.integer(sample_rows), status = status
    )
  })

  dplyr::bind_rows(rows) %>% dplyr::arrange(.data$indicator)
}

#' Compare observed sample bleed against the declared allowance
#'
#' This is the ratchet. `declared` is the reviewed list in
#' `config/validation.yml`; anything bleeding that is not on it is a regression,
#' and anything on it that no longer bleeds is an invitation to shorten the list.
#'
#' @param coverage As returned by [indicator_coverage()].
#' @param declared Indicator names allowed to be on sample data.
#' @return A list of `regressions`, `recovered`, `empty`, `sparse`.
#' @export
coverage_regressions <- function(coverage, declared = character(0)) {
  declared <- as.character(declared %||% character(0))
  bleeding <- coverage$indicator[coverage$status == "sample_bleed"]
  list(
    regressions = sort(setdiff(bleeding, declared)),
    recovered = sort(setdiff(declared, bleeding)),
    empty = sort(coverage$indicator[coverage$status %in% c("empty", "absent")]),
    sparse = sort(coverage$indicator[coverage$status == "sparse"])
  )
}

#' Exit code for a coverage run
#'
#' `2` a declared-set regression or an empty indicator, `1` sparse coverage
#' only, `0` clean. Matches the freshness engine's convention so the two can be
#' read the same way.
#'
#' @param result As returned by [coverage_regressions()].
#' @return Integer exit code.
#' @export
coverage_exit_code <- function(result) {
  if (length(result$regressions) > 0 || length(result$empty) > 0) {
    return(2L)
  }
  if (length(result$sparse) > 0) {
    return(1L)
  }
  0L
}

#' One-line-per-indicator coverage summary
#'
#' @param coverage As returned by [indicator_coverage()].
#' @param declared Indicator names allowed to be on sample data.
#' @return Character vector of lines.
#' @export
coverage_summary_lines <- function(coverage, declared = character(0)) {
  declared <- as.character(declared %||% character(0))
  mark <- function(status, ind) {
    if (status == "sample_bleed" && ind %in% declared) "declared" else status
  }
  vapply(seq_len(nrow(coverage)), function(i) {
    r <- coverage[i, ]
    sprintf(
      "  [%-12s] %-32s %2d values",
      mark(r$status, r$indicator), r$indicator, r$n_values
    )
  }, character(1))
}
