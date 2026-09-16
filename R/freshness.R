#' Compute freshness for every registered source
#'
#' Joins `config/sources.yml` to `data/_state/manifest.csv` and measures each
#' source's vintage against its own SLA.
#'
#' Three rules matter:
#'
#' * A source with no manifest row is `pending`, never `fresh`. An unknown
#'   vintage must not read as up to date.
#' * A source whose last ingest `failed` stays `fail` regardless of dates, so a
#'   broken connector cannot hide behind a recent timestamp.
#' * Age is measured from `publisher_release_date` where known, falling back to
#'   `retrieved_at_utc`. What matters is how old the *data* is, not when we
#'   last happened to fetch it.
#'
#' @param registry Result of `load_sources_registry()`.
#' @param manifest Result of `manifest_read()`.
#' @param as_of Date to measure against.
#' @return Tibble, one row per source.
#' @export
compute_freshness <- function(registry, manifest, as_of = Sys.Date()) {
  as_of <- as.Date(as_of)

  rows <- lapply(registry$sources, function(s) {
    entry <- manifest[manifest$source_id == s$id, , drop = FALSE]
    has_entry <- nrow(entry) > 0

    warn_after <- s$staleness$warn_after_days %||% NA_real_
    fail_after <- s$staleness$fail_after_days %||% NA_real_

    vintage_date <- NA
    age_basis <- NA_character_
    if (has_entry) {
      release <- suppressWarnings(as.Date(entry$publisher_release_date[1]))
      retrieved <- suppressWarnings(as.Date(substr(entry$retrieved_at_utc[1], 1, 10)))
      if (!is.na(release)) {
        vintage_date <- release
        age_basis <- "publisher_release_date"
      } else if (!is.na(retrieved)) {
        vintage_date <- retrieved
        age_basis <- "retrieved_at_utc"
      }
    }

    age_days <- if (inherits(vintage_date, "Date") && !is.na(vintage_date)) {
      as.integer(as_of - vintage_date)
    } else {
      NA_integer_
    }

    manifest_status <- if (has_entry) entry$status[1] else NA_character_

    status <- if (!has_entry || identical(manifest_status, "pending")) {
      "pending"
    } else if (identical(manifest_status, "failed")) {
      "fail"
    } else if (is.na(age_days)) {
      # A row exists but carries no usable date, so its age is unknowable.
      "pending"
    } else if (!is.na(fail_after) && age_days >= fail_after) {
      "fail"
    } else if (!is.na(warn_after) && age_days >= warn_after) {
      "warn"
    } else {
      "ok"
    }

    days_to_warn <- if (is.na(age_days) || is.na(warn_after)) NA_integer_ else as.integer(warn_after - age_days)
    days_overdue <- if (is.na(age_days) || is.na(warn_after)) NA_integer_ else max(0L, as.integer(age_days - warn_after))

    tibble::tibble(
      source_id = s$id,
      label = s$label %||% NA_character_,
      publisher = s$publisher %||% NA_character_,
      access_class = s$access_class %||% NA_character_,
      feeds = paste(s$feeds %||% character(0), collapse = ", "),
      n_indicators = length(s$feeds %||% character(0)),
      cadence = as.character(s$cadence %||% NA_character_),
      steward = as.character(s$steward %||% NA_character_),
      vintage_label = if (has_entry) entry$vintage_label[1] else NA_character_,
      vintage_date = if (inherits(vintage_date, "Date")) as.character(vintage_date) else NA_character_,
      age_basis = age_basis,
      age_days = age_days,
      warn_after_days = as.integer(warn_after),
      fail_after_days = as.integer(fail_after),
      days_to_warn = days_to_warn,
      days_overdue = days_overdue,
      n_geographies = if (has_entry) entry$n_geographies[1] else NA_integer_,
      manifest_status = manifest_status,
      status = status,
      can_commit_raw = isTRUE(s$can_commit_raw)
    )
  })

  out <- dplyr::bind_rows(rows)
  status_rank <- c(fail = 1L, pending = 2L, warn = 3L, ok = 4L)
  out[order(status_rank[out$status], -out$n_indicators, out$source_id), ]
}

#' Exit code for a freshness table
#'
#' 0 all fresh, 1 one or more warn, 2 one or more fail. `pending` counts as a
#' warning: a source whose vintage nobody has ever recorded is a gap in the
#' accountability chain, not a clean bill of health.
#'
#' @param freshness Result of `compute_freshness()`.
#' @return Integer exit code.
#' @export
freshness_exit_code <- function(freshness) {
  if (any(freshness$status == "fail")) {
    return(2L)
  }
  if (any(freshness$status %in% c("warn", "pending"))) {
    return(1L)
  }
  0L
}

#' Summarise a freshness table as a list suitable for JSON
#'
#' @param freshness Result of `compute_freshness()`.
#' @param as_of Date the check was run.
#' @return List.
#' @export
freshness_summary <- function(freshness, as_of = Sys.Date()) {
  counts <- table(factor(freshness$status, levels = c("ok", "warn", "fail", "pending")))
  list(
    generated_at_utc = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    as_of = as.character(as.Date(as_of)),
    exit_code = freshness_exit_code(freshness),
    n_sources = nrow(freshness),
    n_indicators_covered = sum(freshness$n_indicators),
    counts = as.list(as.integer(counts)) |> setNames(names(counts)),
    sources = freshness
  )
}

#' Render a freshness table as markdown
#'
#' @param freshness Result of `compute_freshness()`.
#' @param as_of Date the check was run.
#' @return Character vector of markdown lines.
#' @export
freshness_markdown <- function(freshness, as_of = Sys.Date()) {
  icon <- c(ok = "&#x1F7E2;", warn = "&#x1F7E1;", fail = "&#x1F534;", pending = "&#x26AA;")
  counts <- table(factor(freshness$status, levels = c("ok", "warn", "fail", "pending")))

  age_cell <- function(age_days, status) {
    ifelse(is.na(age_days), "unknown", paste0(age_days, "d"))
  }
  sla_cell <- function(row_warn, row_fail) paste0(row_warn, " / ", row_fail)

  lines <- c(
    "<!-- Generated by scripts/02_check_freshness.R. Do not edit by hand. -->",
    "",
    "# Data status",
    "",
    paste0("As of **", as.character(as.Date(as_of)), "**. ",
           "&#x1F7E2; fresh &middot; &#x1F7E1; past warn &middot; ",
           "&#x1F534; past fail &middot; &#x26AA; no vintage recorded"),
    "",
    paste0(
      "**", counts[["ok"]], " fresh**, ",
      counts[["warn"]], " warning, ",
      counts[["fail"]], " failing, ",
      counts[["pending"]], " never recorded &mdash; across ",
      nrow(freshness), " sources feeding ",
      sum(freshness$n_indicators), " indicators."
    ),
    "",
    "Age is measured from the publisher's release date where it is known, and from",
    "the retrieval date otherwise. A source with no manifest entry is never counted",
    "as fresh.",
    "",
    "| | Source | Vintage | Age | SLA (warn/fail) | Cadence | Steward | Indicators |",
    "|---|---|---|---|---|---|---|---|"
  )

  for (i in seq_len(nrow(freshness))) {
    r <- freshness[i, ]
    lines <- c(lines, paste0(
      "| ", icon[[r$status]],
      " | `", r$source_id, "`<br><sub>", r$label, "</sub>",
      " | ", ifelse(is.na(r$vintage_label), "&mdash;", r$vintage_label),
      " | ", age_cell(r$age_days, r$status),
      " | ", sla_cell(r$warn_after_days, r$fail_after_days),
      " | ", ifelse(is.na(r$cadence), "&mdash;", r$cadence),
      " | ", ifelse(is.na(r$steward) | r$steward == "TODO", "**unassigned**", r$steward),
      " | ", ifelse(r$n_indicators == 0, "&mdash; <sub>(support)</sub>", as.character(r$n_indicators)),
      " |"
    ))
  }

  unassigned <- sum(is.na(freshness$steward) | freshness$steward == "TODO")
  if (unassigned > 0) {
    lines <- c(
      lines, "",
      paste0(
        "> **", unassigned, " of ", nrow(freshness),
        " sources have no steward.** A stale source with no owner generates an issue",
        " nobody is assigned to. Populate `config/stewards.yml` from",
        " `config/stewards.example.yml`."
      )
    )
  }

  c(lines, "")
}
