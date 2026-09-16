#' Stable issue title for a source
#'
#' The title is the deduplication key. It must not change as the source gets
#' staler, or the workflow will open a second issue for the same source every
#' time the age changes. Everything volatile belongs in the body.
#'
#' @param source A source entry from the registry.
#' @return Character title.
#' @export
issue_title <- function(source) {
  paste0("[data-refresh] ", source$id, " — ", source$label)
}

#' Labels for a source's refresh issue
#'
#' @param source A source entry from the registry.
#' @param row One row of `compute_freshness()` output.
#' @return Character vector of labels.
#' @export
issue_labels <- function(source, row) {
  labels <- "data-refresh"

  automated <- identical(source$access_class, "api") ||
    identical(source$access_class, "file_url")
  labels <- c(labels, if (automated) "automated" else "manual-upload")

  if (identical(row$status, "fail")) {
    labels <- c(labels, "blocking")
  }
  labels
}

#' Drop path convention for a manual source
#'
#' @param source A source entry from the registry.
#' @param vintage Vintage label to use in the filename.
#' @return Character path.
#' @export
manual_drop_path <- function(source, vintage = "<vintage>") {
  vintage <- as.character(vintage)
  if (length(vintage) == 0 || is.na(vintage) || !nzchar(vintage) || vintage == "NA") {
    vintage <- "<vintage>"
  }
  # Leave the literal placeholder intact; sanitise only a real vintage label.
  if (vintage != "<vintage>") {
    vintage <- gsub("[^A-Za-z0-9._-]+", "-", vintage)
  }
  paste0(
    "data/manual/", source$id, "/",
    source$id, "_", vintage, "_", format(Sys.Date(), "%Y%m%d"), ".csv"
  )
}

fmt_or_dash <- function(x, dash = "_not recorded_") {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(dash)
  }
  chr <- as.character(x)
  # "TODO" is a placeholder, not a value: render it as the caller's dash so an
  # unassigned steward reads as unassigned rather than as a steward named TODO.
  if (!nzchar(chr) || identical(chr, "TODO")) dash else chr
}

#' Render the column contract as markdown
#'
#' @param source A source entry from the registry.
#' @return Character vector of markdown lines.
#' @export
render_column_contract <- function(source) {
  contract <- source$column_contract
  if (is.null(contract) || length(contract) == 0) {
    return("_No column contract recorded for this source._")
  }
  out <- character(0)
  for (nm in names(contract)) {
    cols <- unlist(contract[[nm]], use.names = FALSE)
    out <- c(
      out,
      paste0("`", nm, "` must contain these columns:"),
      "",
      paste0("```", "\n", paste(cols, collapse = "\n"), "\n", "```"),
      ""
    )
  }
  out
}

#' Build the body of a refresh issue
#'
#' Written to be self-sufficient: the person who opens this at 8am on a Monday
#' should not need to open the repository, read the code, or ask anyone what to
#' do. Everything comes from `config/sources.yml` and the manifest.
#'
#' @param source A source entry from the registry.
#' @param row One row of `compute_freshness()` output.
#' @param as_of Date the check ran.
#' @return Single character string (markdown).
#' @export
issue_body <- function(source, row, as_of = Sys.Date()) {
  automated <- identical(source$access_class, "api") ||
    identical(source$access_class, "file_url")

  age <- if (is.na(row$age_days)) {
    "**never recorded** — no vintage has ever been logged for this source"
  } else {
    paste0("**", row$age_days, " days old**")
  }

  overdue <- if (is.na(row$days_overdue) || row$days_overdue == 0) {
    "not yet past the warning threshold"
  } else {
    paste0("**", row$days_overdue, " days past the warning threshold**")
  }

  lines <- c(
    paste0(
      "> Opened automatically by `.github/workflows/data-freshness.yml`. ",
      "One issue per source, reused — please do not close it manually; it closes ",
      "itself when a validated refresh lands."
    ),
    "",
    "## What is stale",
    "",
    paste0("| | |"),
    paste0("|---|---|"),
    paste0("| Source | `", source$id, "` |"),
    paste0("| Publisher | ", fmt_or_dash(source$publisher), " |"),
    paste0("| Current vintage | ", fmt_or_dash(row$vintage_label), " |"),
    paste0("| Vintage date | ", fmt_or_dash(row$vintage_date), " |"),
    paste0("| Age | ", age, " |"),
    paste0("| Overdue by | ", overdue, " |"),
    paste0("| SLA | warn after ", row$warn_after_days, "d, fail after ", row$fail_after_days, "d |"),
    paste0("| Expected cadence | ", fmt_or_dash(row$cadence), " |"),
    paste0("| Status | `", row$status, "` |"),
    paste0("| Access class | `", fmt_or_dash(source$access_class), "` |"),
    paste0(
      "| Indicators affected | ",
      if (row$n_indicators == 0) {
        "_none directly — this source is a denominator or geography other indicators depend on_"
      } else {
        paste0("**", row$n_indicators, "**: ", paste0("`", unlist(source$feeds), "`", collapse = ", "))
      },
      " |"
    ),
    ""
  )

  if (identical(row$status, "fail")) {
    lines <- c(
      lines,
      paste0(
        "> **Past the failure threshold.** Published outputs are quoting a number ",
        "older than this source's SLA allows. Either refresh it or change the SLA ",
        "in `config/sources.yml` with a reason."
      ),
      ""
    )
  }

  lines <- c(lines, "## How to refresh", "")

  if (automated && !is.null(source$endpoint)) {
    lines <- c(
      lines,
      paste0("This source is `", source$access_class, "` and should refresh itself from:"),
      "",
      paste0("    ", source$endpoint),
      ""
    )
    if (!is.null(source$auth_env)) {
      lines <- c(
        lines,
        paste0(
          "Requires the `", source$auth_env, "` secret. If this issue is open, either ",
          "the connector failed or no connector exists yet — check the most recent ",
          "`data-refresh` workflow run before doing anything by hand."
        ),
        ""
      )
    } else {
      lines <- c(
        lines,
        paste0(
          "No key required. If this issue is open, either the connector failed or no ",
          "connector exists for this source yet — check the most recent ",
          "`data-refresh` workflow run before doing anything by hand."
        ),
        ""
      )
    }
  }

  if (!is.null(source$manual_instructions)) {
    lines <- c(lines, "### Retrieval steps", "", trimws(source$manual_instructions), "")
  } else if (!automated) {
    lines <- c(
      lines,
      paste0(
        "⚠️ **No `manual_instructions` are recorded for this source in ",
        "`config/sources.yml`.** That is a gap in the registry, not in your knowledge. ",
        "Whoever owns this source should add them so the next person does not have to ask."
      ),
      ""
    )
  }

  lines <- c(
    lines,
    "### Where to put it",
    "",
    if (isTRUE(source$can_commit_raw)) {
      c(
        paste0("Commit the file to:"),
        "",
        paste0("    ", manual_drop_path(source)),
        "",
        "This source is marked `can_commit_raw: true`, so the payload belongs in the repository."
      )
    } else {
      c(
        paste0("**Do not commit this file.** `can_commit_raw` is `false` for this source",
               if (!is.null(source$license)) paste0(" (", source$license, ")") else "", "."),
        "",
        "Place it in the directory named by `EIG_MANUAL_DIR`, keeping the same filename convention:",
        "",
        paste0("    $EIG_MANUAL_DIR/", source$id, "/", basename(manual_drop_path(source))),
        "",
        "Only the manifest entry is committed. The payload never enters git."
      )
    },
    "",
    "### Column contract",
    "",
    render_column_contract(source),
    "### Then validate it",
    "",
    paste0("    Rscript scripts/06_validate_manual.R --source ", source$id),
    "",
    paste0(
      "That checks the filename convention, encoding, headers, types, geography ",
      "coverage, duplicates and ranges, and reports the row and column of anything ",
      "wrong. On success it registers the drop in `data/_state/manifest.csv`, ",
      "promotes it, and closes this issue automatically."
    ),
    "",
    paste0(
      "<sub>The validator arrives with the manual-upload lane (Phase 3). Until then, ",
      "record the refresh by hand in `data/_state/manifest.csv`.</sub>"
    ),
    ""
  )

  if (!is.null(source$notes) && nzchar(trimws(as.character(source$notes)))) {
    lines <- c(lines, "## Known issues with this source", "", trimws(as.character(source$notes)), "")
  }

  lines <- c(
    lines,
    "---",
    paste0(
      "<sub>Checked ", as.character(as.Date(as_of)),
      " &middot; steward `", fmt_or_dash(row$steward, "unassigned"),
      "` &middot; full board: [`docs/data_status.md`](../blob/main/docs/data_status.md)</sub>"
    ),
    "",
    paste0("<!-- eig-source:", source$id, " -->")
  )

  paste(lines, collapse = "\n")
}

#' Build a notification plan
#'
#' Pure: no network, no GitHub. Returns what *should* happen so the workflow can
#' reconcile it against the issues that already exist, and so the whole thing can
#' be tested offline.
#'
#' @param registry Result of `load_sources_registry()`.
#' @param freshness Result of `compute_freshness()`.
#' @param as_of Date the check ran.
#' @return List of planned notifications.
#' @export
notification_plan <- function(registry, freshness, as_of = Sys.Date()) {
  by_id <- setNames(registry$sources, vapply(registry$sources, function(s) s$id, character(1)))
  flagged <- freshness[freshness$status %in% c("warn", "fail", "pending"), , drop = FALSE]

  lapply(seq_len(nrow(flagged)), function(i) {
    row <- flagged[i, ]
    source <- by_id[[row$source_id]]
    steward <- as.character(row$steward)
    assignee <- if (is.na(steward) || steward %in% c("", "TODO")) NA_character_ else steward

    list(
      source_id = row$source_id,
      title = issue_title(source),
      labels = issue_labels(source, row),
      assignee = assignee,
      status = row$status,
      days_overdue = if (is.na(row$days_overdue)) 0L else as.integer(row$days_overdue),
      escalation = escalation_milestone(row$days_overdue),
      body = issue_body(source, row, as_of = as_of)
    )
  })
}

#' Escalation milestone for a given overdue count
#'
#' The brief calls for re-commenting at 14 and 30 days overdue rather than
#' spamming weekly. Returning the milestone rather than a yes/no lets the
#' workflow make the comment idempotent: it marks each comment with the
#' milestone it represents and skips one already present.
#'
#' @param days_overdue Days past the warn threshold.
#' @return 30, 14, or NA.
#' @export
escalation_milestone <- function(days_overdue) {
  if (is.na(days_overdue)) {
    return(NA_integer_)
  }
  if (days_overdue >= 30) {
    return(30L)
  }
  if (days_overdue >= 14) {
    return(14L)
  }
  NA_integer_
}
