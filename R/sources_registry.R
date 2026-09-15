#' Read and validate the source registry
#'
#' Loads `config/sources.yml` and checks it against the index definition. The
#' central invariant is coverage: every indicator the index consumes must be
#' claimed by exactly one source, so a new indicator cannot enter the index with
#' no provenance and no owner.
#'
#' @param root Repo root.
#' @return List with `sources` (list) and `path`.
#' @export
load_sources_registry <- function(root = find_repo_root()) {
  path <- Sys.getenv("EIG_SOURCES", fs::path(root, "config", "sources.yml"))
  if (!file.exists(path)) {
    rlang::abort(glue::glue("Source registry not found: {path}"))
  }
  parsed <- yaml::read_yaml(path)
  if (is.null(parsed$sources) || length(parsed$sources) == 0) {
    rlang::abort(glue::glue("{path} contains no `sources:` entries."))
  }
  list(sources = parsed$sources, path = path)
}

#' Allowed access classes
#' @return Character vector.
#' @export
sources_access_classes <- function() {
  c("api", "file_url", "scrape", "manual", "licensed", "derived", "static", "unknown")
}

#' Fields every source entry must declare
#' @return Character vector.
#' @export
sources_required_fields <- function() {
  c(
    "id", "label", "feeds", "publisher", "access_class", "endpoint", "auth_env",
    "geography", "cadence", "typical_release_lag_days", "refresh_cron",
    "staleness", "steward", "can_commit_raw", "license", "notes"
  )
}

#' Fields additionally required for manual and licensed sources
#'
#' A steward receiving a refresh issue for one of these cannot act without
#' retrieval steps, a template and a column contract, so the registry requires
#' them up front rather than discovering the gap at 8am on a Monday.
#'
#' @return Character vector.
#' @export
sources_manual_fields <- function() {
  c("manual_instructions", "template", "column_contract")
}

#' Validate the source registry
#'
#' @param registry Result of `load_sources_registry()`.
#' @param index_definition Parsed `config/index_definition.yml`.
#' @return Character vector of problems; empty when valid.
#' @export
validate_sources_registry <- function(registry, index_definition) {
  problems <- character(0)
  sources <- registry$sources

  add <- function(...) problems <<- c(problems, paste0(...))

  # ---- Per-entry structure ----------------------------------------------
  ids <- vapply(
    sources,
    function(s) if (is.null(s$id)) NA_character_ else as.character(s$id),
    character(1)
  )

  if (anyNA(ids)) {
    add("Every source needs an `id`; ", sum(is.na(ids)), " entries have none.")
  }

  dup_ids <- unique(ids[!is.na(ids) & duplicated(ids)])
  if (length(dup_ids) > 0) {
    add("Duplicate source ids: ", paste(dup_ids, collapse = ", "), ".")
  }

  for (i in seq_along(sources)) {
    s <- sources[[i]]
    label <- if (is.na(ids[i])) paste0("entry ", i) else ids[i]

    missing <- setdiff(sources_required_fields(), names(s))
    if (length(missing) > 0) {
      add(label, ": missing required field(s) ", paste(missing, collapse = ", "), ".")
    }

    if (!is.null(s$access_class) && !s$access_class %in% sources_access_classes()) {
      add(
        label, ": access_class '", s$access_class, "' is not one of ",
        paste(sources_access_classes(), collapse = "/"), "."
      )
    }

    if (!is.null(s$staleness)) {
      warn_after <- s$staleness$warn_after_days
      fail_after <- s$staleness$fail_after_days
      if (is.null(warn_after) || is.null(fail_after)) {
        add(label, ": staleness needs both warn_after_days and fail_after_days.")
      } else if (!is.numeric(warn_after) || !is.numeric(fail_after)) {
        add(label, ": staleness thresholds must be numeric.")
      } else if (warn_after > fail_after) {
        add(
          label, ": warn_after_days (", warn_after,
          ") must not exceed fail_after_days (", fail_after, ")."
        )
      }
    }

    if (!is.null(s$can_commit_raw) && !is.logical(s$can_commit_raw)) {
      add(label, ": can_commit_raw must be true or false.")
    }

    # Manual and licensed sources need enough detail for a human to act.
    if (!is.null(s$access_class) && s$access_class %in% c("manual", "licensed")) {
      missing_manual <- setdiff(sources_manual_fields(), names(s))
      if (length(missing_manual) > 0) {
        add(
          label, ": access_class '", s$access_class, "' also requires ",
          paste(missing_manual, collapse = ", "), "."
        )
      }
    }

    # A secret must never be stored here -- only the name of an env var.
    if (!is.null(s$auth_env) && !is.na(s$auth_env)) {
      if (!grepl("^[A-Z][A-Z0-9_]*$", as.character(s$auth_env))) {
        add(
          label, ": auth_env must be an environment variable NAME ",
          "(upper snake case), never a value. Got '", s$auth_env, "'."
        )
      }
    }
  }

  # ---- Coverage: the check this registry exists for ----------------------
  declared <- unlist(lapply(index_definition$categories %||% list(), function(d) {
    d$variables %||% character(0)
  }), use.names = FALSE)
  declared <- unique(declared)

  claims <- unlist(lapply(sources, function(s) s$feeds %||% character(0)), use.names = FALSE)

  orphans <- setdiff(declared, claims)
  if (length(orphans) > 0) {
    add(
      "Orphaned indicator(s) -- claimed by no source in config/sources.yml, so ",
      "they have no provenance, no owner and no staleness SLA: ",
      paste(sort(orphans), collapse = ", "), "."
    )
  }

  claimed_twice <- unique(claims[duplicated(claims)])
  if (length(claimed_twice) > 0) {
    add(
      "Indicator(s) claimed by more than one source, so ownership is ambiguous: ",
      paste(sort(claimed_twice), collapse = ", "), "."
    )
  }

  unknown_claims <- setdiff(claims, declared)
  if (length(unknown_claims) > 0) {
    add(
      "Source(s) claim to feed indicator(s) that config/index_definition.yml ",
      "does not define: ", paste(sort(unknown_claims), collapse = ", "), "."
    )
  }

  problems
}
