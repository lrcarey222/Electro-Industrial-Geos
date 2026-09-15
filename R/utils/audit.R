#' Build long-form audit table
#'
#' @param raw_inputs Raw input data frame.
#' @param indices Named list of index data frames.
#' @param definition Index definition list.
#' @return Long-form audit tibble.
#' @export
build_audit_table <- function(raw_inputs, indices, definition = NULL) {
  # The output tables are not all shaped alike: cluster_pea and the PEA index are
  # keyed on `economic_area` (and the latter has no `abbr` at all), while
  # incentives_by_sector_year carries `year` and `sector`. Pivoting
  # `-c(state, abbr)` therefore tried to combine character keys with numeric
  # values and aborted with "Can't combine `economic_area` <character> and
  # `workforce_share` <double>". Pivot the numeric columns explicitly instead,
  # keeping whichever identifier columns a given table actually has.
  key_cols <- c("state", "abbr", "economic_area")

  pivot_long <- function(df, name, data_type) {
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    keys <- intersect(key_cols, names(df))
    value_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    if (length(keys) == 0 || length(value_cols) == 0) {
      return(NULL)
    }
    df %>%
      dplyr::select(dplyr::all_of(c(keys, value_cols))) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(value_cols),
        names_to = "variable",
        values_to = "value"
      ) %>%
      dplyr::mutate(category = name, data_type = data_type)
  }

  raw_long <- pivot_long(raw_inputs, "raw", "raw")

  index_long <- purrr::imap_dfr(indices, function(df, name) {
    pivot_long(df, name, "index")
  })

  combined <- dplyr::bind_rows(raw_long, index_long)

  if (!is.null(definition)) {
    meta <- purrr::imap_dfr(definition$categories %||% list(), function(defn, name) {
      tibble::tibble(
        category = name,
        variable = defn$variables %||% character(),
        polarity = defn$polarity %||% NA_character_,
        source = defn$source %||% NA_character_,
        explanation = defn$notes %||% NA_character_
      )
    })
    combined <- combined %>%
      dplyr::left_join(meta, by = c("category", "variable"))
  } else {
    combined <- combined %>%
      dplyr::mutate(polarity = NA_character_, source = NA_character_, explanation = NA_character_)
  }

  combined
}
