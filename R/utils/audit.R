#' Build long-form audit table
#'
#' @param raw_inputs Raw input data frame.
#' @param indices Named list of index data frames.
#' @param definition Index definition list.
#' @return Long-form audit tibble.
#' @export
build_audit_table <- function(raw_inputs, indices, definition = NULL) {
  raw_long <- raw_inputs %>%
    tidyr::pivot_longer(
      cols = -c(state, abbr),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(category = "raw", data_type = "raw")

  index_long <- purrr::imap_dfr(indices, function(df, name) {
    df %>%
      tidyr::pivot_longer(
        cols = -c(state, abbr),
        names_to = "variable",
        values_to = "value"
      ) %>%
      dplyr::mutate(category = name, data_type = "index")
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
