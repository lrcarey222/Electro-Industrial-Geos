#' Weighted index across columns
#'
#' @param data Data frame.
#' @param weights Named numeric vector with weights.
#' @return Numeric vector of weighted scores.
#' @export
weighted_index <- function(data, weights) {
  if (length(weights) == 0) {
    rlang::abort("weights must be a named numeric vector")
  }
  missing_cols <- setdiff(names(weights), names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(glue::glue("Missing columns for weights: {paste(missing_cols, collapse = ', ')}"))
  }
  m <- as.matrix(data[, names(weights), drop = FALSE])
  num <- rowSums(t(t(m) * weights), na.rm = TRUE)
  den <- rowSums(t(t(!is.na(m)) * weights))
  num / den
}

#' Row mean index across columns
#'
#' @param data Data frame.
#' @return Numeric vector.
#' @export
rowmean_index <- function(data) {
  rowMeans(data, na.rm = TRUE)
}
