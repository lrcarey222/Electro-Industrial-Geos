#' Null coalescing helper
#' @param x First value.
#' @param y Fallback value.
#' @return x if not NULL, otherwise y.
#' @keywords internal
`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}
