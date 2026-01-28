#' Min-max scale a vector
#'
#' @param x Numeric vector.
#' @return Scaled vector in [0,1].
#' @export
scale_minmax <- function(x) {
  x <- replace(x, !is.finite(x), NA_real_)
  rng <- range(x, na.rm = TRUE)
  denom <- diff(rng)
  if (!is.finite(denom) || denom == 0) {
    return(rep(0, length(x)))
  }
  (x - rng[1]) / denom
}

#' Reverse min-max scale a vector
#'
#' @param x Numeric vector.
#' @return Scaled vector in [0,1] where higher original values map to 0.
#' @export
scale_reverse_minmax <- function(x) {
  1 - scale_minmax(x)
}

#' Scale data frame columns by polarity
#'
#' @param df Data frame.
#' @param positive Columns where higher is better.
#' @param negative Columns where higher is worse.
#' @return Data frame with scaled columns.
#' @export
scale_dataframe_by_polarity <- function(df, positive, negative) {
  df %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(positive), scale_minmax),
      dplyr::across(dplyr::all_of(negative), scale_reverse_minmax)
    )
}
