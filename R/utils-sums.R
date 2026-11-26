#' Sum the measured values, missing where none was measured
#'
#' A missing value marks a quantity that was never observed. A sum over
#' nothing measured is then missing. A plain `sum(na.rm = TRUE)` reports
#' zero there, a perfect score or a log-density of a probability of one.
#'
#' @param x Numeric vector, matrix or array
#' @param dims Integer number of leading dimensions to keep, as in
#'   [rowSums()]. `0` sums every value.
#' @return A number for `dims = 0`, otherwise an array over the first
#'   `dims` dimensions of `x`
#' @noRd
sum_measured <- function(x, dims = 0L) {
  checkmate::assert_numeric(x)
  checkmate::assert_int(dims, lower = 0L)
  if (dims == 0L) {
    if (all(is.na(x))) return(NA_real_)
    return(sum(x, na.rm = TRUE))
  }
  out <- rowSums(x, na.rm = TRUE, dims = dims)
  out[rowSums(!is.na(x), dims = dims) == 0L] <- NA_real_
  out
}


#' Average the measured values, missing where none was measured
#'
#' The mean counterpart of `sum_measured()`. A plain
#' `mean(na.rm = TRUE)` returns `NaN` when nothing was measured.
#'
#' @param x Numeric vector
#' @return A number, or `NA` when every value is missing
#' @noRd
mean_measured <- function(x) {
  checkmate::assert_numeric(x)
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

