# Log-scale arithmetic shared by the density, distribution, mean and
# sampling kernels. Every kernel that sums probabilities held as logs
# calls these, which keeps one treatment of overflow and of the
# all-`-Inf` case.


#' `log(1 + exp(x))`, kept finite where `exp(x)` overflows
#'
#' @param x Numeric vector, matrix or array
#' @return `x` transformed, with its dimensions
#' @noRd
log1p_exp <- function(x) {
  out <- log1p(exp(x))
  out[is.infinite(out)] <- x[is.infinite(out)]
  out
}


#' Numerically stable `log(1 - exp(-a))` for `a >= 0`
#'
#' The branch at `log(2)` switches between `log(-expm1(-a))`, stable
#' near `a = 0`, and `log1p(-exp(-a))`, stable for large `a`.
#'
#' @param a Non-negative numeric vector or matrix.
#' @return `log(1 - exp(-a))`, with the dimensions of `a`.
#' @noRd
log1mexp <- function(a) {
  out <- a
  small <- a <= log(2)
  out[small] <- log(-expm1(-a[small]))
  out[!small] <- log1p(-exp(-a[!small]))
  out
}


#' Elementwise `log(exp(a) + exp(b))`
#'
#' @param a,b Numeric vectors of log values, recycled together.
#' @return The log of the elementwise sum. Two `-Inf` terms give
#'   `-Inf`.
#' @noRd
log_add_exp <- function(a, b) {
  m <- pmax(a, b)
  out <- m + log1p(exp(-abs(a - b)))
  inf <- !is.na(m) & is.infinite(m)
  out[inf] <- m[inf]
  out
}


#' Elementwise `log(exp(a) - exp(b))` for `a >= b`
#'
#' @param a,b Numeric vectors of log values.
#' @return The log of the elementwise difference.
#' @noRd
log_diff_exp <- function(a, b) {
  a + log1mexp(a - b)
}


#' `log(sum(exp(x)))` over one vector
#'
#' @param x Numeric vector of log values.
#' @return A scalar. An empty or all-`-Inf` vector gives `-Inf`.
#' @noRd
log_sum_exp <- function(x) {
  if (length(x) == 0L) return(-Inf)
  m <- max(x)
  if (is.infinite(m)) return(m)
  m + log(sum(exp(x - m)))
}


#' `log(mean(exp(x)))` over one vector
#'
#' @param x Numeric vector of log values.
#' @return A scalar.
#' @noRd
log_mean_exp <- function(x) {
  log_sum_exp(x) - log(length(x))
}


#' Normalise one vector of log weights to probabilities
#'
#' The transform is invariant to a shift shared by every component.
#' Subtracting the maximum before exponentiating leaves the result
#' unchanged and keeps `exp()` finite for weights of any magnitude.
#'
#' @param x Numeric vector of unnormalised log weights.
#' @return Numeric vector of probabilities summing to one.
#' @noRd
softmax <- function(x) {
  checkmate::assert_numeric(x, min.len = 1L, any.missing = FALSE)
  e <- exp(x - max(x))
  e / sum(e)
}


#' Maximum of each row of a numeric matrix
#'
#' @param m Numeric matrix.
#' @return Numeric vector, one entry per row.
#' @noRd
row_max <- function(m) {
  m[cbind(seq_len(nrow(m)), max.col(m, ties.method = "first"))]
}


#' `log(sum(exp(m[i, ])))` for each row of a matrix
#'
#' @param m Numeric matrix of log values.
#' @return Numeric vector, one entry per row. An all-`-Inf` row gives
#'   `-Inf`.
#' @noRd
row_log_sum_exp <- function(m) {
  mx <- row_max(m)
  out <- mx + log(rowSums(exp(m - mx)))
  inf <- !is.na(mx) & is.infinite(mx)
  out[inf] <- mx[inf]
  out
}


#' Normalise each row of log weights to probabilities
#'
#' @param lw Numeric matrix of unnormalised log weights.
#' @return Matrix of the dimensions of `lw` whose rows sum to one.
#' @noRd
row_softmax <- function(lw) {
  w <- exp(lw - row_max(lw))
  w / rowSums(w)
}


#' Draw one column index per row, with probability proportional to
#' `exp(lw[i, ])`
#'
#' Inverse-CDF sampling on each row's running sum of weights. The
#' running sum is built one column at a time over all rows at once,
#' then divided by its final column. That column is then exactly one,
#' which keeps every index inside the matrix. One uniform is drawn
#' per row.
#'
#' @param lw Numeric matrix of unnormalised log weights.
#' @return Integer vector of column indices, one per row.
#' @noRd
sample_row_index <- function(lw) {
  n_k <- ncol(lw)
  cdf <- exp(lw - row_max(lw))
  if (n_k > 1L) {
    for (k in 2:n_k) {
      cdf[, k] <- cdf[, k - 1L] + cdf[, k]
    }
  }
  cdf <- cdf / cdf[, n_k]
  u <- stats::runif(nrow(cdf))
  as.integer(rowSums(cdf < u)) + 1L
}
