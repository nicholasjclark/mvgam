#' @importFrom generics augment
#' @export
generics::augment

#' Augment an \pkg{mvgam} object's data
#'
#' Add fits and residuals to the data, implementing the generic `augment` from
#' the package \pkg{broom}.
#'
#' A `list` is returned if `class(x$obs_data) == 'list'`, otherwise a `tibble` is
#' returned, but the contents of either object is the same.
#'
#' The arguments `robust` and `probs` are applied to both the fit and
#' residuals calls (see [fitted.mvgam()] and [residuals.mvgam()] for details).
#'
#' @param x An object of class `mvgam`.
#' @param robust If `FALSE` (the default) the mean is used as the measure of
#' central tendency and the standard deviation as the measure of variability.
#' If `TRUE`, the median and the median absolute deviation (MAD)
#' are applied instead.
#' @param probs The percentiles to be computed by the quantile function.
#' @param ... Unused, included for generic consistency only.
#' @returns A `list` or `tibble` (see details) combining:
#'   * The data supplied to `mvgam()`.
#'   * The outcome variable, named as `.observed`.
#'   * The fitted backcasts, along with their variability and credible bounds.
#'   * The residuals, along with their variability and credible bounds.
#'
#' @seealso \code{\link{residuals.mvgam}}, \code{\link{fitted.mvgam}}
#' @examples
#' \donttest{
#' set.seed(0)
#' dat <- sim_mvgam(T = 80,
#'                  n_series = 3,
#'                  mu = 2,
#'                  trend_model = AR(p = 1),
#'                  prop_missing = 0.1,
#'                  prop_trend = 0.6)
#'
#' mod1 <- mvgam(formula = y ~ s(season, bs = 'cc', k = 6),
#'               data = dat$data_train,
#'               trend_model = AR(),
#'               family = poisson(),
#'               noncentred = TRUE,
#'               chains = 2,
#'               silent = 2)
#'
#' augment(mod1, robust = TRUE, probs = c(0.25, 0.75))
#' }
#'
#' @importFrom stats residuals
#' @export
augment.mvgam <- function(x,
                          robust = FALSE,
                          probs = c(0.025, 0.975),
                          ...) {
  obs_data <- x$obs_data
  obs_data$.observed <- obs_data$y
  obs_data <- purrr::discard_at(obs_data, c("index..orig..order", "index..time..index"))

  resids <- residuals(x, robust = robust, probs = probs) %>%
    tibble::as_tibble()
  fits <- fitted(x, robust = robust, probs = probs) %>%
    tibble::as_tibble()
  hc_fits <- fits %>%
    dplyr::slice_head(n = NROW(resids))  # fits can include fcs
  colnames(resids) <- c(".resid", ".resid.variability", ".resid.cred.low", ".resid.cred.high")
  colnames(hc_fits) <- c(".fitted", ".fit.variability", ".fit.cred.low", ".fit.cred.high")

  augmented <- c(obs_data, hc_fits, resids)  # coerces to list
  if (!identical(class(x$obs_data), "list")) {  # data.frame
    augmented <- tibble::as_tibble(augmented)
  }

  augmented
}
