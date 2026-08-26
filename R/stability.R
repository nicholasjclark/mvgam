#' Calculate measures of latent VAR community stability
#'
#' Compute reactivity, return rates and contributions of interactions to
#' stationary forecast variance from \pkg{mvgam} models with Vector
#' Autoregressive dynamics.
#'
#' @name stability.mvgam
#'
#' @param object \code{list} object of class \code{mvgam} resulting from a call
#'   to [mvgam()] that used a Vector Autoregressive latent process model (either
#'   as `VAR(cor = FALSE)` or `VAR(cor = TRUE)`)
#'
#'@param ndraws Optional integer; the number of posterior draws to use.
#'  Each draw costs a Lyapunov solve, so a wide panel is worth
#'  answering from a subset. Default `NULL` uses every draw.
#'@param draw_ids Optional integer vector naming the posterior draws to
#'  use, in place of `ndraws`.
#'@param summary Logical; return the posterior median and interval of
#'  each metric (the default), or the per-draw metrics themselves. The
#'  spread of a metric is often the point of asking, and `plot()` draws
#'  histograms of the draws.
#'@param probs The lower and upper percentiles to report alongside the
#'  median when `summary = TRUE`.
#' @param future \code{Logical}. When `TRUE`, per-draw stability
#'   computation runs under whatever
#'   \code{\link[future:plan]{future::plan()}} the caller has set;
#'   see [`irf()`] for details. Requires the \code{future} package
#'   (mvgam Suggests).
#'
#' @param ... Ignored
#'
#' @details These measures of stability can be used to assess how important
#'   inter-series dependencies are to the variability of a multivariate system
#'   and to ask how systems are expected to respond to environmental
#'   perturbations. Using the formula for a latent VAR(1) as:
#'
#'   \deqn{
#'   \mu_t \sim \text{MVNormal}(A(\mu_{t - 1}), \Sigma)
#'   }
#'
#'   this function will calculate the long-term stationary forecast distribution
#'   of the system, which has mean \eqn{\mu_{\infty}} and variance
#'   \eqn{\Sigma_{\infty}}, to then calculate the following quantities:
#'
#'   \itemize{
#'     \item `prop_int`: Proportion of the volume of the stationary forecast
#'     distribution that is attributable to lagged interactions:
#'     \deqn{ det(A)^2 }
#'
#'     \item `prop_int_adj`: Same as `prop_int` but scaled by the number of
#'     series \eqn{p}:
#'     \deqn{ det(A)^{2/p} }
#'
#'     \item `prop_int_offdiag`: Sensitivity of `prop_int` to inter-series
#'     interactions (off-diagonals of \eqn{A}):
#'     \deqn{ [2~det(A) (A^{-1})^T] }
#'
#'     \item `prop_int_diag`: Sensitivity of `prop_int` to intra-series
#'     interactions (diagonals of \eqn{A}):
#'     \deqn{ [2~det(A) (A^{-1})^T] }
#'
#'     \item `prop_cov_offdiag`: Sensitivity of \eqn{\Sigma_{\infty}} to
#'     inter-series error correlations:
#'     \deqn{ [2~det(\Sigma_{\infty}) (\Sigma_{\infty}^{-1})^T] }
#'
#'     \item `prop_cov_diag`: Sensitivity of \eqn{\Sigma_{\infty}} to error
#'     variances:
#'     \deqn{ [2~det(\Sigma_{\infty}) (\Sigma_{\infty}^{-1})^T] }
#'
#'     \item `reactivity`: Degree to which the system moves away from a stable
#'     equilibrium following a perturbation. If \eqn{\sigma_{max}(A)} is the
#'     largest singular value of \eqn{A}:
#'     \deqn{ \log\sigma_{max}(A) }
#'
#'     \item `mean_return_rate`: Asymptotic return rate of the mean of the
#'     transition distribution to the stationary mean:
#'     \deqn{ \max(\lambda_{A}) }
#'
#'     \item `var_return_rate`: Asymptotic return rate of the variance of the
#'     transition distribution to the stationary variance:
#'     \deqn{ \max(\lambda_{A \otimes A}) }
#'   }
#'
#'   Major advantages of using \pkg{mvgam} to compute these metrics are that
#'   well-calibrated uncertainties are available and that VAR processes are
#'   forced to be stationary. These properties make it simple and insightful to
#'   calculate and inspect aspects of both long-term and short-term stability.
#'
#'   You can also inspect interactions among the time series in a latent VAR
#'   process using \code{\link{irf}} for impulse response functions or
#'   \code{\link{fevd}} for forecast error variance decompositions.
#'
#' @return A \code{data.frame} containing posterior draws for each stability
#'   metric.
#'
#' @references
#'   AR Ives, B Dennis, KL Cottingham & SR Carpenter (2003). Estimating
#'   community stability and ecological interactions from time-series data.
#'   *Ecological Monographs*, 73, 301–330.
#'
#' @author Nicholas J Clark
#'
#' @seealso
#'   \code{\link{VAR}}, \code{\link{irf}}, \code{\link{fevd}},
#'   \code{\link{plot.mvgam_stability}},
#'   \code{\link{plot.mvgam_irf}},
#'   \code{\link{plot.mvgam_fevd}},
#'   \code{\link{plot.mvgam_forecast}}.
#'   For a worked article that runs `stability()` end to end
#'   on an annual bird-count VAR, see
#'   [https://nicholasjclark.github.io/mvgam/articles/var.html](https://nicholasjclark.github.io/mvgam/articles/var.html).
#'
#' @examples
#' \dontrun{
#' # See `?trend_constructors` for a runnable end-to-end VAR
#' # example that exercises `stability()` alongside `irf()`,
#' # `fevd()` and `residual_cor()` on the same fit. The shape
#' # below summarises the API.
#' head(stability(var_mod)[, c("reactivity", "mean_return_rate")])
#' }
#'
#' @export
stability <- function(object, ...) {
  UseMethod("stability", object)
}

# Discrete Lyapunov solver via doubling (Kitagawa 1977 / Anderson
# 1979). Solves X = B X B' + Sigma for X, assuming B has spectral
# radius < 1 (guaranteed under mvgam's Heaps 2022 stationarity
# prior on VAR fits). Doubling accumulates the Neumann series
# `sum_{k>=0} B^k Sigma (B')^k` by squaring the partial sum each
# iteration; after `k` steps the partial sum covers `2^k` terms
# and the multiplier factor becomes `B^{2^k}`, which decays like
# `rho(B)^{2^k}`. Cost is O(K^3 log(1/tol)) per solve rather than
# the O(K^6) dense LU on `I(K^2) - B kron B` the Kronecker form
# uses. Called from `stability.mvgam()` once per posterior draw
# (`R/stability.R:113`); dominated the wall-clock at K >= 12 in
# the earlier implementation.
#'@noRd
solve_dlyap <- function(B, Sigma, tol = 1e-12, max_iter = 100L) {
  X <- Sigma
  A <- B
  for (iter in seq_len(max_iter)) {
    X <- X + A %*% X %*% t(A)
    A <- A %*% A
    if (max(abs(A)) < tol) break
  }
  X
}

#'@rdname stability.mvgam
#'@method stability mvgam
#'@export
stability.mvgam = function(object, ndraws = NULL, draw_ids = NULL,
                           summary = TRUE, probs = c(0.025, 0.975),
                           future = FALSE, ...) {
  checkmate::assert_int(ndraws, lower = 1L, null.ok = TRUE)
  checkmate::assert_integerish(draw_ids, lower = 1L, null.ok = TRUE)
  checkmate::assert_flag(summary)
  checkmate::assert_numeric(probs, len = 2L, lower = 0, upper = 1,
                            any.missing = FALSE, sorted = TRUE)
  checkmate::assert_flag(future)
  assert_var_trend(object, surface = "stability()")
  # Each draw costs a Lyapunov solve at O(K^3 log(1/tol)), so a wide
  # panel is worth answering from a subset. The coefficients and the
  # innovation covariance are read from the same draws, since a
  # stationary variance built from one draw's transition matrix and
  # another's covariance describes no posterior sample.
  var_post <- extract_var_posterior(object, ndraws, draw_ids)

  metrics <- do.call(
    rbind,
    mvgam_maybe_future_lapply(var_post$ndraws, function(i) {
      B <- var_post$A[i, , , drop = TRUE]
      p <- var_post$K
      Sigma <- var_post$Sigma[i, , , drop = TRUE]

      # Stationary variance Sigma_inf satisfies the discrete Lyapunov
      # equation Sigma_inf = B Sigma_inf B' + Sigma. Solve via
      # doubling (Kitagawa 1977 / Anderson 1979) at O(K^3 log(1/tol))
      # rather than the earlier `solve(I - B %x% B) %*% vec(Sigma)`
      # Kronecker form at O(K^6), which was dominating stability()
      # wall-clock at K >= 12.
      Sigma_inf <- solve_dlyap(B, Sigma)
      # Enforce symmetry so downstream `solve(Sigma_inf)` and
      # `det(Sigma_inf)` do not inherit floating-point asymmetry.
      Sigma_inf <- (Sigma_inf + t(Sigma_inf)) / 2

      # The difference in volume between Sigma_inf and Sigma is:
      # det(Sigma_inf - Sigma) = det(Sigma_inf) * det(B) ^ 2
      # according to Ives et al 2003 (eqn 24)

      # We can take partial derivatives to determine which elements of
      # Sigma_inf contribute most to rates of change in the
      # proportion of Sigma_inf that is due to process error
      # Thanks to Mark Scheuerell for providing inspirational code
      # https://github.com/mdscheuerell/safs-quant-sem-2022/blob/main/lwa_analysis.R
      int_env <- det(Sigma_inf) * t(solve(Sigma_inf))

      # Proportion of inter-series covariance to overall environmental
      # variation contribution (i.e. how important are correlated
      # errors for controlling the shape of the stationary forecast
      # distribution?). The mask `Sigma != 0` restricts the
      # off-diagonal average to cells the fit actually estimates:
      # for a non-hierarchical VAR(cor = TRUE) fit every off-diagonal
      # is modelled and the mask covers the full lower triangle;
      # for a hierarchical VAR the block-diagonal parameterisation
      # leaves inter-group cells as structural zeros, and averaging
      # them in would drive the ratio toward zero by construction.
      sigma_mask <- Sigma != 0
      lower_mask <- lower.tri(int_env) & sigma_mask
      off_vals <- abs(int_env[lower_mask])
      diag_vals <- abs(diag(int_env))
      mean_off <- if (length(off_vals)) mean(off_vals) else 0
      mean_diag <- if (length(diag_vals)) mean(diag_vals) else 0
      dat <- data.frame(
        prop_cov_offdiag = if ((mean_diag + mean_off) > 0) {
          mean_off / (mean_diag + mean_off)
        } else 0
      )

      # Proportion of error variances to stationary forecast distribution
      dat$prop_cov_diag <- 1 - dat$prop_cov_offdiag

      # Proportion of volume of Sigma_inf attributable to series interactions,
      # measuring the degree to which interactions increase
      # the variance of the stationary distribution (Sigma_inf) relative
      # to the variance of the process error (Sigma)
      # lower values = more stability
      dat$prop_int = abs(det(B))^2

      # Ives et al 2003 suggest to scale this by the number of series for more direct
      # comparisons among different studies
      dat$prop_int_adj <- abs(det(B))^(2 / p)

      # Sensitivity of the species interaction proportion to particular
      # interactions is also calculated using partial derivatives
      # (note the use of 2 here because we squared det(B) in the above eqn)
      int_sens <- 2 * det(B) * t(solve(B))

      # Proportion of interspecific contributions to overall interaction
      # contribution. Same block-aware masking as `prop_cov_offdiag`:
      # for hierarchical fits `B` is block-diagonal, so only
      # within-block off-diagonals count.
      b_mask <- B != 0
      lower_mask_b <- lower.tri(int_sens) & b_mask
      off_vals_b <- abs(int_sens[lower_mask_b])
      diag_vals_b <- abs(diag(int_sens))
      mean_off_b <- if (length(off_vals_b)) mean(off_vals_b) else 0
      mean_diag_b <- if (length(diag_vals_b)) mean(diag_vals_b) else 0
      dat$prop_int_offdiag <- if ((mean_diag_b + mean_off_b) > 0) {
        mean_off_b / (mean_diag_b + mean_off_b)
      } else 0

      # Proportion of density dependent contributions to
      # to overall interaction contribution
      dat$prop_int_diag <- 1 - dat$prop_int_offdiag

      # Reactivity, measuring the degree to which the system moves
      # away from a stable equilibrium following a perturbation
      # values > 0 suggest the system is reactive, whereby a
      # perturbation of the system in one period can be amplified in the next period
      # Following Neubert et al 2009 Ecology (Detecting reactivity)
      dat$reactivity <- log(max(svd(B)$d))

      # Return rate of transition distribution to the stationary distribution
      # Asymptotic return rate of the mean and variance. Reuse the
      # eigenvalues of B for both: eigen(B kron B) equals the
      # tensor product of eigen(B) with itself, so
      # max|eigen(B kron B)| = max|eigen(B)|^2 exactly. Avoids a
      # second O(K^6) eigen decomposition on the K^2 x K^2 Kronecker.
      lam_B <- eigen(B, only.values = TRUE)$values
      dat$mean_return_rate <- max(abs(lam_B))
      dat$var_return_rate <- dat$mean_return_rate^2
      dat
    })
  )
  class(metrics) <- c("mvgam_stability", class(metrics))
  if (summary) {
    return(summary(metrics, probs = probs))
  }
  metrics
}


#' Summarise posterior stability metrics
#'
#' Reports the posterior median and interval of each stability metric.
#' `stability()` returns this by default, matching the other post-fit
#' surfaces; the draws behind it are available with
#' `stability(summary = FALSE)` and are what `plot()` draws histograms
#' of.
#'
#' @param object An object of class `mvgam_stability`
#' @param probs The lower and upper percentiles to report alongside the
#'   median
#' @param robust Logical; report the median and median absolute
#'   deviation rather than the mean and standard deviation
#' @param ... ignored
#'
#' @return A `data.frame` with one row per metric
#'
#' @method summary mvgam_stability
#' @export
summary.mvgam_stability <- function(object, probs = c(0.025, 0.975),
                                    robust = TRUE, ...) {
  checkmate::assert_class(object, "mvgam_stability")
  checkmate::assert_numeric(probs, len = 2L, lower = 0, upper = 1,
                            any.missing = FALSE, sorted = TRUE)
  checkmate::assert_flag(robust)
  metrics <- colnames(object)
  out <- do.call(rbind, lapply(metrics, function(v) {
    draws <- object[[v]]
    data.frame(
      metric = v,
      estimate = if (robust) stats::median(draws) else mean(draws),
      est_error = if (robust) stats::mad(draws) else stats::sd(draws),
      lower = unname(stats::quantile(draws, min(probs))),
      upper = unname(stats::quantile(draws, max(probs))),
      stringsAsFactors = FALSE
    )
  }))
  colnames(out) <- c("metric", "Estimate", "Est.Error",
                     paste0("Q", 100 * min(probs)),
                     paste0("Q", 100 * max(probs)))
  class(out) <- c("mvgam_stability_summary", class(out))
  out
}


#' Plot summarised stability metrics
#'
#' Draws the posterior median and interval of each metric. The
#' distribution of a metric is often the point of asking, so
#' `stability(summary = FALSE)` returns the draws and plots them as
#' histograms instead.
#'
#' @param x An object of class `mvgam_stability_summary`
#' @param variables Metrics to draw
#' @param ... ignored
#'
#' @return A `ggplot` object
#'
#' @method plot mvgam_stability_summary
#' @export
plot.mvgam_stability_summary <- function(
  x,
  variables = c("reactivity", "mean_return_rate", "var_return_rate"),
  ...
) {
  checkmate::assert_class(x, "mvgam_stability_summary")
  checkmate::assert_character(variables, min.len = 1L, any.missing = FALSE)
  keep <- intersect(variables, x$metric)
  if (!length(keep)) {
    stop(insight::format_error(c(
      "None of the requested 'variables' were found in 'x'.",
      i = paste0("Available metrics: ",
                 paste(x$metric, collapse = ", "), ".")
    )))
  }
  dat <- x[x$metric %in% keep, , drop = FALSE]
  dat$metric <- factor(dat$metric, levels = keep)
  bounds <- grep("^Q", colnames(dat), value = TRUE)
  set_color_scheme_local("red")
  ggplot2::ggplot(
    dat, ggplot2::aes(x = .data$metric, y = .data$Estimate)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        colour = "grey30") +
    ggplot2::geom_pointrange(
      ggplot2::aes(ymin = .data[[bounds[1L]]], ymax = .data[[bounds[2L]]]),
      colour = mvgam_palette()[4L]
    ) +
    ggplot2::labs(x = NULL, y = "Posterior estimate") +
    mvgam_theme()
}


#' Plot posterior distributions of latent VAR stability metrics
#'
#' Renders a faceted histogram of the reactivity, mean return rate
#' and variance return rate posterior draws returned by
#' `stability.mvgam()`. A dashed reference line at zero marks the
#' reactivity threshold above which shocks are amplified rather
#' than absorbed.
#'
#' @param x A `mvgam_stability` object returned by [stability()].
#' @param variables Character vector picking which columns of `x`
#'   to render. Defaults to the three core dynamic-stability
#'   metrics (`"reactivity"`, `"mean_return_rate"`,
#'   `"var_return_rate"`); pass any subset of `x`'s column names
#'   to widen or narrow the panel set.
#' @param bins Number of histogram bins passed to `geom_histogram`.
#' @param ... Ignored.
#'
#' @return A `ggplot` object.
#' @seealso [stability()], [irf()], [fevd()],
#'   [plot.mvgam_irf()], [plot.mvgam_fevd()],
#'   [plot.mvgam_forecast()]
#' @method plot mvgam_stability
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline
#'   facet_wrap labs
#' @export
plot.mvgam_stability = function(
  x,
  variables = c("reactivity", "mean_return_rate", "var_return_rate"),
  bins = 30L,
  ...
) {
  checkmate::assert_class(x, "mvgam_stability")
  checkmate::assert_character(variables, min.len = 1L, any.missing = FALSE)
  checkmate::assert_int(bins, lower = 5L)
  keep <- intersect(variables, colnames(x))
  if (!length(keep)) {
    stop(insight::format_error(c(
      "None of the requested 'variables' were found in 'x'.",
      i = paste0(
        "Available metrics: ",
        paste(colnames(x), collapse = ", "), "."
      )
    )))
  }
  # Force the house red scheme for the duration of this call so
  # stability sits in the same visual family as irf() / fevd() /
  # forecast() plots.
  set_color_scheme_local("red")
  long <- do.call(rbind, lapply(keep, function(v) {
    data.frame(metric = v, value = x[[v]])
  }))
  long$metric <- factor(long$metric, levels = keep)
  ggplot2::ggplot(long, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(
      bins = bins,
      fill = mvgam_palette()[4L],
      colour = "white"
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      colour = "grey30"
    ) +
    ggplot2::facet_wrap(~ metric, scales = "free", nrow = 1L) +
    ggplot2::labs(x = "Posterior draw", y = "Frequency") +
    mvgam_theme()
}
