#' Calculate measures of latent VAR community stability
#'
#' Compute reactivity, return rates and contributions of interactions to
#' stationary forecast variance from \pkg{mvgam} models with Vector
#' Autoregressive dynamics.
#'
#' @name stability.mvgam
#'
#' @param object \code{list} object of class \code{mvgam} resulting from a call
#'   to [mvgam()] that used a `VAR(p = 1)` latent process model
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
#' @param ... Unused. Anything passed here is refused.
#'
#' @details These measures describe how much the lagged interactions
#'   among the series contribute to the variability of the system. They
#'   also describe how the system responds to a perturbation (Ives et
#'   al. 2003). For a latent VAR(1)
#'
#'   \deqn{
#'   x_t \sim \text{MVNormal}(A x_{t - 1}, \Sigma)
#'   }
#'
#'   the stationary covariance \eqn{\Sigma_{\infty}} solves
#'   \eqn{\Sigma_{\infty} = A \Sigma_{\infty} A^\top + \Sigma}. Each
#'   posterior draw of \eqn{A} and \eqn{\Sigma} gives one draw of every
#'   metric:
#'
#'   \itemize{
#'     \item `prop_int`: the proportion of the volume of the stationary
#'     distribution that the lagged interactions generate,
#'     \eqn{\det(A)^2}.
#'
#'     \item `prop_int_adj`: `prop_int` scaled by the number of series
#'     \eqn{p}, \eqn{|\det(A)|^{2/p}}. It compares across systems of
#'     different size.
#'
#'     \item `prop_int_offdiag`, `prop_int_diag`: the sensitivity of
#'     `prop_int` to each cell of \eqn{A} is
#'     \eqn{2 \det(A) (A^{-1})^\top}. `prop_int_offdiag` is the mean
#'     absolute sensitivity of the off-diagonal cells (interactions
#'     between series) as a share of that mean plus the mean absolute
#'     sensitivity of the diagonal cells (self-regulation).
#'     `prop_int_diag` is the remaining share.
#'
#'     \item `prop_cov_offdiag`, `prop_cov_diag`: the same split for the
#'     sensitivity of \eqn{\det(\Sigma_{\infty})} to each cell of
#'     \eqn{\Sigma_{\infty}}, \eqn{\det(\Sigma_{\infty})
#'     (\Sigma_{\infty}^{-1})^\top}. Off-diagonal cells are counted
#'     where \eqn{\Sigma} is estimated.
#'
#'     \item `reactivity`: the log of the largest singular value of
#'     \eqn{A}, \eqn{\log \sigma_{max}(A)} (Neubert et al. 2009). A
#'     positive value means some perturbation grows in the step after
#'     it occurs, before the system returns to equilibrium.
#'
#'     \item `mean_return_rate`: the largest modulus of the eigenvalues
#'     of \eqn{A}. The distance of the mean from its stationary value
#'     shrinks by this factor each step, asymptotically. A value nearer
#'     1 means a slower return.
#'
#'     \item `var_return_rate`: the same factor for the variance, the
#'     largest modulus of the eigenvalues of \eqn{A \otimes A}, which
#'     is `mean_return_rate` squared.
#'   }
#'
#'   The prior on \eqn{A} keeps every draw stationary (Heaps 2023).
#'   \eqn{\Sigma_{\infty}} and every metric then exist in every draw.
#'
#'   [irf()] and [fevd()] describe the same dynamics as responses to
#'   shocks.
#'
#' @return A \code{data.frame} with the posterior median and interval of
#'   each stability metric, or under `summary = FALSE` an object of class
#'   `mvgam_stability` holding one row of metrics per posterior draw.
#'
#' @references
#'   AR Ives, B Dennis, KL Cottingham & SR Carpenter (2003). Estimating
#'   community stability and ecological interactions from time-series data.
#'   *Ecological Monographs*, 73, 301-330.
#'
#'   MG Neubert, H Caswell & AR Solow (2009). Detecting reactivity.
#'   *Ecology*, 90, 2683-2688.
#'
#'   SE Heaps (2023). Enforcing stationarity through the prior in vector
#'   autoregressions. *Journal of Computational and Graphical
#'   Statistics*, 32, 74-83.
#'
#' @author Nicholas J Clark
#'
#' @seealso
#'   \code{\link{VAR}}, \code{\link{irf}}, \code{\link{fevd}},
#'   \code{\link{plot.mvgam_stability}},
#'   \code{\link{plot.mvgam_irf}},
#'   \code{\link{plot.mvgam_fevd}},
#'   \code{\link{plot.mvgam_forecast}}.
#'   The article at
#'   <https://nicholasjclark.github.io/mvgam/articles/var.html>
#'   runs `stability()` on a plankton community.
#'
#' @examples
#' \dontrun{
#' set.seed(0)
#' simdat <- sim_mvgam(family = gaussian(), n_series = 3L,
#'                     n_timepoints = 120L, trend_model = VAR(),
#'                     prop_trend = 0.95)
#' var_mod <- mvgam(y ~ 1, trend_formula = ~ VAR(p = 1),
#'                  data = simdat$data_train, family = gaussian(),
#'                  chains = 2, silent = 2)
#'
#' # Posterior medians and intervals of the stability metrics
#' stab <- stability(var_mod)
#' stab[stab$metric %in% c("reactivity", "mean_return_rate"), ]
#'
#' # The full posterior of each metric
#' plot(stability(var_mod, summary = FALSE))
#' }
#'
#' @export
stability <- function(object, ...) {
  UseMethod("stability", object)
}

# Discrete Lyapunov solver by doubling (Kitagawa 1977; Anderson
# 1979). Solves X = B X B' + Sigma for X when B has spectral radius
# below 1, which the stationarity prior of Heaps (2023) guarantees.
# Doubling sums the series `sum_{k>=0} B^k Sigma (B')^k`. After `k`
# steps the partial sum covers `2^k` terms and the multiplier is
# `B^{2^k}`, which decays like `rho(B)^{2^k}`. Each solve costs
# O(K^3 log(1/tol)). The Kronecker form `solve(I - B %x% B)` costs
# O(K^6).
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
stability.mvgam <- function(object, ndraws = NULL, draw_ids = NULL,
                           summary = TRUE, probs = c(0.025, 0.975),
                           future = FALSE, ...) {
  probs <- assert_var_surface_args(ndraws, draw_ids, summary, probs,
                                   future)
  rlang::check_dots_empty()
  assert_var_trend(object, surface = "stability()")
  # The Ives et al. (2003) metrics and reactivity describe first-order
  # dynamics. A higher-order or moving-average VAR has no single
  # transition matrix they apply to.
  meta <- object$trend_metadata
  if (length(meta$ar_lags %||% 1L) > 1L || length(meta$ma_lags) > 0L) {
    stop(insight::format_error(c(
      "'stability()' is not supported for VAR(p > 1) or VARMA trends.",
      i = "The stability metrics are defined for one VAR(1) transition matrix.",
      i = "'irf()' and 'fevd()' describe the dynamics of this fit."
    )), call. = FALSE)
  }
  # Each draw costs a Lyapunov solve at O(K^3 log(1/tol)), so a wide
  # panel is worth answering from a subset. The coefficients and the
  # innovation covariance are read from the same draws, since a
  # stationary variance built from one draw's transition matrix and
  # another's covariance describes no posterior sample.
  var_post <- extract_var_posterior(object, ndraws, draw_ids)

  metrics <- do.call(
    rbind,
    mvgam_maybe_future_lapply(var_post$ndraws, function(i) {
      B <- var_post$A[i, , , 1L]
      p <- var_post$K
      Sigma <- var_post$Sigma[i, , , drop = TRUE]

      # The stationary variance solves the discrete Lyapunov equation
      # Sigma_inf = B Sigma_inf B' + Sigma
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
      # contribution. Because `B` is not symmetric, every off-diagonal
      # cell counts. A hierarchical fit's `B` is block-diagonal. Its
      # cells outside the blocks are structural zeros.
      b_mask <- B != 0
      off_mask_b <- row(B) != col(B) & b_mask
      off_vals_b <- abs(int_sens[off_mask_b])
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
    }, future = future)
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
#' @param bins Number of bins used to record the shape of each metric
#'   alongside its interval, which is what `plot()` draws
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A `data.frame` with one row per metric
#'
#' @method summary mvgam_stability
#' @export
summary.mvgam_stability <- function(object, probs = c(0.025, 0.975),
                                    robust = TRUE, bins = 30L, ...) {
  checkmate::assert_class(object, "mvgam_stability")
  checkmate::assert_numeric(probs, len = 2L, lower = 0, upper = 1,
                            any.missing = FALSE, sorted = TRUE)
  checkmate::assert_flag(robust)
  checkmate::assert_int(bins, lower = 5L)
  rlang::check_dots_empty()
  # The summary every other accessor reports. A metric with an `NA`
  # draw reports `NA`.
  metrics <- colnames(object)
  draws <- as.matrix(as.data.frame(object)[, metrics, drop = FALSE])
  out <- data.frame(
    metric = metrics,
    mvgam_post_summary(draws, robust = robust, probs = probs),
    check.names = FALSE, stringsAsFactors = FALSE,
    row.names = NULL
  )
  # A median and an interval say where a metric sits but not what
  # shape it has, and the shape is often why the metric was asked for:
  # reactivity is read for whether its mass crosses zero, not for its
  # midpoint. Binning each metric on the way through keeps that shape
  # at a few hundred numbers rather than a few thousand draws, so the
  # summary can be drawn as the same histogram without carrying what
  # it was built from.
  attr(out, "bin_counts") <- lapply(stats::setNames(metrics, metrics),
                                    function(v) bin_draws(object[[v]], bins))
  attr(out, "ndraws") <- nrow(object)
  class(out) <- c("mvgam_stability_summary", class(out))
  out
}


#' Bin a vector of draws for later display
#'
#' Keeps the counts and the edges rather than the draws, which is all a
#' histogram needs and is a fraction of the size. A metric that never
#' varies gets a single degenerate bin rather than an error.
#'
#' @param draws Numeric vector of posterior draws
#' @param bins Number of bins
#' @return List with `breaks` and `counts`
#'
#' @noRd
bin_draws <- function(draws, bins) {
  draws <- draws[is.finite(draws)]
  if (!length(draws)) {
    return(list(breaks = c(0, 0), counts = 0L))
  }
  rng <- range(draws)
  if (isTRUE(all.equal(rng[1L], rng[2L]))) {
    return(list(breaks = c(rng[1L], rng[1L]), counts = length(draws)))
  }
  breaks <- seq(rng[1L], rng[2L], length.out = bins + 1L)
  h <- graphics::hist(draws, breaks = breaks, plot = FALSE)
  list(breaks = h$breaks, counts = as.integer(h$counts))
}


#' Plot summarised stability metrics
#'
#' Draws the same histogram as the plot for the draws. `summary()`
#' bins each metric on its way through, so the shape survives without
#' the draws behind it: a metric is often asked for to see whether its
#' mass crosses zero, which a median and an interval alone do not
#' answer. Pass `intervals = TRUE` for a median and interval instead.
#'
#' @param x An object of class `mvgam_stability_summary`
#' @param variables Metrics to draw
#' @param intervals Logical; draw each metric as a median and interval
#'   rather than as its binned posterior
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A `ggplot` object
#'
#' @method plot mvgam_stability_summary
#' @export
plot.mvgam_stability_summary <- function(
  x,
  variables = c("reactivity", "mean_return_rate", "var_return_rate"),
  intervals = FALSE,
  ...
) {
  checkmate::assert_class(x, "mvgam_stability_summary")
  checkmate::assert_flag(intervals)
  rlang::check_dots_empty()
  keep <- resolve_stability_metrics(variables, x$metric)
  set_color_scheme_local("red")

  counts <- attr(x, "bin_counts")
  if (intervals || is.null(counts)) {
    dat <- x[x$metric %in% keep, , drop = FALSE]
    dat$metric <- factor(dat$metric, levels = keep)
    bounds <- grep("^Q", colnames(dat), value = TRUE)
    return(
      ggplot2::ggplot(
        dat, ggplot2::aes(x = .data$metric, y = .data$Estimate)
      ) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                            colour = "grey30") +
        ggplot2::geom_pointrange(
          ggplot2::aes(ymin = .data[[bounds[1L]]],
                       ymax = .data[[bounds[2L]]]),
          colour = mvgam_palette()[4L]
        ) +
        ggplot2::scale_x_discrete(labels = stability_metric_labels(keep)) +
        ggplot2::labs(x = NULL, y = "Posterior estimate") +
        mvgam_theme()
    )
  }
  stability_histogram(counts[keep])
}


#' Plot posterior distributions of latent VAR stability metrics
#'
#' Renders a faceted histogram of the reactivity, mean return rate
#' and variance return rate posterior draws returned by
#' `stability.mvgam()`. A dashed reference line at zero on the
#' reactivity panel marks the threshold above which the system
#' amplifies a shock before it decays.
#'
#' @param x A `mvgam_stability` object returned by [stability()].
#' @param variables Character vector picking which columns of `x`
#'   to render. Defaults to the three core dynamic-stability
#'   metrics (`"reactivity"`, `"mean_return_rate"`,
#'   `"var_return_rate"`); pass any subset of `x`'s column names
#'   to widen or narrow the panel set.
#' @param bins Number of histogram bins.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A `ggplot` object.
#' @seealso [stability()], [irf()], [fevd()],
#'   [plot.mvgam_irf()], [plot.mvgam_fevd()],
#'   [plot.mvgam_forecast()]
#' @method plot mvgam_stability
#' @export
plot.mvgam_stability <- function(
  x,
  variables = c("reactivity", "mean_return_rate", "var_return_rate"),
  bins = 30L,
  ...
) {
  checkmate::assert_class(x, "mvgam_stability")
  checkmate::assert_int(bins, lower = 5L)
  rlang::check_dots_empty()
  keep <- resolve_stability_metrics(variables, colnames(x))
  # The red scheme `irf()`, `fevd()` and `forecast()` plots use,
  # restored when the call returns.
  set_color_scheme_local("red")
  stability_histogram(lapply(stats::setNames(keep, keep), function(v) {
    bin_draws(x[[v]], bins)
  }))
}


#' The stability metrics a plot was asked for
#'
#' A metric that is not there is refused by name. Plotting the ones
#' that are drew fewer panels than were asked for.
#'
#' @param variables The metrics requested
#' @param available The metrics the object holds
#' @return `variables`, deduplicated
#' @noRd
resolve_stability_metrics <- function(variables, available) {
  checkmate::assert_character(variables, min.len = 1L, any.missing = FALSE)
  unknown <- setdiff(variables, available)
  if (length(unknown)) {
    stop(insight::format_error(c(
      "Some requested 'variables' are not stability metrics.",
      x = paste0(
        "Not found: ", paste0("'", unknown, "'", collapse = ", "), "."
      ),
      i = paste0(
        "Available metrics: ", paste(available, collapse = ", "), "."
      )
    )), call. = FALSE)
  }
  unique(variables)
}


#' Faceted histogram of binned stability metrics
#'
#' Each bar is drawn at its own midpoint and width, which keeps an
#' uneven final bin its true width. Zero is a threshold for reactivity
#' alone, and the reference line is drawn on that panel only. Drawn on
#' a return rate, it squeezed the posterior into the edge of its panel.
#'
#' @param counts Named list of `bin_draws()` results, one per metric
#' @return A `ggplot` object
#' @noRd
stability_histogram <- function(counts) {
  long <- do.call(rbind, lapply(names(counts), function(v) {
    b <- counts[[v]]
    if (is.null(b) || !length(b$counts)) {
      return(NULL)
    }
    lower <- utils::head(b$breaks, -1L)
    upper <- utils::tail(b$breaks, -1L)
    data.frame(metric = v, mid = (lower + upper) / 2,
               width = upper - lower, count = b$counts,
               stringsAsFactors = FALSE)
  }))
  long$metric <- factor(long$metric, levels = names(counts))
  threshold <- data.frame(
    metric = factor(intersect("reactivity", names(counts)),
                    levels = names(counts)),
    x = 0
  )
  ggplot2::ggplot(long, ggplot2::aes(x = .data$mid, y = .data$count)) +
    ggplot2::geom_col(
      ggplot2::aes(width = .data$width),
      fill = mvgam_palette()[4L],
      colour = "white"
    ) +
    ggplot2::geom_vline(data = threshold,
                        ggplot2::aes(xintercept = .data$x),
                        linetype = "dashed", colour = "grey30") +
    ggplot2::facet_wrap(
      ~ metric, scales = "free", nrow = 1L,
      labeller = ggplot2::as_labeller(stability_metric_labels(names(counts)))
    ) +
    ggplot2::labs(x = "Value", y = "Posterior draws") +
    mvgam_theme()
}


#' Display names of stability metrics
#'
#' @param metrics Metric names as `stability()` returns them
#' @return Named character vector of display names. A metric without
#'   one keeps its own name.
#' @noRd
stability_metric_labels <- function(metrics) {
  display <- c(reactivity = "Reactivity",
               mean_return_rate = "Mean return rate",
               var_return_rate = "Variance return rate")
  out <- stats::setNames(metrics, metrics)
  known <- metrics %in% names(display)
  out[known] <- display[metrics[known]]
  out
}
