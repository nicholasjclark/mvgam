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
#' @param groups Hierarchical VAR fits only. The panels to summarise,
#'   by name or by index into the levels of `gr`. The default `NULL`
#'   summarises every panel. A non-hierarchical fit accepts `NULL`
#'   alone.
#'
#'@param ndraws Optional integer; the number of posterior draws to use.
#'  Each draw costs a Lyapunov solve. A subset of draws shortens the
#'  computation on a wide panel. Default `NULL` uses every draw.
#'@param draw_ids Optional integer vector naming the posterior draws to
#'  use, in place of `ndraws`.
#'@param summary Logical. `TRUE` (the default) returns the posterior
#'  median and interval of each metric. `FALSE` returns the metrics of
#'  each draw. `plot()` draws histograms of either form.
#'@param probs The lower and upper percentiles of the interval reported
#'  with the median when `summary = TRUE`.
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
stability.mvgam <- function(object, groups = NULL, ndraws = NULL,
                            draw_ids = NULL, summary = TRUE,
                            probs = c(0.025, 0.975), future = FALSE,
                            ...) {
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
  # The panels of a hierarchical VAR do not interact. Over the whole
  # block-diagonal system, reactivity and the return rates are those of
  # the most extreme panel. `prop_int` would multiply the panels'
  # determinants together. Each panel is summarised alone.
  hierarchical <- !is.null(object$standata$N_groups_trend)
  group_names <- resolve_var_groups(
    object, if (hierarchical) groups %||% "all" else groups
  )
  blocks <- if (hierarchical) {
    all_groups <- group_labels(object, var_dim(object, "N_groups_trend"))
    get_group_info(object$standata)$members[
      match(group_names, all_groups)
    ]
  }
  # Each draw costs a Lyapunov solve at O(K^3 log(1/tol)). A stationary
  # variance built from one draw's transition matrix and another's
  # covariance describes no posterior sample. The coefficients and the
  # innovation covariance come from the same subset of draws.
  var_post <- extract_var_posterior(object, ndraws, draw_ids)
  blocks <- blocks %||% list(seq_len(var_post$K))

  metrics <- do.call(
    rbind,
    mvgam_maybe_future_lapply(var_post$ndraws, function(i) {
      do.call(rbind, lapply(blocks, function(members) {
        stability_metrics(
          var_post$A[i, members, members, 1L],
          var_post$Sigma[i, members, members]
        )
      }))
    }, future = future)
  )
  if (hierarchical) {
    metrics <- data.frame(
      group = factor(rep(group_names, var_post$ndraws),
                     levels = group_names),
      metrics
    )
  }
  class(metrics) <- c("mvgam_stability", class(metrics))
  if (summary) {
    return(summary(metrics, probs = probs))
  }
  metrics
}


#' Stability metrics of one VAR(1) transition matrix
#'
#' @param B Transition matrix (K x K)
#' @param Sigma Innovation covariance (K x K)
#' @return One-row `data.frame` of the metrics `stability()` documents
#' @noRd
stability_metrics <- function(B, Sigma) {
  B <- as.matrix(B)
  Sigma <- as.matrix(Sigma)
  p <- nrow(B)

  # The stationary variance solves the discrete Lyapunov equation
  # Sigma_inf = B Sigma_inf B' + Sigma. Symmetrising it keeps
  # floating-point asymmetry out of `solve()` and `det()` below.
  Sigma_inf <- solve_dlyap(B, Sigma)
  Sigma_inf <- (Sigma_inf + t(Sigma_inf)) / 2

  # The sensitivity of det(Sigma_inf) to each of its cells (Ives et al.
  # 2003, eqn 24). Mark Scheuerell's analysis of Lake Washington
  # plankton
  # (https://github.com/mdscheuerell/safs-quant-sem-2022/blob/main/lwa_analysis.R)
  # computes the same partial derivatives.
  int_env <- det(Sigma_inf) * t(solve(Sigma_inf))
  # Off-diagonal cells count where Sigma is estimated. A VAR fitted
  # with `cor = FALSE` holds its off-diagonal covariances at zero.
  off_cov <- abs(int_env[lower.tri(int_env) & Sigma != 0])
  prop_cov_offdiag <- offdiag_share(off_cov, abs(diag(int_env)))

  # The sensitivity of det(B)^2 to each cell of B. B is not symmetric,
  # and every off-diagonal cell counts.
  int_sens <- 2 * det(B) * t(solve(B))
  prop_int_offdiag <- offdiag_share(abs(int_sens[row(B) != col(B)]),
                                    abs(diag(int_sens)))

  # Reactivity follows Neubert et al. (2009). The eigenvalues of
  # B %x% B are the pairwise products of those of B. The largest
  # modulus among them is the square of the mean return rate.
  mean_return_rate <- max(Mod(eigen(B, only.values = TRUE)$values))
  data.frame(
    prop_cov_offdiag = prop_cov_offdiag,
    prop_cov_diag = 1 - prop_cov_offdiag,
    prop_int = abs(det(B))^2,
    prop_int_adj = abs(det(B))^(2 / p),
    prop_int_offdiag = prop_int_offdiag,
    prop_int_diag = 1 - prop_int_offdiag,
    reactivity = log(max(svd(B)$d)),
    mean_return_rate = mean_return_rate,
    var_return_rate = mean_return_rate^2
  )
}


#' Share of the mean absolute off-diagonal sensitivity
#'
#' @param off Absolute sensitivities of the off-diagonal cells
#' @param diag_vals Absolute sensitivities of the diagonal cells
#' @return `mean(off) / (mean(off) + mean(diag_vals))`, with an empty
#'   set counting as zero
#' @noRd
offdiag_share <- function(off, diag_vals) {
  mean_off <- if (length(off)) mean(off) else 0
  mean_diag <- if (length(diag_vals)) mean(diag_vals) else 0
  if ((mean_off + mean_diag) > 0) mean_off / (mean_off + mean_diag) else 0
}


#' Summarise posterior stability metrics
#'
#' Reports the posterior median and interval of each stability metric.
#' `stability()` returns this summary by default, as the other post-fit
#' summaries of a VAR do. `stability(summary = FALSE)` returns the
#' draws.
#'
#' @param object An object of class `mvgam_stability`
#' @param probs The lower and upper percentiles of the interval
#'   reported with the median
#' @param robust Logical. `TRUE` (the default) reports the median and
#'   median absolute deviation. `FALSE` reports the mean and standard
#'   deviation.
#' @param bins Number of bins that record the shape of each metric for
#'   `plot()`
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A `data.frame` with one row per metric. On a hierarchical
#'   VAR it holds one row per panel and metric, with the panel in
#'   `group`.
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
  draws <- as.data.frame(object)
  metrics <- setdiff(colnames(draws), "group")
  # A metric with an `NA` draw reports `NA`
  summarise_block <- function(d) {
    data.frame(
      metric = metrics,
      mvgam_post_summary(as.matrix(d[, metrics, drop = FALSE]),
                         robust = robust, probs = probs),
      check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
    )
  }
  if (is.null(draws$group)) {
    out <- summarise_block(draws)
    # Reactivity is asked for to learn whether its mass crosses zero,
    # which a median and an interval leave open. The binned draws keep
    # the shape of each metric in a few hundred numbers. `plot()` draws
    # them as histograms.
    attr(out, "bin_counts") <- lapply(
      stats::setNames(metrics, metrics),
      function(v) bin_draws(draws[[v]], bins)
    )
    n_per_block <- nrow(draws)
  } else {
    blocks <- split(draws, draws$group)
    out <- do.call(rbind, lapply(names(blocks), function(g) {
      data.frame(group = factor(g, levels = levels(draws$group)),
                 summarise_block(blocks[[g]]),
                 check.names = FALSE)
    }))
    n_per_block <- nrow(blocks[[1L]])
  }
  attr(out, "ndraws") <- n_per_block
  class(out) <- c("mvgam_stability_summary", class(out))
  out
}


#' Bin a vector of draws for later display
#'
#' A histogram needs the counts and the edges of its bins, a fraction
#' of the size of the draws. A metric that never varies gets one bin
#' of zero width.
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
#' Draws the histogram of each metric from the bins `summary()`
#' recorded. A histogram shows whether the mass of reactivity crosses
#' zero. Pass `intervals = TRUE` for a median and interval instead. A
#' hierarchical VAR draws a median and interval for each panel.
#'
#' @param x An object of class `mvgam_stability_summary`
#' @param variables Metrics to draw
#' @param intervals Logical. `TRUE` draws each metric as a median and
#'   interval. `FALSE` (the default) draws its binned posterior.
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
    return(stability_intervals(x[x$metric %in% keep, , drop = FALSE], keep))
  }
  stability_histogram(counts[keep])
}


#' Plot posterior distributions of latent VAR stability metrics
#'
#' Draws a histogram of the posterior draws of each metric
#' `stability.mvgam()` returned. A dashed line at zero on the
#' reactivity panel marks the threshold above which the system
#' amplifies a shock before it decays. A hierarchical VAR draws a
#' median and interval for each panel.
#'
#' @param x A `mvgam_stability` object returned by [stability()].
#' @param variables Character vector of the metrics to draw. Defaults
#'   to reactivity and the mean and variance return rates. Any column
#'   of `x` can be named.
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
  keep <- resolve_stability_metrics(variables, setdiff(colnames(x), "group"))
  if (!is.null(x$group)) {
    return(plot(summary(x), variables = keep))
  }
  # The red scheme of the `irf()`, `fevd()` and `forecast()` plots,
  # restored when the call returns
  set_color_scheme_local("red")
  stability_histogram(lapply(stats::setNames(keep, keep), function(v) {
    bin_draws(x[[v]], bins)
  }))
}


#' Posterior medians and intervals of stability metrics
#'
#' One panel per metric. A hierarchical VAR's summary puts each panel
#' of the VAR on its own row, in the order of the levels of `gr`.
#'
#' @param dat Rows of a `mvgam_stability_summary` for the metrics drawn
#' @param keep The metrics drawn, in panel order
#' @return A `ggplot` object
#' @noRd
stability_intervals <- function(dat, keep) {
  dat$metric <- factor(dat$metric, levels = keep)
  dat$row <- if (is.null(dat$group)) {
    factor("")
  } else {
    factor(dat$group, levels = rev(levels(dat$group)))
  }
  bounds <- grep("^Q", colnames(dat), value = TRUE)
  ggplot2::ggplot(dat, ggplot2::aes(x = .data$Estimate, y = .data$row)) +
    ggplot2::geom_vline(data = stability_threshold(keep),
                        ggplot2::aes(xintercept = .data$x),
                        linetype = "dashed", colour = "grey30") +
    ggplot2::geom_pointrange(
      ggplot2::aes(xmin = .data[[bounds[1L]]], xmax = .data[[bounds[2L]]]),
      colour = mvgam_colour("mid_highlight")
    ) +
    ggplot2::facet_wrap(
      ~ metric, scales = "free_x", nrow = 1L,
      labeller = ggplot2::as_labeller(stability_metric_labels(keep))
    ) +
    ggplot2::labs(x = "Posterior estimate", y = NULL) +
    mvgam_theme() +
    ggplot2::theme(panel.spacing.x = ggplot2::unit(1.5, "lines"))
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
#' uneven final bin its true width.
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
  ggplot2::ggplot(long, ggplot2::aes(x = .data$mid, y = .data$count)) +
    ggplot2::geom_col(
      ggplot2::aes(width = .data$width),
      fill = mvgam_colour("mid_highlight"),
      colour = "white"
    ) +
    ggplot2::geom_vline(data = stability_threshold(names(counts)),
                        ggplot2::aes(xintercept = .data$x),
                        linetype = "dashed", colour = "grey30") +
    ggplot2::facet_wrap(
      ~ metric, scales = "free", nrow = 1L,
      labeller = ggplot2::as_labeller(stability_metric_labels(names(counts)))
    ) +
    ggplot2::labs(x = "Value", y = "Posterior draws") +
    mvgam_theme()
}


#' The zero line of the reactivity panel
#'
#' Zero is a threshold for reactivity alone. A line at zero on a
#' return rate panel would stretch its axis to a value no draw takes.
#'
#' @param metrics The metrics drawn, in panel order
#' @return A frame of the panel and position of each line
#' @noRd
stability_threshold <- function(metrics) {
  data.frame(
    metric = factor(intersect("reactivity", metrics), levels = metrics),
    x = 0
  )
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
