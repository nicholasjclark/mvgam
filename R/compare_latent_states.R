# Compare the latent state posterior across two or more fits, with
# an optional truth vector for simulation studies. Useful for
# multi-method monitoring (one shared latent process estimated
# under different observation subsets), for ablations (joint vs
# baseline univariate fits), and for ground-truth recovery checks.


#' Compare latent state posteriors across mvgam fits
#'
#' Extracts the per-draw posterior of the latent state variable
#' `trend[t]` from each supplied mvgam fit, optionally aligns each
#' fit's sign to a known truth vector (latent factor models are
#' identified only up to overall sign), and returns either an
#' overlaid trajectory plot or a posterior-SD-over-time plot. All
#' fits must have the same trend length; mismatched fits are NA
#' padded at the tail so the comparison stays meaningful.
#'
#' This is the natural helper for simulation studies that compare
#' recovery across model variants, and for empirical workflows
#' that compare a joint multi-method fit to its single-method
#' baselines.
#'
#' @param fits Named list of fitted [mvgam][mvgam::mvgam] objects.
#'   The list names become the legend / facet labels.
#' @param truth Optional numeric vector of true latent state values
#'   (one entry per training time point). When supplied, each fit's
#'   sign is aligned to maximise correlation with `truth`, and the
#'   `"trajectory"` plot overlays a dashed black line of the truth.
#'   Defaults to `NULL`.
#' @param type One of `"trajectory"` (per-fit posterior median +
#'   95 percent ribbon, faceted by fit) or `"sd"` (per-timepoint
#'   posterior SD, one line per fit). Defaults to `"trajectory"`.
#' @return A `ggplot` object.
#'
#' @author Nicholas J Clark
#'
#' @seealso [hindcast.mvgam()], [posterior_predict.mvgam()].
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 60L, trend_model = AR())
#' fit_a <- mvgam(y ~ 1, trend_formula = ~ AR(p = 1),
#'                family = poisson(), data = simdat$data_train,
#'                chains = 2L, samples = 500L, silent = 2L)
#' fit_b <- mvgam(y ~ 1, trend_formula = ~ RW(),
#'                family = poisson(), data = simdat$data_train,
#'                chains = 2L, samples = 500L, silent = 2L)
#' compare_latent_states(list(AR = fit_a, RW = fit_b))
#' }
#' @export
compare_latent_states <- function(fits,
                                    truth = NULL,
                                    type = c("trajectory", "sd")) {
  checkmate::assert_list(fits, min.len = 1L, names = "named",
                          types = "mvgam")
  checkmate::assert_numeric(truth, null.ok = TRUE)
  type <- match.arg(type)

  state_mats <- lapply(fits, extract_trend_posterior)
  T_max <- max(vapply(state_mats, ncol, integer(1L)))
  state_mats <- lapply(state_mats, function(m) {
    if (ncol(m) < T_max) {
      pad <- matrix(NA_real_, nrow = nrow(m),
                     ncol = T_max - ncol(m))
      cbind(m, pad)
    } else {
      m
    }
  })

  if (!is.null(truth)) {
    state_mats <- lapply(state_mats, function(m) {
      med <- apply(m, 2L, stats::median, na.rm = TRUE)
      tlen <- min(length(truth), length(med))
      r <- suppressWarnings(stats::cor(
        med[seq_len(tlen)], truth[seq_len(tlen)],
        use = "complete.obs"
      ))
      if (!is.na(r) && r < 0) -m else m
    })
  }

  long <- do.call(rbind, lapply(seq_along(state_mats), function(i) {
    m <- state_mats[[i]]
    data.frame(
      fit       = names(fits)[i],
      time      = seq_len(ncol(m)),
      median    = apply(m, 2L, stats::median, na.rm = TRUE),
      sd        = apply(m, 2L, stats::sd, na.rm = TRUE),
      lower_95  = apply(m, 2L, stats::quantile, 0.025,
                         na.rm = TRUE),
      upper_95  = apply(m, 2L, stats::quantile, 0.975,
                         na.rm = TRUE)
    )
  }))
  long$fit <- factor(long$fit, levels = names(fits))

  pal <- mvgam_categorical_palette(length(fits))

  if (identical(type, "sd")) {
    p <- ggplot2::ggplot(long, ggplot2::aes(
      x = .data$time, y = .data$sd,
      colour = .data$fit, group = .data$fit
    )) +
      ggplot2::geom_line(linewidth = 0.7) +
      ggplot2::scale_colour_manual(values = pal, name = "Fit") +
      mvgam_theme() +
      ggplot2::labs(
        x = "Time", y = "Posterior SD of latent state"
      )
    return(p)
  }

  # type == "trajectory"
  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$time)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$lower_95, ymax = .data$upper_95,
                    fill = .data$fit),
      alpha = 0.25
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$median, colour = .data$fit),
      linewidth = 0.7
    ) +
    ggplot2::facet_wrap(~ fit, ncol = 2L) +
    ggplot2::scale_colour_manual(values = pal, guide = "none") +
    ggplot2::scale_fill_manual(values = pal, guide = "none") +
    mvgam_theme() +
    ggplot2::labs(
      x = "Time", y = "Latent state"
    )
  if (!is.null(truth)) {
    truth_df <- data.frame(time = seq_along(truth), truth = truth)
    p <- p +
      ggplot2::geom_line(
        data = truth_df,
        ggplot2::aes(x = .data$time, y = .data$truth),
        inherit.aes = FALSE,
        colour = "black", linewidth = 0.55, linetype = "dashed"
      )
  }
  p
}


# Extract `trend[t]` posterior matrix `[ndraws x T]` from a fit.
# Multi-response fits share one latent state; this returns the
# single-series matrix (all per-response trend columns are
# identical in the joint case).
#'@noRd
extract_trend_posterior <- function(fit) {
  ps <- posterior::as_draws_matrix(fit$fit)
  cols <- grep("^trend\\[", colnames(ps), value = TRUE)
  if (length(cols) == 0L) {
    stop(insight::format_error(c(
      "Fit has no posterior draws for `trend[]`.",
      i = "compare_latent_states() requires a fit with a latent trend; trendless fits are not supported."
    )), call. = FALSE)
  }
  ps[, cols, drop = FALSE]
}
