#' Plot latent dynamic factors from an LV \pkg{mvgam} model
#'
#' Posterior summary plot of the latent dynamic factors `lv_trend`
#' for a fitted `mvgam` object that used latent variables (e.g.
#' `trend_formula = ~ AR(n_lv = 2)`). One panel per factor with a
#' multi-band ribbon and a median line, drawn via the shared
#' `mvgam` plotting palette.
#'
#' @param object A fitted `mvgam` object that included latent
#'   dynamic factors via the `n_lv` argument of an
#'   [AR()]/[VAR()]/[RW()]/[ZMVN()] trend constructor.
#' @param probs Numeric vector of credible interval widths used
#'   for the ribbon. Defaults to four nested bands at 20 / 40 /
#'   60 / 90% credible intervals.
#' @param label_contribution Logical. When `TRUE` (the default),
#'   each facet strip shows the factor's contribution share as
#'   a percentage. Set `FALSE` to fall back to bare `"Factor k"`
#'   strip labels.
#' @param ... Currently unused; reserved for future arguments.
#'
#' @return A `ggplot` object. The returned object carries a
#'   `contribution` attribute: a `data.frame` with columns
#'   `Factor`, `PathVarShare`, `Lower`, and `Upper`.
#'   `PathVarShare` is the factor's share of the total
#'   loading-weighted posterior path variance: for each draw,
#'   factor k's raw score is
#'   `||Z[, k]||^2 * Var(lv_trend[, k])` where `Z[, k]` is the
#'   loadings vector that maps factor k onto the series, and the
#'   draw-level shares are then averaged across draws.
#'   `Lower` / `Upper` are the 2.5th and 97.5th percentiles of
#'   the draw-level shares — a credible interval on the share
#'   itself.
#'
#'   The metric weights each factor's variance by how strongly
#'   it loads onto the series: a factor with wide posterior
#'   excursions but near-zero loadings contributes near-zero,
#'   matching the standard communality decomposition for factor
#'   models. Shares sum to 1 across factors per draw, so the
#'   marginal means sum to 1 as well.
#'
#'   This is a descriptive within-fit variance-partition summary,
#'   not a formal hypothesis test; contributions are not
#'   comparable across models fitted on different time spans.
#'   For an independent check on whether the shrinkage prior
#'   pushed `sigma_trend[k]` itself to zero, inspect the raw
#'   posterior of `sigma_trend[k]` via [as_draws_df.mvgam()].
#'
#' @details Factor paths are read from the Stan posterior under
#'   the name `lv_trend[t, k]`, which is populated in the
#'   generated-quantities block for any LV trend. Time runs
#'   along the training grid only; out-of-sample factor draws
#'   are not displayed.
#'
#' @seealso [as_draws_df.mvgam()], [residual_cor()],
#'   [ordinate.jsdgam()]
#'
#' @author Nicholas J Clark
#' @noRd
plot_factors <- function(
  object,
  probs = c(0.2, 0.4, 0.6, 0.9),
  label_contribution = TRUE,
  ...
) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_numeric(
    probs,
    lower = 0, upper = 1, min.len = 1L, any.missing = FALSE
  )
  checkmate::assert_flag(label_contribution)

  n_lv <- detect_factor_n_lv(object)
  if (is.null(n_lv)) {
    stop(insight::format_error(c(
      "Object does not contain any latent dynamic factors.",
      i = paste0(
        "Latent factor plots require an LV trend ",
        "(e.g. `trend_formula = ~ AR(n_lv = 2)`)."
      )
    )))
  }

  per_lv <- extract_lv_trend_matrices(object, n_lv)
  n_time <- ncol(per_lv[[1L]])
  times <- seq_len(n_time)
  Z_arr <- extract_factor_loadings_array(object, n_lv)
  contrib <- lv_contribution_table(per_lv, Z_arr = Z_arr)

  facet_labels <- if (label_contribution) {
    pct <- round(100 * contrib$PathVarShare, 1)
    stats::setNames(
      paste0(contrib$Factor, " (", pct, "%)"),
      contrib$Factor
    )
  } else {
    stats::setNames(contrib$Factor, contrib$Factor)
  }

  set_color_scheme_local("red")
  layers <- list()
  for (k in seq_len(n_lv)) {
    label <- paste0("Factor ", k)
    layers <- c(
      layers,
      mvgam_band_layer(
        per_lv[[k]], times, probs = probs, group = label
      ),
      list(mvgam_median_layer(per_lv[[k]], times, group = label))
    )
  }
  p <- ggplot2::ggplot() +
    layers +
    ggplot2::labs(x = "Time", y = "Posterior factor") +
    ggplot2::facet_wrap(
      ~series,
      labeller = ggplot2::as_labeller(facet_labels)
    ) +
    mvgam_theme()
  attr(p, "contribution") <- contrib
  p
}


# Internal: extract per-factor (ndraws x n_time) matrices of
# `lv_trend[t, k]` from the Stan posterior. Returns a list of
# length `n_lv`, names "Factor 1" ... "Factor n_lv".
#'@noRd
extract_lv_trend_matrices <- function(object, n_lv) {
  draws_mat <- posterior::as_draws_matrix(object$fit)
  lv_cols <- grep(
    "^lv_trend\\[", colnames(draws_mat), value = TRUE
  )
  if (length(lv_cols) == 0L) {
    stop(insight::format_error(c(
      "Could not locate 'lv_trend' draws in object$fit.",
      i = paste0(
        "Expected an LV-factor model with `lv_trend[t, k]` ",
        "stored in the generated-quantities block."
      )
    )))
  }
  parts <- regmatches(
    lv_cols, regexec("\\[(\\d+),(\\d+)\\]", lv_cols)
  )
  idx <- do.call(rbind, lapply(parts, function(p) {
    as.integer(p[2:3])
  }))
  t_idx <- idx[, 1L]
  lv_idx <- idx[, 2L]
  if (max(lv_idx) != n_lv) {
    stop(insight::format_error(c(
      "Mismatch between detected and stored factor count.",
      x = paste0(
        "detect_factor_n_lv() = ", n_lv,
        ", max lv index in posterior = ", max(lv_idx), "."
      )
    )))
  }
  setNames(
    lapply(seq_len(n_lv), function(k) {
      keep <- lv_idx == k
      cols <- lv_cols[keep][order(t_idx[keep])]
      draws_mat[, cols, drop = FALSE]
    }),
    paste0("Factor ", seq_len(n_lv))
  )
}


# Internal: per-factor contribution as a loading-weighted
# variance partition. For each draw d and factor k:
#   raw_dk = ||Z[d, , k]||^2 * Var(lv_trend[d, , k])
# Per-draw shares share_dk = raw_dk / sum_j(raw_dj). Returns the
# posterior mean of share_dk plus 95% credible bounds. When
# `Z_arr` is NULL (e.g. fixed-loadings models where Z is not in
# the posterior) all loadings are treated as unit-norm, which
# reduces to the bare path-variance partition.
#'@noRd
lv_contribution_table <- function(per_lv, Z_arr = NULL) {
  n_lv <- length(per_lv)
  ndraws <- nrow(per_lv[[1L]])
  raw_dk <- matrix(NA_real_, nrow = ndraws, ncol = n_lv)
  for (k in seq_len(n_lv)) {
    var_d <- apply(per_lv[[k]], 1L, stats::var, na.rm = TRUE)
    norm_sq_d <- if (is.null(Z_arr)) {
      rep(1, ndraws)
    } else {
      rowSums(Z_arr[, , k, drop = FALSE]^2)
    }
    raw_dk[, k] <- norm_sq_d * var_d
  }
  totals <- rowSums(raw_dk, na.rm = TRUE)
  shares_dk <- raw_dk / pmax(totals, .Machine$double.eps)
  out <- data.frame(
    Factor = names(per_lv),
    PathVarShare = colMeans(shares_dk, na.rm = TRUE),
    Lower = apply(
      shares_dk, 2L, stats::quantile,
      probs = 0.025, na.rm = TRUE, names = FALSE
    ),
    Upper = apply(
      shares_dk, 2L, stats::quantile,
      probs = 0.975, na.rm = TRUE, names = FALSE
    )
  )
  out[order(out$PathVarShare, decreasing = TRUE), , drop = FALSE]
}


# Internal: extract per-draw factor loadings array
# `[ndraws, n_series, n_lv]` for plotting. Returns NULL when the
# fit has neither sampled Z[i, j] columns nor a fixed Z stashed
# on `trend_metadata$fixed_Z` (e.g. non-factor trend types).
# Delegates to `resolve_factor_loadings()` so the fixed-vs-
# sampled decision lives in exactly one place.
#'@noRd
extract_factor_loadings_array <- function(object, n_lv) {
  if (!is.null(object$trend_metadata$fixed_Z)) {
    return(resolve_factor_loadings(object = object, n_lv = n_lv))
  }
  draws_mat <- posterior::as_draws_matrix(object$fit)
  z_cols <- grep("^Z\\[", colnames(draws_mat), value = TRUE)
  if (length(z_cols) == 0L) {
    return(NULL)
  }
  parts <- regmatches(z_cols, regexec("\\[(\\d+),(\\d+)\\]", z_cols))
  idx <- do.call(rbind, lapply(parts, function(p) {
    as.integer(p[2:3])
  }))
  n_series <- max(idx[, 1L])
  resolve_factor_loadings(
    draws_mat = draws_mat, n_series = n_series, n_lv = n_lv
  )
}
