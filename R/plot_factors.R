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
#'   the draw-level shares, a credible interval on the share
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
#' @details Factor paths are read from the Stan posterior,
#'   preferring `lv_trend_tilde[t, k]` (the rotated factor
#'   paths from free-Z factor models, in the identified
#'   `Z_tilde` basis) and falling back to `lv_trend[t, k]` for
#'   partial-Z fits where the user-supplied loading pattern is
#'   preserved without rotation. Time runs along the training
#'   grid only; out-of-sample factor draws are not displayed.
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


# Per-(series, time) latent-state ribbon for a closure-unit
# `mvgam` fit. Calls the family-registered `latent_state` method
# (returns `[ndraws x N_unit]`), groups unit columns by
# `(series, time)`, pools across sites for multi-season fits, and
# emits a ribbon-and-median plot faceted by series. Single entry
# point used by `plot.mvgam(type = "latent_state")` for both
# `nmix()` (marginal latent `N`) and `occ()` (marginal occupancy
# `psi`).
#
# @noRd
plot_latent_state <- function(object, probs = c(0.5, 0.8, 0.95),
                                ndraws = NULL, ...) {
  if (!is_closure_unit_family(object$family)) {
    stop(insight::format_error(c(
      "plot(type = 'latent_state') requires a closure-unit family.",
      i = paste0(
        "Use family = nmix() or family = occ() to fit a model with ",
        "a latent abundance or occupancy state."
      )
    )))
  }
  state_fn <- dispatch_closure_unit_method(object$family, "latent_state")
  if (is.null(state_fn)) {
    stop(insight::format_error(c(
      paste0(
        "Family '", resolve_family_name(object$family),
        "' does not expose a latent-state surface."
      ),
      i = paste0(
        "Multi-response closure-unit families (mvn / mvt / diri / ",
        "multi / categ) have no per-unit latent state."
      )
    )))
  }
  # Marginal latent state (no conditioning on observed detections);
  # this is the surface comparable to flocker's `get_Z(history_",
  # "condition = FALSE)` and spOccupancy's `psi.samples`.
  state <- if ("conditional" %in% names(formals(state_fn))) {
    state_fn(object, conditional = FALSE)
  } else {
    state_fn(object)
  }
  checkmate::assert_matrix(state)

  # Per-unit labels come off the unit grid the arrays carry, one row
  # per unit in unit order, rather than from the frame's own columns:
  # the grid holds the values the grouping was built on, so a fit
  # whose closure unit is keyed by something other than `series` and
  # `time` is labelled by what it was actually grouped on. The
  # grouping runs series-first and time-last, and a multi-season fit
  # carries a `site` axis between them that the ribbon pools over.
  data <- object$data
  arrays <- closure_unit_arrays_for(object, data)
  unit_vars <- arrays$unit_vars
  unit_meta <- data.frame(
    series = as.factor(arrays$unit_grid[[unit_vars[1L]]]),
    time   = arrays$unit_grid[[unit_vars[length(unit_vars)]]]
  )
  if (ncol(state) != nrow(unit_meta)) {
    stop(insight::format_error(c(
      "Latent-state matrix column count does not match closure units.",
      x = paste0(
        "ncol(state) = ", ncol(state),
        ", N_unit = ", nrow(unit_meta), "."
      )
    )))
  }

  series_levels <- levels(unit_meta$series)
  time_levels <- sort(unique(unit_meta$time))
  y_label <- if (resolve_family_name(object$family) == "occ") {
    "Posterior occupancy (psi)"
  } else {
    "Posterior latent N"
  }

  set_color_scheme_local("red")
  layers <- list()
  for (sp in series_levels) {
    # Collect per-time pooled posterior draws for this species.
    # Each (series, time) cell pools `ndraws * n_sites` values, so
    # quantile bands reflect cross-site and posterior uncertainty
    # jointly.
    sp_idx <- unit_meta$series == sp
    if (!any(sp_idx)) next
    pooled <- vapply(time_levels, function(tt) {
      cols <- which(sp_idx & unit_meta$time == tt)
      as.numeric(state[, cols, drop = FALSE])
    }, FUN.VALUE = numeric(nrow(state) *
                            max(1L, sum(sp_idx & unit_meta$time ==
                                          time_levels[1L]))))
    # Per-time draw pool. `pooled` is `[ndraws * n_sites_t, n_time]`.
    layers <- c(
      layers,
      mvgam_band_layer(pooled, time_levels, probs = probs,
                        group = sp),
      list(mvgam_median_layer(pooled, time_levels, group = sp))
    )
  }
  ggplot2::ggplot() +
    layers +
    ggplot2::labs(x = "Time", y = y_label) +
    ggplot2::facet_wrap(~ series) +
    mvgam_theme()
}


# Internal: shared column-name lookup for tilde-aware lv_trend
# extraction. Returns a `[n_time, n_lv]` character matrix of
# `lv_trend[t, k]` (or `lv_trend_tilde[t, k]`) column names ordered
# `[time, factor]`, plus the resolved parameter name. Used by
# `extract_lv_trend_matrices()`, which builds a per-factor list of
# `[draws, n_time]` matrices for plotting. Pattern selection
# delegates to `factor_state_param_pattern()` so Z_tilde / Z fits
# stay aligned in lockstep.
#'@noRd
collect_lv_trend_column_names <- function(par_names, n_lv) {
  pattern <- factor_state_param_pattern(par_names)
  param_name <- if (pattern == "^lv_trend_tilde\\[") {
    "lv_trend_tilde"
  } else {
    "lv_trend"
  }
  lv_cols <- grep(pattern, par_names, value = TRUE)
  if (length(lv_cols) == 0L) {
    stop(insight::format_error(c(
      "Could not locate factor-path draws in posterior.",
      i = paste0(
        "Expected an LV-factor model with `", param_name,
        "[t, k]` stored in the posterior."
      )
    )))
  }
  parts <- regmatches(
    lv_cols, regexec("\\[(\\d+),(\\d+)\\]", lv_cols)
  )
  idx <- do.call(rbind, lapply(parts, function(p) {
    as.integer(p[2:3])
  }))
  t_pos <- idx[, 1L]
  k_pos <- idx[, 2L]
  if (max(k_pos) != n_lv) {
    stop(insight::format_error(c(
      "Mismatch between expected and stored factor count.",
      x = paste0(
        "Expected n_lv = ", n_lv,
        ", max factor index in posterior = ", max(k_pos), "."
      )
    )))
  }
  cols_by_tk <- matrix(NA_character_, nrow = max(t_pos), ncol = n_lv)
  for (k in seq_len(n_lv)) {
    keep <- k_pos == k
    cols_by_tk[, k] <- lv_cols[keep][order(t_pos[keep])]
  }
  list(cols_by_tk = cols_by_tk, param_name = param_name)
}


# Internal: extract per-factor (ndraws x n_time) matrices of
# factor paths from the Stan posterior. Returns a named list of
# length `n_lv`, names "Factor 1" ... "Factor n_lv". Column
# resolution comes from `collect_lv_trend_column_names()`.
#'@noRd
extract_lv_trend_matrices <- function(object, n_lv) {
  draws_mat <- posterior::as_draws_matrix(object$fit)
  meta <- collect_lv_trend_column_names(colnames(draws_mat), n_lv)
  setNames(
    lapply(seq_len(n_lv), function(k) {
      draws_mat[, meta$cols_by_tk[, k], drop = FALSE]
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
# fit has neither sampled loading columns (`Z_tilde[i, j]` or
# `Z[i, j]`) nor a fixed Z stashed on `trend_metadata$fixed_Z`
# (e.g. non-factor trend types). Delegates to
# `resolve_factor_loadings()` so the fixed-vs-sampled decision
# lives in exactly one place.
#'@noRd
extract_factor_loadings_array <- function(object, n_lv) {
  if (!is.null(object$trend_metadata$fixed_Z) &&
      !anyNA(object$trend_metadata$fixed_Z)) {
    return(resolve_factor_loadings(object = object, n_lv = n_lv))
  }
  draws_mat <- posterior::as_draws_matrix(object$fit)
  pattern <- factor_loading_param_pattern(colnames(draws_mat))
  z_cols <- grep(pattern, colnames(draws_mat), value = TRUE)
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
