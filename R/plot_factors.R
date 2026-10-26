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
#'   [AR()]/[VAR()]/[RW()]/[LLT()]/[ZMVN()] trend constructor.
#' @param probs Numeric vector of credible interval widths used
#'   for the ribbon. Defaults to four nested bands at 20 / 40 /
#'   60 / 90% credible intervals.
#' @param label_contribution Logical. When `TRUE` (the default),
#'   each facet strip shows the factor's contribution share as
#'   a percentage. Set `FALSE` to fall back to bare `"Factor k"`
#'   strip labels.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A `ggplot` object. The returned object carries a
#'   `contribution` attribute: a `data.frame` with columns
#'   `Factor`, `PathVarShare`, `Lower` and `Upper`.
#'   `PathVarShare` is the factor's share of the total
#'   loading-weighted posterior path variance. For each draw,
#'   factor k's raw score is `||Z[, k]||^2 * Var(lv_trend[, k])`,
#'   where `Z[, k]` is the loadings vector that maps factor k onto the
#'   series. The draw-level shares are averaged across draws.
#'   `Lower` / `Upper` are the 2.5th and 97.5th percentiles of
#'   the draw-level shares, a credible interval on the share
#'   itself.
#'
#'   The metric weights each factor's variance by how strongly
#'   it loads onto the series: a factor with wide posterior
#'   excursions but near-zero loadings contributes near-zero,
#'   matching the standard communality decomposition for factor
#'   models. Shares sum to 1 across factors in each draw. Their
#'   marginal means also sum to 1.
#'
#'   This is a descriptive within-fit variance-partition summary,
#'   not a formal hypothesis test; contributions are not
#'   comparable across models fitted on different time spans.
#'   For an independent check on whether the shrinkage prior
#'   pushed `sigma_trend[k]` itself to zero, inspect the raw
#'   posterior of `sigma_trend[k]` via [as_draws_df.mvgam()].
#'
#' @details The factor paths plotted are the identified ones. A
#'   model identified by rotation stores them as
#'   `lv_trend_tilde[t, k]`. Every other factor model stores them as
#'   `lv_trend[t, k]`. See the Identification section of the trend
#'   constructors. Time runs along the training grid. The plot shows
#'   in-sample factor draws only.
#'
#' @seealso [as_draws_df.mvgam()], [residual_cor()], [ordinate()]
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
  rlang::check_dots_empty()

  n_lv <- require_factor_n_lv(object, 'plot(type = "factors")')

  per_lv <- extract_lv_trend_matrices(object, n_lv)
  n_time <- ncol(per_lv[[1L]])
  # The x axis shows the recorded time values. The other mvgam panels
  # use the same values under the same "Time" label. A fit without
  # recorded times, or whose factor grid has another length, plots
  # the time positions 1 to n_time.
  recorded <- mvgam_axes(object)$time$values
  times <- if (length(recorded) == n_time) {
    as.numeric(recorded)
  } else {
    seq_len(n_time)
  }
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

  set_color_scheme_local()
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


#' Posterior latent state of a closure-unit fit by series and time
#'
#' Serves `plot.mvgam(type = "latent_state")` for `nmix()` (marginal
#' latent `N`) and `occ()` (marginal occupancy `psi`). The family's
#' registered `latent_state` method returns a `[ndraws, N_unit]`
#' matrix. Unit columns are grouped by series and time. A
#' multi-season fit pools the draws over sites. The plot shows a
#' ribbon and median line in one facet per series.
#'
#' @param object Fitted `mvgam` object with a closure-unit family
#' @param probs Numeric vector of credible interval widths for the
#'   ribbon
#' @param ndraws Integer number of draws, or `NULL` for every draw
#' @param ... Unused. Anything passed here is refused.
#' @return A `ggplot` object
#' @noRd
plot_latent_state <- function(object, probs = c(0.5, 0.8, 0.95),
                                ndraws = NULL, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_numeric(probs, lower = 0, upper = 1, min.len = 1L,
                            any.missing = FALSE)
  checkmate::assert_count(ndraws, positive = TRUE, null.ok = TRUE)
  rlang::check_dots_empty()
  # Some closure-unit families have no latent state, such as `mvn()`,
  # `mvt()`, `diri()`, `multi()` and `categ()`. The family registry
  # records which families expose one.
  require_closure_unit_predict_type(object$family, "latent_state")
  state_fn <- dispatch_closure_unit_method(object$family, "latent_state")
  # Marginal latent state, unconditioned on the observed detections.
  # It matches flocker's `get_Z(history_condition = FALSE)` and
  # spOccupancy's `psi.samples`.
  state <- if ("conditional" %in% names(formals(state_fn))) {
    state_fn(object, conditional = FALSE)
  } else {
    state_fn(object)
  }
  checkmate::assert_matrix(state)
  state <- subset_draws_rows(state, ndraws = ndraws)

  # Per-unit labels come from the unit grid of the closure-unit
  # arrays, one row per unit in unit order. The grid stores the
  # grouping values. A fit whose closure unit uses columns other than
  # `series` and `time` is labelled by its own grouping columns. The
  # grouping lists series first and time last. A multi-season fit
  # adds a `site` axis between them.
  data <- object$data
  arrays <- closure_unit_arrays_for(object, data)
  unit_vars <- arrays$unit_vars
  unit_meta <- data.frame(
    series = as.factor(arrays$unit_grid[[unit_vars[1L]]]),
    time = arrays$unit_grid[[unit_vars[length(unit_vars)]]]
  )
  if (ncol(state) != nrow(unit_meta)) {
    stop_shape_fault("The latent-state matrix", ncol(state),
                     nrow(unit_meta))
  }

  y_label <- if (models_latent_count(object$family)) {
    "Posterior latent N"
  } else {
    "Posterior occupancy (psi)"
  }

  # The band helpers pool the columns that share a time. A
  # multi-season fit has one column per site at each time. Its bands
  # cover site and posterior uncertainty together.
  set_color_scheme_local()
  series_levels <- levels(unit_meta$series)
  layers <- list()
  for (sp in series_levels) {
    cols <- which(unit_meta$series == sp)
    if (!length(cols)) next
    panel <- factor(sp, levels = series_levels)
    layers <- c(
      layers,
      mvgam_band_layer(state[, cols, drop = FALSE], unit_meta$time[cols],
                       probs = probs, group = panel),
      list(mvgam_median_layer(state[, cols, drop = FALSE],
                              unit_meta$time[cols], group = panel))
    )
  }
  ggplot2::ggplot() +
    layers +
    ggplot2::labs(x = "Time", y = y_label) +
    ggplot2::facet_wrap(~ series) +
    mvgam_theme()
}


#' The draw column names of each latent factor path
#'
#' `factor_state_param_pattern()` selects `lv_trend_tilde` or
#' `lv_trend`, keeping the states aligned with the loadings.
#'
#' @param par_names Character vector of draw column names
#' @param n_lv Integer number of latent factors
#' @return List holding the resolved `param_name` and `cols_by_tk`, a
#'   character matrix of column names with dimensions `[n_time, n_lv]`
#' @noRd
collect_lv_trend_column_names <- function(par_names, n_lv) {
  checkmate::assert_character(par_names)
  checkmate::assert_int(n_lv, lower = 1L)
  pattern <- factor_state_param_pattern(par_names)
  param_name <- if (pattern == "^lv_trend_tilde\\[") {
    "lv_trend_tilde"
  } else {
    "lv_trend"
  }
  lv_cols <- grep(pattern, par_names, value = TRUE)
  if (length(lv_cols) == 0L) {
    stop_missing_draws(param_name, paste0(param_name, "[t, k]"))
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
    stop_shape_fault(paste0("The '", param_name, "' factor count"),
                     max(k_pos), n_lv)
  }
  cols_by_tk <- matrix(NA_character_, nrow = max(t_pos), ncol = n_lv)
  for (k in seq_len(n_lv)) {
    keep <- k_pos == k
    cols_by_tk[, k] <- lv_cols[keep][order(t_pos[keep])]
  }
  list(cols_by_tk = cols_by_tk, param_name = param_name)
}


#' The posterior draws of each latent factor path
#'
#' Slopes of `x:lv_axis()` terms move each factor before the loadings
#' apply. A factor's path is then its state plus its predictor, which
#' the identified slopes `b_trend_tilde` give in the basis of
#' `Z_tilde`.
#'
#' @param object Fitted `mvgam` factor model
#' @param n_lv Integer number of latent factors
#' @return List of `n_lv` draws matrices with dimensions
#'   `[ndraws, n_time]`, named `"Factor 1"` onward
#' @noRd
extract_lv_trend_matrices <- function(object, n_lv) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_int(n_lv, lower = 1L)
  draws_mat <- posterior::as_draws_matrix(object$fit)
  meta <- collect_lv_trend_column_names(colnames(draws_mat), n_lv)
  X <- object$standata$X_trend
  b_cols <- paste0("b_trend_tilde[", seq_len(NCOL(X)), "]")
  mu <- if (all(b_cols %in% colnames(draws_mat))) {
    unclass(draws_mat[, b_cols, drop = FALSE]) %*% t(X)
  }
  setNames(
    lapply(seq_len(n_lv), function(k) {
      path <- draws_mat[, meta$cols_by_tk[, k], drop = FALSE]
      if (is.null(mu)) path else {
        path + mu[, object$standata$times_trend[, k], drop = FALSE]
      }
    }),
    paste0("Factor ", seq_len(n_lv))
  )
}


#' Each factor's share of the loading-weighted path variance
#'
#' For draw `d` and factor `k` the raw contribution is
#' `||Z[d, , k]||^2 * Var(lv_trend[d, , k])`. Each draw's shares are
#' the raw contributions divided by their sum. A `NULL` loading array,
#' as in a fixed-loadings fit, gives every factor unit-norm loadings.
#' The shares then partition the path variance alone.
#'
#' @param per_lv List of factor path draws matrices from
#'   `extract_lv_trend_matrices()`
#' @param Z_arr Numeric loading array with dimensions
#'   `[ndraws, n_series, n_lv]`, or `NULL`
#' @return Data frame of the posterior mean share and its 95% interval
#'   for each factor, sorted by decreasing share
#' @noRd
lv_contribution_table <- function(per_lv, Z_arr = NULL) {
  checkmate::assert_list(per_lv, types = "matrix", min.len = 1L)
  checkmate::assert_array(Z_arr, d = 3L, null.ok = TRUE)
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


#' The factor loading draws of a fit for plotting
#'
#' `resolve_factor_loadings()` decides between fixed and sampled
#' loadings.
#'
#' @param object Fitted `mvgam` factor model
#' @param n_lv Integer number of latent factors
#' @return Numeric array with dimensions `[ndraws, n_series, n_lv]`, or
#'   `NULL` when the fit has neither sampled loadings nor a fixed `Z`
#' @noRd
extract_factor_loadings_array <- function(object, n_lv) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_int(n_lv, lower = 1L)
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
