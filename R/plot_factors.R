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
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A `ggplot` object.
#'
#' @details The factor paths plotted are the identified ones. A
#'   model identified by rotation stores them as
#'   `lv_trend_tilde[t, k]`. Every other factor model stores them as
#'   `lv_trend[t, k]`. See the Identification section of the trend
#'   constructors. Time runs along the training grid. The plot shows
#'   in-sample factor draws only. [active_factors()] reports the
#'   variance each factor dimension carries.
#'
#' @seealso [active_factors()], [residual_cor()], [ordinate()]
#'
#' @author Nicholas J Clark
#' @noRd
plot_factors <- function(
  object,
  probs = c(0.2, 0.4, 0.6, 0.9),
  ...
) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_numeric(
    probs,
    lower = 0, upper = 1, min.len = 1L, any.missing = FALSE
  )
  rlang::check_dots_empty()

  n_lv <- require_factor_n_lv(object, 'plot(type = "factors")')

  per_lv <- extract_lv_trend_matrices(object, n_lv)
  n_time <- ncol(per_lv[[1L]])
  times <- as.numeric(factor_time_values(object, n_time))

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
  ggplot2::ggplot() +
    layers +
    ggplot2::labs(x = "Time", y = "Posterior factor") +
    ggplot2::facet_wrap(~series) +
    mvgam_theme()
}


#' Posterior latent state of a closure-unit fit by series and time
#'
#' Serves `plot.mvgam(type = "latent_state")`. The state is drawn from
#' the covariates alone: abundance `N` for `nmix()`, the probability of
#' occupancy `psi` for `occ()`. Unit columns are grouped by series and
#' time. A
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
  # The state drawn from the covariates alone, unconditioned on the
  # detections, as flocker's `get_Z(history_condition = FALSE)` is.
  state <- posterior_latent_N(object, conditional = FALSE)
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
    ggplot2::labs(x = "Time",
                  y = closure_unit_labels(object$family)$label) +
    ggplot2::facet_wrap(~ series) +
    mvgam_theme()
}


#' The time values of a factor path's rows
#'
#' The recorded time values, which the other mvgam panels show under
#' the same label. A fit without recorded times, or whose factor grid
#' has another length, takes the positions 1 to `n_time`.
#'
#' @param object Fitted `mvgam` factor model
#' @param n_time Integer number of rows of the factor path
#' @return Vector of length `n_time`
#' @noRd
factor_time_values <- function(object, n_time) {
  recorded <- mvgam_axes(object)$time$values
  if (length(recorded) == n_time) recorded else seq_len(n_time)
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
  meta <- collect_lv_trend_column_names(fit_variables(object$fit), n_lv)
  draws_mat <- fit_draws(object$fit, c(meta$param_name, "b_trend_tilde"))
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
