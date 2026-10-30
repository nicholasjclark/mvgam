#' An approximate Gaussian process at the prediction rows
#'
#' brms writes the Hilbert space approximation as
#' `(sqrt(spd_gp(slambda, sdgp, lscale)) .* zgp) * Xgp'`.
#'
#' @param Xgp Matrix `[N, k]` of the basis functions at the rows
#' @param slambda Array `[k, D]` or `[k, D, 1]` of the square roots of
#'   the basis eigenvalues
#' @param zgp Matrix `[ndraws, k]` of standard normal coefficients
#' @param sdgp Vector `[ndraws]` of marginal SDs
#' @param lscale Matrix `[ndraws, 1]` of an isotropic GP's length scale,
#'   or `[ndraws, D]` of an anisotropic GP's
#' @param kernel `"exp_quad"`, `"matern32"` or `"matern52"`
#' @return Matrix `[ndraws, N]` of the GP's contribution
#' @noRd
approx_gp_pred <- function(Xgp, slambda, zgp, sdgp, lscale, kernel) {
  checkmate::assert_matrix(Xgp, any.missing = FALSE)
  checkmate::assert_matrix(zgp, any.missing = FALSE)
  if (ncol(zgp) != ncol(Xgp)) {
    stop_shape_fault("The GP coefficients 'zgp'", ncol(zgp), ncol(Xgp))
  }
  if (length(sdgp) != nrow(zgp)) {
    stop_shape_fault("The GP scale 'sdgp'", length(sdgp), nrow(zgp))
  }
  spd <- gp_spectral_density(slambda, sdgp, lscale, kernel)
  (sqrt(spd) * zgp) %*% t(Xgp)
}


#' The spectral density of a GP kernel at each basis function
#'
#' The densities brms writes in Stan as `spd_gp_exp_quad()`,
#' `spd_gp_matern32()` and `spd_gp_matern52()`. With `D` dimensions,
#' length scales `l` and `q = sum(l^2 * slambda^2)`, each is
#' `sdgp^2 * prod(l)` times
#' - `sqrt(2 pi)^D * exp(-q / 2)` for `exp_quad`,
#' - `2^D pi^(D/2) gamma((D + n) / 2) n^(n/2) / gamma(n / 2) *
#'   (n + q)^(-(D + n) / 2)` for a Matern kernel of smoothness `n / 2`,
#'   with `n = 3` for `matern32` and `n = 5` for `matern52`.
#'
#' brms counts the columns of `lscale` to tell the forms apart. An
#' isotropic GP's one length scale serves every dimension.
#'
#' @inheritParams approx_gp_pred
#' @return Matrix `[ndraws, k]` of spectral densities
#' @noRd
gp_spectral_density <- function(slambda, sdgp, lscale, kernel) {
  checkmate::assert_array(slambda, min.d = 2, max.d = 3, any.missing = FALSE)
  checkmate::assert_numeric(sdgp, any.missing = FALSE, min.len = 1)
  checkmate::assert_matrix(lscale, any.missing = FALSE)
  checkmate::assert_choice(kernel, c("exp_quad", "matern32", "matern52"))
  slambda <- matrix(slambda, nrow = dim(slambda)[1L])
  d <- ncol(slambda)
  if (nrow(lscale) != length(sdgp)) {
    stop_shape_fault("The GP length scale", nrow(lscale), length(sdgp))
  }
  if (!ncol(lscale) %in% c(1L, d)) {
    stop_mvgam_fault(
      "A GP length scale needs 1 column or one per dimension.",
      paste0("Got ", ncol(lscale), " columns for ", d, " dimensions.")
    )
  }
  l <- matrix(lscale, nrow = length(sdgp), ncol = d)
  q <- l^2 %*% t(slambda^2)
  matern <- function(nu) {
    2^d * pi^(d / 2) * gamma((d + nu) / 2) * nu^(nu / 2) / gamma(nu / 2) *
      (nu + q)^(-(d + nu) / 2)
  }
  kernel_part <- switch(
    kernel,
    exp_quad = sqrt(2 * pi)^d * exp(-q / 2),
    matern32 = matern(3),
    matern52 = matern(5)
  )
  sdgp^2 * apply(l, 1L, prod) * kernel_part
}


#' The ordered levels a monotonic term uses
#'
#' brms codes a monotonic variable with `D + 1` levels as `0..D`, with
#' the first level `0` in every frame. A prediction frame holding only
#' the upper levels is coded from `1`. Its levels are used unchanged.
#' Shifting such a frame to start at `0` would move every row down a
#' level.
#'
#' @param xmo_data The `Xmo_<i>` entry of the Stan data
#' @param xmo_name Its name, for the refusal
#' @param k_levels `D`, one less than the number of levels
#' @param n_obs Number of rows the predictor covers
#' @return Integer vector of levels in `0..D`
#'
#' @noRd
validate_monotonic_indices <- function(xmo_data, xmo_name, k_levels, n_obs) {
  checkmate::assert_integerish(xmo_data, any.missing = FALSE)
  checkmate::assert_count(k_levels, positive = TRUE)
  Xmo <- as.integer(xmo_data)
  if (length(Xmo) != n_obs) {
    stop_mvgam_fault(
      paste0("Monotonic variable '", xmo_name, "' does not cover the ",
             "prediction's rows."),
      paste0("'", xmo_name, "' has ", length(Xmo), " values for ", n_obs,
             " rows.")
    )
  }
  if (any(Xmo < 0L | Xmo > k_levels)) {
    stop_mvgam_fault(
      paste0("Monotonic variable '", xmo_name, "' holds a level ",
             "outside its coding."),
      paste0("Levels run 0 to ", k_levels, " and the data hold ",
             min(Xmo), " to ", max(Xmo), ".")
    )
  }
  Xmo
}


#' Remove the `_trend` the combined fit gives the trend's names
#'
#' The trend is a brms model of its own, whose data names are bare. The
#' combined program marks each of its parameters with `_trend` before
#' the index, as `b_trend[1]`, `r_1_1_trend[2]` or `sd_1_trend[1]`, or
#' at the end of an unindexed name.
#'
#' @param x Parameter names from the combined fit
#' @return `x` as the trend's brms model names them
#' @noRd
strip_trend_infix <- function(x) {
  sub("_trend(_|\\[|$)", "\\1", x)
}


#' The brms model one side of a fit is written as
#'
#' @param mvgam_fit A fitted `mvgam` object or a prefit
#' @param side `"obs"` or `"trend"`
#' @return A `brmsfit`, or `NULL` for a trend the fit does not have
#' @noRd
side_model <- function(mvgam_fit, side) {
  checkmate::assert_choice(side, c("obs", "trend"))
  if (identical(side, "trend")) mvgam_fit$trend_model else mvgam_fit$obs_model
}


#' The draws of one side's parameters, named as its brms model names them
#'
#' The observation model's parameters, a distributional parameter's
#' included, keep the names its brms model gives them. The trend's
#' carry `_trend`, which its own brms model does not write.
#'
#' @param full_draws The fit's draws matrix at the chosen draws, holding
#'   at least the side's parameters
#' @param pars The side's parameter names, as `side_parameters()` gives
#' @param side `"obs"` or `"trend"`
#' @return A draws matrix of that side's parameters
#' @noRd
side_draws <- function(full_draws, pars, side) {
  checkmate::assert_class(full_draws, "draws_matrix")
  checkmate::assert_character(pars, any.missing = FALSE)
  out <- full_draws[, pars, drop = FALSE]
  if (identical(side, "trend")) {
    colnames(out) <- strip_trend_infix(colnames(out))
  }
  out
}


#' Whether prediction draws hold one matrix per response
#'
#' A multivariate model predicted with `resp = NULL` gives a list of
#' draws matrices named by response. Every other call gives one matrix.
#'
#' @param x Draws from a prediction function
#' @return A single logical
#' @noRd
is_per_response <- function(x) {
  is.list(x) && !is.matrix(x)
}


#' Linear predictor of one component of a fitted model
#'
#' The observation model's mean, one of its distributional parameters,
#' or the trend's mean, on the link scale. The observation model's
#' parameters keep the names brms gave them. The composer looks them up
#' under each predictor's own suffix. The trend's parameters are renamed
#' to the bare names its own brms model uses.
#'
#' @param mvgam_fit A fitted `mvgam` object
#' @param newdata Data frame to predict for
#' @param component `"obs"`, `"trend"` or the name of a distributional
#'   parameter that has a formula of its own
#' @param resp The response's key, or `NULL`
#' @param draw_ids Draw indices to use, or `NULL` for all
#' @param re_formula,allow_new_levels,sample_new_levels As
#'   `validate_group_level_args()` takes them
#' @return A `[ndraws x nobs]` matrix, or a list of them named by
#'   response
#' @noRd
extract_component_linpred <- function(mvgam_fit, newdata, component = "obs",
                                      resp = NULL, draw_ids = NULL,
                                      re_formula = NULL,
                                      allow_new_levels = FALSE,
                                      sample_new_levels = "uncertainty") {
  prepared <- prepare_component(
    mvgam_fit, newdata, component, resp, draw_ids, re_formula,
    allow_new_levels, sample_new_levels
  )
  finish_component(prepared, extract_linpred_from_prep(
    prepared$prep, resp = prepared$resp, dpar = prepared$dpar
  ))
}


#' Each formula term's part of one component's linear predictor
#'
#' The parts sum to `extract_component_linpred()` with the same
#' arguments.
#'
#' @inheritParams extract_component_linpred
#' @return A named list of `[ndraws x nobs]` matrices, one per term as
#'   `linear_terms_parts()` names them, or a list of them named by
#'   response
#' @noRd
extract_component_parts <- function(mvgam_fit, newdata, component = "obs",
                                    resp = NULL, draw_ids = NULL,
                                    re_formula = NULL,
                                    allow_new_levels = FALSE,
                                    sample_new_levels = "uncertainty") {
  prepared <- prepare_component(
    mvgam_fit, newdata, component, resp, draw_ids, re_formula,
    allow_new_levels, sample_new_levels
  )
  finish_component(prepared, extract_parts_from_prep(
    prepared$prep, resp = prepared$resp, dpar = prepared$dpar
  ))
}


#' The prepared observation data an ordinal response's thresholds need
#'
#' One set of thresholds and no category-specific term need none. A
#' category-specific term shifts each threshold by the prediction's own
#' design. Thresholds that vary by group, `thres(gr = )`, take each
#' row's set from the prediction data's `Jthres`.
#'
#' @inheritParams extract_component_linpred
#' @return What `prepare_component()` gives, or `NULL`
#' @noRd
ordinal_threshold_prep <- function(mvgam_fit, newdata, resp = NULL,
                                   draw_ids = NULL, re_formula = NULL,
                                   allow_new_levels = FALSE,
                                   sample_new_levels = "uncertainty") {
  checkmate::assert_class(mvgam_fit, "mvgam")
  # A model with one response names its predictors without a key
  key <- if (length(response_formulas(mvgam_fit)) > 1L) resp
  sfx <- predictor_suffix(key)
  sdata <- mvgam_fit$standata
  ranef <- side_model(mvgam_fit, "obs")$ranef
  needed <- (sdata[[paste0("Kcs", sfx)]] %||% 0L) > 0L ||
    !is.null(sdata[[paste0("ngrthres", sfx)]]) ||
    any(blank_if_na(ranef$type) == "cs" &
          blank_if_na(ranef$resp) == (key %||% ""))
  if (!needed) {
    return(NULL)
  }
  prepare_component(
    mvgam_fit, newdata, "obs", key, draw_ids, re_formula,
    allow_new_levels, sample_new_levels
  )
}


#' Each category-specific term's shift of the ordinal thresholds
#'
#' The linear predictor holds one value per draw and row. These shifts
#' hold one per threshold as well. `ordinal_probs()` subtracts their
#' sum from the thresholds.
#'
#' @inheritParams extract_component_linpred
#' @return Named list of `[ndraws x nobs x nthres]` arrays, as
#'   `category_specific_parts()` gives, empty for a response without a
#'   category-specific term
#' @noRd
extract_threshold_shifts <- function(mvgam_fit, newdata, resp = NULL,
                                     draw_ids = NULL, re_formula = NULL,
                                     allow_new_levels = FALSE,
                                     sample_new_levels = "uncertainty") {
  threshold_shifts_of(ordinal_threshold_prep(
    mvgam_fit, newdata, resp = resp, draw_ids = draw_ids,
    re_formula = re_formula, allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels
  ))
}


#' The category-specific shifts a prepared ordinal response holds
#'
#' @param prepared What `ordinal_threshold_prep()` gives
#' @return As `extract_threshold_shifts()`
#' @noRd
threshold_shifts_of <- function(prepared) {
  if (is.null(prepared)) {
    return(list())
  }
  prep <- prepared$prep
  category_specific_parts(
    prep, as_plain_matrix(posterior::as_draws_matrix(prep$draws)),
    prepared$resp, predictor_nobs(prep, prepared$resp)
  )
}


#' The set of thresholds each prediction row takes
#'
#' brms writes `Jthres`, the first and last index of each row's set in
#' the merged thresholds. `nthres` holds the size of each set. A row's
#' first index opens its set.
#'
#' @param prepared What `ordinal_threshold_prep()` gives
#' @param n_sets Number of sets `ordinal_thresholds()` returns
#' @param n_obs Number of prediction rows
#' @return Integer vector, one set per row
#' @noRd
threshold_row_sets <- function(prepared, n_sets, n_obs) {
  checkmate::assert_count(n_sets, positive = TRUE)
  if (n_sets == 1L) {
    return(rep(1L, n_obs))
  }
  sdata <- prepared$prep$sdata
  sfx <- predictor_suffix(prepared$resp)
  J <- sdata_entry(sdata, paste0("Jthres", sfx))
  sizes <- as.integer(sdata_entry(sdata, paste0("nthres", sfx)))
  starts <- cumsum(c(1L, utils::head(sizes, -1L)))
  out <- match(as.integer(J[, 1L]), starts)
  if (length(out) != n_obs || anyNA(out) || length(sizes) != n_sets) {
    stop_mvgam_fault(
      "Each prediction row must open one set of thresholds.",
      paste0(n_sets, " sets, ", length(sizes), " sizes, ", length(out),
             " rows for ", n_obs, ".")
    )
  }
  out
}


#' The prepared data and draws of one component's linear predictor
#'
#' @inheritParams extract_component_linpred
#' @return A list: `prep`, the `mvgam_prep`; `resp` and `dpar`, the
#'   predictor as the composer takes it; and `projection`, `NULL`, or
#'   what `project_factor_rows()` takes for a trend under
#'   `by = lv_axis()`
#' @noRd
prepare_component <- function(mvgam_fit, newdata, component, resp, draw_ids,
                              re_formula, allow_new_levels,
                              sample_new_levels) {
  checkmate::assert_class(mvgam_fit, "mvgam")
  checkmate::assert_data_frame(newdata, min.rows = 1)
  checkmate::assert_string(component)
  checkmate::assert_string(resp, null.ok = TRUE)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE,
                               any.missing = FALSE)
  # Stamp the empty-obs placeholder onto user-supplied newdata
  # when the fit needed one. brms's `validate_data()` would
  # otherwise reject the frame for missing the column.
  newdata <- ensure_obs_placeholder(newdata, mvgam_fit)
  # The gap check runs before the level check. A gap in the axis is
  # named as a gap. Without this check, the level comparison reports an
  # unknown series.
  validate_newdata_complete(newdata, mvgam_fit)
  if (!is.null(mvgam_fit$trend_metadata)) {
    validate_prediction_factor_levels(
      newdata, mvgam_fit$trend_metadata,
      series_label = series_column_label(mvgam_fit)
    )
  }

  is_trend <- identical(component, "trend")
  side <- if (is_trend) "trend" else "obs"
  brms_model <- side_model(mvgam_fit, side)
  if (is.null(brms_model)) {
    if (is_trend) {
      stop(insight::format_error(c(
        "This 'mvgam' fit has no latent trend to predict from.",
        i = paste0(
          "A trend is declared through 'trend_formula', for example ",
          "trend_formula = ~ AR()."
        )
      )), call. = FALSE)
    }
    stop_mvgam_fault(
      "This 'mvgam' fit carries no observation model.",
      "Every fit is built with one."
    )
  }

  # Draws arrive already chosen. The user-facing boundary converts their
  # count into indices. This extraction uses the same rows as every
  # extraction it is combined with.
  # The side's own parameters, plus the loadings a `by = lv_axis()`
  # trend projects its factor predictor through
  by_lv <- is_trend && isTRUE(mvgam_fit$trend_metadata$has_by_lv)
  pars <- side_parameters(mvgam_fit, side)
  full_draws <- subset_draws_rows(
    fit_draws(mvgam_fit$fit, c(pars, if (by_lv) c("Z", "Z_tilde"))),
    draw_ids = draw_ids
  )
  component_draws <- side_draws(full_draws, pars, side)

  if (by_lv) {
    return(prepare_by_lv_trend(
      mvgam_fit = mvgam_fit,
      newdata = newdata,
      full_draws = full_draws,
      trend_draws = component_draws,
      re_formula = re_formula,
      allow_new_levels = allow_new_levels,
      sample_new_levels = sample_new_levels,
      resp = resp
    ))
  }

  if (is_trend) {
    newdata <- derived_series_column(mvgam_fit, newdata)
  }
  prep <- prepare_linpred_data(
    draws = component_draws,
    brmsfit = brms_model,
    newdata = newdata,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels,
    trend = is_trend
  )
  # The trend's brms model has one response, shared by every response
  # of the observation model.
  list(prep = prep, resp = if (!is_trend) resp,
       dpar = if (!component %in% c("obs", "trend")) component,
       projection = NULL)
}


#' A component's composed draws, as a prediction returns them
#'
#' A trend under `by = lv_axis()` is composed per factor and projected
#' onto each row's series. A `draws_matrix` keeps its class through
#' subsetting and arithmetic. Whether a predictor composed from one
#' carries the class depends on the terms the model has. The class is
#' dropped on the way out, where the predictor stops being posterior
#' draws and becomes a value. An S4 slot declared to hold a plain matrix
#' rejects the classed one.
#'
#' @param prepared What `prepare_component()` gives
#' @param x A matrix, or a list of them at any depth
#' @return `x`, each matrix projected and plain
#' @noRd
finish_component <- function(prepared, x) {
  if (!is.matrix(x)) {
    return(lapply(x, finish_component, prepared = prepared))
  }
  if (!is.null(prepared$projection)) {
    x <- project_factor_rows(x, prepared$projection)
  }
  as_plain_matrix(x)
}


#' The trend's data on a fit whose trend runs at the factor grain
#'
#' Under `by = lv_axis()` the trend's brms model is written over
#' `(time, .trend)`, one row per latent factor. Stan computes
#' `trend[t, s] = dot(Z[s, ], lv_trend[t, ] + mu_factor[t, ])`. The
#' composer gives `mu_factor` on a grid holding each row of `newdata`
#' once per factor. `project_factor_rows()` gives the
#' `dot(Z, mu_factor)` half. The `lv_trend` half is the latent state. A
#' prediction receives it as the marginal envelope that
#' `sample_process_errors()` adds. On the hindcast and forecast paths,
#' `extract_trend_latent_states()` supplies it per draw.
#'
#' @param mvgam_fit A fitted `mvgam` object whose trend has `by =
#'   lv_axis()`
#' @param newdata Data frame to predict for, one row per `(time,
#'   series)`
#' @param full_draws The fit's draws at the chosen draws, holding the
#'   loadings
#' @param trend_draws The same draws of the trend's parameters, named
#'   as the trend's brms model names them
#' @param re_formula,allow_new_levels,sample_new_levels As
#'   `validate_group_level_args()` takes them
#' @param resp The response's key on a model with several, or `NULL`
#' @return What `prepare_component()` gives
#' @noRd
prepare_by_lv_trend <- function(mvgam_fit, newdata, full_draws,
                                trend_draws, re_formula, allow_new_levels,
                                sample_new_levels, resp = NULL) {
  checkmate::assert_class(mvgam_fit, "mvgam")
  checkmate::assert_data_frame(newdata, min.rows = 1L)
  checkmate::assert_class(full_draws, "draws_matrix")
  checkmate::assert_class(trend_draws, "draws_matrix")
  checkmate::assert_true(nrow(full_draws) == nrow(trend_draws))

  n_lv <- as.integer(mvgam_fit$trend_metadata$n_lv_for_grain)
  n_series <- mvgam_axes(mvgam_fit)$series$n
  if (length(n_lv) != 1L || length(n_series) != 1L) {
    stop_mvgam_fault(
      "A 'by = lv_axis()' fit does not record its factor and series counts.",
      "The trend prediction needs both counts."
    )
  }
  trend_model <- side_model(mvgam_fit, "trend")

  obs_struct <- get_observation_structure(mvgam_fit, newdata = newdata,
                                          resp = resp)
  s_idx <- as.integer(obs_struct$series_int)
  # The frame's series were checked against the fit's series. Every row
  # has a trend column.
  if (anyNA(s_idx) || any(s_idx < 1L | s_idx > n_series)) {
    stop_mvgam_fault(
      "A prediction row was placed on no trend column.",
      paste0("Series indices run 1 to ", n_series, ".")
    )
  }

  # Per-row by per-factor grid: each newdata row repeated for each
  # factor. A grid collapsed to one row per (time, .trend) would drop
  # the rows of a marginaleffects grid that share a time and differ
  # in a covariate. `expand_grid()` iterates `.trend` fastest: each
  # consecutive block of `n_lv` columns of the composed predictor
  # belongs to one newdata row.
  trend_vars <- mvgam_fit$trend_metadata$covariates %||% character(0)
  attach_cols <- intersect(
    unique(c(trend_vars, axis_vars(mvgam_fit)$time_var)),
    colnames(newdata)
  )
  lv_newdata <- expand_factor_axis(n_lv, .row_id = seq_len(nrow(newdata)))
  for (col in attach_cols) {
    lv_newdata[[col]] <- newdata[[col]][lv_newdata$.row_id]
  }

  prep <- prepare_linpred_data(
    draws = trend_draws, brmsfit = trend_model,
    newdata = lv_newdata, re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels, trend = TRUE
  )
  # `mu_factor` is the per-factor predictor over the latent axis. It
  # belongs to the `Z` the model sampled. Stan folds it inside the
  # projection for these fits. The basis is named explicitly. The two
  # halves cannot drift apart.
  Z <- extract_Z_loadings(full_draws, n_obs_series = n_series, n_lv = n_lv,
                          basis = "model")
  list(prep = prep, resp = NULL, dpar = NULL,
       projection = list(Z = Z, series = s_idx, n_lv = n_lv))
}


#' A per-factor predictor projected onto each row's series
#'
#' `out[d, i] = sum_k Z[d, series_i, k] * mu_factor[d, i, k]`, where
#' the columns of `mu_factor` hold each row's `n_lv` factors in turn.
#'
#' @param mu_factor A `[ndraws x (nrows * n_lv)]` matrix
#' @param projection List: `Z`, the `[ndraws, n_series, n_lv]`
#'   loadings; `series`, each row's series index; `n_lv`
#' @return A `[ndraws x nrows]` matrix
#' @noRd
project_factor_rows <- function(mu_factor, projection) {
  checkmate::assert_names(names(projection),
                          must.include = c("Z", "series", "n_lv"))
  n_lv <- projection$n_lv
  s_idx <- projection$series
  n_rows <- length(s_idx)
  checkmate::assert_matrix(mu_factor, ncols = n_rows * n_lv)
  n_series <- dim(projection$Z)[2L]
  out <- matrix(NA_real_, nrow = nrow(mu_factor), ncol = n_rows)
  for (d in seq_len(nrow(mu_factor))) {
    mu_d <- matrix(mu_factor[d, ], nrow = n_rows, ncol = n_lv, byrow = TRUE)
    Z_d <- matrix(projection$Z[d, , ], nrow = n_series, ncol = n_lv)
    # Per row i: dot(Z_d[series_i, :], mu_d[i, :])
    out[d, ] <- rowSums(Z_d[s_idx, , drop = FALSE] * mu_d)
  }
  out
}


#' Choose the prediction surface a diagnostic should use
#'
#' A residual or a posterior predictive check compares a prediction
#' against the observation that was actually recorded. In sample, the
#' comparison uses the latent state the model inferred for that time.
#' That is the state `hindcast()` returns. Using it keeps every
#' diagnostic describing one series.
#'
#' Given `newdata` the fit never saw, no such state exists. The marginal
#' surface is then the one used. It includes the trend's process
#' uncertainty. The exception is a check weighted by importance ratios.
#' Those ratios come from the likelihood, which is conditional. The draws
#' they reweight must be conditional too, whatever data they cover.
#'
#' The caller states whether the rows are the ones the fit saw. The
#' function does not test the frame for emptiness. A caller that has
#' already substituted the training frame for a `NULL` `newdata` would
#' request the marginal surface while describing the data the model was
#' fitted to. A panel would then plot a conditional residual against a
#' marginal fitted value.
#'
#' @param args Argument list destined for a `posterior_*` method
#' @param in_sample Whether the rows are the ones the fit was given
#' @param weighted Whether the result will be reweighted by importance
#'   ratios
#' @return `args`, with `incl_autocor` stamped on when the conditional
#'   surface applies and the caller has not named it already
#'
#' @noRd
diagnostic_surface_args <- function(args, in_sample, weighted = FALSE) {
  checkmate::assert_list(args)
  checkmate::assert_flag(in_sample)
  checkmate::assert_flag(weighted)
  if ("incl_autocor" %in% names(args)) {
    return(args)
  }
  if (in_sample || weighted) {
    args$incl_autocor <- TRUE
  }
  args
}


#' Which trend column each prediction row uses
#'
#' Ordinarily a row names its own series. The observation structure
#' numbers it. A wide `mvbf()` frame does not follow this pattern. A row
#' there is one occasion carrying every response. Its series column is a
#' single constant. Numbering it puts all sixty rows on series one. The
#' series is a property of the `(row, response)` pair. The axis record
#' holds the response half of that pair. The scoped response picks the
#' column. The row supplies the time.
#'
#' Refusing when no response is named keeps this safe to extend. A new
#' reader of the observation structure that does not scope itself stops
#' here. Without the refusal, it would use the first response's state for
#' every response.
#'
#' @param object A fitted `mvgam` object
#' @param series_int The per-row series index the frame's own series
#'   values give
#' @param resp The response this prediction is scoped to, or `NULL`
#' @return An integer, one per prediction row, indexing the columns of
#'   `trend[t, s]`
#'
#' @noRd
trend_series_index <- function(object, series_int, resp) {
  if (!is_response_keyed(object)) {
    return(series_int)
  }
  resolve_resp(object, resp, required = TRUE)
  col <- response_series_index(mvgam_axes(object)$series$levels, resp)
  rep(col, length(series_int))
}


#' Extract per-observation latent trend state draws
#'
#' Takes the draws of a series-grain state `state[t, s]` and aligns
#' them to the rows of `newdata` by `(time, series)`. The state is the
#' fitted `trend` unless the caller names another, such as a state
#' component from `series_path_draws()`.
#'
#' @param mvgam_fit A fitted `mvgam` object
#' @param newdata The rows to align to
#' @param full_draws Draws matrix holding the `state[t, s]` columns
#' @param resp A response name, or NULL
#' @param state Name of the `[time, series]` state
#' @return `[ndraws, nrow(newdata)]` matrix, or NULL for a fit without
#'   the state
#' @noRd
extract_trend_latent_states <- function(mvgam_fit, newdata, full_draws,
                                        resp = NULL, state = "trend") {
  checkmate::assert_class(mvgam_fit, "mvgam")
  checkmate::assert_data_frame(newdata, min.rows = 1)
  checkmate::assert_matrix(full_draws, min.rows = 1)
  checkmate::assert_string(resp, null.ok = TRUE)
  checkmate::assert_string(state)

  par_names <- colnames(full_draws)
  if (!any(startsWith(par_names, paste0(state, "[")))) {
    return(NULL)
  }

  N_time_trend <- mvgam_fit$standata$N_time_trend
  N_series_trend <- mvgam_fit$standata$N_series_trend
  if (is.null(N_time_trend) || is.null(N_series_trend)) {
    stop_mvgam_fault(
      "The fit's Stan data lack 'N_time_trend' or 'N_series_trend'.",
      "Aligning the latent trend states needs both."
    )
  }

  # Map newdata rows to columns of the fitted trend matrix. The lookup
  # runs on the raw time values against the grid the model was fitted
  # on. The observation structure renumbers time from one within each
  # frame it is handed. A frame holding only the later half of a series
  # would take the state of the earlier half, silently and with the right
  # shape. A time the fit never saw matches nothing in the raw values.
  # Such a time is refused below.
  obs_struct <- get_observation_structure(mvgam_fit, newdata = newdata,
                                          resp = resp)
  s_idx <- obs_struct$series_int
  time_var <- axis_vars(mvgam_fit)$time_var
  # The grid the trend was fitted on, from the record that owns it.
  raw_t_idx <- if (time_var %in% names(newdata)) {
    match(newdata[[time_var]], mvgam_axes(mvgam_fit)$time$values)
  }
  # A closure-unit family predicts on the unit grid. The raw lookup applies
  # only when that grid covers the same rows as the observation structure.
  # Otherwise the position that structure assigned is the alignment used.
  t_idx <- if (!is.null(raw_t_idx) &&
                 length(raw_t_idx) == length(s_idx)) {
    raw_t_idx
  } else {
    match(obs_struct$time, obs_struct$unique_times)
  }

  # `get_observation_structure()` refuses a series the fit never saw
  if (any(s_idx < 1L | s_idx > N_series_trend)) {
    stop_mvgam_fault(
      "Every prediction row needs a fitted series.",
      paste0("Series indices outside 1 to ", N_series_trend,
             " reached the state lookup.")
    )
  }

  # A time outside the fitted grid has no fitted state. `forecast()`
  # propagates the state to later times with its variance.
  if (anyNA(t_idx)) {
    time_label <- user_axis_names(mvgam_fit, time_var)
    stop(insight::format_error(c(
      "The fitted trend state covers the fitted times only.",
      x = paste0(
        sum(is.na(t_idx)), " row(s) of 'newdata' hold a value of '",
        time_label, "' outside the fitted times."
      ),
      i = "Use forecast() to predict new times.",
      i = "Use score() on a forecast or lfo_cv() to score new times.",
      i = "Set 'incl_autocor = FALSE' to predict from the covariates."
    )), call. = FALSE)
  }

  ndraws <- nrow(full_draws)
  nobs <- length(t_idx)
  # Every cell is named and located in one pass. The cost grows with
  # the number of observations plus the number of parameters.
  wanted <- paste0(state, "[", t_idx, ",", s_idx, "]")
  col <- match(wanted, par_names)
  absent <- is.na(col)
  if (any(absent)) {
    stop_mvgam_fault(
      paste0("The draws must hold '", state, "[t, s]' for every fitted ",
             "time and series."),
      paste0("Missing: '", wanted[which(absent)[1L]], "'.")
    )
  }

  latent_mat <- matrix(NA_real_, nrow = ndraws, ncol = nobs)
  latent_mat[] <- full_draws[, col, drop = FALSE]
  latent_mat
}
