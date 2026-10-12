# forecast.mvgam: posterior forecasts that propagate the fitted latent
# state of every trend type, series-grain and factor alike.
# `build_forecast_arms()` and `propagate_one_draw()` give the steps.


#' Posterior forecasts for an mvgam state-space model
#'
#' Posterior forecasts at future time points, with posterior hindcasts
#' at the training time points. Each draw propagates the fitted latent
#' state through the trend's dynamics. The observation family then
#' maps the trend to the response scale.
#'
#' @param object A fitted [mvgam][mvgam::mvgam] object.
#' @param newdata Optional `data.frame` with the columns of the
#'   training data, holding the time and series cells to forecast.
#'   Rows with times beyond the training grid set the horizon. When
#'   `NULL`, `forecast()` uses the held-out data given to
#'   `mvgam(newdata = )`. A fit without held-out data needs `newdata`.
#'
#'   Five trends step once per time point: AR(), RW(), VAR(), LLT()
#'   and PW(). Their forecast times must continue the training series
#'   at its own spacing, without a gap. After training ends at
#'   `t = 30`, a request for `t = 41:42` is refused: two rows would
#'   report a two-step spread for a twelve-step horizon. Supply every
#'   intervening time, whether or not the response is observed there.
#'   `CAR()` takes the elapsed gap into its kernel. Each series may
#'   have its own forecast times, all after the last fitted time.
#'   `ZMVN()` has no temporal dynamics and takes any later times.
#'   `ZMVN()` and `PW()` need one set of forecast times shared by
#'   every series.
#' @param ... Unused. A misspelt argument would land here and is
#'   refused.
#' @param type One of `"response"`, `"link"`, `"expected"`,
#'   `"trend"`. `"response"` samples from the observation family
#'   (the default); `"expected"` returns the family's mean, which
#'   carries any further parameter the expectation needs (a binomial
#'   mean is `trials * p`, a zero-inflated mean `(1 - zi) * mu`);
#'   `"link"` returns the link-scale linear predictor; `"trend"`
#'   returns the latent-trend trajectory on the link scale.
#' @param ndraws Optional integer; the number of posterior draws
#'   to use, taken at random. Defaults to all available draws.
#' @param draw_ids Optional integer vector of the posterior draws to
#'   use, in the order given. Supply one of `ndraws` or `draw_ids`.
#'   Passing the same `draw_ids` to another method, such as
#'   `posterior_predict()` or `stability()`, pairs each forecast draw
#'   with the same iteration there.
#' @param coef_uncertainty Logical. When `FALSE`, every forecast
#'   draw uses the first posterior draw of every coefficient in the
#'   observation and trend formulas: population-level effects, smooth
#'   bases, random effect levels and Hilbert-space `gp(x, k)` bases.
#'   Defaults to `TRUE`.
#' @param trend_uncertainty Logical. When `FALSE`, every forecast
#'   draw uses the first posterior draw of the trend dynamics
#'   parameters (`sigma`, AR, MA and VAR coefficients), the last
#'   fitted state and the trend formula's predictor `mu_trend`. Each
#'   draw still takes fresh innovations. The spread then shows
#'   innovation noise alone. Defaults to `TRUE`.
#' @param obs_uncertainty Logical. When `FALSE`, skips
#'   observation-family sampling for `type = "response"`,
#'   returning the family mean instead.
#'   Defaults to `TRUE`.
#' @param resp For multivariate (multiple-response) models, the
#'   name of a single response variable to return results for.
#'   `NULL` (the default) returns results for all responses.
#'
#' @return An object of class [mvgam_forecast-class][mvgam].
#'   Hindcasts and forecasts are returned as named lists of
#'   `[ndraws, n_times]` matrices, one per series.
#'
#' @seealso [mvgam_forecast-class][mvgam], [hindcast.mvgam].
#'   [posterior_predict.mvgam] and [posterior_epred.mvgam] predict by
#'   integrating over the trend's stochastic dynamics. `forecast()`
#'   extrapolates the fitted latent state. The online article
#'   \url{https://nicholasjclark.github.io/mvgam/articles/forecast_evaluation.html}
#'   walks through `forecast()` together with `score()`,
#'   `lfo_cv()` and `ensemble()` on a worked count-data example.
#' @examples
#' \dontrun{
#' set.seed(11)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR(),
#'                      proportional_train = 0.75)
#'
#' mod <- mvgam(
#'   y ~ s(x),
#'   trend_formula = ~ AR(p = 1),
#'   data    = simdat$data_train,
#'   newdata = simdat$data_test,
#'   family  = poisson(),
#'   chains  = 2, silent = 2
#' )
#'
#' # The held-out data given to mvgam() drive the forecast horizon.
#' fc <- forecast(mod)
#' plot(fc)
#' }
#'
#' @importFrom generics forecast
#' @method forecast mvgam
#' @export
forecast.mvgam <- function(object,
                           newdata = NULL,
                           ...,
                           type = c("response", "link",
                                    "expected", "trend"),
                           ndraws = NULL,
                           draw_ids = NULL,
                           coef_uncertainty = TRUE,
                           trend_uncertainty = TRUE,
                           obs_uncertainty = TRUE,
                           resp = NULL) {
  checkmate::assert_class(object, "mvgam")
  require_fitted_model(object, "forecast")
  # Every argument after `...` is named. A misspelt one would land in
  # `...` and leave its default in force.
  rlang::check_dots_empty()
  type <- match.arg(type)
  validate_draw_selectors(ndraws, draw_ids)
  checkmate::assert_flag(coef_uncertainty)
  checkmate::assert_flag(trend_uncertainty)
  checkmate::assert_flag(obs_uncertainty)
  checkmate::assert_string(resp, null.ok = TRUE)
  fan <- mv_resp_fan_out(object, resp, class = "mvgam_forecast")
  if (!is.null(fan)) {
    return(fan)
  }
  newdata <- ensure_obs_placeholder(
    jsdgam_axis_columns(object, newdata %||% object$test_data), object
  )

  trend_specs <- object$mv_spec$trend_specs
  # A trendless fit forecasts its observation predictor at the new
  # occasions. With no latent state, `type = "trend"` names nothing.
  is_trendless <- is.null(trend_specs)
  if (is_trendless && identical(type, "trend")) {
    stop(insight::format_error(c(
      "'type = \"trend\"' is not defined for trendless fits.",
      i = paste0("Use 'type = \"link\"', 'type = \"expected\"' ",
                 "or 'type = \"response\"' (the default).")
    )), call. = FALSE)
  }
  # The trend-type guard below refuses anything unsupported
  trend_model <- if (is_trendless) NULL else trend_spec_head(trend_specs)
  meta <- if (is_trendless) NULL else object$trend_metadata

  # The horizon comes from the caller's frame or the held-out data
  # stored at fit time. A fit cannot invent occasions. Every
  # covariate the model uses must be in that frame.
  if (is.null(newdata)) {
    stop(insight::format_error(c(
      "'newdata' is required to forecast.",
      x = "The fit stores no held-out data from 'mvgam(newdata = )'.",
      i = paste0("Pass a frame with times after the training grid and ",
                 "every covariate the model uses."),
      i = "For the training occasions, use 'hindcast()'."
    )), call. = FALSE)
  }

  series_levels <- fitted_series_levels(object)
  reported <- reported_series(object, resp, series_levels)

  draws_mat <- posterior::as_draws_matrix(object$fit)
  total_draws <- nrow(draws_mat)
  draw_idx <- resolve_draw_indices(total_draws, ndraws, draw_ids)

  training <- build_training_arms(object, series_levels, resp = resp)
  fc_grid <- resolve_forecast_grid(object, newdata, training,
                                     series_levels, resp = resp)

  # A discrete-time trend takes one step per forecast row. The
  # propagator cannot tell a gap from a shorter horizon. The rows
  # are checked here.
  if (!is.null(fc_grid)) {
    assert_forecast_times_steppable(fc_grid$times, object)
  }

  # The hindcast slot inside a forecast() result uses the same
  # deterministic-state convention as hindcast()
  # (process_error = FALSE) so the two surfaces agree at
  # the training grid.
  hindcasts <- build_hindcast_arms(
    object, training, type, draw_idx, obs_uncertainty,
    process_error = FALSE, resp = resp, series_levels = reported
  )

  forecasts <- if (is.null(fc_grid)) {
    NULL
  } else if (is_trendless) {
    build_trendless_forecast_arms(
      object = object,
      fc_grid = fc_grid,
      type = type,
      draw_idx = draw_idx,
      obs_uncertainty = obs_uncertainty,
      series_levels = reported,
      resp = resp
    )
  } else {
    build_forecast_arms(
      object = object,
      trend_model = trend_model,
      meta = meta,
      training = training,
      fc_grid = fc_grid,
      type = type,
      draws_mat = draws_mat,
      draw_idx = draw_idx,
      coef_uncertainty = coef_uncertainty,
      trend_uncertainty = trend_uncertainty,
      obs_uncertainty = obs_uncertainty,
      series_levels = series_levels,
      resp = resp,
      reported = reported
    )
  }

  family_pars <- if (type == "link") {
    extract_family_pars_for_draws(object, draws_mat, draw_idx,
                                    resp = resp)
  } else {
    NULL
  }

  new_mvgam_forecast(
    object, type, resp, reported, training,
    hindcasts = hindcasts, forecasts = forecasts, fc_grid = fc_grid,
    family_pars = family_pars
  )
}


# ----- Series / training / forecast grids -------------------------

#' The series a result scoped to `resp` reports
#'
#' A wide frame's series are its responses. A result for one response
#' covers its own series.
#'
#' @param object A fitted `mvgam` object
#' @param resp A response name, or NULL
#' @param series_levels The fitted series
#' @return Character vector of series
#' @noRd
reported_series <- function(object, resp, series_levels) {
  checkmate::assert_string(resp, null.ok = TRUE)
  if (!is.null(resp) && is_response_keyed(object)) resp else series_levels
}


#' The `mvgam_forecast` record a hindcast or forecast returns
#'
#' Cut to the series the result reports. The record names the family
#' of the response it is for. A multivariate fit's top-level family is
#' a placeholder every response shares.
#'
#' @param object A fitted `mvgam` object
#' @param type The prediction type
#' @param resp A response name, or NULL
#' @param reported The series the result reports
#' @param training The training data from `build_training_arms()`
#' @param hindcasts,forecasts Lists of draws matrices, one per series
#' @param fc_grid The forecast grid, or NULL
#' @param family_pars Family parameter draws for `type = "link"`
#' @return An `mvgam_forecast`
#' @noRd
new_mvgam_forecast <- function(object, type, resp, reported, training,
                               hindcasts, forecasts = NULL,
                               fc_grid = NULL, family_pars = NULL) {
  pick <- function(x) if (is.null(x)) NULL else x[reported]
  structure(
    list(
      family = resolve_family_name(model_families(object, resp)),
      family_pars = family_pars,
      type = type,
      series_names = factor(reported, levels = reported),
      train_observations = pick(training$observations),
      train_times = if (!is.null(training)) {
        pick(hindcast_column_times(object, type, resp, training))
      },
      test_observations = pick(fc_grid$observations),
      test_times = pick(fc_grid$times),
      hindcasts = hindcasts,
      forecasts = forecasts
    ),
    class = "mvgam_forecast"
  )
}


#' A wide fit's per-response forecasts as one forecast
#'
#' On a wide frame the responses are the series. Each element of the
#' per-response list holds one series. Plotting and scoring take them
#' as one forecast over the whole axis. Each series keeps the family
#' of its own response.
#'
#' @param x An `mvgam_forecast` holding one element per response
#' @return An `mvgam_forecast` over every series
#' @noRd
response_axis_forecast <- function(x) {
  checkmate::assert_class(x, "mvgam_forecast")
  levs <- unlist(lapply(x, function(el) as.character(el$series_names)),
                 use.names = FALSE)
  # `c()` over a named list prefixes each element's names with the
  # list's. Unnamed elements keep the series names.
  arms <- function(field) {
    out <- do.call(c, unname(lapply(unclass(x), `[[`, field)))
    if (is.null(out)) NULL else out[levs]
  }
  structure(
    list(
      family = stats::setNames(
        vapply(unclass(x), `[[`, character(1L), "family"), levs
      ),
      family_pars = NULL,
      type = x[[1L]]$type,
      series_names = factor(levs, levels = levs),
      train_observations = arms("train_observations"),
      train_times = arms("train_times"),
      test_observations = arms("test_observations"),
      test_times = arms("test_times"),
      hindcasts = arms("hindcasts"),
      forecasts = arms("forecasts")
    ),
    class = "mvgam_forecast"
  )
}


#' The training data of each series
#'
#' Each series' rows, observations and times, in time order, with the
#' frame and its time and series column names.
#'
#' On a long frame a series is the rows labelled with it. The label is
#' the derived one: a fit whose series came from a `gr` and `subgr`
#' pair takes its names from that pair. On a wide frame the responses
#' are the series. A series there is the occasions where its own
#' column was measured.
#'
#' @param object A fitted `mvgam` object
#' @param series_levels The series to cut
#' @param resp A response name, or NULL
#' @param data The frame to cut. Defaults to the fit's own. `lfo_cv()`
#'   passes a window of it.
#' @return List of `data`, `time_var`, `series_var`, `response_keyed`,
#'   `series_labels`, `series_rows`, `observations`, `times` (each
#'   occasion once) and `row_times` (one per row)
#' @noRd
build_training_arms <- function(object, series_levels, resp = NULL,
                                data = NULL) {
  checkmate::assert_atomic(series_levels, min.len = 1L)
  checkmate::assert_string(resp, null.ok = TRUE)
  checkmate::assert_data_frame(data, null.ok = TRUE)
  d <- data %||% object$data
  meta_vars <- axis_vars(object)
  time_var <- meta_vars$time_var
  series_var <- meta_vars$series_var

  response_keyed <- is_response_keyed(object)
  series_labels <- if (response_keyed) {
    NULL
  } else {
    as.character(training_series_labels(object, d))
  }
  # A wide frame's series takes its own response's column. Every other
  # series takes the one response in scope. `response_column()` maps
  # brms's key for a response to its column, which can differ by a `.`
  # or an `_`.
  column_of <- function(lv) {
    response_column(object, if (response_keyed) lv else resp)
  }

  # The observations, the row times and the hindcast columns all take
  # this time order
  series_rows <- lapply(series_levels, function(lv) {
    rows <- if (response_keyed) {
      which(!is.na(d[[column_of(lv)]]))
    } else {
      which(series_labels == lv)
    }
    rows[order(d[[time_var]][rows])]
  })
  names(series_rows) <- series_levels

  observations <- lapply(series_levels, function(lv) {
    rows <- series_rows[[lv]]
    if (!length(rows)) return(numeric(0L))
    as.numeric(d[[column_of(lv)]][rows])
  })
  names(observations) <- series_levels
  # The occasions as the user supplied them, continuous ones included.
  # A closure-unit family has one row per visit. Its `expected` and
  # `response` hindcasts take one column per row, which `row_times`
  # labels. `times` holds each occasion once, for every other surface.
  row_times <- lapply(series_rows, function(rows) d[[time_var]][rows])
  times <- lapply(row_times, function(t) sort(unique(t)))

  list(
    data = d,
    time_var = time_var,
    series_var = series_var,
    response_keyed = response_keyed,
    series_labels = series_labels,
    series_rows = series_rows,
    observations = observations,
    times = times,
    row_times = row_times
  )
}


#' The time of each column of a hindcast matrix
#'
#' `build_hindcast_arms()` predicts a closure-unit family's `expected`
#' and `response` predictions per visit row. Every other type takes one
#' column per occasion. `plot()` and `summary()` pair each column with
#' its time by position.
#'
#' @param object A fitted `mvgam` object
#' @param type The prediction type
#' @param resp A response name, or NULL
#' @param training The training data from `build_training_arms()`
#' @return List of times, one vector per series
#' @noRd
hindcast_column_times <- function(object, type, resp, training) {
  checkmate::assert_list(training)
  if (hindcast_per_row(object, type, resp)) {
    training$row_times
  } else {
    training$times
  }
}


#' Does a hindcast take one column per row of the frame?
#'
#' A closure-unit family has one row per visit. Its detection-
#' marginalised `expected` and `response` predictions vary by visit.
#' `trend` and `link` belong to the occasion. The response's own family
#' decides: a multivariate fit's top-level family is a placeholder.
#'
#' @param object A fitted `mvgam` object
#' @param type The prediction type
#' @param resp A response name, or NULL
#' @return Logical scalar
#' @noRd
hindcast_per_row <- function(object, type, resp) {
  checkmate::assert_string(type)
  type %in% c("expected", "response") &&
    uses_closure_unit_layout(model_families(object, resp))
}


#' The occasions of `candidate` a series has yet to reach
#'
#' A horizon lies after the last occasion a series was observed at.
#' An unobserved occasion inside the training grid stays out of it.
#'
#' @param candidate Occasions the new frame supplies
#' @param last_time The series' last observed occasion. `NA` for a
#'   series the fit never observed, which has no origin to forecast
#'   from.
#' @return The occasions after `last_time`, sorted
#' @noRd
horizon_beyond <- function(candidate, last_time) {
  checkmate::assert_atomic_vector(candidate)
  checkmate::assert_atomic_vector(last_time, len = 1L)
  if (is.na(last_time)) return(candidate[0L])
  sort(unique(candidate[candidate > last_time]))
}


#' Refuse a frame whose occasions every series has passed
#'
#' The message names the occasions the frame supplied and the last one
#' each series was observed at. Both are what a caller needs to fix
#' the call.
#'
#' @param newdata The new frame
#' @param candidate Occasions the frame supplies
#' @param last_times Last observed occasion of each series, named
#' @param time_var Name of the time column
#' @return Raises an error
#' @noRd
refuse_empty_horizon <- function(newdata, candidate, last_times,
                                 time_var) {
  checkmate::assert_string(time_var)
  origin <- last_times[!is.na(last_times)]
  observed_line <- if (length(origin)) {
    paste0(
      "Last observed: ",
      paste0(names(origin), " at ",
             format(unname(origin), trim = TRUE), collapse = ", "),
      "."
    )
  } else {
    "No series records an occasion it was last observed at."
  }
  stop(insight::format_error(c(
    "'newdata' ends at or before the last training time.",
    x = paste0(
      "Supplied '", time_var, "' values: ",
      paste(format(candidate, trim = TRUE), collapse = ", "), "."
    ),
    x = observed_line,
    i = "A forecast needs 'newdata' times after the training grid.",
    i = paste0("For occasions the model was fitted on, use ",
               "'hindcast()' or 'posterior_predict()'.")
  )), call. = FALSE)
}


#' The forecast horizon a new frame describes
#'
#' The forecast times and observations of each series, with the rows
#' of the frame inside the horizon for the linear predictor calls. A
#' frame whose occasions every series has passed is refused.
#'
#' @param object A fitted `mvgam` object
#' @param newdata The new frame, or NULL
#' @param training The training data from `build_training_arms()`
#' @param series_levels The fitted series
#' @param resp A response name, or NULL
#' @return NULL for no frame. Otherwise a list of `data`, `times`,
#'   `observations` and `time_var`.
#' @noRd
resolve_forecast_grid <- function(object, newdata, training,
                                  series_levels, resp = NULL) {
  if (is.null(newdata)) return(NULL)
  checkmate::assert_data_frame(newdata, min.rows = 1L)
  checkmate::assert_list(training)
  time_var <- training$time_var
  # `axis_row_series()` places the rows on the series. A grouping
  # names the series without a `series` column.
  assert_axis_column(newdata, time_var, "time")
  # `NA > last_time` is `NA`, which would drop the row from the grid
  refuse_missing_times(newdata[[time_var]], time_var)
  # The validator that owns unknown levels gives one message for every
  # route a frame takes
  validate_prediction_factor_levels(
    newdata, object$trend_metadata,
    series_label = series_column_label(object)
  )

  # A wide frame holds one row per time and one column per response.
  # Each series takes every occasion the frame supplies.
  last_times <- stats::setNames(
    extract_last_observed_times(object, length(series_levels)),
    series_levels
  )

  if (is_response_keyed(object)) {
    nt <- sort(unique(newdata[[time_var]]))
    fc_times <- stats::setNames(lapply(series_levels, function(lv) {
      horizon_beyond(nt, last_times[[lv]])
    }), series_levels)
    if (all(lengths(fc_times) == 0L)) {
      refuse_empty_horizon(newdata, nt, last_times, time_var)
    }
    # Each response's observed values come from its own column. A
    # frame holding some response columns gives observations for
    # those alone.
    fc_observations <- stats::setNames(lapply(series_levels, function(lv) {
      column <- response_column(object, lv)
      if (!column %in% names(newdata)) return(NULL)
      idx <- newdata[[time_var]] %in% fc_times[[lv]]
      if (!any(idx)) return(NULL)
      as.numeric(newdata[[column]][idx])[order(newdata[[time_var]][idx])]
    }), series_levels)
    keep <- newdata[[time_var]] %in%
      unique(unlist(fc_times, use.names = FALSE))
    fc_data <- newdata[keep, , drop = FALSE]
    fc_data <- fc_data[
      order(fc_data[[time_var]]), , drop = FALSE
    ]
    return(list(data = fc_data, times = fc_times,
                observations = fc_observations,
                time_var = time_var))
  }

  # The rows take their series as the fit placed them. A grouping
  # defines the series of a hierarchical frame.
  row_ids <- as.character(axis_row_series(object, newdata, required = TRUE))
  refuse_unseen_levels("Series", unique(row_ids), series_levels)
  series_fac <- factor(row_ids, levels = series_levels)

  fc_times <- lapply(series_levels, function(lv) {
    idx <- series_fac == lv
    if (!any(idx)) return(newdata[[time_var]][0L])
    horizon_beyond(newdata[[time_var]][idx], last_times[[lv]])
  })
  names(fc_times) <- series_levels

  if (all(lengths(fc_times) == 0L)) {
    refuse_empty_horizon(newdata, sort(unique(newdata[[time_var]])),
                         last_times, time_var)
  }

  # The truths and the rows of the linear predictor take the sorted
  # order of `fc_times`. `score()` pairs each truth with its horizon by
  # position.
  column <- response_column(object, resp)
  fc_observations <- lapply(series_levels, function(lv) {
    idx <- series_fac == lv &
      newdata[[time_var]] %in% fc_times[[lv]]
    if (!any(idx) || !(column %in% names(newdata))) return(NULL)
    times_lv <- newdata[[time_var]][idx]
    as.numeric(newdata[[column]][idx])[order(times_lv)]
  })
  names(fc_observations) <- series_levels

  keep <- vapply(seq_len(nrow(newdata)), function(i) {
    lv <- as.character(series_fac[i])
    if (is.na(lv)) return(FALSE)
    newdata[[time_var]][i] %in% fc_times[[lv]]
  }, logical(1L))
  fc_data <- newdata[keep, , drop = FALSE]
  fc_data <- fc_data[
    order(
      match(as.character(series_fac[keep]), series_levels),
      fc_data[[time_var]]
    ), ,
    drop = FALSE
  ]

  list(
    data = fc_data,
    times = fc_times,
    observations = fc_observations,
    time_var = time_var
  )
}


#' The last `max_lag` training rows of each series
#'
#' `extract_component_linpred()` takes these rows to give `mu_trend`
#' over the window of the stored state. Each block is sorted by time.
#' `propagate_one_draw()` centres the stored state row by row against
#' this grid.
#'
#' A wide frame's rows hold every series. Its tail is the last
#' occasions of the fit's recorded time axis. Each response's own rows
#' hold its observed occasions alone, which can stop short of the axis.
#'
#' @param object A fitted `mvgam` object
#' @param training The training data from `build_training_arms()`
#' @param max_lag The largest AR or MA lag
#' @return A data frame, or NULL without lags
#' @noRd
build_training_tail_data <- function(object, training, max_lag) {
  checkmate::assert_list(training)
  checkmate::assert_int(max_lag, lower = 0L)
  if (max_lag == 0L) return(NULL)
  series_levels <- names(training$observations)
  time_var <- training$time_var
  if (training$response_keyed) {
    tail_ts <- tail(mvgam_axes(object)$time$values, max_lag)
    sub <- training$data[
      training$data[[time_var]] %in% tail_ts, , drop = FALSE
    ]
    return(sub[order(sub[[time_var]]), , drop = FALSE])
  }

  # The rows of each series as `build_training_arms()` placed them
  rows <- lapply(series_levels, function(lv) {
    tail_ts <- tail(training$times[[lv]], max_lag)
    block <- training$data[training$series_rows[[lv]], , drop = FALSE]
    sub <- block[block[[time_var]] %in% tail_ts, , drop = FALSE]
    sub[order(sub[[time_var]]), , drop = FALSE]
  })
  do.call(rbind, rows)
}


# ----- Hindcasts --------------------------------------------------

#' The hindcast draws of each series
#'
#' `posterior_predict()`, `posterior_epred()` and the component linear
#' predictors give the draws, cut to `draw_idx`.
#'
#' With `process_error = FALSE` the hindcast takes the fitted
#' `trend[t, s]`, the latent states plus `mu_trend`. Response draws of
#' a closely fitted trend then follow the training values. With `TRUE`
#' the marginal prediction draws fresh innovations from the trend's
#' covariance.
#'
#' @param object A fitted `mvgam` object
#' @param training The training data from `build_training_arms()`
#' @param type The prediction type
#' @param draw_idx Indices of the draws to keep
#' @param obs_uncertainty Whether `type = "response"` samples the
#'   family
#' @param process_error Whether to draw fresh trend innovations
#' @param resp A response name, or NULL
#' @param series_levels The series to report
#' @return Named list of draws matrices, one per series
#' @noRd
build_hindcast_arms <- function(object, training, type, draw_idx,
                                obs_uncertainty,
                                process_error = FALSE,
                                resp = NULL,
                                series_levels =
                                  names(training$observations)) {
  checkmate::assert_list(training)
  checkmate::assert_integerish(draw_idx, lower = 1L, min.len = 1L)
  checkmate::assert_flag(obs_uncertainty)
  checkmate::assert_flag(process_error)
  time_var <- training$time_var
  # On a wide frame each series is predicted as its own response
  keyed <- is_response_keyed(object)
  arm_resp <- function(lv) if (keyed) lv else resp
  arm_rows <- training$series_rows[series_levels]

  # A simplex family's expectation is a softmax across the rows of a
  # closure unit, whose rows are the series. One series alone gives a
  # softmax of width one, which is 1 whatever the predictor. The
  # `expected` and `response` draws come from the whole frame, cut
  # afterwards. `trend` and `link` are per-row linear predictors and
  # take the per-series cut.
  if (type %in% c("expected", "response") &&
        is_simplex_response_family(model_families(object, resp))) {
    full <- hindcast_one_series(
      object, training$data, type, draw_idx, obs_uncertainty,
      process_error, resp = resp
    )
    return(lapply(arm_rows, function(rows) full[, rows, drop = FALSE]))
  }

  out <- vector("list", length(series_levels))
  names(out) <- series_levels
  for (s in seq_along(series_levels)) {
    lv <- series_levels[s]
    if (length(training$times[[lv]]) == 0L) {
      out[[s]] <- matrix(NA_real_, nrow = length(draw_idx),
                           ncol = 0L)
      next
    }
    sub <- training$data[arm_rows[[lv]], , drop = FALSE]
    # One row per occasion unless the prediction varies by visit
    if (!hindcast_per_row(object, type, arm_resp(lv))) {
      sub <- sub[!duplicated(sub[[time_var]]), , drop = FALSE]
    }
    out[[s]] <- hindcast_one_series(
      object, sub, type, draw_idx, obs_uncertainty,
      process_error, resp = arm_resp(lv)
    )
  }
  out
}


#' The hindcast draws of one series
#'
#' `extract_trend_latent_states()` gives the fitted `trend[t, s]` of
#' each draw. The observation predictor added to it gives the `link`
#' hindcast. A fit without a trend takes a zero trend. A closure-unit
#' family's `expected` and `response` marginalise over the latent state
#' of each unit. `posterior_epred()` and `posterior_predict()` give
#' those per visit row. `process_error` applies to them alone.
#'
#' @inheritParams build_hindcast_arms
#' @param sub_data The series' rows of the training frame
#' @return Draws matrix, one column per row of `sub_data` or occasion
#' @noRd
hindcast_one_series <- function(object, sub_data, type, draw_idx,
                                obs_uncertainty,
                                process_error = FALSE,
                                resp = NULL) {
  checkmate::assert_data_frame(sub_data)
  checkmate::assert_integerish(draw_idx, lower = 1L, min.len = 1L)
  sample_family <- type == "response" && obs_uncertainty
  if (hindcast_per_row(object, type, resp)) {
    predictor <- if (sample_family) posterior_predict else posterior_epred
    full <- predictor(
      object, newdata = sub_data, draw_ids = draw_idx,
      process_error = process_error, resp = resp
    )
  } else {
    fitted_states <- extract_trend_latent_states(
      mvgam_fit = object, newdata = sub_data,
      full_draws = subset_draws_rows(
        posterior::as_draws_matrix(object$fit), draw_ids = draw_idx
      ),
      resp = resp
    ) %||% matrix(0, nrow = length(draw_idx), ncol = nrow(sub_data))
    obs_linpred <- extract_component_linpred(
      mvgam_fit = object, newdata = sub_data,
      component = "obs", resp = resp, draw_ids = draw_idx
    )
    assert_one_response_draws(obs_linpred, "The observation predictor")
    linpred <- obs_linpred + fitted_states
    full <- if (type == "trend") {
      fitted_states
    } else if (type == "link") {
      linpred
    } else if (sample_family) {
      draw_observations(object, linpred, sub_data, draw_ids = draw_idx,
                        resp = resp)
    } else {
      expected_from_linpred(object, linpred, newdata = sub_data,
                            draw_ids = draw_idx, resp = resp)
    }
  }
  assert_one_response_draws(full, "The hindcast")
  full
}


# ----- Forecasts --------------------------------------------------

#' The forecast draws of each series of a trendless fit
#'
#' The observation predictor at the new occasions, with no state to
#' propagate. The draws take the shape `build_forecast_arms()` gives.
#'
#' @param object A fitted `mvgam` object
#' @param fc_grid The forecast grid
#' @param type "link", "expected" or "response". `forecast.mvgam()`
#'   refuses "trend" for a trendless fit.
#' @param draw_idx Indices of the draws to keep
#' @param obs_uncertainty Whether `type = "response"` samples the
#'   family
#' @param series_levels The series to report
#' @param resp A response name, or NULL
#' @return Named list of draws matrices, one per series
#' @noRd
build_trendless_forecast_arms <- function(object, fc_grid, type,
                                          draw_idx,
                                          obs_uncertainty,
                                          series_levels,
                                          resp = NULL) {
  checkmate::assert_choice(type, c("link", "expected", "response"))
  checkmate::assert_list(fc_grid)
  fc_data <- fc_grid$data
  predictor <- if (type == "link") {
    posterior_linpred
  } else if (type == "response" && obs_uncertainty) {
    posterior_predict
  } else {
    posterior_epred
  }
  # Scoped to one response, which gives one draws matrix
  full <- predictor(object, newdata = fc_data, draw_ids = draw_idx,
                    resp = resp)

  # The rows take their series as the fit placed them. On a wide frame
  # every row belongs to the response this call is for.
  series_fac <- if (is_response_keyed(object)) {
    factor(rep(resp, nrow(fc_data)), levels = series_levels)
  } else {
    axis_row_series(object, fc_data) %||%
      factor(
        fc_data[[axis_vars(object)$series_var]],
        levels = series_levels
      )
  }
  lapply(stats::setNames(nm = series_levels), function(lv) {
    full[, which(series_fac == lv), drop = FALSE]
  })
}


#' The forecast draws of each series
#'
#' Two passes. The first propagates the latent trend `h_max` steps for
#' each draw, from row slices of linear predictors computed once. The
#' second combines the trend draws with the observation predictor and
#' applies the family to every draw at once.
#'
#' A correlated or VAR state moves all of its columns together at each
#' step. The trend advances over every fitted series for that reason.
#' The draws returned cover the `reported` series.
#'
#' @inheritParams build_hindcast_arms
#' @param trend_model The trend specification
#' @param meta The fit's trend metadata
#' @param fc_grid The forecast grid
#' @param draws_mat The fit's draws matrix
#' @param coef_uncertainty,trend_uncertainty Whether coefficients and
#'   trend quantities vary over draws
#' @param series_levels Every fitted series
#' @param reported The series to report
#' @return Named list of draws matrices, one per reported series
#' @noRd
build_forecast_arms <- function(object, trend_model, meta,
                                training, fc_grid, type,
                                draws_mat, draw_idx,
                                coef_uncertainty,
                                trend_uncertainty,
                                obs_uncertainty,
                                series_levels,
                                resp = NULL,
                                reported = series_levels) {
  checkmate::assert_list(fc_grid)
  checkmate::assert_integerish(draw_idx, lower = 1L, min.len = 1L)
  checkmate::assert_flag(coef_uncertainty)
  checkmate::assert_flag(trend_uncertainty)
  checkmate::assert_flag(obs_uncertainty)
  n_series <- length(series_levels)
  ndraws_use <- length(draw_idx)

  max_lag <- as.integer(meta$max_lag %||% 0L)
  tail_data <- build_training_tail_data(object, training, max_lag)
  obs_struct_fc <- get_observation_structure(object,
                                             newdata = fc_grid$data,
                                             resp = resp)
  obs_struct_tail <- if (!is.null(tail_data)) {
    get_observation_structure(object, newdata = tail_data, resp = resp)
  }
  nobs_fc <- length(obs_struct_fc$time)
  # One step per occasion of the forecast grid. Series forecast at
  # their own times make the union longer than any one series.
  h_max <- length(obs_struct_fc$unique_times)
  arms_of <- function(mat) {
    slice_per_series(mat, fc_grid, obs_struct_fc, ndraws_use, reported)
  }

  # The linear predictors are computed once, over every draw, and the
  # loop below takes a row of each per draw. Without
  # `coef_uncertainty` every row repeats the first draw.
  pin_coefs <- function(lp) {
    if (coef_uncertainty || is.null(lp)) {
      return(lp)
    }
    matrix(lp[1L, ], nrow = nrow(lp), ncol = ncol(lp), byrow = TRUE)
  }
  trend_lp_fc <- pin_coefs(extract_component_linpred(
    mvgam_fit = object, newdata = fc_grid$data, component = "trend"
  ))
  trend_lp_tail <- if (!is.null(tail_data)) {
    pin_coefs(extract_component_linpred(
      mvgam_fit = object, newdata = tail_data, component = "trend"
    ))
  }
  obs_full <- if (type != "trend") {
    pin_coefs(extract_component_linpred(
      mvgam_fit = object, newdata = fc_grid$data,
      component = "obs", resp = resp
    ))
  }

  # What the trend's kernel takes from the new times, the same in
  # every draw: CAR's gaps, PW's times and capacities.
  context <- trend_method(
    meta$trend_type, trend_method_optional_templates[["forecast_context"]]
  )(object, training, fc_grid, series_levels)

  # A factor fit propagates its `n_lv` latent factors. Each draw's
  # loadings `Z` project the `[h, n_lv]` path onto the series. A factor
  # fit can have `n_lv = n_series`, under an MGP loadings prior or a
  # `by = lv_axis()` term. `detect_factor_n_lv()` tells the two kinds
  # of fit apart here and in `extract_last_state()`.
  n_lv_trend <- as.integer(
    object$standata$N_lv_trend %||% n_series
  )
  is_factor_fit <- !is.null(detect_factor_n_lv(object, n_series))
  Z_arr <- if (is_factor_fit) {
    resolve_factor_loadings(object, draws_mat, n_lv = n_lv_trend,
                            n_series = n_series, basis = "model")
  } else {
    NULL
  }

  # `trend_flat[i, j]` is the trend of draw `draw_idx[i]` at row `j`
  # of the forecast grid, in the order of `obs_struct_fc`.
  trend_flat <- matrix(NA_real_, nrow = ndraws_use, ncol = nobs_fc)
  for (i in seq_len(ndraws_use)) {
    # Without `trend_uncertainty`, the dynamics, the last state and
    # the trend's mean all come from the first draw. The innovations
    # alone vary.
    d <- if (trend_uncertainty) draw_idx[i] else 1L
    # A single factor drops `Z[d, , ]` to a vector. The reshape keeps
    # it an `[n_series, n_lv]` matrix.
    Z_slice <- if (!is.null(Z_arr)) {
      matrix(Z_arr[d, , ], nrow = n_series, ncol = n_lv_trend)
    }
    fc_trend <- propagate_one_draw(
      object = object,
      trend_model = trend_model,
      meta = meta,
      draws_mat = draws_mat,
      d = d,
      h_max = h_max,
      n_series = n_series,
      trend_lp_tail = trend_lp_tail,
      trend_lp_fc = trend_lp_fc,
      obs_struct_tail = obs_struct_tail,
      obs_struct_fc = obs_struct_fc,
      context = context,
      Z_slice = Z_slice
    )
    trend_flat[i, ] <- flatten_grid_to_obs_order(fc_trend, obs_struct_fc)
  }

  if (type == "trend") {
    return(arms_of(trend_flat))
  }
  eta_full <- obs_full[draw_idx, , drop = FALSE] + trend_flat
  if (type == "link") {
    return(arms_of(eta_full))
  }
  # `eta_full` holds the rows of `draw_idx`. The family's mean and
  # draw take their parameters at those same draws. A second
  # subsample would pair `mu` with a `zi` or `shape` from another
  # draw.
  if (type == "expected" || isTRUE(!obs_uncertainty)) {
    return(arms_of(expected_from_linpred(
      object, eta_full, newdata = fc_grid$data, draw_ids = draw_idx,
      resp = resp
    )))
  }
  arms_of(draw_observations(
    object, eta_full, fc_grid$data, draw_idx, resp = resp
  ))
}


#' The forecast trend of one draw
#'
#' Every generated program builds the trend as
#' `trend[t, s] = Z[s, ] . lv_trend[t, ] + mu_trend[t, s]`. The state
#' advances with mean zero. The trend formula's predictor `mu_trend`
#' adds at the series grain after the loadings project the state. A
#' `by = lv_axis()` fit puts its mean inside the projection.
#' `extract_component_linpred()` returns that mean projected. The sum
#' is linear, which makes the two orders equal.
#'
#' @param object A fitted `mvgam` object
#' @param trend_model The trend specification
#' @param meta The fit's trend metadata
#' @param draws_mat The fit's draws matrix
#' @param d The draw to propagate
#' @param h_max Number of steps
#' @param n_series Number of fitted series
#' @param trend_lp_tail,trend_lp_fc The trend's predictor over the
#'   training tail and the forecast grid, one row per draw. The tail is
#'   NULL without lags.
#' @param obs_struct_tail,obs_struct_fc Their observation structures
#' @param context What the kernel takes from the forecast times
#' @param Z_slice The draw's `[n_series, n_lv]` loadings, or NULL
#' @return Numeric matrix `[h_max, n_series]`
#' @noRd
propagate_one_draw <- function(object, trend_model, meta, draws_mat, d,
                               h_max, n_series,
                               trend_lp_tail, trend_lp_fc,
                               obs_struct_tail, obs_struct_fc,
                               context = list(), Z_slice = NULL) {
  checkmate::assert_int(d, lower = 1L)
  checkmate::assert_int(h_max, lower = 1L)
  checkmate::assert_matrix(Z_slice, nrows = n_series, null.ok = TRUE)
  ls_d <- extract_last_state(object, d, draws_mat = draws_mat)
  max_lag <- as.integer(meta$max_lag %||% 0L)
  lp_forecast <- trend_linpred_grid(
    trend_lp_fc, obs_struct_fc, d, h_max, n_series
  )

  # A factor fit stores the zero-mean `lv_trend` and advances its
  # `n_lv` columns. Every other fit stores `trend[t, s]`, which holds
  # `mu_trend`. Subtracting it leaves both advancing a zero-mean
  # state.
  in_factor_mode <- !is.null(Z_slice) && !is.null(ls_d$n_lv_active)
  if (!in_factor_mode && max_lag > 0L) {
    ls_d$last_state$trends <- ls_d$last_state$trends -
      trend_linpred_grid(trend_lp_tail, obs_struct_tail, d, max_lag,
                         n_series)
  }

  fc_state <- do.call(propagate_trend, c(
    list(trend_model = trend_model, params = ls_d$params, h = h_max,
         n_series = if (in_factor_mode) ls_d$n_lv_active else n_series,
         last_state = ls_d$last_state),
    context
  ))
  if (in_factor_mode) {
    fc_state <- fc_state %*% t(Z_slice)
  }
  fc_state + lp_forecast
}


#' The trend's predictor of one draw as a time-by-series grid
#'
#' `mu_trend` is indexed by series in every generated program. The grid
#' is at the series grain for factor fits too.
#'
#' @param lp_mat The trend's predictor, one row per draw
#' @param obs_struct Observation structure of the rows of `lp_mat`
#' @param draw_row The draw to take
#' @param n_rows Number of occasions to keep, the last ones of the grid
#' @param n_series Number of fitted series
#' @return Numeric matrix `[n_rows, n_series]`
#' @noRd
trend_linpred_grid <- function(lp_mat, obs_struct, draw_row, n_rows,
                               n_series) {
  checkmate::assert_matrix(lp_mat)
  checkmate::assert_int(draw_row, lower = 1L)
  checkmate::assert_int(n_rows, lower = 1L)
  checkmate::assert_int(n_series, lower = 1L)
  grid <- reshape_linpred_to_grid(lp_mat[draw_row, ], obs_struct)
  if (ncol(grid) != n_series) {
    stop(insight::format_error(c(
      "The trend's linear predictor must span every fitted series.",
      x = paste0("The predictor spans ", ncol(grid), " of ", n_series,
                 " series."),
      i = paste0("Give the series column of 'newdata' every series ",
                 "level of the training data.")
    )), call. = FALSE)
  }
  # A CAR history holds each series' last observed occasions. Series
  # observed at their own times give more occasions than lags.
  if (nrow(grid) < n_rows) {
    stop_shape_fault("The trend's predictor grid", nrow(grid), n_rows)
  }
  grid[seq.int(nrow(grid) - n_rows + 1L, nrow(grid)), , drop = FALSE]
}


#' The forecast times every series shares
#'
#' Refuses series forecast at different times. Only `CAR()` steps each
#' series to its own times. An empty entry is a series with nothing to
#' forecast.
#'
#' @param fc_grid The forecast grid
#' @param series_levels Every fitted series
#' @param trend The trend's name, for the message
#' @return The sorted shared times, or `numeric(0)` without any
#' @noRd
shared_forecast_times <- function(fc_grid, series_levels, trend) {
  checkmate::assert_list(fc_grid)
  checkmate::assert_string(trend)
  per_series <- lapply(fc_grid$times[series_levels], function(t) {
    sort(unique(as.numeric(t)))
  })
  filled <- Filter(length, per_series)
  if (length(filled) == 0L) {
    return(numeric(0L))
  }
  differs <- !vapply(filled, function(v) {
    isTRUE(all.equal(v, filled[[1L]]))
  }, logical(1L))
  if (any(differs)) {
    stop(insight::format_error(c(
      paste0("'", trend, "()' forecasts require every series to share ",
             "one set of forecast times."),
      x = paste0("Series '", names(filled)[1L], "' and '",
                 names(filled)[which(differs)[1L]],
                 "' have different times in 'newdata'."),
      i = "Only 'CAR()' forecasts series at their own times."
    )), call. = FALSE)
  }
  filled[[1L]]
}


#' The forecast context of a ZMVN trend
#'
#' Correlated innovations pair the series at one time. The draws are
#' independent over time and need no context.
#'
#' @inheritParams forecast_context_car
#' @return An empty list
#' @noRd
forecast_context_zmvn <- function(object, training, fc_grid,
                                  series_levels) {
  shared_forecast_times(fc_grid, series_levels, "ZMVN")
  list()
}


#' The time gaps of each step of a CAR forecast
#'
#' Every series steps from its state at the last occasion of the fitted
#' grid, over the union of the forecast times. Each series reports its
#' own times. The gaps are in the unit the fit measured its own gaps
#' in. A series whose last responses are missing has a state at the
#' last occasion too.
#'
#' @param object A fitted `mvgam` object
#' @param training The training data, unused here
#' @param fc_grid The forecast grid
#' @param series_levels Every fitted series
#' @return List holding `time`, one gap per step
#' @noRd
forecast_context_car <- function(object, training, fc_grid,
                                 series_levels) {
  checkmate::assert_list(fc_grid)
  time_axis <- mvgam_axes(object)$time
  origin <- max(time_axis$values)
  for (lv in series_levels) {
    fut_t <- fc_grid$times[[lv]]
    if (length(fut_t) > 0L && min(fut_t) <= origin) {
      stop(insight::format_error(c(
        paste0("CAR forecasts begin after the last fitted time, ",
               format(origin, trim = TRUE), "."),
        x = paste0("Series '", lv, "' has 'newdata' times from ",
                   format(min(fut_t), trim = TRUE), "."),
        i = paste0("Use 'hindcast()' or 'posterior_predict()' for times ",
                   "the model was fitted on.")
      )), call. = FALSE)
    }
  }
  horizon <- sort(unique(as.numeric(
    unlist(fc_grid$times[series_levels], use.names = FALSE)
  )))
  list(time = car_scaled_gaps(diff(c(origin, horizon)),
                              car_time_scale(time_axis)))
}


#' What the PW kernel evaluates its closed form at
#'
#' @param object A fitted `mvgam` object
#' @param training The training data from `build_training_arms()`
#' @param fc_grid The forecast grid
#' @param series_levels Every fitted series
#' @return List of `fc_times`, the shared forecast times on the time
#'   index; `training_times`, the observed occasions on that index,
#'   which set the rate of new changepoints; `cap`, the `[h, n_series]`
#'   link-scale capacities of a logistic trend or NULL; and
#'   `changepoint_range`, the share of the history holding changepoints
#' @noRd
forecast_context_pw <- function(object, training, fc_grid,
                                series_levels) {
  checkmate::assert_list(training)
  checkmate::assert_list(fc_grid)
  fc_times <- shared_forecast_times(fc_grid, series_levels, "PW")

  # Stan fits the piecewise trend on the time index, 1 to N_time, and
  # places its changepoints there. The forecast is evaluated on the
  # same index. The user's own units put a fit on years 2001 to 2030
  # at t = 2031 against changepoints near 1 to 30.
  axis_time <- mvgam_axes(object)$time
  to_index <- function(t) {
    (as.numeric(t) - axis_time$values[1L]) / axis_time$step + 1
  }
  # The occasions carrying a response, which is the history
  # `changepoint_range` is a proportion of at fit time
  training_times <- to_index(sort(unique(unlist(training$times))))

  # A logistic PW takes an `[h, n_series]` capacity matrix from
  # newdata. A linear PW has none.
  spec <- first_trend_spec(object)
  cap <- if (pw_is_logistic(spec)) {
    extract_pw_cap_matrix(
      object, fc_grid, spec, fc_times, series_levels,
      family = pw_cap_link_family(formula_families(object, object$family))
    )
  } else {
    NULL
  }

  list(fc_times = to_index(fc_times),
       training_times = training_times,
       cap = cap,
       changepoint_range = object$trend_metadata$pw_changepoint_range)
}


#' The capacity matrix of a logistic PW trend over a forecast
#'
#' The `cap` column of `newdata` at each forecast time and series, as an
#' `[h, n_series]` matrix. The cap is on the response scale.
#' `transform_pw_cap_to_link()` puts it on the link scale, where the
#' kernel evaluates `cap * inv_logit(...)`. With the zero-intercept
#' observation formula of a logistic PW fit, `E[Y]` then saturates at
#' the stated cap.
#'
#' @param object A fitted `mvgam` object
#' @param fc_grid The forecast grid, holding `data` and `time_var`
#' @param spec The `PW()` trend specification
#' @param fc_times Forecast times, one per row of the result
#' @param series_levels Series of the trend, one per column
#' @param family Family whose link the capacity takes
#' @return Numeric matrix on the link scale
#' @noRd
extract_pw_cap_matrix <- function(object, fc_grid, spec, fc_times,
                                  series_levels, family) {
  checkmate::assert_list(fc_grid, names = "named")
  checkmate::assert_vector(fc_times, min.len = 1L, any.missing = FALSE)
  checkmate::assert_vector(series_levels, min.len = 1L)
  cap_var <- pw_cap_var(spec)
  d <- fc_grid$data
  if (!(cap_var %in% names(d))) {
    stop(insight::format_error(c(
      paste0("A logistic 'PW()' forecast requires a '", cap_var,
             "' column in 'newdata'."),
      i = "The capacity at each forecast time sets the trend's ceiling."
    )), call. = FALSE)
  }
  # The grid's own time column matches the forecast times exactly.
  # The series come from the axes the fit placed each row on.
  time_var <- fc_grid$time_var
  series_ids <- axis_row_series(object, d)
  row_series <- if (is.null(series_ids)) {
    as.character(d[[axis_vars(object)$series_var]])
  } else {
    as.character(series_ids)
  }
  cap_mat <- matrix(NA_real_, nrow = length(fc_times),
                    ncol = length(series_levels))
  for (s in seq_along(series_levels)) {
    for (k in seq_along(fc_times)) {
      ix <- which(row_series == series_levels[s] &
                    d[[time_var]] == fc_times[k])
      if (length(ix) > 0L) {
        cap_mat[k, s] <- as.numeric(d[[cap_var]][ix[1L]])
      }
    }
  }
  if (anyNA(cap_mat)) {
    stop(insight::format_error(c(
      paste0("A logistic 'PW()' forecast needs '", cap_var,
             "' for every series at every forecast time."),
      i = paste0("Add the missing '", cap_var, "' values to 'newdata'.")
    )), call. = FALSE)
  }
  transform_pw_cap_to_link(cap_mat, family, cap_var)
}

#' A response-scale capacity on the link scale
#'
#' Applies the family's link to the capacity matrix, at fit time and at
#' forecast time.
#'
#' @param cap_mat Numeric matrix of capacities on the response scale
#' @param family Family whose link the capacity takes
#' @param cap_var Name of the capacity column, for the message
#' @return `cap_mat` on the link scale
#' @noRd
transform_pw_cap_to_link <- function(cap_mat, family, cap_var = "cap") {
  checkmate::assert_matrix(cap_mat, mode = "numeric")
  checkmate::assert_function(family$linkfun, .var.name = "family$linkfun")
  checkmate::assert_string(cap_var, min.chars = 1L)
  out <- family$linkfun(cap_mat)
  if (any(!is.finite(out))) {
    stop(insight::format_error(c(
      paste0("The '", family$link, "' link maps some '", cap_var,
             "' values to infinity."),
      i = paste0("Give '", cap_var, "' values inside the range of the ",
                 "response.")
    )), call. = FALSE)
  }
  out
}

#' The family whose link puts a logistic PW capacity on the trend scale
#'
#' The one latent trend enters every response's linear predictor. The
#' capacity, given on the response scale, reaches it through a link.
#' Responses on different links would put the trend on different
#' scales, which no single capacity describes.
#'
#' @param families List of the observation families
#' @return The first family, once every link is shown to agree
#' @noRd
pw_cap_link_family <- function(families) {
  checkmate::assert_list(families, min.len = 1L)
  links <- unique(vapply(families, function(f) f$link, character(1L)))
  if (length(links) > 1L) {
    stop(insight::format_error(c(
      "A logistic 'PW()' trend needs every response on one link.",
      x = paste0("The responses use the links ",
                 paste0("'", links, "'", collapse = ", "), "."),
      i = paste0("The capacity is on the response scale and enters ",
                 "the shared trend through the link."),
      i = paste0("Give the responses families with one link or use a ",
                 "linear 'PW()' trend.")
    )), call. = FALSE)
  }
  families[[1L]]
}


#' Forecast draws cut into one matrix per series
#'
#' @param mat Draws matrix `[ndraws_use, nobs_fc]`, one column per row
#'   of the forecast grid in the order of `obs_struct`
#' @param fc_grid The forecast grid
#' @param obs_struct Observation structure of the forecast grid
#' @param ndraws_use Number of draws
#' @param series_levels The series to report
#' @return Named list of `[ndraws_use, h_s]` matrices, one column per
#'   forecast time of each series
#' @noRd
slice_per_series <- function(mat, fc_grid, obs_struct,
                             ndraws_use, series_levels) {
  checkmate::assert_matrix(mat, nrows = ndraws_use)
  # The forecast times come from this column too. Both sides hold the
  # same doubles. Labels printed from them lose digits.
  raw_times <- as.numeric(fc_grid$data[[fc_grid$time_var]])
  out <- lapply(series_levels, function(lv) {
    ts <- fc_grid$times[[lv]]
    # Found by name on the fit's axis. A forecast of one response of
    # a wide frame reports fewer series than the axis holds.
    rows <- which(
      obs_struct$series_int == match(lv, obs_struct$series_levels)
    )
    cells <- rows[match(as.numeric(ts), raw_times[rows])]
    if (anyNA(cells)) {
      stop_mvgam_fault(
        "Every forecast occasion needs a row in the forecast grid.",
        paste0("Series '", lv, "' at ",
               format(ts[is.na(cells)][1L], trim = TRUE),
               " is absent from the grid.")
      )
    }
    mat[, cells, drop = FALSE]
  })
  names(out) <- series_levels
  out
}


#' The family's parameter draws of a `type = "link"` forecast
#'
#' The rows follow `draw_idx`, the rows of the hindcasts and forecasts.
#'
#' @param object A fitted `mvgam` object
#' @param draws_mat The fit's draws matrix
#' @param draw_idx Indices of the draws to keep
#' @param resp A response name, or NULL
#' @return Named list of draws matrices, one per sampled parameter
#' @noRd
extract_family_pars_for_draws <- function(object, draws_mat,
                                          draw_idx, resp = NULL) {
  checkmate::assert_integerish(draw_idx, lower = 1L, min.len = 1L)
  # Each response of a multivariate fit has its own family and
  # distributional parameters
  fam_name <- resolve_family_name(model_families(object, resp))
  dpar_names <- get_family_dpars(fam_name)
  if (length(dpar_names) == 0L) return(list())
  # A response of a model with several carries its parameters under
  # its own key, as in `sigma_y1`.
  suffix <- response_suffix(object, resp)
  out <- list()
  for (nm in dpar_names) {
    pat <- paste0("^", nm, suffix, "(\\[|$)")
    cols <- grep(pat, colnames(draws_mat), value = TRUE)
    if (length(cols) == 0L) next
    # Ordered by index. `sigma[10]` sorts before `sigma[2]` by name.
    if (all(grepl("\\[[0-9]+\\]$", cols))) {
      cols <- cols[order(as.integer(stan_index(cols)))]
    }
    out[[nm]] <- draws_mat[draw_idx, cols, drop = FALSE]
  }
  out
}


#' Fault on draws that span several responses
#'
#' A hindcast scoped to one response holds one draws matrix.
#'
#' @param draws The draws to check
#' @param what What the draws are, for the message
#' @return `TRUE`, invisibly
#' @noRd
assert_one_response_draws <- function(draws, what) {
  checkmate::assert_string(what)
  if (is.list(draws) && !is.matrix(draws)) {
    stop_mvgam_fault(
      paste0(what, " must be one draws matrix."),
      paste0("Got a list over responses: ",
             paste0("'", names(draws), "'", collapse = ", "), ".")
    )
  }
  invisible(TRUE)
}
