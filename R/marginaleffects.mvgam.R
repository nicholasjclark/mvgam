#' marginaleffects methods for mvgam objects
#'
#' These methods let [marginaleffects::predictions()],
#' [marginaleffects::slopes()] and [marginaleffects::comparisons()]
#' take an mvgam fit. `get_predict()` returns the posterior draws in
#' the `posterior_draws` attribute. marginaleffects computes every
#' summary from those draws. `get_coef()`, `get_vcov()` and
#' `set_coef()` complete the methods marginaleffects calls. The draws
#' carry the uncertainty. None of the three changes a result.
#'
#' @section Prediction scale:
#' mvgam registers three `type` values with marginaleffects:
#' \itemize{
#'   \item `"expected"`: the expectation `E[Y]`. Most other model
#'     classes call this `"response"`.
#'   \item `"response"`: predictive draws with observation noise, on
#'     the outcome scale. A count family gives integers.
#'   \item `"link"`: the linear predictor.
#' }
#' marginaleffects takes `"response"` as the default. Pass
#' `type = "expected"` for the expectation. `comparisons()` and
#' `slopes()` replace `"response"` with `"expected"`. A contrast
#' differences two predictions. Observation noise in each would widen
#' it. A slope also divides that noise by a small step. The result
#' keeps the label `"response"`. `predictions()` on `"response"` keeps
#' the observation noise. marginaleffects summarises the draws by
#' their posterior median. For a right-skewed predictive distribution
#' the median falls below the mean.
#'
#' @section The latent trend:
#' With `process_error = FALSE`, the default, the trend contributes its
#' deterministic submodel. Each draw keeps its own coefficients. The
#' result is a covariate effect with the latent process held out. Its
#' credible band excludes the uncertainty of the latent state.
#'
#' A slope or comparison taken this way is biased low against the
#' model's own `E[Y]`. Its expectation leaves out the trend's
#' stationary spread. Under a log link and an AR(1) trend the factor
#' is \eqn{\exp(\sigma^2 / (2(1 - \rho^2)))}. `process_error = TRUE`
#' gives the unbiased effect. Its band is wider because it includes
#' the uncertainty of the latent state.
#'
#' When an autoregressive process carries most of the series-level
#' variation, a covariate effect shows little under either setting.
#' [hindcast.mvgam()] and [forecast.mvgam()] return the state the
#' model inferred.
#'
#' @name mvgam_marginaleffects
#'
#' @references
#' Arel-Bundock, V., Greifer, N. and Heiss, A. (2024). How to
#' interpret statistical models using marginaleffects for R and
#' Python. \emph{Journal of Statistical Software}, 111(9):1-32.
#' \doi{10.18637/jss.v111.i09}
#'
#' @keywords internal
NULL


#' @importFrom marginaleffects get_predict
#' @export
get_predict.mvgam <- function(model,
                              newdata = insight::get_data(model),
                              type = "response",
                              ...,
                              ndraws = NULL,
                              draw_ids = NULL,
                              re_formula = NULL,
                              allow_new_levels = FALSE,
                              sample_new_levels = "uncertainty",
                              incl_autocor = FALSE,
                              resp = NULL,
                              process_error = FALSE) {
  checkmate::assert_class(model, "mvgam")
  checkmate::assert_data_frame(newdata, min.rows = 1L)
  # conditional_effects() sets the response and the trend setting on
  # its copy of the model. marginaleffects would warn on both as
  # arguments.
  preset <- attr(model, "mvgam_predict_args", exact = TRUE)
  if (!is.null(preset)) {
    resp <- resp %||% preset$resp
    process_error <- preset$process_error
  }
  checkmate::assert_logical(process_error, len = 1L)
  # `marginaleffects::datagrid()` drops every column the formula does
  # not name. A closure-unit family's grid then lacks the identifiers
  # its per-unit pipeline needs. The completion fills them with
  # defaults. A frame the caller assembled keeps its own. The test
  # runs once, on the frame as it arrived. The completion writes the
  # columns the test inspects.
  is_grid <- !closure_units_are_intact(model, newdata)
  newdata <- jsdgam_axis_columns(model, newdata)
  newdata <- complete_closure_unit_newdata(model, newdata, is_grid)
  # A composition belongs to a whole site. Its grid is completed to
  # whole sites and predicted. The rows asked for are then taken back.
  # `asked` stays NULL for every other family.
  asked <- NULL
  grid <- newdata
  completed <- complete_simplex_grid(model, newdata, is_grid)
  if (!is.null(completed)) {
    newdata <- completed$data
    asked <- completed$take
  }
  # marginaleffects checks `type` against its own dictionary, which
  # names the latent state `latent_N`. `latent_state` serves a direct
  # call.
  checkmate::assert_choice(
    type,
    c("response", "link", "expected", "detection",
      "latent_N", "latent_state")
  )
  # The contrast or slope of the response is that of its expectation.
  # The observation noise of each prediction would only widen it.
  if (identical(type, "response") && called_from_comparisons()) {
    type <- "expected"
  }

  # marginaleffects passes bookkeeping of its own through `...`, such
  # as `mfx` and `modeldata`. The prediction methods take the
  # arguments named here alone.
  shared <- list(
    object = model,
    newdata = newdata,
    ndraws = ndraws,
    draw_ids = draw_ids,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels,
    incl_autocor = incl_autocor,
    resp = resp,
    process_error = process_error
  )
  draws <- switch(type,
    link         = do.call(posterior_linpred, shared),
    expected     = do.call(posterior_epred, shared),
    response     = do.call(posterior_predict, shared),
    latent_N     = ,
    latent_state = expected_latent_state(
      model, newdata, resolve_draw_ids(model, ndraws, draw_ids)
    ),
    detection    = do.call(
      predict, c(shared, list(type = "detection", summary = FALSE))
    )
  )

  if (is_per_response(draws)) {
    stop(insight::format_error(c(
      "marginaleffects takes one response of a multivariate fit.",
      i = paste0("Pass 'resp' as one of ",
                 paste0("'", names(draws), "'", collapse = ", "), ".")
    )), call. = FALSE)
  }

  # A closure unit's latent state is one value its rows share. It is
  # mapped to the rows of `newdata` before the slice below indexes
  # them.
  draws <- closure_unit_draws_to_rows(model, newdata, draws)

  if (!is.null(asked)) {
    draws <- if (length(dim(draws)) == 3L) {
      draws[, asked, , drop = FALSE]
    } else {
      draws[, asked, drop = FALSE]
    }
    newdata <- grid
  }

  rowid <- newdata[["rowid"]] %||% seq_len(nrow(newdata))

  # marginaleffects computes slopes and comparisons with data.table
  # syntax on the returned table.
  if (length(dim(draws)) == 2L) {
    if (nrow(newdata) != ncol(draws)) {
      stop(insight::format_error(c(
        "The draws need one column per row of 'newdata'.",
        x = paste0("The draws have ", ncol(draws), " columns and ",
                   "'newdata' has ", nrow(newdata), " rows."),
        i = "Pass the same 'newdata' that produced the draws."
      )), call. = FALSE)
    }
    out <- data.table::data.table(
      rowid = rowid,
      group = "main_marginaleffect",
      estimate = apply(draws, 2L, stats::median)
    )
    attr(out, "posterior_draws") <- t(draws)
    return(out)
  }

  if (length(dim(draws)) == 3L) {
    # An ordinal or categorical expectation is `[ndraws, nobs, ncat]`.
    # It becomes one row per observation and category. The labels are
    # the response levels of `get_group_names.mvgam()`, else the
    # array's dimnames, else the category numbers.
    med <- apply(draws, c(2L, 3L), stats::median)
    ncat <- dim(draws)[3L]
    cat_names <- get_group_names(model)
    if (is.null(cat_names) || length(cat_names) != ncat ||
        identical(cat_names, "main_marginaleffect")) {
      cat_names <- dimnames(draws)[[3L]] %||% as.character(seq_len(ncat))
    }
    out <- data.table::data.table(
      rowid = rep(rowid, times = length(cat_names)),
      group = rep(cat_names, each = nrow(med)),
      estimate = as.vector(med)
    )
    flat <- do.call(
      cbind,
      lapply(seq_len(dim(draws)[3L]), function(i) draws[, , i])
    )
    attr(out, "posterior_draws") <- t(flat)
    return(out)
  }

  stop_mvgam_fault(
    "Posterior draws must be a matrix or a three-dimensional array.",
    paste0("Got ", length(dim(draws)), " dimension(s).")
  )
}


#' Whether `marginaleffects::comparisons()` is on the call stack
#'
#' The `comparisons()` and `slopes()` families, `avg_` and `plot_`
#' forms included, all evaluate through `comparisons()`. Each frame's
#' function is compared with it, whatever name the caller used.
#'
#' @return Logical
#' @noRd
called_from_comparisons <- function() {
  comparisons_fn <- marginaleffects::comparisons
  for (i in seq_len(sys.nframe())) {
    if (identical(sys.function(i), comparisons_fn)) {
      return(TRUE)
    }
  }
  FALSE
}


#' @importFrom marginaleffects get_coef
#' @export
get_coef.mvgam <- function(model, trend_effects = FALSE, ...) {
  checkmate::assert_class(model, "mvgam")
  draws <- population_draws(model, trend_effects)
  if (ncol(draws) == 0L) {
    return(setNames(numeric(0L), character(0L)))
  }
  setNames(colMeans(draws), colnames(draws))
}


#' @importFrom marginaleffects get_vcov
#' @export
get_vcov.mvgam <- function(model, vcov = NULL, ...) {
  # marginaleffects passes `vcov = TRUE` on every call. `TRUE` means
  # the model's own uncertainty, which the posterior draws carry. A
  # named estimator is refused with a warning.
  if (!is.null(vcov) && !is.logical(vcov)) {
    insight::format_warning(c(
      "The 'vcov' argument is not supported for mvgam objects.",
      i = "Uncertainty is propagated through the posterior draws."
    ))
  }
  NULL
}


#' @importFrom marginaleffects set_coef
#' @export
set_coef.mvgam <- function(model, coefs, ...) {
  # marginaleffects takes the uncertainty from the `posterior_draws`
  # attribute of get_predict(). Coefficients set here go unused.
  model
}


# Labels for the `group` column of the table get_predict() returns.
# An ordinal model has one row per category, labelled by the levels
# of its response. Every other model has the one group
# "main_marginaleffect".
#' @importFrom marginaleffects get_group_names
#' @export
get_group_names.mvgam <- function(model, ...) {
  if (!is_ordinal_family(model$family)) {
    return("main_marginaleffect")
  }
  # The formula names the response column. A model with several
  # responses has several sets of categories. marginaleffects takes
  # one set per model.
  columns <- response_columns(model)
  if (length(columns) != 1L) {
    return("main_marginaleffect")
  }
  y <- model$data[[columns[[1L]]]]
  if (is.factor(y)) {
    return(levels(y))
  }
  as.character(sort(unique(y)))
}


# Re-exports of the user-facing marginaleffects entry points so
# `library(mvgam); predictions(fit)` works without separately
# attaching marginaleffects.

#' @importFrom marginaleffects predictions
#' @export
marginaleffects::predictions

#' @importFrom marginaleffects avg_predictions
#' @export
marginaleffects::avg_predictions

#' @importFrom marginaleffects plot_predictions
#' @export
marginaleffects::plot_predictions

#' @importFrom marginaleffects slopes
#' @export
marginaleffects::slopes

#' @importFrom marginaleffects avg_slopes
#' @export
marginaleffects::avg_slopes

#' @importFrom marginaleffects plot_slopes
#' @export
marginaleffects::plot_slopes

#' @importFrom marginaleffects comparisons
#' @export
marginaleffects::comparisons

#' @importFrom marginaleffects avg_comparisons
#' @export
marginaleffects::avg_comparisons

#' @importFrom marginaleffects plot_comparisons
#' @export
marginaleffects::plot_comparisons

#' @importFrom marginaleffects datagrid
#' @export
marginaleffects::datagrid

#' @importFrom marginaleffects hypotheses
#' @export
marginaleffects::hypotheses


# Re-export the generic from brms so `library(mvgam)` is enough for
# users to call `conditional_effects(mvgam_fit)` directly.
#' @importFrom brms conditional_effects
#' @export
brms::conditional_effects
