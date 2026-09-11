#' marginaleffects S3 methods for mvgam objects
#'
#' Hook mvgam fits into the `marginaleffects` ecosystem. The current
#' marginaleffects API (>= 0.20) requires only `get_predict`,
#' `get_coef`, `get_vcov` and `set_coef`. Bayesian models bypass the
#' delta-method path and use the `posterior_draws` attribute attached
#' by `get_predict`, so `get_coef` / `get_vcov` / `set_coef` exist
#' only to satisfy the S3 surface. Neither predictions nor slopes
#' read them.
#'
#' @section Prediction scale:
#' The `type` tokens mvgam registers differ from the ones other model
#' classes use, so it is worth naming them. `"expected"` is `E[Y]`,
#' the quantity most classes reach through `"response"`; `"response"`
#' here is the outcome scale, carrying observation noise, and returns
#' integers for a count family. `"link"` is the linear predictor.
#' marginaleffects orders `"response"` first and takes its default
#' from that, so pass `type = "expected"` when the expectation is what
#' is wanted. Results are summarised by the posterior median, which
#' for a skewed predictive sits below the mean.
#'
#' @section The latent trend:
#' `process_error = FALSE`, the default, leaves the trend at its
#' deterministic submodel, which is the counterfactual these surfaces
#' are usually read for: a covariate effect with the latent process
#' held out, drawn against a credible band that stays legible. It does
#' not fix the trend at a posterior mean, and it does not condition on
#' the state the model inferred; every coefficient still varies draw
#' to draw.
#'
#' The cost is that a slope or a comparison taken this way is biased
#' low against the fitted model's own `E[Y]`, by
#' \eqn{\exp(\sigma^2 / (2(1 - \rho^2)))} on a log link, since the
#' trend's stationary spread is left out of the expectation being
#' differentiated. Pass `process_error = TRUE` for the unbiased
#' effect; the band widens with it, because the latent state's own
#' uncertainty is then part of the answer.
#'
#' A fit whose covariates carry little of the signal, with most of the
#' series-level variation in a strong autoregressive process, has
#' little for either setting to show. Read such a model through
#' [hindcast.mvgam()] and [forecast.mvgam()] instead, which return the
#' state the model inferred rather than a covariate counterfactual.
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
                              process_error = FALSE) {
  checkmate::assert_class(model, "mvgam")
  checkmate::assert_data_frame(newdata, min.rows = 1L)
  checkmate::assert_logical(process_error, len = 1L)
  # `marginaleffects::datagrid()` drops every column the model
  # formula does not reference, so a closure-unit family's grid
  # arrives without the identifiers its per-unit pipeline needs and
  # they are filled in below with defaults. A frame the caller
  # assembled keeps its own.
  #
  # Which of the two this is gets settled once, here, on the frame
  # as it arrived: the completion stamps the very columns the
  # question is asked of, so asking again afterwards would answer
  # about mvgam's own stamping rather than about the caller.
  is_grid <- !closure_units_are_intact(model, newdata)
  newdata <- complete_closure_unit_newdata(model, newdata, is_grid)
  # A composition is a property of a whole site, so its grid is
  # completed to whole sites, predicted, and the asked-for rows
  # taken back. `asked` stays NULL for every other family, and the
  # grid the caller supplied is what the result is reported against.
  asked <- NULL
  grid <- newdata
  completed <- complete_simplex_grid(model, newdata, is_grid)
  if (!is.null(completed)) {
    newdata <- completed$data
    asked <- completed$take
  }
  # Type vocabulary at the marginaleffects boundary. marginaleffects
  # validates `type` against its shipped per-class `type_dictionary`
  # (upstream of this method), which for the mvgam class permits
  # `response`, `link`, `expected`, `detection` and `latent_N`. The
  # tokens map to mvgam's prediction scales:
  #   link     - linear predictor on the link scale
  #   expected - E[Y], expectation of the response (epred); no
  #              observation-process noise
  #   response - outcome-scale draws WITH observation-process noise
  #              (posterior_predict). Integer for count families.
  # `latent_N` / `detection` are closure-unit-only family types that
  # delegate to predict.mvgam, which runs the family-availability gate
  # and dispatches to the per-family kernel (posterior_occupancy /
  # posterior_latent_N / posterior_detection). `latent_N` is the
  # marginaleffects wire token for the latent state; mvgam's own
  # user-facing token for the same quantity is `latent_state`, which
  # is accepted here as an alias for direct `get_predict()` calls that
  # bypass the dictionary check.
  checkmate::assert_choice(
    type,
    c("response", "link", "expected", "detection",
      "latent_N", "latent_state")
  )

  # Default process_error = FALSE leaves the latent process out, so a
  # slope or comparison reads the covariate structure of both
  # submodels alone. Passing `process_error = TRUE` through
  # `predictions()` integrates over the trend's dynamics instead.
  draws <- switch(type,
    link       = posterior_linpred(model, newdata = newdata,
                                   process_error = process_error, ...),
    expected   = posterior_epred(model, newdata = newdata,
                                  process_error = process_error, ...),
    response   = posterior_predict(model, newdata = newdata,
                                    process_error = process_error, ...),
    latent_N     = predict(model, newdata = newdata,
                           type = "latent_state",
                           summary = FALSE, ...),
    latent_state = predict(model, newdata = newdata,
                           type = "latent_state",
                           summary = FALSE, ...),
    detection    = predict(model, newdata = newdata,
                            type = "detection",
                            summary = FALSE, ...)
  )

  # Multivariate fits return a per-response list; marginaleffects
  # expects a single matrix per call. Pass `resp = "<name>"` through
  # the `...` of `predictions()` etc. to evaluate one response.
  if (is.list(draws) && !is.matrix(draws)) {
    stop(insight::format_error(c(
      cli::format_inline(paste0(
        "marginaleffects for multivariate mvgam fits requires ",
        "{.field resp}."
      )),
      i = paste0(
        "Pass `resp = '<response-name>'` (one of ",
        paste(shQuote(names(draws)), collapse = ", "),
        ") to evaluate a single response."
      )
    )))
  }

  # A closure unit's latent state is one value the unit's rows share,
  # so it is read at the grain this method pairs with `newdata`.
  # Applied before the slice below, which indexes rows of the grid.
  draws <- closure_unit_draws_to_rows(model, newdata, draws)

  # Back to the grid the caller asked about, now that the whole-site
  # prediction has been made over the completed one.
  if (!is.null(asked)) {
    draws <- if (length(dim(draws)) == 3L) {
      draws[, asked, , drop = FALSE]
    } else {
      draws[, asked, drop = FALSE]
    }
    newdata <- grid
  }

  rowid <- newdata[["rowid"]] %||% seq_len(nrow(newdata))

  # marginaleffects's downstream operations (slopes, comparisons) use
  # data.table syntax (`out[, lo := ..., by = "group"]`); the brmsfit
  # reference method returns a data.table for the same reason.
  # data.table is in marginaleffects's Imports so it is always
  # available at the call site, but check defensively for the rare
  # case where get_predict.mvgam is invoked outside that pipeline.
  insight::check_if_installed(
    "data.table",
    reason = paste0(
      "to build the data.table get_predict() returns ",
      "for `marginaleffects::slopes()` / `comparisons()`."
    )
  )
  if (length(dim(draws)) == 2L) {
    if (nrow(newdata) != ncol(draws)) {
      stop(insight::format_error(c(
        "Dimension mismatch between predicted columns and newdata rows.",
        x = cli::format_inline(
          "Draws have {ncol(draws)} columns; newdata has {nrow(newdata)} rows."
        ),
        i = "Pass the same `newdata` that produced the draws."
      )))
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
    # Ordinal / categorical epred: [ndraws x nobs x ncat]. Collapse to
    # one row per (obs, category). Prefer the response factor levels
    # for category labels (matches `get_group_names.mvgam`), then the
    # draws array dimnames, then integer indices as a last resort.
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

  stop(insight::format_error(c(
    "Unexpected posterior draws shape for mvgam.",
    x = cli::format_inline(
      "Got dim length {length(dim(draws))}; expected 2 or 3."
    )
  )))
}


#' @importFrom marginaleffects get_coef
#' @export
get_coef.mvgam <- function(model, trend_effects = FALSE, ...) {
  checkmate::assert_class(model, "mvgam")
  checkmate::assert_logical(trend_effects, len = 1L)

  if (trend_effects && is.null(model$trend_formula)) {
    stop(insight::format_error(
      "No trend formula on this model; trend coefficients do not exist."
    ))
  }

  # Read the population block through the same `betas` /
  # `trend_betas` keywords `coef()`, `fixef()` and `vcov()` use, so
  # the four accessors cannot disagree about which coefficients
  # belong to which side. Reading the raw stanfit with a `^b_`
  # pattern of its own returned `b_Intercept` alone on every model
  # carrying a slope, because brms writes the population block as
  # the indexed array `b[k]` and only the intercept is spelled out.
  draws <- as_draws_matrix(
    model, variable = if (trend_effects) "trend_betas" else "betas"
  )
  if (ncol(draws) == 0L) {
    return(setNames(numeric(0L), character(0L)))
  }
  setNames(colMeans(draws), colnames(draws))
}


#' @importFrom marginaleffects get_vcov
#' @export
get_vcov.mvgam <- function(model, vcov = NULL, ...) {
  # `marginaleffects` passes `vcov = TRUE` on every call it makes,
  # meaning "the model's own uncertainty", which for a Bayesian fit
  # is the posterior it already uses. Warning on that named an
  # argument the caller never set, on the ordinary path of every
  # `predictions()`, `slopes()` and `comparisons()` call. A vcov
  # *estimator* is the request mvgam cannot honour, and
  # `get_vcov.brmsfit` draws the line in the same place.
  if (!is.null(vcov) && !is.logical(vcov)) {
    rlang::warn(insight::format_warning(
      paste0(
        "The `vcov` argument is not supported for mvgam objects; ",
        "uncertainty propagation uses posterior draws."
      )
    ))
  }
  NULL
}


#' @importFrom marginaleffects set_coef
#' @export
set_coef.mvgam <- function(model, coefs, ...) {
  # Bayesian uncertainty propagation goes through the posterior_draws
  # attribute on get_predict; marginaleffects never reads a
  # coefficient override set on the fitted object.
  model
}


# `get_group_names` is the marginaleffects hook that labels the
# `group` column of the returned data.frame. For non-categorical /
# non-ordinal models it returns "main_marginaleffect". For ordinal
# (cumulative / sratio / cratio / acat) models we return the
# response factor levels so the per-category rows in the 3D ordinal
# epred carry their actual labels instead of integer indices.
#' @importFrom marginaleffects get_group_names
#' @export
get_group_names.mvgam <- function(model, ...) {
  if (!is_ordinal_family(model$family)) {
    return("main_marginaleffect")
  }
  # Both facts through the accessors that own them: the response
  # column off the formula, the frame off the fit. Reading `$data`
  # alone answers `NULL` for a fit that stores its frame as
  # `obs_data`, and the categories then come back as integers with
  # nothing saying the labels were lost. A model with several
  # responses has no one set of categories, and marginaleffects asks
  # this once per model rather than per response.
  columns <- response_columns(model)
  if (length(columns) != 1L) {
    return("main_marginaleffect")
  }
  y <- mvgam_training_data(model)[[columns[[1L]]]]
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
