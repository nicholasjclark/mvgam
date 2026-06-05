#' marginaleffects S3 methods for mvgam objects
#'
#' Hook mvgam fits into the `marginaleffects` ecosystem. The current
#' marginaleffects API (>= 0.20) requires only `get_predict`,
#' `get_coef`, `get_vcov` and `set_coef`. Bayesian models bypass the
#' delta-method path and use the `posterior_draws` attribute attached
#' by `get_predict`, so `get_coef` / `get_vcov` / `set_coef` exist
#' mostly to satisfy the S3 surface — they are not load-bearing for
#' predictions or slopes.
#'
#' @name mvgam_marginaleffects
#' @keywords internal
NULL


#' @importFrom marginaleffects get_predict
#' @export
get_predict.mvgam <- function(model,
                              newdata = insight::get_data(model),
                              type = "response",
                              process_error = FALSE,
                              ...) {
  checkmate::assert_class(model, "mvgam")
  checkmate::assert_data_frame(newdata, min.rows = 1L)
  checkmate::assert_logical(process_error, len = 1L)
  # mvgam's predict() type vocabulary (carried over from master):
  #   link     - linear predictor on the link scale
  #   expected - E[Y], expectation of the response (epred); no
  #              observation-process noise
  #   response - outcome-scale draws WITH observation-process noise
  #              (posterior_predict). Integer for count families.
  # `prediction` is accepted as a brms-style alias for `response`.
  checkmate::assert_choice(
    type, c("response", "link", "expected", "prediction")
  )

  # Default process_error = FALSE collapses the latent trend to its
  # posterior mean for slopes / comparisons / predictions. Users who
  # want per-draw latent-state uncertainty can pass
  # `process_error = TRUE` through `predictions()`.
  draws <- switch(type,
    link       = posterior_linpred(model, newdata = newdata,
                                   process_error = process_error, ...),
    expected   = posterior_epred(model, newdata = newdata,
                                  process_error = process_error, ...),
    response   = posterior_predict(model, newdata = newdata,
                                    process_error = process_error, ...),
    prediction = posterior_predict(model, newdata = newdata,
                                    process_error = process_error, ...)
  )

  # Multivariate fits return a per-response list; marginaleffects
  # expects a single matrix per call. Pass `resp = "<name>"` through
  # the `...` of `predictions()` etc. to evaluate one response.
  if (is.list(draws) && !is.matrix(draws)) {
    stop(insight::format_error(c(
      "marginaleffects for multivariate mvgam fits requires {.field resp}.",
      i = paste0(
        "Pass `resp = '<response-name>'` (one of ",
        paste(shQuote(names(draws)), collapse = ", "),
        ") to evaluate a single response."
      )
    )))
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
    cat_names <- tryCatch(get_group_names(model), error = function(e) NULL)
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

  draws <- posterior::as_draws_matrix(model$fit)
  # Trend submodel coefficients carry the `_trend` token after the
  # leading `b_` prefix (e.g. `b_trend_x` vs `b_x` for obs).
  pattern <- if (trend_effects) "^b_trend_" else "^b_(?!trend_)"
  cols <- grep(pattern, colnames(draws), value = TRUE, perl = TRUE)
  if (length(cols) == 0L) {
    return(setNames(numeric(0L), character(0L)))
  }
  setNames(colMeans(draws[, cols, drop = FALSE]), cols)
}


#' @importFrom marginaleffects get_vcov
#' @export
get_vcov.mvgam <- function(model, vcov = NULL, ...) {
  if (!is.null(vcov) && !isFALSE(vcov)) {
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      rlang::warn(
        paste0(
          "The `vcov` argument is not supported for mvgam objects; ",
          "uncertainty propagation uses posterior draws."
        ),
        .frequency = "once",
        .frequency_id = "mvgam_get_vcov_vcov"
      )
    }
  }
  NULL
}


#' @importFrom marginaleffects set_coef
#' @export
set_coef.mvgam <- function(model, coefs, ...) {
  # Bayesian uncertainty propagation goes through the posterior_draws
  # attribute on get_predict; coefficient overrides on the fitted
  # object are not load-bearing for the marginaleffects pipeline.
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
  fam_name <- model$family$family
  if (identical(fam_name, "cumulative") || identical(fam_name, "sratio") ||
      identical(fam_name, "cratio") || identical(fam_name, "acat")) {
    resp <- model$response_names[1L]
    y <- model$data[[resp]]
    if (is.factor(y)) {
      return(levels(y))
    }
    return(as.character(sort(unique(y))))
  }
  "main_marginaleffect"
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
