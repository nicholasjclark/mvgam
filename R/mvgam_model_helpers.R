# Shared internal helpers for methods that operate on multiple
# fitted `mvgam` objects or `mvgam_forecast` objects (model
# averaging, posterior averaging, forecast ensembling). Mirrors
# the helper surface brms uses to back `posterior_average.brmsfit`
# and `pp_average.brmsfit`, with brms-internal calls replaced by
# exported equivalents per CRAN policy.


#' Model objects separated from the other arguments of a call
#'
#' Collects every object of `class` passed as `x, ...`. Each `...` entry
#' of another class must be named.
#'
#' @param x First model object
#' @param ... Further model objects and named arguments
#' @param model_names Character vector of labels, one per model, or
#'   `NULL`
#' @param class Character class the model objects inherit from, such as
#'   `"mvgam"` or `"mvgam_forecast"`
#' @return List holding the named list of `models` and the named list of
#'   `other` arguments
#' @noRd
mvgam_split_models <- function(x, ..., model_names = NULL,
                                class = "mvgam") {
  checkmate::assert_string(class)
  checkmate::assert_class(x, class)
  dots <- list(...)
  user_names <- names(dots)
  if (is.null(user_names)) {
    user_names <- rep("", length(dots))
  }
  is_model_dot <- vapply(dots, inherits, logical(1L), class)
  other <- dots[!is_model_dot]
  other_names <- user_names[!is_model_dot]
  if (length(other) > 0L &&
      (length(other_names) == 0L ||
         any(!nzchar(other_names)))) {
    stop(insight::format_error(c(
      "All non-model arguments must be named.",
      i = "Pass mvgam fits by position."
    )))
  }
  models <- c(list(x), dots[is_model_dot])
  # `substitute(x)` names the caller's own argument, `object` in
  # `ensemble()`. Substituting that in the caller's frame gives the
  # expression the user passed.
  dot_call <- c(
    list(do.call(substitute, list(substitute(x)), envir = parent.frame())),
    as.list(substitute(list(...), env = parent.frame()))[-1L]
  )
  names(models) <- compared_object_names(
    models, dot_call[c(TRUE, is_model_dot)], model_names
  )
  if (is.null(model_names) && anyDuplicated(names(models))) {
    names(models) <- paste0("model", seq_along(models))
  }
  list(models = models, other = other)
}


#' Whether fits share one response
#'
#' Compares the response names and their values in each fit's `data`
#' by direct equality. The brms equivalent compares hashes of the Stan
#' data.
#'
#' @param models List of `mvgam` fits
#' @return Logical scalar
#' @noRd
mvgam_match_response <- function(models) {
  checkmate::assert_list(models, min.len = 1L, types = "mvgam")
  if (length(models) < 2L) {
    return(TRUE)
  }
  resp_names <- lapply(models, function(m) unname(response_columns(m)))
  ref_names <- resp_names[[1L]]
  if (!all(vapply(resp_names[-1L], identical, logical(1L),
                  ref_names))) {
    return(FALSE)
  }
  ref_y <- lapply(ref_names, function(nm) {
    as.numeric(models[[1L]]$data[[nm]])
  })
  for (i in seq.int(2L, length(models))) {
    for (j in seq_along(ref_names)) {
      cand <- as.numeric(models[[i]]$data[[ref_names[[j]]]])
      if (!identical(cand, ref_y[[j]])) {
        return(FALSE)
      }
    }
  }
  TRUE
}


#' Numeric model weights checked and scaled to sum to one
#'
#' Model averaging and forecast ensembles both call it. The weights must
#' be finite and non-negative with a positive total and one entry per
#' model.
#'
#' @param weights Numeric vector of weights
#' @param n_models Integer number of models
#' @return Numeric vector of weights summing to one
#' @noRd
mvgam_normalize_weights <- function(weights, n_models) {
  checkmate::assert_numeric(weights)
  checkmate::assert_integerish(n_models, lower = 1L, len = 1L)
  if (length(weights) != n_models) {
    stop(insight::format_error(c(
      "Numeric 'weights' must have one entry per model.",
      x = paste0(
        "Got ", length(weights), " weights for ",
        n_models, " models."
      )
    )))
  }
  if (any(weights < 0) || any(!is.finite(weights))) {
    stop(insight::format_error(
      "Numeric 'weights' must be finite and non-negative."
    ))
  }
  if (sum(weights) == 0) {
    stop(insight::format_error(
      "Numeric 'weights' must have a positive total."
    ))
  }
  weights / sum(weights)
}


#' Model-averaging weights from a vector or a named strategy
#'
#' A numeric vector is checked and scaled to sum to one. The stacking
#' and pseudo-BMA strategies call `loo::loo_model_weights()`. The
#' `"loo"` and `"waic"` strategies weight each model by
#' `exp(-0.5 * dIC)`, normalised over the models.
#'
#' @param weights Numeric vector of weights, or one of `"stacking"`,
#'   `"pseudobma"`, `"pseudobma+"`, `"loo"` and `"waic"`
#' @param models List of `mvgam` fits
#' @param control Named list of further arguments to
#'   `loo::loo_model_weights()`
#' @return Numeric vector of weights, one per model
#' @noRd
mvgam_validate_weights <- function(weights, models,
                                    control = list()) {
  checkmate::assert_list(models, min.len = 1L, types = "mvgam")
  checkmate::assert_list(control)
  if (is.numeric(weights)) {
    return(mvgam_normalize_weights(weights, length(models)))
  }
  checkmate::assert_string(weights)
  if (identical(weights, "kfold")) {
    stop(insight::format_error(c(
      "Weight strategy 'kfold' is not supported for 'mvgam' fits.",
      x = paste0(
        "Model averaging does not compute weights from ",
        "cross-validation folds."
      ),
      i = paste0(
        "Use 'stacking', 'pseudobma', 'pseudobma+', 'loo', ",
        "'waic' or a numeric vector."
      )
    )))
  }
  valid <- c("stacking", "pseudobma", "pseudobma+",
             "loo", "waic")
  weights <- match.arg(weights, valid)
  if (weights %in% c("loo", "waic")) {
    crit_fun <- match.fun(weights)
    crits <- lapply(models, crit_fun)
    elpd_col <- paste0("elpd_", weights)
    elpds <- vapply(crits, function(cc) {
      cc$estimates[elpd_col, "Estimate"]
    }, numeric(1L))
    # exp(-0.5 * dIC) normalised is the softmax of the ELPDs.
    return(softmax(elpds))
  }
  loos <- lapply(models, loo)
  if (identical(weights, "pseudobma+")) {
    method <- "pseudobma"
    bb <- TRUE
  } else if (identical(weights, "pseudobma")) {
    method <- "pseudobma"
    bb <- FALSE
  } else {
    method <- "stacking"
    bb <- FALSE
  }
  args <- c(
    list(loos, method = method, BB = bb),
    control[setdiff(names(control), c("method", "BB"))]
  )
  out <- do.call(loo::loo_model_weights, args)
  as.numeric(out)
}


#' Fractional draw counts rounded to integers with the same total
#'
#' Uses largest-remainder rounding, ported from the internal brms
#' function `round_largest_remainder()`.
#'
#' @param x Non-negative numeric vector of fractional counts
#' @return Integer vector of counts summing to `round(sum(x))`, with the
#'   names of `x`
#' @noRd
mvgam_round_largest_remainder <- function(x) {
  checkmate::assert_numeric(x, lower = 0, any.missing = FALSE)
  total <- round(sum(x))
  out <- floor(x)
  diff <- x - out
  J <- order(diff, decreasing = TRUE)
  I <- seq_len(total - floor(sum(out)))
  out[J[I]] <- out[J[I]] + 1L
  stats::setNames(as.integer(out), names(x))
}


#' The prediction method `pp_average()` applies
#'
#' @param method Character name of the prediction function, matched
#'   against `"posterior_predict"`, `"posterior_epred"` and
#'   `"posterior_linpred"`
#' @return Character scalar naming the prediction function
#' @noRd
mvgam_validate_pp_method <- function(method) {
  checkmate::assert_character(method, min.len = 1L)
  match.arg(
    method,
    c("posterior_predict", "posterior_epred", "posterior_linpred")
  )
}


#' An argument value resolved against its deprecated alias
#'
#' Supplying both the argument and its alias is an error.
#'
#' @param x Value of the current argument, or `NULL`
#' @param alias Value of the deprecated alias, or `NULL`
#' @return `alias` when supplied, otherwise `x`
#' @noRd
mvgam_use_alias <- function(x, alias = NULL) {
  if (!is.null(alias)) {
    if (!is.null(x)) {
      stop(insight::format_error(
        "Specify only one of an argument and its deprecated alias."
      ))
    }
    return(alias)
  }
  x
}
