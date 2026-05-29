#' insight S3 methods for mvgam objects
#'
#' Minimum surface that `marginaleffects` and `insight` need to build
#' prediction grids and walk formula structure. mvgam keeps its
#' observation submodel formula in `x$formula` (a `brmsformula`) and
#' its trend submodel formula in `x$trend_formula`; these methods
#' stitch both into the structures insight expects rather than
#' relying on the default formula walker, which chokes on the
#' combined obs + trend representation.
#'
#' @name mvgam_insight
#' @keywords internal
NULL


# Pull the observation submodel's two-sided formula out of `$formula`,
# which may be either a brmsformula or a bare formula. Used by several
# of the methods below.
mvgam_obs_formula <- function(x) {
  if (inherits(x$formula, "brmsformula")) {
    x$formula$formula
  } else {
    x$formula
  }
}


# Predictor variables on the rhs of `f`. NULL formulas (e.g. a missing
# trend submodel) return character(0L) so downstream `unique()` /
# `c()` calls work without a guard.
mvgam_rhs_predictors <- function(f) {
  if (is.null(f)) {
    return(character(0L))
  }
  rhs <- if (length(f) == 3L) f[[3L]] else f[[2L]]
  all.vars(rhs)
}


#' @importFrom insight find_formula
#' @export
find_formula.mvgam <- function(x, verbose = TRUE, ...) {
  f <- list(conditional = mvgam_obs_formula(x))
  if (!is.null(x$trend_formula)) {
    f$trend <- x$trend_formula
  }
  class(f) <- c("insight_formula", "list")
  f
}


#' @importFrom insight find_response
#' @export
find_response.mvgam <- function(x, combine = TRUE, ...) {
  if (length(x$response_names) > 0L) {
    return(x$response_names)
  }
  all.vars(mvgam_obs_formula(x)[[2L]])[1L]
}


#' @importFrom insight find_predictors
#' @export
find_predictors.mvgam <- function(x, effects = "fixed",
                                  component = "conditional",
                                  flatten = FALSE, verbose = TRUE, ...) {
  preds <- unique(c(
    mvgam_rhs_predictors(mvgam_obs_formula(x)),
    mvgam_rhs_predictors(x$trend_formula)
  ))

  # Time / series / grouping variables are addressable in the data
  # grid even though they sit outside the formulas.
  meta <- x$trend_metadata$variables
  if (!is.null(meta)) {
    extras <- c(meta$time_var, meta$series_var, meta$gr_var, meta$subgr_var)
    extras <- extras[!is.na(extras) & nzchar(extras)]
    preds <- unique(c(preds, extras))
  }

  if (flatten) preds else list(conditional = preds)
}


#' @importFrom insight get_data
#' @export
get_data.mvgam <- function(x, effects = "all", component = "all",
                           source = "environment", verbose = TRUE, ...) {
  if (is.null(x$data)) {
    stop(insight::format_error(c(
      "No data found in mvgam object.",
      i = "Refit the model and ensure the data is stored on the object."
    )))
  }
  x$data
}


#' Extract a model frame from a fitted mvgam object
#'
#' Returns the model's training data restricted to the response plus
#' the variables `find_predictors()` reports. Used by upstream
#' consumers (`insight`, `marginaleffects::datagrid`) when they walk
#' the model frame to build prediction grids.
#'
#' @param formula A fitted `mvgam` object.
#' @param trend_effects Logical; if `TRUE` and the model has a trend
#'   formula, return the trend submodel's covariates only.
#' @param ... Ignored.
#'
#' @return A data frame.
#' @export
model.frame.mvgam <- function(formula, trend_effects = FALSE, ...) {
  checkmate::assert_class(formula, "mvgam")
  checkmate::assert_logical(trend_effects, len = 1L)
  if (trend_effects && is.null(formula$trend_formula)) {
    return(NULL)
  }
  if (is.null(formula$data)) {
    stop(insight::format_error(c(
      "No data found in mvgam object.",
      i = "Refit the model and ensure the data is stored on the object."
    )))
  }
  vars <- if (trend_effects) {
    mvgam_rhs_predictors(formula$trend_formula)
  } else {
    response <- all.vars(mvgam_obs_formula(formula)[[2L]])
    preds <- find_predictors(formula, flatten = TRUE)
    unique(c(response, preds))
  }
  vars <- intersect(vars, colnames(formula$data))
  formula$data[, vars, drop = FALSE]
}


#' @importFrom insight model_info
#' @export
model_info.mvgam <- function(x, response = NULL, ...) {
  fam_name <- x$family$family
  link <- x$family$link %||% NA_character_
  list(
    is_binomial = fam_name %in% c("binomial", "bernoulli", "beta_binomial"),
    is_count = fam_name %in% c("poisson", "negbinomial", "negative_binomial",
                                "zero_inflated_poisson",
                                "zero_inflated_negbinomial",
                                "hurdle_poisson", "hurdle_negbinomial"),
    is_continuous = fam_name %in% c("gaussian", "student", "skew_normal",
                                    "gamma", "Gamma", "lognormal",
                                    "exponential", "weibull"),
    is_ordinal = fam_name %in% c("cumulative", "sratio", "cratio", "acat"),
    is_categorical = fam_name == "categorical",
    is_zero_inflated = grepl("zero_inflated|hurdle", fam_name),
    is_proportion = fam_name == "Beta",
    is_linear = link == "identity",
    is_logit = link == "logit",
    is_probit = link == "probit",
    is_log = link == "log",
    is_mixed = FALSE,
    is_multivariate = isTRUE(x$series_info$is_multivariate) &&
                      length(x$response_names) > 1L,
    is_bayesian = TRUE,
    family = fam_name,
    link_function = link,
    n_obs = nrow(x$data %||% data.frame())
  )
}
