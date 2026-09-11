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
  obs_arm_main_formula(x$formula)
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


# Predictor variables anywhere in a brms (or plain) formula: top-level
# RHS plus any per-parameter sub-formulas living in `$pforms`. brms
# stores both nl sub-formulas (`bf(..., nl = TRUE)`'s nlpar formulas)
# and distributional dpar sub-formulas (`sigma ~ x`, `hu ~ z`, etc.)
# under the same `$pforms` slot, keyed by the parameter name. The
# parameter names themselves (a, b, sigma, ...) are not data
# columns so they are filtered out of the returned vector. The
# return order is preserved for callers that want a stable list.
mvgam_formula_predictors <- function(f) {
  if (is.null(f)) {
    return(character(0L))
  }
  # mvbrmsformula has no top-level `$formula` slot; iterate the
  # per-response brmsformulas in `$forms` and union their
  # predictors. Each form may have its own nlpar / dpar pforms.
  if (inherits(f, "mvbrmsformula")) {
    return(unique(unlist(lapply(f$forms, mvgam_formula_predictors),
                           use.names = FALSE)))
  }
  if (inherits(f, "brmsformula")) {
    top <- mvgam_rhs_predictors(f$formula)
    nlpars <- character(0L)
    sub <- character(0L)
    if (length(f$pforms) > 0L) {
      nlpars <- names(f$pforms)
      sub <- unlist(lapply(f$pforms, mvgam_rhs_predictors),
                    use.names = FALSE)
    }
    setdiff(unique(c(top, sub)), nlpars)
  } else {
    mvgam_rhs_predictors(f)
  }
}


# Drop tokens that are not actual columns of the fit's data frame.
# brms RE syntax carries correlation IDs (`(1 | sp | series)`) and
# nested-group separators as if they were variables; this filter
# enforces "predictor names are addressable in the model data".
# Skips the filter when data is unavailable (mock objects in unit
# tests) so the helper composes with stub fixtures.
#
# The empty-obs placeholder is dropped here as well. It is absent
# from `x$data`, so the filter below removes it on a fit, but the
# rewritten formula still names it and mock objects carry no frame
# for the filter to work from.
mvgam_keep_data_columns <- function(vars, x) {
  vars <- setdiff(vars, MVGAM_EMPTY_OBS_PLACEHOLDER)
  if (is.null(x$data)) {
    return(vars)
  }
  intersect(vars, colnames(x$data))
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
  # The columns, not brms's keys for them: insight's callers read the
  # answer off the data, and a multivariate fit returns every response
  # so marginaleffects finds each one.
  unname(response_columns(x))
}


#' @importFrom insight find_predictors
#' @export
find_predictors.mvgam <- function(x, effects = "fixed",
                                  component = "conditional",
                                  flatten = FALSE, verbose = TRUE, ...) {
  # Reason: walk the full brmsformula (top-level + nl/dpar pforms)
  # rather than just the obs formula's top-level RHS, so trait1,
  # sigma covariates, and other sub-formula-only variables surface
  # for marginaleffects::datagrid and insight downstream.
  preds <- unique(c(
    mvgam_formula_predictors(x$formula),
    mvgam_rhs_predictors(x$trend_formula)
  ))

  # Time / series / grouping variables are addressable in the data
  # grid even though they sit outside the formulas. jsdgam aliases
  # the user's species column to 'series' on data_train; persist the
  # original column name so downstream tools that look up the user-
  # facing variable (conditional_effects, predict newdata builders)
  # find both. mv_spec$species_var is set by jsdgam(); the trend
  # metadata covers the regular mvgam case.
  meta <- x$trend_metadata$variables
  if (!is.null(meta)) {
    extras <- c(meta$time_var, meta$series_var, meta$gr_var, meta$subgr_var)
    preds <- unique(c(preds, varying_meta_vars(extras, x)))
  }
  jsdgam_meta <- attr(x$model_data, "prepped_trend_model")
  if (!is.null(jsdgam_meta)) {
    extras <- unlist(jsdgam_meta[c("unit", "species")], use.names = FALSE)
    preds <- unique(c(preds, varying_meta_vars(extras, x)))
  }

  # Drop brms `|id|` correlation-tag tokens and any other
  # non-data names that slipped through all.vars().
  preds <- mvgam_keep_data_columns(preds, x)

  if (flatten) preds else list(conditional = preds)
}




# Internal: keep the meta variables that take more than one value in
# the training data.
#
# Reason: time, series and the grouping columns are surfaced so
# `marginaleffects::datagrid()` can address them, but the same list
# is what `slopes()` and `comparisons()` iterate over when the caller
# names no variables. A column holding a single value supports
# neither a slope nor a contrast, and asking for one aborts the whole
# call, so a single-series fit could not be handed to `avg_slopes()`
# at all. A meta variable that genuinely varies is still reported.
#'@noRd
varying_meta_vars <- function(vars, x) {
  vars <- vars[!is.na(vars) & nzchar(vars)]
  if (!length(vars)) return(character(0))
  dat <- mvgam_training_data(x)
  if (is.null(dat)) return(vars)
  keep <- vapply(vars, function(v) {
    if (!v %in% names(dat)) return(TRUE)
    length(unique(dat[[v]][!is.na(dat[[v]])])) > 1L
  }, logical(1L))
  vars[keep]
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
    # Every response and its addition terms, then the same surface
    # find_predictors() walks so nl / dpar sub-formula vars and jsdgam
    # aliases ride along. Reading the left-hand side of one formula
    # dropped every response of a multivariate fit, whose subscript
    # there is a character vector rather than an expression.
    unique(c(lhs_columns(formula$formula),
             find_predictors(formula, flatten = TRUE)))
  }
  vars <- intersect(vars, colnames(formula$data))
  formula$data[, vars, drop = FALSE]
}


# Families mvgam classifies for `insight::model_info()`, keyed on the
# name `resolve_family_name()` reports.
#
# Reason: every family mvgam defines itself carries the literal
# `family$family == "custom"`, so reading that field classified all ten
# of them as nothing at all. `model_info()` is what marginaleffects and
# the easystats packages read to decide how to treat a fit, so a blank
# answer there silently changes their behaviour.
#'@noRd
mvgam_model_info_families <- function() {
  list(
    binomial = c(
      "binomial", "bernoulli", "beta_binomial", "com_binomial",
      "zero_inflated_binomial", "zero_inflated_beta_binomial", "occ"
    ),
    count = c(
      "poisson", "negbinomial", "negbinomial2", "negative_binomial",
      "geometric", "zero_inflated_poisson", "zero_inflated_negbinomial",
      "hurdle_poisson", "hurdle_negbinomial", "com_poisson",
      "discrete_weibull", "beta_nb", "nmix", "nmix_royle_nichols",
      "nmix_poisson_poisson"
    ),
    continuous = c(
      "gaussian", "student", "skew_normal", "gamma", "lognormal",
      "shifted_lognormal", "exponential", "weibull", "frechet",
      "inverse.gaussian", "exgaussian", "asym_laplace", "von_mises",
      "tweedie", "mvn", "mvt", "hurdle_gamma", "hurdle_lognormal"
    ),
    categorical = c("categorical", "categ"),
    proportion = c(
      "beta", "zero_inflated_beta", "zero_one_inflated_beta"
    ),
    multinomial = c("multinomial", "dirichlet_multinomial", "multi"),
    simplex = c("dirichlet", "dirichlet2", "diri")
  )
}


#' @importFrom insight model_info
#' @export
model_info.mvgam <- function(x, response = NULL, ...) {
  # A multivariate fit gives each response its own family, and
  # insight describes a brms fit once per response. Reading `$family`
  # alone described every arm as the last.
  resolve_resp(x, response)
  keys <- names(response_columns(x))
  if (is.null(response) && length(keys) > 1L) {
    return(lapply(stats::setNames(keys, keys), function(r) {
      model_info.mvgam(x, response = r, ...)
    }))
  }
  fam <- get_family_for_resp(x, response)
  fam_name <- tolower(resolve_family_name(fam))
  link <- fam$link %||% NA_character_
  cls <- mvgam_model_info_families()

  # Group-level terms show up as `sd_` scales and `r_` deviations. A
  # prefit has no draws to show them.
  pars <- if (is.null(x$fit)) character(0) else variables(x)

  list(
    is_binomial = fam_name %in% cls$binomial,
    is_count = fam_name %in% cls$count,
    is_continuous = fam_name %in% cls$continuous,
    is_ordinal = is_ordinal_family(fam),
    is_categorical = fam_name %in% cls$categorical,
    is_multinomial = fam_name %in% cls$multinomial,
    is_dirichlet = fam_name %in% cls$simplex,
    is_zero_inflated = grepl("zero_inflated|zero_one_inflated|hurdle",
                             fam_name),
    is_proportion = fam_name %in% cls$proportion,
    is_linear = identical(link, "identity"),
    is_logit = identical(link, "logit"),
    is_probit = identical(link, "probit"),
    is_log = identical(link, "log"),
    is_mixed = any(grepl("^sd_|^r_", pars)),
    is_multivariate = isTRUE(brms::is.mvbrmsformula(x$formula)),
    is_bayesian = TRUE,
    family = fam_name,
    link_function = link,
    n_obs = nrow(x$data %||% data.frame())
  )
}
