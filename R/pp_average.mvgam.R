# Bayesian model averaging for `mvgam` fits, as brms provides it.
# `posterior_average.mvgam` resamples posterior draws across two or
# more fits in proportion to model weights. `pp_average.mvgam` does
# the same for predictions. Both use the multi-model helpers in
# `R/mvgam_model_helpers.R`.


#' Model-averaged posterior draws and predictions across \pkg{mvgam}
#' fits
#'
#' Bayesian model averaging of two or more `mvgam` fits, as in
#' [brms::posterior_average.brmsfit()] and [brms::pp_average.brmsfit()].
#' Each fit contributes draws in proportion to its model weight.
#' `posterior_average()` averages parameter draws. `pp_average()`
#' averages predictions.
#'
#' @inheritParams compare_scores
#' @param x A fitted `mvgam` object. Further fits are passed via
#'   `...`.
#' @param ... Further fitted `mvgam` objects to average over. For
#'   `pp_average()`, named arguments are passed to the prediction
#'   function `method` names.
#' @param variable Optional character vector of parameter names
#'   to extract, as [variables.mvgam()] lists them. A bare vector name
#'   selects every element. `NULL` (the default) selects all parameters
#'   shared across fits (or all parameters across the union when
#'   `missing` is supplied), excluding `"lp__"`.
#' @param pars Deprecated brms alias for `variable`.
#' @param weights A numeric vector with one weight per fit, scaled to
#'   sum to one, or one of `"stacking"`, `"pseudobma"`, `"pseudobma+"`,
#'   `"loo"` and `"waic"`. [mvgam_loo_extras] describes each.
#' @param ndraws Total number of model-averaged draws to return.
#'   Defaults to the number of draws in the first fit.
#' @param nsamples Deprecated brms alias for `ndraws`.
#' @param missing Values for parameters some fits lack. `NULL` (the
#'   default) restricts `variable` to the parameters every fit has and
#'   refuses a name outside them. A list keyed by parameter name gives
#'   each parameter its own value. A single number fills every
#'   parameter a fit lacks.
#' @param control A list of extra arguments passed to
#'   [loo::loo_model_weights()] when computing stacking /
#'   pseudo-BMA weights.
#' @param seed Optional integer seed. The session's random state is
#'   restored when the call returns.
#'
#' @return `posterior_average()` returns a `data.frame` of
#'   model-averaged posterior draws. `pp_average()` returns a numeric
#'   matrix of averaged predictions. With `summary = TRUE` its rows are
#'   observations and its columns summary statistics. Otherwise its
#'   rows are draws and its columns observations. Attribute `weights`
#'   holds the weight of each fit. Attribute `ndraws` holds the number
#'   of draws taken from each fit.
#'
#' @author Nicholas J Clark
#'
#' @seealso [loo_model_weights.mvgam()] computes the weights.
#'   [ensemble()] averages forecasts. The
#'   `r mvgam_article("forecast_evaluation", "forecast evaluation article")`
#'   weights and combines competing models.
#'
#' @template ex-loo-models
#' @examples
#' \dontrun{
#' # The same smooth of x replaced by a linear effect
#' mod_lin <- update(mod, formula. = y ~ x)
#'
#' # Stacking weights from approximate leave-one-out cross-validation
#' loo_model_weights(mod, mod_lin)
#'
#' # The intercept, averaged over both fits under those weights
#' pa <- posterior_average(mod, mod_lin, variable = "b_Intercept")
#' attr(pa, "weights")
#' summary(pa)
#'
#' # Averaged expected counts, one row per observation
#' head(pp_average(mod, mod_lin, method = "posterior_epred"))
#' }
#'
#' @family model comparison
#' @method posterior_average mvgam
#' @export
posterior_average.mvgam <- function(x, ..., variable = NULL,
                                     pars = NULL,
                                     weights = "stacking",
                                     ndraws = NULL,
                                     nsamples = NULL,
                                     missing = NULL,
                                     model_names = NULL,
                                     control = list(),
                                     seed = NULL) {
  checkmate::assert_class(x, "mvgam")
  local_seed(seed)
  variable <- mvgam_use_alias(variable, pars, c("variable", "pars"))
  ndraws <- mvgam_use_alias(ndraws, nsamples, c("ndraws", "nsamples"))
  split <- mvgam_split_models(x, ..., model_names = model_names)
  refuse_unread_dots(split$other, character(0L), "posterior_average")
  models <- split$models
  # Draws come straight off each `$fit` below. An unfitted stub has
  # none.
  require_fitted_models(models, "posterior_average")
  vars_list <- lapply(models, variables)
  all_vars <- unique(unlist(vars_list))
  if (!is.null(variable)) {
    variable <- expand_variable_names(as.character(variable), all_vars)
  }
  missing_resolved <- NULL
  if (is.null(missing)) {
    common <- Reduce(intersect, vars_list)
    if (is.null(variable)) {
      variable <- setdiff(common, "lp__")
    }
    variable <- as.character(variable)
    inv_vars <- setdiff(variable, common)
    if (length(inv_vars) > 0L) {
      stop(insight::format_error(c(
        "Some parameters are not present in every model.",
        x = paste0(
          "Missing in at least one fit: ",
          paste0("'", inv_vars, "'", collapse = ", "), "."
        ),
        i = "Use 'missing' to supply defaults for absent parameters."
      )))
    }
  } else {
    if (is.null(variable)) {
      variable <- setdiff(all_vars, "lp__")
    }
    variable <- as.character(variable)
    inv_vars <- setdiff(variable, all_vars)
    if (length(inv_vars) > 0L) {
      stop(insight::format_error(c(
        "Some parameters are not present in any model.",
        x = paste0(
          "Unknown: ",
          paste0("'", inv_vars, "'", collapse = ", "), "."
        )
      )))
    }
    if (is.list(missing)) {
      all_miss <- unique(unlist(lapply(models, function(m) {
        setdiff(variable, variables(m))
      })))
      no_default <- setdiff(all_miss, names(missing))
      if (length(no_default) > 0L) {
        stop(insight::format_error(c(
          "'missing' has no entry for some parameters.",
          x = paste0(
            "No default for: ",
            paste0("'", no_default, "'", collapse = ", "), "."
          )
        )))
      }
      missing_resolved <- lapply(missing, as.numeric)
    } else {
      checkmate::assert_number(missing, na.ok = TRUE)
      missing_resolved <- stats::setNames(
        as.list(rep(missing, length(variable))), variable
      )
    }
  }
  shares <- averaging_draws(models, weights, ndraws, control)
  ndraws_per <- shares$ndraws
  out <- stats::setNames(
    vector("list", length(models)), names(models)
  )
  for (i in seq_along(models)) {
    if (ndraws_per[i] > 0L) {
      draw_ids <- resolve_draw_indices(posterior::ndraws(models[[i]]),
                                       ndraws_per[i], NULL)
      found <- intersect(variable, variables(models[[i]]))
      if (length(found) > 0L) {
        out[[i]] <- as.data.frame(
          models[[i]], variable = found, draw = draw_ids
        )
      } else {
        out[[i]] <- as.data.frame(matrix(
          numeric(0L), nrow = ndraws_per[i], ncol = 0L
        ))
      }
      for (v in setdiff(variable, names(out[[i]]))) {
        out[[i]][[v]] <- missing_resolved[[v]]
      }
      out[[i]] <- out[[i]][, variable, drop = FALSE]
    }
  }
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  attr(out, "weights") <- shares$weights
  attr(out, "ndraws") <- ndraws_per
  out
}


#' @importFrom brms posterior_average
#' @export
brms::posterior_average


#' @inheritParams predict.mvgam
#' @param method Which prediction function `pp_average()` calls on each
#'   fit before averaging: `"posterior_predict"` (the default),
#'   `"posterior_epred"` or `"posterior_linpred"`.
#' @param summary Logical. If `TRUE` (the default), `pp_average()`
#'   summarises the averaged predictions with
#'   [posterior_summary.mvgam()] as a mean, an error and two quantiles.
#'   `FALSE` returns the averaged draws.
#'
#' @rdname posterior_average.mvgam
#' @method pp_average mvgam
#' @export
pp_average.mvgam <- function(x, ..., weights = "stacking",
                              method = "posterior_predict",
                              ndraws = NULL, nsamples = NULL,
                              summary = TRUE,
                              probs = c(0.025, 0.975),
                              robust = FALSE,
                              model_names = NULL,
                              control = list(), seed = NULL) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_flag(summary)
  local_seed(seed)
  method <- mvgam_validate_pp_method(method)
  ndraws <- mvgam_use_alias(ndraws, nsamples, c("ndraws", "nsamples"))
  split <- mvgam_split_models(x, ..., model_names = model_names)
  models <- split$models
  # Draws come straight off each `$fit` below. An unfitted stub has
  # none.
  require_fitted_models(models, "pp_average")
  other <- split$other
  reserved <- intersect(c("draw_ids", "subset"), names(other))
  if (length(reserved) > 0L) {
    stop(insight::format_error(c(
      "'pp_average' draws its own per-model subsample.",
      x = paste0(
        "Cannot pass: ",
        paste0("'", reserved, "'", collapse = ", "), "."
      )
    )))
  }
  if (!mvgam_match_response(models)) {
    stop(insight::format_error(
      "Can only average models that predict the same response."
    ))
  }
  shares <- averaging_draws(models, weights, ndraws, control)
  ndraws_per <- shares$ndraws
  pred_fun <- match.fun(method)
  out <- vector("list", length(models))
  for (i in seq_along(models)) {
    if (ndraws_per[i] > 0L) {
      args_i <- c(
        list(object = models[[i]], ndraws = ndraws_per[i]),
        other
      )
      out[[i]] <- do.call(pred_fun, args_i)
    }
  }
  out <- do.call(rbind, out[!vapply(out, is.null, logical(1L))])
  if (summary) {
    out <- mvgam_post_summary(out, robust = robust, probs = probs)
  }
  attr(out, "weights") <- shares$weights
  attr(out, "ndraws") <- ndraws_per
  out
}


#' @importFrom brms pp_average
#' @export
brms::pp_average
