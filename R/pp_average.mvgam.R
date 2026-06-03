# brms-parity Bayesian model averaging for `mvgam` fits.
# `posterior_average.mvgam` resamples posterior draws across two
# or more fits in proportion to model weights;
# `pp_average.mvgam` does the same on the response/expected/linear
# predictor scale. Both compose the shared multi-model helpers in
# `R/mvgam_model_helpers.R` so future combined-forecasting methods
# can reuse the same plumbing.


#' Model-averaged posterior draws across \pkg{mvgam} fits
#'
#' Returns Bayesian model-averaged samples from the posterior of
#' two or more `mvgam` fits, in proportion to the supplied (or
#' computed) model weights. Mirrors
#' [brms::posterior_average.brmsfit()].
#'
#' @param x A fitted `mvgam` object. Further fits are passed via
#'   `...`.
#' @param ... Additional fitted `mvgam` objects to average over,
#'   and (named) additional arguments passed through to
#'   [as.data.frame.mvgam()] / [variables.mvgam()].
#' @param variable Optional character vector of parameter names
#'   to extract. `NULL` (the default) selects all parameters
#'   shared across fits (or all parameters across the union when
#'   `missing` is supplied), excluding `"lp__"`.
#' @param pars Deprecated brms alias for `variable`.
#' @param weights Either a numeric vector with one entry per fit
#'   (auto-normalised to sum to 1) or one of `"stacking"`,
#'   `"pseudobma"`, `"pseudobma+"`, `"loo"`, `"waic"`. See
#'   `mvgam_loo_extras` for the per-strategy semantics.
#' @param ndraws Total number of model-averaged draws to return.
#'   Defaults to the number of draws in the first fit.
#' @param nsamples Deprecated brms alias for `ndraws`.
#' @param missing Default values for parameters that are absent
#'   from some of the supplied fits. `NULL` (the default)
#'   restricts `variable` to the intersection across fits and
#'   errors if any requested parameter is not in every fit. A
#'   list keyed by parameter name supplies per-parameter
#'   defaults; a single numeric is broadcast across every
#'   missing parameter in every fit that lacks it.
#' @param model_names Optional character vector naming the fits
#'   in the same order they were passed.
#' @param control A list of extra arguments passed to
#'   [loo::loo_model_weights()] when computing stacking /
#'   pseudo-BMA weights.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A `data.frame` of model-averaged posterior draws.
#'   Attributes `weights` and `ndraws` record the per-model
#'   weight and the integer draw count drawn from each fit.
#'
#' @author Nicholas J Clark
#'
#' @seealso [brms::posterior_average.brmsfit()],
#'   [pp_average.mvgam()], [mvgam_loo_extras].
#'
#' @examples
#' \donttest{
#' sim <- sim_mvgam(family = gaussian())
#' mod_a <- mvgam(y ~ s(season, bs = "cc"), trend_model = AR(),
#'                data = sim$data_train, family = gaussian(),
#'                chains = 1, silent = 2)
#' mod_b <- mvgam(y ~ s(season, bs = "cc", k = 6),
#'                trend_model = AR(),
#'                data = sim$data_train, family = gaussian(),
#'                chains = 1, silent = 2)
#' avg <- posterior_average(mod_a, mod_b, weights = "stacking")
#' head(avg)
#' attr(avg, "weights")
#' }
#'
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
  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv)) {
      rng_old <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", rng_old, envir = .GlobalEnv))
    }
    set.seed(seed)
  }
  variable <- mvgam_use_alias(variable, pars)
  ndraws <- mvgam_use_alias(ndraws, nsamples)
  split <- mvgam_split_models(x, ..., model_names = model_names)
  models <- split$models
  vars_list <- lapply(models, variables)
  all_vars <- unique(unlist(vars_list))
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
  if (is.null(ndraws)) {
    ndraws <- posterior::ndraws(
      posterior::as_draws(models[[1L]]$fit)
    )
  }
  ndraws <- as.integer(ndraws)
  w <- mvgam_validate_weights(weights, models, control = control)
  ndraws_per <- mvgam_round_largest_remainder(w * ndraws)
  names(w) <- names(ndraws_per) <- names(models)
  out <- stats::setNames(
    vector("list", length(models)), names(models)
  )
  for (i in seq_along(models)) {
    if (ndraws_per[i] > 0L) {
      n_avail <- posterior::ndraws(
        posterior::as_draws(models[[i]]$fit)
      )
      draw_ids <- sort(sample.int(n_avail, ndraws_per[i]))
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
      if (!is.null(missing_resolved)) {
        miss <- setdiff(variable, names(out[[i]]))
        if (length(miss) > 0L) {
          for (v in miss) {
            out[[i]][[v]] <- missing_resolved[[v]]
          }
        }
      }
      out[[i]] <- out[[i]][, variable, drop = FALSE]
    }
  }
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  attr(out, "weights") <- w
  attr(out, "ndraws") <- ndraws_per
  out
}


#' @importFrom brms posterior_average
#' @export
brms::posterior_average


#' Model-averaged posterior predictions across \pkg{mvgam} fits
#'
#' Returns Bayesian model-averaged posterior predictions from
#' two or more `mvgam` fits, in proportion to the supplied (or
#' computed) model weights. Mirrors [brms::pp_average.brmsfit()].
#'
#' @inheritParams posterior_average.mvgam
#' @param method Which prediction primitive to call on each fit
#'   before averaging. One of `"posterior_predict"` (the
#'   default), `"posterior_epred"`, `"posterior_linpred"`.
#' @param summary Logical. If `TRUE` (the default), summarise the
#'   averaged predictions with [posterior_summary.mvgam()] (mean
#'   / Est.Error / two quantiles); otherwise return the full
#'   averaged draw matrix.
#' @param probs Numeric length-2 vector of quantile probabilities
#'   for the summary path. Defaults to `c(0.025, 0.975)`.
#' @param robust Logical. If `TRUE`, summarise via median / MAD
#'   rather than mean / SD on the summary path.
#'
#' @return A numeric matrix of averaged predictions. When
#'   `summary = TRUE`, rows are observations and columns are
#'   summary statistics; otherwise rows are draws and columns
#'   are observations. Attributes `weights` and `ndraws` record
#'   the per-model weight and integer draw count drawn from each
#'   fit.
#'
#' @author Nicholas J Clark
#'
#' @seealso [brms::pp_average.brmsfit()],
#'   [posterior_average.mvgam()], [mvgam_loo_extras].
#'
#' @examples
#' \donttest{
#' sim <- sim_mvgam(family = gaussian())
#' mod_a <- mvgam(y ~ s(season, bs = "cc"), trend_model = AR(),
#'                data = sim$data_train, family = gaussian(),
#'                chains = 1, silent = 2)
#' mod_b <- mvgam(y ~ s(season, bs = "cc", k = 6),
#'                trend_model = AR(),
#'                data = sim$data_train, family = gaussian(),
#'                chains = 1, silent = 2)
#' pp_average(mod_a, mod_b, weights = "stacking")
#' }
#'
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
  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv)) {
      rng_old <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", rng_old, envir = .GlobalEnv))
    }
    set.seed(seed)
  }
  method <- mvgam_validate_pp_method(method)
  ndraws <- mvgam_use_alias(ndraws, nsamples)
  split <- mvgam_split_models(x, ..., model_names = model_names)
  models <- split$models
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
  if (is.null(ndraws)) {
    ndraws <- posterior::ndraws(
      posterior::as_draws(models[[1L]]$fit)
    )
  }
  ndraws <- as.integer(ndraws)
  w <- mvgam_validate_weights(weights, models, control = control)
  ndraws_per <- mvgam_round_largest_remainder(w * ndraws)
  names(w) <- names(ndraws_per) <- names(models)
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
  if (isTRUE(summary)) {
    out <- mvgam_post_summary(out, robust = robust, probs = probs)
  }
  attr(out, "weights") <- w
  attr(out, "ndraws") <- ndraws_per
  out
}


#' @importFrom brms pp_average
#' @export
brms::pp_average
