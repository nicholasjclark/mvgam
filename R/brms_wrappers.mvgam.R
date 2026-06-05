# Thin brms-parity wrappers on a fitted `mvgam` object. Each
# method mirrors the corresponding `brms::*.brmsfit` signature
# and delegates to the primitive that already exists in mvgam
# (`as.matrix.mvgam`, `posterior_predict.mvgam`,
# `mvgam_ranef_metadata`) so users can drop into the brms
# workflow without per-method shims.


#' Posterior credible intervals on a fitted \pkg{mvgam} model
#'
#' Returns quantile-based credible intervals for posterior
#' draws of the requested parameters. Mirrors
#' [brms::posterior_interval.brmsfit()] and dispatches through
#' [as.matrix.mvgam()] (so all `variable` shortcuts and
#' aliasing apply unchanged).
#'
#' @param object A fitted `mvgam` object.
#' @param pars Deprecated brms alias for `variable`. If supplied
#'   and `variable` is `NULL`, `pars` is used.
#' @param variable Optional character vector of parameter names
#'   or a `variables(object)` keyword shortcut. Passed straight
#'   through to [as.matrix.mvgam()].
#' @param prob Numeric in `(0, 1)`. The credible-interval mass
#'   to report. Defaults to `0.95`.
#' @param ... Unused; present for S3 / brms-parity.
#'
#' @return A numeric matrix with one row per parameter and two
#'   columns reporting the lower and upper interval bounds.
#'
#' @author Nicholas J Clark
#'
#' @seealso [brms::posterior_interval.brmsfit()],
#'   [as.matrix.mvgam()], [predictive_interval.mvgam()].
#'
#' @examples
#' \donttest{
#' sim <- sim_mvgam(family = gaussian())
#' mod <- mvgam(y ~ s(season, bs = "cc"), trend_model = AR(),
#'              data = sim$data_train, family = gaussian(),
#'              chains = 2, silent = 2)
#' posterior_interval(mod)
#' posterior_interval(mod, prob = 0.5)
#' }
#'
#' @method posterior_interval mvgam
#' @export
posterior_interval.mvgam <- function(object, pars = NA,
                                      variable = NULL, prob = 0.95,
                                      ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_number(prob, lower = 0, upper = 1)
  if (is.null(variable) && !identical(pars, NA)) {
    variable <- pars
  }
  draws <- as.matrix(object, variable = variable, ...)
  rstantools::posterior_interval(draws, prob = prob)
}


#' @importFrom rstantools posterior_interval
#' @export
rstantools::posterior_interval


#' Posterior predictive credible intervals on a fitted
#' \pkg{mvgam} model
#'
#' Returns quantile-based credible intervals for posterior
#' predictive draws. Mirrors [brms::predictive_interval.brmsfit()]
#' and routes through [posterior_predict.mvgam()].
#'
#' @param object A fitted `mvgam` object.
#' @param prob Numeric in `(0, 1)`. The credible-interval mass
#'   to report. Defaults to `0.9` (matching brms).
#' @param ... Forwarded to [posterior_predict.mvgam()].
#'
#' @return A numeric matrix with one row per observation and two
#'   columns reporting the lower and upper interval bounds.
#'
#' @author Nicholas J Clark
#'
#' @seealso [brms::predictive_interval.brmsfit()],
#'   [posterior_predict.mvgam()], [posterior_interval.mvgam()].
#'
#' @examples
#' \donttest{
#' sim <- sim_mvgam(family = gaussian())
#' mod <- mvgam(y ~ s(season, bs = "cc"), trend_model = AR(),
#'              data = sim$data_train, family = gaussian(),
#'              chains = 2, silent = 2)
#' predictive_interval(mod)
#' }
#'
#' @method predictive_interval mvgam
#' @export
predictive_interval.mvgam <- function(object, prob = 0.9, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_number(prob, lower = 0, upper = 1)
  preds <- posterior_predict(object, ...)
  rstantools::predictive_interval(preds, prob = prob)
}


#' @importFrom rstantools predictive_interval
#' @export
rstantools::predictive_interval


#' Number of levels per grouping factor in a fitted
#' \pkg{mvgam} model
#'
#' Returns a named list of integer counts: one entry per
#' grouping factor with the number of levels it contains.
#' Mirrors [brms::ngrps.brmsfit()] and returns `NULL` when the
#' fit has no observation-side random effects.
#'
#' @param object A fitted `mvgam` object.
#' @param ... Unused; present for S3 / brms-parity.
#'
#' @return A named list of integer scalars, or `NULL` if no
#'   group-level effects are present.
#'
#' @author Nicholas J Clark
#'
#' @seealso [brms::ngrps.brmsfit()], [ranef.mvgam()],
#'   [VarCorr.mvgam()].
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' sim <- sim_mvgam(family = gaussian())
#' sim$data_train$grp <- factor(sample(letters[1:4],
#'                                     NROW(sim$data_train),
#'                                     replace = TRUE))
#' mod <- mvgam(y ~ 1 + (1 | grp), trend_model = AR(),
#'              data = sim$data_train, family = gaussian(),
#'              chains = 2, silent = 2)
#' ngrps(mod)
#' }
#'
#' @method ngrps mvgam
#' @export
ngrps.mvgam <- function(object, ...) {
  checkmate::assert_class(object, "mvgam")
  meta <- mvgam_ranef_metadata(object)
  if (is.null(meta)) {
    return(NULL)
  }
  as.list(lengths(meta$group_levels))
}


#' @importFrom brms ngrps
#' @export
brms::ngrps


#' Posterior predictive errors on a fitted \pkg{mvgam} model
#'
#' Returns posterior draws of the response-scale prediction
#' error (observed minus predicted) for each observation.
#' Mirrors [brms::predictive_error.brmsfit()].
#'
#' @param object A fitted `mvgam` object.
#' @param newdata Optional `data.frame` to compute errors on.
#'   `NULL` (the default) uses the training data.
#' @param method One of `"posterior_predict"` (default) or
#'   `"posterior_epred"`. Selects the predicted-value primitive
#'   from which errors are taken.
#' @param resp,re_formula,re.form,ndraws,draw_ids,sort
#'   Forwarded to the prediction primitive; matched to brms's
#'   signature.
#' @param ... Unused; present for S3 / brms-parity.
#'
#' @return A numeric matrix of dimension
#'   `[n_draws x n_observations]` containing observed minus
#'   predicted for each draw and observation.
#'
#' @author Nicholas J Clark
#'
#' @seealso [brms::predictive_error.brmsfit()],
#'   [posterior_predict.mvgam()], [posterior_epred.mvgam()].
#'
#' @examples
#' \donttest{
#' sim <- sim_mvgam(family = gaussian())
#' mod <- mvgam(y ~ s(season, bs = "cc"), trend_model = AR(),
#'              data = sim$data_train, family = gaussian(),
#'              chains = 2, silent = 2)
#' err <- predictive_error(mod)
#' dim(err)
#' }
#'
#' @method predictive_error mvgam
#' @export
predictive_error.mvgam <- function(object, newdata = NULL,
                                    re_formula = NULL,
                                    re.form = NULL,
                                    method = "posterior_predict",
                                    resp = NULL, ndraws = NULL,
                                    draw_ids = NULL, sort = FALSE,
                                    ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_choice(
    method, c("posterior_predict", "posterior_epred")
  )
  if (!is.null(re.form) && is.null(re_formula)) {
    re_formula <- re.form
  }
  data <- if (is.null(newdata)) object$data else newdata
  resp_name <- mvgam_response_name(object)
  if (!resp_name %in% names(data)) {
    stop(insight::format_error(c(
      paste0(
        "Cannot compute predictive errors: response '",
        resp_name, "' not in 'newdata'."
      ),
      i = paste0(
        "Pass a 'newdata' that includes the response column ",
        "or omit 'newdata' to use the training data."
      )
    )))
  }
  pred_fun <- if (identical(method, "posterior_predict")) {
    posterior_predict
  } else {
    posterior_epred
  }
  preds <- pred_fun(
    object, newdata = newdata, re_formula = re_formula,
    resp = resp, ndraws = ndraws, draw_ids = draw_ids, ...
  )
  y <- as.numeric(data[[resp_name]])
  if (ncol(preds) != length(y)) {
    stop(insight::format_error(c(
      "Internal: predictor output and response length disagree.",
      x = paste0(
        "Predictor columns: ", ncol(preds),
        "; response observations: ", length(y), "."
      )
    )))
  }
  out <- sweep(preds, 2L, y, FUN = function(p, obs) obs - p)
  if (isTRUE(sort)) {
    out <- out[, order(y), drop = FALSE]
  }
  out
}


#' @importFrom rstantools predictive_error
#' @export
rstantools::predictive_error


# ------------------------------------------------------------------
# Deprecated brms aliases retained for back-compat
# ------------------------------------------------------------------


#' Deprecated brms aliases on a fitted \pkg{mvgam} model
#'
#' Thin wrappers that dispatch a deprecated brms generic name to
#' its current mvgam method, so brms-trained users do not see
#' "no applicable method" errors when they reach for legacy
#' names. For the four generics that brms itself has deprecated
#' (`marginal_smooths`, `marginal_effects`, `parnames`,
#' `nsamples`), the brms generic emits the one-time deprecation
#' warning and the mvgam method just forwards the call.
#' `as.mcmc.mvgam` mirrors `brms::as.mcmc.brmsfit` — both are
#' deprecated and emit their own one-time warning recommending
#' `posterior::as_draws_array()` or `as.array(x)` for newer
#' tooling. It converts the posterior draws (with the standard
#' brms-style alias map applied to column names) to a
#' [coda::mcmc.list()] (default) or single [coda::mcmc()] for
#' downstream coda-based diagnostics. Requires the \pkg{coda}
#' package (suggested dependency).
#'
#' * `marginal_smooths.mvgam` -> [conditional_smooths.mvgam()]
#' * `marginal_effects.mvgam` -> [conditional_effects.mvgam()]
#' * `parnames.mvgam` -> [variables.mvgam()]
#' * `nsamples.mvgam` -> [posterior::ndraws()]
#' * `as.mcmc.mvgam` -> [coda::mcmc.list()]
#'
#' @param x,object A fitted `mvgam` object.
#' @param ... Forwarded to the current method.
#'
#' @return Shape depends on the method (see the brms equivalent
#'   for `marginal_smooths` / `marginal_effects` / `parnames` /
#'   `nsamples`). `as.mcmc.mvgam` returns a [coda::mcmc.list()]
#'   when `combine_chains = FALSE` (default) and a single
#'   [coda::mcmc()] when `combine_chains = TRUE`.
#'
#' @author Nicholas J Clark
#'
#' @seealso [posterior::as_draws_array()], [as.array.mvgam()],
#'   [variables.mvgam()].
#'
#' @name mvgam_brms_deprecated
NULL


#' @rdname mvgam_brms_deprecated
#' @method marginal_smooths mvgam
#' @export
marginal_smooths.mvgam <- function(x, ...) {
  conditional_smooths(x, ...)
}


#' @importFrom brms marginal_smooths
#' @export
brms::marginal_smooths


#' @rdname mvgam_brms_deprecated
#' @method marginal_effects mvgam
#' @export
marginal_effects.mvgam <- function(x, ...) {
  conditional_effects(x, ...)
}


#' @importFrom brms marginal_effects
#' @export
brms::marginal_effects


#' @rdname mvgam_brms_deprecated
#' @method parnames mvgam
#' @export
parnames.mvgam <- function(x, ...) {
  variables(x, ...)
}


#' @importFrom brms parnames
#' @export
brms::parnames


#' @rdname mvgam_brms_deprecated
#' @method nsamples mvgam
#' @export
nsamples.mvgam <- function(object, ...) {
  posterior::ndraws(posterior::as_draws(object$fit))
}


#' @importFrom brms nsamples
#' @export
brms::nsamples


#' @rdname mvgam_brms_deprecated
#' @param pars Optional character vector of parameter names to
#'   include. `NA` (the default) keeps all parameters. With
#'   `fixed = FALSE` each entry is treated as a regular expression
#'   and any matching parameter is kept; with `fixed = TRUE` only
#'   exact matches are kept.
#' @param fixed Logical. If `TRUE`, treat `pars` as exact names
#'   rather than regular expressions.
#' @param combine_chains Logical. If `TRUE`, return a single
#'   [coda::mcmc()] object stacking all chains; otherwise return
#'   a [coda::mcmc.list()] with one element per chain (the
#'   default).
#' @param inc_warmup Logical. If `TRUE`, include warmup draws.
#'   Defaults to `FALSE`.
#' @method as.mcmc mvgam
#' @export
as.mcmc.mvgam <- function(x, pars = NA, fixed = FALSE,
                           combine_chains = FALSE,
                           inc_warmup = FALSE, ...) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_logical(fixed, len = 1L)
  checkmate::assert_logical(combine_chains, len = 1L)
  checkmate::assert_logical(inc_warmup, len = 1L)
  if (!requireNamespace("coda", quietly = TRUE)) {
    stop(insight::format_error(c(
      "Package 'coda' is required for 'as.mcmc.mvgam'.",
      i = "Install it with 'install.packages(\"coda\")'."
    )))
  }
  warning(
    "'as.mcmc.mvgam' is deprecated; prefer ",
    "'posterior::as_draws_array(x)' or 'as.array(x)' for ",
    "downstream tooling that accepts the 'posterior' draws ",
    "format.",
    call. = FALSE
  )
  # Route through `as_draws_array.mvgam` so the brms-style alias
  # map (e.g. `b[1] -> b_Intercept`, `bs[k] -> bs_<colname>`,
  # `r_<id>[lvl, coef] -> r_<group>[<level>, <coef>]`) is applied
  # before any `pars` filtering or downstream consumers see the
  # column names.
  drws <- as_draws_array(x, inc_warmup = inc_warmup)
  all_vars <- posterior::variables(drws)
  if (!identical(pars, NA) && !is.null(pars)) {
    checkmate::assert_character(pars, min.len = 1L)
    if (isTRUE(fixed)) {
      kept <- intersect(pars, all_vars)
    } else {
      kept <- unique(unlist(lapply(pars, function(p) {
        grep(p, all_vars, value = TRUE)
      })))
    }
    if (length(kept) == 0L) {
      stop(insight::format_error(c(
        "No parameters matched 'pars'.",
        x = paste0(
          "Requested: ",
          paste0("'", pars, "'", collapse = ", "), "."
        ),
        i = "Use 'variables(x)' to see available names."
      )))
    }
    drws <- posterior::subset_draws(drws, variable = kept)
  }
  if (isTRUE(combine_chains)) {
    mat <- as.matrix(posterior::as_draws_matrix(drws))
    attr(mat, "mcpar") <- c(1L, nrow(mat), 1L)
    class(mat) <- "mcmc"
    return(mat)
  }
  n_chains <- posterior::nchains(drws)
  n_iter <- posterior::niterations(drws)
  vars <- posterior::variables(drws)
  per_chain <- lapply(seq_len(n_chains), function(ch) {
    chain_slice <- drws[, ch, , drop = FALSE]
    mat <- matrix(
      as.numeric(chain_slice),
      nrow = n_iter, ncol = length(vars),
      dimnames = list(NULL, vars)
    )
    attr(mat, "mcpar") <- c(1L, n_iter, 1L)
    class(mat) <- "mcmc"
    mat
  })
  coda::as.mcmc.list(per_chain)
}


#' @importFrom coda as.mcmc
#' @export
coda::as.mcmc


# Internal: return the response variable's name from the fit's
# observation formula. Single source of truth for predictive
# methods that need the column.
#'@noRd
mvgam_response_name <- function(object) {
  checkmate::assert_class(object, "mvgam")
  f <- object$formula
  if (inherits(f, "brmsformula")) f <- f$formula
  all.vars(f[[2L]])[1L]
}


# Internal: training data for a fitted `mvgam`. Prefers
# `obs_data` (the post-fit canonical slot) and falls back to
# `data` (raw input). Returns NULL when neither slot is set.
# Single source of truth so every downstream consumer reads the
# training data the same way. Assertion is intentionally
# omitted: called in low-overhead inner loops where the caller
# has already validated.
#'@noRd
mvgam_training_data <- function(object) {
  object$obs_data %||% object$data
}
