#' LOO / WAIC extras and aliases for mvgam models
#'
#' brms-parity S3 methods that compose mvgam's existing prediction
#' and log-likelihood primitives ([`log_lik.mvgam`], [`loo.mvgam`],
#' [`waic.mvgam`], [`posterior_epred.mvgam`],
#' [`posterior_linpred.mvgam`], [`posterior_predict.mvgam`]) with
#' helpers from the \pkg{loo} package.
#'
#' @name mvgam_loo_extras
#' @aliases LOO.mvgam WAIC.mvgam loo_R2.mvgam loo_predict.mvgam
#'   loo_epred.mvgam loo_linpred.mvgam loo_predictive_interval.mvgam
#'   loo_subsample.mvgam loo_moment_match.mvgam
#'   loo_model_weights.mvgam add_criterion.mvgam
#'
#' @param x,object A fitted `mvgam` object.
#' @param resp Character. Response name for multivariate models.
#' @param summary Logical. If `TRUE` (the default), return a
#'   summary matrix with `Estimate`, `Est.Error`, and credible
#'   interval columns; otherwise return the raw posterior draws.
#' @param robust Logical. If `TRUE`, use median + median absolute
#'   deviation instead of mean + standard deviation when summarising.
#' @param probs Numeric vector. Lower and upper quantile
#'   probabilities for credible intervals. Defaults to
#'   `c(0.025, 0.975)`.
#' @param seed Optional integer seed for reproducibility.
#' @param args_epred,args_loglik Lists of additional arguments
#'   forwarded to [`posterior_epred`] / [`log_lik`] when computing
#'   the LOO R^2.
#' @param type For `loo_predict.mvgam`, `loo_epred.mvgam`, and
#'   `loo_linpred.mvgam`, one of `"mean"`, `"var"`, or
#'   `"quantile"`.
#' @param prob For `loo_predictive_interval.mvgam`, a single
#'   numeric in `(0, 1)` giving the credible-interval mass.
#'   Defaults to `0.9` (matching brms).
#' @param psis_object Optional precomputed `loo::psis` object.
#' @param compare Logical. If `TRUE`, compare multiple models.
#' @param pointwise Logical. Pointwise streaming mode (not
#'   supported; passing `TRUE` raises an error).
#' @param moment_match,reloo Logical. LOO post-processing
#'   modes (not supported; raise informative errors).
#' @param k_threshold,save_psis,moment_match_args,reloo_args Forwarded
#'   verbatim to the LOO machinery.
#' @param model_names Optional character vector of model labels for
#'   multi-model methods.
#' @param criterion For `add_criterion.mvgam`, a character vector
#'   of criterion names. Supported: `"loo"`, `"waic"`,
#'   `"loo_subsample"`, `"bayes_R2"`, `"loo_R2"`.
#' @param model_name Optional character. Stored in `x$model_name`
#'   when supplied.
#' @param overwrite Logical. If `TRUE`, recompute criteria that
#'   already exist in `x$criteria`.
#' @param file Optional file stub. When supplied, the fitted
#'   object is saved as `paste0(file, ".rds")` after the criteria
#'   loop.
#' @param force_save Retained for brms-parity; ignored.
#' @param loo,newdata,check,recompile Retained for brms-parity on
#'   `loo_moment_match.mvgam` (the method errors before they are
#'   read).
#' @param ... Additional arguments forwarded to the underlying
#'   `loo::*` or mvgam method.
#'
#' @return Shape depends on the method (see method details and the
#'   brms / loo equivalents).
#'
#' @seealso
#'   [mvgam_diagnostics] for posterior summaries and convergence
#'   diagnostics,
#'   [mvgam_draws] for raw draws extraction,
#'   [loo.mvgam()], [waic.mvgam()] for the underlying information
#'   criteria,
#'   [log_lik.mvgam()] for pointwise log densities,
#'   [bayes_R2.mvgam()] for the non-LOO Bayesian R^2,
#'   [brms::loo_R2.brmsfit()], [brms::loo_predict.brmsfit()],
#'   [brms::loo_epred.brmsfit()], [brms::loo_linpred.brmsfit()],
#'   [brms::loo_predictive_interval.brmsfit()],
#'   [brms::add_criterion()], [loo::loo_subsample()],
#'   [loo::loo_model_weights()], [loo::E_loo()],
#'   [loo::psis()].
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#'
#' mod <- mvgam(y ~ s(x),
#'               trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' LOO(mod)            # brms-style alias for loo()
#' loo_R2(mod)         # leave-one-out Bayesian R^2
#' head(loo_predict(mod))   # per-observation LOO posterior mean
#' }
#'
#' @author Nicholas J Clark
NULL


# Internal: relative effective sample size for a per-observation
# log-likelihood matrix, using the chain layout of `x$fit`. Mirrors
# brms's `r_eff_log_lik` helper. Shared by every method that builds a
# PSIS object, so the chain bookkeeping lives in one place.
#
# `draw_ids` names the draws retained in `ll` when the caller
# subsampled, indexing into the chain-major flattened draws.
#
# Reason: loo::relative_eff() reshapes the matrix into a draws x
# chains array and so demands every chain contribute the same number
# of rows. A subsampled or otherwise unbalanced set of draws breaks
# that, and building the chain vector by recycling a fractional
# count silently produces one shorter than the matrix. Where the
# chain layout cannot be recovered exactly, report r_eff = 1, which
# is loo's documented reading for draws that are not a contiguous
# MCMC sample; a random subsample across chains is close to
# independent, so the assumption costs little.
#'@noRd
mvgam_r_eff_log_lik <- function(x, ll, draw_ids = NULL) {
  n_draws <- NROW(ll)
  n_obs <- NCOL(ll)
  chains <- tryCatch(
    posterior::nchains(posterior::as_draws_array(x$fit)),
    error = function(e) 1L
  )
  chain_id <- mvgam_chain_id(chains, n_draws, draw_ids, x)
  if (is.null(chain_id)) {
    return(rep(1, n_obs))
  }
  loo::relative_eff(exp(ll), chain_id = chain_id)
}


# Internal: chain membership for each retained draw, or NULL when a
# balanced chain layout cannot be recovered.
#'@noRd
mvgam_chain_id <- function(chains, n_draws, draw_ids = NULL, x = NULL) {
  if (!isTRUE(chains > 1L)) {
    return(NULL)
  }
  if (is.null(draw_ids)) {
    if (n_draws %% chains != 0L) {
      return(NULL)
    }
    return(rep(seq_len(chains), each = n_draws %/% chains))
  }
  total <- tryCatch(
    posterior::ndraws(posterior::as_draws_array(x$fit)),
    error = function(e) NA_integer_
  )
  if (is.na(total) || total %% chains != 0L) {
    return(NULL)
  }
  per_chain <- total %/% chains
  cid <- ((as.integer(draw_ids) - 1L) %/% per_chain) + 1L
  if (length(cid) != n_draws) {
    return(NULL)
  }
  counts <- table(factor(cid, levels = seq_len(chains)))
  if (any(counts == 0L) || length(unique(as.integer(counts))) != 1L) {
    return(NULL)
  }
  # relative_eff() reads the matrix chain-block by chain-block, so the
  # ids must arrive grouped by chain.
  if (is.unsorted(cid)) {
    return(NULL)
  }
  cid
}


# Internal: Bayesian LOO R^2 of Gelman et al. (2019), ported from
# brms:::.loo_R2. Operates on the per-draw expected predictions
# `epred` (S x N), log-likelihood matrix `ll` (S x N), and the
# response `y` (length N). Returns a one-column matrix of LOO R^2
# values (length S), clamped to [-1, 1].
#'@noRd
mvgam_loo_R2 <- function(y, epred, ll, r_eff) {
  psis_obj <- loo::psis(log_ratios = -ll, r_eff = r_eff)
  yhat_loo <- loo::E_loo(epred, psis_obj, log_ratios = -ll)$value
  err <- yhat_loo - y
  S <- nrow(epred)
  N <- ncol(epred)
  # Bayesian-bootstrap weights (Dirichlet(1, ..., 1) via normalised
  # exponentials); identical to the brms reference implementation.
  w <- matrix(stats::rexp(S * N, rate = 1), S, N)
  w <- w / rowSums(w)
  var_y <- (N / (N - 1)) *
    (rowSums(sweep(w, 2, y^2, "*")) -
       rowSums(sweep(w, 2, y, "*"))^2)
  var_err <- (N / (N - 1)) *
    (rowSums(sweep(w, 2, err^2, "*")) -
       rowSums(sweep(w, 2, err, "*"))^2)
  r2 <- 1 - var_err / var_y
  r2[r2 < -1] <- -1
  r2[r2 > 1] <- 1
  as.matrix(r2)
}


# Internal: shared PSIS-weighting skeleton for `loo_predict`,
# `loo_epred`, and `loo_linpred`. Builds a PSIS object (via
# `loo(save_psis = TRUE)` if none supplied), computes posterior
# predictions through `posterior_fn`, and returns the PSIS-
# weighted expectation/quantile via `loo::E_loo`, with the
# brms-parity output normalisation (column-labelled matrix; for
# 3D multivariate prediction arrays, returns a 3D array with the
# per-response slabs stacked along the last dimension).
#
# Weights and predictions are both taken conditional on the latent
# state, so the two describe the same observation rather than two
# draws of the trend. What remains stochastic is the observation
# noise `posterior_predict()` draws, and the calls run under a
# shared seed so a repeated call answers the same way; the user's
# RNG state is snapshotted on entry and restored on exit.
#'@noRd
# Internal: stop with a consistent message when a method that can only
# operate on one response at a time is handed a multivariate fit and
# no `resp`. Reason: the guard was spelled out separately in each
# method that had one, so the methods that lacked it failed deep in
# the prediction stack with an internal message
# ("is.numeric(x) is not TRUE") instead of naming the fix.
#'@noRd
assert_resp_for_mv <- function(object, resp, fn_name) {
  if (!brms::is.mvbrmsformula(object$formula) || !is.null(resp)) {
    return(invisible(NULL))
  }
  stop(insight::format_error(c(
    paste0("'", fn_name, "()' requires 'resp' for multivariate models."),
    i = paste0(
      "Available responses: ",
      paste(shQuote(object$response_names), collapse = ", "), "."
    )
  )), call. = FALSE)
}


#' Narrow predictions to the observations the weights cover
#'
#' `clean_ll()` drops the columns a missing response left unscorable
#' and records which survived, so the importance weights built from it
#' describe fewer observations than a prediction does. Selecting the
#' same columns is what keeps the two talking about the same rows.
#' Predictions that already match are returned untouched, so a fit with
#' no missing responses pays nothing.
#'
#' @param preds Matrix `[ndraws x nobs]` or array `[ndraws x nobs x k]`
#' @param psis_object The PSIS object the weights come from
#' @return `preds` restricted to the scored observations
#'
#' @noRd
narrow_to_scored <- function(preds, psis_object) {
  scored <- attr(psis_object, "scored_columns")
  n_weighted <- dim(psis_object)[2L]
  n_pred <- dim(preds)[2L]
  if (is.null(n_pred) || is.null(n_weighted) || n_pred == n_weighted) {
    return(preds)
  }
  if (is.null(scored) || length(scored) != n_weighted ||
      max(scored) > n_pred) {
    stop(insight::format_error(c(
      "Predictions and importance weights cover different observations.",
      x = paste0("Predicted ", n_pred, " observations; weighted ",
                 n_weighted, "."),
      i = paste0(
        "This happens when a 'psis_object' was built from a different ",
        "model or response. Leave it unset to have it computed here."
      )
    )))
  }
  if (length(dim(preds)) == 3L) {
    return(preds[, scored, , drop = FALSE])
  }
  preds[, scored, drop = FALSE]
}


mvgam_loo_E_loo <- function(object, posterior_fn,
                             type = c("mean", "var", "quantile"),
                             probs = 0.5, psis_object = NULL,
                             resp = NULL, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_function(posterior_fn)
  assert_resp_for_mv(object, resp, loo_fn_name(posterior_fn))
  type <- match.arg(type)
  # `local_seed()` takes responsibility for putting the caller's
  # stream back. The two `set.seed(aligned_seed)` calls below are
  # not redundant with it: they restart the same stream before the
  # weighting and before the prediction, so the two draw together.
  aligned_seed <- 1L
  local_seed(aligned_seed)
  # `incl_autocor` names a prediction surface, so it is held back from
  # the weighting call, which reaches `loo::loo()` through `...` and
  # would not know the argument.
  dots <- list(...)
  surface <- dots$incl_autocor
  dots$incl_autocor <- NULL
  if (is.null(psis_object)) {
    message("Running PSIS to compute weights")
    set.seed(aligned_seed)
    loo_object <- do.call(
      loo, c(list(object, resp = resp, save_psis = TRUE), dots)
    )
    psis_object <- loo_object$psis_object
  }
  set.seed(aligned_seed)
  # The weights come from a density evaluated under the latent state
  # the model inferred at each time, so the prediction they reweight
  # is taken under that same state. brms pairs the two the same way,
  # handing one set of arguments to both `loo()` and the prediction,
  # each carrying its autocorrelation term. A caller naming the
  # surface is left to it.
  preds <- do.call(
    posterior_fn,
    c(list(object, resp = resp),
      list(incl_autocor = surface %||% TRUE), dots)
  )
  # A prediction covers every row of the data; the weights cover only
  # the rows the likelihood could score. Narrow the prediction to those
  # before pairing the two, or a fit with any missing response asks
  # `loo::E_loo()` to weight observations it has no weights for.
  preds <- narrow_to_scored(preds, psis_object)
  if (length(dim(preds)) == 3L) {
    out <- apply(preds, 3L, mvgam_E_loo_normalise,
                  psis_object = psis_object, type = type,
                  probs = probs, simplify = FALSE)
    return(abind::abind(out, rev.along = 0L))
  }
  mvgam_E_loo_normalise(preds, psis_object, type, probs)
}


# Internal: map the prediction function back to the exported wrapper
# the caller actually typed, so the guard names a real function.
#'@noRd
loo_fn_name <- function(posterior_fn) {
  for (nm in c("posterior_predict", "posterior_epred", "posterior_linpred")) {
    if (identical(posterior_fn, get(nm, envir = asNamespace("mvgam")))) {
      return(sub("^posterior_", "loo_", nm))
    }
  }
  "loo_predict"
}


# Internal: per-slab normalisation that mirrors the brms helper
# `E_loo_value`: wraps a vector into a 1-col matrix, transposes
# the quantile matrix into observation-row layout, and assigns
# column labels (`"mean"`, `"var"`, or `"q{prob*100}"`).
#'@noRd
mvgam_E_loo_normalise <- function(preds, psis_object,
                                    type = "mean", probs = 0.5) {
  y <- loo::E_loo(preds, psis_object, type = type,
                   probs = probs)$value
  if (is.matrix(y) && ncol(preds) == ncol(y)) {
    y <- t(y)
  } else if (is.vector(y)) {
    y <- matrix(y)
  }
  labs <- type
  if (identical(type, "quantile")) {
    labs <- paste0("q", probs * 100)
  }
  colnames(y) <- labs
  y
}


#' @rdname mvgam_loo_extras
#' @importFrom brms LOO
#' @method LOO mvgam
#' @export
LOO.mvgam <- function(x, ..., compare = TRUE, resp = NULL,
                       pointwise = FALSE, moment_match = FALSE,
                       reloo = FALSE, k_threshold = 0.7,
                       save_psis = FALSE, moment_match_args = list(),
                       reloo_args = list(), model_names = NULL) {
  loo(x, ..., compare = compare, resp = resp,
      pointwise = pointwise, moment_match = moment_match,
      reloo = reloo, k_threshold = k_threshold,
      save_psis = save_psis, moment_match_args = moment_match_args,
      reloo_args = reloo_args, model_names = model_names)
}


#' @rdname mvgam_loo_extras
#' @importFrom brms WAIC
#' @method WAIC mvgam
#' @export
WAIC.mvgam <- function(x, ..., compare = TRUE, resp = NULL,
                        pointwise = FALSE, model_names = NULL) {
  waic(x, ..., compare = compare, resp = resp,
       pointwise = pointwise, model_names = model_names)
}


#' @rdname mvgam_loo_extras
#' @importFrom brms loo_R2
#' @method loo_R2 mvgam
#' @export
loo_R2.mvgam <- function(object, resp = NULL, summary = TRUE,
                          robust = FALSE, probs = c(0.025, 0.975),
                          seed = NULL, args_epred = list(),
                          args_loglik = list(), ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_string(resp, null.ok = TRUE)
  checkmate::assert_logical(summary, len = 1L)
  checkmate::assert_logical(robust, len = 1L)
  checkmate::assert_numeric(probs, lower = 0, upper = 1, len = 2L)
  checkmate::assert_list(args_epred)
  checkmate::assert_list(args_loglik)
  is_mv <- brms::is.mvbrmsformula(object$formula)
  assert_resp_for_mv(object, resp, "loo_R2")
  local_seed(seed)
  resp_use <- scored_response_name(object, resp)
  y <- object$data[[resp_use]]
  if (is.null(y) || !is.numeric(y)) {
    stop(insight::format_error(c(
      paste0(
        "'loo_R2' requires a numeric response. Response '",
        resp_use, "' is not numeric."
      ),
      i = "LOO Bayesian R^2 is undefined for ordinal / categorical fits."
    )))
  }
  resp_arg <- if (is_mv) list(resp = resp_use) else list()
  # The expectation and the importance weights have to describe the
  # same observation, so both are taken conditional on the latent
  # state the model inferred at that time. Pairing a marginal
  # expectation with weights built from a conditional density is what
  # pinned this statistic at its clamp.
  epred_args <- c(list(object), resp_arg, args_epred)
  if (!"incl_autocor" %in% names(epred_args)) {
    epred_args$incl_autocor <- TRUE
  }
  epred <- do.call(posterior_epred, epred_args)
  ll <- do.call(log_lik, c(list(object), resp_arg, args_loglik))
  # A row with no response contributes no density, so its column is
  # unscorable and importance sampling refuses it. Dropping those
  # columns means the observed values and the expectations paired with
  # them have to lose the same ones, or the three describe different
  # observations.
  ll <- clean_ll(object, ll)
  scored <- attr(ll, "scored_columns")
  y <- y[scored]
  epred <- epred[, scored, drop = FALSE]
  r_eff <- mvgam_r_eff_log_lik(object, ll)
  r2 <- mvgam_loo_R2(y, epred, ll, r_eff)
  colnames(r2) <- "R2"
  if (!isTRUE(summary)) {
    return(r2)
  }
  out <- mvgam_post_summary(r2, robust = robust, probs = probs)
  rownames(out) <- "R2"
  out
}


#' @rdname mvgam_loo_extras
#' @importFrom brms loo_predict
#' @method loo_predict mvgam
#' @export
loo_predict.mvgam <- function(object,
                                type = c("mean", "var", "quantile"),
                                probs = 0.5, psis_object = NULL,
                                resp = NULL, ...) {
  mvgam_loo_E_loo(
    object, posterior_predict, type = type, probs = probs,
    psis_object = psis_object, resp = resp, ...
  )
}


#' @rdname mvgam_loo_extras
#' @importFrom brms loo_epred
#' @method loo_epred mvgam
#' @export
loo_epred.mvgam <- function(object,
                              type = c("mean", "var", "quantile"),
                              probs = 0.5, psis_object = NULL,
                              resp = NULL, ...) {
  mvgam_loo_E_loo(
    object, posterior_epred, type = type, probs = probs,
    psis_object = psis_object, resp = resp, ...
  )
}


#' @rdname mvgam_loo_extras
#' @importFrom brms loo_linpred
#' @method loo_linpred mvgam
#' @export
loo_linpred.mvgam <- function(object,
                                type = c("mean", "var", "quantile"),
                                probs = 0.5, psis_object = NULL,
                                resp = NULL, ...) {
  mvgam_loo_E_loo(
    object, posterior_linpred, type = type, probs = probs,
    psis_object = psis_object, resp = resp, ...
  )
}


#' @rdname mvgam_loo_extras
#' @importFrom brms loo_predictive_interval
#' @method loo_predictive_interval mvgam
#' @export
loo_predictive_interval.mvgam <- function(object, prob = 0.9,
                                            psis_object = NULL, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_number(prob, lower = 0, upper = 1)
  alpha <- (1 - prob) / 2
  # brms's method takes no `resp`, so a multivariate caller supplies
  # it through `...`; it reaches `loo_predict()`, which carries the
  # guard. Declaring it here instead would break signature parity
  # with brms.
  loo_predict(
    object, type = "quantile", probs = c(alpha, 1 - alpha),
    psis_object = psis_object, ...
  )
}


#' @rdname mvgam_loo_extras
#' @importFrom loo loo_subsample
#' @method loo_subsample mvgam
#' @export
loo_subsample.mvgam <- function(x, ..., compare = TRUE, resp = NULL,
                                 model_names = NULL) {
  stop(insight::format_error(c(
    "'loo_subsample()' is not currently supported for 'mvgam' fits.",
    x = paste0(
      "'loo::loo_subsample()' uses the function-based log-likelihood",
      " interface (pointwise = TRUE), which requires a per-observation",
      " log-density callable that mvgam does not yet expose."
    ),
    i = paste0(
      "Use 'loo(x)' on the full log-likelihood matrix; subsampling",
      " offers no efficiency gain when 'log_lik.mvgam' already",
      " returns the full matrix in one call."
    )
  )))
}


#' @rdname mvgam_loo_extras
#' @importFrom loo loo_moment_match
#' @method loo_moment_match mvgam
#' @export
loo_moment_match.mvgam <- function(x, loo = NULL, k_threshold = 0.7,
                                    newdata = NULL, resp = NULL,
                                    check = TRUE, recompile = FALSE,
                                    ...) {
  stop(insight::format_error(c(
    "'loo_moment_match()' is not currently supported for 'mvgam' fits.",
    x = paste0(
      "Moment matching requires re-evaluating the joint log-density",
      " at proposed parameter values via 'unconstrain_pars' /",
      " 'log_prob', which mvgam does not yet expose."
    ),
    i = paste0(
      "Inspect pareto-k diagnostics from 'loo(x)' and refit",
      " excluding problematic observations if needed."
    )
  )))
}


#' @rdname mvgam_loo_extras
#' @importFrom loo loo_model_weights
#' @method loo_model_weights mvgam
#' @export
loo_model_weights.mvgam <- function(x, ..., model_names = NULL) {
  checkmate::assert_class(x, "mvgam")
  dots <- list(...)
  is_model <- vapply(dots, inherits, logical(1), "mvgam")
  models <- c(list(x), dots[is_model])
  rest <- dots[!is_model]
  if (length(rest) > 0L &&
      (is.null(names(rest)) || any(nchar(names(rest)) == 0L))) {
    stop(insight::format_error(c(
      "All non-model arguments to 'loo_model_weights()' must be named.",
      i = paste0(
        "Pass 'method', 'optim_method', 'cores' (etc.) as named",
        " arguments; pass mvgam fits positionally."
      )
    )))
  }
  loos <- lapply(models, loo)
  if (!is.null(model_names)) {
    checkmate::assert_character(
      model_names, len = length(loos), any.missing = FALSE
    )
    names(loos) <- model_names
  }
  do.call(loo::loo_model_weights, c(list(loos), rest))
}


#' @rdname mvgam_loo_extras
#' @importFrom brms add_criterion
#' @method add_criterion mvgam
#' @export
add_criterion.mvgam <- function(x, criterion, model_name = NULL,
                                 overwrite = FALSE, file = NULL,
                                 force_save = FALSE, ...) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_character(criterion, min.len = 1L)
  checkmate::assert_string(model_name, null.ok = TRUE)
  checkmate::assert_logical(overwrite, len = 1L)
  checkmate::assert_string(file, null.ok = TRUE)
  supported <- c("loo", "waic", "loo_subsample",
                 "bayes_R2", "loo_R2")
  bad <- setdiff(criterion, supported)
  if (length(bad) > 0L) {
    stop(insight::format_error(c(
      "Unsupported criterion requested.",
      x = paste0(
        "Unrecognised: ",
        paste0("'", bad, "'", collapse = ", "), "."
      ),
      i = paste0(
        "Supported: ",
        paste0("'", supported, "'", collapse = ", "), "."
      )
    )))
  }
  if (!is.null(model_name)) {
    x$model_name <- model_name
  }
  if (is.null(x$criteria)) {
    x$criteria <- list()
  }
  for (cname in criterion) {
    if (isTRUE(overwrite) || is.null(x$criteria[[cname]])) {
      x$criteria[[cname]] <- match.fun(cname)(x, ...)
    }
  }
  if (!is.null(file)) {
    saveRDS(x, file = paste0(file, ".rds"))
  }
  x
}
