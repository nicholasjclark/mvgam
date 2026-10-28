#' Leave-one-out predictions, R-squared and criteria for mvgam models
#'
#' The \pkg{brms} methods that take their weights from [loo.mvgam()].
#' `loo_predict()`,
#' `loo_epred()` and `loo_linpred()` reweight posterior predictions by
#' the PSIS weights of each observation, approximating the prediction
#' made without it. `loo_predictive_interval()` gives the
#' leave-one-out predictive interval. `loo_R2()` is the Bayesian R^2 of
#' Gelman et al. (2019) computed from leave-one-out predictions.
#' `add_criterion()` stores criteria on the fit. `LOO()` and `WAIC()`
#' are the brms aliases of [loo.mvgam()] and [waic.mvgam()].
#'
#' @section The observations scored:
#' The weights and the predictions describe the observation the
#' likelihood scored. For most families that is a row of the data. The
#' detection families ([occ()], [nmix()]) write their likelihood per
#' closure unit, marginalising its visits. `loo_predict()`,
#' `loo_epred()`, `loo_predictive_interval()` and `loo_R2()` then sum a
#' unit's visits and return one value per unit, in the column order of
#' [log_lik.mvgam()]. `loo_linpred()` is refused for them. A sum of
#' linear predictors over visits has no interpretation.
#'
#' [mvn()] and [mvt()] keep one density per row. The composition
#' families ([diri()], [multi()], [categ()]) hold one joint density per
#' site. These methods refuse them.
#'
#' @name mvgam_loo_extras
#' @aliases LOO.mvgam WAIC.mvgam loo_R2.mvgam loo_predict.mvgam
#'   loo_epred.mvgam loo_linpred.mvgam loo_predictive_interval.mvgam
#'   loo_subsample.mvgam loo_moment_match.mvgam
#'   loo_model_weights.mvgam add_criterion.mvgam
#'
#' @inheritParams predict.mvgam
#' @inheritParams compare_scores
#' @param x,object A fitted `mvgam` object.
#' @param resp Optional name of one response of a multivariate model.
#' @param summary Logical. `TRUE` (the default) returns the summary
#'   matrix with `Estimate`, `Est.Error` and interval columns. `FALSE`
#'   returns the draws.
#' @param seed Optional integer seed. The session's random state is
#'   restored when the call returns.
#' @param args_epred,args_loglik Lists of further arguments to
#'   [posterior_epred.mvgam()] and [log_lik.mvgam()] for `loo_R2()`.
#' @param type The expectation: `"mean"` (the default), `"var"` or
#'   `"quantile"`.
#' @param prob Interval mass of `loo_predictive_interval()`, 0.9 by
#'   default as in brms.
#' @param psis_object Optional `psis` object from
#'   `loo(x, save_psis = TRUE)`, reused across calls.
#' @param compare,pointwise,moment_match,reloo,k_threshold,save_psis,moment_match_args,reloo_args
#'   As in [loo.mvgam()], which describes the two it uses and refuses
#'   the rest.
#' @param criterion Character vector of criteria for `add_criterion()`:
#'   `"loo"`, `"waic"`, `"bayes_R2"` or `"loo_R2"`.
#' @param model_name Optional name stored in `x$model_name`.
#' @param overwrite Logical. `TRUE` recomputes criteria `x$criteria`
#'   already holds.
#' @param file Optional path without extension. The fit is saved to
#'   `paste0(file, ".rds")` with its criteria.
#' @param force_save Unused. Accepted for the brms signature.
#' @param loo,newdata,check,recompile Accepted for the brms signature of
#'   `loo_moment_match()`, which mvgam refuses.
#' @param ... Further arguments to the method each function calls.
#'   `loo_R2()` refuses them.
#'
#' @return `loo_predict()`, `loo_epred()` and `loo_linpred()` return a
#'   matrix with one row per scored observation and a column per
#'   expectation. `loo_predictive_interval()` returns its two quantile
#'   columns. `loo_R2()` returns a one-row summary, or its draws with
#'   `summary = FALSE`. `add_criterion()` returns `x` with the criteria
#'   in `x$criteria`. `loo_model_weights()` returns the weights of
#'   [loo::loo_model_weights()].
#'
#' @references
#' Gelman, A., Goodrich, B., Gabry, J. and Vehtari, A. (2019).
#' R-squared for Bayesian regression models. \emph{The American
#' Statistician}, 73(3):307-309. \doi{10.1080/00031305.2018.1549100}
#'
#' @seealso [loo.mvgam()] estimates the ELPD these weights come from.
#'   [bayes_R2.mvgam()] is the in-sample R^2. [lfo_cv.mvgam()] scores
#'   forecasts of data the model has not seen.
#'
#' @template ex-loo-models
#' @examples
#' \dontrun{
#' # The leave-one-out R^2 is lower than the in-sample one
#' loo_R2(mod)
#' bayes_R2(mod)
#'
#' # One PSIS object serves every leave-one-out prediction
#' psis <- loo(mod, save_psis = TRUE)$psis_object
#' head(loo_predict(mod, psis_object = psis))
#' head(loo_predictive_interval(mod, psis_object = psis))
#'
#' # Next, loo_compare() ranks models by their ELPD
#' }
#'
#' @author Nicholas J Clark
#' @family model comparison
NULL


#' Relative effective sample sizes of a log-likelihood matrix
#'
#' Follows the internal brms function `r_eff_log_lik()`.
#' `loo::relative_eff()` needs the same number of draws from each
#' chain. A subsample that breaks the balance gets a relative
#' efficiency of 1, the loo value for independent draws.
#'
#' @param x Fitted `mvgam` object
#' @param ll Numeric log-likelihood matrix, one row per draw
#' @param draw_ids Integer indices of the retained draws into the
#'   chain-major draws, or `NULL` when no subsample was taken
#' @return Numeric vector with one relative efficiency per column of
#'   `ll`
#' @noRd
mvgam_r_eff_log_lik <- function(x, ll, draw_ids = NULL) {
  checkmate::assert_class(x, "mvgam")
  checkmate::assert_matrix(ll)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE)
  n_obs <- NCOL(ll)
  chain_id <- mvgam_chain_id(
    posterior_chain_layout(x), NROW(ll), draw_ids
  )
  if (is.null(chain_id)) {
    return(rep(1, n_obs))
  }
  without_ess_cap_notice(loo::relative_eff(exp(ll), chain_id = chain_id))
}


#' How a fit's posterior divides into chains
#'
#' The draws of `lp__` give both counts. A `run_model = FALSE` fit
#' holds no posterior and returns `NULL`.
#'
#' @param x Fitted `mvgam` object
#' @return List holding the number of `chains` and the `total` number
#'   of draws, or `NULL` for a fit without a posterior
#' @noRd
posterior_chain_layout <- function(x) {
  checkmate::assert_class(x, "mvgam")
  if (is.null(x$fit)) {
    return(NULL)
  }
  arr <- fit_draws_shape(x$fit)
  list(chains = posterior::nchains(arr), total = posterior::ndraws(arr))
}


#' The chain of each retained draw
#'
#' @param layout List from `posterior_chain_layout()`, or `NULL`
#' @param n_draws Integer number of retained draws
#' @param draw_ids Integer indices of the retained draws, or `NULL`
#' @return Integer vector of chain indices grouped by chain, or `NULL`
#'   when a balanced chain layout cannot be recovered
#' @noRd
mvgam_chain_id <- function(layout, n_draws, draw_ids = NULL) {
  checkmate::assert_list(layout, null.ok = TRUE)
  checkmate::assert_int(n_draws, lower = 0L)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE)
  chains <- layout$chains
  if (!isTRUE(chains > 1L)) {
    return(NULL)
  }
  if (is.null(draw_ids)) {
    if (n_draws %% chains != 0L) {
      return(NULL)
    }
    return(rep(seq_len(chains), each = n_draws %/% chains))
  }
  total <- layout$total
  if (is.null(total) || is.na(total) || total %% chains != 0L) {
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
  # `relative_eff()` takes the draws in blocks of one chain
  if (is.unsorted(cid)) {
    return(NULL)
  }
  cid
}


#' Bayesian LOO R-squared draws
#'
#' Follows Gelman et al. (2019), ported from the internal brms function
#' `.loo_R2()`. Bayesian-bootstrap weights give one value per draw.
#' Values are clamped to `[-1, 1]`.
#'
#' @param y Numeric vector of `N` observed responses
#' @param epred Numeric matrix of expected predictions with dimensions
#'   `[S, N]`
#' @param ll Numeric log-likelihood matrix with dimensions `[S, N]`
#' @param r_eff Numeric vector of relative efficiencies, one per
#'   observation
#' @return Numeric matrix with one column and `S` rows
#' @noRd
mvgam_loo_R2 <- function(y, epred, ll, r_eff) {
  checkmate::assert_numeric(y)
  checkmate::assert_matrix(epred, ncols = length(y))
  checkmate::assert_matrix(ll, ncols = length(y))
  checkmate::assert_numeric(r_eff)
  psis_obj <- loo::psis(log_ratios = -ll, r_eff = r_eff)
  yhat_loo <- loo::E_loo(epred, psis_obj, log_ratios = -ll)$value
  err <- yhat_loo - y
  S <- nrow(epred)
  N <- ncol(epred)
  # Bayesian bootstrap weights, Dirichlet(1, ..., 1) from normalised
  # exponentials, as in brms
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


#' Carry a per-row value onto the columns of `log_lik()`
#'
#' `log_lik()` has one column per row, or one per closure unit for a
#' family that marginalises a latent state over a unit's visits. A
#' per-row value such as a fold key or a series label is taken at each
#' unit's first row. Every row of a unit shares a grouping value. The
#' family decides the grain. Two groupings of the same rows can share a
#' column count.
#'
#' @param object Fitted `mvgam` object
#' @param data Data frame `x` describes
#' @param x Vector with one value per row of `data`
#' @param n_cols Integer number of log-likelihood columns to check
#'   against, or `NULL`
#' @return `x` with one value per log-likelihood column
#' @noRd
loglik_col_values <- function(object, data, x, n_cols = NULL) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(data)
  checkmate::assert_atomic_vector(x, len = nrow(data))
  checkmate::assert_int(n_cols, lower = 0L, null.ok = TRUE)
  if (is_closure_unit_family(object$family)) {
    arrays <- closure_unit_arrays_for(object, data)
    out <- x[closure_unit_first_rows(arrays)]
    label <- "closure units"
  } else {
    out <- x
    label <- "rows"
  }
  if (!is.null(n_cols) && length(out) != n_cols) {
    stop(insight::format_error(c(
      "The log-likelihood and the frame cover different observations.",
      x = paste0("'log_lik()' returned ", n_cols, " columns for ",
                 length(out), " ", label, "."),
      i = paste0(
        "The frame passed here differs from the one the model was ",
        "fitted on."
      )
    )), call. = FALSE)
  }
  out
}


#' Put a per-row quantity on the grain `log_lik()` scores
#'
#' A prediction has one value per row. A detection family's likelihood
#' has one per closure unit. The prediction is summed within each unit
#' before `narrow_to_scored()` selects the scored columns.
#'
#' @param object Fitted `mvgam` object
#' @param data Data frame `x` describes
#' @param x Per-row vector, or `[ndraws, nobs]` matrix
#' @return `x` with one value or column per log-likelihood column
#' @noRd
at_loglik_grain <- function(object, data, x) {
  if (!is_closure_unit_family(object$family)) {
    return(x)
  }
  sum_within_closure_units(closure_unit_arrays_for(object, data), x)
}


#' Refuse a LOO pairing this family's grain cannot carry
#'
#' Each method reweights a prediction by the importance weights of the
#' same observation's density. A detection family scores one density
#' per closure unit. Its expectation and predictive draws sum over a
#' unit's visits. A sum of linear predictors has no meaning. A
#' composition scores one joint density per site, with no scalar
#' prediction to reweight.
#'
#' @param object Fitted `mvgam` object
#' @param fn_name The exported method name, from `loo_fn_name()`
#' @return `TRUE`, invisibly, or an error
#' @noRd
require_loo_pairing <- function(object, fn_name) {
  family <- object$family
  if (is_simplex_response_family(family)) {
    stop(insight::format_error(c(
      paste0("'", fn_name, "' is not available for family '",
             resolve_family_name(family), "'."),
      x = paste0("The family scores one joint density per site across ",
                 "the categories measured there."),
      i = paste0(
        "Use 'loo()' to score each site or 'posterior_epred()' ",
        "for the probability of each category."
      )
    )), call. = FALSE)
  }
  if (is_closure_unit_family(family) &&
        identical(fn_name, "loo_linpred")) {
    stop(insight::format_error(c(
      paste0("'", fn_name, "' is not available for family '",
             resolve_family_name(family), "'."),
      x = "The likelihood is scored once per closure unit.",
      x = "A linear predictor has one value per visit.",
      i = paste0("Use 'loo_epred()' or 'loo_predict()' to aggregate a ",
                 "unit's visits as the per-unit density does.")
    )), call. = FALSE)
  }
  invisible(TRUE)
}


#' Narrow anything paired with a scored log-likelihood
#'
#' `clean_ll()` drops the columns of missing responses and records the
#' kept ones in `scored_columns`. The same columns are selected from
#' `x`. An `x` of the scored width is returned unchanged. The
#' observation axis is the second dimension of a `[ndraws, nobs]`
#' matrix or `[ndraws, nobs, k]` array. A vector's is its length.
#'
#' @param x Matrix, array, or per-observation vector
#' @param scored_from The object carrying the record: a PSIS object,
#'   or the narrowed log-likelihood matrix itself
#' @return `x` restricted to the scored observations
#'
#' @noRd
narrow_to_scored <- function(x, scored_from) {
  obs_axis <- function(z) {
    d <- dim(z)
    if (is.null(d)) length(z) else d[2L]
  }
  n_scored <- obs_axis(scored_from)
  n_x <- obs_axis(x)
  if (is.null(n_x) || is.null(n_scored) || n_x == n_scored) {
    return(x)
  }
  scored <- attr(scored_from, "scored_columns")
  if (is.null(scored) || length(scored) != n_scored ||
      max(scored) > n_x) {
    stop(insight::format_error(c(
      "Paired quantities cover different observations.",
      x = paste0("Supplied ", n_x, " observations and scored ",
                 n_scored, "."),
      i = paste0(
        "A 'psis_object' built from a different model or response ",
        "causes this."
      ),
      i = "Leave 'psis_object' unset to have it computed here."
    )), call. = FALSE)
  }
  if (is.null(dim(x))) {
    return(x[scored])
  }
  if (length(dim(x)) == 3L) {
    return(x[, scored, , drop = FALSE])
  }
  x[, scored, drop = FALSE]
}


#' PSIS-weighted expectations of a prediction function
#'
#' `loo_predict()`, `loo_epred()` and `loo_linpred()` call it. Without a
#' `psis_object`, one is built with `loo(save_psis = TRUE)`. The weights
#' and the predictions both condition on the inferred latent state. Both
#' calls run under one fixed seed, making a repeated call reproducible.
#' The caller's random state is restored on exit.
#'
#' @param object Fitted `mvgam` object
#' @param posterior_fn Prediction function, one of `posterior_predict`,
#'   `posterior_epred` and `posterior_linpred`
#' @param type Character expectation type, `"mean"`, `"var"` or
#'   `"quantile"`
#' @param probs Numeric quantile probabilities for `type = "quantile"`
#' @param psis_object PSIS object, or `NULL` to compute one
#' @param resp Character response name, or `NULL`
#' @param ... Further arguments to the prediction and `loo()` calls
#' @return Numeric matrix with one row per scored observation, or a
#'   three-dimensional array stacking one such matrix per response
#' @noRd
mvgam_loo_E_loo <- function(object, posterior_fn,
                             type = c("mean", "var", "quantile"),
                             probs = 0.5, psis_object = NULL,
                             resp = NULL, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_function(posterior_fn)
  fn_name <- loo_fn_name(posterior_fn)
  resolve_resp(object, resp, required = TRUE,
               caller = paste0(fn_name, "()"))
  # The family alone decides the refusal. PSIS is not run first.
  require_loo_pairing(object, fn_name)
  type <- match.arg(type)
  # `local_seed()` restores the caller's stream. Each `set.seed()` below
  # restarts one stream before the weighting and before the prediction.
  aligned_seed <- 1L
  local_seed(aligned_seed)
  # `incl_autocor` selects the prediction surface. The weighting call
  # omits it. `loo::loo()` would refuse it.
  dots <- list(...)
  surface <- dots$incl_autocor
  dots$incl_autocor <- NULL
  if (is.null(psis_object)) {
    rlang::inform("Running PSIS to compute weights")
    set.seed(aligned_seed)
    loo_object <- do.call(
      loo, c(list(object, resp = resp, save_psis = TRUE), dots)
    )
    psis_object <- loo_object$psis_object
  }
  set.seed(aligned_seed)
  # The weights come from densities under the inferred latent state.
  # The prediction they reweight is taken under the same state unless
  # the caller names another.
  preds <- do.call(
    posterior_fn,
    c(list(object, resp = resp),
      list(incl_autocor = surface %||% TRUE), dots)
  )
  # One weight per scored row, or per closure unit
  preds <- at_loglik_grain(object, object$data, preds)
  preds <- narrow_to_scored(preds, psis_object)
  if (length(dim(preds)) == 3L) {
    out <- apply(preds, 3L, mvgam_E_loo_normalise,
                  psis_object = psis_object, type = type,
                  probs = probs, simplify = FALSE)
    return(abind::abind(out, rev.along = 0L))
  }
  mvgam_E_loo_normalise(preds, psis_object, type, probs)
}


#' The exported name of the LOO wrapper a prediction function serves
#'
#' Lets a refusal name the function the caller typed.
#'
#' @param posterior_fn Prediction function
#' @return Character scalar such as `"loo_epred"`
#' @noRd
loo_fn_name <- function(posterior_fn) {
  checkmate::assert_function(posterior_fn)
  for (nm in c("posterior_predict", "posterior_epred", "posterior_linpred")) {
    if (identical(posterior_fn, get(nm, envir = asNamespace("mvgam")))) {
      return(sub("^posterior_", "loo_", nm))
    }
  }
  "loo_predict"
}


#' One PSIS-weighted expectation shaped as a labelled matrix
#'
#' Follows the internal brms function `E_loo_value()`. A vector becomes
#' a one-column matrix. A quantile matrix is transposed to one row per
#' observation. Columns are labelled `"mean"`, `"var"` or `"q<prob>"`.
#'
#' @param preds Numeric matrix of predictions, one row per draw
#' @param psis_object PSIS object
#' @param type Character expectation type, `"mean"`, `"var"` or
#'   `"quantile"`
#' @param probs Numeric quantile probabilities
#' @return Numeric matrix with one row per observation
#' @noRd
mvgam_E_loo_normalise <- function(preds, psis_object,
                                    type = "mean", probs = 0.5) {
  checkmate::assert_matrix(preds)
  checkmate::assert_class(psis_object, "psis")
  checkmate::assert_choice(type, c("mean", "var", "quantile"))
  checkmate::assert_numeric(probs, lower = 0, upper = 1)
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
  # As in brms, the call is replayed on `loo()` with only the arguments
  # the caller wrote. `loo.mvgam()` refuses some brms arguments when
  # they are supplied.
  cl <- match.call()
  cl[[1L]] <- quote(loo::loo)
  eval(cl, parent.frame())
}


#' @rdname mvgam_loo_extras
#' @importFrom brms WAIC
#' @method WAIC mvgam
#' @export
WAIC.mvgam <- function(x, ..., compare = TRUE, resp = NULL,
                        pointwise = FALSE, model_names = NULL) {
  # Replayed on `waic()` as `LOO.mvgam()` replays its call on `loo()`
  cl <- match.call()
  cl[[1L]] <- quote(loo::waic)
  eval(cl, parent.frame())
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
  checkmate::assert_flag(summary)
  checkmate::assert_flag(robust)
  checkmate::assert_numeric(probs, lower = 0, upper = 1, len = 2L)
  checkmate::assert_list(args_epred)
  checkmate::assert_list(args_loglik)
  rlang::check_dots_empty()
  resolve_resp(object, resp, required = TRUE, caller = "loo_R2()")
  require_loo_pairing(object, "loo_R2")
  local_seed(seed)
  data_used <- object$data
  y <- r2_response(object, resp, "loo_R2()")
  resp_arg <- r2_resp_arg(object, resp)
  # The expectation conditions on the inferred latent state, as the
  # density giving the weights does
  epred_args <- c(list(object), resp_arg, args_epred)
  if (!"incl_autocor" %in% names(epred_args)) {
    epred_args$incl_autocor <- TRUE
  }
  epred <- do.call(posterior_epred, epred_args)
  ll <- do.call(log_lik, c(list(object), resp_arg, args_loglik))
  # The response and the expectation are summed to the likelihood's
  # grain. They then lose the columns `clean_ll()` drops.
  ll <- clean_ll(ll)
  y <- at_loglik_grain(object, data_used, y)
  epred <- at_loglik_grain(object, data_used, epred)
  y <- narrow_to_scored(y, ll)
  epred <- narrow_to_scored(epred, ll)
  r_eff <- mvgam_r_eff_log_lik(object, ll)
  r2_result(mvgam_loo_R2(y, epred, ll, r_eff), summary, robust, probs)
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
  # The brms method has no `resp` argument. A multivariate caller
  # passes it through `...` to `loo_predict()`.
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
    "'loo_subsample()' is not supported for 'mvgam' fits.",
    x = paste0(
      "'loo::loo_subsample()' needs a log-density function for each",
      " observation. mvgam does not expose one."
    ),
    i = "Use 'loo(x)' on the full log-likelihood matrix."
  )), call. = FALSE)
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
    "'loo_moment_match()' is not supported for 'mvgam' fits.",
    x = paste0(
      "Moment matching evaluates the log density at new parameter ",
      "values."
    ),
    x = "mvgam has no 'log_prob' or 'unconstrain_pars' method.",
    i = paste0(
      "Inspect the Pareto k diagnostics from 'loo(x)'. 'kfold()' ",
      "refits the model for the observations PSIS approximates poorly."
    )
  )), call. = FALSE)
}


#' @rdname mvgam_loo_extras
#' @importFrom loo loo_model_weights
#' @method loo_model_weights mvgam
#' @export
loo_model_weights.mvgam <- function(x, ..., model_names = NULL) {
  checkmate::assert_class(x, "mvgam")
  split <- mvgam_split_models(x, ..., model_names = model_names)
  loos <- lapply(split$models, loo)
  do.call(loo::loo_model_weights, c(list(loos), split$other))
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
  checkmate::assert_flag(overwrite)
  checkmate::assert_string(file, null.ok = TRUE)
  supported <- c("loo", "waic", "bayes_R2", "loo_R2")
  bad <- setdiff(criterion, supported)
  if (length(bad) > 0L) {
    stop(insight::format_error(c(
      paste0(paste0("'", bad, "'", collapse = ", "),
             if (length(bad) > 1L) " are" else " is",
             " not supported by 'add_criterion()'."),
      i = paste0("Supported criteria: ",
                 paste0("'", supported, "'", collapse = ", "), "."),
      i = "'kfold()' and 'lfo_cv()' return their own results."
    )), call. = FALSE)
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
