#' LOO / WAIC extras and aliases for mvgam models
#'
#' brms-parity S3 methods that compose mvgam's existing prediction
#' and log-likelihood primitives ([`log_lik.mvgam`], [`loo.mvgam`],
#' [`waic.mvgam`], [`posterior_epred.mvgam`],
#' [`posterior_predict.mvgam`]) with helpers from the
#' \pkg{loo} package.
#'
#' @name mvgam_loo_extras
#' @aliases LOO.mvgam WAIC.mvgam loo_R2.mvgam loo_predict.mvgam
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
#' @param type For `loo_predict.mvgam`, one of `"mean"`, `"var"`,
#'   or `"quantile"`.
#' @param psis_object Optional precomputed `loo::psis` object.
#' @param compare Logical. If `TRUE`, compare multiple models.
#' @param pointwise Logical. Pointwise streaming mode (not
#'   supported; passing `TRUE` raises an error).
#' @param moment_match,reloo Logical. Advanced LOO post-processing
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
#'   [brms::add_criterion()], [loo::loo_subsample()],
#'   [loo::loo_model_weights()], [loo::E_loo()],
#'   [loo::psis()].
#'
#' @author Nicholas J Clark
NULL


# Internal: relative effective sample size for a per-observation
# log-likelihood matrix, using the chain layout of `x$fit`. Mirrors
# brms's `r_eff_log_lik` helper. Shared by every LOO-extras method
# that needs to build a PSIS object.
#'@noRd
mvgam_r_eff_log_lik <- function(x, ll) {
  chains <- posterior::nchains(posterior::as_draws_array(x$fit))
  n_per_chain <- NROW(ll) / chains
  loo::relative_eff(
    exp(ll),
    chain_id = sort(rep(seq_len(chains), n_per_chain))
  )
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


#' @rdname mvgam_loo_extras
#' @importFrom brms LOO
#' @method LOO mvgam
#' @export LOO
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
#' @export WAIC
#' @export
WAIC.mvgam <- function(x, ..., compare = TRUE, resp = NULL,
                        pointwise = FALSE, model_names = NULL) {
  waic(x, ..., compare = compare, resp = resp,
       pointwise = pointwise, model_names = model_names)
}


#' @rdname mvgam_loo_extras
#' @importFrom brms loo_R2
#' @method loo_R2 mvgam
#' @export loo_R2
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
  if (is_mv && is.null(resp)) {
    stop(insight::format_error(c(
      "'loo_R2' requires 'resp' for multivariate models.",
      i = paste0(
        "Available responses: ",
        paste(shQuote(object$response_names), collapse = ", "), "."
      )
    )))
  }
  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv)) {
      rng_old <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", rng_old, envir = .GlobalEnv))
    }
    set.seed(seed)
  }
  resp_use <- if (is.null(resp)) object$response_names[1L] else resp
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
  epred <- do.call(
    posterior_epred,
    c(list(object), resp_arg, args_epred)
  )
  ll <- do.call(log_lik, c(list(object), resp_arg, args_loglik))
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
#' @export loo_predict
#' @export
loo_predict.mvgam <- function(object,
                                type = c("mean", "var", "quantile"),
                                probs = 0.5, psis_object = NULL,
                                resp = NULL, ...) {
  checkmate::assert_class(object, "mvgam")
  type <- match.arg(type)
  if (is.null(psis_object)) {
    message("Running PSIS to compute weights")
    loo_object <- loo(object, resp = resp, save_psis = TRUE, ...)
    psis_object <- loo_object$psis_object
  }
  preds <- posterior_predict(object, resp = resp, ...)
  loo::E_loo(preds, psis_object, type = type, probs = probs)$value
}


#' @rdname mvgam_loo_extras
#' @importFrom loo loo_subsample
#' @method loo_subsample mvgam
#' @export loo_subsample
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
#' @export loo_moment_match
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
#' @export loo_model_weights
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
#' @export add_criterion
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
