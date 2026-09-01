#' Draws from the Posterior Distribution of an mvgam Model
#'
#' Compute posterior predictions for mvgam models. Dispatches by `type`
#' to the appropriate prediction surface:
#' [posterior_predict.mvgam()] for `"response"` (default),
#' [posterior_linpred.mvgam()] for `"link"`,
#' [posterior_epred.mvgam()] for `"expected"`, and an internal
#' family-aware helper for `"variance"`.
#'
#' @param object An object of class `mvgam`.
#' @param newdata An optional data.frame containing new predictor values.
#'   If `NULL` (the default), the original training data is used.
#' @param type Character; one of `"response"` (default), `"link"`,
#'   `"expected"`, `"variance"`, `"terms"`, `"latent_state"`,
#'   `"detection"`. See Details.
#' @inheritParams posterior_epred.mvgam
#' @param ndraws Positive integer specifying the number of posterior draws
#'   to use. If `NULL` (the default), all draws are used.
#' @param draw_ids Integer vector specifying which draws to use. If `NULL`,
#'   draws are selected based on `ndraws`.
#' @param re_formula Formula for random effects. If `NULL` (the default),
#'   all random effects are included. Use `NA` to exclude all random
#'   effects.
#' @param allow_new_levels Logical; accepted for brms compatibility.
#'   A grouping level the model never saw is refused whatever this is
#'   set to, because a new level has no fitted random effect and, for
#'   a new series, no latent state to propagate. Predict for levels
#'   the fit knows, or refit with the new levels included.
#' @param sample_new_levels Character; accepted for brms
#'   compatibility and not used, since new levels are refused. See
#'   `allow_new_levels`.
#' @param resp Character specifying which response variable to predict for
#'   multivariate models. If `NULL`, predictions are returned for all
#'   responses.
#' @param summary Logical. If `TRUE` (the default), returns summary
#'   statistics. If `FALSE`, returns the full matrix of posterior draws.
#' @param robust Logical. If `FALSE` (the default), uses mean and standard
#'   deviation for summaries. If `TRUE`, uses median and median absolute
#'   deviation (MAD).
#' @param probs Numeric vector of probabilities for quantile computation.
#'   Default is `c(0.025, 0.975)` for 95% credible intervals. Can be a
#'   single value or multiple values.
#' @param ... Additional arguments forwarded to the underlying
#'   `posterior_predict` / `posterior_epred` / `posterior_linpred` method.
#'
#' @return If `summary = FALSE`, returns a numeric matrix of posterior
#'   draws with dimensions ``\\[ndraws x nobs\\]``. For multivariate fits with
#'   `resp = NULL`, returns a named list of such matrices.
#'
#'   If `summary = TRUE`, returns a matrix with columns:
#'   \itemize{
#'     \item `Estimate`: point estimate (mean or median depending on
#'       `robust`)
#'     \item `Est.Error`: uncertainty estimate (sd or mad depending on
#'       `robust`)
#'     \item `Q*`: quantile columns corresponding to values in `probs`
#'   }
#'
#' @details
#' The `type` argument selects which posterior summary to return:
#' \itemize{
#'   \item `"response"` (default): draws from the posterior predictive
#'     distribution on the outcome scale. Routes through
#'     [posterior_predict.mvgam()].
#'   \item `"link"`: linear predictor draws on the link scale (no
#'     inverse-link applied). Routes through
#'     [posterior_linpred.mvgam()].
#'   \item `"expected"`: posterior expectation E\[Y | X\] on the response
#'     scale. Routes through [posterior_epred.mvgam()].
#'   \item `"variance"`: conditional variance Var\[Y | theta\] per draw,
#'     using family-specific mean-variance formulas. Supported
#'     families: gaussian, student, lognormal, poisson, bernoulli,
#'     binomial, negbinomial, gamma, beta. Other families raise an
#'     informative error. Under the default the variance is
#'     conditional on the parameter draw alone, so it is the
#'     observation noise around the mean the two submodels give.
#'     `process_error = TRUE` conditions on that draw's sampled
#'     trend innovation as well, giving the observation noise at
#'     the trend-integrated mean.
#'   \item `"terms"`: accepted so that a call carried over from
#'     [mgcv::predict.gam()] is answered rather than silently
#'     mismatched. mvgam splits the decomposition across
#'     [posterior_smooths.mvgam()] for per-smooth draws and
#'     [fixef.mvgam()] / [ranef.mvgam()] for the parametric and
#'     random-effect components, so the call errors with a pointer
#'     at those.
#'   \item `"latent_state"`: closure-unit families. Family-aware
#'     posterior of the latent state per closure unit, conditioned
#'     on the observed detection history:
#'     \itemize{
#'       \item `nmix()`: posterior latent abundance `N`
#'         conditional on the observed visit counts, via
#'         `posterior_latent_N()` (Royle 2004).
#'       \item `occ()`: posterior occupancy probability
#'         `P(z = 1 | y_g)` via `posterior_occupancy()`. Returns
#'         the probability matrix by default; use
#'         `posterior_occupancy(draw = TRUE)` directly for 0/1 z
#'         draws.
#'     }
#'   \item `"detection"`: closure-unit families (nmix(), occ()).
#'     Per-visit detection probability \eqn{p_{g,j}} on the response
#'     scale.
#' }
#'
#' @seealso [posterior_predict.mvgam()], [posterior_epred.mvgam()],
#'   [posterior_linpred.mvgam()], [fitted.mvgam()],
#'   [posterior_smooths.mvgam()].
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # Default: summary matrix with columns Estimate, Est.Error,
#' # Q2.5, Q97.5 on the response scale (counts here).
#' head(predict(mod))
#'
#' # `type` controls the scale of the prediction. The four most
#' # common views, all on the same fit, summarise the predictive
#' # cascade from the linear predictor to the observed response.
#' # `link` is the linear predictor (log-rate for Poisson);
#' # `expected` is E[Y | parameters] (mean count); `response` is
#' # a posterior predictive draw of Y; `variance` is the
#' # conditional variance of Y given the parameters.
#' head(predict(mod, type = "link"))
#' head(predict(mod, type = "expected"))
#' head(predict(mod, type = "response"))
#' head(predict(mod, type = "variance"))
#'
#' # Raw posterior draws (rows = draws, cols = observations).
#' # Useful for any downstream scoring or calibration check that
#' # needs the full posterior, not just a summary.
#' draws <- predict(mod, summary = FALSE)
#' dim(draws)
#' }
#'
#' @export
predict.mvgam <- function(object,
                          newdata = NULL,
                          type = c("response", "link", "expected",
                                   "variance", "terms", "latent_state",
                                   "detection"),
                          process_error = FALSE,
                          incl_autocor = FALSE,
                          ndraws = NULL,
                          draw_ids = NULL,
                          re_formula = NULL,
                          allow_new_levels = FALSE,
                          sample_new_levels = "uncertainty",
                          resp = NULL,
                          summary = TRUE,
                          robust = FALSE,
                          probs = c(0.025, 0.975),
                          ...) {

  # Input validation
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  type <- match.arg(type)
  checkmate::assert_logical(process_error, len = 1, any.missing = FALSE)
  checkmate::assert_logical(incl_autocor, len = 1, any.missing = FALSE)
  checkmate::assert_int(ndraws, lower = 1, null.ok = TRUE)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE)
  checkmate::assert(
    checkmate::check_class(re_formula, "formula"),
    checkmate::check_true(is.na(re_formula)),
    checkmate::check_null(re_formula)
  )
  checkmate::assert_logical(allow_new_levels, len = 1, any.missing = FALSE)
  checkmate::assert_choice(
    sample_new_levels,
    choices = c("uncertainty", "gaussian", "old_levels")
  )
  checkmate::assert_string(resp, null.ok = TRUE)
  checkmate::assert_logical(summary, len = 1, any.missing = FALSE)
  checkmate::assert_logical(robust, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(
    probs,
    lower = 0,
    upper = 1,
    min.len = 1,
    any.missing = FALSE
  )

  # A requested count becomes indices here, at the boundary, so that
  # nothing below is ever handed a bare count and left to choose its
  # own draws. Every extraction the answer is assembled from then
  # reads the same iterations.
  draw_ids <- resolve_draw_ids(object, ndraws, draw_ids)
  if (!is.null(draw_ids)) {
    ndraws <- NULL
  }

  # Closure-unit family types: dispatch via the per-family
  # extractors in families.R. Each family registers its valid
  # `type` strings on `attr(family, "mvgam_predict_types")`. Both
  # nmix() and occ() expose c("latent_state", "detection"). The
  # generic "latent_state" token routes through
  # `dispatch_closure_unit_method(family, "latent_state")` to the
  # family-aware kernel: `posterior_latent_N()` for nmix returning
  # posterior `N`; `posterior_occupancy()` for occ returning
  # posterior `psi`.
  if (type %in% c("latent_state", "detection")) {
    family_types <- attr(object$family, "mvgam_predict_types",
                          exact = TRUE) %||% character(0)
    if (!is_closure_unit_family(object$family) ||
        !(type %in% family_types)) {
      stop(insight::format_error(c(
        paste0("type = '", type,
               "' is not available for this family."),
        x = paste0(
          "Family '", resolve_family_name(object$family),
          "' exposes types: ",
          if (length(family_types) > 0L) {
            paste(paste0("'", family_types, "'"), collapse = ", ")
          } else {
            "none (not a closure-unit family)"
          },
          "."
        ),
        i = "Refit with family = nmix() or family = occ() to enable closure-unit predict types."
      )))
    }
    pred <- if (identical(type, "latent_state")) {
      kernel <- dispatch_closure_unit_method(
        object$family, "latent_state"
      )
      # posterior_occupancy() carries an extra `draw` arg:
      # FALSE returns the probability matrix, TRUE returns 0/1
      # z draws. At the predict() call site we want probability.
      kernel_formals <- names(formals(kernel))
      kernel_args <- list(
        object   = object,
        newdata  = newdata,
        draw_ids = draw_ids,
        conditional = TRUE
      )
      if ("draw" %in% kernel_formals) kernel_args$draw <- FALSE
      do.call(kernel, kernel_args)
    } else {
      posterior_detection(
        object, newdata = newdata, draw_ids = draw_ids
      )
    }
    if (!summary) return(pred)
    return(summarize_predictions(pred, probs = probs, robust = robust))
  }

  # mgcv answers `type = "terms"` with a per-term matrix. mvgam has
  # no single equivalent because a term's draws come from different
  # accessors, so the type is accepted and redirected rather than
  # rejected as an unknown choice.
  if (type == "terms") {
    stop(insight::format_error(c(
      "type = 'terms' has no single equivalent in mvgam.",
      i = paste0(
        "Use posterior_smooths.mvgam() for per-smooth draws, ",
        "fixef.mvgam() / ranef.mvgam() for parametric and random ",
        "components, and posterior_linpred.mvgam() for the combined ",
        "linear predictor."
      )
    )))
  }

  # Dispatch by type. posterior_predict / posterior_epred /
  # posterior_linpred each handle multivariate (resp = NULL returns
  # a named list of matrices) the same way, so summary downstream
  # treats them uniformly.
  draws <- switch(
    type,
    "response" = posterior_predict(
      object,
      newdata = newdata,
      process_error = process_error,
      incl_autocor = incl_autocor,
      ndraws = ndraws,
      draw_ids = draw_ids,
      re_formula = re_formula,
      allow_new_levels = allow_new_levels,
      sample_new_levels = sample_new_levels,
      resp = resp,
      ...
    ),
    "link" = posterior_linpred(
      object,
      newdata = newdata,
      transform = FALSE,
      process_error = process_error,
      incl_autocor = incl_autocor,
      ndraws = ndraws,
      draw_ids = draw_ids,
      re_formula = re_formula,
      allow_new_levels = allow_new_levels,
      sample_new_levels = sample_new_levels,
      resp = resp,
      ...
    ),
    "expected" = posterior_epred(
      object,
      newdata = newdata,
      process_error = process_error,
      incl_autocor = incl_autocor,
      ndraws = ndraws,
      draw_ids = draw_ids,
      re_formula = re_formula,
      allow_new_levels = allow_new_levels,
      sample_new_levels = sample_new_levels,
      resp = resp,
      ...
    ),
    "variance" = predict_variance(
      object,
      newdata = newdata,
      process_error = process_error,
      incl_autocor = incl_autocor,
      ndraws = ndraws,
      draw_ids = draw_ids,
      re_formula = re_formula,
      allow_new_levels = allow_new_levels,
      sample_new_levels = sample_new_levels,
      resp = resp
    )
  )

  # Return raw draws if summary not requested
  if (!summary) {
    return(draws)
  }

  # Compute summary statistics. Multivariate paths return a named
  # list of matrices (one per response); summarise each element
  # separately to match brms's output convention.
  if (is.list(draws) && !is.matrix(draws)) {
    return(lapply(draws, summarize_predictions,
                  probs = probs, robust = robust))
  }
  summarize_predictions(draws, probs = probs, robust = robust)
}


#' Posterior conditional variance Var\[Y | theta\] for predict.mvgam
#'
#' Internal helper that computes per-draw conditional variances using
#' family-specific mean-variance formulas. Called only from
#' [predict.mvgam()] when `type = "variance"`.
#'
#' Draw alignment: `posterior_epred()` and the dpar extraction must
#' index the same posterior rows; otherwise `mu` from draw `i` is
#' paired with `sigma` from a different draw and the conditional
#' variance is statistically incoherent. Because `posterior_epred()`
#' subsamples randomly when `ndraws < total_draws`, we compute `mu`
#' over the full posterior and then subsample `mu` and the dpar
#' matrices with the same `draw_idx`. The fix is independent of
#' draw_ids plumbing through `posterior_epred()`.
#'
#' @noRd
predict_variance <- function(object, newdata, process_error,
                             incl_autocor, ndraws, draw_ids,
                             re_formula, allow_new_levels,
                             sample_new_levels, resp) {
  # Closure-unit families have closed-form per-visit marginal
  # variances:
  #   nmix Y_{g,j} | lambda_g, p_{g,j} ~ Poisson(lambda_g *
  #     p_{g,j}) (thinned-Poisson property), so Var[Y] = E[Y].
  #   occ  Y_{g,j} | psi_g, p_{g,j} ~ Bernoulli(psi_g * p_{g,j}),
  #     so Var[Y] = E[Y] * (1 - E[Y]).
  # Both route through posterior_epred and apply the
  # family-specific variance formula; no dpar broadcasting needed.
  if (is_closure_unit_family(object$family)) {
    epred <- posterior_epred(
      object,
      newdata           = newdata,
      process_error     = process_error,
      incl_autocor      = incl_autocor,
      ndraws            = ndraws,
      draw_ids          = draw_ids,
      re_formula        = re_formula,
      allow_new_levels  = allow_new_levels,
      sample_new_levels = sample_new_levels,
      resp              = resp
    )
    family_name <- resolve_family_name(object$family)
    # mv-response families (mvn, mvt) carry per-row residual scale
    # in Psi (and df in nu for mvt). The marginal per-row variance
    # under the conditional gllvm parameterisation is the residual
    # variance plus the row's contribution from the factor model;
    # the latter is row-constant given the design, so the residual
    # term carries the per-row variability.
    if (is_multi_response_family(object$family)) {
      # Simplex families: per-cell variance follows from the
      # softmax probability and the family's dispersion. Dirichlet:
      # Var[X_k] = p_k (1 - p_k) / (phi + 1). Multinomial:
      # Var[Y_k] = N * p_k (1 - p_k). Categorical: Bernoulli per
      # cell, Var = p_k (1 - p_k).
      if (is_simplex_response_family(object$family)) {
        needs_phi <- identical(family_name, "diri")
        comp <- extract_simplex_response_components(
          object, newdata = newdata, draw_ids = draw_ids,
          ndraws = ndraws, needs_phi = needs_phi
        )
        prob <- comp$prob_row
        base_var <- prob * (1 - prob)
        if (identical(family_name, "diri")) {
          # comp$phi is already `[ndraws x N_obs]` with per-unit-shared
          # values (Stan's `phi[idx[1]]` collapse), so no broadcast.
          return(base_var / (comp$phi + 1))
        }
        if (identical(family_name, "multi")) {
          if (is.null(newdata)) {
            newdata <- object$obs_data %||% object$data
          }
          response_var <- closure_unit_response_var(object$formula)
          y_vec <- newdata[[response_var]]
          total_row <- numeric(comp$N_obs)
          for (g in seq_len(comp$N_unit)) {
            Kg <- comp$arrays$n_rep[g]
            idx <- comp$arrays$visit_idx[g, seq_len(Kg)]
            total_row[idx] <- sum(y_vec[idx])
          }
          total_mat <- matrix(total_row, nrow = comp$ndraws,
                               ncol = comp$N_obs, byrow = TRUE)
          return(base_var * total_mat)
        }
        # categ
        return(base_var)
      }
      needs_nu <- identical(family_name, "mvt")
      comp <- extract_mv_response_components(
        object, newdata = newdata, draw_ids = draw_ids,
        ndraws = ndraws, needs_nu = needs_nu
      )
      base_var <- comp$Psi_row^2
      if (needs_nu) {
        nu_mat <- matrix(comp$nu, nrow = comp$ndraws,
                          ncol = comp$N_obs)
        return(base_var * nu_mat / (nu_mat - 2))
      }
      return(base_var)
    }
    return(switch(
      family_name,
      nmix = epred,                  # Poisson thinned variance
      occ  = epred * (1 - epred),    # Bernoulli variance
      stop(insight::format_error(c(
        paste0(
          "predict(type = 'variance') closure-unit dispatch ",
          "missing for family '", family_name, "'."
        ),
        i = "Add a branch with the family's mean-variance formula."
      )))
    ))
  }
  # Compute mu over the full posterior so row i of mu_full corresponds
  # to draws_mat row i. Without this, posterior_epred(ndraws=K) would
  # randomly subsample, and dpars (which we extract sequentially)
  # would not align with mu.
  mu_full <- posterior_epred(
    object,
    newdata = newdata,
    process_error = process_error,
    incl_autocor = incl_autocor,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels,
    resp = resp
  )

  # A multivariate fit carries one dispersion parameter per response,
  # so a variance is only defined once a response is named.
  if (is.list(mu_full) && !is.matrix(mu_full)) {
    stop(insight::format_error(c(
      "type = 'variance' requires 'resp' for multivariate models.",
      i = paste0(
        "Available responses: ",
        paste(shQuote(object$response_names), collapse = ", "), "."
      )
    )), call. = FALSE)
  }

  draws_mat <- posterior::as_draws_matrix(object$fit)
  total_draws <- nrow(draws_mat)

  if (nrow(mu_full) != total_draws) {
    stop(insight::format_error(c(
      "Internal: posterior_epred row count does not match total draws.",
      x = paste0("Expected ", total_draws, " rows in mu; got ",
                 nrow(mu_full), ".")
    )))
  }

  # Pick the draw subsample once, use it for both mu and dpars so they
  # align.
  draw_idx <- resolve_draw_indices(total_draws, ndraws, draw_ids)

  # An ordinal family predicts a probability per category rather than a
  # mean, so its epred carries a third margin and none of the dispersion
  # parameters below apply. posterior_predict() draws the ordered levels
  # as the integers 1..K, so the variance of that draw is what the
  # category probabilities imply.
  if (is_ordinal_family(object$family)) {
    return(ordinal_category_variance(
      mu_full[draw_idx, , , drop = FALSE]
    ))
  }

  mu <- mu_full[draw_idx, , drop = FALSE]
  ndraws_mu <- nrow(mu)
  nobs_mu <- ncol(mu)

  fpars <- extract_family_pars_for_draws(object, draws_mat, draw_idx,
                                         resp = resp)

  # broadcast: dpar matrices arrive as [ndraws x n_series] for
  # multivariate (already errored above) or [ndraws x 1] / [ndraws x
  # nobs] for univariate. Expand to [ndraws x nobs] for the variance
  # helper's dim check.
  broadcast <- function(x, n_rows, n_cols) {
    if (is.null(x)) return(NULL)
    checkmate::assert_matrix(x)
    if (nrow(x) != n_rows) {
      stop(insight::format_error(c(
        "Internal: dpar row count does not match mu row count.",
        x = paste0("dpar has ", nrow(x), " rows; mu has ", n_rows, ".")
      )))
    }
    if (ncol(x) == n_cols) return(x)
    if (ncol(x) == 1L) {
      return(matrix(x[, 1L], nrow = n_rows, ncol = n_cols,
                    byrow = FALSE))
    }
    stop(insight::format_error(c(
      "Internal: unexpected dpar column count.",
      x = paste0("Got ", ncol(x), " columns; expected 1 or ",
                 n_cols, ".")
    )))
  }

  sigma_mat <- broadcast(fpars$sigma, ndraws_mu, nobs_mu)
  shape_mat <- broadcast(fpars$shape, ndraws_mu, nobs_mu)
  phi_mat   <- broadcast(fpars$phi,   ndraws_mu, nobs_mu)
  nu_mat    <- broadcast(fpars$nu,    ndraws_mu, nobs_mu)

  family <- object$family
  family_name <- family$family

  # Binomial / beta_binomial need trial counts (per-observation).
  trials_vec <- NULL
  if (family_name %in% c("binomial", "beta_binomial")) {
    trials_vec <- extract_trials_for_family(object, family, newdata)
  }

  compute_family_variance(
    mu = mu,
    family = family,
    sigma = sigma_mat,
    shape = shape_mat,
    phi = phi_mat,
    nu = nu_mat,
    trials = trials_vec
  )
}


#' Summarize Posterior Prediction Draws
#'
#' Computes summary statistics from posterior draws following the brms
#'   output format.
#'
#' @param draws Posterior draws, either a matrix
#'   ``\\[ndraws x nobs\\]`` or, for the ordinal and categorical
#'   families, an array ``\\[ndraws x nobs x ncat\\]``.
#' @param probs Numeric vector of probabilities for quantile computation.
#' @param robust Logical. If `FALSE`, uses mean and sd. If `TRUE`, uses
#'   median and mad.
#'
#' @return For matrix input, a matrix with columns for point estimate,
#'   uncertainty and quantiles. For array input, an array
#'   ``\\[nobs x nstat x ncat\\]`` holding the same columns for each
#'   category.
#'
#' @noRd
summarize_predictions <- function(draws, probs, robust) {
  checkmate::assert_numeric(probs, lower = 0, upper = 1, min.len = 1)
  checkmate::assert_logical(robust, len = 1)

  # The ordinal and categorical families predict a probability per
  # category, so their draws carry a third margin. Each category is
  # summarised on its own and the results are stacked back along that
  # margin, which is the shape brms returns for the same families.
  if (length(dim(draws)) == 3L) {
    checkmate::assert_array(draws, mode = "numeric", d = 3L)
    cats <- seq_len(dim(draws)[3L])
    per_cat <- lapply(cats, function(k) {
      slice <- matrix(
        draws[, , k], nrow = dim(draws)[1L], ncol = dim(draws)[2L],
        dimnames = dimnames(draws)[1:2]
      )
      summarize_predictions(slice, probs = probs, robust = robust)
    })
    out <- array(
      unlist(per_cat, use.names = FALSE),
      dim = c(dim(per_cat[[1L]]), length(cats)),
      dimnames = list(
        dimnames(draws)[[2L]],
        colnames(per_cat[[1L]]),
        dimnames(draws)[[3L]]
      )
    )
    return(out)
  }

  checkmate::assert_matrix(
    draws,
    mode = "numeric",
    min.rows = 1,
    min.cols = 1
  )

  # Compute point estimates and uncertainty
  if (robust) {
    estimate <- apply(draws, 2, stats::median)
    est_error <- apply(draws, 2, stats::mad)
  } else {
    estimate <- colMeans(draws)
    est_error <- apply(draws, 2, stats::sd)
  }

  # Compute quantiles
  quantiles <- apply(draws, 2, stats::quantile, probs = probs)

  # Reshape vector to matrix when single quantile requested
  if (length(probs) == 1) {
    quantiles <- matrix(quantiles, nrow = 1)
  }

  # Create quantile column names following brms convention (Q2.5, Q97.5)
  quantile_names <- paste0("Q", probs * 100)

  # Combine into output matrix
  out <- cbind(
    Estimate = estimate,
    Est.Error = est_error,
    t(quantiles)
  )
  colnames(out)[3:ncol(out)] <- quantile_names

  out
}
