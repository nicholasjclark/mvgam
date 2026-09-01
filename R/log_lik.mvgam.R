#' Pointwise log-likelihoods from a fitted mvgam model
#'
#' Compute the per-observation log-likelihood for each posterior draw,
#' matching `brms::log_lik` semantics. The result is an `S x N` matrix where
#' `S` is the number of posterior draws and `N` is the number of observations
#' in `newdata` (or the training data when `newdata = NULL`). Used by
#' [loo::loo()], [loo::waic()], [loo::psis()] and bayesplot PSIS-PIT helpers.
#'
#' @param object An mvgam fit.
#' @param newdata Optional data frame containing the response and all
#'   covariates referenced by the model. `NULL` (default) uses the original
#'   training data.
#' @param re_formula Random-effects formula passed to the prediction
#'   primitives. `NULL` includes all random effects.
#' @param resp Character; response name for multivariate models. Ignored for
#'   univariate fits.
#' @param ndraws Optional integer; subsample to `ndraws` posterior draws.
#' @param draw_ids Optional integer vector of draw indices to use (mutually
#'   exclusive with `ndraws`).
#' @param incl_autocor Logical. When `TRUE` (default) the linear
#'   predictor carries the latent trend state the model inferred at
#'   each time, so the density describes the observation that was
#'   actually seen there. This is the conditional surface, the one
#'   [hindcast.mvgam()] and [residuals.mvgam()] read. When `FALSE`
#'   the trend contributes
#'   only its deterministic submodel, giving a density that leaves the
#'   autocorrelation out. The marginal surface, which integrates over
#'   the trend dynamics, belongs to [posterior_epred.mvgam()] and is
#'   not a basis for an ELPD.
#' @param process_error Superseded by `incl_autocor` and still
#'   accepted, so calls written against it keep working. `TRUE` maps
#'   to `incl_autocor = TRUE` and `FALSE` to `incl_autocor = FALSE`.
#'   When only `process_error` is given it decides; when both are
#'   given `incl_autocor` decides and `process_error` is ignored.
#' @param ... Forwarded to [posterior_linpred.mvgam()], which in
#'   turn forwards brms-style prediction args to the underlying
#'   prediction machinery. Common pass-throughs include
#'   `allow_new_levels = TRUE` and `sample_new_levels = "gaussian"`
#'   for predicting on factor levels that were not in the
#'   training data (e.g. inside `kfold.mvgam()` refits scoring on
#'   held-out groups).
#'
#' @return Numeric matrix \[ndraws x nobs\] of pointwise log densities.
#'   For multi-response (mvbf) fits with `resp = NULL`, the returned
#'   matrix is the joint per-observation log density: each column
#'   sums the pointwise densities across every response arm for that
#'   observation row (this matches `brms::log_lik.brmsfit()`
#'   semantics). Pass `resp = "<name>"` to scope to a single arm.
#'
#' @details
#' The implementation dispatches by `object$family$family` to per-family log
#' density helpers. brms's parameterisations are reproduced verbatim
#' (`negbinomial` uses the `neg_binomial_2` mean-shape parameterisation;
#' `beta` uses `(mu, phi)`; `hurdle_*` and `zero_inflated_*` mix a point mass
#' at zero with the corresponding base family). Trials for `binomial` /
#' `beta_binomial` are extracted from `newdata` or from the model object via
#' the existing `extract_trials_for_family()` helper.
#'
#' A density is evaluated under the latent trend state the model
#' inferred at that time, which is the state [hindcast.mvgam()] returns
#' and [residuals.mvgam()] scores against. That is what lets [loo()],
#' [waic()] and the importance weights built from them describe the
#' series that was observed. The `posterior_*` methods answer a
#' different question, integrating over the trend dynamics so that a
#' covariate effect reads the same whatever time it is asked at, and
#' they should not be used to build an ELPD.
#'
#' @seealso [loo.mvgam()], [waic.mvgam()], [hindcast.mvgam()],
#'   [posterior_linpred.mvgam()].
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
#' # Pointwise posterior log-densities; rows are draws, columns
#' # are observations. Feeds straight into loo::loo().
#' ll <- log_lik(mod)
#' dim(ll)
#' }
#'
#' @method log_lik mvgam
#' @export
log_lik.mvgam <- function(object,
                          newdata = NULL,
                          re_formula = NULL,
                          resp = NULL,
                          ndraws = NULL,
                          draw_ids = NULL,
                          incl_autocor = TRUE,
                          process_error = NULL,
                          ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  checkmate::assert_int(ndraws, lower = 1, null.ok = TRUE)
  checkmate::assert_integerish(draw_ids, lower = 1, null.ok = TRUE)
  checkmate::assert_string(resp, null.ok = TRUE)
  incl_autocor <- resolve_incl_autocor(
    incl_autocor = incl_autocor,
    legacy = process_error,
    autocor_supplied = !missing(incl_autocor)
  )

  newdata <- newdata %||% object$data

  # The linear predictor and the distributional parameters are drawn
  # by separate extractions, so `ndraws` is materialised as concrete
  # `draw_ids` first. Left as a count, each extraction would subsample
  # independently and pair a dispersion, or a detection probability,
  # with a mean from an unrelated iteration.
  draw_ids <- resolve_draw_ids(object, ndraws, draw_ids)
  if (!is.null(draw_ids)) {
    ndraws <- NULL
  }

  # Link-scale linear predictor carrying the state the model inferred,
  # rather than a fresh draw from the trend's marginal dynamics: a
  # density for an observation at a given time has to be evaluated
  # under the state the model put there, which is what makes the
  # importance weights `loo()` builds from it describe that
  # observation. `allow_new_levels` / `sample_new_levels` come through
  # `...` from callers such as `kfold.mvgam()`, which scores on
  # held-out factor levels.
  dots <- list(...)
  linpred <- get_combined_linpred(
    mvgam_fit = object,
    newdata = newdata,
    process_error = FALSE,
    trend_state = autocor_to_trend_state(incl_autocor),
    draw_ids = draw_ids,
    re_formula = re_formula,
    allow_new_levels = dots$allow_new_levels %||% FALSE,
    sample_new_levels = dots$sample_new_levels %||% "uncertainty",
    resp = resp
  )

  # Multivariate fits return a named list of [ndraws x nobs] matrices,
  # one per response. brms's log_lik returns the JOINT per-observation
  # log density when resp is NULL: a [ndraws x nobs] matrix where each
  # column sums the per-response log densities for that observation
  # row (verified against brms::log_lik on mvbind Gaussian fixtures).
  # Subset to a single response when resp is named.
  if (is.list(linpred) && !is.matrix(linpred)) {
    if (!is.null(resp)) {
      linpred <- linpred[[resp]]
    } else {
      # Closure-unit families return at the unit grain
      # (`[S x N_unit]`), which has fewer columns than the
      # per-visit grain produced by every other family. Stitching
      # the per-response matrices via `Reduce("+", ...)` would
      # silently truncate or recycle. Multi-response closure-unit
      # support lands when jsdgam wires together multiple
      # detection-error responses; flag the gap clearly until
      # then.
      resp_names <- names(linpred)
      mv_families <- lapply(resp_names, function(r) {
        get_family_for_resp(object, r)
      })
      if (any(vapply(mv_families, is_closure_unit_family, logical(1)))) {
        stop(insight::format_error(c(
          "Multivariate models with a closure-unit family are not yet supported by log_lik().",
          x = paste0(
            "Responses with closure-unit families: ",
            paste(resp_names[vapply(mv_families,
                                    is_closure_unit_family,
                                    logical(1))], collapse = ", "),
            "."
          ),
          i = "Call log_lik() per response with `resp = '<name>'`."
        )))
      }
      per_resp <- lapply(resp_names, function(r) {
        log_lik_single_response(
          object = object,
          newdata = newdata,
          linpred = linpred[[r]],
          resp = r,
          draw_ids = draw_ids
        )
      })
      if (length(per_resp) == 1L) {
        return(per_resp[[1L]])
      }
      return(Reduce(`+`, per_resp))
    }
  }

  log_lik_single_response(
    object = object,
    newdata = newdata,
    linpred = linpred,
    resp = resp,
    draw_ids = draw_ids
  )
}


# Univariate log-lik path. Pulled out so the multivariate branch can
# loop over responses and stitch the [ndraws x sum(nobs)] matrix back
# together.
log_lik_single_response <- function(object, newdata, linpred, resp,
                                    draw_ids) {
  family_obj <- if (!is.null(resp)) {
    get_family_for_resp(object, resp)
  } else {
    object$family
  }
  family_name <- resolve_family_name(family_obj)
  family_link <- family_obj$link

  # Observed response on the data scale
  y <- extract_response_for_log_lik(object, newdata, resp)

  # Closure-unit families (nmix and successors) return at the
  # closure-unit grain rather than the visit grain. The Stan
  # lpdf marginalises latent N analytically across all visits
  # within a unit, so the unit is the conditionally iid block
  # that loo/waic must score (Vehtari, Gelman, Gabry 2017).
  # Closure-unit data prep is re-run from the (possibly new)
  # data here so cap edits at predict time take effect.
  if (is_closure_unit_family(family_obj)) {
    # mv-response families (mvn / mvt) take a different path:
    # the per-row residual is independent (normal or Student-t)
    # conditional on the latent factor contribution in mu, so the
    # closure-unit log_lik scores at the (site, species) row grain
    # rather than the marginal-over-N grain. Psi (and nu for mvt)
    # are pulled from the posterior and broadcast to per-row level
    # via `extract_mv_response_components()`.
    if (is_multi_response_family(family_obj)) {
      # mv-response continuous (mvn, mvt) and simplex (diri,
      # multi, categ) families share the multi-response gate but
      # need different dpar extraction. mvn/mvt pull Psi/nu; the
      # simplex families pull the per-row softmax probability +
      # closure-unit arrays (and phi for diri).
      if (is_simplex_response_family(family_obj)) {
        needs_phi <- identical(family_name, "diri")
        comp <- extract_simplex_response_components(
          object, newdata, draw_ids, needs_phi = needs_phi,
          linpred = linpred
        )
        family_pars_simplex <- list(
          prob_row = comp$prob_row,
          phi      = comp$phi,
          arrays   = comp$arrays
        )
        log_lik_fn <- switch(
          family_name,
          diri  = log_lik_diri,
          multi = log_lik_multi,
          categ = log_lik_categ,
          stop(insight::format_error(c(
            paste0(
              "Simplex log_lik dispatch missing for family '",
              family_name, "'."
            ),
            i = paste0(
              "Add a '", family_name, " = log_lik_",
              family_name, "' branch."
            )
          )))
        )
        return(log_lik_fn(
          linpred     = linpred,
          link        = family_link,
          y           = y,
          family_pars = family_pars_simplex,
          trials      = NULL
        ))
      }
      needs_nu <- identical(family_name, "mvt")
      comp <- extract_mv_response_components(
        object, newdata, draw_ids, needs_nu = needs_nu,
        linpred = linpred
      )
      family_pars_mv <- list(
        Psi_row = comp$Psi_row,
        nu      = comp$nu
      )
      log_lik_fn <- switch(
        family_name,
        mvn = log_lik_mvn,
        mvt = log_lik_mvt,
        stop(insight::format_error(c(
          paste0(
            "Mv-response log_lik dispatch missing for family '",
            family_name, "'."
          ),
          i = paste0(
            "Add a '", family_name, " = log_lik_",
            family_name, "' branch."
          )
        )))
      )
      return(log_lik_fn(
        linpred     = linpred,
        link        = family_link,
        y           = y,
        family_pars = family_pars_mv,
        trials      = NULL
      ))
    }
    # `closure_unit_grouping()` returns the multi-season opt-in
    # `c("series", "site", "time")` when set; NULL keeps the
    # default 2-axis path inside `build_closure_unit_arrays()`.
    # `default_cap` makes the `cap` data column optional for
    # binary-response families (`occ()` defaults to 1).
    arrays <- build_closure_unit_arrays(
      newdata, response_var = closure_unit_response_var(object$formula),
      default_cap = closure_unit_default_cap(object$family),
      unit_grouping_vars = closure_unit_grouping(object$family)
    )
    # extract_p_for_closure_unit() handles both scalar (no
    # detection sub-formula) and vector (with `bf(p ~ ...)`) cases
    # by routing the vector case through brms's dpar linpred via
    # the mock-stanfit path. Single call covers both shapes.
    p_mat <- extract_p_for_closure_unit(
      object   = object,
      newdata  = newdata,
      draw_ids = draw_ids,
      n_visit  = ncol(linpred),
      ndraws   = nrow(linpred)
    )
    family_pars_cu <- list(closure_arrays = arrays, p = p_mat)
    log_lik_fn <- switch(
      family_name,
      nmix                 = log_lik_nmix,
      nmix_royle_nichols   = log_lik_nmix_royle_nichols,
      nmix_poisson_poisson = log_lik_nmix_poisson_poisson,
      occ                  = log_lik_occ,
      stop(insight::format_error(c(
        paste0(
          "Closure-unit log_lik dispatch missing for family '",
          family_name, "'."
        ),
        i = paste0(
          "Add a '", family_name, " = log_lik_",
          family_name, "' branch to the switch() in ",
          "log_lik_single_response()."
        )
      )))
    )
    return(log_lik_fn(
      linpred     = linpred,
      link        = family_link,
      y           = y,
      family_pars = family_pars_cu,
      trials      = NULL
    ))
  }

  # Distributional parameters (sigma, shape, hu, zi, ...) as
  # [ndraws x nobs], whether each was sampled as a scalar or predicted
  # by a formula of its own.
  family_pars <- resolve_family_pars(
    object,
    dpar_names = get_family_dpars(family_name),
    ndraws = nrow(linpred),
    nobs = ncol(linpred),
    draw_ids = draw_ids,
    newdata = newdata,
    resp = resp
  )

  # Ordinal families need threshold and disc draws from the posterior
  # in addition to the standard dpars.
  if (family_name %in% ORDINAL_FAMILIES) {
    family_pars$thres <- extract_ordinal_thresholds(
      object,
      ndraws = nrow(linpred),
      draw_ids = draw_ids
    )
    family_pars$disc <- extract_ordinal_disc(
      object,
      ndraws = nrow(linpred),
      nobs = ncol(linpred),
      draw_ids = draw_ids
    )
  }

  # Trials for binomial-family responses
  trials <- extract_trials_for_family(object, family_obj, newdata)

  ll <- dispatch_log_lik(
    family_name = family_name,
    link = family_link,
    linpred = linpred,
    y = y,
    family_pars = family_pars,
    trials = trials
  )

  # `weights()`, `cens()` and `trunc()` each change what a row
  # contributes, and every information criterion reads this matrix, so
  # they are applied here rather than in each caller.
  apply_addition_terms(
    ll = ll,
    object = object,
    resp = resp,
    family_name = family_name,
    link = family_link,
    linpred = linpred,
    y = y,
    family_pars = family_pars,
    trials = trials
  )
}

# Generic re-export so users can call log_lik() without attaching brms first
#' @importFrom rstantools log_lik
#' @export
rstantools::log_lik


# Pull the response column out of newdata (or the training data) for the
# requested response. Binomial single-trial responses can arrive as a 2-col
# matrix from cbind() syntax; reduce to integer success counts.
extract_response_for_log_lik <- function(object, newdata, resp) {
  if (!is.null(resp)) {
    y_name <- resp
  } else {
    y_name <- as.character(object$formula$resp)
    if (length(y_name) == 0 || y_name == "") {
      lhs <- if (inherits(object$formula, "brmsformula")) {
        object$formula$formula[[2L]]
      } else {
        object$formula[[2L]]
      }
      y_name <- all.vars(lhs)[1L]
    }
  }
  if (!y_name %in% names(newdata)) {
    stop(insight::format_error(
      cli::format_inline(
        "Response variable {.val {y_name}} not found in {.field newdata}."
      )
    ))
  }
  y <- newdata[[y_name]]
  if (is.matrix(y) && ncol(y) == 2L) {
    # cbind(success, failure) → take success column
    y <- y[, 1L]
  }
  as.numeric(y)
}


# Family dispatcher. Returns [ndraws x nobs] matrix of log densities.
#
# Extension hook: hierarchical detection-error families (nmix and
# successors) need extra inputs beyond linpred + the standard dpars
# (latent abundance draws, detection probabilities, capacity per
# observation). Add them by extending `family_pars` upstream in
# `log_lik.mvgam` (e.g. pulling `trend` / `detprob` / `cap` via
# `as_draws_matrix(object, variable = ...)` when family_name == "nmix")
# and reading them from `family_pars` inside the per-family helper. No
# dispatcher changes needed.
dispatch_log_lik <- function(family_name, link, linpred, y,
                             family_pars, trials) {
  fn_name <- paste0("log_lik_", family_name)
  fn <- tryCatch(
    get(fn_name, mode = "function", envir = asNamespace("mvgam")),
    error = function(e) NULL
  )
  if (is.null(fn)) {
    stop(insight::format_error(
      cli::format_inline(
        "Family {.val {family_name}} is not yet supported by log_lik.mvgam."
      )
    ))
  }
  fn(
    linpred = linpred,
    link = link,
    y = y,
    family_pars = family_pars,
    trials = trials
  )
}


# -------------------------------------------------------------------------
# Per-family log densities
# -------------------------------------------------------------------------
# Each helper receives `linpred` [ndraws x nobs] on the link scale, the
# observed `y`, the distributional parameters as a named list of
# [ndraws x nobs] matrices, the `trials` vector for binomial-family, and the
# link string. Returns [ndraws x nobs] of log densities matching brms's
# *_lpdf / *_lpmf parameterisations.

# Apply the inverse link to a linpred matrix.
.linkinv <- function(linpred, link) {
  switch(link,
    identity = linpred,
    log = exp(linpred),
    logit = stats::plogis(linpred),
    probit = stats::pnorm(linpred),
    cloglog = 1 - exp(-exp(linpred)),
    inverse = 1 / linpred,
    sqrt = linpred^2,
    log1p = expm1(linpred),
    stop(insight::format_error(
      cli::format_inline("Unknown link function: {.val {link}}.")
    ))
  )
}

# Wrap a per-observation function call to keep return shape [ndraws x nobs]
# regardless of the underlying density signature.
.apply_log_density <- function(linpred, y, fn) {
  out <- matrix(NA_real_, nrow = nrow(linpred), ncol = ncol(linpred))
  for (j in seq_along(y)) {
    # A missing response has no density. Families that branch on the
    # response value (the hurdle, zero-inflated and ordinal kernels)
    # would otherwise evaluate `if (NA)` and abort the whole call, so
    # the column is left as NA and dropped downstream by `clean_ll()`.
    if (is.na(y[j])) {
      next
    }
    out[, j] <- fn(y[j], j)
  }
  out
}

log_lik_gaussian <- function(linpred, link, y, family_pars, trials) {
  dist_log_density(
    family_dist_spec("gaussian", link, linpred, family_pars, trials),
    linpred, y
  )
}

log_lik_student <- function(linpred, link, y, family_pars, trials) {
  dist_log_density(
    family_dist_spec("student", link, linpred, family_pars, trials),
    linpred, y
  )
}

log_lik_lognormal <- function(linpred, link, y, family_pars, trials) {
  dist_log_density(
    family_dist_spec("lognormal", link, linpred, family_pars, trials),
    linpred, y
  )
}

log_lik_gamma <- function(linpred, link, y, family_pars, trials) {
  dist_log_density(
    family_dist_spec("gamma", link, linpred, family_pars, trials),
    linpred, y
  )
}

log_lik_weibull <- function(linpred, link, y, family_pars, trials) {
  dist_log_density(
    family_dist_spec("weibull", link, linpred, family_pars, trials),
    linpred, y
  )
}

log_lik_exponential <- function(linpred, link, y, family_pars, trials) {
  dist_log_density(
    family_dist_spec("exponential", link, linpred, family_pars, trials),
    linpred, y
  )
}

log_lik_beta <- function(linpred, link, y, family_pars, trials) {
  dist_log_density(
    family_dist_spec("beta", link, linpred, family_pars, trials),
    linpred, y
  )
}

log_lik_bernoulli <- function(linpred, link, y, family_pars, trials) {
  dist_log_density(
    family_dist_spec("bernoulli", link, linpred, family_pars, trials),
    linpred, y
  )
}

log_lik_binomial <- function(linpred, link, y, family_pars, trials) {
  dist_log_density(
    family_dist_spec("binomial", link, linpred, family_pars, trials),
    linpred, y
  )
}

log_lik_beta_binomial <- function(linpred, link, y, family_pars, trials) {
  dist_log_density(
    family_dist_spec("beta_binomial", link, linpred, family_pars, trials),
    linpred, y
  )
}

log_lik_poisson <- function(linpred, link, y, family_pars, trials) {
  dist_log_density(
    family_dist_spec("poisson", link, linpred, family_pars, trials),
    linpred, y
  )
}

log_lik_negbinomial <- function(linpred, link, y, family_pars, trials) {
  dist_log_density(
    family_dist_spec("negbinomial", link, linpred, family_pars, trials),
    linpred, y
  )
}

log_lik_geometric <- function(linpred, link, y, family_pars, trials) {
  dist_log_density(
    family_dist_spec("geometric", link, linpred, family_pars, trials),
    linpred, y
  )
}

# Hurdle Poisson: P(Y=0) = hu; P(Y=k>0) = (1 - hu) * dpois(k|mu) /
# (1 - exp(-mu))
log_lik_hurdle_poisson <- function(linpred, link, y, family_pars, trials) {
  mu <- .linkinv(linpred, link)
  hu <- family_pars$hu
  .apply_log_density(linpred, y, function(yj, j) {
    if (yj == 0) {
      log(hu[, j])
    } else {
      log1p(-hu[, j]) +
        stats::dpois(yj, lambda = mu[, j], log = TRUE) -
        log1p(-exp(-mu[, j]))
    }
  })
}

# Hurdle NegBin: P(Y=0) = hu; P(Y=k>0) = (1 - hu) * dnbinom(k|mu, shape) /
# (1 - P(Y_NB = 0))
log_lik_hurdle_negbinomial <- function(linpred, link, y, family_pars, trials) {
  mu <- .linkinv(linpred, link)
  hu <- family_pars$hu
  shape <- family_pars$shape
  .apply_log_density(linpred, y, function(yj, j) {
    if (yj == 0) {
      log(hu[, j])
    } else {
      p0 <- stats::dnbinom(0, mu = mu[, j], size = shape[, j])
      log1p(-hu[, j]) +
        stats::dnbinom(yj, mu = mu[, j], size = shape[, j], log = TRUE) -
        log1p(-p0)
    }
  })
}

# Hurdle Gamma: P(Y=0) = hu; P(Y>0) density = (1 - hu) * dgamma(y|shape, rate)
log_lik_hurdle_gamma <- function(linpred, link, y, family_pars, trials) {
  mu <- .linkinv(linpred, link)
  hu <- family_pars$hu
  shape <- family_pars$shape
  .apply_log_density(linpred, y, function(yj, j) {
    if (yj == 0) {
      log(hu[, j])
    } else {
      log1p(-hu[, j]) +
        stats::dgamma(yj, shape = shape[, j],
                      rate = shape[, j] / mu[, j], log = TRUE)
    }
  })
}

log_lik_hurdle_lognormal <- function(linpred, link, y, family_pars, trials) {
  mu <- linpred
  hu <- family_pars$hu
  sigma <- family_pars$sigma
  .apply_log_density(linpred, y, function(yj, j) {
    if (yj == 0) {
      log(hu[, j])
    } else {
      log1p(-hu[, j]) +
        stats::dlnorm(yj, meanlog = mu[, j], sdlog = sigma[, j],
                      log = TRUE)
    }
  })
}

# Zero-inflated Poisson: P(Y=0) = zi + (1 - zi) * exp(-mu);
# P(Y=k>0) = (1 - zi) * dpois(k|mu)
log_lik_zero_inflated_poisson <- function(linpred, link, y,
                                          family_pars, trials) {
  mu <- .linkinv(linpred, link)
  zi <- family_pars$zi
  .apply_log_density(linpred, y, function(yj, j) {
    if (yj == 0) {
      # log(zi + (1-zi) * exp(-mu)) computed with log-sum-exp
      log_zi <- log(zi[, j])
      log_pois0 <- log1p(-zi[, j]) + stats::dpois(0, lambda = mu[, j],
                                                  log = TRUE)
      pmax <- pmax(log_zi, log_pois0)
      pmax + log(exp(log_zi - pmax) + exp(log_pois0 - pmax))
    } else {
      log1p(-zi[, j]) + stats::dpois(yj, lambda = mu[, j], log = TRUE)
    }
  })
}

log_lik_zero_inflated_negbinomial <- function(linpred, link, y,
                                              family_pars, trials) {
  mu <- .linkinv(linpred, link)
  zi <- family_pars$zi
  shape <- family_pars$shape
  .apply_log_density(linpred, y, function(yj, j) {
    if (yj == 0) {
      log_zi <- log(zi[, j])
      log_nb0 <- log1p(-zi[, j]) +
        stats::dnbinom(0, mu = mu[, j], size = shape[, j], log = TRUE)
      pmax <- pmax(log_zi, log_nb0)
      pmax + log(exp(log_zi - pmax) + exp(log_nb0 - pmax))
    } else {
      log1p(-zi[, j]) +
        stats::dnbinom(yj, mu = mu[, j], size = shape[, j], log = TRUE)
    }
  })
}

log_lik_zero_inflated_binomial <- function(linpred, link, y,
                                           family_pars, trials) {
  prob <- .linkinv(linpred, link)
  zi <- family_pars$zi
  trials <- as.integer(trials)
  .apply_log_density(linpred, y, function(yj, j) {
    if (yj == 0) {
      log_zi <- log(zi[, j])
      log_bin0 <- log1p(-zi[, j]) +
        stats::dbinom(0, size = trials[j], prob = prob[, j], log = TRUE)
      pmax <- pmax(log_zi, log_bin0)
      pmax + log(exp(log_zi - pmax) + exp(log_bin0 - pmax))
    } else {
      log1p(-zi[, j]) +
        stats::dbinom(yj, size = trials[j], prob = prob[, j], log = TRUE)
    }
  })
}

log_lik_zero_inflated_beta <- function(linpred, link, y,
                                       family_pars, trials) {
  mu <- .linkinv(linpred, link)
  zi <- family_pars$zi
  phi <- family_pars$phi
  .apply_log_density(linpred, y, function(yj, j) {
    if (yj == 0) {
      log(zi[, j])
    } else {
      a <- mu[, j] * phi[, j]
      b <- (1 - mu[, j]) * phi[, j]
      log1p(-zi[, j]) + stats::dbeta(yj, shape1 = a, shape2 = b, log = TRUE)
    }
  })
}

# Ordinal cumulative model on a 1D linpred, using thresholds + disc from
# posterior draws. brms parameterisation: P(Y <= k) = pnorm(thres[k] - eta * disc)
# for probit link, or plogis for logit. Returns [ndraws x nobs] log densities.
log_lik_cumulative <- function(linpred, link, y, family_pars, trials) {
  if (is.null(family_pars$thres) || is.null(family_pars$disc)) {
    stop(insight::format_error(
      "Ordinal log_lik requires {.field thres} and {.field disc} draws."
    ))
  }
  thres <- family_pars$thres  # [ndraws x ncat-1]
  disc <- family_pars$disc    # [ndraws x nobs] (scalar broadcast)
  link_cdf <- switch(link,
    logit = stats::plogis,
    probit = stats::pnorm,
    cloglog = function(x) 1 - exp(-exp(x)),
    stop(insight::format_error(
      cli::format_inline("Unsupported ordinal link: {.val {link}}.")
    ))
  )
  ncat <- ncol(thres) + 1L
  # Routed through the shared applier so the missing-response guard
  # lives in one place rather than in each kernel that branches on
  # the response value.
  .apply_log_density(linpred, y, function(yj, j) {
    yj <- as.integer(yj)
    eta_j <- linpred[, j] * disc[, j]
    if (yj == 1L) {
      log(link_cdf(thres[, 1L] - eta_j))
    } else if (yj == ncat) {
      log1p(-link_cdf(thres[, ncat - 1L] - eta_j))
    } else {
      log(link_cdf(thres[, yj] - eta_j) -
            link_cdf(thres[, yj - 1L] - eta_j))
    }
  })
}


#' Log-likelihood of a fitted mvgam model
#'
#' Method for the `stats::logLik` generic so that `AIC()` and `BIC()`
#' dispatch on `mvgam` objects. Returns the posterior mean of the
#' summed pointwise log-likelihood (across observations) as a numeric
#' scalar with the `df` and `nobs` attributes the [stats::AIC()] /
#' [stats::BIC()] machinery expects.
#'
#' @param object A fitted \[mvgam\]\[mvgam\] object.
#' @param pointwise Logical. When `FALSE` (default) a single scalar
#'   `logLik` value is returned for compatibility with `AIC` and `BIC`.
#'   When `TRUE` the underlying \[ndraws x nobs\] pointwise matrix from
#'   [log_lik.mvgam()] is returned (matches the historical mvgam
#'   return shape).
#' @param ... Additional arguments forwarded to [log_lik.mvgam()]
#'   (e.g. `newdata`, `ndraws`, `process_error`).
#'
#' @return When `pointwise = FALSE`, a length-1 `logLik` object with
#'   `df` (number of sampled parameters, excluding sampler
#'   diagnostics) and `nobs` attributes. When `pointwise = TRUE`, a
#'   \[ndraws x nobs\] numeric matrix of pointwise log densities.
#'
#' @details
#' The scalar returned is the posterior mean of `rowSums(log_lik(object))`
#' across draws. For Bayesian state-space models AIC and BIC are coarse
#' instruments because every latent state inflates the parameter count;
#' [loo()] / [waic()] are usually the better model-selection tools and
#' are recommended in preference.
#'
#' @seealso [log_lik.mvgam()], [loo.mvgam()], [waic.mvgam()].
#'
#' @method logLik mvgam
#' @export
logLik.mvgam <- function(object, pointwise = FALSE, ...) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_flag(pointwise)

  ll_mat <- log_lik(object, ...)

  if (pointwise) {
    return(ll_mat)
  }

  per_draw_sum <- rowSums(ll_mat)
  ll_scalar <- mean(per_draw_sum)

  diag_pars <- c(
    "lp__", "lprior", "accept_stat__", "stepsize__", "treedepth__",
    "n_leapfrog__", "divergent__", "energy__"
  )
  df <- length(setdiff(variables(object), diag_pars))

  structure(
    ll_scalar,
    df = df,
    nobs = nobs(object),
    class = "logLik"
  )
}


#' Resolve the autocorrelation switch from its two spellings
#'
#' `incl_autocor` is the name brms uses and the one mvgam documents.
#' `process_error` and `incl_dynamics` are the names mvgam used before
#' and are still accepted, so calls written against them keep working.
#' Supplying both spellings is not an error: the brms one wins, which
#' is what the documentation promises, and the superseded value is
#' ignored rather than silently combined with it.
#'
#' @param incl_autocor Value of the documented argument
#' @param legacy Value of the superseded argument, or `NULL` when the
#'   caller did not name it
#' @param autocor_supplied Whether the caller named `incl_autocor`
#' @return A single logical
#'
#' @noRd
resolve_incl_autocor <- function(incl_autocor, legacy, autocor_supplied) {
  checkmate::assert_logical(incl_autocor, len = 1L, any.missing = FALSE)
  checkmate::assert_logical(legacy, len = 1L, any.missing = FALSE,
                            null.ok = TRUE)
  checkmate::assert_logical(autocor_supplied, len = 1L)
  if (is.null(legacy) || isTRUE(autocor_supplied)) {
    return(incl_autocor)
  }
  legacy
}
