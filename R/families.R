# Custom observation families for mvgam.
#
# This file registers families that are not built in to brms. Each
# constructor returns a `brms::custom_family()` object with the
# mvgam-side stanvars (function block + any data inputs) attached
# in `attr(family, "mvgam_stanvars")`. `attach_family_stanvars()`
# in `R/make_stan.R` merges them into the brms call automatically,
# so users can just write `mvgam(... family = tweedie())`.
#
# The Tweedie Stan function block is adapted from the
# brms::custom_family definition packaged by the Australian
# Institute of Marine Science at
# https://github.com/open-AIMS/tweedie (MIT-licensed). The
# underlying compound Poisson-gamma decomposition is due to
# Spinkney on the Stan Forums:
# https://discourse.mc-stan.org/t/14636. Both sources are
# cited in the user-facing roxygen for tweedie().
#
# R-side log_lik and posterior_predict both use mgcv (already a
# hard mvgam dependency) for the Tweedie density and random
# number routines. `mgcv::ldTweedie()` returns log densities
# directly, avoiding the underflow-then-floor pattern that a
# `log(dtweedie(...))` two-step would introduce into LOO/WAIC.

#' Tweedie compound Poisson-gamma family for mvgam
#'
#' Tweedie distribution with power parameter `theta` in the open
#' interval `(1, 2)`, a.k.a. the compound Poisson-gamma. Models
#' continuous-positive data with an exact mass at zero (e.g.
#' CPUE, rainfall, claim totals), where the count of "events"
#' is Poisson and each event contributes a Gamma-distributed
#' amount. `theta = 1` is the scaled Poisson boundary; `theta =
#' 2` is the Gamma boundary. Both endpoints are numerically
#' unstable under the compound decomposition (division by `2 -
#' theta` blows up; the Gamma shape `m * (2 - theta) / (theta -
#' 1)` over- or underflows), so mvgam restricts `theta` to
#' `[1.001, 1.999]`. Values inside that interval give the
#' continuous case with a point mass at zero.
#'
#' Parameterised with three distributional parameters:
#' \describe{
#'   \item{`mu`}{positive mean (log link, fixed; see below)}
#'   \item{`mphi`}{positive dispersion (identity link, lower
#'     bound `1e-8`)}
#'   \item{`mtheta`}{power parameter (identity link, bounded
#'     `[1.001, 1.999]`)}
#' }
#'
#' The log link on `mu` is fixed: the compound Poisson-gamma
#' construction strictly requires `mu > 0`, and identity /
#' inverse links cannot enforce that when the linear predictor
#' is driven by a latent stochastic trend. If a future use case
#' demands a non-log link with a constrained linear predictor,
#' open an issue.
#'
#' By default `mu` is the only dpar tied to the linear predictor
#' (and therefore to the latent state-space trend in mvgam); the
#' dispersion `mphi` and power `mtheta` are estimated as global
#' scalars shared across all observations. brms distributional
#' regression is fully supported: write a `brms::bf()` formula
#' with `mphi ~ covs` and / or `mtheta ~ covs` to give either
#' parameter its own per-observation linear predictor. For
#' example,
#'
#' ```r
#' mvgam(bf(y ~ x, mphi ~ site), family = tweedie(), ...)
#' ```
#'
#' fits a Tweedie with site-varying dispersion. brms generates
#' the `b_mphi_Intercept` / `b_mphi_*` parameters and the
#' associated design matrix automatically; the Tweedie Stan
#' lpdf has overloaded scalar and vector signatures so both
#' the static-dpar and distributional cases work without any
#' extra wiring. If the data display obvious heteroscedasticity
#' (e.g. seasonal claim severity), prefer letting `mphi` vary
#' via a sub-formula over the global default (Bürkner 2018, brms
#' distributional regression vignette).
#'
#' The Stan likelihood is the compound Poisson-gamma
#' decomposition of Spinkney (Stan Forums,
#' \url{https://discourse.mc-stan.org/t/14636}). The mvgam
#' implementation is adapted from the
#' \pkg{brms::custom_family} definition packaged by the
#' Australian Institute of Marine Science at
#' \url{https://github.com/open-AIMS/tweedie}, with attribution
#' to those authors for the Stan function block, the choice of
#' the `mu` / `mphi` / `mtheta` parameterisation, and the
#' default series-truncation count. Each non-zero observation
#' contributes a `log_sum_exp` over `M` Poisson-gamma mixture
#' components, where the truncation `M` covers the Poisson
#' rate `lambda = mu^(2 - theta) / ((2 - theta) * phi)`. The
#' tail mass at `m > M` decays geometrically when `M >>
#' lambda` and slowly otherwise; the default `M = 30` is
#' adequate when the implied `max(lambda)` across non-zero
#' observations stays well below `M`. `lambda` grows quickly
#' as `theta` approaches 2 (the `1 / (2 - theta)` factor) and
#' as `phi` approaches 0; for fits where `mtheta` is sampled
#' near the upper boundary, raise `M` (e.g. `tweedie(M = 100L)`)
#' and re-fit if posterior predictive checks show systematic
#' undershoot in the non-zero tail. The Dunn & Smyth (2005)
#' convergence analysis discusses truncation bounds; see also
#' the runtime advisory printed by `check_tweedie_truncation()`.
#'
#' Identifiability under a latent trend. Because `mu_t` is a
#' stochastic latent process, the joint `(mu_t, mphi, mtheta)`
#' posterior can have a ridge along the marginal-variance
#' relation `Var[Y] = phi * mu^theta`. If the data do not
#' strongly distinguish Poisson-like from Gamma-like tail
#' behaviour, place an informative prior on `mtheta` (a
#' reasonable default is `prior(normal(1.5, 0.3), class =
#' mtheta)`) and inspect `pairs(fit, variable = c("mphi",
#' "mtheta"))` for funnel geometry.
#'
#' R-side likelihood evaluation (`log_lik()`) and posterior
#' predictive draws (`posterior_predict()`) use \pkg{mgcv}
#' (already an mvgam dependency); `mgcv::ldTweedie()` returns
#' log densities directly so LOO-IC and WAIC do not suffer the
#' underflow-then-floor pattern that a `log(dtweedie(...))`
#' two-step introduces.
#'
#' @param M Positive integer; truncation of the Poisson-gamma
#'   mixture series in the Stan log-density. Default `30L`.
#'   Larger values cost linearly more compute per non-zero
#'   observation; values below the practical tail mass produce
#'   downward-biased likelihoods, which translate to bias in
#'   `(mphi, mtheta)` posteriors. Use
#'   `check_tweedie_truncation()` after a fit to verify.
#'
#' @return A `brms::customfamily` object with mvgam-side
#'   stanvars attached as `attr(., "mvgam_stanvars")`. Pass
#'   directly to `mvgam(family = ...)`; no manual stanvars
#'   wiring is required.
#'
#' @references
#' Australian Institute of Marine Science (AIMS) (2023).
#'   `open-AIMS/tweedie`: a brms `custom_family` implementation
#'   of the Tweedie compound Poisson-gamma distribution. GitHub,
#'   \url{https://github.com/open-AIMS/tweedie}. MIT licence.
#'   The Stan function block and the `mu` / `mphi` / `mtheta`
#'   parameterisation in this file are adapted from that
#'   project.
#'
#' Spinkney (2020). Tweedie likelihood (compound Poisson-gamma)
#'   in Stan. Stan Discourse,
#'   \url{https://discourse.mc-stan.org/t/14636}. Source of the
#'   compound Poisson-gamma decomposition that underlies both
#'   the AIMS implementation and the Stan code shipped here.
#'
#' Dunn, P. K., & Smyth, G. K. (2005). Series evaluation of
#'   Tweedie exponential dispersion model densities.
#'   *Statistics and Computing*, 15, 267-280.
#'   \doi{10.1007/s11222-005-4070-y}. Convergence analysis of
#'   the series truncation `M` used here.
#'
#' Dunn, P. K., & Smyth, G. K. (2008). Evaluation of Tweedie
#'   exponential dispersion model densities by Fourier inversion.
#'   *Statistics and Computing*, 18, 73-86.
#'   \doi{10.1007/s11222-007-9039-6}.
#'
#' Jorgensen, B. (1987). Exponential dispersion models.
#'   *Journal of the Royal Statistical Society: Series B*, 49,
#'   127-162.
#'
#' Bürkner, P.-C. (2018). Advanced Bayesian multilevel modeling
#'   with the R package brms. *The R Journal*, 10, 395-411.
#'
#' @export
tweedie <- function(M = 30L) {
  checkmate::assert_integerish(
    M, lower = 1L, len = 1L, any.missing = FALSE
  )
  M <- as.integer(M)
  fam <- brms::custom_family(
    name = "tweedie",
    dpars = c("mu", "mphi", "mtheta"),
    links = c("log", "identity", "identity"),
    lb = c(NA, 1e-8, 1.001),
    ub = c(NA, NA, 1.999),
    type = "real",
    loop = FALSE,
    vars = "M"
  )
  # brms::custom_family() does not attach the standard
  # `linkinv` / `linkfun` slots that base R's family objects
  # carry; mvgam's downstream `compute_family_epred()` and
  # other dispatchers expect them. Attach them now from the
  # primary `link` so `family$linkinv(linpred)` works without
  # a brms-internal lookup.
  link_info <- stats::make.link(fam$link)
  fam$linkinv <- link_info$linkinv
  fam$linkfun <- link_info$linkfun
  # brms keeps `family$family = "custom"` for any customfamily
  # and stores the user-visible name in `family$name`. We do not
  # overwrite `family$family` here because brms internals look
  # up `.family_custom` to drive its dispatch and breaking that
  # crashes the standata step. mvgam's dispatchers use
  # `resolve_family_name()` (see below) to pick the right
  # string instead.
  attr(fam, "mvgam_stanvars") <- make_tweedie_stanvars(M)
  fam
}

#' Resolve the user-facing family name for dispatch
#'
#' For built-in brms families, `family$family` is the canonical
#' name. For `brms::custom_family` objects, `family$family` is
#' the literal string `"custom"` and the user-visible name is
#' stored in `family$name`. mvgam dispatchers (`compute_family_epred`,
#' `dispatch_log_lik`, `sample_from_family`, `get_family_dpars`)
#' all key on the user-visible name, so route through this
#' helper instead of reading `family$family` directly.
#'
#' @param family A `family` / `brmsfamily` / `customfamily`.
#' @return Character scalar.
#' @noRd
resolve_family_name <- function(family) {
  if (inherits(family, "customfamily") &&
        !is.null(family$name) && nzchar(family$name)) {
    return(family$name)
  }
  # stats::Gamma() reports family$family = "Gamma" with the capital
  # initial, but brms normalises to lowercase internally and every
  # mvgam dispatcher (log_lik_*, posterior_predict family switch)
  # is keyed on the lowercase "gamma". Align here so the dispatchers
  # find the right branch regardless of how the user spells the call.
  fam <- family$family
  if (identical(fam, "Gamma")) fam <- "gamma"
  fam
}

#' Build the Stan stanvars bundle for the Tweedie family
#'
#' Returns a `brms::stanvar()` collection containing the
#' function-block helpers used by the Tweedie log-pdf (compound
#' Poisson-gamma decomposition) plus a data stanvar declaring
#' the integer truncation `M`.
#'
#' @param M Positive integer; series truncation passed as Stan
#'   data.
#' @return A `brmsstanvars` object suitable for
#'   `brms::stanvar() + ...`.
#' @noRd
make_tweedie_stanvars <- function(M) {
  checkmate::assert_integerish(M, lower = 1L, len = 1L)
  M <- as.integer(M)
  # Explicit names on every stanvar so the brms `c.stanvars`
  # de-duplicator does not collide with anonymous function-block
  # stanvars emitted by the trend setup.
  brms::stanvar(
    name = "tweedie_funs",
    scode = tweedie_stan_funs(),
    block = "functions"
  ) +
    brms::stanvar(
      x = M,
      name = "M",
      scode = "int<lower=1> M;",
      block = "data"
    )
}

#' Stan code for the Tweedie function-block helpers
#'
#' Pulled out into its own helper so the (long) Stan string is
#' easy to diff and so contract tests can grep specific
#' signatures. Source: Spinkney (Stan Forums, 2020); packaged
#' for brms by open-AIMS/tweedie. mvgam adds strict-positivity
#' guards on `mu` and `mphi` and tighter bounds on `mtheta` to
#' avoid the numerical edge cases at the Poisson (`theta = 1`)
#' and Gamma (`theta = 2`) boundaries.
#' @noRd
tweedie_stan_funs <- function() {
  paste(
    "  int num_non_zero_fun(vector y) {",
    "    int A = 0;",
    "    int N = num_elements(y);",
    "    for (n in 1 : N) {",
    "      if (y[n] != 0) {",
    "        A += 1;",
    "      }",
    "    }",
    "    return A;",
    "  }",
    "",
    "  array[] int non_zero_index_fun(vector y, int A) {",
    "    int N = num_elements(y);",
    "    array[A] int non_zero_index;",
    "    int counter = 0;",
    "    for (n in 1 : N) {",
    "      if (y[n] != 0) {",
    "        counter += 1;",
    "        non_zero_index[counter] = n;",
    "      }",
    "    }",
    "    return non_zero_index;",
    "  }",
    "",
    "  array[] int zero_index_fun(vector y, int Z) {",
    "    int N = num_elements(y);",
    "    array[Z] int zero_index;",
    "    int counter = 0;",
    "    for (n in 1 : N) {",
    "      if (y[n] == 0) {",
    "        counter += 1;",
    "        zero_index[counter] = n;",
    "      }",
    "    }",
    "    return zero_index;",
    "  }",
    "",
    "  // Element-wise check on vector-valued mphi / mtheta. Both",
    "  // scalar and vector dpar shapes are supported: brms passes",
    "  // scalars when no sub-formula is given (e.g. family =",
    "  // tweedie()) and vectors when distributional regression is",
    "  // active (e.g. bf(y ~ x, mphi ~ site)). The overloaded",
    "  // tweedie_lpdf signatures below broadcast scalars to",
    "  // vectors via rep_vector and delegate to the per-element",
    "  // implementation.",
    "  void check_tweedie(vector mu, vector mphi, vector mtheta) {",
    "    int N = num_elements(mu);",
    "    for (n in 1 : N) {",
    "      if (mphi[n] <= 0) {",
    "        reject(\"mphi must be > 0; found mphi =\", mphi[n],",
    "               \"on element\", n);",
    "      }",
    "      if (mtheta[n] <= 1 || mtheta[n] >= 2) {",
    "        reject(\"mtheta must be in (1, 2); found mtheta =\",",
    "               mtheta[n], \"on element\", n);",
    "      }",
    "      if (mu[n] <= 0) {",
    "        reject(\"mu must be > 0; found mu =\", mu[n],",
    "               \"on element\", n);",
    "      }",
    "    }",
    "  }",
    "",
    "  // Per-element implementation; lambda, alpha, beta are all",
    "  // per-observation under distributional regression on phi or",
    "  // theta.",
    "  real tweedie_lpdf(vector y, vector mu, vector mphi,",
    "                    vector mtheta, int M) {",
    "    check_tweedie(mu, mphi, mtheta);",
    "    int N = num_elements(y);",
    "    int N_non_zero = num_non_zero_fun(y);",
    "    int N_zero = N - N_non_zero;",
    "    array[N_zero] int zero_index = zero_index_fun(y, N_zero);",
    "    array[N_non_zero] int non_zero_index =",
    "        non_zero_index_fun(y, N_non_zero);",
    "    vector[N] lambda = (mu .^ (2 - mtheta))",
    "                       ./ ((2 - mtheta) .* mphi);",
    "    vector[N] alpha = (2 - mtheta) ./ (mtheta - 1);",
    "    vector[N] beta = (mu .^ (1 - mtheta))",
    "                     ./ ((mtheta - 1) .* mphi);",
    "    real lp = -sum(lambda[zero_index]);",
    "    for (n in 1 : N_non_zero) {",
    "      int idx = non_zero_index[n];",
    "      vector[M] ps;",
    "      for (m in 1 : M) {",
    "        ps[m] = poisson_lpmf(m | lambda[idx])",
    "              + gamma_lpdf(y[idx] | m * alpha[idx], beta[idx]);",
    "      }",
    "      lp += log_sum_exp(ps);",
    "    }",
    "    return lp;",
    "  }",
    "",
    "  // Overloaded scalar entry points broadcast and dispatch.",
    "  real tweedie_lpdf(vector y, vector mu, real mphi,",
    "                    real mtheta, int M) {",
    "    int N = num_elements(y);",
    "    return tweedie_lpdf(y | mu, rep_vector(mphi, N),",
    "                        rep_vector(mtheta, N), M);",
    "  }",
    "",
    "  real tweedie_lpdf(vector y, vector mu, vector mphi,",
    "                    real mtheta, int M) {",
    "    int N = num_elements(y);",
    "    return tweedie_lpdf(y | mu, mphi,",
    "                        rep_vector(mtheta, N), M);",
    "  }",
    "",
    "  real tweedie_lpdf(vector y, vector mu, real mphi,",
    "                    vector mtheta, int M) {",
    "    int N = num_elements(y);",
    "    return tweedie_lpdf(y | mu, rep_vector(mphi, N),",
    "                        mtheta, M);",
    "  }",
    sep = "\n"
  )
}

#' Diagnose whether the Tweedie series-truncation `M` is adequate
#'
#' Computes the per-observation Poisson rate
#' `lambda = mu^(2 - theta) / ((2 - theta) * phi)` at the
#' posterior mean of `(mu, mphi, mtheta)` and compares it
#' against the truncation count `M` saved in the fit's
#' standata. Warns if `max(lambda)` exceeds `0.7 * M`, the
#' rough point at which the `log_sum_exp` over `m = 1..M`
#' starts to omit appreciable Poisson tail mass and bias the
#' log-density downward.
#'
#' @param object An `mvgam` fit using the `tweedie()` family.
#' @return Invisible `NULL`. Emits a message indicating the
#'   adequacy of `M`, and a warning if `M` looks tight.
#' @export
check_tweedie_truncation <- function(object) {
  checkmate::assert_class(object, "mvgam")
  fam <- object$family
  if (!inherits(fam, "customfamily") ||
        !identical(fam$name, "tweedie")) {
    stop(insight::format_error(
      "'check_tweedie_truncation()' only applies to fits with family = tweedie()."
    ))
  }
  sd <- object$model_data %||% object$standata
  if (is.null(sd) || is.null(sd$M)) {
    stop(insight::format_error(
      "Could not locate the truncation 'M' in the fit's standata."
    ))
  }
  M <- sd$M
  # Posterior mean of mu (per observation) and the two global
  # scalars; mu is on the response scale via family$linkinv.
  pe <- try(posterior_epred(object), silent = TRUE)
  if (inherits(pe, "try-error") || !is.matrix(pe)) {
    stop(insight::format_error(
      "Failed to extract posterior epred for the Tweedie fit."
    ))
  }
  mu_mean <- colMeans(pe)
  draws <- posterior::as_draws_matrix(object$fit)
  mphi_mean <- mean(draws[, "mphi"])
  mtheta_mean <- mean(draws[, "mtheta"])
  lambda <- mu_mean^(2 - mtheta_mean) /
    ((2 - mtheta_mean) * mphi_mean)
  lambda_max <- max(lambda)
  message(
    "Tweedie truncation diagnostic: M = ", M,
    ", max(lambda) at posterior mean = ",
    format(lambda_max, digits = 3),
    " (ratio = ", format(lambda_max / M, digits = 3), ")."
  )
  if (lambda_max > 0.7 * M) {
    rlang::warn(insight::format_warning(c(
      "Tweedie truncation 'M' may be too small.",
      x = paste0(
        "max(lambda) at the posterior mean is ",
        format(lambda_max, digits = 3),
        ", which is more than 70% of M = ", M, "."
      ),
      i = paste0(
        "Refit with a larger truncation, e.g. ",
        "tweedie(M = ", ceiling(lambda_max * 2), "L), and ",
        "compare 'mphi' / 'mtheta' posteriors."
      )
    )))
  }
  invisible(NULL)
}

# ============================================================
# Closure-unit family helpers (nmix, occ, royle_nichols, ...)
# ============================================================
# Every family in this group shares the same wire format: one
# row per visit in the user's data; the (series, time) pair
# identifies a closure unit; replicate rows are visits. The Stan
# lpdf operates per closure unit, marginalising the discrete
# latent state (abundance N for nmix, occupancy Z for occ). All
# closure-unit families flow through four primitives:
#
#   1. `is_closure_unit_family()`: predicate gating downstream
#      bypasses (duplicate-time validation skip, prediction-type
#      routing, etc).
#   2. `build_closure_unit_arrays()`: long-format data to unit
#      arrays (N_unit, n_rep, K_max, Y_max, visit_idx). Called
#      both at fit time and at predict time with newdata so the
#      cap column can vary per-call.
#   3. `validate_closure_unit_data()`: cap column, integer
#      counts, per-unit `cap >= max(y)`, identifiability
#      warnings.
#   4. Family-specific `make_*_stanvars(arrays, ...)`: Stan
#      function block + per-family data block; emitted at fit
#      time once the unit arrays are known.
#
# Future families (`occ()`, `royle_nichols()`, `poisson_poisson()`)
# reuse all four primitives; only step 4 changes per family.
#
# Cap tracking. The per-unit upper truncation `K_max[g]` is
# always read from the `cap` column of the input data. At fit
# time it lands in standata as `K_max` and shapes the Stan
# marginalisation loop. At prediction time, `newdata` must also
# carry a `cap` column; predict.mvgam re-runs
# `build_closure_unit_arrays()` on the new data, so users can
# request predictions under different upper truncations simply
# by passing newdata with different cap values. Predictions at
# the fit-time data reuse the stored K_max without re-deriving
# it.

#' Detect whether a family uses the closure-unit wire format
#'
#' Returns TRUE if the family is one of the closure-unit
#' detection-error families (currently `nmix()`; future
#' `occ()`, `royle_nichols()`, `poisson_poisson()`). Detected
#' via the `mvgam_closure_unit` attribute attached by each
#' family constructor.
#'
#' @param family A family / brmsfamily / customfamily object.
#' @return TRUE or FALSE.
#' @noRd
is_closure_unit_family <- function(family) {
  if (is.null(family)) return(FALSE)
  isTRUE(attr(family, "mvgam_closure_unit", exact = TRUE))
}

#' Detect whether a family wires a multi-response per-unit lpdf
#'
#' Returns TRUE if the family is one of the multivariate-response
#' families that aggregate K species rows per closure unit
#' (`diri`, `mvgam_multinomial`, `mvgam_categorical`,
#' `mvgam_mvnormal`, `mvgam_mvt`). Detected via the
#' `mvgam_multi_response` attribute attached by each family
#' constructor. These families piggyback on the closure-unit data
#' prep pipeline (site = closure unit, K species rows per unit) and
#' replace the per-row likelihood call with a per-unit K-vector
#' multivariate likelihood (Stan's native `dirichlet_logit_lpdf`,
#' `multinomial_logit_lpmf`, `categorical_logit_lpmf`,
#' `multi_normal_cholesky_lpdf`, `multi_student_t_lpdf`).
#'
#' @param family A family / brmsfamily / customfamily object.
#' @return TRUE or FALSE.
#' @noRd
is_multi_response_family <- function(family) {
  if (is.null(family)) return(FALSE)
  isTRUE(attr(family, "mvgam_multi_response", exact = TRUE))
}

#' Detect whether a closure-unit family routes its post-fit
#' surface (pp_check, residuals, log_lik, posterior_predict /
#' epred) through the per-unit aggregation pipeline.
#'
#' TRUE for `nmix()` and `occ()`: their Stan likelihood
#' marginalises a latent state (N for nmix, z for occ) and
#' downstream R-side methods aggregate per-visit responses to the
#' closure-unit grain via `closure_unit_pp_check_setup()` /
#' `compute_closure_unit_residuals()`. FALSE for the mv-response
#' families (`mvn()`, `mvt()`): their per-row residual is
#' conditionally independent given the latent factor scores
#' baked into mu by the trend pipeline, so post-fit methods
#' operate at the (site, species) row grain directly.
#'
#' Used by `pp_check.mvgam()`, `residuals.mvgam()`, and the
#' predict-time guard in `extract_closure_unit_components()` to
#' route mv-response fits around the aggregation / cap-validation
#' machinery that does not apply to them.
#'
#' @param family A family / brmsfamily / customfamily object.
#' @return TRUE or FALSE.
#' @noRd
needs_closure_unit_aggregation <- function(family) {
  is_closure_unit_family(family) &&
    !is_multi_response_family(family)
}

#' Detect whether a multi-response family uses the softmax-based
#' simplex likelihood and therefore needs the sum-to-zero soft
#' constraint on each column of Z to remove the shift indeterminacy
#' that softmax leaves unidentified.
#'
#' Returns TRUE for `diri`, `mvgam_multinomial`,
#' `mvgam_categorical`. Returns FALSE for `mvgam_mvnormal`,
#' `mvgam_mvt` (multivariate normal lpdfs are sensitive to absolute
#' mu levels, so the column-sum constraint is unnecessary and
#' over-restrictive there).
#'
#' Detected via the `mvgam_simplex_response` attribute attached
#' by the three simplex family constructors. The attribute is
#' separate from `mvgam_multi_response` so MV-normal/MV-T can opt
#' into the multi-response wire format without inheriting the
#' identification constraint.
#'
#' @param family A family / brmsfamily / customfamily object.
#' @return TRUE or FALSE.
#' @noRd
is_simplex_response_family <- function(family) {
  if (is.null(family)) return(FALSE)
  isTRUE(attr(family, "mvgam_simplex_response", exact = TRUE))
}

#' Default `brms::prior()` set for simplex multi-response families
#'
#' brms `custom_family()` does not trigger the family-specific prior
#' dispatch and emits flat improper priors on population-level effects
#' by default. Under the mode-2 reference subtraction
#' `mu[idx] - mu[idx[1]]` inside the lpdf (see `diri_stan_funs()`
#' etc.), any K-shared population effect (`b_Intercept`, `b_env`,
#' etc.) has no likelihood contribution at all and would walk
#' unboundedly under a flat prior.
#'
#' Returns a `brmsprior` with `student_t(3, 0, 2.5)` on both
#' `class = "b"` and `class = "Intercept"`. Matches Gelman et al.
#' (2008) for logistic regression with standardised predictors and
#' the brms built-in default for families that trigger the dispatch.
#'
#' Merged with the user's `priors` argument such that user-supplied
#' priors take precedence (brms `validate_prior()` resolves duplicates
#' in favour of explicit user input).
#'
#' @return A `brmsprior` object.
#' @noRd
default_simplex_population_priors <- function() {
  c(
    brms::prior("student_t(3, 0, 2.5)", class = "b"),
    brms::prior("student_t(3, 0, 2.5)", class = "Intercept")
  )
}

#' Default per-unit upper truncation for a closure-unit family
#'
#' Reads the `mvgam_default_cap` family attribute. Returns the
#' integer default (e.g. `1L` for `occ()`, whose latent state is
#' binary) or `NULL` when the family requires the user to supply
#' a `cap` column (`nmix("poisson_binomial")`,
#' `nmix("royle_nichols")`, future `nmix("poisson_poisson")`, all
#' of which carry latent abundance possibly greater than one).
#'
#' The `mvgam_default_cap` attribute is distinct from
#' `mvgam_binary_response`: the latter only controls the `y in
#' {0, 1}` validation check. `nmix("royle_nichols")` sets the
#' response flag (binary detection input) but leaves the default
#' cap unset because its latent `N` is bounded by the user-supplied
#' `cap`, not by `1`.
#'
#' @param family A family / brmsfamily / customfamily object.
#' @return Integer scalar default cap, or `NULL`.
#' @noRd
closure_unit_default_cap <- function(family) {
  if (is.null(family)) return(NULL)
  default_cap <- attr(family, "mvgam_default_cap", exact = TRUE)
  if (is.null(default_cap)) return(NULL)
  as.integer(default_cap)
}

#' Read the closure-unit grouping vars off a family
#'
#' Returns the `mvgam_unit_grouping` family attribute, a character
#' vector of column names that jointly identify a closure unit.
#' `NULL` lets `build_closure_unit_arrays` and
#' `validate_closure_unit_data` apply their `c(series_var,
#' time_var)` default.
#'
#' @param family A `brmsfamily` (or family-like list).
#' @return Character vector or `NULL`.
#' @noRd
closure_unit_grouping <- function(family) {
  if (is.null(family)) return(NULL)
  attr(family, "mvgam_unit_grouping", exact = TRUE)
}


#' Per-unit K_max buffer for count closure-unit families
#'
#' Reads the `mvgam_default_cap_buffer` family attribute. When set
#' on a count family (`nmix()` Poisson-binomial, `nmix("poisson_
#' poisson")`), `build_closure_unit_arrays()` computes a per-unit
#' default `K_max[g] = max(y in g) + buffer` whenever the user
#' supplies no `cap` column. Returns `NULL` when no buffer is
#' configured (royle_nichols handles this via the static
#' `mvgam_default_cap`; the multi-response families do not use
#' K_max at all).
#'
#' @param family A `brmsfamily` (or family-like list).
#' @return Positive integer or `NULL`.
#' @noRd
closure_unit_default_cap_buffer <- function(family) {
  if (is.null(family)) return(NULL)
  buf <- attr(family, "mvgam_default_cap_buffer", exact = TRUE)
  if (is.null(buf)) return(NULL)
  as.integer(buf)
}

#' Build per-closure-unit indexing arrays from long-format data
#'
#' Walks the user's long-format observation data and groups rows
#' by (series, time) into closure units. Returns the integer
#' arrays needed to populate the Stan data block: number of
#' visits per unit, per-unit upper truncation (`K_max`), per-unit
#' maximum observed count (`Y_max`), and a `[N_unit, max_rep]`
#' matrix of visit-row indices (padded with `1L` on the right
#' for units with fewer than `max_rep` visits, harmless because
#' the Stan loop reads only the first `n_rep[g]` entries).
#'
#' Called from two sites with identical signature:
#' (a) at fit time, against the user's training data; and
#' (b) at predict time, against `newdata`. The fit-time call
#' fixes the Stan `K_max` in standata. The predict-time call
#' lets users request latent-state predictions under a
#' different upper truncation by supplying a different `cap`
#' column in newdata.
#'
#' @param data Long-format observation data frame.
#' @param response_var Name of the response column.
#' @param series_var Name of the series factor (default
#'   `"series"`).
#' @param time_var Name of the time column (default `"time"`).
#' @param cap_var Name of the per-row upper-truncation column
#'   (default `"cap"`). Must be constant within each closure
#'   unit; the constant value becomes `K_max[g]`. Optional when
#'   `default_cap` is supplied (used by binary-response families
#'   such as `occ()` where the upper truncation is always 1).
#' @param default_cap Optional integer; when set, fills in `cap`
#'   with this constant if the column is absent from `data`.
#' @param compute_y_max Logical. When TRUE (default) coerce the
#'   response to integer and compute per-unit `Y_max` + `K_max`
#'   arrays used by the count-based closure-unit families
#'   (`nmix()`, `occ()`). Multi-response families that aggregate
#'   K species rows per unit (`diri()`,
#'   `mvgam_multinomial()`, `mvgam_categorical()`,
#'   `mvgam_mvnormal()`, `mvgam_mvt()`) set this FALSE because
#'   their per-unit likelihood reads the response vector directly
#'   without a per-unit truncation bound. When FALSE the
#'   `Y_max`, `K_max`, and `cap_vals` slots are returned as `NA`.
#' @param unit_grouping_vars Character vector of column names that
#'   jointly identify a closure unit. Defaults to
#'   `c(series_var, time_var)` for the count-based detection-error
#'   families (each (species, site) pair is its own closure unit
#'   with repeated visits). Multi-response families set this to
#'   `time_var` only: one closure unit per site, with the K species
#'   rows treated as the "visits" within that unit. The
#'   `visit_idx` matrix then maps each (unit, k) pair to the row
#'   index of the k-th species row at that site.
#' @return Named list with elements `N_unit`, `n_rep`, `K_max`,
#'   `Y_max`, `visit_idx`, `max_rep`, `unit_labels`. `K_max` and
#'   `Y_max` are `NA` when `compute_y_max = FALSE`.
#' @noRd
build_closure_unit_arrays <- function(data,
                                       response_var,
                                       series_var  = "series",
                                       time_var    = "time",
                                       cap_var     = "cap",
                                       default_cap = NULL,
                                       default_cap_buffer = NULL,
                                       compute_y_max = TRUE,
                                       unit_grouping_vars = NULL) {
  checkmate::assert_data_frame(data, min.rows = 1L)
  checkmate::assert_string(response_var)
  checkmate::assert_string(series_var)
  checkmate::assert_string(time_var)
  checkmate::assert_string(cap_var)
  checkmate::assert_integerish(default_cap, lower = 1L, len = 1L,
                               null.ok = TRUE)
  checkmate::assert_integerish(default_cap_buffer, lower = 0L,
                               len = 1L, null.ok = TRUE)
  checkmate::assert_flag(compute_y_max)
  if (is.null(unit_grouping_vars)) {
    unit_grouping_vars <- c(series_var, time_var)
  }
  checkmate::assert_character(unit_grouping_vars, min.len = 1L,
                              any.missing = FALSE)
  # Required columns: response + grouping always; cap only when
  # neither a scalar default (`default_cap`) nor a data-driven
  # buffer (`default_cap_buffer`) is configured. Multi-response
  # families pass `compute_y_max = FALSE`, which drops the cap
  # requirement entirely.
  required_cols <- c(response_var, unit_grouping_vars)
  if (compute_y_max && is.null(default_cap) &&
        is.null(default_cap_buffer)) {
    required_cols <- c(required_cols, cap_var)
  }
  for (col in required_cols) {
    if (!col %in% colnames(data)) {
      stop(insight::format_error(c(
        paste0(
          "Closure-unit families require column '", col,
          "' to be present in 'data'."
        ),
        i = paste0(
          "Add '", col, "' to the data frame, or rename the ",
          "existing variable via the relevant `*_var` argument."
        )
      )))
    }
  }
  # Assemble the closure-unit key from the requested grouping
  # columns. For count-based families this is "(series, time)" so
  # each (species, site) pair is its own closure unit; for
  # multi-response families it is "time" alone so each site is one
  # closure unit and the K species rows are the per-unit
  # contributions.
  grouping_vals <- lapply(unit_grouping_vars, function(col) {
    as.integer(as.factor(data[[col]]))
  })
  unit_label  <- do.call(
    paste, c(grouping_vals, list(sep = "_"))
  )
  unit_levels <- unique(unit_label)
  unit_int    <- match(unit_label, unit_levels)
  n_unit      <- length(unit_levels)
  # Per-unit visit counts indexed in `unit_levels` order. `table`
  # would re-order alphabetically; an explicit tabulate keeps
  # the unit ordering deterministic.
  rep_counts  <- tabulate(unit_int, nbins = n_unit)
  if (any(rep_counts < 1L)) {
    stop(insight::format_error(
      "Every closure unit must have at least one visit row."
    ))
  }
  max_rep <- max(rep_counts)
  visit_idx <- matrix(1L, nrow = n_unit, ncol = max_rep)
  for (g in seq_len(n_unit)) {
    rows_g <- which(unit_int == g)
    visit_idx[g, seq_along(rows_g)] <- as.integer(rows_g)
  }
  if (!compute_y_max) {
    # Multi-response path: count families need Y_max + cap-driven
    # K_max for their per-unit truncation, but dirichlet /
    # multinomial / categorical / mv-normal / mv-t read the K
    # response components directly and do not truncate, so the
    # cap column requirement is dropped and Y_max + K_max return
    # as NA placeholders.
    return(list(
      N_unit      = n_unit,
      n_rep       = rep_counts,
      K_max       = NA_integer_,
      Y_max       = NA_integer_,
      visit_idx   = visit_idx,
      max_rep     = as.integer(max_rep),
      unit_labels = unit_levels
    ))
  }
  y_vals <- as.integer(data[[response_var]])
  if (anyNA(y_vals)) {
    stop(insight::format_error(c(
      paste0(
        "Closure-unit families do not yet support missing values ",
        "in the response '", response_var, "'."
      ),
      i = "Filter or impute before passing the data to mvgam()."
    )))
  }
  # Three cap branches in order of precedence:
  #   1. User-supplied `cap` column -> broadcast per row, then
  #      assert constant within a closure unit.
  #   2. Per-unit data-driven default via `default_cap_buffer`
  #      (count families): K_max[g] = Y_max[g] + buffer. Mirrors
  #      `unmarked::pcount(K = max(y) + 100)`. Conservative on
  #      small Y to avoid truncating the posterior; the cost is
  #      a wider inner k-loop. Users who know their abundance
  #      ceiling should supply `cap` explicitly to tighten.
  #   3. Scalar `default_cap` (e.g. RN's static K_max = 25):
  #      broadcast to every unit.
  cap_in_data <- cap_var %in% colnames(data)
  Y_max <- integer(n_unit)
  K_max <- integer(n_unit)
  if (cap_in_data) {
    cap_vals <- as.integer(data[[cap_var]])
    if (anyNA(cap_vals)) {
      stop(insight::format_error(
        paste0(
          "Missing values in '", cap_var,
          "' are not allowed for closure-unit families."
        )
      ))
    }
    for (g in seq_len(n_unit)) {
      rows_g <- visit_idx[g, seq_len(rep_counts[g])]
      Y_max[g] <- max(y_vals[rows_g])
      cap_g <- cap_vals[rows_g]
      if (length(unique(cap_g)) > 1L) {
        stop(insight::format_error(
          paste0(
            "'", cap_var,
            "' must be constant within a closure unit."
          )
        ))
      }
      K_max[g] <- cap_g[1L]
      # Closure-unit lpmf marginalises N from `max(y)` to K_max[g].
      # A cap below the observed maximum makes the floor exceed
      # the ceiling, producing an empty sum and a garbage
      # likelihood. Surface the offending unit so the user can
      # raise the cap.
      if (K_max[g] < Y_max[g]) {
        stop(insight::format_error(c(
          paste0(
            "'", cap_var,
            "[", unit_levels[g],
            "]' = ", K_max[g],
            " is below the observed max '",
            response_var, "' = ", Y_max[g], "."
          ),
          i = paste0(
            "Cap must be at least the unit's observed maximum",
            " count; raise the '", cap_var, "' value or drop",
            " the column to use the data-driven default."
          )
        )))
      }
    }
  } else if (!is.null(default_cap_buffer)) {
    for (g in seq_len(n_unit)) {
      rows_g <- visit_idx[g, seq_len(rep_counts[g])]
      Y_max[g] <- max(y_vals[rows_g])
      K_max[g] <- Y_max[g] + as.integer(default_cap_buffer)
    }
  } else {
    # Falls back to the scalar default_cap; NULL here means the
    # validator should have refused the call upstream.
    cap_scalar <- as.integer(default_cap)
    for (g in seq_len(n_unit)) {
      rows_g <- visit_idx[g, seq_len(rep_counts[g])]
      Y_max[g] <- max(y_vals[rows_g])
      K_max[g] <- cap_scalar
    }
  }
  list(
    N_unit      = n_unit,
    n_rep       = rep_counts,
    K_max       = K_max,
    Y_max       = Y_max,
    visit_idx   = visit_idx,
    max_rep     = as.integer(max_rep),
    unit_labels = unit_levels
  )
}

#' Closure-unit Poisson-binomial N-mixture family
#'
#' N-mixture model (Royle 2004, *Biometrics*) with a Poisson
#' latent abundance and binomial detection. Each observation
#' row is one visit; closure is enforced over (series, time)
#' pairs so multiple rows sharing the same (series, time)
#' represent replicate visits to the same underlying abundance.
#' The latent count `N_g` for closure unit `g` is marginalised
#' analytically over the truncated range
#' `K_max[g] >= k >= max(y[g, ])` using a `log_sum_exp` over
#' `poisson_log_lpmf(k | log_lambda_g) + binomial_logit_lpmf(y[g,] | k, logit_p_{g,j})`
#' (Royle 2004).
#'
#' Parameterised with two distributional parameters:
#' \describe{
#'   \item{`mu`}{positive abundance rate \eqn{\lambda} (log link, fixed)}
#'   \item{`p`}{per-visit detection probability (logit link, fixed)}
#' }
#'
#' Distributional regression. brms `bf()` syntax handles
#' covariate-dependent detection automatically:
#'
#' ```r
#' mvgam(bf(y ~ s(elev), p ~ s(tod)), family = nmix(), data = ...)
#' ```
#'
#' Identifiability. With a single visit per closure unit the
#' likelihood reduces to a thinned Poisson with only
#' `lambda * p` identified; the individual parameters are not.
#' Information about the decomposition flows entirely from
#' shared structure across units (covariates in either formula
#' and the across-unit hierarchical pooling). Practical
#' guidance from Kery (2018, *Ecology*): with 3 or more visits
#' per unit the model is reliably identified for detection
#' probabilities above 0.1; with 2 visits identification is
#' marginal below `p = 0.3`. `validate_closure_unit_data()`
#' warns when the average visit count is below 2 and errors when
#' every unit has only one visit alongside no covariates.
#'
#' Under overdispersed counts (negative-binomial truth instead
#' of Poisson), the detection probability is biased downward
#' and the abundance is biased upward; the product `lambda * p`
#' remains approximately consistent (Knape et al. 2018,
#' *Methods in Ecology and Evolution*). Use posterior
#' predictive checks to detect this; refit with a richer
#' abundance distribution if overdispersion is present.
#'
#' Data shape. A long-format data frame, one row per visit, with
#' columns: a response (count) column, `series` (factor),
#' `time` (integer), `cap` (per-row upper truncation; constant
#' within a closure unit), plus any covariates referenced in the
#' formulae. Multiple rows sharing the same (series, time) pair
#' encode replicate visits to one closure unit.
#'
#' Prediction at different caps. `cap` is carried as a data
#' column, not stored only as a Stan scalar. Predicting via
#' `predict(fit, newdata = X)` re-extracts the closure-unit
#' arrays from `newdata`, so users can request latent-abundance
#' draws under arbitrary upper truncations by supplying
#' `newdata` with different `cap` values. Set `cap` higher than
#' the fit-time cap to expand the latent-state support;
#' fit-time caps are reused when `newdata` is `NULL`.
#'
#' @return A `brms::customfamily` object with the
#'   `mvgam_closure_unit` attribute set; closure-unit data prep
#'   builds the unit arrays and attaches the Stan lpdf at fit
#'   time.
#'
#' @references
#' Royle, J. A. (2004). N-mixture models for estimating
#'   population size from spatially replicated counts.
#'   *Biometrics*, 60, 108-115.
#'   \doi{10.1111/j.0006-341X.2004.00142.x}.
#'
#' Dennis, E. B., Morgan, B. J. T., & Ridout, M. S. (2015).
#'   Computational aspects of N-mixture models. *Biometrics*,
#'   71, 237-246. \doi{10.1111/biom.12246}.
#'
#' Kery, M. (2018). Identifiability in N-mixture models: a
#'   large-scale screening test with bird data. *Ecology*, 99,
#'   281-288. \doi{10.1002/ecy.2093}.
#'
#' Knape, J., Arlt, D., Barraquand, F., Berg, A., Chevalier, M.,
#'   Part, T., Ruete, A., & Zmihorski, M. (2018). Sensitivity of
#'   binomial N-mixture models to overdispersion. *Methods in
#'   Ecology and Evolution*, 9, 2102-2114.
#'   \doi{10.1111/2041-210X.13062}.
#'
#' @param type Character scalar selecting the N-mixture variant.
#'   `"poisson_binomial"` (default) is the canonical Royle (2004)
#'   model: latent abundance `N ~ Poisson(lambda)`, per-visit
#'   counts `y ~ Binomial(N, p)` with logit-link per-visit
#'   detection `p`. `"royle_nichols"` is the Royle and Nichols
#'   (2003) binary-detection variant: latent `N ~ Poisson(lambda)`,
#'   per-visit binary outcomes `y ~ Bernoulli(1 - (1-r)^N)` with
#'   logit-link per-individual detection `r`. `"poisson_poisson"`
#'   is the Neyman Type A model used for encounter counts (e.g.
#'   camera-trap captures): latent `N ~ Poisson(lambda)`, per-visit
#'   counts `y ~ Poisson(N * p)` with log-link per-individual
#'   encounter rate `p`.
#'
#' @param multi_season Logical. When `TRUE`, closure units are
#'   defined by `(series, site, time)` instead of the default
#'   `(series, time)`. The data must carry a `site` column;
#'   produce it from a 4-axis observation array with
#'   [pivot_detection_array()] (set `multi_season = "hierarchical"`).
#'
#' @examples
#' \dontrun{
#' # Constant detection probability, abundance varies with elevation
#' mvgam(y ~ s(elev), family = nmix(), data = closure_unit_data)
#'
#' # Distributional regression on detection
#' mvgam(bf(y ~ s(elev), p ~ s(tod)),
#'       family = nmix(),
#'       data = closure_unit_data)
#'
#' # Royle-Nichols variant on binary detection / non-detection data
#' mvgam(y ~ s(elev), family = nmix("royle_nichols"),
#'       data = closure_unit_binary_data)
#'
#' # Poisson-Poisson variant on encounter counts
#' mvgam(bf(y ~ s(elev), p ~ tod),
#'       family = nmix("poisson_poisson"),
#'       data = closure_unit_count_data)
#' }
#'
#' @section Choosing a variant:
#' All three variants share the latent-abundance prior
#' `N ~ Poisson(lambda)`; they differ in how `N` maps to the
#' visit-level observation.
#'
#' \describe{
#'   \item{`"poisson_binomial"` (default; Royle 2004)}{Per-visit
#'     counts `y ~ Binomial(N, p)`. `y` is bounded by `N` (each
#'     individual contributes at most one count per visit). Fits
#'     replicate effort-bounded surveys: bird point counts,
#'     electrofishing passes, plot-level vegetation tallies.
#'     Detection covariates attach to `p` via
#'     `bf(y ~ ..., p ~ visit_effort)`.}
#'   \item{`"royle_nichols"` (Royle and Nichols 2003)}{Binary per
#'     visit, `y ~ Bernoulli(1 - (1 - r)^N)`. Requires `y` to be
#'     0 / 1. Fits surveys where the response is detect /
#'     not-detect: camera-trap photos collapsed over an interval,
#'     occupancy-style replicates, eDNA samples scored as binary.
#'     Detection is per-individual, so two individuals at a site
#'     can each push the visit-level probability up.}
#'   \item{`"poisson_poisson"` (Neyman Type A; Neyman 1939)}{Counts
#'     `y ~ Poisson(N * p)`. `y` is unbounded and can exceed `N`
#'     because each individual contributes an independent Poisson
#'     encounter rate. Fits encounter surveys where the same
#'     individual can be recorded multiple times per visit:
#'     camera-trap event counts, acoustic detection counts,
#'     mark-resight encounters. Log-link on both `lambda` and
#'     `p`.}
#' }
#'
#' Pick the variant from the survey, not the data: 0 / 1 visits
#' need `royle_nichols`; counts that cap at abundance need
#' `poisson_binomial`; counts that can re-detect the same
#' individual need `poisson_poisson`. PB and RN have the same
#' visit-level variance at fixed `(N, p)` and `(N, r)` so the
#' data cannot tell them apart after the fact; PPM separates from
#' PB only via the upper tail (PPM has no `y <= N` ceiling).
#'
#' @section Identification (Royle-Nichols):
#' The Stan code uses the logit link on per-individual detection
#' `r`. A flat Uniform(0, 1) prior on `r` propagates to a near-
#' saturated prior on the visit-level detection probability
#' `p_visit = 1 - (1-r)^N` for any moderate `lambda` (e.g.
#' `E[p_visit | r ~ U(0,1), lambda = 5] ~ 0.83`), creating a ridge
#' where small `r` flattens the likelihood in `lambda`. The logit
#' link with a `Normal(0, 1.5)` prior on the logit-r intercept
#' places most prior mass on `r` between 0.05 and 0.95, while
#' remaining workable on the logit scale even when the likelihood
#' is nearly flat. The per-unit upper truncation `K_max[g]` must
#' satisfy
#' both (a) `ppois(K_max, lambda_hat, lower.tail=FALSE) < 1e-4`
#' (Poisson tail negligible) and (b) `(1 - r_hat)^K_max < 1e-4`
#' (Royle-Nichols detection function saturated). Condition (b) is
#' specific to Royle-Nichols: under Poisson-binomial, too-small
#' `K_max` only costs computation; under Royle-Nichols with low
#' `r`, too-small `K_max` leaves non-negligible Poisson mass in
#' cells where the detection function still varies, biasing the
#' posterior on `lambda` upward.
#'
#' @section Identification (Poisson-Poisson):
#' The Stan code uses the log link on per-individual encounter
#' rate `p`. Bollen's `logit(p)` reparameterisation is rejected
#' because under a flat Uniform(0, 1) prior on `p` the implied
#' prior on the rate `mu / (1 - mu)` is Cauchy(0, 1) tailed,
#' which is ecologically unmotivated and computationally unstable.
#' Under intercept-only specifications on both `mu` and `p` only
#' the product `lambda * p` is identified by the data; the
#' marginal variance (Neyman Type A is over-dispersed Poisson)
#' provides weak cross-identification only when at least one
#' covariate separates the two parameters (Kery 2018; "Wild
#' posteriors in the wild" arXiv 2503.00239). The fit-time
#' validator emits a `rlang::warn(...)` when both formulae reduce
#' to intercept-only, recommending an informative prior on at
#' least one intercept. The default prior on the log-scale `p`
#' intercept is tighter than the Poisson-binomial default
#' (`Normal(0, 1)`, implying encounter rate mostly in
#' `(0.1, 10)` encounters per individual per visit) for the same
#' reason. The truncated-N marginalisation is required: the
#' closed-form Poisson(`lambda * p`) marginal is mean-equivalent
#' but distribution-wrong (it drops the Neyman Type A over-
#' dispersion, which is the only handle on separating `lambda`
#' from `p`; under the closed form the likelihood depends only
#' on the product `lambda * p`, so individual posteriors on
#' `lambda` and `p` are prior-dominated and
#' `predict(type = "latent_state")` returns the prior on `N`
#' rather than a data-informed posterior).
#'
#' @section Priors (Poisson-Poisson):
#' For Poisson-Poisson fits the recommended starting prior on
#' the log-scale `p` intercept is `Normal(0, 1)`, which puts
#' most prior mass on encounter rates between `exp(-2) ~ 0.14`
#' and `exp(2) ~ 7.4` per individual per visit. Pass this prior
#' via the `prior` argument of [mvgam()]:
#' ```r
#' mvgam(y ~ elev,
#'       family = nmix("poisson_poisson"),
#'       data   = closure_unit_count_data,
#'       prior  = c(brms::prior(normal(0, 1),
#'                              class = "Intercept",
#'                              dpar  = "p")))
#' ```
#' For rare species with very low encounter rates the centre
#' should shift down (e.g. `Normal(-2, 0.5)`); for passive
#' acoustic monitors logging many detections per individual the
#' centre should shift up (e.g. `Normal(2, 0.5)`). The
#' identifiability validator emits a `rlang::warn(...)` only at
#' the fully intercept-only configuration; with one-sided
#' covariates the intercept of the formula without a covariate
#' is still weakly identified, so an informative prior on that
#' intercept remains advisable.
#'
#' @section K_max defaults and saturation:
#' Each variant has a default that fires when the user supplies no
#' `cap` column on the input data:
#' \itemize{
#'   \item Poisson-binomial and Poisson-Poisson default to
#'     `K_max[g] = max(y in g) + 100`, the per-unit data-driven
#'     buffer used by `unmarked::pcount`. Conservative: pays a
#'     wider inner k-loop when counts are small (`K_max ~ 101`
#'     even with `N ~ 5`). Override via a `cap` column to tighten
#'     the marginalisation when abundance is known to be low.
#'   \item Royle-Nichols defaults to `K_max = 25` per unit (the
#'     `unmarked::occuRN` convention), because binary `y` carries
#'     no per-unit floor.
#' }
#' Whenever a user supplies a `cap` column whose value is below
#' the observed maximum `y` for a unit, the validator errors with
#' the offending unit identifier; the marginalisation floor
#' (`max(y)`) would otherwise exceed the ceiling (`K_max`) and the
#' likelihood would silently degenerate to an empty sum.
#'
#' Post-fit, [latent_N_saturation()] reports the share of the
#' conditional posterior that sits at `K_max[g]` for each unit;
#' units flagged there should have their cap raised to remove
#' truncation bias in `lambda`. The Royle-Nichols default is the
#' most likely to need overriding because of the static fallback;
#' Poisson-Poisson should also satisfy
#' `ppois(K_max[g], lambda_hat, lower.tail = FALSE) < 1e-4` for
#' the tail to be negligible.
#'
#' @section JSDM with imperfect detection:
#' Passing `nmix()` to [jsdgam()] with `n_lv > 0` composes the
#' closure-unit marginalisation with the Heaps factor model so the
#' user gets a hierarchical JSDM whose three levels each live in
#' a distinct dpar:
#'
#' \describe{
#'   \item{**lowest level**: env-driven latent factors.}{
#'     `factor_formula = ~ s(env, by = lv_axis())` makes each
#'     latent factor a smooth of an environmental covariate. The
#'     factors are identified by their environmental signature
#'     rather than by being free draws, which removes the
#'     intercept-vs-Z*lv competition that otherwise hampers
#'     identification when site-level intercepts are also fit on
#'     `mu`.}
#'   \item{**middle level**: species loadings.}{Each species `s`
#'     loads on the `K` factors via the row `Z[s, :]`, so
#'     `log_lambda[s, i] = b_X(species) + Z[s, :] %*% lv[i, :]`
#'     for site `i`. `Z` is identified up to sign via Heaps and
#'     Jermyn (2024) post-hoc QR.}
#'   \item{**top level**: imperfect detection.}{The `p` dpar
#'     carries its own brms sub-formula (e.g.,
#'     `bf(y ~ species, p ~ visit_effort)`) so the detection
#'     process is modelled independently of the latent abundance.
#'     The lpmf marginalises latent `N` per closure unit; nothing
#'     about the factor model has to be aware of the
#'     marginalisation, and nothing about the marginalisation has
#'     to be aware of the factor model.}
#' }
#'
#' Recovery on this composition is documented in
#' `tests/local/jsdgam_mv_nmix.R`. See [jsdgam()] for the
#' wrapper-level API and [lv_axis()] for the `by = lv_axis()`
#' sentinel that turns smooths into per-factor regressors.
#'
#' @section Importing array data:
#' Detection-history data from `unmarked::unmarkedFramePCount`,
#' `spOccupancy`, `ubms`, and `flocker` arrives as a 2D `[J, K]`
#' matrix (single-species), 3D `[N, J, K]` array (multi-species),
#' 4D `[N, J, T, K]` array (multi-season multi-species), or named
#' list of `[J, K]` matrices (`unmarkedFrameOccuMulti`). Use
#' [pivot_detection_array()] to convert any of these into the
#' long-format `data` that `mvgam(family = nmix())` /
#' `jsdgam(family = nmix())` expects, with `site_covs`,
#' `season_covs`, `site_season_covs`, and `obs_covs` broadcast
#' across the right grid axes.
#'
#' @section Performance and threading:
#' The Stan emission for `nmix()` carries three optimisations
#' beyond a naive K-loop enumeration:
#' \itemize{
#'   \item **Analytic ratio recurrence.** The Poisson-binomial
#'     marginalisation uses the log-space Horner form of the
#'     ratio recurrence `T_N / T_{N-1}`, evaluated backward from
#'     `K_max` to `K_min + 1`. Avoids per-`k` re-evaluation of
#'     `binomial_logit_lpmf` that the naive `log_sum_exp` form
#'     pays.
#'   \item **Cached log(N) lookup.** `log(N)` and
#'     `log(N - counts[j])` values come from a `log_n_lookup`
#'     vector precomputed once in `transformed data`, replacing
#'     scalar `log()` calls per inner iteration.
#'   \item **partial_sum + reduce_sum.** The per-closure-unit
#'     body lives in `partial_sum_nmix_lpmf` and the lpmf
#'     wrapper calls `reduce_sum` over the closure-unit index.
#'     With `mvgam(threads = N)` for `N > 1` the model is
#'     compiled with `stan_threads = TRUE` and TBB splits the
#'     closure-unit slice across `N` threads. Without
#'     `threads`, `reduce_sum` falls back to a serial loop.
#'     Grainsize is set to `max(1, N_unit / 8)` so single-
#'     threaded fits pay only ~8 dispatch calls per leapfrog
#'     and multi-threaded fits see ~8 chunks across cores.
#' }
#' On a 150-unit fixture (n_rep = 6, lambda ~ 4, K_max = 14),
#' the stack delivers roughly 2x sampling throughput vs the
#' naive enumeration baseline; the threaded variant
#' (`threads = 4`) adds another ~38% over the serial
#' `reduce_sum` form.
#'
#' @references
#' Royle, J. A., and Nichols, J. D. (2003). Estimating abundance
#'   from repeated presence-absence data or point counts.
#'   *Ecology*, 84, 777-790.
#'   \doi{10.1890/0012-9658(2003)084[0777:EAFRPA]2.0.CO;2}.
#'
#' @export
nmix <- function(type = c("poisson_binomial", "royle_nichols",
                          "poisson_poisson"),
                 multi_season = FALSE) {
  type <- match.arg(type)
  checkmate::assert_flag(multi_season)
  variant_config <- switch(
    type,
    poisson_binomial = list(
      name  = "nmix",
      links = c("log", "logit"),
      lb    = c(0, 0),
      ub    = c(NA, 1)
    ),
    royle_nichols = list(
      name  = "nmix_royle_nichols",
      links = c("log", "logit"),
      lb    = c(0, 0),
      ub    = c(NA, 1)
    ),
    # Poisson-Poisson: p is an encounter rate per individual per
    # visit, NOT a probability. Log link on both dpars keeps the
    # rate on the positive real line. Bollen's logit(p) reparam
    # is rejected because it imposes a Cauchy(0, 1)-tailed prior
    # on the rate (stats review 2026-06-10).
    poisson_poisson = list(
      name  = "nmix_poisson_poisson",
      links = c("log", "log"),
      lb    = c(0, 0),
      ub    = c(NA, NA)
    )
  )
  fam <- brms::custom_family(
    name  = variant_config$name,
    dpars = c("mu", "p"),
    links = variant_config$links,
    # Bounds at the family level let brms declare the scalar-dpar
    # case with the right constraints. The lpmf operates on the
    # response-scale dpars (post-inv-link), with each per-variant
    # function block converting back to the link scale internally
    # for numerically stable likelihood evaluation.
    lb    = variant_config$lb,
    ub    = variant_config$ub,
    type  = "int",
    loop  = FALSE
  )
  link_info <- stats::make.link(fam$link)
  fam$linkinv <- link_info$linkinv
  fam$linkfun <- link_info$linkfun
  attr(fam, "mvgam_closure_unit")  <- TRUE
  attr(fam, "mvgam_nmix_type")     <- type
  # K_max defaults mirror unmarked's long-standing conventions
  # (chosen to keep the marginalisation safely conservative; pay
  # the cost in wasted inner k-loop iterations rather than risk
  # truncating a posterior).
  #   * Poisson-binomial / Poisson-Poisson see counts, so the per-
  #     unit floor is data-driven: `K_max[g] = max(y in g) + 100`.
  #     This matches `unmarked::pcount(K = max(y) + 100)` and runs
  #     hot when counts are small (K_max ~ 101 even when N ~ 5),
  #     so users with abundant data should set `cap` to a tighter
  #     value to avoid wasted likelihood evaluations.
  #   * Royle-Nichols sees only 0 / 1, so there is no data-driven
  #     floor. Default `K_max = 25` matches
  #     `unmarked::occuRN(K = 25)`. Override via the `cap` column
  #     when the latent_N_saturation() diagnostic flags
  #     truncation.
  if (type == "royle_nichols") {
    attr(fam, "mvgam_binary_response") <- TRUE
    attr(fam, "mvgam_default_cap")     <- 25L
  } else {
    attr(fam, "mvgam_default_cap_buffer") <- 100L
  }
  attr(fam, "mvgam_predict_types") <- c("latent_state", "detection")
  # The lpmf signature determines which data fields brms must
  # thread through. Poisson-binomial and Poisson-Poisson use the
  # transformed-data `log_n_lookup` cache to skip per-iter scalar
  # log() calls in the inner marginalisation loop. Royle-Nichols
  # does not (the inner k-loop is k * log_1m_r vector mult, no
  # log of an integer), so its vars list omits it. Poisson-
  # Poisson additionally carries `k_start_ppm`, a per-unit lower
  # bound on the latent-N k-loop that lets high-count units skip
  # the deep Poisson tail; see make_nmix_poisson_poisson_stanvars
  # for the derivation.
  attr(fam, "mvgam_vars") <- switch(
    type,
    royle_nichols = c(
      "N_unit", "n_rep", "K_max", "Y_max", "visit_idx"
    ),
    poisson_poisson = c(
      "N_unit", "n_rep", "K_max", "Y_max", "visit_idx",
      "log_n_lookup", "k_start_ppm"
    ),
    c(
      "N_unit", "n_rep", "K_max", "Y_max", "visit_idx",
      "log_n_lookup"
    )
  )
  # Multi-season grouping. Closure units are defined by
  # (series, site, season) instead of the default (series, time).
  # The data must carry a `site` column; `pivot_detection_array`
  # with `multi_season = "hierarchical"` emits it. Downstream
  # readers (validator, array builder, log_lik, pp_check, tidier,
  # plot_factors, kfold) all consume the attribute via
  # closure_unit_grouping().
  if (multi_season) {
    attr(fam, "mvgam_unit_grouping") <- c("series", "site", "time")
  }
  # mvgam_stanvars is populated at data preparation time, once
  # the closure-unit arrays from the user's data are known.
  attr(fam, "mvgam_stanvars") <- NULL
  fam
}

#' Closure-unit single-season occupancy family
#'
#' Bernoulli-binomial occupancy model (MacKenzie et al. 2002,
#' *Ecology*) with a Bernoulli latent occupancy state and a
#' Bernoulli detection process. Each observation row is one
#' visit; closure is enforced over (series, time) pairs so
#' multiple rows sharing the same (series, time) represent
#' replicate detection / non-detection visits to the same
#' underlying occupancy state. The latent state `z_g` for
#' closure unit `g` is marginalised analytically: at sites with
#' at least one detection, `z_g = 1` is certain and no
#' marginalisation is needed; at sites with all-zero detection
#' histories, `log_sum_exp` combines the all-zero-given-occupied
#' and unoccupied branches (Royle and Dorazio 2008, ch. 3;
#' Kery and Royle 2016, ch. 10).
#'
#' Parameterised with two distributional parameters:
#' \describe{
#'   \item{`mu`}{per-site occupancy probability \eqn{\psi}
#'     (logit link, fixed)}
#'   \item{`p`}{per-visit detection probability (logit link,
#'     fixed)}
#' }
#'
#' Distributional regression. brms `bf()` syntax handles
#' covariate-dependent detection automatically:
#'
#' ```r
#' mvgam(bf(y ~ s(elev), p ~ s(tod)), family = occ(), data = ...)
#' ```
#'
#' Identifiability. With a single visit per closure unit and no
#' covariates the likelihood reduces to a Bernoulli with mean
#' `psi * p` and only that product is identified; the individual
#' parameters are prior-dominated (Royle and Dorazio 2008,
#' ch. 3.5). The validator warns in this configuration but
#' allows the fit. With 3 or more visits per unit and detection
#' probability above 0.1, identification is reliable (the same
#' Kery 2018 result that holds for nmix carries to the
#' Bernoulli-binomial form).
#'
#' Data shape. A long-format data frame, one row per visit, with
#' columns: a binary response (0 = non-detection, 1 = detection),
#' `series` (factor), `time` (integer), plus any covariates
#' referenced in the formulae. Multiple rows sharing the same
#' (series, time) pair encode replicate visits to one closure
#' unit. The `cap` column required by `nmix()` is implicit at
#' `1` for `occ()` and need not be supplied.
#'
#' @return A `brms::customfamily` object with the
#'   `mvgam_closure_unit` and `mvgam_binary_response` attributes
#'   set; closure-unit data prep builds the unit arrays and
#'   attaches the Stan lpdf at fit time.
#'
#' @section Importing array data:
#' Detection-history data from `unmarked::unmarkedFrameOccu`,
#' `unmarkedFrameOccuMulti`, `spOccupancy`, `ubms`, and `flocker`
#' arrives as a 2D `[J, K]` matrix, 3D `[N, J, K]` array, 4D
#' `[N, J, T, K]` array (multi-season), or named list of `[J, K]`
#' matrices per species. Use [pivot_detection_array()] to convert
#' any of these into the long-format `data` that
#' `mvgam(family = occ())` / `jsdgam(family = occ())` expect, with
#' `site_covs`, `season_covs`, `site_season_covs`, and `obs_covs`
#' broadcast across the right grid axes.
#'
#' @references
#' MacKenzie, D. I., Nichols, J. D., Lachman, G. B., Droege, S.,
#'   Royle, J. A., & Langtimm, C. A. (2002). Estimating site
#'   occupancy rates when detection probabilities are less than
#'   one. *Ecology*, 83, 2248-2255.
#'   \doi{10.1890/0012-9658(2002)083[2248:ESORWD]2.0.CO;2}.
#'
#' Royle, J. A., & Dorazio, R. M. (2008). *Hierarchical Modeling
#'   and Inference in Ecology*. Academic Press.
#'
#' Kery, M., & Royle, J. A. (2016). *Applied Hierarchical
#'   Modeling in Ecology, Vol. 1*. Academic Press.
#'
#' Socolar, J. B., & Mills, S. C. (2023). flocker: flexible
#'   occupancy estimation in R. *bioRxiv*.
#'   \doi{10.1101/2023.10.26.564080}.
#'
#' @section JSDM with imperfect detection:
#' Passing `occ()` to [jsdgam()] with `n_lv > 0` composes the
#' single-season Bernoulli marginalisation with the Heaps factor
#' model so the user gets a hierarchical JSDM whose three levels
#' each live in a distinct dpar:
#'
#' \describe{
#'   \item{**lowest level**: env-driven latent factors.}{
#'     `factor_formula = ~ s(env, by = lv_axis())` makes each
#'     latent factor a smooth of an environmental covariate, so
#'     the factors track env gradients rather than being free
#'     draws.}
#'   \item{**middle level**: species loadings on occupancy.}{Each
#'     species `s` loads on the `K` factors via the row
#'     `Z[s, :]`, so
#'     `logit_psi[s, i] = b_X(species) + Z[s, :] %*% lv[i, :]`
#'     for site `i`. `Z` is identified up to sign via Heaps and
#'     Jermyn (2024) post-hoc QR.}
#'   \item{**top level**: imperfect detection.}{The `p` dpar
#'     carries its own brms sub-formula (e.g.,
#'     `bf(y ~ species, p ~ s(tod))`) so the per-visit detection
#'     process is modelled independently of the latent occupancy
#'     state. The lpmf marginalises latent `z` per closure unit;
#'     nothing about the factor model has to be aware of the
#'     marginalisation, and nothing about the marginalisation has
#'     to be aware of the factor model.}
#' }
#'
#' Recovery on this composition is documented in
#' `tests/local/jsdgam_mv_occ.R`. See [jsdgam()] for the
#' wrapper-level API and [lv_axis()] for the `by = lv_axis()`
#' sentinel that turns smooths into per-factor regressors.
#'
#' @section Cross-reference with ubms / spOccupancy / flocker:
#' mvgam's `occ()` uses the long-form ecology vocabulary
#' (`occupancy`, `detection`) in `predict(type = ...)`. The
#' equivalents in adjacent packages:
#' \itemize{
#'   \item ubms
#'     (\url{https://github.com/biodiverse/ubms}):
#'     `predict(submodel = "state")` returns marginal psi;
#'     `predict(submodel = "det")` returns p;
#'     `posterior_predict(param = "z")` returns 0/1 latent
#'     occupancy draws conditioned on the observed history.
#'   \item spOccupancy
#'     (\url{https://github.com/biodiverse/spOccupancy}):
#'     `psi.0.samples` (marginal psi), `z.0.samples` (latent z
#'     draws).
#'   \item flocker
#'     (\url{https://github.com/jsocolar/flocker}):
#'     `fitted_flocker(components = "occ" | "det")` for linpred
#'     extraction; `get_Z(history_condition = TRUE)` for
#'     conditional z draws.
#' }
#'
#' mvgam: `predict(fit, type = "latent_state")` returns the
#' conditional probability `P(z = 1 | y)` per site;
#' `posterior_occupancy(fit, conditional = TRUE, draw = TRUE)`
#' returns 0/1 z draws; `posterior_occupancy(conditional = FALSE)`
#' returns marginal psi. `predict(fit, type = "detection")`
#' returns the per-visit detection probability p_{g,j} on the
#' response scale.
#'
#' @param multi_season Logical. When `TRUE`, closure units are
#'   defined by `(series, site, time)` instead of the default
#'   `(series, time)`. The data must carry a `site` column;
#'   produce it from a 4-axis observation array with
#'   [pivot_detection_array()] (set `multi_season = "hierarchical"`).
#'
#' @examples
#' \dontrun{
#' # Constant detection, occupancy varies with elevation
#' mvgam(y ~ s(elev), family = occ(), data = closure_unit_data)
#'
#' # Distributional regression on detection
#' mvgam(bf(y ~ s(elev), p ~ s(tod)),
#'       family = occ(),
#'       data = closure_unit_data)
#' }
#'
#' @export
occ <- function(multi_season = FALSE) {
  checkmate::assert_flag(multi_season)
  fam <- brms::custom_family(
    name  = "occ",
    dpars = c("mu", "p"),
    links = c("logit", "logit"),
    # mu (psi) and p are both probabilities. Setting both bounds
    # at the family level lets brms declare the scalar-dpar case
    # with the right constraints; the lpmf converts back to
    # logit scale internally for `log_inv_logit` /
    # `bernoulli_logit_lpmf` stability.
    lb    = c(0, 0),
    ub    = c(1, 1),
    type  = "int",
    loop  = FALSE
  )
  link_info <- stats::make.link(fam$link)
  fam$linkinv <- link_info$linkinv
  fam$linkfun <- link_info$linkfun
  attr(fam, "mvgam_closure_unit")    <- TRUE
  attr(fam, "mvgam_binary_response") <- TRUE
  # Latent z is binary, so the per-unit upper truncation is always
  # 1; closure_unit_default_cap() reads this attribute to make the
  # `cap` data column optional for occ() fits.
  attr(fam, "mvgam_default_cap")     <- 1L
  attr(fam, "mvgam_predict_types")   <- c("latent_state", "detection")
  # Multi-season grouping. Closure units are defined by
  # (series, site, season) instead of the default (series, time).
  # The data must carry a `site` column; `pivot_detection_array`
  # with `multi_season = "hierarchical"` emits it. Downstream
  # readers (validator, array builder, log_lik, pp_check, tidier,
  # plot_factors, kfold) all consume the attribute via
  # closure_unit_grouping().
  if (multi_season) {
    attr(fam, "mvgam_unit_grouping") <- c("series", "site", "time")
  }
  # occ_lpmf drops K_max from the nmix signature because the
  # latent z is binary; the lpmf reads Y_max directly as the
  # per-unit `any detection?` indicator.
  attr(fam, "mvgam_vars") <- c(
    "N_unit", "n_rep", "Y_max", "visit_idx"
  )
  # mvgam_stanvars is populated at data preparation time, once
  # the closure-unit arrays from the user's data are known.
  attr(fam, "mvgam_stanvars") <- NULL
  fam
}

#' Stan function block for the closure-unit single-season
#' occupancy lpmf
#'
#' Implements the MacKenzie et al. (2002) Bernoulli-binomial
#' marginalisation over the binary latent state `z` as a
#' `log_sum_exp` of the occupied (`z = 1`) and unoccupied
#' (`z = 0`) branches. For closure units with at least one
#' detection (`Y_max[g] >= 1`), `z = 1` is certain and the
#' marginalisation collapses to the occupied branch directly
#' (flocker / ubms fast path).
#'
#' Numerical-stability notes:
#'   - `mu` arrives from brms as the inv-logit linear predictor
#'     (probability); converting back to
#'     `logit_psi = logit(mu)` lets us use `log_inv_logit`
#'     and `log1m_inv_logit` for the two branches without
#'     boundary underflow.
#'   - `p` arrives from brms as the inv-logit linear predictor
#'     (probability); converting back to
#'     `logit_p = logit(p)` keeps `bernoulli_logit_lpmf` /
#'     `log1m_inv_logit` stable at probabilities close to 0 or
#'     1.
#'   - `log_sum_exp` factors out the larger of the two log
#'     contributions before exponentiating, so the
#'     small-probability branch can decay to a very large
#'     negative value without affecting the result.
#'
#' @param max_rep Positive integer maximum visit count across
#'   closure units. Sets the column count of `visit_idx`. The
#'   Stan loop reads only the first `n_rep[g]` columns per unit.
#' @return Character scalar of Stan function code.
#' @noRd
occ_stan_funs <- function(max_rep) {
  checkmate::assert_integerish(max_rep, lower = 1L, len = 1L)
  paste(
    "  // Per-closure-unit partial sum body. reduce_sum (called from",
    "  // the occ_lpmf wrapper) hands this function a slice [start:end]",
    "  // of the closure-unit index and gets back the partial log-",
    "  // likelihood for that chunk. When the model is compiled with",
    "  // stan_threads (auto-set by `threads = N` on mvgam()), TBB",
    "  // splits the slice across threads; otherwise reduce_sum runs",
    "  // serially. Link-scale conversions live on the wrapper so per-",
    "  // thread chunks see precomputed logit_psi / logit_p vectors",
    "  // instead of redoing the dpar transform per chunk.",
    "  real partial_sum_occ_lpmf(",
    "    array[] int g_slice,",
    "    int start, int end,",
    "    array[] int y,",
    "    vector logit_psi,",
    "    vector logit_p,",
    "    array[] int n_rep,",
    "    array[] int Y_max,",
    "    array[,] int visit_idx) {",
    "    real lp = 0;",
    "    for (g in start : end) {",
    "      int n_g = n_rep[g];",
    "      array[n_g] int idx = visit_idx[g, 1:n_g];",
    "      real lpsi_g = logit_psi[idx[1]];",
    "      array[n_g] int y_g = y[idx];",
    "      vector[n_g] lp_g  = logit_p[idx];",
    "      if (Y_max[g] >= 1) {",
    "        // Detected at least once: z = 1 is certain.",
    "        lp += log_inv_logit(lpsi_g)",
    "            + bernoulli_logit_lpmf(y_g | lp_g);",
    "      } else {",
    "        // All-zero history: marginalise z in {0, 1}.",
    "        // log_sum_exp factors out the larger contribution",
    "        // so the small branch can decay without underflow.",
    "        real loglik_z1 = log_inv_logit(lpsi_g)",
    "                       + sum(log1m_inv_logit(lp_g));",
    "        real loglik_z0 = log1m_inv_logit(lpsi_g);",
    "        lp += log_sum_exp(loglik_z1, loglik_z0);",
    "      }",
    "    }",
    "    return lp;",
    "  }",
    "",
    "  // Per-visit implementation. brms passes `mu` and `p` as",
    "  // vectors when either dpar carries a sub-formula (e.g.",
    "  // bf(y ~ s(elev), p ~ s(tod))). The overloaded scalar",
    "  // signatures below broadcast the static-dpar cases via",
    "  // rep_vector and delegate to this entry point.",
    "  real occ_lpmf(",
    "    array[] int y,",
    "    vector mu,",
    "    vector p,",
    "    int N_unit,",
    "    array[] int n_rep,",
    "    array[] int Y_max,",
    "    array[,] int visit_idx) {",
    "    // Link-scale conversions hoisted to the wrapper so the",
    "    // per-thread partial sum sees them precomputed.",
    "    vector[num_elements(mu)] logit_psi = logit(mu);",
    "    vector[num_elements(p)]  logit_p   = logit(p);",
    "    // Closure-unit index sliced by reduce_sum. Constructed",
    "    // locally so no extra data array is threaded through brms.",
    "    array[N_unit] int g_seq;",
    "    for (g in 1 : N_unit) g_seq[g] = g;",
    "    // grainsize heuristic targets ~8 chunks; matches the",
    "    // nmix wrapper. See ?nmix_stan_funs for the rationale.",
    "    int grainsize = N_unit >= 8 ? N_unit / 8 : 1;",
    "    return reduce_sum(",
    "      partial_sum_occ_lpmf, g_seq, grainsize,",
    "      y, logit_psi, logit_p, n_rep, Y_max, visit_idx",
    "    );",
    "  }",
    "",
    "  // Scalar-p entry point: broadcasts to per-visit length",
    "  // and dispatches to the vector implementation.",
    "  real occ_lpmf(",
    "    array[] int y,",
    "    vector mu,",
    "    real p,",
    "    int N_unit,",
    "    array[] int n_rep,",
    "    array[] int Y_max,",
    "    array[,] int visit_idx) {",
    "    int N = num_elements(mu);",
    "    return occ_lpmf(y | mu, rep_vector(p, N), N_unit,",
    "                    n_rep, Y_max, visit_idx);",
    "  }",
    "",
    "  // Scalar-mu entry point: same broadcast for the static",
    "  // occupancy case.",
    "  real occ_lpmf(",
    "    array[] int y,",
    "    real mu,",
    "    vector p,",
    "    int N_unit,",
    "    array[] int n_rep,",
    "    array[] int Y_max,",
    "    array[,] int visit_idx) {",
    "    int N = num_elements(p);",
    "    return occ_lpmf(y | rep_vector(mu, N), p, N_unit,",
    "                    n_rep, Y_max, visit_idx);",
    "  }",
    "",
    "  // Both-scalar entry point: broadcasts both dpars.",
    "  real occ_lpmf(",
    "    array[] int y,",
    "    real mu,",
    "    real p,",
    "    int N_unit,",
    "    array[] int n_rep,",
    "    array[] int Y_max,",
    "    array[,] int visit_idx) {",
    "    int N = num_elements(y);",
    "    return occ_lpmf(y | rep_vector(mu, N), rep_vector(p, N),",
    "                    N_unit, n_rep, Y_max, visit_idx);",
    "  }",
    sep = "\n"
  )
}

#' Build the closure-unit Stan stanvars for an `occ()` fit
#'
#' Wraps the shared `make_closure_unit_arrays_stanvars()` with
#' the occ-specific function block. `K_max` is omitted (latent z
#' is binary) and `Y_max` is bounded at 1 to mirror the response
#' support; the lpmf reads `Y_max[g] >= 1` as the per-unit
#' "any detection?" indicator that drives the fast-path branch.
#'
#' @inheritParams make_closure_unit_arrays_stanvars
#' @return A `brmsstanvars` object.
#' @noRd
make_occ_stanvars <- function(arrays) {
  make_closure_unit_arrays_stanvars(
    arrays,
    family_funs_name = "occ_funs",
    family_funs      = occ_stan_funs(arrays$max_rep),
    y_max_upper      = 1L,
    include_K_max    = FALSE
  )
}

#' Closure-unit Dirichlet-on-the-simplex family
#'
#' Composes Stan's native `dirichlet_logit_lpdf` with the mvgam
#' factor-model trend. Each closure unit (site) carries K rows
#' (one per species / response component) whose response values
#' sum to 1 within the unit. The custom lpdf assembles the per-unit
#' K-vectors of response and linear predictor inside Stan, then
#' calls Stan's native `dirichlet_logit_lpdf(y_unit | mu_unit, phi)`
#' which maps `mu_unit` through `softmax` and parameterises the
#' Dirichlet by `softmax(mu_unit) * phi`.
#'
#' Distributional parameters:
#' \describe{
#'   \item{`mu`}{per-row latent score on the unconstrained scale.
#'     Softmax inside the lpdf maps the K scores per unit to a
#'     simplex. Default link is identity.}
#'   \item{`phi`}{Dirichlet concentration (positive). Larger `phi`
#'     concentrates draws near `softmax(mu_unit)`. Default link is
#'     log.}
#' }
#'
#' Identification under the K-row free `Z` factor model: softmax
#' is shift-invariant in `mu_unit` along two independent directions,
#' both removed by HARD constraints (Stan >= 2.36). Each column of
#' `Z` is declared as Stan's native `sum_to_zero_vector[K]` so
#' column sums are exactly zero by construction (no soft prior,
#' no scale to tune; see Mitzi Morris "The Sum-to-Zero Constraint
#' in Stan" for the Helmert-style transform Stan uses internally).
#' The per-site K-shared fixed-effect shift is eliminated inside
#' the lpdf by subtracting `mu_unit[1]` from all entries before
#' softmax, matching the brms-native dirichlet reference-category
#' parameterisation. A weakly-informative `student_t(3, 0, 2.5)`
#' population-effect default prior is also injected because brms
#' `custom_family()` would otherwise emit flat improper priors;
#' under the hard mode-2 reference subtraction, any K-shared
#' coefficient (`b_Intercept`, `b_env`, etc.) has no likelihood
#' contribution and samples from that prior, so a once-per-session
#' warning fires from `jsdgam()` when the user's formula contains
#' no per-`species` interaction term. The `loadings_prior` matrix-normal prior, when
#' supplied, composes with the sum-to-zero constraint and is the
#' recommended path when traits or phylogeny are available.
#'
#' Data layout: long format, one row per (site, species), exactly
#' K rows per site. The user passes the species axis via the
#' `species` argument on `jsdgam()` (or labels it `series` in the
#' data when using `mvgam()` directly); the closure-unit data prep
#' groups by (series, time) so each site forms one closure unit.
#'
#' @section Distributional regression on `phi`:
#' A `phi ~ env` sub-formula in `bf()` (e.g.
#' `bf(y ~ env * species, phi ~ env, family = diri())` through
#' `mvgam()` rather than `jsdgam()`, which currently exposes only
#' the response formula) IS supported at fit time: the lpdf
#' dispatches `vector phi` vs `real phi` so brms can pass either.
#' Per-row `phi[i, k]` is collapsed to one scalar per closure unit
#' by taking `phi[idx[1]]`, which assumes the `phi` covariate is
#' constant across the K rows of any site (site-level only). A
#' species-level `phi` covariate is statistically ill-defined for
#' a Dirichlet observation; if you pass one, the lpdf silently uses
#' row 1's value and the user-facing pre-fit validator for this
#' contract is on the v2.0.1 roadmap.
#'
#' Post-fit response-scale dispatchers
#' (`posterior_predict()` / `posterior_epred()` / `log_lik()`)
#' currently expect a scalar `phi` and will error directionally on
#' a phi sub-formula fit. Interrogate the `b_phi_*` posterior
#' directly via `posterior::as_draws_matrix(fit$fit)` until the
#' v2.0.1 plumbing for per-row `phi` extraction lands.
#'
#' The default brms prior on `b_phi` regression coefficients is
#' flat. Consider supplying `prior(student_t(3, 0, 2.5), class = b,
#' dpar = phi)` via the `priors` argument to regularise the
#' log-concentration slopes (matches the simplex population-effect
#' default; flat priors leave room for implausible orders-of-magnitude
#' phi shifts under modest sample sizes).
#'
#' @section Post-fit conventions:
#' `log_lik()`, `loo()`, and `waic()` score at the **site grain**,
#' not the (site, species) row grain. The Stan lpdf evaluates the
#' joint K-vector Dirichlet density once per closure unit, so the
#' per-unit log-density is attributed to the first row of the unit
#' and the remaining K - 1 rows return `0` for `log_lik()`. This is
#' the only correct grain for joint multi-row outcomes and matches
#' the flocker / ubms convention for closure-unit families;
#' Vehtari, Gelman and Gabry (2017) recommend the joint-unit grain
#' for PSIS-LOO whenever the per-row likelihoods are not
#' conditionally independent given the parameters. To compare
#' simplex fits via `loo_compare()` against models scored at the
#' row grain, refit the row-grain model on the unit-aggregated
#' response or use `waic()` on both fits at consistent grain.
#'
#' `posterior_linpred()` returns the **uncentred** linear predictor
#' `mu[i, k]` per row (the value brms emits before the lpdf body
#' applies `mu_unit = mu[idx] - mu[idx[1]]` to remove the per-site
#' K-shared shift). This is the natural scale for `marginaleffects`
#' contrasts on the species axis. For probabilities on the simplex,
#' use `posterior_epred()`, which applies the reference subtraction
#' and softmax internally and returns one probability per (site,
#' species) row summing to 1 within each site.
#'
#' @return A `brms::custom_family` object tagged with the
#'   `mvgam_closure_unit`, `mvgam_multi_response`, and
#'   `mvgam_simplex_response` attributes that route the
#'   downstream data prep, validation, and Stan emission.
#'
#' @references
#' Aitchison, J. (1982). The statistical analysis of compositional
#'   data. *Journal of the Royal Statistical Society Series B*,
#'   44(2):139-177.
#'
#' Vehtari, A., Gelman, A. and Gabry, J. (2017). Practical Bayesian
#'   model evaluation using leave-one-out cross-validation and WAIC.
#'   *Statistics and Computing*, 27:1413-1432.
#'   \doi{10.1007/s11222-016-9696-4}
#'
#' Warton, D. I., Blanchet, F. G., O'Hara, R. B., Ovaskainen, O.,
#'   Taskinen, S., Walker, S. C. and Hui, F. K. C. (2015). So many
#'   variables: joint modeling in community ecology. *Trends in
#'   Ecology and Evolution*, 30(12):766-779.
#'   \doi{10.1016/j.tree.2015.09.007}
#'
#' Heaps, S. E. and Jermyn, I. H. (2024). Structured prior
#'   distributions for the covariance matrix in latent factor
#'   models. *Statistics and Computing*, 34:143.
#'   \doi{10.1007/s11222-024-10454-0}
#'
#' @examples
#' \dontrun{
#' # Compositional JSDM on long-format proportions data. The formula
#' # interacts the environmental covariate with `species` so each
#' # species gets its own intercept and `env` slope; this is the
#' # interpretable form for a simplex multi-response family. The
#' # brms-native-style `y ~ 0 + species + env:species` is equivalent.
#' mod <- jsdgam(
#'   formula = y ~ env * species,
#'   factor_formula = ~ -1,
#'   data = my_long_format_data,
#'   unit = site,
#'   species = species,
#'   family = diri(),
#'   n_lv = 2
#' )
#' plot(residual_cor(mod))
#' }
#'
#' @export
diri <- function() {
  fam <- brms::custom_family(
    name  = "diri",
    dpars = c("mu", "phi"),
    links = c("identity", "log"),
    lb    = c(NA, 0),
    ub    = c(NA, NA),
    type  = "real",
    loop  = FALSE
  )
  link_info_mu <- stats::make.link("identity")
  fam$linkinv <- link_info_mu$linkinv
  fam$linkfun <- link_info_mu$linkfun
  attr(fam, "mvgam_closure_unit")      <- TRUE
  attr(fam, "mvgam_multi_response")    <- TRUE
  attr(fam, "mvgam_simplex_response")  <- TRUE
  # Dirichlet has no latent-state predict surface; downstream
  # `predict()` and `posterior_predict()` use the standard
  # response-scale dispatch.
  attr(fam, "mvgam_predict_types")     <- character(0L)
  # brms threads these data arrays into the lpdf call site so the
  # Stan function block can assemble the K-vector per unit.
  attr(fam, "mvgam_vars") <- c(
    "N_unit", "n_rep", "visit_idx"
  )
  attr(fam, "mvgam_stanvars") <- NULL
  fam
}

#' Stan function block for the closure-unit Dirichlet lpdf
#'
#' Assembles the per-unit K-vector of response values and linear
#' predictor scores from the long-format `Y` and `mu` arrays via
#' `visit_idx`, then calls Stan's native
#' `dirichlet_logit_lpdf(y_unit | mu_unit, phi)` once per closure
#' unit. The softmax over `mu_unit` parameterises the Dirichlet,
#' so the K mu rows are interpretable on a common scale. The
#' post-hoc Heaps QR identification of `Z` + the hard
#' `sum_to_zero_vector[K]` constraint on Z columns + the per-unit
#' `mu_unit = mu[idx] - mu[idx[1]]` reference subtraction together
#' fix the three shift modes that softmax otherwise leaves in
#' `mu_unit`.
#'
#' brms calls this function ONCE per likelihood evaluation with
#' the full `Y` and `mu` vectors plus the closure-unit indexing
#' arrays threaded through via `family$vars`.
#'
#' @return A character scalar suitable for
#'   `brms::stanvar(scode = ..., block = "functions")`.
#' @noRd
diri_stan_funs <- function() {
  # Stan's native `dirichlet_lpdf(y | alpha)` is the only built-in
  # for the Dirichlet density; `dirichlet_logit_lpdf(y | mu, phi)`
  # is a brms-emitted helper that only lives in brms-native
  # Dirichlet stancode. Because our custom family does not trigger
  # brms' native Dirichlet emission, we apply the same logit
  # parameterisation explicitly via `softmax(mu_unit) * phi` and
  # call `dirichlet_lpdf` on the result.
  # Mode-2 identification: subtract `mu_unit[1]` from all entries
  # before softmax. softmax is shift-invariant, so this is identical
  # in likelihood to the unshifted form, but it removes the per-site
  # K-shared shift mode by construction. Paired with the hard
  # `sum_to_zero_vector` constraint on Z columns (mode-1), this
  # leaves the K-shared population effects (`b_Intercept`, `b_env`,
  # etc.) sampling from their `student_t(3, 0, 2.5)` default prior
  # with no likelihood contribution, matching brms-native dirichlet's
  # K-1 reference-category parameterisation while keeping `Z` fully
  # K-row symmetric in the column space (Z[1, :] is determined by
  # Z[2..K, :] through the sum-to-zero constraint).
  #
  # Per-unit phi: the Dirichlet concentration applies to the
  # K-vector as a whole, so `phi` must be constant across the K
  # rows of a closure unit. The vector-phi entry takes
  # `phi[idx[1]]` for unit g; the scalar-phi entry broadcasts
  # to the same value. brms emits the vector-phi entry when the
  # user supplies a `phi ~ ...` sub-formula and the scalar-phi
  # entry otherwise.
  paste(
    "  real diri_lpdf(",
    "    vector y,",
    "    vector mu,",
    "    vector phi,",
    "    int N_unit,",
    "    array[] int n_rep,",
    "    array[,] int visit_idx) {",
    "    real lp = 0;",
    "    for (g in 1:N_unit) {",
    "      int Kg = n_rep[g];",
    "      array[Kg] int idx = visit_idx[g, 1:Kg];",
    "      vector[Kg] y_unit  = y[idx];",
    "      vector[Kg] mu_unit = mu[idx] - mu[idx[1]];",
    "      real phi_g = phi[idx[1]];",
    "      lp += dirichlet_lpdf(y_unit | softmax(mu_unit) * phi_g);",
    "    }",
    "    return lp;",
    "  }",
    "",
    "  // Scalar-phi entry point: broadcasts to vector phi for the",
    "  // no-sub-formula case where brms emits a scalar phi dpar.",
    "  real diri_lpdf(",
    "    vector y,",
    "    vector mu,",
    "    real phi,",
    "    int N_unit,",
    "    array[] int n_rep,",
    "    array[,] int visit_idx) {",
    "    int N = num_elements(y);",
    "    return diri_lpdf(y | mu, rep_vector(phi, N), N_unit,",
    "                    n_rep, visit_idx);",
    "  }",
    sep = "\n"
  )
}

#' Build the closure-unit Stan stanvars for an diri() fit
#'
#' Wraps the shared `make_closure_unit_arrays_stanvars()` with the
#' Dirichlet function block. Skips `K_max` and `Y_max` because the
#' per-unit Dirichlet likelihood reads the K-vector of responses
#' directly without truncation.
#'
#' @inheritParams make_closure_unit_arrays_stanvars
#' @return A `brmsstanvars` object.
#' @noRd
make_diri_stanvars <- function(arrays) {
  # Mode-1 Z-column sum-to-zero constraint and mode-2 per-site
  # mu_unit sum-to-zero constraint live in the trend pipeline
  # (generate_factor_model() in R/stan_assembly.R) and the lpdf
  # function block (diri_stan_funs() above), respectively. They are
  # NOT bundled here because mvgam's sort_stanvars() block-reordering
  # would land a family-bundle constraint before Z / N_lv_trend are
  # declared by the trend pipeline.
  make_closure_unit_arrays_stanvars(
    arrays,
    family_funs_name = "diri_funs",
    family_funs      = diri_stan_funs(),
    include_K_max    = FALSE,
    include_Y_max    = FALSE
  )
}

#' Closure-unit Multinomial-on-the-simplex family
#'
#' Composes Stan's native `multinomial_logit_lpmf` with the mvgam
#' factor-model trend. Each closure unit (site) carries K rows
#' (one per species / response component) whose integer counts sum
#' to a per-site total `N_site = sum(Y_unit)`. The custom lpmf
#' assembles the per-unit K-vectors of counts and linear predictor
#' scores inside Stan, then calls Stan's native
#' `multinomial_logit_lpmf(y_unit_int | mu_unit)` which applies
#' `softmax` over `mu_unit` to obtain the K cell probabilities.
#'
#' The multinomial total `N_site` varies freely across sites
#' (each site has its own `sum(Y_unit)` as the implicit total
#' trials), which is desirable for ecological count data and
#' microbiome read-depth differences. No additional `N_site` data
#' field is required because `sum(Y_unit)` is the sufficient
#' statistic for the trial count.
#'
#' Distributional parameters:
#' \describe{
#'   \item{`mu`}{per-row latent score on the unconstrained scale.
#'     Softmax inside the lpmf maps the K scores per unit to a
#'     simplex of cell probabilities. Default link is identity.}
#' }
#'
#' Identification: identical to [diri()]. Each `Z` column is
#' declared as `sum_to_zero_vector[K]` so column sums are exactly
#' zero by construction (Stan >= 2.36), and the lpdf body subtracts
#' `mu_unit[1]` from all entries before softmax to remove the
#' K-shared fixed-effect shift. Both constraints are hard (no soft
#' priors); the K species are kept symmetric in the loadings via
#' the column-sum-zero manifold.
#'
#' Data layout: long format, one row per (site, species), exactly
#' K rows per site. The integer count for that (site, species)
#' cell is the response.
#'
#' @inheritSection diri Post-fit conventions
#'
#' @return A `brms::custom_family` object tagged with the
#'   `mvgam_closure_unit`, `mvgam_multi_response`, and
#'   `mvgam_simplex_response` attributes.
#'
#' @references
#' Warton, D. I., Blanchet, F. G., O'Hara, R. B., Ovaskainen, O.,
#'   Taskinen, S., Walker, S. C. and Hui, F. K. C. (2015). So many
#'   variables: joint modeling in community ecology. *Trends in
#'   Ecology and Evolution*, 30(12):766-779.
#'   \doi{10.1016/j.tree.2015.09.007}
#'
#' @examples
#' \dontrun{
#' # Microbiome-style read-count JSDM. The `env * taxon` interaction
#' # gives each taxon its own intercept and environmental response;
#' # see `?diri` for the rationale on why a per-`species` interaction
#' # is the interpretable form for a simplex multi-response family.
#' mod <- jsdgam(
#'   formula = y ~ env * taxon,
#'   factor_formula = ~ -1,
#'   data = read_counts_long,
#'   unit = site,
#'   species = taxon,
#'   family = multi(),
#'   n_lv = 2
#' )
#' }
#'
#' @export
multi <- function() {
  fam <- brms::custom_family(
    name  = "multi",
    dpars = "mu",
    links = "identity",
    lb    = NA,
    ub    = NA,
    type  = "int",
    loop  = FALSE
  )
  link_info <- stats::make.link("identity")
  fam$linkinv <- link_info$linkinv
  fam$linkfun <- link_info$linkfun
  attr(fam, "mvgam_closure_unit")      <- TRUE
  attr(fam, "mvgam_multi_response")    <- TRUE
  attr(fam, "mvgam_simplex_response")  <- TRUE
  attr(fam, "mvgam_predict_types")     <- character(0L)
  attr(fam, "mvgam_vars") <- c(
    "N_unit", "n_rep", "visit_idx"
  )
  attr(fam, "mvgam_stanvars") <- NULL
  fam
}

#' Stan function block for the closure-unit Multinomial lpmf
#'
#' Assembles the per-unit K-vector of integer counts and linear
#' predictor scores from the long-format `Y` and `mu` arrays via
#' `visit_idx`, then calls Stan's native
#' `multinomial_logit_lpmf(y_unit | mu_unit)` once per closure
#' unit. Softmax over `mu_unit` parameterises the K cell
#' probabilities; the multinomial total `sum(y_unit)` enters the
#' lpmf via the response itself (sufficient statistic).
#'
#' @return A character scalar suitable for
#'   `brms::stanvar(scode = ..., block = "functions")`.
#' @noRd
multi_stan_funs <- function() {
  # See `diri_stan_funs()` for the rationale on the mode-2
  # identification via subtraction of `mu_unit[1]`.
  paste(
    "  real multi_lpmf(",
    "    array[] int y,",
    "    vector mu,",
    "    int N_unit,",
    "    array[] int n_rep,",
    "    array[,] int visit_idx) {",
    "    real lp = 0;",
    "    for (g in 1:N_unit) {",
    "      int Kg = n_rep[g];",
    "      array[Kg] int idx = visit_idx[g, 1:Kg];",
    "      array[Kg] int y_unit  = y[idx];",
    "      vector[Kg] mu_unit = mu[idx] - mu[idx[1]];",
    "      lp += multinomial_logit_lpmf(y_unit | mu_unit);",
    "    }",
    "    return lp;",
    "  }",
    sep = "\n"
  )
}

#' Build the closure-unit Stan stanvars for a multi() fit
#'
#' Wraps `make_closure_unit_arrays_stanvars()` with the multinomial
#' function block. The hard `sum_to_zero_vector[K]` constraint on
#' the columns of `Z` is emitted from the trend pipeline; the
#' per-site K-shared shift is removed inside the lpdf by subtracting
#' `mu_unit[1]`.
#'
#' @inheritParams make_closure_unit_arrays_stanvars
#' @return A `brmsstanvars` object.
#' @noRd
make_multi_stanvars <- function(arrays) {
  # Mode-1 + mode-2 simplex shift constraints live in the trend
  # pipeline and lpdf source helper. See `make_diri_stanvars()`.
  make_closure_unit_arrays_stanvars(
    arrays,
    family_funs_name = "multi_funs",
    family_funs      = multi_stan_funs(),
    include_K_max    = FALSE,
    include_Y_max    = FALSE
  )
}

#' Closure-unit Categorical single-trial family
#'
#' Composes Stan's native `categorical_logit_lpmf` with the mvgam
#' factor-model trend. Each closure unit (site) carries K rows
#' (one per category) whose binary responses encode the observed
#' category in one-hot form: exactly one row per site has `y = 1`
#' (the observed category) and the remaining K - 1 rows have
#' `y = 0`. The custom lpmf finds the observed category code from
#' the one-hot K-vector and calls Stan's
#' `categorical_logit_lpmf(cat_code | mu_unit)` once per unit.
#'
#' Categorical is one-trial multinomial: per-site information
#' content is at most `log2(K)` bits, so recovery of `Z Z'` from
#' single-trial categorical data is intrinsically weaker than
#' multinomial or dirichlet at comparable site counts. Stats
#' review recommends fixtures with at least 100 sites for K = 4
#' categories.
#'
#' Distributional parameters:
#' \describe{
#'   \item{`mu`}{per-row latent score on the unconstrained scale.
#'     Softmax inside the lpmf maps the K scores per unit to a
#'     simplex of cell probabilities. Default link is identity.}
#' }
#'
#' Identification: identical to [diri()] and [multi()]. Each `Z`
#' column is declared as `sum_to_zero_vector[K]` (Stan >= 2.36) and
#' the lpdf subtracts `mu_unit[1]` from all entries before softmax.
#' Both constraints are hard.
#'
#' Data layout: long format, one row per (site, category), exactly
#' K rows per site. The response `y` is binary: `y = 1` on the row
#' corresponding to the observed category for that site and
#' `y = 0` on the K - 1 other rows.
#'
#' @inheritSection diri Post-fit conventions
#'
#' @return A `brms::custom_family` object tagged with the
#'   `mvgam_closure_unit`, `mvgam_multi_response`, and
#'   `mvgam_simplex_response` attributes.
#'
#' @references
#' Warton, D. I., Blanchet, F. G., O'Hara, R. B., Ovaskainen, O.,
#'   Taskinen, S., Walker, S. C. and Hui, F. K. C. (2015). So many
#'   variables: joint modeling in community ecology. *Trends in
#'   Ecology and Evolution*, 30(12):766-779.
#'   \doi{10.1016/j.tree.2015.09.007}
#'
#' @examples
#' \dontrun{
#' # JSDM on single-trial habitat-type observations per site. The
#' # `elev * habitat_class` interaction gives each habitat class its
#' # own elevation response; see `?diri` for the rationale.
#' mod <- jsdgam(
#'   formula = y ~ elev * habitat_class,
#'   factor_formula = ~ -1,
#'   data = habitat_long,
#'   unit = site,
#'   species = habitat_class,
#'   family = categ(),
#'   n_lv = 2
#' )
#' }
#'
#' @export
categ <- function() {
  fam <- brms::custom_family(
    name  = "categ",
    dpars = "mu",
    links = "identity",
    lb    = NA,
    ub    = NA,
    type  = "int",
    loop  = FALSE
  )
  link_info <- stats::make.link("identity")
  fam$linkinv <- link_info$linkinv
  fam$linkfun <- link_info$linkfun
  attr(fam, "mvgam_closure_unit")      <- TRUE
  attr(fam, "mvgam_multi_response")    <- TRUE
  attr(fam, "mvgam_simplex_response")  <- TRUE
  # Binary one-hot response; the per-row y value is 0 or 1. The
  # shared closure-unit data prep treats it as integer counts and
  # leaves a downstream attr-driven data validator to enforce the
  # one-hot invariant.
  attr(fam, "mvgam_binary_response")   <- TRUE
  attr(fam, "mvgam_predict_types")     <- character(0L)
  attr(fam, "mvgam_vars") <- c(
    "N_unit", "n_rep", "visit_idx"
  )
  attr(fam, "mvgam_stanvars") <- NULL
  fam
}

#' Stan function block for the closure-unit Categorical lpmf
#'
#' Finds the observed category code in the one-hot K-vector by
#' scanning `y_unit` for the position of the 1, then calls Stan's
#' native `categorical_logit_lpmf(cat_code | mu_unit)` once per
#' closure unit. The scan is `O(K)` per unit per draw, which is
#' negligible compared with the brms linear-predictor evaluation.
#'
#' Validates the one-hot invariant inside Stan by setting
#' `cat_code = 0` when no `1` is found; Stan's
#' `categorical_logit_lpmf` rejects `cat_code = 0` with a domain
#' error, so a malformed (all-zero) site surfaces as a sampler
#' rejection rather than a silent fit. The R-side data validator
#' (run before Stan compile) catches the malformed case with a
#' clearer error message.
#'
#' @return A character scalar suitable for
#'   `brms::stanvar(scode = ..., block = "functions")`.
#' @noRd
categ_stan_funs <- function() {
  # See `diri_stan_funs()` for the rationale on the mode-2
  # identification via subtraction of `mu_unit[1]`.
  paste(
    "  real categ_lpmf(",
    "    array[] int y,",
    "    vector mu,",
    "    int N_unit,",
    "    array[] int n_rep,",
    "    array[,] int visit_idx) {",
    "    real lp = 0;",
    "    for (g in 1:N_unit) {",
    "      int Kg = n_rep[g];",
    "      array[Kg] int idx = visit_idx[g, 1:Kg];",
    "      array[Kg] int y_unit  = y[idx];",
    "      vector[Kg] mu_unit = mu[idx] - mu[idx[1]];",
    "      int cat_code = 0;",
    "      for (k in 1:Kg) {",
    "        if (y_unit[k] == 1) {",
    "          cat_code = k;",
    "          break;",
    "        }",
    "      }",
    "      lp += categorical_logit_lpmf(cat_code | mu_unit);",
    "    }",
    "    return lp;",
    "  }",
    sep = "\n"
  )
}

#' Build the closure-unit Stan stanvars for a categ() fit
#'
#' Wraps `make_closure_unit_arrays_stanvars()` with the Categorical
#' function block. The hard `sum_to_zero_vector[K]` constraint on
#' the columns of `Z` is emitted from the trend pipeline; the
#' per-site K-shared shift is removed inside the lpdf by subtracting
#' `mu_unit[1]`.
#'
#' @inheritParams make_closure_unit_arrays_stanvars
#' @return A `brmsstanvars` object.
#' @noRd
make_categ_stanvars <- function(arrays) {
  # Mode-1 + mode-2 simplex shift constraints live in the trend
  # pipeline and lpdf source helper. See `make_diri_stanvars()`.
  make_closure_unit_arrays_stanvars(
    arrays,
    family_funs_name = "categ_funs",
    family_funs      = categ_stan_funs(),
    include_K_max    = FALSE,
    include_Y_max    = FALSE
  )
}

#' Closure-unit Multivariate normal family
#'
#' Continuous-response JSDM with low-rank residual covariance
#' `Sigma = Z Z' + diag(Psi^2)`, parameterised conditionally
#' (gllvm / boral style): each row of the linear predictor receives
#' the latent-factor contribution `Z[k, :] * lv[i, :]` from the
#' trend pipeline (with `lv ~ N(0, I_n_lv)` sampled jointly with
#' the model), and the lpdf is then independent normal per row with
#' SD `Psi[k]` for species `k`. Integrating out `lv` recovers the
#' gllvm marginal covariance `Sigma`.
#'
#' Distributional parameters:
#' \describe{
#'   \item{`mu`}{per-row response mean on the identity scale.
#'     Already includes the latent factor contribution from the
#'     trend pipeline. Default link is identity.}
#' }
#'
#' Identification: the marginal `Sigma = Z Z' + diag(Psi^2)` is
#' invariant to `Z -> Z * Q` for orthogonal `Q` (the gllvm / boral
#' rotation invariance); the existing Heaps post-hoc QR rotation
#' fixes `Q` and `Z Z'` is identified. Unlike the simplex families,
#' the multi-normal likelihood is NOT shift-invariant in `mu_unit`,
#' so neither a `sum_to_zero_vector` constraint on `Z` columns nor
#' a `mu_unit[1]` reference subtraction is needed. `Z` is declared
#' as a plain `matrix[K, n_lv]` free parameter by the trend
#' pipeline.
#'
#' Data layout: long format, one row per `(site, species)`, exactly
#' K rows per site. Responses are continuous reals; the `Y` vector
#' is assembled into the per-unit K-vector `y_unit` by the lpdf.
#'
#' @return A `brms::custom_family` object tagged with the
#'   `mvgam_closure_unit` and `mvgam_multi_response` attributes
#'   that route the closure-unit data prep, validation, and Stan
#'   emission. The `mvgam_simplex_response` attribute is NOT set
#'   so the simplex-only identification machinery is skipped.
#'
#' @references
#' Hui, F. K. C., Taskinen, S., Pledger, S., Foster, S. D. and
#'   Warton, D. I. (2015). Model-based approaches to unconstrained
#'   ordination. *Methods in Ecology and Evolution*, 6(4):399-411.
#'   \doi{10.1111/2041-210X.12236}
#'
#' Niku, J., Brooks, W., Herliansyah, R., Hui, F. K. C., Taskinen,
#'   S. and Warton, D. I. (2019). Efficient estimation of
#'   generalized linear latent variable models. *PLOS ONE*,
#'   14(5):e0216129. \doi{10.1371/journal.pone.0216129}
#'
#' Heaps, S. E. and Jermyn, I. H. (2024). Structured prior
#'   distributions for the covariance matrix in latent factor
#'   models. *Statistics and Computing*, 34:143.
#'   \doi{10.1007/s11222-024-10454-0}
#'
#' @examples
#' \dontrun{
#' # gllvm-style continuous JSDM. The `env * species` interaction
#' # gives each species its own intercept and environmental
#' # response; the latent factor model handles residual
#' # species-by-species correlation.
#' mod <- jsdgam(
#'   formula = y ~ env * species,
#'   factor_formula = ~ -1,
#'   data = wide_to_long_data,
#'   unit = site,
#'   species = species,
#'   family = mvn(),
#'   n_lv = 2
#' )
#' plot(residual_cor(mod))
#' }
#'
#' @export
mvn <- function() {
  fam <- brms::custom_family(
    name  = "mvn",
    dpars = "mu",
    links = "identity",
    lb    = NA,
    ub    = NA,
    type  = "real",
    loop  = FALSE
  )
  link_info <- stats::make.link("identity")
  fam$linkinv <- link_info$linkinv
  fam$linkfun <- link_info$linkfun
  attr(fam, "mvgam_closure_unit")   <- TRUE
  attr(fam, "mvgam_multi_response") <- TRUE
  # No `mvgam_simplex_response` attribute: multi_normal_cholesky_lpdf
  # is sensitive to absolute mu levels so the simplex-only
  # identification machinery (sum_to_zero_vector on Z columns,
  # mu_unit[1] reference subtraction) is skipped.
  attr(fam, "mvgam_predict_types") <- character(0L)
  # brms threads N_unit / n_rep / visit_idx into the lpdf via vint,
  # plus the parameter `Psi` (per-species residual SD vector
  # emitted by `make_mvn_stanvars()`). The loadings matrix `Z`
  # enters via `mu` through the trend-pipeline contribution
  # `Z[k, :] * lv[i, :]`, so the lpdf does not need `Z` as a
  # separate argument.
  attr(fam, "mvgam_vars") <- c(
    "N_unit", "n_rep", "visit_idx", "Psi"
  )
  attr(fam, "mvgam_stanvars") <- NULL
  fam
}

#' Stan function block for the closure-unit mv-normal lpdf
#'
#' Uses the conditional gllvm / boral parameterisation: the trend
#' pipeline already adds `Z[k, :] * lv[i, :]` to `mu[i, k]` via the
#' per-row trend computation, with the latent factor scores
#' `lv ~ N(0, I_n_lv)` sampled as part of the model. Integrating out
#' `lv` gives the marginal covariance
#' `Sigma = Z Z' + diag(Psi .* Psi)` on the K-vector of responses at
#' each site, which is the gllvm / boral target. So the per-row
#' conditional density `y_n | mu_n, Psi_{k(n)}` is independent
#' normal, and the lpdf reduces to one `normal_lpdf` call per
#' closure unit (vectorised across the K rows of the unit). No
#' Cholesky is needed at the lpdf level.
#'
#' `Psi` is declared on the SD scale by `make_mvn_stanvars()` so
#' that the residual variance enters as `Psi[k]^2` and the marginal
#' covariance entry `Sigma[k, k]` is `Z[k, :] * Z[k, :]' + Psi[k]^2`.
#' The loadings prior on `Z` and the post-hoc Heaps QR
#' identification stay unchanged from the rest of the factor-model
#' pipeline.
#'
#' @return Character scalar suitable for
#'   `brms::stanvar(scode = ..., block = "functions")`.
#' @noRd
mvn_stan_funs <- function() {
  paste(
    "  real mvn_lpdf(",
    "    vector y,",
    "    vector mu,",
    "    int N_unit,",
    "    array[] int n_rep,",
    "    array[,] int visit_idx,",
    "    vector Psi) {",
    "    real lp = 0;",
    "    for (g in 1:N_unit) {",
    "      int Kg = n_rep[g];",
    "      array[Kg] int idx = visit_idx[g, 1:Kg];",
    "      vector[Kg] y_unit  = y[idx];",
    "      vector[Kg] mu_unit = mu[idx];",
    "      vector[Kg] psi_unit = Psi[1:Kg];",
    "      lp += normal_lpdf(y_unit | mu_unit, psi_unit);",
    "    }",
    "    return lp;",
    "  }",
    sep = "\n"
  )
}

#' Build the closure-unit Stan stanvars for an mvn() fit
#'
#' Wraps `make_closure_unit_arrays_stanvars()` with the mv-normal
#' function block and adds a per-species residual SD parameter
#' `vector<lower=0>[K] Psi` with an `exponential(1)` prior on the
#' SD scale. `K` is the number of response components per closure
#' unit (constant across sites for multi-response families) and is
#' baked in as a literal at fit time so the declaration is
#' self-contained and does not race with the trend pipeline's
#' emission of `N_series_trend`.
#'
#' Prior choice: SD-scale priors are weakly informative across
#' unstandardised response scales, whereas a variance-scale
#' exponential prior would over-suppress residual variance and bias
#' the inferred `Z Z'` covariance upward.
#'
#' `K_max` and `Y_max` are omitted because the mv-normal
#' likelihood does not truncate the per-unit response support.
#'
#' @inheritParams make_closure_unit_arrays_stanvars
#' @return A `brmsstanvars` object.
#' @noRd
make_mvn_stanvars <- function(arrays) {
  # K (number of response components per closure unit) is constant
  # across sites for multi-response families. Bake the literal K
  # into the `Psi` declaration so the parameter declaration is
  # self-contained and does not race with the trend pipeline's
  # emission of `N_series_trend` (which is declared in the data
  # block but lands after the family-bundle stanvars in
  # `sort_stanvars()` ordering, leaving `N_series_trend` out of
  # scope when Stan parses the parameters block).
  K <- as.integer(arrays$max_rep)
  psi_param <- brms::stanvar(
    name  = "mvn_Psi_param",
    scode = paste0("  vector<lower=0>[", K, "] Psi;"),
    block = "parameters"
  )
  psi_prior <- brms::stanvar(
    name  = "mvn_Psi_prior",
    scode = "  Psi ~ exponential(1);",
    block = "model"
  )
  combine_stanvars(
    make_closure_unit_arrays_stanvars(
      arrays,
      family_funs_name = "mvn_funs",
      family_funs      = mvn_stan_funs(),
      include_K_max    = FALSE,
      include_Y_max    = FALSE
    ),
    psi_param,
    psi_prior
  )
}

#' Closure-unit Multivariate Student-t family
#'
#' Heavy-tailed continuous-response JSDM. Extends `mvn()` with a
#' degrees-of-freedom parameter `nu` so the per-observation residual
#' follows a Student-t distribution rather than a Gaussian. Each row
#' of the linear predictor still receives the latent-factor
#' contribution `Z[k, :] * lv[i, :]` from the trend pipeline
#' (conditional gllvm / boral parameterisation), and the lpdf is
#' independent Student-t per row with scale `Psi[k]` for species
#' `k` and shared `nu`.
#'
#' Under this parameterisation the marginal residual covariance is
#' approximately `Z Z' + diag(Psi^2 * nu / (nu - 2))` (Student-t
#' variance scaling), which converges to the mv-normal target
#' `Z Z' + diag(Psi^2)` as `nu -> Inf`. The strict multivariate
#' Student-t marginal would require a per-site scale-mixture
#' latent, which would not compose cleanly with the existing trend
#' pipeline; the conditional Student-t parameterisation is what
#' gllvm uses for its `family = "tweedie"` / `"normal"` heavy-tail
#' option and is the right operational fit for outlier robustness
#' in JSDM use cases.
#'
#' Distributional parameters:
#' \describe{
#'   \item{`mu`}{per-row response mean on the identity scale.
#'     Already includes the latent factor contribution from the
#'     trend pipeline. Default link is identity.}
#' }
#'
#' Identification: identical to `mvn()`. Marginal covariance is
#' invariant to `Z -> Z * Q` for orthogonal `Q`; the Heaps post-hoc
#' QR fixes `Q`. The Student-t observation is NOT shift-invariant in
#' `mu_unit`, so no simplex-style constraints apply. The
#' `nu` parameter is declared with a hard lower bound of 2 so the
#' marginal residual variance stays finite; the prior is
#' `(nu - 2) ~ gamma(2, 0.1)`, giving a weakly informative pull
#' toward moderate heavy tails (prior median around `nu = 14`,
#' prior `Pr(nu < 5) ~ 0.18`).
#'
#' Data layout: long format, one row per `(site, species)`, exactly
#' K rows per site. Responses are continuous reals.
#'
#' @return A `brms::custom_family` object tagged with the
#'   `mvgam_closure_unit` and `mvgam_multi_response` attributes
#'   that route the closure-unit data prep, validation, and Stan
#'   emission. The `mvgam_simplex_response` attribute is NOT set.
#'
#' @references
#' Hui, F. K. C., Taskinen, S., Pledger, S., Foster, S. D. and
#'   Warton, D. I. (2015). Model-based approaches to unconstrained
#'   ordination. *Methods in Ecology and Evolution*, 6(4):399-411.
#'   \doi{10.1111/2041-210X.12236}
#'
#' Heaps, S. E. and Jermyn, I. H. (2024). Structured prior
#'   distributions for the covariance matrix in latent factor
#'   models. *Statistics and Computing*, 34:143.
#'   \doi{10.1007/s11222-024-10454-0}
#'
#' @examples
#' \dontrun{
#' # Heavy-tailed gllvm-style JSDM. Use mvt() over mvn() when
#' # responses contain occasional outliers (rare extreme abundances,
#' # sensor glitches) that under mvn() would be absorbed by widening
#' # `Psi` and the inferred `Z Z'` off-diagonals.
#' mod <- jsdgam(
#'   formula = y ~ env * species,
#'   factor_formula = ~ -1,
#'   data = wide_to_long_data,
#'   unit = site,
#'   species = species,
#'   family = mvt(),
#'   n_lv = 2
#' )
#' plot(residual_cor(mod))
#' }
#'
#' @export
mvt <- function() {
  fam <- brms::custom_family(
    name  = "mvt",
    dpars = "mu",
    links = "identity",
    lb    = NA,
    ub    = NA,
    type  = "real",
    loop  = FALSE
  )
  link_info <- stats::make.link("identity")
  fam$linkinv <- link_info$linkinv
  fam$linkfun <- link_info$linkfun
  attr(fam, "mvgam_closure_unit")   <- TRUE
  attr(fam, "mvgam_multi_response") <- TRUE
  # No `mvgam_simplex_response`: the Student-t observation is not
  # shift-invariant in mu so the sum_to_zero / reference subtraction
  # machinery does not apply.
  attr(fam, "mvgam_predict_types") <- character(0L)
  # brms threads N_unit / n_rep / visit_idx into the lpdf via vint,
  # plus the parameters `Psi` (per-species residual scale) and `nu`
  # (shared degrees of freedom), both emitted by
  # `make_mvt_stanvars()`. `Z` enters via `mu` through the
  # trend-pipeline contribution `Z[k, :] * lv[i, :]`.
  attr(fam, "mvgam_vars") <- c(
    "N_unit", "n_rep", "visit_idx", "Psi", "nu"
  )
  attr(fam, "mvgam_stanvars") <- NULL
  fam
}

#' Stan function block for the closure-unit mv-Student-t lpdf
#'
#' Same conditional gllvm parameterisation as `mvn_stan_funs()`:
#' the per-row latent-factor contribution `Z[k, :] * lv[i, :]` is
#' added to `mu` by the trend pipeline, the lpdf only sees the
#' per-row residual. With `lv ~ N(0, I_n_lv)` and independent
#' per-row Student-t residuals at degrees of freedom `nu` and scale
#' `Psi[k]`, the marginal residual covariance is approximately
#' `Z Z' + diag(Psi^2 * nu / (nu - 2))` (Student-t variance scaling).
#'
#' `nu` is shared across species within a site; the per-species
#' residual scale is carried by `Psi`. Both are declared by
#' `make_mvt_stanvars()`.
#'
#' @return Character scalar suitable for
#'   `brms::stanvar(scode = ..., block = "functions")`.
#' @noRd
mvt_stan_funs <- function() {
  paste(
    "  real mvt_lpdf(",
    "    vector y,",
    "    vector mu,",
    "    int N_unit,",
    "    array[] int n_rep,",
    "    array[,] int visit_idx,",
    "    vector Psi,",
    "    real nu) {",
    "    real lp = 0;",
    "    for (g in 1:N_unit) {",
    "      int Kg = n_rep[g];",
    "      array[Kg] int idx = visit_idx[g, 1:Kg];",
    "      vector[Kg] y_unit  = y[idx];",
    "      vector[Kg] mu_unit = mu[idx];",
    "      vector[Kg] psi_unit = Psi[1:Kg];",
    "      lp += student_t_lpdf(y_unit | nu, mu_unit, psi_unit);",
    "    }",
    "    return lp;",
    "  }",
    sep = "\n"
  )
}

#' Build the closure-unit Stan stanvars for an mvt() fit
#'
#' Wraps `make_closure_unit_arrays_stanvars()` with the mv-Student-t
#' function block and adds two free parameters: a per-species
#' residual scale `vector<lower=0>[K] Psi` (SD-scale exponential(1)
#' prior, identical to `make_mvn_stanvars()`) and a shared degrees
#' of freedom `real<lower=2> nu`. `nu` is hard-floored at 2 so the
#' marginal residual variance stays finite; without that bound,
#' posterior draws of nu in (0, 2) would produce infinite variance
#' and contaminate downstream LOO PSIS weights and predictive
#' summaries. The prior `(nu - 2) ~ gamma(2, 0.1)` is weakly
#' informative (prior median around 14, prior 95% interval roughly
#' \[2.3, 50\]) and matches the recommendation in Juarez & Steel
#' (2010) for Bayesian Student-t regression.
#'
#' `K` is the number of response components per closure unit
#' (constant across sites) and is baked in as a literal at fit time
#' so the `Psi` declaration is self-contained.
#'
#' @inheritParams make_closure_unit_arrays_stanvars
#' @return A `brmsstanvars` object.
#' @noRd
make_mvt_stanvars <- function(arrays) {
  K <- as.integer(arrays$max_rep)
  psi_param <- brms::stanvar(
    name  = "mvt_Psi_param",
    scode = paste0("  vector<lower=0>[", K, "] Psi;"),
    block = "parameters"
  )
  psi_prior <- brms::stanvar(
    name  = "mvt_Psi_prior",
    scode = "  Psi ~ exponential(1);",
    block = "model"
  )
  nu_param <- brms::stanvar(
    name  = "mvt_nu_param",
    scode = "  real<lower=2> nu;",
    block = "parameters"
  )
  nu_prior <- brms::stanvar(
    name  = "mvt_nu_prior",
    scode = "  target += gamma_lpdf(nu - 2 | 2, 0.1);",
    block = "model"
  )
  combine_stanvars(
    make_closure_unit_arrays_stanvars(
      arrays,
      family_funs_name = "mvt_funs",
      family_funs      = mvt_stan_funs(),
      include_K_max    = FALSE,
      include_Y_max    = FALSE
    ),
    psi_param,
    psi_prior,
    nu_param,
    nu_prior
  )
}

#' Stan function block for the closure-unit N-mixture lpmf
#'
#' Implements the Royle (2004) Poisson-binomial marginalisation
#' over the latent abundance N via an analytic ratio recurrence in
#' log space. The marginal sum
#' \deqn{S = \sum_{N=K_{\min}}^{K_{\max}} \text{Poisson}(N\mid\lambda)
#'           \prod_v \text{Binomial}(y_v\mid N, p_v)}
#' factors as \eqn{T_{K_{\min}} \cdot (1 + r_1 + r_1 r_2 + \ldots)}
#' with consecutive-term ratio
#' \deqn{T_N / T_{N-1} = (\lambda / N) \prod_v ((1 - p_v) \cdot N/(N - y_v)).}
#' Pulling \eqn{ff = \lambda \prod_v (1 - p_v)} out front, the
#' nested Horner form evaluates the sum from
#' \eqn{N = K_{\max}} down to \eqn{N = K_{\min} + 1} in
#' \eqn{O(\text{possible\_N} \cdot \text{n\_visits})} cheap operations
#' rather than the naive log-sum-exp's \eqn{O(K_{\max} \cdot \text{n\_visits})}
#' `lgamma` evaluations.
#'
#' Numerical-stability strategy. The accumulation runs in log
#' space (`log_sum_exp(0, log_prob_n + log_ff + log_k_obs - log(N))`)
#' so the recurrence remains finite for any
#' \eqn{(\lambda, p, \text{possible\_N})} within Stan's representable
#' range. The linear-space form used in earlier mvgam releases
#' overflows in the high-detection / large-window corner (e.g.,
#' \eqn{\lambda=50, p=0.95, V=5, \text{possible\_N}=50}); the log-space
#' form is unconditionally stable at the cost of one `log_sum_exp`
#' per iteration. The loop bounds guarantee
#' \eqn{N \geq K_{\min} + 1 > \max(y_v)} so the per-visit
#' denominator \eqn{N - y_v \geq 1} throughout the recurrence,
#' ruling out the only other numerical hazard.
#'
#' The baseline term `poisson_log_lpmf(K_min | log_lam) +
#' binomial_logit_lpmf(counts | K_min, lp_visits)` accounts for
#' \eqn{T_{K_{\min}}} and is added once per closure unit; the
#' recurrence supplies the additional `log_prob_n` contribution.
#'
#' @param max_rep Positive integer maximum visit count across
#'   closure units. Sets the column count of `visit_idx`. The
#'   Stan loop reads only the first `n_rep[g]` columns per unit.
#' @return Character scalar of Stan function code.
#' @noRd
nmix_stan_funs <- function(max_rep) {
  checkmate::assert_integerish(max_rep, lower = 1L, len = 1L)
  paste(
    "  // Per-visit implementation. brms passes `mu` and `p` as",
    "  // vectors when either dpar carries a sub-formula (e.g.",
    "  // bf(y ~ s(elev), p ~ s(tod))). The overloaded scalar",
    "  // signature below broadcasts the static-dpar case via",
    "  // rep_vector and delegates to this entry point.",
    "  // Per-closure-unit partial sum body. reduce_sum (called",
    "  // from the nmix_lpmf wrapper) hands this function a slice",
    "  // [start:end] of the closure-unit index and gets back the",
    "  // partial log-likelihood for that chunk. When the model is",
    "  // compiled with stan_threads (auto-set by `threads = N`",
    "  // on mvgam()), TBB splits the slice across threads;",
    "  // otherwise reduce_sum runs serially. Link-scale conversions",
    "  // live on the wrapper so per-thread chunks see precomputed",
    "  // log_mu / logit_p / log1m_p vectors instead of redoing the",
    "  // dpar transform per chunk.",
    "  real partial_sum_nmix_lpmf(",
    "    array[] int g_slice,",
    "    int start, int end,",
    "    array[] int y,",
    "    vector log_mu,",
    "    vector logit_p,",
    "    vector log1m_p,",
    "    array[] int n_rep,",
    "    array[] int K_max,",
    "    array[] int Y_max,",
    "    array[,] int visit_idx,",
    "    vector log_n_lookup) {",
    "    real lp = 0;",
    "    for (g in start : end) {",
    "      int K_max_g    = K_max[g];",
    "      int K_min_g    = Y_max[g];",
    "      int possible_N = K_max_g - K_min_g;",
    "      array[n_rep[g]] int idx = visit_idx[g, 1:n_rep[g]];",
    "      real log_lam              = log_mu[idx[1]];",
    "      array[n_rep[g]] int counts = y[idx];",
    "      vector[n_rep[g]] lp_visits = logit_p[idx];",
    "      // Analytic marginalisation via the ratio recurrence",
    "      //   T_N / T_{N-1} = (ff / N) * prod_v (N / (N - y_v))",
    "      // with ff = lambda * prod_v (1 - p_v). The log-space",
    "      // Horner form below evaluates",
    "      //   log(1 + r_1 + r_1*r_2 + ... + r_1*...*r_M)",
    "      // backward from N = K_max down to N = K_min + 1. See",
    "      // ?nmix_stan_funs (R/families.R) for the stability",
    "      // rationale; log_sum_exp inside the loop keeps prob_n",
    "      // finite for any realistic (lambda, p, possible_N).",
    "      // log(N) and log(N - counts[j]) values come from the",
    "      // transformed-data `log_n_lookup` to skip per-iter scalar",
    "      // log() calls in the inner ratio loop. Loop bounds keep",
    "      // all looked-up indices in [1, K_max_global].",
    "      real log_ff     = log_lam + sum(log1m_p[idx]);",
    "      real log_prob_n = 0; // log(1)",
    "      for (i in 1 : possible_N) {",
    "        int  N         = K_max_g - i + 1;",
    "        real log_N     = log_n_lookup[N];",
    "        real log_k_obs = 0;",
    "        // Loop bounds guarantee N >= K_min_g + 1 > max(counts)",
    "        // so N - counts[j] >= 1; no division-by-zero possible.",
    "        for (j in 1 : n_rep[g]) {",
    "          log_k_obs += log_N - log_n_lookup[N - counts[j]];",
    "        }",
    "        log_prob_n = log_sum_exp(0,",
    "          log_prob_n + log_ff + log_k_obs - log_N);",
    "      }",
    "      lp += poisson_log_lpmf(K_min_g | log_lam)",
    "          + binomial_logit_lpmf(counts | K_min_g, lp_visits)",
    "          + log_prob_n;",
    "    }",
    "    return lp;",
    "  }",
    "",
    "  real nmix_lpmf(",
    "    array[] int y,",
    "    vector mu,",
    "    vector p,",
    "    int N_unit,",
    "    array[] int n_rep,",
    "    array[] int K_max,",
    "    array[] int Y_max,",
    "    array[,] int visit_idx,",
    "    vector log_n_lookup) {",
    "    // Link-scale conversions hoisted to the wrapper so the",
    "    // per-thread partial sum sees them precomputed.",
    "    vector[num_elements(mu)] log_mu  = log(mu);",
    "    vector[num_elements(p)]  logit_p = logit(p);",
    "    vector[num_elements(p)]  log1m_p = log1m(p);",
    "    // Closure-unit index sliced by reduce_sum. Constructed",
    "    // locally so no extra data array is threaded through brms.",
    "    array[N_unit] int g_seq;",
    "    for (g in 1 : N_unit) g_seq[g] = g;",
    "    // grainsize heuristic targets ~8 chunks. With no",
    "    // threading: 8 serial dispatches per leapfrog (vs N_unit",
    "    // at grainsize = 1). With threads = 1..8: TBB maps the 8",
    "    // chunks across cores. With more than 8 threads the",
    "    // chunks bound parallelism but per-chunk work amortises",
    "    // the synchronisation overhead. Users can override at",
    "    // fit time by passing a custom stanvar.",
    "    int grainsize = N_unit >= 8 ? N_unit / 8 : 1;",
    "    return reduce_sum(",
    "      partial_sum_nmix_lpmf, g_seq, grainsize,",
    "      y, log_mu, logit_p, log1m_p, n_rep,",
    "      K_max, Y_max, visit_idx, log_n_lookup",
    "    );",
    "  }",
    "",
    "  // Scalar-p entry point: broadcasts to the per-visit",
    "  // vector and dispatches to the vector implementation.",
    "  real nmix_lpmf(",
    "    array[] int y,",
    "    vector mu,",
    "    real p,",
    "    int N_unit,",
    "    array[] int n_rep,",
    "    array[] int K_max,",
    "    array[] int Y_max,",
    "    array[,] int visit_idx,",
    "    vector log_n_lookup) {",
    "    int N = num_elements(mu);",
    "    return nmix_lpmf(y | mu, rep_vector(p, N), N_unit,",
    "                     n_rep, K_max, Y_max, visit_idx,",
    "                     log_n_lookup);",
    "  }",
    sep = "\n"
  )
}

#' Stan function block for the Royle-Nichols nmix variant
#'
#' Marginalises latent abundance `N ~ Poisson(lambda)` over a
#' per-unit upper truncation `K_max[g]`; per-visit binary outcomes
#' have probability `1 - (1 - r)^N` where `r` is per-individual
#' detection (logit link). Per-visit varying `r` is supported via
#' the dpar sub-formula on `p` (matching how the Poisson-binomial
#' lpmf handles per-visit `p`).
#'
#' Avoids two artifacts present in the Bollen reference Stan code:
#' the stray `+ 1` literal on a log-probability in the
#' all-zero-history branch, and any `1e-9` regularisation around
#' `log(0)` (with the logit link on `r` and the loop range
#' `Y_max[g] : K_max[g]`, the `k = 0` cell is unreachable whenever
#' the unit has any detection).
#'
#' @param max_rep Positive integer maximum visit count across
#'   closure units; only used by the scalar-p overload's broadcast.
#' @return Character scalar of Stan function code.
#' @noRd
nmix_royle_nichols_stan_funs <- function(max_rep) {
  checkmate::assert_integerish(max_rep, lower = 1L, len = 1L)
  paste(
    "  // Per-closure-unit partial sum body. reduce_sum (called from",
    "  // the nmix_royle_nichols_lpmf wrapper) hands this function a",
    "  // slice [start:end] of the closure-unit index. When the model",
    "  // is compiled with stan_threads, TBB splits the slice across",
    "  // threads. log_mu and log_1m_r are precomputed on the wrapper.",
    "  real partial_sum_nmix_royle_nichols_lpmf(",
    "    array[] int g_slice,",
    "    int start, int end,",
    "    array[] int y,",
    "    vector log_mu,",
    "    vector log_1m_r,",
    "    array[] int n_rep,",
    "    array[] int K_max,",
    "    array[] int Y_max,",
    "    array[,] int visit_idx) {",
    "    real lp = 0;",
    "    for (g in start : end) {",
    "      int Kg = K_max[g];",
    "      int cmax = Y_max[g];",
    "      array[n_rep[g]] int idx = visit_idx[g, 1:n_rep[g]];",
    "      real log_lam = log_mu[idx[1]];",
    "      array[n_rep[g]] int counts = y[idx];",
    "      vector[n_rep[g]] log_1m_r_v = log_1m_r[idx];",
    "      if (cmax == 0) {",
    "        // Haines (2016) closed form for the all-zero detection",
    "        // history. sum_{N=0}^infty Poisson(N|lambda) *",
    "        // prod_t (1-r_t)^N collapses via the Poisson MGF to",
    "        // exp(lambda * (z - 1)), z = prod_t (1-r_t). Exact,",
    "        // scalar, no log_sum_exp, no K_max truncation bias.",
    "        // The single-subset case (|S| = 0) has no alternating",
    "        // signs so this is numerically clean -- the dangerous",
    "        // cancellation regimes flagged in stats review only",
    "        // appear for n_1 > 0 (Y_max >= 1) full Haines.",
    "        real log_z = sum(log_1m_r_v);",
    "        lp += exp(log_lam) * (exp(log_z) - 1.0);",
    "      } else {",
    "        vector[n_rep[g]] counts_v   = to_vector(counts);",
    "        // Pre-aggregate the constant-in-k non-detection sum so",
    "        // the inner loop is O(1) in n_rep.",
    "        real sum_non_det_log_1m_r = dot_product(",
    "          1.0 - counts_v, log_1m_r_v",
    "        );",
    "        vector[Kg + 1] component_lps;",
    "        // Closure: k < cmax is impossible because the unit",
    "        // observed at least cmax detection events.",
    "        for (k in 0 : (cmax - 1)) {",
    "          component_lps[k + 1] = negative_infinity();",
    "        }",
    "        for (k in cmax : Kg) {",
    "          real lp_k = poisson_log_lpmf(k | log_lam)",
    "            + k * sum_non_det_log_1m_r;",
    "          if (k > 0) {",
    "            // sum_t counts[t] * log(1 - (1 - r_t)^k)",
    "            // log1m_exp(k * log_1m_r_v) computes log(1 - (1-r)^k)",
    "            // elementwise; at k = 0 it returns -inf, which is",
    "            // why this branch is guarded.",
    "            lp_k += dot_product(",
    "              counts_v, log1m_exp(k * log_1m_r_v)",
    "            );",
    "          }",
    "          component_lps[k + 1] = lp_k;",
    "        }",
    "        lp += log_sum_exp(component_lps);",
    "      }",
    "    }",
    "    return lp;",
    "  }",
    "",
    "  // Per-visit implementation. Per-individual detection r is",
    "  // logit-linked; brms passes p (= r) as a probability vector.",
    "  // log(1 - r) is the per-individual non-detection log-prob",
    "  // used in the marginalisation; log1m(p) computes it stably.",
    "  real nmix_royle_nichols_lpmf(",
    "    array[] int y,",
    "    vector mu,",
    "    vector p,",
    "    int N_unit,",
    "    array[] int n_rep,",
    "    array[] int K_max,",
    "    array[] int Y_max,",
    "    array[,] int visit_idx) {",
    "    // Link-scale conversions hoisted to the wrapper so the",
    "    // per-thread partial sum sees them precomputed.",
    "    vector[num_elements(mu)] log_mu   = log(mu);",
    "    vector[num_elements(p)]  log_1m_r = log1m(p);",
    "    array[N_unit] int g_seq;",
    "    for (g in 1 : N_unit) g_seq[g] = g;",
    "    int grainsize = N_unit >= 8 ? N_unit / 8 : 1;",
    "    return reduce_sum(",
    "      partial_sum_nmix_royle_nichols_lpmf, g_seq, grainsize,",
    "      y, log_mu, log_1m_r, n_rep, K_max, Y_max, visit_idx",
    "    );",
    "  }",
    "",
    "  // Scalar-p entry point: broadcasts to the per-visit",
    "  // vector and dispatches to the vector implementation.",
    "  real nmix_royle_nichols_lpmf(",
    "    array[] int y,",
    "    vector mu,",
    "    real p,",
    "    int N_unit,",
    "    array[] int n_rep,",
    "    array[] int K_max,",
    "    array[] int Y_max,",
    "    array[,] int visit_idx) {",
    "    int N = num_elements(mu);",
    "    return nmix_royle_nichols_lpmf(",
    "      y | mu, rep_vector(p, N), N_unit,",
    "      n_rep, K_max, Y_max, visit_idx",
    "    );",
    "  }",
    sep = "\n"
  )
}

#' Stan function block for the Poisson-Poisson nmix variant
#'
#' Per-unit log-likelihood marginalises latent abundance
#' `N ~ Poisson(lambda)` over `0 : K_max[g]`; conditional on
#' `N = k`, per-visit encounter counts are
#' `y_t ~ Poisson(k * p_t)` with per-individual encounter rate
#' `p` (log link). `Y_max[g]` does NOT bound `N` from below for
#' Poisson-Poisson (`y_t` can exceed `N` because each individual
#' contributes its own Poisson process per visit). The `k = 0`
#' cell is only consistent with all-zero detection histories;
#' the guard handles this without resorting to Bollen's `1e-9`
#' regularisation, which is unnecessary under the log link.
#'
#' Efficient factored form. Stan's `poisson_log_lpmf` decomposes
#' `sum_t poisson_log_lpmf(y_t | log(k) + log(p_t))` as
#' `log(k) * sum(y) + sum(y * log(p)) - k * sum(p) - sum(lgamma(y + 1))`.
#' The three terms `sum(y)`, `sum(y * log(p))`, `sum(p)`, and
#' `sum(lgamma(y + 1))` are constant in `k`, so the inner
#' marginalisation loop runs in `O(K_max[g])` total per unit
#' instead of `O(K_max[g] * n_rep[g])`. At realistic
#' `K_max ~ 50, n_rep ~ 5` this is a 5x speed-up; at
#' `K_max ~ 200, n_rep ~ 10` it is a 10x speed-up.
#'
#' @param max_rep Positive integer maximum visit count across
#'   closure units; only used by the scalar-p overload's broadcast.
#' @return Character scalar of Stan function code.
#' @noRd
nmix_poisson_poisson_stan_funs <- function(max_rep) {
  checkmate::assert_integerish(max_rep, lower = 1L, len = 1L)
  paste(
    "  // Per-closure-unit partial sum body. reduce_sum (called from",
    "  // the nmix_poisson_poisson_lpmf wrapper) hands this function a",
    "  // slice [start:end] of the closure-unit index. When the model",
    "  // is compiled with stan_threads, TBB splits the slice across",
    "  // threads. log_mu and log_p are precomputed on the wrapper.",
    "  // log_n_lookup[k] = log(k) replaces the per-iter scalar log()",
    "  // call inside the inner k loop. k_start_ppm[g] is the data-",
    "  // precomputed lower bound on the latent-N loop; cells below",
    "  // it carry negligible Poisson-tail mass (see make_*_stanvars).",
    "  real partial_sum_nmix_poisson_poisson_lpmf(",
    "    array[] int g_slice,",
    "    int start, int end,",
    "    array[] int y,",
    "    vector log_mu,",
    "    vector log_p,",
    "    vector p,",
    "    array[] int n_rep,",
    "    array[] int K_max,",
    "    array[] int Y_max,",
    "    array[,] int visit_idx,",
    "    vector log_n_lookup,",
    "    array[] int k_start_ppm) {",
    "    real lp = 0;",
    "    for (g in start : end) {",
    "      int Kg = K_max[g];",
    "      int kg_lo = k_start_ppm[g];",
    "      array[n_rep[g]] int idx = visit_idx[g, 1:n_rep[g]];",
    "      real log_lam = log_mu[idx[1]];",
    "      array[n_rep[g]] int counts = y[idx];",
    "      vector[n_rep[g]] log_p_v   = log_p[idx];",
    "      vector[n_rep[g]] p_v       = p[idx];",
    "      vector[n_rep[g]] counts_v  = to_vector(counts);",
    "      int any_detection = Y_max[g] > 0;",
    "      // O(n_rep) precomputes; reused for every k in kg_lo..Kg.",
    "      real sum_counts   = sum(counts_v);",
    "      real sum_y_log_p  = dot_product(counts_v, log_p_v);",
    "      real sum_p_v      = sum(p_v);",
    "      real lgamma_const = sum(lgamma(counts_v + 1));",
    "      int n_cells = Kg - kg_lo + 2;",
    "      vector[n_cells] component_lps;",
    "      // k = 0 only consistent with all-zero counts. If any",
    "      // visit detected anything, the k = 0 cell is impossible;",
    "      // otherwise it contributes poisson_log_lpmf(0|log_lam)",
    "      // plus 0 (Poisson(y=0|rate=0) = 1).",
    "      if (any_detection) {",
    "        component_lps[1] = negative_infinity();",
    "      } else {",
    "        component_lps[1] = poisson_log_lpmf(0 | log_lam);",
    "      }",
    "      for (k in kg_lo : Kg) {",
    "        // Factored sum_t poisson_log_lpmf(y_t|log(k)+log_p_v[t])",
    "        // = log(k) * sum_y + sum_y_log_p",
    "        //   - k * sum_p_v - lgamma_const.",
    "        // log(k) read from transformed-data log_n_lookup; the",
    "        // tdata vector is sized to K_max_global so k <= Kg fits.",
    "        component_lps[k - kg_lo + 2] = poisson_log_lpmf(k | log_lam)",
    "          + log_n_lookup[k] * sum_counts + sum_y_log_p",
    "          - k * sum_p_v - lgamma_const;",
    "      }",
    "      lp += log_sum_exp(component_lps);",
    "    }",
    "    return lp;",
    "  }",
    "",
    "  // Per-visit implementation. p is a positive encounter rate",
    "  // per individual per visit (log link). log_p is the linear-",
    "  // predictor scale; log_mu is the abundance log-rate.",
    "  real nmix_poisson_poisson_lpmf(",
    "    array[] int y,",
    "    vector mu,",
    "    vector p,",
    "    int N_unit,",
    "    array[] int n_rep,",
    "    array[] int K_max,",
    "    array[] int Y_max,",
    "    array[,] int visit_idx,",
    "    vector log_n_lookup,",
    "    array[] int k_start_ppm) {",
    "    // Link-scale conversions hoisted to the wrapper so the",
    "    // per-thread partial sum sees them precomputed.",
    "    vector[num_elements(mu)] log_mu = log(mu);",
    "    vector[num_elements(p)]  log_p  = log(p);",
    "    array[N_unit] int g_seq;",
    "    for (g in 1 : N_unit) g_seq[g] = g;",
    "    int grainsize = N_unit >= 8 ? N_unit / 8 : 1;",
    "    return reduce_sum(",
    "      partial_sum_nmix_poisson_poisson_lpmf, g_seq, grainsize,",
    "      y, log_mu, log_p, p, n_rep, K_max, Y_max, visit_idx,",
    "      log_n_lookup, k_start_ppm",
    "    );",
    "  }",
    "",
    "  // Scalar-p entry point: broadcasts to the per-visit",
    "  // vector and dispatches to the vector implementation.",
    "  real nmix_poisson_poisson_lpmf(",
    "    array[] int y,",
    "    vector mu,",
    "    real p,",
    "    int N_unit,",
    "    array[] int n_rep,",
    "    array[] int K_max,",
    "    array[] int Y_max,",
    "    array[,] int visit_idx,",
    "    vector log_n_lookup,",
    "    array[] int k_start_ppm) {",
    "    int N = num_elements(mu);",
    "    return nmix_poisson_poisson_lpmf(",
    "      y | mu, rep_vector(p, N), N_unit,",
    "      n_rep, K_max, Y_max, visit_idx, log_n_lookup, k_start_ppm",
    "    );",
    "  }",
    sep = "\n"
  )
}

#' Assemble the shared closure-unit Stan stanvar bundle
#'
#' Every closure-unit family emits the same `N_unit`, `n_rep`,
#' `Y_max`, `visit_idx` integer arrays in the Stan data block,
#' plus a family-specific function block. nmix() additionally
#' carries `K_max` (per-unit latent abundance upper truncation);
#' binary-response families (`occ()`) omit it because the latent
#' state is binary.
#'
#' This helper centralises the shared declarations so per-family
#' wrappers only differ in (i) the function block, (ii) the
#' Y_max upper bound (1 for binary y, unbounded for counts), and
#' (iii) whether K_max is declared.
#'
#' @param arrays Named list returned by
#'   `build_closure_unit_arrays()`.
#' @param family_funs_name Stanvar name for the function block
#'   (e.g. `"nmix_funs"`, `"occ_funs"`).
#' @param family_funs Character scalar of Stan function-block
#'   code (typically the output of `*_stan_funs(arrays$max_rep)`).
#' @param y_max_upper Upper bound on the per-unit `Y_max`
#'   declaration. Defaults to `NA` (unbounded). Set to `1L` for
#'   binary-response families to mirror the response support.
#' @param include_K_max Logical; emit the `K_max` data array
#'   when `TRUE`. Defaults to `TRUE` for count families.
#' @return A `brmsstanvars` object.
#' @noRd
make_closure_unit_arrays_stanvars <- function(arrays,
                                               family_funs_name,
                                               family_funs,
                                               y_max_upper   = NA_integer_,
                                               include_K_max = TRUE,
                                               include_Y_max = TRUE) {
  checkmate::assert_list(arrays, names = "named")
  checkmate::assert_string(family_funs_name)
  checkmate::assert_string(family_funs)
  checkmate::assert_integerish(y_max_upper, len = 1L, lower = 0L,
                               null.ok = FALSE)
  checkmate::assert_flag(include_K_max)
  checkmate::assert_flag(include_Y_max)
  required <- c("N_unit", "n_rep", "visit_idx", "max_rep")
  if (include_Y_max) required <- c(required, "Y_max")
  if (include_K_max) required <- c(required, "K_max")
  missing_fields <- setdiff(required, names(arrays))
  if (length(missing_fields) > 0L) {
    stop(insight::format_error(
      paste0(
        "Closure-unit arrays are missing fields: ",
        paste(missing_fields, collapse = ", "), "."
      )
    ))
  }
  stanvars <- brms::stanvar(
    name  = family_funs_name,
    scode = family_funs,
    block = "functions"
  ) +
    brms::stanvar(
      x     = as.integer(arrays$N_unit),
      name  = "N_unit",
      scode = "int<lower=1> N_unit;",
      block = "data"
    ) +
    brms::stanvar(
      x     = as.integer(arrays$n_rep),
      name  = "n_rep",
      scode = "array[N_unit] int<lower=1> n_rep;",
      block = "data"
    ) +
    brms::stanvar(
      x     = arrays$visit_idx,
      name  = "visit_idx",
      scode = paste0(
        "array[N_unit, ", arrays$max_rep,
        "] int<lower=1> visit_idx;"
      ),
      block = "data"
    )
  if (include_Y_max) {
    y_max_scode <- if (is.na(y_max_upper)) {
      "array[N_unit] int<lower=0> Y_max;"
    } else {
      paste0(
        "array[N_unit] int<lower=0, upper=", y_max_upper,
        "> Y_max;"
      )
    }
    stanvars <- stanvars +
      brms::stanvar(
        x     = as.integer(arrays$Y_max),
        name  = "Y_max",
        scode = y_max_scode,
        block = "data"
      )
  }
  if (include_K_max) {
    stanvars <- stanvars +
      brms::stanvar(
        x     = as.integer(arrays$K_max),
        name  = "K_max",
        scode = "array[N_unit] int<lower=1> K_max;",
        block = "data"
      )
  }
  stanvars
}

#' Build the closure-unit Stan stanvars for an nmix() fit
#'
#' Wraps the shared `make_closure_unit_arrays_stanvars()` with
#' the nmix-specific function block and the `K_max` data array.
#'
#' @inheritParams make_closure_unit_arrays_stanvars
#' @return A `brmsstanvars` object.
#' @noRd
make_nmix_stanvars <- function(arrays) {
  base <- make_closure_unit_arrays_stanvars(
    arrays,
    family_funs_name = "nmix_funs",
    family_funs      = nmix_stan_funs(arrays$max_rep),
    y_max_upper      = NA_integer_,
    include_K_max    = TRUE
  )
  # Precompute log(N) for N in 1..max(K_max) in transformed data
  # so the Poisson-binomial marginalisation's inner ratio loop can
  # index a vector instead of calling scalar log() per iteration.
  # Saves ~possible_N * (1 + n_rep) scalar log evaluations per
  # closure unit per leapfrog step. Indexed by integer N (Stan
  # requires int for vector indexing) at the lpmf call site.
  base + brms::stanvar(
    scode = paste(
      "  int K_max_global = max(K_max);",
      "  vector[K_max_global] log_n_lookup;",
      "  for (n_lk in 1 : K_max_global) {",
      "    log_n_lookup[n_lk] = log(n_lk);",
      "  }",
      sep = "\n"
    ),
    block = "tdata"
  )
}

#' Build the closure-unit Stan stanvars for an nmix("royle_nichols")
#' fit
#'
#' Wraps the shared `make_closure_unit_arrays_stanvars()` with the
#' Royle-Nichols function block, the per-unit `K_max` data array,
#' and the binary `Y_max` upper bound (Royle-Nichols requires
#' binary detection input, so `Y_max[g] in {0, 1}`).
#'
#' @inheritParams make_closure_unit_arrays_stanvars
#' @return A `brmsstanvars` object.
#' @noRd
make_nmix_royle_nichols_stanvars <- function(arrays) {
  make_closure_unit_arrays_stanvars(
    arrays,
    family_funs_name = "nmix_royle_nichols_funs",
    family_funs      = nmix_royle_nichols_stan_funs(arrays$max_rep),
    y_max_upper      = 1L,
    include_K_max    = TRUE
  )
}

#' Build the closure-unit Stan stanvars for an
#' `nmix("poisson_poisson")` fit
#'
#' Wraps the shared `make_closure_unit_arrays_stanvars()` with
#' the Neyman Type A function block and the per-unit `K_max`
#' data array. Y_max keeps the unbounded count declaration
#' because Poisson-Poisson allows `y_t > N` (each individual
#' contributes its own Poisson process per visit; the closure
#' constraint does NOT bound `N` from below by `Y_max`).
#'
#' @inheritParams make_closure_unit_arrays_stanvars
#' @return A `brmsstanvars` object.
#' @noRd
make_nmix_poisson_poisson_stanvars <- function(arrays) {
  base <- make_closure_unit_arrays_stanvars(
    arrays,
    family_funs_name = "nmix_poisson_poisson_funs",
    family_funs      = nmix_poisson_poisson_stan_funs(arrays$max_rep),
    y_max_upper      = NA_integer_,
    include_K_max    = TRUE
  )
  # Precompute log(k) for k in 1..max(K_max) in transformed data
  # so the Poisson-Poisson marginalisation's inner k loop can index
  # a vector instead of calling scalar log() per iteration. Saves
  # max(K_max) scalar log evaluations per closure unit per leapfrog
  # step. Indexed by integer k at the lpmf call site; loop bounds
  # k in kg_lo..Kg guarantee k <= K_max_global.
  base <- base + brms::stanvar(
    scode = paste(
      "  int K_max_global = max(K_max);",
      "  vector[K_max_global] log_n_lookup;",
      "  for (n_lk in 1 : K_max_global) {",
      "    log_n_lookup[n_lk] = log(n_lk);",
      "  }",
      sep = "\n"
    ),
    block = "tdata"
  )
  # Per-unit lower bound on the latent-N k-loop. Drops cells more
  # than ~7 conservative SDs below Y_max[g] (max observed count).
  # Reason: conditional on N = k, per-visit counts are Poisson(k*p)
  # with worst-case p ~= 1 giving SD ~ sqrt(Y_max) around the MLE
  # N_hat ~= Y_max/p. The log-likelihood at m SDs below the mode
  # drops by m^2/2; m = 7 gives < 1e-11 relative mass, below
  # double-precision noise once aggregated via log_sum_exp. Pure
  # efficiency: posterior is identical up to round-off. Low-count
  # units collapse to k_start = 1 (no skip).
  y_max <- as.integer(arrays$Y_max)
  k_start_ppm <- pmax(1L, y_max - 7L * as.integer(ceiling(sqrt(y_max))))
  base + brms::stanvar(
    x     = k_start_ppm,
    name  = "k_start_ppm",
    scode = "array[N_unit] int<lower=1> k_start_ppm;",
    block = "data"
  )
}

#' Prepare a closure-unit family for fitting
#'
#' Resolves the data-dependent parts of a closure-unit family
#' at fit time: validates the observation data, builds the
#' closure-unit arrays, assembles the family-specific Stan
#' stanvars, and sets `family$vars` so brms threads the right
#' data variables through to the lpdf call.
#'
#' Called from `R/make_stan.R` immediately before
#' `attach_family_stanvars()`. The returned family carries the
#' filled-in stanvars in `attr(family, "mvgam_stanvars")` and
#' is ready to flow through the existing brms pipeline.
#'
#' @param family A closure-unit family (e.g. [nmix()]).
#' @param data User observation data.
#' @param response_var Name of the response (count) column.
#' @param has_obs_covariates Logical; TRUE if the abundance /
#'   state formula contains at least one covariate.
#' @param has_det_covariates Logical; TRUE if a detection
#'   sub-formula was supplied.
#' @return The family with `mvgam_stanvars` attribute populated
#'   and `vars` set.
#' @noRd
prepare_closure_unit_family <- function(family, data, response_var,
                                         has_obs_covariates = FALSE,
                                         has_det_covariates = FALSE) {
  family_name <- family$name
  binary_y_check <- isTRUE(attr(family, "mvgam_binary_response",
                                 exact = TRUE))
  default_cap <- closure_unit_default_cap(family)
  default_cap_buffer <- closure_unit_default_cap_buffer(family)
  multi_response <- is_multi_response_family(family)
  if (multi_response) {
    # Multi-response families (diri / multinomial /
    # categorical / mvnormal / mvt) skip the integer-y / cap
    # validation: their per-unit likelihoods read the K response
    # components directly, without per-unit truncation. The closure
    # unit groups by `time` (site) only -- the K species rows at
    # each site form the per-unit contributions, in contrast to the
    # count-based families where each (species, site) pair is its
    # own closure unit with replicate visits. The "time" default is
    # overridden when the family carries an `mvgam_unit_grouping`
    # attribute (e.g. a future spatial multi-response variant could
    # opt in to (site, time) without further plumbing here).
    arrays <- build_closure_unit_arrays(
      data, response_var = response_var,
      compute_y_max = FALSE,
      unit_grouping_vars = closure_unit_grouping(family) %||% "time"
    )
  } else {
    # cap is required only when neither a scalar default nor a
    # data-driven buffer is configured. Count families (PB / PPM)
    # carry `mvgam_default_cap_buffer = 100L` so the validator
    # accepts data without an explicit cap column. Multi-season
    # families carry an `mvgam_unit_grouping` attr that the
    # accessor returns; otherwise the validator + builder default
    # to (series, time).
    unit_grouping_vars <- closure_unit_grouping(family)
    validate_closure_unit_data(
      data,
      response_var       = response_var,
      has_obs_covariates = has_obs_covariates,
      has_det_covariates = has_det_covariates,
      binary_y_check     = binary_y_check,
      cap_required       = is.null(default_cap) &&
                            is.null(default_cap_buffer),
      unit_grouping_vars = unit_grouping_vars
    )
    arrays <- build_closure_unit_arrays(
      data, response_var = response_var,
      default_cap        = default_cap,
      default_cap_buffer = default_cap_buffer,
      unit_grouping_vars = unit_grouping_vars
    )
  }
  family_stanvars <- switch(
    family_name,
    nmix                 = make_nmix_stanvars(arrays),
    nmix_royle_nichols   = make_nmix_royle_nichols_stanvars(arrays),
    nmix_poisson_poisson = make_nmix_poisson_poisson_stanvars(arrays),
    occ                  = make_occ_stanvars(arrays),
    diri                 = make_diri_stanvars(arrays),
    multi                = make_multi_stanvars(arrays),
    categ                = make_categ_stanvars(arrays),
    mvn                  = make_mvn_stanvars(arrays),
    mvt                  = make_mvt_stanvars(arrays),
    stop(insight::format_error(c(
      paste0(
        "Closure-unit dispatch missing for family '",
        family_name, "'."
      ),
      i = paste0(
        "Add a '", family_name, " = make_",
        family_name,
        "_stanvars(arrays)' branch to the switch() in ",
        "prepare_closure_unit_family()."
      )
    )))
  )
  attr(family, "mvgam_stanvars") <- family_stanvars
  family_vars <- attr(family, "mvgam_vars", exact = TRUE)
  if (is.null(family_vars)) {
    stop(insight::format_error(c(
      paste0(
        "Closure-unit family '", family_name,
        "' is missing the 'mvgam_vars' attribute."
      ),
      i = paste0(
        "Set `attr(fam, \"mvgam_vars\")` in the constructor to ",
        "the integer arrays brms must thread through to the lpmf."
      )
    )))
  }
  family$vars <- family_vars
  # Poisson-Poisson identifiability guard. With intercept-only mu
  # AND intercept-only p, the closed-form marginal would be a
  # Poisson(lambda * p) ridge; the Neyman Type A over-dispersion
  # in the truncated form gives only weak cross-identification, and
  # the lambda * p = constant ridge remains pronounced (Kery 2018;
  # "Wild posteriors in the wild" arXiv 2503.00239). Warn once per
  # session so the user has the option to constrain at least one
  # intercept with an informative prior.
  if (identical(family_name, "nmix_poisson_poisson") &&
      !has_obs_covariates && !has_det_covariates) {
    if (!identical(Sys.getenv("TESTTHAT"), "true")) {
      rlang::warn(
        insight::format_warning(c(
          "nmix(\"poisson_poisson\") with intercept-only mu and p is weakly identified.",
          x = paste0(
            "Only the product `lambda * p` is identified by the ",
            "Neyman Type A marginal; individual posteriors on ",
            "`lambda` and `p` are dominated by their priors."
          ),
          i = paste0(
            "Add a covariate to either formula (`y ~ x` or ",
            "`bf(y ~ ..., p ~ x)`), or supply an informative prior ",
            "on at least one intercept via the `prior` argument."
          )
        )),
        .frequency    = "once",
        .frequency_id = "nmix_poisson_poisson_intercept_only"
      )
    }
  }
  # Stash the arrays on the family so downstream code (predict /
  # posterior_predict / log_lik) can reuse the same closure-unit
  # grouping without re-deriving it from data.
  attr(family, "mvgam_closure_unit_arrays") <- arrays
  family
}

#' Merge a custom family's mvgam_stanvars into the user's stanvars
#'
#' Custom families built via [tweedie()] attach their function-
#' block helpers + any data inputs in
#' `attr(family, "mvgam_stanvars")`. This helper concatenates
#' them with any stanvars the user passed via `mvgam(stanvars =
#' ...)` so the brms call downstream sees a single combined
#' object. Built-in brms families have no attached stanvars and
#' pass through unchanged.
#'
#' @param stanvars Existing stanvars object or NULL.
#' @param family A `brmsfamily` / `customfamily` object.
#' @return A `brmsstanvars` object (or NULL when neither side
#'   contributes anything).
#' @noRd
attach_family_stanvars <- function(stanvars, family) {
  fam_stanvars <- attr(family, "mvgam_stanvars", exact = TRUE)
  if (is.null(fam_stanvars)) return(stanvars)
  checkmate::assert_class(fam_stanvars, "stanvars")
  if (is.null(stanvars)) return(fam_stanvars)
  checkmate::assert_class(stanvars, "stanvars")
  stanvars + fam_stanvars
}

#' R-side log-density evaluator for the Tweedie family
#'
#' Conforms to the `log_lik_<family>` signature expected by
#' `dispatch_log_lik()` (see `R/log_lik.mvgam.R`). Returns a
#' `[ndraws x nobs]` matrix of log densities. Uses
#' `mgcv::ldTweedie()` which returns log densities directly,
#' avoiding the `log(pmax(density, eps))` floor pattern that
#' would silently bias LOO / WAIC scores upward for
#' observations where the raw density underflows to zero.
#'
#' @noRd
log_lik_tweedie <- function(linpred, link, y, family_pars, trials) {
  checkmate::assert_matrix(linpred)
  # mgcv::ldTweedie requires positive `mu`, so the link must
  # have produced positive values. tweedie() restricts the
  # surface to the log link.
  checkmate::assert_choice(link, "log")
  mu <- .linkinv(linpred, link)
  mphi <- family_pars$mphi
  mtheta <- family_pars$mtheta
  checkmate::assert_matrix(mphi, nrows = nrow(linpred),
                           ncols = ncol(linpred))
  checkmate::assert_matrix(mtheta, nrows = nrow(linpred),
                           ncols = ncol(linpred))
  ndraws <- nrow(linpred)
  nobs <- ncol(linpred)
  out <- matrix(NA_real_, nrow = ndraws, ncol = nobs)
  for (j in seq_len(nobs)) {
    # ldTweedie vectorises across `y`, not across `mu` / `p` /
    # `phi`: a scalar y with vector mu only evaluates one row.
    # Broadcast y to length ndraws so the call returns one log
    # density per posterior draw.
    ld <- mgcv::ldTweedie(
      y = rep(y[j], ndraws),
      mu = mu[, j],
      p = mtheta[, j],
      phi = mphi[, j]
    )
    out[, j] <- ld[, 1L]
  }
  out
}

# ============================================================
# Closure-unit family R-side downstream methods
# ============================================================
# All closure-unit families (nmix, occ, ...) share the
# `extract_closure_unit_components()` extractor which yields
# `$state` (per-unit, link-inverted), `$p` (per-visit), and the
# closure-unit arrays. Per-family methods then apply the
# family-specific likelihood / sampling formula. `predict.mvgam`,
# `posterior_epred.mvgam`, `posterior_predict.mvgam` and
# `log_lik.mvgam` route to per-family methods via the central
# `dispatch_closure_unit_method()` switch defined below; adding
# a new family means adding one branch per `method_kind` arm.

#' Dispatch a closure-unit R-side method by family name
#'
#' Central routing table for the closure-unit families. All
#' downstream surfaces (predict, posterior_epred,
#' posterior_predict, posterior_detection, posterior_latent_N /
#' posterior_occupancy) call this helper rather than hard-wiring
#' a family name. Adding a new closure-unit family means adding
#' one branch per `method_kind` arm.
#'
#' @param family A fitted mvgam family (must be a closure-unit
#'   family per `is_closure_unit_family()`).
#' @param method_kind One of `"epred"`, `"predict"`, `"log_lik"`,
#'   `"latent_state"`. The `"latent_state"` arm is family-specific
#'   (latent_N for nmix, occupancy for occ).
#' @return A function with the per-family signature for that
#'   method. Calls `stop()` for unknown families or methods so
#'   the missing branch is reported with the family name in the
#'   error message.
#' @noRd
dispatch_closure_unit_method <- function(family, method_kind) {
  checkmate::assert_choice(
    method_kind, c("epred", "predict", "log_lik", "latent_state")
  )
  if (!is_closure_unit_family(family)) {
    stop(insight::format_error(
      "dispatch_closure_unit_method() requires a closure-unit family."
    ))
  }
  family_name <- resolve_family_name(family)
  fn <- switch(
    family_name,
    nmix = switch(method_kind,
                  epred        = posterior_epred_nmix,
                  predict      = posterior_predict_nmix,
                  log_lik      = log_lik_nmix,
                  latent_state = posterior_latent_N_pb),
    nmix_royle_nichols = switch(
      method_kind,
      epred        = posterior_epred_nmix_royle_nichols,
      predict      = posterior_predict_nmix_royle_nichols,
      log_lik      = log_lik_nmix_royle_nichols,
      latent_state = posterior_latent_N_royle_nichols
    ),
    nmix_poisson_poisson = switch(
      method_kind,
      epred        = posterior_epred_nmix_poisson_poisson,
      predict      = posterior_predict_nmix_poisson_poisson,
      log_lik      = log_lik_nmix_poisson_poisson,
      latent_state = posterior_latent_N_poisson_poisson
    ),
    occ  = switch(method_kind,
                  epred        = posterior_epred_occ,
                  predict      = posterior_predict_occ,
                  log_lik      = log_lik_occ,
                  latent_state = posterior_occupancy),
    mvn  = switch(method_kind,
                  epred        = posterior_epred_mvn,
                  predict      = posterior_predict_mvn,
                  log_lik      = log_lik_mvn,
                  latent_state = NULL),
    mvt  = switch(method_kind,
                  epred        = posterior_epred_mvt,
                  predict      = posterior_predict_mvt,
                  log_lik      = log_lik_mvt,
                  latent_state = NULL),
    diri  = switch(method_kind,
                   epred        = posterior_epred_diri,
                   predict      = posterior_predict_diri,
                   log_lik      = log_lik_diri,
                   latent_state = NULL),
    multi = switch(method_kind,
                   epred        = posterior_epred_multi,
                   predict      = posterior_predict_multi,
                   log_lik      = log_lik_multi,
                   latent_state = NULL),
    categ = switch(method_kind,
                   epred        = posterior_epred_categ,
                   predict      = posterior_predict_categ,
                   log_lik      = log_lik_categ,
                   latent_state = NULL)
  )
  if (is.null(fn)) {
    stop(insight::format_error(c(
      paste0(
        "Closure-unit dispatch missing for family '",
        family_name, "' method '", method_kind, "'."
      ),
      i = paste0(
        "Add a '", family_name, " = switch(method_kind, ...)' ",
        "branch to dispatch_closure_unit_method() in R/families.R."
      )
    )))
  }
  fn
}

#' Materialise `ndraws` to a concrete `draw_ids` vector for the
#' closure-unit kernels.
#'
#' `posterior_epred.mvgam()` and `posterior_predict.mvgam()` accept
#' both `ndraws` and `draw_ids`, but the closure-unit kernels
#' (`posterior_epred_nmix`, `posterior_predict_mvn`, ...) accept
#' only `draw_ids`. Materialising `ndraws` here means a single
#' random subsample is shared by `mu`, dpars (`Psi`, `nu`), and
#' any downstream extraction the kernel performs, instead of each
#' call drawing a different subset.
#'
#' Convention matches `posterior::resample_draws()`: when both
#' `ndraws` and `draw_ids` are supplied the explicit `draw_ids`
#' wins. When neither is supplied the kernel sees `draw_ids =
#' NULL` and uses the full posterior.
#'
#' @param object Fitted `mvgam` object.
#' @param ndraws Integer or NULL.
#' @param draw_ids Integer vector or NULL.
#' @return `draw_ids` vector or NULL.
#' @noRd
closure_unit_resolve_draw_ids <- function(object, ndraws, draw_ids) {
  if (!is.null(draw_ids) || is.null(ndraws)) return(draw_ids)
  total <- posterior::ndraws(posterior::as_draws_matrix(object$fit))
  if (ndraws >= total) return(NULL)
  sort(sample.int(total, ndraws))
}

#' Resolve the response variable name from an mvgam formula slot
#'
#' Handles both the plain `formula` and the `brmsformula` /
#' `mvgam_formula` cases. Used by every closure-unit family's
#' R-side extractor.
#' @noRd
closure_unit_response_var <- function(form) {
  raw <- if (inherits(form, "brmsformula")) {
    form$formula[[2L]]
  } else if (inherits(form, "formula")) {
    form[[2L]]
  } else {
    stop(insight::format_error(
      "Could not resolve response variable from object$formula."
    ))
  }
  vars <- all.vars(raw)
  if (length(vars) != 1L) {
    stop(insight::format_error(c(
      "Closure-unit families require a single response column.",
      x = paste0(
        "Found ", length(vars),
        " variables in the LHS of the observation formula."
      ),
      i = "cbind() responses are not supported for closure-unit families."
    )))
  }
  vars[1L]
}

#' Extract detection-probability draws for an nmix() fit
#'
#' Handles both the scalar-`p` case (no detection sub-formula,
#' brms declares `real<lower=0,upper=1> p;` in the parameters
#' block and the posterior carries `p` directly) and the
#' vector-`p` case (sub-formula such as `bf(y ~ x, p ~ s(tod))`,
#' brms emits `b_p_Intercept` + `b_p_*` and computes `p` as a
#' transient `vector[N]` in the model block).
#'
#' For the vector case the per-row p is rebuilt from the fitted
#' coefficients + the design matrix on `newdata` via the shared
#' `extract_component_linpred(component = "p")` path, which
#' funnels through `prepare_predictions.mock_stanfit()` and so
#' picks up parametric terms, smooths, random effects, GP
#' terms etc. transparently.
#'
#' @param object Fitted mvgam object with a closure-unit family.
#' @param newdata Long-format observation data on which to
#'   predict.
#' @param draw_ids Optional posterior draw indices.
#' @param n_visit Integer; number of visit rows in `newdata`.
#' @param ndraws Integer; number of posterior draws after any
#'   `draw_ids` subsetting.
#' @return `[ndraws x n_visit]` matrix of detection probabilities
#'   in (0, 1).
#' @noRd
extract_p_for_closure_unit <- function(object, newdata, draw_ids,
                                       n_visit, ndraws) {
  draws_mat <- posterior::as_draws_matrix(object$fit)
  all_cols  <- colnames(draws_mat)
  # Any column emitted by a detection sub-formula triggers the
  # vector-p path; the rebuild reuses mvgam's existing dpar-
  # linpred composer via name-stripping so parametric, smooth,
  # random-effect and GP terms in `p ~ ...` all flow through.
  has_any_p_subformula <- any(grepl(
    paste0(
      "^Intercept_p$|^b_p_|^bs_p($|\\[)|^s_p_|^sds_p_|",
      "^zs_p_|^r_.+_p_|^gp_p_|^sdgp_p_|^lscale_p_"
    ),
    all_cols
  ))
  if (has_any_p_subformula) {
    return(extract_p_via_dpar_linpred(object, newdata, draw_ids))
  }
  # Scalar `p` parameter in the posterior; broadcast to per-visit
  # length for downstream indexing.
  if (!"p" %in% all_cols) {
    stop(insight::format_error(
      "Detection probability draws 'p' not found in posterior."
    ))
  }
  if (is.null(draw_ids)) {
    draw_ids <- seq_len(ndraws)
  } else if (length(draw_ids) != ndraws) {
    draw_ids <- draw_ids[seq_len(ndraws)]
  }
  p_scalar <- as.numeric(draws_mat[draw_ids, "p"])
  matrix(p_scalar, nrow = ndraws, ncol = n_visit, byrow = FALSE)
}

#' Vector-p draws via mvgam's shared dpar-linpred composer
#'
#' Delegates to `extract_component_linpred()` with
#' `component = "p"`, the same machinery used for the obs and
#' trend components. The dpar branch strip-renames the `_p`
#' infix on draws and standata, then funnels the result through
#' `extract_linpred_univariate()` which already composes
#' parametric + smooth + RE + GP contributions across draws.
#' Reusing the shared composer means a `bf(y ~ ..., p ~ ...)`
#' detection sub-formula inherits every predictor type brms
#' supports without bespoke code.
#'
#' @return `[ndraws x n_visit]` matrix of probabilities (link
#'   inverse already applied).
#' @noRd
extract_p_via_dpar_linpred <- function(object, newdata, draw_ids) {
  linpred_p <- extract_component_linpred(
    mvgam_fit = object,
    newdata   = newdata,
    component = "p",
    draw_ids  = draw_ids
  )
  stats::plogis(linpred_p)
}

#' Extract state, detection-probability and closure-unit arrays
#'
#' Single-pass extractor used by every closure-unit family's
#' R-side method (`log_lik_*`, `posterior_epred_*`,
#' `posterior_predict_*`, `posterior_latent_N` for nmix,
#' `posterior_occupancy` for occ, `posterior_detection`).
#' Returns the per-unit state quantity at its native grain
#' (one column per closure unit, not per visit), the per-visit
#' detection probability p, and the closure-unit arrays rebuilt
#' from `newdata` so that user edits take effect at prediction
#' time.
#'
#' The state quantity is family-specific: lambda (abundance
#' rate, log link) for nmix, psi (occupancy probability, logit
#' link) for occ, both inverted from the same linpred path via
#' the family's `linkinv`. Downstream methods access
#' `comp$state` and apply the family-specific likelihood.
#'
#' Detection sub-formulas (`bf(y ~ ..., p ~ tod)`) are
#' supported via `extract_p_via_dpar_linpred()`, which rebuilds
#' the per-visit probability matrix from the posterior
#' coefficients + the standata design matrices on `newdata`.
#' Parametric, smooth, random-effect, and GP terms in
#' `p ~ ...` all flow through.
#'
#' Predict-time guard: re-runs `validate_closure_unit_data()` to
#' catch the case where `newdata` carries `cap < max(y)` per
#' unit (nmix) or non-binary y (occ). The cap guard prevents
#' silent garbage from `sample()` in `posterior_latent_N`.
#'
#' @param object Fitted `mvgam` object with a closure-unit family.
#' @param newdata Long-format observation data; defaults to the
#'   training data stored on `object`.
#' @param draw_ids Optional vector of posterior draw indices.
#' @return Named list with `state` (`[S x N_unit]`), `p`
#'   (`[S x N_visit]`), `arrays`, `ndraws`, `n_unit`, `n_visit`.
#' @noRd
extract_closure_unit_components <- function(object, newdata = NULL,
                                             draw_ids = NULL) {
  checkmate::assert_class(object, "mvgam")
  if (!is_closure_unit_family(object$family)) {
    stop(insight::format_error(c(
      "extract_closure_unit_components() requires a closure-unit fit.",
      i = "Use family = nmix() or family = occ()."
    )))
  }
  if (is.null(newdata)) {
    newdata <- object$data
    if (is.null(newdata)) {
      stop(insight::format_error(
        "Training data not stored on object; supply 'newdata'."
      ))
    }
  }
  response_var <- closure_unit_response_var(object$formula)
  binary_y_check <- isTRUE(attr(object$family, "mvgam_binary_response",
                                  exact = TRUE))
  default_cap <- closure_unit_default_cap(object$family)
  # Re-run validation on newdata so cap edits (nmix) or non-binary
  # y (occ, royle_nichols) raise the friendly error rather than
  # producing silent garbage in downstream sampling. Identifiability
  # flags are TRUE at predict time because we do not re-examine the
  # formula here; those warnings are only informative at fit time.
  # Mv-response families (mvn / mvt) read continuous responses
  # without a per-unit truncation; they share the closure-unit
  # data layout (long format, K rows per site) but neither
  # require a cap column nor run the binary-y check.
  aggregates <- needs_closure_unit_aggregation(object$family)
  validate_closure_unit_data(
    newdata,
    response_var       = response_var,
    has_obs_covariates = TRUE,
    has_det_covariates = TRUE,
    binary_y_check     = binary_y_check && aggregates,
    cap_required       = is.null(default_cap) && aggregates
  )
  arrays <- build_closure_unit_arrays(
    newdata, response_var = response_var,
    default_cap = default_cap
  )
  # Per-visit linpred for the state quantity. The linpred is
  # constant within a closure unit because the formula is on
  # site-level covariates; drop the redundant columns to one per
  # unit and carry the unit-grain state to all downstream methods.
  # Per-visit broadcasting happens only at the response sampling
  # step, where it is unavoidable.
  #
  # process_error = FALSE because the current closure-unit
  # families have no stochastic trend layer (trend_type = "None").
  # When trend support lands this flag flips to the caller's
  # request.
  linpred <- posterior_linpred(
    object, newdata = newdata, draw_ids = draw_ids,
    process_error = FALSE
  )
  state_visit <- object$family$linkinv(linpred)
  ndraws   <- nrow(state_visit)
  n_visit  <- ncol(state_visit)
  n_unit   <- arrays$N_unit
  first_visit_idx <- arrays$visit_idx[, 1L]
  state <- state_visit[, first_visit_idx, drop = FALSE]
  # p extraction. brms emits the scalar `p` in the posterior when
  # there is no detection sub-formula; with a sub-formula
  # (`bf(y ~ x, p ~ tod)`) `p` is transient in the model block
  # and the posterior carries `b_p_Intercept` + `b_p_*` instead.
  # Both cases route through `extract_p_for_closure_unit()` which
  # reuses mvgam's existing dpar-prediction machinery so smooths,
  # group-level effects, GP terms etc. all flow through brms.
  p <- extract_p_for_closure_unit(object, newdata, draw_ids,
                                  n_visit, ndraws)
  list(
    state   = state,
    p       = p,
    arrays  = arrays,
    ndraws  = ndraws,
    n_unit  = n_unit,
    n_visit = n_visit
  )
}

#' Per-visit expected count for an nmix() fit
#'
#' Returns the per-visit expectation `lambda_g * p_{g, j}`. No
#' Jensen correction is needed because both the Poisson and the
#' Binomial are linear in their mean parameters at the per-draw
#' level.
#'
#' @param object Fitted `mvgam` object with a closure-unit family.
#' @param newdata Long-format observation data; defaults to the
#'   training data.
#' @param draw_ids Optional vector of posterior draw indices.
#' @return `[S x N_visit]` matrix of expected counts.
#' @noRd
posterior_epred_nmix <- function(object, newdata = NULL,
                                  draw_ids = NULL) {
  comp <- extract_closure_unit_components(object, newdata, draw_ids)
  # Broadcast unit-grain lambda back to per-visit length via the
  # visit-to-unit lookup encoded in `arrays$visit_idx`. The
  # first-visit column is shared by every visit of a unit, so the
  # inverse mapping is straightforward.
  unit_of_visit <- visit_to_unit_lookup(comp$arrays, comp$n_visit)
  comp$state[, unit_of_visit, drop = FALSE] * comp$p
}

#' Inverse of arrays$visit_idx: for each visit row, the unit g
#' that contains it. Used to broadcast unit-grain quantities
#' (lambda, latent N) back to the visit grain without copying
#' the lambda matrix.
#' @noRd
visit_to_unit_lookup <- function(arrays, n_visit) {
  out <- integer(n_visit)
  for (g in seq_len(arrays$N_unit)) {
    idx <- arrays$visit_idx[g, seq_len(arrays$n_rep[g])]
    out[idx] <- g
  }
  out
}

#' Per-visit response draws for an nmix() fit (unconditional)
#'
#' Draws latent abundances unconditionally from the Poisson prior
#' (`N_g ~ Poisson(lambda_g)`) per posterior draw, then samples
#' each visit's count from `Binomial(N_g, p_{g,j})`. This matches
#' the Stan likelihood's marginalisation semantics: the model
#' integrates N out, and the unconditional prior predictive is
#' the natural ppc target. Use `predict(object, type =
#' "latent_state")` for the conditional posterior of N given the
#' observed counts (Royle 2004).
#'
#' @inheritParams posterior_epred_nmix
#' @return `[S x N_visit]` integer matrix of visit counts.
#' @noRd
posterior_predict_nmix <- function(object, newdata = NULL,
                                    draw_ids = NULL) {
  comp <- extract_closure_unit_components(object, newdata, draw_ids)
  arrays <- comp$arrays
  ndraws <- comp$ndraws
  out <- matrix(0L, nrow = ndraws, ncol = comp$n_visit)
  for (g in seq_len(arrays$N_unit)) {
    idx <- arrays$visit_idx[g, seq_len(arrays$n_rep[g])]
    lam_g <- comp$state[, g]
    N_draws <- stats::rpois(ndraws, lambda = lam_g)
    for (j in idx) {
      out[, j] <- stats::rbinom(ndraws, size = N_draws,
                                prob = comp$p[, j])
    }
  }
  out
}

#' Per-visit detection-probability draws for a closure-unit fit
#'
#' Wraps `extract_closure_unit_components()` for
#' `predict(type = "detection")`. Returns a `[S x N_visit]`
#' matrix of detection probabilities on the response (0-1)
#' scale. Family-agnostic; works for any closure-unit family
#' (nmix, occ, future royle_nichols / poisson_poisson).
#'
#' @inheritParams posterior_epred_nmix
#' @return `[S x N_visit]` matrix.
#' @noRd
posterior_detection <- function(object, newdata = NULL,
                                 draw_ids = NULL) {
  extract_closure_unit_components(object, newdata, draw_ids)$p
}

#' Aggregate per-visit observations and posterior predictive draws
#' to the closure-unit grain
#'
#' Closure-unit families (nmix, occ) treat the unit (site x season)
#' as the conditionally iid block. Per-visit observations within a
#' unit share the latent state (latent N for nmix, latent z for
#' occ), so per-visit residuals or PIT statistics inherit a
#' within-unit correlation that distorts standard diagnostics.
#' Aggregating the response and the per-visit posterior predictive
#' draws to the unit grain using a sufficient summary restores
#' exchangeability across units, which is the correct grain for
#' empirical-PIT residuals and pp_check density / interval plots.
#'
#' The aggregating function is `sum`: for nmix the per-visit
#' counts add to a per-unit total (the marginal sufficient
#' statistic for the Poisson-binomial likelihood when the
#' detection probability is constant across visits within a
#' unit); for occ the per-visit 0/1 detections add to the
#' detection count, which carries the full within-unit
#' detection-frequency information. `sum` is the sufficient
#' statistic for the per-unit Bernoulli-binomial marginal under
#' fixed `z_g` only when `p_{g, j}` is constant across visits;
#' with visit-level detection covariates the detection
#' probabilities differ across visits and the per-unit sum is
#' a Poisson-binomial summary that loses some discriminating
#' power relative to a full detection-history score (Kery and
#' Royle 2016, ch. 10.2). It still gives a valid PIT comparison
#' because the per-visit posterior predictive uses the same
#' per-visit `p`, so the marginal distribution of the aggregated
#' replicate matches the data-generating process. `sum` is
#' chosen over `max` because it preserves detection frequency
#' rather than collapsing to presence / absence.
#'
#' @param object Fitted `mvgam` object with a closure-unit family.
#' @param newdata Long-format observation data; defaults to the
#'   training data on `object`.
#' @param yrep_visit `[ndraws x N_visit]` integer matrix of
#'   per-visit posterior predictive draws (as returned by
#'   `posterior_predict(object, newdata)` on a closure-unit
#'   family). The column order must match `newdata` row order.
#' @return Named list with elements:
#'   * `y_unit` -- length-`N_unit` numeric vector of aggregated
#'     observed values, indexed in `arrays$unit_labels` order.
#'   * `yrep_unit` -- `[ndraws x N_unit]` numeric matrix of
#'     aggregated posterior predictive draws (one column per
#'     closure unit, columns named by `arrays$unit_labels`).
#'   * `arrays` -- the closure-unit array list produced by
#'     `build_closure_unit_arrays()` (carries `N_unit`, `n_rep`,
#'     `visit_idx`, `Y_max`, `unit_labels`).
#' @noRd
aggregate_closure_unit_visits <- function(object,
                                           newdata,
                                           yrep_visit) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_matrix(yrep_visit)
  if (is.null(newdata)) newdata <- object$data
  response_var <- closure_unit_response_var(object$formula)
  # closure_unit_default_cap() returns 1L for occ() (binary latent
  # state), NULL otherwise. Threading it through to the array
  # builder mirrors prepare_closure_unit_family() so users do not
  # need to carry a `cap` column through to newdata for occ() fits.
  default_cap <- closure_unit_default_cap(object$family)
  arrays <- build_closure_unit_arrays(
    newdata, response_var = response_var,
    default_cap = default_cap
  )
  if (ncol(yrep_visit) != nrow(newdata)) {
    stop(insight::format_error(c(
      "Posterior predictive matrix column count does not match 'newdata'.",
      x = paste0(
        "ncol(yrep_visit) = ", ncol(yrep_visit),
        ", nrow(newdata) = ", nrow(newdata), "."
      )
    )))
  }
  y_visit <- as.numeric(newdata[[response_var]])
  N_unit <- arrays$N_unit
  ndraws <- nrow(yrep_visit)
  y_unit <- numeric(N_unit)
  yrep_unit <- matrix(0, nrow = ndraws, ncol = N_unit)
  for (g in seq_len(N_unit)) {
    idx <- arrays$visit_idx[g, seq_len(arrays$n_rep[g])]
    y_unit[g] <- sum(y_visit[idx])
    # Single-visit units short-circuit the apply call. Multi-visit
    # units sum across the chosen visit columns; rowSums is the
    # vectorised form and is materially faster than apply on the
    # wide N_visit grain.
    yrep_unit[, g] <- if (length(idx) == 1L) {
      yrep_visit[, idx]
    } else {
      rowSums(yrep_visit[, idx, drop = FALSE])
    }
  }
  colnames(yrep_unit) <- arrays$unit_labels
  names(y_unit) <- arrays$unit_labels
  list(y_unit = y_unit, yrep_unit = yrep_unit, arrays = arrays)
}

#' Per-closure-unit latent-abundance draws for an nmix() fit
#'
#' Thin dispatcher: routes to the per-variant kernel
#' (`posterior_latent_N_pb` for `nmix("poisson_binomial")`,
#' `posterior_latent_N_royle_nichols` for `nmix("royle_nichols")`)
#' via the central `dispatch_closure_unit_method()` table.
#' Public-facing call site (used by `predict(type = "latent_state")`
#' for nmix() families).
#'
#' @param object Fitted `mvgam` object with an nmix() family.
#' @param newdata Long-format observation data; defaults to
#'   training data.
#' @param draw_ids Optional vector of posterior draw indices.
#' @param conditional Logical. If TRUE (default), reweight the
#'   discrete N support by the per-visit likelihood at the
#'   observed counts. If FALSE, sample N from the unconditional
#'   Poisson prior.
#' @return `[S x N_unit]` integer matrix of latent abundance
#'   draws.
#' @noRd
posterior_latent_N <- function(object, newdata = NULL,
                                draw_ids = NULL,
                                conditional = TRUE) {
  if (!is_closure_unit_family(object$family)) {
    stop(insight::format_error(
      "posterior_latent_N() requires a closure-unit family."
    ))
  }
  kernel <- dispatch_closure_unit_method(object$family, "latent_state")
  kernel(
    object, newdata = newdata,
    draw_ids = draw_ids, conditional = conditional
  )
}


#' Per-unit posterior saturation of the closure-unit K_max truncation
#'
#' For a fit using a closure-unit family with a per-unit upper
#' truncation `K_max` (`nmix()`, `nmix("royle_nichols")`,
#' `nmix("poisson_poisson")`), reports the share of conditional
#' posterior `N` draws that sit at the truncation point. Units
#' whose posterior abundance hits `K_max` carry truncation-induced
#' downward bias and are a signal that the `cap` column should be
#' raised (or supplied at all, since `nmix("royle_nichols")`
#' defaults to `K_max = 25`).
#'
#' @param object A fitted `mvgam` with a closure-unit family.
#' @param newdata Optional `data.frame`. When `NULL` uses the fit's
#'   training data.
#' @param threshold Numeric in `[0, 1]`. Units with
#'   `mean(N_draws == K_max) > threshold` are flagged in the
#'   returned `saturated` column. Default `0.05`.
#' @param ndraws,draw_ids Forwarded to the internal latent-N
#'   sampler. `ndraws` caps the per-unit posterior; `draw_ids`
#'   selects a draw subset.
#'
#' @return A `data.frame` with one row per closure unit and columns
#'   \describe{
#'     \item{`unit`}{Integer index `1:N_unit`.}
#'     \item{`label`}{Optional character label if the fit was built
#'       from a long-format frame with unique unit identifiers.}
#'     \item{`K_max`}{The per-unit upper truncation that bounded
#'       the marginalisation.}
#'     \item{`p_saturated`}{Posterior probability that latent `N`
#'       equals `K_max`.}
#'     \item{`saturated`}{Logical, `p_saturated > threshold`.}
#'   }
#'   A 0-row `saturated` set means the configured `K_max` was
#'   sufficient for all units in the sampled posterior.
#'
#' @seealso [nmix()].
#'
#' @export
latent_N_saturation <- function(object, newdata = NULL,
                                 threshold = 0.05,
                                 ndraws = NULL,
                                 draw_ids = NULL) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_number(threshold, lower = 0, upper = 1)
  if (!is_closure_unit_family(object$family)) {
    stop(insight::format_error(
      "latent_N_saturation() requires a closure-unit family."
    ))
  }
  # K_max comes from the per-unit array assembled at fit / predict
  # time; for multi-response families (mvn, mvt, diri, multi, categ)
  # K_max is NA and the diagnostic does not apply.
  comp <- extract_closure_unit_components(
    object, newdata = newdata, draw_ids = draw_ids
  )
  K_max <- comp$arrays$K_max
  if (is.null(K_max) || all(is.na(K_max))) {
    stop(insight::format_error(c(
      "K_max is not defined for this family.",
      i = paste0(
        "latent_N_saturation() applies only to nmix() and its ",
        "royle_nichols / poisson_poisson variants."
      )
    )))
  }
  draws <- posterior_latent_N(
    object, newdata = newdata,
    draw_ids = draw_ids, conditional = TRUE
  )
  if (!is.null(ndraws)) {
    checkmate::assert_int(ndraws, lower = 1L)
    n <- min(nrow(draws), as.integer(ndraws))
    draws <- draws[seq_len(n), , drop = FALSE]
  }
  p_sat <- vapply(
    seq_len(ncol(draws)),
    function(g) mean(draws[, g] >= K_max[g]),
    numeric(1L)
  )
  unit_labels <- comp$arrays$unit_labels %||%
    as.character(seq_along(K_max))
  out <- data.frame(
    unit        = seq_along(K_max),
    label       = unit_labels,
    K_max       = as.integer(K_max),
    p_saturated = p_sat,
    saturated   = p_sat > threshold,
    stringsAsFactors = FALSE
  )
  attr(out, "threshold") <- threshold
  out
}

#' Per-closure-unit latent-abundance draws for an
#' `nmix("poisson_binomial")` fit
#'
#' Conditional posterior of `N` given the observed visit counts
#' (Royle 2004):
#' \deqn{P(N_g = k | y_g, lambda_g, p_g) \propto
#'   Poisson(k | lambda_g) \times \prod_j Binomial(y_{g,j} | k, p_{g,j})}
#' for `k = max(y[g, ])..K_max[g]`. Weights are accumulated in
#' log space and normalised via `log_sum_exp` before
#' exponentiation, so numerical underflow at large `K_max` is
#' avoided.
#'
#' When `conditional = FALSE` (no observed y available, e.g. a
#' fresh prediction grid), N is sampled directly from the prior
#' `Poisson(lambda_g)` per draw.
#'
#' @inheritParams posterior_latent_N
#' @return `[S x N_unit]` integer matrix of latent abundance
#'   draws.
#' @noRd
posterior_latent_N_pb <- function(object, newdata = NULL,
                                   draw_ids = NULL,
                                   conditional = TRUE) {
  checkmate::assert_flag(conditional)
  comp <- extract_closure_unit_components(object, newdata, draw_ids)
  arrays <- comp$arrays
  ndraws <- comp$ndraws
  N_unit <- arrays$N_unit
  if (is.null(newdata)) newdata <- object$data
  response_var <- closure_unit_response_var(object$formula)
  y_vals <- as.integer(newdata[[response_var]])
  out <- matrix(0L, nrow = ndraws, ncol = N_unit)
  for (g in seq_len(N_unit)) {
    idx <- arrays$visit_idx[g, seq_len(arrays$n_rep[g])]
    lam_g <- comp$state[, g]
    if (!conditional) {
      out[, g] <- stats::rpois(ndraws, lambda = lam_g)
      next
    }
    y_g <- y_vals[idx]
    cmax <- arrays$Y_max[g]
    K_g  <- arrays$K_max[g]
    p_g  <- comp$p[, idx, drop = FALSE]
    k_grid <- cmax:K_g
    n_k <- length(k_grid)
    # Per-draw log-weight matrix: rows are draws, columns are k.
    # Computed entirely in log space; subtract log_sum_exp per
    # draw to normalise before exponentiating, keeping the tail
    # mass at extreme k numerically stable even at K_g of a few
    # hundred.
    lw <- matrix(NA_real_, nrow = ndraws, ncol = n_k)
    for (kk in seq_along(k_grid)) {
      k <- k_grid[kk]
      lp_pois <- stats::dpois(k, lambda = lam_g, log = TRUE)
      lp_binom <- rep(0, ndraws)
      for (jj in seq_along(idx)) {
        lp_binom <- lp_binom +
          stats::dbinom(y_g[jj], size = k,
                        prob = p_g[, jj], log = TRUE)
      }
      lw[, kk] <- lp_pois + lp_binom
    }
    # Vectorised inverse-CDF sample: subtract per-row maxima to
    # avoid overflow, exponentiate, accumulate the running CDF
    # column-by-column (one vectorised pass over draws per k),
    # normalise by the row sum, and pick the first column where
    # the running CDF exceeds a single uniform draw. Collapses
    # the per-draw sample.int loop into n_k column updates;
    # identical in distribution to the prob = exp(...) call to
    # sample.int.
    row_max <- do.call(pmax, lapply(seq_len(n_k), function(k) lw[, k]))
    w <- exp(lw - row_max)
    # Column-wise running CDF: each column adds the previous
    # column's running total. n_k iterations, each touching
    # ndraws values; far cheaper than apply(w, 1L, cumsum) when
    # ndraws is large.
    cdf <- w
    if (n_k > 1L) {
      for (k in 2:n_k) {
        cdf[, k] <- cdf[, k - 1L] + cdf[, k]
      }
    }
    cdf <- cdf / cdf[, n_k]
    u <- stats::runif(ndraws)
    # Number of CDF entries strictly less than u is the 0-based
    # bin index; +1 gives the 1-based k_grid index.
    bin_idx <- rowSums(cdf < u) + 1L
    out[, g] <- k_grid[bin_idx]
  }
  out
}

#' Log-likelihood per closure unit for an nmix() fit
#'
#' Matches the Stan lpdf's per-unit marginal:
#' \deqn{\log p(y_g | lambda_g, p_g) = \log \sum_{k = Y\_max_g}^{K\_max_g}
#'   Poisson(k | lambda_g) \times \prod_j Binomial(y_{g,j} | k, p_{g,j})}
#' Returned at the closure-unit grain (one column per unit) so
#' `loo()` / `waic()` see one observation per conditionally iid
#' block. Per-visit log-likelihoods would imply visits within a
#' unit are exchangeable, which they are not (they share latent
#' N_g).
#'
#' Threaded through the standard `dispatch_log_lik` signature.
#' The closure-unit arrays + posterior `p` draws are passed in
#' `family_pars` via the upstream extension hook in
#' `log_lik_single_response()`.
#'
#' @noRd
log_lik_nmix <- function(linpred, link, y, family_pars, trials) {
  checkmate::assert_matrix(linpred)
  checkmate::assert_choice(link, "log")
  arrays <- family_pars$closure_arrays
  if (is.null(arrays)) {
    stop(insight::format_error(
      "log_lik_nmix() requires 'closure_arrays' in family_pars."
    ))
  }
  p_mat <- family_pars$p
  checkmate::assert_matrix(
    p_mat, nrows = nrow(linpred), ncols = ncol(linpred)
  )
  lambda_visit <- .linkinv(linpred, link)
  ndraws <- nrow(linpred)
  N_unit <- arrays$N_unit
  y_int  <- as.integer(y)
  out <- matrix(NA_real_, nrow = ndraws, ncol = N_unit)
  for (g in seq_len(N_unit)) {
    idx  <- arrays$visit_idx[g, seq_len(arrays$n_rep[g])]
    lam  <- lambda_visit[, idx[1L]]
    y_g  <- y_int[idx]
    p_g  <- p_mat[, idx, drop = FALSE]
    cmax <- arrays$Y_max[g]
    K_g  <- arrays$K_max[g]
    k_grid <- cmax:K_g
    lp_mat <- matrix(NA_real_, nrow = ndraws, ncol = length(k_grid))
    for (kk in seq_along(k_grid)) {
      k <- k_grid[kk]
      lp_pois <- stats::dpois(k, lambda = lam, log = TRUE)
      lp_binom <- rep(0, ndraws)
      for (jj in seq_along(idx)) {
        lp_binom <- lp_binom +
          stats::dbinom(y_g[jj], size = k,
                        prob = p_g[, jj], log = TRUE)
      }
      lp_mat[, kk] <- lp_pois + lp_binom
    }
    # log_sum_exp across the truncated k grid.
    m <- apply(lp_mat, 1L, max)
    out[, g] <- m + log(rowSums(exp(lp_mat - m)))
  }
  out
}

# ============================================================
# nmix("royle_nichols") R-side downstream methods
# ============================================================
# Per-individual detection r enters as `p` on the family constructor
# (logit link). The per-visit detection probability marginalised
# over latent abundance N ~ Poisson(lambda) is
# `1 - exp(-r_j * lambda_g)` (Royle and Nichols 2003), giving a
# closed-form posterior_epred. Sampling and the conditional N
# posterior need the truncated `1 - (1 - r_j)^k` form because they
# condition on specific draws of N. Numerically safer via
# log1mexp() than via
# direct (1 - r)^k subtraction.

#' Numerically stable log(1 - exp(-a)) for a > 0
#'
#' Maechler 2012 algorithm. Used by the Royle-Nichols log-lik /
#' latent_N kernels where the per-(unit, k) term carries
#' `log(1 - (1 - r_j)^k) = log1mexp(-k * log(1 - r_j))`. The
#' branch at `log(2)` switches between `log(-expm1(-a))` (stable
#' near `a = 0`) and `log1p(-exp(-a))` (stable for large `a`).
#'
#' @param a Non-negative numeric vector / matrix.
#' @return `log(1 - exp(-a))`.
#' @noRd
log1mexp <- function(a) {
  out <- a
  small <- a <= log(2)
  out[ small] <- log(-expm1(-a[ small]))
  out[!small] <- log1p(-exp(-a[!small]))
  out
}

#' Extract per-row mu and per-species Psi (and nu) draws for an
#' mv-response fit
#'
#' Helper for the `mvn()` and `mvt()` post-fit kernels. Returns
#' `mu` on the response scale (identity link; the trend pipeline
#' has already added the latent factor contribution
#' `Z[k, :] * lv[i, :]`), the per-species residual scale `Psi`
#' broadcast to the per-row level, and (for mvt) `nu`.
#'
#' The mv-response closure-unit pattern groups by site only with
#' K rows per site. `as.integer(as.factor(data$series))` yields
#' the species index per row, which is used to broadcast Psi.
#'
#' @param object Fitted `mvgam` object with an mv-response family.
#' @param newdata Long-format observation data; defaults to the
#'   training data on `object`.
#' @param draw_ids Optional posterior draw indices.
#' @param needs_nu Logical; pull `nu` from the posterior. TRUE for
#'   `mvt()`, FALSE for `mvn()`.
#' @return Named list with `mu` `[ndraws x N_obs]`, `Psi_row`
#'   `[ndraws x N_obs]` (per-row Psi after species lookup), `nu`
#'   numeric vector of length `ndraws` (or `NULL`), `species_idx`
#'   integer vector of length N_obs, `K`, `ndraws`.
#' @noRd
extract_mv_response_components <- function(object, newdata = NULL,
                                            draw_ids = NULL,
                                            ndraws = NULL,
                                            needs_nu = FALSE,
                                            linpred = NULL) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_int(ndraws, lower = 1, null.ok = TRUE)
  if (!is_multi_response_family(object$family)) {
    stop(insight::format_error(
      "extract_mv_response_components() requires an mv-response fit."
    ))
  }
  if (is.null(newdata)) {
    newdata <- object$obs_data %||% object$data
    if (is.null(newdata)) {
      stop(insight::format_error(
        "Training data not stored on object; supply 'newdata'."
      ))
    }
  }
  if (!"series" %in% colnames(newdata)) {
    stop(insight::format_error(c(
      "mv-response families require a 'series' factor column in 'data'.",
      i = "Each row of 'data' is one (site, species) observation."
    )))
  }
  series_fac <- as.factor(newdata$series)
  species_idx <- as.integer(series_fac)
  K <- length(levels(series_fac))

  # mu enters with the latent factor contribution Z[k, :] * lv[i, :]
  # already added via the trend pipeline. process_error = FALSE
  # because closure-unit families do not run a stochastic trend
  # layer. When `linpred` is supplied (log_lik path) skip the
  # recomputation so the caller's already-subsampled linpred is
  # used; this keeps Psi aligned to the same draw indices as mu.
  if (is.null(linpred)) {
    linpred <- posterior_linpred(
      object, newdata = newdata, draw_ids = draw_ids,
      ndraws = ndraws, process_error = FALSE
    )
  }
  mu <- object$family$linkinv(linpred)
  ndraws_actual <- nrow(mu)
  N_obs <- ncol(mu)

  # Resolve which posterior draw indices the linpred is on so Psi
  # (and nu) line up element-wise. nmix uses the same convention:
  # draw_ids when supplied, else seq_len(ndraws_actual).
  draws_mat <- posterior::as_draws_matrix(object$fit)
  if (is.null(draw_ids)) {
    draw_ids_local <- seq_len(ndraws_actual)
  } else if (length(draw_ids) != ndraws_actual) {
    draw_ids_local <- draw_ids[seq_len(ndraws_actual)]
  } else {
    draw_ids_local <- draw_ids
  }

  # Psi declared in Stan as `vector<lower=0>[K] Psi`.
  psi_cols <- paste0("Psi[", seq_len(K), "]")
  missing_cols <- setdiff(psi_cols, colnames(draws_mat))
  if (length(missing_cols) > 0L) {
    stop(insight::format_error(c(
      "Posterior is missing Psi columns for mv-response family.",
      x = paste0(
        "Expected: ", paste(psi_cols, collapse = ", "),
        "; missing: ", paste(missing_cols, collapse = ", "), "."
      )
    )))
  }
  Psi_full <- draws_mat[draw_ids_local, psi_cols, drop = FALSE]
  # Broadcast Psi[, species_idx] to a [ndraws x N_obs] matrix so
  # downstream kernels can use it element-wise alongside mu.
  Psi_row <- Psi_full[, species_idx, drop = FALSE]
  dim(Psi_row) <- c(ndraws_actual, N_obs)

  nu_draws <- NULL
  if (needs_nu) {
    if (!"nu" %in% colnames(draws_mat)) {
      stop(insight::format_error(
        "Posterior is missing the 'nu' column for mvt() family."
      ))
    }
    nu_draws <- as.numeric(draws_mat[draw_ids_local, "nu"])
  }

  list(
    mu          = mu,
    Psi_row     = Psi_row,
    Psi         = unname(as.matrix(Psi_full)),
    nu          = nu_draws,
    species_idx = species_idx,
    K           = K,
    ndraws      = ndraws_actual,
    N_obs       = N_obs
  )
}

#' Per-row expected value for an `mvn()` fit
#'
#' Identity link. The trend pipeline already adds the latent factor
#' contribution `Z[k, :] * lv[i, :]` to `mu`, so the conditional
#' expectation `E[y | lv]` is just `mu`. Marginalising over
#' `lv ~ N(0, I)` leaves the marginal mean unchanged because the
#' factor contribution is mean-zero.
#'
#' @inheritParams extract_mv_response_components
#' @return `[ndraws x N_obs]` matrix of expected values.
#' @noRd
posterior_epred_mvn <- function(object, newdata = NULL,
                                 draw_ids = NULL, ndraws = NULL) {
  extract_mv_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_nu = FALSE
  )$mu
}

#' Per-row response RNG for an `mvn()` fit
#'
#' Draws `y[s, n] ~ N(mu[s, n], Psi[s, species(n)])`. The latent
#' factor contribution to `mu` is sampled jointly with the model,
#' so the per-row residual is conditionally independent normal.
#' Marginalising over `lv ~ N(0, I)` recovers the gllvm/boral
#' marginal `y_unit ~ MVN(mu_unit, Z Z' + diag(Psi^2))`.
#'
#' @inheritParams posterior_epred_mvn
#' @return `[ndraws x N_obs]` numeric matrix of response draws.
#' @noRd
posterior_predict_mvn <- function(object, newdata = NULL,
                                   draw_ids = NULL, ndraws = NULL) {
  comp <- extract_mv_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_nu = FALSE
  )
  matrix(
    stats::rnorm(
      comp$ndraws * comp$N_obs,
      mean = as.numeric(comp$mu),
      sd   = as.numeric(comp$Psi_row)
    ),
    nrow = comp$ndraws, ncol = comp$N_obs
  )
}

#' Per-observation log-likelihood for `mvn()`
#'
#' Independent normal per row at the conditional gllvm
#' parameterisation: `lp_n = dnorm(y_n | mu_n, Psi[species(n)],
#' log = TRUE)` where `mu_n` already includes the per-draw latent
#' factor contribution.
#'
#' loo / waic score at the (site, species) row grain rather than
#' the per-site joint grain because the K rows of a site are
#' conditionally independent given `lv_i`. Aggregating to the
#' site grain (`loo` with `r_eff = relative_eff()` over sites)
#' is appropriate when the user wants leave-one-site-out scoring;
#' the default leave-one-row-out scoring is the natural
#' conditional-on-lv quantity.
#'
#' @param linpred Per-row linpred `[ndraws x N_obs]` already on the
#'   identity scale (mu).
#' @param link Family link ("identity" for mvn / mvt).
#' @param y Numeric response vector of length N_obs.
#' @param family_pars List with `Psi_row` `[ndraws x N_obs]` (and
#'   `nu` numeric vector of length ndraws for mvt).
#' @param trials Unused.
#' @return `[ndraws x N_obs]` log-density matrix.
#' @noRd
log_lik_mvn <- function(linpred, link, y, family_pars, trials) {
  Psi_row <- family_pars$Psi_row
  ndraws <- nrow(linpred)
  N_obs <- ncol(linpred)
  if (length(y) != N_obs) {
    stop(insight::format_error(c(
      "log_lik_mvn: y length does not match linpred columns.",
      x = paste0("length(y) = ", length(y),
                 ", ncol(linpred) = ", N_obs, ".")
    )))
  }
  if (!identical(dim(Psi_row), c(ndraws, N_obs))) {
    stop(insight::format_error(c(
      "log_lik_mvn: Psi_row dimensions do not match linpred.",
      x = paste0(
        "Psi_row: ", paste(dim(Psi_row), collapse = "x"),
        "; expected ", ndraws, "x", N_obs, "."
      )
    )))
  }
  y_mat <- matrix(y, nrow = ndraws, ncol = N_obs, byrow = TRUE)
  matrix(
    stats::dnorm(
      as.numeric(y_mat),
      mean = as.numeric(linpred),
      sd   = as.numeric(Psi_row),
      log  = TRUE
    ),
    nrow = ndraws, ncol = N_obs
  )
}

#' Per-row expected value for an `mvt()` fit
#'
#' Same as `posterior_epred_mvn()`. The Student-t residual has
#' mean `mu` for `nu > 1` (always satisfied here because `nu` has
#' a hard lower bound of 2 by Stan declaration).
#'
#' @inheritParams posterior_epred_mvn
#' @return `[ndraws x N_obs]` matrix.
#' @noRd
posterior_epred_mvt <- function(object, newdata = NULL,
                                 draw_ids = NULL, ndraws = NULL) {
  extract_mv_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_nu = TRUE
  )$mu
}

#' Per-row response RNG for an `mvt()` fit
#'
#' Draws `y[s, n] = mu[s, n] + Psi[s, species(n)] * t_nu` where
#' `t_nu ~ Student-t(0, 1, nu[s])`. The conditional gllvm
#' parameterisation puts the latent factor contribution in `mu`,
#' so the per-row residual is conditionally Student-t with
#' per-species scale.
#'
#' @inheritParams posterior_epred_mvn
#' @return `[ndraws x N_obs]` numeric matrix.
#' @noRd
posterior_predict_mvt <- function(object, newdata = NULL,
                                   draw_ids = NULL, ndraws = NULL) {
  comp <- extract_mv_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_nu = TRUE
  )
  # nu is per-draw; broadcast to per-cell with matrix() recycling.
  nu_row <- matrix(comp$nu, nrow = comp$ndraws, ncol = comp$N_obs)
  t_draws <- matrix(
    stats::rt(comp$ndraws * comp$N_obs, df = as.numeric(nu_row)),
    nrow = comp$ndraws, ncol = comp$N_obs
  )
  comp$mu + comp$Psi_row * t_draws
}

#' Per-observation log-likelihood for `mvt()`
#'
#' Independent scaled Student-t per row at the conditional gllvm
#' parameterisation: `lp_n = dt((y_n - mu_n) / Psi[species(n)],
#' df = nu, log = TRUE) - log(Psi[species(n)])` (location-scale
#' transform Jacobian).
#'
#' @param linpred Per-row linpred `[ndraws x N_obs]`.
#' @param link Family link ("identity").
#' @param y Numeric response vector of length N_obs.
#' @param family_pars List with `Psi_row` `[ndraws x N_obs]` and
#'   `nu` numeric vector of length `ndraws`.
#' @param trials Unused.
#' @return `[ndraws x N_obs]` log-density matrix.
#' @noRd
log_lik_mvt <- function(linpred, link, y, family_pars, trials) {
  Psi_row <- family_pars$Psi_row
  nu      <- family_pars$nu
  ndraws <- nrow(linpred)
  N_obs <- ncol(linpred)
  if (length(y) != N_obs) {
    stop(insight::format_error(
      "log_lik_mvt: y length does not match linpred columns."
    ))
  }
  if (!identical(dim(Psi_row), c(ndraws, N_obs))) {
    stop(insight::format_error(
      "log_lik_mvt: Psi_row dimensions do not match linpred."
    ))
  }
  if (length(nu) != ndraws) {
    stop(insight::format_error(
      "log_lik_mvt: nu length does not match ndraws."
    ))
  }
  y_mat <- matrix(y, nrow = ndraws, ncol = N_obs, byrow = TRUE)
  nu_mat <- matrix(nu, nrow = ndraws, ncol = N_obs)
  z <- (y_mat - linpred) / Psi_row
  # dt is log density at z; Jacobian for the scale Psi is
  # -log(Psi) so the unscaled-y log density is dt(z, log) - log(Psi).
  dt_z <- matrix(
    stats::dt(as.numeric(z), df = as.numeric(nu_mat), log = TRUE),
    nrow = ndraws, ncol = N_obs
  )
  dt_z - log(Psi_row)
}

#' Extract per-row softmax probabilities + dpars for a simplex fit
#'
#' Shared helper for the `diri()`, `multi()`, and `categ()` post-fit
#' kernels. Builds the closure-unit arrays from `newdata`, assembles
#' per-unit mu blocks (`mu[idx] - mu[idx[1]]` reference subtraction,
#' matching the Stan lpdf body), applies `softmax` per draw per unit,
#' and broadcasts the result back to a `[ndraws x N_obs]` per-row
#' probability matrix.
#'
#' The species-axis Z column constraint (`sum_to_zero_vector[K]`)
#' and the per-unit reference subtraction together identify the
#' simplex up to the K-shared shift the softmax cannot resolve;
#' both already live in the Stan emission, so the R-side helper
#' just mirrors the reference subtraction so that R-side predictions
#' agree with the Stan-side likelihood.
#'
#' @param object Fitted `mvgam` object with a simplex-response family.
#' @param newdata Long-format observation data.
#' @param draw_ids Optional posterior draw indices.
#' @param ndraws Optional integer; passed to posterior_linpred when
#'   `linpred` is not supplied.
#' @param needs_phi Logical; pull `phi` from the posterior (TRUE for
#'   `diri()`, FALSE for `multi()` / `categ()`). When a `phi ~ ...`
#'   sub-formula was supplied at fit time, the per-row linpred is
#'   recomputed on `newdata` via `extract_component_linpred()` and
#'   the inverse log link is applied. When no sub-formula was
#'   supplied, the scalar `phi` posterior column is broadcast across
#'   rows. Either way the K rows of a closure unit share the value
#'   at the unit's first row, mirroring Stan's `phi[idx[1]]` per-unit
#'   collapse.
#' @param linpred Optional precomputed linpred. When supplied the
#'   internal `posterior_linpred()` call is skipped.
#' @return Named list with `prob_row` `[ndraws x N_obs]` per-row
#'   softmax probabilities, `phi` `[ndraws x N_obs]` per-row
#'   Dirichlet concentration (constant within each closure unit)
#'   or `NULL`, `arrays`, `ndraws`, `N_obs`, `N_unit`.
#' @noRd
extract_simplex_response_components <- function(object,
                                                  newdata = NULL,
                                                  draw_ids = NULL,
                                                  ndraws = NULL,
                                                  needs_phi = FALSE,
                                                  linpred = NULL) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_int(ndraws, lower = 1, null.ok = TRUE)
  if (!is_simplex_response_family(object$family)) {
    stop(insight::format_error(
      "extract_simplex_response_components() requires a simplex-response fit."
    ))
  }
  if (is.null(newdata)) {
    newdata <- object$obs_data %||% object$data
    if (is.null(newdata)) {
      stop(insight::format_error(
        "Training data not stored on object; supply 'newdata'."
      ))
    }
  }
  response_var <- closure_unit_response_var(object$formula)
  arrays <- build_closure_unit_arrays(
    newdata, response_var = response_var,
    compute_y_max      = FALSE,
    unit_grouping_vars = "time"
  )

  # Resolve draw_ids up front so the mu linpred and the per-row phi
  # (when sourced from a sub-formula) use the same posterior rows.
  # Without this, an ndraws-only call would randomly subsample once
  # for mu and again for phi, mis-aligning the two by draw index.
  draw_ids <- closure_unit_resolve_draw_ids(object, ndraws, draw_ids)

  if (is.null(linpred)) {
    linpred <- posterior_linpred(
      object, newdata = newdata, draw_ids = draw_ids,
      ndraws = ndraws, process_error = FALSE
    )
  }
  mu <- linpred
  ndraws_actual <- nrow(mu)
  N_obs <- ncol(mu)
  N_unit <- arrays$N_unit
  visit_idx <- arrays$visit_idx
  n_rep <- arrays$n_rep

  # Build per-row softmax probabilities by walking units. Each unit
  # contributes Kg rows whose probabilities sum to 1 along the draw
  # axis.
  prob_row <- matrix(0, nrow = ndraws_actual, ncol = N_obs)
  for (g in seq_len(N_unit)) {
    Kg <- n_rep[g]
    idx <- visit_idx[g, seq_len(Kg)]
    mu_unit <- mu[, idx, drop = FALSE]
    # Mirror Stan's `mu_unit = mu[idx] - mu[idx[1]]` reference shift.
    # softmax is shift-invariant so the resulting probabilities are
    # the same as without the subtraction, but staying explicit
    # documents the parameterisation match.
    mu_unit_centred <- mu_unit - mu_unit[, 1L]
    exp_mu <- exp(mu_unit_centred)
    prob_row[, idx] <- exp_mu / rowSums(exp_mu)
  }

  phi_mat <- NULL
  if (needs_phi) {
    phi_mat <- extract_phi_per_row(
      object        = object,
      newdata       = newdata,
      draw_ids      = draw_ids,
      ndraws_actual = ndraws_actual,
      N_obs         = N_obs,
      arrays        = arrays
    )
  }

  list(
    prob_row = prob_row,
    phi      = phi_mat,
    arrays   = arrays,
    ndraws   = ndraws_actual,
    N_obs    = N_obs,
    N_unit   = N_unit
  )
}

#' Per-row Dirichlet concentration on `newdata`
#'
#' Returns the per-row `phi` value for each (draw, observation) in a
#' `[ndraws x N_obs]` matrix. Two paths:
#'
#' - **Scalar phi**: brms emits a `phi` column when no `phi ~ ...`
#'   sub-formula is supplied. We pull the posterior column and
#'   broadcast it across the `N_obs` axis.
#' - **Per-row phi**: when the posterior carries `b_phi_*` /
#'   `Intercept_phi` / `s_phi_*` columns, we call
#'   `extract_component_linpred()` with `component = "phi"` on
#'   `newdata` to recompose the per-row linpred (matching whatever
#'   parametric / smooth / RE / GP structure the user supplied for
#'   `phi`) and apply the log link inverse `exp()` to get phi on the
#'   response scale.
#'
#' After extraction the per-unit collapse is applied: each closure
#' unit's K rows are set to the value at the unit's first row
#' (`visit_idx[g, 1]`), mirroring Stan's `phi[idx[1]]` semantics in
#' `diri_lpdf()`. Per-row phi only enters the joint Dirichlet
#' density once per unit, so any cross-row variation in the unit
#' would be ignored by the likelihood and is removed here to keep
#' the R-side and Stan-side scoring identical.
#'
#' @noRd
extract_phi_per_row <- function(object, newdata, draw_ids,
                                  ndraws_actual, N_obs, arrays) {
  draws_mat <- posterior::as_draws_matrix(object$fit)
  has_scalar_phi <- "phi" %in% colnames(draws_mat)
  has_phi_subformula <- any(grepl(
    "^b_phi_|^Intercept_phi$|^bs_phi($|\\[)|^s_phi_",
    colnames(draws_mat)
  ))

  if (has_phi_subformula) {
    # Per-row phi via the dpar linpred pipeline. The `phi` component
    # branch in extract_component_linpred() strips the `_phi` infix
    # from both the parameter draws and the standata, then composes
    # the linpred via the same kernel mu uses; we then apply log^-1.
    phi_linpred <- extract_component_linpred(
      mvgam_fit = object,
      newdata   = newdata,
      component = "phi",
      draw_ids  = draw_ids
    )
    if (nrow(phi_linpred) != ndraws_actual ||
        ncol(phi_linpred) != N_obs) {
      stop(insight::format_error(c(
        "Per-row 'phi' linpred dimension mismatch.",
        x = paste0(
          "Expected [", ndraws_actual, " x ", N_obs,
          "], got [", nrow(phi_linpred), " x ", ncol(phi_linpred), "]."
        )
      )))
    }
    phi_mat <- exp(phi_linpred)
  } else if (has_scalar_phi) {
    rows <- if (is.null(draw_ids)) seq_len(ndraws_actual) else draw_ids
    phi_draws <- as.numeric(draws_mat[rows, "phi"])
    phi_mat <- matrix(phi_draws, nrow = ndraws_actual, ncol = N_obs,
                       byrow = FALSE)
  } else {
    stop(insight::format_error(
      "Posterior is missing 'phi' column for diri() family."
    ))
  }

  # Per-unit collapse: each closure unit's Kg rows share `phi[idx[1]]`
  # (Stan parameterisation). Without this collapse a covariate that
  # varies WITHIN a unit would introduce per-row variation that the
  # Stan likelihood never sees.
  for (g in seq_len(arrays$N_unit)) {
    Kg <- arrays$n_rep[g]
    idx <- arrays$visit_idx[g, seq_len(Kg)]
    phi_mat[, idx] <- phi_mat[, idx[1L]]
  }
  phi_mat
}

#' Per-row expected value for a `diri()` fit
#'
#' Each (site, species) row receives its softmax probability,
#' broadcast from the per-site K-vector via `arrays$visit_idx`.
#' Compositional responses sum to 1 across the K rows of a site,
#' so `E[y_{i,k}] = softmax(mu_unit_i)[k]`.
#'
#' @inheritParams extract_simplex_response_components
#' @return `[ndraws x N_obs]` matrix of expected proportions.
#' @noRd
posterior_epred_diri <- function(object, newdata = NULL,
                                  draw_ids = NULL, ndraws = NULL) {
  extract_simplex_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_phi = FALSE
  )$prob_row
}

#' Per-row response RNG for a `diri()` fit
#'
#' Draws `Y_unit ~ Dirichlet(softmax(mu_unit) * phi)` per closure
#' unit via the gamma-normalisation construction. The draws are
#' simplex K-vectors, broadcast back to per-row layout.
#'
#' @inheritParams posterior_epred_diri
#' @return `[ndraws x N_obs]` matrix of simplex draws.
#' @noRd
posterior_predict_diri <- function(object, newdata = NULL,
                                    draw_ids = NULL, ndraws = NULL) {
  comp <- extract_simplex_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_phi = TRUE
  )
  out <- matrix(0, nrow = comp$ndraws, ncol = comp$N_obs)
  for (g in seq_len(comp$N_unit)) {
    Kg <- comp$arrays$n_rep[g]
    idx <- comp$arrays$visit_idx[g, seq_len(Kg)]
    prob_g <- comp$prob_row[, idx, drop = FALSE]
    # alpha[s, k] = prob_g[s, k] * phi[s, idx[1]]. `comp$phi` is a
    # `[ndraws x N_obs]` matrix whose Kg-row block is constant within
    # a unit (per Stan's `phi[idx[1]]` collapse), so the slice
    # `comp$phi[, idx]` gives the same per-draw alpha shape via
    # elementwise multiplication.
    alpha <- prob_g * comp$phi[, idx, drop = FALSE]
    gam <- matrix(
      stats::rgamma(comp$ndraws * Kg, shape = as.numeric(alpha),
                     rate = 1),
      nrow = comp$ndraws, ncol = Kg
    )
    out[, idx] <- gam / rowSums(gam)
  }
  out
}

#' Per-observation log-likelihood for `diri()`
#'
#' Per-unit Dirichlet density `dirichlet_lpdf(y_unit |
#' softmax(mu_unit) * phi)`. The per-unit log-density is assigned to
#' the first row of each unit (`visit_idx[g, 1]`) and zero for the
#' remaining K-1 rows, so the sum across rows recovers the joint
#' log-likelihood and loo / waic naturally score at the site grain.
#'
#' @param linpred `[ndraws x N_obs]` linpred on identity scale.
#' @param link Family link ("identity").
#' @param y Numeric vector of observed proportions, length N_obs.
#' @param family_pars List with `prob_row`, `phi`, `arrays`.
#' @param trials Unused.
#' @return `[ndraws x N_obs]` log-density matrix.
#' @noRd
log_lik_diri <- function(linpred, link, y, family_pars, trials) {
  prob_row <- family_pars$prob_row
  phi      <- family_pars$phi
  arrays   <- family_pars$arrays
  ndraws <- nrow(linpred)
  N_obs <- ncol(linpred)
  out <- matrix(0, nrow = ndraws, ncol = N_obs)
  for (g in seq_len(arrays$N_unit)) {
    Kg <- arrays$n_rep[g]
    idx <- arrays$visit_idx[g, seq_len(Kg)]
    y_unit <- y[idx]
    prob_g <- prob_row[, idx, drop = FALSE]
    alpha <- prob_g * phi[, idx, drop = FALSE]
    # dirichlet_lpdf(y | alpha) = lgamma(sum(alpha)) -
    #   sum(lgamma(alpha)) + sum((alpha - 1) * log(y))
    log_y <- matrix(log(y_unit), nrow = ndraws, ncol = Kg, byrow = TRUE)
    lp <- lgamma(rowSums(alpha)) - rowSums(lgamma(alpha)) +
            rowSums((alpha - 1) * log_y)
    out[, idx[1L]] <- lp
  }
  out
}

#' Per-row expected count for a `multi()` fit
#'
#' Each row's expected count is `softmax(mu_unit)[k] * N_site` where
#' `N_site = sum(Y_unit)` is the per-site trial total. The per-site
#' total is read from `object$obs_data` (or `newdata`) so the
#' epred matches the data-generating multinomial sample size.
#'
#' @inheritParams posterior_epred_diri
#' @return `[ndraws x N_obs]` matrix of expected counts.
#' @noRd
posterior_epred_multi <- function(object, newdata = NULL,
                                   draw_ids = NULL, ndraws = NULL) {
  comp <- extract_simplex_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_phi = FALSE
  )
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
  comp$prob_row * total_mat
}

#' Per-row response RNG for a `multi()` fit
#'
#' Draws `Y_unit ~ Multinomial(N_site, softmax(mu_unit))` per unit.
#' Returns the K cell counts broadcast to per-row layout. The
#' per-site total `N_site` is the sufficient statistic of the
#' multinomial and is held fixed at the observed value (matching
#' the closure constraint baked into the Stan likelihood).
#'
#' @inheritParams posterior_epred_diri
#' @return `[ndraws x N_obs]` integer matrix of counts.
#' @noRd
posterior_predict_multi <- function(object, newdata = NULL,
                                     draw_ids = NULL, ndraws = NULL) {
  comp <- extract_simplex_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_phi = FALSE
  )
  if (is.null(newdata)) {
    newdata <- object$obs_data %||% object$data
  }
  response_var <- closure_unit_response_var(object$formula)
  y_vec <- newdata[[response_var]]
  out <- matrix(0L, nrow = comp$ndraws, ncol = comp$N_obs)
  for (g in seq_len(comp$N_unit)) {
    Kg <- comp$arrays$n_rep[g]
    idx <- comp$arrays$visit_idx[g, seq_len(Kg)]
    n_g <- sum(y_vec[idx])
    prob_g <- comp$prob_row[, idx, drop = FALSE]
    for (s in seq_len(comp$ndraws)) {
      out[s, idx] <- as.integer(
        stats::rmultinom(1L, size = n_g, prob = prob_g[s, ])
      )
    }
  }
  out
}

#' Per-observation log-likelihood for `multi()`
#'
#' Per-unit multinomial log-mass attributed to the first row of
#' each unit. `loo` / `waic` score at the site grain naturally.
#'
#' @inheritParams log_lik_diri
#' @return `[ndraws x N_obs]` log-density matrix.
#' @noRd
log_lik_multi <- function(linpred, link, y, family_pars, trials) {
  prob_row <- family_pars$prob_row
  arrays   <- family_pars$arrays
  ndraws <- nrow(linpred)
  N_obs <- ncol(linpred)
  out <- matrix(0, nrow = ndraws, ncol = N_obs)
  for (g in seq_len(arrays$N_unit)) {
    Kg <- arrays$n_rep[g]
    idx <- arrays$visit_idx[g, seq_len(Kg)]
    y_unit <- as.integer(y[idx])
    n_g <- sum(y_unit)
    prob_g <- prob_row[, idx, drop = FALSE]
    # multinomial log-mass: lgamma(n + 1) - sum(lgamma(y + 1)) +
    #   sum(y * log(p))
    log_p <- log(prob_g)
    y_mat <- matrix(y_unit, nrow = ndraws, ncol = Kg, byrow = TRUE)
    lp <- lgamma(n_g + 1) - sum(lgamma(y_unit + 1)) +
            rowSums(y_mat * log_p)
    out[, idx[1L]] <- lp
  }
  out
}

#' Per-row probability for a `categ()` fit
#'
#' Each row's expected value is the probability that this category
#' is the observed one, i.e. `softmax(mu_unit)[k]`. Returns a
#' probability matrix on the response scale.
#'
#' @inheritParams posterior_epred_diri
#' @return `[ndraws x N_obs]` matrix of probabilities.
#' @noRd
posterior_epred_categ <- function(object, newdata = NULL,
                                   draw_ids = NULL, ndraws = NULL) {
  extract_simplex_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_phi = FALSE
  )$prob_row
}

#' Per-row response RNG for a `categ()` fit
#'
#' Draws one category per site from `Categorical(softmax(mu_unit))`
#' and broadcasts to a per-row one-hot encoding so the result
#' matches the long-format data layout (one 1 and (K-1) 0s per
#' site).
#'
#' @inheritParams posterior_epred_diri
#' @return `[ndraws x N_obs]` integer matrix of 0 / 1 values.
#' @noRd
posterior_predict_categ <- function(object, newdata = NULL,
                                     draw_ids = NULL, ndraws = NULL) {
  comp <- extract_simplex_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_phi = FALSE
  )
  out <- matrix(0L, nrow = comp$ndraws, ncol = comp$N_obs)
  for (g in seq_len(comp$N_unit)) {
    Kg <- comp$arrays$n_rep[g]
    idx <- comp$arrays$visit_idx[g, seq_len(Kg)]
    prob_g <- comp$prob_row[, idx, drop = FALSE]
    for (s in seq_len(comp$ndraws)) {
      cat_code <- sample.int(Kg, size = 1L, prob = prob_g[s, ])
      out[s, idx[cat_code]] <- 1L
    }
  }
  out
}

#' Per-observation log-likelihood for `categ()`
#'
#' Per-unit categorical log-mass at the observed one-hot category
#' code, attributed to the first row of each unit.
#'
#' @inheritParams log_lik_diri
#' @return `[ndraws x N_obs]` log-density matrix.
#' @noRd
log_lik_categ <- function(linpred, link, y, family_pars, trials) {
  prob_row <- family_pars$prob_row
  arrays   <- family_pars$arrays
  ndraws <- nrow(linpred)
  N_obs <- ncol(linpred)
  out <- matrix(0, nrow = ndraws, ncol = N_obs)
  for (g in seq_len(arrays$N_unit)) {
    Kg <- arrays$n_rep[g]
    idx <- arrays$visit_idx[g, seq_len(Kg)]
    y_unit <- as.integer(y[idx])
    # Observed category code: the index where y_unit == 1.
    cat_code <- which(y_unit == 1L)
    if (length(cat_code) != 1L) {
      stop(insight::format_error(c(
        "categ() observation is not a single one-hot per site.",
        x = paste0(
          "Site ", g, " has ", sum(y_unit),
          " ones across K = ", Kg, " species rows."
        ),
        i = "Each site must have exactly one species selected."
      )))
    }
    lp <- log(prob_row[, idx[cat_code]])
    out[, idx[1L]] <- lp
  }
  out
}

#' Per-visit expected detection probability for an
#' `nmix("royle_nichols")` fit
#'
#' Closed-form marginal over the Poisson abundance prior:
#' \deqn{E[Y_{g, j} | lambda_g, r_j] = 1 - exp(-r_j * lambda_g)}.
#' (`E[(1-r)^N]` for `N ~ Poisson(lambda)` is the MGF of N at
#' `log(1-r)`, which simplifies to `exp(-r * lambda)`.) No Jensen
#' correction or truncation needed.
#'
#' @inheritParams posterior_epred_nmix
#' @return `[S x N_visit]` matrix of expected detection
#'   probabilities in (0, 1).
#' @noRd
posterior_epred_nmix_royle_nichols <- function(object,
                                                newdata = NULL,
                                                draw_ids = NULL) {
  comp <- extract_closure_unit_components(object, newdata, draw_ids)
  unit_of_visit <- visit_to_unit_lookup(comp$arrays, comp$n_visit)
  lambda_visit <- comp$state[, unit_of_visit, drop = FALSE]
  r_visit <- comp$p
  1 - exp(-r_visit * lambda_visit)
}

#' Per-visit binary response draws for an
#' `nmix("royle_nichols")` fit (unconditional)
#'
#' Two-step generative simulation: per posterior draw, sample
#' `N_g ~ Poisson(lambda_g)`, then for each visit `j` sample
#' `y_{g, j} ~ Bernoulli(1 - (1 - r_j)^N_g)`. This preserves the
#' within-unit correlation structure (every visit to a unit
#' shares the same N draw), which a flat per-visit Bernoulli
#' would discard.
#'
#' @inheritParams posterior_epred_nmix
#' @return `[S x N_visit]` integer matrix of 0/1 detections.
#' @noRd
posterior_predict_nmix_royle_nichols <- function(object,
                                                  newdata = NULL,
                                                  draw_ids = NULL) {
  comp <- extract_closure_unit_components(object, newdata, draw_ids)
  arrays <- comp$arrays
  ndraws <- comp$ndraws
  out <- matrix(0L, nrow = ndraws, ncol = comp$n_visit)
  for (g in seq_len(arrays$N_unit)) {
    idx <- arrays$visit_idx[g, seq_len(arrays$n_rep[g])]
    lam_g <- comp$state[, g]
    N_draws <- stats::rpois(ndraws, lambda = lam_g)
    for (j in idx) {
      r_j <- comp$p[, j]
      # 1 - (1 - r_j)^N_draws via log space avoids the catastrophic
      # cancellation that hits for small r and small N_draws.
      log_1m_r <- log1p(-r_j)
      p_visit <- 1 - exp(N_draws * log_1m_r)
      out[, j] <- stats::rbinom(ndraws, size = 1L, prob = p_visit)
    }
  }
  out
}

#' Per-closure-unit latent-abundance draws for an
#' `nmix("royle_nichols")` fit
#'
#' Reverse-Bayes conditional posterior:
#' \deqn{P(N_g = k | y_g, lambda_g, r_g) \propto
#'   Poisson(k | lambda_g) \times
#'   \prod_j (1 - (1 - r_{g, j})^k)^{y_{g, j}}
#'         ((1 - r_{g, j})^k)^{1 - y_{g, j}}}
#' for `k = Y_max[g]..K_max[g]`. Weights are accumulated in log
#' space using `log1mexp()` for the detection term so the
#' per-draw sample stays stable across realistic `K_max` and the
#' small-`r` regime where `(1 - r)^k` is close to 1. The
#' unconditional branch (`conditional = FALSE`) draws directly
#' from the Poisson prior.
#'
#' @inheritParams posterior_latent_N
#' @return `[S x N_unit]` integer matrix of latent abundance
#'   draws.
#' @noRd
posterior_latent_N_royle_nichols <- function(object,
                                              newdata = NULL,
                                              draw_ids = NULL,
                                              conditional = TRUE) {
  checkmate::assert_flag(conditional)
  comp <- extract_closure_unit_components(object, newdata, draw_ids)
  arrays <- comp$arrays
  ndraws <- comp$ndraws
  N_unit <- arrays$N_unit
  if (is.null(newdata)) newdata <- object$data
  response_var <- closure_unit_response_var(object$formula)
  y_vals <- as.integer(newdata[[response_var]])
  out <- matrix(0L, nrow = ndraws, ncol = N_unit)
  for (g in seq_len(N_unit)) {
    idx <- arrays$visit_idx[g, seq_len(arrays$n_rep[g])]
    lam_g <- comp$state[, g]
    if (!conditional) {
      out[, g] <- stats::rpois(ndraws, lambda = lam_g)
      next
    }
    y_g <- y_vals[idx]
    cmax <- arrays$Y_max[g]
    K_g  <- arrays$K_max[g]
    log_1m_r_g <- log1p(-comp$p[, idx, drop = FALSE])
    detected_idx <- which(y_g == 1L)
    nondet_idx   <- which(y_g == 0L)
    sum_nondet_log_1m_r <- if (length(nondet_idx) > 0L) {
      rowSums(log_1m_r_g[, nondet_idx, drop = FALSE])
    } else {
      rep(0, ndraws)
    }
    k_grid <- cmax:K_g
    n_k <- length(k_grid)
    lw <- matrix(NA_real_, nrow = ndraws, ncol = n_k)
    for (kk in seq_along(k_grid)) {
      k <- k_grid[kk]
      lp_pois <- stats::dpois(k, lambda = lam_g, log = TRUE)
      lp_nondet <- k * sum_nondet_log_1m_r
      lp_det <- if (k > 0L && length(detected_idx) > 0L) {
        rowSums(log1mexp(-k * log_1m_r_g[, detected_idx, drop = FALSE]))
      } else if (k == 0L && length(detected_idx) > 0L) {
        rep(-Inf, ndraws)
      } else {
        rep(0, ndraws)
      }
      lw[, kk] <- lp_pois + lp_nondet + lp_det
    }
    # Vectorised inverse-CDF sample identical to the Poisson-binomial
    # path: subtract per-row maxima for numerical stability, build a
    # running CDF column-by-column, and pick the first column whose
    # running CDF exceeds a single uniform draw per row.
    row_max <- do.call(pmax, lapply(seq_len(n_k), function(k) lw[, k]))
    w <- exp(lw - row_max)
    cdf <- w
    if (n_k > 1L) {
      for (k in 2:n_k) {
        cdf[, k] <- cdf[, k - 1L] + cdf[, k]
      }
    }
    cdf <- cdf / cdf[, n_k]
    u <- stats::runif(ndraws)
    bin_idx <- rowSums(cdf < u) + 1L
    out[, g] <- k_grid[bin_idx]
  }
  out
}

#' Log-likelihood per closure unit for an
#' `nmix("royle_nichols")` fit
#'
#' Per-unit marginal mirroring the Stan emission:
#' \deqn{\log p(y_g | lambda_g, r_g) = \log \sum_{k = Y\_max_g}^{K\_max_g}
#'   Poisson(k | lambda_g) \times
#'   \prod_j (1 - (1 - r_{g, j})^k)^{y_{g, j}}
#'         ((1 - r_{g, j})^k)^{1 - y_{g, j}}}.
#' Returned at the closure-unit grain (one column per unit) so
#' `loo()` / `waic()` see one observation per conditionally iid
#' block.
#'
#' @noRd
log_lik_nmix_royle_nichols <- function(linpred, link, y,
                                         family_pars, trials) {
  checkmate::assert_matrix(linpred)
  checkmate::assert_choice(link, "log")
  arrays <- family_pars$closure_arrays
  if (is.null(arrays)) {
    stop(insight::format_error(
      "log_lik_nmix_royle_nichols() requires 'closure_arrays' in family_pars."
    ))
  }
  p_mat <- family_pars$p
  checkmate::assert_matrix(
    p_mat, nrows = nrow(linpred), ncols = ncol(linpred)
  )
  lambda_visit <- .linkinv(linpred, link)
  ndraws <- nrow(linpred)
  N_unit <- arrays$N_unit
  y_int  <- as.integer(y)
  out <- matrix(NA_real_, nrow = ndraws, ncol = N_unit)
  for (g in seq_len(N_unit)) {
    idx <- arrays$visit_idx[g, seq_len(arrays$n_rep[g])]
    lam <- lambda_visit[, idx[1L]]
    y_g <- y_int[idx]
    log_1m_r_g <- log1p(-p_mat[, idx, drop = FALSE])
    detected_idx <- which(y_g == 1L)
    nondet_idx   <- which(y_g == 0L)
    sum_nondet_log_1m_r <- if (length(nondet_idx) > 0L) {
      rowSums(log_1m_r_g[, nondet_idx, drop = FALSE])
    } else {
      rep(0, ndraws)
    }
    cmax <- arrays$Y_max[g]
    K_g  <- arrays$K_max[g]
    k_grid <- cmax:K_g
    lp_mat <- matrix(NA_real_, nrow = ndraws, ncol = length(k_grid))
    for (kk in seq_along(k_grid)) {
      k <- k_grid[kk]
      lp_pois <- stats::dpois(k, lambda = lam, log = TRUE)
      lp_nondet <- k * sum_nondet_log_1m_r
      lp_det <- if (k > 0L && length(detected_idx) > 0L) {
        rowSums(log1mexp(-k * log_1m_r_g[, detected_idx, drop = FALSE]))
      } else if (k == 0L && length(detected_idx) > 0L) {
        rep(-Inf, ndraws)
      } else {
        rep(0, ndraws)
      }
      lp_mat[, kk] <- lp_pois + lp_nondet + lp_det
    }
    m <- apply(lp_mat, 1L, max)
    out[, g] <- m + log(rowSums(exp(lp_mat - m)))
  }
  out
}

# ============================================================
# nmix("poisson_poisson") R-side downstream methods
# ============================================================
# Encounter-count model. Per-individual encounter rate p is log-
# linked; the marginal of `y_visit ~ Poisson(N * p), N ~
# Poisson(lambda)` is Neyman Type A (over-dispersed Poisson).
# The truncated-N likelihood and the conditional N posterior both
# use the factored Poisson form
#   sum_t poisson_log_pmf(y_t | log(k) + log(p_t))
#   = log(k) * sum(y) + sum(y * log(p))
#     - k * sum(p) - sum(lgamma(y + 1))
# so the inner marginalisation loop is O(K_max[g]) per unit
# with constant per-k work (an O(n_rep) precompute amortises
# across all k values).

#' Per-visit expected count for an `nmix("poisson_poisson")` fit
#'
#' Closed-form marginal mean: `E[y_t] = lambda * p_t`. No Jensen
#' correction needed; the Poisson abundance prior contributes
#' only its mean to the per-visit count expectation.
#'
#' @inheritParams posterior_epred_nmix
#' @return `[S x N_visit]` matrix of expected counts.
#' @noRd
posterior_epred_nmix_poisson_poisson <- function(object,
                                                  newdata = NULL,
                                                  draw_ids = NULL) {
  comp <- extract_closure_unit_components(object, newdata, draw_ids)
  unit_of_visit <- visit_to_unit_lookup(comp$arrays, comp$n_visit)
  lambda_visit <- comp$state[, unit_of_visit, drop = FALSE]
  lambda_visit * comp$p
}

#' Per-visit response draws for an `nmix("poisson_poisson")` fit
#'
#' Two-step generative simulation: sample `N_g ~ Poisson(lambda_g)`
#' once per posterior draw, then sample each visit's count from
#' `Poisson(N_g * p_t)`. Sharing one `N_g` across visits within a
#' unit preserves the Neyman Type A within-unit dependence.
#'
#' @inheritParams posterior_epred_nmix
#' @return `[S x N_visit]` integer matrix of visit counts.
#' @noRd
posterior_predict_nmix_poisson_poisson <- function(object,
                                                    newdata = NULL,
                                                    draw_ids = NULL) {
  comp <- extract_closure_unit_components(object, newdata, draw_ids)
  arrays <- comp$arrays
  ndraws <- comp$ndraws
  out <- matrix(0L, nrow = ndraws, ncol = comp$n_visit)
  for (g in seq_len(arrays$N_unit)) {
    idx <- arrays$visit_idx[g, seq_len(arrays$n_rep[g])]
    lam_g <- comp$state[, g]
    N_draws <- stats::rpois(ndraws, lambda = lam_g)
    for (j in idx) {
      p_j <- comp$p[, j]
      out[, j] <- stats::rpois(ndraws, lambda = N_draws * p_j)
    }
  }
  out
}

#' Per-closure-unit latent-abundance draws for an
#' `nmix("poisson_poisson")` fit
#'
#' Reverse-Bayes conditional posterior:
#' \deqn{P(N_g = k | y_g, lambda_g, p_g) \propto
#'   Poisson(k | lambda_g) \times \prod_j Poisson(y_{g,j} | k * p_{g,j})}
#' for `k = 0..K_max[g]`. The `k = 0` cell is consistent only with
#' all-zero detection histories. The factored log-weight
#' (`log(k) * sum_y + sum_y_log_p - k * sum_p - lgamma_const`)
#' makes the per-k cost `O(1)` after one `O(n_rep)` precompute.
#'
#' @inheritParams posterior_latent_N
#' @return `[S x N_unit]` integer matrix of latent abundance
#'   draws.
#' @noRd
posterior_latent_N_poisson_poisson <- function(object,
                                                newdata = NULL,
                                                draw_ids = NULL,
                                                conditional = TRUE) {
  checkmate::assert_flag(conditional)
  comp <- extract_closure_unit_components(object, newdata, draw_ids)
  arrays <- comp$arrays
  ndraws <- comp$ndraws
  N_unit <- arrays$N_unit
  if (is.null(newdata)) newdata <- object$data
  response_var <- closure_unit_response_var(object$formula)
  y_vals <- as.integer(newdata[[response_var]])
  out <- matrix(0L, nrow = ndraws, ncol = N_unit)
  for (g in seq_len(N_unit)) {
    idx <- arrays$visit_idx[g, seq_len(arrays$n_rep[g])]
    lam_g <- comp$state[, g]
    if (!conditional) {
      out[, g] <- stats::rpois(ndraws, lambda = lam_g)
      next
    }
    y_g <- y_vals[idx]
    K_g <- arrays$K_max[g]
    # comp$p arrives on the response (rate) scale already, post-
    # inv-link from brms; log(p_g) recovers the linear predictor
    # log_p for use in the factored Poisson form. Mirrors the Stan
    # block which computes log_p = log(p) at the top of the lpmf.
    p_g <- comp$p[, idx, drop = FALSE]
    log_p_g <- log(p_g)
    sum_counts <- sum(y_g)
    sum_y_log_p <- as.numeric(log_p_g %*% y_g)
    sum_p_g <- rowSums(p_g)
    lgamma_const <- sum(lgamma(y_g + 1))
    any_detection <- sum_counts > 0L
    k_grid <- 0:K_g
    n_k <- length(k_grid)
    lw <- matrix(NA_real_, nrow = ndraws, ncol = n_k)
    # k = 0 cell.
    if (any_detection) {
      lw[, 1L] <- -Inf
    } else {
      lw[, 1L] <- stats::dpois(0L, lambda = lam_g, log = TRUE)
    }
    if (K_g >= 1L) {
      log_k_grid <- log(seq_len(K_g))
      for (kk in seq_len(K_g)) {
        k <- kk
        log_k <- log_k_grid[kk]
        lw[, kk + 1L] <- stats::dpois(k, lambda = lam_g, log = TRUE) +
          log_k * sum_counts + sum_y_log_p -
          k * sum_p_g - lgamma_const
      }
    }
    # Vectorised inverse-CDF sample identical to the PB and RN
    # paths: subtract per-row maxima for numerical stability,
    # build a running CDF column-by-column, pick the first
    # column whose running CDF exceeds a single uniform draw.
    row_max <- do.call(pmax, lapply(seq_len(n_k), function(k) lw[, k]))
    w <- exp(lw - row_max)
    cdf <- w
    if (n_k > 1L) {
      for (k in 2:n_k) {
        cdf[, k] <- cdf[, k - 1L] + cdf[, k]
      }
    }
    cdf <- cdf / cdf[, n_k]
    u <- stats::runif(ndraws)
    bin_idx <- rowSums(cdf < u) + 1L
    out[, g] <- k_grid[bin_idx]
  }
  out
}

#' Log-likelihood per closure unit for an
#' `nmix("poisson_poisson")` fit
#'
#' Per-unit marginal mirroring the Stan emission:
#' \deqn{\log p(y_g | lambda_g, p_g) = \log \sum_{k = 0}^{K\_max_g}
#'   Poisson(k | lambda_g) \times \prod_j Poisson(y_{g,j} | k * p_{g,j})}.
#' Uses the same factored Poisson form as the Stan code (constant
#' per-k cost after one `O(n_rep)` precompute). Returned at the
#' closure-unit grain (one column per unit) so `loo()` / `waic()`
#' see one observation per conditionally iid block.
#'
#' @noRd
log_lik_nmix_poisson_poisson <- function(linpred, link, y,
                                          family_pars, trials) {
  checkmate::assert_matrix(linpred)
  checkmate::assert_choice(link, "log")
  arrays <- family_pars$closure_arrays
  if (is.null(arrays)) {
    stop(insight::format_error(
      "log_lik_nmix_poisson_poisson() requires 'closure_arrays' in family_pars."
    ))
  }
  p_mat <- family_pars$p
  checkmate::assert_matrix(
    p_mat, nrows = nrow(linpred), ncols = ncol(linpred)
  )
  lambda_visit <- .linkinv(linpred, link)
  ndraws <- nrow(linpred)
  N_unit <- arrays$N_unit
  y_int  <- as.integer(y)
  out <- matrix(NA_real_, nrow = ndraws, ncol = N_unit)
  for (g in seq_len(N_unit)) {
    idx <- arrays$visit_idx[g, seq_len(arrays$n_rep[g])]
    lam <- lambda_visit[, idx[1L]]
    y_g <- y_int[idx]
    p_g <- p_mat[, idx, drop = FALSE]
    log_p_g <- log(p_g)
    sum_counts   <- sum(y_g)
    sum_y_log_p  <- as.numeric(log_p_g %*% y_g)
    sum_p_g      <- rowSums(p_g)
    lgamma_const <- sum(lgamma(y_g + 1))
    any_detection <- sum_counts > 0L
    K_g <- arrays$K_max[g]
    k_grid <- 0:K_g
    lp_mat <- matrix(NA_real_, nrow = ndraws, ncol = length(k_grid))
    # k = 0 cell.
    if (any_detection) {
      lp_mat[, 1L] <- -Inf
    } else {
      lp_mat[, 1L] <- stats::dpois(0L, lambda = lam, log = TRUE)
    }
    if (K_g >= 1L) {
      log_k_grid <- log(seq_len(K_g))
      for (kk in seq_len(K_g)) {
        k <- kk
        log_k <- log_k_grid[kk]
        lp_mat[, kk + 1L] <- stats::dpois(k, lambda = lam, log = TRUE) +
          log_k * sum_counts + sum_y_log_p -
          k * sum_p_g - lgamma_const
      }
    }
    m <- apply(lp_mat, 1L, max)
    out[, g] <- m + log(rowSums(exp(lp_mat - m)))
  }
  out
}

# ============================================================
# occ() R-side downstream methods
# ============================================================
# Mirror the nmix() pattern: every method delegates to
# `extract_closure_unit_components()` for psi (state), p (per
# visit) and the closure-unit arrays, then applies the
# occupancy-specific likelihood / sampling formula.

#' Per-visit expected detection probability for an occ() fit
#'
#' Returns the per-visit response-scale expectation
#' `E[Y_{g, j} | psi_g, p_{g, j}] = psi_g * p_{g, j}`, which is
#' the marginal Bernoulli rate over the latent occupancy state.
#' No Jensen correction is needed: both factors are linear in
#' their parameters at the per-draw level.
#'
#' @param object Fitted `mvgam` object with `family = occ()`.
#' @param newdata Long-format observation data; defaults to
#'   training data.
#' @param draw_ids Optional vector of posterior draw indices.
#' @return `[S x N_visit]` matrix of expected detection rates
#'   in (0, 1).
#' @noRd
posterior_epred_occ <- function(object, newdata = NULL,
                                 draw_ids = NULL) {
  comp <- extract_closure_unit_components(object, newdata, draw_ids)
  # Broadcast unit-grain psi back to per-visit length via the
  # visit-to-unit lookup encoded in `arrays$visit_idx`. The
  # first-visit column is shared by every visit of a unit, so
  # the inverse mapping is straightforward.
  unit_of_visit <- visit_to_unit_lookup(comp$arrays, comp$n_visit)
  comp$state[, unit_of_visit, drop = FALSE] * comp$p
}

#' Per-visit response draws for an occ() fit (unconditional)
#'
#' Two-step generative simulation: per posterior draw, sample
#' `z_g ~ Bernoulli(psi_g)` then `y_{g, j} ~ Bernoulli(z_g *
#' p_{g, j})`. This preserves the within-unit correlation
#' structure (every visit to a site shares the same z draw),
#' which a flat `Bernoulli(psi * p)` would discard. The
#' marginal distribution of y is identical to Bernoulli(psi*p),
#' but PPCs of within-unit detection patterns rely on the
#' shared z (Kery and Royle 2016, ch. 10.3).
#'
#' @inheritParams posterior_epred_occ
#' @return `[S x N_visit]` integer matrix of 0/1 detections.
#' @noRd
posterior_predict_occ <- function(object, newdata = NULL,
                                   draw_ids = NULL) {
  comp <- extract_closure_unit_components(object, newdata, draw_ids)
  arrays <- comp$arrays
  ndraws <- comp$ndraws
  out <- matrix(0L, nrow = ndraws, ncol = comp$n_visit)
  for (g in seq_len(arrays$N_unit)) {
    idx <- arrays$visit_idx[g, seq_len(arrays$n_rep[g])]
    psi_g <- comp$state[, g]
    z_draws <- stats::rbinom(ndraws, size = 1L, prob = psi_g)
    for (j in idx) {
      out[, j] <- stats::rbinom(ndraws, size = 1L,
                                prob = z_draws * comp$p[, j])
    }
  }
  out
}

#' Per-site posterior occupancy for an occ() fit
#'
#' Returns the posterior of the latent occupancy state z at the
#' per-closure-unit grain. Two regimes:
#'
#' * `conditional = TRUE` (default): Bayes-rule posterior given
#'   the observed detection history. Sites with at least one
#'   detection have `P(z = 1 | y) = 1`; sites with all-zero
#'   histories use
#'   \deqn{P(z_g = 1 | y_g) =
#'     \frac{\psi_g \prod_j (1 - p_{g,j})}
#'          {\psi_g \prod_j (1 - p_{g,j}) + (1 - \psi_g)}}
#'   (Royle and Dorazio 2008, ch. 3).
#' * `conditional = FALSE`: the marginal occupancy probability
#'   psi_g returned directly (the prior occupancy at new
#'   prediction sites without observed y).
#'
#' Output is a probability matrix by default. Set `draw = TRUE`
#' to return 0/1 integer Bernoulli draws (matches ubms's
#' `posterior_predict(param = "z")` semantics; required for
#' downstream uses such as richness estimation or
#' colonisation-extinction simulations where ignoring
#' stochasticity in z would introduce bias).
#'
#' @param object Fitted `mvgam` object with `family = occ()`.
#' @param newdata Long-format observation data; defaults to
#'   training data.
#' @param draw_ids Optional vector of posterior draw indices.
#' @param conditional Logical. If TRUE (default), reweight
#'   `psi` by the Bernoulli likelihood of the observed history.
#'   If FALSE, return the marginal `psi`.
#' @param draw Logical. If FALSE (default), return the
#'   probability `P(z = 1)` per site per draw. If TRUE, return
#'   0/1 Bernoulli draws from that probability.
#' @return `[S x N_unit]` matrix; probability in (0, 1) when
#'   `draw = FALSE`, integer 0/1 when `draw = TRUE`.
#' @noRd
posterior_occupancy <- function(object, newdata = NULL,
                                 draw_ids = NULL,
                                 conditional = TRUE,
                                 draw = FALSE) {
  checkmate::assert_flag(conditional)
  checkmate::assert_flag(draw)
  comp <- extract_closure_unit_components(object, newdata, draw_ids)
  arrays <- comp$arrays
  ndraws <- comp$ndraws
  N_unit <- arrays$N_unit
  if (!conditional) {
    # Marginal psi at the unit grain. No use of observed y.
    probs <- comp$state
  } else {
    if (is.null(newdata)) newdata <- object$data
    response_var <- closure_unit_response_var(object$formula)
    y_vals <- as.integer(newdata[[response_var]])
    probs <- matrix(NA_real_, nrow = ndraws, ncol = N_unit)
    for (g in seq_len(N_unit)) {
      idx   <- arrays$visit_idx[g, seq_len(arrays$n_rep[g])]
      psi_g <- comp$state[, g]
      y_g   <- y_vals[idx]
      if (arrays$Y_max[g] >= 1L) {
        # Detected at least once: z = 1 with probability 1.
        probs[, g] <- 1
        next
      }
      # All-zero history: Bayes rule on the binary latent state.
      # Computed in log space and exponentiated last so per-draw
      # underflow at psi ~ 1 with many zero visits is safe.
      p_g <- comp$p[, idx, drop = FALSE]
      log_psi   <- log(psi_g)
      log1m_psi <- log1p(-psi_g)
      sum_log1m_p <- rowSums(log1p(-p_g))
      ll_z1 <- log_psi + sum_log1m_p
      ll_z0 <- log1m_psi
      m <- pmax(ll_z1, ll_z0)
      probs[, g] <- exp(ll_z1 - m) /
        (exp(ll_z1 - m) + exp(ll_z0 - m))
    }
  }
  if (!draw) return(probs)
  # Integer 0/1 draws via Bernoulli; preserved as integer matrix
  # for downstream interop with richness / colonisation code.
  out <- matrix(stats::rbinom(length(probs), size = 1L, prob = probs),
                nrow = nrow(probs), ncol = ncol(probs))
  storage.mode(out) <- "integer"
  out
}

#' Log-likelihood per closure unit for an occ() fit
#'
#' Matches the Stan lpdf's per-unit marginal:
#' \deqn{\log p(y_g | \psi_g, p_g) =
#'   \log[\psi_g \prod_j p_{g,j}^{y_{g,j}} (1 - p_{g,j})^{1 - y_{g,j}}
#'        + (1 - \psi_g) \mathbb{1}(\sum_j y_{g,j} = 0)]}
#' Returned at the closure-unit grain (one column per unit) so
#' `loo()` / `waic()` see one observation per conditionally iid
#' block (visits within a unit share latent z; they are not
#' exchangeable across units).
#'
#' Threaded through the standard `dispatch_log_lik` signature.
#' The closure-unit arrays + posterior `p` draws are passed in
#' `family_pars` via the upstream extension hook in
#' `log_lik_single_response()`.
#'
#' @noRd
log_lik_occ <- function(linpred, link, y, family_pars, trials) {
  checkmate::assert_matrix(linpred)
  checkmate::assert_choice(link, "logit")
  arrays <- family_pars$closure_arrays
  if (is.null(arrays)) {
    stop(insight::format_error(
      "log_lik_occ() requires 'closure_arrays' in family_pars."
    ))
  }
  p_mat <- family_pars$p
  checkmate::assert_matrix(
    p_mat, nrows = nrow(linpred), ncols = ncol(linpred)
  )
  psi_visit <- .linkinv(linpred, link)
  ndraws <- nrow(linpred)
  N_unit <- arrays$N_unit
  y_int  <- as.integer(y)
  out <- matrix(NA_real_, nrow = ndraws, ncol = N_unit)
  for (g in seq_len(N_unit)) {
    idx   <- arrays$visit_idx[g, seq_len(arrays$n_rep[g])]
    psi   <- psi_visit[, idx[1L]]
    y_g   <- y_int[idx]
    p_g   <- p_mat[, idx, drop = FALSE]
    log_psi   <- log(psi)
    log1m_psi <- log1p(-psi)
    # Bernoulli per-visit log-prob given z = 1.
    log_p_y <- matrix(0, nrow = ndraws, ncol = length(idx))
    for (jj in seq_along(idx)) {
      log_p_y[, jj] <- ifelse(
        y_g[jj] == 1L,
        log(p_g[, jj]),
        log1p(-p_g[, jj])
      )
    }
    ll_z1 <- log_psi + rowSums(log_p_y)
    if (arrays$Y_max[g] >= 1L) {
      out[, g] <- ll_z1
    } else {
      ll_z0 <- log1m_psi
      m <- pmax(ll_z1, ll_z0)
      out[, g] <- m + log(exp(ll_z1 - m) + exp(ll_z0 - m))
    }
  }
  out
}
