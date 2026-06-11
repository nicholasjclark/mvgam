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
                                       compute_y_max = TRUE,
                                       unit_grouping_vars = NULL) {
  checkmate::assert_data_frame(data, min.rows = 1L)
  checkmate::assert_string(response_var)
  checkmate::assert_string(series_var)
  checkmate::assert_string(time_var)
  checkmate::assert_string(cap_var)
  checkmate::assert_integerish(default_cap, lower = 1L, len = 1L,
                               null.ok = TRUE)
  checkmate::assert_flag(compute_y_max)
  if (is.null(unit_grouping_vars)) {
    unit_grouping_vars <- c(series_var, time_var)
  }
  checkmate::assert_character(unit_grouping_vars, min.len = 1L,
                              any.missing = FALSE)
  # Required columns: response + grouping always; cap only when no
  # default has been supplied AND we need it (count-family path).
  # Binary-response families pass `default_cap = 1L` to make `cap`
  # optional. Multi-response families pass `compute_y_max = FALSE`,
  # which drops the cap requirement entirely.
  required_cols <- c(response_var, unit_grouping_vars)
  if (compute_y_max && is.null(default_cap)) {
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
  cap_vals <- if (cap_var %in% colnames(data)) {
    as.integer(data[[cap_var]])
  } else {
    rep(as.integer(default_cap), nrow(data))
  }
  if (anyNA(cap_vals)) {
    stop(insight::format_error(
      paste0(
        "Missing values in '", cap_var,
        "' are not allowed for closure-unit families."
      )
    ))
  }
  # Pre-flight invariant: cap must be constant within a closure
  # unit. `validate_closure_unit_data()` catches this earlier in
  # the mvgam pipeline with a richer error message; this defence
  # protects standalone callers (tests, downstream tools).
  Y_max <- integer(n_unit)
  K_max <- integer(n_unit)
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
#' (Royle 2004 equation; ito4303 Stan implementation
#' \url{https://gist.github.com/ito4303/33bf2d192d121e257e25f97e6d48df73}).
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
#' @section Choosing between Poisson-binomial and Royle-Nichols:
#' The two parameterisations are NOT statistically distinguishable
#' from data alone, because their visit-level variance structures
#' coincide at fixed `N` and `r`. Family choice is a scientific
#' judgement about the detection mechanism: per-visit `p` for
#' sampling-effort or observation-condition heterogeneity that
#' applies uniformly to all individuals in a unit; per-individual
#' `r` for territorial / song-post / camera-placement heterogeneity
#' driven by individual behaviour. Royle-Nichols requires binary
#' detection (0/1) per visit; Poisson-binomial accepts arbitrary
#' count data.
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
#' `lambda` and `p` are prior-dominated and the reverse-Bayes
#' `predict(type = "latent_N")` returns the prior on `N` rather
#' than a data-informed posterior).
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
#' @section K_max for Poisson-Poisson:
#' Unlike Royle-Nichols (which has a two-sided saturation rule),
#' Poisson-Poisson `K_max[g]` only needs to satisfy
#' `ppois(K_max[g], lambda_hat, lower.tail = FALSE) < 1e-4`
#' (the Poisson tail negligible). A safe rough default is
#' `K_max[g] = max(Y_max[g], ceiling(lambda_hat + 4 * sqrt(lambda_hat)))`
#' where `lambda_hat` is an initial estimate of the per-unit
#' abundance (e.g. `mean(y_g) / mean(p_hat)` with `p_hat` from a
#' prior-mean encounter rate). Users supply this as the `cap`
#' column on the input data.
#'
#' @references
#' Royle, J. A., and Nichols, J. D. (2003). Estimating abundance
#'   from repeated presence-absence data or point counts.
#'   *Ecology*, 84, 777-790.
#'   \doi{10.1890/0012-9658(2003)084[0777:EAFRPA]2.0.CO;2}.
#'
#' @export
nmix <- function(type = c("poisson_binomial", "royle_nichols",
                          "poisson_poisson")) {
  type <- match.arg(type)
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
  # Royle-Nichols takes binary detection input; trigger the
  # y in {0, 1} validation. Poisson-binomial and Poisson-Poisson
  # accept count input. None of the variants set
  # mvgam_default_cap: latent N exceeds 1 for all three and the
  # user must supply the `cap` column.
  if (type == "royle_nichols") {
    attr(fam, "mvgam_binary_response") <- TRUE
  }
  attr(fam, "mvgam_predict_types") <- c("latent_N", "detection")
  # The lpmf signature determines which data fields brms must
  # thread through; declared once on the family so that
  # prepare_closure_unit_family() does not have to know per-family
  # signatures.
  attr(fam, "mvgam_vars") <- c(
    "N_unit", "n_rep", "K_max", "Y_max", "visit_idx"
  )
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
#' mvgam: `predict(fit, type = "occupancy")` returns the
#' conditional probability `P(z = 1 | y)` per site;
#' `posterior_occupancy(fit, conditional = TRUE, draw = TRUE)`
#' returns 0/1 z draws; `posterior_occupancy(conditional = FALSE)`
#' returns marginal psi. `predict(fit, type = "detection")`
#' returns the per-visit detection probability p_{g,j} on the
#' response scale.
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
occ <- function() {
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
  attr(fam, "mvgam_predict_types")   <- c("occupancy", "detection")
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
    "    real lp = 0;",
    "    // Convert dpars back to logit scale for the stable",
    "    // inverse-link forms.",
    "    vector[num_elements(mu)] logit_psi = logit(mu);",
    "    vector[num_elements(p)]  logit_p   = logit(p);",
    "    for (g in 1 : N_unit) {",
    "      int n_g = n_rep[g];",
    "      array[n_g] int idx = visit_idx[g, 1:n_g];",
    "      // psi is constant within a closure unit; pull it",
    "      // from the first visit's linear predictor.",
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
#' is shift-invariant in `mu_unit` (adding `c * 1_K` leaves the
#' likelihood unchanged), so under the default iid `Z` prior the
#' columns of `Z` carry a residual level indeterminacy that the
#' Heaps post-hoc QR rotation does not absorb. The downstream
#' factor-model emission adds a soft column-sum constraint on
#' `Z` (`sum(Z[, l]) ~ normal(0, 0.01)`) whenever
#' `is_simplex_response_family(family)` is TRUE, which removes the
#' shift mode at trivial cost. The `loadings_prior` matrix-normal
#' prior, when supplied, already encodes a zero column mean and
#' so doubles as a stronger version of the constraint.
#'
#' Data layout: long format, one row per (site, species), exactly
#' K rows per site. The user passes the species axis via the
#' `species` argument on `jsdgam()` (or labels it `series` in the
#' data when using `mvgam()` directly); the closure-unit data prep
#' groups by (series, time) so each site forms one closure unit.
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
#' # Compositional JSDM on long-format proportions data
#' mod <- jsdgam(
#'   formula = y ~ env,
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
#' post-hoc QR identification of `Z` + the column-sum soft
#' constraint together fix the shift indeterminacy that softmax
#' otherwise leaves in `mu_unit`.
#'
#' brms calls this function ONCE per likelihood evaluation with
#' the full `Y` and `mu` vectors plus the closure-unit indexing
#' arrays threaded through via `family$vars`.
#'
#' @return A character scalar suitable for
#'   `brms::stanvar(scode = ..., block = "functions")`.
#' @noRd
diri_stan_funs <- function() {
  paste(
    "  real diri_lpdf(",
    "    vector y,",
    "    vector mu,",
    "    real phi,",
    "    int N_unit,",
    "    array[] int n_rep,",
    "    array[,] int visit_idx) {",
    "    real lp = 0;",
    "    for (g in 1:N_unit) {",
    "      int Kg = n_rep[g];",
    "      array[Kg] int idx = visit_idx[g, 1:Kg];",
    "      vector[Kg] y_unit  = y[idx];",
    "      vector[Kg] mu_unit = mu[idx];",
    "      lp += dirichlet_logit_lpdf(y_unit | mu_unit, phi);",
    "    }",
    "    return lp;",
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
  make_closure_unit_arrays_stanvars(
    arrays,
    family_funs_name = "diri_funs",
    family_funs      = diri_stan_funs(),
    include_K_max    = FALSE,
    include_Y_max    = FALSE
  )
}

#' Stan function block for the closure-unit N-mixture lpmf
#'
#' Implements the Royle (2004) Poisson-binomial marginalisation
#' over the latent abundance N as a `log_sum_exp` across the
#' truncated range `max(y[g, ]) <= k <= K_max[g]`. Each k value
#' scores the joint Poisson-Binomial log-probability of
#' (latent = k, observed visit counts); the sum gives the
#' closure-unit marginal log-likelihood. Below `max(y[g, ])`
#' the closure constraint forces zero probability, encoded as
#' `negative_infinity()` in the lp vector.
#'
#' Numerical-stability notes:
#'   - `mu` arrives from brms as the exponentiated linear
#'     predictor (positive rate); converting back to
#'     `log_mu = log(mu)` keeps the inner `poisson_log_lpmf`
#'     form stable for large abundance values.
#'   - `p` arrives from brms as the inv-logit linear predictor
#'     (probability); converting back to
#'     `logit_p = logit(p)` keeps `binomial_logit_lpmf` stable
#'     at probabilities close to 0 or 1.
#'   - `log_sum_exp` handles the `-Inf` entries below
#'     `max(y[g, ])` without underflow.
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
    "  real nmix_lpmf(",
    "    array[] int y,",
    "    vector mu,",
    "    vector p,",
    "    int N_unit,",
    "    array[] int n_rep,",
    "    array[] int K_max,",
    "    array[] int Y_max,",
    "    array[,] int visit_idx) {",
    "    real lp = 0;",
    "    // Convert dpars back to link scale for numerically",
    "    // stable lpmf forms.",
    "    vector[num_elements(mu)] log_mu    = log(mu);",
    "    vector[num_elements(p)]  logit_p   = logit(p);",
    "    for (g in 1 : N_unit) {",
    "      int Kg = K_max[g];",
    "      int cmax = Y_max[g];",
    "      array[n_rep[g]] int idx = visit_idx[g, 1:n_rep[g]];",
    "      // lambda is constant within a closure unit; pull it",
    "      // from the first visit's linear predictor.",
    "      real log_lam = log_mu[idx[1]];",
    "      array[n_rep[g]] int counts = y[idx];",
    "      vector[n_rep[g]] lp_visits = logit_p[idx];",
    "      vector[Kg + 1] component_lps;",
    "      // Closure: k < cmax is impossible because every visit",
    "      // observed cmax or fewer, never more.",
    "      for (k in 0 : (cmax - 1)) {",
    "        component_lps[k + 1] = negative_infinity();",
    "      }",
    "      for (k in cmax : Kg) {",
    "        component_lps[k + 1] = poisson_log_lpmf(k | log_lam)",
    "          + binomial_logit_lpmf(counts | k, lp_visits);",
    "      }",
    "      lp += log_sum_exp(component_lps);",
    "    }",
    "    return lp;",
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
    "    array[,] int visit_idx) {",
    "    int N = num_elements(mu);",
    "    return nmix_lpmf(y | mu, rep_vector(p, N), N_unit,",
    "                     n_rep, K_max, Y_max, visit_idx);",
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
    "    real lp = 0;",
    "    vector[num_elements(mu)] log_mu   = log(mu);",
    "    vector[num_elements(p)]  log_1m_r = log1m(p);",
    "    for (g in 1 : N_unit) {",
    "      int Kg = K_max[g];",
    "      int cmax = Y_max[g];",
    "      array[n_rep[g]] int idx = visit_idx[g, 1:n_rep[g]];",
    "      // lambda is constant within a closure unit; pull it",
    "      // from the first visit's linear predictor.",
    "      real log_lam = log_mu[idx[1]];",
    "      array[n_rep[g]] int counts = y[idx];",
    "      vector[n_rep[g]] log_1m_r_v = log_1m_r[idx];",
    "      vector[n_rep[g]] counts_v   = to_vector(counts);",
    "      // Pre-aggregate the constant-in-k non-detection sum so",
    "      // the inner loop is O(1) in n_rep.",
    "      real sum_non_det_log_1m_r = dot_product(",
    "        1.0 - counts_v, log_1m_r_v",
    "      );",
    "      vector[Kg + 1] component_lps;",
    "      // Closure: k < cmax is impossible because the unit",
    "      // observed at least cmax detection events.",
    "      for (k in 0 : (cmax - 1)) {",
    "        component_lps[k + 1] = negative_infinity();",
    "      }",
    "      for (k in cmax : Kg) {",
    "        real lp_k = poisson_log_lpmf(k | log_lam)",
    "          + k * sum_non_det_log_1m_r;",
    "        if (k > 0) {",
    "          // sum_t counts[t] * log(1 - (1 - r_t)^k)",
    "          // log1m_exp(k * log_1m_r_v) computes log(1 - (1-r)^k)",
    "          // elementwise; at k = 0 it returns -inf, which is",
    "          // why this branch is guarded.",
    "          lp_k += dot_product(",
    "            counts_v, log1m_exp(k * log_1m_r_v)",
    "          );",
    "        }",
    "        component_lps[k + 1] = lp_k;",
    "      }",
    "      lp += log_sum_exp(component_lps);",
    "    }",
    "    return lp;",
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
    "    array[,] int visit_idx) {",
    "    real lp = 0;",
    "    vector[num_elements(mu)] log_mu = log(mu);",
    "    vector[num_elements(p)]  log_p  = log(p);",
    "    for (g in 1 : N_unit) {",
    "      int Kg = K_max[g];",
    "      array[n_rep[g]] int idx = visit_idx[g, 1:n_rep[g]];",
    "      // lambda is constant within a closure unit; pull from",
    "      // the first visit's linear predictor.",
    "      real log_lam = log_mu[idx[1]];",
    "      array[n_rep[g]] int counts = y[idx];",
    "      vector[n_rep[g]] log_p_v   = log_p[idx];",
    "      vector[n_rep[g]] p_v       = p[idx];",
    "      vector[n_rep[g]] counts_v  = to_vector(counts);",
    "      int any_detection = Y_max[g] > 0;",
    "      // O(n_rep) precomputes; reused for every k in 1..Kg.",
    "      real sum_counts   = sum(counts_v);",
    "      real sum_y_log_p  = dot_product(counts_v, log_p_v);",
    "      real sum_p_v      = sum(p_v);",
    "      real lgamma_const = sum(lgamma(counts_v + 1));",
    "      vector[Kg + 1] component_lps;",
    "      // k = 0 only consistent with all-zero counts. If any",
    "      // visit detected anything, the k = 0 cell is impossible;",
    "      // otherwise it contributes poisson_log_lpmf(0|log_lam)",
    "      // plus 0 (Poisson(y=0|rate=0) = 1).",
    "      if (any_detection) {",
    "        component_lps[1] = negative_infinity();",
    "      } else {",
    "        component_lps[1] = poisson_log_lpmf(0 | log_lam);",
    "      }",
    "      for (k in 1 : Kg) {",
    "        // Factored sum_t poisson_log_lpmf(y_t|log(k)+log_p_v[t])",
    "        // = log(k) * sum_y + sum_y_log_p",
    "        //   - k * sum_p_v - lgamma_const.",
    "        component_lps[k + 1] = poisson_log_lpmf(k | log_lam)",
    "          + log(k) * sum_counts + sum_y_log_p",
    "          - k * sum_p_v - lgamma_const;",
    "      }",
    "      lp += log_sum_exp(component_lps);",
    "    }",
    "    return lp;",
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
    "    array[,] int visit_idx) {",
    "    int N = num_elements(mu);",
    "    return nmix_poisson_poisson_lpmf(",
    "      y | mu, rep_vector(p, N), N_unit,",
    "      n_rep, K_max, Y_max, visit_idx",
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
  make_closure_unit_arrays_stanvars(
    arrays,
    family_funs_name = "nmix_funs",
    family_funs      = nmix_stan_funs(arrays$max_rep),
    y_max_upper      = NA_integer_,
    include_K_max    = TRUE
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
  make_closure_unit_arrays_stanvars(
    arrays,
    family_funs_name = "nmix_poisson_poisson_funs",
    family_funs      = nmix_poisson_poisson_stan_funs(arrays$max_rep),
    y_max_upper      = NA_integer_,
    include_K_max    = TRUE
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
  multi_response <- is_multi_response_family(family)
  if (multi_response) {
    # Multi-response families (diri / multinomial /
    # categorical / mvnormal / mvt) skip the integer-y / cap
    # validation: their per-unit likelihoods read the K response
    # components directly, without per-unit truncation. The closure
    # unit groups by `time` (site) only -- the K species rows at
    # each site form the per-unit contributions, in contrast to the
    # count-based families where each (species, site) pair is its
    # own closure unit with replicate visits.
    arrays <- build_closure_unit_arrays(
      data, response_var = response_var,
      compute_y_max = FALSE,
      unit_grouping_vars = "time"
    )
  } else {
    validate_closure_unit_data(
      data,
      response_var       = response_var,
      has_obs_covariates = has_obs_covariates,
      has_det_covariates = has_det_covariates,
      binary_y_check     = binary_y_check,
      cap_required       = is.null(default_cap)
    )
    arrays <- build_closure_unit_arrays(
      data, response_var = response_var,
      default_cap = default_cap
    )
  }
  family_stanvars <- switch(
    family_name,
    nmix                 = make_nmix_stanvars(arrays),
    nmix_royle_nichols   = make_nmix_royle_nichols_stanvars(arrays),
    nmix_poisson_poisson = make_nmix_poisson_poisson_stanvars(arrays),
    occ                  = make_occ_stanvars(arrays),
    diri      = make_diri_stanvars(arrays),
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
                  latent_state = posterior_occupancy)
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
  validate_closure_unit_data(
    newdata,
    response_var       = response_var,
    has_obs_covariates = TRUE,
    has_det_covariates = TRUE,
    binary_y_check     = binary_y_check,
    cap_required       = is.null(default_cap)
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
#' "latent_N")` for the conditional posterior of N given the
#' observed counts (Royle 2004 reverse-Bayes).
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
#' Public-facing call site (used by `predict(type = "latent_N")`).
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

#' Per-closure-unit latent-abundance draws for an
#' `nmix("poisson_binomial")` fit
#'
#' Royle (2004) reverse-Bayes conditional posterior:
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
# closed-form posterior_epred. Sampling and reverse-Bayes need the
# truncated `1 - (1 - r_j)^k` form because they condition on
# specific draws of N. Numerically safer via log1mexp() than via
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
# The truncated-N likelihood and reverse-Bayes both use the
# factored Poisson form
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
