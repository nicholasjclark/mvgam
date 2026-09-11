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
# https://discourse.mc-stan.org/t/tweedie-likelihood-compound-poisson-gamma-in-stan/14636
# Both sources are cited in the user-facing roxygen for
# tweedie().
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
#' \url{https://discourse.mc-stan.org/t/tweedie-likelihood-compound-poisson-gamma-in-stan/14636}).
#' The mvgam implementation is adapted from the
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
#'   \url{https://discourse.mc-stan.org/t/tweedie-likelihood-compound-poisson-gamma-in-stan/14636}
#'   Source of the compound Poisson-gamma decomposition that
#'   underlies both
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
#' @examples
#' \dontrun{
#' # Tweedie generates non-negative continuous observations with a
#' # point mass at zero. The dpars are `mphi` (dispersion) and
#' # `mtheta` (power exponent on the open interval (1, 2)).
#' set.seed(3)
#' simdat <- sim_mvgam(family = tweedie(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#'
#' mod <- mvgam(
#'   y ~ 1,
#'   trend_formula = ~ AR(p = 1),
#'   data          = simdat$data_train,
#'   family        = tweedie(),
#'   chains        = 2,
#'   silent        = 2
#' )
#' summary(mod, include_betas = FALSE)
#'
#' # Posterior intervals for the AR dynamics. The observation-side
#' # dpars (`mphi`, `mtheta`) are picked up under "obs_params".
#' mcmc_plot(mod, variable = "trend_params", type = "intervals")
#' }
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

#' The families mvgam builds itself, keyed by constructor name
#'
#' brms builds its own families from a name. These are built by
#' mvgam's constructors, which brms cannot name, so a family given
#' as one of these names is built by calling its constructor.
#'
#' @return A named list of family constructors
#' @noRd
mvgam_family_constructors <- function() {
  list(
    tweedie = tweedie, beta_nb = beta_nb, com_binomial = com_binomial,
    occ = occ, nmix = nmix, diri = diri, multi = multi, categ = categ,
    mvn = mvn, mvt = mvt
  )
}

#' The observation formula and family a model is built from
#'
#' brms reads a family written inside `bf()` in preference to the one
#' given beside the formula. A family inside a univariate `bf()` is
#' taken out of the formula here and becomes the model's family. The
#' likelihood already followed it, while every step mvgam runs on the
#' family took the default gaussian: adding a custom family's Stan
#' functions, injecting its default priors, preparing a closure unit
#' and recording the family on the fit. A multivariate formula keeps
#' each response's family in its own `bf()`, where
#' `formula_families()` reads it.
#'
#' Every response's family is checked once, here. Closure-unit and
#' multi-response families lay the data out by unit and write their
#' likelihood for that layout, so each models its response alone.
#'
#' @param formula The observation formula
#' @param family The family given beside it, in any spelling
#'   `validate_family()` accepts
#' @return A list holding `formula`, with a univariate family taken
#'   out, and `family`
#' @noRd
resolve_observation_family <- function(formula, family) {
  family <- validate_family(family)
  if (inherits(formula, "brmsformula") && !is.null(formula$family)) {
    family <- formula$family
    formula$family <- NULL
  }
  families <- formula_families(formula, family)
  lapply(families, validate_supported_family)
  alone <- Filter(function(f) {
    is_closure_unit_family(f) || is_multi_response_family(f)
  }, families)
  if (length(families) > 1L && length(alone) > 0L) {
    stop(insight::format_error(c(
      paste0("'", resolve_family_name(alone[[1L]]), "()' cannot be one ",
             "response of a multivariate model."),
      x = paste0("Its data are laid out by unit, and its likelihood is ",
                 "written for that layout."),
      i = "Fit that response in a model of its own."
    )), call. = FALSE)
  }
  list(formula = formula, family = family)
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
  # `M` is Stan data and is read from the stored Stan data.
  # `model_data` on a `jsdgam()` fit is the frame, which has no `M`.
  M <- object$standata$M
  if (is.null(M)) {
    stop(insight::format_error(
      "Could not locate the truncation 'M' in the fit's standata."
    ))
  }
  # Posterior mean of mu (per observation) and the two global
  # scalars; mu is on the response scale via family$linkinv.
  mu_mean <- colMeans(posterior_epred(object))
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
    rlang::warn(insight::format_message(c(
      "Tweedie truncation 'M' may be too small.",
      x = paste0(
        "max(lambda) at the posterior mean is ",
        format(lambda_max, digits = 3),
        ", which is more than 70% of M = ", M, "."
      ),
      i = paste0(
        "Refit with a larger truncation such as ",
        "tweedie(M = ", ceiling(lambda_max * 2), "L) and ",
        "compare the 'mphi' / 'mtheta' posteriors."
      )
    )))
  }
  invisible(NULL)
}

# ============================================================
# Beta negative binomial (beta_nb) family
# ============================================================
# Heavy-tailed count family that mixes a negative binomial success
# probability over a Beta distribution: Y | p ~ NegBinomial(r, p),
# p ~ Beta(alpha, beta). Also known as the generalised Waring
# distribution. The tail decays as a power law rather than
# geometrically, so it accommodates the occasional very large count
# that outbreak and boom-bust population series produce and that
# NB2 can only absorb by inflating dispersion everywhere.
# Reference: Irwin (1968), JRSS-A. The Stan log-PMF is written
# against lgamma / lbeta rather than Stan's native
# `beta_neg_binomial_lpmf`, so the family needs no minimum Stan
# version and works on either backend.
#
# Parameterisation. Stan's signature is (r, alpha, beta) with
# E[Y] = r * beta / (alpha - 1), which exists only for alpha > 1,
# and Var[Y] finite only for alpha > 2. A regression family needs
# the mean on the linear predictor, so mvgam samples a shifted tail
# parameter mtail = alpha - 1 with a lower bound of zero and sets
# beta = mu * mtail / shape. The mean is then exactly mu, and the
# awkward alpha > 1 constraint becomes an ordinary positivity bound.
# NB2 is recovered as mtail -> infinity.
#
# Stats-review notes (gate A):
# * `shape` does not inherit the brms negative binomial default of
#   gamma(0.01, 0.01). That prior piles mass near zero, and because
#   beta = mu * mtail / shape diverges as shape -> 0 it drags the
#   Beta mixing distribution toward a degenerate point mass. On data
#   simulated from NB2(shape = 3) it returned shape = 9.0
#   [3.9, 37.9]; gamma(2, 0.5) returned 5.4 [3.5, 10.4].
# * `mtail ~ gamma(2, 0.25)` rather than a tighter gamma(2, 1).
#   Since NB2 sits at mtail -> infinity, a prior with almost no mass
#   above 8 pins the parameter whenever the data are NB2-like, and
#   the resulting misfit is absorbed by `shape`. The wider prior cut
#   that bias (shape 3.9 [3.0, 5.9] against a truth of 3.0) and
#   still places 97% of its mass above the finite-variance boundary
#   at mtail = 1.
# * The predictive cost of using this family when NB2 is correct is
#   about 2 elpd units; the cost of using NB2 when this family is
#   correct was 9.0 (SE 4.4). The asymmetry favours availability.
# * Sampling geometry was checked across the NB2 limit, a moderate
#   tail and mtail near the variance boundary: no divergences, bulk
#   ESS above 1200 throughout, max R-hat 1.006.


#' Beta negative binomial family for heavy-tailed counts
#'
#' A three-parameter count family that mixes the negative binomial
#' success probability over a Beta distribution, giving a
#' power-law rather than geometric tail. Useful for outbreak counts
#' and boom-bust population series where a negative binomial fits
#' the bulk of the data but is repeatedly surprised by the largest
#' observations.
#'
#' @details
#' The family is parameterised so that `mu` is the mean:
#' \deqn{Y \mid p \sim \mathrm{NegBinomial}(r, p), \quad
#'       p \sim \mathrm{Beta}(1 + \tau, \mu \tau / r)}
#' where `r` is `shape` and \eqn{\tau} is `mtail`. Writing Stan's
#' native first Beta parameter as \eqn{\alpha = 1 + \tau} keeps the
#' mean finite by construction, since \eqn{E[Y] = \mu} requires
#' \eqn{\alpha > 1}.
#'
#' The variance is finite only when `mtail` exceeds 1, and the
#' family becomes genuinely heavy-tailed below that. This is the
#' behaviour the family exists to provide, but it has two
#' consequences worth knowing. Randomised quantile residuals, the
#' default in [residuals.mvgam()], are unaffected because they need
#' only the distribution function. Moment-based scoring is not:
#' `score(..., score = "dss")` assumes a finite predictive variance
#' and should not be trusted when the posterior for `mtail` places
#' appreciable mass below 1. Prefer `"crps"`, `"drps"` or `"logs"`.
#'
#' As `mtail` grows the family converges to the negative binomial
#' with the same `shape`, so a posterior for `mtail` that runs off
#' toward its prior upper range is evidence that the extra tail
#' parameter is not needed.
#'
#' @section Distributional parameters:
#' \describe{
#'   \item{`mu`}{Mean of the response, with a log link.}
#'   \item{`shape`}{Negative binomial shape `r`, log link, positive.
#'     Default prior `gamma(2, 0.5)`.}
#'   \item{`mtail`}{Tail parameter \eqn{\tau = \alpha - 1}, log
#'     link, positive. Smaller values give heavier tails; values
#'     below 1 give infinite variance. Default prior
#'     `gamma(2, 0.25)`.}
#' }
#'
#' @section Censoring and truncation:
#' The Stan code supplies the distribution function alongside the
#' density, so `cens()` and `trunc()` addition terms work as they do
#' for the built-in count families.
#'
#' Two limits are worth knowing. The distribution function accumulates
#' the mass function term by term, so both its cost and the length of
#' the automatic-differentiation tape grow linearly in the count at
#' which it is evaluated. At the counts these models usually see this
#' is unnoticeable; a censoring bound in the thousands will dominate
#' the gradient evaluation, and one in the millions is impractical.
#'
#' Right censoring also needs the upper tail, which is computed as
#' `log1m_exp()` of the distribution function. That subtraction loses
#' precision once the survival probability approaches the accumulated
#' rounding error, roughly when `1 - F(y)` falls below `y * 5e-16`. A
#' censoring bound placed far into the tail of a light-tailed fit can
#' therefore return a survival probability of zero rather than a small
#' positive number. Heavy-tailed fits, which are the point of this
#' family, keep `1 - F(y)` large and are unaffected.
#'
#' @return A `customfamily` object for use with [mvgam()].
#' @references
#' Irwin, J. O. (1968). The generalized Waring distribution applied
#' to accident theory. \emph{Journal of the Royal Statistical
#' Society A}, 131(2), 205-225. \doi{10.2307/2343842}
#' @seealso [brms::negbinomial()], which this family approaches as
#'   `mtail` grows, and [tweedie()] for non-negative continuous
#'   responses with a point mass at zero.
#' @examples
#' \dontrun{
#' # Counts with a heavier tail than a negative binomial expects. The
#' # dpars are `shape`, playing the usual negative binomial role, and
#' # `mtail`, where smaller values give heavier tails and anything
#' # below 1 gives infinite variance.
#' set.seed(1)
#' simdat <- sim_mvgam(
#'   family = beta_nb(), n_series = 1L, n_timepoints = 120L,
#'   trend_model = AR(),
#'   family_pars = list(shape = 2, mtail = 2)
#' )
#'
#' mod <- mvgam(
#'   y ~ 1,
#'   trend_formula = ~ AR(p = 1),
#'   data          = simdat$data_train,
#'   family        = beta_nb(),
#'   chains        = 2,
#'   silent        = 2
#' )
#' summary(mod, include_betas = FALSE)
#'
#' # `mtail` is what separates this family from a negative binomial.
#' # A posterior that drifts toward the upper prior range is a sign
#' # the extra tail parameter is not earning its place.
#' mcmc_plot(mod, variable = c("shape", "mtail"), type = "hist")
#' }
#' @export
beta_nb <- function() {
  fam <- brms::custom_family(
    name = "beta_nb",
    dpars = c("mu", "shape", "mtail"),
    links = c("log", "log", "log"),
    lb = c(NA, 0, 0),
    ub = c(NA, NA, NA),
    type = "int"
  )
  # brms::custom_family() omits the `linkinv` / `linkfun` slots that
  # base R family objects carry and that mvgam's epred dispatcher
  # expects; attach them from the primary link.
  link_info <- stats::make.link(fam$link)
  fam$linkinv <- link_info$linkinv
  fam$linkfun <- link_info$linkfun
  attr(fam, "mvgam_stanvars") <- make_beta_nb_stanvars()
  fam
}

#' Is this the beta negative binomial family?
#' @param family A family / brmsfamily / customfamily object
#' @return Logical scalar
#' @noRd
is_beta_nb_family <- function(family) {
  identical(resolve_family_name(family), "beta_nb")
}

#' Default population priors for the beta negative binomial family
#'
#' `shape` would otherwise inherit the brms negative binomial
#' default of `gamma(0.01, 0.01)`, which concentrates mass where
#' the Beta mixing scale `mu * mtail / shape` diverges. `mtail`
#' needs enough prior range to reach the negative binomial limit;
#' see the section notes above this family's constructor.
#' @return A `brmsprior` object
#' @noRd
default_beta_nb_population_priors <- function() {
  c(
    brms::prior("gamma(2, 0.5)", class = "shape"),
    brms::prior("gamma(2, 0.25)", class = "mtail")
  )
}

#' Build the Stan stanvars bundle for the beta negative binomial
#' @return A `brmsstanvars` object
#' @noRd
make_beta_nb_stanvars <- function() {
  brms::stanvar(
    name = "beta_nb_funs",
    scode = beta_nb_stan_funs(),
    block = "functions"
  )
}

#' Stan code for the beta negative binomial function-block helpers
#'
#' Written against `lgamma()` and `lbeta()` rather than Stan's native
#' `beta_neg_binomial_*` suite, which arrived only in Stan 2.36. Two
#' things follow from that choice. The family works on the 'rstan'
#' backend as well as 'cmdstanr', so it needs no version gate; and the
#' distribution function is available, which is what lets `cens()` and
#' `trunc()` addition terms work.
#'
#' The log-PMF matches Stan's native implementation exactly across a
#' grid of 162 parameter combinations. The distribution function
#' accumulates the PMF with the ratio
#' \eqn{p(k+1)/p(k) = (r+k)(\beta+k) / ((k+1)(\alpha+\beta+r+k))},
#' so each term costs a handful of logs rather than four log-gamma
#' calls, and the result is exact to machine precision against a
#' direct sum. Cost grows linearly in `y`, which matters only for
#' censored or truncated observations.
#'
#' The wrapper name deliberately differs from Stan's: a function named
#' `beta_neg_binomial_lpmf` taking three real parameters would match
#' the built-in signature on newer Stan versions and silently bind the
#' arguments to the wrong roles.
#' @noRd
beta_nb_stan_funs <- function() {
  paste(
    "  real beta_nb_lpmf(int y, real mu, real shape, real mtail) {",
    "    // mtail = alpha - 1, so beta = mu * mtail / shape gives E[Y] = mu",
    "    real alpha = 1 + mtail;",
    "    real beta = mu * mtail / shape;",
    "    return lgamma(y + shape) - lgamma(y + 1) - lgamma(shape)",
    "           + lbeta(beta + y, alpha + shape) - lbeta(beta, alpha);",
    "  }",
    "  real beta_nb_lcdf(int y, real mu, real shape, real mtail) {",
    "    // No mass below zero. brms builds the two-sided truncation",
    "    // normaliser as log_diff_exp(lcdf(ub), lcdf(lb - 1)), so",
    "    // trunc(lb = 0) calls this with -1; without the guard the",
    "    // empty loop would return log p(0) and quietly subtract the",
    "    // zero count from the normalising constant.",
    "    if (y < 0) {",
    "      return negative_infinity();",
    "    }",
    "    // Accumulate the PMF by its term ratio; exact for counts and",
    "    // cheaper than re-evaluating log-gamma at every step.",
    "    real alpha = 1 + mtail;",
    "    real beta = mu * mtail / shape;",
    "    real lp = beta_nb_lpmf(0 | mu, shape, mtail);",
    "    real acc = lp;",
    "    for (k in 0 : (y - 1)) {",
    "      lp += log(shape + k) + log(beta + k)",
    "            - log(k + 1) - log(alpha + beta + shape + k);",
    "      acc = log_sum_exp(acc, lp);",
    "    }",
    "    return acc;",
    "  }",
    "  real beta_nb_lccdf(int y, real mu, real shape, real mtail) {",
    "    real lc = beta_nb_lcdf(y | mu, shape, mtail);",
    "    // Rounding can push the distribution function a hair above",
    "    // zero far into the tail, where log1m_exp is undefined.",
    "    return lc >= 0 ? negative_infinity() : log1m_exp(lc);",
    "  }",
    sep = "\n"
  )
}


# ============================================================
# Conway-Maxwell-Binomial (com_binomial) family
# ============================================================
# Bounded-count family generalising the binomial by exponentiating
# the binomial coefficient: P(Y = y | T, p, nu) = C(T, y)^nu * p^y *
# (1-p)^(T-y) / Z(T, p, nu), with Z the sum of unnormalised weights
# over j = 0..T. nu = 1 recovers the standard binomial; nu > 1 under-
# disperses; 0 < nu < 1 over-disperses; nu < 0 super-disperses,
# concentrating mass at the boundaries (bimodal "all-or-nothing").
# Reference: Shmueli et al. (2005), JRSS-C. R-side mathematics and
# the recovery example come from upstream contributor jbogomolovas2
# (also author of the TMB COM-Binomial solver and the glmmTMB
# integration), ported to the v2 custom_family architecture.
#
# Stats-review notes (gate A, 2026-06-24):
# * Logit link enforced on p (only link with a clean theta-factored
#   normaliser; probit / cloglog would lose the separable form).
# * Identity link on nu so super-dispersion (nu < 0) is reachable;
#   `lb = -5` clamps the lower tail where the R-side normaliser
#   loses relative precision without the C++ adaptive-window guard.
# * Default `nu ~ normal(1, 1)`, centred on independence and scaled
#   to the range the data can resolve; the response saturates
#   outside roughly nu in (-1, 4).
# * Variance on the response surface is reported on the proportion
#   scale Var[Y/T] to match the binomial convention.


#' Conway-Maxwell-Binomial family for over-, equi-, under-, and
#' super-dispersed bounded counts
#'
#' A two-parameter generalisation of the binomial that exponentiates
#' the binomial coefficient by a real-valued dispersion parameter
#' `nu`. `nu = 1` is the standard binomial; `nu > 1` under-disperses
#' (concentrates mass near the mode); `0 < nu < 1` over-disperses;
#' `nu < 0` super-disperses, producing bimodal "all-or-nothing" mass
#' at the boundaries. Useful when a binomial fit shows systematic
#' lack-of-fit in either tail and the response is bounded by a known
#' trial count.
#'
#' @param link Link function for the success probability `p`.
#'   Currently `logit` only -- the COM-Binomial PMF factorises
#'   cleanly only against the canonical logit parameterisation, so
#'   `probit` and `cloglog` are deliberately not exposed.
#'
#' @section Distributional parameters:
#' \describe{
#'   \item{`mu`}{logit-scale success probability (link = `logit`).
#'     Distributional regression on `mu` is the standard formula
#'     side: `bf(y | trials(trials) ~ x)`.}
#'   \item{`nu`}{real-valued dispersion exponent (link = `identity`,
#'     `lb = -5`). `nu = 1` is binomial. Distributional regression
#'     is supported via `bf(y | trials(trials) ~ x, nu ~ z)`.}
#' }
#'
#' @section Default priors:
#' \itemize{
#'   \item `nu ~ normal(1, 1)` -- centred at `nu = 1`, where the
#'     trials are independent and the distribution is binomial.
#'     The scale spans the range the data can resolve: the
#'     response saturates below `nu = -1` and above `nu = 4`, so
#'     values outside roughly `(-1, 4)` are barely distinguishable.
#'     Strongly under-dispersed data sit near `nu = 3`. Override
#'     with `prior(normal(1, 2), class = "nu")` to explore wider,
#'     or a tighter scale to pull harder toward the binomial.
#' }
#'
#' @section Sampler notes:
#' Two corners of `(p, nu)` parameter space stress HMC and should
#' be monitored:
#' \itemize{
#'   \item `nu >> 1`: the PMF concentrates on a single mode and the
#'     log-normaliser gradient saturates; max-treedepth saturation
#'     becomes possible above roughly `nu = 5`.
#'   \item `nu << 0` with `p` near `0.5`: mass concentrates at both
#'     boundaries and the likelihood is invariant under
#'     `p -> 1 - p`, producing bimodal posteriors. Asymmetric
#'     covariates break the symmetry.
#' }
#' Inspect `mcmc_plot(fit, variable = "nu", type = "trace")` and
#' the R-hat + ESS on both `nu` and the intercept; if either
#' degrades, tighten the prior on `nu` or shift the design to
#' bias `p` away from `0.5`.
#'
#' @return A `brms::custom_family()` object suitable for the
#'   `family` argument of `mvgam()` / `jsdgam()`. Carries
#'   `attr(., "mvgam_stanvars")` holding the Stan function-block
#'   stanvar so mvgam's setup auto-attaches it.
#'
#' @references
#' Shmueli, G., Minka, T. P., Kadane, J. B., Borle, S. and Boatwright,
#' P. (2005). A useful distribution for fitting discrete data:
#' revival of the Conway-Maxwell-Poisson distribution. \emph{Journal
#' of the Royal Statistical Society Series C}, 54:127-142.
#'
#' Conway, R. W. and Maxwell, W. L. (1962). A queuing model with state
#' dependent service rates. \emph{Journal of Industrial Engineering},
#' 12:132-136.
#'
#' Bogomolovas, J. (2026). COM-Binomial solver in TMB and glmmTMB.
#' R-side mathematics adapted from
#' \url{https://github.com/jbogomolovas2/mvgam/tree/fix-fc-trials-alignment}.
#'
#' @seealso \code{\link[brms]{custom_family}}, \code{\link{tweedie}}
#' @author Nicholas J Clark, Julius Bogomolovas
#' @examples
#' \dontrun{
#' # Simulate an under-dispersed COM-Binomial series via
#' # sim_mvgam(): nu_true = 1.5, T = 20 trials per observation,
#' # the default observation-side smooth `s(x)`, and a random-walk
#' # latent state on the logit-probability.
#' set.seed(1)
#' sim <- sim_mvgam(
#'   family       = com_binomial(),
#'   n_series     = 1L,
#'   n_timepoints = 120L,
#'   trend_model  = RW(),
#'   family_pars  = list(nu = 1.5, trials = 20L)
#' )
#' head(sim$data_train)
#'
#' # Fit the matching state-space CMB model. `trials` is supplied
#' # through brms's `trials()` addition term; `nu` is the
#' # dispersion parameter (nu = 1 recovers the binomial,
#' # nu > 1 under-dispersed, nu < 1 over-dispersed, nu < 0
#' # super-dispersed / bimodal).
#' fit <- mvgam(
#'   bf(y | trials(trials) ~ s(x)),
#'   trend_formula = ~ RW(),
#'   data          = sim$data_train,
#'   family        = com_binomial(),
#'   chains        = 2,
#'   iter          = 800,
#'   warmup        = 400,
#'   silent        = 2
#' )
#'
#' # The `nu` posterior should concentrate around the simulated
#' # 1.5 and stay well above the binomial null at nu = 1.
#' summary(fit, include_betas = FALSE)
#' mcmc_plot(fit, variable = "nu", type = "areas")
#'
#' # Posterior predictive check of the marginal count distribution.
#' pp_check(fit, type = "bars", ndraws = 50)
#' }
#' @export
com_binomial <- function(link = "logit") {
  checkmate::assert_choice(link, "logit")
  fam <- brms::custom_family(
    name = "com_binomial",
    dpars = c("mu", "nu"),
    links = c("logit", "identity"),
    lb = c(NA, -5),
    ub = c(NA, NA),
    type = "int",
    # `trials[n]` is the per-row binomial denominator, supplied by
    # the `trials()` addition term in the model formula so brms
    # keeps it aligned with the response when rows are dropped.
    # `lfact_com_binomial` is the transformed-data table of log
    # factorials (Stan functions cannot reach data-block globals so
    # the table is passed in as an additional positional arg).
    vars = c("trials[n]", "lfact_com_binomial")
  )
  # See note on `resolve_family_name()`: brms custom_family leaves
  # `family$family = "custom"` and stores the user-visible name on
  # `family$name`. The link helpers brms omits are restored here so
  # mvgam dispatchers can call `family$linkinv(linpred)` directly.
  link_info <- stats::make.link(fam$link)
  fam$linkinv <- link_info$linkinv
  fam$linkfun <- link_info$linkfun
  attr(fam, "mvgam_stanvars") <- make_com_binomial_stanvars()
  fam
}


#' Predicate: family is `com_binomial()`
#'
#' Routes through `resolve_family_name()` so the check survives the
#' brms `family$family = "custom"` convention on customfamily
#' objects.
#'
#' @noRd
is_com_binomial_family <- function(family) {
  if (is.null(family)) return(FALSE)
  identical(resolve_family_name(family), "com_binomial")
}


#' Default population-level priors for `com_binomial()`
#'
#' Returns the `brms::prior` rows mvgam injects ahead of user
#' priors when the obs-side family is `com_binomial()`. Mirrors
#' `default_simplex_population_priors()` and the injection site in
#' `R/make_stan.R::generate_stan_components_mvgam_formula()`.
#'
#' `nu` is the natural parameter of the COM-Binomial exponential
#' family and enters the likelihood linearly, so a normal prior on
#' it regularises the way one on a regression coefficient does. It
#' is centred at `nu = 1`, where the Bernoulli trials making up
#' each count are independent and the distribution is binomial.
#'
#' The scale follows how much the data can say. The response
#' saturates at both ends of `nu`: below about -1 the distribution
#' is already all-or-none, and above about 4 it is nearly a point
#' mass at half the trials, so neighbouring values become
#' indistinguishable. Measured as the Kullback-Leibler divergence
#' between `nu` and `nu + 0.5` at 30 trials, information peaks near
#' `nu = 0` (1.82) and falls to 0.006 by `nu = 3`. A unit scale
#' places 95% of the prior mass on `(-0.96, 2.96)`, covering the
#' range the data can resolve while still pulling in the saturated
#' tails.
#'
#' @noRd
default_com_binomial_population_priors <- function() {
  brms::prior("normal(1, 1)", class = "nu")
}


#' Merge mvgam's default priors with the user's
#'
#' mvgam injects class-level defaults for parameters whose brms
#' fallback is unsuitable. Concatenating them with a user prior that
#' names the same class leaves two rows for it, and brms refuses the
#' whole set with "Duplicated prior specifications are not allowed",
#' so a user who tries to override a default gets an error instead of
#' their prior. Drop any default the user has already spoken for.
#'
#' @param defaults A `brmsprior` of mvgam defaults, from
#'   `response_default_priors()`, possibly `NULL`
#' @param user_prior The user's `brmsprior`, possibly `NULL`
#' @return A `brmsprior` combining both, with no duplicated classes
#' @noRd
merge_default_priors <- function(defaults, user_prior) {
  if (is.null(defaults) || nrow(defaults) == 0L) {
    return(user_prior)
  }
  if (!is.null(user_prior) && nrow(user_prior) > 0L) {
    prior_key <- function(p) paste(p$class, p$coef, p$dpar, p$resp)
    defaults <- defaults[!(prior_key(defaults) %in% prior_key(user_prior)),
                         , drop = FALSE]
  }
  c(defaults, user_prior)
}

#' The default priors mvgam injects, for every response
#'
#' Each response gets the defaults its own family carries, re-aimed
#' where its formula models the dpar they are set on. On a
#' multivariate formula each default names its response, which brms
#' needs to place it. Code generation injects these and `get_prior()`
#' reports them, so the two read one definition.
#'
#' @param formula The observation formula
#' @param family The family given beside it, validated
#' @return A `brmsprior`, or `NULL` when no response's family carries
#'   a default
#' @noRd
response_default_priors <- function(formula, family) {
  forms <- response_formulas(formula)
  families <- formula_families(formula, family)
  rows <- lapply(names(forms), function(key) {
    defaults <- adjust_modelled_dpar_priors(
      family_default_priors(families[[key]]), forms[[key]],
      families[[key]]
    )
    if (is.null(defaults) || nrow(defaults) == 0L) return(NULL)
    if (inherits(formula, "mvbrmsformula")) defaults$resp <- key
    defaults
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  do.call(c, unname(rows))
}

#' Re-aim injected dpar priors when the user models that dpar
#'
#' mvgam injects class-level defaults for family-specific
#' distributional parameters (`nu` for `com_binomial()`, `shape` and
#' `mtail` for `beta_nb()`). Giving one of those a sub-formula replaces
#' the scalar parameter with a design matrix and an intercept, so a
#' prior aimed at the scalar class matches nothing and brms rejects the
#' whole prior set.
#'
#' Where the dpar uses an identity link its intercept is on the same
#' scale as the scalar was, so the default moves across intact. That
#' matters for `nu`, whose name brms already associates with the
#' Student-t degrees of freedom: without the move brms falls back to a
#' positive-only `gamma(2, 0.1)` on a parameter that is free to go
#' negative, and warns about it. Under any other link the intercept
#' lives on a different scale and the default cannot be carried over,
#' so it is dropped and brms's own intercept default applies.
#'
#' @param prior A `brmsprior` of injected defaults
#' @param formula The observation formula, possibly a `brmsformula`
#'   carrying dpar sub-formulas in `$pforms`
#' @param family The family object, consulted for each dpar's link
#' @return `prior`, with rows for modelled dpars re-aimed or removed
#' @noRd
adjust_modelled_dpar_priors <- function(prior, formula, family) {
  if (is.null(prior) || nrow(prior) == 0L) return(prior)
  modelled <- if (inherits(formula, "brmsformula")) {
    names(formula$pforms %||% list())
  } else {
    character(0)
  }
  if (!length(modelled)) return(prior)

  hit <- prior$class %in% modelled
  if (!any(hit)) return(prior)
  identity_link <- vapply(prior$class[hit], function(dp) {
    identical(family[[paste0("link_", dp)]], "identity")
  }, logical(1))

  moved <- which(hit)[identity_link]
  if (length(moved)) {
    prior$dpar[moved] <- prior$class[moved]
    prior$class[moved] <- "Intercept"
  }
  dropped <- which(hit)[!identity_link]
  if (length(dropped)) {
    prior <- prior[-dropped, , drop = FALSE]
  }
  prior
}

#' Default population priors mvgam injects for a family
#'
#' Some families need a class-level default that brms's own
#' fallback would get wrong: `nu` for `com_binomial()` collides
#' with the Student-t degrees of freedom and inherits a
#' positive-only `gamma(2, 0.1)` on a parameter free to go
#' negative; `shape` and `mtail` for `beta_nb()` inherit a
#' negative binomial default that concentrates mass where the
#' Beta mixing scale diverges; the simplex families' shared
#' population effects carry no likelihood information.
#'
#' Stan code generation injects these ahead of user priors and
#' `get_prior()` reports them, so both surfaces resolve one
#' definition and cannot drift apart.
#'
#' @param family The family object.
#' @return A `brmsprior`, or `NULL` for families whose defaults
#'   mvgam leaves entirely to brms.
#' @noRd
family_default_priors <- function(family) {
  if (is_simplex_response_family(family)) {
    default_simplex_population_priors()
  } else if (is_com_binomial_family(family)) {
    default_com_binomial_population_priors()
  } else if (is_beta_nb_family(family)) {
    default_beta_nb_population_priors()
  } else {
    NULL
  }
}


#' Build the Stan stanvars bundle for the `com_binomial()` family
#'
#' Returns the function-block stanvar wrapping `com_binomial_lpmf`
#' plus the transformed-data `lchoose` lookup table. Auto-attached
#' via `attr(family, "mvgam_stanvars")` at construction time. Both
#' blocks are data-independent: the per-row trials arrive through
#' brms's `trials()` addition term and the table sizes itself from
#' them, so nothing here needs the user's data frame.
#'
#' @noRd
make_com_binomial_stanvars <- function(trials = "trials") {
  brms::stanvar(
    name = "com_binomial_funs",
    scode = com_binomial_stan_funs(),
    block = "functions"
  ) +
    brms::stanvar(
      scode = com_binomial_lookup_stan(trials),
      block = "tdata",
      position = "end"
    )
}


#' Transformed-data Stan code building the log-factorial table
#'
#' `lchoose(T, j)` is `lfact[T] - lfact[j] - lfact[T - j]`, so one
#' vector of log factorials up to `max(trials)` serves every `(T, j)`
#' pair. Storing the binomial coefficients themselves would need a
#' `(max(trials) + 1)^2` array, which on a panel with a denominator
#' in the low thousands runs to millions of doubles for information a
#' vector of a few thousand already carries.
#'
#' `lgamma(k + 1)` is evaluated per element rather than accumulated
#' as a running sum of `log(k)`, so rounding error does not grow
#' along the table.
#'
#' The bound is taken from `trials` inside Stan rather than computed
#' in R, so it follows whatever rows brms kept and cannot fall out
#' of step with the response. brms names the denominator of one
#' response of a multivariate model `trials_<resp>`, and one table
#' serves every `com_binomial()` response, so its size is the
#' largest denominator among them.
#'
#' @param trials The Stan names of the denominators the table serves
#' @noRd
com_binomial_lookup_stan <- function(trials = "trials") {
  checkmate::assert_character(trials, min.len = 1L, any.missing = FALSE)
  bound <- Reduce(function(a, b) paste0("max(", a, ", ", b, ")"),
                  paste0("max(", trials, ")"))
  paste(
    paste0("  int max_com_binomial_T = ", bound, ";"),
    "  vector[max_com_binomial_T + 1] lfact_com_binomial;",
    "  for (k_lf in 0 : max_com_binomial_T) {",
    "    lfact_com_binomial[k_lf + 1] = lgamma(k_lf + 1);",
    "  }",
    sep = "\n"
  )
}


#' Stan code for the `com_binomial` lpmf
#'
#' Argument order matches the brms `custom_family(vars =
#' "trials[n]")` calling convention: brms generates calls like
#' `com_binomial_lpmf(Y[n] | mu[n], nu, trials[n])`, so the lpmf
#' signature is `(int y, real mu, real nu, int T)`.
#'
#' Two properties of the kernel keep the cost down. Every factor that
#' does not depend on the outcome `j` is shared by the numerator and
#' by each term of the normaliser, so it cancels; dropping it leaves
#' the natural-parameter form `theta * j - nu * (log j! + log (T-j)!)`
#' with `theta = logit(p)`, and removes one length-`T + 1` autodiff
#' vector operation per row per gradient. The normaliser itself is
#' then summed only over the terms that carry mass, under an explicit
#' bound on what the omitted terms can contribute, which matters
#' because the series is over `T + 1` outcomes and `T` is the
#' binomial denominator: a panel with denominators in the thousands
#' would otherwise pay thousands of autodiff nodes per row.
#'
#' The same remainder-bound reasoning already governs the latent-`N`
#' loop in the `nmix()` families.
#'
#' @noRd
com_binomial_stan_funs <- function() {
  paste(
    "  /* Conway-Maxwell-Binomial log-PMF (Shmueli et al. 2005). */",
    "  /* P(Y = y | T, p, nu) =",
    "       C(T, y)^nu * p^y * (1-p)^(T-y) / Z(T, p, nu),",
    "     Z(T, p, nu) =",
    "       sum_{j=0..T} C(T, j)^nu * p^j * (1-p)^(T-j).",
    "     mu = logit(p); nu identity-link. nu = 1 recovers the binomial.",
    "     Writing theta = log(p) - log(1-p) and expanding C(T, j) into",
    "     log factorials leaves lw(j) below; everything dropped is",
    "     common to the numerator and to every term of Z, so it",
    "     cancels in the ratio.",
    "  */",
    "  real com_binomial_lw(int j, int T, real nu, real theta,",
    "                       data vector lfact) {",
    "    return theta * j - nu * (lfact[j + 1] + lfact[T - j + 1]);",
    "  }",
    "  /* Largest integer in [0, T] not exceeding the continuous mode",
    "     (T + 1) * inv_logit(theta / nu), by binary search: Stan has",
    "     no real-to-int cast, and the argument depends on parameters",
    "     so it cannot be computed in transformed data. */",
    "  int com_binomial_mode(int T, real nu, real theta) {",
    "    real jr = (T + 1) * inv_logit(theta / nu);",
    "    int lo = 0;",
    "    int hi = T;",
    "    while (lo < hi) {",
    "      int mid = (lo + hi + 1) %/% 2;",
    "      if (mid <= jr) {",
    "        lo = mid;",
    "      } else {",
    "        hi = mid - 1;",
    "      }",
    "    }",
    "    return lo;",
    "  }",
    "  /* brms `loop = TRUE` applies the inverse link before",
    "     calling the lpmf, so `mu` arrives here on the",
    "     probability scale (0, 1) -- NOT on the logit scale.",
    "     Use log(mu) / log1m(mu) directly; log_inv_logit(mu)",
    "     would compute log(sigmoid(mu)) which is wrong when",
    "     mu is already a probability. */",
    "  real com_binomial_lpmf(int y, real mu, real nu, int T,",
    "                         data vector lfact) {",
    "    if (y < 0) reject(\"y must be >= 0; got y = \", y);",
    "    if (y > T) reject(\"y must be <= T; got y = \", y, \", T = \", T);",
    "    if (T == 0) {",
    "      return 0;",
    "    }",
    "    real theta = log(mu) - log1m(mu);",
    "    /* Relative tolerance on the omitted mass. Each branch below",
    "       bounds the whole omitted sum, not merely the next term. */",
    "    real leps = log(1e-12);",
    "    real mx;",
    "    real s;",
    "    real f;",
    "    if (nu > 0) {",
    "      /* lw is concave in j, so it has a single mode and its terms",
    "         fall away monotonically on either side. Stopping at j",
    "         therefore leaves at most (remaining count) * exp(lw(j)),",
    "         and the tolerance is split between the two directions. */",
    "      real lhalf = leps - log(2);",
    "      int j0 = com_binomial_mode(T, nu, theta);",
    "      mx = com_binomial_lw(j0, T, nu, theta, lfact);",
    "      s = 1;",
    "      for (k in 1 : (T - j0)) {",
    "        f = com_binomial_lw(j0 + k, T, nu, theta, lfact);",
    "        if (log(T - j0 - k + 1) + f - mx < lhalf) {",
    "          break;",
    "        }",
    "        if (f > mx) {",
    "          s = s * exp(mx - f) + 1;",
    "          mx = f;",
    "        } else {",
    "          s += exp(f - mx);",
    "        }",
    "      }",
    "      for (k in 1 : j0) {",
    "        f = com_binomial_lw(j0 - k, T, nu, theta, lfact);",
    "        if (log(j0 - k + 1) + f - mx < lhalf) {",
    "          break;",
    "        }",
    "        if (f > mx) {",
    "          s = s * exp(mx - f) + 1;",
    "          mx = f;",
    "        } else {",
    "          s += exp(f - mx);",
    "        }",
    "      }",
    "    } else {",
    "      /* lw is convex in j (linear at nu = 0), so its maxima are",
    "         the two endpoints. Whatever interval [L, R] is left",
    "         unsummed is bounded by (R - L + 1) * exp(max(lw(L),",
    "         lw(R))), so the two ends are consumed inward until that",
    "         bound falls below tolerance. */",
    "      real f0 = com_binomial_lw(0, T, nu, theta, lfact);",
    "      real fT = com_binomial_lw(T, T, nu, theta, lfact);",
    "      int L = 1;",
    "      int R = T - 1;",
    "      mx = fmax(f0, fT);",
    "      s = exp(f0 - mx) + exp(fT - mx);",
    "      while (L <= R) {",
    "        real fL = com_binomial_lw(L, T, nu, theta, lfact);",
    "        real fR = com_binomial_lw(R, T, nu, theta, lfact);",
    "        if (log(R - L + 1) + fmax(fL, fR) - mx < leps) {",
    "          break;",
    "        }",
    "        if (fL >= fR) {",
    "          f = fL;",
    "          L += 1;",
    "        } else {",
    "          f = fR;",
    "          R -= 1;",
    "        }",
    "        if (f > mx) {",
    "          s = s * exp(mx - f) + 1;",
    "          mx = f;",
    "        } else {",
    "          s += exp(f - mx);",
    "        }",
    "      }",
    "    }",
    "    return com_binomial_lw(y, T, nu, theta, lfact) - (mx + log(s));",
    "  }",
    sep = "\n"
  )
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
#      arrays (N_unit, n_rep, K_max, Y_max, visit_idx,
#      visit_row, row_unit). Called
#      both at fit time and at predict time with newdata so the
#      cap column can vary per-call.
#   3. `validate_closure_unit_data()`: cap column, integer
#      counts, per-unit `cap >= max(y)`, identifiability
#      warnings.
#   4. Family-specific `make_*_stanvars(arrays, ...)`: Stan
#      function block + per-family data block; emitted at fit
#      time once the unit arrays are known.
#
# `occ()`, `nmix("royle_nichols")` and `nmix("poisson_poisson")`
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


# Internal: whether a family's response is 0/1 rather than a count.
#
# `occ()` and the Royle-Nichols `nmix()` score detections, so their
# response is binary while the latent state is not. Read through an
# accessor for the same reason its five sibling attributes are: a
# bare `attr()` without `exact = TRUE` is a prefix match, and a
# reader cannot tell a family's own record from an incidental
# attribute.
#' @noRd
is_binary_response_family <- function(family) {
  if (is.null(family)) return(FALSE)
  isTRUE(attr(family, "mvgam_binary_response", exact = TRUE))
}


# Internal: the `type` strings a family adds to `predict()` and
# `conditional_effects()` beyond the shared set.
#
# Empty for a family that adds none, so a caller can bind the result
# without checking for NULL first.
#' @noRd
family_predict_types <- function(family) {
  if (is.null(family)) return(character(0L))
  attr(family, "mvgam_predict_types", exact = TRUE) %||% character(0L)
}


# Internal: refuse a closure-unit predict type the family does not
# offer.
#
# Being a closure-unit family is the wire-format question and does
# not settle this one: `mvn()`, `mvt()` and `diri()` share that
# pipeline and expose no latent state. The registry is what knows,
# and asking it here is what keeps every caller to one answer. A
# caller that asked only whether the family was closure-unit let
# those three through to a dispatcher with no branch for them, which
# replied by naming a source file for the user to edit.
#'@noRd
require_closure_unit_predict_type <- function(family, type) {
  if (!is_closure_unit_family(family) ||
        !(type %in% family_predict_types(family))) {
    refuse_unsupported_predict_type(family, type)
  }
  invisible(TRUE)
}


# Internal: refuse a family the internal dispatch has no kernel for.
#
# Reaching one of these means a family was registered without a
# kernel some method needs, which is a fault in mvgam rather than
# anything the caller wrote. Five copies of this refusal each told
# the reader to add a branch to a named function in a named source
# file, which is not an action a user of the package can take and
# reads as an internal note escaping into the console. One message,
# so a new dispatch table cannot reintroduce that.
#'@noRd
refuse_missing_family_dispatch <- function(family_name, what) {
  stop(insight::format_error(c(
    paste0(
      "mvgam cannot compute ", what, " for family '",
      family_name, "'."
    ),
    x = paste0(
      "This family is registered without the internal kernel that ",
      "step requires."
    ),
    i = paste0(
      "This is a fault in mvgam itself. Please ",
      "report it at https://github.com/nicholasjclark/mvgam/issues, ",
      "quoting the family name above."
    )
  )), call. = FALSE)
}


refuse_unsupported_predict_type <- function(family, type) {
  types <- family_predict_types(family)
  # Two different families reach here and they need different
  # sentences. One has no closure unit at all; the other shares the
  # closure-unit pipeline and still models no latent state, which is
  # true of every multi-response family. Reporting both as "not a
  # closure-unit family" told a `diri()` user something false about
  # their own model.
  detail <- if (length(types) > 0L) {
    paste0(
      "Family '", resolve_family_name(family), "' exposes types: ",
      paste(paste0("'", types, "'"), collapse = ", "), "."
    )
  } else if (is_closure_unit_family(family)) {
    paste0(
      "Family '", resolve_family_name(family), "' groups its ",
      "observations into closure units but models no latent state ",
      "over them. It exposes no prediction types of this kind."
    )
  } else {
    paste0(
      "Family '", resolve_family_name(family),
      "' has no closure units. It exposes no types of this kind."
    )
  }
  stop(insight::format_error(c(
    paste0("type = '", type, "' is not available for this family."),
    x = detail,
    i = paste0(
      "Refit with family = nmix() or family = occ() to model a ",
      "latent abundance or occupancy state."
    )
  )), call. = FALSE)
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

#' Does a closure-unit family treat `time` as a season?
#'
#' `occ(multi_season = TRUE)` and `nmix(multi_season = TRUE)` widen
#' the closure-unit key to `(series, site, time)`, so `time` becomes
#' a season the latent state is indexed by rather than the unit
#' identifier it is in a single-season design. The grouping's arity
#' is what records that, since it is the same fact the likelihood is
#' built on; asking the family for a `multi_season` attribute reads
#' something the constructors never set and is always false.
#'
#' @param family A `brmsfamily` (or family-like list).
#' @return `TRUE` when the closure-unit key names three columns.
#' @noRd
is_multi_season_family <- function(family) {
  length(closure_unit_grouping(family)) >= 3L
}

#' The columns that key a closure unit, default included
#'
#' `closure_unit_grouping()` reports what the family declares, which
#' is `NULL` for a family that takes a default. There are two
#' defaults, and stating them at each caller is how five places came
#' to hold the same fact: an array builder, a validator, two array
#' rebuilds and this accessor, each free to drift from the others.
#' This is where the fact lives; everything else asks.
#'
#' A detection family keys on `(series, time)`, so a unit's rows are
#' the repeat visits to one series. A multi-response family keys on
#' the time alone, so a unit is a site and its rows are the K
#' response components measured there. The difference decides
#' whether a per-series cut of a frame preserves units or destroys
#' them: it preserves them for the detection families, and for a
#' composition it leaves one row per unit, which is a simplex of
#' width one whose single probability is 1 by construction.
#'
#' Answers `NULL` for a family that has no closure unit at all, so a
#' caller can tell "not a closure-unit family" from "the default
#' key" and word its own refusal. `family = NULL` asks for the
#' detection default without a family to consult, which is what the
#' builder and the validator need when a caller names no grouping.
#'
#' @param family A `brmsfamily` (or family-like list), or `NULL` to
#'   ask for the default key alone.
#' @param series_var,time_var The column names the defaults are
#'   built from.
#' @return Character vector of column names, or `NULL`.
#' @noRd
closure_unit_key_vars <- function(family, series_var = "series",
                                  time_var = "time") {
  declared <- closure_unit_grouping(family)
  if (!is.null(declared)) {
    return(declared)
  }
  if (!is.null(family)) {
    if (!is_closure_unit_family(family)) {
      return(NULL)
    }
    if (is_multi_response_family(family)) {
      return(time_var)
    }
  }
  c(series_var, time_var)
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

# Internal: the series axis a closure-unit fit was built on.
#
# For a multi-response family these levels are the response
# components a unit holds one row of, the K species of a composition
# or the K responses of a multivariate normal. For a detection family
# they are the series whose `(series, time)` pairs are the units.
#
# The axis answers this, not the frame's own column. Which component
# a row carries decides which entry of `Psi` it is scored against,
# which is a question about the model: a frame whose series column
# was superseded by a grouping, or which never saw one of the
# components, re-factors into a different numbering while the fit's
# own numbering stays put. `extract_mv_response_components()` pairs
# rows with `Psi` through the same accessor, so a grid completed here
# and a density taken there cannot disagree about which species a row
# belongs to.
#'@noRd
closure_unit_axis_levels <- function(object) {
  levels(axis_row_series(object, mvgam_training_data(object)))
}


# Internal: does this frame already carry intact closure units?
#
# `marginaleffects` reaches `get_predict.mvgam()` with either the
# frame the model was fitted on or a synthetic grid, and the two need
# opposite treatment: a real frame is predicted as it stands, a grid
# has to be given the unit structure its family needs. Different
# evidence settles it at the two grains, because the two kinds of
# frame carry different columns.
#
# A composition or a multivariate response keys its units by time
# alone and fills each with one row per component, which is checkable
# on the frame itself: every unit holds each component exactly once.
# `datagrid()` pins the key at one typical value rather than dropping
# it, so its rows fall into a single unit holding many rows of one
# component. That fails the test whether or not the key is present,
# which a test counting units does not.
#
# A detection unit is a `(series, time)` pair with repeat visits, and
# no structural test separates one visit of each of three units from
# three visits of one. `visit` settles it instead: no model formula
# names that column, so `datagrid()` cannot invent it, and a frame
# carrying it was either assembled by a caller who meant the units it
# describes or replicated wholesale from the training data.
#'@noRd
closure_units_are_intact <- function(object, newdata) {
  key <- closure_unit_key_vars(object$family)
  if (is.null(key) || !all(key %in% names(newdata))) {
    return(FALSE)
  }
  if (!is_multi_response_family(object$family)) {
    return("visit" %in% names(newdata))
  }
  levs <- closure_unit_axis_levels(object)
  component <- as.integer(axis_row_series(object, newdata))
  if (length(levs) < 2L || length(component) != nrow(newdata) ||
        anyNA(component)) {
    return(FALSE)
  }
  idx <- closure_unit_index(newdata, key)
  nrow(newdata) == length(idx$levels) * length(levs) &&
    !anyDuplicated(paste(idx$unit, component))
}


# Internal: complete a composition's prediction grid to whole sites.
#
# A softmax spans the K categories of a site, so a grid row carrying
# one category is a simplex of width one, whose only probability is
# 1 whatever the linear predictor holds. Every panel a composition
# drew was therefore a flat line at one with a zero-width interval.
# Each distinct covariate setting in the grid is completed to the K
# category rows of one synthetic site, and the caller takes back the
# rows the grid asked about.
#
# Grouping the grid's own rows into units is not enough, though it
# looks like it should be. A main-effect grid holds the categories
# together at its first covariate value and then one category across
# the remaining values, so grouping leaves every later setting a
# unit of one.
#
# Only a synthetic grid reaches here, which
# `closure_units_are_intact()` decides, so the K-fold widening is
# paid on a few hundred rows at most and a real frame is returned
# untouched. A real frame needs no completion anyway: it already
# carries the K rows of each site.
#
# @return `NULL` when the family needs no completion; otherwise a
#   list with `data`, the completed grid, and `take`, one index per
#   row of `newdata` into `data`.
#'@noRd
complete_simplex_grid <- function(object, newdata,
                                  is_grid = !closure_units_are_intact(
                                    object, newdata
                                  )) {
  if (!is_simplex_response_family(object$family) || !is_grid) {
    return(NULL)
  }
  levs <- closure_unit_axis_levels(object)
  if (length(levs) < 2L) {
    return(NULL)
  }
  resp <- response_column(object)
  # The covariate setting of a row is everything that is not the
  # category axis, the unit identifiers, or the response.
  held <- setdiff(names(newdata),
                  c("series", "time", "visit", "cap", "rowid", resp))
  key <- if (length(held)) {
    do.call(paste, c(lapply(held, function(v) {
      as.character(newdata[[v]])
    }), list(sep = "\r")))
  } else {
    rep("1", nrow(newdata))
  }
  settings <- unique(key)
  first_row <- match(settings, key)
  n_set <- length(settings)
  K <- length(levs)
  # One block of K rows per setting, carrying that setting's
  # covariates and sharing a unit identifier.
  out <- newdata[rep(first_row, each = K), , drop = FALSE]
  out$series <- factor(rep(levs, times = n_set), levels = levs)
  out$time <- rep(seq_len(n_set), each = K)
  out$visit <- 1L
  out[[resp]] <- 1 / K
  rownames(out) <- NULL
  # Which completed row each original row asked about. The component
  # is read off the axis for the reason `closure_unit_axis_levels()`
  # gives: the categories are the model's, and `out$series` above is
  # built from those, so the index back into it has to be resolved
  # the same way.
  asked <- as.integer(axis_row_series(object, newdata))
  take <- (match(key, settings) - 1L) * K + asked
  if (anyNA(take)) {
    stop(insight::format_error(c(
      "A prediction grid names a category the model does not have.",
      x = paste0(
        "Unknown: ",
        paste(unique(setdiff(as.character(newdata$series), levs)),
              collapse = ", "), "."
      ),
      i = paste0("The model's categories are: ",
                 paste(levs, collapse = ", "), ".")
    )), call. = FALSE)
  }
  list(data = out, take = take)
}


# Fill missing closure-unit identifier columns on an incoming
# newdata so synthetic prediction grids (e.g. those built by
# `marginaleffects::datagrid()`, which drops every column the
# model formula does not reference) round-trip through the per-
# unit prediction pipeline.
#
# The natural interpretation of a per-row marginaleffects grid on
# a closure-unit fit is "each row is one hypothetical single-
# visit closure unit". To get that, we stamp:
#   * `series` to the first training level (held constant so the
#     state intercept stays interpretable across the grid).
#   * `time` to `seq_len(nrow(newdata))` so each row has a
#     distinct unit identifier; `build_closure_unit_arrays()`
#     then treats the rows as `N_grid` independent units of
#     1 visit each. Per-row state-level covariate variation
#     produces per-row state predictions as intended.
#   * `visit` to `1L`.
#   * the response column to `0L`, since a synthetic unit has no
#     observation behind it, which also satisfies
#     `validate_closure_unit_data()`'s integer / binary checks;
#     the response is never consumed by `posterior_epred()` /
#     `posterior_predict()` for these families.
#
# `cap` and the response are filled on any frame that omits them,
# grid or not, because the pipeline reads both whatever the frame
# is. `cap` takes the family's `mvgam_default_cap` attribute or,
# failing that, the first training row's value.
#
# The unit structure is left alone (`newdata` returned with those
# fills alone) when:
#   * the family is not a closure-unit family, or
#   * `newdata` is NULL, or
#   * the fit has no `data` slot to source defaults from, or
#   * the frame already carries intact units, which
#     `closure_units_are_intact()` decides.
#
# Used by `get_predict.mvgam` so every marginaleffects entry
# point (`predictions`, `slopes`, `comparisons`, `plot_predictions`,
# `conditional_effects`) works on closure-unit fits out of the
# box.
#'@noRd
complete_closure_unit_newdata <- function(object, newdata,
                                          is_grid = !closure_units_are_intact(
                                            object, newdata
                                          )) {
  if (is.null(newdata)) return(newdata)
  if (!is_closure_unit_family(object$family)) return(newdata)
  data <- mvgam_training_data(object) %||% data.frame()
  if (nrow(data) == 0L) return(newdata)
  template <- data[1L, , drop = FALSE]
  # The response is resolved unguarded, as it is at every other
  # post-fit closure-unit site: a fit of this family reached here
  # only by resolving a single response at fit time, so a failure
  # would be a broken object rather than a case to fall back on.
  resp <- response_column(object)
  # A column the frame does not carry is filled whichever kind of
  # frame this is. `cap` is the upper truncation the unit arrays
  # need, and the response is read by the binary / non-negative
  # integer validator rather than by any prediction, so a frame that
  # omits either is completed rather than refused.
  if (!"cap" %in% names(newdata)) {
    cap_val <- closure_unit_default_cap(object$family) %||%
                 template$cap %||% 1L
    newdata$cap <- as.integer(rep(cap_val, nrow(newdata)))
  }
  if (!resp %in% names(newdata)) {
    newdata[[resp]] <- rep(0L, nrow(newdata))
  }
  # What remains is the unit structure itself, and only a synthetic
  # grid needs it built. Real long-format newdata keeps the labels it
  # arrived with, which forecasting and multi-season fits depend on.
  if (!is_grid) return(newdata)
  if (!"series" %in% names(newdata)) {
    # The levels come from the axis rather than from the training
    # column, which carries none of its own when the user supplied a
    # character series and orders them differently when a grouping
    # superseded it.
    newdata$series <- factor(
      as.character(template$series),
      levels = closure_unit_axis_levels(object)
    )
  }
  # On the grid path, every row should be its own closure unit
  # so each prediction varies the covariate independently.
  # `datagrid()` pins `time` at a single typical value drawn from
  # training, which would otherwise collapse the whole grid into
  # one unit and flatten the predicted curve.
  newdata$time  <- seq_len(nrow(newdata))
  newdata$visit <- 1L
  # A grid's response is whatever value `datagrid()` held it at, and
  # a synthetic unit has no observation behind it, so the safe
  # default is stamped over it here rather than filled in.
  newdata[[resp]] <- rep(0L, nrow(newdata))
  newdata
}


# Internal: which closure unit each row belongs to.
#
# The grouping columns are integer-coded before they are pasted, so
# columns of any type combine and the separator cannot collide: an
# integer code never contains an underscore, while the values behind
# it can.
#
# `response_var` is what separates a guard from a description. A
# missing response is a visit that did not happen, and brms drops
# those rows from the likelihood, so counts taken over the raw frame
# describe a model that was never fitted. Naming the response makes
# `n_rep` the observed visit count and leaves a unit with none at
# zero.
#' @noRd
closure_unit_index <- function(data, unit_grouping_vars,
                               response_var = NULL) {
  codes <- lapply(unit_grouping_vars, function(col) {
    as.integer(as.factor(data[[col]]))
  })
  label <- do.call(paste, c(codes, list(sep = "_")))
  levels <- unique(label)
  unit <- match(label, levels)
  observed <- if (is.null(response_var)) {
    rep(TRUE, NROW(data))
  } else {
    !is.na(data[[response_var]])
  }
  list(
    unit = unit,
    label = label,
    levels = levels,
    observed = observed,
    n_rep = tabulate(unit[observed], nbins = length(levels))
  )
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
#' @param drop_unobserved_units Logical. A unit whose every response
#'   is missing contributes no density, and Stan declares `n_rep` with
#'   a lower bound of one, so the fit-time call drops such units and
#'   refuses a frame that has nothing else. A prediction frame is the
#'   opposite case: a forecast grid carries no response at all, and
#'   every one of its rows still has a linear predictor and so still
#'   has an expected value. `closure_unit_arrays_for()` therefore
#'   passes `FALSE` and keeps every unit, leaving `n_rep` at zero for
#'   the ones nothing was recorded in.
#' @param unit_grouping_vars Character vector of column names that
#'   jointly identify a closure unit. Defaults to
#'   `c(series_var, time_var)` for the count-based detection-error
#'   families (each (species, site) pair is its own closure unit
#'   with repeated visits). Multi-response families set this to
#'   `time_var` only: one closure unit per site, with the K species
#'   rows treated as the "visits" within that unit. The
#'   `visit_row` matrix then maps each (unit, k) pair to the row
#'   index of the k-th species row at that site.
#' @return Named list with elements `N_unit`, `n_rep`, `K_max`,
#'   `Y_max`, `visit_idx`, `visit_row`, `row_unit`, `max_rep`,
#'   `unit_labels`, `unit_grid` and `unit_vars`. `K_max` and `Y_max`
#'   are `NA` when `compute_y_max = FALSE`. `unit_vars` is the
#'   grouping this call resolved, so a consumer reads the axis the
#'   arrays were built on rather than repeating the default.
#'
#'   `visit_idx` and `visit_row` index the same visits in two
#'   coordinate systems and are not interchangeable. `visit_idx`
#'   numbers the rows brms retained, which is what the Stan data
#'   indexes because brms drops a row whose response is missing.
#'   `visit_row` numbers the rows of the frame as supplied, which
#'   is what every post-fit path needs, since `posterior_linpred()`
#'   answers once per row of `newdata`. `row_unit` gives the unit
#'   of every row, including a visit that never happened: that
#'   visit has no density but still has an expected value.
#' @noRd
build_closure_unit_arrays <- function(data,
                                       response_var,
                                       series_var  = "series",
                                       time_var    = "time",
                                       cap_var     = "cap",
                                       default_cap = NULL,
                                       default_cap_buffer = NULL,
                                       compute_y_max = TRUE,
                                       unit_grouping_vars = NULL,
                                       drop_unobserved_units = TRUE) {
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
  checkmate::assert_flag(drop_unobserved_units)
  if (is.null(unit_grouping_vars)) {
    # No family is in scope here, so the accessor is asked for the
    # default key alone. Restating it produced a copy that never
    # learned the multi-response key.
    unit_grouping_vars <- closure_unit_key_vars(
      NULL, series_var = series_var, time_var = time_var
    )
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
  idx         <- closure_unit_index(data, unit_grouping_vars,
                                    response_var)
  unit_label  <- idx$label
  unit_levels <- idx$levels
  unit_int    <- idx$unit
  n_unit      <- length(unit_levels)
  # The grouping values behind each unit, one row per unit in unit
  # order. Callers that label units, such as
  # `hindcast(type = "latent_state")`, must read this rather than
  # rebuilding the grid and sorting by series: this ordering is
  # first-appearance over time-major data, so sorting by series
  # would relabel every unit on a multi-series fit.
  unit_grid <- data[match(unit_levels, unit_label),
                    unit_grouping_vars, drop = FALSE]
  rownames(unit_grid) <- NULL
  # Per-unit visit counts indexed in `unit_levels` order. `table`
  # would re-order alphabetically; an explicit tabulate keeps
  # the unit ordering deterministic.
  # A missing response is a visit that did not happen. brms drops
  # those rows from the likelihood, so the unit arrays are built
  # over the observed visits and `visit_idx` holds positions in
  # brms's retained rows rather than rows of the raw frame. Units
  # left with no observed visit carry no information about
  # detection and are dropped; their timepoints still appear in
  # the latent process, which spans the full grid regardless.
  observed <- idx$observed
  retained_pos <- cumsum(observed)
  rows_by_unit <- lapply(
    seq_len(n_unit), function(g) which(unit_int == g & observed)
  )
  visited <- lengths(rows_by_unit) > 0L
  if (drop_unobserved_units) {
    if (!any(visited)) {
      stop(insight::format_error(c(
        "Every closure unit has only missing responses.",
        i = paste0(
          "Closure-unit families need at least one observed visit ",
          "in at least one unit."
        )
      )))
    }
    rows_by_unit <- rows_by_unit[visited]
    unit_levels  <- unit_levels[visited]
    unit_grid    <- unit_grid[visited, , drop = FALSE]
    rownames(unit_grid) <- NULL
    n_unit       <- length(unit_levels)
  } else {
    visited <- rep(TRUE, n_unit)
  }
  # The key above is integer-coded so that grouping columns of any
  # type can be pasted together, which makes it useless as a label:
  # a reader given `1_1` cannot get back to the site whose survey
  # effort it describes. The label pastes the values the user
  # supplied instead, one per unit in unit order, and is what every
  # surface reporting one row per unit prints.
  unit_labels <- do.call(
    paste, c(lapply(unit_grid, as.character), list(sep = "_"))
  )
  rep_counts   <- lengths(rows_by_unit)
  # At least one padding column, so a frame carrying no response at
  # all still returns matrices of the documented shape rather than
  # zero-column ones. A fit-time frame always has an observed unit,
  # so this changes nothing there.
  max_rep <- max(rep_counts, 1L)
  # Two coordinate systems, and they are not interchangeable.
  # `visit_idx` numbers the visits brms retained, which is what the
  # Stan data indexes because brms drops a row whose response is
  # missing. `visit_row` numbers the rows of the frame as supplied,
  # which is what every post-fit path needs: `posterior_linpred()`
  # answers for each row of `newdata`, and the response column is
  # read from that frame too. Mixing them shifts a unit's visits by
  # the number of missing responses before it, so both are returned
  # and each caller names the one it means.
  visit_idx <- matrix(1L, nrow = n_unit, ncol = max_rep)
  visit_row <- matrix(NA_integer_, nrow = n_unit, ncol = max_rep)
  for (g in seq_len(n_unit)) {
    visit_idx[g, seq_len(rep_counts[g])] <-
      as.integer(retained_pos[rows_by_unit[[g]]])
    visit_row[g, seq_len(rep_counts[g])] <-
      as.integer(rows_by_unit[[g]])
  }
  # Which response component each of a unit's rows carries. Only a
  # unit whose rows are the components rather than repeat visits has
  # one, which is what a key omitting the series var means. The
  # generated lpdf reads it to pair a row with that component's
  # residual scale; pairing by position instead is right only while
  # every unit holds every component in order, so a site missing one
  # species shifted every later species onto another's scale.
  visit_component <- NULL
  if (!series_var %in% unit_grouping_vars &&
        series_var %in% names(data)) {
    series_col <- data[[series_var]]
    component <- if (is.factor(series_col)) {
      as.integer(series_col)
    } else {
      as.integer(factor(series_col))
    }
    visit_component <- matrix(1L, nrow = n_unit, ncol = max_rep)
    for (g in seq_len(n_unit)) {
      visit_component[g, seq_len(rep_counts[g])] <-
        component[rows_by_unit[[g]]]
    }
  }
  # Which unit each row of the frame belongs to, over every row
  # rather than the observed ones alone. A visit that never happened
  # still has a linear predictor and so still has an expected value;
  # only its density is absent. Deriving this here, where the units
  # are formed, is what lets a per-visit surface be broadcast from a
  # per-unit quantity without reconstructing the grouping. `NA`
  # marks a row whose unit carried no observed visit at all and was
  # dropped above.
  row_unit <- match(unit_int, which(visited))
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
      visit_component = visit_component,
      visit_row   = visit_row,
      row_unit    = row_unit,
      max_rep     = as.integer(max_rep),
      unit_labels = unit_labels,
      unit_grid   = unit_grid,
      unit_vars   = unit_grouping_vars
    ))
  }
  # Missing entries stay in `y_vals` but are never indexed: every
  # per-unit lookup below runs over `rows_by_unit`, which holds
  # observed visits only.
  y_vals <- as.integer(data[[response_var]])
  # The floor the closure-unit lpmf marginalises N up from. Computed
  # once for all three cap branches below, which differ only in the
  # ceiling. A unit with no observed visit has no floor, which
  # `max()` of nothing reports as `-Inf` with a warning; zero is what
  # "nothing was recorded" means here.
  Y_max <- vapply(rows_by_unit, function(rows_g) {
    if (length(rows_g)) max(y_vals[rows_g]) else 0L
  }, integer(1L))
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
    # The cap bounds a unit's latent state, so it is a property of the
    # unit and is carried on every one of its rows. Read over the
    # observed rows alone it was empty for a unit nothing was recorded
    # in, and `K_max[g]` became `NA`, which the comparison below then
    # met as a missing condition rather than as a number.
    rows_of_unit <- closure_unit_row_split(
      list(row_unit = row_unit, N_unit = n_unit)
    )
    for (g in seq_len(n_unit)) {
      rows_g <- rows_of_unit[[g]]
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
    K_max <- Y_max + as.integer(default_cap_buffer)
  } else {
    # Falls back to the scalar default_cap; NULL here means the
    # validator should have refused the call upstream.
    K_max <- rep(as.integer(default_cap), n_unit)
  }
  list(
    N_unit      = n_unit,
    n_rep       = rep_counts,
    K_max       = K_max,
    Y_max       = Y_max,
    visit_idx   = visit_idx,
    visit_row   = visit_row,
    row_unit    = row_unit,
    max_rep     = as.integer(max_rep),
    unit_labels = unit_labels,
    unit_grid   = unit_grid,
    unit_vars   = unit_grouping_vars
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
#'   Per-unit `Y_max[g]` and the K_max buffer (`Y_max[g] + 100` for
#'   PB and PPM by default; static `K_max = 25` for RN) are
#'   computed from the visits in unit `g` only. With few visits per
#'   (site, season) and a highly abundant species, that cap may
#'   bind and downward-bias `lambda`; inspect `standata(fit)$K_max`
#'   against `posterior_latent_N()` / `latent_N_saturation(fit)`
#'   after fitting and raise `cap` if needed. Sparse visits +
#'   low detection probability also produces prior-dominated
#'   posteriors; check marginal posteriors against the prior.
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
#'     Grainsize is set to `max(1, N_unit %/% 8)` so single-
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
#' @examples
#' \dontrun{
#' # Poisson-binomial N-mixture: per-visit counts with imperfect
#' # detection. The state mu is lambda (expected abundance) and
#' # the dpar p is the detection probability per visit.
#' set.seed(5)
#' simdat <- sim_closure_unit_data(family = nmix(), n_species = 1L,
#'                                   n_sites = 50L, n_visits = 4L,
#'                                   type = 1L)
#'
#' mod <- mvgam(
#'   bf(y ~ env, p ~ tod_c),
#'   data    = simdat$data_train,
#'   family  = nmix(),
#'   chains  = 2,
#'   silent  = 2
#' )
#' summary(mod, include_betas = FALSE)
#'
#' # Marginal env effect on the response scale (expected count
#' # = lambda * p).
#' conditional_effects(mod)
#'
#' # Closure-unit post-fit surface. `predict(type = "latent_state")`
#' # returns posterior summaries of latent abundance N at each
#' # unit; `predict(type = "detection")` returns the per-visit
#' # detection probability p. These two quantities are confounded
#' # in the raw response and only separable under the N-mixture
#' # marginalisation.
#' head(predict(mod, type = "latent_state"))
#' head(predict(mod, type = "detection"))
#'
#' # `hindcast(type = "latent_state")` packs the full per-draw
#' # posterior at every training unit into an `mvgam_latent_state`
#' # object with print / summary / as.data.frame / plot methods.
#' # `summary()` returns a tidy (series, time, median, 50% and 95%
#' # CI) frame for direct ggplot use; `plot()` returns a faceted
#' # step-style ribbon that can be extended with extra ggplot
#' # layers (e.g. `+ geom_point(aes(time, true_N))` to overlay a
#' # known truth).
#' hc <- hindcast(mod, type = "latent_state")
#' print(hc)
#' head(as.data.frame(hc))
#' plot(hc)
#'
#' # `pp_check(type = "fit_stat", ...)` runs a closure-unit
#' # goodness-of-fit test: collapse to per-unit sufficient
#' # statistics, compare observed vs replicated under either the
#' # chi-squared or Freeman-Tukey discrepancy of MacKenzie & Bailey
#' # (2004). Returns an `mvgam_ppc_fit_stat` object with the
#' # Bayesian p-value; values near 0 or 1 indicate poor fit.
#' pp_check(mod, type = "fit_stat", stat = "freeman_tukey",
#'           ndraws = 200L)
#' pp_check(mod, type = "fit_stat", stat = "chi_squared",
#'           ndraws = 200L)
#' }
#'
#' @seealso [occ()] for the related joint-occupancy family,
#'   [pp_check.mvgam()] for the closure-unit goodness-of-fit
#'   surface. The online article at
#'   \url{https://nicholasjclark.github.io/mvgam/articles/nmix.html}
#'   introduces N-mixture and joint-occupancy modelling in mvgam
#'   end-to-end; see also \code{vignette("data", package = "mvgam")}
#'   for the closure-unit data-shape requirements that `nmix()`
#'   consumes.
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
#'     (\url{https://github.com/ecoverseR/ubms}):
#'     `predict(submodel = "state")` returns marginal psi;
#'     `predict(submodel = "det")` returns p;
#'     `posterior_predict(param = "z")` returns 0/1 latent
#'     occupancy draws conditioned on the observed history.
#'   \item spOccupancy
#'     (\url{https://github.com/ecoverseR/spOccupancy}):
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
#' returns the per-visit detection probability \eqn{p_{g,j}} on the
#' response scale.
#'
#' @param multi_season Logical. When `TRUE`, closure units are
#'   defined by `(series, site, time)` instead of the default
#'   `(series, time)`. The data must carry a `site` column;
#'   produce it from a 4-axis observation array with
#'   [pivot_detection_array()] (set `multi_season = "hierarchical"`).
#'   The per-unit visits shrink to those within a single
#'   (site, season): with few visits per unit and low detection
#'   probability the posterior occupancy can become prior-dominated;
#'   check marginal posteriors against the prior (e.g. via
#'   `pp_check(fit)`) and consider a hierarchical detection sub-
#'   formula (`p ~ ...`) to share detection information across units.
#'
#' @examples
#' \dontrun{
#' # Single-species occupancy fit with separate covariates on
#' # the occupancy (psi) and detection (p) sub-formulas. The
#' # closure-unit grain is (series, time) -- 50 sites, 4 visits
#' # each.
#' set.seed(4)
#' simdat <- sim_closure_unit_data(family = occ(), n_species = 1L,
#'                                   n_sites = 50L, n_visits = 4L,
#'                                   type = 1L)
#'
#' mod <- mvgam(
#'   bf(y ~ env, p ~ tod_c),
#'   data    = simdat$data_train,
#'   family  = occ(),
#'   chains  = 2,
#'   silent  = 2
#' )
#' summary(mod, include_betas = FALSE)
#'
#' # Marginal env effect on the response scale (occupancy *
#' # detection). For the logit-occupancy view pass
#' # `type = "link"`.
#' conditional_effects(mod)
#'
#' # Closure-unit post-fit surface. `predict(type = "latent_state")`
#' # returns posterior summaries of the latent occupancy probability
#' # psi at each unit; `predict(type = "detection")` returns the
#' # per-visit detection probability p. Both are confounded in the
#' # raw 0/1 record and only separable under the marginalised
#' # occupancy likelihood.
#' head(predict(mod, type = "latent_state"))
#' head(predict(mod, type = "detection"))
#'
#' # `hindcast(type = "latent_state")` returns the full per-draw
#' # posterior on psi as an `mvgam_latent_state` object with
#' # `as.data.frame()` and a faceted `plot()` for direct inspection
#' # and ggplot composition.
#' hc <- hindcast(mod, type = "latent_state")
#' print(hc)
#' plot(hc)
#'
#' # `pp_check(type = "fit_stat", ...)` runs a closure-unit
#' # goodness-of-fit test: collapse to per-unit sufficient
#' # statistics, compare observed vs replicated under the
#' # Freeman-Tukey or chi-squared discrepancy of MacKenzie &
#' # Bailey (2004). Returns an `mvgam_ppc_fit_stat` object with
#' # the Bayesian p-value; values near 0 or 1 indicate poor fit.
#' pp_check(mod, type = "fit_stat", stat = "freeman_tukey",
#'           ndraws = 200L)
#' }
#'
#' @seealso [nmix()] for the related N-mixture family,
#'   [pp_check.mvgam()] for closure-unit goodness-of-fit. The
#'   online article at
#'   \url{https://nicholasjclark.github.io/mvgam/articles/nmix.html}
#'   covers both occupancy and N-mixture workflows; the
#'   closure-unit data-shape requirements that `occ()` consumes
#'   are documented in \code{vignette("data", package = "mvgam")}.
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
    "    int grainsize = N_unit >= 8 ? N_unit %/% 8 : 1;",
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
#' row 1's value, since there is no pre-fit validator for this
#' contract.
#'
#' Post-fit response-scale dispatchers
#' (`posterior_predict()` / `posterior_epred()` / `log_lik()`)
#' extract the per-row `phi` linpred via `extract_phi_per_row()`
#' when a `phi` sub-formula is present, composing it through the
#' same dpar pipeline as `mu` and collapsing per unit to
#' `phi[idx[1]]` to mirror the Stan lpdf.
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
#' and the remaining K - 1 rows return `NA` for `log_lik()`, which
#' is how a row carrying no density of its own is spelled: a `0`
#' there would assert a density of one, and `loo()` would score the
#' site once per species. This is
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
#' # Simulate Dirichlet compositional data on 40 sites x K = 3
#' # categories under an environmental gradient. The first category
#' # is the reference; per-category logit-mean offsets and env
#' # slopes are encoded relative to it.
#' set.seed(2)
#' K <- 3L; cat_names <- paste0("y", seq_len(K)); n_sites <- 40L
#' env <- rnorm(n_sites)
#' intercept_true <- c(0, 0.6, -0.4)
#' beta_env_true  <- c(0, 0.8, -0.6)
#' phi_true <- 12
#' mu_true <- outer(rep(1, n_sites), intercept_true) +
#'              outer(env, beta_env_true)
#' mu_true[, 1L] <- 0
#' probs <- t(apply(mu_true, 1L, function(z) {
#'   z <- exp(z); z / sum(z)
#' }))
#' Y_wide <- t(apply(probs, 1L, function(p) {
#'   d <- rgamma(K, shape = p * phi_true, rate = 1)
#'   d / sum(d)
#' }))
#' Y_wide <- pmax(Y_wide, 1e-4)
#' Y_wide <- Y_wide / rowSums(Y_wide)
#' colnames(Y_wide) <- cat_names
#'
#' # Reshape to the long-format closure-unit layout: K rows per
#' # site in (time, species) order with `series = category`.
#' long_dat <- data.frame(
#'   y      = as.numeric(t(Y_wide)),
#'   series = factor(rep(cat_names, n_sites), levels = cat_names),
#'   time   = rep(seq_len(n_sites), each = K),
#'   env    = rep(env, each = K)
#' )
#'
#' # Fit the compositional JSDM. The `env * series` interaction
#' # makes the environmental slope category-specific, matching
#' # brms-native Dirichlet's per-category linear predictors. `n_lv`
#' # adds a rank-2 latent-factor correction over the population
#' # mean structure.
#' fit <- jsdgam(
#'   formula        = y ~ env * series,
#'   factor_formula = ~ -1,
#'   data           = long_dat,
#'   unit           = time, species = series,
#'   family         = diri(),
#'   n_lv           = 2L,
#'   chains         = 2L,
#'   iter           = 800L, warmup = 400L,
#'   silent         = 2,
#'   backend        = "cmdstanr"
#' )
#'
#' # Per-category offsets recover their truth; the K-shared
#' # `b_Intercept` and `b_env` are not likelihood-identified under
#' # the simplex reference subtraction and sample from their prior.
#' draws <- posterior::as_draws_matrix(fit)
#' draws[, c("b_seriesy2", "b_seriesy3",
#'           "b_env:seriesy2", "b_env:seriesy3")] |>
#'   posterior::summarise_draws(median, ~quantile(.x, c(0.05, 0.95)))
#'
#' # posterior_epred returns one probability per (site, species)
#' # summing to 1 within each site.
#' pe <- posterior_epred(fit, ndraws = 50L)
#' first_site <- which(long_dat$time == 1L)
#' rowSums(pe[1L:5L, first_site, drop = FALSE])
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
#' # Simulate K = 3 species at 60 sites under a low-rank residual
#' # covariance Sigma = Z Z' + diag(Psi^2). Per-species intercepts
#' # and an env slope drive the mean; cross-species covariance comes
#' # entirely from the factor structure.
#' set.seed(1)
#' K <- 3L; species_levels <- paste0("y", seq_len(K)); N_lv <- 2L
#' Z_true <- matrix(rnorm(K * N_lv, sd = 1.0), nrow = K, ncol = N_lv)
#' psi_true <- rep(0.5, K)
#' sigma_true <- tcrossprod(Z_true) + diag(psi_true^2)
#' mu_intercept <- c(0.0, 0.6, -0.4)
#' mu_env_slope <- c(0.3, 0.8, -0.5)
#'
#' n_sites <- 60L; env <- rnorm(n_sites)
#' L_true <- chol(sigma_true)
#' Y_wide <- matrix(NA_real_, n_sites, K)
#' for (i in seq_len(n_sites)) {
#'   mu_i <- mu_intercept + mu_env_slope * env[i]
#'   Y_wide[i, ] <- mu_i + as.numeric(crossprod(L_true, rnorm(K)))
#' }
#' colnames(Y_wide) <- species_levels
#'
#' # Reshape to the long-format closure-unit layout: K rows per
#' # site in (time, species) order with `series = species`.
#' long_dat <- data.frame(
#'   y      = as.numeric(t(Y_wide)),
#'   series = factor(rep(species_levels, n_sites),
#'                   levels = species_levels),
#'   time   = rep(seq_len(n_sites), each = K),
#'   env    = rep(env, each = K)
#' )
#'
#' # Fit the gllvm-style JSDM with a rank-2 factor structure on the
#' # residual covariance.
#' fit <- jsdgam(
#'   formula        = y ~ env * series,
#'   factor_formula = ~ -1,
#'   data           = long_dat,
#'   unit           = time, species = series,
#'   family         = mvn(),
#'   n_lv           = 2L,
#'   chains         = 2L,
#'   iter           = 800L, warmup = 400L,
#'   silent         = 2,
#'   backend        = "cmdstanr"
#' )
#'
#' # Per-species intercepts and env slopes recover their truth.
#' draws <- posterior::as_draws_matrix(fit)
#' draws[, c("b_seriesy2", "b_seriesy3",
#'           "b_env:seriesy2", "b_env:seriesy3")] |>
#'   posterior::summarise_draws(median, ~quantile(.x, c(0.05, 0.95)))
#'
#' # The headline diagnostic: implied residual covariance recovers
#' # the off-diagonal Sigma the data was drawn under.
#' res_cor <- residual_cor(fit)
#' cor(sigma_true[upper.tri(sigma_true)],
#'     res_cor$cov[upper.tri(res_cor$cov)])
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
    "N_unit", "n_rep", "visit_idx", "visit_component", "Psi"
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
    "    array[,] int visit_component,",
    "    vector Psi) {",
    "    real lp = 0;",
    "    for (g in 1:N_unit) {",
    "      int Kg = n_rep[g];",
    "      array[Kg] int idx = visit_idx[g, 1:Kg];",
    "      vector[Kg] y_unit  = y[idx];",
    "      vector[Kg] mu_unit = mu[idx];",
    "      vector[Kg] psi_unit = Psi[visit_component[g, 1:Kg]];",
    "      lp += normal_lpdf(y_unit | mu_unit, psi_unit);",
    "    }",
    "    return lp;",
    "  }",
    sep = "\n"
  )
}

#' Does this family estimate a residual scale per response component?
#'
#' TRUE for `mvn()` and `mvt()`, which declare
#' `vector<lower=0>[K] Psi` alongside the loadings, so a unit's
#' covariance is `Z Sigma Z' + diag(Psi^2)`. FALSE for the simplex
#' families, whose spread is carried by the softmax and, for
#' `diri()`, by a single concentration; and FALSE for the detection
#' families, which have no continuous residual at all.
#'
#' Asked wherever the split between the factor part and the
#' idiosyncratic part matters, which is why it is composed from the
#' two grain predicates rather than listing the families again.
#'
#' @param family A family / brmsfamily / customfamily object.
#' @return TRUE or FALSE.
#' @noRd
family_has_component_scale <- function(family) {
  is_multi_response_family(family) &&
    !is_simplex_response_family(family)
}


#' The largest factor count whose idiosyncratic split is identified
#'
#' A rank-`m` covariance over `K` components carries
#' `K m - m (m - 1) / 2` free values, and one residual scale per
#' component adds another `K`. The observed covariance has
#' `K (K + 1) / 2` distinct entries, so the split into a factor part
#' and an idiosyncratic part is identified only when
#'
#'   `K m - m (m - 1) / 2 + K <= K (K + 1) / 2`,
#'
#' which rearranges to `(K - m)^2 >= K + m`. This is the counting
#' condition, so meeting it does not guarantee the posterior is
#' well behaved; failing it does guarantee a direction the
#' likelihood cannot see.
#'
#' @param n_species Number of response components.
#' @return The largest admissible factor count, or 0 when even one
#'   factor is too many.
#' @noRd
identified_factor_ceiling <- function(n_species) {
  m <- seq_len(max(n_species, 1L))
  ok <- (n_species - m)^2 >= n_species + m
  if (!any(ok)) return(0L)
  as.integer(max(m[ok]))
}


#' Warn when the idiosyncratic split is not identified
#'
#' `mvn()` and `mvt()` estimate a residual scale per component
#' beside the loadings. Where `identified_factor_ceiling()` says the
#' requested factor count is too high, the data cannot separate the
#' two, and the posterior for `Psi` is whatever the prior says along
#' that direction however much data arrives.
#'
#' A warning rather than a refusal: the covariance the model
#' reports, which is what `residual_cor()` and `shared_variation()`
#' summarise, is estimated well in that regime and can be estimated
#' better than at a factor count the bound admits. What cannot be
#' read is the per-component split.
#'
#' @param n_lv Requested factor count.
#' @param n_species Number of response components.
#' @param family The observation family.
#' @return `invisible(NULL)`.
#' @noRd
warn_unidentified_component_scale <- function(n_lv, n_species,
                                              family) {
  if (!family_has_component_scale(family)) return(invisible(NULL))
  n_lv <- as.integer(n_lv)
  n_species <- as.integer(n_species)
  if ((n_species - n_lv)^2 >= n_species + n_lv) return(invisible(NULL))
  ceiling_lv <- identified_factor_ceiling(n_species)
  advice <- if (ceiling_lv >= 1L) {
    paste0("Use 'n_lv = ", ceiling_lv, "' or fewer to estimate ",
           "the residual scales, or add species.")
  } else {
    paste0("No factor count separates the two at ", n_species,
           " species; add species to estimate the residual scales.")
  }
  insight::format_warning(c(
    paste0(
      "The per-species residual scale is not identified at 'n_lv = ",
      n_lv, "' with ", n_species, " species."
    ),
    x = paste0(
      "Family '", resolve_family_name(family), "' splits each site's ",
      "covariance into a factor part and a per-species part, and ",
      "that split needs (n_species - n_lv)^2 >= n_species + n_lv."
    ),
    i = paste0(
      "'residual_cor()', 'shared_variation()' and the predictions ",
      "read the combined covariance and are unaffected. 'Psi' is ",
      "not estimable per species here. A poor Rhat on it reflects ",
      "this design. It does not signal a sampler fault."
    ),
    i = advice
  ))
  invisible(NULL)
}


#' The residual scale shared by the mvn and mvt families
#'
#' Both declare one positive scale per response component and give
#' it the same prior, so the declaration and the prior are written
#' once here rather than separately in each builder. `Psi` is on
#' the standard-deviation scale, which is what
#' `posterior_epred()` and the variance formulas expect.
#'
#' @param K Number of response components per closure unit.
#' @param prefix Stanvar name prefix, `"mvn"` or `"mvt"`.
#' @return A list of two `brms::stanvar` objects.
#' @noRd
make_psi_stanvars <- function(K, prefix) {
  checkmate::assert_int(K, lower = 1)
  checkmate::assert_choice(prefix, c("mvn", "mvt"))
  list(
    param = brms::stanvar(
      name  = paste0(prefix, "_Psi_param"),
      scode = paste0("  vector<lower=0>[", K, "] Psi;"),
      block = "parameters"
    ),
    prior = brms::stanvar(
      name  = paste0(prefix, "_Psi_prior"),
      scode = "  Psi ~ exponential(1);",
      block = "model"
    )
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
  psi <- make_psi_stanvars(K, "mvn")
  psi_param <- psi$param
  psi_prior <- psi$prior
  combine_stanvars(
    make_closure_unit_arrays_stanvars(
      arrays,
      family_funs_name = "mvn_funs",
      family_funs      = mvn_stan_funs(),
      include_K_max    = FALSE,
      include_Y_max    = FALSE,
      include_component = TRUE
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
    "N_unit", "n_rep", "visit_idx", "visit_component", "Psi", "nu"
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
    "    array[,] int visit_component,",
    "    vector Psi,",
    "    real nu) {",
    "    real lp = 0;",
    "    for (g in 1:N_unit) {",
    "      int Kg = n_rep[g];",
    "      array[Kg] int idx = visit_idx[g, 1:Kg];",
    "      vector[Kg] y_unit  = y[idx];",
    "      vector[Kg] mu_unit = mu[idx];",
    "      vector[Kg] psi_unit = Psi[visit_component[g, 1:Kg]];",
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
  psi <- make_psi_stanvars(K, "mvt")
  psi_param <- psi$param
  psi_prior <- psi$prior
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
      include_Y_max    = FALSE,
      include_component = TRUE
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
    "    int grainsize = N_unit >= 8 ? N_unit %/% 8 : 1;",
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
    "    int grainsize = N_unit >= 8 ? N_unit %/% 8 : 1;",
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
    "    int grainsize = N_unit >= 8 ? N_unit %/% 8 : 1;",
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
                                               include_Y_max = TRUE,
                                               include_component = FALSE) {
  checkmate::assert_list(arrays, names = "named")
  checkmate::assert_string(family_funs_name)
  checkmate::assert_string(family_funs)
  checkmate::assert_integerish(y_max_upper, len = 1L, lower = 0L,
                               null.ok = FALSE)
  checkmate::assert_flag(include_K_max)
  checkmate::assert_flag(include_Y_max)
  checkmate::assert_flag(include_component)
  required <- c("N_unit", "n_rep", "visit_idx", "max_rep")
  if (include_component) required <- c(required, "visit_component")
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
  if (include_component) {
    # Emitted in the family's own bundle rather than read from the
    # trend block's `obs_trend_series`, which carries the same fact:
    # the family stanvars are parsed before the trend data lands, so
    # that identifier is not yet in scope here.
    stanvars <- stanvars +
      brms::stanvar(
        x     = arrays$visit_component,
        name  = "visit_component",
        scode = paste0(
          "array[N_unit, ", arrays$max_rep,
          "] int<lower=1> visit_component;"
        ),
        block = "data"
      )
  }
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
  # Which columns key a unit is the family's own fact, so it is read
  # rather than restated. The accessor knows that a multi-response
  # unit is a site keyed by `time` alone, its rows being the K
  # response components, while a detection unit is keyed by
  # `(series, time)` with replicate visits inside.
  unit_grouping_vars <- closure_unit_key_vars(family)
  if (!multi_response) {
    # cap is required only when neither a scalar default nor a
    # data-driven buffer is configured. Count families (PB / PPM)
    # carry `mvgam_default_cap_buffer = 100L` so the validator
    # accepts data without an explicit cap column. A multi-response
    # family skips the integer-y / cap validation entirely: its
    # per-unit likelihood reads the K response components directly,
    # without per-unit truncation.
    validate_closure_unit_data(
      data,
      response_var       = response_var,
      has_obs_covariates = has_obs_covariates,
      has_det_covariates = has_det_covariates,
      binary_y_check     = binary_y_check,
      cap_required       = is.null(default_cap) &&
                            is.null(default_cap_buffer),
      default_cap        = default_cap,
      unit_grouping_vars = unit_grouping_vars
    )
  }
  arrays <- build_closure_unit_arrays(
    data, response_var = response_var,
    default_cap        = default_cap,
    default_cap_buffer = default_cap_buffer,
    compute_y_max      = !multi_response,
    unit_grouping_vars = unit_grouping_vars
  )
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
    refuse_missing_family_dispatch(family_name, "its Stan code")
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
        insight::format_message(c(
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

#' Merge the responses' custom-family stanvars into the user's
#'
#' Custom families built via [tweedie()] attach their function-
#' block helpers + any data inputs in
#' `attr(family, "mvgam_stanvars")`. This helper concatenates
#' them with any stanvars the user passed via `mvgam(stanvars =
#' ...)` so the brms call downstream sees a single combined
#' object. Built-in brms families have no attached stanvars and
#' pass through unchanged.
#'
#' Every response's family contributes, since a custom family named
#' by one response of a multivariate formula needs its functions as
#' much as one given beside the formula. Responses sharing a family
#' share its functions and data, which the program declares once;
#' given different arguments they would declare one Stan name twice,
#' which is refused. `com_binomial()` sizes its table from each of
#' its responses' denominators, which brms suffixes by response.
#'
#' @param stanvars Existing stanvars object or NULL.
#' @param families A list of `brmsfamily` / `customfamily` objects,
#'   one per response and named by response key, as
#'   `formula_families()` returns them.
#' @return A `brmsstanvars` object (or NULL when neither side
#'   contributes anything).
#' @noRd
attach_family_stanvars <- function(stanvars, families) {
  checkmate::assert_list(families, min.len = 1L, names = "named")
  suffixes <- if (length(families) > 1L) paste0("_", names(families)) else ""
  served <- split(seq_along(families),
                  vapply(families, resolve_family_name, character(1L)))
  sets <- lapply(names(served), function(name) {
    i <- served[[name]]
    own <- lapply(families[i], attr, which = "mvgam_stanvars", exact = TRUE)
    if (any(!vapply(own[-1L], identical, logical(1L), own[[1L]]))) {
      stop(insight::format_error(c(
        paste0("Two responses give '", name, "()' different arguments."),
        x = "Its Stan data are declared once and shared between them.",
        i = paste0("Give '", name, "()' the same arguments in every ",
                   "response.")
      )), call. = FALSE)
    }
    if (is_com_binomial_family(families[[i[1L]]])) {
      return(make_com_binomial_stanvars(paste0("trials", suffixes[i])))
    }
    own[[1L]]
  })
  sets <- Filter(Negate(is.null), sets)
  for (fam_stanvars in sets) {
    checkmate::assert_class(fam_stanvars, "stanvars")
    stanvars <- if (is.null(stanvars)) {
      fam_stanvars
    } else {
      checkmate::assert_class(stanvars, "stanvars")
      stanvars + fam_stanvars
    }
  }
  stanvars
}

#' Pointwise log-likelihood for the beta negative binomial family
#'
#' Discovered by name from `dispatch_log_lik()`, so no registration
#' is needed beyond defining it here. Evaluates the closed-form
#' log-pmf rather than round-tripping through Stan, which keeps
#' `log_lik()` usable on refits and subsets.
#'
#' @param linpred `[ndraws x nobs]` matrix of linear predictors
#' @param link Link name; the family restricts this to `"log"`
#' @param y Numeric vector of observed counts, length `nobs`
#' @param family_pars Named list holding `shape` and `mtail`, each an
#'   `[ndraws x nobs]` matrix
#' @param trials Unused; present for the dispatcher's fixed signature
#' @return `[ndraws x nobs]` matrix of log densities
#' @noRd
log_lik_beta_nb <- function(linpred, link, y, family_pars, trials) {
  checkmate::assert_matrix(linpred)
  # beta_nb() restricts the response surface to the log link, which
  # is what keeps `mu` strictly positive.
  checkmate::assert_choice(link, "log")
  mu <- .linkinv(linpred, link)
  shape <- family_pars$shape
  mtail <- family_pars$mtail
  checkmate::assert_matrix(shape, nrows = nrow(linpred),
                           ncols = ncol(linpred))
  checkmate::assert_matrix(mtail, nrows = nrow(linpred),
                           ncols = ncol(linpred))
  # `dbeta_nb_mvgam()` recycles elementwise, so a single call over the
  # whole matrix is enough once `y` is broadcast across draws.
  y_mat <- matrix(y, nrow = nrow(linpred), ncol = ncol(linpred),
                  byrow = TRUE)
  matrix(
    dbeta_nb_mvgam(y_mat, mu, shape, mtail, log = TRUE),
    nrow = nrow(linpred), ncol = ncol(linpred)
  )
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
# Conway-Maxwell-Binomial R-side post-fit kernels
# ============================================================
# R-side numerical helpers ported from upstream contributor
# jbogomolovas2 (also author of the TMB COM-Binomial solver and
# the in-flight glmmTMB integration). The Stan-side lpmf is the
# in-fit work; these helpers run post-fit for `log_lik`,
# `posterior_predict`, `posterior_epred`, and `residuals` over
# all (draw x observation) cells. All are vectorised: each unique
# `T` value triggers one chunked matrix evaluation rather than
# (ndraws * nobs) scalar lpmf calls.


#' Numerically-stable log-sum-exp on a numeric vector
#' @noRd
.cmb_lse <- function(v) {
  m <- max(v)
  m + log(sum(exp(v - m)))
}


# Per-T cache of `lchoose(T, 0:T)`. The values depend only on `T`
# and are invariant across mu, nu and every posterior draw, so
# memoising once per unique trial count avoids re-computing the
# lgamma terms millions of times during post-processing. The
# environment-as-namespace pattern is the same `cmb_lchoose_env`
# the contributor used upstream.
#' @noRd
.cmb_lchoose_env <- new.env(parent = emptyenv())

#' Cached `lchoose(T, 0:T)` for a single trials value
#' @noRd
.cmb_lchoose <- function(T) {
  key <- as.character(T)
  v <- .cmb_lchoose_env[[key]]
  if (is.null(v)) {
    v <- lchoose(T, 0:T)
    .cmb_lchoose_env[[key]] <- v
  }
  v
}


#' Row-wise maximum of a numeric matrix (one entry per row)
#'
#' Picks the max value per row via `max.col`, then reads it back
#' via row + col indexing. Same idea as `matrixStats::rowMaxs` but
#' without the extra dependency.
#' @noRd
.cmb_rowmax <- function(m) {
  m[cbind(seq_len(nrow(m)), max.col(m, ties.method = "first"))]
}


#' COM-Binomial PMF on the support `0:T` for a single (mu, nu, T)
#'
#' Returns a `T + 1` vector of probabilities. Used by `.cmb_cdf`
#' for the Dunn-Smyth residual quantile computation. Vector hot
#' paths (mean / var / pmf summed log densities / random draws)
#' use the dedicated `*_vec` helpers below; this scalar form
#' exists for the per-observation CDF loop where vectorising over
#' the support is the right granularity.
#' @noRd
cmb_pmf <- function(mu, nu, T) {
  theta <- qlogis(mu)
  lc <- .cmb_lchoose(T)
  x <- 0:T
  lw <- nu * lc + theta * x
  exp(lw - .cmb_lse(lw))
}


#' Vectorised COM-Binomial log-PMF over (draw x obs) cells
#'
#' @param y Integer vector of observed counts (length n).
#' @param mu Numeric vector on probability scale (length n).
#' @param nu Numeric vector of dispersion exponents (length n).
#' @param T Integer vector of trial counts (length n).
#' @return Numeric vector of log densities (length n).
#'
#' Groups rows by unique `T` and evaluates each group as a single
#' matrix op (one `lse` per row of the chunk), chunked so one
#' large-T group never allocates an oversized matrix. Numerically
#' identical to a scalar loop over `cmb_lpmf` but ~100x faster on
#' typical (4000 draws) x (1000 obs) post-fit sweeps.
#' @noRd
cmb_lpmf_vec <- function(y, mu, nu, T) {
  theta <- qlogis(mu)
  out <- numeric(length(y))
  for (TT in unique(T)) {
    ia <- which(T == TT)
    lc <- .cmb_lchoose(TT)
    x <- 0:TT
    chunk <- max(1L, as.integer(5e6 %/% (TT + 1L)))
    for (s in seq(1L, length(ia), by = chunk)) {
      idx <- ia[s:min(s + chunk - 1L, length(ia))]
      lw <- outer(nu[idx], lc) + outer(theta[idx], x)
      mx <- .cmb_rowmax(lw)
      lse <- mx + log(rowSums(exp(lw - mx)))
      out[idx] <- theta[idx] * y[idx] + nu[idx] * lc[y[idx] + 1L] -
        lse
    }
  }
  out
}


#' Vectorised COM-Binomial mean `E[Y]` (count-scale)
#'
#' Returns per-row `sum_j j * P(j | mu, nu, T)`. The
#' dispatcher above the kernel is free to scale; the
#' `posterior_epred` surface keeps count-scale, the
#' `predict(type = "variance")` surface divides Var by T^2 to
#' match the binomial proportion-scale convention.
#' @noRd
cmb_mean_vec <- function(mu, nu, T) {
  theta <- qlogis(mu)
  out <- numeric(length(mu))
  for (TT in unique(T)) {
    ia <- which(T == TT)
    lc <- .cmb_lchoose(TT)
    x <- 0:TT
    chunk <- max(1L, as.integer(5e6 %/% (TT + 1L)))
    for (s in seq(1L, length(ia), by = chunk)) {
      idx <- ia[s:min(s + chunk - 1L, length(ia))]
      lw <- outer(nu[idx], lc) + outer(theta[idx], x)
      w <- exp(lw - .cmb_rowmax(lw))
      w <- w / rowSums(w)
      out[idx] <- as.numeric(w %*% x)
    }
  }
  out
}


#' Vectorised COM-Binomial random draw
#'
#' Returns per-row integer draws from `CMB(mu, nu, T)`. Uses
#' inverse-CDF on the cumulative weighted PMF; ties broken by
#' `max.col(..., ties.method = "first")` so the inversion is
#' stable for point-mass mass functions (large positive `nu`).
#' @noRd
rcmb_vec <- function(mu, nu, T) {
  theta <- qlogis(mu)
  out <- integer(length(mu))
  for (TT in unique(T)) {
    ia <- which(T == TT)
    lc <- .cmb_lchoose(TT)
    x <- 0:TT
    chunk <- max(1L, as.integer(5e6 %/% (TT + 1L)))
    for (s in seq(1L, length(ia), by = chunk)) {
      idx <- ia[s:min(s + chunk - 1L, length(ia))]
      lw <- outer(nu[idx], lc) + outer(theta[idx], x)
      w <- exp(lw - .cmb_rowmax(lw))
      w <- w / rowSums(w)
      cw <- w
      if (TT >= 1L) {
        for (j in 2:(TT + 1L)) cw[, j] <- cw[, j - 1L] + w[, j]
      }
      u <- stats::runif(length(idx))
      out[idx] <- max.col(cw >= u, ties.method = "first") - 1L
    }
  }
  out
}


#' COM-Binomial CDF `P(X <= q)` for randomised quantile residuals
#'
#' Per-observation kernel. Handles the `q = -1` lower-bound edge
#' case at `y = 0` (`floor(-1 + 1e-9) = -1`, return 0) so the
#' Dunn-Smyth residual lower interval is `[0, P(Y = 0)]` and not
#' a degenerate window. This is the off-by-one the contributor's
#' `a1c6ddf` commit fixed.
#' @noRd
.cmb_cdf <- function(q, mu, nu, size) {
  vapply(seq_along(q), function(i) {
    kk <- floor(q[i] + 1e-9)
    if (kk < 0) return(0)
    if (kk >= size[i]) return(1)
    sum(cmb_pmf(mu[i], nu[i], size[i])[seq_len(kk + 1L)])
  }, numeric(1))
}


#' Compound Poisson-gamma distribution and density for `tweedie()`
#'
#' A Tweedie with `1 < p < 2` is a Poisson sum of gamma variates, so
#' its distribution function is the Poisson-weighted sum of gamma
#' distribution functions plus a point mass at zero. The series is
#' truncated where the Poisson tail is negligible, which is the same
#' construction the Stan-side log-density uses.
#'
#' `tweedie::ptweedie()` computes the same quantity and agrees with
#' this to 1.5e-11 across a grid of `mu`, `phi` and `p`, but it takes
#' the index parameter as a single value. mvgam samples the index, so
#' a per-draw distribution function through that route needs one call
#' per draw and costs about 190 seconds where the series costs one.
#'
#' The density is `mgcv::ldTweedie()`, which is what `log_lik()`
#' already reads, so a Tweedie has one density rather than two.
#'
#' @param q,x Quantile, recycled against the parameters.
#' @param power Numeric vector of Tweedie index parameters in (1, 2).
#' @param mu Numeric vector of means.
#' @param phi Numeric vector of dispersions.
#' @param lower.tail,log.p,log As for any distribution function.
#' @return Numeric vector the length of the recycled arguments.
#' @noRd
ptweedie_cpg <- function(q, power, mu, phi, lower.tail = TRUE,
                         log.p = FALSE) {
  n <- max(length(q), length(power), length(mu), length(phi))
  q <- rep_len(q, n)
  power <- rep_len(power, n)
  mu <- rep_len(mu, n)
  phi <- rep_len(phi, n)
  lambda <- mu^(2 - power) / (phi * (2 - power))
  alpha <- (2 - power) / (power - 1)
  rate <- 1 / (phi * (power - 1) * mu^(power - 1))
  # The k = 0 term is the mass at zero, which every quantile from
  # zero upwards carries.
  out <- exp(-lambda)
  n_terms <- max(stats::qpois(1 - 1e-12, max(lambda, na.rm = TRUE)), 5L)
  for (k in seq_len(n_terms)) {
    out <- out + stats::dpois(k, lambda) *
      stats::pgamma(q, shape = k * alpha, rate = rate)
  }
  out[!is.na(q) & q < 0] <- 0
  out <- pmin(pmax(out, 0), 1)
  if (!lower.tail) out <- 1 - out
  if (log.p) log(out) else out
}


#' @rdname ptweedie_cpg
#' @noRd
dtweedie_cpg <- function(x, power, mu, phi, log = FALSE) {
  n <- max(length(x), length(power), length(mu), length(phi))
  ld <- mgcv::ldTweedie(
    y = rep_len(x, n), mu = rep_len(mu, n),
    p = rep_len(power, n), phi = rep_len(phi, n)
  )[, 1L]
  if (log) ld else exp(ld)
}


#' COM-Binomial distribution and density functions
#'
#' `family_dist_spec()` names one R distribution per family and hands
#' its callers a `p` and a `d` for it. COM-binomial has neither in
#' base R, so the two vectorised kernels the package already carries
#' are given those signatures here: `.cmb_cdf()` for the
#' distribution and `cmb_lpmf_vec()` for the density. That makes
#' `com_binomial()` reachable from everything reading a family
#' through the spec, which is the randomised quantile residual and
#' the `cens()` and `trunc()` terms.
#'
#' @param q,x Quantile, recycled against the parameters.
#' @param mu Numeric vector on the probability scale.
#' @param nu Numeric vector of dispersion exponents.
#' @param size Integer vector of trial counts.
#' @param lower.tail,log.p,log As for any distribution function.
#' @return Numeric vector the length of the recycled arguments.
#' @noRd
pcmb <- function(q, mu, nu, size, lower.tail = TRUE, log.p = FALSE) {
  n <- max(length(q), length(mu), length(nu), length(size))
  p <- .cmb_cdf(rep_len(q, n), mu = rep_len(mu, n),
                nu = rep_len(nu, n), size = rep_len(size, n))
  if (!lower.tail) p <- 1 - p
  if (log.p) log(p) else p
}


#' @rdname pcmb
#' @noRd
dcmb <- function(x, mu, nu, size, log = FALSE) {
  n <- max(length(x), length(mu), length(nu), length(size))
  ld <- cmb_lpmf_vec(rep_len(x, n), mu = rep_len(mu, n),
                     nu = rep_len(nu, n), T = rep_len(size, n))
  if (log) ld else exp(ld)
}


# ---- Post-fit dispatchers for the v2 generic surface ----

#' R-side `log_lik` for `com_binomial()`
#'
#' Signature matches `log_lik_tweedie()`: vector y / per-row
#' trials, per-draw `(linpred, nu)` matrices. Returns the standard
#' `[ndraws x nobs]` log-density matrix the loo / waic / pp_check
#' machinery expects.
#' @noRd
log_lik_com_binomial <- function(linpred, link, y,
                                  family_pars, trials) {
  checkmate::assert_matrix(linpred)
  checkmate::assert_choice(link, "logit")
  checkmate::assert_numeric(trials, lower = 0L, len = ncol(linpred))
  nu <- family_pars$nu
  checkmate::assert_matrix(nu, nrows = nrow(linpred),
                            ncols = ncol(linpred))
  ndraws <- nrow(linpred)
  nobs <- ncol(linpred)
  # Flatten to column-major (R default): rows fastest, then cols.
  # `outer(nu, lc) + outer(theta, x)` inside `cmb_lpmf_vec`
  # evaluates each unique `T` as a single block.
  mu_flat <- as.numeric(.linkinv(linpred, link))
  nu_flat <- as.numeric(nu)
  y_flat <- rep(y, each = ndraws)
  T_flat <- rep(trials, each = ndraws)
  out_flat <- cmb_lpmf_vec(y_flat, mu_flat, nu_flat, T_flat)
  matrix(out_flat, nrow = ndraws, ncol = nobs)
}


#' R-side `posterior_predict` for `com_binomial()`
#'
#' Returns the standard `[ndraws x nobs]` integer matrix of
#' posterior predictive draws. Used by the
#' `sample_from_family("com_binomial", ...)` branch in
#' `R/posterior_predict.R` and by `predict(type = "response")`.
#' @noRd
posterior_predict_com_binomial <- function(linpred, link,
                                            family_pars, trials) {
  checkmate::assert_matrix(linpred)
  checkmate::assert_choice(link, "logit")
  checkmate::assert_numeric(trials, lower = 0L, len = ncol(linpred))
  nu <- family_pars$nu
  checkmate::assert_matrix(nu, nrows = nrow(linpred),
                            ncols = ncol(linpred))
  ndraws <- nrow(linpred)
  nobs <- ncol(linpred)
  mu_flat <- as.numeric(.linkinv(linpred, link))
  nu_flat <- as.numeric(nu)
  T_flat <- rep(trials, each = ndraws)
  out_flat <- rcmb_vec(mu_flat, nu_flat, T_flat)
  matrix(as.integer(out_flat), nrow = ndraws, ncol = nobs)
}


#' R-side `posterior_epred` for `com_binomial()`
#'
#' Returns count-scale `E[Y | mu, nu, T] = sum_j j * P(j | mu, nu,
#' T)` over all (draw x obs) cells. Matches the binomial /
#' beta_binomial convention of returning the count-scale mean
#' (multiplied by trials), so `posterior_epred()` returns the
#' same units regardless of which trials-aware family produced
#' the fit.
#'
#' Takes the `prep` list every other mean kernel takes, so
#' `family_mean_from_kernel()` can call it the way it calls the
#' rest. `prep$dpars$mu` arrives on the response scale already,
#' which is why no link is applied here and why the signature
#' could not simply be renamed: reading a link-scale predictor out
#' of `mu` would invert it a second time and return a wrong mean
#' with nothing to report.
#' @noRd
posterior_epred_com_binomial <- function(prep) {
  mu <- prep$dpars$mu
  checkmate::assert_matrix(mu)
  nu <- prep$dpars$nu
  checkmate::assert_matrix(nu, nrows = nrow(mu), ncols = ncol(mu))
  if (is.null(prep$data$trials)) {
    stop(insight::format_error(c(
      "Family 'com_binomial' requires the 'trials' data entry.",
      i = "Pass a per-row trials vector to posterior_epred()."
    )))
  }
  trials <- data2draws(prep$data$trials, dim_mu(prep))
  ey_flat <- cmb_mean_vec(as.numeric(mu), as.numeric(nu),
                           as.numeric(trials))
  matrix(ey_flat, nrow = nrow(mu), ncol = ncol(mu))
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
    refuse_missing_family_dispatch(
      family_name, paste0("a '", method_kind, "' surface")
    )
  }
  fn
}

#' Materialise a draw count as concrete draw indices
#'
#' A prediction assembles its answer from several extractions: the
#' linear predictor, the distributional parameters, the latent states.
#' Asked for a count rather than for indices, each of those subsamples
#' on its own, and they do not agree: some take the first `ndraws`
#' rows, others draw at random. The result pairs a dispersion with a
#' mean from an unrelated iteration. Resolving the count to indices
#' once, before any extraction runs, makes every part of the answer
#' come from the same iterations.
#'
#' A count always resolves to indices, even one covering the whole
#' posterior: the extractions subsample by drawing at random, and
#' asking for every draw returns them in a random order rather than in
#' the order they were sampled, so two extractions given the same
#' count would still disagree. `NULL` comes back only when there is
#' genuinely nothing to choose, meaning no count was asked for.
#'
#' @param object An `mvgam` model object
#' @param ndraws Requested number of draws, or `NULL`
#' @param draw_ids Draw indices the caller already has, or `NULL`
#' @return An integer vector of indices, or `NULL`
#'
#' @noRd
resolve_draw_ids <- function(object, ndraws, draw_ids) {
  if (!is.null(draw_ids) || is.null(ndraws)) {
    return(draw_ids)
  }
  resolve_draw_indices(
    posterior::ndraws(posterior::as_draws_matrix(object$fit)),
    ndraws = ndraws, draw_ids = NULL
  )
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
    draw_ids <- resolve_draw_indices(nrow(draws_mat), ndraws, NULL)
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
  # Detection probability is one more parameter carrying a formula of
  # its own, so it is rebuilt the same way as any other. `nobs` is left
  # open because closure-unit families work at visit grain, which the
  # caller reconciles against the unit grain.
  predicted_dpar_draws(
    object, "p", newdata = newdata, draw_ids = draw_ids
  )
}

#' The closure-unit arrays for a fit, over a frame
#'
#' Every post-fit path that needs the unit layout rebuilds it from
#' the fit's own family and formula. Assembling those arguments at
#' each call site is what let two of them leave out
#' `unit_grouping_vars`: a multi-season fit then grouped on
#' `(series, time)` and dropped the site axis, answering on 48
#' units where its likelihood has 480. Deriving them in one place
#' means a caller cannot leave one out.
#'
#' For a multi-response family the K response components of a site
#' are its "visits", so `compute_y_max` is FALSE: there is no
#' per-unit truncation, which is also what makes the two cap
#' declarations inert rather than needing a branch of their own.
#'
#' Both of the family's cap declarations are passed, not one. A
#' count family carries a data-driven buffer rather than a fixed
#' cap, so handing over only the fixed one left the rebuild here
#' with neither and it demanded a `cap` column the fit never needed.
#'
#' @param object A fitted `mvgam` with a closure-unit family.
#' @param newdata Frame to build over; defaults to the fit's data.
#' @return The list `build_closure_unit_arrays()` returns.
#' @noRd
closure_unit_arrays_for <- function(object, newdata = NULL) {
  checkmate::assert_class(object, "mvgam")
  # The frame the model was fitted on, through the accessor that
  # owns that question rather than one of its two spellings.
  newdata <- newdata %||% mvgam_training_data(object)
  fam <- object$family
  build_closure_unit_arrays(
    newdata,
    response_var = response_column(object),
    default_cap = closure_unit_default_cap(fam),
    default_cap_buffer = closure_unit_default_cap_buffer(fam),
    compute_y_max = !is_multi_response_family(fam),
    unit_grouping_vars = closure_unit_key_vars(fam),
    drop_unobserved_units = FALSE
  )
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
                                             draw_ids = NULL,
                                             linpred = NULL) {
  checkmate::assert_class(object, "mvgam")
  if (!is_closure_unit_family(object$family)) {
    stop(insight::format_error(c(
      "extract_closure_unit_components() requires a closure-unit fit.",
      i = "Use family = nmix() or family = occ()."
    )))
  }
  newdata <- newdata %||% mvgam_training_data(object)
  if (is.null(newdata)) {
    stop(insight::format_error(
      "Training data not stored on object; supply 'newdata'."
    ))
  }
  response_var <- response_column(object)
  binary_y_check <- is_binary_response_family(object$family)
  default_cap <- closure_unit_default_cap(object$family)
  default_cap_buffer <- closure_unit_default_cap_buffer(object$family)
  # Re-run validation on newdata so cap edits (nmix) or non-binary
  # y (occ, royle_nichols) raise the friendly error rather than
  # producing silent garbage in downstream sampling. The
  # identifiability checks are switched off rather than talked out of:
  # they ask whether a design can separate state from detection, which
  # is a question about a fit, and claiming covariates the frame was
  # never asked about silenced two of the three while leaving the
  # third to refuse a caller predicting at one site.
  # Mv-response families (mvn / mvt) read continuous responses
  # without a per-unit truncation; they share the closure-unit
  # data layout (long format, K rows per site) but neither
  # require a cap column nor run the binary-y check.
  aggregates <- needs_closure_unit_aggregation(object$family)
  validate_closure_unit_data(
    newdata,
    response_var       = response_var,
    identifiability    = FALSE,
    binary_y_check     = binary_y_check && aggregates,
    # A family declaring a data-driven buffer needs no `cap` column
    # here for the same reason it needed none at fit time, so both
    # declarations are read rather than only the fixed one.
    cap_required       = is.null(default_cap) &&
                           is.null(default_cap_buffer) && aggregates,
    default_cap        = default_cap
  )
  arrays <- closure_unit_arrays_for(object, newdata)
  # Per-visit linpred for the state quantity. The linpred is
  # constant within a closure unit because the formula is on
  # site-level covariates; drop the redundant columns to one per
  # unit and carry the unit-grain state to all downstream methods.
  # Per-visit broadcasting happens only at the response sampling
  # step, where it is unavoidable.
  #
  # A caller holding its own predictor supplies it. A forecast arm
  # propagates the latent state forward and holds a predictor no
  # frame reproduces, so recomputing one here would answer for a
  # different quantity. `process_error = FALSE` on the recomputed
  # path reads the state the sampler fitted, which is what every
  # training-grid surface wants.
  if (is.null(linpred)) {
    linpred <- posterior_linpred(
      object, newdata = newdata, draw_ids = draw_ids,
      process_error = FALSE
    )
  }
  state_visit <- object$family$linkinv(linpred)
  ndraws   <- nrow(state_visit)
  n_visit  <- ncol(state_visit)
  n_unit   <- arrays$N_unit
  # The frame's own row, because `state_visit` has one column per
  # row of `newdata`. Reading `visit_idx` here shifted a unit's
  # first visit by the number of missing responses before it.
  state <- state_visit[, closure_unit_first_rows(arrays), drop = FALSE]
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
# Internal: a closure unit's expectation, read at the visit grain.
#
# The unit carries one latent state -- an abundance for `nmix()`, an
# occupancy for `occ()` -- and every visit of that unit reads the
# same one, thinned by that visit's detection probability. The two
# families differ in what the state means, not in how a visit reads
# it, so the broadcast is written once. `visit_to_unit_lookup()`
# inverts the first-visit column `arrays$row_unit` records.
#' @noRd
closure_unit_visit_epred <- function(object, newdata, draw_ids,
                                     linpred = NULL) {
  comp <- extract_closure_unit_components(object, newdata, draw_ids,
                                          linpred = linpred)
  unit_of_visit <- visit_to_unit_lookup(comp$arrays, comp$n_visit)
  comp$state[, unit_of_visit, drop = FALSE] * comp$p
}


posterior_epred_nmix <- function(object, newdata = NULL,
                                  draw_ids = NULL, linpred = NULL) {
  closure_unit_visit_epred(object, newdata, draw_ids, linpred)
}

#' Read a per-unit quantity at the grain of the frame's rows
#'
#' `predict(type = "latent_state")` answers once per closure unit,
#' which is the grain an abundance or an occupancy lives on. A caller
#' that pairs a column of draws with a row of `newdata` needs one
#' column per row, and every row of a unit reads that unit's one
#' state. `marginaleffects` is such a caller, and met a refusal
#' comparing 75 columns against 300 rows.
#'
#' Left alone when the draws already run at row grain, which
#' `type = "detection"` and every non-closure family do, and when the
#' column count matches neither grain, so the caller's own dimension
#' check reports that rather than a second one here.
#'
#' @param object A fitted `mvgam` object.
#' @param newdata The frame the draws were made for.
#' @param draws `[ndraws x N]` matrix, or anything else unchanged.
#' @return The draws, with one column per row of `newdata`.
#' @noRd
closure_unit_draws_to_rows <- function(object, newdata, draws) {
  if (!is.matrix(draws) || is.null(newdata) ||
        !is_closure_unit_family(object$family) ||
        ncol(draws) == nrow(newdata)) {
    return(draws)
  }
  arrays <- closure_unit_arrays_for(object, newdata)
  if (ncol(draws) != arrays$N_unit) {
    return(draws)
  }
  draws[, visit_to_unit_lookup(arrays, nrow(newdata)), drop = FALSE]
}


#' Inverse of arrays$visit_row: for each row of the frame, the unit
#' that contains it. Used to broadcast unit-grain quantities
#' (lambda, latent N) back to the visit grain without copying
#' the lambda matrix.
#' @noRd
visit_to_unit_lookup <- function(arrays, n_visit) {
  # `row_unit` already answers this, over every row of the frame
  # rather than the observed visits alone, so the mapping is read
  # rather than rebuilt. Walking `visit_row` instead left an unmade
  # visit unassigned, and a zero fill for it dropped the column
  # silently, since R reads a zero index as "omit this one"; the two
  # matrices then failed to conform with nothing said about the
  # unmade visit behind it.
  checkmate::assert_count(n_visit)
  out <- arrays$row_unit
  if (length(out) != n_visit) {
    stop(insight::format_error(c(
      "Closure-unit row map does not cover the prediction frame.",
      x = paste0("Rows mapped: ", length(out),
                 "; visits predicted: ", n_visit, "."),
      i = paste0("The frame passed to the prediction differs from ",
                 "the one the unit arrays were built on.")
    )))
  }
  out
}

# Internal: draw a closure-unit family's per-visit response.
#
# Four families walk the same way: a unit draws one latent state,
# and every visit of that unit is an observation of that state
# thinned by the visit's own detection probability. Only the two
# draws differ, so they are the arguments and the walk is written
# once.
#
# The RNG calls run in one order -- a unit's state, then its rows in
# frame order -- which on a frame whose responses are all present is
# the order the per-family versions of this walk used, so a seeded
# draw from such a frame is unchanged.
#' @noRd
closure_unit_visit_draws <- function(object, newdata, draw_ids,
                                     draw_state, draw_visit,
                                     linpred = NULL) {
  comp <- extract_closure_unit_components(object, newdata, draw_ids,
                                          linpred = linpred)
  arrays <- comp$arrays
  ndraws <- comp$ndraws
  out <- matrix(0L, nrow = ndraws, ncol = comp$n_visit)
  # Every row of the unit, not its observed rows alone: a visit whose
  # response was not recorded still has a detection probability and so
  # still has a predictive draw. On a forecast grid no row carries a
  # response at all, and walking the observed ones left every column
  # of the arm at its zero fill.
  unit_rows <- closure_unit_row_split(arrays)
  for (g in seq_len(arrays$N_unit)) {
    idx <- unit_rows[[g]]
    state_g <- draw_state(comp$state[, g], ndraws)
    for (j in idx) {
      out[, j] <- draw_visit(state_g, comp$p[, j], ndraws)
    }
  }
  out
}


# Internal: the latent abundance a unit draws under a Poisson state,
# which all three `nmix()` parameterisations share.
#' @noRd
draw_poisson_abundance <- function(lambda_g, ndraws) {
  stats::rpois(ndraws, lambda = lambda_g)
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
                                    draw_ids = NULL, linpred = NULL) {
  closure_unit_visit_draws(
    object, newdata, draw_ids, draw_poisson_abundance,
    function(N_draws, p_j, ndraws) {
      stats::rbinom(ndraws, size = N_draws, prob = p_j)
    },
    linpred = linpred
  )
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
#'     `visit_row`, `row_unit`, `Y_max`, `unit_labels`).
#' @noRd
aggregate_closure_unit_visits <- function(object,
                                           newdata,
                                           yrep_visit) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_matrix(yrep_visit)
  newdata <- newdata %||% mvgam_training_data(object)
  response_var <- response_column(object)
  arrays <- closure_unit_arrays_for(object, newdata)
  if (ncol(yrep_visit) != nrow(newdata)) {
    stop(insight::format_error(c(
      "Posterior predictive matrix column count does not match 'newdata'.",
      x = paste0(
        "ncol(yrep_visit) = ", ncol(yrep_visit),
        ", nrow(newdata) = ", nrow(newdata), "."
      )
    )))
  }
  y_unit <- sum_within_closure_units(
    arrays, as.numeric(newdata[[response_var]])
  )
  yrep_unit <- sum_within_closure_units(arrays, yrep_visit)
  colnames(yrep_unit) <- arrays$unit_labels
  names(y_unit) <- arrays$unit_labels
  list(y_unit = y_unit, yrep_unit = yrep_unit, arrays = arrays)
}


#' The multinomial trial total of each row's closure unit
#'
#' `multi()` scores a site's K category counts against the site's
#' own total, so `N_site` is the sum of the response over a unit's
#' rows, carried back to every row of that unit. Three places built
#' it with the same loop -- the epred kernel, the predict kernel and
#' `predict(type = "variance")` -- and a fourth would have been
#' written next.
#'
#' @param object Fitted `mvgam` object with a `multi()` family.
#' @param newdata Long-format frame; the fit's own when `NULL`.
#' @param arrays The list `build_closure_unit_arrays()` returns.
#' @return Numeric vector, one trial total per row of `newdata`.
#' @noRd
multinomial_unit_totals <- function(object, newdata, arrays) {
  newdata <- newdata %||% mvgam_training_data(object)
  y <- as.numeric(newdata[[response_column(object)]])
  # `N_site` is the multinomial's sample size, and it is data rather
  # than a parameter: a site's counts sum to it. A site with no counts
  # supplies none, and summing nothing reports a total of zero, which
  # gives every category an expected count of zero and draws no
  # individuals. That is a forecast grid, so say what it is missing.
  unseen <- which(arrays$n_rep == 0L)
  if (length(unseen)) {
    stop(insight::format_error(c(
      "A multinomial site carries no counts. Its total is unknown.",
      x = paste0(
        "Sites with no observed category: ",
        paste(utils::head(arrays$unit_labels[unseen], 5L),
              collapse = ", "),
        if (length(unseen) > 5L) ", ..." else "", "."
      ),
      i = paste0(
        "'multi()' reads each site's trial total from its own ",
        "counts. It can predict a site only where those counts are ",
        "supplied. Use 'diri()' or 'categ()' to predict a ",
        "composition with no total."
      )
    )))
  }
  # Reduce to one total per unit, then read it back at row grain
  # through the row-to-unit map the arrays already carry.
  sum_within_closure_units(arrays, y)[arrays$row_unit]
}


#' An empty per-unit log-density matrix for a composition family
#'
#' `diri()`, `multi()` and `categ()` score one joint density per
#' closure unit and carry it on the unit's first row. The unit's
#' other rows hold no density of their own, which is not the same as
#' a density of zero: `log p = 0` asserts certainty, and `clean_ll()`
#' reads such a column as perfectly scorable because it is finite.
#' `loo()` then counted a site once per category, reporting 120
#' observations and 120 Pareto diagnostics for a fit with 30
#' independent densities.
#'
#' Starting from `NA_real_` says what is true -- these rows carry no
#' density -- and lets the one place that drops unscorable columns
#' do its job.
#'
#' @param ndraws,N_obs Dimensions of the matrix to allocate.
#' @return `[ndraws x N_obs]` matrix of `NA_real_`.
#' @noRd
empty_unit_loglik <- function(ndraws, N_obs) {
  matrix(NA_real_, nrow = ndraws, ncol = N_obs)
}


#' Every row a closure unit holds, observed or not
#'
#' `visit_row` names a unit's observed rows alone, which is the right
#' axis for a density: a visit that did not happen contributes none.
#' It is the wrong axis for any quantity normalised across the unit.
#' A composition's softmax spans the K components of a site, so taken
#' over the survivors it renormalises the site to the components that
#' were recorded: the probabilities sum to one while the observed
#' shares they are scored against sum to less than one. Stan's own
#' `dirichlet_lpdf` validates its simplex argument and rejects that
#' pair, so the R side and the sampled model parted company exactly
#' where a held-out fold or a missing observation put them.
#'
#' `row_unit` already answers this over every row of the frame, so
#' the split is read rather than rebuilt from the grouping columns.
#'
#' @param arrays The list `build_closure_unit_arrays()` returns, or
#'   any list carrying its `row_unit` and `N_unit` fields, which is
#'   what the builder itself has to hand before the list is assembled.
#' @return A list of length `N_unit`, each element the frame rows of
#'   that unit in row order.
#' @noRd
closure_unit_row_split <- function(arrays) {
  row_unit <- arrays$row_unit
  out <- split(
    seq_along(row_unit),
    factor(row_unit, levels = seq_len(arrays$N_unit))
  )
  names(out) <- NULL
  out
}


#' The first frame row of each closure unit
#'
#' A per-unit quantity is carried on one of the unit's rows, and four
#' callers pick it: the unit-grain state, the per-unit log-density,
#' and two `pp_check()` reductions. `visit_row[, 1]` answered this by
#' naming the first *observed* row, which is `NA` for a unit nothing
#' was recorded in, and an `NA` column index silently produces an `NA`
#' column rather than an error. The first row of the unit exists
#' whether or not its response does.
#'
#' @param arrays The list `build_closure_unit_arrays()` returns.
#' @param unit_rows The split `closure_unit_row_split()` returns.
#' @return Integer vector of length `N_unit`.
#' @noRd
closure_unit_first_rows <- function(arrays,
                                    unit_rows =
                                      closure_unit_row_split(arrays)) {
  vapply(unit_rows, `[`, integer(1L), 1L)
}


#' The closure units whose components were all observed
#'
#' A composition has a density only where every component of the unit
#' was recorded, since the observed shares are a simplex only then.
#' The units this omits are left missing by the density kernels, which
#' is what `clean_ll()` reads to drop them from `loo()` and `waic()`.
#'
#' @param arrays The list `build_closure_unit_arrays()` returns.
#' @param unit_rows The split `closure_unit_row_split()` returns.
#' @return Integer vector of unit indices.
#' @noRd
complete_closure_units <- function(arrays,
                                   unit_rows =
                                     closure_unit_row_split(arrays)) {
  which(lengths(unit_rows) == arrays$n_rep)
}


#' Sum a per-visit quantity within each closure unit
#'
#' The one reduction from the visit grain to the unit grain, which
#' is the grain the detection families' likelihood is written on. A
#' per-visit vector reduces to length `N_unit`; a
#' `[ndraws x N_visit]` matrix reduces to `[ndraws x N_unit]`.
#'
#' Reading the unit's rows from `visit_row` rather than from a
#' recomputed key is what keeps the reduction on the same unit
#' ordering the arrays were built with, which is first appearance
#' and not sorted.
#'
#' @param arrays The list `build_closure_unit_arrays()` returns.
#' @param x Per-visit vector, or `[ndraws x N_visit]` matrix, whose
#'   observation axis is in `newdata` row order.
#' @return `x` summed within units, keeping its shape.
#' @noRd
sum_within_closure_units <- function(arrays, x) {
  unit_rows <- function(g) arrays$visit_row[g, seq_len(arrays$n_rep[g])]
  n_unit <- arrays$N_unit
  if (is.null(dim(x))) {
    return(vapply(
      seq_len(n_unit), function(g) sum(x[unit_rows(g)]), numeric(1L)
    ))
  }
  out <- matrix(0, nrow = nrow(x), ncol = n_unit)
  for (g in seq_len(n_unit)) {
    idx <- unit_rows(g)
    # Single-visit units short-circuit the row sum. Multi-visit
    # units sum across the chosen columns; rowSums is the vectorised
    # form and is materially faster than apply on the wide N_visit
    # grain.
    out[, g] <- if (length(idx) == 1L) {
      x[, idx]
    } else {
      rowSums(x[, idx, drop = FALSE])
    }
  }
  out
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
  # Sharing the closure-unit pipeline is not the same as exposing a
  # latent state; the registry is what records which families do.
  require_closure_unit_predict_type(object$family, "latent_state")
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
  response_var <- response_column(object)
  y_vals <- as.integer(newdata[[response_var]])
  out <- matrix(0L, nrow = ndraws, ncol = N_unit)
  for (g in seq_len(N_unit)) {
    idx <- arrays$visit_row[g, seq_len(arrays$n_rep[g])]
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
    idx  <- arrays$visit_row[g, seq_len(arrays$n_rep[g])]
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
  newdata <- newdata %||% mvgam_training_data(object)
  if (is.null(newdata)) {
    stop(insight::format_error(
      "Training data not stored on object; supply 'newdata'."
    ))
  }
  # `Psi` is one entry per species in the order the fit numbered
  # them, so which entry a row reads is a question about the model
  # rather than about the frame. Re-factoring the frame's own column
  # answers it only while the frame carries every species, spelled
  # the same way and in the same order; the axis answers it whatever
  # subset or spelling arrives.
  series_fac <- axis_row_series(object, newdata)
  if (is.null(series_fac)) {
    stop(insight::format_error(c(
      "mv-response families require a 'series' factor column in 'data'.",
      i = "Each row of 'data' is one (site, species) observation."
    )))
  }
  # A row naming a species the fit never saw is refused by
  # `validate_prediction_factor_levels()`, which owns that condition
  # for every family and names both the stranger and the levels that
  # would have worked. It is not re-asked here.
  species_idx <- as.integer(series_fac)
  K <- nlevels(series_fac)

  # mu enters with the latent factor contribution Z[k, :] * lv[i, :]
  # already added via the trend pipeline. process_error = FALSE
  # because closure-unit families do not run a stochastic trend
  # layer. When `linpred` is supplied (log_lik path) skip the
  # recomputation so the caller's already-subsampled linpred is
  # used; this keeps Psi aligned to the same draw indices as mu.
  # Resolve draw_ids up front so mu and Psi come from the same
  # iterations. Left as a count, the linpred below subsamples at
  # random while Psi is read from the first rows of the posterior, and
  # the two describe different draws.
  draw_ids <- resolve_draw_ids(object, ndraws, draw_ids)
  if (is.null(linpred)) {
    linpred <- posterior_linpred(
      object, newdata = newdata, draw_ids = draw_ids,
      ndraws = if (is.null(draw_ids)) ndraws else NULL,
      process_error = FALSE
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
  # A plain numeric matrix, not the `draws_matrix` the posterior
  # arrives as. Its class survives both subsetting and `as.matrix()`,
  # and arithmetic carries it into whatever a kernel builds from it,
  # so `posterior_predict()` handed marginaleffects a `draws_matrix`
  # for `mvt` and was refused: the slot it fills is typed as a
  # matrix. `mvn` escaped only because its kernel happens to rebuild
  # the result with `matrix()`. Dropping the class here means no
  # kernel has to remember to.
  Psi_full <- matrix(
    as.numeric(draws_mat[draw_ids_local, psi_cols, drop = FALSE]),
    nrow = length(draw_ids_local), ncol = length(psi_cols)
  )
  # Broadcast Psi[, species_idx] to a [ndraws x N_obs] matrix so
  # downstream kernels can use it element-wise alongside mu.
  Psi_row <- Psi_full[, species_idx, drop = FALSE]

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
    Psi         = Psi_full,
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
                                 draw_ids = NULL, ndraws = NULL,
                                 linpred = NULL) {
  extract_mv_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_nu = FALSE,
    linpred = linpred
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
                                   draw_ids = NULL, ndraws = NULL,
                                   linpred = NULL) {
  comp <- extract_mv_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_nu = FALSE,
    linpred = linpred
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
                                 draw_ids = NULL, ndraws = NULL,
                                 linpred = NULL) {
  extract_mv_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_nu = TRUE,
    linpred = linpred
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
                                   draw_ids = NULL, ndraws = NULL,
                                   linpred = NULL) {
  comp <- extract_mv_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_nu = TRUE,
    linpred = linpred
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
  newdata <- newdata %||% mvgam_training_data(object)
  if (is.null(newdata)) {
    stop(insight::format_error(
      "Training data not stored on object; supply 'newdata'."
    ))
  }
  arrays <- closure_unit_arrays_for(object, newdata)

  # Resolve draw_ids up front so the mu linpred and the per-row phi
  # (when sourced from a sub-formula) use the same posterior rows.
  # Without this, an ndraws-only call would randomly subsample once
  # for mu and again for phi, mis-aligning the two by draw index.
  draw_ids <- resolve_draw_ids(object, ndraws, draw_ids)

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

  # Build per-row softmax probabilities by walking units. Each unit
  # contributes its K component rows, whose probabilities sum to 1
  # along the draw axis. Every row of the unit is taken, whether or
  # not its response was recorded: a row that lost its response still
  # carries a linear predictor, so the normaliser is over the site the
  # model has rather than over the components that came back.
  unit_rows <- closure_unit_row_split(arrays)
  prob_row <- matrix(0, nrow = ndraws_actual, ncol = N_obs)
  for (g in seq_len(N_unit)) {
    idx <- unit_rows[[g]]
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
#' unit's K rows are set to the value at the unit's first row,
#' mirroring Stan's `phi[idx[1]]` semantics in
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

  # Per-unit collapse: each closure unit's rows share `phi[idx[1]]`
  # (Stan parameterisation). Without this collapse a covariate that
  # varies WITHIN a unit would introduce per-row variation that the
  # Stan likelihood never sees. Taken over the unit's whole set of
  # rows, so a row whose response was not recorded still carries its
  # site's concentration and can be drawn from.
  unit_rows <- closure_unit_row_split(arrays)
  for (g in seq_len(arrays$N_unit)) {
    idx <- unit_rows[[g]]
    phi_mat[, idx] <- phi_mat[, idx[1L]]
  }
  phi_mat
}

#' Per-row expected value for a `diri()` fit
#'
#' Each (site, species) row receives its softmax probability from the
#' per-site K-vector. Compositional responses sum to 1 across the K
#' rows of a site, so `E[y_{i,k}] = softmax(mu_unit_i)[k]`.
#'
#' @inheritParams extract_simplex_response_components
#' @return `[ndraws x N_obs]` matrix of expected proportions.
#' @noRd
posterior_epred_diri <- function(object, newdata = NULL,
                                  draw_ids = NULL, ndraws = NULL,
                                  linpred = NULL) {
  extract_simplex_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_phi = FALSE,
    linpred = linpred
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
                                    draw_ids = NULL, ndraws = NULL,
                                    linpred = NULL) {
  comp <- extract_simplex_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_phi = TRUE,
    linpred = linpred
  )
  out <- matrix(0, nrow = comp$ndraws, ncol = comp$N_obs)
  unit_rows <- closure_unit_row_split(comp$arrays)
  for (g in seq_len(comp$N_unit)) {
    idx <- unit_rows[[g]]
    Kg <- length(idx)
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
#' the first row of each unit and left missing on the remaining K-1,
#' so the sum across rows recovers the joint log-likelihood and loo /
#' waic score at the site grain. A unit that lost a component is left
#' missing entirely: its observed shares no longer sum to one, so it
#' has no Dirichlet density to give.
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
  out <- empty_unit_loglik(ndraws, N_obs)
  unit_rows <- closure_unit_row_split(arrays)
  for (g in complete_closure_units(arrays, unit_rows)) {
    idx <- unit_rows[[g]]
    Kg <- length(idx)
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
                                   draw_ids = NULL, ndraws = NULL,
                                   linpred = NULL) {
  comp <- extract_simplex_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_phi = FALSE,
    linpred = linpred
  )
  total_mat <- matrix(
    multinomial_unit_totals(object, newdata, comp$arrays),
    nrow = comp$ndraws, ncol = comp$N_obs, byrow = TRUE
  )
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
                                     draw_ids = NULL, ndraws = NULL,
                                     linpred = NULL) {
  comp <- extract_simplex_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_phi = FALSE,
    linpred = linpred
  )
  # The trial total is carried on every row of a unit, so reading it
  # off the unit's first row gives `N_site` without a second sum.
  totals <- multinomial_unit_totals(object, newdata, comp$arrays)
  out <- matrix(0L, nrow = comp$ndraws, ncol = comp$N_obs)
  unit_rows <- closure_unit_row_split(comp$arrays)
  for (g in seq_len(comp$N_unit)) {
    idx <- unit_rows[[g]]
    n_g <- totals[idx[1L]]
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
  out <- empty_unit_loglik(ndraws, N_obs)
  unit_rows <- closure_unit_row_split(arrays)
  for (g in complete_closure_units(arrays, unit_rows)) {
    idx <- unit_rows[[g]]
    Kg <- length(idx)
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
                                   draw_ids = NULL, ndraws = NULL,
                                   linpred = NULL) {
  extract_simplex_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_phi = FALSE,
    linpred = linpred
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
                                     draw_ids = NULL, ndraws = NULL,
                                     linpred = NULL) {
  comp <- extract_simplex_response_components(
    object, newdata, draw_ids, ndraws = ndraws, needs_phi = FALSE,
    linpred = linpred
  )
  out <- matrix(0L, nrow = comp$ndraws, ncol = comp$N_obs)
  unit_rows <- closure_unit_row_split(comp$arrays)
  for (g in seq_len(comp$N_unit)) {
    idx <- unit_rows[[g]]
    Kg <- length(idx)
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
  out <- empty_unit_loglik(ndraws, N_obs)
  unit_rows <- closure_unit_row_split(arrays)
  for (g in complete_closure_units(arrays, unit_rows)) {
    idx <- unit_rows[[g]]
    Kg <- length(idx)
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
                                                draw_ids = NULL,
                                                linpred = NULL) {
  comp <- extract_closure_unit_components(object, newdata, draw_ids,
                                          linpred = linpred)
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
                                                  draw_ids = NULL,
                                                  linpred = NULL) {
  closure_unit_visit_draws(
    object, newdata, draw_ids, draw_poisson_abundance,
    function(N_draws, r_j, ndraws) {
      # 1 - (1 - r_j)^N_draws via log space avoids the catastrophic
      # cancellation that hits for small r and small N_draws.
      p_visit <- 1 - exp(N_draws * log1p(-r_j))
      stats::rbinom(ndraws, size = 1L, prob = p_visit)
    },
    linpred = linpred
  )
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
  response_var <- response_column(object)
  y_vals <- as.integer(newdata[[response_var]])
  out <- matrix(0L, nrow = ndraws, ncol = N_unit)
  for (g in seq_len(N_unit)) {
    idx <- arrays$visit_row[g, seq_len(arrays$n_rep[g])]
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
    idx <- arrays$visit_row[g, seq_len(arrays$n_rep[g])]
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
                                                  draw_ids = NULL,
                                                  linpred = NULL) {
  comp <- extract_closure_unit_components(object, newdata, draw_ids,
                                          linpred = linpred)
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
                                                    draw_ids = NULL,
                                                    linpred = NULL) {
  closure_unit_visit_draws(
    object, newdata, draw_ids, draw_poisson_abundance,
    function(N_draws, p_j, ndraws) {
      stats::rpois(ndraws, lambda = N_draws * p_j)
    },
    linpred = linpred
  )
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
  response_var <- response_column(object)
  y_vals <- as.integer(newdata[[response_var]])
  out <- matrix(0L, nrow = ndraws, ncol = N_unit)
  for (g in seq_len(N_unit)) {
    idx <- arrays$visit_row[g, seq_len(arrays$n_rep[g])]
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
    idx <- arrays$visit_row[g, seq_len(arrays$n_rep[g])]
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
                                 draw_ids = NULL, linpred = NULL) {
  closure_unit_visit_epred(object, newdata, draw_ids, linpred)
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
                                   draw_ids = NULL, linpred = NULL) {
  closure_unit_visit_draws(
    object, newdata, draw_ids,
    function(psi_g, ndraws) {
      stats::rbinom(ndraws, size = 1L, prob = psi_g)
    },
    function(z_draws, p_j, ndraws) {
      stats::rbinom(ndraws, size = 1L, prob = z_draws * p_j)
    },
    linpred = linpred
  )
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
    probs <- matrix(NA_real_, nrow = ndraws, ncol = N_unit)
    for (g in seq_len(N_unit)) {
      idx   <- arrays$visit_row[g, seq_len(arrays$n_rep[g])]
      psi_g <- comp$state[, g]
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
    idx   <- arrays$visit_row[g, seq_len(arrays$n_rep[g])]
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
