# Custom observation families for mvgam, beyond those brms builds. Each
# constructor returns a `brms::custom_family()` with its Stan code in
# `attr(family, "mvgam_stanvars")`. `attach_family_stanvars()` in
# `R/make_stan.R` adds that code to the model.

#' Tweedie compound Poisson-gamma family for mvgam
#'
#' The Tweedie distribution with power `mtheta` between 1 and 2, the
#' compound Poisson-gamma. It models non-negative continuous data with
#' a point mass at zero, such as catch per unit effort, rainfall or
#' biomass. A Poisson number of events each contributes a Gamma amount.
#' The variance is `mphi * mu^mtheta`.
#'
#' The family has three distributional parameters:
#' \describe{
#'   \item{`mu`}{The mean, on a fixed log link. The compound
#'     construction needs `mu > 0`, which an identity link driven by a
#'     latent trend cannot guarantee.}
#'   \item{`mphi`}{The dispersion, on a log link.}
#'   \item{`mtheta`}{The power, bounded to `[1.001, 1.999]`. Near 1 the
#'     Poisson rate `mu^(2 - mtheta) / ((2 - mtheta) mphi)` and near 2
#'     the Gamma shape `(2 - mtheta) / (mtheta - 1)` leave floating-point
#'     range.}
#' }
#'
#' The latent trend enters through `mu`. `mphi` and `mtheta` are
#' scalars by default. A formula such as `bf(y ~ x, mphi ~ site)` gives
#' the dispersion its own linear predictor on the log scale, whose
#' intercept takes a `student_t(3, 0, 2.5)` prior. `mtheta` takes no
#' formula.
#'
#' `mu`, `mphi` and `mtheta` trade off along `Var[Y] = mphi * mu^mtheta`.
#' Data that weakly separate Poisson-like from Gamma-like tails leave a
#' ridge in their joint posterior. An informative prior such as
#' `prior(normal(1.5, 0.3), class = mtheta)` resolves it.
#'
#' The Stan log density sums `M` terms of the compound series for each
#' non-zero observation (Dunn and Smyth 2005). The terms past `M` are
#' negligible when `M` is well above the Poisson rate `lambda`, which
#' grows as `mtheta` approaches 2 or `mphi` approaches 0. Raise `M` when
#' [check_tweedie_truncation()] reports a rate near it. `log_lik()` and
#' `posterior_predict()` use `mgcv::ldTweedie()` and `mgcv::rTweedie()`.
#'
#' The Stan code adapts the `custom_family` of the Australian Institute
#' of Marine Science (2023), which uses the decomposition of Spinkney
#' (2020).
#'
#' @param M Positive integer number of series terms in the Stan log
#'   density. Defaults to `30L`. The cost grows linearly in `M`. Too
#'   small a value biases the likelihood down and the posteriors of
#'   `mphi` and `mtheta` with it.
#'
#' @return A `brms::customfamily` object for `mvgam(family = )`.
#'
#' @references
#' Australian Institute of Marine Science (2023). `open-AIMS/tweedie`: a
#'   brms `custom_family` implementation of the Tweedie compound
#'   Poisson-gamma distribution. GitHub,
#'   \url{https://github.com/open-AIMS/tweedie}. MIT licence.
#'
#' Spinkney (2020). Tweedie likelihood (compound Poisson-gamma) in Stan.
#'   Stan Discourse,
#'   \url{https://discourse.mc-stan.org/t/tweedie-likelihood-compound-poisson-gamma-in-stan/14636}
#'
#' Dunn, P. K., & Smyth, G. K. (2005). Series evaluation of
#'   Tweedie exponential dispersion model densities.
#'   *Statistics and Computing*, 15(4), 267-280.
#'   \doi{10.1007/s11222-005-4070-y}
#'
#' Jorgensen, B. (1987). Exponential dispersion models.
#'   *Journal of the Royal Statistical Society: Series B*, 49(2),
#'   127-162. \doi{10.1111/j.2517-6161.1987.tb01685.x}
#'
#' @examples
#' \dontrun{
#' set.seed(3)
#' simdat <- sim_mvgam(family = tweedie(), n_series = 1L,
#'                     n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(
#'   y ~ s(x, k = 5),
#'   trend_formula = ~ AR(p = 1),
#'   data          = simdat$data_train,
#'   family        = tweedie(),
#'   chains        = 2,
#'   silent        = 2
#' )
#'
#' # The dispersion and power
#' mcmc_plot(mod, variable = c("mphi", "mtheta"), type = "areas")
#'
#' # The share of zeros the model predicts against the share observed
#' prop_zero <- function(y) mean(y == 0)
#' pp_check(mod, type = "stat", stat = "prop_zero", binwidth = 0.02)
#'
#' # Next: check_tweedie_truncation(mod) checks the series length M
#' }
#'
#' @family observation families
#' @export
tweedie <- function(M = 30L) {
  checkmate::assert_integerish(
    M, lower = 1L, len = 1L, any.missing = FALSE
  )
  M <- as.integer(M)
  fam <- brms::custom_family(
    name = "tweedie",
    dpars = c("mu", "mphi", "mtheta"),
    links = c("log", "log", "identity"),
    lb = c(NA, 1e-8, 1.001),
    ub = c(NA, NA, 1.999),
    type = "real",
    loop = FALSE,
    vars = "M"
  )
  # The inverse link the prediction code applies to `mu`
  link_info <- stats::make.link(fam$link)
  fam$linkinv <- link_info$linkinv
  fam$linkfun <- link_info$linkfun
  # `family$family` stays "custom", which brms dispatches on.
  # `resolve_family_name()` returns "tweedie".
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
#' all key on the user-visible name. Call this helper to get that name.
#' Do not access `family$family` directly.
#'
#' @param family A `family` / `brmsfamily` / `customfamily`.
#' @return Character scalar.
#' @noRd
resolve_family_name <- function(family) {
  if (inherits(family, "customfamily") &&
        !is.null(family$name) && nzchar(family$name)) {
    return(family$name)
  }
  # `stats::Gamma()` names itself "Gamma". Every dispatcher keys on
  # "gamma".
  fam <- family$family
  if (identical(fam, "Gamma")) fam <- "gamma"
  fam
}

#' The families mvgam builds itself, keyed by constructor name
#'
#' brms builds its own families from a name. These families are built by
#' mvgam's constructors, which brms cannot name. A family given as one of
#' these names is built by calling its constructor.
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
#' brms takes a family written inside `bf()` in preference to the one
#' supplied with the formula. A family inside a univariate `bf()` is taken
#' out of the formula here and becomes the model's family. Every step
#' mvgam runs needs that family. Those steps include adding a custom
#' family's Stan functions and injecting its default priors. They also
#' prepare a closure unit and record the family on the fit. A
#' multivariate formula keeps each response's family in its own `bf()`.
#' `formula_families()` extracts it there.
#'
#' Every response's family is checked once, here. Closure-unit and
#' multi-response families lay the data out by unit and write their
#' likelihood for that layout. Each models its response alone.
#'
#' @param formula The observation formula
#' @param family The family supplied with the formula, in any spelling
#'   `validate_family()` accepts
#' @return A list with elements `formula` and `family`. A univariate
#'   family is taken out of `formula`.
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
    uses_closure_unit_layout(f) || is_multi_response_family(f)
  }, families)
  if (length(families) > 1L && length(alone) > 0L) {
    stop(insight::format_error(c(
      paste0("'", resolve_family_name(alone[[1L]]), "()' cannot be one ",
             "response of a multivariate model."),
      x = "Closure-unit families lay out their likelihood by unit."
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
    data_stanvar(M, "M", "int<lower=1>")
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
  paste(c(
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
    broadcast_overloads(
      "tweedie", "lpdf",
      c(y = "vector", mu = "vector", mphi = "vector", mtheta = "vector",
        M = "int"),
      c("mphi", "mtheta")
    )
  ), collapse = "\n")
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
#' The rate is taken for every response whose family is `tweedie()`. A
#' multivariate model can name it for one response among several. All of
#' them share the one `M`. `mphi` and `mtheta` are taken per observation.
#' This covers a parameter given a formula of its own as well as a scalar
#' one.
#'
#' @param object An `mvgam` fit with at least one response whose
#'   family is `tweedie()`.
#' @param resp Optional name of one response to check. Without it,
#'   every `tweedie()` response is checked.
#' @return Invisible `NULL`. Emits a message on the adequacy of `M`. It
#'   also emits a warning if `M` looks tight.
#'
#' @examples
#' \dontrun{
#' set.seed(3)
#' simdat <- sim_mvgam(family = tweedie(), n_series = 1L,
#'                     n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
#'              data = simdat$data_train, family = tweedie(),
#'              chains = 2, silent = 2)
#'
#' # The largest Poisson rate the fit implies, against the default M = 30
#' check_tweedie_truncation(mod)
#'
#' # A warning here would call for a refit with a longer series, such
#' # as family = tweedie(M = 60)
#' }
#'
#' @seealso [tweedie()] for the family and its argument `M`.
#' @export
check_tweedie_truncation <- function(object, resp = NULL) {
  checkmate::assert_class(object, "mvgam")
  resolve_resp(object, resp)
  families <- formula_families(object, object$family)
  multivariate <- length(families) > 1L
  if (!is.null(resp)) families <- families[resp]
  tweedie_resps <- names(families)[
    vapply(families, resolve_family_name, character(1L)) == "tweedie"
  ]
  if (length(tweedie_resps) == 0L) {
    stop(insight::format_error(
      "'check_tweedie_truncation()' needs a response with family tweedie()."
    ), call. = FALSE)
  }
  # `M` is Stan data, taken from the stored Stan data.
  M <- object$standata$M
  if (is.null(M)) {
    stop_missing_fields("The stored Stan data", "M")
  }
  # The Poisson rate at the posterior mean of `mu`, `mphi` and
  # `mtheta`, per observation of each tweedie response.
  lambda <- unlist(lapply(tweedie_resps, function(r) {
    key <- if (multivariate) r else NULL
    mu <- colMeans(posterior_epred(object, resp = key))
    mphi <- colMeans(dpar_posterior_linpred(
      object, "mphi", transform = TRUE, resp = key
    ))
    mtheta <- colMeans(dpar_posterior_linpred(
      object, "mtheta", transform = TRUE, resp = key
    ))
    mu^(2 - mtheta) / ((2 - mtheta) * mphi)
  }))
  lambda_max <- max(lambda)
  rlang::inform(
    paste0(
      "Tweedie truncation diagnostic. M = ", M,
      ", max(lambda) at posterior mean = ",
      format(lambda_max, digits = 3),
      " (ratio = ", format(lambda_max / M, digits = 3), ")."
    )
  )
  if (lambda_max > 0.7 * M) {
    insight::format_warning(c(
      "Tweedie truncation 'M' may be too small.",
      x = paste0(
        "max(lambda) at the posterior mean (",
        format(lambda_max, digits = 3),
        ") exceeds 70% of M = ", M, "."
      ),
      i = paste0(
        "Refit with a larger truncation such as ",
        "tweedie(M = ", ceiling(lambda_max * 2), "L)."
      ),
      i = "Compare the 'mphi' and 'mtheta' posteriors across the two fits."
    ))
  }
  invisible(NULL)
}

# ============================================================
# Beta negative binomial (beta_nb) family
# ============================================================
# Heavy-tailed count family that mixes a negative binomial success
# probability over a Beta distribution: Y | p ~ NegBinomial(r, p),
# p ~ Beta(alpha, beta). Also known as the generalised Waring
# distribution. The tail decays as a power law. This suits the
# occasional very large count that outbreak and boom-bust population
# series produce. NB2 can absorb such counts only by raising its
# dispersion everywhere.
# Reference: Irwin (1968), JRSS-A. The Stan log-PMF is written against
# lgamma / lbeta. It does not use Stan's native
# `beta_neg_binomial_lpmf`. The family needs no minimum Stan version and
# works on either backend.
#
# Parameterisation. Stan's signature is (r, alpha, beta) with
# E[Y] = r * beta / (alpha - 1). The mean exists only for alpha > 1. The
# variance is finite only for alpha > 2. A regression family needs the
# mean on the linear predictor. mvgam samples a shifted tail parameter
# `mtail = alpha - 1` with a lower bound of zero. It sets
# beta = mu * mtail / shape. The mean is then exactly mu. The awkward
# alpha > 1 constraint becomes an ordinary positivity bound.
# NB2 is recovered as mtail -> infinity.
#
# Prior choices.
# * `shape` does not take the brms negative binomial default of
#   gamma(0.01, 0.01), which concentrates mass near zero. Because
#   beta = mu * mtail / shape diverges as shape -> 0, the Beta mixing
#   distribution is pulled toward a degenerate point mass. On NB2 data
#   with shape = 3, gamma(2, 0.5) recovers shape 5.4 [3.5, 10.4].
# * `mtail ~ gamma(2, 0.25)`. NB2 is the limit mtail -> infinity. A
#   tighter gamma(2, 1) prior pins mtail whenever the data are NB2-like,
#   and the misfit is absorbed by `shape`. The wider gamma(2, 0.25)
#   prior reduces that bias (shape 3.9 [3.0, 5.9] against a truth of
#   3.0) and places 97% of its mass above the finite-variance boundary
#   at mtail = 1.
# * Using this family when NB2 is correct costs about 2 elpd units.
#   Using NB2 when this family is correct costs 9.0 (SE 4.4). The
#   asymmetry favours offering the family.


#' Beta negative binomial family for heavy-tailed counts
#'
#' A three-parameter count family that mixes the negative binomial
#' success probability over a Beta distribution. The tail follows a
#' power law. It suits outbreak counts and boom-bust population series,
#' where a negative binomial fits the bulk of the data but repeatedly
#' underpredicts the largest observations.
#'
#' @details
#' The family is parameterised with `mu` as the mean:
#' \deqn{Y \mid p \sim \mathrm{NegBinomial}(r, p), \quad
#'       p \sim \mathrm{Beta}(1 + \tau, \mu \tau / r)}
#' where `r` is `shape` and \eqn{\tau} is `mtail`. Writing Stan's
#' native first Beta parameter as \eqn{\alpha = 1 + \tau} makes the
#' mean finite by construction. \eqn{E[Y] = \mu} requires
#' \eqn{\alpha > 1}.
#'
#' The variance is finite only when `mtail` exceeds 1. Below that value,
#' the family is heavy-tailed. This is the behaviour the
#' family exists to provide. It has two consequences worth knowing.
#' Randomised quantile residuals, the default in [residuals.mvgam()],
#' are unaffected. They need only the distribution function. Moment-based
#' scoring is affected. `score(..., score = "dss")` assumes a finite
#' predictive variance. Avoid it when the posterior for `mtail` places
#' appreciable mass below 1. Prefer `"crps"`, `"drps"` or `"logs"`.
#'
#' As `mtail` grows the family converges to the negative binomial with
#' the same `shape`. A posterior for `mtail` that runs off toward its
#' prior upper range is evidence that the extra tail parameter is not
#' needed.
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
#' The Stan code supplies the distribution function as well as the
#' density. The `cens()` and `trunc()` addition terms work as they do
#' for the built-in count families.
#'
#' Two limits are worth knowing. The distribution function accumulates
#' the mass function term by term. Its cost and the length of the
#' automatic-differentiation tape both grow linearly in the count at
#' which it is evaluated. At the counts these models usually see this is
#' unnoticeable. A censoring bound in the thousands dominates the
#' gradient evaluation. One in the millions is impractical.
#'
#' Right censoring also needs the upper tail, which is computed as
#' `log1m_exp()` of the distribution function. That subtraction loses
#' precision once the survival probability approaches the accumulated
#' rounding error, roughly when `1 - F(y)` falls below `y * 5e-16`. A
#' censoring bound placed far into the tail of a light-tailed fit can
#' return a survival probability of zero. A small positive value is the
#' correct result there. Heavy-tailed fits, the purpose of this family,
#' keep `1 - F(y)` large. They are unaffected.
#'
#' @return A `customfamily` object for use with [mvgam()].
#' @references
#' Irwin, J. O. (1968). The generalized Waring distribution applied
#' to accident theory. \emph{Journal of the Royal Statistical
#' Society A}, 131(2), 205-225. \doi{10.2307/2343842}
#' @seealso [brms::negbinomial()], which this family approaches as
#'   `mtail` grows. [tweedie()] covers non-negative continuous responses
#'   with a point mass at zero.
#' @examples
#' \dontrun{
#' # Counts drawn from the mixture in Details: a negative binomial
#' # whose success probability follows a Beta distribution. The tail
#' # is set by `mtail = 2`, the bulk by `shape = 3`.
#' set.seed(1)
#' n <- 300
#' dat <- data.frame(x = rnorm(n), time = seq_len(n))
#' mu <- exp(1 + 0.5 * dat$x)
#' p <- rbeta(n, 1 + 2, mu * 2 / 3)
#' dat$y <- rnbinom(n, size = 3, prob = p)
#'
#' mod <- mvgam(y ~ x, data = dat, family = beta_nb(), chains = 2,
#'              silent = 2)
#'
#' # The posteriors recover `shape` and `mtail`. A negative binomial
#' # is the limit of a large `mtail`.
#' summary(mod)
#' mcmc_plot(mod, variable = c("shape", "mtail"), type = "areas")
#' }
#' @family observation families
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
#' Written against `lgamma()` and `lbeta()`. The code does not call Stan's
#' native `beta_neg_binomial_*` suite, which arrived only in Stan 2.36.
#' The family works on the 'rstan' and 'cmdstanr' backends at any Stan
#' version. Its distribution function supports the `cens()` and
#' `trunc()` addition terms.
#'
#' The log-PMF matches Stan's native implementation exactly across a
#' grid of 162 parameter combinations. The distribution function
#' accumulates the PMF with the ratio
#' \eqn{p(k+1)/p(k) = (r+k)(\beta+k) / ((k+1)(\alpha+\beta+r+k))}.
#' Each term costs a handful of logs, where a direct evaluation needs four
#' log-gamma calls. The result is exact to machine precision against a
#' direct sum. Cost grows linearly in `y`, which matters only for
#' censored or truncated observations.
#'
#' The wrapper name differs from Stan's by design. A function named
#' `beta_neg_binomial_lpmf` taking three real parameters would match the
#' built-in signature on newer Stan versions. It would then silently bind
#' the arguments to the wrong roles.
#' @noRd
beta_nb_stan_funs <- function() {
  paste(
    "  real beta_nb_lpmf(int y, real mu, real shape, real mtail) {",
    "    // With mtail = alpha - 1, beta = mu * mtail / shape gives E[Y] = mu",
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
# integration), adapted to the custom_family architecture.
#
# Design notes:
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
#' `nu`. `nu = 1` is the standard binomial. `nu > 1` under-disperses,
#' concentrating mass near the mode. `0 < nu < 1` over-disperses.
#' `nu < 0` super-disperses, producing bimodal "all-or-nothing" mass at
#' the boundaries. Useful when a binomial fit shows systematic lack-of-fit
#' in either tail and the response is bounded by a known trial count.
#'
#' @param link Link function for the success probability `p`.
#'   Currently `logit` only. The COM-Binomial PMF factorises cleanly only
#'   against the canonical logit parameterisation. For that reason,
#'   `probit` and `cloglog` are not exposed.
#'
#' @section Distributional parameters:
#' \describe{
#'   \item{`mu`}{logit-scale success probability (link = `logit`).
#'     Distributional regression on `mu` is the standard formula
#'     side: `bf(y | trials(trials) ~ x)`.}
#'   \item{`nu`}{real-valued dispersion exponent (link = `identity`,
#'     `lb = -5`). `nu = 1` is binomial. Distributional regression
#'     is supported via `bf(y | trials(trials) ~ x, nu ~ z)`.
#'     The support is `0:T`, a finite range. The normalising sum
#'     runs over `T + 1` positive terms at every real `nu`, which
#'     keeps the density proper with no lower bound on it. `lb = -5`
#'     is a numerical floor. The log factorial term grows with
#'     `-nu`. Gradients past that point are hard for the sampler. A
#'     scalar `nu` carries the floor as
#'     `real<lower=-5>` with a prior truncated to match. A modelled
#'     `nu` is a linear predictor and carries no such declaration,
#'     leaving `nu[n]` free. Hold the predictor inside the range the
#'     data resolve, roughly `(-1, 4)`.}
#' }
#'
#' @section Default priors:
#' \itemize{
#'   \item `nu ~ normal(1, 1)` -- centred at `nu = 1`, where the
#'     trials are independent and the distribution is binomial.
#'     The scale spans the range the data can resolve. The response
#'     saturates below `nu = -1` and above `nu = 4`. Values outside
#'     roughly `(-1, 4)` are barely distinguishable. Strongly
#'     under-dispersed data have `nu` near 3. Override
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
#'   `family` argument of `mvgam()` / `jsdgam()`. The object has an
#'   `attr(., "mvgam_stanvars")` attribute, which holds the Stan
#'   function-block stanvar. mvgam's setup attaches it automatically.
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
#' @seealso \code{\link[brms]{custom_family}}, \code{\link{tweedie}}
#' @author Nicholas J Clark, Julijus Bogomolovas
#' @examples
#' \dontrun{
#' # Counts out of 10 trials, simulated with `nu = 3`. At `nu = 1`
#' # the family is the binomial. Values above 1 give counts with
#' # less spread than a binomial.
#' set.seed(1)
#' sim <- sim_mvgam(
#'   family       = com_binomial(),
#'   n_series     = 1L,
#'   n_timepoints = 160L,
#'   trend_model  = "None",
#'   family_pars  = list(nu = 3, trials = 10L)
#' )
#'
#' # The denominator enters through brms's `trials()` term
#' fit <- mvgam(
#'   bf(y | trials(trials) ~ s(x)),
#'   data   = sim$data_train,
#'   family = com_binomial(),
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # The posterior for `nu` lies above the binomial's value of 1
#' summary(fit)
#' mcmc_plot(fit, variable = "nu", type = "areas")
#' pp_check(fit, type = "bars", ndraws = 50)
#' }
#' @family observation families
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
#' Returns the `brms::prior` rows mvgam injects before user priors when
#' the obs-side family is `com_binomial()`. Mirrors
#' `default_simplex_population_priors()` and the injection site in
#' `R/make_stan.R::generate_stan_components_mvgam_formula()`.
#'
#' `nu` is the natural parameter of the COM-Binomial exponential
#' family. It enters the likelihood linearly. A normal prior on it
#' regularises the way one on a regression coefficient does. The prior
#' is centred at `nu = 1`, where the Bernoulli trials making up each
#' count are independent and the distribution is binomial.
#'
#' The scale follows how much the data can say. The response saturates
#' at both ends of `nu`. Below about -1 the distribution is already
#' all-or-none. Above about 4 it is nearly a point mass at half the
#' trials. Neighbouring values become indistinguishable. Measured as the
#' Kullback-Leibler divergence between `nu` and `nu + 0.5` at 30 trials,
#' information peaks near `nu = 0` (1.82) and falls to 0.006 by `nu = 3`.
#' A unit scale places 95% of the prior mass on `(-0.96, 2.96)`. That
#' range covers the values the data can resolve, while still pulling in
#' the saturated tails.
#'
#' @noRd
default_com_binomial_population_priors <- function() {
  brms::prior("normal(1, 1)", class = "nu")
}


#' Default priors on a detection family's `p`
#'
#' A logit-link `p` is a probability. As a scalar it takes the uniform
#' `beta(1, 1)`. With a formula its intercept takes `normal(0, 1.5)`,
#' which puts most of its mass between 0.05 and 0.95. The log-link `p`
#' of `nmix("poisson_poisson")` is an encounter rate per individual per
#' visit. As a scalar it takes `lognormal(0, 1)`. With a formula its
#' intercept takes `std_normal()`. Either form puts most of the rate
#' between 0.14 and 7.4.
#'
#' @param family A family object
#' @return A named character vector holding the `scalar` and
#'   `intercept` laws, or `NULL` for a family without detection
#' @noRd
detection_prior_laws <- function(family) {
  if (!is_closure_unit_family(family)) {
    return(NULL)
  }
  if (identical(family$link_p, "log")) {
    c(scalar = "lognormal(0, 1)", intercept = "std_normal()")
  } else {
    c(scalar = "beta(1, 1)", intercept = "normal(0, 1.5)")
  }
}


#' Merge mvgam's default priors with the user's
#'
#' mvgam injects class-level defaults for parameters whose brms
#' fallback is unsuitable. Concatenating them with a user prior that
#' names the same class leaves two rows for it. brms then refuses the
#' whole set with "Duplicated prior specifications are not allowed",
#' so a user who tries to override a default receives the error. Drop any
#' default the user has already spoken for.
#'
#' @param defaults A `brmsprior` of mvgam defaults, from
#'   `response_default_priors()`, possibly `NULL`
#' @param user_prior The user's `brmsprior`, possibly `NULL`
#' @return A `brmsprior` combining both, with no duplicated classes
#' @noRd
merge_default_priors <- function(defaults, user_prior) {
  if (NROW(defaults) == 0L) {
    return(user_prior)
  }
  if (NROW(user_prior) > 0L) {
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
#' needs to place it. Code generation injects these. `get_prior()`
#' reports them. The two share one definition.
#'
#' @param formula The observation formula
#' @param family The family supplied with it, validated
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
    if (NROW(defaults) == 0L) return(NULL)
    if (inherits(formula, "mvbrmsformula")) defaults$resp <- key
    defaults
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  do.call(c, unname(rows))
}

#' The distributional parameters a formula gives a sub-formula
#'
#' A `brmsformula` keeps dpar and non-linear sub-formulas together in
#' `$pforms`. The family's own dpars pick out the first kind.
#'
#' @param formula One response's observation formula
#' @param family That response's family object
#' @return Character vector of dpar names, possibly empty
#' @noRd
modelled_dpars <- function(formula, family) {
  if (!inherits(formula, "brmsformula")) return(character(0))
  intersect(names(formula$pforms %||% list()), family$dpars)
}

#' Re-aim injected dpar priors when the user models that dpar
#'
#' mvgam injects class-level defaults for family-specific
#' distributional parameters (`nu` for `com_binomial()`, `shape` and
#' `mtail` for `beta_nb()`). Giving one of those a sub-formula replaces
#' the scalar parameter with a design matrix and an intercept. A prior
#' aimed at the scalar class then matches nothing. brms rejects the whole
#' prior set.
#'
#' Where the dpar uses an identity link, its intercept is on the same
#' scale as the scalar. The default moves across intact. That matters
#' for `nu`, whose name brms already associates with the Student-t
#' degrees of freedom. Without the move, brms falls back to a
#' positive-only `gamma(2, 0.1)` on a parameter free to go negative.
#' brms also warns about it. Under any other link the scalar default
#' describes the wrong scale. It is dropped.
#'
#' brms gives the intercept of a modelled dpar `student_t(3, 0, 2.5)`
#' on its link scale for its own families. A custom family's intercept
#' gets no prior at all. Every modelled dpar of a custom family without a
#' carried-over default gets the brms intercept default. A detection
#' family's `p` takes the intercept law of `detection_prior_laws()`. A
#' user prior on `class = "Intercept", dpar = <name>` replaces either.
#'
#' @param prior A `brmsprior` of injected defaults, or `NULL`
#' @param formula The observation formula, possibly a `brmsformula`
#'   carrying dpar sub-formulas in `$pforms`
#' @param family The family object, consulted for each dpar's link
#' @return `prior`, with rows for modelled dpars re-aimed, removed or
#'   added, or `NULL` when no row remains
#' @noRd
adjust_modelled_dpar_priors <- function(prior, formula, family) {
  modelled <- modelled_dpars(formula, family)
  if (!length(modelled)) return(prior)

  if (NROW(prior) > 0L) {
    hit <- prior$class %in% modelled
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
  }

  if (identical(family$family, "custom")) {
    covered <- if (is.null(prior)) character(0) else
      prior$dpar[prior$class == "Intercept"]
    for (dp in setdiff(modelled, covered)) {
      law <- if (dp == "p") detection_prior_laws(family)[["intercept"]]
      row <- brms::prior_string(law %||% "student_t(3, 0, 2.5)",
                                class = "Intercept", dpar = dp)
      prior <- if (is.null(prior)) row else c(prior, row)
    }
  }
  if (!is.null(prior) && nrow(prior) == 0L) NULL else prior
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
#' population effects carry no likelihood information. A detection
#' family's `p` would take a flat prior, which is improper for the
#' unbounded encounter rate of `nmix("poisson_poisson")`.
#'
#' Stan code generation injects these before user priors. `get_prior()`
#' reports them. Both resolve one definition and cannot drift apart.
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
  } else if (!is.null(detection_prior_laws(family))) {
    brms::prior_string(detection_prior_laws(family)[["scalar"]],
                       class = "p")
  } else {
    NULL
  }
}


#' Build the Stan stanvars bundle for the `com_binomial()` family
#'
#' Returns the function-block stanvar wrapping `com_binomial_lpmf`
#' plus the transformed-data `lchoose` lookup table. Auto-attached
#' via `attr(family, "mvgam_stanvars")` at construction time. Both
#' blocks are data-independent. The per-row trials arrive through
#' brms's `trials()` addition term. The table sizes itself from them.
#' Nothing here needs the user's data frame.
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
#' `lchoose(T, j)` is `lfact[T] - lfact[j] - lfact[T - j]`. One vector
#' of log factorials up to `max(trials)` serves every `(T, j)` pair.
#' Storing the binomial coefficients themselves would need a
#' `(max(trials) + 1)^2` array. On a panel with a denominator in the low
#' thousands, that array runs to millions of doubles, while a vector of a
#' few thousand values holds the same information.
#'
#' Each element is `lgamma(k + 1)`, evaluated directly. Rounding error
#' does not accumulate along the table.
#'
#' The bound is taken from `trials` inside Stan. It follows whatever rows
#' brms kept. It cannot fall out of step with the response. brms names
#' the denominator of each response of a multivariate model
#' `trials_<resp>`. One table serves every `com_binomial()` response. Its
#' size is set by the largest denominator among them.
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
#' `com_binomial_lpmf(Y[n] | mu[n], nu, trials[n])`. The lpmf signature
#' is `(int y, real mu, real nu, int T)`.
#'
#' Two properties of the kernel keep the cost down. Every factor that
#' does not depend on the outcome `j` is shared by the numerator and by
#' each term of the normaliser. Such a factor cancels. Dropping it leaves
#' the natural-parameter form `theta * j - nu * (log j! + log (T-j)!)`
#' with `theta = logit(p)`. This removes one length-`T + 1` autodiff
#' vector operation per row per gradient. The normaliser is summed only
#' over the terms with non-negligible mass. An explicit bound on the
#' omitted terms justifies this. The series runs over `T + 1` outcomes,
#' where `T` is the binomial denominator. A panel with denominators in the
#' thousands would otherwise pay thousands of autodiff nodes per row.
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
    "     log factorials leaves lw(j) below. Every dropped term is",
    "     common to the numerator and to every term of Z. It cancels",
    "     in the ratio.",
    "  */",
    "  real com_binomial_lw(int j, int T, real nu, real theta,",
    "                       data vector lfact) {",
    "    return theta * j - nu * (lfact[j + 1] + lfact[T - j + 1]);",
    "  }",
    "  /* Largest integer in [0, T] at or below the continuous mode",
    "     (T + 1) * inv_logit(theta / nu), found by binary search.",
    "     Stan lacks a real-to-int cast. The mode depends on",
    "     parameters, which keeps it out of transformed data. */",
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
    "       bounds the whole omitted sum, beyond the next term. */",
    "    real leps = log(1e-12);",
    "    real mx;",
    "    real s;",
    "    real f;",
    "    if (nu > 0) {",
    "      /* lw is concave in j. Its terms rise to a single mode and",
    "         fall monotonically after it. Stopping at j leaves",
    "         at most (remaining count) * exp(lw(j)). The tolerance is",
    "         split between the two directions. */",
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
    "      /* lw is convex in j (linear at nu = 0). Its maxima are the",
    "         two endpoints. An unsummed interval [L, R] is bounded by",
    "         (R - L + 1) * exp(max(lw(L), lw(R))). The sum takes terms",
    "         from both ends inward until that bound falls below",
    "         tolerance. */",
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
# Closure-unit family helpers
# ============================================================
# A closure-unit family takes one row per visit. The rows sharing a
# unit key are the visits to one unit. Its Stan density works per
# unit. A detection family (`nmix()` and its variants, `occ()`)
# marginalises the latent state of the unit. A multi-response family
# (`diri()`, `multi()`, `categ()`, `mvn()`, `mvt()`) scores the unit's
# rows as the components of one observation. Each family passes
# through four steps:
#
#   1. `uses_closure_unit_layout()` routes the family to this layout.
#   2. `validate_closure_unit_data()` checks a detection family's
#      response, cap and design.
#   3. `build_closure_unit_arrays()` builds the unit arrays, at fit
#      time and again for each prediction frame.
#   4. The family's `make_*_stanvars()` writes its Stan functions and
#      data from those arrays.
#
# `K_max[g]` bounds the latent count of unit g. A `cap` column sets it
# when the frame has one. The family's default sets it otherwise. A
# prediction frame may carry its own `cap`.

#' Detect whether a family uses the closure-unit data layout
#'
#' TRUE for the detection families: `occ()` and `nmix()` with its
#' variants. TRUE also for the multi-response families `mvn()`,
#' `mvt()`, `diri()`, `multi()` and `categ()`. The value comes from the
#' `mvgam_closure_unit` attribute that each constructor attaches. The
#' data layout alone does not establish a detection process.
#' `is_closure_unit_family()` tests for one.
#'
#' @param family A family / brmsfamily / customfamily object.
#' @return TRUE or FALSE.
#' @noRd
uses_closure_unit_layout <- function(family) {
  if (is.null(family)) return(FALSE)
  isTRUE(attr(family, "mvgam_closure_unit", exact = TRUE))
}

#' Whether a family's density is joint over the rows of a unit
#'
#' [diri()], [multi()], [categ()], [mvn()] and [mvt()] carry the
#' `mvgam_multi_response` attribute. A unit's rows are the components
#' of one observation. The family's Stan density scores them
#' together.
#'
#' @param family A family / brmsfamily / customfamily object.
#' @return TRUE or FALSE.
#' @noRd
is_multi_response_family <- function(family) {
  if (is.null(family)) return(FALSE)
  isTRUE(attr(family, "mvgam_multi_response", exact = TRUE))
}

#' Detect whether a family models a detection process over repeat
#' visits to a closed unit
#'
#' TRUE for `occ()` and `nmix()` with its variants. Their Stan
#' likelihood marginalises a latent state (N for nmix, z for occ).
#' Their post-fit methods aggregate per-visit responses to the
#' closure-unit grain. FALSE for `mvn()`, `mvt()`, `diri()`,
#' `multi()` and `categ()`. These families share the data layout but
#' model no detection. Their per-row density is conditionally independent
#' given the factor scores in mu.
#'
#' The value comes from the `mvgam_predict_types` registry, which records
#' what a family exposes. Testing the layout alone would route five
#' families into code paths written for a detection process.
#'
#' @param family A family / brmsfamily / customfamily object.
#' @return TRUE or FALSE.
#' @noRd
is_closure_unit_family <- function(family) {
  "detection" %in% family_predict_types(family)
}

#' Detect whether a detection family's latent state counts
#' individuals
#'
#' TRUE for `nmix()` and its `royle_nichols` / `poisson_poisson`
#' variants, whose latent state is an abundance. FALSE for `occ()`,
#' whose latent state is presence. The Royle-Nichols variant scores
#' a binary response over an abundance state. That combination is why
#' `is_binary_response_family()` does not settle this question.
#'
#' @param family A family / brmsfamily / customfamily object.
#' @return TRUE or FALSE.
#' @noRd
models_latent_count <- function(family) {
  is_closure_unit_family(family) &&
    startsWith(resolve_family_name(family) %||% "", "nmix")
}

#' Whether a multi-response family takes its probabilities from a
#' softmax
#'
#' TRUE for [diri()], [multi()] and [categ()]. Their loadings columns
#' sum to zero, which removes the shift a softmax leaves unidentified.
#' [mvn()] and [mvt()] identify the level of `mu` and take no such
#' constraint.
#'
#' @param family A family / brmsfamily / customfamily object.
#' @return TRUE or FALSE.
#' @noRd
is_simplex_response_family <- function(family) {
  if (is.null(family)) return(FALSE)
  isTRUE(attr(family, "mvgam_simplex_response", exact = TRUE))
}


#' Whether a family's response is binary
#'
#' `occ()` and the Royle-Nichols `nmix()` score detections. Their
#' response is binary while the latent state is a count or occupancy.
#' `attr()` takes `exact = TRUE` here, as for the other family
#' attributes. Without it `attr()` matches by prefix.
#'
#' @param family Family object, or `NULL`
#' @return Logical scalar
#' @noRd
is_binary_response_family <- function(family) {
  checkmate::assert_list(family, null.ok = TRUE)
  if (is.null(family)) return(FALSE)
  isTRUE(attr(family, "mvgam_binary_response", exact = TRUE))
}


#' The prediction types a family adds to the shared set
#'
#' Covers `predict()` and `conditional_effects()`. A family adding none
#' returns an empty vector, which a caller can bind without a `NULL`
#' check.
#'
#' @param family Family object, or `NULL`
#' @return Character vector of prediction types
#' @noRd
family_predict_types <- function(family) {
  checkmate::assert_list(family, null.ok = TRUE)
  if (is.null(family)) return(character(0L))
  attr(family, "mvgam_predict_types", exact = TRUE) %||% character(0L)
}


#' Refuse a closure-unit prediction type the family lacks
#'
#' `mvn()`, `mvt()` and `diri()` use the closure-unit layout and expose
#' no latent state. The family registry lists the types each family
#' offers. Every caller checks it here.
#'
#' @param family Family object
#' @param type Character prediction type
#' @return `TRUE`, invisibly, or an error
#' @noRd
require_closure_unit_predict_type <- function(family, type) {
  checkmate::assert_string(type)
  if (!uses_closure_unit_layout(family) ||
        !(type %in% family_predict_types(family))) {
    refuse_unsupported_predict_type(family, type)
  }
  invisible(TRUE)
}


#' Refuse a state only a fault in mvgam can reach
#'
#' The caller wrote nothing wrong. The message tells them where to
#' report the fault. Every refusal of this kind goes through here.
#'
#' @param headline Character main line of the message
#' @param detail Character detail line of the message
#' @return Nothing. Always raises an error.
#' @noRd
stop_mvgam_fault <- function(headline, detail) {
  checkmate::assert_string(headline)
  checkmate::assert_string(detail)
  stop(insight::format_error(c(
    headline,
    x = detail,
    i = paste0("Please report this mvgam fault at ", mvgam_issues_url(),
               ".")
  )), call. = FALSE)
}


#' The address of mvgam's issue tracker
#'
#' @return The `BugReports` field of the DESCRIPTION file
#' @noRd
mvgam_issues_url <- function() {
  utils::packageDescription("mvgam", fields = "BugReports")
}


#' Refuse an internal array of the wrong shape
#'
#' @param what Character description of the array
#' @param got Numeric dimension vector received
#' @param expected Numeric dimension vector the consumer needs
#' @return Nothing. Always raises an error.
#' @noRd
stop_shape_fault <- function(what, got, expected) {
  checkmate::assert_string(what)
  stop_mvgam_fault(
    paste0(what, " has the wrong shape."),
    paste0("Got [", paste(got, collapse = " x "), "], expected [",
           paste(expected, collapse = " x "), "].")
  )
}


#' Fault on an array whose dimensions differ from those expected
#'
#' @param arr The array
#' @param what What the array holds, for the message
#' @param expected Integer vector of dimensions
#' @return `arr`, invisibly
#' @noRd
assert_dims <- function(arr, what, expected) {
  if (!identical(dim(arr), as.integer(expected))) {
    stop_shape_fault(what, dim(arr), expected)
  }
  invisible(arr)
}


#' Refuse an internal list missing fields its consumer uses
#'
#' @param what Character description of the list
#' @param missing Character vector of the missing field names
#' @return Nothing. Always raises an error.
#' @noRd
stop_missing_fields <- function(what, missing) {
  checkmate::assert_string(what)
  stop_mvgam_fault(
    paste0(what, " lacks required fields."),
    paste0("Missing: ", paste0("'", missing, "'", collapse = ", "), ".")
  )
}


#' Refuse an internal object of the wrong class
#'
#' @param what Character description of the object
#' @param object The object received
#' @param expected Character description of the expected class
#' @return Nothing. Always raises an error.
#' @noRd
stop_wrong_class <- function(what, object, expected) {
  checkmate::assert_string(what)
  checkmate::assert_string(expected)
  stop_mvgam_fault(
    paste0(what, " must be ", expected, "."),
    paste0("Got class '", paste(class(object), collapse = "', '"), "'.")
  )
}


#' Refuse a posterior missing draws its fit always stores
#'
#' @param what Character parameter name
#' @param columns Character vector of the missing column names
#' @return Nothing. Always raises an error.
#' @noRd
stop_missing_draws <- function(what, columns) {
  checkmate::assert_string(what)
  stop_mvgam_fault(
    paste0("The posterior lacks the '", what, "' draws."),
    paste0("Missing columns: ", paste(utils::head(columns, 6L),
                                      collapse = ", "),
           if (length(columns) > 6L) ", ..." else "", ".")
  )
}


#' Refuse a family registered without a kernel some method needs
#'
#' @param family_name Character family name
#' @param what Character description of the missing computation
#' @return Nothing. Always raises an error.
#' @noRd
refuse_missing_family_dispatch <- function(family_name, what) {
  checkmate::assert_string(family_name)
  checkmate::assert_string(what)
  stop_mvgam_fault(
    paste0("mvgam cannot compute ", what, " for family '",
           family_name, "'."),
    paste0("Family '", family_name, "' registers no kernel for ", what,
           ".")
  )
}


refuse_unsupported_predict_type <- function(family, type) {
  types <- family_predict_types(family)
  # Two different families reach here and they need different
  # sentences. One has no closure unit at all. The other shares the
  # closure-unit pipeline, as `diri()` does. It still models no latent
  # state, which is true of every multi-response family. Calling both
  # "not a closure-unit family" misstates the second family's model.
  detail <- if (length(types) > 0L) {
    paste0(
      "Family '", resolve_family_name(family), "' exposes types: ",
      paste(paste0("'", types, "'"), collapse = ", "), "."
    )
  } else if (uses_closure_unit_layout(family)) {
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

#' The cap a closure-unit family gives a unit without a `cap` column
#'
#' `occ()` caps every unit at 1, as its state is binary.
#' `nmix("royle_nichols")` caps it at 25, as its binary detections give
#' no per-unit floor. The other `nmix()` variants take a buffer of 100
#' above each unit's largest count.
#'
#' @param family A family object, or NULL
#' @param buffer Logical. `TRUE` returns the buffer, `FALSE` the fixed
#'   cap.
#' @return An integer, or `NULL` when the family sets none
#' @noRd
closure_unit_default_cap <- function(family, buffer = FALSE) {
  checkmate::assert_flag(buffer)
  attr(family, if (buffer) "mvgam_default_cap_buffer" else
    "mvgam_default_cap", exact = TRUE)
}

#' Closure-unit grouping vars of a family
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
#' the closure-unit key to `(series, site, time)`. In that key, `time`
#' is a season that indexes the latent state. In a single-season design
#' it is the unit identifier. The arity of the grouping records this
#' fact. The likelihood depends on the same fact.
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
#' defaults. Stating them at each caller would let five places hold the
#' same fact, each free to drift: an array builder, a validator, two
#' array rebuilds and this accessor. The fact lives here. Every other
#' place asks this function instead.
#'
#' A detection family keys on `(series, time)`. A unit's rows are then
#' the repeat visits to one series. A multi-response family keys on the
#' time alone. A unit is then a site. The unit's rows are the K response
#' components measured there. The key determines whether a per-series cut
#' of a frame preserves units. It preserves them for the detection
#' families. For a composition, it leaves one row per unit. That row is a
#' simplex of width one, whose single probability is 1 by construction.
#'
#' Returns `NULL` for a family that has no closure unit at all. A caller
#' can then tell "not a closure-unit family" apart from "the default
#' key" and word its own refusal. `family = NULL` requests the detection
#' default without a family to consult. The builder and the validator
#' need that default when a caller names no grouping.
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
    if (!uses_closure_unit_layout(family)) {
      return(NULL)
    }
    if (is_multi_response_family(family)) {
      return(time_var)
    }
  }
  c(series_var, time_var)
}


#' The columns that key a fitted model's closure units
#'
#' The fit records the time and series columns its trend named.
#'
#' @param object Fitted `mvgam` object
#' @return Character vector of key columns, or `NULL` for a family
#'   without closure units
#' @noRd
fit_closure_unit_keys <- function(object) {
  checkmate::assert_list(object)
  vars <- axis_vars(object)
  closure_unit_key_vars(object$family, vars$series_var, vars$time_var)
}


#' The series levels of a closure-unit fit
#'
#' For a multi-response family the levels are the response components
#' each unit holds one row of. For a detection family they are the
#' series whose `(series, time)` pairs form the units. The levels come
#' from the fit's axis. A frame's own series column can re-factor into
#' another numbering, which would score a row against the wrong entry
#' of `Psi`. `extract_mv_response_components()` uses the same levels.
#'
#' @param object Fitted `mvgam` object
#' @return Character vector of series levels
#' @noRd
closure_unit_axis_levels <- function(object) {
  checkmate::assert_list(object)
  levels(axis_row_series(object, object$data))
}


#' Whether a frame holds intact closure units
#'
#' `marginaleffects` reaches `get_predict.mvgam()` with the training
#' frame or a synthetic grid. A real frame is predicted unchanged. A
#' grid needs the unit structure its family requires. A composition or
#' multivariate response is intact when every unit holds each
#' component once. `datagrid()` pins the key at one typical value,
#' giving one unit with many rows of one component. A detection unit
#' is a `(series, time)` pair with repeat visits, which no structural
#' test separates. A detection frame holding a `visit` column counts as
#' intact. No model formula names that column, so `datagrid()` cannot
#' create it.
#'
#' @param object Fitted `mvgam` object
#' @param newdata Data frame of prediction rows
#' @return Logical scalar
#' @noRd
closure_units_are_intact <- function(object, newdata) {
  checkmate::assert_list(object)
  checkmate::assert_data_frame(newdata)
  key <- fit_closure_unit_keys(object)
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


#' Refuse a compositional frame whose units lack some categories
#'
#' A composition is shared among every category of a unit. Shares
#' computed over a subset sum to one over that subset. They differ from
#' the shares of the whole unit.
#'
#' @param object A fitted `mvgam` object
#' @param newdata Frame to predict for
#' @return `TRUE`, invisibly
#' @noRd
refuse_partial_compositions <- function(object, newdata) {
  if (!is_simplex_response_family(object$family) ||
        closure_units_are_intact(object, newdata)) {
    return(invisible(TRUE))
  }
  levs <- closure_unit_axis_levels(object)
  stop(insight::format_error(c(
    "A composition is predicted for whole units.",
    x = paste0("Some units of 'newdata' lack one of the ", length(levs),
               " categories, or repeat one."),
    i = paste0("Give each unit one row for each of: ",
               paste0("'", levs, "'", collapse = ", "), ".")
  )), call. = FALSE)
}


#' Resolve the frame a closure-unit prediction is made for
#'
#' A frame to predict for may leave the response out. Its rows are
#' then unobserved visits, as rows with a missing response are.
#'
#' @param object A fitted `mvgam` object
#' @param newdata Frame to predict for, or `NULL` for the training data
#' @return A data frame carrying the response column
#' @noRd
closure_unit_frame <- function(object, newdata = NULL) {
  newdata <- prediction_frame(object, newdata)
  response_var <- response_column(object)
  if (!response_var %in% names(newdata)) {
    newdata[[response_var]] <- NA_real_
  }
  refuse_partial_compositions(object, newdata)
  newdata
}


#' A composition's prediction grid completed to whole sites
#'
#' A softmax spans the K categories of a site. A grid row holding one
#' category is a simplex of width one, with probability 1 whatever the
#' linear predictor. Each distinct covariate setting is completed to
#' the K category rows of one synthetic site. The caller takes back the
#' rows the grid asked about. Grouping the grid's own rows is not
#' enough. A main-effect grid holds every category at its first
#' covariate value and one category at the rest. Only a synthetic grid
#' reaches the completion, keeping the K-fold widening to a few hundred
#' rows.
#'
#' @param object Fitted `mvgam` object
#' @param newdata Data frame of prediction rows
#' @param is_grid Logical indicator for a synthetic grid
#' @return `NULL` when the family needs no completion. Otherwise a list
#'   holding the completed grid `data` and `take`, one index into
#'   `data` per row of `newdata`.
#' @noRd
complete_simplex_grid <- function(object, newdata,
                                  is_grid = !closure_units_are_intact(
                                    object, newdata
                                  )) {
  checkmate::assert_list(object)
  checkmate::assert_data_frame(newdata)
  checkmate::assert_flag(is_grid)
  if (!is_simplex_response_family(object$family) || !is_grid) {
    return(NULL)
  }
  levs <- closure_unit_axis_levels(object)
  if (length(levs) < 2L) {
    return(NULL)
  }
  resp <- response_column(object)
  vars <- axis_vars(object)
  # The covariate setting of a row is everything apart from the category
  # axis, the unit identifiers and the response.
  held <- setdiff(names(newdata),
                  c(vars$series_var, vars$time_var, "visit", "cap",
                    "rowid", resp))
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
  out[[vars$series_var]] <- factor(rep(levs, times = n_set), levels = levs)
  out[[vars$time_var]] <- rep(seq_len(n_set), each = K)
  out$visit <- 1L
  out[[resp]] <- 1 / K
  rownames(out) <- NULL
  # Which completed row each original row asked about. The component
  # comes from the axis, as `closure_unit_axis_levels()` specifies. The
  # categories belong to the model. The series column is built from those
  # categories. The index back into it is resolved the same way.
  asked <- as.integer(axis_row_series(object, newdata))
  take <- (match(key, settings) - 1L) * K + asked
  if (anyNA(take)) {
    stop(insight::format_error(c(
      "The prediction grid contains a category absent from the model.",
      x = paste0(
        "Unknown: ",
        paste(unique(setdiff(as.character(newdata[[vars$series_var]]),
                             levs)),
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
#   * the response column to `0L`. A synthetic unit has no observation
#     attached. The value `0L` also satisfies the integer and binary
#     checks in `validate_closure_unit_data()`. The response is never
#     consumed by `posterior_epred()` or `posterior_predict()` for these
#     families.
#
# `cap` and the response are filled on any frame that omits them,
# grid or not. The pipeline uses both whatever the frame's shape. `cap`
# takes the family's `mvgam_default_cap` attribute or, failing that, the
# first training row's value.
#
# The unit structure is left alone (`newdata` returned with those
# fills alone) when:
#   * the family has no closure unit, or
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
  if (!uses_closure_unit_layout(object$family)) return(newdata)
  data <- object$data %||% data.frame()
  if (nrow(data) == 0L) return(newdata)
  template <- data[1L, , drop = FALSE]
  # The response is resolved without a guard, as at every other post-fit
  # closure-unit site. A fit of this family reaches here only after its
  # single response was resolved at fit time. A failure here indicates a
  # broken object. No fallback applies.
  resp <- response_column(object)
  # A column the frame does not carry is filled whichever kind of frame
  # this is. `cap` is the upper truncation the unit arrays need. The
  # response is checked by the binary or non-negative integer validator,
  # not used for any prediction. A frame that omits either is completed,
  # not refused.
  if (!"cap" %in% names(newdata)) {
    cap_val <- closure_unit_default_cap(object$family) %||%
                 template$cap %||% 1L
    newdata$cap <- as.integer(rep(cap_val, nrow(newdata)))
  }
  if (!resp %in% names(newdata)) {
    newdata[[resp]] <- rep(0L, nrow(newdata))
  }
  # What remains is the unit structure itself. Only a synthetic grid
  # needs it built. Real long-format newdata keeps the labels it arrived
  # with. Forecasting and multi-season fits depend on those labels.
  if (!is_grid) return(newdata)
  vars <- axis_vars(object)
  if (!vars$series_var %in% names(newdata)) {
    # The levels come from the axis. A grouping that superseded the
    # training column orders them differently from that column.
    newdata[[vars$series_var]] <- factor(
      as.character(template[[vars$series_var]]),
      levels = closure_unit_axis_levels(object)
    )
  }
  # On the grid path, every row should be its own closure unit
  # so each prediction varies the covariate independently.
  # `datagrid()` pins `time` at a single typical value drawn from
  # training, which would otherwise collapse the whole grid into
  # one unit and flatten the predicted curve.
  newdata[[vars$time_var]] <- seq_len(nrow(newdata))
  newdata$visit <- 1L
  # A grid's response takes whatever value `datagrid()` assigned. A
  # synthetic unit has no observation attached. The safe default
  # overwrites that value here.
  newdata[[resp]] <- rep(0L, nrow(newdata))
  newdata
}


#' The closure unit each row belongs to
#'
#' The grouping columns are integer-coded before pasting. Columns of
#' any type then combine. The separator cannot collide with a value. A
#' missing response is a visit that did not happen. brms drops those
#' rows from the likelihood. Naming `response_var` makes `n_rep` the
#' observed visit count, leaving a unit with none at zero.
#'
#' @param data Data frame of visit rows
#' @param unit_grouping_vars Character vector of key columns
#' @param response_var Character response column, or `NULL` to count
#'   every row
#' @return List holding `unit`, `label`, `levels`, `observed` and
#'   `n_rep`
#' @noRd
closure_unit_index <- function(data, unit_grouping_vars,
                               response_var = NULL) {
  checkmate::assert(
    checkmate::check_data_frame(data),
    checkmate::check_list(data)
  )
  checkmate::assert_character(unit_grouping_vars, min.len = 1L)
  checkmate::assert_string(response_var, null.ok = TRUE)
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


#' Refuse closure-unit data missing a column the layout needs
#'
#' The response and the key columns place every visit. A `cap` column
#' bounds each unit's latent count where the family sets no default.
#' The fit and prediction paths share the message.
#'
#' @param data Data frame of visit rows
#' @param columns Character vector of required columns
#' @return `TRUE`, invisibly, or an error
#' @noRd
assert_closure_unit_columns <- function(data, columns) {
  checkmate::assert(
    checkmate::check_data_frame(data),
    checkmate::check_list(data)
  )
  checkmate::assert_character(columns)
  missing <- setdiff(columns, names(data))
  if (!length(missing)) {
    return(invisible(TRUE))
  }
  roles <- c(
    cap = "'cap' is the upper bound on the latent count of each unit.",
    visit = "'visit' numbers the repeat surveys within each unit."
  )
  roles <- roles[intersect(missing, names(roles))]
  stop(insight::format_error(c(
    "Columns the model uses are missing from the data.",
    x = paste0("Absent: ", paste0("'", missing, "'", collapse = ", "), "."),
    stats::setNames(unname(roles), rep("i", length(roles)))
  )), call. = FALSE)
}


#' Closure-unit index arrays from long-format data
#'
#' Groups the rows of a frame into closure units and returns the
#' integer arrays the Stan data block declares. `visit_idx` is a
#' `[N_unit, max_rep]` matrix of row positions, padded on the right
#' with `1L` for units of fewer than `max_rep` visits. The Stan loop
#' takes the first `n_rep[g]` entries of a row.
#'
#' The fit takes the arrays of its training data, which fix `K_max` in
#' the Stan data. A prediction takes those of `newdata`, whose `cap`
#' column may set another truncation for the latent state.
#'
#' @param data Long-format observation data frame
#' @param response_var Name of the response column
#' @param series_var,time_var Names of the series and time columns
#' @param cap_var Name of the column bounding the latent state. Its
#'   value is `K_max[g]` and must be constant within a unit.
#' @param default_cap Integer `K_max` for every unit when `data` has
#'   no `cap_var` column, as `occ()` takes 1
#' @param default_cap_buffer Integer added to each unit's `Y_max` for
#'   its `K_max` when `data` has no `cap_var` column
#' @param compute_y_max Whether to compute `Y_max` and `K_max`, which
#'   bound the latent count of the detection families. A
#'   multi-response family passes `FALSE`. Its density takes the
#'   response vector with no truncation.
#' @param unit_grouping_vars Columns that key a unit. The default,
#'   `c(series_var, time_var)`, makes each (species, site) pair a unit
#'   of replicate visits. A multi-response family keys on `time_var`
#'   alone. Its unit is a site, whose visits are the components.
#' @param series_levels The fitted series levels, which number the
#'   components in `visit_component`. NULL takes the levels of
#'   `data`.
#' @param family A closure-unit family, or NULL. A family sets
#'   `default_cap`, `default_cap_buffer` and `compute_y_max` in place
#'   of those arguments. It also sets the default of
#'   `unit_grouping_vars`.
#' @param drop_unobserved_units Whether to drop a unit whose every
#'   response is missing. Such a unit has no density. Stan bounds
#'   `n_rep` below by one. The fit drops these units and refuses a
#'   frame holding no other. A forecast grid holds no response.
#'   `closure_unit_arrays_for()` passes `FALSE` for it, keeping every
#'   unit with `n_rep` zero.
#' @return Named list with elements `N_unit`, `n_rep`, `K_max`,
#'   `Y_max`, `visit_idx`, `visit_row`, `visit_component`, `row_unit`,
#'   `max_rep`, `unit_labels`, `unit_grid` and `unit_vars`. `K_max` and
#'   `Y_max` are `NA` when `compute_y_max = FALSE`. `visit_component`
#'   is NULL unless the key omits `series_var`. `unit_vars` records the
#'   grouping this call resolved, for consumers that need the axis.
#'
#'   `visit_idx` and `visit_row` index the same visits in two
#'   numberings. `visit_idx` numbers the rows brms retained, which the
#'   Stan data indexes. brms drops a row whose response is missing.
#'   `visit_row` numbers the rows of the frame as supplied, which every
#'   post-fit path takes, as `posterior_linpred()` gives one value per
#'   row of `newdata`. `row_unit` gives the unit of every row,
#'   including a visit that never happened. Such a visit has no density
#'   and still has an expected value.
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
                                       series_levels = NULL,
                                       drop_unobserved_units = TRUE,
                                       family = NULL) {
  checkmate::assert_data_frame(data, min.rows = 1L)
  if (!is.null(family)) {
    default_cap <- closure_unit_default_cap(family)
    default_cap_buffer <- closure_unit_default_cap(family, buffer = TRUE)
    compute_y_max <- !is_multi_response_family(family)
    unit_grouping_vars <- unit_grouping_vars %||%
      closure_unit_key_vars(family, series_var, time_var)
  }
  checkmate::assert_string(response_var)
  checkmate::assert_string(series_var)
  checkmate::assert_string(time_var)
  checkmate::assert_string(cap_var)
  checkmate::assert_integerish(default_cap, lower = 1L, len = 1L,
                               null.ok = TRUE)
  checkmate::assert_integerish(default_cap_buffer, lower = 0L,
                               len = 1L, null.ok = TRUE)
  checkmate::assert_flag(compute_y_max)
  checkmate::assert_character(series_levels, min.len = 1L,
                              any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_flag(drop_unobserved_units)
  if (is.null(unit_grouping_vars)) {
    # No family is in scope here. The accessor supplies the default key
    # alone. The default is defined in one place. Copies cannot drift.
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
  assert_closure_unit_columns(data, required_cols)
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
  # The grouping values for each unit, one row per unit in unit
  # order. Callers that label units, such as
  # `hindcast(type = "latent_state")`, take the labels from here. They
  # do not rebuild the grid and sort by series. This ordering is
  # first-appearance over time-major data. Sorting by series would relabel
  # every unit on a multi-series fit.
  unit_grid <- data[match(unit_levels, unit_label),
                    unit_grouping_vars, drop = FALSE]
  rownames(unit_grid) <- NULL
  # Per-unit visit counts indexed in `unit_levels` order. `table`
  # would re-order alphabetically. An explicit tabulate keeps the unit
  # ordering deterministic.
  # A missing response is a visit that did not happen. brms drops those
  # rows from the likelihood. The unit arrays are built over the observed
  # visits. `visit_idx` holds positions in brms's retained rows. Those
  # positions differ from the rows of the raw frame. Units with no
  # observed visit have no information about detection. They are
  # dropped. Their timepoints still appear in the latent process, which
  # spans the full grid.
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
  # The key above is integer-coded. Grouping columns of any type can then
  # be pasted together. That makes it unusable as a label. A reader given
  # `1_1` cannot recover the site whose survey effort it describes. The
  # label pastes the values the user supplied, one per unit in unit order.
  # Every output that reports one row per unit prints this label.
  unit_labels <- do.call(
    paste, c(lapply(unit_grid, as.character), list(sep = "_"))
  )
  rep_counts   <- lengths(rows_by_unit)
  # At least one padding column keeps the documented matrix shape when a
  # frame carries no response at all. A fit-time frame always has an
  # observed unit. The padding changes nothing there.
  max_rep <- max(rep_counts, 1L)
  # The two coordinate systems cannot be used interchangeably.
  # `visit_idx` numbers the visits brms retained. The Stan data indexes
  # those visits, because brms drops a row whose response is missing.
  # `visit_row` numbers the rows of the frame as supplied. Every
  # post-fit path needs that numbering, because `posterior_linpred()`
  # gives one value for each row of `newdata`. The response column comes
  # from that frame too. Mixing the two shifts a unit's visits by the
  # number of missing responses before it. Both are returned. Each caller
  # names the one it means.
  visit_idx <- matrix(1L, nrow = n_unit, ncol = max_rep)
  visit_row <- matrix(NA_integer_, nrow = n_unit, ncol = max_rep)
  for (g in seq_len(n_unit)) {
    visit_idx[g, seq_len(rep_counts[g])] <-
      as.integer(retained_pos[rows_by_unit[[g]]])
    visit_row[g, seq_len(rep_counts[g])] <-
      as.integer(rows_by_unit[[g]])
  }
  # Which response component each of a unit's rows holds. Only a unit
  # whose rows are components has one. A unit of repeat visits does not.
  # That is the case when the key omits the series var. The generated
  # lpdf uses this index to pair a row with that component's residual
  # scale. Pairing by position is valid only while every unit holds every
  # component in order. A unit missing one component would shift each
  # later component onto another's scale.
  visit_component <- NULL
  if (!series_var %in% unit_grouping_vars &&
        series_var %in% names(data)) {
    assert_axis_column(data, series_var, "series",
                       require_factor = is.null(series_levels))
    series_col <- data[[series_var]]
    # The fitted levels arrive from the caller, which has the fit in
    # scope. Numbering off the frame alone shifts every later
    # component wherever a unit omits one.
    levs <- series_levels %||% levels(as.factor(series_col))
    component <- match(as.character(series_col), levs)
    if (anyNA(component)) {
      unknown <- unique(as.character(series_col)[is.na(component)])
      stop(insight::format_error(c(
        "A row refers to a series outside the fitted set.",
        x = paste0("Found: ", paste(unknown, collapse = ", "), "."),
        i = paste0("Fitted series are ", paste(levs, collapse = ", "), ".")
      )), call. = FALSE)
    }
    visit_component <- matrix(1L, nrow = n_unit, ncol = max_rep)
    for (g in seq_len(n_unit)) {
      visit_component[g, seq_len(rep_counts[g])] <-
        component[rows_by_unit[[g]]]
    }
  }
  # Which unit each row of the frame belongs to, over every row, observed
  # or not. A visit that never happened still has a linear predictor and
  # an expected value. Only its density is absent. Deriving this here,
  # where the units are formed, lets a per-visit surface be broadcast
  # from a per-unit quantity without reconstructing the grouping. `NA`
  # marks a row whose unit had no observed visit and was dropped above.
  row_unit <- match(unit_int, which(visited))
  arrays_with <- function(K_max, Y_max) {
    list(
      N_unit          = n_unit,
      n_rep           = rep_counts,
      K_max           = K_max,
      Y_max           = Y_max,
      visit_idx       = visit_idx,
      visit_component = visit_component,
      visit_row       = visit_row,
      row_unit        = row_unit,
      max_rep         = as.integer(max_rep),
      unit_labels     = unit_labels,
      unit_grid       = unit_grid,
      unit_vars       = unit_grouping_vars
    )
  }
  # A multi-response density takes the K components of a unit with no
  # truncation and needs neither bound
  if (!compute_y_max) {
    return(arrays_with(NA_integer_, NA_integer_))
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
    # The cap bounds a unit's latent state. It is a property of the unit,
    # and it is recorded on every one of its rows. The cap is taken over
    # all rows of the unit, including units with no observed row. Taking it
    # over observed rows alone would leave such a unit empty. Its `K_max[g]`
    # would then be `NA`. The comparison below treats that as a missing
    # condition.
    unit_rows <- list(row_unit = row_unit, N_unit = n_unit)
    refuse_unit_varying(cap_vals, unit_rows, cap_var,
                        "The cap bounds the latent state of the unit.")
    rows_of_unit <- closure_unit_row_split(unit_rows)
    for (g in seq_len(n_unit)) {
      K_max[g] <- cap_vals[rows_of_unit[[g]][1L]]
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
            "Drop the '", cap_var, "' column to use the data-driven ",
            "default."
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
  arrays_with(K_max, Y_max)
}

#' N-mixture family for repeated counts of unmarked individuals
#'
#' For counts made on repeated visits to sites whose abundance stays
#' fixed across the visits, such as bird point counts within a season.
#' The data hold one row per visit. The visits sharing a `series` and a
#' `time` belong to one closure unit, whose latent abundance `N` is
#' Poisson with mean `mu`. Each visit's count depends on `N` and the
#' detection parameter `p`. The likelihood sums over `N` from the
#' unit's largest count up to the cap `K_max` (Royle 2004).
#'
#' `bf(y ~ elev, p ~ tod)` models abundance and detection, each with
#' its own formula. `mu` takes the log link. [predict()] with
#' `type = "latent_state"` gives the expected abundance of each unit
#' given its counts. `type = "detection"` gives the detection
#' probability of each visit.
#'
#' @param type The variant, `"poisson_binomial"` by default. See
#'   **Choosing a variant**.
#' @param multi_season Logical. `TRUE` keys a closure unit on
#'   `(series, site, time)`, for several seasons of visits to each
#'   site. The data then need a `site` column, which
#'   [pivot_detection_array()] creates from a four-dimensional array
#'   with `multi_season = "hierarchical"`. Each unit holds the visits of
#'   one site in one season.
#'
#' @section Choosing a variant:
#' All three variants take `N ~ Poisson(mu)` and differ in what a visit
#' records.
#' \describe{
#'   \item{`"poisson_binomial"` (Royle 2004)}{Counts
#'     `y ~ Binomial(N, p)`, which never exceed `N`. Point counts,
#'     removal passes and plot tallies fit this design. `p` is the
#'     detection probability of each individual, on the logit link.}
#'   \item{`"royle_nichols"` (Royle and Nichols 2003)}{Detections
#'     `y ~ Bernoulli(1 - (1 - p)^N)` in 0 / 1. Camera-trap or eDNA
#'     detections scored as present or absent fit this design. `p` is
#'     the detection probability of each individual, on the logit
#'     link. A visit's detection probability rises with `N`.}
#'   \item{`"poisson_poisson"` (Neyman 1939)}{Counts
#'     `y ~ Poisson(N * p)`, which can exceed `N`. Camera-trap events
#'     and acoustic detections, where one individual is recorded
#'     several times, fit this design. `p` is the encounter rate of
#'     each individual per visit, on the log link.}
#' }
#'
#' @section Priors:
#' A detection probability `p` on the logit link takes `beta(1, 1)` as
#' a scalar. With a formula, its intercept takes `normal(0, 1.5)`,
#' which puts most of the probability between 0.05 and 0.95. The
#' encounter rate of `"poisson_poisson"` takes `lognormal(0, 1)` as a
#' scalar. With a formula, its intercept takes `std_normal()`. Either
#' puts most of the rate between 0.14 and 7.4 encounters per visit.
#' [get_prior()] lists these defaults. `prior(..., dpar = "p")`
#' replaces one. A rare species might take
#' `prior(normal(-2, 0.5), class = Intercept, dpar = "p")`.
#'
#' @section Identification:
#' The counts identify abundance and detection apart through repeated
#' visits and through covariates that act on one of them. With one
#' visit per unit and no covariate, the data identify only the product
#' `mu * p`. `nmix()` refuses that model. More than 30% of units
#' visited once raises a warning. A `"poisson_poisson"` model with
#' intercepts alone separates `mu` from `p` only through the extra
#' variance of its counts. It warns. An informative prior on one
#' intercept separates them.
#'
#' Overdispersed abundance, such as negative binomial counts fitted by
#' a Poisson `N`, biases abundance up and detection down, leaving their
#' product close to the truth (Knape et al. 2018). [pp_check()] with
#' `type = "fit_stat"` tests the per-unit totals for such misfit.
#'
#' @section The cap on abundance:
#' A `cap` column, constant within each unit and at least its largest
#' count, sets the cap `K_max`. Without one, `"poisson_binomial"` and
#' `"poisson_poisson"` take each unit's largest count plus 100, as
#' `unmarked::pcount()` does. `"royle_nichols"` takes a fixed 25, as
#' `unmarked::occuRN()` does. Its 0 / 1 detections carry no count to
#' build a cap from.
#' A lower cap shortens the sum and speeds sampling. A cap that binds
#' biases abundance down. [latent_N_saturation()] reports, for each
#' unit, the posterior probability that `N` equals its cap. A
#' prediction frame may carry its own `cap`.
#'
#' @section Joint species models:
#' [jsdgam()] with `nmix()` or `occ()` fits several species at once.
#' Latent factors shared by the species enter the state `mu`. `p`
#' keeps its own formula. `factor_formula = ~ s(env, by = lv_axis())`
#' lets each factor follow an environmental gradient.
#'
#' @section Importing array data:
#' Detection histories from \pkg{unmarked}, \pkg{spOccupancy} or
#' \pkg{ubms} come as a site-by-visit matrix, an array with species or
#' seasons as further dimensions, or a list of matrices.
#' [pivot_detection_array()] converts any of these to the long format,
#' broadcasting site, season and visit covariates to their rows.
#'
#' @section Threading:
#' The density sums over units through Stan's `reduce_sum()`. Fitting
#' with `threads = 2` or more splits the units across threads.
#'
#' @return A family object to pass to [mvgam()] or [jsdgam()].
#'
#' @references
#' Royle, J. A. (2004). N-mixture models for estimating
#'   population size from spatially replicated counts.
#'   *Biometrics*, 60, 108-115.
#'   \doi{10.1111/j.0006-341X.2004.00142.x}
#'
#' Royle, J. A. and Nichols, J. D. (2003). Estimating abundance
#'   from repeated presence-absence data or point counts.
#'   *Ecology*, 84, 777-790.
#'   \doi{10.1890/0012-9658(2003)084[0777:EAFRPA]2.0.CO;2}
#'
#' Neyman, J. (1939). On a new class of "contagious" distributions,
#'   applicable in entomology and bacteriology. *Annals of
#'   Mathematical Statistics*, 10, 35-57.
#'   \doi{10.1214/aoms/1177732245}
#'
#' Knape, J., Arlt, D., Barraquand, F., Berg, A., Chevalier, M.,
#'   Part, T., Ruete, A. and Zmihorski, M. (2018). Sensitivity of
#'   binomial N-mixture models to overdispersion. *Methods in
#'   Ecology and Evolution*, 9, 2102-2114.
#'   \doi{10.1111/2041-210X.13062}
#'
#' @examples
#' \dontrun{
#' # Counts at 50 sites, each visited four times. The state `mu` is
#' # the expected abundance and `p` the detection probability.
#' set.seed(5)
#' simdat <- sim_closure_unit_data(family = nmix(), n_species = 1L,
#'                                 n_sites = 50L, n_visits = 4L,
#'                                 type = 1L)
#' mod <- mvgam(
#'   bf(y ~ env, p ~ tod_c),
#'   data   = simdat$data_train,
#'   family = nmix(),
#'   chains = 2,
#'   silent = 2
#' )
#' summary(mod)
#'
#' # Abundance and detection, separated by the model
#' head(predict(mod, type = "latent_state"))
#' head(predict(mod, type = "detection"))
#' plot(hindcast(mod, type = "latent_state"))
#'
#' # A goodness-of-fit test on the per-site totals. A Bayesian p-value
#' # near 0 or 1 marks a misfit.
#' pp_check(mod, type = "fit_stat", stat = "freeman_tukey",
#'          ndraws = 200L)
#' }
#'
#' @seealso [occ()] for the related joint-occupancy family and
#'   [pp_check.mvgam()] for closure-unit goodness-of-fit checks. The
#'   `r mvgam_article("nmix", "N-mixture article")` fits N-mixture and
#'   occupancy models from data preparation to inference. The
#'   `r mvgam_article("data", "data article")` sets out the row layout
#'   that `nmix()` expects.
#'
#' @family observation families
#' @export
nmix <- function(type = c("poisson_binomial", "royle_nichols",
                          "poisson_poisson"),
                 multi_season = FALSE) {
  type <- match.arg(type)
  # The count variants bound a unit's abundance a buffer above its
  # largest count, as unmarked::pcount() does. Royle-Nichols detections
  # give no floor and take unmarked::occuRN()'s fixed cap.
  counts <- c("K_max", "Y_max", "visit_idx", "log_n_lookup")
  fam <- switch(
    type,
    poisson_binomial = detection_family(
      "nmix", c("log", "logit"), ub = c(NA, 1), vars = counts,
      default_cap_buffer = 100L, multi_season = multi_season
    ),
    royle_nichols = detection_family(
      "nmix_royle_nichols", c("log", "logit"), ub = c(NA, 1),
      vars = c("K_max", "Y_max", "visit_idx"), binary = TRUE,
      default_cap = 25L, multi_season = multi_season
    ),
    # The encounter rate is unbounded above
    poisson_poisson = detection_family(
      "nmix_poisson_poisson", c("log", "log"), ub = c(NA, NA),
      vars = c(counts, "k_start_ppm"), default_cap_buffer = 100L,
      multi_season = multi_season
    )
  )
  attr(fam, "mvgam_nmix_type") <- type
  fam
}

#' Occupancy family for repeated detection and non-detection
#'
#' For detections recorded on repeated visits to sites whose occupancy
#' stays fixed across the visits, such as camera-trap or call surveys
#' within a season. The data hold one row per visit with a 0 / 1
#' response. The visits sharing a `series` and a `time` belong to one
#' closure unit, which is occupied with probability `mu`. A visit to an
#' occupied unit detects the species with probability `p`. The
#' likelihood sums over the unit's two states. A unit with a detection
#' is occupied for certain (MacKenzie et al. 2002).
#'
#' `bf(y ~ elev, p ~ tod)` models occupancy and detection, each with
#' its own formula on the logit link. [predict()] with
#' `type = "latent_state"` gives the probability that each unit is
#' occupied given its detections. `type = "detection"` gives the
#' detection probability of each visit. The data need no `cap` column.
#'
#' @inheritParams nmix
#'
#' @section Priors:
#' The occupancy intercept takes brms's `student_t(3, 0, 2.5)` on the
#' logit scale. A scalar `p` takes `beta(1, 1)`. With a formula, its
#' intercept takes `normal(0, 1.5)`. [get_prior()] lists the defaults,
#' and `prior(..., dpar = "p")` replaces one.
#'
#' @section Identification:
#' The detections identify occupancy and detection apart through
#' repeated visits and through covariates that act on one of them.
#' With one visit per unit and no covariate, the data identify only
#' the product `mu * p`. Only the priors then set the two apart (Royle
#' and Dorazio 2008). [mvgam()] warns on such data. It also warns when
#' more than 30% of units have a single visit.
#'
#' @inheritSection nmix Joint species models
#' @inheritSection nmix Importing array data
#' @inheritSection nmix Threading
#'
#' @return A family object to pass to [mvgam()] or [jsdgam()].
#'
#' @references
#' MacKenzie, D. I., Nichols, J. D., Lachman, G. B., Droege, S.,
#'   Royle, J. A. and Langtimm, C. A. (2002). Estimating site
#'   occupancy rates when detection probabilities are less than
#'   one. *Ecology*, 83, 2248-2255.
#'   \doi{10.1890/0012-9658(2002)083[2248:ESORWD]2.0.CO;2}
#'
#' Royle, J. A. and Dorazio, R. M. (2008). *Hierarchical Modeling
#'   and Inference in Ecology*. Academic Press.
#'
#' @examples
#' \dontrun{
#' # Detections at 50 sites, each visited four times, with covariates
#' # on occupancy and on detection
#' set.seed(6)
#' simdat <- sim_closure_unit_data(family = occ(), n_species = 1L,
#'                                 n_sites = 50L, n_visits = 4L,
#'                                 type = 1L)
#' mod <- mvgam(
#'   bf(y ~ env, p ~ tod_c),
#'   data   = simdat$data_train,
#'   family = occ(),
#'   chains = 2,
#'   silent = 2
#' )
#' summary(mod)
#'
#' # Occupancy along the gradient and detection through the day
#' conditional_effects(mod, effects = "env", type = "latent_state")
#' conditional_effects(mod, effects = "tod_c", type = "detection")
#'
#' # Occupancy and detection, separated by the model
#' head(predict(mod, type = "latent_state"))
#' head(predict(mod, type = "detection"))
#'
#' # Next, pp_check(mod, type = "fit_stat") tests the fit of each unit
#' }
#'
#' @seealso [nmix()] for the related N-mixture family and
#'   [pp_check.mvgam()] for closure-unit goodness-of-fit checks. The
#'   `r mvgam_article("nmix", "N-mixture article")` fits occupancy and
#'   N-mixture models from data preparation to inference. The
#'   `r mvgam_article("data", "data article")` sets out the row layout
#'   that `occ()` expects.
#'
#' @family observation families
#' @export
occ <- function(multi_season = FALSE) {
  # The state is binary, which caps every unit at one. `Y_max` marks a
  # unit with a detection, which the density treats as occupied.
  detection_family(
    "occ", c("logit", "logit"), ub = c(1, 1),
    vars = c("Y_max", "visit_idx"), binary = TRUE, default_cap = 1L,
    multi_season = multi_season
  )
}

#' Stan lines that dispatch a closure-unit partial sum to reduce_sum
#'
#' Every closure-unit lpmf wrapper builds the unit index `g_seq`,
#' picks a grainsize and returns the `reduce_sum()` call. The wrapper
#' builds `g_seq` locally, which keeps the index out of the data that
#' brms passes to the likelihood. The grainsize targets about eight
#' chunks. Without threading that costs eight serial dispatches per
#' leapfrog step, against `N_unit` at a grainsize of one. With up to
#' eight threads TBB maps the chunks across cores.
#'
#' @param partial_sum Name of the Stan partial-sum function.
#' @param args Character vector of the indented argument lines that
#'   follow `g_seq, grainsize` in the call.
#' @return Character vector of Stan lines.
#' @noRd
closure_unit_reduce_sum_lines <- function(partial_sum, args) {
  checkmate::assert_string(partial_sum)
  checkmate::assert_character(args, min.len = 1L)
  c(
    "    array[N_unit] int g_seq;",
    "    for (g in 1 : N_unit) g_seq[g] = g;",
    "    int grainsize = N_unit >= 8 ? N_unit %/% 8 : 1;",
    "    return reduce_sum(",
    paste0("      ", partial_sum, ", g_seq, grainsize,"),
    args,
    "    );"
  )
}

#' The comment above a closure-unit family's partial-sum function
#'
#' @param name The family's Stan name, such as `"nmix"`
#' @param hoisted The link-scale vectors its wrapper computes once
#' @return Lines of Stan comment
#' @noRd
partial_sum_header <- function(name, hoisted) {
  c(
    paste0("  // reduce_sum, called from ", name, "_lpmf(), passes this"),
    "  // function a slice of the closure-unit index and sums the",
    "  // slice's log likelihood. A model compiled with stan_threads",
    "  // runs the slices on separate threads. The wrapper computes",
    paste0("  // ", hoisted, " once for every slice.")
  )
}

#' Stan functions for the `occ()` density
#'
#' A unit with a detection is occupied. Its log likelihood is
#' `log(psi)` plus the Bernoulli detections. A unit without one sums
#' the occupied and unoccupied branches through `log_sum_exp()`
#' (MacKenzie et al. 2002). The wrapper converts `mu` and `p` back to
#' the logit scale. `log_inv_logit()` and `log1m_inv_logit()` then stay
#' finite at probabilities near 0 or 1.
#'
#' @return Character scalar of Stan function code.
#' @noRd
occ_stan_funs <- function() {
  args <- closure_unit_density_args(occ())
  paste(c(
    partial_sum_header("occ", "logit_psi and logit_p"),
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
    stan_density_header("occ", "lpmf", args),
    "    vector[num_elements(mu)] logit_psi = logit(mu);",
    "    vector[num_elements(p)]  logit_p   = logit(p);",
    closure_unit_reduce_sum_lines(
      "partial_sum_occ_lpmf",
      "      y, logit_psi, logit_p, n_rep, Y_max, visit_idx"
    ),
    "  }",
    broadcast_overloads("occ", "lpmf", args, "p")
  ), collapse = "\n")
}

#' Stan code and data of an `occ()` fit
#'
#' The occupancy state is binary and takes no `K_max`. `Y_max` is
#' bounded by 1, as the response is. `Y_max[g] = 1` marks a unit with
#' at least one detection, which the density treats as occupied.
#'
#' @inheritParams make_closure_unit_arrays_stanvars
#' @return A `brmsstanvars` object
#' @noRd
make_occ_stanvars <- function(arrays) {
  make_closure_unit_arrays_stanvars(
    arrays,
    family_funs_name = "occ_funs",
    family_funs = occ_stan_funs(),
    y_max_upper = 1L,
    include_K_max = FALSE
  )
}

#' A family whose likelihood takes a vector of responses per unit
#'
#' `diri()`, `multi()`, `categ()`, `mvn()` and `mvt()` share their
#' construction. Each closure unit holds one row per response
#' component. brms passes the indexing arrays in `mvgam_vars` to the
#' family's Stan density, which assembles each unit's vector.
#'
#' @param name Family name, which names its Stan density
#' @param type `"real"` or `"int"`, the type of the response
#' @param dpars,links,lb,ub The distributional parameters with their
#'   links and bounds, as [brms::custom_family()] takes them
#' @param simplex Logical. `TRUE` marks a family whose density applies
#'   a softmax to each unit's linear predictors.
#' @param binary Logical. `TRUE` marks a one-hot binary response.
#' @param vars Character vector of the Stan arguments the density takes
#'   after the indexing arrays
#' @return A `customfamily` carrying the attributes that route the
#'   closure-unit data preparation, validation and Stan code
#' @noRd
multi_response_family <- function(name, type, dpars = "mu",
                                  links = "identity", lb = NA, ub = NA,
                                  simplex = FALSE, binary = FALSE,
                                  vars = character(0L)) {
  checkmate::assert_flag(simplex)
  fam <- closure_unit_family(
    name, type, dpars = dpars, links = links, lb = lb, ub = ub,
    binary = binary, vars = c("N_unit", "n_rep", "visit_idx", vars)
  )
  attr(fam, "mvgam_multi_response") <- TRUE
  if (simplex) {
    attr(fam, "mvgam_simplex_response") <- TRUE
  }
  fam
}


#' A custom family laid out in closure units
#'
#' The construction the detection and multi-response families share.
#'
#' @param name Family name, which names its Stan density
#' @param type `"real"` or `"int"`, the type of the response
#' @param dpars,links,lb,ub The distributional parameters with their
#'   links and bounds, as [brms::custom_family()] takes them. The bounds
#'   let brms declare a scalar parameter in its range.
#' @param binary Logical. `TRUE` restricts the response to 0 / 1.
#' @param vars Character vector of the Stan data the density takes
#' @param predict_types The `predict()` types the family adds
#' @return A `customfamily` carrying the attributes that route the
#'   closure-unit data preparation, validation and Stan code
#' @noRd
closure_unit_family <- function(name, type, dpars, links, lb, ub,
                                binary = FALSE, vars,
                                predict_types = character(0L)) {
  checkmate::assert_string(name)
  checkmate::assert_choice(type, c("real", "int"))
  checkmate::assert_flag(binary)
  checkmate::assert_character(vars, min.len = 1L)
  fam <- brms::custom_family(
    name = name, dpars = dpars, links = links, lb = lb, ub = ub,
    type = type, loop = FALSE
  )
  link_info <- stats::make.link(fam$link)
  fam$linkinv <- link_info$linkinv
  fam$linkfun <- link_info$linkfun
  attr(fam, "mvgam_closure_unit") <- TRUE
  if (binary) {
    attr(fam, "mvgam_binary_response") <- TRUE
  }
  attr(fam, "mvgam_predict_types") <- predict_types
  attr(fam, "mvgam_vars") <- vars
  fam
}


#' A detection family over closure units
#'
#' `nmix()` and its variants and `occ()` share their construction. `mu`
#' is the state parameter and `p` the detection parameter, both bounded
#' below by zero.
#'
#' @param name Family name, which names its Stan density
#' @param links,ub Links and upper bounds of `mu` and `p`
#' @param vars The Stan data the density takes after `N_unit` and
#'   `n_rep`, in its argument order
#' @param binary Logical. `TRUE` restricts the response to 0 / 1.
#' @param default_cap,default_cap_buffer The cap of a unit without a
#'   `cap` column: a fixed value, or a buffer above its largest count
#' @param multi_season Logical. `TRUE` keys a unit on
#'   `(series, site, time)`.
#' @return A `customfamily`
#' @noRd
detection_family <- function(name, links, ub, vars, binary = FALSE,
                             default_cap = NULL, default_cap_buffer = NULL,
                             multi_season = FALSE) {
  checkmate::assert_int(default_cap, lower = 1L, null.ok = TRUE)
  checkmate::assert_int(default_cap_buffer, lower = 0L, null.ok = TRUE)
  checkmate::assert_flag(multi_season)
  fam <- closure_unit_family(
    name, "int", dpars = c("mu", "p"), links = links, lb = c(0, 0),
    ub = ub, binary = binary, vars = c("N_unit", "n_rep", vars),
    predict_types = c("latent_state", "detection")
  )
  attr(fam, "mvgam_default_cap") <- default_cap
  attr(fam, "mvgam_default_cap_buffer") <- default_cap_buffer
  if (multi_season) {
    attr(fam, "mvgam_unit_grouping") <- c("series", "site", "time")
  }
  fam
}

# Stan types of the data and parameters a closure-unit density takes
# after its distributional parameters
CLOSURE_UNIT_ARG_TYPES <- c(
  N_unit = "int", n_rep = "array[] int", visit_idx = "array[,] int",
  visit_component = "array[,] int", K_max = "array[] int",
  Y_max = "array[] int", log_n_lookup = "vector",
  k_start_ppm = "array[] int", Psi = "vector", nu = "real"
)

#' The arguments of a closure-unit family's Stan density
#'
#' @param family A closure-unit family
#' @return Named character vector of Stan types, in argument order:
#'   the response, the distributional parameters, then `mvgam_vars`
#' @noRd
closure_unit_density_args <- function(family) {
  vars <- attr(family, "mvgam_vars", exact = TRUE)
  c(
    y = if (identical(family$type, "int")) "array[] int" else "vector",
    stats::setNames(rep("vector", length(family$dpars)), family$dpars),
    CLOSURE_UNIT_ARG_TYPES[vars]
  )
}

#' The opening lines of a Stan density
#'
#' @param name Family name
#' @param kind `"lpdf"` or `"lpmf"`
#' @param args Named character vector of Stan types, in argument order
#' @return Character vector of Stan lines
#' @noRd
stan_density_header <- function(name, kind, args) {
  checkmate::assert_character(args, min.len = 1L, names = "unique")
  ends <- c(rep(",", length(args) - 1L), ") {")
  c(paste0("  real ", name, "_", kind, "("),
    paste0("    ", args, " ", names(args), ends))
}

#' Stan overloads taking scalar distributional parameters
#'
#' brms passes a distributional parameter as a scalar when it has no
#' formula. One overload per subset of `scalar` declares those
#' parameters `real` and calls the vector density with them repeated.
#'
#' @inheritParams stan_density_header
#' @param scalar Names of the parameters that may arrive as scalars
#' @return Character vector of Stan lines
#' @noRd
broadcast_overloads <- function(name, kind, args, scalar) {
  checkmate::assert_subset(scalar, names(args))
  subsets <- unlist(lapply(seq_along(scalar), function(k) {
    utils::combn(scalar, k, simplify = FALSE)
  }), recursive = FALSE)
  unlist(lapply(subsets, function(sub) {
    types <- args
    types[sub] <- "real"
    call <- ifelse(names(args) %in% sub,
                   paste0("rep_vector(", names(args), ", N)"), names(args))
    c(
      "",
      stan_density_header(name, kind, types),
      "    int N = num_elements(y);",
      paste0("    return ", name, "_", kind, "(", call[1L], " | ",
             paste(call[-1L], collapse = ", "), ");"),
      "  }"
    )
  }))
}

#' The Stan density of a family that takes a vector per unit
#'
#' The density loops over the closure units. In each it gathers the
#' unit's responses and linear predictors through `visit_idx` and adds
#' the unit's log density to `lp`.
#'
#' @param family The family, whose name, response type, parameters and
#'   `mvgam_vars` set the density's signature
#' @param unit_lines Stan statements run for each unit, after `Kg`,
#'   `idx` and `y_unit` are declared
#' @return Character vector of Stan lines
#' @noRd
closure_unit_density <- function(family, unit_lines) {
  checkmate::assert_character(unit_lines, min.len = 1L)
  kind <- if (identical(family$type, "int")) "lpmf" else "lpdf"
  y_unit <- if (kind == "lpmf") {
    "array[Kg] int y_unit  = y[idx];"
  } else {
    "vector[Kg] y_unit  = y[idx];"
  }
  c(
    stan_density_header(family$name, kind,
                        closure_unit_density_args(family)),
    "    real lp = 0;",
    "    for (g in 1:N_unit) {",
    "      int Kg = n_rep[g];",
    "      array[Kg] int idx = visit_idx[g, 1:Kg];",
    paste0("      ", c(y_unit, unit_lines)),
    "    }",
    "    return lp;",
    "  }"
  )
}

#' The linear predictors of a unit, shifted to the first component
#'
#' A softmax is unchanged by a shift shared across a unit's
#' components. Subtracting the first component's predictor removes that
#' shift, as brms's reference category does.
#'
#' @noRd
SIMPLEX_MU_UNIT <- "vector[Kg] mu_unit = mu[idx] - mu[idx[1]];"

#' The Stan statements of the families with a residual scale per
#' component
#'
#' `mvn()` and `mvt()` give each component its own scale `Psi`.
#'
#' @param density The statement adding a unit's log density to `lp`
#' @return Character vector of Stan lines
#' @noRd
component_scale_unit_lines <- function(density) {
  c(
    "vector[Kg] mu_unit = mu[idx];",
    "vector[Kg] psi_unit = Psi[visit_component[g, 1:Kg]];",
    density
  )
}

#' The Stan code a multi-response family adds to a model
#'
#' The family's density and the closure-unit indexing arrays, with the
#' residual parameters of `mvn()` and `mvt()` from
#' `component_scale_stanvars()`. A unit's likelihood takes its whole
#' response vector, which leaves no truncation bound to declare.
#'
#' @inheritParams make_closure_unit_arrays_stanvars
#' @param family The family object
#' @param prior A `brmsprior`, or `NULL`. A row with `class = "Psi"`
#'   or `class = "nu"` sets the prior on that parameter.
#' @return A `brmsstanvars` object
#' @noRd
make_multi_response_stanvars <- function(arrays, family, prior = NULL) {
  checkmate::assert_true(is_multi_response_family(family))
  name <- family$name
  funs <- switch(
    name,
    diri = diri_stan_funs(),
    multi = multi_stan_funs(),
    categ = categ_stan_funs(),
    mvn = mvn_stan_funs(),
    mvt = mvt_stan_funs()
  )
  has_scale <- family_has_component_scale(family)
  stanvars <- make_closure_unit_arrays_stanvars(
    arrays,
    family_funs_name = paste0(name, "_funs"),
    family_funs = funs,
    include_K_max = FALSE,
    include_Y_max = FALSE,
    include_component = has_scale
  )
  if (has_scale) {
    stanvars <- combine_stanvars(
      stanvars,
      component_scale_stanvars(as.integer(arrays$max_rep), name, prior)
    )
  }
  stanvars
}

#' Dirichlet family for compositions
#'
#' For proportions measured together on each unit that sum to one, such
#' as the cover shares of several species at a site. The data hold one
#' row per component at each unit, whose responses sum to one within
#' the unit. Each unit's vector of `K` shares is Dirichlet with
#' concentrations `softmax(mu) * phi`. `mu` holds the components'
#' linear predictors. `phi` sets how tightly the shares gather around
#' `softmax(mu)` (Aitchison 1982).
#'
#' [jsdgam()] fits the family, taking the unit and component columns
#' and the number of latent factors `n_lv`. The factors give the
#' components correlated departures from their expected shares
#' (Warton et al. 2015).
#'
#' @section Identification:
#' The softmax is unchanged when the same constant is added to every
#' component. The data then identify neither a shift of the linear
#' predictors that every component shares nor a shift of the loadings
#' that every component shares. mvgam removes both:
#' \itemize{
#'   \item The first component is the reference. Its linear predictor
#'     is subtracted from every component's before the softmax, as in
#'     brms's `dirichlet()` family. A term that acts on every
#'     component alike, such as `Intercept` or `env` in
#'     `y ~ env * series`, then cancels. Its posterior is its prior,
#'     `student_t(3, 0, 2.5)` by default. The `series` terms are
#'     contrasts with the first component. `jsdgam()` warns when the
#'     formula has no term that varies by component.
#'   \item Each column of the loadings `Z` sums to zero, through
#'     Stan's `sum_to_zero_vector` type.
#' }
#' A `loadings_prior` combines with that constraint.
#'
#' @section Regression on `phi`:
#' `bf(y ~ env * series, phi ~ env)` models the concentration. A unit's
#' density takes one `phi`. Its covariates must take one value per
#' unit. mvgam refuses a covariate that varies within a unit.
#' The coefficients of `phi` take a flat prior by default.
#' `prior(student_t(3, 0, 2.5), class = b, dpar = phi)` regularises
#' them.
#'
#' @section Post-fit conventions:
#' The density is joint over a unit's components, which makes the unit
#' the grain of `log_lik()`, `loo()` and `waic()` (Vehtari et al.
#' 2017). `log_lik()` gives each unit's log density on its first row
#' and `NA` on the others. `loo()` then counts each unit once.
#' `loo_compare()` needs every model it compares scored at that grain.
#'
#' `posterior_linpred()` gives `mu` per row before the reference
#' subtraction. `posterior_epred()` gives the expected shares, which
#' sum to one within each unit.
#'
#' @return A family object to pass to [jsdgam()] or [mvgam()].
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
#' # Shares of three categories at 60 sites along an environmental
#' # gradient
#' simdat <- sim_jsdgam(family = diri(), n_species = 3L, n_sites = 60L,
#'                      seed = 2L)
#' head(simdat$data_train)
#'
#' # `env * series` gives each category its own slope
#' fit <- jsdgam(
#'   formula        = y ~ env * series,
#'   factor_formula = ~ -1,
#'   data           = simdat$data_train,
#'   unit           = time, species = series,
#'   family         = diri(),
#'   n_lv           = 2L,
#'   chains         = 2L,
#'   silent         = 2
#' )
#'
#' # The species terms are contrasts with the first species. `Intercept`
#' # and `env` act on every species alike. The softmax cancels them. Their
#' # posteriors repeat the prior.
#' summary(fit)
#'
#' # The expected shares move along the gradient
#' conditional_effects(fit, effects = "env:series")
#'
#' # Each site's expected shares sum to one
#' pe <- posterior_epred(fit, ndraws = 50L)
#' first_site <- which(simdat$data_train$time == 1L)
#' rowSums(pe[1:5, first_site])
#' }
#'
#' @family observation families
#' @export
diri <- function() {
  multi_response_family(
    "diri", type = "real", dpars = c("mu", "phi"),
    links = c("identity", "log"), lb = c(NA, 0), ub = c(NA, NA),
    simplex = TRUE
  )
}

#' Stan functions of the [diri()] density
#'
#' brms calls `diri_lpdf()` once per log density evaluation, with the
#' long-format `Y` and `mu` and the closure-unit arrays `family$vars`
#' names. Each unit's shares are Dirichlet with concentrations
#' `softmax(mu_unit) * phi`, after the reference subtraction of
#' `SIMPLEX_MU_UNIT`.
#'
#' @return Stan code for the functions block
#' @noRd
diri_stan_funs <- function() {
  # `phi` is the concentration of the whole vector, which takes the
  # value on the unit's first row. brms passes a vector `phi` when it
  # has a formula and a scalar otherwise. The scalar form broadcasts.
  paste(c(
    closure_unit_density(
      diri(),
      unit_lines = c(
        SIMPLEX_MU_UNIT,
        "real phi_g = phi[idx[1]];",
        "lp += dirichlet_lpdf(y_unit | softmax(mu_unit) * phi_g);"
      )
    ),
    broadcast_overloads("diri", "lpdf", closure_unit_density_args(diri()),
                        "phi")
  ), collapse = "\n")
}

#' Multinomial family for counts over several categories
#'
#' For counts of several categories made together on each unit, such
#' as the sequence counts of several taxa in a sample. The data hold
#' one row per category at each unit. Each unit's vector of `K` counts
#' is multinomial with probabilities `softmax(mu)`, where `mu` holds
#' the categories' linear predictors. A unit's total is the sum of its
#' counts and may differ between units.
#'
#' [jsdgam()] fits the family, taking the unit and category columns
#' and the number of latent factors `n_lv`. The constraints that
#' identify the model are those of [diri()].
#'
#' @inheritSection diri Identification
#' @inheritSection diri Post-fit conventions
#'
#' @return A family object to pass to [jsdgam()] or [mvgam()].
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
#' # Counts of four categories at 60 sites, 30 per site
#' simdat <- sim_jsdgam(family = multi(), n_species = 4L, n_sites = 60L,
#'                      seed = 3L)
#'
#' fit <- jsdgam(
#'   formula        = y ~ env * series,
#'   factor_formula = ~ -1,
#'   data           = simdat$data_train,
#'   unit           = time, species = series,
#'   family         = multi(),
#'   n_lv           = 1L,
#'   chains         = 2L,
#'   silent         = 2
#' )
#'
#' # The probability of each category along the gradient
#' conditional_effects(fit, effects = "env:series")
#'
#' # Next, residual_cor(fit) gives the correlations the factor implies
#' }
#'
#' @family observation families
#' @export
multi <- function() {
  multi_response_family("multi", type = "int", simplex = TRUE)
}

#' Stan functions of the [multi()] density
#'
#' Each unit's counts are multinomial with probabilities
#' `softmax(mu_unit)`, after the reference subtraction of
#' `SIMPLEX_MU_UNIT`. The unit's total is the sum of its counts.
#'
#' @return Stan code for the functions block
#' @noRd
multi_stan_funs <- function() {
  paste(closure_unit_density(
    multi(),
    unit_lines = c(SIMPLEX_MU_UNIT,
                   "lp += multinomial_logit_lpmf(y_unit | mu_unit);")
  ), collapse = "\n")
}

#' Categorical family for one category observed per unit
#'
#' For a single category recorded on each unit, such as the dominant
#' species at a site. The data hold one row per category at each unit,
#' with response 1 on the observed category and 0 on the others. The
#' observed category is categorical with probabilities `softmax(mu)`,
#' where `mu` holds the categories' linear predictors.
#'
#' [jsdgam()] fits the family, taking the unit and category columns
#' and the number of latent factors `n_lv`. The constraints that
#' identify the model are those of [diri()]. A unit holds one trial.
#' It informs the factor loadings less than a [multi()] unit of many
#' counts does. Recovering the residual correlations needs more
#' units.
#'
#' @inheritSection diri Identification
#' @inheritSection diri Post-fit conventions
#'
#' @return A family object to pass to [jsdgam()] or [mvgam()].
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
#' # The one category observed at each of 150 sites
#' simdat <- sim_jsdgam(family = categ(), n_species = 3L,
#'                      n_sites = 150L, seed = 4L)
#'
#' fit <- jsdgam(
#'   formula        = y ~ env * series,
#'   factor_formula = ~ -1,
#'   data           = simdat$data_train,
#'   unit           = time, species = series,
#'   family         = categ(),
#'   n_lv           = 1L,
#'   chains         = 2L,
#'   silent         = 2
#' )
#'
#' # The probability of each category along the gradient
#' conditional_effects(fit, effects = "env:series")
#'
#' # Next, pp_check(fit) compares observed and replicated categories
#' }
#'
#' @family observation families
#' @export
categ <- function() {
  multi_response_family("categ", type = "int", simplex = TRUE,
                        binary = TRUE)
}

#' Stan functions of the [categ()] density
#'
#' The observed category is the position of the 1 in the unit's
#' responses. `refuse_off_simplex_response()` admits only units with
#' exactly one. The category is categorical with probabilities
#' `softmax(mu_unit)`, after the reference subtraction of
#' `SIMPLEX_MU_UNIT`.
#'
#' @return Stan code for the functions block
#' @noRd
categ_stan_funs <- function() {
  paste(closure_unit_density(
    categ(),
    unit_lines = c(
      SIMPLEX_MU_UNIT,
      "int cat_code = 0;",
      "for (k in 1:Kg) {",
      "  if (y_unit[k] == 1) {",
      "    cat_code = k;",
      "    break;",
      "  }",
      "}",
      "lp += categorical_logit_lpmf(cat_code | mu_unit);"
    )
  ), collapse = "\n")
}

#' Multivariate normal family for several continuous responses
#'
#' For continuous responses measured together on each unit, such as the
#' biomass of several species at a site. The data hold one row per
#' response component at each unit. Latent factors shared by a unit's
#' components give their residuals the covariance
#' `Z Z' + diag(Psi^2)`, where `Z` holds the factor loadings and `Psi`
#' each component's residual standard deviation (Hui et al. 2015; Niku
#' et al. 2019). Given the factors, each row is normal with mean `mu`
#' and standard deviation `Psi`.
#'
#' [jsdgam()] fits the family, taking the unit and component columns
#' and the number of factors `n_lv`. [residual_cor()] summarises the
#' residual correlations and [shared_variation()] the part the factors
#' share. `Psi` takes the prior `exponential(1)`, which suits responses
#' near unit scale. `prior(..., class = Psi)` sets another.
#'
#' With `K` components the data separate the factor part from the
#' residual part only when `(K - n_lv)^2 >= K + n_lv`. Fewer factors or
#' more components meet the bound. `jsdgam()` warns when it fails.
#'
#' @return A family object to pass to [jsdgam()] or [mvgam()].
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
#' # Five species at 80 sites whose residuals share two latent
#' # factors
#' simdat <- sim_jsdgam(family = mvn(), n_species = 5L, n_sites = 80L,
#'                      n_lv = 2L, seed = 1L)
#'
#' fit <- jsdgam(
#'   formula        = y ~ env * series,
#'   factor_formula = ~ -1,
#'   data           = simdat$data_train,
#'   unit           = time, species = series,
#'   family         = mvn(),
#'   n_lv           = 2L,
#'   chains         = 2L,
#'   silent         = 2
#' )
#' summary(fit)
#'
#' # The species-by-species residual correlations, against the values
#' # the simulation used
#' round(residual_cor(fit)$cor, 2)
#' round(simdat$truth$residual_cor, 2)
#'
#' # Next, shared_variation(fit) gives the part the factors share
#' }
#'
#' @family observation families
#' @export
mvn <- function() {
  multi_response_family("mvn", type = "real",
                        vars = c("visit_component", "Psi"))
}

#' The Stan density of `mvn()`
#'
#' The trend adds each row's factor term `Z[k, ] * lv[i, ]` to `mu`,
#' with `lv ~ N(0, I)`. Given the factors the rows are independent
#' normal with standard deviation `Psi[k]`. Integrating the factors out
#' gives each unit the covariance `Z Z' + diag(Psi^2)`.
#'
#' @return Character scalar of Stan functions
#' @noRd
mvn_stan_funs <- function() {
  paste(closure_unit_density(
    mvn(),
    unit_lines = component_scale_unit_lines(
      "lp += normal_lpdf(y_unit | mu_unit, psi_unit);"
    )
  ), collapse = "\n")
}

#' Does this family estimate a residual scale per response component?
#'
#' TRUE for `mvn()` and `mvt()`, which declare
#' `vector<lower=0>[K] Psi`, together with the loadings. A unit's
#' covariance is `Z Z' + diag(Psi^2)`. FALSE for the simplex
#' families, whose spread is carried by the softmax and, for
#' `diri()`, by a single concentration; and FALSE for the detection
#' families, which have no continuous residual at all.
#'
#' Asked wherever the split between the factor part and the
#' idiosyncratic part matters. It is composed from the two grain
#' predicates. The families are not listed again.
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
#' A rank-`m` covariance over `K` components has
#' `K m - m (m - 1) / 2` free values. One residual scale per component
#' adds another `K`. The observed covariance has
#' `K (K + 1) / 2` distinct entries. The split into a factor part
#' and an idiosyncratic part is identified only when
#'
#'   `K m - m (m - 1) / 2 + K <= K (K + 1) / 2`,
#'
#' which rearranges to `(K - m)^2 >= K + m`. This is the counting
#' condition. Meeting it does not guarantee that the posterior is well
#' behaved. Failing it does guarantee a direction that the likelihood
#' cannot see.
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
#' `mvn()` and `mvt()` estimate a residual scale per component,
#' together with the loadings. Where `identified_factor_ceiling()` says
#' the requested factor count is too high, the data cannot separate the
#' two. The posterior for `Psi` is then whatever the prior says along that
#' direction, however much data arrives.
#'
#' The check issues a warning. It does not refuse. The covariance the
#' model reports, which is what `residual_cor()` and `shared_variation()`
#' summarise, is estimated well in that regime. It can be estimated better
#' than at a factor count the bound admits. What cannot be recovered is the
#' per-component split.
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
    paste0("To estimate the residual scales, use 'n_lv = ", ceiling_lv,
           "' or fewer or add species.")
  } else {
    paste0("At ", n_species, " species every factor count leaves the ",
           "split unidentified. Add species to estimate the residual ",
           "scales.")
  }
  insight::format_warning(c(
    paste0(
      "The per-species residual scale is not identified at 'n_lv = ",
      n_lv, "' with ", n_species, " species."
    ),
    x = paste0(
      "Family '", resolve_family_name(family), "' splits each site's ",
      "covariance into a factor part and a per-species part."
    ),
    x = paste0("Identifying the per-species part needs ",
               "(n_species - n_lv)^2 >= n_species + n_lv."),
    i = paste0(
      "'residual_cor()' and the predictions use the combined ",
      "covariance and are unaffected."
    ),
    i = paste0(
      "A poor Rhat on 'Psi' reflects this design and does not signal ",
      "a sampler fault."
    ),
    i = advice
  ))
  invisible(NULL)
}


#' The residual parameters of the mvn and mvt families
#'
#' Both declare one positive scale per response component, `Psi`, on
#' the standard-deviation scale. `mvt()` adds its degrees of freedom
#' `nu`. Each takes its prior from `prior` or, without a row for its
#' class, from `common_trend_priors`. Written as sampling statements,
#' the priors reach the prior table through
#' `lift_mvgam_stanvar_priors()`.
#'
#' @param K Number of response components per closure unit. It is a
#'   literal. `N_series_trend` is declared after the family's stanvars
#'   and is out of scope in the parameters block.
#' @param name `"mvn"` or `"mvt"`, which prefixes the stanvar names
#' @param prior A `brmsprior`, or `NULL`
#' @return A `brmsstanvars` object
#' @noRd
component_scale_stanvars <- function(K, name, prior = NULL) {
  checkmate::assert_int(K, lower = 1)
  checkmate::assert_choice(name, c("mvn", "mvt"))
  checkmate::assert_class(prior, "brmsprior", null.ok = TRUE)
  combine_stanvars(
    declared_parameter_stanvars(
      "Psi", paste0("vector<lower=0>[", K, "]"), prior,
      name = paste0(name, "_Psi")
    ),
    if (identical(name, "mvt")) {
      declared_parameter_stanvars("nu", "real<lower=2>", prior,
                                  name = "mvt_nu")
    }
  )
}


#' Multivariate Student-t family for several continuous responses
#'
#' [mvn()] with heavy-tailed residuals. Given the latent factors, each
#' row follows a Student-t distribution with location `mu`, scale `Psi`
#' and degrees of freedom `nu`, which every component shares. An outlying
#' row then pulls less on the fit. The residual covariance is
#' `Z Z' + diag(Psi^2 * nu / (nu - 2))`. A unit's responses do not
#' follow a multivariate Student-t law. Their factors stay normal.
#'
#' `nu` is bounded below by 2, which keeps the residual variance finite.
#' Its prior is `gamma(2, 0.1)`, as brms gives a Student-t `nu`. Above
#' the bound it places the median of `nu` at 17 and 95% of its mass
#' between 3.2 and 56. `prior(..., class = nu)` and
#' `prior(..., class = Psi)` set other priors. The data layout, the fit
#' and the bound on `n_lv` follow [mvn()].
#'
#' @return A family object to pass to [jsdgam()] or [mvgam()].
#'
#' @references
#' Hui, F. K. C., Taskinen, S., Pledger, S., Foster, S. D. and
#'   Warton, D. I. (2015). Model-based approaches to unconstrained
#'   ordination. *Methods in Ecology and Evolution*, 6(4):399-411.
#'   \doi{10.1111/2041-210X.12236}
#'
#' @examples
#' \dontrun{
#' simdat <- sim_jsdgam(family = mvt(), n_species = 5L, n_sites = 80L,
#'                      n_lv = 2L, seed = 1L)
#' fit <- jsdgam(
#'   formula        = y ~ env * series,
#'   factor_formula = ~ -1,
#'   data           = simdat$data_train,
#'   unit           = time, species = series,
#'   family         = mvt(),
#'   n_lv           = 2L,
#'   chains         = 2L,
#'   silent         = 2
#' )
#'
#' # The degrees of freedom, against 5 in the simulation
#' posterior_summary(fit, variable = "nu")
#' }
#'
#' @family observation families
#' @export
mvt <- function() {
  multi_response_family("mvt", type = "real",
                        vars = c("visit_component", "Psi", "nu"))
}

#' The Stan density of `mvt()`
#'
#' As `mvn_stan_funs()`, with Student-t rows of scale `Psi[k]` and
#' degrees of freedom `nu`. Integrating the factors out gives each unit
#' the covariance `Z Z' + diag(Psi^2 * nu / (nu - 2))`.
#'
#' @return Character scalar of Stan functions
#' @noRd
mvt_stan_funs <- function() {
  paste(closure_unit_density(
    mvt(),
    unit_lines = component_scale_unit_lines(
      "lp += student_t_lpdf(y_unit | nu, mu_unit, psi_unit);"
    )
  ), collapse = "\n")
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
#' \eqn{O(\text{possible\_N} \cdot \text{n\_visits})} cheap operations.
#' The naive log-sum-exp needs
#' \eqn{O(K_{\max} \cdot \text{n\_visits})} `lgamma` evaluations.
#'
#' Numerical-stability strategy. The accumulation runs in log
#' space (`log_sum_exp(0, log_prob_n + log_ff + log_k_obs - log(N))`)
#' so the recurrence remains finite for any
#' \eqn{(\lambda, p, \text{possible\_N})} within Stan's representable
#' range. A linear-space form overflows in the high-detection /
#' large-window corner (e.g.,
#' \eqn{\lambda=50, p=0.95, V=5, \text{possible\_N}=50}); the log-space
#' form is unconditionally stable at the cost of one `log_sum_exp`
#' per iteration. The loop bounds guarantee
#' \eqn{N \geq K_{\min} + 1 > \max(y_v)} so the per-visit
#' denominator \eqn{N - y_v \geq 1} throughout the recurrence,
#' ruling out one other numerical hazard.
#'
#' The baseline term `poisson_log_lpmf(K_min | log_lam) +
#' binomial_logit_lpmf(counts | K_min, lp_visits)` accounts for
#' \eqn{T_{K_{\min}}} and is added once per closure unit; the
#' recurrence supplies the additional `log_prob_n` contribution.
#'
#' @return Character scalar of Stan function code.
#' @noRd
nmix_stan_funs <- function() {
  args <- closure_unit_density_args(nmix())
  paste(c(
    "  // Per-visit implementation. brms passes `mu` and `p` as",
    "  // vectors when either dpar carries a sub-formula (e.g.",
    "  // bf(y ~ s(elev), p ~ s(tod))). The overloaded scalar",
    "  // signature below broadcasts the static-dpar case via",
    "  // rep_vector and delegates to this entry point.",
    partial_sum_header("nmix", "log_mu, logit_p and log1m_p"),
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
    stan_density_header("nmix", "lpmf", args),
    "    vector[num_elements(mu)] log_mu  = log(mu);",
    "    vector[num_elements(p)]  logit_p = logit(p);",
    "    vector[num_elements(p)]  log1m_p = log1m(p);",
    closure_unit_reduce_sum_lines(
      "partial_sum_nmix_lpmf",
      c("      y, log_mu, logit_p, log1m_p, n_rep,",
        "      K_max, Y_max, visit_idx, log_n_lookup")
    ),
    "  }",
    broadcast_overloads("nmix", "lpmf", args, "p")
  ), collapse = "\n")
}

#' Stan functions of the [nmix()] Royle-Nichols density
#'
#' The latent abundance `N ~ Poisson(mu)` is summed from `Y_max[g]` up
#' to `K_max[g]`. A visit detects at least one individual with
#' probability `1 - (1 - p)^N`, where `p` is the detection probability
#' of each individual and may vary by visit. A unit with a detection
#' never reaches the `N = 0` cell, which leaves `log(0)` out of the sum.
#' A unit without one takes the closed form of the Poisson generating
#' function.
#'
#' @return Character scalar of Stan function code.
#' @noRd
nmix_royle_nichols_stan_funs <- function() {
  args <- closure_unit_density_args(nmix("royle_nichols"))
  paste(c(
    partial_sum_header("nmix_royle_nichols", "log_mu and log_1m_r"),
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
    "        // Haines (2016) closed form for an all-zero detection",
    "        // history. Summing Poisson(N | lambda) * prod_t (1 - r_t)^N",
    "        // over N gives exp(lambda * (z - 1)), z = prod_t (1 - r_t),",
    "        // by the Poisson generating function. The sum is exact,",
    "        // with no truncation at K_max. Its terms carry no",
    "        // alternating signs. Cancellation in the full Haines sum",
    "        // arises only for Y_max >= 1.",
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
    stan_density_header("nmix_royle_nichols", "lpmf", args),
    "    vector[num_elements(mu)] log_mu   = log(mu);",
    "    vector[num_elements(p)]  log_1m_r = log1m(p);",
    closure_unit_reduce_sum_lines(
      "partial_sum_nmix_royle_nichols_lpmf",
      "      y, log_mu, log_1m_r, n_rep, K_max, Y_max, visit_idx"
    ),
    "  }",
    broadcast_overloads("nmix_royle_nichols", "lpmf", args, "p")
  ), collapse = "\n")
}

#' Stan functions of the [nmix()] Poisson-Poisson density
#'
#' The latent abundance `N ~ Poisson(mu)` is summed over `0 : K_max[g]`.
#' Given `N = k`, a visit's count is `Poisson(k * p_t)`, with `p` the
#' encounter rate of each individual. A count can exceed `N`, which
#' leaves `Y_max[g]` without a role as a floor. The `k = 0` cell holds
#' mass only for a unit without counts.
#'
#' The visit sum factors as
#' `log(k) * sum(y) + sum(y * log(p)) - k * sum(p) - sum(lgamma(y + 1))`,
#' whose terms other than `k` are constant across the sum. The inner
#' loop then costs `O(K_max[g])` per unit, against
#' `O(K_max[g] * n_rep[g])` for the visit-by-visit form.
#'
#' @return Character scalar of Stan function code.
#' @noRd
nmix_poisson_poisson_stan_funs <- function() {
  args <- closure_unit_density_args(nmix("poisson_poisson"))
  paste(c(
    partial_sum_header("nmix_poisson_poisson", "log_mu and log_p"),
    "  // log_n_lookup[k] holds log(k) for the inner k loop.",
    "  // k_start_ppm[g] starts that loop where the Poisson tail below",
    "  // holds negligible mass.",
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
    "        // log(k) comes from log_n_lookup, whose length",
    "        // K_max_global bounds every Kg.",
    "        component_lps[k - kg_lo + 2] = poisson_log_lpmf(k | log_lam)",
    "          + log_n_lookup[k] * sum_counts + sum_y_log_p",
    "          - k * sum_p_v - lgamma_const;",
    "      }",
    "      lp += log_sum_exp(component_lps);",
    "    }",
    "    return lp;",
    "  }",
    "",
    stan_density_header("nmix_poisson_poisson", "lpmf", args),
    "    vector[num_elements(mu)] log_mu = log(mu);",
    "    vector[num_elements(p)]  log_p  = log(p);",
    closure_unit_reduce_sum_lines(
      "partial_sum_nmix_poisson_poisson_lpmf",
      c("      y, log_mu, log_p, p, n_rep, K_max, Y_max, visit_idx,",
        "      log_n_lookup, k_start_ppm")
    ),
    "  }",
    broadcast_overloads("nmix_poisson_poisson", "lpmf", args, "p")
  ), collapse = "\n")
}

#' The Stan data shared by the closure-unit families
#'
#' Every closure-unit family declares `N_unit`, `n_rep` and
#' `visit_idx`, with its own function block. The `include_*` arguments
#' add the data one family uses.
#'
#' @param arrays Named list from `build_closure_unit_arrays()`
#' @param family_funs_name Stanvar name of the function block, such as
#'   `"nmix_funs"`
#' @param family_funs Stan code of the function block
#' @param y_max_upper Upper bound on `Y_max`: `NA` for counts, 1 for a
#'   binary response
#' @param include_K_max Whether to declare `K_max`, the per-unit
#'   truncation of the latent abundance. Occupancy states are binary
#'   and take none.
#' @param include_Y_max Whether to declare `Y_max`, the largest count
#'   in each unit
#' @param include_component Whether to declare `visit_component`, the
#'   response component of each visit
#' @param log_n_lookup Whether to declare the transformed-data vector
#'   `log_n_lookup[k] = log(k)` for k up to `max(K_max)`. The
#'   marginalisation over the latent abundance then indexes a vector
#'   in its inner loop where it would call `log()`.
#' @return A `brmsstanvars` object
#' @noRd
make_closure_unit_arrays_stanvars <- function(arrays,
                                              family_funs_name,
                                              family_funs,
                                              y_max_upper = NA_integer_,
                                              include_K_max = TRUE,
                                              include_Y_max = TRUE,
                                              include_component = FALSE,
                                              log_n_lookup = FALSE) {
  checkmate::assert_integerish(y_max_upper, len = 1L, lower = 0L)
  checkmate::assert_flag(include_K_max)
  checkmate::assert_flag(include_Y_max)
  checkmate::assert_flag(include_component)
  checkmate::assert_flag(log_n_lookup)
  if (log_n_lookup && !include_K_max) {
    stop_mvgam_fault(
      "'log_n_lookup' is bounded by 'K_max'.",
      "'make_closure_unit_arrays_stanvars()' got 'include_K_max = FALSE'."
    )
  }
  required <- c("N_unit", "n_rep", "visit_idx", "max_rep",
                if (include_component) "visit_component",
                if (include_Y_max) "Y_max",
                if (include_K_max) "K_max")
  missing_fields <- setdiff(required, names(arrays))
  if (length(missing_fields)) {
    stop_missing_fields("The closure-unit arrays", missing_fields)
  }
  per_unit <- "array[N_unit] int<lower=1>"
  per_visit <- paste0("array[N_unit, ", arrays$max_rep, "] int<lower=1>")
  y_max_upper <- if (is.na(y_max_upper)) "" else {
    paste0(", upper=", y_max_upper)
  }
  combine_stanvars(
    brms::stanvar(name = family_funs_name, scode = family_funs,
                  block = "functions"),
    data_stanvar(as.integer(arrays$N_unit), "N_unit", "int<lower=1>"),
    data_stanvar(as.integer(arrays$n_rep), "n_rep", per_unit),
    data_stanvar(arrays$visit_idx, "visit_idx", per_visit),
    # The trend data's `obs_trend_series` carries the same fact. The
    # family stanvars are parsed before the trend data, where that
    # identifier is not yet in scope.
    if (include_component) {
      data_stanvar(arrays$visit_component, "visit_component", per_visit)
    },
    if (include_Y_max) {
      data_stanvar(as.integer(arrays$Y_max), "Y_max",
                   paste0("array[N_unit] int<lower=0", y_max_upper, ">"))
    },
    if (include_K_max) {
      data_stanvar(as.integer(arrays$K_max), "K_max", per_unit)
    },
    if (log_n_lookup) {
      brms::stanvar(
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
  )
}

#' Stan code and data of a binomial nmix() fit
#'
#' @inheritParams make_closure_unit_arrays_stanvars
#' @return A `brmsstanvars` object
#' @noRd
make_nmix_stanvars <- function(arrays) {
  make_closure_unit_arrays_stanvars(
    arrays,
    family_funs_name = "nmix_funs",
    family_funs = nmix_stan_funs(),
    log_n_lookup = TRUE
  )
}

#' Stan code and data of an nmix("royle_nichols") fit
#'
#' Royle-Nichols takes binary detections, which bounds `Y_max` by 1.
#'
#' @inheritParams make_closure_unit_arrays_stanvars
#' @return A `brmsstanvars` object
#' @noRd
make_nmix_royle_nichols_stanvars <- function(arrays) {
  make_closure_unit_arrays_stanvars(
    arrays,
    family_funs_name = "nmix_royle_nichols_funs",
    family_funs = nmix_royle_nichols_stan_funs(),
    y_max_upper = 1L
  )
}

#' Stan code and data of an nmix("poisson_poisson") fit
#'
#' Each individual contributes its own Poisson count per visit, which
#' lets a count exceed the abundance. `Y_max` then bounds nothing.
#'
#' `k_start_ppm[g]` starts the sum over the latent abundance about
#' seven standard deviations below `Y_max[g]`. Given abundance k and
#' detection p, a visit's count is Poisson(k p). With p near 1 its
#' standard deviation is about `sqrt(Y_max)`. The log likelihood m
#' standard deviations below the mode falls by m^2 / 2. At m = 7 the
#' skipped terms hold a relative mass below 1e-11. The posterior is
#' unchanged to round-off. A unit with a small count starts at 1.
#'
#' @inheritParams make_closure_unit_arrays_stanvars
#' @return A `brmsstanvars` object
#' @noRd
make_nmix_poisson_poisson_stanvars <- function(arrays) {
  y_max <- as.integer(arrays$Y_max)
  k_start_ppm <- pmax(1L, y_max - 7L * as.integer(ceiling(sqrt(y_max))))
  make_closure_unit_arrays_stanvars(
    arrays,
    family_funs_name = "nmix_poisson_poisson_funs",
    family_funs = nmix_poisson_poisson_stan_funs(),
    log_n_lookup = TRUE
  ) + data_stanvar(k_start_ppm, "k_start_ppm", "array[N_unit] int<lower=1>")
}

#' Prepare a closure-unit family for fitting
#'
#' Resolves the data-dependent parts of a closure-unit family
#' at fit time: validates the observation data, builds the
#' closure-unit arrays, assembles the family-specific Stan
#' stanvars. It sets `family$vars`, which names the data variables
#' that brms passes to the lpdf call.
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
#' @param prior A `brmsprior`, or `NULL`. The family builders emit
#'   their own sampling statements. This argument carries a user
#'   prior through to them.
#' @param series_var,time_var The series and time columns the trend
#'   names, which key a closure unit.
#' @param dpar_forms The distributional formulas of the observation
#'   model, named by parameter
#' @return The family with `mvgam_stanvars` attribute populated
#'   and `vars` set.
#' @noRd
prepare_closure_unit_family <- function(family, data, response_var,
                                         has_obs_covariates = FALSE,
                                         has_det_covariates = FALSE,
                                         prior = NULL,
                                         series_var, time_var,
                                         dpar_forms = list()) {
  family_name <- family$name
  multi_response <- is_multi_response_family(family)
  # A multi-response density takes its K components with no truncation
  # and runs its own response check below
  if (!multi_response) {
    validate_closure_unit_data(
      data, family,
      response_var       = response_var,
      has_obs_covariates = has_obs_covariates,
      has_det_covariates = has_det_covariates,
      series_var         = series_var,
      time_var           = time_var
    )
  }
  arrays <- build_closure_unit_arrays(
    data, response_var = response_var,
    series_var = series_var, time_var = time_var, family = family
  )
  # A multi-response density takes one value of each parameter other
  # than `mu` per unit, from the unit's first row
  if (multi_response) {
    refuse_off_simplex_response(data[[response_var]], arrays, family_name,
                                response_var)
    for (dpar in setdiff(names(dpar_forms), "mu")) {
      for (v in intersect(all.vars(dpar_forms[[dpar]]), names(data))) {
        refuse_unit_varying(
          data[[v]], arrays, v,
          paste0("'", dpar, "' takes one value per unit, from covariates ",
                 "constant within it.")
        )
      }
    }
  }
  family_stanvars <- switch(
    family_name,
    nmix                 = make_nmix_stanvars(arrays),
    nmix_royle_nichols   = make_nmix_royle_nichols_stanvars(arrays),
    nmix_poisson_poisson = make_nmix_poisson_poisson_stanvars(arrays),
    occ                  = make_occ_stanvars(arrays),
    diri                 = ,
    multi                = ,
    categ                = ,
    mvn                  = ,
    mvt                  = make_multi_response_stanvars(arrays, family,
                                                        prior),
    refuse_missing_family_dispatch(family_name, "its Stan code")
  )
  attr(family, "mvgam_stanvars") <- family_stanvars
  family_vars <- attr(family, "mvgam_vars", exact = TRUE)
  if (is.null(family_vars)) {
    stop_mvgam_fault(
      paste0("Closure-unit family '", family_name,
             "' lacks its 'mvgam_vars' attribute."),
      "brms passes those integer arrays to the family's likelihood."
    )
  }
  family$vars <- family_vars
  # With intercepts alone, only the extra variance of Neyman Type A
  # counts separates the abundance from the encounter rate
  if (identical(family_name, "nmix_poisson_poisson") &&
      !has_obs_covariates && !has_det_covariates) {
    warn_once(
      c(
        "nmix('poisson_poisson') with intercepts alone is weakly identified.",
        x = "The data mainly identify the product of 'mu' and 'p'.",
        i = "Add a covariate to the formula of 'mu' or of 'p'.",
        i = "Or give one intercept an informative prior."
      ),
      "nmix_poisson_poisson_intercept_only"
    )
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
#' Every response's family contributes. A custom family named by one
#' response of a multivariate formula needs its functions as much as one
#' given with the formula does. Responses that share a family also share
#' its functions and data, which the program declares once. Given
#' different arguments, they would declare one Stan name twice. That is
#' refused. `com_binomial()` sizes its table from each of its responses'
#' denominators, which brms suffixes by response.
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
        x = paste0("'", name, "()' declares its Stan data once for all ",
                   "responses."),
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

# ============================================================
# Conway-Maxwell-Binomial R-side post-fit kernels
# ============================================================
# R-side numerical helpers adapted from upstream contributor
# jbogomolovas2 (also author of the TMB COM-Binomial solver and
# the glmmTMB integration). The Stan-side lpmf is the
# in-fit work; these helpers run post-fit for `log_lik`,
# `posterior_predict`, `posterior_epred` and `residuals` over all
# (draw x observation) cells. All are vectorised. Each unique `T` value
# triggers one chunked matrix evaluation. The helpers make no
# (ndraws * nobs) scalar lpmf calls.


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
  exp(lw - log_sum_exp(lw))
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
  cmb_by_trials(theta, nu, T, numeric(length(y)), function(lw, idx, lc) {
    theta[idx] * y[idx] + nu[idx] * lc[y[idx] + 1L] - row_log_sum_exp(lw)
  })
}


#' Evaluate a COM-Binomial quantity over its support, by trial count
#'
#' Rows with one trial count `T` form one matrix of log weights
#' `nu * lchoose(T, x) + theta * x` over the support `x = 0:T`, one row
#' per observation. A group is taken in chunks, which keeps one large-T
#' group from allocating an oversized matrix.
#'
#' @param theta,nu,T Numeric vectors of the logit of `mu`, the
#'   dispersion and the trials, one element per observation
#' @param out The vector the values fill
#' @param value Function of the log weights `lw`, the observations
#'   `idx` they hold and `lchoose(T, 0:T)` returning one value per row
#' @return `out`, filled
#' @noRd
cmb_by_trials <- function(theta, nu, T, out, value) {
  for (TT in unique(T)) {
    ia <- which(T == TT)
    lc <- .cmb_lchoose(TT)
    chunk <- max(1L, as.integer(5e6 %/% (TT + 1L)))
    for (s in seq(1L, length(ia), by = chunk)) {
      idx <- ia[s:min(s + chunk - 1L, length(ia))]
      lw <- outer(nu[idx], lc) + outer(theta[idx], 0:TT)
      out[idx] <- value(lw, idx, lc)
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
  cmb_by_trials(qlogis(mu), nu, T, numeric(length(mu)), function(lw, ...) {
    as.numeric(row_softmax(lw) %*% (seq_len(ncol(lw)) - 1L))
  })
}


#' Vectorised COM-Binomial random draw
#'
#' Returns per-row integer draws from `CMB(mu, nu, T)`. Uses
#' inverse-CDF on the cumulative weighted PMF; ties broken by
#' `max.col(..., ties.method = "first")` so the inversion is
#' stable for point-mass mass functions (large positive `nu`).
#' @noRd
rcmb_vec <- function(mu, nu, T) {
  cmb_by_trials(qlogis(mu), nu, T, integer(length(mu)), function(lw, ...) {
    sample_row_index(lw) - 1L
  })
}


#' COM-Binomial CDF `P(X <= q)`, one element per observation
#'
#' A quantile below zero has no mass. The randomised quantile
#' residual of `y = 0` asks for `P(X <= -1)` as its lower bound and
#' gets 0, giving the interval `[0, P(Y = 0)]`. A missing quantile
#' gives a missing probability, as the distribution functions in
#' stats do. A missing response then leaves its residual missing.
#' @noRd
.cmb_cdf <- function(q, mu, nu, size) {
  vapply(seq_along(q), function(i) {
    if (is.na(q[i])) return(NA_real_)
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
#' already uses. A Tweedie has a single density.
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
  x <- rep_len(x, n)
  # `mgcv::ldTweedie()` fails on a missing value. A missing value
  # gives a missing density, as the density functions in stats do.
  ok <- !is.na(x)
  ld <- rep(NA_real_, n)
  if (any(ok)) {
    ld[ok] <- mgcv::ldTweedie(
      y = x[ok], mu = rep_len(mu, n)[ok],
      p = rep_len(power, n)[ok], phi = rep_len(phi, n)[ok]
    )[, 1L]
  }
  if (log) ld else exp(ld)
}


#' COM-Binomial distribution and density functions
#'
#' `family_dist_spec()` names one R distribution per family and hands
#' its callers a `p` and a `d` for it. COM-binomial has neither in
#' base R. The two vectorised kernels the package already carries are
#' given those signatures here: `.cmb_cdf()` for the distribution and
#' `cmb_lpmf_vec()` for the density. That makes `com_binomial()`
#' reachable from every consumer of the spec. Those consumers include the
#' randomised quantile residual and the `cens()` and `trunc()` terms.
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


# ---- Post-fit dispatchers for the generic surface ----

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
  mu_flat <- as.numeric(inv_link(linpred, link))
  nu_flat <- as.numeric(nu)
  T_flat <- as.numeric(data2draws(trials, dim(linpred)))
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
#' Takes the `prep` list that every other mean kernel takes. This lets
#' `family_mean_from_kernel()` call it the same way as the rest.
#' `prep$dpars$mu` arrives on the response scale already. No link is
#' applied here. A link-scale predictor taken from `mu` would be inverted
#' a second time, which gives a wrong mean with no warning.
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
# The detection families take the unit state, the visit `p` and the
# closure-unit arrays from `extract_closure_unit_components()`.
# `predict()`, `posterior_epred()`, `posterior_predict()` and
# `log_lik()` reach their kernels through
# `dispatch_closure_unit_method()`.

#' Dispatch a closure-unit R-side method by family name
#'
#' The routing table of the families on the closure-unit layout. The
#' detection families share one kernel per method. Their latent states
#' differ: an abundance under `nmix()`, an occupancy under `occ()`. A
#' new family on the layout needs one branch per `method_kind` value.
#'
#' @param family A family on the closure-unit layout
#' @param method_kind One of `"epred"`, `"predict"`, `"log_lik"`,
#'   `"latent_state"`
#' @return A function with the per-family signature for that
#'   method. Calls `stop()` for unknown families or methods. The error
#'   message names the family.
#' @noRd
dispatch_closure_unit_method <- function(family, method_kind) {
  checkmate::assert_choice(
    method_kind, c("epred", "predict", "log_lik", "latent_state")
  )
  if (!uses_closure_unit_layout(family)) {
    stop(insight::format_error(
      "dispatch_closure_unit_method() requires a closure-unit family."
    ))
  }
  family_name <- resolve_family_name(family)
  if (is_closure_unit_family(family)) {
    latent <- if (models_latent_count(family)) {
      posterior_latent_N_nmix
    } else {
      posterior_occupancy
    }
    return(switch(method_kind,
                  epred        = posterior_epred_detection,
                  predict      = posterior_predict_detection,
                  log_lik      = log_lik_detection,
                  latent_state = latent))
  }
  fn <- switch(
    family_name,
    mvn  = switch(method_kind,
                  epred        = posterior_epred_component_scale,
                  predict      = posterior_predict_mvn,
                  log_lik      = spec_log_lik("mvn"),
                  latent_state = NULL),
    mvt  = switch(method_kind,
                  epred        = posterior_epred_component_scale,
                  predict      = posterior_predict_mvt,
                  log_lik      = spec_log_lik("mvt"),
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
#' A prediction is built from three extractions: the linear predictor,
#' the distributional parameters and the latent states. When the caller
#' gives a count, each extraction subsamples on its own. The extractions
#' then disagree. Some take the first `ndraws` rows. Others draw at
#' random. The result would pair a dispersion with a mean from an
#' unrelated iteration. Resolving the count to indices once, before any
#' extraction runs, keeps every part of the result on the same iterations.
#'
#' A count always resolves to indices, even one covering the whole
#' posterior. Indices that a caller already has are returned unchanged.
#' Each extraction checks them against the draws it uses. `NULL` is
#' returned only when neither was requested. That case means every draw.
#' When both are given, the indices take precedence. A user's pair is
#' refused before it reaches this point, in `validate_draw_selectors()`.
#' The internal seams pass both on purpose. `resolve_family_pars()` takes
#' `ndraws` as the row count its result must match. It also takes the
#' indices that say which draws those rows are.
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
    posterior::ndraws(object),
    ndraws = ndraws, draw_ids = NULL
  )
}

#' Detection probability of every visit of a closure-unit fit
#'
#' `p` is one more distributional parameter: sampled as a scalar
#' without a formula of its own, a linear predictor with one, as in
#' `bf(y ~ x, p ~ s(tod))`. `resolve_family_pars()` handles both.
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
  p <- resolve_family_pars(
    object, "p", ndraws = ndraws, nobs = n_visit, draw_ids = draw_ids,
    newdata = newdata
  )$p
  if (is.null(p)) {
    stop_mvgam_fault(
      "A closure-unit fit has no detection probability.",
      "The posterior carries no 'p' and 'p' has no formula of its own."
    )
  }
  p
}

#' The closure-unit arrays for a fit, over a frame
#'
#' Every post-fit path that needs the unit layout rebuilds it from the
#' fit's own family and formula. Deriving these arguments in one place
#' means no caller can omit one. Omitting `unit_grouping_vars` would group
#' a multi-season fit on `(series, time)` and drop the site axis. The unit
#' count would then be wrong.
#'
#' The family sets the cap declarations and whether the units carry a
#' truncation.
#'
#' @param object A fitted `mvgam` with a closure-unit family.
#' @param newdata Frame to build over; defaults to the fit's data.
#' @return The list `build_closure_unit_arrays()` returns.
#' @noRd
closure_unit_arrays_for <- function(object, newdata = NULL) {
  checkmate::assert_class(object, "mvgam")
  # The frame the model was fitted on, through the accessor that
  # owns that question. Neither of the two spellings is used.
  newdata <- prediction_frame(object, newdata)
  vars <- axis_vars(object)
  build_closure_unit_arrays(
    newdata,
    response_var = response_column(object),
    series_var = vars$series_var,
    time_var = vars$time_var,
    family = object$family,
    unit_grouping_vars = fit_closure_unit_keys(object),
    # The series the model was fitted on. A frame missing one then
    # keeps every component on its own residual scale.
    series_levels = fitted_series_levels(object),
    drop_unobserved_units = FALSE
  )
}


#' Extract state, detection-probability and closure-unit arrays
#'
#' Single-pass extractor used by every closure-unit family's
#' R-side method (`log_lik_*`, `posterior_epred_*`,
#' `posterior_predict_*`, `posterior_latent_N` for nmix,
#' `posterior_occupancy` for occ, `posterior_detection`).
#' Returns the per-unit state quantity at its native grain (one column
#' per closure unit). It also returns the per-visit detection probability
#' p. The closure-unit arrays are rebuilt from `newdata`. User edits then
#' take effect at prediction time.
#'
#' The state quantity is family-specific: lambda (abundance
#' rate, log link) for nmix, psi (occupancy probability, logit
#' link) for occ, both inverted from the same linpred path via
#' the family's `linkinv`. Downstream methods access
#' `comp$state` and apply the family-specific likelihood.
#'
#' A detection sub-formula (`bf(y ~ ..., p ~ tod)`) is rebuilt on
#' `newdata` by `extract_p_for_closure_unit()`, with every term type
#' the linear-predictor composer uses.
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
  newdata <- closure_unit_frame(object, newdata)
  # Cap edits and a non-binary response in `newdata` are refused here.
  # The identifiability checks concern a fit and are switched off.
  vars <- axis_vars(object)
  validate_closure_unit_data(
    newdata, object$family,
    response_var    = response_column(object),
    series_var      = vars$series_var,
    time_var        = vars$time_var,
    identifiability = FALSE
  )
  arrays <- closure_unit_arrays_for(object, newdata)
  # Per-visit linpred for the state quantity. The linpred is
  # constant within a closure unit because the formula is on
  # site-level covariates; drop the redundant columns to one per
  # unit and carry the unit-grain state to all downstream methods.
  # Per-visit broadcasting happens only at the response sampling
  # step, where it is unavoidable.
  #
  # A caller holding its own predictor supplies it. A forecast propagates
  # the latent state forward and uses a predictor that no frame
  # reproduces. Recomputing that predictor here would give a different
  # quantity. With `process_error = FALSE`, the recomputed path uses the
  # state the sampler fitted. Every training-grid surface needs that state.
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
  # row of `newdata`. Using `visit_idx` here would shift a unit's first
  # visit by the number of missing responses before it.
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
    y       = as.integer(newdata[[response_column(object)]]),
    arrays  = arrays,
    ndraws  = ndraws,
    n_unit  = n_unit,
    n_visit = n_visit
  )
}

#' A detection family's expectation at the visit grain
#'
#' Every visit of a unit takes the unit's `psi` or `lambda`. The mean
#' of a visit is that state times the visit's `p` under `occ()`,
#' `"poisson_binomial"` and `"poisson_poisson"`. Under
#' `"royle_nichols"` a visit detects with probability
#' `1 - exp(-p * lambda)`, the mean of `1 - (1 - p)^N` over
#' `N ~ Poisson(lambda)` (Royle and Nichols 2003).
#'
#' @param object Fitted `mvgam` object with a detection family
#' @param newdata Data frame of visit rows, or `NULL`
#' @param draw_ids Integer vector of draw indices, or `NULL`
#' @param linpred Numeric linear predictor matrix, or `NULL`
#' @return Numeric matrix with one row per draw and one column per
#'   visit
#' @noRd
posterior_epred_detection <- function(object, newdata = NULL,
                                      draw_ids = NULL, linpred = NULL) {
  checkmate::assert_matrix(linpred, null.ok = TRUE)
  comp <- extract_closure_unit_components(object, newdata, draw_ids,
                                          linpred = linpred)
  state <- comp$state[, visit_to_unit_lookup(comp$arrays, comp$n_visit),
                      drop = FALSE]
  if (identical(resolve_family_name(object$family),
                "nmix_royle_nichols")) {
    return(-expm1(-comp$p * state))
  }
  state * comp$p
}

#' Take a per-unit quantity at the grain of the frame's rows
#'
#' `predict(type = "latent_state")` returns one value per closure unit.
#' That is the grain of an abundance or an occupancy. A caller that pairs
#' a column of draws with a row of `newdata` needs one column per row.
#' Every row of a unit takes that unit's single state. `marginaleffects`
#' is such a caller. Its column and row counts can then differ, as in 75
#' columns against 300 rows.
#'
#' Left alone when the draws already run at row grain. That is the case for
#' `type = "detection"` and every non-closure family. It also applies when
#' the column count matches neither grain. The caller's own dimension
#' check then reports the mismatch. This function adds no second check.
#'
#' @param object A fitted `mvgam` object.
#' @param newdata The frame the draws were made for.
#' @param draws `[ndraws x N]` matrix, or anything else unchanged.
#' @return The draws, with one column per row of `newdata`.
#' @noRd
closure_unit_draws_to_rows <- function(object, newdata, draws) {
  if (!is.matrix(draws) || is.null(newdata) ||
        !uses_closure_unit_layout(object$family) ||
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
#' that contains it. It broadcasts unit-grain quantities
#' (lambda, latent N) back to the visit grain without copying
#' the lambda matrix.
#' @noRd
visit_to_unit_lookup <- function(arrays, n_visit) {
  # `row_unit` gives this mapping for every row of the frame, observed or
  # not. Taking the mapping from it avoids a rebuild. Walking `visit_row`
  # would leave an unassigned visit. A zero fill for that visit would drop
  # the column silently, because R treats a zero index as "omit this one".
  # The two matrices would then fail to conform. The error would not name
  # the unassigned visit.
  checkmate::assert_count(n_visit)
  out <- arrays$row_unit
  if (length(out) != n_visit) {
    stop(insight::format_error(c(
      "Closure-unit row map does not cover the prediction frame.",
      x = paste0("The map covers ", length(out), " rows."),
      x = paste0("The prediction frame has ", n_visit, " visits.")
    )))
  }
  out
}

#' A detection family's per-visit predictive draws
#'
#' Each draw takes the unit's state from its distribution given the
#' covariates: `z ~ Bernoulli(psi)` under `occ()`, `N ~ Poisson(lambda)`
#' under `nmix()`. Every visit of the unit shares that state. The
#' within-unit dependence of the visits then reaches the replicates.
#' `predict(type = "latent_state")` conditions the state on the visits
#' instead. The random draws run in one order, a unit's state and then
#' its rows in frame order.
#'
#' @inheritParams posterior_epred_detection
#' @return `[S x N_visit]` integer matrix
#' @noRd
posterior_predict_detection <- function(object, newdata = NULL,
                                        draw_ids = NULL,
                                        linpred = NULL) {
  comp <- extract_closure_unit_components(object, newdata, draw_ids,
                                          linpred = linpred)
  n <- comp$ndraws
  draw_state <- if (models_latent_count(object$family)) {
    function(lambda) stats::rpois(n, lambda)
  } else {
    function(psi) stats::rbinom(n, 1L, psi)
  }
  draw_visit <- switch(
    resolve_family_name(object$family),
    occ = function(z, p) stats::rbinom(n, 1L, z * p),
    nmix_royle_nichols = function(N, r) {
      stats::rbinom(n, 1L, -expm1(N * log1p(-r)))
    },
    nmix_poisson_poisson = function(N, p) stats::rpois(n, N * p),
    function(N, p) stats::rbinom(n, size = N, prob = p)
  )
  out <- matrix(0L, nrow = n, ncol = comp$n_visit)
  # Every row of a unit takes a draw, including rows with no recorded
  # response. A forecast grid records none.
  unit_rows <- closure_unit_row_split(comp$arrays)
  for (g in seq_len(comp$arrays$N_unit)) {
    state_g <- draw_state(comp$state[, g])
    for (j in unit_rows[[g]]) {
      out[, j] <- draw_visit(state_g, comp$p[, j])
    }
  }
  out
}

#' The detection probability of each visit, per draw
#'
#' `predict(type = "detection")` returns it. Under
#' `nmix("poisson_poisson")` it is the encounter rate.
#'
#' @inheritParams posterior_epred_detection
#' @return `[S x N_visit]` matrix
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
#' occ). Per-visit residuals or PIT statistics then inherit a
#' within-unit correlation that distorts standard diagnostics.
#' Aggregating the response and the per-visit posterior predictive
#' draws to the unit grain using a sufficient summary restores
#' exchangeability across units, which is the correct grain for
#' empirical-PIT residuals and pp_check density / interval plots.
#'
#' The aggregating function is `sum`. For nmix the per-visit
#' counts add to a per-unit total, which is the marginal sufficient
#' statistic for the Poisson-binomial likelihood when the detection
#' probability is constant across visits within a unit. For occ the
#' per-visit 0/1 detections add to the detection count, which holds
#' the full within-unit detection-frequency information.
#'
#' `sum` is the sufficient statistic for the per-unit
#' Bernoulli-binomial marginal under fixed `z_g` only when
#' `p_{g, j}` is constant across visits. With visit-level detection
#' covariates the detection probabilities differ across visits. The
#' per-unit sum then becomes a Poisson-binomial summary with less
#' discriminating power than a full detection-history score (Kery and
#' Royle 2016, ch. 10.2). The PIT comparison remains valid because
#' the per-visit posterior predictive uses the same per-visit `p`.
#' The marginal distribution of the aggregated replicate matches the
#' data-generating process. `sum` is preferred over `max`. It keeps
#' detection frequency, whereas `max` collapses to presence or absence.
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
  newdata <- prediction_frame(object, newdata)
  response_var <- response_column(object)
  arrays <- closure_unit_arrays_for(object, newdata)
  if (ncol(yrep_visit) != nrow(newdata)) {
    stop_shape_fault("The per-visit predictions", ncol(yrep_visit),
                     nrow(newdata))
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
#' `multi()` scores a site's K category counts against the site's own
#' total, the sum of the response over the unit's rows. A unit with no
#' recorded counts, such as a site of a forecast grid, takes the
#' training total of `multinomial_training_total()`.
#'
#' @param object Fitted `mvgam` object with a `multi()` family.
#' @param newdata Long-format frame; the fit's own when `NULL`.
#' @param arrays The list `build_closure_unit_arrays()` returns.
#' @return Numeric vector, one trial total per row of `newdata`.
#' @noRd
multinomial_unit_totals <- function(object, newdata, arrays) {
  newdata <- prediction_frame(object, newdata)
  y <- as.numeric(newdata[[response_column(object)]])
  totals <- sum_within_closure_units(arrays, y)
  # A unit with no counts sums to zero, which would draw no individuals.
  # It takes the effort of the training design. Its forecast states what
  # the next survey of that design would record.
  unseen <- which(arrays$n_rep == 0L)
  if (length(unseen)) {
    totals[unseen] <- multinomial_training_total(object)
  }
  # Reduce to one total per unit, then take it back at row grain
  # through the row-to-unit map the arrays already carry.
  totals[arrays$row_unit]
}


#' The trial total a multinomial fit's training sites were surveyed at
#'
#' A site the model never saw supplies no counts of its own. Its
#' prediction is made at the effort the fitted design carries. Where
#' the training sites differ, the median limits the influence that one
#' unusually large or small survey has on the forecast's scale.
#'
#' @param object Fitted `mvgam` object with a `multi()` family.
#' @return A single numeric trial total.
#' @noRd
multinomial_training_total <- function(object) {
  train <- object$data
  if (is.null(train)) {
    stop(insight::format_error(c(
      "A multinomial forecast needs the fit's own training data.",
      x = "The training data are absent from the fitted object.",
      x = "A forecast site takes its trial total from the training design.",
      i = paste0("Supply that site's counts in 'newdata' or refit with ",
                 "the data stored on the object.")
    )), call. = FALSE)
  }
  arrays <- closure_unit_arrays_for(object, train)
  y <- as.numeric(train[[response_column(object)]])
  totals <- sum_within_closure_units(arrays, y)[arrays$n_rep > 0L]
  stats::median(totals)
}


#' The per-unit log-density matrix of a composition family
#'
#' `diri()`, `multi()` and `categ()` score one joint density per
#' closure unit and carry it on the unit's first row. The unit's
#' other rows hold no density of their own. That state differs from a
#' density of zero: `log p = 0` asserts certainty. `clean_ll()` treats
#' such a column as scorable because it is finite. Without the `NA` rows,
#' `loo()` would count a site once per category. The result would then
#' show far more observations than the fit has densities.
#'
#' The matrix starts from `NA_real_`, which states the fact: these rows
#' have no density. The helper that drops unscorable columns then does
#' its job. A unit missing a component keeps `NA` on every row.
#'
#' @param linpred `[ndraws x N_obs]` linear predictor, for the dimensions
#' @param arrays The list `build_closure_unit_arrays()` returns
#' @param unit_lp Function of a unit's rows `idx` and its index `g`
#'   returning the unit's log density, one value per draw
#' @return `[ndraws x N_obs]` log-density matrix
#' @noRd
unit_log_lik <- function(linpred, arrays, unit_lp) {
  out <- matrix(NA_real_, nrow = nrow(linpred), ncol = ncol(linpred))
  unit_rows <- closure_unit_row_split(arrays)
  for (g in complete_closure_units(arrays, unit_rows)) {
    idx <- unit_rows[[g]]
    out[, idx[1L]] <- unit_lp(idx, g)
  }
  out
}


#' Per-row draws of a composition family, one unit at a time
#'
#' @param comp What `extract_simplex_response_components()` returns
#' @param unit_draw Function of a unit's rows `idx` returning its
#'   `[ndraws x length(idx)]` draws
#' @param init The value the matrix starts from, `0` or `0L`
#' @return `[ndraws x N_obs]` matrix of draws
#' @noRd
unit_predictions <- function(comp, unit_draw, init = 0) {
  out <- matrix(init, nrow = comp$ndraws, ncol = comp$N_obs)
  for (idx in closure_unit_row_split(comp$arrays)) {
    out[, idx] <- unit_draw(idx)
  }
  out
}


#' Every row a closure unit holds, observed or not
#'
#' `visit_row` names a unit's observed rows only. This is the right axis
#' for a density. A visit that did not happen contributes nothing.
#' It is the wrong axis for any quantity normalised across the unit.
#' A composition's softmax spans the K components of a site. Taken over
#' the recorded components only, the softmax renormalises the site.
#' Its probabilities then sum to one, while the observed shares sum to
#' less than one. Stan's `dirichlet_lpdf` validates its simplex argument
#' and rejects that pair. The R side and the sampled model disagree
#' whenever a held-out fold or a missing observation removes a component.
#'
#' `row_unit` gives this over every row of the frame. The split is taken
#' from that column, without rebuilding it from the grouping columns.
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


#' Refuse a composition response outside its family's support
#'
#' Each unit of a [diri()] fit holds shares that sum to one. Each unit
#' of a [categ()] fit holds one `1` among its zeros. A [multi()] fit
#' holds counts. Stan rejects a unit outside that support during
#' sampling. A [categ()] unit with two `1`s would pass, scored as its
#' first. Missing responses are skipped.
#'
#' @param y The response, one value per frame row
#' @param arrays Closure-unit arrays carrying `row_unit` and `N_unit`
#' @param family_name `"diri"`, `"multi"` or `"categ"`
#' @param response_var The response's name, for the message
#' @return `NULL`, invisibly, or an error
#' @noRd
refuse_off_simplex_response <- function(y, arrays, family_name,
                                        response_var) {
  units <- lapply(closure_unit_row_split(arrays), function(rows) {
    y[rows][!is.na(y[rows])]
  })
  units <- units[lengths(units) > 0L]
  bad <- switch(
    family_name,
    diri = vapply(units, function(v) {
      any(v <= 0 | v >= 1) || abs(sum(v) - 1) > 1e-8
    }, logical(1L)),
    categ = vapply(units, function(v) {
      !all(v %in% c(0, 1)) || sum(v) != 1
    }, logical(1L)),
    multi = vapply(units, function(v) any(v < 0), logical(1L)),
    logical(length(units))
  )
  if (!any(bad)) {
    return(invisible(NULL))
  }
  support <- switch(
    family_name,
    diri = "shares in (0, 1) that sum to one",
    categ = "one 1 among zeros",
    multi = "non-negative counts"
  )
  stop(insight::format_error(c(
    paste0("'", response_var, "' must hold ", support, " in each unit."),
    x = paste0(sum(bad), " of ", length(units), " units fall outside it."),
    i = paste0("Each unit's rows are the components of one ",
               family_name, "() observation."),
    i = if (family_name == "diri") {
      "Stan checks the sum to 1e-8. Divide each share by its unit's total."
    }
  )), call. = FALSE)
}


#' Refuse a variable that takes more than one value in a closure unit
#'
#' @param values The variable, one value per frame row
#' @param arrays Closure-unit arrays carrying `row_unit` and `N_unit`
#' @param var_name The variable's name, for the message
#' @param reason Why the caller needs one value per unit
#' @return `NULL`, invisibly, or an error
#' @noRd
refuse_unit_varying <- function(values, arrays, var_name, reason) {
  checkmate::assert_atomic_vector(values, len = length(arrays$row_unit))
  varying <- vapply(closure_unit_row_split(arrays), function(rows) {
    length(unique(values[rows])) > 1L
  }, logical(1L))
  if (any(varying)) {
    stop(insight::format_error(c(
      paste0("'", var_name, "' varies within a closure unit."),
      x = paste0(sum(varying), " of ", arrays$N_unit,
                 " units hold more than one value."),
      i = reason
    )), call. = FALSE)
  }
  invisible(NULL)
}


#' The first frame row of each closure unit
#'
#' A per-unit quantity is stored on one of the unit's rows. Four callers
#' pick that row: the unit-grain state, the per-unit log-density, and two
#' `pp_check()` reductions. `visit_row[, 1]` names the first *observed*
#' row. That row is `NA` for a unit with no recorded visit. An `NA` column
#' index silently produces an `NA` column. It does not raise an error. The
#' first row of the unit exists whether or not its response does.
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
#' A composition has a density only where every component of the unit was
#' recorded. The observed shares form a simplex only then. The density
#' kernels leave the units this omits missing. `clean_ll()` then drops
#' them from `loo()` and `waic()`.
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
#' The unit's rows come from `visit_row`. The reduction then keeps the
#' unit ordering the arrays were built with. That ordering is first
#' appearance over the data.
#'
#' @param arrays The list `build_closure_unit_arrays()` returns.
#' @param x Per-visit vector, or `[ndraws x N_visit]` matrix, whose
#'   observation axis is in `newdata` row order.
#' @return `x` summed within units, keeping its shape.
#' @noRd
sum_within_closure_units <- function(arrays, x) {
  rows <- closure_unit_visit_rows(arrays)
  n_unit <- arrays$N_unit
  if (is.null(dim(x))) {
    return(vapply(rows, function(idx) sum(x[idx]), numeric(1L)))
  }
  out <- matrix(0, nrow = nrow(x), ncol = n_unit)
  for (g in seq_len(n_unit)) {
    idx <- rows[[g]]
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

#' Draws of each closure unit's latent state
#'
#' `dispatch_closure_unit_method()` picks the family's kernel:
#' `posterior_latent_N_nmix()` or `posterior_occupancy()`.
#' `predict(type = "latent_state")` and `latent_N_saturation()` take
#' their draws from here.
#'
#' @param object Fitted `mvgam` object with a detection family
#' @param newdata Long-format visit data, or `NULL` for the training
#'   data
#' @param draw_ids Posterior draw indices, or `NULL` for every draw
#' @param conditional Logical. `TRUE` draws the state given the
#'   unit's observed counts. `FALSE` draws it from its distribution
#'   given the covariates alone.
#' @param draw Logical. The occupancy kernel returns the probability
#'   of occupancy unless `draw = TRUE`, which draws the 0 / 1 state.
#'   The abundance kernels always draw.
#' @return `[S x N_unit]` matrix of latent state draws or occupancy
#'   probabilities
#' @noRd
posterior_latent_N <- function(object, newdata = NULL,
                                draw_ids = NULL,
                                conditional = TRUE, draw = FALSE) {
  checkmate::assert_flag(draw)
  # Sharing the closure-unit pipeline does not mean exposing a latent
  # state. The registry records which families do.
  require_closure_unit_predict_type(object$family, "latent_state")
  kernel <- dispatch_closure_unit_method(object$family, "latent_state")
  args <- list(object, newdata = newdata, draw_ids = draw_ids,
               conditional = conditional)
  if ("draw" %in% names(formals(kernel))) args$draw <- draw
  do.call(kernel, args)
}


#' Expected closure-unit latent state at each unit
#'
#' The mean of the latent state given the covariates: `lambda` for an
#' `nmix()` variant, `psi` for `occ()`. `get_predict.mvgam()` returns
#' it for effects plots and marginal predictions. A prediction grid
#' holds no observed visits. Its placeholder counts would otherwise
#' condition the state.
#'
#' @param object Fitted `mvgam` object with a closure-unit family
#' @param newdata Long-format data, or `NULL` for the training data
#' @param draw_ids Posterior draw indices, or `NULL`
#' @return `[S x N_unit]` numeric matrix
#' @noRd
expected_latent_state <- function(object, newdata = NULL,
                                  draw_ids = NULL) {
  require_closure_unit_predict_type(object$family, "latent_state")
  extract_closure_unit_components(object, newdata, draw_ids)$state
}


#' How often each unit's latent state reaches its cap
#'
#' The sum over a unit's latent abundance stops at its cap `K_max`. A
#' unit whose posterior abundance often equals the cap is truncated,
#' which biases its abundance down. A higher value in that unit's `cap`
#' column, then a refit, removes the bias. Under [occ()] the cap is one,
#' and a unit at its cap is one the species occupies.
#'
#' @param object A fitted `mvgam` with an [nmix()] or [occ()] family
#' @param newdata Optional `data.frame` of visits. `NULL` takes the
#'   training data.
#' @param threshold Number in `[0, 1]`. A unit whose share of draws at
#'   its cap exceeds `threshold` is marked `saturated`.
#' @param ndraws,draw_ids The number of posterior draws to use, or
#'   their indices. Give at most one. `NULL` for both uses every draw.
#'
#' @return A `data.frame` with one row per closure unit:
#'   \describe{
#'     \item{`unit`}{The unit's index.}
#'     \item{`label`}{The unit's key values, joined by `_`.}
#'     \item{`K_max`}{The unit's cap.}
#'     \item{`p_saturated`}{The posterior probability that the latent
#'       state equals `K_max`.}
#'     \item{`saturated`}{`p_saturated > threshold`.}
#'   }
#'
#' @examples
#' \dontrun{
#' set.seed(5)
#' simdat <- sim_closure_unit_data(family = nmix(), n_species = 1L,
#'                                 n_sites = 50L, n_visits = 4L,
#'                                 type = 1L)
#' # A cap of 3 is low for these counts
#' dat <- simdat$data_train
#' dat$cap <- pmax(3L, ave(dat$y, dat$series, dat$time, FUN = max))
#' mod <- mvgam(bf(y ~ env, p ~ tod_c), data = dat, family = nmix(),
#'              chains = 2, silent = 2)
#' saturation <- latent_N_saturation(mod)
#' saturation[saturation$saturated, ]
#'
#' # Next, raise 'cap' for the saturated units and refit
#' }
#'
#' @seealso [nmix()], [occ()]
#' @export
latent_N_saturation <- function(object, newdata = NULL,
                                threshold = 0.05,
                                ndraws = NULL,
                                draw_ids = NULL) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_number(threshold, lower = 0, upper = 1)
  validate_draw_selectors(ndraws, draw_ids)
  if (!is_closure_unit_family(object$family)) {
    stop(insight::format_error(c(
      paste0(
        "latent_N_saturation() requires a closure-unit family with ",
        "a latent state."
      ),
      x = paste0(
        "Family '", resolve_family_name(object$family),
        "' marginalises no latent state over repeat visits."
      )
    )), call. = FALSE)
  }
  draws <- posterior_latent_N(
    object, newdata = newdata,
    draw_ids = resolve_draw_ids(object, ndraws, draw_ids),
    draw = TRUE
  )
  arrays <- closure_unit_arrays_for(object,
                                    closure_unit_frame(object, newdata))
  p_sat <- colMeans(sweep(draws, 2L, arrays$K_max, ">="))
  out <- data.frame(
    unit        = seq_along(arrays$K_max),
    label       = arrays$unit_labels,
    K_max       = as.integer(arrays$K_max),
    p_saturated = p_sat,
    saturated   = p_sat > threshold,
    stringsAsFactors = FALSE
  )
  attr(out, "threshold") <- threshold
  out
}

#' The frame rows of each closure unit's observed visits
#'
#' @param arrays The list `build_closure_unit_arrays()` returns
#' @return A list of length `N_unit` of row indices into the frame
#' @noRd
closure_unit_visit_rows <- function(arrays) {
  lapply(seq_len(arrays$N_unit), function(g) {
    arrays$visit_row[g, seq_len(arrays$n_rep[g])]
  })
}

#' Reduce the joint density of each unit's state and visits
#'
#' For each unit, `lw[s, i]` holds the log prior of the state at
#' `k[i]` plus the log likelihood of the unit's visits given that
#' state, under draw `s`. The `occ()` state is 0 or 1 with prior
#' `Bernoulli(psi)`. An `nmix()` abundance takes `Poisson(lambda)` up
#' to the unit's cap. A visit records `Binomial(N, p)` under `occ()`
#' and `"poisson_binomial"`, `Bernoulli(1 - (1 - p)^N)` under
#' `"royle_nichols"` and `Poisson(N * p)` under `"poisson_poisson"`.
#' The grid starts at the unit's largest response, below which the
#' visits have no support. A Poisson-Poisson count can exceed `N`. Its
#' grid starts at 0.
#'
#' A row's log sum is the unit's log likelihood with the state
#' marginalised, as the Stan density computes it. The normalised row is
#' the state's posterior given the visits (Royle 2004). `reduce` takes
#' `k` and `lw` and returns one value per draw. Reducing each unit as
#' it is built keeps one `[S x K_max]` matrix in memory at a time.
#'
#' @param family A detection family
#' @param y Integer vector of visit responses, in frame row order
#' @param state `[S x N_unit]` matrix of `psi` or `lambda`
#' @param p `[S x N_visit]` matrix of detection probabilities, or
#'   encounter rates under `"poisson_poisson"`
#' @param arrays The list `build_closure_unit_arrays()` returns
#' @param reduce Function of `k` and `lw` returning a length-`S`
#'   vector of `mode`
#' @param mode Storage mode of the reduced values
#' @return `[S x N_unit]` matrix
#' @noRd
reduce_closure_unit_joint <- function(family, y, state, p, arrays,
                                      reduce, mode = "numeric") {
  name <- resolve_family_name(family)
  counts <- models_latent_count(family)
  S <- nrow(state)
  rows <- closure_unit_visit_rows(arrays)
  out <- vapply(seq_len(arrays$N_unit), function(g) {
    y_g <- rep(y[rows[[g]]], each = S)
    p_g <- p[, rows[[g]], drop = FALSE]
    lower <- if (name == "nmix_poisson_poisson") 0L else arrays$Y_max[g]
    k <- lower:arrays$K_max[g]
    lw <- vapply(k, function(n) {
      prior <- if (counts) {
        stats::dpois(n, state[, g], log = TRUE)
      } else {
        stats::dbinom(n, 1L, state[, g], log = TRUE)
      }
      visits <- switch(
        name,
        nmix_royle_nichols = stats::dbinom(
          y_g, 1L, -expm1(n * log1p(-p_g)), log = TRUE
        ),
        nmix_poisson_poisson = stats::dpois(y_g, n * p_g, log = TRUE),
        stats::dbinom(y_g, n, p_g, log = TRUE)
      )
      prior + rowSums(matrix(visits, nrow = S))
    }, numeric(S))
    reduce(k, matrix(lw, nrow = S))
  }, vector(mode, S))
  matrix(out, nrow = S)
}

#' Draws of each unit's abundance under an `nmix()` fit
#'
#' With `conditional = TRUE`, `N` is drawn from its posterior given the
#' unit's visits. With `conditional = FALSE`, it is drawn from
#' `Poisson(lambda)`, as at a new site without visits.
#'
#' @inheritParams posterior_latent_N
#' @return `[S x N_unit]` integer matrix
#' @noRd
posterior_latent_N_nmix <- function(object, newdata = NULL,
                                    draw_ids = NULL,
                                    conditional = TRUE) {
  comp <- extract_closure_unit_components(object, newdata, draw_ids)
  if (!conditional) {
    return(matrix(stats::rpois(length(comp$state), comp$state),
                  nrow = comp$ndraws))
  }
  reduce_closure_unit_joint(
    object$family, comp$y, comp$state, comp$p, comp$arrays,
    function(k, lw) k[sample_row_index(lw)], mode = "integer"
  )
}

#' Log likelihood of each closure unit under a detection family
#'
#' The unit's state is marginalised as the Stan density does it. The
#' units are the conditionally independent blocks that `loo()` scores.
#' The visits of a unit share its state.
#'
#' @param linpred `[S x N_visit]` linear predictor of the state
#' @param link The state's link
#' @param y Visit responses
#' @param family_pars List of the closure-unit `closure_arrays`, the
#'   `[S x N_visit]` matrix `p` and the `family`
#' @param trials Unused
#' @return `[S x N_unit]` matrix
#' @noRd
log_lik_detection <- function(linpred, link, y, family_pars, trials) {
  arrays <- family_pars$closure_arrays
  state <- inv_link(linpred, link)[, closure_unit_first_rows(arrays),
                                   drop = FALSE]
  reduce_closure_unit_joint(
    family_pars$family, as.integer(y), state, family_pars$p, arrays,
    function(k, lw) row_log_sum_exp(lw)
  )
}

#' The residual scale of each response component, per draw
#'
#' `mvn()` and `mvt()` declare `vector<lower=0>[K] Psi`, one scale per
#' component in the order of the fitted series. A component's residual
#' variance is `Psi^2`. Under `mvt()` it is `Psi^2 * nu / (nu - 2)`,
#' which the bound `nu > 2` keeps finite.
#'
#' @param draws_mat A draws matrix whose rows are the draws wanted
#' @param K Number of response components
#' @param family The `mvn()` or `mvt()` family
#' @return A list of the `[ndraws, K]` matrices `Psi` and `variance`,
#'   and `nu`, one value per draw or `NULL` under `mvn()`
#' @noRd
component_scale_draws <- function(draws_mat, K, family) {
  checkmate::assert_matrix(draws_mat)
  checkmate::assert_int(K, lower = 1L)
  checkmate::assert_true(family_has_component_scale(family))
  psi_cols <- paste0("Psi[", seq_len(K), "]")
  missing_cols <- setdiff(psi_cols, colnames(draws_mat))
  if (length(missing_cols) > 0L) {
    stop_missing_draws("Psi", missing_cols)
  }
  # A plain matrix. A `draws_matrix` keeps its class through subsetting
  # and arithmetic. A slot typed as a matrix refuses it.
  Psi <- matrix(as.numeric(draws_mat[, psi_cols, drop = FALSE]),
                nrow = nrow(draws_mat), ncol = K)
  nu <- NULL
  variance <- Psi^2
  if (identical(family$name, "mvt")) {
    if (!"nu" %in% colnames(draws_mat)) {
      stop_missing_draws("nu", "nu")
    }
    nu <- as.numeric(draws_mat[, "nu"])
    # `nu` holds one value per draw and recycles down each column
    variance <- variance * nu / (nu - 2)
  }
  list(Psi = Psi, variance = variance, nu = nu)
}

#' Per-row means and residual scales of an `mvn()` or `mvt()` fit
#'
#' The trend adds each row's latent factor term to `mu`. Given those
#' factors the rows are independent. The fitted series axis gives each
#' row its component, which indexes `Psi`.
#'
#' @param object Fitted `mvgam` object with an `mvn()` or `mvt()` family
#' @param newdata Long-format data, defaulting to the training data
#' @param draw_ids,ndraws Posterior draws to use
#' @param linpred A linear predictor matrix on those draws, or `NULL`
#'   to compute it
#' @return Named list with `mu`, `Psi_row` and `variance_row`, each
#'   `[ndraws x N_obs]`, `Psi` `[ndraws x K]`, `nu` (one value per draw,
#'   or `NULL`), `species_idx`, `K`, `ndraws` and `N_obs`
#' @noRd
extract_mv_response_components <- function(object, newdata = NULL,
                                            draw_ids = NULL,
                                            ndraws = NULL,
                                            linpred = NULL) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_int(ndraws, lower = 1, null.ok = TRUE)
  checkmate::assert_true(family_has_component_scale(object$family))
  newdata <- prediction_frame(object, newdata)
  # The fitted species axis gives the right entry of `Psi` for any
  # subset or spelling of the species. A species the fit never saw is
  # refused by `validate_prediction_factor_levels()`.
  series_fac <- axis_row_series(object, newdata, required = TRUE)
  species_idx <- as.integer(series_fac)
  K <- nlevels(series_fac)

  # The draws are resolved once. `mu` and `Psi` then come from the same
  # iterations.
  draw_ids <- resolve_draw_ids(object, ndraws, draw_ids)
  if (is.null(linpred)) {
    linpred <- posterior_linpred(
      object, newdata = newdata, draw_ids = draw_ids,
      process_error = FALSE
    )
  }
  mu <- object$family$linkinv(linpred)
  ndraws_actual <- nrow(mu)
  draws_mat <- fit_draws(object$fit, c("Psi", "nu"))
  rows <- if (is.null(draw_ids)) {
    seq_len(ndraws_actual)
  } else {
    draw_ids[seq_len(ndraws_actual)]
  }
  scale <- component_scale_draws(draws_mat[rows, , drop = FALSE], K,
                                 object$family)
  list(
    mu           = mu,
    Psi_row      = scale$Psi[, species_idx, drop = FALSE],
    variance_row = scale$variance[, species_idx, drop = FALSE],
    Psi          = scale$Psi,
    nu           = scale$nu,
    species_idx  = species_idx,
    K            = K,
    ndraws       = ndraws_actual,
    N_obs        = ncol(mu)
  )
}

#' Per-row expected value for an `mvn()` or `mvt()` fit
#'
#' The latent factor term has mean zero, which leaves `mu` the
#' expectation given or marginal over the factors. A Student-t residual
#' has mean zero for `nu > 1`, which the bound `nu > 2` ensures.
#'
#' @inheritParams extract_mv_response_components
#' @return `[ndraws x N_obs]` matrix of expected values
#' @noRd
posterior_epred_component_scale <- function(object, newdata = NULL,
                                            draw_ids = NULL,
                                            ndraws = NULL,
                                            linpred = NULL) {
  extract_mv_response_components(
    object, newdata, draw_ids, ndraws = ndraws, linpred = linpred
  )$mu
}

#' Per-row response draws for an `mvn()` fit
#'
#' Draws `y[s, n] ~ N(mu[s, n], Psi[s, species(n)])`. The trend adds
#' the factor term to `mu`, which leaves the rows independent.
#'
#' @inheritParams extract_mv_response_components
#' @return `[ndraws x N_obs]` numeric matrix of response draws
#' @noRd
posterior_predict_mvn <- function(object, newdata = NULL,
                                   draw_ids = NULL, ndraws = NULL,
                                   linpred = NULL) {
  comp <- extract_mv_response_components(
    object, newdata, draw_ids, ndraws = ndraws, linpred = linpred
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

#' Per-row response draws for an `mvt()` fit
#'
#' Draws `y[s, n] = mu[s, n] + Psi[s, species(n)] * t` with
#' `t ~ Student-t(nu[s])`. The trend adds the factor term to `mu`,
#' which leaves the rows independent.
#'
#' @inheritParams extract_mv_response_components
#' @return `[ndraws x N_obs]` numeric matrix of response draws
#' @noRd
posterior_predict_mvt <- function(object, newdata = NULL,
                                   draw_ids = NULL, ndraws = NULL,
                                   linpred = NULL) {
  comp <- extract_mv_response_components(
    object, newdata, draw_ids, ndraws = ndraws, linpred = linpred
  )
  # nu is per-draw; broadcast to per-cell with matrix() recycling.
  nu_row <- matrix(comp$nu, nrow = comp$ndraws, ncol = comp$N_obs)
  t_draws <- matrix(
    stats::rt(comp$ndraws * comp$N_obs, df = as.numeric(nu_row)),
    nrow = comp$ndraws, ncol = comp$N_obs
  )
  comp$mu + comp$Psi_row * t_draws
}

#' A log-likelihood kernel from the distribution of `family_dist_spec()`
#'
#' `mvn()` and `mvt()` are scored row by row. Given the latent factors in
#' the mean a row is normal or location-scale Student-t, the density
#' `family_dist_spec()` names for censoring, truncation and residuals.
#'
#' @param family_name Character family name
#' @return A function of the arguments every closure-unit `log_lik`
#'   kernel takes
#' @noRd
spec_log_lik <- function(family_name) {
  checkmate::assert_string(family_name)
  function(linpred, link, y, family_pars, trials) {
    dist_log_density(
      family_dist_spec(family_name, link, linpred, family_pars, trials),
      linpred, y
    )
  }
}

#' Extract per-row softmax probabilities + dpars for a simplex fit
#'
#' Shared helper for the post-fit kernels of `diri()`, `multi()` and
#' `categ()`. It builds the closure-unit arrays from `newdata`. It then
#' assembles per-unit mu blocks with reference subtraction
#' (`mu[idx] - mu[idx[1]]`), matching the Stan lpdf body. It applies
#' `softmax` per draw per unit. Finally, it broadcasts the result back
#' to a `[ndraws x N_obs]` per-row
#' probability matrix.
#'
#' The species-axis Z column constraint (`sum_to_zero_vector[K]`)
#' and the per-unit reference subtraction together identify the
#' simplex up to the K-shared shift that the softmax cannot resolve.
#' The Stan emission already contains both. The R-side helper mirrors
#' the reference subtraction. R-side predictions then agree with the
#' Stan-side likelihood.
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
  newdata <- closure_unit_frame(object, newdata)
  arrays <- closure_unit_arrays_for(object, newdata)

  # Resolve draw_ids up front so the mu linpred and the per-row phi
  # (when sourced from a sub-formula) use the same posterior rows.
  # Without this, an ndraws-only call would randomly subsample once
  # for mu and again for phi, mis-aligning the two by draw index.
  draw_ids <- resolve_draw_ids(object, ndraws, draw_ids)
  if (!is.null(draw_ids)) {
    ndraws <- NULL
  }

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
  # along the draw axis. Every row of the unit is included, with or
  # without a recorded response. A row without a recorded response
  # still has a linear predictor. The normaliser then spans the full
  # site. The recorded components alone do not set it.
  unit_rows <- closure_unit_row_split(arrays)
  prob_row <- matrix(0, nrow = ndraws_actual, ncol = N_obs)
  for (g in seq_len(N_unit)) {
    idx <- unit_rows[[g]]
    mu_unit <- mu[, idx, drop = FALSE]
    # Stan shifts by the first row, `mu_unit = mu[idx] - mu[idx[1]]`.
    # The softmax is invariant to a shift, which lets `row_softmax()`
    # shift by the row maximum and keep `exp()` finite.
    prob_row[, idx] <- row_softmax(mu_unit)
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

  # `multi()` scores a site's counts against that site's own total.
  # The row-grain marginal is `Binomial(N_site, prob_row)`. That total
  # is data. This function resolves it in the same pass that builds the
  # probabilities the marginal needs.
  unit_total_row <- NULL
  if (identical(resolve_family_name(object$family), "multi")) {
    unit_total_row <- multinomial_unit_totals(object, newdata, arrays)
  }

  list(
    prob_row       = prob_row,
    phi            = phi_mat,
    unit_total_row = unit_total_row,
    arrays         = arrays,
    ndraws         = ndraws_actual,
    N_obs          = N_obs,
    N_unit         = N_unit
  )
}

#' Per-row Dirichlet concentration on `newdata`
#'
#' `phi` is one more distributional parameter: a sampled scalar
#' without a formula of its own, a linear predictor with one, as
#' `resolve_family_pars()` gives either on the parameter's own scale.
#' Each closure unit's K rows then take the value at the unit's first
#' row, the value that `diri_lpdf()` uses as `phi[idx[1]]`. A covariate
#' that varies within a unit gives the rows variation that the
#' likelihood never uses.
#'
#' @param object A fitted `mvgam` object with the `diri()` family
#' @param newdata The frame the prediction covers
#' @param draw_ids Draw indices to keep, or `NULL` for all
#' @param ndraws_actual Number of draws the prediction covers
#' @param N_obs Number of rows the prediction covers
#' @param arrays The closure-unit arrays for `newdata`
#' @return `[ndraws_actual x N_obs]` matrix of concentrations
#' @noRd
extract_phi_per_row <- function(object, newdata, draw_ids,
                                ndraws_actual, N_obs, arrays) {
  phi_mat <- resolve_family_pars(
    object, "phi", ndraws = ndraws_actual, nobs = N_obs,
    draw_ids = draw_ids, newdata = newdata
  )$phi
  if (is.null(phi_mat)) {
    stop_mvgam_fault(
      "A 'diri()' fit has no concentration.",
      "The posterior carries no 'phi' and 'phi' has no formula of its own."
    )
  }

  # Taken over the unit's whole set of rows, which leaves a row whose
  # response was not recorded carrying its site's concentration.
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
  unit_predictions(comp, function(idx) {
    Kg <- length(idx)
    prob_g <- comp$prob_row[, idx, drop = FALSE]
    # alpha[s, k] = prob_g[s, k] * phi[s, idx[1]]. `comp$phi` is a
    # `[ndraws x N_obs]` matrix whose Kg-row block is constant within
    # a unit, following Stan's `phi[idx[1]]` collapse. The slice
    # `comp$phi[, idx]` gives the same per-draw alpha shape under
    # elementwise multiplication.
    alpha <- prob_g * comp$phi[, idx, drop = FALSE]
    gam <- matrix(
      stats::rgamma(comp$ndraws * Kg, shape = as.numeric(alpha),
                     rate = 1),
      nrow = comp$ndraws, ncol = Kg
    )
    gam / rowSums(gam)
  })
}

#' Per-observation log-likelihood for `diri()`
#'
#' Per-unit Dirichlet density `dirichlet_lpdf(y_unit |
#' softmax(mu_unit) * phi)`. The per-unit log-density is assigned to
#' the first row of each unit. The remaining K-1 rows are left missing.
#' The sum across rows recovers the joint log-likelihood. loo and waic
#' then score at the site grain. A unit that lost a component is left
#' missing entirely. Its observed shares sum to less than one. No
#' Dirichlet density exists for it.
#'
#' @param linpred `[ndraws x N_obs]` linpred on identity scale.
#' @param link Family link ("identity").
#' @param y Numeric vector of observed proportions, length N_obs.
#' @param family_pars List with `prob_row`, `phi`, `arrays`.
#' @param trials Unused.
#' @return `[ndraws x N_obs]` log-density matrix.
#' @noRd
log_lik_diri <- function(linpred, link, y, family_pars, trials) {
  unit_log_lik(linpred, family_pars$arrays, function(idx, g) {
    alpha <- family_pars$prob_row[, idx, drop = FALSE] *
      family_pars$phi[, idx, drop = FALSE]
    # dirichlet_lpdf(y | alpha) = lgamma(sum(alpha)) -
    #   sum(lgamma(alpha)) + sum((alpha - 1) * log(y))
    log_y <- matrix(log(y[idx]), nrow = nrow(alpha), ncol = length(idx),
                    byrow = TRUE)
    lgamma(rowSums(alpha)) - rowSums(lgamma(alpha)) +
      rowSums((alpha - 1) * log_y)
  })
}

#' Per-row expected count for a `multi()` fit
#'
#' Each row's expected count is `softmax(mu_unit)[k] * N_site` where
#' `N_site = sum(Y_unit)` is the per-site trial total. The per-site
#' total is taken from `object$data` (or `newdata`) so the
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
  # The trial total is recorded on every row of a unit. Taking it from
  # the unit's first row gives `N_site` without a second sum.
  totals <- multinomial_unit_totals(object, newdata, comp$arrays)
  unit_predictions(comp, function(idx) {
    prob_g <- comp$prob_row[, idx, drop = FALSE]
    draws <- vapply(seq_len(comp$ndraws), function(s) {
      as.integer(stats::rmultinom(1L, size = totals[idx[1L]],
                                  prob = prob_g[s, ]))
    }, integer(length(idx)))
    t(matrix(draws, nrow = length(idx)))
  }, init = 0L)
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
  unit_log_lik(linpred, family_pars$arrays, function(idx, g) {
    y_unit <- as.integer(y[idx])
    log_p <- log(family_pars$prob_row[, idx, drop = FALSE])
    # multinomial log-mass: lgamma(n + 1) - sum(lgamma(y + 1)) +
    #   sum(y * log(p))
    y_mat <- matrix(y_unit, nrow = nrow(log_p), ncol = length(idx),
                    byrow = TRUE)
    lgamma(sum(y_unit) + 1) - sum(lgamma(y_unit + 1)) +
      rowSums(y_mat * log_p)
  })
}

# Per-row probability for a `categ()` fit. Each row's expected value
# is the probability that this category is the observed one,
# `softmax(mu_unit)[k]`. `diri()` gives that same quantity from one
# definition. The categ name stays bound because `epred_kernel()`
# reaches a family through
# `exists(paste0("posterior_epred_", family))`.
posterior_epred_categ <- posterior_epred_diri

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
  unit_predictions(comp, function(idx) {
    Kg <- length(idx)
    prob_g <- comp$prob_row[, idx, drop = FALSE]
    one_hot <- matrix(0L, nrow = comp$ndraws, ncol = Kg)
    for (s in seq_len(comp$ndraws)) {
      one_hot[s, sample.int(Kg, size = 1L, prob = prob_g[s, ])] <- 1L
    }
    one_hot
  }, init = 0L)
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
  unit_log_lik(linpred, family_pars$arrays, function(idx, g) {
    Kg <- length(idx)
    y_unit <- as.integer(y[idx])
    # Observed category code: the index where y_unit == 1.
    cat_code <- which(y_unit == 1L)
    if (length(cat_code) != 1L) {
      stop(insight::format_error(c(
        "Each site of a 'categ()' response selects exactly one species.",
        x = paste0(
          "Site ", g, " has ", sum(y_unit),
          " ones across K = ", Kg, " species rows."
        )
      )))
    }
    log(family_pars$prob_row[, idx[cat_code]])
  })
}

#' Posterior occupancy of each closure unit of an `occ()` fit
#'
#' With `conditional = TRUE`, the probability that a unit is occupied
#' given its detections. A unit with a detection is occupied for
#' certain. A unit without one is occupied with probability
#' \deqn{P(z_g = 1 | y_g) =
#'   \frac{\psi_g \prod_j (1 - p_{g,j})}
#'        {\psi_g \prod_j (1 - p_{g,j}) + (1 - \psi_g)}}
#' (Royle and Dorazio 2008, ch. 3). With `conditional = FALSE`, the
#' occupancy probability `psi` given the covariates alone, as at a
#' new site without detections.
#'
#' `draw = TRUE` returns 0 / 1 draws of the state from these
#' probabilities. Species richness and other sums over units need the
#' draws to carry the uncertainty in each unit's state.
#'
#' @param object Fitted `mvgam` object with `family = occ()`
#' @param newdata Long-format data, or `NULL` for the training data
#' @param draw_ids Posterior draw indices, or `NULL`
#' @param conditional Logical. `TRUE` conditions on the detections.
#' @param draw Logical. `TRUE` returns 0 / 1 state draws.
#' @return `[S x N_unit]` matrix of probabilities, or of integer 0 / 1
#'   draws with `draw = TRUE`
#' @noRd
posterior_occupancy <- function(object, newdata = NULL,
                                draw_ids = NULL,
                                conditional = TRUE,
                                draw = FALSE) {
  checkmate::assert_flag(draw)
  comp <- extract_closure_unit_components(object, newdata, draw_ids)
  probs <- if (conditional) {
    # The grid ends at z = 1. A unit with a detection has no z = 0.
    reduce_closure_unit_joint(
      object$family, comp$y, comp$state, comp$p, comp$arrays,
      function(k, lw) exp(lw[, ncol(lw)] - row_log_sum_exp(lw))
    )
  } else {
    comp$state
  }
  if (!draw) return(probs)
  matrix(stats::rbinom(length(probs), size = 1L, prob = probs),
         nrow = nrow(probs))
}

