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
  family$family
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
#' @param response_var Name of the response (count) column.
#' @param series_var Name of the series factor (default
#'   `"series"`).
#' @param time_var Name of the time column (default `"time"`).
#' @param cap_var Name of the per-row upper-truncation column
#'   (default `"cap"`). Must be present and constant within
#'   each closure unit; the constant value becomes `K_max[g]`.
#' @return Named list with elements `N_unit`, `n_rep`, `K_max`,
#'   `Y_max`, `visit_idx`, `max_rep`, `unit_labels`.
#' @noRd
build_closure_unit_arrays <- function(data,
                                       response_var,
                                       series_var = "series",
                                       time_var   = "time",
                                       cap_var    = "cap") {
  checkmate::assert_data_frame(data, min.rows = 1L)
  checkmate::assert_string(response_var)
  checkmate::assert_string(series_var)
  checkmate::assert_string(time_var)
  checkmate::assert_string(cap_var)
  for (col in c(response_var, series_var, time_var, cap_var)) {
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
  series_vals <- as.integer(as.factor(data[[series_var]]))
  time_vals   <- as.integer(data[[time_var]])
  unit_label  <- paste(series_vals, time_vals, sep = "_")
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
  cap_vals <- as.integer(data[[cap_var]])
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
#' @examples
#' \dontrun{
#' # Constant detection probability, abundance varies with elevation
#' mvgam(y ~ s(elev), family = nmix(), data = closure_unit_data)
#'
#' # Distributional regression on detection
#' mvgam(bf(y ~ s(elev), p ~ s(tod)),
#'       family = nmix(),
#'       data = closure_unit_data)
#' }
#'
#' @export
nmix <- function() {
  fam <- brms::custom_family(
    name  = "nmix",
    dpars = c("mu", "p"),
    links = c("log", "logit"),
    # mu (lambda) is a positive rate; p is a probability. Setting
    # both bounds at the family level makes brms declare the
    # scalar-dpar case with the right constraints, which keeps
    # the lpdf's logit(p) call valid even when no sub-formula
    # is supplied for p.
    lb    = c(0, 0),
    ub    = c(NA, 1),
    type  = "int",
    loop  = FALSE
  )
  link_info <- stats::make.link(fam$link)
  fam$linkinv <- link_info$linkinv
  fam$linkfun <- link_info$linkfun
  attr(fam, "mvgam_closure_unit")  <- TRUE
  attr(fam, "mvgam_predict_types") <- c("latent_N", "detection")
  # mvgam_stanvars is populated at data preparation time, once
  # the closure-unit arrays from the user's data are known.
  attr(fam, "mvgam_stanvars") <- NULL
  fam
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

#' Build the closure-unit Stan stanvars for an nmix() fit
#'
#' Given the integer arrays returned by
#' `build_closure_unit_arrays()`, assemble the
#' `brms::stanvar()` bundle that declares the closure-unit data
#' in the Stan data block and registers the `nmix_lpmf`
#' function block.
#'
#' Each closure-unit family rolls its own `make_*_stanvars()`
#' helper using the same data arrays, sharing the integer
#' declarations and changing only the function block. Future
#' families (`occ()`, `royle_nichols()`, `poisson_poisson()`)
#' reuse this shape.
#'
#' @param arrays Named list returned by
#'   `build_closure_unit_arrays()`.
#' @return A `brmsstanvars` object.
#' @noRd
make_nmix_stanvars <- function(arrays) {
  checkmate::assert_list(arrays, names = "named")
  required <- c(
    "N_unit", "n_rep", "K_max", "Y_max",
    "visit_idx", "max_rep"
  )
  missing_fields <- setdiff(required, names(arrays))
  if (length(missing_fields) > 0L) {
    stop(insight::format_error(
      paste0(
        "Closure-unit arrays are missing fields: ",
        paste(missing_fields, collapse = ", "), "."
      )
    ))
  }
  brms::stanvar(
    name  = "nmix_funs",
    scode = nmix_stan_funs(arrays$max_rep),
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
      x     = as.integer(arrays$K_max),
      name  = "K_max",
      scode = "array[N_unit] int<lower=1> K_max;",
      block = "data"
    ) +
    brms::stanvar(
      x     = as.integer(arrays$Y_max),
      name  = "Y_max",
      scode = "array[N_unit] int<lower=0> Y_max;",
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
  validate_closure_unit_data(
    data,
    response_var       = response_var,
    has_obs_covariates = has_obs_covariates,
    has_det_covariates = has_det_covariates
  )
  arrays <- build_closure_unit_arrays(
    data, response_var = response_var
  )
  family_name <- family$name
  family_stanvars <- switch(
    family_name,
    nmix = make_nmix_stanvars(arrays),
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
  family$vars <- c("N_unit", "n_rep", "K_max", "Y_max", "visit_idx")
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
