# ==============================================================================
# MVGAM CORE: Model Fitting and Multiple Imputation Support
# ==============================================================================
# This file consolidates the core mvgam fitting architecture, dual-object
# system, and multiple imputation capabilities. The single-fit dual-object
# architecture enables brms ecosystem integration while preserving
# mvgam-specific State-Space modeling functionality.

# The mvgam function provides a single entry point that handles both
# single datasets and multiple imputation scenarios transparently, ensuring
# consistent behavior across different input types.

# Internal: translate the deprecated `samples` / `burnin` argument
# pair to the brms-style `iter` / `warmup` pair. Historical mvgam
# roxygen examples used `samples` (post-warmup draws) and `burnin`
# (warmup draws). The brms integration pipeline
# (`R/backends.R:493-494`) reads `iter` (total iterations) and
# `warmup` (warmup portion). Prior to this helper both deprecated
# names silently disappeared into `...` and every fit got
# cmdstanr's `iter = 2000, warmup = 1000` default. Called from
# `mvgam()` before dispatching to inner fitting machinery.
#
# Returns the `dots` list with `iter` and `warmup` set from the
# deprecated names (defaulting to 1000 each when only one of the
# pair was supplied) and the deprecated names removed. Attaches an
# attribute `translated = TRUE` so the caller knows to re-dispatch
# with the corrected names; `translated = FALSE` if neither
# deprecated name was present.
#
# Errors when the user supplies both the deprecated pair and the
# brms-style pair on the same call, since they configure the same
# sampler budget and the intent would be ambiguous.
#
# Related: `mvgam_use_alias()` (R/mvgam_model_helpers.R) handles
# the simpler single-argument deprecated-alias case where one name
# maps to another without arithmetic on the value. Kept separate
# because the pair-with-arithmetic mapping here (`iter =
# samples + burnin`, `warmup = burnin`) does not fit that shape.
#'@noRd
translate_samples_burnin <- function(dots) {
  has_deprecated <- any(c("samples", "burnin") %in% names(dots))
  if (!has_deprecated) {
    attr(dots, "translated") <- FALSE
    return(dots)
  }
  if (any(c("iter", "warmup") %in% names(dots))) {
    stop(insight::format_error(c(
      "Do not mix 'samples'/'burnin' with 'iter'/'warmup'.",
      x = "Both name pairs configure the same sampler budget.",
      i = paste0(
        "Prefer 'iter'/'warmup' (brms convention); ",
        "'samples'/'burnin' are deprecated."
      )
    )))
  }
  if (!identical(Sys.getenv("TESTTHAT"), "true")) {
    rlang::warn(
      paste0(
        "mvgam(): 'samples' and 'burnin' are deprecated; use ",
        "'iter' (total iterations, warmup + post-warmup) and ",
        "'warmup' instead."
      ),
      .frequency = "once",
      .frequency_id = "mvgam_samples_burnin_deprecated"
    )
  }
  samples <- dots$samples %||% 1000L
  burnin  <- dots$burnin  %||% 1000L
  dots$iter    <- samples + burnin
  dots$warmup  <- burnin
  dots$samples <- NULL
  dots$burnin  <- NULL
  attr(dots, "translated") <- TRUE
  dots
}

#' mvgam Function with Single-Fit Architecture
#'
#' @description
#' mvgam implementation using single-fit dual-object architecture
#' with native multiple imputation support and brms ecosystem integration.
#'
#' @param formula Main observation model formula (supports brms syntax).
#'   Smooth specials `s()`, `t2()`, `gp()`, varying intercepts and
#'   slopes, monotonic `mo()`, measurement-error `me()`, distributional
#'   `dpar` sub-formulas, and `offset()` are all available. `gp()`
#'   accepts both the approximate Hilbert-space form
#'   (`gp(x, k = 20)`) and the exact full-covariance form
#'   (`gp(x)`, no `k`). The two forms differ on what mvgam can do
#'   after the fit: the approximate form supports prediction at
#'   newdata; the exact form fits + scores in-sample fine but
#'   `predict()` / `posterior_epred()` at new covariate values is
#'   not wired up. Picking up an exact `gp()` term emits a
#'   one-shot warning saying as much.
#' @param trend_formula Trend formula specification (may be response-specific).
#'   Same `gp()` caveat applies on the trend side: exact GPs fit
#'   but do not support forecasts / new-time-point prediction yet.
#' @param data Data frame or list of multiply imputed datasets
#' @param newdata Optional test-set `data.frame` persisted on the
#'   fit as `object$test_data`. Used by `plot(fit, type = "series")`
#'   to overlay the test arm without re-passing the data. This is
#'   a deliberate change from earlier mvgam releases: passing
#'   `newdata` to `mvgam()` no longer emits Stan generated-quantities
#'   forecasts at fit time. Keeping forecasting out of Stan keeps
#'   the fit object small and the sampler fast, and means downstream
#'   methods can choose their own newdata. To use the persisted
#'   data downstream, re-pass it through the relevant method.
#'   `forecast()` and `posterior_predict()` answer different
#'   questions:
#'   \itemize{
#'     \item `forecast(mod, newdata = mod$test_data)` propagates the
#'       fitted latent state forward in time per posterior draw, so
#'       future predictions extrapolate the actual trajectory.
#'     \item `posterior_predict(mod, newdata = mod$test_data)`
#'       marginalises over the trend's stochastic dynamics by Monte
#'       Carlo at the supplied design points and ignores the fitted
#'       trajectory's position relative to training time.
#'   }
#'   Use `forecast()` for time-series forecasting and
#'   `posterior_predict()` for marginal predictive checks.
#' @param trend_map Optional fixed factor-loading specification.
#'   Accepts one of three shapes, a numeric `n_series x n_lv`
#'   matrix for general loadings, a `data.frame(series, trend)`
#'   for sparse series-to-trend sharing, or a character code
#'   `"identity"` / `"shared"` for the two most common cases.
#'   The numeric matrix may carry `NA` entries to mark loadings
#'   that should be sampled (partial Z); finite entries are
#'   treated as fixed and preserved exactly on `Z` in the
#'   posterior. An all-NA mask is treated as equivalent to
#'   `trend_map = NULL` (no fixed entries, factor model
#'   triggered by `n_lv` alone) so it composes with
#'   `loadings_prior`. Default factor models (no `trend_map`)
#'   sample `Z` unconstrained and identify it post-hoc via thin
#'   QR (see Heaps & Jermyn 2024); any user-supplied `trend_map`
#'   with at least one fixed entry bypasses that rotation so the
#'   encoded structure is not altered. Top-level alias for the
#'   `trend_map` argument on the trend constructor; passing both
#'   is an error. See `loadings_prior` for the compatibility
#'   matrix between `trend_map` patterns and structured
#'   loadings priors.
#' @param loadings_prior Optional named list specifying a
#'   structured prior on the unconstrained factor loadings
#'   matrix following Heaps & Jermyn (2024). When supplied, the
#'   default iid `to_vector(Z) ~ student_t(3, 0, 1)` prior is
#'   replaced with a per-column matrix-normal prior of the form
#'   `Z[, i] ~ multi_normal_cholesky(0, L_Phi * sqrt(Psi_diag[i]))`,
#'   where the among-row scale matrix `Phi` is assembled
#'   multiplicatively from any combination of:
#'   \itemize{
#'     \item a per-series feature matrix (`features`), expanded
#'       into an ARD exponential kernel via `gp_exponential_cov()`
#'     \item one or more pairwise distance matrices
#'       (`distances`), each contributing an `exp(-d / theta)`
#'       factor
#'   }
#'
#'   Accepted fields:
#'   \itemize{
#'     \item `features`: string lookup into `data2`, a numeric
#'       matrix, or a data.frame. Columns are encoded by type
#'       (numeric/integer/logical: z-score; ordered factor:
#'       z-score after `as.numeric()`; unordered factor /
#'       character: one-hot with all levels retained, following
#'       Heaps Sect. 6.2). Each encoded column receives its own
#'       ARD length-scale.
#'     \item `distances`: string name (or character vector of
#'       names) of pairwise distance matrices in `data2`, or a
#'       single matrix / named list of matrices passed inline.
#'       Each matrix is rescaled to maximum entry one so the
#'       default length-scale prior is interpretable on a unit
#'       scale (matching Heaps Supps S4.2.1).
#'     \item `column_shrinkage`: `"iid"` (default; no shrinkage)
#'       or `"mgp"` (multiplicative-gamma-process increasing
#'       shrinkage of Bhattacharya & Dunson 2011, used in
#'       Heaps Eq. (9)).
#'     \item `mgp_a1`, `mgp_a2`: numeric MGP hyperparameters,
#'       only used when `column_shrinkage = "mgp"`. Default
#'       `(2, 4)` sits within the `a2 in [3, 5]` range
#'       recommended for moderate-n ecology / community panels
#'       by Legramanti, Durante and Dunson (2020, JRSS-B) and
#'       Schiavon, Canale and Dunson (2022, Biometrics 78:995).
#'       Heaps Sect. 6.3.1 used `(2, 3)` for the gas-demand
#'       application; Sect. 6.2.2 used `(2, 6)` for the
#'       Finnish-bird JSDM. Lower `a2` for less shrinkage when
#'       the data weakly identify the active rank.
#'   }
#'
#'   Length-scales receive a `lognormal(0, 1)` prior in the
#'   model block. This prior lives on the standardised distance
#'   scale: pairwise distance matrices are rescaled to
#'   `max(d) = 1` inside the normaliser, so a length-scale of 1
#'   corresponds to correlation `exp(-1)` between the two most
#'   distant series. Heaps' Finnish-birds application (Sect.
#'   6.2.2) used the wider `lognormal(0, sqrt(10))` instead;
#'   override the default by passing a `prior()` for
#'   `theta_features` / `theta_dist_<name>` if your data weakly
#'   identify length-scales and you want less regularisation.
#'   Note: the `max(d) = 1` rule is the natural generic
#'   standardisation. Heaps Supps S4.2.1 used "root-to-tip
#'   distance = 1" for ultrametric phylogenies, which corresponds
#'   to `max(d) = 2` (twice the per-leaf depth); pre-standardise
#'   your distance matrix to match if exact parity matters.
#'   Identification caveat. The structured prior is placed on the
#'   unrotated `Z` columns, before the post-hoc QR rotation
#'   identifies `Z_tilde`. Each column of the identified
#'   `Z_tilde` is a linear mixture of the unrotated columns
#'   under the rotation `Q_tilde`. Trait or phylogeny effects
#'   inferred via `shared_variation()` or `residual_cor()` are
#'   well-defined on the rotation-invariant outer product
#'   `Z Z'`, but column-by-column interpretation of `Z_tilde`
#'   does not directly inherit the structured prior. The same
#'   caveat applies to all per-factor scalars (see the trend
#'   constructors' Identification sections).
#'
#'   Compatibility with `trend_map`:
#'   \itemize{
#'     \item `trend_map = NULL` (default): COMPATIBLE. `n_lv` alone
#'       triggers the free-Z factor model; `loadings_prior` wires
#'       onto the sampled `Z` column-wise.
#'     \item `trend_map = matrix(NA_real_, n_series, n_lv)`
#'       (all-NA mask, used internally by `jsdgam()`): COMPATIBLE.
#'       Treated as a free-Z trigger upstream; `loadings_prior`
#'       applies as if `trend_map` were `NULL`.
#'     \item `trend_map` with a mix of fixed entries and `NA`s:
#'       INCOMPATIBLE. Partial-Z parameterises the free entries
#'       as a vector under an iid `student_t(3, 0, 1)` prior,
#'       which would overlap with the matrix-normal structure
#'       and double-count regularisation.
#'     \item Fully-fixed `trend_map` (no `NA`s): INCOMPATIBLE.
#'       No free parameters left for the structured prior to act
#'       on.
#'   }
#' @param backend Stan backend (defaults to "cmdstanr")
#' @param combine Logical, pool multiple imputation results (default TRUE)
#' @param family Family specification. Supports most brms families including
#'   ordinal families (see [brms::cumulative()], [brms::sratio()],
#'   [brms::cratio()], [brms::acat()]). Multi-category families requiring
#'   separate linear predictors for each response category are not supported:
#'   [brms::categorical()], [brms::multinomial()], [brms::dirichlet()]. For
#'   these response types, use brms directly.
#' @param threads Positive integer or `NULL`. When non-NULL the
#'   model is compiled with `cpp_options$stan_threads = TRUE` and
#'   cmdstanr passes `threads_per_chain = N` at sample time.
#'   Closure-unit families (`nmix()`, `occ()`) and multi-response
#'   families (`diri()`, `mvn()`, `mvt()`, `multinomial()`,
#'   `categorical()`) thread their per-unit lpmf via `reduce_sum`;
#'   brms-native families (`gaussian()`, `poisson()`, etc.) without
#'   a `trend_formula` thread their lpmf loops via brms's
#'   `partial_log_lik_lpmf`. For closure-unit families, expect
#'   ~30-40% sampling throughput improvement at `threads = 4` on a
#'   fixture with N_unit >= 50; smaller fixtures may see
#'   thread-overhead-dominated regressions. The internal grainsize
#'   is auto-tuned to ~8 chunks regardless of `threads`.
#'
#'   Combining `threads > 1` with a `trend_formula` on a brms-native
#'   family is currently a no-op: mvgam's trend injector cannot find
#'   the `mu` assignment once brms moves it inside
#'   `partial_log_lik_lpmf`, so the code is compiled and sampled
#'   serially. This covers every currently-supported trend model
#'   under a brms-native family: `RW()`, `AR()`, `VAR()`, `VARMA()`,
#'   `ZMVN()`, `CAR()`, and `PW()`. A per-fit warning is emitted so
#'   the ignored `threads_per_chain` value is visible in any batch
#'   script that fits repeatedly. The full fix (teaching the
#'   injector to splice into `partial_log_lik_lpmf`) is filed as a
#'   separate enhancement.
#' @param run_model **(deprecated; do not use in new code)** Logical.
#'   Setting `run_model = FALSE` short-circuits before Stan parse /
#'   compile / sampling and returns a stub `mvgam` object whose
#'   `$stancode` and `$standata` slots are populated but `$fit` is
#'   `NULL`. Methods that need a fitted model (`summary()`,
#'   `predict()`, `forecast()`, `loo()`, etc.) reject the stub with
#'   a pointer back to the modern helpers. Use [`stancode()`] and
#'   [`standata()`] on an [`mvgam_formula()`] object instead. Both
#'   dispatch on `mvgam_formula` and share the exact same trend /
#'   `loadings_prior` pipeline used internally by `mvgam()` /
#'   `jsdgam()`, so they surface the same Stan code and data without
#'   any of the stub object's downstream limitations. A one-time
#'   `rlang::warn()` per session fires when this argument is `FALSE`;
#'   repeated calls within the same R session do not re-warn (the
#'   warning is rate-limited via `.frequency = "regularly"`).
#'   Defaults to `TRUE`.
#' @param ... Additional arguments passed to Stan fitting. Two are worth
#'   calling out. `algorithm` selects how the posterior is explored and
#'   accepts `"sampling"` (the default), `"meanfield"`, `"fullrank"` or
#'   `"fixed_param"` on either backend, plus `"pathfinder"` and
#'   `"laplace"` when `backend = "cmdstanr"`. `init` sets the starting
#'   values and accepts `"random"` (the default), `"0"`, a numeric
#'   value, a list or a function, and additionally `"pathfinder"` when
#'   `backend = "cmdstanr"`. The `"pathfinder"` keyword runs Stan's
#'   Pathfinder approximation on the compiled model and draws one
#'   starting value per chain from it. The posterior is still explored
#'   by the algorithm you asked for, so the fit remains exact.
#'
#'   Treat `init = "pathfinder"` as a recovery option rather than a
#'   speedup. On models that already warm up cleanly it is slower, not
#'   faster: across random walk, autoregressive, correlated
#'   autoregressive and dynamic factor trends carrying 60 to 750 latent
#'   states, it added roughly a second of wall time, left warmup
#'   duration essentially unchanged, and returned 15 to 25 percent
#'   fewer effective samples per second. Its value is on models that
#'   struggle to leave a poor starting point at all, where the extra
#'   second buys a warmup that would otherwise stall or fill with
#'   divergences.
#' @return mvgam object with dual brmsfit-like structure
#'
#' @examples
#' \donttest{
#' # ---- Single-series fit with a smooth covariate and an AR(1) trend ----
#' # Useful as a quick first model: one response, one nonlinear
#' # covariate effect, latent autoregressive dynamics.
#' set.seed(1)
#' simdat <- sim_mvgam(family = poisson())
#' mod <- mvgam(
#'   y ~ s(x),
#'   trend_formula = ~ AR(p = 1),
#'   data = simdat$data_train,
#'   family = poisson(),
#'   chains = 2,
#'   silent = 2
#' )
#' summary(mod, include_betas = FALSE)
#' conditional_effects(mod)
#' mcmc_plot(mod, variable = "^ar1", regex = TRUE, type = "hist")
#'
#' # ---- Multivariate VAR(1) with intercept suppression + custom priors ----
#' # Three correlated series, no observation intercept (`y ~ 0`),
#' # per-series smooth on the trend side, and tighter custom
#' # priors on the regression coefficients and trend innovation
#' # scale. Mirrors the workflow used in the README's Portal VAR
#' # example.
#' set.seed(2)
#' vardat <- sim_mvgam(
#'   family       = poisson(),
#'   n_series     = 3L,
#'   n_timepoints = 120L,
#'   trend_model  = VAR(cor = TRUE)
#' )
#'
#' # Inspect the default priors before fitting. Wrap the formula
#' # pair in `mvgam_formula()` so `get_prior()` can dispatch. The
#' # returned table lists every adjustable prior row keyed by
#' # `class` and `coef`; pass a modified subset back via
#' # `priors = c(prior(...), ...)` to override.
#' mf <- mvgam_formula(
#'   formula       = y ~ 0,
#'   trend_formula = ~ s(x, k = 5, by = lv_axis()) + VAR()
#' )
#' get_prior(mf, data = vardat$data_train, family = poisson())
#'
#' var_mod <- mvgam(
#'   formula       = y ~ 0,
#'   trend_formula = ~ s(x, k = 5, by = lv_axis()) + VAR(),
#'   data          = vardat$data_train,
#'   family        = poisson(),
#'   priors        = c(
#'     prior(normal(0, 2), class = b),
#'     prior(exponential(2.5), class = sigma_trend)
#'   ),
#'   chains        = 2,
#'   iter          = 1000,
#'   warmup        = 500,
#'   silent        = 2
#' )
#' summary(var_mod, include_betas = FALSE)
#'
#' # Hindcasts (training cells) and forecasts (held-out cells)
#' # share a single object; plot one series to inspect the
#' # in-sample fit and the out-of-sample predictive interval.
#' fc <- forecast(var_mod, newdata = vardat$data_test)
#' plot(fc, series = 1)
#'
#' # Per-series marginal effects of the trend-side smooth.
#' conditional_effects(var_mod)
#'
#' # Impulse response: how a shock to one series propagates
#' # through the VAR over the next eight steps.
#' plot(irf(var_mod, h = 8L), series = 1)
#'
#' # Methods-section helpers. `how_to_cite()` returns the prose
#' # paragraph for a paper; `methods_md()` returns the matching
#' # math statement of the model (likelihood, link, latent
#' # dynamics, priors, sampler configuration) as Markdown + LaTeX.
#' how_to_cite(var_mod)
#' cat(methods_md(var_mod))
#'
#' # ---- Time-varying coefficient via s(time, by = x) ----
#' # When a covariate's effect on the response drifts over the
#' # study period, an interaction between `time` and the
#' # covariate captures that drift: `s(time, by = x)` fits one
#' # smooth per unit of `x`, evaluated at each timepoint. The
#' # intercept is suppressed with `y ~ 0 + ...` so the smooth
#' # carries the whole conditional mean.
#' set.seed(3)
#' n <- 100
#' tvdat <- data.frame(
#'   time = seq_len(n),
#'   x = rnorm(n),
#'   series = factor("s1")
#' )
#' # True coefficient on x grows linearly from 0 to 2 over time.
#' tvdat$y <- tvdat$x * seq(0, 2, length.out = n) +
#'   rnorm(n, 0, 0.3)
#'
#' tv_mod <- mvgam(
#'   y ~ 0 + s(time, by = x, k = 6),
#'   data     = tvdat,
#'   family   = gaussian(),
#'   chains   = 2,
#'   iter     = 1000,
#'   warmup   = 500,
#'   silent   = 2
#' )
#' # `conditional_effects()` shows the two-way surface: the
#' # slope on x visibly steepens as time progresses.
#' conditional_effects(tv_mod)
#' }
#'
#' @references
#' Clark, N. J. and Wells, K. (2023). Dynamic Generalized
#' Additive Models for forecasting discrete ecological time
#' series. \emph{Methods in Ecology and Evolution}, 14:771-784.
#' \doi{10.1111/2041-210X.13974}
#'
#' Burkner, P.-C. (2017). brms: An R package for Bayesian
#' multilevel models using Stan. \emph{Journal of Statistical
#' Software}, 80(1):1-28. \doi{10.18637/jss.v080.i01}
#'
#' Wood, S. N. (2017). \emph{Generalized Additive Models: An
#' Introduction with R} (2nd edition). Chapman and Hall/CRC.
#'
#' Heaps, S. E. and Jermyn, I. H. (2024). Structured prior
#' distributions for the covariance matrix in latent factor
#' models. \emph{Statistics and Computing}, 34:143.
#' \doi{10.1007/s11222-024-10454-0} (post-hoc QR identification
#' of latent factor loadings; structured \code{loadings_prior}
#' construction.)
#'
#' Riutort-Mayol, G., Burkner, P.-C., Andersen, M. R., Solin, A.
#' and Vehtari, A. (2023). Practical Hilbert space approximate
#' Bayesian Gaussian processes for probabilistic programming.
#' \emph{Statistics and Computing}, 33:1.
#' \doi{10.1007/s11222-022-10167-2} (used when \code{formula}
#' or \code{trend_formula} contains \code{gp()} terms.)
#'
#' Use \code{how_to_cite(fit)} for a citation-ready methods
#' description that adapts to the model's actual structure
#' (trend type, factor identification, sampling algorithm,
#' backend). For the matching math-only model statement
#' (likelihood, link, latent dynamics, priors, sampler config)
#' rendered as a Markdown + LaTeX block, use
#' \code{\link{methods_md}(fit)}.
#'
#' @seealso \code{\link{methods_md}}, \code{\link{how_to_cite}},
#'   \code{\link{jsdgam}}. The CRAN-shipped overview is
#'   available via \code{vignette("mvgam_overview")}; for data
#'   formatting requirements see
#'   \code{vignette("data", package = "mvgam")}; for the
#'   forecast-evaluation workflow (LOO, LFO, ensembling) see
#'   the online article at
#'   \url{https://nicholasjclark.github.io/mvgam/articles/forecast_evaluation.html};
#'   for the multi-response \code{mvbf()} workflow and the
#'   integrated species distribution model pattern that ties
#'   several observation families to one shared latent process,
#'   see
#'   \url{https://nicholasjclark.github.io/mvgam/articles/mvbf.html}
#'   and
#'   \url{https://nicholasjclark.github.io/mvgam/articles/idm.html}.
#'
#' @export
mvgam <- function(formula, trend_formula = NULL, data = NULL,
                           newdata = NULL,
                           trend_map = NULL,
                           loadings_prior = NULL,
                           backend = getOption("brms.backend", "cmdstanr"),
                           combine = TRUE, family = gaussian(),
                           threads = NULL,
                           run_model = TRUE, ...) {

  # Translate the deprecated `samples` / `burnin` argument pair to
  # the brms-style `iter` / `warmup` pair the backends actually
  # consume. See `translate_samples_burnin()` for the mapping and
  # deprecation rationale.
  dots <- translate_samples_burnin(list(...))
  if (attr(dots, "translated", exact = TRUE)) {
    attr(dots, "translated") <- NULL
    return(do.call(
      mvgam,
      c(
        list(
          formula        = formula,
          trend_formula  = trend_formula,
          data           = data,
          newdata        = newdata,
          trend_map      = trend_map,
          loadings_prior = loadings_prior,
          backend        = backend,
          combine        = combine,
          family         = family,
          threads        = threads,
          run_model      = run_model
        ),
        dots
      )
    ))
  }

  # Stash silent on a global option so deep validators (the
  # exact-GP notice in particular) can honour `silent >= 2`
  # without threading the arg through every intermediate call.
  call_silent <- list(...)$silent %||% 1L
  old_silent <- options(mvgam.silent = call_silent)
  on.exit(options(old_silent), add = TRUE)

  checkmate::assert(
    checkmate::check_data_frame(data),
    checkmate::check_list(data, types = "data.frame"),
    .var.name = "data"
  )
  newdata <- validate_newdata(newdata, data)

  # Pre-fit covariate NA guard. brms' validate_data() default
  # `na_action = na_omit` silently drops rows with NAs in any
  # model-frame column. That is fine for the response (mvgam
  # preserves the trend time grid separately), but a missing
  # covariate row breaks dimension alignment downstream in Stan
  # and only shows up as an opaque chain-failure error. Catch it
  # here naming the offending column(s).
  resp_vars <- extract_response_vars(formula)
  validate_no_covariate_nas(
    data           = data,
    formulas       = list(formula, trend_formula),
    response_vars  = resp_vars,
    context        = "data"
  )
  if (!is.null(newdata)) {
    validate_no_covariate_nas(
      data           = newdata,
      formulas       = list(formula, trend_formula),
      response_vars  = resp_vars,
      context        = "newdata"
    )
  }
  checkmate::assert_character(backend, len = 1)
  checkmate::assert_logical(combine, len = 1)
  checkmate::assert_flag(run_model)
  # `threads` is forwarded as-is. The inner pipeline (mvgam_single,
  # stancode.mvgam_formula) does its own validation; eager
  # validate_threads() here would replace NULL with a brmsthreads
  # sentinel that downstream `assert_int(threads, lower = 1)` checks
  # reject. The named arg buys autocomplete + a stable signature
  # without changing the value semantics.
  if (!is.null(threads)) {
    checkmate::assert(
      checkmate::check_number(threads, lower = 1),
      checkmate::check_class(threads, "brmsthreads"),
      .var.name = "threads"
    )
  }
  if (isFALSE(run_model)) {
    rlang::warn(
      paste0(
        "`run_model = FALSE` is deprecated. Use `stancode()` and ",
        "`standata()` on an `mvgam_formula()` object to retrieve the ",
        "generated Stan code and data without fitting; both dispatch ",
        "on `mvgam_formula` and share the same trend / loadings_prior ",
        "pipeline used internally by `mvgam()` and `jsdgam()`."
      ),
      .frequency = "regularly",
      .frequency_id = "mvgam_run_model_false_deprecated"
    )
  }

  # Capture data name from user's call (before passing to internal
  # functions). `deparse()` of a literal data frame expression (e.g.
  # `mvgam(data = as.data.frame(long_dat))` or a `do.call(mvgam, ...)`
  # call that resolved `data` to an inline structure) returns the
  # full expansion, which then bleeds into summary() output. Collapse
  # any multi-line deparse and fall back to a short placeholder when
  # the captured name is longer than a typical symbol identifier.
  data_name <- paste(deparse(match.call()$data), collapse = " ")
  if (nchar(data_name) > 80L) {
    data_name <- "<inline data>"
  }

  # Handle multiple imputation input
  if (is.list(data) && !is.data.frame(data)) {
    if (combine) {
      return(mvgam_multiple(formula, trend_formula, data, backend,
                           combine = TRUE, data_name = data_name,
                           newdata = newdata, ...))
    } else {
      return(mvgam_multiple(formula, trend_formula, data, backend,
                           combine = FALSE, data_name = data_name,
                           newdata = newdata, ...))
    }
  }

  # Single dataset processing. `threads` is forwarded only when the
  # user actually set it; sending `threads = NULL` explicitly down
  # the pipeline trips the `assert_int(threads, lower = 1)` check
  # in stancode.mvgam_formula (which has its own positive-integer
  # default of `getOption("mc.cores", 1)`).
  mvgam_single_args <- list(
    formula = formula,
    trend_formula = trend_formula,
    data = data,
    newdata = newdata,
    trend_map = trend_map,
    loadings_prior = loadings_prior,
    backend = backend,
    family = family,
    data_name = data_name,
    run_model = run_model
  )
  if (!is.null(threads)) {
    mvgam_single_args$threads <- threads
  }
  mvgam_object <- do.call(mvgam_single, c(mvgam_single_args, list(...)))

  # Post-fit advisor: a by_lv factor model at the full-rank boundary
  # with the default iid Z prior is rotationally unidentified. When
  # Rhat on init_trend / lv_trend / sigma_trend / Sigma_trend /
  # L_Sigma_trend exceeds 1.1, point users at the MGP prior or a
  # pinned trend_map. The helper short-circuits for non-factor fits,
  # MGP-prior fits, n_lv != n_series fits, and clean Rhats.
  if (isTRUE(run_model) && inherits(mvgam_object, "mvgam") &&
      !is.null(mvgam_object$fit)) {
    warn_by_lv_full_rank_funnel(mvgam_object, silent = call_silent)
  }

  return(mvgam_object)
}


#' Validate `newdata` against the training data of an \pkg{mvgam} model
#'
#' Runs the structural checks that [predict.mvgam()] and
#' [forecast.mvgam()] apply to a `newdata` frame, so a misaligned frame is
#' caught up front rather than at the back of an expensive prediction
#' pipeline. It confirms that any `time` and `series` columns present in the
#' training `data` also appear in `newdata`, rejects `series` levels that
#' were not in the training data, and coerces `newdata$series` to the
#' training factor levels. Predictor and response columns are not enforced
#' here; the prediction functions resolve those from the model formula.
#'
#' @param newdata A `data.frame` of prediction covariates in the same shape
#'   as the training data (same factor levels and covariate columns).
#' @param data The `data.frame` (or `list`) used to fit the model, whose
#'   `series` levels define the valid set.
#' @return `newdata` with `series` coerced to the training factor levels,
#'   returned invisibly, or `NULL` (invisibly) when `newdata` is `NULL`.
#' @seealso [predict.mvgam()], [forecast.mvgam()]
#' @examples
#' train <- data.frame(
#'   series = factor(rep(c("a", "b"), each = 3L)),
#'   time = rep(1:3, times = 2L),
#'   y = rnorm(6L)
#' )
#' future <- data.frame(
#'   series = factor(rep(c("a", "b"), each = 2L), levels = c("a", "b")),
#'   time = rep(4:5, times = 2L)
#' )
#' validate_newdata(future, train)
#' @export
validate_newdata <- function(newdata, data) {
  if (is.null(newdata)) return(invisible(NULL))
  required <- intersect(c("time", "series"), names(data))
  validate_required_variables(newdata, required, "newdata")
  train_levels <- levels(data$series)
  if (is.null(train_levels)) return(invisible(newdata))
  new_chr <- as.character(newdata$series)
  if (!all(new_chr %in% train_levels)) {
    bad <- unique(new_chr[!new_chr %in% train_levels])
    stop(insight::format_error(c(
      "'newdata' contains series not present in the training data.",
      x = paste0(
        "Unknown levels: ",
        paste0("'", bad, "'", collapse = ", "), "."
      ),
      i = paste0(
        "newdata$series must be a subset of levels(data$series); ",
        "additional series at fit time are not supported."
      )
    )))
  }
  newdata$series <- factor(new_chr, levels = train_levels)
  invisible(newdata)
}

# ------------------------------------------------------------------------------
# SINGLE DATASET PROCESSING
# ------------------------------------------------------------------------------
# Core processing pipeline for single datasets using the two-stage Stan
# assembly system with brms ecosystem integration.

#' Process Single Dataset
#' @param formula Main formula
#' @param trend_formula Trend formula
#' @param data Single data frame
#' @param backend Stan backend
#' @param family Family specification
#' @param ... Additional arguments
#' @return mvgam object
#' @noRd
mvgam_single <- function(formula, trend_formula, data, backend,
                        family, data_name = NULL, newdata = NULL,
                        trend_map = NULL, loadings_prior = NULL,
                        run_model = TRUE, ...) {

  # Create mvgam_formula object for shared processing
  mvgam_formula_obj <- mvgam_formula(formula, trend_formula)

  # Reason: brms uses 'prior' (singular), historic mvgam / jsdgam
  # docs use 'priors'. Without aliasing, the plural form falls
  # into `...` and is silently dropped by every consumer downstream
  # that takes `prior = NULL`.
  forward_dots <- normalise_prior_arg_alias(list(...))

  # Check the sampler dimensions before any code generation, since an
  # impossible iteration count cannot be salvaged later and the user
  # should not wait through a compile to hear about it.
  validate_sampler_iterations(
    iter = forward_dots$iter %||% 2000,
    warmup = forward_dots$warmup
  )

  # Use existing shared infrastructure (same as stancode())
  stan_components <- do.call(
    generate_stan_components_mvgam_formula,
    c(
      list(
        formula = mvgam_formula_obj,
        data = data,
        family = family,
        backend = backend,
        trend_map = trend_map,
        loadings_prior = loadings_prior
      ),
      forward_dots
    )
  )

  # Deprecated run_model = FALSE: short-circuit before parse / compile
  # / fit so callers can inspect the generated stancode + standata
  # without paying for Stan codegen + sampling. The deprecation
  # warning is emitted in mvgam() (so it fires at the user-facing API
  # surface, not the internal single / multi dispatcher).
  if (isFALSE(run_model)) {
    return(create_mvgam_stub_from_stan_components(
      stan_components = stan_components,
      formula = formula,
      trend_formula = trend_formula,
      family = family,
      data = data,
      data_name = data_name,
      newdata = newdata,
      backend = backend
    ))
  }

  # Fit the combined model using backend functions directly.
  # Reuse the alias-normalised list captured above so the singular /
  # plural prior alias survives the fitting branch too.
  dots <- forward_dots

  # Extract fitting parameters with defaults
  algorithm <- dots$algorithm %||% "sampling"
  iter <- dots$iter %||% 2000
  warmup <- dots$warmup %||% (iter %/% 2)
  thin <- dots$thin %||% 1
  chains <- dots$chains %||% 4
  cores <- dots$cores %||% 1
  threads <- dots$threads %||% NULL
  opencl <- dots$opencl %||% NULL
  init <- dots$init %||% "random"
  exclude <- dots$exclude %||% NULL
  seed <- dots$seed %||% sample.int(.Machine$integer.max, 1)
  control <- dots$control %||% NULL
  silent <- dots$silent %||% 1
  future <- dots$future %||% FALSE
  # cmdstanr compile-time passthroughs. cpp_options accepts
  # entries like `stan_threads = TRUE` (auto-set when `threads`
  # is non-NULL), `CXXFLAGS = "-march=native"` for native-CPU
  # tuning, or any other cmdstan-supported C++ flag.
  # stanc_options accepts the stanc3 optimisation level
  # (e.g. `"O1"`) and other stanc-level flags. Both forward
  # directly to `cmdstanr::cmdstan_model()`.
  cpp_options   <- dots$cpp_options   %||% NULL
  stanc_options <- dots$stanc_options %||% NULL
  
  # Validate and normalize parameters
  silent <- validate_silent(silent)
  threads <- validate_threads(threads)
  opencl <- validate_opencl(opencl)
  algorithm <- validate_algorithm(algorithm, backend)
  init <- validate_init(init, backend)
  
  # Parse/validate Stan code
  validated_code <- parse_model(
    model = stan_components$combined_components$stancode,
    backend = backend,
    silent = silent
  )
  
  # Compile Stan model
  if (silent < 2) {
    message("Compiling Stan model...")
  }
  compile_args <- list(
    model         = validated_code,
    backend       = backend,
    threads       = threads,
    opencl        = opencl,
    silent        = silent
  )
  if (!is.null(cpp_options))   compile_args$cpp_options   <- cpp_options
  if (!is.null(stanc_options)) compile_args$stanc_options <- stanc_options
  compiled_model <- do.call(compile_model, compile_args)
  
  # Fit Stan model
  if (silent < 2) {
    message("Fitting Stan model...")
  }
  combined_fit <- fit_model(
    model = compiled_model,
    backend = backend,
    sdata = stan_components$combined_components$standata,
    algorithm = algorithm,
    iter = iter,
    warmup = warmup,
    thin = thin,
    chains = chains,
    cores = cores,
    threads = threads,
    opencl = opencl,
    init = init,
    exclude = exclude,
    seed = seed,
    control = control,
    silent = silent,
    future = future
  )
  
  # Store backend information for later use. `init` is kept as the user
  # wrote it because Stan records only the resolved value, which for
  # list-valued and Pathfinder starts is a temporary file path that
  # cannot be replayed on another machine.
  attr(combined_fit, "backend") <- backend
  attr(combined_fit, "init") <- init
  attr(combined_fit, "algorithm") <- algorithm
  attr(combined_fit, "mvgam_version") <- utils::packageVersion("mvgam")
  attr(combined_fit, "fit_time") <- Sys.time()

  # Enrich trend_metadata with kernel-relevant extras (ar_lags,
  # ma_lags, max_lag, has_cor, n_lv, trend_type). Persisting these
  # at fit time means the forecasting surface doesn't have to
  # re-derive them on every per-draw call.
  enriched_trend_metadata <- enrich_trend_metadata(
    stan_components$trend_metadata,
    stan_components$mv_spec$trend_specs
  )

  mvgam_object <- create_mvgam_from_combined_fit(
    combined_fit = combined_fit,
    obs_setup = stan_components$obs_setup,
    trend_setup = stan_components$trend_setup,
    mv_spec = stan_components$mv_spec,
    trend_metadata = enriched_trend_metadata,
    data_name = data_name,
    combined_stancode = stan_components$combined_components$stancode,
    combined_standata = stan_components$combined_components$standata,
    user_trend_formula = trend_formula,
    # The user's full `prior` / `priors` arg (already aliased to
    # `prior` by `normalise_prior_arg_alias` at the top of `mvgam()`).
    # Plumbed through so `create_mvgam_from_combined_fit()` can mark
    # mvgam-managed trend overrides (sigma_trend, ar1_trend, etc.) as
    # `source = "user"` on the stored prior table; the brms-side path
    # only marks rows brms itself knows about.
    user_prior = dots$prior,
    newdata = newdata
  )

  return(mvgam_object)
}

# ------------------------------------------------------------------------------
# TREND STANVAR EXTRACTION
# ------------------------------------------------------------------------------
# Extracts and generates trend-specific stanvars to enable proper injection
# into the combined Stan model while maintaining compatibility with brms.

# Note: Legacy trend stanvar generation functions have been removed.
# The modern system in stan_assembly.R using generate_trend_injection_stanvars()
# provides the same functionality with better integration.

# ------------------------------------------------------------------------------
# COMBINED STAN CODE GENERATION
# ------------------------------------------------------------------------------
# Orchestrates the combination of observation and trend models into a single
# Stan program while maintaining separate parameterizations for ecosystem
# compatibility.

#' Generate Combined Stan Code and Data Using Modern System
#' @param obs_setup Observation model setup
#' @param trend_setup Trend model setup
#' @param mv_spec Multivariate specification
#' @return List with combined stancode and standata
#' @noRd
generate_combined_stancode_and_data <- function(obs_setup, trend_setup, mv_spec, validate = TRUE, prior = NULL,
                                                backend = "rstan") {

  # Extract trend_specs from mv_spec for the Stan code generator.
  trend_specs <- if (mv_spec$has_trends && !is.null(mv_spec$trend_specs)) {
    # Pass the entire trend_specs (handles both univariate and multivariate)
    mv_spec$trend_specs
  } else {
    NULL
  }

  # Use the two-stage assembly system. `backend` is threaded so the
  # syntax-validation step picks the same Stan parser the user will
  # compile with; otherwise simplex families (which need Stan >= 2.36
  # via cmdstanr) fail validation under the default rstan bundled
  # parser.
  result <- generate_combined_stancode(
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    trend_specs = trend_specs,
    validate = validate,
    prior = prior,
    silent = 1,
    backend = backend
  )

  return(result)
}

# ------------------------------------------------------------------------------
# MODEL FITTING
# ------------------------------------------------------------------------------
# Orchestrates the actual Stan model fitting using the appropriate backend
# while maintaining compatibility with both rstan and cmdstanr.


# ==============================================================================
# MVGAM OBJECT CREATION FROM COMBINED FIT
# ==============================================================================
# Creates mvgam object from combined Stan fit with full parameter map.

#' Create mvgam Object from Combined Stan Fit
#'
#' @param combined_fit Stan fit object from combined model
#' @param obs_setup Observation model setup components (must include brmsfit)
#' @param trend_setup Trend model setup components (must include brmsfit)
#' @param mv_spec Multivariate trend specification
#' @param trend_metadata Trend metadata for prediction
#' @param data_name Optional name for the dataset
#' @return mvgam object with stored brmsfit objects for prediction workflows
#' @noRd
create_mvgam_from_combined_fit <- function(combined_fit, obs_setup,
                                          trend_setup = NULL,
                                          mv_spec = NULL,
                                          trend_metadata = NULL,
                                          data_name = NULL,
                                          combined_stancode = NULL,
                                          combined_standata = NULL,
                                          user_trend_formula = NULL,
                                          user_prior = NULL,
                                          newdata = NULL) {
  checkmate::assert_class(combined_fit, "stanfit")
  checkmate::assert_list(obs_setup, names = "named")
  checkmate::assert_list(trend_setup, names = "named", null.ok = TRUE)
  checkmate::assert_list(mv_spec, names = "named", null.ok = TRUE)
  checkmate::assert_list(trend_metadata, names = "named", null.ok = TRUE)

  # Validate brmsfit field existence for prediction system
  if (!"brmsfit" %in% names(obs_setup)) {
    stop(insight::format_error(c(
      cli::format_inline(
        "{.field obs_setup} missing required {.field brmsfit} component."
      ),
      x = "The observation model setup must include a brmsfit object for predictions.",
      i = "Check setup_brms_lightweight() implementation."
    )))
  }

  if (!is.null(trend_setup) && !"brmsfit" %in% names(trend_setup)) {
    stop(insight::format_error(c(
      cli::format_inline(
        "{.field trend_setup} missing required {.field brmsfit} component."
      ),
      x = "The trend model setup must include a brmsfit object for predictions.",
      i = "Check setup_brms_lightweight() implementation."
    )))
  }

  mvgam_components <- extract_mvgam_components(combined_fit, obs_setup,
                                              trend_setup, mv_spec)

  backend <- attr(combined_fit, "backend") %||% "rstan"

  mvgam_object <- structure(
    list(
      fit = combined_fit,
      formula = obs_setup$formula,
      trend_formula = if (!is.null(trend_setup)) trend_setup$formula else NULL,
      # Original user-supplied trend_formula (with trend constructors
      # like AR(p = 1) intact); preserved verbatim so update.mvgam
      # can round-trip the fit without trying to reconstruct the
      # constructor call from parsed `mv_spec$trend_specs`.
      trend_call = user_trend_formula,
      family = obs_setup$family,
      # Combine obs + trend prior tables before lifting stanvar rows so
      # the fit stores every user-supplied row (not just the obs subset).
      # Passing `obs_setup$prior` alone here silently dropped any
      # `prior(..., class = sigma_trend)` / `class = ar1_trend` /
      # `class = b_trend` overrides the user supplied via `priors = `
      # (Stan still applied them, but they were absent from `$prior`).
      # The trend setup carries brms's own class names (e.g. `sigma`,
      # `b`, `sds`, `ar1`); re-suffix them so the combined table uses
      # the same `<class>_trend` convention as
      # `get_prior.mvgam_formula()` and the rest of the package.
      # Then layer any mvgam-managed user trend priors back on (those
      # are stripped from the brms pipeline by
      # `remove_trend_suffix_from_priors()`, but Stan still applies
      # them via the mvgam stanvar substitution path).
      prior = assemble_stored_prior_table(
        obs_priors      = obs_setup$prior,
        trend_priors    = if (!is.null(trend_setup)) trend_setup$prior else NULL,
        user_prior      = user_prior,
        combined_stancode = combined_stancode %||% obs_setup$stancode
      ),
      data = obs_setup$data,
      test_data = newdata,
      data.name = data_name,
      stancode = combined_stancode %||% obs_setup$stancode,
      standata = combined_standata %||% obs_setup$standata,
      exclude = c("lprior", "lp__"),
      mv_spec = mv_spec,
      response_names = mv_spec$response_names %||% NULL,
      trend_components = mvgam_components$trend_components,
      series_info = mvgam_components$series_info,
      time_info = mvgam_components$time_info,
      trend_metadata = trend_metadata,
      # Store lightweight brmsfit objects for prediction workflows
      obs_model = obs_setup$brmsfit,
      trend_model = if (!is.null(trend_setup)) trend_setup$brmsfit else NULL,
      backend = backend,
      init = attr(combined_fit, "init") %||% "random",
      algorithm = combined_fit@sim$algorithm %||% "sampling",
      brms_version = utils::packageVersion("brms"),
      mvgam_version = utils::packageVersion("mvgam"),
      creation_time = Sys.time()
    ),
    class = c("mvgam", "brmsfit")
  )

  mvgam_object$criteria <- list()
  mvgam_object$call <- match.call(sys.function(sys.parent()),
                                 sys.call(sys.parent()))

  # Defensive sign-canonical pass on saved Z / lv_trend draws.
  # Free-Z factor models save `Z_tilde` and `lv_trend_tilde`
  # with positive diagonal via Stan's `qr_thin_R` so the
  # sign-mode equivalence is already removed at sampling time;
  # this call short-circuits to a no-op whenever `Z_tilde` is
  # in the posterior. Active only for fits that lack the
  # post-hoc QR (none in current architecture, but the function
  # is kept as a defensive belt).
  mvgam_object <- sign_canonicalise_factors(mvgam_object)

  return(mvgam_object)
}

# Build a no-fit mvgam stub from generated stan_components. Used by
# the deprecated `run_model = FALSE` path: callers get an mvgam-shaped
# list with `stancode`, `standata`, `obs_data`, `trend_metadata` and
# friends populated, but `fit` is left NULL because no sampling
# happened. The stub carries `c("mvgam", "mvgam_prefit")` so the
# existing `print.mvgam_prefit()` and `stancode.mvgam_prefit()`
# methods dispatch on it, reusing the unfitted-object convention
# already exposed elsewhere in the package. Downstream surfaces that
# need a real fit (`summary`, `predict`, `loo`, etc.) refuse the stub
# with a pointer back to `stancode()` / `standata()` on an
# `mvgam_formula()`.
create_mvgam_stub_from_stan_components <- function(stan_components,
                                                   formula,
                                                   trend_formula,
                                                   family,
                                                   data,
                                                   data_name,
                                                   newdata,
                                                   backend) {
  obs_setup <- stan_components$obs_setup
  trend_setup <- stan_components$trend_setup
  mv_spec <- stan_components$mv_spec
  enriched_trend_metadata <- enrich_trend_metadata(
    stan_components$trend_metadata,
    mv_spec$trend_specs
  )
  mvgam_object <- structure(
    list(
      fit = NULL,
      formula = obs_setup$formula,
      trend_formula = if (!is.null(trend_setup)) trend_setup$formula else NULL,
      trend_call = trend_formula,
      family = obs_setup$family %||% family,
      prior = lift_mvgam_stanvar_priors(
        obs_setup$prior, stan_components$combined_components$stancode
      ),
      data = obs_setup$data %||% data,
      test_data = newdata,
      data.name = data_name,
      stancode = stan_components$combined_components$stancode,
      standata = stan_components$combined_components$standata,
      exclude = c("lprior", "lp__"),
      mv_spec = mv_spec,
      response_names = mv_spec$response_names %||% NULL,
      trend_metadata = enriched_trend_metadata,
      obs_model = obs_setup$brmsfit,
      trend_model = if (!is.null(trend_setup)) trend_setup$brmsfit else NULL,
      backend = backend,
      algorithm = "none",
      brms_version = utils::packageVersion("brms"),
      mvgam_version = utils::packageVersion("mvgam"),
      creation_time = Sys.time(),
      criteria = list()
    ),
    class = c("mvgam", "mvgam_prefit")
  )
  mvgam_object
}



# ------------------------------------------------------------------------------
# COMPONENT EXTRACTION
# ------------------------------------------------------------------------------
# Extracts mvgam-specific metadata and information from the combined fit to
# enable specialized State-Space model functionality and analysis.

#' Extract mvgam-Specific Components
#' @param combined_fit Stan fit object
#' @param obs_setup Observation setup
#' @param trend_setup Trend setup
#' @param mv_spec Multivariate specification
#' @return List of mvgam-specific components
#' @noRd
extract_mvgam_components <- function(combined_fit, obs_setup, trend_setup,
                                    mv_spec) {
  # Extract time series information
  time_info <- extract_time_information(obs_setup$data)

  # Extract series information
  series_info <- extract_series_information(obs_setup$data, mv_spec)

  # Extract trend components if available
  trend_components <- if (!is.null(mv_spec$has_trends) &&
                         mv_spec$has_trends) {
    extract_trend_component_info(combined_fit, mv_spec)
  } else {
    NULL
  }

  return(list(
    time_info = time_info,
    series_info = series_info,
    trend_components = trend_components
  ))
}

#' Extract Time Information from Data
#' @param data Model data frame
#' @return List with time-related metadata
#' @noRd
extract_time_information <- function(data) {
  if ("time" %in% names(data)) {
    list(
      n_timepoints = length(unique(data$time)),
      time_range = range(data$time, na.rm = TRUE),
      time_spacing = diff(sort(unique(data$time)))[1],
      has_time = TRUE
    )
  } else {
    list(has_time = FALSE)
  }
}

#' Extract Series Information from Data
#' @param data Model data frame
#' @param mv_spec Multivariate specification
#' @return List with series-related metadata
#' @noRd
extract_series_information <- function(data, mv_spec) {
  series_info <- list()

  if ("series" %in% names(data)) {
    series_info$n_series <- length(unique(data$series))
    series_info$series_names <- unique(data$series)
    series_info$has_series = TRUE
  } else {
    series_info$has_series <- FALSE
  }

  # Add multivariate response information
  if (!is.null(mv_spec$response_names)) {
    series_info$response_names <- mv_spec$response_names
    series_info$n_responses <- length(mv_spec$response_names)
    series_info$is_multivariate <- TRUE
  } else {
    series_info$is_multivariate <- FALSE
  }

  return(series_info)
}

#' Extract Trend Component Information
#' @param combined_fit Stan fit object
#' @param mv_spec Multivariate specification
#' @return List with trend component metadata
#' @noRd
extract_trend_component_info <- function(combined_fit, mv_spec) {
  trend_info <- list()

  if (!is.null(mv_spec$trend_specs)) {
    trend_info$specifications <- mv_spec$trend_specs

    # Handle both single spec and list of specs
    specs_list <- if (inherits(mv_spec$trend_specs, "mvgam_trend")) {
      list(mv_spec$trend_specs)
    } else {
      mv_spec$trend_specs
    }

    trend_info$n_trends <- length(specs_list)

    # Extract trend types
    trend_info$types <- sapply(specs_list, function(spec) {
      if (inherits(spec, "mvgam_trend")) {
        spec$trend
      } else {
        "custom"
      }
    })
  }

  return(trend_info)
}

# ==============================================================================
# MULTIPLE IMPUTATION SUPPORT: RUBIN'S RULES POOLING
# ==============================================================================
# Provides multiple imputation support using Rubin's rules for
# proper uncertainty quantification when dealing with missing data in State-
# Space models, ensuring valid statistical inference.

#' Fit mvgam Models to Multiple Imputation Datasets
#'
#' @description
#' Fits Bayesian state-space models to multiply imputed datasets,
#' combining posteriors across imputations for proper uncertainty
#' quantification when dealing with missing data.
#'
#' @details
#' This function processes multiple imputed datasets in two modes:
#'
#' **Combined Mode (combine=TRUE)**:
#' Fits separate models to each imputed dataset, then combines
#' posteriors using `rstan::sflist2stanfit()` to create a pooled
#' posterior distribution. Returns an object of class
#' `c("mvgam_pooled", "mvgam", "brmsfit")` with MI-specific
#' attributes and methods.
#'
#' **List Mode (combine=FALSE)**:
#' Returns a list of individual mvgam fits, one per imputation,
#' allowing manual posterior combination or separate analysis.
#'
#' **Posterior Combination Approach**:
#' Uses `rstan::sflist2stanfit()` (validated brms pattern) to
#' concatenate draws from all imputations at the Stan level,
#' properly accounting for between-imputation and
#' within-imputation variance. This approach ensures valid
#' uncertainty quantification following proper multiple imputation
#' principles.
#'
#' **MI Diagnostics**:
#' Pooled objects include convergence diagnostics per imputation
#' (Rhat, ESS) accessible via `summary()`. The print method
#' displays MI diagnostics including total draws,
#' draws per imputation, and convergence summaries.
#'
#' @param formula Model formula (observation model). Supports brms
#'   formula syntax including smooths and random effects.
#' @param trend_formula Optional trend formula specifying
#'   state-space dynamics. Can be response-specific in multivariate
#'   models.
#' @param data_list List of imputed data frames. Each element must
#'   have identical structure (same variables, same ordering).
#'   Alternatively, a `mids` object from the mice package.
#' @param backend Character string specifying Stan backend:
#'   "cmdstanr" or "rstan". Defaults to
#'   `getOption("brms.backend", "cmdstanr")`.
#' @param combine Logical; if `TRUE` (default), combines posteriors
#'   into a pooled mvgam_pooled object. If `FALSE`, returns list of
#'   individual fits.
#' @param check_data Logical; if `TRUE` (default), validates
#'   consistency across imputations (same column names, types,
#'   rows).
#' @param newdata Optional held-out `data.frame` persisted on
#'   every imputation fit, so later plot / forecast methods can
#'   reach it through any one of them. Defaults to `NULL`.
#' @param ... Additional arguments passed to `mvgam()` for each
#'   imputation (e.g., chains, iter, family, priors).
#'
#' @return
#' If `combine=TRUE`: An object of class
#' `c("mvgam_pooled", "mvgam", "brmsfit")` containing:
#' \itemize{
#'   \item{Pooled posterior draws from all imputations}
#'   \item{Attribute `individual_fits`: List of per-imputation
#'     mvgam objects}
#'   \item{Attribute `n_imputations`: Number of imputations}
#'   \item{Attribute `combination_method`: "sflist2stanfit"}
#' }
#'
#' If `combine=FALSE`: A list of mvgam objects, one per imputation.
#'
#' @examples
#' \dontrun{
#' # Create pseudo-imputed data (3 imputations)
#' base_data <- data.frame(
#'   time = 1:24,
#'   series = factor(rep("series1", 24)),
#'   y = rnorm(24, mean = 3, sd = 1),
#'   season = 1:24
#' )
#'
#' imputed_list <- lapply(1:3, function(i) {
#'   data_copy <- base_data
#'   data_copy$y <- data_copy$y + rnorm(nrow(data_copy), 0, 0.1)
#'   data_copy
#' })
#'
#' # Fit with combined posteriors (recommended)
#' fit_pooled <- mvgam_multiple(
#'   formula = y ~ s(season, bs = "cc", k = 5),
#'   trend_formula = ~ 1,
#'   data_list = imputed_list,
#'   family = gaussian(),
#'   combine = TRUE
#' )
#'
#' # Check MI diagnostics
#' summary(fit_pooled)
#' print(fit_pooled)
#'
#' # Fit separately (for finer control)
#' fit_list <- mvgam_multiple(
#'   formula = y ~ s(season, bs = "cc", k = 5),
#'   trend_formula = ~ 1,
#'   data_list = imputed_list,
#'   family = gaussian(),
#'   combine = FALSE
#' )
#' }
#'
#' @seealso \code{\link{summary.mvgam_pooled}},
#'   \code{\link{print.mvgam_pooled_summary}}, \code{\link{mvgam}}
#'
#' @export
mvgam_multiple <- function(formula,
                           trend_formula = NULL,
                           data_list,
                           backend = getOption("brms.backend", "cmdstanr"),
                           combine = TRUE,
                           check_data = TRUE,
                           newdata = NULL,
                           ...) {
  # Input validation
  checkmate::assert_list(data_list, min.len = 2)
  checkmate::assert_character(backend, len = 1, any.missing = FALSE)
  checkmate::assert_logical(combine, len = 1, any.missing = FALSE)
  checkmate::assert_logical(check_data, len = 1, any.missing = FALSE)

  # Handle mids objects from mice package
  if (inherits(data_list, "mids")) {
    rlang::check_installed("mice",
      reason = "for multiple imputation with mids objects"
    )
    n_imp <- data_list$m
    data_list <- lapply(seq_len(n_imp), function(i) {
      mice::complete(data_list, i)
    })
  }

  # Validate all elements are data frames
  if (!all(sapply(data_list, is.data.frame))) {
    stop(insight::format_error(c(
      cli::format_inline(
        "All elements in {.field data_list} must be data.frames."
      ),
      x = "Found non-data.frame elements in imputation list."
    )))
  }

  # Validate multiple imputation datasets
  if (check_data) {
    validate_multiple_imputation_datasets(data_list)
  }

  # Fit individual models to each imputed dataset. The same
  # `newdata` is persisted on every imputation fit so downstream
  # plot / forecast surfaces can reach it through any one of
  # them.
  individual_fits <- fit_multiple_imputation_models(
    formula = formula,
    trend_formula = trend_formula,
    data_list = data_list,
    backend = backend,
    newdata = newdata,
    ...
  )

  # Return combined or individual results
  if (combine) {
    pooled_fit <- pool_mvgam_fits(individual_fits)
    return(pooled_fit)
  } else {
    return(individual_fits)
  }
}

# ------------------------------------------------------------------------------
# DATASET VALIDATION
# ------------------------------------------------------------------------------
# Ensures that multiple imputation datasets meet requirements for valid
# statistical inference, including structural consistency and proper handling
# of time series identifiers.

#' Validate Multiple Imputation Datasets
#' @param data_list List of imputed datasets
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_multiple_imputation_datasets <- function(data_list) {
  checkmate::assert_list(data_list, min.len = 2)

  # Check all elements are data frames
  if (!all(sapply(data_list, is.data.frame))) {
    stop(insight::format_error(c(
      "All elements in data_list must be data.frames.",
      x = "Found non-data.frame elements in imputation list."
    )))
  }

  # Get reference structure from first dataset
  ref_data <- data_list[[1]]
  ref_names <- names(ref_data)
  ref_nrow <- nrow(ref_data)

  # Validate consistency across datasets
  for (i in seq_along(data_list)[-1]) {
    current_data <- data_list[[i]]

    # Check column names match
    if (!identical(names(current_data), ref_names)) {
      stop(insight::format_error(c(
        paste("Dataset", i, "has different column names than dataset 1."),
        x = "All imputed datasets must have identical structure."
      )))
    }

    # Check number of rows match
    if (nrow(current_data) != ref_nrow) {
      stop(insight::format_error(c(
        paste("Dataset", i, "has", nrow(current_data), "rows, expected",
             ref_nrow),
        x = "All imputed datasets must have same number of observations."
      )))
    }

    # Check essential columns (time, series) are identical
    essential_cols <- intersect(c("time", "series"), ref_names)
    for (col in essential_cols) {
      if (!identical(ref_data[[col]], current_data[[col]])) {
        stop(insight::format_error(c(
          paste("Column", col, "differs between datasets."),
          x = "Time and series identifiers must be identical across imputations."
        )))
      }
    }
  }

  # Validate missing data patterns
  validate_missing_patterns(data_list)

  invisible(TRUE)
}

#' Validate Missing Data Patterns
#' @param data_list List of imputed datasets
#' @return Invisible TRUE if valid, warns about potential issues
#' @noRd
validate_missing_patterns <- function(data_list) {
  n_datasets <- length(data_list)
  dataset_names <- names(data_list[[1]])

  # Check for variables that should not be imputed
  non_imputable <- c("time", "series", "weights", "trials")
  present_non_imputable <- intersect(non_imputable, dataset_names)

  for (col in present_non_imputable) {
    values_list <- lapply(data_list, function(d) d[[col]])

    # Check if any differences exist
    reference_values <- values_list[[1]]
    for (i in 2:n_datasets) {
      if (!identical(reference_values, values_list[[i]])) {
        insight::format_warning(c(
          paste("Column", col, "varies between imputed datasets."),
          x = "This may indicate improper imputation of structural variables."
        ))
      }
    }
  }

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# INDIVIDUAL MODEL FITTING
# ------------------------------------------------------------------------------
# Fits separate mvgam models to each imputed dataset using consistent
# specifications to enable proper pooling of results.

#' Fit Models to Multiple Imputation Datasets
#' @param formula Main formula
#' @param trend_formula Trend formula
#' @param data_list List of datasets
#' @param backend Stan backend
#' @param ... Additional arguments
#' @return List of fitted mvgam objects
#' @noRd
fit_multiple_imputation_models <- function(formula, trend_formula, data_list,
                                          backend, ...) {
  n_datasets <- length(data_list)

  insight::format_message(
    paste("Fitting mvgam models to", n_datasets, "imputed datasets..."),
    "This may take some time depending on model complexity."
  )

  # Fit individual models
  fits <- vector("list", n_datasets)
  names(fits) <- paste0("imputation_", seq_len(n_datasets))

  for (i in seq_len(n_datasets)) {
    insight::format_message(
      paste("Fitting imputation", i, "of", n_datasets, "...")
    )

    # Fit model using standard mvgam_single function
    fits[[i]] <- mvgam_single(
      formula = formula,
      trend_formula = trend_formula,
      data = data_list[[i]],
      backend = backend,
      ...
    )

    # Add imputation metadata
    fits[[i]]$imputation_id <- i
    fits[[i]]$n_imputations <- n_datasets
  }

  return(fits)
}


# ------------------------------------------------------------------------------
# POSTERIOR COMBINATION
# ------------------------------------------------------------------------------
# Combines posterior samples from multiple imputation fits using Stan-level
# combination. Follows brms pattern using rstan::sflist2stanfit().

#' Pool mvgam Fits Using Stan-Level Combination
#' @param fits List of individual mvgam fits
#' @return mvgam_pooled object with combined posteriors
#' @noRd
pool_mvgam_fits <- function(fits) {
  # Input validation
  checkmate::assert_list(fits, min.len = 2)

  if (!all(sapply(fits, function(x) inherits(x, "mvgam")))) {
    stop(insight::format_error(c(
      "All fits must be mvgam objects.",
      x = "Cannot combine fits of different types."
    )))
  }

  # Validate parameter consistency across fits
  ref_vars <- sort(variables(fits[[1]]))
  for (i in seq_along(fits)[-1]) {
    current_vars <- sort(variables(fits[[i]]))
    if (!identical(ref_vars, current_vars)) {
      stop(insight::format_error(c(
        sprintf("Model 1 and %d have different parameters.", i),
        x = "This may indicate fitting failures or model changes."
      )))
    }
  }

  n_imp <- length(fits)
  insight::format_message(
    sprintf("Combining posteriors across %d imputations...", n_imp)
  )

  # Extract stanfit objects
  sflist <- lapply(fits, function(fit) fit$fit)

  # Combine using rstan::sflist2stanfit()
  combined_stanfit <- rstan::sflist2stanfit(sflist)

  # Create combined object using first fit as template
  combined_fit <- fits[[1]]
  combined_fit$fit <- combined_stanfit

  # Store individual fits and metadata
  attr(combined_fit, "individual_fits") <- fits
  attr(combined_fit, "n_imputations") <- n_imp
  attr(combined_fit, "combination_method") <- "sflist2stanfit"
  attr(combined_fit, "combination_time") <- Sys.time()

  # Set class
  class(combined_fit) <- c("mvgam_pooled", "mvgam", "brmsfit")

  insight::format_message("Successfully combined posteriors.")
  return(combined_fit)
}

