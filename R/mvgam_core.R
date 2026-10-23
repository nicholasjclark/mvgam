# ==============================================================================
# mvgam core: model fitting and multiple imputation
# ==============================================================================
# `mvgam()` fits one data frame, or a list of imputed frames through
# `mvgam_multiple()`. It returns an `mvgam` object for the post-fit
# methods.

#' Arguments of mvgam 1.x that 2.0 refuses
#'
#' Each is paired with the 2.0 spelling of the same request. Every one
#' names a modelling choice. Left in `...` it would reach brms, which
#' drops what it does not recognise and fits a different model.
#'
#' @noRd
mvgam_removed_args <- c(
  share_obs_params = paste0(
    "a distributional formula such as 'bf(y ~ x, sigma ~ series)' ",
    "gives each series its own family parameters"
  ),
  trend_model = paste0(
    "trend constructors are written inside 'trend_formula', as in ",
    "'trend_formula = ~ AR(p = 1)'"
  ),
  trend_knots = paste0(
    "knot values are named by covariate in 'knots', one argument ",
    "for the whole model"
  ),
  use_lv = paste0(
    "a dynamic factor model is requested with 'n_lv' on the trend ",
    "constructor, as in 'trend_formula = ~ AR(p = 1, n_lv = 3)'"
  ),
  noncentred = "the trend parameterisation is chosen by mvgam",
  prior_simulation = "use 'sample_prior = \"only\"'",
  return_model_data = "use 'standata()' on the fitted model",
  save_all_pars = paste0(
    "the parameters a fit keeps are chosen with 'save_pars', as in ",
    "'save_pars = brms::save_pars(all = TRUE)'"
  ),
  parallel = "'cores' sets how many chains run in parallel",
  sparse = paste0(
    "a sparse design matrix is requested on the formula, as in ",
    "'bf(y ~ x, sparse = TRUE)'"
  ),
  stan_funs = paste0(
    "extra Stan code is supplied through 'stanvars', as in ",
    "'stanvars = brms::stanvar(scode = ..., block = \"functions\")'"
  )
)

#' The NUTS settings both backends take under one spelling
#'
#' rstan takes them inside `control`. The cmdstanr path flattens
#' `control` into `model$sample()`, which declares each of these as a
#' formal.
#'
#' @noRd
mvgam_nuts_control_args <- c(
  "adapt_delta", "max_treedepth", "adapt_engaged", "metric", "stepsize"
)

#' The fit settings `mvgam_single()` takes from its dots
#'
#' `mvgam_single()` passes them to `fit_model()` by name. With the code
#' generator's formals and the NUTS settings, they are the dots
#' withheld from the sampler.
#'
#' @noRd
mvgam_fit_settings <- c(
  "algorithm", "iter", "warmup", "thin", "chains", "cores", "threads",
  "opencl", "init", "seed", "control", "silent", "future",
  "cpp_options", "stanc_options"
)

#' Move bare NUTS settings into `control`
#'
#' rstan takes the NUTS settings only inside `control`. A bare
#' `adapt_delta = 0.99` would sample at the default delta of 0.8. The
#' bare spelling moves into `control`, which both backends accept and
#' `control_params()` reports.
#'
#' @param dots List of the call's dots
#' @param control List of sampler control settings, or `NULL`
#' @return The control list with the bare settings added
#' @noRd
lift_sampler_control <- function(dots, control = NULL) {
  checkmate::assert_list(dots)
  checkmate::assert_list(control, null.ok = TRUE)
  bare <- intersect(names(dots), mvgam_nuts_control_args)
  if (!length(bare)) {
    return(control)
  }
  clash <- intersect(bare, names(control))
  if (length(clash)) {
    stop(insight::format_error(c(
      "Sampler settings were given twice.",
      x = paste0(
        "Given both as arguments and inside 'control': ",
        paste0("'", clash, "'", collapse = ", "), "."
      ),
      i = "Keep one spelling."
    )), call. = FALSE)
  }
  c(control, dots[bare])
}

#' Refuse arguments mvgam 2.0 removed
#'
#' Every entry point that reaches the code generator calls this on its
#' dots. A removed argument is named where it is written. In `...` it
#' would pass to brms without effect.
#'
#' @param dots List of the call's dots
#' @param fn Name of the calling function, for the message
#' @param removed Named character vector of removed arguments and their
#'   replacements
#' @return `NULL`, invisibly, or an error
#' @noRd
reject_removed_args <- function(dots, fn = "mvgam",
                                removed = mvgam_removed_args) {
  checkmate::assert_list(dots)
  checkmate::assert_string(fn, min.chars = 1)
  checkmate::assert_character(removed, names = "named")
  found <- intersect(names(removed), names(dots))
  if (!length(found)) {
    return(invisible(NULL))
  }
  stop(insight::format_error(c(
    cli::format_inline(
      "{.fn {fn}} no longer takes {.arg {found}}."
    ),
    x = cli::format_inline(
      "{cli::qty(found)}{?This argument was/These arguments were} ",
      "removed in mvgam 2.0."
    ),
    stats::setNames(
      paste0("For '", found, "', ", removed[found], "."),
      rep("i", length(found))
    )
  )), call. = FALSE)
}

#' Translate the deprecated `samples` / `burnin` pair to `iter` / `warmup`
#'
#' `samples` counts post-warmup draws and `burnin` warmup draws. The
#' backends take `iter`, the total iterations, and `warmup`. In `...`
#' the deprecated names would reach no consumer. Every fit would then
#' take cmdstanr's `iter = 2000, warmup = 1000` default. `mvgam()`
#' calls this before dispatching.
#'
#' A user supplying both pairs on one call is refused. They set the
#' same sampler budget.
#'
#' `mvgam_use_alias()` (R/mvgam_model_helpers.R) handles a
#' single-argument alias that maps one name to another without
#' arithmetic. This pair needs arithmetic (`iter = samples + burnin`,
#' `warmup = burnin`).
#'
#' @param dots List of the call's dots
#' @return `dots` with `iter` and `warmup` set from the deprecated names
#'   (each 1000 when only one of the pair was given) and those names
#'   removed. The attribute `translated` is `TRUE` when a deprecated
#'   name was present. The caller then dispatches again with the new
#'   names.
#' @noRd
translate_samples_burnin <- function(dots) {
  checkmate::assert_list(dots)
  has_deprecated <- any(c("samples", "burnin") %in% names(dots))
  if (!has_deprecated) {
    attr(dots, "translated") <- FALSE
    return(dots)
  }
  if (any(c("iter", "warmup") %in% names(dots))) {
    stop(insight::format_error(c(
      "Do not mix 'samples'/'burnin' with 'iter'/'warmup'.",
      x = "'samples'/'burnin' and 'iter'/'warmup' set the same budget.",
      i = paste0(
        "'samples' and 'burnin' are deprecated in favour of 'iter' and ",
        "'warmup'."
      )
    )), call. = FALSE)
  }
  warn_once(
    c(
      "mvgam(): 'samples' and 'burnin' are deprecated.",
      i = paste0(
        "Use 'iter' (total iterations, warmup + post-warmup) and ",
        "'warmup' instead."
      )
    ),
    "mvgam_samples_burnin_deprecated"
  )
  samples <- dots$samples %||% 1000L
  burnin <- dots$burnin %||% 1000L
  dots$iter <- samples + burnin
  dots$warmup <- burnin
  dots$samples <- NULL
  dots$burnin <- NULL
  attr(dots, "translated") <- TRUE
  dots
}

#' The `mvgam()` formals a per-imputation fit needs
#'
#' A forwarding call reaches a formal only by name. The call itself
#' supplies `data` and `combine`. Everything a user passes through
#' `...` travels on its own. `tests/testthat/test-mvgam-core.R`
#' asserts this covers every formal the single-dataset path uses.
#'
#' @noRd
mvgam_imputation_forwarded <- c(
  "formula", "trend_formula", "newdata", "trend_map", "loadings_prior",
  "backend", "family", "threads", "save_pars", "run_model"
)


#' Fit a Bayesian multivariate state-space model
#'
#' @description
#' Fits the model in Stan. The observation `formula` takes brms syntax,
#' and `trend_formula` adds a latent dynamic process, such as a random
#' walk or an autoregression, with predictors of its own. A list of
#' multiply imputed data frames fits the model to each frame and pools
#' the draws.
#'
#' @param formula Main observation model formula (supports brms syntax).
#'   Smooth specials `s()`, `t2()`, `gp()`, varying intercepts and
#'   slopes, monotonic `mo()`, measurement-error `me()`, distributional
#'   `dpar` sub-formulas and `offset()` are all available. `gp()`
#'   accepts both the approximate Hilbert-space form
#'   (`gp(x, k = 20)`) and the exact full-covariance form
#'   (`gp(x)`, no `k`). Prediction at `newdata` requires the
#'   approximate form. An exact `gp()` term fits and scores
#'   in-sample. `mvgam()` says so once per session.
#' @param trend_formula A one-sided formula for the latent process:
#'   a trend constructor such as `AR()` with any predictors of the
#'   latent state. Write [lv_axis()] in a term to vary its effect by
#'   latent factor, or by series in a model without factors, as in
#'   `~ x:lv_axis() + AR()`. The series column itself is not
#'   supported in a term of this formula. An exact `gp()` term here
#'   also predicts in-sample alone. Forecasting a model that holds one
#'   requires the approximate form.
#' @param data Data frame or list of multiply imputed datasets
#' @param newdata Optional held-out `data.frame`, stored on the fit
#'   as `object$test_data`. `forecast()` forecasts to it when called
#'   without `newdata`. `plot(fit, type = "series")` overlays it.
#'   The fit itself draws no forecasts. `forecast()` propagates the
#'   fitted latent state forward in time per posterior draw.
#'   `posterior_predict(mod, newdata = mod$test_data)` marginalises
#'   over the trend's stochastic dynamics at the supplied design
#'   points. Use `forecast()` for time-series forecasting and
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
#'   loadings priors. The mapping is stored on a trend spec and
#'   requires a `trend_formula`. Supplying it without one is an
#'   error.
#' @param loadings_prior Optional named list specifying a
#'   structured prior on the unconstrained factor loadings
#'   matrix following Heaps & Jermyn (2024). The loadings belong
#'   to a trend with latent factors. The argument requires a
#'   `trend_formula`. Supplying it without one is an error. When
#'   supplied, the
#'   default iid `to_vector(Z) ~ student_t(3, 0, 0.5)` prior is
#'   replaced with a per-column matrix-normal prior of the form
#'   `Z[, i] ~ multi_normal_cholesky(0, L_Phi)`, where the
#'   among-row scale matrix `Phi` is assembled
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
#'       Heaps Eq. (9)). `"mgp"` applies its column scale through
#'       the shared innovation scale. It is available for `AR()`,
#'       `RW()` and `ZMVN()` trends. `VAR()` builds its own innovation
#'       covariance and has no shared scale for it; pair a `VAR()`
#'       factor model with `"iid"` instead.
#'     \item `mgp_a1`, `mgp_a2`: numeric MGP hyperparameters,
#'       only used when `column_shrinkage = "mgp"`. Default
#'       `(2, 4)` lies within the `a2 in [3, 5]` range
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
#'   model block. This prior is set on the standardised distance
#'   scale. Pairwise distance matrices are rescaled to
#'   `max(d) = 1` inside the normaliser. A length-scale of 1 then
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
#'   Identification. The structured prior applies to the sampled
#'   columns of `Z`. Where the QR rotation gives the reported
#'   loadings `Z_tilde`, each of its columns mixes the sampled
#'   columns. `shared_variation()` and `residual_cor()` use
#'   `Z Z'`, which is the same before and after the rotation. See
#'   the Identification section of the trend constructors.
#'
#'   Compatibility with `trend_map`:
#'   \itemize{
#'     \item `trend_map = NULL` (default): compatible. `n_lv` alone
#'       triggers the free-Z factor model; `loadings_prior` applies
#'       to the sampled `Z` column-wise.
#'     \item `trend_map = matrix(NA_real_, n_series, n_lv)`
#'       (all-NA mask, used internally by `jsdgam()`): compatible.
#'       Treated as a free-Z trigger upstream; `loadings_prior`
#'       applies as if `trend_map` were `NULL`.
#'     \item `trend_map` with a mix of fixed entries and `NA`s:
#'       incompatible. Partial-Z parameterises the free entries
#'       as a vector under an iid `student_t(3, 0, 1)` prior,
#'       which would overlap with the matrix-normal structure
#'       and double-count regularisation.
#'     \item Fully-fixed `trend_map` (no `NA`s): incompatible.
#'       The structured prior has no free parameters to act on.
#'   }
#' @param backend Stan backend (defaults to "cmdstanr")
#' @param combine Logical, pool multiple imputation results (default TRUE)
#' @param family A description of the response distribution and link
#'   function, given as a family function, a call to one or its name.
#'   Supports most brms families, including the ordinal families
#'   ([brms::cumulative()], [brms::sratio()], [brms::cratio()],
#'   [brms::acat()]). The brms families needing a separate linear
#'   predictor per response category, [brms::categorical()],
#'   [brms::multinomial()] and [brms::dirichlet()], are refused: use
#'   [categ()], [multi()] and [diri()], which take the categories in
#'   long format and share one latent structure across them. A family
#'   whose mean has to stay positive needs a link that keeps it there
#'   once a trend enters the linear predictor: pair [stats::Gamma()],
#'   [brms::weibull()], [brms::exponential()], [brms::frechet()] and
#'   [stats::inverse.gaussian()] with `link = "log"`. Their default
#'   links leave the mean positive only where the predictor is. A
#'   latent trend is centred near zero and reaches negative values,
#'   where the likelihood is undefined. `mvgam()` warns before
#'   fitting. [brms::lognormal()] is the exception: its `mu` is a
#'   log-scale location that takes any sign.
#'
#'   A family written inside a univariate [brms::bf()] is the model's
#'   family, as in brms. In a multivariate model each response may take
#'   its own family, in which case name it inside that response's
#'   [brms::bf()]: `bf(count ~ x, family = poisson()) + bf(seen ~ x,
#'   family = bernoulli())`. Passing a list of families to this
#'   argument, as [brms::brm()] allows, is not supported; the family
#'   belongs to the response that names it. A single family given here
#'   applies to every response that does not name one of its own.
#'   Closure-unit and multi-response families ([occ()], [nmix()],
#'   [mvn()], [mvt()], [diri()], [multi()], [categ()]) lay their data
#'   out by unit. Each models its response alone and cannot be one
#'   response of a multivariate model.
#' @param threads Positive integer or `NULL`. When non-NULL the
#'   model is compiled with `cpp_options$stan_threads = TRUE` and
#'   cmdstanr passes `threads_per_chain = N` at sample time.
#'   Closure-unit families (`nmix()`, `occ()`) and multi-response
#'   families (`diri()`, `mvn()`, `mvt()`, `multi()`, `categ()`)
#'   thread their per-unit lpmf via `reduce_sum`; brms-native
#'   families (`gaussian()`, `poisson()`, etc.) without a
#'   `trend_formula` thread their lpmf loops via brms's
#'   `partial_log_lik_lpmf`. For closure-unit families, expect
#'   ~30-40% sampling throughput improvement at `threads = 4` on a
#'   fixture with N_unit >= 50; smaller fixtures may see
#'   thread-overhead-dominated regressions. The internal grainsize
#'   is auto-tuned to ~8 chunks regardless of `threads`.
#'
#'   A brms-native family with a `trend_formula` samples serially
#'   whatever `threads` says. When it threads, brms moves the `mu`
#'   assignment inside `partial_log_lik_lpmf`, where mvgam cannot add
#'   the trend to it. The program is then built without threading.
#'   This applies to every trend model under a brms-native family:
#'   `RW()`, `AR()`, `VAR()`, `LLT()`, `ZMVN()`, `CAR()` and
#'   `PW()`. Each fit that drops the request warns. A batch script
#'   that fits repeatedly shows it every time.
#' @param run_model Logical. Setting `run_model = FALSE` stops before
#'   Stan parse, compile and sampling and returns a stub `mvgam`
#'   object whose `$stancode` and `$standata` slots are populated but
#'   whose `$fit` is `NULL`. Methods that need a fitted model
#'   (`summary()`, `predict()`, `forecast()`, `loo()`) reject the
#'   stub and say which helper to reach for. Where the model can be
#'   written as an [`mvgam_formula()`], calling [`stancode()`] or
#'   [`standata()`] on that object says the same thing more directly;
#'   `run_model = FALSE` is the route for a specification that only
#'   [`jsdgam()`] knows how to build. Both helpers build the same Stan
#'   code and data as `mvgam()`. Defaults to `TRUE`.
#' @param save_pars A [brms::save_pars()] object naming which
#'   parameters the posterior keeps. A model is built from working
#'   variables it also reports under another name: the standardised
#'   deviates a group-level block is scaled from, the unscaled
#'   coefficients a shrinkage prior scales, the ordered intercepts a
#'   mixture is identified by. Those are left out by default, as brms
#'   leaves them out. `save_pars(all = TRUE)` keeps every one. `group`
#'   and `latent` keep the per-level deviations and the latent values
#'   of a `me()` covariate. `manual` names individual parameters to
#'   keep.
#' @param ... Additional arguments passed to Stan fitting. `cores` sets
#'   how many chains sample at once. It defaults to
#'   `getOption("mc.cores")` when that option is set. Otherwise it is
#'   one fewer than the machine's cores, up to `chains`. Two more are
#'   worth calling out. `algorithm` selects how the posterior is explored and
#'   accepts `"sampling"` (the default), `"meanfield"`, `"fullrank"` or
#'   `"fixed_param"` on either backend, plus `"pathfinder"` and
#'   `"laplace"` when `backend = "cmdstanr"`. `init` sets the starting
#'   values and accepts `"random"` (the default), `"0"`, a numeric
#'   value, a list or a function. With `backend = "cmdstanr"` it also
#'   accepts `"pathfinder"`. The `"pathfinder"` keyword runs Stan's
#'   Pathfinder approximation on the compiled model and draws one
#'   starting value per chain from it. The algorithm you asked for
#'   still explores the posterior. The fit remains exact.
#'
#'   `init = "pathfinder"` is a recovery option. On models that already
#'   warm up cleanly it is slower: across random walk, autoregressive,
#'   correlated autoregressive and dynamic factor trends with 60 to 750
#'   latent states, it added roughly a second of wall time. Warmup
#'   duration was essentially unchanged. Effective samples per second
#'   fell by 15 to 25 percent. It pays off on models that struggle to
#'   leave a poor starting point at all. There the extra second buys a
#'   warmup that would otherwise stall or fill with divergences.
#'
#'   Four arguments shape the Stan program. brms receives them
#'   unchanged. `knots` gives knot positions by covariate name and
#'   reaches the basis constructors for the observation and trend
#'   formulas alike. `drop_unused_levels` decides whether a factor
#'   level with no observations still earns a coefficient.
#'   `sample_prior` takes `"no"`, `"yes"` or `"only"`. Under `"only"`
#'   the observation likelihood drops out while the latent process
#'   keeps its own prior. The draws then describe the model before it
#'   sees the response. `normalize` decides whether the sampling
#'   statements carry their normalising constants; dropping them is
#'   faster and leaves the posterior unchanged. `save_model` names a
#'   file to write the assembled Stan program to.
#' @return An object of class `mvgam`.
#'
#' @examples
#' \dontrun{
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
#' summary(mod)
#' conditional_effects(mod)
#' mcmc_plot(mod, variable = "^ar1", regex = TRUE, type = "hist")
#'
#' # ---- Positive continuous response with a trend ----
#' # A mean that must stay positive needs `link = "log"` once a trend
#' # enters the linear predictor. `stats::Gamma()` supplies the
#' # `inverse` link by default. A trend reaching negative values then
#' # leaves the likelihood undefined. mvgam warns on that pairing.
#' set.seed(3)
#' gamdat <- sim_mvgam(family = Gamma(link = "log"))
#' gmod <- mvgam(
#'   y ~ s(x),
#'   trend_formula = ~ AR(p = 1),
#'   data = gamdat$data_train,
#'   family = Gamma(link = "log"),
#'   chains = 2,
#'   silent = 2
#' )
#' summary(gmod)
#'
#' # ---- Multivariate VAR(1) with intercept suppression + custom priors ----
#' # Three correlated series, no observation intercept (`y ~ 0`),
#' # a per-series smooth on the trend side and tighter custom
#' # priors on the regression coefficients and trend innovation
#' # scale. Mirrors the workflow used in the README's Portal VAR
#' # example.
#' set.seed(2)
#' vardat <- sim_mvgam(
#'   family = poisson(),
#'   n_series = 3L,
#'   n_timepoints = 120L,
#'   trend_model = VAR(cor = TRUE)
#' )
#'
#' # Inspect the default priors before fitting. Wrap the formula
#' # pair in `mvgam_formula()` so `get_prior()` can dispatch. The
#' # returned table lists every adjustable prior row keyed by
#' # `class` and `coef`; pass a modified subset back via
#' # `priors = c(prior(...), ...)` to override.
#' mf <- mvgam_formula(
#'   formula = y ~ 0,
#'   trend_formula = ~ s(x, k = 5, by = lv_axis()) + VAR()
#' )
#' get_prior(mf, data = vardat$data_train, family = poisson())
#'
#' var_mod <- mvgam(
#'   formula = y ~ 0,
#'   trend_formula = ~ s(x, k = 5, by = lv_axis()) + VAR(),
#'   data = vardat$data_train,
#'   family = poisson(),
#'   priors = c(
#'     prior(normal(0, 2), class = b),
#'     prior(exponential(2.5), class = sigma_trend)
#'   ),
#'   chains = 2,
#'   iter = 1000,
#'   warmup = 500,
#'   silent = 2
#' )
#' summary(var_mod)
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
#'   data = tvdat,
#'   family = gaussian(),
#'   chains = 2,
#'   iter = 1000,
#'   warmup = 500,
#'   silent = 2
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
                           save_pars = brms::save_pars(),
                           run_model = TRUE, ...) {

  # Translate the deprecated `samples` / `burnin` argument pair to
  # the `iter` / `warmup` pair the backends take. See
  # `translate_samples_burnin()` for the mapping.
  dots <- translate_samples_burnin(list(...))
  reject_removed_args(dots)
  if (attr(dots, "translated", exact = TRUE)) {
    attr(dots, "translated") <- NULL
    return(do.call(
      mvgam,
      c(
        list(
          formula = formula,
          trend_formula = trend_formula,
          data = data,
          newdata = newdata,
          trend_map = trend_map,
          loadings_prior = loadings_prior,
          backend = backend,
          combine = combine,
          family = family,
          threads = threads,
          save_pars = save_pars,
          run_model = run_model
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
  newdata <- check_newdata(newdata, data, trend_formula)

  # `build_stan_components()` refuses an absent or missing covariate
  # in `data`. `newdata` does not reach it and is checked here. Its
  # responses may be absent, which marks the occasions to forecast.
  if (!is.null(newdata)) {
    resolved <- resolve_observation_family(formula, family)
    validate_model_columns(
      newdata, resolved$formula, resolved$family, trend_formula,
      data2 = list(...)$data2,
      optional = response_columns(resolved$formula),
      context = "newdata"
    )
  }

  # Response-shape guard. brms reports a family's support from
  # `data_response.brmsframe()`, which names neither the column nor
  # the values that broke it. It states only the bound it reached
  # first. Checking here names both and gives the whole constraint.
  validate_response_shapes(data, formula, family)
  if (!is.null(newdata)) {
    # A forecast frame carries an all-NA or absent response, which
    # the check passes over; a value it does carry has to satisfy
    # the family the same way the training response does.
    validate_response_shapes(newdata, formula, family)
  }
  checkmate::assert_string(backend)
  checkmate::assert_flag(combine)
  checkmate::assert_class(save_pars, "save_pars")
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
  # Capture the data name from the user's call. `deparse()` of a
  # literal data frame expression (e.g.
  # `mvgam(data = as.data.frame(long_dat))` or a `do.call(mvgam, ...)`
  # call that resolved `data` to an inline structure) returns the full
  # expansion. That expansion would appear in summary() output. A
  # multi-line deparse is collapsed. A name longer than a typical
  # identifier becomes a short placeholder.
  # This is the last frame that holds the user's symbols. Every step
  # below reaches the pipeline through `do.call()`, which substitutes
  # each argument's value. A call captured deeper would inline the
  # whole data frame and the family object.
  user_call <- match.call()
  data_name <- paste(deparse(user_call$data), collapse = " ")
  if (nchar(data_name) > 80L) {
    data_name <- "<inline data>"
  }

  # Handle multiple imputation input
  if (is.list(data) && !is.data.frame(data)) {
    # `family`, `trend_map`, `loadings_prior`, `threads` and
    # `run_model` are formals. A forwarding call reaches them only by
    # name. Without them every imputation would take the default
    # family.
    forwarded <- mget(
      setdiff(mvgam_imputation_forwarded, "threads"),
      envir = environment()
    )
    # `threads = NULL` would fail the integer assertion downstream. It
    # is forwarded only when the user set it.
    if (!is.null(threads)) forwarded$threads <- threads
    return(do.call(
      mvgam_multiple,
      c(
        list(data_list = data, combine = combine, data_name = data_name),
        forwarded,
        list(...)
      )
    ))
  }

  # A positive mean under a link that admits zero needs the linear
  # predictor to stay positive. A latent trend does not guarantee it.
  # The imputation branch returns earlier. `mvgam_multiple()` raises
  # the same notice once for a whole list.
  warn_positive_mean_link_with_trend(formula, family, trend_formula)

  # Single dataset processing. `threads` is forwarded only when the
  # user set it. `threads = NULL` would fail the
  # `assert_int(threads, lower = 1)` check in `build_stan_components()`,
  # which defaults to one thread.
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
    save_pars = save_pars,
    run_model = run_model
  )
  if (!is.null(threads)) {
    mvgam_single_args$threads <- threads
  }
  mvgam_object <- do.call(mvgam_single, c(mvgam_single_args, list(...)))
  if (inherits(mvgam_object, "mvgam")) {
    mvgam_object$call <- user_call
  }

  # Post-fit advisor: a by_lv factor model at the full-rank boundary
  # with the default iid Z prior is rotationally unidentified. When
  # Rhat on init_trend / lv_trend / sigma_trend / Sigma_trend /
  # L_Sigma_trend exceeds 1.1, the advisor points users at the MGP
  # prior or a pinned trend_map. The helper returns early for fits
  # without factors, MGP-prior fits, n_lv != n_series fits and clean
  # Rhats.
  if (isTRUE(run_model) && inherits(mvgam_object, "mvgam") &&
      !is.null(mvgam_object$fit)) {
    warn_by_lv_full_rank_funnel(mvgam_object)
  }

  return(mvgam_object)
}


#' Validate `newdata` against the training data of an \pkg{mvgam} model
#'
#' Runs the structural checks that [predict.mvgam()] and
#' [forecast.mvgam()] apply to a `newdata` frame. A misaligned frame is
#' refused before an expensive prediction. The trend constructor in
#' `trend_formula` names the time and series columns, `time` and
#' `series` by default. Each of them the training `data` holds must
#' also be in `newdata`. A hierarchical trend identifies its series by
#' its `gr` and `subgr` columns. Those columns are then required. Series
#' levels absent from the training data are refused. The series column
#' of `newdata` is coerced to the training levels. The prediction
#' functions check the columns the formulas use.
#'
#' @param newdata A `data.frame` of prediction covariates in the same
#'   shape as the training data.
#' @param data The training `data.frame` of the model, whose series
#'   levels define the valid set. A fitted model from [mvgam()] or
#'   [jsdgam()] is also accepted. `newdata` is then checked against
#'   every column the fitted formulas use. `trend_formula` is then
#'   ignored.
#' @param trend_formula The trend formula passed to [mvgam()], or
#'   `NULL` for a model without a trend.
#' @return `newdata` with its series column coerced to the training
#'   factor levels, returned invisibly, or `NULL` (invisibly) when
#'   `newdata` is `NULL`. Checked against a fitted model, `newdata`
#'   is returned with the columns that model predicts from.
#' @seealso [predict.mvgam()], [forecast.mvgam()]
#' @examples
#' train <- data.frame(
#'   species = factor(rep(c("a", "b"), each = 3L)),
#'   week = rep(1:3, times = 2L),
#'   y = rnorm(6L)
#' )
#' test_weeks <- data.frame(
#'   species = factor(rep(c("a", "b"), each = 2L)),
#'   week = rep(4:5, times = 2L)
#' )
#' check_newdata(test_weeks, train, ~ AR(time = week, series = species))
#'
#' # A species the model never saw is refused.
#' test_weeks$species <- factor(rep(c("a", "c"), each = 2L))
#' try(check_newdata(test_weeks, train, ~ AR(time = week, series = species)))
#' @export
check_newdata <- function(newdata, data, trend_formula = NULL) {
  if (is.null(newdata)) return(invisible(NULL))
  checkmate::assert_data_frame(newdata)
  checkmate::assert(
    checkmate::check_data_frame(data),
    checkmate::check_class(data, "mvgam"),
    .var.name = "data"
  )
  if (inherits(data, "mvgam")) {
    newdata <- prediction_frame(data, newdata)
    validate_newdata_complete(newdata, data)
    validate_prediction_factor_levels(
      newdata, data$trend_metadata,
      series_label = series_column_label(data)
    )
    return(invisible(newdata))
  }
  checkmate::assert_formula(trend_formula, null.ok = TRUE)
  spec <- if (!is.null(trend_formula)) {
    parse_trend_formula(trend_formula)$trend_model
  }
  axis <- spec_axis_vars(spec)
  series_var <- axis$series_var
  gr <- spec_groupings(spec)
  # `gr` alone takes the series column as its subgroup.
  subgr <- gr$subgr %||% series_var
  series_cols <- if (is.null(gr$gr)) series_var else c(gr$gr, subgr)
  required <- intersect(c(axis$time_var, series_cols), names(data))
  if (length(required)) {
    validate_required_variables(newdata, required, "newdata")
  }
  train_levels <- levels(data[[series_var]])
  if (!series_var %in% required || is.null(train_levels)) {
    return(invisible(newdata))
  }
  new_chr <- as.character(newdata[[series_var]])
  refuse_unseen_levels("Series", unique(new_chr), train_levels)
  newdata[[series_var]] <- factor(new_chr, levels = train_levels)
  invisible(newdata)
}

# ------------------------------------------------------------------------------
# Single dataset processing
# ------------------------------------------------------------------------------
# The processing pipeline for one dataset, through the two-stage Stan
# assembly with brms.

#' Process a single dataset
#' @param formula Main formula
#' @param trend_formula Trend formula
#' @param data Single data frame
#' @param backend Stan backend
#' @param family Family specification
#' @param data_name Name of the data in the user's call, or `NULL`
#' @param newdata Held-out data frame to store on the fit, or `NULL`
#' @param trend_map Fixed factor-loading specification, or `NULL`
#' @param loadings_prior Structured loadings prior, or `NULL`
#' @param save_pars A [brms::save_pars()] object
#' @param run_model Logical. `FALSE` returns an unfitted stub.
#' @param ... Additional arguments
#' @return mvgam object
#' @noRd
mvgam_single <- function(formula, trend_formula, data, backend,
                        family, data_name = NULL, newdata = NULL,
                        trend_map = NULL, loadings_prior = NULL,
                        save_pars = brms::save_pars(),
                        run_model = TRUE, ...) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(backend)
  checkmate::assert_string(data_name, null.ok = TRUE)
  checkmate::assert_data_frame(newdata, null.ok = TRUE)
  checkmate::assert_class(save_pars, "save_pars")
  checkmate::assert_flag(run_model)

  mvgam_formula_obj <- mvgam_formula(formula, trend_formula)

  # Reason: brms takes 'prior' (singular). mvgam and jsdgam also accept
  # 'priors'. Without the alias the plural form would fall into `...`,
  # where every consumer that takes `prior = NULL` ignores it.
  forward_dots <- normalise_prior_arg_alias(list(...))

  # The sampler dimensions are checked before any code generation. An
  # impossible iteration count cannot be salvaged later. The user
  # should not wait through a compile to hear about it.
  validate_sampler_iterations(
    iter = forward_dots$iter %||% 2000,
    warmup = forward_dots$warmup
  )

  # The shared infrastructure `stancode()` also uses
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

  # `run_model = FALSE`: stop before parse, compile and fit. A caller
  # can then inspect the generated stancode and standata without
  # paying for Stan code generation and sampling.
  if (isFALSE(run_model)) {
    return(create_mvgam_stub_from_stan_components(
      stan_components = stan_components,
      formula = formula,
      trend_formula = trend_formula,
      family = family,
      data = data,
      data_name = data_name,
      newdata = newdata,
      backend = backend,
      save_pars = save_pars,
      user_prior = forward_dots$prior
    ))
  }

  # The alias-normalised list carries the prior through the fitting
  # branch as well.
  dots <- forward_dots

  # Fitting parameters with defaults
  algorithm <- dots$algorithm %||% "sampling"
  iter <- dots$iter %||% 2000
  warmup <- dots$warmup %||% (iter %/% 2)
  thin <- dots$thin %||% 1
  chains <- dots$chains %||% 4
  checkmate::assert_number(chains)
  # A request for the program without a posterior has its own
  # spelling, `run_model = FALSE`. A `chains = 0` request would take a
  # brms branch for parameter-free models. That branch samples one
  # chain of two draws and blanks the simulation slot, leaving a
  # posterior without computable diagnostics.
  if (chains < 1) {
    stop(insight::format_error(c(
      "'chains' must be at least 1.",
      x = paste0("Got 'chains' = ", chains, "."),
      i = "Use 'run_model = FALSE' to build the model without sampling."
    )), call. = FALSE)
  }
  cores <- dots$cores %||% default_cores(chains)
  threads <- dots$threads %||% NULL
  opencl <- dots$opencl %||% NULL
  init <- dots$init %||% "random"
  # The working variables each brms model is built from, which the
  # sampler is told to leave out of the posterior. `save_pars()` says
  # which of them a user keeps.
  exclude <- mvgam_excluded_pars(
    obs_model = stan_components$obs_setup$brmsfit,
    trend_model = stan_components$trend_setup$brmsfit,
    standata = stan_components$combined_components$standata,
    save_pars = save_pars
  )
  seed <- dots$seed %||% sample.int(.Machine$integer.max, 1)
  control <- lift_sampler_control(dots, dots$control %||% NULL)
  silent <- dots$silent %||% 1
  future <- dots$future %||% FALSE
  # cmdstanr compile-time passthroughs. cpp_options accepts
  # entries like `stan_threads = TRUE` (auto-set when `threads`
  # is non-NULL), `CXXFLAGS = "-march=native"` for native-CPU
  # tuning, or any other cmdstan-supported C++ flag.
  # stanc_options accepts the stanc3 optimisation level
  # (e.g. `"O1"`) and other stanc-level flags. Both forward
  # directly to `cmdstanr::cmdstan_model()`.
  cpp_options <- dots$cpp_options %||% NULL
  stanc_options <- dots$stanc_options %||% NULL

  silent <- validate_silent(silent)
  threads <- validate_threads(threads)
  opencl <- validate_opencl(opencl)
  algorithm <- validate_algorithm(algorithm, backend)
  init <- validate_init(init, backend)

  validated_code <- parse_model(
    model = stan_components$combined_components$stancode,
    backend = backend,
    silent = silent
  )

  compile_args <- list(
    model = validated_code,
    backend = backend,
    threads = threads,
    opencl = opencl,
    silent = silent
  )
  if (!is.null(cpp_options)) compile_args$cpp_options <- cpp_options
  if (!is.null(stanc_options)) compile_args$stanc_options <- stanc_options
  compiled_model <- do.call(compile_model, compile_args)

  # Every other named argument goes to the sampler, as `...` does in
  # `brms::brm()`: `refresh`, `save_warmup`, `output_dir` and the rest.
  consumed <- c(names(formals(build_stan_components)),
                mvgam_fit_settings, mvgam_nuts_control_args)
  sampler_dots <- dots[!names(dots) %in% consumed]

  combined_fit <- do.call(fit_model, c(
    list(
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
      init = if (is_equal(init, "random")) {
        sparse_ar_inits(
          stan_components$combined_components$stancode,
          stan_components$combined_components$standata, chains, seed
        ) %||% init
      } else {
        init
      },
      exclude = exclude,
      seed = seed,
      control = control,
      silent = silent,
      future = future
    ),
    sampler_dots
  ))

  check_chains_finished(combined_fit, chains, algorithm)

  # Backend information for later use. `init` is kept as the user wrote
  # it. Stan records only the resolved value, which for list-valued and
  # Pathfinder starts is a session file path that cannot be replayed
  # on another machine.
  attr(combined_fit, "backend") <- backend
  attr(combined_fit, "init") <- init
  attr(combined_fit, "algorithm") <- algorithm
  attr(combined_fit, "mvgam_version") <- utils::packageVersion("mvgam")
  attr(combined_fit, "fit_time") <- Sys.time()

  # Kernel-relevant extras for trend_metadata (ar_lags, ma_lags,
  # max_lag, has_cor, n_lv, trend_type). Recorded at fit time, the
  # forecasting code need not derive them again on every per-draw
  # call.
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
    # The user's full `prior` / `priors` argument, aliased to `prior`
    # by `normalise_prior_arg_alias()`. With it the stored prior table
    # marks mvgam-managed trend overrides (sigma_trend, ar1_trend,
    # etc.) as `source = "user"`. The brms-side path marks only the
    # rows of the brms model.
    user_prior = dots$prior,
    newdata = newdata,
    save_pars = save_pars,
    silent = silent
  )

  return(mvgam_object)
}


# ==============================================================================
# mvgam object creation from the combined fit
# ==============================================================================

#' Create an mvgam object from the combined Stan fit
#'
#' @param combined_fit Stan fit object from combined model
#' @param obs_setup Observation model setup components (must include brmsfit)
#' @param trend_setup Trend model setup components (must include brmsfit)
#' @param mv_spec Multivariate trend specification
#' @param trend_metadata Trend metadata for prediction
#' @param data_name Optional name for the dataset
#' @param combined_stancode The assembled Stan code, or `NULL`
#' @param combined_standata The assembled Stan data, or `NULL`
#' @param user_trend_formula The trend formula as the user wrote it
#' @param user_prior The user's `prior` argument, or `NULL`
#' @param newdata Held-out data frame to store on the fit, or `NULL`
#' @param save_pars A [brms::save_pars()] object
#' @param silent The verbosity the fit ran under
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
                                          newdata = NULL,
                                          save_pars = brms::save_pars(),
                                          silent = 1L) {
  checkmate::assert_class(combined_fit, "stanfit")
  checkmate::assert_list(obs_setup, names = "named")
  checkmate::assert_list(trend_setup, names = "named", null.ok = TRUE)
  checkmate::assert_list(mv_spec, names = "named", null.ok = TRUE)
  checkmate::assert_list(trend_metadata, names = "named", null.ok = TRUE)
  checkmate::assert_character(combined_stancode, null.ok = TRUE)
  checkmate::assert_list(combined_standata, null.ok = TRUE)
  checkmate::assert_class(save_pars, "save_pars")
  checkmate::assert_number(silent)

  # Predictions evaluate each side through its mock brmsfit
  if (!"brmsfit" %in% names(obs_setup)) {
    stop_missing_fields("The observation brms setup", "brmsfit")
  }
  if (!is.null(trend_setup) && !"brmsfit" %in% names(trend_setup)) {
    stop_missing_fields("The trend brms setup", "brmsfit")
  }

  # `mvgam_single()` records the settings the fit ran under on the
  # stanfit. A stanfit carries no algorithm of its own: neither rstan
  # nor brms's cmdstanr conversion writes one to `@sim`.
  backend <- attr(combined_fit, "backend")
  algorithm <- attr(combined_fit, "algorithm")
  checkmate::assert_choice(backend, backend_choices(),
                           .var.name = "attr(combined_fit, 'backend')")
  checkmate::assert_choice(algorithm, algorithm_choices(),
                           .var.name = "attr(combined_fit, 'algorithm')")

  mvgam_object <- structure(
    list(
      fit = combined_fit,
      formula = obs_setup$formula,
      trend_formula = if (!is.null(trend_setup)) trend_setup$formula else NULL,
      # The user-supplied trend_formula, with its trend constructors
      # such as AR(p = 1) intact. update.mvgam takes it verbatim and
      # need not rebuild the constructor call from
      # `mv_spec$trend_specs`.
      trend_call = user_trend_formula,
      family = obs_setup$family,
      # The obs and trend prior tables are combined before the stanvar
      # rows are added. The fit then stores every user-supplied row.
      # The trend setup has brms's own class names (e.g. `sigma`, `b`,
      # `sds`, `ar1`). They are re-suffixed to the `<class>_trend`
      # convention of `get_prior.mvgam_formula()`. The mvgam-managed
      # user trend priors are then layered back on.
      # `remove_trend_suffix_from_priors()` strips them from the brms
      # pipeline. Stan applies them through the mvgam stanvars.
      prior = assemble_stored_prior_table(
        obs_priors = obs_setup$prior,
        trend_priors = trend_setup$prior,
        user_prior = user_prior,
        combined_stancode = combined_stancode %||% obs_setup$stancode,
        trend_stancode = trend_setup$stancode
      ),
      # The frame the user supplied. `mvgam()` adds a placeholder
      # column when the observation formula has no terms. brms received
      # that column. It is removed here. `fit$data` then holds the
      # user's own frame. `ensure_obs_placeholder()` restores the
      # column wherever the frame reaches brms.
      data = drop_obs_placeholder(obs_setup$data),
      test_data = newdata,
      data.name = data_name,
      # The brms code-generation options the fit was built under. A
      # refit then rebuilds the same program. Everything else in this
      # object records what the generator produced; this records what
      # it was asked for.
      codegen = obs_setup$codegen,
      # The verbosity the fit ran under. `update()` is then as quiet as
      # the call it rebuilds.
      silent = silent,
      stancode = combined_stancode %||% obs_setup$stancode,
      standata = combined_standata %||% obs_setup$standata,
      save_pars = save_pars,
      mv_spec = mv_spec,
      trend_metadata = trend_metadata,
      # Store lightweight brmsfit objects for prediction workflows
      obs_model = obs_setup$brmsfit,
      trend_model = if (!is.null(trend_setup)) trend_setup$brmsfit else NULL,
      backend = backend,
      init = attr(combined_fit, "init"),
      algorithm = algorithm,
      brms_version = utils::packageVersion("brms"),
      mvgam_version = utils::packageVersion("mvgam"),
      stan_version = live_stan_version(backend),
      creation_time = Sys.time()
    ),
    class = c("mvgam", "brmsfit")
  )

  mvgam_object <- prune_stored_formula_envs(mvgam_object)

  mvgam_object$criteria <- list()
  # The user-facing entry point stamps `$call`. That frame holds the
  # symbols the user typed. A `do.call()` frame here would hold the
  # function object and the resolved argument values.

  # Factors with their own coefficients are relabelled to one
  # ordering and orientation. See `R/factor_alignment.R`.
  mvgam_object <- relabel_factors(mvgam_object)

  return(mvgam_object)
}


#' A formula with its environment reduced to the names it uses
#'
#' A formula records the environment it was written in. `saveRDS`
#' serialises an unnamed environment by value. A model fitted inside a
#' function would then write that function's locals into the file:
#' 15.23 MB against 0.60 MB for the same model written at top level.
#' The names the expression uses are copied into a fresh environment
#' whose parent is the first named ancestor. A named environment is
#' serialised by reference and is returned unchanged.
#'
#' @param f A formula, or any other object to return unchanged
#' @return `f` with its environment pruned
#' @noRd
prune_formula_env <- function(f) {
  if (!inherits(f, "formula")) {
    return(f)
  }
  env <- environment(f)
  if (is.null(env) || nzchar(environmentName(env))) {
    return(f)
  }
  anchor <- env
  while (!identical(anchor, emptyenv()) &&
           !nzchar(environmentName(anchor))) {
    anchor <- parent.env(anchor)
  }
  if (identical(anchor, emptyenv())) {
    anchor <- globalenv()
  }
  # `all.names()` covers a function the user defined and named in the
  # formula, which is reached the same way a variable is.
  used <- unique(all.names(f))
  keep <- Filter(
    function(nm) {
      exists(nm, envir = env, inherits = TRUE) &&
        !exists(nm, envir = anchor, inherits = TRUE)
    },
    used
  )
  bindings <- lapply(keep, function(nm) get(nm, envir = env))
  names(bindings) <- keep
  environment(f) <- list2env(bindings, parent = anchor)
  f
}


#' Prune the environment of every formula a fit stores
#'
#' Nine formulas are reachable on a fitted object, each keeping its own
#' environment: the three named slots, four within `mv_spec` and the
#' main formula of each stored brmsfit. One unpruned reference would
#' write the calling frame into the file. `utils::object.size()` does
#' not follow an environment and reports none of this. The size of the
#' saved file shows it.
#'
#' @param x Object to walk
#' @param depth Integer recursion depth
#' @return `x` with every reachable formula pruned
#' @noRd
prune_stored_formula_envs <- function(x, depth = 0L) {
  checkmate::assert_int(depth, lower = 0L)
  if (inherits(x, "formula")) {
    return(prune_formula_env(x))
  }
  if (depth > 6L) {
    return(x)
  }
  # A model frame keeps its `terms` attribute. A terms object records
  # the environment the formula was evaluated in. brms builds those
  # frames while the model is set up, giving each one a reference to
  # the caller that no formula slot shares.
  at <- attributes(x)
  for (nm in names(at)) {
    if (inherits(at[[nm]], "formula")) {
      attr(x, nm) <- prune_formula_env(at[[nm]])
    }
  }
  if (is.data.frame(x) || !is.list(x) || !length(x)) {
    return(x)
  }
  attrs <- attributes(x)
  y <- unclass(x)
  for (i in seq_along(y)) {
    # Assigning NULL back into a list deletes the element. A slot a fit
    # leaves empty has to survive the walk.
    if (is.null(y[[i]])) {
      next
    }
    y[[i]] <- prune_stored_formula_envs(y[[i]], depth + 1L)
  }
  attributes(y) <- attrs
  y
}


#' An unfitted mvgam stub from generated Stan components
#'
#' Serves the `run_model = FALSE` path. The stub is an `mvgam` with
#' `stancode`, `standata`, `data`, `trend_metadata` and the other slots
#' populated. Its `fit` is `NULL`. Every method dispatches to the
#' `mvgam` one. `print()`, `stancode()`, `standata()` and `methods_md()`
#' work on it. The `mvgam_prefit` class records that no sampling
#' happened.
#'
#' @param stan_components List from
#'   `generate_stan_components_mvgam_formula()`
#' @param formula Observation formula
#' @param trend_formula Trend formula as the user wrote it
#' @param family Family specification
#' @param data Training data frame
#' @param data_name Name of the data in the user's call, or `NULL`
#' @param newdata Held-out data frame, or `NULL`
#' @param backend Stan backend
#' @param save_pars A [brms::save_pars()] object
#' @param user_prior The user's `prior` argument, or `NULL`
#' @return An object of class `c("mvgam", "mvgam_prefit")`
#' @noRd
create_mvgam_stub_from_stan_components <- function(
  stan_components,
  formula,
  trend_formula,
  family,
  data,
  data_name,
  newdata,
  backend,
  save_pars = brms::save_pars(),
  user_prior = NULL
) {
  checkmate::assert_list(stan_components)
  checkmate::assert_data_frame(data)
  checkmate::assert_string(backend)
  checkmate::assert_class(save_pars, "save_pars")
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
      # The same table a fitted object stores, built by the same
      # assembler. The stub describes the model a fit would produce. A
      # prior table missing the trend rows, the user's overrides or the
      # placeholder filter would describe a different model from
      # the one `run_model = TRUE` produces.
      prior = assemble_stored_prior_table(
        obs_priors = obs_setup$prior,
        trend_priors = trend_setup$prior,
        user_prior = user_prior,
        combined_stancode = stan_components$combined_components$stancode,
        trend_stancode = trend_setup$stancode
      ),
      data = drop_obs_placeholder(obs_setup$data %||% data),
      test_data = newdata,
      data.name = data_name,
      stancode = stan_components$combined_components$stancode,
      standata = stan_components$combined_components$standata,
      save_pars = save_pars,
      mv_spec = mv_spec,
      trend_metadata = enriched_trend_metadata,
      obs_model = obs_setup$brmsfit,
      trend_model = if (!is.null(trend_setup)) trend_setup$brmsfit else NULL,
      backend = backend,
      algorithm = "none",
      brms_version = utils::packageVersion("brms"),
      mvgam_version = utils::packageVersion("mvgam"),
      stan_version = live_stan_version(backend),
      creation_time = Sys.time(),
      criteria = list()
    ),
    class = c("mvgam", "mvgam_prefit")
  )
  prune_stored_formula_envs(mvgam_object)
}


# ==============================================================================
# Multiple imputation
# ==============================================================================
# Fits the model to each imputed dataset and pools the draws.

#' Fit mvgam models to multiply imputed datasets
#'
#' @description
#' Fits Bayesian state-space models to multiply imputed datasets,
#' combining posteriors across imputations to carry the imputation
#' uncertainty into the estimates.
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
#' Uses `rstan::sflist2stanfit()`, as `brms::brm_multiple()` does, to
#' concatenate draws from all imputations at the Stan level. The pooled
#' draws then hold both the between-imputation and within-imputation
#' variance.
#'
#' **MI Diagnostics**:
#' Pooled objects include convergence diagnostics per imputation
#' (Rhat, ESS) accessible via `summary()`. The print method
#' displays MI diagnostics: total draws, draws per imputation and
#' convergence summaries.
#'
#' @param formula Model formula (observation model). Supports brms
#'   formula syntax including smooths and random effects.
#' @param trend_formula Optional trend formula specifying
#'   state-space dynamics. One trend formula applies to every response
#'   of a multivariate model.
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
#'   every imputation fit. Later plot and forecast methods can reach
#'   it through any one of them. Defaults to `NULL`.
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
#' If `run_model=FALSE`: a single unfitted stub built from the first
#' dataset, whatever `combine` is set to. Pooling needs draws, which a
#' stub does not have.
#'
#' @examples
#' \dontrun{
#' # A covariate with ten missing values, filled three ways. An
#' # imputation model would draw the fills. This example draws them
#' # from a normal distribution.
#' set.seed(1)
#' dat <- sim_mvgam(family = gaussian(), n_series = 1L,
#'                  n_timepoints = 60L, trend_model = AR())$data_train
#' missing_x <- sample(nrow(dat), 10L)
#' dat$x[missing_x] <- NA
#' imputed_list <- lapply(1:3, function(i) {
#'   filled <- dat
#'   filled$x[missing_x] <- rnorm(10L, mean(dat$x, na.rm = TRUE),
#'                                sd(dat$x, na.rm = TRUE))
#'   filled
#' })
#'
#' # One fit per imputation, with the posteriors pooled
#' fit_pooled <- mvgam_multiple(
#'   formula = y ~ x,
#'   trend_formula = ~ AR(p = 1),
#'   data_list = imputed_list,
#'   family = gaussian(),
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # The pooled summary reports how much of each parameter's variance
#' # comes from the imputations
#' summary(fit_pooled)
#'
#' # `combine = FALSE` keeps the fits separate
#' fit_list <- mvgam_multiple(
#'   formula = y ~ x,
#'   trend_formula = ~ AR(p = 1),
#'   data_list = imputed_list,
#'   family = gaussian(),
#'   combine = FALSE,
#'   chains = 2,
#'   silent = 2
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
  checkmate::assert_list(data_list, min.len = 2)
  checkmate::assert_string(backend)
  checkmate::assert_flag(combine)
  checkmate::assert_flag(check_data)
  checkmate::assert_data_frame(newdata, null.ok = TRUE)

  # This exported function reaches a fit without passing through
  # `mvgam()`. It raises the notice itself. `family` arrives through
  # `...`. Raising it once covers the whole list. The per-imputation
  # fit would repeat it per dataset.
  warn_positive_mean_link_with_trend(formula, list(...)$family,
                                     trend_formula)

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

  checkmate::assert_list(data_list, types = "data.frame", min.len = 2)

  # Validate multiple imputation datasets
  if (check_data) {
    validate_multiple_imputation_datasets(data_list)
  }

  # A request for the program without a posterior is met from the
  # first dataset, before any imputation is fitted. Pooling needs
  # draws. A stub has none. `brms::brm_multiple()` returns one
  # unfitted model the same way under its `chains = 0` spelling.
  if (isFALSE(list(...)$run_model)) {
    return(mvgam_single(
      formula = formula,
      trend_formula = trend_formula,
      data = data_list[[1L]],
      backend = backend,
      newdata = newdata,
      ...
    ))
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
# Dataset validation
# ------------------------------------------------------------------------------
# Checks that the imputed datasets share one structure.

#' Validate multiply imputed datasets
#' @param data_list List of imputed datasets
#' @return Invisible TRUE if valid, stops with error if invalid
#' @noRd
validate_multiple_imputation_datasets <- function(data_list) {
  checkmate::assert_list(data_list, types = "data.frame", min.len = 2)

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
      )), call. = FALSE)
    }

    # Check number of rows match
    if (nrow(current_data) != ref_nrow) {
      stop(insight::format_error(c(
        paste("Dataset", i, "has", nrow(current_data), "rows, expected",
             ref_nrow),
        x = "All imputed datasets must have same number of observations."
      )), call. = FALSE)
    }
  }
  # `pool_mvgam_fits()` checks that the imputations share a time and
  # series axis. The model resolves the columns naming the axis. They
  # need not be called `time` or `series`.

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Fitting each imputed dataset
# ------------------------------------------------------------------------------
# Fits the same model specification to each imputed dataset.

#' Fit models to multiply imputed datasets
#' @param formula Main formula
#' @param trend_formula Trend formula
#' @param data_list List of datasets
#' @param backend Stan backend
#' @param ... Additional arguments
#' @return List of fitted mvgam objects
#' @noRd
fit_multiple_imputation_models <- function(formula, trend_formula, data_list,
                                          backend, ...) {
  checkmate::assert_list(data_list, types = "data.frame", min.len = 1L)
  checkmate::assert_string(backend)
  n_datasets <- length(data_list)

  # Fit individual models
  fits <- vector("list", n_datasets)
  names(fits) <- paste0("imputation_", seq_len(n_datasets))

  # `brms::brm_multiple()` reports each imputed fit as it starts,
  # under the same `silent` setting.
  silent <- list(...)$silent %||% 1L
  for (i in seq_len(n_datasets)) {
    if (silent < 2) {
      rlang::inform(
        paste0("Fitting imputed model ", i, " out of ", n_datasets)
      )
    }

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
# Posterior combination
# ------------------------------------------------------------------------------
# Combines the draws of the imputed fits at the Stan level with
# `rstan::sflist2stanfit()`, as brms does.

#' Pool mvgam fits by Stan-level combination
#' @param fits List of individual mvgam fits
#' @return mvgam_pooled object with combined posteriors
#' @noRd
pool_mvgam_fits <- function(fits) {
  checkmate::assert_list(fits, types = "mvgam", min.len = 2)

  # Pooled draws of `trend[t, s]` describe one cell only when every
  # imputation put the same series and the same occasion there. An
  # imputation that moved a time or a series value is refused.
  ref_axes <- mvgam_axes(fits[[1L]])
  for (i in seq_along(fits)[-1L]) {
    if (!identical(ref_axes, mvgam_axes(fits[[i]]))) {
      stop(insight::format_error(c(
        paste0("Imputation ", i, " has a different time or series axis ",
               "from imputation 1."),
        x = paste0("Pooled trend draws would mix different occasions or ",
                   "series under one label."),
        i = "Impute only the response and covariates."
      )), call. = FALSE)
    }
  }

  # Validate parameter consistency across fits
  ref_vars <- sort(variables(fits[[1]]))
  for (i in seq_along(fits)[-1]) {
    current_vars <- sort(variables(fits[[i]]))
    if (!identical(ref_vars, current_vars)) {
      stop(insight::format_error(c(
        sprintf("Model 1 and %d have different parameters.", i),
        x = paste0("Parameters in one fit alone: ",
                   paste(utils::head(c(setdiff(ref_vars, current_vars),
                                       setdiff(current_vars, ref_vars)),
                                     5L), collapse = ", "), "."),
        i = "Fit the same model to every imputed dataset."
      )), call. = FALSE)
    }
  }

  n_imp <- length(fits)

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
  combined_fit
}

