#' Simulate `mvgam`-shaped time-series data with known truth
#'
#' Generates training (and optional testing) data under a fixed
#' catalog of observation-side covariate recipes (the `type`
#' argument) crossed with any of the supported trend dynamics
#' (the `trend_model` argument). Inspired by [mgcv::gamSim()]'s
#' fixed-recipe pattern but extended to cover mvgam's state-space
#' grammar (smooths + GPs + random effects + AR/RW/CAR/VAR
#' dynamics).
#'
#' @param type Integer in `1:7` selecting the observation-side
#'   recipe. See *Details* for the catalog.
#' @param family A `family` or `brmsfamily` object specifying the
#'   observation likelihood. Supported: `gaussian()`, `student()`,
#'   `poisson()`, `negbinomial()`, `binomial()`, `Beta()`,
#'   `Gamma()`, [`tweedie()`], and [`com_binomial()`] (and the
#'   matching `brms::brmsfamily` equivalents). Defaults to
#'   `gaussian()`. For `com_binomial()` pass the dispersion `nu`
#'   and the per-row `trials` count via `family_pars` (e.g.
#'   `family_pars = list(nu = 0.5, trials = 30L)`).
#' @param n_series Integer; number of time series to simulate.
#'   Defaults to `1L`.
#' @param n_timepoints Integer; number of timepoints per series.
#'   Defaults to `80L`.
#' @param trend_model An optional `mvgam_trend` constructor (e.g.
#'   `AR(p = 1)`, `VAR(p = 2, cor = TRUE)`, `CAR()`, `ZMVN()`).
#'   `NULL` (the default) selects the type's preferred trend (see
#'   *Details*), and `"None"` simulates no trend. Sparse-lag specs
#'   like `AR(p = c(1, 3, 12))` are supported natively.
#' @param prop_trend Numeric in `[0, 1]` controlling the fraction
#'   of total link-scale variance contributed by the latent
#'   trend. `NULL` uses a type-specific default
#'   (0.2 for type 5 / state-space, 0.5 elsewhere).
#' @param proportional_train Numeric in `(0, 1]`; the fraction of
#'   timepoints retained in `data_train`. Remainder goes to
#'   `data_test`. Defaults to `0.75`.
#' @param prop_missing Numeric in `[0, 1)`; fraction of training
#'   observations to mark as `NA`. Defaults to `0`.
#' @param family_pars Optional named list of additional family
#'   parameters (`sigma`, `nu`, `size`, `trials`, `phi`,
#'   `shape`, `power`). Type-specific defaults are used when an
#'   entry is absent. `com_binomial()` reads `nu` (dispersion)
#'   and `trials`.
#' @param seed Optional integer seed for reproducibility.
#'
#' @details
#' The seven recipes:
#'
#' \describe{
#'   \item{`type = 1`}{`y ~ s(x)` with `RW()` trend.
#'     Single smooth covariate + random-walk trend; the simplest
#'     mvgam recipe.}
#'   \item{`type = 2`}{`y ~ s(x) + s(z)` with an `AR(p = 1)` trend
#'     whose coefficient is 0.7. Two additive smooths.}
#'   \item{`type = 3`}{`y ~ s(x) + (1 | grp)` with an `AR(p = 1)`
#'     trend whose coefficient is 0.7. Hierarchical: random
#'     intercept per group level (5 groups by default).}
#'   \item{`type = 4`}{`y ~ gp(x)` with an `AR(p = 1)` trend whose
#'     coefficient is 0.7. Gaussian-process covariate.}
#'   \item{`type = 5`}{State-space: obs `y ~ x`, trend formula
#'     `~ s(z)`, and an `AR(p = 1)` trend whose coefficient is 0.6.
#'     The smooth enters the latent state. `prop_trend` defaults
#'     to `0.2` for this type, which keeps the AR component small
#'     relative to the smooth and makes the two identifiable.}
#'   \item{`type = 6`}{`y ~ s(season, bs = "cc")` with `CAR(time,
#'     series)`. Cyclic seasonal + continuous-time AR(1) on
#'     irregular time gaps (`Delta t ~ Uniform(1, 6)`), with
#'     autocorrelation 0.7 over the median gap.}
#'   \item{`type = 7`}{`y ~ s(x)` with sparse
#'     `AR(p = c(1, 12))`. The latent state carries lag-1 momentum
#'     and lag-12 recurrence, with coefficients 0.55 and 0.40. The
#'     observation side holds a smooth of a covariate that is not
#'     periodic, which makes it identifiable from the latent state.
#'     `prop_trend` defaults to `0.6`.}
#' }
#'
#' For multi-series simulations (`n_series > 1`), the observation
#' covariates `x`, `z`, etc. are independently drawn per
#' series-time cell; the trend dynamics follow `trend_model`
#' (independent per series unless the constructor specifies
#' `cor = TRUE` or `VAR()`).
#'
#' @return A list of class `"mvgam_sim"` with elements:
#'   \describe{
#'     \item{`data_train`}{`data.frame` in long format: columns
#'       include `y`, `series`, `time`, and the type-specific
#'       covariates (`x`, `z`, `grp`, or `season`).}
#'     \item{`data_test`}{`data.frame` of held-out timepoints, or
#'       `NULL` when `proportional_train = 1`.}
#'     \item{`family`}{The `brmsfamily` used.}
#'     \item{`trend_model`}{The `mvgam_trend` used.}
#'     \item{`type`}{The catalog entry.}
#'     \item{`true_betas`}{Named numeric vector of population-
#'       level coefficients.}
#'     \item{`true_smooths`}{Named list of `data.frame(grid,
#'       f_true)` ground-truth smooth functions.}
#'     \item{`true_trend`}{Matrix `[n_timepoints, n_series]` of
#'       latent trend realisations.}
#'     \item{`true_trend_sigma`}{Empirical SD of `true_trend`.}
#'     \item{`true_sigma_obs`}{Observation noise SD (or
#'       family-specific dispersion).}
#'   }
#'
#' @author Nicholas J Clark
#'
#' @seealso [mgcv::gamSim()] for the conceptual inspiration;
#'   [`mvgam()`] for the fitter the simulated data can be fed to.
#'   For detection-error families ([occ()] / [nmix()] variants),
#'   use [sim_closure_unit_data()] instead: the closure-unit
#'   grain (sites x visits) does not fit the time-series layout
#'   `sim_mvgam()` produces. For multi-species site-by-species
#'   data with correlated species, use [sim_jsdm()], which draws
#'   one row per species at each site.
#'
#' @examples
#' # Three Poisson series with an AR(1) latent trend.
#' set.seed(0)
#' simdat <- sim_mvgam(
#'   family       = poisson(),
#'   n_series     = 3L,
#'   n_timepoints = 120L,
#'   trend_model  = AR()
#' )
#' head(simdat$data_train)
#' summary(simdat)
#'
#' @export
sim_mvgam <- function(type = 1L,
                       family = gaussian(),
                       n_series = 1L,
                       n_timepoints = 80L,
                       trend_model = NULL,
                       prop_trend = NULL,
                       proportional_train = 0.75,
                       prop_missing = 0,
                       family_pars = list(),
                       seed = NULL) {
  checkmate::assert_int(type, lower = 1L, upper = 7L)
  checkmate::assert_int(n_series, lower = 1L)
  checkmate::assert_int(n_timepoints, lower = 5L)
  checkmate::assert_number(
    proportional_train, lower = 0.1, upper = 1
  )
  checkmate::assert_number(prop_missing, lower = 0, upper = 0.5)
  checkmate::assert_list(family_pars)
  checkmate::assert(
    checkmate::check_null(trend_model),
    checkmate::check_class(trend_model, "mvgam_trend"),
    checkmate::check_choice(trend_model, "None"),
    .var.name = "trend_model"
  )
  family <- validate_family(family)
  fam_name <- resolve_family_name(family)

  local_seed(seed)

  spec <- sim_type_spec(type)
  if (is.null(trend_model)) trend_model <- spec$default_trend
  if (is.null(prop_trend)) prop_trend <- spec$default_prop_trend

  total_n <- n_timepoints * n_series
  series_fac <- factor(
    rep(paste0("series_", seq_len(n_series)),
        each = n_timepoints),
    levels = paste0("series_", seq_len(n_series))
  )
  time_int <- rep(seq_len(n_timepoints), times = n_series)

  # Per-type obs-side covariate construction + smooth/RE truth.
  built <- spec$build_data(
    n_timepoints = n_timepoints, n_series = n_series,
    series_fac = series_fac, time_int = time_int
  )

  # Bound total link-scale variance per family to keep response-
  # scale values in a recoverable range (e.g. exp(eta) shouldn't
  # span 10 orders of magnitude for Poisson; logit(eta) shouldn't
  # saturate to 0/1 for Binomial).
  total_link_sd <- link_scale_budget(fam_name)
  target_trend_sd <- total_link_sd * sqrt(prop_trend)
  target_obs_sd <- total_link_sd * sqrt(1 - prop_trend)

  # Trend propagation. Every kernel draws with
  # `sigma = trend_sigma(prop_trend)` and is rescaled after
  # propagation so the empirical SD matches the target. The
  # rescale is a single scalar multiply, so it moves amplitude
  # and leaves shape alone: a random walk keeps its
  # linearly growing variance, an autoregression keeps its
  # autocorrelation, and a piecewise trend keeps its
  # changepoints. What it does not leave alone is the one thing
  # `prop_trend` names, which is how much of the link-scale
  # variance the trend accounts for.
  trend_args <- spec$trend_params(
    n_series = n_series, n_timepoints = n_timepoints,
    prop_trend = prop_trend
  )
  # Fill in trend-model-specific param defaults that the
  # generic per-type spec functions don't know about (VAR's
  # transition matrix A and innovation covariance Sigma;
  # cor = TRUE's off-diagonal Sigma). Keeps the per-type
  # spec functions trend-agnostic while letting users pass
  # arbitrary trend constructors via `trend_model = ...`.
  trend_args$params <- fill_multivariate_trend_defaults(
    trend_model, trend_args$params, n_series
  )
  # A type that builds its own irregular time grid supplies the
  # gaps the trend must be propagated over, so the latent process
  # and the recorded time column describe the same spacing.
  if (!is.null(built$trend_time)) {
    trend_args$time <- built$trend_time
  }
  trend_mat <- if (is_pw_trend(trend_model)) {
    # A piecewise trend is deterministic given (k, m, delta,
    # t_change), evaluated at the training times. `propagate_pw()`
    # samples changepoints over a forecast horizon, which a
    # simulation has none of.
    trend_args$params <- fill_pw_trend_defaults(
      trend_model, trend_args$params, n_series, n_timepoints
    )
    pw_trendC(
      t = as.numeric(seq_len(n_timepoints)),
      k = as.numeric(trend_args$params$k),
      m = as.numeric(trend_args$params$m),
      delta = trend_args$params$delta,
      t_change = as.numeric(trend_args$params$t_change),
      cap = if (pw_is_logistic(trend_model)) {
        trend_args$params$cap
      } else {
        matrix(0, 0L, 0L)
      },
      growth_type = pw_growth(trend_model)
    )
  } else {
    propagate_trend(
      trend_model = trend_model,
      params = trend_args$params,
      h = n_timepoints,
      n_series = n_series,
      time = trend_args$time
    )
  }

  # Centre the trend and obs contributions on zero before
  # combining so the mean of eta is the intercept (not
  # intercept + drift of whatever the smooth / trend realisation
  # happened to deposit). Without this, log-link families
  # anchor at exp(intercept + drift) and logit families
  # saturate.
  trend_vec <- as.numeric(trend_mat) - mean(as.numeric(trend_mat))
  trend_mat <- matrix(
    trend_vec, nrow = nrow(trend_mat), ncol = ncol(trend_mat)
  )
  obs_centered <- built$obs_contrib - mean(built$obs_contrib)

  # Rescale every trend to the target empirical SD, whatever
  # kernel produced it. A scalar multiply changes magnitude and
  # nothing else, so a random walk keeps its growing variance
  # envelope, an autoregression keeps its autocorrelation and a
  # piecewise trend keeps its changepoints; only the amplitude
  # moves, which is what `prop_trend` names.
  #
  # Choosing `sigma_innov` up front instead cannot do this. It
  # requires a closed-form stationary variance, which a random
  # walk does not have, which the sparse-lag `AR(p = c(1, 12))`
  # does not have either, and which a deterministic piecewise
  # trend has no analogue of at all. Each was left carrying
  # whatever variance its own
  # parameters implied: at `prop_trend = 0.5`, a three-series
  # random walk delivered 3.9 and a one-series piecewise trend
  # delivered 0.04.
  trend_scale <- sd_rescale_factor(trend_vec, target_trend_sd)
  trend_mat <- trend_mat * trend_scale
  obs_scale <- sd_rescale_factor(obs_centered, target_obs_sd)
  obs_contrib <- obs_centered * obs_scale
  # Apply the obs scale to every recorded ground-truth smooth so
  # the stored truth tracks the same amplitude the data was
  # generated under. Smooths are also centered (subtract their
  # mean over the grid) for the same reason.
  built$true_smooths <- lapply(built$true_smooths, function(df) {
    df$f_true <- (df$f_true - mean(df$f_true)) * obs_scale
    df
  })
  built$true_betas <- built$true_betas * obs_scale

  # Combine linear predictor components.
  intercept <- spec$intercept(fam_name)
  eta <- intercept + obs_contrib + as.numeric(trend_mat)

  # Family-aware response sampling. sigma_obs default depends on
  # family and prop_trend.
  obs_pars <- sim_family_pars(family, family_pars, prop_trend,
                                stats::sd(eta))
  y <- sim_family_rng(eta, family, obs_pars)

  # Optional missing-data injection on training portion.
  if (prop_missing > 0) {
    n_miss <- floor(prop_missing * total_n)
    miss_idx <- sample.int(total_n, n_miss)
    y[miss_idx] <- NA_real_
  }

  data_long <- data.frame(
    y = y, series = series_fac, time = time_int
  )
  # Bind type-specific covariates.
  for (nm in names(built$covariates)) {
    data_long[[nm]] <- built$covariates[[nm]]
  }
  # Binomial and Conway-Maxwell-Binomial fits need a `trials`
  # column; sim_family_pars stored the trial count under
  # obs_pars$trials.
  if (fam_name %in% c("binomial", "com_binomial")) {
    data_long$trials <- obs_pars$trials %||% 10L
  }
  if (!is.null(built$time_long)) {
    # CAR uses irregular continuous time; overwrite integer time.
    data_long$time <- built$time_long
  }

  # Train / test split on time index, preserved across series.
  split <- split_train_test(
    data_long, n_timepoints, proportional_train
  )

  structure(
    list(
      data_train = split$train,
      data_test = split$test,
      family = family,
      trend_model = trend_model,
      type = type,
      true_betas = built$true_betas,
      true_smooths = built$true_smooths,
      true_trend = trend_mat,
      true_trend_sigma = stats::sd(as.numeric(trend_mat)),
      true_sigma_obs = obs_pars$sigma %||% obs_pars$phi %||%
        obs_pars$size %||% obs_pars$shape %||% NA_real_
    ),
    class = c("mvgam_sim", "list")
  )
}


#' Summary for `sim_mvgam()` output
#'
#' Returns a structured snapshot of a simulated dataset's design and
#' true generative parameters so that ground-truth recovery checks
#' downstream can be programmed against it without re-extracting
#' fields manually.
#'
#' @param object An `mvgam_sim` object returned by [sim_mvgam()].
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A list with class `mvgam_sim_summary` containing:
#'   \describe{
#'     \item{`type`}{The catalog type used.}
#'     \item{`family`}{Family name (string).}
#'     \item{`trend`}{Trend type label (string or `"None"`).}
#'     \item{`n_series`}{Number of series.}
#'     \item{`n_timepoints`}{Total number of timepoints.}
#'     \item{`n_train`}{Number of training rows.}
#'     \item{`n_test`}{Number of test rows.}
#'     \item{`true_betas`}{Named numeric vector of population
#'       coefficients.}
#'     \item{`n_smooths`}{Number of true smooth functions.}
#'     \item{`smooth_names`}{Character vector of true smooth names.}
#'     \item{`true_trend_sigma`}{Empirical SD of the latent trend
#'       across all series x time cells.}
#'     \item{`true_sigma_obs`}{Observation-family scale / dispersion
#'       parameter (NA if family is fully discrete).}
#'   }
#'
#' @method summary mvgam_sim
#' @export
summary.mvgam_sim <- function(object, ...) {
  checkmate::assert_class(object, "mvgam_sim")
  rlang::check_dots_empty()
  # `trend_model = "None"` simulates no trend and is stored as given.
  trend_label <- if (inherits(object$trend_model, "mvgam_trend")) {
    object$trend_model$trend
  } else {
    "None"
  }
  smooths <- object$true_smooths %||% list()
  structure(
    list(
      type = object$type,
      # Use resolve_family_name() so brms custom_family objects
      # (com_binomial / tweedie / diri / mvn / mvt) report their
      # user-visible name rather than the literal string "custom"
      # that brms stores on family$family.
      family = resolve_family_name(object$family),
      trend = trend_label,
      n_series = NCOL(object$true_trend),
      n_timepoints = NROW(object$true_trend),
      n_train = NROW(object$data_train),
      n_test = if (is.null(object$data_test)) 0L else NROW(object$data_test),
      true_betas = object$true_betas,
      n_smooths = length(smooths),
      smooth_names = names(smooths),
      true_trend_sigma = object$true_trend_sigma,
      true_sigma_obs = object$true_sigma_obs
    ),
    class = "mvgam_sim_summary"
  )
}


#' Print method for `mvgam_sim_summary`
#'
#' @param x A `mvgam_sim_summary` object.
#' @param digits Integer; significant digits for printed numbers.
#'   Default `3`.
#' @param ... Currently ignored.
#'
#' @return The `mvgam_sim_summary` object `x`, returned invisibly.
#'
#' @method print mvgam_sim_summary
#' @export
print.mvgam_sim_summary <- function(x, digits = 3L, ...) {
  checkmate::assert_class(x, "mvgam_sim_summary")
  checkmate::assert_int(digits, lower = 0L)
  cat("Simulated mvgam dataset (sim_mvgam type ", x$type, ")\n",
      sep = "")
  cat("  Family       : ", x$family, "\n", sep = "")
  cat("  Trend        : ", x$trend, "\n", sep = "")
  cat("  Series       : ", x$n_series, "\n", sep = "")
  cat("  Timepoints   : ", x$n_timepoints, "  (train = ",
      x$n_train, ", test = ", x$n_test, ")\n", sep = "")
  cat("\nTrue generative parameters\n")
  if (length(x$true_betas) > 0L) {
    cat("  Population coefficients:\n")
    for (nm in names(x$true_betas)) {
      cat("    ", nm, " = ",
          format(round(x$true_betas[[nm]], digits), nsmall = digits),
          "\n", sep = "")
    }
  } else {
    cat("  Population coefficients: (none)\n")
  }
  if (x$n_smooths > 0L) {
    cat("  Smooth functions (", x$n_smooths, "): ",
        paste(x$smooth_names, collapse = ", "), "\n", sep = "")
  }
  cat("  Latent trend SD: ",
      format(round(x$true_trend_sigma, digits), nsmall = digits),
      "\n", sep = "")
  cat("  Obs noise / dispersion: ",
      if (is.na(x$true_sigma_obs)) "NA" else
        format(round(x$true_sigma_obs, digits), nsmall = digits),
      "\n", sep = "")
  invisible(x)
}


#' Print method for `mvgam_sim`
#'
#' Delegates to `summary(x)` so the default print is informative.
#'
#' @param x A `mvgam_sim` object.
#' @param ... Passed to `print.mvgam_sim_summary()`.
#'
#' @return The `mvgam_sim` object `x`, returned invisibly.
#'
#' @method print mvgam_sim
#' @export
print.mvgam_sim <- function(x, ...) {
  print(summary(x), ...)
}


# ------------------------------------------------------------------
# Type catalog: per-type spec returns (build_data, trend_params,
# default_trend, default_prop_trend, intercept).
# ------------------------------------------------------------------
#'@noRd
sim_type_spec <- function(type) {
  switch(
    as.integer(type),
    `1` = spec_type_1(),
    `2` = spec_type_2(),
    `3` = spec_type_3(),
    `4` = spec_type_4(),
    `5` = spec_type_5(),
    `6` = spec_type_6(),
    `7` = spec_type_7()
  )
}


# Type 1: y ~ s(x), RW trend.
#'@noRd
spec_type_1 <- function() {
  list(
    default_trend = RW(),
    default_prop_trend = 0.5,
    intercept = function(fam) intercept_for_family(fam),
    build_data = function(n_timepoints, n_series, series_fac,
                           time_int) {
      total_n <- n_timepoints * n_series
      x <- stats::runif(total_n, -2, 2)
      sm <- sim_smooth(x, k = 8L, bs = "tp", scale = 0.6)
      grid <- seq(-2, 2, length.out = 100L)
      true_sm <- sim_smooth_on_grid(
        sm$basis, sm$coefs, grid_x = grid
      )
      list(
        covariates = list(x = x),
        obs_contrib = sm$f,
        true_betas = numeric(),
        true_smooths = list(
          `s(x)` = data.frame(x = grid, f_true = true_sm)
        )
      )
    },
    trend_params = function(n_series, n_timepoints, prop_trend) {
      list(params = list(sigma = trend_sigma(prop_trend)),
           time = NULL)
    }
  )
}


# Type 2: y ~ s(x) + s(z), AR(p = 1).
#'@noRd
spec_type_2 <- function() {
  list(
    default_trend = AR(p = 1),
    # AR identification at moderate n needs the trend to carry
    # most of the link-scale variance; otherwise the obs-side
    # smooths absorb the time-varying signal and the AR
    # coefficient posterior covers zero.
    default_prop_trend = 0.6,
    intercept = function(fam) intercept_for_family(fam),
    build_data = function(n_timepoints, n_series, series_fac,
                           time_int) {
      total_n <- n_timepoints * n_series
      x <- stats::runif(total_n, -2, 2)
      z <- stats::runif(total_n, -2, 2)
      sm_x <- sim_smooth(x, k = 8L, scale = 0.5)
      sm_z <- sim_smooth(z, k = 8L, scale = 0.5)
      grid <- seq(-2, 2, length.out = 100L)
      true_sm_x <- sim_smooth_on_grid(sm_x$basis, sm_x$coefs,
                                        grid_x = grid)
      true_sm_z <- sim_smooth_on_grid(sm_z$basis, sm_z$coefs,
                                        grid_x = grid)
      list(
        covariates = list(x = x, z = z),
        obs_contrib = sm_x$f + sm_z$f,
        true_betas = numeric(),
        true_smooths = list(
          `s(x)` = data.frame(x = grid, f_true = true_sm_x),
          `s(z)` = data.frame(z = grid, f_true = true_sm_z)
        )
      )
    },
    trend_params = function(n_series, n_timepoints, prop_trend) {
      list(
        params = list(ar = 0.7, sigma = trend_sigma(prop_trend)),
        time = NULL
      )
    }
  )
}


# Type 3: y ~ s(x) + (1 | grp), AR(p = 1).
#'@noRd
spec_type_3 <- function() {
  list(
    default_trend = AR(p = 1),
    # Same AR identifiability budget as type 2.
    default_prop_trend = 0.6,
    intercept = function(fam) intercept_for_family(fam),
    build_data = function(n_timepoints, n_series, series_fac,
                           time_int) {
      total_n <- n_timepoints * n_series
      x <- stats::runif(total_n, -2, 2)
      grp <- sim_grp(total_n, n_levels = 5L)
      sm_x <- sim_smooth(x, k = 8L, scale = 0.5)
      re <- sim_re(grp, sigma = 0.6)
      grid <- seq(-2, 2, length.out = 100L)
      true_sm_x <- sim_smooth_on_grid(sm_x$basis, sm_x$coefs,
                                        grid_x = grid)
      list(
        covariates = list(x = x, grp = grp),
        obs_contrib = sm_x$f + re$values,
        true_betas = re$coefs,
        true_smooths = list(
          `s(x)` = data.frame(x = grid, f_true = true_sm_x)
        )
      )
    },
    trend_params = function(n_series, n_timepoints, prop_trend) {
      list(
        params = list(ar = 0.7, sigma = trend_sigma(prop_trend)),
        time = NULL
      )
    }
  )
}


# Type 4: y ~ gp(x), AR(p = 1).
#'@noRd
spec_type_4 <- function() {
  list(
    default_trend = AR(p = 1),
    # Same AR identifiability budget as types 2 and 3.
    default_prop_trend = 0.6,
    intercept = function(fam) intercept_for_family(fam),
    build_data = function(n_timepoints, n_series, series_fac,
                           time_int) {
      total_n <- n_timepoints * n_series
      x <- stats::runif(total_n, -2, 2)
      gp_vals <- sim_gp_cov(x, alpha = 1, rho = 0.5)
      grid <- seq(-2, 2, length.out = 100L)
      true_gp <- sim_gp_cov(grid, alpha = 1, rho = 0.5)
      list(
        covariates = list(x = x),
        obs_contrib = gp_vals,
        true_betas = numeric(),
        true_smooths = list(
          `gp(x)` = data.frame(x = grid, f_true = true_gp)
        )
      )
    },
    trend_params = function(n_series, n_timepoints, prop_trend) {
      list(
        params = list(ar = 0.7, sigma = trend_sigma(prop_trend)),
        time = NULL
      )
    }
  )
}


# Type 5: state-space: obs y ~ x, trend formula ~ s(z), AR(p = 1).
# Trend-side smooth + AR; obs is linear x.
#'@noRd
spec_type_5 <- function() {
  list(
    default_trend = AR(p = 1),
    default_prop_trend = 0.2,
    intercept = function(fam) intercept_for_family(fam),
    build_data = function(n_timepoints, n_series, series_fac,
                           time_int) {
      total_n <- n_timepoints * n_series
      x <- stats::runif(total_n, -1, 1)
      # Trend-side smooth on z. z is the obs-side covariate that
      # interacts with the trend in the SSM.
      z <- stats::runif(total_n, -2, 2)
      beta_x <- 1.0
      sm_z <- sim_smooth(z, k = 8L, scale = 0.6)
      grid <- seq(-2, 2, length.out = 100L)
      true_sm_z <- sim_smooth_on_grid(sm_z$basis, sm_z$coefs,
                                        grid_x = grid)
      list(
        covariates = list(x = x, z = z),
        # The smooth-on-trend contribution adds to the linear
        # predictor on the obs side via the trend's mean.
        obs_contrib = beta_x * x + sm_z$f,
        true_betas = c(b_x = beta_x),
        true_smooths = list(
          `s(z)` = data.frame(z = grid, f_true = true_sm_z)
        )
      )
    },
    trend_params = function(n_series, n_timepoints, prop_trend) {
      list(
        params = list(ar = 0.6, sigma = trend_sigma(prop_trend)),
        time = NULL
      )
    }
  )
}


# Type 6: y ~ s(season, bs = "cc"), CAR(time, series) with
# irregular Delta t ~ Uniform(1, 6).
#'@noRd
spec_type_6 <- function() {
  list(
    default_trend = CAR(),
    default_prop_trend = 0.5,
    intercept = function(fam) intercept_for_family(fam),
    build_data = function(n_timepoints, n_series, series_fac,
                           time_int) {
      total_n <- n_timepoints * n_series
      # Build the irregular continuous time grid once. The same
      # gaps set the recorded time column, the season covariate
      # derived from it, and the spacing the CAR kernel
      # propagates over, so the three cannot disagree. All series
      # share the gap sequence.
      shared_gaps <- c(0, stats::runif(n_timepoints - 1L, 1, 6))
      shared_times <- cumsum(shared_gaps)
      time_long <- rep(shared_times, n_series)
      season <- ((time_long %% 12) + 1)
      sm_season <- sim_smooth(season, k = 6L, bs = "cc",
                                scale = 0.5)
      grid <- seq(1, 12, length.out = 100L)
      true_sm <- sim_smooth_on_grid(sm_season$basis,
                                      sm_season$coefs,
                                      grid_x = grid)
      list(
        covariates = list(season = season),
        obs_contrib = sm_season$f,
        true_betas = numeric(),
        true_smooths = list(
          `s(season)` = data.frame(season = grid, f_true = true_sm)
        ),
        time_long = time_long,
        # In units of the median gap, as a fit measures them
        trend_time = car_scaled_gaps(
          shared_gaps, car_time_scale(list(values = shared_times))
        )
      )
    },
    trend_params = function(n_series, n_timepoints, prop_trend) {
      # The gaps CAR propagates over come from `build_data` via
      # `trend_time`, so the dynamics run on the spacing the data
      # records rather than on a second draw of it.
      list(
        params = list(phi = 0.7,
                       sigma = trend_sigma(prop_trend)),
        time = NULL
      )
    }
  )
}


# Type 7: y ~ s(x), sparse AR(p = c(1, 12)) latent state. The
# observation side carries a smooth of a non-periodic covariate and
# the latent state carries all of the temporal structure, at lag 1
# and lag 12. Demonstrates the value of a state-space model when
# serial dependence is the only source of that structure.
#'@noRd
spec_type_7 <- function() {
  list(
    default_trend = AR(p = c(1L, 12L)),
    # The observation-side covariate must not be periodic. A
    # cyclic seasonal smooth cannot be separated from the lag-12
    # coefficient, because on monthly data a lag-12
    # autoregression is itself an annual cycle: the two describe
    # the same periodicity and the posterior trades variance
    # between them rather than resolving either.
    default_prop_trend = 0.6,
    intercept = function(fam) intercept_for_family(fam),
    build_data = function(n_timepoints, n_series, series_fac,
                           time_int) {
      total_n <- n_timepoints * n_series
      x <- stats::runif(total_n, -2, 2)
      sm <- sim_smooth(x, k = 8L, bs = "tp", scale = 0.9)
      grid <- seq(-2, 2, length.out = 100L)
      true_sm <- sim_smooth_on_grid(sm$basis, sm$coefs, grid_x = grid)
      list(
        covariates = list(x = x),
        obs_contrib = sm$f,
        true_betas = numeric(),
        true_smooths = list(
          `s(x)` = data.frame(x = grid, f_true = true_sm)
        )
      )
    },
    trend_params = function(n_series, n_timepoints, prop_trend) {
      # Sparse AR(1, 12): phi_1 = 0.55, phi_12 = 0.40. Sum 0.95
      # is high enough that the state visibly recurs year-on-year,
      # so a fit carrying no dynamics cannot match it, while
      # keeping the process inside the stationary region. Higher sums push
      # the AR(1, 12) into nonstationary territory and trigger
      # divergent transitions during HMC.
      list(
        params = list(
          ar    = c(0.55, 0.40),
          sigma = trend_sigma(prop_trend)
        ),
        time = NULL
      )
    }
  )
}


# ------------------------------------------------------------------
# Shared building blocks
# ------------------------------------------------------------------


#' The link-scale intercept a simulation uses for a family
#'
#' Each value places the inverse-link mean in a plausible response
#' range, such as a Poisson mean near 5 or a probability near 0.5.
#'
#' @param fam_name Character family name from `resolve_family_name()`
#' @return Numeric scalar intercept on the link scale
#' @noRd
intercept_for_family <- function(fam_name) {
  checkmate::assert_string(fam_name)
  switch(
    fam_name,
    "gaussian" = 0,
    "student" = 0,
    "gamma" = log(2),     # log link: mean ~ 2
    "poisson" = log(5),    # log link: mean ~ 5
    "negbinomial" = log(5),
    "binomial" = 0,        # logit link: probability ~ 0.5
    "bernoulli" = 0,
    "beta" = 0,            # logit link: mean ~ 0.5
    "tweedie" = log(2),    # log link: mean ~ 2 (CP with zeros)
    "beta_nb" = log(5),    # log link: mean ~ 5
    "com_binomial" = 0,    # logit link: probability ~ 0.5
    0
  )
}


#' The innovation SD a simulation type passes to `propagate_trend()`
#'
#' Derived from the trend's share of the link-scale variance, clamped to
#' `[0.01, 0.99]`. `sim_mvgam()` rescales every simulated trend to its
#' target SD after propagation. This value sets only the starting
#' amplitude.
#'
#' @param prop_trend Numeric share of link-scale variance given to the
#'   trend
#' @return Positive numeric scalar
#' @noRd
trend_sigma <- function(prop_trend) {
  checkmate::assert_number(prop_trend)
  prop_trend <- max(min(prop_trend, 0.99), 0.01)
  sqrt(prop_trend / (1 - prop_trend))
}


#' Whether a simulation trend is a piecewise `PW()` trend
#'
#' A piecewise trend is deterministic given `k`, `m`, `delta` and
#' `t_change`. Its simulation evaluates the kernel directly and skips
#' `propagate_trend()`.
#'
#' @param trend_model Trend constructor object, a character trend name or
#'   `NULL`
#' @return Logical scalar
#' @noRd
is_pw_trend <- function(trend_model) {
  checkmate::assert(
    checkmate::check_null(trend_model),
    checkmate::check_character(trend_model),
    checkmate::check_list(trend_model)
  )
  if (is.null(trend_model)) return(FALSE)
  t <- if (is.character(trend_model)) {
    trend_model
  } else {
    trend_model$trend
  }
  identical(t, "PW")
}


#' Default parameters for a simulated piecewise trend
#'
#' Fills each of `k`, `m`, `delta`, `t_change` and, for logistic growth,
#' `cap` that `params` leaves unset.
#' * `k` is 0.02 per unit time, a gentle linear growth.
#' * `m` is 0.5, placing the logistic curve near half its capacity at
#'   the start.
#' * `delta` alternates in sign across changepoints with magnitudes near
#'   0.05. The magnitude grows with the series index to give each series
#'   a distinct path.
#' * `t_change` spaces changepoints evenly over the first
#'   `changepoint_range` share of the horizon. The default of 0.8 leaves
#'   the last fifth free of changepoints before forecasting.
#' * `cap` is a constant ceiling of three times `exp(m[1])`, keeping the
#'   logistic output away from saturation.
#'
#' @param trend_model `PW()` trend object holding `n_changepoints`,
#'   `changepoint_range` and `growth`
#' @param params Named list of user-supplied trend parameters
#' @param n_series Integer number of series
#' @param n_timepoints Integer number of time points
#' @return `params` with every missing piecewise parameter filled
#' @noRd
fill_pw_trend_defaults <- function(trend_model, params,
                                      n_series, n_timepoints) {
  checkmate::assert_list(trend_model)
  checkmate::assert_list(params, null.ok = TRUE)
  checkmate::assert_int(n_series, lower = 1L)
  checkmate::assert_int(n_timepoints, lower = 1L)
  n_change <- as.integer(trend_model$n_changepoints %||% 5L)
  range_prop <- as.numeric(
    trend_model$changepoint_range %||% 0.8
  )
  last_cp <- max(1.0, range_prop * n_timepoints)
  if (is.null(params$t_change)) {
    params$t_change <- seq.int(
      from = max(2L, floor(0.05 * n_timepoints)),
      to = floor(last_cp),
      length.out = n_change
    )
  }
  if (is.null(params$k)) {
    params$k <- rep(0.02, n_series)
  }
  if (is.null(params$m)) {
    params$m <- rep(0.5, n_series)
  }
  if (is.null(params$delta)) {
    signs <- (-1)^seq_len(n_change)
    delta_mat <- matrix(0, nrow = n_change, ncol = n_series)
    for (s in seq_len(n_series)) {
      delta_mat[, s] <- 0.05 * signs *
        (1 + 0.1 * (s - 1L))
    }
    params$delta <- delta_mat
  }
  if (pw_is_logistic(trend_model) && is.null(params$cap)) {
    params$cap <- matrix(
      exp(params$m[1L]) * 3,
      nrow = n_timepoints, ncol = n_series
    )
  }
  params
}


#' Trend-specific default parameters for a simulation
#'
#' The simulation types supply `sigma` and sometimes `ar`, enough for
#' univariate RW, AR, CAR and ZMVN trends. This fills the parameters
#' other trend constructors need when `params` leaves them unset.
#' * An `AR(p)` trend with a scalar `ar` gets a geometric taper over `p`
#'   lags, `ar[l] = ar[1] * 0.3^(l - 1)`.
#' * A `VAR` trend gets a lag-1 transition matrix with 0.5 on the
#'   diagonal. The off-diagonal entries share 0.30 over the other
#'   series, which keeps every row sum below 1 for stability.
#' * A `VAR`, `ZMVN` or `cor = TRUE` trend gets a `Sigma` with
#'   correlation 0.5 between every pair of series.
#'
#' @param trend_model Trend constructor object, a character trend name or
#'   `NULL`
#' @param params Named list of trend parameters from the simulation type
#' @param n_series Integer number of series
#' @return `params` with the trend-specific defaults filled
#' @noRd
fill_multivariate_trend_defaults <- function(trend_model,
                                                params, n_series) {
  checkmate::assert(
    checkmate::check_null(trend_model),
    checkmate::check_character(trend_model),
    checkmate::check_list(trend_model)
  )
  checkmate::assert_list(params, null.ok = TRUE)
  checkmate::assert_int(n_series, lower = 1L)
  if (is.null(trend_model) || is.character(trend_model)) {
    return(params)
  }
  trend_type <- trend_model$trend
  if (is.null(trend_type)) return(params)
  cor_trend <- isTRUE(trend_model$cor)

  # AR / RW: pad `params$ar` to length p when the user passes
  # AR(p > 1) but the spec only supplied a scalar phi. A geometric
  # taper phi_l = phi_1 * (0.3) ^ (l - 1) gives a stable, strong
  # memory profile (sum across lags well under 1) and lets type 2
  # / type 3 / type 4 simulate genuine AR(p) dynamics rather than
  # erroring on the length mismatch in `build_arma_A()`.
  if (identical(trend_type, "AR") && !is.null(params$ar) &&
        !is.null(trend_model$p)) {
    p_req <- if (length(trend_model$p) == 1L) {
      as.integer(trend_model$p)
    } else {
      length(trend_model$p)
    }
    ar_vec <- as.numeric(params$ar)
    if (length(ar_vec) < p_req) {
      ar_taper <- ar_vec[1L] * (0.3) ^ (seq_len(p_req) - 1L)
      ar_vec <- ar_taper
      params$ar <- ar_vec
    }
  }

  # VAR transition matrix.
  if (identical(trend_type, "VAR") && is.null(params$A)) {
    n_lags <- 1L
    if (!is.null(trend_model$p)) {
      n_lags <- length(seq_len(trend_model$p))
    }
    # Default A populates lag-1 only; higher lags stay zero.
    # Warn when the user requested p > 1 but didn't supply A,
    # since the simulated data will reflect VAR(1) dynamics
    # with a higher-lag label and any VAR(p > 1) fit will
    # estimate near-zero coefficients at lags >= 2.
    if (n_lags > 1L) {
      warn_once(
        paste0(
          "VAR(p > 1) default A populates lag 1 only; ",
          "higher lags are zero. Supply 'params$A' to ",
          "sim_mvgam() for genuine VAR(p > 1) dynamics."
        ),
        "mvgam_sim_var_default_p_gt_1"
      )
    }
    # Scale off-diagonal by n_series - 1 so the row sums stay
    # below 1 (a sufficient condition for stability) regardless
    # of n_series.
    off_diag <- 0.30 / max(1L, n_series - 1L)
    A_cube <- array(0, dim = c(n_series, n_series, n_lags))
    A_cube[, , 1L] <- 0.5 * diag(n_series) +
      off_diag * (1 - diag(n_series))
    params$A <- A_cube
  }

  # cor = TRUE (or VAR which is always correlated):
  # cross-correlated Sigma.
  needs_sigma <- (identical(trend_type, "VAR") ||
                    cor_trend ||
                    identical(trend_type, "ZMVN")) &&
    is.null(params$Sigma)
  if (needs_sigma && !is.null(params$sigma)) {
    sigma_vec <- as.numeric(params$sigma)
    if (length(sigma_vec) == 1L) {
      sigma_vec <- rep(sigma_vec, n_series)
    }
    rho <- 0.5
    cor_mat <- rho * matrix(1, n_series, n_series) +
      (1 - rho) * diag(n_series)
    params$Sigma <- diag(sigma_vec) %*% cor_mat %*%
      diag(sigma_vec)
  }
  params
}


#' The total link-scale SD a simulation allows for a family
#'
#' Bounds the linear predictor to keep the simulated response in a
#' recoverable range. Log-link families get 0.8 and logit-link families
#' get 1.5, which spans probabilities near 0.10 to 0.90. Identity-link
#' and other families get 1.
#'
#' @param fam_name Character family name from `resolve_family_name()`
#' @return Positive numeric scalar
#' @noRd
link_scale_budget <- function(fam_name) {
  checkmate::assert_string(fam_name)
  switch(
    fam_name,
    "gaussian" = 1.0,
    "student" = 1.0,
    "gamma" = 0.8,
    "poisson" = 0.8,
    "negbinomial" = 0.8,
    "binomial" = 1.5,
    "bernoulli" = 1.5,
    "beta" = 1.5,
    "tweedie" = 0.8,
    "beta_nb" = 0.8,
    "com_binomial" = 1.5,
    1.0
  )
}


#' The scalar that rescales a vector to a target SD
#'
#' A scalar lets the caller apply one factor to several linked objects,
#' such as the observation contribution and its ground-truth smooths.
#' A constant or non-finite input returns 0 to keep NaN out of the
#' simulation.
#'
#' @param x Numeric vector or matrix to rescale
#' @param target_sd Non-negative target sample SD
#' @return Numeric scalar factor
#' @noRd
sd_rescale_factor <- function(x, target_sd) {
  checkmate::assert_numeric(x)
  checkmate::assert_number(target_sd, lower = 0)
  current <- stats::sd(as.numeric(x))
  if (!is.finite(current) || current < 1e-8) {
    return(0)
  }
  target_sd / current
}


#' Family dispersion parameters for a simulation
#'
#' User values in `family_pars` win. A Gaussian or Student-t `sigma`
#' defaults to the observation share of the link-scale SD, with a floor
#' of 0.1. Other families get fixed defaults.
#'
#' @param family Family object of the simulated model
#' @param family_pars Named list of user-supplied family parameters
#' @param prop_trend Numeric share of link-scale variance given to the
#'   trend
#' @param eta_sd Numeric SD of the simulated linear predictor
#' @return Named list of family parameters for `sim_family_rng()`
#' @noRd
sim_family_pars <- function(family, family_pars, prop_trend,
                              eta_sd) {
  checkmate::assert_list(family)
  checkmate::assert_list(family_pars)
  checkmate::assert_number(prop_trend)
  checkmate::assert_number(eta_sd, na.ok = TRUE)
  fam_name <- resolve_family_name(family)
  # obs noise SD = (1 - prop_trend) share of link-scale variance,
  # converted to family-specific scale.
  noise_sigma <- max(eta_sd * sqrt(1 - prop_trend), 0.1)
  out <- switch(
    fam_name,
    "gaussian" = list(sigma = family_pars$sigma %||% noise_sigma),
    "student" = list(
      sigma = family_pars$sigma %||% noise_sigma,
      nu = family_pars$nu %||% 4
    ),
    "poisson" = list(),
    "negbinomial" = list(size = family_pars$size %||% 10),
    "binomial" = list(trials = family_pars$trials %||% 10L),
    "bernoulli" = list(trials = 1L),
    "beta" = list(phi = family_pars$phi %||% 10),
    "gamma" = list(shape = family_pars$shape %||% 5),
    "tweedie" = list(
      phi   = family_pars$phi   %||% 1,
      power = family_pars$power %||% 1.5
    ),
    "beta_nb" = list(
      shape = family_pars$shape %||% 2,
      mtail = family_pars$mtail %||% 2
    ),
    "com_binomial" = list(
      trials = family_pars$trials %||% 10L,
      nu     = family_pars$nu     %||% 1
    ),
    list()
  )
  out
}


#' A simulated smooth evaluated on a grid
#'
#' Fills the ground-truth `true_smooths` slot by evaluating the basis at
#' new covariate values with `mgcv::PredictMat()`.
#'
#' @param basis mgcv smooth object from `sim_smooth()`
#' @param coefs Numeric vector of spline coefficients
#' @param grid_x Numeric vector of covariate values
#' @return Numeric vector with the smooth at each value of `grid_x`
#' @noRd
sim_smooth_on_grid <- function(basis, coefs, grid_x) {
  checkmate::assert_list(basis)
  checkmate::assert_numeric(coefs, min.len = 1L)
  checkmate::assert_numeric(grid_x, min.len = 1L)
  newdata <- stats::setNames(
    list(grid_x), basis$term[1L]
  )
  Xp <- mgcv::PredictMat(basis, data = as.data.frame(newdata))
  as.numeric(Xp %*% coefs)
}


#' A simulated data set split by time into training and test sets
#'
#' The test set takes the last time points, as a forecast would.
#'
#' @param data_long Data frame holding a `time` column
#' @param n_timepoints Integer number of time points
#' @param proportional_train Numeric share of time points to train on
#' @return List holding the `train` and `test` data frames. `test` is
#'   `NULL` when every time point trains.
#' @noRd
split_train_test <- function(data_long, n_timepoints,
                              proportional_train) {
  checkmate::assert_data_frame(data_long)
  checkmate::assert_int(n_timepoints, lower = 1L)
  checkmate::assert_number(proportional_train, lower = 0, upper = 1)
  n_train <- max(2L, floor(proportional_train * n_timepoints))
  if (n_train >= n_timepoints) {
    return(list(train = data_long, test = NULL))
  }
  is_train <- data_long$time <=
    sort(unique(data_long$time))[n_train]
  train <- data_long[is_train, , drop = FALSE]
  test <- data_long[!is_train, , drop = FALSE]
  rownames(train) <- NULL
  rownames(test) <- NULL
  list(train = train, test = test)
}
