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
#'   *Details*). Sparse-lag specs like `AR(p = c(1, 3, 12))` are
#'   supported natively.
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
#' The six recipes:
#'
#' \describe{
#'   \item{`type = 1`}{`y ~ s(x)` with `RW()` trend.
#'     Single smooth covariate + random-walk trend; the simplest
#'     mvgam recipe.}
#'   \item{`type = 2`}{`y ~ s(x) + s(z)` with `AR(p = 1, phi = 0.7)`
#'     trend. Two additive smooths.}
#'   \item{`type = 3`}{`y ~ s(x) + (1 | grp)` with
#'     `AR(p = 1, phi = 0.7)`. Hierarchical: random intercept per
#'     group level (5 groups by default).}
#'   \item{`type = 4`}{`y ~ gp(x)` with `AR(p = 1, phi = 0.7)`.
#'     Gaussian-process covariate (mvgam-specific).}
#'   \item{`type = 5`}{State-space: obs `y ~ x`, trend formula
#'     `~ s(z)`, `AR(p = 1, phi = 0.6)`. The smooth lives on the
#'     trend, not on `y`'s linear predictor. `prop_trend` defaults
#'     to `0.2` for this type to keep the AR component small
#'     relative to the smooth so the two are identifiable.}
#'   \item{`type = 6`}{`y ~ s(season, bs = "cc")` with `CAR(time,
#'     series)`. Cyclic seasonal + continuous-time AR(1) on
#'     irregular time gaps (`Δt ~ Uniform(1, 6)`).}
#'   \item{`type = 7`}{`y ~ s(season, bs = "cc", k = 12)` with
#'     sparse `AR(p = c(1, 12))`. Monthly seasonal cycle on the
#'     obs side; the latent state carries lag-1 momentum and
#'     lag-12 year-on-year recurrence beyond the deterministic
#'     cycle. Pairs with a `prop_trend` default of `0.7` so the
#'     AR signal dominates residual variance.}
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
#'   `sim_mvgam()` produces.
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
  family <- validate_family(family)
  fam_name <- resolve_family_name(family)

  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv)) {
      rng_old <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", rng_old, envir = .GlobalEnv))
    }
    set.seed(seed)
  }

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

  # Trend propagation. Stationary processes (AR / VAR / CAR /
  # ZMVN) use `sigma = trend_sigma(prop_trend)` and are
  # rescaled post-propagation so the empirical SD matches the
  # target. Non-stationary processes (RW) have variance that
  # grows linearly with time; rescaling to a fixed empirical SD
  # would destroy that growth and produce a stationary-looking
  # trajectory. Instead we choose `sigma_innov` upfront so the
  # accumulated variance at the final timepoint lands near
  # `target_trend_sd^2`, then skip the post-hoc rescale and let
  # the natural RW shape through.
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
  is_pw <- is_pw_trend(trend_model)
  if (is_pw) {
    # PW (piecewise linear or logistic) needs its own param
    # defaults (k, m, delta, t_change) and a direct kernel
    # call -- the propagate_trend dispatcher's PW arm is
    # designed for forecast-horizon changepoint sampling,
    # which is wrong for simulation (we want a fully
    # deterministic trend at the training time grid).
    trend_args$params <- fill_pw_trend_defaults(
      trend_model, trend_args$params, n_series, n_timepoints
    )
  }
  nonstat <- is_nonstationary_trend(trend_model)
  # RW takes the integrated-variance sigma_innov override below.
  # Sparse-lag AR(p) is also classified nonstationary so the
  # post-hoc rescale (further down) is skipped, but the spec's
  # chosen sigma_innov is preserved: the closed-form variance of
  # an AR(p) is sigma_innov^2 / (1 - sum(phi)^2), which does not
  # have an RW-style sqrt(6/T) shape.
  is_ar_trend <- !is.null(trend_model) && !is.character(trend_model) &&
                 identical(trend_model$trend, "AR")
  if (nonstat && !is_pw && !is_ar_trend) {
    # Pick sigma_innov so the empirical SD of the centred RW
    # over t = 1..T matches `target_trend_sd`. The variance of
    # the centred RW at t averages sigma^2 * T/6 across t, so
    # solving sigma^2 * T/6 = target_trend_sd^2 gives the
    # scaling factor sqrt(6/T). This preserves the linearly
    # growing variance of the unscaled RW while keeping the
    # documented `prop_trend` meaning ("the share of total
    # link-scale variance contributed by the latent trend").
    trend_args$params$sigma <-
      target_trend_sd * sqrt(6 / n_timepoints)
  }
  trend_mat <- if (is_pw) {
    # PW is fully deterministic given (k, m, delta, t_change);
    # bypass the propagate_trend dispatcher (whose PW arm is
    # tuned for forecast-horizon changepoint sampling) and
    # evaluate the kernel directly at the training time grid.
    growth <- trend_model$growth %||% "linear"
    pw_trendC(
      t = as.numeric(seq_len(n_timepoints)),
      k = as.numeric(trend_args$params$k),
      m = as.numeric(trend_args$params$m),
      delta = trend_args$params$delta,
      t_change = as.numeric(trend_args$params$t_change),
      cap = if (identical(growth, "logistic")) {
        trend_args$params$cap
      } else {
        matrix(0, 0L, 0L)
      },
      growth_type = growth
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

  # Rescale stationary trends to the target empirical SD. Skip
  # for non-stationary trends -- sigma_innov was chosen upfront
  # to match the target.
  if (!nonstat) {
    trend_scale <- sd_rescale_factor(trend_vec, target_trend_sd)
    trend_mat <- trend_mat * trend_scale
  }
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
  if (tolower(fam_name) %in% c("binomial", "com_binomial")) {
    data_long$trials <- obs_pars$trials %||% 10L
  }
  if (!is.null(trend_args$time_long)) {
    # CAR uses irregular continuous time; overwrite integer time.
    data_long$time <- trend_args$time_long
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
#' @param ... Currently ignored.
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
  trend_label <- if (is.null(object$trend_model)) {
    "None"
  } else {
    object$trend_model$trend %||%
      object$trend_model$label %||%
      object$trend_model$type %||%
      "Unknown"
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
# irregular Δt ~ Uniform(1, 6).
#'@noRd
spec_type_6 <- function() {
  list(
    default_trend = CAR(),
    default_prop_trend = 0.5,
    intercept = function(fam) intercept_for_family(fam),
    build_data = function(n_timepoints, n_series, series_fac,
                           time_int) {
      total_n <- n_timepoints * n_series
      # Build irregular continuous time grid: cumulative
      # uniform-(1, 6) gaps from t = 0. All series share the
      # SAME gap sequence so forecast.mvgam's CAR helper
      # (which requires a single length-h time vector across
      # series) can consume the simulated test data without
      # tripping the per-series gap-mismatch guard.
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
        time_long = time_long
      )
    },
    trend_params = function(n_series, n_timepoints, prop_trend) {
      # CAR needs per-series time gaps; propagate_trend takes a
      # single length-h vector. For multi-series we propagate one
      # gap pattern shared across series (matches the
      # data_long$time we built).
      first_series_gaps <-
        c(0, stats::runif(n_timepoints - 1L, 1, 6))
      list(
        params = list(phi = 0.7,
                       sigma = trend_sigma(0.5)),
        time = first_series_gaps,
        time_long = NULL
      )
    }
  )
}


# Type 7: y ~ s(season, bs = "cc"), sparse AR(p = c(1, 12)) latent
# state. Monthly cycle on the obs side; the latent state carries
# both short-term momentum (lag 1) and annual recurrence beyond the
# deterministic cycle (lag 12). Demonstrates the value of a
# state-space model when the data has structure a fixed seasonal
# smooth cannot represent.
#'@noRd
spec_type_7 <- function() {
  list(
    default_trend = AR(p = c(1L, 12L)),
    # High prop_trend so the AR signal dominates: the seasonal
    # smooth is identifiable from the deterministic cycle alone,
    # and the dynamic state needs to carry most of the residual
    # variance for the model contrast to be visible at moderate n.
    # Tuned upward from the AR-recipe default (types 2/3/4 use 0.6)
    # so the seasonal smooth absorbs only the deterministic cycle
    # and the AR(1, 12) state explains the year-to-year drift that
    # a fixed-cycle fit cannot follow into the held-out horizon.
    default_prop_trend = 0.85,
    intercept = function(fam) intercept_for_family(fam),
    build_data = function(n_timepoints, n_series, series_fac,
                           time_int) {
      # Cycle season 1..12 across time. Per-series time grids are
      # identical so the obs-side smooth is fit on a single shared
      # cyclic covariate; the latent state is what differs by
      # series.
      season <- ((time_int - 1L) %% 12L) + 1L
      # Deterministic seasonal cycle. Amplitude tuned so the
      # seasonal smooth and the AR state contribute distinct,
      # identifiable shares of variance.
      f_season_t <- 1.0 * sin(2 * pi * season / 12) +
                    0.3 * cos(4 * pi * season / 12)
      grid <- seq(1, 12, length.out = 100L)
      f_season_grid <- 1.0 * sin(2 * pi * grid / 12) +
                       0.3 * cos(4 * pi * grid / 12)
      list(
        covariates = list(season = season),
        obs_contrib = f_season_t,
        true_betas = numeric(),
        true_smooths = list(
          `s(season)` = data.frame(season = grid,
                                     f_true = f_season_grid)
        )
      )
    },
    trend_params = function(n_series, n_timepoints, prop_trend) {
      # Sparse AR(1, 12): phi_1 = 0.55, phi_12 = 0.40. Sum 0.95
      # is high enough that the state visibly drifts year-on-year
      # beyond the deterministic seasonal cycle (so a fixed
      # smooth-on-season fit cannot match it) while keeping the
      # process inside the stationary region. Higher sums push
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


# Internal: family-specific intercept on the LINK scale. Chosen so
# the inverse-link image is in a sensible response range.
#'@noRd
intercept_for_family <- function(fam_name) {
  switch(
    tolower(fam_name),
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


# Internal: target latent-trend sigma given a prop_trend share of
# total link-scale variance. With obs-side deterministic SD ~ 1,
# `prop_trend` of the link-scale variance becomes the trend's
# variance share. Kept as a thin wrapper so type specs can pass a
# sensible scalar to propagate_trend's params; the empirical
# trend SD is rescaled post hoc inside `sim_mvgam` for
# stationary trends. Non-stationary trends (RW) override
# `sigma` upfront in the main flow rather than going through
# this helper.
#'@noRd
trend_sigma <- function(prop_trend) {
  prop_trend <- max(min(prop_trend, 0.99), 0.01)
  sqrt(prop_trend / (1 - prop_trend))
}


# Internal: TRUE when the trend kernel produces an integrated
# (non-stationary) trajectory whose variance grows with time.
# Post-propagation SD-rescaling is skipped for these trends to
# preserve the linearly-growing variance that defines the
# process; sim_mvgam picks `sigma_innov` upfront instead.
#
# RW is the only non-stationary kernel this predicate recognises;
# PW is identified separately, by the helper below.
#'@noRd
# Internal: TRUE when the trend constructor is `PW()`. PW
# trends need their own simulation path (deterministic kernel
# evaluation, not stochastic propagation) and their own
# parameter set (k, m, delta, t_change), so they bypass the
# multivariate-default + propagate_trend pipeline used for the
# stationary / non-stationary stochastic trends.
#'@noRd
is_pw_trend <- function(trend_model) {
  if (is.null(trend_model)) return(FALSE)
  t <- if (is.character(trend_model)) {
    trend_model
  } else {
    trend_model$trend
  }
  identical(t, "PW")
}


# Internal: PW-specific param defaults. The `PW()` constructor
# carries `n_changepoints`, `growth`, and `changepoint_scale`;
# this helper turns those into (k, m, delta, t_change) plus an
# optional cap matrix for logistic growth.
#
# Defaults:
#   * k       : 0.02 per unit time -- gentle linear growth.
#   * m       : 0.5 -- mild positive intercept.
#   * delta   : alternating +/- 0.05 magnitudes across
#     changepoints, independently per series so multi-series
#     PW sims have visibly distinct trajectories.
#   * t_change: evenly spaced across the training horizon,
#     respecting `changepoint_range` (defaults 0.8) so the
#     last 20% of the training period stays changepoint-free
#     and the trend has room to settle before forecasting.
#   * cap     : logistic only; defaults to a constant ceiling
#     of `exp(intercept) * 3` per series so the inverse-logit
#     output stays well clear of the saturation regime.
#'@noRd
fill_pw_trend_defaults <- function(trend_model, params,
                                      n_series, n_timepoints) {
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
  if (identical(trend_model$growth %||% "linear", "logistic")
      && is.null(params$cap)) {
    params$cap <- matrix(
      exp(params$m[1L]) * 3,
      nrow = n_timepoints, ncol = n_series
    )
  }
  params
}


is_nonstationary_trend <- function(trend_model) {
  if (is.null(trend_model)) return(FALSE)
  # `trend_model` may be a character ("None", "RW", "PW") or an
  # `mvgam_trend` constructor output with a `trend` slot.
  t <- if (is.character(trend_model)) {
    trend_model
  } else {
    trend_model$trend
  }
  if (is.null(t)) return(FALSE)
  if (identical(t, "RW") || identical(t, "PW")) return(TRUE)
  # Sparse-lag AR(p) (`p = c(1, 12)` etc.) is treated as
  # near-nonstationary: the high-persistence regime that motivates
  # sparse AR is what makes the latent state visibly drift, and the
  # stationary-AR post-hoc rescale would compress that drift to a
  # fixed marginal SD. Skip the rescale so the simulated state
  # behaves like a long-memory process rather than a re-scaled
  # white-noise-with-correlation.
  if (identical(t, "AR") && length(trend_model$p %||% 1L) > 1L) {
    return(TRUE)
  }
  FALSE
}


# Internal: trend-model-specific param defaults. Each per-type
# spec returns AR-style defaults (`sigma`, optionally `ar`),
# which suffice for univariate RW / AR / CAR / ZMVN. VAR
# needs an `A` cube of cross-series transition coefficients,
# and any cor = TRUE trend needs an off-diagonal `Sigma` to
# generate truly correlated draws. Fill those in here when
# the supplied `trend_model` requires them and the per-type
# spec didn't set them. Lets users sim with arbitrary trend
# constructors via `trend_model = VAR(p = 1)` etc.
#
# Defaults:
#   * VAR A: diagonal `phi_diag = 0.5`, off-diagonal
#     `phi_off = 0.30`. Stable (spectral radius 0.5 + 0.30 *
#     (n_series - 1) < 1 for n_series <= 2; larger n_series
#     reduces phi_off proportionally below). Off-diagonal is
#     large enough that the multivariate energy / variogram
#     scores can reliably discriminate VAR vs independent-AR
#     in misspecification tests.
#   * cor=TRUE Sigma: correlation `rho = 0.5` across all
#     series pairs (so the test discrimination has signal
#     without driving the predictive too far off the marginal)
#'@noRd
fill_multivariate_trend_defaults <- function(trend_model,
                                                params, n_series) {
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
    if (n_lags > 1L &&
        !identical(Sys.getenv("TESTTHAT"), "true")) {
      rlang::warn(
        paste0(
          "VAR(p > 1) default A populates lag 1 only; ",
          "higher lags are zero. Supply 'params$A' to ",
          "sim_mvgam() for genuine VAR(p > 1) dynamics."
        ),
        .frequency = "once",
        .frequency_id = "mvgam_sim_var_default_p_gt_1"
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


# Internal: total link-scale variance budget per family. Bounds
# eta so the inverse-link image lands in a recoverable, plottable
# range:
#   identity:  SD ~ 1 (gaussian/student)
#   log:       SD ~ 1 (exp(eta) varies by ~factor of 7 at +/- 1 SD)
#   logit:     SD ~ 1.5 (probabilities span ~[0.10, 0.90])
#   sqrt/inv:  SD ~ 1 (conservative)
#'@noRd
link_scale_budget <- function(fam_name) {
  switch(
    tolower(fam_name),
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


# Internal: scalar factor that, multiplied into `x`, gives an
# output with sample SD == `target_sd`. Returned as a scalar so
# the caller can apply it consistently across an arbitrary set of
# coupled tensors (trend matrix, obs contribution, ground-truth
# smooths) and keep them on the same scale. Returns 0 if the
# input is degenerate (avoids producing NaN downstream).
#'@noRd
sd_rescale_factor <- function(x, target_sd) {
  current <- stats::sd(as.numeric(x))
  if (!is.finite(current) || current < 1e-8) {
    return(0)
  }
  target_sd / current
}


# Internal: derive family-side dispersion parameters (sigma, phi,
# size, etc.) from the obs-side linpred scale and user overrides.
#'@noRd
sim_family_pars <- function(family, family_pars, prop_trend,
                              eta_sd) {
  fam_name <- tolower(resolve_family_name(family))
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


# Internal: rebuild a smooth on a fine grid from its basis +
# coefficients (for the ground-truth `true_smooths` slot). Uses
# mgcv::PredictMat to evaluate the basis at new x.
#'@noRd
sim_smooth_on_grid <- function(basis, coefs, grid_x) {
  newdata <- stats::setNames(
    list(grid_x), basis$term[1L]
  )
  Xp <- mgcv::PredictMat(basis, data = as.data.frame(newdata))
  as.numeric(Xp %*% coefs)
}


# Internal: train / test split by time. Held-out timepoints come
# from the END of the series (forecast-style split).
#'@noRd
split_train_test <- function(data_long, n_timepoints,
                              proportional_train) {
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
