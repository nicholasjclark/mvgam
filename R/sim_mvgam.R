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
#' @param type Integer in `1:6` selecting the observation-side
#'   recipe. See *Details* for the catalog.
#' @param family A `family` or `brmsfamily` object specifying the
#'   observation likelihood. Supported in v1: `gaussian()`,
#'   `student()`, `poisson()`, `negbinomial()`,
#'   `binomial()`, `Beta()`, `Gamma()` (and their `brms::brmsfamily`
#'   equivalents). Defaults to `gaussian()`.
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
#'   `shape`). Type-specific defaults are used when an entry is
#'   absent.
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
#'
#' @examples
#' \donttest{
#' sim <- sim_mvgam(type = 1, family = gaussian(),
#'                  n_timepoints = 60, seed = 1)
#' head(sim$data_train)
#' str(sim$true_smooths)
#' }
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
  checkmate::assert_int(type, lower = 1L, upper = 6L)
  checkmate::assert_int(n_series, lower = 1L)
  checkmate::assert_int(n_timepoints, lower = 5L)
  checkmate::assert_number(
    proportional_train, lower = 0.1, upper = 1
  )
  checkmate::assert_number(prop_missing, lower = 0, upper = 0.5)
  checkmate::assert_list(family_pars)
  family <- validate_family(family)
  fam_name <- family$family

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
  nonstat <- is_nonstationary_trend(trend_model)
  if (nonstat) {
    trend_args$params$sigma <-
      target_trend_sd / sqrt(n_timepoints)
  }
  trend_mat <- propagate_trend(
    trend_model = trend_model,
    params = trend_args$params,
    h = n_timepoints,
    n_series = n_series,
    time = trend_args$time
  )

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
  # Binomial fits need a `trials` column; sim_family_pars stored
  # the trial count under obs_pars$trials.
  if (tolower(fam_name) == "binomial") {
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
    `6` = spec_type_6()
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
    default_prop_trend = 0.4,
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
    default_prop_trend = 0.4,
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
    default_prop_trend = 0.4,
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
      # Build irregular continuous time per series: cumulative
      # uniform-(1, 6) gaps from t = 0.
      time_long <- numeric(total_n)
      for (s in seq_len(n_series)) {
        idx <- ((s - 1L) * n_timepoints + 1L):(s * n_timepoints)
        gaps <- c(0, stats::runif(n_timepoints - 1L, 1, 6))
        time_long[idx] <- cumsum(gaps)
      }
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
# RW is the only non-stationary kernel on this branch. PW joins
# the list once the piecewise-linear / -logistic kernel lands.
#'@noRd
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
  identical(t, "RW") || identical(t, "PW")
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
  fam_name <- tolower(family$family)
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
