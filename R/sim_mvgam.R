#' Simulate `mvgam`-shaped time-series data with known truth
#'
#' Simulates training and test data from one of seven
#' observation-side recipes (`type`) combined with a latent trend
#' (`trend_model`). The recipes cover smooths, Gaussian processes and
#' random effects. Every generative quantity is returned for recovery
#' checks.
#'
#' @param type Integer in `1:7` selecting the observation-side
#'   recipe. See *Details*.
#' @param family The observation family: `gaussian()` (the default),
#'   `student()`, `poisson()`, `negbinomial()`, `bernoulli()`,
#'   `binomial()`, `Beta()`, `Gamma()`, [tweedie()], [beta_nb()] or
#'   [com_binomial()], or the matching `brms::brmsfamily()`.
#' @param n_series Integer number of series. Defaults to `1L`.
#' @param n_timepoints Integer number of time points per series.
#'   Defaults to `80L`.
#' @param trend_model A trend constructor such as `AR(p = 1)`,
#'   `AR(p = c(1, 12))`, `VAR(cor = TRUE)`, `CAR()`, `ZMVN()` or
#'   `PW()`. `NULL` (the default) takes the type's own trend. `"None"`
#'   simulates no trend.
#' @param prop_trend Share of the link-scale variance given to the
#'   trend, in `[0, 1]`. `NULL` takes the type's default: 0.2 for type
#'   5, 0.6 for types 2, 3, 4 and 7, and 0.5 for types 1 and 6.
#' @param proportional_train Share of the time points kept in
#'   `data_train`, between 0.1 and 1. The remaining, later time points
#'   form `data_test`. Defaults to `0.75`.
#' @param prop_missing Share of the training responses set to `NA`,
#'   between 0 and 0.5. Defaults to `0`.
#' @param family_pars Optional named list of family parameters:
#'   `sigma` and `nu` (gaussian, student), `size` (negative binomial),
#'   `trials` (binomial, com_binomial), `phi` (beta, tweedie), `shape`
#'   (gamma, beta_nb), `power` (tweedie), `mtail` (beta_nb) and `nu`
#'   (com_binomial dispersion). Unset entries take defaults.
#' @param seed Optional integer seed. The session's random state is
#'   restored when the call returns.
#'
#' @details
#' \describe{
#'   \item{`type = 1`}{`y ~ s(x)` with an `RW()` trend.}
#'   \item{`type = 2`}{`y ~ s(x) + s(z)` with an `AR(p = 1)` trend
#'     whose coefficient is 0.7.}
#'   \item{`type = 3`}{`y ~ s(x) + (1 | grp)` with an `AR(p = 1)`
#'     trend whose coefficient is 0.7. The random intercept has five
#'     levels.}
#'   \item{`type = 4`}{`y ~ gp(x)` with an `AR(p = 1)` trend whose
#'     coefficient is 0.7.}
#'   \item{`type = 5`}{`y ~ x` with `trend_formula = ~ s(z)` and an
#'     `AR(p = 1)` trend whose coefficient is 0.6. The smallest
#'     `prop_trend` default keeps the autoregression separable from the
#'     smooth.}
#'   \item{`type = 6`}{`y ~ s(season, bs = "cc")` with a `CAR()` trend
#'     on irregular gaps drawn from `Uniform(1, 6)`. The autocorrelation
#'     over the median gap is 0.7.}
#'   \item{`type = 7`}{`y ~ s(x)` with a sparse `AR(p = c(1, 12))`
#'     trend whose coefficients are 0.55 and 0.40. The covariate is not
#'     periodic. All of the seasonal recurrence comes from the lag-12
#'     coefficient.}
#' }
#'
#' Covariates are drawn independently for each row. Series are
#' independent unless the trend correlates them, as `VAR()`, `ZMVN()`
#' and `cor = TRUE` do. The trend and the observation-side terms are
#' each centred, then rescaled to their shares of a link-scale standard
#' deviation that keeps the response in a plausible range for the
#' family.
#'
#' @return A list of class `"mvgam_sim"`:
#'   \describe{
#'     \item{`data_train`, `data_test`}{Long data frames with columns
#'       `y`, `series`, `time`, the recipe's covariates and, for
#'       binomial families, `trials`. `data_test` is `NULL` when
#'       `proportional_train = 1`.}
#'     \item{`family`, `trend_model`, `type`}{The simulation settings.}
#'     \item{`true_betas`}{Named population-level coefficients.}
#'     \item{`true_smooths`}{Named list of data frames holding each
#'       true smooth on a grid of its covariate, in column `f_true`.}
#'     \item{`true_trend`}{`[n_timepoints, n_series]` matrix of the
#'       latent trend.}
#'     \item{`true_trend_sigma`}{Standard deviation of `true_trend`.}
#'     \item{`true_sigma_obs`}{The family's scale or dispersion, or
#'       `NA` for a family without one.}
#'   }
#'
#' @author Nicholas J Clark
#'
#' @seealso [mvgam()] fits the simulated data. [sim_closure_unit_data()]
#'   simulates occupancy and N-mixture data. [sim_jsdgam()] simulates
#'   site-by-species data. The
#'   `r mvgam_article("mvgam_overview", "overview article")` fits a
#'   model to data from `sim_mvgam()`.
#'
#' @examples
#' # Three Poisson series with an AR(1) latent trend
#' simdat <- sim_mvgam(
#'   family       = poisson(),
#'   n_series     = 3L,
#'   n_timepoints = 120L,
#'   trend_model  = AR(),
#'   seed         = 0L
#' )
#' head(simdat$data_train)
#' simdat
#'
#' # The true smooth of x
#' plot(f_true ~ x, data = simdat$true_smooths$`s(x)`, type = "l")
#'
#' # Next: mvgam(y ~ s(x), trend_formula = ~ AR(),
#' #   data = simdat$data_train, family = poisson())
#'
#' @family simulation
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
  checkmate::assert_number(proportional_train, lower = 0.1, upper = 1)
  checkmate::assert_number(prop_missing, lower = 0, upper = 0.5)
  checkmate::assert_list(family_pars)
  checkmate::assert(
    checkmate::check_null(trend_model),
    checkmate::check_class(trend_model, "mvgam_trend"),
    checkmate::check_choice(trend_model, "None"),
    .var.name = "trend_model"
  )
  checkmate::assert_number(prop_trend, lower = 0, upper = 1,
                           null.ok = TRUE)
  family <- validate_family(family)
  fam_name <- resolve_family_name(family)

  local_seed(seed)

  spec <- sim_type_spec(type)
  trend_model <- trend_model %||% spec$default_trend
  prop_trend <- prop_trend %||% spec$default_prop_trend

  series_fac <- factor(
    rep(paste0("series_", seq_len(n_series)), each = n_timepoints),
    levels = paste0("series_", seq_len(n_series))
  )
  time_int <- rep(seq_len(n_timepoints), times = n_series)
  built <- spec$build_data(n_timepoints = n_timepoints,
                           n_series = n_series)

  total_link_sd <- link_scale_budget(fam_name)
  trend_params <- fill_multivariate_trend_defaults(
    trend_model,
    c(spec$trend_params, list(sigma = trend_sigma(prop_trend))),
    n_series
  )
  trend_mat <- if (is_pw_trend(trend_model)) {
    # A piecewise trend is deterministic given (k, m, delta, t_change)
    trend_params <- fill_pw_trend_defaults(
      trend_model, trend_params, n_series, n_timepoints
    )
    pw_trendC(
      t = as.numeric(seq_len(n_timepoints)),
      k = as.numeric(trend_params$k),
      m = as.numeric(trend_params$m),
      delta = trend_params$delta,
      t_change = as.numeric(trend_params$t_change),
      cap = if (pw_is_logistic(trend_model)) {
        trend_params$cap
      } else {
        matrix(0, 0L, 0L)
      },
      growth_type = pw_growth(trend_model)
    )
  } else {
    # A type with irregular times supplies the gaps its trend runs over
    propagate_trend(
      trend_model = trend_model,
      params = trend_params,
      h = n_timepoints,
      n_series = n_series,
      time = built$trend_time
    )
  }

  # Both components are centred. The mean of eta is then the family's
  # intercept. Each is rescaled to its share of the link-scale
  # variance. A scalar rescale keeps the shape of the trend: a random
  # walk keeps its growing variance and an autoregression its
  # autocorrelation.
  trend_mat <- (trend_mat - mean(trend_mat)) *
    sd_rescale_factor(trend_mat, total_link_sd * sqrt(prop_trend))
  obs_centred <- built$obs_contrib - mean(built$obs_contrib)
  obs_scale <- sd_rescale_factor(obs_centred,
                                 total_link_sd * sqrt(1 - prop_trend))
  obs_contrib <- obs_centred * obs_scale
  # The recorded truths take the same centring and scale as the data
  built$true_smooths <- lapply(built$true_smooths, function(df) {
    df$f_true <- (df$f_true - mean(df$f_true)) * obs_scale
    df
  })
  built$true_betas <- built$true_betas * obs_scale

  eta <- intercept_for_family(fam_name) + obs_contrib +
    as.numeric(trend_mat)
  obs_pars <- sim_family_pars(family, family_pars, prop_trend,
                              stats::sd(eta))
  data_long <- data.frame(
    y = sim_family_rng(eta, family, obs_pars),
    series = series_fac,
    time = built$time_long %||% time_int
  )
  for (nm in names(built$covariates)) {
    data_long[[nm]] <- built$covariates[[nm]]
  }
  if (fam_name %in% c("binomial", "com_binomial")) {
    data_long$trials <- obs_pars$trials
  }

  split <- split_train_test(data_long, n_timepoints, proportional_train)
  if (prop_missing > 0) {
    n_train <- nrow(split$train)
    split$train$y[sample.int(n_train,
                             floor(prop_missing * n_train))] <- NA
  }

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
#' The design of a simulated data set and its true generative
#' parameters.
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
#' @param ... Unused.
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
  cat("  Timepoints   : ", x$n_timepoints, "\n", sep = "")
  cat("  Rows         : ", x$n_train, " train, ", x$n_test, " test\n",
      sep = "")
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


#' The specification of a `sim_mvgam()` recipe
#'
#' Each type supplies its default trend and `prop_trend` and the trend
#' parameters other than `sigma`. Its `build_data(n_timepoints,
#' n_series)` function returns the `covariates`, the observation-side
#' contribution `obs_contrib`, `true_betas` and `true_smooths`. A type
#' with irregular times adds `time_long` and the `trend_time` gaps.
#'
#' @param type Integer recipe number
#' @return List describing the recipe
#' @noRd
sim_type_spec <- function(type) {
  list(spec_type_1, spec_type_2, spec_type_3, spec_type_4, spec_type_5,
       spec_type_6, spec_type_7)[[type]]()
}


#' The true smooth of a simulated basis on a covariate grid
#'
#' @param sm List returned by `sim_smooth()`
#' @param var Character name of the covariate
#' @param grid Numeric covariate values
#' @return Data frame with the covariate column and `f_true`
#' @noRd
smooth_truth <- function(sm, var, grid = seq(-2, 2, length.out = 100L)) {
  out <- data.frame(grid, sim_smooth_on_grid(sm$basis, sm$coefs, grid))
  names(out) <- c(var, "f_true")
  out
}


# Type 1: y ~ s(x), RW trend
#'@noRd
spec_type_1 <- function() {
  list(
    default_trend = RW(),
    default_prop_trend = 0.5,
    trend_params = list(),
    build_data = function(n_timepoints, n_series) {
      x <- stats::runif(n_timepoints * n_series, -2, 2)
      sm <- sim_smooth(x, k = 8L, bs = "tp", scale = 0.6)
      list(
        covariates = list(x = x),
        obs_contrib = sm$f,
        true_betas = numeric(),
        true_smooths = list(`s(x)` = smooth_truth(sm, "x"))
      )
    }
  )
}


# Type 2: y ~ s(x) + s(z), AR(1) trend
#'@noRd
spec_type_2 <- function() {
  list(
    default_trend = AR(p = 1),
    # With less of the variance in the trend, the smooths absorb the
    # serial signal and the AR coefficient is poorly identified
    default_prop_trend = 0.6,
    trend_params = list(ar = 0.7),
    build_data = function(n_timepoints, n_series) {
      total_n <- n_timepoints * n_series
      x <- stats::runif(total_n, -2, 2)
      z <- stats::runif(total_n, -2, 2)
      sm_x <- sim_smooth(x, k = 8L, scale = 0.5)
      sm_z <- sim_smooth(z, k = 8L, scale = 0.5)
      list(
        covariates = list(x = x, z = z),
        obs_contrib = sm_x$f + sm_z$f,
        true_betas = numeric(),
        true_smooths = list(
          `s(x)` = smooth_truth(sm_x, "x"),
          `s(z)` = smooth_truth(sm_z, "z")
        )
      )
    }
  )
}


# Type 3: y ~ s(x) + (1 | grp), AR(1) trend
#'@noRd
spec_type_3 <- function() {
  list(
    default_trend = AR(p = 1),
    default_prop_trend = 0.6,
    trend_params = list(ar = 0.7),
    build_data = function(n_timepoints, n_series) {
      total_n <- n_timepoints * n_series
      x <- stats::runif(total_n, -2, 2)
      grp <- sim_grp(total_n, n_levels = 5L)
      sm_x <- sim_smooth(x, k = 8L, scale = 0.5)
      re <- sim_re(grp, sigma = 0.6)
      list(
        covariates = list(x = x, grp = grp),
        obs_contrib = sm_x$f + re$values,
        true_betas = re$coefs,
        true_smooths = list(`s(x)` = smooth_truth(sm_x, "x"))
      )
    }
  )
}


# Type 4: y ~ gp(x), AR(1) trend
#'@noRd
spec_type_4 <- function() {
  list(
    default_trend = AR(p = 1),
    default_prop_trend = 0.6,
    trend_params = list(ar = 0.7),
    build_data = function(n_timepoints, n_series) {
      total_n <- n_timepoints * n_series
      x <- stats::runif(total_n, -2, 2)
      grid <- seq(-2, 2, length.out = 100L)
      # One draw at the data and the grid. The recorded truth is then
      # the function that generated the data.
      f <- sim_gp_cov(c(x, grid), alpha = 1, rho = 0.5)
      list(
        covariates = list(x = x),
        obs_contrib = f[seq_len(total_n)],
        true_betas = numeric(),
        true_smooths = list(
          `gp(x)` = data.frame(x = grid, f_true = f[-seq_len(total_n)])
        )
      )
    }
  )
}


# Type 5: y ~ x, trend_formula = ~ s(z), AR(1) trend
#'@noRd
spec_type_5 <- function() {
  list(
    default_trend = AR(p = 1),
    default_prop_trend = 0.2,
    trend_params = list(ar = 0.6),
    build_data = function(n_timepoints, n_series) {
      total_n <- n_timepoints * n_series
      x <- stats::runif(total_n, -1, 1)
      z <- stats::runif(total_n, -2, 2)
      beta_x <- 1
      sm_z <- sim_smooth(z, k = 8L, scale = 0.6)
      list(
        covariates = list(x = x, z = z),
        # A trend_formula effect adds to the linear predictor through
        # the state's mean
        obs_contrib = beta_x * x + sm_z$f,
        true_betas = c(b_x = beta_x),
        true_smooths = list(`s(z)` = smooth_truth(sm_z, "z"))
      )
    }
  )
}


# Type 6: y ~ s(season, bs = "cc"), CAR trend on irregular gaps
#'@noRd
spec_type_6 <- function() {
  list(
    default_trend = CAR(),
    default_prop_trend = 0.5,
    trend_params = list(phi = 0.7),
    build_data = function(n_timepoints, n_series) {
      # One gap sequence, shared by every series, sets the recorded
      # times, the season derived from them and the spacing the CAR
      # trend runs over
      gaps <- c(0, stats::runif(n_timepoints - 1L, 1, 6))
      times <- cumsum(gaps)
      time_long <- rep(times, n_series)
      season <- (time_long %% 12) + 1
      sm <- sim_smooth(season, k = 6L, bs = "cc", scale = 0.5)
      list(
        covariates = list(season = season),
        obs_contrib = sm$f,
        true_betas = numeric(),
        true_smooths = list(
          `s(season)` = smooth_truth(sm, "season",
                                     seq(1, 12, length.out = 100L))
        ),
        time_long = time_long,
        # In units of the median gap, as a fit measures them
        trend_time = car_scaled_gaps(
          gaps, car_time_scale(list(values = times))
        )
      )
    }
  )
}


# Type 7: y ~ s(x), sparse AR(p = c(1, 12)) trend
#'@noRd
spec_type_7 <- function() {
  list(
    default_trend = AR(p = c(1L, 12L)),
    # On monthly data a lag-12 autoregression is itself an annual
    # cycle. A periodic covariate would compete with it. The covariate
    # here is not periodic.
    default_prop_trend = 0.6,
    # The coefficients sum to 0.95. The state recurs visibly each year
    # and stays stationary.
    trend_params = list(ar = c(0.55, 0.40)),
    build_data = function(n_timepoints, n_series) {
      x <- stats::runif(n_timepoints * n_series, -2, 2)
      sm <- sim_smooth(x, k = 8L, bs = "tp", scale = 0.9)
      list(
        covariates = list(x = x),
        obs_contrib = sm$f,
        true_betas = numeric(),
        true_smooths = list(`s(x)` = smooth_truth(sm, "x"))
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
    params$delta <- outer(0.05 * (-1)^seq_len(n_change),
                          1 + 0.1 * (seq_len(n_series) - 1L))
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

  # A type's scalar AR coefficient tapers over the lags of a user's
  # AR(p > 1). The taper sums to under 1.43 times the first lag.
  if (identical(trend_type, "AR") && !is.null(params$ar) &&
        !is.null(trend_model$p)) {
    p_req <- if (length(trend_model$p) == 1L) {
      as.integer(trend_model$p)
    } else {
      length(trend_model$p)
    }
    if (length(params$ar) < p_req) {
      params$ar <- params$ar[1L] * 0.3^(seq_len(p_req) - 1L)
    }
  }

  # VAR transition matrix.
  if (identical(trend_type, "VAR") && is.null(params$A)) {
    n_lags <- as.integer(trend_model$p %||% 1L)
    if (n_lags > 1L) {
      warn_once(
        c(
          "The simulated VAR(p > 1) trend has dynamics at lag 1 only.",
          i = "Coefficients at lags 2 and above are zero."
        ),
        "mvgam_sim_var_default_p_gt_1"
      )
    }
    # Row sums below 1 are sufficient for stability
    off_diag <- 0.30 / max(1L, n_series - 1L)
    A_cube <- array(0, dim = c(n_series, n_series, n_lags))
    A_cube[, , 1L] <- 0.5 * diag(n_series) +
      off_diag * (1 - diag(n_series))
    params$A <- A_cube
  }

  # A correlated trend takes a Sigma with correlation 0.5
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
  noise_sigma <- max(eta_sd * sqrt(1 - prop_trend), 0.1)
  switch(
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
