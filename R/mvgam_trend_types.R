#' Specify autoregressive dynamic processes in \pkg{mvgam}
#'
#' Set up autoregressive or autoregressive moving average trend models in
#' \pkg{mvgam}. These functions do not evaluate their arguments – they exist
#' purely to help set up a model with particular autoregressive trend models.
#'
#' @param ma \code{Logical}. Include moving average terms of order \code{1}?
#'   Default is \code{FALSE}.
#'
#' @param cor \code{Logical}. Include correlated process errors as part of a
#'   multivariate normal process model? If \code{TRUE} and if
#'   \code{n_series > 1} in the supplied data, a fully structured covariance
#'   matrix will be estimated for the process errors. Default is \code{FALSE}.
#'
#' @param p A non-negative integer specifying the autoregressive (AR) order.
#'   Default is \code{1}. Cannot currently be larger than \code{3} for `AR`
#'   terms, and cannot be anything other than `1` for continuous time AR
#'   (`CAR`) terms.
#'
#' @param gr An optional grouping variable, which must be a `factor` in the
#'   supplied `data`, for setting up hierarchical residual correlation
#'   structures. If specified, this will automatically set `cor = TRUE` and set
#'   up a model where the residual correlations for a specific level of `gr`
#'   are modelled hierarchically:
#'
#'   \eqn{\Omega_{group} = \alpha_{cor}\Omega_{global} +
#'   (1 - \alpha_{cor})\Omega_{group, local}},
#'
#'   where \eqn{\Omega_{global}} is a *global* correlation matrix,
#'   \eqn{\Omega_{group, local}} is a *local deviation* correlation matrix and
#'   \eqn{\alpha_{cor}} is a weighting parameter controlling how strongly the
#'   local correlation matrix \eqn{\Omega_{group}} is shrunk towards the global
#'   correlation matrix \eqn{\Omega_{global}} (larger values of
#'   \eqn{\alpha_{cor}} indicate a greater degree of shrinkage, i.e. a greater
#'   degree of partial pooling).
#'
#'   When used within a `VAR()` model, this essentially sets up a hierarchical
#'   panel vector autoregression where both the autoregressive and correlation
#'   matrices are learned hierarchically. If `gr` is supplied then `subgr`
#'   *must* also be supplied.
#'
#' @param subgr A subgrouping `factor` variable specifying which element in
#'   `data` represents the different time series. Defaults to `series`, but
#'   note that models that use the hierarchical correlations, where the
#'   `subgr` time series are measured in each level of `gr`, *should not*
#'   include a `series` element in `data`. Rather, this element will be created
#'   internally based on the supplied variables for `gr` and `subgr`.
#'
#'   For example, if you are modelling temporal counts for a group of species
#'   (labelled as `species` in `data`) across three different geographical
#'   regions (labelled as `region`), and you would like the residuals to be
#'   correlated within regions, then you should specify `gr = region` and
#'   `subgr = species`. Internally, `mvgam()` will create the `series` element
#'   for the data using:
#'
#'   `series = interaction(group, subgroup, drop = TRUE)`
#'
#' @return An object of class \code{mvgam_trend}, which contains a list of
#'   arguments to be interpreted by the parsing functions in \pkg{mvgam}.
#'
#' @rdname RW
#'
#' @details Use `vignette("mvgam_overview")` to see the full details of
#'   available stochastic trend types in \pkg{mvgam}, or view the rendered
#'   version on the package website at:
#'   https://nicholasjclark.github.io/mvgam/articles/mvgam_overview.html
#'
#' @author Nicholas J Clark
#'
#' @examples
#' \donttest{
#' # A short example to illustrate CAR(1) models
#' # Function to simulate CAR1 data with seasonality
#' sim_corcar1 = function(n = 125,
#'                        phi = 0.5,
#'                        sigma = 2,
#'                        sigma_obs = 0.75) {
#'   # Sample irregularly spaced time intervals
#'   time_dis <- c(1, runif(n - 1, 0, 5))
#'
#'   # Set up the latent dynamic process
#'   x <- vector(length = n); x[1] <- -0.3
#'   for (i in 2:n) {
#'     # zero-distances will cause problems in sampling, so mvgam uses a
#'     # minimum threshold; this simulation function emulates that process
#'     if (time_dis[i] == 0) {
#'       x[i] <- rnorm(
#'         1,
#'         mean = (phi^1e-3) * x[i - 1],
#'         sd = sigma * (1 - phi^(2 * 1e-3)) / (1 - phi^2)
#'       )
#'     } else {
#'       x[i] <- rnorm(
#'         1,
#'         mean = (phi^time_dis[i]) * x[i - 1],
#'         sd = sigma * (1 - phi^(2 * time_dis[i])) / (1 - phi^2)
#'       )
#'     }
#'   }
#'
#'   # Add 12-month seasonality
#'   cov1 <- sin(2 * pi * (1:n) / 12)
#'   cov2 <- cos(2 * pi * (1:n) / 12)
#'   beta1 <- runif(1, 0.3, 0.7)
#'   beta2 <- runif(1, 0.2, 0.5)
#'   seasonality <- beta1 * cov1 + beta2 * cov2
#'
#'   # Take Gaussian observations with error and return
#'   data.frame(
#'     y = rnorm(n, mean = x + seasonality, sd = sigma_obs),
#'     season = rep(1:12, 20)[1:n],
#'     time = cumsum(time_dis)
#'   )
#' }
#'
#' # Sample two time series
#' dat <- rbind(
#'   dplyr::bind_cols(
#'     sim_corcar1(phi = 0.65, sigma_obs = 0.55),
#'     data.frame(series = 'series1')
#'   ),
#'   dplyr::bind_cols(
#'     sim_corcar1(phi = 0.8, sigma_obs = 0.35),
#'     data.frame(series = 'series2')
#'   )
#' ) %>%
#'   dplyr::mutate(series = as.factor(series))
#'
#' # mvgam with CAR(1) trends and series-level seasonal smooths
#' mod <- mvgam(
#'   formula = y ~ -1,
#'   trend_formula = ~ s(season, bs = 'cc', k = 5, by = trend),
#'   trend_model = CAR(),
#'   priors = c(
#'     prior(exponential(3), class = sigma),
#'     prior(beta(4, 4), class = sigma_obs)
#'   ),
#'   data = dat,
#'   family = gaussian(),
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # View usual summaries and plots
#' summary(mod)
#' conditional_effects(mod, type = 'expected')
#' plot(mod, type = 'trend', series = 1)
#' plot(mod, type = 'trend', series = 2)
#' plot(mod, type = 'residuals', series = 1)
#' plot(mod, type = 'residuals', series = 2)
#' mcmc_plot(
#'   mod,
#'   variable = 'ar1',
#'   regex = TRUE,
#'   type = 'hist'
#' )
#'
#' # Now an example illustrating hierarchical dynamics
#' set.seed(123)
#'
#' # Simulate three species monitored in three different regions
#' simdat1 <- sim_mvgam(
#'   trend_model = VAR(cor = TRUE),
#'   prop_trend = 0.95,
#'   n_series = 3,
#'   mu = c(1, 2, 3)
#' )
#' simdat2 <- sim_mvgam(
#'   trend_model = VAR(cor = TRUE),
#'   prop_trend = 0.95,
#'   n_series = 3,
#'   mu = c(1, 2, 3)
#' )
#' simdat3 <- sim_mvgam(
#'   trend_model = VAR(cor = TRUE),
#'   prop_trend = 0.95,
#'   n_series = 3,
#'   mu = c(1, 2, 3)
#' )
#'
#' # Set up the data but DO NOT include 'series'
#' all_dat <- rbind(
#'   simdat1$data_train %>%
#'     dplyr::mutate(region = 'qld'),
#'   simdat2$data_train %>%
#'     dplyr::mutate(region = 'nsw'),
#'   simdat3$data_train %>%
#'     dplyr::mutate(region = 'vic')
#' ) %>%
#'   dplyr::mutate(
#'     species = gsub('series', 'species', series),
#'     species = as.factor(species),
#'     region = as.factor(region)
#'   ) %>%
#'   dplyr::arrange(series, time) %>%
#'   dplyr::select(-series)
#'
#' # Check priors for a hierarchical AR1 model
#' get_mvgam_priors(
#'   formula = y ~ species,
#'   trend_model = AR(gr = region, subgr = species),
#'   data = all_dat
#' )
#'
#' # Fit the model
#' mod <- mvgam(
#'   formula = y ~ species,
#'   trend_model = AR(gr = region, subgr = species),
#'   data = all_dat,
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # Check standard outputs
#' summary(mod)
#'
#' # Inspect posterior estimates for the correlation weighting parameter
#' mcmc_plot(mod, variable = 'alpha_cor', type = 'hist')
#' }
#' @export
RW = function(ma = FALSE, cor = FALSE, gr = NA, subgr = NA) {
  # Validate the supplied groupings and correlation argument
  gr <- deparse0(substitute(gr))
  subgr <- deparse0(substitute(subgr))

  if (gr == 'NA') {
    subgr <- 'series'
  }

  if (gr != 'NA') {
    if (subgr == 'NA') {
      stop(
        'argument "subgr" must be supplied if "gr" is also supplied',
        call. = FALSE
      )
    } else if (subgr == 'series') {
      stop(
        'argument "subgr" cannot be set to "series" if "gr" is also supplied',
        call. = FALSE
      )
    } else {
      cor <- TRUE
    }
  }

  out <- structure(
    list(
      trend_model = 'RW',
      ma = ma,
      cor = cor,
      unit = 'time',
      gr = gr,
      subgr = subgr,
      label = match.call()
    ),
    class = 'mvgam_trend',
    param_info = list(
      param_names = c(
        'trend',
        'tau',
        'sigma',
        'theta',
        'Sigma',
        'error',
        'drift'
      ),
      labels = c(
        'trend_estimates',
        'precision_parameter',
        'standard_deviation',
        'moving_average_coef',
        'covariance_matrix',
        'process_errors',
        'drift_parameter'
      )
    )
  )
}

#' @rdname RW
#' @export
AR = function(p = 1, ma = FALSE, cor = FALSE, gr = NA, subgr = NA) {
  validate_pos_integer(p)
  if (p > 3) {
    stop("Argument 'p' must be <= 3", call. = FALSE)
  }

  # Validate the supplied groupings and correlation argument
  gr <- deparse0(substitute(gr))
  subgr <- deparse0(substitute(subgr))

  if (gr == 'NA') {
    subgr <- 'series'
  }

  if (gr != 'NA') {
    if (subgr == 'NA') {
      stop(
        'argument "subgr" must be supplied if "gr" is also supplied',
        call. = FALSE
      )
    } else if (subgr == 'series') {
      stop(
        'argument "subgr" cannot be set to "series" if "gr" is also supplied',
        call. = FALSE
      )
    } else {
      cor <- TRUE
    }
  }

  # Determine parameter names based on AR order
  ar_params <- paste0('ar', 1:p)
  param_names <- c(
    'trend',
    'tau',
    'sigma',
    ar_params,
    'theta',
    'Sigma',
    'error',
    'drift'
  )
  param_labels <- c(
    'trend_estimates',
    'precision_parameter',
    'standard_deviation',
    paste0('autoregressive_coef_', 1:p),
    'moving_average_coef',
    'covariance_matrix',
    'process_errors',
    'drift_parameter'
  )

  out <- structure(
    list(
      trend_model = paste0('AR', p),
      ma = ma,
      cor = cor,
      unit = 'time',
      gr = gr,
      subgr = subgr,
      label = match.call()
    ),
    class = 'mvgam_trend',
    param_info = list(
      param_names = param_names,
      labels = param_labels
    )
  )
}

#' @rdname RW
#' @export
CAR = function(p = 1) {
  validate_pos_integer(p)
  if (p > 1) {
    stop("Argument 'p' must be = 1", call. = FALSE)
  }
  out <- structure(
    list(
      trend_model = paste0('CAR', p),
      ma = FALSE,
      cor = FALSE,
      unit = 'time',
      gr = 'NA',
      subgr = 'series',
      label = match.call()
    ),
    class = 'mvgam_trend',
    param_info = list(
      param_names = c('trend', 'tau', 'sigma', 'ar1', 'Sigma'),
      labels = c(
        'trend_estimates',
        'precision_parameter',
        'standard_deviation',
        'autoregressive_coef',
        'covariance_matrix'
      )
    )
  )
}

#' @rdname RW
#' @export
VAR = function(ma = FALSE, cor = FALSE, gr = NA, subgr = NA) {
  # Validate the supplied groupings and correlation argument
  gr <- deparse0(substitute(gr))
  subgr <- deparse0(substitute(subgr))

  if (gr == 'NA') {
    subgr <- 'series'
  }

  if (gr != 'NA') {
    if (subgr == 'NA') {
      stop(
        'argument "subgr" must be supplied if "gr" is also supplied',
        call. = FALSE
      )
    } else if (subgr == 'series') {
      stop(
        'argument "subgr" cannot be set to "series" if "gr" is also supplied',
        call. = FALSE
      )
    } else {
      cor <- TRUE
    }
  }

  out <- structure(
    list(
      trend_model = 'VAR',
      ma = ma,
      cor = cor,
      unit = 'time',
      gr = gr,
      subgr = subgr,
      label = match.call()
    ),
    class = 'mvgam_trend',
    param_info = list(
      param_names = c(
        'trend',
        'A',
        'Sigma',
        'P_real',
        'sigma',
        'theta',
        'error',
        'drift'
      ),
      labels = c(
        'trend_estimates',
        'var_coefficient_matrix',
        'covariance_matrix',
        'stationary_precision',
        'standard_deviation',
        'moving_average_matrix',
        'process_errors',
        'drift_parameter'
      )
    )
  )
}

#' Specify dynamic Gaussian process trends in \pkg{mvgam} models
#'
#' Set up low-rank approximate Gaussian Process trend models using Hilbert
#' basis expansions in \pkg{mvgam}. This function does not evaluate its
#' arguments – it exists purely to help set up a model with particular GP
#' trend models.
#'
#' @param ... unused
#'
#' @return An object of class \code{mvgam_trend}, which contains a list of
#'   arguments to be interpreted by the parsing functions in \pkg{mvgam}.
#'
#' @details A GP trend is estimated for each series using Hilbert space
#'   approximate Gaussian Processes. In `mvgam`, latent squared exponential GP
#'   trends are approximated using by default \code{20} basis functions and
#'   using a multiplicative factor of `c = 5/4`, which saves computational
#'   costs compared to fitting full GPs while adequately estimating GP
#'   \code{alpha} and \code{rho} parameters.
#'
#' @rdname GP
#'
#' @author Nicholas J Clark
#'
#' @references Riutort-Mayol G, Burkner PC, Andersen MR, Solin A and Vehtari A
#'   (2023). Practical Hilbert space approximate Bayesian Gaussian processes for
#'   probabilistic programming. Statistics and Computing 33, 1.
#'   https://doi.org/10.1007/s11222-022-10167-2
#'
#' @seealso \code{\link[brms]{gp}}
#'
#' @export
GP = function(...) {
  out <- structure(
    list(
      trend_model = 'GP',
      ma = FALSE,
      cor = FALSE,
      unit = 'time',
      gr = 'NA',
      subgr = 'series',
      label = match.call()
    ),
    class = 'mvgam_trend',
    param_info = list(
      param_names = c('trend', 'alpha_gp', 'rho_gp', 'b_gp'),
      labels = c(
        'trend_estimates',
        'marginal_deviation',
        'length_scale',
        'basis_coefficients'
      )
    )
  )
}

#' Specify piecewise linear or logistic trends in \pkg{mvgam} models
#'
#' Set up piecewise linear or logistic trend models in \code{mvgam}. These
#' functions do not evaluate their arguments – they exist purely to help set up
#' a model with particular piecewise trend models.
#'
#' @param n_changepoints A non-negative integer specifying the number of
#'   potential changepoints. Potential changepoints are selected uniformly from
#'   the first `changepoint_range` proportion of timepoints in \code{data}.
#'   Default is `10`.
#'
#' @param changepoint_range Proportion of history in \code{data} in which trend
#'   changepoints will be estimated. Defaults to `0.8` for the first 80%.
#'
#' @param changepoint_scale Parameter modulating the flexibility of the
#'   automatic changepoint selection by altering the scale parameter of a
#'   Laplace distribution. The resulting prior will be
#'   `double_exponential(0, changepoint_scale)`. Large values will allow many
#'   changepoints and a more flexible trend, while small values will allow few
#'   changepoints. Default is `0.05`.
#'
#' @param growth Character string specifying either `'linear'` or `'logistic'`
#'   growth of the trend. If `'logistic'`, a variable labelled `cap` MUST be in
#'   \code{data} to specify the maximum saturation point for the trend (see
#'   details and examples in \code{\link{mvgam}} for more information). Default
#'   is `'linear'`.
#'
#' @author Nicholas J Clark
#'
#' @references Taylor, Sean J., and Benjamin Letham. "Forecasting at scale."
#'   The American Statistician 72.1 (2018): 37–45.
#'
#' @return An object of class \code{mvgam_trend}, which contains a list of
#'   arguments to be interpreted by the parsing functions in \code{mvgam}.
#'
#' @details
#' *Offsets and intercepts*:
#' For each of these trend models, an offset parameter is included in the trend
#' estimation process. This parameter will be incredibly difficult to identify
#' if you also include an intercept in the observation formula. For that
#' reason, it is highly recommended that you drop the intercept from the
#' formula (i.e. `y ~ x + 0` or `y ~ x - 1`, where `x` are your optional
#' predictor terms).
#'
#' *Logistic growth and the cap variable*:
#' When forecasting growth, there is often some maximum achievable point that a
#' time series can reach. For example, total market size, total population size
#' or carrying capacity in population dynamics. It can be advantageous for the
#' forecast to saturate at or near this point so that predictions are more
#' sensible.
#'
#' This function allows you to make forecasts using a logistic growth trend
#' model, with a specified carrying capacity. Note that this capacity does not
#' need to be static over time; it can vary with each series × timepoint
#' combination if necessary. But you must supply a `cap` value for each
#' observation in the data when using `growth = 'logistic'`.
#'
#' For observation families that use a non-identity link function, the `cap`
#' value will be internally transformed to the link scale (i.e. your specified
#' `cap` will be log-transformed if you are using a `poisson()` or `nb()`
#' family). It is therefore important that you specify the `cap` values on the
#' scale of your outcome. Note also that no missing values are allowed in
#' `cap`.
#'
#' @rdname piecewise_trends
#'
#' @examples
#' \donttest{
#' # Example of logistic growth with possible changepoints
#' dNt = function(r, N, k) {
#'   r * N * (k - N)
#' }
#'
#' Nt = function(r, N, t, k) {
#'   for (i in 1:(t - 1)) {
#'     if (i %in% c(5, 15, 25, 41, 45, 60, 80)) {
#'       N[i + 1] <- max(
#'         1,
#'         N[i] + dNt(r + runif(1, -0.1, 0.1), N[i], k)
#'       )
#'     } else {
#'       N[i + 1] <- max(1, N[i] + dNt(r, N[i], k))
#'     }
#'   }
#'   N
#' }
#'
#' set.seed(11)
#' expected <- Nt(0.004, 2, 100, 30)
#' plot(expected, xlab = 'Time')
#'
#' y <- rpois(100, expected)
#' plot(y, xlab = 'Time')
#'
#' mod_data <- data.frame(
#'   y = y,
#'   time = 1:100,
#'   cap = 35,
#'   series = as.factor('series_1')
#' )
#' plot_mvgam_series(data = mod_data)
#'
#' mod <- mvgam(
#'   y ~ 0,
#'   trend_model = PW(growth = 'logistic'),
#'   family = poisson(),
#'   data = mod_data,
#'   chains = 2,
#'   silent = 2
#' )
#' summary(mod)
#'
#' hc <- hindcast(mod)
#' plot(hc)
#'
#' library(ggplot2)
#' mcmc_plot(mod, variable = 'delta_trend', regex = TRUE) +
#'   scale_y_discrete(labels = mod$trend_model$changepoints) +
#'   labs(
#'     y = 'Potential changepoint',
#'     x = 'Rate change'
#'   )
#'
#' how_to_cite(mod)
#' }
#'
#' @export
PW = function(
  n_changepoints = 10,
  changepoint_range = 0.8,
  changepoint_scale = 0.05,
  growth = 'linear'
) {
  growth <- match.arg(growth, choices = c('linear', 'logistic'))
  validate_proportional(changepoint_range)
  validate_pos_integer(n_changepoints)
  validate_pos_real(changepoint_scale)

  trend_model <- 'PWlinear'
  if (growth == 'logistic') {
    trend_model = 'PWlogistic'
  }
  out <- structure(
    list(
      trend_model = trend_model,
      n_changepoints = n_changepoints,
      changepoint_range = changepoint_range,
      changepoint_scale = changepoint_scale,
      ma = FALSE,
      cor = FALSE,
      unit = 'time',
      gr = 'NA',
      subgr = 'series',
      label = match.call()
    ),
    class = 'mvgam_trend',
    param_info = list(
      param_names = c('trend', 'delta_trend', 'k_trend', 'm_trend'),
      labels = c(
        'trend_estimates',
        'rate_changes',
        'growth_rate',
        'offset_parameter'
      )
    )
  )
}

#' Specify correlated residual processes in \pkg{mvgam}
#'
#' Set up latent correlated multivariate Gaussian residual processes in
#' \pkg{mvgam}. This function does not evaluate its arguments – it exists
#' purely to help set up a model with particular error processes
#'
#' @param unit The unquoted name of the variable that represents the unit of
#'   analysis in `data` over which latent residuals should be correlated. This
#'   variable should be either a `numeric` or `integer` variable in the
#'   supplied `data`. Defaults to `time` to be consistent with other
#'   functionalities in \pkg{mvgam}, though note that the data need not be time
#'   series in this case. See examples below for further details and
#'   explanations
#'
#' @param gr An optional grouping variable, which must be a `factor` in the
#'   supplied `data`, for setting up hierarchical residual correlation
#'   structures. If specified, this will automatically set up a model where the
#'   residual correlations for a specific level of `gr` are modelled
#'   hierarchically:
#'
#'   \eqn{\Omega_{group} = p\Omega_{global} + (1 - p)\Omega_{group, local}},
#'
#'   where \eqn{\Omega_{global}} is a *global* correlation matrix,
#'   \eqn{\Omega_{group, local}} is a *local deviation* correlation matrix, and
#'   \eqn{p} is a weighting parameter controlling how strongly the local
#'   correlation matrix \eqn{\Omega_{group}} is shrunk towards the global
#'   correlation matrix \eqn{\Omega_{global}}. If `gr` is supplied then `subgr`
#'   *must* also be supplied
#'
#' @param subgr A subgrouping `factor` variable specifying which element in
#'   `data` represents the different observational units. Defaults to `series`
#'   to be consistent with other functionalities in \pkg{mvgam}, though note
#'   that the data need not be time series in this case
#'
#'   Models that use the hierarchical correlations (by supplying a value for
#'   `gr`) *should not* include a `series` element in `data`. Rather, this
#'   element will be created internally based on the supplied variables for `gr`
#'   and `subgr`
#'
#'   For example, if you are modelling counts for a group of species (labelled
#'   as `species` in the data) across sampling sites (labelled as `site` in the
#'   data) in three different geographical regions (labelled as `region`), and
#'   you would like the residuals to be correlated within regions, then you
#'   should specify `unit = site`, `gr = region`, and `subgr = species`
#'
#'   Internally, `mvgam()` will appropriately order the data by `unit` (in this
#'   case, by `site`) and create the `series` element for the data using
#'   something like:
#'
#'   `series = as.factor(paste0(group, '_', subgroup))`
#'
#' @return An object of class \code{mvgam_trend}, which contains a list of
#'   arguments to be interpreted by the parsing functions in \pkg{mvgam}
#'
#' @examples
#' \donttest{
#' # Simulate counts of four species over ten sampling locations
#' site_dat <- data.frame(
#'   site = rep(1:10, 4),
#'   species = as.factor(sort(rep(letters[1:4], 10))),
#'   y = c(NA, rpois(39, 3))
#' )
#' head(site_dat)
#'
#' # Set up a correlated residual (i.e. Joint Species Distribution) model
#' trend_model <- ZMVN(unit = site, subgr = species)
#' mod <- mvgam(
#'   y ~ species,
#'   trend_model = ZMVN(unit = site, subgr = species),
#'   data = site_dat,
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # Inspect the estimated species-species residual covariances
#' mcmc_plot(mod, variable = 'Sigma', regex = TRUE, type = 'hist')
#'
#' # A hierarchical correlation example
#' Sigma <- matrix(
#'   c(1, -0.4, 0.5,
#'     -0.4, 1, 0.3,
#'     0.5, 0.3, 1),
#'   byrow = TRUE,
#'   nrow = 3
#' )
#'
#' make_site_dat <- function(...) {
#'   errors <- mgcv::rmvn(
#'     n = 30,
#'     mu = c(0.6, 0.8, 1.8),
#'     V = Sigma
#'   )
#'   site_dat <- do.call(rbind, lapply(1:3, function(spec) {
#'     data.frame(
#'       y = rpois(30, lambda = exp(errors[, spec])),
#'       species = paste0('species', spec),
#'       site = 1:30
#'     )
#'   }))
#'   site_dat
#' }
#'
#' site_dat <- rbind(
#'   make_site_dat() %>%
#'     dplyr::mutate(group = 'group1'),
#'   make_site_dat() %>%
#'     dplyr::mutate(group = 'group2')
#' ) %>%
#'   dplyr::mutate(
#'     species = as.factor(species),
#'     group = as.factor(group)
#'   )
#'
#' # Fit the hierarchical correlated residual model
#' mod <- mvgam(
#'   y ~ species,
#'   trend_model = ZMVN(unit = site, gr = group, subgr = species),
#'   data = site_dat,
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # Inspect the estimated species-species residual covariances
#' mcmc_plot(mod, variable = 'Sigma', regex = TRUE, type = 'hist')
#' }
#'
#' @export
ZMVN = function(unit = time, gr = NA, subgr = series) {
  # Validate the supplied groupings and correlation argument
  unit <- deparse0(substitute(unit))
  gr <- deparse0(substitute(gr))
  subgr <- deparse0(substitute(subgr))
  if (subgr == 'NA') {
    stop('argument "subgr" cannot be NA', call. = FALSE)
  }

  if (unit == 'NA') {
    stop('argument "unit" cannot be NA', call. = FALSE)
  }

  out <- structure(
    list(
      trend_model = 'ZMVN',
      ma = FALSE,
      cor = TRUE,
      unit = unit,
      gr = gr,
      subgr = subgr,
      label = match.call()
    ),
    class = 'mvgam_trend',
    param_info = list(
      param_names = c('trend', 'tau', 'sigma', 'theta', 'Sigma', 'error'),
      labels = c(
        'trend_estimates',
        'precision_parameter',
        'standard_deviation',
        'correlation_parameter',
        'covariance_matrix',
        'process_errors'
      )
    )
  )
}
