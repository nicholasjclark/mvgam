#' Specify autoregressive dynamic processes
#'
#' Set up autoregressive or autoregressive moving average trend models
#' in \code{mvgam}. These functions do not evaluate their arguments –
#' they exist purely to help set up a model with particular autoregressive
#' trend models.
#' @param ma \code{Logical} Include moving average terms of order \code{1}?
#' Default is \code{FALSE}.
#' @param cor \code{Logical} Include correlated process errors as part of a
#' multivariate normal process model? If \code{TRUE} and if \code{n_series > 1}
#' in the supplied data, a fully structured covariance matrix will be estimated
#' for the process errors. Default is \code{FALSE}.
#' @param p A non-negative integer specifying the autoregressive (AR) order.
#' Default is \code{1}. Cannot currently be larger than \code{3} for `AR` terms,
#' and cannot be anything other than `1` for continuous time AR (`CAR`) terms
#' @return An object of class \code{mvgam_trend}, which contains a list of
#' arguments to be interpreted by the parsing functions in \code{mvgam}
#' @rdname RW
#'@examples
#'\dontrun{
#'# A short example to illustrate CAR(1) models
#'# Function to simulate CAR1 data with seasonality
#'sim_corcar1 = function(n = 120,
#'                       phi = 0.5,
#'                       sigma = 1,
#'                       sigma_obs = 0.75){
#'# Sample irregularly spaced time intervals
#'time_dis <- c(0, runif(n - 1, -0.1, 1))
#'time_dis[time_dis < 0] <- 0; time_dis <- time_dis * 5
#'
#'# Set up the latent dynamic process
#'x <- vector(length = n); x[1] <- -0.3
#'for(i in 2:n){
#'  # zero-distances will cause problems in sampling, so mvgam uses a
#'  # minimum threshold; this simulation function emulates that process
#'  if(time_dis[i] == 0){
#'    x[i] <- rnorm(1, mean = (phi ^ 1e-12) * x[i - 1], sd = sigma)
#'   } else {
#'     x[i] <- rnorm(1, mean = (phi ^ time_dis[i]) * x[i - 1], sd = sigma)
#'   }
#' }
#'
#'# Add 12-month seasonality
#' cov1 <- sin(2 * pi * (1 : n) / 12); cov2 <- cos(2 * pi * (1 : n) / 12)
#' beta1 <- runif(1, 0.3, 0.7); beta2 <- runif(1, 0.2, 0.5)
#' seasonality <- beta1 * cov1 + beta2 * cov2
#'
#'# Take Gaussian observations with error and return
#' data.frame(y = rnorm(n, mean = x + seasonality, sd = sigma_obs),
#'            season = rep(1:12, 20)[1:n],
#'            time = cumsum(time_dis))
#'}
#'
#'# Sample two time series
#'dat <- rbind(dplyr::bind_cols(sim_corcar1(phi = 0.65,
#'                                          sigma_obs = 0.55),
#'                              data.frame(series = 'series1')),
#'             dplyr::bind_cols(sim_corcar1(phi = 0.8,
#'                              sigma_obs = 0.35),
#'                              data.frame(series = 'series2'))) %>%
#'       dplyr::mutate(series = as.factor(series))
#'
#'# mvgam with CAR(1) trends and series-level seasonal smooths; the
#'# State-Space representation (using trend_formula) will be more efficient
#'mod <- mvgam(formula = y ~ 1,
#'             trend_formula = ~ s(season, bs = 'cc',
#'                                 k = 5, by = trend),
#'             trend_model = CAR(),
#'             data = dat,
#'             family = gaussian(),
#'             samples = 300,
#'             chains = 2)
#'
#'# View usual summaries and plots
#'summary(mod)
#'conditional_effects(mod, type = 'expected')
#'plot(mod, type = 'trend', series = 1)
#'plot(mod, type = 'trend', series = 2)
#'plot(mod, type = 'residuals', series = 1)
#'plot(mod, type = 'residuals', series = 2)
#'}
#' @export
RW = function(ma = FALSE, cor = FALSE){
  out <- structure(list(trend_model = 'RW',
                        ma = ma,
                        cor = cor,
                        label = match.call()),
                   class = 'mvgam_trend')
}

#' @rdname RW
#' @export
AR = function(p = 1, ma = FALSE, cor = FALSE){
  validate_pos_integer(p)
  if(p > 3){
    stop("Argument 'p' must be <= 3",
         call. = FALSE)
  }
  out <- structure(list(trend_model = paste0('AR', p),
                        ma = ma,
                        cor = cor,
                        label = match.call()),
                   class = 'mvgam_trend')
}

#' @rdname RW
#' @export
CAR = function(p = 1){
  validate_pos_integer(p)
  if(p > 1){
    stop("Argument 'p' must be = 1",
         call. = FALSE)
  }
  out <- structure(list(trend_model = paste0('CAR', p),
                        ma = FALSE,
                        cor = FALSE,
                        label = match.call()),
                   class = 'mvgam_trend')
}

#' @rdname RW
#' @export
VAR = function(ma = FALSE, cor = FALSE){
  out <- structure(list(trend_model = 'VAR',
                        ma = ma,
                        cor = cor,
                        label = match.call()),
                   class = 'mvgam_trend')
}
