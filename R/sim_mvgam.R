#'Simulate a set of discrete time series for mvgam modelling
#'
#'This function simulates discrete time series data for fitting a multivariate GAM that includes
#'shared seasonality and dependence on state-space latent dynamic factors. Random dependencies among series, i.e.
#'correlations in their long-term trends, are included in the form of correlated loadings on the latent dynamic factors
#'
#'
#'@param T \code{integer}. Number of observations (timepoints)
#'@param n_series \code{integer}. Number of discrete time series
#'@param seasonality \code{character}. Either \code{shared}, meaning that all series share the exact same seasonal pattern,
#'or \code{hierarchical}, meaning that there is a global seasonality but each series' pattern can deviate slightly
#'@param use_lv \code{logical}. If \code{TRUE}, use dynamic factors to estimate series'
#'latent trends in a reduced dimension format. If \code{FALSE}, estimate independent latent trends for each series
#'@param n_lv \code{integer}. Number of latent dynamic factors for generating the series' trends
#'@param trend_model \code{character} specifying the time series dynamics for the latent trends. Options are:
#''RW' (random walk with possible drift),
#''AR1' (AR1 model with intercept),
#''AR2' (AR2 model with intercept) or
#''AR3' (AR3 model with intercept) or
#''GP' (Gaussian process with squared exponential kernel
#'@param drift \code{logical}, simulate a drift term for each trend
#'@param trend_rel \code{numeric}. Relative importance of the trend for each series. Should be between \code{0} and \code{1}
#'@param freq \code{integer}. The seasonal frequency of the series
#'@param family \code{character} specifying the exponential observation family for the series. Must be either
#''nb' (for Negative Binomial), 'tw' (for Tweedie) or 'poisson'
#'@param phi_obs \code{vector} of dispersion parameters for the series (i.e. `size` for Negative Binomial or
#'`phi` for Tweedie; ignored for Poisson). If \code{length(phi_obs) < n_series}, the first element of `phi_obs` will
#'be replicated `n_series` times
#'@param mu_obs \code{vector} of location parameters for the series. If \code{length(mu_obs) < n_series}, the first element of `mu_obs` will
#'be replicated `n_series` times
#'@param prop_missing \code{numeric} stating proportion of observations that are missing
#'@param train_prop \code{numeric} stating the proportion of data to use for training. Should be between \code{0.25} and \code{0.75}
#'@return A \code{list} object containing outputs needed for \code{\link{mvgam}}, including 'data_train' and 'data_test',
#'as well as some additional information about the simulated seasonality and trend dependencies
#'@export

sim_mvgam = function(T = 100,
                     n_series = 3,
                     seasonality = 'shared',
                     use_lv = FALSE,
                     n_lv = 2,
                     trend_model = 'RW',
                     drift = FALSE,
                     trend_rel = 0.2,
                     freq = 12,
                     family = 'poisson',
                     phi_obs,
                     mu_obs = 4,
                     prop_missing = 0,
                     train_prop = 0.85){

  # Check arguments
  family <- match.arg(arg = family, choices = c("nb", "poisson", "tw"))
  trend_model <- match.arg(arg = trend_model, choices = c("RW", "GP", 'AR1', 'AR2', 'AR3'))

  if(missing(trend_rel)){
    trend_rel <- 0.2
  }

  if(use_lv){
    if(n_lv > n_series){
      warning('n_lv cannot be greater than n_series; changing n_lv to match n_series')
      n_lv <- n_series
    }
  } else {
    n_lv <- n_series
  }

  if(!seasonality %in% c('shared', 'hierarchical')){
    stop('seasonality must be either shared or hierarchical')
  }

  if(missing(phi_obs)){
    phi_obs <- rep(1, n_series)
  }

  if(missing(mu_obs)){
    mu_obs <- sample(seq(2, 6), n_series, T)
  }

  if(missing(train_prop)){
    train_prop <- 0.75
  }

  if(length(phi_obs) < n_series){
    phi_obs <- rep(phi_obs[1], n_series)
  }

  if(length(mu_obs) < n_series){
    mu_obs <- rep(mu_obs[1], n_series)
  }

  if(trend_model == 'RW'){
    ar1s <- rep(1, n_lv)
    ar2s <- rep(0, n_lv)
    ar3s <- rep(0, n_lv)
  }

  if(trend_model == 'AR1'){
    ar1s <- rnorm(n_lv, sd = 0.5)
    ar2s <- rep(0, n_lv)
    ar3s <- rep(0, n_lv)
  }

  if(trend_model == 'AR2'){
    ar1s <- rnorm(n_lv, sd = 0.5)
    ar2s <- rnorm(n_lv, sd = 0.5)
    ar3s <- rep(0, n_lv)
  }

  if(trend_model == 'AR3'){
    ar1s <- rnorm(n_lv, sd = 0.5)
    ar2s <- rnorm(n_lv, sd = 0.5)
    ar3s <- rnorm(n_lv, sd = 0.5)
  }

  if(trend_model %in% c('RW', 'AR1', 'AR2', 'AR3')){
    # Sample trend drift terms so they are (hopefully) not too correlated
    if(drift){
      trend_alphas <- rnorm(n_lv, sd = 0.15)
    } else {
      trend_alphas <- rep(0, n_lv)
    }

    # Function to simulate trends ahead using ar3 model
    sim_ar3 = function(phi, ar1, ar2, ar3, T){
      states <- rep(NA, length = T + 3)
      inits <- cumsum(rnorm(3, 0, 0.1))
      states[1] <- inits[1]
      states[2] <- inits[2]
      states[3] <- inits[3]
      for (t in 4:(T + 3)) {
        states[t] <- rnorm(1, phi + ar1*states[t - 1] +
                             ar2*states[t - 2] +
                             ar3*states[t - 3], 1)
      }
      states[-c(1:3)]
    }

    # Simulate latent trends
    trends <- do.call(cbind, lapply(seq_len(n_lv), function(x){
      sim_ar3(phi = trend_alphas[x],
              ar1 = ar1s[x],
              ar2 = ar2s[x],
              ar3 = ar3s[x],
              T = T)
    }))
  }

  if(trend_model == 'GP'){

    # Function to simulate from a latent GP with squared exponential covariance
    sim_exp_gp = function(N = 50, alpha = 1, rho = 2){

      # Squared exponential kernel function
      exp_kernel <- function(x, y, alpha = 1, rho = 1) {
        alpha^2 * exp(- (x - y)^2 / (2 * rho^2))
      }

      # Generate covariance matrix for points in sequence `x`
      cov_matrix <- function(x, kernel_fn, ...) {
        outer(x, x, function(a, b) kernel_fn(a, b, ...))
      }

      # Draw from kernel function
      draw_samples <- function(x, kernel_fn, ...) {
        K <- cov_matrix(x, kernel_fn, ...)
        MASS::mvrnorm(1, mu = rep(0, times = length(x)), Sigma = K)
      }

      # Return evenly spaced draws
      gp_true <- draw_samples(x = seq(0, N, length.out = N * 10),
                              kernel_fn = exp_kernel,
                              alpha = alpha,
                              rho = rho)

      gp_true[seq(1, length(gp_true), 10)]
    }

    # Sample alpha and rho parameters
    trend_alphas <- runif(n_lv, 0.75, 1.2)
    trend_rhos <- runif(n_lv, 6, 12)

    # Generate latent GP trends
    trends <- do.call(cbind, lapply(seq_len(n_lv), function(lv){
      set.seed(runif(1, 1, 100))
      sim_exp_gp(N = T, alpha = trend_alphas[lv], rho = trend_rhos[lv])
    }))

  }

  if(use_lv){
    # Loadings on the trends for each series, with sparse but strong correlations
    sparse = function(n){
      x <- rep(0, n)
      x[sample(seq(1, length(x)), ceiling(0.15*length(x)))] <-
        sample(c(-0.75, -0.25, 0.25, 0.75), ceiling(0.15*length(x)), T)
      x
    }
    corMat <- matrix(sparse(n_series ^ 2), n_series)
    corMat[lower.tri(corMat)] = t(corMat)[lower.tri(corMat)]
    diag(corMat) <- 1
    stddev <- rep(1, n_series)
    Sigma <- stddev %*% t(stddev) * corMat
    loadings <- matrix(MASS::mvrnorm(n = n_lv, mu = rep(0, n_series),
                                     Sigma = as.matrix(Matrix::nearPD(Sigma)$mat)),
                       ncol = n_series)

  } else {
    # Else use independent trend loadings
    loadings <- diag(n_lv)
  }

  # Simulate the global seasonal pattern
  stl_season <- stl(smooth::sim.es(model = "ANA" ,frequency = freq, obs = T + 5,
                               randomizer = "rnorm")$data, 'periodic')$time.series[,1]
  glob_season <- as.vector(scale(zoo::rollmean(stl_season, k = 6, na.pad = F)))

  # Simulate observed series as discrete draws dependent on seasonality and trend
  obs_trends <- matrix(NA, nrow = T, ncol = n_series)
   for(s in 1:n_series){
      obs_trends[,s] <- as.vector(scale(as.vector(loadings[,s] %*% t(trends))))
   }

  obs_ys <- c(unlist(lapply(seq_len(n_series), function(x){
    if(seasonality == 'shared'){
      obs <- exp(log(mu_obs[x]) + (glob_season * (1-trend_rel)) +
                   (obs_trends[,x] * trend_rel))


    } else {
      yseason <- as.vector(scale(stl(ts(rnorm(T, glob_season, sd = 2),
                                        frequency = freq), 'periodic')$time.series[,1]))
      obs <- exp(log(mu_obs[x]) + (yseason * (1-trend_rel)) +
            (obs_trends[,x] * trend_rel))
    }

    if(family == 'nb'){
      out <- rnbinom(length(obs), size = phi_obs[x],
                     mu = obs)
    }

    if(family == 'poisson'){
      out <- rpois(length(obs), lambda = obs)
    }

    if(family == 'tw'){
      out <- rpois(n = length(obs),
                   lambda = tweedie::rtweedie(length(obs), mu = obs,
                                              power = 1.5, phi = phi_obs[x]))
    }

    out[is.infinite(out)] <- NA
    if(prop_missing > 0){
      out[sample(seq(1, length(out)), floor(length(out) * prop_missing))] <- NA
    }
    out
  })))

  # Return simulated data in the format that is ready for mvgam analysis
  sim_data = data.frame(y = obs_ys,
                        season = rep(rep(seq(1, freq), ceiling(T/freq))[1:T], n_series),
                        year = rep(sort(rep(seq(1, ceiling(T/freq)), freq))[1:T], n_series),
                        series = as.factor(paste0('series_', sort(rep(seq(1, n_series), T))))) %>%
    dplyr::group_by(series) %>%
    dplyr::arrange(year, season) %>%
    dplyr::mutate(time = 1:dplyr::n()) %>%
    dplyr::ungroup()

  data_train <- sim_data %>%
    dplyr::filter(time <= floor(max(sim_data$time) * train_prop)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(series) %>%
    dplyr::arrange(time)

  data_test <- sim_data %>%
    dplyr::filter(time > max(data_train$time)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(series) %>%
    dplyr::arrange(time)

  list(data_train = data.frame(data_train),
       data_test = data.frame(data_test),
       true_corrs = cov2cor(cov(obs_trends)),
       true_trends = obs_trends,
       global_seasonality = glob_season)

}
