#'Simulate a set of discrete time series for mvgam modelling
#'
#'This function simulates discrete time series data for fitting a multivariate GAM that includes
#'shared seasonality and dependence on state-space latent dynamic factors. Random dependencies among series, i.e.
#'correlations in their long-term trends, are included in the form of correlated loadings on the latent dynamic factors
#'
#'@importFrom stats rnorm rbeta rpois rlnorm rgamma cor cov2cor cov stl ts
#'@importFrom brms lognormal
#'@importFrom Matrix nearPD
#'@param T \code{integer}. Number of observations (timepoints)
#'@param n_series \code{integer}. Number of discrete time series
#'@param seasonality \code{character}. Either \code{shared}, meaning that all series share the exact same seasonal pattern,
#'or \code{hierarchical}, meaning that there is a global seasonality but each series' pattern can deviate slightly
#'@param use_lv \code{logical}. If \code{TRUE}, use dynamic factors to estimate series'
#'latent trends in a reduced dimension format. If \code{FALSE}, estimate independent latent trends for each series
#'@param n_lv \code{integer}. Number of latent dynamic factors for generating the series' trends
#'@param trend_model \code{character} specifying the time series dynamics for the latent trend. Options are:
#'\itemize{
#'   \item `None` (no latent trend component; i.e. the GAM component is all that contributes to the linear predictor,
#'and the observation process is the only source of error; similarly to what is estimated by \code{\link[mgcv]{gam}})
#'   \item `RW` (random walk with possible drift)
#'   \item `AR1` (with possible drift)
#'   \item `AR2` (with possible drift)
#'   \item `AR3` (with possible drift)
#'   \item `VAR1` (contemporaneously uncorrelated VAR1)
#'   \item `VAR1cor` (contemporaneously correlated VAR1)
#'   \item `GP` (Gaussian Process with squared exponential kernel)} See [mvgam_trends] for more details
#'@param drift \code{logical}, simulate a drift term for each trend
#'@param prop_trend \code{numeric}. Relative importance of the trend for each series. Should be between \code{0} and \code{1}
#'@param trend_rel Deprecated. Use `prop_trend` instead
#'@param freq \code{integer}. The seasonal frequency of the series
#'@param family \code{family} specifying the exponential observation family for the series. Currently supported
#'families are: `nb()`, `poisson()`, `bernoulli()`, `tweedie()`, `gaussian()`,
#'`betar()`, `lognormal()`, `student()` and `Gamma()`
#'@param phi \code{vector} of dispersion parameters for the series
#'(i.e. `size` for `nb()` or
#'`phi` for `betar()`). If \code{length(phi) < n_series},
#'the first element of `phi` will
#'be replicated `n_series` times.
#'Defaults to \code{5} for `nb()` and `tweedie()`; \code{10} for
#'`betar()`
#'@param shape \code{vector} of shape parameters for the series
#'(i.e. `shape` for `gamma()`)
#'If \code{length(shape) < n_series}, the first element of `shape` will
#'be replicated `n_series` times. Defaults to \code{10}
#'@param sigma \code{vector} of scale parameters for the series
#'(i.e. `sd` for `gaussian()` or `student()`,
#'`log(sd)` for `lognormal()`). If \code{length(sigma) < n_series}, the first element of `sigma` will
#'be replicated `n_series` times. Defaults to \code{0.5} for `gaussian()` and
#'`student()`; \code{0.2} for `lognormal()`
#'@param nu \code{vector} of degrees of freedom parameters for the
#'series (i.e. `nu` for `student()`)
#'If \code{length(nu) < n_series}, the first element of `nu` will
#'be replicated `n_series` times. Defaults to \code{3}
#'@param mu \code{vector} of location parameters for the series. If \code{length(mu) < n_series}, the first element of `mu` will
#'be replicated `n_series` times. Defaults to small random values between `-0.5` and `0.5` on the link scale
#'@param prop_missing \code{numeric} stating proportion of observations that are missing. Should be between
#'\code{0} and \code{0.8}, inclusive
#'@param prop_train \code{numeric} stating the proportion of data to use for training. Should be between \code{0.2} and \code{1}
#'@return A \code{list} object containing outputs needed for \code{\link{mvgam}}, including 'data_train' and 'data_test',
#'as well as some additional information about the simulated seasonality and trend dependencies
#'@examples
#'# Simulate series with observations bounded at 0 and 1 (Beta responses)
#'sim_data <- sim_mvgam(family = betar(), trend_model = RW(), prop_trend = 0.6)
#'plot_mvgam_series(data = sim_data$data_train, series = 'all')
#'
#'# Now simulate series with overdispersed discrete observations
#'sim_data <- sim_mvgam(family = nb(), trend_model = RW(), prop_trend = 0.6, phi = 10)
#'plot_mvgam_series(data = sim_data$data_train, series = 'all')
#'@export

sim_mvgam = function(T = 100,
                     n_series = 3,
                     seasonality = 'shared',
                     use_lv = FALSE,
                     n_lv = 1,
                     trend_model = 'RW',
                     drift = FALSE,
                     prop_trend = 0.2,
                     trend_rel,
                     freq = 12,
                     family = poisson(),
                     phi,
                     shape,
                     sigma,
                     nu,
                     mu,
                     prop_missing = 0,
                     prop_train = 0.85){

  # Validate the family argument
  family <- validate_family(family)
  family_char <- match.arg(arg = family$family,
                           choices = c('negative binomial',
                                       "poisson",
                                       "bernoulli",
                                       "tweedie",
                                       "beta",
                                       "gaussian",
                                       "lognormal",
                                       "student",
                                       "Gamma"))

  # Validate the trend arguments
  trend_model <- validate_trend_model(trend_model, drift = drift)
  if(trend_model %in% c('VAR1', 'VAR1cor')){
    use_lv <- FALSE
  }

  if(trend_model %in% c('RWcor', 'AR1cor', 'AR2cor', 'AR3cor')){
    warning(paste0('Simulation of correlated AR or RW trends not yet supported.\n',
                   'Reverting to uncorrelated trends'))
  }

  if(missing(trend_rel)){
    trend_rel <- prop_trend
  }
  validate_proportional(trend_rel)

  # Check n_series
  validate_pos_integer(n_series)

  # Check prop_missing
  validate_proportional(prop_missing)

  # Check n_lv
  validate_pos_integer(n_lv)

  if(n_lv == 1){
    use_lv <- FALSE
  } else {
    use_lv <- TRUE
  }

  if(use_lv){
    if(n_lv > n_series){
      warning('Argument "n_lv" cannot be greater than n_series; changing n_lv to match n_series')
      n_lv <- n_series
    }
  } else {
    n_lv <- n_series
  }

  # Check seasonality
  if(!seasonality %in% c('shared', 'hierarchical')){
    stop('seasonality must be either shared or hierarchical')
  }

  # Check family-specific parameters
  if(missing(phi)){
    if(family_char == 'beta'){
      phi <- rep(10, n_series)
    } else {
      phi <- rep(5, n_series)
    }
  }

  if(any(phi <= 0)){
    stop('Argument "phi" must be a non-negative real number',
         call. = FALSE)
  }

  if(missing(shape)){
    shape <- rep(1, n_series)
  }

  if(any(shape <= 0)){
    stop('Argument "shape" must be a non-negative real number',
         call. = FALSE)
  }

  if(missing(sigma)){
    if(family_char == 'lognormal'){
      sigma <- rep(0.2, n_series)
    } else {
      sigma <- rep(0.5, n_series)
    }
  }

  if(any(sigma <= 0)){
    stop('Argument "sigma" must be a non-negative real number',
         call. = FALSE)
  }

  if(missing(nu)){
    nu <- rep(3, n_series)
  }

  if(any(nu <= 0)){
    stop('Argument "nu" must be a non-negative real number',
         call. = FALSE)
  }

  if(missing(mu)){
    mu <- sample(seq(-0.5, 0.5), n_series, TRUE)
  }

  if(length(phi) < n_series){
    phi <- rep(phi[1], n_series)
  }

  if(length(shape) < n_series){
    shape <- rep(shape[1], n_series)
  }

  if(length(sigma) < n_series){
    sigma <- rep(sigma[1], n_series)
  }

  if(length(nu) < n_series){
    nu <- rep(nu[1], n_series)
  }

  if(length(mu) < n_series){
    mu <- rep(mu[1], n_series)
  }

  # Check data splitting
  if(missing(prop_train)){
    prop_train <- 0.75
  }
  if(prop_train < 0.2 || prop_train > 1){
    stop('Argument "prop_train" must be a proportion ranging from 0.2 to 1, inclusive',
         call. = FALSE)
  }

  # Set trend parameters
  if(trend_model %in% c('RW', 'RWcor')){
    ar1s <- rep(1, n_lv)
    ar2s <- rep(0, n_lv)
    ar3s <- rep(0, n_lv)
  }

  if(trend_model %in% c('AR1', 'AR1cor')){
    ar1s <- rnorm(n_lv, sd = 0.5)
    ar2s <- rep(0, n_lv)
    ar3s <- rep(0, n_lv)
  }

  if(trend_model %in% c('AR2', 'AR2cor')){
    ar1s <- rnorm(n_lv, sd = 0.5)
    ar2s <- rnorm(n_lv, sd = 0.5)
    ar3s <- rep(0, n_lv)
  }

  if(trend_model %in% c('AR3', 'AR3cor')){
    ar1s <- rnorm(n_lv, sd = 0.5)
    ar2s <- rnorm(n_lv, sd = 0.5)
    ar3s <- rnorm(n_lv, sd = 0.5)
  }

  if(trend_model %in% c('RW', 'AR1', 'AR2', 'AR3',
                        'VAR1', 'VAR1cor')){
    # Sample trend drift terms so they are (hopefully) not too correlated
    if(drift){
      trend_alphas <- rnorm(n_lv, sd = 0.15)
    } else {
      trend_alphas <- rep(0, n_lv)
    }

    # Simulate latent trends
    if(!trend_model %in% c('VAR1', 'VAR1cor')){
      trends <- do.call(cbind, lapply(seq_len(n_lv), function(x){
        sim_ar3(drift = trend_alphas[x],
                ar1 = ar1s[x],
                ar2 = ar2s[x],
                ar3 = ar3s[x],
                tau = 1,
                last_trends = rnorm(3),
                h = T)
      }))
    }

    if(trend_model %in% c('VAR1', 'VAR1cor')){
      if(trend_model == 'VAR1'){
        # Simulate the Sigma matrix (contemporaneously uncorrelated)
        Sigma <- matrix(0, n_lv, n_lv)
        sigma <- runif(n_lv, 0.4, 1.2)
        diag(Sigma) <- sigma
      }

      if(trend_model == 'VAR1cor'){
        # Use the LKJ distribution to sample correlation matrices
        # with nice properties
        lkj_corr <- function(n_series, eta = 0.8) {
          alpha <- eta + (n_series - 2)/2
          r12 <- 2 * rbeta(1, alpha, alpha) - 1
          R <- matrix(0, n_series, n_series)
          R[1,1] <- 1; R[1,2] <- r12; R[2,2] <- sqrt(1 - r12^2)
          if(n_series > 2) for (m in 2:(n_series - 1)){
            alpha <- alpha - 0.5
            y <- rbeta(1, m / 2, alpha)
            z <- rnorm(m, 0, 1)
            z <- z / sqrt(crossprod(z)[1])
            R[1:m,m+1] <- sqrt(y) * z
            R[m+1,m+1] <- sqrt(1 - y)
          }
          return(crossprod(R))
        }
        # Sample trend SD parameters and construct Sigma
        sigma <- runif(n_lv, 0.4, 1.2)
        Sigma <- outer(sigma, sigma) * lkj_corr(n_series = n_lv)
      }

      # Create a stationary VAR coefficient matrix
      A <- stationary_VAR_phi(p = 1, n_series = n_lv)[[1]]

      # Simulate the VAR trends
      trends <- sim_var1(drift = trend_alphas,
                         A = A,
                         Sigma = Sigma,
                         last_trends = mvnfast::rmvn(n = 1,
                                                     mu = rep(0, n_lv),
                                                     sigma = Sigma),
                         h = T)
    }

  }

  if(trend_model == 'GP'){

    # Sample alpha and rho parameters
    trend_alphas <- runif(n_lv, 0.75, 1.25)
    trend_rhos <- runif(n_lv, 3, 8)

    # Generate latent GP trends
    trends <- do.call(cbind, lapply(seq_len(n_lv), function(lv){

      Sigma <- trend_alphas[lv]^2 *
        exp(-0.5 * ((outer(1:T, 1:T, "-") / trend_rhos[lv]) ^ 2)) +
        diag(1e-9, T)
      mvnfast::rmvn(1,
                    mu = rep(0, T),
                    sigma = Sigma)[1,]
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
    loadings <- matrix(mvnfast::rmvn(n = n_lv, mu = rep(0, n_series),
                                     sigma = as.matrix(Matrix::nearPD(Sigma)$mat)),
                       ncol = n_series)

  } else {
    # Else use independent trend loadings
    loadings <- diag(n_lv)
  }

  # Simulate the global seasonal pattern
  stl_season <- stl(smooth::sim.es(model = "ANA" ,frequency = freq, obs = T + 5,
                               randomizer = "rnorm")$data, 'periodic')$time.series[,1]
  glob_season <- as.vector(scale(zoo::rollmean(stl_season, k = 6, na.pad = F)))

  # Simulate observed series as dependent on seasonality and trend
  obs_trends <- matrix(NA, nrow = T, ncol = n_series)
   for(s in 1:n_series){
      obs_trends[,s] <- as.vector(scale(as.vector(loadings[,s] %*% t(trends))))
   }

  obs_ys <- c(unlist(lapply(seq_len(n_series), function(x){
    if(seasonality == 'shared'){

      dynamics <- (glob_season * (1 - trend_rel)) +
        (obs_trends[,x] * trend_rel)

    } else {
      yseason <- as.vector(scale(stl(ts(rnorm(T, glob_season, sd = 2),
                                        frequency = freq), 'periodic')$time.series[,1]))
      dynamics <- (yseason * (1 - trend_rel)) +
        (obs_trends[,x] * trend_rel)
    }

    if(family_char == 'negative binomial'){
      out <- rnbinom(length(dynamics), size = phi[x],
                     mu = exp(mu[x] + dynamics))
    }

    if(family_char == 'poisson'){
      out <- rpois(length(dynamics),
                   lambda = exp(mu[x] + dynamics))
    }

    if(family_char == 'bernoulli'){
      out <- rbinom(length(dynamics),
                    size = 1,
                    prob = plogis(mu[x] + dynamics))
    }

    if(family_char == 'tweedie'){
      out <- rpois(n = length(dynamics),
                   lambda = tweedie::rtweedie(length(dynamics),
                                              mu = exp(mu[x] + dynamics),
                                              power = 1.5, phi = phi[x]))
    }

    if(family_char == 'gaussian'){
      out <- rnorm(length(dynamics),
                   mean = mu[x] + dynamics,
                   sd = sigma[x])
    }

    if(family_char == 'student'){
      out <- rstudent_t(n = length(dynamics),
                        df = nu[x],
                        mu = mu[x] + dynamics,
                        sigma = sigma[x])
    }

    if(family_char == 'lognormal'){
      out <- rlnorm(length(dynamics),
                   meanlog = mu[x] + (dynamics * 0.3),
                   sdlog = sigma[x])
    }

    if(family_char == 'Gamma'){
      out <- rgamma(length(dynamics),
                    rate = shape[x] / exp(mu[x] + dynamics),
                    shape = shape[x])
    }

    if(family_char == 'beta'){
      shape_pars <- beta_shapes(mu = plogis(mu[x] + dynamics),
                                     phi = phi[x])
      out <- rbeta(length(dynamics),
                   shape1 = shape_pars$shape1,
                   shape2 = shape_pars$shape2)
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
    dplyr::filter(time <= floor(max(sim_data$time) * prop_train)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(series) %>%
    dplyr::arrange(time)

  data_test <- sim_data %>%
    dplyr::filter(time > max(data_train$time)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(series) %>%
    dplyr::arrange(time)


  if(!use_lv){

    if(trend_model %in% c('RW', 'AR1', 'AR2', 'AR3')){
      trend_params = list(ar1 = ar1s,
                          ar2 = ar2s,
                          ar3 = ar3s)
    }

    if(trend_model %in% c('VAR1', 'VAR1cor')){
      trend_params = list(var1 = A,
                          Sigma = Sigma)
    }

    if(trend_model == 'GP'){
      trend_params = list(alpha = trend_alphas,
                          rho = trend_rhos)
    }

    out <- list(data_train = data.frame(data_train),
                data_test = data.frame(data_test),
                true_corrs = cov2cor(cov(obs_trends)),
                true_trends = obs_trends,
                global_seasonality = glob_season,
                trend_params = trend_params)
  } else {
    out <- list(data_train = data.frame(data_train),
                data_test = data.frame(data_test),
                true_corrs = cov2cor(cov(obs_trends)),
                true_trends = obs_trends,
                global_seasonality = glob_season)
  }

return(out)

}
