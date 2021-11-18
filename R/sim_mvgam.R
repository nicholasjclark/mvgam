#'Fit a Bayesian multivariate GAM to a set of discrete time series
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
#'@param n_trends \code{integer}. Number of latent dynamic factors (as random walks with drift) for generating the series' trends
#'@param trend_rel \code{numeric}. Relative importance of the trend for each series. Should be between \code{0} and \code{1}
#'@param freq \code{integer}. The seasonal frequency of the series
#'@param size_obs \code{vector} of negative binomial size parameters for the series
#'@param mu_obs \code{vector} of negative binomial mu parameters for the series
#'@param prop_missing \code{numeric} stating proportion of observations that are missing
#'@param train_prop \code{numeric} stating the proportion of data to use for training. Should be between \code{0.25} and \code{0.75}
#'@return A \code{list} object containing outputs needed for \code{mvjagam}, including 'data_train' and 'data_test',
#'as well as some additional information about the simulated seasonality and trend dependencies
#'@export

sim_mvgam = function(T = 100,
                     n_series = 3,
                     seasonality = 'shared',
                     n_trends = 5,
                     trend_rel,
                     freq = 12,
                     size_obs,
                     mu_obs,
                     prop_missing = 0,
                     train_prop){

  # Sample trend drift terms so they are (hopefully) not too correlated
  trend_alphas <- rnorm(n_trends, sd = 0.15)

  if(missing(trend_rel)){
    trend_rel <- 0.2
  }

  if(!seasonality %in% c('shared', 'hierarchical')){
    stop('seasonality must be either shared or hierarchical')
  }

  if(missing(size_obs)){
    size_obs <- rep(1, n_series)
  }

  if(missing(mu_obs)){
    mu_obs <- sample(seq(3, 7), n_series, T)
  }

  if(missing(train_prop)){
    train_prop <- 0.75
  }

  # Simulate the long-term trends, which evolve as random walks + drift
  trends <- do.call(cbind, lapply(seq_len(n_trends), function(x){
    trend <- rep(NA, length = T + 2)
    trend[1] <- trend_alphas[x]
    for (t in 2:(T+2)) {
      trend[t] <- rnorm(
        1,
        trend_alphas[x] +  trend[t - 1],
        1
      )
    }
    as.vector(scale(zoo::rollmean(as.vector(scale(trend)), k = 3, na.pad = F)))
  }))

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
  loadings <- MASS::mvrnorm(n = n_trends, mu = rep(0, n_series),
                            Sigma = as.matrix(Matrix::nearPD(Sigma)$mat))

  # Simulate the shared seasonal pattern
  stl_season <- stl(smooth::sim.es(model="ANA",frequency=freq,obs = T + 5,
                               randomizer="rnorm")$data, 'periodic')$time.series[,1]
  glob_season <- as.vector(scale(zoo::rollmean(stl_season, k = 6, na.pad = F)))

  # Simulate observed series as negative binomial draws dependent on seasonality and trend
  invlogit = function(x){
    exp(x)/(1+exp(x))
  }
  scale01 <- function(x){
    x = (x-min(x))/(max(x)-min(x))
    x[x == 0] <- 0.0001
    x[x == 1] <- 0.9999
    x
  }

  obs_trends <- matrix(NA, nrow = T, ncol = n_series)
   for(s in 1:n_series){
      obs_trends[,s] <- as.vector(loadings[,s] %*%  t(trends))
   }

  obs_ys <- c(unlist(lapply(seq_len(n_series), function(x){
    if(seasonality == 'shared'){
      obs <- scale01(invlogit(as.vector(scale(as.vector(loadings[,x] %*%  t(trends))) * trend_rel) +
                                glob_season * (1 - trend_rel)))
    } else {
      yseason <- as.vector(scale(stl(ts(rnorm(T, glob_season, sd = 3), frequency = freq), 'periodic')$time.series[,1]))
      obs <- scale01(invlogit(as.vector(scale(as.vector(loadings[,x] %*%  t(trends))) * trend_rel) +
                                 yseason * (1 - trend_rel)))
    }
    out <- qnbinom(obs, size = size_obs[x], mu = mu_obs[x])
    out[is.infinite(out)] <- NA
    if(prop_missing > 0){
      out[sample(seq(1, length(out)), floor(length(out) * prop_missing))] <- NA
    }
    out
  })))

  # Return simulated data in the format that is ready for mvjagam analysis
  sim_data = data.frame(y = obs_ys,
                        season = rep(rep(seq(1, freq), ceiling(T/freq))[1:T], n_series),
                        year = rep(sort(rep(seq(1, ceiling(T/freq)), freq))[1:T], n_series),
                        series = as.factor(paste0('series_', sort(rep(seq(1, n_series), T)))),
                        in_season = 1) %>%
    dplyr::arrange(year, season, series)

  list(data_train = sim_data[1:(floor(nrow(sim_data) * train_prop)),],
       data_test = sim_data[((floor(nrow(sim_data) * train_prop)) + 1):nrow(sim_data),],
       true_corrs = cov2cor(cov(obs_trends)),
       global_seasonality = glob_season)

}
