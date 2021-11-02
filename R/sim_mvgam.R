#'Fit a Bayesian multivariate GAM to a set of discrete time series
#'
#'This function estimates the posterior distribution for a multivariate GAM that includes
#'smooth seasonality and possible other smooth functions specified in the GAM formula. State-space latent trends
#'are estimated for each series, with a number of options for specifying the structures of the trends
#'
#'
#'@param T \code{integer}. Number of observations (timepoints)
#'@param n_series \code{integer}. Number of discrete time series
#'@n_trends \code{integer}. Number of latent dynamic factors (as random walks with drift) for generating the series' trends
#'@trend_alphas \code{vector} of drift terms for the latent factors
#'@param trend_rel \code{numeric}. Relative importance of the trend for each series. Should be between \code{0} and \code{1}
#'@param freq \code{integer}. The seasonal frequency of the series
#'@param size_obs \code{vector} of negative binomial size parameters for the series
#'@param mu_obs \code{vector} of negative binomial mu parameters for the series
#'@param train_prop \code{numeric} stating the proportion of data to use for training. Should be between \code{0.25} and \code{0.75}
#'@export

sim_mvgam = function(T = 100,
                     n_series = 3,
                     n_trends = 2,
                     trend_alphas = rep(0, 2),
                     # relative impact of trend
                     trend_rel = .2,
                     freq = 12,
                     size_obs = c(0.5, 0.5, 1),
                     mu_obs = c(3, 4, 5),
                     train_prop = 0.7){

  # Simulate the long-term trends
  trends <- do.call(cbind, lapply(seq_len(n_trends), function(x){
    trend <- rep(NA, length = T)
    trend[1] <- trend_alphas[x]
    for (t in 2:T) {
      trend[t] <- rnorm(
        1,
        trend_alphas[x] +  trend[t - 1],
        0.25
      )
    }
    as.vector(scale(trend))
  }))

  # Loadings on the trends for each series
  corrs <- c(-0.7, -0.5, -0.25, -0.05, 0, 0, 0, 0, 0.05, 0.25, 0.5, 0.7)
  combos <- combn(seq(1, n_series), 2)
  cor_mat <- diag(n_series)

  n_combos <- ncol(combos)
  for(i in 1:n_combos){
    cor_mat[combos[,i][1],
            combos[,i][2]] <- cor_mat[rev(combos[,i])[1],
                                      rev(combos[,i])[2]] <- sample(corrs, 1)
  }

  cov_mat <- as.matrix(Matrix::nearPD(diag(rep(1, n_series))
                                      %*% cor_mat %*% diag(rep(1, n_series)))$mat)
  loadings <- MASS::mvrnorm(n_trends, rep(0,n_series), cov_mat)

  # Simulate the shared seasonal pattern
  ETSAAA <- stl(smooth::sim.es(model="ANA",frequency=freq,obs = T + 5,randomizer="rnorm")$data, 'periodic')$time.series[,1]
  seasonality <- zoo::rollmean(as.vector(scale(ETSAAA)), k = 6, na.pad = F)

  # Simulate observed series
  invlogit = function(x){
    exp(x)/(1+exp(x))
  }
  obs_ys <- c(unlist(lapply(seq_len(n_series), function(x){
    obs <- invlogit(rowSums(loadings[,x] *  trends) * trend_rel +
                     seasonality * (1 - trend_rel))
    qnbinom(obs, size = size_obs[x], mu = mu_obs[x])
  })))

  # Return simulated data
  sim_data = data.frame(y = obs_ys,
                        season = rep(rep(seq(1, freq), ceiling(T/freq))[1:T], n_series),
                        year = rep(sort(rep(seq(1, ceiling(T/freq)), freq))[1:T], n_series),
                        series = as.factor(paste0('series_', sort(rep(seq(1, n_series), T)))),
                        in_season = 1)

  list(data_train = sim_data[1:(floor(nrow(sim_data) * train_prop)),],
       data_test = sim_data[((floor(nrow(sim_data) * train_prop)) + 1):nrow(sim_data),],
       true_corrs = cor_mat,
       true_seasonality = seasonality)

}
