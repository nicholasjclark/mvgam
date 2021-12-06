#'Calculate trend correlations based on mvjagam latent factor loadings
#'
#'This function uses samples of latent trends for each series from a fitted
#'mvjagam model to calculates correlations among series' trends
#'
#'
#'@param object \code{list} object returned from \code{mvjagam}
#'@return A \code{list} object containing the mean posterior correlations and the full array of posterior correlations
#'@export
lv_correlations = function(object, data_train){
  data_train <- object$obs_data
  samps <- jags.samples(object$jags_model,
                        variable.names = 'trend',
                        n.iter = 1000, thin = 5)
  trends <- samps$trend
  n_series <- dim(trends)[2]
  n_samples <- dim(trends)[3]
  n_chains <- dim(trends)[4]

  # Get list of trend correlation estimates
  get_cors = function(trends, n_samples, chain){
    trend_cors <- lapply(seq_len(n_samples), function(x){
      cov2cor(cov(trends[, , x, chain]))
    })
    trend_cors
  }

  all_trend_cors <- do.call(c, lapply(seq_len(n_chains), function(chain){
    get_cors(trends, n_samples, chain)
  }))

  # Calculate posterior mean correlations
  mean_correlations <- Reduce(`+`, all_trend_cors) / length(all_trend_cors)
  rownames(mean_correlations) <- colnames(mean_correlations) <- levels(data_train$series)

  list(mean_correlations = mean_correlations, posterior_correlations = all_trend_cors)
}
