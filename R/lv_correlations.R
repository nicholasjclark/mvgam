#'Calculate trend correlations based on mvjagam latent factor loadings
#'
#'This function uses samples of latent trends for each series from a fitted
#'mvjagam model to calculates correlations among series' trends
#'
#'
#'@param object \code{list} object returned from \code{mvjagam}
#'@return A \code{list} object containing the mean posterior correlations and the full array of posterior correlations
#'@export
lv_correlations = function(object){
  data_train <- object$obs_data

  # Series start and end indices
  ends <- seq(0, dim(MCMCvis::MCMCchains(object$jags_output, 'ypred'))[2],
              length.out = NCOL(object$ytimes) + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
  ends <- ends[-1]

  # Total number of MCMC samples
  n_preds <- dim(MCMCvis::MCMCchains(object$jags_output, 'trend')[,starts[1]:ends[1]])[1]

  # Total number of observations per series
  if(class(data_train)[1] == 'list'){
    n_obs <- length(data_train$y) / NCOL(object$ytimes)
  } else {
    n_obs <- NROW(data_train) / NCOL(object$ytimes)
  }

  # Extract series trends
  series_trends <- lapply(seq_len(length(ends)), function(y){
    MCMCvis::MCMCchains(object$jags_output, 'trend')[,starts[y]:ends[y]][,1:n_obs]
  })

  # Get list of trend correlation estimates
  all_trend_cors <- lapply(seq_len(n_preds), function(x){
    cov2cor(cov(do.call(cbind, lapply(series_trends, function(y){
      y[x,]
    }))))
  })

  # Calculate posterior mean correlations
  mean_correlations <- Reduce(`+`, all_trend_cors) / length(all_trend_cors)
  rownames(mean_correlations) <- colnames(mean_correlations) <- levels(data_train$series)

  list(mean_correlations = mean_correlations, posterior_correlations = all_trend_cors)
}
