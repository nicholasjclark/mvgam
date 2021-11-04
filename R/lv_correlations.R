#'Calculate trend correlations based on mvjagam latent factor loadings
#'
#'This function uses samples of latent dynamic factor loadings for each series from a fitted
#'mvjagam model and calculates correlations among series' trends
#'
#'
#'@param object \code{list} object returned from \code{mvjagam}
#'@param data_train A \code{dataframe} containing the model response variable and covariates
#'required by the GAM \code{formula}. Should include columns:
#''y' (the discrete outcomes; NAs allowed)
#''series' (character or factor index of the series IDs)
#''season' (numeric index of the seasonal time point for each observation; should not have any missing)
#''year' the numeric index for year
#''in_season' indicator for whether the observation is in season or not. If the counts tend to go to zero
#'during the off season (as in tick counts for example), setting this to zero can be useful as trends won't contribute during
#'during this time but they continue to evolve, allowing the trend from the past season to continue evolving rather than forcing
#'it to zero
#'Any other variables to be included in the linear predictor of \code{formula} must also be present
#'@return A \code{list} object containing the mean posterior correlations and the full array of posterior correlations
#'@export
lv_correlations = function(object, data_train){
  samps <- jags.samples(object$jags_model,
                        variable.names = 'lv_coefs',
                        n.iter = 1000, thin = 5)
  lv_coefs <- samps$lv_coefs
  n_series <- dim(lv_coefs)[1]
  n_lv <- dim(lv_coefs)[2]
  n_samples <- prod(dim(lv_coefs)[3:4])

  # Get array of latent variable loadings
  coef_array <- array(NA, dim = c(n_series, n_lv, n_samples))
  for(i in 1:n_series){
    for(j in 1:n_lv){
      coef_array[i, j, ] <- c(lv_coefs[i, j, , 1],
                              lv_coefs[i, j, , 2])
    }
  }

  # Posterior correlations based on latent variable loadings
  correlations <- array(NA, dim = c(n_series, n_series, n_samples))
  for(i in 1:n_samples){
    correlations[,,i] <- cov2cor(cov(t(coef_array[,,1])))
  }
  mean_correlations <- apply(correlations, c(1,2), function(x) quantile(x, 0.5))
  rownames(mean_correlations) <- colnames(mean_correlations) <- levels(data_train$series)

  list(mean_correlations = mean_correlations, posterior_correlations = correlations)
}
