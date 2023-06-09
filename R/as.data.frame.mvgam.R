#'@title Extract posterior draws from fitted `mvgam` objects
#'@name mvgam_draws
#'@description Extract posterior draws in conventional formats as data.frames, matrices, or arrays.
#'@param x \code{list} object of class `mvgam`
#'@param variable A character specifying which parameters to extract. Options are:
#'\itemize{
#'   \item `betas` (beta coefficients from the GAM observation model linear predictor; default)
#'   \item `trend_params` (parameters governing the trend dynamics, such as AR parameters,
#'trend SD parameters or Gaussian Process parameters)
#'   \item `trend_betas` (beta coefficients from the GAM latent trend model linear predictor;
#'currently ignored)
#'   \item `obs_params` (other parameters specific to the observation model, such as overdispsersions
#'for negative binomial models or observation error SD for gaussian / student-t models)}
#'@param row.names Ignored
#'@param optional Ignored
#'@param ... Ignored
#'@return A `data.frame`, `matrix`, or `array` containing the posterior draws.
#'@examples
#'\dontrun{
#'sim <- sim_mvgam(family = Gamma())
#'mod1 <- mvgam(y ~ s(season, bs = 'cc'),
#'              trend_model = 'AR1',
#'              data = sim$data_train,
#'              family = Gamma())
#'beta_draws_df <- as.data.frame(mod1, variable = 'betas')
#'head(beta_draws_df)
#'str(beta_draws_df)
#'
#'beta_draws_mat <- as.data.frame(mod1, variable = 'betas')
#'head(beta_draws_mat)
#'str(beta_draws_mat)}
NULL

#'@rdname mvgam_draws
#'@export
as.data.frame.mvgam = function(x,
                               row.names = NULL,
                               optional = TRUE,
                               variable,
                               ...){

  variable <- match.arg(arg = variable,
                           choices = c('betas',
                                       "trend_params",
                                       "trend_betas",
                                       "obs_params"))

  if(variable == 'betas'){
    post <- data.frame(mcmc_chains(x$model_output, params = 'b'))
    colnames(post) <- names(coef(x$mgcv_model))
  }

  if(variable == 'trend_params'){
    to_extract <- trend_par_names(x$trend_model,
                                          x$use_lv,
                                          x$drift)
    to_extract <- to_extract[!to_extract %in% c('tau','trend')]
    post <- data.frame(mcmc_chains(x$model_output,
                                           params = to_extract))
    newnames <- gsub("\\.(?=[^.]*\\.)", "[", colnames(post), perl = TRUE)
    newnames <- gsub("\\.", "]", newnames, perl = TRUE)
    colnames(post) <- newnames
  }

  if(variable == 'trend_betas'){
    post <- NULL
  }

  if(variable == 'obs_params'){
    to_extract <- family_par_names(x$family)
    post <- data.frame(mcmc_chains(x$model_output,
                                           params = to_extract))
    newnames <- gsub("\\.(?=[^.]*\\.)", "[", colnames(post), perl = TRUE)
    newnames <- gsub("\\.", "]", newnames, perl = TRUE)
    colnames(post) <- newnames
  }

  return(post)

}

#'@rdname mvgam_draws
#'@export
as.matrix.mvgam = function(x,
                           variable,
                           ...){

  variable <- match.arg(arg = variable,
                        choices = c('betas',
                                    "trend_params",
                                    "trend_betas",
                                    "obs_params"))

  if(variable == 'betas'){
    post <- as.matrix(data.frame(mcmc_chains(x$model_output, params = 'b')))
    colnames(post) <- names(coef(x$mgcv_model))
  }

  if(variable == 'trend_params'){
    to_extract <- trend_par_names(x$trend_model,
                                  x$use_lv,
                                  x$drift)
    to_extract <- to_extract[!to_extract %in% c('tau','trend')]
    post <- as.matrix(data.frame(mcmc_chains(x$model_output,
                                   params = to_extract)))
    newnames <- gsub("\\.(?=[^.]*\\.)", "[", colnames(post), perl = TRUE)
    newnames <- gsub("\\.", "]", newnames, perl = TRUE)
    colnames(post) <- newnames
  }

  if(variable == 'trend_betas'){
    post <- NULL
  }

  if(variable == 'obs_params'){
    to_extract <- family_par_names(x$family)
    post <- as.matrix(data.frame(mcmc_chains(x$model_output,
                                   params = to_extract)))
    newnames <- gsub("\\.(?=[^.]*\\.)", "[", colnames(post), perl = TRUE)
    newnames <- gsub("\\.", "]", newnames, perl = TRUE)
    colnames(post) <- newnames
  }

  return(post)

}

