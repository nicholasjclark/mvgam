#'@title Extract posterior draws from fitted `mvgam` objects
#'@name mvgam_draws
#'@description Extract posterior draws in conventional formats as data.frames, matrices, or arrays.
#'@param x \code{list} object of class `mvgam`
#'@param variable A character specifying which parameters to extract. Can either be one of the
#'following options:
#'\itemize{
#'   \item `obs_params` (other parameters specific to the observation model, such as overdispsersions
#'for negative binomial models or observation error SD for gaussian / student-t models)
#'   \item `betas` (beta coefficients from the GAM observation model linear predictor; default)
#'   \item `smooth_params` (smoothing parameters from the GAM observation model)
#'   \item `linpreds` (estimated linear predictors on whatever link scale was used in the model)
#'   \item `trend_params` (parameters governing the trend dynamics, such as AR parameters,
#'trend SD parameters or Gaussian Process parameters)
#'   \item `trend_betas` (beta coefficients from the GAM latent process model linear predictor;
#'   only available if a `trend_formula` was supplied in the original model)
#'   \item `trend_smooth_params` (process model GAM smoothing parameters;
#'   only available if a `trend_formula` was supplied in the original model)
#'   \item `trend_linpreds` (process model linear predictors on the identity scale;
#'   only available if a `trend_formula` was supplied in the original model)} OR can be a character vector
#'   providing the variables to extract
#'@param regex Logical. If not using one of the prespecified options for extractions,
#'should `variable` be treated as a (vector of)
#'regular expressions? Any variable in x matching at least one of the regular expressions
#'will be selected. Defaults to `FALSE`.
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
                               variable = 'betas',
                               regex = FALSE,
                               ...){

  # Get a string of all possible variables to extract
  all_vars <- variables(x)
  all_orig_vars <- unique(unlist(purrr::map(all_vars, 'orig_name')))
  all_alias_vars <- unique(unlist(purrr::map(all_vars, 'alias')))
  all_alias_vars <- all_alias_vars[!is.na(all_alias_vars)]

  # All possible var sets to extract
  extract_choices = c("obs_params",
                      "betas",
                      "smooth_params",
                      "linpreds",
                      "trend_params",
                      "trend_betas",
                      "trend_smooth_params",
                      "trend_linpreds",
                      all_orig_vars,
                      all_alias_vars)

  if(variable == 'obs_params'){
    to_extract <- family_par_names(x$family)
    post <- data.frame(mcmc_chains(x$model_output,
                                   params = to_extract))
    newnames <- gsub("\\.(?=[^.]*\\.)", "[", colnames(post), perl = TRUE)
    newnames <- gsub("\\.", "]", newnames, perl = TRUE)
    colnames(post) <- newnames
  }

  if(variable == 'betas'){
    post <- data.frame(mcmc_chains(x$model_output, params = 'b'))
    colnames(post) <- names(coef(x$mgcv_model))
  }

  if(variable == 'smooth_params'){
    if(is.null(all_vars$observation_smoothpars)){
      stop('No observation-level smooth parameters in model; no smooth_params to extract',
           call. = FALSE)
    }
    post <- data.frame(mcmc_chains(x$model_output, params = 'rho'))
    colnames(post) <- x$sp_names
  }

  if(variable == 'linpreds'){
    post <- mcmc_chains(x$model_output, params = 'mus')
    varnames <- dimnames(post)[[2]]
    post <- as.data.frame(post)
    colnames(post) <- varnames
  }

  if(variable == 'trend_params'){
    to_extract <- trend_par_names(x$trend_model,
                                          x$use_lv,
                                          x$drift)
    to_extract <- to_extract[!to_extract %in% c('tau','trend',
                                                'LV', 'penalty', 'lv_coefs')]
    if(!is.null(x$trend_call) & x$trend_model %in% c('RW', 'AR1',
                                                   'AR2', 'AR3')){
      to_extract <- c(to_extract, 'sigma')
    }
    post <- mcmc_chains(x$model_output, params = to_extract)
    varnames <- dimnames(post)[[2]]
    post <- as.data.frame(post)
    colnames(post) <- varnames
  }

  if(variable == 'trend_betas'){
    if(is.null(x$trend_call)){
      stop('No trend_formula supplied to model; no trend_betas to extract',
           call. = FALSE)
    }
    post <- data.frame(mcmc_chains(x$model_output, params = 'b_trend'))
    colnames(post) <- paste0(names(coef(x$trend_mgcv_model)), '_trend')
  }

  if(variable == "trend_smooth_params"){
    if(is.null(all_vars$trend_smoothpars)){
      stop('No smoothing parameters included in trend-level model',
           call. = FALSE)
    }

    post <- data.frame(mcmc_chains(x$model_output, params = 'rho_trend'))
    colnames(post) <- paste0(unlist(purrr::map(x$trend_mgcv_model$smooth,
                                               'label')), '_trend')
  }

  if(variable == 'trend_linpreds'){
    if(is.null(x$trend_call)){
      stop('No trend_formula supplied to model; no trend_linpreds to extract',
           call. = FALSE)
    }
    post <- mcmc_chains(x$model_output, params = 'trend_mus')
    varnames <- dimnames(post)[[2]]
    post <- as.data.frame(post)
    colnames(post) <- varnames
  }

  # If not one of the standard subsets, extract the chosen variable(s)
  if(!variable %in% c("obs_params",
                     "betas",
                     "smooth_params",
                     "linpreds",
                     "trend_params",
                     "trend_betas",
                     "trend_smooth_params",
                     "trend_linpreds")){

    if(regex){
      vars_to_extract <- vector(mode = 'list')
      names_to_use <- vector(mode = 'list')
      for(i in 1:length(variable)){
        if(!any(grepl(variable[i], extract_choices))){
          vars_to_extract[[i]] <- NA
          names_to_use[[i]] <- NA
        } else {
          if(any(grepl(variable[i], all_alias_vars))){
            vars_to_extract[[i]] <- unname(unlist(purrr::map(all_vars, 'orig_name'))[
              grepl(variable[i], unlist(purrr::map(all_vars, 'alias')))])

            names_to_use[[i]] <- unname(unlist(purrr::map(all_vars, 'alias'))[
              grepl(variable[i], unlist(purrr::map(all_vars, 'alias')))])

          } else {
            vars_to_extract[[i]] <- unname(unlist(purrr::map(all_vars, 'orig_name'))[
              grepl(variable[i], unlist(purrr::map(all_vars, 'orig_name')))])

            names_to_use[[i]] <- unname(unlist(purrr::map(all_vars, 'orig_name'))[
              grepl(variable[i], unlist(purrr::map(all_vars, 'orig_name')))])
          }
        }
      }

    } else {
      vars_to_extract <- vector(mode = 'list')
      names_to_use <- vector(mode = 'list')
      for(i in 1:length(variable)){
        if(!any(extract_choices == variable[i])){
          vars_to_extract[[i]] <- NA
          names_to_use[[i]] <- NA
        } else {
          if(any(all_alias_vars == variable[i])){
            vars_to_extract[[i]] <- unname(unlist(purrr::map(all_vars, 'orig_name'))[
              which(all_alias_vars == variable[i])])
            names_to_use[[i]] <- variable[i]
          } else {
            vars_to_extract[[i]] <- unname(unlist(purrr::map(all_vars, 'orig_name'))[
              which(all_orig_vars == variable[i])])
            names_to_use[[i]] <- variable[i]
          }
        }
      }
    }

    vars_to_extract <- unlist(vars_to_extract)
    names_to_use <- unlist(names_to_use)

    if(all(is.na(vars_to_extract))){
      stop('could not find any variables matching the supplied patterns',
           call. = FALSE)
    }

    vars_to_extract <- vars_to_extract[!is.na(vars_to_extract)]
    names_to_use <- names_to_use[!is.na(names_to_use)]

    dummy <- structure(list(fit = x$model_output),
                       class = 'brmsfit')
    post <- as.data.frame(dummy, variable = vars_to_extract)
    colnames(post) <- names_to_use
  }

  return(post)

}

#'@rdname mvgam_draws
#'@export
as.matrix.mvgam = function(x,
                           variable = 'betas',
                           regex = FALSE,
                           ...){

  post_df <- as.data.frame(x, variable = variable, regex = regex)
  varnames <- colnames(post_df)
  post <- as.matrix(post_df)
  colnames(post) <- varnames

  return(post)

}

