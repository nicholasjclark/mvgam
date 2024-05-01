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
#'@param use_alias Logical. If more informative names for parameters are available
#'(i.e. for beta coefficients `b` or for smoothing parameters `rho`), replace the uninformative
#'names with the more informative alias. Defaults to `TRUE`
#'@param inc_warmup Should warmup draws be included? Defaults to \code{FALSE}.
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
#'              family = Gamma(),
#'              chains = 2,
#'              samples = 300)
#'beta_draws_df <- as.data.frame(mod1, variable = 'betas')
#'head(beta_draws_df)
#'str(beta_draws_df)
#'
#'beta_draws_mat <- as.matrix(mod1, variable = 'betas')
#'head(beta_draws_mat)
#'str(beta_draws_mat)
#'
#'shape_pars <- as.matrix(mod1, variable = 'shape', regex = TRUE)
#'head(shape_pars)}
NULL

#'@rdname mvgam_draws
#'@export
as.data.frame.mvgam = function(x,
                               row.names = NULL,
                               optional = TRUE,
                               variable = 'betas',
                               use_alias = TRUE,
                               regex = FALSE,
                               ...){

  # Check variable and get more informative names if applicable
  extract_pars <- validate_variables(x,
                                     variable = variable,
                                     regex = regex)

  # Create a slim brmsfit object and use brms machinery to do extraction
  dummy <- structure(list(fit = x$model_output),
                     class = 'brmsfit')
  post <- as.data.frame(dummy, variable = extract_pars$to_extract)

  # Rename if needed
  if(use_alias){
    if(!is.null(extract_pars$newnames)){
      colnames(post) <- extract_pars$newnames
    }
  }

  return(post)
}

#'@rdname mvgam_draws
#'@export
as.matrix.mvgam = function(x,
                           variable = 'betas',
                           regex = FALSE,
                           use_alias = TRUE,
                           ...){

  # Check variable and get more informative names if applicable
  extract_pars <- validate_variables(x,
                                     variable = variable,
                                     regex = regex)

  # Create a slim brmsfit object and use brms machinery to do extraction
  dummy <- structure(list(fit = x$model_output),
                     class = 'brmsfit')
  post <- as.matrix(dummy, variable = extract_pars$to_extract)

  # Rename if needed
  if(use_alias){
    if(!is.null(extract_pars$newnames)){
      colnames(post) <- extract_pars$newnames
    }
  }

  return(post)
}

#'@rdname mvgam_draws
#'@export
as.array.mvgam = function(x,
                          variable = 'betas',
                          regex = FALSE,
                          use_alias = TRUE,
                          ...){

  # Check variable and get more informative names if applicable
  extract_pars <- validate_variables(x,
                                     variable = variable,
                                     regex = regex)

  # Create a slim brmsfit object and use brms machinery to do extraction
  dummy <- structure(list(fit = x$model_output),
                     class = 'brmsfit')
  post <- as.array(dummy, variable = extract_pars$to_extract)

  # Rename if needed
  if(use_alias){
    if(!is.null(extract_pars$newnames)){
      dimnames(post)$variable <- extract_pars$newnames
    }
  }

  return(post)
}

#' @rdname mvgam_draws
#' @importFrom posterior as_draws
#' @method as_draws mvgam
#' @export
as_draws.mvgam <- function(x, variable = NULL, regex = FALSE,
                           inc_warmup = FALSE, use_alias = TRUE,
                           ...){

  # Check variable and get more informative names if applicable
  extract_pars <- validate_variables(x,
                                     variable = variable,
                                     regex = regex)

  # Create a slim brmsfit object and use brms machinery to do extraction
  dummy <- structure(list(fit = x$model_output),
                     class = 'brmsfit')
  # Extract
  post <- as_draws_list(dummy,
                        variable = extract_pars$to_extract,
                        regex = FALSE,
                        inc_warmup = inc_warmup,
                        ...)

  # Rename if needed
  if(use_alias){
    if(!is.null(extract_pars$newnames)){
      for(chain in seq_along(post)){
        names(post[[chain]]) <- extract_pars$newnames
      }
    }
  }
  return(post)
}

#' @rdname mvgam_draws
#' @importFrom posterior as_draws_matrix
#' @method as_draws_matrix mvgam
#' @export
as_draws_matrix.mvgam <- function(x, variable = NULL, regex = FALSE,
                                  inc_warmup = FALSE, use_alias = TRUE,
                                  ...){

  # Check variable and get more informative names if applicable
  extract_pars <- validate_variables(x,
                                     variable = variable,
                                     regex = regex)

  # Create a slim brmsfit object and use brms machinery to do extraction
  dummy <- structure(list(fit = x$model_output),
                     class = 'brmsfit')
  # Extract
  post <- as_draws_matrix(dummy,
                        variable = extract_pars$to_extract,
                        regex = FALSE,
                        inc_warmup = inc_warmup,
                        ...)

  # Rename if needed
  if(use_alias){
    if(!is.null(extract_pars$newnames)){
      colnames(post) <- extract_pars$newnames
    }
  }

  return(post)
}

#' @rdname mvgam_draws
#' @importFrom posterior as_draws_df
#' @method as_draws_df mvgam
#' @export
as_draws_df.mvgam <- function(x, variable = NULL, regex = FALSE,
                                  inc_warmup = FALSE, use_alias = TRUE,
                                  ...){

  # Check variable and get more informative names if applicable
  extract_pars <- validate_variables(x,
                                     variable = variable,
                                     regex = regex)

  # Create a slim brmsfit object and use brms machinery to do extraction
  dummy <- structure(list(fit = x$model_output),
                     class = 'brmsfit')
  # Extract
  post <- as_draws_df(dummy,
                      variable = extract_pars$to_extract,
                      regex = FALSE,
                      inc_warmup = inc_warmup,
                      ...)

  # Rename if needed
  if(use_alias){
    if(!is.null(extract_pars$newnames)){
      colnames(post)[1:length(extract_pars$newnames)] <- extract_pars$newnames
    }
  }

  return(post)
}

#' @rdname mvgam_draws
#' @importFrom posterior as_draws_array
#' @method as_draws_array mvgam
#' @export
as_draws_array.mvgam <- function(x, variable = NULL, regex = FALSE,
                                  inc_warmup = FALSE, use_alias = TRUE,
                                  ...){

  # Check variable and get more informative names if applicable
  extract_pars <- validate_variables(x,
                                     variable = variable,
                                     regex = regex)

  # Create a slim brmsfit object and use brms machinery to do extraction
  dummy <- structure(list(fit = x$model_output),
                     class = 'brmsfit')
  # Extract
  post <- as_draws_array(dummy,
                          variable = extract_pars$to_extract,
                          regex = FALSE,
                          inc_warmup = inc_warmup,
                          ...)

  # Rename if needed
  if(use_alias){
    if(!is.null(extract_pars$newnames)){
      dimnames(post)$variable <- extract_pars$newnames
    }
  }

  return(post)
}

#' @rdname mvgam_draws
#' @importFrom posterior as_draws_list
#' @method as_draws_list mvgam
#' @export
as_draws_list.mvgam <- function(x, variable = NULL, regex = FALSE,
                                 inc_warmup = FALSE, use_alias = TRUE,
                                 ...){

  # Check variable and get more informative names if applicable
  extract_pars <- validate_variables(x,
                                     variable = variable,
                                     regex = regex)

  # Create a slim brmsfit object and use brms machinery to do extraction
  dummy <- structure(list(fit = x$model_output),
                     class = 'brmsfit')
  # Extract
  post <- as_draws_list(dummy,
                         variable = extract_pars$to_extract,
                         regex = FALSE,
                         inc_warmup = inc_warmup,
                         ...)

  # Rename if needed
  if(use_alias){
    if(!is.null(extract_pars$newnames)){
      for(chain in seq_along(post)){
        names(post[[chain]]) <- extract_pars$newnames
      }
    }
  }

  return(post)
}

#' @rdname mvgam_draws
#' @importFrom posterior as_draws_rvars
#' @method as_draws_rvars mvgam
#' @export
as_draws_rvars.mvgam <- function(x, variable = NULL, regex = FALSE,
                                inc_warmup = FALSE,
                                ...){

  # Check variable and get more informative names if applicable
  extract_pars <- validate_variables(x,
                                     variable = variable,
                                     regex = regex)

  # Create a slim brmsfit object and use brms machinery to do extraction
  dummy <- structure(list(fit = x$model_output),
                     class = 'brmsfit')

  # Extract (can't rename rvars due to the way it is structured)
  post <- as_draws_rvars(dummy,
                        variable = extract_pars$to_extract,
                        regex = FALSE,
                        inc_warmup = inc_warmup,
                        ...)
  return(post)
}

#'@noRd
validate_variables = function(x, variable, regex = FALSE){

  # Get a string of all possible variables to extract
  all_vars <- variables(x)
  all_orig_vars <- unlist(purrr::map(all_vars, 'orig_name'))
  all_alias_vars <- unlist(purrr::map(all_vars, 'alias'))

  all_orig_walias <- all_orig_vars[!is.na(all_alias_vars)]
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

  if(variable[1] == 'obs_params'){
    to_extract <- family_par_names(x$family)
    newnames <- NULL
  }

  if(variable[1] == 'betas'){
    to_extract <- 'b'
    newnames <- names(coef(x$mgcv_model))
  }

  if(variable[1] == 'smooth_params'){
    if(is.null(all_vars$observation_smoothpars)){
      stop('No observation-level smooth parameters in model; no smooth_params to extract',
           call. = FALSE)
    }
    to_extract <- 'rho'
    newnames <- paste0(x$sp_names, '_rho')
  }

  if(variable[1] == 'linpreds'){
    to_extract <- 'mus'
    newnames <- NULL
  }

  if(variable[1] == 'trend_params'){
    to_extract <- trend_par_names(attr(x$model_data, 'trend_model'),
                                  x$use_lv,
                                  x$drift)

    to_extract <- to_extract[!to_extract %in% c('tau','trend',
                                                'LV', 'penalty', 'lv_coefs')]

    # Determine which other trend params to include
    included <- vector(length = length(to_extract))
    for(i in 1:length(to_extract)){

      # Check if it can be extracted
      suppressWarnings(estimates <- try(mcmc_chains(x$model_output,
                                                    params = to_extract[i]),
                                        silent = TRUE))

      if(inherits(estimates, 'try-error')){
        included[i] <- FALSE
      } else {
        included[i] <- TRUE
      }
    }
    to_extract <- to_extract[included]

    newnames <- NULL
  }

  if(variable[1] == 'trend_betas'){
    if(is.null(x$trend_call)){
      stop('No trend_formula supplied to model; no trend_betas to extract',
           call. = FALSE)
    }
    to_extract <- 'b_trend'
    newnames <- paste0(names(coef(x$trend_mgcv_model)), '_trend')
  }

  if(variable[1] == "trend_smooth_params"){
    if(is.null(all_vars$trend_smoothpars)){
      stop('No smoothing parameters included in trend-level model',
           call. = FALSE)
    }

    to_extract <- 'rho_trend'
    newnames <- paste0(unlist(purrr::map(x$trend_mgcv_model$smooth,
                                         'label')), '_rho_trend')
  }

  if(variable[1] == 'trend_linpreds'){
    if(is.null(x$trend_call)){
      stop('No trend_formula supplied to model; no trend_linpreds to extract',
           call. = FALSE)
    }
    to_extract <- 'trend_mus'
    newnames <- NULL
  }

  # If not one of the standard subsets, get aliases for the chosen variable(s)
  if(!variable[1] %in% c("obs_params",
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

            names_to_use[[i]] <- unname(unlist(purrr::map(all_vars, 'alias'))[
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
            vars_to_extract[[i]] <- unname(all_orig_walias[
              which(all_alias_vars == variable[i])])
            names_to_use[[i]] <- variable[i]
          } else {
            vars_to_extract[[i]] <- unname(unlist(purrr::map(all_vars, 'orig_name'))[
              which(all_orig_vars == variable[i])])
            names_to_use[[i]] <- unname(unlist(purrr::map(all_vars, 'alias'))[
              which(all_orig_vars == variable[i])])
          }
        }
      }
    }

    vars_to_extract <- unlist(vars_to_extract)
    names_to_use <- unlist(names_to_use)
    names_to_use[is.na(names_to_use)] <- vars_to_extract[is.na(names_to_use)]

    if(all(is.na(vars_to_extract))){
      stop('could not find any variables matching the supplied patterns',
           call. = FALSE)
    }

    to_extract <- vars_to_extract[!is.na(vars_to_extract)]
    newnames <- names_to_use[!is.na(names_to_use)]
  }

  return(list(to_extract = to_extract,
              newnames = newnames))
}


