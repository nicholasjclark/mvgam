#'Update an existing `mvgam` object
#'
#'This function allows a previously fitted `mvgam` model to be updated
#'@name update.mvgam
#'@importFrom mgcv nb betar
#'@importFrom rlang missing_arg
#'@param object A fitted `mvgam` model
#'@param formula Optional new `formula` object. Note, `mvgam` currently does not support dynamic formula
#'updates such as removal of specific terms with `- term`. When updating, the entire formula needs
#'to be supplied
#'@param trend_formula An optional \code{character} string specifying the GAM process model formula. If
#'supplied, a linear predictor will be modelled for the latent trends to capture process model evolution
#'separately from the observation model. Should not have a response variable specified on the left-hand side
#'of the formula (i.e. a valid option would be `~ season + s(year)`). This feature is experimental, and is only
#'currently available for Random Walk trend models.
#'@param data A \code{dataframe} or \code{list} containing the model response variable and covariates
#'required by the GAM \code{formula}. Should include columns:
#''series' (character or factor index of the series IDs)
#''time' (numeric index of the time point for each observation).
#'Any other variables to be included in the linear predictor of \code{formula} must also be present
#'@param newdata Optional \code{dataframe} or \code{list} of test data containing at least 'series' and 'time'
#'in addition to any other variables included in the linear predictor of \code{formula}. If included, the
#'observations in variable \code{y} will be set to \code{NA} when fitting the model so that posterior
#'simulations can be obtained
#'@param trend_model \code{character} specifying the time series dynamics for the latent trend. Options are:
#'\itemize{
#'   \item `None` (no latent trend component; i.e. the GAM component is all that contributes to the linear predictor,
#'and the observation process is the only source of error; similarly to what is estimated by \code{\link[mgcv]{gam}})
#'   \item `RW` (random walk with possible drift)
#'   \item `AR1` (with possible drift)
#'   \item `AR2` (with possible drift)
#'   \item `AR3` (with possible drift)
#'   \item `VAR1` (with possible drift; only available in \code{Stan})
#'   \item `GP` (Gaussian Process with squared exponential kernel;
#'only available in \code{Stan})}
#'@param trend_map Optional `data.frame` specifying which series should depend on which latent
#'trends. Useful for allowing multiple series to depend on the same latent trend process, but with
#'different observation processes. If supplied, a latent factor model is set up by setting
#'`use_lv = TRUE` and using the mapping to set up the shared trends. Needs to have column names
#'`series` and `trend`, with integer values in the `trend` column to state which trend each series
#'should depend on. The `series` column should have a single unique entry for each series in the
#'data (names should perfectly match factor levels of the `series` variable in `data`). See examples
#'in \code{\link{mvgam}} for details
#'@param family \code{family} specifying the exponential observation family for the series. Currently supported
#'families are: `nb()`, `poisson()`, `tweedie()`, `gaussian()`, `betar()`, `lognormal()`, `student_t()` and `Gamma()`
#'@param use_lv \code{logical}. If \code{TRUE}, use dynamic factors to estimate series'
#'latent trends in a reduced dimension format. If \code{FALSE}, estimate independent latent trends for each series
#'@param n_lv \code{integer} the number of latent dynamic factors to use if \code{use_lv == TRUE}.
#'Cannot be \code{>n_series}. Defaults arbitrarily to \code{min(2, floor(n_series / 2))}
#'@param priors An optional \code{data.frame} with prior
#'definitions. See \code{\link{get_mvgam_priors}} and
#'[mvgam] for more information on changing default prior distributions
#'@param lfo Logical indicating whether this is part of a call to [lfo_cv.mvgam]. Returns a
#'lighter version of the model with no residuals and fewer monitored parameters to speed up
#'post-processing. But other downstream functions will not work properly, so users should always
#'leave this set as `FALSE`
#'@param ... Other arguments to be passed to \code{\link{mvgam}}
#'@export
update.mvgam = function(object, formula,
                        trend_formula,
                        data, newdata,
                        trend_model,
                        trend_map,
                        use_lv, n_lv,
                        family, priors,
                        lfo = FALSE,
                        ...){

  if(missing(formula)){
    formula <- object$call
  }

  if(missing(trend_formula)){
    if(is.null(object$trend_call)){
      trend_formula <- missing_arg()
    } else {
      trend_formula <- object$trend_call
    }
  }

  if(missing(trend_map)){
    if(is.null(object$trend_map)){
      trend_map <- missing_arg()
    } else {
      trend_map <- object$trend_map
    }
  }

  if(missing(data)){
    data_train <- object$obs_data
  } else {
    data_train <- data
  }

  if(missing(priors)){
    priors <- object$priors
  }

  if(!missing(newdata)){
    # If new  testing data supplied, include as the test data
    data_test <- newdata
    include_fc <- TRUE
  } else if(!is.null(object$test_data)){
    # only include test data when no new training data is supplied
    if(missing(data)){
      include_fc <- TRUE
      data_test <- object$test_data
    } else {
      include_fc <- FALSE
    }
  } else {
    include_fc <- FALSE
  }

  if(missing(trend_model)){
    trend_model <- object$trend_model

    if(trend_model == 'VAR1'){
      if(any(is.na(mcmc_summary(object$model_output, 'Sigma')[,6]))){
        trend_model <- 'VAR1'
      } else {
        trend_model <- 'VAR1cor'
      }
    }
  }

  if(missing(use_lv)){
    use_lv <- object$use_lv
  }

  if(missing(n_lv)){
    n_lv <- object$n_lv
  }

  if(missing(n_lv)){
    n_lv <- object$n_lv
  }

  if(missing(family)){
    family_char <- object$family
    family <- family_char
    if(family_char == 'negative binomial'){
      family <- nb()
    }
    if(family_char == 'beta'){
      family <- betar()
    }
    if(family_char == 'student'){
      family <- student_t()
    }
  }

  if(include_fc){
    updated_mod <- mvgam(formula = formula,
                         trend_formula = trend_formula,
                         trend_map = trend_map,
                         data = data_train,
                         newdata = data_test,
                         trend_model = trend_model,
                         use_lv = use_lv,
                         n_lv = n_lv,
                         family = family,
                         refit = TRUE,
                         lfo = lfo,
                         use_stan = ifelse(object$fit_engine == 'stan', TRUE,
                                           FALSE),
                         priors = priors,
                         ...)
  } else {
    updated_mod <- mvgam(formula = formula,
                         trend_formula = trend_formula,
                         trend_map = trend_map,
                         data = data_train,
                         trend_model = trend_model,
                         use_lv = use_lv,
                         n_lv = n_lv,
                         family = family,
                         refit = TRUE,
                         lfo = lfo,
                         use_stan = ifelse(object$fit_engine == 'stan', TRUE,
                                           FALSE),
                         priors = priors,
                         ...)
  }

  return(updated_mod)

}
