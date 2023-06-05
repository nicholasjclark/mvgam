#'Update an existing `mvgam` object
#'
#'This function allows a previously fitted `mvgam` model to be updated
#'@name update.mvgam
#'@importFrom mgcv nb betar
#'@param object A fitted `mvgam` model
#'@param formula A new `formula` object. Note, `mvgam` currently does not support dynamic formula
#'updates such as removal of specific terms with `- term`. When updating, the entire formula needs
#'to be supplied
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
#''None' (no latent trend component; i.e. the GAM component is all that contributes to the linear predictor,
#'and the observation process is the only source of error; similarly to what is estimated by \code{\link[mgcv]{gam}}),
#''RW' (random walk with possible drift),
#''AR1' (with possible drift),
#''AR2' (with possible drift) or
#''AR3' (with possible drift) or
#''VAR1' (with possible drift; only available in \code{Stan}) or
#''GP' (Gaussian Process with squared exponential kernel;
#'only available in \code{stan})
#'@param family \code{family} specifying the exponential observation family for the series. Currently supported
#'families are: `nb()`, `poisson()`, `tweedie()`, `gaussian()`, `betar()`, `lognormal()`, `student_t()` and `Gamma()`
#'@param use_lv \code{logical}. If \code{TRUE}, use dynamic factors to estimate series'
#'latent trends in a reduced dimension format. If \code{FALSE}, estimate independent latent trends for each series
#'@param n_lv \code{integer} the number of latent dynamic factors to use if \code{use_lv == TRUE}.
#'Cannot be \code{>n_series}. Defaults arbitrarily to \code{min(2, floor(n_series / 2))}
#'@param ... Other arguments to be passed to \code{\link{mvgam}}
#'@export
update.mvgam = function(object, formula,
                        data, newdata,
                        trend_model,
                        use_lv, n_lv,
                        family, ...){

  if(missing(formula)){
    formula <- object$call
  }

  if(missing(data)){
    data_train <- object$obs_data
  } else {
    data_train <- data
  }

  if(!missing(newdata)){
    data_test <- newdata
    include_fc <- TRUE
  } else if(!is.null(object$test_data)){
    data_test <- object$test_data
    include_fc <- TRUE
  } else {
    include_fc <- FALSE
  }

  if(missing(trend_model)){
    trend_model <- object$trend_model
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
                         data = data_train,
                         newdata = data_test,
                         trend_model = trend_model,
                         use_lv = use_lv,
                         n_lv = n_lv,
                         family = family,
                         refit = TRUE,
                         use_stan = ifelse(object$fit_engine == 'stan', TRUE,
                                           FALSE),
                         ...)
  } else {
    updated_mod <- mvgam(formula = formula,
                         data = data_train,
                         trend_model = trend_model,
                         use_lv = use_lv,
                         n_lv = n_lv,
                         family = family,
                         refit = TRUE,
                         use_stan = ifelse(object$fit_engine == 'stan', TRUE,
                                           FALSE),
                         ...)
  }

  return(updated_mod)

}
