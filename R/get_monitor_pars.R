#' Return parameters to monitor during modelling
#'
#'
#' @export
#' @param family \code{character}. Must be either 'nb' (for Negative Binomial), 'tw' (for Tweedie) or 'poisson'
#' @param smooths_included Logical. Are smooth terms included in the model formula?
#' @param use_lv Logical (use latent variable trends or not)
#' @param trend_model The type of trend model used
#' @param drift Logical (was a drift term estimated or not)
#' @return A string of parameters to monitor
get_monitor_pars = function(family, smooths_included = TRUE, use_lv, trend_model, drift){

  if(smooths_included){
    param <- c('rho', 'b', 'ypred')
  } else {
    param <- c('b', 'ypred')
  }

  if(family == 'nb'){
    param <- c(param, 'r')
  }

  if(family == 'tw'){
    param <- c(param, 'twdis')
  }

  if(use_lv){
    if(trend_model == 'RW'){
      param <- c(param, 'trend', 'LV', 'penalty', 'lv_coefs')
    }

    if(trend_model == 'AR1'){
      param <- c(param, 'trend', 'tau', 'sigma', 'ar1', 'LV',
                 'penalty', 'lv_coefs')
    }

    if(trend_model == 'AR2'){
      param <- c(param, 'trend', 'tau', 'sigma', 'ar1', 'ar2', 'LV',
                 'penalty', 'lv_coefs')
    }

    if(trend_model == 'AR3'){
      param <- c(param, 'trend', 'tau', 'sigma', 'ar1', 'ar2', 'ar3',
                 'LV', 'penalty', 'lv_coefs')
    }
  }

  if(!use_lv){
    if(trend_model == 'RW'){
      param <- c(param, 'trend', 'tau', 'sigma')
    }

    if(trend_model == 'AR1'){
      param <- c(param, 'trend', 'tau', 'sigma', 'ar1')
    }

    if(trend_model == 'AR2'){
      param <- c(param, 'trend', 'tau', 'sigma', 'ar1', 'ar2')
    }

    if(trend_model == 'AR3'){
      param <- c(param, 'trend', 'tau', 'sigma', 'ar1', 'ar2', 'ar3')
    }

    if(trend_model == 'GP'){
      param <- c(param, 'trend', 'alpha_gp', 'rho_gp')
    }

  }

  if(trend_model != 'None'){
    if(drift){
      param <- c(param, 'phi')
    }
  }
  param
}
