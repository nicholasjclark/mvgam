#### Generic trend information ####
#' @noRd
evaluate_trend_model = function(trend_model){
  trend_model <- match.arg(arg = trend_model,
                           choices = c("RW",
                                       "GP",
                                       'AR1',
                                       'AR2',
                                       'AR3'))
  return(trend_model)
}

#' Squared exponential GP simulation function
#' @param last_trends Vector of trend estimates leading up to the current timepoint
#' @param h \code{integer} specifying the forecast horizon
#' @param rho_gp length scale parameter
#' @param alpha_gp marginal variation parameter
#' @noRd
sim_gp = function(last_trends, h, rho_gp, alpha_gp){

  t <- 1:length(last_trends)
  t_new <- 1:(length(last_trends) + h)
  Sigma_new <- alpha_gp^2 * exp(-0.5 * ((outer(t, t_new, "-") / rho_gp) ^ 2))
  Sigma_star <- alpha_gp^2 * exp(-0.5 * ((outer(t_new, t_new, "-") / rho_gp) ^ 2)) +
    diag(1e-4, length(t_new))
  Sigma <- alpha_gp^2 * exp(-0.5 * ((outer(t, t, "-") / rho_gp) ^ 2)) +
    diag(1e-4, length(t))

  as.vector(tail(t(Sigma_new) %*% solve(Sigma, last_trends),
                 h) +
              tail(MASS::mvrnorm(1,
                                 mu = rep(0, length(t_new)),
                                 Sigma = Sigma_star - t(Sigma_new) %*% solve(Sigma, Sigma_new)),
                   h))
}

#' AR3  simulation function
#' @param last_trends Vector of trend estimates leading up to the current timepoint
#' @param h \code{integer} specifying the forecast horizon
#' @param drift drift parameter
#' @param ar1 AR1 parameter
#' @param ar2 AR2 parameter
#' @param ar3 AR3 parameter
#' @param tau precision parameter
#' @noRd
sim_ar3 = function(drift, ar1, ar2, ar3, tau, last_trends, h){
  states <- rep(NA, length = h + 3)
  states[1] <- last_trends[1]
  states[2] <- last_trends[2]
  states[3] <- last_trends[3]
  for (t in 4:(h + 3)) {
    states[t] <- rnorm(1, drift + ar1*states[t - 1] +
                         ar2*states[t - 2] +
                         ar3*states[t - 3], sqrt(1 / tau))
  }
  states[-c(1:3)]
}

#' Parameters to monitor / extract
#' @noRd
trend_par_names = function(trend_model, use_lv = FALSE,
                           drift = FALSE){

  # Check arguments
  trend_model <- match.arg(arg = trend_model, choices = c("None", "RW", "AR1",
                                                          "AR2", "AR3", "GP"))

  if(use_lv){
    if(trend_model == 'RW'){
      param <- c('trend', 'LV', 'penalty', 'lv_coefs')
    }

    if(trend_model == 'AR1'){
      param <- c('trend', 'ar1', 'LV', 'penalty', 'lv_coefs')
    }

    if(trend_model == 'AR2'){
      param <- c('trend', 'ar1', 'ar2', 'LV',
                 'penalty', 'lv_coefs')
    }

    if(trend_model == 'AR3'){
      param <- c('trend', 'ar1', 'ar2', 'ar3',
                 'LV', 'penalty', 'lv_coefs')
    }

    if(trend_model == 'GP'){
      param <- c('trend', 'alpha_gp', 'rho_gp',
                 'LV', 'lv_coefs')
    }

  }

  if(!use_lv){
    if(trend_model == 'RW'){
      param <- c('trend', 'tau', 'sigma')
    }

    if(trend_model == 'AR1'){
      param <- c('trend', 'tau', 'sigma', 'ar1')
    }

    if(trend_model == 'AR2'){
      param <- c('trend', 'tau', 'sigma', 'ar1', 'ar2')
    }

    if(trend_model == 'AR3'){
      param <- c('trend', 'tau', 'sigma', 'ar1', 'ar2', 'ar3')
    }

    if(trend_model == 'GP'){
      param <- c('trend', 'alpha_gp', 'rho_gp')
    }

  }

  if(trend_model != 'None'){
    if(drift){
      param <- c(param, 'drift')
    }
  }

  if(trend_model == 'None'){
    param <- NULL
  }

  param
}

#' Extraction of particular parameters
#' @noRd
extract_trend_pars = function(object, keep_all_estimates = TRUE,
                              ending_time = NULL){

  # Get names of parameters to extract
  pars_to_extract <- trend_par_names(trend_model = object$trend_model,
                                     use_lv = object$use_lv,
                                     drift = object$drift)

  # Extract into a named list
  if(length(pars_to_extract) > 0){
    out <- vector(mode = 'list')
    for(i in 1:length(pars_to_extract)){
      out[[i]] <- mcmc_chains(object$model_output,
                                      params = pars_to_extract[i])
    }
    names(out) <- pars_to_extract

  } else {
    out <- list()
  }

  # Latent trend loadings for dynamic factor models
  if(object$use_lv){
    if(object$trend_model %in% c('RW', 'AR1', 'AR2', 'AR3')){
      # Just due to legacy reasons from working in JAGS, the simulation
      # functions use precision (tau) rather than SD (sigma)
      out$tau <- mcmc_chains(object$model_output, 'penalty')
      out$penalty <- NULL
    }

    n_series <- NCOL(object$ytimes)
    n_lv <- object$n_lv
    out$lv_coefs <- lapply(seq_len(n_series), function(series){
      if(object$fit_engine == 'stan'){
        coef_start <- min(which(sort(rep(1:n_series, n_lv)) == series))
        coef_end <- coef_start + n_lv - 1
        as.matrix(mcmc_chains(object$model_output, 'lv_coefs')[,coef_start:coef_end])
      } else {
        lv_indices <- seq(1, n_series * n_lv, by = n_series) + (series - 1)
        as.matrix(mcmc_chains(object$model_output, 'lv_coefs')[,lv_indices])
      }

    })

  } else {
    if(object$trend_model %in% c('RW', 'AR1', 'AR2', 'AR3')){
      out$sigma <- NULL
    }
  }

  if(!keep_all_estimates){
    #### Extract last xxx timepoints of latent trends for propagating forecasts
    # foreward ####

    # Latent trend estimates for dynamic factor models
    if(object$use_lv){
      n_lv <- object$n_lv
      if(object$fit_engine == 'stan'){
        out$last_lvs <- lapply(seq_len(n_lv), function(lv){
          inds_lv <- seq(lv, dim(out$LV)[2], by = n_lv)
          lv_estimates <- out$LV[,inds_lv]
          # Need to only use estimates from the training period
          if(class(object$obs_data)[1] == 'list'){
            end_train <- data.frame(y = object$obs_data$y,
                                    series = object$obs_data$series,
                                    time = object$obs_data$time) %>%
              dplyr::filter(series == !!(levels(object$obs_data$series)[1])) %>%
              NROW()
          } else {
            end_train <- object$obs_data %>%
              dplyr::filter(series == !!(levels(object$obs_data$series)[1])) %>%
              NROW()
          }

          if(object$trend_model == 'GP'){
            if(!is.null(ending_time)){
              lv_estimates <- lv_estimates[, 1:ending_time]
            } else {
              lv_estimates <- lv_estimates[, 1:end_train]
            }

          } else {
            if(!is.null(ending_time)){
              lv_estimates <- lv_estimates[, 1:ending_time]
            } else {
              lv_estimates <- lv_estimates[,(NCOL(lv_estimates)-2):(NCOL(lv_estimates))]
            }
          }
          lv_estimates
        })

      } else {
        ends <- seq(0, dim(out$LV)[2], length.out = n_lv + 1)
        starts <- ends + 1
        starts <- c(1, starts[-c(1, (n_lv + 1))])
        ends <- ends[-1]

        out$last_lvs <- lapply(seq_len(n_lv), function(lv){
          lv_estimates <- out$LV[,starts[lv]:ends[lv]]

          # Need to only use estimates from the training period
          if(class(object$obs_data)[1] == 'list'){
            end_train <- data.frame(y = object$obs_data$y,
                                    series = object$obs_data$series,
                                    time = object$obs_data$time) %>%
              dplyr::filter(series == !!(levels(object$obs_data$series)[1])) %>%
              NROW()
          } else {
            end_train <- object$obs_data %>%
              dplyr::filter(series == !!(levels(object$obs_data$series)[1])) %>%
              NROW()
          }

          # GP models not available in JAGS
          if(!is.null(ending_time)){
            lv_estimates <- lv_estimates[, 1:ending_time]
          } else {
            lv_estimates <- lv_estimates[,(NCOL(lv_estimates)-2):(NCOL(lv_estimates))]
          }

        })
      }

      # Get rid of the large posterior arrays for trend and LV estimates;
      # they won't be needed for propagating the trends forward
      out$LV <- NULL
      out$trend <- NULL
    }

    if(!object$use_lv){
      if(object$trend_model != 'None'){
        out$last_trends <- lapply(seq_along(levels(object$obs_data$series)), function(series){
          if(object$fit_engine == 'stan'){
            trend_estimates <- mcmc_chains(object$model_output, 'trend')[,seq(series,
                                                                                      dim(mcmc_chains(object$model_output,
                                                                                                              'trend'))[2],
                                                                                      by = NCOL(object$ytimes))]
          } else {
            trend_estimates <- mcmc_chains(object$model_output, 'trend')[,starts[series]:ends[series]]
          }

          # Need to only use estimates from the training period
          if(class(object$obs_data)[1] == 'list'){
            end_train <- data.frame(y = object$obs_data$y,
                                    series = object$obs_data$series,
                                    time = object$obs_data$time) %>%
              dplyr::filter(series == !!(levels(object$obs_data$series)[series])) %>%
              NROW()
          } else {
            end_train <- object$obs_data %>%
              dplyr::filter(series == !!(levels(object$obs_data$series)[series])) %>%
              NROW()
          }

          trend_estimates <- trend_estimates[,1:end_train]

          # Only need last 3 timesteps if this is not a GP trend model
          if(object$trend_model == 'GP'){
            if(!is.null(ending_time)){
              trend_estimates <- trend_estimates[,1:ending_time]
            } else {
              trend_estimates <- trend_estimates
            }

          } else {
            if(!is.null(ending_time)){
              trend_estimates <- trend_estimates[,1:ending_time]
            } else {
              trend_estimates <- trend_estimates[,(NCOL(trend_estimates)-2):(NCOL(trend_estimates))]
            }
          }

          trend_estimates

        })

        out$trend <- NULL

      }
    }
  }

  # Return list of extracted posterior parameter samples
  out
}

#' Wrapper function to forecast trends
#' @noRd
forecast_trend = function(trend_model, use_lv, trend_pars, h = 1){

  # Check arguments
  trend_model <- match.arg(arg = trend_model, choices = c("None", "RW", "AR1",
                                                          "AR2", "AR3", "GP"))

  # Propagate dynamic factors forward
  if(use_lv){
    n_lv <- length(trend_pars$last_lvs)
    if(trend_model %in% c('RW', 'AR1', 'AR2', 'AR3')){
      next_lvs <- do.call(cbind, lapply(seq_len(n_lv), function(lv){

        ar1 <- ifelse('ar1' %in% names(trend_pars),
                      trend_pars$ar1[lv],
                      0)
        if(trend_model == 'RW'){
          ar1 <- 1
        }

        mvgam:::sim_ar3(drift = ifelse('drift' %in% names(trend_pars),
                                       trend_pars$drift[lv],
                                       0),
                        ar1 = ar1,
                        ar2 = ifelse('ar2' %in% names(trend_pars),
                                     trend_pars$ar2[lv],
                                     0),
                        ar3 = ifelse('ar3' %in% names(trend_pars),
                                     trend_pars$ar3[lv],
                                     0),
                        tau = trend_pars$tau[lv],
                        last_trends = trend_pars$last_lvs[[lv]],
                        h = h)
      }))
    }

    if(trend_model == 'GP'){
      next_lvs <- do.call(cbind, lapply(seq_len(n_lv), function(lv){
        mvgam:::sim_gp(alpha_gp = trend_pars$alpha_gp[lv],
                       rho_gp = trend_pars$rho_gp[lv],
                       last_trends = trend_pars$last_lvs[[lv]],
                       h = h)
      }))
    }

    trend_fc <- next_lvs
  }

  # Simpler if not using dynamic factors
  if(!use_lv){

    if(trend_model %in% c('RW', 'AR1', 'AR2', 'AR3')){

      ar1 <- ifelse('ar1' %in% names(trend_pars),
                    trend_pars$ar1,
                    0)
      if(trend_model == 'RW'){
        ar1 <- 1
      }

      trend_fc <-  mvgam:::sim_ar3(drift = ifelse('drift' %in% names(trend_pars),
                                                  trend_pars$drift,
                                                  0),
                                   ar1 = ar1,
                                   ar2 = ifelse('ar2' %in% names(trend_pars),
                                                trend_pars$ar2,
                                                0),
                                   ar3 = ifelse('ar3' %in% names(trend_pars),
                                                trend_pars$ar3,
                                                0),
                                   tau = trend_pars$tau,
                                   last_trends = trend_pars$last_trends,
                                   h = h)
    }

    if(trend_model == 'GP'){
      trend_fc <- mvgam:::sim_gp(alpha_gp = trend_pars$alpha_gp,
                                 rho_gp = trend_pars$rho_gp,
                                 last_trends = trend_pars$last_trends,
                                 h = h)
    }

  }
  return(trend_fc)
}

