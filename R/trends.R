#' Supported mvgam trend models
#' @importFrom utils tail
#' @importFrom stats rnorm
#' @details \code{mvgam} currently supports the following dynamic trend models:
#'\itemize{
#'   \item \code{RW} Random Walk
#'   \item \code{AR1} Autoregressive model with AR coefficient for lag 1
#'   \item \code{AR2} Autoregressive model with AR coefficients for lags 1 and 2
#'   \item \code{AR3} Autoregressive model with AR coefficients for lags 1, 2 and 3
#'   \item \code{VAR1} Vector Autoregressive model with VAR coefficients for lag 1; contemporaneously uncorrelated process errors
#'   \item \code{VAR1cor} Vector Autoregressive model with VAR coefficients for lag 1; contemporaneously correlated process errors
#'   \item \code{GP} Squared exponential Gaussian Process
#'   \item \code{None} No latent trend is fitted
#'   }
#'
#'
#'Dynamic factor models can be used in which the latent factors evolve as either
#'`RW`, `AR1`, `AR2`, `AR3` or `GP`. Note that only `RW`, `AR1`, `AR2` and `AR3` are available if
#'using `JAGS`. All trend models are supported if using `Stan`. For multivariate trend models
#'(i.e. `VAR` and `VARcor` models), users can either fix the trend error covariances to be `0`
#'(using `VAR`) or estimate them and potentially allow for contemporaneously correlated errors using
#'`VARcor`. For all `VAR` models, stationarity of
#'the latent process is enforced through the prior using the parameterisation given by
#'Heaps (2022). Stationarity is not enforced when using `AR1`, `AR2` or `AR3` models,
#'though this can be changed by the user by specifying lower and upper bounds on autoregressive
#'parameters using functionality in [get_mvgam_priors] and the `priors` argument in
#'[mvgam]
#' @references Sarah E. Heaps (2022) Enforcing stationarity through the prior in Vector Autoregressions.
#' Journal of Computational and Graphical Statistics. 32:1, 1-10.
#' @name mvgam_trends
NULL

#### Generic trend information ####
#' @noRd
trend_model_choices = function(){
  c("RW",
    "GP",
    'AR1',
    'AR2',
    'AR3',
    'VAR1',
    'VAR1cor',
    'None')
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
#' @param Xp_trend optional linear predictor matrix
#' @param betas_trend optional coefficients associated with lp matrix
#' @noRd
sim_ar3 = function(drift = 0,
                   ar1 = 1,
                   ar2 = 0,
                   ar3 = 0,
                   tau = 1,
                   Xp_trend = NULL,
                   betas_trend = NULL,
                   last_trends = rnorm(3),
                   h = 50){
  states <- rep(NA, length = h + 3)
  last_trends <- tail(last_trends, 3)
  states[1] <- last_trends[1]
  states[2] <- last_trends[2]
  states[3] <- last_trends[3]

  errors <- rnorm(h + 3, sd = sqrt(1 / tau))

  if(!is.null(Xp_trend)){
    linpreds <- c(rep(0, 3), as.vector(((matrix(Xp_trend, ncol = NCOL(Xp_trend)) %*%
                    betas_trend)) +
      attr(Xp_trend, 'model.offset')))
  } else {
    linpreds <- rep(0, h + 3)
  }

  for (t in 4:(h + 3)) {
    states[t] <- drift +
      ar1*states[t - 1] +
      ar2*states[t - 2] +
      ar3*states[t - 3] +
      linpreds[t] - linpreds[t -1] +
      errors[t]
  }
  states[-c(1:3)]
}

#' VAR1 simulation function
#' @noRd
sim_var1 = function(drift, A, Sigma,
                    last_trends,
                    Xp_trend = NULL,
                    betas_trend = NULL,
                    h){

  if(NCOL(A) != NCOL(Sigma)){
    stop('VAR coefficient matrix "A" and error matrix "Sigma" must have equal dimensions',
         call. = FALSE)
  }

  if(NROW(A) != NROW(Sigma)){
    stop('VAR coefficient matrix "A" and error matrix "Sigma" must have equal dimensions',
         call. = FALSE)
  }

  if(missing(drift)){
    drift <- rep(0, NROW(A))
  }

  if(length(drift) != NROW(A)){
    stop('Number of drift parameters must match number of rows in VAR coefficient matrix "A"',
         call. = FALSE)
  }

  # Linear predictor, if supplied
  if(!is.null(Xp_trend)){
    linpreds <- as.vector(((matrix(Xp_trend,
                                   ncol = NCOL(Xp_trend)) %*%
                              betas_trend)) +
                            attr(Xp_trend, 'model.offset'))
    linpreds <- matrix(linpreds, ncol = NROW(A),
                       byrow = TRUE)
    linpreds <- rbind(rep(0, NROW(A)),
                      linpreds)
  } else {
    linpreds <- matrix(0, nrow = h + 1, ncol = NROW(A))
  }

  # Last estimate in time is where the series will start
  states <- matrix(NA, nrow = h + 1, ncol = NCOL(A))
  states[1, ] <- last_trends
  errors <- MASS::mvrnorm(h + 1, mu = rep(0, NROW(A)),
                          Sigma = Sigma)

  # Stochastic realisations
  for (t in 2:(h + 1)) {
    states[t, ] <- A %*% states[t - 1,] +
      drift +
      linpreds[t, ] - linpreds[t - 1, ]
      errors[t, ]
  }

  # Return state estimates
  states[-1, ]
}

#' Simulate stationary VAR(p) phi matrices using the algorithm proposed by
#' Ansley and Kohn (1986)
#' @noRd
stationary_VAR_phi <- function(p = 1, n_series = 3, ar_scale = 1) {
  stopifnot(ar_scale > 0)
  Id <- diag(nrow = n_series)
  all_P <- array(dim=c(n_series, n_series, p))
  for(i1 in 1:p) {
    A <- matrix(rnorm(n_series*n_series, sd = ar_scale),
                nrow = n_series)
    B <- t(chol(Id + tcrossprod(A, A)))
    all_P[, , i1] <- solve(B, A)
  }

  all_phi <- array(dim = c(n_series, n_series, p, p))
  all_phi_star <- array(dim = c(n_series, n_series, p, p))

  # Set initial values
  L <- L_star <- Sigma <- Sigma_star <- Gamma <- Id

  # Recursion algorithm (Ansley and Kohn 1986, lemma 2.1)
  for(s in 0:(p - 1)) {
    all_phi[, , s+1, s+1] <- L %*%
      all_P[, , s+1] %*%
      solve(L_star)
    all_phi_star[, , s+1, s+1] <- tcrossprod(L_star, all_P[, , s+1]) %*%
      solve(L)

    if(s >= 1) {
      for(k in 1:s) {
        all_phi[, , s+1, k] <- all_phi[, , s, k] - all_phi[, , s+1, s+1] %*%
          all_phi_star[, , s, s-k+1]
        all_phi_star[, , s+1, k] <- all_phi_star[, , s, k] - all_phi_star[, , s+1, s+1] %*%
          all_phi[, , s, s-k+1]
      }
    }

    if(s < p - 1) {
      Sigma_next <- Sigma - all_phi[, , s+1, s+1] %*%
        tcrossprod(Sigma_star, all_phi[, , s+1, s+1])
      if(s < p + 1) {
        Sigma_star <- Sigma_star - all_phi_star[, , s+1, s+1] %*%
          tcrossprod(Sigma, all_phi_star[, , s+1, s+1])
        L_star <- t(chol(Sigma_star))
      }
      Sigma <- Sigma_next
      L <- t(chol(Sigma))
    }
  }

  out <- vector(mode = 'list')
  for(i in 1:p){
    out[[i]] <- all_phi[,,i,i]
  }

  return(out)
}

#' Parameters to monitor / extract
#' @noRd
trend_par_names = function(trend_model,
                           trend_map,
                           use_lv = FALSE,
                           drift = FALSE){

  # Check arguments
  trend_model <- validate_trend_model(trend_model, drift = drift)

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

    if(trend_model == 'VAR1'){
      param <- c('trend', 'A', 'Sigma',
                 'lv_coefs', 'LV')
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

    if(trend_model == 'VAR1'){
      param <- c('trend', 'A', 'Sigma')
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
                                     trend_map = object$trend_map,
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
    # forward ####

    # Latent trend estimates for dynamic factor models
    if(object$use_lv){
      n_lv <- object$n_lv
      if(object$fit_engine == 'stan'){
        out$last_lvs <- lapply(seq_len(n_lv), function(lv){
          inds_lv <- seq(lv, dim(out$LV)[2], by = n_lv)
          lv_estimates <- out$LV[,inds_lv]
          # Need to only use estimates from the training period
          if(inherits(object$obs_data, 'list')){
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

        if(object$trend_model == 'VAR1'){
          # Need to ensure all series' trends are retained when subsampling
          # to produce draw-specific forecasts from VAR models
          out$last_lvs <- out$last_trends
          out$last_trends <- NULL
        }

      }
    }
  }

  # Return list of extracted posterior parameter samples
  out
}

#' Function for extracting a single draw of trend parameters for use
#' in many of the forecasting / evaluation functions
#' @noRd
extract_general_trend_pars = function(samp_index, trend_pars){
  general_trend_pars <- lapply(seq_along(trend_pars), function(x){

    if(names(trend_pars)[x] %in% c('last_lvs', 'lv_coefs', 'last_trends',
                                   'A', 'Sigma')){

      if(names(trend_pars)[x] %in% c('last_lvs', 'lv_coefs', 'last_trends')){
        out <- unname(lapply(trend_pars[[x]], `[`, samp_index, ))
      }

      if(names(trend_pars)[x] %in% c('A', 'Sigma')){
        out <- unname(trend_pars[[x]][samp_index, ])
      }

    } else {
      if(is.matrix(trend_pars[[x]])){
        out <- unname(trend_pars[[x]][samp_index, ])
      } else {
        out <- unname(trend_pars[[x]][samp_index])
      }
    }
    out

  })
  names(general_trend_pars) <- names(trend_pars)
  return(general_trend_pars)
}

#' Function for extracting a single draw of trend parameters for a single series, for use
#' in many of the forecasting / evaluation functions
#' @noRd
extract_series_trend_pars = function(series, samp_index, trend_pars,
                                     use_lv = FALSE){
  trend_extracts <- lapply(seq_along(trend_pars), function(x){

    if(names(trend_pars)[x] %in% c('last_lvs', 'lv_coefs', 'last_trends',
                                   'A', 'Sigma')){
      if(names(trend_pars)[x] %in% c('last_trends', 'lv_coefs')){
        out <- trend_pars[[x]][[series]][samp_index, ]
      }

      if(names(trend_pars)[x] %in% c('last_lvs')){
        out <- lapply(trend_pars[[x]], `[`, samp_index, )
      }

      if(names(trend_pars)[x] %in% c('A', 'Sigma')){
        out <- trend_pars[[x]][samp_index, ]
      }

    } else {
      if(is.matrix(trend_pars[[x]])){
        if(use_lv){
          out <- trend_pars[[x]][samp_index, ]
        } else {
          out <- trend_pars[[x]][samp_index, series]
        }

      } else {
        out <- trend_pars[[x]][samp_index]
      }
    }
    out

  })
  names(trend_extracts) <- names(trend_pars)
  return(trend_extracts)
}

#' Wrapper function to forecast trends
#' @noRd
forecast_trend = function(trend_model, use_lv, trend_pars,
                          Xp_trend = NULL, betas_trend = NULL,
                          h = 1){

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

        if(!is.null(Xp_trend)){
          inds_keep <- seq(lv, NROW(Xp_trend), by = n_lv)
          Xp_trend_sub = Xp_trend[inds_keep, ]
          attr(Xp_trend_sub, 'model.offset') <- attr(Xp_trend, 'model.offset')
        } else {
          Xp_trend_sub <- NULL
        }

        sim_ar3(drift = ifelse('drift' %in% names(trend_pars),
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
                Xp_trend = Xp_trend_sub,
                betas_trend = betas_trend,
                last_trends = tail(trend_pars$last_lvs[[lv]], 3),
                h = h)
      }))
    }

    if(trend_model == 'GP'){
      next_lvs <- do.call(cbind, lapply(seq_len(n_lv), function(lv){
        sim_gp(alpha_gp = trend_pars$alpha_gp[lv],
                       rho_gp = trend_pars$rho_gp[lv],
                       last_trends = trend_pars$last_lvs[[lv]],
                       h = h)
      }))
    }

    if(trend_model == 'VAR1'){
      # Reconstruct the A and Sigma matrices
      Amat <- matrix(trend_pars$A, nrow = length(trend_pars$last_lvs),
                     ncol = length(trend_pars$last_lvs))
      Sigmamat <- matrix(trend_pars$Sigma, nrow = length(trend_pars$last_lvs),
                         ncol = length(trend_pars$last_lvs))

      # Reconstruct the last trend vector
      last_trendvec <- unlist(lapply(trend_pars$last_lvs,
                                     function(x) tail(x, 1)))

      next_lvs <- sim_var1(A = Amat,
                           Sigma = Sigmamat,
                           last_trends = last_trendvec,
                           Xp_trend = Xp_trend,
                           betas_trend = betas_trend,
                           h = h)
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

      trend_fc <-  sim_ar3(drift = ifelse('drift' %in% names(trend_pars),
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
                           Xp_trend = Xp_trend,
                           betas_trend = betas_trend,
                           last_trends = tail(trend_pars$last_trends, 3),
                           h = h)
    }

    if(trend_model == 'GP'){
      trend_fc <- sim_gp(alpha_gp = trend_pars$alpha_gp,
                                 rho_gp = trend_pars$rho_gp,
                                 last_trends = trend_pars$last_trends,
                                 h = h)
    }

    if(trend_model == 'VAR1'){
      # Reconstruct the A and Sigma matrices
      Amat <- matrix(trend_pars$A, nrow = length(trend_pars$last_lvs),
                     ncol = length(trend_pars$last_lvs))
      Sigmamat <- matrix(trend_pars$Sigma, nrow = length(trend_pars$last_lvs),
                         ncol = length(trend_pars$last_lvs))

      # Reconstruct the last trend vector
      last_trendvec <- unlist(lapply(trend_pars$last_lvs,
                                     function(x) tail(x, 1)))

      trend_fc <- sim_var1(A = Amat,
                           Sigma = Sigmamat,
                           last_trends = last_trendvec,
                           Xp_trend = Xp_trend,
                           betas_trend = betas_trend,
                           h = h)
    }

  }
  return(trend_fc)
}
