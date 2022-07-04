#'@title Compute out of sample forecasts for a fitted \code{mvgam} object
#'@name forecast.mvgam
#'@param object \code{list} object returned from \code{mvgam}
#'@param data_test A \code{dataframe} or \code{list} of test data containing at least 'series' and 'time'
#'for the forecast horizon, in addition to any other variables included in the linear predictor of \code{formula}.
#'The covariate information in \code{data_test} will be used to generate forecasts from the fitted model equations. If
#'this same \code{data_test} was originally included in the call to \code{mvgam}, then forecasts have already been
#'produced by the generative model and these will simply be extracted. However if no \code{data_test} was
#'supplied to the original model call, an assumption is made that the \code{data_test} supplied here comes sequentially
#'after the data supplied as \code{data_train} in the original model (i.e. we assume there is no time gap between the last
#'observation of series 1 in \code{data_train} and the first observation for series 1 in \code{data_test}).
#'@param series \code{integer} specifying which series in the set is to be forecast
#'@param type When this has the value \code{link}, the linear predictor is calculated on the log link scale.
#'When \code{response} is used, the predictions take uncertainty in the observation process into account to return
#'predictions on the outcome (discrete) scale (default). When \code{trend} is used, only the forecast distribution for the
#'latent trend is returned.
#'@details Posterior predictions are drawn from the fitted \code{mvgam} and used to simulate a forecast distribution
#'@return A \code{matrix} of the forecast distribution
#'
NULL
#'@export
forecast <- function(x, what, ...){
  UseMethod("forecast")
}

#'@rdname forecast.mvgam
#'@method forecast mvgam
#'@export
forecast.mvgam = function(object, data_test, series = 1,
                          type = 'response'){
  # Check arguments
  if(class(object) != 'mvgam'){
    stop('argument "object" must be of class "mvgam"')
  }

  if(sign(series) != 1){
    stop('argument "series" must be a positive integer',
         call. = FALSE)
  } else {
    if(series%%1 != 0){
      stop('argument "series" must be a positive integer',
           call. = FALSE)
    }
  }

  type <- match.arg(arg = type, choices = c("link", "response", "trend"))

  # Prediction indices for the particular series
  data_train <- object$obs_data
  ends <- seq(0, dim(MCMCvis::MCMCchains(object$model_output, 'ypred'))[2],
              length.out = NCOL(object$ytimes) + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
  ends <- ends[-1]

  if(object$fit_engine == 'stan'){

    # For stan objects, ypred is stored as a vector in column-major order
    preds <- MCMCvis::MCMCchains(object$model_output, 'ypred')[,seq(series,
                                                                    dim(MCMCvis::MCMCchains(object$model_output, 'ypred'))[2],
                                                                    by = NCOL(object$ytimes))]
  } else {
    preds <- MCMCvis::MCMCchains(object$model_output, 'ypred')[,starts[series]:ends[series]]
  }

  # Add variables to data_test if missing
  s_name <- levels(data_train$series)[series]
  if(!missing(data_test)){

    if(!'y' %in% names(data_test)){
      data_test$y <- rep(NA, NROW(data_test))
    }

    if(class(data_test)[1] == 'list'){
      if(!'time' %in% names(data_test)){
        stop('data_train does not contain a "time" column')
      }

      if(!'series' %in% names(data_test)){
        data_test$series <- factor('series1')
      }

    } else {
      if(!'time' %in% colnames(data_test)){
        stop('data_train does not contain a "time" column')
      }

      if(!'series' %in% colnames(data_test)){
        data_test$series <- factor('series1')
      }
    }

  }

  # Function to simulate trends / latent factors ahead using ar3 model
  sim_ar3 = function(phi, ar1, ar2, ar3, tau, state, h){
    states <- rep(NA, length = h + 3)
    states[1] <- state[1]
    states[2] <- state[2]
    states[3] <- state[3]
    for (t in 4:(h + 3)) {
      states[t] <- rnorm(1, phi + ar1*states[t - 1] +
                           ar2*states[t - 2] +
                           ar3*states[t - 3], sqrt(1 / tau))
    }
    states[-c(1:3)]
  }

  # Function to simulate trends ahead using squared exponential GP
  sim_gp = function(alpha_gp, rho_gp, state, h){
    t <- 1:length(state)
    t_new <- 1:(length(state) + h)

    Sigma_new <- alpha_gp^2 * exp(- outer(t, t_new, "-")^2 / (2 * rho_gp^2))
    Sigma_star <- alpha_gp^2 * exp(- outer(t_new, t_new, "-")^2 / (2 * rho_gp^2))
    Sigma <- alpha_gp^2 * exp(- outer(t, t, "-")^2 / (2 * rho_gp^2)) +
      diag(1e-4, length(state))

    tail(t(Sigma_new) %*% solve(Sigma, state), h) +
      tail(MASS::mvrnorm(1, mu = rep(0, dim(Sigma_star - t(Sigma_new) %*% solve(Sigma, Sigma_new))[2]),
                         Sigma = Sigma_star - t(Sigma_new) %*% solve(Sigma, Sigma_new)), h)
  }

  # Extract trend posterior predictions
  if(object$fit_engine == 'stan'){
    trend_estimates <- MCMCvis::MCMCchains(object$model_output, 'trend')[,seq(series,
                                                                              dim(MCMCvis::MCMCchains(object$model_output,
                                                                                                      'trend'))[2],
                                                                              by = NCOL(object$ytimes))]
  } else {
    trend_estimates <- MCMCvis::MCMCchains(object$model_output, 'trend')[,starts[series]:ends[series]]
  }

  # Need to only use estimates from the training period
  end_train <- object$obs_data %>%
    dplyr::filter(series == !!(levels(data_train$series)[series])) %>%
    NROW()
  trend_estimates <- trend_estimates[,1:end_train]

  # Only need last 3 timesteps if this is not a GP trend model
  if(object$trend_model == 'GP'){
    trend_estimates <- trend_estimates
  } else {
    trend_estimates <- trend_estimates[,(NCOL(trend_estimates)-2):(NCOL(trend_estimates))]
  }

  # Generate the linear predictor matrix
  series_test <- data_test %>%
    dplyr::filter(series == s_name) %>%
    dplyr::arrange(time)
  Xp <- predict(object$mgcv_model,
                newdata = series_test,
                type = 'lpmatrix')

  # Beta coefficients for GAM component
  betas <- MCMCvis::MCMCchains(object$model_output, 'b')

  # Family of model
  family <- object$family

  # Negative binomial size estimate
  if(family == 'Negative Binomial'){
    sizes <- MCMCvis::MCMCchains(object$model_output, 'r')
  }

  # Tweedie parameters
  if(family == 'Tweedie'){
    twdiss <- MCMCvis::MCMCchains(object$model_output, 'twdis')
    ps <- matrix(1.5, nrow = NROW(betas), ncol = NCOL(object$ytimes))
  }

  # Latent trend precisions and loadings
  if(object$use_lv){
    taus <- MCMCvis::MCMCchains(object$model_output, 'penalty')

    n_series <- NCOL(object$ytimes)
    n_lv <- object$n_lv
    lv_coefs <- lapply(seq_len(n_series), function(series){
      lv_indices <- seq(1, n_series * n_lv, by = n_series) + (series - 1)
      as.matrix(MCMCvis::MCMCchains(object$model_output, 'lv_coefs')[,lv_indices])
    })
  } else {
    if(object$trend_model %in% c('GP', 'None')){
      taus <- NULL
    } else {
      taus <- MCMCvis::MCMCchains(object$model_output, 'tau')
    }
  }

  # Latent trend estimates
  if(object$use_lv){
    n_lv <- object$n_lv
    ends <- seq(0, dim(MCMCvis::MCMCchains(object$model_output, 'LV'))[2],
                length.out = n_lv + 1)
    starts <- ends + 1
    starts <- c(1, starts[-c(1, (n_lv + 1))])
    ends <- ends[-1]
    lvs <- lapply(seq_len(n_lv), function(lv){
      lv_estimates <- MCMCvis::MCMCchains(object$model_output, 'LV')[,starts[lv]:ends[lv]]

      # Need to only use estimates from the training period
      end_train <- object$obs_data %>%
        dplyr::filter(series == !!(levels(data_train$series)[series])) %>%
        NROW()
      lv_estimates <- lv_estimates[,1:end_train]
      lv_estimates[,(NCOL(lv_estimates)-2):(NCOL(lv_estimates))]
    })

  }

  # Phi estimates for latent trend drift terms
  if(object$drift){
    phis <- MCMCvis::MCMCchains(object$model_output, 'phi')
  } else {
    if(object$use_lv){
      phis <- matrix(0, nrow = NROW(betas), ncol = object$n_lv)
    } else {
      phis <- matrix(0, nrow = NROW(betas), ncol = 1)
    }
  }

  # AR term estimates
  if(object$trend_model == 'GP'){
    alpha_gps <- MCMCvis::MCMCchains(object$model_output, 'alpha_gp')
    rho_gps <- MCMCvis::MCMCchains(object$model_output, 'rho_gp')
    ar1s <- NULL
    ar2s <- NULL
    ar3s <- NULL
  }

  if(object$trend_model == 'RW'){
    if(object$use_lv){
      alpha_gps <- NULL
      rho_gps <- NULL
      ar1s <- matrix(1, nrow = NROW(betas), ncol = object$n_lv)
      ar2s <- matrix(0, nrow = NROW(betas), ncol = object$n_lv)
      ar3s <- matrix(0, nrow = NROW(betas), ncol = object$n_lv)
    } else {
      alpha_gps <- NULL
      rho_gps <- NULL
      ar1s <- matrix(1, nrow = NROW(betas), ncol = 1)
      ar2s <- matrix(0, nrow = NROW(betas), ncol = 1)
      ar3s <- matrix(0, nrow = NROW(betas), ncol = 1)
    }
  }

  if(object$trend_model == 'AR1'){
    ar1s <- MCMCvis::MCMCchains(object$model_output, 'ar1')
    alpha_gps <- NULL
    rho_gps <- NULL
    if(object$use_lv){
      ar2s <- matrix(0, nrow = NROW(betas), ncol = object$n_lv)
      ar3s <- matrix(0, nrow = NROW(betas), ncol = object$n_lv)
    } else {
      ar2s <- matrix(0, nrow = NROW(betas), ncol = 1)
      ar3s <- matrix(0, nrow = NROW(betas), ncol = 1)
    }
  }

  if(object$trend_model == 'AR2'){
    ar1s <- MCMCvis::MCMCchains(object$model_output, 'ar1')
    ar2s <- MCMCvis::MCMCchains(object$model_output, 'ar2')
    alpha_gps <- NULL
    rho_gps <- NULL

    if(object$use_lv){
      ar3s <- matrix(0, nrow = NROW(betas), ncol = object$n_lv)
    } else {
      ar3s <- matrix(0, nrow = NROW(betas), ncol = 1)
    }
  }

  if(object$trend_model == 'AR3'){
    ar1s <- MCMCvis::MCMCchains(object$model_output, 'ar1')
    ar2s <- MCMCvis::MCMCchains(object$model_output, 'ar2')
    ar3s <- MCMCvis::MCMCchains(object$model_output, 'ar3')
    alpha_gps <- NULL
    rho_gps <- NULL
  }


  # Produce forecasts
  fc_preds <- do.call(rbind, lapply(seq_len(dim(betas)[1]), function(i){
    if(object$use_lv){
      # Sample a last state estimate for the latent variables
      samp_index <- i
      last_lvs <- lapply(seq_along(lvs), function(lv){
        lvs[[lv]][samp_index, ]
      })

      # Sample drift and AR parameters
      phi <- phis[samp_index, ]
      ar1 <- ar1s[samp_index, ]
      ar2 <- ar2s[samp_index, ]
      ar3 <- ar3s[samp_index, ]

      # Sample lv precision
      tau <- taus[samp_index,]

      # Sample lv loadings
      lv_coefs <- lv_coefs[[series]][samp_index,]


      # Sample beta coefs
      betas <- betas[samp_index, ]

      # Family-specific parameters
      if(family == 'Negative Binomial'){
        size <- sizes[samp_index, ]
      }

      if(family == 'Tweedie'){
        twdis <- twdis[samp_index, ]
        p <- p[samp_index]
      }

      # Run the latent variables forward h timesteps timestep
      next_lvs <- do.call(cbind, lapply(seq_along(lvs), function(lv){
        sim_ar3(phi[lv], ar1[lv], ar2[lv], ar3[lv], tau[lv], last_lvs[[lv]], NROW(series_test))
      }))

      # Multiply lv states with loadings to generate each series' forecast trend state
      trends <- as.numeric(next_lvs %*% t(lv_coefs))


      if(type == 'trend'){
        out <- trends
        } else {

        # Calculate predictions
        if(family == 'Negative Binomial'){
          out <- rnbinom(NROW(series_test), size = size[series],
                         mu = exp(((Xp %*% betas)) + (trends)))
        }

        if(family == 'Poisson'){
          out <- rpois(NROW(series_test), lambda = exp(((Xp %*% betas)) + (trends)))
        }

        if(family == 'Tweedie'){
          out <- mgcv::rTweedie(mu = exp(((Xp %*% betas)) + (trends)),
                                p = p, phi = twdis[series])
        }
      }


    } else {

      # Sample index for the particle
      samp_index <- i

      # Sample beta coefs
      betas <- betas[samp_index, ]

      # Sample last state estimates for the trends
      last_trends <- trend_estimates[i,]

      if(is.null(alpha_gps)){
        # Sample AR parameters
        phi <- phis[samp_index, ]
        ar1 <- ar1s[samp_index, ]
        ar2 <- ar2s[samp_index, ]
        ar3 <- ar3s[samp_index, ]
        alpha_gp <- rho_gp <- 0

        # Sample trend precisions
        tau <- taus[samp_index,]
      } else {
        phi <- ar1 <- ar2 <- ar3 <- tau <- 0
        alpha_gp <- alpha_gps[samp_index, ]
        rho_gp <- rho_gps[samp_index, ]
      }

      # Family-specific parameters
      if(family == 'Negative Binomial'){
        size <- sizes[samp_index, ]
      }

      if(family == 'Tweedie'){
        twdis <- twdis[samp_index, ]
        p <- p[samp_index]
      }

      # Run the trends forward
      if(is.null(alpha_gps)){
        trends <- sim_ar3(phi = phi[series],
                          ar1 = ar1[series],
                          ar2 = ar2[series],
                          ar3 = ar3[series],
                          tau = tau[series],
                          h = NROW(series_test),
                          state = last_trends)

      } else {
        t <- 1:length(last_trends)
        t_new <- 1:(length(last_trends) + NROW(series_test))


        Sigma_new <- alpha_gp[series]^2 * exp(- outer(t, t_new, "-")^2 / (2 * rho_gp[series]^2))
        Sigma_star <- alpha_gp[series]^2 * exp(- outer(t_new, t_new, "-")^2 / (2 * rho_gp[series]^2))
        Sigma <- alpha_gp[series]^2 * exp(- outer(t, t, "-")^2 / (2 * rho_gp[series]^2)) +
          diag(1e-4, length(last_trends))

        trends <- as.vector(tail(t(Sigma_new) %*% solve(Sigma, last_trends), NROW(series_test)) +
          tail(MASS::mvrnorm(1, mu = rep(0, dim(Sigma_star - t(Sigma_new) %*% solve(Sigma, Sigma_new))[2]),
                             Sigma = Sigma_star -
                               t(Sigma_new) %*% solve(Sigma, Sigma_new)), NROW(series_test)))
      }

      if(type == 'trend'){
        out <- trends
      } else {

        # Calculate predictions
        if(family == 'Negative Binomial'){
          out <- rnbinom(NROW(series_test), size = size[series],
                         mu = exp(((Xp %*% betas)) + (trends)))
        }

        if(family == 'Poisson'){
          out <- rpois(NROW(series_test), lambda = exp(((Xp %*% betas)) + (trends)))
        }

        if(family == 'Tweedie'){
          out <- mgcv::rTweedie(mu = exp(((Xp %*% betas)) + (trends)),
                                p = p, phi = twdis[series])
        }
      }

    }

    out
  }))

  return(fc_preds)
}
