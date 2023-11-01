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
#'   \item \code{VARMA} Vector Autoregressive model with VAR coefficients for lag 1 and MA coefficients for lag 1; contemporaneously correlated process errors
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
  # Will make the commented out versions available soon
  c("RW",
    "RWMA",
    "RWcor",
    "RWMAcor",
    "GP",
    'AR1',
    'AR1MA',
    'AR1cor',
    'AR1MAcor',
    'AR2',
    'AR2MA',
    'AR2cor',
    'AR2MAcor',
    'AR3',
    'AR3MA',
    'AR3cor',
    'AR3MAcor',
    'VAR',
    'VARcor',
    'VAR1',
    'VAR1cor',
    'VARMA',
    'VARMAcor',
    'VARMA1,1cor',
    'None')
}

# Additions needed for adding moving average / correlated process errors
#' @noRd
ma_cor_additions = function(trend_model){
  use_var1 <- use_var1cor <- add_ma <- add_cor <- FALSE
  if(grepl('MA', trend_model, fixed = TRUE)) add_ma <- TRUE

  if(trend_model == 'RWMA') trend_model <- 'RW'

  if(trend_model == 'AR1MA') trend_model <- 'AR1'

  if(trend_model == 'AR2MA') trend_model <- 'AR2'

  if(trend_model == 'AR3MA') trend_model <- 'AR3'

  if(trend_model %in% c('RWcor', 'RWMAcor')){
    add_cor <- TRUE
    trend_model <- 'RW'
  }

  if(trend_model %in% c('AR1cor', 'AR1MAcor')){
    add_cor <- TRUE
    trend_model <- 'AR1'
  }

  if(trend_model %in% c('AR2cor', 'AR2MAcor')){
    add_cor <- TRUE
    trend_model <- 'AR2'
  }

  if(trend_model %in% c('AR3cor', 'AR3MAcor')){
    add_cor <- TRUE
    trend_model <- 'AR3'
  }

  if(trend_model == 'VAR1') use_var1 <- TRUE

  if(trend_model %in% c('VAR1cor', 'VARMA1,1cor')){
    use_var1cor <- TRUE
    trend_model <- 'VAR1'
  }

  return(list(trend_model = trend_model,
              use_var1 = use_var1,
              use_var1cor = use_var1cor,
              add_ma = add_ma,
              add_cor = add_cor))
}

#' Squared exponential GP simulation function
#' @param last_trends Vector of trend estimates leading up to the current timepoint
#' @param h \code{integer} specifying the forecast horizon
#' @param rho_gp length scale parameter
#' @param alpha_gp marginal variation parameter
#' @noRd
sim_gp = function(last_trends, h, rho_gp, alpha_gp){

  t <- as.numeric(1:length(last_trends))
  t_new <- as.numeric(1:(length(last_trends) + h))
  Sigma_new <- alpha_gp^2 *
    exp(-0.5 * ((outer(t, t_new, "-") / rho_gp) ^ 2))
  Sigma_star <- alpha_gp^2 *
    exp(-0.5 * ((outer(t_new, t_new, "-") / rho_gp) ^ 2)) +
    diag(1e-4, length(t_new))
  Sigma <- alpha_gp^2 *
    exp(-0.5 * ((outer(t, t, "-") / rho_gp) ^ 2)) +
    diag(1e-4, length(t))

  as.vector(tail(t(Sigma_new) %*% solve(Sigma, last_trends),
                 h) +
              tail(mvnfast::rmvn(1,
                                 mu = rep(0, length(t_new)),
                                 sigma = Sigma_star - t(Sigma_new) %*%
                                   solve(Sigma, Sigma_new))[1,],
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

  # Draw errors
  errors <- rnorm(h + 3, sd = sqrt(1 / tau))

  # Prepare linear predictors (if necessary)
  if(!is.null(Xp_trend)){
    linpreds <- c(rep(0, 3), as.vector(((matrix(Xp_trend, ncol = NCOL(Xp_trend)) %*%
                    betas_trend)) +
      attr(Xp_trend, 'model.offset')))
  } else {
    linpreds <- rep(0, h + 3)
  }

  # Propagate the process
  ar3_recursC(drift = drift,
              ar1 = ar1,
              ar2 = ar2,
              ar3 = ar3,
              linpreds = linpreds,
              h = h,
              errors = errors,
              last_trends = tail(last_trends, 3))
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

  # Draw errors
  errors <- mvnfast::rmvn(h + 1, mu = rep(0, NROW(A)),
                          sigma = Sigma)

  # Stochastic realisations
  var1_recursC(A = A,
               drift = drift,
               linpreds = linpreds,
               errors = errors,
               last_trends = last_trends,
               h = h)
}

#' VARMA(1-3, 0-1) simulation function
#' @noRd
sim_varma = function(drift,
                     A,
                     A2,
                     A3,
                     theta,
                     Sigma,
                     last_trends,
                     last_errors,
                     Xp_trend = NULL,
                     betas_trend = NULL,
                     h){

  # Validate dimensions
  validate_equaldims(A, Sigma)
  validate_equaldims(A2, Sigma)
  validate_equaldims(A3, Sigma)
  validate_equaldims(theta, Sigma)

  if(NROW(last_trends) != 3){
    stop('Last 3 state estimates are required, in matrix form',
         call. = FALSE)
  }

  if(NROW(last_errors) != 3){
    stop('Last 3 error estimates are required, in matrix form',
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
    if(NROW(linpreds) != h + 3){
      stop('trend linear predictor matrix should be h + 3 rows in dimension',
           call. = FALSE)
    }
  } else {
    linpreds <- matrix(0, nrow = h + 3, ncol = NROW(A))
  }

  # Draw forecast errors
  errors <- rbind(last_errors,
                  mvnfast::rmvn(h,
                                mu = rep(0, NROW(A)),
                                sigma = Sigma))

  # Stochastic realisations
  varma_recursC(A = A,
                A2 = A2,
                A3 = A3,
                theta = theta,
                drift = drift,
                linpreds = linpreds,
                errors = errors,
                last_trends = last_trends,
                h = h)
}

#' Generic function to take outputs from different trend models
#' and prepare the objects needed to propagate VARMA(3,1) processes
#' so that only a single Rcpp function is needed for propagating
#' autoregressive trends
#' @noRd
prep_varma_params = function(ar1,
                             ar2,
                             ar3,
                             A,
                             A2,
                             A3,
                             drift,
                             theta,
                             Sigma,
                             tau,
                             Xp_trend,
                             betas_trend,
                             last_trends,
                             last_errors,
                             h){

  # Construct Autoregressive matrices
  if(missing(A) & missing(A2) & missing(A3)){
    A <- A2 <- A3 <- matrix(0,
                            ncol = length(ar1),
                            nrow = length(ar1))
    diag(A) <- ar1
    diag(A2) <- ar2
    diag(A3) <- ar3
  }

  if(missing(A2) & !missing(A)){
    A2 <- matrix(0,
                 ncol = NROW(A),
                 nrow = NROW(A))
  }

  if(missing(A3) & !missing(A)){
    A3 <- matrix(0,
                 ncol = NROW(A),
                 nrow = NROW(A))
  }

  # Drift terms
  if(missing(drift)){
    drift <- rep(0, NROW(A))
  }

  # Construct moving average matrix
  if(missing(theta)){
    theta <- matrix(0,
                    ncol = NROW(A),
                    nrow = NROW(A))
  }

  if(!is.matrix(theta)){
    theta2 <- matrix(0,
                     ncol = NROW(A),
                     nrow = NROW(A))
    diag(theta2) <- theta
    theta <- theta2
  }

  # Construct covariance matrix
  if(missing(Sigma)){
    Sigma <- matrix(0,
                    ncol = NROW(A),
                    nrow = NROW(A))
    diag(Sigma) <- 1 / tau
  }

  # Construct last trend estimates
  if(!is.matrix(last_trends) | NROW(last_trends) == 1){
    last_trends <- matrix(last_trends)
  }

  # Construct last error estimates
  if(missing(last_errors)){
    last_errors <- matrix(0,
                          ncol = NROW(A),
                          nrow = 3)
  }

  if(!is.matrix(last_errors) | NROW(last_errors) == 1){
    last_errors <- matrix(last_errors)
  }

  return(list(A = A,
              A2 = A2,
              A3 = A3,
              drift = drift,
              theta = theta,
              Sigma = Sigma,
              last_trends = last_trends,
              last_errors = last_errors,
              Xp_trend = Xp_trend,
              betas_trend = betas_trend,
              h = h))
}

#' Simulate stationary VAR(p) phi matrices using the algorithm proposed by
#' Ansley and Kohn (1986)
#' @noRd
stationary_VAR_phi <- function(p = 1, n_series = 3, ar_scale = 1) {
  stopifnot(ar_scale > 0)
  Id <- diag(nrow = n_series)
  all_P <- array(dim=c(n_series, n_series, p))
  for(i in 1:p) {
    A <- matrix(rnorm(n_series*n_series, sd = ar_scale),
                nrow = n_series)

    # Enforce diagonal AR terms to be positive if this is
    # the first phi matrix
    if(i == 1) diag(A) <- abs(diag(A))
    B <- t(chol(Id + tcrossprod(A, A)))
    all_P[, , i] <- solve(B, A)
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
      param <- c('trend', 'LV', 'penalty',
                 'lv_coefs', 'theta', 'Sigma', 'error')
    }

    if(trend_model == 'AR1'){
      param <- c('trend', 'ar1', 'LV', 'penalty',
                 'lv_coefs', 'theta', 'Sigma', 'error')
    }

    if(trend_model == 'AR2'){
      param <- c('trend', 'ar1', 'ar2', 'LV',
                 'penalty', 'lv_coefs', 'theta', 'Sigma', 'error')
    }

    if(trend_model == 'AR3'){
      param <- c('trend', 'ar1', 'ar2', 'ar3',
                 'LV', 'penalty', 'lv_coefs', 'theta',
                 'Sigma', 'error')
    }

    if(trend_model == 'GP'){
      param <- c('trend', 'alpha_gp', 'rho_gp',
                 'LV', 'lv_coefs', 'b_gp')
    }

    if(trend_model == 'VAR1'){
      param <- c('trend', 'A', 'Sigma',
                 'lv_coefs', 'LV', 'P_real', 'sigma',
                 'theta', 'error')
    }

  }

  if(!use_lv){
    if(trend_model == 'RW'){
      param <- c('trend', 'tau', 'sigma', 'theta', 'Sigma', 'error')
    }

    if(trend_model == 'AR1'){
      param <- c('trend', 'tau', 'sigma', 'ar1',
                 'theta', 'Sigma', 'error')
    }

    if(trend_model == 'AR2'){
      param <- c('trend', 'tau', 'sigma', 'ar1', 'ar2',
                 'theta', 'Sigma', 'error')
    }

    if(trend_model == 'AR3'){
      param <- c('trend', 'tau', 'sigma', 'ar1', 'ar2',
                 'ar3', 'theta', 'Sigma', 'error')
    }

    if(trend_model == 'GP'){
      param <- c('trend', 'alpha_gp', 'rho_gp', 'b_gp')
    }

    if(trend_model == 'VAR1'){
      param <- c('trend', 'A', 'Sigma', 'P_real',
                 'sigma', 'theta', 'error')
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
  pars_to_extract <- trend_par_names(trend_model = attr(object$model_data, 'trend_model'),
                                     trend_map = object$trend_map,
                                     use_lv = object$use_lv,
                                     drift = object$drift)

  # Extract into a named list
  if(length(pars_to_extract) > 0){
    out <- vector(mode = 'list')
    included <- vector(length = length(pars_to_extract))
    for(i in 1:length(pars_to_extract)){

      # Check if it can be extracted first
      suppressWarnings(estimates <- try(mcmc_chains(object$model_output,
                                                    params = pars_to_extract[i]),
                                        silent = TRUE))

      if(inherits(estimates, 'try-error')){
        included[i] <- FALSE
      } else {
        included[i] <- TRUE
        out[[i]] <- estimates
      }
    }
    out <- out[included]
    names(out) <- pars_to_extract[included]

  } else {
    out <- list()
  }

  # Latent trend loadings for dynamic factor models
  if(object$use_lv){
    if(attr(object$model_data, 'trend_model') %in% c('RW', 'AR1', 'AR2', 'AR3')){
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
    if(attr(object$model_data, 'trend_model') %in% c('RW', 'AR1', 'AR2', 'AR3')){
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

          if(attr(object$model_data, 'trend_model') == 'GP'){
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
      if(attr(object$model_data, 'trend_model') != 'None'){
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
          if(attr(object$model_data, 'trend_model') == 'GP'){
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

        if(attr(object$model_data, 'trend_model') == 'VAR1'){
          # Need to ensure all series' trends are retained when subsampling
          # to produce draw-specific forecasts from VAR models
          out$last_lvs <- out$last_trends
          out$last_trends <- NULL
        }

      }
    }
  }

  # Extract centred training times and number of GP basis functions
  # if this is a GP model
  if(attr(object$model_data, 'trend_model') == 'GP'){
    num_basis_line <- object$model_file[grep('num_gp_basis = ',
                                             object$model_file)]
    out$num_gp_basis <- as.numeric(unlist(regmatches(num_basis_line,
                                                 gregexpr("[[:digit:]]+",
                                                          num_basis_line))))
    out$mean_time <- mean(unique(object$obs_data$time))
    out$time_cent <- unique(object$obs_data$time) - mean(unique(object$obs_data$time))

    # Get the basis coefficients in the correct format
    n_series <- NCOL(object$ytimes)

    if(object$use_lv){
      n_lv <- object$n_lv
      all_bgps <- out$b_gp
      out$b_gp <- lapply(seq_len(n_lv), function(lv){
        all_bgps[,seq(lv,
                      NCOL(all_bgps),
                      by = NCOL(out$alpha_gp))]

      })

    } else {
      all_bgps <- out$b_gp
      out$b_gp <- lapply(seq_len(n_series), function(series){
        all_bgps[,seq(series,
                      NCOL(all_bgps),
                      by = NCOL(out$alpha_gp))]

      })
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
                                   'A', 'Sigma', 'theta', 'b_gp', 'error')){

      if(names(trend_pars)[x] %in% c('last_lvs', 'lv_coefs', 'last_trends',
                                     'b_gp')){
        out <- unname(lapply(trend_pars[[x]], `[`, samp_index, ))
      }

      if(names(trend_pars)[x] %in% c('A', 'Sigma', 'theta', 'error')){
        out <- unname(trend_pars[[x]][samp_index, ])
      }

    } else if(names(trend_pars)[x] %in% c('time_cent', 'mean_time')){
      out <- trend_pars[[x]]
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
                                   'A', 'Sigma', 'theta', 'b_gp', 'error')){

      if(!use_lv & names(trend_pars)[x] == 'b_gp'){
        out <- trend_pars[[x]][[series]][samp_index, ]
      }

      if(use_lv & names(trend_pars)[x] == 'b_gp'){
        out <- lapply(trend_pars[[x]], `[`, samp_index, )
      }

      if(names(trend_pars)[x] %in% c('last_trends', 'lv_coefs')){
        out <- trend_pars[[x]][[series]][samp_index, ]
      }

      if(names(trend_pars)[x] %in% c('last_lvs')){
        out <- lapply(trend_pars[[x]], `[`, samp_index, )
      }

      if(names(trend_pars)[x] %in% c('A', 'Sigma', 'theta', 'error')){
        out <- trend_pars[[x]][samp_index, ]
      }

    } else if(names(trend_pars)[x] %in% c('time_cent', 'mean_time')){
      out <- trend_pars[[x]]
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
                          h = 1, time = NULL){

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
          attr(Xp_trend_sub, 'model.offset') <- attr(Xp_trend, 'model.offset')[inds_keep]
          attr(Xp_trend_sub, 'model.offset')[is.na(attr(Xp_trend_sub, 'model.offset'))] <- 0
        } else {
          Xp_trend_sub <- NULL
        }

        # Prep VARMA parameters
        if('Sigma' %in% names(trend_pars)){
          Sigma <- trend_pars$Sigma
        } else {
          Sigma <- rlang::missing_arg()
        }
        varma_params <- prep_varma_params(drift = ifelse('drift' %in% names(trend_pars),
                                                         trend_pars$drift[lv],
                                                         0),
                                          ar1 = ar1,
                                          ar2 = ifelse('ar2' %in% names(trend_pars),
                                                       trend_pars$ar2[lv],
                                                       0),
                                          ar3 = ifelse('ar3' %in% names(trend_pars),
                                                       trend_pars$ar3[lv],
                                                       0),
                                          theta = ifelse('theta' %in% names(trend_pars),
                                                         trend_pars$theta[lv],
                                                         0),
                                          tau = trend_pars$tau[lv],
                                          Sigma = Sigma,
                                          Xp_trend = Xp_trend_sub,
                                          betas_trend = betas_trend,
                                          last_trends = tail(trend_pars$last_lvs[[lv]], 3),
                                          h = h)

        # Propagate forward
        sim_varma(A = varma_params$A,
                  A2 = varma_params$A2,
                  A3 = varma_params$A3,
                  drift = varma_params$drift,
                  theta = varma_params$theta,
                  Sigma = varma_params$Sigma,
                  last_trends = varma_params$last_trends,
                  last_errors = varma_params$last_errors,
                  Xp_trend = varma_params$Xp_trend,
                  betas_trend = varma_params$betas_trend,
                  h = varma_params$h)
      }))
    }

    if(trend_model == 'GP'){
      next_lvs <- do.call(cbind, lapply(seq_len(n_lv), function(lv){
        sim_gp(alpha_gp = trend_pars$alpha_gp[lv],
                       rho_gp = trend_pars$rho_gp[lv],
                       last_trends = trend_pars$last_lvs[[lv]],
                       h = h)
        # Before using the Hilbert version, we need to sign flip
        # the b_gp coefficients in the actual model
        # sim_hilbert_gp(alpha_gp = trend_pars$alpha_gp[lv],
        #                rho_gp = trend_pars$rho_gp[lv],
        #                b_gp = trend_pars$b_gp[[lv]],
        #                last_trends = trend_pars$last_lvs[[lv]],
        #                fc_times = time,
        #                train_times = trend_pars$time_cent,
        #                mean_train_times = trend_pars$mean_time)
      }))
    }

    if(trend_model == 'VAR1'){
      # Reconstruct the A and Sigma matrices
      if('A' %in% names(trend_pars)){
        Amat <- matrix(trend_pars$A, nrow = length(trend_pars$last_lvs),
                       ncol = length(trend_pars$last_lvs),
                       byrow = TRUE)
        ar1 <- rlang::missing_arg()
      } else if('ar1' %in% names(trend_pars)){
        ar1 <- trend_pars$ar1
        Amat <- rlang::missing_arg()
      } else {
        ar1 <- rep(1, length(trend_pars$last_lvs))
        Amat <- rlang::missing_arg()
      }

      if('ar2' %in% names(trend_pars)){
        ar2 <- trend_pars$ar2
      } else {
        ar2 <- rep(0, length(trend_pars$last_lvs))
      }

      if('ar3' %in% names(trend_pars)){
        ar3 <- trend_pars$ar3
      } else {
        ar3 <- rep(0, length(trend_pars$last_lvs))
      }

      if('Sigma' %in% names(trend_pars)){
        Sigmamat <- matrix(trend_pars$Sigma, nrow = length(trend_pars$last_lvs),
                           ncol = length(trend_pars$last_lvs),
                           byrow = TRUE)
      } else if('sigma' %in% names(trend_pars)){
        Sigmamat <- matrix(0, nrow = length(trend_pars$last_lvs),
                           ncol = length(trend_pars$last_lvs),
                           byrow = TRUE)
        diag(Sigmamat) <- trend_pars$sigma
      } else {
        Sigmamat <- matrix(0, nrow = length(trend_pars$last_lvs),
                           ncol = length(trend_pars$last_lvs),
                           byrow = TRUE)
        diag(Sigmamat) <- 1 / trend_pars$tau
      }


      # Reconstruct the last trend matrix
      last_trendmat <- do.call(cbind,(lapply(trend_pars$last_lvs,
                                             function(x) tail(x, 3))))

      # If this is a moving average model, reconstruct theta matrix and
      # last error matrix
      if('theta' %in% names(trend_pars)){
        thetamat <- matrix(trend_pars$theta,
                           nrow = length(trend_pars$last_lvs),
                           ncol = length(trend_pars$last_lvs),
                           byrow = TRUE)
        errormat <- rbind(rep(0, length(trend_pars$last_lvs)),
                          rep(0, length(trend_pars$last_lvs)),
                          tail(trend_pars$error, length(trend_pars$last_lvs)))

      } else {
        thetamat <- rlang::missing_arg()
        errormat <- rlang::missing_arg()
      }

      # Prep VARMA parameters
      varma_params <- prep_varma_params(A = Amat,
                                        ar1 = ar1,
                                        ar2 = ar2,
                                        ar3 = ar3,
                                        Sigma = Sigmamat,
                                        last_trends = last_trendmat,
                                        last_errors = errormat,
                                        theta = thetamat,
                                        Xp_trend = Xp_trend,
                                        betas_trend = betas_trend,
                                        h = h)

      next_lvs <- sim_varma(A = varma_params$A,
                            A2 = varma_params$A2,
                            A3 = varma_params$A3,
                            drift = varma_params$drift,
                            theta = varma_params$theta,
                            Sigma = varma_params$Sigma,
                            last_trends = varma_params$last_trends,
                            last_errors = varma_params$last_errors,
                            Xp_trend = varma_params$Xp_trend,
                            betas_trend = varma_params$betas_trend,
                            h = varma_params$h)

      # next_lvs <- sim_var1(A = Amat,
      #                      Sigma = Sigmamat,
      #                      last_trends = last_trendvec,
      #                      Xp_trend = Xp_trend,
      #                      betas_trend = betas_trend,
      #                      h = h)
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

      # Construct VARMA parameters
      if('Sigma' %in% names(trend_pars)){
        Sigma <- trend_pars$sigma
      } else {
        Sigma <- rlang::missing_arg()
      }
      varma_params <- prep_varma_params(drift = ifelse('drift' %in% names(trend_pars),
                                                       trend_pars$drift,
                                                       0),
                                        ar1 = ar1,
                                        ar2 = ifelse('ar2' %in% names(trend_pars),
                                                     trend_pars$ar2,
                                                     0),
                                        ar3 = ifelse('ar3' %in% names(trend_pars),
                                                     trend_pars$ar3,
                                                     0),
                                        theta = ifelse('theta' %in% names(trend_pars),
                                                     trend_pars$theta,
                                                     0),
                                        Sigma = Sigma,
                                        tau = trend_pars$tau,
                                        Xp_trend = Xp_trend,
                                        betas_trend = betas_trend,
                                        last_trends = tail(trend_pars$last_trends, 3),
                                        h = h)

      # Propagate forward
      trend_fc <- sim_varma(A = varma_params$A,
                            A2 = varma_params$A2,
                            A3 = varma_params$A3,
                            drift = varma_params$drift,
                            theta = varma_params$theta,
                            Sigma = varma_params$Sigma,
                            last_trends = varma_params$last_trends,
                            last_errors = varma_params$last_errors,
                            Xp_trend = varma_params$Xp_trend,
                            betas_trend = varma_params$betas_trend,
                            h = varma_params$h)

      # trend_fc <-  sim_ar3(drift = ifelse('drift' %in% names(trend_pars),
      #                                     trend_pars$drift,
      #                                     0),
      #                      ar1 = ar1,
      #                      ar2 = ifelse('ar2' %in% names(trend_pars),
      #                                   trend_pars$ar2,
      #                                   0),
      #                      ar3 = ifelse('ar3' %in% names(trend_pars),
      #                                   trend_pars$ar3,
      #                                   0),
      #                      tau = trend_pars$tau,
      #                      Xp_trend = Xp_trend,
      #                      betas_trend = betas_trend,
      #                      last_trends = tail(trend_pars$last_trends, 3),
      #                      h = h)
    }

    if(trend_model == 'GP'){
      # trend_fc <- sim_gp(alpha_gp = trend_pars$alpha_gp,
      #                            rho_gp = trend_pars$rho_gp,
      #                            last_trends = trend_pars$last_trends,
      #                            h = h)
      #
      trend_fc <- sim_hilbert_gp(alpha_gp = trend_pars$alpha_gp,
                                 rho_gp = trend_pars$rho_gp,
                                 b_gp = trend_pars$b_gp,
                                 last_trends = trend_pars$last_trends,
                                 fc_times = time,
                                 train_times = trend_pars$time_cent,
                                 mean_train_times = trend_pars$mean_time)
    }

    if(trend_model == 'VAR1'){
      # Reconstruct the A and Sigma matrices
      if('A' %in% names(trend_pars)){
        Amat <- matrix(trend_pars$A, nrow = length(trend_pars$last_lvs),
                       ncol = length(trend_pars$last_lvs),
                       byrow = TRUE)
        ar1 <- rlang::missing_arg()
      } else if('ar1' %in% names(trend_pars)){
        ar1 <- trend_pars$ar1
        Amat <- rlang::missing_arg()
      } else {
        ar1 <- rep(1, length(trend_pars$last_lvs))
        Amat <- rlang::missing_arg()
      }

      if('ar2' %in% names(trend_pars)){
        ar2 <- trend_pars$ar2
      } else {
        ar2 <- rep(0, length(trend_pars$last_lvs))
      }

      if('ar3' %in% names(trend_pars)){
        ar3 <- trend_pars$ar3
      } else {
        ar3 <- rep(0, length(trend_pars$last_lvs))
      }

      if('Sigma' %in% names(trend_pars)){
        Sigmamat <- matrix(trend_pars$Sigma, nrow = length(trend_pars$last_lvs),
                           ncol = length(trend_pars$last_lvs),
                           byrow = TRUE)
      } else if('sigma' %in% names(trend_pars)){
        Sigmamat <- matrix(0, nrow = length(trend_pars$last_lvs),
                           ncol = length(trend_pars$last_lvs),
                           byrow = TRUE)
        diag(Sigmamat) <- trend_pars$sigma
      } else {
        Sigmamat <- matrix(0, nrow = length(trend_pars$last_lvs),
                           ncol = length(trend_pars$last_lvs),
                           byrow = TRUE)
        diag(Sigmamat) <- 1 / sqrt(trend_pars$tau)
      }


      # Reconstruct the last trend matrix
      last_trendmat <- do.call(cbind,(lapply(trend_pars$last_lvs,
                                             function(x) tail(x, 3))))

      # If this is a moving average model, reconstruct theta matrix and
      # last error matrix
      if('theta' %in% names(trend_pars)){
        thetamat <- matrix(trend_pars$theta,
                           nrow = length(trend_pars$last_lvs),
                           ncol = length(trend_pars$last_lvs),
                           byrow = TRUE)
        errormat <- rbind(rep(0, length(trend_pars$last_lvs)),
                          rep(0, length(trend_pars$last_lvs)),
                          tail(trend_pars$error, length(trend_pars$last_lvs)))

      } else {
        thetamat <- rlang::missing_arg()
        errormat <- rlang::missing_arg()
      }

      # Prep VARMA parameters
      varma_params <- prep_varma_params(A = Amat,
                                        ar1 = ar1,
                                        ar2 = ar2,
                                        ar3 = ar3,
                                        Sigma = Sigmamat,
                                        last_trends = last_trendmat,
                                        last_errors = errormat,
                                        theta = thetamat,
                                        Xp_trend = Xp_trend,
                                        betas_trend = betas_trend,
                                        h = h)
      # Propagate forward
      trend_fc <- sim_varma(A = varma_params$A,
                            A2 = varma_params$A2,
                            A3 = varma_params$A3,
                            drift = varma_params$drift,
                            theta = varma_params$theta,
                            Sigma = varma_params$Sigma,
                            last_trends = varma_params$last_trends,
                            last_errors = varma_params$last_errors,
                            Xp_trend = varma_params$Xp_trend,
                            betas_trend = varma_params$betas_trend,
                            h = varma_params$h)

      # trend_fc <- sim_var1(A = Amat,
      #                      Sigma = Sigmamat,
      #                      last_trends = last_trendvec,
      #                      Xp_trend = Xp_trend,
      #                      betas_trend = betas_trend,
      #                      h = h)
    }

  }
  return(trend_fc)
}
