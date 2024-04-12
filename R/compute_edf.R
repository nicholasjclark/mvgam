#' Compute approximate EDFs of smooths
#' @importFrom stats fitted
#'@noRd
compute_edf = function(mgcv_model, object, rho_names, sigma_raw_names,
                       conservative = FALSE){

  if(length(mgcv_model$smooth) > 0){

    smooth_labs <- do.call(rbind, lapply(seq_along(mgcv_model$smooth), function(x){
      data.frame(label = mgcv_model$smooth[[x]]$label,
                 term = paste(mgcv_model$smooth[[x]]$term, collapse = ','),
                 class = class(mgcv_model$smooth[[x]])[1])
    }))

    # Find the best overall posterior draw
    if(object$family == 'nmix'){
      best_draw <- 1
    } else {
      liks <- logLik(object, include_forecast = FALSE)
      best_draw <- which.max(rowMeans(liks, na.rm = TRUE))
    }

    # Extract smoothing parameters
    sp_names <- names(mgcv_model$sp)
    rho_sp <- vector()
    random_sp <- vector()

    if(!all(smooth_labs$class == 'random.effect')){
      rho_estimates <- mcmc_chains(object$model_output, rho_names)[best_draw, ]
      rho_sp <- rho_estimates
      names(rho_sp) <- paste0(sp_names[1:length(rho_estimates)],
                              '_',
                              rho_names)
    }

    if(any(smooth_labs$class == 'random.effect')){
      if(length(rho_sp) > 0){
        rho_sp <- rho_sp[-which(smooth_labs$class == 'random.effect')]
      }

      pop_sd_estimates <- mcmc_chains(object$model_output, sigma_raw_names)[best_draw, ]
      random_sp <- pop_sd_estimates
      if(rho_names == 'rho_trend'){
        names(random_sp) <- paste0('sd(',
                                sp_names[which(smooth_labs$class == 'random.effect')],
                                ')_trend')
      } else {
        names(random_sp) <- paste0('sd(',
                                sp_names[which(smooth_labs$class == 'random.effect')],
                                ')')
      }
      names(random_sp) <- gsub('s\\(', '', names(random_sp))
      names(random_sp) <- gsub('\\))', ')', names(random_sp))
    }

    mgcv_model$sp <- exp(c(rho_sp, random_sp))

    # Compute estimated degrees of freedom based on edf.type = 1 from
    # https://github.com/cran/mgcv/blob/master/R/jagam.r
    # using Simon Wood's example calculation
    X <- predict(mgcv_model, type = 'lpmatrix')
    if(rho_names == 'rho_trend'){
      bs <- mcmc_chains(object$model_output, 'b_trend')[best_draw, ]
    } else {
      bs <- mcmc_chains(object$model_output, 'b')[best_draw, ]
    }

    eta <- X %*% bs

    if(rho_names == 'rho_trend'){
      # trend models use Gaussian family; expectations are simply the trend_mus
      mu <- mcmc_chains(object$model_output, 'trend_mus')[best_draw, 1:length(eta)]

    } else {
      # observation models may vary in their response family; need to compute
      # the expectations
      mu <- fitted(object, summary = FALSE)[best_draw, 1:length(eta)]
    }

    # Calculate variance using family's mean-variance relationship
    mu_variance <- predict(object,
                           process_error = FALSE,
                           type = 'variance',
                           summary = FALSE)[best_draw, ]
    if(length(mu_variance) > 1){
      mu_variance <- mu_variance[1:length(eta)]
    }
    if(any(mu_variance == 0)){
      mu_variance[which(mu_variance == 0)] <-
        mu[which(mu_variance == 0)]
    }

    if(!conservative){
      w <- as.numeric(mgcv_model$family$mu.eta(as.vector(eta))^2 / mu_variance)
      XWX <- t(X) %*% (w * X)
    } else XWX <- t(X) %*% X

    lambda <- mgcv_model$sp
    XWXS <- XWX

    for(i in 1:length(lambda)){
      ind <- mgcv_model$off[i]:
        (mgcv_model$off[i] + ncol(mgcv_model$S[[i]]) - 1)
      XWXS[ind, ind] <- XWXS[ind, ind] + mgcv_model$S[[i]] * lambda[i]
    }
    suppressWarnings(edf <- try(diag(solve(XWXS, XWX)), silent = TRUE))
    if(inherits(edf, 'try-error')){
      edf <- mgcv_model$edf
      names(edf) <- names(coef(mgcv_model))
    }
    mgcv_model$edf <- edf
    mgcv_model$edf1 <- edf
    mgcv_model$edf2 <- edf
  }

  # Add frequentist version of parameter covariance estimators
  # for calculation of smooth term p-values;
  # rV <- mroot(mgcv_model$Vp)
  # V <- tcrossprod(rV)
  # XWX <- crossprod(mgcv_model$R)
  # Ve_hat <- V %*% XWX
  # mgcv_model$Ve <- Ve_hat %*% V

  mgcv_model$Ve <- mgcv_model$Vp
  return(mgcv_model)
}
