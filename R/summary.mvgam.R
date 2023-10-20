#'Summary for a fitted mvgam object
#'
#'These functions take a fitted \code{mvgam} object and return various useful summaries
#'@importFrom stats printCoefmat
#'@param object \code{list} object of class `mvgam`
#'@param include_betas Logical. Print a summary that includes posterior summaries
#'of all linear predictor beta coefficients (including spline coefficients)?
#'Defaults to \code{FALSE} for a more concise summary
#'@param digits The number of significant digits for printing out the summary;
#'  defaults to \code{2}.
#'@param ... Ignored
#'@author Nicholas J Clark
#'@details `summary.mvgam` and `summary.mvgam_prefit` return brief summaries of the model's call, along with posterior intervals for
#'some of the key parameters in the model. Note that some smooths have extra penalties on the null space,
#'so summaries for the \code{rho} parameters may include more penalty terms than the number of smooths in
#'the original model formula. Approximate p-values for smooth terms are also returned, with methods used for their
#'calculation following those used for `mgcv` equivalents (see \code{\link[mgcv]{summary.gam}} for details).
#'The Estimated Degrees of Freedom (edf) for smooth terms is computed using
#'`edf.type = 1` as described in the documentation for \code{\link[mgcv]{jagam}}. Experiments suggest
#'these p-values tend to be more conservative than those that might be returned from an equivalent
#'model fit with \code{\link[mgcv]{summary.gam}} using `method = 'REML'`
#'
#'`coef.mvgam` returns either summaries or full posterior estimates for `GAM` component
#'coefficients
#'@return For `summary.mvgam` and `summary.mvgam_prefit`, a \code{list} is printed
#'on-screen showing the summaries for the model
#'
#'For `coef.mvgam`, either a \code{matrix} of posterior coefficient distributions
#'(if \code{summarise == FALSE} or \code{data.frame} of coefficient summaries)
#'@export
summary.mvgam = function(object, include_betas = TRUE, digits = 2, ...){

#### Standard summary of formula and model arguments ####
  if(!is.null(object$trend_call)){
    cat("GAM observation formula:\n")
    print(object$call)

    cat("\nGAM process formula:\n")
    print(object$trend_call)
  } else {
    cat("GAM formula:\n")
    print(object$call)
  }

cat("\nFamily:\n")
cat(paste0(object$family, '\n'))

cat("\nLink function:\n")
cat(paste0(family_links(object$family), '\n'))

cat("\nTrend model:\n")
cat(paste0(object$trend_model, '\n'))

if(object$use_lv){
  if(!is.null(object$trend_call)){
    cat("\nN process models:\n")
    cat(object$n_lv, '\n')
  } else {
    cat("\nN latent factors:\n")
    cat(object$n_lv, '\n')
  }

}

cat('\nN series:\n')
cat(NCOL(object$ytimes), '\n')

if(!is.null(object$upper_bounds)){
  cat('\nUpper bounds:\n')
  cat(object$upper_bounds, '\n')
}

cat('\nN timepoints:\n')
cat(max(object$obs_data$time), '\n')

if(object$fit_engine == 'jags'){
  cat('\nStatus:\n')
  cat('Fitted using JAGS', '\n')
}

if(object$fit_engine == 'stan'){
  cat('\nStatus:\n')
  cat('Fitted using Stan', '\n')
}

if(object$family == 'negative binomial'){
  cat("\nObservation dispersion parameter estimates:\n")
  print(mcmc_summary(object$model_output, 'phi', digits = digits,
                     variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])
}

if(object$family == 'beta'){
  cat("\nObservation precision parameter estimates:\n")
  print(mcmc_summary(object$model_output, 'phi', digits = digits,
                     variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])
}

if(object$family == 'tweedie'){
  cat("\nObservation dispersion parameter estimates:\n")
  print(mcmc_summary(object$model_output, 'phi', digits = digits,
                     variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])
}

if(object$family == 'gaussian'){
  cat("\nObservation error parameter estimates:\n")
  print(mcmc_summary(object$model_output, 'sigma_obs', digits = digits,
                     variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])
}

if(object$family == 'student'){
  cat("\nObservation error parameter estimates:\n")
  print(mcmc_summary(object$model_output, 'sigma_obs', digits = digits,
                     variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

  cat("\nObservation df parameter estimates:\n")
  print(mcmc_summary(object$model_output, 'nu', digits = digits,
                     variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])
}

if(object$family == 'lognormal'){
  cat("\nlog(observation error) parameter estimates:\n")
  print(mcmc_summary(object$model_output, 'sigma_obs', digits = digits,
                     variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])
}

if(object$family == 'Gamma'){
  cat("\nObservation shape parameter estimates:\n")
  print(mcmc_summary(object$model_output, 'shape', digits = digits,
                     variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])
}

  if(!is.null(object$trend_call)){
    if(include_betas){
      cat("\nGAM observation model coefficient (beta) estimates:\n")
      coef_names <- names(object$mgcv_model$coefficients)
      mvgam_coefs <- mcmc_summary(object$model_output, 'b',
                                  digits = digits,
                                  variational = object$algorithm %in% c('fullrank',
                                                                        'meanfield'))[,c(3:7)]
      rownames(mvgam_coefs) <- coef_names
      print(mvgam_coefs)

    } else {
      if(length(object$mgcv_model$nsdf) > 0){
        coefs_keep <- 1:object$mgcv_model$nsdf

        cat("\nGAM observation model coefficient (beta) estimates:\n")
        coef_names <- names(object$mgcv_model$coefficients)[coefs_keep]
        mvgam_coefs <- mcmc_summary(object$model_output, 'b',
                                    digits = digits,
                                    variational = object$algorithm %in% c('fullrank',
                                                                          'meanfield'))[coefs_keep,c(3:7)]
        rownames(mvgam_coefs) <- coef_names
        print(mvgam_coefs)
      }
    }

  } else {
    if(include_betas){
      cat("\nGAM coefficient (beta) estimates:\n")
      coef_names <- names(object$mgcv_model$coefficients)
      mvgam_coefs <- mcmc_summary(object$model_output, 'b',
                                  digits = digits,
                                  variational = object$algorithm %in% c('fullrank',
                                                                        'meanfield'))[,c(3:7)]
      rownames(mvgam_coefs) <- coef_names
      print(mvgam_coefs)
    } else {
      if(length(object$mgcv_model$nsdf) > 0){
        cat("\nGAM coefficient (beta) estimates:\n")
        coefs_keep <- 1:object$mgcv_model$nsdf
        coef_names <- names(object$mgcv_model$coefficients)[coefs_keep]
        mvgam_coefs <- mcmc_summary(object$model_output, 'b',
                                    digits = digits,
                                    variational = object$algorithm %in% c('fullrank',
                                                                          'meanfield'))[coefs_keep,c(3:7)]
        rownames(mvgam_coefs) <- coef_names
        print(mvgam_coefs)
      }
    }
  }

if(all(is.na(object$sp_names))){

} else {
  if(any(unlist(purrr::map(object$mgcv_model$smooth, inherits, 'random.effect')))){
    re_labs <- unlist(lapply(purrr::map(object$mgcv_model$smooth, 'term'),
                             paste, collapse = ','))[
                               unlist(purrr::map(object$mgcv_model$smooth, inherits, 'random.effect'))]
    re_labs <- gsub('series', 'trend', re_labs)
    re_sds <- mcmc_summary(object$model_output, 'sigma_raw',
                           ISB = TRUE, digits = digits,
                           variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)]

    re_mus <- mcmc_summary(object$model_output, 'mu_raw',
                           ISB = TRUE, digits = digits,
                           variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)]

    rownames(re_sds) <- paste0('sd(',re_labs,')')
    rownames(re_mus) <- paste0('mean(',re_labs,')')

    if(!is.null(object$trend_call)){
      cat("\nGAM observation model group-level estimates:\n")
    } else {
      cat("\nGAM group-level estimates:\n")
    }
    print(rbind(re_mus, re_sds))
  }
}

if(!is.null(attr(object$mgcv_model, 'gp_att_table'))){
  gp_names <- unlist(purrr::map(attr(object$mgcv_model, 'gp_att_table'), 'name'))
  alpha_params <- gsub(':', 'by', gsub(')', '_',
                       gsub('(', '_', paste0('alpha_', gp_names),
                            fixed = TRUE), fixed = TRUE))
  alpha_summary <- mcmc_summary(object$model_output, alpha_params,
                         ISB = TRUE, digits = digits,
                         variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)]

  rownames(alpha_summary) <- paste0('alpha_', gp_names)

  rho_params <- gsub(':', 'by', gsub(')', '_',
                                       gsub('(', '_', paste0('rho_', gp_names),
                                            fixed = TRUE), fixed = TRUE))
  rho_summary <- mcmc_summary(object$model_output, rho_params,
                                ISB = TRUE, digits = digits,
                                variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)]
  rownames(rho_summary) <- paste0('rho_', gp_names)

  if(!is.null(object$trend_call)){
    cat("\nGAM observation model gp term marginal deviation (alpha) and length scale (rho) estimates:\n")
  } else {
    cat("\nGAM gp term marginal deviation (alpha) and length scale (rho) estimates:\n")
  }
  print(rbind(alpha_summary, rho_summary))
}

if(any(!is.na(object$sp_names))){
  gam_sig_table <- summary(object$mgcv_model)$s.table[, c(1,3,4), drop = FALSE]
  if(!is.null(attr(object$mgcv_model, 'gp_att_table'))){
    gp_names <- unlist(purrr::map(attr(object$mgcv_model,
                                       'gp_att_table'), 'name'))
    if(all(rownames(gam_sig_table) %in% gsub('gp(', 's(', gp_names, fixed = TRUE))){

    } else {
      gam_sig_table <- gam_sig_table[!rownames(gam_sig_table) %in%
                                       gsub('gp(', 's(', gp_names, fixed = TRUE),,drop = FALSE]

      if(!is.null(object$trend_call)){
        cat("\nApproximate significance of GAM observation smooths:\n")
      } else {
        cat("\nApproximate significance of GAM observation smooths:\n")
      }
      suppressWarnings(printCoefmat(gam_sig_table,
                                    digits = min(3, digits + 1),
                                    signif.stars = getOption("show.signif.stars"),
                                    has.Pvalue = TRUE, na.print = "NA",
                                    cs.ind = 1))
      }
  } else {
    if(!is.null(object$trend_call)){
      cat("\nApproximate significance of GAM observation smooths:\n")
    } else {
      cat("\nApproximate significance of GAM observation smooths:\n")
    }
    suppressWarnings(printCoefmat(gam_sig_table,
                                  digits = min(3, digits + 1),
                                  signif.stars = getOption("show.signif.stars"),
                                  has.Pvalue = TRUE, na.print = "NA",
                                  cs.ind = 1))
  }
}

if(object$use_lv){
  if(object$trend_model != 'None'){
    if(object$trend_model == 'RW'){
      if(object$drift){
        if(!is.null(object$trend_call)){
          cat("\nProcess model drift estimates:\n")
        } else {
          cat("\nLatent trend drift estimates:\n")
        }
        print(mcmc_summary(object$model_output, c('drift'),
                           digits = digits,
                           variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])
      } else {
        if(!is.null(object$trend_call)){
          cat("\nProcess error parameter estimates:\n")
          print(mcmc_summary(object$model_output, c('sigma'),
                             digits = digits,
                             variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])
        }
      }
    }

    if(object$trend_model == 'GP'){
      cat("\nLatent trend length scale (rho) estimates:\n")
      print(mcmc_summary(object$model_output, c('rho_gp'),
                         digits = digits,
                         variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])
    }

    if(object$trend_model == 'AR1'){
      if(object$drift){
        if(!is.null(object$trend_call)){
          cat("\nProcess model drift and AR parameter estimates:\n")
          print(mcmc_summary(object$model_output, c('drift', 'ar1'),
                             digits = digits,
                             variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])
        } else {
          cat("\nLatent trend drift and AR parameter estimates:\n")
          print(mcmc_summary(object$model_output, c('drift', 'ar1'),
                             digits = digits,
                             variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])
        }

      } else {
        if(!is.null(object$trend_call)){
          cat("\nProcess model AR parameter estimates:\n")
          print(mcmc_summary(object$model_output, c('ar1'),
                             digits = digits,
                             variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])
        } else {
          cat("\nLatent trend AR parameter estimates:\n")
          print(mcmc_summary(object$model_output, c('ar1'),
                             digits = digits,
                             variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])
        }

      }

      if(!is.null(object$trend_call)){
        cat("\nProcess error parameter estimates:\n")
        print(mcmc_summary(object$model_output, c('sigma'),
                           digits = digits,
                           variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

      }
    }

    if(object$trend_model == 'AR2'){
      if(object$drift){
        if(!is.null(object$trend_call)){
          cat("\nProcess model drift and AR parameter estimates:\n")
          print(mcmc_summary(object$model_output, c('drift', 'ar1', 'ar2'),
                             digits = digits,
                             variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

        } else {
          cat("\nLatent trend drift and AR parameter estimates:\n")
          print(mcmc_summary(object$model_output, c('drift', 'ar1', 'ar2'),
                             digits = digits,
                             variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

        }

      } else {
        if(!is.null(object$trend_call)){
          cat("\nProcess model AR parameter estimates:\n")
          print(mcmc_summary(object$model_output, c('ar1', 'ar2'),
                             digits = digits,
                             variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

        } else {
          cat("\nLatent trend AR parameter estimates:\n")
          print(mcmc_summary(object$model_output, c('ar1', 'ar2'),
                             digits = digits,
                             variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

        }

      }

      if(!is.null(object$trend_call)){
        cat("\nProcess error parameter estimates:\n")
        print(mcmc_summary(object$model_output, c('sigma'),
                           digits = digits,
                           variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

      }
    }

    if(object$trend_model == 'AR3'){
      if(object$drift){
        if(!is.null(object$trend_call)){
          cat("\nProcess model drift and AR parameter estimates:\n")
          print(mcmc_summary(object$model_output, c('drift', 'ar1', 'ar2', 'ar3'),
                             digits = digits,
                             variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

        } else {
          cat("\nLatent trend drift and AR parameter estimates:\n")
          print(mcmc_summary(object$model_output, c('drift', 'ar1', 'ar2', 'ar3'),
                             digits = digits,
                             variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

        }

      } else {
        if(!is.null(object$trend_call)){
          cat("\nProcess model AR parameter estimates:\n")
          print(mcmc_summary(object$model_output, c('ar1', 'ar2', 'ar3'),
                             digits = digits,
                             variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

        } else {
          cat("\nLatent trend AR parameter estimates:\n")
          print(mcmc_summary(object$model_output, c('ar1', 'ar2', 'ar3'),
                             digits = digits,
                             variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

        }

      }

      if(!is.null(object$trend_call)){
        cat("\nProcess error parameter estimates:\n")
        print(mcmc_summary(object$model_output, c('sigma'),
                           digits = digits,
                           variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

      }
    }

    if(object$trend_model == 'VAR1'){
      if(object$drift){
        if(!is.null(object$trend_call)){
          cat("\nProcess model drift and VAR parameter estimates:\n")
          print(mcmc_summary(object$model_output, c('drift', 'A'),
                             digits = digits,
                             variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

        } else {
          cat("\nLatent trend drift and VAR parameter estimates:\n")
          print(mcmc_summary(object$model_output, c('drift', 'A'),
                             digits = digits,
                             variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

        }

      } else {
        if(!is.null(object$trend_call)){
          cat("\nProcess model VAR parameter estimates:\n")
          print(mcmc_summary(object$model_output, c('A'),
                             digits = digits,
                             variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

        } else {
          cat("\nLatent trend VAR parameter estimates:\n")
          print(mcmc_summary(object$model_output, c('A'),
                             digits = digits,
                             variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

        }

      }

      if(!is.null(object$trend_call)){
        cat("\nProcess error parameter estimates:\n")
        print(mcmc_summary(object$model_output, c('Sigma'),
                           digits = digits,
                           variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

      }
    }
  }
}

if(!object$use_lv){
  if(object$trend_model != 'None'){
    if(object$trend_model == 'RW'){
      if(object$drift){
        cat("\nLatent trend drift and sigma estimates:\n")
        print(mcmc_summary(object$model_output, c('drift', 'sigma'),
                           digits = digits,
                           variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

      } else {
        cat("\nLatent trend variance estimates:\n")
        print(mcmc_summary(object$model_output, c('sigma'),
                           digits = digits,
                           variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

      }
    }

    if(object$trend_model == 'VAR1'){
      if(object$drift){
        cat("\nLatent trend drift and VAR parameter estimates:\n")
        print(mcmc_summary(object$model_output, c('drift', 'A', 'Sigma'),
                           digits = digits,
                           variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

      } else {
        cat("\nLatent trend VAR parameter estimates:\n")
        print(mcmc_summary(object$model_output, c('A', 'Sigma'),
                           digits = digits,
                           variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

      }
    }

    if(object$trend_model == 'AR1'){
      if(object$drift){
        cat("\nLatent trend drift and AR parameter estimates:\n")
        print(mcmc_summary(object$model_output, c('drift', 'ar1', 'sigma'),
                           digits = digits,
                           variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

      } else {
        cat("\nLatent trend parameter AR estimates:\n")
        print(mcmc_summary(object$model_output, c('ar1', 'sigma'),
                           digits = digits,
                           variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

      }
    }

    if(object$trend_model == 'AR2'){
      if(object$drift){
        cat("\nLatent trend drift and AR parameter estimates:\n")
        print(mcmc_summary(object$model_output, c('drift', 'ar1', 'ar2', 'sigma'),
                           digits = digits,
                           variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

      } else {
        cat("\nLatent trend AR parameter estimates:\n")
        print(mcmc_summary(object$model_output, c('ar1', 'ar2', 'sigma'),
                           digits = digits,
                           variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

      }
    }

    if(object$trend_model == 'AR3'){
      if(object$drift){
        cat("\nLatent trend drift and AR parameter estimates:\n")
        print(mcmc_summary(object$model_output, c('drift', 'ar1',
                                                  'ar2', 'ar3', 'sigma'),
                           digits = digits,
                           variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

      } else {
        cat("\nLatent trend AR parameter estimates:\n")
        print(mcmc_summary(object$model_output, c('ar1', 'ar2',
                                                  'ar3', 'sigma'),
                           digits = digits,
                           variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])

      }
    }

    if(object$trend_model == 'GP'){
        cat("\nLatent trend marginal deviation (alpha) and length scale (rho) estimates:\n")
        print(mcmc_summary(object$model_output, c('alpha_gp', 'rho_gp'),
                           digits = digits,
                           variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)])


    }
  }
}

if(!is.null(object$trend_call)){
  if(include_betas){
    cat("\nGAM process model coefficient (beta) estimates:\n")
    coef_names <- paste0(names(object$trend_mgcv_model$coefficients), '_trend')
    mvgam_coefs <- mcmc_summary(object$model_output, 'b_trend',
                                digits = digits,
                                variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)]
    rownames(mvgam_coefs) <- gsub('series', 'trend',
                                  coef_names, fixed = TRUE)
    print(mvgam_coefs)
  } else {
    coefs_include <- 1:object$trend_mgcv_model$nsdf
    cat("\nGAM process model coefficient (beta) estimates:\n")
    coef_names <- paste0(names(object$trend_mgcv_model$coefficients), '_trend')[coefs_include]
    mvgam_coefs <- mcmc_summary(object$model_output, 'b_trend',
                                digits = digits,
                                variational = object$algorithm %in% c('fullrank', 'meanfield'))[coefs_include,c(3:7)]
    rownames(mvgam_coefs) <- gsub('series', 'trend',
                                  coef_names, fixed = TRUE)
    print(mvgam_coefs)
  }

  if(all(is.na(object$trend_sp_names))){

  } else {
    if(any(unlist(purrr::map(object$trend_mgcv_model$smooth, inherits, 'random.effect')))){
      re_labs <- unlist(lapply(purrr::map(object$trend_mgcv_model$smooth, 'term'),
                               paste, collapse = ','))[
                                 unlist(purrr::map(object$trend_mgcv_model$smooth, inherits, 'random.effect'))]
      re_labs <- gsub('series', 'trend', re_labs)
      re_sds <- mcmc_summary(object$model_output, 'sigma_raw_trend',
                             ISB = TRUE, digits = digits,
                             variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)]

      re_mus <- mcmc_summary(object$model_output, 'mu_raw_trend',
                             ISB = TRUE, digits = digits,
                             variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)]

      rownames(re_sds) <- paste0('sd(',re_labs,')_trend')
      rownames(re_mus) <- paste0('mean(',re_labs,')_trend')

      cat("\nGAM process model group-level estimates:\n")
      print(rbind(re_mus, re_sds))
    }
  }

  if(!is.null(attr(object$trend_mgcv_model, 'gp_att_table'))){
    gp_names <- clean_gpnames(unlist(purrr::map(attr(object$trend_mgcv_model,
                                                     'gp_att_table'), 'name')))
    alpha_params <- gsub('gp_', 'gp_trend_', gsub(':', 'by', gsub(')', '_',
                                         gsub('(', '_', paste0('alpha_', gp_names),
                                              fixed = TRUE), fixed = TRUE)))
    alpha_summary <- mcmc_summary(object$model_output, alpha_params,
                                  ISB = TRUE, digits = digits,
                                  variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)]

    rownames(alpha_summary) <- paste0(gsub('series', 'trend', paste0('alpha_', gp_names)),
                                      '_trend')

    rho_params <- gsub('gp_', 'gp_trend_', gsub(':', 'by', gsub(')', '_',
                                       gsub('(', '_', paste0('rho_', gp_names),
                                            fixed = TRUE), fixed = TRUE)))
    rho_summary <- mcmc_summary(object$model_output, rho_params,
                                ISB = TRUE, digits = digits,
                                variational = object$algorithm %in% c('fullrank', 'meanfield'))[,c(3:7)]
    rownames(rho_summary) <- paste0(gsub('series', 'trend', paste0('rho_', gp_names)),
                                    '_trend')

    cat("\nGAM process model gp term marginal deviation (alpha) and length scale (rho) estimates:\n")
    print(rbind(alpha_summary, rho_summary))
  }

  if(any(!is.na(object$trend_sp_names))){
    gam_sig_table <- summary(object$trend_mgcv_model)$s.table[, c(1,3,4), drop = FALSE]
    if(!is.null(attr(object$trend_mgcv_model, 'gp_att_table'))){
      gp_names <- unlist(purrr::map(attr(object$trend_mgcv_model, 'gp_att_table'), 'name'))
      if(all(rownames(gam_sig_table) %in% gsub('gp(', 's(', gp_names, fixed = TRUE))){

      } else {
        gam_sig_table <- gam_sig_table[!rownames(gam_sig_table) %in%
                                         gsub('gp(', 's(', gp_names, fixed = TRUE),drop = FALSE]

        cat("\nApproximate significance of GAM process smooths:\n")
        suppressWarnings(printCoefmat(gam_sig_table,
                                      digits = min(3, digits + 1),
                                      signif.stars = getOption("show.signif.stars"),
                                      has.Pvalue = TRUE, na.print = "NA",
                                      cs.ind = 1))
      }
    } else {
      cat("\nApproximate significance of GAM process smooths:\n")
      suppressWarnings(printCoefmat(gam_sig_table,
                                    digits = min(3, digits + 1),
                                    signif.stars = getOption("show.signif.stars"),
                                    has.Pvalue = TRUE, na.print = "NA",
                                    cs.ind = 1))
    }
  }
}

if(object$fit_engine == 'stan' & object$algorithm == 'sampling'){
  cat('\nStan MCMC diagnostics:\n')
  check_all_diagnostics(object$model_output,
                        max_treedepth = object$max_treedepth)

}

if(object$algorithm != 'sampling'){
  cat('\nVariational Bayes used: no diagnostics to compute\n')
}

if(object$fit_engine == 'jags'){
  cat('\nJAGS MCMC diagnostics:\n')
  rhats <- mcmc_summary(object$model_output, digits = digits,
                        variational = object$algorithm %in% c('fullrank', 'meanfield'))[,6]
  if(any(rhats > 1.05)){
    cat('\nRhats above 1.05 found for',
        length(which(rhats > 1.05)),
        'parameters\n*Diagnose further to investigate why the chains have not mixed\n')
  } else {
    cat('\nRhat looks reasonable for all parameters\n')
  }
}

}

#' @rdname summary.mvgam
#' @export
summary.mvgam_prefit = function(object, ...){

  if(!is.null(object$trend_call)){
    cat("\nGAM observation formula:\n")
    print(object$call)


    cat("\nGAM process formula:\n")
    print(object$trend_call)

  } else {
    cat("\nGAM formula:\n")
    print(object$call)

  }

  cat("\nFamily:\n")
  cat(paste0(object$family, '\n'))


  cat("\nLink function:\n")
  cat(paste0(family_links(object$family), '\n'))


  cat("\nTrend model:\n")
  cat(paste0(object$trend_model, '\n'))


  if(object$use_lv){
    if(!is.null(object$trend_call)){
      cat("\nN process models:\n")
      cat(object$n_lv, '\n')

    } else {
      cat("\nN latent factors:\n")
      cat(object$n_lv, '\n')

    }

  }

  cat('\nN series:')
  cat(NCOL(object$ytimes), '\n')


  cat('\nN timepoints:')
  cat(max(object$obs_data$time), '\n')


  cat('\nStatus:')
  cat('Not fitted', '\n')

}

#' @rdname summary.mvgam
#' @export
#'@title Extract mvgam beta coefficients from the GAM component
#'@param object \code{list} object returned from \code{mvgam}
#'@param summarise \code{logical}. Summaries of coefficients will be returned
#'if \code{TRUE}. Otherwise the full posterior distribution will be returned
#'
#'@method coef mvgam
#'@export
coef.mvgam = function(object, summarise = TRUE, ...){
  coef_names <- names(object$mgcv_model$coefficients)

  if(summarise){
    mvgam_coefs <- mcmc_summary(object$model_output, 'b')[,c(3:7)]
    rownames(mvgam_coefs) <- coef_names
  } else {
    mvgam_coefs <- mcmc_chains(object$model_output, 'b')
    colnames(mvgam_coefs) <- coef_names
  }

  return(mvgam_coefs)
}
