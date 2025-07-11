#'Summary for a fitted \pkg{mvgam} models
#'
#'These functions take a fitted \code{mvgam} or \code{jsdgam} object and
#'return various useful summaries
#'@importFrom stats printCoefmat
#'@param object \code{list} object of class `mvgam`
#'@param include_betas Logical. Print a summary that includes posterior summaries
#'of all linear predictor beta coefficients (including spline coefficients)?
#'Defaults to \code{TRUE} but use \code{FALSE} for a more concise summary
#'@param smooth_test Logical. Compute estimated degrees of freedom and approximate
#'p-values for smooth terms? Defaults to \code{TRUE}, but users may wish to set
#'to \code{FALSE} for complex models with many smooth or random effect terms
#'@param digits The number of significant digits for printing out the summary;
#'  defaults to \code{2}.
#'@param ... Ignored
#'@author Nicholas J Clark
#'@details `summary.mvgam` and `summary.mvgam_prefit` return brief summaries of the model's call, along with posterior intervals for
#'some of the key parameters in the model. Note that some smooths have extra penalties on the null space,
#'so summaries for the \code{rho} parameters may include more penalty terms than the number of smooths in
#'the original model formula. Approximate p-values for smooth terms are also returned,
#'with methods used for their
#'calculation following those used for `mgcv` equivalents (see \code{\link[mgcv]{summary.gam}} for details).
#'The Estimated Degrees of Freedom (edf) for smooth terms is computed using
#'either `edf.type = 1` for models with no trend component, or `edf.type = 0` for models with
#'trend components. These are described in the documentation for \code{\link[mgcv]{jagam}}. Experiments suggest
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
summary.mvgam = function(
  object,
  include_betas = TRUE,
  smooth_test = TRUE,
  digits = 2,
  ...
) {
  #### Some adjustments for cleaner summaries ####
  if (
    attr(object$model_data, 'trend_model') == 'None' &
      object$use_lv &
      object$family != 'nmix'
  ) {
    attr(object$model_data, 'trend_model') <- 'RW'
  }
  variational <- object$algorithm %in%
    c('fullrank', 'meanfield', 'laplace', 'pathfinder')

  #### Smooth tests ####
  if (smooth_test) {
    if (inherits(object$trend_model, 'mvgam_trend')) {
      trend_model <- object$trend_model$label
    } else {
      trend_model <- object$trend_model
    }
    object$mgcv_model <- compute_edf(
      object$mgcv_model,
      object,
      'rho',
      'sigma_raw',
      conservative = trend_model == 'None'
    )

    if (!is.null(object$trend_call) & !inherits(object, 'jsdgam')) {
      object$trend_mgcv_model <- compute_edf(
        object$trend_mgcv_model,
        object,
        'rho_trend',
        'sigma_raw_trend'
      )
    }
  }

  #### Standard summary of formula and model arguments ####
  if (!is.null(object$trend_call)) {
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

  if (!inherits(object, 'jsdgam')) {
    cat("\nTrend model:\n")
    if (inherits(object$trend_model, 'mvgam_trend')) {
      print(object$trend_model$label)
      cat('\n')
    } else {
      cat(paste0(object$trend_model, '\n'))
    }
  }

  if (object$use_lv) {
    if (!is.null(object$trend_call)) {
      cat("\nN process models:\n")
      cat(object$n_lv, '\n')
    } else {
      cat("\nN latent factors:\n")
      cat(object$n_lv, '\n')
    }
  }

  if (inherits(object, 'jsdgam')) {
    cat('\nN species:\n')
    cat(NCOL(object$ytimes), '\n')
  } else {
    cat('\nN series:\n')
    cat(NCOL(object$ytimes), '\n')
  }

  if (!is.null(object$upper_bounds)) {
    cat('\nUpper bounds:\n')
    cat(object$upper_bounds, '\n')
  }

  if (inherits(object, 'jsdgam')) {
    cat('\nN sites:\n')
    cat(NROW(object$ytimes), '\n')
  } else {
    cat('\nN timepoints:\n')
    cat(NROW(object$ytimes), '\n')
  }

  if (object$fit_engine == 'jags') {
    cat('\nStatus:\n')
    cat('Fitted using JAGS', '\n')
  }

  if (object$fit_engine == 'stan') {
    cat('\nStatus:\n')
    cat('Fitted using Stan', '\n')

    n_kept <- object$model_output@sim$n_save - object$model_output@sim$warmup2
    cat(
      object$model_output@sim$chains,
      " chains, each with iter = ",
      object$model_output@sim$iter,
      "; warmup = ",
      object$model_output@sim$warmup,
      "; thin = ",
      object$model_output@sim$thin,
      " \n",
      "Total post-warmup draws = ",
      sum(n_kept),
      "\n\n",
      sep = ''
    )
  }

  if (object$family == 'negative binomial') {
    cat("\nObservation dispersion parameter estimates:\n")
    print(mcmc_summary(
      object$model_output,
      'phi',
      digits = digits,
      variational = variational
    )[, c(3:7)])
  }

  if (object$family == 'beta_binomial') {
    cat("\nObservation dispersion parameter estimates:\n")
    print(mcmc_summary(
      object$model_output,
      'phi',
      digits = digits,
      variational = variational
    )[, c(3:7)])
  }

  if (object$family == 'beta') {
    cat("\nObservation precision parameter estimates:\n")
    print(mcmc_summary(
      object$model_output,
      'phi',
      digits = digits,
      variational = variational
    )[, c(3:7)])
  }

  if (object$family == 'tweedie') {
    cat("\nObservation dispersion parameter estimates:\n")
    print(mcmc_summary(
      object$model_output,
      'phi',
      digits = digits,
      variational = variational
    )[, c(3:7)])
  }

  if (object$family == 'gaussian') {
    cat("\nObservation error parameter estimates:\n")
    print(mcmc_summary(
      object$model_output,
      'sigma_obs',
      digits = digits,
      variational = variational
    )[, c(3:7)])
  }

  if (object$family == 'student') {
    cat("\nObservation error parameter estimates:\n")
    print(mcmc_summary(
      object$model_output,
      'sigma_obs',
      digits = digits,
      variational = variational
    )[, c(3:7)])

    cat("\nObservation df parameter estimates:\n")
    print(mcmc_summary(
      object$model_output,
      'nu',
      digits = digits,
      variational = variational
    )[, c(3:7)])
  }

  if (object$family == 'lognormal') {
    cat("\nlog(observation error) parameter estimates:\n")
    print(mcmc_summary(
      object$model_output,
      'sigma_obs',
      digits = digits,
      variational = variational
    )[, c(3:7)])
  }

  if (object$family == 'Gamma') {
    cat("\nObservation shape parameter estimates:\n")
    print(mcmc_summary(
      object$model_output,
      'shape',
      digits = digits,
      variational = variational
    )[, c(3:7)])
  }

  if (!is.null(object$trend_call)) {
    if (include_betas) {
      cat("\nGAM observation model coefficient (beta) estimates:\n")
      coef_names <- names(object$mgcv_model$coefficients)
      mvgam_coefs <- mcmc_summary(
        object$model_output,
        'b',
        digits = digits,
        variational = variational
      )[, c(3:7)]
      rownames(mvgam_coefs) <- coef_names
      print(mvgam_coefs)
    } else {
      if (object$mgcv_model$nsdf > 0) {
        coefs_keep <- 1:object$mgcv_model$nsdf

        cat("\nGAM observation model coefficient (beta) estimates:\n")
        coef_names <- names(object$mgcv_model$coefficients)[coefs_keep]
        mvgam_coefs <- mcmc_summary(
          object$model_output,
          'b',
          digits = digits,
          variational = variational
        )[coefs_keep, c(3:7)]
        rownames(mvgam_coefs) <- coef_names
        print(mvgam_coefs)
      }
    }
  } else {
    if (include_betas) {
      cat("\nGAM coefficient (beta) estimates:\n")
      coef_names <- names(object$mgcv_model$coefficients)
      mvgam_coefs <- mcmc_summary(
        object$model_output,
        'b',
        digits = digits,
        variational = variational
      )[, c(3:7)]
      rownames(mvgam_coefs) <- coef_names
      print(mvgam_coefs)
    } else {
      if (object$mgcv_model$nsdf > 0) {
        cat("\nGAM coefficient (beta) estimates:\n")
        coefs_keep <- 1:object$mgcv_model$nsdf
        coef_names <- names(object$mgcv_model$coefficients)[coefs_keep]
        mvgam_coefs <- mcmc_summary(
          object$model_output,
          'b',
          digits = digits,
          variational = variational
        )[coefs_keep, c(3:7)]
        rownames(mvgam_coefs) <- coef_names
        print(mvgam_coefs)
      }
    }
  }

  if (all(is.na(object$sp_names))) {} else {
    if (
      any(unlist(purrr::map(
        object$mgcv_model$smooth,
        inherits,
        'random.effect'
      )))
    ) {
      re_labs <- unlist(lapply(
        purrr::map(object$mgcv_model$smooth, 'label'),
        paste,
        collapse = ','
      ))[
        unlist(purrr::map(object$mgcv_model$smooth, inherits, 'random.effect'))
      ]
      re_sds <- mcmc_summary(
        object$model_output,
        'sigma_raw',
        ISB = TRUE,
        digits = digits,
        variational = variational
      )[, c(3:7)]

      re_mus <- mcmc_summary(
        object$model_output,
        'mu_raw',
        ISB = TRUE,
        digits = digits,
        variational = variational
      )[, c(3:7)]

      rownames(re_sds) <- paste0('sd(', re_labs, ')')
      rownames(re_mus) <- paste0('mean(', re_labs, ')')

      if (!is.null(object$trend_call)) {
        cat("\nGAM observation model group-level estimates:\n")
      } else {
        cat("\nGAM group-level estimates:\n")
      }
      print(rbind(re_mus, re_sds))
    }
  }

  if (!is.null(attr(object$mgcv_model, 'gp_att_table'))) {
    gp_summaries <- gp_param_summary(
      object = object,
      mgcv_model = object$mgcv_model,
      digits = digits,
      variational = variational
    )

    if (!is.null(object$trend_call)) {
      cat(
        "\nGAM observation model gp term marginal deviation (alpha) and length scale (rho) estimates:\n"
      )
    } else {
      cat(
        "\nGAM gp term marginal deviation (alpha) and length scale (rho) estimates:\n"
      )
    }
    print(rbind(gp_summaries$alpha_summary, gp_summaries$rho_summary))
  }

  if (any(!is.na(object$sp_names)) & smooth_test) {
    gam_sig_table <- try(
      suppressWarnings(summary(object$mgcv_model)$s.table[,
        c(1, 2, 3, 4),
        drop = FALSE
      ]),
      silent = TRUE
    )
    if (inherits(gam_sig_table, 'try-error')) {
      object$mgcv_model$R <- NULL
      gam_sig_table <- suppressWarnings(summary(object$mgcv_model)$s.table[,
        c(1, 2, 3, 4),
        drop = FALSE
      ])
      gam_sig_table[, 2] <- unlist(
        purrr::map(object$mgcv_model$smooth, 'df'),
        use.names = FALSE
      )
    }
    if (!is.null(attr(object$mgcv_model, 'gp_att_table'))) {
      gp_names <- unlist(purrr::map(
        attr(object$mgcv_model, 'gp_att_table'),
        'name'
      ))
      if (
        all(
          rownames(gam_sig_table) %in% gsub('gp(', 's(', gp_names, fixed = TRUE)
        )
      ) {} else {
        gam_sig_table <- gam_sig_table[
          !rownames(gam_sig_table) %in%
            gsub('gp(', 's(', gp_names, fixed = TRUE),
          ,
          drop = FALSE
        ]

        if (!is.null(object$trend_call)) {
          cat("\nApproximate significance of GAM observation smooths:\n")
        } else {
          cat("\nApproximate significance of GAM smooths:\n")
        }
        suppressWarnings(printCoefmat(
          gam_sig_table,
          digits = min(3, digits + 1),
          signif.stars = getOption("show.signif.stars"),
          has.Pvalue = TRUE,
          na.print = "NA",
          cs.ind = 1
        ))
      }
    } else {
      if (!is.null(object$trend_call)) {
        cat("\nApproximate significance of GAM observation smooths:\n")
      } else {
        cat("\nApproximate significance of GAM smooths:\n")
      }
      suppressWarnings(printCoefmat(
        gam_sig_table,
        digits = min(3, digits + 1),
        signif.stars = getOption("show.signif.stars"),
        has.Pvalue = TRUE,
        na.print = "NA",
        cs.ind = 1
      ))
    }
  }

  if (object$use_lv) {
    if (attr(object$model_data, 'trend_model') != 'None') {
      if (attr(object$model_data, 'trend_model') == 'RW') {
        if (object$drift) {
          if (!is.null(object$trend_call)) {
            cat("\nProcess model drift estimates:\n")
          } else {
            cat("\nLatent trend drift estimates:\n")
          }
          print(suppressWarnings(mcmc_summary(
            object$model_output,
            c('drift', 'theta'),
            digits = digits,
            variational = variational
          ))[, c(3:7)])
        } else {
          if (!is.null(object$trend_call)) {
            if (inherits(object$trend_model, 'mvgam_trend')) {
              trend_model <- object$trend_model$trend_model
            } else {
              trend_model <- object$trend_model
            }
            if (
              trend_model == 'None' &
                object$family == 'nmix' |
                inherits(object, 'jsdgam')
            ) {} else {
              cat("\nProcess error parameter estimates:\n")
              print(suppressWarnings(mcmc_summary(
                object$model_output,
                c('sigma', 'theta'),
                digits = digits,
                variational = variational
              ))[, c(3:7)])
            }
          }
        }
      }

      if (attr(object$model_data, 'trend_model') == 'GP') {
        cat("\nLatent trend length scale (rho) estimates:\n")
        print(mcmc_summary(
          object$model_output,
          c('rho_gp'),
          digits = digits,
          variational = variational
        )[, c(3:7)])
      }

      if (attr(object$model_data, 'trend_model') %in% c('AR1', 'CAR1')) {
        if (object$drift) {
          if (!is.null(object$trend_call)) {
            cat("\nProcess model drift and AR parameter estimates:\n")
            print(mcmc_summary(
              object$model_output,
              c('drift', 'ar1'),
              digits = digits,
              variational = variational
            )[, c(3:7)])
          } else {
            cat("\nLatent trend drift and AR parameter estimates:\n")
            print(mcmc_summary(
              object$model_output,
              c('drift', 'ar1'),
              digits = digits,
              variational = variational
            )[, c(3:7)])
          }
        } else {
          if (!is.null(object$trend_call)) {
            cat("\nProcess model AR parameter estimates:\n")
            print(mcmc_summary(
              object$model_output,
              c('ar1'),
              digits = digits,
              variational = variational
            )[, c(3:7)])
          } else {
            cat("\nLatent trend AR parameter estimates:\n")
            print(mcmc_summary(
              object$model_output,
              c('ar1'),
              digits = digits,
              variational = variational
            )[, c(3:7)])
          }
        }

        if (!is.null(object$trend_call)) {
          cat("\nProcess error parameter estimates:\n")
          print(suppressWarnings(mcmc_summary(
            object$model_output,
            c('sigma', 'theta'),
            digits = digits,
            variational = variational
          ))[, c(3:7)])
        }
      }

      if (attr(object$model_data, 'trend_model') == 'AR2') {
        if (object$drift) {
          if (!is.null(object$trend_call)) {
            cat("\nProcess model drift and AR parameter estimates:\n")
            print(mcmc_summary(
              object$model_output,
              c('drift', 'ar1', 'ar2'),
              digits = digits,
              variational = variational
            )[, c(3:7)])
          } else {
            cat("\nLatent trend drift and AR parameter estimates:\n")
            print(mcmc_summary(
              object$model_output,
              c('drift', 'ar1', 'ar2'),
              digits = digits,
              variational = variational
            )[, c(3:7)])
          }
        } else {
          if (!is.null(object$trend_call)) {
            cat("\nProcess model AR parameter estimates:\n")
            print(mcmc_summary(
              object$model_output,
              c('ar1', 'ar2'),
              digits = digits,
              variational = variational
            )[, c(3:7)])
          } else {
            cat("\nLatent trend AR parameter estimates:\n")
            print(mcmc_summary(
              object$model_output,
              c('ar1', 'ar2'),
              digits = digits,
              variational = variational
            )[, c(3:7)])
          }
        }

        if (!is.null(object$trend_call)) {
          cat("\nProcess error parameter estimates:\n")
          print(suppressWarnings(mcmc_summary(
            object$model_output,
            c('sigma', 'theta'),
            digits = digits,
            variational = variational
          ))[, c(3:7)])
        }
      }

      if (attr(object$model_data, 'trend_model') == 'AR3') {
        if (object$drift) {
          if (!is.null(object$trend_call)) {
            cat("\nProcess model drift and AR parameter estimates:\n")
            print(mcmc_summary(
              object$model_output,
              c('drift', 'ar1', 'ar2', 'ar3'),
              digits = digits,
              variational = variational
            )[, c(3:7)])
          } else {
            cat("\nLatent trend drift and AR parameter estimates:\n")
            print(mcmc_summary(
              object$model_output,
              c('drift', 'ar1', 'ar2', 'ar3'),
              digits = digits,
              variational = variational
            )[, c(3:7)])
          }
        } else {
          if (!is.null(object$trend_call)) {
            cat("\nProcess model AR parameter estimates:\n")
            print(mcmc_summary(
              object$model_output,
              c('ar1', 'ar2', 'ar3'),
              digits = digits,
              variational = variational
            )[, c(3:7)])
          } else {
            cat("\nLatent trend AR parameter estimates:\n")
            print(mcmc_summary(
              object$model_output,
              c('ar1', 'ar2', 'ar3'),
              digits = digits,
              variational = variational
            )[, c(3:7)])
          }
        }

        if (!is.null(object$trend_call)) {
          cat("\nProcess error parameter estimates:\n")
          print(suppressWarnings(mcmc_summary(
            object$model_output,
            c('sigma', 'theta'),
            digits = digits,
            variational = variational
          ))[, c(3:7)])
        }
      }

      if (attr(object$model_data, 'trend_model') == 'VAR1') {
        if (object$drift) {
          if (!is.null(object$trend_call)) {
            cat("\nProcess model drift and VAR parameter estimates:\n")
            print(mcmc_summary(
              object$model_output,
              c('drift', 'A'),
              digits = digits,
              variational = variational
            )[, c(3:7)])
          } else {
            cat("\nLatent trend drift and VAR parameter estimates:\n")
            print(mcmc_summary(
              object$model_output,
              c('drift', 'A'),
              digits = digits,
              variational = variational
            )[, c(3:7)])
          }
        } else {
          if (!is.null(object$trend_call)) {
            cat("\nProcess model VAR parameter estimates:\n")
            print(mcmc_summary(
              object$model_output,
              c('A'),
              digits = digits,
              variational = variational
            )[, c(3:7)])
          } else {
            cat("\nLatent trend VAR parameter estimates:\n")
            print(mcmc_summary(
              object$model_output,
              c('A'),
              digits = digits,
              variational = variational
            )[, c(3:7)])
          }
        }

        if (!is.null(object$trend_call)) {
          cat("\nProcess error parameter estimates:\n")
          print(suppressWarnings(mcmc_summary(
            object$model_output,
            c('Sigma', 'theta'),
            digits = digits,
            variational = variational
          ))[, c(3:7)])
        }
      }
    }
  }

  if (!object$use_lv) {
    if (attr(object$model_data, 'trend_model') != 'None') {
      if (
        attr(object$model_data, 'trend_model') %in% c('PWlinear', 'PWlogistic')
      ) {
        cat("\nLatent trend growth rate estimates:\n")
        print(suppressWarnings(mcmc_summary(
          object$model_output,
          c('k_trend'),
          digits = digits,
          variational = variational
        ))[, c(3:7)])

        cat("\nLatent trend offset estimates:\n")
        print(suppressWarnings(mcmc_summary(
          object$model_output,
          c('m_trend'),
          digits = digits,
          variational = variational
        ))[, c(3:7)])
      }

      if (attr(object$model_data, 'trend_model') == 'RW') {
        if (object$drift) {
          cat("\nLatent trend drift and sigma estimates:\n")
          print(suppressWarnings(mcmc_summary(
            object$model_output,
            c('drift', 'sigma', 'theta'),
            digits = digits,
            variational = variational
          ))[, c(3:7)])
        } else {
          cat("\nLatent trend variance estimates:\n")
          print(suppressWarnings(mcmc_summary(
            object$model_output,
            c('sigma', 'theta'),
            digits = digits,
            variational = variational
          ))[, c(3:7)])
        }
      }

      if (attr(object$model_data, 'trend_model') == 'VAR1') {
        if (object$drift) {
          cat("\nLatent trend drift and VAR parameter estimates:\n")
          print(suppressWarnings(mcmc_summary(
            object$model_output,
            c('drift', 'A', 'Sigma', 'theta'),
            digits = digits,
            variational = variational
          ))[, c(3:7)])
        } else {
          cat("\nLatent trend VAR parameter estimates:\n")
          print(suppressWarnings(mcmc_summary(
            object$model_output,
            c('A', 'Sigma', 'theta'),
            digits = digits,
            variational = variational
          ))[, c(3:7)])
        }
      }

      if (attr(object$model_data, 'trend_model') %in% c('AR1', 'CAR1')) {
        if (object$drift) {
          cat("\nLatent trend drift and AR parameter estimates:\n")
          print(suppressWarnings(mcmc_summary(
            object$model_output,
            c('drift', 'ar1', 'sigma', 'theta'),
            digits = digits,
            variational = variational
          ))[, c(3:7)])
        } else {
          cat("\nLatent trend parameter AR estimates:\n")
          print(suppressWarnings(mcmc_summary(
            object$model_output,
            c('ar1', 'sigma', 'theta'),
            digits = digits,
            variational = variational
          ))[, c(3:7)])
        }
      }

      if (attr(object$model_data, 'trend_model') == 'AR2') {
        if (object$drift) {
          cat("\nLatent trend drift and AR parameter estimates:\n")
          print(suppressWarnings(mcmc_summary(
            object$model_output,
            c('drift', 'ar1', 'ar2', 'sigma', 'theta'),
            digits = digits,
            variational = variational
          ))[, c(3:7)])
        } else {
          cat("\nLatent trend AR parameter estimates:\n")
          print(suppressWarnings(mcmc_summary(
            object$model_output,
            c('ar1', 'ar2', 'sigma', 'theta'),
            digits = digits,
            variational = variational
          ))[, c(3:7)])
        }
      }

      if (attr(object$model_data, 'trend_model') == 'AR3') {
        if (object$drift) {
          cat("\nLatent trend drift and AR parameter estimates:\n")
          print(suppressWarnings(mcmc_summary(
            object$model_output,
            c('drift', 'ar1', 'ar2', 'ar3', 'sigma', 'theta'),
            digits = digits,
            variational = variational
          ))[, c(3:7)])
        } else {
          cat("\nLatent trend AR parameter estimates:\n")
          print(suppressWarnings(mcmc_summary(
            object$model_output,
            c('ar1', 'ar2', 'ar3', 'sigma', 'theta'),
            digits = digits,
            variational = variational
          ))[, c(3:7)])
        }
      }

      if (attr(object$model_data, 'trend_model') == 'GP') {
        cat(
          "\nLatent trend marginal deviation (alpha) and length scale (rho) estimates:\n"
        )
        print(suppressWarnings(mcmc_summary(
          object$model_output,
          c('alpha_gp', 'rho_gp'),
          digits = digits,
          variational = variational
        ))[, c(3:7)])
      }
    }
  }

  if (grepl('hiercor', validate_trend_model(object$trend_model))) {
    cat(
      "\nHierarchical correlation weighting parameter (alpha_cor) estimates:\n"
    )
    print(suppressWarnings(mcmc_summary(
      object$model_output,
      'alpha_cor',
      digits = digits,
      variational = variational
    ))[, c(3:7)])
  }

  if (!is.null(object$trend_call) & !inherits(object, 'jsdgam')) {
    if (include_betas) {
      cat("\nGAM process model coefficient (beta) estimates:\n")
      coef_names <- paste0(
        names(object$trend_mgcv_model$coefficients),
        '_trend'
      )
      mvgam_coefs <- mcmc_summary(
        object$model_output,
        'b_trend',
        digits = digits,
        variational = variational
      )[, c(3:7)]
      rownames(mvgam_coefs) <- gsub('series', 'trend', coef_names, fixed = TRUE)
      print(mvgam_coefs)
    } else {
      if (object$trend_mgcv_model$nsdf > 0) {
        coefs_include <- 1:object$trend_mgcv_model$nsdf
        cat("\nGAM process model coefficient (beta) estimates:\n")
        coef_names <- paste0(
          names(object$trend_mgcv_model$coefficients),
          '_trend'
        )[coefs_include]
        mvgam_coefs <- mcmc_summary(
          object$model_output,
          'b_trend',
          digits = digits,
          variational = variational
        )[coefs_include, c(3:7)]
        rownames(mvgam_coefs) <- gsub(
          'series',
          'trend',
          coef_names,
          fixed = TRUE
        )
        print(mvgam_coefs)
      }
    }

    if (all(is.na(object$trend_sp_names))) {} else {
      if (
        any(unlist(purrr::map(
          object$trend_mgcv_model$smooth,
          inherits,
          'random.effect'
        )))
      ) {
        re_labs <- unlist(lapply(
          purrr::map(object$trend_mgcv_model$smooth, 'label'),
          paste,
          collapse = ','
        ))[
          unlist(purrr::map(
            object$trend_mgcv_model$smooth,
            inherits,
            'random.effect'
          ))
        ]
        re_labs <- gsub('series', 'trend', re_labs)
        re_sds <- mcmc_summary(
          object$model_output,
          'sigma_raw_trend',
          ISB = TRUE,
          digits = digits,
          variational = variational
        )[, c(3:7)]

        re_mus <- mcmc_summary(
          object$model_output,
          'mu_raw_trend',
          ISB = TRUE,
          digits = digits,
          variational = variational
        )[, c(3:7)]

        rownames(re_sds) <- paste0('sd(', re_labs, ')_trend')
        rownames(re_mus) <- paste0('mean(', re_labs, ')_trend')

        cat("\nGAM process model group-level estimates:\n")
        print(rbind(re_mus, re_sds))
      }
    }

    if (!is.null(attr(object$trend_mgcv_model, 'gp_att_table'))) {
      gp_summaries <- gp_param_summary(
        object = object,
        mgcv_model = object$trend_mgcv_model,
        trend_effects = TRUE,
        digits = digits,
        variational = variational
      )

      cat(
        "\nGAM process model gp term marginal deviation (alpha) and length scale (rho) estimates:\n"
      )
      print(rbind(gp_summaries$alpha_summary, gp_summaries$rho_summary))
    }

    if (any(!is.na(object$trend_sp_names)) & smooth_test) {
      gam_sig_table <- try(
        suppressWarnings(summary(object$trend_mgcv_model)$s.table[,
          c(1, 2, 3, 4),
          drop = FALSE
        ]),
        silent = TRUE
      )
      if (inherits(gam_sig_table, 'try-error')) {
        object$trend_mgcv_model$R <- NULL
        gam_sig_table <- suppressWarnings(summary(
          object$trend_mgcv_model
        )$s.table[, c(1, 2, 3, 4), drop = FALSE])
        gam_sig_table[, 2] <- unlist(
          purrr::map(object$trend_mgcv_model$smooth, 'df'),
          use.names = FALSE
        )
      }
      if (!is.null(attr(object$trend_mgcv_model, 'gp_att_table'))) {
        gp_names <- unlist(purrr::map(
          attr(object$trend_mgcv_model, 'gp_att_table'),
          'name'
        ))
        if (
          all(
            rownames(gam_sig_table) %in%
              gsub('gp(', 's(', gp_names, fixed = TRUE)
          )
        ) {} else {
          gam_sig_table <- gam_sig_table[
            !rownames(gam_sig_table) %in%
              gsub('gp(', 's(', gp_names, fixed = TRUE),
            ,
            drop = FALSE
          ]

          cat("\nApproximate significance of GAM process smooths:\n")
          suppressWarnings(printCoefmat(
            gam_sig_table,
            digits = min(3, digits + 1),
            signif.stars = getOption("show.signif.stars"),
            has.Pvalue = TRUE,
            na.print = "NA",
            cs.ind = 1
          ))
        }
      } else {
        cat("\nApproximate significance of GAM process smooths:\n")
        suppressWarnings(printCoefmat(
          gam_sig_table,
          digits = min(3, digits + 1),
          signif.stars = getOption("show.signif.stars"),
          has.Pvalue = TRUE,
          na.print = "NA",
          cs.ind = 1
        ))
      }
    }
  }

  if (object$fit_engine == 'stan' & object$algorithm == 'sampling') {
    cat('\nStan MCMC diagnostics:\n')
    if (inherits(object, 'jsdgam')) {
      ignore_b_trend <- TRUE
    } else {
      ignore_b_trend <- FALSE
    }
    check_all_diagnostics(
      object$model_output,
      max_treedepth = object$max_treedepth,
      ignore_b_trend = ignore_b_trend
    )

    sampler <- attr(object$model_output@sim$samples[[1]], "args")$sampler_t
    if (sampler == "NUTS(diag_e)") {
      sampler <- 'sampling(hmc)'
    }
    cat(
      insight::format_message(
        paste0(
          "\nSamples were drawn using ",
          sampler,
          ". For each parameter, n_eff is",
          " a crude measure of effective",
          " sample size, and Rhat is the",
          " potential scale reduction factor",
          " on split MCMC chains (at",
          " convergence, Rhat = 1)"
        )
      )
    )
    cat('\n')
  }

  if (object$algorithm != 'sampling') {
    cat('\nPosterior approximation used: no diagnostics to compute\n')
  }

  if (object$fit_engine == 'jags') {
    cat('\nJAGS MCMC diagnostics:\n')
    rhats <- mcmc_summary(
      object$model_output,
      digits = digits,
      variational = variational
    )[, 6]
    if (any(rhats > 1.05)) {
      cat(
        '\nRhats above 1.05 found for',
        length(which(rhats > 1.05)),
        'parameters\n* Use pairs() to investigate\n'
      )
    } else {
      cat('\nRhat looks reasonable for all parameters\n')
    }
  }

  cat('\nUse how_to_cite() to get started describing this model')
}

#' @rdname summary.mvgam
#' @export
summary.mvgam_prefit = function(object, ...) {
  if (!is.null(object$trend_call)) {
    cat("\nGAM observation formula:\n")
    print(object$call)

    cat("\nGAM process formula:\n")
    print(object$trend_call)
  } else {
    cat("\nGAM formula:\n")
    print(object$call)
  }

  cat("\n\nFamily:\n")
  cat(paste0(object$family, '\n'))

  cat("\nLink function:\n")
  cat(paste0(family_links(object$family), '\n'))

  if (!inherits(object, 'jsdgam')) {
    cat("\nTrend model:\n")
    if (inherits(object$trend_model, 'mvgam_trend')) {
      print(object$trend_model$label)
      cat('\n')
    } else {
      cat(paste0(object$trend_model, '\n'))
    }
  }

  if (object$use_lv) {
    if (!is.null(object$trend_call)) {
      cat("\nN process models:\n")
      cat(object$n_lv, '\n')
    } else {
      cat("\nN latent factors:\n")
      cat(object$n_lv, '\n')
    }
  }

  if (inherits(object, 'jsdgam')) {
    cat('\nN species:\n')
    cat(NCOL(object$ytimes), '\n')
  } else {
    cat('\nN series:\n')
    cat(NCOL(object$ytimes), '\n')
  }

  if (inherits(object, 'jsdgam')) {
    cat('\nN sites:\n')
    cat(NROW(object$ytimes), '\n')
  } else {
    cat('\nN timepoints:\n')
    cat(NROW(object$ytimes), '\n')
  }

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
coef.mvgam = function(object, summarise = TRUE, ...) {
  coef_names <- names(object$mgcv_model$coefficients)

  if (summarise) {
    mvgam_coefs <- mcmc_summary(object$model_output, 'b')[, c(3:7)]
    rownames(mvgam_coefs) <- coef_names
  } else {
    mvgam_coefs <- mcmc_chains(object$model_output, 'b')
    colnames(mvgam_coefs) <- coef_names
  }

  return(mvgam_coefs)
}

#' Extract a clean mcmc_summary table of params
#' @param object An `mvgam` or `jsdgam` object
#' @param params A string of parameters to extract
#' @param digits The number of significant digits for printing out the summary
#' @param variational Logical indicating whether a variational approximation was used
#' @noRd
clean_summary_table = function(
  object,
  params,
  digits = 2,
  variational = FALSE
) {
  mcmc_summary(
    object$model_output,
    params,
    ISB = TRUE,
    digits = digits,
    variational = variational
  )[, c(3:7)]
}

#' Calculate and return summary table for GP parameters
#' @param object An `mvgam` or `jsdgam` object
#' @param mgcv_model A `gam` object containing GP effects
#' @param trend_effects Logical indicating whether this is a trend_mgcv_model
#' @param digits The number of significant digits for printing out the summary
#' @param variational Logical indicating whether a variational approximation was used
#' @noRd
gp_param_summary = function(
  object,
  mgcv_model,
  trend_effects = FALSE,
  digits = 2,
  variational = FALSE
) {
  # Extract GP name and isotropic information
  gp_names <- unlist(
    purrr::map(attr(mgcv_model, 'gp_att_table'), 'name'),
    use.names = FALSE
  )
  gp_isos <- unlist(
    purrr::map(attr(mgcv_model, 'gp_att_table'), 'iso'),
    use.names = FALSE
  )
  gp_dims <- unlist(
    purrr::map(attr(mgcv_model, 'gp_att_table'), 'dim'),
    use.names = FALSE
  )

  # Create full list of rho parameter names
  full_names <- vector(mode = 'list', length = length(gp_names))
  for (i in seq_len(length(gp_names))) {
    if (gp_isos[i]) {
      full_names[[i]] <- gp_names[i]
    } else {
      full_names[[i]] <- paste0(gp_names[i], '[', 1:gp_dims[i], ']')
    }
  }
  full_names <- unlist(full_names, use.names = FALSE)

  # Determine which parameters to extract
  if (trend_effects) {
    alpha_params <- gsub(
      'gp_',
      'gp_trend_',
      gsub(
        'series',
        'trend',
        paste0('alpha_', clean_gpnames(gp_names)),
        fixed = TRUE
      ),
      fixed = TRUE
    )
    rho_params <- gsub(
      'gp_',
      'gp_trend_',
      gsub(
        'series',
        'trend',
        paste0('rho_', clean_gpnames(gp_names)),
        fixed = TRUE
      ),
      fixed = TRUE
    )
  } else {
    alpha_params <- paste0('alpha_', clean_gpnames(gp_names))
    rho_params <- paste0('rho_', clean_gpnames(gp_names))
  }

  # Create summary tables
  alpha_summary <- clean_summary_table(
    object = object,
    params = alpha_params,
    digits = digits,
    variational = variational
  )
  rownames(alpha_summary) <- paste0('alpha_', gp_names)

  rho_summary <- clean_summary_table(
    object = object,
    params = rho_params,
    digits = digits,
    variational = variational
  )
  rownames(rho_summary) <- paste0('rho_', full_names)

  # Return as a list
  return(list(alpha_summary = alpha_summary, rho_summary = rho_summary))
}
