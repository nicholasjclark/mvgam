#' Function to add moving average processes and/or
#' correlated process errors to an existing Stan model file

#' When adding MA for univariate trends, 'error' needs to take same form
#' as trend / LV (array[n] vector[n_lv]) so it can be
#' extracted in the same way
#' @noRd
add_MaCor = function(model_file,
                     add_ma = FALSE,
                     add_cor = FALSE,
                     trend_model = 'VAR1',
                     drift = FALSE){

  if(trend_model %in% c('RW', 'AR1', 'AR2', 'AR3')){

    if(any(grepl('ytimes_trend', model_file))){
      remove_trendmus <- FALSE
    } else {
      remove_trendmus <- TRUE
    }

    # Update transformed data
      if(any(grepl('vector<lower=0>[n_lv] sigma;',
                   model_file, fixed = TRUE))){
        if(any(grepl('transformed data {', model_file, fixed = TRUE))){
          model_file[grep('transformed data {', model_file, fixed = TRUE)] <-
            paste0('transformed data {\n',
                   'vector[n_lv] trend_zeros = rep_vector(0.0, n_lv);')
        } else {
          model_file[grep('parameters {', model_file, fixed = TRUE)[1]] <-
            paste0('transformed data {\n',
                   'vector[n_lv] trend_zeros = rep_vector(0.0, n_lv);\n',
                   '}\nparameters {')
        }

      } else {
        if(any(grepl('transformed data {', model_file, fixed = TRUE))){
          model_file[grep('transformed data {', model_file, fixed = TRUE)] <-
            paste0('transformed data {\n',
                   'vector[n_series] trend_zeros = rep_vector(0.0, n_series);')
        } else {
          model_file[grep('parameters {', model_file, fixed = TRUE)[1]] <-
            paste0('transformed data {\n',
                   'vector[n_series] trend_zeros = rep_vector(0.0, n_series);\n',
                   '}\nparameters {')
        }

      }
      model_file <- readLines(textConnection(model_file), n = -1)

    # Update parameters block
    if(any(grepl('vector<lower=0>[n_lv] sigma;',
                 model_file, fixed = TRUE))){

      if(add_cor){
        model_file[grep('vector<lower=0>[n_lv] sigma;',
                        model_file, fixed = TRUE)] <-
          paste0('vector<lower=0>[n_lv] sigma;\n',
                 'cholesky_factor_corr[n_lv] L_Omega;')
      }

        model_file[grep('matrix[n, n_lv] LV;',
                        model_file, fixed = TRUE)] <-
          paste0('matrix[n, n_lv] LV;\n',
                 if(add_ma){
                   paste0('// ma coefficients\n',
                   if(add_cor){
                     'matrix<lower=-1,upper=1>[n_lv, n_lv] theta;'
                   } else {
                     'vector<lower=-1,upper=1>[n_lv] theta;'
                   })
                 } else {
                   NULL
                 },
                 '\n// dynamic error parameters\n',
                 'vector[n_lv] error[n];')

      model_file <- readLines(textConnection(model_file), n = -1)
      end <- grep('matrix[n, n_lv] LV;',
                  model_file, fixed = TRUE)
      start <- end - 1
      model_file <- model_file[-c(start:end)]

    } else {
      if(add_cor){
        model_file[grep('vector<lower=0>[n_series] sigma;',
                        model_file, fixed = TRUE)] <-
          paste0('vector<lower=0>[n_series] sigma;\n',
                 'cholesky_factor_corr[n_series] L_Omega;')
      }

      model_file[grep('matrix[n, n_series] trend;',
                      model_file, fixed = TRUE)] <-
        paste0('matrix[n, n_series] trend;\n',
               if(add_ma){
                 paste0(
                   '// ma coefficients\n',
                   if(add_cor){
                     'matrix<lower=-1,upper=1>[n_series, n_series] theta;'
                   } else {
                     'vector<lower=-1,upper=1>[n_series] theta;'
                   })
               } else {
                 NULL
               },
               '\n// dynamic error parameters\n',
               'vector[n_series] error[n];')

      model_file <- readLines(textConnection(model_file), n = -1)
      end <- grep('matrix[n, n_series] trend;',
                  model_file, fixed = TRUE)
      start <- end - 1
      model_file <- model_file[-c(start:end)]
    }
    model_file <- readLines(textConnection(model_file), n = -1)

    # Update transformed parameters
    if(any(grepl('vector<lower=0>[n_lv] sigma;',
                 model_file, fixed = TRUE))){
      model_file[grep('matrix[n, n_series] trend;',
                      model_file, fixed = TRUE)] <-
        paste0('matrix[n, n_series] trend;\n',
               if(add_cor){
                 paste0('vector[n_lv] LV[n];\n',
                        if(add_ma){
                          'vector[n_lv] epsilon[n];\n'
                        } else {
                          NULL
                        },
                        '// LKJ form of covariance matrix\n',
                        'matrix[n_lv, n_lv] L_Sigma;\n',
                        '// computed error covariance matrix\n',
                        'cov_matrix[n_lv] Sigma;')
               } else {
                 paste0('matrix[n, n_lv] LV;\n',
                        if(add_ma){
                          'matrix[n, n_lv] epsilon;'
                        } else {
                          NULL
                        })
               })

      if(add_cor){
        if(trend_model %in% c('AR1', 'RW')){
          if(any(grep('// derived latent states',
                      model_file, fixed = TRUE))){
            to_modify <- grep('// derived latent states',
                              model_file, fixed = TRUE)
          } else {
            to_modify <- grep('// derived latent trends',
                              model_file, fixed = TRUE)
          }
          model_file[to_modify] <-
            paste0('// derived latent states\n',
                   'LV[1] = ',
                   if(drift){ 'drift + '} else {NULL},
                   'trend_mus[ytimes_trend[1, 1:n_lv]] + error[1];\n',
                   if(add_ma){
                     'epsilon[1] = error[1];\n'
                   },
                   'for (i in 2:n) {\n',
                   if(add_ma){
                     paste0('// lagged error ma process\n',
                            'epsilon[i] = theta * error[i - 1];\n',
                            '// full ARMA process\n')
                   } else {
                     '// full AR process\n'
                   },
                   'LV[i] = ',
                   if(drift){ 'drift + '} else {NULL},
                   'trend_mus[ytimes_trend[i, 1:n_lv]] + ',
                   if(trend_model == 'AR1'){ 'ar1 .* '} else {NULL},
                   '(LV[i - 1] - trend_mus[ytimes_trend[i - 1, 1:n_lv]])',
                   if(add_ma){
                     '+ epsilon[i] + error[i];\n'
                   } else {
                     '+ error[i];\n'
                   },
                   '}\n')
        }

        if(trend_model == 'AR2'){
          if(any(grep('// derived latent states',
                      model_file, fixed = TRUE))){
            to_modify <- grep('// derived latent states',
                              model_file, fixed = TRUE)
          } else {
            to_modify <- grep('// derived latent trends',
                              model_file, fixed = TRUE)
          }
          model_file[to_modify] <-
            paste0('// derived latent states\n',
                   'LV[1] = ',
                   if(drift){ 'drift + '} else {NULL},
                   'trend_mus[ytimes_trend[1, 1:n_lv]] + error[1];\n',
                   if(add_ma){
                     paste0('epsilon[1] = error[1];\n',
                            'epsilon[2] = theta * error[1];\n')
                   } else {
                     NULL
                   },
                   'LV[2] = ',
                   if(drift){ 'drift + '} else {NULL},
                   'trend_mus[ytimes_trend[2, 1:n_lv]] + ',
                   'ar1 .* (LV[1] - trend_mus[ytimes_trend[1, 1:n_lv]]) + ',
                   if(add_ma){
                     'epsilon[2] + error[2];\n'
                   } else {
                     'error[2];\n'
                   },
                   'for (i in 3:n) {\n',
                   if(add_ma){
                     paste0('// lagged error ma process\n',
                            'epsilon[i] = theta * error[i - 1];\n',
                            '// full ARMA process\n')
                   } else {
                     '// full AR process\n'
                   },
                   'LV[i] = ',
                   if(drift){ 'drift + '} else {NULL},
                   'trend_mus[ytimes_trend[i, 1:n_lv]] + ',
                   'ar1 .* (LV[i - 1] - trend_mus[ytimes_trend[i - 1, 1:n_lv]]) + ',
                   'ar2 .* (LV[i - 2] - trend_mus[ytimes_trend[i - 2, 1:n_lv]]) + ',
                   if(add_ma){
                     'epsilon[i] + error[i];\n'
                   } else {
                     'error[i];\n'
                   },
                   '}\n')
        }

        if(trend_model == 'AR3'){
          if(any(grep('// derived latent states',
                      model_file, fixed = TRUE))){
            to_modify <- grep('// derived latent states',
                              model_file, fixed = TRUE)
          } else {
            to_modify <- grep('// derived latent trends',
                              model_file, fixed = TRUE)
          }
          model_file[to_modify] <-
            paste0('// derived latent states\n',
                   'LV[1] = ',
                   if(drift){ 'drift + '} else {NULL},
                   'trend_mus[ytimes_trend[1, 1:n_lv]] + error[1];\n',
                   if(add_ma){
                     paste0('epsilon[1] = error[1];\n',
                            'epsilon[2] = theta * error[1];\n',
                            'epsilon[3] = theta * error[2];\n')
                   } else {
                     NULL
                   },
                   'LV[2] = ',
                   if(drift){ 'drift + '} else {NULL},
                   'trend_mus[ytimes_trend[2, 1:n_lv]] + ',
                   'ar1 .* (LV[1] - trend_mus[ytimes_trend[1, 1:n_lv]]) + ',
                   if(add_ma){
                     'epsilon[2] + error[2];\n'
                   } else {
                     'error[2];\n'
                   },
                   'LV[3] = ',
                   if(drift){ 'drift + '} else {NULL},
                   'trend_mus[ytimes_trend[3, 1:n_lv]] + ',
                   'ar1 .* (LV[2] - trend_mus[ytimes_trend[2, 1:n_lv]]) + ',
                   'ar2 .* (LV[1] - trend_mus[ytimes_trend[1, 1:n_lv]]) + ',
                   if(add_ma){
                     'epsilon[3] + error[3];\n'
                   } else {
                     'error[3];\n'
                   },
                   'for (i in 4:n) {\n',
                   if(add_ma){
                     paste0('// lagged error ma process\n',
                            'epsilon[i] = theta * error[i - 1];\n',
                            '// full ARMA process\n')
                   } else {
                     '// full AR process\n'
                   },
                   'LV[i] = ',
                   if(drift){ 'drift + '} else {NULL},
                   'trend_mus[ytimes_trend[i, 1:n_lv]] + ',
                   'ar1 .* (LV[i - 1] - trend_mus[ytimes_trend[i - 1, 1:n_lv]]) + ',
                   'ar2 .* (LV[i - 2] - trend_mus[ytimes_trend[i - 2, 1:n_lv]]) + ',
                   'ar3 .* (LV[i - 3] - trend_mus[ytimes_trend[i - 3, 1:n_lv]]) + ',
                   if(add_ma){
                     'epsilon[i] + error[i];\n'
                   } else {
                     'error[i];\n'
                   },
                   '}\n')
        }

      } else {
        if(trend_model %in% c('AR1', 'RW')){
          if(any(grep('// derived latent states',
                      model_file, fixed = TRUE))){
            to_modify <- grep('// derived latent states',
                              model_file, fixed = TRUE)
          } else {
            to_modify <- grep('// derived latent trends',
                              model_file, fixed = TRUE)
          }
          model_file[to_modify] <-
            paste0('// derived latent states\n',
                   'for(j in 1:n_lv){\n',
                   'LV[1, j] = ',
                   if(drift){ 'drift[j] + '} else {NULL},
                   'trend_mus[ytimes_trend[1, j]] + error[1, j];\n',
                   'epsilon[1, j] = error[1, j];\n',
                   'for(i in 2:n){\n',
                   '// lagged error ma process\n',
                   'epsilon[i, j] = theta[j] * error[i-1, j];\n',
                   '// full ARMA process\n',
                   'LV[i, j] = ',
                   if(drift){ 'drift[j] + '} else {NULL},
                   'trend_mus[ytimes_trend[i, j]] + ',
                   if(trend_model == 'AR1'){ 'ar1[j] * '} else {NULL},
                   '(LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]]) + ',
                   'epsilon[i, j] + error[i, j];\n',
                   '}\n}')
        }

        if(trend_model == 'AR2'){
          if(any(grep('// derived latent states',
                      model_file, fixed = TRUE))){
            to_modify <- grep('// derived latent states',
                              model_file, fixed = TRUE)
          } else {
            to_modify <- grep('// derived latent trends',
                              model_file, fixed = TRUE)
          }
          model_file[to_modify] <-
            paste0('// derived latent states\n',
                   'for(j in 1:n_lv){\n',
                   'LV[1, j] = ',
                   if(drift){ 'drift[j] + '} else {NULL},
                   'trend_mus[ytimes_trend[1, j]] + error[1, j];\n',
                   'epsilon[1, j] = error[1, j];\n',
                   'epsilon[2, j] = theta[j] * error[1, j];\n',
                   'LV[2, j] = ',
                   if(drift){ 'drift[j] + '} else {NULL},
                   'trend_mus[ytimes_trend[1, j]] + ',
                   'ar1[j] * (LV[1, j] - trend_mus[ytimes_trend[1, j]]) + ',
                   'epsilon[2, j] + error[2, j];\n',
                   'for(i in 3:n){\n',
                   '// lagged error ma process\n',
                   'epsilon[i, j] = theta[j] * error[i-1, j];\n',
                   '// full ARMA process\n',
                   'LV[i, j] = ',
                   if(drift){ 'drift[j] + '} else {NULL},
                   'trend_mus[ytimes_trend[i, j]] + ',
                   'ar1[j] * (LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]]) + ',
                   'ar2[j] * (LV[i - 2, j] - trend_mus[ytimes_trend[i - 2, j]]) + ',
                   'epsilon[i, j] + error[i, j];\n',
                   '}\n}')
        }

        if(trend_model == 'AR3'){
          if(any(grep('// derived latent states',
                      model_file, fixed = TRUE))){
            to_modify <- grep('// derived latent states',
                              model_file, fixed = TRUE)
          } else {
            to_modify <- grep('// derived latent trends',
                              model_file, fixed = TRUE)
          }
          model_file[to_modify] <-
            paste0('// derived latent states\n',
                   'for(j in 1:n_lv){\n',
                   'LV[1, j] = ',
                   if(drift){ 'drift[j] + '} else {NULL},
                   'trend_mus[ytimes_trend[1, j]] + error[1, j];\n',
                   'epsilon[1, j] = error[1, j];\n',
                   'epsilon[2, j] = theta[j] * error[1, j];\n',
                   'epsilon[3, j] = theta[j] * error[2, j];\n',
                   'LV[2, j] = ',
                   if(drift){ 'drift[j] + '} else {NULL},
                   'trend_mus[ytimes_trend[2, j]] + ',
                   'ar1[j] * (LV[1, j] - trend_mus[ytimes_trend[1, j]]) + ',
                   'epsilon[2, j] + error[2, j];\n',
                   'LV[3, j] = ',
                   if(drift){ 'drift[j] + '} else {NULL},
                   'trend_mus[ytimes_trend[1, j]] + ',
                   'ar1[j] * (LV[2, j] - trend_mus[ytimes_trend[2, j]]) + ',
                   'ar2[j] * (LV[1, j] - trend_mus[ytimes_trend[1, j]]) + ',
                   'epsilon[3, j] + error[3, j];\n',
                   'for(i in 4:n){\n',
                   '// lagged error ma process\n',
                   'epsilon[i, j] = theta[j] * error[i-1, j];\n',
                   '// full ARMA process\n',
                   'LV[i, j] = ',
                   if(drift){ 'drift[j] + '} else {NULL},
                   'trend_mus[ytimes_trend[i, j]] + ',
                   'ar1[j] * (LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]]) + ',
                   'ar2[j] * (LV[i - 2, j] - trend_mus[ytimes_trend[i - 2, j]]) + ',
                   'ar3[j] * (LV[i - 3, j] - trend_mus[ytimes_trend[i - 3, j]]) + ',
                   'epsilon[i, j] + error[i, j];\n',
                   '}\n}')
        }
      }

      if(add_cor){
        model_file[grep('lv_coefs = Z;', model_file, fixed = TRUE)] <-
          paste0('L_Sigma = diag_pre_multiply(sigma, L_Omega);\n',
                 'Sigma = multiply_lower_tri_self_transpose(L_Sigma);\n',
                 'lv_coefs = Z;')
      }
    } else {
      model_file[grep('transformed parameters {',
                      model_file, fixed = TRUE)] <-
        paste0('transformed parameters {\n',
               if(add_cor){
                 paste0('vector[n_series] trend_raw[n];\n',
                        'matrix[n, n_series] trend;\n',
                        if(add_ma){
                          'vector[n_series] epsilon[n];\n'
                        } else {
                          NULL
                        },
                        '// LKJ form of covariance matrix\n',
                        'matrix[n_series, n_series] L_Sigma;\n',
                        '// computed error covariance matrix\n',
                        'cov_matrix[n_series] Sigma;')
               } else {
                 paste0('matrix[n, n_series] trend;\n',
                        if(add_ma){
                          'matrix[n, n_series] epsilon;'
                        } else {
                          NULL
                        })
               })

      if(add_cor){
        if(trend_model %in% c('AR1', 'RW')){
          if(any(grepl('= mu_raw[',
                      model_file, fixed = TRUE))){
            insert_line <- max(grep('= mu_raw[',
                                    model_file, fixed = TRUE))
          } else if(any(grepl('= b_raw[',
                              model_file, fixed = TRUE))){
            insert_line <- max(grep('= b_raw[',
                                    model_file, fixed = TRUE))
          }
          model_file[insert_line] <-
            paste0(model_file[insert_line],
                   '\n// derived latent states\n',
                   'trend_raw[1] = ',
                   if(drift){ 'drift + '} else {NULL},
                   'error[1];\n',
                   if(add_ma){
                     'epsilon[1] = error[1];\n'
                   } else {
                     NULL
                   },
                   'for (i in 2:n) {\n',
                   if(add_ma){
                     paste0('// lagged error ma process\n',
                            'epsilon[i] = theta * error[i - 1];\n',
                            '// full ARMA process\n')
                   } else {
                     paste0('// full AR process\n')
                   },
                   'trend_raw[i] = ',
                   if(drift){ 'drift + '} else {NULL},
                   if(trend_model == 'AR1'){ 'ar1 .* '} else {NULL},
                   'trend_raw[i - 1] + ',
                   if(add_ma){
                     'epsilon[i] + error[i];\n'
                   } else {
                     'error[i];\n'
                   },
                   '}\n')
        }

        if(trend_model == 'AR2'){
          if(any(grepl('= mu_raw[',
                       model_file, fixed = TRUE))){
            insert_line <- max(grep('= mu_raw[',
                                    model_file, fixed = TRUE))
          } else if(any(grepl('= b_raw[',
                              model_file, fixed = TRUE))){
            insert_line <- max(grep('= b_raw[',
                                    model_file, fixed = TRUE))
          }
          model_file[insert_line] <-
            paste0(model_file[insert_line],
                   '\n// derived latent states\n',
                   'trend_raw[1] = ',
                   if(drift){ 'drift + '} else {NULL},
                   'error[1];\n',
                   if(add_ma){
                     paste0('epsilon[1] = error[1];\n',
                            'epsilon[2] = theta * error[1];\n')
                   } else {
                     NULL
                   },
                   'trend_raw[2] = ',
                   if(drift){ 'drift + '} else {NULL},
                   'ar1 .* trend_raw[1] + ',
                   if(add_ma){
                     'epsilon[2] + error[2];\n'
                   } else {
                     'error[2];\n'
                   },
                   'for (i in 3:n) {\n',
                   if(add_ma){
                     paste0('// lagged error ma process\n',
                            'epsilon[i] = theta * error[i - 1];\n',
                            '// full ARMA process\n')
                   } else {
                     '// full AR process\n'
                   },
                   'trend_raw[i] = ',
                   if(drift){ 'drift + '} else {NULL},
                   'ar1 .* trend_raw[i - 1] + ',
                   'ar2 .* trend_raw[i - 2] + ',
                   if(add_ma){
                     'epsilon[i] + error[i];\n'
                   } else {
                     'error[i];\n'
                   },
                   '}\n')
        }

        if(trend_model == 'AR3'){
          if(any(grepl('= mu_raw[',
                       model_file, fixed = TRUE))){
            insert_line <- max(grep('= mu_raw[',
                                    model_file, fixed = TRUE))
          } else if(any(grepl('= b_raw[',
                              model_file, fixed = TRUE))){
            insert_line <- max(grep('= b_raw[',
                                    model_file, fixed = TRUE))
          }
          model_file[insert_line] <-
            paste0(model_file[insert_line],
                   '\n// derived latent states\n',
                   'trend_raw[1] = ',
                   if(drift){ 'drift + '} else {NULL},
                   'error[1];\n',
                   if(add_ma){
                     paste0('epsilon[1] = error[1];\n',
                            'epsilon[2] = theta * error[1];\n',
                            'epsilon[3] = theta * error[2];\n')
                   } else {
                     NULL
                   },
                   'trend_raw[2] = ',
                   if(drift){ 'drift + '} else {NULL},
                   'ar1 .* trend_raw[1] + ',
                   if(add_ma){
                     'epsilon[2] + error[2];\n'
                   } else {
                     'error[2];\n'
                   },
                   'trend_raw[3] = ',
                   if(drift){ 'drift + '} else {NULL},
                   'ar1 .* trend_raw[2] + ',
                   'ar2 .* trend_raw[1] + ',
                   if(add_ma){
                     'epsilon[3] + error[3];\n'
                   } else {
                     'error[3];\n'
                   },
                   'for (i in 4:n) {\n',
                   if(add_ma){
                     paste0('// lagged error ma process\n',
                            'epsilon[i] = theta * error[i - 1];\n',
                            '// full ARMA process\n')
                   } else {
                     '// full AR process\n'
                   },
                   'trend_raw[i] = ',
                   if(drift){ 'drift + '} else {NULL},
                   'ar1 .* trend_raw[i - 1] + ',
                   'ar2 .* trend_raw[i - 2] + ',
                   'ar3 .* trend_raw[i - 3] + ',
                   if(add_ma){
                     'epsilon[i] + error[i];\n'
                   } else {
                     'error[i];\n'
                   },
                   '}\n')
        }

      } else {
        if(trend_model %in% c('AR1', 'RW')){
          if(any(grepl('= mu_raw[',
                       model_file, fixed = TRUE))){
            insert_line <- max(grep('= mu_raw[',
                                    model_file, fixed = TRUE))
          } else if(any(grepl('= b_raw[',
                              model_file, fixed = TRUE))){
            insert_line <- max(grep('= b_raw[',
                                    model_file, fixed = TRUE))
          }
          model_file[insert_line] <-
            paste0(model_file[insert_line],
                   '\nfor(j in 1:n_series){\n',
                   'trend[1, j] = ',
                   if(drift){ 'drift[j] + '} else {NULL},
                   'error[1, j];\n',
                   'epsilon[1, j] = error[1, j];\n',
                   'for(i in 2:n){\n',
                   '// lagged error ma process\n',
                   'epsilon[i, j] = theta[j] * error[i-1, j];\n',
                   '// full ARMA process\n',
                   'trend[i, j] = ',
                   if(drift){ 'drift[j] + '} else {NULL},
                   if(trend_model == 'AR1'){ 'ar1[j] * '} else {NULL},
                   'trend[i - 1, j] + ',
                   'epsilon[i, j] + error[i, j];\n',
                   '}\n}')
        }

        if(trend_model == 'AR2'){
          if(any(grepl('= mu_raw[',
                       model_file, fixed = TRUE))){
            insert_line <- max(grep('= mu_raw[',
                                    model_file, fixed = TRUE))
          } else if(any(grepl('= b_raw[',
                              model_file, fixed = TRUE))){
            insert_line <- max(grep('= b_raw[',
                                    model_file, fixed = TRUE))
          }
          model_file[insert_line] <-
            paste0(model_file[insert_line],
                   '\nfor(j in 1:n_series){\n',
                   'trend[1, j] = ',
                   if(drift){ 'drift[j] + '} else {NULL},
                   'error[1, j];\n',
                   'epsilon[1, j] = error[1, j];\n',
                   'epsilon[2, j] = theta[j] * error[1, j];\n',
                   'trend[2, j] = ',
                   if(drift){ 'drift[j] + '} else {NULL},
                   'ar1[j] * trend[1, j] + ',
                   'epsilon[2, j] + error[2, j];\n',
                   'for(i in 3:n){\n',
                   '// lagged error ma process\n',
                   'epsilon[i, j] = theta[j] * error[i-1, j];\n',
                   '// full ARMA process\n',
                   'trend[i, j] = ',
                   if(drift){ 'drift[j] + '} else {NULL},
                   'ar1[j] * trend[i - 1, j] + ',
                   'ar2[j] * trend[i - 2, j] + ',
                   'epsilon[i, j] + error[i, j];\n',
                   '}\n}')
        }

        if(trend_model == 'AR3'){
          if(any(grepl('= mu_raw[',
                       model_file, fixed = TRUE))){
            insert_line <- max(grep('= mu_raw[',
                                    model_file, fixed = TRUE))
          } else if(any(grepl('= b_raw[',
                              model_file, fixed = TRUE))){
            insert_line <- max(grep('= b_raw[',
                                    model_file, fixed = TRUE))
          }
          model_file[insert_line] <-
            paste0(model_file[insert_line],
                   '\nfor(j in 1:n_series){\n',
                   'trend[1, j] = ',
                   if(drift){ 'drift[j] + '} else {NULL},
                   'error[1, j];\n',
                   'epsilon[1, j] = error[1, j];\n',
                   'epsilon[2, j] = theta[j] * error[1, j];\n',
                   'epsilon[3, j] = theta[j] * error[2, j];\n',
                   'trend[2, j] = ',
                   if(drift){ 'drift[j] + '} else {NULL},
                   'ar1[j] * trend[1, j] + ',
                   'epsilon[2, j] + error[2, j];\n',
                   'trend[3, j] = ',
                   if(drift){ 'drift[j] + '} else {NULL},
                   'ar1[j] * trend[2, j] + ',
                   'ar2[j] * trend[1, j] + ',
                   'epsilon[2, j] + error[2, j];\n',
                   'for(i in 4:n){\n',
                   '// lagged error ma process\n',
                   'epsilon[i, j] = theta[j] * error[i-1, j];\n',
                   '// full ARMA process\n',
                   'trend[i, j] = ',
                   if(drift){ 'drift[j] + '} else {NULL},
                   'ar1[j] * trend[i - 1, j] + ',
                   'ar2[j] * trend[i - 2, j] + ',
                   'ar3[j] * trend[i - 3, j] + ',
                   'epsilon[i, j] + error[i, j];\n',
                   '}\n}')
        }
      }

      model_file <- readLines(textConnection(model_file), n = -1)
      if(add_cor){

        last <- grep('model {', model_file, fixed = TRUE)
        for(i in last:(last - 5)){
          last <- i
          if(trimws(model_file[i]) != '}'){
          } else {
            break
          }
        }

        model_file[last] <-
          paste0('\nL_Sigma = diag_pre_multiply(sigma, L_Omega);\n',
                 'Sigma = multiply_lower_tri_self_transpose(L_Sigma);\n',
                 'for (i in 1:n) {\n',
                 'trend[i, 1:n_series] = to_row_vector(trend_raw[i]);\n',
                 '}\n}')
      }
    }
    model_file <- readLines(textConnection(model_file), n = -1)

    # Update model block
    if(any(grepl('vector<lower=0>[n_lv] sigma;',
                 model_file, fixed = TRUE))){
      if(any(grepl('LV[1, j] ~ normal',
                        model_file, fixed = TRUE))){
        start <- grep('LV[1, j] ~ normal',
                      model_file, fixed = TRUE) - 1
        end <- grep('LV[i, j] ~ normal',
                    model_file, fixed = TRUE) + 2
      } else {
        start <- grep('LV[1, 1:n_lv] ~ normal(',
                      model_file, fixed = TRUE) - 1
        first <- grep(':n, j] ~ normal(', model_file, fixed = TRUE)
        second <- grep('sigma[j]);', model_file, fixed = TRUE)
        end <- intersect(first, second) + 1
      }

      model_file <- model_file[-c(start:end)]
      model_file[start] <- paste0(
        '// contemporaneous errors\n',
        if(add_cor){
          paste0('L_Omega ~ lkj_corr_cholesky(2);\n',
                 'for(i in 1:n) {\n',
                 'error[i] ~ multi_normal_cholesky(trend_zeros, L_Sigma);\n',
                 '}')
        } else {
          paste0('for(i in 1:n) {\n',
                 'error[i] ~ normal(trend_zeros, sigma);\n',
                 '}')
        },
        if(add_ma){
          paste0('\n// ma coefficients\n',
                 if(add_cor){
                   paste0('for(i in 1:n_lv){\n',
                          'for(j in 1:n_lv){\n',
                          'if (i != j)\n',
                          'theta[i, j] ~ normal(0, 0.2);\n',
                          '}\n}')
                 } else {
                   'theta ~ normal(0, 0.2);'
                 })
        } else {
          NULL
        },
        '\n',
        model_file[start])
    } else {
      start <- grep('trend[1, 1:n_series] ~ normal(',
                    model_file, fixed = TRUE) - 1
      first <- grep(':n, s] ~ normal(', model_file, fixed = TRUE)
      second <- grep('sigma[s]);', model_file, fixed = TRUE)
      end <- intersect(first, second) + 1

      model_file <- model_file[-c(start:end)]
      model_file[start] <- paste0(
        '// contemporaneous errors\n',
        if(add_cor){
          paste0('L_Omega ~ lkj_corr_cholesky(2);\n',
                 'for(i in 1:n) {\n',
                 'error[i] ~ multi_normal_cholesky(trend_zeros, L_Sigma);\n',
                 '}')
        } else {
          paste0('for(i in 1:n) {\n',
                 'error[i] ~ normal(trend_zeros, sigma);\n',
                 '}')
        },
        if(add_ma){
          paste0('\n// ma coefficients\n',
                 if(add_cor){
                   paste0('for(i in 1:n_series){\n',
                          'for(j in 1:n_series){\n',
                          'if (i != j)\n',
                          'theta[i, j] ~ normal(0, 0.2);\n',
                          '}\n}')
                 } else {
                   'theta ~ normal(0, 0.2);'
                 })
        } else {
          NULL
        },
        '\n',
        model_file[start])
    }

    if(remove_trendmus){
      model_file <- gsub('trend_mus[ytimes_trend[1, 1:n_lv]] +',
                         '',
                         model_file, fixed = TRUE)
      model_file <- gsub('trend_mus[ytimes_trend[i, 1:n_lv]] + ',
                         '',
                         model_file, fixed = TRUE)
      model_file <- gsub(' - trend_mus[ytimes_trend[i - 1, 1:n_lv]]',
                         '',
                         model_file, fixed = TRUE)
      model_file <- gsub(' - trend_mus[ytimes_trend[1, 1:n_lv]]',
                         '',
                         model_file, fixed = TRUE)
      model_file <- gsub(' - trend_mus[ytimes_trend[i - 2, 1:n_lv]]',
                         '',
                         model_file, fixed = TRUE)
      model_file <- gsub('trend_mus[ytimes_trend[2, 1:n_lv]] + ',
                         '',
                         model_file, fixed = TRUE)
      model_file <- gsub('trend_mus[ytimes_trend[3, 1:n_lv]] + ',
                         '',
                         model_file, fixed = TRUE)
      model_file <- gsub(' - trend_mus[ytimes_trend[2, 1:n_lv]]',
                         '',
                         model_file, fixed = TRUE)
      model_file <- gsub(' - trend_mus[ytimes_trend[i - 3, 1:n_lv]]',
                         '',
                         model_file, fixed = TRUE)
    }
    model_file <- readLines(textConnection(model_file), n = -1)
  }

  if(trend_model == 'VAR1'){
    # Only ma can be added for VAR models currently
    # Replace the reverse mapping function with the MA representation
    start <- grep('/* Function to perform the reverse mapping*/',
                  model_file, fixed = TRUE)
    end <- grep('return phiGamma;',
                model_file, fixed = TRUE) + 1
    model_file <- model_file[-c(start:end)]
    model_file[grep('return mdivide_left_spd(sqrtm(B), P_real);',
                    model_file, fixed = TRUE) + 1] <-
      paste0('}\n',
             '/* Function to compute Kronecker product */\n\n',
             '/* see Heaps 2022 for details (https://doi.org/10.1080/10618600.2022.2079648)*/\n',
             'matrix kronecker_prod(matrix A, matrix B) {\n',
             'matrix[rows(A) * rows(B), cols(A) * cols(B)] C;\n',
             'int m = rows(A);\n',
             'int n = cols(A);\n',
             'int p = rows(B);\n',
             'int q = cols(B);\n',
             'for (i in 1:m) {\n',
             'for (j in 1:n) {\n',
             'int row_start = (i - 1) * p + 1;\n',
             'int row_end = (i - 1) * p + p;\n',
             'int col_start = (j - 1) * q + 1;\n',
             'int col_end = (j - 1) * q + q;\n',
             'C[row_start:row_end, col_start:col_end] = A[i, j] * B;\n',
             '}\n',
             '}\n',
             'return C;\n',
             '}\n',
             '/* Function to perform the reverse mapping\n\n',
             '/* see Heaps 2022 for details (https://doi.org/10.1080/10618600.2022.2079648)*/\n',
             'matrix[] rev_mapping(matrix[] P, matrix Sigma) {\n',
             'int p = size(P);\n',
             'int m = rows(Sigma);\n',
             'matrix[m, m] phi_for[p, p];   matrix[m, m] phi_rev[p, p];\n',
             'matrix[m, m] Sigma_for[p+1];  matrix[m, m] Sigma_rev[p+1];\n',
             'matrix[m, m] S_for;           matrix[m, m] S_rev;\n',
             'matrix[m, m] S_for_list[p+1];\n',
             '// Step 1:\n',
             'Sigma_for[p+1] = Sigma;\n',
             'S_for_list[p+1] = sqrtm(Sigma);\n',
             'for(s in 1:p) {\n',
             '// In this block of code S_rev is B^{-1} and S_for is a working matrix\n',
             'S_for = - tcrossprod(P[p-s+1]);\n',
             'for(i in 1:m) S_for[i, i] += 1.0;\n',
             'S_rev = sqrtm(S_for);\n',
             'S_for_list[p-s+1] = mdivide_right_spd(mdivide_left_spd(S_rev,\n',
             'sqrtm(quad_form_sym(Sigma_for[p-s+2], S_rev))), S_rev);\n',
             'Sigma_for[p-s+1] = tcrossprod(S_for_list[p-s+1]);\n',
             '}\n',
             '// Step 2:\n',
             'Sigma_rev[1] = Sigma_for[1];\n',
             'for(s in 0:(p-1)) {\n',
             'S_for = S_for_list[s+1];\n',
             'S_rev = sqrtm(Sigma_rev[s+1]);\n',
             'phi_for[s+1, s+1] = mdivide_right_spd(S_for * P[s+1], S_rev);\n',
             "phi_rev[s+1, s+1] = mdivide_right_spd(S_rev * P[s+1]', S_for);\n",
             'if(s>=1) {\n',
             'for(k in 1:s) {\n',
             'phi_for[s+1, k] = phi_for[s, k] - phi_for[s+1, s+1] * phi_rev[s, s-k+1];\n',
             'phi_rev[s+1, k] = phi_rev[s, k] - phi_rev[s+1, s+1] * phi_for[s, s-k+1];\n',
             '}\n',
             '}\n',
             'Sigma_rev[s+2] = Sigma_rev[s+1] - quad_form_sym(Sigma_for[s+1],\n',
             "phi_rev[s+1, s+1]');\n",
             '}\n',
             'return phi_for[p];\n',
             '}\n',

             '/* Function to compute the joint (stationary) distribution of\n',
             '(y_0, ..., y_{1-p}, eps_0, ..., eps_{1-q})\n\n',
             '/* see Heaps 2022 for details (https://doi.org/10.1080/10618600.2022.2079648)*/\n',
             'matrix initial_joint_var(matrix Sigma, matrix[] phi, matrix[] theta) {\n',
             'int p = size(phi);\n',
             'int q = size(theta);\n',
             'int m = rows(Sigma);\n',
             'matrix[(p+q)*m, (p+q)*m] companion_mat = rep_matrix(0.0, (p+q)*m, (p+q)*m);\n',
             'matrix[(p+q)*m, (p+q)*m] companion_var = rep_matrix(0.0, (p+q)*m, (p+q)*m);\n',
             'matrix[(p+q)*m*(p+q)*m, (p+q)*m*(p+q)*m] tmp = diag_matrix(rep_vector(1.0,\n',
             '(p+q)*m*(p+q)*m));\n',
             'matrix[(p+q)*m, (p+q)*m] Omega;\n',
             '// Construct phi_tilde:\n',
             'for(i in 1:p) {\n',
             'companion_mat[1:m, ((i-1)*m+1):(i*m)] = phi[i];\n',
             'if(i>1) {\n',
             'for(j in 1:m) {\n',
             'companion_mat[(i-1)*m+j, (i-2)*m+j] = 1.0;\n',
             '}\n',
             '}\n',
             '}\n',
             'for(i in 1:q) {\n',
             'companion_mat[1:m, ((p+i-1)*m+1):((p+i)*m)] = theta[i];\n',
             '}\n',
             'if(q>1) {\n',
             'for(i in 2:q) {\n',
             'for(j in 1:m) {\n',
             'companion_mat[(p+i-1)*m+j, (p+i-2)*m+j] = 1.0;\n',
             '}\n',
             '}\n',
             '}\n',
             '// Construct Sigma_tilde:\n',
             'companion_var[1:m, 1:m] = Sigma;\n',
             'companion_var[(p*m+1):((p+1)*m), (p*m+1):((p+1)*m)] = Sigma;\n',
             'companion_var[1:m, (p*m+1):((p+1)*m)] = Sigma;\n',
             'companion_var[(p*m+1):((p+1)*m), 1:m] = Sigma;\n',
             '// Compute Gamma0_tilde\n',
             'tmp -= kronecker_prod(companion_mat, companion_mat);\n',
             "Omega = to_matrix(tmp \\ to_vector(companion_var), (p+q)*m, (p+q)*m);\n",
             '// Ensure Omega is symmetric:\n',
             'for(i in 1:(rows(Omega)-1)) {\n',
             'for(j in (i+1):rows(Omega)) {\n',
             'Omega[j, i] = Omega[i, j];\n',
             '}\n',
             '}\n',
             'return Omega;\n',
             '}\n')
    model_file <- readLines(textConnection(model_file), n = -1)

    # Update transformed data
    if(any(grepl('cholesky_factor_corr[n_lv] L_Omega;',
                 model_file, fixed = TRUE))){
      model_file[grep('transformed data {',
                      model_file, fixed = TRUE)] <- paste0(
                        'transformed data {\n',
                        'vector[n_lv] trend_zeros = rep_vector(0.0, n_lv);\n',
                        'vector[n_lv*2] init_zeros = rep_vector(0.0, n_lv*2);\n')
    } else {
      model_file[grep('vector[n_series] trend_zeros = rep_vector(0.0, n_series);',
                      model_file, fixed = TRUE)] <- paste0(
                        'vector[n_series] trend_zeros = rep_vector(0.0, n_series);\n',
                        'vector[n_series*2] init_zeros = rep_vector(0.0, n_series*2);\n')
    }
    model_file <- readLines(textConnection(model_file), n = -1)

    # Update parameters
    if(any(grepl('cholesky_factor_corr[n_lv] L_Omega;',
                 model_file, fixed = TRUE))){

      model_file[grep('matrix[n_lv, n_lv] P_real;',
                      model_file, fixed = TRUE)] <- paste0(
                        'matrix[n_lv, n_lv] P_real;\n',
                        '// unconstrained MA partial autocorrelations\n',
                        'matrix[n_lv, n_lv] R_real;\n',
                        '// initial joint stationary VARMA process\n',
                        'vector[2 * n_lv] init;\n',
                        '// ma error parameters\n',
                        'vector[n_lv] error[n];')
    } else {
      model_file[grep('matrix[n_series, n_series] P_real;',
                      model_file, fixed = TRUE)] <- paste0(
                        'matrix[n_series, n_series] P_real;\n',
                        '// unconstrained MA partial autocorrelations\n',
                        'matrix[n_series, n_series] R_real;\n',
                        '// initial joint stationary VARMA process\n',
                        'vector[2 * n_series] init;\n',
                        '// ma error parameters\n',
                        'vector[n_series] error[n];')
    }

    # Update transformed parameters
    if(any(grepl('cholesky_factor_corr[n_lv] L_Omega;',
                 model_file, fixed = TRUE))){
      model_file[grep('matrix[n_lv, n_lv] A;',
                      model_file, fixed = TRUE)] <- paste0(
                        'matrix[n_lv, n_lv] A;\n',
                        '// latent trend MA autoregressive terms\n',
                        'matrix[n_lv, n_lv] theta;\n',
                        '// ma process\n',
                        'array[n] vector[n_lv] epsilon;\n')

      end <- grep('vector[n_lv] LV[n];', model_file, fixed = TRUE)
      start <- end - 1
      model_file <- model_file[-c(start:end)]

      model_file[grep('cov_matrix[n_lv] Gamma;',
                      model_file, fixed = TRUE)] <- paste0(
                        'cov_matrix[n_lv * 2] Omega;\n',
                        "// latent states\n",
                        "vector[n_lv] LV[n];")

      start <- grep('// derived latent states', model_file, fixed = TRUE)
      end <- grep('Gamma = phiGamma[2, 1];', model_file, fixed = TRUE) + 1
      model_file <- model_file[-c(start:end)]
      model_file[start] <- paste0(
        model_file[start], '\n',
        '// stationary VARMA reparameterisation\n',
        'L_Sigma = diag_pre_multiply(sigma, L_Omega);\n',
        'Sigma = multiply_lower_tri_self_transpose(L_Sigma);\n',

        '{\n',
        '// constrained partial autocorrelations\n',
        'matrix[n_lv, n_lv] P[1];\n',
        'matrix[n_lv, n_lv] R[1];\n',
        '// stationary autoregressive coefficients\n',
        'matrix[n_lv, n_lv] A_init[1];\n',
        'matrix[n_lv, n_lv] theta_init[1];\n',
        'P[1] = P_realtoP(P_real);\n',
        'R[1] = P_realtoP(R_real);\n',
        '// stationary autoregressive and ma coef matrices\n',
        'A_init = rev_mapping(P, Sigma);\n',
        'theta_init = rev_mapping(R, Sigma);\n',
        'theta_init[1] = -theta_init[1];\n',
        '// initial stationary covariance structure\n',
        'Omega = initial_joint_var(Sigma, A_init, theta_init);\n',
        'A = A_init[1];\n',
        'theta = theta_init[1];\n',
        '}\n',

        '// computed VARMA trends\n',
        'epsilon[1] = theta * init[(n_lv + 1) : (n_lv * 2)];\n',
        'LV[1] = (A * init[1 : n_lv]) + trend_mus[ytimes_trend[1, 1 : n_lv]] + epsilon[1] + error[1];\n',
        'for (i in 2 : n) {\n',
        '// lagged error ma process\n',
        'epsilon[i] = theta * error[i - 1];\n',
        '// full VARMA process\n',
        'LV[i] = trend_mus[ytimes_trend[i, 1 : n_lv]] + A * (LV[i - 1] - trend_mus[ytimes_trend[i - 1, 1 : n_lv]]) + epsilon[i] + error[i];\n',
        '}\n',

        '// derived latent states\n',
        'lv_coefs = Z;\n',
        'for (i in 1 : n) {\n',
        'for (s in 1 : n_series) {\n',
        'trend[i, s] = dot_product(lv_coefs[s,  : ], LV[i]);\n',
        '}\n}')

    } else {
      model_file[grep('matrix[n_series, n_series] A;',
                      model_file, fixed = TRUE)] <- paste0(
                        'matrix[n_series, n_series] A;\n',
                        '// latent trend MA autoregressive terms\n',
                        'matrix[n_series, n_series] theta;\n',
                        '// ma process\n',
                        'array[n] vector[n_series] epsilon;\n')

      start <- grep('// raw latent trends', model_file, fixed = TRUE)
      end <- start + 1
      model_file <- model_file[-c(start:end)]

      start <- grep('// trend estimates in matrix-form',
                    model_file, fixed = TRUE)
      end <- grep('Gamma = phiGamma[2, 1];', model_file, fixed = TRUE) + 1
      model_file <- model_file[-c(start:end)]

      model_file[grep('cov_matrix[n_series] Gamma;',
                      model_file, fixed = TRUE)] <- paste0(
                        'cov_matrix[n_series * 2] Omega;\n',
                        '// raw latent trends\n',
                        'vector[n_series] trend_raw[n];\n',
                        '// trend estimates in matrix-form\n',
                        'matrix[n, n_series] trend;')

      model_file[start] <- paste0(
        model_file[start], '\n',
        '// stationary VARMA reparameterisation\n',
        'L_Sigma = diag_pre_multiply(sigma, L_Omega);\n',
        'Sigma = multiply_lower_tri_self_transpose(L_Sigma);\n',

        '{\n',
        '// constrained partial autocorrelations\n',
        'matrix[n_series, n_series] P[1];\n',
        'matrix[n_series, n_series] R[1];\n',
        '// stationary autoregressive coefficients\n',
        'matrix[n_series, n_series] A_init[1];\n',
        'matrix[n_series, n_series] theta_init[1];\n',
        'P[1] = P_realtoP(P_real);\n',
        'R[1] = P_realtoP(R_real);\n',
        '// stationary autoregressive and ma coef matrices\n',
        'A_init = rev_mapping(P, Sigma);\n',
        'theta_init = rev_mapping(R, Sigma);\n',
        'theta_init[1] = -theta_init[1];\n',
        '// initial stationary covariance structure\n',
        'Omega = initial_joint_var(Sigma, A_init, theta_init);\n',
        'A = A_init[1];\n',
        'theta = theta_init[1];\n',
        '}\n',

        '// computed VARMA trends\n',
        'epsilon[1] = theta * init[(n_series + 1) : (n_series * 2)];\n',
        'trend_raw[1] = (A * init[1 : n_series]) + epsilon[1] + error[1];\n',
        'for (i in 2 : n) {\n',
        '// lagged error ma process\n',
        'epsilon[i] = theta * error[i - 1];\n',
        '// full VARMA process\n',
        'trend_raw[i] = (A * trend_raw[i - 1]) + epsilon[i] + error[i];\n',
        '}\n',

        '// computed trends in matrix form\n',
        'for (i in 1 : n) {\n',
        'trend[i, 1 : n_series] = to_row_vector(trend_raw[i]);\n',
        '}')
    }
    model_file <- readLines(textConnection(model_file), n = -1)

    # Update model
    if(any(grepl('cholesky_factor_corr[n_lv] L_Omega;',
                 model_file, fixed = TRUE))){
      start <- grep('// latent state mean parameters',
                    model_file, fixed = TRUE)
      end <- start + 1
      model_file <- model_file[-c(start:end)]

      model_file[grep('// latent state means', model_file, fixed = TRUE)] <-
        paste0('// unconstrained ma inverse partial autocorrelations\n',
               'diagonal(R_real) ~ std_normal();\n',
               'for (i in 1 : n_lv) {\n',
               'for (j in 1 : n_lv) {\n',
               'if (i != j)\n',
               'R_real[i, j] ~ std_normal();\n',
               '}\n',
               '}\n',

               '// initial joint stationary distribution\n',
               'init ~ multi_normal(init_zeros, Omega);\n',

               '// correlated contemporaneous errors\n',
               'for (i in 1 : n) {\n',
               'error[i] ~ multi_normal_cholesky(trend_zeros, L_Sigma);\n',
               '}\n',
               '// latent state means')
      model_file <- readLines(textConnection(model_file), n = -1)

      end <- grep('(LV[i - 1] - trend_mus[ytimes_trend[i - 1, 1:n_lv]]);',
                  model_file, fixed = TRUE) + 1
      start <- grep('// latent state means', model_file, fixed = TRUE)
      model_file <- model_file[-c(start:end)]

      start <- grep('LV[1] ~ multi_normal(trend_mus[ytimes_trend[1, 1:n_lv]], Gamma);',
                    model_file, fixed = TRUE)
      end <- max(grep('L_Sigma);', model_file, fixed = TRUE)) + 1
      model_file <- model_file[-c(start:end)]

    } else {
      start <- grep('// latent trend mean parameters',
                    model_file, fixed = TRUE)
      end <- start + 1
      model_file <- model_file[-c(start:end)]

      model_file[grep('// trend means', model_file, fixed = TRUE)] <-
        paste0('// unconstrained ma inverse partial autocorrelations\n',
               'diagonal(R_real) ~ std_normal();\n',
               'for (i in 1 : n_series) {\n',
               'for (j in 1 : n_series) {\n',
               'if (i != j)\n',
               'R_real[i, j] ~ std_normal();\n',
               '}\n',
               '}\n',

               '// initial joint stationary distribution\n',
               'init ~ multi_normal(init_zeros, Omega);\n',

               '// correlated contemporaneous errors\n',
               'for (i in 1 : n) {\n',
               'error[i] ~ multi_normal_cholesky(trend_zeros, L_Sigma);\n',
               '}\n',
               '// trend means')
      model_file <- readLines(textConnection(model_file), n = -1)

      start <- grep('// trend means',
                    model_file, fixed = TRUE)
      end <- max(grep('trend_raw[i] ~ multi_normal_cholesky(mu[i - 1], L_Sigma);',
                      model_file, fixed = TRUE)) + 1
      model_file <- model_file[-c(start:end)]
    }
    model_file <- readLines(textConnection(model_file), n = -1)
  }

  return(model_file)
}
