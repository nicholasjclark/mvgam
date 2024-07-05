#' Latent trend model file modifications
#'
#'
#' @noRd
#' @param model_file A template `JAGS` or `Stan` model file to be modified
#' @param stan Logical (convert existing `JAGS` model to a `Stan` model?)
#' @param use_lv Logical (use latent variable trends or not)
#' @param trend_model The type of trend model to be added to the model file
#' @param drift Logical (add drift or not)
#' @return A modified `JAGS` or `Stan` model file
add_trend_lines = function(model_file, stan = FALSE,
                           use_lv, trend_model, drift){

  if(use_lv & trend_model == 'None'){
    trend_model <- 'RW'
  }

  # Add in necessary trend structure
  if(stan){
    if(trend_model == 'None'){
      model_file <- model_file[-c((grep('// raw basis', model_file) + 3):
                                 (grep('// raw basis', model_file) + 7))]
      model_file <- model_file[-c((grep('// priors for latent trend', model_file)):
                                    (grep('// priors for latent trend', model_file)+2))]
      model_file <- model_file[-c((grep('// trend estimates', model_file)):
                                    (grep('// trend estimates', model_file)+3))]
      model_file <- model_file[-c((grepws('trend[2:n', model_file, fixed = T)-1):
                                    (grepws('trend[2:n', model_file, fixed = T)+1))]

      model_file[grepws('y[i, s] ~', model_file, fixed = T)] <-
        'y[i, s] ~ poisson_log(eta[ytimes[i, s]]);'


      model_file[grepws('ypred[i, s] =', model_file, fixed = T)] <-
        "ypred[i, s] = poisson_log_rng(eta[ytimes[i, s]]);"

      model_file <- model_file[-c((grepws('tau[s] = pow(sigma[s], -2.0);',
                                        model_file, fixed = TRUE)-1):
                                    (grepws('tau[s] = pow(sigma[s], -2.0);',
                                          model_file, fixed = TRUE)+1))]
      model_file <- model_file[-grepws('vector[n_series] tau', model_file, fixed = T)]


      model_file <- readLines(textConnection(model_file), n = -1)
    }

    if(trend_model == 'GP'){

      hilbert_approx = T
      if(hilbert_approx){
        if(use_lv){
          model_file <- model_file[-c((grep('// raw basis', model_file) + 3):
                                        (grep('// raw basis', model_file) + 5))]

          model_file <- model_file[-c((grep('// dynamic factor estimates', model_file)):
                                        (grep('// dynamic factor estimates', model_file)+8))]

          model_file[grep('transformed data {', model_file, fixed = TRUE)] <-
            paste0('transformed data {\n',
                   'vector<lower=1>[n] times;\n',
                   'vector[n] times_cent;\n',
                   'real mean_times;\n',
                   'real<lower=0> boundary;\n',
                   'int<lower=1> num_gp_basis;\n',
                   'num_gp_basis = min(20, n);\n',
                   'matrix[n, num_gp_basis] gp_phi;\n\n',
                   'for (t in 1:n){\n',
                   'times[t] = t;\n',
                   '}\n\n',
                   'mean_times = mean(times);\n',
                   'times_cent = times - mean_times;\n',
                   'boundary = (5.0/4) * (max(times_cent) - min(times_cent));\n',
                   'for (m in 1:num_gp_basis){\n',
                   'gp_phi[,m] = phi_SE(boundary, m, times_cent);\n',
                   '}\n')

          model_file[grep('##insert data', model_file)-1] <-
            paste0('functions {\n',
                   '/* Spectral density GP eigenvalues*/\n',
                   '/* see Riutort-Mayol et al 2023 for details (https://doi.org/10.1007/s11222-022-10167-2)*/\n',
                   'real lambda_gp(real L, int m) {\n',
                   'real lam;\n',
                   'lam = ((m*pi())/(2*L))^2;\n',
                   'return lam;\n',
                   '}\n\n',
                   '/* Spectral density GP eigenfunctions*/\n',
                   '/* see Riutort-Mayol et al 2023 for details (https://doi.org/10.1007/s11222-022-10167-2)*/\n',
                   'vector phi_SE(real L, int m, vector x) {\n',
                   'vector[rows(x)] fi;\n',
                   'fi = 1/sqrt(L) * sin(m*pi()/(2*L) * (x+L));\n',
                   'return fi;\n',
                   '}\n\n',
                   '/* Spectral density squared exponential Gaussian Process*/\n',
                   '/* see Riutort-Mayol et al 2023 for details (https://doi.org/10.1007/s11222-022-10167-2)*/\n',
                   'real spd_SE(real alpha, real rho, real w) {\n',
                   'real S;\n',
                   'S = (alpha^2) * sqrt(2*pi()) * rho * exp(-0.5*(rho^2)*(w^2));\n',
                   'return S;\n',
                   '}\n}\n')

          model_file <- readLines(textConnection(model_file), n = -1)

          model_file[grep('// dynamic factor lower triangle', model_file)+3] <-
            paste0('// gp parameters\n',
                   'vector<lower=0>[n_lv] rho_gp;\n\n',
                   '// gp coefficient weights\n',
                   'matrix[num_gp_basis, n_lv] b_gp;\n',
                   '// smoothing parameters\n')

          model_file[grep('vector[total_obs] eta;', model_file, fixed = TRUE) + 1] <-
            paste0('\n// gp spectral densities\n',
                   'matrix[n, n_lv] LV_raw;\n',
                   'matrix[num_gp_basis, n_lv] diag_SPD;\n',
                   'matrix[num_gp_basis, n_lv] SPD_beta;\n')

          model_file[grep('eta = to_vector', model_file) + 1] <-
            paste0('\n// gp LV estimates',
                   '\nfor (m in 1:num_gp_basis){\n',
                   'for (s in 1:n_lv){\n',
                   'diag_SPD[m, s] = sqrt(spd_SE(0.25, rho_gp[s], sqrt(lambda_gp(boundary, m))));\n',
                   '}\n}\n',
                   'SPD_beta = diag_SPD .* b_gp;\n',
                   'LV_raw = gp_phi * SPD_beta;\n}\n')

          rho_line <- 'rho_gp ~ inv_gamma(1.499007, 5.670433);\n'

          model_file[grep('// priors for dynamic factor loading', model_file)-1] <-
            paste0('\n// priors for gp parameters\n',
                   'for (s in 1:n_lv){\n',
                   'b_gp[1:num_gp_basis, s] ~ normal(0, 1);\n',
                   '}\n',
                   rho_line)

          model_file <- readLines(textConnection(model_file), n = -1)

          model_file[grep('vector[n_lv] penalty;', model_file,
                          fixed = TRUE)] <-
            'vector[n_lv] alpha_gp;'
          model_file[grep('penalty = rep_vector(100.0, n_lv);', model_file,
                                            fixed = TRUE)] <-
            'alpha_gp = rep_vector(0.25, n_lv);'

          model_file <- model_file[-c((grep('// derived latent trends', model_file)):
                                        (grep('// derived latent trends', model_file) + 5))]

          model_file[grep('LV_raw = gp_phi * SPD_beta;', model_file, fixed = TRUE)] <-
            paste0('LV_raw = gp_phi * SPD_beta;\n',
                   '// derived latent trends\n',
                   'for (i in 1:n){\n',
                   'for (s in 1:n_series){\n',
                   'trend[i, s] = dot_product(lv_coefs_raw[s,], LV_raw[i,1:n_lv]);\n',
                   '}\n}\n')

          model_file <- readLines(textConnection(model_file), n = -1)

        } else {
          model_file <- model_file[-c((grep('// raw basis', model_file) + 3):
                                        (grep('// raw basis', model_file) + 5))]

          model_file <- model_file[-c((grep('// priors for latent trend', model_file)):
                                        (grep('// priors for latent trend', model_file)+2))]

          model_file <- model_file[-c((grep('// trend estimates', model_file)):
                                        (grep('// trend estimates', model_file)+3))]
          model_file <- model_file[-c((grep('trend[2:n', model_file, fixed = T)-1):
                                        (grep('trend[2:n', model_file, fixed = T)+1))]
          model_file <- model_file[-c((grep('tau[s] = pow(sigma[s], -2.0);',
                                            model_file, fixed = TRUE)-1):
                                        (grep('tau[s] = pow(sigma[s], -2.0);',
                                              model_file, fixed = TRUE)+1))]
          model_file <- model_file[-grep('vector[n_series] tau', model_file, fixed = T)]
          model_file[grep('##insert data', model_file)+1] <-
            paste0('transformed data {\n',
                   'vector<lower=1>[n] times;\n',
                   'vector[n] times_cent;\n',
                   'real mean_times;\n',
                   'real<lower=0> boundary;\n',
                   'int<lower=1> num_gp_basis;\n',
                   'num_gp_basis = min(20, n);\n',
                   'matrix[n, num_gp_basis] gp_phi;\n\n',
                   'for (t in 1:n){\n',
                   'times[t] = t;\n',
                   '}\n\n',
                   'mean_times = mean(times);\n',
                   'times_cent = times - mean_times;\n',
                   'boundary = (5.0/4) * (max(times_cent) - min(times_cent));\n',
                   'for (m in 1:num_gp_basis){\n',
                   'gp_phi[,m] = phi_SE(boundary, m, times_cent);\n',
                   '}\n}\n\n',
                   'parameters {')

          model_file[grep('##insert data', model_file)-1] <-
            paste0('functions {\n',
                   '/* Spectral density GP eigenvalues*/\n',
                   '/* see Riutort-Mayol et al 2023 for details (https://doi.org/10.1007/s11222-022-10167-2)*/\n',
                   'real lambda_gp(real L, int m) {\n',
                   'real lam;\n',
                   'lam = ((m*pi())/(2*L))^2;\n',
                   'return lam;\n',
                   '}\n\n',
                   '/* Spectral density GP eigenfunctions*/\n',
                   '/* see Riutort-Mayol et al 2023 for details (https://doi.org/10.1007/s11222-022-10167-2)*/\n',
                   'vector phi_SE(real L, int m, vector x) {\n',
                   'vector[rows(x)] fi;\n',
                   'fi = 1/sqrt(L) * sin(m*pi()/(2*L) * (x+L));\n',
                   'return fi;\n',
                   '}\n\n',
                   '/* Spectral density squared exponential Gaussian Process*/\n',
                   '/* see Riutort-Mayol et al 2023 for details (https://doi.org/10.1007/s11222-022-10167-2)*/\n',
                   'real spd_SE(real alpha, real rho, real w) {\n',
                   'real S;\n',
                   'S = (alpha^2) * sqrt(2*pi()) * rho * exp(-0.5*(rho^2)*(w^2));\n',
                   'return S;\n',
                   '}\n}\n')

          model_file[grep('// latent trends', model_file)+2] <-
            paste0('// gp parameters\n',
                   'vector<lower=0>[n_series] alpha_gp;\n',
                   'vector<lower=0>[n_series] rho_gp;\n\n',
                   '// gp coefficient weights\n',
                   'matrix[num_gp_basis, n_series] b_gp;\n')

          model_file <- model_file[-c(grep('// latent trends', model_file):
                                        (grep('// latent trends', model_file)+1))]

          model_file[grep('vector[total_obs] eta;', model_file, fixed = TRUE) + 1] <-
            paste0('\n// gp spectral densities\n',
                   'matrix[n, n_series] trend;\n',
                   'matrix[num_gp_basis, n_series] diag_SPD;\n',
                   'matrix[num_gp_basis, n_series] SPD_beta;\n')

          model_file[grep('eta = to_vector', model_file) + 1] <-
            paste0('\n// gp trend estimates',
                   '\nfor (m in 1:num_gp_basis){\n',
                   'for (s in 1:n_series){\n',
                   'diag_SPD[m, s] = sqrt(spd_SE(alpha_gp[s], rho_gp[s], sqrt(lambda_gp(boundary, m))));\n',
                   '}\n}\n',
                   'SPD_beta = diag_SPD .* b_gp;\n',
                   'trend = gp_phi * SPD_beta;\n}\n')

          rho_line <- 'rho_gp ~ inv_gamma(1.499007, 5.670433);\n'
          alpha_line <- 'alpha_gp ~ normal(0, 0.5);\n'

          model_file[grep('// likelihood functions', model_file)-1] <-
            paste0('\n// priors for gp parameters\n',
                   'for (s in 1:n_series){\n',
                   'b_gp[1:num_gp_basis, s] ~ normal(0, 1);\n',
                   '}\n',
                   alpha_line,
                   rho_line)

          model_file <- readLines(textConnection(model_file), n = -1)
        }

        # If not Hilbert approx
      } else {
        model_file <- model_file[-c((grep('// raw basis', model_file) + 3):
                                      (grep('// raw basis', model_file) + 5))]

        model_file <- model_file[-c((grep('// priors for latent trend', model_file)):
                                      (grep('// priors for latent trend', model_file)+2))]

        model_file <- model_file[-c((grep('// trend estimates', model_file)):
                                      (grep('// trend estimates', model_file)+3))]
        model_file <- model_file[-c((grep('trend[2:n', model_file, fixed = T)-1):
                                      (grep('trend[2:n', model_file, fixed = T)+1))]
        model_file <- model_file[-c((grep('tau[s] = pow(sigma[s], -2.0);',
                                          model_file, fixed = TRUE)-1):
                                      (grep('tau[s] = pow(sigma[s], -2.0);',
                                            model_file, fixed = TRUE)+1))]
        model_file <- model_file[-grep('vector[n_series] tau', model_file, fixed = T)]
        model_file[grep('##insert data', model_file)+1] <-
          paste0('transformed data {\n',
                 'real times[n];\n',
                 'for (t in 1:n)\n',
                 'times[t] = t;\n',
                 '}\n\n',
                 'parameters {')

        model_file[grep('// latent trends', model_file)+1] <-
          paste0('vector<lower=0>[n_series] alpha_gp;\n',
                 'vector<lower=0>[n_series] rho_gp;\n',
                 'vector[n] gp_std;\n')

        model_file[grep('// basis coefficients', model_file)+2] <-
          paste0('\n\n// gp estimates\n',
                 'matrix[n, n_series] trend;\n',
                 'for (s in 1:n_series) {\n',
                 '// gp covariance matrices\n',
                 'matrix[n, n] cov;\n',
                 'matrix[n, n] L_cov;\n',
                 'cov = cov_exp_quad(times, alpha_gp[s], rho_gp[s]) + diag_matrix(rep_vector(1e-10, n));\n',
                 'L_cov = cholesky_decompose(cov);\n',
                 '// non-centred parameterisation\n',
                 'trend[1:n, s] = to_vector(L_cov * gp_std);\n',
                 '}\n')

        model_file[grep('// priors for smoothing parameters', model_file)+2] <-
          paste0('\n// priors for gp parameters\n',
                 'to_vector(gp_std) ~ normal(0, 1);\n',
                 'alpha_gp ~ normal(0, 0.5);\n',
                 'rho_gp ~ inv_gamma(1.499007, 5.670433);\n')

        model_file <- readLines(textConnection(model_file), n = -1)
      }
    }

    if(trend_model == 'RW'){
      if(drift){
        if(use_lv){
          model_file[grep('// raw basis', model_file) + 1] <-
            paste0('row_vector[num_basis] b_raw;\n\n// latent factor drift terms\nvector[n_lv] drift;\n')
          model_file[grep('LV_raw[1, j] ~ ', model_file, fixed = T)] <-
            "LV_raw[1, j] ~ normal(0, 0.1);"

          model_file[grep('// dynamic factor estimates', model_file) + 6] <-
            paste0('LV_raw[2:n, j] ~ normal(drift[j]*(n - 1) + LV_raw[1:(n - 1), j], 0.1);')

        } else {
          model_file[grep('// raw basis', model_file) + 1] <-
            paste0('row_vector[num_basis] b_raw;\n\n// latent trend drift terms\nvector[n_series] drift;\n')
          model_file[grep('trend[1, s] ~ ', model_file, fixed = T)] <-
            "trend[1, s] ~ normal(0, sigma[s]);"

          model_file[grep('// trend estimates', model_file) + 6] <-
            paste0('trend[2:n, s] ~ normal(drift[s]*(n - 1) + trend[1:(n - 1), s], sigma[s]);')
        }


          model_file[grep('model \\{', model_file) + 2] <-
            paste0('\n// priors for trend parameters\ndrift ~ normal(0, 0.1);\n')


        model_file <- readLines(textConnection(model_file), n = -1)
      }

    }

    if(trend_model == 'CAR1'){
        if(use_lv){
          model_file[grep('// raw basis', model_file) + 1] <-
            paste0('row_vector[num_basis] b_raw;\n\n// latent factor AR1 terms\nvector<lower=0,upper=1>[n_lv] ar1;')

          model_file[grep('// dynamic factor estimates', model_file) + 6] <-
            paste0('LV_raw[2:n, j] ~ normal(ar1[j] * LV_raw[1:(n - 1), j], 0.1);')

          model_file[grep('model \\{', model_file) + 2] <-
            paste0('\n// priors for AR parameters\nar1 ~ std_normal();\n')

        } else {
          model_file[grep('// raw basis', model_file) + 1] <-
            paste0('row_vector[num_basis] b_raw;\n\n// latent trend AR1 terms\nvector<lower=0,upper=1>[n_series] ar1;')

          model_file[grep('// trend estimates', model_file) + 6] <-
            paste0('trend[2:n, s] ~ normal(ar1[s] * trend[1:(n - 1), s], sigma[s]);')

          model_file[grep('model \\{', model_file) + 2] <-
            paste0('\n// priors for AR parameters\nar1 ~ std_normal();\n')
        }

      model_file <- readLines(textConnection(model_file), n = -1)
    }

    if(trend_model == 'AR1'){
      if(drift){
        if(use_lv){
          model_file[grepws('// raw basis', model_file) + 1] <-
            paste0(c('row_vector[num_basis] b_raw;\n\n// latent factor AR1 terms\nvector<lower=-1,upper=1>[n_lv] ar1;\n\n'),
                   '// latent factor drift terms\nvector[n_lv] drift;')

          model_file[grepws('LV_raw[1, j] ~ ', model_file, fixed = T)] <-
            "LV_raw[1, j] ~ normal(0, 0.1);"

          model_file[grepws('// dynamic factor estimates', model_file) + 6] <-
            paste0('LV_raw[2:n, j] ~ normal(drift[j]*(n - 1) + ar1[j] * LV_raw[1:(n - 1), j], 0.1);')

        } else {
          model_file[grepws('// raw basis', model_file) + 1] <-
            paste0(c('row_vector[num_basis] b_raw;\n\n// latent trend AR1 terms\nvector<lower=-1,upper=1>[n_series] ar1;\n\n'),
                   '// latent trend drift terms\nvector[n_series] drift;')

          model_file[grepws('trend[1, s] ~ ', model_file, fixed = T)] <-
            "trend[1, s] ~ normal(0, sigma[s]);"

          model_file[grep('// trend estimates', model_file) + 6] <-
            paste0('trend[2:n, s] ~ normal(drift[s]*(n - 1) + ar1[s] * trend[1:(n - 1), s], sigma[s]);')
        }


          model_file[grep('model \\{', model_file) + 2] <-
            paste0('\n// priors for AR parameters\nar1 ~ std_normal();\ndrift ~ std_normal();\n')

      } else {
        if(use_lv){
          model_file[grep('// raw basis', model_file) + 1] <-
            paste0('row_vector[num_basis] b_raw;\n\n// latent factor AR1 terms\nvector<lower=-1,upper=1>[n_lv] ar1;')

          model_file[grep('// dynamic factor estimates', model_file) + 6] <-
            paste0('LV_raw[2:n, j] ~ normal(ar1[j] * LV_raw[1:(n - 1), j], 0.1);')

          model_file[grep('model \\{', model_file) + 2] <-
            paste0('\n// priors for AR parameters\nar1 ~ std_normal();\n')

        } else {
          model_file[grep('// raw basis', model_file) + 1] <-
            paste0('row_vector[num_basis] b_raw;\n\n// latent trend AR1 terms\nvector<lower=-1,upper=1>[n_series] ar1;')

          model_file[grep('// trend estimates', model_file) + 6] <-
            paste0('trend[2:n, s] ~ normal(ar1[s] * trend[1:(n - 1), s], sigma[s]);')

          model_file[grep('model \\{', model_file) + 2] <-
            paste0('\n// priors for AR parameters\nar1 ~ std_normal();\n')
        }
      }

      model_file <- readLines(textConnection(model_file), n = -1)
    }

    if(trend_model == 'AR2'){

      if(drift){
        if(use_lv){
          model_file[grepws('// raw basis', model_file) + 1] <-
            paste0('row_vector[num_basis] b_raw;\n\n// latent factor AR1 terms\nvector<lower=-1,upper=1>[n_lv] ar1;\n\n',
                   '// latent factor AR2 terms\nvector<lower=-1,upper=1>[n_lv] ar2;\n\n',
                   '// latent factor drift terms\nvector[n_lv] drift;')

          model_file[grepws('LV_raw[1, j] ~ ', model_file, fixed = T)] <-
            "LV_raw[1, j] ~ normal(0, 0.1);"

          model_file <- model_file[-(grep('// dynamic factor estimates', model_file) + 5:7)]
          model_file[grep('// dynamic factor estimates', model_file) + 5] <-
            paste0('for (j in 1:n_lv) {\n',
                   'LV_raw[2, j] ~ normal(drift[j] + LV_raw[1, j] * ar1[j], 0.1);\n',
                   '}\n\n',
                   'for (i in 3:n) {\n',
                   'for (j in 1:n_lv) {\n',
                   'LV_raw[i, j] ~ normal(drift[j]*(i - 1) + ar1[j] * LV_raw[i - 1, j] + ar2[j] * LV_raw[i - 2, j], 0.1);\n',
                   '}\n}\n')
        } else {
          model_file[grep('// raw basis', model_file) + 1] <-
            paste0('row_vector[num_basis] b_raw;\n\n// latent trend AR1 terms\nvector<lower=-1,upper=1>[n_series] ar1;\n\n',
                   '// latent trend AR2 terms\nvector<lower=-1,upper=1>[n_series] ar2;\n\n',
                   '// latent trend drift terms\nvector[n_series] drift;')

          model_file[grepws('trend[1, s] ~ ', model_file, fixed = T)] <-
            "trend[1, s] ~ normal(0, sigma[s]);"

          model_file <- model_file[-(grep('// trend estimates', model_file) + 5:7)]
          model_file[grep('// trend estimates', model_file) + 5] <-
            paste0('for (s in 1:n_series) {\n',
                   'trend[2, s] ~ normal(drift[s] + trend[1, s] * ar1[s], sigma[s]);\n',
                   '}\n\n',
                   'for (i in 3:n) {\n',
                   'for (s in 1:n_series) {\n',
                   'trend[i, s] ~ normal(drift[s]*(i - 1) + ar1[s] * trend[i - 1, s] + ar2[s] * trend[i - 2, s], sigma[s]);\n',
                   '}\n}\n')
        }


          model_file[grep('model \\{', model_file) + 2] <-
            paste0('\n// priors for AR parameters\nar1 ~ std_normal();\nar2 ~ std_normal();\ndrift ~ std_normal();\n')

      } else {
        if(use_lv){
          model_file[grep('// raw basis', model_file) + 1] <-
            paste0('row_vector[num_basis] b_raw;\n\n// latent factor AR1 terms\nvector<lower=-1,upper=1>[n_lv] ar1;\n\n',
                   '// latent factor AR2 terms\nvector<lower=-1,upper=1>[n_lv] ar2;')
          model_file[grep('// dynamic factor estimates', model_file) + 2] <-
            paste0('LV_raw[1, j] ~ normal(0, 0.1);')

          model_file <- model_file[-(grep('// dynamic factor estimates', model_file) + 5:7)]
          model_file[grep('// dynamic factor estimates', model_file) + 5] <-
            paste0('for (j in 1:n_lv) {\n',
                   'LV_raw[2, j] ~ normal(LV_raw[1, j] * ar1[j], 0.1);\n',
                   '}\n\n',
                   'for (i in 3:n) {\n',
                   'for (j in 1:n_lv) {\n',
                   'LV_raw[i, j] ~ normal(ar1[j] * LV_raw[i - 1, j] + ar2[j] * LV_raw[i - 2, j], 0.1);\n',
                   '}\n}\n')
        } else {
          model_file[grep('// raw basis', model_file) + 1] <-
            paste0('row_vector[num_basis] b_raw;\n\n// latent trend AR1 terms\nvector<lower=-1,upper=1>[n_series] ar1;\n\n',
                   '// latent trend AR2 terms\nvector<lower=-1,upper=1>[n_series] ar2;')
          model_file[grep('// trend estimates', model_file) + 2] <-
            paste0('trend[1, s] ~ normal(0, sigma[s]);')

          model_file <- model_file[-(grep('// trend estimates', model_file) + 5:7)]
          model_file[grep('// trend estimates', model_file) + 5] <-
            paste0('for (s in 1:n_series) {\n',
                   'trend[2, s] ~ normal(trend[1, s] * ar1[s], sigma[s]);\n',
                   '}\n\n',
                   'for (i in 3:n) {\n',
                   'for (s in 1:n_series) {\n',
                   'trend[i, s] ~ normal(ar1[s] * trend[i - 1, s] + ar2[s] * trend[i - 2, s], sigma[s]);\n',
                   '}\n}\n')
        }

        model_file[grep('model \\{', model_file) + 2] <-
          paste0('\n// priors for AR parameters\nar1 ~ std_normal();\nar2 ~ std_normal();\n')
      }

      model_file <- readLines(textConnection(model_file), n = -1)
    }

    if(trend_model == 'AR3'){

      if(drift){
        if(use_lv){
          model_file[grep('// raw basis', model_file) + 1] <-
            paste0('row_vector[num_basis] b_raw;\n\n// latent factor AR1 terms\nvector<lower=-1,upper=1>[n_lv] ar1;\n\n',
                   '// latent factor AR2 terms\nvector<lower=-1,upper=1>[n_lv] ar2;\n\n',
                   '// latent factor AR3 terms\nvector<lower=-1,upper=1>[n_lv] ar3;\n\n',
                   '// latent factor drift terms\nvector[n_lv] drift;')

          model_file[grep('LV_raw[1, s] ~ ', model_file, fixed = T)] <-
            "LV_raw[1, s] ~ normal(0, 0.1);"

          model_file <- model_file[-(grep('// dynamic factor estimates', model_file) + 5:7)]
          model_file[grep('// dynamic factor estimates', model_file) + 5] <-
            paste0('for (j in 1:n_lv) {\n',
                   'LV_raw[2, j] ~ normal(drift[j] + LV_raw[1, j] * ar1[j], 0.1);\n',
                   '}\n\n',

                   'for (j in 1:n_lv) {\n',
                   'LV_raw[3, j] ~ normal(drift[j]*2 + LV_raw[2, j] * ar1[j] + LV_raw[1, j] * ar2[j], 0.1);\n',
                   '}\n\n',

                   'for (i in 4:n) {\n',
                   'for (j in 1:n_lv) {\n',
                   'LV_raw[i, j] ~ normal(drift[j]*(i - 1) + ar1[j] * LV_raw[i - 1, j] + ar2[j] * LV_raw[i - 2, j] + ar3[j] * LV_raw[i - 3, j], 0.1);\n',
                   '}\n}\n')
        } else {
          model_file[grep('// raw basis', model_file) + 1] <-
            paste0('row_vector[num_basis] b_raw;\n\n// latent trend AR1 terms\nvector<lower=-1,upper=1>[n_series] ar1;\n\n',
                   '// latent trend AR2 terms\nvector<lower=-1,upper=1>[n_series] ar2;\n\n',
                   '// latent trend AR3 terms\nvector<lower=-1,upper=1>[n_series] ar3;\n\n',
                   '// latent trend drift terms\nvector[n_series] drift;')

          model_file[grep('trend[1, s] ~ ', model_file, fixed = T)] <-
            "trend[1, s] ~ normal(0, sigma[s]);"

          model_file <- model_file[-(grep('// trend estimates', model_file) + 5:7)]
          model_file[grep('// trend estimates', model_file) + 5] <-
            paste0('for (s in 1:n_series) {\n',
                   'trend[2, s] ~ normal(drift[s] + trend[1, s] * ar1[s], sigma[s]);\n',
                   '}\n\n',

                   'for (s in 1:n_series) {\n',
                   'trend[3, s] ~ normal(drift[s]*2 + trend[2, s] * ar1[s] + trend[1, s] * ar2[s], sigma[s]);\n',
                   '}\n\n',

                   'for (i in 4:n) {\n',
                   'for (s in 1:n_series) {\n',
                   'trend[i, s] ~ normal(drift[s]*(i - 1) + ar1[s] * trend[i - 1, s] + ar2[s] * trend[i - 2, s] + ar3[s] * trend[i - 3, s], sigma[s]);\n',
                   '}\n}\n')
        }

        model_file[grep('model \\{', model_file) + 2] <-
          paste0('\n// priors for AR parameters\nar1 ~ std_normal();\nar2 ~ std_normal();\nar3 ~ std_normal();\n',
                 'drift ~ std_normal();\n')


      } else {
        if(use_lv){
          model_file[grep('// raw basis', model_file) + 1] <-
            paste0('row_vector[num_basis] b_raw;\n\n// latent factor AR1 terms\nvector<lower=-1,upper=1>[n_lv] ar1;\n\n',
                   '// latent factor AR2 terms\nvector<lower=-1,upper=1>[n_lv] ar2;\n\n',
                   '// latent factor AR3 terms\nvector<lower=-1,upper=1>[n_lv] ar3;')
          model_file[grep('// dynamic factor estimates', model_file) + 2] <-
            paste0('LV_raw[1, j] ~ normal(0, 0.1);')

          model_file <- model_file[-(grep('// dynamic factor estimates', model_file) + 5:7)]
          model_file[grep('// dynamic factor estimates', model_file) + 5] <-
            paste0('for (j in 1:n_lv) {\n',
                   'LV_raw[2, j] ~ normal(LV_raw[1, j] * ar1[j], 0.1);\n',
                   '}\n\n',

                   'for (j in 1:n_lv) {\n',
                   'LV_raw[3, j] ~ normal(LV_raw[2, j] * ar1[j] + LV_raw[1, j] * ar2[j], 0.1);\n',
                   '}\n\n',

                   'for (i in 4:n) {\n',
                   'for (j in 1:n_lv) {\n',
                   'LV_raw[i, j] ~ normal(ar1[j] * LV_raw[i - 1, j] + ar2[j] * LV_raw[i - 2, j] + ar3[j] * LV_raw[i - 3, j], 0.1);\n',
                   '}\n}\n')
        } else {
          model_file[grep('// raw basis', model_file) + 1] <-
            paste0('row_vector[num_basis] b_raw;\n\n// latent trend AR1 terms\nvector<lower=-1,upper=1>[n_series] ar1;\n\n',
                   '// latent trend AR2 terms\nvector<lower=-1,upper=1>[n_series] ar2;\n\n',
                   '// latent trend AR3 terms\nvector<lower=-1,upper=1>[n_series] ar3;')
          model_file[grep('// trend estimates', model_file) + 2] <-
            paste0('trend[1, s] ~ normal(0, sigma[s]);')

          model_file <- model_file[-(grep('// trend estimates', model_file) + 5:7)]
          model_file[grep('// trend estimates', model_file) + 5] <-
            paste0('for (s in 1:n_series) {\n',
                   'trend[2, s] ~ normal(trend[1, s] * ar1[s], sigma[s]);\n',
                   '}\n\n',

                   'for (s in 1:n_series) {\n',
                   'trend[3, s] ~ normal(trend[2, s] * ar1[s] + trend[1, s] * ar2[s], sigma[s]);\n',
                   '}\n\n',

                   'for (i in 4:n) {\n',
                   'for (s in 1:n_series) {\n',
                   'trend[i, s] ~ normal(ar1[s] * trend[i - 1, s] + ar2[s] * trend[i - 2, s] + ar3[s] * trend[i - 3, s], sigma[s]);\n',
                   '}\n}\n')
        }

        model_file[grep('model \\{', model_file) + 2] <-
          paste0('\n// priors for AR parameters\nar1 ~ std_normal();\nar2 ~ std_normal();\nar3 ~ std_normal();\n')
      }

      model_file <- readLines(textConnection(model_file), n = -1)
    }

  } else {
    # Modify the JAGS model
    if(trend_model == 'None'){
      model_file[grep('mus\\[i, s\\] <- exp', model_file)] <- 'mus[i, s] <- exp(eta[ytimes[i, s]])'
      model_file <- model_file[-c(grep('## trend estimates', model_file):
                                    (grep('## trend estimates', model_file) + 27))]
    }

    if(use_lv){
      if(trend_model == 'RW'){
        model_file <- model_file[-c((grep('## latent factors evolve', model_file) + 6):
                                      (grep('## latent factors evolve', model_file) + 19))]

        if(drift){
          model_file[grep('## latent factors evolve', model_file) + 5] <-
            '\nfor (i in 2:n) {\nfor (j in 1:n_lv){\nLV_raw[i, j] ~ dnorm(drift[j]*(n - 1) + LV_raw[i - 1, j], penalty[j])\n}\n}\n'
        } else {
          model_file[grep('## latent factors evolve', model_file) + 5] <-
            '\nfor (i in 2:n) {\nfor (j in 1:n_lv){\nLV_raw[i, j] ~ dnorm(LV_raw[i - 1, j], penalty[j])\n}\n}\n'
          model_file <- model_file[-grep('drift\\[s\\] ~', model_file)]
        }

        model_file <- readLines(textConnection(model_file), n = -1)
        model_file <- model_file[-c((grep('## AR components', model_file)):
                                      (grep('## AR components', model_file) + 5))]
      }

      if(trend_model == 'AR1'){
        model_file <- model_file[-c((grep('## latent factors evolve', model_file) + 6):
                                      (grep('## latent factors evolve', model_file) + 12))]
        model_file[grep('## latent factors evolve', model_file) + 7] <-
          'for (i in 2:n) {'
        model_file <- model_file[-c((grep('## latent factors evolve', model_file) + 9):
                                      (grep('## latent factors evolve', model_file) + 10))]

        if(drift){

        } else {
          model_file[grep('## latent factors evolve', model_file) + 9] <-
            'LV_raw[i, j] ~ dnorm(ar1[j]*LV_raw[i - 1, j], penalty[j])\n}'
          model_file <- model_file[-grep('drift\\[s\\] ~', model_file)]
        }

        model_file <- readLines(textConnection(model_file), n = -1)
        model_file <- model_file[-grep('ar2\\[s\\] ~', model_file)]
        model_file <- model_file[-grep('ar3\\[s\\] ~', model_file)]
      }

      if(trend_model == 'AR2'){
        model_file <- model_file[-c((grep('## latent factors evolve', model_file) + 10):
                                      (grep('## latent factors evolve', model_file) + 12))]
        model_file[grep('## latent factors evolve', model_file) + 11] <-
          'for (i in 3:n) {'

        if(drift){
          model_file[grep('## latent factors evolve', model_file) + 14] <-
            'ar2[j]*LV_raw[i - 2, j], penalty[j])'
        } else {
          model_file[grep('## latent factors evolve', model_file) + 7] <-
            'LV_raw[2, j] ~ dnorm(ar1[j]*LV_raw[1, j], penalty[j])'
          model_file[grep('## latent factors evolve', model_file) + 13] <-
            'LV_raw[i, j] ~ dnorm(ar1[j]*LV_raw[i - 1, j] +'
          model_file[grep('## latent factors evolve', model_file) + 14] <-
            'ar2[j]*LV_raw[i - 2, j], penalty[j])'
          model_file <- model_file[-grep('drift\\[s\\] ~', model_file)]
        }
        model_file <- readLines(textConnection(model_file), n = -1)
        model_file <- model_file[-grep('ar3\\[s\\] ~', model_file)]
      }

      if(trend_model == 'AR3'){

        if(drift){

        } else {
          model_file[grep('## latent factors evolve', model_file) + 7] <-
            'LV_raw[2, j] ~ dnorm(ar1[j]*LV_raw[1, j], penalty[j])'
          model_file[grep('## latent factors evolve', model_file) + 11] <-
            'LV_raw[3, j] ~ dnorm(ar1[j]*LV_raw[2, j] + ar2[j]*LV_raw[1, j], penalty[j])'
          model_file[grep('## latent factors evolve', model_file) + 16] <-
            'LV_raw[i, j] ~ dnorm(ar1[j]*LV_raw[i - 1, j] +'
          model_file <- model_file[-grep('drift\\[s\\] ~', model_file)]
        }
        model_file <- readLines(textConnection(model_file), n = -1)
      }
    }

    if(!use_lv){

      if(trend_model == 'RW'){
        model_file <- model_file[-c((grep('## trend estimates', model_file) + 4):
                                      (grep('## trend estimates', model_file) + 17))]

        if(drift){
          model_file[grep('## trend estimates', model_file) + 2] <-
            "trend[1, s] ~ dnorm(drift[s], tau[s])"

          model_file[grep('## trend estimates', model_file) + 4] <-
            '\nfor (i in 2:n) {\nfor (s in 1:n_series){\ntrend[i, s] ~ dnorm(drift[s]*(i - 1) + trend[i - 1, s], tau[s])\n}\n}\n'
        } else {
          model_file[grep('## trend estimates', model_file) + 4] <-
            '\nfor (i in 2:n) {\nfor (s in 1:n_series){\ntrend[i, s] ~ dnorm(trend[i - 1, s], tau[s])\n}\n}\n'
          model_file <- model_file[-grep('drift\\[s\\] ~', model_file)]
        }

        model_file <- readLines(textConnection(model_file), n = -1)
        model_file <- model_file[-grep('ar1\\[s\\] ~', model_file)]
        model_file <- model_file[-grep('ar2\\[s\\] ~', model_file)]
        model_file <- model_file[-grep('ar3\\[s\\] ~', model_file)]
      }

      if(trend_model == 'AR1'){
        model_file <- model_file[-c((grep('## trend estimates', model_file) + 4):
                                      (grep('## trend estimates', model_file) + 17))]

        if(drift){
          model_file[grep('## trend estimates', model_file) + 2] <-
            "trend[1, s] ~ dnorm(drift[s], tau[s])"

          model_file[grep('## trend estimates', model_file) + 4] <-
            '\nfor (i in 2:n) {\nfor (s in 1:n_series){\ntrend[i, s] ~ dnorm(drift[s]*(i - 1) + ar1[s]*trend[i - 1, s], tau[s])\n}\n}\n'
        } else {
          model_file[grep('## trend estimates', model_file) + 4] <-
            '\nfor (i in 2:n) {\nfor (s in 1:n_series){\ntrend[i, s] ~ dnorm(ar1[s]*trend[i - 1, s], tau[s])\n}\n}\n'
          model_file <- model_file[-grep('drift\\[s\\] ~', model_file)]
        }

        model_file <- readLines(textConnection(model_file), n = -1)
        model_file <- model_file[-grep('ar2\\[s\\] ~', model_file)]
        model_file <- model_file[-grep('ar3\\[s\\] ~', model_file)]
      }

      if(trend_model == 'AR2'){
        model_file <- model_file[-c((grep('## trend estimates', model_file) + 9):
                                      (grep('## trend estimates', model_file) + 17))]

        if(drift){
          model_file[grep('## trend estimates', model_file) + 2] <-
            "trend[1, s] ~ dnorm(drift[s], tau[s])"

          model_file[grep('## trend estimates', model_file) + 9] <-
            '\nfor (i in 3:n) {\nfor (s in 1:n_series){\ntrend[i, s] ~ dnorm(drift[s]*(i - 1) + ar1[s]*trend[i - 1, s] + ar2[s]*trend[i - 2, s], tau[s])\n}\n}\n'
        } else {
          model_file[grep('## trend estimates', model_file) + 6] <-
            'trend[2, s] ~ dnorm(ar1[s]*trend[1, s], tau[s])'
          model_file[grep('## trend estimates', model_file) + 9] <-
            '\nfor (i in 3:n) {\nfor (s in 1:n_series){\ntrend[i, s] ~ dnorm(ar1[s]*trend[i - 1, s] + ar2[s]*trend[i - 2, s], tau[s])\n}\n}\n'
          model_file <- model_file[-grep('drift\\[s\\] ~', model_file)]
        }
        model_file <- readLines(textConnection(model_file), n = -1)
        model_file <- model_file[-grep('ar3\\[s\\] ~', model_file)]
      }

      if(trend_model == 'AR3'){

        if(drift){
          model_file[grep('## trend estimates', model_file) + 2] <-
            "trend[1, s] ~ dnorm(drift[s], tau[s])"

        } else {
          model_file[grep('## trend estimates', model_file) + 6] <-
            'trend[2, s] ~ dnorm(ar1[s]*trend[1, s], tau[s])'
          model_file[grep('## trend estimates', model_file) + 10] <-
            'trend[3, s] ~ dnorm(ar1[s]*trend[2, s] + ar2[s]*trend[1, s], tau[s])'
          model_file[grep('## trend estimates', model_file) + 15] <-
            'trend[i, s] ~ dnorm(ar1[s]*trend[i - 1, s] + ar2[s]*trend[i - 2, s] + ar3[s]*trend[i - 3, s], tau[s])'
          model_file <- model_file[-grep('drift\\[s\\] ~', model_file)]
        }
        model_file <- readLines(textConnection(model_file), n = -1)
      }
    }
  }

  return(model_file)
}
