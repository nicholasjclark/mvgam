#' Updates for adding N-mixture processes
#' @noRd
add_nmixture = function(model_file,
                        model_data,
                        data_train,
                        data_test = NULL,
                        orig_trend_model){

  if(inherits(orig_trend_model, 'mvgam_trend')){
    orig_trend_model <- orig_trend_model$trend_model
  }

  #### Perform necessary checks on 'cap' (positive integers, no missing values) ####
  if(!(exists('cap', where = data_train))) {
    stop('Max abundances must be supplied as a variable named "cap" for N-mixture models',
         call. = FALSE)
  }
  cap <- data_train$cap
  if(!is.null(data_test)){
    if(!(exists('cap', where = data_test))) {
      stop('Max abundances must be supplied in test data as a variable named "cap" for N-mixture models',
           call. = FALSE)
    }
    cap <- c(cap, data_test$cap)
  }

  validate_pos_integers(cap)

  if(any(is.na(cap)) | any(is.infinite(cap))){
    stop(paste0('Missing or infinite values found for some "cap" terms'),
         call. = FALSE)
  }

  model_data$cap <- as.vector(cap)

  if(any(model_data$cap < model_data$y)){
    stop(paste0('Some "cap" terms are < the observed counts. This is not allowed'),
         call. = FALSE)
  }

  #### Update the model file appropriately ####
  # If orig_trend_model is 'None', this will be set up as a RW model so need
  # to remove sigma and change the process model lines
  if(orig_trend_model == 'None'){
    # Replace Random Walk trends with no dynamic trend
    start_replace <- grep('LV[1, j] ~ normal(trend_mus[ytimes_trend[1, j]], sigma[j]);',
                          model_file, fixed = TRUE) - 1
    end_replace <- grep('LV[i, j] ~ normal(trend_mus[ytimes_trend[i, j]] + LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]], sigma[j]);',
                        model_file, fixed = TRUE) + 2
    model_file <- model_file[-c(start_replace:end_replace)]
    model_file[grep('trend_mus = X_trend * b_trend;',
                    model_file,
                    fixed = TRUE)] <- paste0('trend_mus = X_trend * b_trend;',
                                             '\n',
                                             'for(j in 1:n_lv){\n',
                                             'LV[1:n, j] = trend_mus[ytimes_trend[1:n, j]];\n',
                                             '}\n')
    model_file <- readLines(textConnection(model_file), n = -1)

    # Remove sigma and penalty parameters
    start_replace <- grep('// latent state SD terms',
                          model_file, fixed = TRUE)
    end_replace <- start_replace + 1
    model_file <- model_file[-c(start_replace:end_replace)]
    model_file <- model_file[-grep('vector[n_lv] penalty;',
                                   model_file, fixed = TRUE)]
    model_file <- model_file[-grep('penalty = 1.0 / (sigma .* sigma);',
                                   model_file, fixed = TRUE)]
    model_file <- model_file[-c(grep('// priors for latent state SD parameters',
                                     model_file, fixed = TRUE),
                                grep('// priors for latent state SD parameters',
                                     model_file, fixed = TRUE) + 1)]

    # LV has to be declared in transformed params, not params
    model_file <- model_file[-c(grep('matrix[n, n_lv] LV;',
                                     model_file, fixed = TRUE) - 1,
                                grep('matrix[n, n_lv] LV;',
                                     model_file, fixed = TRUE))]

    model_file[grep("transformed parameters {", model_file, fixed = TRUE)] <-
      paste0("transformed parameters {\n",
             "// latent states\n",
             "matrix[n, n_lv] LV;\n")
  }

  # Add functions for N-mixtures
  if(any(grepl('functions {', model_file, fixed = TRUE))){
    model_file[grep('functions {', model_file, fixed = TRUE)] <-
      paste0('functions {\n',
             '/* Functions to return the log probability of a Poisson Binomial Mixture */\n',
             '/* see Bollen et al 2023 for details (https://doi.org/10.1002/ece3.10595)*/\n',
             'real poisbin_lpmf(int count, int k, real lambda, real p) {\n',
             'if (count > k) {\n',
             'return negative_infinity();\n',
             '}\n',
             'return poisson_log_lpmf(k | lambda) + binomial_logit_lpmf(count | k, p);\n',
             '}\n',
             'vector pb_logp(int count, int max_k,\n',
             'real lambda, real p) {\n',
             'if (max_k < count)\n',
             'reject("cap variable max_k must be >= observed counts");\n',
             'vector[max_k + 1] lp;\n',
             'for (k in 0:(count - 1))\n',
             'lp[k + 1] = negative_infinity();\n',
             'for (k in count:max_k)\n',
             'lp[k + 1] = poisbin_lpmf(count | k, lambda, p);\n',
             'return lp;\n',
             '}\n',
             'real pb_lpmf(int count, int max_k,\n',
             'real lambda, real p) {\n',
             'vector[max_k + 1] lp;\n',
             'lp = pb_logp(count, max_k, lambda, p);\n',
             'return log_sum_exp(lp);\n',
             '}\n',
             '/* Functions to generate truncated Poisson variates */\n',
             'int nmix_rng(int count, int max_k,\n',
             'real lambda, real p) {\n',
             'vector[max_k + 1] lp;\n',
             'lp = pb_logp(count, max_k, lambda, p);\n',
             'return categorical_rng(softmax(lp)) - 1;\n',
             '}\n',
             'int trunc_pois_rng(int max_k, real lambda){\n',
             'real p_ub = poisson_cdf(max_k, lambda);\n',
             'if(p_ub < 1e-9) return max_k;\n',
             'real u = uniform_rng(0, p_ub);\n',
             'int i = 0;\n',
             'int X = 0;\n',
             'real p = exp(-lambda);',
             'real F = p;\n',
             'while(1){\n',
             'if(u < F){\n',
             'X = i;\n',
             'break;\n',
             '}\n',
             'i = i + 1;\n',
             'p = lambda * p / i;\n',
             'F = F + p;\n',
             '}\n',
             'return X;\n',
             '}')
  } else {
    model_file[grep('Stan model code', model_file)] <-
      paste0('// Stan model code generated by package mvgam\n',
             'functions {\n',
             '/* Functions to return the log probability of a Poisson Binomial Mixture */\n',
             '/* see Bollen et al 2023 for details (https://doi.org/10.1002/ece3.10595)*/\n',
             'real poisbin_lpmf(int count, int k, real lambda, real p) {\n',
             'if (count > k) {\n',
             'return negative_infinity();\n',
             '}\n',
             'return poisson_log_lpmf(k | lambda) + binomial_logit_lpmf(count | k, p);\n',
             '}\n',
             'vector pb_logp(int count, int max_k,\n',
             'real lambda, real p) {\n',
             'if (max_k < count)\n',
             'reject("cap variable max_k must be >= observed counts");\n',
             'vector[max_k + 1] lp;\n',
             'for (k in 0:(count - 1))\n',
             'lp[k + 1] = negative_infinity();\n',
             'for (k in count:max_k)\n',
             'lp[k + 1] = poisbin_lpmf(count | k, lambda, p);\n',
             'return lp;\n',
             '}\n',
             'real pb_lpmf(int count, int max_k,\n',
             'real lambda, real p) {\n',
             'vector[max_k + 1] lp;\n',
             'lp = pb_logp(count, max_k, lambda, p);\n',
             'return log_sum_exp(lp);\n',
             '}\n',
             '/* Functions to generate truncated Poisson variates */\n',
             'int nmix_rng(int count, int max_k,\n',
             'real lambda, real p) {\n',
             'vector[max_k + 1] lp;\n',
             'lp = pb_logp(count, max_k, lambda, p);\n',
             'return categorical_rng(softmax(lp)) - 1;\n',
             '}\n',
             'int trunc_pois_rng(int max_k, real lambda){\n',
             'real p_ub = poisson_cdf(max_k, lambda);\n',
             'if(p_ub < 1e-9) return max_k;\n',
             'real u = uniform_rng(0, p_ub);\n',
             'int i = 0;\n',
             'int X = 0;\n',
             'real p = exp(-lambda);',
             'real F = p;\n',
             'while(1){\n',
             'if(u < F){\n',
             'X = i;\n',
             'break;\n',
             '}\n',
             'i = i + 1;\n',
             'p = lambda * p / i;\n',
             'F = F + p;\n',
             '}\n',
             'return X;\n',
             '}\n}\n')
  }
  model_file <- readLines(textConnection(model_file), n = -1)

  # Update the data block
  model_file[grep('int<lower=0> n_nonmissing; // number of nonmissing observations', model_file, fixed = TRUE)] <-
    paste0("int<lower=0> n_nonmissing; // number of nonmissing observations\n",
           "int<lower=0> cap[total_obs]; // upper limits of latent abundances\n")
  model_file <- readLines(textConnection(model_file), n = -1)

  # Update the transformed parameters block
  model_file[grep("transformed parameters {", model_file, fixed = TRUE)] <-
    paste0("transformed parameters {\n",
           "// detection probability\n",
           "vector[total_obs] p;\n")

  model_file[grep('// latent process linear predictors',
                  model_file, fixed = TRUE)] <- paste0('// detection probability\n',
                                                       'p = X * b;\n\n',
                                                       '// latent process linear predictors')
  model_file <- readLines(textConnection(model_file), n = -1)

  # Update the model block
  model_file[grep('vector[n_nonmissing] flat_trends;',
                  model_file, fixed = TRUE)] <- paste0('vector[n_nonmissing] flat_trends;\n',
                                                       'vector[n_nonmissing] flat_ps;\n',
                                                       'int flat_caps[n_nonmissing];')
  model_file <- readLines(textConnection(model_file), n = -1)

  model_file[grep('flat_trends = (to_vector(trend))[obs_ind];',
                  model_file, fixed = TRUE)] <- paste0('flat_trends = (to_vector(trend))[obs_ind];\n',
                                                       'flat_ps = p[obs_ind];\n',
                                                       'flat_caps = cap[obs_ind];')
  model_file <- readLines(textConnection(model_file), n = -1)

  model_file[grep('flat_ys ~ poisson_log_glm(append_col(flat_xs, flat_trends),',
                  model_file, fixed = TRUE)] <- paste0('for (i in 1:n_nonmissing){\n',
                                                       'target += pb_lpmf(flat_ys[i] | flat_caps[i], flat_trends[i], flat_ps[i]);\n',
                                                       '}')
  model_file <- model_file[-grep('0.0,append_row(b, 1.0));',
                                 model_file, fixed = TRUE)]
  model_file <- readLines(textConnection(model_file), n = -1)

  # Update the generated quantities block
  model_file[grep('array[n, n_series] int ypred;',
                  model_file, fixed = TRUE)] <- paste0('array[n, n_series] int ypred;\n',
                                                       'array[n, n_series] int latent_ypred;\n',
                                                       'array[total_obs] int latent_truncpred;\n',
                                                       'vector[n_nonmissing] flat_ps;\n',
                                                       'int flat_caps[n_nonmissing];',
                                                       'vector[total_obs] flat_trends;\n',
                                                       'vector[n_nonmissing] flat_trends_nonmis;\n',
                                                       'vector[total_obs] detprob;\n',
                                                       'detprob = inv_logit(p);')
  model_file <- readLines(textConnection(model_file), n = -1)

  start_remove <- grep('mus[1:n, s] = eta[ytimes[1:n, s]] + trend[1:n, s];',
                                  model_file, fixed = TRUE) - 1
  end_remove <- grep('ypred[1:n, s] = poisson_log_rng(mus[1:n, s]);',
                     model_file, fixed = TRUE) + 1
  model_file <- model_file[-c(start_remove:end_remove)]

  model_file[grep('eta = X * b;',
                   model_file, fixed = TRUE)] <-
    paste0('eta = X * b;\n',
           '{\n',
           'flat_trends = (to_vector(trend));\n',
           'flat_trends_nonmis = flat_trends[obs_ind];\n',
           'flat_ps = p[obs_ind];\n',
           'flat_caps = cap[obs_ind];\n',
           '// prediction for all timepoints that ignore detection prob\n',
           'for(i in 1:total_obs){\n',
           'latent_truncpred[i] = trunc_pois_rng(cap[i], exp(flat_trends[i]));\n',
           '}\n',
           '// prediction for the nonmissing timepoints using actual obs\n',
           'for(i in 1:n_nonmissing){\n',
           'latent_truncpred[obs_ind[i]] = nmix_rng(flat_ys[i], flat_caps[i], flat_trends_nonmis[i], flat_ps[i]);\n',
           '}\n',
           'for(s in 1:n_series){\n',
           '// true latent abundance\n',
           'latent_ypred[1:n, s] = latent_truncpred[ytimes[1:n, s]];\n',
           '// observed abundance\n',
           'ypred[1:n, s] = binomial_rng(latent_ypred[1:n, s], detprob[ytimes[1:n, s]]);\n',
           '// expected values\n',
           'for(i in 1:n){\n',
           'mus[i, s] = detprob[ytimes[i, s]] * latent_ypred[i, s];\n',
           '}\n}\n}')
  model_file <- readLines(textConnection(model_file), n = -1)

  #### Return ####
  return(list(model_file = model_file,
              model_data = model_data))
}
