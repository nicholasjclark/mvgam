#' Updates for adding N-mixture processes
#' @noRd
add_nmixture = function(model_file,
                        model_data,
                        data_train,
                        data_test = NULL,
                        trend_map = NULL,
                        nmix_trendmap = FALSE,
                        orig_trend_model){

  if(inherits(orig_trend_model, 'mvgam_trend')){
    orig_trend_model <- orig_trend_model$trend_model
  }

  # if(model_data$n == 1L){
  #   nmix_trendmap <- FALSE
  # }

  # Update model data
  model_data <- add_nmix_data(model_data,
                              data_train,
                              data_test,
                              trend_map,
                              nmix_trendmap)

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

    # Remove sigma parameters
    start_replace <- grep('// latent state SD terms',
                          model_file, fixed = TRUE)
    end_replace <- start_replace + 1
    model_file <- model_file[-c(start_replace:end_replace)]
    # model_file <- model_file[-grep('vector[n_lv] penalty;',
    #                                model_file, fixed = TRUE)]
    # model_file <- model_file[-grep('penalty = 1.0 / (sigma .* sigma);',
    #                                model_file, fixed = TRUE)]
    model_file[grep("penalty = 1.0 / (sigma .* sigma);", model_file, fixed = TRUE)] <-
      'penalty = rep_vector(1e12, n_lv);'
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
  model_file <- add_nmix_functions(model_file,
                                   trend_map,
                                   nmix_trendmap)

  # Update the data block
  model_file[grep('int<lower=0> n_nonmissing; // number of nonmissing observations', model_file, fixed = TRUE)] <-
    paste0("int<lower=0> n_nonmissing; // number of nonmissing observations\n",
           "int<lower=0> cap[total_obs]; // upper limits of latent abundances\n",
           'array[total_obs] int ytimes_array; // sorted ytimes\n')
  model_file <- readLines(textConnection(model_file), n = -1)

  if(nmix_trendmap){
    model_file[grep('array[total_obs] int ytimes_array; // sorted ytimes',
                    model_file, fixed = TRUE)] <-
      paste0('array[total_obs] int ytimes_array; // sorted ytimes\n',
             'array[n, n_series] int<lower=0> ytimes_pred; // time-ordered matrix for prediction\n',
             'int<lower=0> K_groups; // number of unique replicated observations\n',
             'int<lower=0> K_reps; // maximum number of replicate observations\n',
             'array[K_groups] int<lower=0> K_starts; // col of K_inds where each group starts\n',
             'array[K_groups] int<lower=0> K_stops; // col of K_inds where each group ends\n',
             'array[K_groups, K_reps] int<lower=0> K_inds; // indices of replicated observations')
    model_file <- readLines(textConnection(model_file), n = -1)

    model_file[grep('int<lower=0> flat_ys[n_nonmissing]; // flattened nonmissing observations',
                    model_file, fixed = TRUE)] <-
      'array[total_obs] int<lower=0> flat_ys; // flattened observations'
    model_file <- model_file[-grep('matrix[n_nonmissing, num_basis] flat_xs; // X values for nonmissing observations',
                                   model_file, fixed = TRUE)]
    model_file <- model_file[-grep('int<lower=0> obs_ind[n_nonmissing]; // indices of nonmissing observations',
                                   model_file, fixed = TRUE)]
  }

  # Update the transformed parameters block
  model_file[grep("transformed parameters {", model_file, fixed = TRUE)] <-
    paste0("transformed parameters {\n",
           "// detection probability\n",
           "vector[total_obs] p;\n")

  model_file[grep('// latent process linear predictors',
                  model_file, fixed = TRUE)] <- paste0('// detection probability\n',
                                                       'p = X[ytimes_array,] * b;\n\n',
                                                       '// latent process linear predictors')
  model_file <- readLines(textConnection(model_file), n = -1)

  # Update the model block
  model_file <- add_nmix_model(model_file,
                               trend_map,
                               nmix_trendmap)

  # Update the generated quantities block
  model_file <- add_nmix_genquant(model_file,
                                  trend_map,
                                  nmix_trendmap)

  #### Return ####
  return(list(model_file = model_file,
              model_data = model_data))
}

add_nmix_data = function(model_data,
                         data_train,
                         data_test,
                         trend_map,
                         nmix_trendmap){
  model_data$ytimes_array <- as.vector(model_data$ytimes)

  #### Perform necessary checks on 'cap' (positive integers, no missing values) ####
  if(!(exists('cap', where = data_train))) {
    stop('Max abundances must be supplied as a variable named "cap" for N-mixture models',
         call. = FALSE)
  }

  if(inherits(data_train, 'data.frame')){
    cap = data_train %>%
      dplyr::arrange(series, time) %>%
      dplyr::pull(cap)
  } else {
    cap = data.frame(series = data_train$series,
                     cap = data_train$cap,
                     time = data_train$time)%>%
      dplyr::arrange(series, time) %>%
      dplyr::pull(cap)
  }

  if(!is.null(data_test)){
    if(!(exists('cap', where = data_test))) {
      stop('Max abundances must be supplied in test data as a variable named "cap" for N-mixture models',
           call. = FALSE)
    }
    if(inherits(data_test, 'data.frame')){
      captest = data_test %>%
        dplyr::arrange(series, time) %>%
        dplyr::pull(cap)
    } else {
      captest = data.frame(series = data_test$series,
                           cap = data_test$cap,
                           time = data_test$time)%>%
        dplyr::arrange(series, time) %>%
        dplyr::pull(cap)
    }
    cap <- c(cap, captest)
  }

  validate_pos_integers(cap)

  if(any(is.na(cap)) | any(is.infinite(cap))){
    stop(paste0('Missing or infinite values found for some "cap" terms'),
         call. = FALSE)
  }

  model_data$cap <- as.vector(cap)

  if(any(model_data$cap[model_data$obs_ind] < model_data$flat_ys)){
    stop(paste0('Some "cap" terms are < the observed counts. This is not allowed'),
         call. = FALSE)
  }

  # Additional data objects for trend_map situations
  if(nmix_trendmap){
    obs_ind <- model_data$obs_ind

    # Don't need to exclude non-missing obs anymore thanks to the grouping
    # indices
    model_data$flat_ys <- as.vector(model_data$y)
    model_data$flat_ys[model_data$flat_ys == -1] <- 0
    ytimes <- model_data$ytimes
    Z <- model_data$Z

    # For all observations, which factor do they belong to?
    which_series <- matrix(NA, nrow = NROW(ytimes),
                           ncol = NCOL(ytimes))
    for(j in 1:NCOL(ytimes)){
      which_series[,j] <- j
    }
    which_series <- as.vector(which_series)

    which_factor <- vector(length = length(ytimes))
    for(i in 1:NCOL(Z)){
      Z_obs <- which(which_series %in% which(Z[,i] == 1))
      which_factor[Z_obs] <- i
    }

    # Replicate group sizes for each factor * time sample
    n_replicates <- colSums(Z)

    shift_nas = function(dat){
      # Shift NAs to the right
      dat_new <- t(apply(dat, 1, function(x){
        c(x[!is.na(x)], x[is.na(x)])}))
      # Delete any rows that are all NA
      dat_new[rowSums(is.na(dat_new)) !=
                ncol(dat_new), , drop=FALSE]
    }

    length_reps = function(dat){
      apply(dat, 1, function(x){
        length(x[!is.na(x)])
      })
    }

    K_inds <- dplyr::bind_rows(lapply(seq_len(NCOL(Z)), function(i){
      factor_inds <- which(which_factor == i)
      group_mat <- matrix(NA, nrow = model_data$n,
                          ncol = n_replicates[i])
      for(j in 1:model_data$n){
        group_mat[j, ] <- seq(factor_inds[j],
                              max(factor_inds),
                              by = model_data$n)
      }
      group_mat[!group_mat %in% obs_ind] <- NA
      data.frame(shift_nas(group_mat))
    }))

    # Add starting and ending indices for each group to model_data
    model_data$K_starts <- rep(1, NROW(K_inds))
    model_data$K_stops <- length_reps(K_inds)

    # Change any remaining NAs to 1 so they are integers
    K_inds[is.na(K_inds)] <- 1

    # Add reamining group information to the model_data
    model_data$K_reps <- NCOL(K_inds)
    model_data$K_groups <- NROW(K_inds)
    model_data$K_inds <- as.matrix(K_inds)
    model_data$ytimes_pred <- matrix(1:model_data$total_obs,
                                     nrow = model_data$n,
                                     byrow = FALSE)
  }

  return(model_data)
}


add_nmix_genquant = function(model_file,
                             trend_map,
                             nmix_trendmap){
  if(nmix_trendmap){
    model_file[grep('array[n, n_series] int ypred;',
                    model_file, fixed = TRUE)] <- paste0('array[n, n_series] int ypred;\n',
                                                         'array[n, n_series] int latent_ypred;\n',
                                                         'array[total_obs] int latent_truncpred;\n',
                                                         'vector[total_obs] flat_trends;\n',
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
             '// prediction for all timepoints that ignore detection prob\n',
             'for(i in 1:total_obs){\n',
             'latent_truncpred[i] = trunc_pois_rng(cap[i], exp(flat_trends[i]));\n',
             '}\n',
             '// prediction for the nonmissing timepoints using actual obs\n',
             'for (k in 1:K_groups) {\n',
             'latent_truncpred[K_inds[k, K_starts[k]:K_stops[k]]] =\n',
             'nmix_rng(flat_ys[K_inds[k, K_starts[k]:K_stops[k]]],\n',
             'cap[K_inds[k, K_starts[k]:K_stops[k]]],\n',
             'to_array_1d(flat_trends[K_inds[k, K_starts[k]:K_stops[k]]]),\n',
             'to_array_1d(p[K_inds[k, K_starts[k]:K_stops[k]]]));\n',
             '}\n',
             'for (s in 1 : n_series) {\n',
             'for (i in 1 : n) {\n',
             '// true latent abundance\n',
             'latent_ypred[i, s] = latent_truncpred[ytimes_pred[i, s]];\n',

             '// observed abundance\n',
             'ypred[i, s] = binomial_rng(latent_ypred[i, s],\n',
             'detprob[ytimes_pred[i, s]]);\n',

             '// expected values\n',
             'mus[i, s] = detprob[ytimes[i, s]] * latent_ypred[i, s];\n',
             '}\n',
             '}\n',
             '}')
    model_file <- readLines(textConnection(model_file), n = -1)
  } else {
    model_file[grep('array[n, n_series] int ypred;',
                    model_file, fixed = TRUE)] <- paste0('array[n, n_series] int ypred;\n',
                                                         'array[n, n_series] int latent_ypred;\n',
                                                         'array[total_obs] int latent_truncpred;\n',
                                                         'vector[n_nonmissing] flat_ps;\n',
                                                         'int flat_caps[n_nonmissing];\n',
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
  }
  return(model_file)
}


add_nmix_model = function(model_file,
                          trend_map,
                          nmix_trendmap){
  if(nmix_trendmap){
    model_file[grep('vector[n_nonmissing] flat_trends;',
                    model_file, fixed = TRUE)] <-
      'vector[total_obs] flat_trends;'

    model_file[grep('flat_trends = (to_vector(trend))[obs_ind];',
                    model_file, fixed = TRUE)] <-
      'flat_trends = (to_vector(trend));'
    model_file <- readLines(textConnection(model_file), n = -1)

    model_file[grep('flat_ys ~ poisson_log_glm(append_col(flat_xs, flat_trends),',
                    model_file, fixed = TRUE)] <- paste0('for (k in 1:K_groups){\n',
                                                         'target += pb_lpmf(flat_ys[K_inds[k, K_starts[k]:K_stops[k]]] |\n',
                                                         'cap[K_inds[k, K_starts[k]:K_stops[k]]],\n',
                                                         'to_array_1d(flat_trends[K_inds[k, K_starts[k]:K_stops[k]]]),\n',
                                                         'to_array_1d(p[K_inds[k, K_starts[k]:K_stops[k]]]));\n',
                                                         '}')
    model_file <- model_file[-grep('0.0,append_row(b, 1.0));',
                                   model_file, fixed = TRUE)]
    model_file <- readLines(textConnection(model_file), n = -1)

  } else {
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
  }

  return(model_file)
}

add_nmix_functions = function(model_file,
                              trend_map,
                              nmix_trendmap){
  if(nmix_trendmap){
    # If trend_map supplied, we need array versions of nmixture functions
    if(any(grepl('functions {', model_file, fixed = TRUE))){
      model_file[grep('functions {', model_file, fixed = TRUE)] <-
        paste0('functions {\n',
               '/* Functions to return the log probability of a Poisson Binomial Mixture */\n',
               '/* see Bollen et al 2023 for details (https://doi.org/10.1002/ece3.10595)*/\n',
               'real poisbin_lpmf(array[] int count, int k, array[] real lambda, array[] real p) {\n',
               'if (max(count) > k) {\n',
               'return negative_infinity();\n',
               '}\n',
               'return poisson_log_lpmf(k | lambda) + binomial_logit_lpmf(count | k, p);\n',
               '}\n',
               'vector pb_logp(array[] int count, int max_k,\n',
               'array[] real lambda, array[] real p) {\n',
               'int c_max = max(count);\n',
               'if (max_k < c_max)\n',
               'reject("cap variable max_k must be >= observed counts");\n',
               'vector[max_k + 1] lp;\n',
               'for (k in 0:(c_max - 1))\n',
               'lp[k + 1] = negative_infinity();\n',
               'for (k in c_max:max_k)\n',
               'lp[k + 1] = poisbin_lpmf(count | k, lambda, p);\n',
               'return lp;\n',
               '}\n',
               'real pb_lpmf(array[] int count, array[] int max_k,\n',
               'array[] real lambda, array[] real p) {\n',
               '// Take maximum of all supplied caps, in case they vary for some reason\n',
               'int max_k_max = max(max_k);\n',
               'vector[max_k_max + 1] lp;\n',
               'lp = pb_logp(count, max_k_max, lambda, p);\n',
               'return log_sum_exp(lp);\n',
               '}\n',
               '/* Functions to generate truncated Poisson variates */\n',
               'array[] int nmix_rng(array[] int count, array[] int max_k,\n',
               'array[] real lambda, array[] real p) {\n',
               '// Take maximum of all supplied caps, in case they vary for some reason\n',
               'int max_k_max = max(max_k);\n',
               'vector[max_k_max + 1] lp;\n',
               'lp = pb_logp(count, max_k_max, lambda, p);\n',
               'return  rep_array(categorical_rng(softmax(lp)) - 1, size(count));\n',
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
               'real poisbin_lpmf(array[] int count, int k, array[] real lambda, array[] real p) {\n',
               'if (max(count) > k) {\n',
               'return negative_infinity();\n',
               '}\n',
               'return poisson_log_lpmf(k | lambda) + binomial_logit_lpmf(count | k, p);\n',
               '}\n',
               'vector pb_logp(array[] int count, int max_k,\n',
               'array[] real lambda, array[] real p) {\n',
               'int c_max = max(count);\n',
               'if (max_k < c_max)\n',
               'reject("cap variable max_k must be >= observed counts");\n',
               'vector[max_k + 1] lp;\n',
               'for (k in 0:(c_max - 1))\n',
               'lp[k + 1] = negative_infinity();\n',
               'for (k in c_max:max_k)\n',
               'lp[k + 1] = poisbin_lpmf(count | k, lambda, p);\n',
               'return lp;\n',
               '}\n',
               'real pb_lpmf(array[] int count, array[] int max_k,\n',
               'array[] real lambda, array[] real p) {\n',
               '// Take maximum of all supplied caps, in case they vary for some reason\n',
               'int max_k_max = max(max_k);\n',
               'vector[max_k_max + 1] lp;\n',
               'lp = pb_logp(count, max_k_max, lambda, p);\n',
               'return log_sum_exp(lp);\n',
               '}\n',
               '/* Functions to generate truncated Poisson variates */\n',
               'array[] int nmix_rng(array[] int count, array[] int max_k,\n',
               'array[] real lambda, array[] real p) {\n',
               '// Take maximum of all supplied caps, in case they vary for some reason\n',
               'int max_k_max = max(max_k);\n',
               'vector[max_k_max + 1] lp;\n',
               'lp = pb_logp(count, max_k_max, lambda, p);\n',
               'return  rep_array(categorical_rng(softmax(lp)) - 1, size(count));\n',
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
  } else {
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
  }

  model_file <- readLines(textConnection(model_file), n = -1)
  return(model_file)
}

