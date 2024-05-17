#' Updates for adding N-mixture processes
#' @noRd
add_nmixture = function(model_file,
                        model_data,
                        data_train,
                        data_test = NULL,
                        trend_map = NULL,
                        nmix_trendmap = TRUE,
                        orig_trend_model){

  insight::check_if_installed("extraDistr",
                              reason = 'to simulate from N-Mixture distributions')
  insight::check_if_installed("wrswoR",
                              reason = 'to simulate from N-Mixture distributions')

  if(inherits(orig_trend_model, 'mvgam_trend')){
    orig_trend_model <- orig_trend_model$trend_model
  }

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

  # Update functions block
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

  # Update transformed data block
  if(nmix_trendmap){
  model_file[grep("transformed data {", model_file, fixed = TRUE)] <-
    paste0("transformed data {\n",
           "matrix[total_obs, num_basis] X_ordered = X[ytimes_array,  : ];\n",
           "array[K_groups] int<lower=0> Y_max;\n",
           "array[K_groups] int<lower=0> N_max;\n",
           "for ( k in 1 : K_groups ) {\n",
           "Y_max[k] = max(flat_ys[K_inds[k, K_starts[k] : K_stops[k]]]);\n",
           "N_max[k] = max(cap[K_inds[k, K_starts[k] : K_stops[k]]]);\n",
           "}")
  } else {
    model_file[grep("transformed data {", model_file, fixed = TRUE)] <-
      paste0("transformed data {\n",
             "matrix[total_obs, num_basis] X_ordered = X[ytimes_array,  : ];")
  }
  model_file <- readLines(textConnection(model_file), n = -1)

  # Update the transformed parameters block
  model_file[grep("transformed parameters {", model_file, fixed = TRUE)] <-
    paste0("transformed parameters {\n",
           "// detection probability\n",
           "vector[total_obs] p;\n")

  model_file[grep('// latent process linear predictors',
                  model_file, fixed = TRUE)] <- paste0('// detection probability\n',
                                                       'p = X_ordered * b;\n\n',
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
                         nmix_trendmap = TRUE){
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

    # A second version of K_inds is needed for later generation
    # of properly-constrained latent N predictions; for this version,
    # all observations must be included (no NAs)
    K_inds_all <- dplyr::bind_rows(lapply(seq_len(NCOL(Z)), function(i){
      factor_inds <- which(which_factor == i)
      group_mat <- matrix(NA, nrow = model_data$n,
                          ncol = n_replicates[i])
      for(j in 1:model_data$n){
        group_mat[j, ] <- seq(factor_inds[j],
                              max(factor_inds),
                              by = model_data$n)
      }
      data.frame(group_mat)
    }))

    # Add starting and ending indices for each group to model_data
    model_data$K_starts <- rep(1, NROW(K_inds))
    model_data$K_stops <- length_reps(K_inds)

    # Change any remaining NAs to 1 so they are integers
    K_inds[is.na(K_inds)] <- 1

    # Add remaining group information to the model_data
    model_data$K_reps <- NCOL(K_inds)
    model_data$K_groups <- NROW(K_inds)
    model_data$K_inds <- as.matrix(K_inds)
    model_data$K_inds_all <- as.matrix(K_inds_all)
    model_data$ytimes_pred <- matrix(1:model_data$total_obs,
                                     nrow = model_data$n,
                                     byrow = FALSE)
  }

  return(model_data)
}

add_nmix_genquant = function(model_file,
                             trend_map,
                             nmix_trendmap){

  rho_included <- any(grepl('rho = log(lambda);',
                            model_file, fixed = TRUE))
  rho_trend_included <- any(grepl('rho_trend = log(lambda_trend);',
                            model_file, fixed = TRUE))
  if(any(grepl("penalty = 1.0 / (sigma .* sigma);",
               model_file, fixed = TRUE))){
    penalty_line <- "vector[n_lv] penalty = 1.0 / (sigma .* sigma);"
  } else {
    penalty_line <- "vector[n_lv] penalty = rep_vector(1e12, n_lv);"
  }

  # Delete most generated quantities so that they can be produced after model
  # fitting; this dramatically speeds up model time for nmixture models
  starts <- grep('generated quantities {',
                 model_file, fixed = TRUE)+1
  ends <- max(grep('}',
                   model_file, fixed = TRUE))
  model_file <- model_file[-c(starts:ends)]
  model_file[grep('generated quantities {',
                  model_file, fixed = TRUE)] <-
    paste0('generated quantities {\n',
           penalty_line,
           '\n',
           if(rho_included){
             'vector[n_sp] rho = log(lambda);\n'
           } else {
             NULL
           },
           if(rho_trend_included){
             'vector[n_sp_trend] rho_trend = log(lambda_trend);\n'
           } else {
             NULL
           },'}')
  model_file <- readLines(textConnection(model_file), n = -1)
  return(model_file)
}


add_nmix_model = function(model_file,
                          trend_map,
                          nmix_trendmap){
  if(nmix_trendmap){
    model_file[grep('vector[n_nonmissing] flat_trends;',
                    model_file, fixed = TRUE)] <-
      'array[total_obs] real flat_trends;\narray[total_obs] real flat_ps;'

    model_file[grep('flat_trends = (to_vector(trend))[obs_ind];',
                    model_file, fixed = TRUE)] <-
      'flat_trends = (to_array_1d(trend));\nflat_ps = to_array_1d(p);'
    model_file <- readLines(textConnection(model_file), n = -1)

    model_file[grep('flat_ys ~ poisson_log_glm(append_col(flat_xs, flat_trends),',
                    model_file, fixed = TRUE)] <-
      paste0('// loop over replicate sampling window (each site*time*species combination)\n',
             'for ( k in 1 : K_groups ) {\n',
             '// all log_lambdas are identical because they represent site*time\n',
             '// covariates; so just use the first measurement\n',
             'real log_lambda = flat_trends[K_inds[k, 1]];\n',
             'vector[N_max[k] - Y_max[k] + 1] terms;\n',
             'int l = 0;\n',
             '// marginalize over latent abundance\n',
             'for ( Ni in Y_max[k] : N_max[k] ) {\n',
             'l = l + 1;\n',
             '// factor for poisson prob of latent Ni; compute\n',
             '// only once per sampling window\n',
             'terms[l] = poisson_log_lpmf( Ni | log_lambda ) +\n',
             '// for each replicate observation, binomial prob observed is\n',
             '// computed in a vectorized statement\n',
             'binomial_logit_lpmf( flat_ys[K_inds[k, K_starts[k] : K_stops[k]]] |\n',
             'Ni,\n',
             'flat_ps[K_inds[k, K_starts[k] : K_stops[k]]] );\n',
             '}\n',
             'target += log_sum_exp( terms );\n',
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
    # If trend_map supplied, no modifications needed
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
               'return poisson_log_lpmf(k | lambda) + binomial_logit_lupmf(count | k, p);\n',
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
               'return poisson_log_lpmf(k | lambda) + binomial_logit_lupmf(count | k, p);\n',
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
               '}\n}\n')
    }
  }

  model_file <- readLines(textConnection(model_file), n = -1)
  return(model_file)
}

#' Function to add generated quantities for nmixture models, which
#' saves huge computational time
#' @noRd
add_nmix_posterior = function(model_output,
                              obs_data,
                              test_data,
                              mgcv_model,
                              n_lv,
                              Z,
                              K_inds){

  # Function to add samples to the 'sim' slot of a stanfit object
  add_samples = function(model_output, names, samples, nsamples, nchains,
                         parname){
    samp_starts <- seq(1, NROW(samples), by = nsamples)
    samp_ends <- seq(nsamples, NROW(samples), by = nsamples)
    for(i in 1:nchains){

      samps_df <- data.frame(samples[samp_starts[i]:samp_ends[i],])
      colnames(samps_df) <- names

      if(is.list(model_output@sim$samples[[i]])){
        old <- attributes(model_output@sim$samples[[i]])
        oldnames <- attr(model_output@sim$samples[[i]], 'names')
        model_output@sim$samples[[i]] <-
          append(model_output@sim$samples[[i]], as.list(samps_df))
        mostattributes(model_output@sim$samples[[i]]) <- old
        attr(model_output@sim$samples[[i]], 'names') <-
          c(oldnames, colnames(samps_df))
      } else {
        model_output@sim$samples[[i]] <-
          dplyr::bind_cols(model_output@sim$samples[[i]],
                           samps_df)
      }
    }
    model_output@sim$fnames_oi <- c(model_output@sim$fnames_oi,
                                           names)
    model_output@model_pars <- c(model_output@model_pars,
                                        parname)
    model_output@sim$pars_oi <- c(model_output@sim$pars_oi,
                                         parname)
    return(model_output)
  }

  # Number of chains
  nchains <- model_output@sim$chains

  # Trend samples (for getting dimnames needed for ypred, latent_ypred)
  trend <- mcmc_chains(model_output, 'trend')

  # Construct latent_ypred samples (arranged by time, then series)
  ps <- mcmc_chains(model_output, 'p')
  detprob <- plogis(ps)
  Xp <- matrix(as.vector(ps))
  attr(Xp, 'model.offset') <- 0

  if(!is.null(test_data)){
    cap <- rbind(data.frame(time = obs_data$time,
                      series = obs_data$series,
                      cap = obs_data$cap),
                 data.frame(time = test_data$time,
                            series = test_data$series,
                            cap = test_data$cap))%>%
      dplyr::arrange(series, time) %>%
      dplyr::pull(cap)
  } else {
    cap <- data.frame(time = obs_data$time,
                      series = obs_data$series,
                      cap = obs_data$cap) %>%
      dplyr::arrange(series, time) %>%
      dplyr::pull(cap)
  }
  cap <- as.vector(t(replicate(NROW(ps), cap)))

  # Unconditional latent_N predictions
  if(!is.null(test_data)){
    truth_df <- rbind(data.frame(time = obs_data$time,
                        series = obs_data$series,
                        y = obs_data$y),
                   data.frame(time = test_data$time,
                              series = test_data$series,
                              y = test_data$y))
  } else {
    truth_df <- data.frame(time = obs_data$time,
                        series = obs_data$series,
                        y = obs_data$y)
  }

  get_min_cap = function(truth, K_inds){
    rowgroup = function(x){
      which(K_inds == x, arr.ind = TRUE)[1]
    }

    data.frame(index = 1:length(truth),
               truth = truth) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(group = rowgroup(index)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(min_cap = max(truth, na.rm = TRUE)) %>%
      dplyr::pull(min_cap)
  }

  # K_inds was originally supplied in series, time order
  # so the corresponding truth must be supplied that way
  truth_df %>%
    dplyr::arrange(series, time) %>%
    dplyr::pull(y) -> orig_y
  if(is.null(K_inds)){
    K_inds <- matrix(1:length(orig_y), ncol = 1)
  }
  min_cap <- suppressWarnings(get_min_cap(orig_y, K_inds))
  min_cap[!is.finite(min_cap)] <- 0

  # min_cap is now in the wrong order, so we need to change it
  truth_df %>%
    dplyr::arrange(series, time) %>%
    dplyr::bind_cols(min_cap = min_cap) %>%
    dplyr::arrange(time, series) %>%
    dplyr::pull(min_cap) -> min_cap

  # truth now also needs to be in the correct time, series
  # order
  truth_df %>%
    dplyr::arrange(time, series) %>%
    dplyr::pull(y) -> mod_y
  truth <- as.vector(t(replicate(NROW(ps), mod_y)))
  min_cap <- as.vector(t(replicate(NROW(ps), min_cap)))
  latentypreds_vec <- mvgam_predict(Xp = Xp,
                                    family = 'nmix',
                                    betas = 1,
                                    latent_lambdas = exp(as.vector(trend)),
                                    cap = cap,
                                    min_cap = min_cap,
                                    type = 'latent_N')

  # Conditional latent_N predictions (when observations were not NA)
  whichobs <- which(!is.na(truth))
  Xp <- Xp[whichobs, , drop = FALSE]
  attr(Xp, 'model.offset') <- 0
  condpreds_vec <- mvgam_predict(Xp = Xp,
                                 family = 'nmix',
                                 betas = 1,
                                 latent_lambdas = exp(as.vector(trend)[whichobs]),
                                 cap = cap[whichobs],
                                 min_cap = min_cap[whichobs],
                                 truth = truth[whichobs],
                                 type = 'latent_N')

  # Fill in the unconditionals using the conditionals when there were actually
  # observations
  latentypreds_vec[whichobs] <- condpreds_vec
  latentypreds <- matrix(latentypreds_vec, nrow = NROW(ps))

  # Update parameter names and samples to match expected order
  expand.grid(time = 1:model_output@sim$dims_oi$trend[1],
              series = 1:model_output@sim$dims_oi$trend[2]) %>%
    dplyr::arrange(time, series) %>%
    dplyr::mutate(current = dplyr::row_number()) %>%
    dplyr::arrange(series, time) %>%
    dplyr::mutate(needed = dplyr::row_number()) %>%
    dplyr::mutate(name = paste0('trend[',
                                time,
                                ',',
                                series,
                                ']')) %>%
    dplyr::arrange(current) -> ordering_needed

  parnames <- ordering_needed %>%
    dplyr::arrange(needed) %>%
    dplyr::pull(name)

  indices <- ordering_needed %>%
    dplyr::arrange(needed) %>%
    dplyr::pull(current)

  # Add latent_ypreds to the posterior samples
  model_output <- add_samples(model_output = model_output,
                              names = gsub('trend', 'latent_ypred', parnames),
                              samples = latentypreds[, indices],
                              nsamples = NROW(latentypreds) / nchains,
                              nchains = nchains,
                              parname = 'latent_ypred')
  model_output@sim$dims_oi$latent_ypred <-
    model_output@sim$dims_oi$trend

  # Now construct the detprob samples
  model_output <- add_samples(model_output = model_output,
                        names = gsub('p', 'detprob',
                                     dimnames(ps)[[2]]),
                        samples = detprob,
                        nsamples = NROW(detprob) / nchains,
                        nchains = nchains,
                        parname = 'detprob')
  model_output@sim$dims_oi$detprob <-
    model_output@sim$dims_oi$p

  # Now construct ypred samples
  ypreds_vec <- rbinom(length(latentypreds_vec),
                       size = latentypreds_vec,
                       prob = as.vector(detprob))
  ypreds <- matrix(ypreds_vec, nrow = NROW(ps))
  model_output <- add_samples(model_output = model_output,
                        names = gsub('trend', 'ypred', parnames),
                        samples = ypreds[, indices],
                        nsamples = NROW(ypreds) / nchains,
                        nchains = nchains,
                        parname = 'ypred')
  model_output@sim$dims_oi$ypred <-
    model_output@sim$dims_oi$trend

  # Now construct mus (expectations) samples
  mus_vec <- as.vector(detprob) * latentypreds_vec
  mus <- matrix(mus_vec, nrow = NROW(ps))
  model_output <- add_samples(model_output = model_output,
                        names = gsub('trend', 'mus', parnames),
                        samples = mus[, indices],
                        nsamples = NROW(mus) / nchains,
                        nchains = nchains,
                        parname = 'mus')
  model_output@sim$dims_oi$mus <-
    model_output@sim$dims_oi$trend

  # Now the lv_coefs samples
  n_series <- length(unique(obs_data$series))
  combinations <- expand.grid(1:n_series, 1:n_lv) %>%
    dplyr::arrange(Var2)
  lv_coef_names <- apply(combinations, 1,
                         function(x) paste0('lv_coefs[',
                                            x[1], ',',
                                            x[2], ']'))
  lv_coef_samps <- t(as.matrix(replicate(NROW(ps), as.vector(t(Z)))))
  model_output <- add_samples(model_output = model_output,
                              names = lv_coef_names,
                              samples = lv_coef_samps,
                              nsamples = NROW(lv_coef_samps) / nchains,
                              nchains = nchains,
                              parname = 'lv_coefs')
  model_output@sim$dims_oi$lv_coefs <- c(n_series, n_lv)

  # Update number of total parameters
  model_output@sim$n_flatnames <-
    sum(unlist(lapply(model_output@sim$dims_oi, prod),
               use.names = FALSE))

  return(model_output)
}
