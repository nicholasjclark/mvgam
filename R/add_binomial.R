#' @noRd
add_binomial = function(formula,
                        model_file,
                        model_data,
                        data_train,
                        data_test,
                        family_char){


  # Add trial information if necessary
  if(family_char %in% c('binomial', 'beta_binomial')){
    # Identify which variable in data represents the number of trials
    resp_terms <- as.character(terms(formula(formula))[[2]])
    resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
    trial_name <- resp_terms[2]

    # Pull the trials variable from the data and validate
    train_trials <- data_train[[trial_name]]

    if(any(is.na(train_trials))){
      stop(paste0('variable ', trial_name, ' contains missing values'),
           call. = FALSE)
    }

    if(any(is.infinite(train_trials))){
      stop(paste0('variable ', trial_name, ' contains infinite values'),
           call. = FALSE)
    }

    # Matrix of trials per series
    all_trials <- data.frame(series = as.numeric(data_train$series),
                             time = data_train$time,
                             trials = data_train[[trial_name]]) %>%
      dplyr::arrange(time, series)

    # Same for data_test
    if(!is.null(data_test)){
      if(!(exists(trial_name, where = data_test))) {
        stop('Number of trials must also be supplied in "newdata" for Binomial models',
             call. = FALSE)
      }

      all_trials <- rbind(all_trials,
                          data.frame(series = as.numeric(data_test$series),
                                     time = data_test$time,
                                     trials = data_test[[trial_name]])) %>%
        dplyr::arrange(time, series)

      if(any(is.na(all_trials$trial)) | any(is.infinite(all_trials$trial))){
        stop(paste0('Missing or infinite values found in ', trial_name, ' variable'),
             call. = FALSE)
      }
    }

    # Construct matrix of N-trials in the correct format so it can be
    # flattened into one long vector
    trials <- matrix(NA, nrow = length(unique(all_trials$time)),
                     ncol = length(unique(all_trials$series)))
    for(i in 1:length(unique(all_trials$series))){
      trials[,i] <- all_trials$trials[which(all_trials$series == i)]
    }

    # Add trial info to the model data
    model_data$flat_trials <- as.vector(trials)
    model_data$flat_trials_train <- as.vector(trials)[which(as.vector(model_data$y_observed) == 1)]

    # Add trial vectors to model block
    model_file[grep("int<lower=0> flat_ys[n_nonmissing]; // flattened nonmissing observations",
         model_file, fixed = TRUE)] <-
      paste0("array[n_nonmissing] int<lower=0> flat_ys; // flattened nonmissing observations\n",
             "array[total_obs] int<lower=0> flat_trials; // flattened trial vector\n",
             "array[n_nonmissing] int<lower=0> flat_trials_train; // flattened nonmissing trial vector\n")
    model_file <- readLines(textConnection(model_file), n = -1)
  } else {
    trials <- NULL
  }

  # Update model block
  if(family_char == 'binomial'){
    if(any(grepl("flat_ys ~ poisson_log_glm(flat_xs,",
                model_file, fixed = TRUE))){
      model_file[grep("flat_ys ~ poisson_log_glm(flat_xs,",
                      model_file, fixed = TRUE)] <-
        "flat_ys ~ binomial_logit_glm(flat_trials_train, flat_xs,"
    }

    if(any(grepl("flat_ys ~ poisson_log_glm(append_col(flat_xs, flat_trends),",
                 model_file, fixed = TRUE))){
      model_file[grep("flat_ys ~ poisson_log_glm(append_col(flat_xs, flat_trends),",
                      model_file, fixed = TRUE)] <-
        "flat_ys ~ binomial_logit_glm(flat_trials_train, append_col(flat_xs, flat_trends),"
    }
  }

  if(family_char == 'bernoulli'){
    if(any(grepl("flat_ys ~ poisson_log_glm(flat_xs,",
                 model_file, fixed = TRUE))){
      model_file[grep("flat_ys ~ poisson_log_glm(flat_xs,",
                      model_file, fixed = TRUE)] <-
        "flat_ys ~ bernoulli_logit_glm(flat_xs,"
    }

    if(any(grepl("flat_ys ~ poisson_log_glm(append_col(flat_xs, flat_trends),",
                 model_file, fixed = TRUE))){
      model_file[grep("flat_ys ~ poisson_log_glm(append_col(flat_xs, flat_trends),",
                      model_file, fixed = TRUE)] <-
        "flat_ys ~ bernoulli_logit_glm(append_col(flat_xs, flat_trends),"
    }

  }

  # Update the generated quantities block
  if(family_char == 'binomial'){
    model_file[grep("ypred[1:n, s] = poisson_log_rng(mus[1:n, s]);" ,
                    model_file, fixed = TRUE)] <-
      "ypred[1:n, s] = binomial_rng(flat_trials[ytimes[1:n, s]], inv_logit(mus[1:n, s]));"
  }

  if(family_char == 'bernoulli'){
    model_file[grep("ypred[1:n, s] = poisson_log_rng(mus[1:n, s]);" ,
                    model_file, fixed = TRUE)] <-
      "ypred[1:n, s] = bernoulli_logit_rng(mus[1:n, s]);"
  }

  #### Return ####
  return(list(model_file = model_file,
              model_data = model_data,
              trials = trials))

}
