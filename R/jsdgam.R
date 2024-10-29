# Set up a State-Space model using trend_formula
jsdgam = function(formula,
                  factor_formula = ~ -1,
                  knots,
                  trend_knots,
                  data,
                  newdata,
                  family = poisson(),
                  unit = time,
                  subgr = series,
                  share_obs_params = FALSE,
                  priors,
                  n_lv = 2,
                  chains = 4,
                  burnin = 500,
                  samples = 500,
                  thin = 1,
                  parallel = TRUE,
                  threads = 1,
                  silent = 1,
                  max_treedepth = 12,
                  adapt_delta = 0.85,
                  backend = getOption("brms.backend", "cmdstanr"),
                  algorithm = getOption("brms.algorithm", "sampling"),
                  run_model = TRUE,
                  return_model_data = FALSE,
                  ...){

  #### Validate arguments and initialise the model skeleton ####
  mvgam:::validate_pos_integer(n_lv)

  # Prep the trend so that the data can be structured in the usual
  # mvgam fashion (with 'time' and 'series' variables)
  prepped_trend <- prep_jsdgam_trend(unit = site,
                                     subgr = taxon,
                                     data = spiderdat)
  data_train <- mvgam:::validate_series_time(data = spiderdat,
                                             trend_model = prepped_trend)

  # Set up a simple trend_map to get the model dimensions correct;
  # this requires that we only have n_lv trends and that each series
  # only maps to one distinct trend, resulting in a loading matrix of
  # the correct size (n_series x n_lv)
  trend_map <- prep_jsdgam_trendmap(data_train, n_lv)

  # Set up the model structure but leave autoformat off so that the
  # model file can be easily modified
  mod <- mvgam(formula = formula,
               trend_formula = factor_formula,
               knots = knots,
               trend_knots = trend_knots,
               family = family,
               share_obs_params = share_obs_params,
               priors = priors,
               trend_model = 'None',
               trend_map = trend_map,
               data = data_train,
               noncentred = TRUE,
               run_model = FALSE,
               autoformat = FALSE,
               backend = backend,
               ...)
  model_file <- mod$model_file

  #### Modify model data and model file ####
  # Remove Z from supplied data
  model_file <- model_file[-grep("matrix[n_series, n_lv] Z; // matrix mapping series to latent states",
                                 model_file, fixed = TRUE)]

  # Update transformed data
  if(any(grepl('transformed data {', model_file, fixed = TRUE))){
    model_file[grep('transformed data {', model_file, fixed = TRUE)] <-
      paste0('transformed data {\n',
             '// Ensures identifiability of the model - no rotation of factors\n',
             'int<lower=1> M;\n',
             'M = n_lv * (n_series - n_lv) + n_lv * (n_lv - 1) / 2 + n_lv;')
  } else {
    model_file[grep('parameters {', model_file, fixed = TRUE)[1]] <-
      paste0('// Ensures identifiability of the model - no rotation of factors\n',
             'int<lower=1> M;\n',
             'M = n_lv * (n_series - n_lv) + n_lv * (n_lv - 1) / 2 + n_lv;',
             '}\nparameters {')
  }
  model_file <- readLines(textConnection(model_file), n = -1)

  # Update parameters
  model_file <- model_file[-grep("// latent state SD terms",
                                 model_file, fixed = TRUE)]
  model_file <- model_file[-grep("vector<lower=0>[n_lv] sigma;",
                                 model_file, fixed = TRUE)]
  model_file[grep("matrix[n, n_lv] LV_raw;",
                  model_file, fixed = TRUE)] <- paste0(
                    "matrix[n, n_lv] LV_raw;\n\n",
                    "// factor lower triangle loading coefficients\n",
                    "vector[M] L;")
  model_file <- readLines(textConnection(model_file), n = -1)

  # Update transformed parameters
  model_file <- model_file[-grep("// latent states",
                                 model_file, fixed = TRUE)]
  model_file <- model_file[-grep("lv_coefs = Z;",
                                 model_file, fixed = TRUE)]
  model_file <- model_file[-grep("matrix[n, n_lv] LV;",
                                 model_file, fixed = TRUE)]
  model_file <- model_file[-grep("trend_mus = X_trend * b_trend;",
                                 model_file, fixed = TRUE)]
  model_file[grep("matrix[n_series, n_lv] lv_coefs;",
                  model_file, fixed = TRUE)] <- paste0(
                    "matrix[n_series, n_lv] lv_coefs = rep_matrix(0, n_series, n_lv);\n",
                    'matrix[n, n_lv] LV;\n')

  starts <- grep("LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));",
                 model_file, fixed = TRUE)
  ends <- starts + 5
  model_file <- model_file[-(starts:ends)]
  model_file[grep("// latent process linear predictors",
                  model_file, fixed = TRUE)] <- paste0(
                    "// latent process linear predictors\n",
                    "trend_mus = X_trend * b_trend;\n\n",
                    "// constraints allow identifiability of loadings\n",
                    "{\n",
                    "int index;\n",
                    "index = 0;\n",
                    "for (j in 1 : n_lv) {\n",
                    "for (i in j : n_series) {\n",
                    "index = index + 1;\n",
                    "lv_coefs[i, j] = L[index];\n",
                    "}\n",
                    "}\n",
                    "}\n\n",
                    "// raw latent factors (with linear predictors)\n",
                    "for (j in 1 : n_lv) {\n",                                                                                                       "for (i in 1 : n) {\n",
                    "LV[i, j] = trend_mus[ytimes_trend[i, j]] + LV_raw[i, j];\n",
                    "}\n}\n")

  model_file <- model_file[-grep("// derived latent states",
                                 model_file, fixed = TRUE)]
  model_file <- readLines(textConnection(model_file), n = -1)

  # Update model block
  sigma_prior <- grep("// priors for latent state SD parameters",
                      model_file, fixed = TRUE) + 1
  model_file <- model_file[-sigma_prior]
  model_file[grep("// priors for latent state SD parameters",
                  model_file, fixed = TRUE)] <- paste0(
                    "// priors for factor loading coefficients\n",
                    "L ~ std_normal();")
  model_file <- readLines(textConnection(model_file), n = -1)

  # Update generated quantities
  model_file[grep('matrix[n, n_series] mus;',
                  model_file,
                  fixed = TRUE)] <- paste0(
                    'matrix[n, n_series] mus;\n',
                    'vector[n_lv] sigma;')
  model_file[grep("penalty = 1.0 / (sigma .* sigma);",
                  model_file, fixed = TRUE)] <- paste0(
                    "penalty = rep_vector(1.0, n_lv);\n",
                    "sigma = rep_vector(1.0, n_lv);")
  model_file <- readLines(textConnection(model_file), n = -1)
  model_file <- mvgam:::sanitise_modelfile(model_file)

  # Remove Z from model_data as it is no longer needed
  model_data <- mod$model_data
  model_data$Z <- NULL

  #### Autoformat the Stan code ####
  if(requireNamespace('cmdstanr', quietly = TRUE) & backend == 'cmdstanr'){
    if(requireNamespace('cmdstanr') &
       cmdstanr::cmdstan_version() >= "2.29.0") {
      model_file <- mvgam:::.autoformat(model_file,
                                        overwrite_file = FALSE,
                                        backend = 'cmdstanr',
                                        silent = silent >= 1L)
    }
    model_file <- readLines(textConnection(model_file),
                            n = -1)
  } else {
    model_file <- mvgam:::.autoformat(model_file,
                                      overwrite_file = FALSE,
                                      backend = 'rstan',
                                      silent = silent >= 1L)
    model_file <- readLines(textConnection(model_file),
                            n = -1)
  }

  # Remove lp__ from monitor params if VB is to be used
  param <- unique(c(mod$monitor_pars, 'Sigma', 'LV'))
  if(algorithm %in% c('meanfield', 'fullrank', 'pathfinder', 'laplace')){
    param <- param[!param %in% 'lp__']
  }

  #### Determine what to return ####
  if(!run_model){
    mod$model_file <- model_file
    mod$monitor_pars <- param
    attr(model_data, 'trend_model') <- 'None'
    attr(model_data, 'prepped_trend_model') <- prepped_trend
    attr(model_data, 'noncentred') <- NULL
    attr(model_data, 'threads') <- threads
    mod$model_data <- model_data
    out <- mod
  } else {
    # Check if cmdstan is accessible; if not, use rstan
    if(backend == 'cmdstanr'){
      if(!requireNamespace('cmdstanr', quietly = TRUE)){
        warning('cmdstanr library not found. Defaulting to rstan')
        use_cmdstan <- FALSE
      } else {
        use_cmdstan <- TRUE
        if(is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))){
          warning('cmdstanr library found but Cmdstan not found. Defaulting to rstan')
          use_cmdstan <- FALSE
        }
      }
    }

    #### Run the model ####
    if(use_cmdstan){

      # Prepare threading and generate the model
      cmd_mod <- mvgam:::.model_cmdstanr(model_file,
                                         threads = threads,
                                         silent = silent)

      # Condition the model using Cmdstan
      out_gam_mod <- mvgam:::.sample_model_cmdstanr(model = cmd_mod,
                                                    algorithm = algorithm,
                                                    prior_simulation = FALSE,
                                                    data = model_data,
                                                    chains = chains,
                                                    parallel = parallel,
                                                    silent = silent,
                                                    max_treedepth = max_treedepth,
                                                    adapt_delta = adapt_delta,
                                                    threads = threads,
                                                    burnin = burnin,
                                                    samples = samples,
                                                    param = param,
                                                    save_all_pars = FALSE,
                                                    ...)

    } else {
      # Condition the model using rstan
      requireNamespace('rstan', quietly = TRUE)
      out_gam_mod <- mvgam:::.sample_model_rstan(model = model_file,
                                                 algorithm = algorithm,
                                                 prior_simulation = FALSE,
                                                 data = model_data,
                                                 chains = chains,
                                                 parallel = parallel,
                                                 silent = silent,
                                                 max_treedepth = max_treedepth,
                                                 adapt_delta = adapt_delta,
                                                 threads = threads,
                                                 burnin = burnin,
                                                 samples = samples,
                                                 thin = thin,
                                                 ...)
    }

    # After modeling (add a new class to make predictions and other post-processing
    # simpler)
    out1 <- mod
    out1$model_output <- out_gam_mod
    class(out1) <- c('mvgam')
    mod_residuals <- mvgam:::dsresids_vec(out1)
    rm(out1)

    # Add the posterior median coefficients to the mgcv objects
    ss_gam <- mod$mgcv_model
    V <- cov(mvgam:::mcmc_chains(out_gam_mod, 'b'))
    ss_gam$Vp <- ss_gam$Vc <- V
    p <- mvgam:::mcmc_summary(out_gam_mod, 'b',
                              variational = algorithm %in% c('meanfield',
                                                             'fullrank',
                                                             'pathfinder',
                                                             'laplace'))[,c(4)]
    names(p) <- names(ss_gam$coefficients)
    ss_gam$coefficients <- p

    trend_mgcv_model <- mod$trend_mgcv_model
    V <- cov(mvgam:::mcmc_chains(out_gam_mod, 'b_trend'))
    trend_mgcv_model$Vp <- trend_mgcv_model$Vc <- V
    p <- mvgam:::mcmc_summary(out_gam_mod, 'b_trend',
                              variational = algorithm %in% c('meanfield',
                                                             'fullrank',
                                                             'pathfinder',
                                                             'laplace'))[,c(4)]
    names(p) <- names(trend_mgcv_model$coefficients)
    trend_mgcv_model$coefficients <- p

    #### Return the output as class mvgam ####
    trim_data <- list()
    attr(trim_data, 'threads') <- threads
    attr(trim_data, 'noncentred') <- NULL
    attr(trim_data, 'trend_model') <- 'None'
    attr(trim_data, 'prepped_trend_model') <- prepped_trend

    out <- structure(list(call = mod$call,
                          trend_call = factor_formula,
                          family = mod$family,
                          share_obs_params = mod$share_obs_params,
                          trend_model = 'None',
                          trend_map = trend_map,
                          drift = FALSE,
                          priors = mod$priors,
                          model_output = out_gam_mod,
                          model_file = model_file,
                          model_data = if(return_model_data){
                            model_data
                          } else {
                            trim_data
                          },
                          inits = NULL,
                          monitor_pars = param,
                          sp_names = mod$sp_names,
                          trend_sp_names = mod$trend_sp_names,
                          mgcv_model = ss_gam,
                          trend_mgcv_model = trend_mgcv_model,
                          ytimes = mod$ytimes,
                          resids = mod_residuals,
                          use_lv = TRUE,
                          n_lv = n_lv,
                          upper_bounds = mod$upper_bounds,
                          obs_data = mod$obs_data,
                          test_data = mod$test_data,
                          fit_engine = 'stan',
                          backend = backend,
                          algorithm = algorithm,
                          max_treedepth = max_treedepth,
                          adapt_delta = adapt_delta),
                     class = c('mvgam', 'jsdgam'))
  }

  return(out)
}
