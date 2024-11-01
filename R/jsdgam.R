#'Fit Joint Species Distribution Models in \pkg{mvgam}
#'
#'This function sets up a Joint Species Distribution Model whereby the residual associations among
#'species can be modelled in a reduced-rank format using a set of latent factors. The factor
#'specification is extremely flexible, allowing users to include spatial, temporal or any other type
#'of predictor effects to more efficiently capture unmodelled residual associations, while the
#'observation model can also be highly flexible (including all smooth, GP and other effects that
#'\pkg{mvgam} can handle)
#'
#'@inheritParams mvgam
#'@inheritParams ZMVN
#'@param factor_formula A \code{character} string specifying the linear predictor
#'effects for the latent factors. These are exactly like the formula
#'for a GLM except that smooth terms, `s()`, `te()`, `ti()`, `t2()`, as well as time-varying
#'`dynamic()` terms and nonparametric `gp()` terms, can be added to the right hand side
#'to specify that the linear predictor depends on smooth functions of predictors
#'(or linear functionals of these). Details of the formula syntax used by \pkg{mvgam}
#'can be found in \code{\link{mvgam_formulae}}
#'@param factor_knots An optional \code{list} containing user specified knot values to
#' be used for basis construction of any smooth terms in `factor_formula`.
#'For most bases the user simply supplies the knots to be used, which must match up with the `k` value supplied
#'(note that the number of knots is not always just `k`). Different terms can use different numbers of knots,
#'unless they share a covariate
#'@param data A \code{dataframe} or \code{list} containing the model response variable and covariates
#'required by the GAM \code{formula} and \code{factor_formula} objects
#'@param family \code{family} specifying the exponential observation family for the series. Currently supported
#'families are:
#'\itemize{
#'   \item`gaussian()` for real-valued data
#'   \item`betar()` for proportional data on `(0,1)`
#'   \item`lognormal()` for non-negative real-valued data
#'   \item`student_t()` for real-valued data
#'   \item`Gamma()` for non-negative real-valued data
#'   \item`bernoulli()` for binary data
#'   \item`poisson()` for count data
#'   \item`nb()` for overdispersed count data
#'   \item`binomial()` for count data with imperfect detection when the number of trials is known;
#'   note that the `cbind()` function must be used to bind the discrete observations and the discrete number
#'   of trials
#'   \item`beta_binomial()` as for `binomial()` but allows for overdispersion}
#'Default is `poisson()`. See \code{\link{mvgam_families}} for more details
#'@param ... Other arguments to pass to [mvgam()]
#'@author Nicholas J Clark
#'@details Joint Species Distribution Models allow for responses of multiple species to be
#'learned hierarchically, whereby responses to environmental variables in `formula` can be partially
#'pooled and any latent, unmodelled residual associations can also be learned. In \pkg{mvgam}, both of
#'these effects can be modelled with the full power of latent factor Hierarchical GAMs, providing unmatched
#'flexibility to model full communities of species
#'@seealso [mvgam]
#'@return A \code{list} object of classes \code{jsdgam} and \code{mvgam} containing model output,
#'the text representation of the model file,
#'the mgcv model output (for easily generating simulations at
#'unsampled covariate values), Dunn-Smyth residuals for each series and key information needed
#'for other functions in the package. See \code{\link{mvgam-class}} for details.
#'Use `methods(class = "mvgam")` and `methods(class = "jsdgam")` for an overview on available methods
#'@examples
#'\dontrun{
#' # Simulate latent count data for 500 spatial locations and 10 species
#' set.seed(0)
#' N_points <- 500
#' N_species <- 10
#'
#' # Species-level intercepts (on the log scale)
#' alphas <- runif(N_species, 2, 2.25)
#'
#' # Simulate a covariate and species-level responses to it
#' temperature <- rnorm(N_points)
#' betas <- runif(N_species, -0.5, 0.5)
#'
#' # Simulate background spatial points uniformly over a space
#' lon <- runif(N_points, min = 150, max = 155)
#' lat <- runif(N_points, min = -20, max = -19)
#'
#' # Set up spatial basis functions as a tensor product of lat and lon;
#' # keep this number small so there are noticeable spatiotemporal 'clusters'
#' sm <- mgcv::smoothCon(mgcv::te(lon, lat, k = 5),
#'                       data = data.frame(lon, lat),
#'                       knots = NULL)[[1]]
#'
#' # The design matrix for this smooth is in the 'X' slot
#' des_mat <- sm$X
#' dim(des_mat)
#'
#' # Function to generate a random covariance matrix where all variables
#' # have unit variance (i.e. diagonals are all 1)
#' random_Sigma = function(N){
#'   L_Omega <- matrix(0, N, N);
#'   L_Omega[1, 1] <- 1;
#'   for (i in 2 : N) {
#'     bound <- 1;
#'     for (j in 1 : (i - 1)) {
#'       L_Omega[i, j] <- runif(1, -sqrt(bound), sqrt(bound));
#'       bound <- bound - L_Omega[i, j] ^ 2;
#'     }
#'     L_Omega[i, i] <- sqrt(bound);
#'   }
#'   Sigma <- L_Omega %*% t(L_Omega);
#'   return(Sigma)
#' }
#'
#' # Simulate a variance-covariance matrix for the correlations among
#' # basis coefficients
#' Sigma <- random_Sigma(N = NCOL(des_mat))
#'
#' # Now simulate the species-level basis coefficients hierarchically, where
#' # spatial basis function correlations are a convex sum of a base correlation
#' # matrix and a species-level correlation matrix
#' basis_coefs <- matrix(NA, nrow = N_species, ncol = NCOL(Sigma))
#' base_field <- mgcv::rmvn(1, mu = rep(0, NCOL(Sigma)), V = Sigma)
#' for(t in 1:N_species){
#'   corOmega <- (cov2cor(Sigma) * 0.7) +
#'                       (0.3 * cov2cor(random_Sigma(N = NCOL(des_mat))))
#'   basis_coefs[t, ] <- mgcv::rmvn(1, mu = rep(0, NCOL(Sigma)), V = corOmega)
#' }
#'
#' # Simulate the latent spatiotemporal processes
#' st_process <- do.call(rbind, lapply(seq_len(N_species), function(t){
#'   data.frame(lat = lat,
#'              lon = lon,
#'              species = paste0('species_', t),
#'              temperature = temperature,
#'              process = alphas[t] +
#'                betas[t] * temperature +
#'                des_mat %*% basis_coefs[t,])
#' }))
#'
#' # Now take noisy observations at some of the points (60)
#' obs_points <- sample(1:N_points, size = 60, replace = FALSE)
#' obs_points <- data.frame(lat = lat[obs_points],
#'                          lon = lon[obs_points],
#'                          site = 1:60)
#'
#' # Keep only the process data at these points
#' st_process %>%
#'   dplyr::inner_join(obs_points, by = c('lat', 'lon')) %>%
#'   # now take noisy Poisson observations of the process
#'   dplyr::mutate(count = rpois(NROW(.), lambda = exp(process))) %>%
#'   dplyr::mutate(species = factor(species,
#'                                  levels = paste0('species_', 1:N_species))) %>%
#'   dplyr::group_by(lat, lon) -> dat
#'
#' # View the count distributions for each species
#' library(ggplot2)
#' ggplot(dat, aes(x = count)) +
#'   geom_histogram() +
#'   facet_wrap(~ species, scales = 'free')
#'
#' ggplot(dat, aes(x = lat, y = lon, col = log(count + 1))) +
#'   geom_point(size = 2.25) +
#'   facet_wrap(~ species, scales = 'free') +
#'   viridis::scale_color_viridis() +
#'   theme_classic()
#'
#' # Fit a JSDM that estimates a hierarchical temperature responses
#' # and that uses three latent spatial factors
#' mod <- jsdgam(formula = count ~
#'                 # Environmental model includes species-level interecepts
#'                 # and random slopes for a linear effect of temperature
#'                 species +
#'                 s(species, bs = 're', by = temperature),
#'
#'               # Each factor estimates a different nonlinear spatial process, using
#'               # 'by = trend' as in other mvgam State-Space models
#'               factor_formula = ~ te(lat, lon, k = 5, by = trend) - 1,
#'               n_lv = 3,
#'
#'               # The data and the grouping variables
#'               data = dat,
#'               unit = site,
#'               subgr = species,
#'
#'               # Poisson observations
#'               family = poisson())
#'
#' # Plot species-level intercept estimates
#' plot_predictions(mod, condition = 'species',
#'                  type = 'link')
#'
#' # Plot species' hierarchical responses to temperature
#' plot_predictions(mod, condition = c('temperature', 'species', 'species'),
#'                  type = 'link')
#'
#' # Plotting species' average geographical predictions against the observed
#' # data gives some idea of whether more flexibility in the spatial process
#' # may be needed
#' plot_predictions(mod, condition = c('lat', 'species', 'species'),
#'                  points = 0.5)
#' plot_predictions(mod, condition = c('lon', 'species', 'species'),
#'                  points = 0.5)
#'
#' # Calculate (median) residual spatial correlations
#' med_loadings <- matrix(apply(as.matrix(mod$model_output, 'lv_coefs'),
#'                              2, median),
#'                        nrow = N_species, ncol = 3)
#' cormat <- cov2cor(tcrossprod(med_loadings))
#' rownames(cormat) <- colnames(cormat) <- levels(dat$species)
#'
#' # Plot it using corrplot (if installed)
#' if(requireNamespace('corrplot', quietly = TRUE)){
#'   corrplot::corrplot(cormat, type = 'lower', tl.col = 'black',
#'                      tl.srt = 45)
#' }
#'
#' # Posterior predictive checks and ELPD-LOO can ascertain model fit
#' pp_check(mod, type = "ecdf_overlay_grouped",
#'          group = "species", ndraws = 50)
#' loo(mod)
#'}
#'@export
jsdgam = function(formula,
                  factor_formula = ~ -1,
                  knots,
                  factor_knots,
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
  validate_pos_integer(n_lv)

  # Prep the trend so that the data can be structured in the usual
  # mvgam fashion (with 'time' and 'series' variables)
  unit <- deparse0(substitute(unit))
  subgr <- deparse0(substitute(subgr))
  prepped_trend <- prep_jsdgam_trend(unit = unit,
                                     subgr = subgr,
                                     data = data)
  data_train <- validate_series_time(data = data,
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
               trend_knots = factor_knots,
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

  # Add M to data block
  model_file[grep('int<lower=0> n_lv; // number of dynamic factors',
                  model_file, fixed = TRUE)] <- paste0(
                    'int<lower=0> n_lv; // number of dynamic factors\n',
                    'int<lower=0> M; // number of nonzero factor loadings'
                  )
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
  model_file <- sanitise_modelfile(model_file)

  # Remove Z from model_data as it is no longer needed
  model_data <- mod$model_data
  model_data$Z <- NULL

  # Add M to model_data
  n_series <- NCOL(model_data$ytimes)
  model_data$M <- n_lv * (n_series - n_lv) + n_lv * (n_lv - 1) / 2 + n_lv

  #### Autoformat the Stan code ####
  if(requireNamespace('cmdstanr', quietly = TRUE) & backend == 'cmdstanr'){
    if(requireNamespace('cmdstanr') &
       cmdstanr::cmdstan_version() >= "2.29.0") {
      model_file <- .autoformat(model_file,
                                overwrite_file = FALSE,
                                backend = 'cmdstanr',
                                silent = silent >= 1L)
    }
    model_file <- readLines(textConnection(model_file),
                            n = -1)
  } else {
    model_file <- .autoformat(model_file,
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
      cmd_mod <- .model_cmdstanr(model_file,
                                 threads = threads,
                                 silent = silent)

      # Condition the model using Cmdstan
      out_gam_mod <- .sample_model_cmdstanr(model = cmd_mod,
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
      out_gam_mod <- .sample_model_rstan(model = model_file,
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
    mod_residuals <- dsresids_vec(out1)
    rm(out1)

    # Add the posterior median coefficients to the mgcv objects
    ss_gam <- mod$mgcv_model
    V <- cov(mcmc_chains(out_gam_mod, 'b'))
    ss_gam$Vp <- ss_gam$Vc <- V
    p <- mcmc_summary(out_gam_mod, 'b',
                      variational = algorithm %in% c('meanfield',
                                                     'fullrank',
                                                     'pathfinder',
                                                     'laplace'))[,c(4)]
    names(p) <- names(ss_gam$coefficients)
    ss_gam$coefficients <- p

    trend_mgcv_model <- mod$trend_mgcv_model
    V <- cov(mcmc_chains(out_gam_mod, 'b_trend'))
    trend_mgcv_model$Vp <- trend_mgcv_model$Vc <- V
    p <- mcmc_summary(out_gam_mod, 'b_trend',
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

#' Prep trend for jsdgam
#' @noRd
prep_jsdgam_trend = function(data, unit, subgr){

  unit <- as_one_character(unit)
  subgr <- as_one_character(subgr)
  validate_var_exists(data = data,
                              variable = unit,
                              type = 'num/int',
                              name = 'data',
                              trend_char = 'ZMVN')
  validate_var_exists(data = data,
                              variable = subgr,
                              type = 'factor',
                              name = 'data',
                              trend_char = 'ZMVN')
  out <- structure(list(trend_model = 'ZMVN',
                        ma = FALSE,
                        cor = TRUE,
                        unit = unit,
                        gr = "NA",
                        subgr = subgr,
                        label = NULL),
                   class = 'mvgam_trend')
}

#' @noRd
prep_jsdgam_trendmap = function(data, n_lv){
  if(n_lv > nlevels(data$series)){
    stop('Number of factors must be <= number of levels in subgr',
         call. = FALSE)
  }
  data.frame(trend = rep(1:n_lv,
                         nlevels(data$series))[1:nlevels(data$series)],
             series = factor(levels(data$series),
                             levels = levels(data$series)))
}
