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
#'@param formula A \code{formula} object specifying the GAM observation model formula. These are exactly like the formula
#'for a GLM except that smooth terms, `s()`, `te()`, `ti()`, `t2()`, as well as time-varying
#'`dynamic()` terms, nonparametric `gp()` terms and offsets using `offset()`, can be added to the right hand side
#'to specify that the linear predictor depends on smooth functions of predictors
#'(or linear functionals of these). Details of the formula syntax used by \pkg{mvgam}
#'can be found in \code{\link{mvgam_formulae}}
#'@param factor_formula A \code{formula} object specifying the linear predictor
#'effects for the latent factors. Use `by = trend` within calls to functional terms
#'(i.e. `s()`, `te()`, `ti()`, `t2()`, `dynamic()`, or `gp()`) to ensure that each factor
#'captures a different axis of variation. See the example below as an illustration
#'@param factor_knots An optional \code{list} containing user specified knot values to
#' be used for basis construction of any smooth terms in `factor_formula`.
#'For most bases the user simply supplies the knots to be used, which must match up with the `k` value supplied
#'(note that the number of knots is not always just `k`). Different terms can use different numbers of knots,
#'unless they share a covariate
#'@param data A \code{dataframe} or \code{list} containing the model response variable and covariates
#'required by the GAM \code{formula} and \code{factor_formula} objects
#'@param family \code{family} specifying the observation family for the outcomes. Currently supported
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
#' @param species The unquoted name of the `factor` variable that indexes
#' the different response units in `data` (usually `'species'` in a JSDM).
#' Defaults to `series` to be consistent with other `mvgam` models
#'@param n_lv \code{integer} the number of latent factors to use for modelling
#'residual associations.
#'Cannot be `> n_species`. Defaults arbitrarily to `2`
#'@param threads \code{integer} Experimental option to use multithreading for within-chain
#'parallelisation in \code{Stan}. We recommend its use only if you are experienced with
#'\code{Stan}'s `reduce_sum` function and have a slow running model that cannot be sped
#'up by any other means. Currently works for all families when using \code{Cmdstan}
#'as the backend
#'@param priors An optional \code{data.frame} with prior
#'definitions (in Stan syntax) or, preferentially, a vector containing
#' objects of class `brmsprior` (see. \code{\link[brms]{prior}} for details).
#' See [get_mvgam_priors] and for more information on changing default prior distributions
#'@param ... Other arguments to pass to [mvgam]
#'@author Nicholas J Clark
#'@details Joint Species Distribution Models allow for responses of multiple species to be
#'learned hierarchically, whereby responses to environmental variables in `formula` can be partially
#'pooled and any latent, unmodelled residual associations can also be learned. In \pkg{mvgam}, both of
#'these effects can be modelled with the full power of latent factor Hierarchical GAMs, providing unmatched
#'flexibility to model full communities of species. When calling [jsdgam], an initial State-Space model using
#'`trend = 'None'` is set up and then modified to include the latent factors and their linear predictors.
#'Consequently, you can inspect priors for these models using [get_mvgam_priors] by supplying the relevant
#'`formula`, `factor_formula`, `data` and `family` arguments and keeping the default `trend = 'None'`.
#'
#' In a JSDGAM, the expectation of response \eqn{Y_{ij}} is modelled with
#'
#' \deqn{g(\mu_{ij}) = X_i\beta + u_i\theta_j,}
#'
#' where \eqn{g(.)} is a known link function,
#' \eqn{X} is a design matrix of linear predictors (with associated \eqn{\beta} coefficients),
#' \eqn{u} are \eqn{n_{lv}}-variate latent factors
#' (\eqn{n_{lv}}<<\eqn{n_{species}}) and
#' \eqn{\theta_j} are species-specific loadings on the latent factors, respectively. The design matrix
#' \eqn{X} and \eqn{\beta} coefficients are constructed and modelled using `formula` and can contain
#' any of `mvgam`'s predictor effects, including random intercepts and slopes, multidimensional penalized
#' smooths, GP effects etc... The factor loadings \eqn{\theta_j} are constrained for identifiability but can
#' be used to reconstruct an estimate of the species' residual variance-covariance matrix
#' using \eqn{\Theta \Theta'} (see the example below and [residual_cor()] for details).
#' The latent factors are further modelled using:
#'\deqn{
#'u_i \sim \text{Normal}(Q_i\beta_{factor}, 1) \quad
#'}
#'where the second design matrix \eqn{Q} and associated \eqn{\beta_{factor}} coefficients are
#'constructed and modelled using `factor_formula`. Again, the effects that make up this linear
#'predictor can contain any of `mvgam`'s allowed predictor effects, providing enormous flexibility for
#'modelling species' communities.
#'@seealso [mvgam()], [residual_cor()]
#'@references Nicholas J Clark & Konstans Wells (2020). Dynamic generalised additive models (DGAMs) for forecasting discrete ecological time series.
#'Methods in Ecology and Evolution. 14:3, 771-784.
#' \cr
#' \cr
#'David I Warton, F Guillaume Blanchet, Robert B O’Hara, Otso Ovaskainen, Sara Taskinen, Steven C
#'Walker & Francis KC Hui (2015). So many variables: joint modeling in community ecology.
#'Trends in Ecology & Evolution 30:12, 766-779.
#'@return A \code{list} object of class \code{mvgam} containing model output,
#'the text representation of the model file,
#'the mgcv model output (for easily generating simulations at
#'unsampled covariate values), Dunn-Smyth residuals for each species and key information needed
#'for other functions in the package. See \code{\link{mvgam-class}} for details.
#'Use `methods(class = "mvgam")` for an overview on available methods
#'@examples
#'\donttest{
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
#' # Simulate points uniformly over a space
#' lon <- runif(N_points, min = 150, max = 155)
#' lat <- runif(N_points, min = -20, max = -19)
#'
#' # Set up spatial basis functions as a tensor product of lat and lon
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
#'                  (0.3 * cov2cor(random_Sigma(N = NCOL(des_mat))))
#'   basis_coefs[t, ] <- mgcv::rmvn(1, mu = rep(0, NCOL(Sigma)), V = corOmega)
#' }
#'
#' # Simulate the latent spatial processes
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
#' ggplot(dat, aes(x = lon, y = lat, col = log(count + 1))) +
#'   geom_point(size = 2.25) +
#'   facet_wrap(~ species, scales = 'free') +
#'   scale_color_viridis_c() +
#'   theme_classic()
#'
#' # Inspect default priors for a joint species model with three spatial factors
#' priors <- get_mvgam_priors(formula = count ~
#'                             # Environmental model includes random slopes for
#'                             # a linear effect of temperature
#'                             s(species, bs = 're', by = temperature),
#'
#'                           # Each factor estimates a different nonlinear spatial process, using
#'                           # 'by = trend' as in other mvgam State-Space models
#'                           factor_formula = ~ gp(lon, lat, k = 6, by = trend) - 1,
#'                           n_lv = 3,
#'
#'                           # The data and grouping variables
#'                           data = dat,
#'                           unit = site,
#'                           species = species,
#'
#'                           # Poisson observations
#'                           family = poisson())
#' head(priors)
#'
#' # Fit a JSDM that estimates hierarchical temperature responses
#' # and that uses three latent spatial factors
#' mod <- jsdgam(formula = count ~
#'                 # Environmental model includes random slopes for a
#'                 # linear effect of temperature
#'                 s(species, bs = 're', by = temperature),
#'
#'               # Each factor estimates a different nonlinear spatial process, using
#'               # 'by = trend' as in other mvgam State-Space models
#'               factor_formula = ~ gp(lon, lat, k = 6, by = trend) - 1,
#'               n_lv = 3,
#'
#'               # Change default priors for fixed random effect variances and
#'               # factor P marginal deviations to standard normal
#'               priors = c(prior(std_normal(),
#'                                class = sigma_raw),
#'                          prior(std_normal(),
#'                                class = `alpha_gp_trend(lon, lat):trendtrend1`),
#'                          prior(std_normal(),
#'                                class = `alpha_gp_trend(lon, lat):trendtrend2`),
#'                          prior(std_normal(),
#'                                class = `alpha_gp_trend(lon, lat):trendtrend3`)),
#'
#'               # The data and the grouping variables
#'               data = dat,
#'               unit = site,
#'               species = species,
#'
#'               # Poisson observations
#'               family = poisson(),
#'               chains = 2,
#'               silent = 2)
#'
#' # Plot species-level intercept estimates
#' plot_predictions(mod, condition = 'species',
#'                  type = 'link')
#'
#' # Plot species' hierarchical responses to temperature
#' plot_predictions(mod, condition = c('temperature', 'species', 'species'),
#'                  type = 'link')
#'
#' # Plot posterior median estimates of the latent spatial factors
#' plot(mod, type = 'smooths', trend_effects = TRUE)
#'
#' # Or using gratia, if you have it installed
#' if(requireNamespace('gratia', quietly = TRUE)){
#'   gratia::draw(mod, trend_effects = TRUE)
#' }
#'
#' # Plot species' randomized quantile residual distributions
#' pp_check(mod, type = 'resid_ribbon_grouped', group = 'species')
#'
#' # Calculate residual spatial correlations
#' post_cors <- residual_cor(mod)
#' names(post_cors)

#' # Look at lower and upper credible interval estimates for
#' # some of the estimated correlations
#' post_cors$cor[1:5, 1:5]
#' post_cors$cor_upper[1:5, 1:5]
#' post_cors$cor_lower[1:5, 1:5]

#' # A quick and dirty plot of the posterior median correlations
#' image(post_cors$cor)
#'
#' # Posterior predictive checks and ELPD-LOO can ascertain model fit
#' pp_check(mod, type = "pit_ecdf_grouped",
#'          group = "species", ndraws = 100)
#' loo(mod)
#'
#' # Forecast log(counts) for entire region (site value doesn't matter as long
#' # as each spatial location has a different and unique site identifier);
#' # note this calculation takes a few minutes because of the need to calculate
#' # draws from the stochastic latent factors
#' newdata <- st_process %>%
#'                    dplyr::mutate(species = factor(species,
#'                                                   levels = paste0('species_',
#'                                                                   1:N_species))) %>%
#'                    dplyr::group_by(lat, lon) %>%
#'                    dplyr::mutate(site = dplyr::cur_group_id()) %>%
#'                    dplyr::ungroup()
#' preds <- predict(mod, newdata = newdata)
#'
#' # Plot the median log(count) predictions on a grid
#' newdata$log_count <- preds[,1]
#' ggplot(newdata, aes(x = lon, y = lat, col = log_count)) +
#'   geom_point(size = 1.5) +
#'   facet_wrap(~ species, scales = 'free') +
#'   scale_color_viridis_c() +
#'   theme_classic()
#'}
#'@export
jsdgam = function(
  formula,
  factor_formula = ~ -1,
  knots,
  factor_knots,
  data,
  newdata,
  family = poisson(),
  unit = time,
  species = series,
  share_obs_params = FALSE,
  priors,
  n_lv = 2,
  backend = getOption("brms.backend", "cmdstanr"),
  algorithm = getOption("brms.algorithm", "sampling"),
  control = list(max_treedepth = 10, adapt_delta = 0.8),
  chains = 4,
  burnin = 500,
  samples = 500,
  thin = 1,
  parallel = TRUE,
  threads = 1,
  silent = 1,
  run_model = TRUE,
  return_model_data = FALSE,
  residuals = TRUE,
  ...
) {
  #### Validate arguments and initialise the model skeleton ####
  validate_pos_integer(n_lv)

  # Prep the trend so that the data can be structured in the usual
  # mvgam fashion (with 'time' and 'series' variables)
  unit <- deparse0(substitute(unit))
  subgr <- deparse0(substitute(species))
  prepped_trend <- prep_jsdgam_trend(unit = unit, subgr = subgr, data = data)
  data_train <- validate_series_time(data = data, trend_model = prepped_trend)

  # Set up a simple trend_map to get the model dimensions correct;
  # this requires that we only have n_lv trends and that each series
  # only maps to one distinct trend, resulting in a loading matrix of
  # the correct size (n_series x n_lv)
  trend_map <- prep_jsdgam_trendmap(data_train, n_lv)

  # Set up the model structure but leave autoformat off so that the
  # model file can be easily modified
  mod <- mvgam(
    formula = formula,
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
    ...
  )
  model_file <- mod$model_file

  #### Modify model data and model file ####
  # Remove Z from supplied data
  model_file <- model_file[
    -grep(
      "matrix[n_series, n_lv] Z; // matrix mapping series to latent states",
      model_file,
      fixed = TRUE
    )
  ]

  # Add M to data block
  model_file[grep(
    'int<lower=0> n_lv; // number of dynamic factors',
    model_file,
    fixed = TRUE
  )] <- paste0(
    'int<lower=0> n_lv; // number of dynamic factors\n',
    'int<lower=0> M; // number of nonzero lower-triangular factor loadings'
  )
  model_file <- readLines(textConnection(model_file), n = -1)

  # Update parameters
  model_file <- model_file[
    -grep("// latent state SD terms", model_file, fixed = TRUE)
  ]
  model_file <- model_file[
    -grep("vector<lower=0>[n_lv] sigma;", model_file, fixed = TRUE)
  ]
  model_file[grep(
    "matrix[n, n_lv] LV_raw;",
    model_file,
    fixed = TRUE
  )] <- paste0(
    "matrix[n, n_lv] LV_raw;\n\n",
    "// factor lower triangle loadings\n",
    "vector[M] L_lower;\n",
    "// factor diagonal loadings\n",
    "vector<lower=0>[n_lv] L_diag;"
  )
  model_file <- readLines(textConnection(model_file), n = -1)

  # Update transformed parameters
  model_file <- model_file[-grep("// latent states", model_file, fixed = TRUE)]
  model_file <- model_file[-grep("lv_coefs = Z;", model_file, fixed = TRUE)]
  model_file <- model_file[
    -grep("matrix[n, n_lv] LV;", model_file, fixed = TRUE)
  ]
  model_file <- model_file[
    -grep("trend_mus = X_trend * b_trend;", model_file, fixed = TRUE)
  ]
  model_file[grep(
    "matrix[n_series, n_lv] lv_coefs;",
    model_file,
    fixed = TRUE
  )] <- paste0(
    "matrix[n_series, n_lv] lv_coefs = rep_matrix(0, n_series, n_lv);\n",
    'matrix[n, n_lv] LV;\n'
  )

  starts <- grep(
    "LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));",
    model_file,
    fixed = TRUE
  )
  ends <- starts + 5
  model_file <- model_file[-(starts:ends)]
  model_file[grep(
    "// latent process linear predictors",
    model_file,
    fixed = TRUE
  )] <- paste0(
    "// latent process linear predictors\n",
    "trend_mus = X_trend * b_trend;\n\n",
    "// constraints allow identifiability of loadings\n",
    "{\n",
    "int idx;\n",
    "idx = 0;\n",
    "for(j in 1 : n_lv) lv_coefs[j, j] = L_diag[j];\n",
    "for(j in 1 : n_lv) {\n",
    "for(k in (j + 1) : n_series) {\n",
    "idx = idx + 1;\n",
    "lv_coefs[k, j] = L_lower[idx];\n",
    "}\n",
    "}\n",
    "}\n\n",
    "// raw latent factors (with linear predictors)\n",
    "for (j in 1 : n_lv) {\n",
    "for (i in 1 : n) {\n",
    "LV[i, j] = trend_mus[ytimes_trend[i, j]] + LV_raw[i, j];\n",
    "}\n}\n"
  )

  model_file <- model_file[
    -grep("// derived latent states", model_file, fixed = TRUE)
  ]
  model_file <- readLines(textConnection(model_file), n = -1)

  # Update model block
  sigma_prior <- grep(
    "// priors for latent state SD parameters",
    model_file,
    fixed = TRUE
  ) +
    1
  model_file <- model_file[-sigma_prior]
  model_file[grep(
    "// priors for latent state SD parameters",
    model_file,
    fixed = TRUE
  )] <- paste0(
    "// priors for factors and loading coefficients\n",
    "L_lower ~ student_t(3, 0, 1);\n",
    "L_diag ~ student_t(3, 0, 1);"
  )
  model_file <- readLines(textConnection(model_file), n = -1)

  # Update generated quantities
  model_file[grep(
    'matrix[n, n_series] mus;',
    model_file,
    fixed = TRUE
  )] <- paste0(
    'matrix[n, n_series] mus;\n',
    'vector[n_lv] sigma;'
  )
  model_file[grep(
    "penalty = 1.0 / (sigma .* sigma);",
    model_file,
    fixed = TRUE
  )] <- paste0(
    "penalty = rep_vector(1.0, n_lv);\n",
    "sigma = rep_vector(1.0, n_lv);"
  )
  model_file <- readLines(textConnection(model_file), n = -1)
  model_file <- sanitise_modelfile(model_file)

  # Remove Z from model_data as it is no longer needed
  model_data <- mod$model_data
  model_data$Z <- NULL

  # Add M to model_data
  n_series <- NCOL(model_data$ytimes)
  model_data$M <- n_lv * (n_series - n_lv) + n_lv * (n_lv - 1) / 2

  #### Autoformat the Stan code ####
  if (requireNamespace('cmdstanr', quietly = TRUE) & backend == 'cmdstanr') {
    if (
      requireNamespace('cmdstanr') &
        cmdstanr::cmdstan_version() >= "2.29.0"
    ) {
      model_file <- .autoformat(
        model_file,
        overwrite_file = FALSE,
        backend = 'cmdstanr',
        silent = silent >= 1L
      )
    }
    model_file <- readLines(textConnection(model_file), n = -1)
  } else {
    model_file <- .autoformat(
      model_file,
      overwrite_file = FALSE,
      backend = 'rstan',
      silent = silent >= 1L
    )
    model_file <- readLines(textConnection(model_file), n = -1)
  }

  # Remove lp__ from monitor params if VB is to be used
  param <- unique(c(mod$monitor_pars, 'Sigma', 'LV'))
  if (algorithm %in% c('meanfield', 'fullrank', 'pathfinder', 'laplace')) {
    param <- param[!param %in% 'lp__']
  }

  #### Determine what to return ####
  if (!run_model) {
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
    if (backend == 'cmdstanr') {
      if (!requireNamespace('cmdstanr', quietly = TRUE)) {
        warning('cmdstanr library not found. Defaulting to rstan')
        use_cmdstan <- FALSE
      } else {
        use_cmdstan <- TRUE
        if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
          warning(
            'cmdstanr library found but Cmdstan not found. Defaulting to rstan'
          )
          use_cmdstan <- FALSE
        }
      }
    }

    #### Run the model ####
    if (use_cmdstan) {
      # Prepare threading and generate the model
      cmd_mod <- .model_cmdstanr(model_file, threads = threads, silent = silent)

      # Condition the model using Cmdstan
      out_gam_mod <- .sample_model_cmdstanr(
        model = cmd_mod,
        algorithm = algorithm,
        prior_simulation = FALSE,
        data = model_data,
        chains = chains,
        parallel = parallel,
        silent = silent,
        max_treedepth = control$max_treedepth,
        adapt_delta = control$adapt_delta,
        threads = threads,
        burnin = burnin,
        samples = samples,
        param = param,
        save_all_pars = FALSE,
        ...
      )
    } else {
      # Condition the model using rstan
      requireNamespace('rstan', quietly = TRUE)
      out_gam_mod <- .sample_model_rstan(
        model = model_file,
        algorithm = algorithm,
        prior_simulation = FALSE,
        data = model_data,
        chains = chains,
        parallel = parallel,
        silent = silent,
        max_treedepth = control$max_treedepth,
        adapt_delta = control$adapt_delta,
        threads = threads,
        burnin = burnin,
        samples = samples,
        thin = thin,
        ...
      )
    }

    # After modeling (add a new class to make predictions and other post-processing
    # simpler)
    out1 <- mod
    out1$model_output <- out_gam_mod
    class(out1) <- c('mvgam')
    if (residuals) {
      mod_residuals <- dsresids_vec(out1)
    } else {
      mod_residuals <- NULL
    }
    rm(out1)

    # Add the posterior median coefficients to the mgcv objects
    ss_gam <- mod$mgcv_model
    V <- cov(mcmc_chains(out_gam_mod, 'b'))
    ss_gam$Vp <- ss_gam$Vc <- V
    p <- mcmc_summary(
      out_gam_mod,
      'b',
      variational = algorithm %in%
        c('meanfield', 'fullrank', 'pathfinder', 'laplace')
    )[, c(4)]
    names(p) <- names(ss_gam$coefficients)
    ss_gam$coefficients <- p

    trend_mgcv_model <- mod$trend_mgcv_model
    V <- cov(mcmc_chains(out_gam_mod, 'b_trend'))
    trend_mgcv_model$Vp <- trend_mgcv_model$Vc <- V
    p <- mcmc_summary(
      out_gam_mod,
      'b_trend',
      variational = algorithm %in%
        c('meanfield', 'fullrank', 'pathfinder', 'laplace')
    )[, c(4)]
    names(p) <- names(trend_mgcv_model$coefficients)
    trend_mgcv_model$coefficients <- p

    #### Return the output as class mvgam ####
    trim_data <- list()
    attr(trim_data, 'threads') <- threads
    attr(trim_data, 'noncentred') <- NULL
    attr(trim_data, 'trend_model') <- 'None'
    attr(trim_data, 'prepped_trend_model') <- prepped_trend

    # Extract sampler arguments
    dots <- list(...)
    if ('adapt_delta' %in% names(dots)) {
      message(
        'argument "adapt_delta" should be supplied as an element in "control"'
      )
      adapt_delta <- dots$adapt_delta
      dots$adapt_delta <- NULL
    } else {
      adapt_delta <- control$adapt_delta
      if (is.null(adapt_delta)) adapt_delta <- 0.8
    }

    if ('max_treedepth' %in% names(dots)) {
      message(
        'argument "max_treedepth" should be supplied as an element in "control"'
      )
      max_treedepth <- dots$max_treedepth
      dots$max_treedepth <- NULL
    } else {
      max_treedepth <- control$max_treedepth
      if (is.null(max_treedepth)) max_treedepth <- 10
    }

    out <- structure(
      list(
        call = mod$call,
        trend_call = factor_formula,
        family = mod$family,
        share_obs_params = mod$share_obs_params,
        trend_model = 'None',
        trend_map = trend_map,
        drift = FALSE,
        priors = mod$priors,
        model_output = out_gam_mod,
        model_file = model_file,
        model_data = if (return_model_data) {
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
        adapt_delta = adapt_delta
      ),
      class = c('mvgam', 'jsdgam')
    )
  }

  return(out)
}

#' Prep trend for jsdgam
#' @noRd
prep_jsdgam_trend = function(data, unit, subgr) {
  unit <- as_one_character(unit)
  subgr <- as_one_character(subgr)
  validate_var_exists(
    data = data,
    variable = unit,
    type = 'num/int',
    name = 'data',
    trend_char = 'ZMVN'
  )
  validate_var_exists(
    data = data,
    variable = subgr,
    type = 'factor',
    name = 'data',
    trend_char = 'ZMVN'
  )
  out <- structure(
    list(
      trend_model = 'ZMVN',
      ma = FALSE,
      cor = TRUE,
      unit = unit,
      gr = "NA",
      subgr = subgr,
      label = NULL
    ),
    class = 'mvgam_trend'
  )
}

#' @noRd
prep_jsdgam_trendmap = function(data, n_lv) {
  if (n_lv > nlevels(data$series)) {
    stop(
      'Number of factors must be <= number of levels in species',
      call. = FALSE
    )
  }
  data.frame(
    trend = rep(1:n_lv, nlevels(data$series))[1:nlevels(data$series)],
    series = factor(levels(data$series), levels = levels(data$series))
  )
}
