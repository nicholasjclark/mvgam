#'Initiate particles for online filtering from a fitted mvgam object
#'
#'This function generates a set of particles that each captures a unique proposal about
#'the current state of the system. The next observation in \code{data_assim} is assimilated
#'and particles are weighted by their proposal's multivariate composite likelihood to update the model's
#'forecast distribution
#'
#'@importFrom parallel clusterExport stopCluster setDefaultCluster
#'@importFrom stats predict
#'@param object \code{list} object returned from \code{mvgam}
#'@param newdata A \code{dataframe} or \code{list} of test data containing at least one more observation per series
#'(beyond the last observation seen by the model in \code{object}) to be assimilated by the particle filter.
#'Should at least contain 'series' and 'time' for the one-step ahead horizon,
#'in addition to any other variables included in the linear predictor of \code{object}
#'@param data_assim Deprecated. Still works in place of \code{newdata} but users are recommended to use
#'\code{newdata} instead for more seamless integration into `R` workflows
#'@param n_particles \code{integer} specifying the number of unique particles to generate for tracking the
#'latent system state
#'@param file_path \code{character} string specifying the file path for saving the initiated particles
#'@param n_cores \code{integer} specifying number of cores for generating particle forecasts in parallel
#'@return A \code{list} object of \code{length = n_particles} containing information on parameters and
#'current state estimates for each particle is generated and saved, along with other important information
#'from the original model, to an \code{.rda} object in \code{file_path}
#'@export
pfilter_mvgam_init = function(object,
                              newdata,
                              data_assim,
                              n_particles = 1000,
                              file_path = 'pfilter',
                              n_cores = 2){

  # Check arguments
  if (!(inherits(object, "mvgam"))) {
    stop('argument "object" must be of class "mvgam"')
  }

  if(!missing("newdata")){
    data_assim <- newdata
  }

  if(sign(n_cores) != 1){
    stop('argument "n_cores" must be a positive integer',
         call. = FALSE)
  } else {
    if(n_cores%%1 != 0){
      stop('argument "n_cores" must be a positive integer',
           call. = FALSE)
    }
  }

  # Ensure outcome is labelled 'y' when feeding data to the model for simplicity
  mod_call <- object$call
  if(terms(formula(mod_call))[[2]] != 'y'){
    data_assim$y <- data_assim[[terms(formula(mod_call))[[2]]]]
  }

#### 1. Generate linear predictor matrix for the next timepoint and extract last trend estimates
# (NOTE, all series must have observations for the next timepoint, even if they are NAs!!!!) ####
data_train <- object$obs_data
n_series <- NCOL(object$ytimes)

# Variable name checks
if(class(object$obs_data)[1] == 'list'){
  if(!'time' %in% names(data_assim)){
    stop('data_assim does not contain a "time" column')
  }

  if(!'series' %in% names(data_assim)){
    data_assim$series <- factor('series1')
  }

} else {
  if(!'time' %in% colnames(data_assim)){
    stop('data_assim does not contain a "time" column')
  }

  if(!'series' %in% colnames(data_assim)){
    data_assim$series <- factor('series1')
  }
}

# Next observation for assimilation (ensure data_assim is arranged correctly)
if(class(object$obs_data)[1] == 'list'){

  # Find indices of next observation
  temp_dat = data.frame(time = data_assim$time,
                        series = data_assim$series) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    dplyr::arrange(time, series)
  indices_assim <- temp_dat[1:n_series,'index']

  # Get list object into correct format for lpmatrix prediction
  series_test <- lapply(data_assim, function(x){
    if(is.matrix(x)){
      matrix(x[indices_assim,], ncol = NCOL(x))
    } else {
      x[indices_assim]
    }

  })

} else {
  data_assim %>%
    dplyr::arrange(time, series) -> data_assim
  series_test <- data_assim[1:n_series,]
}


if(length(unique(series_test$time)) > 1){
  stop('data_assim should have one observation per series for the next timepoint')
}

# Linear predictor matrix for the next observation
suppressWarnings(Xp  <- try(predict(object$mgcv_model,
                                    newdata = series_test,
                                    type = 'lpmatrix'),
                                 silent = TRUE))

if(inherits(Xp, 'try-error')){
  testdat <- data.frame(time = series_test$time)

  terms_include <- names(object$mgcv_model$coefficients)[which(!names(object$mgcv_model$coefficients) %in% '(Intercept)')]
  if(length(terms_include) > 0){
    newnames <- vector()
    newnames[1] <- 'time'
    for(i in 1:length(terms_include)){
      testdat <- cbind(testdat, data.frame(series_test[[terms_include[i]]]))
      newnames[i+1] <- terms_include[i]
    }
    colnames(testdat) <- newnames
  }

  suppressWarnings(Xp  <- predict(object$mgcv_model,
                                       newdata = testdat,
                                       type = 'lpmatrix'))
}

# Beta coefficients for GAM component
betas <- mcmc_chains(object$model_output, 'b')

# Family-specific parameters
family <- object$family
family_pars <- extract_family_pars(object = object)

# Trend model
trend_model <- object$trend_model
use_lv <- object$use_lv

# Trend-specific parameters; keep only the last 3 estimates for RW / AR trends
# as we don't need any prior to that for propagating the trends forward. For GP
# trends, we have to keep all estimates through time
trend_pars <- extract_trend_pars(object = object,
                                 keep_all_estimates = FALSE)

# Generate sample sequence for n_particles
if(n_particles < dim(betas)[1]){
  sample_seq <- sample(seq_len(dim(betas)[1]), size = n_particles, replace = F)
} else {
  sample_seq <- sample(seq_len(dim(betas)[1]), size = n_particles, replace = T)
}

#### 2. Generate particles and calculate their proposal weights ####
use_lv <- object$use_lv
truth <- series_test$y
last_assim <- unique(series_test$time)
upper_bounds <- object$upper_bounds

cl <- parallel::makePSOCKcluster(n_cores)
setDefaultCluster(cl)
clusterExport(NULL, c('use_lv',
                      'trend_model',
                      'trend_pars',
                      'Xp',
                      'betas',
                      'series_test',
                      'truth',
                      'last_assim',
                      'family',
                      'family_pars',
                      'n_series',
                      'n_particles',
                      'upper_bounds'),
              envir = environment())

pbapply::pboptions(type = "none")

particles <- pbapply::pblapply(sample_seq, function(x){

  samp_index <- x

  # Sample beta coefs
  betas <- betas[samp_index, ]

  # Sample general trend-specific parameters for storing in the particle
  general_trend_pars <- extract_general_trend_pars(trend_pars = trend_pars,
                                                   samp_index = samp_index)

  if(use_lv || trend_model == 'VAR1'){
    # Propagate the lvs of VAR forward only once using the sampled trend parameters
    trends <- forecast_trend(trend_model = trend_model,
                             use_lv = use_lv,
                             trend_pars = general_trend_pars,
                             h = 1)

    # Include previous states for each lv
    if(trend_model != 'GP'){
      general_trend_pars$last_lvs <- lapply(seq_along(general_trend_pars$last_lvs),
                                            function(x){
                                              c(general_trend_pars$last_lvs[[x]][2],
                                                general_trend_pars$last_lvs[[x]][3],
                                                trends[x])
                                            })
    } else {
      general_trend_pars$last_lvs <- lapply(seq_along(general_trend_pars$last_lvs),
                                            function(x){
                                              c(general_trend_pars$last_lvs[[x]],
                                                trends[x])
                                            })
    }

    general_trend_pars$last_lvs <- lapply(general_trend_pars$last_lvs, unname)
  }

  # Loop across series and produce the next trend estimate
  trend_states <- do.call(cbind, (lapply(seq_len(n_series), function(series){

    # Sample series- and trend-specific parameters
    trend_extracts <- extract_series_trend_pars(series = series,
                                                        samp_index = samp_index,
                                                        trend_pars = trend_pars,
                                                        use_lv = use_lv)

    if(use_lv || trend_model == 'VAR1'){
      if(use_lv){
        # Multiply lv states with loadings to generate the series' forecast trend state
        out <- as.numeric(trends %*% trend_extracts$lv_coefs)
      }

      if(trend_model == 'VAR1'){
        out <- trends[, series]
      }

    } else {
      # Propagate the series-specific trends forward
      out <- forecast_trend(trend_model = trend_model,
                                    use_lv = FALSE,
                                    trend_pars = trend_extracts,
                                    h = 1)
    }

    out
  })))

  # Include previous states for each series' trend in general trend parameters
 if(!use_lv && trend_model != 'VAR1'){
   if(!trend_model %in% c('GP')){
     general_trend_pars$last_trends <- unname(lapply(seq_along(general_trend_pars$last_trends),
                                           function(x){
                                             unname(c(general_trend_pars$last_trends[[x]][2],
                                               general_trend_pars$last_trends[[x]][3],
                                               trend_states[,x]))
                                           }))
   }

   if(trend_model == 'GP'){
     general_trend_pars$last_trends <- unname(lapply(seq_along(general_trend_pars$last_trends),
                                                     function(x){
                                                       unname(c(general_trend_pars$last_trends[[x]],
                                                                trend_states[,x]))
                                                     }))
   }
 }

  # Calculate weight for the particle in the form of a composite likelihood
  liks <- unlist(lapply(seq_len(n_series), function(series){

    if(is.na(truth[series])){
      weight <- 1
    } else {

      # Family-specific parameters
      family_extracts <- lapply(seq_along(family_pars), function(x){
        if(is.matrix(family_pars[[x]])){
          family_pars[[x]][samp_index, series]
        } else {
          family_pars[[x]][samp_index]
        }
      })
      names(family_extracts) <- names(family_pars)

      Xpmat <- t(rbind(as.matrix(Xp[which(as.numeric(series_test$series) == series),]),
                     trend_states[,series]))
      attr(Xpmat, 'model.offset') <- attr(Xp, 'model.offset')

      # Likelihood
      weight <- exp(mvgam_predict(family = family,
                                      family_pars = family_extracts,
                                      truth = truth[series],
                                      Xp = Xpmat,
                                      betas = c(betas, 1),
                                      density = TRUE))

    }
    weight
  }))
  weight <- prod(liks, na.rm = T)

  # Family-specific parameters
  general_family_pars <- lapply(seq_along(family_pars), function(x){
    if(is.matrix(family_pars[[x]])){
      family_pars[[x]][samp_index, ]
    } else {
      family_pars[[x]][samp_index]
    }
  })
  names(general_family_pars) <- names(family_pars)

  # Store important particle-specific information for later filtering
  list(n_series = n_series,
       use_lv = use_lv,
       family = family,
       trend_model = trend_model,
       betas = as.numeric(betas),
       trend_pars = general_trend_pars,
       family_pars = general_family_pars,
       weight = weight,
       liks = liks,
       upper_bounds = upper_bounds,
       last_assim = last_assim)

}, cl = cl)
stopCluster(cl)

# Calculate ESS and save outputs for later online filtering
mgcv_model <- object$mgcv_model
series_test$assimilated <- 'yes'

if(class(data_train)[1] == 'list'){

  data_train$assimilated <- rep('no', length(data_train$y))
  obs_data <- lapply(seq_along(data_train), function(x){

    if(is.matrix(data_train[[x]])){
      rbind(data_train[[x]], series_test[[x]])
    } else {
      if(is.factor(data_train[[x]])){
        factor(unlist(list(data_train[[x]], series_test[[x]])),
               levels = levels(data_train[[x]]))
      } else {
        c(data_train[[x]], series_test[[x]])
      }
    }
  })

  names(obs_data) <- names(series_test)

} else {
  data_train$assimilated <- 'no'
  data_train %>%
    dplyr::bind_rows(series_test) -> obs_data
}

# For multivariate models, gather information on each particle's ranked proposal
# to update weights so that we can target particles that perform well across the
# full set of series, rather than performing extremely well for one at the expense
# of the others
if(NCOL(object$ytimes) > 1){
lik_weights <- do.call(rbind, lapply(seq_along(particles), function(x){
  particles[[x]]$liks
}))

series_sds <- apply(lik_weights, 2, function(x) sd(x))
if(all(series_sds == 0)){
  # Do nothing if all series have equal weights due to resampling or all NAs in
  # last observation
} else {
  # Else update weights using ranking information
  lik_weights <- apply(apply(lik_weights[, !series_sds  == 0 ], 2, rank), 1, prod)
  lik_weights <- lik_weights / max(lik_weights)

  particles <- lapply(seq_along(particles), function(x){
    particles[[x]]$weight <- lik_weights[x] * particles[[x]]$weight
    particles[[x]]
  })
}
}

weights <- (unlist(lapply(seq_along(particles), function(x){
  tail(particles[[x]]$weight, 1)})))
weights <- weights / sum(weights)
ess <- 1 / sum(weights^2)
dir.create(file_path, recursive = T, showWarnings = F)
cat('Saving particles to', paste0(file_path, '/particles.rda'), '\n',
    'ESS =',  ess, '\n')
save(particles, mgcv_model, obs_data, last_assim,
     mod_call,
     ess = ess, file = paste0(file_path, '/particles.rda'))
}

