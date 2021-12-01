#'Initiate particles for online filtering from a fitted mvjagam object
#'
#'This function generates a set of particles that each captures a unique proposal about
#'the current state of the system that was modelled in the mvjagam object. The next observation is assimilated
#'and particles are weighted by their proposal's discrete rank probability score
#'
#'@param object \code{list} object returned from \code{mvjagam}
#'@param data_train A \code{dataframe} containing the model response variable and covariates
#'required by the GAM \code{formula}. Should include columns:
#''y' (the discrete outcomes; NAs allowed)
#''series' (character or factor index of the series IDs)
#''season' (numeric index of the seasonal time point for each observation; should not have any missing)
#''year' the numeric index for year, and
#''in_season' indicator for whether the observation is in season or not. If the counts tend to go to zero
#'during the off season (as in tick counts for example), setting 'in_season' to zero cduring these seasonal periods
#'can be useful as trends won't contribute during
#'during this time but they continue to evolve, allowing the trend to continue evolving rather than forcing
#'it to zero during the off-season
#'Any other variables to be included in the linear predictor of \code{formula} must also be present
#'@param data_assim A \code{dataframe} of test data containing at least one more observation (beyond the last observation
#'seen by the model in \code{object} to be assimilated by the particle filter. Should at least contain 'series', 'season', 'year' and
#''in_season' for the one-step ahead horizon, in addition to any other variables included in the linear predictor of \code{object}
#'@param n_particles \code{integer} specifying the number of unique particles to generate for tracking the latent system state
#'@param file_path \code{character} string specifying the file path for saving the initiated particles
#'@export
pfilter_mvgam_init = function(object,
                              data_train,
                              data_assim,
                              n_particles = 1000,
                              file_path = 'pfilter'){

# Function to calculate Discrete Rank Probability Score
drps_score <- function(truth, fc){
  nsum <- 1000
  Fy = ecdf(fc)
  ysum <- 0:nsum
  indicator <- ifelse(ysum - truth >= 0, 1, 0)
  score <- sum((indicator - Fy(ysum))^2)

  return(score)
}

#### 1. Calculate predictive distribution from the GAM component ####
# Generate linear predictor matrix for the next timepoint (NOTE, all series must have observations
# for the next timepoint, even if they are NAs!!!!)
n_series <- NCOL(object$ytimes)
series_test <- data_assim[1:n_series,]

if(length(unique(series_test$season)) > 1){
  stop('data_assim should have one observation per series for the next seasonal timepoint')
}

Xp <- predict(object$mgcv_model,
              newdata = series_test,
              type = 'lpmatrix')

# Extract beta coefs and gam_contributions
betas <- MCMCvis::MCMCchains(object$jags_output, 'b')
gam_comps <- MCMCvis::MCMCchains(object$jags_output, 'gam_comp')

# GAM component linear predictions
gam_preds <- matrix(NA, nrow = dim(betas)[1], ncol = NROW(series_test))
for(i in 1:dim(betas)[1]){
  gam_preds[i,] <- gam_comps[i,] * (Xp %*% betas[i,])
}

# Extract last trend / latent variable and precision estimates
if(object$use_lv){
  n_lv <- object$n_lv
  ends <- seq(0, dim(MCMCvis::MCMCchains(object$jags_output, 'LV'))[2],
              length.out = n_lv + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (n_lv + 1))])
  ends <- ends[-1]
  lvs <- do.call(cbind, lapply(seq_len(n_lv), function(lv){
    lv_estimates <- MCMCvis::MCMCchains(object$jags_output, 'LV')[,starts[lv]:ends[lv]]
    lv_estimates[,NCOL(lv_estimates)]
  }))

  tau <- as.numeric(hpd(MCMCvis::MCMCchains(object$jags_output, 'tau_fac'), 0.7)[2])

} else {
  ends <- seq(0, dim(MCMCvis::MCMCchains(object$jags_output, 'trend'))[2],
              length.out = n_series + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (n_series + 1))])
  ends <- ends[-1]
  trends <- do.call(cbind, lapply(seq_len(n_series), function(series){
    trend_estimates <- MCMCvis::MCMCchains(object$jags_output, 'trend')[,starts[series]:ends[series]]
    trend_estimates[,NCOL(trend_estimates)]
  }))

  tau <- as.numeric(apply(MCMCvis::MCMCchains(object$jags_output, 'tau'), 2, function(x) hpd(x, 0.7)[2]))

}

# If use_lv, extract latent variable loadings for generating the trend forecasts
if(object$use_lv){

  lv_coefs <- do.call(rbind, lapply(seq_len(n_series), function(series){
    lv_indices <- seq(1, n_series * n_lv, by = n_series) + (series - 1)
    lv_coefs <- MCMCvis::MCMCchains(object$jags_output, 'lv_coefs')[,lv_indices]

    # Calculate posterior mode for each coefficient as we cannot estimate static variables with our
    # particle filter
    as.numeric(apply(lv_coefs, 2, function(x) hpd(x, 0.7)[2]))
  }))
}

# Extract phi estimates for latent trend drift terms
phi <- MCMCvis::MCMCchains(object$jags_output, 'phi')

# Negative binomial size estimate
size <- MCMCvis::MCMCsummary(object$jags_output, 'r')$mean

# Extract upper bound for the series if specified
if(!is.null(object$upper_bounds)){
  upper_bounds <- object$upper_bounds
} else {
  upper_bounds <- NULL
}

# Generate sample sequence for n_particles
if(n_particles < dim(phi)[1]){
  sample_seq <- sample(seq_len(dim(phi)[1]), size = n_particles, replace = F)
} else {
  sample_seq <- sample(seq_len(dim(phi)[1]), size = n_particles, replace = T)
}

#### 2. Generate particles and calculate their proposal weights ####
use_lv = object$use_lv
truth = series_test$y
last_assim = c(unique(series_test$season), unique(series_test$year))

particles <- lapply(sample_seq, function(x){

  if(use_lv){
  # Sample a last state estimate for the latent variables
  last_lv <- lvs[sample(1:NROW(lvs), 1, T), ]

  # Sample drift parameters
  phi <- phi[sample(1:NROW(lvs), 1, T), ]

  # Run the latent variables forward one timestep
  next_lvs <- do.call(cbind, lapply(seq_len(NCOL(lvs)), function(x){
    rnorm(1, phi[x] + last_lv[x], 1 / tau)
  }))

  # Multiply with loadings to generate the series' forecast trend states
  trends <- as.numeric(next_lvs %*% t(lv_coefs))

  # Calculate forecast distributions for the particle
  series_fcs <- lapply(seq_len(n_series), function(series){
    trend_preds <- trends[series] * (1 - gam_comps[,series])
    full_preds <- matrix(NA, nrow = length(trend_preds), ncol = 1)
    for(i in 1:length(trend_preds)){
      trunc_preds <- rnbinom(1, mu = exp(gam_preds[i, series] + trend_preds[i]),
                             size = size)

      if(!is.null(upper_bounds)){
        trunc_preds[trunc_preds > upper_bounds[series]] <- upper_bounds[series]
      }

      full_preds[i,] <- trunc_preds
    }
    full_preds
  })

  } else {
    next_lvs = NULL
    n_lv = NULL
    lv_coefs = NULL

    # Sample a last state estimate for the trends
    last_trend <- trends[sample(1:NROW(trends), 1, T), ]

    # Sample drift parameters
    phi <- phi[sample(1:NROW(trends), 1, T), ]

    # Run the latent variables forward one timestep
    trends <- do.call(cbind, lapply(seq_len(NCOL(trends)), function(x){
      rnorm(1, phi[x] + last_trend[x], 1 / tau[x])
    }))

    # Calculate forecast distributions for the particle
    series_fcs <- lapply(seq_len(n_series), function(series){
      trend_preds <- trends[series] * (1 - gam_comps[,series])
      full_preds <- matrix(NA, nrow = length(trend_preds), ncol = 1)
      for(i in 1:length(trend_preds)){
        trunc_preds <- rnbinom(1, mu = exp(gam_preds[i, series] + trend_preds[i]),
                               size = size)

        if(!is.null(upper_bounds)){
          trunc_preds[trunc_preds > upper_bounds[series]] <- upper_bounds[series]
        }

        full_preds[i,] <- trunc_preds
      }
      full_preds
    })

}

# Calculate weight for the particle in the form of 1 / sum(DRPS)
  weight <- sum(unlist(lapply(seq_len(n_series), function(series){

    if(is.na(truth[series])){
      weight <- 1
    } else {
      weight <- 1 / drps_score(truth[series], series_fcs[[series]])
    }
    weight
  })), na.rm = T)


list(use_lv = use_lv,
     n_lv = n_lv,
     lv_states = next_lvs,
     lv_coefs = lv_coefs,
     size = size,
     upper_bounds = upper_bounds,
     tau = tau,
     phi = as.numeric(phi),
     trend_states = as.numeric(trends),
     weight = weight,
     ess = n_particles,
     last_assim = last_assim)

})

# Save outputs for later online filtering
mgcv_model <- object$mgcv_model
dir.create(file_path, recursive = T, showWarnings = F)
cat('Saving particles to', paste0(file_path, '/particles.rda'), '\n')
save(particles, mgcv_model, betas, gam_comps, last_assim,
     ess = n_particles, file = paste0(file_path, '/particles.rda'))
}

