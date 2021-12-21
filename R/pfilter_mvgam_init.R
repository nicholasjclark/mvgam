#'Initiate particles for online filtering from a fitted mvjagam object
#'
#'This function generates a set of particles that each captures a unique proposal about
#'the current state of the system. The next observation in \code{data_assim} is assimilated
#'and particles are weighted by their proposal's multivariate composite likelihood to update the model's
#'forecast distribution
#'
#'@param object \code{list} object returned from \code{mvjagam}
#'@param data_assim A \code{dataframe} of test data containing at least one more observation per series
#'(beyond the last observation seen by the model in \code{object}) to be assimilated by the particle filter.
#'Should at least contain 'series', 'season' and 'year' for the one-step ahead horizon,
#'in addition to any other variables included in the linear predictor of \code{object}
#'@param n_particles \code{integer} specifying the number of unique particles to generate for tracking the
#'latent system state
#'@param file_path \code{character} string specifying the file path for saving the initiated particles
#'@param n_cores \code{integer} specifying number of cores for generating particle forecasts in parallel
#'@return A \code{list} object of \code{length = n_particles} containing information on parameters and
#'current state estimates for each particle is generated and saved, along with other important information
#'from the original model, to an \code{.rda} object in \code{file_path}
#'@export
pfilter_mvgam_init = function(object,
                              data_assim,
                              n_particles = 1000,
                              file_path = 'pfilter',
                              n_cores = 2){

#### 1. Generate linear predictor matrix for the next timepoint and extract last trend estimates
# (NOTE, all series must have observations for the next timepoint, even if they are NAs!!!!) ####
data_train <- object$obs_data
n_series <- NCOL(object$ytimes)

# Next observation for assimilation when forming particles (ensure data_assim has been arranged correctly)
data_assim %>%
  dplyr::arrange(year, season, series) -> data_assim
series_test <- data_assim[1:n_series,]

if(length(unique(series_test$season)) > 1){
  stop('data_assim should have one observation per series for the next seasonal timepoint')
}

# Linear predictor matrix for the next observation
Xp <- predict(object$mgcv_model,
              newdata = series_test,
              type = 'lpmatrix')

# Extract last trend / latent variable and precision estimates
if(object$use_lv){
  n_lv <- object$n_lv
  ends <- seq(0, dim(MCMCvis::MCMCchains(object$jags_output, 'LV'))[2],
              length.out = n_lv + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (n_lv + 1))])
  ends <- ends[-1]
  lvs <- lapply(seq_len(n_lv), function(lv){
    lv_estimates <- MCMCvis::MCMCchains(object$jags_output, 'LV')[,starts[lv]:ends[lv]]
    lv_estimates[,(NCOL(lv_estimates)-2):(NCOL(lv_estimates))]
  })

  taus <- MCMCvis::MCMCchains(object$jags_output, 'tau_fac')
  trends <- NULL
} else {
  ends <- seq(0, dim(MCMCvis::MCMCchains(object$jags_output, 'trend'))[2],
              length.out = n_series + 1)
  starts <- ends + 1
  starts <- c(1, starts[-c(1, (n_series + 1))])
  ends <- ends[-1]
  trends <- lapply(seq_len(n_series), function(series){
    trend_estimates <- MCMCvis::MCMCchains(object$jags_output, 'trend')[,starts[series]:ends[series]]
    trend_estimates[,(NCOL(trend_estimates)-2):(NCOL(trend_estimates))]
  })

  taus <- MCMCvis::MCMCchains(object$jags_output, 'tau')
  lvs <- NULL
}

# If use_lv, extract latent variable loadings
if(object$use_lv){
  lv_coefs <- lapply(seq_len(n_series), function(series){
    lv_indices <- seq(1, n_series * n_lv, by = n_series) + (series - 1)
    as.matrix(MCMCvis::MCMCchains(object$jags_output, 'lv_coefs')[,lv_indices])
  })
} else {
  lv_coefs <- NULL
}

# Beta coefficients for GAM component
betas_orig <- MCMCvis::MCMCchains(object$jags_output, 'b')
betas <- matrix(NA, nrow = dim(betas_orig)[1],
                ncol = NCOL(MCMCvis::MCMCchains(object$jags_output, 'b')))
betas_hpd <- apply(betas_orig, 2, function(x) hpd(x)[2])
for(i in 1:dim(betas_orig)[1]){
  betas[i,] <- betas_hpd
}

# GAM component weights
gam_comps <- MCMCvis::MCMCchains(object$jags_output, 'gam_comp')

# Phi estimates for latent trend drift terms
phis <- MCMCvis::MCMCchains(object$jags_output, 'phi')

# AR term estimates
ar1s <- MCMCvis::MCMCchains(object$jags_output, 'ar1')
ar2s <- MCMCvis::MCMCchains(object$jags_output, 'ar2')
ar3s <- MCMCvis::MCMCchains(object$jags_output, 'ar3')

# Negative binomial size estimate
sizes <- as.matrix(rep(hpd(MCMCvis::MCMCchains(object$jags_output, 'r'))[2],
                       dim(betas_orig)[1]))

# Generate sample sequence for n_particles
if(n_particles < dim(phis)[1]){
  sample_seq <- sample(seq_len(dim(phis)[1]), size = n_particles, replace = F)
} else {
  sample_seq <- sample(seq_len(dim(phis)[1]), size = n_particles, replace = T)
}

#### 2. Generate particles and calculate their proposal weights ####
use_lv <- object$use_lv
truth <- series_test$y
last_assim <- c(unique(series_test$season), unique(series_test$year))
upper_bounds <- object$upper_bounds

cl <- parallel::makePSOCKcluster(n_cores)
setDefaultCluster(cl)
clusterExport(NULL, c('use_lv',
                      'Xp',
                      'betas',
                      'series_test',
                      'gam_comps',
                      'truth',
                      'last_assim',
                      'taus',
                      'phis',
                      'ar1s',
                      'ar2s',
                      'ar3s',
                      'lvs',
                      'lv_coefs',
                      'trends',
                      'sizes',
                      'n_series',
                      'n_particles',
                      'upper_bounds'),
              envir = environment())

pbapply::pboptions(type = "none")

particles <- pbapply::pblapply(sample_seq, function(x){
  cat('running',x,'\n')
  if(use_lv){
  # Sample a last state estimate for the latent variables
  samp_index <- x
  last_lvs <- lapply(seq_along(lvs), function(lv){
    lvs[[lv]][samp_index, ]
  })

  # Sample drift and AR parameters
  phi <- phis[samp_index, ]
  ar1 <- ar1s[samp_index, ]
  ar2 <- ar2s[samp_index, ]
  ar3 <- ar3s[samp_index, ]

  # Sample lv precision
  tau <- taus[samp_index,]

  # Sample lv loadings
  lv_coefs <- do.call(rbind, lapply(seq_len(n_series), function(series){
    lv_coefs[[series]][samp_index,]
  }))

  # Sample beta coefs and GAM contributions
  betas <- betas[samp_index, ]
  gam_comp <- gam_comps[samp_index, ]

  # Sample a negative binomial size parameter
  size <- sizes[samp_index, ]

  # Run the latent variables forward one timestep
  next_lvs <- do.call(cbind, lapply(seq_along(lvs), function(lv){
    rnorm(1, phi[lv] + ar1[lv] * last_lvs[[lv]][3] +
            ar2[lv] * last_lvs[[lv]][2] +
            ar3[lv] * last_lvs[[lv]][1], sqrt(1 / tau))
  }))

  # Multiply lv states with loadings to generate each series' forecast trend state
  trends <- as.numeric(next_lvs %*% t(lv_coefs))

  # Include previous two states for each lv
  next_lvs <- lapply(seq_along(lvs), function(lv){
    as.vector(c(as.numeric(last_lvs[[lv]][2]),
                as.numeric(last_lvs[[lv]][3]),
                next_lvs[lv]))
  })
  names(next_lvs) <- paste0('lv', seq(1:length(lvs)))

  # Calculate weight for the particle in the form of a composite likelihood
  weight <- prod(unlist(lapply(seq_len(n_series), function(series){

    if(is.na(truth[series])){
      weight <- 1
    } else {
      weight <- dnbinom(truth[series], size = size,
                        mu = exp((gam_comp[series] *
                                    (Xp[which(as.numeric(series_test$series) == series),] %*% betas)) +
                                   (trends[series] * (1 - gam_comp[series]))))
    }
    weight
  })), na.rm = T)
  n_lv <- length(lvs)
  trends <- NULL

  } else {
    next_lvs = NULL
    n_lv = NULL
    lv_coefs = NULL

    # Sample index for the particle
    samp_index <- x

    # Sample beta coefs and GAM contributions
    betas <- betas[samp_index, ]
    gam_comp <- gam_comps[samp_index, ]

    # Sample last state estimates for the trends
    last_trends <- lapply(seq_along(trends), function(trend){
      trends[[trend]][samp_index, ]
    })

    # Sample AR parameters
    phi <- phis[samp_index, ]
    ar1 <- ar1s[samp_index, ]
    ar2 <- ar2s[samp_index, ]
    ar3 <- ar3s[samp_index, ]

    # Sample trend precisions
    tau <- taus[samp_index,]

    # Sample a negative binomial size parameter
    size <- sizes[samp_index, ]

    # Run the trends forward one timestep
    trends <- do.call(cbind, lapply(seq_along(trends), function(trend){
      rnorm(1, phi[trend] + ar1[trend] * last_trends[[trend]][3] +
              ar2[trend] * last_trends[[trend]][2] +
              ar3[trend] * last_trends[[trend]][1], sqrt(1 / tau[trend]))
    }))

    # Calculate weight for the particle in the form of a composite likelihood
    weight <- prod(unlist(lapply(seq_len(n_series), function(series){

      if(is.na(truth[series])){
        weight <- 1
      } else {
        weight <- dnbinom(truth[series], size = size,
                          mu = exp((gam_comp[series] *
                                      (Xp[which(as.numeric(series_test$series) == series),] %*% betas)) +
                                     (trends[series] * (1 - gam_comp[series]))))
      }
      weight
    })), na.rm = T)

    # Include previous two states for each trend
    trends <- lapply(seq_len(n_series), function(trend){
      as.vector(c(last_trends[[trend]][2], last_trends[[trend]][3], trends[trend]))
    })
    names(trends) <- paste0('series', seq_len(n_series))
}

  # Store important particle-specific information for later filtering
list(use_lv = use_lv,
     n_lv = n_lv,
     lv_states = next_lvs,
     lv_coefs = lv_coefs,
     betas = as.numeric(betas),
     gam_comp = as.numeric(gam_comp),
     size = as.numeric(size),
     tau = as.numeric(tau),
     phi = as.numeric(phi),
     ar1 = as.numeric(ar1),
     ar2 = as.numeric(ar2),
     ar3 = as.numeric(ar3),
     trend_states = trends,
     weight = weight,
     upper_bounds = upper_bounds,
     last_assim = last_assim)

}, cl = cl)
stopCluster(cl)

# Calculate ESS and save outputs for later online filtering
mgcv_model <- object$mgcv_model
data_train$assimilated <- 'no'
series_test$assimilated <- 'yes'
data_train %>%
  dplyr::bind_rows(series_test) -> obs_data
weights <- (unlist(lapply(seq_along(particles), function(x){
  tail(particles[[x]]$weight, 1)})))
weights <- weights / sum(weights)
ess <- 1 / sum(weights^2)
dir.create(file_path, recursive = T, showWarnings = F)
cat('Saving particles to', paste0(file_path, '/particles.rda'), '\n',
    'ESS =',  ess, '\n')
save(particles, mgcv_model, obs_data, last_assim,
     ess = ess, file = paste0(file_path, '/particles.rda'))
}

