#'Assimilate new observations into a fitted mvgam model using resampling and kernel smoothing
#'
#'This function operates on a new observation in \code{next_assim} to update the
#'posterior forecast distribution. The next observation is assimilated
#'and particle weights are updated in light of their most recent their multivariate composite likelihood.
#'Low weight particles are smoothed towards the high weight state space using kernel smoothing, and options are
#'given for using resampling of high weight particles when Effective Sample Size falls below a
#'user-specified threshold
#'
#'@param particles A \code{list} of particles that have been run up to one observation prior to the observation
#'in \code{next_assim}
#'@param mgcv_model A \code{\link[mgcv]{gam}} model returned through a call to \code{link{mvjagam}}
#'@param next_assim A \code{dataframe} of test data containing at one more observation per series
#'(beyond the last observation seen by the model when initialising particles with
#' \code{\link{pfilter_mvgam_init}} or in previous calls to \code{pfilter_mvgam_online}.
#'Should at least contain 'series', 'season' and 'year' for the one-step ahead horizon,
#'in addition to any other variables included in the linear predictor of \code{object}
#'@param threshold \code{proportional numeric} specifying the Effective Sample Size limit under which
#'resampling of particles will be triggered (calculated as \code{ESS / n_particles}) if \code{use_resampling == TRUE}.
#'Should be between \code{0} and \code{1}
#'@param use_resampling \code{logical} specifying whether resampling should be used when ESS falls below
#'the specified \code{threshold}. Note that resampling can result in loss of the original model's diversity of
#'GAM beta coefficients, which may have undesirable consequences for the forecast distribution. If
#'\code{use_resampling} is \code{TRUE}, some effort is made to remedy this by assigning randomly sampled draws of
#'beta coefficients from the original model's distribution to each particle. This does not however guarantee that there
#'will be no loss of diversity, especially if successive resamples take place. Default for this option is therefore
#'\code{FALSE}
#'@param kernel_lambda \code{proportional numeric} specifying the strength of kernel smoothing to use when
#'pulling low weight particles toward the high likelihood state space. Should be between \code{0} and \code{1}
#'@param file_path \code{character} string specifying the file path for locating the particles
#'@param n_cores \code{integer} specifying number of cores for generating particle forecasts in parallel
#'@return A \code{list} object of \code{length = n_particles} containing information on parameters and
#'current state estimates for each particle
#'@export
pfilter_mvgam_smooth = function(particles,
                                mgcv_model,
                                next_assim,
                                threshold = 0.5,
                                n_cores = parallel::detectCores() - 1,
                                use_resampling = FALSE,
                                kernel_lambda = 1){

  # Linear predictor matrix for the next observation
  Xp <- predict(mgcv_model,
                newdata = next_assim,
                type = 'lpmatrix')

  use_lv <- particles[[1]]$use_lv
  last_assim = c(unique(next_assim$season), unique(next_assim$year))

  # Update importance weights in light of most recent observation by runnning particles
  # up to the current timepoint
  cl <- makePSOCKcluster(n_cores)
  setDefaultCluster(cl)
  clusterExport(NULL, c('Xp',
                        'use_lv',
                        'particles',
                        'next_assim'),
                envir = environment())

  pbapply::pboptions(type = "none")
  particles <- pbapply::pblapply(seq_along(particles), function(x){
    n_series <- length(particles[[x]]$phi)

    if(use_lv){

      # Run the latent variables forward one timestep
      lv_states <- unlist(lapply(seq_len(length(particles[[x]]$lv_states)), function(lv){
        rnorm(1, particles[[x]]$phi[lv] +
                particles[[x]]$ar1[lv] * particles[[x]]$lv_states[[lv]][3] +
                particles[[x]]$ar2[lv] * particles[[x]]$lv_states[[lv]][2] +
                particles[[x]]$ar3[lv] * particles[[x]]$lv_states[[lv]][1],
              sqrt(1 / particles[[x]]$tau[lv]))
      }))

      # Multiply lv states with loadings to generate each series' forecast trend state
      trend_states <- as.numeric(lv_states %*% t(particles[[x]]$lv_coefs))

      # Include previous two states for each lv
      next_lvs <- lapply(seq_len(length(particles[[x]]$lv_states)), function(lv){
        as.vector(c(particles[[x]]$lv_states[[lv]][2],
                    particles[[x]]$lv_states[[lv]][3],
                    lv_states[lv]))
      })
      names(next_lvs) <- paste0('lv', seq(1:length(lv_states)))
      next_trends <- NULL

    } else {
      # Run the trends forward one timestep
      trend_states <- unlist(lapply(seq_len(length(particles[[x]]$trend_states)), function(series){
        rnorm(1, particles[[x]]$phi[series] +
                particles[[x]]$ar1[series] * particles[[x]]$trend_states[[series]][3] +
                particles[[x]]$ar2[series] * particles[[x]]$trend_states[[series]][2] +
                particles[[x]]$ar3[series] * particles[[x]]$trend_states[[series]][1],
              sqrt(1 / particles[[x]]$tau[series]))
      }))

      # Include previous two states for each trend
      next_trends <- lapply(seq_len(n_series), function(trend){
        as.vector(c(particles[[x]]$trend_states[[trend]][2],
                    particles[[x]]$trend_states[[trend]][3],
                    trend_states[trend]))
      })
      names(next_trends) <- paste0('series', seq_len(n_series))

      next_lvs <- NULL

    }

   particle_weight <- prod(unlist(lapply(seq_len(n_series), function(series){

    if(is.na(next_assim$y[series])){
      series_weight <- 1
    } else {
      series_weight <- 1 + (dnbinom(next_assim$y[series],
                        size = particles[[x]]$size[series],
                        mu = exp(((Xp[which(as.numeric(next_assim$series) == series),] %*%
                                       particles[[x]]$betas)) +
                                   (trend_states[series]))))
    }
    series_weight
  })), na.rm = T)

   if(use_lv){
     trend_states <- NULL
   }

   # Update particle weight using a condensation algorithm
   weight <- particle_weight * particles[[x]]$weight
   list(use_lv = use_lv,
        n_lv = particles[[x]]$n_lv,
        lv_states = next_lvs,
        lv_coefs = particles[[x]]$lv_coefs,
        betas = particles[[x]]$betas,
        size = particles[[x]]$size,
        tau = particles[[x]]$tau,
        phi = particles[[x]]$phi,
        ar1 = particles[[x]]$ar1,
        ar2 = particles[[x]]$ar2,
        ar3 = particles[[x]]$ar3,
        trend_states = next_trends,
        weight = particle_weight,
        upper_bounds = particles[[x]]$upper_bounds,
        last_assim = c(unique(next_assim$season), unique(next_assim$year)))
  }, cl = cl)
  stopCluster(cl)

  weights <- (unlist(lapply(seq_along(particles), function(x){
    tail(particles[[x]]$weight, 1)})))

  # Initial thresholding, keeping only top 50% of weights
  weights_keep <- sample(sort.int(weights, decreasing = TRUE,
                             index.return = TRUE)$ix[1:floor(length(weights) / 2)],
                  length(weights),
                  replace = T)

  # Functions for Pareto smoothing of remaining importance weights
  # (from loo package: https://github.com/stan-dev/loo/tree/master/R)
  nlist <- function(...) {
    m <- match.call()
    out <- list(...)
    no_names <- is.null(names(out))
    has_name <- if (no_names) FALSE else nzchar(names(out))
    if (all(has_name))
      return(out)
    nms <- as.character(m)[-1L]
    if (no_names) {
      names(out) <- nms
    } else {
      names(out)[!has_name] <- nms[!has_name]
    }

    return(out)
  }

  adjust_k_wip <- function(k, n) {
    a <- 10
    n_plus_a <- n + a
    k * n / n_plus_a + a * 0.5 / n_plus_a
  }

  qgpd <- function(p, k, sigma) {
    if (is.nan(sigma) || sigma <= 0) {
      return(rep(NaN, length(p)))
    }

    sigma * expm1(-k * log1p(-p)) / k
  }

  gpdfit <- function(x, wip = TRUE, min_grid_pts = 30, sort_x = TRUE) {
    # See section 4 of Zhang and Stephens (2009)
    if (sort_x) {
      x <- sort.int(x)
    }
    N <- length(x)
    prior <- 3
    M <- min_grid_pts + floor(sqrt(N))
    jj <- seq_len(M)
    xstar <- x[floor(N / 4 + 0.5)] # first quartile of sample
    theta <- 1 / x[N] + (1 - sqrt(M / (jj - 0.5))) / prior / xstar
    l_theta <- N * lx(theta, x) # profile log-lik
    w_theta <- exp(l_theta - matrixStats::logSumExp(l_theta)) # normalize
    theta_hat <- sum(theta * w_theta)
    k <- mean.default(log1p(-theta_hat * x))
    sigma <- -k / theta_hat

    if (wip) {
      k <- adjust_k_wip(k, n = N)
    }

    if (is.nan(k)) {
      k <- Inf
    }

    nlist(k, sigma)
  }


  # internal ----------------------------------------------------------------

  lx <- function(a,x) {
    a <- -a
    k <- vapply(a, FUN = function(a_i) mean(log1p(a_i * x)), FUN.VALUE = numeric(1))
    log(a / k) - k - 1
  }
  enough_tail_samples <- function(tail_len, min_len = 5) {
    tail_len >= min_len
  }

  psis_smooth_tail <- function(x, cutoff) {
    len <- length(x)
    exp_cutoff <- exp(cutoff)

    # save time not sorting since x already sorted
    fit <- gpdfit(exp(x) - exp_cutoff, sort_x = FALSE)
    k <- fit$k
    sigma <- fit$sigma
    if (is.finite(k)) {
      p <- (seq_len(len) - 0.5) / len
      qq <- qgpd(p, k, sigma) + exp_cutoff
      tail <- log(qq)
    } else {
      tail <- x
    }
    list(tail = tail, k = k)
  }

  do_psis_i <- function(log_ratios_i, tail_len_i, ...) {
    S <- length(log_ratios_i)
    # shift log ratios for safer exponentation
    lw_i <- log_ratios_i - max(log_ratios_i)
    khat <- Inf

    if (enough_tail_samples(tail_len_i)) {
      ord <- sort.int(lw_i, index.return = TRUE)
      tail_ids <- seq(S - tail_len_i + 1, S)
      lw_tail <- ord$x[tail_ids]
      if (abs(max(lw_tail) - min(lw_tail)) < .Machine$double.eps/100) {
        # Won't fit when we have just resampled or when weights were just reset
      } else {
        cutoff <- ord$x[min(tail_ids) - 1] # largest value smaller than tail values
        smoothed <- psis_smooth_tail(lw_tail, cutoff)
        khat <- smoothed$k
        lw_i[ord$ix[tail_ids]] <- smoothed$tail
      }
    }

    # truncate at max of raw wts (i.e., 0 since max has been subtracted)
    lw_i[lw_i > 0] <- 0
    # shift log weights back so that the smallest log weights remain unchanged
    lw_i <- lw_i + max(log_ratios_i)

    list(log_weights = lw_i, pareto_k = khat)
  }

  pareto_weights <- do_psis_i(log_ratios_i = weights[weights_keep],
                                  tail_len_i = length(weights) / 5)$log_weights
  index <- sample(weights_keep, length(weights_keep),
                      replace = TRUE,
                      prob = pareto_weights)

  # Use effective sample size of weights to decide whether to resample particles
  # i.e. when there is too much disparity in weights
  if(length(unique(weights)) == 1){
    # When all weights are identical, a resample has just taken place so don't resample here
    cat('Effective sample size is', length(weights), '...\n\n')
    norm_weights <- weights / sum(weights)
    ess <- length(weights)
    next_update_seq <- seq(1:length(weights))
    re_weight = F
  } else{
    # Else calculate normalised weights and effective sample size; resample if ESS falls below
    # threshold
    norm_weights <- weights / sum(weights)
    ess <- 1 / sum(norm_weights^2)
    cat('Effective sample size is', ess, '...\n\n')
    if(ess - (length(norm_weights) * threshold) < 0){
      next_update_seq <- index
      re_weight = T
    } else{
      next_update_seq <- seq(1:length(weights))
      re_weight <- F
    }
  }

  # If resampling is not specified by user, keep the current full set of particles and only
  # use kernel smoothing
  if(!use_resampling){
    re_weight <- F
    next_update_seq <- seq(1:length(weights))
    orig_betas <- NULL
  } else {
    # If resampling, must preserve the original diversity of GAM beta coefficients
    next_update_seq <- index
    orig_betas <- do.call(rbind, purrr::map(particles, 'betas'))
  }

  if(length(unique(index)) > 100){
    use_smoothing <- TRUE
    cat('Smoothing particles ...\n\n')

    if(use_lv){
      # Extract weighted means and covariances of lv states and lv loadings for kernel smoothing
      best_lv <- do.call(rbind, lapply(index, function(j){
        unlist(particles[[j]]$lv_states)
        }))

      best_lv_coefs <- lapply(index, function(j){
        particles[[j]]$lv_coefs
      })
      best_lv_coefs <- do.call(rbind, lapply(seq_along(best_lv_coefs), function(x){
        as.vector(best_lv_coefs[[x]])
      }))

      best_lv_cov <- cov(as.matrix(best_lv))
      best_lv_means <- apply(best_lv, 2, mean)
      lv_draws <- MASS::mvrnorm(n = length(next_update_seq), mu = rep(0, length(best_lv_means)),
                                Sigma = best_lv_cov)

      best_lv_coefs_cov <- cov(as.matrix(best_lv_coefs))
      best_lv_coefs_means <- apply(best_lv_coefs, 2, mean)
      lv_coef_draws <- MASS::mvrnorm(n = length(next_update_seq), mu = rep(0, length(best_lv_coefs_means)),
                                     Sigma = best_lv_coefs_cov)

      best_trend_means <- NULL
      trend_draws <- NULL
      rm(best_lv, best_lv_coefs)

    } else {
      best_trend <- do.call(rbind, lapply(index, function(j){
        unlist(particles[[j]]$trend_states)
      }))
      best_trend_cov <- cov(as.matrix(best_trend))
      best_trend_means <- apply(best_trend, 2, mean)

      trend_draws <- MASS::mvrnorm(n = length(next_update_seq),
                                   mu = rep(0, length(best_trend_means)),
                                   Sigma = best_trend_cov)

      lv_draws <- NULL
      lv_coef_draws <- NULL
      best_lv_means <- NULL
      best_lv_coefs_means <- NULL
      rm(best_trend)
    }

  } else {
    trend_draws <- NULL
    best_lv_means <- NULL
    use_smoothing <- FALSE
    lv_draws <- NULL
    best_lv_means <- NULL
    lv_coef_draws <- NULL
    best_lv_coefs_means <- NULL
    re_weight <- FALSE
  }

  weight_thres.1 <- quantile(norm_weights, prob = 0.1, na.rm = T)
  weight_thres.4 <- quantile(norm_weights, prob = 0.4, na.rm = T)
  weight_thres.85 <- quantile(norm_weights, prob = 0.85, na.rm = T)

  library(parallel)
  cl <- makePSOCKcluster(n_cores)
  setDefaultCluster(cl)
  clusterExport(NULL, c('use_smoothing',
                        'use_resampling',
                        'orig_betas',
                        'lv_draws',
                        'best_lv_means',
                        'lv_coef_draws',
                        'best_lv_coefs_means',
                        'trend_draws',
                        'best_trend_means',
                        'use_lv',
                        'ess',
                        'norm_weights',
                        'particles',
                        'kernel_lambda',
                        're_weight',
                        'weight_thres.1',
                        'weight_thres.4',
                        'weight_thres.85'),
                envir = environment())

  clusterEvalQ(cl, library(MASS))
  pbapply::pboptions(type = "none")

  # Perform kernel smoothing and mutation of particles. Argument next_update_seq is determined by whether
  # or not the user wants to resample and what the ESS currently is
  particles <- pbapply::pblapply(next_update_seq, function(x){

    # No major changes when smoothing not in effect as all weights are very similar
    if(!use_smoothing){

      if(use_lv){
        lv_evolve <- particles[[x]]$lv_states
        lv_coefs_evolve <- particles[[x]]$lv_coefs
        particle_weight <- ifelse(re_weight, 1, tail(particles[[x]]$weight, 1))
        trend_evolve <- NULL
        phi_evolve <- particles[[x]]$phi
        ar1_evolve <- particles[[x]]$ar1
        ar2_evolve <- particles[[x]]$ar2
        ar3_evolve <- particles[[x]]$ar3

      } else {
        trend_evolve <- particles[[x]]$trend_states
        lv_evolve <- NULL
        lv_coefs_evolve <- NULL
        particle_weight <- ifelse(re_weight, 1, tail(particles[[x]]$weight, 1))
        phi_evolve <- particles[[x]]$phi
        ar1_evolve <- particles[[x]]$ar1
        ar2_evolve <- particles[[x]]$ar2
        ar3_evolve <- particles[[x]]$ar3
      }

    } else {

      # Else particles can be kernel smooothed towards higher likelihood space.
      # For kernel smoothing, how much a particle may be pulled towards the high-likelihood
      # space is determined  by its last fitness estimate (weight)
      # Particles with low weight (less than 10th percentile) are pulled more strongly towards the
      # state space of the high weight particles. Particles with moderate weights are only moderatly
      # shifted, while high weight particles are not moved by much. A Gaussian kernel is appropriate in
      # this case as trends / latent variables are Gaussian random walks, and so this form of kernel captures
      # any dependencies in states
      weight <- norm_weights[x]
      if(weight < weight_thres.1){
        evolve <- kernel_lambda

      } else if(weight < weight_thres.4 &
                weight > weight_thres.1){
        evolve <- 0.75 * kernel_lambda

      } else if(weight < weight_thres.85 &
                weight > weight_thres.4){
        evolve <- 0.5 * kernel_lambda

      } else {
        evolve <- 0.35 * kernel_lambda
      }
      if(weight < weight_thres.85){

        if(use_lv){
          particle_weight <- ifelse(re_weight, 1, tail(particles[[x]]$weight, 1))

          # Smooth latent variable states
          lv_evolve <- unlist(particles[[x]]$lv_states) +
            evolve*(best_lv_means - unlist(particles[[x]]$lv_states)) +
            (lv_draws[x,] * (1 - evolve))

          # Put latent variable states back in list format
          lv_begins <- seq(1, length(lv_evolve), by = 3)
          lv_ends <- lv_begins + 2

          lv_evolve <- lapply(seq_along(particles[[x]]$lv_states), function(lv){
            lv_evolve[lv_begins[lv]:lv_ends[lv]]
          })

          # Smooth latent variable loadings
          lv_coefs_evolve <- as.vector(particles[[x]]$lv_coefs) +
            evolve*(best_lv_coefs_means - particles[[x]]$lv_coefs) +
            (lv_coef_draws[x,] * (1 - evolve))

          trend_evolve <- NULL
          phi_evolve <- particles[[x]]$phi
          ar1_evolve <- particles[[x]]$ar1
          ar2_evolve <- particles[[x]]$ar2
          ar3_evolve <- particles[[x]]$ar3

        } else {
          # Smooth trend states
          trend_evolve <- unlist(particles[[x]]$trend_states) +
            evolve*(best_trend_means - unlist(particles[[x]]$trend_states)) +
            (trend_draws[x,] * (1 - evolve))

          # Put trend states back in list format
          trend_begins <- seq(1, length(trend_evolve), by = 3)
          trend_ends <- trend_begins + 2

          trend_evolve <- lapply(seq_along(particles[[x]]$trend_states), function(trend){
            trend_evolve[trend_begins[trend]:trend_ends[trend]]
          })

          particle_weight <- ifelse(re_weight, 1, tail(particles[[x]]$weight, 1))
          lv_evolve <- NULL
          lv_coefs_evolve <- NULL
          phi_evolve <- particles[[x]]$phi
          ar1_evolve <- particles[[x]]$ar1
          ar2_evolve <- particles[[x]]$ar2
          ar3_evolve <- particles[[x]]$ar3
        }

        # If this is a high weight particle leave it where it is
      } else {

        if(use_lv){
          lv_evolve <- particles[[x]]$lv_states
          lv_coefs_evolve <- particles[[x]]$lv_coefs
          trend_evolve <- NULL
        } else {
          lv_evolve <- NULL
          lv_coefs_evolve <- NULL
          trend_evolve <- particles[[x]]$trend_states

        }
        particle_weight <- ifelse(re_weight, 1, tail(particles[[x]]$weight, 1))
        phi_evolve <- particles[[x]]$phi
        ar1_evolve <- particles[[x]]$ar1
        ar2_evolve <- particles[[x]]$ar2
        ar3_evolve <- particles[[x]]$ar3
      }
    }

    # Return the updated particle, preserving original betas
    betas <- particles[[x]]$betas

    list(use_lv = use_lv,
         n_lv = particles[[x]]$n_lv,
         lv_states = lv_evolve,
         lv_coefs = lv_coefs_evolve,
         betas = betas,
         size = particles[[x]]$size,
         tau = particles[[x]]$tau,
         phi = phi_evolve,
         ar1 = ar1_evolve,
         ar2 = ar2_evolve,
         ar3 = ar3_evolve,
         trend_states = trend_evolve,
         weight = particle_weight,
         upper_bounds = particles[[x]]$upper_bounds,
         last_assim = particles[[x]]$last_assim)

  }, cl = cl)
  stopCluster(cl)

  particles
}
