#'Assimilate new observations into a fitted mvgam model using resampling and kernel smoothing
#'
#'This function operates on a new observation in \code{next_assim} to update the
#'posterior forecast distribution. The next observation is assimilated
#'and particle weights are updated in light of their most recent their multivariate composite likelihood.
#'Low weight particles are smoothed towards the high weight state space using importance sampling, and options are
#'given for using resampling of high weight particles when Effective Sample Size falls below a
#'user-specified threshold
#'
#'@importFrom parallel clusterExport stopCluster setDefaultCluster clusterEvalQ makePSOCKcluster
#'@importFrom stats predict
#'@param particles A \code{list} of particles that have been run up to one observation prior to the observation
#'in \code{next_assim}
#'@param mgcv_model A \code{\link[mgcv]{gam}} model returned through a call to \code{link{mvgam}}
#'@param next_assim A \code{dataframe} of test data containing at one more observation per series
#'(beyond the last observation seen by the model when initialising particles with
#' \code{\link{pfilter_mvgam_init}} or in previous calls to \code{pfilter_mvgam_online}.
#'Should at least contain 'series' and 'time' for the one-step ahead horizon,
#'in addition to any other variables included in the linear predictor of \code{object}
#'@param threshold \code{proportional numeric} specifying the Effective Sample Size limit under which
#'resampling of particles will be triggered (calculated as \code{ESS / n_particles}) if \code{use_resampling == TRUE}.
#'Should be between \code{0} and \code{1}
#'@param use_resampling \code{logical} specifying whether resampling should be used when ESS falls below
#'the specified \code{threshold}. Note that resampling can result in loss of the original model's diversity of
#'GAM beta coefficients, which may have undesirable consequences for the forecast distribution. If
#'\code{use_resampling} is \code{TRUE}, some effort is made to remedy this by assigning randomly sampled draws of
#'GAM beta coefficients from the original model's distribution to each particle. This does not however guarantee there
#'will be no loss of diversity, especially if successive resampling take place. Default for this option is therefore
#'\code{FALSE}
#'@param kernel_lambda \code{proportional numeric} specifying the strength of smoothing to use when
#'pulling low weight particles toward the high likelihood state space. Should be between \code{0} and \code{1}
#'@param n_cores \code{integer} specifying number of cores for generating particle forecasts in parallel
#'@return A \code{list} object of \code{length = n_particles} containing information on parameters and
#'current state estimates for each particle
#'@export
pfilter_mvgam_smooth = function(particles,
                                mgcv_model,
                                next_assim,
                                threshold = 0.25,
                                n_cores = 1,
                                use_resampling = FALSE,
                                kernel_lambda = 0.5){

  validate_proportional(threshold)
  validate_proportional(kernel_lambda)
  validate_pos_integer(n_cores)

  # GP models smooth towards the entire state history, so they are more likely
  # to 'overreact' to changing conditions than are AR or RW trend models. Set the
  # kernel_lambda to be lower as a result
  if(particles[[1]]$trend_model == 'GP'){
    kernel_lambda <- 0.5 * kernel_lambda
  }

  # Linear predictor matrix for the next observation
  Xp <- obs_Xp_matrix(newdata = next_assim,
                      mgcv_model = object$mgcv_model)

  use_lv <- particles[[1]]$use_lv
  last_assim <- unique(next_assim$time)

  # Update importance weights in light of most recent observation by running particles
  # up to the current timepoint
  cl <- makePSOCKcluster(n_cores)
  setDefaultCluster(cl)
  clusterExport(NULL, c('Xp',
                        'use_lv',
                        'particles',
                        'next_assim'),
                envir = environment())
  clusterExport(cl = cl,
                unclass(lsf.str(envir = asNamespace("mvgam"),
                                all = T)),
                envir = as.environment(asNamespace("mvgam"))
  )

  pbapply::pboptions(type = "none")
  particles <- pbapply::pblapply(seq_along(particles), function(x){
    n_series <- length(particles[[x]]$n_series)

    if(use_lv){

      # Run the latent variables forward one timestep
      lvs <- forecast_trend(trend_model = particles[[x]]$trend_model,
                                       use_lv = TRUE,
                                       trend_pars = particles[[x]]$trend_pars,
                                       h = 1)

      # Include previous states for each lv
      if(particles[[x]]$trend_model != 'GP'){
        particles[[x]]$trend_pars$last_lvs <- lapply(seq_along(particles[[x]]$trend_pars$last_lvs),
                                              function(x){
                                                c(particles[[x]]$trend_pars$last_lvs[[x]][2],
                                                  particles[[x]]$trend_pars$last_lvs[[x]][3],
                                                  lvs[x])
                                              })
      } else {
        particles[[x]]$trend_pars$last_lvs <- lapply(seq_along(particles[[x]]$trend_pars$last_lvs),
                                              function(x){
                                                c(particles[[x]]$trend_pars[[x]],
                                                  lvs[x])
                                              })
      }

      # Multiply lv states with loadings to generate each series' forecast trend state
      trend_states <- as.numeric(lvs %*% t(do.call(rbind, particles[[x]]$trend_pars$lv_coefs)))

      names(particles[[x]]$trend_pars$last_lvs) <- paste0('lv',
                                                          seq_along(particles[[x]]$trend_pars$last_lvs))

    } else {

      # Loop across series and produce the next trend estimate
      trend_states <- unlist(lapply(seq_along(particles[[x]]$trend_pars$last_trends),
                                             function(series){

        # Series-specific trend parameters
        trend_extracts <- lapply(seq_along(particles[[x]]$trend_pars), function(j){

          if(names(particles[[x]]$trend_pars)[j] == 'last_trends'){
            out <- particles[[x]]$trend_pars[[j]][[series]]
          } else {
            out <- particles[[x]]$trend_pars[[j]]
          }
          out

        })
        names(trend_extracts) <- names(particles[[x]]$trend_pars)

        # Propagate the series-specific trends forward
        out <- forecast_trend(trend_model = particles[[x]]$trend_model,
                                        use_lv = FALSE,
                                        trend_pars = trend_extracts,
                                        h = 1)
        out
      }))

      # Include necessary previous states for each trend
      if(particles[[x]]$trend_model != 'GP'){
        particles[[x]]$trend_pars$last_trends <- unname(lapply(seq_along(particles[[x]]$trend_pars$last_trends),
                                                        function(x){
                                                          unname(c(particles[[x]]$trend_pars$last_trends[[x]][2],
                                                                   particles[[x]]$trend_pars$last_trends[[x]][3],
                                                                   trend_states[x]))
                                                        }))
      } else {
        particles[[x]]$trend_pars$last_trends <- unname(lapply(seq_along(particles[[x]]$trend_pars$last_trends),
                                                        function(x){
                                                          unname(c(particles[[x]]$trend_pars$last_trends[[x]],
                                                                   trend_states[x]))
                                                        }))
      }

      names(particles[[x]]$trend_pars$last_trends) <- paste0('series', seq_len(n_series))
    }

   particle_liks <- unlist(lapply(seq_len(n_series), function(series){

    if(is.na(next_assim$y[series])){
      series_weight <- 1
    } else {

      # Series-specific linear predictor matrix
      Xpmat <- t(rbind(as.matrix(Xp[which(as.numeric(next_assim$series) == series),]),
                       trend_states[series]))
      attr(Xpmat, 'model.offset') <- attr(Xp, 'model.offset')

      # Series-specific family parameters
      # Family-specific parameters
      family_extracts <- lapply(seq_along(particles[[x]]$family_pars), function(j){
        if(is.matrix(particles[[x]]$family_pars[[j]])){
          particles[[x]]$family_pars[[j]][, series]
        } else if(length(particles[[x]]$family_pars[[j]]) == particles[[x]]$n_series){
          particles[[x]]$family_pars[[j]][series]
        } else {
          particles[[x]]$family_pars[[j]]
        }
      })
      names(family_extracts) <- names(particles[[x]]$family_pars)

      # Likelihood
      series_weight <- exp(mvgam_predict(family = particles[[x]]$family,
                                             family_pars = family_extracts,
                                             truth = next_assim$y[series],
                                             Xp = Xpmat,
                                             betas = c(particles[[x]]$betas, 1),
                                             density = TRUE))

    }
    series_weight
  }))
   particle_weight <- prod(particle_liks, na.rm = T)

   # Update particle weight using a condensation algorithm
   weight <- particle_weight * particles[[x]]$weight

   list(n_series = particles[[x]]$n_series,
        use_lv = use_lv,
        family = particles[[x]]$family,
        trend_model = particles[[x]]$trend_model,
        betas = particles[[x]]$betas,
        family_pars = particles[[x]]$family_pars,
        trend_pars = particles[[x]]$trend_pars,
        weight = particle_weight,
        liks = particle_liks,
        upper_bounds = particles[[x]]$upper_bounds,
        last_assim = unique(next_assim$time))

  }, cl = cl)
  stopCluster(cl)

  weights <- (unlist(lapply(seq_along(particles), function(x){
    tail(particles[[x]]$weight, 1)})))

  # Initial thresholding, keeping only top 80% of weights, often leads to minor
  # improvements in particle filter tracking
  weights_keep <- sample(sort.int(weights, decreasing = TRUE,
                             index.return = TRUE)$ix[1:floor(length(weights) / 1.2)],
                  length(weights),
                  replace = T)

  # Functions for Pareto smoothing of remaining importance weights
  # (All credit for these functions go to the maintainers and developers
  # of the loo package: https://github.com/stan-dev/loo/tree/master/R)
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
    w_theta <- exp(l_theta - log_sum_exp(l_theta)) # normalize
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
                                  tail_len_i = length(weights) / 4)$log_weights
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
    re_weight <- FALSE
    use_resampling <- FALSE
  } else {
    # Else calculate normalised weights and effective sample size; resample if ESS falls below
    # threshold and if the user has specified to use_resampling
    norm_weights <- weights / sum(weights)
    ess <- 1 / sum(norm_weights^2)
    cat('Effective sample size is', ess, '...\n\n')
    if(ess - (length(norm_weights) * threshold) < 0){
      next_update_seq <- index
      re_weight <- TRUE
    } else {
      next_update_seq <- seq(1:length(weights))
      re_weight <- F
      use_resampling <- FALSE
    }
  }

  # If resampling is not specified by user, keep the current full set of particles and only
  # use kernel smoothing
  if(!use_resampling){
    re_weight <- T
    next_update_seq <- seq(1:length(weights))
    orig_betas <- NULL
  } else {
    # If resampling, must preserve the original diversity of GAM beta coefficients
    cat('Resampling particles ...\n')
    next_update_seq <- index
    orig_betas <- do.call(rbind, purrr::map(particles, 'betas'))
  }

  # Function to use importance sampling to generate draws from each trend state's
  # highest likelihood distribution; this is useful as the high likelihood
  # space will not necessarily be multivariate Gaussian but we want to smooth
  # low weight particles toward this distribution
  imp_samp = function(x, N = 500){
    # Draw from proposal distribution which is normal(mu, sd = 1)
    mean_x <- mean(x)
    sam <- rnorm(N, mean_x, 1)

    # Weight the sample using ratio of target and proposal densities
    bw <- density(x)$bw
    w <- sapply(sam, function(input) sum(dnorm(input, mean = x, sd = bw)) /
                  dnorm(input, mean_x, 1))

    # Resample according to the importance weights
    sample(sam, length(particles), replace = TRUE, prob = w)

  }

  if(length(unique(index)) > 100){
    use_smoothing <- TRUE
    cat('Smoothing particles ...\n\n')

    if(use_lv){
      # Extract draws from high-likelihood state spaces for kernel smoothing
      best_lv <- do.call(rbind, lapply(index, function(j){
        unlist(particles[[j]]$trend_pars$last_lvs)
        }))

      best_lv_coefs <- lapply(index, function(j){
        do.call(rbind, particles[[j]]$trend_pars$lv_coefs)
      })
      best_lv_coefs <- do.call(rbind, lapply(seq_along(best_lv_coefs), function(x){
        as.vector(best_lv_coefs[[x]])
      }))

      lv_draws <- apply(best_lv, 2, imp_samp)
      lv_coef_draws <- apply(best_lv_coefs, 2, imp_samp)

      trend_draws <- NULL
      rm(best_lv, best_lv_coefs)

    } else {
      best_trend <- do.call(rbind, lapply(index, function(j){
        unlist(particles[[j]]$trend_pars$last_trends)
      }))

      trend_draws <- apply(best_trend, 2, imp_samp)

      lv_draws <- NULL
      lv_coef_draws <- NULL
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

  cl <- makePSOCKcluster(n_cores)
  setDefaultCluster(cl)
  clusterExport(NULL, c('use_smoothing',
                        'use_resampling',
                        'orig_betas',
                        'lv_draws',
                        'lv_coef_draws',
                        'trend_draws',
                        'use_lv',
                        'ess',
                        'norm_weights',
                        'particles',
                        'kernel_lambda',
                        're_weight',
                        'norm_weights'),
                envir = environment())

  clusterEvalQ(cl, library(MASS))
  pbapply::pboptions(type = "none")

  # Perform kernel smoothing and mutation of particles. Argument next_update_seq is determined by whether
  # or not the user wants to resample and what the ESS currently is
  particles <- pbapply::pblapply(next_update_seq, function(x){

    # No major changes when smoothing not in effect as all weights are very similar
    if(!use_smoothing){

      evolve <- 0
      particle_weight <- ifelse(re_weight, 1, tail(particles[[x]]$weight, 1))

    } else {

      # Else particles can be kernel smooothed towards higher likelihood space.
      # For kernel smoothing, how much a particle may be pulled towards the high-likelihood
      # space is determined  by its last fitness estimate (weight)
      # Particles with low weight are pulled more strongly towards the
      # state space of the high weight particles. Particles with moderate weights are only moderatly
      # shifted, while high weight particles are not moved by much
      weight <- norm_weights[x]
      evolve <- kernel_lambda *
        (1 - (length(which(norm_weights < weight)) / length(norm_weights)))

        if(use_lv){
          particle_weight <- ifelse(re_weight, 1, tail(particles[[x]]$weight, 1))

          # Smooth latent variable states
          lv_evolve <- unlist(particles[[x]]$trend_pars$last_lvs) +
            evolve*(lv_draws[x,] - unlist(particles[[x]]$trend_pars$last_lvs))

          # Put latent variable states back in list format
          if(particles[[x]]$trend_model == 'GP'){
            lv_begins <- seq(1,
                                length(lv_evolve),
                                by = length(lv_evolve) / length(particles[[x]]$trend_pars$last_lvs))
            lv_ends <- (lv_begins - 1) +
              (length(lv_evolve) / length(particles[[x]]$trend_pars$last_lvs))
          } else {
            lv_begins <- seq(1, length(lv_evolve), by = 3)
            lv_ends <- lv_begins + 2
          }

          particles[[x]]$trend_pars$last_lvs <- lapply(seq_along(
            particles[[x]]$trend_pars$last_lvs), function(lv){
            lv_evolve[lv_begins[lv]:lv_ends[lv]]
          })

          # Smooth latent variable loadings
          lv_coefs_evolve <- matrix(as.vector(do.call(rbind,
                                                      particles[[x]]$trend_pars$lv_coefs)) +
            evolve*(lv_coef_draws[x,] - as.vector(do.call(rbind, particles[[x]]$trend_pars$lv_coefs))),
            nrow = length(particles[[x]]$trend_pars$lv_coefs))

          # Ensure upper triangle stays at zero to respect initial constraints
          n_lv <- NCOL(lv_coefs_evolve)
          for (i in 1:(n_lv - 1)) {
            for (j in (i + 1):(n_lv)){
              lv_coefs_evolve[i, j] = 0;
            }
          }

          # Put back into list format
          particles[[x]]$trend_pars$lv_coefs <- lapply(seq_along(particles[[x]]$trend_pars$lv_coefs),
                                                       function(x){
                                                         lv_coefs_evolve[x,]
                                                       })

        } else {
          # Smooth trend states
          trend_evolve <- unlist(particles[[x]]$trend_pars$last_trends) +
            evolve*(trend_draws[x,] - unlist(particles[[x]]$trend_pars$last_trends))

          # Put trend states back in list format
          if(particles[[x]]$trend_model == 'GP'){
            trend_begins <- seq(1,
                                length(trend_evolve),
                                by = length(trend_evolve) / length(particles[[x]]$trend_pars$last_trends))
            trend_ends <- (trend_begins - 1) +
              (length(trend_evolve) / length(particles[[x]]$trend_pars$last_trends))
          } else {
            trend_begins <- seq(1, length(trend_evolve), by = 3)
            trend_ends <- trend_begins + 2
          }

          particles[[x]]$trend_pars$last_trends <- lapply(seq_along(particles[[x]]$trend_pars$last_trends),
                                                          function(trend){
            trend_evolve[trend_begins[trend]:trend_ends[trend]]
          })

          particle_weight <- ifelse(re_weight, 1, tail(particles[[x]]$weight, 1))
          lv_evolve <- NULL
          lv_coefs_evolve <- NULL
        }
    }

    # Return the updated particle, preserving original betas
    if(use_resampling){
      betas <- orig_betas[x,]
    } else {
      betas <- particles[[x]]$betas
    }

    list(use_lv = use_lv,
         n_series = particles[[x]]$n_series,
         family = particles[[x]]$family,
         trend_model = particles[[x]]$trend_model,
         betas = betas,
         family_pars = particles[[x]]$family_pars,
         trend_pars = particles[[x]]$trend_pars,
         liks = particles[[x]]$liks,
         weight = particle_weight,
         upper_bounds = particles[[x]]$upper_bounds,
         last_assim = particles[[x]]$last_assim)

  }, cl = cl)
  stopCluster(cl)

  particles
}
