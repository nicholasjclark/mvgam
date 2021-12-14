#'Automatic online particle filtering for assimilating new observations into a fitted mvgam model
#'
#'This function operates sequentially on new observations in \code{data_assim} to update the
#'posterior forecast distribution. It is a wrapper that calls \code{\link{pfilter_mvgam_smooth}}.
#'In each iteration, the next observation is assimilated
#'and particles are weighted by their proposal's multivariate composite likelihood
#'
#'@param data_assim A \code{dataframe} of test data containing at least one more observation per series
#'(beyond the last observation seen by the model when initialising particles with
#' \code{\link{pfilter_mvgam_init}} or in previous calls to \code{pfilter_mvgam_online}.
#'Should at least contain 'series', 'season', 'year' and in_season' for the one-step ahead horizon,
#'in addition to any other variables included in the linear predictor of \code{object}
#'@param threshold \code{proportional numeric} specifying the Effective Sample Size limit under which
#'resampling of particles will be triggered (calculated as \code{ESS / n_particles}) if \code{use_resampling == TRUE}.
#'Should be between \code{0} and \code{1}
#'@param use_resampling \code{logical} specifying whether resampling should be used when ESS falls below
#'the specified \code{threshold}. Default for this option is \code{FALSE}, relying instead on kernel smoothing only
#'to maintain particle diversity
#'@param kernel_lambda \code{proportional numeric} specifying the strength of kernel smoothing to use when
#'pulling low weight particles toward the high likelihood state space. Should be between \code{0} and \code{1}
#'@param file_path \code{character} string specifying the file path for locating the particles
#'@param n_cores \code{integer} specifying number of cores for generating particle forecasts in parallel
#'@return A \code{list} object of \code{length = n_particles} containing information on parameters and
#'current state estimates for each particle is generated and saved, along with other important information
#'from the original model, to an \code{.rda} object in \code{file_path}
#'@export
pfilter_mvgam_online = function(data_assim,
                                file_path = 'pfilter',
                                threshold = 0.5,
                                use_resampling = FALSE,
                                kernel_lambda = 1,
                                n_cores = parallel::detectCores() - 1){

  # Load the particles and key objects for tracking last assimilation dates
  if(file.exists(paste0(file_path, '/particles.rda'))){
    load(paste0(file_path, '/particles.rda'))
  } else {
    stop('file_path either does not exist or does not contain a .rda particle list')
  }

  # Get next observations in line to be assimilated
  data_assim %>%
    dplyr::arrange(year, season, series) -> data_assim
  data_assim %>%
    dplyr::mutate(assimilated = dplyr::case_when(
      season <= last_assim[1] & year <= last_assim[2] ~ 'yes',
      TRUE ~ 'no'
      )) -> data_assim

    if(any(data_assim$assimilated == 'yes')){
      cat('Particles have already assimilated one or more observations. Skipping these\n\n')
    }
    if(all(data_assim$assimilated == 'no')){
      stop('All observations in data_assim have already been assimilated')
    }

  # Assimilate full set of observations using a for loop
  cat('Assimilating the next', NROW(data_assim %>%
                                      dplyr::filter(assimilated == 'no')) /
        length(unique(data_assim$series)), 'observations\n\n')

  starts <- seq(min(which(data_assim$assimilated == 'no')), NROW(data_assim),
                        by = length(unique(data_assim$series)))
  ends <- starts + (length(unique(data_assim$series)) - 1)
  for(i in seq_len(NROW(data_assim %>%
                  dplyr::filter(assimilated == 'no')) / length(unique(data_assim$series)))){

        # Get next set of observations for assimilation (one observation per series)
        next_assim <- (data_assim %>%
          dplyr::arrange(year, season, series))[starts[i]:ends[i],]

        # Run particle filter with kernel smoothing
        particles <- pfilter_mvgam_smooth(particles = particles,
                                          mgcv_model = mgcv_model,
                                          next_assim = next_assim,
                                          threshold = threshold,
                                          use_resampling = use_resampling,
                                          kernel_lambda = kernel_lambda,
                                          n_cores = n_cores)

        # Update observation history with last assimilations
        data_assim$assimilated[starts[i]:ends[i]] <- 'yes'
        obs_data %>%
          dplyr::bind_rows(data_assim[starts[i]:ends[i],]) -> obs_data
  }
  last_assim <- c(tail(obs_data$season, 1),
                tail(obs_data$year, 1))
      cat('Last assimilation time was', last_assim, '\n\n')

      # Keep track of effective sample size
      weights <- (unlist(lapply(seq_along(particles), function(x){
        tail(particles[[x]]$weight, 1)})))
      weights <- weights / sum(weights)
      new_ess <- 1 / sum(weights^2)
      ess <- c(ess, new_ess)

      cat('Saving particles to', paste0(file_path, '/particles.rda'), '\n',
          'ESS =',  new_ess, '\n')
      save(particles, mgcv_model, obs_data, last_assim,
           ess = ess, file = paste0(file_path, '/particles.rda'))

}
