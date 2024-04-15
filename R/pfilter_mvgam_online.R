#'Automatic online particle filtering for assimilating new observations into a fitted mvgam model
#'
#'This function operates sequentially on new observations in \code{data_assim} to update the
#'posterior forecast distribution. It is a wrapper that calls \code{\link{pfilter_mvgam_smooth}}.
#'In each iteration, the next observation is assimilated
#'and particles are weighted by their proposal's multivariate composite likelihood
#'
#'@param newdata A \code{dataframe} or \code{list} of test data containing at least one more observation per series
#'(beyond the last observation seen by the model when initialising particles with
#' \code{\link{pfilter_mvgam_init}} or in previous calls to \code{pfilter_mvgam_online}.
#'Should at least contain 'series' and 'time' for the one-step ahead horizon,
#'in addition to any other variables included in the linear predictor of \code{object}
#'@param data_assim Deprecated. Still works in place of \code{newdata} but users are recommended to use
#'\code{newdata} instead for more seamless integration into `R` workflows
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
#' @noRd
pfilter_mvgam_online = function(newdata,
                                data_assim,
                                file_path = 'pfilter',
                                threshold = 0.5,
                                use_resampling = FALSE,
                                kernel_lambda = 0.25,
                                n_cores = 1){

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

  # Load the particles and key objects for tracking last assimilation dates
  if(file.exists(paste0(file_path, '/particles.rda'))){
    load(paste0(file_path, '/particles.rda'))
  } else {
    stop('file_path either does not exist or does not contain a .rda particle list')
  }

  # Get next observations in line to be assimilated
  # Ensure outcome is labelled 'y' when feeding data to the model for simplicity
  if(terms(formula(mod_call))[[2]] != 'y'){
    data_assim$y <- data_assim[[terms(formula(mod_call))[[2]]]]
  }

  if(class(data_assim)[1] == 'list'){

    if(!'series' %in% names(data_assim)){
      data_assim$series <- factor('series1')
    }

    n_series <- length(unique(obs_data$series))
    all_needed_names <- names(obs_data)

    # Find indices of next observation
    data_assim_orig <- data_assim
    list_names <- names(data_assim_orig)
    data_assim = data.frame(time = data_assim$time,
                            series = data_assim$series) %>%
      dplyr::mutate(index = dplyr::row_number()) %>%
      dplyr::arrange(time, series) %>%
      dplyr::mutate(assimilated = dplyr::case_when(
        time <= last_assim ~ 'yes',
        TRUE ~ 'no'
      ))

    temp_dat = data.frame(time = data_assim$time,
                          series = data_assim$series) %>%
      dplyr::mutate(index = dplyr::row_number()) %>%
      dplyr::arrange(time, series)
    indices_assim <- temp_dat[1:n_series,'index']

    # Get list object into correct order in case it is not already
    data_assim_orig <- lapply(data_assim_orig, function(x){
      if(is.matrix(x)){
        matrix(x[temp_dat$index,], ncol = NCOL(x))
      } else {
        x[temp_dat$index]
      }

    })
    names(data_assim_orig) <- list_names
    data_assim_orig$assimilated <- data_assim$assimilated

  } else {

    if(!'time' %in% colnames(data_assim)){
      stop('data_assim does not contain a "time" column')
    }

    if(!'series' %in% colnames(data_assim)){
      data_assim$series <- factor('series1')
    }
    data_assim %>%
      dplyr::arrange(time, series) -> data_assim
    data_assim %>%
      dplyr::mutate(assimilated = dplyr::case_when(
        time <= last_assim ~ 'yes',
        TRUE ~ 'no'
      )) -> data_assim
  }

  # Stop if all observations already assimilated
  if(all(data_assim$assimilated == 'yes')){
    cat('All observations in data_assim have already been assimilated')

  } else {

    if(any(data_assim$assimilated == 'yes')){
      cat('Particles have already assimilated one or more observations. Skipping these\n\n')
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
    if(class(obs_data)[1] == 'list'){

      indices_assim <- (data_assim %>%
                          dplyr::arrange(time, series))[starts[i]:ends[i],'index']

      # Get list object into correct format for lpmatrix prediction
      next_assim <- lapply(data_assim_orig, function(x){
        if(is.matrix(x)){
          matrix(x[indices_assim,], ncol = NCOL(x))
        } else {
          x[indices_assim]
        }

      })

    } else {
      next_assim <- (data_assim %>%
                       dplyr::arrange(time, series))[starts[i]:ends[i],]
    }
        # Run particle filter with kernel smoothing
        particles <- pfilter_mvgam_smooth(particles = particles,
                                          mgcv_model = mgcv_model,
                                          next_assim = next_assim,
                                          threshold = threshold,
                                          use_resampling = use_resampling,
                                          kernel_lambda = kernel_lambda,
                                          n_cores = n_cores)

        # Update observation history with last assimilations
        if(class(obs_data)[1] == 'list'){
          data_assim_orig$assimilated[starts[i]:ends[i]] <- 'yes'
          obs_data <- lapply(seq_along(obs_data), function(x){

            if(is.matrix(obs_data[[x]])){
              rbind(obs_data[[x]], data_assim_orig[[x]][starts[i]:ends[i],])
            } else {
              if(is.factor(obs_data[[x]])){
                factor(unlist(list(obs_data[[x]], data_assim_orig[[x]][starts[i]:ends[i]])),
                       levels = levels(obs_data[[x]]))
              } else {
              c(obs_data[[x]], data_assim_orig[[x]][starts[i]:ends[i]])
              }
            }
          })

          names(obs_data) <- all_needed_names
        } else {
          data_assim$assimilated[starts[i]:ends[i]] <- 'yes'
          obs_data %>%
            dplyr::bind_rows(data_assim[starts[i]:ends[i],]) -> obs_data
        }

  }
  last_assim <- tail(obs_data$time, 1)
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
           mod_call,
           ess, file = paste0(file_path, '/particles.rda'))

    }
}
