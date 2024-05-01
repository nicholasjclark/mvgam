#' Specify piecewise linear or logistic trends
#'
#' Set up piecewise linear or logistic trend models
#' in \code{mvgam}. These functions do not evaluate their arguments –
#' they exist purely to help set up a model with particular piecewise
#' trend models.
#' @param n_changepoints A non-negative integer specifying the number of potential
#' changepoints. Potential changepoints are selected uniformly from the
#' first `changepoint_range` proportion of timepoints in \code{data}. Default is `10`
#' @param changepoint_range Proportion of history in \code{data} in which trend changepoints
#'  will be estimated. Defaults to 0.8 for the first 80%.
#' @param changepoint_scale Parameter modulating the flexibility of the
#'   automatic changepoint selection by altering the scale parameter of a Laplace distribution.
#'   The resulting prior will be `double_exponential(0, changepoint_scale)`.
#'   Large values will allow many changepoints and a more flexible trend, while
#'   small values will allow few changepoints. Default is `0.05`.
#' @param growth Character string specifying either 'linear' or 'logistic' growth of
#' the trend. If 'logistic', a variable labelled `cap` MUST be in \code{data} to specify the
#' maximum saturation point for the trend (see details and examples in \code{\link{mvgam}} for
#' more information).
#' Default is 'linear'.
#' @references Taylor, Sean J., and Benjamin Letham. "Forecasting at scale." The American Statistician 72.1 (2018): 37-45.
#' @return An object of class \code{mvgam_trend}, which contains a list of
#' arguments to be interpreted by the parsing functions in \code{mvgam}
#' @details
#'*Offsets and intercepts*:
#' For each of these trend models, an offset parameter is included in the trend
#' estimation process. This parameter will be incredibly difficult to identify
#' if you also include an intercept in the observation formula. For that reason,
#' it is highly recommended that you drop the intercept from the formula
#' (i.e. `y ~ x + 0` or `y ~ x - 1`, where `x` are your optional predictor terms).
#'\cr
#'\cr
#'*Logistic growth and the cap variable*:
#' When forecasting growth, there is often some maximum achievable point that
#' a time series can reach. For example, total market size, total population size
#' or carrying capacity in population dynamics. It can be advantageous for the forecast
#' to saturate at or near this point so that predictions are more sensible.
#' This function allows you to make forecasts using a logistic growth trend model,
#' with a specified carrying capacity. Note that this capacity does not need to be static
#' over time, it can vary with each series x timepoint combination if necessary. But you
#' must supply a `cap` value for each observation in the data when using `growth = 'logistic'`.
#' For observation families that use a non-identity link function, the `cap` value will
#' be internally transformed to the link scale (i.e. your specified `cap` will be log
#' transformed if you are using a `poisson()` or `nb()` family). It is therefore important
#' that you specify the `cap` values on the scale of your outcome. Note also that
#' no missing values are allowed in `cap`.
#'
#' @rdname piecewise_trends
#' @examples
#' \dontrun{
#' # Example of logistic growth with possible changepoints
#' # Simple logistic growth model
#' dNt = function(r, N, k){
#'    r * N * (k - N)
#' }
#'
#' # Iterate growth through time
#' Nt = function(r, N, t, k) {
#' for (i in 1:(t - 1)) {
#'
#'  # population at next time step is current population + growth,
#'  # but we introduce several 'shocks' as changepoints
#'  if(i %in% c(5, 15, 25, 41, 45, 60, 80)){
#'    N[i + 1] <- max(1, N[i] + dNt(r + runif(1, -0.1, 0.1),
#'                                  N[i], k))
#'    } else {
#'    N[i + 1] <- max(1, N[i] + dNt(r, N[i], k))
#'    }
#'   }
#'  N
#' }
#'
#' # Simulate expected values
#' set.seed(11)
#' expected <- Nt(0.004, 2, 100, 30)
#' plot(expected, xlab = 'Time')
#'
#' # Take Poisson draws
#' y <- rpois(100, expected)
#' plot(y, xlab = 'Time')
#'
#' # Assemble data into dataframe and model. We set a
#' # fixed carrying capacity of 35 for this example, but note that
#' # this value is not required to be fixed at each timepoint
#' mod_data <- data.frame(y = y,
#'                        time = 1:100,
#'                        cap = 35,
#'                        series = as.factor('series_1'))
#' plot_mvgam_series(data = mod_data)
#'
#' # The intercept is nonidentifiable when using piecewise
#' # trends because the trend functions have their own offset
#' # parameters 'm'; it is recommended to always drop intercepts
#' # when using these trend models
#' mod <- mvgam(y ~ 0,
#'              trend_model = PW(growth = 'logistic'),
#'              family = poisson(),
#'              data = mod_data,
#'              chains = 2)
#' summary(mod)
#'
#' # Plot the posterior hindcast
#' plot(mod, type = 'forecast')
#'
#' # View the changepoints with ggplot2 utilities
#' library(ggplot2)
#' mcmc_plot(mod, variable = 'delta_trend',
#'           regex = TRUE) +
#' scale_y_discrete(labels = mod$trend_model$changepoints) +
#' labs(y = 'Potential changepoint',
#'      x = 'Rate change')
#' }
#' @export
PW = function(n_changepoints = 10,
              changepoint_range = 0.8,
              changepoint_scale = 0.05,
              growth = 'linear'){

  growth <- match.arg(growth, choices = c('linear', 'logistic'))
  validate_proportional(changepoint_range)
  validate_pos_integer(n_changepoints)
  validate_pos_real(changepoint_scale)

  trend_model <- 'PWlinear'
  if(growth == 'logistic'){
    trend_model = 'PWlogistic'
  }
  out <- structure(list(trend_model = trend_model,
                        n_changepoints = n_changepoints,
                        changepoint_range = changepoint_range,
                        changepoint_scale = changepoint_scale,
                        ma = FALSE,
                        cor = FALSE,
                        label = match.call()),
                   class = 'mvgam_trend')
}

#' Updates for adding piecewise trends
#' @noRd
add_piecewise = function(model_file,
                         model_data,
                         data_train,
                         data_test = NULL,
                         orig_trend_model,
                         family){

  trend_model <- orig_trend_model$trend_model
  n_changepoints <- orig_trend_model$n_changepoints
  changepoint_range <- orig_trend_model$changepoint_range
  changepoint_scale <- orig_trend_model$changepoint_scale

  if(family$family == 'Gamma'){
    family <- Gamma(link = 'log')
  }

  if(trend_model == 'PWlogistic'){
    if(!(exists('cap', where = data_train))) {
      stop('Capacities must be supplied as a variable named "cap" for logistic growth',
           call. = FALSE)
    }

    if(any(is.na(data_train$cap))){
      stop('Missing values found for some "cap" terms',
           call. = FALSE)
    }

    if(any(is.infinite(data_train$cap))){
      stop('Infinite values found for some "cap" terms',
           call. = FALSE)
    }

    # Matrix of capacities per series (these must operate on the link scale)
    all_caps <- data.frame(series = as.numeric(data_train$series),
                           time = data_train$time,
                           cap = suppressWarnings(linkfun(data_train$cap,
                                                          link = family$link))) %>%
      dplyr::arrange(time, series)

    if(any(is.na(all_caps$cap)) | any(is.infinite(all_caps$cap))){
      stop(paste0('Missing or infinite values found for some "cap" terms\n',
                  'after transforming to the ',
                  family$link, ' link scale'),
           call. = FALSE)
    }

    if(!is.null(data_test)){
      if(!(exists('cap', where = data_test))) {
        stop('Capacities must also be supplied in "newdata" for logistic growth predictions',
             call. = FALSE)
      }

      all_caps <- rbind(all_caps,
                        data.frame(series = as.numeric(data_test$series),
                                   time = data_test$time,
                                   cap = suppressWarnings(linkfun(data_test$cap,
                                                                  link = family$link)))) %>%
        dplyr::arrange(time, series)

      if(any(is.na(all_caps$cap)) | any(is.infinite(all_caps$cap))){
        stop(paste0('Missing or infinite values found for some "cap" terms\n',
                    'after transforming to the ',
                    family$link, ' link scale'),
             call. = FALSE)
      }
    }

    cap <- matrix(NA, nrow = length(unique(all_caps$time)),
                  ncol = length(unique(all_caps$series)))
    for(i in 1:length(unique(all_caps$series))){
      cap[,i] <- all_caps$cap[which(all_caps$series == i)]
    }
  } else {
    cap <- NULL
  }

  #### Distribute possible changepoints ####
  scaled_time <- unique(data_train$time - min(data_train$time) + 1)
  max_time <- max(scaled_time)
  hist_size <- floor(max_time * changepoint_range)
  t_change <- unique(round(seq.int(1, hist_size,
                                   length.out = (n_changepoints + 1))[-1]))
  n_changepoints <- length(t_change)
  change_freq <- n_changepoints / hist_size

  if(!is.null(data_test)){
    # Get forecast horizon changepoints
    # This can go in with the data if newdata is supplied; else it needs
    # to be used when extrapolating the trend forward
    n_new_changes <- stats::rpois(1, (change_freq *
                                    (max(data_test$time) -
                                       min(data_test$time))))

    # Spread the forecast changepoints evenly across the forecast
    # horizon
    scaled_test_time <- unique(data_test$time - min(data_train$time) + 1)
    t_change_new <- unique(floor(seq.int(min(scaled_test_time),
                                         max(scaled_test_time),
                                         length.out = n_new_changes)))
    t_change <- c(t_change, t_change_new)
    n_changepoints <- n_changepoints + n_new_changes
    scaled_time <- c(scaled_time, scaled_test_time)
  }

  # Add changepoint info to the data
  model_data$n_changepoints <- n_changepoints
  model_data$change_freq <- change_freq
  model_data$t_change <- t_change
  model_data$time <- scaled_time
  model_data$changepoint_scale <- changepoint_scale
  model_data$cap <- cap

  #### Update the model file appropriately ####
  # Add the piecewise functions
  if(any(grepl('functions {', model_file, fixed = TRUE))){
    model_file[grep('functions {', model_file, fixed = TRUE)] <-
      paste0('functions {\n',
             'matrix get_changepoint_matrix(vector t, vector t_change, int T, int S) {\n',
             '/* Function to sort changepoints */\n',
             '/* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/\n',
             'matrix[T, S] A;\n',
             'row_vector[S] a_row;\n',
             'int cp_idx;\n',
             'A = rep_matrix(0, T, S);\n',
             'a_row = rep_row_vector(0, S);\n',
             'cp_idx = 1;\n',
             'for (i in 1:T) {\n',
             'while ((cp_idx <= S) && (t[i] >= t_change[cp_idx])) {\n',
             'a_row[cp_idx] = 1;\n',
             'cp_idx = cp_idx + 1;\n',
             '}\n',
             'A[i] = a_row;\n',
             '}\n',
             'return A;\n',
             '}\n',

             '// logistic trend functions\n',
             'vector logistic_gamma(real k, real m, vector delta, vector t_change, int S) {\n',
             '/* Function to compute a logistic trend with changepoints */\n',
             '/* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/\n',
             'vector[S] gamma;  // adjusted offsets, for piecewise continuity\n',
             'vector[S + 1] k_s;  // actual rate in each segment\n',
             'real m_pr;\n',
             'k_s = append_row(k, k + cumulative_sum(delta));\n',
             'm_pr = m; // The offset in the previous segment\n',
             'for (i in 1:S) {\n',
             'gamma[i] = (t_change[i] - m_pr) * (1 - k_s[i] / k_s[i + 1]);\n',
             'm_pr = m_pr + gamma[i];  // update for the next segment\n',
             '}\n',
             'return gamma;\n',
             '}\n',

             'vector logistic_trend(\n',
             'real k,\n',
             'real m,\n',
             'vector delta,\n',
             'vector t,\n',
             'vector cap,\n',
             'matrix A,\n',
             'vector t_change,\n',
             'int S\n',
             ') {\n',
             '/* Function to adjust a logistic trend using a carrying capacity */\n',
             '/* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/\n',
             'vector[S] gamma;\n',
             'gamma = logistic_gamma(k, m, delta, t_change, S);\n',
             'return cap .* inv_logit((k + A * delta) .* (t - (m + A * gamma)));\n',
             '}\n',

             '// linear trend function\n',
             '/* Function to compute a linear trend with changepoints */\n',
             '/* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/\n',
             'vector linear_trend(\n',
             'real k,\n',
             'real m,\n',
             'vector delta,\n',
             'vector t,\n',
             'matrix A,\n',
             'vector t_change\n',
             ') {\n',
             'return (k + A * delta) .* t + (m + A * (-t_change .* delta));\n',
             '}\n')

  } else {
    model_file[grep('Stan model code', model_file)] <-
      paste0('// Stan model code generated by package mvgam\n',
             'functions {\n',
             'matrix get_changepoint_matrix(vector t, vector t_change, int T, int S) {\n',
             '/* Function to sort changepoints */\n',
             '/* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/\n',
             'matrix[T, S] A;\n',
             'row_vector[S] a_row;\n',
             'int cp_idx;\n',
             'A = rep_matrix(0, T, S);\n',
             'a_row = rep_row_vector(0, S);\n',
             'cp_idx = 1;\n',
             'for (i in 1:T) {\n',
             'while ((cp_idx <= S) && (t[i] >= t_change[cp_idx])) {\n',
             'a_row[cp_idx] = 1;\n',
             'cp_idx = cp_idx + 1;\n',
             '}\n',
             'A[i] = a_row;\n',
             '}\n',
             'return A;\n',
             '}\n',

             '// logistic trend functions\n',
             'vector logistic_gamma(real k, real m, vector delta, vector t_change, int S) {\n',
             '/* Function to compute a logistic trend with changepoints */\n',
             '/* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/\n',
             'vector[S] gamma;  // adjusted offsets, for piecewise continuity\n',
             'vector[S + 1] k_s;  // actual rate in each segment\n',
             'real m_pr;\n',
             'k_s = append_row(k, k + cumulative_sum(delta));\n',
             'm_pr = m; // The offset in the previous segment\n',
             'for (i in 1:S) {\n',
             'gamma[i] = (t_change[i] - m_pr) * (1 - k_s[i] / k_s[i + 1]);\n',
             'm_pr = m_pr + gamma[i];  // update for the next segment\n',
             '}\n',
             'return gamma;\n',
             '}\n',

             'vector logistic_trend(\n',
             'real k,\n',
             'real m,\n',
             'vector delta,\n',
             'vector t,\n',
             'vector cap,\n',
             'matrix A,\n',
             'vector t_change,\n',
             'int S\n',
             ') {\n',
             '/* Function to adjust a logistic trend using a carrying capacity */\n',
             '/* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/\n',
             'vector[S] gamma;\n',
             'gamma = logistic_gamma(k, m, delta, t_change, S);\n',
             'return cap .* inv_logit((k + A * delta) .* (t - (m + A * gamma)));\n',
             '}\n',

             '// linear trend function\n',
             '/* Function to compute a linear trend with changepoints */\n',
             '/* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/\n',
             'vector linear_trend(\n',
             'real k,\n',
             'real m,\n',
             'vector delta,\n',
             'vector t,\n',
             'matrix A,\n',
             'vector t_change\n',
             ') {\n',
             'return (k + A * delta) .* t + (m + A * (-t_change .* delta));\n',
             '}\n}\n')
  }

  # Update the data block
  model_file[grep('int<lower=0> num_basis;', model_file, fixed = TRUE)] <-
    paste0("int<lower=0> num_basis; // total number of basis coefficients\n",
           "vector[n] time; // index of time for changepoint model\n",
           "int n_changepoints; // number of potential trend changepoints\n",
           "vector[n_changepoints] t_change; // times of potential changepoints\n",
           if(trend_model == 'PWlogistic'){
             "matrix[n, n_series] cap; // carrying capacities for logistic trends\n"
           } else {
             NULL
           },
           'real changepoint_scale; // scale of changepoint shock prior\n')
  model_file <- readLines(textConnection(model_file), n = -1)

  # Update the transformed data block
  if(any(grepl('transformed data {', model_file, fixed = TRUE))){
    model_file[grep('transformed data {', model_file, fixed = TRUE)] <-
      paste0('transformed data {\n',
             '// sorted changepoint matrix\n',
             'matrix[n, n_changepoints] A = get_changepoint_matrix(time, t_change, n, n_changepoints);\n')
  } else {
    model_file[grep('parameters {', model_file, fixed = TRUE)[1]] <-
      paste0('transformed data {\n',
             '// sorted changepoint matrix\n',
             'matrix[n, n_changepoints] A = get_changepoint_matrix(time, t_change, n, n_changepoints);\n',
             '}\nparameters {')
  }
  model_file <- readLines(textConnection(model_file), n = -1)

  # Update the parameters block
  model_file <- model_file[-c(grep('// latent trend variance parameters',
                                 model_file, fixed = TRUE):(grep('// latent trend variance parameters',
                                                                 model_file, fixed = TRUE) + 1))]
  model_file <- model_file[-c(grep('// latent trends',
                                   model_file, fixed = TRUE):(grep('// latent trends',
                                                                   model_file, fixed = TRUE) + 1))]
  model_file[grep("vector[num_basis] b_raw;", model_file, fixed = TRUE)] <-
    paste0("vector[num_basis] b_raw;\n",
           "// base trend growth rates\n",
           "vector[n_series] k_trend;\n\n",
           "// trend offset parameters\n",
           "vector[n_series] m_trend;\n\n",
           "// trend rate adjustments per series\n",
           "matrix[n_changepoints, n_series] delta_trend;\n")
  model_file <- readLines(textConnection(model_file), n = -1)

  # Update the transformed parameters block
  model_file[grep("transformed parameters {", model_file, fixed = TRUE)] <-
    paste0("transformed parameters {\n",
           "// latent trends\n",
           "matrix[n, n_series] trend;\n")

  max_rawline <- max(grep('= b_raw', model_file))
  model_file[max_rawline] <- paste0(
    model_file[max_rawline],
    '\n\n',
    '// trend estimates\n',
    'for (s in 1 : n_series) {\n',
    if(trend_model == 'PWlogistic'){
      'trend[1 : n, s] = logistic_trend(k_trend[s], m_trend[s], to_vector(delta_trend[,s]), time, to_vector(cap[,s]), A, t_change, n_changepoints);\n'
    } else {
      'trend[1 : n, s] = linear_trend(k_trend[s], m_trend[s], to_vector(delta_trend[,s]), time, A, t_change);\n'
    },

    '}\n')
  model_file <- readLines(textConnection(model_file), n = -1)

  # Update the model block
  model_file <- model_file[-c(grep('// priors for latent trend variance parameters',
                                   model_file, fixed = TRUE):(grep('// priors for latent trend variance parameters',
                                                                   model_file, fixed = TRUE) + 1))]
  rw_start <- grep("trend[1, 1:n_series] ~ normal(0, sigma);", model_file,
                   fixed = TRUE)
  rw_lines <- (rw_start - 1):(rw_start + 3)
  model_file <- model_file[-rw_lines]
  model_file[grep("// likelihood functions", model_file, fixed = TRUE) - 1] <-
    paste0('// trend parameter priors\n',
           'm_trend ~ student_t(3, 0, 2.5);\n',
           'k_trend ~ std_normal();\n',
           'to_vector(delta_trend) ~ double_exponential(0, changepoint_scale);\n',
           model_file[grep("// likelihood functions", model_file, fixed = TRUE) - 1])
  model_file <- readLines(textConnection(model_file), n = -1)

  # Update the generated quantities block
  model_file <- model_file[-grep("vector[n_series] tau;", model_file, fixed = TRUE)]
  tau_start <- grep("tau[s] = pow(sigma[s], -2.0);", model_file, fixed = TRUE) - 1
  model_file <- model_file[-c(tau_start:(tau_start + 2))]
  model_file <- readLines(textConnection(model_file), n = -1)

  #### Return ####
  return(list(model_file = model_file,
              model_data = model_data))
}
