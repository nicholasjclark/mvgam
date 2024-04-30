#'Argument validation functions
#'@param data Data to be validated (list or dataframe)
#'@noRd
validate_series_time = function(data, name = 'data',
                                trend_model){

  # Series label must be present as a factor and
  # must contain a time variable
  if(inherits(data, 'data.frame')){
    data %>%
      dplyr::ungroup() -> data

    if(!'series' %in% colnames(data)){
      data$series <- factor('series1')
    }

    # Series factor must have all unique levels present
    if(!is.factor(data$series)){
      stop('Variable "series" must be a factor',
           call. = FALSE)
    }

    if(!'time' %in% colnames(data)){
      if(trend_model == 'None'){
        # Add a time indicator if missing
        data %>%
          dplyr::group_by(series) %>%
          dplyr::mutate(time = dplyr::row_number()) %>%
          dplyr::ungroup() -> data
      } else {
        stop(name, " does not contain a 'time' variable",
             call. = FALSE)
      }
    }
  }

  if(inherits(data, 'list')){
    if(!'series' %in% names(data)){
      data$series <- factor('series1')
    }

    # Series factor must have all unique levels present
    if(!is.factor(data$series)){
      stop('Variable "series" must be a factor',
           call. = FALSE)
    }

    if(!'time' %in% names(data)){
      if(trend_model == 'None'){
        # Add a time indicator if missing
        data.frame(series = data$series) %>%
          dplyr::group_by(series) %>%
          dplyr::mutate(time = dplyr::row_number()) %>%
          dplyr::pull(time) -> times
        data$time <- times
      } else {
        stop(name, " does not contain a 'time' variable",
             call. = FALSE)
      }
    }
  }

  # Add a new 'time' variable that will be useful for rearranging data for
  # modeling, in case 'time' is also supplied as a covariate or if this is
  # a continuous time model (CAR1)
  data$index..time..index <- data$time

  # Use the data ordering to set the index of time for CAR1
  if(trend_model == 'CAR1'){
    data.frame(series = data$series,
               time = data$time) %>%
      dplyr::mutate(orig_rows = dplyr::row_number()) %>%
      dplyr::group_by(series) %>%
      dplyr::mutate(idx = dplyr::row_number()) %>%
      dplyr::arrange(time) %>%
      dplyr::mutate(time. = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(orig_rows) %>%
      dplyr::pull(time.) -> times
      data$index..time..index <- times
  }

  # Series factor must have all unique levels present
  if(!all(levels(data$series) %in% unique(data$series))){
    stop(paste0('Mismatch between factor levels of "series" and unique values of "series"',
                '\n',
                'Use\n  `setdiff(levels(data$series), unique(data$series))` \nand',
                '\n',
                '  `intersect(levels(data$series), unique(data$series))`\nfor guidance'),
         call. = FALSE)
  }

  # Ensure each series has an observation, even if NA, for each
  # unique timepoint (only for trend models that require discrete time with
  # regularly spaced sampling intervals)
  all_times_avail = function(time, min_time, max_time){
    identical(as.numeric(sort(time)),
              as.numeric(seq.int(from = min_time, to = max_time)))
  }
  min_time <- as.numeric(min(data$index..time..index))
  max_time <- as.numeric(max(data$index..time..index))
  data.frame(series = data$series,
             time = data$index..time..index) %>%
    dplyr::group_by(series) %>%
    dplyr::summarise(all_there = all_times_avail(time,
                                                 min_time,
                                                 max_time)) -> checked_times
  if(any(checked_times$all_there == FALSE)){
    stop("One or more series in ", name, " is missing observations for one or more timepoints",
         call. = FALSE)
  }

  return(data)
}

#'@importFrom rlang warn
#'@noRd
validate_family = function(family, use_stan = TRUE){
  if(is.character(family)){
    if(family == 'beta')
      family <- betar()

    family <- try(eval(parse(text = family)), silent = TRUE)

    if(inherits(family, 'try-error'))
      stop("family not recognized",
           call. = FALSE)
  }

  if(is.function(family))
    family <- family()

  if(is.null(family$family))
    stop("family not recognized",
         call. = FALSE)

  if(!inherits(family, 'family'))
    stop('family not recognized',
         call. = FALSE)

  if(family$family == 'Beta regression')
    family$family <- 'beta'

  if(family$family == 'tweedie')
    insight::check_if_installed("tweedie",
                                reason = 'to simulate from Tweedie distributions')

  if(!family$family %in% c('poisson', 'negative binomial', 'tweedie') & !use_stan)
    stop('JAGS only supports poisson(), nb() or tweedie() families',
         call. = FALSE)

  # Stan cannot support Tweedie
  if(use_stan & family$family == 'tweedie')
    stop('Tweedie family not supported for stan',
         call. = FALSE)

  if(family$family %in% c('binomial', 'beta_binomial'))
    rlang::warn(paste0("Binomial and Beta-binomial families require cbind(n_successes, n_trials)\nin the formula left-hand side. Do not use cbind(n_successes, n_failures)!"),
                       .frequency = "once", .frequency_id = '1')
  return(family)
}

#'@noRd
validate_family_resrictions = function(response, family){

  response <- response[!is.na(response)]

  # 0s and 1s only for Bernoulli
  if(family$family == 'bernoulli'){
    y <- response
    nobs <- length(response)
    weights <- rep(1, length(response))
    eval(binomial()$initialize)
  }

  # 0s and 1s not allowed for Beta
  if(family$family == 'beta'){
    if(any(response <= 0)){
      stop('Values <= 0 not allowed for beta responses',
           call. = FALSE)
    }
    if(any(response >= 1)){
      stop('Values >= 1 not allowed for beta responses',
           call. = FALSE)
    }
  }

  # negatives not allowed for several families
  if(family$family %in%  c('poisson', 'negative binomial',
                           'tweedie', 'binomial', 'beta_binomial')){
    if(any(response < 0)){
      stop(paste0('Values < 0 not allowed for count family responses'),
           call. = FALSE)
    }
  }

  # negatives and/or zeros not allowed for several families
  if(family$family %in%  c('lognormal', 'Gamma')){
    if(any(response<= 0)){
      stop(paste0('Values <= 0 not allowed for ', family$family, ' responses'),
           call. = FALSE)
    }
  }
}

#'@noRd
validate_trend_model = function(trend_model, drift = FALSE){
  if(inherits(trend_model, 'mvgam_trend')){
    ma_term <- if(trend_model$ma){ 'MA' } else { NULL }
    cor_term <- if(trend_model$cor){ 'cor' } else { NULL }
    trend_model <- paste0(trend_model$trend_model,
                          trend_model$p,
                          ma_term,
                          cor_term)
  }

  trend_model <- match.arg(arg = trend_model,
                           choices = trend_model_choices())

  if(trend_model == 'VAR'){
    trend_model <- 'VAR1'
  }
  if(trend_model == 'VARcor'){
    trend_model <- 'VAR1cor'
  }
  if(trend_model %in% c('VARMA', 'VARMAcor')){
    trend_model <- 'VARMA1,1cor'
  }

  if(trend_model %in% c('VAR','VAR1','VAR1cor','VARMA1,1cor','GP') & drift){
    stop('drift terms not allowed for VAR or GP models',
         call. = FALSE)
  }

  if(trend_model %in% c('PWlinear', 'PWlogistic'))
    insight::check_if_installed("extraDistr",
                                reason = 'to simulate from piecewise trends')
  return(trend_model)
}

#'@noRd
validate_obs_formula = function(formula, data, refit = FALSE){

  if(attr(terms(formula), "response") == 0L){
    stop('response variable is missing from formula',
         call. = FALSE)
  }

  # Check that response terms are in the data; account for possible
  # 'cbind' in there if this is a binomial model
  resp_terms <- as.character(terms(formula(formula))[[2]])
  if(length(resp_terms) == 1){
    out_name <- as.character(terms(formula(formula))[[2]])
    if(!as.character(terms(formula(formula))[[2]]) %in% names(data)){
      stop(paste0('variable ', terms(formula(formula))[[2]], ' not found in data'),
           call. = FALSE)
    }
  } else {
    if(any(grepl('cbind', resp_terms))){
      resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
      out_name <- resp_terms[1]
      for(i in 1:length(resp_terms)){
        if(!resp_terms[i] %in% names(data)){
          stop(paste0('variable ', resp_terms[i], ' not found in data'),
               call. = FALSE)
        }
      }
    } else {
      stop('Not sure how to deal with this response variable specification',
           call. = FALSE)
    }
  }

  if(any(attr(terms(formula), 'term.labels') %in% 'y')){
    stop('due to internal data processing, "y" should not be used as the name of a predictor in mvgam',
         call. = FALSE)
  }

  # Add a y outcome for sending to the modelling backend
  data$y <- data[[out_name]]

  return(data)
}

#'@noRd
validate_trend_formula = function(formula){

  if(!is.null(rlang::f_lhs(formula))){
    stop('Argument "trend_formula" should not have a left-hand side',
         call. = FALSE)
  }

  if(any(grepl('series', as.character(formula)))){
    stop('Argument "trend_formula" should not have the identifier "series" in it.\nUse "trend" instead for varying effects',
         call. = FALSE)
  }

  if(!is.null(attr(terms(formula(formula)), 'offset'))){
    stop('Offsets not allowed in argument "trend_formula"',
         call. = FALSE)
  }

}

#'@noRd
validate_proportional = function(x){
  s <- substitute(x)
  x <- base::suppressWarnings(as.numeric(x))
  if (length(x) != 1L || anyNA(x)) {
    stop("Argument '", s, "' must be a single numeric value",
         call. = FALSE)
  }

  if(x < 0 || x > 1){
    stop("Argument '", s, "' must be a proportion ranging from 0 to 1, inclusive",
         call. = FALSE)
  }
}

#'@noRd
validate_equaldims = function(x, y){
  s <- substitute(x)
  q <- substitute(y)

  if(NCOL(x) != NCOL(y)){
    stop("Argument '", s, "' and argument '", q, "' must have equal dimensions",
         call. = FALSE)
  }

  if(NROW(x) != NROW(y)){
    stop("Argument '", s, "' and argument '", q, "' must have equal dimensions",
         call. = FALSE)
  }
}

#'@noRd
validate_pos_integer = function(x){
  s <- substitute(x)
  x <- base::suppressWarnings(as.numeric(x))
  if (length(x) != 1L || anyNA(x)) {
    stop("Argument '", s, "' must be a single numeric value",
         call. = FALSE)
  }

  if(sign(x) != 1){
    stop("Argument '", s, "' must be a positive integer",
         call. = FALSE)
  } else {
    if(x%%1 != 0){
      stop("Argument '", s, "' must be a positive integer",
           call. = FALSE)
    }
  }
}

#'@noRd
validate_pos_integers = function(x){
  s <- substitute(x)

  val_pos = function(y, s){
    y <- base::suppressWarnings(as.numeric(y))
    if(sign(y) != 1){
      stop("Negative values in ", s, " detected",
           call. = FALSE)
    } else {
      if(y%%1 != 0){
        stop("Non-integer values in ", s, " detected",
             call. = FALSE)
      }
    }
  }
  res <- lapply(seq_along(x), function(i) val_pos(x[i], s))
}

#'@noRd
validate_even <- function(x){
  s <- substitute(x)
  x <- base::suppressWarnings(as.numeric(x))
  if(x %% 2 != 0) {
    stop("Argument '", s, "'  must be an even integer",
         call. = FALSE)
  }
}

#'@noRd
validate_pos_real = function(x){
  s <- substitute(x)
  x <- base::suppressWarnings(as.numeric(x))
  if (length(x) != 1L || anyNA(x)) {
    stop("Argument '", s, "' must be a single numeric value",
         call. = FALSE)
  }

  if(sign(x) != 1){
    stop("Argument '", s, "' must be a positive real value",
         call. = FALSE)
  }
}

#'@noRd
validate_trendmap = function(trend_map,
                             data_train,
                             trend_model,
                             use_stan){

  # Trend mapping not supported by JAGS
  if(!use_stan){
    stop('trend mapping not available for JAGS',
         call. = FALSE)
  }

  # No point in trend mapping if trend model is 'None'
  if(trend_model == 'None'){
    stop('cannot set up latent trends when "trend_model = None"',
         call. = FALSE)
  }

  # trend_map must have an entry for each unique time series
  if(!all(sort(trend_map$series) == sort(unique(data_train$series)))){
    stop('Argument "trend_map" must have an entry for every unique time series in "data"',
         call. = FALSE)
  }

  # trend_map must not specify a greater number of trends than there are series
  if(max(trend_map$trend) > length(unique(data_train$series))){
    stop('Argument "trend_map" specifies more latent trends than there are series in "data"',
         call. = FALSE)
  }

  # trend_map must not skip any trends, but can have zeros for some entries
  drop_zero = function(x) {
    x[x!=0]
  }

  if(!all(drop_zero(sort(unique(trend_map$trend))) == seq(1:max(trend_map$trend)))){
    stop('Argument "trend_map" must link at least one series to each latent trend',
         call. = FALSE)
  }

  # series variable must be a factor with same levels as the series variable
  # in the data
  if(!is.factor(trend_map$series)){
    stop('trend_map$series must be a factor with levels matching levels of data$series',
        call. = FALSE)
  }

  if(!all(levels(trend_map$series) == levels(data_train$series))){
    stop('trend_map$series must be a factor with levels matching levels of data$series',
         call. = FALSE)
  }
}

#'@noRd
validate_trend_restrictions = function(trend_model,
                                       formula,
                                       trend_formula,
                                       trend_map,
                                       drift = FALSE,
                                       drop_obs_intercept = FALSE,
                                       use_lv = FALSE,
                                       n_lv,
                                       data_train,
                                       use_stan = TRUE,
                                       priors = FALSE){

  # Assess whether additional moving average or correlated errors are needed
  ma_cor_adds <- ma_cor_additions(trend_model)
  list2env(ma_cor_adds, envir = environment())

  if(length(unique(data_train$series)) == 1 & add_cor){
    warning('Correlated process errors not possible with only 1 series',
            call. = FALSE)
    add_cor <- FALSE
  }

  # Some checks on general trend setup restrictions
  if(!priors){
    if(trend_model %in% c('PWlinear', 'PWlogistic')){
      if(attr(terms(formula), 'intercept') == 1 & !drop_obs_intercept){
        warning(paste0('It is difficult / impossible to estimate intercepts\n',
                       'and piecewise trend offset parameters. You may want to\n',
                       'consider dropping the intercept from the formula'),
                call. = FALSE)
      }

      if(use_lv) stop('Cannot estimate piecewise trends using dynamic factors',
                      call. = FALSE)
    }
  }

  if(use_lv & (add_ma | add_cor) & missing(trend_formula)){
    stop('Cannot estimate moving averages or correlated errors for dynamic factors',
         call. = FALSE)
  }

  if(drift && use_lv){
    warning('Cannot identify drift terms in latent factor models; setting "drift = FALSE"',
            call. = FALSE)
    drift <- FALSE
  }

  if(use_lv & trend_model == 'VAR1' & missing(trend_formula)){
    stop('Cannot identify dynamic factor models that evolve as VAR processes',
         call. = FALSE)
  }

  if(!use_stan & trend_model %in% c('GP', 'VAR1', 'PWlinear', 'PWlogistic')){
    stop('Gaussian Process, VAR and piecewise trends not supported for JAGS',
         call. = FALSE)
  }

  # Check trend formula and create the trend_map if missing
  if(!missing(trend_formula)){
    validate_trend_formula(trend_formula)
    if(missing(trend_map)){
      trend_map <- data.frame(series = unique(data_train$series),
                              trend = 1:length(unique(data_train$series)))
    }

    if(!trend_model %in% c('RW', 'AR1', 'AR2', 'AR3', 'VAR1', 'CAR1')){
      stop('only RW, AR1, AR2, AR3, CAR1 and VAR trends currently supported for trend predictor models',
           call. = FALSE)
    }
  }

  # Check trend_map is correctly specified
  if(!missing(trend_map)){
    validate_trendmap(trend_map = trend_map, data_train = data_train,
                      trend_model = trend_model, use_stan = use_stan)

    # If trend_map correctly specified, set use_lv to TRUE for
    # most models (but not yet for VAR models, which require additional
    # modifications)
    if(trend_model == 'VAR1'){
      use_lv <- FALSE
    } else {
      use_lv <- TRUE
    }
    n_lv <- max(trend_map$trend)
  }

  # Number of latent variables cannot be greater than number of series
  if(use_lv){
    if(missing(n_lv)){
      n_lv <- min(2, floor(length(unique(data_train$series)) / 2))
    }
    if(n_lv > length(unique(data_train$series))){
      stop('number of latent variables cannot be greater than number of series')
    }
  }

  # No point in latent variables if trend_model is None
  if(trend_model == 'None' & use_lv){
    use_lv <- FALSE
    warning('No point in latent variables if trend model is None; changing use_lv to FALSE')
  }

  if(missing(trend_map)){
    trend_map <- NULL
  }

  if(missing(n_lv)){
    n_lv <- NULL
  }

  return(list(trend_model = trend_model,
              add_cor = add_cor,
              add_ma = add_ma,
              use_var1 = use_var1,
              use_var1cor = use_var1cor,
              use_lv = use_lv,
              n_lv = n_lv,
              trend_map = trend_map))
}

#'@noRd
check_priorsim = function(prior_simulation, data_train, orig_y, formula){
  # Fill y with NAs if this is a simulation from the priors
  if(prior_simulation){
    data_train$y <- rep(NA, length(data_train$y))
  } else {
    data_train$y <- orig_y
  }

  # Fill response variable with original supplied values
  resp_terms <- as.character(terms(formula(formula))[[2]])
  if(length(resp_terms) == 1){
    out_name <- as.character(terms(formula(formula))[[2]])
  } else {
    if(any(grepl('cbind', resp_terms))){
      resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
      out_name <- resp_terms[1]
    }
  }
  data_train[[out_name]] <- orig_y

  return(data_train)
}

#'@noRd
check_gp_terms = function(formula, data_train, family){
  # Check for gp terms in the validated formula
  orig_formula <- gp_terms <- gp_details <- NULL
  if(any(grepl('gp(', attr(terms(formula), 'term.labels'), fixed = TRUE))){

    # Check that there are no multidimensional gp terms
    formula <- interpret_mvgam(formula, N = max(data_train$time),
                               family = family)
    orig_formula <- formula

    # Keep intercept?
    keep_intercept <- attr(terms(formula), 'intercept') == 1

    # Indices of gp() terms in formula
    gp_terms <- which_are_gp(formula)

    # Extract attributes
    gp_details <- get_gp_attributes(formula)

    # Replace with s() terms so the correct terms are included
    # in the model.frame
    formula <- gp_to_s(formula)
    if(!keep_intercept) formula <- update(formula, . ~ . - 1)
  }

  return(list(orig_formula = orig_formula,
              gp_terms = gp_terms,
              formula = formula,
              gp_details = gp_details))
}

#'@noRd
check_obs_intercept = function(formula, orig_formula){
  # Check for missing rhs in formula
  # If there are no terms in the observation formula (i.e. y ~ -1),
  # we will use an intercept-only observation formula and fix
  # the intercept coefficient at zero
  drop_obs_intercept <- FALSE
  if(length(attr(terms(formula), 'term.labels')) == 0 &
     !attr(terms(formula), 'intercept') == 1){
    formula_envir <- attr(formula, '.Environment')

    if(length(attr(terms(formula), 'factors')) == 0){
      resp <- as.character(attr(terms(formula), 'variables'))[2]
    } else {
      resp <- dimnames(attr(terms(formula), 'factors'))[[1]][1]
    }

    if(!is.null(attr(terms(formula(formula)), 'offset'))){
      formula <- formula(paste0(resp, ' ~ ', paste(gsub(' - 1',' + 1',
                                                  rlang::f_text(formula)))))
    } else {
      formula <- formula(paste(resp, '~ 1'))
    }
    attr(formula, '.Environment') <- formula_envir
    drop_obs_intercept <- TRUE
  }

  if(is.null(orig_formula)) orig_formula <- formula

  return(list(orig_formula = orig_formula,
              formula = formula,
              drop_obs_intercept = drop_obs_intercept))
}

#'@noRd
check_nmix = function(family, family_char,
                      trend_formula,
                      trend_model, trend_map,
                      data_train,
                      priors = FALSE){

  # Check for N-mixture modifications
  add_nmix <- FALSE; nmix_trendmap <- TRUE
  if(family_char == 'nmix'){
    if(!(exists('cap', where = data_train))) {
      stop('Max abundances must be supplied as a variable named "cap" for N-mixture models',
           call. = FALSE)
    }

    add_nmix <- TRUE
    if(!priors){
      family <- poisson(); family_char <- 'poisson'
      if(missing(trend_formula)){
        stop('Argument "trend_formula" required for nmix models',
             call. = FALSE)
      }
    }

    if(!missing(trend_map)){
      nmix_trendmap <- TRUE
    }
    use_lv <- TRUE
    if(trend_model == 'None'){
      trend_model <- 'RW'
    }
  }

  return(list(trend_model = trend_model,
              add_nmix = add_nmix,
              nmix_trendmap = nmix_trendmap,
              family = family,
              family_char = family_char))
}

#'@noRd
validate_threads = function(family_char, threads){
  if(threads > 1 & !family_char %in% c('poisson', 'negative binomial', 'gaussian')){
    warning('multithreading not yet supported for this family; setting threads = 1')
    threads <- 1
  }
  return(threads)
}

#'@noRd
find_jags = function(jags_path){
  if(!requireNamespace('runjags', quietly = TRUE)){
    stop('runjags library is required but not found',
         call. = FALSE)
  }

  if(missing(jags_path)){
    requireNamespace('runjags', quietly = TRUE)
    jags_path <- runjags::findjags()
  }

  # Code borrowed from the runjags package
  jags_status <- runjags::testjags(jags_path, silent = TRUE)
  if(!jags_status$JAGS.available){
    if(jags_status$os == "windows"){
      Sys.sleep(0.2)
      jags_status <- runjags::testjags(jags_path, silent = TRUE)
    }

    if(!jags_status$JAGS.available){
      cat("Unable to call JAGS using '", jags_path,
          "'\nTry specifying the path to the JAGS binary as jags_path argument, or re-installing the rjags package.\nUse the runjags::testjags() function for more detailed diagnostics.\n", sep="")
      stop("Unable to call JAGS.\nEither use the Stan backend or follow examples in ?mvgam to generate data / model files and run outside of mvgam", call. = FALSE)
    }
  }
}

#'@noRd
find_stan = function(){
  if(!requireNamespace('rstan', quietly = TRUE)){
    warning('rstan library not found; checking for cmdstanr library')

    if(!requireNamespace('cmdstanr', quietly = TRUE)){
      stop('cmdstanr library not found',
           call. = FALSE)
    }
  }
}
