#'Argument validation functions
#'@param data Data to be validated (list or dataframe)
#'@noRd
validate_series_time = function(data, name = 'data'){

  # Series label must be present as a factor and
  # must contain a time variable
  if(inherits(data, 'data.frame')){
    if(!'series' %in% colnames(data)){
      data$series <- factor('series1')
    }
    if(!'time' %in% colnames(data)){
      stop(name, " does not contain a 'time' variable",
           call. = FALSE)
    }
  }

  if(inherits(data, 'list')){
    if(!'series' %in% names(data)){
      data$series <- factor('series1')
    }
    if(!'time' %in% names(data)){
      stop(name, " does not contain a 'time' variable",
           call. = FALSE)
    }
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
  # unique timepoint
  all_times_avail = function(time, min_time, max_time){
    identical(as.numeric(sort(time)),
              as.numeric(seq.int(from = min_time, to = max_time)))
  }
  min_time <- min(data$time)
  max_time <- max(data$time)
  data.frame(series = data$series,
             time = data$time) %>%
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

#'@noRd
validate_family = function(family){
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

  return(family)
}

#'@noRd
validate_family_resrictions = function(response, family){

  response <- response[!is.na(response)]

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
                           'tweedie')){
    if(any(response < 0)){
      stop(paste0('Values < 0 not allowed for ', family$family, ' responses'),
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

  return(trend_model)
}

#'@noRd
validate_obs_formula = function(formula, data, refit = FALSE){

  if(attr(terms(formula), "response") == 0L){
    stop('response variable is missing from formula',
         call. = FALSE)
  }

  if(!as.character(terms(formula(formula))[[2]]) %in% names(data)){
    stop(paste0('variable ', terms(formula(formula))[[2]], ' not found in data'),
         call. = FALSE)
  }

  if(any(attr(terms(formula), 'term.labels') %in% 'y')){
    stop('due to internal data processing, "y" should not be used as the name of a predictor in mvgam',
         call. = FALSE)
  }

  # Add a y outcome for sending to the modelling backend
  data$y <- data[[terms(formula(formula))[[2]]]]

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
