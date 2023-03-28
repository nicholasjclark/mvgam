#'Summary for a fitted mvgam object
#'
#'This function takes a fitted \code{mvgam} object and prints a quick summary
#'
#'@param object \code{list} object returned from \code{mvgam}
#'@author Nicholas J Clark
#'@details A brief summary of the model's call is printed
#'@return A \code{list} is printed on-screen
#'@export
print.mvgam = function(object){
jam = object$jam_model

# Convert stanfit objects to coda samples
if(class(object$model_output) == 'stanfit'){
  object$model_output <- coda::mcmc.list(lapply(1:NCOL(object$model_output),
                                                function(x) coda::mcmc(as.array(object$model_output)[,x,])))
}

message("GAM formula:")
print(object$call)
message()

message("Family:")
cat(paste0(object$family, '\n'))
message()

message("Link function:")
cat(paste0(family_links(object$family), '\n'))
message()

message("Trend model:")
cat(paste0(object$trend_model, '\n'))
message()

if(object$use_lv){
  message("N latent factors:")
  cat(object$n_lv, '\n')
  message()
}

message('N series:')
cat(NCOL(object$ytimes), '\n')
message()

if(!is.null(object$upper_bounds)){
  message('Upper bounds:')
  cat(object$upper_bounds, '\n')
  message()
}

message('N observations:')
if(class(object$obs_data)[1] == 'list'){
  cat(length(object$obs_data$y), '\n')
} else {
  cat(NROW(object$obs_data), '\n')
}
message()

if(object$fit_engine == 'jags'){
  message('Status:')
  cat('Fitted using JAGS', '\n')
  message()
}

if(object$fit_engine == 'stan'){
  message('Status:')
  cat('Fitted using Stan', '\n')
  message()
}

if(object$family == 'Negative Binomial'){
  message("Dispersion parameter estimates:")
  print(MCMCvis::MCMCsummary(object$model_output, 'phi')[,c(3:7)])
  message()
}

if(object$family == 'Tweedie'){
  message("Dispersion parameter estimates:")
  print(MCMCvis::MCMCsummary(object$model_output, 'phi')[,c(3:7)])
  message()
}

message('Total estimated degrees of freedom:')
cat(sum(jam$edf), '\n')
message()
}


#'@export
print.mvgam_prefit = function(object){
  jam = object$jam_model

  message("GAM formula:")
  print(object$call)
  message()

  message("Family:")
  cat(paste0(object$family, '\n'))
  message()

  message("Link function:")
  cat(paste0(family_links(object$family), '\n'))
  message()

  message("Trend model:")
  cat(paste0(object$trend_model, '\n'))
  message()

  if(object$use_lv){
    message("N latent factors:")
    cat(object$n_lv, '\n')
    message()
  }

  message('N series:')
  cat(NCOL(object$ytimes), '\n')
  message()

  message('N observations:')
  if(class(object$obs_data)[1] == 'list'){
    cat(length(object$obs_data$y), '\n')
  } else {
    cat(NROW(object$obs_data), '\n')
  }
  message()

  message('Status:')
  cat('Not fitted', '\n')
  message()
}
