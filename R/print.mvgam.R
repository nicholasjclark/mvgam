#'Summary for a fitted mvjagam object
#'
#'This function takes a fitted \code{mvjagam} object and prints a quick summary
#'
#'@param object \code{list} object returned from \code{mvjagam}
#'@author Nicholas J Clark
#'@details A brief summary of the model's call is printed
#'@return A \code{list} is printed on-screen
#'@export
print.mvgam = function(object){
jam = object$jam_model

message("GAM formula:")
print(object$call)
message()

message("Family:")
cat(paste0(object$family, '\n'))
message()

message("Link function:")
cat(paste0('log', '\n'))
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
cat('Fitted using runjags::run.jags()', '\n')
message()

if(object$family == 'Negative Binomial'){
  message("Dispersion parameter estimates:")
  print(MCMCvis::MCMCsummary(object$model_output, 'r')[,c(3:7)])
  message()
}

if(object$family == 'Tweedie'){
  message("Dispersion parameter estimates:")
  print(MCMCvis::MCMCsummary(object$model_output, 'twdis')[,c(3:7)])
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
  cat(paste0('log', '\n'))
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
