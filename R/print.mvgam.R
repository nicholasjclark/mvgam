#'Summary for a fitted mvgam object
#'
#'This function takes a fitted \code{mvgam} object and prints a quick summary
#'
#'@param x \code{list} object returned from \code{mvgam}
#'@param ... Ignored
#'@author Nicholas J Clark
#'@details A brief summary of the model's call is printed
#'@return A \code{list} is printed on-screen
#'@export
print.mvgam = function(x, ...){

object <- x

if(!is.null(object$trend_call)){
  cat("GAM observation formula:\n")
  print(object$call)

  cat("\nGAM process formula:\n")
  print(object$trend_call)
} else {
  cat("GAM formula:\n")
  print(object$call)
}

cat("Family:\n")
cat(paste0(object$family, '\n'))


cat("Link function:\n")
cat(paste0(family_links(object$family), '\n'))


cat("Trend model:\n")
cat(paste0(object$trend_model, '\n'))


if(object$use_lv){
  cat("N latent factors:\n")
  cat(object$n_lv, '\n')

}

cat('N series:\n')
cat(NCOL(object$ytimes), '\n')


if(!is.null(object$upper_bounds)){
  cat('Upper bounds:\n')
  cat(object$upper_bounds, '\n')

}

cat('N observations:\n')
if(class(object$obs_data)[1] == 'list'){
  cat(length(object$obs_data$y), '\n')
} else {
  cat(NROW(object$obs_data), '\n')
}


if(object$fit_engine == 'jags'){
  cat('Status:\n')
  cat('Fitted using JAGS', '\n')

}

if(object$fit_engine == 'stan'){
  cat('Status:\n')
  cat('Fitted using Stan', '\n')

}

}


#'@export
print.mvgam_prefit = function(x, ...){

  object <- x

  cat("GAM formula:\n")
  print(object$call)


  cat("Family:\n")
  cat(paste0(object$family, '\n'))


  cat("Link function:\n")
  cat(paste0(family_links(object$family), '\n'))


  cat("Trend model:\n")
  cat(paste0(object$trend_model, '\n'))


  if(object$use_lv){
    cat("N latent factors:\n")
    cat(object$n_lv, '\n')

  }

  cat('N series:\n')
  cat(NCOL(object$ytimes), '\n')


  cat('N observations:\n')
  if(class(object$obs_data)[1] == 'list'){
    cat(length(object$obs_data$y), '\n')
  } else {
    cat(NROW(object$obs_data), '\n')
  }


  cat('Status:\n')
  cat('Not fitted', '\n')

}
