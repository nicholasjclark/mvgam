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

cat("\nFamily:\n")
cat(paste0(object$family, '\n'))

cat("\nLink function:\n")
cat(paste0(family_links(object$family), '\n'))


cat("\nTrend model:\n")
if(inherits(object$trend_model, 'mvgam_trend')){
  print(object$trend_model$label)
} else {
  cat(paste0(object$trend_model, '\n'))
}

if(object$use_lv){
  cat("N latent factors:\n")
  cat(object$n_lv, '\n')

}

cat('\nN series:\n')
cat(NCOL(object$ytimes), '\n')


if(!is.null(object$upper_bounds)){
  cat('\nUpper bounds:\n')
  cat(object$upper_bounds, '\n')

}

cat('\nN timepoints:\n')
cat(NROW(object$ytimes), '\n')


if(object$fit_engine == 'jags'){
  cat('\nStatus:\n')
  cat('Fitted using JAGS', '\n')
}

if(object$fit_engine == 'stan'){
  cat('\nStatus:\n')
  cat('Fitted using Stan', '\n')

  n_kept <- object$model_output@sim$n_save - object$model_output@sim$warmup2
  cat(object$model_output@sim$chains, " chains, each with iter = ",
      object$model_output@sim$iter,
      "; warmup = ", object$model_output@sim$warmup, "; thin = ",
      object$model_output@sim$thin, " \n",
      "Total post-warmup draws = ", sum(n_kept), "\n\n", sep = '')

}

}


#'@export
print.mvgam_prefit = function(x, ...){

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

  cat("\nFamily:\n")
  cat(paste0(object$family, '\n'))

  cat("\nLink function:\n")
  cat(paste0(family_links(object$family), '\n'))


  cat("\nTrend model:\n")
  if(inherits(object$trend_model, 'mvgam_trend')){
    print(object$trend_model$label)
  } else {
    cat(paste0(object$trend_model, '\n'))
  }



  if(object$use_lv){
    cat("\nN latent factors:\n")
    cat(object$n_lv, '\n')

  }

  cat('\nN series:\n')
  cat(NCOL(object$ytimes), '\n')


  cat('\nN timepoints:\n')
  cat(NROW(object$ytimes), '\n')


  cat('\nStatus:\n')
  cat('Not fitted', '\n')

}
