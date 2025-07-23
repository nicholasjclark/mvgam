#' Print a fitted \pkg{mvgam} object
#'
#' This function takes a fitted \code{mvgam} or \code{jsdgam} object and prints
#' a quick summary.
#'
#' @param x \code{list} object returned from \code{mvgam}
#'
#' @param ... Ignored
#'
#' @details A brief summary of the model's call is printed
#'
#' @return A \code{list} is printed on-screen
#'
#' @author Nicholas J Clark
#'
#' @export
print.mvgam = function(x, ...) {
  object <- x
  
  # Use shared extractor functions to eliminate code duplication
  model_spec <- extract_model_spec(object)
  sampling_info <- extract_sampling_info(object)
  
  # Print model specification with simplified logic for print.mvgam
  print_model_specification_simple(model_spec)
  
  # Print sampling information using shared helper  
  print_sampling_information(sampling_info)
}

#' Print model specification section for print.mvgam (simplified version)
#' @param model_spec Model specification from extract_model_spec
#' @noRd
print_model_specification_simple <- function(model_spec) {
  # Print formulas
  if (!is.null(model_spec$formulas$process)) {
    cat("GAM observation formula:\n")
    print(model_spec$formulas$observation)
    cat("\nGAM process formula:\n") 
    print(model_spec$formulas$process)
  } else {
    cat("GAM formula:\n")
    print(model_spec$formulas$observation)
  }
  
  # Print family and link
  cat("\nFamily:\n")
  cat(paste0(model_spec$family, '\n'))
  
  cat("\nLink function:\n")
  cat(paste0(model_spec$link, '\n'))
  
  # Print trend model
  if (!model_spec$is_jsdgam) {
    cat("\nTrend model:\n")
    if (is.call(model_spec$trend_model)) {
      print(model_spec$trend_model)
      cat('\n')
    } else {
      cat(paste0(model_spec$trend_model, '\n'))
    }
  }
  
  # Print latent variable info (simplified - always "latent factors" for print.mvgam)
  if (!is.null(model_spec$latent_variables)) {
    cat("\nN latent factors:\n")
    cat(model_spec$latent_variables$count, '\n')
  }
  
  # Print dimensions
  if (model_spec$is_jsdgam) {
    cat('\nN species:\n')
    cat(model_spec$dimensions$n_species, '\n')
    cat('\nN sites:\n')
    cat(model_spec$dimensions$n_sites, '\n')
  } else {
    cat('\nN series:\n')
    cat(model_spec$dimensions$n_series, '\n')
    cat('\nN timepoints:\n')
    cat(model_spec$dimensions$n_timepoints, '\n')
  }
  
  # Print upper bounds if present
  if (!is.null(model_spec$upper_bounds)) {
    cat('\nUpper bounds:\n')
    cat(model_spec$upper_bounds, '\n')
  }
}


#'@export
print.mvgam_prefit = function(x, ...) {
  object <- x
  
  # Use shared extractor function for model specification
  model_spec <- extract_model_spec(object)
  
  # Print model specification using shared helper
  print_model_specification(model_spec)
  
  # Add prefit-specific status message
  cat('\nStatus:\n')
  cat('Not fitted', '\n')
}
