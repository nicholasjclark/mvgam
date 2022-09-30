#' Update priors for a JAGS or Stan model file
#'
#'
#' @param model_file Prepared mvgam model file
#' @param priors \code{data.frame} with prior definitions (in JAGS or Stan syntax)
#' @return A `character string` containing the updated model file
#' @export
update_priors = function(model_file,
                         priors){

  # Check the prior df structure
  if(!any(class(priors) == 'data.frame')){
    stop('priors must be a data.frame with at least the colnames: param_name, prior')
  }

  if(!'prior' %in% names(priors)){
    stop('priors must be a data.frame with at least the colnames: param_name, prior')
  }

  if(!'param_name' %in% names(priors)){
    stop('priors must be a data.frame with at least the colnames: param_name, prior')
  }

  # Modify the file to update the prior definitions
  for(i in 1:NROW(priors)){
    if(!any(grepl(paste(trimws(strsplit(priors$prior[i], "[~]")[[1]][1]), '~'),
         model_file, fixed = TRUE))){
      warning('no match found in model_file for parameter: ',
              trimws(strsplit(priors$prior[i], "[~]")[[1]][1]))
    } else {
      model_file[grep(paste(trimws(strsplit(priors$prior[i], "[~]")[[1]][1]), '~'),
                      model_file, fixed = TRUE)] <-
        priors$prior[i]
    }
  }

  return(model_file)
}
