#' Update priors for a JAGS or Stan model file
#'
#'
#' @param model_file Prepared mvgam model file
#' @param priors \code{data.frame} with prior definitions (in JAGS or Stan syntax)
#' @param use_stan \code{logical}. Only Stan models can have parameter bounds edited
#' @return A `character string` containing the updated model file
#' @noRd
update_priors = function(model_file,
                         priors,
                         use_stan){

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

      if(grepl('num_gp_basis', priors$prior[i])){
        model_file[grep('num_gp_basis = min(20, n);', model_file, fixed = TRUE)] <-
          priors$prior[i]

      } else {
        warning('no match found in model_file for parameter: ',
                trimws(strsplit(priors$prior[i], "[~]")[[1]][1]))
      }

    } else {
      model_file[grep(paste(trimws(strsplit(priors$prior[i], "[~]")[[1]][1]), '~'),
                      model_file, fixed = TRUE)] <-
        priors$prior[i]
    }
  }


  # Modify the file to update any bounds on parameters
  if(use_stan){
    if(any(!is.na(c(priors$new_lowerbound, priors$new_upperbound)))){
      for(i in 1:NROW(priors)){

        # Create boundary text strings
        if(!is.na(priors$new_lowerbound[i])){
          change_lower <- TRUE
          lower_text <- paste0('lower=',
                               priors$new_lowerbound[i])
        } else {
          if(grepl('lower=', priors$param_name[i])){
            change_lower <- TRUE
            lower_text <-
              paste0('lower=',
                     regmatches(priors$param_name[i],
                                regexpr("lower=.*?\\K-?\\d+",
                                        priors$param_name[i], perl=TRUE)))
          } else {
            change_lower <- FALSE
          }
        }

        if(!is.na(priors$new_upperbound[i])){
          change_upper <- TRUE
          upper_text <- paste0('upper=',
                               priors$new_upperbound[i])
        } else {
          if(grepl('upper=', priors$param_name[i])){
            change_upper <- TRUE
            upper_text <-
              paste0('upper=',
                     regmatches(priors$param_name[i],
                                regexpr("upper=.*?\\K-?\\d+",
                                        priors$param_name[i], perl=TRUE)))
          } else {
            change_upper <- FALSE
          }
        }

        # Insert changes
        if(change_lower & change_upper){
          model_file[grep(trimws(priors$param_name[i]),
                          model_file, fixed = TRUE)] <-
            ifelse(!grepl('<', priors$param_name[i]),
                   sub('\\[', paste0('<',
                                     lower_text,
                                     ',',
                                     upper_text,
                                     '>\\['),
                       priors$param_name[i]),
                   sub("<[^\\)]+>",
                       paste0('<',
                              lower_text,
                              ',',
                              upper_text,
                              '>'),
                       priors$param_name[i]))
        }

        if(change_lower & !change_upper){
          model_file[grep(trimws(priors$param_name[i]),
                          model_file, fixed = TRUE)] <-

            ifelse(!grepl('<', priors$param_name[i]),
                   sub('\\[', paste0('<',
                                     lower_text,
                                     '>\\['),
                       priors$param_name[i]),
                   sub("<[^\\)]+>",
                       paste0('<',
                              lower_text,
                              '>'),
                       priors$param_name[i]))
        }

        if(change_upper & !change_lower){
          model_file[grep(trimws(priors$param_name[i]),
                          model_file, fixed = TRUE)] <-
            ifelse(!grepl('<', priors$param_name[i]),
                   sub('\\[', paste0('<',
                                     upper_text,
                                     '>\\['),
                       priors$param_name[i]),
                   sub("<[^\\)]+>",
                       paste0('<',
                              upper_text,
                              '>'),
                       priors$param_name[i]))
        }
        change_lower <- FALSE
        change_upper <- FALSE
      }
    }
  }

  return(model_file)
}
