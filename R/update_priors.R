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

  # Replace any call to 'Intercept' with '(Intercept)' to match mgcv style
  priors[] <- lapply(priors, function(x)
    gsub("Intercept(?!.*[^()]*\\))", "(Intercept)", x,
         perl = TRUE))

  if(!is.null(attr(priors, 'posterior_to_prior'))){

    model_file <- posterior_to_prior(model_file, priors)

  } else {
    # Modify the file to update the prior definitions
    for(i in 1:NROW(priors)){

      # gp() terms can be supplied using more mgcv-like syntax; replace
      # with the uglier syntax that is used in the Stan code so the prior
      # can be correctly updated
      if(grepl('gp(', priors$prior[i], fixed = TRUE) |
         grepl('gp_trend', priors$prior[i], fixed = TRUE)){
        lhs <- trimws(strsplit(priors$prior[i], "[~]")[[1]][1])
        rhs <- trimws(strsplit(priors$prior[i], "[~]")[[1]][2])
        lhs <- gsub('(', '_', lhs, fixed = TRUE)
        lhs <- gsub(')', '_', lhs, fixed = TRUE)
        lhs <- gsub(':', 'by', lhs, fixed = TRUE)
        priors$prior[i] <- paste(lhs, '~', rhs)
      }

      if(!any(grepl(paste(trimws(strsplit(priors$prior[i], "[~]")[[1]][1]), '~'),
                    model_file, fixed = TRUE))){

        # Updating parametric effects
        if(any(grepl(paste0('// prior for ', priors$param_name[i], '...'), model_file, fixed = TRUE))){
          header_line <- grep(paste0('// prior for ', priors$param_name[i], '...'), model_file, fixed = TRUE)
          newprior <- paste(trimws(strsplit(priors$prior[i], "[~]")[[1]][2]))
          model_file[header_line + 1] <-
            paste(trimws(strsplit(model_file[header_line + 1], "[~]")[[1]][1]), '~',
                  newprior)

        } else if(grepl('num_gp_basis', priors$prior[i])){
          model_file[grep('num_gp_basis = min(20, n);', model_file, fixed = TRUE)] <-
            priors$prior[i]

        } else if(grepl('=', priors$prior[i])){
          tomatch <- trimws(strsplit(paste0('\\b',
                                            gsub(']', '\\]',
                                                 gsub('[', '\\[',
                                                      priors$prior[i], fixed = TRUE),
                                                 fixed = TRUE)), "[=]")[[1]][1])
          model_file[grep(tomatch, model_file, fixed = TRUE)] <-
            priors$prior[i]
        } else {
          warning('no match found in model_file for parameter: ',
                  trimws(strsplit(priors$prior[i], "[~]")[[1]][1]),
                  call. = FALSE)
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

          # Not currently possible to include new bounds on parametric effect
          # priors
          if(grepl('fixed effect|Intercept', priors$param_info[i])){
            if(!is.na(priors$new_lowerbound)[i]|!is.na(priors$new_upperbound)[i]){
              warning('not currently possible to place bounds on fixed effect priors: ',
                      trimws(strsplit(priors$prior[i], "[~]")[[1]][1]),
                      call. = FALSE)
            }
          } else {
            # gp() terms can be supplied using more mgcv-like syntax; replace
            # with the uglier syntax that is used in the Stan code so the prior
            # can be correctly updated
            if(grepl('gp(', priors$param_name[i], fixed = TRUE)|
               grepl('gp_trend', priors$param_name[i], fixed = TRUE)){
              priors$param_name[i] <- gsub('(', '_', priors$param_name[i], fixed = TRUE)
              priors$param_name[i] <- gsub(')', '_', priors$param_name[i], fixed = TRUE)
              priors$param_name[i] <- gsub(':', 'by', priors$param_name[i], fixed = TRUE)
            }

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
          }

          change_lower <- FALSE
          change_upper <- FALSE
        }
      }
    }
  }

  return(model_file)
}

#' Make detailed changes to allow a prior model to as closely match a posterior
#' from a previous model as possible
#' @noRd
posterior_to_prior = function(model_file, priors){

  # parametric terms
  para_terms <- priors$group[which(priors$parametric == TRUE)]
  para_priors <- priors$prior[which(priors$parametric == TRUE)]
  para_lowers <- priors$lb[which(priors$parametric == TRUE)]
  para_uppers <- priors$ub[which(priors$parametric == TRUE)]
  if(length(para_terms) > 0){
    for(i in 1:length(para_terms)){
      header_line <- grep(paste0(para_terms[i], '...'), model_file, fixed = TRUE)
      model_file[header_line + 1] <-
        paste0(trimws(strsplit(model_file[header_line + 1], "[~]")[[1]][1]), ' ~ ',
               para_priors[i], ';')
    }
  }

  # Other lines to modify
  mainlines_to_modify <- unique(priors$group[which(priors$parametric == FALSE)])
  for(i in 1:length(mainlines_to_modify)){
    priors %>%
      dplyr::filter(group == mainlines_to_modify[i]) -> group_priors
    replace_line <- c()
    for(j in 1:NROW(group_priors)){
      replace_line <- c(replace_line,
                        paste0(group_priors$class[j], ' ~ ', group_priors$prior[j]))
    }
    replace_line <- paste0(paste(replace_line, collapse = ';\n'), ';\n')

    orig_line <- grep(paste(trimws(strsplit(mainlines_to_modify[i], "[~]")[[1]][1]), '~'),
                      model_file, fixed = TRUE)
    model_file[orig_line] <- replace_line
  }
  model_file <- readLines(textConnection(model_file), n = -1)

  if('P_real' %in% mainlines_to_modify){
    priors %>%
      dplyr::filter(group == 'P_real') -> group_priors
    replace_line <- c()
    for(j in 1:NROW(group_priors)){
      replace_line <- c(replace_line,
                        paste0(group_priors$class[j], ' ~ ', group_priors$prior[j]))
    }
    replace_line <- paste0(paste(replace_line, collapse = ';\n'), ';\n')

    remove_start <- grep('// partial autocorrelation hyperpriors', model_file, fixed = TRUE) + 1
    remove_end <- grep('P_real[i, j] ~ normal(Pmu[2], 1 / sqrt(Pomega[2]));',
                       model_file, fixed = TRUE) + 2
    model_file <- model_file[-c(remove_start:remove_end)]
    model_file[grep('// partial autocorrelation hyperpriors', model_file, fixed = TRUE)] <-
      paste0('  // partial autocorrelation hyperpriors\n',
             replace_line)
    model_file <- readLines(textConnection(model_file), n = -1)
  }

  return(model_file)
}


#' Allow brmsprior objects to be supplied to mvgam()
#' @noRd
adapt_brms_priors = function(priors,
                             formula,
                             trend_formula,
                             data,
                             family = 'poisson',
                             use_lv = FALSE,
                             n_lv,
                             trend_model = 'None',
                             trend_map,
                             drift = FALSE,
                             warnings = FALSE,
                             knots){

  # Replace any call to 'Intercept' with '(Intercept)' to match mgcv style
  priors[] <- lapply(priors, function(x)
    gsub("Intercept(?!.*[^()]*\\))", "(Intercept)", x,
         perl = TRUE))

  # Get priors that are able to be updated
  priors_df <- get_mvgam_priors(formula = formula,
                                trend_formula = trend_formula,
                                data = data,
                                family = family,
                                use_lv = use_lv,
                                n_lv = n_lv,
                                use_stan = TRUE,
                                trend_model = trend_model,
                                trend_map = trend_map,
                                drift = drift,
                                knots = knots)

  # Update using priors from the brmsprior object
  for(i in 1:NROW(priors)){

    if(any(grepl(paste0(priors$class[i], ' ~ '),
                 priors_df$prior, fixed = TRUE))){

      # Update the prior distribution
      priors_df$prior[grepl(paste0(priors$class[i], ' ~ '),
                            priors_df$prior, fixed = TRUE)] <-
        paste0(priors$class[i], ' ~ ', priors$prior[i], ';')

      # Now update bounds
      priors_df$new_lowerbound[grepl(paste0(priors$class[i], ' ~ '),
                                     priors_df$prior, fixed = TRUE)] <-
        priors$lb[i]

      priors_df$new_upperbound[grepl(paste0(priors$class[i], ' ~ '),
                                     priors_df$prior, fixed = TRUE)] <-
        priors$ub[i]

    } else if(priors$coef[i] != '' &
              any(grepl(paste0(priors$coef[i], ' ~ '),
                        priors_df$prior, fixed = TRUE))){

      # Update the prior distribution
      priors_df$prior[grepl(paste0(priors$class[i], ' ~ '),
                            priors_df$prior, fixed = TRUE)] <-
        paste0(priors$coef[i], ' ~ ', priors$prior[i], ';')

      # Now update bounds
      priors_df$new_lowerbound[grepl(paste0(priors$coef[i], ' ~ '),
                                     priors_df$prior, fixed = TRUE)] <-
        priors$lb[i]

      priors_df$new_upperbound[grepl(paste0(priors$coef[i], ' ~ '),
                                     priors_df$prior, fixed = TRUE)] <-
        priors$ub[i]

    } else if(priors$class[i] == 'b'){
      # Update all fixed effect priors
      if(any(grepl('fixed effect', priors_df$param_info))){

        for(j in 1:NROW(priors_df)){
          if(grepl('fixed effect', priors_df$param_info[j])){
            priors_df$prior[j] <-
              paste0(paste(trimws(
                strsplit(priors_df$prior[j],
                         "[~]")[[1]][1]), '~ '),
                priors$prior[i], ';')
          }
        }
      }

    } else {
      if(warnings){
        warning(paste0('no match found in model_file for parameter: ',
                       paste0(priors$class[i], ' ', priors$coef[i])),
                call. = FALSE)
      }
    }
  }

  return(priors_df)
}
