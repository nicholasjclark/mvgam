#' Function to prepare observation model linear predictor matrix
#' @noRd
obs_Xp_matrix = function(newdata, mgcv_model){
  suppressWarnings(Xp  <- try(predict(mgcv_model,
                                      newdata = newdata,
                                      type = 'lpmatrix'),
                              silent = TRUE))

  if(inherits(Xp, 'try-error')){
    testdat <- data.frame(time = newdata$time)

    terms_include <- insight::find_predictors(mgcv_model)$conditional
    if(any(terms_include %in% names(newdata) == FALSE)){
      stop('not all required variables have been supplied in newdata!',
           call. = FALSE)
    }
    if(length(terms_include) > 0){
      newnames <- vector()
      newnames[1] <- 'time'
      for(i in 1:length(terms_include)){
        testdat <- cbind(testdat, data.frame(newdata[[terms_include[i]]]))
        newnames[i+1] <- terms_include[i]
      }
      colnames(testdat) <- newnames
    }

    suppressWarnings(Xp  <- predict(mgcv_model,
                                    newdata = testdat,
                                    type = 'lpmatrix'))
  }

  # Check for any gp() terms and update the design matrix
  # accordingly
  if(!is.null(attr(mgcv_model, 'gp_att_table'))){
    # Compute the eigenfunctions from the supplied attribute table,
    # and add them to the Xp matrix

    # Extract GP attributes
    gp_att_table <- attr(mgcv_model, 'gp_att_table')
    gp_covariates <- unlist(purrr::map(gp_att_table, 'covariate'))
    by <- unlist(purrr::map(gp_att_table, 'by'))
    level <- unlist(purrr::map(gp_att_table, 'level'))
    k <- unlist(purrr::map(gp_att_table, 'k'))
    scale <- unlist(purrr::map(gp_att_table, 'scale'))
    mean <- unlist(purrr::map(gp_att_table, 'mean'))
    max_dist <- unlist(purrr::map(gp_att_table, 'max_dist'))
    boundary <- unlist(purrr::map(gp_att_table, 'boundary'))
    L <- unlist(purrr::map(gp_att_table, 'L'))

    # Compute eigenfunctions
    test_eigenfunctions <- lapply(seq_along(gp_covariates), function(x){
      prep_eigenfunctions(data = newdata,
                          covariate = gp_covariates[x],
                          by = by[x],
                          level = level[x],
                          k = k[x],
                          boundary = boundary[x],
                          L = L[x],
                          mean = mean[x],
                          scale = scale[x],
                          max_dist = max_dist[x])
    })

    # Find indices to replace in the design matrix and replace with
    # the computed eigenfunctions
    starts <- purrr::map(gp_att_table, 'first_coef')
    ends <- purrr::map(gp_att_table, 'last_coef')
    for(i in seq_along(starts)){
      Xp[,c(starts[[i]]:ends[[i]])] <- test_eigenfunctions[[i]]
    }
  }

  return(Xp)
}


#' Function to prepare trend linear predictor matrix, ensuring ordering and
#' indexing is correct with respect to the model structure
#' @noRd
trend_Xp_matrix = function(newdata, trend_map, series = 'all',
                           mgcv_model){

  trend_test <- newdata
  trend_indicators <- vector(length = length(trend_test$series))
  for(i in 1:length(trend_test$series)){
    trend_indicators[i] <- trend_map$trend[which(as.character(trend_map$series) ==
                                                   as.character(trend_test$series[i]))]
  }
  trend_indicators <- factor(paste0('trend', trend_indicators),
                             levels = paste0('trend',
                                             unique(trend_map$trend)))
  trend_test$series <- trend_indicators
  trend_test$y <- NULL

  suppressWarnings(Xp_trend  <- try(predict(mgcv_model,
                                            newdata = trend_test,
                                            type = 'lpmatrix'),
                                    silent = TRUE))

  if(inherits(Xp_trend, 'try-error')){
    testdat <- data.frame(series = trend_test$series)

    terms_include <- insight::find_predictors(mgcv_model)$conditional
    if(any(terms_include %in% names(trend_test) == FALSE)){
      stop('not all required variables have been supplied in newdata!',
           call. = FALSE)
    }
    if(length(terms_include) > 0){
      newnames <- vector()
      newnames[1] <- 'series'
      for(i in 1:length(terms_include)){
        testdat <- cbind(testdat, data.frame(trend_test[[terms_include[i]]]))
        newnames[i+1] <- terms_include[i]
      }
      colnames(testdat) <- newnames
    }

    suppressWarnings(Xp_trend  <- predict(mgcv_model,
                                          newdata = testdat,
                                          type = 'lpmatrix'))
  }

  # Check for any gp() terms and update the design matrix
  # accordingly
  if(!is.null(attr(mgcv_model, 'gp_att_table'))){
    # Compute the eigenfunctions from the supplied attribute table,
    # and add them to the Xp matrix

    # Extract GP attributes
    gp_att_table <- attr(mgcv_model, 'gp_att_table')
    gp_covariates <- unlist(purrr::map(gp_att_table, 'covariate'))
    by <- unlist(purrr::map(gp_att_table, 'by'))
    level <- unlist(purrr::map(gp_att_table, 'level'))
    k <- unlist(purrr::map(gp_att_table, 'k'))
    scale <- unlist(purrr::map(gp_att_table, 'scale'))
    mean <- unlist(purrr::map(gp_att_table, 'mean'))
    max_dist <- unlist(purrr::map(gp_att_table, 'max_dist'))
    boundary <- unlist(purrr::map(gp_att_table, 'boundary'))
    L <- unlist(purrr::map(gp_att_table, 'L'))

    # Compute eigenfunctions
    test_eigenfunctions <- lapply(seq_along(gp_covariates), function(x){
      prep_eigenfunctions(data = trend_test,
                          covariate = gp_covariates[x],
                          by = by[x],
                          level = level[x],
                          k = k[x],
                          boundary = boundary[x],
                          L = L[x],
                          mean = mean[x],
                          scale = scale[x],
                          max_dist = max_dist[x])
    })

    # Find indices to replace in the design matrix and replace with
    # the computed eigenfunctions
    starts <- purrr::map(gp_att_table, 'first_coef')
    ends <- purrr::map(gp_att_table, 'last_coef')
    for(i in seq_along(starts)){
      Xp_trend[,c(starts[[i]]:ends[[i]])] <- test_eigenfunctions[[i]]
    }
  }

  return(Xp_trend)
}

