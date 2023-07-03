#' Function to prepare observation model linear predictor matrix
#' @noRd
obs_Xp_matrix = function(newdata, mgcv_model){
  suppressWarnings(Xp  <- try(predict(mgcv_model,
                                      newdata = newdata,
                                      type = 'lpmatrix'),
                              silent = TRUE))

  if(inherits(Xp, 'try-error')){
    testdat <- data.frame(time = newdata$time)

    terms_include <- names(mgcv_model$coefficients)[which(!names(mgcv_model$coefficients)
                                                          %in% '(Intercept)')]
    if(length(terms_include) > 0){
      newnames <- vector()
      newnames[1] <- 'time'
      for(i in 1:length(terms_include)){
        testdat <- cbind(testdat, data.frame(newdata[[terms_include[i]]]))
        newnames[i+1] <- terms_include[i]
      }
      colnames(testdat) <- newnames
    }

    suppressWarnings(Xp  <- predict(object$mgcv_model,
                                    newdata = testdat,
                                    type = 'lpmatrix'))
  }

  return(Xp)
}


#' Function to prepare trend linear predictor matrix, ensuring ordering and
#' indexing is correct with respect to the model structure
#' @noRd
trend_Xp_matrix = function(newdata, trend_map, series = 'all',
                           mgcv_model){

  trend_test <- newdata
  trend_indicators <- vector(length = length(trend_test$time))
  for(i in 1:length(trend_test$time)){
    trend_indicators[i] <- trend_map$trend[which(trend_map$series ==
                                                   trend_test$series[i])]
  }
  trend_indicators <- as.factor(paste0('trend', trend_indicators))
  trend_test$series <- trend_indicators
  trend_test$y <- NULL

  # Because these are set up inherently as dynamic factor models,
  # we ALWAYS need to forecast the full set of trends, regardless of
  # which series (or set of series) is being forecast
  data.frame(series = trend_test$series,
             time = trend_test$time,
             row_num = 1:length(trend_test$time)) %>%
    dplyr::group_by(series, time) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(time, series) %>%
    dplyr::pull(row_num) -> inds_keep

  if(inherits(newdata, 'list')){
    trend_test <- lapply(trend_test, function(x){
      if(is.matrix(x)){
        matrix(x[inds_keep,], ncol = NCOL(x))
      } else {
        x[inds_keep]
      }

    })
  } else {
    trend_test <- trend_test[inds_keep, ]
  }

  suppressWarnings(Xp_trend  <- try(predict(mgcv_model,
                                            newdata = trend_test,
                                            type = 'lpmatrix'),
                                    silent = TRUE))

  if(inherits(Xp_trend, 'try-error')){
    testdat <- data.frame(series = trends_test$series)

    terms_include <- names(mgcv_model$coefficients)[which(!names(mgcv_model$coefficients)
                                                          %in% '(Intercept)')]
    if(length(terms_include) > 0){
      newnames <- vector()
      newnames[1] <- 'series'
      for(i in 1:length(terms_include)){
        testdat <- cbind(testdat, data.frame(trends_test[[terms_include[i]]]))
        newnames[i+1] <- terms_include[i]
      }
      colnames(testdat) <- newnames
    }

    suppressWarnings(Xp_trend  <- predict(mgcv_model,
                                          newdata = testdat,
                                          type = 'lpmatrix'))
  }
  return(Xp_trend)
}

