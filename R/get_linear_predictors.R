#' Function to prepare observation model linear predictor matrix
#' @importFrom brms brmsterms
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
    if(length(terms_include) > 0L){
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

    # Compute the gp() eigenfunctions for newdata using the supplied brms_mock object
    # Requires a dataframe of all relevant variables for the gp effects
    mock_terms <- brms::brmsterms(attr(mgcv_model, 'brms_mock')$formula)
    terms_needed <- unique(all.vars(mock_terms$formula)[-1])
    newdata_mock <- data.frame(newdata[[terms_needed[1]]])
    if(length(terms_needed) > 1L){
      for(i in 2:length(terms_needed)){
        newdata_mock <- cbind(newdata_mock,
                              data.frame(newdata[[terms_needed[i]]]))
      }
    }
    colnames(newdata_mock) <- terms_needed
    newdata_mock$.fake_gp_y <- rnorm(NROW(newdata_mock))
    brms_mock_data <- brms::standata(attr(mgcv_model, 'brms_mock'),
                                     newdata = newdata_mock,
                                     internal = TRUE)

    # Extract GP attributes
    gp_att_table <- attr(mgcv_model, 'gp_att_table')
    bys <- unlist(purrr::map(gp_att_table, 'by'), use.names = FALSE)
    lvls <- unlist(purrr::map(gp_att_table, 'level'), use.names = FALSE)

    # Extract eigenfunctions for each gp effect
    eigenfuncs <- eigenfunc_list(stan_data = brms_mock_data,
                                 mock_df = newdata_mock,
                                 by = bys,
                                 level = lvls)

    # Find indices to replace in the design matrix and replace with
    # the computed eigenfunctions
    starts <- purrr::map(gp_att_table, 'first_coef')
    ends <- purrr::map(gp_att_table, 'last_coef')
    for(i in seq_along(starts)){
      Xp[,c(starts[[i]]:ends[[i]])] <- eigenfuncs[[i]]
    }
  }

  return(Xp)
}

#' Function to prepare trend linear predictor matrix in the presence of a
#' trend_map
#' @noRd
trend_map_data_prep = function(newdata, trend_map, forecast = FALSE){
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

  # Only keep one time observation per trend, in case this is a reduced dimensionality
  # State-Space model (with a trend_map) and we are forecasting ahead
  if(forecast){
    data.frame(series = trend_test$series,
               time = trend_test$index..time..index,
               row_num = 1:length(trend_test$index..time..index)) %>%
      dplyr::group_by(series, time) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::pull(row_num) -> inds_keep
    inds_keep <- sort(inds_keep)

    if(inherits(trend_test, 'list')){
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
  }

  return(trend_test)
}

#' Function to prepare trend linear predictor matrix, ensuring ordering and
#' indexing is correct with respect to the model structure
#' @noRd
trend_Xp_matrix = function(newdata, trend_map, series = 'all',
                           mgcv_model, forecast = FALSE){

  trend_test <- trend_map_data_prep(newdata,
                                    trend_map,
                                    forecast = forecast)

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

    # Compute the gp() eigenfunctions for newdata using the supplied brms_mock object
    # Requires a dataframe of all relevant variables for the gp effects
    mock_terms <- brms::brmsterms(attr(mgcv_model, 'brms_mock')$formula)
    terms_needed <- unique(all.vars(mock_terms$formula)[-1])
    newdata_mock <- data.frame(newdata[[terms_needed[1]]])
    if(length(terms_needed) > 1L){
      for(i in 2:length(terms_needed)){
        newdata_mock <- cbind(newdata_mock,
                              data.frame(newdata[[terms_needed[i]]]))
      }
    }
    colnames(newdata_mock) <- terms_needed
    newdata_mock$.fake_gp_y <- rnorm(NROW(newdata_mock))
    brms_mock_data <- brms::standata(attr(mgcv_model, 'brms_mock'),
                                     newdata = newdata_mock,
                                     internal = TRUE)

    # Extract GP attributes
    gp_att_table <- attr(mgcv_model, 'gp_att_table')
    bys <- unlist(purrr::map(gp_att_table, 'by'), use.names = FALSE)
    lvls <- unlist(purrr::map(gp_att_table, 'level'), use.names = FALSE)

    # Extract eigenfunctions for each gp effect
    eigenfuncs <- eigenfunc_list(stan_data = brms_mock_data,
                                 mock_df = newdata_mock,
                                 by = bys,
                                 level = lvls)

    # Find indices to replace in the design matrix and replace with
    # the computed eigenfunctions
    starts <- purrr::map(gp_att_table, 'first_coef')
    ends <- purrr::map(gp_att_table, 'last_coef')
    for(i in seq_along(starts)){
      Xp_trend[,c(starts[[i]]:ends[[i]])] <- eigenfuncs[[i]]
    }
  }

  return(Xp_trend)
}

