#' Get data objects into correct order in case it is not already
#'@noRd
sort_data = function(data, series_time = FALSE){
  if(inherits(data, 'list')){
    data_arranged <- data
    if(series_time){
      temp_dat = data.frame(time = data$index..time..index,
                            series = data$series) %>%
        dplyr::mutate(index = dplyr::row_number()) %>%
        dplyr::arrange(series, time)
    } else {
      temp_dat = data.frame(time = data$index..time..index,
                            series = data$series) %>%
        dplyr::mutate(index = dplyr::row_number()) %>%
        dplyr::arrange(time, series)
    }

    data_arranged <- lapply(data, function(x){
      if(is.matrix(x)){
        matrix(x[temp_dat$index,], ncol = NCOL(x))
      } else {
        x[temp_dat$index]
      }
    })
    names(data_arranged) <- names(data)
  } else {
    if(series_time){
      data_arranged <- data %>%
        dplyr::arrange(series, index..time..index)
    } else {
      data_arranged <- data %>%
        dplyr::arrange(index..time..index, series)
    }
  }

  return(data_arranged)
}

#' Create prediction grids, mostly for simple plotting functions
#'@noRd
data_grid = function(..., newdata){

  dots <- list(...)
  vars <- names(dots)

  # Validate that vars exist in supplied data
  for(i in seq_along(vars)){
    if(!exists(vars[i], newdata)){
      stop(paste0('Variable ', vars[i], ' not found in newdata'),
           call. = FALSE)
    }
  }

  # Create sample dummy dataframe to get the prediction grid, ensuring
  # factors are preserved
  newdat_grid <- data.frame(do.call(cbind.data.frame,
                                    lapply(vars, function(x){
    newdata[[x]]
  })))
  colnames(newdat_grid) <- vars

  # Use the supplied conditions for making the datagrid
  newdat_grid <- marginaleffects::datagrid(..., newdata = newdat_grid)

  # Now replicate the first observation for all other variables
  if(inherits(newdata, 'list')){
    newdat_full <- lapply(seq_along(newdata), function(x){
      if(names(newdata)[x] %in% vars){
        newdat_grid[[names(newdata)[x]]]
      } else {
        if(is.matrix(newdata[[x]])){
          t(replicate(NROW(newdat_grid), newdata[[x]][1, ]))
        } else {
          if(is.factor(newdata[[x]])){
            factor(rep(newdata[[x]][1], NROW(newdat_grid)),
                   levels = levels(newdata[[x]]))
          } else {
            rep(newdata[[x]][1], NROW(newdat_grid))
          }
        }
      }
    })
    names(newdat_full) <- names(newdata)
  } else {
    newdat_full <-
      dplyr::bind_cols(newdat_grid,
                       data.frame(newdata %>%
                                    dplyr::select(!vars) %>%
                                    dplyr::slice_head(n = 1)))
  }

  return(newdat_full)
}
