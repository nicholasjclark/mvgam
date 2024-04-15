#' Updates for adding continuous time AR data
#' @noRd
add_corcar = function(model_data,
                      data_train,
                      data_test = NULL){

  # Calculate temporal separation among observed points
  if(!is.null(data_test)){
    all_times <- rbind(data.frame(series = as.numeric(data_train$series),
                                  time = data_train$time,
                                  index..time..index = data_train$index..time..index),
                      (data.frame(series = as.numeric(data_test$series),
                                 time = data_test$time,
                                 index..time..index = data_test$index..time..index))) %>%
                         dplyr::group_by(series) %>%
                         dplyr::arrange(index..time..index) %>%
                         dplyr::mutate(
                           time_lag = dplyr::lag(time),
                           dis_time = time - time_lag,
                           dis_time = ifelse(is.na(dis_time), 0, dis_time),
                           dis_time = pmax(1e-12, dis_time)) %>%
      dplyr::arrange(index..time..index, series)
  } else {
    all_times <- data.frame(series = as.numeric(data_train$series),
                            time = data_train$time,
                            index..time..index = data_train$index..time..index) %>%
      dplyr::group_by(series) %>%
      dplyr::arrange(index..time..index) %>%
      dplyr::mutate(
        time_lag = dplyr::lag(time),
        dis_time = time - time_lag,
        dis_time = ifelse(is.na(dis_time), 0, dis_time),
        dis_time = pmax(1e-12, dis_time)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(index..time..index, series)
  }

  time_dis <- matrix(NA, nrow = length(unique(all_times$index..time..index)),
                     ncol = length(unique(all_times$series)))
  for(i in 1:length(unique(all_times$series))){
    time_dis[,i] <- all_times$dis_time[which(all_times$series == i)]
  }

  model_data$time_dis <- time_dis

  return(model_data)
}
