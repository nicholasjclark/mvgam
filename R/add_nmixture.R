#' Updates for adding N-mixture processes
#' @noRd
add_nmixture = function(model_file,
                        model_data,
                        data_train,
                        data_test = NULL,
                        orig_trend_model){

  # Check that 'cap' is in data_train and data_test
  # Perform necessary checks on 'cap' (Positive integer, no missing values)
  # If orig_trend_model is 'None', this will be set up as a RW model so need
  # to remove sigma and change the process model lines

}
