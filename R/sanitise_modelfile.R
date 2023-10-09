#' Clean up a stan file
#' @noRd
sanitise_modelfile = function(model_file){

  # Remove empty lines
  clean_up <- vector()
  for(x in 1:length(model_file)){
    clean_up[x] <- trimws(model_file[x]) == "" |
      trimws(model_file[x]) == "NA"
  }
  clean_up[is.na(clean_up)] <- FALSE
  model_file <- model_file[!clean_up]

  # Expand on backslashes to make model more readable
  hashes <- vector()
  hashes[1] <- FALSE
  for(x in 2:length(model_file)){
    hashes[x] <- grepl('//', model_file[x], fixed = TRUE) &
      trimws(model_file[x-1]) != "" &
      (!grepl('{', model_file[x-1], fixed = TRUE) |
      grepl('}', model_file[x-1], fixed = TRUE)) &
      !grepl(';', model_file[x], fixed = TRUE)
  }

  if(any(hashes)){
    model_file[hashes] <- paste0('\n',
                                 model_file[hashes])
  }
  model_file <- readLines(textConnection(model_file), n = -1)
  return(model_file)
}
