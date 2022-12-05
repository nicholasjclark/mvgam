#' Print the model code from an mvgam object
#'
#'
#' @export
#' @param object \code{list} object returned from \code{mvgam}
#' @return A `character string` containing the model code in a tidy format
code = function(object){
  if(!class(object) %in% c('mvgam', 'mvgam_prefit')){
    stop('argument "object" must be of class "mvgam" or "mvgam_prefit"')
  }

  if(object$fit_engine == 'jags'){
    cat(object$model_file[!grepl('^\\s*$', object$model_file)], sep = '\n')
  } else {
    model_file <- object$model_file
    model_file <- model_file[!grepl('^\\s*$', model_file)]

    if(any(grepl('functions {', model_file, fixed = TRUE))){
      func_start <- grep('functions {', model_file, fixed = TRUE)
    } else {
      func_start <- NULL
    }

    if(any(grepl('transformed data {', model_file, fixed = TRUE))){
      transdat_start <- grep('transformed data {', model_file, fixed = TRUE)
    } else {
      transdat_start <- NULL
    }

    func_end <- grep('data {', model_file, fixed = TRUE)
    func_lines <- c(func_end, func_end - 1)
    data_end <- grep('parameters {', model_file, fixed = TRUE)[1]
    data_lines <- c(data_end, data_end - 1)
    param_end <- grep('transformed parameters {', model_file, fixed = TRUE)[1]
    param_lines <- c(param_end, param_end - 1)
    tparam_end <- grep('model {', model_file, fixed = TRUE)[1]
    tparam_lines <- c(tparam_end, tparam_end - 1)
    mod_end <- grep('generated quantities {', model_file, fixed = TRUE)[1]
    mod_lines <- c(mod_end, mod_end - 1)
    final <- length(model_file)

    cat(unlist(lapply(seq_along(model_file), function(x){
      if(x %in% c(1, func_start, func_lines, transdat_start,
                  data_lines, param_lines,
                  tparam_lines, mod_lines, final)){
        model_file[x]
      } else {
        paste0('  ', model_file[x])
      }
    })), sep = '\n')
  }
}
