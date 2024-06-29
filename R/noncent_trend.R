#' Internal functiosn to change dynamic AR or RW trends
#' to a non-centred parameterisation for potentially big speed gains
#' @noRd
noncent_trend = function(model_file, trend_model, drift){
  # Replace trend with trend_raw in params
  model_file[grep("matrix[n, n_series] trend;",
                  model_file, fixed = TRUE)] <-
    "matrix[n, n_series] trend_raw;"
  model_file[grep("// latent trends" ,
                  model_file, fixed = TRUE)] <-
    "// raw latent trends"

  # Add trend to transformed params
  if(drift){
    drift_text <- ' drift[s] +'
  } else {
    drift_text <- NULL
  }
  if(trend_model == 'RW'){
    model_file[grep("vector[num_basis] b;",
                    model_file, fixed = TRUE)] <-
      paste0("vector[num_basis] b;\n\n",
             "// latent trends\n",
             "matrix[n, n_series] trend;\n",
             "trend = trend_raw .* rep_matrix(sigma', rows(trend_raw));\n",
             "for (s in 1 : n_series) {\n",
             "trend[2 : n, s] +=",
             drift_text,
             " trend[1 : (n - 1), s];\n",
             "}\n")
    model_file <- readLines(textConnection(model_file), n = -1)
  }

  if(trend_model == 'AR1'){
    model_file[grep("vector[num_basis] b;",
                    model_file, fixed = TRUE)] <-
      paste0("vector[num_basis] b;\n\n",
             "// latent trends\n",
             "matrix[n, n_series] trend;\n",
             "trend = trend_raw .* rep_matrix(sigma', rows(trend_raw));\n",
             "for (s in 1 : n_series) {\n",
             "trend[2 : n, s] +=",
             drift_text,
             " ar1[s] * trend[1 : (n - 1), s];\n",
             "}\n")
    model_file <- readLines(textConnection(model_file), n = -1)
  }

  if(trend_model == 'CAR1'){
    model_file[grep("vector[num_basis] b;",
                    model_file, fixed = TRUE)] <-
      paste0("vector[num_basis] b;\n\n",
             "// latent trends\n",
             "matrix[n, n_series] trend;\n",
             "trend = trend_raw .* rep_matrix(sigma', rows(trend_raw));\n",
             "for (s in 1 : n_series) {\n",
             "trend[2 : n, s] +=",
             drift_text,
             " pow(ar1[s], to_vector(time_dis[2 : n, s])) .* trend[1 : (n - 1), s];\n",
             "}\n")
    model_file <- readLines(textConnection(model_file), n = -1)
  }

  if(trend_model == 'AR2'){
    model_file[grep("vector[num_basis] b;",
                    model_file, fixed = TRUE)] <-
      paste0("vector[num_basis] b;\n\n",
             "// latent trends\n",
             "matrix[n, n_series] trend;\n",
             "trend = trend_raw .* rep_matrix(sigma', rows(trend_raw));\n",
             "for (s in 1 : n_series) {\n",
             "trend[2, s] +=", drift_text, " ar1[s] * trend[1, s];\n",
             "trend[3 : n, s] +=",
             drift_text,
             " ar1[s] * trend[2 : (n - 1), s] + ar2[s] * trend[1:(n - 2), s];\n",
             "}\n")
    model_file <- readLines(textConnection(model_file), n = -1)
  }

  if(trend_model == 'AR3'){
    model_file[grep("vector[num_basis] b;",
                    model_file, fixed = TRUE)] <-
      paste0("vector[num_basis] b;\n\n",
             "// latent trends\n",
             "matrix[n, n_series] trend;\n",
             "trend = trend_raw .* rep_matrix(sigma', rows(trend_raw));\n",
             "for (s in 1 : n_series) {\n",
             "trend[2, s] +=", drift_text, " ar1[s] * trend[1, s];\n",
             "trend[3, s] +=", drift_text, " ar1[s] * trend[2, s] + ar2[s] * trend[1, s] ;\n",
             "trend[4 : n, s] +=",
             drift_text,
             " ar1[s] * trend[3 : (n - 1), s] + ar2[s] * trend[2:(n - 2), s] + ar3[s] * trend[1:(n - 3), s];\n",
             "}\n")
    model_file <- readLines(textConnection(model_file), n = -1)
  }


  # Remove trend statements from model block and replace with the
  # z scores
  trend_start <- grep("// trend estimates",
                      model_file, fixed = TRUE)
  end_braces <- grep("}",
                     model_file, fixed = TRUE)
  p <- function(f,b) function(a) f(a,b)
  trend_end <- end_braces[Position(p(`==`,1),
                                   sign(end_braces - trend_start))]
  model_file <- model_file[-(trend_start:trend_end)]

  model_file[grep("// priors for latent trend variance parameters",
                  model_file, fixed = TRUE) + 1] <-
    paste0(model_file[grep("// priors for latent trend variance parameters",
                           model_file, fixed = TRUE) + 1],
           '\n',
           "to_vector(trend_raw) ~ std_normal();")
  model_file <- readLines(textConnection(model_file), n = -1)
  model_file

}

#' @noRd
noncent_lv = function(model_file, trend_model, drift){
  # Replace LV with LV_raw in params
  model_file[grep("matrix[n, n_lv] LV;",
                  model_file, fixed = TRUE)] <-
    "matrix[n, n_lv] LV_raw;"
  model_file[grep("// latent states" ,
                  model_file, fixed = TRUE)] <-
    "// raw latent states"

  # Add LV to transformed params
  model_file[grep("vector[num_basis] b;",
                  model_file, fixed = TRUE)] <-
    paste0("vector[num_basis] b;\n\n",
           "// latent states\n",
           "matrix[n, n_lv] LV;\n")
  model_file <- readLines(textConnection(model_file), n = -1)


  # Add LV calculations in transformed params
  if(trend_model == 'None'){
    model_file[grep("trend_mus = X_trend * b_trend;",
                    model_file, fixed = TRUE)] <-
      paste0("trend_mus = X_trend * b_trend;\n\n",
             "LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));\n",
             "for(j in 1:n_lv){\n",
             "for(i in 1:n){\n",
             "LV[i, j] += trend_mus[ytimes_trend[i, j]];\n",
             "}\n",
             "}")
    model_file <- readLines(textConnection(model_file), n = -1)
  }

  if(trend_model == 'RW'){
    model_file[grep("trend_mus = X_trend * b_trend;",
                    model_file, fixed = TRUE)] <-
      paste0("trend_mus = X_trend * b_trend;\n\n",
             "LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));\n",
             "for(j in 1:n_lv){\n",
             "LV[1, j] += trend_mus[ytimes_trend[1, j]];\n",
             "for(i in 2:n){\n",
             "LV[i, j] += trend_mus[ytimes_trend[i, j]] + 1 * (LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]]);\n",
             "}\n",
             "}")
    model_file <- readLines(textConnection(model_file), n = -1)
  }

  if(trend_model == 'AR1'){
    model_file[grep("trend_mus = X_trend * b_trend;",
                    model_file, fixed = TRUE)] <-
      paste0("trend_mus = X_trend * b_trend;\n\n",
             "LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));\n",
             "for(j in 1:n_lv){\n",
             "LV[1, j] += trend_mus[ytimes_trend[1, j]];\n",
             "for(i in 2:n){\n",
             "LV[i, j] += trend_mus[ytimes_trend[i, j]] + ar1[j] * (LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]]);\n",
             "}\n",
             "}")
    model_file <- readLines(textConnection(model_file), n = -1)
  }

  if(trend_model == 'CAR1'){
    model_file[grep("trend_mus = X_trend * b_trend;",
                    model_file, fixed = TRUE)] <-
      paste0("trend_mus = X_trend * b_trend;\n\n",
             "LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));\n",
             "for(j in 1:n_lv){\n",
             "LV[1, j] += trend_mus[ytimes_trend[1, j]];\n",
             "for(i in 2:n){\n",
             "LV[i, j] += trend_mus[ytimes_trend[i, j]] + pow(ar1[j], time_dis[i, j]) * (LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]]);\n",
             "}\n",
             "}")
    model_file <- readLines(textConnection(model_file), n = -1)
  }

  if(trend_model == 'AR2'){
    model_file[grep("trend_mus = X_trend * b_trend;",
                    model_file, fixed = TRUE)] <-
      paste0("trend_mus = X_trend * b_trend;\n\n",
             "LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));\n",
             "for(j in 1:n_lv){\n",
             "LV[1, j] += trend_mus[ytimes_trend[1, j]];\n",
             "LV[2, j] += trend_mus[ytimes_trend[2, j]] + ar1[j] * (LV[1, j] - trend_mus[ytimes_trend[1, j]]);\n",
             "for(i in 3:n){\n",
             "LV[i, j] += trend_mus[ytimes_trend[i, j]] + ar1[j] * (LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]]) + ar2[j] * (LV[i - 2, j] - trend_mus[ytimes_trend[i - 2, j]]);\n",
             "}\n",
             "}")
    model_file <- readLines(textConnection(model_file), n = -1)
  }

  if(trend_model == 'AR3'){
    model_file[grep("trend_mus = X_trend * b_trend;",
                    model_file, fixed = TRUE)] <-
      paste0("trend_mus = X_trend * b_trend;\n\n",
             "LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));\n",
             "for(j in 1:n_lv){\n",
             "LV[1, j] += trend_mus[ytimes_trend[1, j]];\n",
             "LV[2, j] += trend_mus[ytimes_trend[2, j]] + ar1[j] * (LV[1, j] - trend_mus[ytimes_trend[1, j]]);\n",
             "LV[3, j] += trend_mus[ytimes_trend[2, j]] + ar1[j] * (LV[2, j] - trend_mus[ytimes_trend[2, j]]) + ar2[j] * (LV[1, j] - trend_mus[ytimes_trend[1, j]]);\n",
             "for(i in 4:n){\n",
             "LV[i, j] += trend_mus[ytimes_trend[i, j]] + ar1[j] * (LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]]) + ar2[j] * (LV[i - 2, j] - trend_mus[ytimes_trend[i - 2, j]]) + ar3[j] * (LV[i - 3, j] - trend_mus[ytimes_trend[i - 3, j]]);\n",
             "}\n",
             "}")
    model_file <- readLines(textConnection(model_file), n = -1)
  }


  # Remove LV statements from model block and replace with the
  # z scores
  if(trend_model == 'None'){
    trend_start <- grep("LV[i, j] ~ normal(trend_mus[ytimes_trend[i, j]], sigma[j]);",
                        model_file, fixed = TRUE) - 2
    trend_end <- grep("LV[i, j] ~ normal(trend_mus[ytimes_trend[i, j]], sigma[j]);",
                        model_file, fixed = TRUE) + 2
  } else {
    if(any(grepl("LV[1, j] ~ normal(trend_mus[ytimes_trend[1, j]], sigma[j]);",
                model_file, fixed = TRUE))){
      trend_start <- grep("LV[1, j] ~ normal(trend_mus[ytimes_trend[1, j]], sigma[j]);",
                          model_file, fixed = TRUE) - 1
    } else {
      trend_start <- grep("LV[1, 1:n_lv] ~ normal(0, sigma);",
                          model_file, fixed = TRUE) - 1
    }

    end_braces <- grep("}",
                       model_file, fixed = TRUE)
    p <- function(f,b) function(a) f(a,b)
    trend_end <- end_braces[Position(p(`==`,1),
                                     sign(end_braces - trend_start))] + 1
  }

  model_file <- model_file[-(trend_start:trend_end)]

  if(any(grepl("// priors for latent state SD parameters",
              model_file, fixed = TRUE))){
    model_file[grep("// priors for latent state SD parameters",
                    model_file, fixed = TRUE) + 1] <-
      paste0(model_file[grep("// priors for latent state SD parameters",
                             model_file, fixed = TRUE) + 1],
             '\n',
             "to_vector(LV_raw) ~ std_normal();")
  } else {
    model_file[grep("// priors for factor SD parameters",
                    model_file, fixed = TRUE) + 1] <-
      paste0(model_file[grep("// priors for factor SD parameters",
                             model_file, fixed = TRUE) + 1],
             '\n',
             "to_vector(LV_raw) ~ std_normal();")
  }

  model_file <- readLines(textConnection(model_file), n = -1)
  model_file

}

#' @noRd
check_noncent = function(model_file,
                         noncentred,
                         use_lv,
                         trend_map,
                         add_ma,
                         add_cor,
                         trend_model,
                         drift,
                         silent){

  if(!missing(trend_map)){
    trendmap <- TRUE
  } else {
    trendmap <- FALSE
  }

  # Haven't yet implemented noncentering for trend_map models that don't
  # use the trend_formula
  if(trendmap &
     !any(grepl('trend_mus', model_file, fixed = TRUE)) &
     use_lv){
    trendmap <- FALSE; noncentred <- FALSE
  }

  if(!noncentred & use_lv & trendmap & trend_model == 'None'){
    if(silent <= 1L){
      message('Your model may benefit from using "noncentred = TRUE"')
    }
  }

  if(!noncentred & !add_ma & !add_cor & trend_model %in% c('RW',
                                                          'AR1',
                                                          'AR2',
                                                          'AR3',
                                                          'CAR1')){

    if(use_lv & trendmap){
      if(silent <= 1L){
        message('Your model may benefit from using "noncentred = TRUE"')
      }
    }

    if(!use_lv){
      if(silent <= 1L){
        message('Your model may benefit from using "noncentred = TRUE"')
      }
    }
  }

  if(noncentred & !add_ma & !add_cor & trend_model %in% c('RW',
                                                          'AR1',
                                                          'AR2',
                                                          'AR3',
                                                          'CAR1',
                                                          'None')){
    if(use_lv & trendmap){
      model_file <- noncent_lv(model_file = model_file,
                               trend_model = trend_model,
                               drift = FALSE)


    } else {
      model_file <- noncent_trend(model_file = model_file,
                                  trend_model = trend_model,
                                  drift = drift)
    }
  }

  return(list(model_file = model_file,
              noncentred = noncentred))
}
