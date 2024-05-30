#' Internal function to change dynamic residuals for AR or RW trends
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
