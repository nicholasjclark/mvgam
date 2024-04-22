#' Updates for allowing shared observation params across series
#' @noRd
shared_obs_params = function(model_file, family){
  if(family == 'poisson'){
    message('Context share_obs_params: Poisson family has no additional observation params')
    model_file <- model_file
  }

  if(family == 'nmix'){
    message('Context share_obs_params: nmix family has no additional observation params')
    model_file <- model_file
  }

  if(family %in% c('student', 'gaussian', 'lognormal')){
    model_file[grep("vector<lower=0>[n_series] sigma_obs;",
                    model_file, fixed = TRUE)] <-
      "real<lower=0> sigma_obs;"

    model_file <- model_file[-grep("flat_sigma_obs = rep_each(sigma_obs, n)[obs_ind];" ,
                                   model_file, fixed = TRUE)]
    model_file <- model_file[-grep("vector[n_nonmissing] flat_sigma_obs;"   ,
                                   model_file, fixed = TRUE)]
    model_file[grep("flat_sigma_obs);", model_file, fixed = TRUE)] <-
      'sigma_obs);'

    if(any(grepl("flat_sigma_obs,", model_file, fixed = TRUE))){
      model_file[grep("flat_sigma_obs,", model_file, fixed = TRUE)] <-
        "sigma_obs,"
      model_file[grep("data vector Y, matrix X, vector b, vector sigma_obs, real alpha) {",
                      model_file, fixed = TRUE)] <-
        "data vector Y, matrix X, vector b, real sigma_obs, real alpha) {"
      model_file[grep("ptarget += normal_id_glm_lpdf(Y[start:end] | X[start:end], alpha, b, sigma_obs[start:end]);",
                      model_file, fixed = TRUE)] <-
        "ptarget += normal_id_glm_lpdf(Y[start:end] | X[start:end], alpha, b, sigma_obs);"
    }

    model_file[grep("sigma_obs_vec[1:n,s] = rep_vector(sigma_obs[s], n);",
                    model_file, fixed = TRUE)] <-
      "sigma_obs_vec[1:n,s] = rep_vector(sigma_obs, n);"
  }

  if(family == 'student'){
    model_file[grep("vector<lower=0>[n_series] nu;",
                    model_file, fixed = TRUE)] <-
      "real<lower=0> nu;"

    model_file <- model_file[-grep("flat_nu = rep_each(nu, n)[obs_ind];" ,
                                   model_file, fixed = TRUE)]
    model_file <- model_file[-grep("vector[n_nonmissing] flat_nu;"   ,
                                   model_file, fixed = TRUE)]
    model_file[grep("flat_ys ~ student_t(flat_nu,", model_file, fixed = TRUE)] <-
      "flat_ys ~ student_t(nu,"

    model_file[grep("nu_vec[1:n,s] = rep_vector(nu[s], n);",
                    model_file, fixed = TRUE)] <-
      "nu_vec[1:n,s] = rep_vector(nu, n);"
  }

  if(family == 'negative binomial'){
    model_file[grep('vector<lower=0>[n_series] phi_inv;',
                    model_file, fixed = TRUE)] <-
      'real<lower=0> phi_inv;'

    model_file <- model_file[-grep('flat_phis = to_array_1d(rep_each(phi_inv, n)[obs_ind]);',
                    model_file, fixed = TRUE)]
    model_file <- model_file[-grep("real flat_phis[n_nonmissing];",
                                   model_file, fixed = TRUE)]

    model_file[grep("inv(flat_phis));" , model_file,
                    fixed = TRUE)] <-
      'inv(phi_inv));'

    model_file[grep("phi = inv(phi_inv);"  , model_file,
                    fixed = TRUE)] <-
      "phi = rep_vector(inv(phi_inv), n_series);"
  }

  if(family == 'beta'){
    model_file[grep('vector<lower=0>[n_series] phi;',
                    model_file, fixed = TRUE)] <-
      'real<lower=0> phi;'

    model_file <- model_file[-grep('flat_phis = rep_each(phi, n)[obs_ind];',
                                   model_file, fixed = TRUE)]
    model_file <- model_file[-grep("vector[n_nonmissing] flat_phis;" ,
                                   model_file, fixed = TRUE)]

    model_file[grep("inv_logit(flat_xs * b) .* flat_phis," , model_file,
                    fixed = TRUE)] <-
      "inv_logit(flat_xs * b) .* phi,"
    model_file[grep("(1 - inv_logit(flat_xs * b)) .* flat_phis);" , model_file,
                    fixed = TRUE)] <-
      "(1 - inv_logit(flat_xs * b)) .* phi);"

    model_file[grep("inv_logit(append_col(flat_xs, flat_trends) * append_row(b, 1.0)) .* flat_phis," , model_file,
                    fixed = TRUE)] <-
      "inv_logit(append_col(flat_xs, flat_trends) * append_row(b, 1.0)) .* phi,"
    model_file[grep("(1 - inv_logit(append_col(flat_xs, flat_trends) * append_row(b, 1.0))) .* flat_phis);" , model_file,
                    fixed = TRUE)] <-
      "(1 - inv_logit(append_col(flat_xs, flat_trends) * append_row(b, 1.0))) .* phi);"

    model_file[grep("phi_vec[1:n,s] = rep_vector(phi[s], n);"  , model_file,
                    fixed = TRUE)] <-
      "phi_vec[1:n,s] = rep_vector(phi, n);"
  }

  if(family == 'Gamma'){
    model_file[grep("vector<lower=0>[n_series] shape;",
                    model_file, fixed = TRUE)] <-
      "real<lower=0> shape;"

    model_file <- model_file[-grep("flat_shapes = rep_each(shape, n)[obs_ind];",
                                   model_file, fixed = TRUE)]
    model_file <- model_file[-grep("vector[n_nonmissing] flat_shapes;",
                                   model_file, fixed = TRUE)]

    model_file[grep("flat_shapes, flat_shapes ./ exp(flat_xs * b));" , model_file,
                    fixed = TRUE)] <-
      "shape, shape ./ exp(flat_xs * b));"

    model_file[grep("flat_shapes, flat_shapes ./ exp(append_col(flat_xs, flat_trends) * append_row(b, 1.0)));" , model_file,
                    fixed = TRUE)] <-
      "shape, shape ./ exp(append_col(flat_xs, flat_trends) * append_row(b, 1.0)));"

    model_file[grep("shape_vec[1:n,s] = rep_vector(shape[s], n);", model_file,
                    fixed = TRUE)] <-
      "shape_vec[1:n,s] = rep_vector(shape, n);"
  }

  return(model_file)
}
