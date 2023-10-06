#' Make gp() attributes table and necessary stan lines
#' @noRd
make_gp_additions = function(gp_details, data,
                             newdata,
                             model_data, mgcv_model,
                             gp_terms){
  # Need to expand combination of GPs if any of the by variables
  # is a factor; mvgam will drop unused levels automatically
  by <- gp_details$by
  gp_details$row_id <- 1:NROW(gp_details)
  gp_covariates <- gp_details$gp_covariates
  gp_details_orig <- gp_details
  if(any(!is.na(by))){
    for(i in 1:length(by)){
      if(!is.na(by[i])){
        if(is.factor(data[[by[i]]])){
          nlevels <- length(levels(droplevels(data[[by[i]]])))
          new_details <- do.call(rbind, lapply(1:nlevels, function(x){
            gp_details_orig[i,]
          }))
          new_details$level <- levels(droplevels(data[[by[i]]]))

          rows_drop <- which(gp_details$gp_covariates == gp_covariates[i] &
                               gp_details$by == by[i])

          gp_details <- gp_details[-rows_drop,]
          gp_details <- rbind(gp_details, new_details)
        }
      }
    }
  }

  # Preserve ordering of terms
  gp_details %>%
    dplyr::arrange(row_id, level) %>%
    dplyr::select(-row_id) -> gp_details

  # Prepare the GP objects and Stan data lines;
  # Data updates need to happen BEFORE calling vectorise
  # model_file modifications can happen AFTER drop_obs_intercept
  # to ensure everything behaves predictably
  gp_covariates <- gp_details$gp_covariates
  scale <- gp_details$scale
  boundary <- gp_details$boundary
  by <- gp_details$by
  level <- gp_details$level

  # Prep the covariate for GP modelling
  gp_data <- lapply(seq_along(gp_covariates), function(x){

    # Find the correct k to ensure that the total number of coefficients
    # when using gp(k = k) is the same as when using s(k = k + 1)
    smooth_terms <- unlist(purrr::map(mgcv_model$smooth, 'term'))
    smooth_bys <- unlist(purrr::map(mgcv_model$smooth, 'by'))
    if(any(smooth_bys == 'NA')){
      smooth_bys[smooth_bys == 'NA'] <- NA
    }
    term_k <- mgcv_model$smooth[[min(which(smooth_bys %in% by[x] &
                               smooth_terms == gp_covariates[x]))]]$df

    prep_gp_covariate(data = data,
                      covariate = gp_covariates[x],
                      by = by[x],
                      level = level[x],
                      scale = scale[x],
                      k = term_k,
                      boundary = boundary[x])
  })

  # Consolidate Stan data objects and add to model_data
  gp_stan_data <- do.call(c, purrr::map(gp_data, 'data_append'))
  model_data <- append(model_data, gp_stan_data)

  # Consolidate attribute tables
  gp_att_table <- purrr::map(gp_data, 'att_table')

  # Create updated design matrix by replacing the s() basis functions with
  # the gp() eigenfunctions
  coefs_replace <- list()
    for(x in gp_terms){
      label <- attr(terms(formula(mgcv_model)), 'term.labels')[x]
      s_attributes <- eval(rlang::parse_expr(label))
      if(s_attributes$by != 'NA'){
        coef_name <- paste0('s(', s_attributes$term, '):', s_attributes$by)
      } else {
        coef_name <- paste0('s(', s_attributes$term, ')')
      }
      which_replace <- grep(coef_name, names(coef(mgcv_model)), fixed = TRUE)
      names(mgcv_model$coefficients)[which_replace] <-
        gsub('s(', 'gp(', names(mgcv_model$coefficients)[which_replace],
             fixed = TRUE)
      coefs_replace[[x]] <- which_replace
    }

  newX <- model_data$X

  # Training data eigenfunctions
  eigenfuncs <- do.call(cbind, purrr::map(gp_data, 'eigenfunctions'))

  # Testing data eigenfunctions
  if(!is.null(newdata)){
    gp_covariates <- unlist(purrr::map(gp_att_table, 'covariate'))
    by <- unlist(purrr::map(gp_att_table, 'by'))
    level <- unlist(purrr::map(gp_att_table, 'level'))
    k <- unlist(purrr::map(gp_att_table, 'k'))
    scale <- unlist(purrr::map(gp_att_table, 'scale'))
    mean <- unlist(purrr::map(gp_att_table, 'mean'))
    max_dist <- unlist(purrr::map(gp_att_table, 'max_dist'))
    boundary <- unlist(purrr::map(gp_att_table, 'boundary'))
    L <- unlist(purrr::map(gp_att_table, 'L'))
    test_eigenfunctions <- lapply(seq_along(gp_covariates), function(x){
      prep_eigenfunctions(data = newdata,
                          covariate = gp_covariates[x],
                          by = by[x],
                          level = level[x],
                          k = k[x],
                          boundary = boundary[x],
                          L = L[x],
                          mean = mean[x],
                          scale = scale[x],
                          max_dist = max_dist[x])
    })

    eigenfuncs <- rbind(eigenfuncs,
                        do.call(cbind, test_eigenfunctions))
  }

  newX[unlist(coefs_replace), ] <- t(eigenfuncs)
  model_data$X <- newX

  # Consolidate Stan data lines
  gp_stan_lines <- paste0(purrr::map(gp_data, 'data_lines'), collapse = '')

  # Add coefficient indices to attribute table and to Stan data
  for(covariate in seq_along(gp_att_table)){
    coef_indices <- grep(gp_att_table[[covariate]]$name,
                         names(coef(mgcv_model)), fixed = TRUE)
    gp_att_table[[covariate]]$first_coef <- min(coef_indices)
    gp_att_table[[covariate]]$last_coef <- max(coef_indices)

    gp_names <- gp_att_table[[covariate]]$name
    gp_names <- gsub('(', '_', gp_names, fixed = TRUE)
    gp_names <- gsub(')', '_', gp_names, fixed = TRUE)
    gp_names <- gsub(':', 'by', gp_names, fixed = TRUE)

    gp_stan_lines <- paste0(gp_stan_lines,
                            paste0('array[',  gp_att_table[[covariate]]$k,
                                   '] int b_idx_',
                                   gp_names,
                                   '; // gp basis coefficient indices\n'))
    gp_idx_data <- list(coef_indices)
    names(gp_idx_data) <- paste0('b_idx_',
                                 gp_names)
    model_data <- append(model_data, gp_idx_data)
  }

  # Add the GP attribute table to the mgcv_model
  attr(mgcv_model, 'gp_att_table') <- gp_att_table

  return(list(model_data = model_data,
              mgcv_model = mgcv_model,
              gp_stan_lines = gp_stan_lines,
              gp_att_table = gp_att_table))
}

#' Which terms are gp() terms?
#' @noRd
which_are_gp = function(formula){
  tf <- terms.formula(formula, specials = c("gp"))
  if(is.null(rlang::f_lhs(formula))){
    out <- attr(tf,"specials")$gp
  } else {
    out <- attr(tf,"specials")$gp - 1
  }
  return(out)
}

#' Convert gp() terms to s() terms for initial model construction#'
#' @importFrom stats drop.terms
#' @noRd
gp_to_s <- function(formula){
  # Extract details of gp() terms
  gp_details <- get_gp_attributes(formula)

  # Drop these terms from the formula
  which_gp <- which_are_gp(formula)
  response <- rlang::f_lhs(formula)

  suppressWarnings(tt <- try(drop.terms(terms(formula),
                                        which_gp,
                                        keep.response = TRUE),
                             silent = TRUE))
  if(inherits(tt, 'try-error')){
    newformula <- as.formula(paste(response, '~ 1'))
  } else {
    tt <- drop.terms(terms(formula), which_gp, keep.response = TRUE)
    newformula <- reformulate(attr(tt, "term.labels"), rlang::f_lhs(formula))
  }

  # Now replace the gp() terms with s() for constructing the initial model
  s_terms <- vector()
  for(i in 1:NROW(gp_details)){
    if(!is.na(gp_details$by[i])){
      s_terms[i] <- paste0('s(',
                           gp_details$gp_covariates[i],
                           ', by = ',
                           gp_details$by[i],
                           ', k = ',
                           gp_details$k[i] + 1, ')')
    } else {
      s_terms[i] <- paste0('s(',
                           gp_details$gp_covariates[i],
                           ', k = ',
                           gp_details$k[i] + 1, ')')
    }
  }

  if(length(attr(terms(newformula), 'term.labels')) == 0){
    rhs <- '1'
  } else {
    rhs <- attr(terms(newformula), 'term.labels')
  }

  newformula <- as.formula(paste(response, '~',
                                 paste(paste(rhs,
                                             collapse = '+'), '+',
                                       paste(s_terms, collapse = '+'))))
  attr(newformula, '.Environment') <- attr(formula, '.Environment')
  return(newformula)
}


#' Store attributes of the gp terms
#' @importFrom rlang parse_expr
#' @noRd
get_gp_attributes = function(formula){
  gp_terms <- rownames(attr(terms(formula), 'factors'))[
    grep('gp', rownames(attr(terms(formula), 'factors')))]
  gp_attributes <- lapply(seq_along(gp_terms), function(x){
    eval(rlang::parse_expr(gp_terms[x]))
  })

  # Extract information necessary to construct the GP terms
  gp_covariates <- unlist(purrr::map(gp_attributes, 'term'))
  k <- unlist(purrr::map(gp_attributes, 'k'))
  if(any(is.na(k))){
    k[is.na(k)] <- 10
  }
  scale <- unlist(purrr::map(gp_attributes, 'scale'))
  boundary <- unlist(purrr::map(gp_attributes, 'c'))
  if(any(is.na(boundary))){
    boundary[is.na(boundary)] <- 5.0/4
  }
  by <- unlist(purrr::map(gp_attributes, 'by'))
  if(any(by == 'NA')){
    by[by == 'NA'] <- NA
  }

  # Return as a data.frame
  return(data.frame(gp_covariates,
                    k,
                    scale,
                    boundary,
                    by,
                    level = NA))
}


#' Propagate a Hilbert basis GP
#' Evaluate Laplacian eigenfunction for a given GP basis function
#' @noRd
phi = function(boundary, m, centred_covariate) {
  1 / sqrt(boundary) * sin((m * pi)/(2 * boundary) *
                             (centred_covariate + boundary))
}

#' Evaluate eigenvalues for a given GP basis function
#' @noRd
lambda = function(boundary, m) {
  ((m * pi)/(2 * boundary))^2
}

#' Spectral density squared exponential Gaussian Process kernel
#' @noRd
spd = function(alpha_gp, rho_gp, eigenvalues) {
  (alpha_gp^2) * sqrt(2 * pi) * rho_gp *
    exp(-0.5 * (rho_gp^2) * (eigenvalues^2))
}

#' @noRd
sim_hilbert_gp = function(alpha_gp,
                          rho_gp,
                          b_gp,
                          last_trends,
                          fc_times,
                          train_times,
                          mean_train_times){

  num_gp_basis <- length(b_gp)

  # Get vector of eigenvalues of covariance matrix
  eigenvalues <- vector()
  for(m in 1:num_gp_basis){
    eigenvalues[m] <- lambda(boundary = (5.0/4) *
                               (max(train_times) - min(train_times)),
                             m = m)
  }

  # Get vector of eigenfunctions
  eigenfunctions <- matrix(NA, nrow = length(fc_times),
                           ncol = num_gp_basis)
  for(m in 1:num_gp_basis){
    eigenfunctions[, m] <- phi(boundary = (5.0/4) *
                                 (max(train_times) - min(train_times)),
                               m = m,
                               centred_covariate = fc_times - mean_train_times)
  }

  # Compute diagonal of covariance matrix
  diag_SPD <- sqrt(spd(alpha_gp = alpha_gp,
                       rho_gp = rho_gp,
                       sqrt(eigenvalues)))

  # Compute GP trend forecast
  as.vector((diag_SPD * b_gp) %*% t(eigenfunctions))
}

#' Mean-center and scale the particular covariate of interest
#' so that the maximum Euclidean distance between any two points is 1
#' @noRd
scale_cov <- function(data, covariate, by, level, scale = TRUE,
                      mean, max_dist){
  if(!is.na(by)){
    if(!is.na(level)){
      Xgp <- data[[covariate]][data[[by]] == level]
    }
  } else {
    Xgp <- data[[covariate]]
  }

  if(is.na(mean)){
    Xgp_mean <- mean(Xgp, na.rm = TRUE)
  } else {
    Xgp_mean <- mean
  }

  if(is.na(max_dist)){
    Xgp_max_dist <- (abs(max(Xgp, na.rm = TRUE) -
                           min(Xgp, na.rm = TRUE)))
  } else {
    Xgp_max_dist <- max_dist
  }

  if(scale){
    # Mean center and divide by max euclidean distance
    (Xgp - Xgp_mean) / Xgp_max_dist

  } else {
    # Just mean center
    Xgp - Xgp_mean
  }
}

#' prep GP eigenfunctions
#' @noRd
prep_eigenfunctions = function(data,
                               covariate,
                               by = NA,
                               level = NA,
                               k,
                               boundary,
                               mean = NA,
                               max_dist = NA,
                               scale = TRUE,
                               L){

  # Extract and scale covariate (scale set to FALSE if this is a prediction
  # step so that we can scale by the original training covariate values supplied
  # in mean and max_dist)
  covariate_cent <- scale_cov(data = data,
                              covariate = covariate,
                              by = by,
                              level = level,
                              mean = mean,
                              max_dist = max_dist,
                              scale = scale)

  # Construct matrix of eigenfunctions
  eigenfunctions <- matrix(NA, nrow = length(covariate_cent),
                           ncol = k)
  if(missing(L)){
    L <- brms:::choose_L(covariate_cent, boundary)
  }

  for(m in 1:k){
    # eigenfunctions[, m] <- phi(boundary = boundary *
    #                                      (max(covariate_cent) -
    #                                         min(covariate_cent)),
    #                                    m = m,
    #                                    centred_covariate = covariate_cent)

    eigenfunctions[, m] <- brms:::eigen_fun_cov_exp_quad(x = matrix(covariate_cent),
                                                         m = m,
                                                         L = L)
  }

  # Multiply eigenfunctions by the 'by' variable if one is supplied
  if(!is.na(by)){
    if(!is.na(level)){
      # no multiplying needed as this is a factor by variable,
      # but we need to pad the eigenfunctions with zeros
      # for the observations where the by is a different level
      full_eigens <- matrix(0, nrow = length(data[[by]]),
                            ncol = NCOL(eigenfunctions))
      full_eigens[(1:length(data[[by]]))[
        data[[by]] == level],] <- eigenfunctions
      eigenfunctions <- full_eigens
    } else {
      eigenfunctions <- eigenfunctions * data[[by]]
    }
  }
  eigenfunctions
}

#' Prep Hilbert Basis GP covariates
#' @noRd
prep_gp_covariate = function(data,
                             covariate,
                             by = NA,
                             level = NA,
                             scale = TRUE,
                             boundary = 5.0/4,
                             k = 20){

  covariate_cent <- scale_cov(data = data,
                              covariate = covariate,
                              scale = scale,
                              by = by,
                              mean = NA,
                              max_dist = NA,
                              level = level)

  if(!is.na(by)){
    if(!is.na(level)){
      Xgp <- data[[covariate]][data[[by]] == level]
    }
  } else {
    Xgp <- data[[covariate]]
  }

  covariate_mean <- mean(Xgp, na.rm = TRUE)
  covariate_max_dist <- ifelse(scale,
                               abs(max(Xgp,
                                  na.rm = TRUE) -
                                 min(Xgp,
                                     na.rm = TRUE)),
                               1)
  # Check k
  if(k > length(unique(covariate_cent))){
    warning('argument "k" > number of unique covariate values;\ndropping to the maximum allowed "k"',
            call. = FALSE)
    k <- length(unique(covariate_cent))
  }

  # Construct vector of eigenvalues for GP covariance matrix; the
  # same eigenvalues are always used in prediction, so we only need to
  # create them when prepping the data. They will need to be included in
  # the Stan data list
  L <- brms:::choose_L(covariate_cent, boundary)
  eigenvalues <- vector()
  for(m in 1:k){
    eigenvalues[m] <- sqrt(lambda(boundary = L,
                                     m = m))
  }

  # Construct matrix of eigenfunctions; this will change depending on the values
  # of the covariate, so it needs to be computed and included as data but also needs
  # to be computed to make predictions
  eigenfunctions <- prep_eigenfunctions(data = data,
                                        covariate = covariate,
                                        by = by,
                                        level = level,
                                        k = k,
                                        boundary = boundary,
                                        mean = NA,
                                        max_dist = covariate_max_dist,
                                        scale = scale)

  # Make attributes table
  byname <- ifelse(is.na(by), '', paste0(':', by))
  covariate_name <- paste0('gp(', covariate, ')', byname)
  if(!is.na(level)){
    covariate_name <- paste0(covariate_name, level)
  }
  att_table <- list(effect = 'gp',
                    name = covariate_name,
                    covariate = covariate,
                    by = by,
                    level = level,
                    k = k,
                    boundary = boundary,
                    L = L,
                    scale = scale,
                    mean = covariate_mean,
                    max_dist = covariate_max_dist,
                    eigenvalues = eigenvalues)

  # Items to add to Stan data
  # Number of basis functions
  covariate_name <- gsub('(', '_', covariate_name, fixed = TRUE)
  covariate_name <- gsub(')', '_', covariate_name, fixed = TRUE)
  covariate_name <- gsub(':', 'by', covariate_name, fixed = TRUE)
  data_lines <- paste0('int<lower=1> k_', covariate_name, '; // basis functions for approximate gp\n')
  append_dat <- list(k = k)
  names(append_dat) <- paste0('k_', covariate_name, '')

  # Approximate GP eigenvalues
  data_lines <- paste0(data_lines, paste0(
    'vector[',
    'k_', covariate_name,
    '] l_', covariate_name, '; // approximate gp eigenvalues\n'),
    collapse = '\n')
  append_dat2 <- list(slambda = eigenvalues)
  names(append_dat2) <- paste0('l_', covariate_name, '')
  append_dat <- append(append_dat, append_dat2)

  # Return necessary objects in a list
  list(att_table = att_table,
       data_lines = data_lines,
       data_append = append_dat,
       eigenfunctions = eigenfunctions)
}

add_gp_model_file = function(model_file, model_data, mgcv_model, gp_additions){

  # Add data lines
  model_file[grep('int<lower=0> ytimes[n, n_series];',
                  model_file, fixed = TRUE)] <-
    paste0(model_file[grep('int<lower=0> ytimes[n, n_series];',
                           model_file, fixed = TRUE)],
           '\n',
           gp_additions$gp_stan_lines)
  model_file <- readLines(textConnection(model_file), n = -1)

  # Replace the multi_normal_prec lines with spd_cov_exp_quad
  gp_names <- unlist(purrr::map(attr(mgcv_model, 'gp_att_table'), 'name'))
  gp_names_clean <- gsub('(', '_', gp_names, fixed = TRUE)
  gp_names_clean <- gsub(')', '_', gp_names_clean, fixed = TRUE)
  gp_names_clean <- gsub(':', 'by', gp_names_clean, fixed = TRUE)
  s_to_remove <- list()
  for(i in seq_along(gp_names)){
    s_name <- gsub('gp(', 's(', gp_names[i], fixed = TRUE)
    to_replace <- grep(paste0('// prior for ', s_name, '...'),
                       model_file, fixed = TRUE) + 1
    pattern <- "S\\s*(.*?)\\s*\\["
    result <- regmatches(model_file[to_replace],
                         regexec(pattern, model_file[to_replace]))[[1]]
    s_to_remove[[i]] <- unique(unlist(regmatches(result,
                                                 gregexpr("[[:digit:]]+", result))))

    model_file[grep(paste0('// prior for ', s_name, '...'),
         model_file, fixed = TRUE)] <-
      gsub('s(', 'gp(', model_file[grep(paste0('// prior for ', s_name, '...'),
                                        model_file, fixed = TRUE)], fixed = TRUE)

    model_file[to_replace] <-
      paste0('z_',
             gp_names_clean[i],
             ' ~ std_normal();\n',
             'alpha_',
             gp_names_clean[i],
             ' ~ normal(0, 0.5);\n',
             'rho_',
             gp_names_clean[i],
             ' ~ inv_gamma(1.65, 5.97);\n',
             'b_raw[b_idx_',
             gp_names_clean[i],
             '] ~ std_normal();\n')
  }
  b_line <- max(grep('b[', model_file, fixed = TRUE))
  b_edits <- paste0('b[b_idx_',
                    gp_names_clean,
                    '] = sqrt(spd_cov_exp_quad(l_',
                    gp_names_clean,
                    ', alpha_',
                    gp_names_clean,
                    ', rho_',
                    gp_names_clean,
                    ')) .* z_',
                    gp_names_clean,
                    ';',
                    collapse = '\n')
  model_file[b_line] <- paste0(model_file[b_line],
                               '\n',
                               b_edits)
  model_file <- readLines(textConnection(model_file), n = -1)

  # Remove un-needed penalty matrices from the model file and the
  # model data
  for(i in seq_along(unique(unlist(s_to_remove)))){
    model_data[[paste0('S',
                       unique(unlist(s_to_remove))[i])]] <- NULL
    model_file <- model_file[-grep(paste0('mgcv smooth penalty matrix S',
                                          unique(unlist(s_to_remove))[i]),
                                   model_file, fixed = TRUE)]
  }

  # Add alpha, rho and z lines in parameters and model blocks
  alpha_names <- paste(paste0('real<lower=0> alpha_', gp_names_clean,
                              ';'),
                       collapse = '\n')
  rho_names <- paste(paste0('real<lower=0> rho_', gp_names_clean,
                            ';'),
                     collapse = '\n')
  z_names <- paste(paste0('vector[k_',
                          gp_names_clean,
                          '] z_',
                          gp_names_clean,
                          ';'),
                   collapse = '\n')
  model_file[grep("vector[num_basis] b_raw;", model_file, fixed = TRUE)] <-
    paste0("vector[num_basis] b_raw;\n\n",
           '// gp term sd parameters\n',
           alpha_names,
           '\n\n// gp term length scale parameters\n',
           rho_names,
           '\n\n// gp term latent variables\n',
           z_names,
           '\n')
  model_file <- readLines(textConnection(model_file), n = -1)

  # Add spd_cov_exp_quad function from brms code
  if(any(grepl('functions {', model_file, fixed = TRUE))){
    model_file[grep('functions {', model_file, fixed = TRUE)] <-
      paste0('functions {\n',
             '/* Spectral density function of a Gaussian process\n',
             '* with squared exponential covariance kernel\n',
             '* Args:\n',
             '*   l_gp: numeric eigenvalues of an SPD GP\n',
             '*   alpha_gp: marginal SD parameter\n',
             '*   rho_gp: length-scale parameter\n',
             '* Returns:\n',
             '*   numeric values of the GP function evaluated at l_gp\n',
             '*/\n',
             'vector spd_cov_exp_quad(data vector l_gp, real alpha_gp, real rho_gp) {\n',
             'int NB = size(l_gp);\n',
             'vector[NB] out;\n',
             'real constant = square(alpha_gp) * (sqrt(2 * pi()) * rho_gp);\n',
             'real neg_half_lscale2 = -0.5 * square(rho_gp);\n',
             'for (m in 1:NB) {\n',
             'out[m] = constant * exp(neg_half_lscale2 * square(l_gp[m]));\n',
             '}\n',
             'return out;\n',
             '}\n')
  } else {
    model_file[grep('Stan model code', model_file)] <-
      paste0('// Stan model code generated by package mvgam\n',
             'functions {\n',
             '/* Spectral density function of a Gaussian process\n',
             '* with squared exponential covariance kernel\n',
             '* Args:\n',
             '*   l_gp: numeric eigenvalues of an SPD GP\n',
             '*   alpha_gp: marginal SD parameter\n',
             '*   rho_gp: length-scale parameter\n',
             '* Returns:\n',
             '*   numeric values of the GP function evaluated at l_gp\n',
             '*/\n',
             'vector spd_cov_exp_quad(data vector l_gp, real alpha_gp, real rho_gp) {\n',
             'int NB = size(l_gp);\n',
             'vector[NB] out;\n',
             'real constant = square(alpha_gp) * (sqrt(2 * pi()) * rho_gp);\n',
             'real neg_half_lscale2 = -0.5 * square(rho_gp);\n',
             'for (m in 1:NB) {\n',
             'out[m] = constant * exp(neg_half_lscale2 * square(l_gp[m]));\n',
             '}\n',
             'return out;\n',
             '}\n}\n')
  }
  model_file <- readLines(textConnection(model_file), n = -1)

  return(list(model_file = model_file,
              model_data = model_data))
}
