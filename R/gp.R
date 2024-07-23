#' Specify dynamic Gaussian processes
#'
#' Set up low-rank approximate Gaussian Process trend models using Hilbert
#' basis expansions in \code{mvgam}. This function does not evaluate its arguments –
#' it exists purely to help set up a model with particular GP
#' trend models.
#' @param ... unused
#' @return An object of class \code{mvgam_trend}, which contains a list of
#' arguments to be interpreted by the parsing functions in \code{mvgam}
#' @details A GP trend is estimated for each series using
#' [Hilbert space approximate Gaussian Processes](https://arxiv.org/abs/2004.11408).
#' In `mvgam`, latent squared exponential GP trends are approximated using by
#' default \code{20} basis functions and using a multiplicative factor of `c = 5/4`,
#' which saves computational costs compared to fitting full GPs while adequately estimating
#' GP \code{alpha} and \code{rho} parameters.
#' @rdname GP
#' @seealso \code{\link[brms]{gp}}
#' @export
GP = function(...){
  out <- structure(list(trend_model = 'GP',
                        ma = FALSE,
                        cor = FALSE,
                        label = match.call()),
                   class = 'mvgam_trend')
}

#' Make gp() attributes table and necessary stan lines
#' @noRd
make_gp_additions = function(gp_details, data,
                             newdata,
                             model_data, mgcv_model,
                             gp_terms,
                             family = gaussian()){
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

  # Prepare the GP objects and Stan data lines
  gp_covariates <- gp_details$gp_covariates
  scale <- gp_details$scale
  boundary <- gp_details$boundary
  by <- gp_details$by
  level <- gp_details$level

  # Prep the covariates for GP modelling
  gp_data <- lapply(seq_along(gp_covariates), function(x){

    # Find the correct k to ensure that the total number of coefficients
    # when using gp(k = k) is the same as when using s(k = k + 1)
    smooth_terms <- unlist(paste0(purrr::map(mgcv_model$smooth, 'term')))
    smooth_bys <- unlist(purrr::map(mgcv_model$smooth, 'by'))
    if(any(smooth_bys == 'NA')){
      smooth_bys[smooth_bys == 'NA'] <- NA
    }
    term_k <- mgcv_model$smooth[[min(which(smooth_bys %in% by[x] &
                               smooth_terms == gp_covariates[x]))]]$df

    # Check that response terms use the cbind() syntax
    resp_terms <- rlang::f_lhs(formula(mgcv_model))
    if(length(resp_terms) == 1){
      response <- resp_terms
    } else {
      if(any(grepl('cbind', resp_terms))){
        resp_terms <- resp_terms[-grepl('cbind', resp_terms)]
        response <- resp_terms[1]
      }
    }

    prep_gp_covariate(data = data,
                      response = response,
                      covariate = gp_covariates[x],
                      by = by[x],
                      level = level[x],
                      scale = scale[x],
                      k = term_k,
                      boundary = boundary[x],
                      family = family_to_brmsfam(family))
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

  # Replace basis functions with gp() eigenfunctions
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
                          max_dist = max_dist[x],
                          initial_setup = TRUE)
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
    coef_indices <- which(grepl(paste0(gsub("([()])","\\\\\\1",
                                            gp_att_table[[covariate]]$name),
                                       '\\.+[0-9]'),
                                names(coef(mgcv_model)), fixed = FALSE) &
                            !grepl(paste0(gp_att_table[[covariate]]$name,':'),
                                   names(coef(mgcv_model)), fixed = TRUE) == TRUE)

    gp_att_table[[covariate]]$first_coef <- min(coef_indices)
    gp_att_table[[covariate]]$last_coef <- max(coef_indices)

    gp_names <- clean_gpnames(gp_att_table[[covariate]]$name)
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

  # Assign GP labels to smooths
  gp_assign <- data.frame(label = unlist(purrr::map(gp_att_table, 'name')),
                          first.para = unlist(purrr::map(gp_att_table, 'first_coef')),
                          last.para = unlist(purrr::map(gp_att_table, 'last_coef')),
                          by = unlist(purrr::map(gp_att_table, 'by')))
  for(i in seq_along(mgcv_model$smooth)){
    if(mgcv_model$smooth[[i]]$label %in%
       gsub('gp(', 's(', gp_assign$label, fixed = TRUE) &
       mgcv_model$smooth[[i]]$first.para %in% gp_assign$first.para){
      mgcv_model$smooth[[i]]$gp_term <- TRUE
      class(mgcv_model$smooth[[i]]) <- c('tprs.smooth', 'hilbert.smooth', 'mgcv.smooth')
    } else {
      mgcv_model$smooth[[i]]$gp_term <- FALSE
    }
  }

  # Return
  return(list(model_data = model_data,
              mgcv_model = mgcv_model,
              gp_stan_lines = gp_stan_lines,
              gp_att_table = gp_att_table))
}

#' Which terms are gp() terms?
#' @noRd
which_are_gp = function(formula){
  termlabs <- attr(terms(formula, keep.order = TRUE), 'term.labels')
  return(grep('gp(', termlabs, fixed = TRUE))
}

#' Convert gp() terms to s() terms for initial model construction
#' @importFrom stats drop.terms
#' @noRd
gp_to_s <- function(formula){

  # Extract details of gp() terms
  gp_details <- get_gp_attributes(formula)
  termlabs <- attr(terms(formula, keep.order = TRUE), 'term.labels')

  # Replace the gp() terms with s() for constructing the initial model
  which_gp <- which_are_gp(formula)
  response <- rlang::f_lhs(formula)
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

    termlabs[which_gp[i]] <- s_terms[i]
  }

  newformula <- reformulate(termlabs, rlang::f_lhs(formula))
  attr(newformula, '.Environment') <- attr(formula, '.Environment')
  return(newformula)
}


#' Store attributes of the gp terms
#' @importFrom rlang parse_expr
#' @noRd
get_gp_attributes = function(formula){
  gp_terms <- rownames(attr(terms(formula), 'factors'))[
    grep('gp(', rownames(attr(terms(formula), 'factors')), fixed = TRUE)]
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

#' @noRd
seq_cols <- function(x) {
  seq_len(NCOL(x))
}

#' Compute the mth eigen function of an approximate GP
#' Credit to Paul Burkner from brms: https://github.com/paul-buerkner/brms/R/formula-gp.R#L289
#' @noRd
eigen_fun_cov_exp_quad <- function(x, m, L) {
  x <- as.matrix(x)
  D <- ncol(x)
  stopifnot(length(m) == D, length(L) == D)
  out <- vector("list", D)
  for (i in seq_cols(x)) {
    out[[i]] <- 1 / sqrt(L[i]) *
      sin((m[i] * pi) / (2 * L[i]) * (x[, i] + L[i]))
  }
  Reduce("*", out)
}

#' Compute squared differences
#' Credit to Paul Burkner from brms: https://github.com/paul-buerkner/brms/R/formula-gp.R#L241
#' @param x vector or matrix
#' @param x_new optional vector of matrix with the same ncol as x
#' @return an nrow(x) times nrow(x_new) matrix
#' @details if matrices are passed results are summed over the columns
#' @noRd
diff_quad <- function(x, x_new = NULL) {
  x <- as.matrix(x)
  if (is.null(x_new)) {
    x_new <- x
  } else {
    x_new <- as.matrix(x_new)
  }
  .diff_quad <- function(x1, x2) (x1 - x2)^2
  out <- 0
  for (i in seq_cols(x)) {
    out <- out + outer(x[, i], x_new[, i], .diff_quad)
  }
  out
}

#' Extended range of input data for which predictions should be made
#' Credit to Paul Burkner from brms: https://github.com/paul-buerkner/brms/R/formula-gp.R#L301
#' @noRd
choose_L <- function(x, c) {
  if (!length(x)) {
    range <- 1
  } else {
    range <- max(1, max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  }
  c * range
}

#' Mean-center and scale the particular covariate of interest
#' so that the maximum Euclidean distance between any two points is 1
#' @noRd
scale_cov <- function(data, covariate, by, level,
                      mean, max_dist){
  Xgp <- data[[covariate]]
  if(!is.na(by) &
     !is.na(level)){
      Xgp <- data[[covariate]][data[[by]] == level]
   }

  # Compute max Euclidean distance if not supplied
  if(is.na(max_dist)){
    Xgp_max_dist <- sqrt(max(diff_quad(Xgp)))
  } else {
    Xgp_max_dist <- max_dist
  }

  # Scale
  Xgp <- Xgp / Xgp_max_dist

  # Compute mean if not supplied (after scaling)
  if(is.na(mean)){
    Xgp_mean <- mean(Xgp, na.rm = TRUE)
  } else {
    Xgp_mean <- mean
  }

  # Center
  Xgp <- Xgp - Xgp_mean

  return(list(Xgp = Xgp,
              Xgp_mean = Xgp_mean,
              Xgp_max_dist = Xgp_max_dist))
}

#' Prep GP eigenfunctions
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
                               L,
                               initial_setup = FALSE){

  # Extract and scale covariate (scale set to FALSE if this is a prediction
  # step so that we can scale by the original training covariate values supplied
  # in mean and max_dist)
  covariate_cent <- scale_cov(data = data,
                              covariate = covariate,
                              by = by,
                              level = level,
                              mean = mean,
                              max_dist = max_dist)$Xgp

  # Construct matrix of eigenfunctions
  eigenfunctions <- matrix(NA, nrow = length(covariate_cent),
                           ncol = k)
  if(missing(L)){
    L <- choose_L(covariate_cent, boundary)
  }

  for(m in 1:k){
    eigenfunctions[, m] <- eigen_fun_cov_exp_quad(x = matrix(covariate_cent),
                                                  m = m,
                                                  L = L)
  }

  # Multiply eigenfunctions by the 'by' variable if one is supplied
  if(!is.na(by)){
    if(!is.na(level)){
      # no multiplying needed as this is a factor by variable,
      # but we need to pad the eigenfunctions with zeros
      # for the observations where the by is a different level;
      # the design matrix is always sorted by time and then by series
      # in mvgam
      if(initial_setup){
        sorted_by <- data.frame(time = data$time,
                                series = data$series,
                                byvar = data[[by]]) %>%
          dplyr::arrange(time, series) %>%
          dplyr::pull(byvar)
      } else {
        sorted_by <- data[[by]]
      }

      full_eigens <- matrix(0, nrow = length(data[[by]]),
                            ncol = NCOL(eigenfunctions))
      full_eigens[(1:length(data[[by]]))[
        sorted_by == level],] <- eigenfunctions
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
                             response,
                             covariate,
                             by = NA,
                             level = NA,
                             scale = TRUE,
                             boundary = 5.0/4,
                             k = 20,
                             family = gaussian()){

  # Get default gp param priors from a call to brms::get_prior()
  def_gp_prior <- suppressWarnings(brms::get_prior(formula(paste0(response,
                                                                  ' ~ gp(', covariate,
                                                 ifelse(is.na(by), ', ',
                                                        paste0(', by = ', by, ', ')),
                                                 'k = ', k,
                                                 ', scale = ',
                                                 scale,
                                                 ', c = ',
                                                 boundary,
                                                 ')')), data = data,
                                                 family = family))
  def_gp_prior <- def_gp_prior[def_gp_prior$prior != '',]
  def_rho <- def_gp_prior$prior[min(which(def_gp_prior$class == 'lscale'))]
  if(def_rho == ''){
    def_rho <- 'inv_gamma(1.5, 5);'
  }
  def_alpha <- def_gp_prior$prior[min(which(def_gp_prior$class == 'sdgp'))]
  if(def_alpha == ''){
    def_alpha<- 'student_t(3, 0, 2.5);'
  }

  # Prepare the covariate
  if(scale){
    max_dist <- NA
  } else {
    max_dist <- 1
  }

  covariate_cent <- scale_cov(data = data,
                              covariate = covariate,
                              by = by,
                              mean = NA,
                              max_dist = max_dist,
                              level = level)

  covariate_mean <- covariate_cent$Xgp_mean
  covariate_max_dist <- covariate_cent$Xgp_max_dist
  covariate_cent <- covariate_cent$Xgp

  # Construct vector of eigenvalues for GP covariance matrix; the
  # same eigenvalues are always used in prediction, so we only need to
  # create them when prepping the data. They will need to be included in
  # the Stan data list
  L <- choose_L(covariate_cent, boundary)
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
                                        L = L,
                                        k = k,
                                        boundary = boundary,
                                        mean = covariate_mean,
                                        max_dist = covariate_max_dist,
                                        scale = scale,
                                        initial_setup = TRUE)

  # Make attributes table using a cleaned version of the covariate
  # name to ensure there are no illegal characters in the Stan code
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
                    def_rho = def_rho,
                    def_alpha = def_alpha,
                    mean = covariate_mean,
                    max_dist = covariate_max_dist,
                    eigenvalues = eigenvalues)

  # Items to add to Stan data
  # Number of basis functions
  covariate_name <- clean_gpnames(covariate_name)
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

#' Clean GP names so no illegal characters are used in Stan code
#' @noRd
clean_gpnames = function(gp_names){
  gp_names_clean <- gsub(' ', '_', gp_names, fixed = TRUE)
  gp_names_clean <- gsub('(', '_', gp_names_clean, fixed = TRUE)
  gp_names_clean <- gsub(')', '_', gp_names_clean, fixed = TRUE)
  gp_names_clean <- gsub(':', 'by', gp_names_clean, fixed = TRUE)
  gp_names_clean <- gsub('.', '_', gp_names_clean, fixed = TRUE)
  gp_names_clean <- gsub(']', '_', gp_names_clean, fixed = TRUE)
  gp_names_clean <- gsub('[', '_', gp_names_clean, fixed = TRUE)
  gp_names_clean <- gsub(';', '_', gp_names_clean, fixed = TRUE)
  gp_names_clean <- gsub(':', '_', gp_names_clean, fixed = TRUE)
  gp_names_clean <- gsub("'", "", gp_names_clean, fixed = TRUE)
  gp_names_clean <- gsub("\"", "", gp_names_clean, fixed = TRUE)
  gp_names_clean <- gsub("%", "percent", gp_names_clean, fixed = TRUE)
  gp_names_clean <- gsub("[.]+", "_", gp_names_clean, fixed = TRUE)
  gp_names_clean <- gsub("'", "", gp_names_clean, fixed = TRUE)
  #gp_names_clean <- gsub("’", "", gp_names_clean, fixed = TRUE)
  gp_names_clean
}

#' Update a Stan file with GP information
#' @noRd
add_gp_model_file = function(model_file, model_data, mgcv_model, gp_additions){

  rho_priors <- unlist(purrr::map(gp_additions$gp_att_table, 'def_rho'))
  alpha_priors <- unlist(purrr::map(gp_additions$gp_att_table, 'def_alpha'))

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
  gp_names_clean <- clean_gpnames(gp_names)
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
             ' ~ ',
             alpha_priors[i],
             ';\n',
             'rho_',
             gp_names_clean[i],
             ' ~ ',
             rho_priors[i],
             ';\n',
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
    model_file <- model_file[-grep(paste0('\\bmgcv smooth penalty matrix S',
                                          unique(unlist(s_to_remove))[i],
                                          '\\b'),
                                   model_file)]
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
  if(!any(grepl('/* Spectral density function of a Gaussian process',
                model_file, fixed = TRUE))){
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
  }
  model_file <- readLines(textConnection(model_file), n = -1)

  return(list(model_file = model_file,
              model_data = model_data))
}
