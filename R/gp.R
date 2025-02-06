#' Re-label gp terms inside an mgcv gam object for nicer plotting
#' @noRd
relabel_gps = function(mgcv_model){
  if(length(mgcv_model$smooth) > 0L){
    # Get classes of all smooths
    smooth_classes <- purrr::map(mgcv_model$smooth,
                                 class)

    # Check for gp() terms
    for(x in seq_along(smooth_classes)){
      if(any(smooth_classes[[x]] %in% 'hilbert.smooth')){
        mgcv_model$smooth[[x]]$label <-
          gsub('s\\(|ti\\(', 'gp(',
               mgcv_model$smooth[[x]]$label)
      }
    }
  }
  return(mgcv_model)
}

#' @noRd
seq_cols <- function(x) {
  seq_len(NCOL(x))
}

#' Make gp() attributes table and necessary stan lines
#' @importFrom brms brm standata
#' @noRd
make_gp_additions = function(gp_details,
                             orig_formula,
                             data,
                             newdata,
                             model_data,
                             mgcv_model,
                             gp_terms,
                             family = gaussian(),
                             rho_names){
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

  # Initiate a brms GP model using the 'mock' backend so it doesn't actually fit;
  terms_needed <- unique(c(unlist(strsplit(gp_details$gp_covariates, ", |\\n")),
                           unlist(strsplit(gp_details$by, ", |\\n"))))
  terms_needed <- terms_needed[!is.na(terms_needed)]
  terms_needed <- terms_needed[!terms_needed %in% c('series', 'time')]
  brms_fake_df <- data.frame(.fake_gp_y = rnorm(length(data[[1]])),
                             series = data$series,
                             time = data$index..time..index)
  for(i in seq_along(terms_needed)){
    brms_fake_df <- cbind(brms_fake_df, data[[terms_needed[i]]])
  }
  colnames(brms_fake_df) <- c('.fake_gp_y',
                              'series',
                              'time',
                              terms_needed)

  brms_fake_df <- brms_fake_df %>%
    dplyr::arrange(time, series)

  # Build the gp formula to pass to the mock brms
  gp_formula <- reformulate(attr(terms(attr(gp_details, 'gp_formula')),
                                 'term.labels'),
                            '.fake_gp_y')
  brms_mock <- brms::brm(gp_formula,
                         data = brms_fake_df,
                         mock_fit = 1,
                         backend = "mock",
                         rename = FALSE)
  brms_mock <- trim_mockbrms(brms_mock)

  # Eigenfunction design matrices (to be inserted into Xp matrices)
  brms_mock_data <- brms::standata(brms_mock)
  eigenfuncs <- eigenfunc_list(stan_data = brms_mock_data,
                               mock_df = brms_fake_df,
                               by = gp_details$by,
                               level = gp_details$level)

  # Eigenvalues  (l_gp in mvgam stancode)
  eigenvals <- eigenval_list(brms_mock_data)

  # If newdata supplied, compute the eigenfunctions for these out of
  # sample data points
  if(!is.null(newdata)){
    brms_fake_df_new <- data.frame(.fake_gp_y = rnorm(length(newdata[[1]])),
                                   series = newdata$series,
                                   time = newdata$index..time..index)
    for(i in seq_along(terms_needed)){
      brms_fake_df_new <- cbind(brms_fake_df_new,
                                newdata[[terms_needed[i]]])
    }
    colnames(brms_fake_df_new) <- c('.fake_gp_y',
                                    'series',
                                    'time',
                                    terms_needed)

    brms_fake_df_new <- brms_fake_df_new %>%
      dplyr::arrange(time, series)

    # Compute eigenfunctions for these new data and bind to the
    # training data eigenfunctions
    brms_mock_data_new <- brms::standata(brms_mock,
                                         newdata = brms_fake_df_new ,
                                         internal = TRUE)
    eigenfuncs_new <- eigenfunc_list(stan_data = brms_mock_data_new,
                                     mock_df = brms_fake_df_new,
                                     by = gp_details$by,
                                     level = gp_details$level)
    for(i in seq_along(eigenfuncs)){
      eigenfuncs[[i]] <- rbind(eigenfuncs[[i]],
                               eigenfuncs_new[[i]])
    }
  }

  # Numbers of basis functions (k_gp in mvgam stancode)
  k_gps <- lapply(eigenvals, function(x) NROW(x))

  # Put all relevant data into a list
  gp_data <- lapply(seq_along(eigenvals), function(x){
    byname <- ifelse(is.na(gp_details$by[x]), '', paste0(':', gp_details$by[x]))
    covariate_name <- paste0('gp(', gp_details$gp_covariates[x], ')', byname)
    if(!is.na(gp_details$level[x])){
      covariate_name <- paste0(covariate_name, gp_details$level[x])
    }
    orig_name <- if(gp_details$dim[x] > 1L){
      paste0('ti(', gp_details$gp_covariates[x], ')', byname)
    } else {
      paste0('s(', gp_details$gp_covariates[x], ')', byname)
    }
    if(!is.na(gp_details$level[x])){
      orig_name <- paste0(orig_name, gp_details$level[x])
    }
    att_table <- list(effect = 'gp',
                      name = covariate_name,
                      orig_name = orig_name,
                      dim = gp_details$dim[x],
                      iso = gp_details$iso[x],
                      kernel = gp_details$kernel[x],
                      covariate = gp_details$gp_covariates[x],
                      by = gp_details$b[x],
                      level = gp_details$level[x],
                      k = k_gps[[x]],
                      def_rho = gp_details$def_rho[x],
                      def_rho_2 = gp_details$def_rho_2[x],
                      def_rho_3 = gp_details$def_rho_3[x],
                      def_rho_4 = gp_details$def_rho_4[x],
                      def_alpha = gp_details$def_alpha[x],
                      eigenvalues = eigenvals[[x]])

    # Items to add to Stan data
    # Number of basis functions
    covariate_name <- clean_gpnames(covariate_name)
    data_lines <- paste0('int<lower=1> k_', covariate_name,
                         '; // basis functions for approximate gp\n')
    append_dat <- list(k = k_gps[[x]])
    names(append_dat) <- paste0('k_', covariate_name, '')

    # Approximate GP eigenvalues
    data_lines <- paste0(data_lines, paste0(
      'array[',
      'k_', covariate_name,
      '] vector[',
      gp_details$dim[x],
      '] l_',
      covariate_name, '; // approximate gp eigenvalues\n'),
      collapse = '\n')

    append_dat2 <- list(slambda = eigenvals[[x]])
    names(append_dat2) <- paste0('l_', covariate_name, '')
    append_dat <- append(append_dat, append_dat2)

    # Return necessary objects in a list
    list(att_table = att_table,
         data_lines = data_lines,
         data_append = append_dat,
         eigenfunctions = eigenfuncs[[x]])
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
      if(grepl('ti(', label, fixed = TRUE)){
        coef_name <- paste0('ti(', paste(s_attributes$term,
                                         collapse = ','),
                            '):', s_attributes$by)
      } else {
        coef_name <- paste0('s(', s_attributes$term, '):', s_attributes$by)
      }

    } else {
      if(grepl('ti(', label, fixed = TRUE)){
        coef_name <- paste0('ti(', paste(s_attributes$term,
                                         collapse = ','),
                            ')')
      } else {
        coef_name <- paste0('s(', s_attributes$term, ')')
      }
    }
    which_replace <- grep(coef_name, names(coef(mgcv_model)), fixed = TRUE)
    names(mgcv_model$coefficients)[which_replace] <-
      if(grepl('ti(', label, fixed = TRUE)){
        gsub('ti(', 'gp(', names(mgcv_model$coefficients)[which_replace],
             fixed = TRUE)
      } else {
        gsub('s(', 'gp(', names(mgcv_model$coefficients)[which_replace],
             fixed = TRUE)
      }
    coefs_replace[[x]] <- which_replace
  }

  # Replace basis functions with gp() eigenfunctions
  newX <- model_data$X

  # Add eigenfunctions to the GAM design matrix
  eigenfuncs <- do.call(cbind, purrr::map(gp_data, 'eigenfunctions'))
  newX[unlist(coefs_replace), ] <- t(eigenfuncs)
  model_data$X <- newX

  # Consolidate Stan data lines
  gp_stan_lines <- paste0(purrr::map(gp_data, 'data_lines'), collapse = '')

  # Add coefficient indices to attribute table and to Stan data
  for(covariate in seq_along(gp_att_table)){
    coef_indices <- which(grepl(paste0(gsub(' ' , '',
                                            gsub("([()])","\\\\\\1",
                                                 gp_att_table[[covariate]]$name),
                                            '\\.+[0-9]')),
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

  # Add the GP attribute table and mock brmsfit object to the mgcv_model
  attr(mgcv_model, 'gp_att_table') <- gp_att_table
  attr(mgcv_model, 'brms_mock') <- brms_mock

  # Assign GP labels to smooths
  gp_assign <- data.frame(label = unlist(purrr::map(gp_att_table, 'name')),
                          first.para = unlist(purrr::map(gp_att_table, 'first_coef')),
                          last.para = unlist(purrr::map(gp_att_table, 'last_coef')),
                          by = unlist(purrr::map(gp_att_table, 'by')))
  for(i in seq_along(mgcv_model$smooth)){
    if(mgcv_model$smooth[[i]]$label %in%
       gsub('gp(', 's(', gsub(' ', '', gp_assign$label[i]), fixed = TRUE) ||
       mgcv_model$smooth[[i]]$label %in%
       gsub('gp(', 'ti(', gsub(' ', '', gp_assign$label[i]), fixed = TRUE) &
       mgcv_model$smooth[[i]]$first.para %in% gp_assign$first.para){
      mgcv_model$smooth[[i]]$gp_term <- TRUE
      class(mgcv_model$smooth[[i]]) <- c(class(mgcv_model$smooth[[i]])[1],
                                         'hilbert.smooth', 'mgcv.smooth')
    } else {
      mgcv_model$smooth[[i]]$gp_term <- FALSE
    }
  }

  # Update smoothing parameter names and return
  if(!missing(rho_names)){
    gp_names <- unlist(purrr::map(gp_att_table, 'name'))
    gp_names_new <- vector()
    for(i in seq_along(gp_names)){
      if(any(grepl(',', gp_names[i]))){
        gp_names_new[i] <- gsub(' ', '', gsub('gp(', 'ti(', gp_names[i], fixed = TRUE))
      } else {
        gp_names_new[i] <- gsub('gp(', 's(', gp_names[i], fixed = TRUE)
      }
    }

    rhos_change <- list()
    for(i in seq_along(gp_names_new)){
      rhos_change[[i]] <- grep(gp_names_new[i], rho_names, fixed = TRUE)
    }
    rho_names[c(unique(unlist(rhos_change)))] <- gsub('s\\(|ti\\(', 'gp(',
                                                      rho_names[c(unique(unlist(rhos_change)))])
  } else {
    rho_names <- NULL
  }

  # Return
  return(list(model_data = model_data,
              mgcv_model = mgcv_model,
              gp_stan_lines = gp_stan_lines,
              gp_att_table = gp_att_table,
              rho_names))
}

#' Reduce the size of the brmsfit object
#' @noRd
trim_mockbrms = function(brms_mock){
  brms_mock$opencl <- NULL
  brms_mock$data.name <- NULL
  brms_mock$algorithm <- NULL
  brms_mock$backend <- NULL
  brms_mock$stan_args <- NULL
  brms_mock$model <- NULL
  brms_mock$stan_funs <- NULL
  brms_mock$threads <- NULL
  brms_mock$prior <- NULL
  brms_mock$family <- NULL
  brms_mock$save_pars <- NULL
  brms_mock
}

#' Extract eigenfunctions for gp() terms and pad with zeros if necessary
#' @noRd
eigenfunc_list = function(stan_data,
                          mock_df,
                          by = NA,
                          level = NA){
  eigenfuncs <- stan_data[which(grepl('Xgp_', names(stan_data), fixed = TRUE) &
                                  !grepl('_old', names(stan_data), fixed = TRUE) &
                                  !grepl('_prior', names(stan_data), fixed = TRUE))]
  # We need to pad the eigenfunctions with zeros
  # for the observations where the by is a different level;
  padded_eigenfuncs <- lapply(seq_along(eigenfuncs), function(x){
    if(!is.na(by[x])){
      if(!is.na(level[x])){
        sorted_by <- mock_df[[by[x]]]
        full_eigens <- matrix(0, nrow = length(sorted_by),
                              ncol = NCOL(eigenfuncs[[x]]))
        full_eigens[(seq_along(sorted_by))[
          sorted_by == level[x]],] <- eigenfuncs[[x]]
      } else {
        # Numeric by variables should be multiplied by the
        # spectral eigenfunctions
        full_eigens <- eigenfuncs[[x]] * mock_df[[by[x]]]
      }
    } else {
      full_eigens <- eigenfuncs[[x]]
    }
    full_eigens
  })
  padded_eigenfuncs
}

#' Extract eigenvalues for gp() terms
#' @noRd
eigenval_list = function(stan_data){
  stan_data[which(grepl('slambda_', names(stan_data), fixed = TRUE) &
                    !grepl('_old', names(stan_data), fixed = TRUE))]
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
gp_to_s <- function(formula, data, family){

  # Extract details of gp() terms
  gp_details <- get_gp_attributes(formula, data, family)
  termlabs <- attr(terms(formula, keep.order = TRUE), 'term.labels')

  # Replace the gp() terms with s() for constructing the initial model
  which_gp <- which_are_gp(formula)
  response <- rlang::f_lhs(formula)
  s_terms <- vector()
  for(i in 1:NROW(gp_details)){
    if(!is.na(gp_details$by[i])){
      if(is.factor(data[[gp_details$by[i]]])){
        # For terms with factor by variables, constraints are in place
        # we either need one additional
        # value for k (for unidimensionsal terms) or must use mc = 0 for all
        # marginals in a ti call (for multidimensional terms)
        s_terms[i] <- paste0(
          if(gp_details$dim[i] < 2L){
            's('} else {
              'ti('
            },
          gp_details$gp_covariates[i],
          ', by = ',
          gp_details$by[i],
          ', k = ',
          if(gp_details$dim[i] > 1L){
            paste0(gp_details$k[i],
                   ', mc = c(',
                   paste(rep(0, gp_details$dim[i]), collapse = ', '),
                   ')')} else {
                     gp_details$k[i] + 1
                   },')')
      } else {
        # No constraints are used when numeric by variables are in smooths,
        # so number of coefficients will match those from the brms gp
        s_terms[i] <- paste0(
          if(gp_details$dim[i] < 2L){
            's('} else {
              'ti('
            },
          gp_details$gp_covariates[i],
          ', by = ',
          gp_details$by[i],
          ', k = ',
          gp_details$k[i], ')')
      }

    } else {
      # For terms with no by-variable, we either need one additional
      # value for k (for unidimensionsal terms) or must use mc = 0 for all
      # marginals in a ti call (for multidimensional terms)
      s_terms[i] <- paste0(
        if(gp_details$dim[i] < 2L){
          's('} else {
            'ti('
          },
        gp_details$gp_covariates[i],
        ', k = ',
        if(gp_details$dim[i] > 1L){
          paste0(gp_details$k[i],
                 ', mc = c(',
                 paste(rep(0, gp_details$dim[i]), collapse = ', '),
                 ')')} else {
            gp_details$k[i] + 1
          },')')
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
get_gp_attributes = function(formula, data, family = gaussian()){
  gp_terms <- rownames(attr(terms(formula), 'factors'))[
    grep('gp(', rownames(attr(terms(formula), 'factors')), fixed = TRUE)]

  # Term details and default priors
  gp_attributes <- lapply(seq_along(gp_terms), function(x){
    eval(rlang::parse_expr(gp_terms[x]))
  })

  gp_isos <- unlist(purrr::map(gp_attributes, 'iso'),
                    use.names = FALSE)
  gp_kernels <- unlist(purrr::map(gp_attributes, 'cov'),
                       use.names = FALSE)
  gp_cmcs <- unlist(purrr::map(gp_attributes, 'cmc'),
                    use.names = FALSE)
  if(any(gp_cmcs == FALSE)){
    rlang::warn(paste0("gp effects in mvgam cannot yet handle contrast coding\n",
                       "resetting all instances of 'cmc = FALSE' to 'cmc = TRUE'"),
                .frequency = "once",
                .frequency_id = 'gp_cmcs')
  }
  gp_grs <- unlist(purrr::map(gp_attributes, 'gr'),
                   use.names = FALSE)
  if(any(gp_grs == TRUE)){
    rlang::warn(paste0("gp effects in mvgam cannot yet handle autogrouping\n",
                       "resetting all instances of 'gr = TRUE' to 'gr = FALSE'"),
                .frequency = "once",
                .frequency_id = 'gp_grs')
  }

  newgp_terms <- unlist(lapply(seq_along(gp_terms), function(x){
    lbl <- paste0('gp(',
                  paste(gp_attributes[[x]]$term, collapse = ', '),
                  if(gp_attributes[[x]]$by != 'NA'){
                    paste0(', by = ',
                           gp_attributes[[x]]$by)
                  } else {
                    NULL
                  },
                  ', k = ',
                  gp_attributes[[x]]$k,
                  ', cov = "',
                  gp_attributes[[x]]$cov,
                  '", iso = ',
                  gp_attributes[[x]]$iso,
                  ', scale = ',
                  gp_attributes[[x]]$scale,
                  ', c = ',
                  gp_attributes[[x]]$c[1],
                  ', gr = FALSE, cmc = TRUE)')

  }), use.names = FALSE)

  gp_formula <- reformulate(newgp_terms,
                            rlang::f_lhs(formula))

  gp_def_priors <- do.call(rbind, lapply(seq_along(gp_terms), function(x){
    def_gp_prior <- suppressWarnings(brms::get_prior(
      reformulate(newgp_terms[x],
                  rlang::f_lhs(formula)),
      family = family_to_brmsfam(family),
      data = data))
    def_gp_prior <- def_gp_prior[def_gp_prior$prior != '',]
    def_rho <- def_gp_prior$prior[which(def_gp_prior$class == 'lscale')]
    def_alpha <- def_gp_prior$prior[min(which(def_gp_prior$class == 'sdgp'))]
    if(def_alpha == ''){
      def_alpha <- 'student_t(3, 0, 2.5);'
    }
    if(length(def_rho) > 1L){
      def_rho_1 <- def_rho[1]
      def_rho_2 <- def_rho[2]
      out <- data.frame(def_rho = def_rho_1,
                        def_rho_2 = def_rho_2,
                        def_rho_3 = NA,
                        def_rho_4 = NA,
                        def_alpha = def_alpha)
      if(length(def_rho) > 2L) out$def_rho_3 <- def_rho[3]
      if(length(def_rho) > 3L) out$def_rho_4 <- def_rho[4]
    } else {
      out <- data.frame(def_rho = def_rho,
                        def_rho_2 = NA,
                        def_rho_3 = NA,
                        def_rho_4 = NA,
                        def_alpha = def_alpha)
    }
    out
  }))

  # Extract information necessary to construct the GP terms
  gp_terms <- purrr::map(gp_attributes, 'term')
  gp_dims <- unlist(lapply(gp_terms, length), use.names = FALSE)
  gp_covariates <- unlist(lapply(gp_terms, function(x){
    paste(x, collapse = ', ')
  }), use.names = FALSE)
  k <- unlist(purrr::map(gp_attributes, 'k'))
  if(any(is.na(k))){
    stop('argument "k" must be supplied for any gp() terms',
         call. = FALSE)
  }

  # No longer will need boundary or scale information as
  # brms will handle this internally
  by <- unlist(purrr::map(gp_attributes, 'by'), use.names = FALSE)
  if(any(by == 'NA')){
    by[by == 'NA'] <- NA
  }

  ret_dat <- data.frame(gp_covariates,
                        dim = gp_dims,
                        kernel = gp_kernels,
                        iso = gp_isos,
                        k = k,
                        by,
                        level = NA,
                        def_alpha = gp_def_priors$def_alpha,
                        def_rho = gp_def_priors$def_rho,
                        def_rho_2 = gp_def_priors$def_rho_2,
                        def_rho_3 = gp_def_priors$def_rho_3,
                        def_rho_4 = gp_def_priors$def_rho_4)
  attr(ret_dat, 'gp_formula') <- gp_formula

  # Return as a data.frame
  return(ret_dat)
}


#' Clean GP names so no illegal characters are used in Stan code
#' @noRd
clean_gpnames = function(gp_names){
  gp_names_clean <- gsub(' ', '_', gp_names, fixed = TRUE)
  gp_names_clean <- gsub('(', '_', gp_names_clean, fixed = TRUE)
  gp_names_clean <- gsub(')', '_', gp_names_clean, fixed = TRUE)
  gp_names_clean <- gsub(',', 'by', gp_names_clean, fixed = TRUE)
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
add_gp_model_file = function(model_file, model_data,
                             mgcv_model, gp_additions){

  rho_priors <- unlist(purrr::map(gp_additions$gp_att_table, 'def_rho'),
                       use.names = FALSE)
  rho_2_priors <- unlist(purrr::map(gp_additions$gp_att_table, 'def_rho_2'),
                       use.names = FALSE)
  rho_3_priors <- unlist(purrr::map(gp_additions$gp_att_table, 'def_rho_3'),
                         use.names = FALSE)
  rho_4_priors <- unlist(purrr::map(gp_additions$gp_att_table, 'def_rho_4'),
                         use.names = FALSE)
  alpha_priors <- unlist(purrr::map(gp_additions$gp_att_table, 'def_alpha'),
                         use.names = FALSE)

  # Add data lines
  model_file[grep('int<lower=0> ytimes[n, n_series];',
                  model_file, fixed = TRUE)] <-
    paste0(model_file[grep('int<lower=0> ytimes[n, n_series];',
                           model_file, fixed = TRUE)],
           '\n',
           gp_additions$gp_stan_lines)
  model_file <- readLines(textConnection(model_file), n = -1)

  # Replace the multi_normal_prec lines with the relevant spd function
  gp_kernels <- unlist(purrr::map(attr(mgcv_model, 'gp_att_table'), 'kernel'),
                       use.names = FALSE)
  gp_names <- unlist(purrr::map(attr(mgcv_model, 'gp_att_table'), 'name'),
                     use.names = FALSE)
  gp_isos <- unlist(purrr::map(attr(mgcv_model, 'gp_att_table'), 'iso'),
                    use.names = FALSE)
  gp_dims <- unlist(purrr::map(attr(mgcv_model, 'gp_att_table'), 'dim'),
                    use.names = FALSE)
  orig_names <- unlist(purrr::map(attr(mgcv_model, 'gp_att_table'), 'orig_name'),
                       use.names = FALSE)
  gp_names_clean <- clean_gpnames(gp_names)
  s_to_remove <- list()
  for(i in seq_along(gp_names)){
    i_rho_priors <- c(rho_priors[i],
                      rho_2_priors[i],
                      rho_3_priors[i],
                      rho_4_priors[i])
    i_rho_priors <- i_rho_priors[!is.na(i_rho_priors)]
    s_name <- gsub(' ', '', orig_names[i])
    to_replace <- grep(paste0('// prior for ', s_name, '...'),
                       model_file, fixed = TRUE) + 1
    pattern <- "S\\s*(.*?)\\s*\\["
    result <- regmatches(model_file[to_replace],
                         regexec(pattern, model_file[to_replace]))[[1]]
    s_to_remove[[i]] <- unique(unlist(regmatches(result,
                                                 gregexpr("[[:digit:]]+", result))))

    model_file[grep(paste0('// prior for ', s_name, '...'),
         model_file, fixed = TRUE)] <-
      gsub('s\\(|ti\\(', 'gp(', model_file[grep(paste0('// prior for ', s_name, '...'),
                                        model_file, fixed = TRUE)])

    rho_prior_lines <- paste(
      paste0('rho_',
             gp_names_clean[i],
             if(!gp_isos[i]){
               '[1]'
             } else {
               NULL
             },
             '[',
             if(gp_isos[i]){
               1
             } else {
               seq(1:gp_dims[i])
               },
             ']',
             ' ~ ',
             if(gp_isos[i]){
               rho_priors[i]
             } else {
               i_rho_priors
             },
             ';\n'),
             collapse = '\n'
      )

    model_file[to_replace] <-
      paste0('z_',
             gp_names_clean[i],
             ' ~ std_normal();\n',
             'alpha_',
             gp_names_clean[i],
             ' ~ ',
             alpha_priors[i],
             ';\n',
             rho_prior_lines,
             'b_raw[b_idx_',
             gp_names_clean[i],
             '] ~ std_normal();\n')
  }
  b_line <- max(which(grepl('b[', model_file, fixed = TRUE) &
                  grepl('] =', model_file, fixed = TRUE)))
  b_edits <- paste0('b[b_idx_',
                    gp_names_clean,
                    add_gp_spd_calls(gp_kernels),
                    gp_names_clean,
                    ', alpha_',
                    gp_names_clean,
                    ', rho_',
                    gp_names_clean,
                    '[1])) .* z_',
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
  alpha_names <- paste(paste0('real<lower=0> alpha_',
                              gp_names_clean,
                            ';'),
                     collapse = '\n')

  rho_names <- paste(paste0('array[1] vector<lower=0>[',
                              ifelse(gp_isos, 1, gp_dims),
                              '] rho_', gp_names_clean,
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

  # Add spd_ functions from brms code
  kerns_add <- rev(gp_kernels)
  for(i in seq_along(kerns_add)){
    model_file <- add_gp_spd_funs(model_file, kerns_add[i])
  }

  return(list(model_file = model_file,
              model_data = model_data))
}

#' Add GP SPD functions to a stan model file
#' @noRd
add_gp_spd_calls = function(kernels){
  kern_calls <- vector(length = length(kernels))
  for(i in seq_along(kern_calls)){
    if(kernels[i] == 'exp_quad'){
      kern_calls[i] <- '] = sqrt(spd_gp_exp_quad(l_'
    }
    if(kernels[i] == 'exponential'){
      kern_calls[i] <- '] = sqrt(spd_gp_exponential(l_'
    }
    if(kernels[i] == 'matern32'){
      kern_calls[i] <- '] = sqrt(spd_gp_matern32(l_'
    }
    if(kernels[i] == 'matern52'){
      kern_calls[i] <- '] = sqrt(spd_gp_matern52(l_'
    }
  }
  return(kern_calls)
}

#' @noRd
add_gp_spd_funs = function(model_file, kernel){
  if(kernel == 'exp_quad'){
    if(!any(grepl('/* Spectral density of a squared exponential Gaussian process',
                  model_file, fixed = TRUE))){
      fun_lines <- paste0('/* Spectral density of a squared exponential Gaussian process\n',
                          '* Args:\n',
                          '*   x: array of numeric values of dimension NB x D\n',
                          '*   sdgp: marginal SD parameter\n',
                          '*   lscale: vector of length-scale parameters\n',
                          '* Returns:\n',
                          "*   numeric vector of length NB of the SPD evaluated at 'x'\n",
                          '*/\n',
                          'vector spd_gp_exp_quad(data array[] vector x, real sdgp, vector lscale) {\n',
                          'int NB = dims(x)[1];\n',
                          'int D = dims(x)[2];\n',
                          'int Dls = rows(lscale);\n',
                          'real constant = square(sdgp) * sqrt(2 * pi())^D;\n',
                          'vector[NB] out;\n',
                          'if (Dls == 1) {\n',
                          '// one dimensional or isotropic GP\n',
                          'real neg_half_lscale2 = -0.5 * square(lscale[1]);\n',
                          'constant = constant * lscale[1]^D;\n',
                          'for (m in 1:NB) {\n',
                          'out[m] = constant * exp(neg_half_lscale2 * dot_self(x[m]));\n',
                          '}\n',
                          '} else {\n',
                          '// multi-dimensional non-isotropic GP\n',
                          'vector[Dls] neg_half_lscale2 = -0.5 * square(lscale);\n',
                          'constant = constant * prod(lscale);\n',
                          'for (m in 1:NB) {\n',
                          'out[m] = constant * exp(dot_product(neg_half_lscale2, square(x[m])));\n',
                          '}\n',
                          '}\n',
                          'return out;\n',
                          '}')
    } else {
      fun_lines <- NULL
    }
  }

  if(kernel %in% c('exponential', 'matern12')){
    if(!any(grepl('/* Spectral density of an exponential Gaussian process',
                  model_file, fixed = TRUE))){
      fun_lines <- paste0('/* Spectral density of an exponential Gaussian process\n',
                          '* also known as the Matern 1/2 kernel\n',
                          '* Args:\n',
                          '*   x: array of numeric values of dimension NB x D\n',
                          '*   sdgp: marginal SD parameter\n',
                          '*   lscale: vector of length-scale parameters\n',
                          '* Returns:\n',
                          "*   numeric vector of length NB of the SPD evaluated at 'x'\n",
                          '*/\n',
                          'vector spd_gp_exponential(data array[] vector x, real sdgp, vector lscale) {\n',
                          'int NB = dims(x)[1];\n',
                          'int D = dims(x)[2];\n',
                          'int Dls = rows(lscale);\n',
                          'real constant = square(sdgp) *\n',
                          '(2^D * pi()^(D / 2.0) * tgamma((D + 1.0) / 2)) / sqrt(pi());\n',
                          'real expo = -(D + 1.0) / 2;\n',
                          'vector[NB] out;\n',
                          'if (Dls == 1) {\n',
                          '// one dimensional or isotropic GP\n',
                          'real lscale2 = square(lscale[1]);\n',
                          'constant = constant * lscale[1]^D;\n',
                          'for (m in 1:NB) {\n',
                          'out[m] = constant * (1 + lscale2 * dot_self(x[m]))^expo;\n',
                          '}\n',
                          '} else {\n',
                          '// multi-dimensional non-isotropic GP\n',
                          'vector[Dls] lscale2 = square(lscale);\n',
                          'constant = constant * prod(lscale);\n',
                          'for (m in 1:NB) {\n',
                          'out[m] = constant * (1 + dot_product(lscale2, square(x[m])))^expo;\n',
                          '}\n',
                          '}\n',
                          'return out;\n',
                          '}')
    } else {
      fun_lines <- NULL
    }
  }

  if(kernel == 'matern32'){
    if(!any(grepl('/* Spectral density of a Matern 3/2 Gaussian process',
                  model_file, fixed = TRUE))){
      fun_lines <- paste0('/* Spectral density of a Matern 3/2 Gaussian process\n',
                          '* Args:\n',
                          '*   x: array of numeric values of dimension NB x D\n',
                          '*   sdgp: marginal SD parameter\n',
                          '*   lscale: vector of length-scale parameters\n',
                          '* Returns:\n',
                          "*   numeric vector of length NB of the SPD evaluated at 'x'\n",
                          '*/\n',
                          'vector spd_gp_matern32(data array[] vector x, real sdgp, vector lscale) {\n',
                          'int NB = dims(x)[1];\n',
                          'int D = dims(x)[2];\n',
                          'int Dls = rows(lscale);\n',
                          'real constant = square(sdgp) *\n',
                          '(2^D * pi()^(D / 2.0) * tgamma((D + 3.0) / 2) * 3^(3.0 / 2)) /\n',
                          '(0.5 * sqrt(pi()));\n',
                          'real expo = -(D + 3.0) / 2;\n',
                          'vector[NB] out;\n',
                          'if (Dls == 1) {\n',
                          '// one dimensional or isotropic GP\n',
                          'real lscale2 = square(lscale[1]);\n',
                          'constant = constant * lscale[1]^D;\n',
                          'for (m in 1:NB) {\n',
                          'out[m] = constant * (3 + lscale2 * dot_self(x[m]))^expo;\n',
                          '}\n',
                          '} else {\n',
                          '// multi-dimensional non-isotropic GP\n',
                          'vector[Dls] lscale2 = square(lscale);\n',
                          'constant = constant * prod(lscale);\n',
                          'for (m in 1:NB) {\n',
                          'out[m] = constant * (3 + dot_product(lscale2, square(x[m])))^expo;\n',
                          '}\n',
                          '}\n',
                          'return out;\n',
                          '}')
    } else {
      fun_lines <- NULL
    }
  }

  if(kernel == 'matern52'){
    if(!any(grepl('/* Spectral density of a Matern 5/2 Gaussian process',
                  model_file, fixed = TRUE))){
      fun_lines <- paste0('/* Spectral density of a Matern 5/2 Gaussian process\n',
                          '* Args:\n',
                          '*   x: array of numeric values of dimension NB x D\n',
                          '*   sdgp: marginal SD parameter\n',
                          '*   lscale: vector of length-scale parameters\n',
                          '* Returns:\n',
                          "*   numeric vector of length NB of the SPD evaluated at 'x'\n",
                          '*/\n',
                          'vector spd_gp_matern52(data array[] vector x, real sdgp, vector lscale) {\n',
                          'int NB = dims(x)[1];\n',
                          'int D = dims(x)[2];\n',
                          'int Dls = rows(lscale);\n',
                          'real constant = square(sdgp) *\n',
                          '(2^D * pi()^(D / 2.0) * tgamma((D + 5.0) / 2) * 5^(5.0 / 2)) /\n',
                          '(0.75 * sqrt(pi()));\n',
                          'real expo = -(D + 5.0) / 2;\n',
                          'vector[NB] out;\n',
                          'if (Dls == 1) {\n',
                          '// one dimensional or isotropic GP\n',
                          'real lscale2 = square(lscale[1]);\n',
                          'constant = constant * lscale[1]^D;\n',
                          'for (m in 1:NB) {\n',
                          'out[m] = constant * (5 + lscale2 * dot_self(x[m]))^expo;\n',
                          '}\n',
                          '} else {\n',
                          '// multi-dimensional non-isotropic GP\n',
                          'vector[Dls] lscale2 = square(lscale);\n',
                          'constant = constant * prod(lscale);\n',
                          'for (m in 1:NB) {\n',
                          'out[m] = constant * (5 + dot_product(lscale2, square(x[m])))^expo;\n',
                          '}\n',
                          '}\n',
                          'return out;\n',
                          '}')
    } else {
      fun_lines <- NULL
    }
  }

  if(any(grepl('functions {', model_file, fixed = TRUE))){
    model_file[grep('functions {', model_file, fixed = TRUE)] <-
      paste0('functions {\n',
             fun_lines)
  } else {
    model_file[grep('Stan model code', model_file)] <-
      paste0('// Stan model code generated by package mvgam\n',
             'functions {\n',
             fun_lines,
             '\n}\n')
  }

  model_file <- readLines(textConnection(model_file), n = -1)
}

#### Old gp() prepping functions; these are now redundant because
# brms is used to evaluate gp() effects and produce the relevant
# eigenfunctions / eigenvalues, but keeping the functions here for
# now in case they are needed for later work ####

#' #' Evaluate Laplacian eigenfunction for a given GP basis function
#' #' @noRd
#' phi = function(boundary, m, centred_covariate) {
#'   1 / sqrt(boundary) * sin((m * pi)/(2 * boundary) *
#'                              (centred_covariate + boundary))
#' }
#'
#' #' Evaluate eigenvalues for a given GP basis function
#' #' @noRd
#' lambda = function(boundary, m) {
#'   ((m * pi)/(2 * boundary))^2
#' }
#'
#' #' Spectral density squared exponential Gaussian Process kernel
#' #' @noRd
#' spd = function(alpha_gp, rho_gp, eigenvalues) {
#'   (alpha_gp^2) * sqrt(2 * pi) * rho_gp *
#'     exp(-0.5 * (rho_gp^2) * (eigenvalues^2))
#' }
#'
#' #' @noRd
#' sim_hilbert_gp = function(alpha_gp,
#'                           rho_gp,
#'                           b_gp,
#'                           last_trends,
#'                           fc_times,
#'                           train_times,
#'                           mean_train_times){
#'
#'   num_gp_basis <- length(b_gp)
#'
#'   # Get vector of eigenvalues of covariance matrix
#'   eigenvalues <- vector()
#'   for(m in 1:num_gp_basis){
#'     eigenvalues[m] <- lambda(boundary = (5.0/4) *
#'                                (max(train_times) - min(train_times)),
#'                              m = m)
#'   }
#'
#'   # Get vector of eigenfunctions
#'   eigenfunctions <- matrix(NA, nrow = length(fc_times),
#'                            ncol = num_gp_basis)
#'   for(m in 1:num_gp_basis){
#'     eigenfunctions[, m] <- phi(boundary = (5.0/4) *
#'                                  (max(train_times) - min(train_times)),
#'                                m = m,
#'                                centred_covariate = fc_times - mean_train_times)
#'   }
#'
#'   # Compute diagonal of covariance matrix
#'   diag_SPD <- sqrt(spd(alpha_gp = alpha_gp,
#'                        rho_gp = rho_gp,
#'                        sqrt(eigenvalues)))
#'
#'   # Compute GP trend forecast
#'   as.vector((diag_SPD * b_gp) %*% t(eigenfunctions))
#' }

#' #' Compute the mth eigen function of an approximate GP
#' #' Credit to Paul Burkner from brms: https://github.com/paul-buerkner/brms/R/formula-gp.R#L289
#' #' @noRd
#' eigen_fun_cov_exp_quad <- function(x, m, L) {
#'   x <- as.matrix(x)
#'   D <- ncol(x)
#'   stopifnot(length(m) == D, length(L) == D)
#'   out <- vector("list", D)
#'   for (i in seq_cols(x)) {
#'     out[[i]] <- 1 / sqrt(L[i]) *
#'       sin((m[i] * pi) / (2 * L[i]) * (x[, i] + L[i]))
#'   }
#'   Reduce("*", out)
#' }

#' #' Compute squared differences
#' #' Credit to Paul Burkner from brms: https://github.com/paul-buerkner/brms/R/formula-gp.R#L241
#' #' @param x vector or matrix
#' #' @param x_new optional vector of matrix with the same ncol as x
#' #' @return an nrow(x) times nrow(x_new) matrix
#' #' @details if matrices are passed results are summed over the columns
#' #' @noRd
#' diff_quad <- function(x, x_new = NULL) {
#'   x <- as.matrix(x)
#'   if (is.null(x_new)) {
#'     x_new <- x
#'   } else {
#'     x_new <- as.matrix(x_new)
#'   }
#'   .diff_quad <- function(x1, x2) (x1 - x2)^2
#'   out <- 0
#'   for (i in seq_cols(x)) {
#'     out <- out + outer(x[, i], x_new[, i], .diff_quad)
#'   }
#'   out
#' }

#' #' Extended range of input data for which predictions should be made
#' #' Credit to Paul Burkner from brms: https://github.com/paul-buerkner/brms/R/formula-gp.R#L301
#' #' @noRd
#' choose_L <- function(x, c) {
#'   if (!length(x)) {
#'     range <- 1
#'   } else {
#'     range <- max(1, max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
#'   }
#'   c * range
#' }

#' #' Mean-center and scale the particular covariate of interest
#' #' so that the maximum Euclidean distance between any two points is 1
#' #' @noRd
#' scale_cov <- function(data, covariate, by, level,
#'                       mean, max_dist){
#'   Xgp <- data[[covariate]]
#'   if(!is.na(by) &
#'      !is.na(level)){
#'       Xgp <- data[[covariate]][data[[by]] == level]
#'    }
#'
#'   # Compute max Euclidean distance if not supplied
#'   if(is.na(max_dist)){
#'     Xgp_max_dist <- sqrt(max(diff_quad(Xgp)))
#'   } else {
#'     Xgp_max_dist <- max_dist
#'   }
#'
#'   # Scale
#'   Xgp <- Xgp / Xgp_max_dist
#'
#'   # Compute mean if not supplied (after scaling)
#'   if(is.na(mean)){
#'     Xgp_mean <- mean(Xgp, na.rm = TRUE)
#'   } else {
#'     Xgp_mean <- mean
#'   }
#'
#'   # Center
#'   Xgp <- Xgp - Xgp_mean
#'
#'   return(list(Xgp = Xgp,
#'               Xgp_mean = Xgp_mean,
#'               Xgp_max_dist = Xgp_max_dist))
#' }
#'
#' #' Prep GP eigenfunctions
#' #' @noRd
#' prep_eigenfunctions = function(data,
#'                                covariate,
#'                                by = NA,
#'                                level = NA,
#'                                k,
#'                                boundary,
#'                                mean = NA,
#'                                max_dist = NA,
#'                                scale = TRUE,
#'                                L,
#'                                initial_setup = FALSE){
#'
#'   # Extract and scale covariate (scale set to FALSE if this is a prediction
#'   # step so that we can scale by the original training covariate values supplied
#'   # in mean and max_dist)
#'   covariate_cent <- scale_cov(data = data,
#'                               covariate = covariate,
#'                               by = by,
#'                               level = level,
#'                               mean = mean,
#'                               max_dist = max_dist)$Xgp
#'
#'   # Construct matrix of eigenfunctions
#'   eigenfunctions <- matrix(NA, nrow = length(covariate_cent),
#'                            ncol = k)
#'   if(missing(L)){
#'     L <- choose_L(covariate_cent, boundary)
#'   }
#'
#'   for(m in 1:k){
#'     eigenfunctions[, m] <- eigen_fun_cov_exp_quad(x = matrix(covariate_cent),
#'                                                   m = m,
#'                                                   L = L)
#'   }
#'
#'   # Multiply eigenfunctions by the 'by' variable if one is supplied
#'   if(!is.na(by)){
#'     if(!is.na(level)){
#'       # no multiplying needed as this is a factor by variable,
#'       # but we need to pad the eigenfunctions with zeros
#'       # for the observations where the by is a different level;
#'       # the design matrix is always sorted by time and then by series
#'       # in mvgam
#'       if(initial_setup){
#'         sorted_by <- data.frame(time = data$time,
#'                                 series = data$series,
#'                                 byvar = data[[by]]) %>%
#'           dplyr::arrange(time, series) %>%
#'           dplyr::pull(byvar)
#'       } else {
#'         sorted_by <- data[[by]]
#'       }
#'
#'       full_eigens <- matrix(0, nrow = length(data[[by]]),
#'                             ncol = NCOL(eigenfunctions))
#'       full_eigens[(1:length(data[[by]]))[
#'         sorted_by == level],] <- eigenfunctions
#'       eigenfunctions <- full_eigens
#'     } else {
#'       eigenfunctions <- eigenfunctions * data[[by]]
#'     }
#'   }
#'   eigenfunctions
#' }
#'
#' #' Prep Hilbert Basis GP covariates
#' #' @noRd
#' prep_gp_covariate = function(data,
#'                              response,
#'                              covariate,
#'                              by = NA,
#'                              level = NA,
#'                              scale = TRUE,
#'                              boundary = 5.0/4,
#'                              k = 20,
#'                              family = gaussian()){
#'
#'   # Get default gp param priors from a call to brms::get_prior()
#'   def_gp_prior <- suppressWarnings(brms::get_prior(formula(paste0(response,
#'                                                                   ' ~ gp(', covariate,
#'                                                  ifelse(is.na(by), ', ',
#'                                                         paste0(', by = ', by, ', ')),
#'                                                  'k = ', k,
#'                                                  ', scale = ',
#'                                                  scale,
#'                                                  ', c = ',
#'                                                  boundary,
#'                                                  ')')), data = data,
#'                                                  family = family))
#'   def_gp_prior <- def_gp_prior[def_gp_prior$prior != '',]
#'   def_rho <- def_gp_prior$prior[min(which(def_gp_prior$class == 'lscale'))]
#'   if(def_rho == ''){
#'     def_rho <- 'inv_gamma(1.5, 5);'
#'   }
#'   def_alpha <- def_gp_prior$prior[min(which(def_gp_prior$class == 'sdgp'))]
#'   if(def_alpha == ''){
#'     def_alpha<- 'student_t(3, 0, 2.5);'
#'   }
#'
#'   # Prepare the covariate
#'   if(scale){
#'     max_dist <- NA
#'   } else {
#'     max_dist <- 1
#'   }
#'
#'   covariate_cent <- scale_cov(data = data,
#'                               covariate = covariate,
#'                               by = by,
#'                               mean = NA,
#'                               max_dist = max_dist,
#'                               level = level)
#'
#'   covariate_mean <- covariate_cent$Xgp_mean
#'   covariate_max_dist <- covariate_cent$Xgp_max_dist
#'   covariate_cent <- covariate_cent$Xgp
#'
#'   # Construct vector of eigenvalues for GP covariance matrix; the
#'   # same eigenvalues are always used in prediction, so we only need to
#'   # create them when prepping the data. They will need to be included in
#'   # the Stan data list
#'   L <- choose_L(covariate_cent, boundary)
#'   eigenvalues <- vector()
#'   for(m in 1:k){
#'     eigenvalues[m] <- sqrt(lambda(boundary = L,
#'                                      m = m))
#'   }
#'
#'   # Construct matrix of eigenfunctions; this will change depending on the values
#'   # of the covariate, so it needs to be computed and included as data but also needs
#'   # to be computed to make predictions
#'   eigenfunctions <- prep_eigenfunctions(data = data,
#'                                         covariate = covariate,
#'                                         by = by,
#'                                         level = level,
#'                                         L = L,
#'                                         k = k,
#'                                         boundary = boundary,
#'                                         mean = covariate_mean,
#'                                         max_dist = covariate_max_dist,
#'                                         scale = scale,
#'                                         initial_setup = TRUE)
#'
#'   # Make attributes table using a cleaned version of the covariate
#'   # name to ensure there are no illegal characters in the Stan code
#'   byname <- ifelse(is.na(by), '', paste0(':', by))
#'   covariate_name <- paste0('gp(', covariate, ')', byname)
#'   if(!is.na(level)){
#'     covariate_name <- paste0(covariate_name, level)
#'   }
#'   att_table <- list(effect = 'gp',
#'                     name = covariate_name,
#'                     covariate = covariate,
#'                     by = by,
#'                     level = level,
#'                     k = k,
#'                     boundary = boundary,
#'                     L = L,
#'                     scale = scale,
#'                     def_rho = def_rho,
#'                     def_alpha = def_alpha,
#'                     mean = covariate_mean,
#'                     max_dist = covariate_max_dist,
#'                     eigenvalues = eigenvalues)
#'
#'   # Items to add to Stan data
#'   # Number of basis functions
#'   covariate_name <- clean_gpnames(covariate_name)
#'   data_lines <- paste0('int<lower=1> k_', covariate_name,
#'                        '; // basis functions for approximate gp\n')
#'   append_dat <- list(k = k)
#'   names(append_dat) <- paste0('k_', covariate_name, '')
#'
#'   # Approximate GP eigenvalues
#'   data_lines <- paste0(data_lines, paste0(
#'     'vector[',
#'     'k_', covariate_name,
#'     '] l_', covariate_name, '; // approximate gp eigenvalues\n'),
#'     collapse = '\n')
#'   append_dat2 <- list(slambda = eigenvalues)
#'   names(append_dat2) <- paste0('l_', covariate_name, '')
#'   append_dat <- append(append_dat, append_dat2)
#'
#'   # Return necessary objects in a list
#'   list(att_table = att_table,
#'        data_lines = data_lines,
#'        data_append = append_dat,
#'        eigenfunctions = eigenfunctions)
#' }
