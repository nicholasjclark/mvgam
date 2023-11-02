#' Add remaining data, model and parameter blocks to a Stan model
#'
#'
#' @noRd
#' @param jags_file Prepared JAGS mvgam model file
#' @param stan_file Incomplete Stan model file to be edited
#' @param ss_gam The GAM setup object
#' @param use_lv logical
#' @param n_lv \code{integer} number of latent dynamic factors (if \code{use_lv = TRUE})
#' @param jags_data Prepared mvgam data for JAGS modelling
#' @param family \code{character}.
#' @param upper_bounds Optional \code{vector} of \code{integer} values specifying upper limits for each series. If supplied,
#' this generates a modified likelihood where values above the bound are given a likelihood of zero. Note this modification
#' is computationally expensive in \code{JAGS} but can lead to better estimates when true bounds exist. Default is to remove
#' truncation entirely (i.e. there is no upper bound for each series)
#' @return A `list` containing the updated Stan model and model data
add_stan_data = function(jags_file, stan_file,
                         ss_gam,
                         use_lv = FALSE,
                         n_lv,
                         jags_data, family = 'poisson',
                         upper_bounds){

  #### Modify the Stan file ####
  # Update lines associated with particular family
  if(family == 'poisson'){

    if(!is.null(upper_bounds)){
      stan_file[grep('~ poisson_log', stan_file)] <-
        gsub(';', 'T[,U[s]];', stan_file[grep('~ poisson_log', stan_file)])
      stan_file[grep('~ poisson_log', stan_file)] <-
        gsub('poisson_log(', 'poisson(exp(', stan_file[grep('~ poisson_log', stan_file)],
             fixed = TRUE)
      stan_file[grep('~ poisson', stan_file)] <-
        gsub(')', '))', stan_file[grep('~ poisson', stan_file)], fixed = TRUE)
    }
  }

  if(family == 'negative binomial'){
    stan_file[grep('// raw basis', stan_file) + 2] <-
  '\n// negative binomial overdispersion\nvector<lower=0>[n_series] phi_inv;\n'

      stan_file[grep('// priors for smoothing', stan_file) + 2] <-
        paste0('\n// priors for overdispersion parameters\n',
        'phi_inv ~ student_t(3, 0, 0.1);\n')

      to_negbin <- gsub('poisson_log', 'neg_binomial_2',
           stan_file[grep('y[i, s] ~ poisson', stan_file, fixed = T)])
      stan_file[grep('y[i, s] ~ poisson', stan_file, fixed = T)] <-
      gsub(');', ', inv(phi_inv[s]));', to_negbin)

      add_exp_open <- gsub('\\(eta', '(exp(eta',
                      stan_file[grep('y[i, s] ~ neg_binomial', stan_file, fixed = T)])
      add_exp_cl <- gsub('],', ']),',
                         add_exp_open)
      stan_file[grep('y[i, s] ~ neg_binomial', stan_file, fixed = T)] <-
        add_exp_cl

      stan_file[grep('matrix[n, n_series] ypred;', stan_file, fixed = T)] <-
        paste0('matrix[n, n_series] ypred;\n',
               'matrix[n, n_series] phi_vec;\n',
               'vector[n_series] phi;\n',
               'phi = inv(phi_inv);\n',
               'for (s in 1:n_series) {\n',
               'phi_vec[1:n,s] = rep_vector(phi[s], n);\n}\n')

      to_negbin <- gsub('poisson_log_rng', 'neg_binomial_2_rng',
                        stan_file[grep('ypred[i, s] = poisson_log_rng', stan_file, fixed = T)])
      stan_file[grep('ypred[i, s] = poisson_log_rng', stan_file, fixed = T)] <-
        gsub(');', ', phi_vec[i, s]);', to_negbin)

      add_exp_open <- gsub('\\(eta', '(exp(eta',
                           stan_file[grep('ypred[i, s] = neg_binomial', stan_file, fixed = T)])

      if(any(grepl('trend[i, s]', stan_file, fixed = T))){
        add_exp_cl <- gsub('trend[i, s]', 'trend[i, s])',
                           add_exp_open, fixed = T)
      } else {
        add_exp_cl <- gsub('eta[ytimes[i, s]]', 'eta[ytimes[i, s]])',
                           add_exp_open, fixed = T)
      }

      stan_file[grep('ypred[i, s] = neg_binomial', stan_file, fixed = T)] <-
        add_exp_cl

      if(!is.null(upper_bounds)){
        stan_file[grep('~ neg_binomial_2', stan_file)] <-
          gsub(';', 'T[,U[s]];', stan_file[grep('~ neg_binomial_2', stan_file)])
      }

    stan_file <- readLines(textConnection(stan_file), n = -1)
  }

  # Get dimensions and numbers of smooth terms
  snames <- names(jags_data)[grep('S.*', names(jags_data))]
  if(length(snames) == 0){
    smooth_penalty_data <- NULL
  } else {
    smooth_dims <- matrix(NA, ncol = 2, nrow = length(snames))
    for(i in 1:length(snames)){
      smooth_dims[i,] <- dim(jags_data[[snames[i]]])
    }

    # Insert the data block for the model
    smooth_penalty_data <- vector()
    for(i in 1:length(snames)){
      smooth_penalty_data[i] <- paste0('matrix[', smooth_dims[i, 1],
                                       ',',
                                       smooth_dims[i, 2], '] ',
                                       snames[i],
                                       '; // mgcv smooth penalty matrix ', snames[i])
    }

  }

  # Get parametric prior locations and precisions if necessary
  if('p_taus' %in% names(jags_data)){
    p_terms <- paste0('real p_taus[', length(jags_data$p_taus),']; // prior precisions for parametric coefficients\n',
                      'real p_coefs[', length(jags_data$p_taus), ']; // prior locations for parametric coefficients\n')
  } else {
    p_terms <- NULL
  }

  # Add lines for upper bounds if supplied
  if(!is.null(upper_bounds)){
    bounds <- paste0('int U[', length(upper_bounds), ']; // upper bounds\n')
  } else {
    bounds <- NULL
  }

  # Remove smooth parameter info if no smooth terms are included
  if(any(grepl('## smoothing parameter priors...', jags_file))){
    zero_data <- paste0('vector[num_basis] zero; // prior locations for basis coefficients\n')
    n_sp_data <- paste0('int<lower=0> n_sp; // number of smoothing parameters\n')
  } else {
    zero_data <- NULL
    n_sp_data <- NULL
  }

  # Occasionally there are smooths with no zero vector
  # (i.e. for bs = 'fs', they are often just normal(0, lambda))
  if(is.null(jags_data$zero)){
    zero_data <- NULL
  }

  # latent variable lines
  if(use_lv){
    lv_data <- paste0('int<lower=0> n_lv; // number of dynamic factors\n')
  } else {
    lv_data <- NULL
  }

  # shared smoothing parameter lines
  if('L' %in% names(jags_data)){
    lambda_links <- paste0('matrix[',
                           NROW(jags_data$L),',',
                           NCOL(jags_data$L),'] lambda_links; // smooth parameter linking matrix\n',
                           'int<lower=0> n_raw_sp; // number of raw smoothing parameters to estimate\n')
  } else {
    lambda_links <- NULL
  }

  # Offset information
  if(any(grepl('eta <- X %*% b + offset', jags_file, fixed = TRUE))){
    offset_line <- paste0('vector[total_obs] offset; // offset vector\n')
  } else {
    offset_line <- NULL
  }

  if(any(grepl('eta <- X * b + offset', jags_file, fixed = TRUE))){
    offset_line <- paste0('vector[total_obs] offset; // offset vector\n')
  } else {
    offset_line <- NULL
  }

  if(any(grepl('offset; offset vector of length (n x n_series)',
               jags_file,
               fixed = TRUE))){
    offset_line <- paste0('vector[total_obs] offset; // offset vector\n')
  } else {
    offset_line <- NULL
  }

  # Search for any non-contiguous indices that sometimes are used by mgcv
  if(any(grep('in c\\(', jags_file))){
    add_idxs <- TRUE
    seq_character = function(x){
      all_nums <- as.numeric(unlist(strsplit(x, ':')))
      if(length(all_nums) > 1){
        out <- seq(all_nums[1], all_nums[2])
      } else {
        out <- all_nums
      }
      out
    }

    idx_locations <- grep('in c\\(', jags_file)
    idx_vals <- list()
    idx_data <- vector()
    for(i in 1:length(idx_locations)){
      list_vals <- unlist(strsplit(gsub('^.*c\\(*|\\s*).*$', '', jags_file[idx_locations[i]]), ','))
      idx_vals[[i]] <- array(unlist(lapply(list_vals, seq_character)),
                             dim = length(unlist(lapply(list_vals, seq_character))))
      idx_data[i] <- paste0('int idx', i, '[', length(idx_vals[[i]]), ']; // discontiguous index values')
      jags_file[idx_locations][i] <- sub("in.*\\)\\)", paste0("in idx", i, ')'), jags_file[idx_locations][i])
    }

    # Update the Stan data block
    stan_file[grep('##insert data',
                         stan_file)] <- paste0('// Stan model code generated by package mvgam\n',
                                               'data {',
                                               '\n',
                                               bounds,
                                               paste0(idx_data, collapse = '\n'), '\n',
                                               'int<lower=0> total_obs; // total number of observations\n',
                                               'int<lower=0> n; // number of timepoints per series\n',
                                               lv_data,
                                               n_sp_data,
                                               lambda_links,
                                               'int<lower=0> n_series; // number of series\n',
                                               'int<lower=0> num_basis; // total number of basis coefficients\n',
                                               #p_terms,
                                               zero_data,
                                               offset_line,
                                               'matrix[num_basis, total_obs] X; // transposed mgcv GAM design matrix\n',
                                               'int<lower=0> ytimes[n, n_series]; // time-ordered matrix (which col in X belongs to each [time, series] observation?)\n',
                                               paste0(smooth_penalty_data, collapse = '\n'), '\n',
                                               'int<lower=0, upper=1> y_observed[n, n_series]; // indices of missing vs observed\n',
                                               'int<lower=-1> y[n, n_series]; // time-ordered observations, with -1 indicating missing\n',
                                               '}\n')
  } else {
    add_idxs <- FALSE
    stan_file[grep('##insert data',
                         stan_file)] <- paste0('// Stan model code generated by package mvgam\n',
                                               'data {',
                                               '\n',
                                               bounds,
                                               'int<lower=0> total_obs; // total number of observations\n',
                                               'int<lower=0> n; // number of timepoints per series\n',
                                               lv_data,
                                               n_sp_data,
                                               lambda_links,
                                               'int<lower=0> n_series; // number of series\n',
                                               'int<lower=0> num_basis; // total number of basis coefficients\n',
                                               zero_data,
                                               offset_line,
                                               #p_terms,
                                               'matrix[num_basis, total_obs] X; // transposed mgcv GAM design matrix\n',
                                               'int<lower=0> ytimes[n, n_series]; // time-ordered matrix (which col in X belongs to each [time, series] observation?)\n',
                                               paste0(smooth_penalty_data, collapse = '\n'), '\n',
                                               'int<lower=0, upper=1> y_observed[n, n_series]; // indices of missing vs observed\n',
                                               'int<lower=-1> y[n, n_series]; // time-ordered observations, with -1 indicating missing\n',
                                               '}\n')
  }
  stan_file <- readLines(textConnection(stan_file), n = -1)

  # Modify the model block to include each smooth term
  if(any(grepl('## smoothing parameter priors...', jags_file))){
    smooths_start <- grep('## GAM-specific priors', jags_file) + 1
    smooths_end <- grep('## smoothing parameter priors...', jags_file) - 1
    jags_smooth_text <- jags_file[smooths_start:smooths_end]
    jags_smooth_text <- gsub('##', '//', jags_smooth_text)
    jags_smooth_text <- gsub('dexp', 'exponential', jags_smooth_text)

    smooth_labs <- do.call(rbind, lapply(seq_along(ss_gam$smooth), function(x){
      data.frame(label = ss_gam$smooth[[x]]$label,
                 term = paste(ss_gam$smooth[[x]]$term, collapse = ','),
                 class = class(ss_gam$smooth[[x]])[1])
    }))

    if(length(ss_gam$sp) > 0 & !all(smooth_labs$class == 'random.effect')){
      any_ks <- TRUE
    } else {
      any_ks <- FALSE
    }
    # any_ks <- any(grep('K.* <- ', jags_smooth_text))
    any_timevarying <- any(grep('// prior for s(time):', jags_smooth_text, fixed = TRUE))
    if(any_ks ||
       any_timevarying){

      if(any(grep('K.* <- ', jags_smooth_text))){
        K_starts <- grep('K.* <- ', jags_smooth_text)
        for(i in 1:length(K_starts)){
          jags_smooth_text[K_starts[i]+1] <- gsub('\\bb\\b', 'b_raw',
                                                  gsub('dmnorm', 'multi_normal_prec',
                                                       paste0(gsub('K.*',
                                                                   trimws(gsub('K.* <- ', '',
                                                                               jags_smooth_text[K_starts[i]])),
                                                                   jags_smooth_text[K_starts[i]+1]), ')')))
        }
        jags_smooth_text <- jags_smooth_text[-K_starts]
      }

    } else {
      # If no K terms or time-varying terms, then there are no smoothing parameters in the model
      # (probably the only smooth terms included are random effect bases, which don't need
      # smoothing parameters when we use the non-centred parameterisation)
      stan_file <- stan_file[-grep('// priors for smoothing parameters', stan_file,
                                   fixed = TRUE)]
      stan_file <- stan_file[-grep('lambda ~ ', stan_file,
                                   fixed = TRUE)]
      stan_file <- stan_file[-grep('vector[n_sp] rho', stan_file,
                                   fixed = TRUE)]
      stan_file <- stan_file[-grep('rho = log', stan_file,
                                   fixed = TRUE)]
      stan_file <- stan_file[-grep('// smoothing parameters', stan_file,
                                   fixed = TRUE)]
      stan_file <- stan_file[-grep('[n_sp] lambda', stan_file,
                                   fixed = TRUE)]
      # stan_file <- stan_file[-grep('vector[num_basis] zero; //', stan_file,
      #                              fixed = TRUE)]
      stan_file <- stan_file[-grep('int<lower=0> n_sp; //', stan_file,
                                   fixed = TRUE)]
    }

    # If there are no K terms but there are time-varying, we don't need
    # the zero vector
    if(any_timevarying & !any_ks){
      stan_file <- stan_file[-grep('vector[num_basis] zero; //', stan_file,
                                   fixed = TRUE)]
    }

    # Create a new smooths_included check after working through the
    # random effects
    if(!any_timevarying & !any_ks){
      smooths_included <- FALSE
    } else {
      smooths_included <- TRUE
    }

    if(any(grep('b\\[i\\] = b_raw', jags_smooth_text))){
      jags_smooth_text <- jags_smooth_text[-grep('b\\[i\\] = b_raw', jags_smooth_text)]
    }
    jags_smooth_text <- gsub('dnorm', 'normal', jags_smooth_text)
    jags_smooth_text <- gsub('  ', ' ', jags_smooth_text)
    jags_smooth_text[-grep('//|\\}|\\{', jags_smooth_text)] <-
      paste0(jags_smooth_text[-grep('//|\\}|\\{', jags_smooth_text)], ';')
    jags_smooth_text <- gsub(') }', '); }', jags_smooth_text)
    jags_smooth_text <- gsub('}', '}\n', jags_smooth_text)
    jags_smooth_text[(grep('//',
                           jags_smooth_text) - 1)[-1]] <-
      paste0(jags_smooth_text[(grep('//',
                                    jags_smooth_text) - 1)[-1]], '\n')
    stan_file[grep('##insert smooths',
                   stan_file)] <- paste0(jags_smooth_text, collapse = '\n')
    stan_file <- readLines(textConnection(stan_file), n = -1)

    # Deal with any random effect priors
    if(any(grep('b_raw\\[i\\] ~', stan_file))){
      b_raw_string <- paste0(stan_file[grep('b_raw\\[i\\] ~', stan_file)-1], collapse = ',')
      n_b_raw <- max(as.numeric(unlist(regmatches(b_raw_string,
                                                  gregexpr("[[:digit:]]+",
                                                           b_raw_string)))))
      min_b_raw <- min(as.numeric(unlist(regmatches(b_raw_string,
                                                    gregexpr("[[:digit:]]+",
                                                             b_raw_string)))))

      n_sigma_raw <- max(as.numeric(unlist(regmatches(unique(sub(".*(sigma_raw?\\d+).*", "\\1",
                                                                 grep('sigma_raw', stan_file,
                                                                      value = T))),
                                                      gregexpr("[[:digit:]]+",
                                                               unique(sub(".*(sigma_raw?\\d+).*", "\\1",
                                                                          grep('sigma_raw', stan_file,
                                                                               value = T))))))))

      stan_file <- stan_file[-grep('mu_raw.* ~ ', stan_file)]
      stan_file <- stan_file[-grep('<- mu_raw', stan_file)]
      stan_file <- stan_file[-grep('sigma_raw.* ~ ', stan_file)]
      stan_file[grep('model \\{', stan_file)] <-
        paste0('model {\n// prior for random effect population variances\nsigma_raw ~ exponential(0.5);\n\n',
               '// prior for random effect population means\nmu_raw ~ std_normal();\n')

      stan_file[grep('parameters \\{', stan_file)[1] + 2] <-
        paste0(stan_file[grep('parameters \\{', stan_file)[1] + 2],
               '\n',
               '\n// random effect variances\n',
               paste0('vector<lower=0>[',n_sigma_raw,'] sigma_raw', ';\n', collapse = ''),
               '\n',
               '\n// random effect means\n',
               paste0('vector[',n_sigma_raw,'] mu_raw', ';\n', collapse = ''))

      b_raw_text <- vector()
      min_beta <- vector()
      b_raw_indices <- grep('b_raw\\[i\\] ~', stan_file)
      for(i in 1:length(b_raw_indices)){

        b_raw_text[i] <- paste0('for (i in ', as.numeric(sub("for \\(i in ", "",
                                                             sub("\\:.*", "",
                                                                 stan_file[b_raw_indices[i] - 1]))),
                                ':', as.numeric(sub(" ", "",
                                                    sub("\\{", "",
                                                        sub("\\)", "",
                                                            sub(".*\\:", "",
                                                                stan_file[b_raw_indices[i]-1]))))),
                                ') {\nb[i] = mu_raw[', i, '] + b_raw[i] * sigma_raw[',i,
                                '];\n}\n')
        min_beta[i] <- as.numeric(sub("for \\(i in ", "",
                                      sub("\\:.*", "",
                                          stan_file[b_raw_indices[i] - 1])))
      }

      # If parametric coefficients are included, they'll come before random effects
      min_re_betas <- min(min_beta)
      if(min_re_betas > 1){
        b_raw_text <- c(paste0('\nfor (i in 1:',
                               min_re_betas - 1, ') {\nb[i] = b_raw[i];\n}'),
                        b_raw_text)
      } else {
        b_raw_text <- b_raw_text
      }

      if(n_b_raw < dim(jags_data$X)[2]){
        b_raw_text <- c(b_raw_text,
                        paste0('\nfor (i in ',  n_b_raw+1,':num_basis) {\nb[i] = b_raw[i];\n}\n'))
      }

      stan_file[grep('// basis coefficients', stan_file) + 2] <- paste0(b_raw_text,
                                                                        collapse = '\n')
      stan_file <- readLines(textConnection(stan_file), n = -1)

      # If no random effects, betas are equal to beta_raws
    } else {
      stan_file[grep('// basis coefficients', stan_file) + 2] <-
        paste0('\nfor (i in ','1:num_basis) {\nb[i] = b_raw[i];\n}')
      stan_file <- readLines(textConnection(stan_file), n = -1)
    }

    # Update parametric effect priors
    if(any(grep('// parametric effect', stan_file))){

      # Get indices of parametric effects
      smooth_labs <- do.call(rbind, lapply(seq_along(ss_gam$smooth), function(x){
        data.frame(label = ss_gam$smooth[[x]]$label,
                   term = paste(ss_gam$smooth[[x]]$term, collapse = ','),
                   class = class(ss_gam$smooth[[x]])[1])
      }))
      lpmat <- predict(ss_gam, type = 'lpmatrix', exclude = smooth_labs$label)
      para_indices <- which(apply(lpmat, 2, function(x) !all(x == 0)) == TRUE)

      # min_paras <- as.numeric(sub('.*(?=.$)', '',
      #                             sub("\\:.*", "",
      #                                 stan_file[grep('// parametric effect', stan_file) + 1]),
      #                             perl=T))
      # max_paras <- as.numeric(substr(sub(".*\\:", "",
      #                                    stan_file[grep('// parametric effect', stan_file) + 1]),
      #                                1, 1))
      # para_indices <- seq(min_paras, max_paras)

      # Get names of parametric terms
      # int_included <- attr(ss_gam$pterms, 'intercept') == 1L
      # other_pterms <- attr(ss_gam$pterms, 'term.labels')
      # all_paras <- other_pterms
      # if(int_included){
      #   all_paras <- c('(Intercept)', all_paras)
      # }
      all_paras <- names(para_indices)

      # Create prior lines for parametric terms
      para_lines <- vector()
      for(i in seq_along(all_paras)){
        para_lines[i] <- paste0('// prior for ', all_paras[i],
                                '...\n',
                                'b_raw[', para_indices[i], '] ~ student_t(3, 0, 2);\n')
      }

      stan_file <- stan_file[-(grep('// parametric effect', stan_file) + 1)]
      stan_file[grep('// parametric effect', stan_file)] <-
        paste0(paste(para_lines, collapse = '\n'))
      stan_file <- readLines(textConnection(stan_file), n = -1)
    }

    # Check for shared smoothing parameters and link them accordingly
    if('L' %in% names(jags_data)){
      stan_file[grep('lambda ~ normal', stan_file,
                       fixed = TRUE)] <- "lambda_raw ~ normal(30, 25);"

      stan_file[grep("vector<lower=0>[n_sp] lambda;", stan_file,
                     fixed = TRUE)] <- "vector<lower=0>[n_raw_sp] lambda_raw;"

      stan_file[grep('// GAM contribution to expectations',
                     stan_file, fixed = TRUE)] <-
        "// GAM contribution to expectations (log scale)\n// linked smoothing parameters\nvector[n_sp] lambda;\n"

      stan_file[grep('model {',
                     stan_file, fixed = TRUE) - 2] <-
        'lambda = to_vector(lambda_links * lambda_raw);\n}\n'

      stan_file <- readLines(textConnection(stan_file), n = -1)
    }

  } else {
    ## No smooths included
    smooths_included <- FALSE
    stan_file <- stan_file[-grep('// priors for smoothing parameters', stan_file,
                                 fixed = TRUE)]
    stan_file <- stan_file[-grep('lambda ~ normal', stan_file,
                                 fixed = TRUE)]
    stan_file <- stan_file[-grep('vector[n_sp] rho', stan_file,
                                 fixed = TRUE)]
    stan_file <- stan_file[-grep('rho = log', stan_file,
                                 fixed = TRUE)]
    stan_file <- stan_file[-grep('// smoothing parameters', stan_file,
                                 fixed = TRUE)]
    stan_file <- stan_file[-grep('[n_sp] lambda', stan_file,
                                 fixed = TRUE)]

    stan_file[grep('// basis coefficients', stan_file) + 2] <-
      paste0('\nfor (i in ','1:num_basis) {\nb[i] = b_raw[i];\n}\n')

    if(any(grep('## parametric effect priors', jags_file))){

      # Get indices of parametric effects
      smooth_labs <- do.call(rbind, lapply(seq_along(ss_gam$smooth), function(x){
        data.frame(label = ss_gam$smooth[[x]]$label,
                   term = paste(ss_gam$smooth[[x]]$term, collapse = ','),
                   class = class(ss_gam$smooth[[x]])[1])
      }))
      lpmat <- predict(ss_gam, type = 'lpmatrix',
                       exclude = smooth_labs$label)
      para_indices <- which(apply(lpmat, 2, function(x) !all(x == 0)) == TRUE)
      all_paras <- names(para_indices)
      # min_paras <- as.numeric(sub('.*(?=.$)', '',
      #                             sub("\\:.*", "",
      #                                 jags_file[grep('## parametric effect', jags_file) + 1]), perl=T))
      # max_paras <- as.numeric(substr(sub(".*\\:", "",
      #                                    jags_file[grep('## parametric effect', jags_file) + 1]),
      #                                1, 1))
      # para_indices <- seq(min_paras, max_paras)
      #
      # # Get names of parametric terms
      # int_included <- attr(ss_gam$pterms, 'intercept') == 1L
      # other_pterms <- attr(ss_gam$pterms, 'term.labels')
      # all_paras <- other_pterms
      # if(int_included){
      #   all_paras <- c('(Intercept)', all_paras)
      # }

      # Create prior lines for parametric terms
      para_lines <- vector()
      for(i in seq_along(all_paras)){
        para_lines[i] <- paste0('// prior for ', all_paras[i],
                                '...\n',
                                'b_raw[', para_indices[i], '] ~ student_t(3, 0, 2);\n')
      }

      stan_file[grep('##insert smooths', stan_file)] <-
        paste0(paste(para_lines, collapse = '\n'))
      stan_file <- readLines(textConnection(stan_file), n = -1)
    }

  }

  #### Minor text changes to improve efficiency of Stan code ####
  #stan_file <- gsub('...', '', stan_file)
  clean_up <- vector()
  for(x in 1:length(stan_file)){
    clean_up[x] <- stan_file[x-1] == "" & stan_file[x] == ""
  }
  clean_up[is.na(clean_up)] <- FALSE
  stan_file <- stan_file[!clean_up]

  # Use as much vectorization as possible for computing predictions
  stan_file[grep('vector[total_obs] eta;', stan_file,
                 fixed = TRUE)] <-
    paste0('vector[total_obs] eta;\n\n',
           '// expectations\n',
           'matrix[n, n_series] mus;')
  if(any(grepl('trend[i, s]', stan_file))){
    stan_file <- sub('eta[ytimes[i, s]] + trend[i, s]',
                     'mus[i, s]', stan_file, fixed = TRUE)

    stan_file[grep('model {', stan_file, fixed = TRUE) - 2] <-
      paste0('\nfor(s in 1:n_series){\n',
             'mus[1:n, s] = eta[ytimes[1:n, s]] + trend[1:n, s];\n',
             '}\n',
             '}')

  } else {
    stan_file <- sub('eta[ytimes[i, s]]',
                     'mus[i, s]', stan_file, fixed = TRUE)

    stan_file[grep('model {', stan_file, fixed = TRUE) - 2] <-
      paste0('\nfor(s in 1:n_series){\n',
             'mus[1:n, s] = eta[ytimes[1:n, s]];\n',
             '}\n',
             '}')
  }

  stan_file <-
    stan_file[-(grep('// posterior predictions', stan_file, fixed = TRUE) + 1)]
  stan_file <-
    stan_file[-(grep('// posterior predictions', stan_file, fixed = TRUE) + 4)]
  stan_file[grep('ypred[i, s] =', stan_file, fixed = TRUE)] <-
    gsub('i, s', '1:n, s', stan_file[grep('ypred[i, s] =', stan_file, fixed = TRUE)])
  stan_file[grep('matrix[n, n_series] ypred;', stan_file, fixed = TRUE)] <-
    'array[n, n_series] int ypred;'

  # Remove un-needed loops for transformed beta parameters
  b_i_indices <- grep('b[i] = ', stan_file, fixed = TRUE)
  if(length(b_i_indices > 0)){
    for(x in b_i_indices){
      i_text <- paste0(as.numeric(sub("for \\(i in ", "",
                                      sub("\\:.*", "",
                                          stan_file[x - 1]))),
                       ':', sub(" ", "",
                                sub("\\{", "",
                                    sub("\\)", "",
                                        sub(".*\\:", "",
                                            stan_file[x - 1])))))
      stan_file[x] <-
        paste0(gsub('[i]', paste0('[', i_text, ']'), stan_file[x], fixed = TRUE))
    }
    stan_file <- stan_file[-c(b_i_indices - 1,
                              b_i_indices + 1)]
  }

  # Remove un-needed loop for random effect priors
  b_i_indices <- grep('// prior (non-centred) for', stan_file, fixed = TRUE)
  if(length(b_i_indices > 0)){
    for(x in b_i_indices){
      x = x + 2
      i_text <- paste0(as.numeric(sub("for \\(i in ", "",
                                      sub("\\:.*", "",
                                          stan_file[x - 1]))),
                       ':', sub(" ", "",
                                sub("\\{", "",
                                    sub("\\)", "",
                                        sub(".*\\:", "",
                                            stan_file[x - 1])))))
      stan_file[x] <-
        paste0(gsub('[i]', paste0('[', i_text, ']'), stan_file[x], fixed = TRUE))
    }
    stan_file <- stan_file[-c(b_i_indices + 1,
                              b_i_indices + 3)]
  }

  # Replace any normal(0, 1) with std_normal() for faster computation
  stan_file <- readLines(textConnection(stan_file), n = -1)
  stan_file <- gsub('normal(0, 1)', 'std_normal()', stan_file, fixed = TRUE)

  # Change b to b_raw for any idx normals
  if(any(grep('for (i in idx', stan_file, fixed = TRUE))){
    lines_matching <- grep('for (i in idx', stan_file, fixed = TRUE)
    for(i in lines_matching){
      stan_file[i] <- gsub('\\bb\\b', 'b_raw', stan_file[i])
    }
  }

  # Final tidying of the Stan model for readability
  unlink('base_gam_stan.txt')
  stan_file <- readLines(textConnection(stan_file), n = -1)
  clean_up <- vector()
  for(x in 1:length(stan_file)){
    clean_up[x] <- stan_file[x-1] == "" & stan_file[x] == ""
  }
  clean_up[is.na(clean_up)] <- FALSE
  stan_file <- stan_file[!clean_up]


  #### Modify the Stan data list ####
  # Create matrix representing whether an observation was missing or not
  y_observed <- matrix(NA, ncol = NCOL(jags_data$y),
                       nrow = NROW(jags_data$y))
  for (i in 1:dim(jags_data$y)[1]) {
    for (s in 1:dim(jags_data$y)[2]) {
      if (is.na(jags_data$y[i, s])) {
        y_observed[i, s] = 0
      } else {
        y_observed[i, s] = 1
      }
    }
  }

  # Use -1 for any missing observations so Stan doesn't throw errors due to NAs
  y <- jags_data$y
  y[is.na(y)] <- -1

  # The data list for Stan
  stan_data <- jags_data
  stan_data$y <- y
  stan_data$y_observed <- y_observed
  stan_data$X <- t(stan_data$X)
  stan_data$total_obs <- NCOL(stan_data$X)
  stan_data$num_basis <- NROW(stan_data$X)

  if(any(grepl('// priors for smoothing parameters', stan_file, fixed = TRUE))){
    if('L' %in% names(jags_data)){
      stan_data$lambda_links <- jags_data$L
      stan_data$L <- NULL
      stan_data$n_raw_sp <- NCOL(stan_data$lambda_links)
      stan_data$n_sp <- NROW(stan_data$lambda_links)

    } else {
      stan_data$n_sp <- as.numeric(sub('\\) \\{', '',
                                       sub('for \\(i in 1\\:', '',
                                           jags_file[grep('lambda\\[i\\] ~ ',
                                                          trimws(jags_file)) - 1])))
    }
  }

  # Add discontiguous index values if required
  if(add_idxs){
    names(idx_vals) <- paste0('idx', seq_len(length(idx_vals)))
    stan_data <- append(stan_data, idx_vals)
  }

  # Add parametric prior means and precisions if required
  # if('p_taus' %in% names(jags_data)){
  #   stan_data$p_coefs <- array(jags_data$p_coefs, dim = length(jags_data$p_taus))
  #   stan_data$p_taus <- array(jags_data$p_taus, dim = length(jags_data$p_taus))
  # }

  # Add bounds if required
  if(!is.null(upper_bounds)){
    stan_data$U <- upper_bounds
  }

  return(list(stan_file = stan_file,
              model_data = stan_data,
              smooths_included = smooths_included))
}
