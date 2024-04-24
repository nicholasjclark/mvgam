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

  cat(object$model_file, sep = '\n')
}

#' @noRd
remove_likelihood = function(model_file){
  like_line <- grep('// likelihood functions',
                    model_file)
  all_open_braces <- grep('{', model_file,
                          fixed = TRUE)
  all_close_braces <- grep('}', model_file,
                           fixed = TRUE)
  open_distances <- like_line - all_open_braces
  open_distances[open_distances < 0] <- NA
  start_remove <- all_open_braces[
    which.min(open_distances)
  ]

  close_distances <- like_line - all_close_braces
  close_distances[close_distances > 0] <- NA
  end_remove <- all_close_braces[
    which.max(close_distances)
  ]

  model_file[-(start_remove:end_remove)]
}

#' @noRd
.autoformat <- function(stan_file, overwrite_file = TRUE,
                        backend = 'cmdstanr'){

  # No need to fill lv_coefs in each iteration if this is a
  # trend_formula model
  if(any(grepl('lv_coefs = Z;',
               stan_file, fixed = TRUE)) &
     !any(grepl('vector[n_lv] LV[n];',
                stan_file, fixed = TRUE))){
    stan_file <- stan_file[-grep('lv_coefs = Z;',
                                  stan_file, fixed = TRUE)]
    stan_file <- stan_file[-grep('matrix[n_series, n_lv] lv_coefs;',
                                 stan_file, fixed = TRUE)]
    stan_file[grep('trend[i, s] = dot_product(lv_coefs[s,], LV[i,]);',
                   stan_file, fixed = TRUE)] <-
      'trend[i, s] = dot_product(Z[s,], LV[i,]);'

    stan_file[grep('// posterior predictions',
                   stan_file, fixed = TRUE)-1] <-
      paste0(stan_file[grep('// posterior predictions',
                  stan_file, fixed = TRUE)-1],
             '\n',
             'matrix[n_series, n_lv] lv_coefs = Z;')
    stan_file <- readLines(textConnection(stan_file), n = -1)
  }

  if(backend == 'rstan' & rstan::stan_version() < '2.29.0'){
    # normal_id_glm became available in 2.29.0; this needs to be replaced
    # with the older non-glm version
    if(any(grepl('normal_id_glm',
                 stan_file, fixed = TRUE))){
      if(any(grepl("flat_ys ~ normal_id_glm(flat_xs,",
                   stan_file, fixed = TRUE))){
        start <- grep("flat_ys ~ normal_id_glm(flat_xs,",
                                stan_file, fixed = TRUE)
        end <- start + 2
        stan_file <- stan_file[-c((start + 1):(start + 2))]
        stan_file[start] <- 'flat_ys ~ normal(flat_xs * b, flat_sigma_obs);'
      }
    }
  }

  # Old ways of specifying arrays have been converted to errors in
  # the latest version of Cmdstan (2.32.0); this coincides with
  # a decision to stop automatically replacing these deprecations with
  # the canonicalizer, so we have no choice but to replace the old
  # syntax with this ugly bit of code

  # rstan dependency in Description should mean that updates should
  # always happen (mvgam depends on rstan >= 2.29.0)
  update_code <- TRUE

  # Tougher if using cmdstanr
  if(backend == 'cmdstanr'){
    if(cmdstanr::cmdstan_version() < "2.32.0"){
      # If the autoformat options from cmdstanr are available,
      # make use of them to update any deprecated array syntax
      update_code <- FALSE
    }
  }

  if(update_code){
    # Data modifications
    stan_file[grep("int<lower=0> ytimes[n, n_series]; // time-ordered matrix (which col in X belongs to each [time, series] observation?)",
                   stan_file, fixed = TRUE)] <-
      'array[n, n_series] int<lower=0> ytimes;  // time-ordered matrix (which col in X belongs to each [time, series] observation?)'

    stan_file[grep("int<lower=0> flat_ys[n_nonmissing]; // flattened nonmissing observations",
                               stan_file, fixed = TRUE)] <-
      'array[n_nonmissing] int<lower=0> flat_ys; // flattened nonmissing observations'

    stan_file[grep("int<lower=0> obs_ind[n_nonmissing]; // indices of nonmissing observations",
                   stan_file, fixed = TRUE)] <-
      "array[n_nonmissing] int<lower=0> obs_ind; // indices of nonmissing observations"

    if(any(grepl('int<lower=0> ytimes_trend[n, n_lv]; // time-ordered matrix for latent states',
                 stan_file, fixed = TRUE))){
      stan_file[grep("int<lower=0> ytimes_trend[n, n_lv]; // time-ordered matrix for latent states",
                     stan_file, fixed = TRUE)] <-
        "array[n, n_lv] int ytimes_trend;"
    }

    if(any(grepl('int idx', stan_file) &
           grepl('// discontiguous index values',
                 stan_file, fixed = TRUE))){
      lines_replace <- which(grepl('int idx', stan_file) &
                               grepl('// discontiguous index values',
                                     stan_file, fixed = TRUE))
      for(i in lines_replace){
        split_line <- strsplit(stan_file[i], ' ')[[1]]

        idxnum <- gsub(';', '',
                             gsub("\\s*\\[[^\\]+\\]", "",
                                  as.character(split_line[2])))
        idx_length <- gsub("\\]", "", gsub("\\[", "",
                                           regmatches(split_line[2],
                                                      gregexpr("\\[.*?\\]", split_line[2]))[[1]]))

        stan_file[i] <-
          paste0('array[',
                 idx_length,
                 '] int ',
                 idxnum,
                 '; // discontiguous index values')
      }
    }

    if(any(grepl('int<lower=0> cap[total_obs]; // upper limits of latent abundances',
                 stan_file, fixed = TRUE))){
      stan_file[grep('int<lower=0> cap[total_obs]; // upper limits of latent abundances',
                     stan_file, fixed = TRUE)] <-
        'array[total_obs] int<lower=0> cap; // upper limits of latent abundances'

      stan_file[grep('int flat_caps[n_nonmissing];',
                     stan_file, fixed = TRUE)] <-
        'array[n_nonmissing] int flat_caps;'
       }

    # Model modifications
    if(any(grepl('real flat_phis[n_nonmissing];',
                 stan_file, fixed = TRUE))){
      stan_file[grep("real flat_phis[n_nonmissing];",
                     stan_file, fixed = TRUE)] <-
        "array[n_nonmissing] real flat_phis;"
    }

    # n-mixture modifications
    if(any(grepl('real p_ub = poisson_cdf(max_k, lambda);',
                 stan_file, fixed = TRUE))){
      stan_file[grep('real p_ub = poisson_cdf(max_k, lambda);',
                     stan_file, fixed = TRUE)] <-
        'real p_ub = poisson_cdf(max_k | lambda);'
    }

    # trend_formula modifications
    if(any(grepl('int trend_rand_idx', stan_file) &
           grepl('// trend random effect indices',
                 stan_file, fixed = TRUE))){
      lines_replace <- which(grepl('int trend_rand_idx', stan_file) &
                               grepl('// trend random effect indices',
                                     stan_file, fixed = TRUE))
      for(i in lines_replace){
        split_line <- strsplit(stan_file[i], ' ')[[1]]

        trend_idxnum <- gsub(';', '',
                             gsub("\\s*\\[[^\\]+\\]", "",
                                  as.character(split_line[2])))
        idx_length <- gsub("\\]", "", gsub("\\[", "",
                                           regmatches(split_line[2],
                                                      gregexpr("\\[.*?\\]", split_line[2]))[[1]]))

        stan_file[i] <-
          paste0('array[',
                 idx_length,
                 '] int ',
                 trend_idxnum,
                 '; // trend random effect indices')
      }
    }

    if(any(grepl('int trend_idx', stan_file) &
           grepl('// discontiguous index values',
                 stan_file, fixed = TRUE))){
      lines_replace <- which(grepl('int trend_idx', stan_file) &
                               grepl('// discontiguous index values',
                                     stan_file, fixed = TRUE))
      for(i in lines_replace){
        split_line <- strsplit(stan_file[i], ' ')[[1]]

        trend_idxnum <- gsub(';', '',
                             gsub("\\s*\\[[^\\]+\\]", "",
                                  as.character(split_line[2])))
        idx_length <- gsub("\\]", "", gsub("\\[", "",
                                           regmatches(split_line[2],
                                                      gregexpr("\\[.*?\\]", split_line[2]))[[1]]))

        stan_file[i] <-
          paste0('array[',
                 idx_length,
                 '] int ',
                 trend_idxnum,
                 '; // discontiguous index values')
      }
    }

    if(any(grepl('vector[n_series] trend_raw[n];',
                 stan_file, fixed = TRUE))){
      stan_file[grep("vector[n_series] trend_raw[n];",
                     stan_file, fixed = TRUE)] <-
        "array[n] vector[n_series] trend_raw;"
    }

    if(any(grepl('vector[n_lv] error[n];',
                 stan_file, fixed = TRUE))){
      stan_file[grep("vector[n_lv] error[n];",
                     stan_file, fixed = TRUE)] <-
        "array[n] vector[n_lv] error;"
    }

    if(any(grepl('vector[n_series] error[n];',
                 stan_file, fixed = TRUE))){
      stan_file[grep("vector[n_series] error[n];",
                     stan_file, fixed = TRUE)] <-
        "array[n] vector[n_series] error;"
    }

    if(any(grepl('vector[n_lv] LV[n];',
                 stan_file, fixed = TRUE))){
      stan_file[grep("vector[n_lv] LV[n];",
                     stan_file, fixed = TRUE)] <-
        "array[n] vector[n_lv] LV;"
    }

    if(any(grepl('vector[n_series] mu[n - 1];',
                 stan_file, fixed = TRUE))){
      stan_file[grep("vector[n_series] mu[n - 1];",
                     stan_file, fixed = TRUE)] <-
        "array[n - 1] vector[n_series] mu;"
    }

    if(any(grepl('vector[n_lv] mu[n - 1];',
                 stan_file, fixed = TRUE))){
      stan_file[grep("vector[n_lv] mu[n - 1];",
                     stan_file, fixed = TRUE)] <-
        "array[n - 1] vector[n_lv] mu;"
    }

    if(any(grepl('vector[n_series] mu[n];',
                 stan_file, fixed = TRUE))){
      stan_file[grep("vector[n_series] mu[n];",
                     stan_file, fixed = TRUE)] <-
        "array[n] vector[n_series] mu;"
    }

    if(any(grepl('vector[n_lv] mu[n];',
                 stan_file, fixed = TRUE))){
      stan_file[grep("vector[n_lv] mu[n];",
                     stan_file, fixed = TRUE)] <-
        "array[n] vector[n_lv] mu;"
    }
    # Generated quantity modifications
    if(any(grepl('real<lower=0,upper=1> ypred[n, n_series];',
                 stan_file, fixed = TRUE))){
      stan_file[grep("real<lower=0,upper=1> ypred[n, n_series];",
                     stan_file, fixed = TRUE)] <-
        "array[n, n_series] real<lower=0,upper=1> ypred;"
    }

    if(any(grepl('real<lower=0> ypred[n, n_series];',
                 stan_file, fixed = TRUE))){
      stan_file[grep("real<lower=0> ypred[n, n_series];",
                     stan_file, fixed = TRUE)] <-
        "array[n, n_series] real<lower=0> ypred;"
    }

    # ARMA model modifications
    if(any(grepl('vector[n_series] epsilon[n];',
                 stan_file, fixed = TRUE))){
      stan_file[grep("vector[n_series] epsilon[n];",
                     stan_file, fixed = TRUE)] <-
        "array[n] vector[n_series] epsilon;"
    }

    if(any(grepl('vector[n_lv] epsilon[n];',
                 stan_file, fixed = TRUE))){
      stan_file[grep("vector[n_lv] epsilon[n];",
                     stan_file, fixed = TRUE)] <-
        "array[n] vector[n_lv] epsilon;"
    }

    # VARMA model modifications
    if(any(grepl('matrix[n_series, n_series] P[1];',
                 stan_file, fixed = TRUE))){
      stan_file[grep("matrix[n_series, n_series] P[1];",
                     stan_file, fixed = TRUE)] <-
        "array[1] matrix[n_series, n_series] P;"

      stan_file[grep("matrix[n_series, n_series] phiGamma[2, 1];",
                     stan_file, fixed = TRUE)] <-
        "array[2, 1] matrix[n_series, n_series] phiGamma;"
    }

    if(any(grepl('matrix initial_joint_var(matrix Sigma, matrix[] phi, matrix[] theta) {',
                 stan_file, fixed = TRUE))){
      stan_file[grep("matrix initial_joint_var(matrix Sigma, matrix[] phi, matrix[] theta) {",
                     stan_file, fixed = TRUE)] <-
        "matrix initial_joint_var(matrix Sigma, array[] matrix phi, array[] matrix theta) {"
    }

    if(any(grepl('matrix[n_lv, n_lv] P[1];',
                 stan_file, fixed = TRUE))){
      stan_file[grep("matrix[n_lv, n_lv] P[1];",
                     stan_file, fixed = TRUE)] <-
        "array[1] matrix[n_lv, n_lv] P;"

      stan_file[grep("matrix[n_lv, n_lv] R[1];",
                     stan_file, fixed = TRUE)] <-
        "array[1] matrix[n_lv, n_lv] R;"

      stan_file[grep("matrix[n_lv, n_lv] A_init[1];",
                     stan_file, fixed = TRUE)] <-
        "array[1] matrix[n_lv, n_lv] A_init;"

      stan_file[grep("matrix[n_lv, n_lv] theta_init[1];",
                     stan_file, fixed = TRUE)] <-
        "array[1] matrix[n_lv, n_lv] theta_init;"
    }

    if(any(grepl('matrix[n_series, n_series] R[1];',
                 stan_file, fixed = TRUE))){

      stan_file[grep("matrix[n_series, n_series] R[1];",
                     stan_file, fixed = TRUE)] <-
        "array[1] matrix[n_series, n_series] R;"

      stan_file[grep("matrix[n_series, n_series] A_init[1];",
                     stan_file, fixed = TRUE)] <-
        "array[1] matrix[n_series, n_series] A_init;"

      stan_file[grep("matrix[n_series, n_series] theta_init[1];",
                     stan_file, fixed = TRUE)] <-
        "array[1] matrix[n_series, n_series] theta_init;"
    }

    if(any(grepl('matrix[] rev_mapping(matrix[] P, matrix Sigma) {',
                 stan_file, fixed = TRUE))){
      stan_file[grep("matrix[] rev_mapping(matrix[] P, matrix Sigma) {",
                     stan_file, fixed = TRUE)] <-
        "array[] matrix rev_mapping(array[] matrix P, matrix Sigma) {"

      stan_file[grep("matrix[m, m] phi_for[p, p];   matrix[m, m] phi_rev[p, p];",
                     stan_file, fixed = TRUE)] <-
        'array[p, p] matrix[m, m] phi_for;   array[p, p] matrix[m, m] phi_rev;'

      stan_file[grep("matrix[m, m] Sigma_for[p+1];  matrix[m, m] Sigma_rev[p+1];",
                     stan_file, fixed = TRUE)] <-
        'array[p+1] matrix[m, m] Sigma_for;   array[p+1] matrix[m, m] Sigma_rev;'

      stan_file[grep("matrix[m, m] S_for_list[p+1];",
                     stan_file, fixed = TRUE)] <-
        'array[p+1] matrix[m, m] S_for_list;'
    }

    # VAR model modifications
    if(any(grepl('matrix[n_lv, n_lv] phiGamma[2, 1];',
                 stan_file, fixed = TRUE))){
      stan_file[grep('matrix[n_lv, n_lv] phiGamma[2, 1];',
                     stan_file, fixed = TRUE)] <-
        'array[2, 1] matrix[n_lv, n_lv] phiGamma;'
    }

    if(any(grepl('matrix[,] rev_mapping(matrix[] P, matrix Sigma) {',
                 stan_file, fixed = TRUE))){
      stan_file[grep("matrix[,] rev_mapping(matrix[] P, matrix Sigma) {",
                     stan_file, fixed = TRUE)] <-
        "array[,] matrix rev_mapping(array[] matrix P, matrix Sigma) {"

      stan_file[grep("matrix[m, m] phi_for[p, p];   matrix[m, m] phi_rev[p, p];",
                     stan_file, fixed = TRUE)] <-
        'array[p, p] matrix[m, m] phi_for;   array[p, p] matrix[m, m] phi_rev;'

      stan_file[grep("matrix[m, m] Sigma_for[p+1];  matrix[m, m] Sigma_rev[p+1];",
                     stan_file, fixed = TRUE)] <-
        'array[p+1] matrix[m, m] Sigma_for;   array[p+1] matrix[m, m] Sigma_rev;'

      stan_file[grep("matrix[m, m] S_for_list[p+1];",
                     stan_file, fixed = TRUE)] <-
        'array[p+1] matrix[m, m] S_for_list;'

      stan_file[grep("matrix[m, m] Gamma_trans[p+1];",
                     stan_file, fixed = TRUE)] <-
        'array[p+1] matrix[m, m] Gamma_trans;'

      stan_file[grep("matrix[m, m] phiGamma[2, p];",
                     stan_file, fixed = TRUE)] <-
        'array[2, p] matrix[m, m] phiGamma;'
    }

    if(any(grepl("real partial_log_lik(int[] seq, int start, int end,",
                 stan_file, fixed = TRUE))){
      stan_file[grepl("real partial_log_lik(int[] seq, int start, int end,",
                       stan_file, fixed = TRUE)] <-
        "real partial_log_lik(array[] int seq, int start, int end,"
    }

    if(any(grepl("data vector Y, vector mu, real[] shape) {",
                 stan_file, fixed = TRUE))){
      stan_file[grepl("data vector Y, vector mu, real[] shape) {" ,
                      stan_file, fixed = TRUE)] <-
        "data vector Y, vector mu, array[] real shape) {"
    }

    if(any(grepl("int<lower=1> seq[n_nonmissing]; // an integer sequence for reduce_sum slicing",
                 stan_file, fixed = TRUE))){
      stan_file[grepl("int<lower=1> seq[n_nonmissing]; // an integer sequence for reduce_sum slicing",
                      stan_file, fixed = TRUE)] <-
        "array[n_nonmissing] int<lower=1> seq; // an integer sequence for reduce_sum slicing"
    }
  }

  if(backend == 'rstan'){
    options(stanc.allow_optimizations = TRUE,
            stanc.auto_format = TRUE)
    out <- rstan::stanc(model_code = stan_file)$model_code
  } else {
    stan_file <- cmdstanr::write_stan_file(stan_file)
    cmdstan_mod <- cmdstanr::cmdstan_model(stan_file, compile = FALSE)
    out <- utils::capture.output(
      cmdstan_mod$format(
        max_line_length = 80,
        canonicalize = TRUE,
        overwrite_file = overwrite_file, backup = FALSE))
    out <- paste0(out, collapse = "\n")
  }
  return(out)
}

#### Replacement for MCMCvis functions to remove dependence on rstan for working
# with stanfit objects ####
#' @noRd
mcmc_summary = function(object,
            params = 'all',
            excl = NULL,
            ISB = TRUE,
            exact = TRUE,
            probs = c(0.025, 0.5, 0.975),
            hpd_prob = 0.95,
            HPD = FALSE,
            pg0 = FALSE,
            digits = NULL,
            round = NULL,
            Rhat = TRUE,
            n.eff = TRUE,
            func = NULL,
            func_name = NULL,
            variational = FALSE){
  if(variational){
    Rhat <- FALSE
    n.eff <- FALSE
  }
  # SORTING BLOCK

  if (methods::is(object, 'matrix'))
  {
    object2 <- mcmc_chains(object, params, excl, ISB, exact = exact, mcmc.list = FALSE)
  } else {
    if (methods::is(object, 'stanfit'))
    {
      object2 <- object
    } else {
      # rstanarm
      if (methods::is(object, 'stanreg'))
      {
        object2 <- object$stanfit
      } else {
        # brms
        if (methods::is(object, 'brmsfit'))
        {
          object2 <- object$fit
        } else {
          #jagsUI
          if (methods::is(object, 'jagsUI'))
          {
            object2 <- mcmc_chains(object)
          } else {
            object2 <- mcmc_chains(object, params, excl, ISB, exact = exact, mcmc.list = TRUE)
          }
        }
      }
    }
  }

  #--------------------------------------------------------------------------------------------------------------

  # PROCESSING BLOCK - JAGS AND MATRIX MCMC OUTPUT

  if (coda::is.mcmc.list(object2) == TRUE | methods::is(object, 'matrix'))
  {
    if (methods::is(object, 'matrix'))
    {
      np <- NCOL(object2)
      ch_bind <- object2
    } else {
      np <- NCOL(object2[[1]])
      if (np > 1) ch_bind <- do.call("rbind", object2) else ch_bind <- as.matrix(object2)
    }

    x <- list()

    # mean, sd, and quantiles

    if (!is.null(digits))
    {
      if (!is.null(round))
      {
        warning("'digits' and 'round' arguments cannot be used together. Using 'digits'.")
      }

      bind_mn <- data.frame(signif(apply(ch_bind, 2, mean), digits = digits))
      bind_sd <- data.frame(signif(apply(ch_bind, 2, stats::sd), digits = digits))
      colnames(bind_mn) <- "mean"
      colnames(bind_sd) <- "sd"

      if (HPD == FALSE)
      {
        if (length(probs)==1)
        {
          bind_q <- data.frame(signif(apply(ch_bind, 2, stats::quantile, probs = probs), digits = digits))
          colnames(bind_q) <-  paste0(signif(probs * 100, digits = 3), "%")
        } else {
          bind_q <- data.frame(t(signif(apply(ch_bind, 2, stats::quantile, probs = probs), digits = digits)))
          colnames(bind_q) <-  paste0(signif(probs * 100, digits = 3), "%")
        }
      }
      if (HPD == TRUE)
      {
        if (length(hpd_prob) > 1)
        {
          stop('specify only a single probability for HPD interval computation.')
        }
        bind_q <- data.frame(signif(coda::HPDinterval(coda::as.mcmc(ch_bind), prob = hpd_prob), digits = digits))
        colnames(bind_q) <- c(paste0(signif(hpd_prob * 100, digits = 3), "%_HPDL"), paste0(signif(hpd_prob * 100, digits = 3), "%_HPDU"))
      }
    }

    if (is.null(digits) & !is.null(round))
    {
      bind_mn <- data.frame(round(apply(ch_bind, 2, mean), digits = round))
      bind_sd <- data.frame(round(apply(ch_bind, 2, stats::sd), digits = round))
      colnames(bind_mn) <- "mean"
      colnames(bind_sd) <- "sd"

      if (HPD == FALSE)
      {
        if (length(probs)==1)
        {
          bind_q <- data.frame(round(apply(ch_bind, 2, stats::quantile, probs = probs), digits = round))
          colnames(bind_q) <-  paste0(signif(probs * 100, digits = 3), "%")
        } else {
          bind_q <- data.frame(t(round(apply(ch_bind, 2, stats::quantile, probs = probs), digits = round)))
          colnames(bind_q) <-  paste0(signif(probs * 100, digits = 3), "%")
        }
      }
      if (HPD == TRUE)
      {
        if (length(hpd_prob) > 1)
        {
          stop('specify only a single probability for HPD interval computation.')
        }
        bind_q <- data.frame(round(coda::HPDinterval(coda::as.mcmc(ch_bind), prob = hpd_prob), digits = round))
        colnames(bind_q) <- c(paste0(signif(hpd_prob * 100, digits = 3), "%_HPDL"), paste0(signif(hpd_prob * 100, digits = 3), "%_HPDU"))
      }
    }

    if (is.null(digits) & is.null(round))
    {
      bind_mn <- data.frame(apply(ch_bind, 2, mean))
      bind_sd <- data.frame(apply(ch_bind, 2, stats::sd))
      colnames(bind_mn) <- "mean"
      colnames(bind_sd) <- "sd"

      if (HPD == FALSE)
      {
        if (length(probs) == 1)
        {
          bind_q <- data.frame(apply(ch_bind, 2, stats::quantile, probs = probs))
          colnames(bind_q) <-  paste0(signif(probs * 100, digits =3), "%")
        } else {
          bind_q <- data.frame(t(apply(ch_bind, 2, stats::quantile, probs = probs)))
          colnames(bind_q) <-  paste0(signif(probs * 100, digits = 3), "%")
        }
      }
      if (HPD == TRUE)
      {
        if (length(hpd_prob) > 1)
        {
          stop('specify only a single probability for HPD interval computation.')
        }
        bind_q <- data.frame(coda::HPDinterval(coda::as.mcmc(ch_bind), prob = hpd_prob))
        colnames(bind_q) <- c(paste0(signif(hpd_prob * 100, digits = 3), "%_HPDL"), paste0(signif(hpd_prob * 100, digits = 3), "%_HPDU"))
      }
    }

    x[[1]] <- cbind(bind_mn, bind_sd, bind_q)

    # rhat

    if (Rhat == TRUE)
    {
      if (!methods::is(object, 'matrix'))
      {
        if (length(object2) > 1)
        {
          # If > 750 params use loop to calculate Rhat
          if (NCOL(object2[[1]]) > 750)
          {
            r_hat <- c(rep(NA, NCOL(object2[[1]])))
            for (v in 1:length(r_hat)) r_hat[v] <- round(coda::gelman.diag(object2[, v])$psrf[, 1], digits = 2)
            r_hat <- data.frame(r_hat)
            colnames(r_hat) <- "Rhat"
          } else {
            r_hat <- data.frame(round(coda::gelman.diag(object2, multivariate = FALSE)$psrf[, 1], digits = 2))
            colnames(r_hat) <- "Rhat"
          }
        } else {
          warning("Rhat statistic cannot be calculated with one chain. NAs inserted.")
          r_hat <- data.frame(rep(NA, np))
          colnames(r_hat) <- "Rhat"
        }
      } else {
        warning("Rhat statistic cannot be calculated with one chain (matrix input). NAs inserted.")
        r_hat <- data.frame(rep(NA, np))
        colnames(r_hat) <- "Rhat"
      }
      x[[(length(x) + 1)]] <- r_hat
    }

    # neff

    if (n.eff == TRUE)
    {
      if (!methods::is(object, 'matrix'))
      {
        neff <- data.frame(round(coda::effectiveSize(object2), digits = 0))
        colnames(neff) <- "n_eff"
      } else {
        warning('Number of effective samples cannot be calculated without individual chains (matrix input). NAs inserted.')
        neff <- data.frame(rep(NA, np))
        colnames(neff) <- "n_eff"
      }
      x[[(length(x) + 1)]] <- neff
    }

    # p>0

    if (pg0 == TRUE)
    {
      tpg <- data.frame(apply(ch_bind, 2, function(x) round(sum(x > 0) / length(x), 2)))
      colnames(tpg) <- 'p>0'
      x[[(length(x) + 1)]] <- tpg
    }


    # custom function

    if (!is.null(func))
    {
      if (!is.null(digits))
      {
        tmp <- signif(apply(ch_bind, 2, func), digits = digits)
      }
      if (is.null(digits) & !is.null(round))
      {
        tmp <- round(apply(ch_bind, 2, func), digits = round)
      }
      if (is.null(digits) & is.null(round))
      {
        tmp <- apply(ch_bind, 2, func)
      }
      if (!is.null(dim(tmp)))
      {
        tmp <- data.frame(t(tmp))
      } else {
        tmp <- data.frame(tmp)
      }
      if (!is.null(func_name))
      {
        if (length(func_name) != NCOL(tmp))
        {
          stop("length(func_name) must equal number of func outputs")
        }
        colnames(tmp) <- func_name
      } else {
        colnames(tmp) <- 'func'
      }

      x[[(length(x) + 1)]] <- tmp
    }

    # bind them
    mcmc_summary <- do.call("cbind", x)
  }

  #--------------------------------------------------------------------------------------------------------------
  # PROCESSING BLOCK - STAN OR JAGSUI MCMC OUTPUT

  if (methods::is(object2, 'stanfit') | methods::is(object, 'jagsUI'))
  {
    if (methods::is(object2, 'stanfit'))
    {
      # rhat and n_eff directly from rstan output
      all_params <- row.names(rstan::summary(object2)$summary)
      rs_df <- data.frame(rstan::summary(object2)$summary)

      #if brms, reassign names without b_ and r_ (as in MCMCchains)
      if (methods::is(object, 'brmsfit'))
      {
        sp_names_p <- names(object2@sim$samples[[1]])
        #remove b_ and r_
        st_nm <- substr(sp_names_p, start = 1, stop = 2)
        sp_names <- rep(NA, length(sp_names_p))
        b_idx <- which(st_nm == 'b_')
        r_idx <- which(st_nm == 'r_')
        ot_idx <- which(st_nm != 'b_' & st_nm != 'r_')
        #fill names vec with b_ and r_ removed
        sp_names[b_idx] <- gsub('b_', '', sp_names_p[b_idx])
        sp_names[r_idx] <- gsub('r_', '', sp_names_p[r_idx])
        sp_names[ot_idx] <- sp_names_p[ot_idx]

        #assign names to df
        all_params <- sp_names
        row.names(rs_df) <- all_params
      }
    }

    if (methods::is(object, 'jagsUI'))
    {
      all_params <- row.names(object$summary)
      rs_df <- data.frame(object$summary)
    }

    # filtering of parameters from rstan/jagsUI object - from MCMCchains
    if (ISB == TRUE)
    {
      names <- vapply(strsplit(all_params, split = "[", fixed = TRUE), `[`, 1, FUN.VALUE = character(1))
    } else {
      names <- all_params
    }

    x <- list()

    # INDEX BLOCK exclusions

    if (!is.null(excl))
    {
      rm_ind <- c()
      for (i in 1:length(excl))
      {
        if (ISB == TRUE)
        {
          n_excl <- vapply(strsplit(excl,
                                    split = "[", fixed = TRUE), `[`, 1, FUN.VALUE=character(1))
        } else {
          n_excl <- excl
        }

        if (exact == TRUE)
        {
          ind_excl <- which(names %in% n_excl[i])
        } else {
          ind_excl <- grep(n_excl[i], names, fixed = FALSE)
        }

        if (length(ind_excl) < 1)
        {
          warning(paste0("\"", excl[i], "\"", " not found in MCMC output. Check 'ISB'' and 'exact' arguments to make sure the desired parsing methods are being used."))
        }
        rm_ind <- c(rm_ind, ind_excl)
      }

      if (length(rm_ind) > 0)
      {
        dups <- which(duplicated(rm_ind))
        if (length(dups) > 0)
        {
          rm_ind2 <- rm_ind[-dups]
        } else {
          rm_ind2 <- rm_ind
        }
      } else {
        excl <- NULL
      }
    }

    # selections

    if (length(params) == 1)
    {
      if (params == "all")
      {
        if (is.null(excl))
        {
          f_ind <- 1:length(names)
        } else {
          f_ind <- (1:length(names))[-rm_ind2]
        }
      } else {
        if (exact == TRUE)
        {
          get_ind <- which(names %in% params)
        } else {
          get_ind <- grep(paste(params), names, fixed = FALSE)
        }

        if (length(get_ind) < 1)
        {
          stop(paste0("\"", params, "\"", " not found in MCMC output. Check 'ISB' and 'exact' arguments to make sure the desired parsing methods are being used."))
        }
        if (!is.null(excl))
        {
          if (identical(get_ind, rm_ind2))
          {
            stop("No parameters selected.")
          }
          matched <- stats::na.omit(match(rm_ind2, get_ind))
          if (length(matched) > 0)
          {
            f_ind <- get_ind[-matched]
          } else {
            f_ind <- get_ind
          }
        } else {
          f_ind <- get_ind
        }
      }
    } else {
      grouped <- c()
      for (i in 1:length(params))
      {
        if (exact == TRUE)
        {
          get_ind <- which(names %in% params[i])
        } else {
          get_ind <- grep(paste(params[i]), names, fixed = FALSE)
        }

        if (length(get_ind) < 1)
        {
          warning(paste0("\"", params[i], "\"", " not found in MCMC output. Check 'ISB' and 'exact' arguments to make sure the desired parsing methods are being used."))
          (next)()
        }
        grouped <- c(grouped, get_ind)
      }
      if (!is.null(excl))
      {
        if (identical(grouped, rm_ind2))
        {
          stop("No parameters selected.")
        }
        matched <- stats::na.omit(match(rm_ind2, grouped))
        if (length(matched) > 0)
        {
          t_ind <- grouped[-matched]
        } else {
          t_ind <- grouped
        }
        to.rm <- which(duplicated(t_ind))
        if (length(to.rm) > 0)
        {
          f_ind <- t_ind[-to.rm]
        } else {
          f_ind <- t_ind
        }
      } else {
        to.rm <- which(duplicated(grouped))
        if (length(to.rm) > 0)
        {
          f_ind <- grouped[-to.rm]
        } else {
          f_ind <- grouped
        }
      }
    }

    # end sort

    # convert object to matrix if computing non default intervals or using custom func
    if (!is.null(func) | HPD == TRUE |
        identical(probs, c(0.025, 0.5, 0.975)) == FALSE | pg0 == TRUE)
    {
      if (methods::is(object2, 'stanfit'))
      {
        #ensure is matrix, not vector
        ch_bind <- as.matrix(as.matrix(object2)[, f_ind])
      }
      if (methods::is(object, 'jagsUI'))
      {
        ch_bind <- mcmc_chains(object, params, excl, ISB)
      }
    }

    # mean, sd, and quantiles

    if (!is.null(digits))
    {
      if (!is.null(round))
      {
        warning("'digits' and 'round' arguments cannot be used together. Using 'digits'.")
      }

      bind_mn <- data.frame(signif(rs_df["mean"][f_ind, 1], digits = digits))
      bind_sd <- data.frame(signif(rs_df["sd"][f_ind, 1], digits = digits))
      colnames(bind_mn) <- "mean"
      colnames(bind_sd) <- "sd"

      if (HPD == FALSE)
      {
        if (length(probs)==1)
        {
          bind_q <- data.frame(signif(apply(ch_bind, 2, stats::quantile, probs = probs), digits = digits))
          colnames(bind_q) <-  paste0(signif(probs * 100, digits = 3), "%")
        } else {
          if (identical(probs, c(0.025, 0.5, 0.975)) == TRUE)
          {
            bind_LCI <- signif(rs_df["X2.5."][f_ind, 1], digits = digits)
            bind_med <- signif(rs_df["X50."][f_ind, 1], digits = digits)
            bind_UCI <- signif(rs_df["X97.5."][f_ind, 1], digits = digits)
            bind_q <- data.frame(cbind(bind_LCI, bind_med, bind_UCI))
            colnames(bind_q) <-  paste0(signif(probs * 100, digits = 3), "%")
          } else {
            bind_q <- data.frame(t(signif(apply(ch_bind, 2, stats::quantile, probs = probs), digits = digits)))
            colnames(bind_q) <-  paste0(signif(probs * 100, digits = 3), "%")
          }
        }
      }
      if (HPD == TRUE)
      {
        if (length(hpd_prob) > 1)
        {
          stop('Specify only a single probability for HPD interval computation.')
        }
        bind_q <- data.frame(signif(coda::HPDinterval(coda::as.mcmc(ch_bind), prob = hpd_prob), digits = digits))
        colnames(bind_q) <- c(paste0(signif(hpd_prob * 100, digits = 3), "%_HPDL"), paste0(signif(hpd_prob * 100, digits = 3), "%_HPDU"))
      }
    }

    if (is.null(digits) & !is.null(round))
    {
      bind_mn <- data.frame(round(rs_df["mean"][f_ind, 1], digits = round))
      bind_sd <- data.frame(round(rs_df["sd"][f_ind, 1], digits = round))
      colnames(bind_mn) <- "mean"
      colnames(bind_sd) <- "sd"

      if (HPD == FALSE)
      {
        if (length(probs)==1)
        {
          bind_q <- data.frame(round(apply(ch_bind, 2, stats::quantile, probs = probs), digits = round))
          colnames(bind_q) <-  paste0(signif(probs * 100, digits = 3), "%")
        } else {
          if (identical(probs, c(0.025, 0.5, 0.975)) == TRUE)
          {
            bind_LCI <- round(rs_df["X2.5."][f_ind, 1], digits = round)
            bind_med <- round(rs_df["X50."][f_ind, 1], digits = round)
            bind_UCI <- round(rs_df["X97.5."][f_ind, 1], digits = round)
            bind_q <- data.frame(cbind(bind_LCI, bind_med, bind_UCI))
            colnames(bind_q) <-  paste0(signif(probs * 100, digits = 3), "%")
          } else {
            bind_q <- data.frame(t(round(apply(ch_bind, 2, stats::quantile, probs = probs), digits = round)))
            colnames(bind_q) <-  paste0(signif(probs * 100, digits = 3), "%")
          }
        }
      }
      if (HPD == TRUE)
      {
        if (length(hpd_prob) > 1)
        {
          stop('Specify only a single probability for HPD interval computation.')
        }
        bind_q <- data.frame(round(coda::HPDinterval(coda::as.mcmc(ch_bind), prob = hpd_prob), digits = round))
        colnames(bind_q) <- c(paste0(signif(hpd_prob * 100, digits = 3), "%_HPDL"), paste0(signif(hpd_prob * 100, digits = 3), "%_HPDU"))
      }
    }

    if (is.null(digits) & is.null(round))
    {
      bind_mn <- data.frame(rs_df["mean"][f_ind, 1])
      bind_sd <- data.frame(rs_df["sd"][f_ind, 1])
      colnames(bind_mn) <- "mean"
      colnames(bind_sd) <- "sd"

      if (HPD == FALSE)
      {
        if (length(probs)==1)
        {
          bind_q <- data.frame(apply(ch_bind, 2, stats::quantile, probs = probs))
          colnames(bind_q) <-  paste0(signif(probs * 100, digits = 3), "%")
        } else {
          if (identical(probs, c(0.025, 0.5, 0.975)) == TRUE)
          {
            bind_LCI <- rs_df["X2.5."][f_ind, 1]
            bind_med <- rs_df["X50."][f_ind, 1]
            bind_UCI <- rs_df["X97.5."][f_ind, 1]
            bind_q <- data.frame(cbind(bind_LCI, bind_med, bind_UCI))
            colnames(bind_q) <-  paste0(signif(probs * 100, digits = 3), "%")
          } else {
            bind_q <- data.frame(t(apply(ch_bind, 2, stats::quantile, probs = probs)))
            colnames(bind_q) <-  paste0(signif(probs * 100, digits = 3), "%")
          }
        }
      }
      if (HPD == TRUE)
      {
        if (length(hpd_prob) > 1)
        {
          stop('Specify only a single probability for HPD interval computation.')
        }
        bind_q <- data.frame(coda::HPDinterval(coda::as.mcmc(ch_bind), prob = hpd_prob))
        colnames(bind_q) <- c(paste0(signif(hpd_prob * 100, digits = 3), "%_HPDL"), paste0(signif(hpd_prob * 100, digits = 3), "%_HPDU"))
      }
    }
    x[[1]] <- cbind(bind_mn, bind_sd, bind_q)

    # rhat - rhat in Stan calculated within chain (different than with coda package)

    if (Rhat == TRUE)
    {
      r_hat <- data.frame(round(rs_df["Rhat"][f_ind, 1], digits = 2))
      colnames(r_hat) <- "Rhat"
      x[[(length(x) + 1)]] <- r_hat
    }

    # neff - neff in Stan is calculated within chain (different than with coda package)

    if (n.eff == TRUE)
    {
      if (methods::is(object2, 'stanfit'))
      {
        neff <- data.frame(round(rs_df["n_eff"][f_ind, 1], digits = 0))
      }
      if (methods::is(object, 'jagsUI'))
      {
        neff <- data.frame(round(rs_df["n.eff"][f_ind, 1], digits = 0))
      }
      colnames(neff) <- "n_eff"
      x[[(length(x) + 1)]] <- neff
    }

    # p>0

    if (pg0 == TRUE)
    {
      tpg <- data.frame(apply(ch_bind, 2, function(x) round(sum(x > 0) / length(x), 2)))
      colnames(tpg) <- 'p>0'
      x[[(length(x) + 1)]] <- tpg
    }

    # custom function

    if (!is.null(func))
    {
      if (!is.null(digits))
      {
        tmp <- signif(apply(ch_bind, 2, func), digits = digits)
      }
      if (is.null(digits) & !is.null(round))
      {
        tmp <- round(apply(ch_bind, 2, func), digits = round)
      }
      if (is.null(digits) & is.null(round))
      {
        tmp <- apply(ch_bind, 2, func)
      }
      if (!is.null(dim(tmp)))
      {
        tmp <- data.frame(t(tmp))
      } else {
        tmp <- data.frame(tmp)
      }
      if (!is.null(func_name))
      {
        if (length(func_name) != NCOL(tmp))
        {
          stop("length(func_name) must equal number of func outputs")
        }
        colnames(tmp) <- func_name
      } else {
        colnames(tmp) <- 'func'
      }

      x[[(length(x) + 1)]] <- tmp
    }

    # bind them

    mcmc_summary <- do.call("cbind", x)
    row.names(mcmc_summary) <- all_params[f_ind]

    if(variational){
      mcmc_summary$Rhat <- NaN
      mcmc_summary$n.eff <- NaN
    }
  }
  return(mcmc_summary)
}

#' @noRd
mcmc_chains = function(object,
                       params = 'all',
                       excl = NULL,
                       ISB = TRUE,
                       exact = TRUE,
                       mcmc.list = FALSE,
                       chain_num = NULL){
  #for rstanarm/brms objects - set to NULL by default
  sp_names <- NULL

  #if mcmc object (from nimble) - convert to mcmc.list
  if (methods::is(object, 'mcmc'))
  {
    object <- coda::mcmc.list(object)
  }

  #if list object of matrices (from nimble) - convert to mcmc.list
  if (methods::is(object, 'list'))
  {
    object <- coda::mcmc.list(lapply(object, function(x) coda::mcmc(x)))
  }

  if (coda::is.mcmc.list(object) != TRUE &
      !methods::is(object, 'matrix') &
      !methods::is(object, 'mcmc') &
      !methods::is(object, 'list') &
      !methods::is(object, 'rjags') &
      !methods::is(object, 'stanfit') &
      !methods::is(object, 'brmsfit') &
      !methods::is(object, 'jagsUI') &
      !methods::is(object, 'CmdStanMCMC'))
  {
    stop('Invalid object type. Input must be stanfit object (rstan), CmdStanMCMC object (cmdstanr), stanreg object (rstanarm), brmsfit object (brms), mcmc.list object (coda/rjags), mcmc object (coda/nimble), list object (nimble), rjags object (R2jags), jagsUI object (jagsUI), or matrix with MCMC chains.')
  }

  #NAME SORTING BLOCK
  if (methods::is(object, 'stanfit'))
  {
    #convert to mcmc.list
    temp_in <- rstan::As.mcmc.list(object)

    #assign new colnames for mcmc.list object if object exists (for stanreg and brms objs so parameter names are interpretable) - do not rename params for model fit directly with Stan
    if (!is.null(sp_names))
    {
      coda::varnames(temp_in) <- sp_names
    }

    if (ISB == TRUE)
    {
      names <- vapply(strsplit(colnames(temp_in[[1]]),
                               split = '[', fixed = TRUE), `[`, 1, FUN.VALUE=character(1))
    } else {
      names <- colnames(temp_in[[1]])
    }
  }

  if (methods::is(object, 'jagsUI'))
  {
    object <- object$samples
  }

  if (methods::is(object, 'CmdStanMCMC'))
  {
    object <- cmdstanr::as_mcmc.list(object)
  }

  if (coda::is.mcmc.list(object) == TRUE)
  {
    temp_in <- object
    if (is.null(colnames(temp_in[[1]])))
    {
      warning('No parameter names provided. Assigning arbitrary names.')
      sub_cn <- paste0('Param_', 1:NCOL(temp_in[[1]]))
      colnames(temp_in[[1]]) <- sub_cn
    }

    if (ISB == TRUE)
    {
      names <- vapply(strsplit(colnames(temp_in[[1]]),
                               split = "[", fixed = TRUE), `[`, 1, FUN.VALUE=character(1))
    } else {
      names <- colnames(temp_in[[1]])
    }
  }

  if (methods::is(object, 'matrix'))
  {
    temp_in <- object
    if (is.null(colnames(temp_in)))
    {
      warning('No parameter names (column names) provided. Assigning arbitrary names.')
      sub_cn <- paste0('Param_', 1:NCOL(temp_in))
      colnames(temp_in) <- sub_cn
    }

    if (ISB == TRUE)
    {
      names <- vapply(strsplit(colnames(temp_in),
                               split = "[", fixed = TRUE), `[`, 1, FUN.VALUE=character(1))
    } else {
      names <- colnames(temp_in)
    }
  }

  if (methods::is(object, 'rjags'))
  {
    temp_in <- object$BUGSoutput$sims.matrix
    if (ISB == TRUE)
    {
      names <- vapply(strsplit(rownames(object$BUGSoutput$summary),
                               split = "[", fixed = TRUE), `[`, 1, FUN.VALUE=character(1))
    } else {
      names <- rownames(object$BUGSoutput$summary)
    }
  }

  #INDEX BLOCK
  #exclusions
  if (!is.null(excl))
  {
    rm_ind <- c()
    for (i in 1:length(excl))
    {
      if (ISB == TRUE)
      {
        n_excl <- vapply(strsplit(excl,
                                  split = "[", fixed = TRUE), `[`, 1, FUN.VALUE=character(1))
      } else {
        n_excl <- excl
      }

      if (exact == TRUE)
      {
        ind_excl <- which(names %in% n_excl[i])
      } else {
        ind_excl <- grep(n_excl[i], names, fixed = FALSE)
      }

      if (length(ind_excl) < 1)
      {
        warning(paste0("\"", excl[i], "\"", " not found in MCMC output. Check 'ISB' and 'exact' arguments to make sure the desired parsing methods are being used."))
      }
      rm_ind <- c(rm_ind, ind_excl)
    }
    if (length(rm_ind) > 0)
    {
      dups <- which(duplicated(rm_ind))
      if (length(dups) > 0)
      {
        rm_ind2 <- rm_ind[-dups]
      } else {
        rm_ind2 <- rm_ind
      }
    } else {
      excl <- NULL
    }
  }

  #selections
  if (length(params) == 1)
  {
    if (params == 'all')
    {
      if (is.null(excl))
      {
        f_ind <- 1:length(names)
      } else {
        f_ind <- (1:length(names))[-rm_ind2]
      }
    } else {
      if (exact == TRUE)
      {
        get_ind <- which(names %in% params)
      } else {
        get_ind <- grep(paste(params), names, fixed = FALSE)
      }

      if (length(get_ind) < 1)
      {
        stop(paste0("\"", params, "\"", " not found in MCMC output. Check `ISB` and `exact` arguments to make sure the desired parsing methods are being used."))
      }
      if (!is.null(excl))
      {
        if (identical(get_ind, rm_ind2))
        {
          stop('No parameters selected.')
        }
        matched <- stats::na.omit(match(rm_ind2, get_ind))
        if (length(matched) > 0)
        {
          f_ind <- get_ind[-matched]
        } else {
          f_ind <- get_ind
        }
      } else {
        f_ind <- get_ind
      }
    }
  } else {
    grouped <- c()
    for (i in 1:length(params))
    {
      if (exact == TRUE)
      {
        get_ind <- which(names %in% params[i])
      } else {
        get_ind <- grep(paste(params[i]), names, fixed = FALSE)
      }

      if (length(get_ind) < 1)
      {
        warning(paste0("\"", params[i], "\"", " not found in MCMC output. Check 'ISB' and 'exact' arguments to make sure the desired parsing methods are being used."))
        next()
      }
      grouped <- c(grouped, get_ind)
    }
    if (!is.null(excl))
    {
      if (identical(grouped, rm_ind2))
      {
        stop('No parameters selected.')
      }
      matched <- stats::na.omit(match(rm_ind2, grouped))
      if (length(matched) > 0)
      {
        t_ind <- grouped[-matched]
      } else {
        t_ind <- grouped
      }
      to.rm <- which(duplicated(t_ind))
      if (length(to.rm) > 0)
      {
        f_ind <- t_ind[-to.rm]
      } else {
        f_ind <- t_ind
      }
    } else {
      to.rm <- which(duplicated(grouped))
      if (length(to.rm) > 0)
      {
        f_ind <- grouped[-to.rm]
      } else {
        f_ind <- grouped
      }
    }
  }

  #PROCESSING BLOCK
  if (is.null(chain_num))
  {
    if (coda::is.mcmc.list(object) == TRUE | typeof(object) == 'S4')
    {
      if (length(f_ind) > 1)
      {
        dsort_mcmc <- do.call(coda::mcmc.list, temp_in[,f_ind])
        OUT <- do.call('rbind', dsort_mcmc)
      } else {
        dsort_mcmc <- do.call(coda::mcmc.list, temp_in[,f_ind, drop = FALSE])
        OUT <- as.matrix(do.call(coda::mcmc.list, temp_in[,f_ind, drop = FALSE]), ncol = 1)
      }
    }
    if (methods::is(object, 'matrix'))
    {
      OUT <- temp_in[,f_ind, drop = FALSE]
      if (mcmc.list == TRUE)
      {
        stop('Cannot produce mcmc.list output with matrix input')
      }
    }

    if (methods::is(object, 'rjags'))
    {
      OUT <- temp_in[,f_ind, drop = FALSE]
      if (mcmc.list == TRUE)
      {
        #modified coda::as.mcmc (removing ordering of param names)
        x <- object$BUGSoutput
        mclist <- vector("list", x$n.chains)
        mclis <- vector("list", x$n.chains)
        ord <- dimnames(x$sims.array)[[3]]
        for (i in 1:x$n.chains)
        {
          tmp1 <- x$sims.array[, i, ord]
          mclis[[i]] <- coda::mcmc(tmp1, thin = x$n.thin)
        }
        temp2 <- coda::as.mcmc.list(mclis)
        #end mod as.mcmc
        dsort_mcmc <- do.call(coda::mcmc.list, temp2[,f_ind, drop = FALSE])
      }
    }
  }

  if (!is.null(chain_num))
  {
    if (coda::is.mcmc.list(object) == TRUE | typeof(object) == 'S4')
    {
      if (length(f_ind) > 1)
      {
        dsort <- do.call(coda::mcmc.list, temp_in[,f_ind])

        if (chain_num > length(dsort))
        {
          stop('Invalid value for chain_num specified.')
        }
        dsort_mcmc <- dsort[[chain_num]]
        OUT <- as.matrix(dsort_mcmc)
      } else {
        dsort <- do.call(coda::mcmc.list, temp_in[,f_ind, drop = FALSE])

        if (chain_num > length(dsort))
        {
          stop('Invalid value for chain_num specified.')
        }
        dsort_mcmc <- dsort[[chain_num]]
        OUT <- as.matrix(dsort_mcmc)
      }
    }

    if (methods::is(object, 'matrix'))
    {
      stop('Cannot extract posterior information for individual chains from matrix input.')
    }

    if (methods::is(object, 'rjags'))
    {
      #modified coda::as.mcmc (removing ordering of param names)
      x <- object$BUGSoutput
      mclist <- vector("list", x$n.chains)
      mclis <- vector("list", x$n.chains)
      ord <- dimnames(x$sims.array)[[3]]
      for (i in 1:x$n.chains)
      {
        tmp1 <- x$sims.array[, i, ord]
        mclis[[i]] <- coda::mcmc(tmp1, thin = x$n.thin)
      }
      temp2 <- coda::as.mcmc.list(mclis)
      #end mod as.mcmc
      dsort <- do.call(coda::mcmc.list, temp2[,f_ind, drop = FALSE])
      if (chain_num > length(dsort))
      {
        stop('Invalid value for chain_num specified.')
      }
      dsort_mcmc <- dsort[[chain_num]]
      OUT <- as.matrix(dsort_mcmc)
    }
  }

  if (mcmc.list == FALSE)
  {
    return(OUT)
  }
  if (mcmc.list == TRUE)
  {
    return(dsort_mcmc)
  }
}


#### Vectorise a stan model's likelihood for quicker computation ####
#' @noRd
#' @param model_file Stan model file to be edited
#' @param model_data Prepared mvgam data for Stan modelling
#' @param family \code{character}
#' @param trend_model \code{character} specifying the time series dynamics for the latent trend.
#' @param offset \code{logical}
#' @param drift \code{logical}
#' @param threads \code{integer} Experimental option to use multithreading for within-chain
#'parallelisation in \code{Stan}. We recommend its use only if you are experienced with
#'\code{Stan}'s `reduce_sum` function and have a slow running model that cannot be sped
#'up by any other means
#' @return A `list` containing the updated Stan model and model data
vectorise_stan_lik = function(model_file, model_data, family = 'poisson',
                              trend_model = 'None', offset = FALSE,
                              drift = FALSE,
                              threads = 1){

  if(family %in% c('binomial', 'beta_binomial', 'bernoulli')){
    family <- 'poisson'
  }

  # Hack for adding VAR1 models
  if(trend_model %in% c('VAR1', 'VAR1cor')){
    VAR1 <- TRUE
    trend_model <- 'RW'
  } else {
    VAR1 <- FALSE
  }

  # Similar hack for adding piecewise trends
  if(trend_model %in% c('PWlinear', 'PWlogistic')){
    trend_model <- 'RW'
  }

  #### Family specifications ####
  if(threads > 1){
    if(family == 'gaussian'){
      if(any(grepl('functions {', model_file, fixed = TRUE))){
        model_file[grep('functions {', model_file, fixed = TRUE)] <-
          paste0('functions {\n',
                 'real partial_log_lik(int[] seq, int start, int end,\n',
                 ifelse(offset, 'data vector Y, matrix X, vector b, vector sigma_obs, vector alpha) {\n',
                        'data vector Y, matrix X, vector b, vector sigma_obs, real alpha) {\n'),
                 'real ptarget = 0;\n',
                 ifelse(offset,'ptarget += normal_id_glm_lpdf(Y[start:end] | X[start:end], alpha[start:end], b, sigma_obs[start:end]);\n',
                        'ptarget += normal_id_glm_lpdf(Y[start:end] | X[start:end], alpha, b, sigma_obs[start:end]);\n'),
                 'return ptarget;\n',
                 '}\n')

      } else {
        model_file[grep('Stan model code', model_file)] <-
          paste0('// Stan model code generated by package mvgam\n',
                 'functions {\n',
                 'real partial_log_lik(int[] seq, int start, int end,\n',
                 ifelse(offset, 'data vector Y, matrix X, vector b, vector sigma_obs, vector alpha) {\n',
                        'data vector Y, matrix X, vector b, vector sigma_obs, real alpha) {\n'),
                 'real ptarget = 0;\n',
                 ifelse(offset,'ptarget += normal_id_glm_lpdf(Y[start:end] | X[start:end], alpha[start:end], b, sigma_obs[start:end]);\n',
                        'ptarget += normal_id_glm_lpdf(Y[start:end] | X[start:end], alpha, b, sigma_obs[start:end]);\n'),
                 'return ptarget;\n',
                 '}\n}\n')
      }
    }

    if(family == 'poisson'){
      if(any(grepl('functions {', model_file, fixed = TRUE))){
        model_file[grep('functions {', model_file, fixed = TRUE)] <-
          paste0('functions {\n',
                 'real partial_log_lik(int[] seq, int start, int end,\n',
                 ifelse(offset, 'data int[] Y, matrix X, vector b, vector alpha) {\n',
                        'data int[] Y, matrix X, vector b, real alpha) {\n'),
                 'real ptarget = 0;\n',
                 ifelse(offset,'ptarget += poisson_log_glm_lpmf(Y[start:end] | X[start:end], alpha[start:end], b);\n',
                        'ptarget += poisson_log_glm_lpmf(Y[start:end] | X[start:end], alpha, b);\n'),
                 'return ptarget;\n',
                 '}\n')
      } else {
        model_file[grep('Stan model code', model_file)] <-
          paste0('// Stan model code generated by package mvgam\n',
                 'functions {\n',
                 'real partial_log_lik(int[] seq, int start, int end,\n',
                 ifelse(offset, 'data int[] Y, matrix X, vector b, vector alpha) {\n',
                        'data int[] Y, matrix X, vector b, real alpha) {\n'),
                 'real ptarget = 0;\n',
                 ifelse(offset,'ptarget += poisson_log_glm_lpmf(Y[start:end] | X[start:end], alpha[start:end], b);\n',
                        'ptarget += poisson_log_glm_lpmf(Y[start:end] | X[start:end], alpha, b);\n'),
                 'return ptarget;\n',
                 '}\n}\n')
      }
    }

    if(family == 'lognormal'){
      if(any(grepl('functions {', model_file, fixed = TRUE))){
        model_file[grep('functions {', model_file, fixed = TRUE)] <-
          paste0('functions {\n',
                 'real partial_log_lik(int[] seq, int start, int end,\n',
                 'data vector Y, vector mu, real[] sigma_obs) {\n',
                 'real ptarget = 0;\n',
                 'ptarget += lognormal_lpdf(Y[start:end] | mu[start:end],\n',
                 'sigma_obs[start:end]);\n',
                 'return ptarget;\n',
                 '}\n')
      } else {
        model_file[grep('Stan model code', model_file)] <-
          paste0('// Stan model code generated by package mvgam\n',
                 'functions {\n',
                 'real partial_log_lik(int[] seq, int start, int end,\n',
                 'data vector Y, vector mu, real[] sigma_obs) {\n',
                 'real ptarget = 0;\n',
                 'ptarget += lognormal_lpdf(Y[start:end] | mu[start:end],\n',
                 'sigma_obs[start:end]);\n',
                 'return ptarget;\n',
                 '}\n}')
      }
    }

    if(family == 'beta'){
      if(any(grepl('functions {', model_file, fixed = TRUE))){
        model_file[grep('functions {', model_file, fixed = TRUE)] <-
          paste0('functions {\n',
                 'real partial_log_lik(int[] seq, int start, int end,\n',
                 'data vector Y, vector mu, vector phi) {\n',
                 'real ptarget = 0;\n',
                 'ptarget += beta_lpdf(Y[start:end] | inv_logit(mu[start:end]) .* phi[start:end],\n',
                 '(1 - inv_logit(mu[start:end])) .* phi[start:end]);\n',
                 'return ptarget;\n',
                 '}\n')
      } else {
        model_file[grep('Stan model code', model_file)] <-
          paste0('// Stan model code generated by package mvgam\n',
                 'functions {\n',
                 'real partial_log_lik(int[] seq, int start, int end,\n',
                 'data vector Y, vector mu, vector phi) {\n',
                 'real ptarget = 0;\n',
                 'ptarget += beta_lpdf(Y[start:end] | inv_logit(mu[start:end]) .* phi[start:end],\n',
                 '(1 - inv_logit(mu[start:end])) .* phi[start:end]);\n',
                 'return ptarget;\n',
                 '}\n}')
      }
    }

    if(family == 'student'){
      if(any(grepl('functions {', model_file, fixed = TRUE))){
        model_file[grep('functions {', model_file, fixed = TRUE)] <-
          paste0('functions {\n',
                 'real partial_log_lik(int[] seq, int start, int end,\n',
                 'data vector Y, vector mu, real[] sigma_obs, real[] nu) {\n',
                 'real ptarget = 0;\n',
                 'ptarget += student_t_lpdf(Y[start:end] | nu[start:end], mu[start:end],\n',
                 'sigma_obs[start:end]);\n',
                 'return ptarget;\n',
                 '}\n')
      } else {
        model_file[grep('Stan model code', model_file)] <-
          paste0('// Stan model code generated by package mvgam\n',
                 'functions {\n',
                 'real partial_log_lik(int[] seq, int start, int end,\n',
                 'data vector Y, vector mu, real[] sigma_obs, real[] nu) {\n',
                 'real ptarget = 0;\n',
                 'ptarget += student_t_lpdf(Y[start:end] | nu[start:end], mu[start:end],\n',
                 'sigma_obs[start:end]);\n',
                 'return ptarget;\n',
                 '}\n}')
      }
    }

    if(family == 'negative binomial'){
      if(any(grepl('functions {', model_file, fixed = TRUE))){
        model_file[grep('functions {', model_file, fixed = TRUE)] <-
          paste0('functions {\n',
                 'real partial_log_lik(int[] seq, int start, int end,\n',
                 'data int[] Y, vector mu, real[] phi) {\n',
                 'real ptarget = 0;\n',
                 'ptarget += neg_binomial_2_lpmf(Y[start:end] | mu[start:end],\n',
                 'inv(phi[start:end]));\n',
                 'return ptarget;\n',
                 '}\n')
      } else {
        model_file[grep('Stan model code', model_file)] <-
          paste0('// Stan model code generated by package mvgam\n',
                 'functions {\n',
                 'real partial_log_lik(int[] seq, int start, int end,\n',
                 'data int[] Y, vector mu, real[] phi) {\n',
                 'real ptarget = 0;\n',
                 'ptarget += neg_binomial_2_lpmf(Y[start:end] | mu[start:end],\n',
                 'inv(phi[start:end]));\n',
                 'return ptarget;\n',
                 '}\n}')
      }
    }

    if(family == 'Gamma'){
      if(any(grepl('functions {', model_file, fixed = TRUE))){
        model_file[grep('functions {', model_file, fixed = TRUE)] <-
          paste0('functions {\n',
                 'real partial_log_lik(int[] seq, int start, int end,\n',
                 'data vector Y, vector mu, real[] shape) {\n',
                 'real ptarget = 0;\n',
                 'ptarget += gamma_lpdf(Y[start:end] | shape[start:end], shape[start:end] ./ mu[start:end]);\n',
                 'return ptarget;\n',
                 '}\n')
      } else {
        model_file[grep('Stan model code', model_file)] <-
          paste0('// Stan model code generated by package mvgam\n',
                 'functions {\n',
                 'real partial_log_lik(int[] seq, int start, int end,\n',
                 'data vector Y, vector mu, real[] shape) {\n',
                 'real ptarget = 0;\n',
                 'ptarget += gamma_lpdf(Y[start:end] | shape[start:end], shape[start:end] ./ mu[start:end]);\n',
                 'return ptarget;\n',
                 '}\n}')
      }
    }

    model_file <- readLines(textConnection(model_file), n = -1)
  }

  lik_line <- grep('// likelihood functions',
                   model_file, fixed = TRUE)
  model_file <- model_file[-c(lik_line:(lik_line + 6))]

  if(family == 'gaussian'){
    if(threads > 1){
      model_file[lik_line] <- paste0('{\n// likelihood functions\n',
                                     'vector[n_nonmissing] flat_trends;\n',
                                     'vector[n_nonmissing] flat_sigma_obs;\n',
                                     'flat_trends = (to_vector(trend))[obs_ind];\n',
                                     'flat_sigma_obs = rep_each(sigma_obs, n)[obs_ind];\n',
                                     'target += reduce_sum(partial_log_lik, seq,\n',
                                     'grainsize,\n',
                                     'flat_ys,\n',
                                     'append_col(flat_xs, flat_trends),\n',
                                     'append_row(b, 1.0),\n',
                                     'flat_sigma_obs,\n',
                                     ifelse(offset, 'offset[obs_ind],\n);\n}\n',
                                            '0.0);\n}\n}\n'))
    } else {
      model_file[lik_line] <- paste0('{\n// likelihood functions\n',
                                     'vector[n_nonmissing] flat_trends;\n',
                                     'vector[n_nonmissing] flat_sigma_obs;\n',
                                     'flat_trends = (to_vector(trend))[obs_ind];\n',
                                     'flat_sigma_obs = rep_each(sigma_obs, n)[obs_ind];\n',
                                     'flat_ys ~ normal_id_glm(append_col(flat_xs, flat_trends),\n',
                                     ifelse(offset,'offset[obs_ind],', '0.0,'),
                                     'append_row(b, 1.0),\n',
                                     'flat_sigma_obs);\n}\n}\n')
    }

  }

  if(family == 'poisson'){
    if(threads > 1){
      model_file[lik_line] <- paste0('{\n// likelihood functions\n',
                                     'vector[n_nonmissing] flat_trends;\n',
                                     'flat_trends = (to_vector(trend))[obs_ind];\n',
                                     'target += reduce_sum(partial_log_lik, seq,\n',
                                     'grainsize,\n',
                                     'flat_ys,\n',
                                     'append_col(flat_xs, flat_trends),\n',
                                     'append_row(b, 1.0),\n',
                                     ifelse(offset, 'offset[obs_ind]);\n}\n',
                                            '0.0);\n}\n}\n'))
    } else {
      model_file[lik_line] <- paste0('{\n// likelihood functions\n',
                                     'vector[n_nonmissing] flat_trends;\n',
                                     'flat_trends = (to_vector(trend))[obs_ind];\n',
                                     'flat_ys ~ poisson_log_glm(append_col(flat_xs, flat_trends),\n',
                                     ifelse(offset,'offset[obs_ind],', '0.0,'),
                                     'append_row(b, 1.0));\n}\n}\n')
    }

  }

  if(family == 'lognormal'){
    if(threads > 1){
      model_file[lik_line] <- paste0('{\n// likelihood functions\n',
                                     'vector[n_nonmissing] flat_trends;\n',
                                     'vector[n_nonmissing] flat_sigma_obs;\n',
                                     'flat_trends = (to_vector(trend))[obs_ind];\n',
                                     'flat_sigma_obs = rep_each(sigma_obs, n)[obs_ind];\n',
                                     'target += reduce_sum(partial_log_lik, seq,\n',
                                     'grainsize,\n',
                                     'flat_ys,\n',
                                     ifelse(offset,'append_col(flat_xs, flat_trends) * append_row(b, 1.0) + offset[obs_ind],\n',
                                            'append_col(flat_xs, flat_trends) * append_row(b, 1.0),\n'),
                                     'flat_sigma_obs);\n}\n}\n')
    } else {
      model_file[lik_line] <- paste0('{\n// likelihood functions\n',
                                     'vector[n_nonmissing] flat_trends;\n',
                                     'vector[n_nonmissing] flat_sigma_obs;\n',
                                     'flat_trends = (to_vector(trend))[obs_ind];\n',
                                     'flat_sigma_obs = rep_each(sigma_obs, n)[obs_ind];\n',
                                     'flat_ys ~ lognormal(\n',
                                     ifelse(offset,'append_col(flat_xs, flat_trends) * append_row(b, 1.0) + offset[obs_ind],\n',
                                            'append_col(flat_xs, flat_trends) * append_row(b, 1.0),\n'),
                                     'flat_sigma_obs);\n}\n}\n')
    }
    model_file <- readLines(textConnection(model_file), n = -1)
  }

  if(family == 'beta'){
    if(threads > 1){
      model_file[lik_line] <- paste0('{\n// likelihood functions\n',
                                     'vector[n_nonmissing] flat_trends;\n',
                                     'vector[n_nonmissing] flat_phis;\n',
                                     'flat_trends = (to_vector(trend))[obs_ind];\n',
                                     'flat_phis = rep_each(phi, n)[obs_ind];\n',
                                     'target += reduce_sum(partial_log_lik, seq,\n',
                                     'grainsize,\n',
                                     'flat_ys,\n',
                                     ifelse(offset,'append_col(flat_xs, flat_trends) * append_row(b, 1.0) + offset[obs_ind],\n',
                                            'append_col(flat_xs, flat_trends) * append_row(b, 1.0),\n'),
                                     'flat_phis);\n}\n}\n')
    } else {
      model_file[lik_line] <- paste0('{\n// likelihood functions\n',
                                     'vector[n_nonmissing] flat_trends;\n',
                                     'vector[n_nonmissing] flat_phis;\n',
                                     'flat_trends = (to_vector(trend))[obs_ind];\n',
                                     'flat_phis = rep_each(phi, n)[obs_ind];\n',
                                     'flat_ys ~ beta(\n',
                                     ifelse(offset,'inv_logit(append_col(flat_xs, flat_trends) * append_row(b, 1.0) + offset[obs_ind]) .* flat_phis,\n',
                                            'inv_logit(append_col(flat_xs, flat_trends) * append_row(b, 1.0)) .* flat_phis,\n'),
                                     ifelse(offset,'(1 - inv_logit(append_col(flat_xs, flat_trends) * append_row(b, 1.0) + offset[obs_ind])) .* flat_phis);\n}\n}\n',
                                            '(1 - inv_logit(append_col(flat_xs, flat_trends) * append_row(b, 1.0))) .* flat_phis);\n}\n}\n'))
    }
    model_file <- readLines(textConnection(model_file), n = -1)
  }

  if(family == 'Gamma'){
    if(threads > 1){
      model_file[lik_line] <- paste0('{\n// likelihood functions\n',
                                     'vector[n_nonmissing] flat_trends;\n',
                                     'vector[n_nonmissing] flat_shapes;\n',
                                     'flat_trends = (to_vector(trend))[obs_ind];\n',
                                     'flat_shapes = rep_each(shape, n)[obs_ind];\n',
                                     'target += reduce_sum(partial_log_lik, seq,\n',
                                     'grainsize,\n',
                                     'flat_ys,\n',
                                     ifelse(offset,'exp(append_col(flat_xs, flat_trends) * append_row(b, 1.0) + offset[obs_ind]),\n',
                                            'exp(append_col(flat_xs, flat_trends) * append_row(b, 1.0)),\n'),
                                     'flat_shapes);\n}\n}\n')
    } else {
      model_file[lik_line] <- paste0('{\n// likelihood functions\n',
                                     'vector[n_nonmissing] flat_trends;\n',
                                     'vector[n_nonmissing] flat_shapes;\n',
                                     'flat_trends = (to_vector(trend))[obs_ind];\n',
                                     'flat_shapes = rep_each(shape, n)[obs_ind];\n',
                                     'flat_ys ~ gamma(\n',
                                     'flat_shapes, flat_shapes ./ ',
                                     ifelse(offset, 'exp(append_col(flat_xs, flat_trends) * append_row(b, 1.0) + offset[obs_ind])',
                                            'exp(append_col(flat_xs, flat_trends) * append_row(b, 1.0))'),
                                     ');\n}\n}\n')
    }
    model_file <- readLines(textConnection(model_file), n = -1)
  }

  if(family == 'student'){
    if(threads > 1){
      model_file[lik_line] <- paste0('{\n// likelihood functions\n',
                                     'vector[n_nonmissing] flat_trends;\n',
                                     'vector[n_nonmissing] flat_sigma_obs;\n',
                                     'vector[n_nonmissing] flat_nu;\n',
                                     'flat_trends = (to_vector(trend))[obs_ind];\n',
                                     'flat_sigma_obs = rep_each(sigma_obs, n)[obs_ind];\n',
                                     'flat_nu = rep_each(nu, n)[obs_ind];\n',
                                     'target += reduce_sum(partial_log_lik, seq,\n',
                                     'grainsize,\n',
                                     'flat_ys,\n',
                                     ifelse(offset,'append_col(flat_xs, flat_trends) * append_row(b, 1.0) + offset[obs_ind],\n',
                                            'append_col(flat_xs, flat_trends) * append_row(b, 1.0),\n'),
                                     'flat_sigma_obs, flat_nu);\n}\n}\n')
    } else {
      model_file[lik_line] <- paste0('{\n// likelihood functions\n',
                                     'vector[n_nonmissing] flat_trends;\n',
                                     'vector[n_nonmissing] flat_sigma_obs;\n',
                                     'vector[n_nonmissing] flat_nu;\n',
                                     'flat_trends = (to_vector(trend))[obs_ind];\n',
                                     'flat_sigma_obs = rep_each(sigma_obs, n)[obs_ind];\n',
                                     'flat_nu = rep_each(nu, n)[obs_ind];\n',
                                     'flat_ys ~ student_t(flat_nu,\n',
                                     ifelse(offset,'append_col(flat_xs, flat_trends) * append_row(b, 1.0) + offset[obs_ind],\n',
                                            'append_col(flat_xs, flat_trends) * append_row(b, 1.0),\n'),
                                     'flat_sigma_obs);\n}\n}\n')
    }
    model_file <- readLines(textConnection(model_file), n = -1)
  }

  if(family == 'negative binomial'){
    if(threads > 1){
      model_file[lik_line] <- paste0('{\n// likelihood functions\n',
                                     'vector[n_nonmissing] flat_trends;\n',
                                     'real flat_phis[n_nonmissing];\n',
                                     'flat_trends = (to_vector(trend))[obs_ind];\n',
                                     'flat_phis = to_array_1d(rep_each(phi_inv, n)[obs_ind]);\n',
                                     'target += reduce_sum(partial_log_lik, seq,\n',
                                     'grainsize,\n',
                                     'flat_ys,\n',
                                     ifelse(offset,'exp(append_col(flat_xs, flat_trends) * append_row(b, 1.0) + offset[obs_ind]),\n',
                                            'exp(append_col(flat_xs, flat_trends) * append_row(b, 1.0)),\n'),
                                     'flat_phis);\n}\n}\n')
    } else {
      model_file[lik_line] <- paste0('{\n// likelihood functions\n',
                                     'vector[n_nonmissing] flat_trends;\n',
                                     'real flat_phis[n_nonmissing];\n',
                                     'flat_trends = (to_vector(trend))[obs_ind];\n',
                                     'flat_phis = to_array_1d(rep_each(phi_inv, n)[obs_ind]);\n',
                                     'flat_ys ~ neg_binomial_2(\n',
                                     ifelse(offset,'exp(append_col(flat_xs, flat_trends) * append_row(b, 1.0) + offset[obs_ind]),\n',
                                            'exp(append_col(flat_xs, flat_trends) * append_row(b, 1.0)),\n'),
                                     'inv(flat_phis));\n}\n}\n')
    }
    model_file <- readLines(textConnection(model_file), n = -1)
  }

  # Add the rep_each function to replicate series-varying parameters for particular families
  if(family %in% c('negative binomial', 'gaussian', 'lognormal',
                   'student', 'Gamma', 'beta')){
    model_file <- readLines(textConnection(model_file), n = -1)
    if(any(grepl('functions {', model_file, fixed = TRUE))){
      model_file[grep('functions {', model_file, fixed = TRUE)] <-
        paste0('functions {\n',
               'vector rep_each(vector x, int K) {\n',
               'int N = rows(x);\n',
               'vector[N * K] y;\n',
               'int pos = 1;\n',
               'for (n in 1:N) {\n',
               'for (k in 1:K) {\n',
               'y[pos] = x[n];\n',
               'pos += 1;\n',
               '}\n',
               '}\n',
               'return y;\n',
               '}\n')
    } else {
      model_file[grep('Stan model code', model_file)] <-
        paste0('// Stan model code generated by package mvgam\n',
               'functions {\n',
               'vector rep_each(vector x, int K) {\n',
               'int N = rows(x);\n',
               'vector[N * K] y;\n',
               'int pos = 1;\n',
               'for (n in 1:N) {\n',
               'for (k in 1:K) {\n',
               'y[pos] = x[n];\n',
               'pos += 1;\n',
               '}\n',
               '}\n',
               'return y;\n',
               '}\n}')
    }

    model_file <- readLines(textConnection(model_file), n = -1)
  }

  #### Data modifications ####
  # Gather the number of nonmissing observations
  model_data$n_nonmissing <- length(which(model_data$y_observed == 1))

  # Grab indices of nonmissing ys and include reduced sets of ys and Xs
  model_data$obs_ind <- which(as.vector(model_data$y_observed) == 1)
  model_data$flat_ys <- as.vector(model_data$y)[which(as.vector(model_data$y_observed) == 1)]
  model_data$X <- t(model_data$X)
  model_data$flat_xs <- as.matrix(model_data$X[as.vector(model_data$ytimes)[model_data$obs_ind],])

  # Add a grainsize integer
  if(threads > 1){
    model_data$seq <- 1:model_data$n_nonmissing
    model_data$grainsize <- max(100, floor(length(as.vector(model_data$y)) / threads))
  }

  # Update the data statement
  obs_line <- grep('int<lower=0, upper=1> y_observed[n, n_series]; // indices of missing vs observed',
                   model_file, fixed = TRUE)
  model_file <- model_file[-c(obs_line:(obs_line + 2))]

  obs_format <- 'int<lower=0> flat_ys[n_nonmissing];'
  if(family %in% c('gaussian', 'student')){
    obs_format <- 'vector[n_nonmissing] flat_ys;'
  }
  if(family %in% c('Gamma','lognormal')){
    obs_format <- 'vector<lower=0>[n_nonmissing] flat_ys;'
  }
  if(family == 'beta'){
    obs_format <- 'vector<lower=0,upper=1>[n_nonmissing] flat_ys;'
  }

  if(threads > 1){
    model_file[obs_line] <- paste0('int<lower=0> n_nonmissing;',
                                   ' // number of nonmissing observations\n',
                                   obs_format,
                                   ' // flattened nonmissing observations\n',
                                   'matrix[n_nonmissing, num_basis] flat_xs;',
                                   ' // X values for nonmissing observations\n',
                                   'int<lower=0> obs_ind[n_nonmissing];',
                                   ' // indices of nonmissing observations\n',
                                   'int<lower=1> grainsize;',
                                   ' // grainsize for reduce_sum threading\n',
                                   'int<lower=1> seq[n_nonmissing];',
                                   ' // an integer sequence for reduce_sum slicing\n',
                                   '}')
  } else {
    model_file[obs_line] <- paste0('int<lower=0> n_nonmissing;',
                                   ' // number of nonmissing observations\n',
                                   obs_format,
                                   ' // flattened nonmissing observations\n',
                                   'matrix[n_nonmissing, num_basis] flat_xs;',
                                   ' // X values for nonmissing observations\n',
                                   'int<lower=0> obs_ind[n_nonmissing];',
                                   ' // indices of nonmissing observations\n',
                                   '}')
  }

  # Some final edits to improve efficiency of the Stan models
  model_file <- gsub('row_vector[num_basis] b_raw;',
                     'vector[num_basis] b_raw;', model_file, fixed = TRUE)
  model_file <- gsub('row_vector[num_basis] b;',
                     'vector[num_basis] b;', model_file, fixed = TRUE)
  model_file <- gsub('matrix[num_basis, total_obs] X; // transposed mgcv GAM design matrix',
                     'matrix[total_obs, num_basis] X; // mgcv GAM design matrix',
                     model_file, fixed = TRUE)
  model_file <- model_file[-(grep('// GAM contribution to expectations (log scale)',
                                  model_file, fixed = TRUE):
                               (grep('// GAM contribution to expectations (log scale)',
                                     model_file, fixed = TRUE) + 5))]

  if(trend_model == 'GP'){
    model_file <- model_file[-(grep('eta = to_vector(b * X);',
                                    model_file, fixed = TRUE))]
    model_file <- model_file[-((grep('mus[1:n, s] = eta[ytimes[1:n, s]] + trend[1:n, s];',
                                     model_file, fixed = TRUE) - 1):
                                 (grep('mus[1:n, s] = eta[ytimes[1:n, s]] + trend[1:n, s];',
                                       model_file, fixed = TRUE) + 1))]

  } else {
    model_file <- model_file[-(grep('eta = to_vector(b * X);',
                                    model_file, fixed = TRUE):
                                 (grep('eta = to_vector(b * X);',
                                       model_file, fixed = TRUE) + 4))]
  }

  model_file <- model_file[-((grep('// posterior predictions',
                                   model_file, fixed = TRUE) + 1):
                               (grep('// posterior predictions',
                                     model_file, fixed = TRUE) + 3))]
  model_file[grep('generated quantities {',
                  model_file, fixed = TRUE)] <- paste0('generated quantities {\n',
                                                       'vector[total_obs] eta;\n',
                                                       'matrix[n, n_series] mus;')
  if(family == 'poisson'){
    model_file[grep('// posterior predictions',
                    model_file, fixed = TRUE)] <- paste0('// posterior predictions\n',
                                                         ifelse(offset, 'eta = X * b + offset;\n',
                                                                'eta = X * b;\n'),
                                                         'for(s in 1:n_series){ \n',
                                                         'mus[1:n, s] = eta[ytimes[1:n, s]] + trend[1:n, s];\n',
                                                         'ypred[1:n, s] = poisson_log_rng(mus[1:n, s]);\n',
                                                         '}')
  }

  if(family == 'negative binomial'){
    model_file[grep('// posterior predictions',
                    model_file, fixed = TRUE)] <- paste0('// posterior predictions\n',
                                                         ifelse(offset, 'eta = X * b + offset;\n',
                                                                'eta = X * b;\n'),
                                                         'for(s in 1:n_series){ \n',
                                                         'mus[1:n, s] = eta[ytimes[1:n, s]] + trend[1:n, s];\n',
                                                         'ypred[1:n, s] = neg_binomial_2_rng(exp(mus[1:n, s]), phi_vec[1:n, s]);\n',
                                                         '}')
  }

  if(family == 'gaussian'){
    model_file[grep('array[n, n_series] int ypred;',
                    model_file, fixed = TRUE)] <- 'array[n, n_series] real ypred;'
    model_file = readLines(textConnection(model_file), n = -1)
    model_file[grep('vector[num_basis] b_raw;',
                    model_file,
                    fixed = TRUE)] <- paste0('vector[num_basis] b_raw;\n',
                                             '// gaussian observation error\n',
                                             'vector<lower=0>[n_series] sigma_obs;')
    model_file[grep('// likelihood functions',
                    model_file,
                    fixed = TRUE) - 1] <- paste0('// priors for observation error parameters\n',
                                                 'sigma_obs ~ student_t(3, 0, 2);\n',
                                                 '{')
    model_file[grep('matrix[n, n_series] mus;',
                    model_file,
                    fixed = TRUE)] <- paste0('matrix[n, n_series] sigma_obs_vec;\n',
                                             'matrix[n, n_series] mus;')
    model_file[grep('// posterior predictions',
                    model_file, fixed = TRUE)] <- paste0('// posterior predictions\n',
                                                         ifelse(offset, 'eta = X * b + offset;\n',
                                                                'eta = X * b;\n'),
                                                         'for (s in 1:n_series) {\n',
                                                         'sigma_obs_vec[1:n,s] = rep_vector(sigma_obs[s], n);\n',
                                                         '}\n',
                                                         'for(s in 1:n_series){ \n',
                                                         'mus[1:n, s] = eta[ytimes[1:n, s]] + trend[1:n, s];\n',
                                                         'ypred[1:n, s] = normal_rng(mus[1:n, s], sigma_obs_vec[1:n, s]);\n',
                                                         '}')
  }

  if(family == 'student'){
    model_file[grep('array[n, n_series] int ypred;',
                    model_file, fixed = TRUE)] <- 'array[n, n_series] real ypred;'
    model_file = readLines(textConnection(model_file), n = -1)
    model_file[grep('vector[num_basis] b_raw;',
                    model_file,
                    fixed = TRUE)] <- paste0('vector[num_basis] b_raw;\n',
                                             '// student-t observation error\n',
                                             'vector<lower=0>[n_series] sigma_obs;\n',
                                             '// student-t df parameters\n',
                                             'vector<lower=0>[n_series] nu;')
    model_file[grep('// likelihood functions',
                    model_file,
                    fixed = TRUE) - 1] <- paste0('// priors for observation error parameters\n',
                                                 'sigma_obs ~ student_t(3, 0, 2);\n',
                                                 '// priors for df parameters\n',
                                                 'nu ~ gamma(2, 0.1);\n',
                                                 '{')
    model_file[grep('matrix[n, n_series] mus;',
                    model_file,
                    fixed = TRUE)] <- paste0('matrix[n, n_series] sigma_obs_vec;\n',
                                             'matrix[n, n_series] nu_vec;\n',
                                             'matrix[n, n_series] mus;')
    model_file[grep('// posterior predictions',
                    model_file, fixed = TRUE)] <- paste0('// posterior predictions\n',
                                                         ifelse(offset, 'eta = X * b + offset;\n',
                                                                'eta = X * b;\n'),
                                                         'for (s in 1:n_series) {\n',
                                                         'sigma_obs_vec[1:n,s] = rep_vector(sigma_obs[s], n);\n',
                                                         'nu_vec[1:n,s] = rep_vector(nu[s], n);\n',
                                                         '}\n',
                                                         'for(s in 1:n_series){ \n',
                                                         'mus[1:n, s] = eta[ytimes[1:n, s]] + trend[1:n, s];\n',
                                                         'ypred[1:n, s] = student_t_rng(nu_vec[1:n, s], mus[1:n, s], sigma_obs_vec[1:n, s]);\n',
                                                         '}')
  }

  if(family == 'lognormal'){
    model_file[grep('array[n, n_series] int ypred;',
                    model_file, fixed = TRUE)] <- 'real<lower=0> ypred[n, n_series];'
    model_file = readLines(textConnection(model_file), n = -1)
    model_file[grep('vector[num_basis] b_raw;',
                    model_file,
                    fixed = TRUE)] <- paste0('vector[num_basis] b_raw;\n',
                                             '// lognormal observation error\n',
                                             'vector<lower=0>[n_series] sigma_obs;')
    model_file[grep('// likelihood functions',
                    model_file,
                    fixed = TRUE) - 1] <- paste0('// priors for log(observation error) parameters\n',
                                                 'sigma_obs ~ student_t(3, 0, 1);\n',
                                                 '{')
    model_file[grep('matrix[n, n_series] mus;',
                    model_file,
                    fixed = TRUE)] <- paste0('matrix[n, n_series] sigma_obs_vec;\n',
                                             'matrix[n, n_series] mus;')
    model_file[grep('// posterior predictions',
                    model_file, fixed = TRUE)] <- paste0('// posterior predictions\n',
                                                         ifelse(offset, 'eta = X * b + offset;\n',
                                                                'eta = X * b;\n'),
                                                         'for (s in 1:n_series) {\n',
                                                         'sigma_obs_vec[1:n,s] = rep_vector(sigma_obs[s], n);\n',
                                                         '}\n',
                                                         'for(s in 1:n_series){ \n',
                                                         'mus[1:n, s] = eta[ytimes[1:n, s]] + trend[1:n, s];\n',
                                                         'ypred[1:n, s] = lognormal_rng(mus[1:n, s], sigma_obs_vec[1:n, s]);\n',
                                                         '}')
  }

  if(family == 'beta'){
    model_file[grep('array[n, n_series] int ypred;',
                    model_file, fixed = TRUE)] <- 'real<lower=0,upper=1> ypred[n, n_series];'
    model_file = readLines(textConnection(model_file), n = -1)
    model_file[grep('vector[num_basis] b_raw;',
                    model_file,
                    fixed = TRUE)] <- paste0('vector[num_basis] b_raw;\n',
                                             '// Beta precision parameters\n',
                                             'vector<lower=0>[n_series] phi;')
    model_file[grep('// likelihood functions',
                    model_file,
                    fixed = TRUE) - 1] <- paste0('// priors for precision parameters\n',
                                                 'phi ~ gamma(0.01, 0.01);\n',
                                                 '{')
    model_file[grep('matrix[n, n_series] mus;',
                    model_file,
                    fixed = TRUE)] <- paste0('matrix[n, n_series] phi_vec;\n',
                                             'matrix[n, n_series] mus;')
    model_file[grep('// posterior predictions',
                    model_file, fixed = TRUE)] <- paste0('// posterior predictions\n',
                                                         ifelse(offset, 'eta = X * b + offset;\n',
                                                                'eta = X * b;\n'),
                                                         'for (s in 1:n_series) {\n',
                                                         'phi_vec[1:n,s] = rep_vector(phi[s], n);\n',
                                                         '}\n',
                                                         'for(s in 1:n_series){ \n',
                                                         'mus[1:n, s] = eta[ytimes[1:n, s]] + trend[1:n, s];\n',
                                                         'ypred[1:n, s] = beta_rng(inv_logit(mus[1:n, s]) .* phi_vec[1:n, s], (1 - inv_logit(mus[1:n, s])) .* phi_vec[1:n, s]);\n',
                                                         '}')
  }

  if(family == 'Gamma'){
    model_file[grep('array[n, n_series] int ypred;',
                    model_file, fixed = TRUE)] <- 'real<lower=0> ypred[n, n_series];'
    model_file = readLines(textConnection(model_file), n = -1)
    model_file[grep('vector[num_basis] b_raw;',
                    model_file,
                    fixed = TRUE)] <- paste0('vector[num_basis] b_raw;\n',
                                             '// Gamma shape parameters\n',
                                             'vector<lower=0>[n_series] shape;')
    model_file[grep('// likelihood functions',
                    model_file,
                    fixed = TRUE) - 1] <- paste0('// priors for shape parameters\n',
                                                 'shape ~ gamma(0.01, 0.01);\n',
                                                 '{')
    model_file[grep('matrix[n, n_series] mus;',
                    model_file,
                    fixed = TRUE)] <- paste0('matrix[n, n_series] shape_vec;\n',
                                             'matrix[n, n_series] mus;')
    model_file[grep('// posterior predictions',
                    model_file, fixed = TRUE)] <- paste0('// posterior predictions\n',
                                                         ifelse(offset, 'eta = X * b + offset;\n',
                                                                'eta = X * b;\n'),
                                                         'for (s in 1:n_series) {\n',
                                                         'shape_vec[1:n,s] = rep_vector(shape[s], n);\n',
                                                         '}\n',
                                                         'for(s in 1:n_series){ \n',
                                                         'mus[1:n, s] = eta[ytimes[1:n, s]] + trend[1:n, s];\n',
                                                         'ypred[1:n, s] = gamma_rng(shape_vec[1:n, s], shape_vec[1:n, s] ./ exp(mus[1:n, s]));\n',
                                                         '}')
  }

  #### Trend modifications ####
  # Vectorise trend models
  if(trend_model == 'RW'){
    if(any(grepl('// dynamic factor estimates', model_file, fixed = TRUE))){
      init_trend_line <- grep('LV_raw[1, j] ~ normal(0, 0.1)',
                              model_file, fixed = TRUE) - 1
      model_file <- model_file[-c(init_trend_line:(init_trend_line + 2))]
      model_file[init_trend_line] <-
        'LV_raw[1, 1:n_lv] ~ normal(0, 0.1);'

      remainder_line <- grep('LV_raw[2:n, j] ~ normal(LV_raw[1:(n - 1), j], 0.1)',
                             model_file, fixed = TRUE) - 1
      model_file <- model_file[-c(remainder_line:(remainder_line + 2))]
      model_file[remainder_line] <-
        paste0('for(j in 1:n_lv){\n',
               'LV_raw[2:n, j] ~ normal(LV_raw[1:(n - 1), j], 0.1);\n',
               '}')
      model_file = readLines(textConnection(model_file), n = -1)
    } else {

      if(drift){
        init_trend_line <- grep('trend[1, s] ~ normal(0, sigma[s])',
                                model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(init_trend_line:(init_trend_line + 2))]
        model_file[init_trend_line] <-
          'trend[1, 1:n_series] ~ normal(0, sigma);'

        remainder_line <- grep('trend[2:n, s] ~ normal(drift[s] + trend[1:(n - 1), s], sigma[s])',
                               model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(remainder_line:(remainder_line + 2))]
        model_file[remainder_line] <-
          paste0('for(s in 1:n_series){\n',
                 'trend[2:n, s] ~ normal(drift[s] + trend[1:(n - 1), s], sigma[s]);\n',
                 '}')
        model_file = readLines(textConnection(model_file), n = -1)
      } else {
        init_trend_line <- grep('trend[1, s] ~ normal(0, sigma[s])',
                                model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(init_trend_line:(init_trend_line + 2))]
        model_file[init_trend_line] <-
          'trend[1, 1:n_series] ~ normal(0, sigma);'

        remainder_line <- grep('trend[2:n, s] ~ normal(trend[1:(n - 1), s], sigma[s])',
                               model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(remainder_line:(remainder_line + 2))]
        model_file[remainder_line] <-
          paste0('for(s in 1:n_series){\n',
                 'trend[2:n, s] ~ normal(trend[1:(n - 1), s], sigma[s]);\n',
                 '}')
        model_file = readLines(textConnection(model_file), n = -1)
      }

    }

  }

  if(trend_model == 'CAR1'){
    if(any(grepl('// dynamic factor estimates', model_file, fixed = TRUE))){
      init_trend_line <- grep('LV_raw[1, j] ~ normal(0, 0.1)',
                              model_file, fixed = TRUE) - 1
      model_file <- model_file[-c(init_trend_line:(init_trend_line + 2))]
      model_file[init_trend_line] <-
        'LV_raw[1, 1:n_lv] ~ normal(0, 0.1);'

      remainder_line <- grep('LV_raw[2:n, j] ~ normal(ar1[j] * LV_raw[1:(n - 1), j], 0.1)',
                             model_file, fixed = TRUE) - 1
      model_file <- model_file[-c(remainder_line:(remainder_line + 2))]
      model_file[remainder_line] <-
        paste0('for(j in 1:n_lv){\n',
               'LV_raw[2:n, j] ~ normal(pow(ar1[j], to_vector(time_dis[2:n, j])) .* LV_raw[1:(n - 1), j], 0.1);\n',
               '}')
      model_file = readLines(textConnection(model_file), n = -1)
    } else {
      if(drift){
        init_trend_line <- grep('trend[1, s] ~ normal(0, sigma[s])',
                                model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(init_trend_line:(init_trend_line + 2))]
        model_file[init_trend_line] <-
          'trend[1, 1:n_series] ~ normal(0, sigma);'

        remainder_line <- grep('trend[2:n, s] ~ normal(drift[s] + ar1[s] * trend[1:(n - 1), s], sigma[s])',
                               model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(remainder_line:(remainder_line + 2))]
        model_file[remainder_line] <-
          paste0('for(s in 1:n_series){\n',
                 'trend[2:n, s] ~ normal(drift[s] + pow(ar1[s], to_vector(time_dis[2:n, s])) .* trend[1:(n - 1), s], sigma[s]);\n',
                 '}')
        model_file = readLines(textConnection(model_file), n = -1)
      } else {
        init_trend_line <- grep('trend[1, s] ~ normal(0, sigma[s])',
                                model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(init_trend_line:(init_trend_line + 2))]
        model_file[init_trend_line] <-
          'trend[1, 1:n_series] ~ normal(0, sigma);'

        remainder_line <- grep('trend[2:n, s] ~ normal(ar1[s] * trend[1:(n - 1), s], sigma[s])',
                               model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(remainder_line:(remainder_line + 2))]
        model_file[remainder_line] <-
          paste0('for(s in 1:n_series){\n',
                 'trend[2:n, s] ~ normal(pow(ar1[s], to_vector(time_dis[2:n, s])) .* trend[1:(n - 1), s], sigma[s]);\n',
                 '}')
        model_file = readLines(textConnection(model_file), n = -1)
      }
    }
  }

  if(trend_model == 'AR1'){
    if(any(grepl('// dynamic factor estimates', model_file, fixed = TRUE))){
      init_trend_line <- grep('LV_raw[1, j] ~ normal(0, 0.1)',
                              model_file, fixed = TRUE) - 1
      model_file <- model_file[-c(init_trend_line:(init_trend_line + 2))]
      model_file[init_trend_line] <-
        'LV_raw[1, 1:n_lv] ~ normal(0, 0.1);'

      remainder_line <- grep('LV_raw[2:n, j] ~ normal(ar1[j] * LV_raw[1:(n - 1), j], 0.1)',
                             model_file, fixed = TRUE) - 1
      model_file <- model_file[-c(remainder_line:(remainder_line + 2))]
      model_file[remainder_line] <-
        paste0('for(j in 1:n_lv){\n',
               'LV_raw[2:n, j] ~ normal(ar1[j] * LV_raw[1:(n - 1), j], 0.1);\n',
               '}')
      model_file = readLines(textConnection(model_file), n = -1)
    } else {
      if(drift){
        init_trend_line <- grep('trend[1, s] ~ normal(0, sigma[s])',
                                model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(init_trend_line:(init_trend_line + 2))]
        model_file[init_trend_line] <-
          'trend[1, 1:n_series] ~ normal(0, sigma);'

        remainder_line <- grep('trend[2:n, s] ~ normal(drift[s] + ar1[s] * trend[1:(n - 1), s], sigma[s])',
                               model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(remainder_line:(remainder_line + 2))]
        model_file[remainder_line] <-
          paste0('for(s in 1:n_series){\n',
                 'trend[2:n, s] ~ normal(drift[s] + ar1[s] * trend[1:(n - 1), s], sigma[s]);\n',
                 '}')
        model_file = readLines(textConnection(model_file), n = -1)
      } else {
        init_trend_line <- grep('trend[1, s] ~ normal(0, sigma[s])',
                                model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(init_trend_line:(init_trend_line + 2))]
        model_file[init_trend_line] <-
          'trend[1, 1:n_series] ~ normal(0, sigma);'

        remainder_line <- grep('trend[2:n, s] ~ normal(ar1[s] * trend[1:(n - 1), s], sigma[s])',
                               model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(remainder_line:(remainder_line + 2))]
        model_file[remainder_line] <-
          paste0('for(s in 1:n_series){\n',
                 'trend[2:n, s] ~ normal(ar1[s] * trend[1:(n - 1), s], sigma[s]);\n',
                 '}')
        model_file = readLines(textConnection(model_file), n = -1)
      }
    }
  }

  if(trend_model == 'AR2'){
    if(any(grepl('// dynamic factor estimates', model_file, fixed = TRUE))){
      init_trend_line <- grep('LV_raw[1, j] ~ normal(0, 0.1)',
                              model_file, fixed = TRUE) - 1
      model_file <- model_file[-c(init_trend_line:(init_trend_line + 2))]
      model_file[init_trend_line] <-
        'LV_raw[1, 1:n_lv] ~ normal(0, 0.1);'

      second_line <- grep('LV_raw[2, j] ~ normal(LV_raw[1, j] * ar1[j], 0.1)',
                          model_file, fixed = TRUE) - 1
      model_file <- model_file[-c(second_line:(second_line + 2))]
      model_file[second_line] <-
        'LV_raw[2, 1:n_lv] ~ normal(LV_raw[1, 1:n_lv] * ar1, 0.1);'

      remainder_line <- grep('LV_raw[i, j] ~ normal(ar1[j] * LV_raw[i - 1, j] + ar2[j] * LV_raw[i - 2, j]',
                             model_file, fixed = TRUE) - 2
      model_file <- model_file[-c(remainder_line:(remainder_line + 3))]
      model_file[remainder_line] <-
        paste0('for(j in 1:n_lv){\n',
               'LV_raw[3:n, j] ~ normal(ar1[j] * LV_raw[2:(n - 1), j] + ar2[j] * LV_raw[1:(n - 2), j], 0.1);\n',
               '}')
      model_file = readLines(textConnection(model_file), n = -1)
    } else {
      if(drift){
        init_trend_line <- grep('trend[1, s] ~ normal(0, sigma[s])',
                                model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(init_trend_line:(init_trend_line + 2))]
        model_file[init_trend_line] <-
          'trend[1, 1:n_series] ~ normal(0, sigma);'

        second_line <- grep('trend[2, s] ~ normal(drift[s] + trend[1, s] * ar1[s], sigma[s])',
                            model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(second_line:(second_line + 2))]
        model_file[second_line] <-
          'trend[2, 1:n_series] ~ normal(drift + trend[1, 1:n_series] * ar1, sigma);'

        remainder_line <- grep('trend[i, s] ~ normal(drift[s] + ar1[s] * trend[i - 1, s] + ar2[s] * trend[i - 2, s]',
                               model_file, fixed = TRUE) - 2
        model_file <- model_file[-c(remainder_line:(remainder_line + 3))]
        model_file[remainder_line] <-
          paste0('for(s in 1:n_series){\n',
                 'trend[3:n, s] ~ normal(drift[s] + ar1[s] * trend[2:(n - 1), s] + ar2[s] * trend[1:(n - 2), s], sigma[s]);\n',
                 '}')
        model_file = readLines(textConnection(model_file), n = -1)
      } else {
        init_trend_line <- grep('trend[1, s] ~ normal(0, sigma[s])',
                                model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(init_trend_line:(init_trend_line + 2))]
        model_file[init_trend_line] <-
          'trend[1, 1:n_series] ~ normal(0, sigma);'

        second_line <- grep('trend[2, s] ~ normal(trend[1, s] * ar1[s], sigma[s])',
                            model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(second_line:(second_line + 2))]
        model_file[second_line] <-
          'trend[2, 1:n_series] ~ normal(trend[1, 1:n_series] * ar1, sigma);'

        remainder_line <- grep('trend[i, s] ~ normal(ar1[s] * trend[i - 1, s] + ar2[s] * trend[i - 2, s]',
                               model_file, fixed = TRUE) - 2
        model_file <- model_file[-c(remainder_line:(remainder_line + 3))]
        model_file[remainder_line] <-
          paste0('for(s in 1:n_series){\n',
                 'trend[3:n, s] ~ normal(ar1[s] * trend[2:(n - 1), s] + ar2[s] * trend[1:(n - 2), s], sigma[s]);\n',
                 '}')
        model_file = readLines(textConnection(model_file), n = -1)
      }

    }
  }

  if(trend_model == 'AR3'){
    if(any(grepl('// dynamic factor estimates', model_file, fixed = TRUE))){
      init_trend_line <- grep('LV_raw[1, j] ~ normal(0, 0.1)',
                              model_file, fixed = TRUE) - 1
      model_file <- model_file[-c(init_trend_line:(init_trend_line + 2))]
      model_file[init_trend_line] <-
        'LV_raw[1, 1:n_lv] ~ normal(0, 0.1);'

      second_line <- grep('LV_raw[2, j] ~ normal(LV_raw[1, j] * ar1[j], 0.1)',
                          model_file, fixed = TRUE) - 1
      model_file <- model_file[-c(second_line:(second_line + 2))]
      model_file[second_line] <-
        'LV_raw[2, 1:n_lv] ~ normal(LV_raw[1, 1:n_lv] * ar1, 0.1);'

      third_line <- grep('LV_raw[3, j] ~ normal(LV_raw[2, j] * ar1[j] + LV_raw[1, j] * ar2[j]',
                         model_file, fixed = TRUE) - 1
      model_file <- model_file[-c(third_line:(third_line + 2))]
      model_file[third_line] <-
        'LV_raw[3, 1:n_lv] ~ normal(LV_raw[2, 1:n_lv] * ar1 + LV_raw[1, 1:n_lv] * ar2, 0.1);'

      remainder_line <- grep('LV_raw[i, j] ~ normal(ar1[j] * LV_raw[i - 1, j] + ar2[j] * LV_raw[i - 2, j] + ar3[j] * LV_raw[i - 3, j]',
                             model_file, fixed = TRUE) - 2
      model_file <- model_file[-c(remainder_line:(remainder_line + 3))]
      model_file[remainder_line] <-
        paste0('for(j in 1:n_lv){\n',
               'LV_raw[4:n, j] ~ normal(ar1[j] * LV_raw[3:(n - 1), j] + ar2[j] * LV_raw[2:(n - 2), j] + ar3[j] * LV_raw[1:(n - 3), j], 0.1);\n',
               '}')
      model_file = readLines(textConnection(model_file), n = -1)
    } else {
      if(drift){
        init_trend_line <- grep('trend[1, s] ~ normal(0, sigma[s])',
                                model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(init_trend_line:(init_trend_line + 2))]
        model_file[init_trend_line] <-
          'trend[1, 1:n_series] ~ normal(0, sigma);'

        second_line <- grep('trend[2, s] ~ normal(drift[s] + trend[1, s] * ar1[s], sigma[s])',
                            model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(second_line:(second_line + 2))]
        model_file[second_line] <-
          'trend[2, 1:n_series] ~ normal(drift + trend[1, 1:n_series] * ar1, sigma);'

        third_line <- grep('trend[3, s] ~ normal(drift[s] + trend[2, s] * ar1[s] + trend[1, s] * ar2[s]',
                           model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(third_line:(third_line + 2))]
        model_file[third_line] <-
          'trend[3, 1:n_series] ~ normal(drift + trend[2, 1:n_series] * ar1 + trend[1, 1:n_series] * ar2, sigma);'

        remainder_line <- grep('trend[i, s] ~ normal(drift[s] + ar1[s] * trend[i - 1, s] + ar2[s] * trend[i - 2, s] + ar3[s] * trend[i - 3, s]',
                               model_file, fixed = TRUE) - 2
        model_file <- model_file[-c(remainder_line:(remainder_line + 3))]
        model_file[remainder_line] <-
          paste0('for(s in 1:n_series){\n',
                 'trend[4:n, s] ~ normal(drift[s] + ar1[s] * trend[3:(n - 1), s] + ar2[s] * trend[2:(n - 2), s] + ar3[s] * trend[1:(n - 3), s], sigma[s]);\n',
                 '}')
        model_file = readLines(textConnection(model_file), n = -1)
      } else {
        init_trend_line <- grep('trend[1, s] ~ normal(0, sigma[s])',
                                model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(init_trend_line:(init_trend_line + 2))]
        model_file[init_trend_line] <-
          'trend[1, 1:n_series] ~ normal(0, sigma);'

        second_line <- grep('trend[2, s] ~ normal(trend[1, s] * ar1[s], sigma[s])',
                            model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(second_line:(second_line + 2))]
        model_file[second_line] <-
          'trend[2, 1:n_series] ~ normal(trend[1, 1:n_series] * ar1, sigma);'

        third_line <- grep('trend[3, s] ~ normal(trend[2, s] * ar1[s] + trend[1, s] * ar2[s]',
                           model_file, fixed = TRUE) - 1
        model_file <- model_file[-c(third_line:(third_line + 2))]
        model_file[third_line] <-
          'trend[3, 1:n_series] ~ normal(trend[2, 1:n_series] * ar1 + trend[1, 1:n_series] * ar2, sigma);'

        remainder_line <- grep('trend[i, s] ~ normal(ar1[s] * trend[i - 1, s] + ar2[s] * trend[i - 2, s] + ar3[s] * trend[i - 3, s]',
                               model_file, fixed = TRUE) - 2
        model_file <- model_file[-c(remainder_line:(remainder_line + 3))]
        model_file[remainder_line] <-
          paste0('for(s in 1:n_series){\n',
                 'trend[4:n, s] ~ normal(ar1[s] * trend[3:(n - 1), s] + ar2[s] * trend[2:(n - 2), s] + ar3[s] * trend[1:(n - 3), s], sigma[s]);\n',
                 '}')
        model_file = readLines(textConnection(model_file), n = -1)
      }

    }
  }

  # Clean to remove trend components if this is a 'None' trend model
  if(trend_model == 'None'){
    model_file = readLines(textConnection(model_file), n = -1)
    model_file <- gsub(' + trend[1:n, s]', '', model_file, fixed = TRUE)
    model_file <- gsub('exp(append_col(flat_xs, flat_trends)',
                       'exp(flat_xs', model_file, fixed = TRUE)
    model_file <- gsub('append_col(flat_xs, flat_trends)',
                       'flat_xs', model_file, fixed = TRUE)
    model_file <- gsub('append_row(b, 1.0)', 'b', model_file, fixed = TRUE)
    model_file <- model_file[-grep('vector[n_nonmissing] flat_trends;',
                                   model_file, fixed = TRUE)]
    model_file <- model_file[-grep('flat_trends = (to_vector(trend))[obs_ind];',
                                   model_file, fixed = TRUE)]
  }

  # New additions for VAR1 models
  if(VAR1){
    model_file <- model_file[-grep('vector[n_series] tau;', model_file, fixed = TRUE)]
    model_file[grep('// latent trends', model_file, fixed = TRUE)] <-
      '// raw latent trends'
    model_file[grep('matrix[n, n_series] trend;', model_file, fixed = TRUE)] <-
      'vector[n_series] trend_raw[n];'
    model_file[grep('// latent trend variance parameters', model_file, fixed = TRUE) - 1] <-
      paste0('\n// latent trend VAR1 terms\n',
             'matrix<lower=-1,upper=1>[n_series, n_series] A;\n')
    model_file = readLines(textConnection(model_file), n = -1)
    model_file[grep('vector[num_basis] b;', model_file, fixed = TRUE)] <-
      paste0('vector[num_basis] b;',
             '\n// trend estimates in matrix-form\n',
             'matrix[n, n_series] trend;\n',
             '\nfor(i in 1:n){\n',
             'trend[i, 1:n_series] = to_row_vector(trend_raw[i]);\n',
             '}\n')
    model_file = readLines(textConnection(model_file), n = -1)
    model_file[grep('model {', model_file, fixed = TRUE)] <-
      paste0('model {\n',
             '// latent trend mean parameters\n',
             'vector[n_series] mu[n - 1];\n')
    model_file[grep('sigma ~ exponential(2);', model_file, fixed = TRUE)] <-
      paste0('sigma ~ inv_gamma(2.3693353, 0.7311319);\n\n',
             '// VAR coefficients\n',
             'to_vector(A) ~ normal(0, 0.5);\n\n',
             '// trend means\n',
             'for(i in 2:n){\n',
             'mu[i - 1] = A * trend_raw[i - 1];\n',
             '}\n\n',
             '// stochastic latent trends (contemporaneously uncorrelated)\n',
             'trend_raw[1] ~ normal(0, sigma);\n',
             'for(i in 2:n){\n',
             'trend_raw[i] ~ normal(mu[i - 1], sigma);\n',
             '}\n')
    model_file = readLines(textConnection(model_file), n = -1)
    model_file <- model_file[-c((grep("trend[1, 1:n_series] ~ normal(0, sigma);", model_file, fixed = TRUE) - 2):
                                  (grep("trend[1, 1:n_series] ~ normal(0, sigma);" , model_file, fixed = TRUE) + 3))]
    model_file[grep("generated quantities {" , model_file, fixed = TRUE)] <-
      paste0('generated quantities {\n',
             'matrix[n_series, n_series] Sigma;')
    model_file = readLines(textConnection(model_file), n = -1)
    model_file <- model_file[-c((grep("tau[s] = pow(sigma[s], -2.0);", model_file, fixed = TRUE) - 1):
                                  (grep("tau[s] = pow(sigma[s], -2.0);" , model_file, fixed = TRUE) + 1))]
    model_file[grep("// posterior predictions" , model_file, fixed = TRUE) - 1] <-
      paste0('Sigma = diag_matrix(square(sigma));\n')
    model_file = readLines(textConnection(model_file), n = -1)
  }

  # Add time_dis array for tracking length between observations for
  # continuous time AR models
  if(trend_model == 'CAR1'){
    model_file[grep('int<lower=0> ytimes[n, n_series]; //',
                    model_file, fixed = TRUE)] <-
      paste0('int<lower=0> ytimes[n, n_series]; // time-ordered matrix (which col in X belongs to each [time, series] observation?)\n',
             'array[n, n_series] real<lower=0> time_dis; // temporal distances between observations')
    model_file = readLines(textConnection(model_file), n = -1)
    }

  # Change variable 'offset' to 'off_set' to avoid any issues with later
  # versions of cmdstan
  if(any(grepl('offset', model_file, fixed = TRUE))){
    model_file <- gsub('offset', 'off_set', model_file)
    model_file <- gsub('off_set vector', 'offset vector', model_file)
    model_data$off_set <- model_data$offset
    model_data$offset <- NULL
  }

  # Tidying the representation
  if(any(grepl('functions {', model_file, fixed = TRUE))){
    model_file <- model_file[-(grep('// Stan model code generated by package mvgam',
                                    model_file, fixed = TRUE))]
    model_file[grep('functions {', model_file, fixed = TRUE)] <-
      paste0('// Stan model code generated by package mvgam\n',
             'functions {')
  }

  return(list(model_file = readLines(textConnection(model_file), n = -1),
              model_data = model_data))
}

#### Modifications to Stan code for setting up trend mapping ####
#' @noRd
trend_map_mods = function(model_file,
                          model_data,
                          trend_map,
                          trend_model,
                          n_lv,
                          data_train,
                          ytimes){

if(trend_model != 'VAR1'){
  # Model code should be modified to remove any priors and modelling for the
  # latent variable coefficients and sign corrections
  model_file <- model_file[-c(grep('// dynamic factor lower triangle loading coefficients',
                                   model_file, fixed = TRUE):(grep('// dynamic factor lower triangle loading coefficients',
                                                                   model_file, fixed = TRUE) + 2))]
  model_file <- model_file[-c(grep('// Number of non-zero lower triangular factor loadings',
                                   model_file, fixed = TRUE):
                                (grep('// Number of non-zero lower triangular factor loadings',
                                      model_file, fixed = TRUE) + 3))]
  model_file <- model_file[-c(grep('// constraints allow identifiability of loadings',
                                   model_file, fixed = TRUE):
                                (grep('// constraints allow identifiability of loadings',
                                      model_file, fixed = TRUE) + 15))]
  model_file <- model_file[-grep('matrix[n_series, n_lv] lv_coefs_raw;',
                                 model_file, fixed = TRUE)]
  model_file <- model_file[-grep('matrix[n_series, n_lv] lv_coefs;',
                                 model_file, fixed = TRUE)]
  model_file <- model_file[-c(grep('// priors for dynamic factor loading coefficients',
                                   model_file, fixed = TRUE):
                                (grep('// priors for dynamic factor loading coefficients',
                                      model_file, fixed = TRUE) + 2))]
  model_file <- model_file[-c(grep('// Sign correct factor loadings and factors',
                                   model_file, fixed = TRUE):
                                (grep('// Sign correct factor loadings and factors',
                                      model_file, fixed = TRUE) + 9))]
  model_file <- model_file[-grep('matrix[n, n_lv] LV;',
                                 model_file, fixed = TRUE)]
  model_file <- gsub('LV_raw', 'LV', model_file)
  model_file <- gsub('lv_coefs_raw', 'lv_coefs', model_file)
  model_file[grep("matrix[n, n_series] trend;",
                  model_file, fixed = TRUE)] <-
    paste0('matrix[n, n_series] trend;\n',
           'matrix[n_series, n_lv] lv_coefs;')
  model_file[grep("// derived latent trends",
                  model_file, fixed = TRUE)] <-
    paste0('// derived latent trends\n',
           'lv_coefs = Z;')
  model_file <- readLines(textConnection(model_file), n = -1)

  # We can estimate the variance parameters if a trend map is supplied
  if(trend_model %in% c('RW', 'AR1', 'AR2', 'AR3', 'CAR1')){
    model_file <- model_file[-grep('vector[num_basis] b_raw;',
                                   model_file, fixed = TRUE)]
    model_file[grep("// raw basis coefficients",
                    model_file, fixed = TRUE)] <-
      paste0('// raw basis coefficients\n',
             'vector[num_basis] b_raw;\n\n',
             '// latent factor SD terms\n',
             'vector<lower=0>[n_lv] sigma;')

    model_file[grep("// dynamic factor estimates",
                    model_file, fixed = TRUE)] <-
      paste0('// priors for factor SD parameters\n',
             'sigma ~ exponential(2);\n',
             '// dynamic factor estimates')

    model_file[grep("penalty = rep_vector(100.0, n_lv);",
                    model_file, fixed = TRUE)] <-
      "penalty = 1.0 / (sigma .* sigma);"

    model_file[grep("LV[1, 1:n_lv] ~ normal(0, 0.1);",
                    model_file, fixed = TRUE)] <-
      'LV[1, 1:n_lv] ~ normal(0, sigma);'

    model_file <- readLines(textConnection(model_file), n = -1)
    model_file <- gsub('j], 0.1', 'j], sigma[j]', model_file)
  }

}
  if(trend_model == 'VAR1'){
    model_file[grep("// raw latent trends",
                    model_file, fixed = TRUE)] <-
      "// dynamic factors"
    model_file[grep("vector[n_series] trend_raw[n];",
                    model_file, fixed = TRUE)] <-
      "vector[n_lv] LV[n];"

    model_file[grep("// trend estimates in matrix-form",
                    model_file, fixed = TRUE)] <-
      "// trends and dynamic factor loading matrix"
    model_file[grep("matrix[n, n_series] trend;",
                    model_file, fixed = TRUE)] <-
      paste0("matrix[n, n_series] trend;\n",
             "matrix[n_series, n_lv] lv_coefs;")
    model_file <- readLines(textConnection(model_file), n = -1)

    model_file <- model_file[-c((grep("trend[i, 1:n_series] = to_row_vector(trend_raw[i]);",
                                     model_file, fixed = TRUE) - 1):
                                  (grep("trend[i, 1:n_series] = to_row_vector(trend_raw[i]);",
                                        model_file, fixed = TRUE) + 1))]

    model_file[grep("matrix[n_series, n_lv] lv_coefs;",
                    model_file, fixed = TRUE)] <-
      paste0("matrix[n_series, n_lv] lv_coefs;\n",
             "// derived latent trends\n",
             "lv_coefs = Z;\n",
             "for (i in 1:n){\n",
             "for (s in 1:n_series){\n",
             "trend[i, s] = dot_product(lv_coefs[s,], LV[i]);\n",
             "}\n",
             "}\n")
    model_file <- readLines(textConnection(model_file), n = -1)

    model_file <- gsub('trend_raw', 'LV', model_file)

    model_file[grep("vector<lower=0>[n_series] sigma;",
                    model_file, fixed = TRUE)] <-
      "vector<lower=0>[n_lv] sigma;"

    model_file[grep("matrix[n_series, n_series] P_real;",
                    model_file, fixed = TRUE)] <-
      "matrix[n_lv, n_lv] P_real;"

    model_file[grep("matrix[n_series, n_series] A;",
                    model_file, fixed = TRUE)] <-
      "matrix[n_lv, n_lv] A;"

    model_file[grep("vector[n_series] mu[n - 1];",
                    model_file, fixed = TRUE)] <-
      "vector[n_lv] mu[n];"

    model_file[grep("array[n] vector[n_series] mu;",
                    model_file, fixed = TRUE)] <-
      "array[n] vector[n_lv] mu;"

    model_file[grep("matrix[n_series, n_series] Sigma;",
                    model_file, fixed = TRUE)] <-
      "matrix[n_lv, n_lv] Sigma;"

    model_file[grep("matrix[n_series, n_series] P[1];" ,
                    model_file, fixed = TRUE)] <-
      "matrix[n_lv, n_lv] P[1];"

    model_file[grep("matrix[n_series, n_series] phiGamma[2, 1];" ,
                    model_file, fixed = TRUE)] <-
      "matrix[n_lv, n_lv] phiGamma[2, 1];"

    model_file[grep("diagonal(P_real) ~ normal(Pmu[1], 1 / sqrt(Pomega[1]));" ,
                    model_file, fixed = TRUE)+1] <-
      "for(i in 1:n_lv) {"

    model_file[grep("diagonal(P_real) ~ normal(Pmu[1], 1 / sqrt(Pomega[1]));" ,
                    model_file, fixed = TRUE)+2] <-
      "for(j in 1:n_lv) {"

    model_file[grep("int<lower=0> n; // number of timepoints per series",
                    model_file, fixed = TRUE)] <-
    paste0("int<lower=0> n; // number of timepoints per series\n",
           "int<lower=0> n_lv; // number of dynamic factors")
    model_file <- readLines(textConnection(model_file), n = -1)

    if(any(grepl("matrix[n_series, n_series] L_Sigma;",
                 model_file, fixed = TRUE))){
      model_file[grep("matrix[n_series, n_series] L_Sigma;" ,
                      model_file, fixed = TRUE)] <-
        "matrix[n_lv, n_lv] L_Sigma;"

      model_file[grep("cov_matrix[n_series] Sigma;" ,
                      model_file, fixed = TRUE)] <-
        "cov_matrix[n_lv] Sigma;"

      model_file[grep("cov_matrix[n_series] Gamma;" ,
                      model_file, fixed = TRUE)] <-
        "cov_matrix[n_lv] Gamma;"

      model_file[grep("cholesky_factor_corr[n_series] L_Omega;" ,
                      model_file, fixed = TRUE)] <-
        "cholesky_factor_corr[n_lv] L_Omega;"

      model_file[grep("vector[n_series] trend_zeros = rep_vector(0.0, n_series);" ,
                      model_file, fixed = TRUE)] <-
        "vector[n_lv] trend_zeros = rep_vector(0.0, n_lv);"
      model_file <- readLines(textConnection(model_file), n = -1)

    }
  }

  # Need to formulate the lv_coefs matrix and
  # supply it as data
  model_file[grep("int<lower=0> n_series; // number of series",
                  model_file, fixed = TRUE)] <-
    paste0("int<lower=0> n_series; // number of series\n",
           "matrix[n_series, n_lv] Z; // matrix mapping series to latent trends")
  model_file <- readLines(textConnection(model_file), n = -1)

  # Z <- matrix(0, NCOL(ytimes), n_lv)
  # for(i in 1:NROW(trend_map)){
  #   Z[as.numeric(data_train$series)[trend_map$series[i]],
  #            trend_map$trend[i]] <- 1
  # }

  Z <- matrix(0, NCOL(ytimes), n_lv)
  for(i in 1:NROW(trend_map)){
    rowid <- which(levels(data_train$series) == trend_map$series[i])
    Z[rowid, trend_map$trend[i]] <- 1
  }


  model_data$Z <- Z
  return(list(model_file = model_file,
              model_data = model_data))
}

#### Modifications to Stan code for adding predictors to trend models ####
#' @noRd
add_trend_predictors = function(trend_formula,
                                trend_knots,
                                trend_map,
                                trend_model,
                                data_train,
                                data_test,
                                model_file,
                                model_data,
                                nmix = FALSE,
                                drift = FALSE){

  #### Creating the trend mvgam model file and data structures ####
  # Replace any terms labelled 'trend' with 'series' for creating the necessary
  # structures
  trend_formula <- formula(paste(gsub('trend', 'series',
                                      as.character(trend_formula),
                                      fixed = TRUE),
                collapse = " "))

  if(missing(trend_knots)){
    trend_knots <- rlang::missing_arg()
  }

  # Drop any intercept from the formula if this is not an N-mixture model
  # as the intercept will almost surely be unidentifiable
  if(!nmix){
    if(attr(terms(trend_formula), 'intercept') == 1){
      trend_formula <- update(trend_formula, trend_y  ~ . -1)
    } else {
      trend_formula <- update(trend_formula, trend_y  ~ .)
    }
  } else {
    trend_formula <- update(trend_formula, trend_y  ~ .)
  }

  trend_train <- data_train
  trend_train$time <- trend_train$index..time..index
  trend_train$trend_y <- rnorm(length(trend_train$time))

  # Add indicators of trend names as factor levels using the trend_map
  trend_indicators <- vector(length = length(trend_train$time))
  for(i in 1:length(trend_train$time)){
    trend_indicators[i] <- trend_map$trend[which(trend_map$series ==
                                                   trend_train$series[i])]
  }
  trend_indicators <- factor(paste0('trend', trend_indicators),
                             levels = paste0('trend', 1:max(trend_map$trend)))
  trend_train$series <- trend_indicators
  trend_train$y <- NULL

  # Only keep one time observation per trend
  data.frame(series = trend_train$series,
             time = trend_train$time,
             row_num = 1:length(trend_train$time)) %>%
    dplyr::group_by(series, time) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::pull(row_num) -> inds_keep

  if(inherits(trend_train, 'list')){
    trend_train <- lapply(trend_train, function(x){
      if(is.matrix(x)){
        matrix(x[inds_keep,], ncol = NCOL(x))
      } else {
        x[inds_keep]
      }

    })
  } else {
    trend_train <- trend_train[inds_keep, ]
  }

  if(!is.null(data_test)){
    # If newdata supplied, also create a fake design matrix
    # for the test data
    trend_test <- data_test
    trend_test$time <- trend_test$index..time..index
    trend_test$trend_y <- rnorm(length(trend_test$time))
    trend_indicators <- vector(length = length(trend_test$time))
    for(i in 1:length(trend_test$time)){
      trend_indicators[i] <- trend_map$trend[which(trend_map$series == trend_test$series[i])]
    }
    trend_indicators <- as.factor(paste0('trend', trend_indicators))
    trend_test$series <- trend_indicators
    trend_test$y <- NULL

    data.frame(series = trend_test$series,
               time = trend_test$time,
               row_num = 1:length(trend_test$time)) %>%
      dplyr::group_by(series, time) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::pull(row_num) -> inds_keep

    if(inherits(trend_test, 'list')){
      trend_test <- lapply(trend_test, function(x){
        if(is.matrix(x)){
          matrix(x[inds_keep,], ncol = NCOL(x))
        } else {
          x[inds_keep]
        }

      })
    } else {
      trend_test <- trend_test[inds_keep, ]
    }

    # Construct the model file and data structures for testing and training
    trend_mvgam <- mvgam(trend_formula,
                         knots = trend_knots,
                         data = trend_train,
                         newdata = trend_test,
                         family = gaussian(),
                         trend_model = 'None',
                         return_model_data = TRUE,
                         run_model = FALSE,
                         autoformat = FALSE)
  } else {
    # Construct the model file and data structures for training only
    trend_mvgam <- mvgam(trend_formula,
                         knots = trend_knots,
                         data = trend_train,
                         family = gaussian(),
                         trend_model = 'None',
                         return_model_data = TRUE,
                         run_model = FALSE,
                         autoformat = FALSE)
  }

  trend_model_file <- trend_mvgam$model_file

  #### Modifying the model_file and model_data ####
  # Add lines for the raw trend basis coefficients
  model_file[grep("vector[num_basis] b_raw;", model_file, fixed = TRUE)] <-
    paste0("vector[num_basis] b_raw;\n",
           "vector[num_basis_trend] b_raw_trend;")

  # Add lines to data declarations for trend design matrix
  model_file[grep("matrix[total_obs, num_basis] X; // mgcv GAM design matrix",
                  model_file, fixed = TRUE)] <-
    paste0("matrix[total_obs, num_basis] X; // mgcv GAM design matrix\n",
           "matrix[n * n_lv, num_basis_trend] X_trend; // trend model design matrix")

  model_file[grep("int<lower=0> num_basis; // total number of basis coefficients",
                  model_file, fixed = TRUE)] <-
    paste0("int<lower=0> num_basis; // total number of basis coefficients\n",
           "int<lower=0> num_basis_trend; // number of trend basis coefficients")

  model_file[grep("int<lower=0> ytimes[n, n_series]; // time-ordered matrix (which col in X belongs to each [time, series] observation?)",
                  model_file, fixed = TRUE)] <-
    paste0("int<lower=0> ytimes[n, n_series]; // time-ordered matrix (which col in X belongs to each [time, series] observation?)\n",
           "int<lower=0> ytimes_trend[n, n_lv]; // time-ordered matrix for latent trends")

  model_data$ytimes_trend <- trend_mvgam$model_data$ytimes
  model_data$num_basis_trend <- trend_mvgam$model_data$num_basis
  model_data$X_trend <- trend_mvgam$model_data$X

  # Update names to reflect process models rather than latent factors
  model_file[grep("// trends and dynamic factor loading matrix",
                  model_file, fixed = TRUE)] <-
    "// latent states and loading matrix"

  if(trend_model %in% c('RW', 'AR1', 'AR2', 'AR3', 'CAR1')){
    model_file[grep("// latent factor SD terms",
                    model_file, fixed = TRUE)] <-
      "// latent state SD terms"
    model_file[grep("// priors for factor SD parameters",
                    model_file, fixed = TRUE)] <-
      "// priors for latent state SD parameters"
  }

  model_file[grep("// derived latent trends",
                  model_file, fixed = TRUE)] <- "// derived latent states"

  # Add beta_trend lines
  b_trend_lines <- trend_model_file[grep('b[',
                                         trend_model_file, fixed = TRUE)]
  b_trend_lines <- gsub('\\bb\\b', 'b_trend', b_trend_lines)
  b_trend_lines <- gsub('raw', 'raw_trend', b_trend_lines)
  b_trend_lines <- gsub('num_basis', 'num_basis_trend', b_trend_lines)
  b_trend_lines <- gsub('idx', 'trend_idx', b_trend_lines)
  b_trend_lines <- gsub('l_gp', 'l_gp_trend', b_trend_lines)
  b_trend_lines <- gsub('k_gp', 'k_gp_trend', b_trend_lines)
  b_trend_lines <- gsub('alpha_gp', 'alpha_gp_trend', b_trend_lines)
  b_trend_lines <- gsub('rho_gp', 'rho_gp_trend', b_trend_lines)
  b_trend_lines <- gsub('z_gp', 'z_gp_trend', b_trend_lines)
  model_file[grep("// derived latent states", model_file, fixed = TRUE)] <-
    paste0('// process model basis coefficients\n',
           paste(b_trend_lines, collapse = '\n'),
           '\n\n// derived latent states')
  model_file[grep("vector[num_basis] b;", model_file, fixed = TRUE)] <-
    paste0("vector[num_basis] b;\n",
           "vector[num_basis_trend] b_trend;")

  b1_lines <- model_file[min(grep('b[1', model_file, fixed = TRUE))]
  model_file[min(grep('b[1', model_file, fixed = TRUE))] <-
    paste0('// observation model basis coefficients\n',
           b1_lines)

  model_file <- readLines(textConnection(model_file), n = -1)

  trend_smooths_included <- FALSE

  # Add any multinormal smooth lines
  if(any(grepl('multi_normal_prec', trend_model_file)) |
     any(grepl('// priors for smoothing parameters', trend_model_file)) |
     any(grepl('// prior for gp', trend_model_file))){
    trend_smooths_included <- TRUE

    # Replace any indices from trend model so names aren't
    # conflicting with any possible indices in the observation model
    if(any(grepl('idx', trend_model_file))){
      trend_model_file <- gsub('idx', 'trend_idx', trend_model_file)
      idx_data <- trend_mvgam$model_data[grep('idx', names(trend_mvgam$model_data))]
      names(idx_data) <- gsub('idx', 'trend_idx', names(idx_data))
      model_data <- append(model_data, idx_data)

      idx_lines <- c(grep('int trend_idx', trend_model_file),
                     grep('// gp basis coefficient indices', trend_model_file),
                     grep('// monotonic basis coefficient indices', trend_model_file))
      model_file[min(grep('data {', model_file, fixed = TRUE))] <-
        paste0('data {\n',
               paste(trend_model_file[idx_lines],
                     collapse = '\n'))
      model_file <- readLines(textConnection(model_file), n = -1)
    }

    # Check for gp() terms
    if(any(grepl('l_gp', trend_model_file)) &
       any(grepl('k_gp', trend_model_file)) &
       any(grepl('z_gp', trend_model_file))){

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

      trend_model_file <- gsub('l_gp', 'l_gp_trend', trend_model_file)
      trend_model_file <- gsub('k_gp', 'k_gp_trend', trend_model_file)
      trend_model_file <- gsub('alpha_gp', 'alpha_gp_trend', trend_model_file)
      trend_model_file <- gsub('rho_gp', 'rho_gp_trend', trend_model_file)
      trend_model_file <- gsub('z_gp', 'z_gp_trend', trend_model_file)
      idx_data <- trend_mvgam$model_data[grep('l_gp', names(trend_mvgam$model_data))]
      names(idx_data) <- gsub('l_gp', 'l_gp_trend', names(idx_data))
      model_data <- append(model_data, idx_data)

      l_lines <- grep('// approximate gp eigenvalues', trend_model_file, fixed = TRUE)
      model_file[min(grep('data {', model_file, fixed = TRUE))] <-
        paste0('data {\n',
               paste(trend_model_file[l_lines],
                     collapse = '\n'))
      model_file <- readLines(textConnection(model_file), n = -1)
    }

    if(any(grepl('k_gp', trend_model_file))){
      idx_data <- trend_mvgam$model_data[grep('k_gp', names(trend_mvgam$model_data))]
      names(idx_data) <- gsub('k_gp', 'k_gp_trend', names(idx_data))
      model_data <- append(model_data, idx_data)

      k_lines <- grep('// basis functions for approximate gp', trend_model_file, fixed = TRUE)
      model_file[min(grep('data {', model_file, fixed = TRUE))] <-
        paste0('data {\n',
               paste(trend_model_file[k_lines],
                     collapse = '\n'))
      model_file <- readLines(textConnection(model_file), n = -1)

      # Update the parameters block with gp params
      start <- grep("// gp term sd parameters", trend_model_file,
                    fixed = TRUE)
      end <- grep("// gp term latent variables", trend_model_file,
                  fixed = TRUE) + 1
      last <- end
      for(i in end:(end+50)){
        if(grepl('vector[k_gp_trend', trend_model_file[i],
                 fixed = TRUE)){
          last <- i
        } else {
          break
        }
      }
      gp_params <- paste(trend_model_file[start:last],
                         collapse = '\n')

      model_file[min(grep('parameters {', model_file, fixed = TRUE))] <-
        paste0('parameters {\n',
               gp_params)
      model_file <- readLines(textConnection(model_file), n = -1)
    }

    if(any(grepl("int<lower=0> n_sp; // number of smoothing parameters",
                 model_file, fixed = TRUE))){
      model_file[grep("int<lower=0> n_sp; // number of smoothing parameters",
                      model_file, fixed = TRUE)] <-
        paste0("int<lower=0> n_sp; // number of smoothing parameters\n",
               "int<lower=0> n_sp_trend; // number of trend smoothing parameters")
    } else {
      model_file[grep("int<lower=0> n; // number of timepoints per series",
                      model_file, fixed = TRUE)] <-
        paste0("int<lower=0> n; // number of timepoints per series\n",
               "int<lower=0> n_sp_trend; // number of trend smoothing parameters")
    }
    model_data$n_sp_trend <- trend_mvgam$model_data$n_sp

    spline_coef_headers <- trend_model_file[grep('multi_normal_prec',
                                                trend_model_file) - 1]
    if(any(grepl('normal(0, lambda',
                trend_model_file, fixed = TRUE))){
      idx_headers <- trend_model_file[grep('normal(0, lambda',
                            trend_model_file, fixed = TRUE)-1]
      spline_coef_headers <- c(spline_coef_headers,
                               grep('//', idx_headers, value = TRUE))
    }

    if(any(grepl('// prior for gp', trend_model_file))){
      spline_coef_headers <- c(spline_coef_headers,
                               trend_model_file[grep('// prior for gp',
                                                     trend_model_file, fixed = TRUE)])
    }
    spline_coef_headers <- gsub('...', '_trend...', spline_coef_headers,
                                fixed = TRUE)

    spline_coef_lines <- trend_model_file[grepl('multi_normal_prec',
                                                trend_model_file)]
    if(any(grepl('normal(0, lambda',
                 trend_model_file, fixed = TRUE))){
      lambda_normals <- (grep('normal(0, lambda',
                                 trend_model_file, fixed = TRUE))
      for(i in 1:length(lambda_normals)){
        spline_coef_lines <- c(spline_coef_lines,
                               paste(trend_model_file[lambda_normals[i]],
                                     collapse = '\n'))
      }
    }

    if(any(grepl('// prior for gp', trend_model_file))){
      starts <- grep('// prior for gp', trend_model_file, fixed = TRUE) + 1
      ends <- grep('// prior for gp', trend_model_file, fixed = TRUE) + 4
      for(i in seq_along(starts)){
        spline_coef_lines <- c(spline_coef_lines,
                               paste(trend_model_file[starts[i]:ends[i]],
                                     collapse = '\n'))
      }
    }

    spline_coef_lines <- gsub('_raw', '_raw_trend', spline_coef_lines)
    spline_coef_lines <- gsub('lambda', 'lambda_trend', spline_coef_lines)
    spline_coef_lines <- gsub('zero', 'zero_trend', spline_coef_lines)
    spline_coef_lines <- gsub('S', 'S_trend', spline_coef_lines, fixed = TRUE)

    for(i in seq_along(spline_coef_lines)){
      spline_coef_lines[i] <- paste0(spline_coef_headers[i],
                                     '\n',
                                     spline_coef_lines[i])
    }

    lambda_prior_line <- sub('lambda', 'lambda_trend',
                             trend_model_file[grep('lambda ~', trend_model_file, fixed = TRUE)])
    lambda_param_line <- sub('lambda', 'lambda_trend',
                             trend_model_file[grep('vector<lower=0>[n_sp] lambda;',
                                                   trend_model_file, fixed = TRUE)])
    lambda_param_line <- sub('n_sp', 'n_sp_trend', lambda_param_line)

    if(any(grepl('// dynamic process models', model_file, fixed = TRUE))){
      model_file[grep('// dynamic process models', model_file, fixed = TRUE) + 1] <-
        paste0(model_file[grep('// dynamic process models', model_file, fixed = TRUE) + 1],
               '\n',
               paste(spline_coef_lines, collapse = '\n'),
               '\n',
               lambda_prior_line,
               '\n')

    } else {
      if(trend_model != 'VAR1'){
        model_file[grep("// dynamic factor estimates", model_file, fixed = TRUE)] <-
          paste0('// dynamic process models\n',
                 paste(spline_coef_lines, collapse = '\n'),
                 '\n',
                 lambda_prior_line)
      } else {
        model_file[grep('// stochastic latent trends', model_file, fixed = TRUE)] <-
          paste0('// dynamic process models\n',
                 paste(spline_coef_lines, collapse = '\n'),
                 '\n',
                 lambda_prior_line)
      }

    }
    if(any(grepl("vector<lower=0>[n_sp] lambda;", model_file, fixed = TRUE))){
      model_file[grep("// dynamic factors", model_file, fixed = TRUE)] <-
        "// latent states"
      model_file[grep("vector<lower=0>[n_sp] lambda;", model_file, fixed = TRUE)] <-
        paste0("vector<lower=0>[n_sp] lambda;\n",
               "vector<lower=0>[n_sp_trend] lambda_trend;")
    } else {
      if(trend_model != 'VAR1'){
        model_file <- model_file[-grep("matrix[n, n_lv] LV;",
                                       model_file, fixed = TRUE)]
        model_file[grep("// dynamic factors", model_file, fixed = TRUE)] <-
          paste0("// latent states\n",
                 "matrix[n, n_lv] LV;\n\n",
                 "// smoothing parameters\n",
                 "vector<lower=0>[n_sp_trend] lambda_trend;")
      } else {
        model_file <- model_file[-grep("vector[n_lv] LV[n];",
                                       model_file, fixed = TRUE)]
        model_file[grep("// dynamic factors", model_file, fixed = TRUE)] <-
          paste0("// latent states\n",
                 "vector[n_lv] LV[n];\n\n",
                 "// smoothing parameters\n",
                 "vector<lower=0>[n_sp_trend] lambda_trend;")
      }

    }

    if(any(grepl('mgcv smooth penalty matrix',
                 trend_model_file, fixed = TRUE))){
      S_lines <- trend_model_file[grep('mgcv smooth penalty matrix',
                                       trend_model_file, fixed = TRUE)]
      S_lines <- gsub('S', 'S_trend', S_lines, fixed = TRUE)
      model_file[grep("int<lower=0> n_nonmissing; // number of nonmissing observations",
                      model_file, fixed = TRUE)] <-
        paste0("int<lower=0> n_nonmissing; // number of nonmissing observations\n",
               paste(S_lines, collapse = '\n'))

      # Pull out S matrices (don't always start at 1!)
      S_mats <- trend_mvgam$model_data[grepl("S[0-9]",
                                   names(trend_mvgam$model_data))]
      names(S_mats) <- gsub('S', 'S_trend', names(S_mats))
      model_data <- append(model_data, S_mats)
    }

    if(!is.null(trend_mvgam$model_data$zero)){
      model_file[grep("int<lower=0> num_basis_trend; // number of trend basis coefficients",
                      model_file, fixed = TRUE)] <-
        paste0("int<lower=0> num_basis_trend; // number of trend basis coefficients\n",
               "vector[num_basis_trend] zero_trend; // prior locations for trend basis coefficients")
      model_data$zero_trend <- trend_mvgam$model_data$zero
    }

    if(any(grepl("vector[n_sp] rho;", model_file, fixed = TRUE))){
      model_file[grep("vector[n_sp] rho;", model_file, fixed = TRUE)] <-
        paste0("vector[n_sp] rho;\n",
               "vector[n_sp_trend] rho_trend;")

      model_file[grep("rho = log(lambda);", model_file, fixed = TRUE)] <-
        paste0("rho = log(lambda);\n",
               "rho_trend = log(lambda_trend);")
    } else {
      model_file[grep("matrix[n, n_series] mus;", model_file, fixed = TRUE)] <-
        paste0("matrix[n, n_series] mus;\n",
               "vector[n_sp_trend] rho_trend;")

      model_file[grep("// posterior predictions", model_file, fixed = TRUE)] <-
        paste0("rho_trend = log(lambda_trend);\n\n",
               "// posterior predictions")
    }

    model_file <- readLines(textConnection(model_file), n = -1)
  }

  # Add any parametric effect beta lines
  if(length(attr(trend_mvgam$mgcv_model$pterms, 'term.labels')) != 0L ||
     attr(terms(trend_formula), 'intercept') == 1){
    trend_parametrics <- TRUE

    smooth_labs <- do.call(rbind, lapply(seq_along(trend_mvgam$mgcv_model$smooth), function(x){
      data.frame(label = trend_mvgam$mgcv_model$smooth[[x]]$label,
                 term = paste(trend_mvgam$mgcv_model$smooth[[x]]$term, collapse = ','),
                 class = class(trend_mvgam$mgcv_model$smooth[[x]])[1])
    }))
    lpmat <- predict(trend_mvgam$mgcv_model, type = 'lpmatrix',
                     exclude = smooth_labs$label)
    pindices <- which(apply(lpmat, 2, function(x) !all(x == 0)) == TRUE)
    pnames <- names(pindices)
    pnames <- gsub('series', 'trend', pnames)

    # pnames <- attr(trend_mvgam$mgcv_model$pterms, 'term.labels')
    # pindices <- colnames(attr(trend_mvgam$mgcv_model$terms, 'factors'))
    plines <- vector()
    for(i in seq_along(pnames)){
      plines[i] <- paste0('// prior for ', pnames[i], '_trend...',
                          '\n',
                          'b_raw_trend[', pindices[i],
                          '] ~ student_t(3, 0, 2);\n')
    }

    if(any(grepl('// dynamic process models', model_file, fixed = TRUE))){
      model_file[grep("// dynamic process models", model_file, fixed = TRUE)] <-
        paste0('// dynamic process models\n',
               paste0(paste(plines, collapse = '\n')))
    } else {
      if(any(grepl("// dynamic factor estimates", model_file, fixed = TRUE))){
        model_file[grep("// dynamic factor estimates", model_file, fixed = TRUE)] <-
          paste0('// dynamic process models\n',
                 paste0(paste(plines, collapse = '\n')))
      }

      if(any(grepl("// trend means", model_file, fixed = TRUE))){
        model_file[grep("// trend means", model_file, fixed = TRUE)] <-
          paste0('// dynamic process models\n',
                 paste0(paste(plines, collapse = '\n'),
                        '// trend means'))
      }

    }

  }
  model_file <- readLines(textConnection(model_file), n = -1)

  # Add any random effect beta lines
  trend_random_included <- FALSE
  if(any(grepl('mu_raw[', trend_model_file, fixed = TRUE))){
    trend_random_included <- TRUE
    smooth_labs <- do.call(rbind,
                           lapply(seq_along(trend_mvgam$mgcv_model$smooth),
                                  function(x){
                                    data.frame(label = trend_mvgam$mgcv_model$smooth[[x]]$label,
                                               first.para = trend_mvgam$mgcv_model$smooth[[x]]$first.para,
                                               last.para = trend_mvgam$mgcv_model$smooth[[x]]$last.para,
                                               class = class(trend_mvgam$mgcv_model$smooth[[x]])[1])
                                  }))
    random_inds <- vector()
    for(i in 1:NROW(smooth_labs)){
      if(smooth_labs$class[i] == 'random.effect'){
        random_inds[i] <- paste0(smooth_labs$first.para[i],
                                 ':',
                                 smooth_labs$last.para[i])
      }
    }
    random_inds <- random_inds[!is.na(random_inds)]
    trend_rand_idxs <- unlist(lapply(seq_along(random_inds), function(x){
      seq(as.numeric(sub("\\:.*", "", random_inds[x])),
          sub(".*\\:", "", random_inds[x]))
    }))
    model_data$trend_rand_idxs <- trend_rand_idxs

    model_file[grep("int<lower=0> obs_ind[n_nonmissing]; // indices of nonmissing observations",
                    model_file, fixed = TRUE)] <-
      paste0("int<lower=0> obs_ind[n_nonmissing]; // indices of nonmissing observations\n",
             paste0("int trend_rand_idxs[", length(trend_rand_idxs),']; // trend random effect indices'))

    random_param_lines <- trend_model_file[c(grep("// random effect variances",
                                                  trend_model_file, fixed = TRUE) + 1,
                                             grep("// random effect means",
                                                  trend_model_file, fixed = TRUE) + 1)]
    random_param_lines <- gsub('raw', 'raw_trend', random_param_lines)
    model_file[grep("vector[num_basis_trend] b_raw_trend;",
                    model_file, fixed = TRUE)] <-
      paste0("vector[num_basis_trend] b_raw_trend;\n\n",
             "// trend random effects\n",
             paste(random_param_lines, collapse = '\n'))

    if(trend_model %in% c('RW', 'AR1', 'AR2', 'AR3', 'CAR1')){
      model_file[grep("LV[1, 1:n_lv] ~ normal(0, sigma);", model_file,
                      fixed = TRUE)] <- paste0(
                        "sigma_raw_trend ~ exponential(0.5);\n",
                        "mu_raw_trend ~ std_normal();\n",
                        paste0("b_raw_trend[", 'trend_rand_idxs',
                               "] ~ std_normal();\n"),
                        "LV[1, 1:n_lv] ~ normal(0, sigma);")
    }

    if(trend_model == 'VAR1'){
      if(any(grepl("cholesky_factor_corr[n_lv] L_Omega;",
                   model_file, fixed = TRUE))){
        model_file[grep("LV[1] ~ multi_normal(trend_zeros, Gamma);",
                        model_file, fixed = TRUE)] <-
          paste0(
            "sigma_raw_trend ~ exponential(0.5);\n",
            "mu_raw_trend ~ std_normal();\n",
            paste0("b_raw_trend[", 'trend_rand_idxs',
                   "] ~ std_normal();\n"),
            "LV[1] ~ multi_normal(trend_zeros, Gamma);")

      } else {
        model_file[grep("LV[1] ~ normal(0, sigma);",
                        model_file, fixed = TRUE)] <-
          paste0(
            "sigma_raw_trend ~ exponential(0.5);\n",
            "mu_raw_trend ~ std_normal();\n",
            paste0("b_raw_trend[", 'trend_rand_idxs',
                   "] ~ std_normal();\n"),
            "LV[1] ~ normal(0, sigma);")
      }
    }

    model_file <- readLines(textConnection(model_file), n = -1)
  }

  # Update the trend model statements
  model_file[grep("// latent states and loading matrix",
                  model_file, fixed = TRUE)] <-
    paste0("// latent states and loading matrix\n",
           "vector[n * n_lv] trend_mus;")
  model_file[grep("// derived latent states",
                  model_file, fixed = TRUE)] <-
    paste0("// latent process linear predictors\n",
           "trend_mus = X_trend * b_trend;\n\n",
           "// derived latent states")
  model_file <- readLines(textConnection(model_file), n = -1)

  #### Trend model specific updates ####
  if(trend_model == 'RW'){
    model_file <- model_file[-c(grep("for(j in 1:n_lv){", model_file, fixed = TRUE):
                                  (grep("for(j in 1:n_lv){", model_file, fixed = TRUE) + 2))]

    if(drift){
      model_file[grep("LV[1, 1:n_lv] ~ normal(0, sigma);",
                      model_file, fixed = TRUE)] <-
        paste0("for(j in 1:n_lv){\n",
               "LV[1, j] ~ normal(trend_mus[ytimes_trend[1, j]], sigma[j]);\n",
               "for(i in 2:n){\n",
               "LV[i, j] ~ normal(drift[j] + trend_mus[ytimes_trend[i, j]] + LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]], sigma[j]);\n",
               "}\n}")
    } else {
      model_file[grep("LV[1, 1:n_lv] ~ normal(0, sigma);",
                      model_file, fixed = TRUE)] <-
        paste0("for(j in 1:n_lv){\n",
               "LV[1, j] ~ normal(trend_mus[ytimes_trend[1, j]], sigma[j]);\n",
               "for(i in 2:n){\n",
               "LV[i, j] ~ normal(trend_mus[ytimes_trend[i, j]] + LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]], sigma[j]);\n",
               "}\n}")
    }

    model_file <- readLines(textConnection(model_file), n = -1)
  }

  if(trend_model == 'CAR1'){
    model_file[grep('// latent factor AR1 terms', model_file, fixed = TRUE)] <-
      '// latent state AR1 terms'
    model_file <- model_file[-c(grep("for(j in 1:n_lv){", model_file, fixed = TRUE):
                                  (grep("for(j in 1:n_lv){", model_file, fixed = TRUE) + 2))]

    if(drift){
      model_file[grep("LV[1, 1:n_lv] ~ normal(0, sigma);",
                      model_file, fixed = TRUE)] <-
        paste0("for(j in 1:n_lv){\n",
               "LV[1, j] ~ normal(trend_mus[ytimes_trend[1, j]], sigma[j]);\n",
               "for(i in 2:n){\n",
               "LV[i, j] ~ normal(drift[j] + trend_mus[ytimes_trend[i, j]] + pow(ar1[j], time_dis[i, j]) * (LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]]), sigma[j]);\n",
               "}\n}")
    } else {
      model_file[grep("LV[1, 1:n_lv] ~ normal(0, sigma);",
                      model_file, fixed = TRUE)] <-
        paste0("for(j in 1:n_lv){\n",
               "LV[1, j] ~ normal(trend_mus[ytimes_trend[1, j]], sigma[j]);\n",
               "for(i in 2:n){\n",
               "LV[i, j] ~ normal(trend_mus[ytimes_trend[i, j]] + pow(ar1[j], time_dis[i, j]) * (LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]]), sigma[j]);\n",
               "}\n}")
    }

    model_file <- readLines(textConnection(model_file), n = -1)
  }

  if(trend_model == 'AR1'){
    model_file[grep('// latent factor AR1 terms', model_file, fixed = TRUE)] <-
      '// latent state AR1 terms'
    model_file <- model_file[-c(grep("for(j in 1:n_lv){", model_file, fixed = TRUE):
                                  (grep("for(j in 1:n_lv){", model_file, fixed = TRUE) + 2))]

    if(drift){
      model_file[grep("LV[1, 1:n_lv] ~ normal(0, sigma);",
                      model_file, fixed = TRUE)] <-
        paste0("for(j in 1:n_lv){\n",
               "LV[1, j] ~ normal(trend_mus[ytimes_trend[1, j]], sigma[j]);\n",
               "for(i in 2:n){\n",
               "LV[i, j] ~ normal(drift[j] + trend_mus[ytimes_trend[i, j]] + ar1[j] * (LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]]), sigma[j]);\n",
               "}\n}")
    } else {
      model_file[grep("LV[1, 1:n_lv] ~ normal(0, sigma);",
                      model_file, fixed = TRUE)] <-
        paste0("for(j in 1:n_lv){\n",
               "LV[1, j] ~ normal(trend_mus[ytimes_trend[1, j]], sigma[j]);\n",
               "for(i in 2:n){\n",
               "LV[i, j] ~ normal(trend_mus[ytimes_trend[i, j]] + ar1[j] * (LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]]), sigma[j]);\n",
               "}\n}")
    }

    model_file <- readLines(textConnection(model_file), n = -1)
  }

  if(trend_model == 'AR2'){
    model_file[grep('// latent factor AR1 terms', model_file, fixed = TRUE)] <-
      '// latent state AR1 terms'
    model_file[grep('// latent factor AR2 terms', model_file, fixed = TRUE)] <-
      '// latent state AR2 terms'
    model_file <- model_file[-c(grep("for(j in 1:n_lv){", model_file, fixed = TRUE):
                                  (grep("for(j in 1:n_lv){", model_file, fixed = TRUE) + 2))]

    if(drift){
      model_file[grep("LV[1, 1:n_lv] ~ normal(0, sigma);",
                      model_file, fixed = TRUE)] <-
        paste0("for(j in 1:n_lv){\n",
               "LV[1, j] ~ normal(drift[j] + [ytimes_trend[1, j]], sigma[j]);\n",
               "LV[2, j] ~ normal(drift[j] + trend_mus[ytimes_trend[2, j]] + ar1[j] * (LV[1, j] - trend_mus[ytimes_trend[1, j]]), sigma[j]);\n",
               "for(i in 3:n){\n",
               "LV[i, j] ~ normal(drift[j] + trend_mus[ytimes_trend[i, j]] + ar1[j] * (LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]]) + ar2[j] * (LV[i - 2, j] - trend_mus[ytimes_trend[i - 2, j]]), sigma[j]);\n",
               "}\n}")
      model_file <- model_file[-grep("LV[2, 1:n_lv] ~ normal(drift + LV[1, 1:n_lv] * ar1, 0.1);",
                                     model_file, fixed = TRUE)]
    } else {
      model_file[grep("LV[1, 1:n_lv] ~ normal(0, sigma);",
                      model_file, fixed = TRUE)] <-
        paste0("for(j in 1:n_lv){\n",
               "LV[1, j] ~ normal(trend_mus[ytimes_trend[1, j]], sigma[j]);\n",
               "LV[2, j] ~ normal(trend_mus[ytimes_trend[2, j]] + ar1[j] * (LV[1, j] - trend_mus[ytimes_trend[1, j]]), sigma[j]);\n",
               "for(i in 3:n){\n",
               "LV[i, j] ~ normal(trend_mus[ytimes_trend[i, j]] + ar1[j] * (LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]]) + ar2[j] * (LV[i - 2, j] - trend_mus[ytimes_trend[i - 2, j]]), sigma[j]);\n",
               "}\n}")
      model_file <- model_file[-grep("LV[2, 1:n_lv] ~ normal(LV[1, 1:n_lv] * ar1, 0.1);",
                                     model_file, fixed = TRUE)]
    }

    model_file <- readLines(textConnection(model_file), n = -1)
  }

  if(trend_model == 'AR3'){
    model_file[grep('// latent factor AR1 terms', model_file, fixed = TRUE)] <-
      '// latent state AR1 terms'
    model_file[grep('// latent factor AR2 terms', model_file, fixed = TRUE)] <-
      '// latent state AR2 terms'
    model_file[grep('// latent factor AR3 terms', model_file, fixed = TRUE)] <-
      '// latent state AR3 terms'
    model_file <- model_file[-c(grep("for(j in 1:n_lv){", model_file, fixed = TRUE):
                                  (grep("for(j in 1:n_lv){", model_file, fixed = TRUE) + 2))]

    if(drift){
      model_file[grep("LV[1, 1:n_lv] ~ normal(0, sigma);",
                      model_file, fixed = TRUE)] <-
        paste0("for(j in 1:n_lv){\n",
               "LV[1, j] ~ normal(drift[j] + [ytimes_trend[1, j]], sigma[j]);\n",
               "LV[2, j] ~ normal(drift[j] + trend_mus[ytimes_trend[2, j]] + ar1[j] * (LV[1, j] - trend_mus[ytimes_trend[1, j]]), sigma[j]);\n",
               "LV[3, j] ~ normal(drift[j] + trend_mus[ytimes_trend[3, j]] + ar1[j] * (LV[2, j] - trend_mus[ytimes_trend[2, j]]) + ar2[j] * (LV[1, j] - trend_mus[ytimes_trend[1, j]]), sigma[j]);\n",
               "for(i in 4:n){\n",
               "LV[i, j] ~ normal(drift[j] + trend_mus[ytimes_trend[i, j]] + ar1[j] * (LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]]) + ar2[j] * (LV[i - 2, j] - trend_mus[ytimes_trend[i - 2, j]]) + ar3[j] * (LV[i - 3, j] - trend_mus[ytimes_trend[i - 3, j]]), sigma[j]);\n",
               "}\n}")
      model_file <- model_file[-grep("LV[2, 1:n_lv] ~ normal(drift + LV[1, 1:n_lv] * ar1, 0.1);",
                                     model_file, fixed = TRUE)]
      model_file <- model_file[-grep('LV_raw[3, 1:n_lv] ~ normal(drift + LV_raw[2, 1:n_lv] * ar1 + LV_raw[1, 1:n_lv] * ar2, 0.1);',
                                     model_file, fixed = TRUE)]
    } else {
      model_file[grep("LV[1, 1:n_lv] ~ normal(0, sigma);",
                      model_file, fixed = TRUE)] <-
        paste0("for(j in 1:n_lv){\n",
               "LV[1, j] ~ normal(trend_mus[ytimes_trend[1, j]], sigma[j]);\n",
               "LV[2, j] ~ normal(trend_mus[ytimes_trend[2, j]] + ar1[j] * (LV[1, j] - trend_mus[ytimes_trend[1, j]]), sigma[j]);\n",
               "LV[3, j] ~ normal(trend_mus[ytimes_trend[3, j]] + ar1[j] * (LV[2, j] - trend_mus[ytimes_trend[2, j]]) + ar2[j] * (LV[1, j] - trend_mus[ytimes_trend[1, j]]), sigma[j]);\n",
               "for(i in 4:n){\n",
               "LV[i, j] ~ normal(trend_mus[ytimes_trend[i, j]] + ar1[j] * (LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]]) + ar2[j] * (LV[i - 2, j] - trend_mus[ytimes_trend[i - 2, j]]) + ar3[j] * (LV[i - 3, j] - trend_mus[ytimes_trend[i - 3, j]]), sigma[j]);\n",
               "}\n}")
      model_file <- model_file[-grep("LV[2, 1:n_lv] ~ normal(LV[1, 1:n_lv] * ar1, 0.1);",
                                     model_file, fixed = TRUE)]
      model_file <- model_file[-grep('LV_raw[3, 1:n_lv] ~ normal(LV_raw[2, 1:n_lv] * ar1 + LV_raw[1, 1:n_lv] * ar2, 0.1);',
                                     model_file, fixed = TRUE)]
    }

    model_file <- readLines(textConnection(model_file), n = -1)
  }

  if(trend_model == 'VAR1'){

    model_file <- gsub('trend means', 'latent state means',
                       model_file)

    model_file[grep('mu[i - 1] = A * LV[i - 1];', model_file, fixed = TRUE)] <-
      'mu[i] = A * (LV[i - 1] - trend_mus[ytimes_trend[i - 1, 1:n_lv]]);'

    model_file[grep('vector[n_series] mu[n - 1];', model_file, fixed = TRUE)] <-
      "vector[n_series] mu[n];"

    if(any(grepl("cholesky_factor_corr[n_lv] L_Omega;",
                 model_file, fixed = TRUE))){

      model_file <- model_file[-grep("vector[n_lv] trend_zeros = rep_vector(0.0, n_lv);",
                                     model_file, fixed = TRUE)]

      model_file[grep("LV[1] ~ multi_normal(trend_zeros, Gamma);",
                      model_file, fixed = TRUE)] <-
        "LV[1] ~ multi_normal(trend_mus[ytimes_trend[1, 1:n_lv]], Gamma);"

      model_file[grep("LV[i] ~ multi_normal_cholesky(mu[i - 1], L_Sigma);",
                      model_file, fixed = TRUE)] <-
        "LV[i] ~ multi_normal_cholesky(trend_mus[ytimes_trend[i, 1:n_lv]] + mu[i], L_Sigma);"

    } else {
      model_file[grep("LV[1] ~ normal(0, sigma);",
                      model_file, fixed = TRUE)] <-
        "LV[1] ~ normal(trend_mus[ytimes_trend[1, 1:n_lv]], sigma);"

      model_file[grep("LV[i] ~ normal(mu[i - 1], sigma);",
                      model_file, fixed = TRUE)] <-
        "LV[i] ~ normal(trend_mus[ytimes_trend[i, 1:n_lv]] + mu[i], sigma);"
    }
    model_file <- readLines(textConnection(model_file), n = -1)
  }

  model_file <- gsub('latent trend', 'latent state',
                     model_file)

  # Any final tidying for trend_level terms
  model_file <- gsub('byseriestrend', 'bytrendtrend', model_file)
  model_file <- gsub(':seriestrend', ':trendtrend', model_file)

  names(model_data) <- gsub('byseriestrend', 'bytrendtrend', names(model_data))
  names(model_data) <- gsub(':seriestrend', ':trendtrend', names(model_data))

  names(trend_mvgam$mgcv_model$coefficients) <-
    gsub('byseriestrend', 'bytrendtrend', names(trend_mvgam$mgcv_model$coefficients))
  names(trend_mvgam$mgcv_model$coefficients) <-
    gsub(':seriestrend', ':trendtrend', names(trend_mvgam$mgcv_model$coefficients))

  return(list(model_file = model_file,
              model_data = model_data,
              trend_mgcv_model = trend_mvgam$mgcv_model,
              trend_sp_names = trend_mvgam$sp_names,
              trend_smooths_included = trend_smooths_included,
              trend_random_included = trend_random_included))
}

#### Helper functions for extracting parameter estimates from cmdstan objects ####
#' All functions were directly copied from `brms` and so all credit must
#' go to the `brms` development team
#' @noRd
repair_variable_names <- function(x) {
  x <- sub("\\.", "[", x)
  x <- gsub("\\.", ",", x)
  x[grep("\\[", x)] <- paste0(x[grep("\\[", x)], "]")
  x
}

#' @noRd
seq_rows = function (x)
{
  seq_len(NROW(x))
}

#' @noRd
is_equal <- function(x, y, check.attributes = FALSE, ...) {
  isTRUE(all.equal(x, y, check.attributes = check.attributes, ...))
}

#' @noRd
repair_stanfit <- function(x) {
  if (!length(x@sim$fnames_oi)) {
    # nothing to rename
    return(x)
  }
  # the posterior package cannot deal with non-unique parameter names
  # this case happens rarely but might happen when sample_prior = "yes"
  x@sim$fnames_oi <- make.unique(as.character(x@sim$fnames_oi), "__")
  for (i in seq_along(x@sim$samples)) {
    # stanfit may have renamed dimension suffixes (#1218)
    if (length(x@sim$samples[[i]]) == length(x@sim$fnames_oi)) {
      names(x@sim$samples[[i]]) <- x@sim$fnames_oi
    }
  }
  x
}

#' @importFrom methods new
#' @noRd
read_csv_as_stanfit <- function(files, variables = NULL,
                                sampler_diagnostics = NULL) {

  # Code borrowed from brms: https://github.com/paul-buerkner/brms/R/backends.R#L603
  repair_names <- function(x) {
    x <- sub("\\.", "[", x)
    x <- gsub("\\.", ",", x)
    x[grep("\\[", x)] <- paste0(x[grep("\\[", x)], "]")
    x
  }

  if(!is.null(variables)){
    # ensure that only relevant variables are read from CSV
    metadata <- cmdstanr::read_cmdstan_csv(
      files = files, variables = "", sampler_diagnostics = "")

    all_vars <- repair_names(metadata$metadata$variables)
    all_vars <- unique(sub("\\[.+", "", all_vars))
    variables <- variables[variables %in% all_vars]
  }

  csfit <- cmdstanr::read_cmdstan_csv(
    files = files, variables = variables,
    sampler_diagnostics = sampler_diagnostics,
    format = NULL
  )

  # @model_name
  model_name = gsub(".csv", "", basename(files[[1]]))

  # @model_pars
  svars <- csfit$metadata$stan_variables
  if (!is.null(variables)) {
    variables_main <- unique(gsub("\\[.*\\]", "", variables))
    svars <- intersect(variables_main, svars)
  }
  if ("lp__" %in% svars) {
    svars <- c(setdiff(svars, "lp__"), "lp__")
  }
  pars_oi <- svars
  par_names <- csfit$metadata$model_params

  # @par_dims
  par_dims <- vector("list", length(svars))

  names(par_dims) <- svars
  par_dims <- lapply(par_dims, function(x) x <- integer(0))

  pdims_num <- ulapply(
    svars, function(x) sum(grepl(paste0("^", x, "\\[.*\\]$"), par_names))
  )
  par_dims[pdims_num != 0] <-
    csfit$metadata$stan_variable_sizes[svars][pdims_num != 0]

  # @mode
  mode <- 0L

  # @sim
  rstan_diagn_order <- c("accept_stat__", "treedepth__", "stepsize__",
                         "divergent__", "n_leapfrog__", "energy__")

  if (!is.null(sampler_diagnostics)) {
    rstan_diagn_order <- rstan_diagn_order[rstan_diagn_order %in% sampler_diagnostics]
  }

  res_vars <- c(".chain", ".iteration", ".draw")
  if ("post_warmup_draws" %in% names(csfit)) {
    # for MCMC samplers
    n_chains <- max(
      posterior::nchains(csfit$warmup_draws),
      posterior::nchains(csfit$post_warmup_draws)
    )
    n_iter_warmup <- posterior::niterations(csfit$warmup_draws)
    n_iter_sample <- posterior::niterations(csfit$post_warmup_draws)
    if (n_iter_warmup > 0) {
      csfit$warmup_draws <- posterior::as_draws_df(csfit$warmup_draws)
      csfit$warmup_sampler_diagnostics <-
        posterior::as_draws_df(csfit$warmup_sampler_diagnostics)
    }
    if (n_iter_sample > 0) {
      csfit$post_warmup_draws <- posterior::as_draws_df(csfit$post_warmup_draws)
      csfit$post_warmup_sampler_diagnostics <-
        posterior::as_draws_df(csfit$post_warmup_sampler_diagnostics)
    }

    # called 'samples' for consistency with rstan
    samples <- rbind(csfit$warmup_draws, csfit$post_warmup_draws)
    # manage memory
    csfit$warmup_draws <- NULL
    csfit$post_warmup_draws <- NULL

    # prepare sampler diagnostics
    diagnostics <- rbind(csfit$warmup_sampler_diagnostics,
                         csfit$post_warmup_sampler_diagnostics)
    # manage memory
    csfit$warmup_sampler_diagnostics <- NULL
    csfit$post_warmup_sampler_diagnostics <- NULL
    # convert to regular data.frame
    diagnostics <- as.data.frame(diagnostics)
    diag_chain_ids <- diagnostics$.chain
    diagnostics[res_vars] <- NULL

  } else if ("draws" %in% names(csfit)) {
    # for variational inference "samplers"
    n_chains <- 1
    n_iter_warmup <- 0
    n_iter_sample <- posterior::niterations(csfit$draws)
    if (n_iter_sample > 0) {
      csfit$draws <- posterior::as_draws_df(csfit$draws)
    }

    # called 'samples' for consistency with rstan
    samples <- csfit$draws
    # manage memory
    csfit$draws <- NULL

    # VI has no sampler diagnostics
    diag_chain_ids <- rep(1L, nrow(samples))
    diagnostics <- as.data.frame(matrix(nrow = nrow(samples), ncol = 0))
  }

  # convert to regular data.frame
  samples <- as.data.frame(samples)
  chain_ids <- samples$.chain
  samples[res_vars] <- NULL

  move2end <- function(x, last) {
    x[c(setdiff(names(x), last), last)]
  }

  if ("lp__" %in% colnames(samples)) {
    samples <- move2end(samples, "lp__")
  }

  fnames_oi <- colnames(samples)

  colnames(samples) <- gsub("\\[", ".", colnames(samples))
  colnames(samples) <- gsub("\\]", "", colnames(samples))
  colnames(samples) <- gsub("\\,", ".", colnames(samples))

  # split samples into chains
  samples <- split(samples, chain_ids)
  names(samples) <- NULL

  # split diagnostics into chains
  diagnostics <- split(diagnostics, diag_chain_ids)
  names(diagnostics) <- NULL

  #  @sim$sample: largely 113-130 from rstan::read_stan_csv
  values <- list()
  values$algorithm <- csfit$metadata$algorithm
  values$engine <- csfit$metadata$engine
  values$metric <- csfit$metadata$metric

  sampler_t <- NULL
  if (!is.null(values$algorithm)) {
    if (values$algorithm == "rwm" || values$algorithm == "Metropolis") {
      sampler_t <- "Metropolis"
    } else if (values$algorithm == "hmc") {
      if (values$engine == "static") {
        sampler_t <- "HMC"
      } else {
        if (values$metric == "unit_e") {
          sampler_t <- "NUTS(unit_e)"
        } else if (values$metric == "diag_e") {
          sampler_t <- "NUTS(diag_e)"
        } else if (values$metric == "dense_e") {
          sampler_t <- "NUTS(dense_e)"
        }
      }
    }
  }

  adapt_info <- vector("list", 4)
  idx_samples <- (n_iter_warmup + 1):(n_iter_warmup + n_iter_sample)

  for (i in seq_along(samples)) {
    m <- colMeans(samples[[i]][idx_samples, , drop=FALSE])
    rownames(samples[[i]]) <- seq_rows(samples[[i]])
    attr(samples[[i]], "sampler_params") <- diagnostics[[i]][rstan_diagn_order]
    rownames(attr(samples[[i]], "sampler_params")) <- seq_rows(diagnostics[[i]])

    # reformat back to text
    if (is_equal(sampler_t, "NUTS(dense_e)")) {
      mmatrix_txt <- "\n# Elements of inverse mass matrix:\n# "
      mmat <- paste0(apply(csfit$inv_metric[[i]], 1, paste0, collapse=", "),
                     collapse="\n# ")
    } else {
      mmatrix_txt <- "\n# Diagonal elements of inverse mass matrix:\n# "
      mmat <- paste0(csfit$inv_metric[[i]], collapse = ", ")
    }

    adapt_info[[i]] <- paste0("# Step size = ",
                              csfit$step_size[[i]],
                              mmatrix_txt,
                              mmat, "\n# ")

    attr(samples[[i]], "adaptation_info") <- adapt_info[[i]]

    attr(samples[[i]], "args") <- list(sampler_t = sampler_t, chain_id = i)

    if (NROW(csfit$metadata$time)) {
      time_i <- as.double(csfit$metadata$time[i, c("warmup", "sampling")])
      names(time_i) <- c("warmup", "sample")
      attr(samples[[i]], "elapsed_time") <- time_i
    }

    attr(samples[[i]], "mean_pars") <- m[-length(m)]
    attr(samples[[i]], "mean_lp__") <- m["lp__"]
  }

  perm_lst <- lapply(seq_len(n_chains), function(id) sample.int(n_iter_sample))

  # @sim
  sim <- list(
    samples = samples,
    iter = csfit$metadata$iter_sampling + csfit$metadata$iter_warmup,
    thin = csfit$metadata$thin,
    warmup = csfit$metadata$iter_warmup,
    chains = n_chains,
    n_save = rep(n_iter_sample + n_iter_warmup, n_chains),
    warmup2 = rep(n_iter_warmup, n_chains),
    permutation = perm_lst,
    pars_oi = pars_oi,
    dims_oi = par_dims,
    fnames_oi = fnames_oi,
    n_flatnames = length(fnames_oi)
  )

  # @stan_args
  sargs <- list(
    stan_version_major = as.character(csfit$metadata$stan_version_major),
    stan_version_minor = as.character(csfit$metadata$stan_version_minor),
    stan_version_patch = as.character(csfit$metadata$stan_version_patch),
    model = csfit$metadata$model_name,
    start_datetime = gsub(" ", "", csfit$metadata$start_datetime),
    method = csfit$metadata$method,
    iter = csfit$metadata$iter_sampling + csfit$metadata$iter_warmup,
    warmup = csfit$metadata$iter_warmup,
    save_warmup = csfit$metadata$save_warmup,
    thin = csfit$metadata$thin,
    engaged = as.character(csfit$metadata$adapt_engaged),
    gamma = csfit$metadata$gamma,
    delta = csfit$metadata$adapt_delta,
    kappa = csfit$metadata$kappa,
    t0 = csfit$metadata$t0,
    init_buffer = as.character(csfit$metadata$init_buffer),
    term_buffer = as.character(csfit$metadata$term_buffer),
    window = as.character(csfit$metadata$window),
    algorithm = csfit$metadata$algorithm,
    engine = csfit$metadata$engine,
    max_depth = csfit$metadata$max_treedepth,
    metric = csfit$metadata$metric,
    metric_file = character(0), # not stored in metadata
    stepsize = NA, # add in loop
    stepsize_jitter = csfit$metadata$stepsize_jitter,
    num_chains = as.character(csfit$metadata$num_chains),
    chain_id = NA, # add in loop
    file = character(0), # not stored in metadata
    init = NA, # add in loop
    seed = as.character(csfit$metadata$seed),
    file = NA, # add in loop
    diagnostic_file = character(0), # not stored in metadata
    refresh = as.character(csfit$metadata$refresh),
    sig_figs = as.character(csfit$metadata$sig_figs),
    profile_file = csfit$metadata$profile_file,
    num_threads = as.character(csfit$metadata$threads_per_chain),
    stanc_version = gsub(" ", "", csfit$metadata$stanc_version),
    stancflags = character(0), # not stored in metadata
    adaptation_info = NA, # add in loop
    has_time = is.numeric(csfit$metadata$time$total),
    time_info = NA, # add in loop
    sampler_t = sampler_t
  )

  sargs_rep <- replicate(n_chains, sargs, simplify = FALSE)

  for (i in seq_along(sargs_rep)) {
    sargs_rep[[i]]$chain_id <- i
    sargs_rep[[i]]$stepsize <- csfit$metadata$step_size[i]
    sargs_rep[[i]]$init <- as.character(csfit$metadata$init[i])
    # two 'file' elements: select the second
    file_idx <- which(names(sargs_rep[[i]]) == "file")
    sargs_rep[[i]][[file_idx[2]]] <- files[[i]]

    sargs_rep[[i]]$adaptation_info <- adapt_info[[i]]

    if (NROW(csfit$metadata$time)) {
      sargs_rep[[i]]$time_info <- paste0(
        c("#  Elapsed Time: ", "#                ", "#                ", "# "),
        c(csfit$metadata$time[i, c("warmup", "sampling", "total")], ""),
        c(" seconds (Warm-up)", " seconds (Sampling)", " seconds (Total)", "")
      )
    }
  }

  # @stanmodel
  null_dso <- new(
    "cxxdso", sig = list(character(0)), dso_saved = FALSE,
    dso_filename = character(0), modulename = character(0),
    system = R.version$system, cxxflags = character(0),
    .CXXDSOMISC = new.env(parent = emptyenv())
  )
  null_sm <- new(
    "stanmodel", model_name = model_name, model_code = character(0),
    model_cpp = list(), dso = null_dso
  )

  # @date
  sdate <- do.call(max, lapply(files, function(csv) file.info(csv)$mtime))
  sdate <- format(sdate, "%a %b %d %X %Y")

  new(
    "stanfit",
    model_name = model_name,
    model_pars = svars,
    par_dims = par_dims,
    mode = mode,
    sim = sim,
    inits = list(),
    stan_args = sargs_rep,
    stanmodel = null_sm,
    date = sdate,  # not the time of sampling
    .MISC = new.env(parent = emptyenv())
  )
}

#' @noRd
ulapply <- function(X, FUN, ..., recursive = TRUE, use.names = TRUE) {
  unlist(lapply(X, FUN, ...), recursive, use.names)
}


#### Stan diagnostic checks ####
#' Check transitions that ended with a divergence
#' @param fit A stanfit object
#' @param quiet Logical (verbose or not?)
#' @details Utility function written by Michael Betancourt (https://betanalpha.github.io/)
#' @noRd
check_div <- function(fit, quiet=FALSE, sampler_params) {
  if(missing(sampler_params)){
    sampler_params <- rstan::get_sampler_params(fit, inc_warmup=FALSE)
  }
  divergent <- do.call(rbind, sampler_params)[,'divergent__']
  n = sum(divergent)
  N = length(divergent)

  if (!quiet) cat(sprintf('%s of %s iterations ended with a divergence (%s%%)\n',
                            n, N, round(100 * n / N, 4)))
  if (n > 0) {
    if (!quiet) cat(' *Try running with larger adapt_delta to remove the divergences\n')
    if (quiet) return(FALSE)
  } else {
    if (quiet) return(TRUE)
  }
}

#' Check transitions that ended prematurely due to maximum tree depth limit
#' @param fit A stanfit object
#' @param quiet Logical (verbose or not?)
#' @details Utility function written by Michael Betancourt (https://betanalpha.github.io/)
#' @noRd
check_treedepth <- function(fit, max_depth = 10, quiet=FALSE,
                            sampler_params) {
  if(missing(sampler_params)){
    sampler_params <- rstan::get_sampler_params(fit, inc_warmup=FALSE)
  }
  treedepths <- do.call(rbind, sampler_params)[,'treedepth__']
  n = length(treedepths[sapply(treedepths, function(x) x >= max_depth)])
  N = length(treedepths)

  if (!quiet)
    cat(sprintf('%s of %s iterations saturated the maximum tree depth of %s (%s%%)\n',
                  n, N, max_depth, round(100 * n / N, 4)))

  if (n > 0) {
    if (!quiet) cat(' *Run with max_treedepth set to a larger value to avoid saturation\n')
    if (quiet) return(FALSE)
  } else {
    if (quiet) return(TRUE)
  }
}

#' Check the energy fraction of missing information (E-FMI)
#' @importFrom stats var
#' @param fit A stanfit object
#' @param quiet Logical (verbose or not?)
#' @details Utility function written by Michael Betancourt (https://betanalpha.github.io/)
#' @noRd
check_energy <- function(fit, quiet=FALSE, sampler_params) {
  if(missing(sampler_params)){
    sampler_params <- rstan::get_sampler_params(fit, inc_warmup=FALSE)
  }
  no_warning <- TRUE
  for (n in 1:length(sampler_params)) {
    energies = sampler_params[n][[1]][,'energy__']
    numer = sum(diff(energies)**2) / length(energies)
    denom = var(energies)
    if (numer / denom < 0.2) {
      if (!quiet) cat(sprintf('Chain %s: E-FMI = %s\n', n,
                              round(numer / denom, 4)))
      no_warning <- FALSE
    }
  }
  if (no_warning) {
    if (!quiet) cat('E-FMI indicated no pathological behavior\n')
    if (quiet) return(TRUE)
  } else {
    if (!quiet) cat(' *E-FMI below 0.2 indicates you may need to reparameterize your model\n')
    if (quiet) return(FALSE)
  }
}

#' Check the effective sample size per iteration
#' @param fit A stanfit object
#' @param quiet Logical (verbose or not?)
#' @details Utility function written by Michael Betancourt (https://betanalpha.github.io/)
#' @noRd
check_n_eff <- function(fit, quiet=FALSE, fit_summary) {
  if(missing(fit_summary)){
    fit_summary <- rstan::summary(fit, probs = c(0.5))$summary
  }

  if(any(grep('LV', rownames(fit_summary)))){
    fit_summary <- fit_summary[-grep('LV', rownames(fit_summary)), ]
    fit_summary <- fit_summary[-grep('lv_coefs', rownames(fit_summary)), ]
  }
  N <- dim(fit_summary)[[1]]

  iter <- dim(rstan::extract(fit)[[1]])[[1]]

  neffs <- fit_summary[,'n_eff']
  ratios <- neffs / iter
  no_warning <- TRUE
  if(min(ratios, na.rm = TRUE) < 0.001) no_warning <- FALSE
  if (no_warning) {
    if (!quiet) cat('n_eff / iter looks reasonable for all parameters\n')
    if (quiet) return(TRUE)
  } else {
    if (!quiet){
      cat('n_eff / iter below 0.001 found for',
          length(which(ratios < 0.001)),
          'parameters\n *Effective sample size is likely overestimated for these parameters\n')
    }
    if (quiet) return(FALSE)
  }
}

#' Check the potential scale reduction factors
#' @param fit A stanfit object
#' @param quiet Logical (verbose or not?)
#' @details Utility function written by Michael Betancourt (https://betanalpha.github.io/)
#' @noRd
check_rhat <- function(fit, quiet=FALSE, fit_summary) {
  if(missing(fit_summary)){
    fit_summary <- rstan::summary(fit, probs = c(0.5))$summary
  }

  if(any(grep('LV', rownames(fit_summary)))){
    fit_summary <- fit_summary[-grep('LV', rownames(fit_summary)), ]
    fit_summary <- fit_summary[-grep('lv_coefs', rownames(fit_summary)), ]
  }
  N <- dim(fit_summary)[[1]]

  no_warning <- TRUE
  rhats <- fit_summary[,'Rhat']
  if(max(rhats, na.rm = TRUE) > 1.05) no_warning <- FALSE
  if (no_warning) {
    if (!quiet) cat('Rhat looks reasonable for all parameters\n')
    if (quiet) return(TRUE)
  } else {
    if (!quiet){
      cat('Rhats above 1.05 found for',
          length(which(rhats > 1.05)),
          'parameters\n *Diagnose further to investigate why the chains have not mixed\n')
    }
    if (quiet) return(FALSE)
  }
}

#' Run all diagnostic checks
#' @param fit A stanfit object
#' @param quiet Logical (verbose or not?)
#' @details Utility function written by Michael Betancourt (https://betanalpha.github.io/)
#' @noRd
check_all_diagnostics <- function(fit, max_treedepth = 10) {
  sampler_params <- rstan::get_sampler_params(fit, inc_warmup=FALSE)
  fit_summary <- rstan::summary(fit, probs = c(0.5))$summary
  check_n_eff(fit, fit_summary = fit_summary)
  check_rhat(fit, fit_summary = fit_summary)
  check_div(fit, sampler_params = sampler_params)
  check_treedepth(fit, max_depth = max_treedepth,
                  sampler_params = sampler_params)
  check_energy(fit, sampler_params = sampler_params)
}
