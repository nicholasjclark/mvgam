#### Helper functions for preparing and manipulating Stan models ####
# All functions were modified from `brms` source code and so all credit must
# go to the `brms` development team

#' parse Stan model code with cmdstanr
#' @param model Stan model code
#' @return validated Stan model code
#' @noRd
.model_cmdstanr <- function(model_file, threads = 1, silent = 1, ...) {
  if (silent < 2) {
    message('Compiling Stan program using cmdstanr')
    message()
  }

  if (cmdstanr::cmdstan_version() < "2.26.0") {
    warning(
      'Your version of Cmdstan is < 2.26.0; some mvgam models may not work properly!'
    )
  }

  temp_file <- cmdstanr::write_stan_file(model_file)

  if (cmdstanr::cmdstan_version() >= "2.29.0") {
    if (threads > 1) {
      out <- eval_silent(
        cmdstanr::cmdstan_model(
          temp_file,
          stanc_options = list('O1'),
          cpp_options = list(stan_threads = TRUE),
          ...
        ),
        type = "message",
        try = TRUE,
        silent = silent > 0L
      )
    } else {
      out <- eval_silent(
        cmdstanr::cmdstan_model(temp_file, stanc_options = list('O1'), ...),
        type = "message",
        try = TRUE,
        silent = silent > 0L
      )
    }
  } else {
    if (threads > 1) {
      out <- eval_silent(
        cmdstanr::cmdstan_model(
          temp_file,
          cpp_options = list(stan_threads = TRUE),
          ...
        ),
        type = "message",
        try = TRUE,
        silent = silent
      )
    } else {
      out <- eval_silent(
        cmdstanr::cmdstan_model(temp_file, ...),
        type = "message",
        try = TRUE,
        silent = silent
      )
    }
  }

  return(out)
}

#' fit Stan model with cmdstanr using HMC sampling or variational inference
#' @importFrom brms read_csv_as_stanfit
#' @param model a compiled Stan model
#' @param data named list to be passed to Stan as data
#' @return a fitted Stan model
#' @noRd
.sample_model_cmdstanr <- function(
  model,
  algorithm = 'sampling',
  prior_simulation = FALSE,
  data,
  chains = 4,
  parallel = TRUE,
  silent = 1L,
  max_treedepth,
  adapt_delta,
  threads = 1,
  burnin,
  samples,
  param = param,
  save_all_pars = FALSE,
  ...
) {
  if (algorithm == 'pathfinder') {
    if (cmdstanr::cmdstan_version() < "2.33") {
      stop(
        'Your version of Cmdstan is < 2.33; the "pathfinder" algorithm is not available',
        call. = FALSE
      )
    }

    if (utils::packageVersion('cmdstanr') < '0.6.1.9000') {
      stop(
        'Your version of cmdstanr is < 0.6.1.9000; the "pathfinder" algorithm is not available',
        call. = FALSE
      )
    }
  }

  # Construct cmdstanr sampling arguments
  args <- nlist(data = data)
  dots <- list(...)
  args[names(dots)] <- dots

  if (prior_simulation) {
    burnin <- 200
  }

  # do the actual sampling
  if (silent < 2) {
    message("Start sampling")
  }

  if (algorithm == 'sampling') {
    c(args) <- nlist(
      chains = chains,
      refresh = 100,
      max_treedepth,
      adapt_delta,
      diagnostics = NULL,
      iter_sampling = samples,
      iter_warmup = burnin,
      show_messages = silent < 2
    )

    if (utils::packageVersion('cmdstanr') >= '0.7.0') {
      c(args) <- nlist(show_exceptions = silent == 0)
    }

    if (parallel) {
      c(args) <- nlist(
        parallel_chains = min(c(chains, parallel::detectCores() - 1))
      )
    }

    if (threads > 1) {
      c(args) <- nlist(threads_per_chain = threads)
    }

    out <- do_call(model$sample, args)
  } else if (algorithm %in% c("fullrank", "meanfield")) {
    c(args) <- nlist(
      algorithm = algorithm,
      refresh = 500,
      output_samples = samples
    )
    if (threads > 1) {
      c(args) <- nlist(threads = threads)
    }

    out <- do_call(model$variational, args)
  } else if (algorithm %in% c("laplace")) {
    c(args) <- nlist(refresh = 500, draws = samples)
    if (threads > 1) {
      c(args) <- nlist(threads = threads)
    }

    out <- do_call(model$laplace, args)
  } else if (algorithm %in% c("pathfinder")) {
    c(args) <- nlist(refresh = 500, draws = samples)
    if (threads > 1) {
      c(args) <- nlist(num_threads = threads)
    }

    out <- do_call(model$pathfinder, args)
  } else {
    stop("Algorithm '", algorithm, "' is not supported.", call. = FALSE)
  }

  if (algorithm %in% c('meanfield', 'fullrank', 'laplace', 'pathfinder')) {
    param <- param[!param %in% 'lp__']
  }

  # Convert model files to stan_fit class for consistency
  repair_names <- function(x) {
    x <- sub("\\.", "[", x)
    x <- gsub("\\.", ",", x)
    x[grep("\\[", x)] <- paste0(x[grep("\\[", x)], "]")
    x
  }

  if (save_all_pars) {
    out_gam_mod <- brms::read_csv_as_stanfit(
      out$output_files(),
      algorithm = algorithm
    )
  } else {
    # Exclude certain pars and transformed_pars that are never needed
    # for mvgam post-processing
    metadata <- cmdstanr::read_cmdstan_csv(
      files = out$output_files(),
      variables = "",
      sampler_diagnostics = ""
    )
    all_vars <- metadata$metadata$variables
    out_gam_mod <- brms::read_csv_as_stanfit(
      out$output_files(),
      variables = all_vars,
      exclude = c(
        'trend_raw',
        'b_raw',
        'b_raw_trend',
        'p',
        'eta',
        'phi_vec',
        'nu_vec',
        'sigma_obs_vec',
        'shape_vec',
        'phi_inv',
        'lv_coefs_raw',
        'L_Sigma',
        'L_Omega',
        'L_Sigma_group',
        'Sigma_group',
        'Gamma',
        'Gamma_group',
        'A_group',
        'L_deviation_group',
        'L_Omega_group',
        'sub_error'
      ),
      algorithm = algorithm
    )
  }

  out_gam_mod <- repair_stanfit(out_gam_mod)

  if (algorithm %in% c('meanfield', 'fullrank', 'pathfinder', 'laplace')) {
    out_gam_mod@sim$iter <- samples
    out_gam_mod@sim$thin <- 1
    out_gam_mod@stan_args[[1]]$method <- 'sampling'
  }

  return(out_gam_mod)
}

#' fit Stan model with rstan
#' @param model a compiled Stan model
#' @param sdata named list to be passed to Stan as data
#' @return a fitted Stan model
#' @noRd
.sample_model_rstan <- function(
  model,
  algorithm = 'sampling',
  prior_simulation = FALSE,
  data,
  chains = 4,
  parallel = TRUE,
  silent = 1L,
  max_treedepth,
  adapt_delta,
  threads = 1,
  burnin,
  samples,
  thin,
  ...
) {
  if (rstan::stan_version() < "2.26.0") {
    warning(
      'Your version of Stan is < 2.26.0; some mvgam models may not work properly!'
    )
  }

  if (algorithm == 'pathfinder') {
    stop(
      'The "pathfinder" algorithm is not yet available in rstan',
      call. = FALSE
    )
  }

  if (algorithm == 'laplace') {
    stop('The "laplace" algorithm is not yet available in rstan', call. = FALSE)
  }

  # Set up parallel cores
  mc_cores_def <- getOption('mc.cores')
  options(mc.cores = parallel::detectCores())
  on.exit(options(mc.cores = mc_cores_def))

  # Fit the model in rstan using custom control parameters
  if (threads > 1) {
    if (utils::packageVersion("rstan") >= "2.26") {
      threads_per_chain_def <- rstan::rstan_options("threads_per_chain")
      on.exit(rstan::rstan_options(threads_per_chain = threads_per_chain_def))
      rstan::rstan_options(threads_per_chain = threads)
    } else {
      stop(
        "Threading is not supported by backend 'rstan' version ",
        utils::packageVersion("rstan"),
        ".",
        call. = FALSE
      )
    }
  }

  # Compile the model
  if (silent < 2L) {
    message('Compiling Stan program using rstan')
    message()
  }

  stan_mod <- eval_silent(
    rstan::stan_model(model_code = model, verbose = silent < 1L),
    type = "message",
    try = TRUE,
    silent = silent >= 1L
  )

  # Construct rstan sampling arguments
  args <- nlist(object = stan_mod, data = data)
  dots <- list(...)
  args[names(dots)] <- dots

  if (samples <= burnin) {
    samples <- burnin + samples
  }

  # do the actual sampling
  if (silent < 2) {
    message("Start sampling")
  }

  if (algorithm %in% c("sampling", "fixed_param")) {
    stan_control <- list(
      max_treedepth = max_treedepth,
      adapt_delta = adapt_delta
    )
    if (prior_simulation) {
      burnin = 200
      samples = 700
    }
    if (parallel) {
      c(args) <- nlist(cores = min(c(chains, parallel::detectCores() - 1)))
    }

    c(args) <- nlist(
      warmup = burnin,
      iter = samples,
      chains = chains,
      control = stan_control,
      show_messages = silent < 1L,
      verbose = FALSE,
      thin = thin,
      pars = NA,
      refresh = 100,
      save_warmup = FALSE
    )

    out <- do_call(rstan::sampling, args)
  } else if (algorithm %in% c("fullrank", "meanfield")) {
    c(args) <- nlist(algorithm, output_samples = samples, pars = NA)
    out <- do_call(rstan::vb, args)
  } else {
    stop("Algorithm '", algorithm, "' is not supported.", call. = FALSE)
  }

  out <- repair_stanfit(out)
  return(out)
}

#' @noRd
.autoformat <- function(
  stan_file,
  overwrite_file = TRUE,
  backend = 'cmdstanr',
  silent = TRUE
) {
  # Can make LV models slightly more efficient by not filling in zeros in a loop
  if (any(grepl('lv_coefs_raw[i, j] = 0;', stan_file, fixed = TRUE))) {
    starts <- grepws('lv_coefs_raw[i, j] = 0;', stan_file) - 2
    ends <- grepws('lv_coefs_raw[i, j] = 0;', stan_file) + 2
    stan_file <- stan_file[-c(starts:ends)]
    stan_file[grepws('matrix[n_series, n_lv] lv_coefs_raw;', stan_file)] <-
      'matrix[n_series, n_lv] lv_coefs_raw = rep_matrix(0, n_series, n_lv);'
  }

  # No need to fill lv_coefs in each iteration if this is a
  # trend_formula model
  if (
    any(grepl('lv_coefs = Z;', stan_file, fixed = TRUE)) &
      !any(grepl('vector[n_lv] LV[n];', stan_file, fixed = TRUE))
  ) {
    stan_file <- stan_file[-grep('lv_coefs = Z;', stan_file, fixed = TRUE)]
    stan_file <- stan_file[
      -grep('matrix[n_series, n_lv] lv_coefs;', stan_file, fixed = TRUE)
    ]
    stan_file[grep(
      'trend[i, s] = dot_product(lv_coefs[s,], LV[i,]);',
      stan_file,
      fixed = TRUE
    )] <-
      'trend[i, s] = dot_product(Z[s,], LV[i,]);'

    stan_file[grep('// posterior predictions', stan_file, fixed = TRUE) - 1] <-
      paste0(
        stan_file[
          grep('// posterior predictions', stan_file, fixed = TRUE) - 1
        ],
        '\n',
        'matrix[n_series, n_lv] lv_coefs = Z;'
      )
    stan_file <- readLines(textConnection(stan_file), n = -1)
  }

  if (backend == 'rstan' & rstan::stan_version() < '2.29.0') {
    # normal_id_glm became available in 2.29.0; this needs to be replaced
    # with the older non-glm version
    if (any(grepl('normal_id_glm', stan_file, fixed = TRUE))) {
      if (
        any(grepl("flat_ys ~ normal_id_glm(flat_xs,", stan_file, fixed = TRUE))
      ) {
        start <- grep(
          "flat_ys ~ normal_id_glm(flat_xs,",
          stan_file,
          fixed = TRUE
        )
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
  if (backend == 'cmdstanr') {
    if (cmdstanr::cmdstan_version() < "2.32.0") {
      # If the autoformat options from cmdstanr are available,
      # make use of them to update any deprecated array syntax
      update_code <- FALSE
    }
  }

  if (update_code) {
    # Data modifications
    stan_file[grep(
      "int<lower=0> ytimes[n, n_series]; // time-ordered matrix (which col in X belongs to each [time, series] observation?)",
      stan_file,
      fixed = TRUE
    )] <-
      'array[n, n_series] int<lower=0> ytimes;  // time-ordered matrix (which col in X belongs to each [time, series] observation?)'

    stan_file[grep(
      "int<lower=0> flat_ys[n_nonmissing]; // flattened nonmissing observations",
      stan_file,
      fixed = TRUE
    )] <-
      'array[n_nonmissing] int<lower=0> flat_ys; // flattened nonmissing observations'

    stan_file[grep(
      "int<lower=0> obs_ind[n_nonmissing]; // indices of nonmissing observations",
      stan_file,
      fixed = TRUE
    )] <-
      "array[n_nonmissing] int<lower=0> obs_ind; // indices of nonmissing observations"

    if (
      any(grepl(
        'int<lower=0> ytimes_trend[n, n_lv]; // time-ordered matrix for latent states',
        stan_file,
        fixed = TRUE
      ))
    ) {
      stan_file[grep(
        "int<lower=0> ytimes_trend[n, n_lv]; // time-ordered matrix for latent states",
        stan_file,
        fixed = TRUE
      )] <-
        "array[n, n_lv] int ytimes_trend;"
    }

    if (
      any(
        grepl('int idx', stan_file) &
          grepl('// discontiguous index values', stan_file, fixed = TRUE)
      )
    ) {
      lines_replace <- which(
        grepl('int idx', stan_file) &
          grepl('// discontiguous index values', stan_file, fixed = TRUE)
      )
      for (i in lines_replace) {
        split_line <- strsplit(stan_file[i], ' ')[[1]]

        idxnum <- gsub(
          ';',
          '',
          gsub("\\s*\\[[^\\]+\\]", "", as.character(split_line[2]))
        )
        idx_length <- gsub(
          "\\]",
          "",
          gsub(
            "\\[",
            "",
            regmatches(split_line[2], gregexpr("\\[.*?\\]", split_line[2]))[[1]]
          )
        )

        stan_file[i] <-
          paste0(
            'array[',
            idx_length,
            '] int ',
            idxnum,
            '; // discontiguous index values'
          )
      }
    }

    if (
      any(grepl(
        'int<lower=0> cap[total_obs]; // upper limits of latent abundances',
        stan_file,
        fixed = TRUE
      ))
    ) {
      stan_file[grep(
        'int<lower=0> cap[total_obs]; // upper limits of latent abundances',
        stan_file,
        fixed = TRUE
      )] <-
        'array[total_obs] int<lower=0> cap; // upper limits of latent abundances'

      stan_file[grep(
        'int flat_caps[n_nonmissing];',
        stan_file,
        fixed = TRUE
      )] <-
        'array[n_nonmissing] int flat_caps;'
    }

    # Model modifications
    if (any(grepl('real flat_phis[n_nonmissing];', stan_file, fixed = TRUE))) {
      stan_file[grep(
        "real flat_phis[n_nonmissing];",
        stan_file,
        fixed = TRUE
      )] <-
        "array[n_nonmissing] real flat_phis;"
    }

    # n-mixture modifications
    if (
      any(grepl(
        'real p_ub = poisson_cdf(max_k, lambda);',
        stan_file,
        fixed = TRUE
      ))
    ) {
      stan_file[grep(
        'real p_ub = poisson_cdf(max_k, lambda);',
        stan_file,
        fixed = TRUE
      )] <-
        'real p_ub = poisson_cdf(max_k | lambda);'
    }

    # trend_formula modifications
    if (
      any(
        grepl('int trend_rand_idx', stan_file) &
          grepl('// trend random effect indices', stan_file, fixed = TRUE)
      )
    ) {
      lines_replace <- which(
        grepl('int trend_rand_idx', stan_file) &
          grepl('// trend random effect indices', stan_file, fixed = TRUE)
      )
      for (i in lines_replace) {
        split_line <- strsplit(stan_file[i], ' ')[[1]]

        trend_idxnum <- gsub(
          ';',
          '',
          gsub("\\s*\\[[^\\]+\\]", "", as.character(split_line[2]))
        )
        idx_length <- gsub(
          "\\]",
          "",
          gsub(
            "\\[",
            "",
            regmatches(split_line[2], gregexpr("\\[.*?\\]", split_line[2]))[[1]]
          )
        )

        stan_file[i] <-
          paste0(
            'array[',
            idx_length,
            '] int ',
            trend_idxnum,
            '; // trend random effect indices'
          )
      }
    }

    if (
      any(
        grepl('int trend_idx', stan_file) &
          grepl('// discontiguous index values', stan_file, fixed = TRUE)
      )
    ) {
      lines_replace <- which(
        grepl('int trend_idx', stan_file) &
          grepl('// discontiguous index values', stan_file, fixed = TRUE)
      )
      for (i in lines_replace) {
        split_line <- strsplit(stan_file[i], ' ')[[1]]

        trend_idxnum <- gsub(
          ';',
          '',
          gsub("\\s*\\[[^\\]+\\]", "", as.character(split_line[2]))
        )
        idx_length <- gsub(
          "\\]",
          "",
          gsub(
            "\\[",
            "",
            regmatches(split_line[2], gregexpr("\\[.*?\\]", split_line[2]))[[1]]
          )
        )

        stan_file[i] <-
          paste0(
            'array[',
            idx_length,
            '] int ',
            trend_idxnum,
            '; // discontiguous index values'
          )
      }
    }

    if (any(grepl('vector[n_series] trend_raw[n];', stan_file, fixed = TRUE))) {
      stan_file[grep(
        "vector[n_series] trend_raw[n];",
        stan_file,
        fixed = TRUE
      )] <-
        "array[n] vector[n_series] trend_raw;"
    }

    if (any(grepl('vector[n_lv] error[n];', stan_file, fixed = TRUE))) {
      stan_file[grep("vector[n_lv] error[n];", stan_file, fixed = TRUE)] <-
        "array[n] vector[n_lv] error;"
    }

    if (any(grepl('vector[n_series] error[n];', stan_file, fixed = TRUE))) {
      stan_file[grep("vector[n_series] error[n];", stan_file, fixed = TRUE)] <-
        "array[n] vector[n_series] error;"
    }

    if (any(grepl('vector[n_lv] LV[n];', stan_file, fixed = TRUE))) {
      stan_file[grep("vector[n_lv] LV[n];", stan_file, fixed = TRUE)] <-
        "array[n] vector[n_lv] LV;"
    }

    if (any(grepl('vector[n_series] mu[n - 1];', stan_file, fixed = TRUE))) {
      stan_file[grep("vector[n_series] mu[n - 1];", stan_file, fixed = TRUE)] <-
        "array[n - 1] vector[n_series] mu;"
    }

    if (any(grepl('vector[n_lv] mu[n - 1];', stan_file, fixed = TRUE))) {
      stan_file[grep("vector[n_lv] mu[n - 1];", stan_file, fixed = TRUE)] <-
        "array[n - 1] vector[n_lv] mu;"
    }

    if (any(grepl('vector[n_series] mu[n];', stan_file, fixed = TRUE))) {
      stan_file[grep("vector[n_series] mu[n];", stan_file, fixed = TRUE)] <-
        "array[n] vector[n_series] mu;"
    }

    if (any(grepl('vector[n_lv] mu[n];', stan_file, fixed = TRUE))) {
      stan_file[grep("vector[n_lv] mu[n];", stan_file, fixed = TRUE)] <-
        "array[n] vector[n_lv] mu;"
    }
    # Generated quantity modifications
    if (
      any(grepl(
        'real<lower=0,upper=1> ypred[n, n_series];',
        stan_file,
        fixed = TRUE
      ))
    ) {
      stan_file[grep(
        "real<lower=0,upper=1> ypred[n, n_series];",
        stan_file,
        fixed = TRUE
      )] <-
        "array[n, n_series] real<lower=0,upper=1> ypred;"
    }

    if (
      any(grepl('real<lower=0> ypred[n, n_series];', stan_file, fixed = TRUE))
    ) {
      stan_file[grep(
        "real<lower=0> ypred[n, n_series];",
        stan_file,
        fixed = TRUE
      )] <-
        "array[n, n_series] real<lower=0> ypred;"
    }

    # ARMA model modifications
    if (any(grepl('vector[n_series] epsilon[n];', stan_file, fixed = TRUE))) {
      stan_file[grep(
        "vector[n_series] epsilon[n];",
        stan_file,
        fixed = TRUE
      )] <-
        "array[n] vector[n_series] epsilon;"
    }

    if (any(grepl('vector[n_lv] epsilon[n];', stan_file, fixed = TRUE))) {
      stan_file[grep("vector[n_lv] epsilon[n];", stan_file, fixed = TRUE)] <-
        "array[n] vector[n_lv] epsilon;"
    }

    # VARMA model modifications
    if (
      any(grepl('matrix[n_series, n_series] P[1];', stan_file, fixed = TRUE))
    ) {
      stan_file[grep(
        "matrix[n_series, n_series] P[1];",
        stan_file,
        fixed = TRUE
      )] <-
        "array[1] matrix[n_series, n_series] P;"

      stan_file[grep(
        "matrix[n_series, n_series] phiGamma[2, 1];",
        stan_file,
        fixed = TRUE
      )] <-
        "array[2, 1] matrix[n_series, n_series] phiGamma;"
    }

    if (
      any(grepl(
        'matrix initial_joint_var(matrix Sigma, matrix[] phi, matrix[] theta) {',
        stan_file,
        fixed = TRUE
      ))
    ) {
      stan_file[grep(
        "matrix initial_joint_var(matrix Sigma, matrix[] phi, matrix[] theta) {",
        stan_file,
        fixed = TRUE
      )] <-
        "matrix initial_joint_var(matrix Sigma, array[] matrix phi, array[] matrix theta) {"
    }

    if (any(grepl('matrix[n_lv, n_lv] P[1];', stan_file, fixed = TRUE))) {
      stan_file[grep("matrix[n_lv, n_lv] P[1];", stan_file, fixed = TRUE)] <-
        "array[1] matrix[n_lv, n_lv] P;"

      stan_file[grep("matrix[n_lv, n_lv] R[1];", stan_file, fixed = TRUE)] <-
        "array[1] matrix[n_lv, n_lv] R;"

      stan_file[grep(
        "matrix[n_lv, n_lv] A_init[1];",
        stan_file,
        fixed = TRUE
      )] <-
        "array[1] matrix[n_lv, n_lv] A_init;"

      stan_file[grep(
        "matrix[n_lv, n_lv] theta_init[1];",
        stan_file,
        fixed = TRUE
      )] <-
        "array[1] matrix[n_lv, n_lv] theta_init;"
    }

    if (
      any(grepl('matrix[n_series, n_series] R[1];', stan_file, fixed = TRUE))
    ) {
      stan_file[grep(
        "matrix[n_series, n_series] R[1];",
        stan_file,
        fixed = TRUE
      )] <-
        "array[1] matrix[n_series, n_series] R;"

      stan_file[grep(
        "matrix[n_series, n_series] A_init[1];",
        stan_file,
        fixed = TRUE
      )] <-
        "array[1] matrix[n_series, n_series] A_init;"

      stan_file[grep(
        "matrix[n_series, n_series] theta_init[1];",
        stan_file,
        fixed = TRUE
      )] <-
        "array[1] matrix[n_series, n_series] theta_init;"
    }

    if (
      any(grepl(
        'matrix[] rev_mapping(matrix[] P, matrix Sigma) {',
        stan_file,
        fixed = TRUE
      ))
    ) {
      stan_file[grep(
        "matrix[] rev_mapping(matrix[] P, matrix Sigma) {",
        stan_file,
        fixed = TRUE
      )] <-
        "array[] matrix rev_mapping(array[] matrix P, matrix Sigma) {"

      stan_file[grep(
        "matrix[m, m] phi_for[p, p];   matrix[m, m] phi_rev[p, p];",
        stan_file,
        fixed = TRUE
      )] <-
        'array[p, p] matrix[m, m] phi_for;   array[p, p] matrix[m, m] phi_rev;'

      stan_file[grep(
        "matrix[m, m] Sigma_for[p+1];  matrix[m, m] Sigma_rev[p+1];",
        stan_file,
        fixed = TRUE
      )] <-
        'array[p+1] matrix[m, m] Sigma_for;   array[p+1] matrix[m, m] Sigma_rev;'

      stan_file[grep(
        "matrix[m, m] S_for_list[p+1];",
        stan_file,
        fixed = TRUE
      )] <-
        'array[p+1] matrix[m, m] S_for_list;'
    }

    # VAR model modifications
    if (
      any(grepl('matrix[n_lv, n_lv] phiGamma[2, 1];', stan_file, fixed = TRUE))
    ) {
      stan_file[grep(
        'matrix[n_lv, n_lv] phiGamma[2, 1];',
        stan_file,
        fixed = TRUE
      )] <-
        'array[2, 1] matrix[n_lv, n_lv] phiGamma;'
    }

    if (
      any(grepl(
        'matrix[,] rev_mapping(matrix[] P, matrix Sigma) {',
        stan_file,
        fixed = TRUE
      ))
    ) {
      stan_file[grep(
        "matrix[,] rev_mapping(matrix[] P, matrix Sigma) {",
        stan_file,
        fixed = TRUE
      )] <-
        "array[,] matrix rev_mapping(array[] matrix P, matrix Sigma) {"

      stan_file[grep(
        "matrix[m, m] phi_for[p, p];   matrix[m, m] phi_rev[p, p];",
        stan_file,
        fixed = TRUE
      )] <-
        'array[p, p] matrix[m, m] phi_for;   array[p, p] matrix[m, m] phi_rev;'

      stan_file[grep(
        "matrix[m, m] Sigma_for[p+1];  matrix[m, m] Sigma_rev[p+1];",
        stan_file,
        fixed = TRUE
      )] <-
        'array[p+1] matrix[m, m] Sigma_for;   array[p+1] matrix[m, m] Sigma_rev;'

      stan_file[grep(
        "matrix[m, m] S_for_list[p+1];",
        stan_file,
        fixed = TRUE
      )] <-
        'array[p+1] matrix[m, m] S_for_list;'

      stan_file[grep(
        "matrix[m, m] Gamma_trans[p+1];",
        stan_file,
        fixed = TRUE
      )] <-
        'array[p+1] matrix[m, m] Gamma_trans;'

      stan_file[grep(
        "matrix[m, m] phiGamma[2, p];",
        stan_file,
        fixed = TRUE
      )] <-
        'array[2, p] matrix[m, m] phiGamma;'
    }

    if (
      any(grepl(
        "real partial_log_lik(int[] seq, int start, int end,",
        stan_file,
        fixed = TRUE
      ))
    ) {
      stan_file[grepl(
        "real partial_log_lik(int[] seq, int start, int end,",
        stan_file,
        fixed = TRUE
      )] <-
        "real partial_log_lik(array[] int seq, int start, int end,"
    }

    if (
      any(grepl(
        "data vector Y, vector mu, real[] shape) {",
        stan_file,
        fixed = TRUE
      ))
    ) {
      stan_file[grepl(
        "data vector Y, vector mu, real[] shape) {",
        stan_file,
        fixed = TRUE
      )] <-
        "data vector Y, vector mu, array[] real shape) {"
    }

    if (
      any(grepl(
        "int<lower=1> seq[n_nonmissing]; // an integer sequence for reduce_sum slicing",
        stan_file,
        fixed = TRUE
      ))
    ) {
      stan_file[grepl(
        "int<lower=1> seq[n_nonmissing]; // an integer sequence for reduce_sum slicing",
        stan_file,
        fixed = TRUE
      )] <-
        "array[n_nonmissing] int<lower=1> seq; // an integer sequence for reduce_sum slicing"
    }
  }

  if (backend == 'rstan') {
    options(stanc.allow_optimizations = TRUE, stanc.auto_format = TRUE)

    out <- eval_silent(
      rstan::stanc(model_code = stan_file),
      type = "message",
      try = TRUE,
      silent = silent
    )
    out <- out$model_code
  } else {
    stan_file <- cmdstanr::write_stan_file(stan_file)

    cmdstan_mod <- eval_silent(
      cmdstanr::cmdstan_model(stan_file, compile = FALSE),
      type = "message",
      try = TRUE,
      silent = silent
    )
    out <- utils::capture.output(
      cmdstan_mod$format(
        max_line_length = 80,
        canonicalize = TRUE,
        overwrite_file = overwrite_file,
        backup = FALSE
      )
    )
    out <- paste0(out, collapse = "\n")
  }
  return(out)
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

#' @noRd
repair_variable_names <- function(x) {
  x <- sub("\\.", "[", x)
  x <- gsub("\\.", ",", x)
  x[grep("\\[", x)] <- paste0(x[grep("\\[", x)], "]")
  x
}

#' @noRd
seq_rows = function(x) {
  seq_len(NROW(x))
}

#' @noRd
is_equal <- function(x, y, check.attributes = FALSE, ...) {
  isTRUE(all.equal(x, y, check.attributes = check.attributes, ...))
}


#' @noRd
ulapply <- function(X, FUN, ..., recursive = TRUE, use.names = TRUE) {
  unlist(lapply(X, FUN, ...), recursive, use.names)
}
