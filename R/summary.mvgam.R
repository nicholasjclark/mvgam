#' Summary for a fitted \pkg{mvgam} models
#'
#' These functions take a fitted \code{mvgam} or \code{jsdgam} object and
#' return various useful summaries
#'
#' @importFrom stats printCoefmat
#'
#' @param object \code{list} object of class `mvgam`
#'
#' @param include_betas Logical. Print a summary that includes posterior
#'   summaries of all linear predictor beta coefficients (including spline
#'   coefficients)? Defaults to \code{TRUE} but use \code{FALSE} for a more
#'   concise summary
#'
#' @param smooth_test Logical. Compute estimated degrees of freedom and
#'   approximate p-values for smooth terms? Defaults to \code{TRUE}, but users
#'   may wish to set to \code{FALSE} for complex models with many smooth or
#'   random effect terms
#'
#' @param digits The number of significant digits for printing out the summary;
#'   defaults to \code{2}.
#'
#' @param ... Ignored
#'
#' @author Nicholas J Clark
#'
#' @details `summary.mvgam` and `summary.mvgam_prefit` return brief summaries of
#'   the model's call, along with posterior intervals for some of the key
#'   parameters in the model. Note that some smooths have extra penalties on the
#'   null space, so summaries for the \code{rho} parameters may include more
#'   penalty terms than the number of smooths in the original model formula.
#'   Approximate p-values for smooth terms are also returned, with methods used
#'   for their calculation following those used for `mgcv` equivalents (see
#'   \code{\link[mgcv]{summary.gam}} for details). The Estimated Degrees of
#'   Freedom (edf) for smooth terms is computed using either `edf.type = 1` for
#'   models with no trend component, or `edf.type = 0` for models with trend
#'   components. These are described in the documentation for
#'   \code{\link[mgcv]{jagam}}. Experiments suggest these p-values tend to be
#'   more conservative than those that might be returned from an equivalent model
#'   fit with \code{\link[mgcv]{summary.gam}} using `method = 'REML'`
#'
#'   `coef.mvgam` returns either summaries or full posterior estimates for `GAM`
#'   component coefficients
#'
#' @return For `summary.mvgam`, an object of class \code{mvgam_summary} containing:
#'   \itemize{
#'     \item \code{model_spec}: Model specification details (formulas, family, dimensions)
#'     \item \code{parameters}: Parameter estimates and significance tests
#'     \item \code{diagnostics}: MCMC convergence diagnostics
#'     \item \code{sampling_info}: Sampling algorithm details
#'   }
#'
#'   For `summary.mvgam_prefit`, a \code{list} is printed on-screen showing
#'   the model specifications
#'
#'   For `coef.mvgam`, either a \code{matrix} of posterior coefficient
#'   distributions (if \code{summarise == FALSE} or \code{data.frame} of
#'   coefficient summaries)
#'
#' @export
summary.mvgam = function(
  object,
  include_betas = TRUE,
  smooth_test = TRUE,
  digits = 2,
  ...
) {
  #### Some adjustments for cleaner summaries ####
  if (
    attr(object$model_data, 'trend_model') == 'None' &
      object$use_lv &
      object$family != 'nmix'
  ) {
    attr(object$model_data, 'trend_model') <- 'RW'
  }
  variational <- object$algorithm %in%
    c('fullrank', 'meanfield', 'laplace', 'pathfinder')

  #### Smooth tests ####
  if (smooth_test) {
    if (inherits(object$trend_model, 'mvgam_trend')) {
      trend_model <- object$trend_model$label
    } else {
      trend_model <- object$trend_model
    }
    object$mgcv_model <- compute_edf(
      object$mgcv_model,
      object,
      'rho',
      'sigma_raw',
      conservative = trend_model == 'None'
    )

    if (!is.null(object$trend_call) & !inherits(object, 'jsdgam')) {
      object$trend_mgcv_model <- compute_edf(
        object$trend_mgcv_model,
        object,
        'rho_trend',
        'sigma_raw_trend'
      )
    }
  }

  #### Create structured summary object using extractors ####
  summary_obj <- structure(
    list(
      model_spec = extract_model_spec(object),
      parameters = extract_parameters(object, include_betas, smooth_test, digits, variational),
      diagnostics = extract_diagnostics(object, digits, variational),
      sampling_info = extract_sampling_info(object)
    ),
    class = c("mvgam_summary", "list")
  )

  return(summary_obj)
}

#' Print method for mvgam_summary objects
#'
#' @param x An object of class \code{mvgam_summary}
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns the input object after printing
#' @export
print.mvgam_summary <- function(x, ...) {
  print_model_specification(x$model_spec)
  print_sampling_information(x$sampling_info)
  print_parameters(x$parameters)
  print_diagnostics(x$diagnostics)

  cat('\nUse how_to_cite() to get started describing this model')
  invisible(x)
}

#' Print model specification section
#' @param model_spec Model specification from mvgam_summary
#' @noRd
print_model_specification <- function(model_spec) {
  # Print formulas
  if (!is.null(model_spec$formulas$process)) {
    cat("GAM observation formula:\n")
    print(model_spec$formulas$observation)
    cat("\nGAM process formula:\n")
    print(model_spec$formulas$process)
  } else {
    cat("GAM formula:\n")
    print(model_spec$formulas$observation)
  }

  # Print family and link
  cat("\nFamily:\n")
  cat(paste0(model_spec$family, '\n'))

  cat("\nLink function:\n")
  cat(paste0(model_spec$link, '\n'))

  # Print trend model
  if (!model_spec$is_jsdgam) {
    cat("\nTrend model:\n")
    if (is.call(model_spec$trend_model)) {
      print(model_spec$trend_model)
    } else {
      cat(paste0(model_spec$trend_model, '\n'))
    }
  }

  # Print latent variable info
  if (!is.null(model_spec$latent_variables)) {
    if (model_spec$latent_variables$type == "process_models") {
      cat("\nN process models:\n")
      cat(model_spec$latent_variables$count, '\n')
    } else {
      cat("\nN latent factors:\n")
      cat(model_spec$latent_variables$count, '\n')
    }
  }

  # Print dimensions
  if (model_spec$is_jsdgam) {
    cat('\nN species:\n')
    cat(model_spec$dimensions$n_species, '\n')
    cat('\nN sites:\n')
    cat(model_spec$dimensions$n_sites, '\n')
  } else {
    cat('\nN series:\n')
    cat(model_spec$dimensions$n_series, '\n')
    cat('\nN timepoints:\n')
    cat(model_spec$dimensions$n_timepoints, '\n')
  }

  # Print upper bounds if present
  if (!is.null(model_spec$upper_bounds)) {
    cat('\nUpper bounds:\n')
    cat(model_spec$upper_bounds, '\n')
  }
}

#' Print sampling information section
#' @param sampling_info Sampling information from mvgam_summary
#' @noRd
print_sampling_information <- function(sampling_info) {
  cat('\nStatus:\n')

  if (sampling_info$fit_engine == 'jags') {
    cat('Fitted using JAGS', '\n')
  } else if (sampling_info$fit_engine == 'stan') {
    cat('Fitted using Stan', '\n')

    if (!is.null(sampling_info$chains)) {
      cat(
        sampling_info$chains,
        " chains, each with iter = ",
        sampling_info$iter,
        "; warmup = ",
        sampling_info$warmup,
        "; thin = ",
        sampling_info$thin,
        " \n",
        "Total post-warmup draws = ",
        sampling_info$total_draws,
        "\n\n",
        sep = ''
      )
    }
  }
}

#' Print parameters section
#' @param parameters Parameters from mvgam_summary
#' @noRd
print_parameters <- function(parameters) {
  # Print family parameters
  family_param_labels <- c(
    "observation_error", "log_observation_error", "observation_df",
    "observation_shape", "observation_precision", "observation_dispersion"
  )

  for (label in family_param_labels) {
    if (!is.null(parameters[[label]])) {
      cat(paste0("\n", format_param_header(label), ":\n"))
      print(parameters[[label]])
    }
  }

  # Print GAM coefficients
  if (!is.null(parameters$gam_coefficients)) {
    cat("\nGAM coefficient (beta) estimates:\n")
    print(parameters$gam_coefficients)
  }

  if (!is.null(parameters$gam_obs_coefficients)) {
    cat("\nGAM observation model coefficient (beta) estimates:\n")
    print(parameters$gam_obs_coefficients)
  }

  # Print group-level parameters
  if (!is.null(parameters$gam_group_level)) {
    cat("\nGAM group-level estimates:\n")
    print(parameters$gam_group_level)
  }

  if (!is.null(parameters$gam_obs_group_level)) {
    cat("\nGAM observation model group-level estimates:\n")
    print(parameters$gam_obs_group_level)
  }

  # Print GP parameters
  if (!is.null(parameters$gam_gp_parameters)) {
    cat("\nGAM gp term marginal deviation (alpha) and length scale (rho) estimates:\n")
    print(parameters$gam_gp_parameters)
  }

  if (!is.null(parameters$gam_obs_gp_parameters)) {
    cat("\nGAM observation model gp term marginal deviation (alpha) and length scale (rho) estimates:\n")
    print(parameters$gam_obs_gp_parameters)
  }

  # Print smooth tests
  if (!is.null(parameters$gam_smooth_tests)) {
    cat("\nApproximate significance of GAM smooths:\n")
    suppressWarnings(printCoefmat(
      parameters$gam_smooth_tests,
      digits = 4,
      signif.stars = getOption("show.signif.stars"),
      has.Pvalue = TRUE,
      na.print = "NA",
      cs.ind = 1
    ))
  }

  if (!is.null(parameters$gam_obs_smooth_tests)) {
    cat("\nApproximate significance of GAM observation smooths:\n")
    suppressWarnings(printCoefmat(
      parameters$gam_obs_smooth_tests,
      digits = 4,
      signif.stars = getOption("show.signif.stars"),
      has.Pvalue = TRUE,
      na.print = "NA",
      cs.ind = 1
    ))
  }

  # Print trend parameters (using labels from param_info)
  trend_param_patterns <- c(
    "drift_parameter", "standard_deviation", "precision_parameter",
    "autoregressive_coef", "var_coefficient", "marginal_deviation",
    "length_scale", "growth_rate", "offset_parameter"
  )

  for (pattern in trend_param_patterns) {
    matching_params <- names(parameters)[grepl(pattern, names(parameters))]
    for (param_name in matching_params) {
      if (!is.null(parameters[[param_name]])) {
        cat(paste0("\n", format_trend_header(param_name), ":\n"))
        print(parameters[[param_name]])
      }
    }
  }

  # Print hierarchical correlation
  if (!is.null(parameters$hierarchical_correlation)) {
    cat("\nHierarchical correlation weighting parameter (alpha_cor) estimates:\n")
    print(parameters$hierarchical_correlation)
  }

  # Print trend GAM parameters
  if (!is.null(parameters$gam_process_coefficients)) {
    cat("\nGAM process model coefficient (beta) estimates:\n")
    print(parameters$gam_process_coefficients)
  }

  if (!is.null(parameters$gam_process_group_level)) {
    cat("\nGAM process model group-level estimates:\n")
    print(parameters$gam_process_group_level)
  }

  if (!is.null(parameters$gam_process_gp_parameters)) {
    cat("\nGAM process model gp term marginal deviation (alpha) and length scale (rho) estimates:\n")
    print(parameters$gam_process_gp_parameters)
  }

  if (!is.null(parameters$gam_process_smooth_tests)) {
    cat("\nApproximate significance of GAM process smooths:\n")
    suppressWarnings(printCoefmat(
      parameters$gam_process_smooth_tests,
      digits = 4,
      signif.stars = getOption("show.signif.stars"),
      has.Pvalue = TRUE,
      na.print = "NA",
      cs.ind = 1
    ))
  }
}

#' Print diagnostics section
#' @param diagnostics Diagnostics from mvgam_summary
#' @noRd
print_diagnostics <- function(diagnostics) {
  if (diagnostics$fit_engine == 'stan' && diagnostics$algorithm == 'sampling') {
    if (diagnostics$stan_diagnostics_available) {
      cat('\nStan MCMC diagnostics:\n')

      if (!is.null(diagnostics$sampler_message)) {
        cat(insight::format_message(diagnostics$sampler_message,
                                    indent = ""))
        cat('\n')
      }
    }
  } else if (diagnostics$algorithm != 'sampling') {
    if (!is.null(diagnostics$message)) {
      cat(paste0('\n', diagnostics$message, '\n'))
    }
  } else if (diagnostics$fit_engine == 'jags') {
    cat('\nJAGS MCMC diagnostics:\n')
    if (!is.null(diagnostics$jags_diagnostics)) {
      if (diagnostics$jags_diagnostics$rhat_ok) {
        cat('\nRhat looks reasonable for all parameters\n')
      } else {
        cat(
          '\nRhats above 1.05 found for',
          diagnostics$jags_diagnostics$n_high_rhat,
          'parameters\n* Use pairs() to investigate\n'
        )
      }
    }
  }
}

#' Format parameter header for display
#' @param label Parameter label
#' @return Formatted header string
#' @noRd
format_param_header <- function(label) {
  switch(label,
    "observation_error" = "Observation error parameter estimates",
    "log_observation_error" = "log(observation error) parameter estimates",
    "observation_df" = "Observation df parameter estimates",
    "observation_shape" = "Observation shape parameter estimates",
    "observation_precision" = "Observation precision parameter estimates",
    "observation_dispersion" = "Observation dispersion parameter estimates",
    label # fallback
  )
}

#' Format trend parameter header for display
#' @param param_name Parameter name
#' @return Formatted header string
#' @noRd
format_trend_header <- function(param_name) {
  # This would need more sophisticated logic to match the original formatting
  # For now, return a simple transformation
  gsub("_", " ", param_name)
}

#' @rdname summary.mvgam
#'
#' @export
summary.mvgam_prefit = function(object, ...) {
  if (!is.null(object$trend_call)) {
    cat("\nGAM observation formula:\n")
    print(object$call)

    cat("\nGAM process formula:\n")
    print(object$trend_call)
  } else {
    cat("\nGAM formula:\n")
    print(object$call)
  }

  cat("\n\nFamily:\n")
  cat(paste0(object$family, '\n'))

  cat("\nLink function:\n")
  cat(paste0(family_links(object$family), '\n'))

  if (!inherits(object, 'jsdgam')) {
    cat("\nTrend model:\n")
    if (inherits(object$trend_model, 'mvgam_trend')) {
      print(object$trend_model$label)
      cat('\n')
    } else {
      cat(paste0(object$trend_model, '\n'))
    }
  }

  if (object$use_lv) {
    if (!is.null(object$trend_call)) {
      cat("\nN process models:\n")
      cat(object$n_lv, '\n')
    } else {
      cat("\nN latent factors:\n")
      cat(object$n_lv, '\n')
    }
  }

  if (inherits(object, 'jsdgam')) {
    cat('\nN species:\n')
    cat(NCOL(object$ytimes), '\n')
  } else {
    cat('\nN series:\n')
    cat(NCOL(object$ytimes), '\n')
  }

  if (inherits(object, 'jsdgam')) {
    cat('\nN sites:\n')
    cat(NROW(object$ytimes), '\n')
  } else {
    cat('\nN timepoints:\n')
    cat(NROW(object$ytimes), '\n')
  }

  cat('\nStatus:')
  cat('Not fitted', '\n')
}

#' @rdname summary.mvgam
#'
#' @export
#'
#' @title Extract mvgam beta coefficients from the GAM component
#'
#' @param object \code{list} object returned from \code{mvgam}
#'
#' @param summarise \code{logical}. Summaries of coefficients will be returned
#'  if \code{TRUE}. Otherwise the full posterior distribution will be returned
#'
#' @method coef mvgam
#'
#' @export
coef.mvgam = function(object, summarise = TRUE, ...) {
  coef_names <- names(object$mgcv_model$coefficients)

  if (summarise) {
    mvgam_coefs <- mcmc_summary(object$model_output, 'b')[, c(3:7)]
    rownames(mvgam_coefs) <- coef_names
  } else {
    mvgam_coefs <- mcmc_chains(object$model_output, 'b')
    colnames(mvgam_coefs) <- coef_names
  }

  return(mvgam_coefs)
}

#' Extract a clean mcmc_summary table of params
#' @param object An `mvgam` or `jsdgam` object
#' @param params A string of parameters to extract
#' @param digits The number of significant digits for printing out the summary
#' @param variational Logical indicating whether a variational approximation was used
#' @noRd
clean_summary_table = function(
  object,
  params,
  digits = 2,
  variational = FALSE
) {
  mcmc_summary(
    object$model_output,
    params,
    ISB = TRUE,
    digits = digits,
    variational = variational
  )[, c(3:7)]
}

#' Calculate and return summary table for GP parameters
#' @param object An `mvgam` or `jsdgam` object
#' @param mgcv_model A `gam` object containing GP effects
#' @param trend_effects Logical indicating whether this is a trend_mgcv_model
#' @param digits The number of significant digits for printing out the summary
#' @param variational Logical indicating whether a variational approximation was used
#' @noRd
gp_param_summary = function(
  object,
  mgcv_model,
  trend_effects = FALSE,
  digits = 2,
  variational = FALSE
) {
  # Extract GP name and isotropic information
  gp_names <- unlist(
    purrr::map(attr(mgcv_model, 'gp_att_table'), 'name'),
    use.names = FALSE
  )
  gp_isos <- unlist(
    purrr::map(attr(mgcv_model, 'gp_att_table'), 'iso'),
    use.names = FALSE
  )
  gp_dims <- unlist(
    purrr::map(attr(mgcv_model, 'gp_att_table'), 'dim'),
    use.names = FALSE
  )

  # Create full list of rho parameter names
  full_names <- vector(mode = 'list', length = length(gp_names))
  for (i in seq_len(length(gp_names))) {
    if (gp_isos[i]) {
      full_names[[i]] <- gp_names[i]
    } else {
      full_names[[i]] <- paste0(gp_names[i], '[', 1:gp_dims[i], ']')
    }
  }
  full_names <- unlist(full_names, use.names = FALSE)

  # Determine which parameters to extract
  if (trend_effects) {
    alpha_params <- gsub(
      'gp_',
      'gp_trend_',
      gsub(
        'series',
        'trend',
        paste0('alpha_', clean_gpnames(gp_names)),
        fixed = TRUE
      ),
      fixed = TRUE
    )
    rho_params <- gsub(
      'gp_',
      'gp_trend_',
      gsub(
        'series',
        'trend',
        paste0('rho_', clean_gpnames(gp_names)),
        fixed = TRUE
      ),
      fixed = TRUE
    )
  } else {
    alpha_params <- paste0('alpha_', clean_gpnames(gp_names))
    rho_params <- paste0('rho_', clean_gpnames(gp_names))
  }

  # Create summary tables
  alpha_summary <- clean_summary_table(
    object = object,
    params = alpha_params,
    digits = digits,
    variational = variational
  )
  rownames(alpha_summary) <- paste0('alpha_', gp_names)

  rho_summary <- clean_summary_table(
    object = object,
    params = rho_params,
    digits = digits,
    variational = variational
  )
  rownames(rho_summary) <- paste0('rho_', full_names)

  # Return as a list
  return(list(alpha_summary = alpha_summary, rho_summary = rho_summary))
}

#' Extract model specification information from mvgam object
#' @param object An mvgam object
#' @return List containing model specification details
#' @noRd
extract_model_spec <- function(object) {
  # Extract formulas - always use same structure
  formulas <- list(
    observation = object$call,
    process = if (!is.null(object$trend_call)) object$trend_call else NULL
  )

  # Extract trend model information
  if (!inherits(object, 'jsdgam')) {
    if (inherits(object$trend_model, 'mvgam_trend')) {
      trend_model <- object$trend_model$label
    } else {
      trend_model <- object$trend_model
    }
  } else {
    trend_model <- NULL
  }

  # Extract dimensions and counts
  if (object$use_lv) {
    if (!is.null(object$trend_call)) {
      lv_info <- list(
        type = "process_models",
        count = object$n_lv
      )
    } else {
      lv_info <- list(
        type = "latent_factors",
        count = object$n_lv
      )
    }
  } else {
    lv_info <- NULL
  }

  # Extract series/species and timepoints/sites information
  if (inherits(object, 'jsdgam')) {
    dimensions <- list(
      n_species = NCOL(object$ytimes),
      n_sites = NROW(object$ytimes)
    )
  } else {
    dimensions <- list(
      n_series = NCOL(object$ytimes),
      n_timepoints = NROW(object$ytimes)
    )
  }

  # Compile model specification
  model_spec <- list(
    formulas = formulas,
    family = object$family,
    link = family_links(object$family),
    trend_model = trend_model,
    upper_bounds = object$upper_bounds,
    latent_variables = lv_info,
    dimensions = dimensions,
    is_jsdgam = inherits(object, 'jsdgam')
  )

  return(model_spec)
}

#' Extract sampling information from mvgam object
#' @param object An mvgam object
#' @return List containing sampling details
#' @noRd
extract_sampling_info <- function(object) {
  sampling_info <- list(
    fit_engine = object$fit_engine,
    algorithm = object$algorithm
  )

  if (object$fit_engine == 'stan') {
    n_kept <- object$model_output@sim$n_save - object$model_output@sim$warmup2

    sampling_info$chains <- object$model_output@sim$chains
    sampling_info$iter <- object$model_output@sim$iter
    sampling_info$warmup <- object$model_output@sim$warmup
    sampling_info$thin <- object$model_output@sim$thin
    sampling_info$total_draws <- sum(n_kept)

    if (object$algorithm == 'sampling') {
      sampler <- attr(object$model_output@sim$samples[[1]], "args")$sampler_t
      if (sampler == "NUTS(diag_e)") {
        sampler <- 'sampling(hmc)'
      }
      sampling_info$sampler <- sampler
    }
  }

  return(sampling_info)
}

#' Extract diagnostic information from mvgam object
#' @param object An mvgam object
#' @param digits Number of digits for summary statistics
#' @param variational Logical indicating if variational approximation was used
#' @return List containing diagnostic information
#' @noRd
extract_diagnostics <- function(object, digits = 2, variational = FALSE) {
  diagnostics <- list(
    fit_engine = object$fit_engine,
    algorithm = object$algorithm
  )

  if (object$fit_engine == 'stan' & object$algorithm == 'sampling') {
    diagnostics$stan_diagnostics_available <- TRUE
    diagnostics$max_treedepth <- object$max_treedepth
    diagnostics$ignore_b_trend <- inherits(object, 'jsdgam')

    # Get sampler information for message
    sampler <- attr(object$model_output@sim$samples[[1]], "args")$sampler_t
    if (sampler == "NUTS(diag_e)") {
      sampler <- 'sampling(hmc)'
    }

    # Capture Stan diagnostic messages
    diag_output <- utils::capture.output({
      check_all_diagnostics(object$model_output)
    })

    diagnostics$sampler_message <- c(
      diag_output,
      paste0(
        "\n",
        "Samples were drawn using ",
        sampler,
        ". For each parameter, n_eff is",
        " a crude measure of effective",
        " sample size, and Rhat is the",
        " potential scale reduction factor",
        " on split MCMC chains (at",
        " convergence, Rhat = 1)"
      )
    )
  } else if (object$algorithm != 'sampling') {
    diagnostics$message <- "Posterior approximation used: no diagnostics to compute"
  } else if (object$fit_engine == 'jags') {
    # Extract JAGS diagnostics
    rhats <- mcmc_summary(
      object$model_output,
      digits = digits,
      variational = variational
    )[, 6]

    diagnostics$jags_diagnostics <- list(
      rhats = rhats,
      n_high_rhat = length(which(rhats > 1.05)),
      rhat_ok = !any(rhats > 1.05)
    )
  }

  return(diagnostics)
}

#' Helper function to extract parameter summary with consistent columns
#' @param model_output MCMC output object
#' @param param_name Parameter name to extract
#' @param digits Number of digits
#' @param variational Logical for variational approximation
#' @param ISB Logical for ISB parameter (used for group-level effects)
#' @return Parameter summary matrix
#' @noRd
extract_param_summary <- function(model_output,
                                  param_name,
                                  digits = 2,
                                  variational = FALSE,
                                  ISB = TRUE) {
  mcmc_summary(
    model_output,
    param_name,
    ISB = ISB,
    digits = digits,
    variational = variational
  )[, c(3:7)]
}

#' Extract family-specific parameters
#' @param object An mvgam object
#' @param digits Number of digits
#' @param variational Logical for variational approximation
#' @return List of family parameter summaries
#' @noRd
extract_family_parameters <- function(object, digits = 2, variational = FALSE) {
  family_info <- family_param_info(object$family)
  family_params <- list()

  if (length(family_info$param_names) > 0) {
    for (i in seq_along(family_info$param_names)) {
      param_name <- family_info$param_names[i]
      param_label <- family_info$labels[i]

      family_params[[param_label]] <- extract_param_summary(
        object$model_output, param_name, digits, variational
      )
    }
  }

  return(family_params)
}

#' Extract GAM coefficient parameters
#' @param object An mvgam object
#' @param include_betas Logical to include all coefficients
#' @param digits Number of digits
#' @param variational Logical for variational approximation
#' @return List of coefficient summaries
#' @noRd
extract_gam_coefficients <- function(object, include_betas = TRUE, digits = 2, variational = FALSE) {
  gam_params <- list()

  # Determine coefficient subset
  if (include_betas) {
    coef_indices <- seq_along(object$mgcv_model$coefficients)
  } else {
    coef_indices <- if (object$mgcv_model$nsdf > 0) 1:object$mgcv_model$nsdf else integer(0)
  }

  if (length(coef_indices) > 0) {
    coef_names <- names(object$mgcv_model$coefficients)[coef_indices]
    mvgam_coefs <- extract_param_summary(object$model_output, 'b', digits, variational)

    if (nrow(mvgam_coefs) >= max(coef_indices)) {
      mvgam_coefs <- mvgam_coefs[coef_indices, , drop = FALSE]
      rownames(mvgam_coefs) <- coef_names

      # Choose appropriate label based on model structure
      coef_label <- if (!is.null(object$trend_call)) "gam_obs_coefficients" else "gam_coefficients"
      gam_params[[coef_label]] <- mvgam_coefs
    }
  }

  return(gam_params)
}

#' Extract parameter estimates from mvgam object
#' @param object An mvgam object
#' @param include_betas Logical, include all beta coefficients
#' @param smooth_test Logical, compute significance tests for smooths
#' @param digits Number of digits for summaries
#' @param variational Logical indicating if variational approximation was used
#' @return List containing all parameter estimates
#' @noRd
extract_parameters <- function(object, include_betas = TRUE, smooth_test = TRUE,
                               digits = 2, variational = FALSE) {

  parameters <- list()

  # Extract family-specific parameters
  parameters <- c(parameters, extract_family_parameters(object, digits, variational))

  # Extract GAM coefficients
  parameters <- c(parameters, extract_gam_coefficients(object, include_betas, digits, variational))

  # Extract remaining parameter types
  parameters <- c(parameters, extract_group_level_parameters(object, digits, variational))
  parameters <- c(parameters, extract_gp_parameters(object, digits, variational))
  parameters <- c(parameters, extract_smooth_tests(object, smooth_test, digits))
  parameters <- c(parameters, extract_trend_parameters(object, include_betas, smooth_test, digits, variational))

  return(parameters)
}

#' Extract group-level (random effect) parameters
#' @param object An mvgam object
#' @param digits Number of digits
#' @param variational Logical for variational approximation
#' @return List of group-level parameter summaries
#' @noRd
extract_group_level_parameters <- function(object, digits = 2, variational = FALSE) {
  group_params <- list()

  if (!all(is.na(object$sp_names))) {
    has_random_effects <- any(unlist(purrr::map(
      object$mgcv_model$smooth,
      inherits,
      'random.effect'
    )))

    if (has_random_effects) {
      re_labs <- unlist(lapply(
        purrr::map(object$mgcv_model$smooth, 'label'),
        paste,
        collapse = ','
      ))[unlist(purrr::map(object$mgcv_model$smooth, inherits, 'random.effect'))]

      re_sds <- extract_param_summary(
        object$model_output,
        'sigma_raw',
        digits = digits,
        variational = variational,
        ISB = TRUE
      )

      re_mus <- extract_param_summary(
        object$model_output,
        'mu_raw',
        digits = digits,
        variational = variational,
        ISB = TRUE
      )

      rownames(re_sds) <- paste0('sd(', re_labs, ')')
      rownames(re_mus) <- paste0('mean(', re_labs, ')')

      param_label <- if (!is.null(object$trend_call)) "gam_obs_group_level" else "gam_group_level"
      group_params[[param_label]] <- rbind(re_mus, re_sds)
    }
  }

  return(group_params)
}

#' Extract Gaussian Process parameters
#' @param object An mvgam object
#' @param digits Number of digits
#' @param variational Logical for variational approximation
#' @return List of GP parameter summaries
#' @noRd
extract_gp_parameters <- function(object, digits = 2, variational = FALSE) {
  gp_params <- list()

  if (!is.null(attr(object$mgcv_model, 'gp_att_table'))) {
    gp_summaries <- gp_param_summary(
      object = object,
      mgcv_model = object$mgcv_model,
      digits = digits,
      variational = variational
    )

    param_label <- if (!is.null(object$trend_call)) "gam_obs_gp_parameters" else "gam_gp_parameters"
    gp_params[[param_label]] <- rbind(gp_summaries$alpha_summary, gp_summaries$rho_summary)
  }

  return(gp_params)
}

#' Extract smooth significance tests
#' @param object An mvgam object
#' @param smooth_test Logical to compute tests
#' @param digits Number of digits
#' @return List with smooth test results
#' @noRd
extract_smooth_tests <- function(object, smooth_test = TRUE, digits = 2) {
  smooth_params <- list()

  if (any(!is.na(object$sp_names)) & smooth_test) {
    gam_sig_table <- try(
      suppressWarnings(summary(object$mgcv_model)$s.table[, c(1, 2, 3, 4), drop = FALSE]),
      silent = TRUE
    )

    if (inherits(gam_sig_table, 'try-error')) {
      object$mgcv_model$R <- NULL
      gam_sig_table <- suppressWarnings(summary(object$mgcv_model)$s.table[, c(1, 2, 3, 4), drop = FALSE])
      gam_sig_table[, 2] <- unlist(purrr::map(object$mgcv_model$smooth, 'df'), use.names = FALSE)
    }

    # Handle GP terms
    if (!is.null(attr(object$mgcv_model, 'gp_att_table'))) {
      gp_names <- unlist(purrr::map(attr(object$mgcv_model, 'gp_att_table'), 'name'))
      if (!all(rownames(gam_sig_table) %in% gsub('gp(', 's(', gp_names, fixed = TRUE))) {
        gam_sig_table <- gam_sig_table[
          !rownames(gam_sig_table) %in% gsub('gp(', 's(', gp_names, fixed = TRUE), ,
          drop = FALSE
        ]
      } else {
        gam_sig_table <- NULL
      }
    }

    if (!is.null(gam_sig_table) && nrow(gam_sig_table) > 0) {
      param_label <- if (!is.null(object$trend_call)) "gam_obs_smooth_tests" else "gam_smooth_tests"
      smooth_params[[param_label]] <- gam_sig_table
    }
  }

  return(smooth_params)
}

#' Extract trend model parameters using param_info from trend objects
#' @param object An mvgam object
#' @param include_betas Logical to include all coefficients
#' @param smooth_test Logical to compute smooth tests
#' @param digits Number of digits
#' @param variational Logical for variational approximation
#' @return List of trend parameter summaries
#' @noRd
extract_trend_parameters <- function(object, include_betas = TRUE, smooth_test = TRUE, digits = 2, variational = FALSE) {
  trend_params <- list()

  # Get trend model information
  if (inherits(object$trend_model, 'mvgam_trend')) {
    trend_info <- attr(object$trend_model, 'param_info')
    if (!is.null(trend_info)) {
      # Extract parameters that are available in the model
      available_params <- get_available_trend_params(object, trend_info$param_names)

      if (length(available_params) > 0) {
        for (i in seq_along(available_params)) {
          param_name <- available_params[i]
          param_label <- trend_info$labels[match(param_name, trend_info$param_names)]

          # Skip trend estimates as they're not summary statistics
          if (param_name == 'trend') next

          tryCatch({
            param_summary <- extract_param_summary(
              object$model_output, param_name, digits, variational
            )
            if (!is.null(param_summary) && nrow(param_summary) > 0) {
              trend_params[[param_label]] <- param_summary
            }
          }, error = function(e) {
            # Parameter not available in this model, skip silently
          })
        }
      }
    }
  }

  # Handle hierarchical correlation parameters
  if (grepl('hiercor', validate_trend_model(object$trend_model))) {
    tryCatch({
      trend_params[["hierarchical_correlation"]] <- extract_param_summary(
        object$model_output, 'alpha_cor', digits, variational
      )
    }, error = function(e) {
      # Parameter not available, skip
    })
  }

  # Process model coefficients (trend_call section)
  if (!is.null(object$trend_call) && !inherits(object, 'jsdgam')) {
    trend_params <- c(trend_params, extract_trend_gam_parameters(object, include_betas, smooth_test, digits, variational))
  }

  return(trend_params)
}

#' Get available trend parameters for a given model
#' @param object An mvgam object
#' @param param_names Vector of parameter names from trend param_info
#' @return Vector of available parameter names
#' @noRd
get_available_trend_params <- function(object, param_names) {
  available <- character(0)

  # Check which parameters are likely to exist based on model characteristics
  trend_model_attr <- attr(object$model_data, 'trend_model')

  for (param in param_names) {
    # Always include basic parameters that most trend models have
    if (param %in% c('sigma', 'tau', 'theta', 'Sigma', 'drift')) {
      available <- c(available, param)
    }

    # AR parameters - check if AR model
    if (grepl('^ar[0-9]', param) && grepl('^AR', trend_model_attr)) {
      available <- c(available, param)
    }

    # VAR parameters
    if (param == 'A' && trend_model_attr == 'VAR1') {
      available <- c(available, param)
    }

    # GP parameters
    if (param %in% c('alpha_gp', 'rho_gp', 'b_gp') && trend_model_attr == 'GP') {
      available <- c(available, param)
    }

    # Piecewise parameters
    if (param %in% c('k_trend', 'm_trend', 'delta_trend') &&
        trend_model_attr %in% c('PWlinear', 'PWlogistic')) {
      available <- c(available, param)
    }

    # Other specific parameters can be added as needed
  }

  return(available)
}

#' Extract trend GAM parameters (coefficients, group-level, GP, smooth tests)
#' @param object An mvgam object
#' @param include_betas Logical to include all coefficients
#' @param smooth_test Logical to compute smooth tests
#' @param digits Number of digits
#' @param variational Logical for variational approximation
#' @return List of trend GAM parameter summaries
#' @noRd
extract_trend_gam_parameters <- function(object, include_betas = TRUE, smooth_test = TRUE, digits = 2, variational = FALSE) {
  trend_gam_params <- list()

  # Extract trend GAM coefficients
  if (include_betas) {
    coef_names <- paste0(names(object$trend_mgcv_model$coefficients), '_trend')
    mvgam_coefs <- extract_param_summary(object$model_output, 'b_trend', digits, variational)
    rownames(mvgam_coefs) <- gsub('series', 'trend', coef_names, fixed = TRUE)
    trend_gam_params[["gam_process_coefficients"]] <- mvgam_coefs
  } else {
    if (object$trend_mgcv_model$nsdf > 0) {
      coefs_include <- 1:object$trend_mgcv_model$nsdf
      coef_names <- paste0(names(object$trend_mgcv_model$coefficients), '_trend')[coefs_include]
      mvgam_coefs <- extract_param_summary(object$model_output, 'b_trend', digits, variational)[coefs_include, , drop = FALSE]
      rownames(mvgam_coefs) <- gsub('series', 'trend', coef_names, fixed = TRUE)
      trend_gam_params[["gam_process_coefficients"]] <- mvgam_coefs
    }
  }

  # Extract trend group-level parameters
  if (!all(is.na(object$trend_sp_names))) {
    has_random_effects <- any(unlist(purrr::map(
      object$trend_mgcv_model$smooth, inherits, 'random.effect'
    )))

    if (has_random_effects) {
      re_labs <- unlist(lapply(
        purrr::map(object$trend_mgcv_model$smooth, 'label'),
        paste, collapse = ','
      ))[unlist(purrr::map(object$trend_mgcv_model$smooth, inherits, 'random.effect'))]

      re_labs <- gsub('series', 'trend', re_labs)

      re_sds <- extract_param_summary(
        object$model_output, 'sigma_raw_trend', digits = digits,
        variational = variational, ISB = TRUE
      )
      re_mus <- extract_param_summary(
        object$model_output, 'mu_raw_trend', digits = digits,
        variational = variational, ISB = TRUE
      )

      rownames(re_sds) <- paste0('sd(', re_labs, ')_trend')
      rownames(re_mus) <- paste0('mean(', re_labs, ')_trend')

      trend_gam_params[["gam_process_group_level"]] <- rbind(re_mus, re_sds)
    }
  }

  # Extract trend GP parameters
  if (!is.null(attr(object$trend_mgcv_model, 'gp_att_table'))) {
    gp_summaries <- gp_param_summary(
      object = object,
      mgcv_model = object$trend_mgcv_model,
      trend_effects = TRUE,
      digits = digits,
      variational = variational
    )

    trend_gam_params[["gam_process_gp_parameters"]] <- rbind(
      gp_summaries$alpha_summary, gp_summaries$rho_summary
    )
  }

  # Extract trend smooth tests
  if (any(!is.na(object$trend_sp_names)) && smooth_test) {
    gam_sig_table <- try(
      suppressWarnings(summary(object$trend_mgcv_model)$s.table[, c(1, 2, 3, 4), drop = FALSE]),
      silent = TRUE
    )

    if (inherits(gam_sig_table, 'try-error')) {
      object$trend_mgcv_model$R <- NULL
      gam_sig_table <- suppressWarnings(summary(object$trend_mgcv_model)$s.table[, c(1, 2, 3, 4), drop = FALSE])
      gam_sig_table[, 2] <- unlist(purrr::map(object$trend_mgcv_model$smooth, 'df'), use.names = FALSE)
    }

    # Handle trend GP terms
    if (!is.null(attr(object$trend_mgcv_model, 'gp_att_table'))) {
      gp_names <- unlist(purrr::map(attr(object$trend_mgcv_model, 'gp_att_table'), 'name'))
      if (!all(rownames(gam_sig_table) %in% gsub('gp(', 's(', gp_names, fixed = TRUE))) {
        gam_sig_table <- gam_sig_table[
          !rownames(gam_sig_table) %in% gsub('gp(', 's(', gp_names, fixed = TRUE), ,
          drop = FALSE
        ]
      } else {
        gam_sig_table <- NULL
      }
    }

    if (!is.null(gam_sig_table) && nrow(gam_sig_table) > 0) {
      trend_gam_params[["gam_process_smooth_tests"]] <- gam_sig_table
    }
  }

  return(trend_gam_params)
}
