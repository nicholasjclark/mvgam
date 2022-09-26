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
    if (!quiet) cat('*Try running with larger adapt_delta to remove the divergences\n')
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
  n = length(treedepths[sapply(treedepths, function(x) x == max_depth)])
  N = length(treedepths)

  if (!quiet)
    cat(sprintf('%s of %s iterations saturated the maximum tree depth of %s (%s%%)\n',
                  n, N, max_depth, round(100 * n / N, 4)))

  if (n > 0) {
    if (!quiet) cat('*Run again with max_treedepth set to a larger value to avoid saturation\n')
    if (quiet) return(FALSE)
  } else {
    if (quiet) return(TRUE)
  }
}

#' Check the energy fraction of missing information (E-FMI)
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
      if (!quiet) cat(sprintf('Chain %s: E-FMI = %s', n,
                              round(numer / denom, 4)))
      no_warning <- FALSE
    }
  }
  if (no_warning) {
    if (!quiet) cat('E-FMI indicated no pathological behavior\n')
    if (quiet) return(TRUE)
  } else {
    if (!quiet) cat('*E-FMI below 0.2 indicates you may need to reparameterize your model\n')
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
    fit_summary <- fit_summary[-grep('penalty', rownames(fit_summary)), ]
    fit_summary <- fit_summary[-grep('L', rownames(fit_summary)), ]
  }
  N <- dim(fit_summary)[[1]]

  iter <- dim(rstan:::extract(fit)[[1]])[[1]]

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
          'parameters\n*Effective sample size is likely overestimated for these parameters\n')
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
    fit_summary <- fit_summary[-grep('penalty', rownames(fit_summary)), ]
    fit_summary <- fit_summary[-grep('L', rownames(fit_summary)), ]
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
          'parameters\n*Diagnose further to investigate why the chains have not mixed\n')
    }
    if (quiet) return(FALSE)
  }
}

#' Run all diagnostic checks
#' @param fit A stanfit object
#' @param quiet Logical (verbose or not?)
#' @details Utility function written by Michael Betancourt (https://betanalpha.github.io/)
#' @noRd
check_all_diagnostics <- function(fit, quiet=FALSE, max_treedepth = 10) {
  sampler_params <- rstan::get_sampler_params(fit, inc_warmup=FALSE)
  fit_summary <- rstan::summary(fit, probs = c(0.5))$summary
  if (!quiet) {
    check_n_eff(fit, fit_summary = fit_summary)
    check_rhat(fit, fit_summary = fit_summary)
    check_div(fit, sampler_params = sampler_params)
    check_treedepth(fit, max_depth = max_treedepth,
                    sampler_params = sampler_params)
    check_energy(fit, sampler_params = sampler_params)
  } else {
    warning_code <- 0

    if (!check_n_eff(fit, quiet=TRUE, fit_summary = fit_summary))
      warning_code <- bitwOr(warning_code, bitwShiftL(1, 0))
    if (!check_rhat(fit, quiet=TRUE, fit_summary = fit_summary))
      warning_code <- bitwOr(warning_code, bitwShiftL(1, 1))
    if (!check_div(fit, quiet=TRUE, sampler_params = sampler_params))
      warning_code <- bitwOr(warning_code, bitwShiftL(1, 2))
    if (!check_treedepth(fit, quiet=TRUE, sampler_params = sampler_params))
      warning_code <- bitwOr(warning_code, bitwShiftL(1, 3))
    if (!check_energy(fit, quiet=TRUE, sampler_params = sampler_params))
      warning_code <- bitwOr(warning_code, bitwShiftL(1, 4))

    return(warning_code)
  }
}

#' Parse warnings
#' @param warning_code Type of warning code to generate
#' @details Utility function written by Michael Betancourt (https://betanalpha.github.io/)
#' @noRd
parse_warning_code <- function(warning_code) {
  if (bitwAnd(warning_code, bitwShiftL(1, 0)))
    cat("n_eff / iteration warning")
  if (bitwAnd(warning_code, bitwShiftL(1, 1)))
    cat("rhat warning")
  if (bitwAnd(warning_code, bitwShiftL(1, 2)))
    cat("divergence warning")
  if (bitwAnd(warning_code, bitwShiftL(1, 3)))
    cat("treedepth warning")
  if (bitwAnd(warning_code, bitwShiftL(1, 4)))
    cat("energy warning")
}

#' Return parameter arrays separated into divergent and non-divergent transitions
#' @param fit A stanfit object
#' @details Utility function written by Michael Betancourt (https://betanalpha.github.io/)
#' @noRd
partition_div <- function(fit) {
  nom_params <- rstan:::extract(fit, permuted=FALSE)
  n_chains <- dim(nom_params)[2]
  params <- as.data.frame(do.call(rbind, lapply(1:n_chains, function(n) nom_params[,n,])))

  sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
  divergent <- do.call(rbind, sampler_params)[,'divergent__']
  params$divergent <- divergent

  div_params <- params[params$divergent == 1,]
  nondiv_params <- params[params$divergent == 0,]

  return(list(div_params, nondiv_params))
}
