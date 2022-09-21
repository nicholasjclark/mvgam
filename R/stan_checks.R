#' Check transitions that ended with a divergence
#' @param fit A stanfit object
#' @param quiet Logical (verbose or not?)
#' @details Utility function written by Michael Betancourt (https://betanalpha.github.io/)
#' @noRd
check_div <- function(fit, quiet=FALSE) {
  sampler_params <- rstan::get_sampler_params(fit, inc_warmup=FALSE)
  divergent <- do.call(rbind, sampler_params)[,'divergent__']
  n = sum(divergent)
  N = length(divergent)

  if (!quiet) cat(sprintf('%s of %s iterations ended with a divergence (%s%%)\n',
                            n, N, 100 * n / N))
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
check_treedepth <- function(fit, max_depth = 10, quiet=FALSE) {
  sampler_params <- rstan::get_sampler_params(fit, inc_warmup=FALSE)
  treedepths <- do.call(rbind, sampler_params)[,'treedepth__']
  n = length(treedepths[sapply(treedepths, function(x) x == max_depth)])
  N = length(treedepths)

  if (!quiet)
    cat(sprintf('%s of %s iterations saturated the maximum tree depth of %s (%s%%)\n',
                  n, N, max_depth, 100 * n / N))

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
check_energy <- function(fit, quiet=FALSE) {
  sampler_params <- rstan::get_sampler_params(fit, inc_warmup=FALSE)
  no_warning <- TRUE
  for (n in 1:length(sampler_params)) {
    energies = sampler_params[n][[1]][,'energy__']
    numer = sum(diff(energies)**2) / length(energies)
    denom = var(energies)
    if (numer / denom < 0.2) {
      if (!quiet) cat(sprintf('Chain %s: E-FMI = %s', n, numer / denom))
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
check_n_eff <- function(fit, quiet=FALSE) {
  fit_summary <- rstan::summary(fit, probs = c(0.5))$summary
  if(any(grep('LV', rownames(fit_summary)))){
    fit_summary <- fit_summary[-grep('LV', rownames(fit_summary)), ]
    fit_summary <- fit_summary[-grep('lv_coefs', rownames(fit_summary)), ]
    fit_summary <- fit_summary[-grep('penalty', rownames(fit_summary)), ]
    fit_summary <- fit_summary[-grep('L', rownames(fit_summary)), ]
  }
  N <- dim(fit_summary)[[1]]

  iter <- dim(rstan:::extract(fit)[[1]])[[1]]

  no_warning <- TRUE
  for (n in 1:N) {
    if(is.nan(fit_summary[,'n_eff'][n])){
      ratio <- 1
    } else {
      ratio <- fit_summary[,'n_eff'][n] / iter
    }
    if (ratio < 0.001) {
      if (!quiet) cat(sprintf('n_eff / iter for parameter %s is %s!\n',
                                rownames(fit_summary)[n], ratio))
      no_warning <- FALSE
    }

  }
  if (no_warning) {
    if (!quiet) cat('n_eff / iter looks reasonable for all parameters\n')
    if (quiet) return(TRUE)
  }
  else {
    if (!quiet) cat('*n_eff / iter below 0.001 indicates that the effective sample size has likely been overestimated\n')
    if (quiet) return(FALSE)
  }
}

#' Check the potential scale reduction factors
#' @param fit A stanfit object
#' @param quiet Logical (verbose or not?)
#' @details Utility function written by Michael Betancourt (https://betanalpha.github.io/)
#' @noRd
check_rhat <- function(fit, quiet=FALSE) {
  fit_summary <- rstan::summary(fit, probs = c(0.5))$summary
  if(any(grep('LV', rownames(fit_summary)))){
    fit_summary <- fit_summary[-grep('LV', rownames(fit_summary)), ]
    fit_summary <- fit_summary[-grep('lv_coefs', rownames(fit_summary)), ]
    fit_summary <- fit_summary[-grep('penalty', rownames(fit_summary)), ]
    fit_summary <- fit_summary[-grep('L', rownames(fit_summary)), ]
  }
  N <- dim(fit_summary)[[1]]

  no_warning <- TRUE
  rhats <- fit_summary[,'Rhat']
  if(any(rhats > 1.1)) no_warning <- FALSE
  if (no_warning) {
    if (!quiet) cat('Rhat looks reasonable for all parameters\n')
    if (quiet) return(TRUE)
  } else {
    if (!quiet){
      cat('Rhats above 1.1 found for',
          length(which(rhats > 1.1)),
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
  if (!quiet) {
    check_n_eff(fit)
    check_rhat(fit)
    check_div(fit)
    check_treedepth(fit, max_depth = max_treedepth)
    check_energy(fit)
  } else {
    warning_code <- 0

    if (!check_n_eff(fit, quiet=TRUE))
      warning_code <- bitwOr(warning_code, bitwShiftL(1, 0))
    if (!check_rhat(fit, quiet=TRUE))
      warning_code <- bitwOr(warning_code, bitwShiftL(1, 1))
    if (!check_div(fit, quiet=TRUE))
      warning_code <- bitwOr(warning_code, bitwShiftL(1, 2))
    if (!check_treedepth(fit, quiet=TRUE))
      warning_code <- bitwOr(warning_code, bitwShiftL(1, 3))
    if (!check_energy(fit, quiet=TRUE))
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
