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

  if (!quiet) print(sprintf('%s of %s iterations ended with a divergence (%s%%)',
                            n, N, 100 * n / N))
  if (n > 0) {
    if (!quiet) print('  Try running with larger adapt_delta to remove the divergences')
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
    print(sprintf('%s of %s iterations saturated the maximum tree depth of %s (%s%%)',
                  n, N, max_depth, 100 * n / N))

  if (n > 0) {
    if (!quiet) print('  Run again with max_treedepth set to a larger value to avoid saturation')
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
      if (!quiet) print(sprintf('Chain %s: E-FMI = %s', n, numer / denom))
      no_warning <- FALSE
    }
  }
  if (no_warning) {
    if (!quiet) print('E-FMI indicated no pathological behavior')
    if (quiet) return(TRUE)
  } else {
    if (!quiet) print('  E-FMI below 0.2 indicates you may need to reparameterize your model')
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
  N <- dim(fit_summary)[[1]]

  iter <- dim(rstan:::extract(fit)[[1]])[[1]]

  no_warning <- TRUE
  for (n in 1:N) {
    if (is.nan(fit_summary[,'n_eff'][n])) {
      if (!quiet) print(sprintf('n_eff for parameter %s is NaN!',
                                rownames(fit_summary)[n]))
      no_warning <- FALSE
    } else {
      ratio <- fit_summary[,'n_eff'][n] / iter
      if (ratio < 0.001) {
        if (!quiet) print(sprintf('n_eff / iter for parameter %s is %s!',
                                  rownames(fit_summary)[n], ratio))
        no_warning <- FALSE
      }
    }
  }
  if (no_warning) {
    if (!quiet) print('n_eff / iter looks reasonable for all parameters')
    if (quiet) return(TRUE)
  }
  else {
    if (!quiet) print('  n_eff / iter below 0.001 indicates that the effective sample size has likely been overestimated')
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
  N <- dim(fit_summary)[[1]]

  no_warning <- TRUE
  for (n in 1:N) {
    rhat <- fit_summary[,'Rhat'][n]
    if (rhat > 1.1 || is.infinite(rhat) || is.nan(rhat)) {
      if (!quiet) print(sprintf('Rhat for parameter %s is %s!',
                                rownames(fit_summary)[n], rhat))
      no_warning <- FALSE
    }
  }
  if (no_warning) {
    if (!quiet) print('Rhat looks reasonable for all parameters')
    if (quiet) return(TRUE)
  } else {
    if (!quiet) print('  Rhat above 1.1 indicates that the chains very likely have not mixed')
    if (quiet) return(FALSE)
  }
}

#' Run all diagnostic checks
#' @param fit A stanfit object
#' @param quiet Logical (verbose or not?)
#' @details Utility function written by Michael Betancourt (https://betanalpha.github.io/)
#' @noRd
check_all_diagnostics <- function(fit, quiet=FALSE) {
  if (!quiet) {
    check_n_eff(fit)
    check_rhat(fit)
    check_div(fit)
    check_treedepth(fit)
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
    print("n_eff / iteration warning")
  if (bitwAnd(warning_code, bitwShiftL(1, 1)))
    print("rhat warning")
  if (bitwAnd(warning_code, bitwShiftL(1, 2)))
    print("divergence warning")
  if (bitwAnd(warning_code, bitwShiftL(1, 3)))
    print("treedepth warning")
  if (bitwAnd(warning_code, bitwShiftL(1, 4)))
    print("energy warning")
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
