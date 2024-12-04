#' Generate a methods description for mvgam models
#'
#' Create a brief but fully referenced methods description, along with a useful list of references,
#' for fitted \code{mvgam} and \code{jsdgam} models
#'
#'@name how_to_cite.mvgam
#'@param object \code{list} object of class \code{mvgam} resulting from a call to [mvgam()]
#' or [jsdgam()]
#'@param ... ignored
#'@details This function uses the model's structure to come up with a very basic
#'but hopefully useful methods description that can help users to appropriately acknowledge
#'the hard work of developers and champion open science. Please do not consider the
#'text returned by this function to be a completely adequate methods section, it is only
#'meant to get you started.
#'@return An object of class \code{how_to_cite} containing a text description of the
#'methods as well as lists of both primary and additional references
#'@author Nicholas J Clark
#'@seealso \code{\link[utils]{citation}}, \code{\link{mvgam}}, \code{\link{jsdgam}}
#' @examples
#' \dontrun{
#' # Simulate 4 time series with hierarchical seasonality
#' # and a VAR(1) dynamic process
#' set.seed(0)
#' simdat <- sim_mvgam(seasonality = 'hierarchical',
#'                     trend_model = VAR(cor = TRUE),
#'                     family = gaussian())
#'
#' # Fit an appropriate model
#' mod1 <- mvgam(y ~ s(season, bs = 'cc', k = 6),
#'               data = simdat$data_train,
#'               family = gaussian(),
#'               trend_model = VAR(cor = TRUE),
#'               chains = 2)
#' how_to_cite(mod1)
#'
#' # For a GP example, simulate data using the mgcv package
#' dat <- mgcv::gamSim(1, n = 30, scale = 2)
#'
#' # Fit a model that uses an approximate GP from the brms package
#' mod2 <- mvgam(y ~ gp(x2, k = 12),
#'               data = dat,
#'               family = gaussian(),
#'               chains = 2)
#' how_to_cite(mod2)
#'
#' # Repeat using meanfield variational inference
#' mod3 <- mvgam(y ~ gp(x2, k = 12),
#'               data = dat,
#'               family = gaussian(),
#'               algorithm = 'meanfield')
#' how_to_cite(mod3)
#' }
#'@export
how_to_cite <- function(object, ...){
  UseMethod("how_to_cite", object)
}

#'@export
#'@export
print.how_to_cite = function(x, ...){
  cat('Methods text skeleton\n')
  cat(x$methods_text, sep = '\n')
  cat('\nPrimary references\n')
  print(unlist(x$citations))
  cat('\nOther useful references\n')
  print(unlist(x$other_citations))
  invisible(x)
}

#'@rdname how_to_cite.mvgam
#'@method how_to_cite mvgam
#'@export
how_to_cite.mvgam <- function(object, ...){
  current_year <- format(Sys.Date(), "%Y")
  citations <- vector(mode = 'list')

  # mvgam-specific methods
  mvgam_text <- paste0(
    "We used the R package mvgam (version ",
    utils::packageVersion("mvgam"),
    "; Clark & Wells, 2023) to construct, fit and interrogate the model.",
    " mvgam fits Bayesian State-Space models that can include flexible",
    " predictor effects in both the process and observation components",
    " by incorporating functionalities from the brms (Burkner 2017),",
    " mgcv (Wood 2017) and splines2 (Wang & Yan, 2023) packages."
  )

  citations[[1]] <- "Clark, NJ and Wells K (2022). Dynamic Generalized Additive Models (DGAMs) for forecasting discrete ecological time series. Methods in Ecology and Evolution, 14, 771-784. doi.org/10.1111/2041-210X.13974"
  citations[[2]] <- "Burkner, PC (2017). brms: An R Package for Bayesian Multilevel Models Using Stan. Journal of Statistical Software, 80(1), 1-28. doi:10.18637/jss.v080.i01"
  citations[[3]] <- "Wood, SN (2017). Generalized Additive Models: An Introduction with R (2nd edition). Chapman and Hall/CRC."
  citations[[4]] <- "Wang W and Yan J (2021). Shape-Restricted Regression Splines with R Package splines2. Journal of Data Science, 19(3), 498-517. doi:10.6339/21-JDS1020 https://doi.org/10.6339/21-JDS1020."

  # Any specials; first check whether this model used a VAR / VARMA process
  specials_text <- NULL
  trend_model <- attr(object$model_data, 'trend_model')
  if(trend_model %in% c('VAR',
                        'VARcor',
                        'VARhiercor',
                        'VAR1',
                        'VAR1cor',
                        'VAR1hiercor',
                        'VARMA',
                        'VARMAcor',
                        'VARMA1,1cor')){
    specials_text <- c(
      specials_text,
      " To encourage stability and prevent forecast variance from increasing indefinitely, we enforced stationarity of the Vector Autoregressive process following methods described in Heaps (2023)."
    )
    citations <- append(
      citations,
      list("Heaps, SE (2023). Enforcing stationarity through the prior in vector autoregressions. Journal of Computational and Graphical Statistics 32, 74-83.")
    )
  }

  # Check for approximate GPs
  if(!is.null(attr(object$mgcv_model, 'gp_att_table')) |
     !is.null(attr(object$trend_mgcv_model, 'gp_att_table'))){
    specials_text <- c(
      specials_text,
      " Gaussian Process functional effects were estimated using a low-rank Hilbert space approximation following methods described in Riutort-Mayol et al. (2023)."
    )
    citations <- append(
      citations,
      list("Riutort-Mayol, G, Burkner, PC, Andersen, MR, Solin, A and Vehtari, A (2023). Practical Hilbert space approximate Bayesian Gaussian processes for probabilistic programming. Statistics and Computing 33, 1. https://doi.org/10.1007/s11222-022-10167-2")
    )
  }

  # Stan-specific methods
  citations <- append(citations, list("Carpenter, B, Gelman, A, Hoffman, MD, Lee, D, Goodrich, B, Betancourt, M, Brubaker, M, Guo, J, Li, P and Riddell, A (2017). Stan: A probabilistic programming language. Journal of Statistical Software 76."))

  stan_text <-
    paste0(
      " The mvgam-constructed model and observed data",
      " were passed to the probabilistic programming environment Stan"
    )

  if(object$backend == 'cmdstanr'){
    stan_text <- paste0(
      stan_text,
      " (version ",
      cmdstanr::cmdstan_version(),
      "; Carpenter et al. 2017, Stan Development Team ",
      current_year,
      "), specifically through the cmdstanr interface (Gabry & Cesnovar, 2021)."
    )
    citations <- append(
      citations,
      list(paste0(
        "Gabry J, Cesnovar R, Johnson A, and Bronder S (",
        current_year,
        "). cmdstanr: R Interface to 'CmdStan'. https://mc-stan.org/cmdstanr/, https://discourse.mc-stan.org."
      )
      )
    )
  } else {
    stan_text <- paste0(
      stan_text,
      " (version ",
      rstan::stan_version(),
      "; Carpenter et al. 2017)",
      ", specifically through the rstan interface (Stan Development Team ",
      current_year,
      ")"
    )
    citations <- append(
      citations,
      list(paste0(
        "Stan Development Team (",
        current_year,
        "). RStan: the R interface to Stan. R package version ",
        utils::packageVersion("rstan"),
        ". https://mc-stan.org/."
      )
      ))
  }

  if(object$algorithm == 'sampling'){
    stan_text <- paste0(
      stan_text,
      " We ran ",
      object$model_output@sim$chains,
      " Hamiltonian Monte Carlo chains for ",
      object$model_output@sim$warmup,
      " warmup iterations and ",
      object$model_output@sim$iter - object$model_output@sim$warmup,
      " sampling iterations for joint posterior estimation.",
      " Rank normalized split Rhat (Vehtari et al. 2021) and effective",
      " sample sizes were used to monitor convergence."
    )
    citations <- append(
      citations,
      list("Vehtari A, Gelman A, Simpson D, Carpenter B, and Burkner P (2021). Rank-normalization, folding, and localization: An improved Rhat for assessing convergence of MCMC (with discussion). Bayesian Analysis 16(2) 667-718. https://doi.org/10.1214/20-BA1221.")
    )
  }

  if(object$algorithm %in% c('meanfield', 'fullrank')){
    stan_text <- paste0(
      stan_text,
      " We used Stan's Automatic Differentiation Variational Inference algorithm",
      " (Kucukelbir et al. 2017) for posterior approximation, specifically using ",
      object$algorithm,
      " algorithm to draw ",
      object$model_output@sim$iter,
      " samples from the approximate joint posterior."
    )
    citations <- append(
      citations,
      list("Kucukelbir, A, Tran, D, Ranganath, R, Gelman, A, and Blei, DM (2017). Automatic Differentiation Variational Inference. Journal of Machine Learning Research 18 1-45.")
    )
  }

  if(object$algorithm == c('laplace')){
    stan_text <- paste0(
      stan_text,
      " We used Stan's Laplace approximation algorithm",
      " to draw ",
      object$model_output@sim$iter,
      " samples from the approximate joint posterior."
    )

  }

  if(object$algorithm == c('pathfinder')){
    stan_text <- paste0(
      stan_text,
      " We used Stan's Pathfinder variational approximation algorithm (Zhang et al. 2022)",
      " to draw ",
      object$model_output@sim$iter,
      " samples from the approximate joint posterior."
    )

    citations <- append(
      citations,
      list("Zhang, L, Carpenter, B, Gelman, A, and Vehtari, A (2022). Pathfinder: parallel Quasi-Newton variational inference. Journal of Machine Learning Research 23(306), 1-49. http://jmlr.org/papers/v23/21-0889.html.")
    )
  }
  # Append texts
  all_text <- paste0(mvgam_text, specials_text, stan_text)

  # List of additional, possibly very useful references
  other_citations <- vector(mode = 'list')
  other_citations[[1]] <- "Arel-Bundock, V, Greifer, N, and Heiss, A (2024). How to interpret statistical models using marginaleffects for R and Python. Journal of Statistical Software, 111(9), 1-32. https://doi.org/10.18637/jss.v111.i09"
  other_citations[[2]] <- "Gabry J, Simpson D, Vehtari A, Betancourt M, and Gelman A (2019). Visualization in Bayesian workflow. Journal of the Royal Statatistical Society A, 182, 389-402. doi:10.1111/rssa.12378."
  other_citations[[3]] <- "Vehtari A, Gelman A, and Gabry J (2017). Practical Bayesian model evaluation using leave-one-out cross-validation and WAIC. Statistics and Computing, 27, 1413-1432. doi:10.1007/s11222-016-9696-4."
  other_citations[[4]] <- "Burkner, PC, Gabry, J, and Vehtari, A. (2020). Approximate leave-future-out cross-validation for Bayesian time series models. Journal of Statistical Computation and Simulation, 90(14), 2499-2523. https://doi.org/10.1080/00949655.2020.1783262"

  out <- structure(
    list(
      methods_text = all_text,
      citations = citations,
      other_citations = other_citations
    ),
    class = 'how_to_cite'
  )

  return(out)
}




