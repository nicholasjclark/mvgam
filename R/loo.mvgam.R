#' LOO information criteria for `mvgam` models
#'
#' Extract the LOOIC (leave-one-out information criterion) using
#' [loo::loo()]
#' @importFrom loo loo is.loo
#' @param x Object of class `mvgam`
#' @param ... Additional arguments for [loo::loo()]
#' @rdname loo.mvgam
#' @return  for `loo.mvgam`, an object of class `psis_loo` (see [loo::loo()]
#' for details). For `loo_compare.mvgam`, an object of class `compare.loo` (
#' [loo::loo_compare()] for details)
#'@examples
#'\dontrun{
#'# Simulate 4 time series with hierarchical seasonality
#'# and independent AR1 dynamic processes
#'set.seed(111)
#'simdat <- sim_mvgam(seasonality = 'hierarchical',
#'                    trend_model = AR(),
#'                    family = gaussian())
#'
#'# Fit a model with shared seasonality
#'mod1 <- mvgam(y ~ s(season, bs = 'cc', k = 6),
#'              data = rbind(simdat$data_train,
#'              simdat$data_test),
#'              family = gaussian(),
#'              chains = 2)
#'
#'# Inspect the model and calculate LOO
#'plot(mod1, type = 'smooths')
#'mc.cores.def <- getOption('mc.cores')
#'options(mc.cores = 1)
#'loo(mod1)
#'
#'# Now fit a model with hierarchical seasonality
#'mod2 <- update(mod1,
#'               formula = y ~ s(season, bs = 'cc', k = 6) +
#'               s(season, series, bs = 'fs',
#'               xt = list(bs = 'cc'), k = 4),
#'               chains = 2)
#'plot(mod2, type = 'smooths')
#'loo(mod2)
#'
#'# Now add AR1 dynamic errors to mod2
#'mod3 <- update(mod2,
#'               trend_model = AR(),
#'               chains = 2)
#'plot(mod3, type = 'smooths')
#'plot(mod3, type = 'trend')
#'loo(mod3)
#'
#'# Compare models using LOO
#'loo_compare(mod1, mod2, mod3)
#'options(mc.cores = mc.cores.def)
#'}
#' @export
loo.mvgam <- function(x, ...) {
  x$series_names <- levels(x$obs_data$series)
  logliks <- logLik(x,
                    linpreds = predict(x,
                                       newdata = x$obs_data,
                                       type = 'link',
                                       summary = FALSE,
                                       process_error = TRUE),
                    newdata = x$obs_data,
                    family_pars = extract_family_pars(x),
                    include_forecast = FALSE)
  logliks <- logliks[,!apply(logliks, 2, function(x) all(!is.finite(x)))]

  # Remove any remaining non-finite values (very occasionally happens with
  # some observation families)
  min_noinf = function(x){
    x <- x[is.finite(x)]
    min(x)
  }
  logliks[!is.finite(logliks)] <- min_noinf(logliks)
  releffs <- loo::relative_eff(exp(logliks),
                               chain_id = sort(rep(1:x$model_output@sim$chains,
                                                   (NROW(logliks) /
                                                      x$model_output@sim$chains))))
  loo::loo(logliks, r_eff = releffs, ...)
}

#' @importFrom loo loo_compare
#' @param x Object of class `mvgam`
#' @param ... More \code{mvgam} objects.
#' @param model_names If `NULL` (the default) will use model names derived
#' from deparsing the call. Otherwise will use the passed values as model names.
#' @rdname loo.mvgam
#' @export
loo_compare.mvgam <- function(x, ...,
                              model_names = NULL) {

  models <- split_mod_dots(x, ..., model_names = model_names)
  loos <- named_list(names(models))
  for (i in seq_along(models)) {
    loos[[i]] <- loo(models[[i]])
  }
  loo_compare(loos)
}

#'@noRd
split_mod_dots = function (x, ..., model_names = NULL, other = TRUE) {

  dots <- list(x, ...)
  names <- substitute(list(x, ...), env = parent.frame())[-1]
  names <- ulapply(names, deparse)

  if(!is.null(model_names)){
    names <- model_names
  }

  if (length(names)) {
    if (!length(names(dots))) {
      names(dots) <- names
    }
    else {
      has_no_name <- !nzchar(names(dots))
      names(dots)[has_no_name] <- names[has_no_name]
    }
  }
  is_mvgam <- unlist(lapply(dots, function(y) inherits(y, 'mvgam')))
  models <- dots[is_mvgam]
  out <- dots[!is_mvgam]

  if (length(out)) {
    stop("Only model objects can be passed to '...' for this method.",
         call. = FALSE)
  }
  models
}

#'@noRd
named_list = function (names, values = NULL) {
  if (!is.null(values)) {
    if (length(values) <= 1L) {
      values <- replicate(length(names), values)
    }
    values <- as.list(values)
    stopifnot(length(values) == length(names))
  }
  else {
    values <- vector("list", length(names))
  }
  setNames(values, names)
}
