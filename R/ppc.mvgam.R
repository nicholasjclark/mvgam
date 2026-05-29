#' Posterior Predictive Checks for \code{mvgam} models
#'
#' Perform unconditional posterior predictive checks with the help
#' of the \pkg{bayesplot} package. Returns a \pkg{ggplot2} object that
#' can be further customised.
#'
#' @aliases pp_check
#'
#' @inheritParams brms::pp_check
#' @inheritParams brms::prepare_predictions.brmsfit
#'
#' @importFrom stats terms formula weights
#' @importFrom insight get_predictors
#' @importFrom brms do_call
#' @importFrom bayesplot pp_check color_scheme_set color_scheme_get
#'
#' @param object An object of class \code{mvgam}
#'
#' @param newdata Optional \code{dataframe} or \code{list} of test data
#'   containing the variables included in the linear predictor of
#'   \code{formula}. If not supplied, predictions are generated for the
#'   original observations used for the model fit. Ignored if using one
#'   of the residual plots (i.e. 'resid_hist')
#'
#' @param ... Further arguments passed to \code{\link{predict.mvgam}}
#'   as well as to the PPC function specified in \code{type}
#'
#' @return A ggplot object that can be further
#'   customized using the \pkg{ggplot2} package.
#'
#' @details For a detailed explanation of each of the ppc functions,
#'   see the \code{\link[bayesplot:PPC-overview]{PPC}} documentation of the
#'   \pkg{\link[bayesplot:bayesplot-package]{bayesplot}} package. LOO-PIT
#'   bayesplot variants (\code{loo_pit}, \code{loo_pit_overlay},
#'   \code{loo_pit_qq}, \code{loo_intervals}, \code{loo_ribbon}) compute
#'   PSIS weights internally via [log_lik.mvgam()].
#'
#' @seealso \code{\link{predict.mvgam}}, [log_lik.mvgam()]
#'
#' @examples
#' \donttest{
#' simdat <- sim_mvgam(seasonality = "hierarchical")
#' mod <- mvgam(
#'   y ~ series +
#'     s(season, bs = "cc", k = 6) +
#'     s(season, series, bs = "fs", k = 4),
#'   data = simdat$data_train,
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # Use pp_check(mod, type = "xyz") for a list of available plot types
#'
#' # Default is a density overlay for all observations
#' pp_check(mod)
#'
#' # Rootograms particularly useful for count data
#' pp_check(mod, type = "rootogram")
#'
#' # Grouping plots by series is useful
#' pp_check(mod,
#'   type = "bars_grouped",
#'   group = "series", ndraws = 50
#' )
#' pp_check(mod,
#'   type = "ecdf_overlay_grouped",
#'   group = "series", ndraws = 50
#' )
#' pp_check(mod,
#'   type = "stat_freqpoly_grouped",
#'   group = "series", ndraws = 50
#' )
#'
#' # Several types can be used to plot distributions of randomized
#' # quantile residuals
#' pp_check(
#'   object = mod,
#'   x = "season",
#'   type = "resid_ribbon"
#' )
#' pp_check(
#'   object = mod,
#'   x = "season",
#'   group = "series",
#'   type = "resid_ribbon_grouped"
#' )
#' pp_check(mod,
#'   ndraws = 5,
#'   type = "resid_hist_grouped",
#'   group = "series"
#' )
#'
#' # PSIS-LOO bayesplot variants
#' pp_check(mod, type = "loo_pit_overlay")
#'
#' # Custom functions accepted
#' pp_check(mod, type = "stat", stat = function(x) mean(x == 0))
#' pp_check(mod,
#'   type = "stat_grouped",
#'   stat = function(x) mean(x == 0),
#'   group = "series"
#' )
#'
#' # Some functions accept covariates to set the x-axes
#' pp_check(mod,
#'   x = "season",
#'   type = "ribbon_grouped",
#'   prob = 0.5,
#'   prob_outer = 0.8,
#'   group = "series"
#' )
#'
#' # Many plots can be made without the observed data
#' pp_check(mod, prefix = "ppd")
#' }
#'
#' @export pp_check
#'
#' @author Nicholas J Clark
#'
#' @export
pp_check.mvgam <- function(
  object,
  type,
  ndraws = NULL,
  prefix = c("ppc", "ppd"),
  group = NULL,
  x = NULL,
  newdata = NULL,
  ...
) {
  # Set red colour scheme
  col_scheme <- attr(color_scheme_get(), "scheme_name")
  color_scheme_set("red")

  dots <- list(...)
  if (missing(type)) {
    type <- "dens_overlay"
  }

  prefix <- match.arg(prefix)
  ndraws_given <- "ndraws" %in% names(match.call())

  if (is.null(newdata)) {
    newdata <- object$obs_data
  }

  if (prefix == "ppc") {
    # No type checking for prefix 'ppd' yet
    valid_types <- sort(
      c(
        as.character(bayesplot::available_ppc("")),
        "ppc_resid_hist",
        "ppc_resid_hist_grouped",
        "ppc_resid_ribbon",
        "ppc_resid_ribbon_grouped"
      )
    )
    valid_types <- sub("^ppc_", "", valid_types)
    if (!type %in% valid_types) {
      stop(
        "Type '",
        type,
        "' is not a valid ppc type. ",
        "Valid types are:\n",
        paste0("'", valid_types, "'", collapse = ", "),
        call. = FALSE
      )
    }
  }

  bptype <- type

  if (bptype %in% c("resid_hist", "resid_hist_grouped")) {
    if (is.null(object$resids)) {
      object <- add_residuals(object)
    }
    bptype <- sub("resid", "error", bptype)
  }

  if (bptype %in% c("resid_ribbon", "resid_ribbon_grouped")) {
    if (is.null(object$resids)) {
      object <- add_residuals(object)
    }
    bptype <- sub("resid_", "", bptype)
  }

  ppc_fun <- get(paste0(prefix, "_", bptype), asNamespace("bayesplot"))

  family <- object$family
  if (family == "nmix") {
    stop("'pp_check' is not implemented for this family.", call. = FALSE)
  }
  valid_vars <- names(get_predictors(object))
  if ("group" %in% names(formals(ppc_fun))) {
    if (is.null(group)) {
      stop(
        "Argument 'group' is required for ppc type '",
        type,
        "'.",
        call. = FALSE
      )
    }
    if (!group %in% valid_vars) {
      stop(
        "Variable '",
        group,
        "' could not be found in the data.",
        call. = FALSE
      )
    }
  }
  if ("x" %in% names(formals(ppc_fun))) {
    if (!is.null(x) && !x %in% valid_vars) {
      stop("Variable '", x, "' could not be found in the data.", call. = FALSE)
    }
  }
  if (type == "error_binned") {
    method <- "posterior_epred"
  } else {
    method <- "posterior_predict"
  }
  if (!ndraws_given) {
    aps_types <- c(
      "error_scatter_avg",
      "error_scatter_avg_vs_x",
      "intervals",
      "intervals_grouped",
      "loo_intervals",
      "loo_pit",
      "loo_pit_overlay",
      "loo_pit_qq",
      "loo_ribbon",
      "pit_ecdf",
      "pit_ecdf_grouped",
      "ribbon",
      "ribbon_grouped",
      "rootogram",
      "scatter_avg",
      "scatter_avg_grouped",
      "stat",
      "stat_2d",
      "stat_freqpoly_grouped",
      "stat_grouped",
      "violin_grouped"
    )
    if (type %in% aps_types) {
      ndraws <- NULL
      message("Using all posterior draws for ppc type '", type, "' by default.")
    } else {
      ndraws <- 10
      message("Using 10 posterior draws for ppc type '", type, "' by default.")
    }
  }

  y <- NULL
  if (prefix == "ppc") {
    # y is ignored in prefix 'ppd' plots; get the response variable,
    # but take care that binomial models use the cbind() lhs
    resp_terms <- as.character(terms(formula(object$call))[[2]])
    if (length(resp_terms) == 1) {
      out_name <- as.character(terms(object$call)[[2]])
    } else {
      if (any(grepl("cbind", resp_terms))) {
        resp_terms <- resp_terms[-grepl("cbind", resp_terms)]
        out_name <- resp_terms[1]
      }
    }
    y <- newdata[[out_name]]
  }

  # For plotting DS residuals, set y to zero and take
  # -1 * residual so that errors are in the correct direction
  if (grepl("resid", type)) {
    y[!is.na(y)] <- 0
    yrep <- t(-1 * residuals(object, summary = FALSE))

    if (!is.null(ndraws)) {
      yrep <- yrep[1:ndraws, ]
    }
  } else {
    pred_args <- list(
      object,
      newdata = newdata,
      ndraws = ndraws,
      ...
    )
    yrep <- do_call(method, pred_args)
  }

  if (anyNA(y)) {
    warning("NA responses are not shown in 'pp_check'.")
    take <- !is.na(y)
    y <- y[take]
    yrep <- yrep[, take, drop = FALSE]
  } else {
    take <- NULL
  }

  # Prepare plotting arguments
  ppc_args <- list()
  if (prefix == "ppc") {
    ppc_args$y <- y
    ppc_args$yrep <- yrep
  } else if (prefix == "ppd") {
    ppc_args$ypred <- yrep
  }
  if (!is.null(group)) {
    if (!exists(group, newdata)) {
      stop(paste0("Variable ", group, " not in newdata"), call. = FALSE)
    }
    ppc_args$group <- newdata[[group]]

    if (!is.null(take)) {
      ppc_args$group <- ppc_args$group[take]
    }
  }

  is_like_factor <- function(x) {
    is.factor(x) || is.character(x) || is.logical(x)
  }

  if (!is.null(x)) {
    ppc_args$x <- newdata[[x]]
    if (!is_like_factor(ppc_args$x)) {
      ppc_args$x <- as.numeric(ppc_args$x)
    }

    if (!is.null(take)) {
      ppc_args$x <- ppc_args$x[take]
    }
  }

  needs_psis <- any(c("psis_object", "lw") %in%
                    setdiff(names(formals(ppc_fun)), names(ppc_args)))
  if (needs_psis) {
    ll <- log_lik(object, newdata = newdata, process_error = TRUE)
    chains <- posterior::nchains(posterior::as_draws_array(object$fit))
    n_per_chain <- NROW(ll) / chains
    r_eff <- loo::relative_eff(
      exp(ll),
      chain_id = sort(rep(seq_len(chains), n_per_chain))
    )
    psis_obj <- suppressWarnings(
      loo::psis(-ll, r_eff = r_eff)
    )
    if ("psis_object" %in% names(formals(ppc_fun))) {
      ppc_args$psis_object <- psis_obj
    }
    if ("lw" %in% names(formals(ppc_fun))) {
      ppc_args$lw <- stats::weights(psis_obj)
    }
  }

  # Most ... arguments are meant for the prediction function
  for_pred <- names(dots) %in% names(formals(posterior_predict.mvgam))
  ppc_args <- c(ppc_args, dots[!for_pred])

  # Generate plot
  out_plot <- do_call(ppc_fun, ppc_args)

  if ("x" %in% names(formals(ppc_fun)) && !is.null(x)) {
    out_plot <- out_plot +
      ggplot2::labs(x = x)
  }

  # Improve labels for residual plots
  if (type %in% c("resid_hist", "resid_hist_grouped")) {
    out_plot <- out_plot +
      ggplot2::labs(x = "DS residuals")
  }

  if (type %in% c("resid_ribbon", "resid_ribbon_grouped")) {
    out_plot <- out_plot +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(y = "DS residuals")
  }

  # Reset color scheme and return the plot
  color_scheme_set(col_scheme)
  return(out_plot)
}
