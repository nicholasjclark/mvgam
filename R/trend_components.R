# State components of a trend: the parts `hindcast()`, `forecast()` and
# `plot(type = "trend")` report through `component`. A trend declares
# them with `trend_components_<type>()`. One rule assembles a component
# from the summed trend and the stored paths, over the fitted grid and
# over a forecast alike.


#' The state components a fit's trend declares
#'
#' Each component is a list of `path`, `additive` and an optional
#' `less`. `path` names the Stan `matrix[N_time_trend, N_lv_trend]`
#' holding it, or is `NULL` for the one remainder component. `less`
#' names components whose paths the path includes. They are subtracted
#' from it, as the season is from `lv_trend`. The remainder is the
#' summed trend less every additive component with a path. A component
#' with `additive = FALSE` describes the state without adding to the
#' trend, as a slope does.
#'
#' @param object A fitted `mvgam` object
#' @return Named list of components, empty for a trend without any
#' @noRd
trend_components <- function(object) {
  checkmate::assert_class(object, "mvgam")
  trend_type <- get_trend_type(object)
  if (identical(trend_type, "None")) {
    return(list())
  }
  trend_method(
    trend_type, trend_method_optional_templates[["components"]]
  )(first_trend_spec(object))
}


#' The registered trends that declare state components
#'
#' @return Character vector of trend types
#' @noRd
trends_with_components <- function() {
  template <- trend_method_optional_templates[["components"]]
  Filter(function(trend_type) {
    !is.null(trend_method(trend_type, template, default = NULL))
  }, sort(ls(trend_registry)))
}


#' Check a `component` request against the fit and the prediction type
#'
#' The one validator `hindcast()`, `forecast()` and `plot()` share.
#' Several names request the sum of those components.
#'
#' @param object A fitted `mvgam` object
#' @param component The requested component names, or `NULL`
#' @param type The prediction type
#' @return `component`, or `NULL` for the summed trend
#' @noRd
resolve_trend_component <- function(object, component, type) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_character(component, min.len = 1L, any.missing = FALSE,
                              unique = TRUE, null.ok = TRUE)
  checkmate::assert_string(type)
  if (is.null(component)) {
    return(NULL)
  }
  if (!identical(type, "trend")) {
    stop(insight::format_error(c(
      "'component' requires type = 'trend'.",
      x = paste0("Got type = '", type, "'."),
      i = "A state component is part of the latent trend."
    )), call. = FALSE)
  }
  components <- trend_components(object)
  if (length(components) == 0L) {
    trend_type <- get_trend_type(object)
    what <- if (identical(trend_type, "None")) {
      "a fit without a trend"
    } else {
      paste0("a '", trend_type, "()' trend")
    }
    stop(insight::format_error(c(
      paste0("'component' is not supported for ", what, "."),
      i = paste0("Trends with state components: ",
                 paste0("'", trends_with_components(), "()'",
                        collapse = ", "), ".")
    )), call. = FALSE)
  }
  unknown <- setdiff(component, names(components))
  if (length(unknown)) {
    stop(insight::format_error(c(
      paste0("Unknown 'component': '", unknown[1L], "'."),
      i = paste0("The '", get_trend_type(object), "()' components: ",
                 paste0("'", names(components), "'", collapse = ", "),
                 ".")
    )), call. = FALSE)
  }
  component
}


#' The printed name of a component request
#'
#' @param component Component names
#' @return One string, the names joined by `" + "`
#' @noRd
component_label <- function(component) {
  checkmate::assert_character(component, min.len = 1L)
  paste(component, collapse = " + ")
}


#' The additive components that have a path
#'
#' @param components Named list from `trend_components()`
#' @return Character vector of component names
#' @noRd
additive_paths <- function(components) {
  names(Filter(function(x) isTRUE(x$additive) && !is.null(x$path),
               components))
}


#' Whether a request needs the summed trend
#'
#' @param components Named list from `trend_components()`
#' @param component Component names, or `NULL` for the summed trend
#' @return A single logical
#' @noRd
needs_total <- function(components, component) {
  is.null(component) ||
    any(vapply(component, function(nm) is.null(components[[nm]]$path),
               logical(1L)))
}


#' The stored paths a request is assembled from
#'
#' A component with a path takes it and the paths of its `less`. The
#' remainder takes the same for every additive component with a path.
#'
#' @param components Named list from `trend_components()`
#' @param component Component names, or `NULL` for the summed trend
#' @return Character vector of component names
#' @noRd
component_parts <- function(components, component) {
  checkmate::assert_list(components)
  checkmate::assert_character(component, null.ok = TRUE)
  own <- function(nm) c(nm, components[[nm]]$less)
  unique(unlist(lapply(component, function(nm) {
    if (is.null(components[[nm]]$path)) {
      unlist(lapply(additive_paths(components), own))
    } else {
      own(nm)
    }
  })))
}


#' The values of a request from the summed trend and the stored paths
#'
#' A component with a path is its path less its `less`. The remainder
#' is the summed trend less every additive component with a path.
#' Several components sum. The arithmetic holds on any shape, which
#' serves the hindcast's `[ndraws, nrow]` matrices and a forecast
#' draw's `[h, n_series]` grid.
#'
#' @param total The summed trend, or `NULL` when every requested
#'   component has a path
#' @param parts Named list of the paths `component_parts()` names, each
#'   the shape of `total`
#' @param component Component names, or `NULL` for the summed trend
#' @param components Named list from `trend_components()`
#' @return The values of the request
#' @noRd
trend_component_values <- function(total, parts, component, components) {
  checkmate::assert_list(parts)
  if (is.null(component)) {
    return(total)
  }
  value <- function(nm) {
    less <- components[[nm]]$less
    if (!is.null(components[[nm]]$path)) {
      return(Reduce(`-`, parts[less], parts[[nm]]))
    }
    Reduce(`-`, lapply(additive_paths(components), value), total)
  }
  Reduce(`+`, lapply(component, value))
}


#' The draws of a stored path at the series grain
#'
#' A trend without factors stores the path by series. A factor fit
#' stores it by factor. Each draw's sampled loadings then project it,
#' as they project the trend.
#'
#' @param object A fitted `mvgam` object
#' @param path Name of a Stan `matrix[N_time_trend, N_lv_trend]`
#' @param draw_idx Indices of the draws to keep
#' @return Draws matrix, one row per draw, with columns `path[t, s]`
#' @noRd
series_path_draws <- function(object, path, draw_idx) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_string(path)
  checkmate::assert_integerish(draw_idx, lower = 1L, min.len = 1L)
  n_series <- as.integer(object$standata$N_series_trend)
  if (is.null(detect_factor_n_lv(object, n_series))) {
    return(subset_draws_rows(fit_draws(object$fit, path),
                             draw_ids = draw_idx))
  }
  draws <- subset_draws_rows(fit_draws(object$fit, c(path, "Z", "Z_tilde")),
                             draw_ids = draw_idx)
  n_time <- as.integer(object$standata$N_time_trend)
  n_lv <- as.integer(object$standata$N_lv_trend)
  Z <- resolve_factor_loadings(object, draws_mat = draws, n_lv = n_lv,
                               n_series = n_series, basis = "model")
  out <- project_lv_grid(
    as_plain_matrix(draws[, stan_matrix_names(path, n_time, n_lv),
                          drop = FALSE]),
    Z, n_time, n_lv, n_series
  )
  colnames(out) <- stan_matrix_names(path, n_time, n_series)
  out
}


#' The fitted trend, or one of its components, at given rows
#'
#' @param object A fitted `mvgam` object
#' @param newdata Rows of the training frame
#' @param draw_idx Indices of the draws to keep
#' @param resp A response name, or NULL
#' @param component Component names, or `NULL` for the summed trend
#' @return `[length(draw_idx), nrow(newdata)]` matrix, or `NULL` for a
#'   fit without a trend
#' @noRd
trend_state_rows <- function(object, newdata, draw_idx, resp = NULL,
                             component = NULL) {
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_character(component, null.ok = TRUE)
  components <- if (is.null(component)) list() else trend_components(object)
  at_rows <- function(draws, state) {
    extract_trend_latent_states(object, newdata, full_draws = draws,
                                resp = resp, state = state)
  }
  parts <- lapply(
    stats::setNames(nm = component_parts(components, component)),
    function(nm) {
      path <- components[[nm]]$path
      vals <- at_rows(series_path_draws(object, path, draw_idx), path)
      # A fit without a trend reaches no component. A declared path the
      # draws lack would otherwise give a hindcast of zeros.
      if (is.null(vals)) {
        stop_mvgam_fault(
          paste0("The draws must hold the '", nm, "' component."),
          paste0("'", path, "' is absent from the fit's draws.")
        )
      }
      vals
    }
  )
  total <- if (needs_total(components, component)) {
    at_rows(subset_draws_rows(fit_draws(object$fit, "trend"),
                              draw_ids = draw_idx), "trend")
  }
  trend_component_values(total, parts, component, components)
}
