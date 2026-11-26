# The posterior latent state of each closure unit of an `nmix()` or
# `occ()` fit, which `hindcast(fit, type = "latent_state")` returns.
# Its methods give a data frame with one row per unit, ready for
# ggplot2 or dplyr.


#' Build a `mvgam_latent_state` object
#'
#' [hindcast.mvgam()] builds one for `type = "latent_state"`, with one
#' column of draws per closure unit. A multi-season design indexes
#' seasons by `time`. A single-season design uses `time` only to key
#' its units. `has_time` records which.
#'
#' @param draws Numeric matrix `[ndraws, n_unit]` of latent-state
#'   draws from `posterior_latent_N()`
#' @param unit `data.frame` with one row per closure unit, from
#'   `closure_unit_meta()`: `series` (factor), `time` and any further
#'   key columns, such as `site`. Row order matches the columns of
#'   `draws`.
#' @param family The fitted family object.
#' @param has_time Logical. `TRUE` when `time` indexes seasons or a
#'   trend, `FALSE` when it only keys units.
#'
#' @return An object of class `mvgam_latent_state`.
#'
#' @noRd
new_mvgam_latent_state <- function(draws, unit, family,
                                     has_time = FALSE) {
  checkmate::assert_matrix(draws, mode = "numeric",
                            any.missing = FALSE)
  checkmate::assert_data_frame(unit, min.rows = 1L)
  checkmate::assert_subset(c("series", "time"), names(unit))
  checkmate::assert_factor(unit$series)
  checkmate::assert_flag(has_time)
  if (ncol(draws) != nrow(unit)) {
    stop_shape_fault("The latent-state draws", ncol(draws), nrow(unit))
  }
  state <- closure_unit_labels(family)
  structure(
    list(
      draws        = draws,
      unit         = unit,
      family       = family,
      state_label  = state$label,
      state_short  = state$short,
      has_time     = has_time
    ),
    class = "mvgam_latent_state"
  )
}


#' Axis labels of a closure-unit family's state and detection
#'
#' An `nmix()` variant holds a latent abundance `N`. `occ()` holds a
#' probability of occupancy. `nmix("poisson_poisson")` models an
#' encounter rate per individual. The other families model a
#' probability of detection.
#'
#' @param family A closure-unit family object
#' @return List with the state's axis `label`, its `short` name and
#'   the `detection` axis label
#' @noRd
closure_unit_labels <- function(family) {
  detection <- if (identical(attr(family, "mvgam_nmix_type"),
                             "poisson_poisson")) {
    "Encounter rate (p)"
  } else {
    "Detection probability (p)"
  }
  c(closure_unit_state_labels(resolve_family_name(family)),
    list(detection = detection))
}


#' Axis labels of a closure-unit family's state, from the family's name
#'
#' A forecast records its family by name alone. The `nmix()` variants
#' hold a latent abundance. `occ()` holds a probability of occupancy.
#'
#' @param family_name The name `resolve_family_name()` gives.
#' @return List with the state's axis `label` and its `short` name.
#' @noRd
closure_unit_state_labels <- function(family_name) {
  checkmate::assert_string(family_name)
  if (startsWith(family_name, "nmix")) {
    list(label = "Latent abundance (N)", short = "abundance")
  } else {
    list(label = "Occupancy probability", short = "occupancy")
  }
}


#' @rdname hindcast.mvgam
#' @method summary mvgam_latent_state
#' @export
summary.mvgam_latent_state <- function(object,
                                          probs = c(0.025, 0.25,
                                                     0.75, 0.975),
                                          ...) {
  checkmate::assert_class(object, "mvgam_latent_state")
  rlang::check_dots_empty()
  latent_state_quantiles(object$draws, object$unit, probs)
}


#' The median and interval bounds of each column of latent-state draws
#'
#' @param draws `[S x n]` matrix of draws.
#' @param rows Data frame with one row per column of `draws`, whose
#'   columns lead the result.
#' @param probs The four interval bounds, lowest first.
#' @return `rows` with `median`, `lower_50`, `upper_50`, `lower_95` and
#'   `upper_95` appended.
#' @noRd
latent_state_quantiles <- function(draws, rows,
                                   probs = c(0.025, 0.25, 0.75, 0.975)) {
  checkmate::assert_matrix(draws, mode = "numeric", ncols = nrow(rows))
  checkmate::assert_numeric(probs, len = 4L, lower = 0, upper = 1,
                            sorted = TRUE, unique = TRUE)
  q <- apply(draws, 2L, stats::quantile, probs = c(0.5, probs),
             na.rm = TRUE)
  out <- rows
  out$median   <- q[1L, ]
  out$lower_50 <- q[3L, ]
  out$upper_50 <- q[4L, ]
  out$lower_95 <- q[2L, ]
  out$upper_95 <- q[5L, ]
  out
}


#' @method as.data.frame mvgam_latent_state
#' @export
as.data.frame.mvgam_latent_state <- function(x, ...) {
  summary(x, ...)
}


#' @rdname hindcast.mvgam
#' @method print mvgam_latent_state
#' @export
print.mvgam_latent_state <- function(x, digits = 2L, ...) {
  checkmate::assert_class(x, "mvgam_latent_state")
  smry   <- summary(x)
  med_range <- range(smry$median, na.rm = TRUE)
  cat("Posterior latent state at training units\n")
  cat("  Family:           ", resolve_family_name(x$family), "\n",
      sep = "")
  cat("  State:            ", x$state_label, "\n", sep = "")
  cat("  Series:           ", nlevels(x$unit$series),
      " (", paste(levels(x$unit$series), collapse = ", "), ")\n",
      sep = "")
  cat("  Units:            ", nrow(x$unit), "\n", sep = "")
  cat("  Posterior draws:  ", nrow(x$draws), "\n", sep = "")
  cat("  Median ", x$state_short, " range: [",
      round(med_range[1L], digits), ", ",
      round(med_range[2L], digits), "]\n", sep = "")
  if (!x$has_time) {
    cat("  (No time dimension: 'time' is the closure-unit index.)\n")
  }
  invisible(x)
}


#' @rdname hindcast.mvgam
#' @method plot mvgam_latent_state
#' @export
plot.mvgam_latent_state <- function(x, series = NULL, ...) {
  checkmate::assert_class(x, "mvgam_latent_state")
  rlang::check_dots_empty()
  set_color_scheme_local()
  # Several units share an occasion in a multi-season fit, one per site.
  # The plot then draws their mean in each draw.
  smry <- if (anyDuplicated(x$unit[c("series", "time")]) > 0L) {
    pooled <- pool_units_by_occasion(x$draws, x$unit)
    latent_state_quantiles(pooled$draws, pooled$occasions)
  } else {
    summary(x)
  }
  if (!is.null(series)) {
    levs <- levels(smry$series)
    keep <- levs[resolve_series(series, levs)]
    smry <- smry[smry$series %in% keep, , drop = FALSE]
    smry$series <- droplevels(smry$series)
  }
  p <- ggplot2::ggplot(smry, ggplot2::aes(x = .data$time))
  p <- if (x$has_time) {
    p +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower_95,
                                        ymax = .data$upper_95),
                           fill = mvgam_colour("light"), alpha = 0.8) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower_50,
                                        ymax = .data$upper_50),
                           fill = mvgam_colour("mid"), alpha = 0.9) +
      ggplot2::geom_line(ggplot2::aes(y = .data$median),
                         colour = mvgam_colour("dark_highlight"),
                         linewidth = 0.7)
  } else {
    p +
      ggplot2::geom_linerange(ggplot2::aes(ymin = .data$lower_95,
                                           ymax = .data$upper_95),
                              colour = mvgam_colour("light"),
                              linewidth = 0.8) +
      ggplot2::geom_linerange(ggplot2::aes(ymin = .data$lower_50,
                                           ymax = .data$upper_50),
                              colour = mvgam_colour("mid"),
                              linewidth = 1.6) +
      ggplot2::geom_point(ggplot2::aes(y = .data$median),
                          colour = mvgam_colour("dark_highlight"),
                          size = 1.5)
  }
  x_lab <- if (x$has_time) "Time" else "Closure-unit index"
  p +
    mvgam_facet_series() +
    ggplot2::labs(x = x_lab, y = x$state_label) +
    mvgam_theme()
}
