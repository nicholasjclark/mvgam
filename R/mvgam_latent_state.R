# mvgam_latent_state: posterior of the closure-unit latent state
# (N for nmix variants, psi for occ()) at the training unit grid.
# Produced by hindcast(fit, type = "latent_state"); standalone class
# with its own print / summary / as.data.frame / plot methods so
# users get a tidy frame to pipe into ggplot or dplyr without
# wrestling with the per-series matrix-list layout that
# `mvgam_forecast` uses for time-series quantities.


#' Constructor for the mvgam_latent_state object
#'
#' Internal builder used by [hindcast.mvgam()] when
#' `type = "latent_state"`. Closure-unit fits collapse the per-visit
#' rows down to a per-unit grid; the latent state lives on that
#' grid (one value per `(series, unit)` cell). Some closure-unit
#' designs have a real time axis (multi-season occupancy, repeated
#' visits across seasons), but many do not (single-season occupancy
#' / N-mixture where `time` is just the unit identifier), so the
#' object stores the unit grid and a `has_time` logical rather than
#' assuming a temporal axis.
#'
#' @param draws Numeric matrix `[ndraws, n_unit]` of latent-state
#'   posterior draws, in the row order returned by the
#'   family-specific kernel (`posterior_latent_N()` /
#'   `posterior_occupancy()`).
#' @param unit `data.frame` with one row per closure unit and
#'   columns `series` (factor) and `time` (integer / numeric). Row
#'   order matches the columns of `draws`.
#' @param family The fitted family object.
#' @param state_label Character; the human label for the state
#'   (e.g. `"Latent abundance (N)"` or `"Occupancy probability (psi)"`).
#' @param state_short Character; short symbol for the state used in
#'   print output (e.g. `"N"`, `"psi"`).
#' @param has_time Logical. `TRUE` when the fit carries a real time
#'   dimension; `FALSE` when `time` is just the unit index.
#'
#' @return An object of class `mvgam_latent_state`.
#'
#' @noRd
new_mvgam_latent_state <- function(draws, unit, family,
                                     state_label, state_short,
                                     has_time = FALSE) {
  checkmate::assert_matrix(draws, mode = "numeric",
                            any.missing = FALSE)
  checkmate::assert_data_frame(unit, min.rows = 1L)
  checkmate::assert_subset(c("series", "time"), names(unit))
  checkmate::assert_factor(unit$series)
  checkmate::assert_string(state_label)
  checkmate::assert_string(state_short)
  checkmate::assert_flag(has_time)
  if (ncol(draws) != nrow(unit)) {
    stop(insight::format_error(c(
      "Draws / unit grid size mismatch.",
      x = paste0("ncol(draws) = ", ncol(draws),
                  ", nrow(unit) = ", nrow(unit), ".")
    )))
  }
  structure(
    list(
      draws        = draws,
      unit         = unit,
      family       = family,
      state_label  = state_label,
      state_short  = state_short,
      has_time     = has_time
    ),
    class = "mvgam_latent_state"
  )
}


#' Per-unit posterior summary for a `mvgam_latent_state` object
#'
#' Collapses the `[ndraws, n_unit]` draw matrix to one row per unit
#' carrying the posterior median plus 50% and 95% credible interval
#' endpoints. The returned `data.frame` is the same one that
#' `as.data.frame.mvgam_latent_state()` returns and that
#' [plot.mvgam_latent_state()] uses internally.
#'
#' @param object A `mvgam_latent_state` object.
#' @param probs Numeric vector of length 4 giving the lower /
#'   inner-lower / inner-upper / upper interval probabilities.
#'   Defaults to `c(0.025, 0.25, 0.75, 0.975)` (50% and 95%
#'   intervals).
#' @param ... Currently unused.
#'
#' @return A `data.frame` with columns `series`, `time`, `median`,
#'   `lower_50`, `upper_50`, `lower_95`, `upper_95`.
#'
#' @method summary mvgam_latent_state
#' @export
summary.mvgam_latent_state <- function(object,
                                          probs = c(0.025, 0.25,
                                                     0.75, 0.975),
                                          ...) {
  checkmate::assert_class(object, "mvgam_latent_state")
  checkmate::assert_numeric(probs, len = 4L, lower = 0, upper = 1,
                             sorted = TRUE, unique = TRUE)
  q <- apply(object$draws, 2L,
              stats::quantile, probs = c(0.5, probs),
              na.rm = TRUE)
  out <- object$unit
  out$median   <- q[1L, ]
  out$lower_95 <- q[2L, ]
  out$lower_50 <- q[3L, ]
  out$upper_50 <- q[4L, ]
  out$upper_95 <- q[5L, ]
  out[c("series", "time", "median",
         "lower_50", "upper_50", "lower_95", "upper_95")]
}


#' @method as.data.frame mvgam_latent_state
#' @export
as.data.frame.mvgam_latent_state <- function(x, ...) {
  summary(x, ...)
}


#' Short print for a `mvgam_latent_state` object
#'
#' @param x A `mvgam_latent_state` object.
#' @param digits Integer; rounding for the posterior-median range
#'   shown in the print header. Defaults to 2.
#' @param ... Currently unused.
#'
#' @return The `mvgam_latent_state` object `x`, returned invisibly.
#'
#' @method print mvgam_latent_state
#' @export
print.mvgam_latent_state <- function(x, digits = 2L, ...) {
  checkmate::assert_class(x, "mvgam_latent_state")
  fam_nm <- resolve_family_name(x$family) %||% "?"
  smry   <- summary(x)
  med_range <- range(smry$median, na.rm = TRUE)
  cat("Posterior latent state at training units\n")
  cat("  Family:           ", fam_nm, "\n", sep = "")
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


#' Plot the posterior latent state
#'
#' Faceted ribbon plot over the unit / time axis, with the
#' posterior median, 50% credible interval, and 95% credible
#' interval per series. For integer-valued state (`nmix` variants)
#' the ribbon is drawn step-style to match the discrete domain; for
#' continuous state (`occ()` posterior occupancy probability) the
#' ribbon is drawn smooth.
#'
#' @param x A `mvgam_latent_state` object.
#' @param series Optional integer / character; restrict to a subset
#'   of the series. Defaults to all.
#' @param ... Currently unused.
#'
#' @return A `ggplot` object.
#'
#' @method plot mvgam_latent_state
#' @export
plot.mvgam_latent_state <- function(x, series = NULL, ...) {
  checkmate::assert_class(x, "mvgam_latent_state")
  # Pin the scheme the way every other mvgam figure does, so a
  # user's own `bayesplot::color_scheme_set()` does not leave this
  # panel the odd one out.
  set_color_scheme_local("red")
  smry <- summary(x)
  if (!is.null(series)) {
    keep <- if (is.numeric(series)) {
      levels(smry$series)[as.integer(series)]
    } else {
      as.character(series)
    }
    smry <- smry[smry$series %in% keep, , drop = FALSE]
    smry$series <- droplevels(smry$series)
  }
  is_step <- identical(x$state_short, "N")
  x_lab   <- if (x$has_time) "Time" else "Closure-unit index"
  if (is_step) {
    p <- ggplot2::ggplot(smry, ggplot2::aes(x = .data$time)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower_95,
                                          ymax = .data$upper_95),
                            fill = mvgam_colour("light"), alpha = 0.8,
                            stat = "identity") +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower_50,
                                          ymax = .data$upper_50),
                            fill = mvgam_colour("mid"), alpha = 0.9,
                            stat = "identity") +
      ggplot2::geom_step(ggplot2::aes(y = .data$median),
                          colour = mvgam_colour("dark_highlight"),
                          linewidth = 0.7)
  } else {
    p <- ggplot2::ggplot(smry, ggplot2::aes(x = .data$time)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower_95,
                                          ymax = .data$upper_95),
                            fill = mvgam_colour("light"), alpha = 0.8) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower_50,
                                          ymax = .data$upper_50),
                            fill = mvgam_colour("mid"), alpha = 0.9) +
      ggplot2::geom_line(ggplot2::aes(y = .data$median),
                          colour = mvgam_colour("dark_highlight"),
                          linewidth = 0.7)
  }
  p +
    ggplot2::facet_wrap(~ series, scales = "free_y") +
    ggplot2::labs(x = x_lab, y = x$state_label) +
    mvgam_theme()
}
