# Shared ggplot building blocks for every plot method in mvgam.
# Defines the palette, theme, ribbon stacking,
# observation overlays, faceting, and label-repel wrappers.
# Nothing in this file is exported.

#' A bayesplot colour scheme, by name or the active one
#'
#' The plot methods set the `"red"` scheme with
#' `set_color_scheme_local()`. A helper they call takes its colours from
#' the active scheme. A named scheme leaves the global state unchanged.
#'
#' @param scheme Character scheme name, or `NULL` for the active
#'   scheme
#' @return The scheme as `bayesplot::color_scheme_get()` returns it,
#'   a list of six colours named by role
#' @noRd
mvgam_scheme <- function(scheme = NULL) {
  checkmate::assert_string(scheme, null.ok = TRUE)
  if (is.null(scheme)) {
    bayesplot::color_scheme_get()
  } else {
    bayesplot::color_scheme_get(scheme)
  }
}

#' The colours of a bayesplot scheme as an ordered vector
#'
#' @inheritParams mvgam_scheme
#' @return Character vector of six colours, light to dark
#' @noRd
mvgam_palette <- function(scheme = NULL) {
  unname(unlist(mvgam_scheme(scheme)))
}

#' One colour of a bayesplot scheme, selected by its role
#'
#' A figure takes its colours through this helper in place of hex codes.
#'
#' @inheritParams mvgam_scheme
#' @param role One of the six bayesplot scheme roles: `light`,
#'   `light_highlight`, `mid`, `mid_highlight`, `dark`,
#'   `dark_highlight`.
#' @return A single colour string.
#' @noRd
mvgam_colour <- function(role, scheme = NULL) {
  cs <- mvgam_scheme(scheme)
  checkmate::assert_choice(role, names(cs))
  cs[[role]]
}


#' Put back a scheme `color_scheme_get()` returned
#'
#' `color_scheme_set()` takes a scheme name or six colours. A scheme
#' built from six colours is named "custom". Passing "custom" back
#' raises `'arg' should be one of "blue", ...`. A custom scheme is
#' restored from its colours. Every other scheme is restored by name.
#'
#' @param cs A scheme as `bayesplot::color_scheme_get()` returns it.
#' @return `NULL`, invisibly
#' @noRd
restore_color_scheme <- function(cs) {
  checkmate::assert_list(cs)
  nm <- attr(cs, "scheme_name")
  if (is.null(nm) || identical(nm, "custom")) {
    bayesplot::color_scheme_set(unname(unlist(cs)))
  } else {
    bayesplot::color_scheme_set(nm)
  }
  invisible(NULL)
}


#' Evaluate an expression under the mvgam colour scheme and theme
#'
#' Every mvgam figure draws with the bayesplot `"red"` scheme and
#' [mvgam_theme()]. bayesplot fixes its theme into a plot when it builds
#' it. Its default theme sets a serif font. The scheme and theme a user
#' set through bayesplot are restored on exit.
#'
#' @param expr Expression to evaluate
#' @param scheme Character scheme name
#' @return The value of `expr`
#' @noRd
with_color_scheme <- function(expr, scheme = "red") {
  checkmate::assert_string(scheme)
  prior <- bayesplot::color_scheme_get()
  prior_theme <- bayesplot::bayesplot_theme_set(mvgam_theme())
  bayesplot::color_scheme_set(scheme)
  on.exit({
    restore_color_scheme(prior)
    bayesplot::bayesplot_theme_set(prior_theme)
  }, add = TRUE)
  force(expr)
}

#' Set the mvgam colour scheme and theme until the caller returns
#'
#' The `rlang::local_*` form of `with_color_scheme()`. It adds an
#' `on.exit(..., add = TRUE)` to `envir`, which restores the user's
#' scheme and bayesplot theme.
#'
#' @param scheme Character scheme name
#' @param envir Environment whose exit restores the prior scheme.
#'   Defaults to the caller's frame.
#' @return `NULL`, invisibly
#' @noRd
set_color_scheme_local <- function(scheme = "red", envir = parent.frame()) {
  checkmate::assert_string(scheme)
  checkmate::assert_environment(envir)
  prior <- bayesplot::color_scheme_get()
  prior_theme <- bayesplot::bayesplot_theme_set(mvgam_theme())
  bayesplot::color_scheme_set(scheme)
  do.call(
    base::on.exit,
    list(
      substitute({
        restore_color_scheme(prior)
        bayesplot::bayesplot_theme_set(prior_theme)
      }, list(prior = prior, prior_theme = prior_theme)),
      add = TRUE
    ),
    envir = envir
  )
  invisible(NULL)
}

#' The ggplot theme mvgam draws with
#'
#' @description
#' Every figure the package draws uses this theme, including the
#' figures it draws through \pkg{bayesplot}. It extends
#' [ggplot2::theme_classic()]. Apply it to a plot of your own to give it
#' the look of mvgam output.
#'
#' mvgam's figures take their colours from the `"red"` scheme of
#' \pkg{bayesplot}, which `bayesplot::color_scheme_get("red")` returns.
#' The example colours its points with it.
#'
#' @param base_size Base font size in points. Defaults to `11`,
#'   matching [ggplot2::theme_classic()].
#' @param base_family Base font family. Defaults to `""`, the
#'   ggplot2 default.
#' @return A [ggplot2::theme()] object.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point(colour = bayesplot::color_scheme_get("red")$dark) +
#'   mvgam_theme()
#'
#' @seealso [bayesplot::color_scheme_get()]
#' @author Nicholas J Clark
#' @export
mvgam_theme <- function(base_size = 11, base_family = "") {
  checkmate::assert_number(base_size, lower = 1)
  checkmate::assert_string(base_family)
  ggplot2::theme_classic(
    base_size = base_size, base_family = base_family
  ) +
    ggplot2::theme(
      # A grey strip bar with a thin border separates the panels of
      # faceted plots (lfo, kfold, pp_check residual panels,
      # conditional_smooths) from the canvas.
      strip.background = ggplot2::element_rect(
        fill = "grey85", colour = "grey60"
      ),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.title = ggplot2::element_text(size = base_size),
      legend.position = "right"
    )
}

#' The Okabe-Ito eight-colour qualitative palette
#'
#' A colour-blind safe palette from Okabe and Ito (2008,
#' https://jfly.uni-koeln.de/color/). Use it to distinguish two to
#' eight discrete groups. Sequential posterior bands of one quantity
#' use `mvgam_palette()`, a bayesplot single-hue scheme.
#'
#' @return Character vector of eight hex colours
#' @noRd
okabe_ito_palette <- function() {
  c(
    "#D55E00", "#0072B2", "#009E73", "#CC79A7",
    "#E69F00", "#56B4E9", "#F0E442", "#999999"
  )
}


#' The first `n` colours of the Okabe-Ito palette
#'
#' The palette recycles when `n` exceeds eight.
#'
#' @param n Integer number of colours
#' @return Character vector of `n` hex colours
#' @noRd
mvgam_categorical_palette <- function(n) {
  checkmate::assert_count(n)
  rep_len(okabe_ito_palette(), n)
}


#' A colour scale for plots that overlay several groups
#'
#' Every plot that colours discrete groups takes this scale. The groups
#' can be compared models or fits, the levels of a conditional effect's
#' second variable, or the outcomes and sources of a VAR. Up to three
#' groups take a dark, a middle and a light shade of the active
#' bayesplot scheme, matching the forecast and residual figures. The
#' dark shade is the scheme's darkest moved halfway towards black. The
#' three groups then differ clearly in lightness. More groups take the
#' Okabe-Ito hues.
#'
#' @param n Integer number of groups
#' @param name Legend title, or `NULL`
#' @param aesthetics Character vector of aesthetics the scale applies
#'   to
#' @param ... Further arguments to [ggplot2::scale_colour_manual()]
#' @return A ggplot2 scale
#' @noRd
mvgam_model_colour_scale <- function(n, name = NULL,
                                     aesthetics = "colour", ...) {
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_character(aesthetics, min.len = 1L)
  values <- if (n <= 3L) {
    c(darken_colour(mvgam_colour("dark_highlight"), 0.55),
      mvgam_colour("mid_highlight"),
      mvgam_colour("light"))[seq_len(n)]
  } else {
    mvgam_categorical_palette(n)
  }
  ggplot2::scale_colour_manual(name = name, values = values,
                               aesthetics = aesthetics, ...)
}

#' A colour moved towards black
#'
#' @param colour A colour R recognises
#' @param amount Share of the distance to black, between 0 and 1
#' @return A hex colour string
#' @noRd
darken_colour <- function(colour, amount) {
  checkmate::assert_string(colour)
  checkmate::assert_number(amount, lower = 0, upper = 1)
  rgb <- grDevices::col2rgb(colour)[, 1L] * (1 - amount)
  grDevices::rgb(rgb[1L], rgb[2L], rgb[3L], maxColorValue = 255)
}

#' Light-to-dark fills for `n` symmetric quantile bands
#'
#' Takes the leading entries of the palette. The widest band gets the
#' lightest colour and the narrowest band the darkest.
#'
#' @param n Integer number of bands
#' @param palette Character vector of colours, light to dark
#' @return Character vector of `n` colours
#' @noRd
mvgam_band_fills <- function(n, palette = mvgam_palette()) {
  checkmate::assert_character(palette, min.len = 1L)
  checkmate::assert_integerish(
    n, lower = 1L, upper = length(palette), len = 1L
  )
  palette[seq_len(n)]
}

#' An x axis of whole steps
#'
#' A horizon counts steps. An evaluation time on a regular grid also
#' counts steps. The scale keeps the whole-number breaks of
#' `pretty()`. An irregular `CAR()` grid has fractional times. With
#' `whole = FALSE` the scale keeps the default breaks for it.
#'
#' @param name Axis title
#' @param whole Whether every value on the axis is a whole number
#' @return A ggplot2 scale
#' @noRd
scale_x_steps <- function(name = "Horizon", whole = TRUE) {
  checkmate::assert_string(name)
  checkmate::assert_flag(whole)
  if (!isTRUE(whole)) return(ggplot2::scale_x_continuous(name = name))
  ggplot2::scale_x_continuous(
    name = name,
    breaks = function(limits) {
      b <- pretty(limits)
      b[b == round(b)]
    }
  )
}

#' Whether a prediction type shares the observations' scale
#'
#' `response` and `expected` do. `link`, `trend`, `variance`,
#' `latent_state` and `detection` each name another scale.
#' Observations drawn over one of those flatten the band against an
#' axis. Every observation overlay in the package calls this.
#'
#' @param type A prediction type
#' @return A single logical
#' @noRd
on_observation_scale <- function(type) {
  checkmate::assert_string(type, null.ok = TRUE)
  isTRUE(type %in% c("response", "expected"))
}

#' A posterior quantile at each occasion
#'
#' A closure-unit hindcast has one column per visit. Several visits
#' can share an occasion. Their draws are pooled, giving the
#' predictive for a visit chosen at random on that occasion. Every
#' other matrix has one column per occasion and is summarised column
#' by column.
#'
#' @param draws_mat A `(ndraws x n_col)` matrix
#' @param times The time of each column
#' @param prob The quantile to take
#' @return Numeric vector with one value for each of
#'   `sort(unique(times))`
#' @noRd
quantile_by_time <- function(draws_mat, times, prob) {
  checkmate::assert_matrix(draws_mat, mode = "numeric")
  checkmate::assert_numeric(
    times, len = ncol(draws_mat), any.missing = FALSE
  )
  checkmate::assert_number(prob, lower = 0, upper = 1)
  vapply(sort(unique(times)), function(t) {
    stats::quantile(draws_mat[, times == t], probs = prob, na.rm = TRUE,
                    names = FALSE)
  }, numeric(1L))
}

#' Per-occasion quantiles of a draws matrix, one block per group
#'
#' @param draws_mat A `(ndraws x n_col)` matrix
#' @param times The time of each column
#' @param probs The quantiles to take, one column `q1`, `q2`, ... each
#' @param group `NULL`, one panel label, or one label per column.
#'   Each label is its own panel over the same times. The columns of
#'   each label are summarised separately.
#' @return Data frame with `time`, the quantile columns and, when
#'   `group` is given, `series`
#' @noRd
occasion_quantiles <- function(draws_mat, times, probs, group = NULL) {
  checkmate::assert_matrix(draws_mat)
  checkmate::assert_numeric(probs, lower = 0, upper = 1, min.len = 1L)
  per_col <- length(group) > 1L
  if (per_col) checkmate::assert_true(length(group) == ncol(draws_mat))
  blocks <- if (per_col) {
    split(seq_along(times), factor(group, levels = unique(group)))
  } else {
    list(seq_along(times))
  }
  do.call(rbind, lapply(blocks, function(idx) {
    sub <- draws_mat[, idx, drop = FALSE]
    df <- data.frame(time = sort(unique(times[idx])))
    for (j in seq_along(probs)) {
      df[[paste0("q", j)]] <- quantile_by_time(sub, times[idx], probs[j])
    }
    if (!is.null(group)) df$series <- group[idx[1L]]
    df
  }))
}

#' Nested quantile ribbons for a `(ndraws x n_time)` matrix
#'
#' Returns a list of `geom_ribbon` layers, widest first, for adding to
#' a ggplot. `quantile_by_time()` computes the bounds at each
#' occasion. The narrowest band is drawn last and covers the wider
#' ones.
#'
#' @param draws_mat A `(ndraws x n_col)` matrix
#' @param times The time of each column
#' @param probs Numeric vector of central interval widths
#' @param palette Character vector of colours, light to dark
#' @param group `NULL`, one panel label, or one label per column
#' @param fill A single fill colour for every band, or `NULL` to take
#'   the fills from `palette`
#' @return List of ggplot2 ribbon layers
#' @noRd
mvgam_band_layer <- function(
  draws_mat,
  times,
  probs = c(0.5, 0.8, 0.95),
  palette = mvgam_palette(),
  group = NULL,
  fill = NULL
) {
  checkmate::assert_numeric(
    probs, lower = 0, upper = 1, min.len = 1L, any.missing = FALSE
  )
  checkmate::assert_matrix(draws_mat)
  checkmate::assert_character(palette, min.len = 1L)
  checkmate::assert_string(fill, null.ok = TRUE)
  if (any(duplicated(probs))) {
    stop(insight::format_error(
      "'probs' must be unique."
    ), call. = FALSE)
  }
  # Widest first. The narrowest band is drawn last.
  probs <- sort(probs, decreasing = TRUE)
  # A single fill suits a secondary band, such as the grey hindcast
  # interval that precedes a forecast.
  fills <- if (is.null(fill)) {
    mvgam_band_fills(length(probs), palette = palette)
  } else {
    rep(fill, length(probs))
  }
  alpha <- (1 - probs) / 2
  lapply(seq_along(probs), function(i) {
    df <- occasion_quantiles(
      draws_mat, times, c(alpha[i], 1 - alpha[i]), group
    )
    names(df)[2:3] <- c("lower", "upper")
    ggplot2::geom_ribbon(
      data = df,
      mapping = ggplot2::aes(x = time, ymin = lower, ymax = upper),
      fill = fills[i],
      inherit.aes = FALSE
    )
  })
}

#' Median line for a `(ndraws x n_time)` matrix
#'
#' @param draws_mat A `(ndraws x n_col)` matrix
#' @param times The time of each column
#' @param colour Line colour, or `NULL` for the `dark` entry of the
#'   active scheme (slot 5)
#' @param linewidth Numeric line width
#' @param group `NULL`, one panel label, or one label per column
#' @return A ggplot2 line layer
#' @noRd
mvgam_median_layer <- function(
  draws_mat,
  times,
  colour = NULL,
  linewidth = 1,
  group = NULL
) {
  checkmate::assert_matrix(draws_mat)
  checkmate::assert_string(colour, null.ok = TRUE)
  checkmate::assert_number(linewidth, lower = 0)
  if (is.null(colour)) {
    colour <- mvgam_colour("dark")
  }
  df <- occasion_quantiles(draws_mat, times, 0.5, group)
  names(df)[2L] <- "med"
  ggplot2::geom_line(
    data = df,
    mapping = ggplot2::aes(x = time, y = med),
    colour = colour,
    linewidth = linewidth,
    inherit.aes = FALSE
  )
}

#' Observation overlay of outlined points
#'
#' Each point is drawn twice: a larger point in the outline colour,
#' then the point itself. Missing values are dropped.
#'
#' @param times Vector of observation times
#' @param y Numeric vector of observed values
#' @param colour Point colour
#' @param outline Outline colour
#' @param size Numeric point size
#' @param group `NULL`, or a panel label for each observation
#' @return List of two `geom_point` layers, or an empty list when every
#'   point is missing
#' @noRd
mvgam_obs_layer <- function(
  times,
  y,
  colour = "black",
  outline = "white",
  size = 1.5,
  group = NULL
) {
  checkmate::assert_numeric(y)
  checkmate::assert_atomic_vector(times, len = length(y))
  checkmate::assert_string(colour)
  checkmate::assert_string(outline)
  checkmate::assert_number(size, lower = 0)
  ok <- !is.na(y) & !is.na(times)
  if (!any(ok)) {
    return(list())
  }
  df <- data.frame(time = times[ok], y = y[ok])
  if (!is.null(group)) {
    df$series <- group[ok]
  }
  list(
    ggplot2::geom_point(
      data = df,
      mapping = ggplot2::aes(x = time, y = y),
      colour = outline,
      size = size + 0.5,
      inherit.aes = FALSE
    ),
    ggplot2::geom_point(
      data = df,
      mapping = ggplot2::aes(x = time, y = y),
      colour = colour,
      size = size,
      inherit.aes = FALSE
    )
  )
}

#' Dashed vertical line at the training and forecast boundary
#'
#' @param t_cut Time of the boundary, or `NULL`
#' @param linetype Line type
#' @param colour Line colour
#' @return A ggplot2 layer, or `NULL` when there is no boundary. Adding
#'   `NULL` to a ggplot leaves it unchanged.
#' @noRd
mvgam_cut_layer <- function(
  t_cut,
  linetype = "dashed",
  colour = "black"
) {
  checkmate::assert_atomic(t_cut)
  checkmate::assert_string(linetype)
  checkmate::assert_string(colour)
  if (is.null(t_cut) || length(t_cut) == 0L || all(is.na(t_cut))) {
    return(NULL)
  }
  ggplot2::geom_vline(
    xintercept = t_cut,
    linetype = linetype,
    colour = colour
  )
}

#' One panel of a cross-validation diagnostic plot
#'
#' @param x Vector of the x positions, times or group labels
#' @param value Numeric vector of the diagnostic
#' @param facet Character panel title
#' @param threshold Numeric threshold, or `NULL` for the `probs`
#'   quantile of `value`
#' @param probs Numeric quantile probability, used when `threshold` is
#'   `NULL`
#' @param worse_above Logical. `TRUE` when values above the threshold
#'   are the poor ones
#' @return Data frame with columns `x`, `value`, `threshold`, `facet`
#'   and `worse_above`
#' @noRd
cv_panel <- function(x, value, facet, threshold = NULL, probs = NULL,
                     worse_above = TRUE) {
  checkmate::assert_atomic_vector(x)
  checkmate::assert_numeric(value, len = length(x))
  checkmate::assert_string(facet)
  checkmate::assert_number(threshold, null.ok = TRUE)
  checkmate::assert_number(probs, lower = 0, upper = 1,
                           null.ok = !is.null(threshold))
  checkmate::assert_flag(worse_above)
  if (is.null(threshold)) {
    threshold <- unname(stats::quantile(value, probs, na.rm = TRUE))
  }
  data.frame(x = x, value = value, threshold = threshold, facet = facet,
             worse_above = worse_above)
}


#' The Pareto k panel of a cross-validation diagnostic plot
#'
#' loo gives an infinite k when the tail holds fewer than five draws.
#' Such points are drawn at the largest finite k to keep the panel's
#' scale.
#'
#' @param x Vector of the x positions
#' @param k Numeric vector of Pareto k values
#' @param threshold Numeric refit threshold
#' @return Data frame from `cv_panel()`
#' @noRd
pareto_k_panel <- function(x, k, threshold) {
  checkmate::assert_numeric(k)
  finite_k <- k[is.finite(k)]
  if (length(finite_k) > 0L) {
    k[is.infinite(k)] <- max(finite_k)
  }
  cv_panel(x, k, "Pareto k", threshold = threshold)
}


#' Cross-validation diagnostics in one panel each
#'
#' Used by `plot.mvgam_lfo()` and `plot.mvgam_kfold()`. The dashed line
#' in each panel is its threshold. Points past it are drawn dark red.
#' Missing values, such as the Pareto k of a refit, are not drawn.
#'
#' @param long Data frame of stacked `cv_panel()` results
#' @return A `ggplot` object with no x scale or label
#' @noRd
cv_diagnostic_plot <- function(long) {
  checkmate::assert_data_frame(long, min.rows = 1L)
  set_color_scheme_local()
  long <- long[!is.na(long$value), , drop = FALSE]
  long$facet <- factor(long$facet, levels = unique(long$facet))
  past <- ifelse(long$worse_above, long$value > long$threshold,
                 long$value < long$threshold)
  long$colour <- ifelse(past, "outlier", "inlier")
  ggplot2::ggplot(long, ggplot2::aes(x = .data$x, y = .data$value)) +
    ggplot2::facet_wrap(~ .data$facet, ncol = 1L, scales = "free_y") +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = .data$threshold),
      colour = mvgam_colour("mid_highlight"), linetype = "dashed",
      linewidth = 1
    ) +
    ggplot2::geom_line(ggplot2::aes(group = .data$facet),
                       linewidth = 0.5, colour = "grey30") +
    ggplot2::geom_point(shape = 16, colour = "white", size = 2) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$colour),
                        shape = 16, show.legend = FALSE, size = 1.5) +
    ggplot2::scale_colour_manual(
      values = c(inlier = "grey30", outlier = mvgam_colour("dark"))
    ) +
    ggplot2::labs(y = NULL) +
    mvgam_theme()
}


#' A dashed reference line, such as zero or a cutoff
#'
#' Give exactly one of `yintercept` and `xintercept`.
#'
#' @param yintercept Numeric height of a horizontal line, or `NULL`
#' @param xintercept Numeric position of a vertical line, or `NULL`
#' @return A ggplot2 layer
#' @noRd
reference_line <- function(yintercept = NULL, xintercept = NULL) {
  checkmate::assert_number(yintercept, null.ok = TRUE)
  checkmate::assert_number(xintercept, null.ok = TRUE)
  stopifnot(xor(is.null(yintercept), is.null(xintercept)))
  if (is.null(xintercept)) {
    return(ggplot2::geom_hline(yintercept = yintercept,
                               linetype = "dashed", colour = "grey40"))
  }
  ggplot2::geom_vline(xintercept = xintercept, linetype = "dashed",
                      colour = "grey40")
}


#' One figure from one plot per response
#'
#' A plotting method returns one plot per response of a multivariate
#' fit. A list of plots prints one after another under `$name`
#' headers. The stacked figure keeps each plot's own scales and titles
#' each plot with its response.
#'
#' @param plots A named list of `ggplot` or `patchwork` objects
#' @return A single `patchwork`
#' @noRd
stack_response_plots <- function(plots) {
  checkmate::assert_list(plots, min.len = 1L, names = "unique")
  # Each plot is wrapped as a panel. Its title row is then free for the
  # response name.
  titled <- lapply(names(plots), function(r) {
    patchwork::wrap_elements(panel = plots[[r]]) + ggplot2::labs(title = r) +
      mvgam_theme()
  })
  patchwork::wrap_plots(titled, ncol = 1L)
}

#' `facet_wrap(~series)` with the defaults of every faceted plot
#'
#' @param scales Character scales argument of
#'   [ggplot2::facet_wrap()]
#' @param ncol Integer number of facet columns, or `NULL`
#' @return A ggplot2 facet
#' @noRd
mvgam_facet_series <- function(scales = "free_y", ncol = NULL) {
  checkmate::assert_string(scales)
  checkmate::assert_count(ncol, positive = TRUE, null.ok = TRUE)
  ggplot2::facet_wrap(~series, scales = scales, ncol = ncol)
}

#' Repelled text or label layer for label-heavy plots
#'
#' Serves the ordination plots.
#'
#' @param data Data frame of the labels
#' @param mapping Aesthetic mapping from [ggplot2::aes()]
#' @param type Either `"text"` or `"label"`
#' @param ... Further arguments to the geom
#' @return A ggplot2 layer
#' @noRd
mvgam_repel_layer <- function(
  data,
  mapping,
  type = c("text", "label"),
  ...
) {
  checkmate::assert_data_frame(data)
  type <- match.arg(type)
  insight::check_if_installed(
    "ggrepel", reason = "to repel overlapping labels"
  )
  fn <- if (type == "text") {
    ggrepel::geom_text_repel
  } else {
    ggrepel::geom_label_repel
  }
  fn(data = data, mapping = mapping, ...)
}


#' Diverging fill scale for correlation and precision heatmaps
#'
#' A `scale_fill_gradient2()` centred at zero: blue for negative
#' associations, grey at zero and red for positive ones. A diverging
#' scale needs a warm and a cool end. The warm end comes from the
#' bayesplot `"red"` scheme and the cool end from the Okabe-Ito
#' palette, for any active scheme.
#'
#' @param name Legend title
#' @param limits Numeric range of the scale. Defaults to the
#'   correlation range `[-1, 1]`. A precision matrix passes
#'   `c(-L, L)`.
#' @param breaks Numeric legend breaks
#' @param na.value Fill colour of missing cells
#' @return A ggplot2 fill scale
#' @noRd
mvgam_diverging_scale <- function(
  name = "correlation",
  limits = c(-1, 1),
  breaks = pretty_symmetric_breaks(limits),
  na.value = "transparent"
) {
  checkmate::assert_string(name)
  checkmate::assert_numeric(limits, len = 2L, any.missing = FALSE)
  checkmate::assert_numeric(breaks)
  checkmate::assert_string(na.value)
  warm <- mvgam_palette("red")[5L]
  cool <- okabe_ito_palette()[2L]
  ggplot2::scale_fill_gradient2(
    name = name,
    low = cool, mid = "grey95", high = warm,
    midpoint = 0,
    limits = limits,
    breaks = breaks,
    na.value = na.value
  )
}


#' Five evenly spaced legend breaks across a symmetric range
#'
#' Limits taken from the data give breaks with many decimal places.
#' The breaks are rounded to two significant digits of the
#' half-range.
#'
#' @param limits Numeric length-2 range, symmetric about zero.
#' @return Numeric vector of breaks.
#'
#' @noRd
pretty_symmetric_breaks <- function(limits) {
  checkmate::assert_numeric(limits, len = 2L, any.missing = FALSE)
  raw <- seq(limits[1L], limits[2L], length.out = 5L)
  # Two significant digits on the half-range keeps the endpoints
  # distinct without carrying the full float. ggplot2 drops from the
  # legend a break rounded outward past the limits. Such an endpoint
  # steps back one unit of the last digit.
  digits <- max(1L, 2L - ceiling(log10(max(abs(limits), 1e-8))))
  breaks <- round(raw, digits)
  outside <- breaks < limits[1L] | breaks > limits[2L]
  breaks[outside] <- breaks[outside] - sign(breaks[outside]) * 10^-digits
  unique(breaks)
}


#' Lower triangle of a correlation matrix for a heatmap
#'
#' A symmetric matrix repeats each entry in its upper triangle. The
#' diagonal of a correlation matrix is 1. The panel keeps the entries
#' below the diagonal.
#'
#' @param estimate Symmetric matrix of posterior point estimates.
#' @param cluster Reorder by `cluster_cormat()`.
#' @param clear Optional logical matrix, `TRUE` where the interval of
#'   an entry excludes zero. Reordered with `estimate`.
#' @return Frame from `matrix_panel()`.
#'
#' @noRd
residcor_panel_data <- function(estimate, cluster = FALSE, clear = NULL) {
  # A single series has no off-diagonal entry. The assertion names
  # that cause before an empty frame fails in ggplot2.
  checkmate::assert_matrix(estimate, mode = "numeric", min.rows = 2L)
  checkmate::assert_flag(cluster)
  idx <- if (cluster) cluster_cormat(estimate) else seq_len(nrow(estimate))
  matrix_panel(estimate[idx, idx], clear = clear[idx, idx],
               lower_only = TRUE)
}


#' Which entries of a summarised matrix have an interval clear of zero
#'
#' @param lower,upper Matrices of interval bounds, or `NULL`
#' @return Logical matrix, or `NULL` when either bound is missing
#' @noRd
interval_excludes_zero <- function(lower, upper) {
  if (is.null(lower) || is.null(upper)) {
    return(NULL)
  }
  out <- lower > 0 | upper < 0
  out[is.na(out)] <- FALSE
  out
}


#' Heatmap of matrices in long format
#'
#' Draws one tile per entry, filled by `value`. Entries whose `clear`
#' column is `TRUE` take a black outline. A `group` column draws one
#' facet per group, in the order the groups first appear.
#'
#' `layout = "matrix"` draws rows down and columns across, as the
#' matrix is written. `layout = "triangle"` draws the lower triangle
#' of a symmetric matrix. Both axes keep every level of the panel's
#' factors, which keeps the panel square.
#'
#' @param panel Frame of `Var1` (row), `Var2` (column) and `value`,
#'   with optional `clear` and `group` columns
#' @param fill_scale A ggplot2 fill scale
#' @param layout `"matrix"` or `"triangle"`
#' @param x_lab,y_lab Axis titles
#' @param ncol Facet columns when `panel` has a `group` column
#' @return A `ggplot` object
#' @noRd
matrix_heatmap <- function(panel, fill_scale,
                           layout = c("matrix", "triangle"),
                           x_lab = NULL, y_lab = NULL, ncol = NULL) {
  checkmate::assert_data_frame(panel)
  checkmate::assert_names(names(panel),
                          must.include = c("Var1", "Var2", "value"))
  layout <- match.arg(layout)
  checkmate::assert_string(x_lab, null.ok = TRUE)
  checkmate::assert_string(y_lab, null.ok = TRUE)
  checkmate::assert_count(ncol, positive = TRUE, null.ok = TRUE)
  is_matrix <- layout == "matrix"
  position <- if (is_matrix) {
    ggplot2::aes(x = .data$Var2, y = .data$Var1)
  } else {
    ggplot2::aes(x = .data$Var1, y = .data$Var2)
  }
  y_scale <- if (is_matrix) {
    ggplot2::scale_y_discrete(limits = rev)
  } else {
    ggplot2::scale_y_discrete(drop = FALSE)
  }
  if (!is.null(panel$group)) {
    panel$group <- factor(panel$group, levels = unique(panel$group))
  }
  clear <- panel[panel$clear %in% TRUE, , drop = FALSE]
  p <- ggplot2::ggplot(panel, position) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$value), colour = "grey60") +
    ggplot2::geom_tile(data = clear, fill = NA, colour = "black",
                       linewidth = 0.9) +
    fill_scale +
    # A vertical label is one text line wide. Labels of adjacent tiles
    # then stay apart however many tiles the axis holds.
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 90),
                              drop = FALSE) +
    y_scale +
    ggplot2::labs(x = x_lab, y = y_lab) +
    mvgam_theme() +
    # Past 30 levels a label of the base size is taller than its tile.
    # The text shrinks in step with the tiles.
    ggplot2::theme(axis.text = ggplot2::element_text(
      size = ggplot2::rel(min(1, 30 / nlevels(factor(panel$Var1))))
    ))
  if (!is.null(panel$group)) {
    p <- p +
      ggplot2::facet_wrap(
        ~ group, ncol = ncol %||% ceiling(sqrt(nlevels(panel$group)))
      )
  }
  p
}


#' A matrix in long format for `matrix_heatmap()`
#'
#' @param mat Numeric matrix
#' @param clear Optional logical matrix the shape of `mat`, `TRUE` where
#'   an entry's interval excludes zero. `NULL` marks no entry.
#' @param lower_only Logical. `TRUE` keeps the entries below the
#'   diagonal. Rows then start at the second name and columns end at
#'   the second last. The triangle then has no empty row or column.
#' @return Data frame of `Var1` (row), `Var2` (column), `value` and
#'   `clear`
#' @noRd
matrix_panel <- function(mat, clear = NULL, lower_only = FALSE) {
  checkmate::assert_matrix(mat)
  checkmate::assert_flag(lower_only)
  clear <- clear %||% matrix(FALSE, nrow(mat), ncol(mat))
  checkmate::assert_matrix(clear, mode = "logical", nrows = nrow(mat),
                           ncols = ncol(mat))
  out <- expand.grid(Var1 = rownames(mat) %||% seq_len(nrow(mat)),
                     Var2 = colnames(mat) %||% seq_len(ncol(mat)))
  out$value <- as.vector(mat)
  out$clear <- as.vector(clear)
  if (lower_only) {
    out <- out[as.vector(lower.tri(mat)), , drop = FALSE]
    lv <- levels(out$Var1)
    out$Var1 <- factor(out$Var1, levels = lv[-1L])
    out$Var2 <- factor(out$Var2, levels = lv[-length(lv)])
  }
  out
}


#' An approximate Robinson ordering of a correlation matrix
#'
#' Follows Gruvaeus and Wainer (1972). The ordering places clusters of
#' positive and negative correlation next to each other in the
#' heatmap. Distances are `1 - cormat`: r = 1 gives d = 0, r = 0 gives
#' d = 1 and r = -1 gives d = 2. Strong negative correlations are then
#' furthest from strong positive ones. Linkage is `"average"` (UPGMA).
#' For correlation distances it gives more interpretable species
#' groupings than complete linkage. `reorder_clusters()` comes from
#' the `gclus` R package.
#'
#' @param cormat Numeric correlation matrix
#' @param ... Further arguments to [stats::hclust()]
#' @return Integer vector of the row order
#' @importFrom stats hclust as.dist
#' @noRd
cluster_cormat <- function(cormat, ...) {
  checkmate::assert_matrix(cormat, mode = "numeric")
  dis <- 1 - cormat
  dis_d <- stats::as.dist(dis)
  n <- NROW(dis)
  if (n <= 2L) {
    return(seq_len(n))
  }
  clusters <- stats::hclust(dis_d, method = "average", ...)
  clusters <- reorder_clusters(clusters, dis)
  clusters$order
}


#' Reorder the leaves of a clustering by endpoint distance
#'
#' At each merge the two subtrees are flipped to put their closest
#' endpoints together. Adapted from the `gclus` R package.
#'
#' @param x An `hclust` object
#' @param dis Distance matrix, or a `dist` object
#' @return The `hclust` object with reordered `merge` and `order`
#' @noRd
reorder_clusters <- function(x, dis) {
  checkmate::assert_class(x, "hclust")
  if (!is.matrix(dis)) {
    dis <- as.matrix(dis)
  }
  merges <- x$merge
  n <- NROW(merges)
  endpoints <- matrix(0, n, 2)
  dir <- matrix(1L, n, 2)
  for (i in 1L:n) {
    j <- merges[i, 1]
    k <- merges[i, 2]
    if ((j < 0) && (k < 0)) {
      endpoints[i, 1] <- -j
      endpoints[i, 2] <- -k
    } else if (j < 0) {
      j <- -j
      endpoints[i, 1] <- j
      e1 <- endpoints[k, 1]
      e2 <- endpoints[k, 2]
      if (dis[j, e1] < dis[j, e2]) {
        endpoints[i, 2] <- e2
      } else {
        endpoints[i, 2] <- e1
        dir[i, 2] <- -1
      }
    } else if (k < 0) {
      k <- -k
      endpoints[i, 2] <- k
      e1 <- endpoints[j, 1]
      e2 <- endpoints[j, 2]
      if (dis[k, e1] < dis[k, e2]) {
        endpoints[i, 1] <- e2
        dir[i, 1] <- -1
      } else {
        endpoints[i, 1] <- e1
      }
    } else {
      ek1 <- endpoints[k, 1]
      ek2 <- endpoints[k, 2]
      ej1 <- endpoints[j, 1]
      ej2 <- endpoints[j, 2]

      d11 <- dis[ej1, ek1]
      d12 <- dis[ej1, ek2]
      d21 <- dis[ej2, ek1]
      d22 <- dis[ej2, ek2]
      dmin <- min(d11, d12, d21, d22)
      if (dmin == d21) {
        endpoints[i, 1] <- ej1
        endpoints[i, 2] <- ek2
      } else if (dmin == d11) {
        endpoints[i, 1] <- ej2
        endpoints[i, 2] <- ek2
        dir[i, 1] <- -1
      } else if (dmin == d12) {
        endpoints[i, 1] <- ej2
        endpoints[i, 2] <- ek1
        dir[i, 1] <- -1
        dir[i, 2] <- -1
      } else {
        endpoints[i, 1] <- ej1
        endpoints[i, 2] <- ek1
        dir[i, 2] <- -1
      }
    }
  }
  for (i in n:2L) {
    if (dir[i, 1] == -1) {
      m <- merges[i, 1]
      if (m > 0) {
        m1 <- merges[m, 1]
        merges[m, 1] <- merges[m, 2]
        merges[m, 2] <- m1
        if (dir[m, 1] == dir[m, 2]) {
          dir[m, ] <- -dir[m, ]
        }
      }
    }
    if (dir[i, 2] == -1) {
      m <- merges[i, 2]
      if (m > 0) {
        m1 <- merges[m, 1]
        merges[m, 1] <- merges[m, 2]
        merges[m, 2] <- m1
        if (dir[m, 1] == dir[m, 2]) {
          dir[m, ] <- -dir[m, ]
        }
      }
    }
  }
  clusters <- as.list(1:n)
  for (i in 1:n) {
    j <- merges[[i, 1]]
    k <- merges[[i, 2]]
    if ((j < 0) && (k < 0)) {
      clusters[[i]] <- c(-j, -k)
    } else if (j < 0) {
      clusters[[i]] <- c(-j, clusters[[k]])
    } else if (k < 0) {
      clusters[[i]] <- c(clusters[[j]], -k)
    } else {
      clusters[[i]] <- c(clusters[[j]], clusters[[k]])
    }
  }

  x1 <- x
  x1$merge <- merges
  x1$order <- clusters[[n]]
  x1
}


#' Resolve per-draw factor loadings for a fitted mvgam.
#'
#' Returns a `[ndraws, n_series, n_lv]` array of Z loadings whether
#' Z was sampled (the default factor model) or fixed by a
#' `trend_map`. Every method that needs Z calls this resolver, which
#' holds the choice between the fixed and sampled paths.
#'
#' Two calling styles:
#' - Object style: `resolve_factor_loadings(object = fit)`. The
#'   resolver takes `fixed_Z`, `n_lv` and `n_series` from the fit
#'   and extracts draws as needed.
#' - Explicit style: `resolve_factor_loadings(fixed_Z = ...,
#'   draws_mat = ..., n_series = ..., n_lv = ...)`. `n_series` and
#'   `n_lv` are required in this style. `draw_innovation_grid()`
#'   uses it with draws it has already extracted.
#'
#' A fully populated `fixed_Z` is repeated for every draw. Otherwise
#' `extract_Z_loadings()` extracts the draws. A fit identified by the
#' QR rotation stores `Z_tilde[i, j]`. Every other factor fit stores
#' `Z[i, j]`.
#'
#' @param object Fitted mvgam object (object-style entry).
#'   Required when callers omit `n_series`/`n_lv`.
#' @param draws_mat Optional pre-extracted posterior draws.
#'   Required when `object` is omitted and `fixed_Z` is NULL.
#' @param fixed_Z Optional fixed-loadings matrix. With `object` it
#'   defaults to `object$trend_metadata$fixed_Z`.
#' @param n_lv Integer factor dimension. Required in
#'   explicit-style; derived from `object` otherwise.
#' @param n_series Integer series count. Required in
#'   explicit-style; derived from `object` otherwise.
#' @param basis `"identified"` returns the loadings a report or plot
#'   shows. `"model"` returns the `Z` the model sampled. A caller
#'   combining loadings with `sigma_trend`, `Sigma_trend` or
#'   `Omega_trend` needs `"model"`: the model estimated those
#'   parameters with the sampled `Z`. The two differ only in a fit
#'   identified by the QR rotation.
#'
#' @return Numeric array of dimension `[ndraws, n_series, n_lv]`.
#' @noRd
resolve_factor_loadings <- function(object = NULL,
                                    draws_mat = NULL,
                                    fixed_Z = NULL,
                                    n_lv = NULL,
                                    n_series = NULL,
                                    basis = c("identified", "model")) {
  basis <- match.arg(basis)
  checkmate::assert_matrix(draws_mat, null.ok = TRUE)
  checkmate::assert_matrix(fixed_Z, null.ok = TRUE)
  if (!is.null(object)) {
    checkmate::assert_class(object, "mvgam")
    if (is.null(fixed_Z)) {
      fixed_Z <- object$trend_metadata$fixed_Z
    }
    if (is.null(n_lv)) {
      n_lv <- object$trend_metadata$n_lv %||%
        (if (!is.null(fixed_Z)) ncol(fixed_Z) else NULL)
    }
    n_series <- n_series %||% loading_series_count(object)
  }
  checkmate::assert_int(n_lv, lower = 1L)
  checkmate::assert_int(n_series, lower = 1L)

  # A partial Z marks its sampled entries with NA. The posterior
  # stores every entry as `Z[i, j]`. The draws parser below handles
  # that case.
  fully_fixed <- !is.null(fixed_Z) && !anyNA(fixed_Z)
  if (fully_fixed) {
    # A fixed-Z fit stores Z in the Stan data. The posterior has no
    # `Z[i, j]` columns. The matrix is repeated for every draw to
    # match the sampled-Z return shape.
    ndraws <- if (!is.null(draws_mat)) {
      nrow(draws_mat)
    } else if (!is.null(object)) {
      posterior::ndraws(object)
    } else {
      stop_mvgam_fault(
        "The fixed loadings need a draw count.",
        "The caller passed 'object = NULL' and 'draws_mat = NULL'."
      )
    }
    return(array(
      rep(as.numeric(unname(fixed_Z)), each = ndraws),
      dim = c(ndraws, n_series, n_lv)
    ))
  }

  if (is.null(draws_mat)) {
    if (is.null(object)) {
      stop_mvgam_fault(
        "The sampled loadings need draws.",
        "The caller passed 'object = NULL' and 'draws_mat = NULL'."
      )
    }
    draws_mat <- fit_draws(object$fit, c("Z", "Z_tilde"))
  }
  extract_Z_loadings(draws_mat, n_obs_series = n_series,
                     n_lv = n_lv, basis = basis)
}


#' Posterior medians of a factor model's loadings
#'
#' @param object Fitted `mvgam` factor model
#' @param n_lv Integer number of latent factors
#' @return Numeric matrix `[n_series, n_lv]` with the fitted series as
#'   row names
#' @noRd
factor_loading_medians <- function(object, n_lv) {
  med <- apply(resolve_factor_loadings(object, n_lv = n_lv), c(2L, 3L),
               stats::median)
  rownames(med) <- fitted_series_levels(object)
  med
}
