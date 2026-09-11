# Shared ggplot building blocks for every plot method in mvgam.
# Defines the palette, theme, ribbon stacking,
# observation overlays, faceting, and label-repel wrappers.
# Nothing in this file is exported.

#' Return the active bayesplot colour scheme as an ordered vector.
#'
#' Resolves to bayesplot's currently-active scheme by default so
#' user-side `bayesplot::color_scheme_set()` calls compose. Pass a
#' named scheme (e.g. `"red"`, `"blue"`) to override without
#' touching global state.
#'
#' @noRd
mvgam_palette <- function(scheme = NULL) {
  if (is.null(scheme)) {
    cs <- bayesplot::color_scheme_get()
  } else {
    cs <- bayesplot::color_scheme_get(scheme)
  }
  unname(unlist(cs))
}

#' One colour of the active bayesplot scheme, by the role it plays
#' rather than its position. Reading a scheme entry through this is
#' what lets every mvgam figure follow a user's
#' `bayesplot::color_scheme_set()`; a hex written at the call site
#' pins that one panel to whatever scheme was current when it was
#' written, and the package then draws two schemes at once.
#'
#' @param role One of the six bayesplot scheme roles: `light`,
#'   `light_highlight`, `mid`, `mid_highlight`, `dark`,
#'   `dark_highlight`.
#' @param scheme Optional scheme name; the active scheme by default.
#' @return A single colour string.
#'
#' @noRd
mvgam_colour <- function(role, scheme = NULL) {
  roles <- c("light", "light_highlight", "mid", "mid_highlight",
             "dark", "dark_highlight")
  checkmate::assert_choice(role, roles)
  cs <- if (is.null(scheme)) {
    bayesplot::color_scheme_get()
  } else {
    bayesplot::color_scheme_get(scheme)
  }
  cs[[role]]
}


#' Run an expression with a temporary bayesplot colour scheme,
#' restoring the prior scheme on exit. Suits short call-style
#' wrappers (`with_color_scheme("red", do.call(...))`).
#'
#' @noRd
with_color_scheme <- function(scheme, expr) {
  prior <- attr(bayesplot::color_scheme_get(), "scheme_name")
  bayesplot::color_scheme_set(scheme)
  on.exit(bayesplot::color_scheme_set(prior), add = TRUE)
  force(expr)
}

#' Set a temporary bayesplot colour scheme that is restored when
#' the calling function returns. Mirrors the `rlang::local_*`
#' family. Use when the body is too long to wrap in
#' `with_color_scheme(...)`. Adds an `on.exit(..., add = TRUE)`
#' to `envir`, defaulting to the caller's frame.
#'
#' @noRd
set_color_scheme_local <- function(scheme, envir = parent.frame()) {
  prior <- attr(bayesplot::color_scheme_get(), "scheme_name")
  bayesplot::color_scheme_set(scheme)
  do.call(
    base::on.exit,
    list(
      substitute(bayesplot::color_scheme_set(prior),
                 list(prior = prior)),
      add = TRUE
    ),
    envir = envir
  )
  invisible(NULL)
}

#' The ggplot theme mvgam draws with
#'
#' @description
#' Every figure the package draws uses this theme, which is built on
#' [ggplot2::theme_classic()] so it composes with the bayesplot
#' family. Apply it to a plot of your own to sit alongside mvgam
#' output without the two looking like they came from different
#' packages.
#'
#' Colours are a separate question and come from the active
#' bayesplot scheme, so [bayesplot::color_scheme_set()] moves mvgam's
#' figures and yours together, and
#' [bayesplot::color_scheme_get()] reads the current one.
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
      # Grey strip bar + thin border so faceted plots (lfo, kfold,
      # pp_check residual panels, conditional_smooths) read as
      # distinct panels rather than blending into the canvas.
      strip.background = ggplot2::element_rect(
        fill = "grey85", colour = "grey60"
      ),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.title = ggplot2::element_text(size = base_size),
      legend.position = "right"
    )
}

#' Okabe-Ito 8-colour qualitative palette, color-blind safe.
#' Source: Okabe & Ito 2008
#' (https://jfly.uni-koeln.de/color/), the standard go-to for
#' categorical groupings in scientific figures. Use it anywhere
#' the package needs to distinguish small numbers (~2-8) of
#' discrete groups; for sequential posterior bands of a single
#' quantity, prefer `mvgam_palette()` (bayesplot single-hue
#' scheme).
#'
#' @noRd
okabe_ito_palette <- function() {
  c(
    "#D55E00", "#0072B2", "#009E73", "#CC79A7",
    "#E69F00", "#56B4E9", "#F0E442", "#999999"
  )
}


#' Pick the first `n` colours from the Okabe-Ito categorical
#' palette, recycling if `n` exceeds palette length.
#'
#' @noRd
mvgam_categorical_palette <- function(n) {
  rep_len(okabe_ito_palette(), n)
}


#' Shared `scale_colour_manual` for plots that overlay several
#' coloured categorical groups (compare_scores models,
#' compare_elpds models). Uses the Okabe-Ito palette instead of
#' the bayesplot single-hue schemes, which are designed for
#' sequential posterior bands and make distinct categories hard
#' to tell apart at small palette indices.
#'
#' @noRd
mvgam_model_colour_scale <- function(n, name = NULL) {
  ggplot2::scale_colour_manual(
    name   = name,
    values = mvgam_categorical_palette(n)
  )
}

#' Light-to-dark fills for `n` symmetric quantile bands. Picks
#' from the leading entries of the active palette so the outer
#' (widest) band gets the lightest colour and the inner
#' (narrowest) band gets the darkest.
#'
#' @noRd
mvgam_band_fills <- function(n, palette = mvgam_palette()) {
  checkmate::assert_integerish(
    n, lower = 1L, upper = length(palette), len = 1L
  )
  palette[seq_len(n)]
}

#' Multi-quantile ribbon stack for a `(ndraws x n_time)` matrix.
#'
#' Returns a list of `geom_ribbon` layers (outer first, inner
#' last) suitable for `+`-composition. Quantile bounds are
#' computed per column; NAs are dropped per-column. Inner bands
#' plot on top of outer bands.
#'
#' @noRd
mvgam_band_layer <- function(
  draws_mat,
  times,
  probs = c(0.5, 0.8, 0.95),
  palette = mvgam_palette(),
  group = NULL,
  fill = NULL
) {
  checkmate::assert_matrix(draws_mat, mode = "numeric")
  checkmate::assert_numeric(
    times, len = ncol(draws_mat), any.missing = FALSE
  )
  checkmate::assert_numeric(
    probs, lower = 0, upper = 1, min.len = 1L, any.missing = FALSE
  )
  if (any(duplicated(probs))) {
    stop(insight::format_error(
      "'probs' must be unique."
    ))
  }
  # Sort outer-to-inner so the narrowest band is plotted last
  # (on top).
  probs <- sort(probs, decreasing = TRUE)
  # A single flat fill is what a panel wants when its bands are not
  # the subject, as the hindcast arm of a forecast plot does with its
  # one grey interval behind the forecast's own.
  checkmate::assert_string(fill, null.ok = TRUE)
  fills <- if (is.null(fill)) {
    mvgam_band_fills(length(probs), palette = palette)
  } else {
    rep(fill, length(probs))
  }
  alpha <- (1 - probs) / 2
  lapply(seq_along(probs), function(i) {
    lo <- apply(
      draws_mat, 2L, stats::quantile, probs = alpha[i], na.rm = TRUE
    )
    hi <- apply(
      draws_mat, 2L, stats::quantile,
      probs = 1 - alpha[i], na.rm = TRUE
    )
    df <- data.frame(time = times, lower = lo, upper = hi)
    if (!is.null(group)) {
      df$series <- group
    }
    ggplot2::geom_ribbon(
      data = df,
      mapping = ggplot2::aes(x = time, ymin = lower, ymax = upper),
      fill = fills[i],
      inherit.aes = FALSE
    )
  })
}

#' Median line for a `(ndraws x n_time)` matrix. Colour defaults
#' to the palette's `dark` entry (slot 5 in a bayesplot scheme).
#'
#' @noRd
mvgam_median_layer <- function(
  draws_mat,
  times,
  colour = NULL,
  linewidth = 1,
  group = NULL
) {
  checkmate::assert_matrix(draws_mat, mode = "numeric")
  checkmate::assert_numeric(
    times, len = ncol(draws_mat), any.missing = FALSE
  )
  if (is.null(colour)) {
    colour <- mvgam_palette()[5L]
  }
  med <- apply(draws_mat, 2L, stats::median, na.rm = TRUE)
  df <- data.frame(time = times, med = med)
  if (!is.null(group)) {
    df$series <- group
  }
  ggplot2::geom_line(
    data = df,
    mapping = ggplot2::aes(x = time, y = med),
    colour = colour,
    linewidth = linewidth,
    inherit.aes = FALSE
  )
}

#' Observation overlay (double-plotted points, white outline +
#' coloured fill). NAs are dropped silently. Returns a length-2
#' list of `geom_point` layers; returns an empty list when there
#' are no plottable points.
#'
#' @noRd
mvgam_obs_layer <- function(
  times,
  y,
  colour = "black",
  outline = "white",
  size = 1.5,
  group = NULL
) {
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

#' Dashed vertical cut at the training / forecast boundary.
#' Returns `NULL` (a no-op when added to a ggplot) when there is
#' no cut to draw.
#'
#' @noRd
mvgam_cut_layer <- function(
  t_cut,
  linetype = "dashed",
  colour = "black"
) {
  if (is.null(t_cut) || length(t_cut) == 0L || all(is.na(t_cut))) {
    return(NULL)
  }
  ggplot2::geom_vline(
    xintercept = t_cut,
    linetype = linetype,
    colour = colour
  )
}

#' One figure from one plot per response
#'
#' A multivariate fit answers a plotting method once per response.
#' Handed back as a list, the plots print one after another under
#' `$name` headers, and a reader gets a console listing where a figure
#' was asked for. Stacked, each keeps its own scales and is titled by
#' the response it draws.
#'
#' @param plots A named list of `ggplot` or `patchwork` objects
#' @return A single `patchwork`
#' @noRd
stack_response_plots <- function(plots) {
  checkmate::assert_list(plots, min.len = 1L, names = "unique")
  titled <- lapply(names(plots), function(r) {
    patchwork::wrap_elements(full = plots[[r]]) + ggplot2::labs(title = r)
  })
  patchwork::wrap_plots(titled, ncol = 1L)
}

#' `facet_wrap(~series)` wrapper with uniform defaults across
#' every faceted plot in the package.
#'
#' @noRd
mvgam_facet_series <- function(scales = "free_y", ncol = NULL) {
  ggplot2::facet_wrap(~series, scales = scales, ncol = ncol)
}

#' Thin ggrepel wrapper for label-heavy plots (the ordination
#' surface in particular). Falls back to plain `geom_text` /
#' `geom_label` when ggrepel is not installed so the function
#' degrades gracefully.
#'
#' @noRd
mvgam_repel_layer <- function(
  data,
  mapping,
  type = c("text", "label"),
  ...
) {
  type <- match.arg(type)
  has_repel <- requireNamespace("ggrepel", quietly = TRUE)
  fn <- if (has_repel && type == "text") {
    ggrepel::geom_text_repel
  } else if (has_repel) {
    ggrepel::geom_label_repel
  } else if (type == "text") {
    ggplot2::geom_text
  } else {
    ggplot2::geom_label
  }
  fn(data = data, mapping = mapping, ...)
}


#' Diverging fill scale for correlation / precision heatmaps.
#'
#' Symmetric `scale_fill_gradient2` centred at zero with the
#' canonical sign convention: cool blue for large-negative
#' associations, grey at zero, warm red for large-positive ones.
#' Sign conventions for correlation heatmaps are independent of
#' the active bayesplot single-hue scheme (diverging plots need
#' both warm and cool ends), so the colours are read from the
#' fixed bayesplot `"red"` and `"blue"` schemes regardless of
#' the caller's `color_scheme_set`. Defaults to the `[-1, 1]`
#' correlation range; pass `limits = c(-L, L)` to widen for
#' precision matrices.
#'
#' @noRd
mvgam_diverging_scale <- function(
  name = "correlation",
  limits = c(-1, 1),
  breaks = pretty_symmetric_breaks(limits),
  na.value = "transparent"
) {
  warm <- mvgam_palette("red")[5L]
  cool <- mvgam_palette("blue")[5L]
  ggplot2::scale_fill_gradient2(
    name = name,
    low = cool, mid = "grey95", high = warm,
    midpoint = 0,
    limits = limits,
    breaks = breaks,
    na.value = na.value
  )
}


#' Five evenly spaced legend breaks across a symmetric range,
#' rounded to something a reader can take in. The arithmetic
#' sequence is exact and unreadable once the limits come from the
#' data rather than from the unit interval: a precision heatmap
#' scaled to its own entries labelled its legend to seven decimal
#' places.
#'
#' @param limits Numeric length-2 range, symmetric about zero.
#' @return Numeric vector of breaks.
#'
#' @noRd
pretty_symmetric_breaks <- function(limits) {
  checkmate::assert_numeric(limits, len = 2L, any.missing = FALSE)
  raw <- seq(limits[1L], limits[2L], length.out = 5L)
  # Two significant digits on the half-range keeps the endpoints
  # distinct without carrying the full float.
  digits <- max(1L, 2L - ceiling(log10(max(abs(limits), 1e-8))))
  unique(round(raw, digits))
}


#' Ordered evidence levels for a correlation heatmap.
#'
#' `Pr(sign)` is the larger of `Pr(r > 0)` and `Pr(r < 0)`, so it
#' runs from 0.5 (the sign is a coin flip) to 1. Cutting it into a
#' few ordered bins follows `bayesplot::mcmc_rhat()`, which cuts a
#' continuous diagnostic at fixed breaks and shows the bins with the
#' colour scheme's graded steps. The point is that the bins are
#' ordered rather than binary: a correlation the data barely resolve
#' is drawn faintly, not erased.
#'
#' @param p Numeric vector of `Pr(sign)` values.
#' @return Ordered factor with three levels.
#'
#' @noRd
residcor_evidence_bin <- function(p) {
  cut(
    p,
    breaks = c(-Inf, 0.75, 0.95, Inf),
    labels = c("< 0.75", "0.75 - 0.95", "> 0.95"),
    ordered_result = TRUE
  )
}


#' Evidence levels, named so a legend key is drawn for each.
#' @noRd
residcor_evidence_levels <- function() {
  levels(residcor_evidence_bin(0.5))
}


#' Long-format lower triangle of an estimate matrix and its
#' matching evidence matrix, ready for a heatmap.
#'
#' The upper triangle and the diagonal are not returned at all: a
#' symmetric matrix says everything once, and the diagonal is 1 for
#' any correlation.
#'
#' @param estimate Symmetric matrix of posterior point estimates.
#' @param evidence Matrix of `Pr(sign)`, same dimensions.
#' @param cluster Reorder both by `cluster_cormat()`.
#' @return Data frame with `Var1`, `Var2`, `value` and `ev`.
#'
#' @noRd
residcor_panel_data <- function(estimate, evidence, cluster = FALSE) {
  # A single series has no off-diagonal entry, so the lower triangle
  # is empty and there is no panel to draw. Refusing here names the
  # reason, where the empty frame would fail further downstream.
  checkmate::assert_matrix(estimate, mode = "numeric", min.rows = 2L)
  checkmate::assert_matrix(
    evidence, mode = "numeric", nrows = nrow(estimate),
    ncols = ncol(estimate)
  )
  checkmate::assert_flag(cluster)
  if (isTRUE(cluster)) {
    idx <- cluster_cormat(estimate)
    estimate <- estimate[idx, idx]
    evidence <- evidence[idx, idx]
  }
  keep <- lower.tri(estimate)
  out <- expand.grid(dimnames(estimate))
  colnames(out) <- c("Var1", "Var2")
  out$value <- as.vector(estimate)
  out$ev <- residcor_evidence_bin(as.vector(evidence))
  out[as.vector(keep), , drop = FALSE]
}


#' One invisible row per evidence level, so every level has a
#' legend key.
#'
#' ggplot builds a key from the rows a layer holds, so a level the
#' data never reach draws its label with no swatch beside it. On a
#' fit where nothing clears `Pr(sign) > 0.75` that emptied two
#' thirds of the legend. These rows render nothing: no fill, no
#' border.
#'
#' @param panel Output of `residcor_panel_data()`.
#' @return `panel` with one row per evidence level.
#'
#' @noRd
residcor_legend_filler <- function(panel) {
  lv <- residcor_evidence_levels()
  filler <- panel[rep(1L, length(lv)), , drop = FALSE]
  filler$ev <- factor(lv, levels = lv, ordered = TRUE)
  filler
}


#' Opacity scale for the evidence bins.
#'
#' Reads on its own greyscale rather than borrowing the diverging
#' hue, since it grades confidence rather than correlation. The
#' floor sits well above zero so the least certain bin is still
#' legible.
#'
#' @noRd
residcor_evidence_scale <- function() {
  ggplot2::scale_alpha_ordinal(
    name = "Pr(sign)",
    range = c(0.55, 1),
    limits = residcor_evidence_levels(),
    drop = FALSE,
    na.translate = FALSE
  )
}


#' Guide order for a residual-correlation heatmap: the estimate's
#' colour bar first, the evidence legend under it.
#' @noRd
residcor_guides <- function() {
  ggplot2::guides(
    fill = ggplot2::guide_colourbar(order = 1),
    alpha = ggplot2::guide_legend(
      order = 2,
      override.aes = list(fill = "grey45")
    )
  )
}


#' Melt a symmetric matrix into a long data.frame keyed by
#' `Var1` / `Var2` with the numeric entries in a single `value`
#' column. By default the upper triangle and the diagonal are
#' set to `NA` before melting so the heatmap shows only the
#' lower triangle of off-diagonal entries (suits a correlation
#' matrix where the diagonal is uninformative). Toggle
#' `drop_diag` / `drop_upper` to keep both for use cases like
#' a covariance matrix where the diagonal carries inferential
#' signal or the symmetric upper triangle helps readability.
#'
#' @noRd
gather_matrix <- function(mat, drop_diag = TRUE, drop_upper = TRUE) {
  if (drop_upper) mat[upper.tri(mat)] <- NA
  if (drop_diag) diag(mat) <- NA
  if (is.null(dimnames(mat))) {
    grid <- expand.grid(seq.int(NROW(mat)), seq.int(NCOL(mat)))
  } else {
    grid <- expand.grid(dimnames(mat))
  }
  out <- as.data.frame(cbind(grid, value = as.vector(mat)))
  colnames(out) <- c("Var1", "Var2", "value")
  out
}


#' Reorder a symmetric correlation matrix using approximate
#' Robinson ordering (Gruvaeus & Wainer 1972), so visually
#' coherent positive and negative clusters sit adjacent in the
#' heatmap. Distances use `1 - cormat`, a valid non-negative
#' metric where r = 1 → d = 0, r = 0 → d = 1, r = -1 → d = 2,
#' which preserves sign by treating large-negative correlations
#' as maximally distant from large-positive ones. Linkage is
#' `"average"` (UPGMA), which produces more interpretable
#' species groupings than complete linkage for correlation
#' distances. Credit for `reorder_clusters` goes to the
#' maintainers of the `gclus` R package.
#'
#' @importFrom stats hclust as.dist
#' @noRd
cluster_cormat <- function(cormat, ...) {
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


#'@noRd
reorder_clusters <- function(x, dis, ...) {
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
#' Single entry point that returns a `[ndraws, n_series, n_lv]`
#' array of Z loadings regardless of whether Z was sampled at
#' fit time (default factor model) or fixed by the user via
#' `trend_map` (the fixed-Z path). Everything that needs Z
#' (sample_innovations, residual_cor, plot_factors and
#' ordinate.jsdgam) calls this resolver so the fixed-vs-sampled
#' decision lives in exactly one place.
#'
#' Two callable styles:
#' - Object-style: `resolve_factor_loadings(object = fit)`. The
#'   resolver pulls `fixed_Z`, `n_lv`, `n_series` from the fit and
#'   extracts draws as needed. All explicit args are optional.
#' - Explicit-style: `resolve_factor_loadings(fixed_Z = ...,
#'   draws_mat = ..., n_series = ..., n_lv = ...)`. `n_series`
#'   and `n_lv` are REQUIRED in this mode (no fallback derivation
#'   without `object`). Used by hot paths (e.g.
#'   `draw_innovation_grid()`) that already hold pre-extracted
#'   draws and bookkeeping scalars.
#'
#' When `fixed_Z` is fully populated (no NAs) the matrix is
#' broadcast across `ndraws`. Otherwise the resolver delegates
#' @param basis `"identified"` reads the QR-rotated `Z_tilde`,
#'   which is what reporting and plotting want. `"model"` reads
#'   the raw `Z` the model sampled, which is what any caller
#'   combining loadings with `sigma_trend`, `Sigma_trend` or
#'   `Omega_trend` needs, since those live in the unrotated
#'   basis. A fixed `trend_map` has no rotation, so both agree.
#'
#' to `extract_Z_loadings()`, which prefers the QR-identified
#' `Z_tilde[i, j]` draws (free-Z factor models) and falls back
#' to `Z[i, j]` for partial-Z fits where the user-supplied
#' pattern is preserved without rotation.
#'
#' @param object Fitted mvgam object (object-style entry).
#'   Required when callers omit `n_series`/`n_lv`.
#' @param draws_mat Optional pre-extracted posterior draws.
#'   Required when `object` is omitted and `fixed_Z` is NULL.
#' @param fixed_Z Optional explicit fixed-loadings matrix; the
#'   normal path pulls this from `object$trend_metadata`.
#' @param n_lv Integer factor dimension. Required in
#'   explicit-style; derived from `object` otherwise.
#' @param n_series Integer series count. Required in
#'   explicit-style; derived from `object` otherwise.
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
  if (!is.null(object)) {
    checkmate::assert_class(object, "mvgam")
    if (is.null(fixed_Z)) {
      fixed_Z <- object$trend_metadata$fixed_Z
    }
    if (is.null(n_lv)) {
      n_lv <- object$trend_metadata$n_lv %||%
        (if (!is.null(fixed_Z)) ncol(fixed_Z) else NULL)
    }
    if (is.null(n_series)) {
      n_series <- object$series_info$n_series %||%
        (if (!is.null(fixed_Z)) nrow(fixed_Z) else NULL)
    }
  }
  checkmate::assert_int(n_lv, lower = 1L)
  checkmate::assert_int(n_series, lower = 1L)

  # Partial Z (some entries NA = sampled): the free entries are
  # saved as `Z[i, j]` in the posterior alongside the fixed
  # entries, so fall through to the column-major draws parser.
  fully_fixed <- !is.null(fixed_Z) && !anyNA(fixed_Z)
  if (fully_fixed) {
    # Reason: fixed-Z fits store Z in standata, so there are no
    # `Z[i, j]` posterior columns to read. Broadcast the matrix
    # across ndraws to match the sampled-Z return shape.
    ndraws <- if (!is.null(draws_mat)) {
      nrow(draws_mat)
    } else if (!is.null(object)) {
      posterior::ndraws(posterior::as_draws_matrix(object$fit))
    } else {
      stop(insight::format_error(c(
        "Cannot resolve ndraws for fixed-Z broadcast.",
        i = paste0("Supply 'object' or 'draws_mat'. The resolver ",
                   "reads the draw count from either one.")
      )))
    }
    return(array(
      rep(as.numeric(unname(fixed_Z)), each = ndraws),
      dim = c(ndraws, n_series, n_lv)
    ))
  }

  if (is.null(draws_mat)) {
    if (is.null(object)) {
      stop(insight::format_error(c(
        "Cannot extract sampled Z without draws.",
        i = "Supply 'object' or 'draws_mat'."
      )))
    }
    draws_mat <- posterior::as_draws_matrix(object$fit)
  }
  extract_Z_loadings(draws_mat, n_obs_series = n_series,
                     n_lv = n_lv, basis = basis)
}
