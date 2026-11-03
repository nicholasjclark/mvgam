#' Partition each series' predictor variance among its sources
#'
#' @description
#' Shows what moves each series: its covariates, the latent factors it
#' shares with other series, its own trend, its season and, in a
#' detection model, its detection process. Each source's share of the
#' variance is computed draw by draw, which gives every share a
#' posterior distribution.
#'
#' @param object A fitted `mvgam` object.
#' @param ndraws,draw_ids The posterior draws to use, as
#'   [hindcast.mvgam()] takes them. `NULL` for both uses every draw.
#' @param scale `"link"` (the default) partitions the predictor, where
#'   its parts add. `"expected"` partitions the expected value: a
#'   source's values are the expected value less the expected value
#'   without that source.
#' @param resp The response to report, needed on a model with several.
#' @param ... Unused. Anything passed here is refused.
#'
#' @details
#' *Sources*. Each source is a sum of parts of the fitted predictor:
#' \describe{
#'   \item{`environment`}{every term of the observation and trend
#'     formulas, group-level terms included}
#'   \item{`associations`}{the latent factors, shared across series}
#'   \item{`trend`}{a latent state belonging to its series alone}
#'   \item{`season`}{the season of an [LLT()] trend}
#'   \item{`detection`}{the terms of an `occ()` or `nmix()` detection
#'     formula, `p ~ ...`, on that formula's own link scale. It joins
#'     `scale = "link"` alone.}
#' }
#' A source a model lacks or that never varies is left out.
#'
#' Under a non-linear link, `scale = "expected"` holds the other sources
#' at their fitted values. Its sources then interact, as a covariate's
#' effect on a count grows with the level.
#'
#' *Shares*. A source's share is the variance of its values over the
#' fitted rows of a series, divided by the sum of those variances
#' across sources. Covariances between sources are left out. They would
#' give a negative share to a source that offsets another. Observation
#' noise is left out too: the shares describe the predictor, on a scale
#' every family has. A series whose predictor never moves has no share.
#'
#' A series row reports what moves that series over its own rows. An
#' effect that differs only between series, such as a covariate measured
#' once per site, has no variance there. The community row pools every
#' row and counts those effects.
#'
#' @return An object of class `mvgam_variance_partition`. `print()` gives
#'   the mean share of each source by series. `summary()` returns a
#'   tibble of each share's median and interval. `plot()` draws one
#'   stacked bar per series and one for the community.
#'
#' @seealso [residual_cor()] for the associations between series,
#'   [seasonality()] for the season, [bayes_R2()] for the share of the
#'   response's variance the predictor explains,
#'   [forecast_uncertainty()] for the variance of a forecast
#'
#' @examples
#' \dontrun{
#' set.seed(4)
#' simdat <- sim_mvgam(family = poisson(), n_series = 4L,
#'                     n_timepoints = 80L, trend_model = AR())
#' mod <- mvgam(
#'   y ~ s(x, k = 5),
#'   trend_formula = ~ AR(p = 1, n_lv = 2),
#'   data = simdat$data_train,
#'   family = poisson(),
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # The covariate's share against the shared factors', by series
#' vp <- variance_partition(mod)
#' vp
#' plot(vp)
#'
#' # Each share with its posterior interval
#' summary(vp)
#' }
#'
#' @author Nicholas J Clark
#' @export
variance_partition <- function(object, ...) {
  UseMethod("variance_partition")
}


#' @rdname variance_partition
#' @method variance_partition mvgam
#' @export
variance_partition.mvgam <- function(object, ..., ndraws = NULL,
                                     draw_ids = NULL,
                                     scale = c("link", "expected"),
                                     resp = NULL) {
  checkmate::assert_class(object, "mvgam")
  rlang::check_dots_empty()
  validate_draw_selectors(ndraws, draw_ids)
  scale <- match.arg(scale)
  require_fitted_model(object, "variance_partition")
  require_series_axis(object, "variance_partition")
  resolve_resp(object, resp, required = TRUE, caller = "variance_partition")
  draw_ids <- resolve_draw_indices(posterior::ndraws(object), ndraws,
                                   draw_ids)
  frame <- fitted_data(object)
  sources <- predictor_sources(object, frame, draw_ids, resp, scale)
  series <- axis_row_series(object, frame) %||%
    factor(rep(resp, nrow(frame)))
  rows <- split(seq_len(nrow(frame)), droplevels(series))
  shares <- lapply(rows, function(at) source_shares(sources, at))
  structure(
    list(
      shares = shares,
      community = source_shares(sources, seq_len(nrow(frame))),
      sources = names(sources),
      draw_ids = draw_ids,
      scale = scale
    ),
    class = "mvgam_variance_partition"
  )
}


#' The fitted predictor summed by source
#'
#' @param object A fitted `mvgam` object
#' @param frame The fitted rows
#' @param draw_ids Indices of the draws
#' @param resp A response name, or NULL
#' @param scale `"link"` or `"expected"`
#' @return Named list of `[ndraws, nrow]` matrices, one per source
#'   that varies over the rows
#' @noRd
predictor_sources <- function(object, frame, draw_ids, resp, scale) {
  parts <- get_linpred_parts(object, frame, draw_ids = draw_ids, resp = resp)
  specs <- if (has_trend_formula(object)) latent_part_specs(object)
  group <- ifelse(specs$part == "season", "season",
                  ifelse(is.na(specs$factor), "trend", "associations"))
  latent <- split(parts$latent[specs$label], group)
  detection <- if (scale == "link" &&
                     is_closure_unit_family(model_families(object, resp)) &&
                     length(predicted_dpar_names(object, "p", resp))) {
    extract_component_linpred(object, frame, component = "p", resp = resp,
                              draw_ids = draw_ids)
  }
  sources <- Filter(Negate(is.null), c(
    list(environment = Reduce(`+`, c(parts$obs, parts$trend))),
    lapply(latent, function(group_parts) Reduce(`+`, group_parts)),
    list(detection = detection)
  ))
  whole <- Reduce(`+`, sources)
  # A source fixed over the rows moves nothing, on any scale
  sources <- Filter(function(values) any(row_var(values) > 0), sources)
  if (scale == "expected") {
    expect <- epred_map(object, frame, draw_ids, resp, dim(whole),
                        ordinal = "mean")
    with_all <- expect(whole)
    sources <- lapply(sources, function(values) {
      with_all - expect(whole - values)
    })
  }
  order <- c("environment", "associations", "trend", "season", "detection")
  sources[intersect(order, names(sources))]
}


#' Each source's share of the variance over some rows, by draw
#'
#' @param sources What `predictor_sources()` gives
#' @param rows The rows of one series
#' @return `[ndraws, n_sources]` matrix, columns named by source
#' @noRd
source_shares <- function(sources, rows) {
  variances <- vapply(sources, function(values) {
    row_var(values[, rows, drop = FALSE])
  }, numeric(nrow(sources[[1L]])))
  variances <- matrix(variances, ncol = length(sources),
                      dimnames = list(NULL, names(sources)))
  variances / rowSums(variances)
}


#' Each series' draws of the shares, the community's last
#'
#' @param x An `mvgam_variance_partition` object
#' @return Named list of `[ndraws, n_sources]` matrices
#' @noRd
partition_rows <- function(x) {
  c(x$shares, list(Community = x$community))
}


#' @param probs Interval probabilities of the table. Defaults to
#'   `c(0.025, 0.975)`.
#' @rdname variance_partition
#' @method summary mvgam_variance_partition
#' @export
summary.mvgam_variance_partition <- function(object,
                                             probs = c(0.025, 0.975), ...) {
  checkmate::assert_numeric(probs, lower = 0, upper = 1, len = 2L,
                            any.missing = FALSE)
  rlang::check_dots_empty()
  rows <- partition_rows(object)
  tibble::as_tibble(do.call(rbind, Map(function(series, shares) {
    cbind(series = series, source = colnames(shares),
          median_interval_cols(shares, probs, ""))
  }, names(rows), rows)))
}


#' @param digits Number of decimal places to print. Defaults to `2`.
#' @rdname variance_partition
#' @method print mvgam_variance_partition
#' @export
print.mvgam_variance_partition <- function(x, digits = 2L, ...) {
  checkmate::assert_count(digits)
  cat("Share of ", variance_scale_label(x$scale), " variance by source, ",
      length(x$draw_ids), " draws\n\n", sep = "")
  print(round(do.call(rbind, lapply(partition_rows(x), colMeans)), digits))
  invisible(x)
}


#' What a variance on a scale is the variance of
#'
#' @param scale `"link"`, `"expected"` or `"response"`
#' @return One string
#' @noRd
variance_scale_label <- function(scale) {
  switch(scale, link = "predictor", expected = "expected value",
         response = "response")
}


#' @param x An `mvgam_variance_partition` object from
#'   [variance_partition()].
#' @rdname variance_partition
#' @method plot mvgam_variance_partition
#' @export
plot.mvgam_variance_partition <- function(x, ...) {
  rlang::check_dots_empty()
  rows <- partition_rows(x)
  means <- do.call(rbind, Map(function(series, shares) {
    data.frame(series = series, source = colnames(shares),
               share = colMeans(shares))
  }, names(rows), rows))
  means$series <- factor(means$series, levels = rev(names(rows)))
  means$source <- factor(means$source, levels = x$sources)
  # The community pools every row, a different quantity from any series
  means$panel <- factor(ifelse(means$series == "Community", "Pooled", "Series"),
                        levels = c("Series", "Pooled"))
  set_color_scheme_local()
  ggplot2::ggplot(
    means,
    ggplot2::aes(fill = .data$source, x = .data$share, y = .data$series)
  ) +
    share_bars(length(x$sources)) +
    share_axis("x") +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$panel), scales = "free_y",
                        space = "free_y") +
    mvgam_theme() +
    ggplot2::theme(strip.text = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank()) +
    ggplot2::labs(x = paste0("Mean share of ", variance_scale_label(x$scale),
                             " variance"),
                  y = NULL)
}
