# The tests of test-plot-mvgam-forecast.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

# Helper: count layer instances by ggproto type from a built
# ggplot.
.layer_geoms <- function(p) {
  vapply(
    p$layers,
    function(l) class(l$geom)[1L],
    character(1L)
  )
}

test_that("default plot faceted when >1 series", {
  fc <- .make_fc(series = c("a", "b", "c"))
  p <- plot(fc)
  expect_true(inherits(p$facet, "FacetWrap"))
})

test_that("single-series request drops the facet wrap", {
  fc <- .make_fc(series = c("a", "b"))
  p <- plot(fc, series = "a")
  expect_false(inherits(p$facet, "FacetWrap"))
})

test_that("cut line drawn only when both arms are present", {
  both <- plot(.make_fc())
  geoms_both <- .layer_geoms(both)
  expect_true("GeomVline" %in% geoms_both)

  fc_only <- plot(.make_fc(), hindcast = FALSE)
  expect_false("GeomVline" %in% .layer_geoms(fc_only))

  hc_only_obj <- .make_fc(include_forecast = FALSE)
  hc_only <- plot(hc_only_obj)
  expect_false("GeomVline" %in% .layer_geoms(hc_only))
})

test_that("hindcast-only object yields no GeomRibbon from the forecast arm", {
  fc <- .make_fc(include_forecast = FALSE)
  p <- plot(fc)
  # Single grey ribbon per series; no multi-band stack.
  expect_equal(
    sum(.layer_geoms(p) == "GeomRibbon"),
    length(fc$series_names)
  )
})

test_that("multi-band forecast emits one ribbon per prob, per series", {
  fc <- .make_fc(series = c("a", "b"))
  p <- plot(fc, probs = c(0.5, 0.8, 0.95))
  geoms <- .layer_geoms(p)
  # 2 series x (1 hindcast ribbon + 3 forecast ribbons) = 8.
  expect_equal(sum(geoms == "GeomRibbon"), 8L)
  # The forecast bands start at the last training time and join the
  # hindcast. Drawn alone, they start at the first forecast time.
  # Layers 1 and 2 are the hindcast band and median.
  expect_identical(min(p$layers[[3L]]$data$time), 10L)
  alone <- plot(fc, hindcast = FALSE, probs = 0.5)
  expect_identical(min(alone$layers[[1L]]$data$time), 11L)
})

test_that("observation overlay can be suppressed via newdata_obs=FALSE", {
  fc <- .make_fc()
  with_obs <- plot(fc, newdata_obs = TRUE)
  without_obs <- plot(fc, newdata_obs = FALSE)
  # Train + test obs are concatenated into the same point layers,
  # so disabling test obs drops POINTS not layers. Count the rows.
  pt_rows_with <- sum(vapply(
    with_obs$layers,
    function(l) {
      if (inherits(l$geom, "GeomPoint")) nrow(l$data) else 0L
    },
    integer(1L)
  ))
  pt_rows_without <- sum(vapply(
    without_obs$layers,
    function(l) {
      if (inherits(l$geom, "GeomPoint")) nrow(l$data) else 0L
    },
    integer(1L)
  ))
  expect_lt(pt_rows_without, pt_rows_with)
})

test_that("series='all' is equivalent to NULL", {
  fc <- .make_fc(series = c("a", "b", "c"))
  p_null <- plot(fc, series = NULL)
  p_all <- plot(fc, series = "all")
  expect_equal(length(p_null$layers), length(p_all$layers))
})

test_that("character vector of series subsets the panels", {
  fc <- .make_fc(series = c("a", "b", "c"))
  p <- plot(fc, series = c("a", "c"))
  expect_ggplot(p)
  # Hindcast + multi-band forecast contribute equally per series:
  # 2 series should yield exactly 2/3 of the all-series layers.
  expect_lt(
    length(p$layers),
    length(plot(fc, series = NULL)$layers)
  )
})

test_that("y-axis label names one series picked from several", {
  fc <- .make_fc(series = c("a", "b"))
  expect_identical(plot(fc, series = "b")$labels$y, "Predictions for b")
  expect_identical(plot(.make_fc(series = "a"))$labels$y, "Predictions")
})
