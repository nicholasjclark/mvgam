# plot.mvgam_forecast tests using hand-built minimal forecast
# objects — no Stan fit dependency.

# Build a tiny mvgam_forecast with controllable hindcast / forecast
# arms and an arbitrary number of series. Cells default to draws
# of N(series_offset, 1) so series can be distinguished visually
# in the local renderer.
.make_fc <- function(
  series = c("a", "b"),
  n_train = 10L,
  n_test = 5L,
  ndraws = 40L,
  include_forecast = TRUE,
  include_train_obs = TRUE,
  include_test_obs = TRUE
) {
  set.seed(1L)
  train_times <- stats::setNames(
    lapply(series, function(.) seq_len(n_train)),
    series
  )
  test_times <- stats::setNames(
    lapply(series, function(.) n_train + seq_len(n_test)),
    series
  )
  hindcasts <- stats::setNames(
    lapply(seq_along(series), function(i) {
      matrix(
        rnorm(ndraws * n_train, mean = i),
        nrow = ndraws, ncol = n_train
      )
    }),
    series
  )
  forecasts <- stats::setNames(
    lapply(seq_along(series), function(i) {
      matrix(
        rnorm(ndraws * n_test, mean = i),
        nrow = ndraws, ncol = n_test
      )
    }),
    series
  )
  train_obs <- stats::setNames(
    lapply(seq_along(series), function(i) {
      rnorm(n_train, mean = i)
    }),
    series
  )
  test_obs <- stats::setNames(
    lapply(seq_along(series), function(i) {
      rnorm(n_test, mean = i)
    }),
    series
  )
  structure(
    list(
      call = NULL,
      family = "gaussian",
      type = "response",
      series_names = factor(series, levels = series),
      train_observations = if (include_train_obs) train_obs else NULL,
      train_times = train_times,
      test_observations = if (include_test_obs &&
                                include_forecast) {
        test_obs
      } else {
        NULL
      },
      test_times = if (include_forecast) test_times else NULL,
      hindcasts = hindcasts,
      forecasts = if (include_forecast) forecasts else NULL
    ),
    class = "mvgam_forecast"
  )
}

# Helper: count layer instances by ggproto type from a built
# ggplot.
.layer_geoms <- function(p) {
  vapply(
    p$layers,
    function(l) class(l$geom)[1L],
    character(1L)
  )
}

test_that("plot.mvgam_forecast returns a ggplot", {
  fc <- .make_fc()
  p <- plot(fc)
  expect_ggplot(p)
})

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

test_that("invalid series name errors with the available list", {
  fc <- .make_fc(series = c("a", "b"))
  expect_error(plot(fc, series = "zzz"), "Unknown series name")
})

test_that("both arms FALSE errors clearly", {
  fc <- .make_fc()
  expect_error(
    plot(fc, hindcast = FALSE, forecast = FALSE),
    "At least one"
  )
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

test_that("y-axis label names the series when single-series", {
  fc <- .make_fc(series = c("a", "b"))
  p <- plot(fc, series = "b")
  expect_match(p$labels$y, "Predictions for b")
})
