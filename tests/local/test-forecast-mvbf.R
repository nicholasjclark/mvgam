# Forecasting a fit whose series are its responses.
#
# A wide `mvbf()` frame holds one row per time and one column per
# response, so the series an observation sits on belongs to the
# `(row, response)` pair. `forecast()` reaches every fit through
# `resolve_forecast_grid()`, which placed rows by reading a
# `series` column; a wide frame has none, so every `mvbf()` and
# `jsdgam()` fit came back with an empty horizon and no complaint.
# Nothing here needed a new fixture: five multivariate fits were
# already cached and none of them had ever been forecast.

CACHE_DIR <- "fixtures"

mvbf_cases <- list(
  list(file = "val_mvgam_mv_na_gaps.rds", label = "two responses, gappy"),
  list(file = "val_mvgam_mv_nocol.rds", label = "two responses"),
  list(file = "val_mvgam_mv_three_odd.rds", label = "three responses"),
  list(file = "val_jsdgam_mv_families.rds", label = "jsdgam species")
)


test_that("a wide fit forecasts every response it was fitted on", {
  for (case in mvbf_cases) {
    fit <- readRDS(file.path(CACHE_DIR, case$file))
    responses <- fit$response_names
    axis <- as.character(mvgam:::mvgam_axes(fit)$series$levels)
    # The record names the series, and on a wide fit those are the
    # responses in formula order.
    expect_identical(axis, as.character(responses))

    last <- max(fit$data[[fit$trend_metadata$variables$time_var]])
    h <- 3L
    newdata <- fit$data[
      fit$data[[fit$trend_metadata$variables$time_var]] == last, ,
      drop = FALSE
    ]
    newdata <- do.call(rbind, lapply(seq_len(h), function(k) {
      row <- newdata
      row[[fit$trend_metadata$variables$time_var]] <- last + k
      row
    }))

    fc <- forecast(fit, newdata = newdata, type = "link")

    # Every response is forecast, over the horizon it was given.
    # An empty arm is what the defect produced, so the length is
    # the assertion that matters rather than the values.
    for (resp in responses) {
      arm <- fc$forecasts[[resp]]
      expect_false(
        is.null(arm),
        label = paste(case$label, resp, "is forecast at all")
      )
      expect_identical(
        ncol(arm), h,
        label = paste(case$label, resp, "spans the horizon asked for")
      )
      expect_false(
        all(is.na(arm)),
        label = paste(case$label, resp, "carries draws")
      )
    }
    # And no response reads another's state: two responses of one
    # fit forecast from different latent series, so identical arms
    # would mean both took the same column.
    if (length(responses) > 1L) {
      first <- fc$forecasts[[responses[1L]]]
      second <- fc$forecasts[[responses[2L]]]
      expect_false(
        isTRUE(all.equal(as.numeric(first), as.numeric(second))),
        label = paste(case$label, "responses forecast their own series")
      )
    }
  }
})


test_that("a wide fit refuses a horizon it cannot step to", {
  # The grid a discrete-time trend steps along is shared by every
  # response, so a frame that skips occasions is refused rather
  # than forecast as though the gap were not there.
  fit <- readRDS(file.path(CACHE_DIR, "val_mvgam_mv_three_odd.rds"))
  time_var <- fit$trend_metadata$variables$time_var
  last <- max(fit$data[[time_var]])
  newdata <- fit$data[fit$data[[time_var]] == last, , drop = FALSE]
  newdata[[time_var]] <- last + 10L
  expect_error(
    forecast(fit, newdata = newdata, type = "link"),
    regexp = "continue the training series"
  )
})
