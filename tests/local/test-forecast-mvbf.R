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

    # A wide fit answers with one forecast object per response,
    # each carrying an arm per series, so a response's own arm is
    # `fc[[resp]]$forecasts[[resp]]`.
    expect_identical(
      names(fc), as.character(responses),
      label = paste(case$label, "answers for every response")
    )

    # Every response is forecast, over the horizon it was given.
    # An empty arm is what the defect produced, so the length is
    # the assertion that matters rather than the values.
    for (resp in responses) {
      expect_identical(
        as.character(fc[[resp]]$series_names), as.character(responses),
        label = paste(case$label, resp, "names the whole axis")
      )
      arm <- fc[[resp]]$forecasts[[resp]]
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
      first <- fc[[responses[1L]]]$forecasts[[responses[1L]]]
      second <- fc[[responses[2L]]]$forecasts[[responses[2L]]]
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
  #
  # The fixture has to carry a trend that steps. `mv_three_odd` is
  # `ZMVN()`, which has no temporal structure and is exempt by the
  # registry's own rule, so asserting a refusal there asserted the
  # opposite of the contract. `mv_na_gaps` is `RW()`.
  fit <- readRDS(file.path(CACHE_DIR, "val_mvgam_mv_na_gaps.rds"))
  time_var <- fit$trend_metadata$variables$time_var
  last <- max(fit$data[[time_var]])
  newdata <- fit$data[fit$data[[time_var]] == last, , drop = FALSE]
  newdata[[time_var]] <- last + 10L
  expect_error(
    forecast(fit, newdata = newdata, type = "link"),
    regexp = "continue the training series"
  )
})


test_that("each response reads its own latent column, per the draws", {
  # Shapes agreeing says every arm was produced. It does not say
  # each response read the column the sampler drew for it, and a
  # permutation of the responses over the columns keeps every shape
  # intact while giving each species another's state.
  #
  # `obs_trend_series_<resp>` is the column the sampler indexed for
  # that response, so it settles the question outright.
  for (case in mvbf_cases) {
    fit <- readRDS(file.path(CACHE_DIR, case$file))
    responses <- fit$response_names
    n_col <- as.integer(fit$standata$N_series_trend)

    # Every response sits on exactly one column, and between them
    # they use the whole axis in the order the formula names them.
    columns <- vapply(responses, function(r) {
      idx <- unique(as.integer(
        fit$standata[[paste0("obs_trend_series_", r)]]
      ))
      if (length(idx) == 1L) idx else NA_integer_
    }, integer(1), USE.NAMES = FALSE)
    expect_false(
      anyNA(columns),
      label = paste(case$label, "no response spans two columns")
    )
    expect_identical(
      columns, seq_along(responses),
      label = paste(case$label, "responses run in formula order")
    )
    expect_identical(
      as.integer(n_col), length(responses),
      label = paste(case$label, "the axis is as wide as the responses")
    )

    # The record names those columns, so its labels have to be the
    # responses in that same order.
    expect_identical(
      as.character(mvgam:::mvgam_axes(fit)$series$levels),
      as.character(responses),
      label = paste(case$label, "the record names the columns")
    )

    # And the states really are distinct: two responses sharing one
    # column is the shape the defect took, and it survives every
    # check above that counts rather than compares.
    dm <- posterior::as_draws_matrix(fit$fit)
    state_of <- function(k) {
      vapply(seq_len(as.integer(fit$standata$N_time_trend)), function(t) {
        mean(dm[, paste0("trend[", t, ",", k, "]")])
      }, numeric(1))
    }
    first <- state_of(columns[1L])
    second <- state_of(columns[2L])
    expect_false(
      isTRUE(all.equal(first, second)),
      label = paste(case$label, "responses hold their own state")
    )
  }
})
