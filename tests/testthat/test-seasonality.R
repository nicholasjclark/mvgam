# The calendar positions and cycle statistics of seasonality().
# tests/local/test-trend-llt.R checks its season draws against fitted
# models.

test_that("positions come from the calendar column and then continue", {
  # Monthly data starting in November. The future frame lacks the
  # calendar column. Its times continue from the last month.
  frame <- data.frame(time = 1:14, season = c(11L, 12L, 1:12))
  layout <- season_layout(times = 1:16, n_fitted = 14L, period = 12,
                          frames = list(frame, data.frame(time = 15:16)),
                          column = "season", time_var = "time")
  expect_identical(layout$position, c(11L, 12L, 1:12, 1:2))
  expect_identical(as.character(layout$label[c(1L, 3L)]), c("Nov", "Jan"))
  expect_identical(layout$cycle, rep(1:3, c(2L, 12L, 2L)))
  # The first and last cycles miss positions
  expect_identical(unique(layout$cycle[layout$complete]), 2L)

  # Without the column, position 1 is the first time and the labels
  # are numbers
  plain <- season_layout(times = 1:8, n_fitted = 8L, period = 4,
                         frames = list(data.frame(time = 1:8)),
                         column = "season", time_var = "time")
  expect_identical(plain$position, rep(1:4, 2L))
  expect_identical(levels(plain$label), as.character(1:4))
})

test_that("a fractional period's short cycles count as complete", {
  # Steps 0 to 9 of a period of 2.5 fall at positions 1 2 3 1 2 1 2 3 1 2
  frame <- data.frame(time = 1:10,
                      season = c(1L, 2L, 3L, 1L, 2L, 1L, 2L, 3L, 1L, 2L))
  layout <- season_layout(times = 1:10, n_fitted = 10L, period = 2.5,
                          frames = list(frame), column = "season",
                          time_var = "time")
  expect_identical(layout$cycle, rep(1:4, c(3L, 2L, 3L, 2L)))
  expect_true(all(layout$complete))
})

test_that("a peak shift takes the shorter way around the period", {
  expect_identical(circular_shift(c(12, 1, 3), c(1, 12, 9), 12), c(1, -1, -6))
})
