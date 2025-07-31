test_that("CAR constructor rejects factor models correctly", {
  # CAR with n_lv should error immediately
  expect_error(CAR(n_lv = 2), 
               "Factor models.*not supported for CAR trends")
  expect_error(CAR(n_lv = 2), 
               "irregular time intervals")
  expect_error(CAR(n_lv = 2), 
               "Remove.*n_lv.*parameter")
  
  # CAR without n_lv should work
  suppressWarnings({
    car_trend <- CAR()
    expect_is(car_trend, "mvgam_trend")
    expect_equal(car_trend$trend_model, "CAR")
  })
})

test_that("PW constructor rejects factor models correctly", {
  # PW with n_lv should error immediately
  expect_error(PW(n_lv = 3), 
               "Factor models.*not supported for PW trends")
  expect_error(PW(n_lv = 3), 
               "changepoint modeling")
  expect_error(PW(n_lv = 3), 
               "Remove.*n_lv.*parameter")
  
  # PW without n_lv should work
  suppressWarnings({
    pw_trend <- PW()
    expect_is(pw_trend, "mvgam_trend")
    expect_true(pw_trend$trend_model %in% c("PWlinear", "PWlogistic"))
  })
})

test_that("AR constructor accepts factor models", {
  # AR with n_lv should work
  suppressWarnings({
    ar_trend <- AR(n_lv = 2)
    expect_is(ar_trend, "mvgam_trend")
    expect_equal(ar_trend$n_lv, 2)
    expect_equal(ar_trend$trend_model, "AR1")
  })
  
  # AR without n_lv should work
  suppressWarnings({
    ar_trend_no_lv <- AR()
    expect_is(ar_trend_no_lv, "mvgam_trend")
    expect_null(ar_trend_no_lv$n_lv)
  })
})

test_that("RW constructor accepts factor models", {
  # RW with n_lv should work
  suppressWarnings({
    rw_trend <- RW(n_lv = 3)
    expect_is(rw_trend, "mvgam_trend")
    expect_equal(rw_trend$n_lv, 3)
  })
  
  # RW without n_lv should work
  suppressWarnings({
    rw_trend_no_lv <- RW()
    expect_is(rw_trend_no_lv, "mvgam_trend")
    expect_null(rw_trend_no_lv$n_lv)
  })
})

test_that("VAR constructor accepts factor models", {
  # VAR with n_lv should work
  suppressWarnings({
    var_trend <- VAR(n_lv = 4)
    expect_is(var_trend, "mvgam_trend")
    expect_equal(var_trend$n_lv, 4)
  })
  
  # VAR without n_lv should work
  suppressWarnings({
    var_trend_no_lv <- VAR()
    expect_is(var_trend_no_lv, "mvgam_trend")
    expect_null(var_trend_no_lv$n_lv)
  })
})

test_that("Factor validation error messages are consistent", {
  # Check that all factor-incompatible trends give similar error structure
  
  expect_error(CAR(n_lv = 1), "Factor models.*not supported")
  expect_error(PW(n_lv = 1), "Factor models.*not supported")
  
  # Check they mention specific alternatives
  expect_error(CAR(n_lv = 1), "factor-compatible trends.*AR.*RW.*VAR.*ZMVN")
  expect_error(PW(n_lv = 1), "factor-compatible trends.*AR.*RW.*VAR.*ZMVN")
  
  # Check they have specific reasons
  expect_error(CAR(n_lv = 1), "irregular time")
  expect_error(PW(n_lv = 1), "changepoint")
})