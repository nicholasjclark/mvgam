context("mvgam_priors")

expect_match2 <- function(object, regexp) {
  any(grepl(regexp, object, fixed = TRUE))
}

test_that("family must be correctly specified", {
  expect_error(get_mvgam_priors(y ~ s(season),
                                trend_model = 'AR1',
                                data = beta_data$data_train,
                                family = 'besta'),
               'family not recognized')
})

test_that("trend_model must be correctly specified", {
  expect_error(get_mvgam_priors(y ~ s(season),
                                trend_model = 'AR11',
                                data = beta_data$data_train,
                                family = betar()))
})

test_that("specified priors appear in the Stan code", {

  priors <- get_mvgam_priors(y ~ s(season, bs = 'cc'),
                             trend_model = 'GP',
                             data = beta_data$data_train,
                             family = betar())
  priors$prior[2] <- "alpha_gp ~ normal(-1, 0.75);"
  stancode <- mvgam(y ~ s(season, bs = 'cc'),
                    trend_model = 'GP',
                    data = beta_data$data_train,
                    family = betar(),
                    priors = priors,
                    run_model = FALSE)$model_file
  expect_true(expect_match2(stancode,
                            'alpha_gp ~ normal(-1, 0.75);'))
})
