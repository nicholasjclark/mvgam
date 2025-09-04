context("gp")

skip_on_cran()

test_that("gp_to_s is working properly for unidimensional gps", {
  # All true gp() terms should be changed to s() with k = k+1
  formula <- y ~
    s(series) +
      gp(banana, k = 3, scale = FALSE) +
      infect:you +
      gp(hardcourt, k = 3)
  dat <- data.frame(
    y = rnorm(10),
    series = rnorm(10),
    banana = rnorm(10),
    infect = rnorm(10),
    you = rnorm(10),
    hardcourt = rnorm(10),
    gp = rnorm(10)
  )

  # Check that the brms_mock formula is correctly remade
  gp_atts <- mvgam:::get_gp_attributes(formula, data = dat, family = gaussian())

  # scale should be passed; gr is always false and cmc is always true
  expect_true(identical(
    attr(terms(attr(gp_atts, 'gp_formula')), 'term.labels')[1],
    "gp(banana, k = 3, cov = \"exp_quad\", iso = TRUE, scale = FALSE, c = 1.25, gr = FALSE, cmc = TRUE)"
  ))

  expect_true(identical(
    attr(terms(attr(gp_atts, 'gp_formula')), 'term.labels')[2],
    "gp(hardcourt, k = 3, cov = \"exp_quad\", iso = TRUE, scale = TRUE, c = 1.25, gr = FALSE, cmc = TRUE)"
  ))

  expect_equal(
    attr(
      terms(
        mvgam:::gp_to_s(formula, data = dat, family = gaussian()),
        keep.order = TRUE
      ),
      'term.labels'
    ),
    attr(
      terms(
        formula(
          y ~
            s(series) +
              s(banana, k = 4) +
              infect:you +
              s(hardcourt, k = 4)
        ),
        keep.order = TRUE
      ),
      'term.labels'
    )
  )

  # Characters that match to 'gp' should not be changed
  formula <- y ~ gp(hardcourt, k = 3) + s(gp, k = 3)
  expect_equal(
    attr(
      terms(
        mvgam:::gp_to_s(formula, data = dat, family = gaussian()),
        keep.order = TRUE
      ),
      'term.labels'
    ),
    attr(
      terms(formula(y ~ s(hardcourt, k = 4) + s(gp, k = 3)), keep.order = TRUE),
      'term.labels'
    )
  )
})

test_that("gp_to_s is working properly for multidimensional gps", {
  # All true gp() terms should be changed to s() with k = k+1
  formula <- y ~
    s(series) +
      gp(banana, hardcourt, k = 3, iso = FALSE, c = 1.33, cov = 'matern52') +
      infect:you
  dat <- data.frame(
    y = rnorm(10),
    series = rnorm(10),
    banana = rnorm(10),
    infect = rnorm(10),
    you = rnorm(10),
    hardcourt = rnorm(10),
    gp = rnorm(10)
  )
  gp_atts <- mvgam:::get_gp_attributes(formula, data = dat, family = gaussian())

  expect_true(identical(
    attr(terms(attr(gp_atts, 'gp_formula')), 'term.labels')[1],
    "gp(banana, hardcourt, k = 3, cov = \"matern52\", iso = FALSE, scale = TRUE, c = 1.33, gr = FALSE, cmc = TRUE)"
  ))

  expect_equal(
    attr(
      terms(
        mvgam:::gp_to_s(formula, data = dat, family = gaussian()),
        keep.order = TRUE
      ),
      'term.labels'
    ),
    attr(
      terms(
        formula(
          y ~
            s(series) +
              ti(banana, hardcourt, k = 3, mc = c(0, 0)) +
              infect:you
        ),
        keep.order = TRUE
      ),
      'term.labels'
    )
  )
})

test_that("unidimensional gp for observation models working properly", {
  gaus_data$data_train$y[is.na(gaus_data$data_train$y)] <- 0
  mod <- mvgam(
    formula = y ~
      s(series, bs = 're') +
        gp(time, by = series, k = 10, c = 5 / 4) +
        year:season,
    data = gaus_data$data_train,
    family = gaussian(),
    run_model = FALSE,
    autoformat = FALSE
  )

  expect_true(
    any(grepl(
      'b[b_idx_gp_time_byseriesseries_3] = sqrt(spd_gp_exp_quad(l_gp_time_byseriesseries_3',
      mod$model_file,
      fixed = TRUE
    ))
  )

  # Gp data structures should be in the model_data
  expect_true("l_gp_time_byseriesseries_1" %in% names(mod$model_data))
  expect_true("b_idx_gp_time_byseriesseries_1" %in% names(mod$model_data))
  expect_true("k_gp_time_byseriesseries_1" %in% names(mod$model_data))

  # These should match to the eigenvalues and eigenfunctions created by
  # a similar brms call
  brms_dat <- suppressWarnings(brms::make_standata(
    y ~
      s(series, bs = 're') +
        gp(time, by = series, k = 10, c = 5 / 4) +
        year:season,
    data = gaus_data$data_train,
    family = gaussian()
  ))
  # Eigenvalues should be identical
  expect_true(all.equal(
    as.vector(brms_dat$slambda_1_1),
    as.vector(mod$model_data$l_gp_time_byseriesseries_1)
  ))

  # Eigenfunctions will be nearly identical
  row_s1 <- which(
    gaus_data$data_train$series == 'series_1' &
      !is.na(gaus_data$data_train$y)
  )
  col_s1 <- grep(
    'gp(time):seriesseries_1',
    names(coef(mod$mgcv_model)),
    fixed = TRUE
  )

  expect_true(identical(
    dim(brms_dat$Xgp_1_1),
    dim(mod$model_data$X[row_s1, col_s1])
  ))

  expect_true(all(
    unlist(
      lapply(seq_len(NCOL(brms_dat$Xgp_1_1)), function(x) {
        cor(brms_dat$Xgp_1_1[, x], mod$model_data$X[row_s1, col_s1][, x])
      }),
      use.names = FALSE
    ) >
      0.99
  ))

  # The mgcv model formula should contain s() in place of gp()
  expect_equal(
    attr(terms(mod$mgcv_model$formula, keep.order = TRUE), 'term.labels'),
    attr(
      terms(
        formula(
          y ~
            s(time, by = series, k = 11) +
              year:season +
              s(series, bs = "re")
        ),
        keep.order = TRUE
      ),
      'term.labels'
    )
  )
})

test_that("multidimensional gp for observation models working properly", {
  gaus_data$data_train$y[is.na(gaus_data$data_train$y)] <- 0
  mod <- mvgam(
    y ~
      s(series, bs = 're') +
        gp(time, year, k = 4, cov = 'matern32'),
    data = gaus_data$data_train,
    family = gaussian(),
    run_model = FALSE,
    autoformat = FALSE
  )

  expect_true(
    any(grepl(
      'b[b_idx_gp_timeby_year_] = sqrt(spd_gp_matern32(l_gp_timeby_year_',
      mod$model_file,
      fixed = TRUE
    ))
  )

  # Gp data structures should be in the model_data
  expect_true("l_gp_timeby_year_" %in% names(mod$model_data))
  expect_true("b_idx_gp_timeby_year_" %in% names(mod$model_data))
  expect_true("k_gp_timeby_year_" %in% names(mod$model_data))

  # These should match to the eigenvalues and eigenfunctions created by
  # a similar brms call
  brms_dat <- suppressWarnings(brms::make_standata(
    y ~
      s(series, bs = 're') +
        gp(time, year, k = 4, gr = FALSE),
    data = gaus_data$data_train,
    family = gaussian()
  ))

  # Eigenvalues should be identical
  expect_true(all.equal(brms_dat$slambda_1, mod$model_data$l_gp_timeby_year_))

  # Eigenfunctions will be nearly identical
  col_s1 <- grep('gp(time,year)', names(coef(mod$mgcv_model)), fixed = TRUE)

  expect_true(identical(dim(brms_dat$Xgp_1), dim(mod$model_data$X[, col_s1])))

  expect_true(all(
    unlist(
      lapply(seq_len(NCOL(brms_dat$Xgp_1)), function(x) {
        cor(brms_dat$Xgp_1[, x], mod$model_data$X[, col_s1][, x])
      }),
      use.names = FALSE
    ) >
      0.99
  ))

  # The mgcv model formula should contain s() in place of gp()
  expect_equal(
    attr(terms(mod$mgcv_model$formula, keep.order = TRUE), 'term.labels'),
    attr(
      terms(
        formula(
          y ~
            ti(time, year, k = 4, mc = c(0, 0)) +
              s(series, bs = "re")
        ),
        keep.order = TRUE
      ),
      'term.labels'
    )
  )
})

test_that("noncentring with gp terms working properly", {
  mod <- mvgam(
    y ~
      s(series, bs = 're') +
        s(season, bs = 'cc', k = 8) +
        gp(time, by = series, k = 10),
    trend_model = RW(),
    data = gaus_data$data_train,
    newdata = gaus_data$data_test,
    family = gaussian(),
    noncentred = TRUE,
    run_model = FALSE
  )

  # Model file should have the non-centred trend parameterisation now
  expect_true(
    any(grepl(
      trimws("trend = trend_raw .* rep_matrix(sigma', rows(trend_raw));"),
      trimws(mod$model_file),
      fixed = TRUE
    ))
  )

  expect_true(
    any(grepl(
      trimws("trend[2 : n, s] += trend[1 : (n - 1), s];"),
      trimws(mod$model_file),
      fixed = TRUE
    ))
  )

  # Gp data structures should be in the model_data
  expect_true("l_gp_time_byseriesseries_1" %in% names(mod$model_data))
  expect_true("b_idx_gp_time_byseriesseries_1" %in% names(mod$model_data))
  expect_true("k_gp_time_byseriesseries_1" %in% names(mod$model_data))
})

test_that("unidimensional gp for process models working properly", {
  mod <- mvgam(
    y ~ s(series, bs = 're'),
    trend_formula = ~ gp(time, by = trend, k = 10) +
      year:season,
    data = beta_data$data_train,
    family = betar(),
    trend_model = AR(),
    run_model = FALSE,
    autoformat = FALSE
  )

  # Model file should have prior lines for gp terms
  expect_true(any(grepl(
    '// prior for gp(time):trendtrend1_trend...',
    mod$model_file,
    fixed = TRUE
  )))

  expect_true(any(grepl(
    "b_trend[b_trend_idx_gp_time_bytrendtrend1] = sqrt(spd_gp_exp_quad(",
    mod$model_file,
    fixed = TRUE
  )))

  # Gp data structures should be in the model_data
  expect_true("l_gp_trend_time_bytrendtrend1" %in% names(mod$model_data))
  expect_true("b_trend_idx_gp_time_bytrendtrend1" %in% names(mod$model_data))
  expect_true("k_gp_trend_time_bytrendtrend1" %in% names(mod$model_data))

  # These should match to the eigenvalues and eigenfunctions created by
  # a similar brms call
  brms_dat <- suppressWarnings(brms::make_standata(
    y ~ gp(time, by = series, k = 10, c = 5 / 4),
    data = beta_data$data_train,
    family = gaussian()
  ))
  # Eigenvalues should be identical
  expect_true(all.equal(
    as.vector(brms_dat$slambda_1_1),
    as.vector(mod$model_data$l_gp_trend_time_bytrendtrend1)
  ))

  # Eigenfunctions will be nearly identical
  row_s1 <- mod$model_data$ytimes_trend[, 1]
  col_s1 <- grep(
    'gp(time):trendtrend1',
    names(coef(mod$trend_mgcv_model)),
    fixed = TRUE
  )

  expect_true(identical(
    dim(brms_dat$Xgp_1_1),
    dim(mod$model_data$X_trend[row_s1, col_s1])
  ))

  expect_true(
    max(abs(
      brms_dat$Xgp_1_1 -
        mod$model_data$X_trend[row_s1, col_s1]
    )) <
      0.01
  )

  # The mgcv model formula should contain s() in place of gp()
  expect_equal(
    attr(terms(mod$trend_mgcv_model$formula, keep.order = TRUE), 'term.labels'),
    attr(
      terms(
        formula(
          y ~
            s(time, by = series, k = 11) +
              year:season
        ),
        keep.order = TRUE
      ),
      'term.labels'
    )
  )
})

test_that("multidimensional gp for process models working properly", {
  mod <- mvgam(
    y ~ s(series, bs = 're'),
    trend_formula = ~ gp(time, season, k = 10, iso = FALSE),
    data = beta_data$data_train,
    family = betar(),
    trend_model = AR(),
    run_model = FALSE,
    autoformat = FALSE
  )

  # Model file should have prior lines for gp terms
  expect_true(any(grepl(
    '// prior for gp(time,season)_trend...',
    mod$model_file,
    fixed = TRUE
  )))

  expect_true(any(grepl(
    "b_trend[b_trend_idx_gp_timeby_season_] = sqrt(spd_gp_exp_quad(",
    mod$model_file,
    fixed = TRUE
  )))

  expect_true(any(grepl(
    "array[1] vector<lower=0>[2] rho_gp_trend_timeby_season_;",
    mod$model_file,
    fixed = TRUE
  )))

  expect_true(any(grepl(
    "rho_gp_trend_timeby_season_[1][1] ~ inv_gamma",
    mod$model_file,
    fixed = TRUE
  )))

  expect_true(any(grepl(
    "rho_gp_trend_timeby_season_[1][2] ~ inv_gamma",
    mod$model_file,
    fixed = TRUE
  )))

  # Gp data structures should be in the model_data
  expect_true("l_gp_trend_timeby_season_" %in% names(mod$model_data))
  expect_true("b_trend_idx_gp_timeby_season_" %in% names(mod$model_data))
  expect_true("k_gp_trend_timeby_season_" %in% names(mod$model_data))

  # These should match to the eigenvalues and eigenfunctions created by
  # a similar brms call
  brms_dat <- suppressWarnings(brms::make_standata(
    y ~ gp(time, season, k = 10, c = 5 / 4, cmc = TRUE, gr = FALSE),
    data = beta_data$data_train,
    family = gaussian()
  ))
  # Eigenvalues should be identical
  expect_true(all.equal(
    brms_dat$slambda_1,
    mod$model_data$l_gp_trend_timeby_season_
  ))

  # Eigenfunctions will be nearly identical
  col_s1 <- grep(
    'gp(time,season)',
    names(coef(mod$trend_mgcv_model)),
    fixed = TRUE
  )

  expect_true(identical(
    dim(brms_dat$Xgp_1),
    dim(mod$model_data$X_trend[, col_s1])
  ))

  expect_true(all(
    unlist(
      lapply(seq_len(NCOL(brms_dat$Xgp_1)), function(x) {
        cor(brms_dat$Xgp_1[, x], mod$model_data$X_trend[, col_s1][, x])
      }),
      use.names = FALSE
    ) >
      0.99
  ))

  # The mgcv model formula should contain s() in place of gp()
  expect_equal(
    attr(terms(mod$trend_mgcv_model$formula, keep.order = TRUE), 'term.labels'),
    attr(
      terms(
        formula(y ~ ti(time, season, k = 10, mc = c(0, 0))),
        keep.order = TRUE
      ),
      'term.labels'
    )
  )
})
