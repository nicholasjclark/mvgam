context("n_mixture")

# Simulations take a bit of time to set up
skip_on_cran()

set.seed(100)
poisdat <- sim_mvgam()

test_that("only count data allowed for nmixtures", {
  gaus_data$data_train$cap <- 100
  expect_error(mvgam(y ~ s(season),
                     trend_formula = ~ trend,
                     family = nmix(),
                     data = gaus_data$data_train),
               'Values < 0 not allowed for count family responses',
               fixed = TRUE)
})

test_that("cap must be supplied in data", {
  expect_error(get_mvgam_priors(formula = y ~ s(season),
                                trend_formula = ~ s(season) +
                                  trend,
                                trend_model = 'None',
                                family = nmix(),
                                data = poisdat$data_train),
               'Max abundances must be supplied as a variable named "cap" for N-mixture models',
               fixed = TRUE)

  poisdat$data_train$cap <- rpois(NROW(poisdat$data_train),
                                  lambda = 5) +
    max(poisdat$data_train$y, na.rm = TRUE)
  expect_error(mvgam(formula = y ~ s(season),
                     trend_formula = ~ s(season) +
                       trend,
                     trend_model = 'None',
                     family = nmix(),
                     data = poisdat$data_train,
                     newdata = poisdat$data_test),
               '"data" and "newdata" have different numbers of columns',
               fixed = TRUE)

  poisdat$data_test$emu <- 50
  expect_error(mvgam(formula = y ~ s(season),
                     trend_formula = ~ s(season) +
                       trend,
                     trend_model = 'None',
                     family = nmix(),
                     data = poisdat$data_train,
                     newdata = poisdat$data_test),
               'Max abundances must be supplied in test data as a variable named "cap" for N-mixture models',
               fixed = TRUE)
})

poisdat$data_train$cap <- rpois(NROW(poisdat$data_train),
                               lambda = 5) +
  max(poisdat$data_train$y, na.rm = TRUE)
poisdat$data_test$cap <- rpois(NROW(poisdat$data_test),
                               lambda = 5) +
  max(poisdat$data_test$y, na.rm = TRUE)

test_that("latent process intercept is allowed in nmixtures", {
  prior_df <- get_mvgam_priors(formula = y ~ s(season),
                   trend_formula = ~ s(season) +
                     trend,
                   trend_model = 'None',
                   family = nmix(),
                   data = poisdat$data_train)
  expect_true(any(grepl('(Intercept)_trend',
                        prior_df$param_name, fixed = TRUE)))

  mod <- mvgam(formula = y ~ s(season),
               trend_formula = ~ s(season) +
                 trend,
               trend_model = 'None',
               family = nmix(),
               data = poisdat$data_train,
               newdata = poisdat$data_test,
               priors = prior(std_normal(),
                              class = '(Intercept)_trend'),
               run_model = FALSE)
  expect_true(any(grepl('(Intercept)_trend',
                        mod$model_file, fixed = TRUE)))
  expect_true(any(grepl('b_raw_trend[1] ~ std_normal();',
                        mod$model_file, fixed = TRUE)))

  # Can also test that 'cap' is properly included in model_data
  # The caps should be arranged by series and then by time
  train_cap = poisdat$data_train %>%
    dplyr::arrange(series, time) %>%
    dplyr::pull(cap)
  test_cap = poisdat$data_test %>%
    dplyr::arrange(series, time) %>%
    dplyr::pull(cap)
  expect_true(all(mod$model_data$cap ==
                    c(train_cap, test_cap)))
})

# Check that the model fits and post-processing works using the
# example from the families man page
test_that("nmix() post-processing works", {
  set.seed(0)
  data.frame(site = 1,
             # five replicates per year; six years
             replicate = rep(1:5, 6),
             time = sort(rep(1:6, 5)),
             species = 'sp_1',
             # true abundance declines nonlinearly
             truth = c(rep(28, 5),
                       rep(26, 5),
                       rep(23, 5),
                       rep(16, 5),
                       rep(14, 5),
                       rep(14, 5)),
             # observations are taken with detection prob = 0.7
             obs = c(rbinom(5, 28, 0.7),
                     rbinom(5, 26, 0.7),
                     rbinom(5, 23, 0.7),
                     rbinom(5, 15, 0.7),
                     rbinom(5, 14, 0.7),
                     rbinom(5, 14, 0.7))) %>%
    # add 'series' information, which is an identifier of site, replicate and species
    dplyr::mutate(series = paste0('site_', site,
                                  '_', species,
                                  '_rep_', replicate),
                  time = as.numeric(time),
                  # add a 'cap' variable that defines the maximum latent N to
                  # marginalize over when estimating latent abundance; in other words
                  # how large do we realistically think the true abundance could be?
                  cap = 100) %>%
    dplyr::select(- replicate) -> testdat

  # Now add another species that has a different temporal trend and a smaller
  # detection probability (0.45 for this species)
  testdat = testdat %>%
    dplyr::bind_rows(data.frame(site = 1,
                                replicate = rep(1:5, 6),
                                time = sort(rep(1:6, 5)),
                                species = 'sp_2',
                                truth = c(rep(4, 5),
                                          rep(7, 5),
                                          rep(15, 5),
                                          rep(16, 5),
                                          rep(19, 5),
                                          rep(18, 5)),
                                obs = c(rbinom(5, 4, 0.45),
                                        rbinom(5, 7, 0.45),
                                        rbinom(5, 15, 0.45),
                                        rbinom(5, 16, 0.45),
                                        rbinom(5, 19, 0.45),
                                        rbinom(5, 18, 0.45))) %>%
                       dplyr::mutate(series = paste0('site_', site,
                                                     '_', species,
                                                     '_rep_', replicate),
                                     time = as.numeric(time),
                                     cap = 50) %>%
                       dplyr::select(-replicate))

  # series identifiers
  testdat$species <- factor(testdat$species,
                            levels = unique(testdat$species))
  testdat$series <- factor(testdat$series,
                           levels = unique(testdat$series))

  # The trend_map to state how replicates are structured
  testdat %>%
    # each unique combination of site*species is a separate process
    dplyr::mutate(trend = as.numeric(factor(paste0(site, species)))) %>%
    dplyr::select(trend, series) %>%
    dplyr::distinct() -> trend_map

  # Fit a model
  mod <- SW(mvgam(
    # the observation formula sets up linear predictors for
    # detection probability on the logit scale
    formula = obs ~ species - 1,

    # the trend_formula sets up the linear predictors for
    # the latent abundance processes on the log scale
    trend_formula = ~ s(time, by = trend, k = 4) + species,

    # the trend_map takes care of the mapping
    trend_map = trend_map,

    # nmix() family and data
    family = nmix(),
    data = testdat,

    # priors can be set in the usual way
    priors = c(prior(std_normal(), class = b),
               prior(normal(1, 1.5), class = Intercept_trend)),
    samples = 300,
    residuals = FALSE,
    chains = 2,
    silent = 2))

  expect_no_error(capture_output(summary(mod)))
  expect_no_error(capture_output(plot(mod, type = 'pterms')))
  expect_no_error(capture_output(plot(mod, type = 'pterms', trend_effects = TRUE)))
  expect_no_error(capture_output(print(mod)))
  expect_true(inherits(hindcast(mod), 'mvgam_forecast'))
  expect_true(inherits(hindcast(mod,
                                type = 'latent_N'),
                       'mvgam_forecast'))
  expect_true(inherits(hindcast(mod,
                                type = 'detection'),
                       'mvgam_forecast'))

  preds <- predict(mod, summary = FALSE, type = 'response')
  expect_true(NCOL(preds) == NROW(testdat))
  expect_true(all(preds >= 0L))

  preds <- predict(mod, summary = FALSE, type = 'detection')
  expect_true(NCOL(preds) == NROW(testdat))
  expect_true(all(preds <= 1L & preds >= 0L))

  preds <- predict(mod, summary = FALSE, type = 'latent_N')
  expect_true(NCOL(preds) == NROW(testdat))
  expect_true(all(preds >= 0L))

  expect_no_error(plot(mod, type = 'smooths',
                       trend_effects = TRUE))
  expect_no_error(plot(mod, type = 'smooths',
                       realisations = TRUE,
                       trend_effects = TRUE))
  expect_no_error(plot(mod, type = 'smooths',
                       residuals = TRUE,
                       trend_effects = TRUE))
  options(mc.cores = 1)
  expect_loo(SW(loo(mod)))
})
