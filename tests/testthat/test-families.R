context("Tests for family functions")

test_that("distributions work correctly", {
  fam <- tweedie()
  expect_true(inherits(fam, 'family'))
  expect_true(inherits(fam, 'extended.family'))
  expect_true(fam$link == 'log')

  fam <- student_t()
  expect_true(inherits(fam, 'family'))
  expect_true(inherits(fam, 'extended.family'))
  expect_true(fam$link == 'identity')

  fam <- nmix()
  expect_true(inherits(fam, 'family'))
  expect_true(inherits(fam, 'extended.family'))
  expect_true(fam$link == 'log')
})

test_that("nmix predictions work correctly", {
  set.seed(1)
  Xp <- matrix(rnorm(100), ncol = 10, nrow = 10)
  attr(Xp, 'model.offset') <- 0
  family <- 'nmix'
  betas <- rnorm(10)
  latent_lambdas <- runif(10, 2, 5)
  cap <- rep(12, 10)
  family_pars <- list()
  expect_true(all(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = latent_lambdas,
                                        cap = cap,
                                        type = 'link',
                                        family_pars = family_pars) > 0))

  expect_true(all(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = latent_lambdas,
                                        cap = cap,
                                        type = 'expected',
                                        family_pars = family_pars) > 0))

  detects <- mvgam:::mvgam_predict(Xp = Xp,
                                   family = family,
                                   betas = betas,
                                   latent_lambdas = latent_lambdas,
                                   cap = cap,
                                   type = 'detection',
                                   family_pars = family_pars)
  expect_true(all(detects > 0) & all(detects < 1))

  truth <- rpois(10, 6)
  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = latent_lambdas,
                                        cap = cap,
                                        type = 'link',
                                        density = TRUE,
                                        truth = truth,
                                        family_pars = family_pars))

  # Test that the residual calculation is working
  preds <- runif(10, 0.1, 10)
  N <- rpois(10, 20)
  p <- runif(10, 0.3, 0.6)
  resids <- mvgam:::ds_resids_nmix(truth = truth,
                                   fitted = 1,
                                   draw = 1,
                                   N = as.vector(N),
                                   p = as.vector(p))
  expect_true(all(!is.na(resids)))

})

test_that("beta predictions work correctly", {
  set.seed(1)
  Xp <- matrix(rnorm(100), ncol = 10, nrow = 10)
  attr(Xp, 'model.offset') <- 0
  family <- 'beta'
  betas <- rnorm(10)
  family_pars <- list(phi = rep(1, 10))
  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'link',
                                        family_pars = family_pars))

  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'variance',
                                        family_pars = family_pars))
  expecteds <- mvgam:::mvgam_predict(Xp = Xp,
                                     family = family,
                                     betas = betas,
                                     latent_lambdas = NULL,
                                     cap = NULL,
                                     type = 'expected',
                                     family_pars = family_pars)

  expect_true(all(expecteds >= 0) & all(expecteds <= 1))

  preds <- mvgam:::mvgam_predict(Xp = Xp,
                                 family = family,
                                 betas = betas,
                                 latent_lambdas = NULL,
                                 cap = NULL,
                                 type = 'response',
                                 family_pars = family_pars)
  expect_true(all(preds >= 0) & all(preds <= 1))

  truth <- runif(10, 0.01, 0.99)
  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'link',
                                        density = TRUE,
                                        truth = truth,
                                        family_pars = family_pars))

  preds <- runif(10, 0.01, 0.99)
  resids <- mvgam:::ds_resids_beta(truth = truth,
                                   fitted = as.vector(preds),
                                   draw = 1,
                                   precision = rep(1, 10))
  expect_true(all(!is.na(resids)))

})

test_that("beta-binomial predictions work correctly", {
  set.seed(1)
  Xp <- matrix(rnorm(100), ncol = 10, nrow = 10)
  attr(Xp, 'model.offset') <- 0
  family <- 'beta_binomial'
  betas <- rnorm(10)
  family_pars <- list(phi = rep(1, 10),
                      trials = rep(20, 10))
  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'link',
                                        family_pars = family_pars))

  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'variance',
                                        family_pars = family_pars))

  expect_true(all(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'expected',
                                        family_pars = family_pars) > 0))

  expect_true(all(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'response',
                                        family_pars = family_pars) >= 0))

  truth <- rpois(10, 8)
  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'link',
                                        density = TRUE,
                                        truth = truth,
                                        family_pars = family_pars))

  p <- runif(10, 0.3, 0.6)
  resids <- mvgam:::ds_resids_beta_binomial(truth = truth,
                                            fitted = p,
                                            draw = 1,
                                            N = rep(20, 10),
                                            phi = rep(1, 10))
  expect_true(all(!is.na(resids)))
})

test_that("negative binomial predictions work correctly", {
  set.seed(1)
  Xp <- matrix(rnorm(100), ncol = 10, nrow = 10)
  attr(Xp, 'model.offset') <- 0
  family <- 'negative binomial'
  betas <- rnorm(10)
  family_pars <- list(phi = rep(1, 10))
  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'link',
                                        family_pars = family_pars))

  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'variance',
                                        family_pars = family_pars))

  expect_true(all(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'expected',
                                        family_pars = family_pars) > 0))

  expect_true(all(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'response',
                                        family_pars = family_pars) >= 0))

  truth <- rpois(10, 8)
  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'link',
                                        density = TRUE,
                                        truth = truth,
                                        family_pars = family_pars))

  preds <- rpois(10, 8)
  resids <- mvgam:::ds_resids_nb(truth = truth,
                                 fitted = preds,
                                 draw = 1,
                                 size = rep(1, 10))
  expect_true(all(!is.na(resids)))
})

test_that("lognormal predictions work correctly", {
  set.seed(1)
  Xp <- matrix(rnorm(100), ncol = 10, nrow = 10)
  attr(Xp, 'model.offset') <- 0
  family <- 'lognormal'
  betas <- rnorm(10)
  family_pars <- list(sigma_obs = 1)
  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'link',
                                        family_pars = family_pars))

  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'variance',
                                        family_pars = family_pars))

  expect_true(all(mvgam:::mvgam_predict(Xp = Xp,
                                    family = family,
                                    betas = betas,
                                    latent_lambdas = NULL,
                                    cap = NULL,
                                    type = 'expected',
                                    family_pars = family_pars) > 0))

  expect_true(all(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'response',
                                        family_pars = family_pars) > 0))

  truth <- runif(10, 0.1, 20)
  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'link',
                                        density = TRUE,
                                        truth = truth,
                                        family_pars = family_pars))

  preds <- runif(10, 0.1, 20)
  resids <- mvgam:::ds_resids_lnorm(truth = truth,
                                   fitted = preds,
                                   draw = 1,
                                   sigma = rep(1, 10))
  expect_true(all(!is.na(resids)))
})

test_that("gamma predictions work correctly", {
  set.seed(1)
  Xp <- matrix(rnorm(100), ncol = 10, nrow = 10)
  attr(Xp, 'model.offset') <- 0
  family <- 'Gamma'
  betas <- rnorm(10)
  family_pars <- list(shape = 1)
  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'link',
                                        family_pars = family_pars))

  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'variance',
                                        family_pars = family_pars))

  expect_true(all(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'expected',
                                        family_pars = family_pars) > 0))

  expect_true(all(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'response',
                                        family_pars = family_pars) > 0))

  truth <- runif(10, 0.1, 20)
  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'link',
                                        density = TRUE,
                                        truth = truth,
                                        family_pars = family_pars))

  preds <- runif(10, 0.1, 20)
  resids <- mvgam:::ds_resids_gamma(truth = truth,
                                   fitted = preds,
                                   draw = 1,
                                   shape = rep(1, 10))
  expect_true(all(!is.na(resids)))
})

test_that("student-t predictions work correctly", {
  set.seed(1)
  Xp <- matrix(rnorm(100), ncol = 10, nrow = 10)
  attr(Xp, 'model.offset') <- 0
  family <- 'student'
  betas <- rnorm(10)
  family_pars <- list(sigma_obs = 1,
                      nu = 3)
  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'link',
                                        family_pars = family_pars))

  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'variance',
                                        family_pars = family_pars))

  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'expected',
                                        family_pars = family_pars))

  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'response',
                                        family_pars = family_pars))

  truth <- rnorm(10)
  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'link',
                                        density = TRUE,
                                        truth = truth,
                                        family_pars = family_pars))

  preds <- rnorm(10)
  resids <- mvgam:::ds_resids_student(truth = truth,
                                     fitted = preds,
                                     draw = 1,
                                     sigma = rep(1, 10),
                                     nu = rep(5, 10))
  expect_true(all(!is.na(resids)))
})

test_that("tweedie predictions work correctly", {
  set.seed(1)
  Xp <- matrix(rnorm(100), ncol = 10, nrow = 10)
  attr(Xp, 'model.offset') <- 0
  family <- 'tweedie'
  betas <- rnorm(10)
  family_pars <- list(phi = 1)
  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'link',
                                        family_pars = family_pars))

  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'variance',
                                        family_pars = family_pars))

  expect_no_error(all(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'expected',
                                        family_pars = family_pars) > 0))

  expect_no_error(all(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'response',
                                        family_pars = family_pars) >= 0))

  truth <- rpois(10, 5)
  expect_no_error(mvgam:::mvgam_predict(Xp = Xp,
                                        family = family,
                                        betas = betas,
                                        latent_lambdas = NULL,
                                        cap = NULL,
                                        type = 'link',
                                        density = TRUE,
                                        truth = truth,
                                        family_pars = family_pars))

  preds <- rpois(10, 5)
  resids <- mvgam:::ds_resids_tw(truth = truth,
                                fitted = preds,
                                draw = 1)
  expect_true(all(!is.na(resids)))
})

# Skip actual model setups on CRAN as they take some time
test_that("family setups work correctly", {
  skip_on_cran()
  simdat <- sim_mvgam(family = poisson())
  mod <- mvgam(y ~ -1,
               trend_model = PW(),
               data = simdat$data_train,
               family = poisson(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_no_error(capture_output(print(mod)))

  simdat <- sim_mvgam(family = nb(),
                      n_lv = 2)
  mod <- mvgam(y ~ -1,
               trend_model = PW(),
               data = simdat$data_train,
               family = nb(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_no_error(capture_output(print(mod)))

  simdat <- sim_mvgam(family = lognormal(),
                      trend_model = VAR())
  mod <- mvgam(y ~ -1,
               trend_model = PW(),
               data = simdat$data_train,
               family = lognormal(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_no_error(capture_output(print(mod)))

  simdat <- sim_mvgam(family = bernoulli(),
                      trend_model = VAR(cor = TRUE))
  mod <- mvgam(y ~ -1,
               trend_model = PW(),
               data = simdat$data_train,
               family = bernoulli(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_no_error(capture_output(print(mod)))

  simdat <- sim_mvgam(family = student_t(),
                      trend_model = GP())
  mod <- mvgam(y ~ -1,
               trend_model = PW(),
               data = simdat$data_train,
               family = student_t(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_no_error(capture_output(print(mod)))

  simdat <- sim_mvgam(family = Gamma(),
                      seasonality = 'shared',
                      trend_model = AR(p = 3))
  mod <- mvgam(y ~ -1,
               trend_model = PW(),
               data = simdat$data_train,
               family = Gamma(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_no_error(capture_output(print(mod)))

  simdat <- sim_mvgam(family = betar(),
                      seasonality = 'hierarchical',
                      trend_model = AR(p = 2))
  mod <- mvgam(y ~ -1,
               trend_model = PW(),
               data = simdat$data_train,
               family = betar(),
               run_model = FALSE)
  expect_true(inherits(mod, 'mvgam_prefit'))
  expect_no_error(capture_output(print(mod)))
})
