library(mvgam)
dat <- sim_mvgam(T = 100, n_series=3, prop_missing = .4)
plot_mvgam_series(data_train = dat$data_train, series = 'all')


mod1 <- mvgam(formula = y ~ 1,
              data_train = dat$data_train,
              trend_model = 'GP',
              family = 'poisson',
              use_stan = TRUE,
              run_model = T,
              burnin = 10)
pairs(mod1$model_output, pars = c('rho_gp', 'r'))
stan_trace(mod1$model_output, pars = c('rho_gp', 'r'))
mod1$model_file
mod1$model_data

r_inv <- abs(rnorm(1000, 4, 2.5))
summary(r_inv)
hist(r_inv)

summary(mod1)
plot_mvgam_factors(mod1)
plot(mod1, type = 'series')
fake <- dat$data_test
fake$y <- NULL
plot(mod1, 'trend', data_test = fake)




dat <- sim_mvgam(T = 100, n_series = 4, n_lv = 2,
                 mu_obs = c(4, 6, 10, 14), trend_rel = 0.3,
                 seasonality = 'hierarchical')
plot_mvgam_series(data_train = dat$data_train,
                  n_bins = 20,
                  series = 'all',
                  log_scale = TRUE)

# Good for testing model files without compiling
stanc(model_code = mod1$model_file)$model_name
model_file <- mod1$model_file

mod2 <- mvgam(formula = y ~ s(season, bs = 'cc') +
                s(series, bs = 're'),
              data_train = dat$data_train,
              trend_model = 'RW',
              family = 'poisson',
              use_lv = TRUE,
              n_lv = 2,
              run_model = TRUE,
              burnin = 10)


plot(mod1, series = 3, 'forecast', data_test = dat$data_test)
plot(mod2, series = 3, 'forecast', data_test = dat$data_test)

plot(mod1, series = 4, 'trend', data_test = dat$data_test)
plot(mod2, series = 4, 'trend', data_test = dat$data_test)


# respect upper bounds for forecasts, prediction, particle filtering
trunc_poiss = function(lambda, bound){
  out <- vector(length = length(lambda))
  for(i in 1:length(out)){
    func <- ecdf(rpois(10000, lambda = lambda[i]))
    unif_bound <- func(bound)
    out[i] <- qpois(runif(1, 0, unif_bound), lambda = lambda[i])
  }
 out
}
trunc_poiss(c(10,8, 21, 11, 5), 20)

