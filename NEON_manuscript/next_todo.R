library(mvgam)
dat <- sim_mvgam(T = 100, n_series=4, n_lv = 1)
dat$true_corrs


mod1 <- mvgam(formula = y ~ s(season, bs = 'cc') +
                s(series, bs = 're'),
              data_train = dat$data_train,
              trend_model = 'AR3',
              family = 'poisson',
              use_lv = TRUE,
              n_lv = 2,
              use_stan = TRUE,
              run_model = T,
              burnin = 10)
summary(mod1)

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

