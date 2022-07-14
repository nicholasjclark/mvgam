library(mvgam)
dat <- sim_mvgam(T = 100, n_series=4, n_lv = 1)
mod1 <- mvgam(formula = y ~ s(season) + s(series, bs = 're'),
              data_train = dat$data_train,
              trend_model = 'GP',
              family = 'nb',
              use_stan = TRUE,
              run_model = FALSE)
mod1$model_file

mod2 <- mvgam(formula = y~year,
              data_train = dat$data_train,
              trend_model = 'RW',
              family = 'poisson',
              run_model = TRUE,
              use_stan = TRUE)
mod2$model_file

plot(mod2, 'smooths', residuals = TRUE, derivatives = TRUE)
compare_mvgams(model1 = mod1, model2 = mod2, fc_horizon = 6,
               n_evaluations = 30, n_cores = 3)
eval_mvgam(object = mod2, n_cores = 1)
plot(mod1, type = 'forecast', realisations = TRUE)
plot(mod1, type = 'trend', realisations = TRUE)

plot_mvgam_smooth(mod1, 1, 'season', realisations = TRUE, n_realisations = 10)
plot_mvgam_fc(object = mod1, series = 1,
              realisations = TRUE, n_realisations = 15)
fake <- dat$data_test
fake$y <- NULL
plot_mvgam_fc(object = mod1, series = 1, data_test = fake)
obj <- forecast(mod1, data_test = fake)
dim(obj)


plot_mvgam_trend(object = mod1, series = 1, data_test = fake,
                 realisations = TRUE)






pfilter_mvgam_init(object = mod1, n_particles = 2000,
                   n_cores = 3, data_assim = model_dat[28,])


# models with no smooths
# predictions with new data by extending the temporal process forward
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

