library(mvgam)
dat <- sim_mvgam(T = 50, n_series=2)
mod1 <- mvgam(y~s(season)-1,
              data_train = dat$data_train,
              trend_model = 'RW',
              family = 'pois',
              run_model = TRUE)

# Use Michael Betancourt's utility functions for checking diagnostics
#source('https://raw.githubusercontent.com/betanalpha/knitr_case_studies/master/factor_modeling/stan_utility.R')
check_all_diagnostics(fit1)

plot(mod1, type = 'forecast', realisations = TRUE)
plot(mod1, type = 'trend', realisations = TRUE,
     derivatives = TRUE)
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

