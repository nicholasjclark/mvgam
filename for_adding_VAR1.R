library(mvgam)
data <- sim_mvgam(n_series = 4, trend_rel = 0.6,
                  trend_model = 'VAR1')
names(data)
data$trend_params
plot_mvgam_series(data = data$data_train, series = 'all')
mod_mvgam <- mvgam(y ~ s(season),
                   trend_model = 'VAR1',
                   data = data$data_train,
                   family = poisson(),
                   run_model = TRUE)
plot(mod_mvgam, type = 'forecast', newdata = data$data_test)
summary(mod_mvgam)
code(mod_mvgam)
plot(mod_mvgam, type = 'smooths')

mod_2 <- mvgam(y ~ s(season),
                   trend_model = 'RW',
                   data = data$data_train,
                   family = poisson(),
                   run_model = TRUE)
compare_mvgams(model1 = mod_mvgam, model2 = mod_2)

mod_2$trend_model

roll_eval_mvgam(mod_2, n_evaluations = 3, n_cores = 1)
eval_mvgam(object = mod_mvgam, n_cores = 2)
