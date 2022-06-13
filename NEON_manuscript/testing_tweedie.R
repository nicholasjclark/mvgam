# Generate series with independent latent GP trends
library(mvgam)
sim_data <- sim_mvgam(T = 90, n_series = 2,
                      use_lv = F,
                      trend_model = 'GP',
                      trend_rel = 0.6,
                      family = 'poisson',
                      mu_obs = c(7, 10),
                      train_prop = 1)

# To run outside of mvgam (maybe show how other elements could be added?)
train_dat <- sim_data$data_train %>%
  dplyr::filter(time <= 75)
test_dat <- sim_data$data_train %>%
  dplyr::filter(time > 75)
# mod1stan <- mvgam(data_train = train_dat,
#                   data_test = test_dat,
#                   formula = y ~ s(season, k = 12, bs = 'cc'),
#                   knots = list(season = c(0.5, 12.5)),
#                   family = 'nb',
#                   trend_model = 'GP',
#                   use_stan = T,
#                   run_model = F)
# test <- rstan::stan(model_code = mod1stan$model_file,
#                     data = mod1stan$model_data,
#                     chains = 1, iter = 1000,
#                     init = mod1stan$inits)

mod1jags <- mvgam(data_train = train_dat,
                  data_test = test_dat,
                  formula = y ~ s(season, k = 12, bs = 'cc'),
                  knots = list(season = c(0.5, 12.5)),
                  family = 'poisson',
                  trend_model = 'AR3')

mod1stan <- mvgam(data_train = train_dat,
                  data_test = test_dat,
                  formula = y ~ s(season, k = 12, bs = 'cc'),
                  knots = list(season = c(0.5, 12.5)),
                  family = 'poisson',
                  trend_model = 'GP',
                  use_stan = T,
                  burnin = 1000)

# Compare the out of sample DRPS for each model
plot(mod1jags, 'forecast', series = 1, data_test = test_dat)
plot(mod1stan, 'forecast', series = 1, data_test = test_dat)

plot(mod1jags, 'forecast', series = 2, data_test = test_dat)
plot(mod1stan, 'forecast', series = 2, data_test = test_dat)

compare_mvgams(mod1jags, mod1stan, fc_horizon = 10,
               n_evaluations = 20, n_cores = 4)

plot(mod1stan, 'residuals', series = 1)
plot(mod1stan, 'residuals', series = 2)
plot(mod1stan, 'uncertainty', data_test = test_dat, series = 1)
plot(mod1stan, 'uncertainty', data_test = test_dat, series = 2)
plot(mod1stan, 'forecast', series = 1, data_test = test_dat)
plot(mod1stan, 'forecast', series = 2, data_test = test_dat)
plot(mod1stan, 'smooths', series = 1)
plot(mod1stan, 'smooths', series = 2)
plot(mod1stan, 'trend', series = 2)
plot(sim_data$true_trends[,2])

# Need to fix the particle filter so it can initiate even if only one series has a next observation
pfilter_mvgam_init(object = mod1stan, data_assim = test_dat,
                   n_particles = 1000)

pfilter_mvgam_online(data_assim = test_dat[1:14, ],
                     n_cores = 3, use_resampling = T,
                     threshold = 0.6)

fcs <- pfilter_mvgam_fc(data_test = test_dat, ylim = c(0, 20), n_cores = 3)

layout(matrix(c(1:2), ncol = 1, nrow = 2))
plot_mvgam_fc(mod1stan, series = 1, data_test = test_dat,
     ylim = c(0, 20))
fcs$series_1()
points(c(sim_data$data_train %>%
           dplyr::filter(series == 'series_1')%>%
           dplyr::pull(y),
         sim_data$data_test %>%
           dplyr::filter(series == 'series_1') %>%
           dplyr::pull(y)),
       pch = 16, cex = 0.4)


layout(matrix(c(1:2), ncol = 1, nrow = 2))
plot_mvgam_fc(mod1stan, series = 2, data_test = test_dat,
              ylim = c(0, 20))
fcs$series_2()
points(c(sim_data$data_train %>%
           dplyr::filter(series == 'series_2')%>%
           dplyr::pull(y),
         sim_data$data_test %>%
           dplyr::filter(series == 'series_2') %>%
           dplyr::pull(y)),
       pch = 16, cex = 0.4)
unlink('pfilter', recursive = T)
