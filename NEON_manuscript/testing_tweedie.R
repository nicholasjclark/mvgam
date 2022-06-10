# Generate series with independent latent GP trends
library(mvgam)
sim_data <- sim_mvgam(T = 70, n_series = 2,
                      use_lv = F,
                      trend_model = 'GP',
                      trend_rel = 0.6,
                      family = 'poisson',
                      mu_obs = c(7, 10))
mod1stan <- mvgam(data_train = sim_data$data_train,
                    data_test = sim_data$data_test,
                    formula = y ~ s(season, k = 12, bs = 'cc'),
                    knots = list(season = c(0.5, 12.5)),
                    family = 'poisson',
                    trend_model = 'GP',
                    chains = 4,
                    burnin = 1000,
                    use_stan = T, run_model = T)
mod1stan$model_file
summary(mod1stan)
plot(mod1stan, 'forecast', series = 2)
plot(mod1stan, 'smooths', series = 2)
plot(mod1stan, 'trend')

pfilter_mvgam_init(object = mod1stan, data_assim = sim_data$data_test %>%
                     dplyr::filter(time == 54),
                   n_particles = 10000)

pfilter_mvgam_online(data_assim = sim_data$data_test[1:10, ], n_cores = 3, kernel_lambda = 1)

fcs <- pfilter_mvgam_fc(data_test = sim_data$data_test, ylim = c(0, 17))

layout(matrix(c(1:2), ncol = 2, nrow = 1))
plot_mvgam_fc(mod1stan, series = 1, data_test = sim_data$data_test,
     ylim = c(0, 17))
fcs$series_1()
points(c(sim_data$data_train %>%
           dplyr::filter(series == 'series_1')%>%
           dplyr::pull(y),
         sim_data$data_test %>%
           dplyr::filter(series == 'series_1') %>%
           dplyr::pull(y)))

