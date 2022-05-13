# Simulate a seasonal smooth and some autocorrelated, Tweedie distributed discrete random variates
season <- rep(seq(1, 24), 6)
mean_func = function(x){
  0.1 + x * 0.01 + sin(0.1 * x) - cos(-0.15 * x)
}
set.seed(15)
y = rpois(length(season),
          lambda = mgcv::rTweedie(mu = exp(-0.9 + mean_func(season) +
                                             cumsum(rnorm(length(season), sd = 0.075))),
                                  p = 1.5, phi = 1.25))
plot(y, type = 'l')

# Split data into training and testing
data_train <-
  data.frame(y = y[1:125],
             season = season[1:125],
             time = 1:125)
data_test <-
  data.frame(y = y[126:length(season)],
             season = season[126:length(season)],
             time = 126:length(season))

# Fit non-dynamic GAMs
# Poisson model
library(mvgam)
mod1 <- mvjagam(data_train = data_train,
                data_test = data_test,
                formula = y ~ s(season, k = 15, bs = 'cc') +
                  s(time, k = 4, bs = 'gp'),
                knots = list(season = c(0.5, 24.5)),
                family = 'nb',
                trend_model = 'AR1',
                chains = 4,
                burnin = 1000)
summary(mod1)


mod1stan <- mvjagam(data_train = data_train,
                data_test = data_test,
                formula = y ~ s(season, k = 15, bs = 'cc') +
                  s(time, k = 4, bs = 'gp'),
                knots = list(season = c(0.5, 24.5)),
                family = 'nb',
                trend_model = 'AR1',
                chains = 4,
                burnin = 1000,
                use_stan = T)
mod1stan$model_file
summary(mod1stan)


compare_mvgams(mod1, mod1stan, fc_horizon = 4,
               n_evaluations = 20, n_cores = 5)

# Check if overdispersion correctly captured
ppc(mod1stan, type = 'hist', n_bins = 100)
ppc(mod1stan, type = 'rootogram', data_test = data_test, n_bins = 25)
ppc(mod1stan, type = 'pit', data_test = data_test)
plot(mod1stan, type = 'residuals')

# Not captured; try Negative binomial model
mod2 <- mvjagam(data_train = data_train,
                data_test = data_test,
                formula = y ~ s(season, k = 15, bs = 'cc') +
                  s(time, k = 12, bs = 'gp'),
                knots = list(season = c(0.5, 24.5)),
                family = 'nb',
                trend_model = 'None',
                chains = 4,
                burnin = 2000)
plot_mvgam_trace(mod2)
ppc(mod2, type = 'rootogram', data_test = data_test, n_bins = 25)
ppc(mod2, type = 'pit', data_test = data_test)
plot(mod2, type = 'residuals')
summary(mod2)

# Dispersion parameter clearly playing a role; but
# Q-Q plot still shows some problems remain; try Tweedie model
mod3 <- mvjagam(data_train = data_train,
                data_test = data_test,
                formula = y ~ s(season, k = 15, bs = 'cc') +
                  s(time, k = 12, bs = 'gp'),
                knots = list(season = c(0.5, 24.5)),
                family = 'tw',
                trend_model = 'None',
                chains = 4,
                burnin = 2000)
mod3$model_file
plot_mvgam_trace(mod3)
ppc(mod3, type = 'rootogram', data_test = data_test, n_bins = 25)
ppc(mod3, type = 'pit', data_test = data_test)
plot(mod3, type = 'residuals')

# Tweedie shows best fit in terms of capturing dispersion, with no autocorrelation left
# in the residuals
summary(mod3)
plot(mod3, type = 'forecast', data_test = data_test)
plot(mod3, type = 'smooth', residuals = T)

# In-sample DICs confirm model 3 is most parsimonious
dic(mod1)
dic(mod2)
dic(mod3)


# Check if AR process improves forecasts; better to estimate AR3 even if it is not as likely;
# this is because we don't want the trend to compete with the overdispersion parameter too much;
# on this end, we also use a tighter prior on the latent trend variance as we don't have any prior
# belief that this trend process should jump around much from timepoint to timepoint (we would really)
# expect a temporal trend to be somewhat smooth, while the dispersion parameter can capture any
# observation process)
mod3b <- mvjagam(data_train = data_train,
                data_test = data_test,
                formula = y ~ s(season, k = 15, bs = 'cc') +
                  s(time, k = 12, bs = 'gp'),
                knots = list(season = c(0.5, 24.5)),
                family = 'tw',
                trend_model = 'AR3',
                # containment prior that doesn't allow the log-variance to be minute (which is unidentifiable)
                # or very large (which goes against our prior belief about trend evolution)
                sigma_prior = 'dunif(0.01, 0.5)',
                chains = 4,
                burnin = 5000)


# Does DIC reflect this also?
dic(mod3b)

# Yes!; Overdispserion parameter is now smaller but still contributing
# quite a lot to the overall model; forecasts and smooth inferences are similar but slightly more
# precise (and forecast does appear to be a bit more accurate)
summary(mod3b)
plot(mod3b, type = 'forecast', data_test = data_test)
ppc(mod3b, type = 'rootogram')
ppc(mod3b, type = 'rootogram', data_test = data_test, n_bins = 25)
ppc(mod3b, type = 'cdf', data_test = data_test)
plot(mod3b, type = 'trend', data_test = data_test)
plot(mod3b, type = 'smooth', residuals = T)





# The dispersion and latent trend variance parameters can interact strongly
# particularly when dispersion is high (less need for a latent trend so the trend precision
# can go up to bloody infinity!; carefully selected priors are required to ensure the trend
# variance doesn't sample in outrageous spaces
MCMCvis::MCMCtrace(mod3b$model_output, c('twdis','tau'), pdf = F, n.eff = T)
plot(log(MCMCvis::MCMCchains(mod3b$model_output, 'twdis')),
     log(MCMCvis::MCMCchains(mod3b$model_output, 'tau')))


# Simulating data via sim_mvgam for a further Tweedie comparison
sim_data <- sim_mvgam(T = 60,
                      n_series = 4,
                      prop_missing = 0.1,
                      n_trends = 2,
                      train_prop = 0.833,
                      trend_rel = 0.6,
                      seasonality = 'shared',
                      phi_obs = c(0.4, 1.1, 0.4, 1.1),
                      mu_obs = c(6, 9, 3, 12),
                      family = 'tw')
sim_data$data_train$fake_cov <- rnorm(NROW(sim_data$data_train))
sim_data$data_test$fake_cov <- rnorm(NROW(sim_data$data_test))

sim_data$data_train$fake_cov2 <- rnorm(NROW(sim_data$data_train))
sim_data$data_test$fake_cov2 <- rnorm(NROW(sim_data$data_test))

hier_mod <- mvjagam(data_train = sim_data$data_train,
                    data_test = sim_data$data_test,
                    formula = y ~ fake_cov + fake_cov2 +
                      s(season, k = 12, m = 2, bs = 'cc'),
                    knots = list(season = c(0.5, 12.5)),
                    trend_model = 'RW',
                    use_lv = TRUE,
                    family = 'tw',
                    burnin = 1000)
hier_mod$model_file

summary(hier_mod)
plot(hier_mod$mgcv_model, all.terms = T, residuals = T)
plot(hier_mod, type = 're')
plot(hier_mod, type = 'pterms')
plot(hier_mod, series = 1, type = 'smooths')
plot(hier_mod, series = 1, type = 'forecast')
plot(hier_mod, series = 2, type = 'forecast')
plot(hier_mod, series = 1, type = 'trend')
plot(hier_mod, series = 2, type = 'trend')
plot(hier_mod, series = 1, type = 'residuals')
plot(hier_mod, series = 2, type = 'residuals')
ppc(hier_mod, series = 1, type = 'rootogram')
ppc(hier_mod, series = 2, type = 'rootogram')

MCMCvis::MCMCtrace(hier_mod$model_output, c('twdis','phi'), pdf = F, n.eff = T)
