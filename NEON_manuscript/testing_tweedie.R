# Simulate a seasonal smooth and some autocorrelated, Tweedie distributed discrete random variates
season <- rep(seq(1, 24), 6)
mean_func = function(x){
  0.1 + x * 0.01 + sin(0.1 * x) - cos(-0.15 * x)
}
set.seed(15)
y = rpois(length(season),
          lambda = mgcv::rTweedie(mu = exp(-0.9 + mean_func(season) +
                                             cumsum(rnorm(length(season), sd = 0.075))),
                                  p = 1.5, phi = 1.1))
plot(y, type = 'l')

# Split data into training and testing
data_train <-
  data.frame(y = y[1:125],
             season = season[1:125],
             time = 1:125,
             ran1 = rnorm(125),
             ran2 = rnorm(125))
data_test <-
  data.frame(y = y[126:length(season)],
             season = season[126:length(season)],
             time = 126:length(season),
             ran1 = rnorm(length(season)-125),
             ran2 = rnorm(length(season)-125))

# Fit dynamic GAMs
# Poisson model
mod1 <- mvjagam(data_train = data_train,
                data_test = data_test,
                formula = y ~ s(season, k = 15, bs = 'cc') +
                  s(time, k = 8, bs = 'gp') +
                  s(ran1) + s(ran2) + ti(ran1, ran2),
                knots = list(season = c(0.5, 24.5)),
                family = 'poisson',
                trend_model = 'AR1',
                chains = 4,
                burnin = 1000)
mod1
summary(mod1)
predict(mod1)
plot(mod1, type = 'smooths', smooth_residuals = T)
plot(mod1, type = 'residuals')
plot(mod1, type = 'forecast')
plot(mod1, type = 'uncertainty', data_test = data_test)

dic(mod1)



# Negative binomial model
mod2 <- mvjagam(data_train = data_train,
                data_test = data_test,
                formula = y ~ s(season, k = 15, bs = 'cc') +
                  s(time, k = 8, bs = 'gp'),
                knots = list(season = c(0.5, 24.5)),
                family = 'nb',
                trend_model = 'AR1',
                chains = 4,
                burnin = 6000)

# Tweedie model
mod3 <- mvjagam(data_train = data_train,
                data_test = data_test,
                formula = y ~ s(season, k = 15, bs = 'cc') +
                  s(time, k = 8, bs = 'gp'),
                knots = list(season = c(0.5, 24.5)),
                family = 'tw',
                trend_model = 'AR1',
                chains = 4,
                burnin = 12000)

# Examine DS residual distributions
plot_mvgam_resids(mod1)
plot_mvgam_resids(mod2)
plot_mvgam_resids(mod3)

# Examine out of sample forecast distributions
plot_mvgam_fc(mod1, data_test = data_test)
plot_mvgam_fc(mod2, data_test = data_test)
plot_mvgam_fc(mod3, data_test = data_test)

# Examine estimated temporal processes
# Examine out of sample forecast distributions
plot_mvgam_trend(mod1, data_test = data_test)
plot_mvgam_trend(mod2, data_test = data_test)
plot_mvgam_trend(mod3, data_test = data_test)

# Examine estimated smooth functions
plot_mvgam_smooth(object = mod1, series = 1,
                  smooth = 'season', residuals = T)
plot_mvgam_smooth(object = mod2, series = 1,
                  smooth = 'season', residuals = T)
plot_mvgam_smooth(object = mod3, series = 1,
                  smooth = 'season', residuals = T)


# Testing the particle filtering and compare_mvgam functions
pfilter_mvgam_init(object = mod3, n_particles = 5000,
                   n_cores = 4, data_assim = data_test)

pfilter_mvgam_online(data_assim = data_test[1:5,],
                     n_cores = 4, kernel_lambda = 1)

fc <- pfilter_mvgam_fc(file_path = "pfilter",
                       n_cores = 4, data_test = data_test)
fc$series1()
unlink('pfilter', recursive = T, force = T)

compare_mvgams(model1 = mod1, model2 = mod3,
               fc_horizon = 6, n_evaluations = 25, n_cores = 4)

