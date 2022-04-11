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

# Fit dynamic GAMs
# Poisson model
mod1 <- mvjagam(data_train = data_train,
                data_test = data_test,
                formula = y ~ s(season, k = 15, bs = 'cc') +
                  s(time, k = 8, bs = 'gp'),
                knots = list(season = c(0.5, 24.5)),
                family = 'poisson',
                trend_model = 'AR1',
                chains = 4,
                burnin = 6000)

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
plot(mod1, type = 'residuals')
plot(mod2, type = 'residuals')
plot(mod3, type = 'residuals')

# Examine out of sample forecast distributions
plot(mod1, type = 'forecast', data_test = data_test)
plot(mod2, type = 'forecast', data_test = data_test)
plot(mod3, type = 'forecast', data_test = data_test)

# Examine estimated temporal processes
# Examine out of sample forecast distributions
plot(mod1, type = 'trend', data_test = data_test)
plot(mod2, type = 'trend', data_test = data_test)
plot(mod3, type = 'trend', data_test = data_test)

# Examine estimated smooth functions
plot(mod1, type = 'smooth', residuals = T)
plot(mod2, type = 'smooth', residuals = T)
plot(mod3, type = 'smooth', residuals = T)

# In-sample DICs
dic(mod1)
dic(mod2)
dic(mod3)

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
