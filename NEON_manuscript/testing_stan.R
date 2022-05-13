# Testing Rstan for modelling discrete time series with missing values


# Extract data and modify for fitting in Stan; requires the full JAGS model be made first,
# but not actually fitted
model_data <- mod1$model_data
model_file <- mod1$model_file
trend_model <- mod1$trend_model
drift <- mod1$drift
ss_jagam <- list()
ss_jagam$jags.data <- model_data
family <- mod1$family
use_lv <- mod1$use_lv
# These will get used within mvjagam #
#model_data <- ss_jagam$jags.data
#model_file <- trimws(model_file)
#trend_model <- trend_model
#chains <- chains

# Import the base Stan model file
modification <- add_base_dgam_lines(stan = TRUE)
unlink('base_gam_stan.txt')
cat(modification, file = 'base_gam_stan.txt', sep = '\n', append = T)
base_stan_model <- trimws(suppressWarnings(readLines('base_gam_stan.txt')))
unlink('base_gam_stan.txt')

# Add necessary trend structure
base_stan_model <- add_trend_lines(model_file = base_stan_model,
                                   stan = T,
                                   trend_model = trend_model,
                                   drift = drift)

# Add remaining data, model and parameters blocks to the Stan model file;
# gather Stan data structure
stan_objects <- add_stan_data(jags_file = model_file,
                              stan_file = base_stan_model,
                              jags_data = ss_jagam$jags.data)

library(rstan)
options(mc.cores = parallel::detectCores())
chains <- 4

if(trend_model != 'None'){
  initf1 <- function() {
    list(b_raw = runif(stan_objects$model_data$num_basis, -0.25, 0.25))
  }
} else {
  initf1 <- function() {
    list(b_raw = runif(stan_objects$model_data$num_basis, -0.25, 0.25),
         sigma = runif(stan_objects$model_data$n_series, 0.075, 1))
  }
}


fit1 <- stan(model_code = stan_objects$stan_file,
             iter = 2000, chains = chains,
             data = stan_objects$model_data,
             cores = min(c(chains, parallel::detectCores() - 1)),
             init = initf1,
             verbose = F,
             pars = get_monitor_pars(family = family,
                                     use_lv = use_lv,
                                     trend_model = trend_model,
                                     drift = drift),
             refresh = 500)
source('https://raw.githubusercontent.com/betanalpha/knitr_case_studies/master/factor_modeling/stan_utility.R')
check_all_diagnostics(fit1)

# Convert stanfit object to samples
library(coda)
model_output <- mcmc.list(lapply(1:NCOL(fit1),
                                 function(x) mcmc(as.array(fit1)[,x,])))

MCMCvis::MCMCtrace(model_output, 'rho', pdf = F)
MCMCvis::MCMCtrace(model_output, 'phi', pdf = F)
MCMCvis::MCMCtrace(model_output, 'ar1', pdf = F)
MCMCvis::MCMCtrace(model_output, 'ar2', pdf = F)
MCMCvis::MCMCtrace(model_output, c('sigma'), pdf = F)

object <- mod1
object$model_output <- model_output
class(object) = 'mvgam'
summary(object)
summary(mod1)

par(mfrow = c(2,1))
plot(object, type = 'forecast', series = 1)
plot(mod1, type = 'forecast', series = 1)
plot(hier_mod, type = 'forecast', series = 1, data_test = sim_data$data_test)
plot(object, type = 'forecast', series = 2, data_test = sim_data$data_test)
plot(hier_mod, type = 'forecast', series = 2, data_test = sim_data$data_test)
plot(object, type = 'forecast', series = 3, data_test = sim_data$data_test)
plot(hier_mod, type = 'forecast', series = 3, data_test = sim_data$data_test)

plot(object, type = 'trend', series = 1, data_test = sim_data$data_test)
plot(hier_mod, type = 'trend', series = 1, data_test = sim_data$data_test)
plot(object, type = 'trend', series = 2, data_test = sim_data$data_test)
plot(hier_mod, type = 'trend', series = 2, data_test = sim_data$data_test)
plot(object, type = 'trend', series = 3, data_test = sim_data$data_test)
plot(hier_mod, type = 'trend', series = 3, data_test = sim_data$data_test)


plot(object, type = 'smooths')
plot(hier_mod, type = 'smooths')

ppc(object, type = 'rootogram', series = 2)
ppc(hier_mod, type = 'rootogram', series = 2)

plot(object, type = 're')
plot(hier_mod, type = 're')
