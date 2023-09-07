## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,   
  dpi = 150,
  fig.asp = 0.8,
  fig.width = 6,
  out.width = "60%",
  fig.align = "center")
library(mvgam)
library(ggplot2)
theme_set(theme_bw(base_size = 12, base_family = 'serif'))

## -----------------------------------------------------------------------------
load(url('https://github.com/atsa-es/MARSS/raw/master/data/lakeWAplankton.rda'))

## -----------------------------------------------------------------------------
outcomes <- c('Greens', 'Bluegreens', 'Diatoms', 'Unicells', 'Other.algae')

## -----------------------------------------------------------------------------
# loop across each plankton group to create the long datframe
plankton_data <- do.call(rbind, lapply(outcomes, function(x){
  
  # create a group-specific dataframe with counts labelled 'y'
  # and the group name in the 'series' variable
  data.frame(year = lakeWAplanktonTrans[, 'Year'],
             month = lakeWAplanktonTrans[, 'Month'],
             y = lakeWAplanktonTrans[, x],
             series = x,
             temp = lakeWAplanktonTrans[, 'Temp'])})) %>%
  
  # change the 'series' label to a factor
  dplyr::mutate(series = factor(series)) %>%
  
  # filter to only include some years in the data
  dplyr::filter(year >= 1965 & year < 1975) %>%
  dplyr::arrange(year, month) %>%
  dplyr::group_by(series) %>%
  
  # z-score the counts so they are approximately standard normal
  dplyr::mutate(y = as.vector(scale(y))) %>%
  
  # add the time indicator
  dplyr::mutate(time = dplyr::row_number()) %>%
  dplyr::ungroup()

## -----------------------------------------------------------------------------
head(plankton_data)

## -----------------------------------------------------------------------------
dplyr::glimpse(plankton_data)

## -----------------------------------------------------------------------------
plot_mvgam_series(data = plankton_data, series = 'all')

## -----------------------------------------------------------------------------
image(is.na(t(plankton_data)), axes = F,
      col = c('grey80', 'darkred'))
axis(3, at = seq(0,1, len = NCOL(plankton_data)), 
     labels = colnames(plankton_data))

## -----------------------------------------------------------------------------
plankton_data %>%
  dplyr::filter(series == 'Other.algae') %>%
  ggplot(aes(x = time, y = temp)) +
  geom_line(size = 1.1) +
  geom_line(aes(y = y), col = 'white',
            size = 1.3) +
  geom_line(aes(y = y), col = 'darkred',
            size = 1.1) +
  ylab('z-score') +
  xlab('Time') +
  ggtitle('Temperature (black) vs Other algae (red)')

## -----------------------------------------------------------------------------
plankton_data %>%
  dplyr::filter(series == 'Diatoms') %>%
  ggplot(aes(x = time, y = temp)) +
  geom_line(size = 1.1) +
  geom_line(aes(y = y), col = 'white',
            size = 1.3) +
  geom_line(aes(y = y), col = 'darkred',
            size = 1.1) +
  ylab('z-score') +
  xlab('Time') +
  ggtitle('Temperature (black) vs Diatoms (red)')

## -----------------------------------------------------------------------------
plankton_data %>%
  dplyr::filter(series == 'Greens') %>%
  ggplot(aes(x = time, y = temp)) +
  geom_line(size = 1.1) +
  geom_line(aes(y = y), col = 'white',
            size = 1.3) +
  geom_line(aes(y = y), col = 'darkred',
            size = 1.1) +
  ylab('z-score') +
  xlab('Time') +
  ggtitle('Temperature (black) vs Greens (red)')

## -----------------------------------------------------------------------------
plankton_train <- plankton_data %>%
  dplyr::filter(time <= 112)
plankton_test <- plankton_data %>%
  dplyr::filter(time > 112)

## ----notrend_mod, include = FALSE, results='hide'-----------------------------
notrend_mod <- mvgam(y ~ 
                       te(temp, month, k = c(4, 4)) +
                       te(temp, month, k = c(4, 4), by = series),
                     family = gaussian(),
                     data = plankton_train,
                     newdata = plankton_test,
                     trend_model = 'None')

## ----eval=FALSE---------------------------------------------------------------
#  notrend_mod <- mvgam(y ~
#                         # tensor of temp and month to capture
#                         # "global" seasonality
#                         te(temp, month, k = c(4, 4)) +
#  
#                         # series-specific deviation tensor products
#                         te(temp, month, k = c(4, 4), by = series),
#                       family = gaussian(),
#                       data = plankton_train,
#                       newdata = plankton_test,
#                       trend_model = 'None')
#  

## -----------------------------------------------------------------------------
plot_mvgam_smooth(notrend_mod, smooth = 1)

## -----------------------------------------------------------------------------
plot_mvgam_smooth(notrend_mod, smooth = 2)

## -----------------------------------------------------------------------------
plot_mvgam_smooth(notrend_mod, smooth = 3)

## -----------------------------------------------------------------------------
plot_mvgam_smooth(notrend_mod, smooth = 4)

## -----------------------------------------------------------------------------
plot_mvgam_smooth(notrend_mod, smooth = 5)

## -----------------------------------------------------------------------------
plot_mvgam_smooth(notrend_mod, smooth = 6)

## -----------------------------------------------------------------------------
plot(notrend_mod, type = 'forecast', series = 1)

## -----------------------------------------------------------------------------
plot(notrend_mod, type = 'forecast', series = 2)

## -----------------------------------------------------------------------------
plot(notrend_mod, type = 'forecast', series = 3)

## -----------------------------------------------------------------------------
plot(notrend_mod, type = 'forecast', series = 4)

## -----------------------------------------------------------------------------
plot(notrend_mod, type = 'forecast', series = 5)

## -----------------------------------------------------------------------------
plot(notrend_mod, type = 'residuals', series = 1)

## -----------------------------------------------------------------------------
plot(notrend_mod, type = 'residuals', series = 2)

## -----------------------------------------------------------------------------
plot(notrend_mod, type = 'residuals', series = 3)

## -----------------------------------------------------------------------------
plot(notrend_mod, type = 'residuals', series = 4)

## -----------------------------------------------------------------------------
plot(notrend_mod, type = 'residuals', series = 5)

## -----------------------------------------------------------------------------
priors <- get_mvgam_priors(
  # observation formula, which just uses an intercept
  y ~ 1,
  
  # process model formula, which includes the smooth functions
  trend_formula = ~ te(temp, month, k = c(4, 4)) +
    te(temp, month, k = c(4, 4), by = trend),
  
  # VAR1 model with uncorrelated process errors
  trend_model = 'VAR1',
  family = gaussian(),
  data = plankton_train)

# update the sigma_obs prior so that it avoids very small
# and very large values that are nonsensical
priors$prior[10] <- "sigma_obs ~ uniform(0.1, 1);"
priors$new_lowerbound[10] <- 0.1
priors$new_upperbound[10] <- 1

## ----eval=FALSE---------------------------------------------------------------
#  priors <- prior(uniform(0.1, 1), class = sigma_obs, lb = 0.1, ub = 1)

## ----var_mod, include = FALSE, results='hide'---------------------------------
var_mod <- mvgam(y ~ 1,
                 trend_formula = ~
                   # tensor of temp and month should capture
                   # seasonality
                   te(temp, month, k = c(4, 4)) +
                   # need to use 'trend' rather than series
                   # here
                   te(temp, month, k = c(4, 4), by = trend),
                 family = gaussian(),
                 data = plankton_train,
                 newdata = plankton_test,
                 trend_model = 'VAR1',
                 priors = priors, 
                 burnin = 800)

## ----eval=FALSE---------------------------------------------------------------
#  var_mod <- mvgam(
#    # observation formula, which just uses an intercept
#    y ~ 1,
#  
#    # process model formula, which includes the smooth functions
#    trend_formula = ~ te(temp, month, k = c(4, 4)) +
#      te(temp, month, k = c(4, 4), by = trend),
#  
#    # VAR1 model with uncorrelated process errors
#    trend_model = 'VAR1',
#    family = gaussian(),
#    data = plankton_train,
#    newdata = plankton_test,
#  
#    # include the updated priors
#    priors = priors)

## -----------------------------------------------------------------------------
summary(var_mod)

## -----------------------------------------------------------------------------
plot(var_mod, 'smooths', trend_effects = TRUE)

## ----warning=FALSE, message=FALSE---------------------------------------------
mcmc_plot(var_mod, variable = 'A', regex = TRUE, type = 'hist')

## ----warning=FALSE, message=FALSE---------------------------------------------
A_pars <- matrix(NA, nrow = 5, ncol = 5)
for(i in 1:5){
  for(j in 1:5){
    A_pars[i, j] <- paste0('A[', i, ',', j, ']')
  }
}
mcmc_plot(var_mod, 
          variable = as.vector(t(A_pars)), 
          type = 'hist')

## -----------------------------------------------------------------------------
plot(var_mod, type = 'trend', series = 1)

## -----------------------------------------------------------------------------
plot(var_mod, type = 'trend', series = 3)

## ----warning=FALSE, message=FALSE---------------------------------------------
Sigma_pars <- matrix(NA, nrow = 5, ncol = 5)
for(i in 1:5){
  for(j in 1:5){
    Sigma_pars[i, j] <- paste0('Sigma[', i, ',', j, ']')
  }
}
mcmc_plot(var_mod, 
          variable = as.vector(t(Sigma_pars)), 
          type = 'hist')

## ----warning=FALSE, message=FALSE---------------------------------------------
mcmc_plot(var_mod, variable = 'sigma_obs', regex = TRUE, type = 'hist')

## -----------------------------------------------------------------------------
priors <- get_mvgam_priors(
  # observation formula, which just uses an intercept
  y ~ 1,
  
  # process model formula, which includes the smooth functions
  trend_formula = ~ te(temp, month, k = c(4, 4)) +
    te(temp, month, k = c(4, 4), by = trend),
  
  # VAR1 model with correlated process errors
  trend_model = 'VAR1cor',
  family = gaussian(),
  data = plankton_train)

# update the sigma_obs prior so that it avoids very small
# and very large values that are nonsensical
priors$prior[11] <- "sigma_obs ~ uniform(0.1, 1);"
priors$new_lowerbound[11] <- 0.1
priors$new_upperbound[11] <- 1

## ----varcor_mod, include = FALSE, results='hide'------------------------------
varcor_mod <- mvgam(y ~ 1,
                 trend_formula = ~
                   # tensor of temp and month should capture
                   # seasonality
                   te(temp, month, k = c(4, 4)) +
                   # need to use 'trend' rather than series
                   # here
                   te(temp, month, k = c(4, 4), by = trend),
                 family = gaussian(),
                 data = plankton_train,
                 newdata = plankton_test,
                 trend_model = 'VAR1cor',
                 burnin = 800,
                 priors = priors)

## ----eval=FALSE---------------------------------------------------------------
#  varcor_mod <- mvgam(
#    # observation formula, which just uses an intercept
#    y ~ 1,
#  
#    # process model formula, which includes the smooth functions
#    trend_formula = ~ te(temp, month, k = c(4, 4)) +
#      te(temp, month, k = c(4, 4), by = trend),
#  
#    # VAR1 model with correlated process errors
#    trend_model = 'VAR1cor',
#    family = gaussian(),
#    data = plankton_train,
#    newdata = plankton_test,
#  
#    # include the updated priors
#    priors = priors)

