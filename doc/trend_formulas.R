## ----echo = FALSE-------------------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  purl = NOT_CRAN,
  eval = NOT_CRAN
)


## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,   
  dpi = 100,
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
## notrend_mod <- mvgam(y ~
##                        # tensor of temp and month to capture
##                        # "global" seasonality
##                        te(temp, month, k = c(4, 4)) +
## 
##                        # series-specific deviation tensor products
##                        te(temp, month, k = c(4, 4), by = series),
##                      family = gaussian(),
##                      data = plankton_train,
##                      newdata = plankton_test,
##                      trend_model = 'None')
## 


## -----------------------------------------------------------------------------
plot_mvgam_smooth(notrend_mod, smooth = 1)


## -----------------------------------------------------------------------------
plot_mvgam_smooth(notrend_mod, smooth = 2)


## -----------------------------------------------------------------------------
plot_mvgam_smooth(notrend_mod, smooth = 3)


## -----------------------------------------------------------------------------
plot(notrend_mod, type = 'forecast', series = 1)


## -----------------------------------------------------------------------------
plot(notrend_mod, type = 'forecast', series = 2)


## -----------------------------------------------------------------------------
plot(notrend_mod, type = 'forecast', series = 3)


## -----------------------------------------------------------------------------
plot(notrend_mod, type = 'residuals', series = 1)


## -----------------------------------------------------------------------------
plot(notrend_mod, type = 'residuals', series = 2)


## -----------------------------------------------------------------------------
plot(notrend_mod, type = 'residuals', series = 3)


## -----------------------------------------------------------------------------
priors <- get_mvgam_priors(
  # observation formula, which has no terms in it
  y ~ -1,
  
  # process model formula, which includes the smooth functions
  trend_formula = ~ te(temp, month, k = c(4, 4)) +
    te(temp, month, k = c(4, 4), by = trend),
  
  # VAR1 model with uncorrelated process errors
  trend_model = VAR(),
  family = gaussian(),
  data = plankton_train)


## -----------------------------------------------------------------------------
priors[, 3]


## -----------------------------------------------------------------------------
priors[, 4]


## -----------------------------------------------------------------------------
priors <- c(prior(normal(0.5, 0.1), class = sigma_obs, lb = 0.2),
            prior(normal(0.5, 0.25), class = sigma))


## ----var_mod, include = FALSE, results='hide'---------------------------------
var_mod <- mvgam(y ~ -1,
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
                 trend_model = VAR(),
                 priors = priors, 
                 adapt_delta = 0.99,
                 burnin = 1000)


## ----eval=FALSE---------------------------------------------------------------
## var_mod <- mvgam(
##   # observation formula, which is empty
##   y ~ -1,
## 
##   # process model formula, which includes the smooth functions
##   trend_formula = ~ te(temp, month, k = c(4, 4)) +
##     te(temp, month, k = c(4, 4), by = trend),
## 
##   # VAR1 model with uncorrelated process errors
##   trend_model = VAR(),
##   family = gaussian(),
##   data = plankton_train,
##   newdata = plankton_test,
## 
##   # include the updated priors
##   priors = priors,
##   silent = 2)


## -----------------------------------------------------------------------------
summary(var_mod, include_betas = FALSE)


## -----------------------------------------------------------------------------
plot(var_mod, 'smooths', trend_effects = TRUE)


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
priors <- c(prior(normal(0.5, 0.1), class = sigma_obs, lb = 0.2),
            prior(normal(0.5, 0.25), class = sigma))


## ----varcor_mod, include = FALSE, results='hide'------------------------------
varcor_mod <- mvgam(y ~ -1,
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
                 trend_model = VAR(cor = TRUE),
                 burnin = 1000,
                 adapt_delta = 0.99,
                 priors = priors)


## ----eval=FALSE---------------------------------------------------------------
## varcor_mod <- mvgam(
##   # observation formula, which remains empty
##   y ~ -1,
## 
##   # process model formula, which includes the smooth functions
##   trend_formula = ~ te(temp, month, k = c(4, 4)) +
##     te(temp, month, k = c(4, 4), by = trend),
## 
##   # VAR1 model with correlated process errors
##   trend_model = VAR(cor = TRUE),
##   family = gaussian(),
##   data = plankton_train,
##   newdata = plankton_test,
## 
##   # include the updated priors
##   priors = priors,
##   silent = 2)


## ----warning=FALSE, message=FALSE---------------------------------------------
Sigma_pars <- matrix(NA, nrow = 5, ncol = 5)
for(i in 1:5){
  for(j in 1:5){
    Sigma_pars[i, j] <- paste0('Sigma[', i, ',', j, ']')
  }
}
mcmc_plot(varcor_mod, 
          variable = as.vector(t(Sigma_pars)), 
          type = 'hist')


## -----------------------------------------------------------------------------
Sigma_post <- as.matrix(varcor_mod, variable = 'Sigma', regex = TRUE)
median_correlations <- cov2cor(matrix(apply(Sigma_post, 2, median),
                                      nrow = 5, ncol = 5))
rownames(median_correlations) <- colnames(median_correlations) <- levels(plankton_train$series)

round(median_correlations, 2)


## -----------------------------------------------------------------------------
# create forecast objects for each model
fcvar <- forecast(var_mod)
fcvarcor <- forecast(varcor_mod)

# plot the difference in variogram scores; a negative value means the VAR1cor model is better, while a positive value means the VAR1 model is better
diff_scores <- score(fcvarcor, score = 'variogram')$all_series$score -
  score(fcvar, score = 'variogram')$all_series$score
plot(diff_scores, pch = 16, cex = 1.25, col = 'darkred', 
     ylim = c(-1*max(abs(diff_scores), na.rm = TRUE),
              max(abs(diff_scores), na.rm = TRUE)),
     bty = 'l',
     xlab = 'Forecast horizon',
     ylab = expression(variogram[VAR1cor]~-~variogram[VAR1]))
abline(h = 0, lty = 'dashed')


## -----------------------------------------------------------------------------
# plot the difference in energy scores; a negative value means the VAR1cor model is better, while a positive value means the VAR1 model is better
diff_scores <- score(fcvarcor, score = 'energy')$all_series$score -
  score(fcvar, score = 'energy')$all_series$score
plot(diff_scores, pch = 16, cex = 1.25, col = 'darkred', 
     ylim = c(-1*max(abs(diff_scores), na.rm = TRUE),
              max(abs(diff_scores), na.rm = TRUE)),
     bty = 'l',
     xlab = 'Forecast horizon',
     ylab = expression(energy[VAR1cor]~-~energy[VAR1]))
abline(h = 0, lty = 'dashed')

