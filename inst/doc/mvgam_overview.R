## ----echo = FALSE--------------------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  purl = NOT_CRAN,
  eval = NOT_CRAN
)


## ----setup, include=FALSE------------------------------------------------------
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


## ----Access time series data---------------------------------------------------
data("portal_data")


## ----Inspect data format and structure-----------------------------------------
head(portal_data)


## ------------------------------------------------------------------------------
dplyr::glimpse(portal_data)


## ------------------------------------------------------------------------------
data <- sim_mvgam(n_series = 4, T = 24)
head(data$data_train, 12)


## ----Wrangle data for modelling------------------------------------------------
portal_data %>%
  
  # mvgam requires a 'time' variable be present in the data to index
  # the temporal observations. This is especially important when tracking 
  # multiple time series. In the Portal data, the 'moon' variable indexes the
  # lunar monthly timestep of the trapping sessions
  dplyr::mutate(time = moon - (min(moon)) + 1) %>%
  
  # We can also provide a more informative name for the outcome variable, which 
  # is counts of the 'PP' species (Chaetodipus penicillatus) across all control
  # plots
  dplyr::mutate(count = PP) %>%
  
  # The other requirement for mvgam is a 'series' variable, which needs to be a
  # factor variable to index which time series each row in the data belongs to.
  # Again, this is more useful when you have multiple time series in the data
  dplyr::mutate(series = as.factor('PP')) %>%
  
  # Select the variables of interest to keep in the model_data
  dplyr::select(series, year, time, count, mintemp, ndvi) -> model_data


## ------------------------------------------------------------------------------
head(model_data)


## ------------------------------------------------------------------------------
dplyr::glimpse(model_data)


## ----Summarise variables-------------------------------------------------------
summary(model_data)


## ------------------------------------------------------------------------------
plot_mvgam_series(data = model_data, series = 1, y = 'count')


## ------------------------------------------------------------------------------
model_data %>%
  
  # Create a 'year_fac' factor version of 'year'
  dplyr::mutate(year_fac = factor(year)) -> model_data


## ------------------------------------------------------------------------------
dplyr::glimpse(model_data)
levels(model_data$year_fac)


## ----model1, include=FALSE, results='hide'-------------------------------------
model1 <- mvgam(count ~ s(year_fac, bs = 're') - 1,
                family = poisson(),
                data = model_data,
                parallel = FALSE)


## ----eval=FALSE----------------------------------------------------------------
## model1 <- mvgam(count ~ s(year_fac, bs = 're') - 1,
##                 family = poisson(),
##                 data = model_data)


## ------------------------------------------------------------------------------
get_mvgam_priors(count ~ s(year_fac, bs = 're') - 1,
                 family = poisson(),
                 data = model_data)


## ------------------------------------------------------------------------------
summary(model1)


## ----Extract coefficient posteriors--------------------------------------------
beta_post <- as.data.frame(model1, variable = 'betas')
dplyr::glimpse(beta_post)


## ------------------------------------------------------------------------------
code(model1)


## ----Plot random effect estimates----------------------------------------------
plot(model1, type = 're')


## ------------------------------------------------------------------------------
mcmc_plot(object = model1,
          variable = 'betas',
          type = 'areas')


## ------------------------------------------------------------------------------
pp_check(object = model1)


## ----Plot posterior hindcasts--------------------------------------------------
plot(model1, type = 'forecast')


## ----Extract posterior hindcast------------------------------------------------
hc <- hindcast(model1)
str(hc)


## ----Extract hindcasts on the linear predictor scale---------------------------
hc <- hindcast(model1, type = 'link')
range(hc$hindcasts$PP)


## ----Plot posterior residuals--------------------------------------------------
plot(model1, type = 'residuals')


## ------------------------------------------------------------------------------
model_data %>% 
  dplyr::filter(time <= 160) -> data_train 
model_data %>% 
  dplyr::filter(time > 160) -> data_test


## ----include=FALSE, message=FALSE, warning=FALSE-------------------------------
model1b <- mvgam(count ~ s(year_fac, bs = 're') - 1,
                family = poisson(),
                data = data_train,
                newdata = data_test,
                parallel = FALSE)


## ----eval=FALSE----------------------------------------------------------------
## model1b <- mvgam(count ~ s(year_fac, bs = 're') - 1,
##                  family = poisson(),
##                  data = data_train,
##                  newdata = data_test)


## ----Plotting predictions against test data------------------------------------
plot(model1b, type = 'forecast', newdata = data_test)


## ----Extract posterior forecasts-----------------------------------------------
fc <- forecast(model1b)
str(fc)


## ----model2, include=FALSE, message=FALSE, warning=FALSE-----------------------
model2 <- mvgam(count ~ s(year_fac, bs = 're') + 
                  ndvi - 1,
                family = poisson(),
                data = data_train,
                newdata = data_test,
                parallel = FALSE)


## ----eval=FALSE----------------------------------------------------------------
## model2 <- mvgam(count ~ s(year_fac, bs = 're') +
##                   ndvi - 1,
##                 family = poisson(),
##                 data = data_train,
##                 newdata = data_test)


## ----class.output="scroll-300"-------------------------------------------------
summary(model2)


## ----Posterior quantiles of model coefficients---------------------------------
coef(model2)


## ------------------------------------------------------------------------------
beta_post <- as.data.frame(model2, variable = 'betas')
dplyr::glimpse(beta_post)


## ----Histogram of NDVI effects-------------------------------------------------
hist(beta_post$ndvi,
     xlim = c(-1 * max(abs(beta_post$ndvi)),
              max(abs(beta_post$ndvi))),
     col = 'darkred',
     border = 'white',
     xlab = expression(beta[NDVI]),
     ylab = '',
     yaxt = 'n',
     main = '',
     lwd = 2)
abline(v = 0, lwd = 2.5)


## ----warning=FALSE-------------------------------------------------------------
conditional_effects(model2)


## ----model3, include=FALSE, message=FALSE, warning=FALSE-----------------------
model3 <- mvgam(count ~ s(time, bs = 'bs', k = 15) + 
                  ndvi,
                family = poisson(),
                data = data_train,
                newdata = data_test,
                parallel = FALSE)


## ----eval=FALSE----------------------------------------------------------------
## model3 <- mvgam(count ~ s(time, bs = 'bs', k = 15) +
##                   ndvi,
##                 family = poisson(),
##                 data = data_train,
##                 newdata = data_test)


## ------------------------------------------------------------------------------
summary(model3)


## ----warning=FALSE-------------------------------------------------------------
conditional_effects(model3, type = 'link')


## ----class.output="scroll-300"-------------------------------------------------
code(model3)


## ------------------------------------------------------------------------------
plot(model3, type = 'forecast', newdata = data_test)


## ----Plot extrapolated temporal functions using newdata------------------------
plot_mvgam_smooth(model3, smooth = 's(time)',
                  # feed newdata to the plot function to generate
                  # predictions of the temporal smooth to the end of the 
                  # testing period
                  newdata = data.frame(time = 1:max(data_test$time),
                                       ndvi = 0))
abline(v = max(data_train$time), lty = 'dashed', lwd = 2)


## ----model4, include=FALSE-----------------------------------------------------
model4 <- mvgam(count ~ s(ndvi, k = 6),
                family = poisson(),
                data = data_train,
                newdata = data_test,
                trend_model = 'AR1',
                parallel = FALSE)


## ----eval=FALSE----------------------------------------------------------------
## model4 <- mvgam(count ~ s(ndvi, k = 6),
##                 family = poisson(),
##                 data = data_train,
##                 newdata = data_test,
##                 trend_model = 'AR1')


## ----Summarise the mvgam autocorrelated error model, class.output="scroll-300"----
summary(model4)


## ------------------------------------------------------------------------------
plot(model4, type = 'forecast', newdata = data_test)


## ------------------------------------------------------------------------------
plot(model4, type = 'trend', newdata = data_test)


## ------------------------------------------------------------------------------
loo_compare(model3, model4)


## ------------------------------------------------------------------------------
fc_mod3 <- forecast(model3)
fc_mod4 <- forecast(model4)
score_mod3 <- score(fc_mod3, score = 'drps')
score_mod4 <- score(fc_mod4, score = 'drps')
sum(score_mod4$PP$score, na.rm = TRUE) - sum(score_mod3$PP$score, na.rm = TRUE)

