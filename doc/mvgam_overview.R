params <-
  list(EVAL = TRUE)

## ----echo = FALSE----------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE
)


## ----setup, include=FALSE--------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  dpi = 100,
  fig.asp = 0.8,
  fig.width = 6,
  out.width = "60%",
  fig.align = "center"
)
library(mvgam)
library(ggplot2)
theme_set(theme_bw(base_size = 12, base_family = "serif"))


## ----Access time series data-----------------------------------------------------
data("portal_data")


## ----Inspect data format and structure-------------------------------------------
head(portal_data)


## --------------------------------------------------------------------------------
dplyr::glimpse(portal_data)


## --------------------------------------------------------------------------------
data <- sim_mvgam(n_series = 4, T = 24)
head(data$data_train, 12)


## ----Wrangle data for modelling--------------------------------------------------
portal_data %>%
  # Filter the data to only contain captures of the 'PP'
  dplyr::filter(series == 'PP') %>%
  droplevels() %>%
  dplyr::mutate(count = captures) %>%
  # Add a 'year' variable
  dplyr::mutate(year = sort(rep(1:8, 12))[time]) %>%
  # Select the variables of interest to keep in the model_data
  dplyr::select(series, year, time, count, mintemp, ndvi_ma12) -> model_data


## --------------------------------------------------------------------------------
head(model_data)


## --------------------------------------------------------------------------------
dplyr::glimpse(model_data)


## ----Summarise variables---------------------------------------------------------
summary(model_data)


## --------------------------------------------------------------------------------
plot_mvgam_series(data = model_data, series = 1, y = "count")


## --------------------------------------------------------------------------------
model_data %>%
  # Create a 'year_fac' factor version of 'year'
  dplyr::mutate(year_fac = factor(year)) -> model_data


## --------------------------------------------------------------------------------
dplyr::glimpse(model_data)
levels(model_data$year_fac)


## ----model1, include=FALSE, results='hide'---------------------------------------
model1 <- mvgam(
  count ~ s(year_fac, bs = "re") - 1,
  family = poisson(),
  data = model_data,
  parallel = FALSE
)


## ----eval=FALSE------------------------------------------------------------------
# model1 <- mvgam(
#   count ~ s(year_fac, bs = "re") - 1,
#   family = poisson(),
#   data = model_data
# )

## --------------------------------------------------------------------------------
get_mvgam_priors(
  count ~ s(year_fac, bs = "re") - 1,
  family = poisson(),
  data = model_data
)


## --------------------------------------------------------------------------------
summary(model1)


## ----Extract coefficient posteriors----------------------------------------------
beta_post <- as.data.frame(model1, variable = "betas")
dplyr::glimpse(beta_post)


## --------------------------------------------------------------------------------
stancode(model1)


## ----Plot random effect estimates------------------------------------------------
plot(model1, type = "re")


## --------------------------------------------------------------------------------
mcmc_plot(
  object = model1,
  variable = "betas",
  type = "areas"
)


## --------------------------------------------------------------------------------
pp_check(object = model1)


## ----Plot posterior hindcasts----------------------------------------------------
plot(model1, type = "forecast")


## ----Extract posterior hindcast--------------------------------------------------
hc <- hindcast(model1)
str(hc)


## ----Extract hindcasts on the linear predictor scale-----------------------------
hc <- hindcast(model1, type = "link")
range(hc$hindcasts$PP)


## ----Plot posterior residuals----------------------------------------------------
plot(model1, type = "residuals")


## --------------------------------------------------------------------------------
model_data %>%
  dplyr::filter(time <= 70) -> data_train
model_data %>%
  dplyr::filter(time > 70) -> data_test


## ----include=FALSE, message=FALSE, warning=FALSE---------------------------------
model1b <- mvgam(
  count ~ s(year_fac, bs = "re") - 1,
  family = poisson(),
  data = data_train,
  newdata = data_test,
  parallel = FALSE
)


## ----eval=FALSE------------------------------------------------------------------
# model1b <- mvgam(
#   count ~ s(year_fac, bs = "re") - 1,
#   family = poisson(),
#   data = data_train,
#   newdata = data_test
# )

## ----Plotting predictions against test data--------------------------------------
plot(model1b, type = "forecast", newdata = data_test)


## ----Extract posterior forecasts-------------------------------------------------
fc <- forecast(model1b)
str(fc)


## ----model2, include=FALSE, message=FALSE, warning=FALSE-------------------------
model2 <- mvgam(
  count ~ s(year_fac, bs = "re") +
    ndvi_ma12 -
    1,
  family = poisson(),
  data = data_train,
  newdata = data_test,
  parallel = FALSE
)


## ----eval=FALSE------------------------------------------------------------------
# model2 <- mvgam(
#   count ~ s(year_fac, bs = "re") +
#     ndvi_ma12 - 1,
#   family = poisson(),
#   data = data_train,
#   newdata = data_test
# )

## ----class.output="scroll-300"---------------------------------------------------
summary(model2)


## ----Posterior quantiles of model coefficients-----------------------------------
coef(model2)


## --------------------------------------------------------------------------------
beta_post <- as.data.frame(model2, variable = "betas")
dplyr::glimpse(beta_post)


## ----Histogram of NDVI effects---------------------------------------------------
hist(
  beta_post$ndvi_ma12,
  xlim = c(
    -1 * max(abs(beta_post$ndvi_ma12)),
    max(abs(beta_post$ndvi))
  ),
  col = "darkred",
  border = "white",
  xlab = expression(beta[NDVI]),
  ylab = "",
  yaxt = "n",
  main = "",
  lwd = 2
)
abline(v = 0, lwd = 2.5)


## ----warning=FALSE---------------------------------------------------------------
conditional_effects(model2)


## ----model3, include=FALSE, message=FALSE, warning=FALSE-------------------------
model3 <- mvgam(
  count ~ s(time, bs = "bs", k = 15) +
    ndvi_ma12,
  family = poisson(),
  data = data_train,
  newdata = data_test,
  parallel = FALSE
)


## ----eval=FALSE------------------------------------------------------------------
# model3 <- mvgam(
#   count ~ s(time, bs = "bs", k = 15) +
#     ndvi_ma12,
#   family = poisson(),
#   data = data_train,
#   newdata = data_test
# )

## --------------------------------------------------------------------------------
summary(model3)


## ----warning=FALSE---------------------------------------------------------------
conditional_effects(model3, type = "link")


## ----class.output="scroll-300"---------------------------------------------------
stancode(model3)


## --------------------------------------------------------------------------------
plot(model3, type = "forecast", newdata = data_test)


## ----Plot extrapolated temporal functions using newdata--------------------------
plot_mvgam_smooth(
  model3,
  smooth = "s(time)",
  # pass newdata to the plot function to generate
  # predictions of the temporal smooth to the end of the
  # testing period
  newdata = data.frame(
    time = 1:max(data_test$time),
    ndvi_ma12 = 0
  )
)
abline(v = max(data_train$time), lty = "dashed", lwd = 2)


## ----model4, include=FALSE-------------------------------------------------------
model4 <- mvgam(
  count ~ s(ndvi_ma12, k = 6),
  family = poisson(),
  data = data_train,
  newdata = data_test,
  trend_model = AR(),
  parallel = FALSE
)


## ----eval=FALSE------------------------------------------------------------------
# model4 <- mvgam(
#   count ~ s(ndvi_ma12, k = 6),
#   family = poisson(),
#   data = data_train,
#   newdata = data_test,
#   trend_model = AR()
# )

## ----Summarise the mvgam autocorrelated error model, class.output="scroll-300"----
summary(model4)


## --------------------------------------------------------------------------------
plot(model4, type = "forecast", newdata = data_test)


## --------------------------------------------------------------------------------
plot(model4, type = "trend", newdata = data_test)


## --------------------------------------------------------------------------------
loo_compare(model3, model4)


## --------------------------------------------------------------------------------
fc_mod3 <- forecast(model3)
fc_mod4 <- forecast(model4)
score_mod3 <- score(fc_mod3, score = "drps")
score_mod4 <- score(fc_mod4, score = "drps")
sum(score_mod4$PP$score, na.rm = TRUE) -
  sum(score_mod3$PP$score, na.rm = TRUE)
