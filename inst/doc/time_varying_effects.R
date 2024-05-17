## ----echo = FALSE-----------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  purl = NOT_CRAN,
  eval = NOT_CRAN
)


## ----setup, include=FALSE---------------------------------------------
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


## ---------------------------------------------------------------------
set.seed(1111)
N <- 200
beta_temp <- mvgam:::sim_gp(rnorm(1),
                            alpha_gp = 0.75,
                            rho_gp = 10,
                            h = N) + 0.5


## ----fig.alt = "Simulating time-varying effects in mvgam and R"-------
plot(beta_temp, type = 'l', lwd = 3, 
     bty = 'l', xlab = 'Time', ylab = 'Coefficient',
     col = 'darkred')
box(bty = 'l', lwd = 2)


## ---------------------------------------------------------------------
temp <- rnorm(N, sd = 1)


## ----fig.alt = "Simulating time-varying effects in mvgam and R"-------
out <- rnorm(N, mean = 4 + beta_temp * temp,
             sd = 0.25)
time <- seq_along(temp)
plot(out,  type = 'l', lwd = 3, 
     bty = 'l', xlab = 'Time', ylab = 'Outcome',
     col = 'darkred')
box(bty = 'l', lwd = 2)


## ---------------------------------------------------------------------
data <- data.frame(out, temp, time)
data_train <- data[1:190,]
data_test <- data[191:200,]


## ----include=FALSE----------------------------------------------------
mod <- mvgam(out ~ dynamic(temp, rho = 8, stationary = TRUE, k = 40),
             family = gaussian(),
             data = data_train)


## ----eval=FALSE-------------------------------------------------------
## mod <- mvgam(out ~ dynamic(temp, rho = 8, stationary = TRUE, k = 40),
##              family = gaussian(),
##              data = data_train)


## ---------------------------------------------------------------------
summary(mod, include_betas = FALSE)


## ---------------------------------------------------------------------
plot_mvgam_smooth(mod, smooth = 1, newdata = data)
abline(v = 190, lty = 'dashed', lwd = 2)
lines(beta_temp, lwd = 2.5, col = 'white')
lines(beta_temp, lwd = 2)


## ---------------------------------------------------------------------
range_round = function(x){
  round(range(x, na.rm = TRUE), 2)
}
plot_predictions(mod, 
                 newdata = datagrid(time = unique,
                                    temp = range_round),
                 by = c('time', 'temp', 'temp'),
                 type = 'link')


## ---------------------------------------------------------------------
fc <- forecast(mod, newdata = data_test)
plot(fc)


## ----include=FALSE----------------------------------------------------
mod <- mvgam(out ~ dynamic(temp, k = 40),
             family = gaussian(),
             data = data_train)


## ----eval=FALSE-------------------------------------------------------
## mod <- mvgam(out ~ dynamic(temp, k = 40),
##              family = gaussian(),
##              data = data_train)


## ---------------------------------------------------------------------
summary(mod, include_betas = FALSE)


## ---------------------------------------------------------------------
plot_mvgam_smooth(mod, smooth = 1, newdata = data)
abline(v = 190, lty = 'dashed', lwd = 2)
lines(beta_temp, lwd = 2.5, col = 'white')
lines(beta_temp, lwd = 2)


## ---------------------------------------------------------------------
load(url('https://github.com/atsa-es/MARSS/raw/master/data/SalmonSurvCUI.rda'))
dplyr::glimpse(SalmonSurvCUI)


## ---------------------------------------------------------------------
SalmonSurvCUI %>%
  # create a time variable
  dplyr::mutate(time = dplyr::row_number()) %>%

  # create a series variable
  dplyr::mutate(series = as.factor('salmon')) %>%

  # z-score the covariate CUI.apr
  dplyr::mutate(CUI.apr = as.vector(scale(CUI.apr))) %>%

  # convert logit-transformed survival back to proportional
  dplyr::mutate(survival = plogis(logit.s)) -> model_data


## ---------------------------------------------------------------------
dplyr::glimpse(model_data)


## ---------------------------------------------------------------------
plot_mvgam_series(data = model_data, y = 'survival')


## ----include = FALSE--------------------------------------------------
mod0 <- mvgam(formula = survival ~ 1,
             trend_model = 'RW',
             family = betar(),
             data = model_data)


## ----eval = FALSE-----------------------------------------------------
## mod0 <- mvgam(formula = survival ~ 1,
##               trend_model = RW(),
##               family = betar(),
##               data = model_data)


## ---------------------------------------------------------------------
summary(mod0)


## ---------------------------------------------------------------------
plot(mod0, type = 'trend')


## ----include=FALSE----------------------------------------------------
mod1 <- mvgam(formula = survival ~ 1,
              trend_formula = ~ dynamic(CUI.apr, k = 25, scale = FALSE),
              trend_model = 'RW',
              family = betar(),
              data = model_data,
              adapt_delta = 0.99)


## ----eval=FALSE-------------------------------------------------------
## mod1 <- mvgam(formula = survival ~ 1,
##               trend_formula = ~ dynamic(CUI.apr, k = 25, scale = FALSE),
##               trend_model = 'RW',
##               family = betar(),
##               data = model_data)


## ---------------------------------------------------------------------
summary(mod1, include_betas = FALSE)


## ---------------------------------------------------------------------
plot(mod1, type = 'trend')


## ---------------------------------------------------------------------
plot(mod1, type = 'forecast')


## ---------------------------------------------------------------------
# Extract estimates of the process error 'sigma' for each model
mod0_sigma <- as.data.frame(mod0, variable = 'sigma', regex = TRUE) %>%
  dplyr::mutate(model = 'Mod0')
mod1_sigma <- as.data.frame(mod1, variable = 'sigma', regex = TRUE) %>%
  dplyr::mutate(model = 'Mod1')
sigmas <- rbind(mod0_sigma, mod1_sigma)

# Plot using ggplot2
library(ggplot2)
ggplot(sigmas, aes(y = `sigma[1]`, fill = model)) +
  geom_density(alpha = 0.3, colour = NA) +
  coord_flip()


## ---------------------------------------------------------------------
plot(mod1, type = 'smooth', trend_effects = TRUE)


## ---------------------------------------------------------------------
loo_compare(mod0, mod1)


## ----include=FALSE----------------------------------------------------
lfo_mod0 <- lfo_cv(mod0, min_t = 30)
lfo_mod1 <- lfo_cv(mod1, min_t = 30)


## ----eval=FALSE-------------------------------------------------------
## lfo_mod0 <- lfo_cv(mod0, min_t = 30)
## lfo_mod1 <- lfo_cv(mod1, min_t = 30)


## ---------------------------------------------------------------------
sum(lfo_mod0$elpds)
sum(lfo_mod1$elpds)


## ----fig.alt = "Comparing forecast skill for dynamic beta regression models in mvgam and R"----
plot(x = 1:length(lfo_mod0$elpds) + 30,
     y = lfo_mod0$elpds - lfo_mod1$elpds,
     ylab = 'ELPDmod0 - ELPDmod1',
     xlab = 'Evaluation time point',
     pch = 16,
     col = 'darkred',
     bty = 'l')
abline(h = 0, lty = 'dashed')

