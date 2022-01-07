#### Simulation analysis ####
# Simulate a series with a nonlinear trend to visualise how a spline extrapolates
library(mvgam)
library(xts)
library(forecast)
data("AirPassengers")
set.seed(1e8)
dat <- floor(AirPassengers + cumsum(rnorm(length(AirPassengers),
                                          sd = 35)))
dat <- dat + abs(min(dat))
series <- ts(dat, start = c(1949, 1), frequency = 12)
fake_data <- series_to_mvgam(series, freq = 365, train_prop = 0.74)

# Fit a GAM to the data using mgcv; use a wiggly Gaussian Process smooth for the
# trend and a cyclic smooth for the seasonality
gam_mod <- gam(y ~ s(year, k = 9, bs = 'tp') +
                 s(season, bs = 'cc', k = 10) +
                 ti(season, year),
               data = fake_data$data_train,
               family = nb(),
               method = 'REML')

# Calculate predictions for the mgcv model to inspect extrapolation behaviour
Xp <- predict(gam_mod, newdata = rbind(fake_data$data_train,
                                       fake_data$data_test), type = 'lpmatrix')
vc <- vcov(gam_mod)
sim <- MASS::mvrnorm(1000,
                     mu = coef(gam_mod), Sigma = vc)
dims_needed <- dim(exp(Xp %*% t(sim)))
fits <- rnbinom(n = prod(dims_needed), mu = as.vector(exp(Xp %*% t(sim))),
                size = gam_mod$family$getTheta(TRUE))
fits <- t(matrix(fits, nrow = dims_needed[1], ncol = dims_needed[2]))

# Plot the smooth function and forecast 95% and 68% HPD intervals
pdf('NEON_manuscript/Figures/Fig1_extrapolation_example.pdf', width = 6.25, height = 5.85)
par(mfrow = c(2, 2),
    mgp = c(2.5, 1, 0),
    mai = c(0.6, 0.6, 0.2, 0.2))
plot(gam_mod, select = 1, shade = T, main = '2nd derivative penalty')
cred_ints <- apply(fits, 2, function(x) hpd(x, 0.95))
ylims <- c(0,
           max(cred_ints) + 2)
plot(cred_ints[3,] ~ seq(1:NCOL(cred_ints)), type = 'l',
     col = rgb(1,0,0, alpha = 0),
     ylim = ylims,
     ylab = 'Predicted counts',
     xlab = 'Time')
polygon(c(seq(1:(NCOL(cred_ints))), rev(seq(1:NCOL(cred_ints)))),
        c(cred_ints[1,],rev(cred_ints[3,])),
        col = rgb(150, 0, 0, max = 255, alpha = 100), border = NA)
cred_ints <- apply(fits, 2, function(x) hpd(x, 0.68))
polygon(c(seq(1:(NCOL(cred_ints))), rev(seq(1:NCOL(cred_ints)))),
        c(cred_ints[1,],rev(cred_ints[3,])),
        col = rgb(150, 0, 0, max = 255, alpha = 180), border = NA)
lines(cred_ints[2,], col = rgb(150, 0, 0, max = 255), lwd = 2, lty = 'dashed')
lines(as.vector(series))
abline(v = NROW(fake_data$data_train),
       lty = 'dashed')
text(x=NROW(fake_data$data_train) + 5,
     y=3000, labels = 'Forecast horizon', srt = -90)

# Repeat by shifting the training window slightly so see how the extrapolation of the
# mgcv model is simply reacting to autocorrelation
fake_data <- series_to_mvgam(series, freq = 365, train_prop = 0.74)
gam_mod <- gam(y ~ s(year, k = 9, bs = 'tp', m = 1) +
                 s(season, bs = 'cc', k = 10) +
                 ti(season, year),
               data = fake_data$data_train,
               family = nb(),
               method = 'REML')
plot(gam_mod, select = 1, shade = T, main = '1st derivative penalty')
Xp <- predict(gam_mod, newdata = rbind(fake_data$data_train,
                                       fake_data$data_test), type = 'lpmatrix')
vc <- vcov(gam_mod)
sim <- MASS::mvrnorm(1000,
                     mu = coef(gam_mod), Sigma = vc)
dims_needed <- dim(exp(Xp %*% t(sim)))
fits <- rnbinom(n = prod(dims_needed), mu = as.vector(exp(Xp %*% t(sim))),
                size = gam_mod$family$getTheta(TRUE))
fits <- t(matrix(fits, nrow = dims_needed[1], ncol = dims_needed[2]))
cred_ints <- apply(fits, 2, function(x) hpd(x, 0.95))
ylims <- c(0,
           max(cred_ints) + 2)
plot(cred_ints[3,] ~ seq(1:NCOL(cred_ints)), type = 'l',
     col = rgb(1,0,0, alpha = 0),
     ylim = ylims,
     ylab = 'Predicted counts',
     xlab = 'Time')
polygon(c(seq(1:(NCOL(cred_ints))), rev(seq(1:NCOL(cred_ints)))),
        c(cred_ints[1,],rev(cred_ints[3,])),
        col = rgb(150, 0, 0, max = 255, alpha = 100), border = NA)
cred_ints <- apply(fits, 2, function(x) hpd(x, 0.68))
polygon(c(seq(1:(NCOL(cred_ints))), rev(seq(1:NCOL(cred_ints)))),
        c(cred_ints[1,],rev(cred_ints[3,])),
        col = rgb(150, 0, 0, max = 255, alpha = 180), border = NA)
lines(cred_ints[2,], col = rgb(150, 0, 0, max = 255), lwd = 2, lty = 'dashed')
lines(as.vector(series))
abline(v = NROW(fake_data$data_train),
       lty = 'dashed')
dev.off()

#### Analysis plots from the simulation component ####
load('NEON_manuscript/Results/sim_results.rda')
run_parameters <- expand.grid(n_series = c(4, 12),
                              T = c(72),
                              prop_missing = c(0, 0.1, 0.5),
                              trend_rel = c(0.3, 0.7),
                              stringsAsFactors = F)
drps_plot_dat <- do.call(rbind, purrr::map(sim_results, 'model_drps'))
coverage_plot_dat <- do.call(rbind, purrr::map(sim_results, 'model_coverages'))
effic_plot_dat <- do.call(rbind, purrr::map(sim_results, 'model_efficiencies'))
corr_plot_dat <- do.call(rbind, purrr::map(sim_results, 'model_correlations'))

trend_rel <- vector()
prop_missing <- vector()
T <- vector()
n_series <- vector()
for(i in 1:nrow(run_parameters)){
  trend_rel <- c(trend_rel, rep(run_parameters$trend_rel[i], 3))
  prop_missing <- c(prop_missing, rep(run_parameters$prop_missing[i], 3))
  T <- c(T, rep(run_parameters$T[i], 3))
  n_series <- c(n_series , rep(run_parameters$n_series[i], 3))
}
drps_plot_dat$trend_rel <- trend_rel
drps_plot_dat$T <- T
drps_plot_dat$prop_missing <- prop_missing
drps_plot_dat$n_series <- n_series
coverage_plot_dat$trend_rel <- trend_rel
coverage_plot_dat$T <- T
coverage_plot_dat$prop_missing <- prop_missing
coverage_plot_dat$n_series <- n_series

library(dplyr)
drps_plot_dat %>%
  dplyr::mutate(model = dplyr::case_when(
    model == 'null' ~ 'Nonseasonal GAMDF',
    model == 'hierarchical' ~ 'Seasonal GAMDF',
    model == 'mgcv_hierarchical' ~ 'Seasonal GAM'
  )) -> drps_plot_dat
drps_plot_dat$model <- factor(drps_plot_dat$model,
                              levels = c('Nonseasonal GAMDF',
                                         'Seasonal GAMDF',
                                         'Seasonal GAM'))

coverage_plot_dat %>%
  dplyr::mutate(model = dplyr::case_when(
    model == 'null' ~ 'Nonseasonal GAMDF',
    model == 'hierarchical' ~ 'Seasonal GAMDF',
    model == 'mgcv_hierarchical' ~ 'Seasonal GAM'
  )) -> coverage_plot_dat
coverage_plot_dat$model <- factor(coverage_plot_dat$model,
                              levels = c('Nonseasonal GAMDF',
                                         'Seasonal GAMDF',
                                         'Seasonal GAM'))

library(ggplot2)
library(viridis)
prop_names <- c(
  `0` = 'None missing',
  `0.1` = "10% missing",
  `0.5` = "50% missing"
)
n_names <- c(
  `4` = "4 series",
  `12` = "12 series"
)

ggplot(drps_plot_dat %>%
         dplyr::filter(trend_rel == 0.3),
       aes(y = as.numeric(drps_med) * as.numeric(drps_upper), x = model, fill = model)) +
  geom_boxplot() +
  facet_wrap(~prop_missing, labeller = as_labeller(prop_names), scales = 'free_x') +
  scale_fill_viridis(discrete = T, begin = 0.2, end = 1, guide = FALSE) +
  theme_bw() + coord_flip() + labs(x = '', y = 'Normalised DRPS calibration (lower is better)',
                                   title = 'Moderate trend') -> plot1

ggplot(drps_plot_dat %>%
         dplyr::filter(trend_rel == 0.7),
       aes(y = as.numeric(drps_med) * as.numeric(drps_upper), x = model, fill = model)) +
  geom_boxplot() +
  facet_wrap(~prop_missing, labeller = as_labeller(prop_names), scales = 'free_x') +
  scale_fill_viridis(discrete = T, begin = 0.2, end = 1, guide = FALSE) +
  theme_bw() + coord_flip() + labs(x = '', y = 'Normalised DRPS calibration (lower is better)',
                                   title = 'Strong trend') -> plot2
pdf('NEON_manuscript/Figures/Fig2_simulation_drps_missing_plot.pdf')
cowplot::plot_grid(plot1, plot2, ncol = 1)
dev.off()


ggplot(drps_plot_dat %>%
         dplyr::filter(trend_rel == 0.3),
       aes(y = as.numeric(drps_med) * as.numeric(drps_upper), x = model, fill = model)) +
  geom_boxplot() +
  facet_wrap(~n_series, labeller = as_labeller(n_names), scales = 'free_x') +
  scale_fill_viridis(discrete = T, begin = 0.2, end = 1, guide = FALSE) +
  theme_bw() + coord_flip() + labs(x = '', y = 'Normalised DRPS calibration (lower is better)',
                                   title = 'Moderate trend') -> plot1

ggplot(drps_plot_dat %>%
         dplyr::filter(trend_rel == 0.7),
       aes(y = as.numeric(drps_med) * as.numeric(drps_upper), x = model, fill = model)) +
  geom_boxplot() +
  facet_wrap(~n_series, labeller = as_labeller(n_names), scales = 'free_x') +
  scale_fill_viridis(discrete = T, begin = 0.2, end = 1, guide = FALSE) +
  theme_bw() + coord_flip() + labs(x = '', y = 'Normalised DRPS calibration (lower is better)',
                                   title = 'Strong trend') -> plot2
pdf('NEON_manuscript/Figures/FigS1_simulation_drps_nseries_plot.pdf')
cowplot::plot_grid(plot1, plot2, ncol = 1)
dev.off()

ggplot(coverage_plot_dat %>%
         dplyr::filter(trend_rel == 0.3),
       aes(y = as.numeric(coverage), x = model, fill = model)) +
  geom_hline(yintercept = 0.9) +
  geom_boxplot() +
  facet_wrap(~prop_missing, labeller = as_labeller(prop_names)) +
  scale_fill_viridis(discrete = T, begin = 0.2, end = 1, guide = FALSE) +
  ylim(0, 1) +
  theme_bw() + coord_flip() + labs(x = '', y = '90% interval coverage',
                                   title = 'Moderate trend') -> plot1

ggplot(coverage_plot_dat %>%
         dplyr::filter(trend_rel == 0.7),
       aes(y = as.numeric(coverage), x = model, fill = model)) +
  geom_hline(yintercept = 0.9) +
  geom_boxplot() +
  facet_wrap(~prop_missing, labeller = as_labeller(prop_names), scales = 'free_x') +
  scale_fill_viridis(discrete = T, begin = 0.2, end = 1, guide = FALSE) +
  ylim(0, 1) +
  theme_bw() + coord_flip() + labs(x = '', y = '90% interval coverage',
                                   title = 'Strong trend') -> plot2
pdf('NEON_manuscript/Figures/FigS2_simulation_coverage_missing_plot.pdf')
cowplot::plot_grid(plot1, plot2, ncol = 1)
dev.off()


ggplot(coverage_plot_dat %>%
         dplyr::filter(trend_rel == 0.3),
       aes(y = as.numeric(coverage), x = model, fill = model)) +
  geom_hline(yintercept = 0.9) +
  geom_boxplot() +
  facet_wrap(~n_series, labeller = as_labeller(n_names), scales = 'free_x') +
  scale_fill_viridis(discrete = T, begin = 0.2, end = 1, guide = FALSE) +
  ylim(0, 1) +
  theme_bw() + coord_flip() + labs(x = '', y = '90% interval coverage',
                                   title = 'Moderate trend') -> plot1

ggplot(coverage_plot_dat %>%
         dplyr::filter(trend_rel == 0.7),
       aes(y = as.numeric(coverage), x = model, fill = model)) +
  geom_hline(yintercept = 0.9) +
  geom_boxplot() +
  facet_wrap(~n_series, labeller = as_labeller(n_names), scales = 'free_x') +
  scale_fill_viridis(discrete = T, begin = 0.2, end = 1, guide = FALSE) +
  ylim(0, 1) +
  theme_bw() + coord_flip() + labs(x = '', y = '90% interval coverage',
                                   title = 'Strong trend') -> plot2
pdf('NEON_manuscript/Figures/Fig3_simulation_coverage_nseries_plot.pdf')
cowplot::plot_grid(plot1, plot2, ncol = 1)
dev.off()
