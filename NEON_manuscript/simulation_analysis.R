#### Simulation analysis ####
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
pdf('NEON_manuscript/Figures/Fig1_simulation_drps_missing_plot.pdf')
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
pdf('NEON_manuscript/Figures/Fig2_simulation_coverage_nseries_plot.pdf')
cowplot::plot_grid(plot1, plot2, ncol = 1)
dev.off()
