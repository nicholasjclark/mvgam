#### Hypothesis testing for NEON tick abundance forecasts ####
#### Load data and functions ####
library(mvgam)
library(dplyr)
library(ggplot2)
library(viridis)
data("all_neon_tick_data")
source('NEON_manuscript/neon_utility_functions.R')

# Prep data
all_data <- prep_neon_data(species = 'Ixodes_scapularis', split_prop = 0.8)

#### Set hypothesis formulae ####
# NULL. There is no seasonal pattern to be estimated, and we simply let the latent
# factors and site-level effects of growing days influence the series dynamics
null_hyp = y ~ siteID + s(cum_gdd, by = siteID, k = 3) - 1

# 1. Do all series share same seasonal pattern, with any remaining variation due to
# non-seasonal local variation captured by the trends?
hyp1 = y ~
  siteID +
  s(cum_gdd, by = siteID, k = 3) +
  # Global cyclic seasonality term (smooth)
  s(season, k = 12, m = 2, bs = 'cc') - 1

# 2. Is there evidence for global seasonality but each site's seasonal pattern deviates
# based on more local conditions?
hyp2 = y ~
  siteID +
  s(cum_gdd, by = siteID, k = 3) +
  s(season, k = 6, m = 2, bs = 'cc') +
  # Site-level deviations from global pattern, which can be wiggly (m=1 to reduce concurvity);
  # If these dominate, they will have smaller smoothing parameters and the global seasonality
  # will become less important (larger smoothing parameter). Sites with the smallest smooth
  # parameters are those that deviate the most from the global seasonality
  s(season, by = siteID, m = 1, k = 8) - 1

# 3. Is there evidence for global seasonality but each plot's seasonal pattern deviates
# based on even more local conditions than above (i.e. site-level is not as informative)?
# If evidence of gdd effects, can also let use a global smoother and then site-level
# deviations for a 'hierarchical' setup
hyp3 = y ~
  siteID +
  s(cum_gdd, by = siteID, k = 3) +
  s(season, k = 6, m = 2, bs = 'cc') +
  # Series-level deviations from global pattern
  s(season, by = series, m = 1, k = 8) - 1

# Fit each hypothesis
n.burnin = 1000
n.iter = 5000
thin = 5

fit_null <- fit_mvgam(data_train = all_data$data_train,
                  data_test = all_data$data_test,
                  formula = null_hyp,
                  formula_name = 'Null_hyp',
                  family = 'nb',
                  use_lv = F,
                  n.burnin = n.burnin,
                  n.iter = n.iter,
                  thin = thin,
                  auto_update = FALSE,
                  interval_width = 0.9)

fit_hyp1 <- fit_mvgam(data_train = all_data$data_train,
                      data_test = all_data$data_test,
                      formula = hyp1,
                      formula_name = 'Hyp1',
                      family = 'nb',
                      use_lv = F,
                      n.burnin = n.burnin,
                      n.iter = n.iter,
                      thin = thin,
                      auto_update = F,
                      interval_width = 0.9)

fit_hyp2 <- fit_mvgam(data_train = all_data$data_train,
                      data_test = all_data$data_test,
                      formula = hyp2,
                      formula_name = 'Hyp2',
                      family = 'nb',
                      use_lv = F,
                      n.burnin = n.burnin,
                      n.iter = n.iter,
                      thin = thin,
                      auto_update = FALSE,
                      interval_width = 0.9)

fit_hyp3 <- fit_mvgam(data_train = all_data$data_train,
                      data_test = all_data$data_test,
                      formula = hyp3,
                      formula_name = 'Hyp3',
                      family = 'nb',
                      use_lv = F,
                      n.burnin = n.burnin,
                      n.iter = n.iter,
                      thin = thin,
                      auto_update = FALSE,
                      interval_width = 0.9)

# Save models
dir.create('NEON_manuscript/Results', recursive = T, showWarnings = F)
save(fit_null,
     fit_hyp1,
     fit_hyp2,
     fit_hyp3,
     file = 'NEON_manuscript/Results/ixodes_models.rda')

## Post-process to investigate results
load('NEON_manuscript/Results/ixodes_models.rda')

rbind(fit_null$DRPS_scores,
      fit_hyp1$DRPS_scores,
      fit_hyp2$DRPS_scores,
      fit_hyp3$DRPS_scores) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Truth)) %>%
  dplyr::group_by(Series) %>%
  dplyr::mutate(mean_drps = mean(DRPS),
                sd_drps = sd(DRPS)) %>%
  dplyr::mutate(scale_drps = (DRPS - mean_drps) / sd_drps) -> plot_dat

plot_dat %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Series, Horizon, Formula) %>%
  dplyr::summarise(model_drps = mean(DRPS)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Series, Horizon) %>%
  dplyr::mutate(rank_drps = rank(model_drps)) -> plot_dat_ranks

# Set model levels for plotting
plot_dat$Formula <- factor(plot_dat$Formula, levels = rev(c('Null_hyp', 'Null_hyp_mv',
                                                            'Hyp1', 'Hyp1_mv',
                                                            'Hyp2', 'Hyp2_mv',
                                                            'Hyp3', 'Hyp3_mv')))
plot_dat_ranks$Formula <- factor(plot_dat_ranks$Formula, levels = rev(c('Null_hyp', 'Null_hyp_mv',
                                                                        'Hyp1', 'Hyp1_mv',
                                                                        'Hyp2', 'Hyp2_mv',
                                                                        'Hyp3', 'Hyp3_mv')))
# Calculate empirical coverage of 90% prediction intervals
coverages <- plot_dat %>%
  dplyr::group_by(Formula) %>%
  dplyr::summarise(coverage = round(sum(In_90) / length(In_90), 2))

# Performance plots
ggplot(plot_dat,
       aes(x = Formula, y = scale_drps, colour = Formula, fill = Formula)) +
  geom_hline(yintercept = 0, size = 1.2) +
  geom_violin(outlier.shape = NA, size = 1.1,
              draw_quantiles = c(0.5), scale = 'width') +
  geom_jitter(alpha = 0.5, width = 0.3, colour = 'black', size = 0.25) +
  geom_violin(fill = NA, size = 0.6,
              draw_quantiles = c(0.5), scale = 'width', colour = 'black') +
  ylim(-3.75,3.5) +
  scale_colour_viridis(discrete = T, option = 'plasma', begin = 0.35, end = 1) +
  scale_fill_viridis(discrete = T, option = 'plasma', begin = 0.35, end = 1) +
  theme_dark() + theme(legend.position = 'None') + coord_flip() +
  labs(y = 'Discrete rank probability score (scaled)', x = '') +
  geom_text(data = coverages, aes(y = -3.5,
                                  label = sprintf("%0.2f", round(coverage, digits = 2))),
            fontface = 'bold', colour = 'black') +
  theme(panel.background = element_rect(fill = "gray80")) -> plot1

ggplot(plot_dat_ranks,
       aes(x = Formula, y = rank_drps, colour = Formula, fill = Formula)) +
  geom_violin(size = 1.1,
              draw_quantiles = c(0.5), scale = 'width') +
  geom_violin(fill = NA, size = 0.6,
              draw_quantiles = c(0.5), scale = 'width', colour = 'black') +
  geom_jitter(alpha = 0.5, height = 0.1, width = 0.3, colour = 'black', size = 0.25) +
  ylim(0.5,7.5) +
  scale_y_continuous(breaks = seq(1,4)) +
  scale_colour_viridis(discrete = T, option = 'plasma', begin = 0.35, end = 1) +
  scale_fill_viridis(discrete = T, option = 'plasma', begin = 0.35, end = 1, name = '') +
  theme_dark() + theme(legend.position = 'None') + coord_flip() +
  labs(y = 'Performance ranking (lower is better)', x = '') +
  theme(panel.background = element_rect(fill = "gray80"),
        panel.grid.minor = element_blank()) -> plot2

dir.create('NEON_manuscript/Figures', recursive = T, showWarnings = F)
pdf('NEON_manuscript/Figures/Fig4_Ixodes_performances.pdf', width = 6.25, height = 5)
cowplot::plot_grid(plot1, plot2, ncol = 1)
dev.off()

pdf('NEON_manuscript/Figures/FigS3_Ixodes_sitewise_analysis.pdf',
    width = 6.25, height = 4)
ggplot(plot_dat %>% dplyr::filter(Formula %in% c('Null_hyp','Hyp1', 'Hyp2','Hyp3')),
       aes(y = scale_drps,x = Series, fill = Formula))+
  geom_hline(yintercept=0, size = 1.1)+
  geom_violin(scale = 'width', draw_quantiles = 0.5) +
  scale_fill_discrete(type = c(viridis::plasma(n=10, begin = 0.15, end = 1)[c(4,6,8,10)]), name = '')  +
  coord_flip() +theme_dark() +
  guides(fill = guide_legend(reverse = T)) +
  labs(y = 'Discrete rank probability score (scaled)', x = '') +
  theme(panel.background = element_rect(fill = "gray80"),
        panel.grid.minor = element_blank())
dev.off()

# Plot changing trend correlations from latent factor models
plot1 <- ggplot(fit_hyp1$mean_correlations %>%
                  tibble::rownames_to_column("series1") %>%
                  tidyr::pivot_longer(-c(series1), names_to = "series2", values_to = "Correlation"),
                aes(x = series1, y = series2)) + geom_tile(aes(fill = Correlation)) +
  scale_fill_gradient2(low="darkred", mid="white", high="darkblue",
                       midpoint = 0,
                       breaks = seq(-1,1,length.out = 5),
                       limits = c(-1, 1),
                       name = 'Trend\ncorrelation') +
  labs(x = '', y = '', title = '\nGlobal seasonality') +
  theme_dark() +
  theme(axis.text.x = element_blank(),
        title = element_text(size = 9))

plot2 <- ggplot(fit_hyp2$mean_correlations %>%
                  tibble::rownames_to_column("series1") %>%
                  tidyr::pivot_longer(-c(series1), names_to = "series2", values_to = "Correlation"),
                aes(x = series1, y = series2)) + geom_tile(aes(fill = Correlation)) +
  scale_fill_gradient2(low="darkred", mid="white", high="darkblue",
                       midpoint = 0,
                       breaks = seq(-1,1,length.out = 5),
                       limits = c(-1, 1),
                       name = 'Trend\ncorrelation') +
  labs(x = '', y = '', title = 'Global seasonality,\nSite seasonality') +
  theme_dark() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        title = element_text(size = 9))

plot3 <- ggplot(fit_hyp3$mean_correlations %>%
                  tibble::rownames_to_column("series1") %>%
                  tidyr::pivot_longer(-c(series1), names_to = "series2", values_to = "Correlation"),
                aes(x = series1, y = series2)) + geom_tile(aes(fill = Correlation)) +
  scale_fill_gradient2(low="darkred", mid="white", high="darkblue",
                       midpoint = 0,
                       breaks = seq(-1,1,length.out = 5),
                       limits = c(-1, 1),
                       name = 'Trend\ncorrelation') +
  labs(x = '', y = '', title = 'Global seasonality,\nPlot seasonality') +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        title = element_text(size = 9))


pdf('NEON_manuscript/Figures/FigS5_Ixodes_trendcorrelations.pdf',
    width = 6.25, height = 5)
cowplot::plot_grid(plot1 + theme(legend.position = 'none'),
                   plot2 + theme(legend.position = 'none'),
                   plot3 + theme(legend.position = 'none'),
                   cowplot::get_legend(plot1),
                   rel_widths = c(1, 0.85),
                   rel_heights = c(0.8, 1),
                   ncol = 2)
dev.off()

# Plot PIT histograms
data.frame(rbind(fit_null$PIT_scores,
      fit_hyp1$PIT_scores,
      fit_hyp2$PIT_scores,
      fit_hyp3$PIT_scores)) %>%
  dplyr::group_by(Formula) %>%
  dplyr::summarise_all(mean) -> plot_dat

blank <- function(x = 1, y = 1, type = "n", xlab = "", ylab = "",
                  xaxt = "n", yaxt = "n", bty = "n", ...){
  plot(x = x, y = y, type = type, xlab = xlab, ylab = ylab,
       xaxt = xaxt, yaxt = yaxt, bty = bty, ...)
}

formulas <- c('Null_hyp',
              'Hyp1',
              'Hyp2',
              'Hyp3')
colours <- rev(viridis::plasma(n = 4, begin = 0.35, end = 1))

pdf('NEON_manuscript/Figures/FigS4_Ixodes_PITs.pdf',
    width = 6.25, height = 3.75)
par(mfrow = c(1, 4),
    mai = c(.38,.35,.45,.05),
    mgp = c(2, 1, 0))
for(i in seq_along(formulas)){
pit_points <- data.frame(plot_dat %>%
                           dplyr::filter(Formula == formulas[i]) %>%
                           dplyr::ungroup() %>%
                           dplyr::select(-c(1)))

  blank(bty = "L", ylim = c(0, max(pit_points) * 1.1), xlim = c(0.5, 10.5))
  points(x = seq(1, 10), y = pit_points, type = "h", lwd = 4.25, col = colours[i])
  abline(h = mean(as.numeric(pit_points)), lty = 3, lwd = 2)
  axis(1, at = seq(1, 10, 1), labels = FALSE, tck = -0.01)
  axis(1, at = seq(1, 10, 5), labels = FALSE, tck = -0.025)
  axis(1, at = c(1, 10), labels = c(0, 1), lwd = 0, line = -0.5,
       cex.axis = 1.25, xpd = NA)
  axis(2, at = seq(0, 3.5, 0.5), labels = FALSE, tck = -0.01)
  axis(2, at = seq(0, 3.5, 1), labels = FALSE, tck = -0.025)
  if(i == 3){
    mtext(side = 1, "Probability Integral Transform                    ", cex = 1,
          line = 1.75)
  }

    mtext(side = 3, formulas[i], cex = 1,
          line = 1.55)
    if(i %in% c(1, 6)){
      mtext(side = 2, "Frequency", cex = 1, line = 1.45)
    }
}
invisible()
dev.off()

# Plot forecasts from select multivariate models
dir.create('NEON_manuscript/Figures/Ixodes_forecasts', recursive = T, showWarnings = F)
for(i in seq_along(levels(all_data$data_train$series))){
  pdf(paste0('NEON_manuscript/Figures/Ixodes_forecasts/', levels(all_data$data_train$series)[i],
      '.pdf'))
  par(mfrow = c(4,1),
      mai = c(.38,.4,.45,.05),
      mgp = c(2, 1, 0))
  plot_mvgam_fc(object = fit_null$out_gam_mod, data_test = all_data$data_test,
                series = i)
  mtext(side = 3, 'Null', cex = 1,
        line = 1.55)
  plot_mvgam_fc(object = fit_hyp1$out_gam_mod, data_test = all_data$data_test,
                series = i)
  mtext(side = 3, 'Hyp1', cex = 1,
        line = 1.35)
  plot_mvgam_fc(object = fit_hyp2$out_gam_mod, data_test = all_data$data_test,
                series = i)
  mtext(side = 3, 'Hyp2', cex = 1,
        line = 1.55)
  plot_mvgam_fc(object = fit_hyp3$out_gam_mod, data_test = all_data$data_test,
                series = i)
  mtext(side = 3, 'Hyp3', cex = 1,
        line = 1.55)
  invisible()
  dev.off()
}

fit_null$rho_summary
fit_hyp1$rho_summary
fit_hyp2$rho_summary
fit_hyp3$rho_summary
