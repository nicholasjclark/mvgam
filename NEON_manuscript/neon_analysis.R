#### NEON analysis / figures script ####
load('NEON_manuscript/Results/ixodes_models.rda')

# Hypothesis 3 was the most well-supported of the tested models
# Plot some results for particular series
source('NEON_manuscript/neon_utility_functions.R')
library(mvgam)

# Prep Ixodes data
all_data <- prep_neon_data(species = 'Ixodes_scapularis', split_prop = 0.8)

# Make plots for manuscript
pdf('NEON_manuscript/Figures/Fig4_ixodes_example.pdf', width = 6.25, height = 5.85)
par(mfrow = c(2, 2),
    mgp = c(2.5, 1, 0),
    mai = c(0.6, 0.6, 0.2, 0.2))
plot_mvgam_season(fit_hyp3$out_gam_mod, series = 1,
                  data_test = all_data$data_test,
                  data_train = all_data$data_train, xlab = 'Epidemiological week')
plot_mvgam_gdd(fit_hyp3$out_gam_mod, series = 1,
               data_test = all_data$data_test,
               data_train = all_data$data_train,
               mean_gdd = all_data$mean_gdd,
               sd_gdd = all_data$sd_gdd)
plot_mvgam_fc(fit_hyp3$out_gam_mod, series = 1,
                  data_test = all_data$data_test,
                  data_train = all_data$data_train, hide_xlabels = TRUE)
axis(1, at = seq(0, dim(fit_hyp3$out_gam_mod$ytimes)[1],
                 b = 26), labels = seq(2015, 2020), cex.axis = 1)
text(x=NROW(all_data$data_train) / NCOL(fit_hyp3$out_gam_mod$ytimes) + 8,
     y=75, labels = 'Forecast horizon', srt = -90)
plot_mvgam_trend(fit_hyp3$out_gam_mod, series = 1,
              data_test = all_data$data_test,
              data_train = all_data$data_train,
              hide_xlabels = TRUE)
axis(1, at = seq(0, dim(fit_hyp3$out_gam_mod$ytimes)[1],
                 b = 26), labels = seq(2015, 2020), cex.axis = 1)
text(x = 48.5, y = -30, labels = 'Trend component = 0.28\n(0.11 - 0.52)')
dev.off()

# Plot some of the varying seasonal shapes from the Amblyomma model
load('NEON_manuscript/Results/amb_models.rda')
all_data <- prep_neon_data(species = 'Ambloyomma_americanum', split_prop = 0.8)
pdf('NEON_manuscript/Figures/Fig5_amb_seasonalities.pdf', width = 6.25, height = 5.85)
par(mfrow = c(2, 2),
    mgp = c(2.5, 1, 0),
    mai = c(0.6, 0.6, 0.2, 0.2))
for(i in c(1, 6, 9, 15)){
  if(i %in% c(13, 15)){
    plot_mvgam_season(fit_hyp3$out_gam_mod, series = i,
                      data_test = all_data$data_test,
                      data_train = all_data$data_train, xlab = 'Epidemiological week')
  } else {
    plot_mvgam_season(fit_hyp3$out_gam_mod, series = i,
                      data_test = all_data$data_test,
                      data_train = all_data$data_train, xlab = '')
  }

}
dev.off()

# Plot some of the different uncertainty decompositions for Amblyomma model
pdf('NEON_manuscript/Figures/Fig6_amb_uncertainties.pdf', width = 6.25, height = 5.85)
par(mfrow = c(2, 2),
    mgp = c(2.5, 1, 0),
    mai = c(0.6, 0.6, 0.2, 0.2))
for(i in c(4, 6, 9, 15)){
  plot_mvgam_uncertainty(fit_hyp3$out_gam_mod, series = i,
                          data_test = all_data$data_test,
                          data_train = all_data$data_train,
                         legend_position = 'bottomleft')

}
dev.off()
