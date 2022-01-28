#### NEON analysis / figures script ####
load('NEON_manuscript/Results/ixodes_models.rda')

# Hypothesis 2 was the most well-supported of the tested models
# Plot some results for particular series
source('NEON_manuscript/neon_utility_functions.R')
library(mvgam)

# Prep Ixodes data
all_data <- prep_neon_data(species = 'Ixodes_scapularis', split_prop = 0.8)

# Make plots for manuscript
pdf('NEON_manuscript/Figures/Fig5_ixodes_example.pdf', width = 6.25, height = 5.85)
par(mfrow = c(2, 2),
    mgp = c(2.5, 1, 0),
    mai = c(0.6, 0.6, 0.2, 0.2))
plot_mvgam_smooth(fit_hyp1$out_gam_mod, series = 4, smooth = 'season')
plot_mvgam_smooth(fit_hyp1$out_gam_mod, series = 4, smooth = 'cum_gdd')
plot_mvgam_fc(fit_hyp1$out_gam_mod, series = 4,
                  data_test = all_data$data_test,
              hide_xlabels = TRUE)
axis(1, at = seq(0, dim(fit_hyp3$out_gam_mod$ytimes)[1],
                 b = 26), labels = seq(2015, 2020), cex.axis = 1)
text(x=NROW(all_data$data_train) / NCOL(fit_hyp2$out_gam_mod$ytimes) + 6.5,
     y=45, labels = 'Forecast horizon', srt = -90)
plot_mvgam_trend(fit_hyp1$out_gam_mod, series = 4,
              data_test = all_data$data_test,
              hide_xlabels = TRUE)
axis(1, at = seq(0, dim(fit_hyp3$out_gam_mod$ytimes)[1],
                 b = 26), labels = seq(2015, 2020), cex.axis = 1)
dev.off()

pdf('NEON_manuscript/Figures/FigS6_ixodes_ppcs.pdf', width = 6.25, height = 5.85)
par(mfrow = c(2, 2),
    mgp = c(2.5, 1, 0),
    mai = c(0.6, 0.6, 0.2, 0.2))
plot_mvgam_ppc(fit_hyp1$out_gam_mod, series = 3, type = 'cdf')
mtext('Training period (retrodictive) checks', side=3, line=0.2, at=19)
plot_mvgam_ppc(fit_hyp1$out_gam_mod, series = 3, type = 'mean')

plot_mvgam_ppc(fit_hyp1$out_gam_mod, series = 3, type = 'cdf',
               data_test = all_data$data_test)
mtext('Testing period (predictive) checks', side=3, line=0.2, at=7)
plot_mvgam_ppc(fit_hyp1$out_gam_mod, series = 3, type = 'mean',
               data_test = all_data$data_test)
dev.off()

# Plot some of the varying seasonal shapes from the Amblyomma model
load('NEON_manuscript/Results/amb_models.rda')
all_data <- prep_neon_data(species = 'Ambloyomma_americanum', split_prop = 0.8)
pdf('NEON_manuscript/Figures/Fig6_amb_seasonalities.pdf', width = 6.25, height = 5.85)
par(mfrow = c(2, 2),
    mgp = c(2.5, 1, 0),
    mai = c(0.6, 0.6, 0.2, 0.2))
for(i in c(1, 7, 13, 16)){

 plot_mvgam_smooth(fit_hyp3$out_gam_mod, series = i,
                      smooth = 'season',
                   newdata = expand.grid(series = levels(fit_hyp3$out_gam_mod$obs_data$series)[i],
                                         season = seq(1, 26, length.out = 100),
                                         cum_gdd = 0,
                                         siteID = unique(fit_hyp3$out_gam_mod$obs_data$siteID[which(
                                           as.numeric(fit_hyp3$out_gam_mod$obs_data$series) == i
                                         )])))

}
dev.off()

# Plot some of the different uncertainty decompositions for Amblyomma model
pdf('NEON_manuscript/Figures/Fig7_amb_uncertainties.pdf', width = 6.25, height = 5.85)
par(mfrow = c(2, 2),
    mgp = c(2.5, 1, 0),
    mai = c(0.6, 0.6, 0.2, 0.2))
for(i in c(1, 7, 13, 16)){
  plot_mvgam_uncertainty(fit_hyp3$out_gam_mod, series = i,
                          data_test = all_data$data_test,
                         legend_position = 'bottomleft')

}
dev.off()
