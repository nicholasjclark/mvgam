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
plot_mvgam_smooth(fit_hyp3$out_gam_mod, series = 5, smooth = 'season')
plot_mvgam_smooth(fit_hyp3$out_gam_mod, series = 5, smooth = 'cum_gdd')
plot_mvgam_fc(fit_hyp3$out_gam_mod, series = 5,
                  data_test = all_data$data_test,
              hide_xlabels = TRUE)
axis(1, at = seq(0, dim(fit_hyp3$out_gam_mod$ytimes)[1],
                 b = 52), labels = seq(2015, 2020), cex.axis = 1)
text(x=NROW(all_data$data_train) / NCOL(fit_hyp3$out_gam_mod$ytimes) + 7,
     y=40, labels = 'Forecast horizon', srt = -90)
plot_mvgam_trend(fit_hyp3$out_gam_mod, series = 5,
              data_test = all_data$data_test,
              hide_xlabels = TRUE)
axis(1, at = seq(0, dim(fit_hyp3$out_gam_mod$ytimes)[1],
                 b = 52), labels = seq(2015, 2020), cex.axis = 1)
dev.off()


pdf('NEON_manuscript/Figures/FigS8_ixodes_realisations.pdf', width = 6.25, height = 5.85)
par(mfrow = c(2, 2),
    mgp = c(2.5, 1, 0),
    mai = c(0.6, 0.6, 0.2, 0.2))
plot_mvgam_smooth(fit_hyp3$out_gam_mod, series = 5, smooth = 'season',
                  realisations = TRUE, n_realisations = 20)
plot_mvgam_smooth(fit_hyp3$out_gam_mod, series = 5, smooth = 'cum_gdd',
                  realisations = TRUE, n_realisations = 20)
plot_mvgam_fc(fit_hyp3$out_gam_mod, series = 5,
              data_test = all_data$data_test,
              hide_xlabels = TRUE)
axis(1, at = seq(0, dim(fit_hyp3$out_gam_mod$ytimes)[1],
                 b = 52), labels = seq(2015, 2020), cex.axis = 1)
text(x=NROW(all_data$data_train) / NCOL(fit_hyp3$out_gam_mod$ytimes) + 7,
     y=40, labels = 'Forecast horizon', srt = -90)
plot_mvgam_trend(fit_hyp3$out_gam_mod, series = 5,
                 data_test = all_data$data_test,
                 hide_xlabels = TRUE,
                 realisations = TRUE, n_realisations = 8)
axis(1, at = seq(0, dim(fit_hyp3$out_gam_mod$ytimes)[1],
                 b = 52), labels = seq(2015, 2020), cex.axis = 1)
dev.off()


pdf('NEON_manuscript/Figures/FigS7_ixodes_ppcs.pdf', width = 6.25, height = 5.85)
par(mfrow = c(2, 2),
    mgp = c(2.5, 1, 0),
    mai = c(0.6, 0.6, 0.2, 0.2))
ppc(fit_hyp3$out_gam_mod, series = 5, type = 'cdf')
mtext('Training period (retrodictive) checks', side=3, line=0.2, at=19)
ppc(fit_hyp3$out_gam_mod, series = 5, type = 'mean')

ppc(fit_hyp3$out_gam_mod, series = 5, type = 'cdf',
               data_test = all_data$data_test)
mtext('Testing period (predictive) checks', side=3, line=0.2, at=11)
ppc(fit_hyp3$out_gam_mod, series = 5, type = 'mean',
               data_test = all_data$data_test)
dev.off()

# Plot some of the varying seasonal shapes from the Amblyomma model
plot_seasonality = function(i){

  newdata = expand.grid(series = levels(fit_hyp2$out_gam_mod$obs_data$series)[i],
                        season = seq(1, 52, length.out = 500),
                        cum_gdd = 0,
                        siteID = unique(fit_hyp3$out_gam_mod$obs_data$siteID[which(
                          as.numeric(fit_hyp3$out_gam_mod$obs_data$series) == i
                        )]))
  preds <- predict(fit_hyp3$out_gam_mod, newdata = newdata)

  probs = c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)
  cred <- sapply(1:NCOL(preds),
                 function(n) quantile(preds[,n],
                                      probs = probs))

  c_light <- c("#DCBCBC")
  c_light_highlight <- c("#C79999")
  c_mid <- c("#B97C7C")
  c_mid_highlight <- c("#A25050")
  c_dark <- c("#8F2727")
  c_dark_highlight <- c("#7C0000")

  pred_vals <- as.vector(as.matrix(newdata[['season']]))
  ylim <- c(min(cred)-2, max(cred) + 2)
  plot(1, type = "n", bty = 'L',
       xlab = 'season',
       ylab = 'Partial effect',
       xlim = c(min(pred_vals), max(pred_vals)),
       ylim = ylim)
  title(paste0('s(season) for ', levels(fit_hyp2$out_gam_mod$obs_data$series)[i]),
        adj = 0)
  polygon(c(pred_vals, rev(pred_vals)), c(cred[1,], rev(cred[9,])),
          col = c_light, border = NA)
  polygon(c(pred_vals, rev(pred_vals)), c(cred[2,], rev(cred[8,])),
          col = c_light_highlight, border = NA)
  polygon(c(pred_vals, rev(pred_vals)), c(cred[3,], rev(cred[7,])),
          col = c_mid, border = NA)
  polygon(c(pred_vals, rev(pred_vals)), c(cred[4,], rev(cred[6,])),
          col = c_mid_highlight, border = NA)
  lines(pred_vals, cred[5,], col = c_dark, lwd = 2.5)
  box(bty = 'L', lwd = 2)
}

load('NEON_manuscript/Results/amb_models.rda')
all_data <- prep_neon_data(species = 'Ambloyomma_americanum', split_prop = 0.8)
pdf('NEON_manuscript/Figures/Fig6_amb_seasonalities.pdf', width = 6.25, height = 5.85)
par(mfrow = c(2, 2),
    mgp = c(2.5, 1, 0),
    mai = c(0.6, 0.6, 0.2, 0.2))
plot_seasonality(1)
plot_seasonality(7)
plot_seasonality(12)
plot_seasonality(16)
dev.off()

# Plot some of the different uncertainty decompositions for Amblyomma model
pdf('NEON_manuscript/Figures/Fig7_amb_uncertainties.pdf', width = 6.25, height = 5.85)
par(mfrow = c(2, 2),
    mgp = c(2.5, 1, 0),
    mai = c(0.6, 0.6, 0.2, 0.2))
for(i in c(1, 7, 9, 16)){
  plot_mvgam_uncertainty(object = fit_hyp2$out_gam_mod, series = i,
                          data_test = all_data$data_test,
                         legend_position = 'bottomleft')

}
dev.off()

# Plot the random effect posterior distributions
pdf('NEON_manuscript/Figures/FigS9_amb_res.pdf', width = 6.25, height = 5)
plot(fit_hyp3$out_gam_mod, type = 're')
dev.off()
