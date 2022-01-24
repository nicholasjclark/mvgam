library(mvgam)

## Use null hypothesis that a latent variable evolves as a white noise to extract
## summaries on the number of retained latent variables; also can return trend plots
## as in the plot_mvgam_trend(), which is useful
dat <- sim_mvgam(T = 80, n_series = 6, n_trends = 2,
                 trend_rel = 0.6, train_prop = 0.8)
mod1 <- mvjagam(data_train = dat$data_train,
                data_test = dat$data_test,
                formula = y ~ s(series, bs = 're') +
                  s(season, bs = c('cc'), k = 12),
                knots = list(season = c(0.5, 12.5)),
                use_lv = T,
                n_lv = 2,
                family = 'nb',
                trend_model = 'None',
                n.burnin = 1000,
                n.iter = 1000,
                thin = 1,
                auto_update = F)
plot_mvgam_fc(mod1, series = 1, data_test = dat$data_test)
plot_mvgam_fc(mod1, series = 2, data_test = dat$data_test)
plot_mvgam_fc(mod1, series = 3, data_test = dat$data_test)
plot_mvgam_fc(mod1, series = 4, data_test = dat$data_test)
plot_mvgam_fc(mod1, series = 5, data_test = dat$data_test)

plot_mvgam_factors(mod1)

mod2 <- mvjagam(data_train = dat$data_train,
                data_test = dat$data_test,
                formula = y ~ s(series, bs = 're') +
                  s(season, bs = c('cc'), k = 12),
                knots = list(season = c(0.5, 12.5)),
                use_lv = T,
                n_lv = 6,
                family = 'nb',
                trend_model = 'None',
                n.burnin = 1000,
                n.iter = 1000,
                thin = 1,
                auto_update = F)
plot_mvgam_fc(mod2, series = 1, data_test = dat$data_test)
plot_mvgam_fc(mod2, series = 2, data_test = dat$data_test)
plot_mvgam_fc(mod2, series = 3, data_test = dat$data_test)
plot_mvgam_fc(mod2, series = 4, data_test = dat$data_test)
plot_mvgam_fc(mod2, series = 5, data_test = dat$data_test)
plot_mvgam_factors(mod2)

lvs <- MCMCvis::MCMCchains(mod2$jags_output, 'LV')
MCMCvis::MCMCtrace(mod2$jags_output, 'lv_coefs', pdf = F)
lv1 <- lvs[,1:80][,1:(NROW(mod1$obs_data) / NCOL(mod1$ytimes))]
lv_dropped(lv1)
plot(lv1[1,], type = 'l')
for(i in 2:100){
  lines(lv1[i,])
}
lv2 <- lvs[,81:160][,1:(NROW(mod1$obs_data) / NCOL(mod1$ytimes))]
lv_dropped(lv2)
plot(lv2[1,], type = 'l')
for(i in 2:100){
  lines(lv2[i,])
}
lv3 <- lvs[,161:240][,1:(NROW(mod1$obs_data) / NCOL(mod1$ytimes))]
lv_dropped(lv3)
plot(lv3[1,], type = 'l')
for(i in 2:100){
  lines(lv3[i,])
}
lv4 <- lvs[,241:320][,1:(NROW(mod1$obs_data) / NCOL(mod1$ytimes))]
lv_dropped(lv4)
plot(lv4[1,], type = 'l')
for(i in 2:100){
  lines(lv4[i,])
}
lv5 <- lvs[,321:400][,1:(NROW(mod1$obs_data) / NCOL(mod1$ytimes))]
lv_dropped(lv5)
plot(lv5[1,], type = 'l')
for(i in 2:100){
  lines(lv5[i,])
}
lv6 <- lvs[,401:480][,1:(NROW(mod1$obs_data) / NCOL(mod1$ytimes))]
lv_dropped(lv6)
plot(lv6[1,], type = 'l', ylim = c(-2, 2))
for(i in 2:100){
  lines(lv6[i,])
}
