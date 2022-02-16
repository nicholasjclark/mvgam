#### Comparing particle filter fits of independent and dynamic factor models ####
#### Load data and functions ####
library(mvgam)
library(dplyr)
library(ggplot2)
library(viridis)
data("all_neon_tick_data")
source('NEON_manuscript/neon_utility_functions.R')

# Discrete Rank Probability Score and coverage of 90% interval
drps_score <- function(truth, fc, interval_width = 0.9){
  nsum <- 1000
  Fy = ecdf(fc)
  ysum <- 0:nsum
  indicator <- ifelse(ysum - truth >= 0, 1, 0)
  score <- sum((indicator - Fy(ysum))^2)

  # Is value within 90% HPD?
  interval <- hpd(fc, interval_width)
  in_interval <- ifelse(truth <= interval[3] & truth >= interval[1], 1, 0)
  return(c(score, in_interval))
}

# Wrapper to operate on all observations in fc_horizon
drps_mcmc_object <- function(truth, fc, interval_width = 0.9){
  indices_keep <- which(!is.na(truth))
  if(length(indices_keep) == 0){
    scores = data.frame('drps' = rep(NA, length(truth)),
                        'interval' = rep(NA, length(truth)))
  } else {
    scores <- matrix(NA, nrow = length(truth), ncol = 2)
    for(i in indices_keep){
      scores[i,] <- drps_score(truth = as.vector(truth)[i],
                               fc = fc[,i], interval_width)
    }
  }
  scores
}

# Prep data
all_data <- prep_neon_data(species = 'Ambloyomma_americanum', split_prop = 0.8)

# Hypothesis 1 was the best fitting model
hyp1 = y ~
  s(siteID, bs = 're') +
  s(cum_gdd, siteID, k = 3, bs = 'fs') +
  # Global cyclic seasonality term (smooth)
  s(season, k = 26, m = 2, bs = 'cc')

# Fit independent trends model
mod_ind <- mvjagam(data_train = all_data$data_train,
                   data_test = all_data$data_test,
                   formula = hyp1,
                   knots = list(season = c(0.5, 26.5)),
                   family = 'nb',
                   trend_model = 'AR1',
                   chains = 4,
                   use_lv = F,
                   burnin = 1000)

# Total (summed) out of sample DRPS from the fitted independent trend model,
# excluding the next 10 observations (which will be assimilated)
object <- mod_ind
length_train <- object$obs_data %>%
  dplyr::select(season, year) %>%
  dplyr::distinct() %>% nrow()
ends <- seq(0, dim(MCMCvis::MCMCchains(object$jags_output, 'ypred'))[2],
            length.out = NCOL(object$ytimes) + 1)
starts <- ends + 1
starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
ends <- ends[-1]

ind_drps <- unlist(lapply(seq_len(length(unique(all_data$data_train$series))), function(i){
  preds <- MCMCvis::MCMCchains(object$jags_output, 'ypred')[,starts[i]:ends[i]][,-c(1:length_train)]
  sum(drps_mcmc_object(truth = all_data$data_test[-c(1:(length(unique(all_data$data_train$series))*10)),] %>%
                         dplyr::filter(series == levels(all_data$data_train$series)[i]) %>%
                         dplyr::pull(y),
                       fc = preds[,-c(1:10)])[,1], na.rm = T)
}))
names(ind_drps) <- levels(all_data$data_test$series)

# Initiate particle filter
pfilter_mvgam_init(object = mod_ind, n_particles = 25000, n_cores = 4,
                   data_assim = all_data$data_test)

# Assimilate next 9 observations per series
data_assim <- all_data$data_test[1:(length(unique(all_data$data_train$series))*10),]
pfilter_mvgam_online(data_assim = data_assim,
                     n_cores = 4,
                     kernel_lambda = 1)
# Forecast remaining observations
fc <- pfilter_mvgam_fc(file_path = 'pfilter', n_cores = 4,
                       data_test = all_data$data_test, return_forecasts = T)


# Total (summed) out of sample DRPS from the particle filtered independent trend model
ind_drps_pf <- unlist(lapply(seq_len(length(unique(all_data$data_train$series))), function(i){
  sum(drps_mcmc_object(truth = all_data$data_test[-c(1:(length(unique(all_data$data_train$series))*10)),] %>%
                         dplyr::filter(series == levels(all_data$data_train$series)[i]) %>%
                         dplyr::pull(y),
                       fc = fc$forecasts[[i]])[,1], na.rm = T)
}))
names(ind_drps_pf) <- levels(all_data$data_test$series)

# How would a model that is trained on these next 8 observations, rather than assimilating them
# via a particle filter, perform?
new_data_train <- rbind(all_data$data_train,
                        all_data$data_test[c(1:(length(unique(all_data$data_train$series))*10)),])
new_data_test <- all_data$data_test[-c(1:(length(unique(all_data$data_train$series))*10)),]
mod_ind_updated <- mvjagam(data_train = new_data_train,
                   data_test = new_data_test,
                   formula = hyp1,
                   knots = list(season = c(0.5, 26.5)),
                   family = 'nb',
                   trend_model = 'AR1',
                   chains = 4,
                   use_lv = F,
                   burnin = 10000)

object <- mod_ind_updated
length_train <- object$obs_data %>%
  dplyr::select(season, year) %>%
  dplyr::distinct() %>% nrow()
ends <- seq(0, dim(MCMCvis::MCMCchains(object$jags_output, 'ypred'))[2],
            length.out = NCOL(object$ytimes) + 1)
starts <- ends + 1
starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
ends <- ends[-1]

ind_drps_updated <- unlist(lapply(seq_len(length(unique(all_data$data_train$series))), function(i){
  preds <- MCMCvis::MCMCchains(object$jags_output, 'ypred')[,starts[i]:ends[i]][,-c(1:length_train)]
  sum(drps_mcmc_object(truth = new_data_test %>%
                         dplyr::filter(series == levels(all_data$data_train$series)[i]) %>%
                         dplyr::pull(y),
                       fc = preds)[,1], na.rm = T)
}))
names(ind_drps_updated) <- levels(all_data$data_test$series)

sum(ind_drps)
sum(ind_drps_pf)
sum(ind_drps_updated)
(ind_drps - ind_drps_pf) / ind_drps
plot_mvgam_fc(mod_ind, series = 8, data_test = all_data$data_test)
plot_mvgam_fc(mod_ind_updated, series = 8, data_test = all_data$data_test)
fc$fc_plots$SERC_002()




