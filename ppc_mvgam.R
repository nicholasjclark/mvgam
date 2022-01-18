## Posterior predictive checks
library(mvgam)
library(dplyr)
library(xts)
library(forecast)
data("AirPassengers")
series <- xts::as.xts(floor(AirPassengers))
colnames(series) <- c('Air')
fake_data <- series_to_mvgam(series, freq = 12, train_prop = 0.75)
mod <- mvjagam(data_train = fake_data$data_train,
               data_test = fake_data$data_test,
               formula = y ~ s(season, bs = c('cc'), k = 12),
               knots = list(season = c(0.5, 12.5)),
               family = 'nb',
               trend_model = 'AR3',
               n.burnin = 2000,
               auto_update = F)
plot_mvgam_fc(mod, 1)

#### ppc density overlay ####
object = mod
series = 1
n_samples = 500

data_train <- object$obs_data
ends <- seq(0, dim(MCMCvis::MCMCchains(object$jags_output, 'ypred'))[2],
            length.out = NCOL(object$ytimes) + 1)
starts <- ends + 1
starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
ends <- ends[-1]

s_name <- levels(data_train$series)[series]
truths <- data_train %>%
  dplyr::filter(series == s_name) %>%
  dplyr::select(year, season, y) %>%
  dplyr::distinct() %>%
  dplyr::arrange(year, season) %>%
  dplyr::pull(y)

preds <- MCMCvis::MCMCchains(object$jags_output, 'ypred')[,starts[series]:ends[series]]
preds <- preds[,1:length(truths)]

sample_seq <- sample(1:NROW(preds), n_samples, T)
ymax <- max(c(apply(preds[sample_seq,], 1, function(x) max(density(x)$y))),
            max(density(truths)$y))

plot(density(preds[1,]),
     main = '', xlab = '',
     ylim = c(0, ymax),
     col = rgb(150, 0, 0, max = 255, alpha = 10))
for(i in 1:n_samples){
  lines(density(preds[sample_seq[i],]),
        col = rgb(150, 0, 0, max = 255, alpha = 10))
}

lines(density(truths), lwd = 3)
legend('topright',
       legend = c(expression(hat(y)),
                  'y'),
       bg = 'white',
       col = c(rgb(150, 0, 0, max = 255, alpha = 180),
             'black'),
       lty = 1,lwd = 2)


#### ppc cumulative distribution function overlay ####
object = mod
series = 1
n_samples = 500

data_train <- object$obs_data
ends <- seq(0, dim(MCMCvis::MCMCchains(object$jags_output, 'ypred'))[2],
            length.out = NCOL(object$ytimes) + 1)
starts <- ends + 1
starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
ends <- ends[-1]

s_name <- levels(data_train$series)[series]
truths <- data_train %>%
  dplyr::filter(series == s_name) %>%
  dplyr::select(year, season, y) %>%
  dplyr::distinct() %>%
  dplyr::arrange(year, season) %>%
  dplyr::pull(y)

preds <- MCMCvis::MCMCchains(object$jags_output, 'ypred')[,starts[series]:ends[series]]
preds <- preds[,1:length(truths)]

ecdf_plotdat = function(vals, x){
  func <- ecdf(vals)
  func(x)
}

sample_seq <- sample(1:NROW(preds), n_samples, T)
plot_x <- seq(min(truths, na.rm = T),
              max(truths, na.rm = T))
plot(x = plot_x,
     y = ecdf_plotdat(preds[1,],
                      plot_x),
     main = '', xlab = '',
     ylab = '',
     ylim = c(0, 1),
     col = rgb(150, 0, 0, max = 255, alpha = 10),
     type = 'l')

for(i in 1:n_samples){
  lines(x = plot_x,
        y = ecdf_plotdat(preds[i,],
                         plot_x),
        col = rgb(150, 0, 0, max = 255, alpha = 10))
}

lines(x = plot_x,
      y = ecdf_plotdat(truths,
                       plot_x),
      col = 'black',
      lwd = 3)
legend('bottomright',
       legend = c(expression(hat(y)),
                              'y'),
       bg = 'white',
       col = c(rgb(150, 0, 0, max = 255, alpha = 180),
             'black'),
       lty = 1, lwd = 2)


#### ppc PIT histogram ####
object = mod
series = 1

data_train <- object$obs_data
ends <- seq(0, dim(MCMCvis::MCMCchains(object$jags_output, 'ypred'))[2],
            length.out = NCOL(object$ytimes) + 1)
starts <- ends + 1
starts <- c(1, starts[-c(1, (NCOL(object$ytimes)+1))])
ends <- ends[-1]

s_name <- levels(data_train$series)[series]
truths <- data_train %>%
  dplyr::filter(series == s_name) %>%
  dplyr::select(year, season, y) %>%
  dplyr::distinct() %>%
  dplyr::arrange(year, season) %>%
  dplyr::pull(y)

preds <- MCMCvis::MCMCchains(object$jags_output, 'ypred')[,starts[series]:ends[series]]
preds <- preds[,1:length(truths)]
preds <- preds[!is.na(truths),1:NCOL(preds)]
truths <- truths[!is.na(truths)]

# Calculate emipirical cumulative distribution function as the
# portion of (y_predicted <= y_true)
n_pred <- ncol(preds)
P_x <- vapply(seq_along(truths),
              function(i) {
                sum(preds[i, ] <= truths[i]) / n_pred
              },
              .0)

P_xm1 <- vapply(seq_along(truths),
                function(i) {
                  sum(preds[i,] <= truths[i] - 1) / n_pred
                },
                .0)
# 1000 replicates for randomised PIT
u <- replicate(1000, P_xm1 + stats::runif(length(truths)) * (P_x - P_xm1))
pit_hist <- hist(u, breaks = seq(0, 1, by = 0.1), plot = F)$density
pit_hist <- (pit_hist / sum(pit_hist)) * 10

barplot(pit_hist,
        col = rgb(150, 0, 0, max = 255, alpha = 180),
        border = NA)
abline(h = 1, lty = 'dashed', lwd = 2)
