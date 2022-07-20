library(mvgam)
dat <- sim_mvgam(T = 100, n_series=4, n_lv = 1)
dat$true_corrs


mod1 <- mvgam(formula = y ~ s(season, bs = 'cc') +
                s(series, bs = 're'),
              data_train = dat$data_train,
              trend_model = 'AR1',
              family = 'poisson',
              use_lv = TRUE,
              n_lv = 2,
              use_stan = TRUE,
              run_model = T,
              burnin = 10)
mod1$model_file
summary(mod1)
plot_mvgam_factors(mod1)
plot(mod1, type = 'residuals')
fake <- dat$data_test
fake$y <- NULL
plot(mod1, 'trend', data_test = fake)



plot_mvgam_series = function(data_train, data_test, series, n_bins,
                             log_scale = FALSE){

  if(series == 'all'){
    n_plots <- length(levels(data_train$series))
    pages <- 1
    .pardefault <- par(no.readonly=T)
    par(.pardefault)

    if (n_plots > 4) pages <- 2
    if (n_plots > 8) pages <- 3
    if (n_plots > 12) pages <- 4
    if (pages != 0)  {
      ppp <- n_plots %/% pages

      if (n_plots %% pages != 0) {
        ppp <- ppp + 1
        while (ppp * (pages - 1) >= n_plots) pages <- pages - 1
      }

      # Configure layout matrix
      c <- r <- trunc(sqrt(ppp))
      if (c<1) r <- c <- 1
      if (c*r < ppp) c <- c + 1
      if (c*r < ppp) r <- r + 1
      oldpar<-par(mfrow=c(r,c))

    } else { ppp <- 1; oldpar <- par()}

    all_ys <- lapply(seq_len(n_plots), function(x){
      if(log_scale){
        log(      data.frame(y = data_train$y,
                             series = data_train$series,
                             time = data_train$time) %>%
                    dplyr::filter(series == levels(data_train$series)[x]) %>%
                    dplyr::pull(y) + 1)
      } else {
        data.frame(y = data_train$y,
                   series = data_train$series,
                   time = data_train$time) %>%
          dplyr::filter(series == levels(data_train$series)[x]) %>%
          dplyr::pull(y)
      }
    })

    for(i in 1:n_plots){
      s_name <- levels(data_train$series)[i]

      truth <- data.frame(y = data_train$y,
                          series = data_train$series,
                          time = data_train$time) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::select(time, y) %>%
        dplyr::distinct() %>%
        dplyr::arrange(time) %>%
        dplyr::pull(y)

      if(log_scale){
        truth <- log(truth + 1)
        ylab <- 'log(Y + 1)'
      } else {
        ylab <- 'Y'
      }

      plot(1, type = "n", bty = 'L',
           xlab = 'Time',
           ylab = ylab,
           ylim = range(unlist(all_ys)),
           xlim = c(0, length(c(truth))))
      title(s_name, line = 0)

      if(n_plots > 1){
        for(x in 1:n_plots){
          lines(all_ys[[x]], lwd = 1.85, col = 'grey85')
        }
      }

      lines(x = 1:length(truth), y = truth, lwd = 3, col = "white")
      lines(x = 1:length(truth), y = truth, lwd = 2.5, col = "#8F2727")
      box(bty = 'L', lwd = 2)

    }

  } else {
    s_name <- levels(data_train$series)[series]
    truth <- data.frame(y = data_train$y,
                        series = data_train$series,
                        time = data_train$time) %>%
      dplyr::filter(series == s_name) %>%
      dplyr::select(time, y) %>%
      dplyr::distinct() %>%
      dplyr::arrange(time) %>%
      dplyr::pull(y)

    layout(matrix(1:4, nrow = 2, byrow = TRUE))
    if(!missing(data_test)){
      test <- data.frame(y = data_test$y,
                         series = data_test$series,
                         time = data_test$time) %>%
        dplyr::filter(series == s_name) %>%
        dplyr::select(time, y) %>%
        dplyr::distinct() %>%
        dplyr::arrange(time) %>%
        dplyr::pull(y)

      plot(1, type = "n", bty = 'L',
           xlab = 'Time',
           ylab = 'Y',
           ylim = range(c(truth, test)),
           xlim = c(0, length(c(truth, test))))
      title('Time series', line = 0)

      lines(x = 1:length(truth), y = truth, lwd = 2, col = "#8F2727")
      lines(x = (length(truth)+1):length(c(truth, test)), y = test, lwd = 2, col = "black")
      abline(v = length(truth)+1, col = '#FFFFFF60', lwd = 2.85)
      abline(v = length(truth)+1, col = 'black', lwd = 2.5, lty = 'dashed')
      box(bty = 'L', lwd = 2)

      if(missing(n_bins)){
        n_bins <- max(c(length(hist(c(truth, test), plot = F)$breaks),
                        20))
      }

      hist(c(truth, test), border = "#8F2727",
           lwd = 2,
           freq = FALSE,
           breaks = n_bins,
           col = "#C79999",
           ylab = 'Density',
           xlab = 'Count', main = '')
      title('Histogram', line = 0)

      acf(c(truth, test),
          na.action = na.pass, bty = 'L',
          lwd = 2.5, ci.col = 'black', col = "#8F2727",
          main = '', ylab = 'Autocorrelation')
      acf1 <- acf(c(truth, test), plot = F,
                  na.action = na.pass)
      clim <- qnorm((1 + .95)/2)/sqrt(acf1$n.used)
      abline(h = clim,  col = '#FFFFFF', lwd = 2.85)
      abline(h = clim,  col = 'black', lwd = 2.5, lty = 'dashed')
      abline(h = -clim,  col = '#FFFFFF', lwd = 2.85)
      abline(h = -clim,  col = 'black', lwd = 2.5, lty = 'dashed')
      box(bty = 'L', lwd = 2)
      title('ACF', line = 0)

      ecdf_plotdat = function(vals, x){
        func <- ecdf(vals)
        func(x)
      }

      plot_x <- seq(min(c(truth, test), na.rm = T),
                    max(c(truth, test), na.rm = T))
      plot(1, type = "n", bty = 'L',
           xlab = 'Count',
           ylab = 'Empirical CDF',
           xlim = c(min(plot_x), max(plot_x)),
           ylim = c(0, 1))
      title('CDF', line = 0)
      lines(x = plot_x,
            y = ecdf_plotdat(c(truth, test),
                             plot_x),
            col = "#8F2727",
            lwd = 2.5)
      box(bty = 'L', lwd = 2)

    } else {
      plot(1, type = "n", bty = 'L',
           xlab = 'Time',
           ylab = 'Observations',
           ylim = range(c(truth)),
           xlim = c(0, length(c(truth))))
      title('Time series', line = 0)

      lines(x = 1:length(truth), y = truth, lwd = 2, col = "#8F2727")
      box(bty = 'L', lwd = 2)

      if(missing(n_bins)){
        n_bins <- max(c(length(hist(c(truth), plot = F)$breaks),
                        20))
      }

      hist(c(truth), border = "#8F2727",
           lwd = 2,
           freq = FALSE,
           breaks = n_bins,
           col = "#C79999",
           ylab = 'Density',
           xlab = 'Count', main = '')
      title('Histogram', line = 0)


      acf(c(truth),
          na.action = na.pass, bty = 'L',
          lwd = 2.5, ci.col = 'black', col = "#8F2727",
          main = '', ylab = 'Autocorrelation')
      acf1 <- acf(c(truth), plot = F,
                  na.action = na.pass)
      clim <- qnorm((1 + .95)/2)/sqrt(acf1$n.used)
      abline(h = clim,  col = '#FFFFFF', lwd = 2.85)
      abline(h = clim,  col = 'black', lwd = 2.5, lty = 'dashed')
      abline(h = -clim,  col = '#FFFFFF', lwd = 2.85)
      abline(h = -clim,  col = 'black', lwd = 2.5, lty = 'dashed')
      box(bty = 'L', lwd = 2)
      title('ACF', line = 0)


      ecdf_plotdat = function(vals, x){
        func <- ecdf(vals)
        func(x)
      }

      plot_x <- seq(min(truth, na.rm = T),
                    max(truth, na.rm = T))
      plot(1, type = "n", bty = 'L',
           xlab = 'Count',
           ylab = 'Empirical CDF',
           xlim = c(min(plot_x), max(plot_x)),
           ylim = c(0, 1))
      title('CDF', line = 0)
      lines(x = plot_x,
            y = ecdf_plotdat(truth,
                             plot_x),
            col = "#8F2727",
            lwd = 2.5)
      box(bty = 'L', lwd = 2)
    }

    layout(1)
  }

}

dat <- sim_mvgam(T = 100, n_series = 4, n_lv = 2,
                 mu_obs = c(4, 6, 10, 14), trend_rel = 0.3,
                 seasonality = 'hierarchical')
plot_mvgam_series(data_train = dat$data_train,
                  n_bins = 20,
                  series = 'all',
                  log_scale = TRUE)

# Good for testing model files without compiling
stanc(model_code = mod1$model_file)$model_name
model_file <- mod1$model_file

mod2 <- mvgam(formula = y ~ s(season, bs = 'cc') +
                s(series, bs = 're'),
              data_train = dat$data_train,
              trend_model = 'RW',
              family = 'poisson',
              use_lv = TRUE,
              n_lv = 2,
              run_model = TRUE,
              burnin = 10)


plot(mod1, series = 3, 'forecast', data_test = dat$data_test)
plot(mod2, series = 3, 'forecast', data_test = dat$data_test)

plot(mod1, series = 4, 'trend', data_test = dat$data_test)
plot(mod2, series = 4, 'trend', data_test = dat$data_test)


# respect upper bounds for forecasts, prediction, particle filtering
trunc_poiss = function(lambda, bound){
  out <- vector(length = length(lambda))
  for(i in 1:length(out)){
    func <- ecdf(rpois(10000, lambda = lambda[i]))
    unif_bound <- func(bound)
    out[i] <- qpois(runif(1, 0, unif_bound), lambda = lambda[i])
  }
 out
}
trunc_poiss(c(10,8, 21, 11, 5), 20)

