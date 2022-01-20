#'Compare fitted mvjagam objects for model selection using rolling forecast evaluations
#'
#'This function compares two fitted models using rolling window forecast evaluation and
#'provides a series of summary plots to facilitate model selection. It is essentially a wrapper for
#'\code{roll_eval_mvgam}
#'
#'@param model1 \code{list} object returned from \code{mvjagam} representing the first model to be
#'evaluated
#'@param model2 \code{list} object returned from \code{mvjagam} representing the second model to be
#'evaluated
#'@param n_samples \code{integer} specifying the number of samples to generate from the model's
#'posterior distribution
#'@param fc_horizon \code{integer} specifying the length of the forecast horizon for evaluating forecasts
#'@param n_evaluations \code{integer} specifying the total number of evaluations to perform
#'@param n_cores \code{integer} specifying number of cores for generating particle forecasts in parallel
#'@return A series of plots comparing forecast Discrete Rank Probability Scores (DRPS) for each competing
#'model. A lower DRPS is preferred. Note however that it is possible to select a model that ultimately
#'would perform poorly in true out-of-sample forecasting. For example if a wiggly smooth function of 'year'
#'is included in the model then this function will be learned prior to evaluating rolling window forecasts,
#'and the model could generate very tight predictions as a result. But when forecasting ahead to timepoints
#'that the model has not seen (i.e. next year), the smooth function will end up extrapolating, sometimes
#'in very strange and unexpected ways. It is therefore recommended to only use smooth functions for
#'covariates that are adequately measured in the data (i.e. 'seasonality', for example) to reduce possible
#'extrapolation of smooths and let the latent trends in the \code{mvjagam} model capture any
#'temporal dependencies in the data. These trends are time series models and so will provide much more
#'stable forecasts
#'@seealso \code{roll_eval_mvgam}, \code{eval_mvgam}
#'@export
compare_mvgams = function(model1,
                             model2,
                             n_samples = 1000,
                             fc_horizon = 3,
                             n_evaluations = 10,
                             n_cores = 2){

# Evaluate the two models
mod1_eval <- roll_eval_mvgam(model1,
                             n_samples = n_samples,
                             fc_horizon = fc_horizon,
                             n_cores = n_cores,
                             n_evaluations = n_evaluations)
mod2_eval <- roll_eval_mvgam(model2,
                             n_samples = n_samples,
                             fc_horizon = fc_horizon,
                             n_cores = n_cores,
                             n_evaluations = n_evaluations)

# Generate a simple summary of forecast DRPS for each model
model_summary <- rbind(mod1_eval$drps_summary, mod2_eval$drps_summary)
rownames(model_summary) <- c('Model 1', 'Model 2')
cat('DRPS summaries per model (lower is better)\n')
print(model_summary)

# Print 90% interval coverages for each model
cat('\n90% interval coverages per model (closer to 0.9 is better)\n')
cat('Model 1', mod1_eval$interval_coverage, '\n')
cat('Model 2', mod2_eval$interval_coverage)

# Set up plotting loop and return summary plots of DRPS
ask <- TRUE

for(i in 1:3) {
if(i == 1){
  barplot(c('model 1' = mod1_eval$sum_drps,
            'model 2' = mod2_eval$sum_drps),
          col = c("#B97C7C",  "#7C0000"),
          border = NA,
          ylab = 'Sum DRPS (lower is better)')
} else if(i == 2){
  boxplot(list('model 1' = mod1_eval$drps_summary,
               'model 2' = mod2_eval$drps_summary),
          border = c("#B97C7C",  "#7C0000"),
          ylab = 'Sum DRPS per evaluation')
} else {
  plot_dat <- rbind(mod1_eval$drps_horizon_summary$mean_drps,
                    mod2_eval$drps_horizon_summary$mean_drps)
  colnames(plot_dat) <- seq(1:NCOL(plot_dat))
  barplot(plot_dat,
          ylim = c(0, max(plot_dat, na.rm = T) * 1.5),
          beside = T,
          xlab = 'Forecast horizon',
          ylab = 'Mean DRPS',
          col = c("#B97C7C",  "#7C0000"),
          border = NA,
          legend.text = c('Model 1', 'Model 2'),
          args.legend = list(x = "top", ncol = 2, border = NA))
}

  if(ask){
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
    ask <- FALSE
  }

}
invisible()
}
