
testing_evalplots = function(mod, mod2){


mod1_eval <- roll_eval_mvgam(mod, n_samples = 1000,
                fc_horizon = 6, n_cores = 3,
                n_evaluations = 10)
mod2_eval <- roll_eval_mvgam(mod2, n_samples = 1000,
                fc_horizon = 6, n_cores = 3,
                n_evaluations = 10)

model_summary <- rbind(mod1_eval$drps_summary, mod2_eval$drps_summary)
rownames(model_summary) <- c('Model 1', 'Model 2')
cat('DRPS summaries per model (lower is better)\n')
print(model_summary)

# Plotting loop
ask <- TRUE

for(i in 1:3) {
if(i==1){
  barplot(c('model 1' = mod1_eval$sum_drps,
            'model 2' = mod2_eval$sum_drps),
          ylab = 'Sum DRPS (lower is better)')
} else if(i ==2){

  boxplot(list('model 1' = mod1_eval$drps_summary,
               'model 2' = mod2_eval$drps_summary),
          ylab = 'Sum DRPS (lower is better)')
} else {
  plot_dat <- rbind(mod1_eval$drps_horizon_summary$mean_drps,
                    mod2_eval$drps_horizon_summary$mean_drps)
  colnames(plot_dat) <- seq(1:NCOL(plot_dat))
  barplot(plot_dat,
          beside=T,
          xlab = 'Forecast horizon',
          ylab = 'Mean DRPS (lower is better)',
          col = c("darkgrey",  "black"),
          legend.text = c('Model 1', 'Model 2'),
          args.legend = list(x = "topleft"))
}

  if (ask) { ## this is within loop so we don't get asked before it's necessary
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
    ask <- FALSE ## only need to do this once
  }

}

invisible()
}
testing_evalplots(mod, mod2)
