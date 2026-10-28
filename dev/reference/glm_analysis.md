# GLM likelihood detection and trend injection

brms folds the linear predictor into its GLM likelihood calls. The
functions here find those calls and decide how the trend joins each one.
