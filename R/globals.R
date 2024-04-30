#' Prevent R CMD Check notes about missing global variables due to
#' dplyr mutates etc...
#' @noRd
utils::globalVariables(c("y", "year", "smooth_vals", "smooth_num",
                         "series", "season", "rowid", "row_number",
                         "nsp", "last_assim", "index", "in_interval",
                         "assimilated", "eval_horizon", "label",
                         "mod_call", "particles", "obs", "mgcv_model",
                         "param_name", "outcome", "mgcv_plottable",
                         "term", "data_test", "object", "row_num", "trends_test",
                         "trend", "trend_series", "trend_y", ".", "gam",
                         "group", "mod", "row_id", "byvar", "direction",
                         "index..time..index", "trend_test", "Var2",
                         "add_cor", "add_ma", "add_nmix", "binomial",
                         "current", "drop_obs_intercept",
                         "gp_details", "gp_terms", "k", "mus",
                         "name", "needed", "nmix_trendmap", "orig_formula",
                         "trial", "use_var1", "use_var1cor", "xcols_drop",
                         "time_lag", "dis_time", "maxt", "orig_rows",
                         "matches", "time.", "file_name"))
