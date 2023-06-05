#' Prevent R CMD Check notes about missing global variables due to
#' dplyr mutates etc...
#' @noRd
utils::globalVariables(c("y", "year", "smooth_vals", "smooth_num",
                         "series", "season", "rowid", "row_number",
                         "nsp", "last_assim", "index", "in_interval",
                         "assimilated", "eval_horizon", "label",
                         "mod_call", "particles", "obs", "mgcv_model",
                         "param_name", "outcome", "mgcv_plottable"))
