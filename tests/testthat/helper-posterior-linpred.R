# Helpers shared by test-posterior-linpred.R and test-extra-posterior-linpred.R.


# ---- The linear-predictor composer --------------------------------
#
# A prep as `prepare_linpred_data()` builds it: the draws,
# the Stan data brms writes, the formula, the group-level table and
# the `re_formula` the data was built under. The end-to-end check
# against brms's own predictor lives in tests/local, which needs fits.

mk_prep <- function(sdata, draws, formula, ranef = NULL, re_formula = NULL,
                    nobs = NULL) {
  structure(list(
    draws = posterior::as_draws_matrix(draws),
    sdata = sdata,
    nobs = nobs %||% nrow(sdata$X %||% sdata[[1L]]),
    formula = formula,
    predictors = model_predictors(formula),
    ranef = ranef,
    re_formula = re_formula
  ), class = "mvgam_prep")
}

one_draw <- function(...) {
  values <- c(...)
  matrix(values, nrow = 1L, dimnames = list(NULL, names(values)))
}

# A design as brms writes it, each column numbered by its formula term
# in `assign`, the intercept as 0
design <- function(X, assign) {
  structure(X, assign = as.integer(assign))
}

# An intercept-only design of `n` rows
intercept_design <- function(n) {
  design(cbind(Intercept = rep(1, n)), 0L)
}


# The group-level table brms keeps on a fit, one row per term.
mk_ranef <- function(rows, levels) {
  ranef <- data.frame(
    id = rows$id, group = rows$group, gn = rows$gn %||% 1L, gtype = "",
    coef = "Intercept", cn = rows$cn %||% 1L, resp = rows$resp %||% "",
    dpar = rows$dpar %||% "", nlpar = "", type = "", cor = TRUE,
    stringsAsFactors = FALSE
  )
  ranef$form <- rep(list(~1), nrow(ranef))
  ranef$data_id <- ranef$id
  ranef$data_cn <- ranef$cn
  attr(ranef, "levels") <- levels
  ranef
}
