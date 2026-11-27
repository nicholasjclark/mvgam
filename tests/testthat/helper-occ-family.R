# Helpers shared by test-occ-family.R and test-extra-occ-family.R.

# ------------------------------------------------------------
# validate_closure_unit_data() binary-response branch
# ------------------------------------------------------------

make_occ_data <- function(n_unit = 5, n_visit = 4, seed = 1) {
  set.seed(seed)
  n_total <- n_unit * n_visit
  data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_total),
    visit  = rep(seq_len(n_visit), n_unit),
    y      = rbinom(n_total, 1L, 0.4),
    elev   = rep(rnorm(n_unit), each = n_visit),
    tod    = stats::runif(n_total)
  )
}
