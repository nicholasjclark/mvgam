# Helpers shared by test-brms-observation-specials.R and test-extra-brms-observation-specials.R.
## Coverage of brms observation-level specials through the
## mvgam_formula -> stancode/standata pipeline, plus the
## trend_formula formula_ad gatekeeper.
##
## Two surfaces under test:
##   1. Observation side: each brms special that mvgam claims to
##      support (`mm()`, `me()`, `cs()`, `cens()`,
##      `trials()`, `car()`) plumbs through to specific standata
##      keys and stancode markers via the mvgam dispatch path.
##   2. Trend side: `mvgam_formula()` refuses every brms
##      `formula_ad` special. Detection walks the formula AST, which
##      keeps a covariate named like an addition term from matching.

make_specials_data <- function(seed = 1L, n = 60L) {
  set.seed(seed)
  data.frame(
    y        = rnorm(n),
    ybin     = rbinom(n, size = 10L, prob = 0.4),
    yord     = factor(sample(1:4, n, replace = TRUE), ordered = TRUE),
    x        = rnorm(n),
    sdx      = abs(rnorm(n, 0.2, 0.05)),
    sex      = abs(rnorm(n, 0.5, 0.1)),
    cens_ind = sample(c("none", "right"), n, replace = TRUE),
    ntrials  = sample(8:12, n, replace = TRUE),
    g1       = factor(sample(letters[1:4], n, replace = TRUE)),
    g2       = factor(sample(letters[5:8], n, replace = TRUE)),
    loc      = factor(paste0("L", rep(1:10, each = n / 10L))),
    time     = rep(seq_len(n / 2L), 2L),
    series   = factor(rep(c("a", "b"), each = n / 2L))
  )
}

# ---- Observation-side specials wire through to stancode/standata ----

# One prefit gives the program and its data. Each test checks the
# standata key brms emits for its special, the column that fills it,
# and the model-block line that uses it.
specials_program <- function(formula, family, d = make_specials_data(),
                             ...) {
  mod <- mvgam(formula, trend_formula = ~ AR(p = 1), data = d,
               family = family, run_model = FALSE, silent = 2, ...,
               validate = FALSE)
  list(code = as.character(mod$stancode), data = mod$standata)
}
