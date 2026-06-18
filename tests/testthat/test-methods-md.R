# CI tests for methods_md(): the math-only model exporter. Each
# test builds a small mvgam prefit via run_model = FALSE so no
# Stan fit is required, then asserts the rendered markdown has
# the expected section structure and key symbols.

make_methods_md_prefit <- function(formula, trend_formula = NULL,
                                    family = poisson(), data = NULL) {
  set.seed(1L)
  if (is.null(data)) {
    data <- data.frame(
      time   = rep(1:30, 4),
      series = factor(rep(paste0("s", 1:4), each = 30)),
      x      = rnorm(120),
      grp    = factor(rep(c("a", "b", "c", "d"), each = 30)),
      y      = rpois(120, lambda = 3)
    )
  }
  suppressWarnings(mvgam(
    formula       = formula,
    trend_formula = trend_formula,
    data          = data,
    family        = family,
    run_model     = FALSE,
    silent        = 2
  ))
}

test_that("methods_md returns mvgam_methods_md and prints", {
  mod <- make_methods_md_prefit(y ~ x)
  out <- methods_md(mod)
  expect_s3_class(out, "mvgam_methods_md")
  expect_type(out, "character")
  expect_true(grepl("## Data", out))
  expect_true(grepl("## Model", out))
  expect_true(grepl("## Priors", out))
})

test_that("Data section labels family + reports dimensions", {
  mod <- make_methods_md_prefit(y ~ x)
  out <- methods_md(mod)
  expect_true(grepl("non-negative integer counts", out))
  expect_true(grepl("N = 120", out))
  expect_true(grepl("S = 4", out))
  expect_true(grepl("T = 30", out))
})

test_that("Model section emits a single align block with likelihood + link", {
  mod <- make_methods_md_prefit(y ~ x)
  out <- methods_md(mod)
  expect_true(grepl("\\\\begin\\{aligned\\}", out))
  expect_true(grepl("\\\\sim \\\\text\\{Poisson\\}", out))
  expect_true(grepl("\\\\log \\\\mu_\\{i,t\\}", out))
  expect_true(grepl("\\\\alpha", out))
  expect_true(grepl("\\\\beta_\\{x\\}", out))
})

test_that("Gaussian family renders Normal likelihood + identity link", {
  mod <- make_methods_md_prefit(
    y ~ x, family = gaussian(),
    data = data.frame(
      time = rep(1:30, 4),
      series = factor(rep(paste0("s", 1:4), each = 30)),
      x = rnorm(120),
      grp = factor(rep(c("a", "b", "c", "d"), each = 30)),
      y = rnorm(120)
    )
  )
  out <- methods_md(mod)
  expect_true(grepl("\\\\text\\{Normal\\}\\(\\\\mu_\\{i,t\\}", out))
  expect_true(grepl("real-valued observations", out))
  # Identity link should not introduce a log / logit wrapper.
  expect_false(grepl("\\\\log \\\\mu_\\{i,t\\}", out))
})

test_that("RW trend emits eta_{t-1} dynamics + Normal innovation", {
  mod <- make_methods_md_prefit(y ~ 1, trend_formula = ~ RW())
  out <- methods_md(mod)
  expect_true(grepl("\\\\eta_\\{i,t\\} &= \\\\eta_\\{i,t-1\\}", out))
  expect_true(grepl("\\\\epsilon\\^\\{\\(\\\\eta\\)\\}_\\{i,t\\}", out))
  expect_true(grepl("\\\\sigma_\\\\eta", out))
})

test_that("AR(p = 1) trend emits phi_1 eta_{t-1} dynamics", {
  mod <- make_methods_md_prefit(y ~ 1, trend_formula = ~ AR(p = 1))
  out <- methods_md(mod)
  expect_true(grepl("\\\\phi_\\{1\\} \\\\eta_\\{i,t-1\\}", out))
})

test_that("AR(p = c(1, 2)) emits both lag terms in the dynamics row", {
  mod <- make_methods_md_prefit(
    y ~ 1, trend_formula = ~ AR(p = c(1L, 2L))
  )
  out <- methods_md(mod)
  expect_true(grepl("\\\\phi_\\{1\\} \\\\eta_\\{i,t-1\\}", out))
  expect_true(grepl("\\\\phi_\\{2\\} \\\\eta_\\{i,t-2\\}", out))
})

test_that("Smooth term renders f_{x}(x) inline + basis decomposition", {
  mod <- make_methods_md_prefit(y ~ s(x, k = 5))
  out <- methods_md(mod)
  expect_true(grepl("f_\\{x\\}\\(x_\\{i,t\\}\\)", out))
  expect_true(grepl("\\\\beta\\^\\{\\(x\\)\\}_k", out))
  expect_true(grepl("B_k\\(x\\)", out))
  expect_true(grepl("\\\\lambda_\\{x\\}", out))
})

test_that("Approximate 1D GP renders with kernel + length scale + marginal SD", {
  mod <- make_methods_md_prefit(y ~ gp(x, k = 5))
  out <- methods_md(mod)
  expect_true(grepl("f\\^\\{\\(\\\\text\\{gp\\}\\)\\}_\\{x\\}", out))
  expect_true(grepl(
    "\\\\text\\{GP\\}\\\\left\\(0, k_\\{\\\\text\\{ExpQuad\\}\\}",
    out
  ))
  expect_true(grepl("\\\\rho_\\{x\\}", out))
  expect_true(grepl("\\\\sigma\\^\\{\\(\\\\text\\{gp\\}\\)\\}_\\{x\\}", out))
  expect_true(grepl("approximated with 5 basis functions", out))
})

test_that("Exact GP (no k) renders as 'exact (full covariance kernel)'", {
  mod <- make_methods_md_prefit(y ~ gp(x))
  out <- methods_md(mod)
  expect_true(grepl("f\\^\\{\\(\\\\text\\{gp\\}\\)\\}_\\{x\\}", out))
  expect_true(grepl("exact \\(full covariance kernel\\)", out))
  # Should not claim a basis-function count.
  expect_false(grepl("approximated with", out))
})

test_that("Approximate 2D GP renders vector length-scale + both vars", {
  set.seed(1L)
  dat <- data.frame(
    time = rep(1:30, 4),
    series = factor(rep(paste0("s", 1:4), each = 30)),
    x = rnorm(120), z = rnorm(120),
    grp = factor(rep(c("a","b","c","d"), each = 30)),
    y = rpois(120, 3)
  )
  mod <- make_methods_md_prefit(y ~ gp(x, z, k = 5), data = dat)
  out <- methods_md(mod)
  # Bold rho for multi-dim length scale; subscript carries both vars.
  expect_true(grepl(
    "\\\\boldsymbol\\{\\\\rho\\}_\\{x, z\\}", out
  ))
  expect_true(grepl("Gaussian process in \\$x, z\\$", out))
})

test_that("By-factor GP carries the grouping variable in the subscript", {
  set.seed(1L)
  dat <- data.frame(
    time = rep(1:30, 4),
    series = factor(rep(paste0("s", 1:4), each = 30)),
    x = rnorm(120),
    grp = factor(rep(c("a","b","c","d"), each = 30)),
    y = rpois(120, 3)
  )
  mod <- make_methods_md_prefit(
    y ~ gp(x, by = grp, k = 5), data = dat
  )
  out <- methods_md(mod)
  # Subscript uses `\mid` to separate vars from the by var.
  expect_true(grepl("x \\\\mid grp", out))
  expect_true(grepl("stratified by \\$grp\\$", out))
})

test_that("Tensor t2(x, z) smooth renders as f_{x, z} with both args", {
  set.seed(1L)
  dat <- data.frame(
    time = rep(1:30, 4),
    series = factor(rep(paste0("s", 1:4), each = 30)),
    x = rnorm(120), z = rnorm(120),
    grp = factor(rep(c("a","b","c","d"), each = 30)),
    y = rpois(120, 3)
  )
  mod <- make_methods_md_prefit(y ~ t2(x, z, k = 5), data = dat)
  out <- methods_md(mod)
  # Linear predictor has both vars indexed
  expect_true(grepl("f_\\{x, z\\}\\(x_\\{i,t\\}, z_\\{i,t\\}\\)", out))
  # Term definition uses the joined key in basis-size / coef subscripts
  expect_true(grepl(
    "\\\\sum_\\{k=1\\}\\^\\{K_\\{x:z\\}\\}",
    out
  ))
  expect_true(grepl("\\\\beta\\^\\{\\(x:z\\)\\}_k B_k\\(x, z\\)", out))
  # Glossary names the tensor basis kind and lists both dims
  expect_true(grepl("tensor product smooth \\(t2\\)", out))
  expect_true(grepl("in \\$x\\$, \\$z\\$", out))
})

test_that("gp_call_to_spec errors on a malformed call with no variables", {
  # gp() with only named args (no positional variable list) is
  # nonsensical; should error rather than silently produce a
  # stub with vars = character(0).
  bad_call <- str2lang("gp(k = 5)")
  expect_error(
    mvgam:::gp_call_to_spec(bad_call),
    "no positional variable arguments"
  )
})

test_that("Group-level RE renders alpha_{grp[i]} + hyperprior shape", {
  mod <- make_methods_md_prefit(y ~ (1 | grp))
  out <- methods_md(mod)
  expect_true(grepl("\\\\alpha_\\{grp\\[i\\]\\}", out))
  expect_true(grepl(
    "\\\\alpha_\\{grp\\} &\\\\sim \\\\text\\{Normal\\}\\(0",
    out
  ))
  expect_true(grepl("\\\\sigma_\\{grp\\}", out))
})

test_that("No-trend gaussian model omits eta_{i,t} from linpred", {
  mod <- make_methods_md_prefit(
    y ~ x, family = gaussian(),
    data = data.frame(
      time = rep(1:30, 4),
      series = factor(rep(paste0("s", 1:4), each = 30)),
      x = rnorm(120),
      grp = factor(rep(c("a", "b", "c", "d"), each = 30)),
      y = rnorm(120)
    )
  )
  out <- methods_md(mod)
  expect_false(grepl("\\\\eta_\\{i,t\\}", out))
})

test_that("Priors section backfills umbrella text onto specific rows", {
  mod <- make_methods_md_prefit(y ~ (1 | grp))
  out <- methods_md(mod)
  # sigma_{grp} should appear (specific group label), not sigma_j.
  expect_true(grepl(
    "\\\\sigma_\\{grp\\} &\\\\sim \\\\text\\{StudentT\\}", out
  ))
  expect_false(grepl("\\\\sigma_\\{j\\}", out))
})

test_that("methods_md(file = path) writes the markdown to disk", {
  mod <- make_methods_md_prefit(y ~ x)
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp), add = TRUE)
  ret <- methods_md(mod, file = tmp)
  expect_true(file.exists(tmp))
  expect_true(grepl("## Model", paste(readLines(tmp), collapse = "\n")))
  expect_s3_class(ret, "mvgam_methods_md")
})

test_that("notation arg accepts 'default' and 'brms', rejects others", {
  mod <- make_methods_md_prefit(y ~ x)
  expect_s3_class(methods_md(mod, notation = "default"),
                   "mvgam_methods_md")
  expect_s3_class(methods_md(mod, notation = "brms"),
                   "mvgam_methods_md")
  expect_error(methods_md(mod, notation = "bogus"),
               "element of set")
})

# Family-distribution renderer: covers brms-native shapes the
# methods_md user is most likely to fit. Targeted unit tests on
# family_distribution_text + family_data_label so adding a new
# family is one switch entry plus one assertion line here.

test_that("family_distribution_text covers core brms families", {
  ft <- mvgam:::family_distribution_text
  mu <- "\\mu"
  expect_equal(ft("poisson", mu, NULL),
               "\\text{Poisson}(\\mu)")
  expect_equal(ft("bernoulli", mu, NULL),
               "\\text{Bernoulli}(\\mu)")
  expect_equal(ft("binomial", mu, NULL),
               "\\text{Binomial}(n_{i,t}, \\mu)")
  expect_equal(ft("gaussian", mu, NULL),
               "\\text{Normal}(\\mu, \\sigma)")
  expect_equal(ft("student", mu, NULL),
               "\\text{StudentT}(\\nu, \\mu, \\sigma)")
  expect_equal(ft("lognormal", mu, NULL),
               "\\text{LogNormal}(\\mu, \\sigma)")
  expect_equal(ft("Gamma", mu, NULL),
               "\\text{Gamma}(\\alpha, \\mu)")
  expect_equal(ft("beta", mu, NULL),
               "\\text{Beta}(\\mu, \\phi)")
  expect_equal(ft("negbinomial", mu, NULL),
               "\\text{NegBin}(\\mu, \\phi)")
})

test_that("family_distribution_text covers tweedie + hurdle + ZI", {
  ft <- mvgam:::family_distribution_text
  mu <- "\\mu"
  expect_equal(ft("tweedie", mu, NULL),
               "\\text{Tweedie}(\\mu, \\phi, \\xi)")
  expect_equal(ft("hurdle_poisson", mu, NULL),
               "\\text{Hurdle-Poisson}(\\mu, \\pi_{\\text{hu}})")
  expect_equal(
    ft("hurdle_negbinomial", mu, NULL),
    "\\text{Hurdle-NegBin}(\\mu, \\phi, \\pi_{\\text{hu}})"
  )
  expect_equal(ft("zero_inflated_poisson", mu, NULL),
               "\\text{ZIPoisson}(\\mu, \\pi_{\\text{zi}})")
  expect_equal(
    ft("zero_inflated_negbinomial", mu, NULL),
    "\\text{ZINegBin}(\\mu, \\phi, \\pi_{\\text{zi}})"
  )
  expect_equal(
    ft("zero_inflated_binomial", mu, NULL),
    "\\text{ZIBinomial}(n_{i,t}, \\mu, \\pi_{\\text{zi}})"
  )
})

test_that("family_distribution_text covers ordinal families", {
  ft <- mvgam:::family_distribution_text
  mu <- "\\eta_{i,t}"
  expect_equal(
    ft("cumulative", mu, NULL),
    "\\text{OrderedCumulative}(\\boldsymbol{\\theta}, \\eta_{i,t})"
  )
  expect_equal(
    ft("sratio", mu, NULL),
    "\\text{OrderedStoppingRatio}(\\boldsymbol{\\theta}, \\eta_{i,t})"
  )
  expect_equal(
    ft("cratio", mu, NULL),
    "\\text{OrderedContinuationRatio}(\\boldsymbol{\\theta}, \\eta_{i,t})"
  )
  expect_equal(
    ft("acat", mu, NULL),
    "\\text{OrderedAdjacentCategory}(\\boldsymbol{\\theta}, \\eta_{i,t})"
  )
})

test_that("family_data_label maps mixture / ordinal families", {
  fl <- mvgam:::family_data_label
  expect_equal(
    fl("hurdle_poisson"),
    "non-negative integer counts with point mass at zero"
  )
  expect_equal(
    fl("zero_inflated_poisson"),
    "zero-inflated non-negative integer counts"
  )
  expect_equal(fl("cumulative"), "ordered categorical observations")
  expect_equal(
    fl("tweedie"),
    "non-negative real observations (compound Poisson-gamma)"
  )
})

# Prior distribution formatter: targeted unit tests on the
# regex-based string mapper. Easier to keep in sync with brms
# prior-string conventions when tested directly.

test_that("format_prior_distribution maps every common brms family", {
  fmt <- mvgam:::format_prior_distribution
  expect_equal(fmt("normal(0, 1)"), "\\text{Normal}(0, 1)")
  expect_equal(fmt("student_t(3, 0, 2.5)"),
               "\\text{StudentT}(3, 0, 2.5)")
  expect_equal(fmt("lognormal(0, 1)"), "\\text{LogNormal}(0, 1)")
  expect_equal(fmt("exponential(1)"), "\\text{Exponential}(1)")
  expect_equal(fmt("gamma(2, 1)"), "\\text{Gamma}(2, 1)")
  expect_equal(fmt("inv_gamma(2, 1)"), "\\text{InvGamma}(2, 1)")
  expect_equal(fmt("cauchy(0, 5)"), "\\text{Cauchy}(0, 5)")
  expect_equal(fmt("beta(1, 1)"), "\\text{Beta}(1, 1)")
  expect_equal(fmt("uniform(-1, 1)"), "\\text{Uniform}(-1, 1)")
  expect_equal(fmt("lkj_corr_cholesky(1)"), "\\text{LKJCorr}(1)")
  expect_equal(fmt("(flat)"), "\\text{flat}")
  expect_equal(fmt(""), "\\text{flat}")
})
