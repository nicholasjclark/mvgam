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

test_that("the software versions reported are the ones the fit recorded", {
  # The describing session may have other versions installed than the
  # one that built the model. Reporting the session's versions
  # described software the model never met, and under rstan it gave
  # the rstan package version as the version of Stan.
  mod <- make_methods_md_prefit(y ~ x)
  expect_identical(mod$stan_version, mvgam:::live_stan_version(mod$backend))
  mod$stan_version <- "9.9.9"
  mod$brms_version <- package_version("8.8.8")
  out <- methods_md(mod)
  expect_match(out, "Stan 9.9.9", fixed = TRUE)
  expect_match(out, "brms 8.8.8", fixed = TRUE)
  mod$stan_version <- NULL
  expect_match(methods_md(mod), "Stan (version not recorded)", fixed = TRUE)
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

test_that("Varying slope (x | grp) renders MVNormal + LKJ joint", {
  set.seed(1L)
  dat <- data.frame(
    time = rep(1:30, 4),
    series = factor(rep(paste0("s", 1:4), each = 30)),
    x = rnorm(120),
    grp = factor(rep(c("a","b","c","d"), each = 30)),
    y = rpois(120, 3)
  )
  mod <- make_methods_md_prefit(y ~ x + (x | grp), data = dat)
  out <- methods_md(mod)
  # Linear predictor contains both intercept and slope deviations.
  expect_true(grepl("\\\\alpha_\\{grp\\[i\\]\\}", out))
  expect_true(grepl(
    "\\\\beta\\^\\{\\(grp\\)\\}_\\{x, grp\\[i\\]\\} x_\\{i,t\\}",
    out
  ))
  # Term def: joint MVNormal + LKJ on Omega.
  expect_true(grepl("\\\\text\\{MVNormal\\}", out))
  expect_true(grepl(
    "\\\\boldsymbol\\{\\\\Omega\\}_\\{grp\\} &\\\\sim \\\\text\\{LKJCorr\\}",
    out
  ))
  expect_true(grepl(
    "\\\\boldsymbol\\{\\\\Sigma\\}_\\{grp\\}", out
  ))
  # Priors: distinct intercept and slope SDs.
  expect_true(grepl(
    "\\\\sigma\\^\\{\\(\\\\alpha\\)\\}_\\{grp\\}", out
  ))
  expect_true(grepl(
    "\\\\sigma\\^\\{\\(\\\\beta_\\{x\\}\\)\\}_\\{grp\\}", out
  ))
})

test_that("me(x, sdx) renders observation + latent + hyperprior layers", {
  set.seed(1L)
  dat <- data.frame(
    time = rep(1:30, 4),
    series = factor(rep(paste0("s", 1:4), each = 30)),
    x = rnorm(120), sdx = abs(rnorm(120, 0.2, 0.05)),
    y = rpois(120, 3)
  )
  mod <- make_methods_md_prefit(y ~ me(x, sdx), data = dat)
  out <- methods_md(mod)
  # Linear predictor uses latent tilde{x}, not the noisy x_{i,t}.
  expect_true(grepl(
    "\\\\beta\\^\\{\\(\\\\text\\{me\\}\\)\\}_\\{x\\} \\\\, \\\\tilde\\{x\\}_\\{i,t\\}",
    out
  ))
  # Observation layer: noisy x ~ Normal(tilde{x}, sdx).
  expect_true(grepl(
    "x_\\{i,t\\} &\\\\sim \\\\text\\{Normal\\}.*\\\\tilde\\{x\\}_\\{i,t\\}.*sdx_\\{i,t\\}",
    out
  ))
  # Latent layer: tilde{x} ~ Normal(hyper-mean, hyper-SD).
  expect_true(grepl(
    "\\\\tilde\\{x\\}_\\{i,t\\} &\\\\sim \\\\text\\{Normal\\}.*\\\\mu\\^\\{\\(\\\\text\\{me\\}\\)\\}_\\{x\\}.*\\\\sigma\\^\\{\\(\\\\text\\{me\\}\\)\\}_\\{x\\}",
    out
  ))
  # Glossary entry exists.
  expect_true(grepl(
    "latent true covariate underlying noisy observation", out
  ))
})

test_that("Monotonic mo(x) renders beta^{(mo)}_x m_x + Dirichlet simplex", {
  set.seed(1L)
  dat <- data.frame(
    time = rep(1:30, 4),
    series = factor(rep(paste0("s", 1:4), each = 30)),
    x = factor(sample(1:5, 120, replace = TRUE), ordered = TRUE),
    y = rpois(120, 3)
  )
  mod <- make_methods_md_prefit(y ~ mo(x), data = dat)
  out <- methods_md(mod)
  # Linear predictor uses the mo-specific magnitude * step
  # transform form, not the plain beta_{mox} x_{i,t}.
  expect_true(grepl(
    "\\\\beta\\^\\{\\(\\\\text\\{mo\\}\\)\\}_\\{x\\} \\\\, m_\\{x\\}",
    out
  ))
  expect_false(grepl("\\\\beta_\\{mox\\}", out))
  # Term def: cumulative step + Dirichlet simplex.
  expect_true(grepl("\\(D_\\{x\\} - 1\\) \\\\sum", out))
  expect_true(grepl(
    "\\\\boldsymbol\\{\\\\zeta\\}_\\{x\\} &\\\\sim \\\\text\\{Dirichlet\\}",
    out
  ))
  # Glossary line names the monotonic step transform.
  expect_true(grepl("monotonic step transform of ordinal", out))
})

test_that("dpar second-formula renders its own linear predictor", {
  set.seed(1L)
  dat <- data.frame(
    time = rep(1:30, 4),
    series = factor(rep(paste0("s", 1:4), each = 30)),
    x = rnorm(120), y = rnorm(120)
  )
  mod <- make_methods_md_prefit(
    brms::bf(y ~ x, sigma ~ x),
    data = dat, family = gaussian()
  )
  out <- methods_md(mod)
  # Sigma gets its own row, log-linked by default.
  expect_true(grepl(
    "\\\\log \\\\sigma_\\{i,t\\} &= \\\\alpha\\^\\{\\(sigma\\)\\}",
    out
  ))
  # `\\,` thin space between coef token and the data-column
  # symbol; shared with the nl per-nlpar predictor format.
  expect_true(grepl(
    "\\\\beta_\\{sigma,x\\} \\\\, x_\\{i,t\\}", out
  ))
  # Priors block carries the distinct alpha^{(sigma)}.
  expect_true(grepl(
    "\\\\alpha\\^\\{\\(sigma\\)\\} &\\\\sim", out
  ))
})

test_that("nl formula renders per-nlpar decompositions", {
  set.seed(1L)
  dat <- data.frame(
    time = rep(1:20, 4),
    series = factor(rep(paste0("s", 1:4), each = 20)),
    env = rnorm(80),
    trait1 = rep(rnorm(4), each = 20),
    y = rnorm(80)
  )
  f <- brms::bf(y ~ a + b * env,
                a + b ~ trait1,
                nl = TRUE)
  mod <- suppressWarnings(suppressMessages(mvgam(
    formula = f, data = dat, family = gaussian(),
    run_model = FALSE, silent = 2L
  )))
  out <- methods_md(mod)
  # Top-level mu uses nlpar tokens verbatim with (i, t) subscripts.
  expect_true(grepl("a_\\{i,t\\}", out))
  expect_true(grepl("b_\\{i,t\\}", out))
  expect_true(grepl("env_\\{i,t\\}", out))
  # Each nlpar gets its own decomposition row with alpha^{(np)}
  # and beta_{np,trait1}.
  expect_true(grepl(
    "a_\\{i,t\\} &= \\\\alpha\\^\\{\\(a\\)\\}", out
  ))
  expect_true(grepl(
    "b_\\{i,t\\} &= \\\\alpha\\^\\{\\(b\\)\\}", out
  ))
  expect_true(grepl(
    "\\\\beta_\\{a,trait1\\} \\\\, trait1_\\{i,t\\}", out
  ))
  expect_true(grepl(
    "\\\\beta_\\{b,trait1\\} \\\\, trait1_\\{i,t\\}", out
  ))
})

test_that("Response column never surfaces in the Predictors list", {
  set.seed(1L)
  dat <- data.frame(
    time = rep(1:30, 4),
    series = factor(rep(paste0("s", 1:4), each = 30)),
    x = rnorm(120),
    grp = factor(rep(c("a","b","c","d"), each = 30)),
    y = rpois(120, 3)
  )
  mod <- make_methods_md_prefit(y ~ x, data = dat)
  out <- methods_md(mod)
  # The Predictors bullet list must not include "y" as a covariate.
  expect_false(grepl("- \\$y\\$:", out))
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
  expect_equal(ft("gamma", mu, NULL),
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
  expect_equal(fmt("lkj_corr_cholesky(1)"), "\\text{LKJCholesky}(1)")
  expect_equal(fmt("(flat)"), "\\text{flat}")
  expect_equal(fmt(""), "\\text{flat}")
})


# ---------------------------------------------------------------
# Phase C: latent branches (has_cor / gr) + factor-model block
# ---------------------------------------------------------------

test_that("AR(cor = TRUE) emits MVNormal innovations", {
  mod <- make_methods_md_prefit(
    y ~ x, trend_formula = ~ AR(p = 1, cor = TRUE)
  )
  out <- methods_md(mod)
  # Vector epsilon row with MVNormal Sigma (cross-series cor).
  expect_true(grepl(
    "\\\\boldsymbol\\{\\\\epsilon\\}_t &\\\\sim \\\\text\\{MVNormal\\}",
    out
  ))
  expect_false(grepl(
    "\\\\epsilon\\^\\{\\(\\\\eta\\)\\}_\\{i,t\\} &\\\\sim \\\\text\\{Normal\\}",
    out
  ))
})

test_that("AR(gr = ...) emits hierarchical cor decomposition", {
  dat <- data.frame(
    time = rep(1:30, 4),
    series = factor(rep(paste0("s", 1:4), each = 30)),
    region = factor(rep(c("r1", "r1", "r2", "r2"), each = 30)),
    x = rnorm(120),
    y = rpois(120, lambda = 3)
  )
  mod <- make_methods_md_prefit(
    y ~ x, trend_formula = ~ AR(p = 1, gr = region, subgr = series),
    data = dat
  )
  out <- methods_md(mod)
  # Hierarchical cor decomposition row present.
  expect_true(grepl("\\\\boldsymbol\\{\\\\Omega\\}_\\{region\\}", out))
  expect_true(grepl("\\\\alpha_\\{cor\\}", out))
  expect_true(grepl(
    "\\\\boldsymbol\\{\\\\Omega\\}_\\{\\\\text\\{global\\}\\}",
    out
  ))
  # gr -> cor = TRUE so innovations are MVNormal.
  expect_true(grepl(
    "\\\\boldsymbol\\{\\\\epsilon\\}_t &\\\\sim \\\\text\\{MVNormal\\}",
    out
  ))
})

test_that("Factor model (n_lv > 0) emits decomposition + iid Z + QR", {
  mod <- make_methods_md_prefit(
    y ~ x, trend_formula = ~ AR(p = 1, n_lv = 2)
  )
  out <- methods_md(mod)
  # Loading decomposition.
  expect_true(grepl(
    "\\\\sum_\\{k=1\\}\\^\\{2\\} Z_\\{i,k\\} \\\\tilde\\\\eta_\\{k,t\\}",
    out
  ))
  # iid default Z prior.
  expect_true(grepl(
    "Z_\\{i,k\\} &\\\\sim \\\\text\\{Student-t\\}\\(3, 0, 0\\.5\\)",
    out
  ))
  # In factor mode the latent dynamics are on tilde-eta_{k,t}.
  # The post-hoc thin-QR rotation of (Z, tilde-eta) is
  # post-processing -- it must NOT appear in the model section.
  expect_true(grepl(
    "\\\\tilde\\\\eta_\\{k,t\\}",
    out
  ))
  expect_false(grepl("thin QR", out))
  expect_false(grepl("\\\\tilde Z", out))
})

test_that("MGP loadings_prior emits varrho + Psi rows + Normal(0, sqrt(Psi))", {
  # n_lv = 2 forces a non-trivial column shrinkage.
  mod <- make_methods_md_prefit(
    y ~ x, trend_formula = ~ AR(p = 1, n_lv = 2)
  )
  # Inject the MGP loadings spec on the prefit's trend spec (the
  # canonical pre-fit location) so the renderer dispatches to the
  # MGP branch without needing a full Stan run.
  mod$mv_spec$trend_specs$loadings_prior_spec <- list(
    features_mat   = NULL,
    distance_mats  = list(),
    column_shrinkage = "mgp",
    mgp_a1 = 2, mgp_a2 = 3,
    n_series = 4L, N_features_trend = 0L, n_distances = 0L
  )
  out <- methods_md(mod)
  expect_true(grepl(
    "\\\\varrho_1 &\\\\sim \\\\text\\{InvGamma\\}\\(a_1, 1\\)", out
  ))
  expect_true(grepl(
    "\\\\Psi_k &= \\\\prod_\\{l \\\\le k\\} \\\\varrho_l", out
  ))
  expect_true(grepl(
    "Z_\\{i,k\\} &\\\\sim \\\\text\\{Normal\\}\\(0, \\\\sqrt\\{\\\\Psi_k\\}\\)",
    out
  ))
})

test_that("loadings_prior features + distances kernel emits kernel rows", {
  mod <- make_methods_md_prefit(
    y ~ x, trend_formula = ~ AR(p = 1, n_lv = 2)
  )
  mod$mv_spec$trend_specs$loadings_prior_spec <- list(
    features_mat   = matrix(rnorm(4), nrow = 4L, ncol = 1L),
    distance_mats  = list(phylo = matrix(0, 4L, 4L)),
    column_shrinkage = "iid",
    mgp_a1 = NA, mgp_a2 = NA,
    n_series = 4L, N_features_trend = 1L, n_distances = 1L
  )
  out <- methods_md(mod)
  # Kernel assembly: distance term + features term, Hadamard
  # product joiner.
  expect_true(grepl("\\\\exp\\(-d_\\{phylo\\}", out))
  expect_true(grepl(
    "\\\\text\\{GP\\}_\\{\\\\text\\{exp\\}\\}", out
  ))
  expect_true(grepl("\\\\odot", out))
  expect_true(grepl("L_\\\\Phi", out))
  # Z prior is MVNormal(0, L_Phi L_Phi^T) for kernel-only branch.
  expect_true(grepl(
    "Z_\\{\\\\cdot,k\\} &\\\\sim \\\\text\\{MVNormal\\}", out
  ))
})


# ---------------------------------------------------------------
# Phase D: closure-unit + mv-custom family rendering
# ---------------------------------------------------------------

make_occ_prefit <- function() {
  set.seed(1L)
  d <- data.frame(
    series = factor(rep(seq_len(8), each = 4)),
    time = rep(1L, 32),
    visit = rep(seq_len(4), 8),
    y = rbinom(32, 1L, 0.4),
    elev = rep(rnorm(8), each = 4),
    tod = stats::runif(32)
  )
  suppressWarnings(suppressMessages(mvgam(
    formula = bf(y ~ elev, p ~ tod),
    data = d, family = occ(),
    run_model = FALSE, silent = 2
  )))
}

make_nmix_prefit <- function(type = "poisson_binomial") {
  set.seed(1L)
  d <- data.frame(
    series = factor(rep(seq_len(8), each = 4)),
    time = rep(1L, 32),
    visit = rep(seq_len(4), 8),
    y = if (type == "royle_nichols") {
      rbinom(32, 1L, 0.4)
    } else {
      rpois(32, 3)
    },
    cap = rep(10L, 32),
    elev = rep(rnorm(8), each = 4),
    tod = stats::runif(32)
  )
  suppressWarnings(suppressMessages(mvgam(
    formula = bf(y ~ elev, p ~ tod),
    data = d, family = nmix(type),
    run_model = FALSE, silent = 2
  )))
}

test_that("occ() emits state + obs + logit(p) rows", {
  out <- methods_md(make_occ_prefit())
  expect_true(grepl(
    "y_\\{i,j\\} \\\\mid z_\\{i\\} &\\\\sim \\\\text\\{Bernoulli\\}",
    out
  ))
  expect_true(grepl("z_\\{i\\} &\\\\sim \\\\text\\{Bernoulli\\}", out))
  expect_true(grepl("\\\\text\\{logit\\}\\(\\\\psi_\\{i\\}\\)", out))
  expect_true(grepl("\\\\text\\{logit\\}\\(p_\\{i,j\\}\\)", out))
  # Data section calls out the closure-unit grouping dims.
  expect_true(grepl("\\$G = 8\\$ closure units", out))
  expect_true(grepl("\\\\bar J", out))
})

test_that("nmix Poisson-binomial emits N | lambda + Binomial obs row", {
  out <- methods_md(make_nmix_prefit("poisson_binomial"))
  expect_true(grepl(
    "y_\\{i,j\\} \\\\mid N_\\{i\\} &\\\\sim \\\\text\\{Binomial\\}",
    out
  ))
  expect_true(grepl(
    "N_\\{i\\} &\\\\sim \\\\text\\{Poisson\\}\\(\\\\lambda_\\{i\\}\\)",
    out
  ))
  # Detection link is logit for PB.
  expect_true(grepl("\\\\text\\{logit\\}\\(p_\\{i,j\\}\\)", out))
})

test_that("nmix RN emits Bernoulli(1 - (1 - p)^N) obs row", {
  out <- methods_md(make_nmix_prefit("royle_nichols"))
  expect_true(grepl(
    "\\\\text\\{Bernoulli\\}\\(1 - \\(1 - p_\\{i,j\\}\\)\\^\\{N_\\{i\\}\\}\\)",
    out
  ))
})

test_that("nmix PPM emits Poisson(N * p) and log(p) link", {
  out <- methods_md(make_nmix_prefit("poisson_poisson"))
  expect_true(grepl(
    "\\\\text\\{Poisson\\}\\(N_\\{i\\} \\\\cdot p_\\{i,j\\}\\)",
    out
  ))
  # PPM detection link is log per R/families.R::nmix_poisson_poisson.
  expect_true(grepl("\\\\log p_\\{i,j\\}", out))
})

test_that("diri() emits Dirichlet + alpha = phi * pi + softmax row", {
  set.seed(1L)
  K <- 3L; n_sites <- 5L
  d <- expand.grid(
    time = 1:n_sites, series = factor(paste0("sp", 1:K))
  )
  d$env <- rep(rnorm(n_sites), times = K)
  d$y <- runif(n_sites * K)
  d$y <- d$y / tapply(d$y, d$time, sum)[match(d$time, names(tapply(d$y, d$time, sum)))]
  mod <- suppressWarnings(suppressMessages(jsdgam(
    formula = y ~ env * series, factor_formula = ~ -1,
    data = d, unit = time, species = series,
    family = diri(), n_lv = 2L,
    run_model = FALSE, silent = 2, backend = "cmdstanr"
  )))
  out <- methods_md(mod)
  expect_true(grepl(
    "\\\\mathbf\\{Y\\}_i &\\\\sim \\\\text\\{Dirichlet\\}",
    out
  ))
  expect_true(grepl(
    "\\\\boldsymbol\\{\\\\alpha\\}_i &= \\\\phi", out
  ))
  expect_true(grepl(
    "\\\\boldsymbol\\{\\\\pi\\}_i &= \\\\text\\{softmax\\}",
    out
  ))
})

test_that("nl b_<nlpar>_<term> prior carries nlpar superscript on beta", {
  set.seed(1L)
  d <- data.frame(
    time = rep(1:30, 4), series = factor(rep(1:4, each = 30)),
    x = rnorm(120),
    trait1 = rep(rnorm(4), each = 30),
    yC = rnorm(120)
  )
  # Explicit nlpar priors so the prior table carries the rows the
  # renderer needs (default flat priors are filtered out).
  mod <- suppressWarnings(suppressMessages(mvgam(
    formula = bf(yC ~ a + b * x, a + b ~ trait1, nl = TRUE),
    data = d, family = gaussian(),
    prior = c(
      brms::prior(normal(0, 1), nlpar = "a"),
      brms::prior(normal(0, 1), nlpar = "b")
    ),
    run_model = FALSE, silent = 2
  )))
  out <- methods_md(mod)
  expect_true(grepl("\\\\beta\\^\\{\\(a\\)\\}_\\{trait1\\}", out))
  expect_true(grepl("\\\\beta\\^\\{\\(b\\)\\}_\\{trait1\\}", out))
})

test_that("Ordinal Intercept rows render as theta_{k} thresholds", {
  set.seed(1L)
  d <- data.frame(
    time = rep(1:30, 4), series = factor(rep(1:4, each = 30)),
    x = rnorm(120),
    yord = factor(sample(1:5, 120, replace = TRUE), ordered = TRUE)
  )
  mod <- suppressWarnings(suppressMessages(mvgam(
    formula = yord ~ x,
    data = d, family = cumulative(),
    run_model = FALSE, silent = 2
  )))
  out <- methods_md(mod)
  expect_true(grepl("\\\\theta_\\{1\\}", out) ||
              grepl("\\\\theta_\\{[0-9]+\\}", out))
})

test_that("mvbind rescor priors carry response superscripts", {
  set.seed(1L)
  d <- data.frame(
    time = rep(1:30, 2), series = factor(rep(1:2, each = 30)),
    x = rnorm(60), yA = rnorm(60), yB = rnorm(60)
  )
  mod <- suppressWarnings(suppressMessages(mvgam(
    formula = brms::bf(brms::mvbind(yA, yB) ~ x) +
      brms::set_rescor(TRUE),
    data = d, family = gaussian(),
    run_model = FALSE, silent = 2
  )))
  out <- methods_md(mod)
  expect_true(grepl("\\\\sigma\\^\\{\\(yA\\)\\}", out))
  expect_true(grepl("\\\\sigma\\^\\{\\(yB\\)\\}", out))
  expect_true(grepl("\\\\mathbf\\{L\\}_\\{\\\\text\\{rescor\\}\\}", out))
  expect_true(grepl("\\\\text\\{LKJCholesky\\}", out))
})

test_that("mvn() emits MVNormal + Sigma decomposition + LKJCholesky", {
  set.seed(1L)
  K <- 3L; n_sites <- 6L
  d <- expand.grid(
    time = 1:n_sites, series = factor(paste0("sp", 1:K))
  )
  d$env <- rep(rnorm(n_sites), times = K)
  d$y <- rnorm(n_sites * K)
  mod <- suppressWarnings(suppressMessages(jsdgam(
    formula = y ~ env, factor_formula = ~ -1,
    data = d, unit = time, species = series,
    family = mvn(), n_lv = 1L,
    run_model = FALSE, silent = 2, backend = "cmdstanr"
  )))
  out <- methods_md(mod)
  expect_true(grepl(
    "\\\\mathbf\\{Y\\}_i &\\\\sim \\\\text\\{MVNormal\\}",
    out
  ))
  # The kernel evaluates independent normals with a per-element
  # scale, so the covariance is diagonal: rendering a Cholesky
  # decomposition under an LKJ prior would name a correlation this
  # family's Sigma has no parameter for. The equation and the
  # symbol glossary describe the same quantity and must agree.
  expect_true(grepl(
    "\\\\text\\{diag\\}\\(\\\\boldsymbol\\{\\\\Psi\\}\\^2\\)", out
  ))
  expect_false(grepl("L_\\\\Omega L_\\\\Omega\\^\\\\top", out))
  # The trend's own correlation is a different matter: this fit
  # samples `L_Omega_trend ~ lkj_corr_cholesky(2)`, so the prior
  # table reports it and the write-up renders it.
  expect_true(grepl("LKJCholesky", out))
})


test_that("a moving-average trend is named as one", {
  # `AR(ma = TRUE)` is recorded under trend type "AR", so nothing
  # ever stores the spelling "ARMA". Keying on that spelling meant
  # the order label and the coefficient definition both described
  # the AR model underneath, and an ARMA fit printed identically to
  # the plain AR fit beside it.
  fake <- function(tt, ar, ma) {
    structure(
      list(trend_metadata = list(trend_type = tt, ar_lags = ar,
                                 ma_lags = ma)),
      class = "mvgam"
    )
  }
  expect_identical(
    mvgam:::trend_order_label(fake("AR", 1L, integer(0))), "AR(1)"
  )
  expect_identical(
    mvgam:::trend_order_label(fake("AR", 1L, 1L)), "ARMA(1, 1)"
  )
  expect_identical(
    mvgam:::trend_order_label(fake("VAR", 1L, integer(0))), "VAR(1)"
  )
  expect_identical(
    mvgam:::trend_order_label(fake("VAR", 1L, 1L)), "VARMA(1, 1)"
  )
  # The two the label has to tell apart do not collide.
  expect_false(identical(
    mvgam:::trend_order_label(fake("AR", 1L, integer(0))),
    mvgam:::trend_order_label(fake("AR", 1L, 1L))
  ))
  expect_true(mvgam:::trend_has_ma(fake("AR", 1L, 1L)))
  expect_false(mvgam:::trend_has_ma(fake("AR", 1L, integer(0))))
})


test_that("a gamma fit is described as one", {
  # `resolve_family_name()` names `stats::Gamma()` "gamma", and the
  # description tables keyed on "Gamma", which no family resolves to.
  # A gamma fit fell through to the default text in all three.
  d <- data.frame(
    time = rep(1:30, 2), series = factor(rep(c("a", "b"), each = 30)),
    x = rnorm(60), y = rgamma(60, 2, 1)
  )
  mod <- make_methods_md_prefit(y ~ x, family = Gamma(link = "log"),
                                data = d)
  out <- methods_md(mod)
  expect_match(out, "positive real observations", fixed = TRUE)
  expect_match(out, "\\text{Gamma}(", fixed = TRUE)
  expect_identical(mvgam:::family_call_text(mod$family),
                   "gamma(link = \"log\")")
})


test_that("an unfitted model prints each family by its own name", {
  # `print()` on a `run_model = FALSE` model reached the fitted printer
  # and stopped on the missing draws. The unfitted printer it was meant
  # to reach read `family$family`, which brms writes as "custom" for
  # every family built with `custom_family()`, and printed one family
  # for a model with several responses.
  d <- data.frame(time = 1:30, series = factor("s1"),
                  y = rgamma(30, 2, 1))
  pf <- mvgam(y ~ 1, data = d, family = tweedie(), run_model = FALSE)
  printed <- capture.output(print(pf))
  expect_true(any(grepl("Family: tweedie", printed, fixed = TRUE)))
  expect_false(any(grepl("custom", printed, fixed = TRUE)))

  w <- data.frame(time = 1:20, count = rpois(20, 4),
                  seen = rbinom(20, 1, 0.5))
  mv <- mvgam(
    brms::bf(count ~ 1, family = poisson()) +
      brms::bf(seen ~ 1, family = bernoulli()) + brms::set_rescor(FALSE),
    trend_formula = ~ AR(p = 1), data = w, run_model = FALSE
  )
  printed <- capture.output(print(mv))
  expect_true(any(grepl("count: poisson", printed, fixed = TRUE)))
  expect_true(any(grepl("seen: bernoulli", printed, fixed = TRUE)))
})
