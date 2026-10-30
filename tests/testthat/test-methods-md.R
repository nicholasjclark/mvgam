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
  mvgam(
    formula       = formula,
    trend_formula = trend_formula,
    data          = data,
    family        = family,
    run_model     = FALSE,
    silent        = 2,
    validate      = FALSE
  )
}

# The lines of a page, each equation row without its line break
md_rows <- function(out) {
  sub(" \\\\\\\\$", "", strsplit(as.character(out), "\n")[[1L]])
}

# The `y ~ x` Poisson prefit most tests describe, built once
base_prefit <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) cached <<- make_methods_md_prefit(y ~ x)
    cached
  }
})

test_that("the software versions reported are the ones the fit recorded", {
  # The page reports the versions the fit recorded. The describing
  # session may have others installed. Under rstan the Stan version is
  # Stan's own, distinct from the rstan package version.
  mod <- base_prefit()
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
  rows <- md_rows(methods_md(base_prefit()))
  expect_contains(rows, c(
    "$y$: non-negative integer counts",
    "$N = 120$ observations, $S = 4$ series, $T = 30$ time points"
  ))
})

test_that("Model section emits the likelihood and its link", {
  expect_contains(md_rows(methods_md(base_prefit())), c(
    "y_{i,t} &\\sim \\text{Poisson}(\\mu_{i,t})",
    "\\log \\mu_{i,t} &= \\alpha + \\beta_{x} x_{i,t}"
  ))
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
  # The identity link writes the mean itself. A model without a trend
  # has no eta in its predictor.
  expect_contains(md_rows(methods_md(mod)), c(
    "$y$: real-valued observations",
    "y_{i,t} &\\sim \\text{Normal}(\\mu_{i,t}, \\sigma)",
    "\\mu_{i,t} &= \\alpha + \\beta_{x} x_{i,t}"
  ))
})

test_that("RW trend emits eta_{t-1} dynamics + Normal innovation", {
  mod <- make_methods_md_prefit(y ~ 1, trend_formula = ~ RW())
  # Four series each sample their own innovation SD.
  expect_contains(md_rows(methods_md(mod)), c(
    "\\eta_{i,t} &= \\eta_{i,t-1} + \\epsilon^{(\\eta)}_{i,t}",
    "\\epsilon^{(\\eta)}_{i,t} &\\sim \\text{Normal}(0, \\sigma_{\\eta,i})"
  ))
  ma <- methods_md(make_methods_md_prefit(
    y ~ 1, trend_formula = ~ RW(ma = TRUE)
  ))
  expect_contains(md_rows(ma), paste0(
    "\\eta_{i,t} &= \\eta_{i,t-1} + \\epsilon^{(\\eta)}_{i,t} + ",
    "\\theta_{i,1} \\epsilon^{(\\eta)}_{i,t-1}"
  ))
})

test_that("AR(p = 1) trend emits phi_1 eta_{t-1} dynamics", {
  mod <- make_methods_md_prefit(y ~ 1, trend_formula = ~ AR(p = 1))
  expect_contains(
    md_rows(methods_md(mod)),
    "\\eta_{i,t} &= \\phi_{i,1} \\eta_{i,t-1} + \\epsilon^{(\\eta)}_{i,t}"
  )
  # A shared coefficient is one value for every series.
  shared <- methods_md(make_methods_md_prefit(
    y ~ 1, trend_formula = ~ AR(p = 1, coef_sharing = "shared")
  ))
  expect_contains(
    md_rows(shared),
    "\\eta_{i,t} &= \\phi_{1} \\eta_{i,t-1} + \\epsilon^{(\\eta)}_{i,t}"
  )
  expect_match(shared, "\\phi_{1} &\\sim", fixed = TRUE)
})

test_that("AR(p = c(1, 2)) emits both lag terms in the dynamics row", {
  mod <- make_methods_md_prefit(
    y ~ 1, trend_formula = ~ AR(p = c(1L, 2L))
  )
  expect_contains(md_rows(methods_md(mod)), paste0(
    "\\eta_{i,t} &= \\phi_{i,1} \\eta_{i,t-1} + \\phi_{i,2} \\eta_{i,t-2} + ",
    "\\epsilon^{(\\eta)}_{i,t}"
  ))
  # A hierarchical fit's coefficient prior names its hyperparameters.
  hier <- methods_md(make_methods_md_prefit(
    y ~ 1, trend_formula = ~ AR(p = c(1L, 3L), coef_sharing = "hierarchical")
  ))
  expect_match(hier, "\\phi_{i,3} &\\sim \\text{Normal}(\\mu^{(\\phi)}_{3}, ",
               fixed = TRUE)
})

test_that("a seasonal LLT writes the level plus each harmonic's turn", {
  mod <- make_methods_md_prefit(
    y ~ 1, trend_formula = ~ LLT(season = 12, harmonics = 2)
  )
  out <- methods_md(mod)
  expect_contains(md_rows(out), c(
    "\\eta_{i,t} &= \\ell_{i,t} + \\gamma_{i,t}",
    paste0("\\ell_{i,t} &= \\ell_{i,t-1} + \\beta_{i,t-1} + ",
           "\\epsilon^{(\\eta)}_{i,t}"),
    "\\gamma_{i,t} &= \\sum_{j=1}^{K} \\gamma_{j,i,t}",
    paste0("\\gamma_{j,i,t} &= \\cos(\\lambda_j) \\gamma_{j,i,t-1} + ",
           "\\sin(\\lambda_j) \\gamma^*_{j,i,t-1} + \\omega_{j,i,t}"),
    paste0("\\gamma^*_{j,i,t} &= \\cos(\\lambda_j) \\gamma^*_{j,i,t-1} - ",
           "\\sin(\\lambda_j) \\gamma_{j,i,t-1} + \\omega^*_{j,i,t}")
  ))
  expect_match(out, "for $j = 1, \\dots, 2$ of period 12", fixed = TRUE)
  expect_true("delivera_trig" %in% names(how_to_cite(mod)$bibtex))
})

test_that("Smooth term renders f_{x}(x) inline + basis decomposition", {
  mod <- make_methods_md_prefit(y ~ s(x, k = 5))
  rows <- md_rows(methods_md(mod))
  expect_contains(rows, c(
    "\\log \\mu_{i,t} &= \\alpha + f_{x}(x_{i,t})",
    "f_{x}(x) &= \\sum_{m=1}^{K_{x}} \\beta^{(x)}_m B_m(x)"
  ))
  expect_true(any(startsWith(rows, "\\sigma^{(s)}_{x} &\\sim")))
  # A random effect basis is one normal coefficient per level
  re <- md_rows(methods_md(make_methods_md_prefit(y ~ s(grp, bs = "re"))))
  expect_contains(re, c(
    paste0("f_{\\mathrm{grp}}(\\mathrm{grp}) &= ",
           "\\beta^{(\\mathrm{grp})}_{\\mathrm{grp}}"),
    paste0("\\beta^{(\\mathrm{grp})}_m &\\sim ",
           "\\text{Normal}(0, \\sigma^{(s)}_{\\mathrm{grp}})")
  ))
})

test_that("an approximate GP names its kernel, length scale and SD", {
  out <- methods_md(make_methods_md_prefit(y ~ gp(x, k = 5)))
  expect_contains(md_rows(out), paste0(
    "f^{(\\text{gp})}_{x}(x) &\\sim \\text{GP}\\left(0, ",
    "k_{\\text{ExpQuad}}(\\rho_{x}, \\sigma^{(\\text{gp})}_{x})\\right)"
  ))
  expect_match(out, "approximated with 5 basis functions", fixed = TRUE)
})

test_that("Exact GP (no k) renders as 'exact (full covariance kernel)'", {
  out <- methods_md(make_methods_md_prefit(y ~ gp(x)))
  expect_match(out, "exact (full covariance kernel)", fixed = TRUE)
  expect_no_match(out, "approximated with", fixed = TRUE)
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
  expect_match(out, "\\boldsymbol{\\rho}_{x, z}", fixed = TRUE)
  expect_match(out, "Gaussian process in $x, z$", fixed = TRUE)
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
  expect_match(out, "x \\mid \\mathrm{grp}", fixed = TRUE)
  expect_match(out, "stratified by $\\mathrm{grp}$", fixed = TRUE)
})

test_that("Terms render in the predictor they belong to", {
  set.seed(1L)
  dat <- data.frame(
    time = rep(1:30, 4),
    series = factor(rep(paste0("s", 1:4), each = 30)),
    x = rnorm(120),
    grp = factor(rep(c("a", "b", "c", "d"), each = 30)),
    y = rnorm(120)
  )
  # A by-factor smooth is one smooth per level of its by variable
  out <- methods_md(make_methods_md_prefit(
    y ~ s(x, by = grp, k = 5), data = dat, family = gaussian()
  ))
  expect_match(out, "f_{x \\mid \\mathrm{grp}}", fixed = TRUE)
  expect_match(out, "stratified by $\\mathrm{grp}$", fixed = TRUE)
  # A random effect on sigma stays out of the mean's predictor
  out <- methods_md(make_methods_md_prefit(
    brms::bf(y ~ x, sigma ~ (1 | grp)), data = dat, family = gaussian()
  ))
  expect_no_match(out, "\\alpha_{\\mathrm{grp}[i]}", fixed = TRUE)
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
  mod <- make_methods_md_prefit(y ~ t2(x, z, k = c(4, 5)), data = dat)
  out <- methods_md(mod)
  # Linear predictor has both vars indexed
  expect_match(out, "f_{x, z}(x_{i,t}, z_{i,t})", fixed = TRUE)
  # Term definition uses the joined key in basis-size / coef subscripts
  expect_match(out, "\\sum_{m=1}^{K_{x:z}}", fixed = TRUE)
  expect_match(out, "\\beta^{(x:z)}_m B_m(x, z)", fixed = TRUE)
  # The glossary names the tensor basis and one basis size per margin
  expect_match(out, paste0("tensor product smooth (t2) in $x$, $z$, basis ",
                           "size $K_{x:z} = 4 \\times 5$"), fixed = TRUE)
})

test_that("gp_call_to_spec errors on a malformed call with no variables", {
  # A gp() term needs a variable. One with named arguments alone is
  # refused.
  bad_call <- str2lang("gp(k = 5)")
  expect_error(
    mvgam:::gp_call_to_spec(bad_call),
    "needs at least one variable"
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
  rows <- md_rows(methods_md(mod))
  sds <- paste0("\\sigma^{(\\alpha)}_{\\mathrm{grp}}, ",
                "\\sigma^{(\\beta_{x})}_{\\mathrm{grp}}")
  expect_contains(rows, c(
    paste0("\\log \\mu_{i,t} &= \\alpha + \\beta_{x} x_{i,t} + ",
           "\\alpha_{\\mathrm{grp}[i]} + ",
           "\\beta^{(\\mathrm{grp})}_{x, \\mathrm{grp}[i]} x_{i,t}"),
    paste0("(\\alpha_{\\mathrm{grp}}, \\beta^{(\\mathrm{grp})}_{x, ",
           "\\mathrm{grp}})^\\top &\\sim \\text{MVNormal}\\!\\left(",
           "\\mathbf{0}, \\boldsymbol{\\Sigma}_{\\mathrm{grp}}\\right)"),
    paste0("\\boldsymbol{\\Sigma}_{\\mathrm{grp}} &= \\text{diag}(", sds,
           ") \\, \\boldsymbol{\\Omega}_{\\mathrm{grp}} \\, \\text{diag}(",
           sds, ")"),
    "\\boldsymbol{\\Omega}_{\\mathrm{grp}} &\\sim \\text{LKJCorr}(1)"
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
  # The linear predictor takes the latent covariate. The observed
  # covariate centres on it. The latent covariate has a population mean
  # and SD.
  expect_contains(md_rows(methods_md(mod)), c(
    paste0("\\log \\mu_{i,t} &= \\alpha + ",
           "\\beta^{(\\text{me})}_{x} \\, \\tilde{x}_{i,t}"),
    paste0("x_{i,t} &\\sim \\text{Normal}\\!\\left(\\tilde{x}_{i,t}, ",
           "\\mathrm{sdx}_{i,t}\\right)"),
    paste0("\\tilde{x}_{i,t} &\\sim \\text{Normal}\\!\\left(",
           "\\mu^{(\\text{me})}_{x}, \\sigma^{(\\text{me})}_{x}\\right)")
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
  # The linear predictor takes the effect times the step transform.
  # brms's coefficient name `mox` never appears.
  expect_contains(md_rows(out), c(
    paste0("\\log \\mu_{i,t} &= \\alpha + ",
           "\\beta^{(\\text{mo})}_{x} \\, m_{x}(x_{i,t})"),
    "m_{x}(x) &= (D_{x} - 1) \\sum_{j=1}^{x} \\zeta_{x,j}",
    "\\boldsymbol{\\zeta}_{x} &\\sim \\text{Dirichlet}(1)"
  ))
  expect_no_match(out, "mox", fixed = TRUE)
})

test_that("each special coefficient renders its own parts", {
  set.seed(1L)
  dat <- data.frame(
    time = rep(1:30, 4),
    series = factor(rep(paste0("s", 1:4), each = 30)),
    z = rnorm(120), x = rnorm(120), sdx = 0.1,
    m = factor(sample(1:4, 120, replace = TRUE), ordered = TRUE),
    f = factor(rep(c("hi", "lo"), 60)), g = factor(rep(letters[1:3], 40)),
    y = rpois(120, 3)
  )
  # `mo(m):f` takes the indicator of the level its coefficient names.
  # Each coefficient has its own simplex. The varying slope multiplies
  # the same transform as its population term.
  mod <- make_methods_md_prefit(
    y ~ mo(m):z + mo(m):f + me(x, sdx):mo(m) + (mo(m) | g) + mo(m),
    data = dat
  )
  rows <- md_rows(methods_md(mod))
  expect_contains(rows, paste0(
    "\\log \\mu_{i,t} &= \\alpha + ",
    "\\beta^{(\\text{mo})}_{m} \\, m_{m}(m_{i,t}) + ",
    "\\beta^{(\\text{mo})}_{m:z} \\, m_{m:z}(m_{i,t}) \\, z_{i,t} + ",
    "\\beta^{(\\text{mo})}_{m:f[\\mathrm{lo}]} \\, ",
    "m_{m:f[\\mathrm{lo}]}(m_{i,t}) \\, \\mathbb{1}[f_{i,t} = \\mathrm{lo}] + ",
    "\\beta^{(\\text{sp})}_{m:x} \\, m_{m:x}(m_{i,t}) \\, \\tilde{x}_{i,t} + ",
    "\\alpha_{g[i]} + \\beta^{(g)}_{m, g[i]} m_{m}(m_{i,t})"
  ))
  expect_contains(rows, "\\boldsymbol{\\zeta}_{m:x} &\\sim \\text{Dirichlet}(1)")

  # A factor's varying slope takes the indicator of its level
  mod <- make_methods_md_prefit(y ~ z + (1 + f | g), data = dat)
  out <- methods_md(mod)
  expect_match(out, paste0("\\beta^{(g)}_{f[\\mathrm{lo}], g[i]} ",
                           "\\mathbb{1}[f_{i,t} = \\mathrm{lo}]"), fixed = TRUE)
  expect_match(out, "- $f$: factor with 2 levels", fixed = TRUE)

  # A grouped measurement-error term has one latent value per level
  dat$x <- c(-1, 0, 1)[as.integer(dat$g)]
  mod <- make_methods_md_prefit(y ~ me(x, sdx, gr = g), data = dat)
  expect_contains(md_rows(methods_md(mod)), c(
    paste0("\\log \\mu_{i,t} &= \\alpha + ",
           "\\beta^{(\\text{me})}_{x} \\, \\tilde{x}_{g[i]}"),
    paste0("\\tilde{x}_{g} &\\sim \\text{Normal}\\!\\left(",
           "\\mu^{(\\text{me})}_{x}, \\sigma^{(\\text{me})}_{x}\\right)")
  ))
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
  # Sigma takes its own log-linked row. Its symbols carry the
  # parameter as a tag, in the equation and in the priors. Its
  # intercept is named as the predictor's.
  rows <- md_rows(methods_md(mod))
  expect_contains(rows, c(
    paste0("\\log \\sigma_{i,t} &= \\alpha^{(\\mathrm{sigma})} + ",
           "\\beta^{(\\mathrm{sigma})}_{x} x_{i,t}"),
    paste0("- $\\alpha^{(\\mathrm{sigma})}$: population intercept of the ",
           "predictor of $\\sigma$")
  ))
  expect_true(any(startsWith(rows, "\\alpha^{(\\mathrm{sigma})} &\\sim")))
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
  f <- brms::bf(y ~ a + exp(b * env),
                a + b ~ trait1,
                nl = TRUE)
  mod <- mvgam(
    formula = f, data = dat, family = gaussian(),
    run_model = FALSE, silent = 2L, validate = FALSE
  )
  # The mean is the formula as written, indexed, with `exp()` set as
  # LaTeX's function. Each non-linear parameter takes its own predictor.
  expect_contains(md_rows(methods_md(mod)), c(
    "\\mu_{i,t} &= a_{i,t} + \\exp(b_{i,t} \\, \\mathrm{env}_{i,t})",
    paste0("a_{i,t} &= \\alpha^{(a)} + ",
           "\\beta^{(a)}_{\\mathrm{trait1}} \\mathrm{trait1}_{i,t}"),
    paste0("b_{i,t} &= \\alpha^{(b)} + ",
           "\\beta^{(b)}_{\\mathrm{trait1}} \\mathrm{trait1}_{i,t}")
  ))
})

test_that("Response column never surfaces in the Predictors list", {
  out <- methods_md(base_prefit())
  # The Predictors bullet list must not include "y" as a covariate.
  expect_no_match(out, "- $y$:", fixed = TRUE)
})

test_that("Group-level RE renders alpha_{grp[i]} + hyperprior shape", {
  mod <- make_methods_md_prefit(y ~ (1 | grp))
  # The class's SD prior is stated on the group's own row, with no
  # placeholder symbol for the class.
  out <- methods_md(mod)
  rows <- md_rows(out)
  expect_contains(rows, c(
    "\\log \\mu_{i,t} &= \\alpha + \\alpha_{\\mathrm{grp}[i]}",
    "\\alpha_{\\mathrm{grp}} &\\sim \\text{Normal}(0, \\sigma_{\\mathrm{grp}})"
  ))
  expect_true(any(startsWith(rows, "\\sigma_{\\mathrm{grp}} &\\sim")))
  expect_no_match(out, "\\sigma_{j}", fixed = TRUE)
  # A block without an intercept adds none. A block written with `||`
  # has independent coefficients.
  rows <- md_rows(methods_md(make_methods_md_prefit(y ~ (0 + x | grp))))
  expect_contains(rows, paste0(
    "\\log \\mu_{i,t} &= \\alpha + ",
    "\\beta^{(\\mathrm{grp})}_{x, \\mathrm{grp}[i]} x_{i,t}"
  ))
  rows <- md_rows(methods_md(make_methods_md_prefit(y ~ (1 + x || grp))))
  expect_contains(rows, paste0(
    "\\beta^{(\\mathrm{grp})}_{x, \\mathrm{grp}} &\\sim ",
    "\\text{Normal}(0, \\sigma^{(\\beta_{x})}_{\\mathrm{grp}})"
  ))
  expect_false(any(grepl("MVNormal", rows, fixed = TRUE)))
})

test_that("methods_md(file = path) writes the markdown to disk", {
  mod <- base_prefit()
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp), add = TRUE)
  ret <- methods_md(mod, file = tmp)
  expect_identical(readLines(tmp), strsplit(as.character(ret), "\n")[[1L]])
  expect_s3_class(ret, "mvgam_methods_md")
})

# Prior distribution formatter: targeted unit tests on the
# regex-based string mapper. Easier to keep in sync with brms
# prior-string conventions when tested directly.

test_that("format_prior_distribution renders the arguments it is given", {
  fmt <- mvgam:::format_prior_distribution
  expect_equal(fmt("student_t(3, 0, 2.5)"), "\\text{StudentT}(3, 0, 2.5)")
  expect_equal(fmt("std_normal()"), "\\text{Normal}(0, 1)")
  expect_equal(fmt("(flat)"), "\\text{flat}")
  expect_equal(fmt(""), "\\text{flat}")
  # A full-precision location is shown to three significant digits
  expect_equal(fmt("normal(0.123456, 1)"), "\\text{Normal}(0.123, 1)")
  expect_equal(fmt("normal(0, sqrt(0.455))"),
               "\\text{Normal}(0, \\sqrt{0.455})")
  # A sampled hyperparameter takes its symbol. Another name is set
  # upright, with its underscore escaped.
  expect_equal(fmt("inv_gamma(mgp_a1, 1)", symbols = c(mgp_a1 = "a_1")),
               "\\text{InvGamma}(a_1, 1)")
  expect_equal(fmt("inv_gamma(mgp_a1, 1)"),
               "\\text{InvGamma}(\\mathrm{mgp\\_a1}, 1)")
  # An unrecognised distribution is rendered verbatim
  expect_equal(fmt("frechet(1, 2)"), "\\text{frechet(1, 2)}")
})


# ---------------------------------------------------------------
# Phase C: latent branches (has_cor / gr) + factor-model block
# ---------------------------------------------------------------

test_that("AR(cor = TRUE) emits MVNormal innovations", {
  mod <- make_methods_md_prefit(
    y ~ x, trend_formula = ~ AR(p = 1, cor = TRUE)
  )
  # The innovations are one correlated vector. No row gives each its
  # own Normal.
  out <- methods_md(mod)
  expect_contains(md_rows(out), c(
    paste0("\\boldsymbol{\\epsilon}_{t} &\\sim \\text{MVNormal}",
           "(\\mathbf{0}, \\boldsymbol{\\Sigma})"),
    paste0("\\boldsymbol{\\Sigma} &= \\text{diag}(\\boldsymbol{\\sigma}_\\eta)",
           " \\, \\boldsymbol{\\Omega} \\, ",
           "\\text{diag}(\\boldsymbol{\\sigma}_\\eta)"),
    # The glossary ties the SD vector to the entries the priors name
    paste0("- $\\boldsymbol{\\epsilon}_{t}$: process innovations at time ",
           "$t$, covariance $\\boldsymbol{\\Sigma}$, with ",
           "$\\boldsymbol{\\sigma}_\\eta$ the vector of SDs ",
           "$\\sigma_{\\eta,i}$")
  ))
  expect_no_match(out, "\\epsilon^{(\\eta)}_{i,t} &\\sim", fixed = TRUE)
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
  # Each region's correlation pools a global matrix with its own.
  expect_contains(md_rows(methods_md(mod)), c(
    paste0("\\boldsymbol{\\epsilon}_{t} &\\sim \\text{MVNormal}",
           "(\\mathbf{0}, \\boldsymbol{\\Sigma}_{\\mathrm{region}})"),
    paste0("\\boldsymbol{\\Omega}_{\\mathrm{region}} &= ",
           "\\alpha_{\\mathrm{cor}} \\boldsymbol{\\Omega}_{\\text{global}} + ",
           "(1 - \\alpha_{\\mathrm{cor}}) ",
           "\\boldsymbol{\\Omega}_{\\mathrm{region}, \\text{local}}")
  ))
})

test_that("Factor model (n_lv > 0) emits factor dynamics and loadings", {
  mod <- make_methods_md_prefit(
    y ~ x, trend_formula = ~ AR(p = 1, n_lv = 2)
  )
  # The dynamics act on the factors. The loadings map them to the
  # series and take the stored iid prior.
  expect_contains(md_rows(methods_md(mod)), c(
    paste0("\\tilde\\eta_{k,t} &= \\phi_{k,1} \\tilde\\eta_{k,t-1} ",
           "+ \\tilde\\epsilon^{(\\eta)}_{k,t}"),
    "\\eta_{i,t} &= \\sum_{k=1}^{2} Z_{i,k} \\tilde\\eta_{k,t}",
    "Z_{i,k} &\\sim \\text{StudentT}(3, 0, 0.5)"
  ))
  # `x:lv_axis()` gives each factor its own slope, one term indexed by
  # k. The factors run over time, which fixes `x` within a time.
  d <- data.frame(time = rep(1:30, 4),
                  series = factor(rep(paste0("s", 1:4), each = 30)),
                  x = rep(rnorm(30), 4), y = rpois(120, 3))
  slope <- make_methods_md_prefit(
    y ~ 1, trend_formula = ~ x:lv_axis() - 1 + AR(p = 1, n_lv = 2),
    data = d
  )
  expect_contains(md_rows(methods_md(slope)), c(
    "\\mu^{(\\eta)}_{k,t} &= \\beta^{(\\eta)}_{x, k} x_{t}",
    "- $\\beta^{(\\eta)}_{x, k}$: population effect of $x$ on factor $k$"
  ))
})

test_that("MGP shrinkage scales each loadings column by sqrt(Psi_k)", {
  set.seed(1L)
  d <- data.frame(
    time = rep(1:30, 4), series = factor(rep(paste0("s", 1:4), each = 30)),
    x = rnorm(120), y = rpois(120, 3)
  )
  mod <- mvgam(y ~ x, trend_formula = ~ AR(p = 1, n_lv = 2), data = d,
               family = poisson(), run_model = FALSE, silent = 2,
               loadings_prior = list(column_shrinkage = "mgp"),
               validate = FALSE)
  # Stan scales column k of the unit-scale loadings by sqrt(Psi_k),
  # with Psi_k the cumulative product of `varrho_inv`. The factor
  # innovations stay at unit scale.
  rows <- md_rows(methods_md(mod))
  expect_contains(rows, c(
    "\\Psi_k &= \\prod_{h \\le k} \\varrho_h",
    "Z_{i,k} &\\sim \\text{Normal}(0, \\sqrt{\\Psi_k})",
    "\\tilde\\epsilon^{(\\eta)}_{k,t} &\\sim \\text{Normal}(0, 1)",
    "\\varrho_{1} &\\sim \\text{InvGamma}(a_1, 1)"
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
  # The kernel multiplies the distance and feature kernels element by
  # element. Each loadings column is correlated by its Cholesky factor.
  expect_contains(md_rows(methods_md(mod)), c(
    paste0("\\boldsymbol{\\Phi} &= \\exp(-d_{\\mathrm{phylo}} / ",
           "\\theta_{\\mathrm{phylo}}) ",
           "\\odot \\text{GP}_{\\text{exp}}(\\text{features}; 1, ",
           "\\boldsymbol{\\theta}_{\\text{features}})"),
    "L_\\Phi &= \\text{Cholesky}(\\boldsymbol{\\Phi})",
    "Z_{\\cdot,k} &\\sim \\text{MVNormal}(\\mathbf{0}, L_\\Phi L_\\Phi^\\top)",
    paste0("- $d_{\\mathrm{phylo}}$: distances between the series from ",
           "'phylo', with length scale $\\theta_{\\mathrm{phylo}}$")
  ))
  # The methods paragraph names the sources the kernel was built from
  expect_match(
    how_to_cite(mod)$methods_text,
    paste0("built from trait features of the series and pairwise ",
           "distances among the series"),
    fixed = TRUE
  )
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
  mvgam(
    formula = bf(y ~ elev, p ~ tod),
    data = d, family = occ(),
    run_model = FALSE, silent = 2, validate = FALSE
  )
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
  mvgam(
    formula = bf(y ~ elev, p ~ tod),
    data = d, family = nmix(type),
    run_model = FALSE, silent = 2, validate = FALSE
  )
}

test_that("each response of a multivariate model is described by its family", {
  # Each response takes its own family, link and data label. The
  # Implementation call names each response's family in its `bf()`.
  # A distributional formula prints as `sigma ~ x`.
  set.seed(3L)
  d <- data.frame(time = 1:30, series = factor("a"), x = rnorm(30),
                  n_seen = rpois(30, 4), size_g = rlnorm(30, 1, 0.5))
  mv <- mvgam(
    bf(n_seen ~ x, family = poisson()) +
      bf(size_g ~ x, sigma ~ x, family = lognormal()) + set_rescor(FALSE),
    data = d, run_model = FALSE, validate = FALSE
  )
  out <- methods_md(mv)
  expect_match(out, "$\\mathrm{n\\_seen}$ non-negative integer counts",
               fixed = TRUE)
  expect_match(out, "$\\mathrm{size\\_g}$ positive real observations",
               fixed = TRUE)
  expect_contains(md_rows(out), c(
    paste0("\\mathrm{nseen}_{i,t} &\\sim ",
           "\\text{Poisson}(\\mu^{(\\mathrm{nseen})}_{i,t})"),
    paste0("\\log \\sigma^{(\\mathrm{sizeg})}_{i,t} &= ",
           "\\alpha^{(\\mathrm{sizeg}, \\mathrm{sigma})} + ",
           "\\beta^{(\\mathrm{sizeg}, \\mathrm{sigma})}_{x} x_{i,t}"),
    paste0("- $\\sigma^{(\\mathrm{sizeg})}_{i,t}$: standard deviation of ",
           "the log response")
  ))
  expect_match(
    out,
    paste0("bf(n_seen ~ x, family = poisson()) + ",
           "bf(size_g ~ x, sigma ~ x, family = lognormal())"),
    fixed = TRUE
  )
  # Each `bf()` names its family. The call's `family` argument would
  # print a default the user never set.
  expect_no_match(out, "family        =", fixed = TRUE)
  # Without one in a `bf()`, the `family` argument covers the response.
  # One response's varying slopes have their own correlation matrix.
  # Its prior carries that response.
  d$g <- factor(rep(c("a", "b", "c"), 10))
  mv <- mvgam(bf(n_seen ~ x) + bf(size_g ~ (1 + x | g)) + set_rescor(FALSE),
              data = d, family = gaussian(), run_model = FALSE,
              validate = FALSE)
  out <- methods_md(mv)
  expect_match(out, "family        = gaussian()", fixed = TRUE)
  expect_match(out, paste0("\\boldsymbol{\\Omega}^{(\\mathrm{sizeg})}_{g} ",
                           "&\\sim \\text{LKJCorr}(1)"), fixed = TRUE)
})


test_that("a parameter fixed in bf() is named in the call and glossary", {
  set.seed(4L)
  d <- data.frame(time = 1:40, series = factor("a"), x = rnorm(40),
                  y = rnorm(40))
  # A wrapper passes the data on as `..2`, which the call names `data`
  fit_via <- function(...) mvgam(...)
  mod <- fit_via(bf(y ~ x, quantile = 0.25), data = d,
                 family = asym_laplace(),
                 run_model = FALSE, silent = 2, validate = FALSE)
  out <- methods_md(mod)
  expect_contains(md_rows(out), c(
    "y_{i,t} &\\sim \\text{AsymLaplace}(\\mu_{i,t}, \\sigma, q)",
    "- $\\mu_{i,t}$: conditional $q$-quantile of $y_{i,t}$",
    "- $q = 0.25$: quantile level, fixed",
    "  formula       = bf(y ~ x, quantile = 0.25),",
    "  data          = data,"
  ))
})


test_that("the reproduction call carries the formula the user wrote", {
  # mvgam builds a formula with no coefficients on a placeholder
  # column. The printed call omits it.
  mod <- make_methods_md_prefit(y ~ 0, trend_formula = ~ AR(p = 1))
  out <- methods_md(mod)
  expect_no_match(out, ".mvgam_empty_obs", fixed = TRUE)
  expect_match(out, "formula       = y ~ 0", fixed = TRUE)
})


test_that("how_to_cite cites each family a model has", {
  # Each response's family is cited, a tweedie response inside
  # `mvbf()` included. Each of the three `nmix()` variants cites its
  # own references.
  cited <- function(mod) names(how_to_cite(mod)$bibtex)
  pb <- cited(make_nmix_prefit())
  expect_true("royle_nmix_2004" %in% pb)
  expect_false("royle_nichols_2003" %in% pb)
  rn <- cited(make_nmix_prefit("royle_nichols"))
  expect_true("royle_nichols_2003" %in% rn)
  expect_false("royle_nmix_2004" %in% rn)
  expect_true("neyman_type_a_1939" %in%
                cited(make_nmix_prefit("poisson_poisson")))
  expect_true("mackenzie_occu_2002" %in% cited(make_occ_prefit()))
  expect_false("jorgensen_tweedie" %in% cited(base_prefit()))
  set.seed(2L)
  d <- data.frame(time = 1:30, amount = rgamma(30, 2, 1),
                  count = rpois(30, 4))
  mv <- mvgam(
    bf(amount ~ 1, family = tweedie()) +
      bf(count ~ 1, family = poisson()) + set_rescor(FALSE),
    data = d, run_model = FALSE, validate = FALSE
  )
  expect_true(all(c("jorgensen_tweedie", "dunn_smyth_tweedie") %in%
                    cited(mv)))
  # The paragraph names each response's family and link
  expect_match(how_to_cite(mv)$methods_text,
               "amount: tweedie with a log link; count: poisson with a log link",
               fixed = TRUE)
})


test_that("how_to_cite cites a prefit's trend", {
  # A prefit records its trend type in its metadata alone. The
  # citation rules take the type from there and add the references
  # a VAR trend carries.
  mod <- make_methods_md_prefit(y ~ 1, trend_formula = ~ VAR())
  expect_true("heaps_var" %in% names(how_to_cite(mod)$bibtex))
  expect_false("savage_hvar" %in% names(how_to_cite(mod)$bibtex))
  # A hierarchical VAR also describes its pooling
  hier <- data.frame(time = rep(1:30, 4), region = rep(c("n", "s"), each = 60),
                     sp = rep(rep(c("a", "b"), each = 30), 2),
                     y = rpois(120, 3))
  hier_mod <- make_methods_md_prefit(
    y ~ 1, trend_formula = ~ VAR(gr = region, subgr = sp, cor = TRUE),
    data = hier
  )
  hier_cite <- how_to_cite(hier_mod)
  expect_true("savage_hvar" %in% names(hier_cite$bibtex))
  expect_match(hier_cite$methods_text, "exchangeable prior")
  # The paragraph names the trend and its correlated innovations
  expect_match(how_to_cite(mod)$methods_text,
               "poisson family with a log link. The latent process followed VAR(1) dynamics",
               fixed = TRUE)
  expect_match(hier_cite$methods_text,
               "dynamics with correlated innovations.", fixed = TRUE)
  # `other = FALSE` keeps the cited entries and drops the related ones
  bib <- withr::local_tempfile(fileext = ".bib")
  cited_only <- bibtex(hier_cite, file = bib, other = FALSE)
  expect_match(cited_only, "savage2016hvar", fixed = TRUE)
  expect_no_match(cited_only, "gabry2019workflow", fixed = TRUE)
  expect_match(bibtex(hier_cite, file = bib), "gabry2019workflow",
               fixed = TRUE)
  # The backend and algorithm are what the object records. One
  # recording neither was described as an rstan HMC run.
  mod$backend <- NULL
  expect_error(how_to_cite(mod), "object\\$backend")
  expect_error(methods_md(mod), "obj\\$backend")
})


test_that("methods_md renders the family the model stores", {
  # Every section renders the family `mvgam()` validated. An object
  # with no family is refused.
  mod <- base_prefit()
  expect_match(methods_md(mod), "family        = poisson()", fixed = TRUE)
  mod$family <- NULL
  expect_error(methods_md(mod), "object\\$family")
})


test_that("kernel and basis labels take the setting the spec records", {
  # `gp_call_to_spec()` and `parse_smooth_coef()` record brms's and
  # mgcv's own defaults where the user wrote none.
  spec <- mvgam:::gp_call_to_spec(quote(gp(x, k = 5)))
  expect_identical(
    mvgam:::gp_kernel_names(spec$cov),
    c(label = "exponentiated-quadratic", symbol = "k_{\\text{ExpQuad}}")
  )
  smooth <- mvgam:::parse_smooth_coef("s(x)")
  expect_identical(
    mvgam:::basis_label(smooth$bs, smooth$fname),
    "thin plate regression spline"
  )
})


test_that("occ() emits state + obs + logit(p) rows", {
  # The detection covariate varies by visit, the state's by unit.
  rows <- md_rows(methods_md(make_occ_prefit()))
  expect_contains(rows, c(
    paste0("y_{i,t,j} \\mid z_{i,t} &\\sim ",
           "\\text{Bernoulli}(z_{i,t} \\cdot p_{i,t,j})"),
    "z_{i,t} &\\sim \\text{Bernoulli}(\\psi_{i,t})",
    paste0("\\text{logit}(\\psi_{i,t}) &= \\alpha + ",
           "\\beta_{\\mathrm{elev}} \\mathrm{elev}_{i,t}"),
    paste0("\\text{logit}(p_{i,t,j}) &= \\alpha^{(p)} + ",
           "\\beta^{(p)}_{\\mathrm{tod}} \\mathrm{tod}_{i,t,j}")
  ))
  expect_true(any(grepl(
    "$G = 8$ closure units, $\\bar J = 4$ visits per unit (range 4-4)",
    rows, fixed = TRUE
  )))
})

test_that("a multi-season unit is indexed by series, site and season", {
  d <- expand.grid(
    series = factor(c("sp1", "sp2")), site = factor(seq_len(3L)),
    time = seq_len(2L), visit = seq_len(2L), KEEP.OUT.ATTRS = FALSE
  )
  d$y <- rep(c(0L, 1L), length.out = nrow(d))
  mod <- mvgam(y ~ 1, data = d, family = occ(multi_season = TRUE),
               run_model = FALSE, silent = 2, validate = FALSE)
  out <- methods_md(mod)
  expect_match(out, "s &\\in \\{1, \\ldots, 3\\}", fixed = TRUE)
  expect_match(out, "y_{i,s,t,j} \\mid z_{i,s,t} &\\sim", fixed = TRUE)
  expect_match(out, "family        = occ(multi_season = TRUE)",
               fixed = TRUE)
  # Each constructor setting off its default is named in the call
  expect_identical(
    vapply(list(nmix("royle_nichols"), tweedie(M = 50L), com_binomial(),
                brms::brmsfamily("gaussian", link_sigma = "identity")),
           mvgam:::family_call_text, character(1L)),
    c("nmix(type = \"royle_nichols\")", "tweedie(M = 50L)",
      "com_binomial()",
      "brmsfamily(\"gaussian\", link_sigma = \"identity\")")
  )
})

test_that("closure units are counted over every column that keys them", {
  # The count uses every column of the key. A frame missing one of
  # them is refused.
  mod <- make_occ_prefit()
  mod$data$time <- NULL
  expect_error(methods_md(mod), "'time'")
})

test_that("nmix Poisson-binomial emits N | lambda + Binomial obs row", {
  out <- methods_md(make_nmix_prefit("poisson_binomial"))
  expect_match(
    out,
    "y_{i,t,j} \\mid N_{i,t} &\\sim \\text{Binomial}",
    fixed = TRUE
  )
  expect_match(
    out,
    "N_{i,t} &\\sim \\text{Poisson}(\\lambda_{i,t})",
    fixed = TRUE
  )
  # Detection link is logit for PB.
  expect_match(out, "\\text{logit}(p_{i,t,j})", fixed = TRUE)
})

test_that("nmix RN emits Bernoulli(1 - (1 - p)^N) obs row", {
  out <- methods_md(make_nmix_prefit("royle_nichols"))
  expect_match(
    out,
    "\\text{Bernoulli}(1 - (1 - p_{i,t,j})^{N_{i,t}})",
    fixed = TRUE
  )
})

test_that("nmix PPM emits Poisson(N * p) and log(p) link", {
  out <- methods_md(make_nmix_prefit("poisson_poisson"))
  expect_match(out, "\\text{Poisson}(N_{i,t} \\cdot p_{i,t,j})", fixed = TRUE)
  # PPM detection link is log per R/families.R::nmix_poisson_poisson.
  expect_match(out, "\\log p_{i,t,j}", fixed = TRUE)
})

test_that("diri() writes one simplex per unit against the first series", {
  set.seed(1L)
  K <- 3L; n_sites <- 5L
  d <- expand.grid(
    time = 1:n_sites, series = factor(paste0("sp", 1:K))
  )
  d$env <- rep(rnorm(n_sites), times = K)
  d$y <- runif(n_sites * K)
  d$y <- d$y / stats::ave(d$y, d$time, FUN = sum)
  mod <- jsdgam(
    formula = y ~ env * series, factor_formula = ~ -1,
    data = d, unit = time, species = series,
    family = diri(), n_lv = 2L,
    run_model = FALSE, silent = 2, backend = "cmdstanr", validate = FALSE
  )
  # The kernel subtracts the first series' predictor before the
  # softmax. Each loadings column sums to zero. A series level enters
  # through its indicator, against the reference level sp1.
  expect_contains(md_rows(methods_md(mod)), c(
    "\\mathbf{Y}_t &\\sim \\text{Dirichlet}(\\phi \\, \\boldsymbol{\\pi}_t)",
    paste0("\\boldsymbol{\\pi}_t &= ",
           "\\text{softmax}(\\boldsymbol{\\mu}_t - \\mu_{1,t})"),
    "\\textstyle\\sum_{i} Z_{i,k} &= 0",
    paste0("- $\\beta_{\\mathrm{env}:\\mathrm{series}[\\mathrm{sp2}]}$: ",
           "difference in the effect of $\\mathrm{env}$ between level ",
           "$\\mathrm{sp2}$ and reference level $\\mathrm{sp1}$ of ",
           "$\\mathrm{series}$")
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
  # Explicit nlpar priors give the table proper rows to render. The
  # Priors section lists no flat prior.
  mod <- mvgam(
    formula = bf(yC ~ a + b * x, a + b ~ trait1, nl = TRUE),
    data = d, family = gaussian(),
    prior = c(
      brms::prior(normal(0, 1), nlpar = "a"),
      brms::prior(normal(0, 1), nlpar = "b")
    ),
    run_model = FALSE, silent = 2, validate = FALSE
  )
  expect_contains(md_rows(methods_md(mod)), c(
    "\\beta^{(a)}_{\\mathrm{trait1}} &\\sim \\text{Normal}(0, 1)",
    "\\beta^{(b)}_{\\mathrm{trait1}} &\\sim \\text{Normal}(0, 1)"
  ))
})

test_that("Ordinal Intercept rows render as theta_{k} thresholds", {
  set.seed(1L)
  d <- data.frame(
    time = rep(1:30, 4), series = factor(rep(1:4, each = 30)),
    x = rnorm(120),
    yord = factor(sample(1:5, 120, replace = TRUE), ordered = TRUE)
  )
  mod <- mvgam(
    formula = yord ~ x,
    data = d, family = cumulative(),
    run_model = FALSE, silent = 2, validate = FALSE
  )
  # The thresholds take the intercept's place. The mean has none. The
  # link maps each threshold less the latent score to a probability.
  rows <- md_rows(methods_md(mod))
  expect_contains(rows, c(
    paste0("\\mathrm{yord}_{i,t} &\\sim \\text{OrderedCumulative}",
           "(\\boldsymbol{\\theta}, \\mu_{i,t})"),
    "\\mu_{i,t} &= \\beta_{x} x_{i,t}",
    paste0("- $\\mu_{i,t}$: latent score of $\\mathrm{yord}_{i,t}$, with ",
           "$P(\\mathrm{yord}_{i,t} \\le c) = F(\\theta_{c} - \\mu_{i,t})$ ",
           "and $F$ the inverse of the logit link")
  ))
  expect_true(any(startsWith(rows, "\\theta_{4} &\\sim")))
})

test_that("ordinal thresholds render their shifts, groups and scale", {
  set.seed(1L)
  d <- data.frame(
    time = rep(1:30, 4), series = factor(rep(1:4, each = 30)),
    x = rnorm(120), g = factor(rep(letters[1:3], 40)),
    gs = factor(rep(c("p", "q"), 60)),
    yord = factor(sample(1:4, 120, replace = TRUE), ordered = TRUE)
  )
  d$yg <- pmin(as.integer(d$yord), ifelse(d$gs == "q", 3L, 4L))
  rows_of <- function(formula, family) {
    md_rows(methods_md(mvgam(formula, data = d, family = family,
                             run_model = FALSE, silent = 2,
                             validate = FALSE)))
  }
  # Each `cs()` term shifts every threshold by its own amount. brms
  # repeats its advisory on cumulative `cs()` terms at each parse.
  rows <- without_brms_fit_advisories(
    rows_of(bf(yord ~ cs(x) + (cs(1) | g)), cumulative())
  )
  expect_contains(rows, c(
    paste0("\\mathrm{yord}_{i,t} &\\sim \\text{OrderedCumulative}",
           "(\\boldsymbol{\\theta}_{i,t}, \\mu_{i,t})"),
    paste0("\\theta_{i,t,c} &= \\theta_{c} - (\\beta_{x,c} x_{i,t} + ",
           "\\alpha_{g[i],c})"),
    "- $\\beta_{x,c}$: population effect of $x$ on threshold $c$"
  ))
  expect_true(any(startsWith(rows, "\\sigma^{(\\alpha)}_{g,3} &\\sim")))
  # Each level of `gs` has thresholds of its own. Its header row
  # repeats their prior and is not shown.
  rows <- rows_of(bf(yg | thres(gr = gs) ~ x), cumulative())
  expect_contains(rows, paste0(
    "\\mathrm{yg}_{i,t} &\\sim \\text{OrderedCumulative}",
    "(\\boldsymbol{\\theta}_{\\mathrm{gs}[i]}, \\mu_{i,t})"
  ))
  expect_true(any(startsWith(rows, "\\theta_{\\mathrm{q},2} &\\sim")))
  expect_false(any(startsWith(rows, "\\alpha &\\sim")))
  # A predicted discrimination scales each cut. The hurdle family's zero
  # is its hurdle.
  rows <- rows_of(bf(yord ~ 1, disc ~ x), cumulative())
  expect_match(rows, "F(\\mathrm{disc}_{i,t} \\, (\\theta_{c} - \\mu_{i,t}))",
               fixed = TRUE, all = FALSE)
  rows <- rows_of(bf(yg ~ x), hurdle_cumulative())
  expect_match(rows, "\\text{Hurdle-OrderedCumulative}(\\boldsymbol{\\theta}",
               fixed = TRUE, all = FALSE)
})

test_that("mvbind rescor priors carry response superscripts", {
  set.seed(1L)
  d <- data.frame(
    time = rep(1:30, 2), series = factor(rep(1:2, each = 30)),
    x = rnorm(60), yA = rnorm(60), yB = rnorm(60)
  )
  mod <- mvgam(
    formula = brms::bf(brms::mvbind(yA, yB) ~ x) +
      brms::set_rescor(TRUE),
    data = d, family = gaussian(),
    run_model = FALSE, silent = 2, validate = FALSE
  )
  # The prior on the Cholesky factor is stated on the matrix it builds.
  sds <- "\\sigma^{(\\mathrm{yA})}, \\sigma^{(\\mathrm{yB})}"
  expect_contains(md_rows(methods_md(mod)), c(
    paste0("\\boldsymbol{\\Sigma} &= \\text{diag}(", sds, ") \\, ",
           "\\boldsymbol{\\Omega} \\, \\text{diag}(", sds, ")"),
    "\\boldsymbol{\\Omega} &\\sim \\text{LKJCorr}(1)"
  ))
})

test_that("mvn() emits MVNormal with a diagonal Sigma", {
  set.seed(1L)
  K <- 3L; n_sites <- 6L
  d <- expand.grid(
    time = 1:n_sites, series = factor(paste0("sp", 1:K))
  )
  d$env <- rep(rnorm(n_sites), times = K)
  d$y <- rnorm(n_sites * K)
  mod <- jsdgam(
    formula = y ~ env, factor_formula = ~ -1,
    data = d, unit = time, species = series,
    family = mvn(), n_lv = 1L,
    run_model = FALSE, silent = 2, backend = "cmdstanr", validate = FALSE
  )
  out <- methods_md(mod)
  # The kernel evaluates independent normals with a per-element
  # scale. The covariance is diagonal. Every loading is sampled, which
  # fixes the factor correlation at the identity. The program has no
  # LKJ prior and the page states none.
  expect_contains(md_rows(out), c(
    paste0("\\mathbf{Y}_t &\\sim \\text{MVNormal}(\\boldsymbol{\\mu}_t, ",
           "\\boldsymbol{\\Sigma})"),
    "\\boldsymbol{\\Sigma} &= \\text{diag}(\\boldsymbol{\\Psi}^2)"
  ))
  expect_no_match(out, "LKJ", fixed = TRUE)
})

test_that("a jsdgam model names species, sampling units and factors", {
  set.seed(1L)
  d <- expand.grid(time = 1:6, series = factor(paste0("sp", 1:3)))
  d$env <- rep(rnorm(6), times = 3)
  d$y <- rpois(18, 2)
  mod <- jsdgam(y ~ env, data = d, family = poisson(), n_lv = 2L,
                run_model = FALSE, silent = 2, backend = "cmdstanr",
                validate = FALSE)
  out <- methods_md(mod)
  expect_match(out, "$S = 3$ species, $T = 6$ sampling units", fixed = TRUE)
  expect_match(out, "loading of species $i$ on factor $k$", fixed = TRUE)
  # ZMVN factors have no dynamics. The paragraph names the factors.
  expect_match(
    how_to_cite(mod)$methods_text,
    "Residual associations among species were modelled with 2 latent factors.",
    fixed = TRUE
  )
})

test_that("how_to_cite describes ZMVN states as independent over time", {
  mod <- make_methods_md_prefit(y ~ x, trend_formula = ~ ZMVN())
  expect_match(
    how_to_cite(mod)$methods_text,
    paste0("The latent states were zero-mean multivariate normal and ",
           "independent over time, with correlations among series."),
    fixed = TRUE
  )
})


test_that("a moving-average trend is named as one", {
  # `AR(ma = TRUE)` is recorded under trend type "AR" with its MA lags.
  # The order label and the coefficient definitions name the MA part.
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
  # A contiguous lag set is named by its order. `ARMA(1, 2, 1)` is
  # the spelling of an ARIMA order.
  expect_identical(
    mvgam:::trend_order_label(fake("AR", 1:2, integer(0))), "AR(2)"
  )
  expect_identical(
    mvgam:::trend_order_label(fake("AR", 1:2, 1L)), "ARMA(2, 1)"
  )
  expect_identical(
    mvgam:::trend_order_label(fake("VAR", 1:3, integer(0))), "VAR(3)"
  )
  # A sparse lag set lists its lags in braces.
  expect_identical(
    mvgam:::trend_order_label(fake("AR", c(1L, 12L), integer(0))),
    "AR({1, 12})"
  )
  expect_identical(
    mvgam:::trend_order_label(fake("AR", c(1L, 12L), 1L)),
    "ARMA({1, 12}, 1)"
  )
  expect_true(mvgam:::trend_has_ma(fake("AR", 1L, 1L)))
  expect_false(mvgam:::trend_has_ma(fake("AR", 1L, integer(0))))
})


test_that("a gamma fit is described as one", {
  # `resolve_family_name()` names `stats::Gamma()` "gamma". The data
  # label, the likelihood and the call each describe a gamma fit.
  d <- data.frame(
    time = rep(1:30, 2), series = factor(rep(c("a", "b"), each = 30)),
    x = rnorm(60), y = rgamma(60, 2, 1)
  )
  mod <- make_methods_md_prefit(y ~ x, family = Gamma(link = "log"),
                                data = d)
  expect_contains(md_rows(methods_md(mod)), c(
    "$y$: positive real observations",
    "y_{i,t} &\\sim \\text{Gamma}(\\varphi, \\mu_{i,t})"
  ))
  # The call names the brms constructor. `gamma()` is the mathematical
  # function.
  expect_identical(mvgam:::family_call_text(mod$family),
                   "Gamma(link = \"log\")")
})


test_that("an offset enters the predictor with its coefficient at one", {
  set.seed(4L)
  d <- data.frame(time = rep(1:30, 2),
                  series = factor(rep(c("a", "b"), each = 30)),
                  x = rnorm(60), e = runif(60, 1, 2), y = rpois(60, 3))
  out <- methods_md(make_methods_md_prefit(
    y ~ x + offset(e) + offset(log(e)), data = d
  ))
  expect_match(out, "\\beta_{x} x_{i,t} + e_{i,t} + o_{i,t}\n", fixed = TRUE)
  expect_match(out, "$o_{i,t}$: offset `log(e)`, with its coefficient",
               fixed = TRUE)
  # brms writes the default intercept location of an offset model at
  # full precision. The page shows three significant digits.
  expect_match(out, "\\\\text\\{StudentT\\}\\(3, -?0\\.[0-9]{1,3}, 2\\.5\\)")
})


test_that("an unfitted model prints each family by its own name", {
  # An unfitted model prints with the unfitted printer. It names each
  # family by `resolve_family_name()`. brms writes `family$family` as
  # "custom" for every family built with `custom_family()`. A model
  # with several responses prints each response's family.
  d <- data.frame(time = 1:30, series = factor("s1"),
                  y = rgamma(30, 2, 1))
  pf <- mvgam(y ~ 1, data = d, family = tweedie(), run_model = FALSE,
              validate = FALSE)
  printed <- capture.output(print(pf))
  expect_true(any(grepl("Family: tweedie", printed, fixed = TRUE)))
  expect_false(any(grepl("custom", printed, fixed = TRUE)))

  w <- data.frame(time = 1:20, count = rpois(20, 4),
                  seen = rbinom(20, 1, 0.5))
  mv <- mvgam(
    brms::bf(count ~ 1, family = poisson()) +
      brms::bf(seen ~ 1, family = bernoulli()) + brms::set_rescor(FALSE),
    trend_formula = ~ AR(p = 1), data = w, run_model = FALSE, validate = FALSE
  )
  printed <- capture.output(print(mv))
  expect_true(any(grepl("count: poisson", printed, fixed = TRUE)))
  expect_true(any(grepl("seen: bernoulli", printed, fixed = TRUE)))
})
