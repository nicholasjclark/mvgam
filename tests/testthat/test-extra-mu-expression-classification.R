# The tests of test-mu-expression-classification.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

test_that("mu statements come with the variables they use", {
  set.seed(123)
  d <- data.frame(Y = rnorm(60), x = runif(60, 0, 10),
                  mono = sample(1:5, 60, TRUE), g = sample(1:6, 60, TRUE))
  mu_of <- function(f) {
    mvgam:::extract_mu_construction(
      as.character(brms::stancode(f, data = d, family = gaussian()))
    )
  }

  # A gaussian GLM takes `Xc * b` as arguments. The statement it never
  # wrote joins the others. The call `mo()` stays out of the variables.
  glm <- mu_of(Y ~ x + mo(mono) + (1 | g))
  expect_identical(utils::tail(glm$mu_construction, 1L), "mu += Xc * b;")
  expect_identical(
    glm$referenced_variables,
    c("N", "mu", "Intercept", "bsp", "simo_1", "Xmo_1", "r_1_1", "J_1",
      "Z_1_1", "Xc", "b")
  )

  # An approximate GP's prediction follows the basis it is built from
  gp <- mu_of(Y ~ gp(x, c = 5 / 4, k = 20))
  expect_identical(
    sub("^vector\\[[^]]*\\] (\\w+) =.*$", "\\1",
        gp$supporting_declarations),
    c("mu", "rgp_1", "gp_pred_1")
  )

  no_mu <- mvgam:::extract_mu_construction(
    "model { target += normal_lpdf(y | 0, 1); }"
  )
  expect_length(no_mu$mu_construction, 0L)
})
