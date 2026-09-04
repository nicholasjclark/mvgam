# What a structured prior on the loadings buys, and whether it
# recovers the structure it was given.
#
# A factor model's loadings are free by default: each species picks
# up whatever the data supports, and species that ought to behave
# alike are told nothing about each other. The Heaps and Jermyn
# (2024) construction puts a row covariance on `Z` instead, built
# from what is known about the species -- their traits, and how far
# apart they sit on a phylogeny -- so that similar species are
# shrunk toward similar loadings.
#
# Three fits, each asking a different question of that prior:
#
#   cluster    Eight series in two clusters over an AR(1) factor
#              model, with a hierarchical distance that is zero
#              within a cluster and one across it. The question is
#              whether the block structure of the true loadings
#              survives into the implied covariance.
#   birds      Thirty species at 25 sites, presence-absence, with a
#              body-mass trait and a coalescent phylogeny. Fitted
#              twice, under mvgam's default prior on the
#              length-scales and under the wider one the paper
#              uses, so the two can be compared on the same data.
#   phylo      The same architecture on a continuous response, with
#              the kernel deliberately tilted: the phylogenetic
#              length-scale is a third of the trait one, so the
#              phylogeny carries the row structure and the trait
#              kernel is nearly flat. The question is whether the
#              posterior finds that tilt.
#
# The three files this replaces computed most of these numbers and
# checked none of them. `heaps_birds_replica.R` wrapped twelve
# post-fit calls in `tryCatch`, turned each error into a row of a
# table, printed how many had passed and exited clean whatever that
# count was. `heaps_birds_replica_phylo_dominant.R` listed three checks
# in its header, computed all three and printed them. Every printed
# verdict below is now an expectation.
#
# Fits cache under fixtures/. Delete one to refit it; the birds and
# phylo fits take roughly ten minutes each.
#
# Run with:
#   Rscript -e "devtools::load_all('.'); testthat::test_file('tests/local/test-factor-loadings-prior.R')"

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(ape)
  library(posterior)
  library(testthat)
})

cache_path <- function(name) {
  dir <- if (dir.exists("fixtures")) {
    "fixtures"
  } else {
    file.path("tests", "local", "fixtures")
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, name)
}

# A ggplot is returned whether or not a layer received any data, so
# asserting the class passes on the empty panel it looks like it is
# guarding.
expect_drawn <- function(p) {
  expect_s3_class(p, "ggplot")
  layers <- ggplot2::ggplot_build(p)$data
  expect_gt(sum(vapply(layers, nrow, integer(1L))), 0L)
  invisible(layers)
}

# The trait and phylogeny both reach `Z` through a distance, and
# both replicas build theirs the same way: standardise the tree so
# root-to-tip is one, then scale each distance matrix to a maximum
# of one so the length-scales are comparable.
heaps_kernels <- function(n_species, species_levels) {
  body_mass <- rnorm(n_species)
  body_mass <- (body_mass - mean(body_mass)) / sd(body_mass)
  tree <- ape::rcoal(n = n_species, tip.label = species_levels)
  tree$edge.length <- tree$edge.length /
    max(diag(ape::vcv.phylo(tree, model = "Brownian")))
  phylo_dist <- ape::cophenetic.phylo(tree)
  phylo_dist <- phylo_dist / max(phylo_dist)
  trait_dist <- as.matrix(stats::dist(body_mass))
  trait_dist <- trait_dist / max(trait_dist)
  list(
    body_mass = body_mass, tree = tree,
    trait_df = data.frame(series = species_levels,
                          body_mass = body_mass),
    phylo_dist = phylo_dist, trait_dist = trait_dist
  )
}


# ---- 1. Clustered loadings over an AR(1) factor model ----------------
#
# Cluster A loads on factors 1 and 3, cluster B on factors 2 and 3,
# and the shared third factor is modulated by a trait. So the true
# `Z Z'` is block structured, and within-cluster pairs share more
# than between-cluster pairs by construction.

sim_cluster <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    set.seed(8024L)
    n_t <- 60L
    n_series <- 8L
    n_lv <- 3L
    ar <- c(0.75, 0.55, 0.45)
    sigma <- rep(0.45, n_lv)
    series <- paste0("s", seq_len(n_series))
    cluster <- factor(rep(c("A", "B"), each = n_series / 2L),
                      levels = c("A", "B"))
    trait <- c(seq(-1, 1, length.out = n_series / 2L),
               seq(-1, 1, length.out = n_series / 2L))

    Z <- matrix(0, nrow = n_series, ncol = n_lv)
    Z[cluster == "A", 1L] <- 0.9 + 0.1 * trait[cluster == "A"]
    Z[cluster == "B", 2L] <- 0.9 - 0.1 * trait[cluster == "B"]
    Z[, 3L] <- 0.5 + 0.3 * trait
    rownames(Z) <- series

    lv <- matrix(0, n_t, n_lv)
    for (k in seq_len(n_lv)) {
      lv[1L, k] <- rnorm(1L, 0, sigma[k] / sqrt(1 - ar[k]^2))
      for (t in 2:n_t) {
        lv[t, k] <- ar[k] * lv[t - 1L, k] + rnorm(1L, 0, sigma[k])
      }
    }
    y <- as.vector(lv %*% t(Z)) + rnorm(n_t * n_series, 0, 0.25)
    d <- data.frame(
      series = factor(rep(series, each = n_t), levels = series),
      time = rep(seq_len(n_t), times = n_series),
      y = y
    )
    features <- data.frame(series = series, trait = trait,
                           cluster = cluster)
    # Hierarchical distance: zero within a cluster, one across it.
    d_cluster <- as.matrix(
      stats::dist(as.numeric(cluster), method = "manhattan")
    )
    rownames(d_cluster) <- colnames(d_cluster) <- series
    cached <<- list(
      data = d, features = features, d_cluster = d_cluster,
      Z_true = Z, cluster = cluster, trait = trait,
      n_series = n_series, n_lv = n_lv
    )
    cached
  }
})

cluster_fit <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    path <- cache_path("val_mvgam_loadings_prior.rds")
    if (file.exists(path)) {
      cached <<- readRDS(path)
      return(cached)
    }
    sim <- sim_cluster()
    cached <<- mvgam(
      formula = y ~ 1,
      trend_formula = ~ AR(p = 1, n_lv = sim$n_lv),
      data = sim$data, family = gaussian(),
      data2 = list(features = sim$features,
                   cluster = sim$d_cluster),
      loadings_prior = list(features = "features",
                            distances = "cluster"),
      chains = 2L, iter = 1000L, warmup = 500L,
      refresh = 0, silent = 2, backend = "cmdstanr"
    )
    saveRDS(cached, path)
    cached
  }
})


test_that("cluster: the structured prior reaches the Stan program", {
  # The kernel is what distinguishes this from a free-loadings fit,
  # so its pieces have to be in the program rather than merely
  # requested. Named in both spellings, since a normalised program
  # writes the density call and testing only the tilde form passes
  # vacuously.
  sc <- cluster_fit()$stancode
  for (piece in c("gp_exponential_cov", "dist_cluster",
                  "theta_features", "theta_dist_cluster",
                  "multi_normal_cholesky", "qr_thin_R")) {
    expect_match(sc, piece, fixed = TRUE)
  }
  expect_false(grepl("to_vector\\(Z\\)\\s*~\\s*student_t", sc))
  expect_false(grepl("student_t_lpdf(to_vector(Z)", sc, fixed = TRUE))
})


test_that("cluster: standata carries the encoded features", {
  sd <- cluster_fit()$standata
  # One continuous trait plus two one-hot cluster indicators, both
  # levels retained.
  expect_equal(sd$N_features_trend, 3L)
  expect_equal(dim(sd$row_features), c(8L, 3L))
  expect_equal(dim(sd$dist_cluster), c(8L, 8L))
  # The distance matrix is standardised to a maximum of one, and it
  # is not the zero matrix, which would leave the kernel flat.
  expect_equal(max(sd$dist_cluster), 1)
  expect_gt(stats::sd(as.numeric(sd$dist_cluster)), 0)
})


test_that("cluster: the identified loadings converge", {
  draws <- as_draws_df(cluster_fit()$fit)
  z_cols <- grep("^Z_tilde\\[", colnames(draws), value = TRUE)
  expect_gt(length(z_cols), 0L)
  diag <- summarise_draws(
    subset_draws(draws, variable = z_cols),
    default_convergence_measures()
  )
  expect_lt(max(diag$rhat, na.rm = TRUE), 1.1)
  expect_gt(min(diag$ess_bulk, na.rm = TRUE), 100)
})


test_that("cluster: the length-scales are finite and positive", {
  draws <- as_draws_df(cluster_fit()$fit)
  theta_dist <- extract_variable(draws, "theta_dist_cluster")
  expect_true(all(is.finite(theta_dist)))
  expect_true(all(theta_dist > 0))
  feat <- grep("^theta_features\\[", colnames(draws), value = TRUE)
  expect_equal(length(feat), 3L)
  med <- vapply(feat, function(v) {
    median(extract_variable(draws, v))
  }, numeric(1L))
  expect_true(all(is.finite(med)))
  expect_true(all(med > 0))
})


test_that("cluster: within-cluster series share more than across", {
  # The claim the simulation was built to make. Cluster A and B load
  # on disjoint primary factors and share only the third, so both
  # the implied correlation and the shared-variation matrix have to
  # put within-cluster pairs above between-cluster ones.
  sim <- sim_cluster()
  fit <- cluster_fit()
  cl <- sim$cluster
  cor_mat <- residual_cor(fit)$cor
  expect_equal(dim(cor_mat), c(8L, 8L))
  within <- outer(cl, cl, "==") & upper.tri(cor_mat)
  between <- outer(cl, cl, "!=") & upper.tri(cor_mat)
  expect_gt(mean(cor_mat[within]), mean(cor_mat[between]))

  sv <- shared_variation(fit)
  expect_s3_class(sv, "mvgam_shared_variation")
  expect_equal(sv$n_series, 8L)
  expect_equal(sv$n_lv, 3L)
  expect_equal(dim(sv$delta), c(8L, 8L))
  expect_equal(rownames(sv$delta), rownames(sim$Z_true))
  expect_gt(mean(sv$delta[within]), mean(sv$delta[between]))
  expect_true(all(diag(sv$delta) > 0))
})


test_that("cluster: the dominant contrast survives the rotation", {
  # `Z_tilde` lives in a rotated basis, so a column-by-column match
  # against the truth is not identifiable. The cluster contrast is
  # the dominant rotation-invariant signal in this simulation, so it
  # has to appear in some fitted factor.
  sim <- sim_cluster()
  draws <- as_draws_matrix(cluster_fit()$fit)
  z_cols <- grep("^Z_tilde\\[", colnames(draws), value = TRUE)
  Z_med <- matrix(
    apply(draws[, z_cols], 2L, stats::median),
    nrow = nrow(sim$Z_true), ncol = ncol(sim$Z_true)
  )
  contrast <- sim$Z_true[, 1L] - sim$Z_true[, 2L]
  expect_gte(max(abs(stats::cor(contrast, Z_med))), 0.6)
})


test_that("cluster: summary reports the prior's own block", {
  s <- summary(cluster_fit())
  expect_false(is.null(s$loadings_prior))
  block <- rownames(s$loadings_prior)
  expect_true(all(c("theta_features[1]", "theta_features[2]",
                    "theta_features[3]", "theta_dist_cluster") %in%
                    block))
  # `varrho_inv` is a cumulative-product parameter for `Psi_diag` and
  # means nothing on its own, so it is deliberately hidden.
  expect_false(any(grepl("^varrho_inv", block)))
})


# ---- 2. Trait and phylogeny on presence-absence ----------------------

sim_birds <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    set.seed(2024L)
    n_species <- 30L
    n_sites <- 25L
    n_lv <- 3L
    lev <- paste0("sp", sprintf("%02d", seq_len(n_species)))
    k <- heaps_kernels(n_species, lev)

    theta_trait <- 0.4
    theta_phylo <- 0.4
    Phi <- exp(-k$trait_dist / theta_trait) *
      exp(-k$phylo_dist / theta_phylo)
    Phi <- Phi + diag(1e-6, n_species)
    Z_true <- t(chol(Phi)) %*%
      matrix(rnorm(n_species * n_lv), n_species, n_lv)
    lv_true <- matrix(rnorm(n_sites * n_lv), n_sites, n_lv)
    Y <- matrix(rbinom(n_sites * n_species, 1L,
                       pnorm(lv_true %*% t(Z_true))),
                nrow = n_sites, ncol = n_species)
    colnames(Y) <- lev
    d <- data.frame(
      series = factor(rep(lev, each = n_sites), levels = lev),
      time = rep(seq_len(n_sites), n_species),
      y = as.integer(as.vector(Y))
    )
    cached <<- c(k, list(
      data = d, n_species = n_species, n_sites = n_sites,
      n_lv = n_lv, species_levels = lev, Phi = Phi,
      Z_true = Z_true, theta_trait = theta_trait,
      theta_phylo = theta_phylo
    ))
    cached
  }
})

birds_fit <- function(wide = FALSE) {
  nm <- if (wide) "val_mvgam_heaps_birds_wide.rds" else
    "val_mvgam_heaps_birds.rds"
  path <- cache_path(nm)
  if (file.exists(path)) return(readRDS(path))
  sim <- sim_birds()
  args <- list(
    formula = y ~ 1, factor_formula = ~ -1,
    data = sim$data, unit = quote(time), species = quote(series),
    family = bernoulli(), n_lv = sim$n_lv,
    traits = sim$trait_df, phylo = sim$tree,
    chains = 2L, burnin = 400L, samples = 400L, silent = 2
  )
  if (wide) {
    # The paper puts log(theta) ~ N(0, sqrt(10)) on the inverse
    # length-scales where mvgam defaults to N(0, 1).
    args$priors <- c(
      brms::prior("normal(0, 3.162)", class = "theta_features"),
      brms::prior("normal(0, 3.162)", class = "theta_dist_phylo")
    )
  }
  fit <- do.call(jsdgam, args)
  saveRDS(fit, path)
  fit
}


test_that("birds: the multiplicative kernel reaches the program", {
  fit <- birds_fit()
  sc <- as.character(fit$stancode)
  for (piece in c("theta_features", "theta_dist_phylo",
                  "gp_exponential_cov", "multi_normal_cholesky")) {
    expect_match(sc, piece, fixed = TRUE)
  }
  sd <- fit$standata
  expect_true("row_features" %in% names(sd))
  expect_true("dist_phylo" %in% names(sd))
  # One trait column over thirty species, and a full distance matrix
  # that is neither empty nor constant.
  expect_identical(nrow(sd$row_features), 30L)
  expect_identical(dim(sd$dist_phylo), c(30L, 30L))
  expect_equal(max(sd$dist_phylo), 1)
  expect_gt(stats::sd(as.numeric(sd$dist_phylo)), 0)
})


test_that("birds: every post-fit method answers on this fit", {
  # This replaces a `tryCatch` wrapper that recorded each method's
  # error as a row of a table and let the file exit clean however
  # many had failed. Each method is now held to the shape and scale
  # it owes, so a failure stops the file.
  fit <- birds_fit()
  sim <- sim_birds()
  n_obs <- nrow(sim$data)

  txt <- capture.output(summary(fit))
  expect_true(any(grepl(paste0("Series:\\s*", sim$n_species), txt)))
  expect_gt(length(capture.output(print(fit))), 5L)

  ep <- posterior_epred(fit, draw_ids = 1:30)
  pp <- posterior_predict(fit, draw_ids = 1:30)
  expect_identical(dim(ep), c(30L, n_obs))
  expect_identical(dim(pp), c(30L, n_obs))
  # A probit-linked presence model: the expectation is a probability
  # and a draw is a presence.
  expect_true(all(ep > 0 & ep < 1))
  expect_true(all(as.numeric(pp) %in% c(0, 1)))

  expect_identical(nrow(predict(fit, type = "link")), n_obs)
  expect_identical(nrow(predict(fit, type = "response")), n_obs)

  ll <- log_lik(fit, draw_ids = 1:30)
  expect_identical(dim(ll), c(30L, n_obs))
  expect_true(all(is.finite(ll)))
  expect_gt(stats::sd(colMeans(ll)), 1e-8)

  seen <- character(0)
  ic <- withCallingHandlers(loo(fit), warning = function(w) {
    seen <<- c(seen, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
  expect_identical(length(ic$diagnostics$pareto_k), n_obs)
  expect_true(all(grepl("Pareto", seen)))

  expect_drawn(plot(fit, type = "factors"))
  ord <- expect_drawn(ordinate(fit, alpha = 0.7))
  # One point per species on the biplot.
  expect_true(any(vapply(ord, function(x) nrow(x) == sim$n_species,
                         logical(1))))
  expect_drawn(pp_check(fit, ndraws = 30L))
  expect_s3_class(conditional_effects(fit),
                  "mvgam_conditional_effects")
})


test_that("birds: the loadings recover the covariance the data holds", {
  # `Phi` is the prior covariance the loadings were drawn from, not
  # the covariance this dataset carries. The Stan program gives each
  # column of `Z` its own `multi_normal_cholesky(0, L_Phi_loadings)`
  # prior, so with three factors `Z Z'` is a three-degree-of-freedom
  # Wishart around `Phi` rather than `Phi` itself. Over 2000 fresh
  # draws at that rank the agreement between a realised covariance
  # and `Phi` averages 0.40 with a standard deviation of 0.20, and
  # it climbs to 0.98 only by rank 200. So an assertion aimed at
  # `Phi` is aimed at a quantity that varies with the simulation
  # seed and not with the fit, which is why its threshold had to
  # come down to a tenth. The realised covariance is what a fit can
  # be held to, and against that one this fit reads 0.837 on a
  # bernoulli likelihood over 25 sites.
  sim <- sim_birds()
  fit <- birds_fit()
  off <- upper.tri(sim$Phi, diag = FALSE)
  rc <- residual_cor(fit)$cor
  expect_identical(rownames(rc), sim$species_levels)
  realised <- stats::cov2cor(tcrossprod(sim$Z_true))
  agree <- stats::cor(rc[off], realised[off])
  expect_gt(agree, 0.7)

  # And it is this species ordering that is recovered, not the
  # spread of the numbers. Against 500 permutations of the axis the
  # agreement sits 19 standard deviations out, so a fit that loaded
  # every species on another's factor fails here while still
  # producing a correlation matrix of the right shape.
  set.seed(11L)
  null <- replicate(500L, {
    p <- sample(sim$n_species)
    stats::cor(rc[off], realised[p, p][off])
  })
  expect_lt(mean(null >= agree), 0.01)
  expect_gt((agree - mean(null)) / stats::sd(null), 5)

  # The null has to be a null: a permutation that left the matrix
  # alone would score every replicate at the truth's own value and
  # the comparison above would be a number against itself.
  expect_lt(abs(mean(null)), 0.05)
  expect_gt(stats::sd(null), 1e-3)
})


test_that("birds: the wider prior moves the length-scales", {
  # Two fits, same data, differing only in the prior on the inverse
  # length-scales. The override has to change the answer rather than
  # merely be accepted, which is the whole point of comparing
  # against the published parameterisation.
  pars <- c("theta_features[1]", "theta_dist_phylo")
  post <- as_draws_matrix(birds_fit()$fit)
  wide <- as_draws_matrix(birds_fit(wide = TRUE)$fit)
  for (p in pars) {
    expect_true(all(is.finite(as.numeric(post[, p]))))
    expect_true(all(as.numeric(post[, p]) > 0))
    expect_true(all(as.numeric(wide[, p]) > 0))
  }
  # A wider prior does not have to widen every marginal, and here it
  # does not: the phylogenetic length-scale's spread grows by 89 per
  # cent while the trait one's falls by 38. So "at least one got
  # wider" is a coin flip over two parameters, and it would pass on
  # a run where the prior reached neither. What the override has to
  # do is move both, which an argument accepted and dropped cannot.
  ratio <- vapply(pars, function(p) {
    stats::sd(as.numeric(wide[, p])) / stats::sd(as.numeric(post[, p]))
  }, numeric(1))
  expect_true(all(abs(log(ratio)) > 0.25))

  # And the direction on the parameter the override names, so a
  # change that merely differed would not satisfy this.
  expect_gt(ratio[["theta_dist_phylo"]], 1)
})


# ---- 3. A phylogeny that dominates the trait -------------------------

sim_phylo <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    set.seed(2024L)
    n_species <- 30L
    n_sites <- 100L
    n_lv <- 3L
    lev <- paste0("sp", sprintf("%02d", seq_len(n_species)))
    k <- heaps_kernels(n_species, lev)

    # A smaller length-scale is a tighter kernel, so the phylogeny
    # dominates when its theta is the smaller of the two. At 0.9 the
    # trait kernel sits near one across the whole distance range and
    # carries almost nothing.
    theta_trait <- 0.9
    theta_phylo <- 0.3
    K_trait <- exp(-k$trait_dist / theta_trait)
    K_phylo <- exp(-k$phylo_dist / theta_phylo)
    Phi <- K_trait * K_phylo + diag(1e-6, n_species)

    Z_true <- t(chol(Phi)) %*%
      matrix(rnorm(n_species * n_lv), n_species, n_lv)
    lv_true <- matrix(rnorm(n_sites * n_lv), n_sites, n_lv)
    sigma_obs <- 0.5
    Y <- lv_true %*% t(Z_true) +
      matrix(rnorm(n_sites * n_species, 0, sigma_obs),
             nrow = n_sites, ncol = n_species)
    colnames(Y) <- lev
    d <- data.frame(
      series = factor(rep(lev, each = n_sites), levels = lev),
      time = rep(seq_len(n_sites), n_species),
      y = as.numeric(as.vector(Y))
    )
    cached <<- c(k, list(
      data = d, n_species = n_species, n_lv = n_lv,
      species_levels = lev, Phi = Phi, K_trait = K_trait,
      K_phylo = K_phylo, theta_trait = theta_trait,
      theta_phylo = theta_phylo, Z_true = Z_true
    ))
    cached
  }
})

phylo_fit <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    path <- cache_path("val_mvgam_heaps_birds_phylo_dominant.rds")
    if (file.exists(path)) {
      cached <<- readRDS(path)
      return(cached)
    }
    sim <- sim_phylo()
    cached <<- jsdgam(
      formula = y ~ 1, factor_formula = ~ -1,
      data = sim$data, unit = time, species = series,
      family = gaussian(), n_lv = sim$n_lv,
      traits = sim$trait_df, phylo = sim$tree,
      priors = c(
        brms::prior("normal(0, 3.162)", class = "theta_features"),
        brms::prior("normal(0, 3.162)", class = "theta_dist_phylo")
      ),
      chains = 2L, burnin = 500L, samples = 500L, silent = 2
    )
    saveRDS(cached, path)
    cached
  }
})


test_that("phylo: the simulation really is phylogeny-dominated", {
  # The premise, checked rather than assumed. If the two kernels
  # were equally informative the recovery claims below would be
  # asking the model to find something that is not there.
  sim <- sim_phylo()
  off <- upper.tri(sim$Phi, diag = FALSE)
  to_phylo <- stats::cor(sim$Phi[off], sim$K_phylo[off])
  to_trait <- stats::cor(sim$Phi[off], sim$K_trait[off])
  expect_gt(to_phylo, to_trait)
  expect_gt(to_phylo / to_trait, 1.5)

  # `Phi` is the prior the loadings were drawn from, so the two
  # lines above restate how `Phi` was built and would hold whatever
  # data came out of it. What the fit is shown is one draw, and the
  # premise it needs is that the draw kept the ordering: at rank 3
  # a realised covariance tracks its own prior at about 0.4, so
  # this is not automatic.
  realised <- stats::cov2cor(tcrossprod(sim$Z_true))
  expect_gt(stats::cor(realised[off], sim$K_phylo[off]),
            stats::cor(realised[off], sim$K_trait[off]))
})


test_that("phylo: the posterior finds the tighter kernel", {
  # A length-scale posterior that had collapsed, or that put the two
  # kernels the wrong way round, is still a set of finite positive
  # numbers, so the ordering between them is what has to be claimed.
  post <- as_draws_matrix(phylo_fit()$fit)
  trait <- as.numeric(post[, "theta_features[1]"])
  phylo <- as.numeric(post[, "theta_dist_phylo"])
  expect_true(all(is.finite(c(trait, phylo))))
  expect_true(all(phylo > 0))
  # A smaller length-scale is the tighter kernel, so the phylogeny
  # dominating means its theta sits below the trait's, and it has to
  # do so with real posterior mass rather than on the point
  # estimates alone.
  expect_gt(mean(phylo < trait), 0.8)
})


test_that("phylo: the recovered covariance follows the phylogeny", {
  # The third check the header promised. The implied species
  # correlation should track the phylogenetic kernel at the
  # posterior length-scale more closely than the trait kernel at
  # its own, mirroring the truth-level diagnostic above.
  sim <- sim_phylo()
  fit <- phylo_fit()
  post <- as_draws_matrix(fit$fit)
  off <- upper.tri(sim$Phi, diag = FALSE)
  rc <- residual_cor(fit)$cor
  expect_identical(rownames(rc), sim$species_levels)

  K_phylo_post <- exp(
    -sim$phylo_dist / mean(as.numeric(post[, "theta_dist_phylo"]))
  )
  K_trait_post <- exp(
    -sim$trait_dist / mean(as.numeric(post[, "theta_features[1]"]))
  )
  to_phylo <- stats::cor(rc[off], K_phylo_post[off])
  to_trait <- stats::cor(rc[off], K_trait_post[off])
  expect_gt(to_phylo, to_trait)

  # Both of those are small in absolute terms, 0.156 and 0.116, and
  # a reader would take that for a poor recovery. It is not. A
  # kernel is the covariance the loadings were drawn from, and the
  # data carries one rank-3 draw of them rather than the kernel
  # itself, so agreement with a kernel is capped by whatever that
  # draw happened to be. Against the covariance the draw actually
  # implies the fit reads 0.978, and it does so across the middle
  # of the distribution rather than on a few extreme pairs.
  realised <- stats::cov2cor(tcrossprod(sim$Z_true))
  agree <- stats::cor(rc[off], realised[off])
  expect_gt(agree, 0.9)
  expect_gt(stats::cor(rc[off], realised[off], method = "spearman"), 0.9)

  # And it is this species ordering that was recovered. Against 500
  # permutations of the axis the agreement sits far outside the
  # null, so a fit that loaded every species on another's factor
  # fails here while still returning a well-formed correlation
  # matrix and still satisfying the kernel comparison above.
  set.seed(7L)
  null <- replicate(500L, {
    p <- sample(nrow(realised))
    stats::cor(rc[off], realised[p, p][off])
  })
  expect_lt(mean(null >= agree), 0.01)
  expect_gt((agree - mean(null)) / stats::sd(null), 5)
  expect_lt(abs(mean(null)), 0.05)
  expect_gt(stats::sd(null), 1e-3)
})
