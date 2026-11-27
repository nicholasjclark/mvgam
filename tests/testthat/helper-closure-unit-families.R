# Helpers shared by test-closure-unit-families.R and test-extra-closure-unit-families.R.

# ------------------------------------------------------------
# diri(): family registration, attribute tagging,
# closure-unit grouping, and Stan emission round-trip
# ------------------------------------------------------------

make_dirichlet_long_data <- function(n_sites = 6L, n_species = 4L,
                                     seed = 11L) {
  set.seed(seed)
  species_levels <- paste0("y", seq_len(n_species))
  alpha <- exp(matrix(rnorm(n_sites * n_species, sd = 0.3),
                      n_sites, n_species))
  Y <- t(apply(alpha, 1L, function(a) {
    d <- rgamma(n_species, shape = a, rate = 1)
    d / sum(d)
  }))
  Y <- pmax(Y, 1e-4); Y <- Y / rowSums(Y)
  data.frame(
    series = factor(rep(species_levels, times = n_sites),
                    levels = species_levels),
    time   = rep(seq_len(n_sites), each = n_species),
    y      = as.vector(t(Y)),
    env    = rep(rnorm(n_sites), each = n_species)
  )
}

# ------------------------------------------------------------
# multi(): family registration + Stan emission for multinomial
# ------------------------------------------------------------

make_multi_long_data <- function(n_sites = 6L, n_species = 4L,
                                  seed = 21L) {
  set.seed(seed)
  species_levels <- paste0("y", seq_len(n_species))
  rows <- list()
  for (s in seq_len(n_sites)) {
    p <- exp(rnorm(n_species, sd = 0.4))
    p <- p / sum(p)
    N_s <- sample(30:80, 1L)
    y_s <- as.vector(rmultinom(1L, N_s, p))
    env_s <- rnorm(1L)
    for (k in seq_len(n_species)) {
      rows[[length(rows) + 1L]] <- data.frame(
        series = factor(species_levels[k], levels = species_levels),
        time   = s,
        y      = y_s[k],
        env    = env_s
      )
    }
  }
  do.call(rbind, rows)
}

# ------------------------------------------------------------
# categ(): family registration + Stan emission for categorical
# ------------------------------------------------------------

make_categ_long_data <- function(n_sites = 8L, n_categories = 4L,
                                  seed = 31L) {
  set.seed(seed)
  species_levels <- paste0("c", seq_len(n_categories))
  rows <- list()
  for (s in seq_len(n_sites)) {
    p <- exp(rnorm(n_categories, sd = 0.5))
    p <- p / sum(p)
    cat_obs <- sample.int(n_categories, 1L, prob = p)
    env_s <- rnorm(1L)
    for (k in seq_len(n_categories)) {
      rows[[length(rows) + 1L]] <- data.frame(
        series = factor(species_levels[k], levels = species_levels),
        time   = s,
        y      = as.integer(k == cat_obs),
        env    = env_s
      )
    }
  }
  do.call(rbind, rows)
}

# ------------------------------------------------------------
# mvn(): family registration + Stan emission for the
# multivariate normal closure-unit family.
# ------------------------------------------------------------

make_mvn_long_data <- function(n_sites = 6L, n_species = 4L,
                                seed = 41L) {
  set.seed(seed)
  species_levels <- paste0("y", seq_len(n_species))
  rows <- list()
  for (s in seq_len(n_sites)) {
    env_s <- rnorm(1L)
    eps <- rnorm(n_species)
    for (k in seq_len(n_species)) {
      rows[[length(rows) + 1L]] <- data.frame(
        series = factor(species_levels[k], levels = species_levels),
        time   = s,
        y      = eps[k] + 0.4 * env_s,
        env    = env_s
      )
    }
  }
  do.call(rbind, rows)
}

# The five multi-response families, the frame each is fitted to, and
# what separates them: the response type, whether the likelihood is
# shift-invariant (a simplex) and the Stan data each family declares.
multi_response_cases <- function() {
  unit_vars <- c("N_unit", "n_rep", "visit_idx")
  list(
    diri = list(fam = diri(), type = "real", simplex = TRUE,
                dpars = c("mu", "phi"), vars = unit_vars,
                data = make_dirichlet_long_data(5L, 4L)),
    multi = list(fam = multi(), type = "int", simplex = TRUE,
                 dpars = "mu", vars = unit_vars,
                 data = make_multi_long_data(5L, 4L)),
    categ = list(fam = categ(), type = "int", simplex = TRUE,
                 dpars = "mu", vars = unit_vars,
                 data = make_categ_long_data(5L, 4L)),
    # A normal or Student-t likelihood moves with a shift of mu, which
    # rules out the simplex identification. Both families index their
    # scale by the component of each row.
    mvn = list(fam = mvn(), type = "real", simplex = FALSE,
               dpars = "mu", vars = c(unit_vars, "visit_component", "Psi"),
               data = make_mvn_long_data(5L, 4L)),
    mvt = list(fam = mvt(), type = "real", simplex = FALSE,
               dpars = "mu",
               vars = c(unit_vars, "visit_component", "Psi", "nu"),
               data = make_mvn_long_data(5L, 4L))
  )
}

# ------------------------------------------------------------
# build_closure_unit_arrays()
# ------------------------------------------------------------

# Helper used across tests in this file.
make_nmix_data <- function(n_unit = 4, n_visit = 3, seed = 1) {
  set.seed(seed)
  n_total <- n_unit * n_visit
  data.frame(
    series = factor(rep(seq_len(n_unit), each = n_visit)),
    time   = rep(1L, n_total),
    visit  = rep(seq_len(n_visit), n_unit),
    y      = pmin(rpois(n_total, 5), 20L),
    cap    = rep(20L, n_total),
    elev   = rep(rnorm(n_unit), each = n_visit),
    tod    = stats::runif(n_total)
  )
}

# ------------------------------------------------------------
# Stan emission contract tests via brms make_stancode round-trip
# ------------------------------------------------------------
#
# These tests exercise the data-prep hook in
# `generate_stan_components_mvgam_formula()` to confirm that
# closure-unit arrays land in standata and the nmix lpdf
# function block lands in stancode with the expected signature.

# The four detection families: the data each is fitted to, the Stan
# name of its likelihood, its likelihood call and the data it
# declares. A binary response bounds Y_max at one. The Poisson-binomial
# and Poisson-Poisson bodies take log(k) from a lookup table.
detection_cases <- list(
  occ = list(
    family = occ, binary = TRUE, name = "occ", k_max = FALSE,
    log_n_lookup = FALSE,
    call = "occ_lpmf(Y | mu, p, N_unit, n_rep, Y_max, visit_idx)"
  ),
  nmix = list(
    family = nmix, binary = FALSE, name = "nmix", k_max = TRUE,
    log_n_lookup = TRUE,
    call = paste0("nmix_lpmf(Y | mu, p, N_unit, n_rep, K_max, Y_max, ",
                  "visit_idx, log_n_lookup)")
  ),
  royle_nichols = list(
    family = function() nmix("royle_nichols"), binary = TRUE,
    name = "nmix_royle_nichols", k_max = TRUE, log_n_lookup = FALSE,
    call = paste0("nmix_royle_nichols_lpmf(Y | mu, p, N_unit, n_rep, ",
                  "K_max, Y_max, visit_idx)")
  ),
  poisson_poisson = list(
    family = function() nmix("poisson_poisson"), binary = FALSE,
    name = "nmix_poisson_poisson", k_max = TRUE, log_n_lookup = TRUE,
    call = paste0("nmix_poisson_poisson_lpmf(Y | mu, p, N_unit, n_rep, ",
                  "K_max, Y_max, visit_idx, log_n_lookup, k_start_ppm)")
  )
)

# The first request for a family generates its program and the cache
# returns it to every later test in the file
detection_stancode_cache <- new.env(parent = emptyenv())
detection_stancode <- function(key) {
  if (is.null(detection_stancode_cache[[key]])) {
    case <- detection_cases[[key]]
    d <- make_nmix_data(n_unit = 4, n_visit = 3)
    if (case$binary) d$y <- as.integer(d$y > 0L)
    detection_stancode_cache[[key]] <- as.character(stancode(
      mvgam_formula(y ~ elev), data = d, family = case$family(),
      validate = FALSE
    ))
  }
  detection_stancode_cache[[key]]
}
