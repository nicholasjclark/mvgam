# Dev Tasks: mvgam 2.0 release survey

**Source:** post-fit sweep over the cached fixtures plus three
read-only survey agents. No TRD.
**Generated:** 2026-08-25

## Relevant Files

**Created** — `tests/local/postfit_sweep.R` drives every post-fit
method against every cached fit and logs a status and shape per call
(`--fixtures=`, `--groups=` to filter).
`tests/local/test-hierarchical-trends.R` covers `gr`/`subgr` fits end
to end. `R/mvgam_families.R` restores the family hub topic.
`R/stan_source.R` holds the rules for reading the Stan source brms
generated. `tests/local/test-new-levels.R`,
`tests/local/test-update-trend-args.R`,
`tests/testthat/test-multi-response-kernels.R` and
`tests/testthat/test-mvgam-core.R` cover what the sweep could not
reach. `mvgam_codegen_options()` in `R/brms_integration.R` carries the
brms code-generation settings from one place to every generator that
needs them, and `tests/testthat/test-stancode-standata.R` pins each of
their journeys.

**Notes** — long commands run in the background and write to a log
that is then read. Fixture-dependent tests live in `tests/local/`; CI
tests never skip on a missing fixture and the CI sweep stays under ten
minutes. Cached fits are read once, never re-fitted to inspect.

## Tasks

- [ ] **14.0 One column, one scale, everywhere**
  > A latent factor column carries a loadings scale and an
  > innovation scale, and only their product reaches the
  > likelihood. Fixed for `column_shrinkage = "mgp"`, where
  > `sigma_trend` is now derived as `sqrt(Psi_diag)` and the
  > loadings prior draws `Z` at unit scale. Every other factor path
  > still has it: `RW()`, `AR()` and `ZMVN()` factor models, and the
  > jsdgam default, `"iid"` and distances-only routes, all sample a
  > free `Z` against a free `sigma_trend`.
  >
  > Measured on `AR(p = 1, n_lv = 2)`, four series, 40 timepoints:
  > `Z[1,2]` at bulk ESS 6.1 and R-hat 1.27, `sigma_trend[1]` at
  > 21.5, while the scale- and rotation-invariant
  > `sum_k sigma_trend[k]^2 * ||Z_tilde[, k]||^2` reaches 202. The
  > pathology sits in the split, not in the quantity.
  >
  > The QR step fixes rotation for reporting but runs in generated
  > quantities, so it does nothing for the sampler's geometry along
  > the scale direction. Only the priors pin the split.
  >
  > Apply the device the MGP branch now uses: give `Z` a fixed unit
  > scale and let `sigma_trend` carry the column magnitude alone.
  > The kernel routes need it too, since `Phi` has unit diagonal and
  > so contributes no scale of its own.

- [ ] **14.1 VAR samples its latent state centred**
  > `generate_var_trend_stanvars()` declares `lv_trend` directly in
  > `parameters` and samples it as
  > `lv_trend[t, ]' ~ multi_normal(mu_t_trend[t], Sigma_trend)`,
  > where `Sigma_trend` is built from the sampled `sigma_trend` and
  > `L_Omega_trend`. The conditional density tightens with the
  > sampled scale, which is a funnel. `init_trend` is the same
  > against `Omega_trend`.
  >
  > Every other Gaussian-innovation trend is written non-centred
  > from `innovations_trend ~ std_normal()`, and VAR's own
  > `A_raw_trend` sub-priors are too. The state is the exception.
  >
  > Measured on a plain `VAR()`, four series, 40 timepoints: E-BFMI
  > 0.295 on one chain against Stan's 0.3 threshold, 17 of 800
  > divergences, and `trace(Sigma_trend)` at bulk ESS 32.6. This is
  > the default VAR call, so it reaches every VAR user.
  >
  > Sample a raw `std_normal()` matrix and build
  > `lv_trend[t, ]' = mu_t_trend[t] + L_Sigma_trend * z[t, ]'`;
  > `L_Sigma_trend` is already computed. Same for `init_trend`
  > through `cholesky_decompose(Omega_trend)`.

- [ ] **14.2 `VAR(n_lv = k)` does not compile**
  > `R/stan_assembly.R:3307` declares
  > `array[size(A_trend)] matrix[N_lv_trend, N_lv_trend] A_trend_tilde;`
  > in generated quantities. Stan rejects a non-data expression as a
  > top-level array size; `N_lags_trend` is already a data integer
  > and is what was meant. Every VAR and VARMA factor model fails at
  > `stanc`, so a capability the registry advertises
  > (`supports_factors = TRUE` for VAR) has never worked.
  >
  > Reproduced identically before and after 14.0's fix, so it is not
  > a regression from it. Blocks testing 14.0 and 14.1 together.

- [ ] **15.0 The prior a model reports is not always the prior it samples**
  > Decision 3.1 fixed this for `sigma_trend` on the fitted object.
  > The same shape is still live on `Z`, which carries the whole
  > factor and JSDM story. One registry entry, three emission sites,
  > four strings:
  >
  > | path | reported | sampled |
  > |---|---|---|
  > | free factor `n_lv` | `student_t(3, 0, 0.5)` | agrees |
  > | MGP | `student_t(3, 0, 0.5)` | `std_normal()` |
  > | partial `trend_map` | `student_t(3, 0, 0.5)` | `student_t(3, 0, 1)` |
  > | kernel | `student_t(3, 0, 0.5)` | `multi_normal_cholesky` |
  >
  > `get_prior.mvgam_formula()` takes neither `loadings_prior` nor
  > `trend_map`, so it cannot know which branch will fire; passing
  > either is swallowed by the dots and the table is unchanged.
  >
  > Do 15.1 first: it supplies the predicate this needs.

- [ ] **15.1 Which classes belong to mvgam, written four times**
  > `filter_obs_priors()` and `filter_trend_priors()` (R/priors.R:763,
  > 793) test a bare `_trend$` regex;
  > `get_all_mvgam_trend_parameters()` (R/priors.R:809) builds a list;
  > `mvgam_unsuffixed_params` (R/brms_integration.R:645) carries the
  > regex plus an exception list; `suffix_trend_prior_classes()`
  > (R/priors.R:1034) adds a `sigma`-with-empty-coef special case.
  >
  > They disagree exactly on `Z`, which has no `_trend` suffix. A
  > user prior on `Z` is filed as observation-side, handed to brms,
  > and rejected with a message pointing them at `default_prior()`,
  > which is where they read the class name. `Psi` is routed the
  > same way.
  >
  > One predicate built from `mvgam_unsuffixed_params` plus the
  > suffix rule, read by both filters, the suffixer and
  > `mvgam_stancode_prior_rows()`.

- [ ] **15.2 `PW()` advertises a parameter it does not sample**
  > `get_prior()` on a `PW()` spec lists `sigma_trend`. The emitted
  > program contains no such parameter, and
  > `prior(exponential(9), class = sigma_trend)` produces neither an
  > error nor a `9` anywhere in the Stan. The sibling of the MGP case
  > 14.0 fixed, except that one now refuses and this one is silent.
  >
  > Two unconditional copies of one fact: `base_params <-
  > c("sigma_trend")` (R/trend_system.R:623) feeding `get_prior()`,
  > and `all_mvgam_params` (R/priors.R:838) feeding the suffix
  > stripper. They already disagree for `PW()`, over `nu_trend`.
  > Make the innovation scale a property of the trend registration.

- [ ] **15.3 AR bounds stated three times, with two values**
  > The registry says `c(-1, 1)` (R/priors.R:64), the type fallback
  > says `[-1, 1]` (R/priors.R:521), and `get_ar_parameter_prior()`
  > (R/priors.R:593) says `[-0.99, 0.99]`, which is what the user is
  > shown. Stan declares `vector<lower=-1,upper=1>`. Nothing reads
  > the tightened bound. `get_car_parameter_prior()` is the same
  > shape and does agree with its declaration, so the pattern is
  > right and AR is the outlier. Delete the AR resolver.

- [ ] **15.4 `get_prior(fit)` and `prior_summary(fit)` disagree**
  > Both document themselves as returning the table the model was
  > fitted with; both re-derive. `get_prior()` drops the
  > stanvar-lifted rows and invents a `b_trend` the fit never
  > sampled, because the no-trend-predictors guard tests one
  > spelling: `~ -1 + AR(p = 1)` is not `~ 0`, so brms is asked and
  > returns a `b` row. `~ 0 + AR()` and `~ AR()` do not.
  >
  > The guard also reads `!all.equal(...) == TRUE`, which works only
  > because `all.equal` happens to return length one here.

- [ ] **15.5 The jsdgam prior surface is unreachable**
  > `get_prior()` on a jsdgam specification reports `b`, `Intercept`
  > and `shape` and no trend classes at all, so a user cannot see
  > the priors they might set. The cause is structural:
  > `mvgam_formula()` holds `formula` and `trend_formula` only,
  > while `jsdgam()` carries its factor structure in `trend_map` and
  > `loadings_prior`. No `mvgam_formula` can describe a jsdgam fit.
  >
  > The plain path is correct: `~ ZMVN(n_lv = 2)` does report `Z`.
  > Give `get_prior.mvgam_formula()` the `trend_map` and
  > `loadings_prior` arguments `stancode.mvgam_formula()` already
  > takes, which also closes 15.0.

- [ ] **15.6 Length-scale priors cannot be set**
  > `theta_features` and each `theta_dist_<name>` are emitted as
  > literal `lognormal(0, 1)` targets (R/stan_assembly.R:3448, 3477)
  > with no registry entry. `get_prior()` never lists them and
  > `prior_summary()` does, so a user sees a row they cannot change.
  > For a phylogenetic kernel that length-scale is a modelling
  > choice, not a nuisance.
  >
  > Separately, `R/stan_assembly.R:3326` claims the `normal(0, 1)`
  > default matches Heaps' practice; the paper uses `N(0, 10)`.

- [ ] **16.0 Smaller things the sweeps turned up**
  > - `prior(constant(1), class = sigma_trend)` is accepted and
  >   emits `sigma_trend ~ constant(1);`, which has no
  >   `constant_lpdf` and fails at `stanc`. Legal brms input,
  >   invalid Stan out.
  > - `AR(p >= 2)` boxes each coefficient in `(-1, 1)`
  >   independently. The stationary region of an AR(2) is a triangle
  >   strictly inside that box, so the prior admits non-stationary
  >   draws. VAR already uses the Heaps `AtoP` mapping.
  > - `CAR()` and `AR(p = 2)` start the state from
  >   `Normal(0, sigma_trend)` rather than the stationary variance.
  >   AR(1)-style trends already divide by `sqrt(1 - phi^2)`.
  > - `ZMVN()` on a single series makes
  >   `sigma_trend^2 + sigma^2` an exact sum with nothing to split
  >   it. Narrow, and analytic rather than measured.
  > - `pkgdown/jsdgam_cache/figs/` holds 11 tracked PNGs, 1.4 MB,
  >   referenced by nothing.

- [ ] **13.1 The migration the names still need**
  > Deferred from 13.0 because each renames something a fitted object
  > carries, so cached fixtures stop being readable until they are
  > rebuilt. Belongs with 5.0's refit rather than ahead of it.
  >
  > `trend_model` means three things. It is a constructor when passed
  > (`sim_mvgam(trend_model = AR())`), a character type name on a
  > trend spec, and a `brmsfit` on the fitted object: `class(
  > fit$trend_model)` is `"brmsfit"`, the trend-side prefit, across
  > 74 read sites. `tidier_methods.R:170` reaches
  > `x$trend_model$trend_model`, which is the collision in one
  > expression. `trend_prefit` is the name that describes the slot.
  >
  > `process_error` still means two things. 13.0 gave it the
  > innovation axis everywhere, but `log_lik()` keeps it as the
  > superseded spelling of the surface, where innovations are pinned
  > off. Back-compatibility is the reason and `incl_dynamics` is the
  > 1.x name that would serve instead.
  >
  > `trend_arg_metadata` names the trend-constructor arguments whose
  > values a fit stores, `trend_map` and `n_lv`, and
  > `mvgam_update_inheritance` names the top-level ones. Those are two
  > lists of one idea, an argument whose value the object can give
  > back, and nothing makes them agree. A constructor naming any other
  > out-of-scope variable, `AR(p = 1, gr = my_grouping)` say, still
  > fails on refit.
  >
  > Three Stan names still break the `_trend` suffix rule:
  > `time_dis`, `theta_features` and `varrho_inv`. `time_dis` reaches
  > into the compiled C++ signatures in `RcppExports.R`, and the
  > other two are parameters, so renaming them changes posterior
  > column names and every fit carrying a structured loadings prior
  > stops being readable. `N_free_Z` puts its qualifier before the
  > noun where every sibling puts it after.

- [ ] **5.2 Three CRAN vignettes render with no output on the website**
  > `data.Rmd`, `dfm.Rmd` and `mvgam_overview.Rmd` gate every chunk on
  > `params$EVAL`, which reads `NOT_CRAN`. That gate is right for CRAN,
  > where a vignette must not fit Stan models. But
  > `.github/workflows/pkgdown.yaml` never sets `NOT_CRAN` either, so
  > the published site shows all three as code listings with no
  > results, and their code has never run in CI.
  >
  > 5.1 is the reason this matters: the one article whose chunks had
  > never executed was hiding two package bugs and four false claims.
  >
  > All three have since been rendered with the gate open, and each
  > carried something. `data.Rmd` overstated a claim the validator
  > did not support, which is 5.3. `dfm.Rmd` drew a conclusion its
  > own scores contradicted. `mvgam_overview.Rmd` displayed priors
  > the model does not use and built its comparison on a simulation
  > that cannot be recovered, which is 5.4. What remains is setting
  > `NOT_CRAN: true` in `.github/workflows/pkgdown.yaml`, which
  > belongs with 5.0's rebuild so the site is published from fits
  > made by the shipping version.

- [ ] **5.0 Rebuild every vignette and the pkgdown site**
  > Five articles still hold numbers from June and July caches:
  > `var.Rmd`, `hierarchical_var.Rmd`, `mvbf.Rmd`, `jsdgam.Rmd` and
  > `idm.Rmd`. Each carries one inline R expression, so every figure
  > in its prose is frozen text. `nmix.Rmd` (5.1) and
  > `forecast_evaluation.Rmd` (5.5) are done; `data.Rmd`, `dfm.Rmd`
  > and `mvgam_overview.Rmd` are rendered but still need the
  > workflow gate in 5.2.
  >
  > Per article, in order:
  >
  > 1. `R CMD INSTALL --preclean --no-multiarch .` first. See the
  >    rule below; skipping this invalidates everything after it.
  > 2. Re-run the builder in `tests/local/*_vignette_fits.R` where
  >    one exists. `idm_cache` and `var_cache` have no builder and
  >    need one written, or they cannot be regenerated from a clean
  >    clone.
  > 3. Render, then read the rendered output and check every prose
  >    claim against the block printed directly above it.
  > 4. Replace each quoted number with inline R computed from the
  >    object, so the next stale cache cannot go unnoticed.
  > 5. Check the sampler diagnostics the article prints, and say
  >    something about them if they are poor.
  > 6. `prose_lint.py`, then a writing pass.
  >
  > Two rules, both learned by getting them wrong.
  >
  > Install from HEAD before rendering. Every article calls
  > `library(mvgam)`, so it renders against the installed build
  > rather than the working tree, and the installed build carries
  > the same `2.0.0` version string as HEAD whatever its age.
  > Nothing warns. The build in place during 5.5 dated from 24 July
  > and still scored the marginal likelihood, so the article
  > re-rendered to its original numbers and read as consistent with
  > prose written against them. Refitting does not save you: the
  > builders use `load_all()`, but every `loo()`, `score()` and
  > `lfo_cv()` call inside the article goes through the installed
  > code.
  >
  > Do not retire a model on its forecast score. In 5.5 the energy
  > score said the VAR earned nothing over a per-species AR, and
  > that was read as a verdict on the model. The transition matrix
  > and the innovation covariance answer different questions and are
  > not equally identified: `A_trend` reached a bulk ESS of 26 where
  > the covariance reached 322, and the VAR recovered a residual
  > correlation of 0.58 between two congeneric pocket mice that an
  > AR cannot represent at any horizon. Wherever an article compares
  > models, check what the losing model estimates before dropping
  > it, and keep forecast accuracy and structural inference apart.

- [ ] **6.0 Final release verification**
  > Clean `document()`, clean test sweep, `R CMD check --as-cran`,
  > tarball under the size limit, sweep green.

## Behaviour confirmed, worth documenting

`posterior_epred()` and `posterior_predict()` default to
`process_error = FALSE` and `incl_autocor = FALSE`, so the trend
contributes its deterministic submodel and the answer is the same on
every call. Passing the training data back as `newdata` is an exact
no-op. Under `process_error = TRUE` innovations are drawn afresh, so
two calls on one fit and the same `draw_ids` differ, by as much as
160 units on a cached Poisson fit; reproducible output there needs a
seed.
