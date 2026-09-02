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
reach. `val_mvgam_mgp_ceiling.rds` is the one cached fit at
`n_lv = n_series`, built by `tests/local/build_fixtures.R` and
driven by `tests/local/test-factor-forecast.R`.
`mvgam_codegen_options()` in `R/brms_integration.R` carries the
brms code-generation settings from one place to every generator that
needs them, and `tests/testthat/test-stancode-standata.R` pins each of
their journeys.

**Notes** — long commands run in the background and write to a log
that is then read. Fixture-dependent tests live in `tests/local/`; CI
tests never skip on a missing fixture and the CI sweep stays under ten
minutes. Cached fits are read once, never re-fitted to inspect.

## Tasks

- [ ] **14.0 The factor labels move between chains, and `ar1_trend` carries it**
  > Recorded first as two free scales on a column, then as a
  > rotation. Neither. Every route draws `Z` at a fixed scale, so
  > there is one free scale per column and no ridge; and arbitrary
  > rotation is not a symmetry here, because mixing two AR(1)
  > factors with different coefficients does not give an AR(1).
  > The autoregressive dynamics break rotation and leave the
  > discrete symmetry: column permutation with sign.
  >
  > Measured on the cached four-series `n_lv = 2` AR(1) fit. The
  > per-chain posterior means of `ar1_trend` are (0.652, 0.367) and
  > (0.403, 0.614): the same pair, swapped. Chain 1's first
  > loadings column sits on series 1 and 2; chain 2's second column
  > sits there with the sign flipped. Label switching, not a
  > continuous family.
  >
  > The QR step handles it for what it covers. `Z_tilde` agrees
  > across chains to two decimals, so its triangular form fixes
  > both the sign and the ordering, and every post-fit surface
  > reads it: `ordinate()`, `compare_loadings()`, `residual_cor()`
  > and `active_factors()` are unaffected.
  >
  > `ar1_trend` is not covered. It belongs to the unpermuted
  > column, nothing relabels it, and `summary()` reports it: R-hat
  > 1.12 to 1.17 and bulk ESS 9 to 14 on a fit whose `Z_tilde` is
  > clean and whose `sigma_trend` reaches 1.02 and ESS 76 to 293.
  > A user reads that as a model that has not converged. Raw `Z`
  > has the same problem for the same reason.
  >
  > Identifying rotation at sampling time is declined: it would
  > make inference depend on the order the series arrive in, and
  > truncate rather than match the kernel prior
  > `Z[, k] ~ MVN(0, Phi)`.
  >
  > What is left is relabelling after the fact, which is the usual
  > treatment for label switching and needs no constraint. Match
  > each chain's columns to a reference by their loadings, then
  > permute `Z`, `lv_trend` and the per-column dynamics parameters
  > together. `sign_canonicalise_factors()` already walks the
  > per-chain samples to do exactly this for sign, and skips
  > whenever `Z_tilde` is present on the grounds that QR settled
  > it; that reasoning holds for `Z` and not for `ar1_trend`.
  > Failing that, say in `summary()` that per-column dynamics
  > parameters carry label switching on a factor model and that
  > `Z_tilde` is the identified quantity.
  >
  > Separately and cheaply: the default `Z` prior is
  > `student_t(3, 0, 0.5)` on four routes and Gaussian on the
  > other four. Harmonising to `std_normal()` is one string in
  > `common_trend_priors` and makes the default agree with the
  > kernel and MGP branches. A prior harmonisation, not a
  > geometry fix.
  >
  > Judge any change here across seeds. An earlier entry chased an
  > E-BFMI of 0.295 on a single `VAR()` draw, implemented two
  > parameterisations against it and reverted both: across four
  > seeds the E-BFMI, the divergence count and the ESS each moved
  > further between seeds than between parameterisations.

- [ ] **15.5b `get_prior()` refuses a `trend_map` it could describe**
  > `get_prior.mvgam_formula()` already takes `trend_map` and
  > `loadings_prior`, and the plain path is right, with
  > `~ ZMVN(n_lv = 2)` reporting `Intercept`, `Z`, `sigma_trend`
  > and `L_Omega_trend`.
  >
  > What it does with a `trend_map` is refuse, saying fixed
  > loadings move `Z` to the data block and partial loadings
  > replace it with `Z_free_vec`, so the classes differ from the
  > free-loadings table, and pointing at `stancode()`. That is
  > honest and better than a wrong table, but it withholds an
  > answer that exists.
  >
  > A fully fixed map is exactly describable. Reading the priors
  > off the emitted program for a four-series map onto two
  > factors gives two settable classes, `sigma_trend` at
  > `exponential(2)` and `ar1_trend` at `normal(0, 0.5)`, and no
  > `Z` row at all, since `Z` is data. So the table for that case
  > is the free-loadings table minus `Z`, and the refusal can
  > narrow to the partial case.
  >
  > The partial case needs the matrix spelling to reproduce: a
  > `trend_map` data frame carrying a `trend` column is validated
  > as positive integers, and it is the `[n_series, n_lv]` matrix
  > form whose `NA` cells mark free loadings. Confirm what
  > `Z_free_vec` looks like in that program before deciding
  > whether it too can be described or should keep the refusal.

- [ ] **16.0 Two parameterisation changes, both declined**
  > Kept so neither is raised again as an oversight.
  >
  > `AR(p >= 2)` boxes each coefficient in `(-1, 1)` independently
  > while the stationary region of an AR(2) is a triangle strictly
  > inside that box, so the prior admits non-stationary draws. The
  > Heaps `AtoP` and `rev_mapping` functions the VAR generator uses
  > would fix it and reduce to the scalar case at `m = 1`. Adopting
  > them would move what a user's `prior(class = ar2_trend)`
  > constrains onto the partial-autocorrelation scale and rename
  > posterior columns. The coefficient scale stays.
  >
  > `CAR()` and `AR(p = 2)` start the state from `Normal(0,
  > sigma_trend)` rather than the stationary variance, where AR(1)
  > divides by `sqrt(1 - phi^2)`. The comment at the branch names
  > the Yule-Walker solve this needs. Left alone for the same
  > reason: it changes the posterior of every AR(p>1) and CAR fit,
  > so it belongs with a deliberate parameterisation pass rather
  > than a release sweep.

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

- [ ] **5.2 The published site is not the site these sources describe**
  > Three separate faults, and only the first was visible from the
  > vignette sources alone.
  >
  > `data.Rmd`, `dfm.Rmd` and `mvgam_overview.Rmd` gate every chunk
  > on `params$EVAL`, which reads `NOT_CRAN`. The gate is right for
  > CRAN, where a vignette must not fit Stan models, but
  > `.github/workflows/pkgdown.yaml` sets `GITHUB_PAT` and
  > `R_KEEP_PKG_SOURCE` and never sets `NOT_CRAN`, so a CI build
  > renders all three as code listings with no results.
  >
  > The tracked `docs/` build appears to contradict that:
  > `mvgam_overview.html` carries real figures, so its chunks ran.
  > The gate predates it, having been in place since September 2024
  > against a build committed in February 2025. The explanation is
  > that `docs/` was built locally and committed, and an interactive
  > `devtools` session sets `NOT_CRAN` itself. The evidence that the
  > site is fine therefore comes from a build that never went
  > through the workflow, which is the same trap 5.0 records for
  > articles rendered against a stale installed package.
  >
  > Second, and worse: eight of the ten articles have no published
  > page at all. `docs/articles/` holds `data_in_mvgam`,
  > `nmixtures`, `shared_states`, `time_varying_effects` and
  > `trend_formulas`, none of which still exists as a source, and
  > lacks `data`, `dfm`, `var`, `hierarchical_var`, `mvbf`,
  > `jsdgam`, `idm` and `nmix`, all of which do. `_pkgdown.yml`
  > names every one of the missing ones in its `articles:` section,
  > so the article index links to pages that are not there.
  >
  > Third, two workflow settings keep it that way. The deploy step
  > passes `clean: false`, so the five renamed pages stay on
  > `gh-pages` with nothing to remove them, and
  > `build_site_github_pages(lazy = TRUE)` skips any article whose
  > HTML is newer than its source, which is exactly the stale page
  > that needs rebuilding.
  >
  > So this is not the one-line change it was written as. It needs
  > `NOT_CRAN: true`, `clean: true` on the deploy or an explicit
  > purge of the five dead pages, and a decision on `lazy`. Sequence
  > it with 5.0: turning the gate on now would publish articles
  > built on sampling that 14.0 changes.
  >
  > Unverified, worth checking during 5.0: the workflow pins
  > `pkgdown` to 2.0.9, released in 2022, while `_pkgdown.yml` now
  > carries a full `reference:` and `articles:` structure.
  >
  > The manual points both ways across the same gap.
  > `man/mvgam_use_cases.Rd` links to `shared_states`,
  > `time_varying_effects` and `trend_formulas`, which resolve only
  > because the published site is the January build and have no
  > source in `vignettes/`; they die when the site is rebuilt.
  > Meanwhile `man/ZMVN.Rd`, `man/jsdgam.Rd`, `man/mvgam.Rd`,
  > `man/irf.mvgam.Rd`, `man/fevd.mvgam.Rd`, `man/nmix.Rd` and
  > others link to `idm`, `jsdgam`, `mvbf`, `nmix` and `var`, which
  > have source and no published page, so they 404 today and the
  > rebuild fixes them. Decide what the first three should point at
  > before rebuilding, since afterwards nothing will resolve them.
  >
  > 5.1 is why this matters. The one article whose chunks had never
  > executed was hiding two package bugs and four false claims. All
  > three gated vignettes have since been rendered with the gate
  > open and each carried something: `data.Rmd` overstated a claim
  > the validator does not support, which is 5.3; `dfm.Rmd` drew a
  > conclusion its own scores contradict; `mvgam_overview.Rmd`
  > displays priors the model does not use and builds its comparison
  > on a simulation that cannot be recovered, which is 5.4.

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

- [ ] **17.0 What `R CMD check --as-cran` reports**
  > The check reports no WARNING and one actionable NOTE:
  > `predict(type = "variance")` answers, `ev` is declared in the
  > single `globalVariables()` block, and `Rplots.pdf` stays out of
  > the tarball. What remains is the
  > non-portable compiler flag, which comes from the local
  > Makevars rather than the package, and the URL NOTE.
  >
  > Two things the run turned up that are worth more than the
  > NOTEs.
  >
  > `--run-donttest` fits Stan models. There are 168 `mvgam()` or
  > `jsdgam()` calls across 59 help files, 55 of them inside
  > `\donttest{}`, and CRAN runs those blocks. The check has been
  > sampling for over an hour without reaching the end of them.
  > This wants a decision before submission: move the fits behind
  > `\dontrun{}`, cut them to a handful of tiny ones, or accept a
  > check time that CRAN may refuse.
  >
  > Five of the flagged URLs are
  > `nicholasjclark.github.io/mvgam/articles/{idm,jsdgam,mvbf,nmix,var}.html`,
  > returning 404 from `man/ZMVN.Rd`, `man/jsdgam.Rd`,
  > `man/mvgam.Rd`, `man/irf.mvgam.Rd`, `man/fevd.mvgam.Rd` and
  > others. Those articles have no published page, which is 5.2
  > read from the manual rather than from the site. The remaining
  > three are handled: both GitHub redirects in `man/occ.Rd` point
  > at `ecoverseR` after confirming the 301s, and the Stan forum
  > thread in `man/mvgam_use_cases.Rd` is removed, having 404'd on
  > both its id and its slug.

- [ ] **19.0 The family scales have the defect 15.6 closed for the kernel**
  > `Psi`, the residual scale of `mvn()` and `mvt()`, and `nu`,
  > the mvt degrees of freedom, are emitted as literals in
  > `R/families.R`. A survey corrected three things the entry
  > first claimed, each of which shrinks the work.
  >
  > `nu` is not in `prior_summary()` either. It is invisible on
  > every surface: `mvgam_stancode_prior_rows()` reads the
  > `target +=` form with a regex wanting an identifier straight
  > before the `|`, and `gamma_lpdf(nu - 2 | 2, 0.1)` does not
  > match, while `is_mvgam_param("nu")` is FALSE anyway.
  >
  > The refusal is not brms rejecting an unknown class on the
  > observation side. `Psi` is in `mvgam_unsuffixed_params`, so
  > `filter_obs_priors()` keeps it away from the obs setup; it is
  > then dropped only by name from the trend monitor list, which
  > it is not on, so it survives into the trend-side brms call
  > and is refused there. The fix has to remove family-owned
  > classes before the obs and trend split, not after.
  >
  > And only two of the nine dispatch branches change. The
  > `switch()` arms are independent calls, so `mvn` and `mvt` can
  > take a `prior` argument while the other seven keep theirs.
  >
  > `get_trend_parameter_prior()` is trend-specific in name only:
  > it takes a NULL trend object and already does the override,
  > `constant()` refusal and default lookup that every emission
  > site needs. `Psi` and `nu` should resolve through it rather
  > than grow a second mechanism, with defaults added to
  > `common_trend_priors` and a `family_stanvar_prior_classes()`
  > naming what a family owns so the emitter, the prior table and
  > the obs/trend split cannot disagree.
  >
  > For `nu`, keep the shift by moving it out of the statement:
  > sample `nu_excess` with a lower bound of zero and define
  > `nu = 2 + nu_excess` as a transformed parameter. The prior is
  > then an ordinary `~` statement the existing reader already
  > understands, the class name says what it constrains, and
  > `nu` survives unchanged in the posterior so cached fits stay
  > readable. Writing a settable distribution into the
  > `target +=` form instead would need the regex extended, a
  > family-scoped allowance threaded through three more
  > signatures, and would report a prior string that omits the
  > shift.
  >
  > `lift_mvgam_stanvar_priors()` names the sites it knows about
  > in its own roxygen, and `nu` is not among them. Whatever
  > spelling it ends up with, that list has to name it, or the
  > next reader audit concludes the prior does not exist.
  >
  > One hazard to design around rather than guard: `"nu"` must
  > never go into `mvgam_unsuffixed_params`. That predicate has
  > no family context, and `filter_obs_priors()` splits on its
  > complement, so the entry would divert `com_binomial()`'s
  > genuine brms `nu` prior away from brms and drop it silently.
  > `nu_excess` collides with nothing.
  >
  > No fixture stops being readable: none of the 65 cached fits
  > is an `mvn()` or `mvt()` model. The declaration and prior are
  > written once now, since `make_psi_stanvars()` replaced two
  > byte-identical blocks.
  >
  > Check the emitted `mvt` program for a stanc3 non-linear
  > transform warning before writing tests to the new spelling.

- [x] **18.0 Convert the tests to the density spelling**
  > Done. Every prior mvgam emits is now written through
  > `stan_prior_statement()` and normalised in one post-assembly
  > pass, so `lp__` matches the log joint and `bridge_sampler()`
  > can be trusted across trend structures.
  >
  > The test conversion found more than a spelling change. Five
  > negative assertions named a tilde nothing writes any more and
  > so passed whatever the program held; each now names the
  > parameter, read through `mvgam_stancode_prior_rows()` by the
  > `stan_prior_on()` helper. Both replacements were shown to
  > discriminate rather than pass trivially: a default program
  > reports `Z` at `student_t(3, 0, 0.5)`, and a user prior on
  > `b_trend` reads back as `normal(0, 3)`.
  >
  > Two helpers were worse than stale. `expect_match2()` returned
  > a bare logical that testthat never saw, so 50 unwrapped call
  > sites asserted nothing, and it was defined in two setup files.
  > `stan_pattern()` tried to guess whether a pattern was a
  > literal or a regex, and neither branch worked: the detector
  > looked for a backslash the callers never wrote, and the
  > escaping character class closed on its own `]` so it escaped
  > nothing. Being a no-op in both directions is why the suite
  > stayed green. The guessing is gone and `fixed = TRUE`, already
  > the idiom in that file, says when a pattern is a literal.
  >
  > One assertion was a real casualty: the custom-prior test
  > looked for the literal `normal(0, 0.75)`, which normalisation
  > had turned into `target += normal_lpdf(ar1_trend | 0, 0.75)`.
  > It now asserts each prior reached the parameter it names,
  > which the original would not have caught landing on the wrong
  > one.
  >
  > The `bridge_sampler()` gate had tests for brms's `_lupdf`
  > spelling only, never mvgam's own tilde. It has both now.

- [ ] **20.0 The last part of the forecasting audit**
  > Five parts are done, and so is the sixth. The trend linear
  > predictor now reaches every forecast: generated Stan builds
  > the series-scale trend as `Z . lv_trend + mu_trend`, so
  > `propagate_one_draw()` advances a zero-mean state and adds the
  > mean once, at series scale. The drop covered ZMVN and CAR as
  > reported, and PW and every factor fit besides.
  > `tests/local/test-forecast-trend-linpred.R` pins it with one
  > fit per branch, and `tests/testthat/test-forecast-mvgam.R`
  > fails if either the centring or the re-add is removed.
  >
  > What is left is `lfo_sum_rows()`, which carries two NA
  > conventions: `log p = 0` in the multi-column branch, and a
  > dropped draw in the single-column branch, which then recycles
  > against the full weight vector. Not yet reproduced here.

- [ ] **23.0 The empty-observation-formula residuals sit a standard
  deviation high**
  > `tests/local/test-sim-mvgam-recovery.R` passes 20 of 20. It
  > called `mvgam(trend_model = ...)`, an argument 2.0 removed,
  > so `reject_removed_args()` stopped all nine cases before they
  > fitted and the recovery coverage it claims never ran. A
  > `type_trends` map now holds one `trend_formula` per catalog
  > type and `trend_model_from_formula()` evaluates its
  > right-hand side for `sim_mvgam()`, so a single entry drives
  > both the simulation and the fit rather than two spellings
  > that can drift. The map matches the catalog defaults: `RW()`
  > for type 1, `AR(p = 1)` for types 2 to 5, `CAR()` for type 6.
  >
  > `tests/local/test-empty-obs-formula.R` fails 3 of 13, and one
  > reading of it is already ruled out. Two failures are median
  > quantile residuals of 0.92 and 1.23 against a threshold of
  > 0.5, and those are not PIT values read on the wrong scale: a
  > PIT median cannot exceed 1. They are normal-scale quantile
  > residuals, so the fit sits about a standard deviation below
  > the observations on two of three series.
  >
  > Raising `prop_trend` to 0.95 does not move them, so trend
  > dominance is not what sets the level. Nor is the simulator:
  > the counts are the same with `R/sim_mvgam.R` at `HEAD`.
  >
  > 31.1 is now the first suspect, and this entry should not be
  > worked before it lands. These fits are Poisson, which has no
  > analytic quantile spec, so their residuals take the
  > empirical-PIT path through `posterior_predict()` and are
  > PIT-ed against draws that ignore the fitted latent state. A
  > residual carrying the trend rather than the fit is exactly the
  > one-sided miscalibration recorded here. Re-run the file once
  > 31.1 is fixed and see what is left before reading further.
  >
  > The reading below stands as the alternative if anything
  > survives. The test guards `extract_linpred_univariate()`
  > against dropping the intercept column on a
  > `y ~ 0 + <regressor>` formula, where the lone all-1s column is
  > the regressor rather than `b_Intercept`. Zeroing it puts a
  > factor of `exp(b[1])` through residuals, hindcasts and
  > predictions, and a one-sided bias of the size seen has that
  > shape. Establish whether the obs
  > linear predictor reaches the residuals at all before touching
  > the threshold: a
  > threshold moved to fit the observed number tests nothing.
  > The third failure is a placeholder-name assertion.
  >
  > Neither file runs in CI, and the local suite has no runner
  > that sweeps every file, so nothing reports a stale one.

- [x] **24.0 A factor fit at the ceiling forecast as if it had none**
  > Done, and it was a live defect rather than a tidy-up.
  > `forecast.mvgam()` and `extract_last_state()` each decided
  > whether a fit was a factor model by comparing `N_lv_trend`
  > against `N_series_trend`. That comparison cannot answer it: a
  > factor fit reaches `n_lv = n_series` under an MGP loadings
  > prior, a `by = lv_axis()` term or a fully fixed `trend_map`,
  > and a non-factor fit carries the same pair with an identity Z
  > in transformed data. Such a fit was read as series-grain, so
  > no Z projection ran and `ar1_trend[k]`, indexed by latent
  > column, reached the recursion for series k with lengths that
  > matched and nothing to complain.
  >
  > Only the requested `n_lv` separates the two, and the trend
  > spec keeps it. `detect_factor_n_lv()` moved from
  > `R/residual_cor.R` to sit beside `is_factor_model_spec()` and
  > now routes through it, so the forecast reads the answer
  > codegen used when it decided whether to sample `Z`. It already
  > had eleven callers, so the name stayed.
  >
  > Shown on `val_mvgam_mgp_ceiling.rds`, built for this because
  > no cached fit sat at the ceiling: the state now reads
  > `lv_trend[T, ]` at (1.452, 0.386, 0.228) where the series
  > grain is (-1.114, 0.468, 0.166), opposite in sign on the first
  > element. That fixture samples with a few divergences and a low
  > E-BFMI on one chain, which is the geometry the ceiling has and
  > why it exists only under shrinkage; its tests assert grain and
  > never parameter accuracy.
  >
  > `posterior_transition_matrix()` labelled its latent axes from
  > `standata$series_names`, which nothing in `R/` ever writes, so
  > the `%||%` always fell through and the reported labels were
  > already `process_<k>`. The dead read is gone, which is what
  > kept a future change from silently reintroducing series names
  > on a factor VAR's latent axes.

- [ ] **25.0 One fixture stores a Cholesky factor as a covariance**
  > `tests/local/fixtures/val_jsdgam_trait.rds` was built when the
  > generator emitted
  > `Sigma_trend = diag_pre_multiply(sigma_trend, L_Omega_trend)`,
  > so its stored `Sigma_trend` is the Cholesky factor rather than
  > the covariance: not symmetric, not positive semi-definite.
  > Current codegen emits
  > `multiply_lower_tri_self_transpose(...)`, so only the fixture
  > is stale. Rebuilding it moves its agreement with a simulated
  > truth from 0.80 to 0.999982.
  >
  > The rule the fixture teaches is worth keeping either way: read
  > `sigma_trend` and `L_Omega_trend` and rebuild, rather than
  > reading `Sigma_trend[i, j]` off the posterior of a
  > cholesky-scaled trend. `extract_cov_draws_flat()` already does
  > that.

- [ ] **27.0 A trend formula may name the response**
  > `test-mvgam-formula.R` carried a test named
  > "validate_trend_covariates prevents response variables in
  > trend formulas" that built a data frame and asserted nothing.
  > It was the suite's only SKIP. No `validate_trend_covariates()`
  > exists in `R/`, so the stub named a function from an earlier
  > architecture and the check it promised was never written. The
  > stub is removed; this entry is the record.
  >
  > The gap is real. `mvgam_formula(count ~ 1, trend_formula =
  > ~ count + AR())` is accepted, so the latent state can be
  > regressed on its own observed response. Forecasting cannot
  > honour that, since the future response is what is being
  > predicted.
  >
  > Refusing it outright needs a decision rather than a patch. A
  > multivariate fit may legitimately want one response as a
  > covariate on another's trend, and `mvgam_formula()` sees only
  > the formulas, so the rule has to be written in terms of the
  > response it was given. Decide whether the refusal covers the
  > fit's own response only, or every response in an `mvbind()`.

- [ ] **29.0 Does `sign_canonicalise_factors()` have a job left**
  > The dead correlation-dimension branches are gone: both
  > compared the string `"N_lv_trend"` against a number, so
  > neither ever ran and the emitted Stan is unchanged.
  >
  > What remains is a decision. `sign_canonicalise_factors()`
  > returns early for a fixed or partial `fixed_Z`, for
  > `by = lv_axis()`, and for any fit carrying `Z_tilde`. Free-Z
  > fits are rotated unless they are by-lv, so they always carry
  > `Z_tilde`. Between them those three cover every factor fit
  > mvgam can currently build, and the sign-flipping loop is
  > unreachable. The file header says it stays active for
  > partial-Z fits, which is the one case the second early return
  > already covers.
- [ ] **30.0 A bounded AR coefficient carries an untruncated prior**
  > `ar{lag}_trend` is declared `vector<lower=-1, upper=1>` and
  > given `normal(mu, sigma)`, with no correction for the density
  > mass outside those bounds. brms subtracts an `_lccdf` term
  > whenever a parameter's declaration is tighter than its prior's
  > support; mvgam does not.
  >
  > Where the location and scale are literals, as in the plain
  > `ar1_trend ~ normal(0, 0.5)`, the missing term is a constant.
  > It shifts `lp__` and nothing else, and normalising the
  > statement does not supply it, so a Bayes factor between models
  > with different numbers of bounded parameters keeps a residual
  > offset of about 0.047 per element.
  >
  > The hierarchical spelling is a different matter.
  > `build_plain_ar_stanvars()` writes
  > `ar{lag}_trend ~ normal(mu_ar{lag}_trend, sigma_ar{lag}_trend)`
  > with both hyperparameters sampled, so the truncation term
  > `log(Phi((1 - mu) / sigma) - Phi((-1 - mu) / sigma))` varies
  > with the parameters. It is not a constant, Stan cannot drop it
  > as one, and the posterior it defines is not the truncated
  > normal the declaration implies. Draws move if this is fixed,
  > so a hierarchical AR fixture would need refitting.
  >
  > Decide the parameterisation before writing anything: adding
  > the `_lccdf` correction keeps the coefficient scale and the
  > class name, while the Heaps partial-autocorrelation mapping
  > the VAR generator already carries would remove the bound
  > instead. 16.0 records the reasons the coefficient scale was
  > kept, and those still apply.

- [ ] **31.0 Six defects found writing the case study**
  > Reported against 30ce433 and verified here against the
  > source. Ordered by what a CRAN user meets first.
  >
  > **31.1 `posterior_predict()` never honoured `incl_autocor`.**
  > It derives `trend_state` and hands it to `posterior_linpred()`,
  > whose formal is `incl_autocor`, so the argument falls into
  > `...` and the callee takes its own `FALSE`. Every draw has
  > always been marginal. It is the only bad site of four:
  > `log_lik()`, `posterior_epred()` and `posterior_linpred()`
  > pass `trend_state` to `get_combined_linpred()`, where it is a
  > real formal. What singles this one out is that it delegates to
  > a sibling public method rather than to the internal one, and
  > that is exactly where the name changes from the internal
  > spelling to the external.
  >
  > The cost is silent. `diagnostic_surface_args()` sets
  > `incl_autocor = TRUE` for in-sample diagnostics, so the helper
  > the architecture names as the one place diagnostics choose a
  > surface is a no-op on this path: `residuals()` reaches it at
  > three sites and `pp_check()` at one. `loo_predict()` asks for
  > the conditional surface explicitly and is refused, so it pairs
  > conditional importance weights with marginal draws, against
  > the invariant its own comment states.
  >
  > Only the empirical-PIT path is affected, which is every family
  > without an analytic spec: `quantile_family_specs` holds
  > gaussian, student, lognormal and beta, so continuous fits
  > already read the conditional surface through
  > `posterior_linpred()` and do not move. `loo()`, `waic()`,
  > `lfo_cv()`, `kfold()`, `hindcast()` and `forecast()` reach
  > `get_combined_linpred()` directly and are untouched. No fixture
  > needs refitting; residual and `pp_check` values on a
  > strong-trend fit will move, correctly.
  >
  > `test-posterior-linpred.R` asserts the argument exists and
  > defaults correctly on all five entry points, and never that
  > `posterior_predict()` acts on it. A behavioural test belongs
  > with the fix: the two settings must differ on a strong-AR fit,
  > and the conditional one must track `hindcast()`.
  >
  > **31.2 A prediction grid that loses or contradicts an addition
  > term.** One fault in three places, which is why they are
  > written together.
  >
  > `hindcast()`, `plot(type = "trend")` and
  > `plot(type = "residuals")` fail on any zero-padded binomial or
  > beta-binomial fit with "Number of trials is smaller than the
  > number of events". `get_safe_dummy_value()` fills a padded
  > response with 1 for a count family, and `mock-stanfit.R` calls
  > `brms::standata()` without `check_response = FALSE`, so brms
  > compares that 1 against a padded `trials` of 0. Padding with 1
  > instead is the wrong repair: a padded row would then draw a
  > real Bernoulli value and put synthetic noise into the very
  > plots the padding exists to enable. The smooth path already
  > passes `check_response = FALSE` and says why.
  >
  > `conditional_effects()` cannot find `trials`, because
  > `find_predictors.mvgam()` reads the right-hand side only and
  > `find_response.mvgam()` returns the first LHS name, so
  > `datagrid()` drops the column. The repair belongs at the
  > `get_predict.mvgam()` boundary, where
  > `complete_closure_unit_newdata()` already backfills grid
  > columns for the same reason, and not in `find_predictors()`:
  > anything named there becomes a variable `avg_slopes()` will
  > differentiate, and a slope with respect to a binomial
  > denominator means nothing.
  >
  > `backfill_smooth_grid()` holds a non-focal `trials` at its
  > median, which is fractional. Rounding an integer-valued column
  > is a better grid value regardless and reaches no design matrix.
  >
  > **31.3 `plot(type = "smooths")` on any `trials()` fit.** Two
  > further faults, a strict chain with the one above, so all
  > three land together or none does.
  > `mvgam_smooth_label_spec()` attaches the family only when the
  > formula is not already a `brmsformula`, but
  > `mvgam_side_formula()` always returns one, so `brmsterms()`
  > validates as gaussian and rejects `trials()`. The
  > `mvbrmsformula` branch below it already handles this; the test
  > should be on whether the formula carries a family, not on its
  > class. And `build_smooth_grid()` is the one bare call site of
  > three, with the family in scope. It has to be side-aware:
  > `mvgam_smooth_terms()` uses the fit's family for the
  > observation side and gaussian for the trend.
  >
  > **31.4 `posterior_epred()` errors on `com_binomial`.**
  > `family_mean_from_kernel()` calls `kernel(prep)`, and
  > `posterior_epred_com_binomial()` is the last kernel still on
  > the `(linpred, link, family_pars, trials)` signature, so `prep`
  > binds to `linpred` and the matrix assertion fires. Convert the
  > body, not the signature: it applies `.linkinv()` to a value
  > already inverse-linked, so an argument-only wrapper would
  > invert twice and return wrong means with no error. Read
  > `prep$dpars$mu`, `prep$dpars$nu` and the trials draws as the
  > binomial kernel does. The `"com_binomial"` arm of the switch in
  > `posterior_epred.R` is unreachable and must go in the same
  > change, or the converted signature leaves an install note.
  > `posterior_predict_com_binomial()` keeps the old signature and
  > is called correctly; leave it.
  >
  > **31.5 `s(series, bs = "re")` draws an empty panel.** One point
  > per level rendered with `geom_ribbon` and `geom_line`, both of
  > which need two points per group. brms branches on
  > `is.numeric()` and draws points with intervals. Cosmetic.
  >
  > Sequence: 31.1 first and alone, since it changes numbers and
  > nothing else here blocks it. Then 31.2, whose failures block
  > the binomial reproducers that would demonstrate 31.1. 31.3 and
  > 31.4 after. Re-run 23.0 once 31.1 lands, before touching
  > anything in it.

- [ ] **6.0 Final release verification**
  > Clean `document()`, clean test sweep, `R CMD check --as-cran`,
  > tarball under the size limit, sweep green. Gated on 31.0:
  > three of those defects are hard errors on ordinary
  > `binomial()` fits and one silently misreports every residual
  > on a discrete family.

## Behaviour confirmed, worth documenting

`posterior_epred()` and `posterior_predict()` default to
`process_error = FALSE` and `incl_autocor = FALSE`, so the trend
contributes its deterministic submodel and the answer is the same on
every call. Passing the training data back as `newdata` is an exact
no-op. Under `process_error = TRUE` innovations are drawn afresh, so
two calls on one fit and the same `draw_ids` differ, by as much as
160 units on a cached Poisson fit; reproducible output there needs a
seed.
