#' Generate a methods description for \pkg{mvgam} models
#'
#' Construct a methods skeleton plus a deduplicated reference
#' list and BibTeX block for a fitted `mvgam` object. The
#' assembled text and citation set are driven by the model's
#' actual structure: the family / link, trend type, factor
#' identification, structured loadings prior, GP smooths,
#' piecewise trends, Stan backend, and sampling algorithm.
#'
#' @name how_to_cite.mvgam
#'
#' @param object A fitted `mvgam` or `jsdgam` object.
#' @param ... Currently ignored.
#'
#' @return An object of class `how_to_cite` carrying a
#'   `methods_text` skeleton, the matched `citations`, a
#'   curated `other_citations` block, and a `bibtex` list
#'   keyed by reference handle. Use `bibtex(x)` to extract the
#'   BibTeX entries; the default `print()` method renders all
#'   sections together.
#'
#' @author Nicholas J Clark
#'
#' @seealso \code{\link[utils]{citation}}, \code{\link{mvgam}},
#'   \code{\link{methods_md}}. `how_to_cite()` produces the
#'   prose methods paragraph; `methods_md()` produces the
#'   matching math statement of the model.
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # Methods skeleton, citations, and BibTeX, all driven by the
#' # fitted family / trend / GP / Stan backend.
#' how_to_cite(mod)
#' }
#'
#' @export
how_to_cite <- function(object, ...) {
  UseMethod("how_to_cite", object)
}


# Reference database: each entry carries the rendered text
# citation (matching the in-text "(Author Year)" pointers) and
# a BibTeX entry. New citations should be added here and
# referenced by key from the detection table below.
#'@noRd
reference_db <- function() {
  list(
    clark_dgam = list(
      text = "Clark NJ and Wells K (2023). Dynamic Generalized Additive Models (DGAMs) for forecasting discrete ecological time series. Methods in Ecology and Evolution, 14, 771-784. https://doi.org/10.1111/2041-210X.13974",
      bibtex = paste(
        "@article{clark2023dgam,",
        "  title = {Dynamic Generalized Additive Models {(DGAMs)} for forecasting discrete ecological time series},",
        "  author = {Clark, Nicholas J. and Wells, Konstans},",
        "  journal = {Methods in Ecology and Evolution},",
        "  volume = {14},",
        "  pages = {771--784},",
        "  year = {2023},",
        "  doi = {10.1111/2041-210X.13974}",
        "}",
        sep = "\n"
      )
    ),
    burkner_brms = list(
      text = "Burkner PC (2017). brms: An R Package for Bayesian Multilevel Models Using Stan. Journal of Statistical Software, 80(1), 1-28. https://doi.org/10.18637/jss.v080.i01",
      bibtex = paste(
        "@article{burkner2017brms,",
        "  title = {{brms}: An {R} Package for {B}ayesian Multilevel Models Using {S}tan},",
        "  author = {B{\\\"u}rkner, Paul-Christian},",
        "  journal = {Journal of Statistical Software},",
        "  volume = {80},",
        "  number = {1},",
        "  pages = {1--28},",
        "  year = {2017},",
        "  doi = {10.18637/jss.v080.i01}",
        "}",
        sep = "\n"
      )
    ),
    wood_gam = list(
      text = "Wood SN (2017). Generalized Additive Models: An Introduction with R (2nd edition). Chapman and Hall/CRC.",
      bibtex = paste(
        "@book{wood2017gam,",
        "  title = {Generalized Additive Models: An Introduction with {R}},",
        "  author = {Wood, Simon N.},",
        "  publisher = {Chapman and Hall/CRC},",
        "  edition = {2},",
        "  year = {2017}",
        "}",
        sep = "\n"
      )
    ),
    stan = list(
      text = "Carpenter B, Gelman A, Hoffman MD, Lee D, Goodrich B, Betancourt M, Brubaker M, Guo J, Li P and Riddell A (2017). Stan: A probabilistic programming language. Journal of Statistical Software 76.",
      bibtex = paste(
        "@article{carpenter2017stan,",
        "  title = {{Stan}: A Probabilistic Programming Language},",
        "  author = {Carpenter, Bob and Gelman, Andrew and Hoffman, Matthew D. and Lee, Daniel and Goodrich, Ben and Betancourt, Michael and Brubaker, Marcus and Guo, Jiqiang and Li, Peter and Riddell, Allen},",
        "  journal = {Journal of Statistical Software},",
        "  volume = {76},",
        "  year = {2017},",
        "  doi = {10.18637/jss.v076.i01}",
        "}",
        sep = "\n"
      )
    ),
    cmdstanr = list(
      text = "Gabry J, Cesnovar R, Johnson A and Bronder S (2024). cmdstanr: R Interface to 'CmdStan'. https://mc-stan.org/cmdstanr/",
      bibtex = paste(
        "@misc{gabry2024cmdstanr,",
        "  title = {{cmdstanr}: {R} Interface to {'CmdStan'}},",
        "  author = {Gabry, Jonah and {\\v C}e{\\v s}novar, Rok and Johnson, Andrew and Bronder, Steve},",
        "  year = {2024},",
        "  url = {https://mc-stan.org/cmdstanr/}",
        "}",
        sep = "\n"
      )
    ),
    rstan = list(
      text = "Stan Development Team (2024). RStan: the R interface to Stan. https://mc-stan.org/",
      bibtex = paste(
        "@misc{standevteam2024rstan,",
        "  title = {{RStan}: the {R} interface to {Stan}},",
        "  author = {{Stan Development Team}},",
        "  year = {2024},",
        "  url = {https://mc-stan.org/}",
        "}",
        sep = "\n"
      )
    ),
    vehtari_rhat = list(
      text = "Vehtari A, Gelman A, Simpson D, Carpenter B and Burkner P (2021). Rank-normalization, folding, and localization: An improved Rhat for assessing convergence of MCMC. Bayesian Analysis 16(2), 667-718. https://doi.org/10.1214/20-BA1221",
      bibtex = paste(
        "@article{vehtari2021rhat,",
        "  title = {Rank-Normalization, Folding, and Localization: {A}n Improved {R-hat} for Assessing Convergence of {MCMC}},",
        "  author = {Vehtari, Aki and Gelman, Andrew and Simpson, Daniel and Carpenter, Bob and B{\\\"u}rkner, Paul-Christian},",
        "  journal = {Bayesian Analysis},",
        "  volume = {16},",
        "  number = {2},",
        "  pages = {667--718},",
        "  year = {2021},",
        "  doi = {10.1214/20-BA1221}",
        "}",
        sep = "\n"
      )
    ),
    kucukelbir_advi = list(
      text = "Kucukelbir A, Tran D, Ranganath R, Gelman A and Blei DM (2017). Automatic Differentiation Variational Inference. Journal of Machine Learning Research 18, 1-45.",
      bibtex = paste(
        "@article{kucukelbir2017advi,",
        "  title = {Automatic Differentiation Variational Inference},",
        "  author = {Kucukelbir, Alp and Tran, Dustin and Ranganath, Rajesh and Gelman, Andrew and Blei, David M.},",
        "  journal = {Journal of Machine Learning Research},",
        "  volume = {18},",
        "  pages = {1--45},",
        "  year = {2017}",
        "}",
        sep = "\n"
      )
    ),
    zhang_pathfinder = list(
      text = "Zhang L, Carpenter B, Gelman A and Vehtari A (2022). Pathfinder: parallel Quasi-Newton variational inference. Journal of Machine Learning Research 23(306), 1-49.",
      bibtex = paste(
        "@article{zhang2022pathfinder,",
        "  title = {Pathfinder: parallel Quasi-{N}ewton variational inference},",
        "  author = {Zhang, Lu and Carpenter, Bob and Gelman, Andrew and Vehtari, Aki},",
        "  journal = {Journal of Machine Learning Research},",
        "  volume = {23},",
        "  number = {306},",
        "  pages = {1--49},",
        "  year = {2022}",
        "}",
        sep = "\n"
      )
    ),
    heaps_var = list(
      text = "Heaps SE (2023). Enforcing stationarity through the prior in vector autoregressions. Journal of Computational and Graphical Statistics 32, 74-83.",
      bibtex = paste(
        "@article{heaps2023var,",
        "  title = {Enforcing stationarity through the prior in vector autoregressions},",
        "  author = {Heaps, Sarah E.},",
        "  journal = {Journal of Computational and Graphical Statistics},",
        "  volume = {32},",
        "  pages = {74--83},",
        "  year = {2023}",
        "}",
        sep = "\n"
      )
    ),
    clark_multispecies = list( # prose-lint-disable-line
      text = "Clark NJ, Ernest SKM, Senyondo H, Simonis J, White EP, Yenni GM and Karunarathna KANK (2025). Beyond single-species models: leveraging multispecies forecasts to navigate the dynamics of ecological predictability. PeerJ 13, e18929.", # prose-lint-disable-line
      bibtex = paste(
        "@article{clark2025multispecies,",
        "  title = {Beyond single-species models: leveraging multispecies forecasts to navigate the dynamics of ecological predictability},", # prose-lint-disable-line
        "  author = {Clark, Nicholas J. and Ernest, S. K. Morgan and Senyondo, Henry and Simonis, Juniper and White, Ethan P. and Yenni, Glenda M. and Karunarathna, K. A. N. K.},",
        "  journal = {PeerJ},",
        "  volume = {13},",
        "  pages = {e18929},",
        "  year = {2025}",
        "}",
        sep = "\n"
      )
    ),
    riutort_gp = list(
      text = "Riutort-Mayol G, Burkner PC, Andersen MR, Solin A and Vehtari A (2023). Practical Hilbert space approximate Bayesian Gaussian processes for probabilistic programming. Statistics and Computing 33, 1. https://doi.org/10.1007/s11222-022-10167-2",
      bibtex = paste(
        "@article{riutort2023gp,",
        "  title = {Practical {H}ilbert space approximate {B}ayesian {G}aussian processes for probabilistic programming},",
        "  author = {Riutort-Mayol, Gabriel and B{\\\"u}rkner, Paul-Christian and Andersen, Michael R. and Solin, Arno and Vehtari, Aki},",
        "  journal = {Statistics and Computing},",
        "  volume = {33},",
        "  pages = {1},",
        "  year = {2023},",
        "  doi = {10.1007/s11222-022-10167-2}",
        "}",
        sep = "\n"
      )
    ),
    taylor_pw = list(
      text = "Taylor S and Letham B (2018). Forecasting at scale. The American Statistician 72(1), 37-45. https://doi.org/10.1080/00031305.2017.1380080",
      bibtex = paste(
        "@article{taylor2018prophet,",
        "  title = {Forecasting at scale},",
        "  author = {Taylor, Sean J. and Letham, Benjamin},",
        "  journal = {The American Statistician},",
        "  volume = {72},",
        "  number = {1},",
        "  pages = {37--45},",
        "  year = {2018},",
        "  doi = {10.1080/00031305.2017.1380080}",
        "}",
        sep = "\n"
      )
    ),
    heaps_jermyn = list(
      text = "Heaps SE and Jermyn IH (2024). Structured prior distributions for the covariance matrix in latent factor models. Statistics and Computing 34, 143. https://doi.org/10.1007/s11222-024-10454-0",
      bibtex = paste(
        "@article{heaps2024structured,",
        "  title = {Structured prior distributions for the covariance matrix in latent factor models},",
        "  author = {Heaps, Sarah E. and Jermyn, Ian H.},",
        "  journal = {Statistics and Computing},",
        "  volume = {34},",
        "  pages = {143},",
        "  year = {2024},",
        "  doi = {10.1007/s11222-024-10454-0}",
        "}",
        sep = "\n"
      )
    ),
    warton_jsdm = list(
      text = "Warton DI, Blanchet FG, O'Hara RB, Ovaskainen O, Taskinen S, Walker SC and Hui FKC (2015). So many variables: joint modeling in community ecology. Trends in Ecology and Evolution 30(12), 766-779. https://doi.org/10.1016/j.tree.2015.09.007",
      bibtex = paste(
        "@article{warton2015jsdm,",
        "  title = {So many variables: joint modeling in community ecology},",
        "  author = {Warton, David I. and Blanchet, F. Guillaume and O'Hara, Robert B. and Ovaskainen, Otso and Taskinen, Sara and Walker, Steven C. and Hui, Francis K. C.},",
        "  journal = {Trends in Ecology and Evolution},",
        "  volume = {30},",
        "  number = {12},",
        "  pages = {766--779},",
        "  year = {2015},",
        "  doi = {10.1016/j.tree.2015.09.007}",
        "}",
        sep = "\n"
      )
    ),
    hui_boral = list(
      text = "Hui FKC (2016). boral - Bayesian Ordination and Regression Analysis of Multivariate Abundance Data in R. Methods in Ecology and Evolution 7(6), 744-750. https://doi.org/10.1111/2041-210X.12514",
      bibtex = paste(
        "@article{hui2016boral,",
        "  title = {{boral} - {B}ayesian Ordination and Regression Analysis of Multivariate Abundance Data in {R}},",
        "  author = {Hui, Francis K. C.},",
        "  journal = {Methods in Ecology and Evolution},",
        "  volume = {7},",
        "  number = {6},",
        "  pages = {744--750},",
        "  year = {2016},",
        "  doi = {10.1111/2041-210X.12514}",
        "}",
        sep = "\n"
      )
    ),
    ovaskainen_hmsc_2017 = list(
      text = "Ovaskainen O, Tikhonov G, Norberg A, Blanchet FG, Duan L, Dunson D, Roslin T and Abrego N (2017). How to make more out of community data? A conceptual framework and its implementation as models and software. Ecology Letters 20(5), 561-576. https://doi.org/10.1111/ele.12757",
      bibtex = paste(
        "@article{ovaskainen2017hmsc,",
        "  title = {How to make more out of community data? {A} conceptual framework and its implementation as models and software},",
        "  author = {Ovaskainen, Otso and Tikhonov, Gleb and Norberg, Anna and Blanchet, F. Guillaume and Duan, Leo and Dunson, David and Roslin, Tomas and Abrego, Nerea},",
        "  journal = {Ecology Letters},",
        "  volume = {20},",
        "  number = {5},",
        "  pages = {561--576},",
        "  year = {2017},",
        "  doi = {10.1111/ele.12757}",
        "}",
        sep = "\n"
      )
    ),
    tikhonov_hmsc_2020 = list(
      text = "Tikhonov G, Opedal OH, Abrego N, Lehikoinen A, de Jonge MMJ, Oksanen J and Ovaskainen O (2020). Joint species distribution modelling with the R-package Hmsc. Methods in Ecology and Evolution 11(3), 442-447. https://doi.org/10.1111/2041-210X.13345",
      bibtex = paste(
        "@article{tikhonov2020hmsc,",
        "  title = {Joint species distribution modelling with the {R}-package {H}msc},",
        "  author = {Tikhonov, Gleb and Opedal, {\\O}ystein H. and Abrego, Nerea and Lehikoinen, Aleksi and de Jonge, Melinda M. J. and Oksanen, Jari and Ovaskainen, Otso},",
        "  journal = {Methods in Ecology and Evolution},",
        "  volume = {11},",
        "  number = {3},",
        "  pages = {442--447},",
        "  year = {2020},",
        "  doi = {10.1111/2041-210X.13345}",
        "}",
        sep = "\n"
      )
    ),
    bhattacharya_mgp = list(
      text = "Bhattacharya A and Dunson DB (2011). Sparse Bayesian infinite factor models. Biometrika 98, 291-306. https://doi.org/10.1093/biomet/asr013",
      bibtex = paste(
        "@article{bhattacharya2011mgp,",
        "  title = {Sparse {B}ayesian infinite factor models},",
        "  author = {Bhattacharya, Anirban and Dunson, David B.},",
        "  journal = {Biometrika},",
        "  volume = {98},",
        "  pages = {291--306},",
        "  year = {2011},",
        "  doi = {10.1093/biomet/asr013}",
        "}",
        sep = "\n"
      )
    ),
    arel_bundock_marginaleffects = list(
      text = "Arel-Bundock V, Greifer N and Heiss A (2024). How to interpret statistical models using marginaleffects for R and Python. Journal of Statistical Software, 111(9), 1-32. https://doi.org/10.18637/jss.v111.i09",
      bibtex = paste(
        "@article{arelbundock2024marginal,",
        "  title = {How to interpret statistical models using {marginaleffects} for {R} and {Python}},",
        "  author = {Arel-Bundock, Vincent and Greifer, Noah and Heiss, Andrew},",
        "  journal = {Journal of Statistical Software},",
        "  volume = {111},",
        "  number = {9},",
        "  pages = {1--32},",
        "  year = {2024},",
        "  doi = {10.18637/jss.v111.i09}",
        "}",
        sep = "\n"
      )
    ),
    gabry_workflow = list(
      text = "Gabry J, Simpson D, Vehtari A, Betancourt M and Gelman A (2019). Visualization in Bayesian workflow. Journal of the Royal Statistical Society A, 182, 389-402. https://doi.org/10.1111/rssa.12378",
      bibtex = paste(
        "@article{gabry2019workflow,",
        "  title = {Visualization in {B}ayesian workflow},",
        "  author = {Gabry, Jonah and Simpson, Daniel and Vehtari, Aki and Betancourt, Michael and Gelman, Andrew},",
        "  journal = {Journal of the Royal Statistical Society A},",
        "  volume = {182},",
        "  pages = {389--402},",
        "  year = {2019},",
        "  doi = {10.1111/rssa.12378}",
        "}",
        sep = "\n"
      )
    ),
    vehtari_loo = list(
      text = "Vehtari A, Gelman A and Gabry J (2017). Practical Bayesian model evaluation using leave-one-out cross-validation and WAIC. Statistics and Computing, 27, 1413-1432. https://doi.org/10.1007/s11222-016-9696-4",
      bibtex = paste(
        "@article{vehtari2017loo,",
        "  title = {Practical {B}ayesian model evaluation using leave-one-out cross-validation and {WAIC}},",
        "  author = {Vehtari, Aki and Gelman, Andrew and Gabry, Jonah},",
        "  journal = {Statistics and Computing},",
        "  volume = {27},",
        "  pages = {1413--1432},",
        "  year = {2017},",
        "  doi = {10.1007/s11222-016-9696-4}",
        "}",
        sep = "\n"
      )
    ),
    burkner_lfo = list(
      text = "Burkner PC, Gabry J and Vehtari A (2020). Approximate leave-future-out cross-validation for Bayesian time series models. Journal of Statistical Computation and Simulation, 90(14), 2499-2523. https://doi.org/10.1080/00949655.2020.1783262",
      bibtex = paste(
        "@article{burkner2020lfo,",
        "  title = {Approximate leave-future-out cross-validation for {B}ayesian time series models},",
        "  author = {B{\\\"u}rkner, Paul-Christian and Gabry, Jonah and Vehtari, Aki},",
        "  journal = {Journal of Statistical Computation and Simulation},",
        "  volume = {90},",
        "  number = {14},",
        "  pages = {2499--2523},",
        "  year = {2020},",
        "  doi = {10.1080/00949655.2020.1783262}",
        "}",
        sep = "\n"
      )
    ),
    durbin_koopman_ssm = list(
      text = "Durbin J and Koopman SJ (2012). Time Series Analysis by State Space Methods (2nd edition). Oxford University Press.",
      bibtex = paste(
        "@book{durbin2012ssm,",
        "  title = {Time Series Analysis by State Space Methods},",
        "  author = {Durbin, James and Koopman, Siem Jan},",
        "  publisher = {Oxford University Press},",
        "  edition = {2},",
        "  year = {2012}",
        "}",
        sep = "\n"
      )
    ),
    irwin_waring = list(
      text = "Irwin JO (1968). The generalized Waring distribution applied to accident theory. Journal of the Royal Statistical Society: Series A (General), 131(2), 205-225. https://doi.org/10.2307/2343842",
      bibtex = paste(
        "@article{irwin1968waring,",
        "  title = {The generalized {W}aring distribution applied to accident theory},",
        "  author = {Irwin, J. O.},",
        "  journal = {Journal of the Royal Statistical Society: Series A (General)},",
        "  volume = {131},",
        "  number = {2},",
        "  pages = {205--225},",
        "  year = {1968},",
        "  doi = {10.2307/2343842}",
        "}",
        sep = "\n"
      )
    ),
    jorgensen_tweedie = list(
      text = "Jorgensen B (1987). Exponential dispersion models. Journal of the Royal Statistical Society: Series B (Methodological), 49(2), 127-162. https://doi.org/10.1111/j.2517-6161.1987.tb01685.x",
      bibtex = paste(
        "@article{jorgensen1987edm,",
        "  title = {Exponential dispersion models},",
        "  author = {J{\\o}rgensen, Bent},",
        "  journal = {Journal of the Royal Statistical Society: Series B (Methodological)},",
        "  volume = {49},",
        "  number = {2},",
        "  pages = {127--162},",
        "  year = {1987},",
        "  doi = {10.1111/j.2517-6161.1987.tb01685.x}",
        "}",
        sep = "\n"
      )
    ),
    dunn_smyth_tweedie = list(
      text = "Dunn PK and Smyth GK (2005). Series evaluation of Tweedie exponential dispersion model densities. Statistics and Computing, 15(4), 267-280. https://doi.org/10.1007/s11222-005-4070-y",
      bibtex = paste(
        "@article{dunn2005tweedie,",
        "  title = {Series evaluation of {T}weedie exponential dispersion model densities},",
        "  author = {Dunn, Peter K. and Smyth, Gordon K.},",
        "  journal = {Statistics and Computing},",
        "  volume = {15},",
        "  number = {4},",
        "  pages = {267--280},",
        "  year = {2005},",
        "  doi = {10.1007/s11222-005-4070-y}",
        "}",
        sep = "\n"
      )
    ),
    shmueli_cmb = list(
      text = "Shmueli G, Minka TP, Kadane JB, Borle S and Boatwright P (2005). A useful distribution for fitting discrete data: revival of the Conway-Maxwell-Poisson distribution. Journal of the Royal Statistical Society: Series C (Applied Statistics), 54(1), 127-142. https://doi.org/10.1111/j.1467-9876.2005.00474.x",
      bibtex = paste(
        "@article{shmueli2005cmp,",
        "  title = {A useful distribution for fitting discrete data: revival of the {C}onway-{M}axwell-{P}oisson distribution},",
        "  author = {Shmueli, Galit and Minka, Thomas P. and Kadane, Joseph B. and Borle, Sharad and Boatwright, Peter},",
        "  journal = {Journal of the Royal Statistical Society: Series C (Applied Statistics)},",
        "  volume = {54},",
        "  number = {1},",
        "  pages = {127--142},",
        "  year = {2005},",
        "  doi = {10.1111/j.1467-9876.2005.00474.x}",
        "}",
        sep = "\n"
      )
    ),
    conway_maxwell_1962 = list(
      text = "Conway RW and Maxwell WL (1962). A queuing model with state-dependent service rates. Journal of Industrial Engineering, 12(2), 132-136.",
      bibtex = paste(
        "@article{conway1962queue,",
        "  title = {A queuing model with state-dependent service rates},",
        "  author = {Conway, Richard W. and Maxwell, William L.},",
        "  journal = {Journal of Industrial Engineering},",
        "  volume = {12},",
        "  number = {2},",
        "  pages = {132--136},",
        "  year = {1962}",
        "}",
        sep = "\n"
      )
    ),
    bogomolovas_cmb_tmb = list(
      text = "Bogomolovas J (2024). Conway-Maxwell-Binomial in TMB: a fast adjoint for super- and under-dispersed bounded counts. Source code at https://github.com/jbogomolovas2/glmmTMB (com_binomial branch).",
      bibtex = paste(
        "@misc{bogomolovas2024cmb,",
        "  title = {{C}onway-{M}axwell-{B}inomial in {TMB}: a fast adjoint for super- and under-dispersed bounded counts},",
        "  author = {Bogomolovas, Jurgen},",
        "  year = {2024},",
        "  howpublished = {Source code at \\url{https://github.com/jbogomolovas2/glmmTMB}}",
        "}",
        sep = "\n"
      )
    ),
    neyman_type_a_1939 = list(
      text = "Neyman J (1939). On a new class of contagious distributions, applicable in entomology and bacteriology. Annals of Mathematical Statistics, 10(1), 35-57. https://doi.org/10.1214/aoms/1177732245",
      bibtex = paste(
        "@article{neyman1939contagious,",
        "  title = {On a new class of contagious distributions, applicable in entomology and bacteriology},",
        "  author = {Neyman, Jerzy},",
        "  journal = {Annals of Mathematical Statistics},",
        "  volume = {10},",
        "  number = {1},",
        "  pages = {35--57},",
        "  year = {1939},",
        "  doi = {10.1214/aoms/1177732245}",
        "}",
        sep = "\n"
      )
    ),
    royle_nichols_2003 = list(
      text = "Royle JA and Nichols JD (2003). Estimating abundance from repeated presence-absence data or point counts. Ecology, 84(3), 777-790. https://doi.org/10.1890/0012-9658(2003)084[0777:EAFRPA]2.0.CO;2",
      bibtex = paste(
        "@article{royle2003abundance,",
        "  title = {Estimating abundance from repeated presence-absence data or point counts},",
        "  author = {Royle, J. Andrew and Nichols, James D.},",
        "  journal = {Ecology},",
        "  volume = {84},",
        "  number = {3},",
        "  pages = {777--790},",
        "  year = {2003},",
        "  doi = {10.1890/0012-9658(2003)084[0777:EAFRPA]2.0.CO;2}",
        "}",
        sep = "\n"
      )
    ),
    royle_nmix_2004 = list(
      text = "Royle JA (2004). N-mixture models for estimating population size from spatially replicated counts. Biometrics, 60(1), 108-115. https://doi.org/10.1111/j.0006-341X.2004.00142.x",
      bibtex = paste(
        "@article{royle2004nmixture,",
        "  title = {{N}-mixture models for estimating population size from spatially replicated counts},",
        "  author = {Royle, J. Andrew},",
        "  journal = {Biometrics},",
        "  volume = {60},",
        "  number = {1},",
        "  pages = {108--115},",
        "  year = {2004},",
        "  doi = {10.1111/j.0006-341X.2004.00142.x}",
        "}",
        sep = "\n"
      )
    ),
    dennis_nmix_2015 = list(
      text = "Dennis EB, Morgan BJT and Ridout MS (2015). Computational aspects of N-mixture models. Biometrics, 71(1), 237-246. https://doi.org/10.1111/biom.12246",
      bibtex = paste(
        "@article{dennis2015nmixture,",
        "  title = {Computational aspects of {N}-mixture models},",
        "  author = {Dennis, Emily B. and Morgan, Byron J. T. and Ridout, Martin S.},",
        "  journal = {Biometrics},",
        "  volume = {71},",
        "  number = {1},",
        "  pages = {237--246},",
        "  year = {2015},",
        "  doi = {10.1111/biom.12246}",
        "}",
        sep = "\n"
      )
    ),
    kery_nmix_2018 = list(
      text = "Kery M (2018). Identifiability in N-mixture models: a large-scale screening test with bird data. Ecology, 99(2), 281-288. https://doi.org/10.1002/ecy.2093",
      bibtex = paste(
        "@article{kery2018identifiability,",
        "  title = {Identifiability in {N}-mixture models: a large-scale screening test with bird data},",
        "  author = {K{\\'e}ry, Marc},",
        "  journal = {Ecology},",
        "  volume = {99},",
        "  number = {2},",
        "  pages = {281--288},",
        "  year = {2018},",
        "  doi = {10.1002/ecy.2093}",
        "}",
        sep = "\n"
      )
    ),
    knape_overdispersion_2018 = list(
      text = "Knape J, Arlt D, Barraquand F, Berg A, Chevalier M, Part T, Ruete A and Zmihorski M (2018). Sensitivity of binomial N-mixture models to overdispersion: the importance of assessing model fit. Methods in Ecology and Evolution, 9(10), 2102-2114. https://doi.org/10.1111/2041-210X.13062",
      bibtex = paste(
        "@article{knape2018overdispersion,",
        "  title = {Sensitivity of binomial {N}-mixture models to overdispersion: the importance of assessing model fit},",
        "  author = {Knape, Jonas and Arlt, Debora and Barraquand, Fr{\\'e}d{\\'e}ric and Berg, {\\AA}ke and Chevalier, Mathieu and P{\\\"a}rt, Tomas and Ruete, Alejandro and {\\.Z}mihorski, Micha{\\l}},",
        "  journal = {Methods in Ecology and Evolution},",
        "  volume = {9},",
        "  number = {10},",
        "  pages = {2102--2114},",
        "  year = {2018},",
        "  doi = {10.1111/2041-210X.13062}",
        "}",
        sep = "\n"
      )
    ),
    mackenzie_occu_2002 = list(
      text = "MacKenzie DI, Nichols JD, Lachman GB, Droege S, Royle JA and Langtimm CA (2002). Estimating site occupancy rates when detection probabilities are less than one. Ecology, 83(8), 2248-2255. https://doi.org/10.1890/0012-9658(2002)083[2248:ESORWD]2.0.CO;2",
      bibtex = paste(
        "@article{mackenzie2002occupancy,",
        "  title = {Estimating site occupancy rates when detection probabilities are less than one},",
        "  author = {MacKenzie, Darryl I. and Nichols, James D. and Lachman, Gideon B. and Droege, Sam and Royle, J. Andrew and Langtimm, Catherine A.},",
        "  journal = {Ecology},",
        "  volume = {83},",
        "  number = {8},",
        "  pages = {2248--2255},",
        "  year = {2002},",
        "  doi = {10.1890/0012-9658(2002)083[2248:ESORWD]2.0.CO;2}",
        "}",
        sep = "\n"
      )
    ),
    royle_dorazio_2008 = list(
      text = "Royle JA and Dorazio RM (2008). Hierarchical Modeling and Inference in Ecology: The Analysis of Data from Populations, Metapopulations and Communities. Academic Press.",
      bibtex = paste(
        "@book{royle2008hierarchical,",
        "  title = {Hierarchical Modeling and Inference in Ecology: The Analysis of Data from Populations, Metapopulations and Communities},",
        "  author = {Royle, J. Andrew and Dorazio, Robert M.},",
        "  year = {2008},",
        "  publisher = {Academic Press}",
        "}",
        sep = "\n"
      )
    ),
    niku_gllvm_2019 = list(
      text = "Niku J, Brooks W, Herliansyah R, Hui FKC, Taskinen S and Warton DI (2019). Efficient estimation of generalized linear latent variable models. PLoS ONE, 14(5), e0216129. https://doi.org/10.1371/journal.pone.0216129",
      bibtex = paste(
        "@article{niku2019gllvm,",
        "  title = {Efficient estimation of generalized linear latent variable models},",
        "  author = {Niku, Jenni and Brooks, Wesley and Herliansyah, Riki and Hui, Francis K. C. and Taskinen, Sara and Warton, David I.},",
        "  journal = {PLoS ONE},",
        "  volume = {14},",
        "  number = {5},",
        "  pages = {e0216129},",
        "  year = {2019},",
        "  doi = {10.1371/journal.pone.0216129}",
        "}",
        sep = "\n"
      )
    ),
    harrison_dmm_2020 = list(
      text = "Harrison JG, Calder WJ, Shastry V and Buerkle CA (2020). Dirichlet-multinomial modelling outperforms alternatives for analysis of microbiome and other ecological count data. Molecular Ecology Resources, 20(2), 481-497. https://doi.org/10.1111/1755-0998.13128",
      bibtex = paste(
        "@article{harrison2020dmm,",
        "  title = {{D}irichlet-multinomial modelling outperforms alternatives for analysis of microbiome and other ecological count data},",
        "  author = {Harrison, Joshua G. and Calder, W. John and Shastry, Vivaswat and Buerkle, C. Alex},",
        "  journal = {Molecular Ecology Resources},",
        "  volume = {20},",
        "  number = {2},",
        "  pages = {481--497},",
        "  year = {2020},",
        "  doi = {10.1111/1755-0998.13128}",
        "}",
        sep = "\n"
      )
    )
  )
}

# Shared predicate: does the fit's family resolve to a given
# user-facing name? Routes through `resolve_family_name()` so the
# customfamily storage convention (e.g. name = "nmix" / "tweedie"
# while family = "custom") is recognised correctly. All per-name
# predicates below are one-line wrappers; new closure-unit /
# custom families add a wrapper rather than re-coding the
# null-check + identical() boilerplate.
#' @noRd
family_name_is <- function(object, name) {
  if (is.null(object$family)) return(FALSE)
  identical(resolve_family_name(object$family), name)
}

# Per-family predicates. `uses_nmix_family()` matches the original
# Royle (2004) Poisson-binomial variant only (not the RN or PPM
# variants, which have their own citation rules).
#' @noRd
uses_nmix_family <- function(object) {
  family_name_is(object, "nmix")
}

#' @noRd
uses_nmix_royle_nichols_family <- function(object) {
  family_name_is(object, "nmix_royle_nichols")
}

#' @noRd
uses_nmix_poisson_poisson_family <- function(object) {
  family_name_is(object, "nmix_poisson_poisson")
}

#' @noRd
uses_occ_family <- function(object) {
  family_name_is(object, "occ")
}

#' @noRd
uses_tweedie_family <- function(object) {
  family_name_is(object, "tweedie")
}

#' @noRd
uses_beta_nb_family <- function(object) {
  family_name_is(object, "beta_nb")
}

# Heavy-tailed latent innovations are a modelling choice worth
# describing, so the methods text reports them rather than leaving the
# process implicitly Gaussian.
#' @noRd
uses_heavy_tailed_trend <- function(object) {
  df <- object$trend_metadata$df %||% Inf
  !is_gaussian_df(df)
}

#' @noRd
uses_com_binomial_family <- function(object) {
  family_name_is(object, "com_binomial")
}

# Simplex multi-response: diri(), multi(), categ(). Detect via the
# mvgam_simplex_response attribute set by the constructor.
#' @noRd
uses_simplex_response <- function(object) {
  is_simplex_response_family(object$family)
}

# Multivariate continuous response: mvn(), mvt(). Detect via the
# mvgam_multi_response attribute while excluding the simplex trio.
#' @noRd
uses_mv_continuous_response <- function(object) {
  is_multi_response_family(object$family) &&
    !is_simplex_response_family(object$family)
}



# Predicate: was the fit produced by the jsdgam() wrapper? Keys
# off the class hierarchy set in jsdgam() (c("mvgam", "jsdgam"))
# rather than family or trend type, because jsdgam composes onto
# the standard mvgam factor-model path and shares the family /
# trend surface with plain mvgam fits.
#' @noRd
uses_jsdgam <- function(object) {
  inherits(object, "jsdgam")
}


# Predicate: does the fit use any approximate-GP smooth on the
# observation or trend side? Scans the formulas for `gp(`. The
# brms-integration architecture parses gp() smooths in the
# brms formula machinery rather than carrying a separate
# `gp_att_table` attribute, so the formula scan is the single
# detection point that stays accurate as the architecture
# evolves.
#'@noRd
uses_gp_smooth <- function(object) {
  has_gp <- function(f) {
    if (is.null(f)) return(FALSE)
    txt <- if (inherits(f, "brmsformula")) {
      paste(format(f$formula), collapse = " ")
    } else {
      paste(format(f), collapse = " ")
    }
    grepl("\\bgp\\(", txt)
  }
  has_gp(object$formula) || has_gp(object$trend_formula)
}


# Threading detection: returns TRUE iff the compiled stancode
# contains a `reduce_sum(...)` call -- either the brms-emitted
# `partial_log_lik_lpmf` for native families, or one of the
# mvgam-emitted `partial_sum_*_lpmf` wrappers for closure-unit
# families. Either path means within-chain parallelism was on.
#'@noRd
uses_threading <- function(object) {
  sc <- object$stancode
  if (is.null(sc)) return(FALSE)
  grepl("reduce_sum\\(", sc)
}


# Read sampling info from the stanfit S4 on `object$fit`.
# mvgam normalises both rstan and cmdstanr output into a
# stanfit, so the `stan_args` slot is the only access path
# needed. Returns NULL when stan_args is empty (variational /
# Laplace / Pathfinder fits).
#
# Fields returned: chains, warmup, iter (consumed by
# `how_to_cite()` for the methods-section sentence) plus the
# extended set (threads, adapt_delta, max_treedepth, init) that
# `methods_md()`'s Implementation block emits. Defaults stay
# NA when the user accepted the brms / Stan default so callers
# can branch on `is.na()` to decide whether to print.
# An initial-value specification can be echoed back into a
# reproduction call only when it is a single keyword. Numeric, list
# and function starts are not printable, and neither is the temporary
# file path Stan records in their place.
#'@noRd
printable_init <- function(init) {
  if (is.null(init) || length(init) != 1L || !is.character(init)) {
    return(NA_character_)
  }
  if (identical(init, "random") || !nzchar(init)) {
    return(NA_character_)
  }
  init
}

#'@noRd
extract_sampling_info <- function(object) {
  fit <- object$fit
  if (is.null(fit) || !methods::is(fit, "stanfit")) return(NULL)
  sa <- methods::slot(fit, "stan_args")
  if (length(sa) == 0L) return(NULL)
  first <- sa[[1L]]
  ctl <- first$control
  list(
    chains        = length(sa),
    warmup        = first$warmup %||% NA_integer_,
    iter          = first$iter   %||% NA_integer_,
    threads       = as.integer(
      first$threads_per_chain %||% NA_integer_
    ),
    adapt_delta   = if (is.null(ctl)) NA_real_ else {
      as.numeric(ctl$adapt_delta %||% NA_real_)
    },
    max_treedepth = if (is.null(ctl)) NA_integer_ else {
      as.integer(ctl$max_treedepth %||% NA_integer_)
    },
    init          = printable_init(first$init)
  )
}


#' @rdname how_to_cite.mvgam
#' @method how_to_cite mvgam
#' @export
how_to_cite.mvgam <- function(object, ...) {
  db <- reference_db()

  # Always-cited references: mvgam itself plus its essential
  # upstream stack.
  refs <- c("clark_dgam", "burkner_brms", "wood_gam")
  methods_text <- paste0(
    "We used the R package mvgam (version ",
    utils::packageVersion("mvgam"),
    "; Clark & Wells, 2023) to construct, fit and interrogate the model.",
    " mvgam fits Bayesian state-space models that combine flexible",
    " predictor effects in both the process and observation components,",
    " building on functionality from the brms (Burkner 2017) and",
    " mgcv (Wood 2017) packages."
  )

  trend_model <- object$trend_components$types[1L] %||% ""

  # Detection table: each rule pairs a TRUE/FALSE predicate
  # with the methods text it appends and the reference keys it
  # adds. Keeping all conditional citations in one structure
  # avoids scattering append() calls across the function and
  # makes new citations a one-line addition.
  rules <- list(
    list(
      detect = grepl("^VAR", trend_model),
      text = " To encourage stability and prevent forecast variance from increasing indefinitely, we enforced stationarity of the Vector Autoregressive process following Heaps (2023) and Clark et al. (2025).",
      refs = c("heaps_var", "clark_multispecies")
    ),
    list(
      detect = uses_gp_smooth(object),
      text = " Gaussian Process functional effects were estimated using the low-rank Hilbert-space approximation of Riutort-Mayol et al. (2023).",
      refs = "riutort_gp"
    ),
    list(
      detect = grepl("^PW", trend_model),
      text = " Piecewise dynamic trends were parameterised following Taylor and Letham (2018).",
      refs = "taylor_pw"
    ),
    list(
      # Free-loadings branch: default factor model (trend_map
      # NULL or all-NA) and jsdgam under Heaps identification.
      # The QR rotation only applies here.
      detect = !is.null(detect_factor_n_lv(object)) &&
                 is.null(object$mv_spec$trend_specs$fixed_Z),
      text = " Latent-factor loadings were sampled unconstrained and identified post-hoc via thin QR decomposition following Heaps and Jermyn (2024).",
      refs = "heaps_jermyn"
    ),
    list(
      # Fixed-loadings branch: user supplied a `trend_map`
      # with all finite entries. Z is data, so the QR
      # identification does not apply -- the encoded sharing
      # pattern is preserved verbatim.
      detect = !is.null(detect_factor_n_lv(object)) &&
                 !is.null(object$mv_spec$trend_specs$fixed_Z),
      text = " Latent-factor loadings were fixed at the user-supplied `trend_map`, preserving the encoded series-to-trend sharing structure exactly.",
      refs = character(0)
    ),
    list(
      detect = uses_jsdgam(object),
      text = paste0(
        " The joint species distribution model layered the",
        " latent-factor covariance directly on the species axis,",
        " following the general framework of Warton et al.",
        " (2015) and the structured implementations of",
        " Ovaskainen et al. (2017) and Tikhonov et al. (2020).",
        " The ordination and residual-correlation post-fit",
        " summaries follow the conventions of Hui (2016)."
      ),
      refs = c(
        "warton_jsdm",
        "ovaskainen_hmsc_2017",
        "tikhonov_hmsc_2020",
        "hui_boral"
      )
    ),
    list(
      detect = !is.null(detect_factor_n_lv(object)) &&
               uses_loadings_prior(object),
      text = " Domain knowledge was encoded into the loadings prior via a structured matrix-normal distribution whose among-row scale was assembled from per-series features and pairwise distance matrices (Heaps and Jermyn, 2024).",
      refs = "heaps_jermyn"
    ),
    list(
      detect = !is.null(detect_factor_n_lv(object)) &&
               !is.null(object$standata$mgp_a1),
      text = " Column shrinkage on the factor variances used the multiplicative gamma process of Bhattacharya and Dunson (2011).",
      refs = "bhattacharya_mgp"
    ),
    list(
      detect = uses_tweedie_family(object),
      text = paste0(
        " Observations were modelled with the Tweedie compound",
        " Poisson-gamma family (Jorgensen 1987), with the",
        " log-density evaluated via the truncated Poisson-gamma",
        " series of Dunn and Smyth (2005)."
      ),
      refs = c("jorgensen_tweedie", "dunn_smyth_tweedie")
    ),
    list(
      detect = uses_heavy_tailed_trend(object),
      text = paste0(
        " Innovations of the latent process followed a multivariate",
        " Student-t distribution. Its tails absorb an occasional large",
        " shock, which leaves the process variance of the rest of the",
        " series unchanged (Durbin and Koopman 2012). The innovation",
        " scale is shared across series at each time point: large",
        " innovations tend to occur together, while each series keeps",
        " its own direction and magnitude."
      ),
      refs = "durbin_koopman_ssm"
    ),
    list(
      detect = uses_beta_nb_family(object),
      text = paste0(
        " Counts were modelled with the beta negative binomial",
        " family (Irwin 1968), which mixes the negative binomial",
        " success probability over a beta distribution. Its tail",
        " decays as a power law."
      ),
      refs = "irwin_waring"
    ),
    list(
      detect = uses_com_binomial_family(object),
      text = paste0(
        " Bounded counts were modelled with the",
        " Conway-Maxwell-Binomial family (Shmueli et al. 2005;",
        " Conway and Maxwell 1962), extending the binomial with",
        " a dispersion exponent that admits both under- and",
        " super-dispersed mass on a fixed trials grid. The Stan",
        " lpmf draws on the adjoint and stability work of",
        " Bogomolovas (2024)."
      ),
      refs = c(
        "shmueli_cmb", "conway_maxwell_1962",
        "bogomolovas_cmb_tmb"
      )
    ),
    list(
      detect = uses_nmix_family(object),
      text = paste0(
        " Counts were modelled with the Poisson-binomial",
        " N-mixture family (Royle 2004), with the latent",
        " abundance marginalised analytically over a truncated",
        " support per closure unit using the log-sum-exp form",
        " of Dennis et al. (2015). Identifiability of detection",
        " and abundance under the visit structure was assessed",
        " following Kery (2018), and sensitivity to abundance",
        " over-dispersion was considered after Knape et al.",
        " (2018)."
      ),
      refs = c(
        "royle_nmix_2004",
        "dennis_nmix_2015",
        "kery_nmix_2018",
        "knape_overdispersion_2018"
      )
    ),
    list(
      detect = uses_nmix_royle_nichols_family(object),
      text = paste0(
        " Binary detection / non-detection histories were modelled",
        " with the Royle-Nichols N-mixture family",
        " (Royle and Nichols 2003), where the per-visit detection",
        " probability `1 - (1 - r)^N` marginalises a per-",
        " individual detection `r` over latent abundance",
        " `N ~ Poisson(lambda)`. The marginalisation uses the",
        " log-sum-exp form of Dennis et al. (2015) over a",
        " truncated per-unit support, and identifiability under",
        " the visit structure was assessed following Kery (2018)."
      ),
      refs = c(
        "royle_nichols_2003",
        "dennis_nmix_2015",
        "kery_nmix_2018"
      )
    ),
    list(
      detect = uses_nmix_poisson_poisson_family(object),
      text = paste0(
        " Encounter counts were modelled with the Poisson-Poisson",
        " N-mixture family, where per-visit counts `y ~ Poisson(N * p)`",
        " marginalise the per-individual encounter rate `p` over a",
        " latent abundance `N ~ Poisson(lambda)`. The compound",
        " marginal is the Neyman Type A distribution (Neyman 1939),",
        " over-dispersed relative to Poisson; the marginalisation",
        " uses the log-sum-exp form of Dennis et al. (2015) over a",
        " truncated per-unit support, and identifiability of",
        " `lambda` and `p` separately requires either a covariate",
        " on one of the formulae or an informative prior on at",
        " least one intercept (Kery 2018)."
      ),
      refs = c(
        "neyman_type_a_1939",
        "dennis_nmix_2015",
        "kery_nmix_2018"
      )
    ),
    list(
      detect = uses_occ_family(object),
      text = paste0(
        " Detection / non-detection histories were modelled with",
        " the single-season Bernoulli-binomial occupancy family",
        " (MacKenzie et al. 2002), with the latent occupancy state",
        " marginalised analytically per closure unit via",
        " log-sum-exp under the hierarchical framing of Royle and",
        " Dorazio (2008)."
      ),
      refs = c(
        "mackenzie_occu_2002",
        "royle_dorazio_2008"
      )
    ),
    list(
      detect = uses_simplex_response(object),
      text = paste0(
        " The multi-category response (proportions, counts or",
        " single-category outcomes across K mutually exclusive",
        " categories) was modelled with a softmax-link",
        " observation likelihood following the Dirichlet-multinomial",
        " framing of Harrison et al. (2020); the per-category",
        " linear predictors share the factor-loading structure of",
        " the generalised linear latent variable model (Niku et al.",
        " 2019) extended to joint species distribution modelling",
        " by Tikhonov et al. (2020). Mode-1 unidentified column",
        " shifts on the loadings matrix are removed with Stan's",
        " sum_to_zero_vector parameterisation, and the per-site",
        " mode-2 shift is removed by subtracting the reference",
        " category from the per-site linear predictor inside the",
        " custom log-density."
      ),
      refs = c(
        "harrison_dmm_2020",
        "niku_gllvm_2019",
        "tikhonov_hmsc_2020"
      )
    ),
    list(
      detect = uses_mv_continuous_response(object),
      text = paste0(
        " The multivariate continuous response was modelled with",
        " a conditional generalised linear latent variable",
        " parameterisation (Niku et al. 2019) in which",
        " per-category linear predictors share a low-rank factor",
        " loading; the resulting marginal residual covariance",
        " `Z Z' + diag(Psi^2)` (Gaussian) or",
        " `Z Z' + diag(Psi^2 * nu / (nu - 2))` (Student-t with",
        " hard floor at nu > 2) follows the joint species",
        " distribution framing of Tikhonov et al. (2020)."
      ),
      refs = c(
        "niku_gllvm_2019",
        "tikhonov_hmsc_2020"
      )
    ),
    list(
      detect = uses_threading(object),
      text = paste0(
        " Sampling was accelerated by within-chain parallelism via",
        " Stan's `reduce_sum` (Intel oneTBB) on the closure-unit",
        " or population likelihood, as exposed through brms's",
        " threading interface (Burkner 2017)."
      ),
      refs = "burkner_brms"
    )
  )

  for (rule in rules) {
    if (isTRUE(rule$detect)) {
      methods_text <- paste0(methods_text, rule$text)
      refs <- c(refs, rule$refs)
    }
  }

  # Stan backend + algorithm.
  refs <- c(refs, "stan")
  backend <- object$backend %||% "rstan"
  stan_text <- " The mvgam-constructed model and data were passed to Stan"
  if (backend == "cmdstanr") {
    stan_text <- paste0(
      stan_text,
      " (Carpenter et al. 2017) via the cmdstanr interface",
      " (Gabry et al. 2024)."
    )
    refs <- c(refs, "cmdstanr")
  } else {
    stan_text <- paste0(
      stan_text,
      " (Carpenter et al. 2017) via the rstan interface",
      " (Stan Development Team 2024)."
    )
    refs <- c(refs, "rstan")
  }

  algorithm <- object$algorithm %||% "sampling"
  info <- extract_sampling_info(object)
  if (algorithm == "sampling") {
    if (!is.null(info)) {
      stan_text <- paste0(
        stan_text,
        " We ran ", info$chains,
        " Hamiltonian Monte Carlo chains for ", info$warmup,
        " warmup iterations and ", info$iter - info$warmup,
        " sampling iterations."
      )
    } else {
      stan_text <- paste0(
        stan_text,
        " We ran Hamiltonian Monte Carlo for joint posterior estimation."
      )
    }
    stan_text <- paste0(
      stan_text,
      " Rank-normalised split Rhat and effective sample sizes",
      " (Vehtari et al. 2021) were used to monitor convergence."
    )
    refs <- c(refs, "vehtari_rhat")
  } else if (algorithm %in% c("meanfield", "fullrank")) {
    stan_text <- paste0(
      stan_text,
      " We used Stan's Automatic Differentiation Variational Inference",
      " (Kucukelbir et al. 2017), specifically the ", algorithm,
      " algorithm, to draw samples from the approximate joint posterior."
    )
    refs <- c(refs, "kucukelbir_advi")
  } else if (algorithm == "laplace") {
    stan_text <- paste0(
      stan_text,
      " We used Stan's Laplace approximation algorithm to draw",
      " samples from the approximate joint posterior."
    )
  } else if (algorithm == "pathfinder") {
    stan_text <- paste0(
      stan_text,
      " We used Stan's Pathfinder variational approximation",
      " (Zhang et al. 2022) to draw samples from the approximate posterior."
    )
    refs <- c(refs, "zhang_pathfinder")
  }

  methods_text <- paste0(methods_text, stan_text)

  refs <- unique(refs)
  citations <- lapply(refs, function(k) db[[k]]$text)
  bibtex <- stats::setNames(
    lapply(refs, function(k) db[[k]]$bibtex),
    refs
  )

  other_keys <- c(
    "arel_bundock_marginaleffects", "gabry_workflow",
    "vehtari_loo", "burkner_lfo"
  )
  other_citations <- lapply(other_keys, function(k) db[[k]]$text)
  other_bibtex <- stats::setNames(
    lapply(other_keys, function(k) db[[k]]$bibtex),
    other_keys
  )

  structure(
    list(
      methods_text = methods_text,
      citations = citations,
      other_citations = other_citations,
      bibtex = c(bibtex, other_bibtex)
    ),
    class = "how_to_cite"
  )
}


#' Extract the BibTeX block from a `how_to_cite` object
#'
#' Concatenates the matched BibTeX entries into one string.
#' Pass `file = "refs.bib"` to write the block to disk; the
#' default returns the string invisibly and prints to stdout.
#'
#' @param x A `how_to_cite` object.
#' @param file Optional output path. When non-NULL the BibTeX
#'   is written to disk and the string is returned invisibly.
#' @param other Logical. When `TRUE` (default), include the
#'   curated "other useful references" block; set to `FALSE`
#'   to emit only the matched primary references.
#' @param ... Ignored.
#'
#' @return A single character string containing the BibTeX
#'   entries separated by blank lines.
#' @export
bibtex <- function(x, ...) {
  UseMethod("bibtex", x)
}


#' @rdname bibtex
#' @export
bibtex.how_to_cite <- function(x, file = NULL, other = TRUE, ...) {
  entries <- if (isTRUE(other)) x$bibtex else {
    primary_keys <- setdiff(names(x$bibtex), c(
      "arel_bundock_marginaleffects", "gabry_workflow",
      "vehtari_loo", "burkner_lfo"
    ))
    x$bibtex[primary_keys]
  }
  out <- paste(unlist(entries), collapse = "\n\n")
  if (!is.null(file)) {
    writeLines(out, file)
    return(invisible(out))
  }
  cat(out, "\n", sep = "")
  invisible(out)
}


#' @export
print.how_to_cite <- function(x, ...) {
  cat("Methods text skeleton\n")
  cat(insight::format_message(x$methods_text))
  cat("\n\n")

  print_sorted <- function(citations, header) {
    if (length(citations) == 0L) return(invisible(NULL))
    cat(header, "\n", sep = "")
    refs <- vapply(citations, identity, character(1L))
    for (ref in refs[order(refs)]) {
      cat(insight::format_message(ref))
      cat("\n")
    }
  }
  print_sorted(x$citations, "Primary references")
  cat("\n")
  print_sorted(x$other_citations, "Other useful references")

  cat("\nUse `bibtex(x)` for a BibTeX block ready for your .bib file.\n")
  invisible(x)
}
