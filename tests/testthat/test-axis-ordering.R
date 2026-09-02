# The three axes a trend is indexed by -- time, series and latent
# factor -- are each derived from the data and then read back by Stan
# against a matrix built on the same derivation. When a derivation
# answers differently in two places the indices still fall in range,
# so nothing raises and the model reads another series' or another
# time's state without saying so.
#
# The checks below assert invariants rather than recomputing what the
# package computes. Recomputing would restate the implementation and
# pass whatever it does; an invariant holds only if the answer is
# right. Four carry the weight:
#
#   Permutation. The axes are properties of the data, not of the
#   order the rows arrive in, so shuffling the frame must leave every
#   observation's indices attached to the same observation.
#
#   Chronology. A trend recursion steps along its time index, so that
#   index must run in the order the times do, and the real gaps that
#   `CAR()` and the Gaussian processes measure must agree with it.
#
#   Bounds. Every index must fall inside the matrix it reads, which
#   is what catches an axis built at one grain and read at another.
#
#   Completeness. Between them the observations must use the whole
#   axis, which catches an axis wider than anything reaches.
#
# These run on `standata()` and `stancode()` alone, so they cover the
# fit side. What a fitted object reports, and whether it agrees with
# what the fit recorded, needs sampling and lives in
# `tests/local/postfit_sweep.R`.

# The per-response arrays on a multi-response fit, or the single pair
# on a one-response fit, as a list of (time, series) index vectors.
axis_index_arms <- function(sd, resp_names) {
  if (is.null(resp_names)) {
    return(list(list(
      time = as.integer(sd$obs_trend_time),
      series = as.integer(sd$obs_trend_series)
    )))
  }
  stats::setNames(lapply(resp_names, function(r) list(
    time = as.integer(sd[[paste0("obs_trend_time_", r)]]),
    series = as.integer(sd[[paste0("obs_trend_series_", r)]])
  )), resp_names)
}

# The time column a fit was built on.
time_col <- function(prefit) {
  prefit$trend_metadata$variables$time_var %||% "time"
}

# The axis a frame ought to produce, written out rather than derived,
# so the check has something to compare against that did not come
# from the code under test. `NULL` means the responses supply the
# labels and `resp_names` carries them.
#
# A `gr` / `subgr` pair respells any series column the frame also
# holds, joining the two names with an underscore, so the
# hierarchical frames do not name their own column's levels verbatim.
frame_axis_labels <- function(name) {
  # A hierarchical trend names its series by the grouping, which
  # `hierarchical_series_values()` builds with `lex.order = TRUE`, so
  # a region's species sit together. A supplied `series` column is
  # superseded in order as well as in spelling: `hier_col` declares
  # north, south, north, south and the trend still runs north, north,
  # south, south. Written out rather than derived, so this states the
  # answer instead of recomputing it.
  hier <- c("north_sp_a", "north_sp_b", "south_sp_a", "south_sp_b")
  switch(
    name,
    long = c("a_site", "c_site", "b_site"),
    unused = c("a_site", "c_site", "b_site"),
    char_series = c("a_site", "b_site", "c_site"),
    uni = "only",
    unbal = c("a_site", "c_site", "b_site"),
    hier = hier,
    hier_col = hier,
    wide_col = "one_site",
    # The responses name these axes; `resp_names` carries them.
    wide = NULL,
    wide_na = NULL,
    NULL
  )
}

# The loadings a `trend_map` cell asked for, series by series,
# written out rather than recovered from the matrix the cell handed
# in, so the check has a statement of intent that did not come from
# the normaliser. `NA` marks an entry left free. A label absent here
# has no `trend_map`, and the block that reads this no-ops.
cell_fixed_loadings <- function(lab) {
  switch(
    lab,
    "trend_map matrix / long" = list(
      a_site = c(1, 0), c_site = c(0, 1), b_site = c(1, 0)
    ),
    "trend_map matrix / character" = list(
      a_site = c(1, 0), b_site = c(0, 1), c_site = c(1, 0)
    ),
    "trend_map frame / long" = list(
      a_site = c(0, 1), c_site = c(1, 0), b_site = c(1, 0)
    ),
    "trend_map frame / character" = list(
      a_site = c(0, 1), b_site = c(1, 0), c_site = c(1, 0)
    ),
    "trend_map partial / long" = list(
      a_site = c(1, NA), c_site = c(0, 1), b_site = c(NA, 0)
    ),
    "trend_map free mask / long" = list(
      a_site = c(NA, NA), c_site = c(NA, NA), b_site = c(NA, NA)
    ),
    "trend_map shared / long" = list(
      a_site = 1, c_site = 1, b_site = 1
    ),
    "trend_map identity / long" = list(
      a_site = c(1, 0, 0), c_site = c(0, 1, 0), b_site = c(0, 0, 1)
    ),
    "trend_map VAR / long" = list(
      a_site = c(1, 0), c_site = c(0, 1), b_site = c(1, 0)
    ),
    "trend_map ZMVN / long" = list(
      a_site = c(1, 0), c_site = c(0, 1), b_site = c(1, 0)
    ),
    "trend_map matrix / wide col" = list(one_site = 1),
    NULL
  )
}

# How the frame's series axis was arrived at, as the fit recorded it.
meta_series_source <- function(prefit) {
  prefit$trend_metadata$series_source %||% NA_character_
}

# Bounds, completeness and the matrix shapes, for one emitted program.
expect_axes_sound <- function(prefit, resp_names, lab, frame,
                              frame_name) {
  sd <- prefit$standata
  meta <- prefit$trend_metadata
  n_time <- as.integer(sd$N_time_trend)
  n_series <- as.integer(sd$N_series_trend)

  expect_identical(
    dim(sd$times_trend), c(n_time, n_series),
    label = paste(lab, "times_trend shape")
  )
  # `times_trend[i, s]` indexes the trend-side design, and every
  # design row is named exactly once. `<=` alone would pass a
  # map that never reaches the last rows, or one that names a row
  # twice and leaves another unread.
  expect_setequal(
    as.integer(sd$times_trend), seq_len(as.integer(sd$N_trend))
  )

  arms <- axis_index_arms(sd, resp_names)
  all_series <- unlist(lapply(arms, `[[`, "series"), use.names = FALSE)
  all_time <- unlist(lapply(arms, `[[`, "time"), use.names = FALSE)
  # Nothing on the axis is unreachable. On its own this proves very
  # little: three responses each reading series one and two still
  # cover the axis between them, which is the exact shape of the
  # defect this file exists for. The per-arm checks below are the
  # ones that do the work, because they compare against the frame
  # rather than against another part of the same derivation.
  expect_setequal(unique(all_series), seq_len(n_series))
  expect_setequal(unique(all_time), seq_len(n_time))

  # A factor axis cannot be wider than the series it loads.
  if (!is.null(sd$N_lv_trend)) {
    expect_true(
      as.integer(sd$N_lv_trend) <= n_series,
      label = paste(lab, "factors do not outnumber series")
    )
  }

  # `time_dis` is what `CAR()` raises its coefficient to, so it is
  # indexed by the same time axis. One shared grid means the columns
  # are equal, and an NA would reach a `real<lower=0>` declaration.
  if (!is.null(sd$time_dis)) {
    expect_identical(dim(sd$time_dis), c(n_time, n_series))
    expect_false(anyNA(sd$time_dis),
                 label = paste(lab, "time_dis resolved"))
    for (col in seq_len(n_series)) {
      expect_identical(
        as.numeric(sd$time_dis[, col]),
        as.numeric(sd$time_dis[, 1L]),
        label = paste(lab, "time_dis shares one grid")
      )
    }
  }

  # `group_inds_trend[s]` names the group the series in column `s`
  # belongs to, so it is subscripted by the trend's own series index.
  # Nothing else compares the two, and a mapping read in row order
  # answers with a permutation that stays in range.
  if (!is.null(sd$group_inds_trend)) {
    gi <- as.integer(sd$group_inds_trend)
    expect_false(anyNA(gi), label = paste(lab, "group_inds resolved"))
    expect_identical(length(gi), n_series)
    expect_identical(
      as.integer(sd$N_subgroups_trend), max(as.integer(table(gi))),
      label = paste(lab, "subgroups is the largest group")
    )
    # Counting the groups and checking the set are both unchanged by
    # any permutation of `gi`, so on their own they cannot tell
    # `1,2,1,2` from `1,1,2,2`. Entry `s` has to name the group of the
    # series that occupies column `s`, which means reading the
    # occupancy out of the record and the group out of the frame.
    gr_var <- meta$variables$gr_var
    if (!is.null(gr_var) && !identical(gr_var, "NA") &&
          gr_var %in% names(frame) && !is.null(sd$obs_trend_series)) {
      row_series <- as.integer(sd$obs_trend_series)
      row_group <- as.character(frame[[gr_var]])
      group_of_column <- vapply(seq_len(n_series), function(k) {
        row_group[which(row_series == k)[1L]]
      }, character(1))
      expect_identical(
        gi, match(group_of_column, sort(unique(row_group))),
        label = paste(lab, "group_inds names each column's group")
      )
    }
  }

  grid <- sort(unique(frame[[time_col(prefit)]]))
  for (i in seq_along(arms)) {
    arm <- arms[[i]]
    tag <- paste(lab, names(arms)[i] %||% "")
    if (!is.null(resp_names)) {
      # Where the responses are the series, the series is fixed by
      # which response is being mapped: one value, and this
      # response's own position on the axis. A response holding two
      # values is reading part of another response's latent state.
      # A frame that names its own series says something different,
      # that the responses are measurements of one unit sharing a
      # state, so there each response reads the series its rows name.
      if (identical(meta_series_source(prefit), "multivariate")) {
        expect_identical(unique(arm$series), i,
                         label = paste(tag, "sits on its own series"))
      } else if (n_series > 1L) {
        # Only says something where there is more than one column to
        # choose between.
        expect_length(unique(arm$series), 1L)
      }
      # And its times are the occasions it was measured on, ranked
      # against the shared grid, so a block of the timeline handed
      # to one response cannot pass.
      observed <- which(!is.na(frame[[resp_names[i]]]))
      expect_identical(
        length(arm$series),
        as.integer(sd[[paste0("N_", resp_names[i])]]),
        label = paste(tag, "arm is as long as its response")
      )
      expect_identical(
        arm$time, as.integer(match(frame[[time_col(prefit)]][observed],
                                   grid)),
        label = paste(tag, "reads the times it was measured on")
      )
    }
  }

  # The emitted program reads the trend on the axes it was given.
  code <- paste(as.character(prefit$stancode), collapse = "\n")
  # `Z` is declared `... Z;` on a factor model and
  # `... Z = diag_matrix(...)` otherwise, so the name is followed by
  # one or the other. Matching the bare name would also accept
  # `Z_tilde`, the rotated copy, which says nothing about the axis
  # the loadings are declared on.
  expect_true(
    grepl("matrix\\[N_series_trend, N_lv_trend\\] Z(;| =)", code),
    label = paste(lab, "loadings on the series axis")
  )
  for (r in resp_names %||% "") {
    suffix <- if (nzchar(r)) paste0("_", r) else ""
    expect_true(
      grepl(
        paste0("trend[obs_trend_time", suffix,
               "[n], obs_trend_series", suffix, "[n]]"),
        code, fixed = TRUE
      ),
      label = paste(lab, r, "trend read on both axes")
    )
  }

  # What the object recorded has to agree with what reading it back
  # derives. This is the whole of the bug class: one fact recorded at
  # fit time, derived again elsewhere, the two disagreeing, and every
  # index still landing in range so nothing says a word.
  index <- mvgam:::fitted_series_index(prefit)
  expect_false(is.null(index), label = paste(lab, "index resolves"))
  # `fitted_series_index()` returns `seq_along()` over whichever
  # labels it settled on, so comparing its values against `1:n` says
  # only that it produced n of them. What matters is that the label
  # it gives a column is the label the record puts there, which the
  # occupancy below asks directly.
  expect_identical(length(index), n_series,
                   label = paste(lab, "index covers the axis"))
  if (is.null(resp_names) && !is.null(sd$obs_trend_series)) {
    labels <- as.character(mvgam:::get_series_for_grouping(
      mvgam:::prepare_mvgam_frame(prefit, frame)
    ))
    expect_identical(
      as.integer(sd$obs_trend_series),
      unname(as.integer(index[labels])),
      label = paste(lab, "index reproduces the recorded mapping")
    )
  }
  expect_identical(
    length(meta$levels$series), n_series,
    label = paste(lab, "stored levels count the axis")
  )
  expect_identical(
    as.character(names(index)), as.character(meta$levels$series),
    label = paste(lab, "labels match stored levels")
  )
  # The labels a user would name: the responses of a wide frame, in
  # the order the formula writes them, or the levels of the column
  # they supplied. Read from the frame rather than from the fit, so
  # the two derivations are not compared against each other.
  expected <- frame_axis_labels(frame_name) %||% resp_names
  if (!is.null(expected)) {
    expect_identical(
      as.character(names(index)), as.character(expected),
      label = paste(lab, "labels are the user's own")
    )
  }

  # A `trend_map` is the only route on which `Z` reaches Stan as
  # data, so it is the only place the row order of the loadings can
  # be compared against anything at all: everywhere else `Z` is a
  # parameter whose rows carry no labels until a fit exists. Row `k`
  # has to hold the loadings asked for for the series occupying trend
  # column `k`, and the occupant is read out of `obs_trend_series`
  # rather than out of the stored levels, so the two accounts of the
  # axis are not compared against each other.
  wanted <- cell_fixed_loadings(lab)
  if (!is.null(wanted)) {
    n_lv <- as.integer(sd$N_lv_trend)
    if (all(vapply(wanted, function(w) all(is.na(w)), logical(1)))) {
      # Every loading free is the sampled-Z model, so none of the
      # pattern reaches the data block. A template of zeros would
      # satisfy every shape check below and fix each loading at zero.
      expect_null(sd$Z, label = paste(lab, "fixes no loading"))
      expect_null(sd$Z_template, label = paste(lab, "templates none"))
    } else {
      z_data <- sd$Z %||% sd$Z_template
      expect_false(is.null(z_data),
                   label = paste(lab, "Z reaches Stan as data"))
      expect_identical(
        dim(z_data), c(n_series, n_lv),
        label = paste(lab, "Z is sized by the axis it is read on")
      )
      series_obs <- if (is.null(resp_names)) {
        as.integer(sd$obs_trend_series)
      } else {
        as.integer(sd[[paste0("obs_trend_series_", resp_names[1L])]])
      }
      col_labels <- as.character(frame$series)
      if (!is.null(resp_names)) {
        col_labels <- col_labels[!is.na(frame[[resp_names[1L]]])]
      }
      occupant <- vapply(seq_len(n_series), function(k) {
        col_labels[which(series_obs == k)[1L]]
      }, character(1))
      # The rows say who they belong to. The normaliser writes these
      # from its own reading of the series column, so agreeing with
      # the occupancy is the whole claim.
      expect_identical(
        rownames(z_data), occupant,
        label = paste(lab, "Z rows name the series they load")
      )
      for (k in seq_len(n_series)) {
        want <- wanted[[occupant[k]]]
        expect_identical(
          length(want), n_lv,
          label = paste(lab, occupant[k], "row spans the axis")
        )
        free <- is.na(want)
        expect_equal(
          as.numeric(z_data[k, !free]), as.numeric(want[!free]),
          tolerance = 1e-12,
          label = paste(lab, occupant[k], "loads as it was asked to")
        )
        if (!is.null(sd$Z_is_free)) {
          expect_identical(
            as.integer(sd$Z_is_free[k, ]), as.integer(free),
            label = paste(lab, occupant[k], "frees what was asked")
          )
          # The assembly loop reads the template wherever the mask
          # says fixed, so a free entry carrying a value would be
          # read in place of the sampled one if the mask ever moved.
          expect_equal(
            as.numeric(sd$Z_template[k, free]), rep(0, sum(free)),
            tolerance = 1e-12,
            label = paste(lab, occupant[k], "leaves free cells empty")
          )
        }
      }
      if (!is.null(sd$Z_is_free)) {
        expect_identical(
          as.integer(sd$N_free_Z),
          sum(vapply(wanted, function(w) sum(is.na(w)), integer(1))),
          label = paste(lab, "counts the free loadings")
        )
      }
    }
  }
}


# Frames, one per route by which a series axis comes to exist. The
# routes are what the checks below are crossed with, because a bug in
# the axis is a bug in how it was built rather than in which trend
# reads it.
axis_frames <- function() {
  set.seed(20240902L)
  n_t <- 15L

  # Levels deliberately out of alphabetical order, so an axis taken
  # from sorted labels differs from the one the data declares.
  # Level order, alphabetical order and first-appearance order are
  # three different permutations here. A frame where any two coincide
  # cannot say which one an index was built from.
  series_ids <- c("a_site", "c_site", "b_site")
  row_order <- c("c_site", "a_site", "b_site")
  long_grid <- expand.grid(
    time = seq_len(n_t), series = row_order,
    stringsAsFactors = FALSE
  )
  long <- data.frame(
    time   = long_grid$time,
    series = factor(long_grid$series, levels = series_ids),
    env    = rnorm(nrow(long_grid)),
    y      = rpois(nrow(long_grid), 5)
  )

  # One series, named. A frame carrying none of the three things an
  # axis can be built from is the `uni_bare` case below.
  uni <- data.frame(
    time = seq_len(n_t), series = factor(rep("only", n_t)),
    env = rnorm(n_t), y = rpois(n_t, 5)
  )
  uni_bare <- uni[, setdiff(names(uni), "series")]

  hier_grid <- expand.grid(
    time = seq_len(n_t), species = c("sp_b", "sp_a"),
    region = c("north", "south"), stringsAsFactors = FALSE
  )
  hier <- data.frame(
    time    = hier_grid$time,
    region  = factor(hier_grid$region),
    species = factor(hier_grid$species),
    env     = rnorm(nrow(hier_grid)),
    y       = rpois(nrow(hier_grid), 5)
  )
  # The same frame naming its series, which the grouping supersedes.
  hier_col <- hier
  hier_col$series <- interaction(
    hier$region, hier$species, drop = TRUE
  )

  # Rows deliberately not divisible by the response count, and
  # response names whose alphabetical order is not formula order.
  n_w <- 23L
  wide <- data.frame(
    # Regular, as `AR()` and `RW()` require, but not starting at one,
    # so an index taken from the raw time value is not the index
    # taken from its rank. Telling a rank from a row position needs
    # the rows shuffled, which the permutation test does.
    time  = seq_len(n_w) + 2L,
    env   = rnorm(n_w),
    zebra = rpois(n_w, 5),
    apple = rbinom(n_w, 1L, 0.5),
    mango = rnorm(n_w)
  )
  wide_col <- wide
  wide_col$series <- factor(rep("one_site", n_w))
  # Disjoint gaps, so no response can be recovered from another's
  # occasions and the three valid-row sets differ from one another.
  wide_na <- wide
  # Three, two and four dropped, so the arms are 20, 21 and 19 long
  # and a swapped pair shows up in the counts alone.
  wide_na$zebra[c(3L, 4L, 11L)] <- NA_integer_
  wide_na$apple[c(7L, 15L)] <- NA_integer_
  wide_na$mango[c(2L, 19L, 20L, 22L)] <- NA_real_

  # A panel whose series neither start nor end together.
  unbal <- long[!(long$series == "a_site" & long$time <= 3L), ]
  unbal <- unbal[!(unbal$series == "b_site" & unbal$time >= 14L), ]

  # A series column carrying a level nothing observes.
  unused <- long
  unused$series <- factor(
    as.character(unused$series), levels = c(series_ids, "ghost_site")
  )

  # Series named by character rather than by factor.
  char_series <- long
  char_series$series <- as.character(char_series$series)

  list(
    uni = uni, uni_bare = uni_bare,
    long = long, hier = hier, hier_col = hier_col,
    wide = wide, wide_col = wide_col, wide_na = wide_na,
    unbal = unbal, unused = unused, char_series = char_series
  )
}

# The message a cell is expected to produce, named so that any other
# warning still reaches the test rather than being swept up with it.
# A response carrying `NA` makes brms report the rows it drops, once
# per arm, which is what the gappy cells exercise.
axis_expected_warnings <- function() {
  "Rows containing NAs"
}

# The multivariate formula the wide frames are read with.
axis_wide_formula <- function() {
  bf(zebra ~ env, family = poisson()) +
    bf(apple ~ env, family = bernoulli()) +
    bf(mango ~ env, family = gaussian()) +
    set_rescor(FALSE)
}

# The multivariate formula a jsdgam species axis is written with.
# `jsdgam()` reads the species from the responses, so no interaction
# term and no species column appear.
axis_jsdgam_formula <- function() {
  bf(zebra ~ env, family = poisson()) +
    bf(apple ~ env, family = bernoulli()) +
    bf(mango ~ env, family = gaussian())
}

# Build one cell as a prefit. `run_model = FALSE` stops before Stan
# parse and compile and returns an object carrying `standata`,
# `stancode`, the training frame and the trend metadata, which is
# every record a later method reads. `spec` is a trend formula for
# the mvgam routes and a latent-factor count for the jsdgam ones.
axis_prefit <- function(frame, spec, route) {
  switch(
    route,
    uni = mvgam(
      y ~ env, trend_formula = spec, data = frame,
      family = poisson(), run_model = FALSE, silent = 2
    ),
    wide = mvgam(
      axis_wide_formula(), trend_formula = spec, data = frame,
      run_model = FALSE, silent = 2
    ),
    jsdgam_mv = jsdgam(
      formula = axis_jsdgam_formula(), factor_formula = ~ -1,
      data = frame, n_lv = spec, unit = time,
      run_model = FALSE, silent = 2
    ),
    jsdgam_species = jsdgam(
      formula = y ~ env, factor_formula = ~ -1, data = frame,
      n_lv = spec, unit = time, species = series,
      family = poisson(), run_model = FALSE, silent = 2
    ),
    stop("Unknown axis route: ", route)
  )
}

# One row per configuration mvgam can be asked for. `expect` is
# "sound" where the battery above must hold, and "refuse" where the
# combination is turned away. A refusal is asserted rather than
# skipped, so a combination that starts being accepted is noticed
# here rather than in a user's model.
axis_matrix <- function() {
  # `terms()` deparses a trend constructor's arguments before they
  # are evaluated, and deparsing turns `NA_real_` into `NA`, so an
  # inline all-`NA` matrix would arrive logical and be refused for
  # the wrong reason. Naming the matrices here sidesteps that: the
  # formula's environment is this one, and a name survives the round
  # trip.
  tm_fixed <- matrix(c(1, 0, 0, 1, 1, 0), nrow = 3, byrow = TRUE)
  tm_part <- matrix(c(1, NA, 0, 1, NA, 0), nrow = 3, byrow = TRUE)
  tm_free <- matrix(NA_real_, nrow = 3, ncol = 2)
  tm_hier <- matrix(c(1, 0, 0, 1, 1, 0, 0, 1), nrow = 4, byrow = TRUE)
  tm_one <- matrix(1, nrow = 1, ncol = 1)
  tm_frame <- data.frame(
    series = c("c_site", "a_site", "b_site"), trend = c(1L, 2L, 1L)
  )
  tm_stranger <- data.frame(
    series = c("a_site", "b_site", "c_site", "ghost_site"),
    trend = c(1L, 2L, 1L, 2L)
  )
  tribble_rows <- list(
    # Series named by a column.
    list("explicit / RW",        "long", ~ RW(),                  "sound", "uni"),
    list("explicit / AR1",       "long", ~ AR(p = 1),             "sound", "uni"),
    list("explicit / CAR",       "long", ~ CAR(),                 "sound", "uni"),
    list("explicit / factor",    "long", ~ AR(p = 1, n_lv = 2),   "sound", "uni"),
    list("explicit / covariate", "long", ~ env + AR(p = 1),       "sound", "uni"),

    # One series, named by a column.
    list("single / AR1", "uni", ~ AR(p = 1), "sound", "uni"),

    # A frame naming no series, carrying no grouping and holding one
    # response gives nothing to build an axis from, and is turned
    # away saying so.
    list("no axis to build", "uni_bare", ~ AR(p = 1), "refuse", "uni",
         "series variable"),

    # Series built from a grouping, with and without a column that
    # the grouping supersedes.
    list("hier col / AR1", "hier_col",
         ~ AR(p = 1, gr = region, subgr = species), "sound", "uni"),
    list("hier col / cor", "hier_col",
         ~ AR(p = 1, cor = TRUE, gr = region, subgr = species),
         "sound", "uni"),

    # Series taken from the responses of a wide frame.
    list("wide / AR1",       "wide", ~ AR(p = 1),           "sound", "wide"),
    list("wide / ZMVN 2",    "wide", ~ ZMVN(n_lv = 2),      "sound", "wide"),
    list("wide / factor",    "wide", ~ AR(p = 1, n_lv = 2), "sound", "wide"),
    list("wide / covariate", "wide", ~ env + AR(p = 1),     "sound", "wide"),
    list("wide / gaps",      "wide_na", ~ AR(p = 1),        "sound", "wide"),
    list("wide + column",    "wide_col", ~ AR(p = 1),       "sound", "wide"),

    # Columns that are awkward but legal, crossed with the trends
    # that reach the axis by different routes. One trend per awkward
    # frame is what let `CAR()` on a character column go unnoticed:
    # it reads the series column itself rather than the derived axis.
    list("unused level / AR1",  "unused", ~ AR(p = 1),   "sound", "uni"),
    list("unused level / CAR",  "unused", ~ CAR(),       "sound", "uni"),
    list("unused level / VAR",  "unused", ~ VAR(),       "sound", "uni"),
    list("unused level / ZMVN", "unused", ~ ZMVN(n_lv = 2), "sound", "uni"),
    list("character / AR1",  "char_series", ~ AR(p = 1), "sound", "uni"),
    list("character / VAR",  "char_series", ~ VAR(),     "sound", "uni"),
    list("character / ZMVN", "char_series", ~ ZMVN(n_lv = 2), "sound",
         "uni"),

    # The latent state lives on one time grid shared by every
    # series, so a panel whose series cover different times is
    # turned away and asked to pad with `NA` instead.
    list("unbalanced panel", "unbal", ~ AR(p = 1), "refuse", "uni",
         "time grid"),

    # Deliberately refused: a factor model cannot also be
    # hierarchical, because the per-group blocks and the loadings
    # both claim the series axis.
    list("hier + factor", "hier_col",
         ~ AR(p = 1, gr = region, subgr = species, n_lv = 2),
         "refuse", "uni", "factor model"),

    # `CAR()` reads its gaps from the shared time grid, so it holds
    # on a frame whose series is derived from the responses and on
    # one whose series column is character. Both once crashed, by
    # coercing that column with `as.numeric()`.
    list("wide / CAR", "wide", ~ CAR(), "sound", "wide"),
    list("character / CAR", "char_series", ~ CAR(), "sound", "uni"),
    # A grouping names the series, so a frame carrying `gr` and
    # `subgr` and no `series` column is a complete specification.
    # Both of these were refused, at a different layer each, by
    # guards that asked for the column rather than for the axis.
    list("hier / RW", "hier", ~ RW(gr = region, subgr = species),
         "sound", "uni"),
    list("hier / AR1", "hier",
         ~ AR(p = 1, gr = region, subgr = species),
         "sound", "uni"),

    # `jsdgam()` reaches the same axis by two routes of its own: the
    # species named in a column, and the species named as the
    # responses of a multivariate formula. Both put the loadings on
    # the series axis, so both belong in the same battery.
    # `trend_map` is the only route on which the loadings reach Stan
    # as data, so it is the only place a row of `Z` can be tied to a
    # series before a fit exists. The normaliser reads the series
    # column itself rather than the axis, so the frames that separate
    # level order from alphabetical order are the ones that say which
    # reading a row was built from.
    list("trend_map matrix / long", "long",
         ~ AR(p = 1, trend_map = tm_fixed), "sound", "uni"),
    list("trend_map matrix / character", "char_series",
         ~ AR(p = 1, trend_map = tm_fixed), "sound", "uni"),
    list("trend_map frame / long", "long",
         ~ AR(p = 1, trend_map = tm_frame), "sound", "uni"),
    list("trend_map frame / character", "char_series",
         ~ AR(p = 1, trend_map = tm_frame), "sound", "uni"),
    list("trend_map partial / long", "long",
         ~ AR(p = 1, trend_map = tm_part), "sound", "uni"),
    list("trend_map free mask / long", "long",
         ~ AR(p = 1, trend_map = tm_free), "sound", "uni"),
    list("trend_map shared / long", "long",
         ~ AR(p = 1, trend_map = "shared"), "sound", "uni"),
    list("trend_map identity / long", "long",
         ~ AR(p = 1, trend_map = "identity"), "sound", "uni"),
    list("trend_map VAR / long", "long",
         ~ VAR(trend_map = tm_fixed), "sound", "uni"),
    list("trend_map ZMVN / long", "long",
         ~ ZMVN(trend_map = tm_fixed), "sound", "uni"),
    # The only wide frame a `trend_map` reaches is one naming its own
    # series, and there the axis is that column rather than the
    # responses.
    list("trend_map matrix / wide col", "wide_col",
         ~ AR(p = 1, trend_map = tm_one), "sound", "wide"),

    list("trend_map on wide responses", "wide",
         ~ AR(p = 1, trend_map = tm_fixed), "refuse", "wide",
         "series' column"),
    list("trend_map with no series", "uni_bare",
         ~ AR(p = 1, trend_map = tm_one), "refuse", "uni",
         "series' column"),
    list("trend_map plus grouping", "hier_col",
         ~ AR(p = 1, gr = region, subgr = species,
              trend_map = tm_hier), "refuse", "uni", "factor model"),
    # Refused, but for the wrong reason. `hier_col` is turned away
    # because a grouping and the loadings both claim the series axis,
    # which is the real objection. `hier` never reaches that gate:
    # `normalise_trend_map()` reads the series column before any axis
    # exists and asks for a column this model does not need. The
    # message is pinned so the day it improves is noticed.
    list("trend_map on a grouping", "hier",
         ~ AR(p = 1, gr = region, subgr = species,
              trend_map = tm_hier), "refuse", "uni", "series' column"),
    list("trend_map fights n_lv", "long",
         ~ AR(p = 1, n_lv = 3, trend_map = tm_fixed), "refuse", "uni",
         "n_lv"),
    list("trend_map wrong height", "long",
         ~ AR(p = 1, trend_map = tm_hier), "refuse", "uni",
         "number of rows"),
    list("trend_map names a stranger", "long",
         ~ AR(p = 1, trend_map = tm_stranger), "refuse", "uni",
         "training data"),

    list("jsdgam species / 2", "long", 2L, "sound", "jsdgam_species"),
    list("jsdgam mv / 2",      "wide", 2L, "sound", "jsdgam_mv"),
    list("jsdgam mv / gaps",   "wide_na", 2L, "sound", "jsdgam_mv")
  )
  lapply(tribble_rows, function(r) {
    # A `refuse` row must name what the refusal says. Padding a short
    # row with `NA` would hand `expect_error()` a `regexp` of `NA`,
    # which asserts that no error occurs at all: the exact opposite of
    # the cell, passing in silence.
    if (identical(r[[4L]], "refuse") && length(r) < 6L) {
      stop("refuse cell '", r[[1L]], "' names no message to match")
    }
    r <- c(r, list(NA_character_))[seq_len(6L)]
    stats::setNames(
      r, c("label", "frame", "trend", "expect", "route", "says")
    )
  })
}


test_that("every model configuration keeps its axes sound", {
  frames <- axis_frames()

  for (cell in axis_matrix()) {
    frame <- frames[[cell$frame]]
    wide <- cell$route %in% c("wide", "jsdgam_mv")
    resp <- if (wide) c("zebra", "apple", "mango") else NULL

    if (identical(cell$expect, "refuse")) {
      # A refusal has to be the one this cell is about. Without a
      # pattern the cell passes on any error at all, including a
      # mistake in the test itself.
      expect_error(
        axis_prefit(frame, cell$trend, cell$route),
        regexp = cell$says, label = cell$label
      )
      next
    }

    sd <- withCallingHandlers(
      axis_prefit(frame, cell$trend, cell$route),
      warning = function(w) {
        if (any(vapply(axis_expected_warnings(), grepl,
                       logical(1L), conditionMessage(w)))) {
          invokeRestart("muffleWarning")
        }
      }
    )
    expect_axes_sound(sd, resp, cell$label, frame, cell$frame)
  }
})


test_that("no axis depends on the order the rows arrive in", {
  # An index built from row position rather than row content gives a
  # different answer for every permutation, and stays in range either
  # way, so only this comparison notices.
  frames <- axis_frames()
  set.seed(99L)

  wide_cells <- list(~ AR(p = 1), ~ RW(), ~ ZMVN(n_lv = 2),
                     ~ env + AR(p = 1))
  for (tf in wide_cells) {
    lab <- paste(deparse(tf), collapse = "")
    a <- axis_prefit(frames$wide, tf, "wide")$standata
    perm <- sample.int(nrow(frames$wide))
    b <- axis_prefit(frames$wide[perm, ], tf, "wide")$standata
    for (r in c("zebra", "apple", "mango")) {
      # The series arm is one repeated value on a response-keyed
      # frame, so comparing it across orderings cannot fail and is
      # kept only to pin that constancy. The time arm below is what
      # a row-position index would break.
      expect_length(unique(a[[paste0("obs_trend_series_", r)]]), 1L)
      expect_identical(
        as.integer(a[[paste0("obs_trend_time_", r)]])[perm],
        as.integer(b[[paste0("obs_trend_time_", r)]]),
        label = paste("wide", lab, r, "time")
      )
    }
  }

  # An observation keeps its own indices, so undoing the shuffle
  # restores the mapping exactly.
  for (nm in c("long", "hier_col", "unused")) {
    tf <- if (nm == "hier_col") {
      ~ AR(p = 1, gr = region, subgr = species)
    } else {
      ~ AR(p = 1)
    }
    d <- frames[[nm]]
    a <- axis_prefit(d, tf, "uni")$standata
    perm <- sample.int(nrow(d))
    b <- axis_prefit(d[perm, ], tf, "uni")$standata
    expect_identical(
      as.integer(a$obs_trend_time)[perm],
      as.integer(b$obs_trend_time), label = paste(nm, "time")
    )
    expect_identical(
      as.integer(a$obs_trend_series)[perm],
      as.integer(b$obs_trend_series), label = paste(nm, "series")
    )
    # One entry per series rather than per row, so a shuffle must
    # leave it untouched. Deriving it from the rows instead answers
    # with whatever order the frame arrived in, which on a frame
    # whose rows are already blocked by group happens to agree; only
    # a shuffled frame tells the two apart.
    expect_identical(
      as.integer(a$group_inds_trend),
      as.integer(b$group_inds_trend), label = paste(nm, "groups")
    )
    expect_identical(
      as.integer(a$N_subgroups_trend),
      as.integer(b$N_subgroups_trend), label = paste(nm, "subgroups")
    )
  }
})


test_that("the time index runs in the order the times do", {
  # A trend advances along this index, so numbering the times by the
  # order they first appear makes a recursion walk the timeline in
  # whatever order the frame was assembled.
  frames <- axis_frames()
  set.seed(101L)
  shuffled <- frames$long[sample.int(nrow(frames$long)), ]

  sd <- axis_prefit(shuffled, ~ AR(p = 1), "uni")$standata
  expect_identical(
    as.integer(sd$obs_trend_time),
    as.integer(match(shuffled$time, sort(unique(shuffled$time))))
  )
})


test_that("the loadings are sized by the series axis", {
  # Sizing either dimension of `Z` from a second reading of the axis
  # puts a series on another series' row.
  frames <- axis_frames()
  for (n_lv in 1:3) {
    tf <- eval(parse(text = paste0("~ ZMVN(n_lv = ", n_lv, ")")))
    sd <- axis_prefit(frames$wide, tf, "wide")$standata
    expect_identical(as.integer(sd$N_lv_trend), as.integer(n_lv))
    expect_identical(as.integer(sd$N_series_trend), 3L)
  }
})


test_that("irregular time distances agree with the time index", {
  # Time reaches Stan twice: as the integer index a recursion steps
  # along, and as the real gaps `CAR()` and the Gaussian processes
  # measure gaps with. Nothing else compares them, so an index built
  # in one order and distances in another would leave every gap
  # attached to the wrong step.
  set.seed(4L)
  times <- c(1, 2, 5, 6, 11, 12, 20)
  ids <- c("c_site", "a_site", "b_site")
  g <- expand.grid(time = times, series = ids,
                   stringsAsFactors = FALSE)
  d <- data.frame(
    time   = g$time,
    series = factor(g$series, levels = ids),
    env    = rnorm(nrow(g)),
    y      = rpois(nrow(g), 5)
  )
  # Shuffled, so an index taken from first appearance would disagree.
  d <- d[sample.int(nrow(d)), ]

  sd <- standata(
    mvgam_formula(y ~ env, trend_formula = ~ CAR()),
    data = d, family = poisson(), silent = 2L
  )

  sorted_times <- sort(unique(d$time))
  expect_identical(as.integer(sd$N_time_trend), length(sorted_times))
  expect_identical(
    dim(sd$time_dis),
    c(length(sorted_times), as.integer(sd$N_series_trend))
  )
  # Every series shares one time grid, so the columns are equal by
  # construction and one of them carries the claim. Step `t` holds
  # the gap from `t - 1`; the first step has no predecessor and
  # holds a placeholder rather than a distance.
  expect_equal(
    as.numeric(sd$time_dis[-1L, 1L]), diff(sorted_times),
    tolerance = 1e-8
  )
  expect_false(anyNA(sd$time_dis))
})





test_that("times_trend names the design row it claims to", {
  # `mu_trend[times_trend[i, s]]` is the only consumer of this map, so
  # entry (i, s) has to be the design row holding series s's covariate
  # at time i. Transposing the fill keeps the shape, keeps every index
  # in range and keeps the map a bijection onto the design, and hands
  # all but the diagonal another series' covariates.
  #
  # The series occupying each column is read from `obs_trend_series`,
  # the fit's own record, rather than from the stored levels, so the
  # check does not lean on a second account of the axis.
  frames <- axis_frames()
  for (nm in c("long", "hier_col")) {
    tf <- if (nm == "hier_col") {
      ~ env + AR(p = 1, gr = region, subgr = species)
    } else {
      ~ env + AR(p = 1)
    }
    frame <- frames[[nm]]
    prefit <- axis_prefit(frame, tf, "uni")
    sd <- prefit$standata

    labels <- as.character(mvgam:::get_series_for_grouping(
      mvgam:::prepare_mvgam_frame(prefit, frame)
    ))
    s_idx <- as.integer(sd$obs_trend_series)
    occupant <- vapply(
      seq_len(as.integer(sd$N_series_trend)),
      function(k) labels[which(s_idx == k)[1L]], character(1)
    )

    grid <- sort(unique(frame$time))
    key <- paste(frame$time, labels)
    env_col <- ncol(sd$X_trend)
    for (s in seq_along(occupant)) {
      expect_equal(
        as.numeric(sd$X_trend[sd$times_trend[, s], env_col]),
        as.numeric(frame$env[match(paste(grid, occupant[s]), key)]),
        tolerance = 1e-9,
        label = paste(nm, occupant[s], "reads its own covariates")
      )
    }
  }
})
