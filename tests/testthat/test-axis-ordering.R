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

# A column named by the record and present in the frame.
named_col <- function(v, frame) {
  !is.null(v) && !is.na(v) && !identical(v, "NA") && v %in% names(frame)
}

# The order a grouping column declares its groups in, counting only
# the ones something is observed at. A factor declares them; a
# character column has only alphabetical order to declare. Stan
# numbers the groups this way, and `sigma_group_trend[g]` is read
# back under that number, so it is a contract rather than an
# internal detail.
frame_group_levels <- function(frame, gr_var) {
  levels(droplevels(as.factor(frame[[gr_var]])))
}

# Which series each row of a frame names, read from the user's own
# columns and from nothing the package derives. Every ground truth
# below that needs a per-row series takes it from here: reading it
# through `get_series_for_grouping()` would put the same derivation
# on both sides of the comparison, and a derivation wrong in one
# direction would then satisfy an assertion written to catch it.
#
# A grouping supersedes a series column, and joins its two names
# with an underscore, which is the whole of the hierarchical
# spelling. A frame carrying neither names one series, where any
# labelling is trivially the right one.
frame_row_series <- function(frame, prefit) {
  vars <- prefit$trend_metadata$variables
  if (named_col(vars$gr_var, frame) &&
        named_col(vars$subgr_var, frame)) {
    return(paste(
      as.character(frame[[vars$gr_var]]),
      as.character(frame[[vars$subgr_var]]),
      sep = "_"
    ))
  }
  series_var <- vars$series_var %||% "series"
  if (series_var %in% names(frame)) {
    return(as.character(frame[[series_var]]))
  }
  rep(
    as.character(prefit$trend_metadata$axes$series$levels)[1L],
    nrow(frame)
  )
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
  # Region runs west, north, south and species sp_c, sp_a, sp_b, as
  # the frame declares them, so neither half of a label is in
  # alphabetical order and a grouping rebuilt from sorted values
  # differs from this in both.
  hier3 <- c(
    "west_sp_c", "west_sp_a", "west_sp_b",
    "north_sp_c", "north_sp_a", "north_sp_b",
    "south_sp_c", "south_sp_a", "south_sp_b"
  )
  switch(
    name,
    hier3 = hier3,
    long = c("a_site", "c_site", "b_site"),
    ragged = c("a_site", "c_site", "b_site"),
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

# How a frame's axis was arrived at, written out per frame rather
# than checked against a list of the strings the package can emit.
# A whitelist passes a fit that decided a hierarchical frame was
# explicit, which is the exact confusion that gave one model two
# series axes, so the cell states which answer is the right one.
frame_axis_source <- function(name) {
  switch(
    name,
    long = "explicit",
    unused = "explicit",
    char_series = "explicit",
    uni = "explicit",
    unbal = "explicit",
    ragged = "explicit",
    wide_col = "explicit",
    hier = "hierarchical",
    hier_col = "hierarchical",
    hier3 = "hierarchical",
    # The responses are the series.
    wide = "multivariate",
    wide_na = "multivariate",
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
    "trend_map matrix / unused" = list(
      a_site = c(1, 0), c_site = c(0, 1), b_site = c(1, 0)
    ),
    NULL
  )
}

# How the frame's series axis was arrived at, as the fit recorded it.
# The rows a fit was actually given. brms drops a row whose
# response is missing, so the arrays Stan reads are one entry per
# observation and not one per row of the frame. Every ground truth
# compared against those arrays has to be subset the same way.
frame_observed <- function(frame, resp_names) {
  observed <- rep(TRUE, nrow(frame))
  for (r in resp_names %||% "y") {
    if (!is.null(frame[[r]])) observed <- observed & !is.na(frame[[r]])
  }
  observed
}

# The column a frame names its series in, or `NULL` where the
# responses are the series and there is none.
#
# A `gr` / `subgr` pair supersedes any series column the frame also
# carries, so the grouping is what names them. Every check that has
# to reach the naming column asks here, because getting it wrong
# reverses the meaning of the test: a stranger planted in the
# superseded column checks that the fit ignores what it should
# ignore, not that it refuses what it should refuse.
axis_naming_column <- function(frame, prefit) {
  vars <- prefit$trend_metadata$variables
  if (named_col(vars$gr_var, frame) &&
        named_col(vars$subgr_var, frame)) {
    return(vars$subgr_var)
  }
  series_var <- vars$series_var %||% "series"
  if (named_col(series_var, frame)) series_var else NULL
}

# `frame` with a series the model never fitted planted in it, or
# `NULL` where the frame names its series through its responses.
plant_stranger <- function(frame, prefit) {
  target <- axis_naming_column(frame, prefit)
  if (is.null(target)) {
    return(NULL)
  }
  frame[[target]] <- as.character(frame[[target]])
  frame[[target]][1L] <- "never_fitted"
  frame
}

# What the package says when a frame names a series it never had.
# One condition, one message, and one place the tests spell it.
STRANGER_REFUSAL <- "not found in training data"

# When each series was last observed, from the user's own frame.
# Not the last row it has: mvgam asks a panel whose series end at
# different times to be padded with `NA`, so a padded series has
# rows past its own end. Reading the rows dates it from the
# padding, which is the defect this answer exists to catch, so both
# sides of that comparison cannot be allowed to make it.
frame_last_times <- function(frame, prefit, resp_names, levels_expected) {
  row_time <- as.numeric(frame[[time_col(prefit)]])
  if (identical(frame_axis_source_of(prefit, frame), "multivariate")) {
    return(vapply(resp_names,
                  function(r) max(row_time[!is.na(frame[[r]])]),
                  numeric(1L), USE.NAMES = FALSE))
  }
  row_series <- frame_row_series(frame, prefit)
  observed <- frame_observed(frame, resp_names)
  vapply(as.character(levels_expected), function(lv) {
    max(row_time[row_series == lv & observed])
  }, numeric(1L), USE.NAMES = FALSE)
}

# Whether the responses are the series. Read from the record
# rather than from `trend_metadata$series_source`, which is a
# second account of the same fact. The record's value is checked
# against the frame's own in `expect_axes_sound()`, so branching on
# it here leans on something already pinned.
frame_axis_source_of <- function(prefit, frame) {
  prefit$trend_metadata$axes$series$source
}

# Bounds, completeness and the matrix shapes, for one emitted program.
expect_axes_sound <- function(prefit, resp_names, lab, frame,
                              frame_name) {
  sd <- prefit$standata
  meta <- prefit$trend_metadata
  n_time <- as.integer(sd$N_time_trend)
  n_series <- as.integer(sd$N_series_trend)

  # The record post-fit reads. `predict()`, `forecast()`, `summary()`
  # and the plots no longer derive an axis of their own, they read
  # this, so whatever is wrong here is wrong in all of them. Checking
  # it on a prefit is what lets this file speak for them without
  # sampling a model.
  axes <- meta$axes
  expect_false(is.null(axes), label = paste(lab, "records its axes"))
  if (!is.null(axes)) {
    expect_identical(
      as.integer(axes$series$n), n_series,
      label = paste(lab, "recorded series count is Stan's")
    )
    expect_identical(
      length(axes$series$levels), n_series,
      label = paste(lab, "recorded levels span the axis")
    )
    expect_identical(
      as.integer(axes$time$n), n_time,
      label = paste(lab, "recorded time count is Stan's")
    )
    wanted_source <- frame_axis_source(frame_name)
    if (!is.null(wanted_source)) {
      expect_identical(
        axes$series$source, wanted_source,
        label = paste(lab, "records how the axis was arrived at")
      )
    }
    # `group_inds_trend` is built from these groups, so the record and
    # the array Stan is handed have to name the same grouping.
    gr_var <- meta$variables$gr_var
    if (!is.null(sd$group_inds_trend) && named_col(gr_var, frame)) {
      # Group `g` is the `g`th level the user's own column declares,
      # counting only the levels something is observed at. That is a
      # contract a user can rely on when they read
      # `sigma_group_trend[2]`, and it is taken from the frame's
      # declaration rather than from any package derivation.
      # Asserting only that the two induce the same partition would
      # pass any consistent relabelling, which is exactly what would
      # print one group's correlation under another's name.
      expect_identical(
        match(axes$series$groups, frame_group_levels(frame, gr_var)),
        as.integer(sd$group_inds_trend),
        label = paste(lab, "recorded groups are group_inds_trend")
      )
    }
    # Every label a user sees after fitting is read from this list.
    recorded_expect <- frame_axis_labels(frame_name) %||% resp_names
    if (!is.null(recorded_expect)) {
      expect_identical(
        as.character(axes$series$levels), as.character(recorded_expect),
        label = paste(lab, "recorded levels are the user's own")
      )
    }
    # The times the user supplied, not their ranks. The wide frame
    # starts at three precisely so the two differ; taking the index
    # for the values would give a forecast the wrong horizon and
    # `CAR()` the wrong gaps.
    expect_identical(
      as.numeric(axes$time$values),
      as.numeric(sort(unique(frame[[time_col(prefit)]]))),
      label = paste(lab, "recorded times are the user's own")
    )
    expect_identical(
      as.integer(axes$factor$n_lv), as.integer(sd$N_lv_trend),
      label = paste(lab, "recorded factor count is Stan's")
    )
    # `CAR()` forecasts each series forward from its own last
    # observation, so these are in axis order and are the times the
    # frame holds, not their ranks.
    expect_identical(
      length(axes$series$last_time), n_series,
      label = paste(lab, "records a last time per series")
    )
    # A series ends when its own observations stop, whether it is a
    # response column or a stretch of a stacked frame. `wide_na`
    # and `ragged` are built so the entries differ, because a frame
    # whose series end together cannot tell a permuted record from
    # the right one.
    expect_identical(
      as.numeric(axes$series$last_time),
      frame_last_times(frame, prefit, resp_names, axes$series$levels),
      label = paste(lab, "last times are each series' own")
    )
    # Named, never left at a default: a record that reaches a reader
    # before the grain is known says nothing rather than saying
    # "series" and being believed.
    expect_true(
      identical(axes$grain, "series") || identical(axes$grain, "lv"),
      label = paste(lab, "records the grain of its design")
    )
    expect_identical(
      axes$grain, if (isTRUE(meta$has_by_lv)) "lv" else "series",
      label = paste(lab, "grain follows the trend's own design")
    )
    # The columns that place a row on these axes. Post-fit reads them
    # to identify a frame the model has never seen, so a missing one
    # is a frame that cannot be mapped.
    expect_identical(
      axes$vars$time_var, time_col(prefit),
      label = paste(lab, "records the time column")
    )
  }

  # `times_trend[i, k]` is indexed by whatever grain the trend design
  # runs on. Ordinarily that is the series. Under `by = lv_axis()` it
  # is the factor: Stan declares the map `[N_time_trend, N_lv_trend]`,
  # folds `mu_factor` into `lv_trend`, and lets `Z` carry it to the
  # series, so the design has one row per (time, factor). Assuming the
  # series grain would fail a correct model, so the grain is read from
  # the fit.
  grain <- if (identical(axes$grain, "lv")) {
    as.integer(sd$N_lv_trend)
  } else {
    n_series
  }
  expect_identical(
    dim(sd$times_trend), c(n_time, grain),
    label = paste(lab, "times_trend shape")
  )
  # How many design rows there are is not `n_time * grain`. A
  # response-keyed frame holds one covariate value per time, so its
  # trend design is one row per time and every column of the map
  # points at the same rows. The size is pinned by the bijection
  # asserted below rather than by a formula that holds for only one
  # of the three shapes.
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
    if (named_col(gr_var, frame) && !is.null(sd$obs_trend_series)) {
      row_series <- as.integer(sd$obs_trend_series)
      row_group <- as.character(frame[[gr_var]])
      group_of_column <- vapply(seq_len(n_series), function(k) {
        row_group[which(row_series == k)[1L]]
      }, character(1))
      expect_identical(
        gi, match(group_of_column, frame_group_levels(frame, gr_var)),
        label = paste(lab, "group_inds names each column's group")
      )
    }
  }

  # Which series occupies each column of the trend matrix. This is
  # the claim the whole file exists for and nothing else states it
  # directly: every row Stan sends to column `k` has to name the
  # same series, and that series has to be the one the record puts
  # at position `k`. A permuted axis keeps every index in range and
  # keeps every column occupied, so counting and bounds say nothing;
  # only the occupancy does. The labels come from the frame's own
  # columns and the order comes from the record, so the two sides
  # are not two readings of one derivation.
  if (is.null(resp_names) && !is.null(sd$obs_trend_series) &&
        !is.null(axes)) {
    observed <- frame_observed(frame, resp_names)
    row_series <- frame_row_series(frame, prefit)[observed]
    s_idx <- as.integer(sd$obs_trend_series)
    expect_identical(
      length(s_idx), sum(observed),
      label = paste(lab, "one trend cell per observation")
    )
    occupant <- vapply(seq_len(n_series), function(k) {
      held <- unique(row_series[s_idx == k])
      # `NA` where a column holds rows from more than one series,
      # which fails against any label the record can name.
      if (length(held) == 1L) held else NA_character_
    }, character(1))
    expect_identical(
      occupant, as.character(axes$series$levels),
      label = paste(lab, "each column holds its recorded series")
    )
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
      if (identical(frame_axis_source_of(prefit, frame),
                    "multivariate")) {
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
    labels <- frame_row_series(frame, prefit)[
      frame_observed(frame, resp_names)
    ]
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


# What post-processing answers, driven on a prefit. None of these
# needs draws: they resolve labels, counts and row identities, which
# is the structural half of every summary, plot, prediction and
# forecast. Asserting the record alone would say only that a field
# holds a value; these say the functions a user's results flow
# through give the right answer, which is the claim that matters.
#
# Ground truth is the frame and the emitted Stan data, never the
# record, so a wrong record cannot satisfy both sides of a check.
expect_postfit_sound <- function(prefit, resp_names, lab, frame,
                                 frame_name) {
  sd <- prefit$standata
  n_series <- as.integer(sd$N_series_trend)
  n_time <- as.integer(sd$N_time_trend)
  expected <- frame_axis_labels(frame_name) %||% resp_names

  # The accessor, not the field. Every post-fit reader asks through
  # `mvgam_axes()`, which also understands the older spelling a fit
  # saved before the record carries, so reading
  # `trend_metadata$axes` directly would leave the one function they
  # all share untested.
  acc <- mvgam:::mvgam_axes(prefit)
  expect_identical(
    as.character(acc$series$levels), as.character(expected),
    label = paste(lab, "the accessor answers with the axis")
  )
  expect_identical(
    as.integer(acc$series$n), n_series,
    label = paste(lab, "the accessor counts the axis")
  )

  # What `print()` and `summary()` put in front of a user.
  counts <- mvgam:::printed_axis_counts(prefit)
  expect_identical(
    as.integer(counts$n_series), n_series,
    label = paste(lab, "the printed series count is Stan's")
  )
  expect_identical(
    as.integer(counts$n_timepoints), n_time,
    label = paste(lab, "the printed time count is Stan's")
  )

  # The labels every summary, plot and forecast prints.
  expect_identical(
    as.character(mvgam:::resolve_series_info(prefit)$series_levels),
    as.character(expected),
    label = paste(lab, "forecast surface names the user's series")
  )

  # The count every loadings plot and factor surface sizes itself by.
  expect_identical(
    as.integer(mvgam:::loading_series_count(prefit)), n_series,
    label = paste(lab, "loadings count the series Stan was given")
  )
  detected <- mvgam:::detect_factor_n_lv(prefit)
  expect_identical(
    as.integer(detected %||% n_series), as.integer(sd$N_lv_trend),
    label = paste(lab, "factor count matches Stan's")
  )

  # The structure every conditional prediction is built on. Where the
  # responses are the series a row names no response, so the structure
  # is asked for one response at a time and every row reads that
  # response's column.
  keyed_by_response <- identical(
    frame_axis_source_of(prefit, frame), "multivariate"
  )
  if (keyed_by_response) {
    for (r in expected) {
      os_r <- mvgam:::get_observation_structure(prefit, resp = r)
      expect_true(
        all(os_r$series_int == match(r, expected)),
        label = paste(lab, "each response reads its own trend column")
      )
    }
    os <- os_r
  } else {
    os <- mvgam:::get_observation_structure(prefit)
  }
  expect_identical(
    as.character(os$series_levels), as.character(expected),
    label = paste(lab, "observation structure names the axis")
  )
  expect_identical(
    as.integer(os$n_series), n_series,
    label = paste(lab, "observation structure counts the axis")
  )
  expect_identical(
    as.integer(length(os$unique_times)), n_time,
    label = paste(lab, "observation structure spans the time grid")
  )

  # Where `CAR()` forecasts each series from.
  last_times <- mvgam:::extract_last_observed_times(prefit, n_series)
  expect_identical(
    length(last_times), n_series,
    label = paste(lab, "a last time per series")
  )
  # The same fact the record was checked on, asked of the function
  # `CAR()` forecasting actually calls. One helper answers it, so a
  # ground truth wrong in the same direction as the code cannot
  # satisfy both sides.
  expect_identical(
    as.numeric(last_times),
    frame_last_times(frame, prefit, resp_names, expected),
    label = paste(lab, "last times are read off the frame")
  )

  if (!keyed_by_response) {
    row_series <- frame_row_series(frame, prefit)
    # Which series a row belongs to, on the axis's own levels.
    ids <- mvgam:::axis_row_series(prefit, frame)  # nolint
    expect_identical(
      levels(ids), as.character(expected),
      label = paste(lab, "row identity is levelled on the axis")
    )
    expect_identical(
      as.integer(ids), as.integer(match(row_series, expected)),
      label = paste(lab, "each row is placed on its own series")
    )
  }

  # The training frame is a frame the fit has seen, so it passes.
  expect_true(
    mvgam:::validate_prediction_factor_levels(
      frame, prefit$trend_metadata
    ),
    label = paste(lab, "training data validates against itself")
  )
}

# The refusals post-processing owes a user, driven on a prefit. A
# frame naming a series the fit never saw has no latent state to
# read, so it is turned away rather than mapped onto some other
# series' column.
expect_postfit_refuses <- function(prefit, lab, frame, frame_name) {
  # A response-keyed frame has no column to plant a stranger in:
  # its series are the responses, and a response the fit never had
  # is a different formula rather than a different frame. Saying so
  # here is the point -- a helper that returns quietly registers no
  # expectation and reads in the output exactly like one that
  # passed.
  stranger <- plant_stranger(frame, prefit)
  if (is.null(stranger)) {
    expect_identical(
      frame_axis_source_of(prefit, frame), "multivariate",
      label = paste(lab, "names its series through its responses")
    )
    return(invisible(NULL))
  }
  # Pinned to the one message that owns this condition. `series` and
  # `level` appear in most of the package's errors, including the
  # one raised when a grouping column is absent, so a looser pattern
  # would pass a refusal for the wrong reason.
  expect_error(
    mvgam:::validate_prediction_factor_levels(
      stranger, prefit$trend_metadata
    ),
    regexp = STRANGER_REFUSAL,
    label = paste(lab, "refuses a series it never saw")
  )
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
  # and a swapped pair shows up in the counts alone. Mango's are the
  # last four rows, so it stops being observed before the other two
  # do: a frame where every response ends together cannot tell a
  # permuted last-observed time from the right one, because every
  # entry holds the same value.
  wide_na$zebra[c(3L, 4L, 11L)] <- NA_integer_
  wide_na$apple[c(7L, 15L)] <- NA_integer_
  wide_na$mango[c(2L, 21L, 22L, 23L)] <- NA_real_

  # Three groups of three, with the region and species levels
  # declared out of alphabetical order. Two-by-two cannot separate a
  # swap inside a group from a swap between groups, and with two
  # groups `group_inds_trend` reads the same forwards and backwards
  # under some permutations. Nine series over three groups tells
  # those apart, and gives `N_subgroups_trend` a value that is
  # neither the series count nor the group count.
  hier3_grid <- expand.grid(
    time = seq_len(n_t), species = c("sp_c", "sp_a", "sp_b"),
    region = c("west", "north", "south"), stringsAsFactors = FALSE
  )
  hier3 <- data.frame(
    time    = hier3_grid$time,
    region  = factor(hier3_grid$region,
                     levels = c("west", "north", "south")),
    species = factor(hier3_grid$species,
                     levels = c("sp_c", "sp_a", "sp_b")),
    env     = rnorm(nrow(hier3_grid)),
    y       = rpois(nrow(hier3_grid), 5)
  )
  # Three species in one region and two in the other, which the
  # per-group blocks cannot size.
  hier_unbal <- hier3[!(hier3$region == "south" &
                          hier3$species == "sp_b"), ]
  hier_unbal <- hier_unbal[hier_unbal$region != "west", ]

  # The shape the unbalanced-panel refusal asks users to supply:
  # one shared time grid, with the series that stop early padded to
  # the end by `NA` responses. Every series has a row at every time
  # and only some have an observation there, which is what tells a
  # last row from a last observation.
  ragged <- long
  ragged$y[ragged$series == "b_site" & ragged$time > 12L] <- NA_integer_
  ragged$y[ragged$series == "a_site" & ragged$time > 14L] <- NA_integer_

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
    hier3 = hier3, hier_unbal = hier_unbal, ragged = ragged,
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

# The multivariate formula the wide frames are read with. Each arm
# carries an intercept alone, for the reason `axis_prefit()` gives:
# several cells put `env` on the trend side, and the same covariate on
# both sides of one arm is a pairing the likelihood cannot separate.
axis_wide_formula <- function() {
  bf(zebra ~ 1, family = poisson()) +
    bf(apple ~ 1, family = bernoulli()) +
    bf(mango ~ 1, family = gaussian()) +
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
#
# The observation side carries an intercept and nothing else. Several
# cells put `env` on the trend side, and the same covariate on both
# sides is a pairing the likelihood cannot separate, so writing it
# here would have every one of those cells build a model no user
# should be shown. Nothing in this file reads the observation design.
axis_prefit <- function(frame, spec, route) {
  switch(
    route,
    uni = mvgam(
      y ~ 1, trend_formula = spec, data = frame,
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
    list("explicit / RW", "long", ~ RW(), "sound", "uni"),
    list("explicit / AR1", "long", ~ AR(p = 1), "sound", "uni"),
    list("explicit / CAR", "long", ~ CAR(), "sound", "uni"),
    # Series that stop being observed at different times. Every
    # other frame has its series end together, so a last time read
    # off the rows rather than off the observations agrees with the
    # right answer everywhere else.
    list("ragged / AR1", "ragged", ~ AR(p = 1), "sound", "uni"),
    list("ragged / CAR", "ragged", ~ CAR(), "sound", "uni"),
    list("explicit / factor", "long", ~ AR(p = 1, n_lv = 2),
         "sound", "uni"),
    list("explicit / covariate", "long", ~ env + AR(p = 1),
         "sound", "uni"),

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
    # Three groups of three. The axis, the group each column sits in
    # and the subgroup width are all values a two-by-two frame
    # cannot separate from a permutation of themselves.
    list("hier3 / AR1", "hier3",
         ~ AR(p = 1, gr = region, subgr = species), "sound", "uni"),
    list("hier3 / cor", "hier3",
         ~ AR(p = 1, cor = TRUE, gr = region, subgr = species),
         "sound", "uni"),
    list("hier3 / covariate", "hier3",
         ~ env + AR(p = 1, gr = region, subgr = species),
         "sound", "uni"),
    # Groups of different sizes share one block size in Stan, so the
    # design is refused rather than silently sized by the largest.
    list("hier unbalanced", "hier_unbal",
         ~ AR(p = 1, gr = region, subgr = species), "refuse", "uni",
         "equal"),

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
    # A frame whose series column declares a level nothing observes.
    # The normaliser read the declaration while the axis reads what
    # is observed, so a three-series model was told its three-row
    # map had the wrong number of rows.
    list("trend_map matrix / unused", "unused",
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

    # A grouping names the series whatever the trend does with them,
    # so the axis has to hold across the trends that reach it by
    # different routes. `CAR()` takes no grouping at all, which is
    # pinned so the day it does is noticed.
    list("hier / VAR", "hier_col",
         ~ VAR(gr = region, subgr = species), "sound", "uni"),
    list("hier / ZMVN", "hier_col",
         ~ ZMVN(gr = region, subgr = species), "sound", "uni"),
    list("hier / RW cor", "hier_col",
         ~ RW(cor = TRUE, gr = region, subgr = species), "sound", "uni"),
    list("hier / CAR", "hier_col",
         ~ CAR(gr = region, subgr = species), "refuse", "uni",
         "unused argument"),

    # `by = lv_axis()` moves the trend design onto the factor axis,
    # so the map Stan is given is one column per factor rather than
    # one per series. The cell exists to pin that second grain,
    # which no other cell exercises.
    list("by lv / AR factor", "long",
         ~ s(env, by = lv_axis()) + AR(p = 1, n_lv = 2), "sound",
         "uni"),
    # `CAR()` never reaches that second grain, and the two cells
    # below say why rather than leaving the combination untried.
    # `time_dis` is declared `[N_time_trend, N_series_trend]` and
    # then read as `time_dis[i, j]` against `lv_trend`'s columns, so
    # the array is sized on one axis and subscripted on another.
    # Nothing has to reconcile the two, because a multivariate
    # `CAR()` takes no trend covariate and `by = lv_axis()` is one,
    # and because `CAR()` takes no `n_lv` at all. Pinned so the day
    # either restriction lifts is the day the grains have to be told
    # apart.
    list("by lv / CAR", "long",
         ~ s(env, by = lv_axis()) + CAR(), "refuse", "uni",
         "trend covariates"),
    list("by lv / CAR factor", "long",
         ~ s(env, by = lv_axis()) + CAR(n_lv = 2), "refuse", "uni",
         "unused argument"),

    list("jsdgam species / 1", "long", 1L, "sound", "jsdgam_species"),
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
    expect_postfit_sound(sd, resp, cell$label, frame, cell$frame)
    expect_postfit_refuses(sd, cell$label, frame, cell$frame)
  }
})


# Frames a user might hand a fitted model, well formed and not.
# Placing a row on the fit's axes is a structural question, so none
# of this needs draws: a prefit knows its axes and that is the whole
# of what the answer depends on.
newdata_variants <- function(frame, prefit) {
  vars <- prefit$trend_metadata$variables
  time_var <- vars$time_var %||% "time"
  last <- max(frame[[time_var]])

  future <- frame[frame[[time_var]] == last, , drop = FALSE]
  future[[time_var]] <- last + 1L

  shuffled <- frame[rev(seq_len(nrow(frame))), , drop = FALSE]

  # One series only, chosen on the identity the fit reads rather
  # than on any single column, so a hierarchical frame is subset by
  # the series it actually has.
  row_series <- frame_row_series(frame, prefit)
  one_series <- frame[row_series == row_series[1L], , drop = FALSE]

  no_time <- frame[, setdiff(names(frame), time_var), drop = FALSE]
  na_time <- frame
  na_time[[time_var]][1L] <- NA

  good <- list(
    training = frame, future = future, shuffled = shuffled,
    "one series" = one_series
  )
  bad <- list(
    "no time column" = list(
      frame = no_time, message = "[Mm]ust include"
    ),
    "a missing time" = list(
      frame = na_time, message = "missing values"
    )
  )

  stranger <- plant_stranger(frame, prefit)
  if (!is.null(stranger)) {
    bad[["a series never fitted"]] <- list(
      frame = stranger, message = STRANGER_REFUSAL
    )
  }
  list(good = good, bad = bad)
}


test_that("newdata is placed on the fit's axes, or refused", {
  # Every post-fit surface begins by asking where a frame's rows sit
  # on the axes the model was built on. That question needs no
  # draws, so a prefit answers it, and a frame that cannot be placed
  # has to be turned away rather than mapped onto some other series'
  # column.
  frames <- axis_frames()
  hier_tf <- ~ AR(p = 1, gr = region, subgr = species)
  # `unbal` is absent by design: an unbalanced panel is refused at
  # fitting, so there is no fit of it to hand a frame to.
  for (nm in c("long", "hier", "hier_col", "char_series", "unused")) {
    tf <- if (nm %in% c("hier", "hier_col")) hier_tf else ~ AR(p = 1)
    frame <- frames[[nm]]
    prefit <- axis_prefit(frame, tf, "uni")
    axis <- as.character(prefit$trend_metadata$axes$series$levels)
    variants <- newdata_variants(frame, prefit)

    for (vn in names(variants$good)) {
      nd <- variants$good[[vn]]
      ids <- mvgam:::axis_row_series(prefit, nd)
      expect_identical(
        levels(ids), axis,
        label = paste(nm, vn, "is read on the fit's own axis")
      )
      expect_false(
        anyNA(ids),
        label = paste(nm, vn, "places every row on a series")
      )
      # The series a row names, read from the frame's own columns.
      # A subset frame still names the whole axis, so the levels
      # above and the values here answer different questions.
      expect_identical(
        as.integer(ids),
        as.integer(match(frame_row_series(nd, prefit), axis)),
        label = paste(nm, vn, "each row keeps its own series")
      )
    }
    # Placing frames does not renumber the axis. Asserted once, on
    # the fit: `resolve_series_info()` reads only the object, so
    # repeating it per variant would run the same expectation four
    # times and say nothing about the frames.
    expect_identical(
      as.character(mvgam:::resolve_series_info(prefit)$series_levels),
      axis,
      label = paste(nm, "leaves the axis where it was")
    )

    for (vn in names(variants$bad)) {
      # Pinned to the message, so a mistake in the test itself
      # cannot pass as the refusal it was written to check.
      expect_error(
        mvgam:::prepare_mvgam_frame(prefit, variants$bad[[vn]]$frame),
        regexp = variants$bad[[vn]]$message,
        label = paste(nm, vn, "is refused")
      )
    }
  }
})


# The frame a user hands `forecast()`, and the occasions each series
# is thereby asking for.
#
# A stacked frame can ask each series for a different horizon,
# because a row belongs to one series, and ragged horizons are what
# tell a permuted grid from the right one: give every series the
# same three occasions and swapping two arms changes nothing
# anything can see. A response-keyed frame cannot be ragged, since
# every row carries every response, so there the horizon is shared
# and the claim is that each response is given it rather than none.
forecast_request <- function(frame, prefit, levels_expected) {
  time_var <- time_col(prefit)
  last <- max(frame[[time_var]])
  last_rows <- frame[frame[[time_var]] == last, , drop = FALSE]

  if (identical(frame_axis_source_of(prefit, frame), "multivariate")) {
    horizon <- last + 1:3
    newdata <- do.call(rbind, lapply(horizon, function(tt) {
      row <- last_rows
      row[[time_var]] <- tt
      row
    }))
    return(list(
      newdata = newdata,
      wanted = stats::setNames(
        rep(list(horizon), length(levels_expected)), levels_expected
      )
    ))
  }

  last_series <- frame_row_series(last_rows, prefit)
  wanted <- stats::setNames(
    lapply(seq_along(levels_expected), function(i) last + seq_len(i)),
    levels_expected
  )
  pieces <- unlist(lapply(levels_expected, function(lv) {
    base_row <- last_rows[last_series == lv, , drop = FALSE]
    lapply(wanted[[lv]], function(tt) {
      base_row[[time_var]] <- tt
      base_row
    })
  }), recursive = FALSE)
  list(newdata = do.call(rbind, pieces), wanted = wanted)
}

test_that("a frame the fit has never seen lands on the right cells", {
  # The whole point of recording the axes: given the record and a
  # frame, every row can be placed on a trend cell without going back
  # to the training data. `resolve_forecast_grid()` is what every
  # forecast goes through and it needs no draws, so the claim is
  # testable here rather than only after sampling.
  #
  # Every route the package offers is asked, because the routes
  # differ in exactly the thing being checked: `hier` names its
  # series through a grouping and carries no series column, and the
  # wide and `jsdgam()` routes name them through the responses,
  # where a row belongs to all of them at once. Asking only the
  # stacked routes is how every `mvbf()` fit came to return an empty
  # horizon without complaint.
  frames <- axis_frames()
  hier_tf <- ~ AR(p = 1, gr = region, subgr = species)
  cells <- list(
    list(nm = "long", spec = ~ AR(p = 1), route = "uni"),
    list(nm = "hier", spec = hier_tf, route = "uni"),
    list(nm = "hier_col", spec = hier_tf, route = "uni"),
    list(nm = "unused", spec = ~ AR(p = 1), route = "uni"),
    list(nm = "char_series", spec = ~ AR(p = 1), route = "uni"),
    list(nm = "wide", spec = ~ AR(p = 1), route = "wide"),
    list(nm = "wide_na", spec = ~ AR(p = 1), route = "wide"),
    list(nm = "long", spec = 2L, route = "jsdgam_species"),
    list(nm = "wide", spec = 2L, route = "jsdgam_mv")
  )

  for (cell in cells) {
    nm <- cell$nm
    lab <- paste(nm, cell$route)
    frame <- frames[[nm]]
    prefit <- withCallingHandlers(
      axis_prefit(frame, cell$spec, cell$route),
      warning = function(w) {
        if (any(vapply(axis_expected_warnings(), grepl,
                       logical(1L), conditionMessage(w)))) {
          invokeRestart("muffleWarning")
        }
      }
    )
    levels_expected <- frame_axis_labels(nm) %||%
      c("zebra", "apple", "mango")
    training <- mvgam:::build_training_arms(prefit, levels_expected)
    request <- forecast_request(frame, prefit, levels_expected)

    grid <- mvgam:::resolve_forecast_grid(
      prefit, request$newdata, training, levels_expected
    )
    expect_false(is.null(grid), label = paste(lab, "grid resolves"))

    # Each series is asked for the occasions it was given, and no
    # others. A row placed on another series' arm shows here as a
    # horizon of the wrong length or the wrong times, and a series
    # left out shows as an empty one.
    for (lv in levels_expected) {
      expect_identical(
        as.numeric(grid$times[[lv]]), as.numeric(request$wanted[[lv]]),
        label = paste(lab, lv, "is forecast at the times asked for")
      )
    }

    # A frame naming a series the model never fitted has nowhere to
    # put those rows, so it is turned away rather than folded onto
    # whichever column happens to be first. Where the responses are
    # the series there is no column to name a stranger in: a
    # response the fit never had is a different formula, not a
    # different frame, and saying so is what stops this reading as
    # a cell that quietly checked nothing.
    stranger <- plant_stranger(request$newdata, prefit)
    if (is.null(stranger)) {
      expect_identical(
        frame_axis_source_of(prefit, frame), "multivariate",
        label = paste(lab, "names its series through its responses")
      )
    } else {
      expect_error(
        mvgam:::resolve_forecast_grid(
          prefit, stranger, training, levels_expected
        ),
        regexp = "not found in training data",
        label = paste(lab, "a stranger is refused")
      )
    }
  }
})



test_that("one frame gives one axis, whichever route reads it", {
  # `mvgam()` names its series in a column and `jsdgam()` names the
  # same column as its species. They are two doors into one model, so
  # a frame put through both has to come out on the same axis. Two
  # entry points disagreeing about one axis is the defect this file
  # exists for, and nothing else here compares a route against
  # another route rather than against the frame.
  frames <- axis_frames()
  uni <- axis_prefit(frames$long, ~ AR(p = 1, n_lv = 2), "uni")
  jsd <- axis_prefit(frames$long, 2L, "jsdgam_species")

  expect_identical(
    uni$trend_metadata$axes$series$levels,
    jsd$trend_metadata$axes$series$levels,
    label = "mvgam and jsdgam name the same series"
  )
  expect_identical(
    as.integer(uni$standata$N_series_trend),
    as.integer(jsd$standata$N_series_trend),
    label = "mvgam and jsdgam count the same series"
  )
  expect_identical(
    as.integer(uni$standata$obs_trend_series),
    as.integer(jsd$standata$obs_trend_series),
    label = "mvgam and jsdgam put each row on the same series"
  )
  expect_identical(
    as.integer(uni$standata$obs_trend_time),
    as.integer(jsd$standata$obs_trend_time),
    label = "mvgam and jsdgam put each row at the same time"
  )
  expect_identical(
    as.numeric(uni$trend_metadata$axes$time$values),
    as.numeric(jsd$trend_metadata$axes$time$values),
    label = "mvgam and jsdgam share one time grid"
  )
})


test_that("a level nothing observes changes nothing", {
  # `unused` is `long` with one more declared level that no row
  # holds. A series the data never observes has no latent state to
  # estimate, so it must not reach the axis, must not widen the
  # loadings and must not shift any other series' column. Taking a
  # factor's declared levels rather than its observed ones is what
  # made the stored levels outnumber the trend columns.
  frames <- axis_frames()
  for (tf in list(~ AR(p = 1), ~ AR(p = 1, n_lv = 2), ~ CAR())) {
    lab <- paste(deparse(tf), collapse = "")
    a <- axis_prefit(frames$long, tf, "uni")
    b <- axis_prefit(frames$unused, tf, "uni")
    expect_identical(
      a$trend_metadata$axes$series,
      b$trend_metadata$axes$series,
      label = paste(lab, "the ghost level is not on the axis")
    )
    expect_identical(
      as.integer(a$standata$obs_trend_series),
      as.integer(b$standata$obs_trend_series),
      label = paste(lab, "no row moves because of it")
    )
    expect_identical(
      as.integer(a$standata$N_lv_trend),
      as.integer(b$standata$N_lv_trend),
      label = paste(lab, "the loadings do not widen")
    )
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

    labels <- frame_row_series(frame, prefit)
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
