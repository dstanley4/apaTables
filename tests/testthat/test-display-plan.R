test_that("apa_display_plan describes a real correlation table", {
  spec <- as_spec(apa.cor.table(attitude))
  p <- apa_display_plan(spec)
  expect_equal(p$n_col, length(spec$col_labels))
  expect_equal(length(p$align), p$n_col)
  # the column-label row is the LAST header row
  labels <- p$header[[length(p$header)]]
  expect_equal(length(labels$cells), p$n_col)
  # every body entry is a group label or a full data row
  types <- vapply(p$body, function(r) r$type, character(1))
  expect_true(all(types %in% c("group", "data")))
  for (r in p$body[types == "data"]) expect_equal(length(r$cells), p$n_col)
})

test_that("apa_display_plan places row-group labels before their first row", {
  # A single-block apa.reg.table() has NO row groups, so this test needs a
  # spec that genuinely produces them -- a two-block regression -- or the
  # assertion below never runs (a decorative "if it applies" test that stays
  # green even if the group label moved to the wrong spot; see MEMORY.md
  # "absence assertions fail silently").
  spec <- as_spec(apa.reg.table(lm(mpg ~ wt, data = mtcars),
                                lm(mpg ~ wt + hp, data = mtcars)))
  expect_gt(length(spec$row_groups), 0)
  p <- apa_display_plan(spec)
  types <- vapply(p$body, function(r) r$type, character(1))
  group_positions <- which(types == "group")
  # one group entry per row group, each immediately before its first row
  expect_equal(length(group_positions), length(spec$row_groups))
  for (k in seq_along(spec$row_groups)) {
    starts <- min(spec$row_groups[[k]]$rows)
    # position in p$body of the k-th group's first DATA row: count how many
    # group entries precede it (one per group, since each is placed right
    # before its own first row) -- so the group entry itself sits at
    # starts + (k - 1).
    expect_identical(p$body[[starts + (k - 1L)]]$type, "group")
    expect_equal(as_plaintext(p$body[[starts + (k - 1L)]]$label),
                as_plaintext(spec$row_groups[[k]]$label))
  }
  expect_identical(p$body[[1]]$type, "group")
})

test_that("apa_display_plan carries a flat correlation table's cells and alignment through unconverted", {
  spec <- as_spec(apa.cor.table(attitude))
  p <- apa_display_plan(spec)
  # no groups -> every body entry is data, one per spec row, in spec order
  expect_equal(length(p$body), length(spec$body))
  expect_true(all(vapply(p$body, function(r) identical(r$type, "data"), logical(1))))
  for (i in seq_along(spec$body)) {
    expect_identical(p$body[[i]]$cells, spec$body[[i]])
  }
  expect_identical(p$align, spec$col_align)
  # a flat table (no spanners) has exactly one header row: the column labels
  expect_length(p$header, 1)
  labels <- p$header[[1]]
  for (j in seq_len(p$n_col)) {
    expect_identical(labels$cells[[j]]$rt, spec$col_labels[[j]])
    expect_equal(labels$cells[[j]]$span, 1L)
    expect_equal(labels$cells[[j]]$align, spec$col_align[j])
  }
  expect_length(labels$rules, 0)
})

test_that("apa_display_plan orders a decked (two-level) spanner header highest level first, richtext preserved", {
  spec <- as_spec(apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                                  data = goggles))
  p <- apa_display_plan(spec)
  # 2 spanner levels (1 and 2) + the column-label row
  expect_length(p$header, 3)

  # highest level (2, the "alcohol" factor spanner) is FIRST
  top <- p$header[[1]]
  # cols 2:7 belong to one spanner covering all 6 non-row-label columns;
  # column 1 (the row-label column) is not covered by any spanner
  expect_equal(vapply(top$cells, function(c) c$span, integer(1)), c(1L, 6L))
  expect_equal(as_plaintext(top$cells[[2]]$rt), "alcohol")
  expect_equal(top$rules, list(c(2L, 7L)))

  # level 1 (the three alcohol-level spanners: None / 2 Pints / 4 Pints) is next
  mid <- p$header[[2]]
  expect_equal(vapply(mid$cells, function(c) c$span, integer(1)), c(1L, 2L, 2L, 2L))
  expect_equal(vapply(mid$cells[-1], function(c) as_plaintext(c$rt), character(1)),
               c("None", "2 Pints", "4 Pints"))
  expect_equal(mid$rules, list(c(2L, 3L), c(4L, 5L), c(6L, 7L)))

  # the column-label row (M / SD repeated) is LAST, one cell per column, no rules
  labels <- p$header[[3]]
  expect_equal(length(labels$cells), p$n_col)
  expect_length(labels$rules, 0)

  # spanner labels are real richtext (not stringified) -- e.g. carry a class,
  # not a bare character -- so a LaTeX or markdown emitter can still style them
  expect_s3_class(top$cells[[2]]$rt, "apa_richtext")
})

test_that("apa_display_plan reproduces row groups for a spec with no spanners (two-way, CI)", {
  spec <- as_spec(apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                                  data = goggles, show.conf.interval = TRUE))
  p <- apa_display_plan(spec)
  expect_length(spec$spanners, 0)
  expect_length(p$header, 1)  # only the column-label row
  # 3 row groups -> 3 group entries interleaved with 6 data rows -> 9 entries
  expect_equal(length(p$body), length(spec$body) + length(spec$row_groups))
  types <- vapply(p$body, function(r) r$type, character(1))
  expect_equal(sum(types == "group"), length(spec$row_groups))
  expect_equal(sum(types == "data"), length(spec$body))
  # each group label immediately precedes the first row of its group
  group_positions <- which(types == "group")
  expect_equal(group_positions, c(1L, 4L, 7L))
  expect_equal(as_plaintext(p$body[[1]]$label), as_plaintext(spec$row_groups[[1]]$label))
})

test_that("apa_display_plan rejects a spanner whose columns are not contiguous", {
  # No shipped builder produces this shape (the only spanner-bearing builder,
  # apa.2way.table via twoway_spanners(), always emits first:last ranges),
  # but the header-row walk finds a spanner by testing min(cols) == col and
  # then advancing by length(cols) -- so cols = c(2, 4) would otherwise
  # silently produce a header row with cell spans (1, 2, 1) alongside a rule
  # range of (2, 4): internally inconsistent, and no error anywhere.
  wip <- apa_spec_start(1)
  wip <- apa_col(wip, "a", values = list("1"))
  wip <- apa_col(wip, "b", values = list("2"))
  wip <- apa_col(wip, "c", values = list("3"))
  wip <- apa_col(wip, "d", values = list("4"))
  wip <- apa_spanner(wip, "bad", cols = c(2, 4))
  spec <- apa_spec_finish(wip)
  expect_error(apa_display_plan(spec), "contiguous")
})

test_that("apa_display_plan accepts a contiguous spanner given as doubles", {
  # The defect this pins: sort() on a double stays double, seq.int() returns
  # integer even for whole-number double arguments, and identical() is
  # type-strict -- so cols = c(2, 3) raised "must be contiguous" while 2:3
  # passed, for the same two columns. Dormant only because the sole shipped
  # spanner-bearing builder (apa.2way.table via twoway_spanners()) always
  # writes first:last. It goes live the moment apa_spanner() is exported or
  # any builder writes cols = c(x, y).
  wip <- apa_spec_start(1)
  wip <- apa_col(wip, "a", values = list("1"))
  wip <- apa_col(wip, "b", values = list("2"))
  wip <- apa_col(wip, "c", values = list("3"))
  wip <- apa_col(wip, "d", values = list("4"))
  wip <- apa_spanner(wip, "ok", cols = c(2, 3))   # DOUBLES, contiguous
  spec <- apa_spec_finish(wip)

  p <- apa_display_plan(spec)

  # The spanner row exists and spans exactly two columns.
  spanner_row <- p$header[[1]]
  spans <- vapply(spanner_row$cells, function(c) c$span, integer(1))
  expect_equal(spans, c(1L, 2L, 1L))

  # And the rule range is INTEGER, not double: a double range would flow on
  # into every emitter's cmidrule arithmetic untyped. expect_identical, not
  # expect_equal -- expect_equal would pass on c(2, 3) and see nothing.
  expect_identical(spanner_row$rules[[1]], c(2L, 3L))
})

test_that("the spec validator rejects fractional spanner columns before a plan is ever built", {
  # Where the real guard is: validate_apa_table_spec() (R/spec.R:89) tests
  # sp$cols %in% seq_len(n_col), and %in% is exact-match, so a fractional
  # column can never reach apa_display_plan() through apa_spec_finish() at
  # all. Recorded here because the obvious place to look for this guard is
  # the contiguity check, and it is not there.
  wip <- apa_spec_start(1)
  for (nm in c("a", "b", "c", "d")) wip <- apa_col(wip, nm, values = list("1"))
  wip <- apa_spanner(wip, "bad", cols = c(2.5, 3.5))
  expect_error(apa_spec_finish(wip), "index existing columns")
})

test_that("apa_display_plan itself rejects fractional spanner columns", {
  # Reached only by mutating a FINISHED spec, which is the one route that
  # bypasses validate_apa_table_spec(). Before the as.integer() fix this
  # passed silently: sort(c(2.5, 3.5)) and seq.int(2.5, 3.5) are both the
  # same double vector, so identical() was TRUE and a fractional spanner
  # sailed through the contiguity check. Truncating the left side to
  # integer is what makes the two sides disagree and the check fire.
  wip <- apa_spec_start(1)
  for (nm in c("a", "b", "c", "d")) wip <- apa_col(wip, nm, values = list("1"))
  wip <- apa_spanner(wip, "ok", cols = 2:3)
  spec <- apa_spec_finish(wip)
  spec$spanners[[1]]$cols <- c(2.5, 3.5)   # after validation, on purpose
  expect_error(apa_display_plan(spec), "contiguous")
})

test_that("apa_display_plan rejects two spanners at the same level that overlap a column", {
  wip <- apa_spec_start(1)
  wip <- apa_col(wip, "a", values = list("1"))
  wip <- apa_col(wip, "b", values = list("2"))
  wip <- apa_col(wip, "c", values = list("3"))
  wip <- apa_col(wip, "d", values = list("4"))
  wip <- apa_spanner(wip, "left", cols = 2:3)
  wip <- apa_spanner(wip, "right", cols = 3:4)
  spec <- apa_spec_finish(wip)
  expect_error(apa_display_plan(spec), "overlap")
})
