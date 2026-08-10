test_that("twoway_col_label maps M/SD symbols and passes through factor names", {
  expect_equal(as_plaintext(twoway_col_label("$M$")), "M")
  expect_true(twoway_col_label("$M$")[[1]]$italic)
  expect_equal(as_plaintext(twoway_col_label("$SD$")), "SD")
  expect_equal(as_plaintext(twoway_col_label("gender")), "gender")
})

test_that("twoway_spanners builds a two-level decked head (2x3, no marginal)", {
  sp <- twoway_spanners(c(" ", "None", "2 Pints", "4 Pints"), "alcohol", n_col = 7)
  # 3 level-1 spanners + 1 level-2 spanner
  lvl1 <- Filter(function(s) s$level == 1, sp)
  lvl2 <- Filter(function(s) s$level == 2, sp)
  expect_length(lvl1, 3)
  expect_length(lvl2, 1)
  expect_equal(lapply(lvl1, `[[`, "cols"), list(2:3, 4:5, 6:7))
  expect_equal(vapply(lvl1, function(s) as_plaintext(s$label), character(1)),
               c("None", "2 Pints", "4 Pints"))
  expect_equal(lvl2[[1]]$cols, 2:7)
  expect_equal(as_plaintext(lvl2[[1]]$label), "alcohol")
})

test_that("twoway_spanners excludes a trailing Marginal group from the factor spanner", {
  sp <- twoway_spanners(c(" ", "None", "2 Pints", "4 Pints", "Marginal"),
                        "alcohol", n_col = 9)
  lvl1 <- Filter(function(s) s$level == 1, sp)
  lvl2 <- Filter(function(s) s$level == 2, sp)
  expect_length(lvl1, 4)                       # incl. Marginal
  expect_equal(lvl1[[4]]$cols, 8:9)
  expect_equal(as_plaintext(lvl1[[4]]$label), "Marginal")
  expect_equal(lvl2[[1]]$cols, 2:7)            # factor spans the 3 levels only
})

test_that("twoway_spec_note mirrors the console note", {
  base <- as_plaintext(twoway_spec_note(FALSE, FALSE))
  expect_false(grepl("Note\\.", base))
  expect_match(base, "M = mean")
  expect_match(base, "SD = standard deviation")
  expect_false(grepl("Marginal", base))
  marg <- as_plaintext(twoway_spec_note(TRUE, FALSE))
  expect_match(marg, "Marginal indicates")
})

test_that("apa.2way.table attaches a well-formed spec (goggles, no CI)", {
  tbl <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                        data = goggles, table.number = 3)
  expect_s3_class(tbl, "apa_table")
  s <- tbl$spec
  expect_s3_class(s, "apa_table_spec")
  expect_silent(validate_apa_table_spec(s))
  expect_equal(s$number, 3L)
  # 1 row-label col + 3 alcohol levels x (M, SD) = 7 columns
  expect_length(s$col_labels, 7)
  labs <- vapply(s$col_labels, as_plaintext, character(1))
  expect_equal(labs[2:7], rep(c("M", "SD"), 3))
  # 2 body rows (Female, Male); first cell is the row label
  expect_length(s$body, 2)
  expect_equal(as_plaintext(s$body[[1]][[1]]), "Female")
  expect_equal(as_plaintext(s$body[[2]][[1]]), "Male")
  # decked head: 3 level-1 spanners + 1 level-2 spanner over cols 2:7
  lvl1 <- Filter(function(g) g$level == 1, s$spanners)
  lvl2 <- Filter(function(g) g$level == 2, s$spanners)
  expect_length(lvl1, 3)
  expect_length(lvl2, 1)
  expect_equal(lvl2[[1]]$cols, 2:7)
  expect_equal(as_plaintext(lvl2[[1]]$label), tbl$latex.extra.header2)
  # note has no "Note." prefix
  expect_false(grepl("Note\\.", as_plaintext(s$notes[[1]])))
})

test_that("apa.2way.table spec handles marginal means (extra column + row)", {
  tbl <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                        data = goggles, show.marginal.means = TRUE)
  s <- tbl$spec
  expect_silent(validate_apa_table_spec(s))
  # adds a Marginal column group (now 9 cols) and a Marginal row (now 3 rows)
  expect_length(s$col_labels, 9)
  expect_length(s$body, 3)
  expect_equal(as_plaintext(s$body[[3]][[1]]), "Marginal")
  # the factor (level-2) spanner still covers only the 3 alcohol levels
  lvl2 <- Filter(function(g) g$level == 2, s$spanners)
  expect_equal(lvl2[[1]]$cols, 2:7)
  expect_match(as_plaintext(s$notes[[1]]), "Marginal indicates")
})

test_that("apa.2way.table attaches a well-formed spec (goggles, with CI -> row groups)", {
  tbl <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                        data = goggles, table.number = 3, show.conf.interval = TRUE)
  expect_equal(tbl$table.type, "twoway-ci")
  s <- tbl$spec
  expect_s3_class(s, "apa_table_spec")
  expect_silent(validate_apa_table_spec(s))
  expect_equal(s$number, 3L)
  # 4 flat columns: gender, M, 95% CI, SD (CI between M and SD); no spanners
  expect_length(s$col_labels, 4)
  expect_equal(vapply(s$col_labels, as_plaintext, character(1)),
               c("gender", "M", "95% CI", "SD"))
  expect_length(s$spanners, 0)
  # 6 body rows, 3 row groups of 2 (alcohol levels)
  expect_length(s$body, 6)
  expect_length(s$row_groups, 3)
  expect_equal(vapply(s$row_groups, function(g) as_plaintext(g$label), character(1)),
               c("alcohol: None", "alcohol: 2 Pints", "alcohol: 4 Pints"))
  expect_equal(s$row_groups[[1]]$rows, 1:2)
  expect_equal(s$row_groups[[3]]$rows, 5:6)
  # note has no "Note." prefix, mentions confidence interval, no Marginal sentence
  expect_false(grepl("Note\\.", as_plaintext(s$notes[[1]])))
  expect_match(as_plaintext(s$notes[[1]]), "confidence interval")
  expect_false(grepl("Marginal", as_plaintext(s$notes[[1]])))
})

test_that("attaching $spec leaves the legacy two-way fields intact", {
  tbl <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                        data = goggles, table.number = 3)
  expect_true(all(c("table_title", "table_body", "table_note",
                    "latex.body", "latex.column.labels", "table.type") %in% names(tbl)))
})
