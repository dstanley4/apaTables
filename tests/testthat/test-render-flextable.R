# ft_font_sizes() -- every font size set on a flextable -- now lives in
# helper-test-functions.R, so the render-calibration tests can use it too.

test_that("apa_flextable_theme returns a flextable with horizontal rules", {
  ft <- flextable::flextable(head(mtcars[, 1:3], 3))
  styled <- apa_flextable_theme(ft)
  expect_s3_class(styled, "flextable")
})

demo_spec <- function() {
  apa_spec_start(2) |>
    apa_col("Predictor", "l", list("Adverts", "Airplay")) |>
    apa_col(apa_sym("b"), "r", list(richtext(".08"), richtext(".90"))) |>
    apa_col(apa_ci_label(.95), "c",
            list(apa_ci(0.05, 0.11), apa_ci(0.70, 1.10))) |>
    apa_note("* p < .05.") |>
    apa_spec_finish(title = "Regression of Sales", number = 2L)
}

test_that("apa_flextable builds a 3-column, 2-row flextable", {
  ft <- apa_flextable(demo_spec())
  expect_s3_class(ft, "flextable")
  expect_equal(flextable::nrow_part(ft, "body"), 2L)
  expect_equal(length(ft$col_keys), 3L)
})

test_that("apa_flextable HTML output is stable (snapshot)", {
  ft <- apa_flextable(demo_spec())
  html <- flextable::htmltools_value(ft) |> as.character()
  # strip volatile auto-generated ids before snapshotting
  html <- gsub("id=\"[^\"]+\"", "id=\"X\"", html)
  # strip volatile hash-based CSS class names (e.g., cl-4ff62982 -> cl-X)
  html <- gsub("cl-[0-9a-f]{8}", "cl-X", html)
  expect_snapshot(cat(html))
})

test_that("composed flextable cells use the APA theme font, not officer's Arial default", {
  spec <- apa_spec_start(1) |>
    apa_col(apa_sym("beta"), "r", list(richtext(".50"))) |>
    apa_spec_finish(number = 1L)
  html <- as.character(flextable::htmltools_value(apa_flextable(spec)))
  expect_match(html, "Times New Roman")
  expect_no_match(html, "Arial")
})

test_that("header symbol renders as a styled paragraph (beta), not literal text", {
  spec <- apa_spec_start(1) |>
    apa_col(apa_sym("beta"), "r", list(richtext(".50"))) |>
    apa_spec_finish(number = 1L)
  ft <- apa_flextable(spec)
  # the rendered header should contain the beta glyph, supplied by a chunk
  html <- as.character(flextable::htmltools_value(ft))
  expect_match(html, "\u03b2")
})

test_that("apa_flextable adds a spanner header row", {
  spec <- apa_spec_start(1) |>
    apa_col("Var", "l", list("A")) |>
    apa_col(apa_sym("M"), "r", list(richtext("1.0"))) |>
    apa_col(apa_sym("SD"), "r", list(richtext("0.2"))) |>
    apa_spanner("Descriptives", cols = 2:3) |>
    apa_spec_finish(number = 1L)
  ft <- apa_flextable(spec)
  expect_gte(flextable::nrow_part(ft, "header"), 2L)
  html <- as.character(flextable::htmltools_value(ft))
  expect_match(html, "Descriptives")
  # spanner rule survives theme: header row 1, cols 2-3 must have a bottom border
  # field is border.width.bottom (not border.bottom.width) in flextable 0.9.12
  spanner_border <- ft$header$styles$cells$border.width.bottom$data[1, 2:3]
  expect_length(spanner_border, 2)
  expect_true(all(spanner_border > 0),
              info = "spanner spanning rule (header row 1, cols 2-3) must survive apa_flextable_theme()")
})

test_that("apa_flextable renders a row-group label", {
  spec <- apa_spec_start(2) |>
    apa_col("Predictor", "l", list("Adverts", "Airplay")) |>
    apa_col(apa_sym("b"), "r", list(richtext(".08"), richtext(".90"))) |>
    apa_row_group("Step 1", rows = 1:2) |>
    apa_spec_finish(number = 1L)
  ft <- apa_flextable(spec)
  html <- as.character(flextable::htmltools_value(ft))
  expect_match(html, "Step 1")
})

test_that("multi-line cells render without error and keep both lines", {
  spec <- apa_spec_start(1) |>
    apa_col("Var", "l", list("A")) |>
    apa_col(apa_sym("r"), "c",
            list(richtext_lines(".42**", apa_ci(0.20, 0.55)))) |>
    apa_spec_finish(number = 1L)
  ft <- apa_flextable(spec)
  html <- as.character(flextable::htmltools_value(ft))
  expect_match(html, "\\.42")
  expect_match(html, "\\.20")
})

test_that("apa_flextable accepts a legacy apa_table that wraps a spec", {
  legacy <- structure(list(spec = demo_spec()), class = "apa_table")
  expect_s3_class(apa_flextable(legacy), "flextable")
})

test_that("a superscript run that is also italic keeps both", {
  rt <- richtext(list(rt_run("R", italic = TRUE), rt_run("2", italic = TRUE, script = "super")))
  spec <- apa_spec_start(1) |>
    apa_col(rt, "r", list(richtext("1.0"))) |>
    apa_spec_finish(number = 1L)
  html <- as.character(flextable::htmltools_value(apa_flextable(spec)))
  # the "2" run itself must be both raised and italic
  span <- regmatches(html, gregexpr('class="[^"]*">2</span>', html))[[1]]
  expect_length(span, 1L)
  rule <- ft_css_rule(html, sub('class="([^"]*)".*', "\\1", span))
  expect_true(css_is_superscript(rule), info = rule)
  expect_match(rule, "font-style:italic", fixed = TRUE)
  expect_match(html, "Times New Roman")
})

test_that("apa_flextable places each row-group label above its own block", {
  spec <- apa_spec_start(4) |>
    apa_col("Predictor", "l", list("Zeta", "Theta", "Kappa", "Sigma")) |>
    apa_col(apa_sym("b"), "r", list("1", "2", "3", "4")) |>
    apa_row_group("Model 1", rows = 1:2) |>
    apa_row_group("Model 2", rows = 3:4) |>
    apa_spec_finish(number = 1L)
  ft <- apa_flextable(spec)
  # 4 data rows + 2 inserted label rows
  expect_equal(flextable::nrow_part(ft, "body"), 6L)
  html <- as.character(flextable::htmltools_value(ft))
  pos <- function(s) regexpr(s, html, fixed = TRUE)[[1]]
  expect_true(pos("Model 1") < pos("Zeta"))
  expect_true(pos("Zeta")    < pos("Model 2"))
  expect_true(pos("Model 2") < pos("Kappa"))
})

test_that("apa_flextable stacks multi-level spanners (decked head)", {
  spec <- apa_spec_start(2) |>
    apa_col("Group", "l", list("Female", "Male")) |>
    apa_col(apa_sym("M"), "r", list("1", "3")) |>
    apa_col(apa_sym("SD"), "r", list("2", "4")) |>
    apa_col(apa_sym("M"), "r", list("5", "7")) |>
    apa_col(apa_sym("SD"), "r", list("6", "8")) |>
    apa_spanner("None", cols = 2:3, level = 1) |>
    apa_spanner("Pints", cols = 4:5, level = 1) |>
    apa_spanner("Alcohol", cols = 2:5, level = 2) |>
    apa_spec_finish(number = 1L)
  ft <- apa_flextable(spec)
  expect_equal(flextable::nrow_part(ft, "header"), 3L)   # 2 spanner rows + column labels
  html <- as.character(flextable::htmltools_value(ft))
  pos <- function(s) regexpr(s, html, fixed = TRUE)[[1]]
  expect_true(pos("Alcohol") < pos("None"))   # level 2 renders above level 1
  expect_true(pos("None") < pos("Female"))    # spanners above the body
})

# No page-fitting tests belong here. apa_flextable() takes one argument and
# knows nothing about any page -- it writes Word, and what Word needs is
# fitted by apa.save() against the page it is actually writing
# (test-apa-save.R). The Quarto PDF path's own width decisions are tested in
# test-render-latex.R and test-quarto-table.R.
