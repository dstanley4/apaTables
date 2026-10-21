test_that("apa_caption and apa_note_text return clean strings", {
  s <- apa_spec_start(1) |>
    apa_col("Var", "l", list("A")) |>
    apa_note("N = 200.", "* p < .05.") |>
    apa_spec_finish(title = "Means and SDs", number = 1L)
  expect_equal(apa_caption(s), "Means and SDs")
  expect_equal(apa_note_text(s), "N = 200. * p < .05.")
  expect_false(grepl("Note\\.", apa_note_text(s)))
})

test_that("apa_note_text is empty when there are no notes", {
  s <- apa_spec_start(1) |>
    apa_col("Var", "l", list("A")) |>
    apa_spec_finish(number = 1L)
  expect_equal(apa_note_text(s), "")
})

test_that("apa.quarto.template emits NEITHER a tbl-cap NOR an apa-note line", {
  # apa.quarto.pdf() supplies the title AND the note from the spec, so either
  # line would only hand the reader back wording the package
  # writes for itself -- and a typed title stops matching when the analysis
  # changes. The spec below HAS both a title and a note, so a template that
  # printed either would be caught here rather than passing vacuously.
  s <- apa_spec_start(1) |>
    apa_col("Var", "l", list("A")) |>
    apa_note("N = 200.") |>
    apa_spec_finish(title = "Means and SDs", number = 1L)
  out <- capture.output(txt <- apa.quarto.template(s, label = "tbl-demo", code = "my_tbl"))
  expect_match(txt, "#\\| label: tbl-demo")
  expect_false(grepl("tbl-cap", txt, fixed = TRUE))
  expect_false(grepl("Means and SDs", txt, fixed = TRUE))
  expect_false(grepl("apa-note", txt, fixed = TRUE))
  expect_false(grepl("N = 200.", txt, fixed = TRUE))
  expect_match(txt, "#\\| message: false")
  expect_match(txt, "#\\| warning: true")
  # TWO LINES, NO PIPE. The call takes the object as an ordinary argument;
  # the pipe form must be absent from the artifact the package HANDS the
  # user to paste, not merely from the prose about it.
  expect_match(txt, "apa\\.quarto\\.pdf\\(my_tbl\\)")
  expect_false(grepl("|>", txt, fixed = TRUE))
  expect_false(any(grepl(" $", out)))  # printed chunk has no trailing whitespace
  # The console explanation has to SAY the title and note are coming, since
  # the chunk itself no longer shows a line for either. Without this the
  # reader is handed a chunk that mentions neither and told nothing.
  full <- paste(out, collapse = "\n")
  expect_true(grepl("title and note", full, fixed = TRUE))
  expect_true(grepl("tbl-cap", full, fixed = TRUE))   # ...and how to override
  expect_true(grepl("apa-note", full, fixed = TRUE))
})

test_that("apa.quarto.template default code reuses what was typed for x", {
  s <- apa_spec_start(1) |>
    apa_col("Var", "l", list("A")) |>
    apa_spec_finish(title = "T", number = 1L)
  my_tbl <- s
  out <- capture.output(txt <- apa.quarto.template(my_tbl))
  expect_match(txt, "apa\\.quarto\\.pdf\\(my_tbl\\)")

  # BARE NAME: the template cannot know what built the object, so the build
  # line is a COMMENTED placeholder sitting where the real line goes.
  # This spec was hand-built and carries no table.type, so the placeholder
  # takes its generic form -- the case every existing test in this file
  # exercises, and the reason the mapping needs a fallback at all.
  # The chunk's R lines are everything that is not a fence or an option line.
  # Reading them as LINES, not as a substring of the whole block: "# my_tbl
  # <- " appearing somewhere in a multi-line string would also be satisfied
  # by a stray mention in the prose.
  rlines <- apa_chunk_code_lines(txt)
  expect_equal(length(rlines), 2L)
  expect_match(rlines[1], "^# my_tbl <- ")
  expect_match(rlines[1], "the line that builds")
  expect_equal(rlines[2], "apa.quarto.pdf(my_tbl)")

  # AND NO PROSE SENTENCE above the chunk telling the reader to remember the
  # build line, in either case: a novice copies the block, runs it, and meets
  # the failure well after having scrolled past the explanation. The comment
  # is in the place the missing line goes.
  expect_false(any(grepl("code that creates", out)))

  make_spec <- function() s
  out2 <- capture.output(txt2 <- apa.quarto.template(make_spec()))
  # SELF-CONTAINED CALL: the template SAW the expression, so both lines are
  # live code and the object gets the same name the label already uses.
  rlines2 <- apa_chunk_code_lines(txt2)
  expect_equal(length(rlines2), 2L)
  # LIVE, not commented -- this is the case the template can complete, and
  # asserting the text without asserting it is uncommented would pass on a
  # placeholder that happened to contain the right call.
  expect_equal(rlines2[1], "mytable <- make_spec()")
  expect_equal(rlines2[2], "apa.quarto.pdf(mytable)")
  expect_false(any(grepl("code that creates", out2)))
})

# The placeholder names the REAL builder, which costs a table.type ->
# function-name map (APA_TYPE_BUILDER). These two
# tests are the guard that stops it going stale: one catches a builder that is
# added or renamed without being mapped, the other catches a builder whose
# table.type has no entry. Without them the template would silently print the
# wrong function name -- worse than printing none, because it is confidently
# wrong.
test_that("every exported builder appears in the table.type map", {
  exported <- grep("^apa\\.[^.]+\\.table$", getNamespaceExports("apaTables"),
                   value = TRUE)
  # apa.quarto.* entry points are not builders and must not be in the map.
  exported <- setdiff(exported, grep("^apa\\.quarto\\.", exported, value = TRUE))
  expect_gt(length(exported), 0L)
  expect_equal(sort(setdiff(exported, unname(apaTables:::APA_TYPE_BUILDER))),
               character(0))
})

test_that("every real builder's table.type has a map entry", {
  # Built by RUNNING the builders, not by reading a list of type strings --
  # a list would agree with itself while disagreeing with the code, which is
  # a common failure shape.
  #
  # AND IT READS `as_spec(x)$meta$table.type`, NOT `x$table.type`. Those are
  # DIFFERENT FIELDS. They agree today, but apa.aov.table() once set the
  # object's to "aovstats" and the spec's to "aov", and a version of this test
  # reading the object field passed while `R CMD check` died running the
  # apa.aov.table example. apa_builder_hint() is handed the SPEC, so the spec
  # is what has to be asserted.
  old <- options(contrasts = c("contr.sum", "contr.poly"))
  on.exit(options(old), add = TRUE)
  tables <- list(
    apa.cor.table(attitude),
    apa.reg.table(lm(sales ~ adverts, data = album)),
    apa.1way.table(iv = dose, dv = libido, data = viagra),
    apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                   data = goggles),
    apa.d.table(iv = dose, dv = libido, data = viagra),
    apa.aov.table(lm(libido ~ dose, data = viagra)))
  types <- vapply(tables, function(t) as_spec(t)$meta$table.type[1],
                  character(1))
  expect_gt(length(unique(types)), 3L)
  for (ty in unique(types)) {
    expect_true(ty %in% names(apaTables:::APA_TYPE_BUILDER),
                info = paste("unmapped spec table.type:", ty))
  }

  # And the hint is a real builder name for every one of them -- the property
  # the map exists for. Asserting only that a key EXISTS would still pass if
  # the lookup itself were broken: `[[` on a named vector throws instead of
  # returning NULL.
  for (t in tables) {
    expect_match(apaTables:::apa_builder_hint(as_spec(t)),
                 "^apa\\.[^.]+\\.table\\(\\.\\.\\.\\)$")
  }
})

test_that("the placeholder names the builder that actually made the table", {
  my_table <- apa.cor.table(attitude)
  txt <- capture.output(t1 <- apa.quarto.template(my_table))
  expect_match(t1, "# my_table <- apa\\.cor\\.table\\(")

  my_table <- apa.reg.table(lm(sales ~ adverts, data = album))
  txt <- capture.output(t2 <- apa.quarto.template(my_table))
  expect_match(t2, "# my_table <- apa\\.reg\\.table\\(")

  # Not merely "a builder name is printed": the WRONG one must not be.
  expect_false(grepl("apa.reg.table", t1, fixed = TRUE))
  expect_false(grepl("apa.cor.table", t2, fixed = TRUE))
})

test_that("apa.quarto.template printed output is self-documenting", {
  s <- apa_spec_start(1) |>
    apa_col("Var", "l", list("A")) |>
    apa_spec_finish(title = "T", number = 1L)
  out <- capture.output(apa.quarto.template(s, code = "my_tbl"))
  expect_true(any(grepl("Copy this chunk", out)))
  expect_false(any(grepl(" $", out)))   # no trailing whitespace anywhere
})

test_that("apa.quarto.template never emits apa-note, even for a table with a multi-part note", {
  # A real builder whose note has TWO parts (the confidence-interval sentence
  # and the significance-star sentence) -- the case the old YAML-list
  # escaping used to exercise. The note is generated automatically now, so
  # the template prints neither part.
  t1 <- apa.cor.table(attitude)
  out <- capture.output(chunk <- apa.quarto.template(t1, code = "table1"))
  expect_false(grepl("apa-note", chunk, fixed = TRUE))
  expect_false(grepl("number of cases", chunk, fixed = TRUE))
  expect_false(grepl("indicates *p* < .05.", chunk, fixed = TRUE))
  # ...and no title line either. The generated title is the one thing
  # a real builder is guaranteed to have, so its ABSENCE is what to assert.
  expect_false(grepl("tbl-cap", chunk, fixed = TRUE))
  expect_false(grepl("Descriptive Statistics and Correlations", chunk, fixed = TRUE))
})

test_that("apa.quarto.template always omits apa-note, note present or not", {
  s <- apa_spec_start(1) |>
    apa_col("Var", "l", list("A")) |>
    apa_spec_finish(title = "T", number = 1L)
  txt <- apa.quarto.template(s, label = "tbl-x", code = "my_tbl")
  expect_false(grepl("apa-note", txt))
  # The settings lines are label, message, warning -- CONSECUTIVE, which is
  # what catches a title line creeping back in between them.
  expect_match(txt, "#\\| label: tbl-x\n#\\| message: false\n#\\| warning: true")
})

test_that("knit_print.apa_table_spec delegates to the flextable knit_print", {
  skip_if_not_installed("knitr")
  s <- apa_spec_start(1) |>
    apa_col("Var", "l", list("A")) |>
    apa_col(apa_sym("M"), "r", list(richtext("1.0"))) |>
    apa_spec_finish(number = 1L)
  out <- knitr::knit_print(s)
  # flextable's knit_print returns a knit_asis object
  expect_s3_class(out, "knit_asis")
})

test_that("apa_caption / apa_note_text accept a legacy wrapper", {
  s <- apa_spec_start(1) |> apa_col("A", "l", list("x")) |>
    apa_note("N = 10.") |> apa_spec_finish(title = "T", number = 1L)
  legacy <- structure(list(spec = s), class = "apa_table")
  expect_equal(apa_caption(legacy), "T")
  expect_equal(apa_note_text(legacy), "N = 10.")
})

test_that("knit_print.apa_table renders a spec-carrying object as a markdown grid table (no flextable)", {
  # Outside any real knit (so not a LaTeX target), knit_print.apa_table ->
  # apa_quarto_knit_print() -> apa_quarto_render() returns
  # apa_quarto_markdown_block()'s pandoc grid table -- there is no flextable
  # branch anywhere on this path.
  tbl <- apa.cor.table(attitude)
  out <- knitr::knit_print(tbl)
  expect_s3_class(out, "knit_asis")
  txt <- as.character(out)
  expect_match(txt, "+===", fixed = TRUE)
  expect_match(txt, "rating", fixed = TRUE)
})

test_that("knit_print.apa_table delegates (no flextable) when there is no spec", {
  reg <- apa.reg.table(lm(mpg ~ wt, mtcars))
  reg$spec <- NULL                      # strip the spec -> force the fallback branch
  out <- suppressWarnings(capture.output(res <- knitr::knit_print(reg)))
  expect_false(inherits(res, "flextable"))
})

test_that("apa.quarto.template measures with the SAME ladder the chunk uses", {
  blk1 <- lm(sales ~ adverts + airplay, data = album)
  blk2 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data = album)
  wide <- apa.reg.table(blk1, blk2)
  out <- capture.output(txt <- apa.quarto.template(wide, code = "wide"))
  joined <- paste(out, collapse = "\n")

  # ONE chunk, ONE display line, no markers and no YAML to add. Do not
  # reintroduce ::: {.landscape}, a page = "landscape" argument, or an
  # instruction to set floatsintext: true -- three statements of one fact, in
  # three languages, none able to see the others. Rotation is measured in R
  # and executed by apaquarto in BOTH table placements.
  expect_match(txt, "apa.quarto.pdf(wide)", fixed = TRUE)
  expect_no_match(txt, 'page = "landscape"', fixed = TRUE)
  expect_no_match(txt, "landscape")
  expect_no_match(joined, "floatsintext", fixed = TRUE)

  # It still SAYS the table is wide, and it says so from the native model.
  expect_match(joined, "inches wide", fixed = TRUE)
  expect_match(joined, "rotated (landscape) page automatically", fixed = TRUE)
})

test_that("apa.quarto.template's landscape message: MEASURED wording is unchanged", {
  # The branch fired when spec$landscape is NA and the table is too wide
  # upright -- the automatic path's wording. (Pinned here, alongside the
  # other two branches, so the three tests read as one set.)
  blk1 <- lm(sales ~ adverts + airplay, data = album)
  blk2 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data = album)
  wide_measured <- apa.reg.table(blk1, blk2)                     # landscape = NA
  out <- capture.output(txt <- apa.quarto.template(wide_measured, code = "wide"))
  joined <- paste(out, collapse = "\n")
  expect_match(joined, "too wide for an upright", fixed = TRUE)
  expect_match(joined, "rotated (landscape) page automatically", fixed = TRUE)
  expect_no_match(joined, "passed landscape = TRUE", fixed = TRUE)
  expect_no_match(joined, "stays upright", fixed = TRUE)
})

test_that("apa.quarto.template's landscape message: REQUESTED (landscape = TRUE) blames the argument, not measurement", {
  # apa.d.table(..., landscape = TRUE) measures 3.0 in, well under the 6.5 in
  # upright budget -- fits easily. The ladder receives landscape =
  # spec$landscape, so the MEASURED wording would be reachable here, and
  # every part of it would be false: "too wide" (it is not), "automatically"
  # (the argument caused it), and "nothing to add" (the argument IS what was
  # added, and removing it changes the result).
  narrow_requested <- apa.d.table(iv = dose, dv = libido, data = viagra,
                                   landscape = TRUE)              # ~3.0 in, fits upright
  out <- capture.output(txt <- apa.quarto.template(narrow_requested, code = "tb"))
  joined <- paste(out, collapse = "\n")
  expect_match(joined, "passed landscape = TRUE", fixed = TRUE)
  expect_no_match(joined, "too wide", fixed = TRUE)
  expect_no_match(joined, "rotated (landscape) page automatically", fixed = TRUE)
  expect_no_match(joined, "nothing to add to the chunk", fixed = TRUE)
})

test_that("apa.quarto.template's landscape message: FORBIDDEN (landscape = FALSE) reports the shrink instead of staying silent", {
  # A wide table with landscape = FALSE rotates in neither the template's
  # decision nor the render -- it stays upright and is shrunk. Silence here
  # would be incomplete: the reader would have no way to know from the
  # template output that anything had happened to the table at all.
  blk1 <- lm(sales ~ adverts + airplay, data = album)
  blk2 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data = album)
  wide_forbidden <- apa.reg.table(blk1, blk2, landscape = FALSE)
  out <- capture.output(txt <- apa.quarto.template(wide_forbidden, code = "wide"))
  joined <- paste(out, collapse = "\n")
  expect_match(joined, "does not fit an upright", fixed = TRUE)
  expect_match(joined, "stays upright", fixed = TRUE)
  expect_match(joined, "landscape = FALSE", fixed = TRUE)
  expect_no_match(joined, "too wide for an upright", fixed = TRUE)
  expect_no_match(joined, "rotated (landscape) page automatically", fixed = TRUE)
})

test_that("apa.quarto.template's landscape message: FORBIDDEN prints NO advice at all on html", {
  # apa_layout_decide()'s SHRINK math is not gated by can_execute_rotation
  # the way its rotate math is, so the FORBIDDEN branch above (shrunk_upright)
  # is reachable on target = "html" even though HTML has no page: nothing
  # rotates and nothing shrinks there, the table renders at full width
  # regardless. Ungated, apa.quarto.template(wide_forbidden, target = "html")
  # prints "This table does not fit an upright (portrait) PDF page ... stays
  # upright and is printed at N pt instead" -- wrong destination name AND a
  # shrink promise HTML never keeps. This pins "no landscape advice is
  # printed for html at all" for the FORBIDDEN case specifically; the two
  # other branches (measured, requested) are gated by
  # needs_landscape/can_execute_rotation.
  blk1 <- lm(sales ~ adverts + airplay, data = album)
  blk2 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data = album)
  wide_forbidden <- apa.reg.table(blk1, blk2, landscape = FALSE)
  out <- capture.output(txt <- apa.quarto.template(wide_forbidden, code = "wide",
                                                     target = "html"))
  joined <- paste(out, collapse = "\n")
  expect_no_match(joined, "does not fit", fixed = TRUE)
  expect_no_match(joined, "stays upright", fixed = TRUE)
  expect_no_match(joined, "PDF", fixed = TRUE)
  expect_no_match(joined, "Word", fixed = TRUE)
  expect_no_match(joined, "pt instead", fixed = TRUE)
})

test_that("the template's width is the ladder's width, not flextable's", {
  # The template must not measure with
  # sum(flextable_dim(apa_flextable(spec))$widths) while apa.quarto.pdf()
  # lays the table out as native LaTeX. For apa.cor.table(attitude) those two
  # numbers are 8.6 in and 5.29 in, so a flextable-measuring template
  # announces "too wide for a portrait PDF page" for a table that fits
  # upright, and sends it to a rotated page it does not need.
  narrow <- apa.cor.table(attitude)
  decision <- suppressWarnings(
    apaTables:::apa_layout_decide(apaTables:::apa_display_plan(narrow)))
  expect_false(decision$rotate)                    # 5.44 in against 6.5 in

  out <- capture.output(txt <- apa.quarto.template(narrow, code = "t"))
  joined <- paste(out, collapse = "\n")
  expect_no_match(txt, "landscape")
  expect_no_match(joined, "too wide", fixed = TRUE)

  # and the flextable number really is the far larger one -- so this test
  # would go red if the flextable measurement came back rather than merely
  # passing because both happen to agree
  ft_w <- sum(flextable::flextable_dim(
    apaTables:::apa_flextable(apaTables:::as_spec(narrow)))$widths)
  expect_gt(ft_w, 6.5)
})

test_that("apa.quarto.template leaves narrow tables untouched", {
  narrow <- apa.cor.table(attitude[, 1:3])               # 4.7 in
  out <- capture.output(txt <- apa.quarto.template(narrow, code = "narrow"))
  expect_no_match(txt, "landscape")                       # no markers
  expect_match(txt, "apa.quarto.pdf(narrow)", fixed = TRUE)
  expect_no_match(paste(out, collapse = "\n"), "floatsintext")
})

test_that("apa.quarto.template names the chunk after the table", {
  s <- apa_spec_start(1) |>
    apa_col("Var", "l", list("A")) |>
    apa_spec_finish(title = "T", number = 1L)
  table3 <- s
  txt <- apa.quarto.template(table3, code = "table3")
  expect_match(txt, "#\\| label: tbl-table3")
})

test_that("apa.quarto.template turns dots and underscores into hyphens", {
  s <- apa_spec_start(1) |>
    apa_col("Var", "l", list("A")) |>
    apa_spec_finish(title = "T", number = 1L)
  # "@tbl-my.table" in the document text stops the reference at the dot
  my.table <- s
  expect_match(apa.quarto.template(my.table, code = "x"),
               "#\\| label: tbl-my-table")
  my_table <- s
  expect_match(apa.quarto.template(my_table, code = "x"),
               "#\\| label: tbl-my-table")
})

test_that("apa.quarto.template falls back to tbl-mytable without a name", {
  s <- apa_spec_start(1) |>
    apa_col("Var", "l", list("A")) |>
    apa_spec_finish(title = "T", number = 1L)
  make_spec <- function() s
  expect_match(apa.quarto.template(make_spec(), code = "x"),
               "#\\| label: tbl-mytable")
  expect_match(apa.quarto.template(s |> as_spec(), code = "x"),
               "#\\| label: tbl-mytable")
})

test_that("apa.quarto.template lets an explicit label win", {
  s <- apa_spec_start(1) |>
    apa_col("Var", "l", list("A")) |>
    apa_spec_finish(title = "T", number = 1L)
  table3 <- s
  expect_match(apa.quarto.template(table3, label = "tbl-chosen", code = "x"),
               "#\\| label: tbl-chosen")
})

test_that("two tables give two labels, so both chunks can be pasted", {
  s <- apa_spec_start(1) |>
    apa_col("Var", "l", list("A")) |>
    apa_spec_finish(title = "T", number = 1L)
  first_table <- s
  second_table <- s
  a <- apa.quarto.template(first_table, code = "x")
  b <- apa.quarto.template(second_table, code = "x")
  lab <- function(txt) sub(".*#\\| label: ([^\n]+)\n.*", "\\1", txt)
  expect_false(identical(lab(a), lab(b)))
  expect_equal(lab(a), "tbl-first-table")
  expect_equal(lab(b), "tbl-second-table")
})

test_that("apa.quarto.template no longer writes apa-longtable, tall table or not", {
  # There is no height model: the template asks for nothing, and the
  # property worth pinning is that ABSENCE: a tall table and a short one
  # produce the same chunk options, because the extension reads the
  # splittable grid off the LaTeX and needs no attribute.
  set.seed(7)
  grp <- factor(rep(paste0("Group", sprintf("%02d", 1:30)), each = 5))
  dv <- stats::rnorm(150, 10, 2)
  tall <- apa.1way.table(iv = grp, dv = dv, data = data.frame(dv, grp))
  out <- capture.output(txt <- apa.quarto.template(tall, code = "tall"))
  expect_no_match(txt, "apa-longtable", fixed = TRUE)
  joined <- paste(out, collapse = "\n")
  expect_no_match(joined, "inches tall", fixed = TRUE)
  # and no advice about the counter or journal mode, which would describe a
  # chunk option the template no longer writes
  expect_no_match(joined, "documentmode: jou", fixed = TRUE)

  grp <- factor(rep(paste0("Group", sprintf("%02d", 1:10)), each = 5))
  dv <- stats::rnorm(50, 10, 2)
  short <- apa.1way.table(iv = grp, dv = dv, data = data.frame(dv, grp))
  out2 <- capture.output(txt2 <- apa.quarto.template(short, code = "short"))
  expect_no_match(txt2, "apa-longtable", fixed = TRUE)
  # the two chunks differ only in their label and caption, not in their options
  opts <- function(t) grep("^#[|]", strsplit(t, "\n")[[1]], value = TRUE)
  expect_identical(sub("tall", "short", opts(txt)), opts(txt2))
})

test_that("apa.quarto.template writes no markers, wide table or not", {
  # There are no ::: {.landscape} markers to suppress, in either direction,
  # with or without `landscape = FALSE` -- the template states one fact once
  # and apaquarto acts on it.
  blk1 <- lm(sales ~ adverts + airplay, data = album)
  blk2 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data = album)
  flat <- apa.reg.table(blk1, blk2, landscape = FALSE)
  out <- capture.output(txt <- apa.quarto.template(flat, code = "flat"))
  expect_no_match(txt, ":::", fixed = TRUE)
  wide <- apa.reg.table(blk1, blk2)
  out2 <- capture.output(txt2 <- apa.quarto.template(wide, code = "wide"))
  expect_no_match(txt2, ":::", fixed = TRUE)
})
