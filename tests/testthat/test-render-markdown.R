test_that("a flat table emits an aligned grid whose rule lines all match", {
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude[, 1:3])))
  md <- apa_markdown_table(plan)
  lines <- strsplit(md, "\n", fixed = TRUE)[[1]]

  rules <- grep("^[+]", lines)
  expect_gt(length(rules), 2)
  widths <- unique(nchar(lines[rules]))
  expect_length(widths, 1)

  # Every content row is exactly as wide as the rules. This is the check that
  # catches the formatC padding trap; without it the failure is silent.
  content <- grep("^[|]", lines)
  expect_true(all(nchar(lines[content]) == widths))
})

test_that("a row-group label longer than the natural table width widens the columns, not truncated", {
  # Hand-built (mirroring tests/testthat/test-render-latex.R:149's pattern):
  # no shipped builder yet routes a very long label into a row group, but
  # apa_markdown_table() already renders group rows with merged-width logic
  # (R/render-markdown.R), and apa.reg.table() -- exercised by Task 2's
  # round-trip tests -- does use row groups. A group row spans every column,
  # so its label is a constraint on the TOTAL width (sum(w) + (n_col - 1)),
  # not on any one column; a label that overflows that must widen the
  # columns, never truncate.
  spec <- apa_spec_start(2L)
  spec <- apa_col(spec, label = "Variable", align = "l",
                  values = list("row one", "row two"))
  spec <- apa_col(spec, label = "M", align = "r", values = list("1.00", "2.00"))
  spec <- apa_row_group(
    spec,
    label = "A row-group label that is far longer than the natural width of this narrow two-column table",
    rows = 1:2
  )
  plan <- apa_display_plan(apa_spec_finish(spec))
  md <- apa_markdown_table(plan)
  lines <- strsplit(md, "\n", fixed = TRUE)[[1]]

  rules <- grep("^[+]", lines)
  expect_gt(length(rules), 2)
  widths <- unique(nchar(lines[rules]))
  expect_length(widths, 1)

  content <- grep("^[|]", lines)
  expect_true(all(nchar(lines[content]) == widths))

  # The label itself must survive intact somewhere in the rendered table --
  # this is the "never truncate" half of the requirement, which the rule/
  # content width-equality check above cannot see on its own (a truncated
  # label that still happens to fit the (wrong) width would pass it).
  expect_true(any(grepl("far longer than the natural width", lines, fixed = TRUE)))
})

# Parse an emitted table back through pandoc and report its true shape. The
# grid table's failure mode is SILENT AND TOTAL -- a column too narrow for its
# content collapses the whole table into one column -- so nothing else in the
# suite can see it. The parsing itself lives in R/render-markdown.R
# (apa_md_roundtrip(), internal but reusable by later tasks' tests); this
# thin wrapper just adds the testthat skips so a missing pandoc skips rather
# than errors.
roundtrip <- function(md) {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  apa_md_roundtrip(md)
}

test_that("apa_md_count_matches() reports zero on zero matches", {
  # gregexpr()'s length is NOT the match count: on zero matches it returns
  # -1L (a length-1 vector), so a naive length(gregexpr(p, txt)[[1]]) reports
  # 1, not 0. A total grid collapse severe enough that pandoc parses no Table
  # node at all would then read as n_col == 1 -- which can match a genuine
  # one-column expectation and pass silently, the one shape of collapse
  # apa_md_roundtrip() would otherwise be unable to see.
  expect_equal(apa_md_count_matches("ColWidth", "no match here"), 0L)
  expect_equal(apa_md_count_matches("ColWidth", "ColWidth ColWidth"), 2L)
  expect_equal(apa_md_count_matches("\\bRow\\b", "RowSpan 2 1"), 0L)
  expect_equal(apa_md_count_matches("\\bRow\\b", "Row ["), 1L)
})

test_that("every builder's table round-trips to the expected shape", {
  specs <- list(
    cor  = apa.cor.table(attitude),
    wide = apa.cor.table(mtcars),
    reg  = apa.reg.table(lm(mpg ~ disp, mtcars), lm(mpg ~ disp + hp, mtcars)),
    # A flat plan never carries rule-div markup after this revision (D34/D35)
    # -- the spanned plan is now the SOLE carrier of that constant-overhead
    # source in the width model (apa_md_span_widen()'s ApaTableRule wrap on
    # the spanner cells), so the total-collapse invariant must watch this
    # path directly, not just the flat ones above.
    spanned = apa.2way.table(
      iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles,
      show.conf.interval = FALSE, show.marginal.means = TRUE)
  )
  for (nm in names(specs)) {
    plan <- apa_display_plan(as_spec(specs[[nm]]))
    rt <- roundtrip(apa_markdown_table(plan))
    expect_equal(rt$n_col, plan$n_col, info = nm)
    # ROWS TOO, not columns alone. The collapse this guards against destroys
    # both, but a width bug that merged two BODY rows while leaving the column
    # count intact would pass a columns-only check.
    expect_equal(rt$n_row, length(plan$header) + length(plan$body), info = nm)
  }
})

test_that("a spanner survives as a real column span, at its TOP-LEVEL width", {
  # whole-branch-review Important 5: this fixture's own plan$header shows
  # level-1 (top-level) spans of 1, 6, 1, 1 -- the "6" is the real spanner
  # this test is named for. Level-2 SUB-spanners underneath it are span 2
  # each, which is where "ColSpan 2" actually comes from. The old assertion
  # (`ColSpan 2 || ColSpan 3`) could never observe the top-level spanner at
  # all: degrading the 6-column spanner into six ordinary, unspanned cells
  # would still leave the level-2 "ColSpan 2" cells intact, so that mutation
  # passes both this test AND the round-trip invariant. Assert the top-level
  # span width directly.
  plan <- apa_display_plan(as_spec(apa.2way.table(
    iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles,
    show.conf.interval = FALSE, show.marginal.means = TRUE)))
  expect_equal(vapply(plan$header[[1]]$cells, function(c) c$span, numeric(1)),
               c(1, 6, 1, 1))
  md <- apa_markdown_table(plan)
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  f <- tempfile(fileext = ".md"); writeLines(md, f)
  out <- tempfile(); rmarkdown::pandoc_convert(f, to = "native", output = out)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl("ColSpan 6", txt, fixed = TRUE))
  # The level-2 sub-spanners are still real column spans too, not merely a
  # side effect of the top-level one.
  expect_true(grepl("ColSpan 2", txt, fixed = TRUE))
})

test_that("a pipe in a variable name does not break the grid", {
  d <- attitude[, 1:3]
  names(d)[1] <- "a|b"
  plan <- apa_display_plan(as_spec(apa.cor.table(d)))
  rt <- roundtrip(apa_markdown_table(plan))
  expect_equal(rt$n_col, plan$n_col)
})

test_that("every tricky name round-trips to the right shape", {
  for (nm in tricky_names) {
    d <- attitude[, 1:3]
    names(d)[1] <- nm
    plan <- apa_display_plan(as_spec(apa.cor.table(d)))
    rt <- roundtrip(apa_markdown_table(plan))
    expect_equal(rt$n_col, plan$n_col, info = nm)
  }
})

test_that("a spanner much longer than its columns still round-trips, and the grid stays aligned", {
  # Watches the spanner half of Task 1's known gap: apa_md_widths() measured
  # only the last header row and "data" body rows, so a spanner label (a
  # header cell at a HIGHER level, span > 1) was never checked against the
  # width it needs. A spanner label far longer than the narrow columns
  # beneath it must widen those columns, not collapse the grid.
  spec <- apa_spec_start(2L)
  spec <- apa_col(spec, label = "V", align = "l", values = list("a", "b"))
  spec <- apa_col(spec, label = "M", align = "r", values = list("1", "2"))
  spec <- apa_col(spec, label = "SD", align = "r", values = list("1", "2"))
  spec <- apa_spanner(spec, label = "A spanner label far longer than the two narrow columns it covers",
                       cols = 2:3)
  plan <- apa_display_plan(apa_spec_finish(spec))
  md <- apa_markdown_table(plan)
  lines <- strsplit(md, "\n", fixed = TRUE)[[1]]

  rules <- grep("^[+]", lines)
  widths <- unique(nchar(lines[rules]))
  expect_length(widths, 1)

  content <- grep("^[|]", lines)
  expect_true(all(nchar(lines[content]) == widths))

  expect_true(any(grepl("far longer than the two narrow columns", lines, fixed = TRUE)))

  rt <- roundtrip(md)
  expect_equal(rt$n_col, plan$n_col)
})

test_that("a single-column spanner does not corrupt the grid (Phase 1.7 Task 8)", {
  # apa_display_plan() gives a spanner covering exactly ONE column a span-1
  # header cell that is still recorded in that row's `rules` (R/display-
  # plan.R: span = length(hit$cols) = 1L, rules[[...]] <- c(min, max) =
  # c(2, 2) here) -- so apa_markdown_table() still wraps its label in the
  # "::: {custom-style=\"ApaTableRule\"}" rule div (apa_md_in_ranges() tests
  # column membership, never span). Before this fix, apa_md_span_widen()
  # guarded on `span > 1L` alone, so it never budgeted width for that wrap on
  # a span-1 spanner -- the 34-character fence line then outgrew its
  # unbudgeted column and desynced the whole grid. No shipped builder can
  # reach this today (apa_spanner() is unexported), so it is constructed
  # directly here, with `cols = 2L` (not `cols = c(2)`): apa_display_plan()'s
  # own contiguity check compares `sort(sp$cols)` against `seq.int(...)` with
  # `identical()`, which is FALSE for a double against seq.int()'s integer
  # result even when the values agree -- an unrelated, pre-existing gap this
  # task does not touch, worked around here by passing an integer.
  spec <- apa_spec_start(2L)
  spec <- apa_col(spec, label = "V", align = "l", values = list("a", "b"))
  spec <- apa_col(spec, label = "M", align = "r", values = list("1", "2"))
  spec <- apa_col(spec, label = "SD", align = "r", values = list("1", "2"))
  spec <- apa_spanner(spec, label = "Solo", cols = 2L)
  plan <- apa_display_plan(apa_spec_finish(spec))
  md <- apa_markdown_table(plan)
  lines <- strsplit(md, "\n", fixed = TRUE)[[1]]

  # The grid-format invariant itself: every rule and content line the same
  # total width. Measured RED before the fix: two distinct widths (17 and
  # 44 on this exact fixture) because the rule-div fence line ran past its
  # unbudgeted column.
  widths <- unique(nchar(lines[grepl("^[+|]", lines)]))
  expect_length(widths, 1)

  # The column count itself, via a real pandoc parse -- what the brief asks
  # for directly. Measured RED before the fix: pandoc reads the desynced "|"
  # positions as literal text and reports 4 columns from this 3-column plan
  # (the same shape as the brief's own "8 columns from a 7-column plan").
  rt <- roundtrip(md)
  expect_equal(rt$n_col, plan$n_col)
})

test_that("D55 regression guard: a flattened spanner label does not desync the grid's line widths", {
  # I3 (Task 7 review): D55's own guard (test-slide-render.R) is quarto-only
  # -- skip_on_cran() + the quarto check + apa_skip_if_library_stale() --
  # so on CRAN, or any machine without quarto, a regression here would ship
  # silently. This test needs no render at all.
  #
  # apa_slide_flatten_spanners() (R/quarto-slide.R, pptx only) used to build
  # this fixture directly; Phase 1.9 (D88/D90) DELETED that function along
  # with the rest of the pptx machinery, because PowerPoint left the Quarto
  # path entirely. But the DEFECT this test guards is not about pptx at
  # all -- it is about apa_markdown_table()'s own width model, which must
  # measure ANY span == 1 cell in a NON-final header row, wherever such a
  # cell comes from. So the minimal shape that once triggered it is
  # reconstructed here, by hand, without depending on the deleted function
  # -- its logic is recorded verbatim in
  # dev/plans/2026-07-31-roadmap.md under Phase 1.13 if a future reader
  # needs the original.
  #
  # Before D55, apa_md_widths() measured only the LAST header row and data
  # rows, and apa_md_span_widen() only span > 1 cells -- a flattened label
  # fell between both and got no width guarantee. A grid table's own format
  # invariant is that every line (every rule and every content row) is the
  # SAME total width; a column one character too narrow for its content
  # breaks that invariant in the EMITTED TEXT ITSELF, before pandoc ever
  # sees it -- measured on this exact plan: line widths of 76 and 77 (two
  # distinct values, i.e. ragged) before the fix, a uniform 83 after it.
  plan <- apa_display_plan(as_spec(apa.2way.table(
    iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles,
    show.conf.interval = FALSE, show.marginal.means = TRUE)))
  plan$header <- lapply(plan$header, function(row) {
    cells <- list()
    for (cell in row$cells) {
      n <- as.integer(cell$span)
      if (n <= 1L) {
        cells[[length(cells) + 1L]] <- cell
      } else {
        mid <- (n - 1L) %/% 2L
        for (k in seq_len(n) - 1L) {
          cells[[length(cells) + 1L]] <- if (identical(k, mid)) {
            list(rt = cell$rt, span = 1L, align = cell$align)
          } else {
            list(rt = richtext(""), span = 1L, align = cell$align)
          }
        }
      }
    }
    list(cells = cells, rules = list())
  })
  md <- apa_markdown_table(plan, font_size = NULL, rules = TRUE)
  lines <- strsplit(md, "\n", fixed = TRUE)[[1]]
  expect_length(unique(nchar(lines)), 1L)
})

# ---- Task 3: style hooks (font_size / rules become live) -------------------

test_that("a font size wraps every cell in the matching character style", {
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude[, 1:3])))
  md <- apa_markdown_table(plan, font_size = 9L)
  expect_true(grepl('custom-style="ApaTable9"', md, fixed = TRUE))
  expect_false(grepl('custom-style="ApaTable10"', md, fixed = TRUE))
  # still a valid grid after the wrapping widened every cell
  lines <- strsplit(md, "\n", fixed = TRUE)[[1]]
  widths <- unique(nchar(lines[grep("^[+|]", lines)]))
  expect_length(widths, 1)
})

test_that("no per-cell markup at the default size", {
  # D35: the default emission puts nothing on ordinary cells -- 10 pt comes
  # from the reference document's Table style, not a per-cell span, and the
  # last header row's rule comes from that same style, not a rule div. Before
  # this revision, rules defaulted to TRUE and wrapped every cell of the last
  # header row in "::: {custom-style=\"ApaTableRule\"}", which also contains
  # "custom-style=" -- so this assertion is RED against the pre-revision code.
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude[, 1:3])))
  md <- apa_markdown_table(plan)
  expect_false(grepl("custom-style=", md, fixed = TRUE))
})

test_that("the width model no longer charges every column a constant rule-div overhead", {
  # NOT a test of the font-size span (D34/D35's 3.83:1 -> 1.28:1 collapse) --
  # with font_size = NULL (this test's default call, and apa_markdown_table()'s
  # own default) that span is never emitted, so it cannot be what this
  # assertion discriminates. What it actually watches is the OTHER constant
  # overhead the pre-revision code charged every column: apa_md_widths()
  # used to rule-divide every cell of the last header row regardless of
  # font_size, and that ~34-character fence line, applied identically to
  # every column via floor_w, flattened the ratio to exactly 1.0 (watched
  # directly, RED, before this revision's fix -- narrower even than D34's
  # combined 1.28:1, because here it is the rule overhead alone). Assert the
  # SHAPE of the width model, observable without a render: the widest column
  # must be at least 2.5x the narrowest.
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude[, 1:4])))
  w <- apa_md_widths(plan)
  expect_gt(max(w) / min(w), 2.5)
})

test_that("a spanner still gets its cmidrule, and the header row does NOT", {
  # D34: apaquarto's Table style already draws a continuous rule under the
  # LAST header row for both a one-row and a three-row header, so the emitter
  # must not draw a second, segmented one there -- but D29's partial
  # (cmidrule) rule under a spanner is still the emitter's job, since a table
  # style has no notion of "only some columns". Before this revision the flat
  # table's assertion was RED: apa_markdown_table() wrapped every cell of the
  # last header row regardless of spanners.
  flat <- apa_display_plan(as_spec(apa.cor.table(attitude[, 1:3])))
  expect_false(grepl("ApaTableRule", apa_markdown_table(flat), fixed = TRUE))
  spanned <- apa_display_plan(as_spec(apa.2way.table(
    iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles,
    show.conf.interval = FALSE, show.marginal.means = TRUE)))
  expect_true(grepl("ApaTableRule", apa_markdown_table(spanned), fixed = TRUE))
})

test_that("rules = FALSE suppresses a spanner's cmidrule too", {
  # The other half of "rules gates the rule divs". A flat table no longer
  # discriminates rules = TRUE vs FALSE after this revision (neither wraps
  # its last header row), so this must use a spanned table, where rules = TRUE
  # is known (the test above) to emit ApaTableRule.
  plan <- apa_display_plan(as_spec(apa.2way.table(
    iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles,
    show.conf.interval = FALSE, show.marginal.means = TRUE)))
  md <- apa_markdown_table(plan, rules = FALSE)
  expect_false(grepl("ApaTableRule", md, fixed = TRUE))
})

test_that("a rule-divided spanner cell parses as a real pandoc Div, not literal fence text", {
  # Watches the trap this task actually hit: apa_md_row() glues every
  # non-final line of a multi-line cell together with a trailing "\\" hard
  # break, which is correct for a stacked r/CI/p cell but WRONG for a
  # fenced div -- a "\\" keeps the ":::" marker inside the same inline
  # paragraph as its neighbours, so pandoc never recognises it as a block
  # boundary and reads it back as the literal three characters ":::"
  # instead of a Div node. Caught directly: before apa_md_is_fence_line()
  # was added, this round-tripped as ColSpan 7 (the whole header row
  # collapsed into one cell) with `Str ":::"` in the dump. Uses a spanned
  # plan (not a flat one) because this revision no longer rule-divides a
  # flat table's header row at all.
  plan <- apa_display_plan(as_spec(apa.2way.table(
    iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles,
    show.conf.interval = FALSE, show.marginal.means = TRUE)))
  md <- apa_markdown_table(plan, rules = TRUE)
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  f <- tempfile(fileext = ".md"); writeLines(md, f)
  out <- tempfile(); rmarkdown::pandoc_convert(f, to = "native", output = out)
  # pandoc's native pretty-printer wraps a Div's constructor arguments onto
  # their own line, so collapse all whitespace runs to a single space before
  # matching -- otherwise a `fixed = TRUE` pattern written on one line can
  # never match text pandoc itself split across two.
  txt <- gsub("\\s+", " ", paste(readLines(out, warn = FALSE), collapse = " "))
  expect_true(grepl('Div ( "" , [] , [ ( "custom-style" , "ApaTableRule" )',
                    txt, fixed = TRUE))
  expect_false(grepl('Str ":::"', txt, fixed = TRUE))
})

test_that("the rule div covers only the spanned columns, not the filler cell beside a spanner", {
  # D29's partial (cmidrule) rule: a spanner's own header row must NOT wrap
  # every cell in that row, only the columns plan$header[[i]]$rules names --
  # otherwise a spanner sitting beside an unspanned column (as "alcohol"
  # sits beside "gender" here) would draw a rule under "gender" too, which
  # is not what a cmidrule means.
  plan <- apa_display_plan(as_spec(apa.2way.table(
    iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles,
    show.conf.interval = FALSE, show.marginal.means = TRUE)))
  md <- apa_markdown_table(plan, rules = TRUE)
  lines <- strsplit(md, "\n", fixed = TRUE)[[1]]
  # Lines 2:4 are the top spanner level's three physical rows (the fence
  # open, the "alcohol" label, the fence close) -- one spanner covering
  # columns 2:7 over column 1's blank, unspanned filler.
  top_block <- paste(lines[2:4], collapse = "\n")
  n_rule <- lengths(regmatches(top_block,
                               gregexpr('custom-style="ApaTableRule"', top_block)))
  expect_equal(n_rule, 1L)
})

test_that("a numbered row label survives as literal text, not an auto-detected ordered list", {
  # apa.cor.table() numbers row labels "1. name", "2. name", ... Pandoc's
  # markdown reader treats an unescaped "N. " at the start of a table
  # cell's content as an ORDERED LIST marker, not literal text -- measured
  # directly against the bundled reference document: the cell's paragraph
  # kept style "Compact" (already firstLine=0 in stock apaquarto), but
  # gained a real <w:numPr> and the "1." vanished into the list marker,
  # leaving indentation that comes from the LIST's own indent in
  # numbering.xml, invisible to any paragraph-style fix. This asserts the
  # escape at the only place observable before Task 10's rendered-docx
  # fixtures exist: the parsed pandoc AST must hold a Plain/Para run
  # "1. rating", never an OrderedList.
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude[, 1:3])))
  md <- apa_markdown_table(plan)
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  f <- tempfile(fileext = ".md"); writeLines(md, f)
  out <- tempfile(); rmarkdown::pandoc_convert(f, to = "native", output = out)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_false(grepl("OrderedList", txt, fixed = TRUE))
  expect_true(grepl('Str "1."', txt, fixed = TRUE))
})

test_that("the bundled reference document defines every style the emitter names", {
  ref <- system.file("_extensions/wjschne/apaquarto/apaquarto.docx",
                     package = "apaTables")
  skip_if(ref == "")
  tmp <- tempfile(); dir.create(tmp)
  utils::unzip(ref, files = "word/styles.xml", exdir = tmp)
  styles <- paste(readLines(file.path(tmp, "word", "styles.xml"), warn = FALSE),
                  collapse = "")
  for (s in c(paste0("ApaTable", c(10, 9, 8, 7, 6)), "ApaTableRule")) {
    expect_true(grepl(paste0('w:styleId="', s, '"'), styles, fixed = TRUE), info = s)
  }
})

test_that("the Table style carries the 10 pt run properties", {
  # D35: 10 pt is a property of the reference document's Table TABLE style
  # (a <w:rPr> there applies to every run in the table), not per-cell markup.
  # Before Step 1's docx edit this is RED -- the Table style carries no
  # <w:rPr> at all.
  ref <- system.file("_extensions/wjschne/apaquarto/apaquarto.docx",
                     package = "apaTables")
  skip_if(ref == "")
  tmp <- tempfile(); dir.create(tmp)
  utils::unzip(ref, files = "word/styles.xml", exdir = tmp)
  s <- paste(readLines(file.path(tmp, "word", "styles.xml"), warn = FALSE),
             collapse = "")
  tbl <- regmatches(s, regexpr('<w:style w:type="table"[^>]*w:styleId="Table">.*?</w:style>', s))
  expect_match(tbl, '<w:sz w:val="20"/>', fixed = TRUE)
})

# ---- Task 4: HTML -- measure what apaquarto already styles ----------------
#
# AMENDED (D35): after the Task 3 revision, the default markdown emission
# carries NO per-cell custom-style at all, so a "data-custom-style=
# \"ApaTable10\"" assertion (the brief's original Step 2) has no subject and
# would fail for a reason that is not a defect -- it is simply never emitted.
# The only custom-style pandoc's HTML writer still sees is "ApaTableRule",
# and only on spanner header cells (D29's cmidrule), and only when the table
# has spanners.
#
# Measured directly (D37, reported to the controller): rendering a real
# apaTables grid table through `quarto render` with format apaquarto-html
# shows apaquarto's OWN .table CSS already supplies every ordinary-table
# furniture rule with NO per-cell markup at all -- a rule above the header,
# a rule under the last header row (one-row and multi-row headers alike), a
# rule under the last body row, no vertical rules, and no interior
# horizontal rules between body rows. What apaquarto's CSS does NOT do is
# style the cmidrule div: before apa.css gained the
# "[data-custom-style=\"ApaTableRule\"]" rule below, a spanner's fenced div
# rendered as a plain, unstyled <div data-custom-style="ApaTableRule"> with
# no visible border at all. That is the one thing this task adds.

test_that("a spanner's cmidrule survives pandoc's HTML writer as a data-custom-style hook", {
  # The narrow, emitter-side half of the measurement -- what markup
  # apa_markdown_table() itself produces -- checked with bare pandoc, per
  # the brief's own caveat that a bare pandoc_convert() cannot speak to
  # apaquarto's CSS (only `quarto render` loads the extension's stylesheet).
  #
  # to = "html5", NOT the brief's "html". Watched directly with
  # rmarkdown::pandoc_convert(..., verbose = TRUE): to = "html" resolves to
  # pandoc's `--to html4` writer, which passes an unrecognised div attribute
  # through UNPREFIXED ("custom-style=", no "data-") -- only the html5 writer
  # adds the "data-" prefix that both apaquarto's CSS selector and Quarto's
  # own apaquarto-html output (confirmed by a real `quarto render`, which
  # targets html5) rely on. The brief's own Step 2 code, run as written,
  # would never see a match and would misreport this as a missing hook.
  plan <- apa_display_plan(as_spec(apa.2way.table(
    iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles,
    show.conf.interval = FALSE, show.marginal.means = TRUE)))
  md <- apa_markdown_table(plan)
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  f <- tempfile(fileext = ".md"); writeLines(md, f)
  out <- tempfile(fileext = ".html")
  rmarkdown::pandoc_convert(f, to = "html5", output = out)
  html <- paste(readLines(out, warn = FALSE), collapse = "")
  expect_true(grepl('data-custom-style="ApaTableRule"', html, fixed = TRUE))
  # D35: no font-size hook survives to HTML either, at the default call --
  # confirms the flat-table assertion above ("no per-cell markup at the
  # default size") also holds for a SPANNED table and for pandoc's HTML
  # writer specifically, not only for the raw markdown source.
  expect_false(grepl('data-custom-style="ApaTable[0-9]', html))
})

test_that("a flat table's HTML carries no custom-style hook at all", {
  # The other half of the same measurement: apa.cor.table() has no
  # spanners, so at the default call NOTHING in its HTML should carry
  # custom-style -- the ordinary rules (top/header/bottom, no verticals, no
  # interior rules) come entirely from apaquarto's existing .table CSS,
  # which targets the <table> structure, never a per-cell attribute.
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude[, 1:3])))
  md <- apa_markdown_table(plan)
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  f <- tempfile(fileext = ".md"); writeLines(md, f)
  out <- tempfile(fileext = ".html")
  rmarkdown::pandoc_convert(f, to = "html5", output = out)
  html <- paste(readLines(out, warn = FALSE), collapse = "")
  expect_false(grepl("data-custom-style", html, fixed = TRUE))
})

test_that("the bundled apa.css draws a rule under a spanner's custom-style hook", {
  # The CSS half of the measurement. Reading the bundled stylesheet mirrors
  # the docx style tests above (system.file() resolves into the source
  # tree's inst/ under pkgload::load_all()). Before this task's edit this is
  # RED -- watched directly: apa.css had no selector for
  # "[data-custom-style=\"ApaTableRule\"]" at all, so a spanner's cmidrule
  # div rendered with no border in a real quarto render (see the report for
  # the rasterized before/after).
  ref <- system.file("_extensions/wjschne/apaquarto/apa.css",
                     package = "apaTables")
  skip_if(ref == "")
  css <- paste(readLines(ref, warn = FALSE), collapse = "\n")
  rule <- regmatches(css, regexpr('\\[data-custom-style="ApaTableRule"\\][^{]*\\{[^}]*\\}', css))
  expect_length(rule, 1)
  expect_true(grepl("border", rule, fixed = TRUE))
  # No font-size ladder is added: HTML has no page, so rotation/shrinking are
  # not applicable there (the spec's own words), and the emitter never wraps
  # an ordinary cell in an ApaTable<N> span by default (D35) -- so nothing
  # would ever match a "[data-custom-style^=\"ApaTable1\"]"-style selector in
  # the HTML this task's own tests produce. Adding CSS for a hook the
  # emitter never emits would be untestable dead weight in the bundle.
  expect_false(grepl('ApaTable1', css, fixed = TRUE))
})

# ---- Task 5: destination policy, rotation decision, Route A shrink ladder --
#
# D38 (measured in real Word, see task-5-spike.md): the ladder is FIVE TABLE
# STYLES selected by a Lua filter, not per-cell ApaTableN spans. Per-cell
# spans are Route B, measured to crush a real table's column ratio from
# 10.01:1 to 2.12:1 and break Organizational_Citizenship_Behavior mid-word --
# exactly the defect D35 revised Task 3 to remove. So apa_markdown_table() is
# NEVER handed a font size on the Quarto policy path
# (apa_quarto_markdown_block()); its font_size parameter is exercised
# directly by the Task 3 tests above and stays available for other callers
# (e.g. apa.save()), but this task's own tests must watch for the ABSENCE of
# per-cell markup on the policy path, not its presence.

test_that("a wide table asks for rotation and a narrow one does not", {
  narrow <- apa_display_plan(as_spec(apa.cor.table(attitude[, 1:3])))
  wide   <- apa_display_plan(as_spec(apa.cor.table(mtcars)))
  expect_false(apa_layout_decide(narrow)$rotate)
  expect_true(apa_layout_decide(wide)$rotate)
})

test_that("font.size overrides the ladder and warns below the readable floor", {
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude[, 1:3])))
  expect_warning(apa_quarto_font_size(plan, font.size = 5L), "readable")
})

test_that("an explicit font.size at or above the readable floor is honoured with no warning", {
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude[, 1:3])))
  expect_equal(apa_quarto_font_size(plan, font.size = 8L), 8L)
  expect_silent(apa_quarto_font_size(plan, font.size = 8L))
})

test_that("with no explicit font.size, the ladder's own decision is used", {
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude[, 1:3])))
  expect_equal(apa_quarto_font_size(plan), APA_QUARTO_DEFAULT_SIZE)
})

test_that("a table that fits at 10 pt gets no size markup and no style override at all", {
  block <- apa_quarto_markdown_block(apa.cor.table(attitude[, 1:3]))
  body <- as.character(block)
  expect_false(grepl("custom-style=", body, fixed = TRUE))
  expect_false(grepl("apa-table-size-", body, fixed = TRUE))
})

test_that("a shrunken table gets a Route A style marker, never a per-cell span, and keeps its column proportions", {
  # RED before implementation for two independent reasons: neither
  # apa_quarto_markdown_block() nor the marker text exists yet.
  #
  # apa_quarto_docx_target() mocked TRUE (whole-branch-review Critical 1):
  # the size marker is a docx-only mechanism (apatablesize.lua stands down
  # on any non-docx FORMAT, and no `apa-table-size-*` CSS exists for HTML,
  # D37), so this specifically docx-flavoured ("Route A style marker")
  # assertion must exercise the docx gate rather than fall through the "no
  # real knit session" default of FALSE.
  local_mocked_bindings(apa_quarto_docx_target = function() TRUE)
  block <- apa_quarto_markdown_block(apa.cor.table(attitude[, 1:3]), font.size = 8L)
  body <- as.character(block)

  # Route A, not Route B: no per-cell ApaTable8 span anywhere...
  expect_false(grepl('custom-style="ApaTable8"', body, fixed = TRUE))
  # ...but a marker naming the size for apatablesize.lua to read.
  expect_true(grepl("apa-table-size-8", body, fixed = TRUE))

  # Column proportions undisturbed: apa_markdown_table() must have been
  # called with font_size = NULL underneath, so the grid's own column widths
  # are identical to the unshrunk table's -- the whole point of D38. A
  # per-column constant-overhead span (Route B) would flatten this ratio
  # toward 1:1, which is the defect this route exists to avoid (measured
  # 3.83:1 -> 1.28:1 on a real table in the task-5 spike).
  lines <- strsplit(body, "\n", fixed = TRUE)[[1]]
  rule <- lines[grepl("^[+]", lines)][1]
  segs <- strsplit(rule, "[+]", fixed = FALSE)[[1]]
  segs <- segs[nzchar(segs)]
  widths <- nchar(segs)
  expect_gt(max(widths) / min(widths), 2.5)
})

test_that("a shrunken table's grid still round-trips through pandoc as one real table", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc not available")
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude[, 1:3])))
  block <- apa_quarto_markdown_block(apa.cor.table(attitude[, 1:3]), font.size = 8L)
  rt <- apa_md_roundtrip(as.character(block))
  expect_equal(rt$n_col, plan$n_col)
})

test_that("apa_landscape_wrap() marks its body with apaTables' own class, carrying real page geometry", {
  # D31: Quarto's own `.landscape` div for docx closes with an EMPTY sectPr,
  # silently stripping the running head and page number from itself and
  # every page before it, and its rotated size is hardcoded A4 -- so this
  # must be a class Quarto's own docx pipeline does not already act on. Unit
  # test of the wrap function itself.
  wrapped <- apa_landscape_wrap("BODY")
  expect_true(grepl("apa-table-landscape", wrapped, fixed = TRUE))
  # RENAMED (Phase 1.7 Task 8), was: "apa_landscape_wrap() marks its body
  # with apaTables' own class, not Quarto's .landscape div", asserting
  # `expect_false(grepl("{.landscape}", wrapped, fixed = TRUE))` as proof of
  # the "not Quarto's div" half. That literal four-character form -- brace,
  # dot, "landscape", brace, with NOTHING between "landscape" and the closing
  # brace -- can never appear in this function's output, even in the exact
  # regression the assertion was written to catch: apa_landscape_wrap()
  # (R/quarto-table.R) always appends the real page-geometry attributes
  # (pgw=/pgh=/mt=/...) between the class name and the closing brace, in the
  # SAME paste0() call that writes the class, so reverting
  # APA_QUARTO_LANDSCAPE_CLASS to Quarto's own "landscape" (the actual D31
  # regression) would emit `{.landscape pgw="12240" ...}`, never the bare
  # `{.landscape}` this checked for -- confirmed directly by simulating that
  # exact reversion and reading `grepl("{.landscape}", ..., fixed = TRUE)` as
  # FALSE against it too. Kept (not deleted, per the standing rule for an
  # assertion that cannot observe its own subject) as what it actually is: a
  # trivial format check on the wrapper's own known-good attrs, which any
  # geometry this function is ever handed will satisfy.
  expect_false(grepl("{.landscape}", wrapped, fixed = TRUE))
  expect_true(grepl("BODY", wrapped, fixed = TRUE))
  # The div also carries the real page geometry apatablelandscape.lua needs
  # (Task 6) -- read out of the bundled reference document, not any of these
  # exact numbers assumed here beyond what the reference document is known
  # to be (US Letter, 1 in margins -- D31; measured in
  # dev/quarto-landscape-sectpr-issue.md; the patch that carries it is
  # apaquarto fork commit 125cbe4 on branch apatables-bundle).
  expect_true(grepl('pgw="12240"', wrapped, fixed = TRUE))
  expect_true(grepl('pgh="15840"', wrapped, fixed = TRUE))
})

test_that("apa_quarto_markdown_block() rotates a table wide enough to want it, on a docx render", {
  # Task 6 makes apa_quarto_markdown_block()'s can_execute_rotation
  # apa_quarto_docx_target() -- MOCKED to TRUE here, matching this file's
  # own established convention (apa_render_metadata() is mocked the same
  # way, never a knitr internal directly), because this is a plain testthat
  # call with no real knit session behind it to report a pandoc target at
  # all. mtcars' correlation table does not fit the upright 6.5 in budget
  # at 10 pt (measured: apa_layout_decide(plan, can_execute_rotation =
  # FALSE) shrinks it to ~7.65 pt instead) but DOES fit the wider rotated
  # budget at the full 10 pt (D7, rotate before shrink) -- so on a docx
  # render the public function must rotate it, at the default size, with no
  # shrink marker at all. The companion test just below proves the OTHER
  # half: an HTML render (apa_quarto_docx_target() mocked FALSE) must NOT
  # rotate this same table.
  local_mocked_bindings(apa_quarto_docx_target = function() TRUE)
  block <- apa_quarto_markdown_block(apa.cor.table(mtcars))
  body <- as.character(block)
  expect_true(grepl("apa-table-landscape", body, fixed = TRUE))
  expect_false(grepl("apa-table-size-", body, fixed = TRUE))
})

test_that("apa_quarto_markdown_block() does NOT rotate, and does NOT mark a shrink, on a non-docx render (fix round 1 + whole-branch-review Critical 1)", {
  # The gate finding 4 of the fix-round-1 review exists for. Before that
  # fix, can_execute_rotation was TRUE unconditionally -- correct for docx
  # but silently wrong for html, which apa_quarto_markdown_block() also
  # serves (D24) and which apatablelandscape.lua's own `if FORMAT ~=
  # "docx" then return end` guard stands down for entirely: an HTML render
  # would have priced mtcars against the WIDER rotated budget, emitted it
  # at the full 10 pt with a dead, unprocessed `.apa-table-landscape` div,
  # and never actually rotated -- the same shape of mispricing Phase 2's
  # whole-branch review found on the LaTeX path before
  # apa_landscape_executable() existed there.
  #
  # The whole-branch review's Critical 1 finding was that the SAME
  # can_execute_rotation value was not also gating the size marker and the
  # floor warning -- both are docx-only mechanisms (apatablesize.lua
  # returns early for any non-docx FORMAT, and no `apa-table-size-*` CSS
  # was ever written, D37: HTML has no page, so nothing rotates and
  # nothing shrinks). Before that fix this test asserted the WRONG
  # corrected behaviour: with the gate mocked FALSE (simulating an html
  # render), it expected the shrink-only ladder's marker to still appear.
  # With the gate now threaded through to the marker too, the same wide
  # table must render at full, unmarked size on a non-docx destination --
  # neither rotated nor shrunk.
  local_mocked_bindings(apa_quarto_docx_target = function() FALSE)
  block <- apa_quarto_markdown_block(apa.cor.table(mtcars))
  body <- as.character(block)
  expect_false(grepl("apa-table-landscape", body, fixed = TRUE))
  expect_false(grepl("apa-table-size-", body, fixed = TRUE))
})

test_that("the automatic ladder's own continuous size is snapped to a real style before it becomes a marker", {
  # Fix round 1, CRITICAL finding: apa_size_for_width() returns a continuous
  # size (measured on mtcars under can_execute_rotation = FALSE: 7.645047),
  # and the filter's marker pattern is digit-only
  # ("^apa%-table%-size%-(%d+)$") -- so before this fix the emitted marker
  # was literally "apa-table-size-7.645047", which the filter's Div handler
  # never matches, so the table silently stayed at the DEFAULT 10 pt with no
  # warning anywhere. Assert the FLOORED integer rung directly (7, not 8 --
  # rounding up could put a column back over the edge the ladder shrank it
  # to clear). This half is deliberately the LOWER-LEVEL function with
  # can_execute_rotation = FALSE forced, which is no longer what
  # apa_quarto_markdown_block() itself passes (Task 6) -- see the next
  # block for the same snapping behaviour observed through the public path.
  plan <- apa_display_plan(as_spec(apa.cor.table(mtcars)))
  decision <- apa_layout_decide(plan, can_execute_rotation = FALSE)
  expect_false(decision$rotate)
  expect_gt(decision$size, 7)
  expect_lt(decision$size, 8)
  expect_equal(apa_quarto_size_rung(decision$size), 7L)

  # The scenario none of the round-1 evidence covered: every earlier test
  # drove the marker through an explicit, already-integer font.size. A
  # 14-variable correlation table (Task 6: can_execute_rotation is TRUE
  # inside apa_quarto_markdown_block() on a docx render, so mtcars itself
  # now rotates to the full 10 pt with no marker at all on THAT render --
  # see the rotation test above -- and can no longer stand in here) rotates
  # AND still needs shrinking below the rotated budget, measured at size
  # 9.181, which floors to rung 9. This is also the rotated-AND-shrunk
  # combination D38 (dev/specs/2026-07-31-phase2-decisions.md; the Route A
  # shrink ladder, carried by apaquarto fork commit 125cbe4 on branch
  # apatables-bundle) left unverified through the public function; both
  # markers are asserted together for that reason. apa_quarto_docx_target()
  # mocked TRUE (fix round 1) so the
  # public function's own gate (finding 4) does not fall back to the
  # shrink-only ladder here, the same way the rotation test above does.
  set.seed(1)
  d14 <- as.data.frame(matrix(rnorm(30 * 14), ncol = 14))
  names(d14) <- paste0("Var", 1:14)
  plan14 <- apa_display_plan(as_spec(apa.cor.table(d14)))
  decision14 <- apa_layout_decide(plan14, can_execute_rotation = TRUE)
  expect_true(decision14$rotate)
  expect_gt(decision14$size, 9)
  expect_lt(decision14$size, 10)
  expect_equal(apa_quarto_size_rung(decision14$size), 9L)

  # B6 option 2 (Phase 1.12): the DOCX render prices this same table with
  # Word's fixed cell padding charged (17 columns x 30 twips = 0.354 in),
  # which moves the continuous size from 9.181 to 8.822 and the rung from
  # 9 to 8 -- measured. The marker below must carry the CHARGED rung: the
  # uncharged 9 is exactly the "shrinks until it JUST fits, padding pushes
  # it back over" defect this phase closed.
  charged14 <- apa_layout_decide(plan14, can_execute_rotation = TRUE,
                                 col_overhead_in = APA_WORD_CELL_PADDING_IN)
  expect_lt(charged14$size, decision14$size)
  expect_equal(apa_quarto_size_rung(charged14$size), 8L)

  local_mocked_bindings(apa_quarto_docx_target = function() TRUE)
  block <- apa_quarto_markdown_block(apa.cor.table(d14))
  body <- as.character(block)
  expect_true(grepl("apa-table-landscape", body, fixed = TRUE))
  expect_true(grepl("apa-table-size-8", body, fixed = TRUE))
  expect_false(grepl("apa-table-size-8\\.", body))
  expect_false(grepl("apa-table-size-9", body, fixed = TRUE))
  expect_false(grepl("custom-style=", body, fixed = TRUE))
})

test_that("a size the automatic ladder wants below the readable floor is floored to the smallest real style, not silently dropped", {
  # Fix round 1: the companion case to the test above -- a table so wide
  # that even can_execute_rotation = FALSE's upright budget drives the
  # continuous ladder size below APA_QUARTO_READABLE_SIZE (6 pt), which has
  # no cloned style at all (only Table9/8/7/6 exist). apa_quarto_size_rung()
  # must floor UP to 6 here (clamping, not truncating past what exists), and
  # apa_quarto_markdown_block() must still emit a marker the filter can act
  # on -- never a continuous marker the filter silently ignores.
  set.seed(1)
  wide <- as.data.frame(matrix(rnorm(30 * 22), ncol = 22))
  names(wide) <- paste0("Var", 1:22)
  plan <- apa_display_plan(as_spec(apa.cor.table(wide)))
  # suppressWarnings(): apa_layout_decide()'s own warning is expected here
  # (it always fires on its own decision, muffled only inside
  # apa_quarto_markdown_block()) and is not what this assertion is about.
  decision <- suppressWarnings(apa_layout_decide(plan, can_execute_rotation = FALSE))
  expect_lt(decision$size, APA_QUARTO_READABLE_SIZE)
  expect_equal(apa_quarto_size_rung(decision$size), APA_QUARTO_READABLE_SIZE)

  # apa_quarto_docx_target() mocked TRUE (fix round 2) so the public
  # function's own call genuinely exercises the docx path this test is
  # about, rather than incidentally landing on the same rung 6 through the
  # non-docx fallback -- both land here for THIS table (5.76 pt under TRUE,
  # 3.78 pt under FALSE, both floor to 6), so the gate's value did not
  # change the earlier, unmocked version of this assertion's PASS/FAIL, but
  # it did leave the comment claiming more than the code actually
  # exercised. expect_warning() returns the CAPTURED CONDITION, not the
  # wrapped expression's value -- as.character() on that condition object
  # is its warning text, not the table markdown, which would make the
  # assertion below pass or fail for the wrong reason. Assign inside
  # instead.
  local_mocked_bindings(apa_quarto_docx_target = function() TRUE)
  block <- NULL
  expect_warning(block <- apa_quarto_markdown_block(apa.cor.table(wide)), "6 pt")
  body <- as.character(block)
  expect_true(grepl("apa-table-size-6", body, fixed = TRUE))
})

test_that("the floored-size warning is destination-aware, not apa_layout_decide()'s LaTeX-flavoured message", {
  # Fix round 1, Important finding: apa_layout_decide()'s own warning claims
  # a raw continuous point size, calls an upright page "rotated (landscape)"
  # whenever ITS OWN internal decision happened to rotate, and advises
  # apa.save() as if this were not already a Word document -- none of which
  # is true once reached from the markdown/docx path. It must be muffled,
  # and apa_quarto_warn_floored()'s own wording used instead.
  #
  # apa_quarto_docx_target() mocked TRUE (fix round 2): this test's own
  # comment says "the markdown/docx path", and before this mock the
  # unguarded call below actually took the non-docx branch (no knit
  # session behind a plain testthat call) -- the assertions still passed,
  # because this wide table floors to rung 6 and warns "6 pt" whichever
  # branch runs, but the comment was claiming more than the code was
  # exercising. Mocking closes that gap rather than only rewording it.
  set.seed(1)
  wide <- as.data.frame(matrix(rnorm(30 * 22), ncol = 22))
  names(wide) <- paste0("Var", 1:22)
  local_mocked_bindings(apa_quarto_docx_target = function() TRUE)
  w <- tryCatch({
    apa_quarto_markdown_block(apa.cor.table(wide))
    NULL
  }, warning = function(w) w)
  expect_false(is.null(w))
  msg <- conditionMessage(w)
  expect_false(grepl("rotated (landscape)", msg, fixed = TRUE))
  expect_false(grepl("\\d+\\.\\d+ pt", msg))
  expect_false(grepl("\n", msg, fixed = TRUE))
})

test_that("apa_quarto_markdown_block() does not burn the once-per-render journal-mode warning", {
  # Fix round 2, NEW finding: fix round 1's handler was
  # `withCallingHandlers(apa_layout_decide(...), warning = function(w)
  # invokeRestart("muffleWarning"))` -- a BLANKET handler that catches every
  # warning apa_layout_decide() can raise, not only the ladder-size message
  # it was meant for. apa_layout_decide() calls apa_warn_journal_mode(geom)
  # (R/page-geometry.R) BEFORE it ever reaches the ladder, and that routes
  # through apa_warn_once() (R/apaquarto-version.R), which assign()s its
  # "already warned" flag BEFORE calling warning() -- so a blanket handler
  # does not just suppress this one call, it permanently burns the
  # once-per-render key, silencing B4's journal-mode warning (its ONLY
  # documented mitigation) for the rest of the render, even from a LATER,
  # completely unguarded call. apa_render_metadata() is mocked (the codebase's
  # own established pattern, see test-quarto-table.R) so this exercises the
  # actual PUBLIC apa_quarto_markdown_block(), not apa_layout_decide() called
  # directly.
  apa_reset_version_warnings()
  on.exit(apa_reset_version_warnings(), add = TRUE)
  local_mocked_bindings(
    apa_render_metadata = function() {
      list(format = list("apaquarto-docx" = list(documentmode = "jou")))
    }
  )
  w <- tryCatch({
    apa_quarto_markdown_block(apa.cor.table(attitude[, 1:3]))
    NULL
  }, warning = function(w) w)
  expect_false(is.null(w))
  expect_match(conditionMessage(w), "documentmode: jou", fixed = TRUE)

  # The design IS "once per render" (see R/apaquarto-version.R's own
  # comment), so a second, later call with the same key must now stay
  # silent -- that is not a defect, it is proof the flag was consumed by a
  # REAL warning the caller actually saw (the assertion just above), rather
  # than the round-1 bug: silently pre-burning the key with the warning
  # never reaching anyone at all. Distinguishing those two shapes is exactly
  # why the assertion above has to pass FIRST.
  w2 <- tryCatch({
    apa_warn_journal_mode(apa_page_geometry("portrait",
      list(format = list("apaquarto-docx" = list(documentmode = "jou")))))
    NULL
  }, warning = function(w) w)
  expect_true(is.null(w2))
})

test_that("apa_quarto_markdown_block() does not burn the once-per-render unknown-geometry warning", {
  # The companion case: an unrecognised YAML geometry setting, the other
  # apa_warn_once() call inside apa_layout_decide() (R/page-geometry.R).
  apa_reset_version_warnings()
  on.exit(apa_reset_version_warnings(), add = TRUE)
  local_mocked_bindings(
    apa_render_metadata = function() {
      list(format = list("pdf" = list(geometry = list("left=2cm"))))
    }
  )
  w <- tryCatch({
    apa_quarto_markdown_block(apa.cor.table(attitude[, 1:3]))
    NULL
  }, warning = function(w) w)
  expect_false(is.null(w))
  expect_match(conditionMessage(w), "geometry", fixed = TRUE)
})

test_that("an explicit font.size across the whole 6-12 range reaches the docx as a real style or no marker, never an invented one", {
  # Fix round 1 of Task 2 (D71 adjudication): this test used to pin a 6-9
  # world where 11 and 12 had no cloned style, so a request for either
  # collapsed to NO marker exactly like an out-of-range request did. Task 1
  # cloned Table11/Table12, so 11 and 12 are now REAL rungs (D71) and MUST
  # each produce their own marker -- asserting the old "no marker" outcome
  # for them would now be asserting the defect D71 exists to remove.
  #
  # font.size = 16L is the case the old 6-9 world could not even express:
  # a request ABOVE the ceiling. It must still land on the top real rung,
  # apa-table-size-12, and never invent "apa-table-size-16".
  #
  # apa_quarto_docx_target() mocked TRUE (whole-branch-review Critical 1):
  # the marker this test is about is a docx-only mechanism (D37), so the
  # docx gate must be exercised deliberately rather than left to fall
  # through to its "no real knit session" default of FALSE.
  local_mocked_bindings(apa_quarto_docx_target = function() TRUE)
  b5 <- suppressWarnings(apa_quarto_markdown_block(apa.cor.table(attitude[, 1:3]), font.size = 5L))
  body5 <- as.character(b5)
  expect_true(grepl("apa-table-size-6", body5, fixed = TRUE))
  expect_false(grepl("apa-table-size-5", body5, fixed = TRUE))

  b11 <- apa_quarto_markdown_block(apa.cor.table(attitude[, 1:3]), font.size = 11L)
  body11 <- as.character(b11)
  expect_true(grepl("apa-table-size-11", body11, fixed = TRUE))

  b12 <- apa_quarto_markdown_block(apa.cor.table(attitude[, 1:3]), font.size = 12L)
  body12 <- as.character(b12)
  expect_true(grepl("apa-table-size-12", body12, fixed = TRUE))

  b16 <- suppressWarnings(apa_quarto_markdown_block(apa.cor.table(attitude[, 1:3]), font.size = 16L))
  body16 <- as.character(b16)
  expect_true(grepl("apa-table-size-12", body16, fixed = TRUE))
  expect_false(grepl("apa-table-size-16", body16, fixed = TRUE))
})

test_that("apatablesize.lua's Div handler gates a matched size on valid_sizes membership, not merely on being a digit string", {
  # The Lua half of finding 2, still missing after round 1: the filter itself
  # must not trust its input just because today's only caller (R, clamped by
  # apa_quarto_size_rung()) behaves -- it is a bundled, independently
  # loadable artifact. A marker naming a size no style exists for can reach
  # this filter directly (a hand-authored document, or a future caller), so
  # the class-name match alone must not be trusted to name a real style.
  #
  # Fix round 1 of Task 2 (D71 adjudication): this test used to also assert
  # the declared valid_sizes SET as a single-line source-text literal
  # (`local valid_sizes = { ["9"] = true, ... }`), which pinned FORMATTING,
  # not the rule -- Task 1 rewrote the declaration across six lines with six
  # sizes and broke this on formatting alone, values notwithstanding. That
  # set -- that valid_sizes agrees with the styles actually shipped in
  # apaquarto.docx -- is now covered, format-independently, by
  # tests/testthat/test-word-size-styles.R, so it is not repeated here.
  #
  # What that file does NOT cover, and this one still does: the class-name
  # pattern below (sizeclass) is a GENERIC digit matcher, "(%d+)" -- it
  # matches "apa-table-size-13" exactly as readily as "apa-table-size-6".
  # The ONLY thing that turns an unrecognised size into "left alone" rather
  # than "written into custom-style as a style that does not exist" is the
  # `valid_sizes[n]` membership check immediately after the match. A future
  # edit that loosened that guard -- e.g. `if n then size = n end`, which
  # would let ANY digit string through -- changes no formatting and would
  # not be caught by test-word-size-styles.R, which only compares the
  # DECLARED set against the binary and never looks at whether the set is
  # actually consulted.
  lua <- system.file("_extensions/wjschne/apaquarto/apatablesize.lua",
                     package = "apaTables")
  skip_if(lua == "")
  txt <- paste(readLines(lua, warn = FALSE), collapse = "\n")
  code <- gsub("--[^\n]*", "", txt)
  code <- gsub("\\s+", " ", code)
  # The extraction pattern accepts any digit string...
  expect_true(grepl('sizeclass = "^apa%-table%-size%-(%d+)$"', code, fixed = TRUE))
  # ...so this membership check is the ONLY thing standing between an
  # unrecognised size and a custom-style naming a style that does not exist.
  expect_true(grepl("if n and valid_sizes[n] then", code, fixed = TRUE))
})

test_that("the bundled reference document defines the six cloned table styles at the right sizes", {
  # Fix round 1 of Task 2 (D71 adjudication): extended from four styles to
  # six -- Task 1 added Table11 (22 half-points = 11 pt) and Table12
  # (24 half-points = 12 pt) alongside the original Table9/8/7/6 shrink
  # ladder. "Four" in this test's name was true before Task 1 and is false
  # now.
  ref <- system.file("_extensions/wjschne/apaquarto/apaquarto.docx",
                     package = "apaTables")
  skip_if(ref == "")
  tmp <- tempfile(); dir.create(tmp)
  utils::unzip(ref, files = "word/styles.xml", exdir = tmp)
  s <- paste(readLines(file.path(tmp, "word", "styles.xml"), warn = FALSE),
            collapse = "")
  expected_sz <- c("9" = "18", "8" = "16", "7" = "14", "6" = "12",
                   "11" = "22", "12" = "24")
  for (n in names(expected_sz)) {
    id <- paste0("Table", n)
    pat <- paste0('<w:style w:type="table"[^>]*w:styleId="', id, '">.*?</w:style>')
    m <- regmatches(s, regexpr(pat, s))
    expect_length(m, 1)
    expect_true(grepl(paste0('<w:sz w:val="', expected_sz[[n]], '"/>'), m, fixed = TRUE),
               info = id)
    # D36: <w:rPr> must sit AFTER <w:pPr> in CT_Style's strict sequence, or a
    # stricter consumer than Word can reject the whole styles.xml part.
    pos_ppr <- regexpr("</w:pPr>", m, fixed = TRUE)
    pos_rpr <- regexpr("<w:rPr>", m, fixed = TRUE)
    expect_true(pos_ppr > 0 && pos_rpr > 0 && pos_rpr > pos_ppr, info = id)
  }
})

test_that("the Table style and its shrink-ladder clones cut fixed cell padding to 15 twips (D39)", {
  # D39: a Word table cell's <w:tblCellMar> is FIXED padding, the same twips
  # whatever the column's width, and the width model never subtracts it. On
  # apa.cor.table(mtcars) (14 columns, auto-shrunk to Table7) the narrow N
  # column lost 55% of its own gross width to the stock 58-twip margin and
  # real Word broke numbers mid-digit. Measured in real Word: 58 breaks
  # nearly every narrow cell, 29 still breaks the widest values, 15 is
  # clean, 0 is clean. This asserts the accepted value, 15, on every table
  # style that carries its own <w:tblCellMar> declaration -- Table plus the
  # four D38 shrink-ladder clones, which the D39 spike confirmed each
  # declare the margin independently rather than inheriting it. Before this
  # patch, RED: every style below carries 58, not 15.
  #
  # Fix round 1 of Task 2 (D71 adjudication, coverage gap): extended to
  # Table11/Table12, Task 1's new clones above the default. Verified by hand
  # against the shipped binary that both already carry 15/15/15/15 -- this
  # closes a gap rather than reporting a bug, but D39 exists because the
  # WRONG number here breaks real Word invisibly to every other check, so a
  # future clone built off the wrong template must not go unwatched.
  ref <- system.file("_extensions/wjschne/apaquarto/apaquarto.docx",
                     package = "apaTables")
  skip_if(ref == "")
  tmp <- tempfile(); dir.create(tmp)
  utils::unzip(ref, files = "word/styles.xml", exdir = tmp)
  s <- paste(readLines(file.path(tmp, "word", "styles.xml"), warn = FALSE),
            collapse = "")
  for (id in c("Table", "Table9", "Table8", "Table7", "Table6",
              "Table11", "Table12")) {
    pat <- paste0('<w:style w:type="table"[^>]*w:styleId="', id, '">.*?</w:style>')
    m <- regmatches(s, regexpr(pat, s))
    expect_length(m, 1)
    margin <- regmatches(m, regexpr('<w:tblCellMar>.*?</w:tblCellMar>', m))
    expect_length(margin, 1)
    for (side in c("top", "left", "bottom", "right")) {
      expect_true(
        grepl(paste0('<w:', side, ' w:w="15" w:type="dxa"/>'), margin, fixed = TRUE),
        info = paste(id, side))
    }
  }
  # B6 option 2 (Phase 1.12): the ladder charges the Word destination for
  # exactly this padding, via APA_WORD_CELL_PADDING_IN. The constant must be
  # the binary's own number -- 15 twips a side, 1440 twips to the inch -- or
  # a future re-cut of these styles would silently re-open the gap the
  # ladder just closed (the constant lying about the binary is invisible to
  # every other check, which is why it is asserted HERE, next to the twips).
  expect_equal(APA_WORD_CELL_PADDING_IN, (2 * 15) / 1440)
})

test_that("apatablesize.lua's return statement is the plain array shape, not augmented with a stray key", {
  # Fix round 1, finding 5: the ORIGINAL version of this test asserted only
  # that the string "FloatRefTarget" appears somewhere in the file and that
  # the string "decide" does not -- which passes for any file that merely
  # MENTIONS FloatRefTarget (e.g. in a comment) and would still pass on the
  # exact defect it names, `{ { FloatRefTarget = fn }, decide = M.decide }`,
  # if that stray key were spelled any other way (or if "decide" appeared in
  # a comment, which would make the OLD version fail for the wrong reason
  # entirely). Assert the actual return statement's shape instead: strip Lua
  # "--" comments first, collapse whitespace, and require the EXACT plain
  # shape substring to appear, with exactly one "return {" in the file.
  lua <- system.file("_extensions/wjschne/apaquarto/apatablesize.lua",
                     package = "apaTables")
  skip_if(lua == "")
  txt <- paste(readLines(lua, warn = FALSE), collapse = "\n")
  code <- gsub("--[^\n]*", "", txt)
  code <- gsub("\\s+", " ", code)
  # Count occurrences of the FULL target string, not a "return {" prefix --
  # the filter's own Div handler also contains a bare "return {}" (to strip
  # a matched marker div), which a prefix match would double-count. A
  # gregexpr() no-match sentinel (-1L) is ALSO a length-1 vector, so a naive
  # length(gregexpr(...)[[1]]) cannot tell "exactly one match" from "zero
  # matches" -- the same trap apa_md_count_matches() (R/render-markdown.R)
  # exists to avoid; that helper takes a regex, not fixed = TRUE, so the
  # count is done directly here with the same sentinel guard.
  #
  # Task 6 fix round 1 (Important, finding 3): this filter was rescoped
  # from `{ { FloatRefTarget = processfloat } }` to
  # `{ { Pandoc = process_pandoc } }`, because a FloatRefTarget-only handler
  # silently did nothing for an unlabelled (non-float) wide table -- see
  # apatablesize.lua's own banner and apatablelandscape.lua's identical
  # resolution of the same question. The shape being asserted here is
  # updated to match; the ASSERTION -- exactly one plain, stray-key-free
  # return statement -- is unchanged.
  target <- "return { { Pandoc = process_pandoc } }"
  hits <- gregexpr(target, code, fixed = TRUE)[[1]]
  n_hits <- if (length(hits) == 1L && hits == -1L) 0L else length(hits)
  expect_equal(n_hits, 1L)

  yml <- system.file("_extensions/wjschne/apaquarto/_extension.yml",
                     package = "apaTables")
  skip_if(yml == "")
  ytxt <- paste(readLines(yml, warn = FALSE), collapse = "\n")
  expect_true(grepl("apatablesize.lua", ytxt, fixed = TRUE))
})
