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
  # (R/render-markdown.R), and apa.reg.table() -- exercised by the
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
    # A flat plan never carries rule-div markup -- the spanned plan is the
    # SOLE carrier of that constant-overhead
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
  # This fixture's own plan$header shows level-1 (top-level) spans of
  # 1, 6, 1, 1 -- the "6" is the real spanner this test is named for.
  # Level-2 SUB-spanners underneath it are span 2 each, which is where
  # "ColSpan 2" actually comes from. An assertion of
  # `ColSpan 2 || ColSpan 3` could never observe the top-level spanner at
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
  # A spanner label is a header cell at a HIGHER level (span > 1), so a width
  # model that measured only the last header row and "data" body rows would
  # never check it against the width it needs. A spanner label far longer
  # than the narrow columns beneath it must widen those columns, not
  # collapse the grid.
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

test_that("a single-column spanner does not corrupt the grid", {
  # apa_display_plan() gives a spanner covering exactly ONE column a span-1
  # header cell that is still recorded in that row's `rules` (R/display-
  # plan.R: span = length(hit$cols) = 1L, rules[[...]] <- c(min, max) =
  # c(2, 2) here) -- so apa_markdown_table() still wraps its label in the
  # "::: {custom-style=\"ApaTableRule\"}" rule div (apa_md_in_ranges() tests
  # column membership, never span). So apa_md_span_widen() must budget width
  # for that wrap on a span-1 spanner too: guarding on `span > 1L` alone lets
  # the 34-character fence line outgrow its unbudgeted column and desync the
  # whole grid. No shipped builder can reach this today (apa_spanner() is
  # unexported), so it is constructed directly here, with `cols = 2L`.
  spec <- apa_spec_start(2L)
  spec <- apa_col(spec, label = "V", align = "l", values = list("a", "b"))
  spec <- apa_col(spec, label = "M", align = "r", values = list("1", "2"))
  spec <- apa_col(spec, label = "SD", align = "r", values = list("1", "2"))
  spec <- apa_spanner(spec, label = "Solo", cols = 2L)
  plan <- apa_display_plan(apa_spec_finish(spec))
  md <- apa_markdown_table(plan)
  lines <- strsplit(md, "\n", fixed = TRUE)[[1]]

  # The grid-format invariant itself: every rule and content line the same
  # total width. Without the budget there are two distinct widths (17 and
  # 44 on this exact fixture, measured) because the rule-div fence line runs
  # past its unbudgeted column.
  widths <- unique(nchar(lines[grepl("^[+|]", lines)]))
  expect_length(widths, 1)

  # The column count itself, via a real pandoc parse. Without the budget,
  # pandoc reads the desynced "|" positions as literal text and reports 4
  # columns from this 3-column plan (measured).
  rt <- roundtrip(md)
  expect_equal(rt$n_col, plan$n_col)
})

test_that("a flattened spanner label does not desync the grid's line widths", {
  # The render-level guard (test-slide-render.R) is quarto-only --
  # skip_on_cran() + the quarto check + apa_skip_if_library_stale() -- so on
  # CRAN, or any machine without quarto, a regression here would ship
  # silently. This test needs no render at all.
  #
  # The defect guarded here is in apa_markdown_table()'s own width model,
  # which must measure ANY span == 1 cell in a NON-final header row,
  # wherever such a cell comes from. The minimal shape that triggers it -- a
  # spanner flattened into span-1 cells, its label in the middle one -- is
  # built here by hand.
  #
  # If apa_md_widths() measures only the LAST header row and data rows, and
  # apa_md_span_widen() only span > 1 cells, a flattened label falls between
  # both and gets no width guarantee. A grid table's own format invariant
  # is that every line (every rule and every content row) is the
  # SAME total width; a column one character too narrow for its content
  # breaks that invariant in the EMITTED TEXT ITSELF, before pandoc ever
  # sees it -- measured on this exact plan: line widths of 76 and 77 (two
  # distinct values, i.e. ragged) without that guarantee, a uniform 83 with
  # it.
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

# ---- Style hooks (font_size / rules) ---------------------------------------

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
  # The default emission puts nothing on ordinary cells -- 10 pt comes from
  # the reference document's Table style, not a per-cell span, and the last
  # header row's rule comes from that same style, not a rule div. Do not
  # default rules to TRUE: that wraps every cell of the last header row in
  # "::: {custom-style=\"ApaTableRule\"}", which also contains
  # "custom-style=" -- so this assertion catches it.
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude[, 1:3])))
  md <- apa_markdown_table(plan)
  expect_false(grepl("custom-style=", md, fixed = TRUE))
})

test_that("the width model does not charge every column a constant rule-div overhead", {
  # NOT a test of the per-cell font-size span (which collapses a real
  # table's column ratio from 3.83:1 to 1.28:1, measured) -- with
  # font_size = NULL (this test's default call, and apa_markdown_table()'s
  # own default) that span is never emitted, so it cannot be what this
  # assertion discriminates. What it actually watches is the OTHER constant
  # overhead: rule-dividing every cell of the last header row regardless of
  # font_size. That ~34-character fence line, applied identically to every
  # column via floor_w, flattens the ratio to exactly 1.0 (measured --
  # narrower even than the combined 1.28:1, because here it is the rule
  # overhead alone). Assert the
  # SHAPE of the width model, observable without a render: the widest column
  # must be at least 2.5x the narrowest.
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude[, 1:4])))
  w <- apa_md_widths(plan)
  expect_gt(max(w) / min(w), 2.5)
})

test_that("a spanner still gets its cmidrule, and the header row does NOT", {
  # apaquarto's Table style already draws a continuous rule under the LAST
  # header row for both a one-row and a three-row header, so the emitter
  # must not draw a second, segmented one there -- but the partial
  # (cmidrule) rule under a spanner is still the emitter's job, since a table
  # style has no notion of "only some columns". The flat-table assertion
  # catches an emitter that wraps every cell of the last header row
  # regardless of spanners.
  flat <- apa_display_plan(as_spec(apa.cor.table(attitude[, 1:3])))
  expect_false(grepl("ApaTableRule", apa_markdown_table(flat), fixed = TRUE))
  spanned <- apa_display_plan(as_spec(apa.2way.table(
    iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles,
    show.conf.interval = FALSE, show.marginal.means = TRUE)))
  expect_true(grepl("ApaTableRule", apa_markdown_table(spanned), fixed = TRUE))
})

test_that("rules = FALSE suppresses a spanner's cmidrule too", {
  # The other half of "rules gates the rule divs". A flat table does not
  # discriminate rules = TRUE vs FALSE (neither wraps its last header row),
  # so this must use a spanned table, where rules = TRUE
  # is known (the test above) to emit ApaTableRule.
  plan <- apa_display_plan(as_spec(apa.2way.table(
    iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles,
    show.conf.interval = FALSE, show.marginal.means = TRUE)))
  md <- apa_markdown_table(plan, rules = FALSE)
  expect_false(grepl("ApaTableRule", md, fixed = TRUE))
})

test_that("a rule-divided spanner cell parses as a real pandoc Div, not literal fence text", {
  # Watches a real trap: apa_md_row() glues every
  # non-final line of a multi-line cell together with a trailing "\\" hard
  # break, which is correct for a stacked r/CI/p cell but WRONG for a
  # fenced div -- a "\\" keeps the ":::" marker inside the same inline
  # paragraph as its neighbours, so pandoc never recognises it as a block
  # boundary and reads it back as the literal three characters ":::"
  # instead of a Div node. Without apa_md_is_fence_line() this round-trips
  # as ColSpan 7 (the whole header row collapsed into one cell) with
  # `Str ":::"` in the dump (measured). Uses a spanned plan (not a flat one)
  # because a flat table's header row is not rule-divided at all.
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
  # The partial (cmidrule) rule: a spanner's own header row must NOT wrap
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
  # escape at the emitter level, without a render: the parsed pandoc AST
  # must hold a Plain/Para run "1. rating", never an OrderedList.
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
  # 10 pt is a property of the reference document's Table TABLE style
  # (a <w:rPr> there applies to every run in the table), not per-cell markup.
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

# ---- HTML -- measure what apaquarto already styles -------------------------
#
# The default markdown emission carries NO per-cell custom-style at all, so
# a "data-custom-style=\"ApaTable10\"" assertion has no subject -- it is
# simply never emitted. The only custom-style pandoc's HTML writer sees is
# "ApaTableRule", and only on spanner header cells (the cmidrule), and only
# when the table has spanners.
#
# Measured directly: rendering a real
# apaTables grid table through `quarto render` with format apaquarto-html
# shows apaquarto's OWN .table CSS already supplies every ordinary-table
# furniture rule with NO per-cell markup at all -- a rule above the header,
# a rule under the last header row (one-row and multi-row headers alike), a
# rule under the last body row, no vertical rules, and no interior
# horizontal rules between body rows. What apaquarto's CSS does NOT do is
# style the cmidrule div: without apa.css's
# "[data-custom-style=\"ApaTableRule\"]" rule, a spanner's fenced div
# renders as a plain, unstyled <div data-custom-style="ApaTableRule"> with
# no visible border at all. That rule is the one thing apaTables adds here.

test_that("a spanner's cmidrule survives pandoc's HTML writer as a data-custom-style hook", {
  # The narrow, emitter-side half of the measurement -- what markup
  # apa_markdown_table() itself produces -- checked with bare pandoc, which
  # cannot speak to apaquarto's CSS (only `quarto render` loads the
  # extension's stylesheet).
  #
  # to = "html5", NOT "html". Watched directly with
  # rmarkdown::pandoc_convert(..., verbose = TRUE): to = "html" resolves to
  # pandoc's `--to html4` writer, which passes an unrecognised div attribute
  # through UNPREFIXED ("custom-style=", no "data-") -- only the html5 writer
  # adds the "data-" prefix that both apaquarto's CSS selector and Quarto's
  # own apaquarto-html output (confirmed by a real `quarto render`, which
  # targets html5) rely on. A check run against "html" would never see a
  # match and would misreport this as a missing hook.
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
  # No font-size hook survives to HTML either, at the default call --
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
  # tree's inst/ under pkgload::load_all()). Without a selector for
  # "[data-custom-style=\"ApaTableRule\"]", a spanner's cmidrule div renders
  # with no border in a real quarto render (measured).
  ref <- system.file("_extensions/wjschne/apaquarto/apa.css",
                     package = "apaTables")
  skip_if(ref == "")
  css <- paste(readLines(ref, warn = FALSE), collapse = "\n")
  rule <- regmatches(css, regexpr('\\[data-custom-style="ApaTableRule"\\][^{]*\\{[^}]*\\}', css))
  expect_length(rule, 1)
  expect_true(grepl("border", rule, fixed = TRUE))
  # No font-size ladder is added: HTML has no page, so rotation/shrinking are
  # not applicable there, and the emitter never wraps an ordinary cell in an
  # ApaTable<N> span by default -- so nothing would ever match a
  # "[data-custom-style^=\"ApaTable1\"]"-style selector in the HTML these
  # tests produce. Adding CSS for a hook the
  # emitter never emits would be untestable dead weight in the bundle.
  expect_false(grepl('ApaTable1', css, fixed = TRUE))
})

# ---- Destination policy, rotation decision, table-style shrink ladder ------
#
# The shrink ladder is a set of cloned TABLE STYLES selected by a Lua filter,
# not per-cell ApaTableN spans (measured in real Word). Per-cell spans were
# measured to crush a real table's column ratio from 10.01:1 to 2.12:1 and
# break Organizational_Citizenship_Behavior mid-word. So apa_markdown_table()
# is NEVER handed a font size on the Quarto policy path
# (apa_quarto_markdown_block()); its font_size parameter is exercised
# directly by the style-hook tests above and stays available for other
# callers (e.g. apa.save()), but the tests below must watch for the ABSENCE
# of per-cell markup on the policy path, not its presence.

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

test_that("a shrunken table gets a table-style marker, never a per-cell span, and keeps its column proportions", {
  # apa_quarto_docx_target() mocked TRUE: the size marker is a docx-only
  # mechanism (apatablesize.lua stands down on any non-docx FORMAT, and no
  # `apa-table-size-*` CSS exists for HTML), so this specifically
  # docx-flavoured ("table-style marker") assertion must exercise the docx
  # gate rather than fall through the "no
  # real knit session" default of FALSE.
  local_mocked_bindings(apa_quarto_docx_target = function() TRUE)
  block <- apa_quarto_markdown_block(apa.cor.table(attitude[, 1:3]), font.size = 8L)
  body <- as.character(block)

  # A table-style marker, not per-cell spans: no ApaTable8 span anywhere...
  expect_false(grepl('custom-style="ApaTable8"', body, fixed = TRUE))
  # ...but a marker naming the size for apatablesize.lua to read.
  expect_true(grepl("apa-table-size-8", body, fixed = TRUE))

  # Column proportions undisturbed: apa_markdown_table() must have been
  # called with font_size = NULL underneath, so the grid's own column widths
  # are identical to the unshrunk table's -- the whole point of shrinking by
  # table style. A per-column constant-overhead span would flatten this
  # ratio toward 1:1, which is the defect the table-style ladder exists to
  # avoid (measured 3.83:1 -> 1.28:1 on a real table).
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
  # Quarto's own `.landscape` div for docx closes with an EMPTY sectPr,
  # silently stripping the running head and page number from itself and
  # every page before it, and its rotated size is hardcoded A4 -- so this
  # must be a class Quarto's own docx pipeline does not already act on. Unit
  # test of the wrap function itself.
  wrapped <- apa_landscape_wrap("BODY")
  expect_true(grepl("apa-table-landscape", wrapped, fixed = TRUE))
  # The next assertion CANNOT catch a reversion to Quarto's own .landscape
  # div, and is kept only as a trivial format check on the wrapper's own
  # known-good attrs. The bare literal `{.landscape}` -- NOTHING between
  # "landscape" and the closing brace -- can never appear in this function's
  # output: apa_landscape_wrap() (R/quarto-table.R) always appends the real
  # page-geometry attributes (pgw=/pgh=/mt=/...) between the class name and
  # the closing brace, in the SAME paste0() call that writes the class, so
  # reverting APA_QUARTO_LANDSCAPE_CLASS to Quarto's own "landscape" would
  # emit `{.landscape pgw="12240" ...}`, never the bare form (confirmed by
  # simulating that exact reversion).
  expect_false(grepl("{.landscape}", wrapped, fixed = TRUE))
  expect_true(grepl("BODY", wrapped, fixed = TRUE))
  # The div also carries the real page geometry apatablelandscape.lua needs
  # -- read out of the bundled reference document, not any of these exact
  # numbers assumed here beyond what the reference document is known to be
  # (US Letter, 1 in margins, measured).
  expect_true(grepl('pgw="12240"', wrapped, fixed = TRUE))
  expect_true(grepl('pgh="15840"', wrapped, fixed = TRUE))
})

test_that("apa_quarto_markdown_block() rotates a table wide enough to want it, on a docx render", {
  # apa_quarto_markdown_block()'s can_execute_rotation is
  # apa_quarto_docx_target() -- MOCKED to TRUE here, matching this file's
  # own established convention (apa_render_metadata() is mocked the same
  # way, never a knitr internal directly), because this is a plain testthat
  # call with no real knit session behind it to report a pandoc target at
  # all. mtcars' correlation table does not fit the upright 6.5 in budget
  # at 10 pt (measured: apa_layout_decide(plan, can_execute_rotation =
  # FALSE) shrinks it to ~7.65 pt instead) but DOES fit the wider rotated
  # budget at the full 10 pt (rotate before shrink) -- so on a docx
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

test_that("apa_quarto_markdown_block() does NOT rotate, and does NOT mark a shrink, on a non-docx render", {
  # can_execute_rotation must NOT be TRUE unconditionally: correct for docx
  # but silently wrong for html, which apa_quarto_markdown_block() also
  # serves and which apatablelandscape.lua's own `if FORMAT ~= "docx" then
  # return end` guard stands down for entirely. An HTML render would price
  # mtcars against the WIDER rotated budget, emit it at the full 10 pt with
  # a dead, unprocessed `.apa-table-landscape` div, and never actually
  # rotate -- the same shape of mispricing apa_landscape_executable()
  # prevents on the LaTeX path.
  #
  # The SAME can_execute_rotation value must also gate the size marker and
  # the floor warning -- both are docx-only mechanisms (apatablesize.lua
  # returns early for any non-docx FORMAT, and no `apa-table-size-*` CSS
  # exists: HTML has no page, so nothing rotates and nothing shrinks). With
  # the gate mocked FALSE (simulating an html render), the same wide table
  # must render at full, unmarked size -- neither rotated nor shrunk.
  local_mocked_bindings(apa_quarto_docx_target = function() FALSE)
  block <- apa_quarto_markdown_block(apa.cor.table(mtcars))
  body <- as.character(block)
  expect_false(grepl("apa-table-landscape", body, fixed = TRUE))
  expect_false(grepl("apa-table-size-", body, fixed = TRUE))
})

test_that("the automatic ladder's own continuous size is snapped to a real style before it becomes a marker", {
  # apa_size_for_width() returns a continuous size (measured on mtcars
  # under can_execute_rotation = FALSE: 7.645047), and the filter's marker
  # pattern is digit-only ("^apa%-table%-size%-(%d+)$") -- so an unsnapped
  # marker would be literally "apa-table-size-7.645047", which the filter's
  # Div handler never matches, and the table would silently stay at the
  # DEFAULT 10 pt with no warning anywhere. Assert the FLOORED integer rung
  # directly (7, not 8 -- rounding up could put a column back over the edge
  # the ladder shrank it to clear). This half is deliberately the
  # LOWER-LEVEL function with can_execute_rotation = FALSE forced, which is
  # not what apa_quarto_markdown_block() itself passes on a docx render --
  # see the next block for the same snapping behaviour observed through the
  # public path.
  plan <- apa_display_plan(as_spec(apa.cor.table(mtcars)))
  decision <- apa_layout_decide(plan, can_execute_rotation = FALSE)
  expect_false(decision$rotate)
  expect_gt(decision$size, 7)
  expect_lt(decision$size, 8)
  expect_equal(apa_quarto_size_rung(decision$size), 7L)

  # The public path, driven by a continuous ladder size rather than an
  # explicit, already-integer font.size. A 14-variable correlation table
  # (mtcars cannot stand in here: can_execute_rotation is TRUE inside
  # apa_quarto_markdown_block() on a docx render, so mtcars rotates to the
  # full 10 pt with no marker at all -- see the rotation test above) rotates
  # AND still needs shrinking below the rotated budget, measured at size
  # 9.181, which floors to rung 9. This is the rotated-AND-shrunk
  # combination, so both markers are asserted together.
  # apa_quarto_docx_target() mocked TRUE so the public function's own docx
  # gate does not fall back to the shrink-only ladder here, the same way the
  # rotation test above does.
  set.seed(1)
  d14 <- as.data.frame(matrix(rnorm(30 * 14), ncol = 14))
  names(d14) <- paste0("Var", 1:14)
  plan14 <- apa_display_plan(as_spec(apa.cor.table(d14)))
  decision14 <- apa_layout_decide(plan14, can_execute_rotation = TRUE)
  expect_true(decision14$rotate)
  expect_gt(decision14$size, 9)
  expect_lt(decision14$size, 10)
  expect_equal(apa_quarto_size_rung(decision14$size), 9L)

  # The DOCX render prices this same table with Word's fixed cell padding
  # charged (17 columns x 30 twips = 0.354 in), which moves the continuous
  # size from 9.181 to 8.822 and the rung from 9 to 8 -- measured. The
  # marker below must carry the CHARGED rung: the uncharged 9 is exactly the
  # "shrinks until it JUST fits, padding pushes it back over" defect the
  # padding charge prevents.
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
  # The companion case to the test above -- a table so wide
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

  # apa_quarto_docx_target() mocked TRUE so the public function's own call
  # genuinely exercises the docx path this test is about, rather than
  # incidentally landing on the same rung 6 through the non-docx fallback --
  # both land here for THIS table (5.76 pt under TRUE, 3.78 pt under FALSE,
  # both floor to 6), so without the mock the assertion would pass without
  # exercising the docx path. expect_warning() returns the CAPTURED
  # CONDITION, not the
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
  # apa_layout_decide()'s own warning claims
  # a raw continuous point size, calls an upright page "rotated (landscape)"
  # whenever ITS OWN internal decision happened to rotate, and advises
  # apa.save() as if this were not already a Word document -- none of which
  # is true once reached from the markdown/docx path. It must be muffled,
  # and apa_quarto_warn_floored()'s own wording used instead.
  #
  # apa_quarto_docx_target() mocked TRUE: without the mock the call below
  # takes the non-docx branch (no knit session behind a plain testthat
  # call) -- the assertions would still pass, because this wide table floors
  # to rung 6 and warns "6 pt" whichever branch runs, but they would not be
  # exercising the markdown/docx path this test is about.
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
  # The muffling handler must NOT be a BLANKET
  # `withCallingHandlers(apa_layout_decide(...), warning = function(w)
  # invokeRestart("muffleWarning"))`, which catches every warning
  # apa_layout_decide() can raise, not only the ladder-size message it is
  # meant for. apa_layout_decide() calls apa_warn_journal_mode(geom)
  # (R/page-geometry.R) BEFORE it ever reaches the ladder, and that routes
  # through apa_warn_once() (R/apaquarto-version.R), which assign()s its
  # "already warned" flag BEFORE calling warning() -- so a blanket handler
  # does not just suppress this one call, it permanently burns the
  # once-per-render key, silencing the journal-mode warning (its ONLY
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
  # than the blanket-handler bug: silently pre-burning the key with the warning
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
  # Table11/Table12 are cloned in the reference document, so 11 and 12 are
  # REAL rungs and MUST each produce their own marker -- collapsing either to
  # NO marker (as an out-of-range request would) is a defect.
  #
  # font.size = 16L is a request ABOVE the ceiling. It must still land on
  # the top real rung, apa-table-size-12, and never invent
  # "apa-table-size-16".
  #
  # apa_quarto_docx_target() mocked TRUE: the marker this test is about is
  # a docx-only mechanism, so the
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
  # The Lua half of the size clamp: the filter itself
  # must not trust its input just because today's only caller (R, clamped by
  # apa_quarto_size_rung()) behaves -- it is a bundled, independently
  # loadable artifact. A marker naming a size no style exists for can reach
  # this filter directly (a hand-authored document, or a future caller), so
  # the class-name match alone must not be trusted to name a real style.
  #
  # Do not assert the declared valid_sizes SET here as a source-text literal
  # (`local valid_sizes = { ["9"] = true, ... }`): that pins FORMATTING, not
  # the rule, and breaks when the declaration is merely re-wrapped. That
  # set -- that valid_sizes agrees with the styles actually shipped in
  # apaquarto.docx -- is covered, format-independently, by
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
  # Table11 (22 half-points = 11 pt) and Table12 (24 half-points = 12 pt)
  # sit alongside the Table9/8/7/6 shrink ladder.
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
    # <w:rPr> must sit AFTER <w:pPr> in CT_Style's strict sequence, or a
    # stricter consumer than Word can reject the whole styles.xml part.
    pos_ppr <- regexpr("</w:pPr>", m, fixed = TRUE)
    pos_rpr <- regexpr("<w:rPr>", m, fixed = TRUE)
    expect_true(pos_ppr > 0 && pos_rpr > 0 && pos_rpr > pos_ppr, info = id)
  }
})

test_that("the Table style and its shrink-ladder clones cut fixed cell padding to 15 twips", {
  # A Word table cell's <w:tblCellMar> is FIXED padding, the same twips
  # whatever the column's width, and the width model never subtracts it. On
  # apa.cor.table(mtcars) (14 columns, auto-shrunk to Table7) the narrow N
  # column lost 55% of its own gross width to the stock 58-twip margin and
  # real Word broke numbers mid-digit. Measured in real Word: 58 breaks
  # nearly every narrow cell, 29 still breaks the widest values, 15 is
  # clean, 0 is clean. This asserts the accepted value, 15, on every table
  # style that carries its own <w:tblCellMar> declaration -- Table plus
  # every cloned size style, each of which declares the margin independently
  # rather than inheriting it (measured). The clones above the default
  # (Table11/Table12) are watched too: the WRONG number here breaks real
  # Word invisibly to every other check, so a future clone built off the
  # wrong template must not go unwatched.
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
  # The ladder charges the Word destination for
  # exactly this padding, via APA_WORD_CELL_PADDING_IN. The constant must be
  # the binary's own number -- 15 twips a side, 1440 twips to the inch -- or
  # a future re-cut of these styles would silently re-open the gap the
  # charge closes (the constant lying about the binary is invisible to
  # every other check, which is why it is asserted HERE, next to the twips).
  expect_equal(APA_WORD_CELL_PADDING_IN, (2 * 15) / 1440)
})

test_that("apatablesize.lua's return statement is the plain array shape, not augmented with a stray key", {
  # Do not weaken this to substring checks (the handler name present,
  # "decide" absent): those pass for any file that merely MENTIONS the
  # handler (e.g. in a comment), still pass on the exact defect named here,
  # a stray key such as `decide = M.decide`, if it is spelled any other way,
  # and fail for the wrong reason if "decide" appears in a comment. Assert
  # the actual return statement's shape instead: strip Lua
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
  # The handler is `{ { Pandoc = process_pandoc } }`, NOT a FloatRefTarget
  # handler: a FloatRefTarget-only handler silently does nothing for an
  # unlabelled (non-float) wide table -- see apatablesize.lua's own banner
  # and apatablelandscape.lua's identical resolution of the same question.
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
