# The format-named entry points, and the retirement of apa.quarto.table().
#
# apa.quarto.pdf() / apa.quarto.docx() / apa.quarto.html() are FORGIVING: the
# render TARGET decides the mechanism (LaTeX to apa_quarto_latex_block(),
# everything else to apa_quarto_markdown_block()), and the function name only
# states intent. A name/target mismatch produces a guidance message naming
# the exact YAML line to change -- never a refusal, never a warning standing
# in for a fix.
#
# apa.quarto.revealjs() is tested in test-quarto-slide.R.

test_that("the three manuscript entry points produce the SAME REAL TABLE", {
  # Outside any knit, apa_latex_target() reads FALSE for all three calls (no
  # pandoc target is set), so all three dispatch to apa_quarto_markdown_block()
  # -- this is what "forgiving" means: the CALL NAME never decides the
  # mechanism by itself. Identity alone would pass if all three returned ""
  # or NULL, so the content is asserted FIRST.
  x <- apa.cor.table(attitude[, 1:3])
  out <- lapply(list(apa.quarto.pdf, apa.quarto.docx, apa.quarto.html),
                function(f) as.character(f(x)))
  expect_match(out[[1]], "rating", fixed = TRUE)
  expect_match(out[[1]], "+===", fixed = TRUE)
  expect_equal(length(unique(unlist(out))), 1L)
})

test_that("apa.quarto.table is gone", {
  expect_false("apa.quarto.table" %in% getNamespaceExports("apaTables"))
  expect_false(exists("apa.quarto.table", where = asNamespace("apaTables"),
                       inherits = FALSE))
})

test_that("a name/target mismatch names the exact YAML line to change", {
  # rendered under apaquarto-pdf while calling apa.quarto.docx()
  msg <- apa_quarto_target_message(called = "docx", actual = "latex")
  expect_match(msg, "apaquarto-pdf", fixed = TRUE)
  expect_match(msg, "format: apaquarto-docx", fixed = TRUE)
})

test_that("the mismatch message also names the line the other direction", {
  # rendered under apaquarto-docx while calling apa.quarto.pdf()
  msg <- apa_quarto_target_message(called = "pdf", actual = "docx")
  expect_match(msg, "apaquarto-docx", fixed = TRUE)
  expect_match(msg, "format: apaquarto-pdf", fixed = TRUE)
})

test_that("the mismatch message names an HTML mismatch too", {
  msg <- apa_quarto_target_message(called = "html", actual = "docx")
  expect_match(msg, "apaquarto-docx", fixed = TRUE)
  expect_match(msg, "format: apaquarto-html", fixed = TRUE)
})

test_that("a rendering document whose target disagrees with the call name warns, naming the fix, AND still renders the table", {
  # apa_reset_version_warnings(): the mismatch warning is routed through the
  # SAME once-per-document registry page-geometry.R/docx-landscape.R use,
  # and apa_render_id() reads a CONSTANT
  # "<no-document>" outside a real knit -- so without resetting, a test
  # earlier in this same R session that already warned for this exact
  # (called, actual) pair would silence this one, and this assertion would
  # pass for the wrong reason (nothing fired) rather than the right one.
  apa_reset_version_warnings()
  on.exit(apa_reset_version_warnings(), add = TRUE)
  local_mocked_bindings(apa_quarto_actual_format = function() "docx")
  # Content is asserted on the RETURN VALUE, not just the warning: an
  # implementation that warned and then returned NULL would pass an
  # expect_warning()-only test, which asserts nothing about the "forgiving,
  # never refuses" half of the design -- its entire point. Assigning INSIDE the
  # expect_warning() expression, not to its own return value: expect_warning()
  # returns the CONDITION object it caught, not apa.quarto.pdf()'s result.
  expect_warning(out <- apa.quarto.pdf(apa.cor.table(attitude[, 1:3])),
                 "format: apaquarto-pdf", fixed = TRUE)
  expect_match(as.character(out), "+===", fixed = TRUE)
})

test_that("the mismatch warning fires ONCE per (called, actual) pair, not once per table", {
  # Without the once-per-pair registry this fires 8 times in an 8-table
  # document (measured). A second call with the SAME (called, actual) pair in
  # the same "document" must be silent; a DIFFERENT called name against the
  # same actual render is a different fact and must still be told.
  apa_reset_version_warnings()
  on.exit(apa_reset_version_warnings(), add = TRUE)
  local_mocked_bindings(apa_quarto_actual_format = function() "docx")
  expect_warning(apa.quarto.pdf(apa.cor.table(attitude[, 1:3])),
                 "format: apaquarto-pdf", fixed = TRUE)
  expect_no_warning(apa.quarto.pdf(apa.cor.table(attitude[, 1:3])))
  expect_warning(apa.quarto.html(apa.cor.table(attitude[, 1:3])),
                 "format: apaquarto-html", fixed = TRUE)
})

test_that("a rendering document whose target AGREES with the call name is silent", {
  apa_reset_version_warnings()
  on.exit(apa_reset_version_warnings(), add = TRUE)
  local_mocked_bindings(apa_quarto_actual_format = function() "docx")
  expect_no_warning(apa.quarto.docx(apa.cor.table(attitude[, 1:3])))
})

test_that("an UNKNOWN target (not currently rendering) never warns", {
  # apa_quarto_actual_format() reads NA outside any knit -- there is nothing
  # to compare the call name against, so forgiving naming has nothing to
  # forgive and nothing to say.
  apa_reset_version_warnings()
  on.exit(apa_reset_version_warnings(), add = TRUE)
  expect_no_warning(apa.quarto.html(apa.cor.table(attitude[, 1:3])))
})

test_that("apa.quarto.pdf() on an ACTUAL LaTeX render composes exactly ONE meta div", {
  # The landmine: apa_quarto_markdown_block()
  # already composes its own .apa-table-meta div, so a dispatcher that both
  # calls it AND wraps a second div around the result sends TWO divs to
  # apatablemeta.lua, whose walk has no early exit -- the SECOND one wins,
  # silently. Assert the count directly rather than trusting "it rendered".
  local_mocked_bindings(apa_latex_target = function() TRUE)
  out <- as.character(apa.quarto.pdf(apa.cor.table(attitude[, 1:3])))
  # apa_md_count_matches(), not a hand-rolled gregexpr()/identical(-1L) check
  # -- that exact comparison is a known trap (see apa_md_count_matches() in
  # R/render-markdown.R): gregexpr()'s no-match sentinel carries
  # match.length/useBytes attributes, so it is never identical() to a bare
  # -1L, and the comparison silently falls through to a length()
  # branch -- reading a ZERO-match div count as 1L and passing.
  expect_equal(apa_md_count_matches("apa-table-meta", out), 1L)
  expect_true(grepl("\\begin{longtable}", out, fixed = TRUE))
})

test_that("apa.quarto.docx() off the LaTeX path composes exactly ONE meta div", {
  local_mocked_bindings(apa_latex_target = function() FALSE)
  out <- as.character(apa.quarto.docx(apa.cor.table(attitude[, 1:3])))
  # See the sibling PDF test above: apa_md_count_matches(), not the
  # gregexpr()/identical(-1L) comparison that reads a zero-match count as 1L.
  expect_equal(apa_md_count_matches("apa-table-meta", out), 1L)
  expect_true(grepl("+===", out, fixed = TRUE))
})

test_that("title/note arguments reach the table on the non-LaTeX path (no flextable fallback)", {
  # inherits() is checked on the UN-COERCED return value --
  # `expect_false(inherits(out, "flextable"))` after
  # `out <- as.character(...)` is unconditionally TRUE regardless of what
  # apa.quarto.html() actually returned, since as.character() strips the
  # flextable class either way. test-quarto-table-meta.R's equivalent
  # assertion does this correctly; mirrored here.
  local_mocked_bindings(apa_latex_target = function() FALSE)
  out <- apa.quarto.html(apa.cor.table(attitude[, 1:3]),
                         title = "My Own Title",
                         note = "My own note.")
  expect_false(inherits(out, "flextable"))
  txt <- as.character(out)
  expect_match(txt, "My Own Title", fixed = TRUE)
  expect_match(txt, "My own note.", fixed = TRUE)
})

test_that("font.size overrides the ladder on the LaTeX path too", {
  local_mocked_bindings(apa_latex_target = function() TRUE)
  out <- as.character(apa.quarto.pdf(apa.cor.table(attitude[, 1:3]), font.size = 8))
  expect_true(grepl("\\fontsize{8", out, fixed = TRUE))
})

test_that("font.size overrides the ladder on the markdown path", {
  # apa_quarto_docx_target() mocked TRUE alongside apa_latex_target(): the size
  # marker is a docx-only mechanism (apatablesize.lua stands down on any
  # non-docx FORMAT and no HTML CSS consumes it), so this specifically
  # docx-flavoured assertion must exercise the docx gate rather than fall
  # through the "no real knit session" default of FALSE.
  local_mocked_bindings(apa_latex_target = function() FALSE,
                         apa_quarto_docx_target = function() TRUE)
  out <- as.character(apa.quarto.docx(apa.cor.table(attitude[, 1:3]), font.size = 8))
  expect_true(grepl("apa-table-size-8", out, fixed = TRUE))
})

test_that("apa.quarto.template() writes the call matching the requested target", {
  x <- apa.cor.table(attitude[, 1:3])
  capture.output(txt_docx <- apa.quarto.template(x, target = "docx"))
  capture.output(txt_pdf  <- apa.quarto.template(x))
  expect_match(txt_docx, "apa.quarto.docx", fixed = TRUE)
  expect_match(txt_pdf,  "apa.quarto.pdf",  fixed = TRUE)
})

# target = "revealjs" is accepted and writes a chunk calling
# apa.quarto.revealjs() -- see test-quarto-slide.R
# ("apa.quarto.template(target = 'revealjs') writes a slide chunk"), which
# also asserts the absent tbl-cap/label lines this test does not repeat.
test_that("apa.quarto.template() accepts target = 'revealjs' and writes the slide call", {
  x <- apa.cor.table(attitude[, 1:3])
  capture.output(txt_slide <- apa.quarto.template(x, target = "revealjs"))
  expect_match(txt_slide, "apa.quarto.revealjs", fixed = TRUE)
})

test_that("apa.quarto.template() defaults to the pdf form and never names the retired function", {
  capture.output(txt <- apa.quarto.template(apa.cor.table(attitude[, 1:3])))
  # The object is passed as an ARGUMENT, not piped. Asserted with
  # the object name in place rather than as "apa.quarto.pdf()" with empty
  # parentheses -- that form appears nowhere in the chunk,
  # and a match on the bare function name would also be satisfied by the
  # commented build line.
  expect_match(txt, "apa.quarto.pdf(x = mytable)", fixed = TRUE)
  expect_false(grepl("apa.quarto.table", txt, fixed = TRUE))
})
