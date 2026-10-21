# Landscape acceptance, apa.save() side. This path measures its OWN output
# -- flextable, 12 pt -- against apa_usable_width(rotate, paper), never the
# Quarto/LaTeX prediction. The widths quoted below are measured.

# The FIRST <w:pgSz> in a saved .docx is the table's own section; the LAST is
# officer's untouched document default (no w:orient attribute at all, so it
# always reads "portrait"). Confirmed by inspection of an actual apa.save()
# .docx (two <w:pgSz> elements, in that order) before writing this helper --
# reading the wrong one would make every case in this file come back
# "portrait" and every test pass for the wrong reason.
sect_orient <- function(path) {
     xml <- apa_docx_document_xml(path)
     m <- regmatches(xml, gregexpr("<w:pgSz[^/>]*/>", xml))[[1]]
     if (grepl('w:orient="landscape"', m[[1]], fixed = TRUE)) "landscape" else "portrait"
}

# The table's own <w:tblGrid>, read as twips (sum) and as a column count --
# shared by the shrink-not-clip check below and the paper-differs-contents
# check at the bottom of this file, rather than defined twice.
grid_widths <- function(xml) {
     grid <- regmatches(xml, regexpr("<w:tblGrid>.*?</w:tblGrid>", xml, perl = TRUE))
     sum(as.integer(regmatches(grid,
          gregexpr('(?<=w:w=")[0-9]+', grid, perl = TRUE))[[1]]))
}
grid_col_count <- function(xml) {
     grid <- regmatches(xml, regexpr("<w:tblGrid>.*?</w:tblGrid>", xml, perl = TRUE))
     length(regmatches(grid, gregexpr("<w:gridCol\\b", grid))[[1]])
}

test_that("apa.save() stops rotating a table that does not need it", {
     # apa.2way.table() with marginal means measures 6.18 in by flextable,
     # against a 7.5 in US Letter portrait page -- it used to get a rotated
     # page ONLY because the builder defaulted landscape = TRUE (pre-1.7).
     f <- withr::local_tempfile(fileext = ".docx")
     apa.save(f, apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                                data = goggles, show.marginal.means = TRUE))
     expect_equal(sect_orient(f), "portrait")
})

test_that("apa.save() still rotates a table that does need it", {
     # apa.cor.table(mtcars) measures 12.76 in -- far past even the rotated
     # 10 in US Letter budget.
     f <- withr::local_tempfile(fileext = ".docx")
     apa.save(f, apa.cor.table(mtcars))
     expect_equal(sect_orient(f), "landscape")
})

test_that("apa.save() honours an explicit landscape in both directions", {
     f1 <- withr::local_tempfile(fileext = ".docx")
     tb1 <- apa.cor.table(mtcars, landscape = FALSE)
     apa.save(f1, tb1)
     expect_equal(sect_orient(f1), "portrait")   # forbidden: shrink, do not rotate

     # Pinned rather than left to hold by accident: forbidding rotation must
     # still get the table onto the page by SHRINKING it, never by clipping
     # or dropping a column. Read off the
     # rendered .docx, not inferred from the code path.
     xml1 <- apa_docx_document_xml(f1)
     usable1_twips <- apaTables:::apa_usable_width(FALSE, "us") * 1440
     expect_lte(grid_widths(xml1), usable1_twips)
     expect_equal(grid_col_count(xml1),
                  length(apaTables:::apa_flextable(tb1)$col_keys))

     f2 <- withr::local_tempfile(fileext = ".docx")
     apa.save(f2, apa.1way.table(iv = dose, dv = libido, data = viagra,
                                 landscape = TRUE))
     expect_equal(sect_orient(f2), "landscape")  # forced on a 2.16 in table
})

# --- The cross-destination matrix ------------------------------------------
#
# Two cross-destination checks already exist, in substance, in the rest of
# this file and test-landscape-tristate.R, and are not duplicated here:
#
#   - "The argument beats measurement on apa.save()" IS the existing
#     "apa.save() honours an explicit landscape in both directions" test
#     above: apa.1way.table(..., landscape = TRUE) forced to "landscape" on
#     a 2.16 in table.
#   - The PRECEDENCE CLAIM (chunk beats argument) is already unit-tested in
#     isolation by test-landscape-tristate.R's
#     "apa_landscape_request() puts the chunk option above the argument",
#     in both directions. What is NOT tested anywhere is that the two real
#     Quarto call sites (R/quarto-table.R:631 and :791) actually pass
#     (chunk, argument) to apa_landscape_request() in that order rather than
#     transposed -- a transposition that test-landscape-tristate.R's own
#     "passes the builder argument" tests (lines 173-225) cannot catch,
#     because they leave apa_chunk_landscape() at its real, unmocked NA
#     default, and apa_landscape_request(NA, x) == apa_landscape_request(x,
#     NA) == x for any x. The first test below closes exactly that gap by
#     mocking apa_chunk_landscape() to a value that CONFLICTS with the
#     table's own landscape = argument.

test_that("the chunk option beats the argument at the real Quarto call sites, not only inside apa_landscape_request() itself", {
  # A table whose OWN argument says ROTATE; the chunk (mocked) says FORBID.
  # If either call site passed the two values to apa_landscape_request() in
  # the wrong order, this pair would resolve to TRUE instead of FALSE, and
  # only this test -- not the unit-level precedence test, not the existing
  # "passes the builder argument" tests -- would notice.
  wide <- apa.cor.table(mtcars, landscape = TRUE)

  testthat::local_mocked_bindings(apa_chunk_landscape = function() FALSE)

  captured_latex <- "unset"
  testthat::local_mocked_bindings(
    apa_layout_decide = function(plan, geom = NULL, geom_landscape = NULL,
                                 landscape = NA, can_execute_rotation = TRUE,
                                 size = NULL,
                                 col_overhead_in = 0) {
      captured_latex <<- landscape
      list(size = APA_QUARTO_DEFAULT_SIZE, rotate = FALSE, fits = TRUE, width = 1)
    })
  apa_quarto_latex_block(wide)
  expect_identical(captured_latex, FALSE)   # chunk's FALSE won, not the argument's TRUE

  captured_md <- "unset"
  testthat::local_mocked_bindings(
    apa_layout_decide = function(plan, geom = NULL, geom_landscape = NULL,
                                 landscape = NA, can_execute_rotation = TRUE,
                                 size = NULL,
                                 col_overhead_in = 0) {
      captured_md <<- landscape
      list(size = APA_QUARTO_DEFAULT_SIZE, rotate = FALSE, fits = TRUE, width = 1)
    })
  apa_quarto_markdown_block(wide)
  expect_identical(captured_md, FALSE)      # same precedence, same call shape, Word's own site
})

test_that("Quarto PDF and Quarto Word reach the SAME rotate decision from the SAME chunk override, because they share one page model -- unlike apa.save(), which legitimately does not", {
  # apa.2way.table() with marginals is 3.61 in and fits upright on its own
  # (see test-landscape-tristate.R's "landscape = TRUE rotates a table that
  # does not need it"). Here the CHUNK option (mocked, not the argument)
  # forces rotation, driven through the REAL apa_layout_decide() ladder on
  # both emitters -- no mock of the ladder here, unlike the wiring test
  # above -- and both must actually rotate, because both consult the same
  # 6.5 in / 9.0 in Quarto page.
  narrow <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                           data = goggles, show.marginal.means = TRUE)

  testthat::local_mocked_bindings(apa_chunk_landscape = function() TRUE)

  rotated_latex <- FALSE
  testthat::local_mocked_bindings(
    apa_landscape_executable = function() TRUE,
    apa_request_landscape = function() { rotated_latex <<- TRUE; invisible(TRUE) })
  apa_quarto_latex_block(narrow)
  expect_true(rotated_latex)   # PDF: rotation was actually requested from apaquarto

  testthat::local_mocked_bindings(apa_quarto_docx_target = function() TRUE)
  out_md <- as.character(apa_quarto_markdown_block(narrow))
  expect_match(out_md, "apa-table-landscape", fixed = TRUE)   # Word: the div was actually emitted
})

test_that("the rotation threshold comes from paper, not a literal", {
     # A CONSTRUCTED table sized into the 7.27-7.50 in band -- between A4's
     # portrait usable width and US Letter's. No real apaTables table lands
     # there (see helper-landscape.R); band membership is asserted here so a
     # drift in the width model fails loudly rather than silently inside the
     # helper.
     tb <- band_table_7_27_to_7_50()
     w  <- flextable::flextable_dim(apaTables:::apa_flextable(tb))$widths
     expect_gt(w, apaTables:::apa_usable_width(FALSE, "a4"))   # 7.27
     expect_lt(w, apaTables:::apa_usable_width(FALSE, "us"))   # 7.50

     f_us <- withr::local_tempfile(fileext = ".docx")
     apa.save(f_us, tb, paper = "us")
     expect_equal(sect_orient(f_us), "portrait")

     f_a4 <- withr::local_tempfile(fileext = ".docx")
     apa.save(f_a4, tb, paper = "a4")
     expect_equal(sect_orient(f_a4), "landscape")
})

test_that("the ROTATED threshold also comes from paper", {
     # apa.reg.table() with two blocks measures 10.23 in -- inside the
     # rotated-page gap (US 10.00 / A4 10.69 in). Rotated on both papers;
     # shrunk on US (exceeds 10.00), full size on A4 (fits under 10.69). A
     # shipped example, not a constructed table.
     tb <- apa.reg.table(lm(sales ~ adverts, data = album),
                         lm(sales ~ adverts + airplay + attract, data = album))
     ft <- apaTables:::apa_flextable(tb)
     w  <- flextable::flextable_dim(ft)$widths
     expect_gt(w, apaTables:::apa_usable_width(TRUE, "us"))   # 10.00 -> shrunk
     expect_lt(w, apaTables:::apa_usable_width(TRUE, "a4"))   # 10.69 -> not shrunk

     f_us <- withr::local_tempfile(fileext = ".docx")
     apa.save(f_us, tb, paper = "us")
     expect_equal(sect_orient(f_us), "landscape")

     f_a4 <- withr::local_tempfile(fileext = ".docx")
     apa.save(f_a4, tb, paper = "a4")
     expect_equal(sect_orient(f_a4), "landscape")

     # The US copy was shrunk to fit the 10.00 in rotated budget; the A4 copy,
     # under the wider 10.69 in budget, was left at its natural 10.23 in --
     # the discriminating half of the claim that this table's *contents*,
     # not just its rotation, differ by paper.
     xml_us <- apa_docx_document_xml(f_us)
     xml_a4 <- apa_docx_document_xml(f_a4)
     expect_lt(grid_widths(xml_us), grid_widths(xml_a4))
})

test_that("the measured row DIFFERS between destinations, and that is correct -- not a bug, not a tolerance", {
     # apa.cor.table(attitude), no explicit landscape anywhere -- pure
     # measurement, on both destinations. THE SAME RULE (measure this
     # destination's own output against this destination's own page) fires
     # on both sides and legitimately reaches OPPOSITE answers, because the
     # two destinations do not share a page model: Quarto prices the
     # LaTeX rendering at 10 pt against a 6.5 in portrait / 9.0 in rotated
     # text block; apa.save() prices its OWN flextable rendering at 12 pt
     # against apa_usable_width(rotate, "us") (7.50 in portrait / 10.00 in
     # rotated). Measured directly (not merely asserted here):
     #   Quarto:     5.44 in at 10 pt against 6.50 in -> fits -> UPRIGHT
     #   apa.save(): 8.61 in at 12 pt against 7.50 in -> rotate -> 8.61 in
     #               against 10.00 in rotated budget -> fits -> ROTATED
     # A matrix that forced these to agree would be asserting something
     # FALSE about the two destinations' independently-correct page models.
     tb   <- apa.cor.table(attitude)
     plan <- apa_display_plan(as_spec(tb))

     decision <- suppressWarnings(apa_layout_decide(plan))
     expect_false(decision$rotate)
     expect_lt(decision$width, 6.5)

     f <- withr::local_tempfile(fileext = ".docx")
     apa.save(f, tb)
     expect_equal(sect_orient(f), "landscape")
})
