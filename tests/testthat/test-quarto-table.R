# OLD_FLEXTABLE_FLOOR is 5 pt: the hard floor the deleted flextable fitter
# (APA_MIN_FONT_SIZE, gone in Task 10) stopped at. It is kept here as a
# LITERAL, not imported, because the assertions below are about walking PAST
# it -- a reference point in the package's history, not a value the package
# still holds.
OLD_FLEXTABLE_FLOOR <- 5

# The layout ladder (D7/D7b) and the public chunk function (D8).
#
# WHERE EACH TEST'S WIDTH COMES FROM, AND WHY IT IS NOT THE MODEL UNDER TEST.
#
# The ladder's decisions are made from apa_latex_natural_width(). A test that
# built its own "8 inch table" by asking apa_latex_natural_width() for one
# would be comparing the model against itself, and would keep passing while
# the model drifted -- this project has shipped that mistake twice. So the
# widths here come from two sources, neither of which is the model:
#
#   1. tests/testthat/fixtures/phase2-measured-widths.rds. Every `actual_in`
#      in that file is a sum of LaTeX's own \LT@entry column widths, read out
#      of a rendered .aux (Task 1/Task 4; artifacts in
#      dev/factcheck/phase2-preflight/). The rotate/do-not-rotate assertions
#      below state what LaTeX's OWN measurement says should happen -- e.g.
#      apa.cor.table(attitude) really is 5.2903 in at 10 pt and really does
#      fit a 6.5 in page -- so a model drift big enough to flip a verdict
#      fails the test rather than being ratified by it.
#
#   2. fake_plan_of_width(): pure \tabcolsep arithmetic, containing NO font
#      metric at all. Task 5's emitter writes "@{}" on both outer edges and
#      APA_LATEX_COLGAP_PT (4 pt) between columns, so an n-column table
#      carries exactly (4n - 4) pt of inter-column space, and \tabcolsep does
#      not scale with the type size. A table with enough columns is therefore
#      wider than the page at EVERY font size, by arithmetic, whatever the
#      font model says. That is the only property the below-the-floor tests
#      need, and it is a LOWER bound -- see the helper's own comment.

# A plan guaranteed to be AT LEAST `inches` wide at every font size.
#
# The guarantee is LaTeX's, not the width model's: n columns of Task 5's
# longtable carry (4n - 4) pt of \tabcolsep (4 pt per gap, "@{}" on both outer
# edges), and that term is font-independent, so it alone puts a floor under
# the table's width at any size. Cell content only adds to it. This is a lower
# bound and nothing more -- it says "no smaller than", never "exactly" -- so
# it is used only where the assertion is "too wide to fit, at any size".
fake_plan_of_width <- function(inches) {
  gap <- APA_LATEX_COLGAP_PT
  n <- as.integer(ceiling((inches * 72.27 + gap) / gap))
  sp <- apa_spec_start(1L)
  for (j in seq_len(n)) sp <- apa_col(sp, sprintf("c%d", j), "r", "0")
  apa_display_plan(apa_spec_finish(sp, title = "A Very Wide Table"))
}

# Built once each: these are 1-1.5 s of flextable font measurement apiece and
# several assertions need the same objects.
#
# huge_plan cannot fit a 9 in page at ANY size -- its \tabcolsep term alone is
# 9.02 in -- so it is the CLIPPING case, where the ladder warns that content
# will be lost.
huge_plan <- fake_plan_of_width(9.0)

# shrink_plan is the other, and much more important, case: 60 columns, whose
# \tabcolsep term is 3.27 in, so it CAN be made to fit -- but only by going
# below 6 pt (measured: 4.49 pt, 8.50 in on a 9 in rotated page). This is the
# branch D7b is about and the branch that had no test at all.
shrink_plan <- local({
  sp <- apa_spec_start(1L)
  for (j in seq_len(60L)) sp <- apa_col(sp, sprintf("c%d", j), "r", "0")
  apa_display_plan(apa_spec_finish(sp, title = "A Table That Must Shrink"))
})

measured <- readRDS(test_path("fixtures", "phase2-measured-widths.rds"))
plan_of <- function(nm) apa_display_plan(measured[[nm]]$spec)

test_that("the ladder rotates before it shrinks", {
  # Independent ground truth, from LaTeX's own \LT@entry sums at 10 pt:
  #   cor_attitude   5.2903 in  -- fits a 6.5 in upright page
  #   twoway         2.6047 in  -- fits comfortably
  #   cor_mtcars     7.9877 in  -- too wide upright, fits a 9 in rotated page
  #   reg_multiblock 6.6412 in  -- too wide upright, fits a 9 in rotated page
  expect_lt(measured$cor_attitude$actual_in, 6.5)
  expect_lt(measured$twoway$actual_in, 6.5)
  expect_gt(measured$cor_mtcars$actual_in, 6.5)
  expect_lt(measured$cor_mtcars$actual_in, 9.0)
  expect_gt(measured$reg_multiblock$actual_in, 6.5)
  expect_lt(measured$reg_multiblock$actual_in, 9.0)

  # The whole object is deliberately NOT compared: apa_layout_decide() also
  # reports `fits` and `width`, and an expect_equal() against a two-element
  # list would fail on the extra fields while saying nothing about the
  # decision. The fields the ladder exists to produce are asserted one by one.
  narrow <- apa_layout_decide(plan_of("cor_attitude"))
  expect_equal(narrow$size, 10)
  expect_false(narrow$rotate)
  expect_true(narrow$fits)

  small <- apa_layout_decide(plan_of("twoway"))
  expect_equal(small$size, 10)
  expect_false(small$rotate)

  for (nm in c("cor_mtcars", "reg_multiblock")) {
    wide <- apa_layout_decide(plan_of(nm))
    expect_true(wide$rotate, info = nm)
    expect_equal(wide$size, 10, info = nm)   # rotate, do NOT shrink
    expect_true(wide$fits, info = nm)
  }

  # NEVER CLIP (D7b), and this is the assertion that used to lie. It read
  # `expect_lt(huge$size, 6)` commented "never clip", while
  # apa_size_for_width() and apa_latex_table() both hard-clamped at 5 pt --
  # so it passed on a table that WAS clipped. Both halves are now stated:
  # the size goes past 6, past 5 and past 1, AND the table fits.
  shrunk <- suppressWarnings(apa_layout_decide(shrink_plan))
  expect_lt(shrunk$size, 6)
  expect_lt(shrunk$size, OLD_FLEXTABLE_FLOOR)
  expect_true(shrunk$rotate)
  expect_true(shrunk$fits)                       # nothing is cut off
  expect_lte(shrunk$width, 9.0)

  # The old 5 pt clamp is what this table proves is gone: at 5 pt it does NOT
  # fit the rotated page, so the shipped-Task-9 ladder would have set it at
  # 5 pt and lost columns off the right edge, silently.
  expect_gt(apa_latex_natural_width(shrink_plan, OLD_FLEXTABLE_FLOOR), 9.0)
})

test_that("the Word destination is charged for its fixed cell padding (B6, Phase 1.12)", {
  # Word's <w:tblCellMar> is FIXED per-column padding (15 twips a side, D39)
  # the character model cannot see, so before Phase 1.12 the ladder shrank a
  # dense docx table until it JUST fit and the padding pushed it back over --
  # measured in real Word (dev/factcheck/phase112-padding): the widest
  # column paid, wrapping its labels ("10. Var10" split across two lines
  # while "9. Var9" fit). col_overhead_in charges the ladder for it; the
  # LaTeX destination passes 0 because its own \tabcolsep term is already
  # in the model.
  plan <- plan_of("cor_attitude")
  pad <- plan$n_col * APA_WORD_CELL_PADDING_IN

  # THE FLIP, at the boundary the defect lives on: a page that holds the
  # CONTENT at 10 pt but not content plus padding. Without the overhead the
  # ladder calls it a fit; with it, the same table on the same page rotates
  # (rotate before shrink, D7) -- exactly the decision the old model got
  # wrong by "fitting" a table whose padding then overflowed the column.
  w10 <- apa_layout_decide(plan)$width
  geom_between <- list(width = w10 + pad / 2, height = 11,
                       config = "default", unknown = NULL)
  geom_land <- list(width = 9, height = 8.5,
                    config = "default", unknown = NULL)
  d0 <- apa_layout_decide(plan, geom = geom_between,
                          geom_landscape = geom_land)
  expect_false(d0$rotate)
  expect_equal(d0$size, 10)
  expect_true(d0$fits)
  dW <- apa_layout_decide(plan, geom = geom_between,
                          geom_landscape = geom_land,
                          col_overhead_in = APA_WORD_CELL_PADDING_IN)
  expect_true(dW$rotate)
  expect_equal(dW$size, 10)
  expect_true(dW$fits)
  # The overhead is charged on the WIDTH side -- the space the table really
  # occupies -- so warnings keep quoting the true page width.
  expect_equal(dW$width, d0$width + pad, tolerance = 1e-8)

  # An ordinary table on the real page is untouched by the charge: same
  # size, same orientation. The correction moves boundary tables only.
  same0 <- apa_layout_decide(plan)
  sameW <- apa_layout_decide(plan,
                             col_overhead_in = APA_WORD_CELL_PADDING_IN)
  expect_equal(sameW$size, same0$size)
  expect_equal(sameW$rotate, same0$rotate)

  # A table the ladder must shrink shrinks FURTHER once the padding is
  # real: the ladder used to spend exactly this headroom (roadmap 1.12:
  # "the ladder shrinks until the table *just* fits").
  s0 <- suppressWarnings(apa_layout_decide(shrink_plan))
  sW <- suppressWarnings(apa_layout_decide(
    shrink_plan, col_overhead_in = APA_WORD_CELL_PADDING_IN))
  expect_lt(sW$size, s0$size)
  expect_true(sW$fits)
})

test_that("going below 6 pt warns in ONE sentence naming apa.save", {
  # The branch that shrinks and FITS. Untested until fix round 1, and
  # unreachable before it -- the 5 pt clamp meant a table needing less was
  # clipped rather than set smaller.
  expect_warning(apa_layout_decide(shrink_plan), "apa\\.save")
  w <- tryCatch(apa_layout_decide(shrink_plan),
                warning = function(w) conditionMessage(w))
  # A warning string containing a newline is TRUNCATED off the page edge in a
  # knitted PDF (knitr only wraps single-line messages), and pdftotext still
  # reports the off-page text, so grep cannot catch it after the fact.
  expect_false(grepl("\n", w))
  expect_true(nzchar(w))
  # it must describe what actually happened -- the table is COMPLETE
  expect_match(w, "no column cut off", fixed = TRUE)
  expect_false(grepl("will still be cut off", w, fixed = TRUE))
})

test_that("a table no font size can fit warns that it WILL be cut off", {
  # The one honest limit left in D7b, and it is a different warning:
  # \tabcolsep is 4 pt per gap whatever the type size and every column rounds
  # up to 0.01 in, so past about 136 columns nothing fits. Saying "nothing is
  # ever cut off" here would be the falsehood this round exists to remove.
  d <- suppressWarnings(apa_layout_decide(huge_plan))
  expect_false(d$fits)
  expect_equal(d$size, APA_LATEX_MIN_EMITTABLE_SIZE)
  w <- tryCatch(apa_layout_decide(huge_plan),
                warning = function(w) conditionMessage(w))
  expect_false(grepl("\n", w))
  expect_match(w, "apa\\.save")
  expect_match(w, "will still be cut off", fixed = TRUE)
  expect_match(w, "does not shrink with the type", fixed = TRUE)
})

test_that("an explicit size that misses names the size that WOULD fit (F1)", {
  # Fable pre-push review 2026-08-09. The explicit-size branch used to raise
  # apa_ladder_no_fit_message() for ANY miss, claiming "no font size can fit
  # it" -- FALSE for an ordinary wide table: cor_mtcars fits the rotated page
  # at 10 pt (the automatic-ladder test above proves so) while size = 12
  # misses it. The message now names the requested size as the cause and
  # offers a size that fits.
  plan <- plan_of("cor_mtcars")
  w <- tryCatch(apa_layout_decide(plan, size = 12),
                warning = function(w) conditionMessage(w))
  expect_false(grepl("\n", w))                       # knitr truncation trap
  expect_false(grepl("no font size can fit", w, fixed = TRUE))
  expect_match(w, "12 pt requested", fixed = TRUE)
  expect_match(w, "font.size = ", fixed = TRUE)
  # the size the message offers must ITSELF fit -- an offer that does not is
  # this defect rebuilt inside its own fix
  offered <- as.numeric(sub(".*font\\.size = ([0-9.]+) or smaller.*", "\\1", w))
  expect_false(is.na(offered))
  expect_lte(apa_latex_natural_width(plan, offered), 9.0)
  # and the condition class is unchanged -- the docx path muffles by it
  expect_warning(apa_layout_decide(plan, size = 12),
                 class = "apaTables_ladder_size")
})

test_that("the honest no-fit message still fires where it is TRUE (F1)", {
  # huge_plan's \tabcolsep term alone exceeds the rotated page, so here "no
  # font size can fit it" is a fact, and the explicit-size branch must keep
  # saying exactly that rather than offering a size that cannot exist.
  w <- tryCatch(apa_layout_decide(huge_plan, size = 8),
                warning = function(w) conditionMessage(w))
  expect_match(w, "no font size can fit", fixed = TRUE)
  expect_match(w, "will still be cut off", fixed = TRUE)
  expect_false(grepl("\n", w))
})

test_that("an invalid font.size dies with a named error, not a bare one (F3)", {
  # Before this fix apa.quarto.pdf(t, font.size = "abc") died mid-render
  # with base R's "missing value where TRUE/FALSE needed" -- loud (correct)
  # but naming neither the argument nor the fix. Validated once at
  # apa_quarto_render(), the chokepoint every public route passes through.
  expect_error(apa_quarto_font_size_value("abc"), "font.size must be")
  expect_error(apa_quarto_font_size_value(c(10, 12)), "font.size must be")
  expect_error(apa_quarto_font_size_value(NA), "font.size must be")
  expect_error(apa_quarto_font_size_value(-3), "font.size must be")
  expect_error(apa_quarto_font_size_value(numeric(0)), "font.size must be")
  # what always worked keeps working: NULL passes through, and a
  # numeric-looking character is coerced exactly as the downstream code
  # always coerced it
  expect_null(apa_quarto_font_size_value(NULL))
  expect_identical(apa_quarto_font_size_value(10), 10)
  expect_identical(apa_quarto_font_size_value("9.5"), 9.5)
})

test_that("the sub-6pt warning does NOT fire for an ordinary table", {
  # An absence assertion, and it is worth something only because the test
  # above constructs the case where the warning DOES fire and watches it.
  expect_no_warning(apa_layout_decide(plan_of("cor_attitude")))
  expect_no_warning(apa_layout_decide(plan_of("cor_mtcars")))
})

test_that("the ladder's decision is made from ONE font measurement", {
  # apa_latex_natural_width() re-runs flextable's autofit() on every call
  # (measured: 0.66 s on a 14-column table). A ladder that measured once per
  # candidate size would take seconds per table. The measurement is therefore
  # taken once, by apa_latex_content_at_12(), and every candidate is priced
  # from that vector -- including apa_size_for_width(), which accepts it.
  real <- apa_latex_content_at_12
  calls <- 0L
  testthat::local_mocked_bindings(
    apa_latex_content_at_12 = function(plan) {
      calls <<- calls + 1L
      real(plan)
    })
  # cor_mtcars rotates AND still fits at 10 pt, so this exercises the
  # rotate branch; the shrink branch below is the one that would re-measure
  # if apa_size_for_width() were called without the pre-measured vector.
  apa_layout_decide(plan_of("cor_mtcars"))
  expect_equal(calls, 1L)

  calls <- 0L
  suppressWarnings(apa_layout_decide(huge_plan))
  expect_equal(calls, 1L)
})

test_that("apa_size_for_width() takes a pre-measured content vector", {
  plan <- plan_of("cor_attitude")
  content <- apa_latex_content_at_12(plan)
  expect_equal(apa_size_for_width(plan, 3.0, content_12 = content),
               apa_size_for_width(plan, 3.0))
})

test_that("a rotated table asks apaquarto for a landscape page", {
  # The channel: R decides, and the decision reaches apaquarto as the
  # `apa-landscape` attribute apafloatlatex.lua reads. Outside a knit there is
  # no chunk to mark, so the request is a no-op that reports FALSE rather than
  # erroring.
  expect_false(apa_request_landscape())
})

test_that("apa_quarto_latex_block() emits ONE complete unstarred longtable", {
  # Exercised directly rather than through apa.quarto.pdf(): the PUBLIC
  # dispatcher's own LaTeX-vs-markdown routing has its own coverage in
  # test-quarto-entrypoints.R, and this test is about the LONGTABLE shape,
  # which apa_quarto_latex_block() alone is responsible for.
  # Task 3 fix round 1: tests/testthat/fixtures/phase2-measured-widths.rds
  # used to carry landscape = TRUE baked into every entry, left over from
  # before Task 2 made NA apa.cor.table()'s own default (D63) -- inert until
  # Task 3 wired the argument into apa_quarto_latex_block(), and then enough
  # to raise the "cannot rotate" warning this test never asked for (this
  # test is about the LONGTABLE SHAPE, not landscape). The fixture itself was
  # regenerated with landscape = NA on every entry (confirmed by re-running
  # test-render-latex.R's width-regression suite unchanged), so no local
  # workaround is needed here any more.
  x <- measured$cor_attitude$spec
  out <- apa_quarto_latex_block(x)
  expect_true(inherits(out, "knit_asis"))
  txt <- as.character(out)
  expect_length(txt, 1L)

  # D9: the emitter writes NO caption, which is what makes it apaquarto's job
  # to splice one in -- and the spliced caption is what keeps the environment
  # unstarred. Emitting our own \caption kills the render inside Quarto's
  # filter chain; emitting none and letting apaquarto supply it is the only
  # route that keeps the rows.
  expect_false(grepl("\\caption", txt, fixed = TRUE))
  expect_equal(lengths(regmatches(txt, gregexpr("\\begin{longtable}", txt,
                                                fixed = TRUE))), 1L)
  expect_equal(lengths(regmatches(txt, gregexpr("\\end{longtable}", txt,
                                                fixed = TRUE))), 1L)
  expect_false(grepl("longtable*", txt, fixed = TRUE))
  # D12: asis_output() is raw-TeX only for a SELF-CONTAINED environment.
  # Pandoc re-parses neighbouring lines as markdown (it turned a trailing "%"
  # into "\%"), so nothing may precede \begin{longtable} or follow
  # \end{longtable} on the way out.
  expect_true(startsWith(txt, "\\begin{longtable}"))
  expect_true(endsWith(txt, "\\end{longtable}"))
})

test_that("the native LaTeX path registers no ulem dependency (Step 6)", {
  # apa_restore_emph() USED TO exist because flextable's LaTeX dependencies
  # load ulem WITHOUT [normalem], redefining \emph as underlining
  # document-wide. The native path never loads flextable, so it must not
  # register any such correction either -- that would add a package nothing
  # else in the document needs. Rasterized proof of the appearance is in the
  # Task 9 report.
  withr::local_options(knitr.in.progress = TRUE)
  knitr::knit_meta(clean = TRUE)
  apa_quarto_latex_block(measured$twoway$spec)
  expect_length(knitr::knit_meta(clean = TRUE), 0L)

  # This test used to carry a companion half that called apa_restore_emph()
  # directly and asserted a "ulem" entry DID appear, proving the check above
  # could fire. Task 9 (Phase 1.6a) deleted apa_restore_emph() as confirmed
  # dead -- the render-based measurement is in the Task 9 report, and "apa_
  # restore_emph() no longer exists" below is the guard that keeps it deleted
  # -- so there is no longer a companion call to make. The assertion above is
  # unweakened: it still passes a real knit_meta registry through a real
  # function call and reads it back, on the ONE path (the native LaTeX
  # emitter) that could plausibly regress.
})

test_that("apa.quarto.pdf() off the LaTeX path no longer falls back to a flextable (D14 complete, Task 8)", {
  # SUPERSEDES "apa.quarto.table() falls back to a flextable off the LaTeX
  # path". That branch was labelled INTERIM in its own source comment and
  # survived only until Phase 1.6a's pandoc emitter existed; Task 8 retires
  # apa.quarto.table() and wires apa.quarto.pdf() to
  # apa_quarto_markdown_block() off the LaTeX path instead (D24: one emitter
  # for every non-PDF destination).
  local_mocked_bindings(apa_latex_target = function() FALSE)
  out <- apa.quarto.pdf(measured$twoway$spec)
  expect_false(inherits(out, "flextable"))
  expect_true(grepl("+===", as.character(out), fixed = TRUE))
})

test_that("apa_flextable() is no longer exported", {
  # D8: un-exporting is free ONLY now -- apa_flextable() arrived with the 4.0
  # work, which has never been pushed, so the name has no external users
  # today and would have them permanently after the next release.
  expect_false("apa_flextable" %in% getNamespaceExports("apaTables"))
  # apa.quarto.table() is retired outright too (D30, Task 8) -- see
  # test-quarto-entrypoints.R for the dedicated coverage of that.
  expect_false("apa.quarto.table" %in% getNamespaceExports("apaTables"))
})

test_that("apa_restore_emph() no longer exists", {
  # Task 9 (Phase 1.6a): the ulem/normalem fix for flextable's LaTeX
  # dependencies was deleted as confirmed dead, not merely un-exported --
  # apa_flextable() can no longer reach a LaTeX destination at all (D14/D24),
  # so a guard against underlined "Note." text in one has nothing left to
  # guard. A comment saying so is not enough on its own (see
  # tests/testthat/test-apa-save.R's retargeted knit_meta check, which used
  # to prove this function fired); this asserts the removal directly, so the
  # function cannot creep back silently.
  expect_false(exists("apa_restore_emph", where = asNamespace("apaTables"),
                       inherits = FALSE))
})

test_that("a chunk that ASKS for a rotated page it cannot have says so", {
  # The one case worth a warning: the author wrote apa-landscape: true by
  # hand, and no chunk without a tbl- label can be rotated. The table is
  # fitted upright and complete either way -- the warning is the net beside
  # the fix, and it names the fix. The automatic case stays silent, which the
  # companion below pins.
  #
  # Task 3 WIDENED this warning's wording so it stops naming only the chunk
  # option: it now fires the same way whether the request came from
  # apa-landscape: true or from the landscape = TRUE builder argument, so the
  # text no longer contains the literal string "apa-landscape" -- the
  # assertion below was rewritten (D66) to match the fix it now names
  # instead of the source of the request.
  withr::local_options(knitr.in.progress = TRUE)
  local_mocked_bindings(
    apa_chunk_landscape = function() TRUE,
    apa_landscape_executable = function() FALSE)
  expect_warning(apa_quarto_latex_block(apa.cor.table(attitude)),
                 "tbl-")
  w <- tryCatch(apa_quarto_latex_block(apa.cor.table(attitude)),
                warning = function(w) conditionMessage(w))
  # knitr TRUNCATES a multi-line warning off the right edge of a rendered PDF
  # page, where pdftotext still reports the text nobody can see.
  expect_false(grepl("\n", w))
  expect_match(w, "rotated (landscape) page", fixed = TRUE)
  expect_match(w, "#| label: tbl-mytable", fixed = TRUE)
})

test_that("the automatic rotate-then-fall-back path warns about NOTHING", {
  # The companion, and the reason the assertion above is worth making: this
  # one takes the same branch without the author having asked, and it must be
  # silent -- master shrank silently and kept every column, and a warning on
  # every unlabelled wide table would be noise, not information.
  #
  # apa_chunk_landscape() mocked to NA, not FALSE: under the Phase 1.7
  # tri-state, FALSE now means the author explicitly FORBADE rotation, and NA
  # is "no opinion", which is what "automatic" means here. (Fix round 1: this
  # mock used to read FALSE, which meant "no opinion" under the pre-1.7
  # two-state contract; after the tri-state landed it silently became a test
  # of FORBID instead, under an unchanged name, and still passed -- see the
  # sibling test below for FORBID's own coverage.)
  withr::local_options(knitr.in.progress = TRUE)
  local_mocked_bindings(
    apa_chunk_landscape = function() NA,
    apa_landscape_executable = function() FALSE)
  expect_no_warning(apa_quarto_latex_block(apa.cor.table(attitude)))
})

test_that("an explicit FORBID does not warn either, even where rotation could not have executed anyway", {
  # A different question from the automatic case just above: here the author
  # DID have an opinion (apa-landscape: false) and got exactly what they
  # asked for -- the table shrinks to the upright page instead of rotating --
  # so there is nothing to warn about. The "cannot have it" warning two tests
  # up only fires when the request was for TRUE and could not be honoured.
  # This is also the caller-level check that FORBID (the user's choice,
  # resolved through apa_landscape_request() from apa_chunk_landscape() and
  # the builder argument) and CANNOT-EXECUTE (the render's limitation, here
  # apa_landscape_executable()) are independent facts at the call site
  # (R/quarto-table.R:812's `landscape = requested` and
  # `can_execute_rotation = executable`), not one implemented via the other.
  withr::local_options(knitr.in.progress = TRUE)
  local_mocked_bindings(
    apa_chunk_landscape = function() FALSE,
    apa_landscape_executable = function() FALSE)
  expect_no_warning(apa_quarto_latex_block(apa.cor.table(attitude)))
})

test_that("a rotation that cannot be EXECUTED is not decided (C1)", {
  # The regression this argument exists for. apa_request_landscape() can only
  # mark a chunk Quarto turns into a float, and its answer used to be thrown
  # away: the ladder rotated, the request refused, and the table was priced
  # against the 9 in rotated page and printed on the 6.5 in upright one.
  plan <- plan_of("cor_mtcars")

  rotatable <- apa_layout_decide(plan)
  expect_true(rotatable$rotate)
  expect_equal(rotatable$size, APA_QUARTO_DEFAULT_SIZE)
  expect_gt(rotatable$width, 6.5)      # it does NOT fit the upright page

  # The same table where nothing can rotate: the ladder must fall through to
  # the shrink rung and land INSIDE the upright budget. Asserting `fits` alone
  # would not do it -- `fits` is measured against whatever budget the ladder
  # chose, so a wrongly rotated table also reports TRUE.
  stuck <- apa_layout_decide(plan, can_execute_rotation = FALSE)
  expect_false(stuck$rotate)
  expect_lt(stuck$size, APA_QUARTO_DEFAULT_SIZE)
  expect_true(stuck$fits)
  expect_lte(stuck$width, 6.5)

  # An explicit request cannot override it either: the author asking for a
  # rotated page does not create a float for apaquarto to rotate.
  forced <- apa_layout_decide(plan, landscape = TRUE,
                              can_execute_rotation = FALSE)
  expect_false(forced$rotate)
  expect_lte(forced$width, 6.5)
})

test_that("only a tbl- label makes a rotation executable", {
  # Quarto builds a table FLOAT out of a code cell only when its label begins
  # tbl-, and apafloatlatex.lua's landscapewrap has nothing to wrap without
  # one. Outside a knit there is no chunk at all.
  expect_false(apa_landscape_executable())

  withr::local_options(knitr.in.progress = TRUE)
  local_mocked_bindings(apa_chunk_label = function() NA_character_)
  expect_false(apa_landscape_executable())
})

test_that("a tbl- label makes it executable and any other label does not", {
  withr::local_options(knitr.in.progress = TRUE)
  local_mocked_bindings(apa_chunk_label = function() "tbl-mytable")
  expect_true(apa_landscape_executable())
})

test_that("a plain label is NOT executable", {
  withr::local_options(knitr.in.progress = TRUE)
  local_mocked_bindings(apa_chunk_label = function() "mytable")
  expect_false(apa_landscape_executable())
})

test_that("landscape labels do not survive from one render into the next", {
  # The registry is package state, so in an R session that renders twice a
  # label registered by document A would still stamp apa-landscape on a
  # same-named chunk in document B -- rotating a page nobody asked to rotate.
  # knitr restores its own chunk hook between renders, which is the signal.
  old_labels <- apa_landscape_env$labels
  old_hook <- apa_landscape_env$hook
  on.exit({
    apa_landscape_env$labels <- old_labels
    apa_landscape_env$hook <- old_hook
    knitr::knit_hooks$restore()
  }, add = TRUE)

  knitr::knit_hooks$restore()
  apa_landscape_env$labels <- character(0)
  apa_landscape_env$hook <- NULL
  withr::local_options(knitr.in.progress = TRUE)

  # Document A.
  local({
    local_mocked_bindings(apa_chunk_label = function() "tbl-from-a")
    expect_true(apa_request_landscape())
  })
  expect_equal(apa_landscape_env$labels, "tbl-from-a")

  # ...and within the SAME render, a second table adds to the set rather than
  # replacing it. Without this the clearing would be a different defect.
  local({
    local_mocked_bindings(apa_chunk_label = function() "tbl-also-a")
    expect_true(apa_request_landscape())
  })
  expect_setequal(apa_landscape_env$labels, c("tbl-from-a", "tbl-also-a"))

  # Document B: knitr has restored its hooks, so ours is gone.
  knitr::knit_hooks$restore()
  local({
    local_mocked_bindings(apa_chunk_label = function() "tbl-from-b")
    expect_true(apa_request_landscape())
  })
  expect_equal(apa_landscape_env$labels, "tbl-from-b")
})

# --- Task 3, Step 5: landscape = TRUE is accepted and silently ignored on
# an HTML render ------------------------------------------------------------

test_that("landscape = TRUE on a docx render actually produces the rotation wrapper", {
  # Fix round 1 (Important 4): the presence partner the absence assertion
  # just below needs. attitude's correlation table fits the upright page on
  # its own (measured elsewhere in this suite at 5.29 in against 6.5 in), so
  # only the landscape = TRUE argument -- not measurement -- can be what
  # rotates it here; this is the end-to-end proof that Task 3's wiring
  # actually produces the user-visible deliverable on Word, not just that
  # apa_layout_decide() receives the right value (test-landscape-tristate.R
  # already covers that in isolation) and not just that automatic
  # (measurement-driven) rotation still works (test-render-markdown.R's
  # mtcars case covers that).
  forced <- apa.cor.table(attitude, landscape = TRUE)
  testthat::local_mocked_bindings(apa_quarto_docx_target = function() TRUE)
  out <- as.character(apa_quarto_markdown_block(forced))
  expect_true(any(grepl("apa-table-landscape", out, fixed = TRUE)))
})

test_that("landscape = TRUE is accepted and silently ignored on an HTML render", {
  # D32: landscape is a PAGED concept and HTML has no page. The argument is
  # set when the table is BUILT, before any destination is known, so one
  # object can correctly carry landscape = TRUE into a Word render and an
  # HTML one. A warning here would fire on a CORRECT document.
  #
  # apa_quarto_docx_target() mocked FALSE, matching this file's own
  # convention (apa_render_metadata() and apa_latex_target() are mocked the
  # same way, never a knitr internal directly) -- this is a plain testthat
  # call with no real knit session behind it to report a pandoc target at
  # all.
  forced <- apa.cor.table(attitude, landscape = TRUE)
  testthat::local_mocked_bindings(apa_quarto_docx_target = function() FALSE)
  out <- expect_silent(as.character(apa_quarto_markdown_block(forced)))
  expect_false(any(grepl("apa-table-landscape", out, fixed = TRUE)))
})
