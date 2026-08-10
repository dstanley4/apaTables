test_that("apa_quarto_revealjs_target() names revealjs and nothing else (pptx removed, Phase 1.9)", {
  local_mocked_bindings(apa_quarto_actual_format = function() "revealjs")
  expect_identical(apa_quarto_revealjs_target(), "revealjs")

  # D88/D90: pptx is no longer a slide destination at all -- it USED to
  # return "pptx" here; now it must read exactly like every other
  # not-a-slide token below.
  for (tok in c("pptx", "latex", "beamer", "docx", "html", "html4", "html5")) {
    local_mocked_bindings(apa_quarto_actual_format = function() tok)
    expect_identical(apa_quarto_revealjs_target(), NA_character_,
                     info = paste("token:", tok))
  }

  # No knit in progress at all.
  local_mocked_bindings(apa_quarto_actual_format = function() NA_character_)
  expect_identical(apa_quarto_revealjs_target(), NA_character_)
})

test_that("apa_quarto_actual_entry() maps revealjs to 'revealjs'; pptx is classified as nothing (Phase 1.9)", {
  expect_identical(apa_quarto_actual_entry("revealjs"), "revealjs")
  # unchanged for the three that already worked
  expect_identical(apa_quarto_actual_entry("latex"), "pdf")
  expect_identical(apa_quarto_actual_entry("docx"), "docx")
  expect_identical(apa_quarto_actual_entry("html5"), "html")
  expect_identical(apa_quarto_actual_entry("gfm"), NA_character_)
  # D88/D90: was the "slide" entry before Phase 1.9 Task 2 renamed the key
  # to "revealjs" (D89). PowerPoint left the Quarto slide family entirely
  # and is deliberately NOT given a "pptx" entry of its own either --
  # apa_quarto_dispatch() checks `actual == "pptx"` directly
  # (R/quarto-table.R) to raise its own not-yet-supported message, so this
  # NA is what tells the generic mismatch gate there is nothing for it to
  # say about a pptx render.
  expect_identical(apa_quarto_actual_entry("pptx"), NA_character_)
})

test_that("the mismatch message names a real YAML line for slides", {
  msg <- apa_quarto_target_message(called = "revealjs", actual = "docx")
  expect_match(msg, "renders to `apaquarto-docx`", fixed = TRUE)
  expect_match(msg, "format: revealjs", fixed = TRUE)
  # M4 (whole-branch review, 2026-08-06): APA_QUARTO_ENTRY_PROSE[["revealjs"]]
  # is "reveal.js slides" (D91 Ruling 1), not "slides" -- the word now
  # matches the format string on the same line ("set `format: revealjs`").
  # No test asserted the PROSE itself before this: the two checks above pass
  # just as happily if this reverts to "slides", so they could not have
  # caught that regression.
  expect_match(msg, "To produce reveal.js slides", fixed = TRUE)
  # and the reverse direction
  msg2 <- apa_quarto_target_message(called = "docx", actual = "revealjs")
  expect_match(msg2, "renders to `revealjs`", fixed = TRUE)
})

test_that("apa_quarto_target_message() gives PowerPoint its own not-yet-supported sentence, regardless of `called` (D90)", {
  # Phase 1.9 replaces the generic mismatch wording for pptx (which used to
  # read "This document renders to `pptx`. To produce X, set ...") with a
  # dedicated sentence: PowerPoint is not served through Quarto yet, at all,
  # so naming what YAML line would make `called` agree with the render is
  # not the useful fact any more -- what to do instead is.
  msg <- apa_quarto_target_message(called = "pdf", actual = "pptx")
  expect_match(msg, "not yet supported", fixed = TRUE)
  expect_match(msg, "revealjs", fixed = TRUE)
  expect_false(grepl("\\bunsupported\\b", msg))

  # It fires the SAME way when the caller used the slide entry point itself
  # -- PowerPoint is the fact that matters here, not which name was typed.
  msg2 <- apa_quarto_target_message(called = "revealjs", actual = "pptx")
  expect_match(msg2, "not yet supported", fixed = TRUE)

  # The companion case, carried forward from before this phase: an ACTUAL
  # revealjs render must still be named `revealjs` by the ordinary path.
  msg3 <- apa_quarto_target_message(called = "pdf", actual = "revealjs")
  expect_match(msg3, "renders to `revealjs`", fixed = TRUE)
})

test_that("a pptx render is told PowerPoint is not yet supported, and still renders (D90)", {
  # D90: render, THEN warn. Not a refusal -- that breaks a render the user
  # may not have chosen -- and not silence, which was the defect: pptx used
  # to map to the "slide" entry, so the call name and the target AGREED and
  # the existing guidance in apa_quarto_dispatch() never fired.
  #
  # expect_warning(), NOT tryCatch(..., warning = ). tryCatch()'s handler
  # UNWINDS THE CALL STACK at the point the warning fires, so
  # apa_quarto_dispatch()'s subsequent apa_quarto_render() call would never
  # run and its return value would never be inspected -- a version that
  # warned and then returned early, skipping the render entirely, would
  # still satisfy a tryCatch()-based check, leaving the "still renders" half
  # of this test's own name (the entire point of D90) unverified. This is
  # documented as a known trap in test-quarto-entrypoints.R's "fix round 1,
  # finding 3" comment, and this is at least the sixteenth instance of it in
  # this project. expect_warning() captures the condition WITHOUT unwinding
  # the stack, so the call completes normally: `out` holds the real return
  # value, and the condition it returns still carries the message, so both
  # halves of the claim can be checked in one call. Do not "simplify" this
  # back to tryCatch() -- that is exactly the regression this comment exists
  # to block.
  apa_reset_version_warnings()
  on.exit(apa_reset_version_warnings(), add = TRUE)
  spec <- apa.cor.table(attitude[, 1:3])
  local_mocked_bindings(apa_quarto_actual_format = function() "pptx")

  w <- expect_warning(out <- apa_quarto_dispatch(spec, called = "revealjs"),
                      "not yet supported", fixed = TRUE)
  msg <- conditionMessage(w)
  expect_match(msg, "revealjs", fixed = TRUE)
  # never the permanent-sounding word: Phase 1.13 is scheduled (D89)
  expect_false(grepl("\\bunsupported\\b", msg))
  # THE HALF A tryCatch()-based check could not see: the table was actually
  # produced, not skipped after the warning fired.
  expect_match(as.character(out), "+===", fixed = TRUE)
})

test_that("knit_print.apa_table warns on a pptx render too, not only apa_quarto_dispatch() (C1, whole-branch review)", {
  # C1: apa_quarto_render() (R/quarto-table.R) has TWO callers.
  # apa_quarto_dispatch() -- exercised by the test above -- is used by
  # apa.quarto.pdf()/docx()/html()/revealjs() and already warned on pptx.
  # apa_quarto_knit_print() (R/quarto.R) is the SECOND caller: both
  # knit_print.apa_table() and knit_print.apa_table_spec() call it directly
  # for the ordinary case of a bare table left as a chunk's last line, with
  # no function name typed at all, and it used to skip the warning
  # entirely -- MEASURED before this fix: a bare `apa.cor.table(attitude)`
  # as a chunk's last line in a `format: pptx` deck emitted a full table
  # and NO warning, on this documented, exported path. The fix moved the
  # pptx check into apa_quarto_render() itself so both routes see it; this
  # test drives the SECOND route, which no earlier test in this file did.
  #
  # expect_warning(), not tryCatch() -- see the comment on the test above
  # for why tryCatch()'s stack-unwinding would hide whether the table was
  # still produced.
  apa_reset_version_warnings()
  on.exit(apa_reset_version_warnings(), add = TRUE)
  x <- apa.cor.table(attitude[, 1:3])
  local_mocked_bindings(apa_quarto_actual_format = function() "pptx")

  w <- expect_warning(out <- knitr::knit_print(x), "not yet supported",
                      fixed = TRUE)
  msg <- conditionMessage(w)
  expect_match(msg, "revealjs", fixed = TRUE)
  expect_false(grepl("\\bunsupported\\b", msg))
  # THE HALF a mocked-dispatch-only fix could still miss: the table was
  # actually produced by the knit_print route, not skipped.
  expect_match(as.character(out), "+===", fixed = TRUE)
})

test_that("a pptx render carries the not-yet-supported sentence in the BODY, once (docs audit, 2026-08-09)", {
  # The warning() the two tests above assert is invisible on the one render
  # it is about: Quarto's pptx format suppresses R warnings by default
  # (measured on a real `quarto render`: a plain warning() in a chunk under
  # `format: pptx` reaches neither the deck nor the render console, while
  # the same chunk under HTML shows it; `execute: warning: true` restores
  # it). The README's promise -- "so that you do not hand out a deck
  # without knowing" -- therefore needs the sentence IN the slide body, and
  # apa_quarto_render() now prepends it to the first table's block. Only
  # the first: later tables in the same document ride the same
  # apa_warn_once() dedupe the warning uses, so a five-table deck is not
  # branded five times.
  apa_reset_version_warnings()
  on.exit(apa_reset_version_warnings(), add = TRUE)
  spec <- apa.cor.table(attitude[, 1:3])
  local_mocked_bindings(apa_quarto_actual_format = function() "pptx")

  w <- expect_warning(out <- apa_quarto_dispatch(spec, called = "revealjs"),
                      "not yet supported", fixed = TRUE)
  body <- as.character(out)
  expect_match(body, "^Warning: ")
  expect_match(body, "not yet supported", fixed = TRUE)
  expect_match(body, "revealjs", fixed = TRUE)
  expect_match(body, "+===", fixed = TRUE)   # notice rides WITH the table

  # Second table, same document: no second notice (and no second warning --
  # expect_no_warning would also pass vacuously if the function errored, so
  # capture the value and check the block is a bare table).
  out2 <- expect_no_warning(apa_quarto_dispatch(spec, called = "revealjs"))
  body2 <- as.character(out2)
  expect_false(grepl("^Warning: ", body2))
  expect_match(body2, "+===", fixed = TRUE)
})

test_that("apa_quarto_dispatch() no longer special-cases pptx itself (C1 refactor guard)", {
  # C1 moved the pptx check out of apa_quarto_dispatch() and into
  # apa_quarto_render(). This does not change apa_quarto_dispatch()'s own
  # OBSERVABLE behaviour (the test above and the two before it still see
  # the warning, because apa_quarto_dispatch() calls apa_quarto_render()),
  # but it does mean apa_quarto_dispatch() must still warn on a pptx render
  # even though its own `identical(actual, "pptx")` clause is gone -- this
  # pins that the warning survives the refactor via the shared call, not
  # via a second copy of the check.
  apa_reset_version_warnings()
  on.exit(apa_reset_version_warnings(), add = TRUE)
  spec <- apa.cor.table(attitude[, 1:3])
  local_mocked_bindings(apa_quarto_actual_format = function() "pptx")

  expect_warning(apa_quarto_dispatch(spec, called = "pdf"), "not yet supported",
                fixed = TRUE)
})

test_that("the default emits neither title nor note", {
  f <- apa_slide_furniture(apa.cor.table(attitude[, 1:3]))
  expect_identical(f$title, "")
  expect_identical(f$note, "")
})

test_that("TRUE emits the generated title and note; FALSE and NULL emit nothing", {
  x <- apa.cor.table(attitude[, 1:3])
  f <- apa_slide_furniture(x, title = TRUE, note = TRUE)
  expect_true(nzchar(f$title))
  expect_true(nzchar(f$note))
  # The generated title of a correlation table names what it is.
  expect_match(f$title, "Descriptive Statistics and Correlations", fixed = TRUE)
  # The generated note is what DEFINES the significance stars the cells use,
  # and it arrives in two parts (sp$notes has length 2) -- the star sentence
  # is the SECOND. Asserting on it specifically is what proves both parts
  # survive the join; a bare expect_match(f$note, "*") would pass on the
  # first part alone, which is the silent loss D27 accepts note = TRUE as
  # the remedy for.
  expect_match(f$note, "standard deviation", fixed = TRUE)   # part 1
  expect_match(f$note, "indicates", fixed = TRUE)            # part 2

  for (v in list(FALSE, NULL)) {
    g <- apa_slide_furniture(x, title = v, note = v)
    expect_identical(g$title, "")
    expect_identical(g$note, "")
  }
})

test_that("apa_slide_resolve refuses a logical that is not a single TRUE or FALSE", {
  # The guard at R/quarto-slide.R:54 has had no observer. Without one, a
  # future edit that dropped the length check would let title = c(TRUE, FALSE)
  # through to isTRUE(), which answers FALSE for a length-2 vector -- so the
  # user's requested title would silently vanish rather than erroring.
  resolve <- apaTables:::apa_slide_resolve

  expect_error(resolve(c(TRUE, FALSE), "gen"), "TRUE, FALSE, a string, or NULL")
  expect_error(resolve(NA, "gen"),             "TRUE, FALSE, a string, or NULL")
  expect_error(resolve(logical(0), "gen"),     "TRUE, FALSE, a string, or NULL")

  # The accepted cases, asserted in the same test so that a "fix" which
  # tightened the guard into rejecting everything could not pass.
  expect_identical(resolve(TRUE, "gen"), "gen")
  expect_identical(resolve(FALSE, "gen"), "")
  expect_identical(resolve(NULL, "gen"), "")
})

test_that("a string overrides, and markdown in it is passed through untouched", {
  x <- apa.cor.table(attitude[, 1:3])
  f <- apa_slide_furniture(x, title = "Study *N* by Group", note = "Mine.")
  expect_identical(f$title, "Study *N* by Group")
  expect_identical(f$note, "Mine.")
})

test_that("a generated note is ONE markdown paragraph, not a list", {
  # apaquarto's utilsapa.lua splits apa-note on a literal '","' -- that
  # mechanism does not exist on a slide, so a multi-part note must arrive
  # here already joined into flowing markdown, never as a character vector
  # (which would emit as separate lines and silently become separate
  # paragraphs, dropping the significance-star sentence onto its own slide
  # line).
  f <- apa_slide_furniture(apa.cor.table(attitude[, 1:3]), note = TRUE)
  expect_length(f$note, 1L)
  expect_false(grepl("\n", f$note, fixed = TRUE))
})

# --- Task 3: the fitting model (D47) ----------------------------------------

# The author's frame of reference (D47): the two tables a conference talk
# actually uses. apa.reg.table()'s own help page uses these same models.
reg_plans <- function() {
  blk1  <- lm(sales ~ adverts + airplay, data = album)
  blk1b <- lm(sales ~ adverts, data = album)
  blk2  <- lm(sales ~ adverts + airplay + attract, data = album)
  list(single = apa_display_plan(as_spec(apa.reg.table(blk1))),
       blocks = apa_display_plan(as_spec(apa.reg.table(blk1b, blk2))))
}

test_that("rows and extra lines are counted SEPARATELY", {
  # They cost different amounts of height (a row also costs cell padding),
  # which is the whole reason for two constants. A regression table's cells
  # are all one line; a correlation table's are mostly two.
  p <- reg_plans()
  expect_identical(apa_slide_row_lines(p$single), c(rows = 7L, extra = 0L))
  expect_identical(apa_slide_row_lines(p$blocks), c(rows = 15L, extra = 0L))

  cor7 <- apa_display_plan(as_spec(apa.cor.table(attitude)))
  expect_identical(apa_slide_row_lines(cor7), c(rows = 8L, extra = 12L))
})

test_that("the blocks regression table is WIDER by one column", {
  # The author flagged this specifically, and it is what makes the blocks
  # table width-bound at a smaller size than the single-block one.
  p <- reg_plans()
  expect_identical(p$single$n_col, 9L)
  expect_identical(p$blocks$n_col, 10L)
  expect_gt(apa_slide_char_width(p$blocks), apa_slide_char_width(p$single))
})

test_that("BOTH regression tables are WIDTH-bound, at the measured sizes", {
  # THE REGRESSION TEST FOR D47's DEFECT. The height constraint alone
  # returns 48.7px for the single-block table, whose wrap cap is 23.2px --
  # a slide overflowing by more than double, silently. If someone deletes
  # the wrap cap, these two numbers are what catches it.
  # Tolerance tightened to 0.001 (fix round 1): the model reproduces these
  # to four decimal places, so the original 0.02 was far looser than the
  # arithmetic needs -- loose enough that it let a genuine defect (the
  # APA_SLIDE_ROW_FACTOR / APA_SLIDE_LINE_FACTOR transposition caught by
  # mutation testing, see below) slip through undetected on a neighbouring
  # test by coincidence.
  p <- reg_plans()
  expect_equal(apa_slide_font_pt(p$single), 17.41, tolerance = 0.001)
  expect_equal(apa_slide_font_pt(p$blocks), 14.60, tolerance = 0.001)
})

test_that("a tall narrow table is HEIGHT-bound -- both constraints can bind", {
  # A check that the min() is real rather than the width cap always winning:
  # for a 7-variable correlation matrix the HEIGHT constraint is the smaller
  # one (18.1px against a 21.6px cap), so this size comes from the other
  # branch entirely.
  # Tolerance tightened to 0.001 (fix round 1) -- see the comment on the
  # regression-table test above.
  cor7 <- apa_display_plan(as_spec(apa.cor.table(attitude)))
  expect_equal(apa_slide_font_pt(cor7), 13.56, tolerance = 0.001)
})

test_that("the row and line factors are not interchangeable", {
  # Fix round 1, from mutation testing: swapping APA_SLIDE_ROW_FACTOR (1.60)
  # and APA_SLIDE_LINE_FACTOR (1.45) is NOT caught by any test above. Every
  # other reference table is either width-bound (so k never reaches the
  # returned value) or has rows == extra, which makes k swap-invariant --
  # cor3 (attitude[, 1:3]) is rows = 4, extra = 4 and so cannot possibly
  # detect it, and cor7's rows = 8/extra = 12 gives a swap error just under
  # the old tolerance = 0.02 by coincidence, not by design.
  #
  # apa.cor.table(mtcars) has a STARK rows-vs-extra asymmetry (12 vs 20),
  # which is what makes the two constants distinguishable at all: under the
  # correct constants this table computes 8.4959pt; under the swap it comes
  # to 8.2895pt, a 2.429% error, comfortably outside tolerance = 0.001. Base
  # R data, so this is deterministic and always available -- no set.seed()
  # dependency on rendered character counts.
  mt <- apa_display_plan(as_spec(apa.cor.table(mtcars)))
  expect_identical(apa_slide_row_lines(mt), c(rows = 12L, extra = 20L))
  expect_identical(apa_slide_char_width(mt), 140L)
  expect_equal(apa_slide_font_pt(mt), 8.4959, tolerance = 0.001)
})

test_that("the per-column padding term is real: a FEW-column table is not oversized", {
  # THE DEFECT THAT FORCED A SECOND WIDTH TERM. The 2-way spanner table has
  # only 66 characters across 9 columns, so it is padding-dominated: a
  # single character coefficient fitted on the regression tables predicts a
  # 54.5px limit against a MEASURED 34px, i.e. it would set this table half
  # again too large and run it off the side of the slide.
  options(contrasts = c("contr.sum", "contr.poly"))
  p <- apa_display_plan(as_spec(apa.2way.table(
    iv1 = gender, iv2 = alcohol, dv = attractiveness,
    data = goggles, show.marginal.means = TRUE)))

  # Tolerance tightened to 0.001 (fix round 1) -- see the comment on the
  # regression-table test above; this table reproduces just as tightly
  # (computed 25.4289 against the pinned 25.43).
  expect_equal(apa_slide_font_pt(p), 25.43, tolerance = 0.001)
  # ... and specifically NOT the 41pt a character-only model allows, which
  # would run this table off the side of the slide.
  expect_lt(apa_slide_font_pt(p), 30)
})

test_that("the width input is RENDERED text, not the grid's source width", {
  # D50. apa_md_widths() answers how wide the grid SOURCE cell must be and
  # scales every column by max(markup/rendered) to guarantee it -- a factor
  # that varies per table. The ANOVA table is the extreme case: 142 source
  # characters for 42 drawn ones, because `*eta*^2^~p~` is 9 characters of
  # markup for 3 of text. Using the source width over-predicted that
  # table's width by 1.57x and set it at 30px where 47px fitted.
  aov <- apa_display_plan(as_spec(apa.aov.table(
    lm(libido ~ dose, data = viagra))))
  expect_identical(apa_slide_char_width(aov), 42L)
  expect_gt(sum(apa_md_widths(aov, pad = 2L, font_size = NULL,
                              rules = FALSE)), 100L)

  # And the markup stripping is what does it: no rendered line may contain
  # a fence, an emphasis marker or a superscript caret.
  spanned <- apa_display_plan(as_spec(apa.2way.table(
    iv1 = gender, iv2 = alcohol, dv = attractiveness,
    data = goggles, show.marginal.means = TRUE)))
  expect_identical(apa_slide_char_width(spanned), 48L)
})

test_that("the wrap cap is not blind to spanner labels (I3, whole-branch review)", {
  # Before this fix, apa_slide_char_width() read only
  # `plan$header[[length(plan$header)]]` (the last header row) plus data
  # rows -- every spanner label, which sits in a NON-final header row, was
  # invisible to it entirely. MEASURED: two structurally identical two-way
  # tables differing ONLY in the length of their group labels ("A"/"B"/"C"
  # versus these) both returned char_width 48 and font_pt 25.43 before this
  # fix, despite one having ~45 more characters to draw.
  options(contrasts = c("contr.sum", "contr.poly"))
  short <- goggles
  levels(short$alcohol) <- c("A", "B", "C")
  long <- goggles
  levels(long$alcohol) <- c("No alcohol whatsoever",
                            "Two pints of strong lager",
                            "Four pints of lager")
  p_short <- apa_display_plan(as_spec(apa.2way.table(
    iv1 = gender, iv2 = alcohol, dv = attractiveness,
    data = short, show.marginal.means = TRUE)))
  p_long <- apa_display_plan(as_spec(apa.2way.table(
    iv1 = gender, iv2 = alcohol, dv = attractiveness,
    data = long, show.marginal.means = TRUE)))

  # char_width: 48 vs 83, computed live -- NOT the same value the old code
  # returned for both.
  expect_gt(apa_slide_char_width(p_long), apa_slide_char_width(p_short))
  # And the wrap cap is what apa_slide_font_pt() reads it through, so the
  # long-label table must come out with a STRICTLY smaller automatic size:
  # 17.71 vs 25.43, computed live.
  expect_lt(apa_slide_font_pt(p_long), apa_slide_font_pt(p_short))

  # The pinned regression sizes for tables WITHOUT spanners must not have
  # moved -- if this fix were wrong it would change every table's width, not
  # only the spanner ones.
  p <- reg_plans()
  expect_equal(apa_slide_font_pt(p$single), 17.41, tolerance = 0.001)
  expect_equal(apa_slide_font_pt(p$blocks), 14.60, tolerance = 0.001)
})

test_that("an explicit font.size is honoured EXACTLY and bypasses both constraints", {
  # D43/D44: font.size is appearance, at the exact size the destination
  # allows. CSS is exact, so a fractional size survives intact -- Word's
  # five-rung snapping must NOT be imitated here. 30pt is far above every
  # cap above, which is the point: an explicit request is not second-guessed.
  p <- reg_plans()$blocks
  expect_identical(apa_slide_font_pt(p, font.size = 8.5), 8.5)
  expect_identical(apa_slide_font_pt(p, font.size = 30L), 30)
})

test_that("the automatic size shrinks as the table grows", {
  # A distinguishing check, not a tautology: the bigger table must get a
  # STRICTLY smaller size.
  p <- reg_plans()
  expect_gt(apa_slide_font_pt(p$single), apa_slide_font_pt(p$blocks))
})

test_that("the automatic size never exceeds the deck's own body text", {
  # A 4-row table has room to spare on both constraints; nothing on a slide
  # should be set larger than the prose around it.
  tiny <- apa_display_plan(as_spec(apa.cor.table(attitude[, 1:3])))
  expect_identical(apa_slide_font_pt(tiny), APA_SLIDE_BODY_PT)
})

# --- Task 4: the size wrapper and the APA furniture CSS (D48) --------------

test_that("the wrapper opens and closes, with blank lines around the body", {
  out <- apa_slide_size_wrap("TABLE-BODY", 9.6)
  lines <- strsplit(out, "\n", fixed = TRUE)[[1]]

  # The class is part of the opener, not an add-on: without it the CSS
  # below (scoped to .apa-slide-table) matches nothing (D48). The brief's
  # draft test hardcoded the opener without the class; corrected here to
  # match both the reference prototype (dev/slides-prototype/build.R
  # size_wrap()) and the "carries the class" test a few lines below, which
  # would otherwise contradict this one.
  expect_identical(lines[1],
                    '::: {.apa-slide-table style="font-size: 9.6pt;"}')
  expect_identical(lines[2], "")
  expect_true("TABLE-BODY" %in% lines)

  # Exactly one opener and exactly one closer, and the closer is LAST.
  # Pattern includes the class for the same reason as above.
  expect_identical(
    apa_md_count_matches("^::: \\{\\.apa-slide-table style=", lines), 1L)
  closers <- which(lines == ":::")
  expect_length(closers, 1L)
  expect_identical(closers, max(which(nzchar(lines))))
})

test_that("the body is separated from the closing fence by a blank line", {
  lines <- strsplit(apa_slide_size_wrap("BODY", 10), "\n", fixed = TRUE)[[1]]
  close_at <- which(lines == ":::")
  expect_identical(lines[close_at - 1L], "")
})

test_that("an exact fractional size survives into the CSS", {
  # D44: exact size where the destination allows. 8.5pt must not become 8.
  expect_match(apa_slide_size_wrap("B", 8.5), "font-size: 8.5pt;", fixed = TRUE)
  # and a whole number must not gain a spurious decimal
  expect_match(apa_slide_size_wrap("B", 12), "font-size: 12pt;", fixed = TRUE)
  # ... and a continuous ladder size is ROUNDED, not written at full
  # precision. The blocks regression table's real size is
  # 1008/(0.370*104 + 1.33*10)*0.75 = 14.600258..., and format()'s default
  # 7 significant digits would put `font-size: 14.60026pt` in the deck.
  expect_match(apa_slide_size_wrap("B", 14.600258), "font-size: 14.6pt;",
               fixed = TRUE)
})

test_that("the wrapper carries the class the CSS is scoped to", {
  # Without this class the style block below matches nothing -- the two are
  # one mechanism (D48).
  expect_match(apa_slide_size_wrap("B", 12), ".apa-slide-table", fixed = TRUE)
})

test_that("the CSS supplies APA's three rules and suppresses the rest", {
  css <- apa_slide_css()

  # Delivered as a RAW HTML block: measured, this reaches revealjs and is
  # silently DROPPED by the pptx writer, so no format branch is needed and a
  # <style> can never print as text on a PowerPoint slide (D48).
  expect_match(css, "```{=html}", fixed = TRUE)
  expect_match(css, "<style>", fixed = TRUE)
  expect_match(css, "</style>", fixed = TRUE)

  # Every selector is scoped to our own class -- nothing may restyle a table
  # the user wrote by hand. Extracted from inside <style>...</style> only:
  # over the whole string the same regex also catches the ```{=html} fence
  # opener as a spurious "selector" (it too is `text{`), which would fail
  # this check even on a correct implementation.
  style_body <- sub(".*<style>", "", css)
  style_body <- sub("</style>.*", "", style_body)
  sels <- regmatches(style_body,
                      gregexpr("[^{}\n]+(?=\\s*\\{)", style_body, perl = TRUE))[[1]]
  sels <- trimws(sels[nzchar(trimws(sels))])
  expect_true(all(grepl("apa-slide-table", sels, fixed = TRUE)),
              info = paste(sels[!grepl("apa-slide-table", sels, fixed = TRUE)],
                           collapse = " | "))

  # The three rules APA wants, and the suppression of the ones reveal adds.
  expect_match(css, "border-top: 1px solid", fixed = TRUE)
  expect_match(css, "border-bottom: 1px solid", fixed = TRUE)
  expect_match(css, "border-bottom: none", fixed = TRUE)

  # The author's two observations (D48).
  expect_match(css, "vertical-align: bottom", fixed = TRUE)
  expect_match(css, "tabular-nums", fixed = TRUE)

  # The spanner cmidrule. Measured on a deck: these divs ARE emitted and
  # draw NOTHING (computed border 0px none) until this selector exists --
  # exactly the state D37 found on HTML before the same fix. This is why
  # rules = TRUE is kept on the slide path.
  expect_match(css, "[data-custom-style=\"ApaTableRule\"]", fixed = TRUE)

  # D50: the col override, not just the table one. Pandoc turns a grid
  # table's column widths into a <colgroup> of PERCENTAGES that force the
  # table to fill its container (measured: the ANOVA table's natural 784
  # slide units drawn at 1229). `width: auto` on the TABLE alone does
  # nothing -- measured, `auto` and `fit-content` give an identical layout
  # -- so the override must target the col elements themselves. Without this
  # line the CSS test above still passes in full (nothing else asserts it),
  # which is exactly why it is pinned separately here.
  expect_match(css, "col { width: auto", fixed = TRUE)
})

# Phase 1.9 (D88/D90) removed apa_slide_flatten_spanners() along with the
# rest of the pptx machinery -- PowerPoint left the Quarto path entirely, so
# there is no destination left for these three tests to exercise. This is
# coverage of a REMOVED feature, not coverage lost: the logic itself is
# recorded verbatim in dev/plans/2026-07-31-roadmap.md under Phase 1.13,
# which will need it again (and its own tests) whoever writes a PowerPoint
# file next. The three tests that stood here were "flattening removes every
# span but keeps every label", "a flattened label sits over the MIDDLE of
# its group, not the first column", and "a plan with no spanners is
# returned unchanged".

# --- Task 6: apa.quarto.revealjs() -- assemble the block -------------------

test_that("a slide block is title, wrapped table, note -- and NO meta div", {
  x <- apa.cor.table(attitude[, 1:3])
  out <- as.character(apa_quarto_revealjs_block(x, title = TRUE, note = TRUE))

  expect_match(out, "font-size: ", fixed = TRUE)
  # The meta div belongs to apaquarto, which a deck does not load.
  expect_false(grepl("apa-table-meta", out, fixed = TRUE))
  # Nor may any of the docx-only furniture leak onto a slide.
  expect_false(grepl("apa-table-size-", out, fixed = TRUE))
  expect_false(grepl("apa-table-landscape", out, fixed = TRUE))

  # Order: title before the fence, note after the closing fence.
  #
  # BOTH anchors are chosen to appear NOWHERE ELSE in the output, and that
  # is the whole difficulty here. The obvious ones do not work: "*" is in
  # every cell of a correlation table (".83**") and in its column labels
  # ("*M*", "*SD*"), so regexpr("*") finds the TABLE, not the note, and the
  # assertion passes or fails for the wrong reason. The generated title is
  # "Descriptive Statistics and Correlations" and the note's first part ends
  # "...the 95% confidence interval." -- neither string occurs in the body.
  fence_open  <- regexpr("::: \\{\\.apa-slide-table style=", out)
  fence_close <- tail(gregexpr(":::", out, fixed = TRUE)[[1]], 1L)
  title_at    <- regexpr("Descriptive Statistics and Correlations", out,
                         fixed = TRUE)
  note_at     <- regexpr("confidence interval", out, fixed = TRUE)
  expect_gt(title_at, 0L)
  expect_lt(title_at, fence_open)
  expect_gt(note_at, fence_close)
})

test_that("the default slide carries neither title nor note", {
  out <- as.character(apa_quarto_revealjs_block(apa.cor.table(attitude[, 1:3])))
  expect_false(grepl("Descriptive Statistics and Correlations", out,
                     fixed = TRUE))
  expect_false(grepl("confidence interval", out, fixed = TRUE))
  # but it does carry the table
  expect_match(out, "+---", fixed = TRUE)
})

test_that("the assembled block contains a properly closed raw-HTML block", {
  # Carried forward from Task 4's review: the existing tests pinned only the
  # OPENING ```{=html} fence, never the closing ```. A bare unfenced <style>
  # is invalid pandoc and would print as visible text on a slide, so the
  # close must be asserted too.
  out <- as.character(apa_quarto_revealjs_block(apa.cor.table(attitude[, 1:3])))
  open_at  <- regexpr("```{=html}", out, fixed = TRUE)
  expect_gt(open_at, 0L)
  after_open <- substring(out, open_at + nchar("```{=html}"))
  close_at <- regexpr("\n```", after_open, fixed = TRUE)
  expect_gt(close_at, 0L)
})

# Phase 1.9 (D88/D90) removed two more tests here: "D54 regression guard:
# pptx gets no wrapper and no CSS block; revealjs gets both" and "spanners
# are flattened for pptx and KEPT for revealjs". Both mocked
# apa_quarto_revealjs_target() to "pptx" to force apa_quarto_revealjs_block()'s
# is_pptx branch -- a branch that no longer exists, because
# apa_quarto_revealjs_target() itself can no longer answer "pptx" at all
# (APA_REVEALJS_FORMATS is revealjs-only now). This is coverage of a REMOVED
# code path, not coverage lost: nothing in apa_quarto_revealjs_block()
# branches on the destination any more, so there is nothing left for a
# "pptx vs revealjs" comparison to discriminate.

test_that("apa.quarto.revealjs() has the four-argument signature apa_quarto_dispatch() expects", {
  # Renamed from "... is a thin wrapper over apa_quarto_dispatch()" (I5,
  # whole-branch review, 2026-08-04): this only ever checked the FORMALS, not
  # that the body actually calls apa_quarto_dispatch(). Measured: replacing
  # the whole body of apa.quarto.revealjs() with a direct apa_quarto_render()
  # call -- dropping the guidance message entirely -- left the suite at
  # FAIL 0. The test below this one closes that gap; this one keeps the
  # narrower, still-real fact its old name overclaimed.
  expect_true(is.function(apa.quarto.revealjs))
  expect_identical(names(formals(apa.quarto.revealjs)),
                   c("x", "title", "note", "font.size"))
})

test_that("apa.quarto.revealjs() really is a thin wrapper over apa_quarto_dispatch(): a name/target mismatch warns, names the fix, and the table still renders (I5)", {
  # Modelled on test-quarto-entrypoints.R's identical proof for
  # apa.quarto.pdf() (":54-75" there): mock the render target away from what
  # was called, so apa_quarto_dispatch() -- not apa_quarto_render() called
  # directly -- is the only path that can produce the guidance warning. If
  # apa.quarto.revealjs()'s body were replaced with a direct
  # apa_quarto_render(x, ...) call, `called` would never reach
  # apa_quarto_target_message() and this expect_warning() would fail to see
  # any warning at all.
  apa_reset_version_warnings()
  on.exit(apa_reset_version_warnings(), add = TRUE)
  local_mocked_bindings(apa_quarto_actual_format = function() "docx")
  expect_warning(
    out <- apa.quarto.revealjs(apa.cor.table(attitude[, 1:3])),
    "format: revealjs", fixed = TRUE)
  # And the "forgiving, never refuses" half of D30: the table itself is
  # still produced, on the markdown path this mismatch actually renders.
  expect_match(as.character(out), "+===", fixed = TRUE)
})

test_that("apa_quarto_render() routes a slide target to apa_quarto_revealjs_block(), not the markdown block", {
  local_mocked_bindings(apa_quarto_actual_format = function() "revealjs")
  out <- as.character(apa_quarto_render(apa.cor.table(attitude[, 1:3])))
  expect_match(out, "font-size: ", fixed = TRUE)
  expect_false(grepl("apa-table-meta", out, fixed = TRUE))
})

# --- Task 8: apa.quarto.template(target = "revealjs") ----------------------

test_that("apa.quarto.template(target = 'revealjs') writes a slide chunk", {
  out <- capture.output(apa.quarto.template(apa.cor.table(attitude),
                                            target = "revealjs"))
  txt <- paste(out, collapse = "\n")
  expect_match(txt, "apa.quarto.revealjs(", fixed = TRUE)
  # A deck has no float, so a tbl- label buys nothing and tbl-cap is not
  # read by anything -- neither belongs in a slide chunk.
  expect_false(grepl("tbl-cap", txt, fixed = TRUE))
  expect_false(grepl("label: tbl-", txt, fixed = TRUE))
})
