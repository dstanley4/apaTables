# apa_quarto_size_rung()'s contract. Every size names a style, the 10 pt
# default included: the reference document's plain "Table" style is stock
# apaquarto's (12 pt, wide margins), kept for the user's own markdown tables.

test_that("apa_quarto_size_rung maps every band of the 6-12 range", {
  rung <- apaTables:::apa_quarto_size_rung

  # below the readable floor: clamped up to the smallest style that exists
  expect_identical(rung(2), 6L)
  expect_identical(rung(5.9), 6L)

  # the ordinary band, floored not rounded
  expect_identical(rung(6), 6L)
  expect_identical(rung(8), 8L)
  expect_identical(rung(8.5), 8L)
  expect_identical(rung(9.9), 9L)

  # EXACTLY 10 and just above it: Table10, named like every other size. An
  # apaTables table with no marker would get the plain "Table" style.
  expect_identical(rung(10), 10L)
  expect_identical(rung(10.5), 10L)

  # the new upper band
  expect_identical(rung(11), 11L)
  expect_identical(rung(12), 12L)
  expect_identical(rung(11.5), 11L)

  # ABOVE the ceiling: the largest style there is.
  expect_identical(rung(13), 12L)
  expect_identical(rung(16), 12L)
  expect_identical(rung(72), 12L)
})

test_that("apa_quarto_size_rung never names a style the reference document lacks", {
  rung <- apaTables:::apa_quarto_size_rung
  produced <- vapply(seq(1, 40, by = 0.25), function(s) rung(s), integer(1))
  expect_false(anyNA(produced))
  expect_setequal(unique(produced), 6:12)
})

test_that("Word reports every size it could not deliver, and stays silent when it could", {
  warn_text <- function(requested) {
    w <- tryCatch(apaTables:::apa_quarto_warn_word_size(requested,
                    apaTables:::apa_quarto_delivered_size(requested)),
                  warning = function(e) conditionMessage(e))
    if (is.character(w)) w else NA_character_
  }

  # delivered == requested: nothing to report
  expect_true(is.na(warn_text(8)))
  expect_true(is.na(warn_text(12)))
  expect_true(is.na(warn_text(10)))

  # adjusted: the message must NAME the size actually used
  expect_match(warn_text(16), "12", fixed = TRUE)
  expect_match(warn_text(4), "6", fixed = TRUE)
  expect_match(warn_text(8.5), "8", fixed = TRUE)

  # one flowing sentence -- knitr truncates a multi-line warning off the page
  expect_false(grepl("\n", warn_text(16), fixed = TRUE))
})

test_that("apa_quarto_delivered_size matches what apa_quarto_size_rung implies", {
  # Direct check that the "what Word actually shows" helper agrees with the
  # rung: the rung is the style, and the style is what is delivered.
  delivered <- apaTables:::apa_quarto_delivered_size
  expect_identical(delivered(10), 10L)
  expect_identical(delivered(10.5), 10L)
  expect_identical(delivered(8), 8L)
  expect_identical(delivered(16), 12L)
})

test_that("the docx markdown block warns on an explicit out-of-range size and stays silent in range", {
  # This is the call-site wiring in apa_quarto_markdown_block(): an explicit
  # font.size outside 6-12 must surface apa_quarto_warn_word_size() when
  # rendering to docx, and must not when the requested size is deliverable.
  local_mocked_bindings(apa_quarto_docx_target = function() TRUE)

  expect_warning(
    apa_quarto_markdown_block(apa.cor.table(attitude), font.size = 16),
    "12"
  )
  expect_no_warning(
    apa_quarto_markdown_block(apa.cor.table(attitude), font.size = 8)
  )
})

test_that("every Word table carries a size marker, the 10 pt default included", {
  # The plain "Table" style is stock apaquarto's, for the user's own markdown
  # tables; an apaTables table that reached Word with no marker would take it
  # (12 pt, 58-twip margins) instead of apaTables' own Table10.
  local_mocked_bindings(apa_quarto_docx_target = function() TRUE)
  narrow <- as.character(apa_quarto_markdown_block(apa.cor.table(attitude[, 1:3])))
  expect_match(narrow, "::: {.apa-table-size-10}", fixed = TRUE)
  sized <- as.character(apa_quarto_markdown_block(apa.cor.table(attitude[, 1:3]),
                                                  font.size = 8))
  expect_match(sized, "::: {.apa-table-size-8}", fixed = TRUE)
  expect_false(grepl("apa-table-size-10", sized, fixed = TRUE))
})

test_that("the HTML marker is emitted with or without a size", {
  wrap <- apaTables:::apa_quarto_html_wrap

  # UNSIZED: the marker still appears, carrying only the class. The HTML width
  # rules in apa.css are scoped to the wrapper div this marker produces, so it
  # must be present on EVERY apaTables table in an HTML render -- not only a
  # sized one.
  expect_match(wrap("TBL"), "::: {.apa-table-html}", fixed = TRUE)
  expect_false(grepl("apa-table-size-pt", wrap("TBL"), fixed = TRUE))

  # SIZED: class AND attribute, on ONE div. Two sibling markers would leave the
  # second unpaired -- apatablesize.lua pairs a marker with the table
  # immediately before it and clears the pairing once consumed.
  expect_match(wrap("TBL", 8),   'apa-table-size-pt="8"',   fixed = TRUE)
  expect_match(wrap("TBL", 8.5), 'apa-table-size-pt="8.5"', fixed = TRUE)
  expect_match(wrap("TBL", 16),  'apa-table-size-pt="16"',  fixed = TRUE)
  expect_match(wrap("TBL", 8), ".apa-table-html apa-table-size-pt", fixed = TRUE)

  # ONE fence open and one close, in both forms: counting them is what would
  # catch a second marker being appended rather than the attribute being folded
  # into the first
  expect_equal(lengths(gregexpr(":::", wrap("TBL", 8), fixed = TRUE)), 2L)
  expect_equal(lengths(gregexpr(":::", wrap("TBL"),    fixed = TRUE)), 2L)

  # the marker sits AFTER the table's own markdown and nothing comes between
  # them -- apatablesize.lua pairs a marker with the table immediately before
  # it in the SAME block list, so anything inserted between breaks the pairing
  expect_match(wrap("TBL", 8), "^TBL\n\n:::", fixed = FALSE)
  expect_match(wrap("TBL"),    "^TBL\n\n:::", fixed = FALSE)

  # NOT the class form for the size: the class pattern is integers-only and
  # would silently drop the fraction
  expect_false(grepl("apa-table-size-8\\.5\\b", wrap("TBL", 8.5)))
})

test_that("an explicit font.size on a non-docx target emits the exact-size marker", {
  # apa_quarto_html_wrap() is tested in isolation above; this exercises the
  # JOIN between it and the call site in apa_quarto_markdown_block() -- the
  # emit-site branch that decides WHETHER to call it. This is the positive
  # half of that wiring: a non-docx target
  # (mocked, since there is no live Quarto render in a unit test) plus an
  # explicit font.size must produce the attribute marker in the body.
  local_mocked_bindings(apa_quarto_docx_target = function() FALSE)
  b <- apa_quarto_markdown_block(apa.cor.table(attitude[, 1:3]), font.size = 8.5)
  body <- as.character(b)
  expect_true(grepl('apa-table-size-pt="8.5"', body, fixed = TRUE))
})

test_that("a non-docx target with no explicit font.size carries no exact-size marker", {
  # The negative half of the same wiring: the automatic ladder must never reach
  # HTML (nothing shrinks automatically on a web page).
  # test-render-markdown.R's rotation and no-rotation tests already cover
  # this shape for the class marker's substring; this is the same assertion,
  # restated here beside its
  # positive counterpart so the pairing is visible in one place.
  local_mocked_bindings(apa_quarto_docx_target = function() FALSE)
  b <- apa_quarto_markdown_block(apa.cor.table(attitude[, 1:3]))
  body <- as.character(b)
  expect_false(grepl("apa-table-size-pt=", body, fixed = TRUE))

  # But the marker DIV itself is there regardless, carrying only the class.
  # Asserting it is what keeps "no exact-size marker" distinct from "no marker
  # at all".
  expect_true(grepl("::: {.apa-table-html}", body, fixed = TRUE))
})

test_that("a non-HTML, non-docx target carries no marker at all", {
  # THE GATE. apatablesize.lua is registered for html and docx only, so
  # nothing consumes this marker on a reveal.js deck or a typst render -- and
  # an unconsumed empty div is not inert everywhere: it would print as an
  # empty block in the deck. The marker is emitted for unsized tables too, so
  # EVERY table reaches this path, which is what makes the gate worth its own
  # test.
  local_mocked_bindings(apa_quarto_docx_target = function() FALSE)
  local_mocked_bindings(apa_quarto_html_target = function() FALSE)
  b <- apa_quarto_markdown_block(apa.cor.table(attitude[, 1:3]))
  body <- as.character(b)
  expect_false(grepl("apa-table-html", body, fixed = TRUE))
  expect_false(grepl("apa-table-size-pt", body, fixed = TRUE))

  # and the same with an explicit size, which is the other way in
  b2 <- apa_quarto_markdown_block(apa.cor.table(attitude[, 1:3]), font.size = 8)
  expect_false(grepl("apa-table-html", as.character(b2), fixed = TRUE))
})

test_that("Word's size-adjustment warning does not fire on a non-docx target", {
  # apa_quarto_warn_word_size() is gated on is_docx at the call site
  # (`if (is_docx && !is.null(font.size))`). This drives an explicit,
  # out-of-range font.size at a MOCKED non-docx target to prove that gate
  # actually excludes it -- the other tests either mock is_docx TRUE or leave
  # font.size NULL. font.size = 16 is chosen because it
  # is exactly the value that DOES trigger the warning on a docx target
  # (see the first test in this file), so a false-positive gate would be
  # caught here, not hidden by an in-range value that stays silent for an
  # unrelated reason.
  #
  # expect_silent(), not expect_no_warning(): this exact call, with is_docx
  # mocked FALSE, raises neither a warning nor a message anywhere in the call
  # (confirmed directly) -- the readability
  # warning in apa_quarto_font_size() only fires below 6 pt, and 16 is far
  # above it. expect_silent() therefore asserts the strongest true thing
  # about this path rather than a weaker "the specific Word warning was
  # absent," which would pass even if some unrelated warning had appeared.
  local_mocked_bindings(apa_quarto_docx_target = function() FALSE)
  expect_silent(apa_quarto_markdown_block(apa.cor.table(attitude[, 1:3]), font.size = 16))
})

# --- the readability floor tells the truth on every destination ---
#
# The floor warning inside apa_quarto_font_size() is true on HTML because
# HTML applies the size exactly. Slides apply any size exactly too
# (apa_slide_font_pt()), so apa.quarto.revealjs(x, font.size = 2) must warn
# rather than deliver 2 pt in total silence. This section covers the slide
# half and confirms the shared apa_quarto_warn_below_floor() rule still reads
# correctly from apa_quarto_font_size()'s own caller.

test_that("a size below the readable floor warns on slides, which were silent", {
  tb <- apa.cor.table(attitude)
  plan <- apaTables:::apa_display_plan(apaTables:::as_spec(tb))
  expect_warning(apaTables:::apa_slide_font_pt(plan, font.size = 2),
                 "readable")
  # and the size is still HONOURED exactly -- an explicit size is warned
  # about, never refused
  pt <- suppressWarnings(apaTables:::apa_slide_font_pt(plan, font.size = 2))
  expect_identical(pt, 2)
})

test_that("a size at or above the floor is silent on slides", {
  tb <- apa.cor.table(attitude)
  plan <- apaTables:::apa_display_plan(apaTables:::as_spec(tb))
  expect_silent(apaTables:::apa_slide_font_pt(plan, font.size = 6))
  expect_silent(apaTables:::apa_slide_font_pt(plan, font.size = 12))
})

test_that("the floor warning is one flowing sentence with no newline", {
  msg <- tryCatch(apaTables:::apa_quarto_font_size(NULL, font.size = 3),
                  warning = function(e) conditionMessage(e))
  expect_false(grepl("\n", msg, fixed = TRUE))
  expect_match(msg, "6", fixed = TRUE)
})

# --- the floor is judged on the size DELIVERED, not the size requested
#
# Do not run the readability-floor check on the RAW request. On Word,
# font.size = 5 would then put two contradictory warnings into the same
# rendered draft: the floor warning saying the table is unreadable at 5 pt
# (FALSE -- Word can only ever deliver 6 pt or above, per
# apa_quarto_delivered_size()) sitting right beside
# apa_quarto_warn_word_size()'s true "Word sets this at 6, not the 5
# requested."
#
# The pair must be inspected TOGETHER, deliberately without
# suppressWarnings() (unlike test-render-markdown.R, which wraps a
# font.size = 5L call in suppressWarnings() for an unrelated reason and so
# cannot see this) -- a test that only checks "a warning fired"
# cannot tell a true Word-adjustment warning from the false floor warning
# riding along beside it.
test_that("on docx, the readability floor is judged on the delivered size, not the requested size", {
  local_mocked_bindings(apa_quarto_docx_target = function() TRUE)

  warnings_seen <- character(0)
  withCallingHandlers(
    apa_quarto_markdown_block(apa.cor.table(attitude[, 1:3]), font.size = 5),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # the true fact: Word cannot deliver 5 pt, and says so
  expect_true(any(grepl(
    "sets the table at 6 points rather than the 5 points requested",
    warnings_seen, fixed = TRUE)))
  # the false fact this bug asserted: that the table is unreadable at 5 pt
  # (it never ships at 5 pt on Word, so this must never fire here)
  expect_false(any(grepl("stops being readable", warnings_seen, fixed = TRUE)))
  # exactly the one true warning -- not two contradictory ones
  expect_length(warnings_seen, 1)
})

# The other half: PDF, HTML and slides deliver the
# requested size EXACTLY, so for them requested and delivered are the same
# value and the floor warning must still fire below 6 pt -- unlike Word,
# there is no adjustment warning to accompany it, because none of those
# destinations round or snap the size at all.
test_that("on a non-docx target, the readability floor still uses the requested size", {
  local_mocked_bindings(apa_quarto_docx_target = function() FALSE)

  warnings_seen <- character(0)
  withCallingHandlers(
    apa_quarto_markdown_block(apa.cor.table(attitude[, 1:3]), font.size = 5),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl("stops being readable", warnings_seen, fixed = TRUE)))
  expect_length(warnings_seen, 1)
})

# --- the exact-size marker is written for HTML only ---
#
# apa_quarto_markdown_block() must not write the exact-size marker div for
# every non-docx target. apatablesize.lua is registered for html and docx
# only, so a typst render would carry an unconsumed
# `::: {apa-table-size-pt="8"}` block. Pandoc's typst writer happens to drop
# it silently (measured: no rendered document is damaged), but nothing
# should depend on that.

test_that("the exact-size marker is written for HTML and withheld from typst", {
  spec <- apa.cor.table(attitude)

  emitted <- function(fmt) {
    testthat::local_mocked_bindings(
      apa_quarto_actual_format = function() fmt,
      apa_quarto_docx_target   = function() identical(fmt, "docx"),
      .package = "apaTables")
    out <- apaTables:::apa_quarto_markdown_block(spec, font.size = 8)
    grepl("apa-table-size-pt", out, fixed = TRUE)
  }

  expect_true(emitted("html"))        # the destination the marker exists for
  expect_false(emitted("typst"))      # the leak
  expect_false(emitted("gfm"))        # any other unknown paged/plain writer
})

test_that("the exact-size marker survives a direct call with no knit in progress", {
  # apa_quarto_actual_format() returns NA both when no knit is running AND
  # when a knit is running for a format we cannot classify. Those are not the
  # same situation and the narrowing must not conflate them: outside a knit
  # nothing is being produced, so nothing can leak, and every direct unit
  # call in this suite depends on the marker still being emitted.
  spec <- apa.cor.table(attitude)
  testthat::local_mocked_bindings(
    apa_quarto_actual_format = function() NA_character_,
    apa_quarto_docx_target   = function() FALSE,
    .package = "apaTables")
  out <- apaTables:::apa_quarto_markdown_block(spec, font.size = 8)
  expect_true(grepl("apa-table-size-pt", out, fixed = TRUE))
})

test_that("apa_quarto_html_target reports three states, not two", {
  html_target <- apaTables:::apa_quarto_html_target
  for (fmt in c("html", "html4", "html5")) {
    testthat::local_mocked_bindings(apa_quarto_actual_format = function() fmt,
                                    .package = "apaTables")
    expect_true(html_target(), info = fmt)
  }
  testthat::local_mocked_bindings(apa_quarto_actual_format = function() "typst",
                                  .package = "apaTables")
  expect_false(html_target())
  # FALSE is wider than "typst, gfm": pdf (via "latex") and the slide
  # destination (via "revealjs") also withhold the marker -- as does "pptx",
  # classified as nothing rather than as "revealjs" (R/quarto-table.R's
  # apa_quarto_actual_entry()).
  testthat::local_mocked_bindings(apa_quarto_actual_format = function() "latex",
                                  .package = "apaTables")
  expect_false(html_target())
  testthat::local_mocked_bindings(apa_quarto_actual_format = function() "revealjs",
                                  .package = "apaTables")
  expect_false(html_target())
  # "pptx" driven explicitly, so the claim above is a check and not only
  # prose.
  testthat::local_mocked_bindings(apa_quarto_actual_format = function() "pptx",
                                  .package = "apaTables")
  expect_false(html_target())
  testthat::local_mocked_bindings(
    apa_quarto_actual_format = function() NA_character_, .package = "apaTables")
  expect_true(is.na(html_target()))
})
