# Phase 1.18 / D114 -- the ONLY evidence in the suite that the width fix works.
#
# The rest of the suite cannot see this defect at all. A table whose cells are
# squashed and broken mid-value and one that prints whole are IDENTICAL HTML --
# the same blindness D46 recorded for slides, where an overflowing table and a
# fitted one produce the same markup. Only geometry differs, and geometry lives
# in a browser. test-html-structure.R asserts the wrapper is THERE; this file is
# the only thing that asserts it WORKS.
#
# So this shells out to headless Chrome via dev/factcheck/phase118-htmlwidth/
# measure.py, which reports, per table, its width and how many text nodes print
# on more than one line.
#
# WHERE THIS RUNS: under devtools::test() from the source tree. Inside
# `R CMD check` it SKIPS, because dev/ is .Rbuildignore'd and the script is not
# in the tarball -- deliberately; a CRAN machine has no browser either. That
# makes the skip below load-bearing rather than incidental: if this file reports
# SKIP on a development machine, the fix is UNTESTED, not fine.

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto not installed")
apa_skip_if_library_stale()

apa_chrome <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
apa_measure <- testthat::test_path("..", "..", "dev", "factcheck",
                                   "phase118-htmlwidth", "measure.py")

skip_if_not(file.exists(apa_chrome), "headless Chrome not available")
skip_if_not(nzchar(Sys.which("python3")), "python3 not available")
skip_if_not(file.exists(apa_measure),
            "measure.py not present (expected inside R CMD check: dev/ is not shipped)")

test_that("a wide table on a real HTML render breaks no text at any window width, D114", {
  # the PATH, not the text: Chrome loads the file from disk, beside its own
  # `_files/` directory, and reads the stylesheets from there.
  html <- apa_render_fixture_html("html-wide-table")

  out <- suppressWarnings(system2("python3",
    c(shQuote(apa_measure), shQuote(html), "--check-css"),
    stdout = TRUE, stderr = TRUE))
  out <- paste(out, collapse = "\n")

  # THE GUARD FIRST. A Quarto HTML render separated from its `_files/`
  # directory loads with no CSS, reports no wrapping whatsoever, and reads as a
  # pass -- that false read is what this flag exists for, and asserting it here
  # is what stops this whole test from certifying an unstyled page.
  expect_match(out, "apaCssActive=true", info = out)

  # the harness reported at all -- an empty parse would make the all() below
  # vacuously true
  totals <- regmatches(out, gregexpr("TOTAL brokenTextNodes=[0-9]+", out))[[1]]
  expect_gte(length(totals), 8L)

  broken <- as.integer(sub(".*=", "", totals))
  expect_true(all(broken == 0L), info = out)

  # and the READER'S PAGE never scrolls sideways, at any width: where there is
  # no room for the table to grow into, the table's own box scrolls instead
  # (D114, the author's second ruling).
  expect_false(grepl("pageScrollsSideways=true", out), info = out)

  # the fallback is actually REACHED, and this fixture is what reaches it: 20
  # variables is 14.42 in of table against a 6.50 in column, far past the 1.3 in
  # a table may reach into the margin. Without this assertion the test above
  # would also pass if the table had simply been squashed back into the column,
  # which is the defect.
  expect_true(grepl("ownScrollbar=true", out, fixed = TRUE), info = out)
})

test_that("a table that already fits is unchanged, D114", {
  # The other half of the matrix, and the reason Quarto's own column classes
  # were rejected: they would have stretched this table from the text-column
  # width to 9.88 in. dev/factcheck/phase118-htmlwidth/SPIKE-column-classes.md.
  html <- apa_render_fixture_html("html-kable-beside")

  out <- suppressWarnings(system2("python3",
    c(shQuote(apa_measure), shQuote(html), "--width", "1440", "--check-css"),
    stdout = TRUE, stderr = TRUE))
  out <- paste(out, collapse = "\n")

  expect_match(out, "apaCssActive=true", info = out)
  expect_match(out, "TOTAL brokenTextNodes=0", fixed = TRUE, info = out)

  # T1 is the apaTables table (3 variables, comfortably inside the column); it
  # must print at the text-column width, not at its own narrower natural width
  # and not wider. 6.50 in is what apaquarto's `body-width: 7in` less Quarto's
  # 3em gutter computes to -- asserted as a range so a gutter change reads as a
  # legible failure rather than an exact-match mystery.
  w <- as.numeric(sub(".*T1 width=([0-9.]+)in.*", "\\1", out))
  expect_gt(w, 6.3)
  expect_lt(w, 6.7)
})
