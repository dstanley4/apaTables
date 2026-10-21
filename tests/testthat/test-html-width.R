# The only test in the suite that checks the HTML width fix WORKS, as opposed
# to checking that its markup is present (test-html-structure.R does that).
#
# A table whose cells are squashed and broken mid-value and one that prints
# whole are IDENTICAL HTML; only the geometry differs, and geometry lives in a
# browser. So this renders a 20-variable fixture and measures it in headless
# Chrome, reporting each table's width and how many text nodes print on more
# than one line, at a range of window widths.
#
# MAINTAINER-ONLY. The measuring script lives in the maintainer's local
# development notes, which are not part of the public repository or the
# package tarball, and it needs Google Chrome. Everywhere else -- CRAN, CI,
# a contributor's clone -- this file skips with a message saying so. On the
# maintainer's machine a SKIP here means the width fix is untested, not fine.
# Set APATABLES_CHROME to point at a Chrome binary outside the macOS default.

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto not installed")
apa_skip_if_library_stale()

apa_chrome <- Sys.getenv("APATABLES_CHROME",
                         "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
apa_measure <- testthat::test_path("..", "..", "dev", "factcheck",
                                   "phase118-htmlwidth", "measure.py")

skip_if_not(file.exists(apa_measure),
            paste("maintainer-only HTML width measurement: needs a local",
                  "measuring script that is not in the public repository"))
skip_if_not(file.exists(apa_chrome),
            "headless Chrome not available (set APATABLES_CHROME to its path)")
skip_if_not(nzchar(Sys.which("python3")), "python3 not available")

test_that("a wide table on a real HTML render breaks no text at any window width", {
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
  # no room for the table to grow into, the table's own box scrolls instead.
  expect_false(grepl("pageScrollsSideways=true", out), info = out)

  # the fallback is actually REACHED, and this fixture is what reaches it: 20
  # variables is 14.42 in of table against a 6.50 in column, far past the 1.3 in
  # a table may reach into the margin. Without this assertion the test above
  # would also pass if the table had simply been squashed back into the column,
  # which is the defect.
  expect_true(grepl("ownScrollbar=true", out, fixed = TRUE), info = out)
})

test_that("a table that already fits is unchanged", {
  # The other half of the matrix, and the reason Quarto's own column classes
  # were rejected: they would have stretched this table from the text-column
  # width to 9.88 in (measured).
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
