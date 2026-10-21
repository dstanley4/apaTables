# The documented way to display a table is TWO LINES AND NO PIPE --
# build the object, then pass it to a plain call. This rule comes from
# real CRAN rejections.
#
# NOTHING ELSE IN THE SUITE CAN SEE THIS. Rewriting every taught example
# across the roxygen, the vignettes, the shipped example documents and the
# README leaves the rest of the suite green whether it is done or not. A style
# rule with no assertion behind it is a rule that survives exactly as long as
# the memory of it does.
#
# WHAT THIS FILE CAN HONESTLY GUARD is the part that SHIPS as data and can be
# read back at test time: the example documents under inst/templates/. The
# roxygen and the vignettes are swept too, but they are not readable from an
# installed package in a form worth asserting against, so they are guarded by
# review rather than by this file. Say so rather than implying full coverage.

test_that("the shipped example documents teach two lines, never the pipe", {
  dir <- system.file("templates", package = "apaTables")
  skip_if(!nzchar(dir), "templates not installed")
  qmds <- list.files(dir, pattern = "[.]qmd$", full.names = TRUE)
  expect_gt(length(qmds), 0L)

  for (f in qmds) {
    txt <- readLines(f, warn = FALSE)

    # 1. No table is piped into an entry point.
    piped <- grep("\\|>\\s*apa\\.quarto\\.", txt, value = TRUE)
    expect_equal(piped, character(0),
                 info = paste(basename(f), "-- piped into an entry point"))

    # 2. And no prose teaches the arrow. Such sentences are deleted rather
    #    than reworded, so their absence is the assertion. Checked
    #    separately from (1) because a sentence naming the arrow contains no
    #    apa.quarto call and would slip past the first check entirely.
    arrow <- grep("\\|>.{0,20}arrow|arrow.{0,20}\\|>", txt, value = TRUE)
    expect_equal(arrow, character(0),
                 info = paste(basename(f), "-- prose teaching the arrow"))
  }
})

test_that("every entry-point call in a shipped example takes an argument", {
  # The complement of the check above, and the reason it is not redundant:
  # deleting a pipe satisfies "no pipe" while leaving `apa.quarto.pdf()` with
  # nothing to display -- a chunk that renders no table and reports no error.
  # This is the assertion that catches a bad two-line split.
  dir <- system.file("templates", package = "apaTables")
  skip_if(!nzchar(dir), "templates not installed")

  seen <- 0L
  for (f in list.files(dir, pattern = "[.]qmd$", full.names = TRUE)) {
    txt <- readLines(f, warn = FALSE)
    # Only lines that START a call -- prose in these documents mentions
    # `apa.quarto.pdf()` by name many times, and those are not calls.
    calls <- grep("^\\s*apa\\.quarto\\.(pdf|docx|html|revealjs)\\(", txt,
                  value = TRUE)
    # EVERY template must have one, including apaquarto-new.qmd -- the
    # STARTER document apa.quarto.new() writes. The bare-chunk path (a table
    # object as the chunk's last line, rendered by knit_print()) is still a
    # supported feature, but the FIRST document a new user opens must not
    # teach a different shape from every other example, so no file is
    # exempt.
    expect_true(length(calls) > 0L,
                info = paste(basename(f), "-- no entry-point call at all"))
    seen <- seen + 1L
    empty <- grep("^\\s*apa\\.quarto\\.\\w+\\(\\s*\\)\\s*$", calls, value = TRUE)
    expect_equal(empty, character(0),
                 info = paste(basename(f), "-- entry point called with nothing"))
  }
  # ...and the loop is not allowed to have inspected nothing, which is how an
  # every-file check quietly becomes decorative.
  expect_gt(seen, 0L)
})
