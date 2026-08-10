# The taught-style rule, checked in the PROSE surfaces as well as the templates.
#
# D104, ruled from real CRAN rejections and not open to re-argument: taught code
# builds the table on one line and displays it on the next --
# `my_table <- apa.cor.table(attitude)` then `apa.quarto.pdf(my_table)` -- never
# the pipe, and never the chained one-liner. C2: a sentence teaching *"the `|>`
# arrow"* is DELETED, not reworded.
#
# WHY THIS FILE EXISTS SEPARATELY FROM test-taught-style.R. That file guards
# `inst/templates/`, which an installed package carries, so it runs everywhere.
# The roxygen and the vignettes are SOURCE-ONLY: an installed package has the
# rendered `.Rd` and `.html`, not the files an author edits. The 2026-08-06
# sweep therefore left roughly 60 swept sites guarded by review alone -- open
# register item 5 -- and this closes it for the surfaces that can be read.
#
# WHAT IS DELIBERATELY NOT IN SCOPE: implementation pipes. Four exist
# (`vapply(...) |> paste0(collapse = )` and friends) and they are fine --
# **D104 governs what the reader is TAUGHT, not how the package is written.**
# That is why R files are searched only on roxygen (`#'`) lines: everything a
# reader is taught in an R file lives in a comment, and everything else is the
# package talking to itself.

apa_source_root <- function() {
  # tests run from tests/testthat/; the package root is two levels up when the
  # SOURCE tree is present (devtools::test, test_local). Under R CMD check the
  # tests are run from a copy with no R/ or vignettes/ beside them.
  root <- testthat::test_path("..", "..")
  if (dir.exists(file.path(root, "R")) &&
      dir.exists(file.path(root, "vignettes"))) normalizePath(root) else NA_character_
}

test_that("no taught code uses the pipe, in roxygen, vignettes or the home page", {
  root <- apa_source_root()
  skip_if(is.na(root), "source tree not available (installed package or R CMD check)")

  offenders <- character(0)
  seen <- 0L

  # (a) R files -- ROXYGEN LINES ONLY, so the four implementation pipes are
  #     out of scope by construction rather than by an exclusion list that
  #     would go stale.
  for (f in list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)) {
    txt <- readLines(f, warn = FALSE)
    rox <- grep("^\\s*#'", txt)
    if (!length(rox)) next
    seen <- seen + 1L
    hit <- rox[grepl("|>", txt[rox], fixed = TRUE)]
    if (length(hit))
      offenders <- c(offenders, sprintf("%s:%d", basename(f), hit))
  }

  # (b) vignettes and pkgdown articles, and (c) README.md, which IS the pkgdown
  #     home page -- every line of all three is addressed to a reader.
  prose <- c(
    list.files(file.path(root, "vignettes"), pattern = "[.]Rmd$",
               full.names = TRUE, recursive = TRUE),
    file.path(root, "README.md"))
  for (f in prose[file.exists(prose)]) {
    txt <- readLines(f, warn = FALSE)
    seen <- seen + 1L
    hit <- which(grepl("|>", txt, fixed = TRUE))
    if (length(hit))
      offenders <- c(offenders, sprintf("%s:%d", basename(f), hit))
  }

  # The loop must not be able to inspect nothing -- an every-file check that
  # silently iterates an empty list reads green and guards nothing.
  expect_gt(seen, 10L)
  expect_equal(offenders, character(0))
})

test_that("no document teaches the pipe by name", {
  root <- apa_source_root()
  skip_if(is.na(root), "source tree not available (installed package or R CMD check)")

  # C2: four sentences taught *"the `|>` arrow"* and were DELETED rather than
  # reworded. This catches the sentence coming back in words even if the
  # operator itself does not appear -- "the arrow operator", "pipe the table".
  # NOT a pattern for the operator itself -- the test above catches any
  # literal `|>` anywhere in these files, so a second pattern for it would be
  # redundant AND easy to get subtly wrong (the real sentence was "the `|>`
  # arrow", with backticks between the two halves). These match the sentence
  # when it comes back in WORDS, which is the gap the first test cannot see.
  patterns <- c("arrow\\s*operator", "pipe (it|the table|this)")
  files <- c(
    list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE),
    list.files(file.path(root, "vignettes"), pattern = "[.]Rmd$",
               full.names = TRUE, recursive = TRUE),
    file.path(root, "README.md"))
  files <- files[file.exists(files)]
  expect_gt(length(files), 10L)

  offenders <- character(0)
  for (f in files) {
    txt <- readLines(f, warn = FALSE)
    for (p in patterns) {
      hit <- which(grepl(p, txt, ignore.case = TRUE, perl = TRUE))
      if (length(hit))
        offenders <- c(offenders, sprintf("%s:%d (%s)", basename(f), hit, p))
    }
  }
  expect_equal(offenders, character(0))
})
