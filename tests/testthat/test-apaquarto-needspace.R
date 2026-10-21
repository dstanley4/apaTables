# A SHORT table must not straddle a page break -- and nothing must be left
# behind in the prose when it cannot help.
#
# A longtable is not wrapped in a float, which is what lets a tall table
# page-break with every row intact. The cost is that a SHORT table
# becomes breakable too, where a float would have moved it whole. That is not a
# theory: the very document below, rendered against the bundle as it stood
# before this behaviour changed (5.0.18+apaTables.2), with floats in the text,
# split the same 6-row table at every offset tried --
#
#   offset 6.0 in   rows 5/1 across pages 3-4, note on page 4
#   offset 6.4 in   rows 4/2 across pages 5-6, note on page 6
#   offset 6.8 in   rows 3/3 across pages 7-8, note on page 8
#   offset 7.2 in   rows 1/5 across pages 9-10, note on page 10
#
# -- so the "stays whole" assertions here are ones that HAVE been watched to
# fail, not ones assumed to be able to. Four offsets are rendered rather than
# one because the exact offset at which a break falls depends on the TeX
# distribution and the fonts; a single offset could land somewhere that never
# straddled on another machine, and the assertion would quietly become
# decorative.
#
# \Needspace* is the mechanism. The rejected alternative was starring every row
# (\\*), which makes the table unbreakable: 10 of 40 rows vanished from a
# document that compiled without a murmur. Hence the tall-table case below --
# a fix for the short table that breaks the tall one is not a fix.
#
# IT IS THE STARRED, EAGER FORM, AND THE LOWER-CASE \needspace IS A MEASURED
# DEFECT. \needspace only makes a break in front of the table CHEAP and
# leaves the decision to TeX's page builder; by the time that
# runs, longtable has installed its own output routine, which repeats the
# table's header on the new page above the caption, attached to nothing
# (measured across 26 documents, including the shipped example). \Needspace*
# does the comparison itself and breaks before \begin{longtable} is read.
#
# NOTE WHAT THIS FILE CAN AND CANNOT SEE. The straddle assertions below read
# which PAGE each row landed on, so they DO see the short-table property. They
# do NOT see a stranded header: a stranded head and a clean page differ only in
# a repeated header row, and every row marker is still present and still on one
# page either way. That defect is caught by test_that("the reservation is the
# EAGER form...") below, which pins the emitted command, and by looking at a
# render.
#
# The second half is the harder one to get right. The reservation is emitted
# ONLY where floats stay in the text. In the default placement apa7's endfloat
# re-emits the deferred environment alone and drops whatever preceded it, so
# the instruction never reaches the table; what it does reach is the
# manuscript's prose, where a bare reservation can force a page break no
# table justifies.
# Measured on a probe document with a table chunk mid-paragraph: with the
# guard in place the text before and after the chunk both sat on page 3 and the
# document was 4 pages; with the guard removed the text after the chunk was
# pushed to page 4 and the document grew to 5 pages, with no table on either.
#
# The default-placement assertion is an ABSENCE check, so it was made to fire
# before being trusted: replacing the guard with `if true then` in a project's
# copy of the extension took the count in the body .tex from 0 to 5.

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto not installed")
skip_if_not(nzchar(Sys.which("pdftotext")), "poppler (pdftotext) not installed")
skip_if_not(nzchar(Sys.which("pdfinfo")), "poppler (pdfinfo) not installed")

# quarto spawns its own R session and calls library(apaTables), so a render
# exercises the INSTALLED package -- its R code AND the extension it ships. A
# green result against a stale library certifies the library, not this branch,
# and a skip prints in the same summary line as a pass. The guard is in
# helper-stale-library.R and says so in words that cannot be read as success.
apa_skip_if_library_stale()

# Row markers are letters only. An underscore in a cell is math mode and
# ABORTS the render, and a digit would collide with the value column.
apa_ns_marker <- function(prefix, i) {
  a <- "abcdefghijklmnopqrstuvwxyz"
  paste0(prefix, "ROW",
         substr(a, ((i - 1L) %/% 26L) + 1L, ((i - 1L) %/% 26L) + 1L),
         substr(a, ((i - 1L) %% 26L) + 1L, ((i - 1L) %% 26L) + 1L))
}

# The four offsets, in inches, at which the unpatched bundle was measured to
# split this table, each with its own marker prefix. Six rows each.
APA_NS_OFFSETS <- c(SA = 6.0, SB = 6.4, SC = 6.8, SD = 7.2)
APA_NS_TALL <- 40L

# The ORPHANED-NOTE case, and it is a separate case because the first version
# of this file could not see it: with only 6-row tables the note assertion
# never had the chance to fail. At SEVEN rows and an offset of 5.6 in, a hint
# sized for the rows alone leaves the rows whole and pushes \endlastfoot -- the
# bottom rule and the note -- onto the next page, on its own under a repeated
# "Marker | Value" continuation header. Measured against the bundle at
# 8\baselineskip: rows whole on page 4, note alone on page 5. A table whose
# note has floated away is still a broken table, and it looks deliberate.
#
# Its note has TWO entries, which is what apaTables actually emits (cor and reg
# tables carry a stars sentence as a second entry), so the case exercises the
# height the hint has to cover rather than a best case.
APA_NS_ORPHAN <- list(prefix = "SE", rows = 7L, offset = 5.6)

apa_ns_note <- function(prefix) {
  if (identical(prefix, APA_NS_ORPHAN$prefix))
    return(paste0("[\"", prefix, "NOTEa is the first note entry, long enough ",
                  "to occupy a full line of the table width.\", \"", prefix,
                  "NOTEb is the second note entry, as a starred-significance ",
                  "sentence would be.\"]"))
  paste0("[\"", prefix, "NOTEa is the note sentence.\"]")
}

# The note marker a page break separates FIRST -- the last entry, not the
# first. Asserting on the first entry of a two-entry note would pass with the
# second entry stranded.
apa_ns_last_note <- function(prefix)
  paste0(prefix, if (identical(prefix, APA_NS_ORPHAN$prefix)) "NOTEb" else "NOTEa")

apa_ns_chunk <- function(prefix, n, label) {
  paste0(
    "```{r}\n",
    "#| label: ", label, "\n",
    "#| tbl-cap: \"Table ", prefix, "\"\n",
    "#| apa-note: ", apa_ns_note(prefix), "\n",
    "#| echo: false\n",
    "#| message: false\n",
    "library(apaTables)\n",
    "a <- \"abcdefghijklmnopqrstuvwxyz\"\n",
    "mk <- function(i) paste0(\"", prefix, "ROW\",\n",
    "  substr(a, ((i - 1L) %/% 26L) + 1L, ((i - 1L) %/% 26L) + 1L),\n",
    "  substr(a, ((i - 1L) %% 26L) + 1L, ((i - 1L) %% 26L) + 1L))\n",
    "n <- ", n, "\n",
    "sp <- apaTables:::apa_spec_start(n)\n",
    "sp <- apaTables:::apa_col(sp, \"Marker\", \"l\",\n",
    "                          vapply(seq_len(n), mk, character(1)))\n",
    "sp <- apaTables:::apa_col(sp, \"Value\", \"r\", sprintf(\"%d\", seq_len(n)))\n",
    "sp <- apaTables:::apa_spec_finish(sp, title = \"Table ", prefix, "\")\n",
    "knitr::asis_output(\n",
    "  apaTables:::apa_latex_table(apaTables:::apa_display_plan(sp), 10))\n",
    "```\n\n")
}

apa_ns_qmd <- function(placement) {
  body <- ""
  for (p in names(APA_NS_OFFSETS)) {
    body <- paste0(body,
      "```{=latex}\n\\clearpage\n\\vspace*{", APA_NS_OFFSETS[[p]],
      "in}\n```\n\n",
      apa_ns_chunk(p, 6L, paste0("tbl-", tolower(p))))
  }
  body <- paste0(body,
    "```{=latex}\n\\clearpage\n\\vspace*{", APA_NS_ORPHAN$offset, "in}\n```\n\n",
    apa_ns_chunk(APA_NS_ORPHAN$prefix, APA_NS_ORPHAN$rows,
                 paste0("tbl-", tolower(APA_NS_ORPHAN$prefix))))
  body <- paste0(body, "```{=latex}\n\\clearpage\n```\n\n",
                 apa_ns_chunk("TT", APA_NS_TALL, "tbl-tt"))
  paste0(
    "---\n",
    "title: \"Needspace\"\n",
    "shorttitle: \"Needspace\"\n",
    "author:\n",
    "  - name: Test Author\n",
    "    corresponding: true\n",
    "    email: test@example.org\n",
    "    affiliations:\n",
    "      - name: Test University\n",
    if (identical(placement, "intext")) "floatsintext: true\n" else "",
    "format:\n",
    "  apaquarto-pdf:\n",
    "    keep-tex: true\n",
    "---\n\n",
    "## Results\n\n", body,
    "## Discussion\n\nENDMARKER closes the body.\n")
}

# Render once per placement, and hold the results so each assertion reads a
# document rather than re-rendering one.
apa_ns_render <- function(placement) {
  proj <- file.path(tempdir(), paste0("needspace-", placement))
  unlink(proj, recursive = TRUE)
  # A FRESH project every time: apa.quarto.new() copies the extension into the
  # project, so a reused folder would render against whatever copy was left
  # there by an earlier run.
  suppressMessages(apa.quarto.new(proj, filename = "doc.qmd"))
  writeLines(apa_ns_qmd(placement), file.path(proj, "doc.qmd"))
  log <- suppressWarnings(system2(
    "quarto", c("render", shQuote(file.path(proj, "doc.qmd"))),
    stdout = TRUE, stderr = TRUE))
  list(proj = proj, log = log, pdf = file.path(proj, "doc.pdf"),
       tex = file.path(proj, "doc.tex"), ttt = file.path(proj, "doc.ttt"))
}

# How many times the reservation COMMAND appears -- never the
# \usepackage{needspace} line, which is preamble and is expected in every
# document regardless of placement. Counting bare "needspace" would make the
# default-placement assertion unfailable, and it is also now CASE-BLIND in a
# way that matters: `\Needspace*` and `\needspace` differ by one letter and
# mean different mechanisms.
#
# OCCURRENCES, not matching lines. Two reservations on one line is not a
# far-fetched shape -- pandoc is free to run adjacent raw blocks together --
# and a line count would silently report 1 for 2, understating exactly the
# quantity the default-placement assertion exists to hold at zero.
apa_ns_count <- function(path, cmd = "\\Needspace*{") {
  if (!file.exists(path)) return(NA_integer_)
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  m <- gregexpr(cmd, txt, fixed = TRUE)[[1]]
  if (length(m) == 1L && m[1] == -1L) 0L else length(m)
}

# Every table in this document must reach the PDF whole: all its rows on one
# page, and its note -- the LAST entry of it -- on that same page.
apa_ns_expect_whole <- function(pages, p, n) {
  expected <- vapply(seq_len(n), function(i) apa_ns_marker(p, i), character(1))
  row_pages <- vapply(expected, function(m) apa_page_of(pages, m), integer(1))

  # Every row reached the PDF -- compared as a SET, never as a count. A
  # hand-rolled tally once reported 37 of 40 and read as data loss that was
  # not there.
  found <- unique(unlist(regmatches(
    pages, gregexpr(paste0(p, "ROW[a-z]{2}"), pages))))
  expect_equal(sort(setdiff(expected, found)), character(0),
               info = paste(p, "-- rows missing from the PDF"))
  expect_equal(sort(setdiff(found, expected)), character(0),
               info = paste(p, "-- unexpected rows in the PDF"))

  # And they are all on ONE page.
  expect_equal(length(unique(row_pages[!is.na(row_pages)])), 1L,
               info = paste(p, "-- the table straddled a page break, rows on",
                            paste(sort(unique(row_pages)), collapse = "/")))

  # The note travels with it. \endlastfoot is the last thing in the table, so
  # it is what a break separates first -- and it can be separated while every
  # row stays whole, which is the SE case.
  note <- apa_ns_last_note(p)
  note_page <- apa_page_of(pages, note)
  expect_false(is.na(note_page),
               info = paste(p, "-- the note never reached the PDF"))
  expect_true(!is.na(note_page) &&
                identical(note_page, unname(row_pages[[1]])),
              info = paste0(p, " -- the note (", note, ", p", note_page,
                            ") is not on its table's page (p",
                            row_pages[[1]], ")"))
}

test_that("a short longtable is kept whole where floats stay in the text", {
  r <- apa_ns_render("intext")
  expect_true(file.exists(r$pdf), info = paste(r$log, collapse = "\n"))
  skip_if_not(file.exists(r$pdf), "render produced no output to read")

  pages <- apa_pdf_pages(r$pdf)

  # The four 6-row offsets. These are the assertions the unpatched bundle
  # fails, at all four.
  for (p in names(APA_NS_OFFSETS)) apa_ns_expect_whole(pages, p, 6L)

  # And the 7-row orphaned-note case, which a hint sized for the rows alone
  # passes on the rows and fails on the note.
  apa_ns_expect_whole(pages, APA_NS_ORPHAN$prefix, APA_NS_ORPHAN$rows)

  # The tall table STILL splits, with every row present. This is the half
  # that the rejected \\* alternative failed: it kept short tables whole by
  # making the environment unbreakable, and 10 of 40 rows disappeared.
  expected <- vapply(seq_len(APA_NS_TALL), function(i) apa_ns_marker("TT", i),
                     character(1))
  found <- unique(unlist(regmatches(pages, gregexpr("TTROW[a-z]{2}", pages))))
  expect_equal(sort(setdiff(expected, found)), character(0))
  expect_equal(sort(setdiff(found, expected)), character(0))
  tall_pages <- unique(vapply(expected, function(m) apa_page_of(pages, m),
                              integer(1)))
  expect_gt(length(tall_pages[!is.na(tall_pages)]), 1L)

  # One reservation per table, in the body .tex, where floats stay in the text.
  expect_equal(apa_ns_count(r$tex), length(APA_NS_OFFSETS) + 2L)
})

# The one-letter difference between `\Needspace*` and `\needspace` is the
# difference between a break decided BEFORE the longtable begins and one
# decided after, inside longtable's own output routine -- and the latter
# stamps a repeated header on the new page, above the caption,
# attached to nothing.
#
# THIS ASSERTION EXISTS BECAUSE NOTHING ELSE IN THE SUITE CAN SEE THAT DEFECT.
# The straddle checks above read which page each row landed on; a stranded
# header changes neither the set of rows nor the page they sit on, so those
# assertions stay green with the defect present. The page evidence is a
# render that was rasterised and looked at; what this test can honestly
# hold is the COMMAND, and a revert to the lower-case form is exactly how
# the defect would come back.
test_that("the reservation is the EAGER form, not the page-builder one", {
  r <- apa_ns_render("intext")
  skip_if_not(file.exists(r$tex), "render produced no .tex to read")

  # The eager form is present once per table...
  expect_equal(apa_ns_count(r$tex, "\\Needspace*{"),
               length(APA_NS_OFFSETS) + 2L)

  # ...and the deferred, glue-based form appears NOWHERE. Counted with the
  # same helper against the other command rather than by grepping the file
  # here, so both counts are subject to the same occurrences-not-lines rule.
  #
  # \usepackage{needspace} is preamble and is NOT matched: the helper looks
  # for the command with its brace, never the bare package name.
  expect_equal(apa_ns_count(r$tex, "\\needspace{"), 0L)
})

test_that("no reservation is emitted where floats are collected at the end", {
  r <- apa_ns_render("end")
  expect_true(file.exists(r$pdf), info = paste(r$log, collapse = "\n"))
  skip_if_not(file.exists(r$pdf), "render produced no output to read")

  # Zero, in the body and in the deferred file alike. endfloat strips it from
  # the deferred copy, so emitting it there would buy nothing and leave the
  # command stranded in the prose, where it forces a break no table justifies.
  #
  # This check has been WATCHED to fire: with the emitting guard replaced by
  # `if true then` in the project's copy of apafloatlatex.lua, the body count
  # went from 0 to 5 while the .ttt stayed at 0.
  #
  # BOTH COMMANDS, and this is not belt-and-braces. The helper counts a
  # SPECIFIC command, not "the reservation" whatever form it takes.
  # Checking only `\Needspace*` here would be satisfied by six lower-case
  # `\needspace` sitting in the manuscript's prose -- precisely
  # the defect this test exists to prevent, passing green.
  for (cmd in c("\\Needspace*{", "\\needspace{")) {
    expect_equal(apa_ns_count(r$tex, cmd), 0L, info = cmd)
  }
  # doc.ttt MUST exist here -- this is the end placement, so endfloat wrote
  # the deferred environments into it. Without pinning that, the disjunction
  # below is satisfied by the file simply not being there, and the claim
  # "zero in the deferred file too" is made about nothing.
  expect_true(file.exists(r$ttt), info = "endfloat wrote no doc.ttt")
  for (cmd in c("\\Needspace*{", "\\needspace{")) {
    expect_equal(apa_ns_count(r$ttt, cmd), 0L, info = cmd)
  }

  # ...and the document is still correct without it, which is the reason it is
  # not needed here: endfloat gives each deferred float its own page, so a
  # short table has nothing to straddle.
  pages <- apa_pdf_pages(r$pdf)
  for (p in names(APA_NS_OFFSETS)) apa_ns_expect_whole(pages, p, 6L)
  apa_ns_expect_whole(pages, APA_NS_ORPHAN$prefix, APA_NS_ORPHAN$rows)

  expected <- vapply(seq_len(APA_NS_TALL), function(i) apa_ns_marker("TT", i),
                     character(1))
  found <- unique(unlist(regmatches(pages, gregexpr("TTROW[a-z]{2}", pages))))
  expect_equal(sort(setdiff(expected, found)), character(0))
  expect_equal(sort(setdiff(found, expected)), character(0))
})
