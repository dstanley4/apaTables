# The Phase 2 acceptance matrix: the native LaTeX emitter, end to end.
#
# test-apaquarto-acceptance.R is the Phase 1.5 matrix and still covers the
# FLEXTABLE path. This file covers the path Phase 2 built -- every table here
# goes through apa.quarto.pdf(), which measures the table, decides its type
# size and whether the page turns, and writes the longtable grid itself.
#
# The four requirements are the author's, and they are binding. A matrix that
# is green while any of them is broken is worse than no matrix at all:
#
#   R1  BOTH PLACEMENTS WORK -- tables floating in the text (floatsintext) AND
#       tables collected at the end of the manuscript (apaquarto's default,
#       apa7 endfloat). Neither may be sacrificed for the other.
#   R2  TABLE NUMBERS ARE CORRECT -- consecutive from 1, and every @tbl- cross
#       reference resolves to the number its own table carries.
#   R3  LONG TABLES WORK -- a table taller than the page continues onto the
#       next one, keeps EVERY row, and keeps its note attached to ITSELF.
#   R4  A WARNING IS NOT A SOLUTION -- nothing here may be certified by a
#       warning. No warning may reach the rendered manuscript at all.
#
# THE SHAPES. Two placements x two formats x four shapes:
#
#            narrow                       wide
#   short    fits, upright, one page      rotates, one page
#   long     spans pages, upright         SPANS PAGES AND ROTATES
#
# The last is the cell Phase 1.5 could not deliver: rotation meant apa7's
# sidewaystable, a FLOAT, and a longtable cannot live in a float, so a wide and
# long table with tables collected at the end came out upright, starred, 51 of
# 60 rows, with its note missing from the PDF entirely -- exit 0, no error, no
# warning. All four shapes are in every document, because the numbering defect
# only appears when shapes are MIXED.
#
# THE ORDER OF THE FOUR TABLES IS NOT ARBITRARY: spans, fits, fits, spans. The
# upstream counter defect latches on the first captioned longtable and then
# costs the SECOND ordinary float after it, so a document whose tables are all
# one shape numbers correctly even with the patch reverted, and a matrix built
# on one would certify nothing.
#
# WATCHED FAILING, against the PRE-PHASE-2 bundle (5.0.18+apaTables.1) with
# this branch's R: captions read 1, 2, 3 -- the fourth table's caption is not
# renumbered, it is GONE, because the environment came out starred and a
# starred longtable carries no caption. The same run lost Td49 and Td50, left
# the wide tables upright, dropped a note out of the PDF entirely, and exited
# 0. Full record in the Task 11 report.
#
# NOTHING IN THESE DOCUMENTS ASKS FOR WHAT IT IS MEANT TO RECEIVE. There is no
# `::: {.landscape}` div, no `#| apa-landscape` option and no
# `#| apa-longtable` option anywhere in the matrix -- asserted of the document
# text before it is rendered. That is not tidiness. Task 8 measured that with
# `#| apa-longtable: true` present, EVERY cell of the Phase 1.5 matrix stayed
# green while the interim landscape guard was restored by hand, because the Lua
# branch reads `apa-longtable == "true" OR haslongtable(float)` and the option
# satisfies the first disjunct before the detection under test is ever reached.
# The option is still supported, and there is a cell below that uses it -- but
# it is a cell of its own, never the matrix's.
#
# HOW EACH THING IS DECIDED:
#
#   rows        DISTINCT MARKER SETS, compared with setdiff in BOTH
#               directions, never a tally -- a tally read 37 of 40 during the
#               spikes and looked like data loss that was not there.
#   rotation    by RASTERIZING the page. `strings file.pdf | grep /Rotate`
#               returned 0 on a genuinely rotated document and pdfinfo's
#               per-page report was no better.
#   notes       IN A PDF, by the PAGE the note lands on; IN A .docx, which has
#               no pages at all, by DOCUMENT ORDER, bracketed on BOTH sides --
#               after its own table's last row, before the next table's, with
#               nothing else in between. The two are different checks and are
#               named separately because they have to be: whichever currency is
#               used, the point is ADJACENCY. With the note-placement defect
#               every note marker still EXISTS in the document, merely stranded
#               away from its table, so any(grepl(...)) passes against the exact
#               defect this matrix exists to catch, and so does a bare
#               "after its own table" in a format with no pages.
#   warnings    in the RENDERED DOCUMENT. knitr writes an R warning into the
#               manuscript, never onto quarto's console, so a log-only check is
#               constant-FALSE.
#   overfull    from the LaTeX log, which is why these documents set
#               `latex-clean: false`. A table wider than its page does not
#               fail; it silently runs into the margin, and `Overfull \hbox` is
#               the only thing that says so.
#
# A SUCCESSFUL RENDER PROVES NOTHING. Measured this phase: a starred
# environment dropped 15 of 60 rows in one document and 38 of 60 in another,
# the page still correctly rotated, both exiting 0 with no warning. Every
# assertion here is about content.
#
# Every render is a real quarto render, so the file skips unless the whole
# toolchain is present. It must never fail a CRAN check.

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto not installed")
skip_if_not(nzchar(Sys.which("pdftotext")), "poppler (pdftotext) not installed")
skip_if_not(nzchar(Sys.which("pdfinfo")), "poppler (pdfinfo) not installed")
skip_if_not(nzchar(Sys.which("pdftoppm")), "poppler (pdftoppm) not installed")

# quarto spawns its own R session and calls library(apaTables), so a render
# exercises the INSTALLED package -- its R code AND the extension it ships --
# and never this source tree. A stale library does not merely certify the
# wrong thing: it makes every render test SKIP, which reads as a pass when
# they never ran. The guard is in helper-stale-library.R and compares both
# halves; use a fresh project directory per render, because apa.quarto.new()
# COPIES the extension in.
apa_skip_if_library_stale()

# ---------------------------------------------------------------------------
# The four shapes.
#
# apa.2way.table() is the builder because it is the only one whose WIDTH and
# HEIGHT are independently controllable from real data: the number of levels of
# the second factor sets the columns, the first factor sets the rows. Its row
# labels are the factor LEVELS, so each row can carry its own marker verbatim,
# and its column headings are the second factor's levels, so column loss is
# checkable too.
#
# The widths are measured, not guessed (apa_layout_decide on each spec):
#   4 columns of cells -> 3.133 in   fits the 6.5 in upright text block
#  10 columns of cells -> 7.217 in   past it, inside the 9 in rotated one
# so "narrow" and "wide" here mean what the ladder decides, and the wide ones
# are in the branch D7 exists for: rotate, and stay at 10 pt.
#
# EVERY TABLE HAS ITS OWN COLUMN-HEADING PREFIX as well as its own row prefix,
# and that is not cosmetic. Fix round 1: the heading check used to look for one
# shared "Bb01..BbN" across the whole document, so it could only ever see
# document-wide heading loss -- deleting a heading from the narrow tables left
# it TRUE, because the wide table still carried the same string. A per-table
# prefix makes the heading check exactly the set comparison the row check is,
# and it fails when ONE table loses a heading.
p2_shapes <- list(
  list(prefix = "Ta", cprefix = "Ha", n1 = 50L, n2 = 4L,
       long = TRUE,  wide = FALSE,
       cap = "A Narrow Table That Spans More Than One Page"),
  list(prefix = "Tb", cprefix = "Hb", n1 = 5L,  n2 = 4L,
       long = FALSE, wide = FALSE,
       cap = "A Narrow Table That Fits on One Page"),
  list(prefix = "Tc", cprefix = "Hc", n1 = 5L,  n2 = 10L,
       long = FALSE, wide = TRUE,
       cap = "A Wide Table That Fits on One Page"),
  list(prefix = "Td", cprefix = "Hd", n1 = 50L, n2 = 10L,
       long = TRUE,  wide = TRUE,
       cap = "A Wide Table That Spans More Than One Page")
)

p2_setup <- paste0(
  "```{r}\n#| label: p2-setup\n#| echo: false\n#| message: false\n",
  "library(apaTables)\n",
  "set.seed(20260801)\n",
  "p2_data <- function(n1, n2, prefix, cprefix) {\n",
  "  d <- expand.grid(\n",
  "    a = factor(sprintf(paste0(prefix, \"%02d\"), seq_len(n1))),\n",
  "    b = factor(sprintf(paste0(cprefix, \"%02d\"), seq_len(n2))),\n",
  "    rep = 1:3)\n",
  "  d$y <- stats::rnorm(nrow(d), 10, 2)\n",
  "  d\n}\n```\n\n")

p2_yaml <- function(fmt, opts) {
  paste0(
    "---\n",
    "title: \"A Phase 2 Acceptance Manuscript\"\n",
    "shorttitle: \"Phase 2 Acceptance\"\n",
    "author:\n  - name: Test Author\n    corresponding: true\n",
    "    email: test@example.org\n    affiliations:\n",
    "      - name: Test University\n",
    if (length(opts))
      paste0("format:\n  ", fmt, ":\n", paste0("    ", opts, "\n", collapse = ""))
    else paste0("format: ", fmt, "\n"),
    "---\n\n")
}

p2_qmd <- function(placement, format) {
  fmt <- if (identical(format, "pdf")) "apaquarto-pdf" else "apaquarto-docx"
  # keep-tex so the MECHANISM can be read (which environment, and whether the
  # landscape wrapper survived endfloat); latex-clean so the LaTeX log
  # survives, which is the only place an Overfull \hbox is reported.
  opts <- c(if (identical(format, "pdf")) c("keep-tex: true", "latex-clean: false"),
            if (identical(placement, "intext")) "floatsintext: true")

  # THE CALL MATCHES THE RENDER TARGET, deliberately (Task 8, D30): a
  # docx cell calling apa.quarto.pdf() would still produce a correct table
  # (the entry points are forgiving), but it would ALSO fire the
  # name/target mismatch message into the rendered document -- and R4 is
  # binding: "no warning may reach the rendered manuscript at all". Using
  # the matching name keeps this matrix a test of the emitter, not of the
  # mismatch guidance (which has its own coverage in
  # test-quarto-entrypoints.R).
  entry_fn <- if (identical(format, "pdf")) "apa.quarto.pdf" else "apa.quarto.docx"
  chunk <- function(k) {
    s <- p2_shapes[[k]]
    paste0(
      "```{r}\n",
      "#| label: tbl-t", k, "\n",
      "#| tbl-cap: \"", s$cap, "\"\n",
      "#| apa-note: \"NOTE-MARKER-", k, " is the note belonging to this table.\"\n",
      "#| echo: false\n#| message: false\n#| warning: true\n",
      "apa.2way.table(iv1 = a, iv2 = b, dv = y,\n",
      "               data = p2_data(", s$n1, ", ", s$n2, ", \"", s$prefix,
      "\", \"", s$cprefix, "\")) |>\n  ", entry_fn, "()\n",
      "```\n\n")
  }

  paste0(
    p2_yaml(fmt, opts), p2_setup,
    "## Results\n\n",
    "This paragraph opens the body of the manuscript, above every table.\n\n",
    "XREF: @tbl-t1, @tbl-t2, @tbl-t3, @tbl-t4.\n\n",
    paste0(vapply(seq_along(p2_shapes), chunk, character(1)), collapse = ""),
    # The TRAILING body paragraph is what makes placement decidable. A marker
    # BEFORE the tables cannot decide it: under endfloat the first deferred
    # table lands on the page immediately after the body, so "some table is
    # within a page of the body" is true of a document with ZERO tables in the
    # text. Against a marker that comes AFTER every table the two placements
    # are opposites.
    "ENDMARKER closes the body of this manuscript, after every table has been ",
    "declared.\n")
}

# ---------------------------------------------------------------------------
# Render into a project of its own. apa.quarto.new() COPIES the extension in,
# so a reused directory would render against whatever the previous test left
# behind.
p2_render <- function(name, qmd, format) {
  proj <- file.path(tempdir(), paste0("p2-", name))
  unlink(proj, recursive = TRUE)
  suppressMessages(apa.quarto.new(proj, filename = "doc.qmd"))
  writeLines(qmd, file.path(proj, "doc.qmd"))
  log <- suppressWarnings(system2(
    "quarto", c("render", shQuote(file.path(proj, "doc.qmd"))),
    stdout = TRUE, stderr = TRUE))
  list(proj = proj, log = apa_strip_ansi(log),
       out = file.path(proj, paste0("doc.", format)))
}

# `Overfull \hbox` lines in the LaTeX log. A table wider than its page does not
# fail the render -- it runs into the margin and off the paper -- so this is
# the only mechanical evidence that the width the emitter computed is the width
# the page can hold.
p2_overfull <- function(proj) {
  log <- file.path(proj, "doc.log")
  if (!file.exists(log)) return(NA_integer_)
  sum(grepl("Overfull \\hbox", readLines(log, warn = FALSE), fixed = TRUE))
}

# The distinct markers actually present, as a SET.
p2_found <- function(txt, prefix) {
  sort(unique(unlist(regmatches(
    txt, gregexpr(paste0(prefix, "[0-9]{2}"), txt)))))
}
p2_expected <- function(prefix, n) sort(sprintf(paste0(prefix, "%02d"), seq_len(n)))

# Both directions. setdiff one way catches loss; the other way catches a
# marker that appeared from nowhere (a shifted column, a duplicated row).
p2_expect_marker_set <- function(txt, prefix, n, where, what = "row") {
  found <- p2_found(txt, prefix)
  expected <- p2_expected(prefix, n)
  expect_equal(setdiff(expected, found), character(0),
               info = paste(where, prefix, "--", what, "MISSING from the table"))
  expect_equal(setdiff(found, expected), character(0),
               info = paste(where, prefix, "--", what,
                            "present that should not exist"))
}

# ---------------------------------------------------------------------------
# The matrix.
cells <- expand.grid(placement = c("end", "intext"),
                     format = c("pdf", "docx"),
                     stringsAsFactors = FALSE)

for (i in seq_len(nrow(cells))) {
  cell <- cells[i, ]
  label <- paste(cell$format, cell$placement, sep = "-")

  test_that(paste("phase 2 acceptance:", label), {
    qmd <- p2_qmd(cell$placement, cell$format)

    # The document must not be able to ask for what it is meant to receive.
    expect_false(grepl("landscape", qmd, fixed = TRUE))
    expect_false(grepl("apa-longtable", qmd, fixed = TRUE))

    r <- p2_render(label, qmd, cell$format)
    expect_true(file.exists(r$out), info = paste(r$log, collapse = "\n"))
    skip_if_not(file.exists(r$out), "render produced no output to read")

    if (cell$format == "pdf") {
      pages <- apa_pdf_pages(r$out)
      shapes <- apa_pdf_page_shapes(r$out)
      expect_length(shapes, length(pages))
      all_txt <- paste(pages, collapse = "\n")

      # R2a / R2b: captions consecutive from 1, and the body's own
      # cross-references resolving to the SAME numbers. The counter defect
      # corrupts both together, so checking captions alone would miss a
      # document where both read "Table 1".
      expect_equal(apa_pdf_caption_numbers(pages), 1:4)
      expect_equal(apa_xref_numbers(pages), 1:4)

      first_page <- integer(4); last_page <- integer(4); note_page <- integer(4)
      for (k in seq_along(p2_shapes)) {
        s <- p2_shapes[[k]]

        # R3a: EVERY row, as a set.
        p2_expect_marker_set(all_txt, s$prefix, s$n1, label)

        # Every COLUMN HEADING of THIS table, as a set, both directions. A `&`
        # that reached LaTeX unescaped shifts the cells of a row into the wrong
        # columns and can still exit 0; a too-wide table loses its right-hand
        # columns off the page edge. Each table's headings carry its own prefix
        # so this cannot be satisfied by a sibling table that still has them.
        p2_expect_marker_set(all_txt, s$cprefix, s$n2, label,
                             what = "column heading")

        occupied <- which(vapply(pages, function(p)
          grepl(paste0(s$prefix, "[0-9]{2}"), p), logical(1)))
        expect_true(length(occupied) > 0L)
        first_page[k] <- occupied[1]
        last_page[k] <- apa_page_of(pages, sprintf(paste0(s$prefix, "%02d"), s$n1))

        # R3c: the tables that are supposed to span DO span, and the ones that
        # are not supposed to DO NOT. Without the second half a table that
        # silently collapsed onto one page would still carry its markers, its
        # note and its number.
        if (s$long) {
          expect_true(first_page[k] < last_page[k],
                      info = paste(label, s$prefix, "-- did not span pages"))
        } else {
          expect_equal(first_page[k], last_page[k],
                       info = paste(label, s$prefix, "-- spread over pages"))
        }

        # Rotation, on every page the table occupies, decided by rasterizing.
        # Both directions: a cell where everything rotated would satisfy an
        # "a page was rotated" check on its own.
        want <- if (s$wide) "landscape" else "portrait"
        expect_true(all(shapes[occupied] == want),
                    info = paste(label, s$prefix, "-- wanted", want, "got",
                                 paste(shapes[occupied], collapse = " ")))

        # R3b: the note is on the page its OWN table ends on.
        note_page[k] <- apa_page_of(pages, paste0("NOTE-MARKER-", k))
      }
      expect_false(any(is.na(note_page)), info = "a note vanished")
      expect_equal(note_page, last_page)

      # R1: placement, decided against the closing body paragraph. The two
      # directions are deliberately not symmetric: "collected at the end" is a
      # claim about EVERY table, while LaTeX may legitimately carry one float
      # in the text past the closing paragraph.
      end_page <- apa_page_of(pages, "ENDMARKER")
      expect_false(is.na(end_page), info = "the closing paragraph vanished")
      if (cell$placement == "end") {
        expect_true(all(last_page > end_page),
                    info = "a table did not move to the end of the manuscript")
      } else {
        expect_true(any(last_page < end_page),
                    info = "no table stayed in the text")
      }

      # The MECHANISM, read from each .tex in its own right. Under endfloat the
      # .ttt holds a COPY that endfloat re-emitted alone, having dropped what
      # surrounded it, so concatenating the two hides a defect present in only
      # one of them and makes every "exactly one" count read two.
      # Where floats are collected at the end, doc.ttt MUST be there: endfloat
      # writes the deferred environment into it and the body .tex then holds
      # no table at all. Without this the file list silently narrows to
      # doc.tex alone and every mechanism assertion below runs on a file that
      # cannot fail them.
      if (cell$placement == "end") {
        expect_true(file.exists(file.path(r$proj, "doc.ttt")),
                    info = paste(label, "-- endfloat wrote no doc.ttt"))
      }
      tex_files <- file.path(r$proj, c("doc.tex", "doc.ttt"))
      tex_files <- tex_files[file.exists(tex_files)]
      expect_true(length(tex_files) > 0L)
      for (tf in tex_files) {
        tex <- readLines(tf, warn = FALSE)
        where <- paste(label, basename(tf))
        # D9: unstarred. Quarto rewrites a captionless raw longtable to
        # longtable*, which every later longtable fixup is blind to and which
        # silently loses rows.
        expect_equal(sum(grepl("\\begin{longtable}", tex, fixed = TRUE)), 4L,
                     info = where)
        expect_false(any(grepl("\\begin{longtable*}", tex, fixed = TRUE)),
                     info = paste(where, "-- an environment came out starred"))
        # A longtable cannot live in a float. sidewaystable is a float, and is
        # what Phase 2 replaced.
        expect_false(any(grepl("\\begin{table}", tex, fixed = TRUE)),
                     info = paste(where, "-- a longtable is inside a float"))
        expect_false(any(grepl("sidewaystable", tex, fixed = TRUE)), info = where)
        # Exactly the two wide tables are wrapped. Zero would mean the decision
        # never crossed from R into apaquarto; four would mean everything
        # rotated and the per-table rotation assertions above were satisfied by
        # a bug.
        expect_equal(sum(trimws(tex) == "\\begin{landscape}"), 2L, info = where)
        expect_equal(sum(trimws(tex) == "\\end{landscape}"), 2L, info = where)
      }

      # Nothing ran off the page.
      expect_equal(p2_overfull(r$proj), 0L)

      # R4: no warning reached the manuscript. knitr writes an R warning INTO
      # the document, not onto quarto's console, so this is where the reader
      # meets it and this is where it is looked for.
      expect_false(any(grepl("Warning:", pages, fixed = TRUE)),
                   info = paste(grep("Warning:", unlist(strsplit(pages, "\n")),
                                     value = TRUE), collapse = " | "))
    } else {
      styles <- apa_docx_styles(r$out)
      txt <- apa_docx_text(r$out)

      # R2: four tables with the full APA furniture, numbered consecutively,
      # and the body's references agreeing.
      expect_equal(sum(styles == "FigureTitle"), 4L)
      expect_equal(sum(styles == "FigureNote"), 4L)
      caps <- as.integer(unlist(regmatches(
        txt, gregexpr("(?m)^\\s*Table\\s+\\K[0-9]+", txt, perl = TRUE))))
      expect_equal(caps, 1:4)
      expect_equal(apa_xref_numbers(txt), 1:4)

      pos_last <- integer(4); pos_note <- integer(4)
      for (k in seq_along(p2_shapes)) {
        s <- p2_shapes[[k]]
        # R3a: every row, as a set. Word has no pages, but it has every cell.
        p2_expect_marker_set(txt, s$prefix, s$n1, label)
        # And every column heading of THIS table, by its own prefix.
        p2_expect_marker_set(txt, s$cprefix, s$n2, label,
                             what = "column heading")
        pos_last[k] <- regexpr(sprintf(paste0(s$prefix, "%02d"), s$n1), txt,
                               fixed = TRUE)[[1]]
        pos_note[k] <- regexpr(paste0("NOTE-MARKER-", k), txt,
                               fixed = TRUE)[[1]]
      }
      pos_end <- regexpr("ENDMARKER", txt, fixed = TRUE)[[1]]
      expect_true(pos_end > 0L, info = "the closing paragraph vanished")

      # R3b: adjacency, in the only currency Word has -- document order --
      # and BRACKETED on both sides. "After its own table" alone is satisfied
      # by a note stranded at the very end of the document, which is the
      # stranded-note defect passing the stranded-note check.
      for (k in seq_along(p2_shapes)) {
        expect_true(pos_note[k] > pos_last[k],
                    info = paste("note", k, "is not after its table"))
        right <- if (k < 4L) pos_last[k + 1L] else
          if (pos_end > pos_last[4L]) pos_end else NA_integer_
        if (!is.na(right))
          expect_true(pos_note[k] < right,
                      info = paste("note", k, "is stranded past its table"))
        # And nothing else came between a table's last row and its own note.
        others <- c(vapply(p2_shapes[-k], function(s)
          sprintf(paste0(s$prefix, "%02d"), s$n1), character(1)),
          paste0("NOTE-MARKER-", setdiff(seq_along(p2_shapes), k)), "ENDMARKER")
        between <- substr(txt, pos_last[k], pos_note[k])
        intruder <- others[vapply(others, function(m)
          grepl(m, between, fixed = TRUE), logical(1))]
        expect_length(intruder, 0L)
      }

      # R1: placement, the same ENDMARKER discrimination the PDF cells use.
      if (cell$placement == "end") {
        expect_true(all(pos_last > pos_end),
                    info = "a table did not move to the end of the manuscript")
      } else {
        expect_true(any(pos_last < pos_end),
                    info = "no table stayed in the text")
      }

      expect_false(grepl("Warning:", txt, fixed = TRUE))
    }

    # No apaquarto ERROR lines. quarto prints ERROR and still produces output,
    # so nothing else catches it. It is the ONLY thing the log is asked for --
    # a warning never appears there.
    expect_false(any(grepl("^\\s*ERROR", r$log)))
  })
}

# ---------------------------------------------------------------------------
# THE HOLE THE MATRIX ABOVE CANNOT SEE ON ITS OWN, closed here.
#
# A table that is LONG and inside a HAND-WRITTEN `::: {.landscape}` div, with
# tables collected at the end, and NO `#| apa-longtable` option.
#
# The absence of that option is the whole test. The Lua branch reads
#
#     apa-longtable == "true"  OR  haslongtable(float)
#
# so the option satisfies the FIRST disjunct and the detection -- the thing the
# removed interim guard used to stand in front of -- is never reached. Measured
# in Task 8: with the guard restored by hand AND the option present, this
# document renders 60 of 60 rows, unstarred, rotated, indistinguishable from
# correct. With the option gone and the guard restored it drops to 22 of 60,
# 38 rows missing FROM THE MIDDLE, the note absent from the PDF entirely, the
# page still rasterizing landscape, and exit status 0.
#
# The rotation here comes from the DIV, not from the ladder: this table is
# narrow, so R does not ask for a rotated page. That makes it the div path's
# own cell rather than a second copy of the matrix's wide-long one.
test_that("a long table in a landscape div, with no legacy option, keeps every row", {
  qmd <- paste0(
    p2_yaml("apaquarto-pdf", c("keep-tex: true", "latex-clean: false")),
    p2_setup,
    "## Results\n\nThis paragraph is the body of the manuscript.\n\n",
    "::: {.landscape}\n\n",
    "```{r}\n#| label: tbl-t1\n",
    "#| tbl-cap: \"A Narrow Table That Spans More Than One Page\"\n",
    "#| apa-note: \"NOTE-MARKER-1 is the note belonging to this table.\"\n",
    "#| echo: false\n#| message: false\n#| warning: true\n",
    "apa.2way.table(iv1 = a, iv2 = b, dv = y,\n",
    "               data = p2_data(50, 4, \"Ta\", \"Ha\")) |>\n",
    "  apa.quarto.pdf()\n",
    "```\n\n",
    ":::\n\n",
    "ENDMARKER closes the body.\n")
  expect_false(grepl("apa-longtable", qmd, fixed = TRUE))

  r <- p2_render("landscape-div-long", qmd, "pdf")
  expect_true(file.exists(r$out), info = paste(r$log, collapse = "\n"))
  skip_if_not(file.exists(r$out), "render produced no output to read")

  pages <- apa_pdf_pages(r$out)
  all_txt <- paste(pages, collapse = "\n")

  # Every row, as a set -- the failure this replaces took 38 out of the middle.
  p2_expect_marker_set(all_txt, "Ta", 50L, "landscape-div-long")

  # It spans, and its note is attached to it.
  occupied <- which(vapply(pages, function(p) grepl("Ta[0-9]{2}", p), logical(1)))
  last_page <- apa_page_of(pages, "Ta50")
  expect_true(length(occupied) > 1L, info = "the table did not span pages")
  # NA == NA is a PASS for expect_equal, so a note and a last row that BOTH
  # went missing would satisfy the line below without either ever reaching the
  # PDF. Pin each side to a real page first.
  expect_false(is.na(last_page))
  expect_false(is.na(apa_page_of(pages, "NOTE-MARKER-1")))
  expect_equal(apa_page_of(pages, "NOTE-MARKER-1"), last_page)

  # Rotated on every page it occupies, decided by rasterizing.
  shapes <- apa_pdf_page_shapes(r$out)
  expect_true(all(shapes[occupied] == "landscape"),
              info = paste("shapes:", paste(shapes, collapse = " ")))

  # By the mechanism that can hold a longtable: an UNSTARRED longtable, not in
  # a float, with the landscape wrapper surviving endfloat into the deferred
  # .ttt -- which is the trick, because endfloat re-emits the environment alone
  # and drops what surrounded it unless that wrapper is a declared float
  # flavour.
  ttt <- readLines(file.path(r$proj, "doc.ttt"), warn = FALSE)
  expect_true(any(grepl("\\begin{longtable}", ttt, fixed = TRUE)))
  expect_false(any(grepl("\\begin{longtable*}", ttt, fixed = TRUE)))
  expect_false(any(grepl("\\begin{table}", ttt, fixed = TRUE)))
  expect_false(any(grepl("sidewaystable", ttt, fixed = TRUE)))
  expect_equal(sum(trimws(ttt) == "\\begin{landscape}"), 1L)
  expect_equal(sum(trimws(ttt) == "\\end{landscape}"), 1L)

  expect_equal(p2_overfull(r$proj), 0L)
  expect_false(any(grepl("Warning:", pages, fixed = TRUE)))
  expect_false(any(grepl("^\\s*ERROR", r$log)))
})

# The escape hatch, still supported. A manuscript written against the previous
# release carries `#| apa-longtable: true`, and it must still render -- the
# option is now redundant (the emitter's environment is detected without it)
# but redundant is not the same as broken, and a user does not rewrite their
# paper because apaTables got better at guessing.
test_that("the legacy apa-longtable option still renders a correct table", {
  qmd <- paste0(
    p2_yaml("apaquarto-pdf", c("keep-tex: true", "latex-clean: false")),
    p2_setup,
    "## Results\n\nThis paragraph is the body of the manuscript.\n\n",
    "```{r}\n#| label: tbl-t1\n",
    "#| tbl-cap: \"A Narrow Table That Spans More Than One Page\"\n",
    "#| apa-note: \"NOTE-MARKER-1 is the note belonging to this table.\"\n",
    "#| apa-longtable: true\n",
    "#| echo: false\n#| message: false\n#| warning: true\n",
    "apa.2way.table(iv1 = a, iv2 = b, dv = y,\n",
    "               data = p2_data(50, 4, \"Ta\", \"Ha\")) |>\n",
    "  apa.quarto.pdf()\n",
    "```\n\n",
    "ENDMARKER closes the body.\n")

  r <- p2_render("legacy-longtable", qmd, "pdf")
  expect_true(file.exists(r$out), info = paste(r$log, collapse = "\n"))
  skip_if_not(file.exists(r$out), "render produced no output to read")

  pages <- apa_pdf_pages(r$out)
  p2_expect_marker_set(paste(pages, collapse = "\n"), "Ta", 50L, "legacy")
  last_page <- apa_page_of(pages, "Ta50")
  # Both sides pinned to a real page: NA == NA passes expect_equal, so a
  # vanished note and a vanished last row would agree with each other.
  expect_false(is.na(last_page))
  expect_false(is.na(apa_page_of(pages, "NOTE-MARKER-1")))
  expect_equal(apa_page_of(pages, "NOTE-MARKER-1"), last_page)
  expect_true(apa_page_of(pages, "Ta01") < last_page)
  # Still upright: the option asks for a long table, never for a rotated one.
  shapes <- apa_pdf_page_shapes(r$out)
  expect_true(all(shapes == "portrait"),
              info = paste(shapes, collapse = " "))
  expect_equal(p2_overfull(r$proj), 0L)
  # And no warning about the option being obsolete. A warning is not a
  # solution, and telling an author to edit a paper that renders correctly is
  # not a fix.
  expect_false(any(grepl("Warning:", pages, fixed = TRUE)))
  expect_false(any(grepl("^\\s*ERROR", r$log)))
})

# ---------------------------------------------------------------------------
# SPECIAL CHARACTERS IN VARIABLE NAMES, THROUGH A REAL RENDER.
#
# An underscore in a variable name broke a previous version of apaTables, and
# until Phase 2 the LaTeX path had no coverage of any of this: one unit
# assertion on apa_latex_escape("a_b") and nothing carrying a real builder spec
# through the emitter, let alone through a render.
#
# The names are the shared `tricky_names` (helper-special-names.R) -- all ten
# LaTeX specials, a non-ASCII letter, a single newline, two consecutive
# newlines and a tab -- split across three tables so that each stays inside the
# upright text block (measured 5.358, 5.228 and 5.003 in at 10 pt), which keeps
# this cell about escaping rather than about the width ladder.
#
# THE ASSERTION IS THAT THE NAME REACHES THE PAGE, not that the render
# succeeded, and the ranking of failure modes is why. `_` and `^` abort the
# render outright and `\`, `{`, `}` usually error -- those are the SAFE ones,
# because they are loud. `%` comments out the rest of the LaTeX line and `&`
# shifts every following cell into the wrong column, and both can complete with
# exit status 0 and nothing said. Only reading the name back out of the
# rendered PDF can see those two.
#
# The names are compared with runs of whitespace squashed, because that is what
# the emitter promises for the two newline cases: a single newline is harmless,
# two consecutive ones would make a blank line and a blank line inside a LaTeX
# cell aborts the render, so apa_latex_collapse_newlines() turns every one of
# them into a space. "line\nbreak" must arrive as "line break".
p2_special_groups <- split(unname(tricky_names),
                           rep(1:3, times = c(7L, 7L, 6L)))

# TeX's own typographic ligatures, undone before the comparison.
#
# THIS IS NOT A LOOPHOLE, AND IT WAS MEASURED RATHER THAN ASSUMED. A straight
# apostrophe typeset by any TeX engine comes out as U+2019, the right single
# quotation mark: a two-line document containing "it's" and nothing else, run
# through xelatex, gives "it" + U+2019 + "s" out of pdftotext. It is a property of TeX's
# ligature table, applies to every LaTeX document ever written, and no escaping
# decision of apaTables' is involved. Undoing it here keeps this cell about the
# characters that can LOSE content -- "%" swallowing the rest of a line, "&"
# shifting cells into the wrong column -- instead of failing on the one
# character TeX renders more prettily than it was typed.
#
# It IS a real deviation from verbatim display and it is recorded as one in the
# Task 11 report: in a PDF the name "it's" reads with U+2019 in place of the apostrophe, while the .docx
# path keeps the straight apostrophe. Nothing is lost and nothing shifts, so it
# is a typography question for the author, not a defect this gate blocks on.
# The mapping is deliberately narrow -- quotes and dashes only -- so it cannot
# absorb anything else.
p2_untypeset <- function(x) {
  for (m in list(c("\u2019", "'"), c("\u2018", "`"), c("\u201c", "\""),
                 c("\u201d", "\""), c("\u2013", "--"), c("\u2014", "---")))
    x <- gsub(m[1], m[2], x, fixed = TRUE)
  x
}

p2_special_qmd <- function(placement) {
  chunk <- function(k) paste0(
    "```{r}\n#| label: tbl-sc", k, "\n",
    "#| tbl-cap: \"Variable Names Carrying Special Characters, Part ", k, "\"\n",
    "#| apa-note: \"NOTE-MARKER-", k, " is the note belonging to this table.\"\n",
    "#| echo: false\n#| message: false\n#| warning: true\n",
    "sc_table(", paste(deparse(p2_special_groups[[k]]), collapse = ""),
    ")\n```\n\n")
  paste0(
    p2_yaml("apaquarto-pdf",
            c("keep-tex: true", "latex-clean: false",
              if (identical(placement, "intext")) "floatsintext: true")),
    "```{r}\n#| label: sc-setup\n#| echo: false\n#| message: false\n",
    "library(apaTables)\nset.seed(20260801)\n",
    # names<-() does not sanitise, so this is the check.names = FALSE case --
    # what read.csv(check.names = FALSE), haven and readxl hand a user.
    "sc_table <- function(nms) {\n",
    "  d <- as.data.frame(matrix(stats::rnorm(40 * length(nms)),\n",
    "                            ncol = length(nms)))\n",
    "  names(d) <- nms\n",
    "  apa.quarto.pdf(apa.cor.table(d))\n}\n```\n\n",
    "## Results\n\nXREF: @tbl-sc1, @tbl-sc2, @tbl-sc3.\n\n",
    paste0(vapply(1:3, chunk, character(1)), collapse = ""),
    "ENDMARKER closes the body.\n")
}

for (placement in c("end", "intext")) {
  test_that(paste0("every awkward variable name reaches the rendered page (",
                   placement, ")"), {
    r <- p2_render(paste0("special-", placement), p2_special_qmd(placement),
                   "pdf")
    expect_true(file.exists(r$out), info = paste(r$log, collapse = "\n"))
    skip_if_not(file.exists(r$out), "render produced no output to read")

    pages <- apa_pdf_pages(r$out)
    squashed <- p2_untypeset(gsub("[[:space:]]+", " ",
                                  paste(pages, collapse = "\n")))

    for (case_name in names(tricky_names)) {
      nm <- tricky_names[[case_name]]
      want <- gsub("[[:space:]]+", " ", nm)
      expect_true(grepl(want, squashed, fixed = TRUE),
                  info = paste0(placement, ": the name '", case_name,
                                "' did not reach the rendered page"))
    }

    # Nothing was lost or shifted around them either.
    expect_equal(apa_pdf_caption_numbers(pages), 1:3)
    expect_equal(apa_xref_numbers(pages), 1:3)
    for (k in 1:3)
      expect_false(is.na(apa_page_of(pages, paste0("NOTE-MARKER-", k))),
                   info = paste("note", k, "vanished"))
    expect_equal(p2_overfull(r$proj), 0L)
    expect_false(any(grepl("Warning:", pages, fixed = TRUE)))
    expect_false(any(grepl("^\\s*ERROR", r$log)))
  })
}

# ---------------------------------------------------------------------------
# Task 10 (Phase 1.6a): THE OTHER HOLE THE MATRIX ABOVE CANNOT SEE.
#
# Every docx cell of the matrix above sets BOTH `tbl-cap` and `apa-note` on
# every chunk (the `chunk()` closure inside `p2_qmd()`, a few hundred lines
# up) -- and so does every one of the 12 pre-existing `apaquarto-*.qmd`
# fixtures elsewhere in this suite (CLAUDE.md's "COVERAGE THE PLAN MUST NOT
# LEAN ON" note: 12 of 12 set `tbl-cap`, 11 of 12 set `apa-note`). So nothing
# in the suite has ever run THIS matrix's own numbering (R2) and note-
# adjacency (R3b) assertions -- the ones that specifically exist to catch a
# numbering counter defect or a stranded note -- against a docx table whose
# title and note are the AUTOMATIC ones Task 7 built (D-series), rather than
# typed chunk-option text. If a future change tied numbering or note
# placement to the PRESENCE of an explicit tbl-cap/apa-note (a plausible
# enough bug: the automatic path takes a different code branch through
# apatablemeta.lua than the explicit one), this matrix would not see it.
#
# The fix is the smallest cell that CAN see it, reusing p2_shapes/p2_setup/
# p2_yaml/p2_render/p2_expect_marker_set from the matrix above rather than
# duplicating them: the SAME four mixed shapes (so a numbering defect that
# only shows up when shapes differ is not hidden by a uniform document, the
# same reasoning the matrix's own banner gives), docx only (the automatic
# title/note mechanism is the markdown/docx path's own; LaTeX has its own
# already-covered mechanism), both placements (R1 -- both placements work --
# is binding regardless of which title/note source is in play).
#
# NOTE ADJACENCY HERE CANNOT USE "NOTE-MARKER-k" TEXT, unlike the matrix
# above -- there is no explicit apa-note to carry one. All four tables share
# the SAME generated note text (apa.2way.table()'s default has no
# significance stars or CI clause here, so it is a single "FigureNote"
# paragraph), so the only thing that can distinguish "table k's own note"
# from a sibling's is POSITION: read directly out of the raw document.xml
# (not the flattened text apa_docx_text() returns, which strips the
# `w:pStyle w:val="FigureNote"` style token along with every other tag) and
# counted between each table's own last row and the next boundary -- the
# same "exactly one, bracketed on both sides" technique
# test-docx-structure.R's rotated-note-position test (Task 7 fix round 1)
# uses for the same reason: a plain text search misses text split across
# run boundaries, but a style token never is.
p2_qmd_notitle <- function(placement) {
  opts <- c(if (identical(placement, "intext")) "floatsintext: true")
  chunk <- function(k) {
    s <- p2_shapes[[k]]
    paste0(
      "```{r}\n",
      "#| label: tbl-t", k, "\n",
      "#| echo: false\n#| message: false\n#| warning: true\n",
      "apa.2way.table(iv1 = a, iv2 = b, dv = y,\n",
      "               data = p2_data(", s$n1, ", ", s$n2, ", \"", s$prefix,
      "\", \"", s$cprefix, "\")) |>\n  apa.quarto.docx()\n",
      "```\n\n")
  }
  paste0(
    p2_yaml("apaquarto-docx", opts), p2_setup,
    "## Results\n\n",
    "This paragraph opens the body of the manuscript, above every table.\n\n",
    "XREF: @tbl-t1, @tbl-t2, @tbl-t3, @tbl-t4.\n\n",
    paste0(vapply(seq_along(p2_shapes), chunk, character(1)), collapse = ""),
    "ENDMARKER closes the body of this manuscript, after every table has been ",
    "declared.\n")
}

for (placement in c("end", "intext")) {
  label <- paste("docx-notitle", placement, sep = "-")

  test_that(paste("phase 2 acceptance, tbl-cap/apa-note ABSENT:", label), {
    qmd <- p2_qmd_notitle(placement)
    expect_false(grepl("tbl-cap", qmd, fixed = TRUE))
    expect_false(grepl("apa-note", qmd, fixed = TRUE))

    r <- p2_render(label, qmd, "docx")
    expect_true(file.exists(r$out), info = paste(r$log, collapse = "\n"))
    skip_if_not(file.exists(r$out), "render produced no output to read")

    styles <- apa_docx_styles(r$out)
    txt <- apa_docx_text(r$out)

    # R2: four tables, still with the full APA furniture -- now GENERATED --
    # numbered consecutively, and the body's references agreeing.
    expect_equal(sum(styles == "FigureTitle"), 4L)
    expect_equal(sum(styles == "FigureNote"), 4L)
    caps <- as.integer(unlist(regmatches(
      txt, gregexpr("(?m)^\\s*Table\\s+\\K[0-9]+", txt, perl = TRUE))))
    expect_equal(caps, 1:4)
    expect_equal(apa_xref_numbers(txt), 1:4)

    # R3a: every row and every column heading, as sets, exactly as the
    # matrix above checks them -- unaffected by where the title/note came
    # from.
    for (k in seq_along(p2_shapes)) {
      s <- p2_shapes[[k]]
      p2_expect_marker_set(txt, s$prefix, s$n1, label)
      p2_expect_marker_set(txt, s$cprefix, s$n2, label, what = "column heading")
    }

    # R3b: note adjacency, read from the RAW document.xml (see this
    # section's own banner for why) rather than from `txt`.
    doc <- apa_docx_document_xml(r$out)
    note_pos <- gregexpr("FigureNote", doc, fixed = TRUE)[[1]]
    expect_equal(length(note_pos[note_pos > 0L]), 4L,
                info = "expected one FigureNote paragraph per table")

    pos_last <- vapply(seq_along(p2_shapes), function(k) {
      s <- p2_shapes[[k]]
      regexpr(sprintf(paste0(s$prefix, "%02d"), s$n1), doc, fixed = TRUE)[[1]]
    }, numeric(1))
    expect_false(any(pos_last < 0L), info = "a table's own last row vanished")
    pos_end <- regexpr("ENDMARKER", doc, fixed = TRUE)[[1]]
    expect_true(pos_end > 0L, info = "the closing paragraph vanished")

    for (k in seq_along(p2_shapes)) {
      # Mirrors the matrix's own bracketing exactly: the right edge is the
      # NEXT table's last row, or -- for the final table -- ENDMARKER only
      # when placement keeps it AFTER the tables (intext); with placement
      # "end" the tables are deferred past ENDMARKER, so there is nothing to
      # bracket against and the upper bound is skipped, same as above.
      right <- if (k < length(p2_shapes)) pos_last[k + 1L] else
        if (pos_end > pos_last[length(p2_shapes)]) pos_end else NA_real_
      n_between <- if (is.na(right)) {
        sum(note_pos > pos_last[k])
      } else {
        sum(note_pos > pos_last[k] & note_pos < right)
      }
      expect_equal(n_between, 1L,
                  info = paste("table", k, "-- note count between its own",
                               "last row and the next boundary"))
    }

    # R1: placement, the same ENDMARKER discrimination the matrix above uses.
    if (identical(placement, "end")) {
      expect_true(all(pos_last > pos_end),
                  info = "a table did not move to the end of the manuscript")
    } else {
      expect_true(any(pos_last < pos_end),
                  info = "no table stayed in the text")
    }

    expect_false(grepl("Warning:", txt, fixed = TRUE))
    expect_false(any(grepl("^\\s*ERROR", r$log)))
  })
}
