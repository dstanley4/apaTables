# The acceptance matrix for the bundled apaquarto extension.
#
# Three requirements are binding, and this file is the only thing that
# decides whether they hold. Each is checked by reading values back out of a
# real rendered manuscript, never by inspecting the source:
#
#   R1  Both placements work -- tables floating in the text (floatsintext) AND
#       tables collected at the end (apaquarto's default, apa7 endfloat).
#       Neither may be sacrificed for the other.
#   R2  Table numbers are correct: consecutive from 1, and every @tbl- cross
#       reference resolves to the number its table actually carries.
#   R3  A table taller than the page continues onto the next page, keeps its
#       last row, and keeps its note attached to ITSELF.
#
# Two design rules follow from how the defects behave, and neither may be
# relaxed to make a cell go green:
#
#   * Adjacency, not presence. With the note-placement bug every note marker
#     still EXISTS in the document -- it is merely stranded pages away from its
#     table -- so any(grepl(marker, pages)) passes against the exact defect
#     this file exists to catch. Notes are therefore checked by the PAGE they
#     land on.
#   * Per-table markers. One shared marker would let a single surviving table
#     vouch for its broken siblings.
#
# The document shape (fits / spans / fits / fits) is likewise not arbitrary: it
# is the shortest shape stock apaquarto gets WRONG. See the comment on
# apa_acceptance_qmd() in helper-apaquarto-extract.R.
#
# Every render here is a real quarto render taking tens of seconds, so the file
# skips unless the whole toolchain is present. It must never fail a CRAN check.

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto not installed")
skip_if_not(nzchar(Sys.which("pdftotext")), "poppler (pdftotext) not installed")
skip_if_not(nzchar(Sys.which("pdfinfo")), "poppler (pdfinfo) not installed")
# Rotation is decided by rasterizing, because every cheaper way of asking a PDF
# whether a page is rotated has been measured lying here.
skip_if_not(nzchar(Sys.which("pdftoppm")), "poppler (pdftoppm) not installed")

# The spawned quarto R session runs library(apaTables) against the INSTALLED
# package, not this source tree. A green matrix rendered against a stale
# library certifies the library, not the branch -- so this is checked rather
# than asserted in a comment.
#
# The guard is in helper-stale-library.R. It compares BOTH halves of the
# installed package against this tree: the bundled apaquarto extension, byte
# for byte, and the R namespace, deparsed. The R half is why an R/ edit that
# leaves inst/ untouched can no longer slip through -- it used to, and the
# rule "reinstall from a built tarball" was all that stood in its way.
apa_skip_if_library_stale()

# placement x length x orientation x format. Length is a dimension WITHIN each
# document -- every cell contains a table that spans pages and three that do
# not -- because the numbering defect only appears when shapes are mixed.
#
# Landscape is asserted for PDF only: Quarto's own landscape in .docx is a
# known upstream defect (it emits A4 pages inside a Letter manuscript with a
# bare sectPr that strips the running head, page numbers and double spacing),
# and the bundled replacement for it is checked in test-docx-structure.R.
cells <- expand.grid(
  placement   = c("end", "intext"),
  orientation = c("portrait", "landscape"),
  format      = c("pdf", "docx"),
  stringsAsFactors = FALSE)
cells <- cells[!(cells$orientation == "landscape" & cells$format == "docx"), ]

for (i in seq_len(nrow(cells))) {
  cell <- cells[i, ]
  label <- paste(cell$format, cell$placement, cell$orientation, sep = "-")

  test_that(paste("acceptance:", label), {
    proj <- file.path(tempdir(), paste0("acc-", label))
    unlink(proj, recursive = TRUE)
    suppressMessages(apa.quarto.new(proj, filename = "doc.qmd"))
    writeLines(apa_acceptance_qmd(cell$placement, cell$orientation,
                                  cell$format),
               file.path(proj, "doc.qmd"))

    log <- suppressWarnings(system2("quarto",
                                    c("render", shQuote(file.path(proj, "doc.qmd"))),
                                    stdout = TRUE, stderr = TRUE))
    out <- file.path(proj, paste0("doc.", cell$format))
    expect_true(file.exists(out), info = paste(log, collapse = "\n"))
    skip_if_not(file.exists(out), "render produced no output to read")

    if (cell$format == "pdf") {
      pages <- apa_pdf_pages(out)

      # R2a: captions number consecutively from 1, in document order.
      expect_equal(apa_pdf_caption_numbers(pages), 1:4)

      # R2b: the body's cross-references resolve to the SAME numbers. The
      # counter bug corrupts captions and references together, so checking
      # captions alone would miss a document where both read "Table 1".
      expect_equal(apa_xref_numbers(pages), 1:4)

      # R3a: every table's last row survived. Markers are per-table so a
      # single surviving row cannot vouch for the others.
      last_page <- vapply(1:4, function(k)
        apa_page_of(pages, paste0("LASTROW-", k)), integer(1))
      expect_false(any(is.na(last_page)),
                   info = "a table lost its last row")

      # R3b: each note is on the same page as the END of its own table --
      # not merely present somewhere in the document. This is the whole
      # point: with the note-placement bug every marker still EXISTS, it is
      # just stranded pages away from its table, so an any()/%in% check
      # would pass against the very defect this matrix exists to catch.
      note_page <- vapply(1:4, function(k)
        apa_page_of(pages, paste0("NOTE-MARKER-", k)), integer(1))
      expect_false(any(is.na(note_page)), info = "a note vanished")
      expect_equal(note_page, last_page)

      # R3c: the table that is supposed to span pages actually spans. Its
      # caption must land on an EARLIER page than its last row. Without this a
      # table that silently collapsed onto one page would still carry its
      # markers, its note and its number, and R3 would be certified by a table
      # that never broke.
      span_cap <- apa_page_of(pages, "A Table That Spans More Than One Page")
      expect_false(is.na(span_cap), info = "the long table's caption vanished")
      expect_true(span_cap < last_page[2],
                  info = paste("the long table did not span pages: caption on",
                               span_cap, "last row on", last_page[2]))

      # R1: placement, decided against the body paragraph that comes AFTER
      # every table in the source.
      #
      # A marker BEFORE the tables cannot decide it. Under endfloat the first
      # deferred table lands on the page immediately after the body, so "some
      # table is within a page of the body" is true of a document with ZERO
      # tables in the text -- the assertion passes against the very defect it
      # exists to catch (measured: stripping floatsintext from the intext cell
      # left it green). Against ENDMARKER the two placements are opposites.
      #
      # The two directions are deliberately NOT symmetric, and the asymmetry is
      # the honest reading of each requirement rather than an oversight.
      # "Collected at the end" is a claim about EVERY table: one table left
      # behind in the text is a failure of it, so all(). "In the text" is not
      # the mirror image -- LaTeX may legitimately push a float that will not
      # fit onto a later page, and a document whose last table is carried past
      # the closing paragraph has still placed its tables in the text -- so
      # any(). What any() must not become is a check that endfloat also
      # satisfies, and it is not: with floats delayed, every table follows the
      # whole body, so no table can precede ENDMARKER.
      end_page <- apa_page_of(pages, "ENDMARKER")
      expect_false(is.na(end_page), info = "the closing body paragraph vanished")
      if (cell$placement == "end") {
        expect_true(all(last_page > end_page),
                    info = "a table did not move to the end of the manuscript")
      } else {
        expect_true(any(last_page < end_page),
                    info = "no table stayed in the text")
      }

      # Rotation, decided by RASTERIZING the pages.
      #
      # One mechanism now serves both placements -- pdflscape, a literal
      # landscape block, which endfloat defers whole when tables are collected
      # at the end -- so one assertion serves both, and it is made against the
      # page a reader would look at rather than against an intermediate.
      # Neither of the cheap textual checks is trusted: `strings | grep
      # /Rotate` returned 0 on a genuinely rotated document, and pdfinfo's
      # per-page report was no better.
      #
      # Not merely "a page was rotated": the pages that matter are the ones
      # each table actually ENDS on, so a table that silently stopped rotating
      # while its siblings carried on is a failure. Both directions are
      # asserted, so a portrait cell that started rotating is one too.
      #
      # The .tex is read only for the MECHANISM, and only to pin the one that
      # was replaced: sidewaystable rotates the table on an upright page,
      # which is apa7's convention rather than APA's instruction, and it is a
      # float, so it silently loses the rows of any table that spans pages.
      tex <- paste(readLines(file.path(proj, "doc.tex"), warn = FALSE),
                   collapse = "\n")
      shapes <- apa_pdf_page_shapes(out)
      expect_length(shapes, length(pages))
      expect_false(grepl("sidewaystable", tex, fixed = TRUE))
      if (cell$orientation == "landscape") {
        expect_true(all(shapes[last_page] == "landscape"),
                    info = paste("a table did not land on a rotated page:",
                                 paste(shapes, collapse = " ")))
      } else {
        expect_true(all(shapes == "portrait"),
                    info = paste(shapes, collapse = " "))
      }

      # No warning reached the manuscript. knitr writes an R warning INTO the
      # rendered document, not onto quarto's console, so a log-only check is
      # one of those assertions that cannot fail; the reader of the finished
      # PDF is who actually meets the warning, so that is where it is looked
      # for.
      expect_false(any(grepl("Warning:", pages, fixed = TRUE)),
                   info = paste(grep("Warning:", unlist(strsplit(pages, "\n")),
                                     value = TRUE), collapse = " | "))
    } else {
      styles <- apa_docx_styles(out)
      txt <- apa_docx_text(out)
      # R2a: four tables, each with the full APA furniture.
      expect_equal(sum(styles == "FigureTitle"), 4L)
      expect_equal(sum(styles == "FigureNote"), 4L)
      # R2a: captions number consecutively, in order.
      caps <- as.integer(unlist(regmatches(
        txt, gregexpr("(?m)^\\s*Table\\s+\\K[0-9]+", txt, perl = TRUE))))
      expect_equal(caps, 1:4)
      # R2b: the body's cross-references resolve to the same numbers.
      expect_equal(apa_xref_numbers(txt), 1:4)
      # R3a: every table kept its last row.
      for (k in 1:4)
        expect_true(grepl(paste0("LASTROW-", k), txt, fixed = TRUE),
                    info = paste("table", k, "lost its last row"))
      # R3b: each note is present AND ordered after its own table's last
      # row and BEFORE the next table's last row. Word has no pages to
      # compare, so document order is the available adjacency check -- but
      # "after its own table" alone is not adjacency: a note stranded at the
      # very end of the document satisfies it, which is the stranded-note
      # defect passing the stranded-note check. Bracketing it on both sides
      # is what makes it an adjacency assertion.
      pos_last <- vapply(1:4, function(k)
        regexpr(paste0("LASTROW-", k), txt, fixed = TRUE)[[1]], integer(1))
      pos_note <- vapply(1:4, function(k)
        regexpr(paste0("NOTE-MARKER-", k), txt, fixed = TRUE)[[1]], integer(1))
      pos_end <- regexpr("ENDMARKER", txt, fixed = TRUE)[[1]]
      expect_true(pos_end > 0L, info = "the closing body paragraph vanished")
      for (k in 1:4) {
        expect_true(pos_note[k] > pos_last[k],
                    info = paste("note", k, "is not after its table"))
        # A right bracket for every note, including the last one. The next
        # table's last row serves for the first three; for the fourth there is
        # no next table, and in the intext document ENDMARKER follows every
        # table, so it serves there. In the end document nothing follows the
        # tables at all, which is why the marker sweep below is not optional:
        # it is the only right-hand bound the fourth note has.
        right <- if (k < 4L) pos_last[k + 1L] else
          if (pos_end > pos_last[4L]) pos_end else NA_integer_
        if (!is.na(right))
          expect_true(pos_note[k] < right,
                      info = paste("note", k, "is stranded past its table"))
        # NOTHING else comes between a table's last row and its own note.
        # Ordering alone is not adjacency in a format with no pages: a note
        # that jumped over other document furniture can still be "after its
        # table and before the next one".
        others <- c(paste0("LASTROW-", setdiff(1:4, k)),
                    paste0("NOTE-MARKER-", setdiff(1:4, k)), "ENDMARKER")
        between <- substr(txt, pos_last[k], pos_note[k])
        intruder <- others[vapply(others, function(m)
          grepl(m, between, fixed = TRUE), logical(1))]
        expect_length(intruder, 0L)
      }

      # R1: placement, the same ENDMARKER discrimination the PDF cells use.
      # Word has no pages, but it has document order, and that is enough:
      # measured, the end document puts ENDMARKER before every table and the
      # intext document puts it after them. Leaving this out left half of a
      # binding requirement uncertified in the format most manuscripts are
      # actually submitted in.
      if (cell$placement == "end") {
        expect_true(all(pos_last > pos_end),
                    info = "a table did not move to the end of the manuscript")
      } else {
        expect_true(any(pos_last < pos_end),
                    info = "no table stayed in the text")
      }

      expect_false(grepl("Warning:", txt, fixed = TRUE))
    }

    # No apaquarto ERROR lines. This is a genuine check -- quarto prints ERROR
    # and still produces a PDF, so nothing else catches it -- and it is the
    # ONLY thing the log is asked for. It is deliberately not accompanied by a
    # warning check: knitr writes an R warning into the DOCUMENT, never onto
    # quarto's console, so grepl("Warning message", log) is constant-FALSE and
    # was removed rather than left standing as advertised coverage that does
    # not exist. Warnings are checked in the artefact, above.
    expect_false(any(grepl("^\\s*ERROR", apa_strip_ansi(log))))
  })
}

# The combination that had no test anywhere: a table that is BOTH long AND
# inside a landscape div, with tables collected at the end of the manuscript.
#
# This file used to pin the OPPOSITE -- that such a table is not rotated --
# because while rotation meant apa7's sidewaystable it could not be. That is
# no longer a limitation to document; it is a defect that has been fixed, so
# the test says what the fix delivers.
#
# It is also the case whose absence made the two halves of the fix separable,
# and dangerously so. The acceptance matrix above rotates its long table only
# now that this works; before, it deliberately kept the long table OUT of the
# landscape div. So while the interim guard held rotation together with the
# float wrapper, NOTHING in the suite rendered a rotated longtable, and
# removing that guard on its own would have sealed one inside \begin{table}
# and lost rows in silence. Measured on this very document, with the guard
# still in place: 51 of 60 rows, a starred longtable*, an upright page, and a
# note that never reached the PDF -- exit 0, no error, no warning.
test_that("a table that spans pages IS rotated when floats go to the end", {
  proj <- file.path(tempdir(), "acc-landscape-longtable")
  unlink(proj, recursive = TRUE)
  suppressMessages(apa.quarto.new(proj, filename = "doc.qmd"))
  writeLines(apa_landscape_longtable_qmd(), file.path(proj, "doc.qmd"))

  log <- suppressWarnings(system2("quarto",
                                  c("render", shQuote(file.path(proj, "doc.qmd"))),
                                  stdout = TRUE, stderr = TRUE))
  out <- file.path(proj, "doc.pdf")
  expect_true(file.exists(out), info = paste(log, collapse = "\n"))
  skip_if_not(file.exists(out), "render produced no output to read")

  pages <- apa_pdf_pages(out)

  # It still spans, it still keeps its last row, and its note is still
  # attached to it -- the three things rotation used to cost.
  last_page <- apa_page_of(pages, "LASTROW-1")
  expect_false(is.na(last_page))
  # And the note's own side: NA == NA passes expect_equal, so without this a
  # note that never reached the PDF would agree with a last row that did not
  # either.
  expect_false(is.na(apa_page_of(pages, "NOTE-MARKER-1")))
  expect_equal(apa_page_of(pages, "NOTE-MARKER-1"), last_page)
  first_page <- apa_page_of(pages, "A Table That Spans More Than One Page")
  expect_true(first_page < last_page)

  # EVERY row, compared as a SET. A count is not enough: the failure this
  # replaces dropped nine rows out of the middle of the table.
  expected <- c(sprintf("Group%02d", seq_len(59L)), "LASTROW-1")
  found <- unique(unlist(regmatches(
    pages, gregexpr("Group[0-9]{2}|LASTROW-1", pages))))
  expect_equal(sort(setdiff(expected, found)), character(0))

  # Rotated, on every page the table occupies, decided by rasterizing.
  #
  # The NA guard is not defensive tidiness: when this test was watched failing
  # against the restored guard, the table lost its last row entirely, so
  # last_page was NA and `first_page:last_page` raised an ERROR that stopped
  # the remaining assertions from ever running. A test that aborts reports
  # less than a test that fails.
  shapes <- apa_pdf_page_shapes(out)
  span <- if (is.na(first_page) || is.na(last_page)) integer(0) else
    seq.int(first_page, last_page)
  expect_true(length(span) > 0L && all(shapes[span] == "landscape"),
              info = paste("shapes:", paste(shapes, collapse = " ")))

  # And by the mechanism that can hold a longtable. The environment is the
  # unstarred \begin{longtable}, it is NOT inside a float, and the landscape
  # wrapper survived endfloat into the deferred .ttt -- which is the whole
  # trick: endfloat re-emits the environment alone and drops what surrounded
  # it, unless the surrounding environment is itself a declared float flavour.
  ttt <- readLines(file.path(proj, "doc.ttt"), warn = FALSE)
  expect_true(any(grepl("\\begin{longtable}", ttt, fixed = TRUE)))
  expect_false(any(grepl("\\begin{longtable*}", ttt, fixed = TRUE)))
  expect_false(any(grepl("\\begin{table}", ttt, fixed = TRUE)))
  expect_false(any(grepl("sidewaystable", ttt, fixed = TRUE)))
  expect_equal(sum(trimws(ttt) == "\\begin{landscape}"), 1L)
  expect_equal(sum(trimws(ttt) == "\\end{landscape}"), 1L)

  # Nothing was said about it. The old behaviour came with a warning typed
  # into the manuscript; the fix must not leave the warning behind.
  expect_false(any(grepl("Warning:", pages, fixed = TRUE)),
               info = paste(grep("Warning:", unlist(strsplit(pages, "\n")),
                                 value = TRUE), collapse = " | "))
  expect_false(any(grepl("^\\s*ERROR", apa_strip_ansi(log))))
})

# What a CLOBBERED project actually produces, end to end.
#
# This is the seam the six cells cannot see: apa.quarto.new() installs a
# correct extension, and none of the matrix documents call apa.quarto.setup(),
# so the matrix never renders a project that needs repairing. The existing
# mid-render-repair test renders format: html, which involves neither LaTeX nor
# the longtable filters, and asserts only where the receipt went.
#
# The behaviour being pinned is not the behaviour anyone assumed. The four
# patches are consumed at two different times:
#
#   apatemplate.tex   read by xelatex AFTER the R chunks run -> a mid-render
#                     repair DOES take effect, so the numbers come out right
#   the Lua filters   loaded by pandoc BEFORE any R runs -> a mid-render repair
#                     CANNOT take effect, so the long table's note is stranded
#
# So the first render of a clobbered project is half correct, and the assertion
# below says so rather than pretending otherwise. The second render, with the
# repair now on disk before Quarto starts, is correct throughout.
#
# If Quarto ever changes when it loads filters, this test fails loudly -- which
# is the point. It is also what keeps apa_midrender_clause() honest: the day
# the clause stops being necessary, the first render's note assertion breaks
# and someone has to come and look.
test_that("acceptance: a clobbered project repairs itself, and says so", {
  proj <- file.path(tempdir(), "acc-clobbered")
  unlink(proj, recursive = TRUE)
  suppressMessages(apa.quarto.new(proj, filename = "doc.qmd"))
  writeLines(apa_acceptance_qmd("end", "portrait", "pdf", self_repair = TRUE),
             file.path(proj, "doc.qmd"))
  apa_unpatch_extension(file.path(proj, "_extensions", "wjschne", "apaquarto"))

  render <- function() {
    log <- suppressWarnings(system2(
      "quarto", c("render", shQuote(file.path(proj, "doc.qmd"))),
      stdout = TRUE, stderr = TRUE))
    list(log = apa_strip_ansi(log), pages = apa_pdf_pages(file.path(proj, "doc.pdf")))
  }
  pages_of <- function(pages, prefix)
    vapply(1:4, function(k) apa_page_of(pages, paste0(prefix, k)), integer(1))

  # ---- first render: the project is stock when Quarto starts ----
  first <- render()
  expect_true(file.exists(file.path(proj, "doc.pdf")),
              info = paste(first$log, collapse = "\n"))
  skip_if_not(file.exists(file.path(proj, "doc.pdf")), "no output to read")

  # The repair happened, and it was reported.
  expect_true(any(grepl("replaced the apaquarto extension in this project",
                        first$log, fixed = TRUE)),
              info = "the clobbered extension was not repaired")
  # ...including the part that says this render is stale. Without this clause
  # a user gets exit 0, a receipt saying we fixed it, correct table numbers and
  # a note four pages from its table, with nothing said.
  expect_true(any(grepl("rendered once more before the output is used",
                        first$log, fixed = TRUE)),
              info = "the repair receipt did not say this render is stale")

  # The LaTeX half of the repair DID reach this render: the template is read
  # after the chunks run, so numbering is correct even on the first pass.
  expect_equal(apa_pdf_caption_numbers(first$pages), 1:4)
  expect_equal(apa_xref_numbers(first$pages), 1:4)

  # The Lua half did NOT: the note filter was loaded before any R ran, so the
  # long table's note is stranded away from its table. Asserted as a fact about
  # this render, not as something acceptable.
  first_last <- pages_of(first$pages, "LASTROW-")
  first_note <- pages_of(first$pages, "NOTE-MARKER-")
  expect_false(any(is.na(first_last)))
  expect_false(any(is.na(first_note)))
  expect_true(first_note[2] != first_last[2],
              info = "the long table's note reached its table on the FIRST render -- Quarto now loads filters after the R chunks, or the note no longer needs the filter; re-examine apa_midrender_clause(), which tells users this cannot happen")

  # ---- second render: the repair is on disk before Quarto starts ----
  second <- render()
  # Nothing left to repair, so nothing is said at all.
  expect_false(any(grepl("apaquarto extension", second$log, fixed = TRUE)))
  expect_false(any(grepl("rendered once more", second$log, fixed = TRUE)))

  expect_equal(apa_pdf_caption_numbers(second$pages), 1:4)
  expect_equal(apa_xref_numbers(second$pages), 1:4)
  second_last <- pages_of(second$pages, "LASTROW-")
  second_note <- pages_of(second$pages, "NOTE-MARKER-")
  expect_false(any(is.na(second_last)))
  expect_equal(second_note, second_last)
  expect_false(any(grepl("^\\s*ERROR", second$log)))
})
