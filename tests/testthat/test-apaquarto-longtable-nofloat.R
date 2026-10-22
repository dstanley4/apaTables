# A longtable must never be wrapped in a LaTeX float -- and nothing in the
# chunk should have to say so.
#
# The bundled apaquarto used to skip the float wrapper only when the user
# wrote `#| apa-longtable: true`. Forgetting it did not produce an error or a
# warning: LaTeX cannot page-break a longtable inside \begin{table}, so the
# rows past the bottom of the page were dropped from a document that compiled
# cleanly. Measured on the very document below, rendered against the extension
# as it stood before this behaviour changed:
#
#   floats at the end   55 of 60 rows, note absent from the PDF entirely
#   floats in the text  55 of 60 rows, note absent from the PDF entirely
#
# so this file's assertions are ones that HAVE been watched to fail, not ones
# assumed to be able to.
#
# Five things are checked, because four of them can hold while the fifth
# silently does not:
#
#   1. the environment is the UNSTARRED \begin{longtable}. Quarto's own
#      main.lua rewrites a captionless raw longtable to longtable*, and the
#      starred name is then invisible to every later longtable fixup, Quarto's
#      and apaquarto's alike -- measured cost, 45 of 60 rows and a stranded
#      note, with no error.
#   2. the \caption is spliced INSIDE the environment. That is what keeps the
#      name unstarred, so it is not a cosmetic check.
#   3. the note is inside the table's own \endlastfoot, so LaTeX carries it
#      wherever the table goes.
#   4. every row is present, compared as a SET against the expected set -- a
#      hand-rolled tally once reported 37 of 40 and read as data loss that was
#      not there.
#   5. the table number is right.
#
# Both placements are rendered, because they take different routes: with
# floats collected at the end apa7's endfloat moves the environment into a
# .ttt file, dropping whatever surrounded it.

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto not installed")
skip_if_not(nzchar(Sys.which("pdftotext")), "poppler (pdftotext) not installed")
skip_if_not(nzchar(Sys.which("pdfinfo")), "poppler (pdfinfo) not installed")

# quarto spawns its own R session and calls library(apaTables), so a render
# exercises the INSTALLED package -- its R code AND the extension it ships. A
# green result against a stale library certifies the library, not this branch.
# The guard is in helper-stale-library.R.
apa_skip_if_library_stale()

# Row markers are letters only. An underscore in a cell is math mode and
# ABORTS the render, and a digit would collide with the value column.
apa_nofloat_marker <- function(i) {
  paste0("LTROW", chartr("0123456789", "abcdefghij", sprintf("%02d", i)))
}

APA_NOFLOAT_ROWS <- 60L

# The chunk carries NO apa-longtable option. That absence is the whole point:
# it is what the old extension needed and what the new one works out for
# itself.
apa_nofloat_qmd <- function(placement) {
  paste0(
    "---\n",
    "title: \"Longtable Without A Float\"\n",
    "shorttitle: \"Longtable Without A Float\"\n",
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
    "## Results\n\n",
    "XREF: @tbl-tall\n\n",
    "```{r}\n",
    "#| label: tbl-tall\n",
    "#| tbl-cap: \"A Table Taller Than The Page\"\n",
    "#| apa-note: [\"LTNOTEa is the note sentence.\"]\n",
    "#| echo: false\n",
    "#| message: false\n",
    "library(apaTables)\n",
    "mk <- function(i) paste0(\"LTROW\",\n",
    "  chartr(\"0123456789\", \"abcdefghij\", sprintf(\"%02d\", i)))\n",
    "n <- ", APA_NOFLOAT_ROWS, "\n",
    "sp <- apaTables:::apa_spec_start(n)\n",
    "sp <- apaTables:::apa_col(sp, \"Marker\", \"l\",\n",
    "                          vapply(seq_len(n), mk, character(1)))\n",
    "sp <- apaTables:::apa_col(sp, \"Value\", \"r\", sprintf(\"%d\", seq_len(n)))\n",
    "sp <- apaTables:::apa_spec_finish(sp, title = \"A Table Taller Than The Page\")\n",
    "knitr::asis_output(\n",
    "  apaTables:::apa_latex_table(apaTables:::apa_display_plan(sp), 10))\n",
    "```\n\n",
    "## Discussion\n\n",
    "ENDMARKER closes the body.\n")
}

for (placement in c("end", "intext")) {

  test_that(paste0("a longtable is emitted without a float wrapper, and no ",
                   "chunk option asks for it (", placement, ")"), {
    proj <- file.path(tempdir(), paste0("nofloat-", placement))
    unlink(proj, recursive = TRUE)
    suppressMessages(apa.quarto.new(proj, filename = "doc.qmd"))
    writeLines(apa_nofloat_qmd(placement), file.path(proj, "doc.qmd"))

    log <- suppressWarnings(system2(
      "quarto", c("render", shQuote(file.path(proj, "doc.qmd"))),
      stdout = TRUE, stderr = TRUE))
    out <- file.path(proj, "doc.pdf")
    expect_true(file.exists(out), info = paste(log, collapse = "\n"))
    skip_if_not(file.exists(out), "render produced no output to read")

    # Under endfloat the deferred copy of the environment is written to
    # doc.ttt, and it is a COPY -- endfloat re-emits the environment alone and
    # drops what surrounded it, which is how a font size written in front of
    # \begin{longtable} goes missing from a table collected at the end. Each
    # file that exists is therefore checked in its own right, rather than
    # concatenated: concatenating hides a defect that is present in only one
    # of them, and makes every "exactly one" count read two.
    # Where floats are collected at the end, doc.ttt MUST be there: endfloat
    # writes the deferred environment into it, and every end-placement
    # assertion below reads it. Without this the file list silently narrows to
    # doc.tex alone and the end placement is certified by the body .tex, which
    # under endfloat holds no table at all.
    if (identical(placement, "end")) {
      expect_true(file.exists(file.path(proj, "doc.ttt")),
                  info = "endfloat wrote no doc.ttt")
    }
    tex_files <- file.path(proj, c("doc.tex", "doc.ttt"))
    tex_files <- tex_files[file.exists(tex_files)]
    expect_true(length(tex_files) > 0L)

    for (tf in tex_files) {
      tex <- readLines(tf, warn = FALSE)
      where <- basename(tf)

      ib <- grep("\\begin{longtable}", tex, fixed = TRUE)
      ie <- grep("\\end{longtable}", tex, fixed = TRUE)

      # 1. Unstarred, and not inside a float. Both halves matter: the starred
      #    name compiles and loses rows, and \begin{table} is what makes the
      #    longtable unbreakable in the first place.
      expect_length(ib, 1L)
      expect_length(ie, 1L)
      expect_false(any(grepl("\\begin{longtable*}", tex, fixed = TRUE)),
                   info = paste(where, "-- the environment came out starred"))
      expect_false(any(grepl("\\begin{table}", tex, fixed = TRUE)),
                   info = paste(where, "-- still wrapped in a float"))

      # 2. The caption is INSIDE the environment, carrying the label, so the
      #    cross-reference resolves and the name stays unstarred.
      icap <- grep("\\caption{\\label{tbl-tall}", tex, fixed = TRUE)
      expect_length(icap, 1L)
      expect_true(length(icap) == 1L && length(ib) == 1L && length(ie) == 1L &&
                    icap > ib[1] && icap < ie[1],
                  info = paste(where,
                               "-- caption is not inside the environment"))

      # 3. The note is inside the table's own last foot, not a sibling
      #    paragraph left behind wherever the chunk was written.
      inote <- grep("LTNOTEa", tex, fixed = TRUE)
      # In the document body only: the bundled template patches the last
      # foot itself, so the preamble names it too.
      body <- grep("\\begin{document}", tex, fixed = TRUE)
      ifoot <- grep("\\endlastfoot", tex, fixed = TRUE)
      ifoot <- ifoot[ifoot > if (length(body)) body[1] else 0L]
      expect_length(inote, 1L)
      expect_length(ifoot, 1L)
      expect_true(length(inote) == 1L && length(ifoot) == 1L &&
                    length(ib) == 1L && inote > ib[1] && inote < ifoot[1],
                  info = paste(where, "-- note is not inside \\endlastfoot"))
    }

    pages <- apa_pdf_pages(out)

    # 4. Every row reached the PDF. Compared as a SET, never as a count.
    expected <- vapply(seq_len(APA_NOFLOAT_ROWS), apa_nofloat_marker,
                       character(1))
    found <- unique(unlist(regmatches(
      pages, gregexpr("LTROW[a-j]{2}", pages))))
    expect_equal(sort(setdiff(expected, found)), character(0))
    expect_equal(sort(setdiff(found, expected)), character(0))

    # The note has to be ON a page of the table, not merely in the document:
    # with the float wrapper in place the note existed in the .tex and never
    # reached the PDF at all, and in other failure modes it strands pages
    # away from its own table.
    note_page <- apa_page_of(pages, "LTNOTEa")
    expect_false(is.na(note_page), info = "the note never reached the PDF")
    row_pages <- which(vapply(pages, function(p) grepl("LTROW", p),
                              logical(1)))
    expect_true(!is.na(note_page) &&
                  note_page >= min(row_pages) && note_page <= max(row_pages) + 1L,
                info = "the note is stranded away from its table")

    # 5. The number is right, in the caption and in the body's reference.
    expect_equal(apa_pdf_caption_numbers(pages), 1L)
    expect_equal(apa_xref_numbers(pages), 1L)
  })
}
