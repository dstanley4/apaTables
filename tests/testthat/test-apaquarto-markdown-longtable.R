# Tables in an apaquarto PDF: a long table written in markdown (or by
# knitr::kable() in a captioned chunk) keeps every row, its note travels with
# it, every table closes with ONE rule, rows are single-spaced and notes keep
# the manuscript's spacing.
#
# Pandoc 3.8+ (Quarto 1.9) wraps a captioned markdown table in \begin{table},
# and a longtable inside a float cannot break across pages: a 90-row table
# printed rows 0 to 45 and stopped at the foot of its page, with no error and
# no warning (stock apaquarto too). The bundle lifts a float holding exactly
# one table out of its float. A markdown table stays a pandoc Table -- the
# writer must see its cells, or a highlighted code span or a language span in
# one leaves an undefined command and no PDF -- and carries an invisible
# raw-LaTeX marker in its first cell that arms the template's closing-rule
# flag and, when it has a note, stores the note for the template to print as
# the table's last foot row. A table Quarto has already written as raw LaTeX
# (kable) is armed the way apaTables' own tables are.
#
# Every rule count below is read from the PAGE IMAGE, not from the .tex: a
# check on the arming text passes with no rule at all and with two.
#
# Mutations, run by hand and recorded rather than automated (2026-10-21):
# the extension before this fix (5.0.18+apaTables.27, the one previously
# released here) -- rows 46 of 90, the note missing, two rules under the
# uncaptioned and kable tables, apaTables row pitch 1.99 times the type size,
# the kable caption printed on every page; writing the tables out as LaTeX
# in the filter (with pandoc.write), which was tried first -- the code/language document renders no PDF; the
# template's \endlastfoot hook removed -- the markdown note never printed;
# arming removed -- two rules; the below-rule gap removed -- note gap 3.9 pt
# against 6.6 pt; the single-spacing hook removed -- every row-pitch check.

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto not installed")
skip_if_not(nzchar(Sys.which("pdftotext")), "poppler (pdftotext) not installed")
skip_if_not(nzchar(Sys.which("pdfinfo")), "poppler (pdfinfo) not installed")
skip_if_not(nzchar(Sys.which("pdftoppm")), "poppler (pdftoppm) not installed")
apa_skip_if_library_stale()

# Every word on a page with its box, in PDF points from the top-left corner.
# pdftotext's box height for a word is the type size, which lets a line pitch
# be judged against the size it is set in.
apa_pdf_words <- function(pdf, page) {
  x <- paste(system2("pdftotext", c("-bbox", "-f", page, "-l", page,
                                    shQuote(pdf), "-"), stdout = TRUE),
             collapse = "\n")
  m <- regmatches(x, gregexpr(paste0(
    '<word xMin="[-0-9.]+" yMin="[-0-9.]+" xMax="[-0-9.]+" ',
    'yMax="[-0-9.]+">[^<]*</word>'), x, perl = TRUE))[[1]]
  data.frame(
    word = sub("</word>$", "", sub('^.*">', "", m)),
    xmin = as.numeric(sub('.*xMin="([-0-9.]+)".*', "\\1", m)),
    ymin = as.numeric(sub('.*yMin="([-0-9.]+)".*', "\\1", m)),
    xmax = as.numeric(sub('.*xMax="([-0-9.]+)".*', "\\1", m)),
    ymax = as.numeric(sub('.*yMax="([-0-9.]+)".*', "\\1", m)),
    stringsAsFactors = FALSE)
}

# A page as a grey-level matrix (0 black, 255 white), rows top to bottom.
apa_pdf_gray <- function(pdf, page, dpi) {
  f <- tempfile()
  system2("pdftoppm", c("-gray", "-r", dpi, "-f", page, "-l", page,
                        "-singlefile", shQuote(pdf), shQuote(f)))
  g <- paste0(f, ".pgm")
  on.exit(unlink(g))
  bytes <- readBin(g, "raw", n = file.size(g))
  head <- rawToChar(bytes[1:20])
  m <- regmatches(head, regexec("^P5\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)\\s", head,
                                useBytes = TRUE))[[1]]
  w <- as.integer(m[2]); h <- as.integer(m[3])
  off <- nchar(m[1], type = "bytes")
  matrix(as.integer(bytes[(off + 1):(off + w * h)]), nrow = h, ncol = w,
         byrow = TRUE)
}

# How many rules close a table, read from the page image: the strip directly
# under `last_word` (a token of the table's last row, across its own width),
# from the bottom of that word down to the top of `stop_word` (the note) or
# 30 pt, counting the runs of dark pixel rows that cross the whole strip.
# Nothing but a rule crosses that strip unbroken. Watched to see both cases
# before it was trusted: 2 under an uncaptioned markdown table with the
# extension before this fix,
# 1 under the same table now.
apa_rule_count <- function(pdf, page, last_word, stop_word = NULL, dpi = 300) {
  w <- apa_pdf_words(pdf, page)
  lw <- w[w$word == last_word, ]
  if (!nrow(lw)) return(NA_integer_)
  lw <- lw[nrow(lw), ]
  y1 <- lw$ymax + 30
  if (!is.null(stop_word)) {
    sw <- w[w$word == stop_word & w$ymin > lw$ymax, ]
    if (nrow(sw)) y1 <- min(sw$ymin)
  }
  img <- apa_pdf_gray(pdf, page, dpi)
  k <- dpi / 72
  cols <- seq(ceiling(lw$xmin * k) + 1, floor(lw$xmax * k))
  rows <- seq(ceiling(lw$ymax * k) + 1, floor(y1 * k))
  dark <- vapply(rows, function(r) all(img[r, cols] < 160), logical(1))
  sum(diff(c(FALSE, dark)) == 1L)
}

# The LAST page a marker appears on (a continuation header repeats words, so
# the first page can be the wrong one for a last-row token).
apa_last_page_of <- function(pages, marker) {
  hit <- which(vapply(pages, function(p) grepl(marker, p, fixed = TRUE),
                      logical(1)))
  if (!length(hit)) NA_integer_ else as.integer(max(hit))
}

apa_render_pdf <- function(proj, qmd) {
  unlink(proj, recursive = TRUE)
  suppressMessages(apa.quarto.new(proj, filename = "doc.qmd"))
  writeLines(qmd, file.path(proj, "doc.qmd"))
  suppressWarnings(system2("quarto",
                           c("render", shQuote(file.path(proj, "doc.qmd")),
                             "-M", "keep-tex:true"),
                           stdout = TRUE, stderr = TRUE))
}

apa_header_qmd <- function(title, mode = "man", floatsintext = FALSE) {
  paste0(
    "---\n",
    "title: \"", title, "\"\n",
    "shorttitle: \"", title, "\"\n",
    "author:\n",
    "  - name: Test Author\n",
    "    corresponding: true\n",
    "    email: test@example.com\n",
    "    affiliations:\n",
    "      - name: Test University\n",
    "format:\n",
    "  apaquarto-pdf:\n",
    "    documentmode: ", mode, "\n",
    if (floatsintext) "    floatsintext: true\n" else "",
    "---\n\n")
}

apa_long_markdown_qmd <- function(floatsintext, mode = "man") {
  rows <- paste0("| r", 0:89, " | ", 0:89, " |", collapse = "\n")
  paste0(
    apa_header_qmd("Long", mode, floatsintext),
    "XREF: @tbl-long, @tbl-kab\n\n",
    "| A | B |\n|---|---|\n| UNCAPMARK | 1 |\n| UNCAPLAST | 2 |\n\n",
    "| Row | Value |\n|---|---:|\n", rows, "\n\n",
    ": Ninety rows {#tbl-long apa-note=\"NOTEMARK this is the note.\"}\n\n",
    "```{r}\n#| label: tbl-kab\n#| tbl-cap: \"Kable rows\"\n",
    "#| apa-note: \"KABNOTE this is the note.\"\n#| echo: false\n",
    "knitr::kable(data.frame(K = paste0(\"k\", 0:59), V = 0:59))\n```\n\n",
    "ENDMARK after.\n")
}

for (fit in c(FALSE, TRUE)) {
  where <- if (fit) "floatsintext" else "tables at the end"
  test_that(paste0("long markdown and kable tables keep every row, their ",
                   "note and one rule (", where, ")"), {
    proj <- file.path(tempdir(), paste0("md-longtable-", fit))
    log <- apa_render_pdf(proj, apa_long_markdown_qmd(fit))
    pdf <- file.path(proj, "doc.pdf")
    expect_true(file.exists(pdf), info = paste(log, collapse = "\n"))
    skip_if_not(file.exists(pdf), "render produced no output to read")

    pages <- apa_pdf_pages(pdf)
    words <- unlist(strsplit(pages, "\\s+"))
    # Every row exactly once (a continuation page repeats the header, never
    # a row), for both long tables.
    rows <- words[grepl("^r[0-9]+$", words)]
    expect_setequal(rows, paste0("r", 0:89))
    expect_equal(anyDuplicated(rows), 0L)
    krows <- words[grepl("^k[0-9]+$", words)]
    expect_setequal(krows, paste0("k", 0:59))
    expect_equal(anyDuplicated(krows), 0L)
    # Numbering and cross-references; each caption printed ONCE, not again
    # on every continuation page.
    expect_equal(apa_pdf_caption_numbers(pages), 1:2)
    expect_equal(apa_xref_numbers(pages), 1:2)
    # The uncaptioned table is in the document.
    expect_false(is.na(apa_page_of(pages, "UNCAPMARK")))

    # Each note prints once, on its table's last page, under its last row --
    # at the end placement the table moves and the note has to move with it.
    for (case in list(c("NOTEMARK", "r89"), c("KABNOTE", "k59"))) {
      expect_equal(sum(lengths(regmatches(pages, gregexpr(case[1], pages)))),
                   1L, info = case[1])
      last_page <- apa_last_page_of(pages, case[2])
      expect_false(is.na(last_page), info = case[2])
      expect_identical(apa_page_of(pages, case[1]), last_page, info = case[1])
      if (!is.na(last_page)) {
        w <- apa_pdf_words(pdf, last_page)
        expect_gt(w$ymin[w$word == case[1]][1], w$ymax[w$word == case[2]][1])
        # One closing rule, read off the page.
        expect_equal(apa_rule_count(pdf, last_page, case[2], "Note."), 1L,
                     info = case[2])
      }
    }
    up <- apa_last_page_of(pages, "UNCAPLAST")
    expect_equal(apa_rule_count(pdf, up, "UNCAPLAST"), 1L)

    # Every page holding rows of either long table has that table's column
    # header above them (the kable table's continuation head is rebuilt when
    # its caption is moved to a first head of its own).
    for (i in seq_along(pages)) {
      w <- apa_pdf_words(pdf, i)
      for (case in list(c("^r[0-9]+$", "Row"), c("^k[0-9]+$", "K"))) {
        rows <- w[grepl(case[1], w$word), ]
        if (nrow(rows))
          expect_true(any(w$word == case[2] & w$ymax <= min(rows$ymin)),
                      info = paste("page", i, case[2], "header missing"))
      }
    }

    # Rows single-spaced: pitch over type size is about 1.2 single-spaced
    # and 2 double-spaced.
    first <- apa_pdf_words(pdf, apa_page_of(pages, "r0"))
    r0 <- first[first$word == "r0", ][1, ]
    r1 <- first[first$word == "r1", ][1, ]
    expect_lt((r1$ymin - r0$ymin) / (r0$ymax - r0$ymin), 1.5)
  })
}

for (mode in c("stu", "doc")) {
  test_that(paste0("a long markdown table keeps every row and one rule in ",
                   "documentmode: ", mode), {
    proj <- file.path(tempdir(), paste0("md-longtable-", mode))
    log <- apa_render_pdf(proj, apa_long_markdown_qmd(FALSE, mode))
    pdf <- file.path(proj, "doc.pdf")
    expect_true(file.exists(pdf), info = paste(log, collapse = "\n"))
    skip_if_not(file.exists(pdf), "render produced no output to read")
    pages <- apa_pdf_pages(pdf)
    words <- unlist(strsplit(pages, "\\s+"))
    expect_setequal(words[grepl("^r[0-9]+$", words)], paste0("r", 0:89))
    expect_setequal(words[grepl("^k[0-9]+$", words)], paste0("k", 0:59))
    last_page <- apa_last_page_of(pages, "r89")
    expect_false(is.na(last_page))
    expect_identical(apa_page_of(pages, "NOTEMARK"), last_page)
    if (!is.na(last_page))
      expect_equal(apa_rule_count(pdf, last_page, "r89", "Note."), 1L)
  })
}

test_that("journal mode leaves a markdown table in its float with its rule", {
  # Journal mode keeps a captioned markdown table in a float (ThreePartTable),
  # where the template's \hline is its ONLY closing rule; arming the flag
  # there would leave it with none. (Long tables are not supported in
  # journal mode; this is a short one.)
  qmd <- paste0(apa_header_qmd("Journal", "jou"),
                "| A | B |\n|---|---|\n| JROW0 | 1 |\n| JLAST | 2 |\n\n",
                ": Journal table {#tbl-j}\n\nENDMARK after.\n")
  proj <- file.path(tempdir(), "md-journal")
  log <- apa_render_pdf(proj, qmd)
  pdf <- file.path(proj, "doc.pdf")
  expect_true(file.exists(pdf), info = paste(log, collapse = "\n"))
  skip_if_not(file.exists(pdf), "render produced no output to read")
  pages <- apa_pdf_pages(pdf)
  p <- apa_page_of(pages, "JLAST")
  expect_equal(apa_rule_count(pdf, p, "JLAST"), 1L)
})

test_that("code and language spans in a table or its note still render", {
  # The writer sets up the preamble from what it writes: highlighting macros
  # for highlighted code, babel for a language span. A table it does not
  # write leaves an undefined command and no PDF (writing the tables out in
  # the filter, which was tried first, did exactly that). The notes are here too:
  # apaquarto reads an apa-note as plain text (the code and language markup
  # do not survive into it, measured), so a note cannot need the writer
  # today; the words must still arrive.
  rows <- paste0("| c", 0:59, " | ", 0:59, " |", collapse = "\n")
  qmd <- paste0(
    apa_header_qmd("Writer"),
    "| Code | Value |\n|---|---|\n| CODEROW `mean(x)`{.r} | 1 |\n",
    "| LANGROW [bonjour]{lang=fr} | 2 |\n", rows, "\n\n",
    ": Code and language {#tbl-code apa-note=\"CODENOTE uses `sd(x)`{.r}.\"}\n\n",
    "| A | B |\n|---|---|\n| PLAINROW | 1 |\n\n",
    ": Plain {#tbl-plain apa-note=\"PLAINNOTE [merci]{lang=fr}.\"}\n\n",
    "ENDMARK after.\n")
  proj <- file.path(tempdir(), "md-writer")
  log <- apa_render_pdf(proj, qmd)
  pdf <- file.path(proj, "doc.pdf")
  expect_true(file.exists(pdf), info = paste(log, collapse = "\n"))
  skip_if_not(file.exists(pdf), "render produced no output to read")
  flat <- paste(apa_pdf_pages(pdf), collapse = "\n")
  for (m in c("CODEROW", "mean(x)", "LANGROW", "bonjour", "CODENOTE",
              "sd(x)", "PLAINNOTE", "merci", "c59"))
    expect_true(grepl(m, flat, fixed = TRUE), info = m)
})

test_that("a float holding two tables is never silently renumbered", {
  # Lifted out of its float, a two-table float printed each table with the
  # float's caption and its own number, the cross-reference pointed at the
  # second, and every later table was numbered one too high. Only a float
  # holding exactly one table is lifted; this one keeps upstream's behaviour,
  # which fails the render loudly -- what must never happen is a PDF with the
  # wrong numbers.
  qmd <- paste0(
    apa_header_qmd("Two"),
    "XREF: @tbl-two, @tbl-after\n\n",
    "::: {#tbl-two}\n\n| A | B |\n|---|---|\n| ONE | 1 |\n\n",
    "| C | D |\n|---|---|\n| TWO | 2 |\n\nTwo tables caption\n\n:::\n\n",
    "| E | F |\n|---|---|\n| AFTER | 3 |\n\n: After {#tbl-after}\n")
  proj <- file.path(tempdir(), "md-two")
  log <- apa_render_pdf(proj, qmd)
  pdf <- file.path(proj, "doc.pdf")
  if (file.exists(pdf)) {
    pages <- apa_pdf_pages(pdf)
    expect_equal(apa_xref_numbers(pages), 1:2)
    flat <- paste(pages, collapse = "\n")
    expect_equal(lengths(regmatches(flat, gregexpr("Two tables caption",
                                                   flat, fixed = TRUE))), 1L)
  } else {
    succeed("the render failed loudly rather than renumbering")
  }
})

# Sections each starting on a fresh page, pushed down by \vspace*, each
# holding a lead paragraph (LEAD<id>) and one table whose rows are <id>R<n>.
apa_section <- function(offset, id, table) {
  paste0("```{=latex}\n\\clearpage\n\\vspace*{", offset, "in}\n```\n\n",
         "LEAD", id, " text.\n\n", table, "\n\n")
}
apa_md_table <- function(id, n, note = 1) {
  note_text <- if (identical(note, 2) || identical(note, 2L)) {
    paste0(id, "NOTE is a note long enough to run onto a second line of the ",
           "page, as most table notes do once they define their abbreviations.")
  } else {
    paste0(id, "NOTE a note sentence.")
  }
  paste0("| A | B |\n|---|---|\n",
         paste0("| ", id, "R", sprintf("%02d", seq_len(n)), " | ", seq_len(n),
                " |", collapse = "\n"),
         "\n\n: Table ", id, " {#tbl-", tolower(id),
         if (!isFALSE(note)) paste0(" apa-note=\"", note_text, "\"") else "",
         "}")
}
apa_kable_table <- function(id, n) {
  paste0("```{r}\n#| label: tbl-", tolower(id), "\n#| tbl-cap: \"Table ", id,
         "\"\n#| apa-note: \"", id, "NOTE a note sentence.\"\n#| echo: false\n",
         "knitr::kable(data.frame(A = sprintf('", id, "R%02d', 1:", n,
         "), B = 1:", n, "))\n```")
}
apa_grid_table <- function(id) {
  rule <- "+------------------+------------------+"
  rows <- vapply(1:5, function(r) paste0(
    "| - ", id, "R", r, "a        | 1.5              |\n",
    "| - item b         |                  |\n",
    "| - item c         |                  |\n",
    "| - item d         |                  |\n", rule), character(1))
  paste0(rule, "\n| A                | B                |\n",
         "+==================+==================+\n",
         paste(rows, collapse = "\n"), "\n\n: Table ", id, " {#tbl-",
         tolower(id), " apa-note=\"", id, "NOTE a note.\"}")
}

# Calibrated (2026-10-21) against the same document with the reservation
# removed, and against mutants of it. Cases that must MOVE (they straddle
# without the reservation):
#   M, K  6-row markdown / kable table: straddles without a reservation at
#         6.2-6.8 in;
#   G     5-row grid table whose cells hold four-item lists: straddles without
#         a reservation at 3.5-6.0 in, and when cells are sized by character
#         count alone at 3.5-5.0 in;
#   T     24-row table: straddles at 2.5-4.0 in without a reservation and with
#         the reservation capped at 20 lines;
#   B     36-row table with a two-line note, a little under a page: straddled
#         at every offset from 0.1 to 0.6 in while its estimate (a little over
#         a page) was treated as "taller than a page".
# Cases that must STAY (they fit; a reservation 1.3 times the estimate
# pushes them to the next page):
#   MS, KS  6-row markdown / kable table with a note at 5.6 in (the
#           no-reservation control fits up to 6.0 in; the shipped estimate
#           moves them from 5.9 in, about a line early by design);
#   AS    apaTables correlation table at 5.0 in (fits up to 5.5 in);
#   O     1-row table at 6.9 and 7.0 in (fits up to 7.2 in).
apa_reserve_qmd <- function() {
  s <- c(
    apa_section(6.2, "MA", apa_md_table("MA", 6)),
    apa_section(6.4, "MB", apa_md_table("MB", 6)),
    apa_section(6.6, "MC", apa_md_table("MC", 6)),
    apa_section(6.8, "MD", apa_md_table("MD", 6)),
    apa_section(6.2, "KA", apa_kable_table("KA", 6)),
    apa_section(6.6, "KB", apa_kable_table("KB", 6)),
    apa_section(3.5, "GA", apa_grid_table("GA")),
    apa_section(4.0, "GB", apa_grid_table("GB")),
    apa_section(4.5, "GC", apa_grid_table("GC")),
    apa_section(2.5, "TA", apa_md_table("TA", 24)),
    apa_section(3.0, "TB", apa_md_table("TB", 24)),
    apa_section(0.2, "BA", apa_md_table("BA", 36, note = 2)),
    apa_section(0.4, "BB", apa_md_table("BB", 36, note = 2)),
    apa_section(5.6, "MS", apa_md_table("MS", 6)),
    apa_section(5.6, "KS", apa_kable_table("KS", 6)),
    apa_section(5.0, "AS", paste0(
      "```{r}\n#| label: tbl-as\n#| tbl-cap: \"Table AS\"\n#| echo: false\n",
      "t_as <- apa.cor.table(attitude[, 1:3])\napa.quarto.pdf(t_as)\n```")),
    apa_section(6.9, "OA", apa_md_table("OA", 1, note = FALSE)),
    apa_section(7.0, "OB", apa_md_table("OB", 1, note = FALSE)))
  paste0(apa_header_qmd("Reserve", floatsintext = TRUE),
         "```{r}\n#| include: false\nlibrary(apaTables)\n```\n\n",
         paste(s, collapse = ""), "ENDMARK.\n")
}

test_that("the in-text reservation keeps a table whole without moving one that fits", {
  # In the text, a table that would straddle a page break is moved to the
  # next page whole; one that fits stays where it is. The space reserved in
  # front of each is its own estimated height, counting a cell's lines.
  proj <- file.path(tempdir(), "md-reserve")
  log <- apa_render_pdf(proj, apa_reserve_qmd())
  pdf <- file.path(proj, "doc.pdf")
  expect_true(file.exists(pdf), info = paste(log, collapse = "\n"))
  skip_if_not(file.exists(pdf), "render produced no output to read")
  pages <- apa_pdf_pages(pdf)
  page_set <- function(pattern) {
    which(vapply(pages, function(p) grepl(pattern, p), logical(1),
                 USE.NAMES = FALSE))
  }
  for (id in c("MA", "MB", "MC", "MD", "KA", "KB", "GA", "GB", "GC",
               "TA", "TB", "BA", "BB", "MS", "KS")) {
    rp <- page_set(paste0("\\b", id, "R[0-9]"))
    expect_length(rp, 1L)
    expect_identical(apa_page_of(pages, paste0(id, "NOTE")), rp[1],
                     info = id)
  }
  # The ones that fit stay on their lead paragraph's page.
  for (id in c("MS", "KS", "OA", "OB")) {
    expect_identical(apa_page_of(pages, paste0(id, "R01")),
                     apa_page_of(pages, paste0("LEAD", id)), info = id)
  }
  expect_identical(apa_page_of(pages, "Table AS"),
                   apa_page_of(pages, "LEADAS"))
  expect_identical(apa_page_of(pages, "privileges"),
                   apa_page_of(pages, "LEADAS"))
})

test_that("a table moved to the next page leaves the text above it unstretched", {
  # The page a moved table leaves is ended with \vfil, as \newpage ends one.
  # With a bare \break, the manuscript's \flushbottom spread that page's
  # paragraphs down to its foot, with gaps of 40-138 pt where the line pitch
  # is 24 pt (measured on the shipped example's second page).
  para <- paste(rep("STRETCHWORD sentence of ordinary body text.", 9),
                collapse = " ")
  qmd <- paste0(
    apa_header_qmd("Stretch", floatsintext = TRUE),
    "```{=latex}\n\\clearpage\n```\n\n",
    paste(rep(para, 4), collapse = "\n\n"), "\n\n",
    apa_md_table("SX", 30), "\n\nENDMARK.\n")
  proj <- file.path(tempdir(), "md-stretch")
  log <- apa_render_pdf(proj, qmd)
  pdf <- file.path(proj, "doc.pdf")
  expect_true(file.exists(pdf), info = paste(log, collapse = "\n"))
  skip_if_not(file.exists(pdf), "render produced no output to read")
  pages <- apa_pdf_pages(pdf)
  tp <- apa_page_of(pages, "SXR01")
  pp <- apa_page_of(pages, "STRETCHWORD")
  # The table did move (else this test checks nothing) ...
  expect_true(!is.na(tp) && !is.na(pp) && tp > pp)
  # ... and the lines it left behind keep their pitch.
  # Every text line below the running head, top to bottom: the gap between
  # consecutive lines (paragraph breaks included -- the manuscript adds no
  # space between paragraphs) stays at the line pitch.
  w <- apa_pdf_words(pdf, pp)
  ys <- sort(unique(round(w$ymin[w$ymin > 90])))
  gaps <- diff(ys)
  expect_true(length(gaps) > 3L)
  expect_lt(max(gaps), 1.5 * stats::median(gaps))
})

test_that("a long note does not spread its table over near-empty pages", {
  # Every page of a table keeps room at its foot for the last foot (rule and
  # note), so the note can never be orphaned -- but that room is capped at a
  # quarter of the page. Uncapped, a table with a long note left most of each
  # page blank (measured on an apaTables table of 45 rows with a
  # 2,000-character note: 3 pages became 8; on this document, 3 became 4).
  note <- paste(rep("LONGNOTE sentence that defines an abbreviation used in the table.",
                    24), collapse = " ")
  qmd <- paste0(
    apa_header_qmd("Longnote"),
    "| A | B |\n|---|---|\n",
    paste0("| LN", sprintf("%02d", 1:45), " | ", 1:45, " |", collapse = "\n"),
    "\n\n: Long note {#tbl-ln apa-note=\"", note, "\"}\n\nENDMARK.\n")
  proj <- file.path(tempdir(), "md-longnote")
  log <- apa_render_pdf(proj, qmd)
  pdf <- file.path(proj, "doc.pdf")
  expect_true(file.exists(pdf), info = paste(log, collapse = "\n"))
  skip_if_not(file.exists(pdf), "render produced no output to read")
  pages <- apa_pdf_pages(pdf)
  row_pages <- which(vapply(pages, function(p) grepl("\\bLN[0-9]{2}\\b", p),
                            logical(1), USE.NAMES = FALSE))
  words <- unlist(strsplit(pages, "\\s+"))
  expect_setequal(words[grepl("^LN[0-9]{2}$", words)], sprintf("LN%02d", 1:45))
  expect_lte(length(row_pages), 3L)
})

test_that("notes with several parts or a footnote travel, with or without a header row", {
  # A note is stored, numbered, in a block of its own before its table; the
  # table carries only a marker. Stored in a header-less table's caption it
  # failed to render: a caption cannot hold a paragraph break, and its copy
  # in the list of tables broke on a footnote.
  hl <- function(id, note) paste0(
    "::: {#tbl-", tolower(id), " apa-note='", note, "'}\n\n",
    "-------- -----\n", id, "R1    1\n", id, "R2    2\n-------- -----\n\n",
    "Caption ", id, "\n\n:::\n\n")
  # The ::: form keeps a note's markup; a note given as a caption attribute
  # is read by apaquarto as plain text.
  hd <- function(id, note) paste0(
    "::: {#tbl-", tolower(id), " apa-note='", note, "'}\n\n",
    "| A | B |\n|---|---|\n| ", id, "R1 | 1 |\n| ", id, "R2 | 2 |\n\n",
    "Caption ", id, "\n\n:::\n\n")
  qmd <- paste0(
    apa_header_qmd("Notes"),
    "```{=latex}\n\\listoftables\n```\n\nBODYMARK.\n\n",
    hl("HM", '["First part APART.","Second part BPART."]'),
    hl("HF", "A footnote^[NFTEXT in the footnote.] HFNOTE end."),
    hd("DF", "A footnote^[DFTEXT in the footnote.] DFNOTE end."),
    "ENDMARK.\n")
  proj <- file.path(tempdir(), "md-notes")
  log <- apa_render_pdf(proj, qmd)
  pdf <- file.path(proj, "doc.pdf")
  expect_true(file.exists(pdf), info = paste(log, collapse = "\n"))
  skip_if_not(file.exists(pdf), "render produced no output to read")
  pages <- apa_pdf_pages(pdf)
  for (m in c("APART", "BPART", "HFNOTE", "DFNOTE"))
    expect_false(is.na(apa_page_of(pages, m)), info = m)
  # Each note on its table's page, not in the body text.
  for (case in list(c("APART", "HMR2"), c("HFNOTE", "HFR2"), c("DFNOTE", "DFR2")))
    expect_identical(apa_page_of(pages, case[1]), apa_page_of(pages, case[2]),
                     info = case[1])
  expect_false(identical(apa_page_of(pages, "APART"),
                         apa_page_of(pages, "BODYMARK")))
  # The footnote mark survives in a header table's note (the writer strips
  # footnotes from the repeated header row, which once held the note).
  expect_match(pages[[apa_page_of(pages, "DFNOTE")]], "footnote[ ]?[0-9]")
  # The list of tables lists captions, not notes.
  lot <- pages[[apa_page_of(pages, "BODYMARK")]]
  expect_false(grepl("APART|HFNOTE|DFNOTE", lot))
})

test_that("a heading goes with a table that has to move", {
  # The space reserved for a table in the text is asked for in front of a
  # heading that directly precedes it, so the two move together; between
  # them it broke the page after the heading, which was left alone at the
  # foot of the page.
  para <- paste(rep("Body text sentence for the page.", 12), collapse = " ")
  qmd <- paste0(
    apa_header_qmd("Heading", floatsintext = TRUE),
    "```{=latex}\n\\clearpage\n\\vspace*{4.6in}\n```\n\n", para, "\n\n",
    "## HEADMARK Section\n\n", apa_md_table("HT", 12), "\n\nENDMARK.\n")
  proj <- file.path(tempdir(), "md-heading")
  log <- apa_render_pdf(proj, qmd)
  pdf <- file.path(proj, "doc.pdf")
  expect_true(file.exists(pdf), info = paste(log, collapse = "\n"))
  skip_if_not(file.exists(pdf), "render produced no output to read")
  pages <- apa_pdf_pages(pdf)
  expect_identical(apa_page_of(pages, "HEADMARK"), apa_page_of(pages, "HTR01"))
})

test_that("a table up to a page moves whole; a taller one starts where it is", {
  # With two-line notes, a 37-row table (estimated 46 lines) fits a fresh
  # page and is moved there whole; a 38-row one (47 lines) cannot fit any
  # page, so it starts below its lead paragraph and continues on the next
  # page rather than leaving that page nearly empty. (A band three lines
  # over the page moved the 38-row table whole, and it split anyway.)
  qmd <- paste0(
    apa_header_qmd("Near", floatsintext = TRUE),
    apa_section(3.0, "NA", apa_md_table("NA", 37, note = 2)),
    apa_section(3.0, "NB", apa_md_table("NB", 38, note = 2)),
    "ENDMARK.\n")
  proj <- file.path(tempdir(), "md-near")
  log <- apa_render_pdf(proj, qmd)
  pdf <- file.path(proj, "doc.pdf")
  expect_true(file.exists(pdf), info = paste(log, collapse = "\n"))
  skip_if_not(file.exists(pdf), "render produced no output to read")
  pages <- apa_pdf_pages(pdf)
  pg <- function(pattern) which(vapply(pages, function(p) grepl(pattern, p),
                                       logical(1), USE.NAMES = FALSE))
  expect_length(pg("\\bNAR[0-9]"), 1L)
  expect_identical(apa_page_of(pages, "NAR01"),
                   apa_page_of(pages, "NANOTE"))
  expect_identical(apa_page_of(pages, "NBR01"), apa_page_of(pages, "LEADNB"))
})

apa_fill_qmd <- function() {
  body <- ""
  for (n in 36:45) {
    rows <- paste0("| R", n, "x", sprintf("%02d", 0:(n - 1)), " | ",
                   0:(n - 1), " |", collapse = "\n")
    body <- paste0(body, "| A | B |\n|---|---|\n", rows, "\n\n",
                   ": Table with ", n, " rows {#tbl-n", n,
                   " apa-note=\"FILL", n, "NOTE is a note long enough to run ",
                   "onto a second line of the page, as most table notes do ",
                   "once they define their abbreviations.\"}\n\n")
  }
  paste0(apa_header_qmd("Fill"), body, "ENDMARK after.\n")
}

test_that("a table that fills its page keeps its note and its header together", {
  # At the end of a table whose last foot (rule and note) is taller than its
  # running foot, longtable ships the final rows and sets the last foot on a
  # page of its own, under a repeated header with no rows. And a note is a
  # top-aligned box, so its lines after the first were DEPTH, which nothing
  # measured: the page overflowed and broke before the last row, which then
  # printed with no column header. Which row count does it depends on the
  # page, so a range is rendered, with two-line notes: measured, 38 and 39
  # rows lost their header and a 40-row table orphaned its note.
  proj <- file.path(tempdir(), "md-fill")
  log <- apa_render_pdf(proj, apa_fill_qmd())
  pdf <- file.path(proj, "doc.pdf")
  expect_true(file.exists(pdf), info = paste(log, collapse = "\n"))
  skip_if_not(file.exists(pdf), "render produced no output to read")
  pages <- apa_pdf_pages(pdf)
  for (n in 36:45) {
    p <- apa_page_of(pages, paste0("FILL", n, "NOTE"))
    expect_false(is.na(p), info = n)
    if (!is.na(p))
      expect_true(grepl(paste0("R", n, "x[0-9]{2}"), pages[[p]]),
                  info = paste(n, "rows: the note is on a page with none"))
  }
  # Every page that holds rows holds the column header above them.
  for (i in seq_along(pages)) {
    w <- apa_pdf_words(pdf, i)
    rows <- w[grepl("^R[0-9]+x[0-9]{2}$", w$word), ]
    if (nrow(rows))
      expect_true(any(w$word == "A" & w$ymax <= min(rows$ymin)),
                  info = paste("page", i, "has rows and no header"))
  }
})

test_that("an apaTables table that fills its page keeps a row with its note", {
  # apaTables' own running foot is a 48 pt spacer, shorter than a two- or
  # three-line double-spaced note, so at the end of a table that filled its
  # page longtable set the note on a page of its own under a repeated header
  # (measured at 39 predictors); and a model's R-squared, interval and closing
  # blank row could be carried over without a predictor. The template pads
  # the running foot to the note's height and the emitter joins those rows to
  # the one above.
  chunks <- vapply(37:42, function(k) paste0(
    "\\newpage\n\nLead text for table ", k, ".\n\n",
    "```{r}\n#| label: tbl-k", k, "\n#| tbl-cap: Sweep ", k, "\n",
    "t", k, " <- apa.reg.table(lm(y ~ ., D[, c(paste0('p', 1:", k,
    "), 'y')]))\napa.quarto.pdf(t", k, ")\n```\n\n"), character(1))
  qmd <- paste0(
    apa_header_qmd("Regsweep", floatsintext = TRUE),
    "```{r}\n#| include: false\nlibrary(apaTables)\nset.seed(1)\n",
    "D <- as.data.frame(matrix(rnorm(400 * 60), 400))\n",
    "names(D) <- paste0('p', 1:60)\nD$y <- rnorm(400)\n```\n\nIntro.\n\n",
    paste(chunks, collapse = ""), "ENDMARK.\n")
  proj <- file.path(tempdir(), "md-regsweep")
  log <- apa_render_pdf(proj, qmd)
  pdf <- file.path(proj, "doc.pdf")
  expect_true(file.exists(pdf), info = paste(log, collapse = "\n"))
  skip_if_not(file.exists(pdf), "render produced no output to read")
  for (i in seq_len(apa_pdf_npages(pdf))) {
    w <- apa_pdf_words(pdf, i)
    rows <- w[grepl("^p[0-9]+$", w$word), ]
    for (ny in w$ymin[w$word == "Note."])
      expect_true(any(rows$ymin < ny),
                  info = paste("page", i, "has a note and no predictor"))
    if (nrow(rows))
      expect_true(any(w$word == "Predictor" & w$ymax <= min(rows$ymin)),
                  info = paste("page", i, "has rows and no header"))
  }
})

test_that("a short apaTables table is moved whole at every position on the page", {
  # A table well under a page must never split across a page break. Two
  # estimates were a line short: the space apaTables adds above each group
  # label of a two-way table (its last row was carried over, measured at one
  # position in a manuscript), and in documentmode: doc the space longtable
  # leaves above itself (a correlation table split at one position). Each is
  # swept in 0.1 in steps across the positions where it stops fitting.
  sweep_qmd <- function(mode, prefix, offsets, call) {
    s <- vapply(seq_along(offsets), function(i) {
      id <- sprintf("%s%02d", prefix, i)
      apa_section(offsets[i], id, paste0(
        "```{r}\n#| label: tbl-", tolower(id), "\n#| tbl-cap: \"Sweep ", id,
        "\"\n#| echo: false\nt_", i, " <- ", call, "\napa.quarto.pdf(t_", i,
        ")\n```"))
    }, character(1))
    paste0(apa_header_qmd("Sweep", mode = mode, floatsintext = TRUE),
           "```{r}\n#| include: false\nlibrary(apaTables)\n```\n\n",
           paste(s, collapse = ""), "ENDMARK.\n")
  }
  cases <- list(
    list(mode = "man", prefix = "W", offsets = seq(4.5, 7.0, by = 0.1),
         last = "35.62",
         call = paste0("apa.2way.table(iv1 = gender, iv2 = alcohol, ",
                       "dv = attractiveness, data = goggles, ",
                       "show.conf.interval = TRUE)")),
    list(mode = "doc", prefix = "C", offsets = seq(3.0, 5.0, by = 0.1),
         last = "advance", call = "apa.cor.table(attitude)"))
  for (cs in cases) {
    proj <- file.path(tempdir(), paste0("md-shortsweep-", cs$mode))
    log <- apa_render_pdf(proj, sweep_qmd(cs$mode, cs$prefix, cs$offsets,
                                          cs$call))
    pdf <- file.path(proj, "doc.pdf")
    expect_true(file.exists(pdf), info = paste(log, collapse = "\n"))
    if (!file.exists(pdf)) next
    pages <- apa_pdf_pages(pdf)
    for (i in seq_along(cs$offsets)) {
      id <- sprintf("%s%02d", cs$prefix, i)
      cp <- apa_page_of(pages, paste0("Sweep ", id))
      expect_false(is.na(cp), info = id)
      if (is.na(cp)) next
      later <- apa_page_of(pages[cp:length(pages)], cs$last)
      expect_identical(later, 1L,
                       info = paste(id, "at", cs$offsets[i], "in split"))
    }
  }
})

test_that("tables in a callout or a layout panel keep their rule and note", {
  # Inside a box (callout, layout panel, minipage) longtable never places a
  # table's last foot, so the template sets it by hand as the table's final
  # item: one closing rule, and the note under it.
  qmd <- paste0(
    apa_header_qmd("Boxes", floatsintext = TRUE),
    "```{r}\n#| include: false\nlibrary(apaTables)\n```\n\n",
    "::: {.callout-note}\n\n```{r}\n#| label: tbl-boxcor\n",
    "#| tbl-cap: \"Correlations in a callout\"\n",
    "t_box <- apa.cor.table(attitude[, 1:3])\napa.quarto.pdf(t_box)\n```\n\n:::\n\n",
    "::: {.callout-note}\n\n| CA | CB |\n|---|---|\n| CALLU1 | 1 |\n",
    "| CALLU2 | 2 |\n\nAfter the uncaptioned table.\n\n:::\n\n",
    "::: {.callout-note}\n\n| DA | DB |\n|---|---|\n| CAPC1 | 1 |\n",
    "| CAPC2 | 2 |\n\n: In a callout {#tbl-call apa-note=\"CALLNOTE here.\"}",
    "\n\n:::\n\n",
    ":::: {layout-ncol=2}\n\n::: {}\n\n| LA | LB |\n|---|---|\n| LAYA1 | 1 |\n",
    "| LAYA2 | 2 |\n\n:::\n\n::: {}\n\n| MA | MB |\n|---|---|\n| LAYB1 | 1 |\n",
    "| LAYB2 | 2 |\n\n:::\n\n::::\n\nENDMARK.\n")
  proj <- file.path(tempdir(), "md-boxes")
  log <- apa_render_pdf(proj, qmd)
  pdf <- file.path(proj, "doc.pdf")
  expect_true(file.exists(pdf), info = paste(log, collapse = "\n"))
  skip_if_not(file.exists(pdf), "render produced no output to read")
  pages <- apa_pdf_pages(pdf)
  expect_false(is.na(apa_page_of(pages, "CALLNOTE")))
  # apaTables' own table in a callout: its note is spliced into its last
  # foot, which only the template's hand-set last foot brings to the page.
  cp <- apa_page_of(pages, "Correlations in a callout")
  expect_true(grepl("number of cases", pages[[cp]], fixed = TRUE))
  expect_equal(apa_rule_count(pdf, cp, ".001", "Note."), 1L)
  for (last in c("CALLU2", "LAYA2", "LAYB2")) {
    p <- apa_last_page_of(pages, last)
    expect_equal(apa_rule_count(pdf, p, last), 1L, info = last)
  }
})

test_that("a table with no header row keeps every row and its note", {
  # Its note cannot be stored in a header cell, so it is stored from the
  # caption, which the writer also sets in the table's first head; the table
  # is lifted like any other and carries its note to the end.
  qmd <- paste0(
    apa_header_qmd("Headless"),
    "XREF: @tbl-hl\n\nBODYMARK.\n\n",
    "-------- -----\n",
    paste0(sprintf("HLR%02d    %d", 1:60, 1:60), collapse = "\n"),
    "\n-------- -----\n\n",
    ": No header {#tbl-hl apa-note=\"HLNOTE here.\"}\n\nENDMARK.\n")
  proj <- file.path(tempdir(), "md-headless")
  log <- apa_render_pdf(proj, qmd)
  pdf <- file.path(proj, "doc.pdf")
  expect_true(file.exists(pdf), info = paste(log, collapse = "\n"))
  skip_if_not(file.exists(pdf), "render produced no output to read")
  pages <- apa_pdf_pages(pdf)
  words <- unlist(strsplit(pages, "\\s+"))
  expect_setequal(words[grepl("^HLR[0-9]+$", words)], sprintf("HLR%02d", 1:60))
  expect_identical(apa_page_of(pages, "HLNOTE"),
                   apa_last_page_of(pages, "HLR60"))
  expect_false(identical(apa_page_of(pages, "HLNOTE"),
                         apa_page_of(pages, "BODYMARK")))
})

test_that("a table float holding a paragraph as well is not split from it", {
  # Lifted, only the table would travel to the end and the paragraph that
  # belongs to it would stay in the body text. Such a float keeps upstream's
  # behaviour, which fails the render loudly.
  rows <- paste0("| DVR", 1:3, " | ", 1:3, " |", collapse = "\n")
  qmd <- paste0(
    apa_header_qmd("Divfloat"),
    "BODYMARK.\n\n::: {#tbl-dv apa-note=\"DVNOTE here.\"}\n\n",
    "INTROPARA belongs to the table.\n\n| A | B |\n|---|---|\n", rows,
    "\n\nDiv float caption\n\n:::\n\nENDMARK.\n")
  proj <- file.path(tempdir(), "md-divfloat")
  log <- apa_render_pdf(proj, qmd)
  pdf <- file.path(proj, "doc.pdf")
  if (file.exists(pdf)) {
    pages <- apa_pdf_pages(pdf)
    expect_identical(apa_page_of(pages, "INTROPARA"),
                     apa_page_of(pages, "DVR1"))
  } else {
    succeed("the render failed loudly rather than separating them")
  }
})

apa_spacing_qmd <- function() {
  paste0(
    apa_header_qmd("Spacing", floatsintext = TRUE),
    "```{r}\n#| include: false\nlibrary(apaTables)\n```\n\n",
    "```{r}\n#| label: tbl-cor\n#| tbl-cap: \"Correlations\"\n",
    "t_cor <- apa.cor.table(attitude[, 1:3])\n",
    "apa.quarto.pdf(t_cor)\n```\n\n",
    "| Group | Score |\n|---|---:|\n| mda | 1 |\n| mdb | 2 |\n\n",
    ": Markdown {#tbl-md apa-note=\"MDNOTE first line of a note that is long ",
    "enough to wrap onto a second line of the page so that its line spacing ",
    "can be measured MDWRAP here.\"}\n")
}

test_that("rows are single-spaced and notes keep the manuscript's spacing", {
  proj <- file.path(tempdir(), "table-spacing")
  log <- apa_render_pdf(proj, apa_spacing_qmd())
  pdf <- file.path(proj, "doc.pdf")
  expect_true(file.exists(pdf), info = paste(log, collapse = "\n"))
  skip_if_not(file.exists(pdf), "render produced no output to read")

  pages <- apa_pdf_pages(pdf)
  w <- apa_pdf_words(pdf, apa_page_of(pages, ".83**"))
  one <- function(t) w[w$word == t, ][1, ]
  ratio <- function(a, b) (one(b)$ymin - one(a)$ymin) /
    (one(a)$ymax - one(a)$ymin)
  # The stacked lines of one correlation cell (r, CI, p): measured 1.20
  # single-spaced and 1.99 when the table took the manuscript's spacing.
  expect_lt(ratio(".83**", "[.66,"), 1.5)
  # The note's two lines ("... indicate" / "the 95% confidence interval.")
  # are double-spaced, like the text: measured about 2.
  expect_gt(ratio("Note.", "confidence"), 1.5)
  # The note clears the bottom rule: measured 6.6 pt from the last row's
  # text to the note's with booktabs' below-rule gap, 3.9 pt without it,
  # where the rule runs through the note's first line.
  last <- w[w$word == ".001", ]
  last <- last[nrow(last), ]
  expect_gt(one("Note.")$ymin - last$ymax, 5.5)
  expect_equal(apa_rule_count(pdf, apa_page_of(pages, ".83**"), ".001",
                              "Note."), 1L)
  # One note per table on the page: a last foot printed twice would also
  # count one rule above its first note.
  expect_equal(sum(w$word == "Note."),
               sum(w$word %in% c("Correlations", "Markdown")))

  w <- apa_pdf_words(pdf, apa_page_of(pages, "mda"))
  expect_lt(ratio("mda", "mdb"), 1.5)
  expect_gt(ratio("MDNOTE", "MDWRAP"), 1.5)
  # The markdown table's note is printed by the template, not spliced in, so
  # its gap below the rule is checked separately: measured 8.7 pt from the
  # last row's text to the note's, 5.3 pt without booktabs' gap.
  expect_gt(one("MDNOTE")$ymin - one("mdb")$ymax, 7)
  expect_equal(apa_rule_count(pdf, apa_page_of(pages, "mda"), "mdb",
                              "Note."), 1L)
})
