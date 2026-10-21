# Tables written in markdown number 1, 2, 3, ... in an apaquarto PDF, and so
# do apaTables' own tables in the same document.
#
# Pandoc 3.8+ (Quarto 1.9) wraps each captioned markdown table as a float
# whose \caption sits OUTSIDE the longtable, with \def\LTcaptype{none} inside
# so the longtable steps no counter. The stock template decrements the table
# counter at every \end{longtable} that had no caption of its own, which there
# cancels the float's number. Measured on stock apaquarto 5.0.18 and upstream
# main: EVERY markdown table prints as "Table 1" and every @tbl- reference to
# one reads "Table 1". In a document MIXING the two kinds, the markdown
# captions come out right and the apaTables table AFTER each one repeats its
# number instead (1, 2, 2, 3 for apaTables, markdown, apaTables, markdown).
# apaTables' labelled tables are protected only by their own \caption inside
# the longtable; nothing about them is immune to the counter.
#
# Bundle 5.0.18+apaTables.27 decrements only when the longtable was itself
# counting as a table (\LTcaptype is "table") and no caption ran inside it.
# This file is the pin, with two documents:
#
# * a minimal reproducer: three markdown tables, a raw captioned longtable
#   (which takes a number of its own), a raw uncaptioned one (which takes
#   none), and a fourth markdown table -- so a template that always
#   decrements fails, and so does one that never does;
# * the MIXED document, in both placements (tables at the end, and
#   floatsintext).
#
# Mutations, run by hand and recorded rather than automated. Reproducer, with
# apa_unpatch_extension() applied (the document has no R chunk, so nothing
# repairs the extension mid-render): captions 1 1 1 1 3, cross-references
# 1 1 1 3 (2026-09-14). Mixed document, with the pre-fix bundle .26 in place:
# captions and cross-references 1 2 2 3 (2026-10-21). Every assertion below
# fails under its mutation.

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto not installed")
skip_if_not(nzchar(Sys.which("pdftotext")), "poppler (pdftotext) not installed")
skip_if_not(nzchar(Sys.which("pdfinfo")), "poppler (pdfinfo) not installed")
apa_skip_if_library_stale()

apa_markdown_numbering_qmd <- function() {
  tbl <- function(k, cap, rows) paste0(
    "| A | B |\n|---|---|\n", rows, "\n\n: ", cap, " {#tbl-", k, "}\n\n")
  paste0(
    "---\n",
    "title: \"Markdown Table Numbering\"\n",
    "shorttitle: \"Numbering\"\n",
    "author:\n",
    "  - name: Test Author\n",
    "    corresponding: true\n",
    "    email: test@example.com\n",
    "    affiliations:\n",
    "      - name: Test University\n",
    "format:\n",
    "  apaquarto-pdf:\n",
    "    documentmode: man\n",
    "---\n\n",
    "XREF: @tbl-a, @tbl-b, @tbl-c, @tbl-e\n\n",
    tbl("a", "First table", "| 1 | 2 |"),
    tbl("b", "Second table", "| 3 | 4 |"),
    tbl("c", "Third table", "| 5 | 6 |"),
    "```{=latex}\n",
    "\\begin{longtable}{ll}\n",
    "\\caption{Raw captioned longtable}\\\\\n",
    "\\hline\na & b \\\\\n\\hline\n",
    "\\end{longtable}\n\n",
    "\\begin{longtable}{ll}\n",
    "\\hline\nc & d \\\\\n\\hline\n",
    "\\end{longtable}\n",
    "```\n\n",
    tbl("e", "Fifth table", "| 7 | 8 |"))
}

test_that("markdown tables in a manuscript-mode PDF number consecutively", {
  proj <- file.path(tempdir(), "md-numbering")
  unlink(proj, recursive = TRUE)
  suppressMessages(apa.quarto.new(proj, filename = "doc.qmd"))
  writeLines(apa_markdown_numbering_qmd(), file.path(proj, "doc.qmd"))

  log <- suppressWarnings(system2("quarto",
                                  c("render", shQuote(file.path(proj, "doc.qmd"))),
                                  stdout = TRUE, stderr = TRUE))
  pdf <- file.path(proj, "doc.pdf")
  expect_true(file.exists(pdf), info = paste(log, collapse = "\n"))
  skip_if_not(file.exists(pdf), "render produced no output to read")

  pages <- apa_pdf_pages(pdf)
  # Captions: the three markdown tables, the raw captioned longtable, and the
  # fourth markdown table; the raw uncaptioned longtable takes no number.
  expect_equal(apa_pdf_caption_numbers(pages), 1:5)
  # Cross-references resolve to the numbers the captions carry.
  expect_equal(apa_xref_numbers(pages), c(1L, 2L, 3L, 5L))
})

apa_mixed_numbering_qmd <- function(floatsintext) {
  md <- function(k, cap) paste0(
    "| A | B |\n|---|---|\n| 1 | 2 |\n\n: ", cap, " {#tbl-", k, "}\n\n")
  apa <- function(k, cap, call) paste0(
    "```{r}\n#| label: tbl-", k, "\n#| tbl-cap: \"", cap, "\"\n",
    "t_", k, " <- ", call, "\n", "apa.quarto.pdf(t_", k, ")\n```\n\n")
  paste0(
    "---\n",
    "title: \"Mixed Table Numbering\"\n",
    "shorttitle: \"Mixed\"\n",
    "author:\n",
    "  - name: Test Author\n",
    "    corresponding: true\n",
    "    email: test@example.com\n",
    "    affiliations:\n",
    "      - name: Test University\n",
    "format:\n",
    "  apaquarto-pdf:\n",
    "    documentmode: man\n",
    if (floatsintext) "    floatsintext: true\n" else "",
    "---\n\n",
    "```{r}\n#| include: false\nlibrary(apaTables)\n```\n\n",
    "XREF: @tbl-cor, @tbl-mdone, @tbl-reg, @tbl-mdtwo\n\n",
    apa("cor", "Apa Correlations", "apa.cor.table(attitude[, 1:3])"),
    md("mdone", "Markdown one"),
    apa("reg", "Apa Regression",
        "apa.reg.table(lm(rating ~ complaints, data = attitude))"),
    md("mdtwo", "Markdown two"))
}

for (fit in c(FALSE, TRUE)) {
  where <- if (fit) "floatsintext" else "tables at the end"
  test_that(paste0("apaTables and markdown tables mixed number 1 to 4 (",
                   where, ")"), {
    proj <- file.path(tempdir(), paste0("md-mixed-", fit))
    unlink(proj, recursive = TRUE)
    suppressMessages(apa.quarto.new(proj, filename = "doc.qmd"))
    writeLines(apa_mixed_numbering_qmd(fit), file.path(proj, "doc.qmd"))

    log <- suppressWarnings(system2("quarto",
                                    c("render", shQuote(file.path(proj, "doc.qmd"))),
                                    stdout = TRUE, stderr = TRUE))
    pdf <- file.path(proj, "doc.pdf")
    expect_true(file.exists(pdf), info = paste(log, collapse = "\n"))
    skip_if_not(file.exists(pdf), "render produced no output to read")

    pages <- apa_pdf_pages(pdf)
    expect_equal(apa_pdf_caption_numbers(pages), 1:4)
    expect_equal(apa_xref_numbers(pages), 1:4)
  })
}
