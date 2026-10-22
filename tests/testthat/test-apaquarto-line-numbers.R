# A PDF manuscript with line numbers (numbered-lines: true) renders when it
# holds tables, and the text around the tables stays numbered.
#
# The class gets the longtable option, which lineno passes on to edtable.
# With line numbers on, edtable numbers each row of a longtable on the page
# and leaves a macro undefined until a row's first cell has run; a caption
# line, or a first row that opens with \multicolumn, ends before that, and
# the document stopped with "Undefined control sequence". apaTables' tables
# are longtables on the page, and so are markdown and knitr::kable()
# tables, which are kept out of their float so that long ones keep every
# row. An unlabelled chunk's table has no caption and is a longtable*
# (Quarto's name for it). The bundled template turns line numbering off
# inside both environments.
#
# Mutation, run by hand (2026-10-22): with bundle .35's template both
# renders fail with "Undefined control sequence" (in-text placement: a
# one-page PDF with no tables; tables at the end: the text pages, no
# tables). With only the longtable environment hooked, the uncaptioned raw
# table whose first cell is a \multicolumn stops the render, and without it
# the unlabelled two-way table renders with its rows numbered. With the
# environment hooks of an earlier version, the run-in heading loses its
# number; with the patch but no \par, so do the heading and the paragraph
# running into the raw table. Those two show only with the tables in the
# text: at the end, nothing interrupts the text.

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto not installed")
skip_if_not(nzchar(Sys.which("pdftotext")), "poppler (pdftotext) not installed")
skip_if_not(nzchar(Sys.which("pdfinfo")), "poppler (pdfinfo) not installed")
apa_skip_if_library_stale()

apa_line_numbers_qmd <- function(floatsintext) {
  paste0(
    "---\n",
    "title: \"Line Numbers\"\n",
    "shorttitle: \"Line Numbers\"\n",
    "author:\n",
    "  - name: Test Author\n",
    "    corresponding: true\n",
    "    email: test@example.com\n",
    "    affiliations:\n",
    "      - name: Test University\n",
    "format:\n",
    "  apaquarto-pdf:\n",
    "    numbered-lines: true\n",
    "    floatsintext: ", tolower(floatsintext), "\n",
    "---\n\n",
    "```{r}\n#| include: false\nlibrary(apaTables)\n```\n\n",
    "TEXTBEFORE paragraph.\n\n",
    # First table in the document, on purpose: measured, the same table
    # placed after another longtable renders even without the fix, so only
    # here can it show the defect.
    "```{=latex}\n",
    "\\begin{longtable}{lcc}\n",
    "\\toprule\n",
    "\\multicolumn{1}{c}{} & \\multicolumn{2}{c}{SPANHEAD} \\\\\n",
    "\\cmidrule{2-3}\n",
    "Row & A & B \\\\\n",
    "\\midrule\n",
    "RAWROW & 1 & 2 \\\\\n",
    "\\bottomrule\n",
    "\\end{longtable}\n",
    "```\n\n",
    "TEXTFIRST paragraph.\n\n",
    "```{r}\n#| label: tbl-cor\n#| echo: false\n",
    "cor_table <- apa.cor.table(attitude[, 1:3])\n",
    "apa.quarto.pdf(cor_table)\n```\n\n",
    "TEXTBETWEEN paragraph.\n\n",
    "```{r}\n#| label: tbl-long\n#| tbl-cap: \"Long Markdown\"\n",
    "#| apa-note: \"Long note.\"\n#| echo: false\n",
    "knitr::kable(data.frame(id = sprintf(\"row%02d\", 1:70), ",
    "x = round((1:70) / 7, 2)))\n```\n\n",
    "TEXTAFTER paragraph.\n\n",
    "```{r}\n#| echo: false\n",
    "apa.quarto.pdf(apa.2way.table(iv1 = gender, iv2 = alcohol, ",
    "dv = attractiveness, data = goggles))\n```\n\n",
    "TEXTLAST paragraph.\n\n",
    # A level-4 heading is run in: it waits for the next paragraph, here a
    # table, and must still be set (and numbered) before numbering stops.
    "#### RUNINHEAD heading.\n\n",
    "```{r}\n#| echo: false\n",
    "apa.quarto.pdf(apa.1way.table(iv = dose, dv = libido, data = viagra))\n```\n\n",
    # Raw LaTeX can run a paragraph straight into a table.
    "```{=latex}\n",
    "RUNINPARA paragraph running into a table.\n",
    "\\begin{longtable}{ll}\n",
    "\\toprule\nA & B \\\\\n\\midrule\nINTOROW & 1 \\\\\n\\bottomrule\n",
    "\\end{longtable}\n",
    "```\n\n",
    "TEXTEND paragraph.\n")
}

for (fit in c(TRUE, FALSE)) {
  test_that(paste0("a line-numbered manuscript renders with its tables (floatsintext: ",
                   tolower(fit), ")"), {
    proj <- file.path(tempdir(), paste0("line-numbers-", tolower(fit)))
    unlink(proj, recursive = TRUE)
    suppressMessages(apa.quarto.new(proj, filename = "doc.qmd"))
    writeLines(apa_line_numbers_qmd(fit), file.path(proj, "doc.qmd"))

    log <- suppressWarnings(system2("quarto",
                                    c("render", shQuote(file.path(proj, "doc.qmd"))),
                                    stdout = TRUE, stderr = TRUE))
    pdf <- file.path(proj, "doc.pdf")
    # The render's own exit status, not only the file: a LaTeX failure here
    # still leaves a one-page PDF (the title page) behind.
    expect_null(attr(log, "status"), info = paste(log, collapse = "\n"))
    expect_true(file.exists(pdf), info = paste(log, collapse = "\n"))
    skip_if_not(file.exists(pdf), "render produced no output to read")

    pages <- apa_pdf_pages(pdf)
    text <- paste(pages, collapse = "\n")
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]

    # both captioned tables, every row of the long one, its note, and the
    # unlabelled two-way table (no caption: it has no number)
    expect_equal(apa_pdf_caption_numbers(pages), 1:2)
    expect_match(text, "Female", fixed = TRUE)
    expect_setequal(unique(regmatches(text, gregexpr("row[0-9]{2}", text))[[1]]),
                    sprintf("row%02d", 1:70))
    expect_match(text, "Long note.", fixed = TRUE)

    # the text carries line numbers that keep counting up past each table
    markers <- c("TEXTBEFORE", "TEXTFIRST", "TEXTBETWEEN", "TEXTAFTER", "TEXTLAST",
                 "RUNINHEAD", "RUNINPARA", "TEXTEND")
    # (A marker may share its numbered line with the one before it: with
    # the tables at the end, the run-in heading runs into RUNINPARA.)
    nums <- vapply(markers, function(m) {
      hit <- grep(paste0("^\\s*[0-9]+\\s.*\\b", m, "\\b"), lines, value = TRUE)
      if (length(hit) == 1) as.integer(sub("^\\s*([0-9]+).*", "\\1", hit)) else NA_integer_
    }, integer(1))
    expect_false(anyNA(nums), info = paste(names(nums)[is.na(nums)], collapse = " "))
    expect_true(all(diff(nums) >= 0) && nums[length(nums)] > nums[1],
                info = paste(nums, collapse = " "))
    # the rows of the tables do not
    expect_false(any(grepl("^\\s*[0-9]+\\s+row[0-9]{2}", lines)))
    expect_false(any(grepl("^\\s*[0-9]+\\s+1\\. rating", lines)))
    expect_false(any(grepl("^\\s*[0-9]+\\s+(Female|Male)\\b", lines)))
    expect_match(text, "RAWROW", fixed = TRUE)
    expect_false(any(grepl("^\\s*[0-9]+\\s+RAWROW", lines)))
    expect_false(any(grepl("^\\s*[0-9]+\\s+INTOROW", lines)))
  })
}
