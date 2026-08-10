# The rendered acceptance suite for automatic table titles and notes.
#
# apa.quarto.pdf() now supplies the title and the note from the spec by
# default. `title =` / `note =` arguments override the spec, and the chunk's
# own `tbl-cap` / `apa-note` settings lines override both. R emits an empty
# ::: {.apa-table-meta} div carrying the generated values and the bundled Lua
# filter apatablemeta.lua populates whatever the float still lacks.
#
# THIS FILE IS THE ONLY AUTOMATED COVERAGE THE FEATURE HAS, and that is a
# measured statement rather than a rhetorical one. Every one of the twelve
# apaquarto-*.qmd fixtures sets `tbl-cap`, eleven of them also set `apa-note`,
# and both matrix generators in helper-apaquarto-extract.R hard-code the pair
# for every generated table. So the whole of the rest of the suite passes with
# this filter doing nothing at all: every document in it supplies by hand the
# very option the filter exists to infer. If the cells below are weak, the
# feature ships untested.
#
# Four rules follow from that, and from what this project has already paid for:
#
#   * ROUND TRIP, never a literal string. What is asserted to reach the page is
#     `as_plaintext(as_spec(x)$title)` and the spec's own notes, computed here
#     at test time. This slice rewords the generated titles; a hard-coded
#     expectation would fail later for the wrong reason and be "fixed" by
#     retyping it, which is exactly how the statistic-italics defect survived.
#   * REAL BUILDERS. apa.cor.table(), apa.reg.table(), apa.d.table(),
#     apa.aov.table() -- not a hand-built spec. The most recent silent defect
#     in this package survived because the only fixture watching was a two-row
#     synthetic spec with no p cells in it.
#   * BOTH PLACEMENTS, every cell. Tables in the text (floatsintext: true) and
#     tables collected at the end (apaquarto's default, apa7 endfloat) are both
#     binding on this package, and neither may be sacrificed for the other.
#   * SETS, compared with setdiff in both directions. A tally once undercounted
#     37 rows of 40 here.
#
# Every render is a real quarto render taking tens of seconds, so the file
# skips unless the whole toolchain is present. It must never fail a CRAN check.

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto not installed")
skip_if_not(nzchar(Sys.which("pdftotext")), "poppler (pdftotext) not installed")
skip_if_not(nzchar(Sys.which("pdfinfo")), "poppler (pdfinfo) not installed")
skip_if_not(nzchar(Sys.which("pdftoppm")), "poppler (pdftoppm) not installed")

# The spawned quarto R session runs library(apaTables) against the INSTALLED
# package -- both its R code and the apaquarto extension it ships -- so a green
# run against a stale library certifies the library and not this branch.
apa_skip_if_library_stale()

# ---------------------------------------------------------------------------
# The document under test
#
# Six floats in one document, because every measurement in this slice until now
# used exactly one, and "the title is right" is a different claim from "each of
# six titles is right and they are numbered 1 to 6". Each float is a different
# point in the precedence matrix, and each is a different real builder so that
# no single wording carries the whole file:
#
#   1 tbl-auto       nothing set          -> generated title AND generated note
#   2 tbl-capmixed   tbl-cap only         -> author's title, generated note
#                                            (THE AUTHOR'S PRIMARY CASE, and the
#                                            harder one: a user-set tbl-cap means
#                                            Quarto builds the float itself and R
#                                            has no container to hang a note on)
#   3 tbl-argmixed   title = argument     -> argument title, generated note
#   4 tbl-both       tbl-cap + apa-note,  -> the chunk wins over the argument AND
#                    AND title = / note =    over the generated value, both ways
#   5 tbl-plainnote  note = plain text    -> the italics control (see below)
#   6 tbl-rotated    tbl-cap, .landscape  -> the mixed case on a ROTATED page
#
# Row labels are markers rather than real variable names so that the survival of
# every table's rows is decidable as a SET. They are letters only: an underscore
# is math mode in LaTeX and aborts the render.
#
# The two regressions deliberately run on different numbers of cases (200 and
# 150) so their generated notes differ in their first sentence; otherwise
# "table 6's generated note reached the page" would be satisfied by table 2's.
tnd_qmd <- function(placement) {
  fmt <- if (identical(placement, "intext"))
    "format:\n  apaquarto-pdf:\n    keep-tex: true\n    floatsintext: true\n"
  else
    "format:\n  apaquarto-pdf:\n    keep-tex: true\n"

  chunk <- function(label, opts, code) paste0(
    "```{r}\n#| label: ", label, "\n", opts,
    "#| echo: false\n#| message: false\n#| warning: true\n", code, "\n```\n\n")

  paste0(
    "---\n",
    "title: \"Title And Note Defaults\"\n",
    "shorttitle: \"Defaults\"\n",
    "author:\n",
    "  - name: Test Author\n",
    "    corresponding: true\n",
    "    email: test@example.com\n",
    "    affiliations:\n",
    "      - name: Test University\n",
    fmt,
    "---\n\n",

    "```{r}\n",
    "#| label: tnd-setup\n#| echo: false\n#| message: false\n",
    "library(apaTables)\n",
    "options(contrasts = c(\"contr.sum\", \"contr.poly\"))\n",
    "data(album); data(viagra)\n",
    "d1 <- attitude[, 1:4]\n",
    "names(d1) <- c(\"markeraa\", \"markerab\", \"markerac\", \"markerad\")\n",
    "d2 <- album[, c(\"sales\", \"adverts\", \"airplay\")]\n",
    "names(d2) <- c(\"markerba\", \"markerbb\", \"markerbc\")\n",
    "d3 <- viagra\n",
    "levels(d3$dose) <- c(\"markerca\", \"markercb\", \"markercc\")\n",
    "d4 <- viagra\n",
    "names(d4) <- c(\"markerda\", \"markerdb\")\n",
    "d5 <- album[1:150, ]\n",
    "names(d5) <- c(\"markerea\", \"markereb\", \"markerec\", \"markered\")\n",
    "```\n\n",

    "## Results\n\n",
    "XREF: @tbl-auto, @tbl-capmixed, @tbl-argmixed, @tbl-both, ",
    "@tbl-plainnote, @tbl-rotated.\n\n",

    chunk("tbl-auto", "",
          "apa.cor.table(d1) |> apa.quarto.pdf()"),

    chunk("tbl-capmixed", "#| tbl-cap: \"Capmixed Author Wrote This Title\"\n",
          paste0("apa.reg.table(stats::lm(markerba ~ markerbb + markerbc, ",
                 "data = d2)) |> apa.quarto.pdf()")),

    chunk("tbl-argmixed", "",
          paste0("apa.d.table(iv = dose, dv = libido, data = d3) |> ",
                 "apa.quarto.pdf(title = \"Argmixed Title From The Argument\")")),

    chunk("tbl-both",
          paste0("#| tbl-cap: \"Chunkboth Title Wins\"\n",
                 "#| apa-note: \"Chunknoteboth wins over the argument.\"\n"),
          paste0("apa.aov.table(stats::lm(markerdb ~ markerda, data = d4)) |> ",
                 "apa.quarto.pdf(title = \"Argboth Title Loses\", ",
                 "note = \"Argnoteboth loses.\")")),

    # The italics control, and the only reason it exists: a dropped italic flag
    # leaves the note's PLAINTEXT byte-identical, so a plaintext assertion is
    # blind to the whole defect class -- the one that printed 21 upright p
    # values in this package last week. This float carries the SAME note text as
    # tbl-auto with the markup stripped, so the two floats differ in exactly one
    # thing. The assertion that can see it is asserted, and the assertion that
    # cannot is asserted TOO, as a standing demonstration of its blindness.
    # The float is a ONE-WAY table, not a second correlation table, and that is
    # about cell 5 rather than about this cell: two floats carrying the same
    # generated title would make a swap between exactly those two invisible to
    # the (number, title) pair assertion. The note is still the CORRELATION
    # table's own note with the markup stripped, because that is what makes the
    # italics comparison exact -- the same words and the same statistic symbols
    # as tbl-auto's note, differing in nothing but the markup.
    chunk("tbl-plainnote", "",
          paste0("tnd5 <- apa.cor.table(d1)\n",
                 "apa.1way.table(iv = dose, dv = libido, data = d3) |>\n",
                 "  apa.quarto.pdf(note = paste(\"Plainnotemarker.\",\n",
                 "  apaTables:::as_plaintext(apaTables:::as_spec(tnd5)$notes[[1]])))")),

    "::: {.landscape}\n\n",
    chunk("tbl-rotated", "#| tbl-cap: \"Rotatedcap Author Wrote This Title\"\n",
          paste0("apa.reg.table(stats::lm(markerea ~ markereb + markerec + ",
                 "markered, data = d5)) |> apa.quarto.pdf()")),
    ":::\n\n",

    # The trailing body paragraph is what makes placement decidable. A marker
    # BEFORE the tables cannot decide it: under endfloat the first deferred
    # table lands on the page right after the body, so every comparison against
    # it is true in both placements.
    "ENDMARKER closes the body of this manuscript, after every table.\n")
}

# ---------------------------------------------------------------------------
# Reading the artefact back

# pdftotext reports LaTeX's hyphenation, so a note that happened to break
# across a line reads "indi- cates" and "calcu- lated" -- both MEASURED on this
# very document. Without this, a whole-sentence round-trip match on a note is a
# coin toss on where the line broke, and the natural "fix" is to shorten the
# expectation until it stops failing, which is how an assertion stops being
# able to observe anything. The wanted string is put through the same
# normalisation, where it is a no-op.
tnd_flatten <- function(x) {
  s <- apa_nbsp_to_space(paste(x, collapse = "\n"))
  s <- gsub("([a-z])-[[:space:]]+([a-z])", "\\1\\2", s)
  gsub("[[:space:]]+", " ", s)
}

# The spec's title and its notes as the reader will meet them, joined the way
# apa_meta_div_for() joins them.
tnd_title <- function(x) {
  ttl <- as_spec(x)$title
  if (inherits(ttl, "apa_richtext")) as_plaintext(ttl) else as.character(ttl)
}
tnd_note <- function(x) {
  paste(vapply(as_spec(x)$notes, as_plaintext, character(1)), collapse = " ")
}

# One float's LaTeX, keyed by its label.
#
# The negatives in this file are asked of the .tex rather than of the page, and
# deliberately: the PDF has no per-float boundaries, two floats routinely share
# a page in the floatsintext document (measured: tables 2 and 3 land together),
# and "this string is absent from the page" would then be answerable by the
# neighbour rather than by the float under test. Every POSITIVE is still asked
# of the rendered page.
tnd_float_tex <- function(tex, label) {
  blocks <- strsplit(tnd_flatten(tex), "\\begin{longtable}", fixed = TRUE)[[1]]
  hit <- blocks[grepl(paste0("\\label{", label, "}"), blocks, fixed = TRUE)]
  if (!length(hit)) "" else hit[1]
}

# The note portion of one float's LaTeX, and ONLY that: from the "Note." run to
# \endlastfoot, which is where a longtable's note ends.
#
# Both cuts matter and neither is tidiness. A correlation table sets \textit{N},
# \textit{M} and \textit{SD} in its column headings and \textit{p} in its body
# cells, so a region that reached either end would let the TABLE answer an
# italics question that was asked of the NOTE -- an assertion passing on
# evidence from the wrong place, which is the failure mode this whole file is
# built against.
tnd_note_tex <- function(tex, label) {
  blk <- tnd_float_tex(tex, label)
  i <- regexpr("\\emph{Note.}", blk, fixed = TRUE)
  if (i < 0L) return("")
  blk <- substr(blk, i, nchar(blk))
  j <- regexpr("\\endlastfoot", blk, fixed = TRUE)
  if (j < 0L) blk else substr(blk, 1L, j - 1L)
}

# HOW MANY PARAGRAPHS one float's note is set in.
#
# This exists because tnd_note() above cannot see the answer, and neither can
# any other cell in this file. tnd_note() derives its expectation by joining
# the spec's entries with a space -- which is what the production code did --
# and the page text is flattened before comparison, so a note printed as one
# paragraph and the same note printed as three are the SAME STRING to every
# other assertion here. 141 of them passed while apa.cor.table() and
# apa.reg.table(), the two most-used builders, printed their significance-star
# sentence run into the body of the note instead of on its own line.
#
# Asked of the UNFLATTENED .tex on purpose: tnd_float_tex() flattens, and
# flattening is precisely what destroys the evidence. A blank line is what
# makes a new paragraph in LaTeX, so the count is one more than the number of
# blank lines between the "Note." run and \endlastfoot.
tnd_note_paragraphs <- function(tex, label) {
  raw <- paste(tex, collapse = "\n")
  blocks <- strsplit(raw, "\\begin{longtable}", fixed = TRUE)[[1]]
  hit <- blocks[grepl(paste0("\\label{", label, "}"), blocks, fixed = TRUE)]
  if (!length(hit)) return(NA_integer_)
  blk <- hit[1]
  i <- regexpr("\\emph{Note.}", blk, fixed = TRUE)
  if (i < 0L) return(NA_integer_)
  blk <- substr(blk, i, nchar(blk))
  j <- regexpr("\\endlastfoot", blk, fixed = TRUE)
  if (j > 0L) blk <- substr(blk, 1L, j - 1L)
  m <- gregexpr("\n[ \t]*\n", blk)[[1]]
  1L + if (m[1] == -1L) 0L else length(m)
}

# A LaTeX note reduced to the words a reader would read off the page: the
# italic wrappers removed, the ties and the escapes undone. This is what makes
# the blindness of a plaintext check demonstrable rather than merely asserted
# -- put the italic note and the markup-free one through it and the two come
# out IDENTICAL, which is the whole defect class in one line.
tnd_detex <- function(x) {
  s <- gsub("\\\\emph\\{([^{}]*)\\}", "\\1", x)
  s <- gsub("\\textless{}", "<", s, fixed = TRUE)
  s <- gsub("\\%", "%", s, fixed = TRUE)
  s <- gsub("~", " ", s, fixed = TRUE)
  tnd_flatten(s)
}

# Caption number + the title printed under it, in page order.
#
# Reading the pair rather than the number alone is what makes this able to see
# a title attached to the wrong table: six correct numbers over six shuffled
# titles is a green tally and a broken document.
tnd_caption_pairs <- function(pages) {
  lines <- unlist(strsplit(apa_nbsp_to_space(paste(pages, collapse = "\n")), "\n"))
  i <- grep("^\\s*Table\\s+[0-9]+\\s*$", lines)
  data.frame(
    number = as.integer(sub("^\\s*Table\\s+([0-9]+)\\s*$", "\\1", lines[i])),
    title  = trimws(lines[pmin(i + 1L, length(lines))]),
    stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# The renders. One per placement, cached, so ten cells cost two renders.
#
# apa.quarto.new() COPIES the bundled extension into the project, so a reused
# directory would render an old copy: each placement gets a directory of its
# own and it is removed first.

tnd_cache <- new.env(parent = emptyenv())

tnd_render <- function(placement) {
  key <- paste0("r-", placement)
  if (!is.null(tnd_cache[[key]])) return(tnd_cache[[key]])

  proj <- file.path(tempdir(), paste0("tnd-", placement))
  unlink(proj, recursive = TRUE)
  suppressMessages(apa.quarto.new(proj, filename = "doc.qmd"))
  writeLines(tnd_qmd(placement), file.path(proj, "doc.qmd"))

  log <- suppressWarnings(system2(
    "quarto", c("render", shQuote(file.path(proj, "doc.qmd"))),
    stdout = TRUE, stderr = TRUE))
  pdf <- file.path(proj, "doc.pdf")
  # stop(), not skip(): a document that would not compile is the loudest
  # possible failure of this feature, and a skip prints in the same summary
  # line as a pass.
  if (!file.exists(pdf))
    stop("the ", placement, " document did not render:\n",
         paste(apa_strip_ansi(log), collapse = "\n"))

  res <- list(
    proj  = proj,
    log   = apa_strip_ansi(log),
    pages = apa_pdf_pages(pdf),
    tex   = readLines(file.path(proj, "doc.tex"), warn = FALSE),
    pdf   = pdf)
  res$flat <- tnd_flatten(res$pages)
  # The same text with its LINE STRUCTURE intact. res$flat cannot answer a
  # question about which line something is on, because collapsing whitespace is
  # exactly what erases the answer -- see tnd_note_paragraphs().
  res$flat_lines <- apa_nbsp_to_space(paste(res$pages, collapse = "\n"))
  tnd_cache[[key]] <- res
  res
}

# The specs the document builds, rebuilt here so every expectation is a ROUND
# TRIP through the same code the document ran rather than a transcription of
# what it once printed.
tnd_specs <- function() {
  if (!is.null(tnd_cache$specs)) return(tnd_cache$specs)
  op <- options(contrasts = c("contr.sum", "contr.poly"))
  on.exit(options(op), add = TRUE)
  album <- NULL; viagra <- NULL
  utils::data("album", "viagra", package = "apaTables",
              envir = environment())
  d1 <- datasets::attitude[, 1:4]
  names(d1) <- c("markeraa", "markerab", "markerac", "markerad")
  d2 <- album[, c("sales", "adverts", "airplay")]
  names(d2) <- c("markerba", "markerbb", "markerbc")
  d3 <- viagra
  levels(d3$dose) <- c("markerca", "markercb", "markercc")
  d4 <- viagra
  names(d4) <- c("markerda", "markerdb")
  d5 <- album[1:150, ]
  names(d5) <- c("markerea", "markereb", "markerec", "markered")
  out <- list(
    cor = apa.cor.table(d1),
    reg = apa.reg.table(stats::lm(markerba ~ markerbb + markerbc, data = d2)),
    dv  = apa.d.table(iv = dose, dv = libido, data = d3),
    aov = apa.aov.table(stats::lm(markerdb ~ markerda, data = d4)),
    one = apa.1way.table(iv = dose, dv = libido, data = d3),
    rot = apa.reg.table(stats::lm(markerea ~ markereb + markerec + markered,
                                  data = d5)))
  tnd_cache$specs <- out
  out
}

# Every row label the six tables put on the page. markerba, markerda and
# markerea are criteria/outcomes, which appear only inside a generated TITLE
# (title-cased, so out of this lower-case set) and never as a row.
TND_ROW_MARKERS <- c("markeraa", "markerab", "markerac", "markerad",
                     "markerbb", "markerbc",
                     "markerca", "markercb", "markercc",
                     "markerda",
                     "markereb", "markerec", "markered")

for (placement in c("end", "intext")) local({

  pl <- placement
  where <- if (identical(pl, "intext")) "in the text" else "collected at the end"

  # ---- Cell 1: nothing supplied at all -------------------------------------
  test_that(paste0("fully automatic (", where,
                   "): the generated title AND note both reach the page"), {
    r <- tnd_render(pl)
    ct <- tnd_specs()$cor
    expect_true(grepl(tnd_flatten(tnd_title(ct)), r$flat, fixed = TRUE),
                info = tnd_title(ct))
    # The WHOLE note, not a 40-character prefix of it: the filter sets one
    # attribute, so a truncating defect would be invisible to a prefix match.
    expect_true(grepl(tnd_flatten(tnd_note(ct)), r$flat, fixed = TRUE),
                info = tnd_note(ct))
    # Attached to its own table, not merely present in the document. With the
    # note-placement defects this project has already had, a stranded note is
    # still a note that exists somewhere.
    cap_page <- apa_page_of(r$pages, tnd_title(ct))
    note_page <- apa_page_of(r$pages, "number of cases")
    expect_false(is.na(cap_page))
    expect_false(is.na(note_page))
    expect_equal(note_page, cap_page)
  })

  # ---- Cell 2: THE AUTHOR'S PRIMARY CASE -----------------------------------
  test_that(paste0("mixed, the primary case (", where,
                   "): the author's tbl-cap survives and the note is generated"), {
    r <- tnd_render(pl)
    rg <- tnd_specs()$reg
    expect_true(grepl("Capmixed Author Wrote This Title", r$flat, fixed = TRUE))
    expect_true(grepl(tnd_flatten(tnd_note(rg)), r$flat, fixed = TRUE),
                info = tnd_note(rg))
    # ...and the generated title did NOT also appear. Asked of this float's own
    # LaTeX, so a neighbour cannot answer for it.
    blk <- tnd_float_tex(r$tex, "tbl-capmixed")
    expect_true(nzchar(blk))
    expect_true(grepl("Capmixed Author Wrote This Title", blk, fixed = TRUE))
    # Flattened for the same reason as cell 3's, though this title carries no
    # hyphen today: an expectation that does not go through the same pipe as
    # the text is one rewording away from matching nothing at all.
    expect_false(grepl(tnd_flatten(tnd_title(rg)), blk, fixed = TRUE),
                 info = tnd_title(rg))
  })

  # ---- Cell 3: the argument, with the note still generated ------------------
  test_that(paste0("mixed via the argument (", where,
                   "): title = wins over the generated title, note still generated"), {
    r <- tnd_render(pl)
    dv <- tnd_specs()$dv
    expect_true(grepl("Argmixed Title From The Argument", r$flat, fixed = TRUE))
    expect_true(grepl(tnd_flatten(tnd_note(dv)), r$flat, fixed = TRUE),
                info = tnd_note(dv))
    blk <- tnd_float_tex(r$tex, "tbl-argmixed")
    expect_true(nzchar(blk))
    # FLATTENED, like every positive in this file, and this negative is the one
    # that needs it: the d-table's generated title is "Means, Standard
    # Deviations, and d-Values for ...", so if pandoc ever wraps at that hyphen
    # the raw expectation stops matching anything the .tex could contain and
    # the negative goes VACUOUSLY true. tnd_flatten()'s un-hyphenator wants
    # lower case on both sides of the hyphen, so it would not repair "d-Values"
    # after the fact -- the expectation has to go through the same pipe as the
    # text it is compared against.
    expect_false(grepl(tnd_flatten(tnd_title(dv)), blk, fixed = TRUE),
                 info = tnd_title(dv))
  })

  # ---- Cell 4: everything supplied, the full precedence ladder -------------
  test_that(paste0("both supplied (", where,
                   "): the chunk options beat the arguments and the generated values"), {
    r <- tnd_render(pl)
    av <- tnd_specs()$aov
    expect_true(grepl("Chunkboth Title Wins", r$flat, fixed = TRUE))
    expect_true(grepl("Chunknoteboth wins over the argument.", r$flat, fixed = TRUE))
    # The arguments lost. These two strings occur nowhere else in the document,
    # so absence from the whole PDF is the honest question to ask of them.
    expect_false(grepl("Argboth Title Loses", r$flat, fixed = TRUE))
    expect_false(grepl("Argnoteboth loses.", r$flat, fixed = TRUE))
    # ...and so did the generated pair, asked of this float's own LaTeX.
    blk <- tnd_float_tex(r$tex, "tbl-both")
    expect_true(nzchar(blk))
    expect_false(grepl(tnd_flatten(tnd_title(av)), blk, fixed = TRUE),
                 info = tnd_title(av))
    expect_false(grepl("Sum of squares", blk, fixed = TRUE))
    expect_false(grepl("Argboth", blk, fixed = TRUE))
    expect_false(grepl("Argnoteboth", blk, fixed = TRUE))
  })

  # ---- Cell 5: six floats, numbered, each with ITS OWN title ---------------
  test_that(paste0("several floats (", where,
                   "): numbering, cross-references and per-table titles"), {
    r <- tnd_render(pl)
    ct <- tnd_specs()$cor
    expect_equal(apa_pdf_caption_numbers(r$pages), 1:6)
    expect_equal(apa_xref_numbers(r$pages), 1:6)
    # The pair, not the tally. Six right numbers over six shuffled titles is a
    # green count and a broken manuscript.
    pairs <- tnd_caption_pairs(r$pages)
    expect_equal(pairs$number, 1:6)
    want <- c(tnd_title(ct),
              "Capmixed Author Wrote This Title",
              "Argmixed Title From The Argument",
              "Chunkboth Title Wins",
              tnd_title(tnd_specs()$one),
              "Rotatedcap Author Wrote This Title")
    # All six DISTINCT, asserted rather than assumed. Two floats sharing a
    # title would make a swap between exactly those two invisible to the pair
    # check below -- the assertion would still be green with the document
    # wrong. tbl-plainnote is a one-way table for this reason alone.
    expect_length(unique(want), 6L)
    expect_equal(pairs$title, want)
  })

  # ---- Cell 6: the mixed case on a ROTATED page ---------------------------
  test_that(paste0("rotated mixed case (", where,
                   "): the generated note reaches the rotated page"), {
    r <- tnd_render(pl)
    rt <- tnd_specs()$rot
    expect_true(grepl("Rotatedcap Author Wrote This Title", r$flat, fixed = TRUE))
    expect_true(grepl(tnd_flatten(tnd_note(rt)), r$flat, fixed = TRUE),
                info = tnd_note(rt))
    cap_page <- apa_page_of(r$pages, "Rotatedcap Author Wrote This Title")
    note_page <- apa_page_of(r$pages, "N = 150")
    expect_false(is.na(cap_page))
    expect_false(is.na(note_page))
    expect_equal(note_page, cap_page)
    # Rotation is decided by RASTERIZING: every cheap textual way of asking a
    # PDF whether a page is turned has been measured lying in this project.
    shapes <- apa_pdf_page_shapes(r$pdf)
    expect_length(shapes, length(r$pages))
    expect_equal(shapes[cap_page], "landscape")
    # ...and only that one turned, so a document that started rotating
    # everything is a failure too.
    auto_page <- apa_page_of(r$pages, tnd_title(tnd_specs()$cor))
    expect_false(is.na(auto_page))
    expect_equal(shapes[auto_page], "portrait")
  })

  # ---- Cell 7: the markup survived, and the proof that this can see it -----
  test_that(paste0("the note renders as italic markup (", where,
                   "), and the plaintext check is shown to be blind to that"), {
    r <- tnd_render(pl)
    auto  <- tnd_note_tex(r$tex, "tbl-auto")
    plain <- tnd_note_tex(r$tex, "tbl-plainnote")
    expect_true(nzchar(auto))
    expect_true(nzchar(plain))

    # The generated note is set in italics where a statistic symbol appears.
    for (sym in c("\\emph{N}", "\\emph{M}", "\\emph{SD}", "\\emph{p}"))
      expect_true(grepl(sym, auto, fixed = TRUE), info = sym)

    # tbl-plainnote carries the SAME note with the markup stripped -- the
    # defect, constructed. The italics assertion goes false on it...
    for (sym in c("\\emph{N}", "\\emph{M}", "\\emph{SD}"))
      expect_false(grepl(sym, plain, fixed = TRUE), info = sym)
    # ...while the plaintext of the two notes is the same, so a plaintext
    # assertion answers TRUE for both. That is not an aside: it is the whole
    # reason the italics assertion has to exist, kept here where it can be
    # read rather than in a report nobody opens.
    same <- "N = number of cases. M = mean. SD = standard deviation."
    expect_true(grepl(same, tnd_detex(auto), fixed = TRUE),
                info = tnd_detex(auto))
    expect_true(grepl(same, tnd_detex(plain), fixed = TRUE),
                info = tnd_detex(plain))
    expect_true(grepl("Plainnotemarker.", plain, fixed = TRUE))

    # And on the page: markdown was interpreted, not printed. A filter that
    # set the attribute as literal text would leave "*N*" standing here.
    for (sym in c("*N*", "*M*", "*SD*", "*p*"))
      expect_false(grepl(sym, r$flat, fixed = TRUE), info = sym)
  })

  # ---- Cell 7b: a multi-part note is SEVERAL paragraphs -------------------
  test_that(paste0("a generated multi-part note keeps each part on its own line (",
                   where, ")"), {
    r <- tnd_render(pl)
    ct <- tnd_specs()$cor
    # The premise, asserted rather than assumed: a correlation table's note
    # really does have two entries, the second being the star sentence. If a
    # future spec merges them this test must fail loudly rather than quietly
    # start asking for one paragraph.
    expect_equal(length(as_spec(ct)$notes), 2L)
    expect_true(grepl("indicates", as_plaintext(as_spec(ct)$notes[[2]]),
                      fixed = TRUE))
    # Two entries -> two paragraphs. This is the whole cell.
    expect_equal(tnd_note_paragraphs(r$tex, "tbl-auto"), 2L)
    # The regression's two most-used builders, so the count is not carried by
    # one table: apa.reg.table() also generates a two-part note.
    expect_equal(length(as_spec(tnd_specs()$reg)$notes), 2L)
    expect_equal(tnd_note_paragraphs(r$tex, "tbl-capmixed"), 2L)
    expect_equal(tnd_note_paragraphs(r$tex, "tbl-rotated"), 2L)

    # THE CONTROL, and it is what makes the counter's answer worth reading. A
    # counter stuck at 2 would pass every line above. tbl-plainnote's note is
    # supplied as ONE string and tbl-both's comes from a single-valued chunk
    # option, so both must come back as 1 -- the collapsed shape, constructed,
    # standing beside the correct one in the same document.
    expect_equal(tnd_note_paragraphs(r$tex, "tbl-plainnote"), 1L)
    expect_equal(tnd_note_paragraphs(r$tex, "tbl-both"), 1L)

    # And on the PAGE the reader reads, not only in the .tex: the star sentence
    # begins a line, and that line does not also carry the end of the sentence
    # before it. Under the collapsed shape the two sat together on one line
    # ("...the 95% confidence interval. * indicates p < .05." -- measured).
    lines <- unlist(strsplit(r$flat_lines, "\n", fixed = TRUE))
    star <- grep("^\\s*\\* indicates", lines, value = TRUE)
    expect_gt(length(star), 0L)
    expect_false(any(grepl("confidence interval", star, fixed = TRUE)))
  })

  # ---- Cell 8: no rows were lost ------------------------------------------
  test_that(paste0("every table kept its rows (", where, ")"), {
    r <- tnd_render(pl)
    found <- unique(unlist(regmatches(
      r$pages, gregexpr("marker[a-z]{2}", r$pages))))
    # Both directions, as SETS. A tally once undercounted 37 of 40 here: a row
    # duplicated by a repeated header covers for a row that vanished.
    expect_equal(sort(setdiff(TND_ROW_MARKERS, found)), character(0))
    expect_equal(sort(setdiff(found, TND_ROW_MARKERS)), character(0))
    # The environment that can span pages, unstarred. A starred longtable* is
    # how rows have gone missing here before, silently and with exit 0.
    tex <- paste(r$tex, collapse = "\n")
    expect_equal(length(gregexpr("\\begin{longtable}", tex, fixed = TRUE)[[1]]), 6L)
    expect_false(grepl("\\begin{longtable*}", tex, fixed = TRUE))
  })

  # ---- Cell 9: placement, which is binding on this package ----------------
  test_that(paste0("placement is honoured (", where, ")"), {
    r <- tnd_render(pl)
    ct <- tnd_specs()$cor
    end_page <- apa_page_of(r$pages, "ENDMARKER")
    expect_false(is.na(end_page))
    caps <- c(tnd_title(ct), "Capmixed Author Wrote This Title",
              "Argmixed Title From The Argument", "Chunkboth Title Wins",
              "Rotatedcap Author Wrote This Title")
    tbl_pages <- vapply(caps, function(s) apa_page_of(r$pages, s), integer(1))
    expect_false(any(is.na(tbl_pages)))
    # The asymmetry is the honest reading of each requirement, not an oversight.
    # "Collected at the end" is a claim about EVERY table. "In the text" is not
    # its mirror: LaTeX may legitimately carry a float that will not fit past
    # the closing paragraph, so any() -- and endfloat cannot satisfy any(),
    # because with floats delayed no table precedes the whole body.
    if (identical(pl, "end")) {
      expect_true(all(tbl_pages > end_page),
                  info = paste(tbl_pages, collapse = " "))
    } else {
      expect_true(any(tbl_pages < end_page),
                  info = paste(tbl_pages, collapse = " "))
    }
  })

  # ---- Cell 10: nothing leaked into the manuscript ------------------------
  test_that(paste0("the meta div leaves no artefact and nothing is warned (",
                   where, ")"), {
    r <- tnd_render(pl)
    # R emits ::: {.apa-table-meta apa-title="..." apa-note="..."} on every
    # LaTeX render. It is a carrier, and a carrier that prints is a defect the
    # author would meet in a finished paper.
    for (leak in c("apa-table-meta", "apa-title=", "apa-note=", ":::"))
      expect_false(grepl(leak, r$flat, fixed = TRUE), info = leak)
    # knitr writes an R warning INTO the document, never onto quarto's console,
    # so the reader of the PDF is who meets it and the PDF is where it is
    # looked for.
    expect_false(any(grepl("Warning:", r$pages, fixed = TRUE)),
                 info = paste(grep("Warning:", unlist(strsplit(r$pages, "\n")),
                                   value = TRUE), collapse = " | "))
    # Quarto prints ERROR and still produces a PDF, so nothing else catches it.
    expect_false(any(grepl("^\\s*ERROR", r$log)))
  })
})

# ---------------------------------------------------------------------------
# Plain Quarto (no extension).
#
# An ordinary `format: pdf` document renders with no apaquarto extension at
# all, so apatablemeta.lua never runs and the ::: {.apa-table-meta} div
# SURVIVES into pandoc's output. That is the one kind of document where the
# carrier could print, and nothing else in the suite looks. It is checked as
# an artefact question -- does anything from the div reach the page -- rather
# than as a note question, because with no filter there is nothing to
# populate the note. The vehicle used to be the shipped
# example-without-apaquarto.qmd twin; that file was retired (the example
# twins are now -pdf/-docx, both apaquarto), so the document is built inline
# here -- one table is all the property needs.
test_that("plain Quarto: the meta div reaches the page as nothing at all", {
  tmp <- tempfile("tnd-plain"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  pkg <- Sys.getenv("APATABLES_PKG_ROOT",
                    "/Users/dstanley/os/RDevelopment/apaTables")
  qmd <- file.path(tmp, "plain.qmd")

  # keep-md is what turns the assertions below from advertised coverage into
  # coverage. Without the intermediate markdown there is no way to show the
  # div was ever emitted into THIS document, and "nothing from the div
  # reached the page" would read exactly the same if apa.quarto.pdf() had
  # quietly stopped emitting it -- an absence assertion that cannot fail.
  # With it, the positive control below fires first.
  writeLines(c(
    "---",
    'title: "Plain Quarto"',
    "format: pdf",
    "keep-md: true",
    "---",
    "",
    "```{r}",
    "#| include: false",
    sprintf('pkgload::load_all("%s", quiet = TRUE)', pkg),
    "```",
    "",
    "```{r}",
    "#| label: tbl-correlation",
    '#| tbl-cap: "Descriptive Statistics and Correlations"',
    "#| message: false",
    "#| warning: true",
    "my_table <- apa.cor.table(attitude)",
    "apa.quarto.pdf(my_table)",
    "```"), qmd)

  log <- suppressWarnings(system2("quarto", c("render", qmd),
                                  stdout = TRUE, stderr = TRUE))
  pdf <- file.path(tmp, "plain.pdf")
  expect_true(file.exists(pdf), info = paste(apa_strip_ansi(log), collapse = "\n"))
  skip_if_not(file.exists(pdf), "render produced no output to read")

  # POSITIVE CONTROL. Plain Quarto's `format: pdf` is a LaTeX target, so
  # apa.quarto.pdf() emits the meta div here exactly as it does under
  # apaquarto -- and no filter exists to consume it, which is what makes this
  # the one kind of document where the carrier could print.
  md <- file.path(tmp, "plain.pdf.md")
  expect_true(file.exists(md))
  skip_if_not(file.exists(md), "no intermediate markdown to read")
  mdtxt <- paste(readLines(md, warn = FALSE), collapse = "\n")
  expect_true(grepl("::: {.apa-table-meta", mdtxt, fixed = TRUE))

  txt <- tnd_flatten(apa_pdf_pages(pdf))
  for (leak in c("apa-table-meta", "apa-title=", "apa-note=",
                 "number of cases"))
    expect_false(grepl(leak, txt, fixed = TRUE), info = leak)
  # And the document is still a document: the table itself rendered, with
  # Quarto's own caption style.
  expect_true(grepl("Table 1", txt, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Task 7: title and note on the markdown (docx) path.
#
# Everything above this line is the PDF acceptance suite Task 1 of the
# title/note slice built, and its own banner already states the coverage
# trap this task inherits verbatim: every fixture in THIS file sets
# tbl-cap, and most set apa-note too, so a cell that does either would look
# covered and prove nothing about the default. The fixture below
# (fixtures/title-note-defaults-docx.qmd) is built to set NEITHER, on a
# `tbl-` labelled chunk that calls apa_quarto_markdown_block() directly --
# the same way the existing landscape-*.qmd fixtures already exercise that
# function, and the only way to reach the docx path before Task 8 wires
# apa.quarto.pdf() itself over to it.
#
# apatablemeta.lua (inst/_extensions/wjschne/apaquarto/apatablemeta.lua) is
# registered `at: post-quarto` in the COMMON filter list (_extension.yml),
# not inside the docx-only block -- so the SAME filter that already serves
# the PDF path above serves this one too, once R hands it a meta div to
# read. Nothing about the filter or the precedence decision (D22) is
# reimplemented here; only apa_quarto_markdown_block() itself changes, to
# emit the div the way apa_quarto_latex_block()'s caller already does.
skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto not installed")
apa_skip_if_library_stale()

# apa_docx_text() strips tags but does not decode XML entities -- never
# exercised before this task, because no earlier caller in this suite
# compared it against text containing "<", "&" or ">". apa.cor.table()'s
# significance-star note does ("indicates *p* < .05"), so word/document.xml's
# literal "&lt;" would otherwise make the note compare unequal to itself.
# Local to this file rather than a change to the shared helper, since this is
# the first caller that needs it.
tnd_docx_text <- function(d) {
  x <- apa_docx_text(d)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

# One float's text, from its own "Table k" line up to (but not including)
# the next float's "Table k+1" line or the end of the document -- so a
# per-float assertion cannot be satisfied by a NEIGHBOUR float's caption or
# note, the same discipline tnd_float_tex() applies on the PDF side above
# (blocks split on \begin{longtable}) and for the identical reason: two of
# the four floats below are correlation tables with a generated note of the
# same SHAPE, so a document-wide substring check could not tell "this
# float's own note arrived" apart from "some OTHER float's note happens to
# be readable from here too".
tnd_docx_floats <- function(txt) {
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  starts <- grep("^Table [0-9]+$", trimws(lines))
  ends <- c(starts[-1] - 1L, length(lines))
  setNames(
    lapply(seq_along(starts),
          function(i) paste(lines[starts[i]:ends[i]], collapse = "\n")),
    trimws(lines[starts]))
}

test_that("apa_quarto_markdown_block() proves all three precedence cases, plus the pure-default one, on a real docx render", {
  # Four floats, one render (each `quarto render` is tens of seconds -- see
  # this file's own banner -- so the four cases share it rather than each
  # paying for its own). fixtures/title-note-defaults-docx.qmd:
  #   tbl-auto      nothing set                    -> generated title AND note
  #   tbl-capmixed  tbl-cap only                    -> own title, generated note
  #                                                     (THE AUTHOR'S PRIMARY CASE)
  #   tbl-argmixed  title = argument, no tbl-cap    -> argument title, generated note
  #   tbl-both      tbl-cap + apa-note, AND         -> the chunk wins over the
  #                 title =/note = arguments            argument AND the generated
  #                                                      value, both ways
  d <- apa_render_fixture_docx("title-note-defaults-docx")
  styles <- apa_docx_styles(d)
  txt <- apa_nbsp_to_space(tnd_docx_text(d))
  floats <- tnd_docx_floats(txt)
  expect_equal(names(floats), paste("Table", 1:4))

  # Document-wide furniture count: FOUR captioned floats. The FigureNote
  # total (cases 1-3's generated two-part notes plus case 4's single
  # hand-written one) is asserted below, once each case's own note is known.
  expect_equal(sum(styles == "Caption"), 4L, info = paste(styles, collapse = ", "))

  # ---- Case 1: package default only (neither tbl-cap nor apa-note) -------
  x1 <- apa.cor.table(attitude[, 1:3])
  sp1 <- as_spec(x1)
  expect_length(sp1$notes, 2)  # the fixture's own claim of "two-part note"
  title1 <- tnd_title(x1)
  notes1 <- vapply(sp1$notes, as_plaintext, character(1))
  expect_true(grepl(title1, floats[["Table 1"]], fixed = TRUE))
  for (n in notes1) expect_true(grepl(n, floats[["Table 1"]], fixed = TRUE), info = n)

  # ---- Case 2: chunk option only -- tbl-cap set, apa-note absent (THE ----
  # ---- AUTHOR'S PRIMARY CASE: own title, generated note) -----------------
  x2 <- apa.reg.table(stats::lm(rating ~ complaints, data = attitude))
  sp2 <- as_spec(x2)
  notes2 <- vapply(sp2$notes, as_plaintext, character(1))
  expect_true(grepl("Capmixed Author Wrote This Title", floats[["Table 2"]],
                    fixed = TRUE))
  # The GENERATED title must NOT be the one that printed -- a title that
  # merely CONTAINS both the chunk's and the generated wording would let a
  # union bug pass this cell.
  expect_false(grepl(tnd_title(x2), floats[["Table 2"]], fixed = TRUE))
  for (n in notes2) expect_true(grepl(n, floats[["Table 2"]], fixed = TRUE), info = n)

  # ---- Case 3: the title argument (no tbl-cap) -- argument title, --------
  # ---- generated note ------------------------------------------------------
  x3 <- apa.cor.table(attitude[, 2:4])
  sp3 <- as_spec(x3)
  notes3 <- vapply(sp3$notes, as_plaintext, character(1))
  expect_true(grepl("Argmixed Title From The Argument", floats[["Table 3"]],
                    fixed = TRUE))
  expect_false(grepl(tnd_title(x3), floats[["Table 3"]], fixed = TRUE))
  for (n in notes3) expect_true(grepl(n, floats[["Table 3"]], fixed = TRUE), info = n)

  # ---- Case 4: tbl-cap + apa-note, AND title =/note = arguments -- the ---
  # ---- chunk wins over the ARGUMENT and over the GENERATED value, both ---
  # ---- ways round ----------------------------------------------------------
  expect_true(grepl("Chunkboth Title Wins", floats[["Table 4"]], fixed = TRUE))
  expect_true(grepl("Chunknoteboth wins over the argument.", floats[["Table 4"]],
                    fixed = TRUE))
  expect_false(grepl("Argboth Title Loses", floats[["Table 4"]], fixed = TRUE))
  expect_false(grepl("Argnoteboth loses.", floats[["Table 4"]], fixed = TRUE))
  x4 <- apa.cor.table(attitude[, 3:5])
  expect_false(grepl(tnd_title(x4), floats[["Table 4"]], fixed = TRUE))

  # Per-float FigureNote counts, cross-checked against the document-wide
  # total: cases 1-3 each generate a TWO-part note; case 4's chunk apa-note
  # is a single hand-written string, so it must be exactly ONE paragraph --
  # a space-joined multi-part note would ALSO be one paragraph, so this
  # alone would not catch that defect, which is why cases 1-3's per-part
  # substring checks above are the ones that do (D34's trap).
  expect_equal(sum(styles == "FigureNote"), 7L, info = paste(styles, collapse = ", "))

  # No leftover marker text anywhere in the document.
  for (leak in c("apa-table-meta", "apa-title=", "apa-note="))
    expect_false(grepl(leak, txt, fixed = TRUE), info = leak)

  # The document is still a document.
  expect_true(grepl("ENDMARKER", txt, fixed = TRUE))
})
