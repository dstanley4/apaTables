# How much of the page a table actually has.
#
# Every number in this file was READ OUT OF LaTeX rather than assumed. Each
# configuration below was rendered with a probe document whose body is the
# raw LaTeX line
#
#     \the\linewidth \the\textheight \the\paperwidth \the\paperheight
#
# and the values were read back out of the resulting PDF. The quantity that
# decides whether a table is clipped is \linewidth (equal to \columnwidth in
# ordinary body text and inside a table float), NOT \textwidth: in
# apaquarto's journal mode \textwidth is the whole two-column spread while a
# table sits in ONE column, less than half as wide.
#
# Measurements (apaquarto 5.0.18, quarto 1.9.38, xelatex; 1 in = 72.27 pt):
#
#   apaquarto-pdf, documentmode man (also the default) or stu, US letter
#       linewidth  469.75502 pt = 6.500 in    textheight 650.43001 pt = 9.000 in
#   apaquarto-pdf, documentmode man/stu, a4paper: true
#       linewidth  452.96790 pt = 6.268 in    textheight 700.50687 pt = 9.693 in
#   apaquarto-pdf, documentmode jou (US letter AND a4paper -- apa7 fixes the
#   journal text block, so the paper size does not change it)
#       columnwidth 241.74324 pt = 3.345 in   textheight 643.20255 pt = 8.900 in
#       linewidth inside the ThreePartTable a table is put in:
#                   240.74376 pt = 3.331 in
#   apaquarto-pdf, documentmode jou, chunk option apa-twocolumn: true
#       linewidth  501.55397 pt = 6.940 in    textheight 643.20255 pt = 8.900 in
#   plain format: pdf, US letter, no geometry:
#       linewidth  446.76004 pt = 6.182 in    textheight 582.20026 pt = 8.056 in
#   plain format: pdf, a4, no geometry:
#       linewidth  434.55125 pt = 6.013 in    textheight 623.00027 pt = 8.620 in
#
# Two further facts came out of the same probes and are used below:
#
#   * a "margin=X" geometry entry is exact arithmetic on the paper, in both
#     plain Quarto and apaquarto (which applies the document's geometry after
#     its own inner=1in, outer=1in). Verified: apaquarto man + margin=1.5in
#     gave 397.485 pt = 5.500 in = 8.5 - 2*1.5 and 578.15999 pt = 8.000 in =
#     11 - 2*1.5; plain pdf + the documented margin=1in gave exactly 6.500 in
#     by 9.000 in; a4 + margin=1in gave 6.268 in by 9.693 in.
#   * a rotated (landscape) page is the same text block turned through 90
#     degrees, so its width and height are the portrait figures exchanged.
#     Verified in a ::: {.landscape} block: letter gave columnwidth
#     650.43001 pt = 9.000 in with textheight 469.75502 pt = 6.500 in, and
#     a4paper gave 700.50687 pt = 9.693 in with 452.96790 pt = 6.268 in.

# Paper, in inches, from \the\paperwidth and \the\paperheight.
APA_PAPER_SIZES <- list(
  letter = c(width = 8.5, height = 11),         # 614.29500 x 794.96999 pt
  a4     = c(width = 8.26783, height = 11.69200) # 597.50787 x 845.04684 pt
)

# The text block, in inches, of the plain-Quarto configurations that were
# measured. (apaquarto's own text blocks are arithmetic on the paper and are
# computed below rather than tabulated.)
APA_PAGE_GEOMETRIES <- list(
  "pdf-letter" = c(width = 6.182, height = 8.056),
  "pdf-a4"     = c(width = 6.013, height = 8.620)
)

# Journal mode, in inches. apa7 fixes the two-column text block itself: the
# spread measured 6.940 in on US letter AND with a4paper: true, and the
# height 8.900 in in both. A geometry: margin=X setting does move it, and
# then both follow the paper (jou with margin=1.5in on letter measured
# 397.485 pt = 5.500 in of spread and 578.15999 pt = 8.000 in of height,
# which is 8.5 - 3 and 11 - 3), so the spread is tabulated only for the
# stock case and derived otherwise.
#
# The column is the spread less the gutter, halved: 501.55397 pt of spread
# less 2 x 241.74324 pt of column leaves 18.07 pt = 0.25 in of gutter, and
# the same 0.25 in reproduces the narrower columns measured under
# margin=1.5in (189.70876 pt = 2.625 in = (5.500 - 0.25) / 2).
#
# A table in journal mode is put in apaquarto's ThreePartTable, whose
# \linewidth measured 240.74376 pt against the column's 241.74324 pt -- one
# point of inset that the table does not have.
APA_JOU_TEXTWIDTH  <- 6.940
APA_JOU_TEXTHEIGHT <- 8.900
APA_JOU_COLUMNSEP  <- 0.25
APA_JOU_TABLE_INSET <- 1 / 72.27

# The budget used when the document cannot be identified: the APA manuscript
# page. This is what apaquarto's manuscript mode produces on US letter and
# what plain Quarto produces with the geometry: margin=1in the apaTables
# documentation recommends, so it is the right answer for the great majority
# of documents and a defensible one for the rest.
APA_DEFAULT_GEOMETRY <- c(width = 6.5, height = 9.0)

# The document YAML of the render in progress, or NULL.
#
# Quarto exposes the resolved YAML through rmarkdown::metadata while knitr
# runs; outside a render (or without rmarkdown) there is nothing to read.
apa_render_metadata <- function() {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) return(NULL)
  m <- tryCatch(rmarkdown::metadata, error = function(e) NULL)
  if (!is.list(m) || !length(m)) NULL else m
}

# The output format names a document asks for, as written in its YAML.
# `format: pdf` gives a character value; `format:` with keys beneath it gives
# a list whose names are the formats.
apa_quarto_format_names <- function(meta) {
  fmt <- if (is.list(meta)) meta$format else NULL
  if (is.list(fmt)) names(fmt) else as.character(fmt)
}

# One YAML setting, looked up the way Quarto resolves it for the writer: a
# key set inside the output format wins, and a key set at the top level
# applies to every format. Both placements were confirmed by rendering --
# documentmode: jou at the top level and under format: apaquarto-pdf: each
# produced the same 3.345 in journal column.
apa_yaml_setting <- function(meta, key) {
  if (!is.list(meta) || !length(meta)) return(NULL)
  fmt <- meta$format
  if (is.list(fmt)) {
    for (f in fmt) if (is.list(f) && !is.null(f[[key]])) return(f[[key]])
  }
  meta[[key]]
}

# A LaTeX length as inches, or NA when the unit is not one of TeX's.
apa_length_inches <- function(s) {
  s <- gsub("[[:space:]]", "", as.character(s))
  m <- regmatches(s, regexec("^([0-9]*[.]?[0-9]+)(in|cm|mm|pt|bp)$", s))[[1]]
  if (length(m) < 3L) return(NA_real_)
  v <- as.numeric(m[2])
  switch(m[3],
         "in" = v,
         "cm" = v / 2.54,
         "mm" = v / 25.4,
         "pt" = v / 72.27,
         "bp" = v / 72,
         NA_real_)
}

# The margin, in inches, that a document's geometry: setting asks for --
# but only when the whole setting is a single margin=X entry, which is the
# one form whose effect on the text block is simple arithmetic on the paper
# (paper less twice the margin, verified by rendering). Anything else
# (left=, textwidth=, a bare keyword such as landscape) returns NA, which is
# this file's way of saying "do not pretend to know".
apa_geometry_margin <- function(geometry) {
  if (is.null(geometry)) return(NA_real_)
  entries <- unlist(geometry, use.names = FALSE)
  if (is.list(geometry) && !is.null(names(geometry)) &&
      all(nzchar(names(geometry)))) {
    entries <- paste0(names(geometry), "=", entries)
  }
  entries <- as.character(entries)
  entries <- entries[nzchar(gsub("[[:space:]]", "", entries))]
  if (length(entries) != 1L) return(NA_real_)
  m <- regmatches(entries, regexec("^[[:space:]]*margin[[:space:]]*=(.+)$",
                                   entries))[[1]]
  if (length(m) < 2L) return(NA_real_)
  apa_length_inches(m[2])
}

# Is this chunk asking apaquarto for a table that spans both journal
# columns? apaquarto reads apa-twocolumn off the float and puts the table in
# its twocolumntable environment, which measures 6.940 in instead of the
# 3.331 in of one column (verified by rendering a correlation table both
# ways). The setting is a CHUNK option, so it is read from knitr, not from
# the document YAML.
apa_chunk_twocolumn <- function() {
  if (!isTRUE(getOption("knitr.in.progress"))) return(FALSE)
  v <- tryCatch(knitr::opts_current$get("apa-twocolumn"),
                error = function(e) NULL)
  isTRUE(v) || identical(as.character(v), "true")
}

# The width and height, in inches, that this document gives a table.
#
# Returns a list with `width`, `height`, `config` (a short label for the
# configuration that was recognised) and `unknown` -- NULL when the answer
# rests on a measurement, or a short description of the setting that could
# not be interpreted when it does not.
#
# When the configuration cannot be worked out the APA manuscript page
# (6.5 x 9.0 in) is used rather than the narrowest budget in the table
# above. Falling back to the narrowest -- the 3.331 in journal column --
# would shrink every table in every unrecognised document to the smallest
# font the fitter has, trading a rare wrong decision for a certain bad one.
# What must not happen is that the fallback be SILENT, so `unknown` is
# filled in and the caller warns.
apa_page_geometry <- function(page = "portrait", meta = NULL,
                              twocolumn = NULL) {
  if (is.null(meta)) meta <- apa_render_metadata()
  if (is.null(twocolumn)) twocolumn <- apa_chunk_twocolumn()

  unknown <- NULL
  fmts <- apa_quarto_format_names(meta)
  apaq <- any(grepl("^apaquarto-", fmts))
  landscape <- identical(page, "landscape")

  # No metadata at all -- console use, a bare knitr LaTeX run, a format we
  # were told nothing about. There is no evidence of a non-standard page, so
  # the manuscript default stands and nothing is reported.
  if (!length(fmts)) {
    g <- APA_DEFAULT_GEOMETRY
    if (landscape) g <- c(width = unname(g[["height"]]),
                          height = unname(g[["width"]]))
    return(list(width = unname(g[["width"]]), height = unname(g[["height"]]),
                config = "default", unknown = NULL))
  }

  mode <- apa_yaml_setting(meta, "documentmode")
  mode <- if (is.null(mode)) "man" else tolower(as.character(mode)[1])

  # Paper. apaquarto selects it with its own a4paper flag and IGNORES
  # Quarto's papersize (its LaTeX template has no papersize branch, and a
  # probe with papersize: a4 came back on US letter), so the two formats
  # read different keys.
  if (apaq) {
    paper <- if (isTRUE(apa_yaml_setting(meta, "a4paper"))) "a4" else "letter"
  } else {
    ps <- apa_yaml_setting(meta, "papersize")
    paper <- if (is.null(ps)) "letter" else tolower(as.character(ps)[1])
    if (identical(paper, "a4paper")) paper <- "a4"
    if (identical(paper, "letterpaper")) paper <- "letter"
    if (!paper %in% names(APA_PAPER_SIZES)) {
      unknown <- paste0("papersize: ", paper)
      paper <- "letter"
    }
  }
  sheet <- APA_PAPER_SIZES[[paper]]

  geometry <- apa_yaml_setting(meta, "geometry")
  margin <- apa_geometry_margin(geometry)
  if (!is.null(geometry) && is.na(margin)) {
    unknown <- "a geometry: setting apaTables cannot measure"
  }

  # Journal mode is decided before anything else, because it is the one mode
  # whose text block apa7 sets itself: the paper size does not move it (a4
  # measured the same as US letter), and a ::: {.landscape} block does not
  # widen it either -- a rotated journal page keeps its two columns, and a
  # probe confirmed its content runs off the page rather than gaining room.
  # A geometry: margin=X setting DOES move it, and then the spread follows
  # the paper the same way every other mode does.
  if (apaq && identical(mode, "jou")) {
    spread <- if (!is.na(margin)) unname(sheet[["width"]]) - 2 * margin
              else APA_JOU_TEXTWIDTH
    height <- if (!is.na(margin)) unname(sheet[["height"]]) - 2 * margin
              else APA_JOU_TEXTHEIGHT
    width <- if (isTRUE(twocolumn)) spread
             else (spread - APA_JOU_COLUMNSEP) / 2 - APA_JOU_TABLE_INSET
    return(list(width = width, height = height,
                config = if (isTRUE(twocolumn)) "apaquarto-jou-twocolumn"
                         else "apaquarto-jou",
                unknown = unknown))
  }
  if (apaq && !mode %in% c("man", "stu", "doc")) {
    unknown <- paste0("documentmode: ", mode)
    mode <- "man"
  }

  if (!is.na(margin)) {
    g <- c(width = unname(sheet[["width"]]) - 2 * margin,
           height = unname(sheet[["height"]]) - 2 * margin)
    config <- paste0(if (apaq) "apaquarto-" else "pdf-", paper, "-margin")
  } else if (!is.null(unknown)) {
    g <- APA_DEFAULT_GEOMETRY
    config <- "default"
  } else if (apaq) {
    # apaquarto's manuscript and student modes are the paper less one inch
    # of margin on every side: 6.500 x 9.000 in on letter and 6.268 x 9.693
    # in on a4 were measured, and both equal the paper less 2 in.
    g <- c(width = unname(sheet[["width"]]) - 2,
           height = unname(sheet[["height"]]) - 2)
    config <- paste0("apaquarto-man-", paper)
  } else {
    g <- APA_PAGE_GEOMETRIES[[paste0("pdf-", paper)]]
    config <- paste0("pdf-", paper)
  }

  if (landscape) g <- c(width = unname(g[["height"]]),
                        height = unname(g[["width"]]))
  list(width = unname(g[["width"]]), height = unname(g[["height"]]),
       config = config, unknown = unknown)
}

# apa_geometry_is_journal() stood here. Journal mode used to need a branch of
# its own in every caller, because the remedies differed there: a rotated page
# does not widen a two-column body. Phase 2's ladder needs no such branch --
# apa_layout_decide() rotates only when the rotated geometry is WIDER than the
# upright one, and in journal mode both report the same column, so the
# comparison stands journal mode down by itself. The predicate had no caller
# left, and one nobody calls is one nobody keeps true.

# Warn -- once per render -- that the page could not be identified.
#
# ONE flowing sentence with no embedded newline: knitr wraps a warning to
# the page width only when the message contains no "\n"
# (knitr:::msg_wrap), so a message broken across lines runs off the right
# edge of the rendered draft.
apa_warn_unknown_geometry <- function(geom) {
  if (is.null(geom$unknown)) return(invisible(FALSE))
  apa_warn_once("page-geometry", paste0(
    "apaTables could not work out how much of the page this document gives ",
    "a table, because its YAML header carries ", geom$unknown,
    ", so this table was laid out for a standard APA manuscript page (",
    format(APA_DEFAULT_GEOMETRY[["width"]], trim = TRUE), " inches wide by ",
    format(APA_DEFAULT_GEOMETRY[["height"]], trim = TRUE),
    " inches tall) and may not match the page it lands on; if columns are ",
    "cut off at the right edge, save the table as a Word file instead, ",
    "where nothing is cut off: apa.save(filename = \"mytable.docx\", ",
    "your_table)."))
}

# Journal mode is not supported, and this is where it says so (B4).
#
# THIS WARNING IS NOT A FIX AND IS NOT OFFERED AS ONE. The rule in this
# package is that a warning stands beside a real fix and never instead of
# one -- and here there is no fix to stand beside, which is the only reason
# this exists. Measured: with documentmode: jou and four ordinary tables the
# render exits 0, produces ONE page, drops two of the four notes entirely and
# 5 of 25 variable names, and says nothing. The container is not the defect
# either: apafloatlatex.lua's longtable branch stands down in journal mode
# because REMOVING that guard ABORTS the render outright ("Package longtable
# Error: longtable not in 1-column mode"), so in `jou` it is the grid that
# has to change, which is a phase of its own. Until that phase runs, the
# honest thing is to stop the loss being silent and to say the same in the
# documentation rather than let `jou` look supported.
#
# ONE flowing sentence with no embedded newline, for the reason given above
# apa_warn_unknown_geometry().
apa_warn_journal_mode <- function(geom) {
  if (is.null(geom$config) ||
      !startsWith(as.character(geom$config)[1], "apaquarto-jou")) {
    return(invisible(FALSE))
  }
  apa_warn_once("journal-mode", paste0(
    "This document is in apaquarto's journal mode (documentmode: jou), which ",
    "apaTables does not support: a two-column journal page cannot break a ",
    "table across pages, so rows, columns and whole notes can be missing from ",
    "the rendered PDF with no error anywhere -- check every table in the PDF ",
    "against what apaTables printed at the console, and where anything is ",
    "missing either render the manuscript with documentmode: man or save that ",
    "table as a Word file instead, where nothing is dropped: ",
    "apa.save(filename = \"mytable.docx\", your_table)."))
}
