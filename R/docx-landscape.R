# Task 6: the landscape section-break filter's R half.
#
# D31 measured Quarto's own `.landscape` div broken twice over for docx: its
# portrait closer is a COMPLETELY EMPTY sectPr (so every page before a
# rotated table loses the running head and page number, not just the
# rotated page itself), and its rotated page size is hardcoded A4 regardless
# of the manuscript's own paper. apaTables writes its own section
# properties instead (`apatablelandscape.lua`, bundled in the apaquarto
# extension) -- a portrait closer carrying the document's REAL page size,
# margins and header references, and a landscape section that is the same
# page with width and height swapped, never a hardcoded constant.
#
# "The document's real page size" for docx is not a LaTeX-style geometry
# computation (R/page-geometry.R's apa_page_geometry() answers a different
# question -- inches of TEXT AREA for a table that must not clip -- and is
# built from measured LaTeX probes that have no docx counterpart). For docx
# there is exactly one source of truth: apaTables bundles ONE reference
# document, inst/_extensions/wjschne/apaquarto/apaquarto.docx, and pandoc's
# docx writer takes the page size, margins and header/footer relationships
# of every section that is not explicitly overridden straight from that
# file. So the real page is read out of the reference document itself,
# never assumed -- if a future reference document ships a different paper
# (an A4 variant, say) this reads its real page without any code change.
#
# WHICH COPY OF apaquarto.docx IS READ (Phase 1.7 Task 5; this used to be a
# recorded assumption rather than a behaviour). The render does NOT use the
# installed package's reference document -- it uses the one in the rendering
# PROJECT's own `_extensions/wjschne/apaquarto/`, the copy
# `apa.quarto.new()` / `apa.quarto.setup()` puts there and that Quarto
# resolves by looking beside the document and then in each parent directory.
# Reading the installed copy instead was identical for every project this
# package's own tooling sets up, but silently divergent the moment a user
# edits THEIR copy -- which is supported, and is exactly where an asymmetric
# margin (a thesis binding edge) lives. D64 makes that divergence matter
# more, because the rotated section now carries those margins round with the
# page rather than ignoring them. So apa_docx_reference_sectpr() prefers the
# project's copy, found by the same upward walk apa.quarto.setup() already
# uses (apa_quarto_search_dirs(), R/apaquarto-version.R -- Quarto's own
# resolution order), and falls back to system.file() when there is none.

# One numeric attribute off a self-closing OOXML element, e.g. reading "w"
# out of <w:pgSz w:w="12240" w:h="15840"/>. NA when the attribute or the
# element itself is not there, which is this file's way of saying a
# reference document does not have the shape expected of it -- see
# apa_docx_reference_sectpr()'s caller for what that NA becomes.
apa_docx_xml_attr <- function(xml, name) {
  if (is.na(xml) || !nzchar(xml)) return(NA_integer_)
  m <- regmatches(xml, regexpr(paste0('w:', name, '="([0-9]+)"'), xml))
  if (!length(m) || !nzchar(m)) return(NA_integer_)
  as.integer(sub('.*"([0-9]+)"', "\\1", m))
}

# A .docx's own trailing section properties: page size and margins, read
# out of its word/document.xml rather than assumed. Takes a path so it can
# be proven generic -- fed a synthetic A4-sized .docx in
# tests/testthat/test-docx-structure.R -- without needing a second bundled
# reference document that does not exist (apaquarto ships exactly one docx
# reference, US Letter; there is no a4paper equivalent for docx the way
# R/page-geometry.R has one for LaTeX, see this file's banner).
#
# Deliberately does NOT return the header/footer relationship IDs -- D31
# measured those STABLE under pandoc's own docx writer (preserved from the
# reference document, new relationships allocated above them: a probe with
# twelve new relationships left rId8..rId13 untouched), so
# apatablelandscape.lua hardcodes rId8/rId9/rId12 directly rather than
# threading them through here. Reading the page geometry back out of the
# same file both functions ultimately answer from is what keeps the two
# questions independent: a reference-document edit that only moves a
# margin cannot silently desynchronise from a hardcoded R constant, because
# there is no hardcoded R constant for it.
#
# Returns NULL when the file cannot be found or does not have the single
# trailing <w:sectPr> this reads -- the caller decides what NULL means
# (apa_landscape_wrap() declines to rotate rather than write a landscape
# section with guessed numbers).
apa_docx_sectpr_from_file <- function(path) {
  if (!nzchar(path) || !file.exists(path)) return(NULL)

  d <- tempfile()
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  ok <- tryCatch({
    utils::unzip(path, files = "word/document.xml", exdir = d)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  docxml <- file.path(d, "word", "document.xml")
  if (!isTRUE(ok) || !file.exists(docxml)) return(NULL)

  x <- paste(readLines(docxml, warn = FALSE), collapse = "")
  # The LAST <w:sectPr> in the file is the document's default (trailing,
  # unwrapped-in-a-paragraph) section -- the one every ordinary page uses.
  sects <- regmatches(x, gregexpr("<w:sectPr\\b.*?</w:sectPr>", x, perl = TRUE))[[1]]
  if (!length(sects)) return(NULL)
  sect <- sects[length(sects)]

  pgsz <- regmatches(sect, regexpr("<w:pgSz\\b[^>]*/?>", sect))
  pgmar <- regmatches(sect, regexpr("<w:pgMar\\b[^>]*/?>", sect))
  if (!length(pgsz) || !nzchar(pgsz) || !length(pgmar) || !nzchar(pgmar)) {
    return(NULL)
  }

  out <- list(
    width         = apa_docx_xml_attr(pgsz, "w"),
    height        = apa_docx_xml_attr(pgsz, "h"),
    margin_top    = apa_docx_xml_attr(pgmar, "top"),
    margin_bottom = apa_docx_xml_attr(pgmar, "bottom"),
    margin_left   = apa_docx_xml_attr(pgmar, "left"),
    margin_right  = apa_docx_xml_attr(pgmar, "right"),
    margin_header = apa_docx_xml_attr(pgmar, "header"),
    margin_footer = apa_docx_xml_attr(pgmar, "footer"),
    margin_gutter = apa_docx_xml_attr(pgmar, "gutter"))
  if (anyNA(out)) return(NULL)
  out
}

# The rendering PROJECT's own copy of apaquarto.docx, or "" when there is
# none -- see this file's banner for why the project's copy, not the
# installed package's, is the one the render actually uses.
#
# The walk is Quarto's own extension resolution (beside the document first,
# then each parent directory), reusing apa_quarto_search_dirs() rather than
# a second implementation of it. Deliberately NOT gated on "is a render in
# progress" the way apa_setup_root() is: that gate exists so a brand-new
# project is not CAPTURED by an ancestor's extension folder when apaTables
# is about to WRITE one, and nothing is written here -- if an ancestor
# holds the extension, that ancestor's copy is the one a render started
# from this directory would use, so reading it is right, not a false
# positive.
apa_docx_project_reference <- function(start = NULL) {
  for (d in apa_quarto_search_dirs(start)) {
    p <- file.path(d, APA_EXT_REL_PATH, "apaquarto.docx")
    if (file.exists(p)) return(p)
  }
  ""
}

# WHICH reference document, and whose. Split out of
# apa_docx_reference_sectpr() (fix round 1, Important 2) so the failure
# WARNING can name the file it actually tried and give advice that fits it:
# once the project's own copy is preferred, "reinstall apaTables" is the
# wrong instruction for the likeliest failure, because the file that would
# not parse is the user's, not ours.
#
# `project` is the whole difference the message turns on, so it is returned
# as a flag rather than re-derived by comparing paths at the call site.
apa_docx_reference_path <- function() {
  p <- apa_docx_project_reference()
  if (nzchar(p)) return(list(path = p, project = TRUE))
  list(path = system.file("_extensions", "wjschne", "apaquarto",
                          "apaquarto.docx", package = "apaTables"),
       project = FALSE)
}

# The reference document the render will actually use -- what
# apa_landscape_wrap() calls. The project's own copy when there is one, the
# installed package's otherwise.
apa_docx_reference_sectpr <- function() {
  ref <- apa_docx_reference_path()
  if (!nzchar(ref$path)) return(NULL)
  apa_docx_sectpr_from_file(ref$path)
}

# Once-per-render: the reference document could not be read for its real
# page geometry, so the table stays upright rather than being rotated onto
# a landscape section built from guessed numbers.
#
# NAMES THE FILE AND FITS THE ADVICE TO IT (fix round 1, Important 2). Before
# the project's copy was preferred there was only one candidate, so "its own
# bundled apaquarto.docx ... reinstall apaTables" was true by construction.
# It is not any more: the likelier failure now is a project's own copy that
# has been hand-edited, truncated or half-written, and reinstalling apaTables
# would not touch it -- apa.quarto.setup() is what restores it. Saying
# "reinstall apaTables" there would be a false instruction printed into a
# rendered manuscript, the category this project treats as release-relevant.
#
# ONE FLOWING SENTENCE, NO "\n" ANYWHERE (auto-memory
# `knitr-warning-newline-trap`): knitr only wraps single-line warnings, so an
# embedded newline pushes the rest of the text off the right edge of a
# knitted page while pdftotext still reports it -- invisible to the reader
# AND to a grep-based check.
apa_warn_no_docx_geometry <- function(ref = apa_docx_reference_path()) {
  where <- if (isTRUE(ref$project)) {
    paste0("the apaquarto reference document this project is using, ",
           ref$path, ", so it has been kept upright instead of guessing a ",
           "page size that might not match this document; every column is ",
           "still present -- run apa.quarto.setup() to restore that file ",
           "from the copy apaTables ships.")
  } else {
    paste0("its own bundled apaquarto.docx, ", ref$path,
           ", so it has been kept upright instead of guessing a page size ",
           "that might not match this document; every column is still ",
           "present -- if this keeps happening, reinstall apaTables.")
  }
  apa_warn_once("docx-landscape-geometry", paste0(
    "This table was going to be placed on a rotated (landscape) page, but ",
    "apaTables could not read the real page size out of ", where))
}
