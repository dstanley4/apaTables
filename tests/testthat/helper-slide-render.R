# Render helpers for the slide acceptance suite
# (tests/testthat/test-slide-render.R).
#
# Slides are PLAIN Quarto -- no apaquarto extension, no float, no
# `::: {.apa-table-meta}` div. apa_render_fixture_html()/_docx() in
# helper-apaquarto-extract.R build an apaquarto PROJECT via
# apa.quarto.new(), which is the wrong scaffold for a deck: there is no
# _extension.yml to install and no bundled filter chain to travel with the
# render. So a deck fixture is rendered where it stands, copied into an
# otherwise-empty scratch directory, exactly the discipline the docx/html
# helpers use (fresh dir per render, loud failure with the render log
# attached) minus the project-building step that does not apply here.

apa_render_slide_fixture <- function(name, to = c("revealjs", "pptx")) {
  to  <- match.arg(to)
  src <- testthat::test_path("fixtures", paste0(name, ".qmd"))
  if (!file.exists(src)) stop("No such fixture: ", src, call. = FALSE)

  dir <- file.path(tempdir(), paste0("slide-", name, "-", to, "-",
                                     as.integer(Sys.time())))
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  qmd <- file.path(dir, "deck.qmd")
  file.copy(src, qmd, overwrite = TRUE)

  log <- suppressWarnings(system2(
    "quarto", c("render", shQuote(qmd), "--to", to),
    stdout = TRUE, stderr = TRUE))
  out <- file.path(dir, paste0("deck.", if (to == "pptx") "pptx" else "html"))
  if (!file.exists(out)) {
    stop("quarto render produced no ", basename(out), " for '", name, "':\n",
         paste(log, collapse = "\n"), call. = FALSE)
  }
  out
}

# apa_pptx_slide2_xml() and apa_pptx_table_rows() (pptx XML extraction
# helpers, D54/D46) were REMOVED here in Phase 1.9 Task 2 -- grep-confirmed
# zero remaining callers once Task 1 deleted the pptx-specific tests in
# test-slide-render.R that used them, on the premise that PowerPoint had
# left the Quarto path entirely (D88/D90) and there was nothing left in
# THIS package's output for them to read.
#
# RESTORED (D92, whole-branch review of Phase 1.9, 2026-08-06). D92
# reversed the "no output worth checking" premise: a pptx render still
# gets a table from this package (D90's render-then-warn), and D92 exists
# specifically because that table's OWN correctness (spanner flattening)
# needed verifying against a real render, the same way D54 first found the
# table-less-slide defect these helpers were built to catch. They read
# pandoc's pptx writer output, which is still what a pptx render produces
# today -- Phase 1.13's officer-based route is a different, NOT-YET-BUILT
# mechanism and will need its own extraction helpers when it exists; these
# stay pandoc-pptx-writer-specific on purpose.
apa_pptx_slide2_xml <- function(path) {
  con <- unz(path, "ppt/slides/slide2.xml")
  on.exit(close(con), add = TRUE)
  paste(readLines(con, warn = FALSE), collapse = "")
}

# The text of every <a:t> run in a pptx slide that contains a table, as a
# list of rows of cell strings -- the same extraction the D46 measurement
# used, so an assertion here is comparable with the recorded evidence.
apa_pptx_table_rows <- function(path) {
  xml <- apa_pptx_slide2_xml(path)
  rows <- regmatches(xml, gregexpr("<a:tr [^>]*>.*?</a:tr>", xml))[[1]]
  lapply(rows, function(r) {
    cells <- regmatches(r, gregexpr("<a:tc[ >].*?</a:tc>", r))[[1]]
    vapply(cells, function(c) paste(
      regmatches(c, gregexpr("(?<=<a:t>).*?(?=</a:t>)", c, perl = TRUE))[[1]],
      collapse = ""), character(1), USE.NAMES = FALSE)
  })
}
