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

# apa_pptx_slide2_xml() and apa_pptx_table_rows(): pptx XML extraction
# helpers. PowerPoint is not a supported destination, but a pptx render
# still gets a table from this package (rendered, then warned about), and
# that table's OWN correctness -- spanner flattening -- has to be verified
# against a real render. These read pandoc's pptx writer output, which is
# what a pptx render produces today. A future officer-based PowerPoint route
# would need its own extraction helpers; these stay
# pandoc-pptx-writer-specific on purpose.
apa_pptx_slide2_xml <- function(path) {
  con <- unz(path, "ppt/slides/slide2.xml")
  on.exit(close(con), add = TRUE)
  paste(readLines(con, warn = FALSE), collapse = "")
}

# The text of every <a:t> run in a pptx slide that contains a table, as a
# list of rows of cell strings -- the same extraction used to measure how
# PowerPoint lays out spanner rows, so an assertion here is comparable with
# that measurement.
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
