# Page geometry: what a document actually gives a table.
#
# The inch figures asserted here are the ones recorded in R/page-geometry.R,
# every one of them read out of a rendered document with a raw-LaTeX probe
# (\the\linewidth, \the\textheight, \the\paperwidth, \the\paperheight). The
# three that matter most -- an apaquarto manuscript page, the same page
# rotated, and a journal column -- are re-measured from a live render in
# test-latex-calibration.R, so this file is the fast, toolchain-free copy of
# the same knowledge plus the branching logic around it.

meta_fmt <- function(fmt, ...) {
  args <- list(...)
  structure(list(format = stats::setNames(list(if (length(args)) args else list()),
                                          fmt)))
}

test_that("apaquarto manuscript pages are the paper less an inch all round", {
  # letter: linewidth 469.75502 pt, textheight 650.43001 pt
  g <- apa_page_geometry("portrait", meta_fmt("apaquarto-pdf"),
                         twocolumn = FALSE)
  expect_equal(g$width, 6.5, tolerance = 1e-6)
  expect_equal(g$height, 9.0, tolerance = 1e-6)
  expect_identical(g$config, "apaquarto-man-letter")
  expect_null(g$unknown)

  # a4paper: 452.96790 pt by 700.50687 pt
  a4 <- apa_page_geometry("portrait", meta_fmt("apaquarto-pdf", a4paper = TRUE),
                          twocolumn = FALSE)
  expect_equal(a4$width, 6.268, tolerance = 0.001)
  expect_equal(a4$height, 9.692, tolerance = 0.001)

  # documentmode stu and doc measured identical to man
  for (m in c("man", "stu", "doc")) {
    g2 <- apa_page_geometry("portrait",
                            meta_fmt("apaquarto-pdf", documentmode = m),
                            twocolumn = FALSE)
    expect_equal(g2$width, 6.5, tolerance = 1e-6)
    expect_equal(g2$height, 9.0, tolerance = 1e-6)
  }
})

test_that("a rotated page is the same text block turned through 90 degrees", {
  g <- apa_page_geometry("landscape", meta_fmt("apaquarto-pdf"),
                         twocolumn = FALSE)
  expect_equal(g$width, 9.0, tolerance = 1e-6)
  expect_equal(g$height, 6.5, tolerance = 1e-6)

  # measured inside a ::: {.landscape} block on a4: 700.50687 pt of column
  # width against 452.96790 pt of text height
  a4 <- apa_page_geometry("landscape", meta_fmt("apaquarto-pdf", a4paper = TRUE),
                          twocolumn = FALSE)
  expect_equal(a4$width, 9.692, tolerance = 0.001)
  expect_equal(a4$height, 6.268, tolerance = 0.001)
})

test_that("journal mode is a column, not a page", {
  # the whole point of C12: documentmode jou is two-column, so a table gets
  # 3.33 in -- laying it out for 6.5 in loses more than half of it
  g <- apa_page_geometry("portrait",
                         meta_fmt("apaquarto-pdf", documentmode = "jou"),
                         twocolumn = FALSE)
  expect_equal(g$width, 3.331, tolerance = 0.001)   # 240.74376 pt
  expect_equal(g$height, 8.900, tolerance = 0.001)  # 643.20255 pt
  expect_identical(g$config, "apaquarto-jou")

  # a4 does not move it: apa7 fixes the journal text block itself
  a4 <- apa_page_geometry("portrait",
                          meta_fmt("apaquarto-pdf", documentmode = "jou",
                                   a4paper = TRUE),
                          twocolumn = FALSE)
  expect_equal(a4$width, g$width, tolerance = 1e-9)
  expect_equal(a4$height, g$height, tolerance = 1e-9)

  # nor does a rotated page: the two columns survive the rotation
  ls <- apa_page_geometry("landscape",
                          meta_fmt("apaquarto-pdf", documentmode = "jou"),
                          twocolumn = FALSE)
  expect_equal(ls$width, g$width, tolerance = 1e-9)
  expect_equal(ls$height, g$height, tolerance = 1e-9)

  # a chunk asking for a table across both columns gets the whole spread
  # (twocolumntable measured 501.55397 pt)
  tw <- apa_page_geometry("portrait",
                          meta_fmt("apaquarto-pdf", documentmode = "jou"),
                          twocolumn = TRUE)
  expect_equal(tw$width, 6.940, tolerance = 0.001)
  expect_identical(tw$config, "apaquarto-jou-twocolumn")
})

test_that("documentmode is read from the top level as well as from the format", {
  # both placements were confirmed by rendering
  top <- apa_page_geometry("portrait",
                           list(documentmode = "jou",
                                format = list("apaquarto-pdf" = "default")),
                           twocolumn = FALSE)
  expect_identical(top$config, "apaquarto-jou")
})

test_that("plain Quarto PDF is narrower than a manuscript page unless told otherwise", {
  # format: pdf with no geometry: 446.76004 pt by 582.20026 pt -- NOT the
  # 6.5 x 9 in the model used to assume for every LaTeX render
  g <- apa_page_geometry("portrait", list(format = "pdf"), twocolumn = FALSE)
  expect_equal(g$width, 6.182, tolerance = 0.001)
  expect_equal(g$height, 8.056, tolerance = 0.001)

  a4 <- apa_page_geometry("portrait", meta_fmt("pdf", papersize = "a4"),
                          twocolumn = FALSE)
  expect_equal(a4$width, 6.013, tolerance = 0.001)
  expect_equal(a4$height, 8.620, tolerance = 0.001)

  # the geometry: margin=1in the apaTables documentation recommends gives
  # exactly the manuscript page (measured 469.75502 pt by 650.43001 pt)
  m <- apa_page_geometry("portrait",
                         meta_fmt("pdf", geometry = list("margin=1in")),
                         twocolumn = FALSE)
  expect_equal(m$width, 6.5, tolerance = 1e-6)
  expect_equal(m$height, 9.0, tolerance = 1e-6)
})

test_that("a margin= geometry is arithmetic on the paper, in any unit", {
  # apaquarto man + margin=1.5in measured 397.485 pt by 578.15999 pt
  g <- apa_page_geometry("portrait",
                         meta_fmt("apaquarto-pdf",
                                  geometry = list("margin=1.5in")),
                         twocolumn = FALSE)
  expect_equal(g$width, 5.5, tolerance = 1e-6)
  expect_equal(g$height, 8.0, tolerance = 1e-6)
  expect_null(g$unknown)

  # the same margin written in centimetres, and as a mapping rather than a
  # list of strings
  expect_equal(apa_length_inches("2.54cm"), 1, tolerance = 1e-9)
  expect_equal(apa_length_inches("25.4mm"), 1, tolerance = 1e-9)
  expect_equal(apa_geometry_margin(list(margin = "1in")), 1, tolerance = 1e-9)

  # jou follows the paper too once a margin is given: the spread becomes
  # 5.5 in and the column half of it less the 0.25 in gutter
  j <- apa_page_geometry("portrait",
                         meta_fmt("apaquarto-pdf", documentmode = "jou",
                                  geometry = list("margin=1.5in")),
                         twocolumn = FALSE)
  expect_equal(j$width, (5.5 - 0.25) / 2 - 1 / 72.27, tolerance = 1e-6)
  expect_equal(j$height, 8.0, tolerance = 1e-6)
})

test_that("an unrecognised page is reported, not silently guessed at", {
  # a geometry apaTables cannot turn into a text block
  g <- apa_page_geometry("portrait",
                         meta_fmt("pdf", geometry = list("left=2cm", "right=2cm")),
                         twocolumn = FALSE)
  expect_equal(g$width, 6.5, tolerance = 1e-6)     # the manuscript fallback
  expect_identical(g$config, "default")
  expect_match(g$unknown, "geometry")

  # an unknown paper, and an unknown documentmode
  expect_match(apa_page_geometry("portrait", meta_fmt("pdf", papersize = "b5"),
                                 twocolumn = FALSE)$unknown, "papersize")
  expect_match(apa_page_geometry("portrait",
                                 meta_fmt("apaquarto-pdf", documentmode = "zzz"),
                                 twocolumn = FALSE)$unknown, "documentmode")

  # and the fallback is the manuscript page rather than the narrowest budget
  # known: shrinking every table in every unrecognised document to the 3.3 in
  # journal column would trade a rare wrong decision for a certain bad one
  expect_gt(apa_page_geometry("portrait", meta_fmt("pdf", papersize = "b5"),
                              twocolumn = FALSE)$width, 6)
})

test_that("no metadata at all means the manuscript default, and no complaint", {
  # console use, a bare knitr LaTeX run, a format apaTables was told nothing
  # about: there is no evidence of a non-standard page, so nothing to report
  for (m in list(NULL, list(), list(title = "x"))) {
    g <- apa_page_geometry("portrait", if (is.null(m)) list() else m,
                           twocolumn = FALSE)
    expect_equal(g$width, 6.5, tolerance = 1e-6)
    expect_equal(g$height, 9.0, tolerance = 1e-6)
    expect_identical(g$config, "default")
    expect_null(g$unknown)
  }
})

test_that("the unknown-page warning is one flowing line", {
  apa_reset_version_warnings()
  on.exit(apa_reset_version_warnings(), add = TRUE)
  g <- apa_page_geometry("portrait",
                         meta_fmt("pdf", geometry = list("left=2cm")),
                         twocolumn = FALSE)
  w <- tryCatch(apa_warn_unknown_geometry(g), warning = conditionMessage)
  expect_match(w, "geometry", fixed = TRUE)
  expect_match(w, "apa.save", fixed = TRUE)
  # knitr:::msg_wrap only wraps a message with no newline in it; a multi-line
  # warning runs off the right edge of the rendered draft
  expect_false(grepl("\n", w, fixed = TRUE))

  # and it is said once per render, not once per table
  expect_false(apa_warn_unknown_geometry(g))

  # a page that IS recognised says nothing
  expect_false(apa_warn_unknown_geometry(
    apa_page_geometry("portrait", meta_fmt("apaquarto-pdf"), twocolumn = FALSE)))
})

test_that("the layout ladder uses the geometry the document reports", {
  # The four longtable/landscape warnings that stood here were deleted in
  # Task 10 with the checks they exercised. What must NOT be lost with them is
  # this: the page a document actually has has to reach the layout decision.
  # The same table, two documents -- a manuscript page and a journal column --
  # and the ladder must treat them differently.
  spec <- apa.cor.table(attitude[, 1:4])
  plan <- apa_display_plan(as_spec(spec))

  man <- withCallingHandlers(
    apa_layout_decide(plan,
      geom = list(width = 6.5, height = 9, config = "apaquarto-man-letter",
                  unknown = NULL),
      geom_landscape = list(width = 9, height = 6.5,
                            config = "apaquarto-man-letter", unknown = NULL)),
    warning = function(w) invokeRestart("muffleWarning"))

  jou <- withCallingHandlers(
    apa_layout_decide(plan,
      geom = list(width = 3.331, height = 8.9, config = "apaquarto-jou",
                  unknown = NULL),
      # journal mode: a rotated page keeps its two columns, so the rotated
      # geometry is the SAME width and the ladder must not rotate
      geom_landscape = list(width = 3.331, height = 8.9,
                            config = "apaquarto-jou", unknown = NULL)),
    warning = function(w) invokeRestart("muffleWarning"))

  expect_identical(man$size, APA_QUARTO_DEFAULT_SIZE)   # fits upright as is
  expect_false(man$rotate)
  expect_lt(jou$size, man$size)                          # the column shrank it
  expect_false(jou$rotate)                               # rotation buys nothing
  expect_lte(jou$width, 3.331)
})

test_that("the ladder reports a page it could not identify", {
  # apa_warn_unknown_geometry() used to be called from apa_flextable()'s LaTeX
  # branch. Task 10 deleted that branch, and the warning moved to the ladder --
  # so this pins that it still has a caller. Without the call the fallback to a
  # standard manuscript page goes silent, and a table laid out for 6.5 in on a
  # page that is not 6.5 in wide is exactly the clipping Phase 2 exists to
  # prevent.
  apa_reset_version_warnings()
  on.exit(apa_reset_version_warnings(), add = TRUE)
  plan <- apa_display_plan(as_spec(apa.1way.table(iv = dose, dv = libido,
                                                  data = viagra)))
  unknown <- list(width = 6.5, height = 9, config = "default",
                  unknown = "a geometry: setting apaTables cannot measure")
  w <- tryCatch({ apa_layout_decide(plan, geom = unknown,
                                    geom_landscape = unknown); NULL },
                warning = conditionMessage)
  expect_match(w, "could not work out how much of the page", fixed = TRUE)

  # and a page that IS identified stays silent
  apa_reset_version_warnings()
  known <- list(width = 6.5, height = 9, config = "apaquarto-man-letter",
                unknown = NULL)
  expect_no_warning(apa_layout_decide(plan, geom = known,
                                      geom_landscape = known))
})

test_that("journal mode says out loud that it is not supported (B4)", {
  # THIS WARNING IS NOT A FIX AND IS NOT OFFERED AS ONE -- there is no fix to
  # offer. Measured: documentmode jou with four ordinary tables renders exit 0
  # in ONE page, drops two of the four notes entirely and 5 of 25 variable
  # names, and says nothing. Removing apafloatlatex.lua's journalmode guard
  # ABORTS the render ("longtable not in 1-column mode"), so in jou it is the
  # grid that has to change, which is a phase of its own. Until then the
  # honest move is to stop the loss being silent.
  apa_reset_version_warnings()
  on.exit(apa_reset_version_warnings(), add = TRUE)

  g <- apa_page_geometry("portrait",
                         meta_fmt("apaquarto-pdf", documentmode = "jou"),
                         twocolumn = FALSE)
  expect_identical(g$config, "apaquarto-jou")
  w <- tryCatch(apa_warn_journal_mode(g), warning = conditionMessage)
  expect_match(w, "documentmode: jou", fixed = TRUE)
  expect_match(w, "documentmode: man", fixed = TRUE)
  expect_match(w, "apa.save", fixed = TRUE)
  # knitr:::msg_wrap only wraps a message with no newline in it; a multi-line
  # warning runs off the right edge of the rendered draft, where pdftotext
  # still reports the text nobody can see.
  expect_false(grepl("\n", w, fixed = TRUE))

  # once per render, not once per table
  expect_false(apa_warn_journal_mode(g))

  # the two-column variant is the same document and says the same thing
  apa_reset_version_warnings()
  tw <- apa_page_geometry("portrait",
                          meta_fmt("apaquarto-pdf", documentmode = "jou"),
                          twocolumn = TRUE)
  expect_identical(tw$config, "apaquarto-jou-twocolumn")
  expect_warning(apa_warn_journal_mode(tw), "documentmode: jou")
})

test_that("a manuscript page says nothing about journal mode", {
  # The companion absence check, worth something only because the test above
  # builds the case where the warning DOES fire and watches it.
  apa_reset_version_warnings()
  on.exit(apa_reset_version_warnings(), add = TRUE)
  expect_false(apa_warn_journal_mode(
    apa_page_geometry("portrait", meta_fmt("apaquarto-pdf"),
                      twocolumn = FALSE)))
  expect_false(apa_warn_journal_mode(list(config = "default")))
  expect_false(apa_warn_journal_mode(list()))
})

test_that("the ladder is where journal mode gets said", {
  # A warning nothing calls is a warning nobody sees. Every table in a Quarto
  # PDF passes through apa_layout_decide(), which is why the call lives there
  # and not in a builder.
  apa_reset_version_warnings()
  on.exit(apa_reset_version_warnings(), add = TRUE)
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude[, 1:4])))
  jou <- list(width = 3.331, height = 8.9, config = "apaquarto-jou",
              unknown = NULL)
  w <- tryCatch(apa_layout_decide(plan, geom = jou, geom_landscape = jou),
                warning = conditionMessage)
  expect_match(w, "documentmode: jou", fixed = TRUE)
})
