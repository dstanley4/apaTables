test_that("apa.save writes a valid .docx (PK zip magic)", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra, table.number = 1)
     tmp <- tempfile(fileext = ".docx")
     apa.save(filename = tmp, t1)
     expect_true(file.exists(tmp))
     expect_gt(file.info(tmp)$size, 0)
     expect_identical(readBin(tmp, "raw", 2), as.raw(c(0x50, 0x4b)))  # "PK"
     unlink(tmp)
})

# WHAT `landscape =` ACTUALLY DOES, asserted rather than described.
#
# `landscape =` is easy to mistake for a PDF-path argument and remove: it once
# nearly was, because nothing in the suite mentioned w:orient,
# apa_doc_section() or apa_usable_width() at all. It never fed the PDF path
# (apa_flextable() does not read x$landscape). It sets the WORD page, and
# removing it would turn every wide table's .docx portrait, where Word CLIPS
# rather than wraps.
#
# So the assertion reads the OOXML the user's file actually contains, not the R
# code that wrote it: w:pgSz inside the per-table section, at both values, on a
# correlation table and a regression table.
docx_orientations <- function(path) {
     ex <- tempfile("docxor"); dir.create(ex)
     on.exit(unlink(ex, recursive = TRUE), add = TRUE)
     utils::unzip(path, exdir = ex)
     xml <- paste(readLines(file.path(ex, "word", "document.xml"), warn = FALSE),
                  collapse = "")
     m <- regmatches(xml, gregexpr("<w:pgSz[^>]*>", xml))[[1]]
     m[grepl("w:orient", m)]
}

test_that("landscape = turns the Word page, and portrait leaves it upright", {
     blk1 <- lm(sales ~ adverts + airplay, data = album)
     cases <- list(cor = function(l) apa.cor.table(attitude, landscape = l),
                   reg = function(l) apa.reg.table(blk1, landscape = l))
     for (nm in names(cases)) {
          land <- tempfile(fileext = ".docx")
          port <- tempfile(fileext = ".docx")
          on.exit(unlink(c(land, port)), add = TRUE)
          apa.save(filename = land, cases[[nm]](TRUE))
          apa.save(filename = port, cases[[nm]](FALSE))

          lo <- docx_orientations(land)
          po <- docx_orientations(port)
          # one section per table, and it is the table's own
          expect_identical(length(lo), 1L, info = nm)
          expect_identical(length(po), 1L, info = nm)
          expect_match(lo[1], 'w:orient="landscape"', fixed = TRUE, info = nm)
          expect_match(po[1], 'w:orient="portrait"', fixed = TRUE, info = nm)
          # US letter turned on its side: 15840 x 12240 twips, not merely a
          # relabelled portrait page
          expect_match(lo[1], 'w:w="15840"', fixed = TRUE, info = nm)
          expect_match(lo[1], 'w:h="12240"', fixed = TRUE, info = nm)
          expect_match(po[1], 'w:w="12240"', fixed = TRUE, info = nm)
     }
})

# No blank trailing page. A section BREAK after the last table would leave officer's default body-level section -- A4, empty --
# at the end of the document, which Word renders as a blank page (measured in
# real Word's own PDF export). The last table's section properties are
# therefore the document's CLOSING section, so the
# document ends where the last note ends. These assertions read the OOXML
# the user's file actually contains: one <w:sectPr> per table, the last one
# at body level (not inside a paragraph, which is what a break is), and
# carrying the last table's own page -- including a landscape last table,
# the case where a wrong closing section would snap the table to portrait
# and clip it.
docx_sections <- function(path) {
     ex <- tempfile("docxsect"); dir.create(ex)
     on.exit(unlink(ex, recursive = TRUE), add = TRUE)
     utils::unzip(path, files = "word/document.xml", exdir = ex)
     xml <- paste(readLines(file.path(ex, "word", "document.xml"), warn = FALSE),
                  collapse = "")
     all <- regmatches(xml, gregexpr("(?s)<w:sectPr.*?</w:sectPr>", xml,
                                     perl = TRUE))[[1]]
     # a section break is a sectPr inside a paragraph's properties; the
     # closing section is the body-level one after the last paragraph
     breaks <- regmatches(xml, gregexpr(
          "(?s)<w:pPr>\\s*<w:sectPr.*?</w:sectPr>", xml, perl = TRUE))[[1]]
     list(all = all, breaks = breaks)
}

test_that("apa.save writes no blank trailing page: the last table's section closes the document", {
     oneway <- apa.1way.table(iv = dose, dv = libido, data = viagra)
     wide <- apa.cor.table(attitude, landscape = TRUE)

     # one table: its section IS the closing section; no break at all
     single <- tempfile(fileext = ".docx")
     on.exit(unlink(single), add = TRUE)
     apa.save(filename = single, oneway)
     s <- docx_sections(single)
     expect_length(s$all, 1)
     expect_length(s$breaks, 0)
     expect_match(s$all[1], 'w:orient="portrait"', fixed = TRUE)

     # two tables, landscape LAST: one break (after table 1, portrait), and
     # the closing section carries the landscape page
     land_last <- tempfile(fileext = ".docx")
     on.exit(unlink(land_last), add = TRUE)
     apa.save(filename = land_last, oneway, wide)
     s <- docx_sections(land_last)
     expect_length(s$all, 2)
     expect_length(s$breaks, 1)
     expect_match(s$breaks[1], 'w:orient="portrait"', fixed = TRUE)
     closing <- s$all[length(s$all)]
     expect_match(closing, 'w:orient="landscape"', fixed = TRUE)
     expect_match(closing, 'w:w="15840"', fixed = TRUE)
     expect_match(closing, 'w:h="12240"', fixed = TRUE)

     # the same pair the other way: closing section is portrait
     port_last <- tempfile(fileext = ".docx")
     on.exit(unlink(port_last), add = TRUE)
     apa.save(filename = port_last, wide, oneway)
     s <- docx_sections(port_last)
     expect_length(s$all, 2)
     expect_length(s$breaks, 1)
     expect_match(s$breaks[1], 'w:orient="landscape"', fixed = TRUE)
     expect_match(s$all[length(s$all)], 'w:orient="portrait"', fixed = TRUE)
})

test_that("a rotated Word page gives a wide table more room to fit into", {
     # The second thing rotation drives, via apa_usable_width() (R/save.R):
     # the width flextable::fit_to_width() aims at. A table that has to be
     # shrunk on an upright page must be shrunk LESS -- or not at all -- on a
     # rotated one. (Which page a table gets is apa_save_rotate()'s
     # decision, not a bare read of tb$landscape.)
     expect_equal(apa_usable_width(TRUE, "us"), 10)
     expect_equal(apa_usable_width(FALSE, "us"), 7.5)

     blk1 <- lm(sales ~ adverts + airplay, data = album)
     wide <- apa.reg.table(blk1)
     natural <- flextable::flextable_dim(apa_flextable(wide))$widths
     expect_gt(natural, apa_usable_width(FALSE, "us"))   # portrait must fit it
     expect_lt(natural, apa_usable_width(TRUE, "us"))    # landscape need not
})

test_that("apa.save writes a valid .html", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra, table.number = 1)
     tmp <- tempfile(fileext = ".html")
     apa.save(filename = tmp, t1)
     expect_true(file.exists(tmp))
     expect_gt(file.info(tmp)$size, 0)
     unlink(tmp)
})

test_that("apa.save coerces a .doc filename to .docx with a message", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra, table.number = 1)
     tmp <- tempfile(fileext = ".doc")
     expect_message(apa.save(filename = tmp, t1), "docx")
     docx <- sub("\\.doc$", ".docx", tmp)
     expect_true(file.exists(docx))
     expect_false(file.exists(tmp))
     unlink(docx)
})

test_that("apa.save errors on .pdf and points to Quarto", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra, table.number = 1)
     tmp <- tempfile(fileext = ".pdf")
     expect_error(apa.save(filename = tmp, t1), "Quarto|apa_flextable")
})

test_that("apa.save writes multiple tables into one .docx", {
     t1 <- apa.1way.table(iv = dose, dv = libido, data = viagra, table.number = 1)
     t2 <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                          data = goggles, table.number = 2)
     tmp <- tempfile(fileext = ".docx")
     apa.save(filename = tmp, t1, t2)
     expect_true(file.exists(tmp))
     expect_gt(file.info(tmp)$size, 0)
     unlink(tmp)
})

test_that("apa.save's Word output does not vary with knit context (regression net)", {
     # apa.save() writes a document of its own -- a 10 in landscape Word page
     # -- so nothing about it may depend on what knitr happens to be
     # rendering: calling apa.save() from a chunk of a PDF manuscript once
     # ran the LaTeX width fitter over it, and a two-block regression table
     # came out at 6 pt instead of 11 pt, silently, in the file the user
     # submits.
     #
     # Nothing in apa.save()'s call chain branches on knit context today, so
     # this is a regression net, not proof that a specific guard works. The
     # "inside" call below runs with options(knitr.in.progress = TRUE) and
     # out.format = "latex" set -- exactly what apa_latex_target() reads --
     # and its bytes must still match the "outside" call's.
     blk1 <- lm(sales ~ adverts + airplay, data = album)
     blk2 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data = album)
     t3 <- apa.reg.table(blk1, blk2)

     outside <- tempfile(fileext = ".docx")
     inside <- tempfile(fileext = ".docx")
     on.exit(unlink(c(outside, inside)), add = TRUE)
     apa.save(filename = outside, t3)

     op <- options(knitr.in.progress = TRUE)
     old <- knitr::opts_knit$get("out.format")
     knitr::opts_knit$set(out.format = "latex")
     on.exit({ options(op); knitr::opts_knit$set(out.format = old) },
             add = TRUE, after = FALSE)
     expect_true(knitr::is_latex_output())
     apa.save(filename = inside, t3)

     member <- function(f, m) {
          con <- unz(f, m, open = "rb")
          on.exit(close(con))
          readBin(con, "raw", n = 5e7)
     }
     # docProps/core.xml carries the creation timestamp, which differs
     # between any two writes; everything that describes the document itself
     # must match byte for byte.
     names_of <- function(f) {
          n <- utils::unzip(f, list = TRUE)$Name
          sort(n[n != "docProps/core.xml"])
     }
     expect_identical(names_of(outside), names_of(inside))
     for (m in names_of(outside)) {
          expect_identical(member(inside, m), member(outside, m), info = m)
     }

     # apa.save() must not add anything to the knit_meta registry of the
     # document it happens to be called from -- saving a Word file from a
     # chunk must not add a LaTeX package (or anything else) to the
     # manuscript around it. Checked by construction rather than by reading:
     # the registry is emptied, apa.save() runs inside the knit, and it must
     # still be empty afterwards.
     #
     # Nothing in apa_flextable()'s call chain calls knit_meta_add() today,
     # so this too is a regression net for anything that might add a
     # dependency in future, not proof of a specific guard.
     knitr::knit_meta(clean = TRUE)
     apa.save(filename = inside, t3)
     meta <- knitr::knit_meta(clean = TRUE)
     expect_length(meta, 0)
})

test_that("apa.save rejects non-apa_table arguments", {
     expect_error(apa.save(filename = tempfile(fileext = ".docx"), 42),
                  "apa_table")
})


# ---------------------------------------------------------------------------
# HTML output: APA furniture (number, italic title, note)
# ---------------------------------------------------------------------------

# Small parsers, so the assertions read the emitted HTML rather than the code
# that produced it.
html_text <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
html_captions <- function(h) {
     regmatches(h, gregexpr("(?s)<caption.*?</caption>", h, perl = TRUE))[[1]]
}
html_footers <- function(h) {
     regmatches(h, gregexpr("(?s)<tfoot.*?</tfoot>", h, perl = TRUE))[[1]]
}
html_footer_rows <- function(foot) {
     rows <- regmatches(foot, gregexpr("(?s)<tr.*?</tr>", foot, perl = TRUE))[[1]]
     trimws(gsub("<[^>]+>", "", rows))
}
html_strip <- function(x) trimws(gsub("<[^>]+>", " ", x))
# The CSS rule flextable wrote for the <span> that holds `txt`.
html_style_of <- function(h, fragment, txt) {
     cls <- regmatches(fragment,
                       regexpr(paste0("class=\"(cl-[0-9a-f]+)\">", txt),
                               fragment, perl = TRUE))
     cls <- sub("^class=\"", "", sub(paste0("\">", txt, "$"), "", cls))
     if (!length(cls) || !nzchar(cls)) return("")
     rule <- regmatches(h, regexpr(paste0("\\.", cls, "\\{[^}]*\\}"), h))
     if (length(rule)) rule else ""
}

test_that("apa.save .html carries the table number, italic title, and note", {
     t1 <- apa.cor.table(attitude[, 1:3], table.number = 1)
     tmp <- tempfile(fileext = ".html")
     apa.save(filename = tmp, t1)
     h <- html_text(tmp)

     caps <- html_captions(h)
     expect_length(caps, 1)
     expect_match(html_strip(caps[1]), "Table 1", fixed = TRUE)
     expect_match(html_strip(caps[1]), "Descriptive Statistics and Correlations",
                  fixed = TRUE)
     # number and title on separate lines
     expect_match(caps[1], "<br>", fixed = TRUE)
     # the title is italic; the number is not
     expect_match(html_style_of(h, caps[1], "Descriptive Statistics and Correlations"),
                  "font-style:italic", fixed = TRUE)
     expect_match(html_style_of(h, caps[1], "Table 1"),
                  "font-style:normal", fixed = TRUE)

     foots <- html_footers(h)
     expect_length(foots, 1)
     rows <- html_footer_rows(foots[1])
     expect_length(rows, 2)                       # note, then the stars sentence
     expect_match(rows[1], "^Note\\.")
     expect_match(rows[1], "M = mean", fixed = TRUE)
     expect_match(rows[2], "indicates", fixed = TRUE)
     unlink(tmp)
})

test_that("apa.save .html gives every table in a multi-table file its furniture", {
     t1 <- apa.cor.table(attitude[, 1:3])
     t2 <- apa.reg.table(lm(sales ~ adverts, data = album))
     tmp <- tempfile(fileext = ".html")
     apa.save(filename = tmp, t1, t2)
     h <- html_text(tmp)

     caps <- html_captions(h)
     expect_length(caps, 2)
     expect_match(html_strip(caps[1]), "Table 1", fixed = TRUE)
     expect_match(html_strip(caps[2]), "Table 2", fixed = TRUE)
     expect_length(html_footers(h), 2)
     unlink(tmp)
})

test_that("apa.save .html numbering agrees with the Word file", {
     t1 <- apa.cor.table(attitude[, 1:3])            # default -> numbered by position
     t2 <- apa.cor.table(attitude[, 1:3], table.number = 7)   # explicit -> kept
     expect_equal(apa_document_numbers(list(t1, t2)), c(1L, 7L))

     html <- tempfile(fileext = ".html")
     docx <- tempfile(fileext = ".docx")
     apa.save(filename = html, t1, t2)
     apa.save(filename = docx, t1, t2)

     h <- html_text(html)
     expect_equal(sort(unique(regmatches(h, gregexpr("Table [0-9]+", h))[[1]])),
                  c("Table 1", "Table 7"))

     xml_dir <- tempfile(); dir.create(xml_dir)
     utils::unzip(docx, files = "word/document.xml", exdir = xml_dir)
     w <- paste(readLines(file.path(xml_dir, "word", "document.xml"), warn = FALSE),
                collapse = "")
     expect_equal(sort(unique(regmatches(w, gregexpr("Table [0-9]+", w))[[1]])),
                  c("Table 1", "Table 7"))
     unlink(c(html, docx)); unlink(xml_dir, recursive = TRUE)
})

test_that("apa.save .html writes no number when the number is suppressed", {
     t1 <- apa.cor.table(attitude[, 1:3], table.number = 0)
     tmp <- tempfile(fileext = ".html")
     apa.save(filename = tmp, t1)
     h <- html_text(tmp)
     # (the word "Table" alone appears in the boilerplate flextable embeds, so
     # the test looks for a numbered one, and for any "Table" in the caption)
     expect_length(regmatches(h, gregexpr("Table [0-9]+", h))[[1]], 0)
     caps <- html_captions(h)
     expect_false(grepl("Table", caps[1], fixed = TRUE))
     expect_length(caps, 1)
     expect_match(html_strip(caps[1]), "Descriptive Statistics and Correlations",
                  fixed = TRUE)
     expect_length(html_footers(h), 1)             # the note is still there
     unlink(tmp)
})

test_that("apa.save .html keeps rich text in the note (italics, superscripts)", {
     t1 <- apa.reg.table(lm(sales ~ adverts, data = album))
     tmp <- tempfile(fileext = ".html")
     apa.save(filename = tmp, t1)
     h <- html_text(tmp)
     foot <- html_footers(h)[1]
     # "R2" in the note is written as an italic R with a superscript 2; the
     # styling lives in the CSS rules for the classes used inside the footer
     classes <- unique(regmatches(foot,
          gregexpr("(?<=class=\")cl-[0-9a-f]+", foot, perl = TRUE))[[1]])
     rules <- vapply(classes, function(cl) {
          r <- regmatches(h, regexpr(paste0("\\.", cl, "\\{[^}]*\\}"), h))
          if (length(r)) r else ""
     }, character(1))
     expect_true(any(grepl("vertical-align:super", rules, fixed = TRUE)))
     expect_true(any(grepl("font-style:italic", rules, fixed = TRUE)))
     unlink(tmp)
})

test_that("paper is validated: capitalization forgiven, anything else an error (docs audit)", {
     # Before 2026-08-09 an unrecognized paper value was SILENTLY coerced
     # to "us" -- so paper = "A4", the plausible innocent spelling, handed
     # the user a US-letter page with nothing anywhere saying so. Now the
     # case typo is forgiven by lowercasing and everything else stops
     # naming the two valid values.
     tb <- apa.cor.table(attitude[, 1:3])
     f <- tempfile(fileext = ".docx")
     on.exit(unlink(f), add = TRUE)
     apa.save(filename = f, tb, paper = "A4")
     ex <- tempfile("papercase"); dir.create(ex)
     on.exit(unlink(ex, recursive = TRUE), add = TRUE)
     utils::unzip(f, exdir = ex)
     xml <- paste(readLines(file.path(ex, "word", "document.xml"), warn = FALSE),
                  collapse = "")
     # A4 portrait width in twips -- the page "A4" used to silently lose
     expect_match(xml, 'w:w="11909"', fixed = TRUE)

     expect_error(apa.save(filename = f, tb, paper = "letter"),
                  'paper must be "us" or "a4", not "letter"', fixed = TRUE)
     expect_error(apa.save(filename = f, tb, paper = NA),
                  'paper must be "us" or "a4"', fixed = TRUE)
     expect_error(apa.save(filename = f, tb, paper = c("us", "a4")),
                  'paper must be "us" or "a4"', fixed = TRUE)
})
