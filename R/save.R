#' Save apaTables tables to a Word (.docx) or HTML (.html) file
#'
#' \code{apa.save()} writes one or more finished tables to a single file --
#' the way to get an apaTables table into a manuscript you write in Word,
#' or to give a journal its tables as a file of their own:
#'
#' \preformatted{table1 <- apa.cor.table(attitude)
#' apa.save("Table1.docx", table1)}
#'
#' The file appears in your working directory -- the folder R is currently
#' pointing at. Open it in Word and the finished APA table is there: the
#' "Table N" line, the italic title, the body, and the \emph{Note.} below
#' it. Copy it into your manuscript, or send the file as it is. The format
#' is chosen by the file extension: \code{.docx} for Word, \code{.html}
#' for a web page (a legacy \code{.doc}/\code{.rtf} name is written as
#' \code{.docx}). \code{apa.save()} does not write PDF files.
#'
#' Several tables can go into one file, each on a page of its own, in the
#' order you list them:
#'
#' \preformatted{apa.save("AllTables.docx", table1, table2, table3)}
#'
#' A table left with its default number is numbered by its position in the
#' file -- Table 1, Table 2, Table 3 above -- while a number set when the
#' table was built is kept.
#'
#' Tables are set in 12 point, matching the manuscript body type APA style
#' expects. Each page is upright (portrait) unless the table is too wide
#' for it: \code{apa.save()} measures every table, and one that will not
#' fit upright is placed on a landscape page at full size -- columns are
#' never shrunk or cut off. To decide the orientation yourself, use the
#' \code{landscape} argument of the function that built the table -- for
#' example \code{apa.cor.table(attitude, landscape = TRUE)}; an explicit
#' value always wins over measurement.
#'
#' \strong{The title and the note are written from the table itself}, so
#' they stay correct when the analysis changes; there is no \code{title}
#' or \code{note} argument here at this time. If you need particular
#' wording, edit the finished Word file.
#'
#' @param filename Output filename ending in \code{.docx} or \code{.html}
#'   (a \code{.doc}/\code{.rtf} name is written as \code{.docx}).
#' @param ... One or more apaTables table objects to save.
#' @param paper Page size for \code{.docx}: \code{"us"} (default) or
#'   \code{"a4"}. Capitalization is forgiven (\code{"A4"} works); any other
#'   value is an error.
#' @return Invisibly returns the \code{filename} actually written.
#' @seealso
#' The functions that make the tables this saves:
#' \code{\link{apa.cor.table}}, \code{\link{apa.reg.table}},
#' \code{\link{apa.aov.table}}, \code{\link{apa.1way.table}},
#' \code{\link{apa.2way.table}}, \code{\link{apa.afex.table}}, and
#' \code{\link{apa.d.table}}.
#'
#' If your manuscript lives in a Quarto document -- text and analysis in
#' one file -- place tables with \code{\link{apa.quarto.pdf}} or
#' \code{\link{apa.quarto.docx}} there instead of saving separate files.
#'
#' \strong{The package website} has all of this documentation with rendered
#' examples: \url{https://dstanley4.github.io/apaTables/}.
#' @examples
#' library(apaTables)
#'
#' table1 <- apa.1way.table(iv = dose, dv = libido,
#'                          data = viagra, table.number = 1)
#'
#' apa.save(filename = file.path(tempdir(), "Table1.docx"), table1)
#'
#' # delete demo file
#' unlink(file.path(tempdir(), "Table1.docx"))
#' @export
apa.save <- function(filename, ..., paper = "us") {
     tables <- list(...)

     for (i in seq_along(tables)) {
          if (!inherits(tables[[i]], "apa_table")) {
               stop(sprintf("Argument %d is not an apa_table object. Use a function like apa.afex.table() or apa.reg.table() to create a table object first, then pass it to apa.save().", i + 1L))
          }
     }

     # "A4"/"US" are accepted by lowercasing -- the plausible innocent
     # spelling -- but anything else stops. This used to coerce silently to
     # "us", which handed a user who asked for A4 a US-letter page with
     # nothing anywhere saying so.
     if (!(is.character(paper) && length(paper) == 1L && !is.na(paper))) {
          stop("paper must be \"us\" or \"a4\".")
     }
     paper <- tolower(paper)
     if (!any(paper == c("us", "a4"))) {
          stop("paper must be \"us\" or \"a4\", not \"", paper, "\".")
     }

     ext <- tolower(tools::file_ext(filename))

     if (ext %in% c("doc", "rtf")) {
          filename <- sub("\\.[^.]+$", ".docx", filename)
          message("apaTables 4.0 writes Word files in .docx format (the legacy .doc/.rtf RTF engine was retired). Writing: ", filename)
          ext <- "docx"
     }

     # The file written here is its own document, so it must not be shaped by
     # whatever knitr happens to be rendering (e.g. apa.save() called from a
     # chunk of a PDF manuscript). Nothing apa_save_docx()/apa_save_html()
     # call reads knitr state -- measured by counting calls to the knitr
     # readers with options(knitr.in.progress = TRUE) set: zero. So no
     # wrapper that hides knitr state is needed here; if a knitr-aware
     # mechanism is ever added below, it must not see the calling document.
     if (ext == "docx") {
          apa_save_docx(filename, tables, paper)
     } else if (ext == "html") {
          apa_save_html(filename, tables)
     } else if (ext == "pdf") {
          stop("apa.save() does not write PDF. For a PDF table, place apa.quarto.pdf() in an apaquarto / Quarto document and render to PDF. See vignette(\"apaquarto-usage\", package = \"apaTables\").", call. = FALSE)
     } else {
          stop(sprintf("Unsupported file extension '.%s'. Use .docx or .html.", ext), call. = FALSE)
     }

     invisible(filename)
}


# Table numbers for the tables written to one document. A table built without
# an explicit table.number is numbered by its position in the document (1, 2,
# 3, ...); a table the user numbered keeps that number, and the numbering of
# the tables after it continues from there. A table whose number was
# suppressed (table.number = 0/NA/NULL) stays unnumbered, and is skipped by
# the count.
apa_document_numbers <- function(tables) {
     out <- rep(NA_integer_, length(tables))
     next_number <- 1L
     for (i in seq_along(tables)) {
          num <- tables[[i]]$table_number
          if (is.null(num) || length(num) != 1L || !is.numeric(num)) num <- NA_integer_
          num <- if (is.na(num)) NA_integer_ else as.integer(round(num))
          flag <- tables[[i]]$table_number_explicit
          # An object built before this field existed (or built by hand) keeps
          # whatever number it carries.
          explicit <- if (is.null(flag)) !is.na(num) else isTRUE(flag)
          if (explicit) {
               out[i] <- num
          } else {
               out[i] <- next_number
          }
          if (!is.na(out[i])) next_number <- out[i] + 1L
     }
     out
}


# Page section (orientation from the table's RESOLVED rotate flag, size from
# paper). Side margins are 0.5 in so wide tables (e.g., multi-block
# regression) fit inside the text area -- Word clips, not wraps, a table
# that crosses it.
#
# Named `rotate`, not `landscape`: every caller passes apa_save_rotate()'s
# plain logical (TRUE/FALSE, never NA), so the isTRUE() below is harmless --
# but the name `landscape` would claim a tri-state this parameter never is,
# and confusing the resolved flag with the builders' tri-state `landscape`
# argument is exactly the mistake to rule out.
apa_doc_section <- function(rotate, paper) {
     dims <- if (identical(paper, "a4")) c(8.27, 11.69) else c(8.5, 11)
     orient <- if (isTRUE(rotate)) "landscape" else "portrait"
     ps <- officer::page_size(width = dims[1], height = dims[2], orient = orient)
     pm <- officer::page_mar(top = 1, bottom = 1, left = 0.5, right = 0.5)
     officer::prop_section(page_size = ps, page_mar = pm, type = "nextPage")
}

# Usable text width (inches) for a table's page: page width minus the two
# 0.5 in side margins set in apa_doc_section(). Same rename as
# apa_doc_section() above and for the same reason: `rotate` is a resolved
# logical, not the tri-state `landscape` argument a builder accepts.
apa_usable_width <- function(rotate, paper) {
     dims <- if (identical(paper, "a4")) c(8.27, 11.69) else c(8.5, 11)
     (if (isTRUE(rotate)) dims[2] else dims[1]) - 1
}


# Does THIS table, in THIS document, want a rotated page?
#
# The rule is shared with the Quarto destinations; the ARITHMETIC IS NOT, and
# that is deliberate. apa.save() sets its tables at 12 pt on a page with 0.5 in
# side margins; the Quarto paths measure a 10 pt table against apaquarto's
# 6.5 in text width. The same table really is wider here -- bigger type, more
# padding -- so the same table can rotate in a saved .docx and stand upright in
# a Quarto PDF, and BOTH ARE CORRECT. A single shared measurement would judge
# this 12 pt table by a 10 pt LaTeX prediction: measured, apa.cor.table(attitude)
# draws 8.61 in here on a 7.5 in page, and that model calls it "fits".
#
# NOTE the measurement it uses is the one apa_save_docx() already computed for
# shrinking (its caller passes the SAME flextable_dim() width in, computed
# once). Nothing new is measured; a number that was only ever allowed to
# decide the font is now also allowed to decide the page.
apa_save_rotate <- function(width, landscape, paper) {
     # chunk = NA (the default, left unnamed here) deliberately: apa.save()
     # writes a standalone .docx/.html file, not a Quarto chunk, so there is
     # no `#| apa-landscape:` option to read and nothing to omit by mistake.
     requested <- apa_landscape_request(argument = landscape)
     if (!is.na(requested)) return(requested)
     width > apa_usable_width(FALSE, paper)
}

apa_save_docx <- function(filename, tables, paper = "us") {
     number_fp <- officer::fp_text(bold = TRUE,   font.family = "Times New Roman", font.size = 12)
     title_fp  <- officer::fp_text(italic = TRUE, font.family = "Times New Roman", font.size = 12)
     note_fp   <- officer::fp_text(italic = TRUE, font.family = "Times New Roman", font.size = 12)
     body_fp   <- officer::fp_text(italic = FALSE, font.family = "Times New Roman", font.size = 12)
     blank_par <- officer::fpar(officer::ftext("", body_fp))

     doc <- officer::read_docx()
     numbers <- apa_document_numbers(tables)
     for (i in seq_along(tables)) {
          tb <- tables[[i]]
          # APA 7: bold table number; title one double-spaced line below it;
          # a blank line between the table body and the note. A table with no
          # number (table.number = 0/NA/NULL) gets no number line at all.
          if (!is.na(numbers[i])) {
               doc <- officer::body_add_fpar(doc, officer::fpar(
                    officer::ftext(sprintf("Table %d", numbers[i]), number_fp)))
               doc <- officer::body_add_fpar(doc, blank_par)
          }
          doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(apa_caption(tb), title_fp)))
          ft <- apa_flextable(tb)
          width <- flextable::flextable_dim(ft)$widths
          rotate <- apa_save_rotate(width, tb$landscape, paper)
          usable <- apa_usable_width(rotate, paper)
          if (width > usable) {
               ft <- flextable::fit_to_width(ft, max_width = usable)
          }
          doc <- flextable::body_add_flextable(doc, ft, align = "left")
          doc <- officer::body_add_fpar(doc, blank_par)
          doc <- officer::body_add_fpar(doc, apa_note_fpar(tb, note_fp, body_fp))
          # Every table but the last closes its page with a section BREAK (an
          # empty paragraph carrying the section properties). The LAST table's
          # properties are written as the document's closing section instead:
          # a break there leaves officer's default body-level section -- A4,
          # empty -- after it, which Word renders as a blank trailing page
          # (measured in real Word's own PDF export). The closing section
          # carries the last table's own orientation, so a landscape final
          # table stays landscape.
          if (i < length(tables)) {
               doc <- officer::body_end_block_section(doc,
                         officer::block_section(apa_doc_section(rotate, paper)))
          } else {
               doc <- officer::body_set_default_section(doc,
                         apa_doc_section(rotate, paper))
          }
     }
     print(doc, target = filename)
}

# The Note paragraph: italic "Note" with a roman period (APA style), then each
# spec note entry with its richtext styling (italics, super/subscripts)
# preserved. Entries after the first start on their own line -- the
# significance-star sentence is its own entry, so it begins on a new line.
apa_note_fpar <- function(tb, note_fp, body_fp) {
     spec <- tryCatch(as_spec(tb), error = function(e) NULL)
     parts <- list(officer::ftext("Note", note_fp), officer::ftext(". ", body_fp))
     if (!is.null(spec) && length(spec$notes)) {
          for (k in seq_along(spec$notes)) {
               if (k > 1L) parts <- c(parts, list(officer::run_linebreak()))
               rt <- spec$notes[[k]]
               runs <- if (is.character(rt)) list(rt_run(rt)) else unclass(rt)
               parts <- c(parts, lapply(runs, rt_run_to_ftext))
          }
     } else {
          parts <- c(parts, list(officer::ftext(apa_note_text(tb), body_fp)))
     }
     do.call(officer::fpar, parts)
}

# One styled richtext run as an officer ftext (Word note rendering).
rt_run_to_ftext <- function(r) {
     va <- if (identical(r$script, "super")) "superscript"
           else if (identical(r$script, "sub")) "subscript"
           else "baseline"
     officer::ftext(r$text, officer::fp_text(
          italic = isTRUE(r$italic), bold = isTRUE(r$bold),
          vertical.align = va, font.family = "Times New Roman", font.size = 12))
}


# The HTML file carries the same APA furniture as the Word file: the "Table N"
# line and the italic title in the table's caption, and the note below the
# table. flextable renders a caption and footer lines natively in HTML, and the
# styled runs come from rt_run_to_chunk() -- the same conversion the Word path
# uses -- so italics and super/subscripts survive. Numbers come from
# apa_document_numbers(), so the same objects saved to .docx and to .html carry
# the same numbers.
apa_save_html <- function(filename, tables) {
     numbers <- apa_document_numbers(tables)
     args <- lapply(seq_along(tables),
                    function(i) apa_html_flextable(tables[[i]], numbers[i]))
     args$path <- filename
     do.call(flextable::save_as_html, args)
}

# One table as a flextable with its caption and note attached.
apa_html_flextable <- function(tb, number) {
     ft <- apa_flextable(tb)
     cap <- apa_html_caption_paragraph(tb, number)
     if (!is.null(cap)) ft <- flextable::set_caption(ft, caption = cap)
     notes <- apa_html_note_paragraphs(tb)
     for (k in seq_along(notes)) {
          ft <- flextable::add_footer_lines(ft, values = notes[[k]])
     }
     if (length(notes)) {
          ft <- flextable::border(ft, border = officer::fp_border(width = 0),
                                  part = "footer")
          ft <- flextable::font(ft, fontname = "Times New Roman", part = "footer")
          ft <- flextable::align(ft, align = "left", part = "footer")
     }
     ft
}

# A styled run as a flextable chunk at the 12 pt body size APA asks for.
# rt_run_to_chunk() leaves the size to be inherited (a table cell takes it from
# the table); a caption and a footer line inherit flextable's own default
# instead, so the size is set here.
apa_html_chunk <- function(r) {
     ch <- rt_run_to_chunk(r)
     ch$font.size <- 12
     ch
}

# Caption paragraph: "Table N" on its own line (omitted when the number is
# suppressed), then the title in italics, as APA 7 sets a table up.
apa_html_caption_paragraph <- function(tb, number) {
     spec <- tryCatch(as_spec(tb), error = function(e) NULL)
     runs <- if (!is.null(spec) && !is.null(spec$title)) {
          if (is.character(spec$title)) list(rt_run(spec$title)) else unclass(spec$title)
     } else list()
     # An APA table title is italic as a whole; any super/subscripts within it
     # are kept.
     runs <- lapply(runs, function(r) { r$italic <- TRUE; r })
     has_title <- length(runs) > 0 &&
          any(nzchar(vapply(runs, function(r) as.character(r$text), character(1))))
     if (is.na(number) && !has_title) return(NULL)
     parts <- list()
     if (!is.na(number)) {
          parts <- list(apa_html_chunk(rt_run(sprintf("Table %d", number))))
          if (has_title) parts <- c(parts, list(flextable::as_chunk("\n")))
     }
     if (has_title) parts <- c(parts, lapply(runs, apa_html_chunk))
     do.call(flextable::as_paragraph, parts)
}

# The note, one paragraph per line: italic "Note" and a roman period open the
# first line, and every further spec note entry -- the significance-star
# sentence, for one -- starts a line of its own, as it does in the Word file.
apa_html_note_paragraphs <- function(tb) {
     spec <- tryCatch(as_spec(tb), error = function(e) NULL)
     entries <- if (!is.null(spec) && length(spec$notes)) spec$notes else list()
     if (!length(entries)) return(list())
     out <- vector("list", length(entries))
     for (k in seq_along(entries)) {
          rt <- entries[[k]]
          runs <- if (is.character(rt)) list(rt_run(rt)) else unclass(rt)
          chunks <- lapply(runs, apa_html_chunk)
          if (k == 1L) {
               chunks <- c(list(apa_html_chunk(rt_run("Note", italic = TRUE)),
                                apa_html_chunk(rt_run(". "))), chunks)
          }
          out[[k]] <- do.call(flextable::as_paragraph, chunks)
     }
     out
}
