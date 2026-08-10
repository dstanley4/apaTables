#' Create an APA-style correlation table
#'
#' \code{apa.cor.table()} creates a table of means, standard deviations, and
#' Pearson correlations in APA style. Each correlation is reported with its
#' 95\% confidence interval and, optionally, significance stars and a
#' p-value. Typing \code{apa.cor.table(attitude)} prints the finished table
#' at the console. The most common next step is saving the table as a Word
#' file to use in a manuscript:
#'
#' \preformatted{table1 <- apa.cor.table(attitude)
#' apa.save("Table1.docx", table1)}
#'
#' The file appears in your working directory -- the folder R is currently
#' pointing at. See the \emph{Word files (apa.save)} section below. The
#' table can also be placed in a Quarto document, so a rendered manuscript
#' comes out with the table already in place -- see the \emph{Quarto
#' documents} section below.
#'
#' @param data A data frame. Non-numeric columns are dropped before the
#'   table is constructed.
#' @param filename Optional file name. If supplied, the table is also saved
#'   as a Word (\code{.docx}) or HTML (\code{.html}) file, according to the
#'   file extension. Legacy \code{.doc}/\code{.rtf} names are saved as
#'   \code{.docx}.
#' @param table.number Number for the "Table N" line of the output. Defaults
#'   to \code{1}. Use \code{0} (or \code{NA}, or \code{NULL}) for a table with
#'   no number: none is then shown at the console or written to the Word file.
#'   When \code{\link{apa.save}} writes several tables to one file, a table
#'   left at the default is numbered by its position in that file, while a
#'   number given here is kept. In a Quarto document that uses the apaquarto
#'   extension this value is ignored: apaquarto numbers the tables itself, in
#'   the order they appear in the document.
#' @param show.conf.interval Deprecated and ignored; confidence intervals
#'   are always displayed. This argument will be removed in a later version.
#' @param show.sig.stars If \code{TRUE} (the default), correlations are
#'   marked with stars for significance (* p < .05, ** p < .01).
#' @param show.pvalue If \code{TRUE} (the default), the p-value for each
#'   correlation is displayed below its confidence interval.
#' @param landscape Whether this table is placed on a rotated (landscape)
#'   page. The default, \code{NA}, means apaTables decides by measuring the
#'   table against the page. Pass \code{TRUE} to insist on a rotated page or
#'   \code{FALSE} to insist on an upright one. An explicit value is honoured
#'   wherever the destination can carry it out; in a Quarto PDF that means
#'   the chunk needs a \code{tbl-} label (see \code{\link{apa.quarto.pdf}}).
#'   A table too wide for the page it is given is shrunk to fit rather
#'   than having a column cut off. Landscape is a property of a PAGE,
#'   so it applies to PDF and Word output and is ignored, without complaint,
#'   for HTML and slides. See \strong{Rotated (landscape) pages} below.
#'
#' @details
#' For each variable the table reports the number of cases (\emph{N}), mean
#' (\emph{M}), and standard deviation (\emph{SD}), computed after removing
#' missing values for that variable. Correlations and their confidence
#' intervals are computed pairwise with \code{\link[stats]{cor.test}}, so
#' each correlation uses all cases that are complete for that pair of
#' variables.
#'
#' @return A table object (class \code{apa_table}). Typing its name at the
#'   console displays the table. Pass it to \code{\link{apa.save}} to create
#'   a Word or HTML file. Or, if you are working in a Quarto document, use
#'   \code{\link{apa.quarto.pdf}} or \code{\link{apa.quarto.docx}} to make
#'   PDF and Word manuscripts, respectively.
#'
#' @section Word files (apa.save):
#' \code{\link{apa.save}} writes the finished table to a Word file you can
#' open, copy from, or send -- the most common way to use an apaTables
#' table:
#'
#' \preformatted{table1 <- apa.cor.table(attitude)
#' apa.save("Table1.docx", table1)}
#'
#' The file appears in your working directory -- the folder R is currently
#' pointing at. Several tables can go into one file, each on its own page,
#' numbered Table 1, Table 2, and so on in the order you list them:
#'
#' \preformatted{apa.save("AllTables.docx", table1, table2)}
#'
#' The title above each table and the note below it are written by
#' apaTables from the table itself; on this route there is no way to
#' supply your own wording at this time. Tables are set in 12 point, and a
#' table too wide for an upright page gets a rotated (landscape) page of
#' its own rather than smaller type -- so a wide correlation table coming
#' out sideways is expected, not an error.
#'
#' @section Quarto documents:
#' Quarto (\url{https://quarto.org}) is a free system for writing documents
#' that combine text and R code. The apaquarto extension makes a Quarto
#' document follow APA style: place this table in such a document and it
#' appears as a fully formatted APA table -- number, title, and note
#' included -- in Word, PDF, or HTML. The extension must be installed beside the document, and one command does
#' it. \code{\link{apa.quarto.new}("my-paper")} creates a new manuscript
#' folder with the extension already installed; for a folder you already
#' have, \code{\link{apa.quarto.setup}()} installs the extension there
#' instead.
#' Tables go into the document inside a
#' "code chunk": a small block of R code with a few settings lines at the
#' top. "Rendering" the document turns it into the finished Word, PDF, or
#' HTML file.
#'
#' Every apaquarto document begins with a YAML header: the settings block
#' between \code{---} lines at the very top of the document. apaquarto
#' needs an author there -- with no author, or an empty one, the render
#' stops with a LaTeX error -- and it reports a problem unless that author
#' has an affiliation and is marked \code{corresponding: true}. This
#' header is enough to render a PDF manuscript:
#'
#' \preformatted{---
#' title: "My Paper"
#' author:
#'   - name: Jane Doe
#'     corresponding: true
#'     affiliations:
#'       - name: University of Guelph
#' format: apaquarto-pdf
#' ---}
#'
#' For a Word manuscript, the same header with one line changed:
#'
#' \preformatted{---
#' title: "My Paper"
#' author:
#'   - name: Jane Doe
#'     corresponding: true
#'     affiliations:
#'       - name: University of Guelph
#' format: apaquarto-docx
#' ---}
#'
#' There are four format-named functions for this -- \code{\link{apa.quarto.pdf}},
#' \code{\link{apa.quarto.docx}}, \code{\link{apa.quarto.html}} and
#' \code{\link{apa.quarto.revealjs}} -- one for each Quarto destination, and
#' they behave identically: the document's own
#' YAML \code{format:} line decides what is actually produced, so the name only
#' states intent. This page's examples use \code{apa.quarto.pdf()} to match the
#' \code{format: apaquarto-pdf} shown above; swap in \code{apa.quarto.docx()} or
#' \code{apa.quarto.html()} together with the matching YAML line for Word or HTML.
#'
#' In the document, the table lives inside a code chunk like this one.
#' \code{\link{apa.quarto.pdf}} displays the table body with its title and
#' note included automatically, so this chunk is complete -- there is no
#' title or note line to write, and the wording stays correct when the
#' analysis changes. The \code{library(apaTables)} line is part of the
#' chunk: without it the document cannot find \code{apa.cor.table()} and
#' the render stops.
#'
#' \preformatted{```{r}
#' #| label: tbl-correlations
#' #| message: false
#' #| warning: true
#' library(apaTables)
#' table1 <- apa.cor.table(attitude)
#' apa.quarto.pdf(table1)
#' ```}
#'
#' That \code{#| label: tbl-correlations} line is not optional: without a
#' label starting \code{tbl-}, Quarto builds no float for the chunk, so there
#' is no number, no title, and no note, however you try to supply them.
#'
#' \emph{Wide and tall tables.} If this table turns out wider than the upright
#' (portrait) PDF page, APA style places it on a rotated (landscape) page --
#' and apaTables does that for you. It measures the table and asks apaquarto
#' for a rotated page when one is needed, whether your tables sit in the text
#' or are collected at the end of the manuscript. There is nothing to add to the chunk and nothing to add to the document's
#' YAML header. A table wider still -- wider than even a rotated page -- is
#' printed smaller instead, as small as it has to be so
#' that no column is cut off. If that takes the type below 6 points, a
#' warning in the rendered draft says so and points to \code{\link{apa.save}}
#' for a full-size Word version. A table too tall for its page needs nothing
#' either: it continues onto the next page by itself, with the column
#' headings repeated at the top of each continuation and the note below the
#' last piece. None of this applies to \code{\link{apa.save}} files: there, each table
#' sits on a page of its own, always at full size.
#'
#' To see this table (and every apaTables table type) in a complete
#' working document, type
#' \code{apa.quarto.example("apaTables-examples.qmd")} in the R console.
#' Two ready-to-render example documents appear in your working directory,
#' with the apaquarto extension installed beside them -- one per
#' destination:
#'
#' \preformatted{apaTables-examples-pdf.qmd
#' apaTables-examples-docx.qmd}
#'
#' Render the \code{-pdf} file for a PDF manuscript, the \code{-docx} file
#' for a Word manuscript.
#'
#' @section Rotated (landscape) pages:
#' A table too wide to stand upright is placed on a rotated page automatically.
#' You can overrule that per table with \code{landscape = TRUE} or \code{FALSE},
#' and per chunk in a Quarto document with \code{#| apa-landscape: true} or
#' \code{false}, which wins over the argument.
#'
#' \strong{The same table can be rotated in one output and upright in
#' another, and both are right.} \code{apa.save()} sets tables in 12 point on
#' a page with half inch side margins; a Quarto manuscript sets them in 10
#' point inside a one inch text block. The table really is wider in the Word
#' file, so it can need a rotated page there while fitting upright in the
#' PDF. Each destination measures the table it is actually going to draw.
#'
#' @seealso
#' \strong{New to Quarto?} \code{\link{apa.quarto.example}} writes a complete,
#' ready-to-render example document with one apaTables table per page -- the
#' fastest way to see every table type working in an apaquarto document. In
#' the R console, type:
#'
#' \preformatted{apa.quarto.example("apaTables-examples.qmd")}
#'
#' \strong{Prefer a written guide?} A step-by-step guide to placing these
#' tables in Quarto documents comes installed with the package. To open it,
#' type:
#'
#' \preformatted{vignette("apaquarto-usage", package = "apaTables")}
#'
#' \strong{The package website} has all of this documentation with rendered
#' examples: \url{https://dstanley4.github.io/apaTables/}.
#'
#' Other functions used with this table: \code{\link{apa.save}} to save it as
#' a Word or HTML file; \code{\link{apa.quarto.template}} and
#' \code{\link{apa.quarto.pdf}} to place it in a Quarto document.
#' @examples
#' library(apaTables)
#'
#' # Means, standard deviations, and correlations for the attitude data
#' table1 <- apa.cor.table(attitude)
#' table1
#'
#' # Word documents ----------------------------------------
#' # Save the table as a .docx file. This example writes to a temporary
#' # folder; for real use give an ordinary name, such as
#' # apa.save("Table1.docx", table1), and the file appears in your
#' # working directory.
#' apa.save(file.path(tempdir(), "table1.docx"), table1)
#'
#' # Quarto documents ----------------------------------------
#' # Print a ready-to-paste apaquarto code chunk:
#' apa.quarto.template(table1)
#'
#' # Or write your own chunk; apa.quarto.pdf() displays the table body:
#' apa.quarto.pdf(table1)
#'
#' # Clean up
#' unlink(file.path(tempdir(), "table1.docx"))
#' @export
apa.cor.table<-function(data, filename = NA, table.number = 1, show.conf.interval = TRUE, show.sig.stars = TRUE, show.pvalue = TRUE, landscape = NA) {

     data <- as.data.frame(data)
     table_number_explicit <- !missing(table.number)
     table_number <- apa_table_number_value(table.number)
     table.number <- table_number

     if (show.conf.interval == FALSE) {
          message("The ability to suppress reporting of confidence intervals has been deprecated in this version.\nThe function argument show.conf.interval will be removed in a later version.")
     }
     show.conf.interval <- TRUE

     show_conf_interval <- show.conf.interval
     show_stars <- show.sig.stars
     show_pvalue <- show.pvalue

     if (is.na(filename)) {
          make_file_flag <- FALSE
     } else {
          make_file_flag <-TRUE
     }

     df_col <- dim(data)[2]
     column_is_numeric <- c()
     for (i in 1:df_col) {
          column_is_numeric[i] <- is.numeric(data[,i])
     }
     data <- data[,column_is_numeric]


     number_variables <- ncol(data)
     number_columns   <- number_variables -1

     output_cor     <- matrix(" ", number_variables, number_columns)
     output_cor_latex <- matrix(" ", number_variables, number_columns)

     output_pvalue     <- matrix(" ", number_variables, number_columns)
     output_pvalue_latex <- matrix(" ", number_variables, number_columns)


     output_ci     <- matrix(" ", number_variables, number_columns)
     output_ci_latex <- matrix(" ", number_variables, number_columns)

     output_descriptives   <- matrix(" ",number_variables,3)
     output_variable_names <- paste(as.character(1:number_variables),". ",names(data),sep="")

     for (i in 1:number_variables) {
          output_descriptives[i,1] <- as.character(sum(!is.na(data[,i]), na.rm=TRUE) )
          output_descriptives[i,2] <- txt.number(mean(data[,i], na.rm=TRUE))
          output_descriptives[i,3] <- txt.number(sd(data[,i], na.rm=TRUE))

          for(j in 1:number_variables) {
               if ((j<i)) {
                    x <- data[,i]
                    y <- data[,j]
                    ctest      <- cor.test(x, y)

                    cor_string <- txt.r(ctest, show_stars)
                    output_cor[i,j]        <- cor_string
                    output_cor_latex[i,j]    <- txt.r.latex(ctest, show_stars)

                    output_pvalue[i,j]       <- txt.r.p.console(ctest)
                    output_pvalue_latex[i,j] <- txt.r.p.latex(ctest)

                    cor_ci_string  <- txt.ci(ctest)
                    output_ci[i,j] <- cor_ci_string
                    output_ci_latex[i,j] <- cor_ci_string

               } #end lower triangle
          }#end j
     }#end i


     #weave
     left_padding   <- c(" ", " ", " "," ")
     first_line     <- c(output_variable_names[1], output_descriptives[1,], output_cor[1,])
     first_line_latex     <- c(output_variable_names[1], output_descriptives[1,], output_cor[1,])

     second_line     <- c(left_padding, output_ci[1,])
     second_line_latex    <- c(left_padding, output_ci[1,])

     extra_line <- c(left_padding, output_pvalue[1,])
     extra_line_latex <- c(left_padding, output_pvalue[1,])

     third_line <- rep(" ", length(second_line))


     new_lines <- first_line
     new_lines_latex <- first_line_latex

     if (show_conf_interval==TRUE) {
          new_lines     <- rbind(new_lines, second_line)
          new_lines_latex <- rbind(new_lines_latex, second_line_latex)
     }

     if (show_pvalue==TRUE) {
          new_lines     <- rbind(new_lines, extra_line)
          new_lines_latex <- rbind(new_lines_latex, extra_line_latex)
     }

     new_lines       <- rbind(new_lines, third_line)
     new_lines_latex <- rbind(new_lines_latex, third_line)

     output_matrix_console <- new_lines
     output_matrix_latex <-  new_lines_latex

     # output_matrix_console <- rbind(first_line, second_line)
     for (i in 2:number_variables) {
          first_line <- c(output_variable_names[i], output_descriptives[i,], output_cor[i,])
          first_line_latex <- c(output_variable_names[i], output_descriptives[i,], output_cor_latex[i,])

          second_line <- c(left_padding, output_ci[i,])
          second_line_latex <- c(left_padding, output_ci_latex[i,])

          extra_line <- c(left_padding, output_pvalue[i,])
          extra_line_latex <- c(left_padding, output_pvalue_latex[i,])

          third_line <- rep(" ", length(second_line))

          new_lines <- first_line
          new_lines_latex <- first_line_latex
          if (show_conf_interval==TRUE) {
               new_lines     <- rbind(new_lines, second_line)
               new_lines_latex     <- rbind(new_lines_latex, second_line_latex)
          }

          if (show_pvalue==TRUE) {
               new_lines     <- rbind(new_lines, extra_line)
               new_lines_latex     <- rbind(new_lines_latex, extra_line_latex)
          }

          new_lines     <- rbind(new_lines, third_line)
          new_lines_latex     <- rbind(new_lines_latex, third_line)

          output_matrix_console <- rbind(output_matrix_console, new_lines)
          output_matrix_latex <- rbind(output_matrix_latex, new_lines_latex)
     }



     rownames(output_matrix_console) <- 1:nrow(output_matrix_console)
     colnames(output_matrix_console) <- c(c("Variable","N", "M","SD"), as.character(1:number_columns))

     rownames(output_matrix_latex) <- rownames(output_matrix_console)
     colnames(output_matrix_latex) <- colnames(output_matrix_console)
     #done making input


     table_title <- "Descriptive Statistics and Correlations\n"


     #make table
     row_with_colnames <- colnames(output_matrix_console)
     df_temp <- data.frame(output_matrix_console, stringsAsFactors = FALSE)
     rownames(output_matrix_console) <- rep(" ", length((rownames(output_matrix_console))))
     table_body <- output_matrix_console

     #make console output
     if (show_conf_interval == TRUE) {
          table_note <- "Note. N = number of cases. M = mean. SD = standard deviation.\nValues in square brackets indicate the 95% confidence interval.\n"
     } else {
          table_note <- "Note. N = number of cases. M = mean. SD = standard deviation.\n"
     }

     if (show_stars == TRUE) {
             table_note <- paste(table_note, "* indicates p < .05. ** indicates p < .01.\n")
     }

     tbl.console <- list(table_number = table_number,
                 table_title = table_title,
                 table_body = table_body,
                 table_note = table_note)
     class(tbl.console) <- "apa_table"


     if (show_conf_interval == TRUE) {
          table_title <- "Descriptive Statistics and Correlations"
          table_note_latex <- "\\\\textit{Note}. \\\\textit{N} = number of cases. \\\\textit{M} = mean. \\\\textit{SD} = standard deviation. Square brackets = 95\\\\% confidence interval."
     } else {
          table_title <- "Descriptive Statistics and Correlations"
          table_note_latex <- "\\\\textit{Note}. \\\\textit{N} = number of cases. \\\\textit{M} = mean. \\\\textit{SD} = standard deviation."
     }

     if (show_stars == TRUE) {
             table_note_latex <- paste(table_note_latex, "\\\\newline  * indicates $p$ < .05. ** indicates $p$ < .01.")
     }


     tbl.console$landscape   <- landscape



     output_matrix_latex[,1] <- gsub("_", " ", output_matrix_latex[,1])

     tbl.console$latex.body <- output_matrix_latex
     tbl.console$latex.table.title <- table_title
     tbl.console$latex.table.note  <- table_note_latex
     tbl.console$table.type        <- "correlation"


     tbl.console$spec <- cor_build_spec(
          variable_names     = output_variable_names,
          descriptives       = output_descriptives,
          cor_mat            = output_cor,
          ci_mat             = output_ci,
          pvalue_mat         = output_pvalue,
          show_conf_interval = show_conf_interval,
          show_pvalue        = show_pvalue,
          show_stars         = show_stars,
          table_title        = tbl.console$latex.table.title,
          table_number       = table_number,
          landscape          = landscape)

     tbl.console <- apa_set_table_number(tbl.console, table_number, table_number_explicit)

     if (make_file_flag == TRUE) {
          apa.save(filename = filename, tbl.console)
     }

     return(tbl.console)
}#end function

# Correlation-table note as richtext (no "Note." prefix). Mirrors the console
# note: N/M/SD definitions, the CI sentence iff show_conf_interval, the stars
# sentence iff show_stars. The console note does not mention p-values.
cor_spec_note <- function(show_conf_interval) {
  runs <- list(
    rt_run("N", italic = TRUE),  rt_run(" = number of cases. "),
    rt_run("M", italic = TRUE),  rt_run(" = mean. "),
    rt_run("SD", italic = TRUE), rt_run(" = standard deviation. ")
  )
  if (isTRUE(show_conf_interval)) {
    runs <- c(runs, list(rt_run("Values in square brackets indicate the 95% confidence interval.")))
  }
  richtext(runs)
}

# The stacked p line of a correlation cell, with the statistic italic.
#
# APA sets a statistic's symbol in italics wherever it appears, in the body of a
# table exactly as in its note -- and the note already does this, because it is
# built from apa_sym("p"). The body did not: the formatted string ("p < .001",
# "p = .019") went in as one plain run, so every one of the 21 p cells in
# apa.cor.table(attitude) emitted an upright p. That was a SPEC-level defect, so
# it reached Word, HTML and PDF alike; no emitter could have corrected it,
# because none of them can tell a statistic from ordinary text once the italic
# flag is gone.
#
# The leading symbol is split off and marked italic; everything after it -- the
# comparison and the value -- stays roman, which is what APA prescribes. The
# regex is anchored so a string that does not begin with a p is returned
# unchanged rather than silently reshaped.
cor_pvalue_richtext <- function(p_str) {
  m <- regmatches(p_str, regexec("^(\\s*)(p)(\\s*[<=].*)$", p_str))[[1]]
  if (length(m) != 4L) return(richtext(p_str))
  runs <- list(rt_run(m[3], italic = TRUE), rt_run(m[4]))
  if (nzchar(m[2])) runs <- c(list(rt_run(m[2])), runs)
  richtext(runs)
}

# Build an apa_table_spec from the correlation builder's already-formatted
# matrices. One spec row per variable; correlation cells stack r over [CI] over p
# (whichever the flags include) via richtext_lines. Descriptive cells are single
# line. Lower-triangle blanks become empty cells.
cor_build_spec <- function(variable_names, descriptives, cor_mat, ci_mat, pvalue_mat,
                           show_conf_interval, show_pvalue, show_stars,
                           table_title, table_number, landscape) {
  n_var  <- length(variable_names)
  n_corr <- ncol(cor_mat)                 # number_variables - 1

  spec <- apa_spec_start(n_var)
  spec <- apa_col(spec, label = richtext("Variable"), align = "l",
                  values = as.list(variable_names))
  spec <- apa_col(spec, label = apa_sym("N"),  align = "r",
                  values = as.list(as.character(descriptives[, 1])))
  spec <- apa_col(spec, label = apa_sym("M"),  align = "r",
                  values = as.list(as.character(descriptives[, 2])))
  spec <- apa_col(spec, label = apa_sym("SD"), align = "r",
                  values = as.list(as.character(descriptives[, 3])))

  for (j in seq_len(n_corr)) {
    cells <- vector("list", n_var)
    for (i in seq_len(n_var)) {
      r_str <- cor_mat[i, j]
      if (!nzchar(trimws(r_str))) {
        cells[[i]] <- richtext("")                       # blank (upper triangle / diagonal)
      } else {
        lines <- list(richtext(r_str))
        if (isTRUE(show_conf_interval)) lines <- c(lines, list(richtext(ci_mat[i, j])))
        if (isTRUE(show_pvalue))        lines <- c(lines, list(cor_pvalue_richtext(pvalue_mat[i, j])))
        cells[[i]] <- if (length(lines) == 1L) lines[[1]] else do.call(richtext_lines, lines)
      }
    }
    spec <- apa_col(spec, label = richtext(as.character(j)), align = "r", values = cells)
  }

  spec <- apa_note(spec, cor_spec_note(show_conf_interval))
  if (isTRUE(show_stars)) spec <- apa_note(spec, stars_spec_note())

  num <- apa_table_number_value(table_number)
  apa_spec_finish(spec, title = table_title, number = num, landscape = landscape,
                  meta = list(table.type = "correlation"))
}

