#' Create an APA-style table of means for a one-way design
#'
#' \code{apa.1way.table()} creates a table of means and standard deviations
#' of a dependent variable for each level of a grouping variable, in APA
#' style. Optionally, the 95\% confidence interval for each mean is
#' displayed. The table prints to the console. The most
#' common next step is saving the table as a Word file to use in a
#' manuscript:
#'
#' \preformatted{table1 <- apa.1way.table(iv = dose, dv = libido, data = viagra)
#' apa.save(filename = "Table1.docx", table1)}
#'
#' The file appears in your working directory -- the folder R is currently
#' pointing at. See the \emph{Word files (apa.save)} section below. The
#' table can also be placed in a Quarto document, so a rendered manuscript
#' comes out with the table already in place -- see the \emph{Quarto
#' documents} section below.
#'
#' @param iv Name of the grouping (independent) variable column in
#'   \code{data}, given without quotation marks (for example,
#'   \code{iv = dose}, not \code{iv = "dose"}).
#' @param dv Name of the dependent variable column in \code{data}, given
#'   without quotation marks (for example, \code{dv = libido}, not
#'   \code{dv = "libido"}).
#' @param data A data frame.
#' @param filename Optional file name. If supplied, the table is also saved
#'   as a Word (\code{.docx}) or HTML (\code{.html}) file, according to the
#'   file extension. Legacy \code{.doc}/\code{.rtf} names are saved as
#'   \code{.docx}.
#' @param table.number Number for the "Table N" line of the output. Defaults
#'   to \code{1}. Use \code{0} (or \code{NA}, or \code{NULL}) for a table with
#'   no number: none is then shown at the console or written to the Word file.
#'   When \code{\link{apa.save}} writes several tables to one file, a table
#'   left at the default takes the next number after the last numbered table before it
#'   (starting at 1), and a number given here is kept. In a Quarto document that uses the apaquarto
#'   extension this value is ignored: apaquarto numbers the tables itself, in
#'   the order they appear in the document.
#' @param show.conf.interval If \code{TRUE}, display the 95\% confidence
#'   interval for each mean. Defaults to \code{FALSE}.
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
#' \preformatted{apa.save(filename = "Table1.docx", table1)}
#'
#' The file appears in your working directory -- the folder R is currently
#' pointing at. Several tables can go into one file, each on its own page,
#' numbered Table 1, Table 2, and so on in the order you list them:
#'
#' \preformatted{apa.save(filename = "AllTables.docx", table1, table2)}
#'
#' The title above each table and the note below it are written by
#' apaTables from the table itself; on this route there is no way to
#' supply your own wording at this time. Tables are set in 12 point, and a
#' table too wide for an upright page gets a rotated (landscape) page of
#' its own rather than smaller type -- so a wide table coming out sideways
#' is expected, not an error.
#'
#' @section Quarto documents:
#' Quarto (\url{https://quarto.org}) is a free system for writing documents
#' that combine text and R code. The apaquarto extension makes a Quarto
#' document follow APA style: place this table in such a document and it
#' appears as a fully formatted APA table -- number, title, and note
#' included -- in Word, PDF, or HTML. The extension must be installed beside the document, and one command does
#' it. \code{\link{apa.quarto.new}(path = "my-paper")} creates a new manuscript
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
#' chunk: without it the document cannot find
#' \code{apa.1way.table()} and the render stops.
#'
#' \preformatted{```{r}
#' #| label: tbl-means
#' #| message: false
#' #| warning: true
#' library(apaTables)
#' table1 <- apa.1way.table(iv = dose, dv = libido, data = viagra)
#' apa.quarto.pdf(x = table1)
#' ```}
#'
#' That \code{#| label: tbl-means} line is not optional: without a label
#' starting \code{tbl-}, Quarto builds no float for the chunk, so there is no
#' number, no title, and no note, however you try to supply them.
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
#' \code{apa.quarto.example(filename = "apaTables-examples.qmd")} in the R console.
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
#' @inheritSection apa.cor.table Rotated (landscape) pages
#'
#' @seealso
#' \strong{New to Quarto?} \code{\link{apa.quarto.example}} writes a complete,
#' ready-to-render example document with one apaTables table per page -- the
#' fastest way to see every table type working in an apaquarto document. In
#' the R console, type:
#'
#' \preformatted{apa.quarto.example(filename = "apaTables-examples.qmd")}
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
#'
#' @examples
#' library(apaTables)
#'
#' # Means and standard deviations of libido for each dose
#' # (viagra data from Field et al. (2012), Discovering Statistics Using R)
#' table1 <- apa.1way.table(iv = dose, dv = libido,
#'                          data = viagra, table.number = 1)
#' table1
#'
#' # Word documents ----------------------------------------
#' # Save the table as a .docx file (here, in a temporary
#' # folder; for real use give an ordinary name, such as
#' # apa.save(filename = "Table1.docx", table1), and the file appears in your
#' # working directory)
#' apa.save(filename = file.path(tempdir(), "table1.docx"), table1)
#'
#' # Quarto documents ----------------------------------------
#' # Print a ready-to-paste apaquarto code chunk:
#' apa.quarto.template(x = table1)
#'
#' # Or write your own chunk; apa.quarto.pdf() displays the table body:
#' apa.quarto.pdf(x = table1)
#'
#' # Clean up
#' unlink(file.path(tempdir(), "table1.docx"))
#' @export
apa.1way.table <- function(iv, dv, data,filename=NA, table.number=1, show.conf.interval=FALSE, landscape=NA){
     data <- as.data.frame(data)
     table_number_explicit <- !missing(table.number)
     table.number <- apa_table_number_value(table.number)

     if (is.na(filename)) {
          make.file.flag=FALSE
     } else {
          make.file.flag=TRUE
     }

     if (!is.null(data)) {
          data.col.names <- colnames(data)
     } else {
          stop("apa.1way.table error:\ndata not specified.", call. = FALSE)
     }

     iv.sub <- substitute(iv)
     is.iv <- is.valid.name(iv.sub,data.col.names)

     dv.sub <- substitute(dv)
     is.dv  <- is.valid.name(dv.sub, data.col.names)


     if (is.dv==FALSE) {
          stop("apa.1way.table error:\nA valid dependent variable (dv) must be specified.", call. = FALSE)
     }

     if (is.iv==FALSE) {
          stop("apa.1way.table error:\nA valid independent variable (iv) must be specified.", call. = FALSE)
     }

     iv.name <- deparse(iv.sub)
     dv.name <- deparse(dv.sub)

     iv <- as.factor(data[,iv.name])
     dv <- data[,dv.name]

     tables.out = one.way.table.console.and.rtf(iv=iv,dv=dv,iv.name=iv.name,dv.name=dv.name,show.conf.interval = show.conf.interval,table.number)
     tbl.console <- tables.out$tbl.console

     table.title <- sprintf("Descriptive Statistics for %s by %s", apa_name_to_title(dv.name), apa_name_to_title(iv.name))
     table.note <- "{\\i M} = mean. {\\i SD} = standard deviation."

     if (show.conf.interval==TRUE) {
          ci.txt <- "{\\i CI} = confidence interval."
          table.note <- paste(table.note,ci.txt)
     }

     # Ver 3.0 add ons
     latex.table.note <- "\\\\textit{Note}. \\\\textit{M} = mean. \\\\textit{SD} = standard deviation. "
     if (show.conf.interval==TRUE) {
          latex.ci.txt <- "CI = confidence interval."
          latex.table.note <- paste(latex.table.note, latex.ci.txt)
     }

     tbl.console$latex.column.labels <-tables.out$latex.column.labels
     tbl.console$latex.column.centering <- make_markdown_column_alignment(tables.out$latex.column.labels)
     tbl.console$latex.table.note <- latex.table.note
     tbl.console$latex.table.title <- sprintf("Descriptive Statistics for %s by %s", apa_name_to_title(dv.name), apa_name_to_title(iv.name))

     tbl.console$landscape      <- landscape
     tbl.console$table.type      <- "oneway"

     tbl.console$spec <- oneway_build_spec(
          table_body         = tbl.console$table_body,
          column_labels      = tbl.console$latex.column.labels,
          column_centering   = tbl.console$latex.column.centering,
          show_conf_interval = show.conf.interval,
          table_title        = tbl.console$latex.table.title,
          table_number       = table.number,
          landscape          = landscape)

     tbl.console <- apa_set_table_number(tbl.console, table.number, table_number_explicit)

     if (make.file.flag == TRUE) {
          apa.save(filename = filename, tbl.console)
     }

     return(tbl.console)
}




apa.1way.table.work <- function(iv,dv,iv.name,dv.name, show.conf.interval) {
     iv.levels <- levels(iv)
     iv.level.numbers <- 1:length(iv.levels)

     my.means <- matrix(" ",length(iv.levels),1)
     my.sds <- matrix(" ",length(iv.levels),1)
     my.ci <- matrix(" ",length(iv.levels),1)

     for (iv.cur in iv.levels) {
          is.iv.level <- iv == iv.cur
          r.num.iv <- iv.level.numbers[iv.levels == iv.cur]

          cur.cell <- dv[is.iv.level]
          cell.mean <- mean(cur.cell,na.rm=TRUE)
          cell.sd <-  stats::sd(cur.cell,na.rm=TRUE)

          my.means[r.num.iv,1] <- sprintf("%1.2f",cell.mean)
          my.sds[r.num.iv,1] <- sprintf("%1.2f",cell.sd)
          LL <- get.ci.mean(cur.cell)$lower.conf.limit
          UL <- get.ci.mean(cur.cell)$upper.conf.limit
          ci_string <- sprintf("[%s, %s]", LL,UL)
          my.ci[r.num.iv,1] <- ci_string
     }


     if (show.conf.interval==FALSE) {
          data.table <- data.frame(my.means,my.sds,stringsAsFactors = FALSE)
          names(data.table) <- c("M","SD")
     } else {
          data.table <- data.frame(my.means,my.ci,my.sds,stringsAsFactors = FALSE)
          names(data.table) <- c("M","CI","SD")
     }

     return(data.table)
}

one.way.table.console.and.rtf <- function(iv,dv,iv.name, dv.name, show.conf.interval=FALSE,table.number,add.blank.header=FALSE) {
     table.out <- apa.1way.table.work(iv=iv,dv=dv,iv.name=iv.name,dv.name=dv.name, show.conf.interval=show.conf.interval)

     #add first name column
     level.names <- as.data.frame(matrix(levels(iv),ncol=1))
     names(level.names) <- iv.name
     table.out <- cbind(level.names,table.out)
     names(table.out)[1] <- "IV"



     #make console output
     table.title <- sprintf("Descriptive Statistics for %s by %s", apa_name_to_title(dv.name), apa_name_to_title(iv.name))

     table.body <- table.out
     names(table.body) <- get_oneway_column_names(table.body)
     names(table.body)[1] <- iv.name

     table.note <- "Note. M = mean. SD = standard deviation.\n"
     if (show.conf.interval==TRUE) {
          ci.txt <- "CI = confidence interval."
          table.note <- paste(table.note,ci.txt,sep="")
     }
     tbl.console <- list()
     tbl.console <- list(table_number = table.number,
                         table_title = table.title,
                         table_body = table.body,
                         table_note = table.note)
     class(tbl.console) <- "apa_table"


     #make latex column names
     latex.column.labels <- get_oneway_latex_column_names(table.out)
     latex.column.labels[1] <- iv.name

     output <- list()
     output$tbl.console <- tbl.console
     output$latex.column.labels <- latex.column.labels
     return(output)
}


oneway_column_width <- function(column_name) {
     narrow <- .60
     wide   <- .95

     switch(column_name,
            IV = wide*1.5,
            M = narrow,
            CI = wide*1.25,
            SD = narrow)
}



oneway_latex_column_names <- function(column_name) {
     switch(column_name,
            IV = "IV",
            M = "$M$",
            CI = "95\\% CI",
            SD = "$SD$")
}

get_oneway_latex_column_names <- function(df) {
     n <- names(df)
     names_out <- c()
     for (i in 1:length(n)) {
          names_out[i] <-oneway_latex_column_names(n[i])
     }
     return(names_out)
}


oneway_column_names <- function(column_name) {
     switch(column_name,
            IV = "IV",
            M = "M",
            CI = "M_95%_CI",
            SD = "SD")
}

get_oneway_column_names <- function(df) {
     n <- names(df)
     names_out <- c()
     for (i in 1:length(n)) {
          names_out[i] <-oneway_column_names(n[i])
     }
     return(names_out)
}


make_markdown_column_alignment <- function(column_names) {
     N = length(column_names)
     output = rep("r", N)
     output[1] <- "l"
     return(output)
}


# One-way table note as richtext (no "Note." prefix). Mirrors the console note.
oneway_spec_note <- function(show_conf_interval) {
  runs <- list(rt_run("M", italic = TRUE),  rt_run(" = mean. "),
               rt_run("SD", italic = TRUE), rt_run(" = standard deviation."))
  if (isTRUE(show_conf_interval)) {
    runs <- c(runs, list(rt_run(" CI = confidence interval.")))
  }
  richtext(runs)
}

# Build an apa_table_spec from the one-way builder's formatted table_body.
# Flat single-level columns; reuses twoway_col_label for the M/SD/CI/factor map.
oneway_build_spec <- function(table_body, column_labels, column_centering,
                              show_conf_interval, table_title, table_number, landscape) {
  n_col <- ncol(table_body)
  n_row <- nrow(table_body)
  spec <- apa_spec_start(n_row)
  for (j in seq_len(n_col)) {
    spec <- apa_col(spec, label = twoway_col_label(column_labels[j]),
                    align = column_centering[j],
                    values = as.list(as.character(table_body[[j]])))
  }
  spec <- apa_note(spec, oneway_spec_note(show_conf_interval))
  num <- apa_table_number_value(table_number)
  apa_spec_finish(spec, title = trimws(table_title), number = num, landscape = landscape,
                  meta = list(table.type = "oneway"))
}
