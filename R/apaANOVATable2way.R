#' Create an APA-style table of means for a two-way design
#'
#' \code{apa.2way.table()} creates a table of cell means and standard
#' deviations of a dependent variable for a two-way (factorial) design, in
#' APA style. Optionally, the 95\% confidence interval for each cell mean
#' or the marginal means can be displayed. The table prints to the console. The most
#' common next step is saving the table as a Word file to use in a
#' manuscript:
#'
#' \preformatted{table1 <- apa.2way.table(iv1 = gender, iv2 = alcohol,
#'                          dv = attractiveness, data = goggles)
#' apa.save(filename = "Table1.docx", table1)}
#'
#' The file appears in your working directory -- the folder R is currently
#' pointing at. See the \emph{Word files (apa.save)} section below. The
#' table can also be placed in a Quarto document, so a rendered manuscript
#' comes out with the table already in place -- see the \emph{Quarto
#' documents} section below.
#'
#' @param iv1 Name of the first grouping (independent) variable column in
#'   \code{data}, given without quotation marks (for example,
#'   \code{iv1 = gender}, not \code{iv1 = "gender"}).
#' @param iv2 Name of the second grouping (independent) variable column in
#'   \code{data}, given without quotation marks.
#' @param dv Name of the dependent variable column in \code{data}, given
#'   without quotation marks.
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
#'   interval for each cell mean; marginal means are then not shown.
#'   Defaults to \code{FALSE}.
#' @param show.marginal.means If \code{TRUE}, add the marginal means for
#'   each level of \code{iv1} and \code{iv2}. Used only when
#'   \code{show.conf.interval = FALSE}. Defaults to \code{FALSE}.
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
#' \code{apa.2way.table()} and the render stops.
#'
#' \preformatted{```{r}
#' #| label: tbl-means
#' #| message: false
#' #| warning: true
#' library(apaTables)
#' table1 <- apa.2way.table(iv1 = gender, iv2 = alcohol,
#'                          dv = attractiveness, data = goggles)
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
#' @examples
#' library(apaTables)
#'
#' # Cell means for a 2-way design from Fidler & Thompson (2001)
#' table2 <- apa.2way.table(iv1 = a, iv2 = b, dv = dv,
#'                          data = fidler_thompson,
#'                          table.number = 2)
#' table2
#'
#' # Cell means for the goggles data
#' # (from Field et al. (2012), Discovering Statistics Using R)
#' table3 <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
#'                          data = goggles, table.number = 3)
#' table3
#'
#' # Word documents ----------------------------------------
#' # apa.save() can write several tables into one Word document
#' # (here, in a temporary
#' # folder; for real use give an ordinary name, such as
#' # apa.save(filename = "Table1.docx", table1), and the file appears in your
#' # working directory)
#' apa.save(filename = file.path(tempdir(), "my_tables.docx"), table2, table3)
#'
#' # Quarto documents ----------------------------------------
#' # Print a ready-to-paste apaquarto code chunk:
#' apa.quarto.template(x = table2)
#'
#' # Or write your own chunk; apa.quarto.pdf() displays the table body:
#' apa.quarto.pdf(x = table2)
#'
#' # Clean up
#' unlink(file.path(tempdir(), "my_tables.docx"))
#' @export
apa.2way.table <- function(iv1, iv2, dv, data, filename = NA, table.number = 1, show.conf.interval = FALSE, show.marginal.means = FALSE, landscape = NA){

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
          stop("apa.2way.table error:\ndata not specified.", call. = FALSE)
     }

     iv1.sub <- substitute(iv1)
     is.iv1 <- is.valid.name(iv1.sub,data.col.names)
     iv2.sub <- substitute(iv2)
     is.iv2 <- is.valid.name(iv2.sub,data.col.names)

     dv.sub <- substitute(dv)
     is.dv  <- is.valid.name(dv.sub, data.col.names)


     if (is.dv==FALSE) {
          stop("apa.2way.table error:\nA valid dependent variable (dv) must be specified.", call. = FALSE)
     }

     if (all(is.iv1==TRUE & is.iv2==TRUE)==FALSE) {
          stop("apa.2way.table error:\nTwo valid independent variables (iv's) must be specified.", call. = FALSE)
     }

     iv1.name <- deparse(iv1.sub)
     iv2.name <- deparse(iv2.sub)
     dv.name <- deparse(dv.sub)

     iv1 <- as.factor(data[,iv1.name])
     iv2 <- as.factor(data[,iv2.name])
     dv <- data[,dv.name]

     if (show.conf.interval==FALSE) {
          output.information <- apa.2way.table.work(iv1 = iv1, iv2 = iv2, dv = dv,
                                                    iv1.name = iv1.name, iv2.name = iv2.name, dv.name = dv.name,
                                                    show.marginal.means = show.marginal.means,
                                                    show.conf.interval = show.conf.interval)

          #store output unaltered for console output
          table.initial.console <- output.information


          # create latex table
          rows.columns.out <- dim(output.information)[1]
          latex.body <- output.information[3:rows.columns.out,]
          latex.body.names = output.information[2,]
          names(latex.body) <- latex.body.names
          if (show.marginal.means == FALSE) {
               latex.extra.header1 = c(" ", levels(iv2))
          } else {
               latex.extra.header1 = c(" ", levels(iv2), "Marginal")
          }

          latex.extra.header2 = iv2.name

          latex.group.names = ""
          latex.group.num.per.group = 0

     } else {
          output.information <- apa.2way.table.ci.work(iv1 = iv1, iv2 = iv2, dv = dv,
                                                       iv1.name = iv1.name, iv2.name = iv2.name,
                                                       dv.name = dv.name,
                                                       table.number = table.number)
          table.initial.console <- output.information$console

          # create latex table
          latex.body.temp <- output.information$latex.body
          rows.columns.out <- dim(latex.body.temp)[1]
          latex.body <- latex.body.temp
          latex.group.names = paste0(paste0(iv2.name, ": "), levels(iv2))
          latex.group.num.per.group = length(levels(iv1))
          latex.extra.header1 <- " "
     }

     #make table title
     iv1.num.levels = length(levels(iv1))
     iv2.num.levels = length(levels(iv2))
     table.title <- sprintf("Descriptive Statistics for %s in a %1.0f(%s) by %1.0f(%s) Design",apa_name_to_title(dv.name),iv1.num.levels,apa_name_to_title(iv1.name),iv2.num.levels,apa_name_to_title(iv2.name))
     latex.table.title <- table.title

     if ((show.marginal.means==TRUE) & (show.conf.interval==FALSE)) {
          table.note <- "{\\i M} = mean. {\\i SD} = standard deviation. Marginal indicates the means and standard deviations pertaining to main effects."
          table.note.txt <- "Note. M = mean. SD = standard deviation. \nMarginal indicates the means and standard deviations pertaining to main effects."
     } else {
          table.note <- "{\\i M} = mean. {\\i SD} = standard deviation."
          table.note.txt <- "Note. M = mean. SD = standard deviation."
     }
     if (show.conf.interval==TRUE) {
          ci.txt <- "\nCI = confidence interval."
          table.note.txt <- paste(table.note.txt,ci.txt)
     }


     #make console output
     tbl.console <- list()
     tbl.console <- list(table_number = table.number,
                         table_title = table.title,
                         table_body = table.initial.console,
                         table_note = table.note.txt)


     class(tbl.console) <- "apa_table"


     # Ver 3.0 add ons
     latex.table.note <- "\\\\textit{Note}. \\\\textit{M} = mean. \\\\textit{SD} = standard deviation. "
     if (show.conf.interval==TRUE) {
          latex.ci.txt <- "CI = confidence interval."
          latex.table.note <- paste(latex.table.note, latex.ci.txt)
     }


     # V3

     tbl.console$latex.group.names <- latex.group.names
     tbl.console$latex.group.num.per.group <- latex.group.num.per.group

     tbl.console$latex.body <- latex.body
     tbl.console$latex.extra.header1 <- latex.extra.header1
     tbl.console$latex.extra.header2 <- iv2.name
     tbl.console$latex.column.labels <- get_oneway_latex_column_names(latex.body)
     tbl.console$latex.column.labels[1] <- iv1.name
     tbl.console$latex.column.centering <- make_markdown_column_alignment(tbl.console$latex.column.labels)
     tbl.console$latex.table.note <- latex.table.note
     tbl.console$latex.table.title <- latex.table.title

     tbl.console$landscape      <- landscape




     if (show.conf.interval == FALSE) {
               tbl.console$table.type     <- "twoway"
     } else {
          tbl.console$table.type     <- "twoway-ci"
     }

     if (show.conf.interval == FALSE) {
          tbl.console$spec <- twoway_build_spec(
               latex_body          = tbl.console$latex.body,
               column_labels       = tbl.console$latex.column.labels,
               column_centering    = tbl.console$latex.column.centering,
               extra_header1       = tbl.console$latex.extra.header1,
               extra_header2       = tbl.console$latex.extra.header2,
               show_marginal_means = show.marginal.means,
               show_conf_interval  = show.conf.interval,
               table_title         = tbl.console$latex.table.title,
               table_number        = table.number,
               landscape           = landscape)
     }

     if (show.conf.interval == TRUE) {
          tbl.console$spec <- twoway_ci_build_spec(
               latex_body          = tbl.console$latex.body,
               column_labels       = tbl.console$latex.column.labels,
               column_centering    = tbl.console$latex.column.centering,
               group_names         = tbl.console$latex.group.names,
               group_num_per_group = tbl.console$latex.group.num.per.group,
               table_title         = tbl.console$latex.table.title,
               table_number        = table.number,
               landscape           = landscape)
     }

     tbl.console <- apa_set_table_number(tbl.console, table.number, table_number_explicit)

     if (make.file.flag == TRUE) {
          apa.save(filename = filename, tbl.console)
     }

     return(tbl.console)
}










apa.2way.table.work <- function(iv1,iv2,dv,iv1.name,iv2.name,dv.name,show.marginal.means,show.conf.interval) {
     #create table cells
     count <- 0
     column.sets <- list()
     for (cur.iv2.level in levels(iv2)) {
          count <- count + 1
          cur.dv <- dv[iv2==cur.iv2.level]
          cur.iv1 <- iv1[iv2==cur.iv2.level]
          m.sd.columns <- apa.1way.table.work(iv=cur.iv1,dv=cur.dv,iv.name=iv1.name,show.conf.interval = show.conf.interval)
          column.sets[[count]] <- m.sd.columns
     }
     final.count <- count

     columns.out <- column.sets[[1]]
     for (i in 2:final.count) {
          columns.out <- cbind(columns.out,column.sets[[i]])
     }


     if (show.conf.interval == FALSE) {
          if (show.marginal.means ==TRUE) {
               #create marginal means
               iv1.marginal <- apa.1way.table.work(iv=iv1,dv=dv,iv.name=iv1.name,show.conf.interval = FALSE)
               iv2.marginal <- apa.1way.table.work(iv=iv2,dv=dv,iv.name=iv2.name,show.conf.interval = FALSE)
               columns.out <- cbind(columns.out,iv1.marginal)

               iv2.marginal.strung.out <- string.out.data.frame(iv2.marginal)
               iv2.marginal.strung.out <- cbind(iv2.marginal.strung.out," "," ")
               names(iv2.marginal.strung.out) <- names(columns.out)
               columns.out <- rbind(columns.out,iv2.marginal.strung.out)
          }
     }


     #add labels for m and sd as row
     columns.out.column.names <- colnames(columns.out)
     m.sd.column.names.row <- t(data.frame(columns.out.column.names,stringsAsFactors = FALSE))
     colnames(m.sd.column.names.row) <- columns.out.column.names
     columns.out <- rbind(m.sd.column.names.row, columns.out)


     #add IV2 levels as row
     iv2.levels <- levels(iv2)
     if ((show.marginal.means==TRUE) & (show.conf.interval==FALSE)) {
          iv2.levels <- c(iv2.levels,"Marginal")
     }
     iv2.num.levels <- length(iv2.levels)

     #weave spaces into levels
     my.spaces <- rep(" ",length(iv2.levels))
     if (show.conf.interval==TRUE) {
          num.cell.multiplier <- 4
          iv2.levels.column.names <- matrix(rbind(iv2.levels,my.spaces,my.spaces,my.spaces),nrow=iv2.num.levels*num.cell.multiplier)
     } else {
          num.cell.multiplier <- 2
          iv2.levels.column.names <- matrix(rbind(iv2.levels,my.spaces),nrow=iv2.num.levels*num.cell.multiplier)
     }
     iv2.levels.column.names.row <- t(data.frame(iv2.levels.column.names,stringsAsFactors = FALSE))
     colnames(iv2.levels.column.names.row) <- columns.out.column.names
     columns.out <- rbind(iv2.levels.column.names.row, columns.out)


     #add IV2 varaible name as column name
     columns.out.new.column.names <- rep(" ",length(iv2.levels.column.names))
     columns.out.new.column.names[1] <- iv2.name
     colnames(columns.out) <- columns.out.new.column.names


     #add label column for IV1
     iv1.levels <- c(" ",iv1.name,levels(iv1))
     if ((show.marginal.means==TRUE) & (show.conf.interval==FALSE)) {
          iv1.levels <- c(iv1.levels,"Marginal")
     }
     columns.out <- cbind(iv1.levels,columns.out)
     my.col.names <- names(columns.out)
     my.col.names[1] <- "  "


     columns.out <- data.frame(as.matrix(columns.out),stringsAsFactors = FALSE)
     names(columns.out) <- my.col.names
     rownames(columns.out) <- NULL

     return(columns.out)
}


string.out.data.frame <- function(df.in) {
     size.frame <- dim(df.in)
     n.row <- size.frame[1]
     n.col <- size.frame[2]

     row.out <- df.in[1,]
     for (i in 2:n.row) {
          cur.row <- df.in[i,]
          row.out <- cbind(row.out,cur.row)
     }
     colnames(row.out) <- NULL
     return(row.out)
}


apa.2way.table.ci.work <- function(iv1,iv2,dv,iv1.name,iv2.name,dv.name,table.number) {
     count <- 0
     for (cur.iv2.level in levels(iv2)) {
          count <- count + 1
          cur.dv <- dv[iv2==cur.iv2.level]
          cur.iv <- iv1[iv2==cur.iv2.level]

          tables.out = one.way.table.console.and.rtf(iv = cur.iv, dv = cur.dv, iv.name = iv1.name,
                                                     dv.name = dv.name,
                                                     show.conf.interval = TRUE,
                                                     table.number, add.blank.header = TRUE)

          #console

          if (count==1) {
               tbl.console.output <- tables.out$tbl.console
               tbl.console <- tbl.console.output$table_body
               latex.body <- tbl.console

               tbl.console <- add.blank.first.row(tbl.console)
               tbl.console[1,1] <- sprintf("%s",iv1.name)

               tbl.console <- add.blank.first.row(tbl.console)
               tbl.console[1,1] <- sprintf("%s:%s",iv2.name,cur.iv2.level)
               console.data <- tbl.console
          } else {
               tbl.console <- tables.out$tbl.console$table_body
               latex.cur.body <- tbl.console

               tbl.console <- add.blank.first.row(tbl.console)
               tbl.console[1,1] <- sprintf("%s",iv1.name)

               tbl.console <- add.blank.first.row(tbl.console)
               tbl.console[1,1] <- sprintf("%s:%s",iv2.name,cur.iv2.level)
               tbl.console <- add.blank.first.row(tbl.console)

               console.data <- my.rbind(console.data,tbl.console,1)
               latex.body <- rbind(latex.body, latex.cur.body)
          }
          my.col.names <- colnames(console.data)
          my.col.names[1] <- ""
          colnames(console.data) <- my.col.names
          tbl.console.output$table_body <- console.data

     }
     final.count <- count

     latex.names <- names(latex.body)
     latex.names[3] <- "CI"
     names(latex.body) <- latex.names

     output <- list()
     output$console <- tbl.console.output$table_body
     output$latex.body <- latex.body
     return(output)
}


add.blank.first.row <-function(df.in) {
     df.out <- df.in[1,]
     number.columns <- dim(df.out)[2]
     for (i in 1:number.columns) {
          df.out[,i] <- ""
     }
     df.out <- my.rbind(df.out,df.in,2)

     return(df.out)
}

my.rbind <- function(df.1,df.2,names.from.table.number) {
     if (names.from.table.number==1) {
          my.col.names <- colnames(df.1)
     } else {
          my.col.names <- colnames(df.2)
     }
     colnames(df.1) <- my.col.names
     colnames(df.2) <- my.col.names

     df.out <- rbind(df.1,df.2)
     df.out <- data.frame(df.out,stringsAsFactors = FALSE)
     colnames(df.out) <- my.col.names

     return(df.out)
}


# Map one V3 latex column label to richtext for the spec.
twoway_col_label <- function(latex_label) {
  switch(latex_label,
    "$M$"      = apa_sym("M"),
    "$SD$"     = apa_sym("SD"),
    "95\\% CI" = richtext("95% CI"),
    "95% CI"   = richtext("95% CI"),
    richtext(latex_label)               # default: row-label / factor-name column
  )
}

# Build two-level decked-head spanners from the V3 header fields (no-CI case).
# extra_header1: c(" ", <iv2 levels...>, ["Marginal"]) -- one entry per column GROUP;
#   the leading " " is the row-label column (no spanner).
# extra_header2: the iv2 (column factor) name -- a level-2 spanner over the iv2-level
#   groups only (a trailing "Marginal" group is excluded).
# n_col: total columns including the row-label column.
twoway_spanners <- function(extra_header1, extra_header2, n_col) {
  group_labels <- extra_header1[-1]
  n_groups <- length(group_labels)
  if (n_groups < 1) return(list())
  gsize <- (n_col - 1L) %/% n_groups
  spanners <- list()
  for (k in seq_len(n_groups)) {
    first <- 2L + (k - 1L) * gsize
    last  <- 1L + k * gsize
    spanners <- c(spanners, list(list(label = richtext(trimws(group_labels[k])),
                                      cols = first:last, level = 1L)))
  }
  n_factor_groups <- sum(trimws(group_labels) != "Marginal")
  if (n_factor_groups >= 1L && nzchar(trimws(extra_header2))) {
    last_factor_col <- 1L + n_factor_groups * gsize
    spanners <- c(spanners, list(list(label = richtext(extra_header2),
                                      cols = 2:last_factor_col, level = 2L)))
  }
  spanners
}

# Two-way table note as richtext (no "Note." prefix). Mirrors the console note.
twoway_spec_note <- function(show_marginal_means, show_conf_interval) {
  runs <- list(rt_run("M", italic = TRUE),  rt_run(" = mean. "),
               rt_run("SD", italic = TRUE), rt_run(" = standard deviation."))
  if (isTRUE(show_marginal_means) && !isTRUE(show_conf_interval)) {
    runs <- c(runs, list(rt_run(" Marginal indicates the means and standard deviations pertaining to main effects.")))
  }
  if (isTRUE(show_conf_interval)) {
    runs <- c(runs, list(rt_run(" CI = confidence interval.")))
  }
  richtext(runs)
}

# Build an apa_table_spec from the two-way CI builder's stacked fields. The iv2
# grouping is expressed as ROW GROUPS (one per iv2 level), not column spanners.
twoway_ci_build_spec <- function(latex_body, column_labels, column_centering,
                                 group_names, group_num_per_group,
                                 table_title, table_number, landscape) {
  n_col <- ncol(latex_body)
  n_row <- nrow(latex_body)
  spec <- apa_spec_start(n_row)
  for (j in seq_len(n_col)) {
    spec <- apa_col(spec, label = twoway_col_label(column_labels[j]),
                    align = column_centering[j],
                    values = as.list(as.character(latex_body[[j]])))
  }
  per <- group_num_per_group
  for (k in seq_along(group_names)) {
    spec <- apa_row_group(spec, label = richtext(group_names[k]),
                          rows = ((k - 1L) * per + 1L):(k * per))
  }
  spec <- apa_note(spec, twoway_spec_note(show_marginal_means = FALSE,
                                          show_conf_interval = TRUE))
  num <- apa_table_number_value(table_number)
  apa_spec_finish(spec, title = trimws(table_title), number = num, landscape = landscape,
                  meta = list(table.type = "twoway-ci"))
}

# Build an apa_table_spec from the two-way builder's V3 structured fields (no-CI).
twoway_build_spec <- function(latex_body, column_labels, column_centering,
                              extra_header1, extra_header2,
                              show_marginal_means, show_conf_interval,
                              table_title, table_number, landscape) {
  n_col <- ncol(latex_body)
  n_row <- nrow(latex_body)

  spec <- apa_spec_start(n_row)
  for (j in seq_len(n_col)) {
    spec <- apa_col(spec, label = twoway_col_label(column_labels[j]),
                    align = column_centering[j],
                    values = as.list(as.character(latex_body[[j]])))
  }

  spanners <- twoway_spanners(extra_header1, extra_header2, n_col)
  for (sp in spanners) spec <- apa_spanner(spec, label = sp$label, cols = sp$cols, level = sp$level)

  spec <- apa_note(spec, twoway_spec_note(show_marginal_means, show_conf_interval))

  num <- apa_table_number_value(table_number)
  apa_spec_finish(spec, title = table_title, number = num, landscape = landscape,
                  meta = list(table.type = "twoway"))
}
