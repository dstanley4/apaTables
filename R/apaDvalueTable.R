#' Create an APA-style table of pairwise d-values
#'
#' \code{apa.d.table()} creates a table of standardized mean differences
#' (Cohen's d) for all pairwise comparisons of the levels of a grouping
#' variable, in APA style. The table also reports the mean and standard
#' deviation for each group, and each d-value is accompanied by its 95\%
#' confidence interval. The table prints to the console. The most
#' common next step is saving the table as a Word file to use in a
#' manuscript:
#'
#' \preformatted{table1 <- apa.d.table(iv = dose, dv = libido, data = viagra)
#' apa.save("Table1.docx", table1)}
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
#'   left at the default is numbered by its position in that file, while a
#'   number given here is kept. In a Quarto document that uses the apaquarto
#'   extension this value is ignored: apaquarto numbers the tables itself, in
#'   the order they appear in the document.
#' @param show.conf.interval Deprecated and ignored; confidence intervals
#'   are always displayed when the \pkg{MBESS} package is installed. This
#'   argument will be removed in a later version.
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
#' The d-values are calculated with the pooled-variance formulas 4.18 and
#' 4.19 of Borenstein, Hedges, Higgins, and Rothstein (2009) -- that is,
#' the mean difference divided by the pooled within-groups standard
#' deviation, with no small-sample correction applied; a d-value is
#' not calculated when unequal variances prevent pooling. Each confidence
#' interval is the exact noncentral t interval computed by
#' \code{MBESS::ci.smd()}; the \pkg{MBESS} package is therefore required
#' for the intervals, and when \pkg{MBESS} is not installed the table is
#' created without them.
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
#' \preformatted{apa.save("Table1.docx", table1)}
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
#' its own rather than smaller type -- so a wide table coming out sideways
#' is expected, not an error.
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
#' chunk: without it the document cannot find
#' \code{apa.d.table()} and the render stops.
#'
#' \preformatted{```{r}
#' #| label: tbl-dvalues
#' #| message: false
#' #| warning: true
#' library(apaTables)
#' table1 <- apa.d.table(iv = dose, dv = libido, data = viagra)
#' apa.quarto.pdf(table1)
#' ```}
#'
#' That \code{#| label: tbl-dvalues} line is not optional: without a label
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
#' @inheritSection apa.cor.table Rotated (landscape) pages
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
#'
#' @references
#' Borenstein, M., Hedges, L. V., Higgins, J. P. T., & Rothstein, H. R. (2009). Introduction to meta-analysis. John Wiley & Sons.
#' @examples
#' library(apaTables)
#'
#' # Pairwise d-values for the effect of dose on libido
#' # (viagra data from Field et al. (2012), Discovering Statistics Using R)
#' table1 <- apa.d.table(iv = dose, dv = libido, data = viagra)
#' table1
#'
#' # Word documents ----------------------------------------
#' # Save the table as a .docx file (here, in a temporary
#' # folder; for real use give an ordinary name, such as
#' # apa.save("Table1.docx", table1), and the file appears in your
#' # working directory)
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
apa.d.table <- function(iv, dv, data, filename=NA, table.number=1, show.conf.interval = TRUE, landscape=NA){
     data <- as.data.frame(data)
     table_number_explicit <- !missing(table.number)
     table_number <- apa_table_number_value(table.number)
     table.number <- table_number


     if (show.conf.interval==FALSE) {
          message("The ability to suppress reporting of confidence intervals has been deprecated in this version.\nThe function argument show.conf.interval will be removed in a later version.")
     }

     if (requireNamespace("MBESS", quietly = TRUE)) {
          show.conf.interval = TRUE
     } else {
          show.conf.interval = FALSE
     }

     show_conf_interval <- show.conf.interval

     # MBESS::ci.smd() reaches MBESS::conf.limits.nct(), which runs
     # options(warn = -1) in its own scope and never restores it. Verified
     # against MBESS 5.0.0: this is the one MBESS entry point we use that
     # really leaks, and it silences every warning for the rest of the
     # session (or the rest of a Quarto render). Restore the caller's
     # setting when this function exits, including on error.
     old_warn_option <- getOption("warn")
     on.exit(options(warn = old_warn_option), add = TRUE)

     if (is.na(filename)) {
          make_file_flag=FALSE
     } else {
          make_file_flag=TRUE
     }

     if (!is.null(data)) {
          data_col_names <- colnames(data)
     } else {
          stop("apa.d.table error:\ndata not specified.", call. = FALSE)
     }

     iv_sub <- substitute(iv)
     is_iv <- is.valid.name(iv_sub, data_col_names)

     dv_sub <- substitute(dv)
     is_dv  <- is.valid.name(dv_sub, data_col_names)


     if (is_dv==FALSE) {
          stop("apa.d.table error:\nA valid dependent variable (dv) must be specified.", call. = FALSE)
     }

     if (is_iv==FALSE) {
          stop("apa.d.table error:\nA valid independent variable (iv) must be specified.", call. = FALSE)
     }
     iv_name <- deparse(iv_sub)
     dv_name <- deparse(dv_sub)
     iv <- as.factor(data[,iv_name])
     dv <- data[,dv_name]

     #create table
     iv_levels <- levels(iv)
     iv_num_levels  <- length(iv_levels)
     number_columns <- iv_num_levels -1

     output_d     <- matrix(" ",iv_num_levels, number_columns)
     output_d_ci     <- matrix(" ",iv_num_levels, number_columns)

     output_row_descriptives <- matrix(" ",iv_num_levels, ncol=2)
     for (c_row in 1:iv_num_levels) {
          for (c_col in 1:iv_num_levels) {
               if (c_col<c_row) {
                    group1_id <- iv == iv_levels[c_row]
                    group2_id <- iv == iv_levels[c_col]
                    group1_data <- na.omit(dv[group1_id])
                    group2_data <- na.omit(dv[group2_id])
                    group1_n <- length(group1_data)
                    group2_n <- length(group2_data)
                    group1_mean <- sprintf("%1.2f",mean(group1_data))
                    group1_sd <- sprintf("%1.2f",sd(group1_data))

                    output_row_descriptives[c_row,1] <- group1_mean
                    output_row_descriptives[c_row,2] <- group1_sd

                    cur_d_value <- get_d_value(group1_data, group2_data)

                    if (requireNamespace("MBESS", quietly = TRUE) == TRUE) {
                         cur_d_ci <- MBESS::ci.smd(smd=cur_d_value, n.1=group1_n, n.2=group2_n)
                         # ci.smd() leaves warn = -1 behind (see above); put it
                         # back now so warnings raised later in this function
                         # still show.
                         options(warn = old_warn_option)
                    } else {
                         cur_d_ci <- list()
                         cur_d_ci$Lower.Conf.Limit.smd <- (-99)
                         cur_d_ci$Upper.Conf.Limit.smd <- (-99)
                    }

                    my_t_test_results <- t.test(x=group1_data, y=group2_data, alternative="two.sided", var.equal = TRUE)
                    cur_t_p_value <- my_t_test_results$p.value

                    d_string <- txt_d(cur_d_value, cur_t_p_value)
                    d.ci.string <- txt_d_ci(cur_d_ci)
                    if (is_equal_var(group1_data,group2_data) == FALSE) {
                         d_string <- "-"
                         d.ci.string <- "        [-,-]"
                         output_d[c_row,c_col] <- d_string
                         output_d_ci[c_row,c_col]<-"[-,-]"
                    } else {
                         output_d[c_row,c_col] <- d_string
                         output_d_ci[c_row,c_col]<-d.ci.string
                    }


               }
          }
     }
     group1_id <- iv == iv_levels[1]
     group1_data <- na.omit(dv[group1_id])
     group1_mean <- sprintf("%1.2f", mean(group1_data))
     group1_sd   <- sprintf("%1.2f", sd(group1_data))
     output_row_descriptives[1,] <- c(group1_mean, group1_sd)


     #weave
     iv_levels_numbers <- 1:iv_num_levels
     iv_levels_periods <- rep(". ", iv_num_levels)
     iv_levels <- paste(iv_levels_numbers, iv_levels_periods, iv_levels,sep="")

     left_padding <- c(" ", " ", " ")
     first_line <- c(iv_levels[1], output_row_descriptives[1,], output_d[1,])
     second_line <- c(left_padding, output_d_ci[1,])
     third_line <- rep(" ", length(second_line))

     output_matrix_console <- rbind(first_line,second_line)
     for (i in 2:iv_num_levels) {
          first_line <- c(iv_levels[i], output_row_descriptives[i,], output_d[i,])

          second_line <- c(left_padding, output_d_ci[i,])

          third_line <- rep(" ", length(second_line))

          if (show_conf_interval==TRUE) {
               new_lines     <- rbind(first_line,second_line,third_line)
               new_lines     <- rbind(first_line,second_line,third_line)
          } else {
               new_lines     <- rbind(first_line,third_line)
          }

          output_matrix_console <- rbind(output_matrix_console, new_lines)
     }
     rownames(output_matrix_console) <- 1:nrow(output_matrix_console)
     colnames(output_matrix_console) <- c(c("Variable","M","SD"),as.character(1:number_columns))


     # This title is GENERATED from the iv and dv, exactly as the one-way
     # table's is. The console, LaTeX and spec paths below must all say the
     # same thing. There is deliberately no "with Confidence Intervals" phrase
     # in any of them -- the note already states that square brackets hold the
     # 95% CI, and the phrase could not be kept conditional without the three
     # paths diverging.
     dvalue_title_names <- sprintf("for %s by %s",
                                   apa_name_to_title(dv_name),
                                   apa_name_to_title(iv_name))
     table_title <- sprintf("Means, Standard Deviations, and d-Values %s\n",
                            dvalue_title_names)

     #make table
     row_with_colnames <- colnames(output_matrix_console)
     df_temp <- data.frame(output_matrix_console, stringsAsFactors = FALSE)
     rownames(output_matrix_console) <- rep(" ", length((rownames(output_matrix_console))))
     table_body <- output_matrix_console

     #make console output
     if (show_conf_interval==TRUE) {
          table_note <- "Note. M indicates mean. SD indicates standard deviation. d-values are estimates calculated using formulas 4.18 and 4.19\nfrom Borenstein, Hedges, Higgins, & Rothstein (2009). d-values not calculated if unequal variances prevented pooling.\nValues in square brackets indicate the 95% confidence interval for each d-value. \n"
     } else {
          table_note <- "Note. M indicates mean. SD indicates standard deviation. d-values are estimates calculated using formulas 4.18 and 4.19\nfrom Borenstein, Hedges, Higgins, & Rothstein (2009). d-values not calculated if unequal variances prevented pooling.\n"
     }
     tbl_console <- list(table_number = table_number,
                         table_title = table_title,
                         table_body = table_body,
                         table_note = table_note)
     class(tbl_console) <- "apa_table"


     table_title_latex <- sprintf("Means, Standard Deviations, and $d$-Values %s",
                                  dvalue_title_names)
     if (show_conf_interval==TRUE) {
          table_note_latex <- "\\\\textit{Note}. $M$ indicates mean. $SD$ indicates standard deviation. $d$-values are estimates calculated using formulas 4.18 and 4.19 from Borenstein, Hedges, Higgins, \\\\& Rothstein (2009). $d$-values not calculated if unequal variances prevented pooling. Values in square brackets indicate the 95\\\\% confidence interval for each $d$-value. "
     } else {
          table_note_latex <- "\\\\textit{Note}. $M$ indicates mean. $SD$ indicates standard deviation. $d$-values are estimates calculated using formulas 4.18 and 4.19 from Borenstein, Hedges, Higgins, \\\\& Rothstein (2009). $d$-values not calculated if unequal variances prevented pooling."
     }

     tbl_console$latex.table.note   <- table_note_latex
     tbl_console$latex.table.title  <- table_title_latex
     tbl_console$latex.body         <- table_body
     #tbl_console$latex.table.note   <- "table note"


     tbl_console$landscape      <- landscape
     tbl_console$table.type      <- "dvalue"

     tbl_console$spec <- dvalue_build_spec(
          variable_names = iv_levels,
          descriptives   = output_row_descriptives,
          d_mat          = output_d,
          d_ci_mat       = output_d_ci,
          table_number   = table.number,
          landscape      = landscape,
          iv_name        = iv_name,
          dv_name        = dv_name)

     tbl_console <- apa_set_table_number(tbl_console, table_number, table_number_explicit)

     if (make_file_flag == TRUE) {
          apa.save(filename = filename, tbl_console)
     }

     return(tbl_console)
}


# d-value table title as richtext, GENERATED from the iv and dv. The "d"
# is a statistic symbol: it stays lowercase and italic, and must be its own run
# -- once the italic flag is lost at the spec level no emitter can recover it.
# "Values" capitalizes because APA capitalizes the second part of a hyphenated
# major word, but that never reaches the symbol, which is a separate run.
dvalue_spec_title <- function(iv_name, dv_name) {
  richtext(list(
    rt_run("Means, Standard Deviations, and "),
    rt_run("d", italic = TRUE),
    rt_run(sprintf("-Values for %s by %s",
                   apa_name_to_title(dv_name), apa_name_to_title(iv_name)))))
}

# d-value table note as richtext (no "Note." prefix). Mirrors the console note;
# M, SD, and every "d" of "d-value(s)" are italic.
dvalue_spec_note <- function() {
  d <- function() rt_run("d", italic = TRUE)
  richtext(list(
    rt_run("M", italic = TRUE), rt_run(" indicates mean. "),
    rt_run("SD", italic = TRUE), rt_run(" indicates standard deviation. "),
    d(), rt_run("-values are estimates calculated using formulas 4.18 and 4.19 from Borenstein, Hedges, Higgins, & Rothstein (2009). "),
    d(), rt_run("-values not calculated if unequal variances prevented pooling. "),
    rt_run("Values in square brackets indicate the 95% confidence interval for each "),
    d(), rt_run("-value.")))
}

# Build an apa_table_spec from the d-value builder's UN-WOVEN source matrices.
# One spec row per IV level; each d-column stacks d over its [CI] via richtext_lines.
dvalue_build_spec <- function(variable_names, descriptives, d_mat, d_ci_mat,
                              table_number, landscape, iv_name, dv_name) {
  n_var <- length(variable_names)
  n_d   <- ncol(d_mat)                                   # n_var - 1
  spec <- apa_spec_start(n_var)
  spec <- apa_col(spec, label = richtext("Variable"), align = "l",
                  values = as.list(variable_names))
  spec <- apa_col(spec, label = apa_sym("M"),  align = "r",
                  values = as.list(as.character(descriptives[, 1])))
  spec <- apa_col(spec, label = apa_sym("SD"), align = "r",
                  values = as.list(as.character(descriptives[, 2])))
  for (j in seq_len(n_d)) {
    cells <- vector("list", n_var)
    for (i in seq_len(n_var)) {
      d_str <- d_mat[i, j]
      if (!nzchar(trimws(d_str))) {
        cells[[i]] <- richtext("")
      } else {
        ci_str <- d_ci_mat[i, j]
        cells[[i]] <- if (nzchar(trimws(ci_str)))
          richtext_lines(richtext(d_str), richtext(ci_str)) else richtext(d_str)
      }
    }
    spec <- apa_col(spec, label = richtext(as.character(j)), align = "r", values = cells)
  }
  spec <- apa_note(spec, dvalue_spec_note())
  num <- apa_table_number_value(table_number)
  apa_spec_finish(spec, title = dvalue_spec_title(iv_name, dv_name),
                  number = num, landscape = landscape,
                  meta = list(table.type = "dvalue"))
}


txt_d <- function(d_in, p_in) {
     # omit significance testing in this version
     # if (p_in<.01) {
     #      star_str <- "**"
     # } else if (p_in<.05) {
     #      star_str <- "*"
     # } else {
          star_str <-""
     #}

     d_str <- sprintf("%1.2f%s",d_in, star_str)
     return(d_str)
}


txt_d_ci <- function(cur_ci) {
     my_low_lim  <- cur_ci$Lower.Conf.Limit.smd
     my_up_lim   <- cur_ci$Upper.Conf.Limit.smd
     ci_str <- sprintf("[%1.2f, %1.2f]",my_low_lim,my_up_lim)
     return(ci_str)
}


get_d_value <- function(group1_data,group2_data) {
     #confirm this is the unbiased one
     m1 <- mean(group1_data)
     m2 <- mean(group2_data)
     v1 <- var(group1_data)
     v2 <- var(group2_data)
     n1 <- length(group1_data)
     n2 <- length(group2_data)
     sp <- sqrt((v1*(n1-1) + v2*(n2-1))/(n1+n2-2))
     d_out <- (m1-m2)/sp
     d_out <- abs(d_out)
     return(d_out)
}



is_equal_var <- function(group1_dv,group2_dv) {
     iv_1 <- rep(1,length(group1_dv))
     iv_2 <- rep(2,length(group2_dv))

     iv <- c(iv_1,iv_2)
     dv <- c(group1_dv,group2_dv)

     my_df <- data.frame(iv,dv)
     my_df$iv <- as.factor(my_df$iv)

     lev_result <- car::leveneTest(dv~iv,center=median,data=my_df)
     lev_p <- lev_result$`Pr(>F)`[1]

     b_result <- TRUE
     if (lev_p<.05) {
          b_result <- FALSE
     }
     return(b_result)
}
