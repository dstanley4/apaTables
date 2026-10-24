#' Create an APA-style regression table
#'
#' @description
#' \code{apa.reg.table()} creates a regression table in APA style from one
#' or more \code{\link[stats]{lm}} results. For each predictor the table
#' reports the unstandardized regression weight (b), the standardized
#' regression weight (beta), and the semi-partial correlation squared, each
#' with a confidence interval, and the zero-order correlation, along with
#' model fit (R-squared). The Details section below describes when beta and
#' the zero-order correlation (r) are left out. When several \code{lm} results are supplied
#' (blocks in a hierarchical regression), the table also reports the change
#' in R-squared for each block. The table prints to the console. The most
#' common next step is saving the table as a Word file to use in a
#' manuscript:
#'
#' \preformatted{blk1 <- lm(sales ~ adverts + airplay, data = album)
#' table1 <- apa.reg.table(blk1)
#' apa.save(filename = "Table1.docx", table1)}
#'
#' The file appears in your working directory -- the folder R is currently
#' pointing at. See the \emph{Word files (apa.save)} section below. The
#' table can also be placed in a Quarto document, so a rendered manuscript
#' comes out with the table already in place -- see the \emph{Quarto
#' documents} section below.
#'
#' @param ... One or more results from \code{\link[stats]{lm}} -- one for
#'   each block in a hierarchical regression. All must use the same
#'   criterion variable, and each needs an intercept and at least one
#'   predictor. Models from \code{glm()} are not supported.
#' @param filename Optional file name. If supplied, the table is also saved
#'   as a Word (\code{.docx}) or HTML (\code{.html}) file, according to the
#'   file extension. Legacy \code{.doc}/\code{.rtf} names are saved as
#'   \code{.docx}. An existing file with that name is replaced.
#' @param table.number Number for the "Table N" line of the output. Defaults
#'   to \code{1}. Use \code{0} (or \code{NA}, or \code{NULL}) for a table with
#'   no number: none is then shown at the console or written to the Word file.
#'   When \code{\link{apa.save}} writes several tables to one file, a table
#'   left at the default takes the next number after the last numbered table before it
#'   (starting at 1), and a number given here is kept. In a Quarto document that uses the apaquarto
#'   extension this value is ignored: apaquarto numbers the tables itself, in
#'   the order they appear in the document.
#' @param prop.var.conf.level Confidence level (\code{.90} or \code{.95},
#'   the default) for the intervals around the proportion-of-variance
#'   measures (semi-partial correlation squared, R-squared, and change in
#'   R-squared). Using \code{.90} keeps conclusions based on these
#'   intervals consistent with the p-value for the block. Any other value
#'   stops with a message.
#' @param landscape Whether this table is placed on a rotated (landscape)
#'   page. The default, \code{NA}, means apaTables decides by measuring the
#'   table against the page. Pass \code{TRUE} to insist on a rotated page or
#'   \code{FALSE} to insist on an upright one. An explicit value is honored
#'   wherever the destination can carry it out; in a Quarto PDF that means
#'   the chunk needs a \code{tbl-} label (see \code{\link{apa.quarto.pdf}}).
#'   A table too wide for the page it is given is shrunk to fit rather
#'   than having a column cut off. Landscape is a property of a PAGE,
#'   so it applies to PDF and Word output and is ignored, without complaint,
#'   for HTML and slides. See \strong{Rotated (landscape) pages} below.
#'
#' @details
#' Confidence intervals for the change in R-squared, and for the
#' semi-partial correlation squared of each predictor when the model has
#' two or more predictors, are calculated with the asymptotic approach of
#' Alf and Graf (1999). Note that Algina, Keselman, and Penfield (2008)
#' found this approach can under some circumstances lead to inaccurate
#' confidence intervals on proportion-of-variance values. The interval is
#' symmetric and, for a small effect, can extend below 0; the limits are
#' held to the range 0 to 1, so a lower limit can print as .00 where the
#' formula gave a negative value.
#'
#' The confidence interval for R-squared -- and, when a model has a single
#' predictor, the interval for the semi-partial correlation squared of
#' that predictor, which then equals R-squared -- is computed by
#' \code{MBESS::ci.R2()} using the noncentral F distribution for fixed
#' predictors.
#'
#' The confidence interval for b is the one \code{\link[stats]{confint}}
#' gives. The interval for beta is that interval rescaled by the ratio of
#' the standard deviation of the predictor to that of the criterion; it
#' treats both sample standard deviations as fixed, so it does not reflect
#' the sampling error in them and can be too narrow (see Jones & Waller, 2013, for
#' intervals that do).
#'
#' beta and r are not shown when any block has a factor (categorical)
#' predictor or a term that R builds into new model columns, such as
#' an interaction written \code{a * b} or \code{a:b}, or \code{poly()},
#' and they are not shown when the models are weighted. Otherwise, a
#' product written with \code{I()}, such as \code{I(a * b)}, keeps beta,
#' and r for that term is left blank; a single transformed term such as
#' \code{log(x)} keeps both, computed on the transformed variable. The
#' table note defines only the columns shown.
#'
#' An unordered categorical predictor is shown with each level compared with
#' the first level (the default coding in R), so the rows are labeled by
#' level, such as \code{factor(cyl)6}. This holds even when
#' \code{options(contrasts = c("contr.sum", "contr.poly"))} was set before
#' the model was fitted, as is common for an ANOVA: \code{apa.reg.table()}
#' fits the model again with the default coding, which gives the same fit
#' and R-squared. Coding chosen for one model -- in
#' \code{lm(contrasts = ...)}, with \code{C()} in the formula, or set on
#' the variable -- is kept when it compares each level with one reference
#' level; other coding, such as effect coding, stops with a message. An
#' ordered factor keeps the polynomial rows (\code{.L}, \code{.Q}) that R
#' gives it.
#'
#' The table describes the models as \code{lm()} fitted them, so cases that
#' \code{lm()} dropped for missing values (the default) are not in it, and
#' the N in the note is the number of cases the models used. Every block
#' must use the same cases. A model with an offset (an \code{offset()} term,
#' or the \code{offset} argument of \code{lm()}) is not supported and stops
#' with a message.
#'
#' @return A table object (class \code{apa_table}). Typing the object name at
#'   the console displays the table. Pass it to \code{\link{apa.save}} to create
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
#' pointing at. Several tables can go into one file, each on a separate page,
#' numbered Table 1, Table 2, and so on in the order you list them:
#'
#' \preformatted{apa.save(filename = "AllTables.docx", table1, table2)}
#'
#' The title above each table and the note below it are written by
#' apaTables from the table itself; on this route there is no way to
#' supply your own wording at this time. Tables are set in 12 point, and a
#' table too wide for an upright page gets a rotated (landscape) page
#' rather than smaller type -- so a wide table coming out sideways
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
#' they behave identically: the YAML
#' \code{format:} line in the document decides what is actually produced, so the name only
#' states intent. The examples on this page use \code{apa.quarto.pdf()} to match the
#' \code{format: apaquarto-pdf} shown above; swap in \code{apa.quarto.docx()} or
#' \code{apa.quarto.html()} together with the matching YAML line for Word or HTML.
#'
#' In the document, the table lives inside a code chunk like this one.
#' \code{\link{apa.quarto.pdf}} displays the table body with the title and
#' note included automatically, so this chunk is complete -- there is no
#' title or note line to write, and the wording stays correct when the
#' analysis changes. The \code{library(apaTables)} line is part of the
#' chunk: without it the document cannot find
#' \code{apa.reg.table()} and the render stops.
#'
#' \preformatted{```{r}
#' #| label: tbl-regression
#' #| message: false
#' #| warning: true
#' library(apaTables)
#' blk1 <- lm(sales ~ adverts + airplay, data = album)
#' table1 <- apa.reg.table(blk1)
#' apa.quarto.pdf(x = table1)
#' ```}
#'
#' That \code{#| label: tbl-regression} line is not optional: without a
#' label starting \code{tbl-}, Quarto builds no float for the chunk, so there
#' is no number, no title, and no note, however you try to supply them.
#'
#' \emph{Wide and tall tables.} If this table turns out wider than the upright
#' (portrait) PDF page, APA style places it on a rotated (landscape) page --
#' and apaTables does that for you. It measures the table and asks apaquarto
#' for a rotated page when one is needed, whether your tables sit in the text
#' or are collected at the end of the manuscript. There is nothing to add to the chunk and nothing to add to the
#' YAML header. A table wider still -- wider than even a rotated page -- is
#' printed smaller instead, as small as it has to be so
#' that no column is cut off. If that takes the type below 6 points, a
#' warning in the rendered draft says so and points to \code{\link{apa.save}}
#' for a full-size Word version. A table too tall for the page needs nothing
#' either: it continues onto the next page by itself, with the column
#' headings repeated at the top of each continuation and the note below the
#' last piece. None of this applies to \code{\link{apa.save}} files: there, each table
#' sits alone on a page, always at full size.
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
#' @references
#' Alf Jr, E. F., & Graf, R. G. (1999). Asymptotic confidence limits for the difference between two squared multiple correlations: A simplified approach. Psychological Methods, 4(1), 70-75.
#'
#' Algina, J., Keselman, H. J., & Penfield, R. J. (2008). Note on a confidence interval for the squared semipartial correlation coefficient. Educational and Psychological Measurement, 68(5), 734-741.
#'
#' Jones, J. A., & Waller, N. G. (2013). Computing confidence intervals for standardized regression coefficients. Psychological Methods, 18(4), 435-453.
#'
#' Kelley, K. (2007). Constructing confidence intervals for standardized effect sizes: Theory, application, and implementation. Journal of Statistical Software, 20(8), 1-24.
#'
#' Steiger, J. H., & Fouladi, R. T. (1992). R2: A computer program for interval estimation, power calculation, and hypothesis testing for the squared multiple correlation. Behavior Research Methods, Instruments, & Computers, 24, 581-582.
#'
#' @examples
#' library(apaTables)
#'
#' # Single-block regression predicting album sales
#' # (album data from Field et al. (2012), Discovering Statistics Using R)
#' blk1 <- lm(sales ~ adverts + airplay, data = album)
#' table1 <- apa.reg.table(blk1, table.number = 1)
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
#' # Clean up
#' unlink(file.path(tempdir(), "table1.docx"))
#'
#' \donttest{
#' # Hierarchical regression ----------------------------------------
#' # Two blocks; more than two can be used
#' blk1 <- lm(sales ~ adverts, data = album)
#' blk2 <- lm(sales ~ adverts + airplay + attract, data = album)
#' table2 <- apa.reg.table(blk1, blk2, table.number = 2)
#'
#' # Interaction product-term test with blocks
#' blk1 <- lm(sales ~ adverts + airplay, data = album)
#' blk2 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data = album)
#' table3 <- apa.reg.table(blk1, blk2, table.number = 3)
#'
#' # Interaction product-term test with blocks and additional product terms
#' blk1 <- lm(sales ~ adverts + airplay, data = album)
#' blk2 <- lm(sales ~ adverts + airplay + I(adverts*adverts) + I(airplay*airplay),
#'            data = album)
#' blk3 <- lm(sales ~ adverts + airplay + I(adverts*adverts) + I(airplay*airplay) +
#'            I(adverts*airplay), data = album)
#' table4 <- apa.reg.table(blk1, blk2, blk3, table.number = 4)
#'
#' # Interaction product-term test with a single regression
#' # (i.e., semi-partial correlation focus)
#' blk1 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data = album)
#' table5 <- apa.reg.table(blk1, table.number = 5)
#'
#' # apa.save() can write several tables into one Word document
#' apa.save(filename = file.path(tempdir(), "regression_tables.docx"),
#'          table1, table2, table3, table4, table5)
#'
#' # Clean up
#' unlink(file.path(tempdir(), "regression_tables.docx"))
#' }
#' @export
apa.reg.table<-function(...,filename=NA,table.number=1, prop.var.conf.level = .95, landscape = NA) {
     regression_results_list <- list(...)

     table_number_explicit <- !missing(table.number)
     table_number <- apa_table_number_value(table.number)
     table.number <- table_number
     is_random_predictors <- FALSE

     filename <- apa_check_filename(filename)
     if (is.na(filename)) {
          make_file_flag=FALSE
     } else {
          make_file_flag=TRUE
     }

     apa_check_landscape(landscape)
     prop_var_conf_level <- apa_check_conf_level(prop.var.conf.level, "prop.var.conf.level")



     L=length(regression_results_list)
     if (L == 0) {
          stop("apa.reg.table() needs at least one model fitted with lm().", call. = FALSE)
     }
     model_label <- function(i) if (L == 1) "the model" else sprintf("block %d", i)
     for (i in seq_len(L)) {
          apa_check_lm("apa.reg.table", regression_results_list[[i]], model_label(i),
                       allow_aov = FALSE)
          cur_terms <- stats::terms(regression_results_list[[i]])
          if (length(attr(cur_terms, "term.labels")) == 0) {
               stop(sprintf("apa.reg.table() needs at least one predictor in each model, but %s has none.",
                            model_label(i)), call. = FALSE)
          }
          if (attr(cur_terms, "intercept") == 0) {
               stop(sprintf("apa.reg.table() needs models with an intercept, but %s was fitted without one (a 0 or - 1 in the formula).",
                            model_label(i)), call. = FALSE)
          }
     }
     if (any(vapply(regression_results_list, apa_model_has_offset, logical(1)))) {
          stop("apa.reg.table error:\napaTables does not support models with an offset (an offset() term, or the offset argument of lm()).\nFit the model without it.", call. = FALSE)
     }
     for (i in seq_len(L)) {
          if (is.null(regression_results_list[[i]]$model)) {
               stop(sprintf("apa.reg.table() needs the data stored in the model, but %s was fitted with model = FALSE; fit it again without model = FALSE.",
                            model_label(i)), call. = FALSE)
          }
     }
     # A categorical predictor is shown with each level compared with the
     # first level, whatever options(contrasts = ...) said when the model was
     # fitted; done before the block checks so they compare like with like.
     regression_results_list <- lapply(seq_len(L), function(i)
          reg_treatment_contrasts(regression_results_list[[i]], model_label(i)))
     is_same_criterion <- c()
     first_result <- regression_results_list[[1]]
     first_criterion <- colnames(first_result$model)[1]
     for (i in 1:L) {
          cur_result <- regression_results_list[[i]]
          cur_criterion_name <- colnames(cur_result$model)[1]
          is_same_criterion[i] <- first_criterion == cur_criterion_name
     }
     if (any(is_same_criterion == FALSE)) {
          stop("apa.reg.table error:\nAll regression objects (i.e., blocks) must use the same criterion.\nThe regression objects used had different criterion variables.", call. = FALSE)
     }


     n <- dim(regression_results_list[[1]]$model)[1]
     n_size <- n
     if (L > 1) {
          # Delta R2 compares blocks fitted to the same cases, in the same
          # order, with the same weights.
          same_vector <- function(a, b) {
               (is.null(a) && is.null(b)) ||
                    (!is.null(a) && !is.null(b) && length(a) == length(b) &&
                     isTRUE(all.equal(unname(a), unname(b))))
          }
          frames <- lapply(regression_results_list, stats::model.frame)
          for (i in 2:L) {
               if (!same_vector(stats::model.response(frames[[i]]), stats::model.response(frames[[1]]))) {
                    stop("apa.reg.table error:\nAll regression objects (i.e., blocks) must be fitted to the same cases, in the same order.\nThese blocks were fitted to different rows of data -- usually because a predictor added in a later block has missing values -- or to the same rows in a different order (for example, after merge() or sorting).\n\nFit every block to one data frame, with the incomplete rows removed once:\ncomplete <- na.omit(my_data[, c(\"y\", \"a\", \"b\", \"c\")])\nblock1 <- lm(y ~ a + b, data = complete)\nblock2 <- lm(y ~ a + b + c, data = complete)", call. = FALSE)
               }
               if (!same_vector(stats::model.weights(frames[[i]]), stats::model.weights(frames[[1]]))) {
                    stop("apa.reg.table error:\nAll regression objects (i.e., blocks) must use the same weights.", call. = FALSE)
               }
          }

          # Each block must be able to reproduce every column of the block
          # before it and add at least one more. Comparing the model matrices
          # rather than the formulas accepts a step written as a new
          # parameterization (wt, then poly(wt, 2)) and refuses blocks that
          # share nothing. (An earlier check compared intersect() with the
          # previous block, which is vacuously TRUE for disjoint blocks, and
          # printed a negative Delta R2 with a CI of [NaN, NaN].)
          for (i in 2:L) {
               x_last <- stats::model.matrix(regression_results_list[[i - 1]])
               x_cur  <- stats::model.matrix(regression_results_list[[i]])
               qr_cur <- qr(x_cur)
               leftover <- qr.resid(qr_cur, x_last)
               col_size <- pmax(sqrt(colSums(x_last^2)), 1e-12)
               spans_last <- all(sqrt(colSums(leftover^2)) / col_size < 1e-7)
               adds_more <- qr_cur$rank > qr(x_last)$rank
               if (!(spans_last && adds_more)) {
                    stop("apa.reg.table error:\nEach regression object (i.e., block) must contain every term of the preceding block, including any interaction, plus at least one more.\n\nFor example:\nblock1 <- lm(y ~ a + b)\nblock2 <- lm(y ~ a + b + c)\n\nThe second block contains all of the predictors from the first block plus additional predictors.\n\nTherefore the command below will work:\napa.reg.table(block1, block2)", call. = FALSE)
               }
          }
     }



     #get analyses for each block
     block_results <- list()
     L <- length(regression_results_list)
     for (i in 1:L) {
          cur_result <- apa_single_block(regression_results_list[[i]],is_random_predictors, prop_var_conf_level)
          block_results[[i]] <- cur_result
     }


     is_multiple_blocks <- FALSE
     if (L>1) {
          is_multiple_blocks <- TRUE
     }

     #Combine blocks
     block_out_txt <- block_results[[1]]$model_details_txt
     block_out_latex <- block_results[[1]]$model_details_latex


     if (is_multiple_blocks == TRUE) {
          cur_block_label <- "Model 1"

          model_label_line <- get_blank_row(block_out_txt)
          model_label_line[1,1] <- cur_block_label
          block_out_txt <- rbind(model_label_line, block_out_txt)

          model_label_line_latex <- get_blank_row(block_out_latex)
          model_label_line_latex[1,1] <- cur_block_label
          block_out_latex <- rbind(model_label_line_latex, block_out_latex)
     }


     last_block_summary <- block_results[[1]]$model_summary_extended
     last_block_lm <- regression_results_list[[1]]
     if (is_multiple_blocks == TRUE) {
          for (i in 2:L) {
               cur_block_lm <- regression_results_list[[i]]

               cur_block_summary <- block_results[[i]]$model_summary_extended

               cur_block_out_txt <- block_results[[i]]$model_details_txt
               cur_block_out_latex <- block_results[[i]]$model_details_latex

               delta_R2_details <- get_delta_R2_blocks(blk2=cur_block_lm,blk1=last_block_lm,summary2=cur_block_summary,summary1=last_block_summary,n, prop_var_conf_level = prop_var_conf_level)

               num_lines <- dim(cur_block_out_txt)[1]

               cur_block_out_txt$difference[num_lines-2] <- delta_R2_details$deltaR2_txt
               cur_block_out_txt$difference[num_lines-1] <- delta_R2_details$deltaR2_CI_txt

               cur_block_out_latex$difference[num_lines-2] <- delta_R2_details$deltaR2_latex
               cur_block_out_latex$difference[num_lines-1] <- delta_R2_details$deltaR2_CI_latex

               last_block_summary <- cur_block_summary
               last_block_lm <- cur_block_lm

               # beta and r are shown only when every block can show them,
               # whichever block is the one that cannot
               if (has_beta_cols(block_out_txt) != has_beta_cols(cur_block_out_txt)) {
                    no_beta <- c("beta", "beta_CI", "r")
                    block_out_txt       <- block_out_txt[, setdiff(names(block_out_txt), no_beta), drop = FALSE]
                    block_out_latex     <- block_out_latex[, setdiff(names(block_out_latex), no_beta), drop = FALSE]
                    cur_block_out_txt   <- cur_block_out_txt[, setdiff(names(cur_block_out_txt), no_beta), drop = FALSE]
                    cur_block_out_latex <- cur_block_out_latex[, setdiff(names(cur_block_out_latex), no_beta), drop = FALSE]
               }

               cur_block_label <- sprintf("Model %g",i)

               model_label_line <- get_blank_row(cur_block_out_txt)
               model_label_line[1,1] <- cur_block_label

               model_label_line_latex <- get_blank_row(cur_block_out_latex)
               model_label_line_latex[1,1] <- cur_block_label

               block_out_txt <- rbind(block_out_txt, model_label_line, cur_block_out_txt)
               block_out_latex <- rbind(block_out_latex,model_label_line, cur_block_out_latex)
          }
     } else {
          block_out_txt <- dplyr::select(block_out_txt, -difference)
          block_out_latex <- dplyr::select(block_out_latex, -difference)

     }


     term_coefs <- unique(unlist(lapply(regression_results_list, term_coef_names)))
     block_out_txt$predictor <- clean_predictor_names(block_out_txt$predictor, term_coefs)
     block_out_latex$predictor <- clean_predictor_names(block_out_latex$predictor, term_coefs)

     spec_source_df <- block_out_txt   # raw dplyr keys + clean predictors, before console rename
     # The note defines beta and r only when the table shows them (factor
     # predictors, weights, or any block with either drop them).
     has_beta <- "beta" %in% names(block_out_txt)
     has_r    <- "r" %in% names(block_out_txt)


     #console table

     first_title_criterion = first_criterion
     #first_title_criterion = gsub("_", " ", first_title_criterion)

     if (is_multiple_blocks == TRUE) {
          table_title <- sprintf("Hierarchical Multiple Regression Predicting %s\n",apa_name_to_title(first_title_criterion))
     } else {
          table_title <- sprintf("Regression Predicting %s\n",apa_name_to_title(first_title_criterion))
     }
     txt_column_names <- get_txt_column_names(block_out_txt)
     if (prop_var_conf_level == .95) {
          txt_column_names <- sub("XX","95",txt_column_names)
     } else {
          txt_column_names <- sub("XX","90",txt_column_names)
     }
     names(block_out_txt) <- txt_column_names


     table_body <- block_out_txt

     table_note <- get_reg_table_note_txt(has_r, has_beta, n_size)

     table_block_results <-  block_results

     tbl_console <- list(table_number = table_number,
                         table_title = table_title,
                         table_body = as.data.frame(table_body),
                         table_note = table_note,
                         table_block_results = table_block_results)

     table_block_results <-  block_results

     class(tbl_console) <- "apa_table"



     if (is_multiple_blocks == TRUE) {
          table_title <- sprintf("Hierarchical Multiple Regression Predicting %s\n",    apa_name_to_title(first_title_criterion))
          table_title_latex <- sprintf("Hierarchical Multiple Regression Predicting %s",apa_name_to_title(first_title_criterion))
     } else {
          table_title <- sprintf("Regression Predicting %s\n",    apa_name_to_title(first_title_criterion))
          table_title_latex <- sprintf("Regression Predicting %s",apa_name_to_title(first_title_criterion))
     }

     reg_spec <- reg_build_spec(spec_source_df, prop_var_conf_level, n_size,
                                table_title_latex, table_number,
                                landscape = apa_landscape_value(landscape))

     table_note_latex <- get_reg_table_note_latex(has_r, has_beta, n_size)

     block_out_latex$predictor <- gsub("_", " ", block_out_latex$predictor)

     tbl_console$latex.body         <- block_out_latex
     tbl_console$latex.table.title  <- table_title_latex
     tbl_console$latex.table.note   <- table_note_latex

     tbl_console$landscape       <- apa_landscape_value(landscape)
     tbl_console$table.type      <- "regression"

     tbl_console$spec <- reg_spec

     tbl_console <- apa_set_table_number(tbl_console, table_number, table_number_explicit)

     if (make_file_flag == TRUE) {
          apa.save(filename = filename, tbl_console)
     }

     return(tbl_console)
}


apa_single_block<-function(cur_blk,is_random_predictors, prop_var_conf_level) {
     #Regression data
     reg_table_data <- cur_blk$model
     n              <- dim(reg_table_data)[1]
     col_names      <- names(reg_table_data)


     #Determine if beta weights should be calculated
     is_weighted    <- "(weights)" %in% col_names
     is_var_factor  <- is_variable_factor(reg_table_data)
     calculate_beta <- TRUE
     calculate_cor  <- TRUE
     # A weighted model drops r as well as beta: cor() of the raw variables
     # is unweighted and would sit beside weighted b, sr2 and R2.
     if (is_weighted == TRUE) {
          calculate_beta <- FALSE
          calculate_cor  <- FALSE
     }
     if (is_var_factor == TRUE) {
          calculate_beta <- FALSE
          calculate_cor  <- FALSE
     }



     #Summary statistics
     model_summary_extended    <- broom::glance(cur_blk) #glance include lm constant in df
     model_summary_extended$df <- summary(cur_blk)$df[1] #use summary information to get df without constant

     #Regression table statistics
     reg_table <- broom::tidy(cur_blk)

     names(reg_table) <- c("predictor","b","SE","t","p")

     #adjust df xyzzy
     if (reg_table$predictor[1] == "(Intercept)") {
          model_summary_extended$df[1] <- model_summary_extended$df[1]-1
     }

     R2          <- model_summary_extended$r.squared
     R2_pvalue   <- model_summary_extended$p.value
     df1         <- model_summary_extended$df
     df2         <- model_summary_extended$df.residual

     prop_var_conf_level_str <- sprintf("%g", round(prop_var_conf_level*100))

     # Defensive: MBESS::conf.limits.nct() runs options(warn = -1) in its
     # own scope and never restores it, which silences every warning for
     # the rest of the session (or the rest of a Quarto render). As of
     # MBESS 5.0.0 ci.R2() routes through conf.limits.ncf(), which does
     # not reach nct() and leaves the option alone -- but DESCRIPTION
     # pins no MBESS version and that internal routing is not part of
     # MBESS's documented interface, so guard here too. Restore the
     # caller's setting when this function exits, including on error, and
     # again right after the call so later warnings here still show.
     old_warn_option <- getOption("warn")
     on.exit(options(warn = old_warn_option), add = TRUE)
     R2CI <- MBESS::ci.R2(R2=R2,df.1=df1,df.2=df2,Random.Predictors = is_random_predictors, conf.level = prop_var_conf_level)
     options(warn = old_warn_option)
     R2LL <- R2CI$Lower.Conf.Limit.R2
     R2UL <- R2CI$Upper.Conf.Limit.R2



     R2_txt     <- strip.leading.zero(add.sig.stars(sprintf("%1.3f",R2),R2_pvalue))
     R2LL_txt   <- strip.leading.zero(sprintf("%1.2f",R2LL))
     R2UL_txt   <- strip.leading.zero(sprintf("%1.2f",R2UL))

     model_summary_txt    <- sprintf("R2 = %s",R2_txt)
     model_summary_CI_txt <- sprintf("%s%% CI[%s,%s]",prop_var_conf_level_str, R2LL_txt,R2UL_txt)

     model_summary_latex    <- sprintf("$R^2$ = %s",R2_txt)
     model_summary_CI_latex <- sprintf("%s\\%% CI[%s,%s]",prop_var_conf_level_str, R2LL_txt,R2UL_txt)

     #Add b-weight CI's
     b_CI <- confint(cur_blk) #uses .95 confidence by default
     LLb  <- b_CI[,c("2.5 %")]
     ULb  <- b_CI[,c("97.5 %")]

     reg_table <- dplyr::mutate(reg_table, LLb=LLb, ULb=ULb)
     reg_table <- dplyr::select(reg_table,predictor, b,LLb,ULb,SE,t,p)

     #table parts
     reg_table_components <- table_without_intercept_row(reg_table)
     reg_table_lower      <- reg_table_components$lower_table
     reg_table_first      <- reg_table_components$first_row #intercept row

     #Check if predictors not data table
     all_predictor_have_data <- TRUE
     for (i in 1:length(reg_table_lower$predictor)) {
          p_name  <- reg_data_name(reg_table_lower$predictor[i])
          is_present <- p_name %in% names(reg_table_data)
          if (is_present == FALSE) {
               all_predictor_have_data <- FALSE
          }
     }
     if (all_predictor_have_data == FALSE) {
          calculate_beta <- FALSE
          calculate_cor  <- FALSE
     }



     #correlation
     if (calculate_cor==TRUE) {
          r_with_criterion <- correlations_with_criterion(reg_table_data)
          # the correlations carry the data column names; give them the
          # term names (backticks included) so the join finds every row
          term_idx <- match(r_with_criterion$predictor, reg_data_name(reg_table_lower$predictor))
          r_with_criterion$predictor[!is.na(term_idx)] <- reg_table_lower$predictor[term_idx[!is.na(term_idx)]]
          reg_table_lower  <- dplyr::full_join(reg_table_lower,r_with_criterion,by="predictor")
     }

     #semi-partial correlation squared
     reg_table_lower   <- dplyr::mutate(reg_table_lower, sr2=(t*t)*(1-R2)/df2, LLsr2=-999,ULsr2=-999)
     number_predictors <- dim(reg_table_lower[,1])[1]
     if (number_predictors>1) {
          #use delta R2 process for CI
          for (i in 1:number_predictors) {
               sr2 <- reg_table_lower$sr2[i]
               ci  <- get_sr2_ci(sr2 = sr2, R2 = R2, n = n, conf_level = prop_var_conf_level)
               LL  <- ci$LL
               UL  <- ci$UL
               reg_table_lower$LLsr2[i] <- LL
               reg_table_lower$ULsr2[i] <- UL
          }
     } else {
          #single predictor so use R2 CI
          reg_table_lower$LLsr2[1] <- R2LL
          reg_table_lower$ULsr2[1] <- R2UL
     }


     #beta
     if (calculate_beta==TRUE) {
          reg_table_lower   <- dplyr::mutate(reg_table_lower, beta = -999, LLbeta=-999,ULbeta=-999)
          for (i in 1:number_predictors) {
               p_name  <- reg_table_lower$predictor[i]
               b       <- reg_table_lower$b[i]
               LLb     <- reg_table_lower$LLb[i]
               ULb     <- reg_table_lower$ULb[i]

               sd_crit <- stats::sd(reg_table_data[,1],na.rm = TRUE)
               sd_pred <- stats::sd(reg_table_data[,reg_data_name(p_name)],na.rm = TRUE)


               beta   <- convert_b_to_beta(b = b  , sd_pred = sd_pred, sd_crit = sd_crit)
               LLbeta <- convert_b_to_beta(b = LLb, sd_pred = sd_pred, sd_crit = sd_crit)
               ULbeta <- convert_b_to_beta(b = ULb, sd_pred = sd_pred, sd_crit = sd_crit)

               # beta    <-   b*(sd_pred / sd_crit)
               # LLbeta  <- LLb*(sd_pred / sd_crit)
               # ULbeta  <- ULb*(sd_pred / sd_crit)

               reg_table_lower$beta[i]   <- beta
               reg_table_lower$LLbeta[i] <- LLbeta
               reg_table_lower$ULbeta[i] <- ULbeta
          }
     }

     #Combine with intercept
     intercept_row         <- get_empty_row(reg_table_lower)
     intercept_row$predictor[1] <- reg_table_first$predictor[1]
     intercept_row$b[1]    <- reg_table_first$b[1]
     intercept_row$LLb[1]  <- reg_table_first$LLb[1]
     intercept_row$ULb[1]  <- reg_table_first$ULb[1]
     intercept_row$t[1]    <- reg_table_first$t[1]
     intercept_row$p[1]    <- reg_table_first$p[1]
     intercept_row$SE[1]   <- reg_table_first$SE[1]

     model_details_extended <- rbind(intercept_row, reg_table_lower)

     L <- dim(model_details_extended)[1]

     CIb_str    <- txt.ci.brackets(model_details_extended$LLb, model_details_extended$ULb, strip_zero = FALSE)
     CIsr2_str  <- txt.ci.brackets(model_details_extended$LLsr2, model_details_extended$ULsr2, strip_zero = TRUE)

     if (calculate_beta == TRUE) {
          CIbeta_str <- txt.ci.brackets(model_details_extended$LLbeta, model_details_extended$ULbeta, strip_zero = FALSE)
     }

     model_details         <- model_details_extended[,1,drop=FALSE]

     model_details$b       <- add.sig.stars(sprintf("%1.2f",model_details_extended$b), model_details_extended$p)
     model_details$b_CI    <- CIb_str

     if (calculate_beta==TRUE) {
          model_details$beta     <- sprintf("%1.2f",model_details_extended$beta)
          model_details$beta[1]  <- ""

          model_details$beta_CI    <- CIbeta_str
          model_details$beta_CI[1] <- ""
     }

     model_details$sr2     <- strip.leading.zero(sprintf("%1.2f", model_details_extended$sr2))
     model_details$sr2    <- add.sig.stars(model_details$sr2, model_details_extended$p)
     model_details$sr2[1]  <- ""
     model_details$sr2_CI     <- CIsr2_str
     model_details$sr2_CI[1]  <- ""







     if (calculate_cor==TRUE) {

          model_details$r     <- strip.leading.zero(add.sig.stars(sprintf("%1.2f",model_details_extended$r), model_details_extended$r_pvalue)) #intercept row issue
          model_details$r[1]  <- ""

          product_rows <- is_product_row(model_details$predictor, term_coef_names(cur_blk))
          model_details$r[product_rows] <- ""

     }



     model_details_txt <- model_details
     model_details_txt$summary      <- ""
     model_details_txt$difference <- "" #blank column to be populated later

     model_details_txt <- add_row_to_model_summary(model_details_txt)
     model_details_txt <- add_row_to_model_summary(model_details_txt)
     model_details_txt <- add_row_to_model_summary(model_details_txt)

     num_row <- dim(model_details_txt)[1]
     model_details_latex <- model_details_txt

     model_details_txt$summary[num_row-2] <- model_summary_txt
     model_details_txt$summary[num_row-1] <- model_summary_CI_txt

     model_details_latex$summary[num_row-2] <- model_summary_latex
     model_details_latex$summary[num_row-1] <- model_summary_CI_latex


     output <- list()
     output$model_summary_extended <- model_summary_extended
     output$model_details_extended <- model_details_extended
     output$model_details_txt      <- model_details_txt
     output$model_details_latex    <- model_details_latex
     output$calculate_beta         <- calculate_beta
     output$calculate_cor          <- calculate_cor

     return(output)
}


# A row is a product when its term multiplies, e.g. I(a * b). The term is
# read as R code rather than searched for "*" or ":", so a variable named
# `a*b` is not a product. Only a coefficient named as a model term is R
# code; any other row is not a product.
is_product_row <- function(row_names, term_coefs) {
     multiplies <- function(e) {
          if (!is.call(e)) return(FALSE)
          if (identical(e[[1]], as.name("*")) || identical(e[[1]], as.name(":"))) return(TRUE)
          any(vapply(as.list(e)[-1], multiplies, logical(1)))
     }
     is_a_product <- vapply(row_names, function(x) {
          if (!x %in% term_coefs) return(FALSE)
          e <- tryCatch(str2lang(x), error = function(err) NULL)
          !is.null(e) && multiplies(e)
     }, logical(1), USE.NAMES = FALSE)
     return(which(is_a_product))
}


is_variable_factor <- function(df) {
     num_var <- dim(df)[2]
     is_var_factor <- FALSE
     for (i in 1:num_var) {
          cur_col <- df[,i]
          if ( is.factor(cur_col)) {is_var_factor <- TRUE}
          if (is.ordered(cur_col)) {is_var_factor <- TRUE}
     }
     return(is_var_factor)
}



output_txt_name <- function(column_name) {
     switch(column_name,
            predictor="Predictor",
            b = "b",
            b_CI = "b_95%_CI",
            beta = "beta",
            beta_CI ="beta_95%_CI",
            sr2="Unique_R2",
            sr2_CI ="Unique_XX%_CI",
            r="r",
            summary="Fit",
            difference="Delta_Fit")
}

get_txt_column_names <- function(df) {
     n <- names(df)
     names_out <- c()
     for (i in 1:length(n)) {
          names_out[i] <-output_txt_name(n[i])
     }
     return(names_out)
}

output_column_width <- function(column_name) {
     narrow <- .60
     wide   <- .95

     switch(column_name,
            predictor=wide,
            b = wide,
            b_CI = wide*1.35,
            beta = narrow*.9,
            beta_CI =wide,
            sr2=narrow*1.1,
            sr2_CI =wide,
            r=narrow,
            summary=wide*1.3,
            difference=wide*1.4)
}

get_reg_table_note_txt <- function(calculate_cor,calculate_beta, n_size) {

     table_note <- paste0(sprintf("Note. N = %g. b = unstandardized regression weight. ", n_size),
                          if (calculate_beta) "beta = standardized regression weight. " else "",
                          "Unique R2 = semipartial correlation squared. ",
                          if (calculate_cor) "r = zero-order correlation. " else "",
                          "CI = confidence interval.  \n* indicates p < .05. ** indicates p < .01.\n")

     return(table_note)

}


get_reg_table_note_latex <- function(calculate_cor,calculate_beta, n_size) {
     table_note <- paste0(sprintf("\\\\textit{Note}. $N$ = %g. $b$ = unstandardized regression weight. ", n_size),
                          if (calculate_beta) "$beta$ = standardized regression weight. " else "",
                          "Unique $R^2$ = semipartial correlation squared. ",
                          if (calculate_cor) "$r$ = zero-order correlation. " else "",
                          "CI = confidence interval. \\\\newline  * indicates $p$ < .05. ** indicates $p$ < .01.")
     return(table_note)
}



convert_b_to_beta <- function(b, sd_pred,sd_crit) {
     beta_value <- b*(sd_pred / sd_crit)
     return(beta_value)
}



get_delta_R2_blocks <- function(blk2,blk1,summary2,summary1,n, prop_var_conf_level) {
     R2_2 <- summary2$r.squared
     R2_1 <- summary1$r.squared

     deltaR2 <- R2_2 - R2_1
     deltaR2_test <- anova(blk2,blk1)
     deltaR2_p <- deltaR2_test$`Pr(>F)`[2]
     deltaR2_str <- strip.leading.zero(add.sig.stars(sprintf("%1.3f",deltaR2),deltaR2_p))

     deltaR2_txt <- sprintf("Delta R2 = %s", deltaR2_str)
     deltaR2_latex <- sprintf("$\\Delta R^2$  = %s", deltaR2_str)



     deltaR2_CI <- get_deltaR2_ci(R2_2 = R2_2, R2_1 = R2_1,n=n, conf_level = prop_var_conf_level)
     deltaR2_LL_str <- strip.leading.zero(sprintf("%1.2f",deltaR2_CI$LL))
     deltaR2_UL_str <- strip.leading.zero(sprintf("%1.2f",deltaR2_CI$UL))


     prop_var_conf_level_str <- sprintf("%g", round(prop_var_conf_level*100))
     deltaR2_CI_txt <- sprintf("%s%% CI[%s, %s]",prop_var_conf_level_str, deltaR2_LL_str,deltaR2_UL_str)
     deltaR2_CI_latex <- sprintf("%s\\%% CI[%s, %s]",prop_var_conf_level_str, deltaR2_LL_str,deltaR2_UL_str)


     output <- list()
     output$deltaR2_txt    <- deltaR2_txt
     output$deltaR2_CI_txt <- deltaR2_CI_txt

     output$deltaR2_latex <- deltaR2_latex
     output$deltaR2_CI_latex <- deltaR2_CI_latex
     output$deltaR2        <- deltaR2
     output$deltaR2_pvalue <- deltaR2_p
     return(output)
}


has_beta_cols <- function(df) {
     return("beta" %in% names(df))
}

clean_predictor_names <- function(x, term_coefs) {
     N <- length(x)
     for (i in 1:N) {
          x[i] <- reg_predictor_label(x[i], x[i] %in% term_coefs)
     }
 return(x)
}

# Return the model with every unordered categorical predictor in the default
# coding (treatment), each level compared with the first, which is what a
# regression table's factor rows are read as. Coding that came from the
# session setting -- options(contrasts = ...), usually typed for an ANOVA --
# is refitted from the model frame and weights stored in the model, never the
# workspace, and the refit's fitted values must equal the original's (the
# same model in another coding). Coding chosen for one model is kept when it
# is dummy coding with level-named columns and refused otherwise. R records
# a choice in one of two places: the contrasts list written in the lm()
# call, or an attribute on the model-frame column (C() in the formula, or
# contrasts() set on the variable). The session setting leaves both empty.
reg_treatment_contrasts <- function(model, label) {
     ctr <- model$contrasts
     if (length(ctr) == 0) return(model)
     mf <- model$model
     call_ctr <- model$call$contrasts
     call_readable <- is.null(call_ctr) ||
          (is.call(call_ctr) && identical(call_ctr[[1]], as.name("list")))
     call_names <- if (!is.null(call_ctr) && call_readable) names(call_ctr)[-1] else character(0)
     r_name <- function(name) if (make.names(name) == name) name else paste0("`", name, "`")

     new_ctr <- ctr
     for (name in names(ctr)) {
          if (is.ordered(mf[[name]])) next
          spec <- ctr[[name]]
          if (identical(spec, "contr.treatment")) next
          chosen <- name %in% call_names || !is.null(attr(mf[[name]], "contrasts"))
          if (!chosen && is.character(spec)) {
               if (!call_readable) {
                    stop(sprintf(paste0("In %s, the contrasts argument of lm() was given as an object, so ",
                                        "apa.reg.table() cannot tell which predictors it sets and which ",
                                        "follow options(contrasts = ...). Write the list inside lm(), such ",
                                        "as lm(..., contrasts = list(%s = \"contr.treatment\"))."),
                                 label, r_name(name)), call. = FALSE)
               }
               new_ctr[[name]] <- "contr.treatment"
               next
          }
          dummy <- reg_dummy_coding(spec, model$xlevels[[name]])
          if (identical(dummy, "named")) next
          fix <- if (grepl("^C\\(", name)) {
               "remove C() from the formula and fit the model again"
          } else {
               sprintf("fit the model again with lm(..., contrasts = list(%s = \"contr.treatment\"))",
                       r_name(name))
          }
          if (identical(dummy, "unnamed")) {
               stop(sprintf(paste0("In %s, %s is dummy coded, but the coding columns are not named by ",
                                   "level, so the rows cannot be labeled; %s."), label, name, fix),
                    call. = FALSE)
          }
          stop(sprintf(paste0("apa.reg.table() shows a categorical predictor with each level compared ",
                              "with one reference level (dummy coding). In %s, %s is coded another way, ",
                              "such as effect coding, set for this model; %s."), label, name, fix),
               call. = FALSE)
     }
     if (identical(new_ctr, ctr)) return(model)

     x <- stats::model.matrix(stats::terms(model), mf, contrasts.arg = new_ctr)
     y <- stats::model.response(mf, "numeric")
     w <- stats::model.weights(mf)
     fit <- if (is.null(w)) stats::lm.fit(x, y) else stats::lm.wfit(x, y, w)
     refit <- model
     refit[names(fit)] <- fit
     refit$contrasts <- attr(x, "contrasts")
     if (!is.null(model$x)) refit$x <- x
     same_model <- length(refit$fitted.values) == length(model$fitted.values) &&
          isTRUE(all.equal(unname(refit$fitted.values), unname(model$fitted.values)))
     if (!same_model) {
          stop(sprintf(paste0("apa.reg.table() could not refit %s with each level of a categorical ",
                              "predictor compared with the first level; fit it again after ",
                              "options(contrasts = c(\"contr.treatment\", \"contr.poly\"))."), label),
               call. = FALSE)
     }
     refit
}

# "named" for dummy coding whose columns are named by level (each column
# marks one level with a 1, the rest 0), "unnamed" for dummy coding without
# level names, and "other" for anything else.
reg_dummy_coding <- function(spec, levels) {
     cm <- tryCatch(if (is.character(spec) || is.function(spec)) do.call(spec, list(levels)) else spec,
                    error = function(e) NULL)
     is_dummy <- is.matrix(cm) && is.numeric(cm) && all(cm %in% c(0, 1)) &&
          all(colSums(cm) == 1) && all(rowSums(cm) <= 1)
     if (!is_dummy) return("other")
     if (!is.null(colnames(cm)) && all(colnames(cm) %in% levels)) "named" else "unnamed"
}

# The coefficients named exactly as a model term, such as log(wt) or
# I(wt * qsec). Those names are R code. Any other coefficient name is a
# term plus a level or a column number (agegrp26-35, poly(wt, 2)1) and is
# not R code, even when the text happens to parse.
term_coef_names <- function(model) {
     intersect(names(stats::coef(model)), attr(stats::terms(model), "term.labels"))
}


# A non-syntactic variable name such as `pre-test` carries backticks in the
# model terms but not in the model frame; this gives the model frame name.
reg_data_name <- function(term_name) {
     sub("^`([^`]*)`$", "\\1", term_name)
}

# The row label is the term as R's summary() prints it, with three changes:
# a product reads "a x b", the I( ) wrapper is dropped, and names carry no
# backticks. Only a coefficient named exactly as a model term is parsed;
# any other (a factor level such as factor(cyl)6 or agegrp26-35) keeps its
# text, less the backticks R adds around a name.
reg_predictor_label <- function(original_string, is_term) {
     if (!isTRUE(is_term)) {
          return(gsub("`", "", original_string))
     }
     e <- tryCatch(str2lang(original_string), error = function(err) NULL)
     if (is.null(e)) {
          return(gsub("`", "", original_string))
     }
     if (is.call(e) && identical(e[[1]], as.name("I")) && length(e) == 2) {
          e <- e[[2]]
     }
     reg_format_term(e)
}

reg_format_term <- function(e) {
     if (is.call(e) && identical(e[[1]], as.name("*")) && length(e) == 3) {
          return(paste(reg_format_term(e[[2]]), "x", reg_format_term(e[[3]])))
     }
     paste(deparse(e, width.cutoff = 500L, backtick = FALSE), collapse = " ")
}


# Richtext header + alignment for one raw regression-block column key.
# Richtext analog of output_txt_name(); used to build the spec.
reg_col_spec <- function(key, prop_var_conf_level = .95) {
  unique_r2 <- function() {
    rt <- richtext(c(list(rt_run("Unique ")), unclass(apa_sym("R2"))))
    attr(rt, "plain") <- "Unique R2"
    rt
  }
  switch(key,
    predictor  = list(label = richtext("Predictor"),               align = "l"),
    b          = list(label = apa_sym("b"),                        align = "r"),
    b_CI       = list(label = apa_ci_label(.95),                   align = "c"),
    beta       = list(label = apa_sym("beta"),                     align = "r"),
    beta_CI    = list(label = apa_ci_label(.95),                   align = "c"),
    sr2        = list(label = unique_r2(),                         align = "r"),
    sr2_CI     = list(label = apa_ci_label(prop_var_conf_level),   align = "c"),
    r          = list(label = apa_sym("r"),                        align = "r"),
    summary    = list(label = richtext("Fit"),                     align = "l"),
    difference = list(label = richtext("\u0394Fit"),               align = "l"),
    stop(sprintf("reg_col_spec: unknown column key '%s'", key))
  )
}

# Regression table note as richtext (no "Note." prefix; apaquarto/apa.save add it).
reg_spec_note <- function(n_size, has_beta = TRUE, has_r = TRUE) {
  runs <- c(
    list(rt_run("N", italic = TRUE), rt_run(sprintf(" = %g. ", n_size))),
    list(rt_run("b", italic = TRUE), rt_run(" = unstandardized regression weight. ")),
    if (has_beta) list(rt_run("\u03b2", italic = TRUE), rt_run(" = standardized regression weight. ")),
    list(rt_run("Unique ")), unclass(apa_sym("R2")),
    list(rt_run(" = semipartial correlation squared. ")),
    if (has_r) list(rt_run("r", italic = TRUE), rt_run(" = zero-order correlation. ")),
    list(rt_run("CI = confidence interval."))
  )
  richtext(runs)
}

# Build an apa_table_spec from the assembled console data.frame (raw dplyr keys,
# Fit-cell strings like "R2 = .629**" / "Delta R2 = .295**" as styled richtext
# (italic R, superscript 2, real Delta glyph -- matching the column headers).
# Anything else (CI lines, blanks) passes through unchanged.
reg_fit_value <- function(v) {
  if (!is.character(v) || length(v) != 1L) return(v)
  if (grepl("^R2 = ", v)) {
    richtext(c(unclass(apa_sym("R2")), list(rt_run(sub("^R2", "", v)))))
  } else if (grepl("^Delta R2 = ", v)) {
    richtext(c(unclass(apa_sym("dR2")), list(rt_run(sub("^Delta R2", "", v)))))
  } else {
    v
  }
}

# clean predictor names, including "Model k" label rows and the Fit summary rows).
reg_build_spec <- function(df, prop_var_conf_level, n_size, table_title, table_number, landscape = NA) {
  keys <- names(df)
  pred <- df$predictor
  other <- setdiff(keys, "predictor")
  other_blank <- if (length(other))
    apply(df[, other, drop = FALSE], 1, function(r) all(r == "")) else rep(TRUE, nrow(df))
  is_label <- grepl("^Model [0-9]+$", pred) & other_blank

  body_df <- df[!is_label, , drop = FALSE]
  rownames(body_df) <- NULL

  row_groups <- list()
  if (any(is_label)) {
    label_pos <- which(is_label)
    retained <- which(!is_label)
    for (k in seq_along(label_pos)) {
      start_orig <- label_pos[k]
      end_orig <- if (k < length(label_pos)) label_pos[k + 1L] - 1L else nrow(df)
      grp_body <- match((start_orig + 1L):end_orig, retained)
      grp_body <- grp_body[!is.na(grp_body)]
      row_groups[[k]] <- list(label = pred[start_orig], rows = grp_body)
    }
  }

  spec <- apa_spec_start(nrow(body_df))
  for (key in keys) {
    cs <- reg_col_spec(key, prop_var_conf_level)
    vals <- as.list(body_df[[key]])
    if (key %in% c("summary", "difference")) vals <- lapply(vals, reg_fit_value)
    spec <- apa_col(spec, label = cs$label, align = cs$align, values = vals)
  }
  for (g in row_groups) spec <- apa_row_group(spec, label = g$label, rows = g$rows)
  spec <- apa_note(spec, reg_spec_note(n_size, has_beta = "beta" %in% keys,
                                       has_r = "r" %in% keys))
  spec <- apa_note(spec, stars_spec_note())

  num <- apa_table_number_value(table_number)
  apa_spec_finish(spec, title = table_title, number = num, landscape = landscape,
                  meta = list(table.type = "regression",
                              prop.var.conf.level = prop_var_conf_level))
}
