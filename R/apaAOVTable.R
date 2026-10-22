# ANOVA (aov) table note as richtext (no "Note." prefix). Mirrors the console note;
# partial eta-squared rendered as the symbol to match the column header.
aov_spec_note <- function() {
  richtext(list(
    rt_run("SS", italic = TRUE), rt_run(" = Sum of squares. "),
    rt_run("df", italic = TRUE), rt_run(" = degrees of freedom. "),
    rt_run("MS", italic = TRUE), rt_run(" = mean square. "),
    rt_run("CI indicates the confidence interval for "),
    rt_run("\u03b7", italic = TRUE), rt_run("2", script = "super"),
    rt_run("p", script = "sub"), rt_run(".")))
}

# Return the model refitted so that every factor's contrasts sum to zero,
# which Type III tests need for a factor's row to be its main effect. A model
# whose factors already sum to zero (contr.sum, contr.helmert, contr.poly)
# comes back unchanged. The refit uses only what the model object stores --
# its model frame, weights and offset -- never the workspace, so it is the
# model the user fitted whatever has happened to their data since. Its
# fitted values must equal the original's (the same model in a different
# coding); a model stored without its frame (lm(..., model = FALSE)) cannot
# be refitted, and the user is told how to set the contrasts instead.
aov_sum_contrasts <- function(lm_output) {
  ctr <- lm_output$contrasts
  if (length(ctr) == 0) return(lm_output)

  sums_to_zero <- function(name) {
    spec <- ctr[[name]]
    lev  <- lm_output$xlevels[[name]]
    cm <- tryCatch({
      if (is.character(spec)) do.call(spec, list(lev)) else spec
    }, error = function(e) NULL)
    is.matrix(cm) && is.numeric(cm) && all(abs(colSums(cm)) < 1e-8)
  }
  needs_sum <- !vapply(names(ctr), sums_to_zero, logical(1))
  if (!any(needs_sum)) return(lm_output)

  new_ctr <- ctr
  for (name in names(ctr)[needs_sum]) new_ctr[[name]] <- "contr.sum"

  cannot_refit <- function() {
    stop("apa.aov.table() needs sum-to-zero contrasts for a Type III table and could not refit this model to use them; ",
         "add options(contrasts = c(\"contr.sum\", \"contr.poly\")) before the lm() line, fit the model again, ",
         "and run apa.aov.table() again.", call. = FALSE)
  }
  mf <- lm_output$model
  if (is.null(mf)) cannot_refit()

  # The same steps lm() itself takes after building its model frame.
  x <- stats::model.matrix(stats::terms(lm_output), mf, contrasts.arg = new_ctr)
  y <- stats::model.response(mf, "numeric")
  w <- stats::model.weights(mf)
  offset <- stats::model.offset(mf)
  fit <- if (is.null(w)) {
    stats::lm.fit(x, y, offset = offset)
  } else {
    stats::lm.wfit(x, y, w, offset = offset)
  }
  refit <- lm_output
  refit[names(fit)] <- fit
  refit$contrasts <- attr(x, "contrasts")

  same_model <- length(refit$fitted.values) == length(lm_output$fitted.values) &&
    isTRUE(all.equal(unname(refit$fitted.values), unname(lm_output$fitted.values)))
  if (!same_model) cannot_refit()
  refit
}

# Build an apa_table_spec from the aov builder's latex.body. Suppress the
# (Intercept) row; keep Error. CI column label tracks conf.level.
aov_build_spec <- function(latex_body, conf_level, table_title, table_number, landscape) {
  body <- latex_body[latex_body[["Predictor"]] != "(Intercept)", , drop = FALSE]
  n_row <- nrow(body)
  ci_label <- richtext(sprintf("%g%% CI", round(conf_level * 100)))
  ci_col   <- ncol(body)                                  # last column is the CI column
  v <- function(col) as.list(as.character(body[[col]]))
  spec <- apa_spec_start(n_row)
  spec <- apa_col(spec, label = richtext("Predictor"), align = "l", values = v("Predictor"))
  spec <- apa_col(spec, label = apa_sym("SS"),  align = "r", values = v("SS"))
  spec <- apa_col(spec, label = apa_sym("df"),  align = "r", values = v("df"))
  spec <- apa_col(spec, label = apa_sym("MS"),  align = "r", values = v("MS"))
  spec <- apa_col(spec, label = apa_sym("F"),   align = "r", values = v("F"))
  spec <- apa_col(spec, label = apa_sym("p"),   align = "r", values = v("p"))
  spec <- apa_col(spec, label = apa_sym("eta2p"), align = "r", values = v("partial_eta2"))
  spec <- apa_col(spec, label = ci_label, align = "r", values = as.list(as.character(body[[ci_col]])))
  spec <- apa_note(spec, aov_spec_note())
  num <- apa_table_number_value(table_number)
  apa_spec_finish(spec, title = trimws(table_title), number = num, landscape = landscape,
                  meta = list(table.type = "aov"))
}

#' Create an APA-style fixed-effects ANOVA table
#'
#' \code{apa.aov.table()} creates a fixed-effects ANOVA table in APA style
#' from an \code{\link[stats]{lm}} result. For each predictor the table
#' reports the sum of squares, degrees of freedom, mean square, F, p, and
#' partial eta-squared with its confidence interval. The table prints to the console. The most
#' common next step is saving the table as a Word file to use in a
#' manuscript:
#'
#' \preformatted{options(contrasts = c("contr.sum", "contr.poly"))
#' lm_output <- lm(libido ~ dose, data = viagra)
#' table1 <- apa.aov.table(lm_output)
#' apa.save("Table1.docx", table1)}
#'
#' The file appears in your working directory -- the folder R is currently
#' pointing at. See the \emph{Word files (apa.save)} section below. The
#' table can also be placed in a Quarto document, so a rendered manuscript
#' comes out with the table already in place -- see the \emph{Quarto
#' documents} section below.
#'
#' @param lm_output A result from \code{\link[stats]{lm}} in which all
#'   predictors are factors (categorical variables).
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
#' @param conf.level Confidence level (\code{.90}, the default, or
#'   \code{.95}) for the interval around partial eta-squared. Using
#'   \code{.90} keeps conclusions based on this interval consistent with
#'   the p-value.
#' @param type Sum-of-squares type: \code{2} (Type II) or \code{3}
#'   (Type III, the default).
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
#' Confidence intervals for partial eta-squared are calculated with the
#' noncentral approach described by Smithson (2001) and Fidler and Thompson
#' (2001). For Type III sums of squares to match programs such as SPSS,
#' factor contrasts must sum to zero. \code{apa.aov.table()} sees to this
#' itself: when \code{type = 3} and a factor uses R's default treatment
#' contrasts, it refits the model with sum-to-zero contrasts, from the
#' data stored in the model object, before computing the table, so the
#' table is the same whether or not you set them. The examples still set
#' \code{options(contrasts = c("contr.sum", "contr.poly"))}, which keeps
#' the coefficients of your own \code{lm()} model consistent with the
#' table.
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
#' analysis changes. The \code{library(apaTables)} line is easy to leave
#' out and it matters: without it the document cannot find
#' \code{apa.aov.table()} and the render stops. The
#' \code{options(contrasts = ...)} line sets sum-to-zero contrasts before
#' the model is fitted. \code{apa.aov.table()} uses those contrasts for a
#' Type III table whether or not the line is there, so the table is right
#' either way; the line keeps your own model's coefficients in the same
#' coding as the table:
#'
#' \preformatted{```{r}
#' #| label: tbl-anova
#' #| message: false
#' #| warning: true
#' library(apaTables)
#' options(contrasts = c("contr.sum", "contr.poly"))
#' lm_output <- lm(libido ~ dose, data = viagra)
#' table1 <- apa.aov.table(lm_output)
#' apa.quarto.pdf(table1)
#' ```}
#'
#' That \code{#| label: tbl-anova} line is not optional: without a label
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
#' Smithson, M. (2001). Correct confidence intervals for various regression effect sizes and parameters: The importance of noncentral distributions in computing intervals. Educational and Psychological Measurement, 61(4), 605-632.
#'
#' Fidler, F., & Thompson, B. (2001). Computing correct confidence intervals for ANOVA fixed- and random-effects effect sizes. Educational and Psychological Measurement, 61(4), 575-604.
#' @examples
#' library(apaTables)
#'
#' op <- options()   # save the current settings, restored at the end
#'
#' # 1-way ANOVA of libido by dose
#' # (viagra data from Field et al. (2012), Discovering Statistics Using R)
#' # Sum-to-zero contrasts, the coding the Type III table uses:
#' options(contrasts = c("contr.sum", "contr.poly"))
#' lm_output <- lm(libido ~ dose, data = viagra)
#' table1 <- apa.aov.table(lm_output, table.number = 4)
#' table1
#'
#' # 2-way ANOVA from Fidler & Thompson (2001)
#' options(contrasts = c("contr.sum", "contr.poly"))
#' lm_output <- lm(dv ~ a*b, data = fidler_thompson)
#' table2 <- apa.aov.table(lm_output, table.number = 5)
#'
#' # 2-way ANOVA for the goggles data
#' # (from Field et al. (2012), Discovering Statistics Using R)
#' options(contrasts = c("contr.sum", "contr.poly"))
#' lm_output <- lm(attractiveness ~ gender*alcohol, data = goggles)
#' table3 <- apa.aov.table(lm_output, table.number = 6)
#'
#' # Word documents ----------------------------------------
#' # apa.save() can write several tables into one Word document
#' # (here, in a temporary
#' # folder; for real use give an ordinary name, such as
#' # apa.save("Table1.docx", table1), and the file appears in your
#' # working directory)
#' apa.save(file.path(tempdir(), "my_tables.docx"), table1, table2, table3)
#'
#' # Quarto documents ----------------------------------------
#' # Print a ready-to-paste apaquarto code chunk:
#' apa.quarto.template(table1)
#'
#' # Or write your own chunk; apa.quarto.pdf() displays the table body:
#' apa.quarto.pdf(table1)
#'
#' # Clean up
#' unlink(file.path(tempdir(), "my_tables.docx"))
#' options(op)
#'
#' @export
apa.aov.table<-function(lm_output,filename=NA,table.number=1, conf.level=.90,type=3, landscape=NA) {
     table_number_explicit <- !missing(table.number)
     table_number <- apa_table_number_value(table.number)
     table.number <- table_number
     conf_level <- conf.level

     if ((conf_level!=.90)&(conf_level!=.95)) {conf_level=.90}

     if (is.na(filename)) {
          make_file_flag <- FALSE
     } else {
          make_file_flag <- TRUE
     }

     dv_name <- names(lm_output$model)[1]

     # Type III tests of a factor in an interaction are only main effects
     # when the factor's contrasts sum to zero; under R's default treatment
     # contrasts they are simple effects at the reference level. Type II
     # sums of squares do not depend on the coding, so only Type III refits.
     if (identical(as.character(type), "3") || identical(as.character(type), "III")) {
          lm_output <- aov_sum_contrasts(lm_output)
     }

     table_values <- car::Anova(lm_output,type=type)
     table_values <- broom::tidy(table_values)
     names(table_values) <- c("Predictor","SS", "df","Fvalue","pvalue")

     # Everything below is computed at full precision; rounding happens only
     # when the cells are formatted. Rounding SS first turned every SS of a
     # small-scale DV into 0, and partial eta-squared into 0/0.
     table_out <- table_values

     num_rows <- dim(table_out)[1]
     start_row <- 1
     if (table_out$Predictor[1]=="(Intercept)") {
          start_row <- 2
     }
     if (table_out$Predictor[num_rows]=="Residuals") {
          table_out$Predictor[num_rows] <- "Error"
     }

     ss_total <- sum(table_out$SS[start_row:num_rows])
     ss_error <- table_out$SS[num_rows]

     df_error <- table_out$df[num_rows]
     MSvalue <- rep(NA, num_rows)
     partial_eta_sq <- rep(NA, num_rows)
     LL_partial_eta_sq <- rep(NA, num_rows)
     UL_partial_eta_sq <- rep(NA, num_rows)
     for (i in start_row:(num_rows-1)) {
          ss_effect <- table_out$SS[i]
          MSvalue[i] <- table_out$SS[i]/table_out$df[i]
          partial_eta_sq[i] <- ss_effect / (ss_effect + ss_error)
          LL_partial_eta_sq[i] <- get.ci.partial.eta.squared(F.value=table_out$Fvalue[i], df1=table_out$df[i],df2=df_error,conf.level = conf_level)$LL
          UL_partial_eta_sq[i] <- get.ci.partial.eta.squared(F.value=table_out$Fvalue[i], df1=table_out$df[i],df2=df_error,conf.level = conf_level)$UL
     }
     MSvalue[1] <- table_out$SS[1]/table_out$df[1]
     MSvalue[num_rows] <- table_out$SS[num_rows]/table_out$df[num_rows]


     Predictor <- table_out$Predictor
     Predictor <- gsub(":"," x ", Predictor)
     SSvalue   <- sprintf("%1.2f",table_out$SS)
     MSvalue   <- sprintf("%1.2f",MSvalue)
     Fvalue    <- sprintf("%1.2f",table_out$Fvalue)
     df        <- table_out$df
     p         <- sprintf("%1.3f",table_out$pvalue)
     p         <- strip.leading.zero(p)
     idp0 <- p == ".000"
     p[idp0] <- "<.001"



     partial_eta_sq    <- strip.leading.zero(sprintf("%1.2f",partial_eta_sq))
     partial_eta_sq_LL <- strip.leading.zero(sprintf("%1.2f",LL_partial_eta_sq))
     partial_eta_sq_UL <- strip.leading.zero(sprintf("%1.2f",UL_partial_eta_sq))

     partial_eta_sq_CI <- rep("", num_rows)
     for (i in start_row:(num_rows-1)) {
          partial_eta_sq_CI[i] <- sprintf("[%s, %s]",partial_eta_sq_LL[i],partial_eta_sq_UL[i])
     }

     table_out <- cbind(Predictor,SSvalue,df,MSvalue,Fvalue,p,partial_eta_sq, partial_eta_sq_CI)
     table_out <- data.frame(table_out,stringsAsFactors = FALSE)
     table_out[table_out=="NA"] <- ""

     table_out_txt   <- table_out
     table_out_names <- get_txt_column_names_anova(table_out_txt)
     if (conf_level==.95) {
          table_out_names <- sub("CI_partial_eta2","CI_95_partial_eta2",table_out_names)
     } else {
          table_out_names <- sub("CI_partial_eta2","CI_90_partial_eta2",table_out_names)
     }
     names(table_out_txt) <- table_out_names

     # One title for every destination: console, LaTeX and spec. Until
     # 2026-08-02 a second sprintf below silently overwrote this with
     # "Fixed-Effects ANOVA Results for %s" for the LaTeX and spec paths only,
     # so the console and every rendered document disagreed. Author's ruling:
     # unify on this, the shorter form.
     table_title <- sprintf("ANOVA Results for %s\n",apa_name_to_title(dv_name))
     table_body  <- table_out_txt
     table_note  <- sprintf("Note: Values in square brackets indicate the bounds of the %1.0f%% confidence interval for partial eta-squared",conf_level*100)
     table_note <- "Note. SS = Sum of squares. df = degrees of freedom. MS = mean square. CI indicates the confidence interval for partial eta-squared."

     tbl_console <- list(table_number = table_number,
                         table_title = table_title,
                         table_body = table_body,
                         table_note = table_note)

     class(tbl_console) <- "apa_table"

     # Ver 3.0 add ons

     # conf_level, not conf.level: an out-of-range conf.level is replaced by
     # .90 above, and every consumer of this value labels a column with it.
     tbl_console$ci.conf.level = conf_level
     latex_note <- "\\\\textit{Note}. $SS$ = Sum of squares. $df$ = degrees of freedom. $MS$ = mean square. CI indicates the confidence interval for $\\\\eta_{partial}^2$."

     tbl_console$latex.table.note <- latex_note
     tbl_console$latex.table.title <- table_title
     tbl_console$latex.body         <- table_body

     tbl_console$landscape      <- apa_landscape_value(landscape)
     # "aov", NOT "aovstats": this field must agree with the spec's
     # meta$table.type, which is what the type-to-builder map in R/quarto.R
     # keys on -- anyone who reasonably assumes the two are the same field
     # must get a value the map has an entry for. "aovstats" existed only on
     # the unreleased v3 line (the released predecessor to 4.0.0 is 2.0.8), so
     # no user code relies on it. Nothing in R/ reads this field; it only
     # reports what the object is.
     tbl_console$table.type      <- "aov"

     tbl_console$spec <- aov_build_spec(
       latex_body   = tbl_console$latex.body,
       conf_level   = conf_level,
       table_title  = tbl_console$latex.table.title,
       table_number = table.number,
       landscape    = tbl_console$landscape)

     tbl_console <- apa_set_table_number(tbl_console, table_number, table_number_explicit)

     if (make_file_flag == TRUE) {
          apa.save(filename = filename, tbl_console)
     }

     return(tbl_console)
}


output_txt_name_anova <- function(column_name) {
     switch(column_name,
            Predictor="Predictor",
            SSvalue = "SS",
            MSvalue = "MS",
            df = "df",
            Fvalue = "F",
            p = "p",
            partial_eta_sq ="partial_eta2",
            partial_eta_sq_CI ="CI_partial_eta2",
            df_num = "df_num",
            df_den = "df_den",
            df_num_int = "df_num",
            df_den_int = "df_den",
            SS_num = "SS_num",
            SS_den = "SS_den",
            Epsilon    = "Epsilon",
            ges        = "ges")
}

get_txt_column_names_anova <- function(df) {
     n <- names(df)
     names_out <- c()
     for (i in 1:length(n)) {
          names_out[i] <-output_txt_name_anova(n[i])
     }
     return(names_out)
}







output_column_width_anova <- function(column_name) {
     narrow <- .60
     wide   <- .95
     switch(column_name,
            Predictor = wide,
            SSvalue   = narrow*1.5,
            MSvalue   = narrow*1.5,
            df        = narrow*.8,
            Fvalue    = narrow*1.8,
            p         = narrow,
            ges       = narrow,
            Epsilon   = narrow*1.2,
            SS_num    = narrow*1.5,
            SS_den    = narrow*1.8,
            df_num    = narrow*1.5,
            df_den    = narrow*1.5,
            df_num_int = narrow,
            df_den_int = narrow,
            partial_eta_sq    = wide,
            partial_eta_sq_CI = wide)
}

# get_anova_table_note_txt <- function(calculate_cor,calculate_beta) {
#           table_note <- "Note. * indicates p < .05; ** indicates p < .01.\nA significant b-weight indicates the beta-weight and semi-partial correlation are also significant.\nb represents unstandardized regression weights; beta indicates the standardized regression weights; \nsr2 represents the semi-partial correlation squared; r represents the zero-order correlation.\nSquare brackets are used to enclose the lower and upper limits of a confidence interval.\n"
#      return(table_note)
# }








