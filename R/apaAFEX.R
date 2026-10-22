# Map one afex latex.body column name to a richtext header.
afex_col_label <- function(name) {
  switch(name,
    "Predictor" = richtext("Predictor"),
    "df_num"    = apa_sym("df_num"),
    "df_den"    = apa_sym("df_den"),
    "Epsilon"   = apa_sym("epsilon"),
    "MSE"       = apa_sym("MSE"),
    "F"         = apa_sym("F"),
    "p"         = apa_sym("p"),
    "ges"       = apa_sym("eta2g"),
    richtext(name))
}

# afex table note as richtext (no "Note." prefix). The Epsilon sentence appears
# only when an Epsilon column is present (within-subjects + GG/HF correction).
# correction: "GG" | "HF" | "none" \u2014 controls the multiplier name in the Epsilon sentence.
afex_spec_note <- function(has_epsilon, correction = "GG") {
  runs <- list(
    rt_run("df", italic = TRUE), rt_run("Num", script = "sub"),
    rt_run(" = degrees of freedom numerator. "),
    rt_run("df", italic = TRUE), rt_run("Den", script = "sub"),
    rt_run(" = degrees of freedom denominator. "))
  if (isTRUE(has_epsilon)) {
    multiplier_name <- if (correction == "HF") "Huynh-Feldt" else "Greenhouse-Geisser"
    # "p-values" carries a statistic symbol in running prose, so the p is
    # italic here exactly as it is everywhere else in this note. It was one
    # plain run until 2026-08-01; every other symbol in the sentence was
    # already marked, which is what made the single roman p easy to miss.
    runs <- c(runs, list(
      rt_run("\u03b5", italic = TRUE),
      rt_run(paste0(" = ", multiplier_name,
                    " multiplier for degrees of freedom, ")),
      rt_run("p", italic = TRUE),
      rt_run("-values and degrees of freedom in the table incorporate this correction. ")))
  }
  runs <- c(runs, list(
    rt_run("MSE", italic = TRUE), rt_run(" = mean square error. "),
    rt_run("\u03b7", italic = TRUE), rt_run("2", script = "super"),
    rt_run("g", script = "sub"), rt_run(" = generalized eta-squared.")))
  richtext(runs)
}

# Build an apa_table_spec from the afex builder's latex.body (variable layout).
# correction: "GG" | "HF" | "none" -- passed through to afex_spec_note for the Epsilon sentence.
afex_build_spec <- function(latex_body, table_title, table_number, landscape, correction = "GG") {
  cols  <- names(latex_body)
  n_row <- nrow(latex_body)
  spec  <- apa_spec_start(n_row)
  for (nm in cols) {
    al <- if (nm == "Predictor") "l" else "r"
    spec <- apa_col(spec, label = afex_col_label(nm), align = al,
                    values = as.list(as.character(latex_body[[nm]])))
  }
  spec <- apa_note(spec, afex_spec_note(has_epsilon = "Epsilon" %in% cols, correction = correction))
  num <- apa_table_number_value(table_number)
  apa_spec_finish(spec, title = trimws(table_title), number = num, landscape = landscape,
                  meta = list(table.type = "afex"))
}

#' Create an APA-style ANOVA table from afex output
#'
#' \code{apa.afex.table()} creates an ANOVA table in APA style from the
#' result of \code{afex::aov_ez()}. It handles between-participant,
#' within-participant, and mixed designs. For each effect the table reports
#' the numerator and denominator degrees of freedom, mean square error, F,
#' p, and generalized eta-squared; for within-participant effects with a
#' sphericity correction, the epsilon multiplier is also reported. The table prints to the console. The most
#' common next step is saving the table as a Word file to use in a
#' manuscript:
#'
#' \preformatted{goggles_results <- afex::aov_ez(id = "participant",
#'      dv = "attractiveness", data = goggles,
#'      between = c("gender", "alcohol"))
#' table1 <- apa.afex.table(afex.output = goggles_results)
#' apa.save(filename = "Table1.docx", table1)}
#'
#' The file appears in your working directory -- the folder R is currently
#' pointing at. See the \emph{Word files (apa.save)} section below. The
#' table can also be placed in a Quarto document, so a rendered manuscript
#' comes out with the table already in place -- see the \emph{Quarto
#' documents} section below.
#'
#' @param afex.output The result returned by \code{afex::aov_ez()} (an
#'   \code{afex_aov} object).
#' @param correction Sphericity correction for within-participant effects:
#'   \code{"GG"} (Greenhouse-Geisser, the default), \code{"HF"}
#'   (Huynh-Feldt), or \code{"none"}. Degrees of freedom and p-values in
#'   the table incorporate the correction.
#' @param table.title Optional string with the table title. When left empty
#'   (the default) the title is "ANOVA Results". The title given here is used
#'   everywhere the table appears: at the console, in a Word or HTML file, and
#'   in a Quarto document.
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
#' \strong{Two other packages must be installed first.} This table is built
#' from the output of \code{afex::aov_ez()}, so the afex package has to be
#' installed; and repeated-measures data usually arrive with one row per
#' participant, whereas \code{aov_ez()} needs one row per observation, so
#' \code{tidyr::pivot_longer()} is normally used to reshape them. Neither
#' package is installed with apaTables. In the R console, type:
#'
#' \preformatted{install.packages(c("afex", "tidyr"))}
#'
#' Without them the render stops at the chunk below with
#' \code{there is no package called 'afex'}.
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
#' \code{apa.afex.table()} and the render stops.
#'
#' \preformatted{```{r}
#' #| label: tbl-anova
#' #| message: false
#' #| warning: true
#' library(apaTables)
#' goggles_results <- afex::aov_ez(id = "participant",
#'                    dv = "attractiveness", data = goggles,
#'                    between = c("gender", "alcohol"))
#' table1 <- apa.afex.table(afex.output = goggles_results)
#' apa.quarto.pdf(x = table1)
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
#' if (requireNamespace("afex", quietly = TRUE)) {
#'
#' #
#' # ** Example 1: Between Participant Predictors
#' #
#'
#' goggles <- apaTables::goggles
#'
#' goggles_results <- afex::aov_ez(id = "participant", dv = "attractiveness",
#'                                 data = goggles,
#'                                 between = c("gender", "alcohol"))
#'
#' # Make APA table (all three tables are saved together at the end)
#' goggles_table <- apa.afex.table(afex.output = goggles_results)
#' goggles_table
#'
#' # Quarto documents ----------------------------------------
#' # Print a ready-to-paste apaquarto code chunk:
#' apa.quarto.template(x = goggles_table)
#'
#' # Or write your own chunk; apa.quarto.pdf() displays the table body:
#' apa.quarto.pdf(x = goggles_table)
#' }
#'
#' \donttest{
#' if (requireNamespace("afex", quietly = TRUE) &&
#'     requireNamespace("tidyr", quietly = TRUE)) {
#'
#' #
#' # ** Example 2: Within Participant Predictors
#' #
#'
#' drink_attitude_wide <- apaTables::drink_attitude_wide
#'
#' # Convert data from wide format to long format where one row represents
#' # one OBSERVATION. Wide format column names MUST represent levels of each
#' # variable separated by an underscore. See vignette for further details.
#'
#' drink_attitude_long <- tidyr::pivot_longer(drink_attitude_wide,
#'                                            cols = beer_positive:water_neutral,
#'                                            names_to = c("drink", "imagery"),
#'                                            names_sep = "_",
#'                                            values_to = "attitude")
#'
#' drink_attitude_long$drink <- as.factor(drink_attitude_long$drink)
#' drink_attitude_long$imagery <- as.factor(drink_attitude_long$imagery)
#'
#' drink_attitude_results <- afex::aov_ez(id = "participant", dv = "attitude",
#'                                        data = drink_attitude_long,
#'                                        within = c("drink", "imagery"))
#'
#' # Make APA table (saved together with the others below)
#' drink_table <- apa.afex.table(afex.output = drink_attitude_results)
#'
#' #
#' # ** Example 3: Between and Within Participant Predictors
#' #
#'
#' dating_wide <- apaTables::dating_wide
#'
#' # Convert data from wide format to long format where one row represents
#' # one OBSERVATION. Wide format column names MUST represent levels of each
#' # variable separated by an underscore. See vignette for further details.
#'
#' dating_long <- tidyr::pivot_longer(dating_wide,
#'                                    cols = attractive_high:ugly_none,
#'                                    names_to = c("looks", "personality"),
#'                                    names_sep = "_",
#'                                    values_to = "date_rating")
#'
#' dating_long$looks <- as.factor(dating_long$looks)
#' dating_long$personality <- as.factor(dating_long$personality)
#'
#' dating_results <- afex::aov_ez(id = "participant", dv = "date_rating",
#'                                data = dating_long,
#'                                between = "gender",
#'                                within = c("looks", "personality"))
#'
#' # Make APA table (saved together with the others below)
#' dating_table <- apa.afex.table(afex.output = dating_results)
#'
#' # ----------------------------------------------------------------
#' # Save the tables to a Microsoft Word (.docx) file
#' # ----------------------------------------------------------------
#' # apa.save() can write several tables into one Word document. This
#' # example writes to a temporary folder (tempdir()) so it leaves no
#' # files on your computer; for real use give an ordinary name such as
#' # "Tables_afex.docx".
#' apa.save(filename = file.path(tempdir(), "tables_afex.docx"),
#'          goggles_table,
#'          drink_table,
#'          dating_table)
#'
#' # delete demo file
#' unlink(file.path(tempdir(), "tables_afex.docx"))
#' }
#' }
#' @export
apa.afex.table <- function(afex.output, correction = "GG", table.title = "", filename = NA, table.number = 1, landscape = NA) {

     afex_obj <- afex.output
     table_title <- table.title
     # The title the user asked for, if any -- used for every output (console,
     # Word, HTML, Quarto). Empty means the default title below.
     table_title_supplied <- is.character(table.title) &&
          length(table.title) == 1L && !is.na(table.title) &&
          nzchar(trimws(table.title))
     table_number_explicit <- !missing(table.number)
     table_number <- apa_table_number_value(table.number)
     table.number <- table_number

     if (!requireNamespace("afex", quietly = TRUE)) {
          stop("The 'afex' package is required. Install it with install.packages('afex').")
     }

     if (!inherits(afex_obj, "afex_aov")) {
          stop("Input must be an afex_aov object from afex::aov_ez().")
     }

     if (is.na(filename)) {
          make_file_flag <- FALSE
     } else {
          make_file_flag <- TRUE
     }

     # Determine design type
     within_factors <- names(attr(afex_obj, "within"))
     between_factors <- names(attr(afex_obj, "between"))

     has_within <- length(within_factors) > 0
     has_between <- length(between_factors) > 0

     # Extract ANOVA table
     anova_df <- as.data.frame(anova(afex_obj, correction = correction))

     # Build internal data frame
     effect_names <- rownames(anova_df)
     df_internal <- data.frame(
          Predictor = effect_names,
          df_num = anova_df$`num Df`,
          df_den = anova_df$`den Df`,
          MSvalue = anova_df$MSE,
          Fvalue = anova_df[["F"]],
          pvalue = anova_df$`Pr(>F)`,
          ges = anova_df$ges,
          stringsAsFactors = FALSE
     )

     # Handle corrections and epsilon for within effects
     is_epsilon <- FALSE
     if (has_within && correction != "none") {
          is_epsilon <- TRUE

          # Classify effects as within or between
          is_within_effect <- classify_afex_effects(effect_names, within_factors)

          # EPSILON IS DERIVED FROM THE DEGREES OF FREEDOM, NOT ASKED FOR.
          #
          # It used to come from `summary(afex_obj)$pval.adjustments`, and that
          # had two faults. (a) `summary()` builds the WHOLE adjustments table,
          # Greenhouse-Geisser AND Huynh-Feldt, so it warns "HF eps > 1 treated
          # as 1" even on the GG default -- a warning about a number this table
          # never shows. Measured: `anova(correction = "none")` and
          # `anova(correction = "GG")` are both SILENT; only the HF computation
          # warns. (b) It returns the HF estimator UNCAPPED -- 1.0744 for
          # `looks` on the `dating_wide` mixed design -- while afex has already
          # used 1 for the df and p-values, so the table printed an impossible
          # epsilon (it cannot exceed 1) next to df that did not incorporate it,
          # contradicting its own note.
          #
          # The ratio of corrected to uncorrected numerator df IS the multiplier
          # that was actually applied. Verified against `summary()` on every
          # row: identical for GG (0.9617, 0.9340, 0.7994), and 1.0000 where
          # `summary()` reports 1.0744 -- which is the value the df reflect.
          #
          # So this is not a cap bolted on: the value cannot exceed 1 because
          # afex has already capped the df it returns, and the epsilon shown is
          # consistent with the df shown BY CONSTRUCTION rather than by
          # coincidence -- which is exactly what the note promises the reader.
          df_uncorrected <- as.data.frame(anova(afex_obj, correction = "none"))

          epsilon_values <- rep(NA, nrow(df_internal))
          for (i in seq_along(effect_names)) {
               if (is_within_effect[i]) {
                    nm <- effect_names[i]
                    if (nm %in% rownames(df_uncorrected)) {
                         raw_df <- df_uncorrected[nm, "num Df"]
                         cor_df <- anova_df[nm, "num Df"]
                         epsilon_values[i] <- if (is.finite(raw_df) && raw_df > 0) {
                              cor_df / raw_df
                         } else 1.0
                    } else {
                         epsilon_values[i] <- 1.0
                    }
               }
          }

          df_internal$Epsilon <- epsilon_values
     }

     # Format table to strings
     table_out <- afex_table_to_string(df_internal, is_epsilon, has_within)

     # Replace .000 p-values with <.001
     p <- table_out$p
     idp0 <- p == ".000"
     p[idp0] <- "<.001"
     table_out$p <- p


     # Console output
     table_out_txt <- table_out
     table_out_names <- get_txt_column_names_anova(table_out_txt)
     # Rename MS to MSE for afex tables
     table_out_names[table_out_names == "MS"] <- "MSE"
     names(table_out_txt) <- table_out_names

     if (table_title == "") {
          table_title <- sprintf("ANOVA Results\n")
     }
     table_body <- table_out_txt


     # Console table note
     correction_text <- ""
     if (has_within) {
          if (correction == "GG") {
               correction_text <- "Epsilon = Greenhouse-Geisser multiplier for degrees of freedom,"
          } else if (correction == "HF") {
               correction_text <- "Epsilon = Huynh-Feldt multiplier for degrees of freedom,"
          } else {
               correction_text <- "p-values based on assumed sphericity."
          }
          table_note <- sprintf("Note. df_num = degrees of freedom numerator. df_den = degrees of freedom denominator. \n%s \np-values and degrees of freedom in the table incorporate this correction.\nMSE = mean square error. \nges = generalized eta-squared.\n", correction_text)
     } else {
          table_note <- sprintf("Note. df_num = degrees of freedom numerator. df_den = degrees of freedom denominator. \nMSE = mean square error. \nges = generalized eta-squared.\n")
     }

     tbl_console <- list(table_number = table_number,
                         table_title = table_title,
                         table_body = table_body,
                         table_note = table_note)

     latex_body <- table_body

     class(tbl_console) <- "apa_table"


     # Latex table note
     if (has_within) {
          if (correction == "GG") {
               correction_text_latex <- "Epsilon = Greenhouse-Geisser multiplier for degrees of freedom,"
          } else if (correction == "HF") {
               correction_text_latex <- "Epsilon = Huynh-Feldt multiplier for degrees of freedom,"
          } else {
               correction_text_latex <- "$p$-values based on assumed sphericity."
          }
          table_note_latex <- sprintf("\\\\textit{Note}. $df_{Num}$ = degrees of freedom numerator. $df_{Den}$ = degrees of freedom denominator. %s $p$-values and degrees of freedom in the table incorporate this correction. $MSE$ = mean square error. $\\\\eta_{g}^2$ = generalized eta-squared.", correction_text_latex)
     } else {
          table_note_latex <- sprintf("\\\\textit{Note}. $df_{Num}$ = degrees of freedom numerator. $df_{Den}$ = degrees of freedom denominator. $MSE$ = mean square error. $\\\\eta_{g}^2$ = generalized eta-squared.")
     }


     tbl_console$latex.body         <- latex_body
     tbl_console$latex.table.title  <- if (table_title_supplied)
          trimws(table.title) else "ANOVA Results"
     tbl_console$latex.table.note   <- table_note_latex

     tbl_console$landscape       <- apa_landscape_value(landscape)
     tbl_console$table.type      <- "afex"

     tbl_console$spec <- afex_build_spec(
       latex_body   = tbl_console$latex.body,
       table_title  = tbl_console$latex.table.title,
       table_number = table.number,
       landscape    = tbl_console$landscape,
       correction   = correction)

     tbl_console <- apa_set_table_number(tbl_console, table_number, table_number_explicit)

     if (make_file_flag == TRUE) {
          apa.save(filename = filename, tbl_console)
     }

     return(tbl_console)
}


afex_table_to_string <- function(df_internal, is_epsilon, has_within) {

     Predictor <- df_internal$Predictor
     Predictor <- gsub(":", " x ", Predictor)

     df_num     <- sprintf("%1.2f", df_internal$df_num)
     df_den     <- sprintf("%1.2f", df_internal$df_den)
     df_num_int <- sprintf("%g", df_internal$df_num)
     df_den_int <- sprintf("%g", df_internal$df_den)

     MSvalue <- sprintf("%1.2f", df_internal$MSvalue)
     Fvalue  <- sprintf("%1.2f", df_internal$Fvalue)

     p   <- sprintf("%1.3f", df_internal$pvalue)
     p   <- strip.leading.zero(p)

     ges <- sprintf("%1.2f", df_internal$ges)
     ges <- strip.leading.zero(ges)

     if (is_epsilon) {
          Epsilon <- ifelse(is.na(df_internal$Epsilon),
                            "",
                            sprintf("%1.2f", df_internal$Epsilon))
     }

     if (has_within) {
          if (is_epsilon) {
               table_out <- cbind(Predictor, df_num, df_den, Epsilon, MSvalue, Fvalue, p, ges)
          } else {
               table_out <- cbind(Predictor, df_num, df_den, MSvalue, Fvalue, p, ges)
          }
     } else {
          # Between-only: integer df
          table_out <- cbind(Predictor, df_num_int, df_den_int, MSvalue, Fvalue, p, ges)
     }

     table_out <- data.frame(table_out, stringsAsFactors = FALSE)
     table_out[table_out == "NA"] <- ""

     return(table_out)
}


classify_afex_effects <- function(effect_names, within_factors) {
     is_within <- logical(length(effect_names))
     for (i in seq_along(effect_names)) {
          terms <- unlist(strsplit(effect_names[i], ":"))
          is_within[i] <- any(terms %in% within_factors)
     }
     return(is_within)
}
