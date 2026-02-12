#' Creates an ANOVA table in APA style based on output of aov_ez command from afex package
#' @param afex.output Output object from afex::aov_ez() command (class afex_aov)
#' @param correction Type of sphericity correction: "none", "GG", or "HF" corresponding to none, Greenhouse-Geisser and Huynh-Feldt, respectively.
#' @param table.title String containing text for table title
#' @param filename (optional) Output filename document filename (must end in .rtf or .doc only)
#' @param table.number  Integer to use in table number output line
#' @return APA table object
#' @examples
#' if  (requireNamespace("afex", quietly = TRUE)){
#' if  (requireNamespace("apaTables", quietly = TRUE)){
#' if  (requireNamespace("tidyr", quietly = TRUE)){
#'
#'
#'#
#'# ** Example 1: Between Participant Predictors
#'#
#'
#' goggles <- apaTables::goggles
#'
#' goggles_results <- afex::aov_ez(id = "participant", dv = "attractiveness",
#'                                  data = goggles,
#'                                  between = c("gender", "alcohol"))
#'
#'
#' # Make APA table - save after all 3 examples
#' goggles_table <- apa.afex.table(afex.output = goggles_results)
#'
#' # Create a table for your PDF
#' # Include the lines below in your rmarkdown or Quarto document
#' apa.knit.table.for.pdf(table_object = goggles_table)
#'
#'
#' \donttest{
#' #
#' # ** Example 2: Within Participant Predictors
#' #
#'
#' drink_attitude_wide <- apaTables::drink_attitude_wide
#'
#'# Convert data from wide format to long format where one row represents one OBSERVATION.
#'# Wide format column names MUST represent levels of each variable separated by an underscore.
#'# See vignette for further details.
#'
#' drink_attitude_long <- tidyr::pivot_longer(drink_attitude_wide,
#'                               cols = beer_positive:water_neutral,
#'                              names_to = c("drink", "imagery"),
#'                              names_sep = "_",
#'                               values_to = "attitude")
#'
#'
#' drink_attitude_long$drink <- as.factor(drink_attitude_long$drink)
#' drink_attitude_long$imagery <- as.factor(drink_attitude_long$imagery)
#'
#'
#' drink_attitude_results <- afex::aov_ez(id = "participant", dv = "attitude",
#'                              data = drink_attitude_long,
#'                              within = c("drink", "imagery"))
#'
#'
#' # Make APA table - save after all 3 examples
#' drink_table <- apa.afex.table(afex.output = drink_attitude_results)
#'
#' # Create a table for your PDF
#' # Include the lines below in your rmarkdown or Quarto document
#' apa.knit.table.for.pdf(table_object = drink_table)
#'
#'
#'#
#'# ** Example 3: Between and Within Participant Predictors
#'#
#'
#' dating_wide <- apaTables::dating_wide
#'
#'# Convert data from wide format to long format where one row represents one OBSERVATION.
#'# Wide format column names MUST represent levels of each variable separated by an underscore.
#'# See vignette for further details.
#'
#'
#' dating_long <- tidyr::pivot_longer(dating_wide,
#'                               cols = attractive_high:ugly_none,
#'                              names_to = c("looks", "personality"),
#'                              names_sep = "_",
#'                               values_to = "date_rating")
#'#'
#' dating_long$looks <- as.factor(dating_long$looks)
#' dating_long$personality <- as.factor(dating_long$personality)
#'
#'
#' dating_results <- afex::aov_ez(id = "participant", dv = "date_rating",
#'                                 data = dating_long,
#'                                 between = "gender",
#'                                 within = c("looks", "personality"))
#'
#'
#' # Make APA table - save after all 3 examples
#' dating_table <- apa.afex.table(afex.output = dating_results)
#'
#' # Create a table for your PDF
#' # Include the lines below in your rmarkdown or Quarto document
#' apa.knit.table.for.pdf(table_object = dating_table)
#'
#'
#' #
#' # Saving all three tables
#' #
#' apa.save(filename = file.path(tempdir(), "tables_afex.doc"),
#'                 goggles_table,
#'                  drink_table,
#'                 dating_table)
#'
#' # delete demo file
#' unlink(file.path(tempdir(), "tables_afex.doc"))
#' }
#' }}}
#' @export
apa.afex.table <- function(afex.output, correction = "GG", table.title = "", filename, table.number = 0) {

     afex_obj <- afex.output
     table_title <- table.title
     table_number <- table.number

     if (!requireNamespace("afex", quietly = TRUE)) {
          stop("The 'afex' package is required. Install it with install.packages('afex').")
     }

     if (!inherits(afex_obj, "afex_aov")) {
          stop("Input must be an afex_aov object from afex::aov_ez().")
     }

     if (missing(filename)) {
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

          # Extract epsilon values
          pval_adj <- summary(afex_obj)$pval.adjustments

          if (correction == "GG") {
               eps_col <- "GG eps"
          } else {
               eps_col <- "HF eps"
          }

          # Get epsilon for within effects
          # Note: anova(afex_obj, correction=...) already returns corrected df and p-values,
          # so we do NOT multiply df by epsilon here — just extract epsilon for display.
          epsilon_values <- rep(NA, nrow(df_internal))
          for (i in seq_along(effect_names)) {
               if (is_within_effect[i]) {
                    if (!is.null(pval_adj) && effect_names[i] %in% rownames(pval_adj)) {
                         epsilon_values[i] <- pval_adj[effect_names[i], eps_col]
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


     # RTF table note
     correction_text <- ""
     if (has_within) {
          if (correction == "GG") {
               correction_text <- "Epsilon = Greenhouse-Geisser multiplier for degrees of freedom,"
               correction_text_latex <- "Epsilon = Greenhouse-Geisser multiplier for degrees of freedom,"
          } else if (correction == "HF") {
               correction_text <- "Epsilon = Huynh-Feldt multiplier for degrees of freedom,"
               correction_text_latex <- "Epsilon = Huynh-Feldt multiplier for degrees of freedom,"
          } else {
               correction_text <- "{\\i p}-values based on assumed sphericity."
               correction_text_latex <- "$p$-values based on assumed sphericity."
          }
          table_note <- sprintf("{\\i df\\sub Num\\nosupersub} = degrees of freedom numerator. {\\i df\\sub Den\\nosupersub} = degrees of freedom denominator. %s \n{\\i p}-values and degrees of freedom in the table incorporate this correction. {\\i MSE} = mean square error. {\\u0951\\ \\super 2\\nosupersub \\sub g\\nosupersub} = generalized eta-squared.\n", correction_text)
          table_note_latex <- sprintf("\\\\textit{Note}. $df_{Num}$ = degrees of freedom numerator. $df_{Den}$ = degrees of freedom denominator. %s $p$-values and degrees of freedom in the table incorporate this correction. $MSE$ = mean square error. $\\\\eta_{g}^2$ = generalized eta-squared.", correction_text_latex)
     } else {
          table_note <- sprintf("{\\i df\\sub Num\\nosupersub} = degrees of freedom numerator. {\\i df\\sub Den\\nosupersub} = degrees of freedom denominator. {\\i MSE} = mean square error. {\\u0951\\ \\super 2\\nosupersub \\sub g\\nosupersub} = generalized eta-squared.\n")
          table_note_latex <- sprintf("\\\\textit{Note}. $df_{Num}$ = degrees of freedom numerator. $df_{Den}$ = degrees of freedom denominator. $MSE$ = mean square error. $\\\\eta_{g}^2$ = generalized eta-squared.")
     }


     # RTF table
     colwidths <- get_rtf_column_widths_anova(table_out)

     anova_matrix <- as.matrix(table_out)
     new_col_names <- get_rtf_column_names_anova(table_out)
     # Rename MS to MSE for afex tables in RTF
     new_col_names[new_col_names == "MS"] <- "{\\i MSE}"
     colnames(anova_matrix) <- new_col_names


     if (!has_within) {
          # Between-only design: Predictor, df_num_int, df_den_int, MSvalue, Fvalue, p, ges
          rtfTable <- RtfTable$new(isHeaderRow=TRUE, defaultDecimalTableProportionInternal=.15)
          rtfTable$setTableContent(anova_matrix)
          rtfTable$setCellWidthsInches(colwidths)
          rtfTable$setRowDecimalTabForColumn(0,2)   #df_num_int
          rtfTable$setRowDecimalTabForColumn(0,3)   #df_den_int
          rtfTable$setRowDecimalTabForColumn(.6,4)  #MSvalue
          rtfTable$setRowDecimalTabForColumn(.6,5)  #F
          rtfTable$setRowDecimalTabForColumn(.1,6)  #pvalue
          rtfTable$setRowDecimalTabForColumn(.2,7)  #ges
     } else {
          if (is_epsilon) {
               # Within or mixed with epsilon: Predictor, df_num, df_den, Epsilon, MSvalue, Fvalue, p, ges
               rtfTable <- RtfTable$new(isHeaderRow=TRUE, defaultDecimalTableProportionInternal=.15)
               rtfTable$setTableContent(anova_matrix)
               rtfTable$setCellWidthsInches(colwidths)
               rtfTable$setRowDecimalTabForColumn(.3,2)  #df_num
               rtfTable$setRowDecimalTabForColumn(.4,3)  #df_den
               rtfTable$setRowDecimalTabForColumn(.2,4)  #Epsilon
               rtfTable$setRowDecimalTabForColumn(.6,5)  #MSvalue
               rtfTable$setRowDecimalTabForColumn(.6,6)  #F
               rtfTable$setRowDecimalTabForColumn(.1,7)  #pvalue
               rtfTable$setRowDecimalTabForColumn(.2,8)  #ges
          } else {
               # Within or mixed without epsilon (correction = "none")
               # Predictor, df_num, df_den, MSvalue, Fvalue, p, ges
               rtfTable <- RtfTable$new(isHeaderRow=TRUE, defaultDecimalTableProportionInternal=.15)
               rtfTable$setTableContent(anova_matrix)
               rtfTable$setCellWidthsInches(colwidths)
               rtfTable$setRowDecimalTabForColumn(.3,2)  #df_num
               rtfTable$setRowDecimalTabForColumn(.4,3)  #df_den
               rtfTable$setRowDecimalTabForColumn(.6,4)  #MSvalue
               rtfTable$setRowDecimalTabForColumn(.6,5)  #F
               rtfTable$setRowDecimalTabForColumn(.1,6)  #pvalue
               rtfTable$setRowDecimalTabForColumn(.2,7)  #ges
          }
     }


     # Create RTF code
     txt_body <- rtfTable$getTableAsRTF(FALSE, FALSE)

     if (make_file_flag == TRUE) {
          write.rtf.table(filename = filename, txt.body = txt_body, table.title = table_title, table.note = table_note, landscape = TRUE, table.number = table_number)
     }

     tbl_console$rtf.body         <- txt_body
     tbl_console$rtf.table.title  <- table_title
     tbl_console$rtf.table.note   <- table_note

     tbl_console$latex.body         <- latex_body
     tbl_console$latex.table.title  <- "ANOVA Results"
     tbl_console$latex.table.note   <- table_note_latex

     tbl_console$landscape       <- TRUE
     tbl_console$table.type      <- "afex"

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
