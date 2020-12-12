#' Creates an ANOVA table in APA style based output of ezANOVA command from ez package
#' @param ez.output Output object from ezANOVA command from ez package
#' @param correction Type of sphercity correction: "none", "GG", or "HF" corresponding to none, Greenhouse-Geisser and Huynh-Feldt, respectively.
#' @param table.title String containing text for table title
#' @param filename (optional) Output filename document filename (must end in .rtf or .doc only)
#' @param table.number  Integer to use in table number output line
#' @return APA table object
#' @examples
#' \dontrun{
#'# ** Example 1: Between Participant Predictors
#'#
#'
#'library(apaTables)
#'library(ez)
#'
#'# See format where one row represents one PERSON
#'# Note that participant, gender, and alcohol are factors
#'
#'print(goggles)
#'
#'
#'# Use ezANOVA
#'# Be sure use the options command, as below, to ensure sufficient digits
#'
#'options(digits = 10)
#'goggles_results <- ezANOVA(data = goggles,
#'                           dv = attractiveness,
#'                           between = .(gender, alcohol),
#'                           participant ,
#'                           detailed = TRUE)
#'
#'
#' # Make APA table
#'
#'goggles_table <- apa.ezANOVA.table(goggles_results,
#'                                   filename="ex1_ez_independent.doc")
#'
#'print(goggles_table)
#'
#'
#'
#' #
#' # ** Example 2: Within Participant Predictors
#' #
#'
#'library(apaTables)
#'library(tidyr)
#'library(forcats)
#'library(ez)
#'
#'# See initial wide format where one row represents one PERSON
#'print(drink_attitude_wide)
#'
#'# Convert data from wide format to long format where one row represents one OBSERVATION.
#'# Wide format column names MUST represent levels of each variable separated by an underscore.
#'# See vignette for further details.
#'
#'drink_attitude_long <- gather(data = drink_attitude_wide,
#'                               key = cell, value = attitude,
#'                               beer_positive:water_neutral,
#'                               factor_key=TRUE)
#'
#'drink_attitude_long <- separate(data = drink_attitude_long,
#'                                 col = cell, into = c("drink","imagery"),
#'                                 sep = "_", remove = TRUE)
#'
#'drink_attitude_long$drink <- as_factor(drink_attitude_long$drink)
#'drink_attitude_long$imagery <- as_factor(drink_attitude_long$imagery)
#'
#'# See new long format of data, where one row is one OBSERVATION.
#'# As well, notice that we have two columns (drink, imagery)
#'# drink, imagery, and participant are factors

#'print(drink_attitude_long)
#'
#'
#'# Set contrasts to match Field et al. (2012) textbook output
#'
#'alcohol_vs_water <- c(1, 1, -2)
#'beer_vs_wine <- c(-1, 1, 0)
#'negative_vs_other <- c(1, -2, 1)
#'positive_vs_neutral <- c(-1, 0, 1)
#'contrasts(drink_attitude_long$drink) <- cbind(alcohol_vs_water, beer_vs_wine)
#'contrasts(drink_attitude_long$imagery) <- cbind(negative_vs_other, positive_vs_neutral)
#'
#'
#'# Use ezANOVA
#'# Be sure use the options command, as below, to ensure sufficient digits
#'
#'options(digits = 10)
#'drink_attitude_results <- ezANOVA(data = drink_attitude_long,
#'                    dv = .(attitude), wid = .(participant),
#'                    within = .(drink, imagery),
#'                    type = 3, detailed = TRUE)
#'
#'
#'# Make APA table
#'
#'drink_table <- apa.ezANOVA.table(drink_attitude_results,
#'                                  filename="ex2_repeated_table.doc")
#'
#'print(drink_table)
#'
#'
#'#
#'# ** Example 3: Between and Within Participant Predictors
#'#
#'
#'library(apaTables)
#'library(tidyr)
#'library(forcats)
#'library(ez)
#'
#'# See initial wide format where one row represents one PERSON
#'print(dating_wide)
#'
#'
#'# Convert data from wide format to long format where one row represents one OBSERVATION.
#'# Wide format column names MUST represent levels of each variable separated by an underscore.
#'# See vignette for further details.
#'
#'dating_long <- gather(data = dating_wide,
#'                      key = cell, value = date_rating,
#'                      attractive_high:ugly_none,
#'                      factor_key = TRUE)
#'
#'dating_long <- separate(data = dating_long,
#'                        col = cell, into = c("looks","personality"),
#'                        sep = "_", remove = TRUE)
#'
#'dating_long$looks <- as_factor(dating_long$looks)
#'dating_long$personality <- as_factor(dating_long$personality)
#'
#'
#'# See new long format of data, where one row is one OBSERVATION.
#'# As well, notice that we have two columns (looks, personality)
#'# looks, personality, and participant are factors
#'
#'print(dating_long)
#'
#'# Set contrasts to match Field et al. (2012) textbook output
#'
#'some_vs_none <- c(1, 1, -2)
#'hi_vs_av <- c(1, -1, 0)
#'attractive_vs_ugly <- c(1, 1, -2)
#'attractive_vs_average <- c(1, -1, 0)
#'contrasts(dating_long$personality) <- cbind(some_vs_none, hi_vs_av)
#'contrasts(dating_long$looks) <- cbind(attractive_vs_ugly, attractive_vs_average)
#'
#'
#' # Use ezANOVA
#'
#'library(ez)
#'options(digits = 10)
#'dating_results <-ezANOVA(data = dating_long, dv = .(date_rating), wid = .(participant),
#'                         between = .(gender), within = .(looks, personality),
#'                         type = 3, detailed = TRUE)
#'
#'
#'# Make APA table
#'
#'dating_table <- apa.ezANOVA.table(dating_results,
#'                                  filename = "ex3_mixed_table.doc")
#'print(dating_table)
#'}
#' @export
apa.ezANOVA.table<-function(ez.output, correction = "GG", table.title = "", filename, table.number=NA) {

     ez_output <- ez.output

     # make sure gg and hf both work
     table_title <- table.title
     table_number <- table.number

     if (missing(filename)) {
          make_file_flag <- FALSE
     } else {
          make_file_flag <- TRUE
     }

     #include check so that no errors if used with between subjects

     all_effects <- ez_output$ANOVA

     is_within <- FALSE
     if (is.null(ez_output$`Sphericity Corrections`) == FALSE) {
          is_within <- TRUE
     }



     if (is_within == FALSE) {
          # Between Subject Design
          between_subject_effects <- all_effects

          ez_detailed <- FALSE
          if (any(names(between_subject_effects)=="SSn")) {
               ez_detailed <- TRUE
          }

          if (ez_detailed == TRUE) {
               between <- select(between_subject_effects, Effect, DFn, DFd, SSn, SSd, F, p, ges)
               names(between) <- c("Predictor","df_num", "df_den", "SS_num","SS_den", "Fvalue","pvalue", "ges")
          } else {
               between <- select(between_subject_effects, Effect, DFn, DFd, F, p, ges)
               names(between) <- c("Predictor","df_num", "df_den", "Fvalue","pvalue", "ges")
          }

          between <- ez_table_to_string(between, ez_detailed)

          all_tables <- list()
          all_tables$between <- between

          table_out <- between

     } else {
          # Within Subject or Mixed Design
          within_effect_corrections <- ez_output$'Sphericity Corrections'
          full_table <- dplyr::full_join(all_effects, within_effect_corrections, by = "Effect")
          between_subject_effects <- dplyr::filter(full_table, is.na(GGe))
          within_subject_effects  <- dplyr::filter(full_table, !is.na(GGe))

          is_epsilon <- TRUE
          if (correction=="GG") {
               within_subject_effects <- dplyr::mutate(within_subject_effects, DFn=DFn*GGe)
               within_subject_effects <- dplyr::mutate(within_subject_effects, DFd=DFd*GGe)
               within_subject_effects <- dplyr::mutate(within_subject_effects, Epsilon = GGe)
               within_subject_effects$p <- within_subject_effects$"p[GG]"
          } else if (correction=="HF") {
               within_subject_effects <- dplyr::mutate(within_subject_effects, DFn=DFn*HFe)
               within_subject_effects <- dplyr::mutate(within_subject_effects, DFd=DFd*HFe)
               within_subject_effects <- dplyr::mutate(within_subject_effects, Epsilon = HFe)
               within_subject_effects$p <- within_subject_effects$"p[HF]"
          } else {
               is_epsilon <- FALSE
               # none and other strings
          }


          ez_detailed <- FALSE
          if (any(names(between_subject_effects)=="SSn")) {
               ez_detailed <- TRUE
          }

          if (ez_detailed == TRUE) {
               if (is_epsilon == TRUE) {
                    between_subject_effects$Epsilon = ""
                    between <- select(between_subject_effects, Effect, DFn, DFd, Epsilon, SSn, SSd, F, p, ges)
                    within  <- select(within_subject_effects , Effect, DFn, DFd, Epsilon, SSn, SSd, F, p, ges)
                    names(between) <- c("Predictor","df_num", "df_den", "Epsilon", "SS_num","SS_den", "Fvalue","pvalue", "ges")
                    names(within)  <- c("Predictor","df_num", "df_den", "Epsilon", "SS_num","SS_den", "Fvalue","pvalue", "ges")
               } else {
                    between_subject_effects$Epsilon = ""
                    between <- select(between_subject_effects, Effect, DFn, DFd, SSn, SSd, F, p, ges)
                    within  <- select(within_subject_effects , Effect, DFn, DFd, SSn, SSd, F, p, ges)
                    names(between) <- c("Predictor","df_num", "df_den", "SS_num","SS_den", "Fvalue","pvalue", "ges")
                    names(within)  <- c("Predictor","df_num", "df_den", "SS_num","SS_den", "Fvalue","pvalue", "ges")
               }
          } else {
               if (is_epsilon == TRUE) {
                    if (dim(between_subject_effects)[1]>0) {
                         between_subject_effects$Epsilon = ""
                    } else {
                         Epsilon <- data.frame(Epsilon=numeric())
                         between_subject_effects <- cbind(between_subject_effects, Epsilon)
                    }
                    between <- select(between_subject_effects, Effect, DFn, DFd, Epsilon, F, p, ges)
                    within  <- select(within_subject_effects , Effect, DFn, DFd, Epsilon, F, p, ges)
                    names(between) <- c("Predictor","df_num", "df_den", "Epsilon", "Fvalue","pvalue", "ges")
                    names(within)  <- c("Predictor","df_num", "df_den", "Epsilon", "Fvalue","pvalue", "ges")
               } else {
                    between <- select(between_subject_effects, Effect, DFn, DFd, Epsilon, F, p, ges)
                    within  <- select(within_subject_effects , Effect, DFn, DFd, Epsilon, F, p, ges)
                    names(between) <- c("Predictor","df_num", "df_den", "Fvalue","pvalue", "ges")
                    names(within)  <- c("Predictor","df_num", "df_den", "Fvalue","pvalue", "ges")
               }
          }


          if (dim(between)[1]>0) {
               between <- ez_table_to_string(between, ez_detailed)
               within  <- ez_table_to_string(within, ez_detailed)
               table_out <- rbind(between, within)
          } else {
               table_out <- ez_table_to_string(within, ez_detailed)
          }


          all_tables <- list()
          all_tables$full    <- table_out
          all_tables$between <- between
          all_tables$within  <- within
     }

     # within.options("sa (sphericity assumed)", "gg", "hf")
     # provide indication of what was done in table note "For all within suject effects, a XX correction"...

     table_out_txt        <- table_out
     table_out_names      <- get_txt_column_names_anova(table_out_txt)
     names(table_out_txt) <- table_out_names

     #console table
     if (table_title=="") {
          table_title <- sprintf("ANOVA results\n")
     }
     table_body  <- table_out_txt


     correction_text <- ""
     if (is_within == TRUE) {
          if (correction=="GG") {
               correction_text <- "Epsilon indicates Greenhouse-Geisser multiplier for degrees of freedom,"

          } else if (correction=="HF") {
               correction_text <- "Epsilon indicates Huynh-Feldt multiplier for degrees of freedom,"
          } else {
               correction_text <- "p-values based on assumed sphericity."
          }
          if (ez_detailed==TRUE) {
               table_note  <- sprintf("Note. df_num indicates degrees of freedom numerator. df_den indicates degrees of freedom denominator. \n%s \np-values and degrees of freedom in the table incorporate this correction.\nSS_num indicates sum of squares numerator. SS_den indicates sum of squares denominator. \nges indicates generalized eta-squared.\n", correction_text)
          } else {
               table_note  <- sprintf("Note. df_num indicates degrees of freedom numerator. df_den indicates degrees of freedom denominator. \n%s \np-values and degrees of freedom in the table incorporate this correction.\nges indicates generalized eta-squared.\n", correction_text)
          }
     } else {
          if (ez_detailed==TRUE) {
               table_note  <- sprintf("Note. df_num indicates degrees of freedom numerator. df_den indicates degrees of freedom denominator. \nSS_num indicates sum of squares numerator. SS_den indicates sum of squares denominator. \nges indicates generalized eta-squared.\n")
          } else {
               table_note  <- sprintf("Note. df_num indicates degrees of freedom numerator. df_den indicates degrees of freedom denominator. \nges indicates generalized eta-squared.\n")
          }
     }


     tbl_console <- list(table_number = table_number,
                         table_title = table_title,
                         table_body = table_body,
                         table_note = table_note)

     class(tbl_console) <- "apa_table"



     if (make_file_flag==TRUE) {
          correction_text <- ""
          if (is_within == TRUE) {
               if (correction=="GG") {
                    correction_text <- "Epsilon indicates Greenhouse-Geisser multiplier for degrees of freedom,"

               } else if (correction=="HF") {
                    correction_text <- "Epsilon indicates Huynh-Feldt multiplier for degrees of freedom,"
               } else {
                    correction_text <- "{\\i p}-values based on assumed sphericity."
               }
               if (ez_detailed==TRUE) {
                    table_note  <- sprintf("{\\i df\\sub Num\\nosupersub} indicates degrees of freedom numerator. {\\i df\\sub Den\\nosupersub} indicates degrees of freedom denominator. %s \n{\\i p}-values and degrees of freedom in the table incorporate this correction. {\\i SS\\sub Num\\nosupersub} indicates sum of squares numerator. {\\i SS\\sub Den\\nosupersub} indicates sum of squares denominator. {\\u0951\\ \\super 2\\nosupersub \\sub g\\nosupersub} indicates generalized eta-squared.\n", correction_text)
               } else {
                    table_note  <- sprintf("{\\i df\\sub Num\\nosupersub} indicates degrees of freedom numerator. {\\i df\\sub Den\\nosupersub} indicates degrees of freedom denominator. %s \n{\\i p}-values and degrees of freedom in the table incorporate this correction. {\\u0951\\ \\super 2\\nosupersub \\sub g\\nosupersub} indicates generalized eta-squared.\n", correction_text)
               }

          } else {
               if (ez_detailed==TRUE) {
                    table_note  <- sprintf("{\\i df\\sub Num\\nosupersub} indicates degrees of freedom numerator. {\\i df\\sub Den\\nosupersub} indicates degrees of freedom denominator. {\\i SS\\sub Num\\nosupersub} indicates sum of squares numerator. {\\i SS\\sub Den\\nosupersub} indicates sum of squares denominator. {\\u0951\\ \\super 2\\nosupersub \\sub g\\nosupersub} indicates generalized eta-squared.\n")
               } else {
                    table_note  <- sprintf("{\\i df\\sub Num\\nosupersub} indicates degrees of freedom numerator. {\\i df\\sub Den\\nosupersub} indicates degrees of freedom denominator. {\\u0951\\ \\super 2\\nosupersub \\sub g\\nosupersub} indicates generalized eta-squared.\n")
               }
          }

          #set columns widths and names
          colwidths <- get_rtf_column_widths_anova(table_out)

          anova_table <- as.matrix(table_out)
          new_col_names  <- get_rtf_column_names_anova(table_out)
          colnames(anova_table) <- new_col_names


          if (is_within == FALSE) {
               rtfTable <- RtfTable$new(isHeaderRow=TRUE, defaultDecimalTableProportionInternal=.15)
               rtfTable$setTableContent(anova_table)
               rtfTable$setCellWidthsInches(colwidths)
               rtfTable$setRowDecimalTabForColumn(0,2) #df1
               rtfTable$setRowDecimalTabForColumn(0,3)  #df2
               rtfTable$setRowDecimalTabForColumn(.6,4) #F
               rtfTable$setRowDecimalTabForColumn(.1,5) #pvalue
               rtfTable$setRowDecimalTabForColumn(.2,6) #ges
          } else {
               if (ez_detailed == FALSE) {
                    rtfTable <- RtfTable$new(isHeaderRow=TRUE, defaultDecimalTableProportionInternal=.15)
                    rtfTable$setTableContent(anova_table)
                    rtfTable$setCellWidthsInches(colwidths)
                    rtfTable$setRowDecimalTabForColumn(.3,2) #df 1
                    rtfTable$setRowDecimalTabForColumn(.4,3)  #df  2
                    rtfTable$setRowDecimalTabForColumn(.2,4) #Epsilon
                    rtfTable$setRowDecimalTabForColumn(.6,5) #F
                    rtfTable$setRowDecimalTabForColumn(.1,6)  #pvalue
                    rtfTable$setRowDecimalTabForColumn(.2,7) #ges
               } else {
                    rtfTable <- RtfTable$new(isHeaderRow=TRUE, defaultDecimalTableProportionInternal=.15)
                    rtfTable$setTableContent(anova_table)
                    rtfTable$setCellWidthsInches(colwidths)
                    rtfTable$setRowDecimalTabForColumn(.3,2) #df1
                    rtfTable$setRowDecimalTabForColumn(.4,3) #df2
                    rtfTable$setRowDecimalTabForColumn(.3,4) #Epsilon
                    rtfTable$setRowDecimalTabForColumn(.6,5) #SS1
                    rtfTable$setRowDecimalTabForColumn(.6,6)  #SS2
                    rtfTable$setRowDecimalTabForColumn(.6,7)  #F
                    rtfTable$setRowDecimalTabForColumn(.1,8)  #pvalue
                    rtfTable$setRowDecimalTabForColumn(.2,9) #ges
               }
          }


          #Create RTF code
          txt_body <- rtfTable$getTableAsRTF(FALSE,FALSE)


          write.rtf.table(filename = filename,txt.body = txt_body,table.title = table_title, table.note = table_note,landscape=TRUE,table.number=table_number)

     }

     return(tbl_console)
}


add_top_blank_line <- function(df) {
     nrnc <- dim(df)
     n_cols <- nrnc[2]


     new_line <- df[1,]
     for (i in 1:n_cols) {
          new_line[1,i] <- ""
     }
     output <- rbind(new_line,df)
     return(output)
}


ez_table_to_string <- function(table_out, ez_detailed) {

     is_epsilon <- any(names(table_out)=="Epsilon")

     Predictor <- table_out$Predictor
     Predictor <- gsub(":"," x ", Predictor)
     df_num   <- sprintf("%1.2f",table_out$df_num)
     df_den   <- sprintf("%1.2f",table_out$df_den)
     df_num_int   <- sprintf("%g",table_out$df_num)
     df_den_int   <- sprintf("%g",table_out$df_den)

     if (is.numeric(table_out$Epsilon)) {
          Epsilon   <- sprintf("%1.2f",table_out$Epsilon)
     } else {
          Epsilon <- table_out$Epsilon
     }
     Fvalue    <- sprintf("%1.2f",table_out$Fvalue)
     p         <- sprintf("%1.3f",table_out$pvalue)
     p         <- strip.leading.zero(p)
     ges  <- sprintf("%1.2f",table_out$ges)
     ges  <- strip.leading.zero(ges)
     if (ez_detailed == TRUE) {
          SS_num   <- sprintf("%1.2f",table_out$SS_num)
          SS_den   <- sprintf("%1.2f",table_out$SS_den)
     }

     if (ez_detailed == TRUE) {
          if (is_epsilon==TRUE) {
               table_out <- cbind(Predictor,df_num, df_den, Epsilon, SS_num, SS_den,Fvalue,p,ges)
          } else {
               table_out <- cbind(Predictor,df_num_int, df_den_int, SS_num, SS_den,Fvalue,p,ges)
          }
     } else {
          if (is_epsilon==TRUE) {
               table_out <- cbind(Predictor,df_num, df_den, Epsilon, Fvalue,p,ges)
          } else {
               table_out <- cbind(Predictor,df_num_int, df_den_int, Fvalue,p,ges)
          }
     }

     table_out <- data.frame(table_out,stringsAsFactors = FALSE)
     table_out[table_out=="NA"] <- ""

     return(table_out)
}
