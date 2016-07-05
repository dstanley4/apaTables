#' Creates a ANOVA F table in APA style
#' @param ... Regression (i.e., lm) result objects. Typically, one for each block in the regression.
#' @param filename (optional) Output filename document filename (must end in .rtf or .doc only)
#' @param table.number  Integer to use in table number output line
#' @param conf.level Level of confidence for interval around partial eta-squared (.90 or .95). A value of .90 is the default, this helps to create consistency between the CI overlapping with zero and conclusions based on the p-value.
#' @param type  Sum of Squares Type. Type II or Type III; specify, 2 or 3, respectively. Default value is 3.
#' @return APA table object
#' @examples
#' # Example from Fidler & Thompson (2001)
#' # You must set these contrasts to ensure values match SPSS
#' options(contrasts = c("contr.sum", "contr.poly"))
#' lm_output <- lm(dv ~ a*b, data=fidler_thompson)
#' apa.anova.table(lm_output)
#'
#' #Example for Field et al. (2012) Discovery Statistics Using R
#' # You must set these contrasts to ensure values match SPSS
#' options(contrasts = c("contr.sum", "contr.poly"))
#' lm_output <- lm(attractiveness ~ gender*alcohol, data=goggles)
#' apa.anova.table(lm_output)
#' @export
apa.anova.table<-function(lm_output,filename,table.number=NA, conf.level=.90,type=3) {
     table_number <- table.number
     conf_level <- conf.level

     if ((conf_level!=.90)&(conf_level!=.95)) {conf_level=.90}

     if (missing(filename)) {
          make_file_flag=FALSE
     } else {
          make_file_flag=TRUE
     }

     # Add a check using model df to see if variable was factor in lm
     # If not, abort



     dv_name <- names(lm_output$model)[1]
     table_values <- car::Anova(lm_output,type=type)
     table_values <- broom::tidy(table_values)
     names(table_values) <- c("Predictor","SS", "df","Fvalue","pvalue")

     table_out <- table_values
     table_out$SS     <- round(table_out$SS,2)
     table_out$Fvalue <- round(table_out$Fvalue,2)
     table_out$pvalue <- round(table_out$pvalue,3)

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
     Predictor <- sub(":"," x ", Predictor)
     SSvalue   <- sprintf("%1.2f",table_out$SS)
     MSvalue   <- sprintf("%1.2f",MSvalue)
     Fvalue    <- sprintf("%1.2f",table_out$Fvalue)
     df        <- table_out$df
     p         <- sprintf("%1.3f",table_out$pvalue)
     p         <- strip.leading.zero(p)

     partial_eta_sq    <- strip.leading.zero(sprintf("%1.2f",partial_eta_sq))
     partial_eta_sq_LL <- strip.leading.zero(sprintf("%1.2f",LL_partial_eta_sq))
     partial_eta_sq_UL <- strip.leading.zero(sprintf("%1.2f",UL_partial_eta_sq))

     partial_eta_sq_CI <- rep("", num_rows)
     for (i in 2:(num_rows-1)) {
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

     #console table
     table_title <- sprintf("ANOVA results using %s as the criterion\n",dv_name)
     table_body  <- table_out_txt
     table_note  <- ""

     tbl_console <- list(table_number = table_number,
                         table_title = table_title,
                         table_body = table_body,
                         table_note = table_note)

     class(tbl_console) <- "apa_table"



     if (make_file_flag==TRUE) {
          table_title <- sprintf("Fixed-Effects ANOVA results using %s as the criterion\n",dv_name)
          table_note <- "Here is the note."

          #set columns widths and names
          colwidths <- get_rtf_column_widths_anova(table_out)

          anova_table <- as.matrix(table_out)
          new_col_names  <- get_rtf_column_names_anova(table_out)
          if (conf_level==.95) {
               new_col_names <- sub("xyzzy","95",new_col_names)
          } else {
               new_col_names <- sub("xyzzy","90",new_col_names)
          }
          colnames(anova_table) <- new_col_names


          #Create RTF code
          rtfTable <- RtfTable$new(isHeaderRow=TRUE, defaultDecimalTableProportionInternal=.15)
          rtfTable$setTableContent(anova_table)
          rtfTable$setCellWidthsInches(colwidths)
          rtfTable$setRowSecondColumnDecimalTab(.4)
          txt_body <- rtfTable$getTableAsRTF(FALSE,FALSE)


          write.rtf.table(filename = filename,txt.body = txt_body,table.title = table_title, table.note = table_note,landscape=FALSE,table.number=table_number)

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
            partial_eta_sq_CI ="CI_partial_eta2")
}

get_txt_column_names_anova <- function(df) {
     n <- names(df)
     names_out <- c()
     for (i in 1:length(n)) {
          names_out[i] <-output_txt_name_anova(n[i])
     }
     return(names_out)
}

output_rtf_name_anova <- function(column_name) {
     switch(column_name,
            Predictor="Predictor",
            SSvalue = "{Sum\\par}{of\\par}Squares",
            MSvalue = "{Mean\\par}Square",
            df = "{\\i df}",
            Fvalue = "{\\i F}",
            p = "{\\i p}",
            partial_eta_sq ="{\\sub partial \\nosupersub \\u0951\\ \\super 2\\nosupersub}",
            partial_eta_sq_CI ="{\\sub partial \\nosupersub \\u0951\\ \\super 2 \\nosupersub \\par xyzzy% CI\\par[LL, UL]}")

}


get_rtf_column_names_anova <- function(df) {
     n <- names(df)
     names_out <- c()
     for (i in 1:length(n)) {
          names_out[i] <-output_rtf_name_anova(n[i])
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
            df        = narrow,
            Fvalue    = narrow,
            p         = narrow,
            partial_eta_sq    = wide,
            partial_eta_sq_CI = wide)
}

get_rtf_column_widths_anova <- function(df) {
     n <- names(df)
     width_out <- c()
     for (i in 1:length(n)) {
          width_out[i] <-output_column_width_anova(n[i])
     }
     return(width_out)
}

get_anova_table_note_txt <- function(calculate_cor,calculate_beta) {
          table_note <- "Note. * indicates p < .05; ** indicates p < .01.\nA significant b-weight indicates the beta-weight and semi-partial correlation are also significant.\nb represents unstandardized regression weights; beta indicates the standardized regression weights; \nsr2 represents the semi-partial correlation squared; r represents the zero-order correlation.\nSquare brackets are used to enclose the lower and upper limits of a confidence interval.\n"
     return(table_note)
}



convert_colon_to_x <- function(predictor_strings) {
     L <- length(predictor_strings)
     for (i in 1:L) {
          cur_string <- predictor_strings
     }

}
