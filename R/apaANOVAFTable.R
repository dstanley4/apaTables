#' Creates a ANOVA F table in APA style
#' @param ... Regression (i.e., lm) result objects. Typically, one for each block in the regression.
#' @param filename (optional) Output filename document filename (must end in .rtf or .doc only)
#' @param table.number  Integer to use in table number output line
#' @param conf.level Level of confidence for interval around partial eta-squared (0 to 1). A value of .90 is the default, this helps to create consistency between the CI overlapping with zero and conclusions based on the p-value.
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
     SSvalue <- sprintf("%1.2f",table_out$SS)
     MSvalue <- sprintf("%1.2f",MSvalue)
     Fvalue <- sprintf("%1.2f",table_out$Fvalue)
     df <- table_out$df
     p <- sprintf("%1.2f",table_out$pvalue)
     partial_eta_sq <- sprintf("%1.2f",partial_eta_sq)
     LL_partial_eta_sq <- sprintf("%1.2f",LL_partial_eta_sq)
     UL_partial_eta_sq <- sprintf("%1.2f",UL_partial_eta_sq)

     table_out <- cbind(Predictor,SSvalue,df,MSvalue,Fvalue,p,partial_eta_sq, LL_partial_eta_sq, UL_partial_eta_sq)
     table_out <- data.frame(table_out,stringsAsFactors = FALSE)
     table_out[table_out=="NA"] <- ""

     #console table
     table_title <- sprintf("ANOVA results using %s as the criterion\n",dv_name)
     table_body <- table_out
     table_note <- ""

     tbl_console <- list(table_number = table_number,
                         table_title = table_title,
                         table_body = table_body,
                         table_note = table_note)

     class(tbl_console) <- "apa_table"



     if (make_file_flag==TRUE) {
          table_title <- sprintf("Regression results using %s as the criterion\n",first_criterion)
          table_note <- get_reg_table_note_rtf(first_block_calculate_cor, first_block_calculate_beta)

          #set columns widths and names
          colwidths <- get_rtf_column_widths(block_out_rtf)

          regression_table <- as.matrix(block_out_rtf)
          new_col_names <- get_rtf_column_names(block_out_rtf)
          colnames(regression_table) <- new_col_names


          #Create RTF code
          rtfTable <- RtfTable$new(isHeaderRow=TRUE, defaultDecimalTableProportionInternal=.15)
          rtfTable$setTableContent(regression_table)
          rtfTable$setCellWidthsInches(colwidths)
          rtfTable$setRowSecondColumnDecimalTab(.4)
          txt_body <- rtfTable$getTableAsRTF(FALSE,FALSE)


          if (is_multiple_blocks==TRUE) {
               write.rtf.table(filename = filename,txt.body = txt_body,table.title = table_title, table.note = table_note,landscape=TRUE,table.number=table_number)
          } else {
               write.rtf.table(filename = filename,txt.body = txt_body,table.title = table_title, table.note = table_note,table.number=table_number)
          }

     }


     return(tbl_console)
}




is_product_row <- function(row_names) {
     is_a_colon   <- grep(":",row_names)
     is_a_star    <- grep("\\*",row_names)
     is_a_product <- unique(sort(c(is_a_colon,is_a_star)))
     return(is_a_product)
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
            sr2="sr2",
            sr2_CI ="sr2_95%_CI",
            r="r",
            summary="Fit",
            difference="Difference")
}

get_txt_column_names <- function(df) {
     n <- names(df)
     names_out <- c()
     for (i in 1:length(n)) {
          names_out[i] <-output_txt_name(n[i])
     }
     return(names_out)
}

output_rtf_name <- function(column_name) {
     switch(column_name,
            predictor="Predictor",
            b = "{\\i b}",
            b_CI = "{{\\i b}\\par}{95% CI\\par}[LL, UL]",
            beta = "{\\i beta}",
            beta_CI ="{{\\i beta}\\par}{95% CI\\par}[LL, UL]",
            sr2="{\\i sr\\super 2 \\nosupersub}",
            sr2_CI ="{{{\\i sr\\super 2 \\nosupersub}\\par}95% CI\\par}[LL, UL]",
            r="{\\i r}",
            summary="Fit",
            difference="Difference")
}


get_rtf_column_names <- function(df) {
     n <- names(df)
     names_out <- c()
     for (i in 1:length(n)) {
          names_out[i] <-output_rtf_name(n[i])
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
            beta = narrow,
            beta_CI =wide,
            sr2=narrow,
            sr2_CI =wide*.8,
            r=narrow,
            summary=wide*1.3,
            difference=wide*1.4)
}

get_rtf_column_widths <- function(df) {
     n <- names(df)
     width_out <- c()
     for (i in 1:length(n)) {
          width_out[i] <-output_column_width(n[i])
     }
     return(width_out)
}

get_anova_table_note_txt <- function(calculate_cor,calculate_beta) {
          table_note <- "Note. * indicates p < .05; ** indicates p < .01.\nA significant b-weight indicates the beta-weight and semi-partial correlation are also significant.\nb represents unstandardized regression weights; beta indicates the standardized regression weights; \nsr2 represents the semi-partial correlation squared; r represents the zero-order correlation.\nSquare brackets are used to enclose the lower and upper limits of a confidence interval.\n"
     return(table_note)
}

