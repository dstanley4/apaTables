#' Creates a regresion table in APA style
#' @param ... Regression (i.e., lm) result objects. Typically, one for each block in the regression.
#' @param filename (optional) Output filename document filename (must end in .rtf or .doc only)
#' @param table.number  Integer to use in table number output line
#' @return APA table object
#' @references
#' sr2 and delta R2 confidence intervals calculated via:
#'
#' Alf Jr, E. F., & Graf, R. G. (1999). Asymptotic confidence limits for the difference between two squared multiple correlations: A simplified approach. Psychological Methods, 4(1), 70.
#'
#' @examples
#' # View top few rows of goggles data set
#' # from Discovering Statistics Using R
#' head(album)
#'
#' # Single block example
#' blk1 <- lm(sales ~ adverts + airplay, data=album)
#' apa.reg.table(blk1)
#' apa.reg.table(blk1,filename="exRegTable.doc")
#'
#' # Two block example, more than two blocks can be used
#' blk1 <- lm(sales ~ adverts, data=album)
#' blk2 <- lm(sales ~ adverts + airplay + attract, data=album)
#' apa.reg.table(blk1,blk2,filename="exRegBlocksTable.doc")
#'
#' # Interaction product-term test with blocks
#' blk1 <- lm(sales ~ adverts + airplay, data=album)
#' blk2 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data=album)
#' apa.reg.table(blk1,blk2,filename="exInteraction1.doc")
#'
#' # Interaction product-term test with blocks and additional product terms
#' blk1<-lm(sales ~ adverts + airplay, data=album)
#' blk2<-lm(sales ~ adverts + airplay + I(adverts*adverts) + I(airplay*airplay), data=album)
#' blk3<-lm(sales~adverts+airplay+I(adverts*adverts)+I(airplay*airplay)+I(adverts*airplay),data=album)
#' apa.reg.table(blk1,blk2,blk3,filename="exInteraction2.doc")
#'
#' #Interaction product-term test with single regression (i.e., semi-partial correlation focus)
#' blk1 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data=album)
#' apa.reg.table(blk1,filename="exInteraction3.doc")
#'
#' @export
apa.reg.table<-function(...,filename=NA,table.number=NA) {
     regression_results_list <- list(...)

     table_number <- table.number
     is_random_predictors <- FALSE

     if (is.na(filename)) {
          make_file_flag=FALSE
     } else {
          make_file_flag=TRUE
     }

     L=length(regression_results_list)
     is_same_criterion <- c()
     first_result <- regression_results_list[[1]]
     first_criterion <- colnames(first_result$model)[1]
     for (i in 1:L) {
          cur_result <- regression_results_list[[i]]
          cur_criterion_name <- colnames(cur_result$model)[1]
          is_same_criterion[i] <- first_criterion == cur_criterion_name
     }
     if (any(is_same_criterion == FALSE)) {
          cat("apa.reg.table error:\nAll regression objects (i.e., blocks) must use the same criterion.\n")
          cat("The regression objects used had different criterion variables.\n\n")
          return(FALSE)
     }


     is_same_predictors <- c()
     first_result <- regression_results_list[[1]]
     first_model  <- first_result$model
     last_model_number_predictors <- dim(first_model)[2]
     last_predictors <- colnames(first_result$model)[2:last_model_number_predictors]
     n <- dim(first_result$model)[1]
     for (i in 1:L) {
          cur_result <- regression_results_list[[i]]
          cur_model  <- cur_result$model
          cur_model_number_predictors <- dim(cur_model)[2]
          cur_predictors <- colnames(cur_model)[2:cur_model_number_predictors]
          is_same_predictors[i] <- all(intersect(last_predictors,cur_predictors) == last_predictors)
          last_predictors <- cur_predictors
     }
     if (any(is_same_predictors==FALSE)) {
          cat("apa.reg.table error:\nEach regression objects (i.e., block) must contain all of the predictors from the preceeding regression object (i.e., block).\n\n")
          cat("For example:\n")
          cat("block1 <- lm(y ~ a + b)\n")
          cat("block2 <- lm(y ~ a + b + c)\n\n")
          cat("The second block contains all of the predictors from the first block plus additional predictors.\n\n")
          cat("Therefore the command below will work: \n\n")
          cat("apa.reg.table(block1, block2)\n\n")
          return(FALSE)
     }



     #get analyses for each block
     block_results <- list()
     L <- length(regression_results_list)
     for (i in 1:L) {
          cur_result <- apa_single_block(regression_results_list[[i]],is_random_predictors)
          block_results[[i]] <- cur_result
     }


     is_multiple_blocks <- FALSE
     if (L>1) {
          is_multiple_blocks <- TRUE
     }

     #Combine blocks
     block_out_txt <- block_results[[1]]$model_details_txt
     block_out_rtf <- block_results[[1]]$model_details_rtf

     first_block_calculate_cor  <- block_results[[1]]$calculate_cor # use later for table note
     first_block_calculate_beta <- block_results[[1]]$calculate_beta # use later for table note

     last_block_summary <- block_results[[1]]$model_summary_extended
     last_block_lm <- regression_results_list[[1]]
     if (is_multiple_blocks == TRUE) {
          for (i in 2:L) {
               cur_block_lm <- regression_results_list[[i]]

               cur_block_summary <- block_results[[i]]$model_summary_extended

               cur_block_out_txt <- block_results[[i]]$model_details_txt
               cur_block_out_rtf <- block_results[[i]]$model_details_rtf

               delta_R2_details <- get_delta_R2_blocks(blk2=cur_block_lm,blk1=last_block_lm,summary2=cur_block_summary,summary1=last_block_summary,n)

               num_lines <- dim(cur_block_out_txt)[1]
               cur_block_out_txt$difference[num_lines-2] <- delta_R2_details$deltaR2_txt
               cur_block_out_txt$difference[num_lines-1] <- delta_R2_details$deltaR2_CI_txt

               cur_block_out_rtf$difference[num_lines-2] <- delta_R2_details$deltaR2_rtf
               cur_block_out_rtf$difference[num_lines-1] <- delta_R2_details$deltaR2_CI_rtf

               last_block_summary <- cur_block_summary
               last_block_lm <- cur_block_lm

               block_out_txt <- rbind(block_out_txt,cur_block_out_txt)
               block_out_rtf <- rbind(block_out_rtf,cur_block_out_rtf)

          }
     } else {
          block_out_txt <- dplyr::select(block_out_txt, -difference)
          block_out_rtf <- dplyr::select(block_out_rtf, -difference)
     }




     #console table
     table_title <- sprintf("Regression results using %s as the criterion\n",first_criterion)
     names(block_out_txt) <- get_txt_column_names(block_out_txt)
     table_body <- block_out_txt

     table_note <- get_reg_table_note_txt(first_block_calculate_cor, first_block_calculate_beta)

     table_block_results <-  block_results

     tbl_console <- list(table_number = table_number,
                         table_title = table_title,
                         table_body = table_body,
                         table_note = table_note,
                         table_block_results = table_block_results)

     table_block_results <-  block_results

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
               write.rtf.table(filename = filename,txt.body = txt_body,table.title = table_title, table.note = table_note,table.number=table_number,landscape=TRUE)
           }

     }


     return(tbl_console)
}


apa_single_block<-function(cur_blk,is_random_predictors) {
     #Regression data
     reg_table_data <- cur_blk$model
     n              <- dim(reg_table_data)[1]
     col_names      <- names(reg_table_data)


     #Determine if beta weights should be calculated
     is_weighted    <- "(weights)" %in% col_names
     is_var_factor  <- is_variable_factor(reg_table_data)
     calculate_beta <- TRUE
     calculate_cor  <- TRUE
     if (is_weighted == TRUE) {
          calculate_beta <- FALSE
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

     #adjust df
     if (reg_table$predictor[1] == "(Intercept)") {
          model_summary_extended$df[1] <- model_summary_extended$df[1]-1
     }

     R2          <- model_summary_extended$r.squared
     R2_pvalue   <- model_summary_extended$p.value
     df1         <- model_summary_extended$df
     df2         <- model_summary_extended$df.residual

     R2CI <- MBESS::ci.R2(R2=R2,df.1=df1,df.2=df2,Random.Predictors = is_random_predictors)
     R2LL <- R2CI$Lower.Conf.Limit.R2
     R2UL <- R2CI$Upper.Conf.Limit.R2

     R2_txt     <- strip.leading.zero(add.sig.stars(sprintf("%1.3f",R2),R2_pvalue))
     R2LL_txt   <- strip.leading.zero(sprintf("%1.2f",R2LL))
     R2UL_txt   <- strip.leading.zero(sprintf("%1.2f",R2UL))

     model_summary_txt    <- sprintf("R2 = %s",R2_txt)
     model_summary_CI_txt <- sprintf("95%% CI[%s,%s]",R2LL_txt,R2UL_txt)

     model_summary_rtf    <- sprintf("{\\i R\\super 2 \\nosupersub}  = %s",R2_txt)
     model_summary_CI_rtf <- sprintf("95%% CI[%s,%s]",R2LL_txt,R2UL_txt)


     #Add b-weight CI's
     b_CI <- confint(cur_blk)
     LLb  <- b_CI[,c("2.5 %")]
     ULb  <- b_CI[,c("97.5 %")]

     reg_table <- dplyr::mutate(reg_table, LLb=LLb, ULb=ULb)
     reg_table <- dplyr::select(reg_table,predictor, b,LLb,ULb,SE,t,p)

     #table parts
     reg_table_components <- table_without_intercept_row(reg_table)
     reg_table_lower      <- reg_table_components$lower_table
     reg_table_first      <- reg_table_components$first_row #intercept row

     #correlation
     if (calculate_cor==TRUE) {
          r_with_criterion <- correlations_with_criterion(reg_table_data)
          reg_table_lower  <- dplyr::inner_join(reg_table_lower,r_with_criterion,by="predictor")
     }

     #semi-partial correlation squared
     reg_table_lower   <- dplyr::mutate(reg_table_lower, sr2=(t*t)*(1-R2)/df2, LLsr2=-999,ULsr2=-999)
     number_predictors <- length(reg_table_lower[,1])
     if (number_predictors>1) {
          #use delta R2 process for CI
          for (i in 1:number_predictors) {
               sr2 <- reg_table_lower$sr2[i]
               ci  <- get_sr2_ci(sr2=sr2,R2=R2,n=n)
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
          for (i in 1:number_predictors) {
               p_name  <- reg_table_lower$predictor[i]
               b       <- reg_table_lower$b[i]
               LLb     <- reg_table_lower$LLb[i]
               ULb     <- reg_table_lower$ULb[i]

               sd_crit <- stats::sd(reg_table_data[,1],na.rm = TRUE)
               sd_pred <- stats::sd(reg_table_data[,p_name],na.rm = TRUE)


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
     model_details$sr2[1]  <- ""
     model_details$sr2_CI     <- CIsr2_str
     model_details$sr2_CI[1]  <- ""







     if (calculate_cor==TRUE) {

          model_details$r     <- strip.leading.zero(add.sig.stars(sprintf("%1.2f",model_details_extended$r), model_details_extended$r_pvalue)) #intercept row issue
          model_details$r[1]  <- ""

          product_rows <- is_product_row(model_details$predictor)
          model_details$r[product_rows] <- ""

     }



     model_details_txt <- model_details
     model_details_txt$summary      <- ""
     model_details_txt$difference <- "" #blank column to be populated later

     model_details_txt <- add_row_to_model_summary(model_details_txt)
     model_details_txt <- add_row_to_model_summary(model_details_txt)
     model_details_txt <- add_row_to_model_summary(model_details_txt)

     num_row <- dim(model_details_txt)[1]
     model_details_rtf <- model_details_txt

     model_details_txt$summary[num_row-2] <- model_summary_txt
     model_details_txt$summary[num_row-1] <- model_summary_CI_txt

     model_details_rtf$summary[num_row-2] <- model_summary_rtf
     model_details_rtf$summary[num_row-1] <- model_summary_CI_rtf


     output <- list()
     output$model_summary_extended <- model_summary_extended
     output$model_details_extended <- model_details_extended
     output$model_details_txt      <- model_details_txt
     output$model_details_rtf      <- model_details_rtf
     output$calculate_beta         <- calculate_beta
     output$calculate_cor          <- calculate_cor

     return(output)
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
            sr2_CI =wide,
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

get_reg_table_note_txt <- function(calculate_cor,calculate_beta) {
     if (calculate_cor==TRUE & calculate_beta==TRUE) {

          table_note <- "Note. * indicates p < .05; ** indicates p < .01.\nA significant b-weight indicates the beta-weight and semi-partial correlation are also significant.\nb represents unstandardized regression weights; beta indicates the standardized regression weights; \nsr2 represents the semi-partial correlation squared; r represents the zero-order correlation.\nSquare brackets are used to enclose the lower and upper limits of a confidence interval.\n"

     } else if (calculate_cor==TRUE & calculate_beta==FALSE) {

          table_note <- "Note. * indicates p < .05; ** indicates p < .01.\nA significant b-weight indicates the semi-partial correlation is also significant.\nb represents unstandardized regression weights; sr2 represents the semi-partial correlation squared;\nr represents the zero-order correlation.\nSquare brackets are used to enclose the lower and upper limits of a confidence interval.\n"

     } else if (calculate_cor==FALSE & calculate_beta==TRUE) {

          table_note <- "Note. * indicates p < .05; ** indicates p < .01.\nA significant b-weight indicates the beta-weight and semi-partial correlation are also significant.\nb represents unstandardized regression weights; beta indicates the standardized regression weights; \nsr2 represents the semi-partial correlation squared.\nSquare brackets are used to enclose the lower and upper limits of a confidence interval.\n"
     } else {
          table_note <- "Note. * indicates p < .05; ** indicates p < .01.\nA significant b-weight indicates the semi-partial correlation is also significant.\nb represents unstandardized regression weights; \nsr2 represents the semi-partial correlation squared.\nSquare brackets are used to enclose the lower and upper limits of a confidence interval.\n"
     }
     return(table_note)

}


get_reg_table_note_rtf <- function(calculate_cor,calculate_beta) {
     if (calculate_cor==TRUE & calculate_beta==TRUE) {

          table_note <- "* indicates {\\i p} < .05; ** indicates {\\i p} < .01. A significant {\\i b}-weight indicates the beta-weight and semi-partial correlation are also significant. {\\i b} represents unstandardized regression weights; {\\i beta} indicates the standardized regression weights; {\\i sr\\super 2\\nosupersub} represents the semi-partial correlation squared; {\\i r} represents the zero-order correlation. {\\i LL} and {\\i UL} indicate the lower and upper limits of a confidence interval, respectively."

     } else if (calculate_cor==TRUE & calculate_beta==FALSE) {

          table_note <- "* indicates {\\i p} < .05; ** indicates {\\i p} < .01. A significant {\\i b}-weight indicates the semi-partial correlation is also significant. {\\i b} represents unstandardized regression weights; {\\i sr\\super 2\\nosupersub} represents the semi-partial correlation squared; {\\i r} represents the zero-order correlation. {\\i LL} and {\\i UL} indicate the lower and upper limits of a confidence interval, respectively."

     } else if (calculate_cor==FALSE & calculate_beta==TRUE) {

          table_note <- "* indicates {\\i p} < .05; ** indicates {\\i p} < .01. A significant {\\i b}-weight indicates the beta-weight and semi-partial correlation are also significant. {\\i b} represents unstandardized regression weights; {\\i beta} indicates the standardized regression weights; {\\i sr\\super 2\\nosupersub} represents the semi-partial correlation squared. {\\i LL} and {\\i UL} indicate the lower and upper limits of a confidence interval, respectively."

     } else {
          table_note <- "* indicates p < .05; ** indicates p < .01. A significant {\\i b}-weight indicates the semi-partial correlation is also significant. {\\i b} represents unstandardized regression weights; {\\i sr\\super 2\\nosupersub} represents the semi-partial correlation squared. {\\i LL} and {\\i UL} indicate the lower and upper limits of a confidence interval, respectively."
     }
     return(table_note)

}




convert_b_to_beta <- function(b, sd_pred,sd_crit) {
     beta_value <- b*(sd_pred / sd_crit)
     return(beta_value)
}
