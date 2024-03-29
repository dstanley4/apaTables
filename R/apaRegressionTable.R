#' Creates a regresion table in APA style
#' @param ... Regression (i.e., lm) result objects. Typically, one for each block in the regression.
#' @param filename (optional) Output filename document filename (must end in .rtf or .doc only)
#' @param prop.var.conf.level Level of confidence (.90 or .95, default .95) for interval around sr2, R2, and Delta R2. Use of .90 confidence level helps to create consistency between the CI overlapping with zero and conclusions based on the p-value for that block (or block difference).
#' @param table.number  Integer to use in table number output line
#' @return APA table object
#' @references
#' sr2 and delta R2 confidence intervals calculated via:
#'
#' Alf Jr, E. F., & Graf, R. G. (1999). Asymptotic confidence limits for the difference between two squared multiple correlations: A simplified approach. Psychological Methods, 4(1), 70.
#'
#' Note that Algina, Keselman, & Penfield (2008) found this approach can under some circumstances lead to inaccurate CIs on proportion of variance values.
#' You might consider using the Algina, Keselman, & Penfield (2008) approach via the apa.reg.boot.table function
#'
#' @examples
#' # View top few rows of goggles data set
#' # from Discovering Statistics Using R
#' head(album)
#'
#' # Single block example
#' blk1 <- lm(sales ~ adverts + airplay, data=album)
#' apa.reg.table(blk1)
#' table1 <- apa.reg.table(blk1,table.number = 1)
#'
#' # Two block example, more than two blocks can be used
#' blk1 <- lm(sales ~ adverts, data=album)
#' blk2 <- lm(sales ~ adverts + airplay + attract, data=album)
#' table2 <- apa.reg.table(blk1, blk2, table.number = 2)
#'
#' # Interaction product-term test with blocks
#' blk1 <- lm(sales ~ adverts + airplay, data=album)
#' blk2 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data=album)
#' table3 <- apa.reg.table(blk1, blk2, table.number = 3)
#'
#' # Interaction product-term test with blocks and additional product terms
#' blk1<-lm(sales ~ adverts + airplay, data=album)
#' blk2<-lm(sales ~ adverts + airplay + I(adverts*adverts) + I(airplay*airplay), data=album)
#' blk3<-lm(sales~adverts+airplay+I(adverts*adverts)+I(airplay*airplay)+I(adverts*airplay),data=album)
#' table4 <- apa.reg.table(blk1,blk2,blk3, table.number = 4)
#'
#' #Interaction product-term test with single regression (i.e., semi-partial correlation focus)
#' blk1 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data=album)
#' table5 <- apa.reg.table(blk1, table.number = 5)
#'
#' # Save Table 1 in a .doc document
#' apa.save(filename = "regression_tables.doc",
#'          table1,
#'          table2,
#'          table3,
#'          table4,
#'          table5)
#'
#' # Create a table for your PDF
#' # Include the lines below in your rmarkdown or Quarto document
#' apa.knit.table.for.pdf(table1)
#' apa.knit.table.for.pdf(table2)
#' apa.knit.table.for.pdf(table3)
#' apa.knit.table.for.pdf(table4)
#' apa.knit.table.for.pdf(table5)
#'
#' # delete demo file
#' if (file.exists("regression_tables.doc")) {
#'      file.remove("regression_tables.doc")
#' }
#' @export
apa.reg.table<-function(...,filename=NA,table.number=0, prop.var.conf.level = .95) {
     regression_results_list <- list(...)

     table_number <- table.number
     is_random_predictors <- FALSE

     if (is.na(filename)) {
          make_file_flag=FALSE
     } else {
          make_file_flag=TRUE
     }

     if (prop.var.conf.level == .90) {
          prop_var_conf_level <- .90
     } else {
          prop_var_conf_level <- .95
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
     n_size <- dim(first_result$model)[1]

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
          cur_result <- apa_single_block(regression_results_list[[i]],is_random_predictors, prop_var_conf_level)
          block_results[[i]] <- cur_result
     }


     is_multiple_blocks <- FALSE
     if (L>1) {
          is_multiple_blocks <- TRUE
     }

     #Combine blocks
     block_out_txt <- block_results[[1]]$model_details_txt
     block_out_rtf <- block_results[[1]]$model_details_rtf
     block_out_latex <- block_results[[1]]$model_details_latex


     if (is_multiple_blocks == TRUE) {
          cur_block_label <- "Model 1"

          model_label_line <- get_blank_row(block_out_txt)
          model_label_line[1,1] <- cur_block_label
          block_out_txt <- rbind(model_label_line, block_out_txt)

          model_label_line_rtf <- get_blank_row(block_out_rtf)
          model_label_line_rtf[1,1] <- cur_block_label
          block_out_rtf <- rbind(model_label_line_rtf, block_out_rtf)

          model_label_line_latex <- get_blank_row(block_out_latex)
          model_label_line_latex[1,1] <- cur_block_label
          block_out_latex <- rbind(model_label_line_latex, block_out_latex)
     }


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
               cur_block_out_latex <- block_results[[i]]$model_details_latex

               delta_R2_details <- get_delta_R2_blocks(blk2=cur_block_lm,blk1=last_block_lm,summary2=cur_block_summary,summary1=last_block_summary,n, prop_var_conf_level = prop_var_conf_level)

               num_lines <- dim(cur_block_out_txt)[1]

               cur_block_out_txt$difference[num_lines-2] <- delta_R2_details$deltaR2_txt
               cur_block_out_txt$difference[num_lines-1] <- delta_R2_details$deltaR2_CI_txt

               cur_block_out_rtf$difference[num_lines-2] <- delta_R2_details$deltaR2_rtf
               cur_block_out_rtf$difference[num_lines-1] <- delta_R2_details$deltaR2_CI_rtf

               cur_block_out_latex$difference[num_lines-2] <- delta_R2_details$deltaR2_latex
               cur_block_out_latex$difference[num_lines-1] <- delta_R2_details$deltaR2_CI_latex

               last_block_summary <- cur_block_summary
               last_block_lm <- cur_block_lm

               if (has_beta_cols(block_out_txt) == TRUE & (has_beta_cols(cur_block_out_txt) == FALSE)) {
                    block_out_txt <- select(block_out_txt, -beta, -beta_CI, -r)
                    block_out_rtf <- select(block_out_rtf, -beta, -beta_CI, -r)
                    block_out_latex <- select(block_out_latex, -beta, -beta_CI, -r)
               }

               cur_block_label <- sprintf("Model %g",i)

               model_label_line <- get_blank_row(cur_block_out_txt)
               model_label_line[1,1] <- cur_block_label

               model_label_line_rtf <- get_blank_row(cur_block_out_rtf)
               model_label_line_rtf[1,1] <- cur_block_label

               model_label_line_latex <- get_blank_row(cur_block_out_latex)
               model_label_line_latex[1,1] <- cur_block_label

               block_out_txt <- rbind(block_out_txt, model_label_line, cur_block_out_txt)
               block_out_rtf <- rbind(block_out_rtf,model_label_line, cur_block_out_rtf)
               block_out_latex <- rbind(block_out_latex,model_label_line, cur_block_out_latex)
          }
     } else {
          block_out_txt <- dplyr::select(block_out_txt, -difference)
          block_out_rtf <- dplyr::select(block_out_rtf, -difference)
          block_out_latex <- dplyr::select(block_out_latex, -difference)

     }


     block_out_txt$predictor <- clean_predictor_names(block_out_txt$predictor)
     block_out_rtf$predictor <- clean_predictor_names(block_out_rtf$predictor)
     block_out_latex$predictor <- clean_predictor_names(block_out_latex$predictor)



     #console table

     first_title_criterion = first_criterion
     #first_title_criterion = gsub("_", " ", first_title_criterion)

     if (is_multiple_blocks == TRUE) {
          table_title <- sprintf("Hierarchical Multiple Regression Predicting %s\n",stringr::str_to_title(first_title_criterion))
     } else {
          table_title <- sprintf("Regression Predicting %s\n",stringr::str_to_title(first_title_criterion))
     }
     txt_column_names <- get_txt_column_names(block_out_txt)
     if (prop_var_conf_level == .95) {
          txt_column_names <- sub("XX","95",txt_column_names)
     } else {
          txt_column_names <- sub("XX","90",txt_column_names)
     }
     names(block_out_txt) <- txt_column_names


     table_body <- block_out_txt

     table_note <- get_reg_table_note_txt(first_block_calculate_cor, first_block_calculate_beta, n_size)

     table_block_results <-  block_results

     tbl_console <- list(table_number = table_number,
                         table_title = table_title,
                         table_body = as.data.frame(table_body),
                         table_note = table_note,
                         table_block_results = table_block_results)

     table_block_results <-  block_results

     class(tbl_console) <- "apa_table"



     if (is_multiple_blocks == TRUE) {
          table_title <- sprintf("Hierarchical Multiple Regression Predicting %s\n",    stringr::str_to_title(first_title_criterion))
          table_title_latex <- sprintf("Hierarchical Multiple Regression Predicting %s",stringr::str_to_title(first_title_criterion))
     } else {
          table_title <- sprintf("Regression Predicting %s\n",    stringr::str_to_title(first_title_criterion))
          table_title_latex <- sprintf("Regression Predicting %s",stringr::str_to_title(first_title_criterion))
     }

     table_note <- get_reg_table_note_rtf(first_block_calculate_cor, first_block_calculate_beta, n_size)
     table_note_latex <- get_reg_table_note_latex(first_block_calculate_cor, first_block_calculate_beta, n_size)

     #set columns widths and names
     colwidths <- get_rtf_column_widths(block_out_rtf)

     regression_table <- as.matrix(block_out_rtf)

     rtf_column_names <- get_rtf_column_names(block_out_rtf)
     if (prop_var_conf_level == .95) {
          rtf_column_names <- sub("XX","95",rtf_column_names)
     } else {
          rtf_column_names <- sub("XX","90",rtf_column_names)
     }
     new_col_names <- rtf_column_names


     colnames(regression_table) <- new_col_names


     #Create RTF code
     rtfTable <- RtfTable$new(isHeaderRow=TRUE, defaultDecimalTableProportionInternal=.15)
     rtfTable$setTableContent(regression_table)
     rtfTable$setCellWidthsInches(colwidths)
     rtfTable$setRowSecondColumnDecimalTab(.4)
     txt_body <- rtfTable$getTableAsRTF(FALSE,FALSE)

     if (make_file_flag==TRUE) {

           if (is_multiple_blocks==TRUE) {
               write.rtf.table(filename = filename,txt.body = txt_body,table.title = table_title, table.note = table_note,landscape=TRUE,table.number=table_number)
           } else {
               write.rtf.table(filename = filename,txt.body = txt_body,table.title = table_title, table.note = table_note,table.number=table_number,landscape=TRUE)
           }

     }

     tbl_console$rtf.body         <- txt_body
     tbl_console$rtf.table.title  <- table_title
     tbl_console$rtf.table.note   <- table_note

     block_out_latex$predictor <- gsub("_", " ", block_out_latex$predictor)

     tbl_console$latex.body         <- block_out_latex
     tbl_console$latex.table.title  <- table_title_latex
     tbl_console$latex.table.note   <- table_note_latex

     tbl_console$landscape       <- TRUE
     tbl_console$table.type      <- "regression"

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

     #adjust df xyzzy
     if (reg_table$predictor[1] == "(Intercept)") {
          model_summary_extended$df[1] <- model_summary_extended$df[1]-1
     }

     R2          <- model_summary_extended$r.squared
     R2_pvalue   <- model_summary_extended$p.value
     df1         <- model_summary_extended$df
     df2         <- model_summary_extended$df.residual

     prop_var_conf_level_str <- sprintf("%g", round(prop_var_conf_level*100))

     if (requireNamespace("MBESS", quietly = TRUE)) {
          R2CI <- MBESS::ci.R2(R2=R2,df.1=df1,df.2=df2,Random.Predictors = is_random_predictors, conf.level = prop_var_conf_level)
          R2LL <- R2CI$Lower.Conf.Limit.R2
          R2UL <- R2CI$Upper.Conf.Limit.R2
     } else {
          R2LL <- NA
          R2UL <- NA
          cat("\nMBESS package needs to be installed to calculate R2 confidence intervals.\n")
     }



     R2_txt     <- strip.leading.zero(add.sig.stars(sprintf("%1.3f",R2),R2_pvalue))
     R2LL_txt   <- strip.leading.zero(sprintf("%1.2f",R2LL))
     R2UL_txt   <- strip.leading.zero(sprintf("%1.2f",R2UL))

     model_summary_txt    <- sprintf("R2 = %s",R2_txt)
     model_summary_CI_txt <- sprintf("%s%% CI[%s,%s]",prop_var_conf_level_str, R2LL_txt,R2UL_txt)

     model_summary_rtf    <- sprintf("{\\i R\\super 2 \\nosupersub}  = %s",R2_txt)
     model_summary_CI_rtf <- sprintf("%s%% CI[%s,%s]",prop_var_conf_level_str, R2LL_txt,R2UL_txt)

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
          p_name  <- reg_table_lower$predictor[i]
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
     model_details$sr2    <- add.sig.stars(model_details$sr2, model_details_extended$p)
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
     model_details_latex <- model_details_txt

     model_details_txt$summary[num_row-2] <- model_summary_txt
     model_details_txt$summary[num_row-1] <- model_summary_CI_txt

     model_details_rtf$summary[num_row-2] <- model_summary_rtf
     model_details_rtf$summary[num_row-1] <- model_summary_CI_rtf

     model_details_latex$summary[num_row-2] <- model_summary_latex
     model_details_latex$summary[num_row-1] <- model_summary_CI_latex


     output <- list()
     output$model_summary_extended <- model_summary_extended
     output$model_details_extended <- model_details_extended
     output$model_details_txt      <- model_details_txt
     output$model_details_rtf      <- model_details_rtf
     output$model_details_latex    <- model_details_latex
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

output_rtf_name <- function(column_name) {
     switch(column_name,
            predictor="Predictor",
            b = "{\\i b}",
            b_CI = "{95% CI}",
            beta = "{\\i beta}",
            beta_CI ="{95% CI}",
            sr2="Unique {\\i R\\super 2 \\nosupersub}",
            sr2_CI ="{XX% CI}",
            r="{\\i r}",
            summary="Fit",
            difference="\\u0916\3Fit")
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
            beta = narrow*.9,
            beta_CI =wide,
            sr2=narrow*1.1,
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

get_reg_table_note_txt <- function(calculate_cor,calculate_beta, n_size) {

     table_note <- sprintf("Note. N = %g. b = unstandardized regression weight. beta = standardized regression weight. Unique R2 = semipartial correlation squared. r = zero-order correlation. CI = confidence interval.  \n* indicates p < .05. ** indicates p < .01.\n",n_size)

     return(table_note)

}


get_reg_table_note_rtf <- function(calculate_cor,calculate_beta, n_size) {
     table_note <- sprintf("{\\i N} = %g. {\\i b} = unstandardized regression weight. {\\i beta} = standardized regression weight. Unique {\\i R\\super 2\\nosupersub} = semipartial correlation squared. {\\i r} = zero-order correlation. CI = confidence interval. \\line* indicates p < .05. ** indicates p < .01.", n_size)
     return(table_note)
}


get_reg_table_note_latex <- function(calculate_cor,calculate_beta, n_size) {
     table_note <- sprintf("\\\\textit{Note}. $N$ = %g. $b$ = unstandardized regression weight. $beta$ = standardized regression weight. Unique $R^2$ = semipartial correlation squared. $r$ = zero-order correlation. CI = confidence interval. \\\\newline  * indicates $p$ < .05. ** indicates $p$ < .01.", n_size)
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
     deltaR2_rtf <- sprintf("\\u0916\3{\\i R\\super 2 \\nosupersub}  = %s", deltaR2_str)
     deltaR2_latex <- sprintf("$\\Delta R^2$  = %s", deltaR2_str)



     deltaR2_CI <- get_deltaR2_ci(R2_2 = R2_2, R2_1 = R2_1,n=n, conf_level = prop_var_conf_level)
     deltaR2_LL_str <- strip.leading.zero(sprintf("%1.2f",deltaR2_CI$LL))
     deltaR2_UL_str <- strip.leading.zero(sprintf("%1.2f",deltaR2_CI$UL))


     prop_var_conf_level_str <- sprintf("%g", round(prop_var_conf_level*100))
     deltaR2_CI_rtf <- sprintf("{%s%% CI}[%s, %s]",prop_var_conf_level_str, deltaR2_LL_str,deltaR2_UL_str)
     deltaR2_CI_txt <- sprintf("%s%% CI[%s, %s]",prop_var_conf_level_str, deltaR2_LL_str,deltaR2_UL_str)
     deltaR2_CI_latex <- sprintf("%s\\%% CI[%s, %s]",prop_var_conf_level_str, deltaR2_LL_str,deltaR2_UL_str)


     output <- list()
     output$deltaR2_txt    <- deltaR2_txt
     output$deltaR2_CI_txt <- deltaR2_CI_txt

     output$deltaR2_rtf <- deltaR2_rtf
     output$deltaR2_CI_rtf <- deltaR2_CI_rtf
     output$deltaR2_latex <- deltaR2_latex
     output$deltaR2_CI_latex <- deltaR2_CI_latex
     output$deltaR2        <- deltaR2
     output$deltaR2_pvalue <- deltaR2_p
     return(output)
}


has_beta_cols <- function(df) {
     return("beta" %in% names(df))
}

clean_predictor_names <- function(x) {
     N <- length(x)
     for (i in 1:N) {
          x[i] <- remove_product_chars(x[i])
     }
 return(x)
}


remove_product_chars <- function(original_string) {
     modified_string <- gsub("I\\(", "", original_string)
     modified_string <- gsub("\\)", "", modified_string)
     modified_string <- gsub("\\(", "", modified_string)
     modified_string <- gsub("\\s", "", modified_string)
     modified_string <- gsub("\\*", " x ", modified_string)
     modified_string <- gsub("Intercept", "(Intercept)", modified_string)

     pattern <- "Model(\\d+)"
     replacement <- "Model \\1"
     modified_string <- gsub(pattern = pattern, replacement = replacement, modified_string)

     return(modified_string)
}
