#' Creates a regresion table in APA style
#' @param ... Regression (i.e., lm) result objects. Typically, one for each block in the regression.
#' @param filename (optional) Output filename document filename (must end in .rtf or .doc only)
#' @param table.number  Integer to use in table number output line
#' @return APA table object
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
#' # V1: Interaction product-term test with single regression (i.e., semi-partial correlation focus)
#' blk1 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data=album)
#' apa.reg.table(blk1,filename="exInteraction3.doc")
#'
#' # V2: Interaction product-term test with single regression (i.e., semi-partial correlation focus)
#' blk1<-lm(sales~adverts*airplay,data=album)
#' apa.reg.table(blk1,filename="exInteraction4.doc")
#' @export
apa.reg.table<-function(...,filename=NA,table.number=NA,is.random.predictors=FALSE) {
     regression.results.list <- list(...)

     is_random_predictors <- is.random.predictors
     if (is.na(filename)) {
          make.file.flag=FALSE
     } else {
          make.file.flag=TRUE
     }




     L=length(regression.results.list)
     is.same.criterion <- c()
     first.result <- regression.results.list[[1]]
     first.criterion <- colnames(first.result$model)[1]
     for (i in 1:L) {
          cur.result <- regression.results.list[[i]]
          cur.criterion.name <- colnames(cur.result$model)[1]
          is.same.criterion[i] <- first.criterion == cur.criterion.name
     }
     if (any(is.same.criterion==FALSE)) {
          cat("apa.reg.table error:\nAll regression objects (i.e., blocks) must use the same criterion.\n")
          cat("The regression objects used had different criterion variables.\n\n")
          return(FALSE)
     }



     is.same.predictors<- c()
     first.result <- regression.results.list[[1]]
     first.model <- first.result$model
     last.model.number.predictors <- dim(first.model)[2]
     last.predictors <- colnames(first.result$model)[2:last.model.number.predictors]
     n <- dim(first.result$model)[1]
     for (i in 1:L) {
          cur.result <- regression.results.list[[i]]
          cur.model <- cur.result$model
          cur.model.number.predictors <- dim(cur.model)[2]
          cur.predictors <- colnames(cur.model)[2:cur.model.number.predictors]
          is.same.predictors[i] <- all(intersect(last.predictors,cur.predictors) == last.predictors)
          last.predictors = cur.predictors
     }
     if (any(is.same.predictors==FALSE)) {
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
     block.results <- list()
     L=length(regression.results.list)
     for (i in 1:L) {
          cur.result <- apa_single_block(regression.results.list[[i]],is_random_predictors)
          block.results[[i]] <- cur.result
     }


     is.multiple.blocks = FALSE
     if (L>1) {
          is.multiple.blocks = TRUE
     }

     #Combine blocks
     block.out.txt <- block.results[[1]]$model_details_txt
     block.out.rtf <- block.results[[1]]$model_details_rtf
     last.block.summary <- block.results[[1]]$model_summary_extended
     last.block.lm <- regression.results.list[[1]]
     if (is.multiple.blocks == TRUE) {
          for (i in 2:L) {
               cur.block.lm <- regression.results.list[[i]]

               cur.block.summary <- block.results[[i]]$model_summary_extended

               cur.block.out.txt <- block.results[[i]]$model_details_txt
               cur.block.out.rtf <- block.results[[i]]$model_details_rtf

               delta_R2_details <- get_delta_R2_blocks(blk2=cur.block.lm,blk1=last.block.lm,summary2=cur.block.summary,summary1=last.block.summary,n)

               num_lines <- dim(cur.block.out.txt)[1]
               cur.block.out.txt$difference[num_lines-2] <- delta_R2_details$deltaR2_txt
               cur.block.out.txt$difference[num_lines-1] <- delta_R2_details$deltaR2_CI_txt

               cur.block.out.rtf$difference[num_lines-2] <- delta_R2_details$deltaR2_rtf
               cur.block.out.rtf$difference[num_lines-1] <- delta_R2_details$deltaR2_CI_rtf

               last.block.summary <- cur.block.summary
               last.block.lm <- cur.block.lm

               block.out.txt <- rbind(block.out.txt,cur.block.out.txt)
               block.out.rtf <- rbind(block.out.rtf,cur.block.out.rtf)

          }
     } else {
          block.out.txt <- dplyr::select(block.out.txt, -difference)
          block.out.rtf <- dplyr::select(block.out.rtf, -difference)
     }




     #console table
     table.title <- sprintf("Regression results using %s as the criterion\n",first.criterion)
     names(block.out.txt) <- get_txt_column_names(block.out.txt)
     table.body <- block.out.txt
     table.note <- "Note. * indicates p < .05; ** indicates p < .01.\nA significant b-weight indicates the beta-weight and semi-partial correlation are also significant.\nb represents unstandardized regression weights; SE represents the standard error of the \nunstandardized regression weights; beta indicates the beta-weights or standardized regression weights; \nsr2 represents the semi-partial correlation squared;r represents the zero-order correlation.\n"
     table.block.results <-  block.results

     tbl.console <- list(table.number = table.number,
                         table.title = table.title,
                         table.body = table.body,
                         table.note = table.note,
                         table.block.results=table.block.results)

     table.block.results <-  block.results

     class(tbl.console) <- "apa.table"



     if (make.file.flag==TRUE) {
          table.title <- sprintf("Regression results using %s as the criterion\n",first.criterion)
          #table.note <- "* indicates {\\i p} < .05; ** indicates {\\i p} < .01."
          table.note <- "* indicates {\\i p} < .05; ** indicates {\\i p} < .01. A significant {\\i b}-weight indicates the beta-weight and semi-partial correlation are also significant. {\\i b} represents unstandardized regression weights; {\\i SE} represents the standard error of the unstandardized regression weights; {\\i beta} indicates the beta-weights or standardized regression weights; {\\i sr\\super 2\\nosupersub} represents the semi-partial correlation squared; {\\i r} represents the zero-order correlation."
          #set columns widths and names
          n.col <- .65
          w.col <- 1
          w.col2 <- 1.5


          colwidths <- get_rtf_column_widths(block.out.rtf)
          extend.columns.end <- c()
          extend.columns.start <- c()

          regressionTable.table <- as.matrix(block.out.rtf)
          new.col.names <- get_rtf_column_names(block.out.rtf)
          colnames(regressionTable.table) <- new.col.names


          #Create RTF code
          rtfTable <- RtfTable$new(isHeaderRow=TRUE, defaultDecimalTableProportionInternal=.15)
          rtfTable$setTableContent(regressionTable.table)
          rtfTable$setCellWidthsInches(colwidths)
          rtfTable$setRowSecondColumnDecimalTab(.4)
          txt.body <- rtfTable$getTableAsRTF(FALSE,FALSE)


           if (is.multiple.blocks==TRUE) {
               write.rtf.table(filename = filename,txt.body = txt.body,table.title = table.title, table.note = table.note,landscape=TRUE,table.number=table.number)
           } else {
                write.rtf.table(filename = filename,txt.body = txt.body,table.title = table.title, table.note = table.note,table.number=table.number)
           }

     }


     return(tbl.console)
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
     if (is_weighted==TRUE) {
          calculate_beta <- FALSE
     }
     if (is_var_factor==TRUE) {
          calculate_beta <- FALSE
          calculate_cor  <- FALSE
     }


     #Summary statistics
     model_summary_extended <- broom::glance(cur_blk) #glance include lm constant in df
     model_summary_extended$df <- summary(cur_blk)$df[1] #use summary information to get df without constant

     #Regression table statistics
     reg_table <- broom::tidy(cur_blk)
     names(reg_table) <- c("predictor","b","SE","t","p")

     #adjust df
     if (reg_table$predictor[1]=="(Intercept)") {
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
     b_CI<-confint(cur_blk)
     LLb <- b_CI[,c("2.5 %")]
     ULb <- b_CI[,c("97.5 %")]

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
               ci <- get_sr2_ci(sr2=sr2,R2=R2,n=n)
               LL <- ci$LL
               UL <- ci$UL
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

               beta    <- b*(sd_pred/sd_crit)
               LLbeta  <- LLb*(sd_pred/sd_crit)
               ULbeta  <- ULb*(sd_pred/sd_crit)

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

     model_details_extended <- rbind(intercept_row,reg_table_lower)

     L <- dim(model_details_extended)[1]

     CIb_str    <- txt.ci.brackets(model_details_extended$LLb,model_details_extended$ULb,strip_zero = FALSE)
     CIsr2_str  <- txt.ci.brackets(model_details_extended$LLsr2,model_details_extended$ULsr2,strip_zero = TRUE)

     if (calculate_beta==TRUE) {
          CIbeta_str <- txt.ci.brackets(model_details_extended$LLbeta,model_details_extended$ULbeta,strip_zero = FALSE)
     }

     model_details         <- model_details_extended[,1,drop=FALSE]

     model_details$b       <- add.sig.stars(sprintf("%1.2f",model_details_extended$b),model_details_extended$p)
     model_details$b_CI    <- CIb_str

     if (calculate_beta==TRUE) {
          model_details$beta     <- sprintf("%1.2f",model_details_extended$beta)
          model_details$beta[1]  <- ""

          model_details$beta_CI    <- CIbeta_str
          model_details$beta_CI[1] <- ""
     }

     model_details$sr2     <- strip.leading.zero(sprintf("%1.2f",model_details_extended$sr2))
     model_details$sr2[1]  <- ""
     model_details$sr2_CI     <- CIsr2_str
     model_details$sr2_CI[1]  <- ""

     if (calculate_cor==TRUE) {
          model_details$r     <- strip.leading.zero(add.sig.stars(sprintf("%1.2f",model_details_extended$r),model_details_extended$r_pvalue)) #intercept row issue
          model_details$r[1]  <- ""

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
     model_details_txt$summary[num_row-1]   <- model_summary_CI_txt

     model_details_rtf$summary[num_row-2] <- model_summary_rtf
     model_details_rtf$summary[num_row-1]   <- model_summary_CI_rtf


     output <- list()
     output$model_summary_extended <- model_summary_extended
     output$model_details_extended <- model_details_extended
     output$model_details_txt      <- model_details_txt
     output$model_details_rtf      <- model_details_rtf


     return(output)
}


is_product_row <- function(row_names) {
     is_a_colon <- grep(":",row_names)
     is_a_star <- grep("\\*",row_names)
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






# model_prep_print <- function(model_in=NA, model_number=1,past_model_diff=NA, rtf_flag=FALSE) {
#      model_in_summary <- model_in$model_summary
#      num_model_rows   <- dim(model_in_summary)[1]
#
#      model_in_summary$variables[1] <- sprintf("Model %d",model.number)
#
#
#      if (rtf_flag == FALSE) {
#           model_in_summary$fit[num_model_rows-1] <- model_in$model_stats$Fvalue.report.txt # put in F Stuff
#           model_in_summary$fit[num_model_rows-2] <- model_in$model_stats$R2.report.txt     # put in R Stuff
#           if (!is.na(past.model.diff[1])) {
#                model_in_summary$delta[num_model_rows-1] <- past_model_diff$Fout.txt   # put in F Stuff
#                model_in_summary$delta[num_model_rows-2] <- past_model_diff$R2out.txt # put in R Stuff
#           }
#      } else {
#           model_in_summary$fit[num_model_rows-1] <- model_in$model.stats$Fvalue.report.rtf # put in F Stuff
#           model_in_summary$fit[num_model_rows-2] <- model_in$model.stats$R2.report.rtf     # put in R Stuff
#           if (!is.na(past.model.diff[1])) {
#                model_in_summary$delta[num_model_rows-1] <- past_model_diff$Fout.rtf  # put in F Stuff
#                model_in_summary$delta[num_model_rows-2] <- past_model_diff$R2out.rtf # put in R Stuff
#           }
#      }
#
#
#      return(model.in.summary)
# }



output_txt_name <- function(column_name) {
     switch(column_name,
            predictor="Predictor",
            b = "b",
            b_CI = "b 95%CI",
            beta = "beta",
            beta_CI ="beta 95%CI",
            sr2="sr2",
            sr2_CI ="sr2 95%CI",
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
