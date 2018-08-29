#' Creates a regresion table in APA style with bootstrap confidence intervals
#' @param ... Regression (i.e., lm) result objects. Typically, one for each block in the regression.
#' @param filename (optional) Output filename document filename (must end in .rtf or .doc only)
#' @param table.number  Integer to use in table number output line
#' @param number.samples Number of samples to create for bootstrap CIs
#' @return APA table object
#' @references
#' Algina, J. Keselman, H.J. & Penfield, R.J. (2008). Note on a confidence interval for the squared semipartial correlation coefficient. Educational and Psychological Measurement, 68, 734-741.
#' @examples
#'
#' #Note: number.samples = 50 below.
#' #      However, please use a value of 1000 or higher
#'
#' # View top few rows of goggles data set
#' # from Discovering Statistics Using R
#' set.seed(1)
#' head(album)
#'
#' # Single block example
#' blk1 <- lm(sales ~ adverts + airplay, data=album)
#' \donttest{apa.reg.boot.table(blk1)}
#' \donttest{apa.reg.boot.table(blk1,filename="exRegTable.doc")}
#'
#' # Two block example, more than two blocks can be used
#' blk1 <- lm(sales ~ adverts, data=album)
#' blk2 <- lm(sales ~ adverts + airplay + attract, data=album)
#' \donttest{apa.reg.boot.table(blk1,blk2,filename="exRegBlocksTable.doc")}
#'
#' # Interaction product-term test with blocks
#' blk1 <- lm(sales ~ adverts + airplay, data=album)
#' blk2 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data=album)
#' \donttest{apa.reg.boot.table(blk1,blk2,filename="exInteraction1.doc")}
#' @export
apa.reg.boot.table<-function(...,filename=NA, table.number=NA, number.samples = 1000) {

     cat("\n\napa.reg.boot.table is a beta version.\n")

     prop_var_conf_level <- .95

     K <- number.samples
     conf.level <- .95
     conf_level <- conf.level

     prop.var.conf.level = .95
     if (prop.var.conf.level == .90) {
          prop_var_conf_level <- .90
     } else {
          prop_var_conf_level <- .95
     }



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
          cat(sprintf("Block %g: Generating %g bootstrap samples\n", i, K))
          cur_result <- apa_single_boot_block(regression_results_list[[i]],is_random_predictors, K = K, prop_var_conf_level = prop_var_conf_level)
          block_results[[i]] <- cur_result
     }
     cat(sprintf("Bootstrap for Delta RSQ in progress\n"))
     R2_boot_results<- boot::boot(data = regression_results_list[[L]]$model, statistic = boot_blocks, R=K, regression_results_list=regression_results_list)


     is_multiple_blocks <- FALSE
     if (L>1) {
          is_multiple_blocks <- TRUE
     }


     if (is_multiple_blocks == TRUE) {
          #contain (for each boot dataset/row) R2 for each block
          # must do some subtraction b2-b1 (b3-b2) etc... to get Delta's
          delta_R2_data <- calculate_boot_delta_R2(R2_boot_results$t)
          delta_R2_result <- R2_boot_results$t0
          delta_R2_CI <- get_delta_R2_CI(delta_R2_data, prop_var_conf_level)
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

               delta_R2_details <- get_delta_R2_blocks(blk2=cur_block_lm,blk1=last_block_lm,summary2=cur_block_summary,summary1=last_block_summary,n, prop_var_conf_level = prop_var_conf_level)

               delta_R2_details <- update_with_delta_R2_boot_CI(delta_R2_details, delta_R2_CI[i,], prop_var_conf_level = prop_var_conf_level)


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
     txt_column_names <- get_txt_column_names(block_out_txt)
     if (prop_var_conf_level == .95) {
          txt_column_names <- sub("XX","95",txt_column_names)
     } else {
          txt_column_names <- sub("XX","90",txt_column_names)
     }
     names(block_out_txt) <- txt_column_names



     table_body <- block_out_txt

     table_note <- get_reg_table_note_txt(first_block_calculate_cor, first_block_calculate_beta)

     table_block_results <-  block_results

     tbl_console <- list(table_number = table_number,
                         table_title = table_title,
                         table_body = as.data.frame(table_body),
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


          if (is_multiple_blocks==TRUE) {
               write.rtf.table(filename = filename,txt.body = txt_body,table.title = table_title, table.note = table_note,landscape=TRUE,table.number=table_number)
          } else {
               write.rtf.table(filename = filename,txt.body = txt_body,table.title = table_title, table.note = table_note,table.number=table_number,landscape=TRUE)
          }

     }


     return(tbl_console)
}


apa_single_boot_block<-function(cur_blk,is_random_predictors, K, prop_var_conf_level) {
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
     is_intercept <- FALSE
     if (reg_table$predictor[1] == "(Intercept)") {
          model_summary_extended$df[1] <- model_summary_extended$df[1]-1
          is_intercept <- TRUE
     }

     R2          <- model_summary_extended$r.squared
     R2_pvalue   <- model_summary_extended$p.value
     df1         <- model_summary_extended$df
     df2         <- model_summary_extended$df.residual


     # Get bootstrap samples
     boot_results <- boot::boot(data = reg_table_data, statistic = boot_equation, R = K, lm_out=cur_blk)
     boot_data <- get_boot_data(boot_results = boot_results)


     prop_var_conf_level_str <- sprintf("%g", round(prop_var_conf_level*100))

     R2CI_boot <- get_boot_R2_CI(boot_data, conf_level = prop_var_conf_level)
     R2LL <- R2CI_boot[1]
     R2UL <- R2CI_boot[2]
     R2_txt     <- strip.leading.zero(add.sig.stars(sprintf("%1.3f",R2),R2_pvalue))


     if (R2_pvalue>= .05) {
          R2LL <- 0 # Algina, Keselman, & Penfield (2008) Correction
          cat("     R2 CI lower bound adjusted to zero as per Algina, Keselman, & Penfield (2008)\n\n")
     }


     R2LL_txt   <- strip.leading.zero(sprintf("%1.2f",R2LL))
     R2UL_txt   <- strip.leading.zero(sprintf("%1.2f",R2UL))
     model_summary_txt    <- sprintf("R2 = %s",R2_txt)
     model_summary_CI_txt <- sprintf("%s%% CI[%s,%s]",prop_var_conf_level_str, R2LL_txt,R2UL_txt)
     model_summary_rtf    <- sprintf("{\\i R\\super 2 \\nosupersub}  = %s",R2_txt)
     model_summary_CI_rtf <- sprintf("%s%% CI[%s,%s]",prop_var_conf_level_str, R2LL_txt,R2UL_txt)


     #Add b-weight CI's
     b_CI_boot <- get_boot_b_CI(boot_data = boot_data, cur_blk = cur_blk, conf_level = .95)

     if (!is.null(dim(b_CI_boot))){
          LLb  <- b_CI_boot[,c("LL")]
          ULb  <- b_CI_boot[,c("UL")]
     } else {
          LLb  <- b_CI_boot[1]
          ULb  <- b_CI_boot[2]
     }


     # LLb  <- b_CI_boot[,c("LL")]
     # ULb  <- b_CI_boot[,c("UL")]
     # b_CI <- confint(cur_blk)
     # LLb  <- b_CI[,c("2.5 %")]
     # ULb  <- b_CI[,c("97.5 %")]
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
     number_predictors <- dim(reg_table_lower[,1])[1]
     # if (number_predictors>1) {
     #      #use delta R2 process for CI
     #      for (i in 1:number_predictors) {
     #           sr2 <- reg_table_lower$sr2[i]
     #           ci  <- get_sr2_ci(sr2=sr2,R2=R2,n=n)
     #           LL  <- ci$LL
     #           UL  <- ci$UL
     #           reg_table_lower$LLsr2[i] <- LL
     #           reg_table_lower$ULsr2[i] <- UL
     #      }
     # } else {
     #      #single predictor so use R2 CI
     #      reg_table_lower$LLsr2[1] <- R2LL
     #      reg_table_lower$ULsr2[1] <- R2UL
     # }

     sr2_CI_boot <- get_boot_sr2_CI(boot_data, cur_blk, prop_var_conf_level)
     sr2_CI_boot <- trim_intercept_row(sr2_CI_boot)
     if (!is.null(dim(sr2_CI_boot))){
          reg_table_lower$LLsr2 <- sr2_CI_boot[,c("LL")]
          reg_table_lower$ULsr2 <- sr2_CI_boot[,c("UL")]
     } else {
          reg_table_lower$LLsr2 <- sr2_CI_boot[1]
          reg_table_lower$ULsr2 <- sr2_CI_boot[2]
     }

     id_is_ns <- (reg_table_lower$p >= .05)
     if (sum(id_is_ns) >0) {
          reg_table_lower$LLsr2[id_is_ns] <- 0
          cat("     sr2 CI lower bound(s) adjusted to zero as per Algina, Keselman, & Penfield (2008)\n\n")
     }

     #beta
     if (calculate_beta==TRUE) {
          reg_table_lower   <- dplyr::mutate(reg_table_lower, beta = - 999, LLbeta = -999, ULbeta = -999)
          for (i in 1:number_predictors) {
               p_name  <- reg_table_lower$predictor[i]
               b       <- reg_table_lower$b[i]
               #LLb     <- reg_table_lower$LLb[i]
               #ULb     <- reg_table_lower$ULb[i]

               sd_crit <- stats::sd(reg_table_data[,1],na.rm = TRUE)
               sd_pred <- stats::sd(reg_table_data[,p_name],na.rm = TRUE)


               beta   <- convert_b_to_beta(b = b  , sd_pred = sd_pred, sd_crit = sd_crit)
               # LLbeta <- convert_b_to_beta(b = LLb, sd_pred = sd_pred, sd_crit = sd_crit)
               # ULbeta <- convert_b_to_beta(b = ULb, sd_pred = sd_pred, sd_crit = sd_crit)

               # beta    <-   b*(sd_pred / sd_crit)
               # LLbeta  <- LLb*(sd_pred / sd_crit)
               # ULbeta  <- ULb*(sd_pred / sd_crit)

               reg_table_lower$beta[i]   <- beta
               # reg_table_lower$LLbeta[i] <- LLbeta
               # reg_table_lower$ULbeta[i] <- ULbeta
          }
          beta_CI_boot <- get_boot_beta_CI(boot_data, cur_blk, .95)
          beta_CI_boot <- trim_intercept_row(beta_CI_boot)
          if (!is.null(dim(beta_CI_boot))){
               reg_table_lower$LLbeta <- beta_CI_boot[,c("LL")]
               reg_table_lower$ULbeta <- beta_CI_boot[,c("UL")]
          } else {
               reg_table_lower$LLbeta <- beta_CI_boot[1]
               reg_table_lower$ULbeta <- beta_CI_boot[2]
          }
     }

     #Combine with intercept
     if (reg_table_components$is_intercept == TRUE) {
          intercept_row         <- get_empty_row(reg_table_lower)
          intercept_row$predictor[1] <- reg_table_first$predictor[1]
          intercept_row$b[1]    <- reg_table_first$b[1]
          intercept_row$LLb[1]  <- reg_table_first$LLb[1]
          intercept_row$ULb[1]  <- reg_table_first$ULb[1]
          intercept_row$t[1]    <- reg_table_first$t[1]
          intercept_row$p[1]    <- reg_table_first$p[1]
          intercept_row$SE[1]   <- reg_table_first$SE[1]
          model_details_extended <- rbind(intercept_row, reg_table_lower)
     } else {
          model_details_extended <- reg_table_lower
     }


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
          model_details$beta_CI    <- CIbeta_str

          if (is_intercept == TRUE) {
               model_details$beta[1]  <- ""
               model_details$beta_CI[1] <- ""
          }
     }

     model_details$sr2     <- strip.leading.zero(sprintf("%1.2f", model_details_extended$sr2))
     model_details$sr2_CI     <- CIsr2_str

     if (is_intercept == TRUE) {
          model_details$sr2[1]  <- ""
          model_details$sr2_CI[1]  <- ""
     }



     if (calculate_cor==TRUE) {

          model_details$r     <- strip.leading.zero(add.sig.stars(sprintf("%1.2f",model_details_extended$r), model_details_extended$r_pvalue)) #intercept row issue
          product_rows <- is_product_row(model_details$predictor)

          if (is_intercept == TRUE) {
               model_details$r[1]  <- ""
               model_details$r[product_rows] <- ""
          }
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


boot_equation <- function(data, indices, lm_out){
     data2 <- data[indices, ]
     #data2 <- attitude
     cur_lm  <- lm(formula = lm_out$call$formula, data=data2)
     lm_data <- tibble::as_tibble(cur_lm$model)

     cur_results <- summary(cur_lm)
     cur_coef <- cur_results$coefficients
     t_values <- cur_coef[,colnames(cur_coef)=="t value"]
     b_values <- cur_coef[,colnames(cur_coef)=="Estimate"]
     names(b_values) <- rownames(cur_coef)
     names(t_values) <- rownames(cur_coef)


     df2 <- cur_results$df[2]
     R2  <- cur_results$r.squared
     other_stats <- c(df2, R2)
     names(other_stats) <- c("df2","R2")

     #Get beta
     sd_model <- apply(lm_data, FUN=sd, 2)
     sd_criterion  <- sd_model[1]
     sd_predictors <- sd_model[2:length(sd_model)]
     b_to_beta_multiplier <- sd_predictors/sd_criterion
     colnames_predictors <- names(b_values)

     is_intercept <- FALSE # Fix in original XYZZY
     if (!is.null(colnames_predictors)) {
          if (colnames_predictors[1]=="(Intercept)") {
               is_intercept <- TRUE
          }
     }


     if (is_intercept == TRUE) {
          b_values_to_convert <-  b_values[2:length(b_values)]
          initial_names <- names(b_values_to_convert)

          beta_values <- c(-999, b_values_to_convert*b_to_beta_multiplier)
          names(beta_values) <- c("(Intercept)", initial_names)
     } else {
          b_values_to_convert <- b_values
          beta_values <-  b_values_to_convert*b_to_beta_multiplier
     }


     names(t_values) <- paste("t_",names(t_values), sep="")
     names(b_values) <- paste("b_",names(b_values), sep="")
     names(beta_values) <- paste("beta_",names(beta_values), sep="")
     output <- c(t_values, b_values, beta_values, other_stats)

     return(output)
}



t_df_to_sr2_df <- function(t_data, other_data) {

     df2 <- other_data$df2
     R2  <- other_data$R2
     n_col <- dim(t_data)[2]
     t_data_names <- names(t_data)
     sr2_data_names <- gsub("t_","sr2_", t_data_names)


     t_to_sr2_multiplier <- (1-R2)/df2
     t_to_sr2_multiplier_matrix <- t_to_sr2_multiplier %o% rep(1,n_col)

     t_matrix <- as.matrix(t_data)
     sr2_matrix <- (t_matrix^2) * t_to_sr2_multiplier_matrix
     sr2_data <- as.data.frame(sr2_matrix)
     names(sr2_data) <- sr2_data_names
     return(sr2_data)
}

get_boot_data <- function(boot_results) {
     boot_data <- boot_results$t
     boot_column_names <- names(boot_results$t0)
     boot_data <- tibble::as_tibble(boot_data)
     names(boot_data) <- boot_column_names
     return(boot_data)
}

get_percentile_conf <- function(x, conf_level) {
     K <- length(x)
     lower_index <- round(  ((1-conf_level)/2) * K) + 1
     upper_index <- round(  ((1-(1-conf_level)/2)) * K)

     x_sorted <- sort(x)
     LL <- x_sorted[lower_index]
     UL <- x_sorted[upper_index]
     return(c(LL,UL))
}


get_boot_R2_CI <- function(boot_data, conf_level) {
     x <- boot_data$R2
     ci_values <- get_percentile_conf(x, conf_level=conf_level)
     return(ci_values)
}


get_boot_b_CI <- function(boot_data, cur_blk, conf_level) {
     predictor_names <- rownames(summary(cur_blk)$coefficients)
     b_names <- paste("b_", predictor_names, sep="")
     b_data <- boot_data[, b_names]

     matrix_out <- ci_matrix_from_data(cur_data = b_data, predictor_names = predictor_names, conf_level = conf_level)
     return(matrix_out)
}

ci_matrix_from_data <- function(cur_data, predictor_names, conf_level) {

     num_predictors <- length(predictor_names)
     # if (!is.null(dim(cur_data))) {
     #      num_weights <- dim(cur_data)[2]
     # } else {
     #      num_weights <- 1
     # }
     matrix_out <- matrix(NA, nrow=num_predictors, ncol=2)
     rownames(matrix_out) <- predictor_names
     colnames(matrix_out) <- c("LL","UL")
     for (i in 1:num_predictors) {
          cur_col <- unlist(cur_data[,i])
          cur_ci  <- get_percentile_conf(x = cur_col, conf_level=conf_level)
          matrix_out[i, 1] <- cur_ci[1]
          matrix_out[i, 2] <- cur_ci[2]
     }

     return(matrix_out)
}

get_boot_beta_CI <- function(boot_data, cur_blk, conf_level) {
     lm_summary <- summary(cur_blk)
     predictor_names <- rownames(lm_summary$coefficients)
     beta_names <- paste("beta_", predictor_names, sep="")
     beta_data <- boot_data[, beta_names]

     matrix_out <- ci_matrix_from_data(cur_data = beta_data, predictor_names = predictor_names, conf_level = conf_level)
     return(matrix_out)
}


get_boot_sr2_CI <- function(boot_data, cur_blk, conf_level) {
     lm_summary <- summary(cur_blk)
     predictor_names <- rownames(lm_summary$coefficients)
     t_names <- paste("t_", predictor_names, sep="")
     t_data <- boot_data[, t_names]
     sr2_data <- t_df_to_sr2_df(t_data, boot_data)
     matrix_out <- ci_matrix_from_data(cur_data = sr2_data, predictor_names = predictor_names, conf_level = conf_level)
     return(matrix_out)
}


trim_intercept_row <- function(matrix_in) {
     cur_rownames <- rownames(matrix_in)
     id <- cur_rownames != "(Intercept)"
     matrix_out <- matrix_in[id,]
     return(matrix_out)
}



boot_blocks <- function(data, indices, regression_results_list){
  data2 <- data[indices, ] # select obs. in bootstrap sample
  L <- length(regression_results_list)

  R2_vec <- rep(NA,L)
  for (i in 1:L) {
    orig_reg_result <- regression_results_list[[i]]
    cur_reg_result  <- lm(formula = orig_reg_result$call$formula, data=data2)
    R2_vec[i] <- summary(cur_reg_result)$r.squared
  }
  return(R2_vec)
}


calculate_boot_delta_R2 <- function(matrix_in){
     matrix_dims <- dim(matrix_in)
     number_blocks <- matrix_dims[2]
     K <- matrix_dims[1]

     delta_R2_data <- matrix(NA, nrow=K, ncol=number_blocks)

     for (i in 2:number_blocks) {
          cur_R2 <- matrix_in[,i]
          prev_R2 <- matrix_in[,(i-1)]
          delta_value <- cur_R2 - prev_R2
          delta_R2_data[,i] <- delta_value
     }
     return(delta_R2_data)
}


get_delta_R2_CI <- function(delta_R2_data, conf_level) {
     number_blocks <- dim(delta_R2_data)[2]
     block_names <- paste("block",1:number_blocks,sep="")
     colnames(delta_R2_data) <- block_names

     matrix_out <- ci_matrix_from_data(cur_data = delta_R2_data, predictor_names = block_names, conf_level = conf_level)
     return(matrix_out)
}


update_with_delta_R2_boot_CI <- function(delta_R2_details, delta_R2_CI, prop_var_conf_level) {
     pvalue <- delta_R2_details$deltaR2_pvalue
     deltaR2 <- delta_R2_details$deltaR2
     is_sig <- FALSE
     if (pvalue<.05) {
          is_sig <- TRUE
     }

     LL <- delta_R2_CI[1]
     UL <- delta_R2_CI[2]


     #correction from literature
     if (is_sig == FALSE) {
          LL <- 0
          cat("     Delta R2 CI lower bound adjusted to zero as per Algina, Keselman, & Penfield (2008)\n\n")
     }
     prop_var_conf_level_str <- sprintf("%g", round(prop_var_conf_level*100))

     delta_R2_details$deltaR2_CI_txt<- sprintf("%s%% CI[%s, %s]", prop_var_conf_level_str, strip.leading.zero(sprintf("%1.2f",LL)), strip.leading.zero(sprintf("%1.2f",UL)))
     delta_R2_details$deltaR2_CI_rtf<- sprintf("{%s%% CI}[%s, %s]",prop_var_conf_level_str, strip.leading.zero(sprintf("%1.2f",LL)), strip.leading.zero(sprintf("%1.2f",UL)))

     return(delta_R2_details)
}


