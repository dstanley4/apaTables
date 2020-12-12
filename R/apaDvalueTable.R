#' Creates a d-values for all paired comparisons in APA style
#' @param iv Name of independent variable column in data frame for all paired comparisons
#' @param dv Name of dependent variable column in data frame for all paired comparisons
#' @param data Project data frame name
#' @param filename (optional) Output filename document filename (must end in .rtf or .doc only)
#' @param table.number Integer to use in table number output line
#' @param show.conf.interval  (TRUE/FALSE) Display confidence intervals in table. This argument is deprecated and will be removed from later versions.
#' @param landscape (TRUE/FALSE) Make RTF file landscape
#' @return APA table object
#' @examples
#' \dontrun{
#' # View top few rows of viagra data set from Discovering Statistics Using R
#' head(viagra)
#'
#' # Use apa.d.table function
#' apa.d.table(iv = dose, dv = libido, data = viagra, filename = "ex1_d_table.doc")
#' }
#' @export
apa.d.table <- function(iv, dv, data, filename=NA, table.number=NA, show.conf.interval = TRUE, landscape=TRUE){
     data <- as.data.frame(data)
     table_number <- table.number


     if (show.conf.interval==FALSE) {
          cat("The ability to suppress reporting of reporting confidence intervals has been deprecated in this version.\nThe function arguement show.conf.interval will be removed in a later version.\n")
     }

     if (requireNamespace("MBESS", quietly = TRUE)) {
          show.conf.interval = TRUE
     } else {
          show.conf.interval = FALSE
     }

     show_conf_interval <- show.conf.interval

     if (is.na(filename)) {
          make_file_flag=FALSE
     } else {
          make_file_flag=TRUE
     }

     if (!is.null(data)) {
          data_col_names <- colnames(data)
     } else {
          cat("apa.mean.table error:\n")
          cat("data not specified.\n\n")
          return(FALSE)
     }

     iv_sub <- substitute(iv)
     is_iv <- is.valid.name(iv_sub, data_col_names)

     dv_sub <- substitute(dv)
     is_dv  <- is.valid.name(dv_sub, data_col_names)


     if (is_dv==FALSE) {
          cat("apa.mean.table error:\n")
          cat("A valid dependent variable (dv) must be specified.\n")
          return(FALSE)
     }

     if (is_iv==FALSE) {
          cat("apa.mean.table error:\n")
          cat("Two valid independent variables (iv's) must be specified.\n\n")
          return(FALSE)
     }
     iv_name <- deparse(iv_sub)
     dv_name <- deparse(dv_sub)
     iv <- as.factor(data[,iv_name])
     dv <- data[,dv_name]

     #create table
     iv_levels <- levels(iv)
     iv_num_levels  <- length(iv_levels)
     number_columns <- iv_num_levels -1

     output_d     <- matrix(" ",iv_num_levels, number_columns)
     output_d_rtf <- matrix(" ",iv_num_levels, number_columns)
     output_d_ci     <- matrix(" ",iv_num_levels, number_columns)
     output_d_ci_rtf <- matrix(" ",iv_num_levels, number_columns)

     output_row_descriptives <- matrix(" ",iv_num_levels, ncol=2)
     for (c_row in 1:iv_num_levels) {
          for (c_col in 1:iv_num_levels) {
               if (c_col<c_row) {
                    group1_id <- iv == iv_levels[c_row]
                    group2_id <- iv == iv_levels[c_col]
                    group1_data <- na.omit(dv[group1_id])
                    group2_data <- na.omit(dv[group2_id])
                    group1_n <- length(group1_data)
                    group2_n <- length(group2_data)
                    group1_mean <- sprintf("%1.2f",mean(group1_data))
                    group1_sd <- sprintf("%1.2f",sd(group1_data))

                    output_row_descriptives[c_row,1] <- group1_mean
                    output_row_descriptives[c_row,2] <- group1_sd

                    cur_d_value <- get_d_value(group1_data, group2_data)

                    if (requireNamespace("MBESS", quietly = TRUE) == TRUE) {
                         cur_d_ci <- MBESS::ci.smd(smd=cur_d_value, n.1=group1_n, n.2=group2_n)
                    } else {
                         cur_d_ci <- list()
                         cur_d_ci$Lower.Conf.Limit.smd <- (-99)
                         cur_d_ci$Upper.Conf.Limit.smd <- (-99)
                    }

                    my_t_test_results <- t.test(x=group1_data, y=group2_data, alternative="two.sided", var.equal = TRUE)
                    cur_t_p_value <- my_t_test_results$p.value

                    d_string <- txt_d(cur_d_value, cur_t_p_value)
                    d.ci.string <- txt_d_ci(cur_d_ci)
                    if (is_equal_var(group1_data,group2_data) == FALSE) {
                         d_string <- "-"
                         d.ci.string <- "        [-,-]"
                         output_d[c_row,c_col] <- d_string
                         output_d_rtf[c_row,c_col] <- d_string
                         output_d_ci[c_row,c_col]<-"[-,-]"
                         output_d_ci_rtf[c_row,c_col] <- paste("{\\fs20",d.ci.string,"}", sep="")
                    } else {
                         output_d[c_row,c_col] <- d_string
                         output_d_rtf[c_row,c_col] <- d_string
                         output_d_ci[c_row,c_col]<-d.ci.string
                         output_d_ci_rtf[c_row,c_col] <- paste("{\\fs20",d.ci.string,"}", sep="")
                    }


               }
          }
     }
     group1_id <- iv == iv_levels[1]
     group1_data <- na.omit(dv[group1_id])
     group1_mean <- sprintf("%1.2f", mean(group1_data))
     group1_sd   <- sprintf("%1.2f", sd(group1_data))
     output_row_descriptives[1,] <- c(group1_mean, group1_sd)


     #weave
     iv_levels_numbers <- 1:iv_num_levels
     iv_levels_periods <- rep(". ", iv_num_levels)
     iv_levels <- paste(iv_levels_numbers, iv_levels_periods, iv_levels,sep="")

     left_padding <- c(" ", " ", " ")
     first_line <- c(iv_levels[1], output_row_descriptives[1,], output_d[1,])
     first_line_rtf <- c(iv_levels[1], output_row_descriptives[1,], output_d_rtf[1,])
     second_line <- c(left_padding, output_d_ci[1,])
     second_line_rtf <- c(left_padding, output_d_ci_rtf[1,])
     third_line <- rep(" ", length(second_line))

     output_matrix_console <- rbind(first_line,second_line)
     output_matrix_rtf <- rbind(first_line_rtf,second_line_rtf)
     for (i in 2:iv_num_levels) {
          first_line <- c(iv_levels[i], output_row_descriptives[i,], output_d[i,])
          first_line_rtf <- c(iv_levels[i], output_row_descriptives[i,], output_d_rtf[i,])

          second_line <- c(left_padding, output_d_ci[i,])
          second_line_rtf <- c(left_padding, output_d_ci_rtf[i,])

          third_line <- rep(" ", length(second_line))

          if (show_conf_interval==TRUE) {
               new_lines     <- rbind(first_line,second_line,third_line)
               new_lines     <- rbind(first_line,second_line,third_line)
               new_lines_rtf <- rbind(first_line_rtf,second_line_rtf,third_line)
          } else {
               new_lines     <- rbind(first_line,third_line)
               new_lines_rtf <- rbind(first_line_rtf,third_line)
          }

          output_matrix_console <- rbind(output_matrix_console, new_lines)
          output_matrix_rtf     <- rbind(output_matrix_rtf, new_lines_rtf)
     }
     rownames(output_matrix_console) <- 1:nrow(output_matrix_console)
     colnames(output_matrix_console) <- c(c("Variable","M","SD"),as.character(1:number_columns))
     rownames(output_matrix_rtf) <- rownames(output_matrix_console)
     colnames(output_matrix_rtf) <- colnames(output_matrix_console)


     if (show_conf_interval==TRUE) {
          table_title <- "Means, standard deviations, and d-values with confidence intervals\n"
     } else {
          table_title <- "Means, standard deviations, and d-values\n"
     }

     #make table
     row_with_colnames <- colnames(output_matrix_console)
     df_temp <- data.frame(output_matrix_console, stringsAsFactors = FALSE)
     rownames(output_matrix_console) <- rep(" ", length((rownames(output_matrix_console))))
     table_body <- output_matrix_console

     #make console output
     if (show_conf_interval==TRUE) {
          table_note <- "Note. M indicates mean. SD indicates standard deviation. d-values are estimates calculated using formulas 4.18 and 4.19\nfrom Borenstein, Hedges, Higgins, & Rothstein (2009). d-values not calculated if unequal variances prevented pooling.\nValues in square brackets indicate the 95% confidence interval for each d-value. \nThe confidence interval is a plausible range of population d-values \nthat could have caused the sample d-value (Cumming, 2014). \n"
     } else {
          table_note <- "Note. M indicates mean. SD indicates standard deviation. d-values are estimates calculated using formulas 4.18 and 4.19\nfrom Borenstein, Hedges, Higgins, & Rothstein (2009). d-values not calculated if unequal variances prevented pooling.\n"
     }
     tbl_console <- list(table.number = table_number,
                         table.title = table_title,
                         table.body = table_body,
                         table.note = table_note)
     class(tbl_console) <- "apa.table"


     #make RTF output file
     if (make_file_flag==TRUE) {
          colnames(output_matrix_rtf) <- c(c("Variable","{\\i M}","{\\i SD}"),as.character(1:number_columns))
          #add leading blank line on table
          number_columns <- dim(output_matrix_rtf)[2]
          blankLine <- rep("",number_columns)
          output_matrix_rtf <- rbind(blankLine,output_matrix_rtf)

          if (show_conf_interval==TRUE) {
               table_title <- "Means, standard deviations, and d-values with confidence intervals"
               table_note <- "{\\i M} indicates mean. {\\i SD} indicates standard deviation. {\\i d}-values are estimates calculated using formulas 4.18 and 4.19 from Borenstein, Hedges, Higgins, & Rothstein (2009). {\\i d}-values not calculated if unequal variances prevented pooling. Values in square brackets indicate the 95% confidence interval for each {\\i d}-value The confidence interval is a plausible range of population {\\i d}-values that could have caused the sample {\\i d}-value (Cumming, 2014)."

          } else {
               table_title <- "Means, standard deviations, and d-values"
               table_note <- "{\\i M} indicates mean. {\\i SD} indicates standard deviation. {\\i d}-values are estimates calculated using formulas 4.18 and 4.19 from Borenstein, Hedges, Higgins, & Rothstein (2009). {\\i d}-values not calculated if unequal variances prevented pooling."
          }

          #Create RTF code
          rtfTable <- RtfTable$new(isHeaderRow=TRUE)
          rtfTable$setTableContent(output_matrix_rtf)
          cell_widths_in_inches <-c(1.25,.85,.85,rep(1,iv_num_levels-1))
          rtfTable$setCellWidthsInches(cell_widths_in_inches)
          rtfTable$setRowFirstColumnJustification("left")
          txt.body <- rtfTable$getTableAsRTF(FALSE,FALSE)
          write.rtf.table(filename = filename,txt.body = txt.body,table.title = table_title, table.note = table_note, landscape=landscape,table.number=table.number)
     }

     return(tbl_console)
}


txt_d <- function(d_in, p_in) {
     # omit significance testing in this version
     # if (p_in<.01) {
     #      star_str <- "**"
     # } else if (p_in<.05) {
     #      star_str <- "*"
     # } else {
          star_str <-""
     #}

     d_str <- sprintf("%1.2f%s",d_in, star_str)
     return(d_str)
}


txt_d_ci <- function(cur_ci) {
     my_low_lim  <- cur_ci$Lower.Conf.Limit.smd
     my_up_lim   <- cur_ci$Upper.Conf.Limit.smd
     ci_str <- sprintf("[%1.2f, %1.2f]",my_low_lim,my_up_lim)
     return(ci_str)
}


get_d_value <- function(group1_data,group2_data) {
     #confirm this is the unbiased one
     m1 <- mean(group1_data)
     m2 <- mean(group2_data)
     v1 <- var(group1_data)
     v2 <- var(group2_data)
     n1 <- length(group1_data)
     n2 <- length(group2_data)
     sp <- sqrt((v1*(n1-1) + v2*(n2-1))/(n1+n2-2))
     d_out <- (m1-m2)/sp
     d_out <- abs(d_out)
     return(d_out)
}



is_equal_var <- function(group1_dv,group2_dv) {
     iv_1 <- rep(1,length(group1_dv))
     iv_2 <- rep(2,length(group2_dv))

     iv <- c(iv_1,iv_2)
     dv <- c(group1_dv,group2_dv)

     my_df <- data.frame(iv,dv)
     my_df$iv <- as.factor(my_df$iv)

     lev_result <- car::leveneTest(dv~iv,center=median,data=my_df)
     lev_p <- lev_result$`Pr(>F)`[1]

     b_result <- TRUE
     if (lev_p<.05) {
          b_result <- FALSE
     }
     return(b_result)
}
