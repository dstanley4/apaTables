#' Creates a correlation table in APA style with means and standard deviations
#' @param data Project data frame
#' @param filename (optional) Output filename document filename (must end in .rtf or .doc only)
#' @param table.number  Integer to use in table number output line
#' @param show.conf.interval  (TRUE/FALSE) Display confidence intervals in table.
#' @param landscape (TRUE/FALSE) Make RTF file landscape
#' @return APA table object
#' @examples
#' # View top few rows of attitude data set
#' head(attitude)
#'
#' # Use apa.cor.table function
#' apa.cor.table(attitude)
#' apa.cor.table(attitude, show.conf.interval=FALSE)
#' apa.cor.table(attitude, filename="ex.CorTable1.doc")
#' apa.cor.table(attitude, show.conf.interval=FALSE, filename="ex.CorTable2.doc")
#' @export
apa.cor.table<-function(data,filename=NA,table.number=NA, show.conf.interval=TRUE,landscape=TRUE) {

     data <- as.data.frame(data)
     table_number <- table.number
     show_conf_interval <- show.conf.interval

     if (is.na(filename)) {
          make_file_flag <- FALSE
     } else {
          make_file_flag <-TRUE
     }

     df_col <- dim(data)[2]
     column_is_numeric <- c()
     for (i in 1:df_col) {
          column_is_numeric[i] <- is.numeric(data[,i])
     }
     data <- data[,column_is_numeric]


     number_variables <- ncol(data)
     number_columns   <- number_variables -1

     output_cor     <- matrix(" ", number_variables, number_columns)
     output_cor_rtf <- matrix(" ", number_variables, number_columns)

     output_ci     <- matrix(" ", number_variables, number_columns)
     output_ci_rtf <- matrix(" ", number_variables, number_columns)

     output_descriptives   <- matrix(" ",number_variables,2)
     output_variable_names <- paste(as.character(1:number_variables),". ",names(data),sep="")

     for (i in 1:number_variables) {
          output_descriptives[i,1] <- txt.number(mean(data[,i], na.rm=TRUE))
          output_descriptives[i,2] <- txt.number(sd(data[,i], na.rm=TRUE))
          for(j in 1:number_variables) {
               if ((j<i)) {
                    x <- data[,i]
                    y <- data[,j]
                    ctest      <- cor.test(x, y)
                    cor_string <- txt.r(ctest)
                    output_cor[i,j]     <- cor_string
                    output_cor_rtf[i,j] <- cor_string

                    cor_ci_string  <- txt.ci(ctest)
                    output_ci[i,j] <- cor_ci_string
                    output_ci_rtf[i,j] <- paste("{\\fs20",cor_ci_string,"}",sep="")
               } #end lower triangle
          }#end j
     }#end i


     #weave
     left_padding   <- c(" ", " ", " ")
     first_line     <- c(output_variable_names[1], output_descriptives[1,], output_cor[1,])
     first_line_rtf <- c(output_variable_names[1], output_descriptives[1,], output_cor_rtf[1,])

     second_line     <- c(left_padding, output_ci[1,])
     second_line_rtf <- c(left_padding, output_ci_rtf[1,])

     third_line <- rep(" ", length(second_line))


     output_matrix_console <- rbind(first_line, second_line)
     output_matrix_rtf     <- rbind(first_line_rtf, second_line_rtf)
     for (i in 2:number_variables) {
          first_line <- c(output_variable_names[i], output_descriptives[i,], output_cor[i,])
          first_line_rtf <- c(output_variable_names[i], output_descriptives[i,], output_cor_rtf[i,])

          second_line <- c(left_padding, output_ci[i,])
          second_line_rtf <- c(left_padding, output_ci_rtf[i,])

          third_line <- rep(" ", length(second_line))

          if (show_conf_interval==TRUE) {
               new_lines     <- rbind(first_line, second_line, third_line)
               new_lines     <- rbind(first_line, second_line, third_line)
               new_lines_rtf <- rbind(first_line_rtf, second_line_rtf, third_line)
          } else {
               new_lines     <- rbind(first_line, third_line)
               new_lines_rtf <- rbind(first_line_rtf, third_line)
          }

          output_matrix_console <- rbind(output_matrix_console, new_lines)
          output_matrix_rtf <- rbind(output_matrix_rtf, new_lines_rtf)
     }



     rownames(output_matrix_console) <- 1:nrow(output_matrix_console)
     colnames(output_matrix_console) <- c(c("Variable","M","SD"), as.character(1:number_columns))
     rownames(output_matrix_rtf) <- rownames(output_matrix_console)
     colnames(output_matrix_rtf) <- colnames(output_matrix_console)
     #done making input
     #now two matrices exist outputMatrixConsole and outputMatrixRTF that need to be printed


     if (show_conf_interval==TRUE) {
          table_title <- "Means, standard deviations, and correlations with confidence intervals\n"
     } else {
          table_title <- "Means, standard deviations, and correlations\n"
     }

     #make table
     row_with_colnames <- colnames(output_matrix_console)
     df_temp <- data.frame(output_matrix_console, stringsAsFactors = FALSE)
     rownames(output_matrix_console) <- rep(" ", length((rownames(output_matrix_console))))
     table_body <- output_matrix_console

     #make console output
     if (show_conf_interval == TRUE) {
          table_note <- "Note. * indicates p < .05; ** indicates p < .01.\nM and SD are used to represent mean and standard deviation, respectively.\nValues in square brackets indicate the 95% confidence interval.\nThe confidence interval is a plausible range of population correlations \nthat could have caused the sample correlation (Cumming, 2014).\n"
     } else {
          table_note <- "Note. * indicates p < .05; ** indicates p < .01.\nM and SD are used to represent mean and standard deviation, respectively.\n"
     }

     tbl.console <- list(table.number = table_number,
                 table.title = table_title,
                 table.body = table_body,
                 table.note = table_note)
     class(tbl.console) <- "apa.table"


     #make RTF output file
     if (make_file_flag == TRUE) {
          colnames(output_matrix_rtf) <- c(c("Variable","{\\i M}","{\\i SD}"),as.character(1:number_columns))
          #add leading blank line on table
          number_columns <- dim(output_matrix_rtf)[2]
          blankLine <- rep("",number_columns)
          output_matrix_rtf <- rbind(blankLine,output_matrix_rtf)

          if (show_conf_interval == TRUE) {
               table_title <- "Means, standard deviations, and correlations with confidence intervals"
               table_note <- "* indicates {\\i p} < .05; ** indicates {\\i p} < .01. {\\i M} and {\\i SD} are used to represent mean and standard deviation, respectively. Values in square brackets indicate the 95% confidence interval for each correlation. The confidence interval is a plausible range of population correlations that could have caused the sample correlation (Cumming, 2014)."

          } else {
               table_title <- "Means, standard deviations, and correlations"
               table_note <- "* indicates {\\i p} < .05; ** indicates {\\i p} < .01. {\\i M} and {\\i SD} are used to represent mean and standard deviation, respectively."
          }

          #Create RTF code
          rtfTable <- RtfTable$new(isHeaderRow=TRUE)
          rtfTable$setTableContent(output_matrix_rtf)
          rtfTable$setRowFirstColumnJustification("left")
          txt_body <- rtfTable$getTableAsRTF(FALSE,FALSE)
          write.rtf.table(filename = filename,
                          txt.body = txt_body,
                          table.title = table_title,
                          table.note = table_note,
                          landscape=landscape,
                          table.number=table_number)
     }
     return(tbl.console)
}#end function


#
#
# txt.d <- function() {
#
# }
