#' Creates a correlation table in APA style with means and standard deviations
#' @param data Project data frame
#' @param filename (optional) Output filename document filename (must end in .rtf or .doc only)
#' @param table.number  Integer to use in table number output line
#' @param show.conf.interval  (TRUE/FALSE) Display confidence intervals in table. This argument is deprecated and will be removed from later versions.
#' @param show.sig.stars  (TRUE/FALSE) Display stars for significance in table.
#' @param show.pvalue  (TRUE/FALSE) Display p-value in table.
#' @param landscape (TRUE/FALSE) Make RTF file landscape
#' @return APA table object
#' @examples
#' # View top few rows of attitude data set
#' head(attitude)
#'
#' # Use apa.cor.table function
#' table1 <- apa.cor.table(attitude)
#'
#'
#' # Save Table 1 in a .doc document
#' apa.save(filename = "table1.doc", table1)
#'
#'
#' # Create a table for your PDF
#' # Include the lines below in your rmarkdown or Quarto document
#' apa.knit.table.for.pdf(table1)
#'
#' # delete demo file
#' if (file.exists("table1.doc")) {
#'      file.remove("table1.doc")
#' }
#' @export
apa.cor.table<-function(data, filename = NA, table.number = 0, show.conf.interval = TRUE, show.sig.stars = TRUE, show.pvalue = TRUE, landscape = TRUE) {

     # test git tower April 23
     data <- as.data.frame(data)
     table_number <- table.number

     show_conf_interval <- show.conf.interval
     show_stars <- show.sig.stars
     show_pvalue <- show.pvalue

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
     output_cor_latex <- matrix(" ", number_variables, number_columns)

     output_pvalue     <- matrix(" ", number_variables, number_columns)
     output_pvalue_rtf <- matrix(" ", number_variables, number_columns)
     output_pvalue_latex <- matrix(" ", number_variables, number_columns)


     output_ci     <- matrix(" ", number_variables, number_columns)
     output_ci_rtf <- matrix(" ", number_variables, number_columns)
     output_ci_latex <- matrix(" ", number_variables, number_columns)

     output_descriptives   <- matrix(" ",number_variables,3)
     output_variable_names <- paste(as.character(1:number_variables),". ",names(data),sep="")

     for (i in 1:number_variables) {
          output_descriptives[i,1] <- as.character(sum(!is.na(data[,i]), na.rm=TRUE) )
          output_descriptives[i,2] <- txt.number(mean(data[,i], na.rm=TRUE))
          output_descriptives[i,3] <- txt.number(sd(data[,i], na.rm=TRUE))

          for(j in 1:number_variables) {
               if ((j<i)) {
                    x <- data[,i]
                    y <- data[,j]
                    ctest      <- cor.test(x, y)

                    cor_string <- txt.r(ctest, show_stars)
                    output_cor[i,j]        <- cor_string
                    output_cor_rtf[i,j]    <- cor_string
                    output_cor_latex[i,j]    <- txt.r.latex(ctest, show_stars)

                    output_pvalue[i,j]       <- txt.r.p.console(ctest)
                    output_pvalue_rtf[i,j]   <- paste("{\\fs20",txt.r.p.rtf(ctest),"}",sep="")
                    output_pvalue_latex[i,j] <- txt.r.p.latex(ctest)

                    cor_ci_string  <- txt.ci(ctest)
                    output_ci[i,j] <- cor_ci_string
                    output_ci_rtf[i,j] <- paste("{\\fs20",cor_ci_string,"}",sep="")
                    output_ci_latex[i,j] <- cor_ci_string

               } #end lower triangle
          }#end j
     }#end i


     #weave
     left_padding   <- c(" ", " ", " "," ")
     first_line     <- c(output_variable_names[1], output_descriptives[1,], output_cor[1,])
     first_line_rtf <- c(output_variable_names[1], output_descriptives[1,], output_cor_rtf[1,])
     first_line_latex     <- c(output_variable_names[1], output_descriptives[1,], output_cor[1,])

     second_line     <- c(left_padding, output_ci[1,])
     second_line_rtf <- c(left_padding, output_ci_rtf[1,])
     second_line_latex    <- c(left_padding, output_ci[1,])

     extra_line <- c(left_padding, output_pvalue[1,])
     extra_line_rtf <- c(left_padding, output_pvalue_rtf[1,])
     extra_line_latex <- c(left_padding, output_pvalue[1,])

     third_line <- rep(" ", length(second_line))


     new_lines <- first_line
     new_lines_rtf <- first_line_rtf
     new_lines_latex <- first_line_latex

     if (show_conf_interval==TRUE) {
          new_lines     <- rbind(new_lines, second_line)
          new_lines_rtf <- rbind(new_lines_rtf, second_line_rtf)
          new_lines_latex <- rbind(new_lines_latex, second_line_latex)
     }

     if (show_pvalue==TRUE) {
          new_lines     <- rbind(new_lines, extra_line)
          new_lines_rtf <- rbind(new_lines_rtf, extra_line_rtf)
          new_lines_latex <- rbind(new_lines_latex, extra_line_latex)
     }

     new_lines       <- rbind(new_lines, third_line)
     new_lines_rtf   <- rbind(new_lines_rtf, third_line)
     new_lines_latex <- rbind(new_lines_latex, third_line)

     output_matrix_console <- new_lines
     output_matrix_rtf <-  new_lines_rtf
     output_matrix_latex <-  new_lines_latex

     # output_matrix_console <- rbind(first_line, second_line)
     # output_matrix_rtf     <- rbind(first_line_rtf, second_line_rtf)
     for (i in 2:number_variables) {
          first_line <- c(output_variable_names[i], output_descriptives[i,], output_cor[i,])
          first_line_rtf <- c(output_variable_names[i], output_descriptives[i,], output_cor_rtf[i,])
          first_line_latex <- c(output_variable_names[i], output_descriptives[i,], output_cor_latex[i,])

          second_line <- c(left_padding, output_ci[i,])
          second_line_rtf <- c(left_padding, output_ci_rtf[i,])
          second_line_latex <- c(left_padding, output_ci_latex[i,])

          extra_line <- c(left_padding, output_pvalue[i,])
          extra_line_rtf <- c(left_padding, output_pvalue_rtf[i,])
          extra_line_latex <- c(left_padding, output_pvalue_latex[i,])

          third_line <- rep(" ", length(second_line))

          new_lines <- first_line
          new_lines_rtf <- first_line_rtf
          new_lines_latex <- first_line_latex
          if (show_conf_interval==TRUE) {
               new_lines     <- rbind(new_lines, second_line)
               new_lines_rtf <- rbind(new_lines_rtf, second_line_rtf)
               new_lines_latex     <- rbind(new_lines_latex, second_line_latex)
          }

          if (show_pvalue==TRUE) {
               new_lines     <- rbind(new_lines, extra_line)
               new_lines_rtf <- rbind(new_lines_rtf, extra_line_rtf)
               new_lines_latex     <- rbind(new_lines_latex, extra_line_latex)
          }

          new_lines     <- rbind(new_lines, third_line)
          new_lines_rtf <- rbind(new_lines_rtf, third_line)
          new_lines_latex     <- rbind(new_lines_latex, third_line)

          # if (show_conf_interval==TRUE) {
          #      new_lines     <- rbind(first_line, second_line, third_line)
          #      new_lines     <- rbind(first_line, second_line, third_line)
          #      new_lines_rtf <- rbind(first_line_rtf, second_line_rtf, third_line)
          # } else {
          #      new_lines     <- rbind(first_line, third_line)
          #      new_lines_rtf <- rbind(first_line_rtf, third_line)
          # }

          output_matrix_console <- rbind(output_matrix_console, new_lines)
          output_matrix_latex <- rbind(output_matrix_latex, new_lines_latex)
          output_matrix_rtf <- rbind(output_matrix_rtf, new_lines_rtf)
     }



     rownames(output_matrix_console) <- 1:nrow(output_matrix_console)
     colnames(output_matrix_console) <- c(c("Variable","N", "M","SD"), as.character(1:number_columns))

     rownames(output_matrix_latex) <- rownames(output_matrix_console)
     colnames(output_matrix_latex) <- colnames(output_matrix_console)

     rownames(output_matrix_rtf) <- rownames(output_matrix_console)
     colnames(output_matrix_rtf) <- colnames(output_matrix_console)
     #done making input
     #now two matrices exist outputMatrixConsole and outputMatrixRTF that need to be printed


     table_title <- "Descriptive Statistics and Correlations\n"


     #make table
     row_with_colnames <- colnames(output_matrix_console)
     df_temp <- data.frame(output_matrix_console, stringsAsFactors = FALSE)
     rownames(output_matrix_console) <- rep(" ", length((rownames(output_matrix_console))))
     table_body <- output_matrix_console

     #make console output
     if (show_conf_interval == TRUE) {
          table_note <- "Note. N = number of cases. M = mean. SD = standard deviation.\nValues in square brackets indicate the 95% confidence interval.\n"
     } else {
          table_note <- "Note. N = number of cases. M = mean. SD = standard deviation.\n"
     }

     if (show_stars == TRUE) {
             table_note <- paste(table_note, "* indicates p < .05. ** indicates p < .01.\n")
     }

     tbl.console <- list(table.number = table_number,
                 table.title = table_title,
                 table.body = table_body,
                 table.note = table_note)
     class(tbl.console) <- "apa.table"


     #make RTF output file

     colnames(output_matrix_rtf) <- c(c("Variable","{\\i N}","{\\i M}","{\\i SD}"),as.character(1:number_columns))
     #add leading blank line on table
     number_columns <- dim(output_matrix_rtf)[2]
     blankLine <- rep("",number_columns)
     output_matrix_rtf <- rbind(blankLine,output_matrix_rtf)

     if (show_conf_interval == TRUE) {
          table_title <- "Descriptive Statistics and Correlations"
          table_note <- "{\\i N} = number of cases. {\\i M} = mean. {\\i SD} = standard deviation. Square brackets = 95% confidence interval."
          table_note_latex <- "\\\\textit{Note}. \\\\textit{N} = number of cases. \\\\textit{M} = mean. \\\\textit{SD} = standard deviation. Square brackets = 95\\\\% confidence interval."


     } else {
          table_title <- "Descriptive Statistics and Correlations"
          table_note <- "{\\i N} = number of cases. {\\i M} = mean. {\\i SD} = standard deviation."
          table_note_latex <- "\\\\textit{Note}. \\\\textit{N} = number of cases. \\\\textit{M} = mean. \\\\textit{SD} = standard deviation."

     }

     if (show_stars == TRUE) {
             table_note_rtf <- paste(table_note, "\\line * indicates {\\i p} < .05. ** indicates {\\i p} < .01.")
             table_note_latex <- paste(table_note_latex, "\\\\newline  * indicates $p$ < .05. ** indicates $p$ < .01.")
             table_note <- paste(table_note, "\n* indicates {\\i p} < .05. ** indicates {\\i p} < .01.")
     }


     #Create RTF code
     rtfTable <- RtfTable$new(isHeaderRow=TRUE)
     rtfTable$setTableContent(output_matrix_rtf)
     rtfTable$setRowFirstColumnJustification("left")
          txt_body <- rtfTable$getTableAsRTF(FALSE,FALSE)

     if (make_file_flag == TRUE) {
          write.rtf.table(filename = filename,
                          txt.body = txt_body,
                          table.title = table_title,
                          table.note = table_note_rtf,
                          landscape=landscape,
                          table.number=table_number)
     }


     tbl.console$rtf.body         <- txt_body
     tbl.console$rtf.table.title  <- table_title
     tbl.console$rtf.table.note   <- table_note_rtf
     tbl.console$landscape   <- landscape



     output_matrix_latex[,1] <- gsub("_", " ", output_matrix_latex[,1])

     tbl.console$latex.body <- output_matrix_latex
     tbl.console$latex.table.title <- table_title
     tbl.console$latex.table.note  <- table_note_latex
     tbl.console$table.type        <- "correlation"


     return(tbl.console)
}#end function

