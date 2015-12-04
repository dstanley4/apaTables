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
     
     if (is.na(filename)) {
          make.file.flag <- FALSE
     } else {
          make.file.flag <-TRUE
     }

     df.col <- dim(data)[2]
     column.is.numeric <- c()
     for (i in 1:df.col) {
          column.is.numeric[i] <- is.numeric(data[,i])
     }
     data <- data[,column.is.numeric]
     
     
     number.variables <- ncol(data)
     number.columns <- number.variables -1
     
     output.cor <- matrix(" ",number.variables,number.columns)
     output.cor.rtf <- matrix(" ",number.variables,number.columns)
     
     output.ci <- matrix(" ",number.variables,number.columns)
     output.ci.rtf <- matrix(" ",number.variables,number.columns)
     
     output.descriptives <- matrix(" ",number.variables,2)
     output.variable.names <- paste(as.character(1:number.variables),". ",names(data),sep="")

          
          
     variable.means <- c()
     variable.sds <- c()
     for (i in 1:number.variables) {
          output.descriptives[i,1]=txt.number(mean(data[,i],na.rm=TRUE))
          output.descriptives[i,2]=txt.number(sd(data[,i],na.rm=TRUE))
          for(j in 1:number.variables) {
               if ((j<i)) {
                    x=data[,i]
                    y=data[,j]
                    ctest <- cor.test(x,y)
                    cor.string <- txt.r(ctest)
                    output.cor[i,j] <- cor.string
                    output.cor.rtf[i,j] <- cor.string
                    
                    cor.ci.string <- txt.ci(ctest)
                    output.ci[i,j]<-cor.ci.string
                    output.ci.rtf[i,j] <- paste("{\\fs20",cor.ci.string,"}",sep="")
               } #end lower triangle      
          }#end j
     }#end i
     

     #weave
     left.padding <- c(" ", " ", " ")
     first.line <- c(output.variable.names[1],output.descriptives[1,], output.cor[1,])
     first.line.rtf <- c(output.variable.names[1],output.descriptives[1,], output.cor.rtf[1,])
     
     second.line <- c(left.padding, output.ci[1,])
     second.line.rtf <- c(left.padding, output.ci.rtf[1,])
     
     third.line <- rep(" ", length(second.line))
     
     
     output.matrix.console <- rbind(first.line,second.line)
     output.matrix.rtf <- rbind(first.line.rtf,second.line.rtf)
     for (i in 2:number.variables) {
          first.line <- c(output.variable.names[i], output.descriptives[i,], output.cor[i,])
          first.line.rtf <- c(output.variable.names[i], output.descriptives[i,], output.cor.rtf[i,])
          
          second.line <- c(left.padding, output.ci[i,])
          second.line.rtf <- c(left.padding, output.ci.rtf[i,])
          
          third.line <- rep(" ", length(second.line))
          
          if (show.conf.interval==TRUE) {
               new.lines <- rbind(first.line,second.line,third.line)
               new.lines <- rbind(first.line,second.line,third.line)
               new.lines.rtf <- rbind(first.line.rtf,second.line.rtf,third.line)
          } else {
               new.lines <- rbind(first.line,third.line)
               new.lines.rtf <- rbind(first.line.rtf,third.line)
          }
          
          output.matrix.console <- rbind(output.matrix.console, new.lines)
          output.matrix.rtf <- rbind(output.matrix.rtf, new.lines.rtf)
     }

     
     
     rownames(output.matrix.console) <- 1:nrow(output.matrix.console)
     colnames(output.matrix.console) <- c(c("Variable","M","SD"),as.character(1:number.columns))
     rownames(output.matrix.rtf) <- rownames(output.matrix.console)
     colnames(output.matrix.rtf) <- colnames(output.matrix.console)

     #done making input
     #now two matrices exist outputMatrixConsole and outputMatrixRTF that need to be printed
     #r
     
     
     
     

     
     if (show.conf.interval==TRUE) {
          table.title <- "Means, standard deviations, and correlations with confidence intervals\n"
     } else {
          table.title <- "Means, standard deviations, and correlations\n"
     }
     
     #make table    
     row.with.colnames <- colnames(output.matrix.console)
     #outputMatrixConsole <- rbind(row.with.colnames,outputMatrixConsole)
     df.temp <- data.frame(output.matrix.console, stringsAsFactors = FALSE)
     #write.table(df.temp,row.names=FALSE,col.names=FALSE,quote=FALSE)
     rownames(output.matrix.console) <- rep(" ",length((rownames(output.matrix.console))))
     table.body <- output.matrix.console
     
     #make console output
     if (show.conf.interval==TRUE) {
          table.note <- "Note. * indicates p < .05; ** indicates p < .01.\nM and SD are used to represent mean and standard deviation, respectively.\nValues in square brackets indicate the 95% confidence interval for each correlation.\nThe confidence interval is a plausible range of population correlations \nthat could have caused the sample correlation (Cumming, 2014).\n"
     } else {
          table.note <- "Note. * indicates p < .05; ** indicates p < .01.\nM and SD are used to represent mean and standard deviation, respectively.\n"
     }
     tbl.console <- list(table.number = table.number,
                 table.title = table.title,
                 table.body = table.body,
                 table.note = table.note)
     class(tbl.console) <- "apa.table"
     
     
     #make RTF output file
     if (make.file.flag==TRUE) {
          colnames(output.matrix.rtf) <- c(c("Variable","{\\i M}","{\\i SD}"),as.character(1:number.columns))
          #add leading blank line on table
          number.columns <- dim(output.matrix.rtf)[2]
          blankLine <- rep("",number.columns)
          output.matrix.rtf <- rbind(blankLine,output.matrix.rtf)
     
          if (show.conf.interval==TRUE) {
               table.title <- "Means, standard deviations, and correlations with confidence intervals"
               table.note <- "* indicates {\\i p} < .05; ** indicates {\\i p} < .01. {\\i M} and {\\i SD} are used to represent mean and standard deviation, respectively. Values in square brackets indicate the 95% confidence interval for each correlation. The confidence interval is a plausible range of population correlations that could have caused the sample correlation (Cumming, 2014)."
               
          } else {
               table.title <- "Means, standard deviations, and correlations"
               table.note <- "* indicates {\\i p} < .05; ** indicates {\\i p} < .01. {\\i M} and {\\i SD} are used to represent mean and standard deviation, respectively."
          }

          #Create RTF code
          rtfTable <- RtfTable$new(isHeaderRow=TRUE)
          rtfTable$setTableContent(output.matrix.rtf)
          rtfTable$setRowFirstColumnJustification("left")
          txt.body <- rtfTable$getTableAsRTF(FALSE,FALSE)
#           if (number.columns>5) {
#                #print("landscape")
               write.rtf.table(filename = filename,txt.body = txt.body,table.title = table.title, table.note = table.note, landscape=landscape,table.number=table.number)
#           } else {
#                #print("portrait")
#                write.rtf.table(filename = filename,txt.body = txt.body,table.title = table.title, table.note = table.note,table.number=table.number)
#           }
     }
     return(tbl.console)     
}#end function




txt.d <- function() {
     
}
