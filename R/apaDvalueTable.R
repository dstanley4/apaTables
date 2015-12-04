#' Creates a d-values for all paired comparisons in APA style
#' @param iv Name of independent variable column in data frame for all paired comparisons
#' @param dv Name of dependent variable column in data frame for all paired comparisons
#' @param data Project data frame name
#' @param filename (optional) Output filename document filename (must end in .rtf or .doc only)
#' @param table.number Integer to use in table number output line
#' @param show.conf.interval  (TRUE/FALSE) Display confidence intervals in table. 
#' @param landscape (TRUE/FALSE) Make RTF file landscape
#' @return APA table object
#' @examples
#' # View top few rows of viagra data set from Discovering Statistics Using R
#' head(viagra)
#' 
#' # Use apa.d.table function
#' apa.d.table(iv=dose,dv=libido,data=viagra,filename="ex.d.Table.doc")
#' @export
apa.d.table <- function(iv, dv, data, filename=NA, table.number=NA,show.conf.interval = TRUE, landscape=TRUE){
     
     if (is.na(filename)) {
          make.file.flag=FALSE
     } else {
          make.file.flag=TRUE
     }
     
     if (!is.null(data)) {
          data.col.names <- colnames(data)
     } else {
          cat("apa.mean.table error:\n")
          cat("data not specified.\n\n")
          return(FALSE)
     }
     
     iv.sub <- substitute(iv)
     is.iv <- is.valid.name(iv.sub,data.col.names)

     dv.sub <- substitute(dv)
     is.dv  <- is.valid.name(dv.sub, data.col.names)
     
     
     if (is.dv==FALSE) {
          cat("apa.mean.table error:\n")
          cat("A valid dependent variable (dv) must be specified.\n")
          return(FALSE)
     }
     
     if (is.iv==FALSE) {
          cat("apa.mean.table error:\n")
          cat("Two valid independent variables (iv's) must be specified.\n\n")
          return(FALSE)
     }
     iv.name <- deparse(iv.sub)
     dv.name <- deparse(dv.sub)
     iv <- as.factor(data[,iv.name])
     dv <- data[,dv.name]

     #create table
     iv.levels <- levels(iv)
     iv.num.levels <- length(iv.levels)
     number.columns <- iv.num.levels -1

     output.d <- matrix(" ",iv.num.levels,number.columns)
     output.d.rtf <- matrix(" ",iv.num.levels,number.columns)
     output.d.ci <- matrix(" ",iv.num.levels,number.columns)
     output.d.ci.rtf <- matrix(" ",iv.num.levels,number.columns)
     output.row.descriptives <- matrix(" ",iv.num.levels,ncol=2)
     output.variable.names <- paste(as.character(1:iv.num.levels),". ",names(data),sep="")
     for (c.row in 1:iv.num.levels) {
          for (c.col in 1:iv.num.levels) {
               if (c.col<c.row) {
                    group1.id <- iv == iv.levels[c.row]
                    group2.id <- iv == iv.levels[c.col]
                    group1.data <- na.omit(dv[group1.id])
                    group2.data <- na.omit(dv[group2.id])
                    group1.n <- length(group1.data)
                    group2.n <- length(group2.data)
                    group1.mean <- sprintf("%1.2f",mean(group1.data))
                    group1.sd <- sprintf("%1.2f",sd(group1.data))
#                     group2.mean <- sprintf("%1.2f",mean(group2.data))
#                     group2.sd <- sprintf("%1.2f",sd(group2.data))

                    output.row.descriptives[c.row,1] <- group1.mean
                    output.row.descriptives[c.row,2] <- group1.sd

                    cur.d.value <- get.d.value(group1.data,group2.data)
                    #cur.d.value <- MBESS::smd(Group.1=group1.data,Group.2=group2.data,Unbiased = TRUE)
                    cur.d.ci <- MBESS::ci.smd(smd=cur.d.value,n.1=group1.n,n.2=group2.n)
                    my.t.test.results <- t.test(x=group1.data,y=group2.data,alternative = "two.sided",var.equal = TRUE)
                    cur.t.p.value <- my.t.test.results$p.value
                    
                    d.string <- txt.d(cur.d.value,cur.t.p.value)
                    d.ci.string <- txt.d.ci(cur.d.ci)
                    if (is.equal.var(group1.data,group2.data)==FALSE) {
                         d.string <- "-"
                         d.ci.string <- "        [-,-]"
                         output.d[c.row,c.col] <- d.string
                         output.d.rtf[c.row,c.col] <- d.string
                         output.d.ci[c.row,c.col]<-"[-,-]"
                         output.d.ci.rtf[c.row,c.col] <- paste("{\\fs20",d.ci.string,"}",sep="")
                    } else {
                         output.d[c.row,c.col] <- d.string
                         output.d.rtf[c.row,c.col] <- d.string
                         output.d.ci[c.row,c.col]<-d.ci.string
                         output.d.ci.rtf[c.row,c.col] <- paste("{\\fs20",d.ci.string,"}",sep="")
                    }
                    

               } 
          }
     }
     group1.id <- iv == iv.levels[1]
     group1.data <- na.omit(dv[group1.id])
     group1.mean <- sprintf("%1.2f",mean(group1.data))
     group1.sd <- sprintf("%1.2f",sd(group1.data))
     output.row.descriptives[1,]<-c(group1.mean,group1.sd)
     
          
     #weave
     iv.levels.numbers <- 1:iv.num.levels
     iv.levels.periods <- rep(". ", iv.num.levels)
     iv.levels <- paste(iv.levels.numbers,iv.levels.periods,iv.levels,sep="")

     left.padding <- c(" ", " ", " ")
     first.line <- c(iv.levels[1],output.row.descriptives[1,], output.d[1,])
     first.line.rtf <- c(iv.levels[1],output.row.descriptives[1,], output.d.rtf[1,])
     second.line <- c(left.padding, output.d.ci[1,])
     second.line.rtf <- c(left.padding, output.d.ci.rtf[1,])
     third.line <- rep(" ", length(second.line))

     output.matrix.console <- rbind(first.line,second.line)
     output.matrix.rtf <- rbind(first.line.rtf,second.line.rtf)
     for (i in 2:iv.num.levels) {
          first.line <- c(iv.levels[i], output.row.descriptives[i,], output.d[i,])
          first.line.rtf <- c(iv.levels[i], output.row.descriptives[i,], output.d.rtf[i,])
          
          second.line <- c(left.padding, output.d.ci[i,])
          second.line.rtf <- c(left.padding, output.d.ci.rtf[i,])
          
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

     
     if (show.conf.interval==TRUE) {
          table.title <- "Means, standard deviations, and d-values with confidence intervals\n"
     } else {
          table.title <- "Means, standard deviations, and d-values\n"
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
          table.note <- "Note. * indicates p < .05; ** indicates p < .01.\nM and SD are used to represent mean and standard deviation, respectively.\nValues in square brackets indicate the 95% confidence interval for each d-value. \nThe confidence interval is a plausible range of population d-values \nthat could have caused the sample d-value (Cumming, 2014). \nd-values are unbiased estimates calculated using formulas 4.18 and 4.19 \nfrom Borenstein, Hedges, Higgins, & Rothstein (2009). \nd-values not calculated if unequal variances prevented pooling.\n"
     } else {
          table.note <- "Note. * indicates p < .05; ** indicates p < .01.\nM and SD are used to represent mean and standard deviation, respectively.\nd-values are unbiased estimates calculated using formulas 4.18 and 4.19 \nfrom Borenstein, Hedges, Higgins, & Rothstein (2009). \nd-values not calculated if unequal variances prevented pooling.\n"
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
               table.title <- "Means, standard deviations, and d-values with confidence intervals"
               table.note <- "* indicates {\\i p} < .05; ** indicates {\\i p} < .01. {\\i M} and {\\i SD} are used to represent mean and standard deviation, respectively. Values in square brackets indicate the 95% confidence interval for each {\\i d}-value The confidence interval is a plausible range of population {\\i d}-values that could have caused the sample {\\i d}-value (Cumming, 2014). {\\i d}-values are unbiased estimates calculated using formulas 4.18 and 4.19 from Borenstein, Hedges, Higgins, & Rothstein (2009).  {\\i d}-values not calculated if unequal variances prevented pooling."
               
          } else {
               table.title <- "Means, standard deviations, and d-values"
               table.note <- "* indicates {\\i p} < .05; ** indicates {\\i p} < .01. {\\i M} and {\\i SD} are used to represent mean and standard deviation, respectively. {\\i d}-values are unbiased estimates calculated using formulas 4.18 and 4.19 from Borenstein, Hedges, Higgins, & Rothstein (2009). {\\i d}-values not calculated if unequal variances prevented pooling."
          }
          
          #Create RTF code
          rtfTable <- RtfTable$new(isHeaderRow=TRUE)
          rtfTable$setTableContent(output.matrix.rtf)
          cell.widths.in.inches <-c(1.25,.85,.85,rep(1,iv.num.levels-1))
          rtfTable$setCellWidthsInches(cell.widths.in.inches)
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
}


txt.d <- function(d.in,p.in) {
     if (p.in<.01) {
          star.str <- "**"
     } else if (p.in<.05) {
          star.str <- "*"
     } else {
          star.str <-""
     }

     d.str <- sprintf("%1.2f%s",d.in,star.str)
     return(d.str)
}


txt.d.ci <- function(cur.ci) {
     my.low.lim <- cur.ci$Lower.Conf.Limit.smd
     my.up.lim <- cur.ci$Upper.Conf.Limit.smd
     ci.str <- sprintf("[%1.2f, %1.2f]",my.low.lim,my.up.lim)
     return(ci.str)
}


get.d.value <- function(group1.data,group2.data) {
     #confirm this is the unbiased one
     m1 <- mean(group1.data)
     m2 <- mean(group2.data)
     v1 <- var(group1.data)
     v2 <- var(group2.data)
     n1 <- length(group1.data)
     n2 <- length(group2.data)
     sp <- sqrt((v1*(n1-1) + v2*(n2-1))/(n1+n2-2))
     d.out <- (m1-m2)/sp
     d.out <- abs(d.out)
     return(d.out)     
}

is.equal.var <- function(group1.dv,group2.dv) {
     iv.1 <- rep(1,length(group1.dv))
     iv.2 <- rep(2,length(group2.dv))
     
     iv <- c(iv.1,iv.2)
     dv <- c(group1.dv,group2.dv)
     
     my.df <- data.frame(iv,dv)
     my.df$iv <- as.factor(my.df$iv)
     
     lev.result <- car::leveneTest(dv~iv,center=median,data=my.df)
     lev.p <- lev.result$`Pr(>F)`[1]

     b.result <- TRUE
     if (lev.p<.05) {
          b.result <- FALSE
     }
     return(b.result)
}
