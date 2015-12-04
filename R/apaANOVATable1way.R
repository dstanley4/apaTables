#' Creates a table of means and standard deviations for a 1-way ANOVA design in APA style
#' @param iv Name of independent variable column in data frame
#' @param dv Name of dependent variable column in data frame
#' @param data Project data frame name
#' @param filename (optional) Output filename document filename (must end in .rtf or .doc only)
#' @param table.number Integer to use in table number output line
#' @param show.conf.interval  (TRUE/FALSE) Display confidence intervals in table.
#' @param landscape (TRUE/FALSE) Make RTF file landscape
#' @return APA table object
#' @examples
#' # View top few rows of viagra data set 
#' # from Discovering Statistics Using R
#' head(viagra)
#' 
#' # Use apa.1way.table function
#' apa.1way.table(iv=dose,dv=libido,data=viagra,filename="ex1wayTable.doc")
#' @export
apa.1way.table <- function(iv, dv, data,filename=NA, table.number=NA, show.conf.interval=FALSE, landscape=FALSE){
     
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
          cat("A valid independent variables (iv) must be specified.\n\n")
          return(FALSE)
     }
     
     iv.name <- deparse(iv.sub)
     dv.name <- deparse(dv.sub)
     
     iv <- as.factor(data[,iv.name])
     dv <- data[,dv.name]

     tables.out = one.way.table.console.and.rtf(iv=iv,dv=dv,iv.name=iv.name,dv.name=dv.name,show.conf.interval = show.conf.interval,table.number)
     tbl.console <- tables.out$tbl.console
     txt.body <- tables.out$txt.body

     if (make.file.flag == TRUE) {
          #Create RTF code 
          table.title <- sprintf("Descriptive statistics for %s as a function of %s. ",dv.name,iv.name)
          table.note <- "{\\i M} and {\\i SD} represent mean and standard deviation, respectively."

          if (show.conf.interval==TRUE) {
               ci.txt <- "{\\i LL} and {\\i UL} indicate the lower and upper limits of the 95% confidence interval for the mean, respectively. The confidence interval is a plausible range of population means that could have created a sample mean (Cumming, 2014)."
               table.note <- paste(table.note,ci.txt)
          }
          
          #print("portrait")
          write.rtf.table(filename = filename,txt.body = txt.body,table.title = table.title, table.note = table.note, table.number=table.number,landscape=landscape)
     }

          return(tbl.console)
}     






apa.1way.table.work <- function(iv,dv,iv.name,dv.name, show.conf.interval) {
     iv.levels <- levels(iv)
     iv.level.numbers <- 1:length(iv.levels)
     
     my.means <- matrix(" ",length(iv.levels),1)
     my.sds <- matrix(" ",length(iv.levels),1)
     my.ci.lower <- matrix(" ",length(iv.levels),1)
     my.ci.upper <- matrix(" ",length(iv.levels),1)
     
     for (iv.cur in iv.levels) {
          is.iv.level <- iv == iv.cur
          r.num.iv <- iv.level.numbers[iv.levels == iv.cur]

          cur.cell <- dv[is.iv.level]
          cell.mean <- mean(cur.cell,na.rm=TRUE)
          cell.sd <-  stats::sd(cur.cell,na.rm=TRUE)
               
          my.means[r.num.iv,1] <- sprintf("%1.2f",cell.mean)
          my.sds[r.num.iv,1] <- sprintf("%1.2f",cell.sd)
          my.ci.lower[r.num.iv,1] <- get.ci.mean(cur.cell)$lower.conf.limit
          my.ci.upper[r.num.iv,1] <- get.ci.mean(cur.cell)$upper.conf.limit
     }

     
     if (show.conf.interval==FALSE) {
          data.table <- data.frame(my.means,my.sds,stringsAsFactors = FALSE)
          names(data.table) <- c("M","SD")
     } else {
          data.table <- data.frame(my.means,my.ci.lower,my.ci.upper,my.sds,stringsAsFactors = FALSE)
          names(data.table) <- c("M","LL","UL","SD")
     }
     
     return(data.table)
}


one.way.table.console.and.rtf <- function(iv,dv,iv.name, dv.name, show.conf.interval=FALSE,table.number,add.blank.header=FALSE) {
     table.out <- apa.1way.table.work(iv=iv,dv=dv,iv.name=iv.name,dv.name=dv.name, show.conf.interval=show.conf.interval)
     
     #add first name column
     level.names <- as.data.frame(matrix(levels(iv),ncol=1))
     names(level.names) <- iv.name
     table.out <- cbind(level.names,table.out)
     
     #make console output
     table.title <- sprintf("Descriptive statistics for %s as a function of %s. ",dv.name,iv.name)
     table.body <- table.out
     table.note <- "Note. M and SD represent mean and standard deviation, respectively.\n"
     if (show.conf.interval==TRUE) {
          ci.txt <- "LL and UL indicate the lower and upper limits of the 95% confidence interval \nfor the mean, respectively. \nThe confidence interval is a plausible range of population means that could \nhave caused a sample mean (Cumming, 2014)."
          table.note <- paste(table.note,ci.txt,sep="")
     }
     tbl.console <- list()
     tbl.console <- list(table.number = table.number,
                         table.title = table.title,
                         table.body = table.body,
                         table.note = table.note)
     class(tbl.console) <- "apa.table"
     
     
     #make rtf header row
     table.out.rtf <- table.out
     names(table.out.rtf) <- sprintf("{\\i %s}",names(table.out))
     
     #make rtf table
     rtfTable <- RtfTable$new(isHeaderRow=TRUE)
     rtfTable$setTableContent(as.matrix(table.out.rtf))
     rtfTable$setRowFirstColumnJustification("right")
     txt.body <- rtfTable$getTableAsRTF(isExtraSpacing=TRUE,FALSE)
     
     if (add.blank.header==TRUE) {
          #add extra top row
          #Create RTF code for IV2 name
          cell.widths <- sum(rtfTable$cellWidthsInches)
          h1.num.cells <- 1
          rtf.table.h1 <- RtfTable$new(isHeaderRow=FALSE)
          rtf.table.h1$setTableContent(matrix(c("placeholder"),nrow = 1))
          rtf.table.h1$setCellWidthsInches(cellWidths = cell.widths)
          rtf.table.h1$setDecimalTabWidthsProportions(rep(0,1)) #no tabs, ensures centering
          rtf.table.h1$setRowFirstColumnJustification("center")
          txt.h1 <- rtf.table.h1$getTableAsRTF(isExtraSpacing=TRUE,FALSE)
          txt.body <- c(txt.h1,txt.body)    
     }
     
     
     output <- list()
     output$tbl.console <- tbl.console
     output$txt.body <- txt.body 
     return(output)
}








