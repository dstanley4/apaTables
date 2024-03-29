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
#' # Example 1: 1-way from Field et al. (2012) Discovery Statistics Using R
#'
#' table1 <- apa.1way.table(iv = dose, dv = libido,
#'                data = viagra, table.number = 1)
#'
#' apa.save(filename = "table1.doc", table1)
#'
#' # Create a table for your PDF
#' # Include the line below in your rmarkdown or Quarto document
#' apa.knit.table.for.pdf(table1)
#'
#' # delete demo file
#' if (file.exists("table1.doc")) {
#'      file.remove("table1.doc")
#' }
#' @export
apa.1way.table <- function(iv, dv, data,filename=NA, table.number=0, show.conf.interval=FALSE, landscape=FALSE){
     data <- as.data.frame(data)

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

     if (is.na(table.number)) {
          table.number = 0
          tbl.console$table.number = 0
     }


          #Create RTF code
     table.title <- sprintf("Descriptive Statistics for %s For Each Level of %s. ", stringr::str_to_sentence(dv.name), stringr::str_to_sentence(iv.name))
     table.note <- "{\\i M} = mean. {\\i SD} = standard deviation."

     if (show.conf.interval==TRUE) {
          ci.txt <- "{\\i CI} = confidence interval."
          table.note <- paste(table.note,ci.txt)
     }

      if (make.file.flag == TRUE) {
               write.rtf.table(filename = filename,txt.body = txt.body,table.title = table.title, table.note = table.note, table.number=table.number,landscape=landscape)
     }


     # Ver 3.0 add ons
     latex.table.note <- "\\\\textit{Note}. \\\\textit{M} = mean. \\\\textit{SD} = standard deviation. "
     if (show.conf.interval==TRUE) {
          latex.ci.txt <- "CI = confidence interval."
          latex.table.note <- paste(latex.table.note, latex.ci.txt)
     }

     tbl.console$latex.column.labels <-tables.out$latex.column.labels
     tbl.console$latex.column.centering <- make_markdown_column_alignment(tables.out$latex.column.labels)
     tbl.console$latex.table.note <- latex.table.note
     tbl.console$latex.table.title <- sprintf("Descriptive Statistics for %s For Each Level of %s. ",stringr::str_to_sentence(dv.name),stringr::str_to_sentence(iv.name))

     tbl.console$rtf.body         <- txt.body
     tbl.console$rtf.table.title  <- table.title
     tbl.console$rtf.table.note   <- table.note

     tbl.console$landscape      <- landscape
     tbl.console$table.type      <- "oneway"


     return(tbl.console)
}




apa.1way.table.work <- function(iv,dv,iv.name,dv.name, show.conf.interval) {
     iv.levels <- levels(iv)
     iv.level.numbers <- 1:length(iv.levels)

     my.means <- matrix(" ",length(iv.levels),1)
     my.sds <- matrix(" ",length(iv.levels),1)
     my.ci <- matrix(" ",length(iv.levels),1)

     for (iv.cur in iv.levels) {
          is.iv.level <- iv == iv.cur
          r.num.iv <- iv.level.numbers[iv.levels == iv.cur]

          cur.cell <- dv[is.iv.level]
          cell.mean <- mean(cur.cell,na.rm=TRUE)
          cell.sd <-  stats::sd(cur.cell,na.rm=TRUE)

          my.means[r.num.iv,1] <- sprintf("%1.2f",cell.mean)
          my.sds[r.num.iv,1] <- sprintf("%1.2f",cell.sd)
          LL <- get.ci.mean(cur.cell)$lower.conf.limit
          UL <- get.ci.mean(cur.cell)$upper.conf.limit
          ci_string <- sprintf("[%s, %s]", LL,UL)
          my.ci[r.num.iv,1] <- ci_string
     }


     if (show.conf.interval==FALSE) {
          data.table <- data.frame(my.means,my.sds,stringsAsFactors = FALSE)
          names(data.table) <- c("M","SD")
     } else {
          data.table <- data.frame(my.means,my.ci,my.sds,stringsAsFactors = FALSE)
          names(data.table) <- c("M","CI","SD")
     }

     return(data.table)
}

one.way.table.console.and.rtf <- function(iv,dv,iv.name, dv.name, show.conf.interval=FALSE,table.number,add.blank.header=FALSE) {
     table.out <- apa.1way.table.work(iv=iv,dv=dv,iv.name=iv.name,dv.name=dv.name, show.conf.interval=show.conf.interval)

     #add first name column
     level.names <- as.data.frame(matrix(levels(iv),ncol=1))
     names(level.names) <- iv.name
     table.out <- cbind(level.names,table.out)
     names(table.out)[1] <- "IV"



     #make console output
     table.title <- sprintf("Descriptive Statistics for %s For Each Level of %s. ",stringr::str_to_sentence(dv.name),stringr::str_to_sentence(iv.name))

     table.body <- table.out
     names(table.body) <- get_oneway_column_names(table.body)
     names(table.body)[1] <- iv.name

     table.note <- "Note. M = mean. SD = standard deviation.\n"
     if (show.conf.interval==TRUE) {
          ci.txt <- "CI = confidence interval."
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
     names(table.out.rtf) <- get_oneway_rtf_column_names(table.out)
     names(table.out.rtf)[1] <- iv.name
     #names(table.out.rtf) <- sprintf("{\\i %s}",names(table.out))
     #make latex column names
     latex.column.labels <- get_oneway_latex_column_names(table.out)
     latex.column.labels[1] <- iv.name

     #make rtf table
     rtfTable <- RtfTable$new(isHeaderRow=TRUE)
     rtfTable$setTableContent(as.matrix(table.out.rtf))
     rtfTable$setRowFirstColumnJustification("right")

     cell.widths <- get_oneway_rtf_column_widths(table.out)
     rtfTable$setCellWidthsInches(cellWidths = cell.widths)
     txt.body <- rtfTable$getTableAsRTF(isExtraSpacing=FALSE,FALSE)

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
     output$latex.column.labels <- latex.column.labels
     return(output)
}


oneway_column_width <- function(column_name) {
     narrow <- .60
     wide   <- .95

     switch(column_name,
            IV = wide*1.5,
            M = narrow,
            CI = wide*1.25,
            SD = narrow)
}



get_oneway_rtf_column_widths <- function(df) {
     n <- names(df)
     width_out <- c()
     for (i in 1:length(n)) {
          width_out[i] <-oneway_column_width(n[i])
     }
     return(width_out)
}


oneway_rtf_column_names <- function(column_name) {
     switch(column_name,
            IV = "IV",
            M = "{\\i M}",
            CI = "95% CI",
            SD = "{\\i SD}")
}

oneway_latex_column_names <- function(column_name) {
     switch(column_name,
            IV = "IV",
            M = "$M$",
            CI = "95\\% CI",
            SD = "$SD$")
}

get_oneway_latex_column_names <- function(df) {
     n <- names(df)
     names_out <- c()
     for (i in 1:length(n)) {
          names_out[i] <-oneway_latex_column_names(n[i])
     }
     return(names_out)
}


get_oneway_rtf_column_names <- function(df) {
     n <- names(df)
     names_out <- c()
     for (i in 1:length(n)) {
          names_out[i] <-oneway_rtf_column_names(n[i])
     }
     return(names_out)
}


oneway_column_names <- function(column_name) {
     switch(column_name,
            IV = "IV",
            M = "M",
            CI = "M_95%_CI",
            SD = "SD")
}

get_oneway_column_names <- function(df) {
     n <- names(df)
     names_out <- c()
     for (i in 1:length(n)) {
          names_out[i] <-oneway_column_names(n[i])
     }
     return(names_out)
}


make_markdown_column_alignment <- function(column_names) {
     N = length(column_names)
     output = rep("r", N)
     output[1] <- "l"
     return(output)
}
