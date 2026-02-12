#' Creates a table of means and standard deviations for a 2-way ANOVA design in APA style
#' @param iv1 Name of independent variable 1 column in data frame
#' @param iv2 Name of independent variable 2 column in data frame
#' @param dv Name of dependent variable column in data frame
#' @param data Project data frame name
#' @param filename (optional) Output filename document filename (must end in .rtf or .doc only)
#' @param table.number  Integer to use in table number output line
#' @param show.conf.interval  (TRUE/FALSE) Display confidence intervals in table. Negates show.marginal.means = TRUE.
#' @param show.marginal.means (TRUE/FALSE) Show marginal means in output. Only used if show.conf.interval = FALSE.
#' @param landscape (TRUE/FALSE) Make RTF file landscape
#' @return APA table object
#' @examples
#' # Example 2: 2-way from Fidler & Thompson (2001)
#'
#' table2 <- apa.2way.table(iv1 = a, iv2 = b, dv = dv,
#'                           data = fidler_thompson,
#'                           landscape = TRUE,
#'                           table.number = 2)
#'
#'
#' # Example 3: 2-way from Field et al. (2012) Discovery Statistics Using R
#'
#' table3 <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
#'                          data = goggles, table.number = 3)
#'
#'
#' # Save both Table 2 and Table 3 in a single .doc document
#' apa.save(filename = file.path(tempdir(), "my_tables.doc"), table2, table3)
#'
#' # Create a table for your PDF
#' # Include the lines below in your rmarkdown or Quarto document
#' apa.knit.table.for.pdf(table2)
#' apa.knit.table.for.pdf(table3)
#'
#' # delete demo file
#' unlink(file.path(tempdir(), "my_tables.doc"))
#' @export
apa.2way.table <- function(iv1, iv2, dv, data, filename = NA, table.number = 0, show.conf.interval = FALSE, show.marginal.means = FALSE, landscape = TRUE){

     data <- as.data.frame(data)


     if (is.na(filename)) {
          make.file.flag=FALSE
     } else {
          make.file.flag=TRUE
     }

     if (!is.null(data)) {
          data.col.names <- colnames(data)
     } else {
          stop("apa.2way.table error:\ndata not specified.", call. = FALSE)
     }

     iv1.sub <- substitute(iv1)
     is.iv1 <- is.valid.name(iv1.sub,data.col.names)
     iv2.sub <- substitute(iv2)
     is.iv2 <- is.valid.name(iv2.sub,data.col.names)

     dv.sub <- substitute(dv)
     is.dv  <- is.valid.name(dv.sub, data.col.names)


     if (is.dv==FALSE) {
          stop("apa.2way.table error:\nA valid dependent variable (dv) must be specified.", call. = FALSE)
     }

     if (all(is.iv1==TRUE & is.iv2==TRUE)==FALSE) {
          stop("apa.2way.table error:\nTwo valid independent variables (iv's) must be specified.", call. = FALSE)
     }

     iv1.name <- deparse(iv1.sub)
     iv2.name <- deparse(iv2.sub)
     dv.name <- deparse(dv.sub)

     iv1 <- as.factor(data[,iv1.name])
     iv2 <- as.factor(data[,iv2.name])
     dv <- data[,dv.name]

     if (show.conf.interval==FALSE) {
          output.information <- apa.2way.table.work(iv1 = iv1, iv2 = iv2, dv = dv,
                                                    iv1.name = iv1.name, iv2.name = iv2.name, dv.name = dv.name,
                                                    show.marginal.means = show.marginal.means,
                                                    show.conf.interval = show.conf.interval)

          #store output unaltered for console output
          table.initial.console <- output.information
          txt.body <- apa.2way.console.to.rtf(table.initial.console)


          # create latex table
          rows.columns.out <- dim(output.information)[1]
          latex.body <- output.information[3:rows.columns.out,]
          latex.body.names = output.information[2,]
          names(latex.body) <- latex.body.names
          if (show.marginal.means == FALSE) {
               latex.extra.header1 = c(" ", levels(iv2))
          } else {
               latex.extra.header1 = c(" ", levels(iv2), "Marginal")
          }

          latex.extra.header2 = iv2.name

          latex.group.names = ""
          latex.group.num.per.group = 0

     } else {
          # output <- apa.2way.table.ci.work(iv1=iv1,iv2=iv2,dv=dv,iv1.name=iv1.name,iv2.name=iv2.name,dv.name=dv.name,table.number=table.number)
          # txt.body <- output$rtf
          # table.initial.console <- output$console
          output.information <- apa.2way.table.ci.work(iv1 = iv1, iv2 = iv2, dv = dv,
                                                       iv1.name = iv1.name, iv2.name = iv2.name,
                                                       dv.name = dv.name,
                                                       table.number = table.number)
          txt.body <- output.information$rtf
          table.initial.console <- output.information$console

          # create latex table
          latex.body.temp <- output.information$latex.body
          rows.columns.out <- dim(latex.body.temp)[1]
          latex.body <- latex.body.temp
          latex.group.names = paste0(paste0(iv2.name, ": "), levels(iv2))
          latex.group.num.per.group = length(levels(iv1))
          latex.extra.header1 <- " "
     }

     #make table title
     iv1.num.levels = length(levels(iv1))
     iv2.num.levels = length(levels(iv2))
     table.title <- sprintf("Descriptive Statistics For %s In a %1.0f(%s) X %1.0f(%s) Design",stringr::str_to_sentence(dv.name),iv1.num.levels,stringr::str_to_sentence(iv1.name),iv2.num.levels,stringr::str_to_sentence(iv2.name))
     latex.table.title <- table.title

     #make table notes console and rtf
     if ((show.marginal.means==TRUE) & (show.conf.interval==FALSE)) {
          table.note <- "{\\i M} = mean. {\\i SD} = standard deviation. Marginal indicates the means and standard deviations pertaining to main effects."
          table.note.txt <- "Note. M = mean. SD = standard deviation. \nMarginal indicates the means and standard deviations pertaining to main effects."
     } else {
          table.note <- "{\\i M} = mean. {\\i SD} = standard deviation."
          table.note.txt <- "Note. M = mean. SD = standard deviation."
     }
     if (show.conf.interval==TRUE) {
          ci.txt <- "\nCI = confidence interval."
          table.note.txt <- paste(table.note.txt,ci.txt)
     }


     #make console output
     tbl.console <- list()
     tbl.console <- list(table_number = table.number,
                         table_title = table.title,
                         table_body = table.initial.console,
                         table_note = table.note.txt)


     class(tbl.console) <- "apa_table"


     rtf.title <- table.title
     table.title <- rtf.title
     table.note <- "{\\i M} = mean. {\\i SD} = standard deviation."
     if (show.conf.interval==TRUE) {
          ci.txt <- "CI = confidence interval."
          table.note <- paste(table.note,ci.txt)
     }

     if (make.file.flag == TRUE) {
               write.rtf.table(filename = filename,txt.body = txt.body,table.title = table.title, table.note = table.note, table.number=table.number, landscape=landscape)
     }

     # Ver 3.0 add ons
     if (is.na(table.number)) {
          table.number = 0
          tbl.console$table_number = 0
     }

     latex.table.note <- "\\\\textit{Note}. \\\\textit{M} = mean. \\\\textit{SD} = standard deviation. "
     if (show.conf.interval==TRUE) {
          latex.ci.txt <- "CI = confidence interval."
          latex.table.note <- paste(latex.table.note, latex.ci.txt)
     }


     # V3

     tbl.console$latex.group.names <- latex.group.names
     tbl.console$latex.group.num.per.group <- latex.group.num.per.group

     tbl.console$latex.body <- latex.body
     tbl.console$latex.extra.header1 <- latex.extra.header1
     tbl.console$latex.extra.header2 <- iv2.name
     tbl.console$latex.column.labels <- get_oneway_latex_column_names(latex.body)
     tbl.console$latex.column.labels[1] <- iv1.name
     tbl.console$latex.column.centering <- make_markdown_column_alignment(tbl.console$latex.column.labels)
     tbl.console$latex.table.note <- latex.table.note
     tbl.console$latex.table.title <- latex.table.title

     tbl.console$rtf.body         <- txt.body
     tbl.console$rtf.table.title  <- table.title
     tbl.console$rtf.table.note   <- table.note

     tbl.console$landscape      <- landscape




     if (show.conf.interval == FALSE) {
               tbl.console$table.type     <- "twoway"
     } else {
          tbl.console$table.type     <- "twoway-ci"
     }

     return(tbl.console)
}










apa.2way.console.to.rtf <- function(table.initial.console) {
     #process headings for rtf creation
     n.row <- dim(table.initial.console)[1]
     table.body <- table.initial.console[3:n.row,]


     table.h1 <- names(table.initial.console) # left cell, one big cell / iv2 name

     iv2.name <- table.h1[2]
     table.h2 <- table.initial.console[1,] # left cell, several 2*K double width cells /iv2 levels
     table.h2 <- table.h2[1,c(1,seq(2,dim(table.h2)[2],by=2))] #tab
     table.h3 <- table.initial.console[2,] # left cell, several K single width cells, M/SD
     iv1.name <- table.h3[1]
     table.h3 <- sprintf("{\\i %s}",table.h3) # make Mean/SD  in italic
     table.h3[1] <- iv1.name
     names(table.body) <- table.h3

     #Create RTF code for main table
     rtf.table <- RtfTable$new(isHeaderRow=TRUE)
     rtf.table$setTableContent(as.matrix(table.body))
     rtf.table$setRowFirstColumnJustification("right")
     txt.body <- rtf.table$getTableAsRTF(isExtraSpacing=TRUE,FALSE)


     #Create RTF code for IV2 levels (table header)
     txt.body.cell.widths <- rtf.table$cellWidthsInches
     h2.num.cells <- dim(table.h2)[2]
     h2.cell.widths <- rep(txt.body.cell.widths[2]*2,h2.num.cells)
     h2.cell.widths[1] <- txt.body.cell.widths[1]
     rtf.table.h2 <- RtfTable$new(isHeaderRow=FALSE)
     rtf.table.h2$setTableContent(as.matrix(table.h2))
     rtf.table.h2$setCellWidthsInches(cellWidths = h2.cell.widths)
     rtf.table.h2$setDecimalTabWidthsProportions(rep(0,h2.num.cells)) #no tabs, ensures centering
     last_column_number <- h2.num.cells

     show.marginal.means <- FALSE
     if (table.h2[1,h2.num.cells]=="Marginal") {
          show.marginal.means <- TRUE
     }

     if (show.marginal.means==TRUE) {
          rtf.table.h2$noLineAboveColumns <- c(1, last_column_number) # no line first/marginal column
     } else {
          rtf.table.h2$noLineAboveColumns <- c(1)
     }
     txt.h2 <- rtf.table.h2$getTableAsRTF(isExtraSpacing=TRUE,FALSE)
     txt.body <- c(txt.h2,txt.body)

     #Create RTF code for IV2 name
     h1.cell.widths <- c(h2.cell.widths[1],sum(h2.cell.widths[2:h2.num.cells]))
     h1.num.cells <- 2
     h1.text.row <- c(" ",iv2.name)
     if (show.marginal.means==TRUE) {
          h1.num.cells <- h1.num.cells + 1
          h1.cell.widths <- c(h2.cell.widths[1],sum(h2.cell.widths[2:h2.num.cells])-h2.cell.widths[h2.num.cells],h2.cell.widths[h2.num.cells])
          h1.text.row <- c(" ",iv2.name, " ")
     }
     rtf.table.h1 <- RtfTable$new(isHeaderRow=FALSE)

     rtf.table.h1$setTableContent(matrix(h1.text.row,nrow = 1))
     rtf.table.h1$setCellWidthsInches(cellWidths = h1.cell.widths)
     rtf.table.h1$setDecimalTabWidthsProportions(rep(0,h1.num.cells)) #no tabs, ensures centering
     txt.h1 <- rtf.table.h1$getTableAsRTF(isExtraSpacing=TRUE,FALSE)
     txt.body <- c(txt.h1,txt.body)

     return(txt.body)
}


apa.2way.table.work <- function(iv1,iv2,dv,iv1.name,iv2.name,dv.name,show.marginal.means,show.conf.interval) {
     #create table cells
     count <- 0
     column.sets <- list()
     for (cur.iv2.level in levels(iv2)) {
          count <- count + 1
          cur.dv <- dv[iv2==cur.iv2.level]
          cur.iv1 <- iv1[iv2==cur.iv2.level]
          m.sd.columns <- apa.1way.table.work(iv=cur.iv1,dv=cur.dv,iv.name=iv1.name,show.conf.interval = show.conf.interval)
          column.sets[[count]] <- m.sd.columns
     }
     final.count <- count

     columns.out <- column.sets[[1]]
     for (i in 2:final.count) {
          columns.out <- cbind(columns.out,column.sets[[i]])
     }


     if (show.conf.interval == FALSE) {
          if (show.marginal.means ==TRUE) {
               #create marginal means
               iv1.marginal <- apa.1way.table.work(iv=iv1,dv=dv,iv.name=iv1.name,show.conf.interval = FALSE)
               iv2.marginal <- apa.1way.table.work(iv=iv2,dv=dv,iv.name=iv2.name,show.conf.interval = FALSE)
               columns.out <- cbind(columns.out,iv1.marginal)

               iv2.marginal.strung.out <- string.out.data.frame(iv2.marginal)
               iv2.marginal.strung.out <- cbind(iv2.marginal.strung.out," "," ")
               names(iv2.marginal.strung.out) <- names(columns.out)
               columns.out <- rbind(columns.out,iv2.marginal.strung.out)
          }
     }


     #add labels for m and sd as row
     columns.out.column.names <- colnames(columns.out)
     m.sd.column.names.row <- t(data.frame(columns.out.column.names,stringsAsFactors = FALSE))
     colnames(m.sd.column.names.row) <- columns.out.column.names
     columns.out <- rbind(m.sd.column.names.row, columns.out)


     #add IV2 levels as row
     iv2.levels <- levels(iv2)
     if ((show.marginal.means==TRUE) & (show.conf.interval==FALSE)) {
          iv2.levels <- c(iv2.levels,"Marginal")
     }
     iv2.num.levels <- length(iv2.levels)

     #weave spaces into levels
     my.spaces <- rep(" ",length(iv2.levels))
     if (show.conf.interval==TRUE) {
          num.cell.multiplier <- 4
          iv2.levels.column.names <- matrix(rbind(iv2.levels,my.spaces,my.spaces,my.spaces),nrow=iv2.num.levels*num.cell.multiplier)
     } else {
          num.cell.multiplier <- 2
          iv2.levels.column.names <- matrix(rbind(iv2.levels,my.spaces),nrow=iv2.num.levels*num.cell.multiplier)
     }
     iv2.levels.column.names.row <- t(data.frame(iv2.levels.column.names,stringsAsFactors = FALSE))
     colnames(iv2.levels.column.names.row) <- columns.out.column.names
     columns.out <- rbind(iv2.levels.column.names.row, columns.out)


     #add IV2 varaible name as column name
     columns.out.new.column.names <- rep(" ",length(iv2.levels.column.names))
     columns.out.new.column.names[1] <- iv2.name
     colnames(columns.out) <- columns.out.new.column.names


     #add label column for IV1
     iv1.levels <- c(" ",iv1.name,levels(iv1))
     if ((show.marginal.means==TRUE) & (show.conf.interval==FALSE)) {
          iv1.levels <- c(iv1.levels,"Marginal")
     }
     columns.out <- cbind(iv1.levels,columns.out)
     my.col.names <- names(columns.out)
     my.col.names[1] <- "  "


     columns.out <- data.frame(as.matrix(columns.out),stringsAsFactors = FALSE)
     names(columns.out) <- my.col.names
     rownames(columns.out) <- NULL

     return(columns.out)
}


string.out.data.frame <- function(df.in) {
     size.frame <- dim(df.in)
     n.row <- size.frame[1]
     n.col <- size.frame[2]

     row.out <- df.in[1,]
     for (i in 2:n.row) {
          cur.row <- df.in[i,]
          row.out <- cbind(row.out,cur.row)
     }
     colnames(row.out) <- NULL
     return(row.out)
}


apa.2way.table.ci.work <- function(iv1,iv2,dv,iv1.name,iv2.name,dv.name,table.number) {
     count <- 0
     rtf.data <- c()
     for (cur.iv2.level in levels(iv2)) {
          count <- count + 1
          cur.dv <- dv[iv2==cur.iv2.level]
          cur.iv <- iv1[iv2==cur.iv2.level]

          tables.out = one.way.table.console.and.rtf(iv = cur.iv, dv = cur.dv, iv.name = iv1.name,
                                                     dv.name = dv.name,
                                                     show.conf.interval = TRUE,
                                                     table.number, add.blank.header = TRUE)

          #console

          if (count==1) {
               tbl.console.output <- tables.out$tbl.console
               tbl.console <- tbl.console.output$table_body
               latex.body <- tbl.console

               tbl.console <- add.blank.first.row(tbl.console)
               tbl.console[1,1] <- sprintf("%s",iv1.name)

               tbl.console <- add.blank.first.row(tbl.console)
               tbl.console[1,1] <- sprintf("%s:%s",iv2.name,cur.iv2.level)
               console.data <- tbl.console
          } else {
               tbl.console <- tables.out$tbl.console$table_body
               latex.cur.body <- tbl.console

               tbl.console <- add.blank.first.row(tbl.console)
               tbl.console[1,1] <- sprintf("%s",iv1.name)

               tbl.console <- add.blank.first.row(tbl.console)
               tbl.console[1,1] <- sprintf("%s:%s",iv2.name,cur.iv2.level)
               tbl.console <- add.blank.first.row(tbl.console)

               console.data <- my.rbind(console.data,tbl.console,1)
               latex.body <- rbind(latex.body, latex.cur.body)
          }
          my.col.names <- colnames(console.data)
          my.col.names[1] <- ""
          colnames(console.data) <- my.col.names
          tbl.console.output$table_body <- console.data

          #rtf
          txt.body <- tables.out$txt.body
          rtf.data.header <- txt.body[5]
          rtf.data.header.correct <- sub("placeholder",sprintf("%s:%s",iv2.name,cur.iv2.level),rtf.data.header)
          txt.body[5] <- rtf.data.header.correct
          rtf.data <- c(rtf.data,txt.body)


     }
     final.count <- count

     latex.names <- names(latex.body)
     latex.names[3] <- "CI"
     names(latex.body) <- latex.names

     output <- list()
     output$rtf <- rtf.data
     output$console <- tbl.console.output$table_body
     output$latex.body <- latex.body
     return(output)
}


add.blank.first.row <-function(df.in) {
     df.out <- df.in[1,]
     number.columns <- dim(df.out)[2]
     for (i in 1:number.columns) {
          df.out[,i] <- ""
     }
     df.out <- my.rbind(df.out,df.in,2)

     return(df.out)
}

my.rbind <- function(df.1,df.2,names.from.table.number) {
     if (names.from.table.number==1) {
          my.col.names <- colnames(df.1)
     } else {
          my.col.names <- colnames(df.2)
     }
     colnames(df.1) <- my.col.names
     colnames(df.2) <- my.col.names

     df.out <- rbind(df.1,df.2)
     df.out <- data.frame(df.out,stringsAsFactors = FALSE)
     colnames(df.out) <- my.col.names

     return(df.out)
}


