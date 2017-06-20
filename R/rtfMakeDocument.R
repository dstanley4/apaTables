write.rtf.table <- function(filename,txt.body,landscape=FALSE,paper="us", table.title=NA,table.note=NA,table.number=NA) {
     #generate document format code if needed
     doc.type <- list()
     doc.type$uslandscape <- "\\paperw15840 \\paperh12240 \\margl1440 \\margr1440 \\margt1440 \\margb1440 \\landscape "
     doc.type$usportrait <- ""
     doc.type$a4landscape <- "\\paperw16834 \\paperh11909 \\margl1440 \\margr1440 \\margt1440 \\margb1440 \\landscape "
     doc.type$a4portrait <- ""

     if (!any(paper == c("us","a4"))) {
          paper <- "us"
     }
     if (landscape == TRUE) {
          orientation <- "landscape"
     } else {
          orientation <- "portrait"
     }

     table.number.str <- ""
     if (is.na(table.number)) {
          table.number.str <- "XX"
     } else {
          table.number <- round(table.number)
          table.number.str <- sprintf("%1.0f",table.number)
     }



     #document format
     doc.spec <- paste(paper,orientation,sep="")
     txt.format <- doc.type[[doc.spec]]
     txt.start <- "{\\rtf1\\ansi\\deff0 {\\fonttbl {\\f0 Times New Roman;}}"
     txt.end <- "}"

     #Table X, title, and note
     blank.line <- c("{\\pard  \\par}")
     number.line <-sprintf("{\\pard Table %s \\par}",table.number.str)
     if (is.na(table.title)) {
          title.line <- sprintf("{\\pard\\i Table title goes here \\par}")
     } else {
          title.line <- sprintf("{\\pard\\i %s\\par}",table.title)
     }
     if (is.na(table.note)) {
          note.line <- sprintf("{\\i Table note goes here}")
     } else {
          note.line <- sprintf("{\\pard \\par}{\\pard{\\i Note.} %s\\par}",table.note)
     }
     txt.body <- c(number.line,blank.line,title.line, blank.line,txt.body,note.line)


     file.id <- file(filename,"wt")
     writeLines(txt.start,file.id)
     if (landscape==TRUE) {
          writeLines(txt.format,file.id)
     }
     length.body <- length(txt.body)
     for (i in 1:length.body) {
          writeLines(txt.body[i],file.id)
     }
     writeLines(txt.end,file.id)
     close(file.id)
}

