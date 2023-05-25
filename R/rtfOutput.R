apa.save <- function(filename, ..., paper = "us") {
     list_of_tables <- list(...)
     write.rtf.document(filename, list_of_tables, paper)
}

write.rtf.document <- function(filename, list_of_tables, paper="us") {
     #generate document format code if needed
     doc.type <- list()
     doc.type$uslandscape <- "\\paperw15840 \\paperh12240 \\margl1440 \\margr1440 \\margt1440 \\margb1440 \\landscape "
     doc.type$usportrait <- ""
     doc.type$a4landscape <- "\\paperw16834 \\paperh11909 \\margl1440 \\margr1440 \\margt1440 \\margb1440 \\landscape "
     doc.type$a4portrait <- ""

     if (!any(paper == c("us","a4"))) {
          paper <- "us"
     }

     orientation <- "portrait"
     #
     # TO DO:  Check if only one table and if it is landscape then change below to landscape
     #
     # if (landscape == TRUE) {
     #      orientation <- "landscape"
     # }

     #document format
     doc.spec <- paste(paper,orientation,sep="")
     txt.format <- doc.type[[doc.spec]]
     txt.start <- "{\\rtf1\\ansi\\deff0 {\\fonttbl {\\f0 Times New Roman;}}"
     txt.end <- "}"

     # KEY PART HERE
     txt.body <- convert_tables_to_rtf_txt(list_of_tables, paper)

     file.id <- file(filename,"wt")
     writeLines(txt.start,file.id)
#      if (landscape==TRUE) {
#           writeLines(txt.format,file.id)
#      }
     length.body <- length(txt.body)
     for (i in 1:length.body) {
          writeLines(txt.body[i],file.id)
     }
     writeLines(txt.end,file.id)
     close(file.id)
}


convert_tables_to_rtf_txt <- function(list_of_tables, paper = "us") {

     txt_for_pages <- ""

     number_of_tables <- length(list_of_tables)


     for(cur_table_number in 1:number_of_tables) {

          start_page_txt <- get_start_page_txt(list_of_tables, cur_table_number, paper)
          end_page_txt <- get_end_page_txt(list_of_tables, cur_table_number)
          table_for_page_txt <- convert_single_table_to_txt(list_of_tables[[cur_table_number]])

          new_txt_for_pages <- c(start_page_txt, "{", table_for_page_txt, end_page_txt, "}")
          txt_for_pages <- c(txt_for_pages, new_txt_for_pages)

     }
     return(txt_for_pages)

}

get_start_page_txt <- function(list_of_tables, cur_table_number, paper) {
     section.type <- list()
     section.type$uslandscape <- "\\sectd \\ltrsect\\lndscpsxn\\pgwsxn15840\\pghsxn12240\\pard\\plain \\ltrpar"
     section.type$usportrait  <- "\\sectd \\ltrsect\\lndscpsxn\\pgwsxn12240\\pghsxn15840\\pard\\plain \\ltrpar"
     section.type$a4landscape <- "\\sectd \\ltrsect\\lndscpsxn\\pgwsxn16834\\pghsxn11909\\pard\\plain \\ltrpar"
     section.type$a4portrait  <- "\\sectd \\ltrsect\\lndscpsxn\\pgwsxn11909\\pghsxn16834\\pard\\plain \\ltrpar"

     if (paper == "us") {
          section_landscape_header <- section.type$uslandscape
          section_portrait_header  <- section.type$usportrait
     } else {
          section_landscape_header <- section.type$a4landscape
          section_portrait_header  <- section.type$a4portrait
     }


     number_of_tables <- length(list_of_tables)

     if (cur_table_number == 1) {
          start_page_txt <- ""
     } else {
          cur_is_landscape = list_of_tables[[cur_table_number]]$landscape
          prev_is_landscape = list_of_tables[[cur_table_number-1]]$landscape

          if (cur_is_landscape == prev_is_landscape) {
               start_page_txt <- ""
          } else {
               # new section
               if (cur_is_landscape) {
                    start_page_txt <- section_landscape_header
               } else {
                    start_page_txt <- section_portrait_header
               }
          }
     }
     return(start_page_txt)
}


get_end_page_txt <- function(list_of_tables, cur_table_number) {
     number_of_tables <- length(list_of_tables)

     if (cur_table_number == number_of_tables) {
          end_page_txt <- " "
     } else {
          cur_is_landscape = list_of_tables[[cur_table_number]]$landscape
          next_is_landscape = list_of_tables[[cur_table_number+1]]$landscape

          if (cur_is_landscape == next_is_landscape) {
               end_page_txt <- "\\page"
          } else {
               end_page_txt <- "\\sect"
          }
     }
     return(end_page_txt)
}


convert_single_table_to_txt <- function(cur_table_object){


     table.number<- cur_table_object$table.number
     table.number.str <- sprintf("%1.0f",table.number)

     #Table X, title, and note
     blank.line <- c("{\\pard  \\par}")
     number.line <-sprintf("{\\pard Table %s \\par}",table.number.str)
     if (is.na(cur_table_object$rtf.table.title)) {
          title.line <- sprintf("{\\pard\\i Table title goes here \\par}")
     } else {
          title.line <- sprintf("{\\pard\\i %s\\par}",cur_table_object$rtf.table.title)
     }
     if (is.na(cur_table_object$table.note)) {
          note.line <- sprintf("{\\i Table note goes here}")
     } else {
          note.line <- sprintf("{\\pard \\par}{\\pard{\\i Note.} %s\\par}",cur_table_object$rtf.table.note)
     }

     txt.body <- cur_table_object$rtf.body


     txt.body <- c(number.line,blank.line,title.line, blank.line,txt.body,note.line)

     return(txt.body)
}


