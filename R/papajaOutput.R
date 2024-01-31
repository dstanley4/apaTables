#' Create output for papaja
#' @param table_object Previously constructed apaTable object
#' @param latex_font_size A strijg that indicates normalsize, small, footnotesize, scriptsize
#' @return Save status
#' @export
apa.knit.table.for.papaja <- function(table_object, latex_font_size = "footnotesize"){

     #apa.knit.table.for.papaja <- function(table_object, table_note = NULL, table_title = NULL, line_spacing = 1)


     table_type <- table_object$table.type

     table_out <- " "

     if (table_type == "regression") {
          #table_out <- apa.knit.regression.for.papaja(table_object, table_note, table_title, line_spacing)
          table_out <- apa.knit.regression.for.papaja(table_object, latex_font_size)
     }


     return(table_out)
}


apa.knit.regression.for.papaja <- function(table_object, latex_font_size = "footnotesize"){



     table_df <- dplyr::as_tibble(as.matrix(table_object$table_body))
     #table_df <- as_tibble(table_df)
     table_df$Predictor <- gsub("_", " ", table_df$Predictor)



     num_cols <- dim(table_df)[2]


     tblcolnames = names(table_df)
     tblcolnames[2] <- "$b$"
     tblcolnames[3] <- "95\\% CI"
     tblcolnames[4] <- "$beta$"
     tblcolnames[5] <- "95\\% CI"
     tblcolnames[6] <- "Unique $R^2$"
     tblcolnames[7] <- "95\\% CI"
     tblcolnames[8] <- "$r$"

     if (num_cols == 10) {
          tblcolnames[10] <- "$\\Delta$ Fit "
     }


     table_note <- table_object$latex.table.note
     table_note <- substr(table_note, 17,nchar(table_note))

     table_title <- table_object$latex.table.title
     table_title <- gsub("_", " ", table_title)
     table_title <- stringr::str_to_title(table_title)

     table_out <- papaja::apa_table(table_df,
                            format = "latex",
                            font_size = latex_font_size,
                            landscape = TRUE,
                            col.names = tblcolnames,
                            caption = table_title,
                            note = table_note,
                            escape = FALSE)

     table_out[[1]] = gsub(" Delta "," $\\\\Delta$ ", table_out[[1]])
     table_out[[1]] = gsub("R2","$R^2$", table_out[[1]])
     table_out[[1]] = gsub("\\\\\\\\newline","\\\\newline", table_out[[1]])


     return(table_out)

}
