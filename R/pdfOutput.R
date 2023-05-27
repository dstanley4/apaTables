apa.knit.table.for.pdf <- function(table_object){

     table_type <- table_object$table.type

     table_out <- " "

     if (table_type == "oneway") {
          table_out <- apa.knit.oneway.for.pdf(table_object)
     } else if (table_type == "twoway-nomarginal") {

     }

     return(table_out)
}


apa.knit.oneway.for.pdf <- function(table_object){

     table_df <- table_object$table.body

     table_column_labels <- table_object$latex.column.labels
     table_column_center <- table_object$latex.column.centering
     table_note          <- table_object$latex.table.note
     table_title         <- table_object$latex.table.title


     table_out <- kableExtra::kbl(table_df, booktabs = T, escape = FALSE,
                                  col.names = table_column_labels,
                                  format = "latex",
                                  align = table_column_center,
                                  caption = table_title)

     if (dim(table_df)[2]>3) {
          table_out <- kableExtra::add_header_above(table_out, c(" " = 2, "95\\\\% CI" =1, " "), escape = FALSE)
     }
     table_out <- kableExtra::kable_styling(table_out, position = "left", font_size = 10)
     table_out <- kableExtra::footnote(table_out, escape = FALSE, general = table_note, general_title = "", threeparttable = T)

     return(table_out)
}

apa.knit.twoway.nomarginal.for.pdf <- function(table_object){

     table_df <- table_object$table.body

     table_column_labels <- table_object$latex.column.labels
     table_column_center <- table_object$latex.column.centering
     table_note          <- table_object$latex.table.note
     table_title         <- table_object$latex.table.title


     table_out <- kableExtra::kbl(table_df, booktabs = T, escape = FALSE,
                                  col.names = table_column_labels,
                                  format = "latex",
                                  align = table_column_center,
                                  caption = table_title)

     table_out <- kableExtra::add_header_above(table_out, c(" " = 2, "95\\\\% CI" =1, " "), escape = FALSE)
     table_out <- kableExtra::kable_styling(table_out, position = "left", font_size = 10)
     table_out <- kableExtra::footnote(table_out, escape = FALSE, general = table_note, general_title = "", threeparttable = T)

     return(table_out)
}
