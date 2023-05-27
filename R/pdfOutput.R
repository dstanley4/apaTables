apa.knit.table.for.pdf <- function(table_object){

     table_type <- table_object$table.type

     table_out <- " "

     if (table_type == "oneway") {
          table_out <- apa.knit.oneway.for.pdf(table_object)
     } else if (table_type == "twoway") {
          table_out <- apa.knit.twoway.for.pdf(table_object)
     }

     return(table_out)
}

apa.knit.twoway.for.pdf <- function(table_object){

     table_df <- table_object$latex.body
     rownames(table_df) <- NULL

     table_extra_header1 <- table_object$latex.extra.header1
     table_extra_header2 <- table_object$latex.extra.header2


     header1_spacing <- rep(2, length(table_extra_header1))
     header1_spacing[1] <- 1
     names(header1_spacing) <- table_extra_header1

     is_marginal_means <- "Marginal" %in% table_extra_header1
     x.num.columns = dim(table_df)[2]
     y.num.columns = dim(table_df)[1]


     if (is_marginal_means) {
          header2_spacing <- c(1, x.num.columns-3,2)
          names(header2_spacing) <- c(" ", table_extra_header2, " ")
     } else {
          header2_spacing <- c(1, x.num.columns-1)
          names(header2_spacing) <- c(" ", table_extra_header2)
     }



     table_column_labels <- table_object$latex.column.labels
     table_column_center <- table_object$latex.column.centering
     table_note          <- table_object$latex.table.note
     table_title         <- table_object$latex.table.title



     table_out <- kableExtra::kbl(table_df, booktabs = T, escape = FALSE,
                                  col.names = table_column_labels,
                                  format = "latex",
                                  align = table_column_center,
                                  caption = table_title)

     table_out <- kableExtra::add_header_above(table_out, header1_spacing, escape = FALSE)
     table_out <- kableExtra::add_header_above(table_out, header2_spacing, escape = FALSE)
     table_out <- kableExtra::kable_styling(table_out, position = "left", font_size = 10)
     table_out <- kableExtra::row_spec(table_out, (y.num.columns-1), hline_after = TRUE)
     table_out <- kableExtra::footnote(table_out, escape = FALSE, general = table_note, general_title = "", threeparttable = T)

     if (table_object$landscape == TRUE) {
          table_out <- kableExtra::landscape(table_out)
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


