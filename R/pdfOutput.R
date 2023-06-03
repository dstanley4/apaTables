#' Save previously constructed APA table objects in a single .doc file
#' @param table_object Previously constructed apaTable object
#' @param table_title  Replace default table title with this text
#' @param table_note  Replace default table note with this text
#' @param line_spacing  Line spacing multiplier for table
#' @return Save status
#' @export
apa.knit.table.for.pdf <- function(table_object, table_note = NULL, table_title = NULL, line_spacing = 1){

     table_type <- table_object$table.type

     table_out <- " "

     if (table_type == "oneway") {
          table_out <- apa.knit.oneway.for.pdf(table_object, table_note, table_title, line_spacing)
     } else if (table_type == "twoway") {
          table_out <- apa.knit.twoway.for.pdf(table_object, table_note, table_title, line_spacing)
     } else if (table_type == "twoway-ci") {
          table_out <- apa.knit.twoway.ci.for.pdf(table_object, table_note, table_title, line_spacing)
     } else if (table_type == "aovstats") {
          table_out <- apa.knit.aov.for.pdf(table_object, table_note, table_title, line_spacing)
     } else if (table_type == "correlation") {
          table_out <- apa.knit.correlation.for.pdf(table_object, table_note, table_title, line_spacing)
     } else if (table_type == "regression") {
          table_out <- apa.knit.regression.for.pdf(table_object, table_note, table_title, line_spacing)
     } else if (table_type == "ezanova") {
          table_out <- apa.knit.ezanova.for.pdf(table_object, table_note, table_title, line_spacing)
     } else if (table_type == "dvalue") {
          table_out <- apa.knit.dvalue.for.pdf(table_object, table_note, table_title, line_spacing)
     }


     return(table_out)
}

apa.knit.dvalue.for.pdf <- function(table_object, table_note, table_title, line_spacing){
     table_df <- table_object$latex.body
     rownames(table_df) <- NULL

     if (is.null(table_note)) {
          table_note          <- table_object$latex.table.note
     }

     if (is.null(table_title)) {
          table_title         <- table_object$latex.table.title
     }

     num_columns = dim(table_df)[2]


     table_column_labels <- c("Variable", "$M$", "$SD$", seq(1:(num_columns-3)))
     column_alignment = rep("c", num_columns)
     column_alignment[1] <- "l"

     table_out <- kableExtra::kbl(table_df, booktabs = T, escape = FALSE,
                                  col.names = table_column_labels,
                                  format = "latex",
                                  align = column_alignment,
                                  caption = table_title, linesep = "")

     table_out <- kableExtra::kable_styling(table_out, position = "left", font_size = 10)
     table_out <- kableExtra::footnote(table_out, escape = FALSE, general = table_note, general_title = "", threeparttable = T)


     if (table_object$landscape == TRUE) {
          table_out <- kableExtra::landscape(table_out)
     }

     #adjust line spacing
     table_spacing <- "\\renewcommand{\\arraystretch}{XX}"
     table_spacing <- gsub(pattern = "XX", replacement = as.character(line_spacing), table_spacing)
     end_spacing <- "\n\\renewcommand{\\arraystretch}{1}\n "
     table_out[1] <- paste0(table_spacing, table_out[1], end_spacing)

     return(table_out)
}




apa.knit.ezanova.for.pdf <- function(table_object, table_note, table_title, line_spacing){


     table_df <- table_object$latex.body
     rownames(table_df) <- NULL

     if (is.null(table_note)) {
          table_note          <- table_object$latex.table.note
     }

     if (is.null(table_title)) {
          table_title         <- table_object$latex.table.title
     }

     num_columns = dim(table_df)[2]

     table_column_labels <- get_latex_ezanova_labels(table_df)
     column_alignment = rep("c", num_columns)
     column_alignment[1] <- "l"

     table_out <- kableExtra::kbl(table_df, booktabs = T, escape = FALSE,
                                  col.names = table_column_labels,
                                  format = "latex",
                                  align = column_alignment,
                                  caption = table_title, linesep = "")

     table_out <- kableExtra::kable_styling(table_out, position = "left", font_size = 10)
     table_out <- kableExtra::footnote(table_out, escape = FALSE, general = table_note, general_title = "", threeparttable = T)

     if (table_object$landscape == TRUE) {
          table_out <- kableExtra::landscape(table_out)
     }

     #adjust line spacing
     table_spacing <- "\\renewcommand{\\arraystretch}{XX}"
     table_spacing <- gsub(pattern = "XX", replacement = as.character(line_spacing), table_spacing)
     end_spacing <- "\n\\renewcommand{\\arraystretch}{1}\n "
     table_out[1] <- paste0(table_spacing, table_out[1], end_spacing)

     return(table_out)
}



apa.knit.regression.for.pdf <- function(table_object, table_note, table_title, line_spacing){

     table_df <- table_object$latex.body
     rownames(table_df) <- NULL

     if (is.null(table_note)) {
          table_note          <- table_object$latex.table.note
     }

     if (is.null(table_title)) {
          table_title         <- table_object$latex.table.title
     }

     num_columns = dim(table_df)[2]


     table_column_labels <- c("Predictor",
                              "$b$",
                              "95\\% CI",
                              "$beta$",
                              "95\\% CI",
                              "Unique $R^2$",
                              "95\\% CI",
                              "$r$",
                              "Fit",
                              "$\\Delta$ Fit")

     column_alignment = c("l","r", "r", "c", "r","c","r","c","c","c")

     if (num_columns == 9) {
          table_column_labels <- table_column_labels[1:9]
          column_alignment <- column_alignment[1:9]
     }


     table_out <- kableExtra::kbl(table_df, booktabs = T, escape = FALSE,
                                  col.names = table_column_labels,
                                  format = "latex",
                                  align = column_alignment,
                                  caption = table_title, linesep = "")

     table_out <- kableExtra::kable_styling(table_out, position = "left", font_size = 10)
     table_out <- kableExtra::footnote(table_out, escape = FALSE, general = table_note, general_title = "", threeparttable = T)

     if (table_object$landscape == TRUE) {
          table_out <- kableExtra::landscape(table_out)
     }

     #adjust line spacing
     table_spacing <- "\\renewcommand{\\arraystretch}{XX}"
     table_spacing <- gsub(pattern = "XX", replacement = as.character(line_spacing), table_spacing)
     end_spacing <- "\n\\renewcommand{\\arraystretch}{1}\n "
     table_out[1] <- paste0(table_spacing, table_out[1], end_spacing)

     return(table_out)
}





apa.knit.correlation.for.pdf <- function(table_object, table_note, table_title, line_spacing){
     table_df <- table_object$latex.body
     rownames(table_df) <- NULL

     if (is.null(table_note)) {
          table_note          <- table_object$latex.table.note
     }

     if (is.null(table_title)) {
          table_title         <- table_object$latex.table.title
     }

     num_columns = dim(table_df)[2]

     table_column_labels <- colnames(table_df)
     table_column_labels[2] <- "$M$"
     table_column_labels[3] <- "$SD$"

     table_out <- kableExtra::kbl(table_df, booktabs = T, escape = FALSE,
                                  col.names = table_column_labels,
                                  format = "latex",
                                  align = c("l", "r", "r", rep("c", num_columns-3)),
                                  caption = table_title, linesep = "")

     table_out <- kableExtra::kable_styling(table_out, position = "left", font_size = 10)
     table_out <- kableExtra::footnote(table_out, escape = FALSE, general = table_note, general_title = "", threeparttable = T)

     if (table_object$landscape == TRUE) {
          table_out <- kableExtra::landscape(table_out)
     }


     #adjust line spacing
     table_spacing <- "\\renewcommand{\\arraystretch}{XX}"
     table_spacing <- gsub(pattern = "XX", replacement = as.character(line_spacing), table_spacing)
     end_spacing <- "\n\\renewcommand{\\arraystretch}{1}\n "
     table_out[1] <- paste0(table_spacing, table_out[1], end_spacing)

     return(table_out)
}




apa.knit.aov.for.pdf <- function(table_object, table_note, table_title, line_spacing){

     table_df <- table_object$latex.body
     rownames(table_df) <- NULL

     if (is.null(table_note)) {
          table_note          <- table_object$latex.table.note
     }

     if (is.null(table_title)) {
          table_title         <- table_object$latex.table.title
     }

     if (table_object$ci.conf.level == .95) {
          ci_txt_str <-  "95\\% CI"
     } else {
          ci_txt_str <-  "90\\% CI"
     }



     table_column_labels <- c("Predictor",
                              "$SS$",
                              "$df$",
                              "$MS$",
                              "$F$",
                              "$p$",
                              "$\\eta_{partial}^2$",
                              ci_txt_str)

     table_out <- kableExtra::kbl(table_df, booktabs = T, escape = FALSE,
                                  col.names = table_column_labels,
                                  format = "latex",
                                  align = c("l", rep("r", 6), "c"),
                                  caption = table_title)

     # if (table_object$ci.conf.level == .95) {
     #      table_out <- kableExtra::add_header_above(table_out, c(" " = 7, "$\\\\eta_{partial}^2$ 95\\\\% CI " = 1), escape = FALSE)
     # } else {
     #      table_out <- kableExtra::add_header_above(table_out, c(" " = 7, "$\\\\eta_{partial}^2$ 90\\\\% CI " = 1), escape = FALSE)
     # }

     table_out <- kableExtra::kable_styling(table_out, position = "left", font_size = 10)
     table_out <- kableExtra::footnote(table_out, escape = FALSE, general = table_note, general_title = "", threeparttable = T)

     #adjust line spacing
     table_spacing <- "\\renewcommand{\\arraystretch}{XX}"
     table_spacing <- gsub(pattern = "XX", replacement = as.character(line_spacing), table_spacing)
     end_spacing <- "\n\\renewcommand{\\arraystretch}{1}\n "
     table_out[1] <- paste0(table_spacing, table_out[1], end_spacing)

     return(table_out)
}

apa.knit.twoway.ci.for.pdf <- function(table_object, table_note, table_title, line_spacing){

     if (is.null(table_note)) {
          table_note          <- table_object$latex.table.note
     }

     if (is.null(table_title)) {
          table_title         <- table_object$latex.table.title
     }



     table_df <- table_object$latex.body
     rownames(table_df) <- NULL

     latex_group_names <- table_object$latex.group.names
     latex_num_groups <- length(latex_group_names)
     latex_group_num_per_group <- table_object$latex.group.num.per.group

     table_column_labels <- table_object$latex.column.labels
     table_column_center <- table_object$latex.column.centering



     table_out <- kableExtra::kbl(table_df, booktabs = T, escape = FALSE,
                                  col.names = table_column_labels,
                                  format = "latex",
                                  align = c("l","r","c","r"),
                                  caption = table_title)

     group_rows <- c(1, latex_group_num_per_group)
     for (i in 1:latex_num_groups) {
          cur_level_name <- latex_group_names[i]
          cur_group_rows <- group_rows + (i-1)*latex_group_num_per_group
          table_out <- kableExtra::pack_rows(table_out, cur_level_name, cur_group_rows[1], cur_group_rows[2], bold = FALSE)
     }

     #table_out <- kableExtra::add_header_above(table_out, c(" " = 2, "95\\\\% CI", " "), escape = FALSE)

     table_out <- kableExtra::kable_styling(table_out, position = "left", font_size = 10)
     table_out <- kableExtra::footnote(table_out, escape = FALSE, general = table_note, general_title = "", threeparttable = T)

     #adjust line spacing
     table_spacing <- "\\renewcommand{\\arraystretch}{XX}"
     table_spacing <- gsub(pattern = "XX", replacement = as.character(line_spacing), table_spacing)
     end_spacing <- "\n\\renewcommand{\\arraystretch}{1}\n "
     table_out[1] <- paste0(table_spacing, table_out[1], end_spacing)

     return(table_out)
}

apa.knit.twoway.for.pdf <- function(table_object, table_note, table_title, line_spacing){


     if (is.null(table_note)) {
          table_note          <- table_object$latex.table.note
     }

     if (is.null(table_title)) {
          table_title         <- table_object$latex.table.title
     }

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



     table_out <- kableExtra::kbl(table_df, booktabs = T, escape = FALSE,
                                  col.names = table_column_labels,
                                  format = "latex",
                                  align = table_column_center,
                                  caption = table_title)

     table_out <- kableExtra::add_header_above(table_out, header1_spacing, escape = FALSE)
     table_out <- kableExtra::add_header_above(table_out, header2_spacing, escape = FALSE)
     table_out <- kableExtra::kable_styling(table_out, position = "left", font_size = 10)

     if (is_marginal_means == TRUE) {
      table_out <- kableExtra::row_spec(table_out, (y.num.columns-1), hline_after = TRUE)
     }
     table_out <- kableExtra::footnote(table_out, escape = FALSE, general = table_note, general_title = "", threeparttable = T)

     if (table_object$landscape == TRUE) {
          table_out <- kableExtra::landscape(table_out)
     }

     #adjust line spacing
     table_spacing <- "\\renewcommand{\\arraystretch}{XX}"
     table_spacing <- gsub(pattern = "XX", replacement = as.character(line_spacing), table_spacing)
     end_spacing <- "\n\\renewcommand{\\arraystretch}{1}\n "
     table_out[1] <- paste0(table_spacing, table_out[1], end_spacing)

     return(table_out)
}


apa.knit.oneway.for.pdf <- function(table_object, table_note, table_title, line_spacing){


     if (is.null(table_note)) {
          table_note          <- table_object$latex.table.note
     }

     if (is.null(table_title)) {
          table_title         <- table_object$latex.table.title
     }

     table_df <- table_object$table.body

     table_column_labels <- table_object$latex.column.labels
     table_column_center <- table_object$latex.column.centering


     table_out <- kableExtra::kbl(table_df, booktabs = T, escape = FALSE,
                                  col.names = table_column_labels,
                                  format = "latex",
                                  align = table_column_center,
                                  caption = table_title)

     # if (dim(table_df)[2]>3) {
     #      table_out <- kableExtra::add_header_above(table_out, c(" " = 2, "95\\\\% CI" =1, " "), escape = FALSE)
     # }
     table_out <- kableExtra::kable_styling(table_out, position = "left", font_size = 10)
     table_out <- kableExtra::footnote(table_out, escape = FALSE, general = table_note, general_title = "", threeparttable = T)

     #adjust line spacing
     table_spacing <- "\\renewcommand{\\arraystretch}{XX}"
     table_spacing <- gsub(pattern = "XX", replacement = as.character(line_spacing), table_spacing)
     end_spacing <- "\n\\renewcommand{\\arraystretch}{1}\n "
     table_out[1] <- paste0(table_spacing, table_out[1], end_spacing)

     return(table_out)
}




ezanova_latex_column_names <- function(column_name) {
     switch(column_name,
            Predictor = "Predictor",
            df_num = "$df_{Num}$",
            df_den = "$df_{Dem}$",
            SS_num = "$SS_{Num}$",
            SS_den = "$SS_{Dem}$",
            Epsilon = "Epsilon",
            Fvalue ="$F$",
            p ="$p$",
            ges ="$\\eta_{g}^2$")
}


get_latex_ezanova_labels <- function(df) {
     n <- names(df)
     id <- n == "F"
     n[id]<-"Fvalue"

     names_out <- c()
     for (i in 1:length(n)) {
          names_out[i] <-ezanova_latex_column_names(n[i])
     }

     return(names_out)
}
