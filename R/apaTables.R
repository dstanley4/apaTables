#' A common task faced by researchers is the creation of APA style (i.e., \emph{American Psychological Association} style) tables from statistical output. In R a large number of function calls are often needed to obtain all of the desired information for a single APA style table. As well, the process of manually creating APA style tables in a word processor is prone to transcription errors. This package creates Word files (.doc files) and latex code containing APA style tables for several types of analyses. Using this package minimizes transcription errors and reduces the number commands needed by the user. Examples are provided in this documentation and at \url{http://www.StatsCanBeFun.com}.
#'
#'  Bugs and feature requests can be reported at: \url{https://github.com/dstanley4/apaTables/issues}
#'
#'  Tutorial at: \url{https://dstanley4.github.io/apaTables/articles/apaTables.html}
#'
#'    Currently, the following tables can be created:
#' \itemize{
#'   \item Correlation tables - Correlation tables (with confidence intervals and descriptive statistics) are created from data frames using \code{\link{apa.cor.table}}.
#'   \item Single "block" regression tables - Single "block" regression tables are created from a regression object using \code{\link{apa.reg.table}}.
#'   \item Multiple "block" regression tables - Multiple "block" regression tables are created from regression objects using \code{\link{apa.reg.table}}.
#'   \item ANOVA tables - An ANOVA F-table can be created via \code{\link{apa.aov.table}} from a regression object (i.e. lm output or aov output). Cell mean/standard deviation tables for 1- and 2-way designs are created from data frames using \code{\link{apa.1way.table}} and \code{\link{apa.2way.table}}.
#'   \item ezANOVA tables from ez package - An ANOVA F-table from ezANOVA output can be created via \code{\link{apa.ezANOVA.table}}.
#'   \item Standardized mean difference (i.e., \emph{d}-value) tables (with confidence intervals and descriptive statistics) illustrating all possible paired comparisons using a single independent variable are created from data frames using \code{\link{apa.d.table}}.
#'  }
#'\tabular{ll}{
#'Package: \tab apaTables\cr
#'Type: \tab Package\cr
#'Version: \tab 3.0.0\cr
#'Date: \tab 2023-06-29\cr
#'License: \tab MIT\cr
#'}
#'
#'@name apaTables
#'@aliases apaTables
#'@docType package
#'@title Create American Psychological Association (APA) Style Tables
#'@author
#'\tabular{ll}{
#'Author: \tab David J. Stanley \email{dstanley@@uoguelph.ca}\cr
#'Maintainer: \tab David J. Stanley \email{dstanley@@uoguelph.ca}
#'}
#'@importFrom "stats" "anova" "cor.test" "median" "na.omit" "pf" "sd" "t.test" "var"
#'@importFrom "utils" "capture.output"
#'@importFrom "stats" "confint"
#'@importFrom "dplyr" "mutate" "select" "as_tibble"
#'@importFrom "broom" "glance" "tidy"
#'@importFrom "stats" "qnorm" "rnorm" "lm"
#'@importFrom "stringr" "str_to_sentence" "str_to_title"
#'@importFrom "papaja" "apa_table"
utils::globalVariables(c("difference", "predictor","SE","p", "DFd","DFn","Effect","GGe","HFe","SSd","SSn","ges","r","beta_CI"))
NULL


strip.leading.zero <- function(string.in) {
     string.out = string.in
     id.r.is.one <- string.in == "1.00"
     id.r.is.mone <- string.in == "-1.00"
     string.out <- sub(pattern="0.",replacement=".",x=string.in)
     string.out[id.r.is.one] <- "1.00"
     string.out[id.r.is.mone] <- "-1.00"
     return(string.out)
}


txt.ci<- function(cortest.result,strip_zero=TRUE) {
     ci.interval<-cortest.result$conf.int
     ci.lower<- ci.interval[1]
     ci.upper<- ci.interval[2]

     output <- txt.ci.brackets(ci.lower,ci.upper,strip_zero = strip_zero)
     return(output)
}


txt.r <- function(ctest, show_stars = TRUE) {
     r.value=ctest$estimate
     p.value=ctest$p.value
     r.value.txt <- strip.leading.zero(sprintf("%1.2f", r.value))
     if (show_stars == TRUE) {
             r.value.txt <- add.sig.stars(r.value.txt,p.value)
     }
     string.out=sprintf("%s",r.value.txt)
     return(string.out)
}

txt.r.latex <- function(ctest, show_stars = TRUE) {
     r.value=ctest$estimate
     p.value=ctest$p.value
     r.value.txt <- strip.leading.zero(sprintf("%1.2f", r.value))
     if (show_stars == TRUE) {
          r.value.txt <- add.sig.stars.latex(r.value.txt,p.value)
     }
     string.out=sprintf("%s",r.value.txt)
     return(string.out)
}


txt.r.p.console <- function(ctest) {
     r.value=ctest$estimate
     p.value=ctest$p.value

     p_is_small <- FALSE
     if (p.value < .001) {
          p_is_small <- TRUE
     }

     p.value=round(p.value, 3)
     p.value.txt <- strip.leading.zero(sprintf("%1.3f", p.value))
     if (p_is_small == TRUE) {
          p.value.txt <- "p < .001"
     } else {
          p.value.txt <- paste0("p = ", p.value.txt)
     }
     return(p.value.txt)
}

txt.r.p.latex <- function(ctest) {
     r.value=ctest$estimate
     p.value=ctest$p.value

     p_is_small <- FALSE
     if (p.value < .001) {
          p_is_small <- TRUE
     }

     p.value=round(p.value, 3)
     p.value.txt <- strip.leading.zero(sprintf("%1.3f", p.value))
     if (p_is_small == TRUE) {
          p.value.txt <- "$p$ < .001"
     } else {
          p.value.txt <- paste0("$p$ = ", p.value.txt)
     }
     return(p.value.txt)}

txt.r.p.rtf <- function(ctest) {
     r.value=ctest$estimate
     p.value=ctest$p.value

     p_is_small <- FALSE
     if (p.value < .001) {
          p_is_small <- TRUE
     }

     p.value=round(p.value, 3)
     p.value.txt <- strip.leading.zero(sprintf("%1.3f", p.value))
     if (p_is_small == TRUE) {
          p.value.txt <- "{\\i p} < .001"
     } else {
          p.value.txt <- paste0("{\\i p} = ", p.value.txt)
     }
     return(p.value.txt)
}



rtf.R2 <- function(R2.value,p.value) {
     R2.value.txt <- strip.leading.zero(sprintf("%1.3f", R2.value))
     R2.value.txt <- add.sig.stars(R2.value.txt,p.value)
     string.out=sprintf("{\\i R\\super 2 \\nosupersub} = %s",R2.value.txt)
}

txt.R2 <- function(R2.value,p.value) {
     R2.value.txt <- strip.leading.zero(sprintf("%1.3f", R2.value))
     R2.value.txt <- add.sig.stars(R2.value.txt,p.value)
     string.out=sprintf("R2 = %s",R2.value.txt)
}

rtf.F <- function(Fvalue,df1,df2) {
     string.out=sprintf("{\\i F}(%1.0f, %1.0f) = %1.2f",df1,df2,Fvalue)
     return(string.out)
}

txt.F <- function(Fvalue,df1,df2) {
     string.out=sprintf("F(%1.0f, %1.0f) = %1.2f",df1,df2,Fvalue)
     return(string.out)
}


txt.number <- function(number.in) {
     number.out <- sprintf("%1.2f",number.in)
}



add.decimal.tab <- function(string.in) {
     new.string <- paste("{\\tqdec\\tldot\\tx600 ",string.in,"}")
     return(new.string)
}


get.cors <- function(dv.in,pred.in) {
     num.people <- length(dv.in)
     num.pred <- dim(pred.in)[2]
     r.values <-c()
     p.values <-c()
     for (i in 1:num.pred) {
          cur.pred <- pred.in[,i]
          r.out <- stats::cor.test(dv.in,cur.pred)
          r.values[i] <- r.out$estimate
          p.values[i] <- r.out$p.value
     }
     output <- list()
     output$r <- r.values
     output$p <- p.values
     return(output)
}


# Helper functions
#' @export
print.apa.table <- function(x,...) {
     cat("\n\n")
     tbl <- x
     if (!is.na(tbl$table.number)) {
          cat(sprintf("Table %1.0f",tbl$table.number),"\n")
          cat("\n")
     }
     cat(tbl$table.title,"\n")
     cat("\n")
     print(tbl$table.body,row.names=FALSE,quote=FALSE)
     cat("\n")
     cat(tbl$table.note,"\n")
     cat("\n")
}

#' @export
print.apa_table <- function(x,...) {
     cat("\n\n")
     tbl <- x
     if (!is.na(tbl$table_number)) {
          cat(sprintf("Table %1.0f",tbl$table_number),"\n")
          cat("\n")
     }
     cat(tbl$table_title,"\n")
     cat("\n")
     print(tbl$table_body,row.names=FALSE,quote=FALSE)
     cat("\n")
     cat(tbl$table_note,"\n")
     cat("\n")
}



get.ci.mean <- function(x.vector) {
     lower <- stats::t.test(x.vector)$conf.int[1]
     upper <- stats::t.test(x.vector)$conf.int[2]

     output <- list()
     output$lower.conf.limit <- sprintf("%1.2f",lower)
     output$upper.conf.limit <- sprintf("%1.2f",upper)
     return(output)
}

#check if valid names used
is.valid.name <- function(sub.name, data.col.names) {
     is.name.valid <- FALSE
     if (!is.null(sub.name)) {
          is.name.valid <- any(sub.name == data.col.names)
          if (is.name.valid==FALSE){
               cat("apa.mean.table error:\n")
               cat(sprintf("%s is not a valid column name.\n\n",as.character(sub.name)))
          }
     }
     return(is.name.valid)
}

add.sig.stars <- function(string.in,p.values.in) {
     string.out <- string.in
     L <- length(p.values.in)
     for (i in 1:L) {
          cur.p.value<-p.values.in[i]
          if (!is.na(cur.p.value)) {
               if ((cur.p.value<.05) & (cur.p.value>.01)) {
                    string.out[i]<-paste(string.in[i],"*",sep="")
               } else if (cur.p.value<.01) {
                    string.out[i]<-paste(string.in[i],"**",sep="")
               }
          }
     }
     return(string.out)
}

add.sig.stars.latex <- function(string.in,p.values.in) {
     string.out <- string.in
     L <- length(p.values.in)
     for (i in 1:L) {
          cur.p.value<-p.values.in[i]
          if (!is.na(cur.p.value)) {
               if ((cur.p.value<.05) & (cur.p.value>.01)) {
                    string.out[i]<-paste(string.in[i],"$^{*}$",sep="")
               } else if (cur.p.value<.01) {
                    string.out[i]<-paste(string.in[i],"$^{**}$",sep="")
               }
          }
     }
     return(string.out)
}

txt.ci.brackets<- function(LL,UL,strip_zero=TRUE) {
     ci.lower.txt <- sprintf("%1.2f",LL)
     ci.upper.txt <- sprintf("%1.2f",UL)

     if (strip_zero==TRUE) {
          ci.lower.txt <- strip.leading.zero(ci.lower.txt)
          ci.upper.txt <- strip.leading.zero(ci.upper.txt)
     }
     ci.txt <- sprintf("[%s, %s]",ci.lower.txt,ci.upper.txt)
     return(ci.txt)
}

table_without_intercept_row <- function(df) {
     #check if predictor
     predictor_names <- df$predictor
     is_intercept <- FALSE

     if (predictor_names[1]=="(Intercept)") {
          is_intercept <- TRUE
     }

     #make table without intercept
     if (is_intercept==FALSE) {
          df_lower_table <- df
          df_first_row <- NULL
     } else {
          num_table_rows <- dim(df)[1]
          df_lower_table <- df[2:num_table_rows,]
          df_first_row   <- df[1,]
     }

     output<- list()
     output$lower_table  <- df_lower_table
     output$first_row    <- df_first_row
     output$is_intercept <- is_intercept

     return(output)
}

correlations_with_criterion <- function(df) {
     #assumes df is the model data frame from lm output where criterion is the first column

     #remove weights column if present
     column_names      <- colnames(df)
     column_names_good <- column_names!="(weights)"
     column_names      <- column_names[column_names_good]
     df <- df[,column_names]

     output <- data.frame(predictor=character(),r=numeric(),r_pvalue=numeric())

     for (i in 2:length(column_names)) {
          cor_output <- stats::cor.test(df[,1],df[,i])

          predictor      <- colnames(df)[i]
          r              <- cor_output$estimate
          r_pvalue       <- cor_output$p.value

          output_row <- data.frame(predictor,r,r_pvalue,stringsAsFactors = FALSE)
          output     <- rbind(output,output_row)
     }
     return(output)
}



get_empty_row <- function(df) {
     row <- df[1,]
     for (i in 1:length(names(df))) {
          row[,i] <- NA
     }
     return(row)
}

get_blank_row <- function(df) {
     row <- df[1,]
     for (i in 1:length(names(df))) {
          row[,i] <- ""
     }
     return(row)
}

add_row_to_model_summary <- function(df) {
     new_row <- get_blank_row(df)
     df <- rbind(df,new_row)
     return(df)
}

