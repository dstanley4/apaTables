#' A common task faced by researchers is the creation of APA style (i.e., \emph{American Psychological Association} style) tables from statistical output. In R a large number of function calls are often needed to obtain all of the desired information for a single APA style table. As well, the process of manually creating APA style tables in a word processor is prone to transcription errors. This package creates Word files (.doc files) containing APA style tables for several types of analyses. Using this package minimizes transcription errors and reduces the number commands needed by the user. Examples are provided in this documentation and at \url{http://www.StatsCanBeFun.com}. Currently, the following tables can be created:
#' \itemize{
#'   \item Correlation tables - Correlation tables (with confidence intervals and descriptive statistics) are created from data frames using \code{\link{apa.cor.table}}
#'   \item Single "block" regression tables - Single "block" regression tables are created from a regression object using \code{\link{apa.reg.table}}
#'   \item Multiple "block" regression tables - Multiple "block" regression tables are created from regression objects using \code{\link{apa.reg.table}}
#'   \item ANOVA cell tables - ANOVA mean/standard deviation tables for 1- and 2-way designs are created from data frames using \code{\link{apa.1way.table}} and \code{\link{apa.2way.table}}
#'   \item Standardized mean difference (i.e., \emph{d}-value) tables (with confidence intervals and descriptive statistics) illustrating all possible paired comparisons using a single independent variable are created from data frames using \code{\link{apa.d.table}}
#'  }
#'\tabular{ll}{
#'Package: \tab apaTables\cr
#'Type: \tab Package\cr
#'Version: \tab 1.0.4\cr
#'Date: \tab 2015-011-20\cr
#'License: \tab Unlimited\cr
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





add.sig.stars <- function(string.in,p.values.in) {
     string.out <- string.in
     L <- length(p.values.in)
     for (i in 1:L) {
          cur.p.value<-p.values.in[i]
          if ((cur.p.value<.05) & (cur.p.value>.01)) {
               string.out[i]<-paste(string.in[i],"*",sep="")
          } else if (cur.p.value<.01) {
               string.out[i]<-paste(string.in[i],"**",sep="")
          }
     }          
     return(string.out)
}

txt.ci<- function(cortest.result) {
     ci.interval<-cortest.result$conf.int
     ci.lower<- ci.interval[1]
     ci.upper<- ci.interval[2]
     
     ci.lower.txt <- strip.leading.zero(sprintf("%1.2f",ci.lower))
     ci.upper.txt <- strip.leading.zero(sprintf("%1.2f",ci.upper))
     ci.txt <- sprintf("[%s, %s]",ci.lower.txt,ci.upper.txt)
     return(ci.txt)
}

txt.r <- function(ctest) {
     r.value=ctest$estimate
     p.value=ctest$p.value
     r.value.txt <- strip.leading.zero(sprintf("%1.2f", r.value))
     r.value.txt <- add.sig.stars(r.value.txt,p.value)
     string.out=sprintf("%s",r.value.txt)
     return(string.out)
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
     string.out=sprintf("{\\i F}(%d, %d) = %1.2f",df1,df2,Fvalue)
     return(string.out)
}

txt.F <- function(Fvalue,df1,df2) {
     string.out=sprintf("F(%d, %d) = %1.2f",df1,df2,Fvalue)
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
          cat(sprintf("Table %d",tbl$table.number),"\n")
          cat("\n")
     }
     cat(tbl$table.title,"\n")
     cat("\n")
     print(tbl$table.body,row.names=FALSE,quote=FALSE)
     cat("\n")
     cat(tbl$table.note,"\n")
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


