#' Creates a regresion table in APA style
#' @param ... Regression (i.e., lm) result objects. Typically, one for each block in the regression.
#' @param filename (optional) Output filename document filename (must end in .rtf or .doc only)
#' @param table.number  Integer to use in table number output line
#' @return APA table object
#' @examples
#' # View top few rows of goggles data set
#' # from Discovering Statistics Using R
#' head(album)
#'
#' # Single block example
#' blk1 <- lm(sales ~ adverts + airplay, data=album)
#' apa.reg.table(blk1)
#' apa.reg.table(blk1,filename="exRegTable.doc")
#'
#' # Two block example, more than two blocks can be used
#' blk1 <- lm(sales ~ adverts, data=album)
#' blk2 <- lm(sales ~ adverts + airplay + attract, data=album)
#' apa.reg.table(blk1,blk2,filename="exRegBlocksTable.doc")
#'
#' # Interaction product-term test with blocks
#' blk1 <- lm(sales ~ adverts + airplay, data=album)
#' blk2 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data=album)
#' apa.reg.table(blk1,blk2,filename="exInteraction1.doc")
#'
#' # Interaction product-term test with blocks and additional product terms
#' blk1<-lm(sales ~ adverts + airplay, data=album)
#' blk2<-lm(sales ~ adverts + airplay + I(adverts*adverts) + I(airplay*airplay), data=album)
#' blk3<-lm(sales~adverts+airplay+I(adverts*adverts)+I(airplay*airplay)+I(adverts*airplay),data=album)
#' apa.reg.table(blk1,blk2,blk3,filename="exInteraction2.doc")
#'
#' # V1: Interaction product-term test with single regression (i.e., semi-partial correlation focus)
#' blk1 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data=album)
#' apa.reg.table(blk1,filename="exInteraction3.doc")
#'
#' # V2: Interaction product-term test with single regression (i.e., semi-partial correlation focus)
#' blk1<-lm(sales~adverts*airplay,data=album)
#' apa.reg.table(blk1,filename="exInteraction4.doc")
#' @export
apa.reg.table<-function(...,filename=NA,table.number=NA) {
     regression.results.list <- list(...)
     if (is.na(filename)) {
          make.file.flag=FALSE
     } else {
          make.file.flag=TRUE
     }

     L=length(regression.results.list)
     is.same.criterion <- c()
     first.result <- regression.results.list[[1]]
     first.criterion <- colnames(first.result$model)[1]
     for (i in 1:L) {
          cur.result <- regression.results.list[[i]]
          cur.criterion.name <- colnames(cur.result$model)[1]
          is.same.criterion[i] <- first.criterion == cur.criterion.name
     }
     if (any(is.same.criterion==FALSE)) {
          cat("apa.reg.table error:\nAll regression objects (i.e., blocks) must use the same criterion.\n")
          cat("The regression objects used had different criterion variables.\n\n")
          return(FALSE)
     }



     is.same.predictors<- c()
     first.result <- regression.results.list[[1]]
     first.model <- first.result$model
     last.model.number.predictors <- dim(first.model)[2]
     last.predictors <- colnames(first.result$model)[2:last.model.number.predictors]
     for (i in 1:L) {
          cur.result <- regression.results.list[[i]]
          cur.model <- cur.result$model
          cur.model.number.predictors <- dim(cur.model)[2]
          cur.predictors <- colnames(cur.model)[2:cur.model.number.predictors]
          is.same.predictors[i] <- all(intersect(last.predictors,cur.predictors) == last.predictors)
          last.predictors = cur.predictors
     }
     if (any(is.same.predictors==FALSE)) {
          cat("apa.reg.table error:\nEach regression objects (i.e., block) must contain all of the predictors from the preceeding regression object (i.e., block).\n\n")
          cat("For example:\n")
          cat("block1 <- lm(y ~ a + b)\n")
          cat("block2 <- lm(y ~ a + b + c)\n\n")
          cat("The second block contains all of the predictors from the first block plus additional predictors.\n\n")
          cat("Therefore the command below will work: \n\n")
          cat("apa.reg.table(block1, block2)\n\n")
          return(FALSE)
     }



     #get analyses for each block
     block.results <- list()
     L=length(regression.results.list)
     for (i in 1:L) {
          cur.result <- apa.single.block(regression.results.list[[i]])
          block.results[[i]] <- cur.result
     }


     is.multiple.blocks = FALSE
     if (L>1) {
          is.multiple.blocks = TRUE
     }

     #get differences between models
     model.diffs <- list()
     if (is.multiple.blocks == TRUE) {
          for (i in 2:L) {
               model.diffs[[i-1]] <- delta.stats(regression.results.list[[i]],regression.results.list[[i-1]])
          }
     }

     blockout.txt <- model.prep.print(model.in=block.results[[1]], model.number=1, rtf.flag=FALSE)
     blockout.rtf <- model.prep.print(model.in=block.results[[1]], model.number=1, rtf.flag=TRUE)


     #Combine blocks
     if (is.multiple.blocks == TRUE) {
          for (i in 2:L) {
               cur.block <- block.results[[i]]
               cur.block.out.txt <- model.prep.print(model.in=cur.block, model.number=i, past.model.diff = model.diffs[[i-1]], rtf.flag=FALSE)
               cur.block.out.rtf <- model.prep.print(model.in=cur.block, model.number=i, past.model.diff = model.diffs[[i-1]], rtf.flag=TRUE)

               blockout.txt <- rbind(blockout.txt,cur.block.out.txt)
               blockout.rtf <- rbind(blockout.rtf,cur.block.out.rtf)
          }
     } else {
          #remove delta fit column
          blockout.txt.names <- names(blockout.txt)
          blockout.txt.names[8] <- ""
          names(blockout.txt) <- blockout.txt.names
     }


     #console table
     table.title <- sprintf("Regression results using %s as the criterion\n",first.criterion)
     table.body <- blockout.txt
     table.note <- "Note. * indicates p < .05; ** indicates p < .01.\nA significant b-weight indicates the beta-weight and semi-partial correlation are also significant.\nb represents unstandardized regression weights; SE represents the standard error of the \nunstandardized regression weights; beta indicates the beta-weights or standardized regression weights; \nsr2 represents the semi-partial correlation squared;r represents the zero-order correlation.\n"
     tbl.console <- list(table.number = table.number,
                         table.title = table.title,
                         table.body = table.body,
                         table.note = table.note)
     class(tbl.console) <- "apa.table"



     if (make.file.flag==TRUE) {
          table.title <- sprintf("Regression results using %s as the criterion\n",first.criterion)
          #table.note <- "* indicates {\\i p} < .05; ** indicates {\\i p} < .01."
          table.note <- "* indicates {\\i p} < .05; ** indicates {\\i p} < .01. A significant {\\i b}-weight indicates the beta-weight and semi-partial correlation are also significant. {\\i b} represents unstandardized regression weights; {\\i SE} represents the standard error of the unstandardized regression weights; {\\i beta} indicates the beta-weights or standardized regression weights; {\\i sr\\super 2\\nosupersub} represents the semi-partial correlation squared; {\\i r} represents the zero-order correlation."
          #set columns widths and names
          n.col <- .65
          w.col <- 1
          w.col2 <- 1.5
          if (is.multiple.blocks == TRUE) {
               colwidths <- c(w.col, n.col*1.5, n.col, n.col, n.col, n.col, w.col2,w.col2)
               new.col.names <- c(" ","{\\i b}","{\\i SE}","{\\i beta}","{\\i sr\\super 2 \\nosupersub}","{\\i r}","Fit","Change in Fit")
               extend.columns.end <- c(7,8)
          } else {
               colwidths <- c(w.col, n.col*1.5, n.col, n.col, n.col, n.col, w.col2)
               new.col.names <- c(" ","{\\i b}","{\\i SE}","{\\i beta}","{\\i sr\\super 2 \\nosupersub}","{\\i r}","Fit")
               blockout.rtf <- blockout.rtf[,1:7]
               extend.columns.end <- c(7)
          }
          extend.columns.start <- c(1,2)

          regressionTable.table <- as.matrix(blockout.rtf)
          colnames(regressionTable.table) <- new.col.names


          #Create RTF code
          rtfTable <- RtfTable$new(isHeaderRow=TRUE, defaultDecimalTableProportionInternal=.15)
          rtfTable$setTableContent(regressionTable.table)
          rtfTable$setCellWidthsInches(colwidths)
          rtfTable$setRowSecondColumnDecimalTab(.4)
          txt.body <- rtfTable$getTableAsRTF(FALSE,FALSE)


           if (is.multiple.blocks==TRUE) {
               write.rtf.table(filename = filename,txt.body = txt.body,table.title = table.title, table.note = table.note,landscape=TRUE,table.number=table.number)
           } else {
                write.rtf.table(filename = filename,txt.body = txt.body,table.title = table.title, table.note = table.note,table.number=table.number)
           }

     }


     return(tbl.console)
}


apa.single.block<-function(regression.results) {
     b.and.se.matrix<-summary(regression.results)$coefficients
     b.values=b.and.se.matrix[,1]
     se.values=b.and.se.matrix[,2]
     t.values=b.and.se.matrix[,3]
     p.values=b.and.se.matrix[,4]

     col_names <- names(regression.results$model)

     is_weighted <- "(weights)" %in% col_names
     if (is_weighted==TRUE) {
          beta.values<-rep(NA,(length(t.values)-1))
     } else {
          beta.values<-rockchalk::standardize(regression.results)$coefficients

     }

     predictor.names.intercept <- names(b.values)[1:length(b.values)] # excludes (Intercept)
     predictor.names <- names(b.values)[2:length(b.values)] # excludes (Intercept)
     names(beta.values) <- predictor.names
     beta.values<-c(0,beta.values) #placeholder 0 for intercept

     #get sr-squared Version 2
     tsq <- t.values^2
     R2 <- summary(regression.results)$r.squared
     df2 <- summary(regression.results)$fstatistic[3]
     sr2 <- tsq*(1-R2)/df2

     #get zero-order correlations
     regression.data <-regression.results$model #get raw data from regression
     my.criterion <- regression.data[,1]
     my.predictors <- model.matrix(regression.results$terms,regression.results$model) #MAJOR CHANGE
     my.predictors <- as.data.frame(my.predictors[,2:ncol(my.predictors),drop=FALSE]) #MAJOR CHANGE


     cor.info <- get.cors(my.criterion,my.predictors)
     cor.values <- cor.info$r
     cor.values <- c(0,cor.values) #placeholder for intercept
     r.p.values <- cor.info$p
     r.p.values <- c(0,r.p.values) #placeholder for intercept

     #make summary table
     model.summary.numeric<-data.frame(predictor.names.intercept,b.values,se.values,beta.values,sr2,cor.values,stringsAsFactors = FALSE)

     #format numbers as strings in main table
     model.summary.txt <- model.summary.numeric
     format.strings <- list("","%5.2f","%5.2f","%0.2f","%0.2f","%0.2f")
     for (i in 2:6) {
          #convert numeric columns to strings
          model.summary.txt[,i] <-sprintf(format.strings[[i]],model.summary.numeric[,i])
     }
     names(model.summary.txt) <- names(model.summary.numeric)
     model.summary.txt$cor.values <- strip.leading.zero(model.summary.txt$cor.values)
     model.summary.txt$cor.values <- add.sig.stars(string.in = model.summary.txt$cor.values, p.values.in = r.p.values)

     #Remove correlations for product terms
     cor.values <- model.summary.txt$cor.values
     product.row.numbers <- is.product.row(predictor.names.intercept)
     cor.values[product.row.numbers] <- ""
     model.summary.txt$cor.values <- cor.values

     model.summary.txt$sr2 <- strip.leading.zero(model.summary.txt$sr2)
     model.summary.txt$b.values   <- add.sig.stars(string.in = model.summary.txt$b.values, p.values.in = p.values)





     #add needed blank rows and columns
     blank.column <- rep(" ",length(model.summary.txt$b.values))
     fit <- blank.column
     deltafit <- blank.column
     model.summary.txt$fit <- fit
     model.summary.txt$deltafit <- deltafit

     blank.row <- rep(" ",length(model.summary.txt[1,]))
     model.summary.txt <- rbind(model.summary.txt, blank.row)
     model.summary.txt <- rbind(model.summary.txt, blank.row)
     model.summary.txt <- rbind(model.summary.txt, blank.row)
     model.summary.txt <- rbind(blank.row, model.summary.txt)


     regression.results.summary <- summary(regression.results)
     R2.value <-regression.results.summary$r.squared
     Fvalue=regression.results.summary$fstatistic[1]
     df1=regression.results.summary$fstatistic[2]
     df2=regression.results.summary$fstatistic[3]
     Fvalue.pvalue=1-pf(Fvalue,df1,df2,lower.tail=T)

     R2.report.rtf <- rtf.R2(R2.value=R2.value,p.value=Fvalue.pvalue)
     R2.report.txt <- txt.R2(R2.value=R2.value,p.value=Fvalue.pvalue)

     Fvalue.report.rtf <- rtf.F(Fvalue=Fvalue,df1=df1,df2 = df2)
     Fvalue.report.txt <- txt.F(Fvalue=Fvalue,df1=df1,df2 = df2)

     #fix names
     colnames(model.summary.txt) <- c("variables","b","SE","beta","sr2","r","fit","delta")
     output <- list()

     model.summary.txt[2,4]<-""
     model.summary.txt[2,5]<-""
     model.summary.txt[2,6]<-""
     output$model.summary <- model.summary.txt

     model.stats <- list()
     model.stats$R2value <- R2.value
     model.stats$Fvalue <- Fvalue
     model.stats$df1 <- df1
     model.stats$df2 <- df2
     model.stats$R2.report.rtf <- R2.report.rtf
     model.stats$R2.report.txt <- R2.report.txt
     model.stats$Fvalue.report.rtf <- Fvalue.report.rtf
     model.stats$Fvalue.report.txt <- Fvalue.report.txt
     output$model.stats <- model.stats

     return(output)
}



delta.stats <- function(reg2,reg1) {
     R2.new <- summary(reg2)$r.squared
     R2.old <- summary(reg1)$r.squared

     delta.anova.results <- anova(reg1,reg2)
     delta.Fvalue <- delta.anova.results$F[2]
     delta.df1 <- delta.anova.results$Df[2]
     delta.df2 <- delta.anova.results$Res.Df[2]
     delta.p.value <- delta.anova.results[["Pr(>F)"]][2]

     deltaR2 <- round((R2.new-R2.old),3)
     deltaR2.txt <- add.sig.stars(strip.leading.zero(sprintf("%1.3f",deltaR2)),delta.p.value)

     R2out.txt <- sprintf("Delta R2 = %s", deltaR2.txt)
     R2out.rtf <- sprintf("\\u0916\3{\\i R\\super 2 \\nosupersub}  = %s", deltaR2.txt)

     Fout.txt <- sprintf("F(%d, %d) = %1.2f",delta.df1,delta.df2, delta.Fvalue)
     Fout.rtf <- sprintf("{\\i F}(%d, %d) = %1.2f",delta.df1,delta.df2, delta.Fvalue)

     output.txt <- list()
     output.txt$R2out.txt <- R2out.txt
     output.txt$R2out.rtf <- R2out.rtf
     output.txt$Fout.txt <- Fout.txt
     output.txt$Fout.rtf <- Fout.rtf

     return(output.txt)
}





model.prep.print <- function(model.in=NA, model.number=1,past.model.diff=NA, rtf.flag=FALSE) {
     model.in.summary <- model.in$model.summary
     num.model.rows <- nrow(model.in.summary)

     model.in.summary$variables[1] <- sprintf("Model %d",model.number)


     if (rtf.flag == FALSE) {
          model.in.summary$fit[num.model.rows-1] <- model.in$model.stats$Fvalue.report.txt # put in F Stuff
          model.in.summary$fit[num.model.rows-2] <- model.in$model.stats$R2.report.txt     # put in R Stuff
          if (!is.na(past.model.diff[1])) {
               model.in.summary$delta[num.model.rows-1] <- past.model.diff$Fout.txt   # put in F Stuff
               model.in.summary$delta[num.model.rows-2] <- past.model.diff$R2out.txt # put in R Stuff
          }
     } else {
          model.in.summary$fit[num.model.rows-1] <- model.in$model.stats$Fvalue.report.rtf # put in F Stuff
          model.in.summary$fit[num.model.rows-2] <- model.in$model.stats$R2.report.rtf     # put in R Stuff
          if (!is.na(past.model.diff[1])) {
               model.in.summary$delta[num.model.rows-1] <- past.model.diff$Fout.rtf  # put in F Stuff
               model.in.summary$delta[num.model.rows-2] <- past.model.diff$R2out.rtf # put in R Stuff
          }
     }


     return(model.in.summary)
}


is.product.row <- function(row_names) {
     is.a.colon <- grep(":",row_names)
     is.a.star <- grep("\\*",row_names)
     is.a.product <- unique(sort(c(is.a.colon,is.a.star)))
     return(is.a.product)
}

#' @export
apa.rt<-function(...,filename=NA,table.number=NA) {
     regression.results.list <- list(...)
     if (is.na(filename)) {
          make.file.flag=FALSE
     } else {
          make.file.flag=TRUE
     }

     L=length(regression.results.list)
     is.same.criterion <- c()
     first.result <- regression.results.list[[1]]
     first.criterion <- colnames(first.result$model)[1]
     for (i in 1:L) {
          cur.result <- regression.results.list[[i]]
          cur.criterion.name <- colnames(cur.result$model)[1]
          is.same.criterion[i] <- first.criterion == cur.criterion.name
     }
     if (any(is.same.criterion==FALSE)) {
          cat("apa.reg.table error:\nAll regression objects (i.e., blocks) must use the same criterion.\n")
          cat("The regression objects used had different criterion variables.\n\n")
          return(FALSE)
     }



     is.same.predictors<- c()
     first.result <- regression.results.list[[1]]
     first.model <- first.result$model
     last.model.number.predictors <- dim(first.model)[2]
     last.predictors <- colnames(first.result$model)[2:last.model.number.predictors]
     for (i in 1:L) {
          cur.result <- regression.results.list[[i]]
          cur.model <- cur.result$model
          cur.model.number.predictors <- dim(cur.model)[2]
          cur.predictors <- colnames(cur.model)[2:cur.model.number.predictors]
          is.same.predictors[i] <- all(intersect(last.predictors,cur.predictors) == last.predictors)
          last.predictors = cur.predictors
     }
     if (any(is.same.predictors==FALSE)) {
          cat("apa.reg.table error:\nEach regression objects (i.e., block) must contain all of the predictors from the preceeding regression object (i.e., block).\n\n")
          cat("For example:\n")
          cat("block1 <- lm(y ~ a + b)\n")
          cat("block2 <- lm(y ~ a + b + c)\n\n")
          cat("The second block contains all of the predictors from the first block plus additional predictors.\n\n")
          cat("Therefore the command below will work: \n\n")
          cat("apa.reg.table(block1, block2)\n\n")
          return(FALSE)
     }



     #get analyses for each block
     block.results <- list()
     L=length(regression.results.list)
     for (i in 1:L) {
          cur.result <- apa.single.block(regression.results.list[[i]])
          block.results[[i]] <- cur.result
     }


     is.multiple.blocks = FALSE
     if (L>1) {
          is.multiple.blocks = TRUE
     }

     #get differences between models
     model.diffs <- list()
     if (is.multiple.blocks == TRUE) {
          for (i in 2:L) {
               model.diffs[[i-1]] <- delta.stats(regression.results.list[[i]],regression.results.list[[i-1]])
          }
     }

     blockout.txt <- model.prep.print(model.in=block.results[[1]], model.number=1, rtf.flag=FALSE)
     blockout.rtf <- model.prep.print(model.in=block.results[[1]], model.number=1, rtf.flag=TRUE)


     #Combine blocks
     if (is.multiple.blocks == TRUE) {
          for (i in 2:L) {
               cur.block <- block.results[[i]]
               cur.block.out.txt <- model.prep.print(model.in=cur.block, model.number=i, past.model.diff = model.diffs[[i-1]], rtf.flag=FALSE)
               cur.block.out.rtf <- model.prep.print(model.in=cur.block, model.number=i, past.model.diff = model.diffs[[i-1]], rtf.flag=TRUE)

               blockout.txt <- rbind(blockout.txt,cur.block.out.txt)
               blockout.rtf <- rbind(blockout.rtf,cur.block.out.rtf)
          }
     } else {
          #remove delta fit column
          blockout.txt.names <- names(blockout.txt)
          blockout.txt.names[8] <- ""
          names(blockout.txt) <- blockout.txt.names
     }


     #console table
     table.title <- sprintf("Regression results using %s as the criterion\n",first.criterion)
     table.body <- blockout.txt
     table.note <- "Note. * indicates p < .05; ** indicates p < .01.\nA significant b-weight indicates the beta-weight and semi-partial correlation are also significant.\nb represents unstandardized regression weights; SE represents the standard error of the \nunstandardized regression weights; beta indicates the beta-weights or standardized regression weights; \nsr2 represents the semi-partial correlation squared;r represents the zero-order correlation.\n"
     tbl.console <- list(table.number = table.number,
                         table.title = table.title,
                         table.body = table.body,
                         table.note = table.note)
     class(tbl.console) <- "apa.table"



     if (make.file.flag==TRUE) {
          table.title <- sprintf("Regression results using %s as the criterion\n",first.criterion)
          #table.note <- "* indicates {\\i p} < .05; ** indicates {\\i p} < .01."
          table.note <- "* indicates {\\i p} < .05; ** indicates {\\i p} < .01. A significant {\\i b}-weight indicates the beta-weight and semi-partial correlation are also significant. {\\i b} represents unstandardized regression weights; {\\i SE} represents the standard error of the unstandardized regression weights; {\\i beta} indicates the beta-weights or standardized regression weights; {\\i sr\\super 2\\nosupersub} represents the semi-partial correlation squared; {\\i r} represents the zero-order correlation."
          #set columns widths and names
          n.col <- .65
          w.col <- 1
          w.col2 <- 1.5
          if (is.multiple.blocks == TRUE) {
               colwidths <- c(w.col, n.col*1.5, n.col, n.col, n.col, n.col, w.col2,w.col2)
               new.col.names <- c(" ","{\\i b}","{\\i SE}","{\\i beta}","{\\i sr\\super 2 \\nosupersub}","{\\i r}","Fit","Change in Fit")
               extend.columns.end <- c(7,8)
          } else {
               colwidths <- c(w.col, n.col*1.5, n.col, n.col, n.col, n.col, w.col2)
               new.col.names <- c(" ","{\\i b}","{\\i SE}","{\\i beta}","{\\i sr\\super 2 \\nosupersub}","{\\i r}","Fit")
               blockout.rtf <- blockout.rtf[,1:7]
               extend.columns.end <- c(7)
          }
          extend.columns.start <- c(1,2)

          regressionTable.table <- as.matrix(blockout.rtf)
          colnames(regressionTable.table) <- new.col.names


          #Create RTF code
          rtfTable <- RtfTable$new(isHeaderRow=TRUE, defaultDecimalTableProportionInternal=.15)
          rtfTable$setTableContent(regressionTable.table)
          rtfTable$setCellWidthsInches(colwidths)
          rtfTable$setRowSecondColumnDecimalTab(.4)
          txt.body <- rtfTable$getTableAsRTF(FALSE,FALSE)


          if (is.multiple.blocks==TRUE) {
               write.rtf.table(filename = filename,txt.body = txt.body,table.title = table.title, table.note = table.note,landscape=TRUE,table.number=table.number)
          } else {
               write.rtf.table(filename = filename,txt.body = txt.body,table.title = table.title, table.note = table.note,table.number=table.number)
          }

     }


     return(tbl.console)
}
