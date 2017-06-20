#set tabs to zero to center cells
#' @import methods
RtfTable <- setRefClass("RtfTable",
    fields = list(headerHorizontalAlignment = "numeric",
               rowHorizontalAlignment = "numeric",
               isHeaderRow = "logical",
               defaultDecimalTableProportionInternal = "numeric",
               decimalTabProportionInternal = "numeric",
               decimalTabPositionTwipsInternal = "numeric",
               defaultCellWidthInches = "numeric",
               cellWidthsTwipsInternal = "numeric",
               cellWidthsInches = "numeric",
               cellRightSidePositionTwipsInternal = "numeric",
               defaultAlignmentInternal= "numeric",
               numberColumnsInternal = "numeric",
               twipsPerInchInternal = "numeric",
               cellSpaceBorderInternal = "numeric",
               lineThicknessInternal = "numeric",
               tableContentInternal = "matrix",
               leftAlignment = "numeric",
               centerAlignment = "numeric",
               rightAlignment = "numeric",
               noLineAboveColumns = "numeric"
    ),
    methods= list(
         initialize = function(...) {
               isHeaderRow <<- FALSE
               twipsPerInchInternal <<- 1440
               cellSpaceBorderInternal <<- 100
               lineThicknessInternal <<- 30
               numberColumnsInternal <<- 0

               defaultCellWidthInches <<- .85
               defaultDecimalTableProportionInternal <<- .35

               leftAlignment <<- 1
               rightAlignment <<- 2
               centerAlignment <<- 3
               defaultAlignmentInternal <<- centerAlignment

               noLineAboveColumns <<- c(0)

               callSuper(...)
               return(TRUE)
          },

          setCellWidthsInches = function(cellWidths) {
               cellWidthsInches <<- cellWidths
               cellWidthsTwipsInternal <<- cellWidths * twipsPerInchInternal
               for (i in 1:numberColumnsInternal) {
                   cellRightSidePositionTwipsInternal[i] <<- sum(cellWidthsTwipsInternal[1:i])
               }
               .self$updateDecimalTabs()
               return(TRUE)
         },


         updateDecimalTabs = function(){
               decimalTabPositionTwipsInternal <<- cellWidthsTwipsInternal * decimalTabProportionInternal
               return(TRUE)
         },

         setDecimalTabWidthsProportions = function(decimalTabs){
              decimalTabProportionInternal <<- decimalTabs
              .self$updateDecimalTabs()
              return(TRUE)
         },

         getRowContentInRTF = function(rowContent,isExtraSpacing,isCurRowHeaderRow) {
              horizAlignmentFormatCodes <- list(left="\\ql",right="\\qr",center="\\qc")
              rowHorizAlignmentQuad <- unname(unlist(horizAlignmentFormatCodes[rowHorizontalAlignment]))
              headerHorizAlignmentQuad <- unname(unlist(horizAlignmentFormatCodes[headerHorizontalAlignment]))
              headerHorizAlignmentQuad[1]<-rowHorizAlignmentQuad[1]

              cellTabs <- sprintf("\\tqdec\\tldot\\tx%1.0f",decimalTabPositionTwipsInternal) #multiple at once
              noTabId <- decimalTabPositionTwipsInternal == 0
              cellTabs[noTabId] <- ""
              if (isExtraSpacing == TRUE) {
                   spacingAmount <- 100
              } else {
                   spacingAmount <- 0
              }

              formattedRow <- c()
              for (i in 1:numberColumnsInternal) {
                   if (cellTabs[i]=="") {
                        formattedRow[i] <- sprintf("\\pard\\sa%1.0f\\sb%1.0f\\intbl%s{%s}\\cell",
                                                   spacingAmount,spacingAmount,rowHorizAlignmentQuad[i],rowContent[i])
                   } else {
                        formattedRow[i] <- sprintf("\\pard\\sa%1.0f\\sb%1.0f\\intbl%s{%s}\\cell",
                                                   spacingAmount,spacingAmount,cellTabs[i],rowContent[i])
                   }
                   if (isCurRowHeaderRow==TRUE){
                        formattedRow[i] <- sprintf("\\pard\\sa%1.0f\\sb%1.0f\\intbl%s{%s}\\cell",
                                                   spacingAmount,spacingAmount,headerHorizAlignmentQuad[i],rowContent[i])
                   }
              }
              return(formattedRow)
         },

         getRowFormatInRTF = function(isLineAbove,isLineBelow,isThinLine) {
              cellRightSide <- as.integer(cellRightSidePositionTwipsInternal)

              if (isThinLine == TRUE) {
                   curLineThickness <- lineThicknessInternal/2
              } else {
                   curLineThickness <- lineThicknessInternal/2
              }

              tableLineCellDefn <- c()
              outputLineNumber <-1
              isLineAboveInitial <- isLineAbove #new
              for (i in 1:numberColumnsInternal) {
                   if (any(i==noLineAboveColumns)) {
                        isLineAbove=FALSE #new
                   }

                   if (isLineAbove==TRUE){
                        tableLineCellDefn[outputLineNumber] = sprintf("\\clbrdrt\\brdrw%1.0f\\brdrs",curLineThickness)
                        outputLineNumber <- outputLineNumber + 1
                   }
                   if (isLineBelow==TRUE){
                        tableLineCellDefn[outputLineNumber] = sprintf("\\clbrdrb\\brdrw%1.0f\\brdrs",curLineThickness)
                        outputLineNumber <- outputLineNumber + 1
                   }
                   if (i==1) {
                        isLineAbove=isLineAboveInitial #new
                   }

                   tableLineCellDefn[outputLineNumber] = sprintf("\\clvertalc\\cellx%1.0f ",cellRightSide[i])
                   outputLineNumber <- outputLineNumber + 1
              }
              return(tableLineCellDefn)
          },

          getTableAsRTF = function(isExtraSpacing=FALSE,isLineAbove=FALSE,isLineBelow=FALSE,isThinLine=FALSE) {

               if (isHeaderRow==TRUE) {
                    headerRow <- colnames(tableContentInternal)
                    headerRtfRow <- getRow(rowContent=headerRow,isExtraSpacing=isExtraSpacing,isLineAbove=TRUE,isLineBelow=FALSE,isThinLine=isThinLine,isCurRowHeaderRow=TRUE)
               }


               numberRows <- dim(tableContentInternal)[1]
               if (isHeaderRow==TRUE) {
                    rtfTable <- headerRtfRow
               } else {
                    rtfTable <- c()
               }

               #tableContentInternalWithTabs <- .self$addTabToMatrixCells(tableContentInternal)
               tableContentInternalWithTabs <- tableContentInternal


               for (i in 1:numberRows) {
                    curRow <- tableContentInternalWithTabs[i,]
                    if (i==1) {
                         rtfRow <- getRow(rowContent=curRow,isExtraSpacing=isExtraSpacing,isLineAbove=TRUE,isLineBelow=FALSE,isThinLine=isThinLine,isCurRowHeaderRow=FALSE)
                         if (isHeaderRow==FALSE) {
                              rtfTable <- rtfRow
                         } else {
                              rtfTable <- c(rtfTable,rtfRow)
                         }
                    } else if (i==numberRows) {
                         rtfRow <- getRow(rowContent=curRow,isExtraSpacing=isExtraSpacing,isLineAbove=FALSE,isLineBelow=TRUE,isThinLine=isThinLine,isCurRowHeaderRow=FALSE)
                         rtfTable <- c(rtfTable,rtfRow)
                    } else {
                         rtfRow <- getRow(rowContent=curRow,isExtraSpacing=isExtraSpacing,isLineAbove-FALSE,isLineBelow=FALSE,isThinLine=isThinLine,isCurRowHeaderRow=FALSE)
                         rtfTable <- c(rtfTable,rtfRow)
                    }
               }
               return(rtfTable)
          },


          getRow = function(rowContent,isExtraSpacing,isLineAbove,isLineBelow,isThinLine,isCurRowHeaderRow) {
               rowPrefix <- "{"
               rowStart <- sprintf("\\trowd \\trgaph%1.0f",cellSpaceBorderInternal)
               rowFormat <- getRowFormatInRTF(isLineAbove = isLineAbove,isLineBelow=isLineBelow, isThinLine=isThinLine)
               rtfRowContent <- .self$getRowContentInRTF(rowContent,isExtraSpacing = isExtraSpacing,isCurRowHeaderRow = isCurRowHeaderRow)
               rowEnd <- "\\row"
               rowSuffix <- "}"

               rowOut <- c(rowPrefix, rowStart, rowFormat, rtfRowContent,rowEnd,rowSuffix)
               return(rowOut)
          },

          setTableContent = function(tableContent) {
               tableContentInternal <<- tableContent
               numberColumnsInternal <<- dim(tableContent)[2]
               .self$setDefaultWidthAlignmentAndTabs()
               return(TRUE)
          },

          setDefaultWidthAlignmentAndTabs = function() {
               unitVector <- rep(1,numberColumnsInternal)

               cellWidths <- unitVector*defaultCellWidthInches
               cellWidths[1] <- defaultCellWidthInches*1.5
               .self$setCellWidthsInches(cellWidths)

               decTabs <- unitVector*defaultDecimalTableProportionInternal
               decTabs[1] <- 0
               .self$setDecimalTabWidthsProportions(decTabs)

               horizAlign <- rep(centerAlignment,numberColumnsInternal)
               horizAlign[1] <- rightAlignment
               headerHorizontalAlignment <<- horizAlign
               rowHorizontalAlignment <<- horizAlign
               return(TRUE)
          },

          addTabToMatrixCells = function(matrixStringIn) {
               myDims <- dim(matrixStringIn)
               newString <- paste("\\tqdec\\tldot\\tx600",matrixStringIn)
               newString <- matrix(newString,myDims[1],myDims[2])
               return(newString)
          },


          setRowFirstColumnJustification =function(columnJustificationString) {
               newRowHorizontalAlignment <- rowHorizontalAlignment
               if (columnJustificationString=="center") {
                    newRowHorizontalAlignment[1] <- centerAlignment
               } else if (columnJustificationString=="left") {
                    newRowHorizontalAlignment[1] <- leftAlignment
               } else {
                    newRowHorizontalAlignment[1] <- rightAlignment
               }
               rowHorizontalAlignment <<- newRowHorizontalAlignment
          },

         setRowSecondColumnDecimalTab =function(columnDecimalTabProportion) {
              newRowDecimalTab <- decimalTabProportionInternal
              newRowDecimalTab[2] <- columnDecimalTabProportion
              .self$setDecimalTabWidthsProportions(newRowDecimalTab)
         },

         setRowDecimalTabForColumn =function(columnDecimalTabProportion,columnNumber) {
              newRowDecimalTab <- decimalTabProportionInternal
              newRowDecimalTab[columnNumber] <- columnDecimalTabProportion
              .self$setDecimalTabWidthsProportions(newRowDecimalTab)
         },


          setRowLastColumnWidth =function(columnWidth) {
               newCellWidths <- .self$cellWidthsInches
               newCellWidths[.self$numberColumnsInternal] <- columnWidth
               .self$setCellWidthsInches(cellWidths = newCellWidths)

          }



    )
)

