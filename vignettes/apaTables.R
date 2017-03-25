## ----eval=FALSE----------------------------------------------------------
#  library(apaTables)
#  apa.cor.table(attitude, filename="Table1_APA.doc", table.number=1)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  library(apaTables)
#  basic.reg <- lm(sales ~ adverts + airplay, data=album)
#  apa.reg.table(basic.reg, filename="Table2_APA.doc", table.number=2)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  library(apaTables)
#  block1 <- lm(sales ~ adverts + airplay, data=album)
#  block2 <- lm(sales ~ adverts + airplay + I(adverts*airplay), data=album)
#  apa.reg.table(block1, block2, filename="Table3_APA.doc", table.number=3)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  options(contrasts = c("contr.sum", "contr.poly"))
#  lm_output <- lm(libido ~ dose, data=viagra)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  library(apaTables)
#  apa.aov.table(lm_output,filename="Figure4_APA.doc",table.number = 4)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  apa.1way.table(iv=dose,dv=libido,data=viagra,filename="Figure5_APA.doc",table.number = 5)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  apa.d.table(iv=dose,dv=libido,data=viagra,filename="Figure6_APA.doc",table.number = 6)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  options(contrasts = c("contr.sum", "contr.poly"))
#  lm_output <- lm(attractiveness ~ gender*alcohol, data=goggles)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  library(apaTables)
#  apa.aov.table(lm_output,filename="Figure7_APA.doc",table.number = 7)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  apa.2way.table(iv1=gender,iv2=alcohol,dv=attractiveness,data=goggles,filename="Figure8_APA.doc",table.number = 8)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  library(apaTables)
#  library(dplyr)
#  goggles.men   <- filter(goggles,gender=="Male")
#  goggles.women <- filter(goggles,gender=="Female")
#  
#  apa.d.table(iv=alcohol,dv=attractiveness,data=goggles.men,filename="Table9_APA.doc",table.number = 9)
#  apa.d.table(iv=alcohol,dv=attractiveness,data=goggles.women,filename="Table10_APA.doc",table.number = 10)

