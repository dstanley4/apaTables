## ----eval=FALSE----------------------------------------------------------
#  library(apaTables)
#  apa.cor.table(attitude, filename="Table1_APA.doc", table.number=1)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  library(apaTables)
#  basic.reg <- lm(sales ~ adverts + airplay, data = album)
#  apa.reg.table(basic.reg, filename = "Table2_APA.doc", table.number = 2)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  library(apaTables)
#  block1 <- lm(sales ~ adverts + airplay, data = album)
#  block2 <- lm(sales ~ adverts + airplay + I(adverts*airplay), data = album)
#  apa.reg.table(block1, block2, filename = "Table3_APA.doc", table.number = 3)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  options(contrasts = c("contr.sum", "contr.poly"))
#  lm_output <- lm(libido ~ dose, data = viagra)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  library(apaTables)
#  apa.aov.table(lm_output, filename = "Table4_APA.doc", table.number = 4)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  apa.1way.table(iv = dose, dv = libido, data = viagra,
#                 filename = "Table5_APA.doc",
#                 table.number = 5)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  apa.d.table(iv = dose, dv = libido, data = viagra,
#              filename = "Table6_APA.doc",
#              table.number = 6)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  options(contrasts = c("contr.sum", "contr.poly"))
#  lm_output <- lm(attractiveness ~ gender*alcohol, data = goggles)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  library(apaTables)
#  apa.aov.table(lm_output, filename = "Table7_APA.doc", table.number = 7)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  apa.2way.table(iv1 = gender,iv2 = alcohol, dv = attractiveness,
#                 data = goggles,
#                 filename = "Table8_APA.doc",
#                 show.marginal.means = TRUE,
#                 table.number = 8)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  library(apaTables)
#  library(dplyr)
#  goggles.men   <- filter(goggles,gender=="Male")
#  goggles.women <- filter(goggles,gender=="Female")
#  
#  apa.d.table(iv = alcohol, dv = attractiveness,
#              data = goggles.men,
#              filename = "Table9_APA.doc",
#              table.number = 9)
#  
#  apa.d.table(iv = alcohol, dv = attractiveness,
#              data = goggles.women,
#              filename = "Table10_APA.doc",
#              table.number = 10)

## ---- warning=FALSE, message=FALSE, eval = TRUE, echo = FALSE------------
library(apaTables)
library(tidyr)
library(tibble)
library(forcats)
library(ez)

## ---- eval = FALSE, echo = TRUE------------------------------------------
#  library(apaTables)
#  library(tidyverse)
#  library(ez)

## ---- warning=FALSE, message=FALSE---------------------------------------
glimpse(drink_attitude_wide)

## ------------------------------------------------------------------------
drink_attitude_long <- gather(data = drink_attitude_wide,
                              key = cell, value = attitude,
                              beer_positive:water_neutral,
                              factor_key=TRUE)

## ------------------------------------------------------------------------

drink_attitude_long <- separate(data = drink_attitude_long,
                                col = cell, into = c("drink","imagery"),
                                sep = "_", remove = TRUE)

drink_attitude_long$drink <- as_factor(drink_attitude_long$drink)
drink_attitude_long$imagery <- as_factor(drink_attitude_long$imagery)


## ------------------------------------------------------------------------
glimpse(drink_attitude_long)

## ------------------------------------------------------------------------
head(drink_attitude_long)

## ------------------------------------------------------------------------
alcohol_vs_water <- c(1, 1, -2)
beer_vs_wine <- c(-1, 1, 0)
negative_vs_other <- c(1, -2, 1)
positive_vs_neutral <- c(-1, 0, 1)
contrasts(drink_attitude_long$drink) <- cbind(alcohol_vs_water, beer_vs_wine)
contrasts(drink_attitude_long$imagery) <- cbind(negative_vs_other, positive_vs_neutral)

## ------------------------------------------------------------------------
options(digits = 10)
drink_attitude_results <- ezANOVA(data = drink_attitude_long,
                   dv = .(attitude), wid = .(participant),
                   within = .(drink, imagery),
                   type = 3, detailed = TRUE)


## ---- eval=FALSE---------------------------------------------------------
#  apa.ezANOVA.table(drink_attitude_results,
#                    table.number = 11,
#                    filename="Table11_APA.doc")

## ---- warning=FALSE, message=FALSE, eval = TRUE, echo = FALSE------------
library(apaTables)
library(tidyr)
library(tibble)
library(forcats)
library(ez)

## ---- eval = FALSE, echo = TRUE------------------------------------------
#  library(apaTables)
#  library(tidyverse)
#  library(ez)

## ---- warning=FALSE, message=FALSE---------------------------------------
glimpse(dating_wide)

## ------------------------------------------------------------------------
dating_long <- gather(data = dating_wide,
                     key = cell, value = date_rating,
                     attractive_high:ugly_none,
                     factor_key = TRUE)



## ------------------------------------------------------------------------
dating_long <- separate(data = dating_long,
                       col = cell, into = c("looks","personality"),
                       sep = "_", remove = TRUE)

dating_long$looks <- as_factor(dating_long$looks)
dating_long$personality <- as_factor(dating_long$personality)


## ------------------------------------------------------------------------
glimpse(dating_long)

## ------------------------------------------------------------------------
head(dating_long)

## ------------------------------------------------------------------------
some_vs_none <- c(1, 1, -2)
hi_vs_av <- c(1, -1, 0)
attractive_vs_ugly <- c(1, 1, -2)
attractive_vs_average <- c(1, -1, 0)
contrasts(dating_long$personality) <- cbind(some_vs_none, hi_vs_av)
contrasts(dating_long$looks) <- cbind(attractive_vs_ugly, attractive_vs_average)


## ------------------------------------------------------------------------
options(digits = 10)
dating_results <-ezANOVA(data = dating_long, 
                         dv = .(date_rating), wid = .(participant),
                        between = .(gender), within = .(looks, personality),
                        type = 3, detailed = TRUE)


## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  dating_table <- apa.ezANOVA.table(dating_results,
#                                    filename = "Table12_APA.doc",
#                                    table.number = 12)
#  print(dating_table)

