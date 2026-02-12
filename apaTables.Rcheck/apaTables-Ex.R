pkgname <- "apaTables"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "apaTables-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('apaTables')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("apa.1way.table")
### * apa.1way.table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: apa.1way.table
### Title: Creates a table of means and standard deviations for a 1-way
###   ANOVA design in APA style
### Aliases: apa.1way.table

### ** Examples

# Example 1: 1-way from Field et al. (2012) Discovery Statistics Using R

table1 <- apa.1way.table(iv = dose, dv = libido,
               data = viagra, table.number = 1)

apa.save(filename = file.path(tempdir(), "table1.doc"), table1)

# Create a table for your PDF
# Include the line below in your rmarkdown or Quarto document
apa.knit.table.for.pdf(table1)

# delete demo file
unlink(file.path(tempdir(), "table1.doc"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("apa.1way.table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("apa.2way.table")
### * apa.2way.table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: apa.2way.table
### Title: Creates a table of means and standard deviations for a 2-way
###   ANOVA design in APA style
### Aliases: apa.2way.table

### ** Examples

# Example 2: 2-way from Fidler & Thompson (2001)

table2 <- apa.2way.table(iv1 = a, iv2 = b, dv = dv,
                          data = fidler_thompson,
                          landscape = TRUE,
                          table.number = 2)


# Example 3: 2-way from Field et al. (2012) Discovery Statistics Using R

table3 <- apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness,
                         data = goggles, table.number = 3)


# Save both Table 2 and Table 3 in a single .doc document
apa.save(filename = file.path(tempdir(), "my_tables.doc"), table2, table3)

# Create a table for your PDF
# Include the lines below in your rmarkdown or Quarto document
apa.knit.table.for.pdf(table2)
apa.knit.table.for.pdf(table3)

# delete demo file
unlink(file.path(tempdir(), "my_tables.doc"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("apa.2way.table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("apa.afex.table")
### * apa.afex.table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: apa.afex.table
### Title: Creates an ANOVA table in APA style based on output of aov_ez
###   command from afex package
### Aliases: apa.afex.table

### ** Examples

if  (requireNamespace("afex", quietly = TRUE)){
if  (requireNamespace("apaTables", quietly = TRUE)){
if  (requireNamespace("tidyr", quietly = TRUE)){


#
# ** Example 1: Between Participant Predictors
#

goggles <- apaTables::goggles

goggles_results <- afex::aov_ez("participant", "attractiveness", goggles,
                                 between = c("gender", "alcohol"))


# Make APA table - save after all 3 examples
goggles_table <- apa.afex.table(goggles_results)

# Create a table for your PDF
# Include the lines below in your rmarkdown or Quarto document
apa.knit.table.for.pdf(goggles_table)


## No test: 
#
# ** Example 2: Within Participant Predictors
#

drink_attitude_wide <- apaTables::drink_attitude_wide

# Convert data from wide format to long format where one row represents one OBSERVATION.
# Wide format column names MUST represent levels of each variable separated by an underscore.
# See vignette for further details.

drink_attitude_long <- tidyr::pivot_longer(drink_attitude_wide,
                              cols = beer_positive:water_neutral,
                             names_to = c("drink", "imagery"),
                             names_sep = "_",
                              values_to = "attitude")


drink_attitude_long$drink <- as.factor(drink_attitude_long$drink)
drink_attitude_long$imagery <- as.factor(drink_attitude_long$imagery)


drink_attitude_results <- afex::aov_ez("participant", "attitude",
                             drink_attitude_long,
                             within = c("drink", "imagery"))


# Make APA table - save after all 3 examples
drink_table <- apa.afex.table(drink_attitude_results)

# Create a table for your PDF
# Include the lines below in your rmarkdown or Quarto document
apa.knit.table.for.pdf(drink_table)


#
# ** Example 3: Between and Within Participant Predictors
#

dating_wide <- apaTables::dating_wide

# Convert data from wide format to long format where one row represents one OBSERVATION.
# Wide format column names MUST represent levels of each variable separated by an underscore.
# See vignette for further details.


dating_long <- tidyr::pivot_longer(dating_wide,
                              cols = attractive_high:ugly_none,
                             names_to = c("looks", "personality"),
                             names_sep = "_",
                              values_to = "date_rating")
#'
dating_long$looks <- as.factor(dating_long$looks)
dating_long$personality <- as.factor(dating_long$personality)


dating_results <- afex::aov_ez("participant", "date_rating", dating_long,
                                between = "gender",
                                within = c("looks", "personality"))


# Make APA table - save after all 3 examples
dating_table <- apa.afex.table(dating_results)

# Create a table for your PDF
# Include the lines below in your rmarkdown or Quarto document
apa.knit.table.for.pdf(dating_table)


#
# Saving all three tables
#
apa.save(file.path(tempdir(), "tables_afex.doc"),
                goggles_table,
                 drink_table,
                dating_table)

# delete demo file
unlink(file.path(tempdir(), "tables_afex.doc"))
## End(No test)
}}}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("apa.afex.table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("apa.aov.table")
### * apa.aov.table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: apa.aov.table
### Title: Creates a fixed-effects ANOVA table in APA style
### Aliases: apa.aov.table

### ** Examples


#Example 1: 1-way from Field et al. (2012) Discovery Statistics Using R
op <- options(contrasts = c("contr.helmert", "contr.poly"))
lm_output <- lm(libido ~ dose, data = viagra)
table1 <- apa.aov.table(lm_output, table.number = 4)


# Example 2: 2-way from Fidler & Thompson (2001)
# You must set these contrasts to ensure values match SPSS
lm_output <- lm(dv ~ a*b, data = fidler_thompson)
table2 <- apa.aov.table(lm_output, table.number = 5)


#Example 3: 2-way from Field et al. (2012) Discovery Statistics Using R
# You must set these contrasts to ensure values match SPSS
lm_output <- lm(attractiveness ~ gender*alcohol, data = goggles)
table3 <- apa.aov.table(lm_output, table.number = 6)


# Save all three table in the same .doc document
apa.save(filename = file.path(tempdir(), "my_tables.doc"), table1, table2, table3)


# Create a table for your PDF
# Include the lines below in your rmarkdown or Quarto document
apa.knit.table.for.pdf(table1)
apa.knit.table.for.pdf(table2)
apa.knit.table.for.pdf(table3)


# delete demo file
unlink(file.path(tempdir(), "my_tables.doc"))
options(op)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("apa.aov.table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
base::options(contrasts = c(unordered = "contr.treatment",ordered = "contr.poly"))
cleanEx()
nameEx("apa.cor.table")
### * apa.cor.table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: apa.cor.table
### Title: Creates a correlation table in APA style with means and standard
###   deviations
### Aliases: apa.cor.table

### ** Examples

# View top few rows of attitude data set
head(attitude)

# Use apa.cor.table function
table1 <- apa.cor.table(attitude)


# Save Table 1 in a .doc document
apa.save(filename = file.path(tempdir(), "table1.doc"), table1)


# Create a table for your PDF
# Include the lines below in your rmarkdown or Quarto document
apa.knit.table.for.pdf(table1)

# delete demo file
unlink(file.path(tempdir(), "table1.doc"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("apa.cor.table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("apa.d.table")
### * apa.d.table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: apa.d.table
### Title: Creates a d-values for all pairwise (two-sample) comparisons in
###   APA style
### Aliases: apa.d.table

### ** Examples

# View top few rows of viagra data set from Discovering Statistics Using R
head(viagra)

# Use apa.d.table function
table1 <- apa.d.table(iv = dose, dv = libido, data = viagra)


# Save Table 1 in a .doc document
apa.save(filename = file.path(tempdir(), "table1.doc"), table1)


# Create a table for your PDF
# Include the lines below in your rmarkdown or Quarto document
apa.knit.table.for.pdf(table1)

# delete demo file
unlink(file.path(tempdir(), "table1.doc"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("apa.d.table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("apa.ezANOVA.table")
### * apa.ezANOVA.table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: apa.ezANOVA.table
### Title: Creates an ANOVA table in APA style based output of ezANOVA
###   command from ez package
### Aliases: apa.ezANOVA.table

### ** Examples

## Not run: 
##D # Note: ez package has been archived on CRAN.
##D # Use apa.afex.table() with afex::aov_ez() instead.
##D 
##D #
##D # ** Example 1: Between Participant Predictors
##D #
##D 
##D goggles <- apaTables::goggles
##D 
##D # Use ezANOVA
##D # Be sure use the options command, as below, to ensure sufficient digits
##D 
##D op <- options(digits = 10)
##D goggles_results <- ez::ezANOVA(data = goggles,
##D                           dv = attractiveness,
##D                           between = .(gender, alcohol),
##D                           participant ,
##D                           detailed = TRUE)
##D 
##D # Make APA table
##D goggles_table <- apa.ezANOVA.table(goggles_results)
##D 
##D # Create a table for your PDF
##D # Include the lines below in your rmarkdown or Quarto document
##D apa.knit.table.for.pdf(goggles_table)
##D options(op)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("apa.ezANOVA.table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("apa.reg.table")
### * apa.reg.table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: apa.reg.table
### Title: Creates a regresion table in APA style
### Aliases: apa.reg.table

### ** Examples

# View top few rows of goggles data set
# from Discovering Statistics Using R
head(album)

# Single block example
blk1 <- lm(sales ~ adverts + airplay, data=album)
apa.reg.table(blk1)
table1 <- apa.reg.table(blk1,table.number = 1)

## No test: 
# Two block example, more than two blocks can be used
blk1 <- lm(sales ~ adverts, data=album)
blk2 <- lm(sales ~ adverts + airplay + attract, data=album)
table2 <- apa.reg.table(blk1, blk2, table.number = 2)

# Interaction product-term test with blocks
blk1 <- lm(sales ~ adverts + airplay, data=album)
blk2 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data=album)
table3 <- apa.reg.table(blk1, blk2, table.number = 3)

# Interaction product-term test with blocks and additional product terms
blk1<-lm(sales ~ adverts + airplay, data=album)
blk2<-lm(sales ~ adverts + airplay + I(adverts*adverts) + I(airplay*airplay), data=album)
blk3<-lm(sales~adverts+airplay+I(adverts*adverts)+I(airplay*airplay)+I(adverts*airplay),data=album)
table4 <- apa.reg.table(blk1,blk2,blk3, table.number = 4)

#Interaction product-term test with single regression (i.e., semi-partial correlation focus)
blk1 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data=album)
table5 <- apa.reg.table(blk1, table.number = 5)

# Save Table 1 in a .doc document
apa.save(filename = file.path(tempdir(), "regression_tables.doc"),
         table1,
         table2,
         table3,
         table4,
         table5)

# Create a table for your PDF
# Include the lines below in your rmarkdown or Quarto document
apa.knit.table.for.pdf(table1)
apa.knit.table.for.pdf(table2)
apa.knit.table.for.pdf(table3)
apa.knit.table.for.pdf(table4)
apa.knit.table.for.pdf(table5)

# delete demo file
unlink(file.path(tempdir(), "regression_tables.doc"))
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("apa.reg.table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("apa.save")
### * apa.save

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: apa.save
### Title: Save previously constructed APA table objects in a single .doc
###   file
### Aliases: apa.save

### ** Examples

library(apaTables)

table1 <- apa.1way.table(iv = dose, dv = libido,
               data = viagra, table.number = 1)

table2 <- apa.2way.table(iv1 = gender, iv2 = alcohol,
                         dv = attractiveness,
                         data = goggles, table.number = 1)

apa.save(filename = file.path(tempdir(), "my.tables.doc"), table1, table2)

# delete demo file
unlink(file.path(tempdir(), "my.tables.doc"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("apa.save", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get.ci.partial.eta.squared")
### * get.ci.partial.eta.squared

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get.ci.partial.eta.squared
### Title: Calculates confidence interval for partial eta-squared in a
###   fixed-effects ANOVA
### Aliases: get.ci.partial.eta.squared

### ** Examples

# Smithson (2001) p. 619
get.ci.partial.eta.squared(F.value=6.00, df1=1, df2=42, conf.level=.90)
get.ci.partial.eta.squared(F.value=2.65, df1=6, df2=42, conf.level=.90)
get.ci.partial.eta.squared(F.value=2.60, df1=6, df2=42, conf.level=.90)

# Fidler & Thompson (2001) Fixed Effects 2x4 p. 594 (Table 6) / p. 596 (Table 8)
get.ci.partial.eta.squared(F.value=1.50, df1=1, df2=16, conf.level=.90)
get.ci.partial.eta.squared(F.value=4.00, df1=3, df2=16, conf.level=.90)
get.ci.partial.eta.squared(F.value=1.50, df1=3, df2=16, conf.level=.90)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get.ci.partial.eta.squared", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
