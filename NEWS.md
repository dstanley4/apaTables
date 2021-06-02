# apaTable 2.0

## New features

* apa.exANOVA.table Support repeated measures designs via this new function. 
  Also included in the documentation is a tidyverse based within-subject ANOVA workflow.

* apa.reg.boot.table. Support for bootstrap confidence intervals in a regression table


## Changes

* apa.cor.table Confidence intervals are always displayed. The argument show.confidence.intervals has been deprecated.

* apa.d.table Confidence intervals are always displayed. The argument show.confidence.intervals has been deprecated.

* apa.2way.table Fixed a format problem related to lines and the Marginal column label

* Update in APA Style to all table notes

* Support for factors as predictors in apa.reg.table

* Support for using or not using Steiger (2004) correction for proportion of variance confidence intervals in the apa.reg.table and apa.aov.table commands

* General improvements to documentation (typos etc)



# apaTable 2.0.3

* Avoid calls to tidyverse package on CRAN server checks. 

* Fixed bug with apa.reg.table and categorical predictors.


# apaTable 2.0.4

* Fixed a bug to ensure factor coding of data matched Field et al. (2012) book. Specifically, changed viagra and goggles data set factor order.

* Added donottest to bootstrap analyses to avoid CRAN server time check problems

# apaTable 2.0.5

* Fixed a bug with apaTables so it can work with current version of imported packages (e.g., tibble). This appeared to only impact apa.reg.table and apa.reg.boot.table. Both work again.

* Updated documentation to apa.cor.table

# apaTable 2.0.7

* Minor fixes


