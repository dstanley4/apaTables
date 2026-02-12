# apaTables Version 3.0.1

\[![](http://cranlogs.r-pkg.org/badges/grand-total/apaTables)

A common task faced by researchers is the creation of APA style (i.e.,
American Psychological Association style) tables from statistical
output. In R a large number of function calls are often needed to obtain
all of the desired information for a single APA style table. As well,
the process of manually creating APA style tables in a word processor is
prone to transcription errors. This package creates Word files (.doc
files) containing APA style tables for several types of analyses. Using
this package minimizes transcription errors and reduces the number
commands needed by the user.

The development version of apaTables R package is hosted here on Github.
Current stable version is on the CRAN, see apaTables
[here.](https://cran.r-project.org/package=apaTables)

### What’s new in Version 3.0.1

- **afex support**: Use
  [`apa.afex.table()`](http://dstanley4.github.io/apaTables/reference/apa.afex.table.md)
  with output from
  [`afex::aov_ez()`](https://rdrr.io/pkg/afex/man/aov_car.html) for
  ANOVA tables. The afex package is the recommended approach for
  repeated-measures and mixed designs.
- **ez package deprecated**: The ez package has been archived on CRAN.
  [`apa.ezANOVA.table()`](http://dstanley4.github.io/apaTables/reference/apa.ezANOVA.table.md)
  still works for users who have ez installed but is now deprecated.
  Please use
  [`apa.afex.table()`](http://dstanley4.github.io/apaTables/reference/apa.afex.table.md)
  instead.
- **LaTeX table support**: All table functions now support LaTeX output
  in addition to Word (.doc/.rtf) files.

### Install Stable 2.0 CRAN Version

This version does not support LaTeX tables or afex.

``` r
install.packages("apaTables",dep=T)

library(apaTables)
```

### Install 3.0.1 Development Version

This version supports LaTeX tables and afex.

``` r
install.packages("remotes")

remotes::install_github("dstanley4/apaTables")

library(apaTables)
```

## Tutorial

You can learn how to use apaTables
[here](https://dstanley4.github.io/apaTables/).
