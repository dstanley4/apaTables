# apaTables: Create American Psychological Association (APA) Style Tables

A common task faced by researchers is the creation of APA style (i.e.,
*American Psychological Association* style) tables from statistical
output. In R a large number of function calls are often needed to obtain
all of the desired information for a single APA style table. As well,
the process of manually creating APA style tables in a word processor is
prone to transcription errors. This package creates Word files (.doc
files) and latex code containing APA style tables for several types of
analyses. Using this package minimizes transcription errors and reduces
the number commands needed by the user.

Bugs and feature requests can be reported at:
<https://github.com/dstanley4/apaTables/issues>

Tutorial at:
<https://dstanley4.github.io/apaTables/articles/apaTables.html>

Currently, the following tables can be created:

- Correlation tables - Correlation tables (with confidence intervals and
  descriptive statistics) are created from data frames using
  [`apa.cor.table`](http://dstanley4.github.io/apaTables/reference/apa.cor.table.md).

- Single "block" regression tables - Single "block" regression tables
  are created from a regression object using
  [`apa.reg.table`](http://dstanley4.github.io/apaTables/reference/apa.reg.table.md).

- Multiple "block" regression tables - Multiple "block" regression
  tables are created from regression objects using
  [`apa.reg.table`](http://dstanley4.github.io/apaTables/reference/apa.reg.table.md).

- ANOVA tables - An ANOVA F-table can be created via
  [`apa.aov.table`](http://dstanley4.github.io/apaTables/reference/apa.aov.table.md)
  from a regression object (i.e. lm output or aov output). Cell
  mean/standard deviation tables for 1- and 2-way designs are created
  from data frames using
  [`apa.1way.table`](http://dstanley4.github.io/apaTables/reference/apa.1way.table.md)
  and
  [`apa.2way.table`](http://dstanley4.github.io/apaTables/reference/apa.2way.table.md).

- ANOVA tables from afex package - An ANOVA F-table from afex::aov_ez()
  output (between, within, or mixed designs) can be created via
  [`apa.afex.table`](http://dstanley4.github.io/apaTables/reference/apa.afex.table.md).

- Standardized mean difference (i.e., *d*-value) tables (with confidence
  intervals and descriptive statistics) illustrating all possible paired
  comparisons using a single independent variable are created from data
  frames using
  [`apa.d.table`](http://dstanley4.github.io/apaTables/reference/apa.d.table.md).

## See also

Useful links:

- <https://github.com/dstanley4/apaTables>

- <http://dstanley4.github.io/apaTables/>

- Report bugs at <https://github.com/dstanley4/apaTables/issues>

## Author

|             |                                         |
|-------------|-----------------------------------------|
| Author:     | David J. Stanley <dstanley@uoguelph.ca> |
| Maintainer: | David J. Stanley <dstanley@uoguelph.ca> |
