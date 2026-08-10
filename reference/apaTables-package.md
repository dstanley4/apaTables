# apaTables: Create American Psychological Association (APA) Style Tables

A common task faced by researchers is the creation of APA style (i.e.,
*American Psychological Association* style) tables from statistical
output. In R a large number of function calls are often needed to obtain
all of the desired information for a single APA style table, and
manually re-creating that table in a word processor is prone to
transcription errors. This package builds complete APA style tables
directly from analysis output. A single command saves a finished table
as a Microsoft Word (.docx) file, ready for a manuscript. Tables can
also be placed in Quarto documents (via the bundled apaquarto
extension), so a rendered Word or PDF manuscript comes out with its
tables already in place – numbered, titled, and with notes. The same
tables can be shown on reveal.js slides, and HTML output is also
supported. Using this package minimizes transcription errors and reduces
the number of commands needed by the user.

Bugs and feature requests can be reported at:
<https://github.com/dstanley4/apaTables/issues>

Tutorial at:
<https://dstanley4.github.io/apaTables/articles/apaTables.html>

Currently, the following tables can be created:

- Correlation tables - Correlation tables (with confidence intervals and
  descriptive statistics) are created from data frames using
  [`apa.cor.table`](https://dstanley4.github.io/apaTables/reference/apa.cor.table.md).

- Single "block" regression tables - Single "block" regression tables
  are created from a regression object using
  [`apa.reg.table`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md).

- Multiple "block" regression tables - Multiple "block" regression
  tables are created from regression objects using
  [`apa.reg.table`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md).

- ANOVA tables - An ANOVA F-table can be created via
  [`apa.aov.table`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md)
  from a regression object (i.e. lm output or aov output). Cell
  mean/standard deviation tables for 1- and 2-way designs are created
  from data frames using
  [`apa.1way.table`](https://dstanley4.github.io/apaTables/reference/apa.1way.table.md)
  and
  [`apa.2way.table`](https://dstanley4.github.io/apaTables/reference/apa.2way.table.md).

- ANOVA tables from afex package - An ANOVA F-table from afex::aov_ez()
  output (between, within, or mixed designs) can be created via
  [`apa.afex.table`](https://dstanley4.github.io/apaTables/reference/apa.afex.table.md).

- Standardized mean difference (i.e., *d*-value) tables (with confidence
  intervals and descriptive statistics) illustrating all possible paired
  comparisons using a single independent variable are created from data
  frames using
  [`apa.d.table`](https://dstanley4.github.io/apaTables/reference/apa.d.table.md).

## See also

Useful links:

- <https://github.com/dstanley4/apaTables>

- <https://dstanley4.github.io/apaTables/>

- Report bugs at <https://github.com/dstanley4/apaTables/issues>

## Author

**Maintainer**: David J. Stanley <dstanley@uoguelph.ca> \[copyright
holder\]

Authors:

- David J. Stanley <dstanley@uoguelph.ca> \[copyright holder\]

Other contributors:

- W. Joel Schneider (author of the apaquarto Quarto extension, bundled
  under CC0) \[contributor\]

- Brenton M. Wiernik (author of apa.csl, bundled under CC BY-SA 3.0)
  \[contributor, copyright holder\]
