# Migrating to apaTables 4.0

## Who this guide is for

This guide is for code written for apaTables 2.0.8, the version on CRAN
before 4.0.0. It lists what you may need to change and what now behaves
differently. (Versions 3.0.0 and 3.0.1 were never released to CRAN. If
you installed one of them from GitHub, the last section is for you.)

## What keeps working

The table functions –
[`apa.cor.table()`](https://dstanley4.github.io/apaTables/reference/apa.cor.table.md),
[`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md),
[`apa.d.table()`](https://dstanley4.github.io/apaTables/reference/apa.d.table.md),
[`apa.aov.table()`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md),
[`apa.1way.table()`](https://dstanley4.github.io/apaTables/reference/apa.1way.table.md)
and
[`apa.2way.table()`](https://dstanley4.github.io/apaTables/reference/apa.2way.table.md)
– keep the same names and every argument from 2.0.8, so most scripts
that build tables with them run unchanged. What changes is the kind of
file they write, some default settings, some names inside the table
object, and some of the numbers in ANOVA and hierarchical regression
tables, where 2.0.8 had errors. A few scripts now stop with a message
instead – a file name with an unusual ending, regression blocks that do
not build on one another, a model with an offset, a regression with
effect-coded predictors, or a setting outside the values a function
accepts. Each is described below.

## Two functions were removed

`apa.ezANOVA.table()` is gone, because the ez package it relied on was
archived on CRAN. Use
[`apa.afex.table()`](https://dstanley4.github.io/apaTables/reference/apa.afex.table.md)
instead, with the output of
[`afex::aov_ez()`](https://rdrr.io/pkg/afex/man/aov_car.html). It covers
between-participant, repeated-measures and mixed designs. The afex
package does not come with apaTables; install it once with
`install.packages("afex")`.

``` r

goggles_results <- afex::aov_ez(id = "participant",
     dv = "attractiveness", data = goggles,
     between = c("gender", "alcohol"))
apa.afex.table(afex.output = goggles_results)
```

`apa.reg.boot.table()` is gone too, and there is no replacement in this
release: apaTables 4.0 does not make bootstrapped regression tables.
[`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
still makes the standard regression table.

## Files are now Word `.docx` files

In 2.0.8, `filename = "Table1.doc"` wrote an RTF file with a `.doc`
name. The same code in 4.0 writes a real Word file named `Table1.docx`,
and prints a message naming the file each time:

``` r

my_table <- apa.cor.table(data = attitude, filename = "Table1.doc")
# apaTables 4.0 writes Word files in .docx format (the legacy .doc/.rtf
# RTF engine was retired). Writing: Table1.docx
```

A `.rtf` name is handled the same way. Change the name to `.docx` and
the message stops. A name ending in `.html` writes a web page instead. A
name ending in `.pdf` stops with a message explaining how to make a PDF
table (see “Tables in Quarto documents” below), and any other ending, or
none, stops with an error asking for `.docx` or `.html` (2.0.8 wrote a
file under any name).

[`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
is new in 4.0, and it is the usual way to write tables to a file. Build
the table on one line and save it on the next:

``` r

my_table <- apa.cor.table(data = attitude)
apa.save(filename = "Table1.docx", my_table)
```

It also writes several tables to a single Word file, one table per page:

``` r

table1 <- apa.cor.table(data = attitude)
table2 <- apa.reg.table(lm(rating ~ complaints + privileges, data = attitude))
apa.save(filename = "Tables.docx", table1, table2)
```

The pages are US letter. Add `paper = "a4"` for A4.

## Tables are numbered 1 unless you say otherwise

In 2.0.8, `table.number` defaulted to `NA`: a table built without one
had no number at the console, and the file said “Table XX”. It now
defaults to `1`. A number you supply is used as before. To leave a table
unnumbered, use `table.number = 0` (or `NA`, or `NULL`).

When
[`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
writes several tables to one file, a number you supplied is kept, and a
table left at the default takes the next number after the last numbered
table before it, starting at 1. Three tables left at the default become
Table 1, Table 2 and Table 3. If the second is given `table.number = 5`,
they become Table 1, Table 5 and Table 6.

## Which tables are printed sideways

In 2.0.8 the orientation of each file was fixed by the function that
wrote it:

- [`apa.cor.table()`](https://dstanley4.github.io/apaTables/reference/apa.cor.table.md),
  [`apa.d.table()`](https://dstanley4.github.io/apaTables/reference/apa.d.table.md)
  and
  [`apa.2way.table()`](https://dstanley4.github.io/apaTables/reference/apa.2way.table.md)
  wrote a landscape (sideways) page unless you passed
  `landscape = FALSE`;
- [`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  always wrote a landscape page;
- [`apa.1way.table()`](https://dstanley4.github.io/apaTables/reference/apa.1way.table.md)
  wrote an upright page unless you passed `landscape = TRUE`;
- [`apa.aov.table()`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md)
  always wrote an upright page.

In 4.0 every table function takes a `landscape` argument
([`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
and
[`apa.aov.table()`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md)
gain it), and by default apaTables measures the table and turns the page
only when the table does not fit upright. In the Word files that
[`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
(or `filename =`) writes:

- small d-value and two-way tables, such as the examples on the help
  pages, and correlation tables of up to about five variables, now print
  upright; larger ones still turn sideways when they need to;
- regression tables are wide enough that they usually still print
  sideways.

`landscape = TRUE` gives you a landscape page whether the table needs it
or not. `landscape = FALSE` keeps the page upright; a table too wide for
it is set in smaller type so that every column is kept.

In a Quarto manuscript, apaTables measures each table against the page
and type size of the manuscript, so more tables stay upright there – a
one-block regression table, for example.

## Numbers that can differ from 2.0.8

These changes correct errors in 2.0.8, so a table can show different
numbers from the one 2.0.8 printed for the same model:

- **[`apa.aov.table()`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md),
  Type III (the default).** If your model was fitted with the R default
  contrasts and had an interaction, 2.0.8 printed each main effect at
  one level of the other factor rather than the main effect. In the
  `goggles` example, alcohol printed as *F* = 0.61, *p* = .546 instead
  of *F* = 20.07, *p* \< .001. apaTables 4.0 refits the model with
  sum-to-zero contrasts, as SPSS does, so the table is right whether or
  not you set `options(contrasts = c("contr.sum", "contr.poly"))`. If
  you already set that line, your main effects are unchanged.
- **[`apa.aov.table()`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md),
  all types.** 2.0.8 rounded the sums of squares and *F* to two decimals
  before computing the mean squares, partial eta-squared and the
  confidence interval for it, so those could be off by .01, or print as
  `NaN` for a dependent variable on a very small scale. They are now
  computed at full precision and rounded only when printed. With
  `type = 2`, the confidence interval for the first effect is no longer
  blank.
- **[`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  with several blocks.** 2.0.8 accepted blocks that did not build on one
  another and printed a negative change in *R*² with a `[NaN, NaN]`
  interval. Each block must now contain every predictor of the block
  before it, plus at least one more, and all blocks must be fitted to
  the same cases. Code that broke these rules now stops with a message
  saying what to change.
- **[`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  intervals for the semi-partial correlation squared and the change in
  *R*².** 2.0.8 could print a negative lower limit for these proportions
  of variance. The limits are now held to the range 0 to 1, so such a
  limit prints as .00.
- **[`apa.d.table()`](https://dstanley4.github.io/apaTables/reference/apa.d.table.md).**
  2.0.8 left a d-value and the interval for it blank (`-` and `[-,-]`)
  when Levene’s test, centered on the median, gave *p* \< .05 for that
  pair of groups. A d-value is now reported for every pair, computed
  with the pooled standard deviation as before, and no test of equal
  variances is run. Compare the standard deviations in the table to
  judge whether one pooled value describes both groups.
- **[`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  with weights.** For a model fitted with `weights`, 2.0.8 printed the
  unweighted correlation in the *r* column, beside the weighted *b*,
  semi-partial correlation squared and *R*². The *r* column is now left
  out for weighted models, as beta already was.
- **[`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  with a categorical predictor after
  `options(contrasts = c("contr.sum", "contr.poly"))`.** 2.0.8 used the
  sum-to-zero coding that line sets, so the rows read `factor(cyl)1`,
  `factor(cyl)2` and each b compared one group with the mean of the
  group means. The rows now compare each level with the first level, as
  without the line: in `lm(mpg ~ factor(cyl) + wt, data = mtcars)`, 6
  cylinders is -4.26, not -0.81.
- **Significance stars.** A *p* value of exactly .01 was unmarked in
  2.0.8; it is now marked with one star.
- **[`apa.2way.table()`](https://dstanley4.github.io/apaTables/reference/apa.2way.table.md)
  marginal means.** 2.0.8 counted a case with a missing value on one
  factor in the marginal mean of the other factor, though it was in no
  cell. Marginal means now use only the cases in the cells.

## Settings that now stop with a message

apaTables 4.0 checks these settings, and a value it does not accept
stops with a message naming the values allowed:

- `conf.level` in
  [`apa.aov.table()`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md)
  and `prop.var.conf.level` in
  [`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  must be `.90` or `.95` (2.0.8 silently used `.90` and `.95`,
  respectively, in place of any other value);
- `landscape` must be `TRUE`, `FALSE` or `NA`;
- `table.number` must be a whole number, 0 or more.

[`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
and
[`apa.aov.table()`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md)
also stop for a model with an offset (an
[`offset()`](https://rdrr.io/r/stats/offset.html) term, or the `offset`
argument of [`lm()`](https://rdrr.io/r/stats/lm.html)). In 2.0.8,
[`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
printed the offset as a predictor row with no estimate. Fit the model
without the offset.

[`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
also stops for a categorical predictor coded for that model in a way
other than comparing each level with one reference level – effect coding
set with `lm(contrasts = list(cyl = "contr.sum"))`, with `C(cyl, sum)`
in the formula, or on the variable. 2.0.8 printed such rows as `cyl1`,
`cyl2`. Fit the model with
`lm(..., contrasts = list(cyl = "contr.treatment"))`, or without
[`C()`](https://rdrr.io/r/stats/zC.html).

## New p values and n columns

A correlation table now prints the *p* value for each correlation on a
line below the confidence interval. To leave those lines out, as 2.0.8
did, add `show.pvalue = FALSE`:

``` r

my_table <- apa.cor.table(data = attitude, show.pvalue = FALSE)
```

Correlation tables also gain an *N* column, the number of cases for each
variable. When the data have missing values, the note gives the range of
the number of cases behind each correlation. d-value and one-way tables
gain an *n* column, the number of cases in each group.

## If your script reads values out of the table object

Most scripts only print or save a table. If yours reads values out of
the object a table function returns, these changed:

- Correlation, d-value, one-way and two-way tables now use the same
  names as regression and ANOVA tables already did: `table_number`,
  `table_title`, `table_body` and `table_note` instead of
  `table.number`, `table.title`, `table.body` and `table.note`. The old
  names now give `NULL`. The object class is `apa_table` (it was
  `apa.table` for those four).
- In a regression table, the columns `sr2` and `sr2_95%_CI` are now
  `Unique_R2` and `Unique_95%_CI` (the interval column follows
  `prop.var.conf.level`, so `Unique_90%_CI` at .90), and the `Unique_R2`
  values carry significance stars, so
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html) on that column
  no longer works. In a table with several blocks, the `Difference`
  column is now `Delta_Fit`, and each block starts with a “Model 1”,
  “Model 2”, … row, so the predictor rows sit one row lower per block. A
  table for a weighted model has no `r` column. When R writes a
  predictor name in backticks, as it does for a name with a space or a
  hyphen, the table now has the `beta`, `beta_95%_CI` and `r` columns
  that 2.0.8 left out, and the `Predictor` value has no backticks
  (`pre-test`, not `` `pre-test` ``). A term written with
  [`I()`](https://rdrr.io/r/base/AsIs.html) has a new `Predictor` value:
  `I(wt * qsec)` is now `wt x qsec`, and `I(wt^2)` is now `wt^2`.
- In an ANOVA table, a very small *p* value prints as `<.001` rather
  than `.000`.
- A correlation table has the new `N` column and the *p* value lines.
- d-value and one-way tables have a new `n` column, the number of cases
  in each group, after the variable names.
- Titles and notes are worded differently, in APA title case. For
  example, “Means, standard deviations, and correlations with confidence
  intervals” is now “Descriptive Statistics and Correlations”.

## Tables in Quarto documents

This part is new rather than changed. apaTables 4.0 places tables
directly in [Quarto](https://quarto.org) documents formatted by the
[apaquarto](https://wjschne.github.io/apaquarto/) extension, which
produces APA-style manuscripts as PDF, Word or HTML. Build the table on
one line, and pass it to the function named for the document format on
the next. In a `.qmd` document rendered with apaquarto, a table chunk
looks like this:

```` markdown
```{r}
#| label: tbl-correlations
#| message: false
#| warning: true
library(apaTables)
my_table <- apa.cor.table(data = attitude)
apa.quarto.pdf(x = my_table)
```
````

The `#| label:` line must begin with `tbl-`; without it Quarto does not
treat the chunk as a table.

For a full walkthrough, see the companion vignette:

``` r

vignette("apaquarto-usage", package = "apaTables")
```

## Dependency changes

apaTables 4.0 needs R 4.1.0 or later (2.0.8 needed R 3.1.2). The boot,
tibble and methods packages are no longer required. The flextable and
officer packages are new requirements; they write the Word and HTML
files. knitr, which 2.0.8 only suggested, is now required. MBESS is
still required, as in 2.0.8.

## If you used version 3.0.0 or 3.0.1 from GitHub

Those versions were never released to CRAN. In 3.0,
[`apa.cor.table()`](https://dstanley4.github.io/apaTables/reference/apa.cor.table.md)
had `show.pvalue` before `landscape`; in 4.0 it comes after, as the last
argument, so a 3.0 call that gave both without names now needs them
named. Those versions also had two functions that 4.0 removes.
`apa.knit.table.for.pdf()` and `apa.knit.table.for.papaja()` wrote LaTeX
through the kableExtra package. For a table in a PDF, place it in a
Quarto document rendered with apaquarto:

```` markdown
```{r}
#| label: tbl-correlations
#| message: false
#| warning: true
library(apaTables)
my_table <- apa.cor.table(data = attitude)
apa.quarto.pdf(x = my_table)
```
````

For a Word file, use
[`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md):

``` r

my_table <- apa.cor.table(data = attitude)
apa.save(filename = "Table1.docx", my_table)
```

The kableExtra and papaja packages are no longer listed among the
apaTables dependencies.
