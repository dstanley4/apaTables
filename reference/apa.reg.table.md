# Create an APA-style regression table

`apa.reg.table()` creates a regression table in APA style from one or
more [`lm`](https://rdrr.io/r/stats/lm.html) results. For each predictor
the table reports the unstandardized regression weight (b), the
standardized regression weight (beta), the semi-partial correlation
squared, and the zero-order correlation, each with its confidence
interval, along with model fit (R-squared). When several `lm` results
are supplied (blocks in a hierarchical regression), the table also
reports the change in R-squared for each block. The table prints to the
console. The most common next step is saving the table as a Word file to
use in a manuscript:

## Usage

``` r
apa.reg.table(
  ...,
  filename = NA,
  table.number = 1,
  prop.var.conf.level = 0.95,
  landscape = NA
)
```

## Arguments

- ...:

  One or more results from [`lm`](https://rdrr.io/r/stats/lm.html) – one
  for each block in a hierarchical regression. All must use the same
  criterion variable.

- filename:

  Optional file name. If supplied, the table is also saved as a Word
  (`.docx`) or HTML (`.html`) file, according to the file extension.
  Legacy `.doc`/`.rtf` names are saved as `.docx`.

- table.number:

  Number for the "Table N" line of the output. Defaults to `1`. Use `0`
  (or `NA`, or `NULL`) for a table with no number: none is then shown at
  the console or written to the Word file. When
  [`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
  writes several tables to one file, a table left at the default is
  numbered by its position in that file, while a number given here is
  kept. In a Quarto document that uses the apaquarto extension this
  value is ignored: apaquarto numbers the tables itself, in the order
  they appear in the document.

- prop.var.conf.level:

  Confidence level (`.90` or `.95`, the default) for the intervals
  around the proportion-of-variance measures (semi-partial correlation
  squared, R-squared, and change in R-squared). Using `.90` keeps
  conclusions based on these intervals consistent with the p-value for
  the block.

- landscape:

  Whether this table is placed on a rotated (landscape) page. The
  default, `NA`, means apaTables decides by measuring the table against
  the page. Pass `TRUE` to insist on a rotated page or `FALSE` to insist
  on an upright one. An explicit value is honoured wherever the
  destination can carry it out; in a Quarto PDF that means the chunk
  needs a `tbl-` label (see
  [`apa.quarto.pdf`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)).
  A table too wide for the page it is given is shrunk to fit rather than
  having a column cut off. Landscape is a property of a PAGE, so it
  applies to PDF and Word output and is ignored, without complaint, for
  HTML and slides. See **Rotated (landscape) pages** below.

## Value

A table object (class `apa_table`). Typing its name at the console
displays the table. Pass it to
[`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
to create a Word or HTML file. Or, if you are working in a Quarto
document, use
[`apa.quarto.pdf`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
or
[`apa.quarto.docx`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
to make PDF and Word manuscripts, respectively.

## Details

    blk1 <- lm(sales ~ adverts + airplay, data = album)
    table1 <- apa.reg.table(blk1)
    apa.save("Table1.docx", table1)

The file appears in your working directory – the folder R is currently
pointing at. See the *Word files (apa.save)* section below. The table
can also be placed in a Quarto document, so a rendered manuscript comes
out with the table already in place – see the *Quarto documents* section
below.

Confidence intervals for the change in R-squared, and for the
semi-partial correlation squared of each predictor when the model has
two or more predictors, are calculated with the asymptotic approach of
Alf and Graf (1999). Note that Algina, Keselman, and Penfield (2008)
found this approach can under some circumstances lead to inaccurate
confidence intervals on proportion-of-variance values.

The confidence interval for R-squared – and, when a model has a single
predictor, the interval for that predictor's semi-partial correlation
squared, which then equals R-squared – is computed by
[`MBESS::ci.R2()`](https://rdrr.io/pkg/MBESS/man/ci.R2.html) using the
noncentral F distribution for fixed predictors.

## Word files (apa.save)

[`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
writes the finished table to a Word file you can open, copy from, or
send – the most common way to use an apaTables table:

    apa.save("Table1.docx", table1)

The file appears in your working directory – the folder R is currently
pointing at. Several tables can go into one file, each on its own page,
numbered Table 1, Table 2, and so on in the order you list them:

    apa.save("AllTables.docx", table1, table2)

The title above each table and the note below it are written by
apaTables from the table itself; on this route there is no way to supply
your own wording at this time. Tables are set in 12 point, and a table
too wide for an upright page gets a rotated (landscape) page of its own
rather than smaller type – so a wide table coming out sideways is
expected, not an error.

## Quarto documents

Quarto (<https://quarto.org>) is a free system for writing documents
that combine text and R code. The apaquarto extension makes a Quarto
document follow APA style: place this table in such a document and it
appears as a fully formatted APA table – number, title, and note
included – in Word, PDF, or HTML. The extension must be installed beside
the document, and one command does it.
[`apa.quarto.new`](https://dstanley4.github.io/apaTables/reference/apa.quarto.new.md)`("my-paper")`
creates a new manuscript folder with the extension already installed;
for a folder you already have,
[`apa.quarto.setup()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)
installs the extension there instead. Tables go into the document inside
a "code chunk": a small block of R code with a few settings lines at the
top. "Rendering" the document turns it into the finished Word, PDF, or
HTML file.

Every apaquarto document begins with a YAML header: the settings block
between `---` lines at the very top of the document. apaquarto needs an
author there – with no author, or an empty one, the render stops with a
LaTeX error – and it reports a problem unless that author has an
affiliation and is marked `corresponding: true`. This header is enough
to render a PDF manuscript:

    ---
    title: "My Paper"
    author:
      - name: Jane Doe
        corresponding: true
        affiliations:
          - name: University of Guelph
    format: apaquarto-pdf
    ---

For a Word manuscript, the same header with one line changed:

    ---
    title: "My Paper"
    author:
      - name: Jane Doe
        corresponding: true
        affiliations:
          - name: University of Guelph
    format: apaquarto-docx
    ---

There are four format-named functions for this –
[`apa.quarto.pdf`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md),
[`apa.quarto.docx`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md),
[`apa.quarto.html`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
and
[`apa.quarto.revealjs`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
– one for each Quarto destination, and they behave identically: the
document's own YAML `format:` line decides what is actually produced, so
the name only states intent. This page's examples use
[`apa.quarto.pdf()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
to match the `format: apaquarto-pdf` shown above; swap in
[`apa.quarto.docx()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
or
[`apa.quarto.html()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
together with the matching YAML line for Word or HTML.

In the document, the table lives inside a code chunk like this one.
[`apa.quarto.pdf`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
displays the table body with its title and note included automatically,
so this chunk is complete – there is no title or note line to write, and
the wording stays correct when the analysis changes. The
[`library(apaTables)`](https://github.com/dstanley4/apaTables) line is
part of the chunk: without it the document cannot find `apa.reg.table()`
and the render stops.

    ```{r}
    #| label: tbl-regression
    #| message: false
    #| warning: true
    library(apaTables)
    blk1 <- lm(sales ~ adverts + airplay, data = album)
    table1 <- apa.reg.table(blk1)
    apa.quarto.pdf(table1)
    ```

That `#| label: tbl-regression` line is not optional: without a label
starting `tbl-`, Quarto builds no float for the chunk, so there is no
number, no title, and no note, however you try to supply them.

*Wide and tall tables.* If this table turns out wider than the upright
(portrait) PDF page, APA style places it on a rotated (landscape) page –
and apaTables does that for you. It measures the table and asks
apaquarto for a rotated page when one is needed, whether your tables sit
in the text or are collected at the end of the manuscript. There is
nothing to add to the chunk and nothing to add to the document's YAML
header. A table wider still – wider than even a rotated page – is
printed smaller instead, as small as it has to be so that no column is
cut off. If that takes the type below 6 points, a warning in the
rendered draft says so and points to
[`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
for a full-size Word version. A table too tall for its page needs
nothing either: it continues onto the next page by itself, with the
column headings repeated at the top of each continuation and the note
below the last piece. None of this applies to
[`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
files: there, each table sits on a page of its own, always at full size.

To see this table (and every apaTables table type) in a complete working
document, type `apa.quarto.example("apaTables-examples.qmd")` in the R
console. Two ready-to-render example documents appear in your working
directory, with the apaquarto extension installed beside them – one per
destination:

    apaTables-examples-pdf.qmd
    apaTables-examples-docx.qmd

Render the `-pdf` file for a PDF manuscript, the `-docx` file for a Word
manuscript.

## Rotated (landscape) pages

A table too wide to stand upright is placed on a rotated page
automatically. You can overrule that per table with `landscape = TRUE`
or `FALSE`, and per chunk in a Quarto document with
`#| apa-landscape: true` or `false`, which wins over the argument.

**The same table can be rotated in one output and upright in another,
and both are right.**
[`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
sets tables in 12 point on a page with half inch side margins; a Quarto
manuscript sets them in 10 point inside a one inch text block. The table
really is wider in the Word file, so it can need a rotated page there
while fitting upright in the PDF. Each destination measures the table it
is actually going to draw.

## References

Alf Jr, E. F., & Graf, R. G. (1999). Asymptotic confidence limits for
the difference between two squared multiple correlations: A simplified
approach. Psychological Methods, 4(1), 70-75.

Algina, J., Keselman, H. J., & Penfield, R. J. (2008). Note on a
confidence interval for the squared semipartial correlation coefficient.
Educational and Psychological Measurement, 68(5), 734-741.

Kelley, K. (2007). Constructing confidence intervals for standardized
effect sizes: Theory, application, and implementation. Journal of
Statistical Software, 20(8), 1-24.

Steiger, J. H., & Fouladi, R. T. (1992). R2: A computer program for
interval estimation, power calculation, and hypothesis testing for the
squared multiple correlation. Behavior Research Methods, Instruments, &
Computers, 24, 581-582.

## See also

**New to Quarto?**
[`apa.quarto.example`](https://dstanley4.github.io/apaTables/reference/apa.quarto.example.md)
writes a complete, ready-to-render example document with one apaTables
table per page – the fastest way to see every table type working in an
apaquarto document. In the R console, type:

    apa.quarto.example("apaTables-examples.qmd")

**Prefer a written guide?** A step-by-step guide to placing these tables
in Quarto documents comes installed with the package. To open it, type:

    vignette("apaquarto-usage", package = "apaTables")

**The package website** has all of this documentation with rendered
examples: <https://dstanley4.github.io/apaTables/>.

Other functions used with this table:
[`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
to save it as a Word or HTML file;
[`apa.quarto.template`](https://dstanley4.github.io/apaTables/reference/apa.quarto.template.md)
and
[`apa.quarto.pdf`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
to place it in a Quarto document.

## Examples

``` r
library(apaTables)

# Single-block regression predicting album sales
# (album data from Field et al. (2012), Discovering Statistics Using R)
blk1 <- lm(sales ~ adverts + airplay, data = album)
table1 <- apa.reg.table(blk1, table.number = 1)
table1
#> 
#> 
#> Table 1 
#> 
#> Regression Predicting Sales
#>  
#> 
#>    Predictor       b       b_95%_CI beta  beta_95%_CI Unique_R2 Unique_95%_CI
#>  (Intercept) 41.12** [22.72, 59.53]                                          
#>      adverts  0.09**   [0.07, 0.10] 0.52 [0.44, 0.61]     .27**    [.18, .36]
#>      airplay  3.59**   [3.02, 4.15] 0.55 [0.46, 0.63]     .29**    [.20, .38]
#>                                                                              
#>                                                                              
#>                                                                              
#>      r             Fit
#>                       
#>  .58**                
#>  .60**                
#>            R2 = .629**
#>        95% CI[.55,.69]
#>                       
#> 
#> Note. N = 200. b = unstandardized regression weight. beta = standardized regression weight. Unique R2 = semipartial correlation squared. r = zero-order correlation. CI = confidence interval.  
#> * indicates p < .05. ** indicates p < .01.
#>  
#> 

# Word documents ----------------------------------------
# Save the table as a .docx file (here, in a temporary
# folder; for real use give an ordinary name, such as
# apa.save("Table1.docx", table1), and the file appears in your
# working directory)
apa.save(file.path(tempdir(), "table1.docx"), table1)

# Quarto documents ----------------------------------------
# Print a ready-to-paste apaquarto code chunk:
apa.quarto.template(table1)
#> Copy this chunk into your apaquarto (.qmd) document:
#> 
#> ```{r}
#> #| label: tbl-table1
#> #| message: false
#> #| warning: true
#> # table1 <- apa.reg.table(...)   # <- the line that builds your table
#> apa.quarto.pdf(table1)
#> ```
#> 
#> The table's title and note need no lines here.
#> apa.quarto.pdf() writes both from the table itself, every time
#> you render, so they stay correct when your analysis changes.
#> To use your own wording instead, add an #| tbl-cap line for the title
#> or an #| apa-note line for the note (or pass title = / note = to it).

# Or write your own chunk; apa.quarto.pdf() displays the table body:
apa.quarto.pdf(table1)
#> [1] "+----------------------+-------------+---------------------------+---------+-----------------------+------------------+--------------------+---------+-----------------------------+\n|Predictor             |*b*          |95% CI                     |*β*      |95% CI                 |Unique *R*^2^     |95% CI              |*r*      |Fit                          |\n+:=====================+============:+:=========================:+========:+:=====================:+=================:+:==================:+========:+:============================+\n|(Intercept)           |41.12\\*\\*    |[22.72, 59.53]             |         |                       |                  |                    |         |                             |\n+----------------------+-------------+---------------------------+---------+-----------------------+------------------+--------------------+---------+-----------------------------+\n|adverts               |0.09\\*\\*     |[0.07, 0.10]               |0.52     |[0.44, 0.61]           |.27\\*\\*           |[.18, .36]          |.58\\*\\*  |                             |\n+----------------------+-------------+---------------------------+---------+-----------------------+------------------+--------------------+---------+-----------------------------+\n|airplay               |3.59\\*\\*     |[3.02, 4.15]               |0.55     |[0.46, 0.63]           |.29\\*\\*           |[.20, .38]          |.60\\*\\*  |                             |\n+----------------------+-------------+---------------------------+---------+-----------------------+------------------+--------------------+---------+-----------------------------+\n|                      |             |                           |         |                       |                  |                    |         |*R*^2^ = .629\\*\\*            |\n+----------------------+-------------+---------------------------+---------+-----------------------+------------------+--------------------+---------+-----------------------------+\n|                      |             |                           |         |                       |                  |                    |         |95% CI[.55,.69]              |\n+----------------------+-------------+---------------------------+---------+-----------------------+------------------+--------------------+---------+-----------------------------+\n|                      |             |                           |         |                       |                  |                    |         |                             |\n+----------------------+-------------+---------------------------+---------+-----------------------+------------------+--------------------+---------+-----------------------------+\n\n::: {.apa-table-html}\n:::\n\n\n::: {.apa-table-meta apa-title='Regression Predicting Sales' apa-note='[&#34;*N* = 200. *b* = unstandardized regression weight. *β* = standardized regression weight. Unique *R*^2^ = semipartial correlation squared. *r* = zero-order correlation. CI = confidence interval.&#34;,&#34;\\\\* indicates *p* < .05. \\\\*\\\\* indicates *p* < .01.&#34;]'}\n:::\n"
#> attr(,"class")
#> [1] "knit_asis"
#> attr(,"knit_cacheable")
#> [1] NA

# Clean up
unlink(file.path(tempdir(), "table1.docx"))

# \donttest{
# Hierarchical regression ----------------------------------------
# Two blocks; more than two can be used
blk1 <- lm(sales ~ adverts, data = album)
blk2 <- lm(sales ~ adverts + airplay + attract, data = album)
table2 <- apa.reg.table(blk1, blk2, table.number = 2)

# Interaction product-term test with blocks
blk1 <- lm(sales ~ adverts + airplay, data = album)
blk2 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data = album)
table3 <- apa.reg.table(blk1, blk2, table.number = 3)

# Interaction product-term test with blocks and additional product terms
blk1 <- lm(sales ~ adverts + airplay, data = album)
blk2 <- lm(sales ~ adverts + airplay + I(adverts*adverts) + I(airplay*airplay),
           data = album)
blk3 <- lm(sales ~ adverts + airplay + I(adverts*adverts) + I(airplay*airplay) +
           I(adverts*airplay), data = album)
table4 <- apa.reg.table(blk1, blk2, blk3, table.number = 4)

# Interaction product-term test with a single regression
# (i.e., semi-partial correlation focus)
blk1 <- lm(sales ~ adverts + airplay + I(adverts * airplay), data = album)
table5 <- apa.reg.table(blk1, table.number = 5)

# apa.save() can write several tables into one Word document
apa.save(file.path(tempdir(), "regression_tables.docx"),
         table1, table2, table3, table4, table5)

# Clean up
unlink(file.path(tempdir(), "regression_tables.docx"))
# }
```
