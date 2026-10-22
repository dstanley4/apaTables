# Create an APA-style table of pairwise d-values

`apa.d.table()` creates a table of standardized mean differences
(Cohen's d) for all pairwise comparisons of the levels of a grouping
variable, in APA style. The table also reports the mean and standard
deviation for each group, and each d-value is accompanied by its 95%
confidence interval. The table prints to the console. The most common
next step is saving the table as a Word file to use in a manuscript:

## Usage

``` r
apa.d.table(
  iv,
  dv,
  data,
  filename = NA,
  table.number = 1,
  show.conf.interval = TRUE,
  landscape = NA
)
```

## Arguments

- iv:

  Name of the grouping (independent) variable column in `data`, given
  without quotation marks (for example, `iv = dose`, not `iv = "dose"`).

- dv:

  Name of the dependent variable column in `data`, given without
  quotation marks (for example, `dv = libido`, not `dv = "libido"`).

- data:

  A data frame.

- filename:

  Optional file name. If supplied, the table is also saved as a Word
  (`.docx`) or HTML (`.html`) file, according to the file extension.
  Legacy `.doc`/`.rtf` names are saved as `.docx`.

- table.number:

  Number for the "Table N" line of the output. Defaults to `1`. Use `0`
  (or `NA`, or `NULL`) for a table with no number: none is then shown at
  the console or written to the Word file. When
  [`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
  writes several tables to one file, a table left at the default takes
  the next number after the last numbered table before it (starting at
  1), and a number given here is kept. In a Quarto document that uses
  the apaquarto extension this value is ignored: apaquarto numbers the
  tables itself, in the order they appear in the document.

- show.conf.interval:

  Deprecated and ignored; confidence intervals are always displayed.
  This argument will be removed in a later version.

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

    table1 <- apa.d.table(iv = dose, dv = libido, data = viagra)
    apa.save(filename = "Table1.docx", table1)

The file appears in your working directory – the folder R is currently
pointing at. See the *Word files (apa.save)* section below. The table
can also be placed in a Quarto document, so a rendered manuscript comes
out with the table already in place – see the *Quarto documents* section
below.

The d-values are calculated with the pooled-variance formulas 4.18 and
4.19 of Borenstein, Hedges, Higgins, and Rothstein (2009) – that is, the
mean difference divided by the pooled within-groups standard deviation,
with no small-sample correction applied; a d-value is not calculated
when unequal variances prevent pooling. Each confidence interval is the
exact noncentral t interval computed by
[`MBESS::ci.smd()`](https://rdrr.io/pkg/MBESS/man/ci.smd.html).

## Word files (apa.save)

[`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
writes the finished table to a Word file you can open, copy from, or
send – the most common way to use an apaTables table:

    apa.save(filename = "Table1.docx", table1)

The file appears in your working directory – the folder R is currently
pointing at. Several tables can go into one file, each on its own page,
numbered Table 1, Table 2, and so on in the order you list them:

    apa.save(filename = "AllTables.docx", table1, table2)

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
[`apa.quarto.new`](https://dstanley4.github.io/apaTables/reference/apa.quarto.new.md)`(path = "my-paper")`
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
part of the chunk: without it the document cannot find `apa.d.table()`
and the render stops.

    ```{r}
    #| label: tbl-dvalues
    #| message: false
    #| warning: true
    library(apaTables)
    table1 <- apa.d.table(iv = dose, dv = libido, data = viagra)
    apa.quarto.pdf(x = table1)
    ```

That `#| label: tbl-dvalues` line is not optional: without a label
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
document, type `apa.quarto.example(filename = "apaTables-examples.qmd")`
in the R console. Two ready-to-render example documents appear in your
working directory, with the apaquarto extension installed beside them –
one per destination:

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

Borenstein, M., Hedges, L. V., Higgins, J. P. T., & Rothstein, H. R.
(2009). Introduction to meta-analysis. John Wiley & Sons.

## See also

**New to Quarto?**
[`apa.quarto.example`](https://dstanley4.github.io/apaTables/reference/apa.quarto.example.md)
writes a complete, ready-to-render example document with one apaTables
table per page – the fastest way to see every table type working in an
apaquarto document. In the R console, type:

    apa.quarto.example(filename = "apaTables-examples.qmd")

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

# Pairwise d-values for the effect of dose on libido
# (viagra data from Field et al. (2012), Discovering Statistics Using R)
table1 <- apa.d.table(iv = dose, dv = libido, data = viagra)
table1
#> 
#> 
#> Table 1 
#> 
#> Means, Standard Deviations, and d-Values for Libido by Dose
#>  
#> 
#>   Variable     M    SD   1             2            
#>   1. Placebo   2.20 1.30                            
#>                                                     
#>   2. Low Dose  3.20 1.30 0.77                       
#>                          [-0.55, 2.04]              
#>                                                     
#>   3. High Dose 5.00 1.58 1.93          1.24         
#>                          [0.34, 3.44]  [-0.17, 2.59]
#>                                                     
#> 
#> Note. M indicates mean. SD indicates standard deviation. d-values are estimates calculated using formulas 4.18 and 4.19
#> from Borenstein, Hedges, Higgins, & Rothstein (2009). d-values not calculated if unequal variances prevented pooling.
#> Values in square brackets indicate the 95% confidence interval for each d-value. 
#>  
#> 

# Word documents ----------------------------------------
# Save the table as a .docx file (here, in a temporary
# folder; for real use give an ordinary name, such as
# apa.save(filename = "Table1.docx", table1), and the file appears in your
# working directory)
apa.save(filename = file.path(tempdir(), "table1.docx"), table1)

# Quarto documents ----------------------------------------
# Print a ready-to-paste apaquarto code chunk:
apa.quarto.template(x = table1)
#> Copy this chunk into your apaquarto (.qmd) document:
#> 
#> ```{r}
#> #| label: tbl-table1
#> #| message: false
#> #| warning: true
#> # table1 <- apa.d.table(...)   # <- the line that builds your table
#> apa.quarto.pdf(x = table1)
#> ```
#> 
#> The table's title and note need no lines here.
#> apa.quarto.pdf() writes both from the table itself, every time
#> you render, so they stay correct when your analysis changes.
#> To use your own wording instead, add an #| tbl-cap line for the title
#> or an #| apa-note line for the note (or pass title = / note = to it).

# Or write your own chunk; apa.quarto.pdf() displays the table body:
apa.quarto.pdf(x = table1)
#> [1] "+---------------+-------+-------+-----------------+-----------------+\n|Variable       |*M*    |*SD*   |1                |2                |\n+:==============+======:+======:+================:+================:+\n|1\\. Placebo    |2.20   |1.30   |                 |                 |\n+---------------+-------+-------+-----------------+-----------------+\n|2\\. Low Dose   |3.20   |1.30   |0.77\\            |                 |\n|               |       |       |[-0.55, 2.04]    |                 |\n+---------------+-------+-------+-----------------+-----------------+\n|3\\. High Dose  |5.00   |1.58   |1.93\\            |1.24\\            |\n|               |       |       |[0.34, 3.44]     |[-0.17, 2.59]    |\n+---------------+-------+-------+-----------------+-----------------+\n\n::: {.apa-table-html}\n:::\n\n\n::: {.apa-table-meta apa-title='Means, Standard Deviations, and *d*-Values for Libido by Dose' apa-note='[&#34;*M* indicates mean. *SD* indicates standard deviation. *d*-values are estimates calculated using formulas 4.18 and 4.19 from Borenstein, Hedges, Higgins, & Rothstein (2009). *d*-values not calculated if unequal variances prevented pooling. Values in square brackets indicate the 95% confidence interval for each *d*-value.&#34;]'}\n:::\n"
#> attr(,"class")
#> [1] "knit_asis"
#> attr(,"knit_cacheable")
#> [1] NA

# Clean up
unlink(file.path(tempdir(), "table1.docx"))
```
