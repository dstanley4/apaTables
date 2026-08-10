# Create an APA-style fixed-effects ANOVA table

`apa.aov.table()` creates a fixed-effects ANOVA table in APA style from
an [`lm`](https://rdrr.io/r/stats/lm.html) result. For each predictor
the table reports the sum of squares, degrees of freedom, mean square,
F, p, and partial eta-squared with its confidence interval. The table
prints to the console. The most common next step is saving the table as
a Word file to use in a manuscript:

## Usage

``` r
apa.aov.table(
  lm_output,
  filename = NA,
  table.number = 1,
  conf.level = 0.9,
  type = 3,
  landscape = NA
)
```

## Arguments

- lm_output:

  A result from [`lm`](https://rdrr.io/r/stats/lm.html) in which all
  predictors are factors (categorical variables).

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

- conf.level:

  Confidence level (`.90`, the default, or `.95`) for the interval
  around partial eta-squared. Using `.90` keeps conclusions based on
  this interval consistent with the p-value.

- type:

  Sum-of-squares type: `2` (Type II) or `3` (Type III, the default).

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

    options(contrasts = c("contr.sum", "contr.poly"))
    lm_output <- lm(libido ~ dose, data = viagra)
    table1 <- apa.aov.table(lm_output)
    apa.save("Table1.docx", table1)

The file appears in your working directory – the folder R is currently
pointing at. See the *Word files (apa.save)* section below. The table
can also be placed in a Quarto document, so a rendered manuscript comes
out with the table already in place – see the *Quarto documents* section
below.

Confidence intervals for partial eta-squared are calculated with the
noncentral approach described by Smithson (2001) and Fidler and Thompson
(2001). For Type III sums of squares to match programs such as SPSS,
factor contrasts must sum to zero; the examples set
`options(contrasts = c("contr.sum", "contr.poly"))` for this reason.

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
the wording stays correct when the analysis changes. Two lines of the
chunk are easy to leave out and both matter. The first is
[`library(apaTables)`](https://github.com/dstanley4/apaTables): without
it the document cannot find `apa.aov.table()` and the render stops. The
second is the `options(contrasts = ...)` line: without it the Type III
sums of squares are computed from R's default treatment contrasts, and
the table comes out with different – wrong – F values and p-values, with
no warning anywhere. Set the contrasts before fitting the model:

    ```{r}
    #| label: tbl-anova
    #| message: false
    #| warning: true
    library(apaTables)
    options(contrasts = c("contr.sum", "contr.poly"))
    lm_output <- lm(libido ~ dose, data = viagra)
    table1 <- apa.aov.table(lm_output)
    apa.quarto.pdf(table1)
    ```

That `#| label: tbl-anova` line is not optional: without a label
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

Smithson, M. (2001). Correct confidence intervals for various regression
effect sizes and parameters: The importance of noncentral distributions
in computing intervals. Educational and Psychological Measurement,
61(4), 605-632.

Fidler, F., & Thompson, B. (2001). Computing correct confidence
intervals for ANOVA fixed- and random-effects effect sizes. Educational
and Psychological Measurement, 61(4), 575-604.

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

op <- options()   # save the current settings, restored at the end

# 1-way ANOVA of libido by dose
# (viagra data from Field et al. (2012), Discovering Statistics Using R)
# Contrasts must sum to zero for correct Type III sums of squares:
options(contrasts = c("contr.sum", "contr.poly"))
lm_output <- lm(libido ~ dose, data = viagra)
table1 <- apa.aov.table(lm_output, table.number = 4)
table1
#> 
#> 
#> Table 4 
#> 
#> ANOVA Results for Libido
#>  
#> 
#>    Predictor     SS df     MS     F     p partial_eta2 CI_90_partial_eta2
#>  (Intercept) 180.27  1 180.27 91.66 <.001                                
#>         dose  20.13  2  10.06  5.12  .025          .46         [.04, .62]
#>        Error  23.60 12   1.97                                            
#> 
#> Note. SS = Sum of squares. df = degrees of freedom. MS = mean square. CI indicates the confidence interval for partial eta-squared. 
#> 

# 2-way ANOVA from Fidler & Thompson (2001)
options(contrasts = c("contr.sum", "contr.poly"))
lm_output <- lm(dv ~ a*b, data = fidler_thompson)
table2 <- apa.aov.table(lm_output, table.number = 5)

# 2-way ANOVA for the goggles data
# (from Field et al. (2012), Discovering Statistics Using R)
options(contrasts = c("contr.sum", "contr.poly"))
lm_output <- lm(attractiveness ~ gender*alcohol, data = goggles)
table3 <- apa.aov.table(lm_output, table.number = 6)

# Word documents ----------------------------------------
# apa.save() can write several tables into one Word document
# (here, in a temporary
# folder; for real use give an ordinary name, such as
# apa.save("Table1.docx", table1), and the file appears in your
# working directory)
apa.save(file.path(tempdir(), "my_tables.docx"), table1, table2, table3)

# Quarto documents ----------------------------------------
# Print a ready-to-paste apaquarto code chunk:
apa.quarto.template(table1)
#> Copy this chunk into your apaquarto (.qmd) document:
#> 
#> ```{r}
#> #| label: tbl-table1
#> #| message: false
#> #| warning: true
#> # table1 <- apa.aov.table(...)   # <- the line that builds your table
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
#> [1] "+-----------------------------+-----------------+--------+-----------------+--------------+--------------+-----------+--------------------------------+\n|Predictor                    |*SS*             |*df*    |*MS*             |*F*           |*p*           |*η*^2^~p~  |90% CI                          |\n+:============================+================:+=======:+================:+=============:+=============:+==========:+===============================:+\n|dose                         |20.13            |2       |10.06            |5.12          |.025          |.46        |[.04, .62]                      |\n+-----------------------------+-----------------+--------+-----------------+--------------+--------------+-----------+--------------------------------+\n|Error                        |23.60            |12      |1.97             |              |              |           |                                |\n+-----------------------------+-----------------+--------+-----------------+--------------+--------------+-----------+--------------------------------+\n\n::: {.apa-table-html}\n:::\n\n\n::: {.apa-table-meta apa-title='ANOVA Results for Libido' apa-note='[&#34;*SS* = Sum of squares. *df* = degrees of freedom. *MS* = mean square. CI indicates the confidence interval for *η*^2^~p~.&#34;]'}\n:::\n"
#> attr(,"class")
#> [1] "knit_asis"
#> attr(,"knit_cacheable")
#> [1] NA

# Clean up
unlink(file.path(tempdir(), "my_tables.docx"))
options(op)
```
