# Create an APA-style table of means for a one-way design

`apa.1way.table()` creates a table of means and standard deviations of a
dependent variable for each level of a grouping variable, in APA style.
Optionally, the 95% confidence interval for each mean is displayed. The
table prints to the console. The most common next step is saving the
table as a Word file to use in a manuscript:

    table1 <- apa.1way.table(iv = dose, dv = libido, data = viagra)
    apa.save(filename = "Table1.docx", table1)

The file appears in your working directory – the folder R is currently
pointing at. See the *Word files (apa.save)* section below. The table
can also be placed in a Quarto document, so a rendered manuscript comes
out with the table already in place – see the *Quarto documents* section
below.

## Usage

``` r
apa.1way.table(
  iv,
  dv,
  data,
  filename = NA,
  table.number = 1,
  show.conf.interval = FALSE,
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
  Legacy `.doc`/`.rtf` names are saved as `.docx`. An existing file with
  that name is replaced.

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

  If `TRUE`, display the 95% confidence interval for each mean. Defaults
  to `FALSE`.

- landscape:

  Whether this table is placed on a rotated (landscape) page. The
  default, `NA`, means apaTables decides by measuring the table against
  the page. Pass `TRUE` to insist on a rotated page or `FALSE` to insist
  on an upright one. An explicit value is honored wherever the
  destination can carry it out; in a Quarto PDF that means the chunk
  needs a `tbl-` label (see
  [`apa.quarto.pdf`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)).
  A table too wide for the page it is given is shrunk to fit rather than
  having a column cut off. Landscape is a property of a PAGE, so it
  applies to PDF and Word output and is ignored, without complaint, for
  HTML and slides. See **Rotated (landscape) pages** below.

## Value

A table object (class `apa_table`). Typing the object name at the
console displays the table. Pass it to
[`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
to create a Word or HTML file. Or, if you are working in a Quarto
document, use
[`apa.quarto.pdf`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
or
[`apa.quarto.docx`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
to make PDF and Word manuscripts, respectively.

## Details

The size (*n*), mean and standard deviation of each group use the cases
in that group with a value on `dv`; a case with a missing `dv` is left
out, and a case with no value on `iv` is in no group. With
`show.conf.interval = TRUE`, the 95% confidence interval for each mean
is the one-sample *t* interval from
[`t.test`](https://rdrr.io/r/stats/t.test.html), which uses only the
scores in that group, not a pooled error term.

`dv` must be numeric (a TRUE/FALSE `dv` gives proportions). A level of
`iv` that no row of `data` has (for example, one left over after
[`subset()`](https://rdrr.io/r/base/subset.html)) is left out of the
table. Every other group needs at least two cases with a value on `dv`,
or the function stops with a message naming the group.

## Word files (apa.save)

[`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
writes the finished table to a Word file you can open, copy from, or
send – the most common way to use an apaTables table:

    apa.save(filename = "Table1.docx", table1)

The file appears in your working directory – the folder R is currently
pointing at. Several tables can go into one file, each on a separate
page, numbered Table 1, Table 2, and so on in the order you list them:

    apa.save(filename = "AllTables.docx", table1, table2)

The title above each table and the note below it are written by
apaTables from the table itself; on this route there is no way to supply
your own wording at this time. Tables are set in 12 point, and a table
too wide for an upright page gets a rotated (landscape) page rather than
smaller type – so a wide table coming out sideways is expected, not an
error.

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
– one for each Quarto destination, and they behave identically: the YAML
`format:` line in the document decides what is actually produced, so the
name only states intent. The examples on this page use
[`apa.quarto.pdf()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
to match the `format: apaquarto-pdf` shown above; swap in
[`apa.quarto.docx()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
or
[`apa.quarto.html()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
together with the matching YAML line for Word or HTML.

In the document, the table lives inside a code chunk like this one.
[`apa.quarto.pdf`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
displays the table body with the title and note included automatically,
so this chunk is complete – there is no title or note line to write, and
the wording stays correct when the analysis changes. The
[`library(apaTables)`](https://github.com/dstanley4/apaTables) line is
part of the chunk: without it the document cannot find
`apa.1way.table()` and the render stops.

    ```{r}
    #| label: tbl-means
    #| message: false
    #| warning: true
    library(apaTables)
    table1 <- apa.1way.table(iv = dose, dv = libido, data = viagra)
    apa.quarto.pdf(x = table1)
    ```

That `#| label: tbl-means` line is not optional: without a label
starting `tbl-`, Quarto builds no float for the chunk, so there is no
number, no title, and no note, however you try to supply them.

*Wide and tall tables.* If this table turns out wider than the upright
(portrait) PDF page, APA style places it on a rotated (landscape) page –
and apaTables does that for you. It measures the table and asks
apaquarto for a rotated page when one is needed, whether your tables sit
in the text or are collected at the end of the manuscript. There is
nothing to add to the chunk and nothing to add to the YAML header. A
table wider still – wider than even a rotated page – is printed smaller
instead, as small as it has to be so that no column is cut off. If that
takes the type below 6 points, a warning in the rendered draft says so
and points to
[`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
for a full-size Word version. A table too tall for the page needs
nothing either: it continues onto the next page by itself, with the
column headings repeated at the top of each continuation and the note
below the last piece. None of this applies to
[`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
files: there, each table sits alone on a page, always at full size.

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

# Means and standard deviations of libido for each dose
# (viagra data from Field et al. (2012), Discovering Statistics Using R)
table1 <- apa.1way.table(iv = dose, dv = libido,
                         data = viagra, table.number = 1)
table1
#> 
#> 
#> Table 1 
#> 
#> Descriptive Statistics for Libido by Dose 
#> 
#>       dose n    M   SD
#>    Placebo 5 2.20 1.30
#>   Low Dose 5 3.20 1.30
#>  High Dose 5 5.00 1.58
#> 
#> Note. n = group size. M = mean. SD = standard deviation.
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
#> # table1 <- apa.1way.table(...)   # <- the line that builds your table
#> apa.quarto.pdf(x = table1)
#> ```
#> 
#> The title and note need no lines here.
#> apa.quarto.pdf() writes both from the table itself, every time
#> you render, so they stay correct when your analysis changes.
#> To use your own wording instead, add an #| tbl-cap line for the title
#> or an #| apa-note line for the note (or pass title = / note = to it).

# Clean up
unlink(file.path(tempdir(), "table1.docx"))
```
