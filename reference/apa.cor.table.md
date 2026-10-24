# Create an APA-style correlation table

`apa.cor.table()` creates a table of means, standard deviations, and
Pearson correlations in APA style. Each correlation is reported with a
95% confidence interval and, optionally, significance stars and a
p-value. Typing `apa.cor.table(data = attitude)` prints the finished
table at the console. The most common next step is saving the table as a
Word file to use in a manuscript:

    table1 <- apa.cor.table(data = attitude)
    apa.save(filename = "Table1.docx", table1)

The file appears in your working directory – the folder R is currently
pointing at. See the *Word files (apa.save)* section below. The table
can also be placed in a Quarto document, so a rendered manuscript comes
out with the table already in place – see the *Quarto documents* section
below.

## Usage

``` r
apa.cor.table(
  data,
  filename = NA,
  table.number = 1,
  show.conf.interval = TRUE,
  show.sig.stars = TRUE,
  landscape = NA,
  show.pvalue = TRUE
)
```

## Arguments

- data:

  A data frame. Non-numeric columns are dropped before the table is
  constructed. At least two numeric columns must remain, and each needs
  at least two different values.

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

  Deprecated and ignored; confidence intervals are always displayed.
  This argument will be removed in a later version.

- show.sig.stars:

  If `TRUE` (the default), correlations are marked with stars for
  significance (\* p \< .05, \*\* p \< .01).

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

- show.pvalue:

  If `TRUE` (the default), the p-value for each correlation is displayed
  below the confidence interval. It comes last among the arguments so
  that a call written for an earlier version, which passed `landscape`
  by position, works as before.

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

For each variable the table reports the number of cases (*N*), mean
(*M*), and standard deviation (*SD*), computed after removing missing
values for that variable. Correlations and the confidence intervals for
them are computed pairwise with
[`cor.test`](https://rdrr.io/r/stats/cor.test.html), so each correlation
uses all cases that are complete for that pair of variables (pairwise
deletion). With missing values, a correlation can rest on fewer cases
than the *N* for either variable; the table note then gives the range of
these pairwise sample sizes. Each confidence interval is the Fisher
r-to-z interval that [`cor.test`](https://rdrr.io/r/stats/cor.test.html)
computes.

## Word files (apa.save)

[`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
writes the finished table to a Word file you can open, copy from, or
send – the most common way to use an apaTables table:

    table1 <- apa.cor.table(data = attitude)
    apa.save(filename = "Table1.docx", table1)

The file appears in your working directory – the folder R is currently
pointing at. Several tables can go into one file, each on a separate
page, numbered Table 1, Table 2, and so on in the order you list them:

    apa.save(filename = "AllTables.docx", table1, table2)

The title above each table and the note below it are written by
apaTables from the table itself; on this route there is no way to supply
your own wording at this time. Tables are set in 12 point, and a table
too wide for an upright page gets a rotated (landscape) page rather than
smaller type – so a wide correlation table coming out sideways is
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
part of the chunk: without it the document cannot find `apa.cor.table()`
and the render stops.

    ```{r}
    #| label: tbl-correlations
    #| message: false
    #| warning: true
    library(apaTables)
    table1 <- apa.cor.table(data = attitude)
    apa.quarto.pdf(x = table1)
    ```

That `#| label: tbl-correlations` line is not optional: without a label
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

# Means, standard deviations, and correlations for the attitude data
table1 <- apa.cor.table(data = attitude)
table1
#> 
#> 
#> Table 1 
#> 
#> Descriptive Statistics and Correlations
#>  
#> 
#>   Variable      N  M     SD    1           2           3           4          
#>   1. rating     30 64.63 12.17                                                
#>                                                                               
#>                                                                               
#>                                                                               
#>   2. complaints 30 66.60 13.31 .83**                                          
#>                                [.66, .91]                                     
#>                                p < .001                                       
#>                                                                               
#>   3. privileges 30 53.13 12.24 .43*        .56**                              
#>                                [.08, .68]  [.25, .76]                         
#>                                p = .019    p = .001                           
#>                                                                               
#>   4. learning   30 56.37 11.74 .62**       .60**       .49**                  
#>                                [.34, .80]  [.30, .79]  [.16, .72]             
#>                                p < .001    p < .001    p = .006               
#>                                                                               
#>   5. raises     30 64.63 10.40 .59**       .67**       .45*        .64**      
#>                                [.29, .78]  [.41, .83]  [.10, .69]  [.36, .81] 
#>                                p < .001    p < .001    p = .014    p < .001   
#>                                                                               
#>   6. critical   30 74.77 9.89  .16         .19         .15         .12        
#>                                [-.22, .49] [-.19, .51] [-.22, .48] [-.25, .46]
#>                                p = .409    p = .321    p = .438    p = .542   
#>                                                                               
#>   7. advance    30 42.93 10.29 .16         .22         .34         .53**      
#>                                [-.22, .49] [-.15, .54] [-.02, .63] [.21, .75] 
#>                                p = .413    p = .233    p = .063    p = .003   
#>                                                                               
#>   5          6          
#>                         
#>                         
#>                         
#>                         
#>                         
#>                         
#>                         
#>                         
#>                         
#>                         
#>                         
#>                         
#>                         
#>                         
#>                         
#>                         
#>                         
#>                         
#>                         
#>                         
#>   .38*                  
#>   [.02, .65]            
#>   p = .040              
#>                         
#>   .57**      .28        
#>   [.27, .77] [-.09, .58]
#>   p < .001   p = .129   
#>                         
#> 
#> Note. N = number of cases. M = mean. SD = standard deviation.
#> Values in square brackets indicate the 95% confidence interval.
#>  * indicates p < .05. ** indicates p < .01.
#>  
#> 

# Word documents ----------------------------------------
# Save the table as a .docx file. This example writes to a temporary
# folder; for real use give an ordinary name, such as
# apa.save(filename = "Table1.docx", table1), and the file appears in your
# working directory.
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
#> # table1 <- apa.cor.table(...)   # <- the line that builds your table
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
