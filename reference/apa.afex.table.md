# Create an APA-style ANOVA table from afex output

`apa.afex.table()` creates an ANOVA table in APA style from the result
of [`afex::aov_ez()`](https://rdrr.io/pkg/afex/man/aov_car.html). It
handles between-participant, within-participant, and mixed designs. For
each effect the table reports the numerator and denominator degrees of
freedom, mean square error, F, p, and generalized eta-squared; for
within-participant effects with a sphericity correction, the epsilon
multiplier is also reported. The table prints to the console. The most
common next step is saving the table as a Word file to use in a
manuscript:

## Usage

``` r
apa.afex.table(
  afex.output,
  correction = "GG",
  table.title = "",
  filename = NA,
  table.number = 1,
  landscape = NA
)
```

## Arguments

- afex.output:

  The result returned by
  [`afex::aov_ez()`](https://rdrr.io/pkg/afex/man/aov_car.html) (an
  `afex_aov` object).

- correction:

  Sphericity correction for within-participant effects: `"GG"`
  (Greenhouse-Geisser, the default), `"HF"` (Huynh-Feldt), or `"none"`.
  Degrees of freedom and p-values in the table incorporate the
  correction.

- table.title:

  Optional string with the table title. When left empty (the default)
  the title is "ANOVA Results". The title given here is used everywhere
  the table appears: at the console, in a Word or HTML file, and in a
  Quarto document.

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

    goggles_results <- afex::aov_ez(id = "participant",
         dv = "attractiveness", data = goggles,
         between = c("gender", "alcohol"))
    table1 <- apa.afex.table(afex.output = goggles_results)
    apa.save(filename = "Table1.docx", table1)

The file appears in your working directory – the folder R is currently
pointing at. See the *Word files (apa.save)* section below. The table
can also be placed in a Quarto document, so a rendered manuscript comes
out with the table already in place – see the *Quarto documents* section
below.

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

**Two other packages must be installed first.** This table is built from
the output of
[`afex::aov_ez()`](https://rdrr.io/pkg/afex/man/aov_car.html), so the
afex package has to be installed; and repeated-measures data usually
arrive with one row per participant, whereas `aov_ez()` needs one row
per observation, so
[`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
is normally used to reshape them. Neither package is installed with
apaTables. In the R console, type:

    install.packages(c("afex", "tidyr"))

Without them the render stops at the chunk below with
`there is no package called 'afex'`.

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
part of the chunk: without it the document cannot find
`apa.afex.table()` and the render stops.

    ```{r}
    #| label: tbl-anova
    #| message: false
    #| warning: true
    library(apaTables)
    goggles_results <- afex::aov_ez(id = "participant",
                       dv = "attractiveness", data = goggles,
                       between = c("gender", "alcohol"))
    table1 <- apa.afex.table(afex.output = goggles_results)
    apa.quarto.pdf(x = table1)
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
if (requireNamespace("afex", quietly = TRUE)) {

#
# ** Example 1: Between Participant Predictors
#

goggles <- apaTables::goggles

goggles_results <- afex::aov_ez(id = "participant", dv = "attractiveness",
                                data = goggles,
                                between = c("gender", "alcohol"))

# Make APA table (all three tables are saved together at the end)
goggles_table <- apa.afex.table(afex.output = goggles_results)
goggles_table

# Quarto documents ----------------------------------------
# Print a ready-to-paste apaquarto code chunk:
apa.quarto.template(x = goggles_table)

# Or write your own chunk; apa.quarto.pdf() displays the table body:
apa.quarto.pdf(x = goggles_table)
}
#> Contrasts set to contr.sum for the following variables: gender, alcohol
#> Copy this chunk into your apaquarto (.qmd) document:
#> 
#> ```{r}
#> #| label: tbl-goggles-table
#> #| message: false
#> #| warning: true
#> # goggles_table <- apa.afex.table(...)   # <- the line that builds your table
#> apa.quarto.pdf(x = goggles_table)
#> ```
#> 
#> The table's title and note need no lines here.
#> apa.quarto.pdf() writes both from the table itself, every time
#> you render, so they stay correct when your analysis changes.
#> To use your own wording instead, add an #| tbl-cap line for the title
#> or an #| apa-note line for the note (or pass title = / note = to it).
#> [1] "+--------------------------------------------------+-----------------+-----------------+-----------------+-----------------+-----------------+-----------+\n|Predictor                                         |*df*~Num~        |*df*~Den~        |*MSE*            |*F*              |*p*              |*η*^2^~g~  |\n+:=================================================+================:+================:+================:+================:+================:+==========:+\n|gender                                            |1                |42               |83.04            |2.03             |.161             |.05        |\n+--------------------------------------------------+-----------------+-----------------+-----------------+-----------------+-----------------+-----------+\n|alcohol                                           |2                |42               |83.04            |20.07            |<.001            |.49        |\n+--------------------------------------------------+-----------------+-----------------+-----------------+-----------------+-----------------+-----------+\n|gender x alcohol                                  |2                |42               |83.04            |11.91            |<.001            |.36        |\n+--------------------------------------------------+-----------------+-----------------+-----------------+-----------------+-----------------+-----------+\n\n::: {.apa-table-html}\n:::\n\n\n::: {.apa-table-meta apa-title='ANOVA Results' apa-note='[&#34;*df*~Num~ = degrees of freedom numerator. *df*~Den~ = degrees of freedom denominator. *MSE* = mean square error. *η*^2^~g~ = generalized eta-squared.&#34;]'}\n:::\n"
#> attr(,"class")
#> [1] "knit_asis"
#> attr(,"knit_cacheable")
#> [1] NA

# \donttest{
if (requireNamespace("afex", quietly = TRUE) &&
    requireNamespace("tidyr", quietly = TRUE)) {

#
# ** Example 2: Within Participant Predictors
#

drink_attitude_wide <- apaTables::drink_attitude_wide

# Convert data from wide format to long format where one row represents
# one OBSERVATION. Wide format column names MUST represent levels of each
# variable separated by an underscore. See vignette for further details.

drink_attitude_long <- tidyr::pivot_longer(drink_attitude_wide,
                                           cols = beer_positive:water_neutral,
                                           names_to = c("drink", "imagery"),
                                           names_sep = "_",
                                           values_to = "attitude")

drink_attitude_long$drink <- as.factor(drink_attitude_long$drink)
drink_attitude_long$imagery <- as.factor(drink_attitude_long$imagery)

drink_attitude_results <- afex::aov_ez(id = "participant", dv = "attitude",
                                       data = drink_attitude_long,
                                       within = c("drink", "imagery"))

# Make APA table (saved together with the others below)
drink_table <- apa.afex.table(afex.output = drink_attitude_results)

#
# ** Example 3: Between and Within Participant Predictors
#

dating_wide <- apaTables::dating_wide

# Convert data from wide format to long format where one row represents
# one OBSERVATION. Wide format column names MUST represent levels of each
# variable separated by an underscore. See vignette for further details.

dating_long <- tidyr::pivot_longer(dating_wide,
                                   cols = attractive_high:ugly_none,
                                   names_to = c("looks", "personality"),
                                   names_sep = "_",
                                   values_to = "date_rating")

dating_long$looks <- as.factor(dating_long$looks)
dating_long$personality <- as.factor(dating_long$personality)

dating_results <- afex::aov_ez(id = "participant", dv = "date_rating",
                               data = dating_long,
                               between = "gender",
                               within = c("looks", "personality"))

# Make APA table (saved together with the others below)
dating_table <- apa.afex.table(afex.output = dating_results)

# ----------------------------------------------------------------
# Save the tables to a Microsoft Word (.docx) file
# ----------------------------------------------------------------
# apa.save() can write several tables into one Word document. This
# example writes to a temporary folder (tempdir()) so it leaves no
# files on your computer; for real use give an ordinary name such as
# "Tables_afex.docx".
apa.save(filename = file.path(tempdir(), "tables_afex.docx"),
         goggles_table,
         drink_table,
         dating_table)

# delete demo file
unlink(file.path(tempdir(), "tables_afex.docx"))
}
#> Contrasts set to contr.sum for the following variables: gender
# }
```
