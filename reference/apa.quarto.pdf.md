# Display an apaTables table in a Quarto document

You have made a table, and your manuscript is a Quarto document. Put the
table in with a short code chunk, using the function named for the
document's `format:` line:

## Usage

``` r
apa.quarto.revealjs(x, title = NULL, note = NULL, font.size = NULL)

apa.quarto.pdf(x, title = NULL, note = NULL, font.size = NULL)

apa.quarto.docx(x, title = NULL, note = NULL, font.size = NULL)

apa.quarto.html(x, title = NULL, note = NULL, font.size = NULL)
```

## Arguments

- x:

  An apaTables table (an `apa.*.table()` result or an `apa_table_spec`).

- title:

  Optional table title, overriding the one apaTables generated. A plain
  character string, or markdown (e.g. `"Study *N* by Group"`). A
  `tbl-cap` settings line in the chunk still takes precedence over this
  argument. Default `NULL` uses the generated title. **On
  `apa.quarto.revealjs()` this is different**, because a slide reads no
  `tbl-cap`: the default `NULL` puts no title on the slide at all, and
  `TRUE` (rather than a string) requests the generated title – see
  **Slides are different** below.

- note:

  Optional table note, overriding the one apaTables generated. A plain
  character string, or markdown. An `apa-note` settings line in the
  chunk still takes precedence over this argument. Default `NULL` uses
  the generated note. A note given here is one paragraph; apaquarto
  starts a new paragraph wherever the text contains a double quote, a
  comma and a double quote in a row, so a note written with that
  sequence in it is split there. This is long-standing apaquarto
  behaviour, and the same is true of a hand-written `apa-note` settings
  line. A generated note is unaffected. **On `apa.quarto.revealjs()`
  this is different**, because a slide reads no `apa-note`: the default
  `NULL` puts no note on the slide at all, and `TRUE` (rather than a
  string) requests the generated note – see **Slides are different**
  below.

- font.size:

  Optional explicit type size in points for the table body, overriding
  the automatic rotate-then-shrink sequence. The title above the table
  and the note below it keep their usual size. PDF, HTML and slides use
  the size exactly, fractions included. **Word is different**: it takes
  a table's type size from a style in the document template, so it uses
  the requested size rounded down to a whole point, kept between 6 and
  12, and says in the rendered draft when it had to adjust. A size below
  6 points, where an APA table stops being comfortable to read, warns on
  every destination. Nothing is ever refused or cut off. Default `NULL`
  lets apaTables choose.

## Value

The table, ready for the document (a `knit_asis` object).

## Details

    ```{r}
    #| label: tbl-correlations
    #| message: false
    #| warning: true
    library(apaTables)
    table1 <- apa.cor.table(data = attitude)
    apa.quarto.pdf(x = table1)
    ```

Render the document, and the table appears fully APA-formatted – number,
italic title, and *Note.* included – with nothing else written. (New to
Quarto? It is a free system for writing documents that combine text and
R code, at <https://quarto.org>, and
[`apa.quarto.new`](https://dstanley4.github.io/apaTables/reference/apa.quarto.new.md)`(path = "my-paper")`
creates a working APA manuscript with everything already installed. A
slide deck works differently; see **Slides are different** below.)

There are four of these functions, one named for each destination –
`apa.quarto.pdf()`, `apa.quarto.docx()`, `apa.quarto.html()`,
`apa.quarto.revealjs()`. The first three put a table on a manuscript
page and behave identically: put one of them on the last line of a
table's code chunk and the table appears in APA style, number, title and
note included, with nothing else written. **The name states intent; the
document's own YAML `format:` line decides what is actually produced.**
A document rendered with `format: apaquarto-docx` gets a correct Word
table even from a chunk that calls `apa.quarto.pdf()`. Quarto's output
format is a property of the document, not of a chunk, so a function name
can only ever state what you meant. If the name and the render disagree,
the rendered draft carries one line saying exactly which YAML line would
make them agree, for example:

    This document renders to `apaquarto-pdf`. To produce Word,
    set `format: apaquarto-docx` in the YAML.

The table itself is unaffected either way – the message is guidance
only.
[`apa.quarto.template`](https://dstanley4.github.io/apaTables/reference/apa.quarto.template.md)
writes the whole chunk for you, and its `target` argument picks which of
the four names it prints.

`apa.quarto.revealjs()` goes on a slide deck rather than a manuscript
page, and differs from the other three in several ways that matter – see
its own **Slides are different** section below.

The title and note come from the table itself by default on the three
manuscript destinations. Three things can override them, each winning
over the one after it: the chunk's own `tbl-cap` / `apa-note` settings
lines, then the `title` / `note` arguments below, then the generated
value. Supplying both a chunk option and the matching argument is not an
error, but only the chunk option is used. This is identical across the
three manuscript destinations; `apa.quarto.revealjs()` does not use
`tbl-cap` or `apa-note` at all, because a slide deck has no float for
either to attach to (see **Slides are different** below).
[`apa.quarto.template`](https://dstanley4.github.io/apaTables/reference/apa.quarto.template.md)
writes the whole chunk for you to copy; the chunk it prints has no
`tbl-cap` or `apa-note` line at all, because both are automatic – add
one only for your own wording.

A chunk needs a label to become a table at all. Give it a settings line
`#| label: tbl-something` –
[`apa.quarto.template`](https://dstanley4.github.io/apaTables/reference/apa.quarto.template.md)
always writes one. Without a `tbl-` label, Quarto builds no float for
the chunk, so there is no number, no title (generated or written by
hand), and no note, no matter what `tbl-cap`, `apa-note`, `title`, or
`note` say. (This does not apply to `apa.quarto.revealjs()`: a slide
deck builds no float at all, with or without a label, so a table appears
regardless – see **Slides are different** below for how its title and
note are requested instead.)

Nothing needs to be said about the size of the table or the shape of the
page. When a table is too wide for an upright (portrait) page, it is
placed on a rotated (landscape) page automatically, at full size. That
is what APA style asks for: "if a table is too wide to fit on one page,
use landscape orientation on the page with the wide table." A table too
wide to fit even at its smallest readable size is printed smaller still
instead – as small as it has to be, so that no column is cut off. When
that takes the type below 6 points, where an APA table stops being
comfortable to read, a warning in the rendered draft says so and points
to
[`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
for a full-size Word version; the table itself is still complete. A
table taller than the page continues onto the next one, with the column
headings repeated and the note below the last piece. (On an HTML render
there is no page at all, so none of this applies: nothing rotates and
nothing shrinks – the table renders at full size regardless of width. On
a slide render there is no page either, so nothing rotates there. But a
slide's fixed viewport means shrinking IS meaningful, and
`apa.quarto.revealjs()` uses its own separate automatic sizing to fit
the table on the slide rather than this rotate-then-shrink sequence; see
**Slides are different** below.)

There is one table shape apaTables cannot rescue. The space between
columns is a fixed 4 points and does not shrink with the type, so a
table of more than roughly 136 columns is wider than a rotated page at
every possible font size. Such a table is warned about explicitly. No
apaTables function produces one from real data.

Automatic measurement can be overruled per table with the `landscape`
argument every builder accepts, and per chunk with a settings line,
which wins over the argument. Add `#| apa-landscape: true` to a code
chunk to place that table on a rotated page even though it would
otherwise stand upright, or `#| apa-landscape: false` to forbid rotation
and keep it on an upright page instead. A wide table is then shrunk to
fit rather than losing a column.

Rotating a page needs a label. Give every table chunk a settings line
`#| label: tbl-something` – the label a table needs anyway for its
number and for references to it. A chunk without one is not a table as
far as the document is concerned, so its page cannot be turned. A wide
table in such a chunk is printed smaller on the upright page instead,
with every column kept. If the chunk asked for a rotated page, the
rendered draft says the label is what is missing.

## Slides are different

`apa.quarto.revealjs()` places a table on a slide rather than a
manuscript page, and it differs from `apa.quarto.pdf()` /
`apa.quarto.docx()` / `apa.quarto.html()` in ways worth knowing before
you rely on it.

**revealjs is the supported slide format.** Set `format: revealjs` in
the document's YAML – everything below describes that format.
**PowerPoint is not supported yet.** The obstacle is not something a
PowerPoint template or setting can fix. Quarto renders PowerPoint
through pandoc, and pandoc's PowerPoint writer gives no way to set a
table's text size from the document, so a table placed on a PowerPoint
slide comes out at PowerPoint's own type size. Even at a smaller size, a
larger table (a two-block regression table, for instance) is taller than
a slide. Support for PowerPoint is planned for a future release;
apaTables has no working route to a PowerPoint table yet. If a document
using `apa.quarto.revealjs()` renders to PowerPoint anyway, the table
still appears – styled only by PowerPoint's own default table look, not
APA format – and a warning says PowerPoint is not yet supported and
names `format: revealjs` as the supported destination.

**There is no table number, ever.** A manuscript has a running list of
tables to number against – "Table 1", "Table 2", and so on. A slide deck
has no such list, so `apa.quarto.revealjs()` never adds a number.

**Title and note are opt-in, not automatic.**
`apa.quarto.revealjs(x = x)` on its own puts the table alone on the
slide, nothing else. Pass `title = TRUE` and/or `note = TRUE` to add the
title and note apaTables generated for the table – the same arguments
work here as on `apa.quarto.pdf()` and its siblings. This matters most
for a correlation table: its cells mark a significant correlation with
`**`, and the sentence explaining what `**` means
(`* indicates p < .05. ** indicates p < .01.`) lives in the note. Left
at the default, a correlation table on a slide shows the stars with
their meaning nowhere on the slide – `note = TRUE` is the remedy:

    my_table <- apa.cor.table(data = attitude)
    apa.quarto.revealjs(x = my_table, note = TRUE)

**An explicit `font.size` is honoured exactly**, the same value you
typed, though a size below 6 points – where an APA table stops being
comfortable to read – warns in the rendered draft. Left at the default,
apaTables shrinks the table automatically to fit the slide.

Apart from the number, title, note, and sizing, a slide table is the
same table every other destination draws: identical values, identical
formatting.

## Journal mode (a PDF manuscript setting) is not supported

This section concerns PDF manuscripts only. A slide deck never uses
apaquarto, so it is unaffected; so are Word and HTML output.

apaquarto can typeset a manuscript as a printed journal article
(`documentmode: jou`), in two narrow columns. Do not put apaTables
tables in such a document. A two-column page cannot break a table across
pages, so rows, columns and whole notes can be missing from the rendered
PDF with nothing reported anywhere. A document of four ordinary tables
came out with two of the notes and several variable names simply absent.
A warning says so during the render, but it is a warning and not a
repair: there is no setting that makes journal mode safe. Render the
manuscript with `documentmode: man` instead, which is what a journal is
submitted in, or save the table as a Word file with
[`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md).

## See also

[`apa.quarto.new`](https://dstanley4.github.io/apaTables/reference/apa.quarto.new.md)`(path = "my-paper")`
starts an APA manuscript with the apaquarto extension already installed;
[`apa.quarto.setup()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)
installs the extension into a project you already have. A step-by-step
guide to placing apaTables tables in Quarto documents also comes
installed with the package. To open it, type:

    vignette("apaquarto-usage", package = "apaTables")

A table leaves apaTables by one of two routes, and this is one of them.
`apa.quarto.pdf()` / `apa.quarto.docx()` / `apa.quarto.html()` /
`apa.quarto.revealjs()` put a table inside a Quarto document (a
manuscript page or a slide deck);
[`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
writes a table in a Word (`.docx`) or HTML file of its own.
[`apa.quarto.template`](https://dstanley4.github.io/apaTables/reference/apa.quarto.template.md)
writes the whole code chunk for you to copy;
[`apa.quarto.new`](https://dstanley4.github.io/apaTables/reference/apa.quarto.new.md)
creates a Quarto project that renders an APA manuscript on the first
try.

## Examples

``` r
# Build the table once; display it with the entry point named for the
# document's own format: line
my_table <- apa.cor.table(data = attitude)

# In a document whose YAML says format: apaquarto-pdf
apa.quarto.pdf(x = my_table)
#> [1] "+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|Variable              |*N*  |*M*       |*SD*      |1                  |2                  |3                  |4                  |5                |6                  |\n+:=====================+====:+=========:+=========:+==================:+==================:+==================:+==================:+================:+==================:+\n|1\\. rating            |30   |64.63     |12.17     |                   |                   |                   |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|2\\. complaints        |30   |66.60     |13.31     |.83\\*\\*\\           |                   |                   |                   |                 |                   |\n|                      |     |          |          |[.66, .91]\\        |                   |                   |                   |                 |                   |\n|                      |     |          |          |*p* < .001         |                   |                   |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|3\\. privileges        |30   |53.13     |12.24     |.43\\*\\             |.56\\*\\*\\           |                   |                   |                 |                   |\n|                      |     |          |          |[.08, .68]\\        |[.25, .76]\\        |                   |                   |                 |                   |\n|                      |     |          |          |*p* = .019         |*p* = .001         |                   |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|4\\. learning          |30   |56.37     |11.74     |.62\\*\\*\\           |.60\\*\\*\\           |.49\\*\\*\\           |                   |                 |                   |\n|                      |     |          |          |[.34, .80]\\        |[.30, .79]\\        |[.16, .72]\\        |                   |                 |                   |\n|                      |     |          |          |*p* < .001         |*p* < .001         |*p* = .006         |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|5\\. raises            |30   |64.63     |10.40     |.59\\*\\*\\           |.67\\*\\*\\           |.45\\*\\             |.64\\*\\*\\           |                 |                   |\n|                      |     |          |          |[.29, .78]\\        |[.41, .83]\\        |[.10, .69]\\        |[.36, .81]\\        |                 |                   |\n|                      |     |          |          |*p* < .001         |*p* < .001         |*p* = .014         |*p* < .001         |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|6\\. critical          |30   |74.77     |9.89      |.16\\               |.19\\               |.15\\               |.12\\               |.38\\*\\           |                   |\n|                      |     |          |          |[-.22, .49]\\       |[-.19, .51]\\       |[-.22, .48]\\       |[-.25, .46]\\       |[.02, .65]\\      |                   |\n|                      |     |          |          |*p* = .409         |*p* = .321         |*p* = .438         |*p* = .542         |*p* = .040       |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|7\\. advance           |30   |42.93     |10.29     |.16\\               |.22\\               |.34\\               |.53\\*\\*\\           |.57\\*\\*\\         |.28\\               |\n|                      |     |          |          |[-.22, .49]\\       |[-.15, .54]\\       |[-.02, .63]\\       |[.21, .75]\\        |[.27, .77]\\      |[-.09, .58]\\       |\n|                      |     |          |          |*p* = .413         |*p* = .233         |*p* = .063         |*p* = .003         |*p* < .001       |*p* = .129         |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n\n::: {.apa-table-html}\n:::\n\n\n::: {.apa-table-meta apa-title='Descriptive Statistics and Correlations' apa-note='[&#34;*N* = number of cases. *M* = mean. *SD* = standard deviation. Values in square brackets indicate the 95% confidence interval.&#34;,&#34;\\\\* indicates *p* < .05. \\\\*\\\\* indicates *p* < .01.&#34;]'}\n:::\n"
#> attr(,"class")
#> [1] "knit_asis"
#> attr(,"knit_cacheable")
#> [1] NA

# format: apaquarto-docx
apa.quarto.docx(x = my_table)
#> [1] "+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|Variable              |*N*  |*M*       |*SD*      |1                  |2                  |3                  |4                  |5                |6                  |\n+:=====================+====:+=========:+=========:+==================:+==================:+==================:+==================:+================:+==================:+\n|1\\. rating            |30   |64.63     |12.17     |                   |                   |                   |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|2\\. complaints        |30   |66.60     |13.31     |.83\\*\\*\\           |                   |                   |                   |                 |                   |\n|                      |     |          |          |[.66, .91]\\        |                   |                   |                   |                 |                   |\n|                      |     |          |          |*p* < .001         |                   |                   |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|3\\. privileges        |30   |53.13     |12.24     |.43\\*\\             |.56\\*\\*\\           |                   |                   |                 |                   |\n|                      |     |          |          |[.08, .68]\\        |[.25, .76]\\        |                   |                   |                 |                   |\n|                      |     |          |          |*p* = .019         |*p* = .001         |                   |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|4\\. learning          |30   |56.37     |11.74     |.62\\*\\*\\           |.60\\*\\*\\           |.49\\*\\*\\           |                   |                 |                   |\n|                      |     |          |          |[.34, .80]\\        |[.30, .79]\\        |[.16, .72]\\        |                   |                 |                   |\n|                      |     |          |          |*p* < .001         |*p* < .001         |*p* = .006         |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|5\\. raises            |30   |64.63     |10.40     |.59\\*\\*\\           |.67\\*\\*\\           |.45\\*\\             |.64\\*\\*\\           |                 |                   |\n|                      |     |          |          |[.29, .78]\\        |[.41, .83]\\        |[.10, .69]\\        |[.36, .81]\\        |                 |                   |\n|                      |     |          |          |*p* < .001         |*p* < .001         |*p* = .014         |*p* < .001         |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|6\\. critical          |30   |74.77     |9.89      |.16\\               |.19\\               |.15\\               |.12\\               |.38\\*\\           |                   |\n|                      |     |          |          |[-.22, .49]\\       |[-.19, .51]\\       |[-.22, .48]\\       |[-.25, .46]\\       |[.02, .65]\\      |                   |\n|                      |     |          |          |*p* = .409         |*p* = .321         |*p* = .438         |*p* = .542         |*p* = .040       |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|7\\. advance           |30   |42.93     |10.29     |.16\\               |.22\\               |.34\\               |.53\\*\\*\\           |.57\\*\\*\\         |.28\\               |\n|                      |     |          |          |[-.22, .49]\\       |[-.15, .54]\\       |[-.02, .63]\\       |[.21, .75]\\        |[.27, .77]\\      |[-.09, .58]\\       |\n|                      |     |          |          |*p* = .413         |*p* = .233         |*p* = .063         |*p* = .003         |*p* < .001       |*p* = .129         |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n\n::: {.apa-table-html}\n:::\n\n\n::: {.apa-table-meta apa-title='Descriptive Statistics and Correlations' apa-note='[&#34;*N* = number of cases. *M* = mean. *SD* = standard deviation. Values in square brackets indicate the 95% confidence interval.&#34;,&#34;\\\\* indicates *p* < .05. \\\\*\\\\* indicates *p* < .01.&#34;]'}\n:::\n"
#> attr(,"class")
#> [1] "knit_asis"
#> attr(,"knit_cacheable")
#> [1] NA

# format: apaquarto-html
apa.quarto.html(x = my_table)
#> [1] "+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|Variable              |*N*  |*M*       |*SD*      |1                  |2                  |3                  |4                  |5                |6                  |\n+:=====================+====:+=========:+=========:+==================:+==================:+==================:+==================:+================:+==================:+\n|1\\. rating            |30   |64.63     |12.17     |                   |                   |                   |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|2\\. complaints        |30   |66.60     |13.31     |.83\\*\\*\\           |                   |                   |                   |                 |                   |\n|                      |     |          |          |[.66, .91]\\        |                   |                   |                   |                 |                   |\n|                      |     |          |          |*p* < .001         |                   |                   |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|3\\. privileges        |30   |53.13     |12.24     |.43\\*\\             |.56\\*\\*\\           |                   |                   |                 |                   |\n|                      |     |          |          |[.08, .68]\\        |[.25, .76]\\        |                   |                   |                 |                   |\n|                      |     |          |          |*p* = .019         |*p* = .001         |                   |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|4\\. learning          |30   |56.37     |11.74     |.62\\*\\*\\           |.60\\*\\*\\           |.49\\*\\*\\           |                   |                 |                   |\n|                      |     |          |          |[.34, .80]\\        |[.30, .79]\\        |[.16, .72]\\        |                   |                 |                   |\n|                      |     |          |          |*p* < .001         |*p* < .001         |*p* = .006         |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|5\\. raises            |30   |64.63     |10.40     |.59\\*\\*\\           |.67\\*\\*\\           |.45\\*\\             |.64\\*\\*\\           |                 |                   |\n|                      |     |          |          |[.29, .78]\\        |[.41, .83]\\        |[.10, .69]\\        |[.36, .81]\\        |                 |                   |\n|                      |     |          |          |*p* < .001         |*p* < .001         |*p* = .014         |*p* < .001         |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|6\\. critical          |30   |74.77     |9.89      |.16\\               |.19\\               |.15\\               |.12\\               |.38\\*\\           |                   |\n|                      |     |          |          |[-.22, .49]\\       |[-.19, .51]\\       |[-.22, .48]\\       |[-.25, .46]\\       |[.02, .65]\\      |                   |\n|                      |     |          |          |*p* = .409         |*p* = .321         |*p* = .438         |*p* = .542         |*p* = .040       |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|7\\. advance           |30   |42.93     |10.29     |.16\\               |.22\\               |.34\\               |.53\\*\\*\\           |.57\\*\\*\\         |.28\\               |\n|                      |     |          |          |[-.22, .49]\\       |[-.15, .54]\\       |[-.02, .63]\\       |[.21, .75]\\        |[.27, .77]\\      |[-.09, .58]\\       |\n|                      |     |          |          |*p* = .413         |*p* = .233         |*p* = .063         |*p* = .003         |*p* < .001       |*p* = .129         |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n\n::: {.apa-table-html}\n:::\n\n\n::: {.apa-table-meta apa-title='Descriptive Statistics and Correlations' apa-note='[&#34;*N* = number of cases. *M* = mean. *SD* = standard deviation. Values in square brackets indicate the 95% confidence interval.&#34;,&#34;\\\\* indicates *p* < .05. \\\\*\\\\* indicates *p* < .01.&#34;]'}\n:::\n"
#> attr(,"class")
#> [1] "knit_asis"
#> attr(,"knit_cacheable")
#> [1] NA

# format: revealjs -- a slide has no automatic title or note, so pass
# note = TRUE to show the note that explains the significance stars
apa.quarto.revealjs(x = my_table)
#> [1] "+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|Variable              |*N*  |*M*       |*SD*      |1                  |2                  |3                  |4                  |5                |6                  |\n+:=====================+====:+=========:+=========:+==================:+==================:+==================:+==================:+================:+==================:+\n|1\\. rating            |30   |64.63     |12.17     |                   |                   |                   |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|2\\. complaints        |30   |66.60     |13.31     |.83\\*\\*\\           |                   |                   |                   |                 |                   |\n|                      |     |          |          |[.66, .91]\\        |                   |                   |                   |                 |                   |\n|                      |     |          |          |*p* < .001         |                   |                   |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|3\\. privileges        |30   |53.13     |12.24     |.43\\*\\             |.56\\*\\*\\           |                   |                   |                 |                   |\n|                      |     |          |          |[.08, .68]\\        |[.25, .76]\\        |                   |                   |                 |                   |\n|                      |     |          |          |*p* = .019         |*p* = .001         |                   |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|4\\. learning          |30   |56.37     |11.74     |.62\\*\\*\\           |.60\\*\\*\\           |.49\\*\\*\\           |                   |                 |                   |\n|                      |     |          |          |[.34, .80]\\        |[.30, .79]\\        |[.16, .72]\\        |                   |                 |                   |\n|                      |     |          |          |*p* < .001         |*p* < .001         |*p* = .006         |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|5\\. raises            |30   |64.63     |10.40     |.59\\*\\*\\           |.67\\*\\*\\           |.45\\*\\             |.64\\*\\*\\           |                 |                   |\n|                      |     |          |          |[.29, .78]\\        |[.41, .83]\\        |[.10, .69]\\        |[.36, .81]\\        |                 |                   |\n|                      |     |          |          |*p* < .001         |*p* < .001         |*p* = .014         |*p* < .001         |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|6\\. critical          |30   |74.77     |9.89      |.16\\               |.19\\               |.15\\               |.12\\               |.38\\*\\           |                   |\n|                      |     |          |          |[-.22, .49]\\       |[-.19, .51]\\       |[-.22, .48]\\       |[-.25, .46]\\       |[.02, .65]\\      |                   |\n|                      |     |          |          |*p* = .409         |*p* = .321         |*p* = .438         |*p* = .542         |*p* = .040       |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|7\\. advance           |30   |42.93     |10.29     |.16\\               |.22\\               |.34\\               |.53\\*\\*\\           |.57\\*\\*\\         |.28\\               |\n|                      |     |          |          |[-.22, .49]\\       |[-.15, .54]\\       |[-.02, .63]\\       |[.21, .75]\\        |[.27, .77]\\      |[-.09, .58]\\       |\n|                      |     |          |          |*p* = .413         |*p* = .233         |*p* = .063         |*p* = .003         |*p* < .001       |*p* = .129         |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n\n::: {.apa-table-html}\n:::\n\n\n::: {.apa-table-meta apa-title='Descriptive Statistics and Correlations' apa-note='[&#34;*N* = number of cases. *M* = mean. *SD* = standard deviation. Values in square brackets indicate the 95% confidence interval.&#34;,&#34;\\\\* indicates *p* < .05. \\\\*\\\\* indicates *p* < .01.&#34;]'}\n:::\n"
#> attr(,"class")
#> [1] "knit_asis"
#> attr(,"knit_cacheable")
#> [1] NA
apa.quarto.revealjs(x = my_table, note = TRUE)
#> [1] "+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|Variable              |*N*  |*M*       |*SD*      |1                  |2                  |3                  |4                  |5                |6                  |\n+:=====================+====:+=========:+=========:+==================:+==================:+==================:+==================:+================:+==================:+\n|1\\. rating            |30   |64.63     |12.17     |                   |                   |                   |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|2\\. complaints        |30   |66.60     |13.31     |.83\\*\\*\\           |                   |                   |                   |                 |                   |\n|                      |     |          |          |[.66, .91]\\        |                   |                   |                   |                 |                   |\n|                      |     |          |          |*p* < .001         |                   |                   |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|3\\. privileges        |30   |53.13     |12.24     |.43\\*\\             |.56\\*\\*\\           |                   |                   |                 |                   |\n|                      |     |          |          |[.08, .68]\\        |[.25, .76]\\        |                   |                   |                 |                   |\n|                      |     |          |          |*p* = .019         |*p* = .001         |                   |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|4\\. learning          |30   |56.37     |11.74     |.62\\*\\*\\           |.60\\*\\*\\           |.49\\*\\*\\           |                   |                 |                   |\n|                      |     |          |          |[.34, .80]\\        |[.30, .79]\\        |[.16, .72]\\        |                   |                 |                   |\n|                      |     |          |          |*p* < .001         |*p* < .001         |*p* = .006         |                   |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|5\\. raises            |30   |64.63     |10.40     |.59\\*\\*\\           |.67\\*\\*\\           |.45\\*\\             |.64\\*\\*\\           |                 |                   |\n|                      |     |          |          |[.29, .78]\\        |[.41, .83]\\        |[.10, .69]\\        |[.36, .81]\\        |                 |                   |\n|                      |     |          |          |*p* < .001         |*p* < .001         |*p* = .014         |*p* < .001         |                 |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|6\\. critical          |30   |74.77     |9.89      |.16\\               |.19\\               |.15\\               |.12\\               |.38\\*\\           |                   |\n|                      |     |          |          |[-.22, .49]\\       |[-.19, .51]\\       |[-.22, .48]\\       |[-.25, .46]\\       |[.02, .65]\\      |                   |\n|                      |     |          |          |*p* = .409         |*p* = .321         |*p* = .438         |*p* = .542         |*p* = .040       |                   |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n|7\\. advance           |30   |42.93     |10.29     |.16\\               |.22\\               |.34\\               |.53\\*\\*\\           |.57\\*\\*\\         |.28\\               |\n|                      |     |          |          |[-.22, .49]\\       |[-.15, .54]\\       |[-.02, .63]\\       |[.21, .75]\\        |[.27, .77]\\      |[-.09, .58]\\       |\n|                      |     |          |          |*p* = .413         |*p* = .233         |*p* = .063         |*p* = .003         |*p* < .001       |*p* = .129         |\n+----------------------+-----+----------+----------+-------------------+-------------------+-------------------+-------------------+-----------------+-------------------+\n\n::: {.apa-table-html}\n:::\n\n\n::: {.apa-table-meta apa-title='Descriptive Statistics and Correlations' apa-note='TRUE'}\n:::\n"
#> attr(,"class")
#> [1] "knit_asis"
#> attr(,"knit_cacheable")
#> [1] NA
```
