# Print a ready-to-paste apaquarto chunk for a table

You have made a table and want it in your Quarto document, but you would
rather not write the code chunk yourself. Ask for it, in the R console:

## Usage

``` r
apa.quarto.template(
  x,
  label = NULL,
  code = NULL,
  target = c("pdf", "docx", "html", "revealjs")
)
```

## Arguments

- x:

  An apaTables table (an `apa.*.table()` result or an `apa_table_spec`).

- label:

  A length-1 character string naming the chunk. By default the name is
  built from the name of the table given for `x`, so
  `apa.quarto.template(table3)` names the chunk `tbl-table3`; when `x`
  is not a stored table but an expression written out in full, the name
  is `tbl-mytable`. A label you supply yourself is used exactly as
  given, so make it begin with `"tbl-"` – Quarto only treats a chunk as
  a table when its label does. Writing `@tbl-table3` in your document
  text then produces that table's number (for example, "Table 1").

- code:

  A length-1 character string with the R code that produces the table,
  used in the display line of the chunk. You can usually ignore this
  argument: by default, whatever you typed for `x` is reused in the
  chunk, so `apa.quarto.template(table1)` prints the line
  `apa.quarto.pdf(table1)`.

- target:

  Which of the format-named entry points to print the chunk for –
  `"pdf"` (the default), `"docx"`, `"html"`, or `"revealjs"`. This only
  picks which name the printed chunk uses; because that name is
  forgiving, the chunk still renders correctly however the document's
  own YAML `format:` line is actually set. Picking the matching name
  simply keeps the printed chunk consistent with the YAML, and changes
  the wide-table advice below to name the right page (a docx table
  rotates like a PDF one; an HTML table has no page at all, so nothing
  rotates and nothing shrinks – it renders at full width regardless, and
  nothing is printed about landscape). `target = "revealjs"` is
  different in one more way: a slide deck has no float, so the chunk it
  prints has no `#| label: tbl-...` line either – see
  [`apa.quarto.revealjs`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
  for why a deck has no table number, and why title and note are opt-in
  there instead of automatic.

## Value

The chunk text as a single character string, returned invisibly (most
users simply copy the printed output; the surrounding instructions are
not part of the returned text).

## Details

    table1 <- apa.cor.table(attitude)
    apa.quarto.template(table1)

This prints a finished chunk, named after your table:

    ```{r}
    #| label: tbl-table1
    #| message: false
    #| warning: true
    # table1 <- apa.cor.table(...)   # <- the line that builds your table
    apa.quarto.pdf(table1)
    ```

Copy it into your `.qmd` document, replace the comment with the line
that builds `table1` –
[`library(apaTables)`](https://github.com/dstanley4/apaTables) with it –
and render. The chunk needs nothing more: the table's title and note are
written from the table itself, every time you render, so there is no
`tbl-cap` or `apa-note` line to fill in and nothing that can quietly
stop matching your analysis.

The display line matches your document's `format:` line – the `target`
argument below picks
[`apa.quarto.pdf`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
(the default),
[`apa.quarto.docx`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md),
[`apa.quarto.html`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md),
or
[`apa.quarto.revealjs`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md).
`target = "revealjs"` prints a chunk showing the table alone, because a
slide adds no title or note unless asked (see **Slides are different**
on
[`apa.quarto.revealjs`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)'s
page). The printed output is self-documenting either way: it says where
to put the chunk. Call this function in the R console – not inside the
Quarto document itself – and copy the chunk it prints into your `.qmd`
file.

**The chunk is always two lines: build the table, then display it.** If
you passed the table-building call itself –
`apa.quarto.template(apa.cor.table(attitude))` – both lines are ready to
run. If you passed a table you had already built, by name, the first
line is a *comment* showing where your own table-building line belongs;
replace it with that line, or delete it if the table is built in an
earlier chunk.

**The chunk's `#| label:` line is not optional** on the three manuscript
destinations, unlike the optional `tbl-cap` and `apa-note` lines this
function no longer prints. A chunk without a label beginning `tbl-` is
not a table as far as Quarto is concerned, so it gets no number, no
title – generated or written by hand – and no note. This function always
writes one for you on those three destinations; keep it if you write the
chunk yourself. A `target = "revealjs"` chunk has no `#| label:` line at
all, and needs none: a slide deck has no float, so a label would buy
nothing.

Every apaquarto document begins with a YAML header: the settings block
between `---` lines at the very top of the document. apaquarto needs an
author there – with no author, or an empty one, the render stops with a
LaTeX error – and it reports a problem unless that author has an
affiliation and is marked `corresponding: true`. This header is enough
to render:

    ---
    title: "My Paper"
    author:
      - name: Jane Doe
        corresponding: true
        affiliations:
          - name: University of Guelph
    format: apaquarto-pdf
    ---

In the R console, create the table and then ask for its Quarto code:

    library(apaTables)
    table1 <- apa.cor.table(attitude)
    apa.quarto.template(table1)

The printed chunk displays `table1` but does not create it, so the chunk
you paste into your Quarto document must also carry the lines that do –
[`library(apaTables)`](https://github.com/dstanley4/apaTables) among
them, without which the render stops with
`could not find function "apa.cor.table"`. This particular table fits an
upright (portrait) PDF page, and a table that does not is handled
without anything being added to the chunk – see *wide tables* below:

    ```{r}
    #| label: tbl-table1
    #| message: false
    #| warning: true
    library(apaTables)
    table1 <- apa.cor.table(attitude)
    apa.quarto.pdf(table1)
    ```

Nothing in that chunk mentions the title or the note, and nothing needs
to:
[`apa.quarto.pdf()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
writes both from the table itself, with the text styling intact (italics
and super/subscripts render as they would anywhere else in the document,
and a multi-part note starts each part, such as the significance-star
sentence, on its own line). They are written again on every render, so
they stay correct when the analysis changes. To use your own wording
instead, add a `tbl-cap` line for the title or an `apa-note` line for
the note, or pass `title =` / `note =` to the entry point function –
whichever is supplied wins over the generated version.
[`apa_caption`](https://dstanley4.github.io/apaTables/reference/apa_caption.md)
returns the generated title if you would rather start from it than write
one from scratch.

*Wide tables.* When the table is too wide for an upright (portrait) PDF
page, APA style places it on a rotated (landscape) page, and that
happens by itself: apaTables measures the table with the same
calculation the chunk will use, and asks apaquarto for a rotated page
when one is needed. This works whether your tables sit in the text or
are collected at the end of the manuscript, and there is nothing to add
to the chunk and nothing to add to the document's YAML header. When the
table given here is a wide one, this function says so and reports its
width, so you know why a page in your document will be turned. A
document that already wraps a chunk in `::: {.landscape}` markers keeps
rendering exactly as before.

*Tall tables.* A table too tall for the page it goes on continues onto
the next one by itself, with the column headings repeated at the top of
each continuation and the note below the last piece. There is no
settings line to add: every apaTables table is written in a form that
can be split across pages, and apaquarto places it accordingly.

Run this function in the R console and copy the printed chunk into your
Quarto document.

Every chunk in a Quarto document must have a different name, so the
chunk's name is built from the name of the table you asked for:
`apa.quarto.template(table3)` names the chunk `tbl-table3`. Two
different tables therefore give two chunks that can both be pasted into
the same document. Dots and underscores in the table's name become
hyphens (`my.table` gives `tbl-my-table`), because a dot inside a name
would break the `@tbl-` reference that produces the table's number in
your text. When there is no name to use – because the table was written
out in full rather than stored under a name – the chunk is named
`tbl-mytable`, and a second such chunk in the same document needs its
name changed by hand. Give the `label` argument to choose the name
yourself.

## See also

[`apa.quarto.new`](https://dstanley4.github.io/apaTables/reference/apa.quarto.new.md)`("my-paper")`
starts an APA manuscript with the apaquarto extension already installed;
[`apa.quarto.setup()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)
installs the extension into a project you already have. A step-by-step
guide to placing apaTables tables in Quarto documents also comes
installed with the package. To open it, type:

    vignette("apaquarto-usage", package = "apaTables")

## Examples

``` r
# Print a ready-to-paste chunk for the table (PDF document by default)
table1 <- apa.cor.table(attitude)
apa.quarto.template(table1)
#> Copy this chunk into your apaquarto (.qmd) document:
#> 
#> ```{r}
#> #| label: tbl-table1
#> #| message: false
#> #| warning: true
#> # table1 <- apa.cor.table(...)   # <- the line that builds your table
#> apa.quarto.pdf(table1)
#> ```
#> 
#> The table's title and note need no lines here.
#> apa.quarto.pdf() writes both from the table itself, every time
#> you render, so they stay correct when your analysis changes.
#> To use your own wording instead, add an #| tbl-cap line for the title
#> or an #| apa-note line for the note (or pass title = / note = to it).

# The same chunk for a Word document
apa.quarto.template(table1, target = "docx")
#> Copy this chunk into your apaquarto (.qmd) document:
#> 
#> ```{r}
#> #| label: tbl-table1
#> #| message: false
#> #| warning: true
#> # table1 <- apa.cor.table(...)   # <- the line that builds your table
#> apa.quarto.docx(table1)
#> ```
#> 
#> The table's title and note need no lines here.
#> apa.quarto.docx() writes both from the table itself, every time
#> you render, so they stay correct when your analysis changes.
#> To use your own wording instead, add an #| tbl-cap line for the title
#> or an #| apa-note line for the note (or pass title = / note = to it).
```
