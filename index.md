# apaTables Version 4.0.0

A common task faced by researchers is the creation of APA style (i.e.,
American Psychological Association style) tables from statistical
output. In R a large number of function calls are often needed to obtain
all of the desired information for a single APA style table, and
manually re-creating that table in a word processor is prone to
transcription errors. This package builds complete APA style tables
directly from analysis output. A single command saves a finished table
as a Microsoft Word (.docx) file, ready for a manuscript. Tables can
also be placed in Quarto documents (via the bundled apaquarto
extension), so a rendered Word or PDF manuscript comes out with its
tables already in place — numbered, titled, and with notes. The same
tables can be shown on reveal.js slides, and HTML output is also
supported. Using this package minimizes transcription errors and reduces
the number of commands needed by the user.

The development version of the apaTables R package is hosted here on
GitHub. The current stable version is on CRAN, see apaTables
[here.](https://cran.r-project.org/package=apaTables)

**Version 4.0.0 has not been released on CRAN yet.**
`install.packages("apaTables")` still delivers the CRAN version (2.0.8),
which has none of the Version 4.0.0 features described on this page —
including everything in the quick start below. To use them now, install
the development version from GitHub (the second install command near the
end of this page).

## Quick start — the common cases

Most people arrive here with one of two goals, and they call for
different ways of working:

- **Your manuscript lives in Word.** You want a Word file holding a
  finished APA table, so you can copy the table into your manuscript.
- **Your manuscript lives in Quarto** ([quarto.org](https://quarto.org)
  — a free system where your text and R code share one document), and
  you want the rendered Word or PDF manuscript to come out with the
  tables already in place, numbered, in APA style.

This quick start shows both. Everything else — HTML, slides, PowerPoint,
overriding titles, landscape pages, font sizes — is in the numbered
sections that follow.

**A Word table file, to copy into your manuscript.** This way needs
nothing but apaTables itself:

``` r

library(apaTables)

table1 <- apa.cor.table(data = attitude)

regression <- lm(rating ~ complaints + learning, data = attitude)
table2 <- apa.reg.table(regression)

apa.save(filename = "Tables.docx", table1, table2)
```

`Tables.docx` appears in your working directory — the folder R is
currently pointing at — holding both tables, one per page, numbered
Table 1 and Table 2 in the order you listed them. Open it in Word and
copy the tables into your manuscript. That is the whole workflow;
details in section 1a below.

The rest of the quick start is the Quarto way.

**Quarto needs one piece of setup — done for you here.** A Quarto
document only comes out in APA style if the **apaquarto extension** is
installed in an `_extensions` folder beside it; the extension is the
part that turns your table into an APA table. In your own projects, that
installation is what
[`apa.quarto.new()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.new.md)
(a new manuscript) and
[`apa.quarto.setup()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)
(a folder you already have) are for, and every new project needs it done
once. What they install is apaTables’ own tested copy of apaquarto —
apaquarto plus a few table fixes not yet in the standard release — so
use these functions rather than Quarto’s own `quarto add`, which gets
the standard release instead. Both functions used below install it for
you, so there is no separate setup step in this quick start.

**See every table type, in PDF and in Word.** apaTables can write you
complete, working example documents:

``` r

library(apaTables)
apa.quarto.example(filename = "apaTables-examples.qmd")
```

Two example documents appear in your working directory, with the
apaquarto extension installed beside them (an `_extensions` folder) —
one per destination:

    apaTables-examples-pdf.qmd
    apaTables-examples-docx.qmd

The two are the same document, set up for different destinations: render
the `-pdf` file and you get a PDF manuscript; render the `-docx` file
and you get a Word manuscript — both with full APA table numbering,
titles, and notes.

One page of each example uses the afex and tidyr packages, which do not
come with apaTables — install them once with
`install.packages(c("afex", "tidyr"))`. Then render: open either file in
Positron and press Preview, or in any terminal:

``` bash
quarto render apaTables-examples-pdf.qmd
quarto render apaTables-examples-docx.qmd
```

(The PDF render also needs a LaTeX engine such as TinyTeX; the Word
render does not.) Each finished manuscript shows every apaTables table
type, one per page — including two tables too wide to stand upright,
which arrive on rotated (landscape) pages without your having asked.
Open either `.qmd` file in your editor to see the code chunk that made
each table.

**Start your own manuscript.**

``` r

apa.quarto.new(path = "my-paper")
```

This creates a `my-paper` folder holding a working APA manuscript with
the apaquarto extension already installed — the setup from above, done
for you — so the very first render works. The installed extension is
apaTables’ own tested copy of apaquarto, with table fixes not yet in the
standard release, so do not run Quarto’s `quarto add` or `quarto update`
in this folder — both replace it with the standard release. Details in
section 1b below.

## Where do you want your table to end up?

Start with the kind of document you are writing, and read only that
section — each one is complete on its own.

1.  [Microsoft Word](#id_1-how-do-i-make-tables-for-microsoft-word)
2.  [PDF](#id_2-how-do-i-make-tables-in-a-pdf)
3.  [reveal.js slides](#id_3-how-do-i-make-tables-for-revealjs-slides)
4.  [PowerPoint](#id_4-how-do-i-make-tables-for-powerpoint)
5.  [A Quarto document](#id_5-how-do-i-make-tables-in-a-quarto-document)
6.  [Just look at it in R](#id_6-how-do-i-just-look-at-a-table-in-r)

And if you need to change what the table shows:

7.  [Writing your own title or
    note](#id_7-how-do-i-write-my-own-table-title-or-note)
8.  [Putting a table on a landscape
    page](#id_8-how-do-i-put-a-table-on-a-landscape-page)
9.  [Changing a table’s font
    size](#id_9-how-do-i-change-a-tables-font-size)

### 1. How do I make tables for Microsoft Word?

There are two ways, and which suits you depends on where your manuscript
lives.

#### 1a. If you already write your manuscript in Word

Make the table in R, then save it as a Word file:

``` r

library(apaTables)

my_table <- apa.cor.table(data = attitude)
apa.save(filename = "Table1.docx", my_table)
```

A file called `Table1.docx` now sits in your working directory — the
folder R is currently pointing at. Open it in Word and you will find a
finished APA table. Copy it into your manuscript.

To put several tables in one file, list them all:

``` r

apa.save(filename = "AllTables.docx", table1, table2, table3)
```

They are numbered Table 1, Table 2 and Table 3, in the order you list
them. (A number you gave a table when you built it – `table.number = 5`,
say – is kept, and the tables after it count on from it.)

The title and the note are written by apaTables from the table itself.
**On this route there is no way to supply your own wording at this
time** —
[`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
takes a filename and your tables, and nothing else. If you need a
particular title or note, the Quarto route in section 1b can do it;
editing the finished Word file by hand is the other option.

#### 1b. If you write the whole manuscript in Quarto

Quarto lets your writing and your analysis live in one file, so the
finished Word document comes out with the tables already in place and
numbered. To start a paper set up that way, run this once:

``` r

apa.quarto.new(path = "my-paper")
```

> **Do not skip this step, and do not replace it by writing the settings
> block yourself.**
> [`apa.quarto.new()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.new.md)
> writes the starter document *and* installs the **apaquarto extension**
> into an `_extensions` folder beside it. The extension is the part that
> turns your table into an APA table — the settings block on its own
> does nothing without it. A document whose settings say
> `apaquarto-docx` with no extension installed does not render at all:
> Quarto stops with
>
>     ERROR: Unable to read the extension 'apaquarto'.
>
> before any of your R code runs, and produces no file. **If you already
> have a project**, run
> [`apa.quarto.setup()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)
> once in its folder instead — it installs the same thing into a folder
> you already have.
>
> What these functions install is **apaTables’ own tested copy of
> apaquarto** — apaquarto plus a few table fixes that are not yet in the
> standard release. So use these functions to install it; Quarto’s own
> `quarto add` gets the standard release instead — and do not run
> `quarto update` in the folder afterwards, which quietly puts the
> standard release back.

That creates a folder called `my-paper` with a starter document inside
it, and an `_extensions` folder holding the apaquarto extension. Open
the document in Positron. Near the top is a block of settings between
two rows of dashes. The starter arrives set up for PDF, so its format
line says `format: apaquarto-pdf` — for a Word manuscript, change that
line to:

    format: apaquarto-docx

That line is what makes the finished file a Word document. (The
starter’s own worked table block calls
[`apa.quarto.pdf()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
— change that to
[`apa.quarto.docx()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
too, so the call and the format line agree.) Further down, wherever you
want a table, put your analysis in a block like this one:

    ```{r}
    #| label: tbl-correlations
    #| message: false
    #| warning: true
    my_table <- apa.cor.table(data = attitude)
    apa.quarto.docx(x = my_table)
    ```

The `#| label:` line must begin with `tbl-` – without it Quarto does not
treat the block as a table, and it gets no number and no title. Then
render the document (the Preview button in Positron). Quarto runs your R
code and produces `my-paper.docx`, with the table numbered and captioned
for you.

### 2. How do I make tables in a PDF?

PDF goes through Quarto. Quarto lets your writing and your analysis live
in one file, and produces the finished PDF with the tables already in
place and numbered. To start a paper set up that way, run this once:

``` r

apa.quarto.new(path = "my-paper")
```

> **Do not skip this step, and do not replace it by writing the settings
> block yourself.**
> [`apa.quarto.new()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.new.md)
> writes the starter document *and* installs the **apaquarto extension**
> into an `_extensions` folder beside it. The extension is the part that
> turns your table into an APA table — the settings block on its own
> does nothing without it. A document whose settings name an apaquarto
> format with no extension installed does not render at all: Quarto
> stops with
>
>     ERROR: Unable to read the extension 'apaquarto'.
>
> before any of your R code runs, and produces no file. **If you already
> have a project**, run
> [`apa.quarto.setup()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)
> once in its folder instead — it installs the same thing into a folder
> you already have.
>
> What these functions install is **apaTables’ own tested copy of
> apaquarto** — apaquarto plus a few table fixes that are not yet in the
> standard release. So use these functions to install it; Quarto’s own
> `quarto add` gets the standard release instead — and do not run
> `quarto update` in the folder afterwards, which quietly puts the
> standard release back.

That creates a folder called `my-paper` with a starter document inside
it, and an `_extensions` folder holding the apaquarto extension. Open
the document in Positron. In the settings block near the top, the format
line reads `format: apaquarto-pdf`. Where you want the table, put a
block like this one:

    ```{r}
    #| label: tbl-correlations
    #| message: false
    #| warning: true
    my_table <- apa.cor.table(data = attitude)
    apa.quarto.pdf(x = my_table)
    ```

The `#| label:` line must begin with `tbl-` – without it Quarto does not
treat the block as a table, and it gets no number and no title. Then
render the document (the Preview button in Positron), and you get
`my-paper.pdf`.

A table too wide to stand upright is turned sideways for you, and a
table taller than the page continues onto the next one with its column
headings repeated.

[`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
does not write PDF files. If you ask it to, it tells you so and points
you back here.

### 3. How do I make tables for reveal.js slides?

reveal.js is the slide format Quarto produces by default.

**Slides are the one case that needs no extension.** A deck is plain
Quarto: it does not use apaquarto at all, so there is nothing to install
and nothing to repair. None of the
[`apa.quarto.new()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.new.md)
/
[`apa.quarto.setup()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)
advice elsewhere on this page applies to a deck. Start from a plain
`.qmd` file. The trade is that a slide has no table number and no
automatic title or note — a deck has no list of tables to number
against.

In the settings block at the top of your document:

    format: revealjs

and on the slide where you want the table:

    ```{r}
    my_table <- apa.cor.table(data = attitude)
    apa.quarto.revealjs(x = my_table)
    ```

The table is sized to fit the slide. A very large table may still be too
much for one slide — if it looks cramped, show fewer variables.

### 4. How do I make tables for PowerPoint?

Not yet. If you render to PowerPoint, apaTables still puts the table on
the slide and warns you that PowerPoint is not yet supported, so that
you do not hand out a deck without knowing. Support is planned for a
future release; for now, reveal.js is the presentation format apaTables
supports.

### 5. How do I make tables in a Quarto document?

This is the part worth learning. **You write the analysis once. Which
kind of document comes out is one line of settings.**

| What you want | The settings line | The function to call |
|----|----|----|
| A Word document | `format: apaquarto-docx` | [`apa.quarto.docx()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md) |
| A PDF | `format: apaquarto-pdf` | [`apa.quarto.pdf()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md) |
| A web page | `format: apaquarto-html` | [`apa.quarto.html()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md) |
| Slides | `format: revealjs` | [`apa.quarto.revealjs()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md) |

Change those two lines and nothing else changes.

Whether you are new to Quarto or not, start a paper with:

``` r

apa.quarto.new(path = "my-paper")
```

> **Do not skip this step, and do not replace it by writing the settings
> block yourself.**
> [`apa.quarto.new()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.new.md)
> writes the starter document *and* installs the **apaquarto extension**
> into an `_extensions` folder beside it. The extension is the part that
> turns your table into an APA table — the settings block on its own
> does nothing without it. A document whose settings name an apaquarto
> format with no extension installed does not render at all: Quarto
> stops with
>
>     ERROR: Unable to read the extension 'apaquarto'.
>
> before any of your R code runs, and produces no file. **If you already
> have a project**, run
> [`apa.quarto.setup()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)
> once in its folder instead — it installs the same thing into a folder
> you already have.
>
> What these functions install is **apaTables’ own tested copy of
> apaquarto** — apaquarto plus a few table fixes that are not yet in the
> standard release. So use these functions to install it; Quarto’s own
> `quarto add` gets the standard release instead — and do not run
> `quarto update` in the folder afterwards, which quietly puts the
> standard release back.

That gives you a working document to edit rather than a blank page, with
the extension already beside it. The first three rows of the table above
need that extension. **The Slides row does not** — a reveal.js deck is
plain Quarto and never uses apaquarto, so there is nothing to install
for it.

### 6. How do I just look at a table in R?

Print it. Every table-making function returns an object that displays as
a plain APA table at the console — no file and no rendering involved:

``` r

my_table <- apa.cor.table(data = attitude)
my_table
```

This is the quickest way to check your analysis before putting the table
into a document.

## Do you need to change something?

These are the two things people most often want to control by hand. Both
are optional — apaTables handles both for you unless you say otherwise.

### 7. How do I write my own table title or note?

**You usually do not need to.** apaTables writes the title and the note
from the table itself, every time you render, so they stay correct when
your analysis changes. Write your own only when you want particular
wording.

> **This applies to the Quarto route only.** If you are saving a table
> straight to a Word file with
> [`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
> — section 1a above — there is no way to supply your own title or note
> at this time:
> [`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
> takes a filename and your tables, and the title and note come from the
> table itself. The rest of this section will not work on that route.

There are two ways, and they do the same thing.

**With function arguments.** Pass `title =` and `note =` to whichever
entry point matches your document:

``` r

my_table <- apa.cor.table(data = attitude)
apa.quarto.pdf(x = my_table,
               title = "Correlations Among Seven Attitude Measures",
               note  = "N = 30 clerical employees.")
```

The same arguments work on
[`apa.quarto.docx()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
and
[`apa.quarto.html()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md).

**With settings lines in the code block.** `tbl-cap` sets the title and
`apa-note` sets the note:

    ```{r}
    #| label: tbl-correlations
    #| tbl-cap: "Correlations Among Seven Attitude Measures"
    #| apa-note: "N = 30 clerical employees."
    my_table <- apa.cor.table(data = attitude)
    apa.quarto.pdf(x = my_table)
    ```

**If you use both, the settings line wins.** So if you pass `title =`
and the table keeps a different title, look for a `tbl-cap` line in the
same block — that is what is overriding it.

#### Getting italics, superscripts and subscripts

APA notes are full of symbols that have to be italic — *N*, *M*, *SD*,
*p* — and the titles apaTables writes for you already are. When you
write your own, you get them with three pieces of markup:

| you type | you get      | for example                                          |
|----------|--------------|------------------------------------------------------|
| `*M*`    | *M* — italic | `*N* = 30.`                                          |
| `^2^`    | superscript  | `*R*^2^ = .41` gives *R*²                            |
| `~p~`    | subscript    | `*eta*~p~^2^` gives η with a *p* below and a 2 above |

They work in the title and in the note, and in both of the ways shown
above — the `title =` and `note =` arguments, and the `tbl-cap` and
`apa-note` settings lines. So a hand-written note can carry exactly the
same formatting as a generated one:

``` r

my_table <- apa.reg.table(blk1)
apa.quarto.pdf(x = my_table,
               note = "*N* = 200. *R*^2^ is the variance explained.")
```

> **One exception, in the title only.** An APA table title is *already*
> set in italics, so there is nothing to gain by marking a symbol italic
> there — and the result is not the same everywhere: in PDF a `*M*`
> inside the title comes out **upright**, while in Word and HTML it
> stays italic. Superscripts and subscripts are unaffected and behave
> the same everywhere. **In a title, just type the symbol; in a note,
> mark it up as normal.**

**A note that is really several notes** — a general note, then a
probability note — is written as a list, so that each part starts on its
own line as APA requires:

    ```{r}
    #| label: tbl-correlations
    #| apa-note: ["*N* = 30 clerical employees.","\\* indicates *p* < .05."]
    my_table <- apa.cor.table(data = attitude)
    apa.quarto.pdf(x = my_table)
    ```

The backslashes in front of the star keep it from being read as the
start of italics.

#### Greek letters and other symbols

APA notes often need a symbol — Δ*R*², *M* ± *SD*, η²ₚ, μ. **Type the
character itself.** It works in the title and the note, in both of the
ways shown above, and on every destination:

``` r

my_table <- apa.reg.table(blkA, blkB)
apa.quarto.pdf(x = my_table,
               note = "*N* = 200. Δ*R*² is the change in variance explained.")
```

and the same thing as a settings line:

    ```{r}
    #| label: tbl-blocks
    #| apa-note: "*N* = 200. Δ*R*² is the change in variance explained."
    my_table <- apa.reg.table(blkA, blkB)
    apa.quarto.pdf(x = my_table)
    ```

**If you would rather not paste a character you cannot type**, you can
spell it out instead — `\u0394` is Δ, `\u03bc` is μ, `\u03b7` is η, and
`\u00b2` is ². This works in *both* places: R reads the escape inside a
string, and the settings line reads it too, as long as the value is in
**double quotes**.

``` r

apa.quarto.pdf(x = my_table,
               note = "*N* = 200. \u0394*R*\u00b2 is the change in variance explained.")
```

    ```{r}
    #| label: tbl-blocks
    #| apa-note: "*N* = 200. \u0394*R*\u00b2 is the change in variance explained."
    my_table <- apa.reg.table(blkA, blkB)
    apa.quarto.pdf(x = my_table)
    ```

**LaTeX maths notation such as `$\Delta$` also works**, in PDF, Word and
HTML alike — but prefer the plain character anyway. Maths is set in a
*maths* font, so `$\mu$` comes out in a slightly different typeface from
a μ standing next to it in the same sentence, which is visible in all
three. In HTML it is also rendered by a script: one fetched from the
internet on an ordinary render, or built into the file itself when the
document is rendered self-contained. A typed character needs none of
that.

#### Starting from apaTables’ own wording

If you want to *extend* the generated wording rather than replace it,
two helpers hand it to you as ordinary text:
[`apa_caption()`](https://dstanley4.github.io/apaTables/reference/apa_caption.md)
gives the title and
[`apa_note_text()`](https://dstanley4.github.io/apaTables/reference/apa_note_text.md)
gives the note.

**With function arguments**, you can build the new text in the call:

``` r

my_table <- apa.cor.table(data = attitude)
apa.quarto.pdf(x = my_table,
               note = paste(apa_note_text(x = my_table),
                            "Data collected in 2026."))
```

**With settings lines**, print the wording in the R console first and
paste the result in — a settings line is plain text and cannot run R
code:

``` r

my_table <- apa.cor.table(data = attitude)
apa_caption(x = my_table)
#> Descriptive Statistics and Correlations
apa_note_text(x = my_table)
#> N = number of cases. M = mean. SD = standard deviation. ...
```

then edit what you copied:

    ```{r}
    #| label: tbl-correlations
    #| apa-note: "N = number of cases. M = mean. SD = standard deviation. Data collected in 2026."
    my_table <- apa.cor.table(data = attitude)
    apa.quarto.pdf(x = my_table)
    ```

### 8. How do I put a table on a landscape page?

**You usually do not need to.** apaTables measures every table as the
document renders and rotates the ones that will not fit upright onto a
landscape page, at full size. Set this yourself only to overrule that
decision.

**This only means something where there are pages — PDF and Word.** An
HTML page has no pages and neither does a slide, so the setting is
accepted and ignored there, without complaint.

There are two ways, and they do the same thing.

**With a function argument.** Every table function takes `landscape`:

``` r

my_table <- apa.cor.table(data = attitude, landscape = TRUE)
apa.quarto.pdf(x = my_table)
```

It takes three values:

| value | what happens |
|----|----|
| `TRUE` | always a rotated page |
| `FALSE` | never a rotated page — the table is shrunk to fit upright instead |
| `NA` (the default) | apaTables measures the table and decides |

**With a settings line in the code block:**

    ```{r}
    #| label: tbl-correlations
    #| apa-landscape: true
    my_table <- apa.cor.table(data = attitude)
    apa.quarto.pdf(x = my_table)
    ```

**If you use both, the settings line wins** — including when it says
`false` and the argument said `TRUE`.

**The `#| label:` line is required for this to work in PDF.** Without a
label starting `tbl-`, Quarto does not build a table float, and a page
can only be turned for a float. A wide table in an unlabelled block is
printed smaller on an upright page instead, complete — and if it had
asked for a rotated page (`landscape = TRUE`, or the settings line), the
rendered draft says the label is what is missing.

### 9. How do I change a table’s font size?

**You usually do not need to.** apaTables sizes each table as the
document renders: a table too wide for the page is turned landscape
first, and shrunk only when turning is not enough — or not allowed. Set
a size yourself only to overrule that decision.

All four Quarto functions take `font.size`, in points:

``` r

my_table <- apa.cor.table(data = attitude)
apa.quarto.pdf(x = my_table, font.size = 10)
```

The size applies to the body of the table. The title above it and the
note below it keep their usual size.

**What arrives on the page differs by destination:**

| destination | what you get |
|----|----|
| PDF, HTML, reveal.js slides | your size exactly, fractions included — `font.size = 9.5` really is 9.5 pt |
| Word ([`apa.quarto.docx()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)) | whole points only, kept between 6 and 12 |

Word is different because it takes a table’s type size from a named
style in the document template, so the request is rounded down to a
whole point and held inside the 6–12 range — and the rendered draft
tells you when the size delivered is not the size you asked for.

Two warnings to know about; neither stops the render:

- **A size below 6 points** — where an APA table stops being comfortable
  to read — draws a warning on every destination. PDF, HTML and slides
  still deliver it; Word will not go below 6 at all.
- **A size can also be too big.** In PDF, if the table at your requested
  size is wider than the page even after rotation, the rendered draft
  warns that the right-hand columns will be cut off and names the
  largest size that fits — request that, or remove `font.size` and let
  apaTables choose.

**Font size is set only as a function argument.** Unlike landscape
(section 8), there is no `#|` chunk option for it.

**On the
[`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
route (section 1a) there is no font size setting at this time** — those
tables are always 12 point Times New Roman, matching the manuscript body
type APA style expects. If a table is too wide at 12 points,
[`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
turns its page landscape first; only a table too wide even for that (or
kept upright with `landscape = FALSE`) is set in smaller type, so that
every column is kept.

### What’s new in Version 4.0.0

- **One-command APA manuscript**: apaTables now ships its own patched
  copy of the [apaquarto](https://wjschne.github.io/apaquarto/)
  extension and can install it for you.
  `apa.quarto.new(path = "my-paper")` creates a folder with a working
  Quarto document and the extension already installed, so the first
  render produces a real APA manuscript:

  ``` r

  library(apaTables)
  apa.quarto.new(path = "my-paper")
  ```

  [`apa.quarto.setup()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)
  installs or repairs the extension in a project you already have, and
  does nothing on a project that is already correct.

- **Quarto / apaquarto support**: build the table on one line, then pass
  it to
  [`apa.quarto.pdf()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md),
  [`apa.quarto.docx()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md),
  [`apa.quarto.html()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
  or
  [`apa.quarto.revealjs()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
  (named for the document’s own destination) on the next, and the
  apaquarto extension formats it as an APA table in PDF, Word, HTML or
  reveal.js slides:

  ``` r

  my_table <- apa.cor.table(data = attitude)
  apa.quarto.pdf(x = my_table)
  ```

  The helper
  [`apa.quarto.template()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.template.md)
  prints a ready-to-paste block of code for you.

- **Wide and tall tables in PDF handled for you**: a table too wide to
  stand upright is placed on a rotated (landscape) page automatically,
  at full size, and a table taller than the page continues onto the next
  one with its column headings repeated. You do not have to work out in
  advance which of your tables need this — apaTables measures each one
  as the document renders. If you ever need to overrule that decision,
  every table function takes a `landscape` argument: `landscape = TRUE`
  forces a rotated page, `landscape = FALSE` forbids one, and the
  default measures.

- **Word and HTML output**:
  [`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
  writes Microsoft Word (.docx) and HTML files (chosen by the file
  extension) via the flextable and officer packages.

- **`apa.ezANOVA.table()` removed**: The ez package was archived on
  CRAN. Use
  [`apa.afex.table()`](https://dstanley4.github.io/apaTables/reference/apa.afex.table.md)
  with [`afex::aov_ez()`](https://rdrr.io/pkg/afex/man/aov_car.html)
  output for repeated-measures and mixed designs instead.

- **`apa.reg.boot.table()` removed**: bootstrapped regression tables are
  not part of this release.
  [`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  still makes the standard regression table.

- **Upgrading from 2.0.8**: the [migration
  guide](https://dstanley4.github.io/apaTables/articles/migrating-to-4.0.html)
  lists what to change – Word files are now `.docx`, tables are numbered
  1 by default, a table now prints sideways only when it needs to,
  correlation tables show *p* values, and some ANOVA and hierarchical
  regression numbers are corrected.

### Install the CRAN Version

``` r

install.packages("apaTables")

library(apaTables)
```

### Install the Development Version

``` r

install.packages("remotes")

remotes::install_github("dstanley4/apaTables")

library(apaTables)
```

## Tutorial

You can learn how to use apaTables
[here](https://dstanley4.github.io/apaTables/).
