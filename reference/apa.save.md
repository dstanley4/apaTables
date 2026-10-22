# Save apaTables tables to a Word (.docx) or HTML (.html) file

`apa.save()` writes one or more finished tables to a single file – the
way to get an apaTables table into a manuscript you write in Word, or to
give a journal its tables as a file of their own:

## Usage

``` r
apa.save(filename, ..., paper = "us")
```

## Arguments

- filename:

  Output filename ending in `.docx` or `.html` (a `.doc`/`.rtf` name is
  written as `.docx`).

- ...:

  One or more apaTables table objects to save.

- paper:

  Page size for `.docx`: `"us"` (default) or `"a4"`. Capitalization is
  forgiven (`"A4"` works); any other value is an error.

## Value

Invisibly returns the `filename` actually written.

## Details

    table1 <- apa.cor.table(data = attitude)
    apa.save(filename = "Table1.docx", table1)

The file appears in your working directory – the folder R is currently
pointing at. Open it in Word and the finished APA table is there: the
"Table N" line, the italic title, the body, and the *Note.* below it.
Copy it into your manuscript, or send the file as it is. The format is
chosen by the file extension: `.docx` for Word, `.html` for a web page
(a legacy `.doc`/`.rtf` name is written as `.docx`). `apa.save()` does
not write PDF files.

Several tables can go into one file, each on a page of its own, in the
order you list them:

    apa.save(filename = "AllTables.docx", table1, table2, table3)

A number set when a table was built is kept. A table left with its
default number takes the next number after the last numbered table
before it, starting at 1: the three tables above become Table 1, Table 2
and Table 3, and if `table2` had been built with `table.number = 5` they
would become Table 1, Table 5 and Table 6.

Tables are set in 12 point, matching the manuscript body type APA style
expects. Each page is upright (portrait) unless the table is too wide
for it: `apa.save()` measures every table, and one that will not fit
upright is placed on a landscape page at full size. A table too wide
even for a landscape page – or kept upright with `landscape = FALSE` –
is set in smaller type so that every column is kept; no column is cut
off. To decide the orientation yourself, use the `landscape` argument of
the function that built the table – for example
`apa.cor.table(data = attitude, landscape = TRUE)`; an explicit value
always wins over measurement.

**The title and the note are written from the table itself**, so they
stay correct when the analysis changes; there is no `title` or `note`
argument here at this time. If you need particular wording, edit the
finished Word file.

## See also

The functions that make the tables this saves:
[`apa.cor.table`](https://dstanley4.github.io/apaTables/reference/apa.cor.table.md),
[`apa.reg.table`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md),
[`apa.aov.table`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md),
[`apa.1way.table`](https://dstanley4.github.io/apaTables/reference/apa.1way.table.md),
[`apa.2way.table`](https://dstanley4.github.io/apaTables/reference/apa.2way.table.md),
[`apa.afex.table`](https://dstanley4.github.io/apaTables/reference/apa.afex.table.md),
and
[`apa.d.table`](https://dstanley4.github.io/apaTables/reference/apa.d.table.md).

If your manuscript lives in a Quarto document – text and analysis in one
file – place tables with
[`apa.quarto.pdf`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
or
[`apa.quarto.docx`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
there instead of saving separate files.

**The package website** has all of this documentation with rendered
examples: <https://dstanley4.github.io/apaTables/>.

## Examples

``` r
library(apaTables)

table1 <- apa.1way.table(iv = dose, dv = libido,
                         data = viagra, table.number = 1)

apa.save(filename = file.path(tempdir(), "Table1.docx"), table1)

# delete demo file
unlink(file.path(tempdir(), "Table1.docx"))
```
