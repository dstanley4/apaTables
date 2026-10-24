# Convert an apaTables table to a flextable (internal)

Internal. Its one caller is
[`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md),
when it writes a separate `.docx` or `.html` file. The Quarto entry
points
([`apa.quarto.pdf`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md),
[`apa.quarto.docx`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md),
[`apa.quarto.html`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md))
never reach it: PDF gets native LaTeX and every other destination gets a
pandoc grid table
([`apa_quarto_markdown_block()`](https://dstanley4.github.io/apaTables/reference/apa_quarto_markdown_block.md)).

## Usage

``` r
apa_flextable(x)
```

## Arguments

- x:

  An apaTables table (an `apa.*.table()` result or an `apa_table_spec`).

## Value

A `flextable` object that displays the table body.

## Details

`apa_flextable()` converts an apaTables table (an `apa.*.table()`
result) into a "flextable", a table object from the flextable package,
styled for APA: horizontal rules only, Times New Roman 12 point. The
result holds the table body and column headers; the table number, title
and note are added by
[`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md).

Nothing here depends on the shape of a page. Fitting a table to the page
it is written on is
[`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md)'s
job.
