# Knit-print method for apa_table_spec (displays the table in a Quarto chunk)

Called automatically by knitr when an \`apa_table_spec\` object is
returned from a chunk. Delegates to the same rendering
[`apa.quarto.pdf`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
/
[`apa.quarto.docx`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
/
[`apa.quarto.html`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
share, so a bare \`table1\` alone on a line in a chunk takes exactly the
same path as the explicit call the template writes – including the
automatic rotated page for a table too wide to stand upright.
"Rendering" the document turns it into the finished Word, PDF, or HTML
file; this method supplies the table's contribution to that finished
file. There is no function name here for a name/target mismatch message
to name (that guidance is a property of the three named entry points,
not of this automatic path), so none is ever printed for a bare table.

## Usage

``` r
# S3 method for class 'apa_table_spec'
knit_print(x, ...)
```

## Arguments

- x:

  An \`apa_table_spec\` object.

- ...:

  Additional arguments passed on when the table is not going into a PDF.

## Value

A `knit_asis` object.
