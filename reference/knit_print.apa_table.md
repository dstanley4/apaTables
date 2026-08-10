# Knit-print method for apa_table builder results (displays the table in a chunk)

Called automatically by knitr when an \`apa.\*.table()\` result is
returned from a chunk. "Rendering" the document turns it into the
finished Word, PDF, or HTML file. The table body is displayed the same
way
[`apa.quarto.pdf`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
/
[`apa.quarto.docx`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
/
[`apa.quarto.html`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
display it (apaquarto supplies the number, title, and "Note." framing
from the chunk's settings lines); a table object with no spec falls back
to the default knitr printing.

## Usage

``` r
# S3 method for class 'apa_table'
knit_print(x, ...)
```

## Arguments

- x:

  An \`apa_table\` object (the result of an \`apa.\*.table()\` builder).

- ...:

  Additional arguments passed on when the table is not going into a PDF.

## Value

The result of
[`knitr::knit_print`](https://rdrr.io/pkg/knitr/man/knit_print.html): a
`knit_asis` object when the object carries a spec, otherwise the
default.
