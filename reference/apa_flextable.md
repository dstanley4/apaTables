# Convert an apaTables table to a flextable (internal)

INTERNAL as of Phase 2 (D8): the public chunk functions are
[`apa.quarto.pdf`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
/
[`apa.quarto.docx`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
/
[`apa.quarto.html`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md).
D14 is complete as of Phase 1.6a (Task 8): this builder is now used for
exactly the ONE destination D14 always meant to leave it –
[`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
writing a `.docx` or `.html` file of its own. None of the three Quarto
entry points reach it any more; they route through the native pandoc
grid-table emitter
([`apa_quarto_markdown_block()`](https://dstanley4.github.io/apaTables/reference/apa_quarto_markdown_block.md))
on every non-PDF destination. Un-exporting cost nothing only because
`apa_flextable()` arrived with the unreleased 4.0 work; CRAN carries
3.0.1, which has no such function.

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
result) into a "flextable" – a table format, from the flextable package,
that displays correctly when a document is turned into Word, PDF, or
HTML. Place the call inside a code chunk of an apaquarto document and
the table appears with APA styling (horizontal rules only, Times New
Roman 12 point).

The result contains only the table body and column headers. The table
number, title, and note are added by apaquarto from the `tbl-cap` and
`apa-note` settings lines of the chunk – see the *Quarto documents*
section of any table function's help page (for example,
[`?apa.cor.table`](https://dstanley4.github.io/apaTables/reference/apa.cor.table.md)),
or run
[`apa.quarto.template`](https://dstanley4.github.io/apaTables/reference/apa.quarto.template.md)
in the R console, which writes the whole chunk for you to copy into your
Quarto document. The helpers
[`apa_caption`](https://dstanley4.github.io/apaTables/reference/apa_caption.md)
and
[`apa_note_text`](https://dstanley4.github.io/apaTables/reference/apa_note_text.md)
supply the caption and note text.

Nothing here is affected by the shape of any page. The width fitting
that used to live in this function – and with it the `fit_to_page` and
`page` arguments – belonged to the Quarto PDF path, which now emits
native LaTeX from
[`apa.quarto.pdf`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
and decides size and rotation there (Task 10 removed both arguments).
What reaches Word is fitted by
[`apa.save`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
against the page it is actually writing.
