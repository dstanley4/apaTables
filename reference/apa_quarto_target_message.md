# The guidance message for a name/target mismatch

Used when a chunk calls an entry point that does not match the render
target, e.g.
[`apa.quarto.pdf()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
in a document rendering to Word. The table renders correctly either way,
because the render target decides the mechanism and the function name
does not; the message states the one useful fact, which YAML line would
make the two agree. A separate internal function so the tests can check
the wording directly.

## Usage

``` r
apa_quarto_target_message(called, actual)
```

## Arguments

- called:

  the format key of the function that was called (`"pdf"`, `"docx"`,
  `"html"`, or `"revealjs"`).

- actual:

  the RAW render-target token, e.g. from `apa_quarto_actual_format()`
  (`"latex"`, `"docx"`, `"html"`, ...) – not yet normalised to a format
  key; this function does that itself via `apa_quarto_actual_entry()`.

## Value

A length-1 character string.
