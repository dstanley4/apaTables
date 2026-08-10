# The guidance message for a name/target mismatch (D30)

The table renders correctly either way (the TARGET decides the
mechanism, never the call name); the message states the one useful fact:
which YAML line would make the call name and the render agree. Exported
as an internal helper (not part of the public API) so it can be tested
and reused without duplicating the format tables above.

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
