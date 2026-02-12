# Create latex output for an apaTables object

Create latex output for an apaTables object

## Usage

``` r
apa.knit.table.for.pdf(
  table_object,
  table_note = NULL,
  table_title = NULL,
  line_spacing = 1
)
```

## Arguments

- table_object:

  Previously constructed apaTable object

- table_note:

  Replace default table note with this text

- table_title:

  Replace default table title with this text

- line_spacing:

  Line spacing multiplier for table

## Value

A kableExtra LaTeX table object for rendering in PDF documents.
