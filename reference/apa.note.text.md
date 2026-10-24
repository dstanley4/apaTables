# Note text for a table (for the apaquarto `apa-note` settings line)

Returns the table note as a plain-text string, suitable for pasting into
the `apa-note` settings line of a table chunk (a small block of R code)
in an apaquarto document (a Quarto document using the apaquarto
APA-style extension). Returns `""` if the table has no notes. Does not
include "Note.".

## Usage

``` r
apa.note.text(x)
```

## Arguments

- x:

  An apaTables table (an `apa.*.table()` result or an `apa_table_spec`).

## Value

A length-1 character string.

## See also

[`apa.quarto.new`](https://dstanley4.github.io/apaTables/reference/apa.quarto.new.md)`(path = "my-paper")`
starts an APA manuscript with the apaquarto extension already installed;
[`apa.quarto.setup()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)
installs the extension into a project you already have. A step-by-step
guide to placing apaTables tables in Quarto documents also comes
installed with the package. To open it, type:

    vignette("apaquarto-usage", package = "apaTables")

## Examples

``` r
my_table <- apa.cor.table(data = attitude)
apa.note.text(x = my_table)
#> [1] "N = number of cases. M = mean. SD = standard deviation. Values in square brackets indicate the 95% confidence interval. * indicates p < .05. ** indicates p < .01."

# Extend the generated wording rather than replace it
paste(apa.note.text(x = my_table), "Data collected in 2026.")
#> [1] "N = number of cases. M = mean. SD = standard deviation. Values in square brackets indicate the 95% confidence interval. * indicates p < .05. ** indicates p < .01. Data collected in 2026."
```
