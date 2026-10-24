# Caption text for a table (for the apaquarto `tbl-cap` settings line)

Returns the table title as a plain-text string, suitable for pasting
into the `tbl-cap` settings line of a table chunk (a small block of R
code) in an apaquarto document (a Quarto document using the apaquarto
APA-style extension). Returns `""` if the table has no title. Does not
include "Table N".

## Usage

``` r
apa.caption(x)
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
apa.caption(x = my_table)
#> [1] "Descriptive Statistics and Correlations"
```
