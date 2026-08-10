# Coerce a builder result (or a bare spec) to an apa_table_spec

Accepts either an \`apa_table_spec\` or an \`apa.\*.table()\` result
that carries a nested \`\$spec\`. This is the bridge that lets a
builder's result be passed straight to \`apa.quarto.pdf()\` while the
builder keeps returning its legacy object.

## Usage

``` r
as_spec(x)
```
