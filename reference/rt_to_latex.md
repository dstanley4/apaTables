# Map a richtext value to a LaTeX string

The LaTeX backend for the richtext run model in \`R/richtext.R\`. Every
run's text is mapped through symbol substitution and character escaping
TOGETHER, in one pass (see \`rt_run_to_latex()\`), before any style
macro is applied, so macros added by this function are never themselves
re-escaped or re-scanned.

## Usage

``` r
rt_to_latex(rt)
```

## Arguments

- rt:

  an \`apa_richtext\` value, or a plain character string.

## Value

a length-1 character string of LaTeX.
