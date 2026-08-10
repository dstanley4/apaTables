# Escape LaTeX special characters in a plain string

Escape LaTeX special characters in a plain string

## Usage

``` r
apa_latex_escape(txt)
```

## Arguments

- txt:

  a length-1 (or vectorised) character string.

## Value

\`txt\` with LaTeX special characters escaped.

## Invariant

Not safe under double application – see the source comment above this
function for the confirmed counter-example. Call exactly once, on raw
text only.
