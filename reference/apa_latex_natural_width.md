# The width a table will occupy as a native LaTeX longtable

Measures the plan's cell strings ONCE, at 12 pt, with the same font
metrics flextable's \`autofit()\` depends on, then scales the CONTENT
term (only) to \`size\` and applies the corrected \`K\` (empirically fit
– see the block comment above this function in \`R/render-latex.R\`; NOT
the theoretical 10/72 in), the \`\tabcolsep\` arithmetic, and a 0.10 in
margin. The result is biased HIGH against real LaTeX layout by design:
across the 8 fixture entries now measured (6, 8, and 10 pt; 4 different
real specs), +0.13 to +0.24 in (2.4-7.5%) – so an error here costs a
needlessly rotated or shrunk page, never a table run off the page edge.

## Usage

``` r
apa_latex_natural_width(plan, size)
```

## Arguments

- plan:

  a display plan, from \`apa_display_plan()\`.

- size:

  numeric point size to lay the table out at.

## Value

width in inches.
