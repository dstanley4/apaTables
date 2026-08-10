# Emit a display plan as a LaTeX longtable

Writes the GRID and nothing else: one complete `longtable` environment,
with no caption, no label, no note and no surrounding float.

## Usage

``` r
apa_latex_table(plan, size = 10)
```

## Arguments

- plan:

  a display plan, from
  [`apa_display_plan()`](https://dstanley4.github.io/apaTables/reference/apa_display_plan.md).

- size:

  numeric point size for the table's text.

## Value

a length-1 character string: one `longtable` environment.

## Why there is no caption (D9, measured)

Quarto's `main.lua` rewrites any raw-LaTeX `longtable` that contains no
`\caption` into `longtable*`, and every later fixup – Quarto's own and
apaquarto's `apalongtablenote.lua` alike – matches the literal UNSTARRED
name, so a starred environment becomes invisible to all of them.
Measured on two byte-identical documents differing only in that one
string: the unstarred form kept 60 of 60 marked rows with the note
spliced into the table's own foot; the starred form kept 45, **rows
46-60 vanished with no error**, and the note printed on another page.
Writing our own `\caption` is not an escape route either – the render
then dies inside Quarto's filter chain (`main.lua:3237`, `refType`).
apaquarto splices the caption in, which is what keeps the name unstarred
and the rows intact, and what keeps `@tbl-` cross-references resolving.
So the emitter writes the grid; apaquarto supplies the number, title,
and note around it.

## Structure

`\begin{longtable}` with the column spec carrying the size and the
inter-column spacing (never a `\setlength` in front of the environment –
see `apa_latex_colspec()` and D10), then the strut fix, then `\toprule`
/ header rows / `\midrule` twice, for `\endfirsthead` and `\endhead`.
`\endfoot` draws NOTHING – no rule may appear at a page break in the
middle of a table – but it is not EMPTY: it carries an invisible
zero-width strut that reserves the room `longtable` needs for the last
foot, without which that foot can break onto a page of its own, carrying
the note away from the last row. `\bottomrule` belongs to
`\endlastfoot`, which must exist because `apalongtablenote.lua` splices
the note into it. Row groups are a full-width bold `\multicolumn`
preceded by `\addlinespace`; spanners are a `\multicolumn` plus
`\cmidrule(lr)`; stacked cells are a nested `tabular`.
