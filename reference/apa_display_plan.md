# Build a destination-neutral display plan from a table spec

Walks an `apa_table_spec` once and produces a plan describing how the
table should be laid out, with every destination-specific decision left
to the caller. The plan is consumed by
[`apa_latex_table()`](https://dstanley4.github.io/apaTables/reference/apa_latex_table.md)
(Quarto PDF, the native LaTeX emitter) and by
[`apa_flextable()`](https://dstanley4.github.io/apaTables/reference/apa_flextable.md)
(which now writes only Word and HTML), and will also be consumed by a
pandoc-markdown emitter – so it carries only structure: column count and
alignment codes, header rows (with spans and rule ranges expressed as
column indices, never as `\cmidrule` or any other markup), and body
rows. Richtext runs (see
[`rt_run`](https://dstanley4.github.io/apaTables/reference/rt_run.md))
pass through unconverted so no emitter loses italics, superscripts, or
symbols; each destination-specific backend
([`rt_to_paragraph()`](https://dstanley4.github.io/apaTables/reference/rt_to_paragraph.md)
for flextable, `rt_run_to_ftext()` for Word via officer,
`rt_to_markdown()` for pandoc) converts them at the point of use, not
here.

## Usage

``` r
apa_display_plan(spec)
```

## Arguments

- spec:

  An `apa_table_spec`, or an `apa.*.table()` result carrying one (see
  [`as_spec`](https://dstanley4.github.io/apaTables/reference/as_spec.md)).

## Value

A list with elements `n_col` (integer), `align` (character vector of
`"l"`/`"r"`/`"c"`, length `n_col`), `header`, and `body` as described
above.

## Details

`header` is a list of rows in RENDERING order: the highest spanner level
first, descending through any lower levels, with the column-label row
always last. Each row is
`list(cells = <list of cell>, rules = <list of column ranges>)`. Each
cell is
`list(rt = <richtext>, span = <columns it covers>, align = <"l"/"r"/"c">)`;
a column not covered by any spanner at that level becomes its own blank,
span-1 cell, so a row's cell spans always sum to `n_col`. `rules` lists
the column range (`c(from, to)`) each spanner in that row covers – the
caller decides what to draw there (a `\cmidrule`, a bottom border,
nothing).

`body` is a list of rows: a `list(type = "group", label = <richtext>)`
entry appears immediately before the first data row of a row group
(added by the internal `apa_row_group()`, which has no help page of its
own to link to), and every spec body row becomes a
`list(type = "data", cells = <list of richtext, one per column>)` entry.
A spec with no row groups produces an all-`"data"` body, reproducing a
flat table exactly.

Every spanner's columns must be contiguous, and no two spanners at the
same level may share a column; either shape makes the header-row walk
silently drop or misattribute a spanner, so both raise an error instead
of returning a wrong-but-silent plan.
