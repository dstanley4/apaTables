# Decide the layout and emit the markdown block for one table (docx/html)

The non-PDF counterpart of \`apa_quarto_latex_block()\`: same ladder
(\`apa_layout_decide()\`), same "rotate before shrink" policy, but the
output is a pandoc grid table (\`apa_markdown_table()\`) rather than raw
LaTeX, and a font-size choice below the default becomes a STYLE MARKER
(D38, Route A) rather than per-cell markup.

## Usage

``` r
apa_quarto_markdown_block(x, font.size = NULL, title = NULL, note = NULL)
```

## Arguments

- x:

  an apaTables table or spec.

- font.size:

  optional explicit point size, overriding the ladder (D26).

- title:

  optional table title, overriding the one apaTables generated, as the
  \`title\` argument shared by \`apa.quarto.pdf()\` /
  \`apa.quarto.docx()\` / \`apa.quarto.html()\` does on the LaTeX path
  too. A chunk's \`tbl-cap\` still wins over this (D22, resolved by the
  filter, not here).

- note:

  optional table note, overriding the one apaTables generated, mirroring
  \`title\` above. A chunk's \`apa-note\` still wins over this.

## Value

a \`knit_asis\` object.

## Details

Rotation in Word is DECIDED BY MEASUREMENT by default (D32), but Phase
1.7 (Task 3) makes the \`landscape =\` argument – carried on
\`x\$landscape\` – a binding tri-state override. It is resolved against
the chunk option by \`apa_landscape_request()\`: chunk option first,
then the argument, then \`NA\` meaning "measurement decides". The two
Quarto destinations still agree with each other, and with PDF
(\`apa_quarto_latex_block()\`), because both now read the same
precedence.

Title and note (Task 7) are attached the same way
\`apa_quarto_latex_block()\` relies on its caller to attach them –
\`apa_meta_div_for()\`, an empty \`::: {.apa-table-meta}\` div read by
the bundled \`apatablemeta.lua\`. That filter is registered \`at:
post-quarto\` in \`\_extension.yml\`'s COMMON filter list, so it already
runs on this destination too (never reimplemented or branched on
precedence here; see that function's own comment for why). Unlike the
LaTeX path, where \`apa_quarto_render()\` (Task 8; \`apa.quarto.
table()\` did this before it was retired, D30) combines the two outside
\`apa_quarto_latex_block()\`, here the div is appended INSIDE this
function. This is the one function every non-PDF entry point (Task 8)
and every fixture in this suite already calls directly.
