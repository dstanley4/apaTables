# Changelog

## apaTables 4.0.0

This release follows **2.0.8**, the previous version on CRAN, so
everything below describes what changed for someone upgrading from that
version. Versions 3.0.0 and 3.0.1 existed on GitHub only and were never
released.

### apaquarto / Quarto support

- apaTables now ships its own patched copy of the
  [apaquarto](https://wjschne.github.io/apaquarto/) Quarto extension and
  can install it for you, rather than asking you to run Quarto’s own
  extension commands. `apa.quarto.new(path)` creates a folder containing
  a Quarto document with a working APA header and a worked table, with
  the extension already installed beside it, so the very first render
  produces a real APA manuscript.
  [`apa.quarto.setup()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)
  installs or repairs the extension in a project you already have, and
  is silent when the project’s copy is already correct. Running
  `quarto add wjschne/apaquarto` or
  `quarto update extension wjschne/apaquarto` in one of these projects
  silently reverts the extension to the stock, unpatched copy – while
  reporting `[No Change]` – undoing the fixes; running
  [`apa.quarto.setup()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)
  again puts the tested copy back.
- In an apaquarto PDF, a document containing tables written as plain
  markdown now numbers every table 1, 2, 3, … and its cross-references
  match. With the stock extension under Quarto 1.9, each markdown table
  is captioned “Table 1” and every reference to one reads “Table 1”; in
  a document that mixes markdown tables with apaTables tables, the
  apaTables table after each markdown table repeats its number instead.
  The bundled extension carries the fix.
- In an apaquarto PDF, a long table written in markdown, or made by
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) in a
  chunk with a `tbl-cap`, continues onto the next page with every row,
  prints its caption once, and keeps its note with it when tables are
  collected at the end of the manuscript. With the stock extension under
  Quarto 1.9 such a table stops at the foot of its first page and every
  later row is lost, with no error or warning. (Journal mode,
  `documentmode: jou`, still cannot break a table across pages.)
- In an apaquarto PDF the rows of apaTables’ tables, markdown tables and
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) tables
  are single-spaced, as in Word, and a table’s note keeps the
  manuscript’s spacing, as in Word. Each such table closes with a single
  bottom rule (with the stock extension a markdown table without a
  caption ends with two), and a table’s note is kept on a page with at
  least one of its rows unless the note itself is longer than about a
  quarter of a page.
- Tables drop directly into apaquarto documents. Build the table on one
  line and pass it to the entry point named for the document’s own
  destination on the next:
  [`apa.quarto.pdf()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md),
  [`apa.quarto.docx()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md),
  [`apa.quarto.html()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
  or
  [`apa.quarto.revealjs()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md).
  The same analysis produces a PDF, a Word file, a web page or a slide –
  you change the document’s `format:` line and the matching call, not
  your code.
- The table’s number, title and note are supplied automatically. Nothing
  has to be typed for them, and they are re-derived on every render, so
  they stay correct when the analysis changes. To use your own wording
  instead, add a `tbl-cap` or `apa-note` chunk option, or pass `title =`
  / `note =` to the entry point; whichever is supplied wins over the
  generated version, and the chunk option wins over the argument.
  [`apa_caption()`](https://dstanley4.github.io/apaTables/reference/apa_caption.md)
  and
  [`apa_note_text()`](https://dstanley4.github.io/apaTables/reference/apa_note_text.md)
  return apaTables’ own wording for an author who would rather start
  from it.
- [`apa.quarto.template()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.template.md),
  run in the R console, prints a ready-to-paste block of code for a
  table you have already built. It derives the chunk label from the name
  you typed – `apa.quarto.template(table3)` labels the chunk
  `tbl-table3` – so two templates pasted into one document do not
  collide.
- Wide and tall tables are handled without anything being added to the
  document. apaTables measures each table as the document renders and
  asks apaquarto for a rotated (landscape) page when it will not stand
  upright, at full size, under either table placement. A table taller
  than the page continues onto the next one with its column headings
  repeated. Every table function takes a `landscape` argument for the
  cases where you want to overrule the measurement: `TRUE` forces a
  rotated page, `FALSE` forbids one, and the default measures. The chunk
  option `#| apa-landscape:` overrides that argument and can also forbid
  one.
- `font.size` means the same thing on all four Quarto destinations: your
  size exactly, applied to the table body. Where a destination cannot
  deliver the size asked for, it says so in the rendered draft rather
  than silently ignoring it. Word’s range is 6 to 12 pt.
- In Word output, the automatic fit now counts the fixed padding every
  Word table cell carries (which does not shrink with the type, so it
  weighs most on dense tables at small sizes). A many-column table that
  used to be shrunk until it *just* fit — and then wrapped its widest
  column’s labels — now takes the next size down and keeps every label
  on one line.
- The page a table is fitted to is read from the document’s own settings
  rather than assumed: US letter and A4 paper and a `geometry: margin=`
  setting are recognised. When the YAML carries something apaTables
  cannot measure, the standard manuscript page is used and the knitted
  draft says so.
- New
  [`apa.quarto.example()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.example.md)
  writes **two** ready-to-edit example documents that demonstrate every
  table type, one table per page – one document per common destination:
  `<name>-pdf.qmd` renders to a PDF manuscript and `<name>-docx.qmd` to
  a Word manuscript. Both use the apaquarto extension, which the
  function installs alongside them. It returns their paths invisibly as
  the named character vector `c(pdf = , docx = )`. (Using apaTables
  tables in an ordinary Quarto document *without* apaquarto is still
  supported and is covered in the vignette.)
- Quarto slide support is **reveal.js only**.
  [`apa.quarto.revealjs()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
  fits the table to the slide. PowerPoint is not yet supported: a `pptx`
  render still produces the table and warns you, so a deck is never
  handed out without your knowing.
- Journal mode (`documentmode: jou`) is not supported for apaTables
  tables. A two-column page cannot break a table across pages, so rows,
  columns and whole notes can be missing with nothing reported.
  apaTables warns during the render; use `documentmode: man`, or save
  the table with
  [`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md).

### File output

- Table numbers are now real by default. `table.number` defaults to `1`,
  so a table built without one is Table 1; version 2.0.8 defaulted to
  `NA` and printed the literal line “Table NA”. Setting
  `table.number = 0` – or `NA`, or `NULL` – now means the table carries
  no number at all: none is shown at the console or written to the Word
  file. When
  [`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
  writes several tables to one file, a table left at the default is
  numbered by its position in that file (Table 1, Table 2, Table 3),
  while a number you supplied is kept. In an apaquarto document the
  value is ignored, because apaquarto numbers the tables itself.
- [`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
  now writes Microsoft Word `.docx` and `.html` files (chosen by file
  extension) via flextable/officer. The hand-rolled RTF engine has been
  retired; a `.doc`/`.rtf` filename is written as `.docx`.
  [`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
  does not write PDF; for a PDF table, use
  [`apa.quarto.pdf()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
  inside an apaquarto / Quarto document, which is what the error message
  says if you try.
- **Every** table function now takes a `landscape` argument, and it is a
  three-way setting rather than an on/off switch: `TRUE` forces a
  rotated page, `FALSE` forbids one, and the default – `NA` – measures
  the table and decides. **This changes existing behaviour**: in 2.0.8
  [`apa.cor.table()`](https://dstanley4.github.io/apaTables/reference/apa.cor.table.md),
  [`apa.d.table()`](https://dstanley4.github.io/apaTables/reference/apa.d.table.md)
  and
  [`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  defaulted to `landscape = TRUE`, so those tables were rotated whether
  or not they needed it. Nothing rotates now unless it does not fit.
  Pass `landscape = TRUE` for the old orientation.
- [`apa.afex.table()`](https://dstanley4.github.io/apaTables/reference/apa.afex.table.md)
  is new: an APA ANOVA table from
  [`afex::aov_ez()`](https://rdrr.io/pkg/afex/man/aov_car.html) output,
  covering the repeated-measures and mixed designs `apa.ezANOVA.table()`
  used to serve. Its `table.title` argument is honoured in every
  destination, and “ANOVA Results” is the title when none is given.
- `.html` output from
  [`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
  carries the same APA elements as the Word file: the table’s caption
  holds the “Table N” line and the italic title, and the note sits below
  the table, with its italics and super/subscripts intact. Table numbers
  are assigned exactly as for `.docx`, so the same tables saved to both
  formats carry the same numbers.
- Tables are left-aligned so each shares the same left margin as its
  number, title, and note. Correlation and d-value tables add a blank
  line of separation between variable rows for readability.
- Word output follows APA 7 table setup: the table number is bold, the
  title sits one double-spaced line below it, and a blank line separates
  the table from its note. The note preserves italics and
  super/subscripts (e.g., *M*, *SD*, eta-squared symbols), and the
  significance-star sentence (“\* indicates p \< .05. \*\* indicates p
  \< .01.”) starts on its own line. Regression Fit cells render
  R-squared and change-in-R-squared with proper italics, superscripts,
  and the Delta symbol. Table pages use 0.5 in side margins, and a table
  wider than the text area is scaled to fit it rather than clipped by
  Word.
- Notes keep their styling in every destination – italic *M*, subscripts
  and superscripts, and a multi-part note that starts each part, such as
  the significance-star sentence, on its own line.

### Deprecations and dependencies

- **Breaking:** `apa.ezANOVA.table()` has been removed. The `ez` package
  it depended on was archived on CRAN; use
  [`apa.afex.table()`](https://dstanley4.github.io/apaTables/reference/apa.afex.table.md)
  with [`afex::aov_ez()`](https://rdrr.io/pkg/afex/man/aov_car.html)
  output for repeated-measures and mixed designs instead. `ez` is no
  longer listed in Suggests.

- **Breaking:** `apa.reg.boot.table()` has been removed. Bootstrapped
  regression tables are not part of this release; use
  [`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  for the standard regression table.

- Dependencies have changed. `boot`, `tibble` and `MBESS` are no longer
  imported – `boot` and `tibble` are unused, and `MBESS` is now
  suggested rather than required, so it is needed only for the
  confidence intervals in
  [`apa.d.table()`](https://dstanley4.github.io/apaTables/reference/apa.d.table.md).
  `flextable` and `officer` are new imports, used by
  [`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
  to write the Word and HTML files.

- On a Mac without XQuartz, some builds of `gdtools` (a package
  `flextable` needs) cannot load – Posit Package Manager’s can fail this
  way; CRAN’s do not.
  [`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
  and the Quarto entry points now stop with a message that says how to
  fix it (install XQuartz, or reinstall `gdtools` from CRAN) instead of
  a low-level library-loading error.

### Bug fixes

- [`apa.aov.table()`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md)
  labelled the confidence-interval column with the `conf.level` you
  supplied even when that value was out of range. Only `.90` and `.95`
  are supported and anything else is treated as `.90`, so
  `apa.aov.table(fit, conf.level = .85)` computed a 90% interval and
  headed the column “85% CI”. The header, the console column name, and
  the interval now always agree.
- Building a d-value table no longer disables warnings for the rest of
  the session.
  [`MBESS::ci.smd()`](https://rdrr.io/pkg/MBESS/man/ci.smd.html) sets
  `options(warn = -1)` and does not restore it, so after one
  [`apa.d.table()`](https://dstanley4.github.io/apaTables/reference/apa.d.table.md)
  call every later warning – from any package – was discarded; inside a
  Quarto render that included apaTables’ own too-wide-table warning.
  apaTables now saves and restores the option around its MBESS calls.

## apaTables 3.0

### New features

- Support for latex tables via the new command apa.knit.table.for.pdf()
