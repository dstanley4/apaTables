# Changelog

## apaTables 4.0.0

This release follows **2.0.8**, the previous version on CRAN, so
everything below describes what changed for someone upgrading from that
version. Versions 3.0.0 and 3.0.1 existed on GitHub only and were never
released.

### apaquarto / Quarto support

- apaTables now ships a patched copy of the
  [apaquarto](https://wjschne.github.io/apaquarto/) Quarto extension and
  can install it for you, rather than asking you to run the Quarto
  extension commands. `apa.quarto.new(path = "my-paper")` creates a
  folder containing a Quarto document with a working APA header and a
  worked table, with the extension already installed beside it, so the
  very first render produces a real APA manuscript.
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
- In an apaquarto Word or HTML document, markdown tables whose labels
  share a first part – `{#tbl-md-plain}` and `{#tbl-md-note}`, or
  `{#tbl-a_b}` and `{#tbl-a_c}`, say – number 1, 2, 3, … like any
  others, and their cross-references match. With the stock extension
  such tables are captioned with the same number, because each label was
  read only up to the first hyphen, underscore, dot or colon after
  `tbl-`. The bundled extension carries the fix.
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
- An apaquarto PDF with line numbers (`numbered-lines: true`) renders
  when it contains tables: the text around each table is numbered and
  the rows of the table are not. With the stock extension, a captioned
  table set to break across pages (`apa-longtable: true`) stops the
  render with “Undefined control sequence”, and so does an uncaptioned
  table whose first cell spans columns (`\multicolumn`). The bundled
  extension sets every apaTables table and every captioned markdown
  table that way, so without the fix a line-numbered manuscript with
  almost any table would not render.
- In an apaquarto PDF, a table written as raw LaTeX, by
  `knitr::kable(format = "latex")` or kableExtra, in a chunk with a
  `tbl-cap`, prints like any other table: its title sits above the top
  rule and its note below the bottom rule, it stays where it was written
  in the text, and a long one continues onto the next page with every
  row, its column headings repeated and its note at the end. (Two cases
  still print as before: kableExtra’s `latex_options = "scale_down"`,
  and a caption given to `kable(caption = ...)` itself rather than with
  `tbl-cap`.) With the stock extension the title is struck through by
  the top rule, “Note.” prints over the last row, in `documentmode: doc`
  the table drifts away from where it was written, and a long one is cut
  off at the foot of the page with its note.
- Tables drop directly into apaquarto documents. Build the table on one
  line and pass it to the entry point named for the output format on the
  next:
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
  [`apa.caption()`](https://dstanley4.github.io/apaTables/reference/apa.caption.md)
  and
  [`apa.note.text()`](https://dstanley4.github.io/apaTables/reference/apa.note.text.md)
  return the apaTables wording for an author who would rather start from
  it.
- [`apa.quarto.template()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.template.md),
  run in the R console, prints a ready-to-paste block of code for a
  table you have already built. It derives the chunk label from the name
  you typed – `apa.quarto.template(x = table3)` labels the chunk
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
- The Quarto entry points take a `font.size` argument, and it means the
  same thing on all four destinations: your size exactly, applied to the
  table body. Where a destination cannot deliver the size asked for, it
  says so in the rendered draft. Word’s range is 6 to 12 pt.
- In Word output, the automatic fit counts the fixed padding every Word
  table cell carries (which does not shrink with the type, so it weighs
  most on dense tables at small sizes), so a many-column table is set at
  a size that keeps every label on one line.
- The page a table is fitted to is read from the settings in the
  document rather than assumed: US letter and A4 paper and a
  `geometry: margin=` setting are recognized. When the YAML carries
  something apaTables cannot measure, the standard manuscript page is
  used and the knitted draft says so.
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
  columns and whole notes can be missing, or the render can stop with a
  LaTeX error and write no PDF. When the PDF does render, apaTables puts
  a warning in it; use `documentmode: man`, or save the table with
  [`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md).

### File output

- Table numbers are now real by default. `table.number` defaults to `1`,
  so a table built without one is Table 1; version 2.0.8 defaulted to
  `NA`, which showed no number at the console and wrote “Table XX” in
  the file. Setting `table.number = 0` – or `NA`, or `NULL` – now means
  the table carries no number at all: none is shown at the console or
  written to the Word file. When
  [`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
  writes several tables to one file, a number you supplied is kept, and
  a table left at the default takes the next number after the last
  numbered table before it, starting at 1 (three defaults give Table 1,
  2, 3; a supplied 5 in the middle gives Table 1, 5, 6). In an apaquarto
  document the value is ignored, because apaquarto numbers the tables
  itself.
- New
  [`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
  writes one or more tables to a Microsoft Word `.docx` or an `.html`
  file (chosen by file extension) via flextable/officer. A table
  function’s `filename =` argument writes the same files: the
  hand-rolled RTF engine of 2.0.8 has been retired, so a `.doc`/`.rtf`
  filename is written as `.docx`, and any other ending stops with an
  error.
  [`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
  does not write PDF; for a PDF table, use
  [`apa.quarto.pdf()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
  inside an apaquarto / Quarto document, which is what the error message
  says if you try.
- **Every** table function now takes a `landscape` argument, and it is a
  three-way setting rather than an on/off switch: `TRUE` forces a
  rotated page, `FALSE` forbids one, and the default – `NA` – measures
  the table and decides. **This changes existing behavior**: in 2.0.8
  [`apa.cor.table()`](https://dstanley4.github.io/apaTables/reference/apa.cor.table.md),
  [`apa.d.table()`](https://dstanley4.github.io/apaTables/reference/apa.d.table.md)
  and
  [`apa.2way.table()`](https://dstanley4.github.io/apaTables/reference/apa.2way.table.md)
  defaulted to `landscape = TRUE` and
  [`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  always wrote a landscape page, so those tables were rotated whether or
  not they needed it. Nothing rotates now unless it does not fit. Pass
  `landscape = TRUE` for the old orientation.
- [`apa.afex.table()`](https://dstanley4.github.io/apaTables/reference/apa.afex.table.md)
  is new: an APA ANOVA table from
  [`afex::aov_ez()`](https://rdrr.io/pkg/afex/man/aov_car.html) output,
  covering the repeated-measures and mixed designs `apa.ezANOVA.table()`
  used to serve. Its `table.title` argument is honored in every
  destination, and “ANOVA Results” is the title when none is given.
- `.html` output from
  [`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
  carries the same APA elements as the Word file: the table’s caption
  holds the “Table N” line and the italic title, and the note sits below
  the table, with its italics and super/subscripts intact. Table numbers
  are assigned exactly as for `.docx`, so the same tables saved to both
  formats carry the same numbers. Writing `.html` needs pandoc; without
  it
  [`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
  stops with a message that says so and points to `.docx`, which does
  not need pandoc.
- Tables are left-aligned so each shares the same left margin as its
  number, title, and note. Correlation and d-value tables add a blank
  line of separation between variable rows for readability.
- Word output follows APA 7 table setup: the table number is bold, the
  title sits one double-spaced line below it, and a blank line separates
  the table from its note. The note preserves italics and
  super/subscripts (e.g., *M*, *SD*, eta-squared symbols), and the
  significance-star sentence (“\* indicates p \< .05. \*\* indicates p
  \< .01.”) starts on a new line. Regression Fit cells render R-squared
  and change-in-R-squared with proper italics, superscripts, and the
  Delta symbol. Table pages use 0.5 in side margins, and a table wider
  than the text area is scaled to fit it rather than clipped by Word.
- Notes keep their styling in every destination – italic *M*, subscripts
  and superscripts, and a multi-part note that starts each part, such as
  the significance-star sentence, on a new line.

### Deprecations and dependencies

- **Breaking:** `apa.ezANOVA.table()` has been removed. The `ez` package
  it depended on was archived on CRAN; use
  [`apa.afex.table()`](https://dstanley4.github.io/apaTables/reference/apa.afex.table.md)
  with [`afex::aov_ez()`](https://rdrr.io/pkg/afex/man/aov_car.html)
  output for repeated-measures and mixed designs instead.

- **Breaking:** `apa.reg.boot.table()` has been removed. Bootstrapped
  regression tables are not part of this release; use
  [`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  for the standard regression table.

- `apa.knit.table.for.pdf()` and `apa.knit.table.for.papaja()`, which
  appeared only in the unreleased 3.0 versions below, are not in this
  release;
  [`apa.quarto.pdf()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md)
  in an apaquarto document and
  [`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
  replace them.

- Correlation tables print each correlation’s *p* value on a line below
  its confidence interval (`show.pvalue = FALSE` gives the 2.0.8
  layout), and gain an *N* column. When the data have missing values,
  the note also gives the range of the number of cases behind each
  correlation (pairwise deletion).

- d-value and one-way tables gain an *n* column: the number of cases in
  each group.

- Settings are checked, and a value that is not allowed stops with a
  message naming the values that are: `conf.level` in
  [`apa.aov.table()`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md)
  and `prop.var.conf.level` in
  [`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  must be `.90` or `.95` (2.0.8 silently used `.90` and `.95`,
  respectively, in place of any other value); `landscape` must be
  `TRUE`, `FALSE` or `NA`; and `table.number` must be a whole number, 0
  or more.

- [`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  and
  [`apa.aov.table()`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md)
  stop with a message for a model with an offset (an
  [`offset()`](https://rdrr.io/r/stats/offset.html) term, or the
  `offset` argument of [`lm()`](https://rdrr.io/r/stats/lm.html)).
  2.0.8’s
  [`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  printed the offset as a predictor row with no estimate, and R-squared
  is then not the usual proportion of variance explained;
  [`apa.aov.table()`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md)
  refuses such models too.

- The object a table function returns uses the same field names for
  every table type: `table_number`, `table_title`, `table_body` and
  `table_note`. In 2.0.8, correlation, d-value, one-way and two-way
  tables used `table.number`, `table.title`, `table.body` and
  `table.note` and the class `apa.table`; the class is now `apa_table`
  throughout. A regression table’s `sr2` and `sr2_95%_CI` columns are
  now `Unique_R2` and `Unique_95%_CI`. Titles and notes are generated in
  APA title case.

- A regression table labels each predictor row with the term as
  [`summary()`](https://rdrr.io/r/base/summary.html) prints it, with
  three changes: a product such as `I(wt * qsec)` reads `wt x qsec`
  (2.0.8 printed `I(wt * qsec)`), a term such as `I(wt^2)` loses the
  wrapper (`wt^2`), and names have no backticks. A factor level prints
  as R gives it (`agegrp26-35`).

- Dependencies have changed. R 4.1.0 or later is required (2.0.8 needed
  R 3.1.2). `boot`, `tibble` and `methods` are no longer imported
  (`boot` and `tibble` are unused). `flextable` and `officer` are new
  imports, used by
  [`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
  to write the Word and HTML files, and `knitr`, which 2.0.8 suggested,
  is now imported. `MBESS` stays a required import, as in 2.0.8: it
  computes the confidence intervals for d values, R-squared and partial
  eta-squared, so every installation of apaTables prints them.

- On a Mac without XQuartz, some builds of `gdtools` (a package
  `flextable` needs) cannot load – Posit Package Manager’s can fail this
  way; CRAN’s do not.
  [`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md)
  and the Quarto entry points now stop with a message that says how to
  fix it (install XQuartz, or reinstall `gdtools` from CRAN) instead of
  a low-level library-loading error.

### Bug fixes

- [`apa.aov.table()`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md)
  now gives correct Type III main effects whatever contrasts the model
  was fitted with. Under R’s default treatment contrasts, a factor’s row
  in a model with an interaction was the effect at one level of the
  other factor rather than its main effect, and nothing said so (for the
  `goggles` example, alcohol printed as F = 0.61, p = .546 instead of F
  = 20.07, p \< .001). For `type = 3`,
  [`apa.aov.table()`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md)
  now refits the model with sum-to-zero contrasts, as SPSS does, so the
  table is the same whether or not you set
  `options(contrasts = c("contr.sum", "contr.poly"))`. The refit uses
  the data stored in the model object, so the table always describes the
  model you fitted, even if the data in your workspace has changed
  since. The (Intercept) row printed at the console now tests the grand
  mean in every case.
- `apa.aov.table(type = 2)` left the confidence interval for the first
  effect blank.
- [`apa.aov.table()`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md)
  rounded the sums of squares and F to two decimals before computing the
  mean squares, partial eta-squared and its confidence interval. For a
  dependent variable on a very small scale (such as seconds divided
  by 1000) every sum of squares rounded to zero and partial eta-squared
  printed as `NaN`. On a proportion scale partial eta-squared could be
  .01 off with nothing to show it, and on any scale a mean square or a
  confidence-interval limit could be .01 off. Everything is now computed
  at full precision and rounded only when printed.
- [`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  accepted blocks that share no predictor, and printed a negative change
  in R-squared with a `[NaN, NaN]` interval under a hierarchical title.
  Each block must now contain every term of the block before it,
  interactions included, plus at least one more; a step written as a new
  parameterization, such as `wt` followed by `poly(wt, 2)`, is still
  accepted. Blocks must also be fitted to the same cases in the same
  order, with the same weights. Blocks fitted to different rows (usually
  because a later predictor has missing values) now stop with a message
  that shows how to fit every block to the same rows, instead of an
  error from inside base R.
- [`apa.d.table()`](https://dstanley4.github.io/apaTables/reference/apa.d.table.md)
  reports a d-value for every pair of groups. 2.0.8 left the d-value and
  the interval for it blank when Levene’s test (centered on the median)
  gave p \< .05, and the help page did not say a test made the decision.
  Every d-value now uses the pooled standard deviation, no test of equal
  variances is run, and the note says that the d-values use the pooled
  standard deviation. The note also says that d-values are absolute, so
  the direction of a difference is read from the means.
- [`apa.2way.table()`](https://dstanley4.github.io/apaTables/reference/apa.2way.table.md)
  leaves out a case with a missing value on either factor or on the
  dependent variable from the marginal means, as it already did from the
  cells. 2.0.8 counted a case missing one factor in the marginal mean of
  the other, so a marginal mean could differ from the cells beside it.
- [`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  leaves out the *r* column for a weighted model, as 2.0.8 already left
  out beta. 2.0.8 printed the unweighted correlation there, beside the
  weighted *b*, semi-partial correlation squared and R-squared. A table
  for weighted models no longer stops with an R error when a later block
  adds a factor predictor or an interaction written `a * b` or `a:b`.
- [`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  shows the beta and *r* columns when R writes a predictor name in
  backticks, as it does for a name with a space or a hyphen
  (`` `pre-test` ``). 2.0.8 left both columns out of the whole table for
  such a name without saying why; in a table with several blocks, the
  note still defined them. The name now prints without the backticks. A
  variable with `*` or `:` in the name is no longer taken for a product
  term, so the *r* for it is shown.
- [`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  no longer stops with an R error (“numbers of columns of arguments do
  not match”) when an earlier block cannot show beta and *r* and a later
  block can – for example, an interaction written `wt * qsec` in block 1
  and `I(wt * qsec)` in block 2. As the help page says, beta and *r* are
  then left out of every block.
- [`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  shows an unordered categorical predictor with each level compared with
  the first level even when
  `options(contrasts = c("contr.sum", "contr.poly"))` was set before the
  model was fitted, as is common for an ANOVA. 2.0.8 used the
  sum-to-zero coding that line sets: the rows read `factor(cyl)1`,
  `factor(cyl)2`, and each b compared one group with the mean of the
  group means, with nothing in the table to say so. In
  `lm(mpg ~ factor(cyl) + wt, data = mtcars)`, 6 cylinders printed as
  -0.81 (not significant) instead of -4.26 (*p* \< .01). Coding chosen
  for one model is kept when it is dummy coding (such as `contr.SAS`);
  other coding, such as effect coding, stops with a message.
- A *p* value of exactly .01 is now marked with one significance star.
  In 2.0.8 it was unmarked; one star was given only for .01 \< p \< .05.
- The confidence intervals for the semi-partial correlation squared of
  each predictor and for the change in R-squared are held to the range 0
  to 1. The asymptotic method used for them could print a negative lower
  limit for a proportion of variance.
- The two-way table note no longer says that the marginal means pertain
  to main effects. They are the observed means at each level of a
  factor; in an unbalanced design these differ from the unweighted means
  a Type III test compares.
- Building a d-value table no longer disables warnings for the rest of
  the session.
  [`MBESS::ci.smd()`](https://rdrr.io/pkg/MBESS/man/ci.smd.html) sets
  `options(warn = -1)` and does not restore it, so after one
  [`apa.d.table()`](https://dstanley4.github.io/apaTables/reference/apa.d.table.md)
  call every later warning – from any package – was discarded; inside a
  Quarto render that included the apaTables too-wide-table warning.
  apaTables now saves and restores the option around each call to MBESS.
- Common input mistakes now stop with a message that says what to
  change, instead of an error from inside R: a `dv` that is not numeric;
  a group, or a cell of a two-way table, with fewer than two cases (the
  message names it); a correlation table with fewer than two numeric
  columns, or a column with only one value; a model from
  [`glm()`](https://rdrr.io/r/stats/glm.html) given to
  [`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md)
  or
  [`apa.aov.table()`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md),
  or no model given to
  [`apa.reg.table()`](https://dstanley4.github.io/apaTables/reference/apa.reg.table.md);
  a regression model with no predictor or no intercept; a show/hide
  argument that is not `TRUE` or `FALSE`; and a `filename` that is not
  one name. `filename = NULL` now means no file, as `NA` does.
- A grouping variable with unused factor levels, as is common after
  [`subset()`](https://rdrr.io/r/base/subset.html), now gives a table:
  the unused levels are left out. 2.0.8 stopped with an error.
- Printing a table returns the table invisibly, as
  [`print()`](https://rdrr.io/r/base/print.html) methods do, instead of
  `NULL`.

## apaTables 3.0

### New features

- Support for latex tables via the new command apa.knit.table.for.pdf()
