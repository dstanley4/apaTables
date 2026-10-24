# Troubleshooting

Each heading below is what you would say out loud, not what is
technically happening. If your symptom is here, the fix is under it.

## “Where did my tables go? They are all at the end.”

That is not a fault, and it is the most common first surprise. An APA
manuscript collects the tables at the end, after the reference list, and
apaquarto does that by default because that is the format journals ask
for.

If you would rather see each table where you wrote it – which is usually
easier while you are drafting – add one line to the settings block at
the top of your document:

    format:
      apaquarto-pdf:
        floatsintext: true

Both are correct APA output. Nothing else about your document changes.

## “One of my tables is on a sideways page by itself.”

This is expected behavior for apaTables. A table too wide to stand
upright is placed on a rotated (landscape) page, and a rotated page
cannot also carry ordinary paragraphs, so that table gets a page to
itself. apaTables measures each table as the document renders and only
rotates the ones that need it.

If you want to overrule that decision, every table function takes a
`landscape` argument: `landscape = FALSE` forbids a rotated page, and
`landscape = TRUE` insists on one.

## “My table has no number, no title, and no note.”

**The most likely cause is a missing label.** The code block for a table
needs a settings line that starts with `tbl-`:

    #| label: tbl-correlations

Without it, Quarto does not treat the block as a table at all. There is
then nothing to number, nothing to attach a title to, and no note – no
matter what else you write in the block. The label is also what lets you
write `@tbl-correlations` in a sentence and have the number filled in.

**The other cause is that the document is not using apaquarto**, or is
using the standard copy of it rather than the one apaTables ships. The
automatic title and note come from that patched copy. Run this once in
the project folder:

``` r

apa.quarto.setup()
```

It installs or repairs the extension, and does nothing at all if the
project is already correct.

## “A table I typed in markdown is at the end with no number or title.”

With tables collected at the end of the manuscript (the apaquarto
default), every table moves to the end pages, including one typed in
markdown. A markdown table with no caption has no number and no title,
so on the end pages nothing ties it back to the text that introduced it.

Give every table a caption. For a markdown table, put the caption on a
separate line after the table, starting with a colon and ending with a
label:

    | Group   | M    | SD   |
    |---------|------|------|
    | Control | 4.20 | 1.10 |
    | Treated | 5.30 | 0.90 |

    : Mean Scores by Group {#tbl-means}

The table is then numbered and titled like any other, and `@tbl-means`
in a sentence prints the number.

## “I set the title myself and nothing happened.”

Three things can supply a title, and they have a fixed order of
precedence: a `#| tbl-cap:` settings line wins over a `title =`
argument, which wins over the title apaTables generates. So if a
`tbl-cap` line is present, a `title =` argument is ignored – by design.

The same order applies to `#| apa-note:` and `note =`.

If you did not mean to override anything, delete the line and the
argument and let apaTables write both.

## “My tables looked right before and now they do not.”

**Did you run `quarto add wjschne/apaquarto` or `quarto update`?**
Either one replaces the patched copy of the extension that apaTables
installed with the standard one – while printing `[No Change]`, which is
why it is easy to miss. Put the tested copy back:

``` r

apa.quarto.setup()
```

**Did you send someone just the `.qmd` file?** The extension lives in an
`_extensions` folder next to the document. Send the whole folder and
there is nothing for the recipient to do; send only the file and they
need to run
[`apa.quarto.setup()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)
once before rendering.

**Does your document cache the chunks** (`cache: true`)? Quarto then
reuses the earlier output of a chunk instead of running it again. After
you update apaTables, a table from a cached chunk in a Word document can
come out in larger type with wider spacing than apaTables gives it.
Render once with the cache refreshed, by typing this in a terminal (not
the R console):

    quarto render mydocument.qmd --cache-refresh

Deleting the `mydocument_cache` folder beside the document does the
same.

## “The numbers in my two-way ANOVA look wrong.”

**If
[`apa.aov.table()`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md)
disagrees with output you produced yourself, check the contrasts.** A
Type III two-way ANOVA needs contrasts that sum to zero. Under the R
default contrasts, the row for each factor is the effect at one level of
the other factor, not the main effect, and nothing in the output says
so.

[`apa.aov.table()`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md)
takes care of this itself: for a Type III table it uses sum-to-zero
contrasts whether or not you set them, so the F values in the table
match programs such as SPSS. Your own analysis, for example with
[`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html), does not. Set
the contrasts before fitting the model so both use the same coding:

``` r

options(contrasts = c("contr.sum", "contr.poly"))
```

The examples on the
[`apa.aov.table()`](https://dstanley4.github.io/apaTables/reference/apa.aov.table.md)
help page set this line before every model.

## “A correlation with p = .010 has two stars.”

**The stars come from the exact p-value; the printed p-value is rounded
to three decimals.** When the exact value is just below .01, rounding
prints it as .010, beside two stars. In the same way, a value just below
.05 prints as .050, beside one star. The stars are correct: the exact
p-value is below the cutoff that the note gives.

For example, in this table the correlation between `hp` and `drat`
prints as `-.45**` with `p = .010` under it:

``` r

apa.cor.table(data = mtcars[, c("mpg", "hp", "drat")])
```

The exact p-value is .009989, which
[`cor.test()`](https://rdrr.io/r/stats/cor.test.html) shows:

``` r

cor.test(x = mtcars$hp, y = mtcars$drat)
```

This happens only for a p-value from .0095 up to .01, or from .0495 up
to .05. If a reader or reviewer asks about it, report the exact p-value
from [`cor.test()`](https://rdrr.io/r/stats/cor.test.html) in the text.

## “My table is missing rows, the note has vanished, or the PDF will not render.”

Are you rendering with `documentmode: jou`, the two-column journal
layout of apaquarto? Tables are not supported there. A two-column page
cannot break a table across pages, so rows, columns and whole notes can
go missing. Or the render can stop with a LaTeX error and write no PDF
at all; the error reads `longtable not in 1-column mode` or
`Not in outer par mode`. When the PDF does render, apaTables puts a
warning in it, but there is no setting that makes it safe – render with
`documentmode: man` instead, which is the mode a manuscript is submitted
in, or save the table as a Word file with
[`apa.save()`](https://dstanley4.github.io/apaTables/reference/apa.save.md).

In the other modes (`man`, `stu` and `doc`) a long table – an apaTables
table or one written in markdown – continues onto the next page with
every row, and the note prints under the last row. If rows are missing
there, the copy of apaquarto in the project has probably been replaced
by the standard one (running `quarto add` or `quarto update extension`
does that). Run
[`apa.quarto.setup()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)
in the project to put the tested copy back, then render again.

## “In my PDF, a long word in a markdown table runs into the next column.”

This happens when a row of a markdown table is longer than 72
characters, in any Quarto PDF, with or without apaquarto. pandoc, the
program Quarto uses to convert the document, then sets the column widths
in proportion to the number of dashes under each heading, and a word
longer than the column prints over the next one. Two fixes: add dashes
under the heading of the column that is too narrow, so that column gets
a larger share of the width, or keep every row of the table under 72
characters, so that each column is as wide as the widest entry.

## “In my PDF, a kable() table has two table numbers, or the note sits on top of the last row.”

This happens with a table made with `knitr::kable(format = "latex")` –
written that way or inside kableExtra’s `kable_styling()` – when the
title is given with `kable(caption = ...)` in a chunk labeled `tbl-`.
The table gets an extra, empty table number, every later table is
numbered one too high, and a note from an `apa-note` settings line
prints over the last row. With a `#| tbl-cap:` line as well, the extra
number prints over the title. A plain `knitr::kable(scores)`, without
`format`, given both `caption = ...` and a `#| tbl-cap:` line, prints
the table number and title twice.

In every case the fix is to give the title with a `#| tbl-cap:` settings
line and leave `caption` out of the call. The settings lines and the
call look like this:

    #| label: tbl-scores
    #| tbl-cap: "Mean Scores by Group"
    knitr::kable(scores, format = "latex", booktabs = TRUE)

`format = "latex"` is for PDF documents only. In a Word document it
leaves the title with no table under it, so in Word or HTML leave
`format` out.

The kableExtra option `kable_styling(latex_options = "scale_down")` also
puts the note over the last row in a PDF manuscript. If the table fits
the page without that option, leave it out.

## “In my PDF, a long kable() table repeats the first row at the top of the next page.”

This happens with a table made with
`knitr::kable(format = "latex", col.names = NULL)` – no column headings
– and without `booktabs = TRUE`. A table continued onto a new page
repeats the headings, and with no headings to repeat, the first row is
taken for them. Add `booktabs = TRUE`, which is also the APA-style
layout:

    knitr::kable(scores, format = "latex", booktabs = TRUE, col.names = NULL)

`format = "latex"` is for PDF documents only; in Word or HTML leave it
out.

## “R says it cannot find `apa.ezANOVA.table()`.”

It was removed in version 4.0.0, because the `ez` package it depended on
was archived by CRAN. Use
[`apa.afex.table()`](https://dstanley4.github.io/apaTables/reference/apa.afex.table.md)
with output from
[`afex::aov_ez()`](https://rdrr.io/pkg/afex/man/aov_car.html) instead,
which covers the same repeated-measures and mixed designs.

## “R says it cannot find `apa.cor.table()` when I render.”

Every code block in a Quarto document runs in a fresh R session that
knows nothing about your console. Add
[`library(apaTables)`](https://github.com/dstanley4/apaTables) to a
block near the top of the document.

## “On my Mac, apaTables says XQuartz is missing.”

You may see this message:

    apaTables could not load the flextable package, which it uses to measure
    and save tables, because your Mac is missing XQuartz, ...

or, from an older version or another package, an error that ends with
something like `Library not loaded: /opt/X11/lib/libXrender.1.dylib`.

Some R packages that apaTables relies on are built, on some Macs, to use
XQuartz, a free add-on that gives a Mac the X11 graphics libraries that
parts of R have long expected. Whether you need it depends on where your
R packages come from: packages installed from CRAN, which is what R,
RStudio and Positron use unless they have been set up otherwise, do not
need it. Packages from Posit Package Manager, which some universities
and some setups use, can.

**The fix that lasts:** install XQuartz from
[xquartz.org](https://www.xquartz.org), then log out of your Mac, log
back in, and restart R (or RStudio, or Positron). Nothing else needs to
change, and the problem will not come back.

**To keep working right now**, run this in the R console:

``` r

install.packages("gdtools", repos = "https://cloud.r-project.org")
```

Then restart R. This replaces the one package that needs XQuartz with
the CRAN version, which does not. It works at once, but if your R is set
up to install from Posit Package Manager, a later package update may
bring the problem back – installing XQuartz is what stops that.
