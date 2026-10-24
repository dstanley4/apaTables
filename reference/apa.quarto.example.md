# Write two ready-to-render example documents showing every apaTables table

The fastest way to learn the Quarto workflow is to see it working.
`apa.quarto.example()` writes two complete example manuscripts into your
working directory, with everything they need already installed beside
them:

    apa.quarto.example(filename = "apaTables-examples.qmd")

Two files appear:

    apaTables-examples-pdf.qmd
    apaTables-examples-docx.qmd

Both show every apaTables table type, one table per page, each with an
APA number, title, and note. They are the same document set up for
different destinations: render the `-pdf` file and you get a PDF
manuscript (this needs a LaTeX engine such as TinyTeX); render the
`-docx` file and you get a Word manuscript (no LaTeX needed). Render
with the Preview button in Positron, or in a terminal:

    quarto render apaTables-examples-docx.qmd

Then read the finished manuscript side by side with the `.qmd` source
file. Every table in the output was made by a short code chunk you can
find in the source, copy, and adapt to your own data – that comparison
is the whole lesson. Along the way you will see two tables arrive on
rotated (landscape) pages: nothing in the source asks for this.
apaTables measures each table as the document renders and rotates the
ones too wide to stand upright.

One page in each document (the repeated-measures ANOVA) uses the afex
and tidyr packages, which do not come with apaTables. Install them once
with `install.packages(c("afex", "tidyr"))`, or the render stops on that
page. The same instruction is repeated in a comment at the top of each
written file.

The apaquarto extension – the part that makes a rendered document follow
APA style – is installed in an `_extensions` folder beside the two files
automatically, which is why they render with no other setup.

## Usage

``` r
apa.quarto.example(filename = "apaTables-examples.qmd", overwrite = FALSE)
```

## Arguments

- filename:

  Path used as the stem for both files. A `.qmd` extension, if present,
  is stripped before deriving the two output names:
  `apa.quarto.example(filename = "apaTables-examples.qmd")` writes
  `apaTables-examples-pdf.qmd` and `apaTables-examples-docx.qmd`. A bare
  name (the default) is written in the current working directory (the
  folder R is currently using); an absolute or `~` path is written
  wherever it points. Alongside the two files, this also creates (or
  repairs, via
  [`apa.quarto.setup`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md))
  an `_extensions/wjschne/apaquarto` folder in the same directory, so
  both files render without any extra setup.

- overwrite:

  Logical. When `FALSE` (default) the function stops if either output
  file already exists; set `TRUE` to replace existing files.

## Value

A named character vector `c(pdf = ..., docx = ...)` with the paths to
the two written files, invisibly. Called for the side effects: it also
writes (or repairs) an `_extensions/wjschne/apaquarto` folder beside
them, as described under `filename`.

## See also

[`apa.quarto.new`](https://dstanley4.github.io/apaTables/reference/apa.quarto.new.md)`(path = "my-paper")`
starts an APA manuscript with the apaquarto extension already installed;
[`apa.quarto.setup()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)
installs the extension into a project you already have. A step-by-step
guide to placing apaTables tables in Quarto documents also comes
installed with the package. To open it, type:

    vignette("apaquarto-usage", package = "apaTables")

Related functions:
[`apa.quarto.pdf`](https://dstanley4.github.io/apaTables/reference/apa.quarto.pdf.md),
[`apa.quarto.template`](https://dstanley4.github.io/apaTables/reference/apa.quarto.template.md).

## Examples

``` r
# Write both example documents to a temporary folder
out <- file.path(tempdir(), "apaTables-examples.qmd")
apa.quarto.example(filename = out)
#> Installed the apaquarto extension apaTables ships (version 5.0.18+apaTables.36) at _extensions/wjschne/apaquarto.
#> Wrote two example documents (the apaquarto extension is already installed alongside them):
#>   /tmp/RtmpDLB3cf/apaTables-examples-pdf.qmd  (renders to PDF; needs a LaTeX engine such as TinyTeX)
#>   /tmp/RtmpDLB3cf/apaTables-examples-docx.qmd  (renders to Word; no LaTeX needed)
#> The repeated-measures page in each needs the afex and tidyr packages: install.packages(c("afex", "tidyr"))

# (delete the demo files, and the extension folder installed beside them)
unlink(file.path(tempdir(),
                  c("apaTables-examples-pdf.qmd",
                    "apaTables-examples-docx.qmd")))
unlink(file.path(tempdir(), "_extensions"), recursive = TRUE)
```
