# Start a new APA manuscript that renders on the first try

One command gives you a working APA manuscript:

    apa.quarto.new("my-paper")

This creates a `my-paper` folder holding `my-paper.qmd` – a Quarto
document with a working APA header and one worked table – and, beside
it, an `_extensions` folder holding the extension that makes the
rendered document follow APA style. Open `my-paper.qmd` and click Render
(the Preview button in Positron), or run `quarto render` in that folder:
the very first render produces a complete APA PDF. The starter is set up
for PDF; for a Word manuscript, change its `format: apaquarto-pdf` line
to `format: apaquarto-docx`.

**What gets installed is not the standard apaquarto release.** It is
apaTables' own tested copy: apaquarto plus table fixes that are not yet
in the standard release. So always install it with this function (or
[`apa.quarto.setup`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)
for a folder you already have), never with Quarto's own `quarto add`,
which gets the standard release instead.

The `_extensions` folder is the part you cannot write by hand: a
document's settings block only names a format, and the folder is what
supplies it. Without it the document does not render at all – Quarto
stops with `Unable to read the extension 'apaquarto'` before any R code
runs, so apaTables never gets the chance to install it for you.

## Usage

``` r
apa.quarto.new(path, filename = NULL, overwrite = FALSE)
```

## Arguments

- path:

  Folder to create. It is created if it does not exist.

- filename:

  Name for the document. Defaults to the folder's own name with a `.qmd`
  extension.

- overwrite:

  Overwrite an existing document of that name?

## Value

Invisibly, a list with `path`, `document` and `setup` (what
[`apa.quarto.setup`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)
did).

## Quarto documents

Quarto (<https://quarto.org>) is a free system for writing documents
that combine text and R code. apaquarto is a Quarto extension that makes
such a document follow APA style – tables get their number, italic
title, and *Note.* line automatically, in Word, PDF, or HTML. In the R
console, type:

    library(apaTables)
    apa.quarto.new("my-paper")

This creates a folder called `my-paper` containing `my- paper.qmd`. The
document's YAML header (the settings block between `---` lines at the
top) already has everything apaquarto needs to render: a title, an
author with `corresponding: true` and an affiliation (apaquarto stops
with a LaTeX error if the author is missing or has neither), and
`format: apaquarto-pdf` –

    ---
    title: "My Paper"
    shorttitle: "My Paper"
    author:
      - name: Your Name
        corresponding: true
        email: you@example.org
        affiliations:
          - name: Your University
    format: apaquarto-pdf
    ---

plus one worked apaTables table already in the body. Beside the
document, `apa.quarto.new()` also installs an
`_extensions/wjschne/apaquarto` folder (via
[`apa.quarto.setup`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)),
so opening `my-paper/my-paper.qmd` and clicking Render – or running
`quarto render` in that folder – produces a complete APA PDF on the
first try, with no terminal command to install anything first.

Do **not** run `quarto add wjschne/apaquarto` or
`quarto update extension wjschne/apaquarto` in the folder this creates:
both commands silently replace the patched extension with the plain,
unpatched one, while reporting `[No Change]`. That reintroduces bugs in
table numbering and in where a long table's note lands. If you have
already run one of those commands, running
[`apa.quarto.setup`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)`()`
again puts the tested copy back.

Sending a coauthor or reviewer the whole project folder carries
`_extensions/` with it, so they do nothing extra. Sending only the
`.qmd` file does not: they must run
[`apa.quarto.setup()`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)
in that folder themselves before rendering, because with the extension
folder absent, Quarto fails before any R code runs.

## See also

A step-by-step guide to placing apaTables tables in Quarto documents
comes installed with the package – see its "Installing the apaquarto
extension" section. To open it, type:

    vignette("apaquarto-usage", package = "apaTables")

Related functions:
[`apa.quarto.setup`](https://dstanley4.github.io/apaTables/reference/apa.quarto.setup.md)
to install or repair the extension in a project you already have,
[`apa.quarto.example`](https://dstanley4.github.io/apaTables/reference/apa.quarto.example.md).

## Examples

``` r
# Start a new manuscript in a temporary folder
proj <- file.path(tempdir(), "apaTables-quarto-new-example")
apa.quarto.new(proj)
#> Installed the apaquarto extension apaTables ships (version 5.0.18+apaTables.26) at _extensions/wjschne/apaquarto.
#> apaTables created /tmp/RtmpogTn6g/apaTables-quarto-new-example/apaTables-quarto-new-example.qmd; open it and click Render, or run quarto render in this folder.

# (delete the demo folder)
unlink(proj, recursive = TRUE)
```
