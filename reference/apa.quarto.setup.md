# Install or repair this project's apaquarto extension

You already have a project folder, and you want apaTables tables to come
out APA-formatted when its documents render. Run this once, in the R
console, from that folder:

    apa.quarto.setup()

It puts the apaquarto extension into an `_extensions` folder beside your
document. **What gets installed is not the standard apaquarto release**:
it is apaTables' own tested copy – apaquarto plus table fixes that are
not yet in the standard release. That is also why this command exists at
all. Running Quarto's own `quarto add` or `quarto update` in a project
silently replaces the tested copy with the standard one – while
reporting `[No Change]` – which brings back bugs in table numbering and
in where a long table's note lands. Running `apa.quarto.setup()` again
puts the tested copy back.

It is safe to run at any time: on a project that is already correct it
does nothing and says nothing. When it does repair something during a
render, the report reaches the render log rather than the manuscript
itself, so a repaired document still renders with nothing extra typeset
into it.

## Usage

``` r
apa.quarto.setup(path = ".", auto = TRUE, force = FALSE)
```

## Arguments

- path:

  Project folder. Defaults to the current directory, or, during a
  render, the folder whose `_extensions` the render is using.

- auto:

  Replace a copy that is not ours (the standard apaquarto, or one
  restored by `quarto update`)? Defaults to `TRUE`.

- force:

  Overwrite a copy that IS ours but has been edited locally? Defaults to
  `FALSE`, so your own edits survive.

## Value

Invisibly, a list with `action`, `version`, `found` and `path`.

## Quarto documents

Quarto (<https://quarto.org>) is a free system for writing documents
that combine text and R code. apaquarto is a Quarto extension that makes
such a document follow APA style: apaTables tables placed in it appear
as fully formatted APA tables – number, title, and note included – in
Word, PDF, or HTML. apaquarto is installed per project folder, not into
R itself, which is what `apa.quarto.setup()` does. Call it once, in the
R console, from the folder that contains – or will contain – your `.qmd`
document:

    library(apaTables)
    apa.quarto.setup()

This puts an `_extensions/wjschne/apaquarto` folder next to your
document, and its YAML header – the settings block between `---` lines
at the very top of the document – can then select an apaquarto format.
apaquarto needs an author in that header – with no author, or an empty
one, the render stops with a LaTeX error – and it reports a problem
unless that author has an affiliation and is marked
`corresponding: true`. This header is enough to render:

    ---
    title: "My Paper"
    author:
      - name: Jane Doe
        corresponding: true
        affiliations:
          - name: University of Guelph
    format: apaquarto-pdf
    ---

`apa.quarto.setup()` is safe to run at any time – on a project whose
copy is already correct, it does nothing and says nothing. Do **not**
run `quarto add wjschne/apaquarto` or
`quarto update extension wjschne/apaquarto` in one of these folders:
both commands silently replace the patched copy with the plain,
unpatched extension, while reporting `[No Change]`. That reintroduces
bugs in table numbering and in where a long table's note lands. If you
have already run one of those commands, running `apa.quarto.setup()`
again puts the tested copy back.

Sending a coauthor or reviewer the whole project folder carries
`_extensions/` with it, so they do nothing extra. Sending only the
`.qmd` file does not: they must run `apa.quarto.setup()` in that folder
themselves before rendering, because with the extension folder absent,
Quarto fails before any R code runs, so nothing can repair itself
automatically.

## See also

A step-by-step guide to placing apaTables tables in Quarto documents
comes installed with the package – see its "Installing the apaquarto
extension" section. To open it, type:

    vignette("apaquarto-usage", package = "apaTables")

Related functions:
[`apa.quarto.new`](https://dstanley4.github.io/apaTables/reference/apa.quarto.new.md)
to start a whole new manuscript in one command,
[`apa.quarto.template`](https://dstanley4.github.io/apaTables/reference/apa.quarto.template.md),
[`apa.quarto.example`](https://dstanley4.github.io/apaTables/reference/apa.quarto.example.md).

## Examples

``` r
# Install the apaquarto extension into a temporary project folder
proj <- file.path(tempdir(), "apaTables-quarto-setup-example")
dir.create(proj, showWarnings = FALSE)
apa.quarto.setup(proj)
#> Installed the apaquarto extension apaTables ships (version 5.0.18+apaTables.26) at _extensions/wjschne/apaquarto.

# Running it again on the same, already-correct folder does nothing
apa.quarto.setup(proj)

# (delete the demo folder)
unlink(proj, recursive = TRUE)
```
