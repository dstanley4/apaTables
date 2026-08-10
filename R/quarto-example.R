#' Write two ready-to-render example documents showing every apaTables table
#'
#' @description
#' The fastest way to learn the Quarto workflow is to see it working.
#' \code{apa.quarto.example()} writes two complete example manuscripts into
#' your working directory, with everything they need already installed
#' beside them:
#'
#' \preformatted{apa.quarto.example("apaTables-examples.qmd")}
#'
#' Two files appear:
#'
#' \preformatted{apaTables-examples-pdf.qmd
#' apaTables-examples-docx.qmd}
#'
#' Both show every apaTables table type, one table per page, each with its
#' APA number, title, and note. They are the same document set up for
#' different destinations: render the \code{-pdf} file and you get a PDF
#' manuscript (this needs a LaTeX engine such as TinyTeX); render the
#' \code{-docx} file and you get a Word manuscript (no LaTeX needed).
#' Render with the Preview button in Positron, or in a terminal:
#'
#' \preformatted{quarto render apaTables-examples-docx.qmd}
#'
#' Then read the finished manuscript side by side with its \code{.qmd}
#' source file. Every table in the output was made by a short code chunk
#' you can find in the source, copy, and adapt to your own data -- that
#' comparison is the whole lesson. Along the way you will see two tables
#' arrive on rotated (landscape) pages: nothing in the source asks for
#' this. apaTables measures each table as the document renders and rotates
#' the ones too wide to stand upright.
#'
#' One page in each document (the repeated-measures ANOVA) uses the afex
#' and tidyr packages, which do not come with apaTables. Install them once
#' with \code{install.packages(c("afex", "tidyr"))}, or the render stops on
#' that page. The same instruction is repeated in a comment at the top of
#' each written file.
#'
#' The apaquarto extension -- the part that makes a rendered document
#' follow APA style -- is installed in an \code{_extensions} folder beside
#' the two files automatically, which is why they render with no other
#' setup.
#'
#' @param filename Path used as the stem for both files. A \code{.qmd}
#'   extension, if present, is stripped before deriving the two output names:
#'   \code{apa.quarto.example("apaTables-examples.qmd")} writes
#'   \code{apaTables-examples-pdf.qmd} and
#'   \code{apaTables-examples-docx.qmd}. A bare name (the
#'   default) is written in the current working directory (the folder R is
#'   currently using); an absolute or \code{~} path is written wherever it
#'   points. Alongside the two files, this also creates (or repairs, via
#'   \code{\link{apa.quarto.setup}}) an \code{_extensions/wjschne/apaquarto}
#'   folder in the same directory, so both files render without any extra
#'   setup.
#' @param overwrite Logical. When \code{FALSE} (default) the function stops if
#'   either output file already exists; set \code{TRUE} to replace existing
#'   files.
#' @return A named character vector \code{c(pdf = ..., docx = ...)} with
#'   the paths to the two written files, invisibly. Called for its side
#'   effects: it also writes (or repairs) an \code{_extensions/wjschne/apaquarto}
#'   folder beside them, as described under \code{filename}.
#' @seealso
#' \code{\link{apa.quarto.new}("my-paper")} starts an APA manuscript with the
#' apaquarto extension already installed; \code{\link{apa.quarto.setup}()}
#' installs the extension into a project you already have. A step-by-step
#' guide to placing apaTables tables in Quarto documents also comes
#' installed with the package. To open it, type:
#'
#' \preformatted{vignette("apaquarto-usage", package = "apaTables")}
#'
#' Related functions: \code{\link{apa.quarto.pdf}},
#' \code{\link{apa.quarto.template}}.
#' @examples
#' # Write both example documents to a temporary folder
#' out <- file.path(tempdir(), "apaTables-examples.qmd")
#' apa.quarto.example(filename = out)
#'
#' # (delete the demo files, and the extension folder installed beside them)
#' unlink(file.path(tempdir(),
#'                   c("apaTables-examples-pdf.qmd",
#'                     "apaTables-examples-docx.qmd")))
#' unlink(file.path(tempdir(), "_extensions"), recursive = TRUE)
#' @export
apa.quarto.example <- function(filename = "apaTables-examples.qmd", overwrite = FALSE) {
  if (!is.character(filename) || length(filename) != 1L || is.na(filename)) {
    stop("'filename' must be a single file path.", call. = FALSE)
  }
  parent <- dirname(filename)
  if (!dir.exists(parent)) {
    stop("Directory does not exist: ", parent, call. = FALSE)
  }
  stem <- sub("\\.qmd$", "", filename)
  outfiles <- c(pdf  = paste0(stem, "-pdf.qmd"),
                docx = paste0(stem, "-docx.qmd"))
  templates <- c(pdf  = "example-pdf.qmd",
                 docx = "example-docx.qmd")
  for (k in names(outfiles)) {
    if (file.exists(outfiles[[k]]) && !isTRUE(overwrite)) {
      stop("File already exists: ", outfiles[[k]],
           ". Use overwrite = TRUE to replace it.", call. = FALSE)
    }
  }
  for (k in names(outfiles)) {
    src <- system.file("templates", templates[[k]], package = "apaTables")
    if (!nzchar(src)) {
      stop("Could not find the example template ", templates[[k]],
           " in the installed package.", call. = FALSE)
    }
    if (!file.copy(src, outfiles[[k]], overwrite = TRUE)) {
      stop("Failed to write the example document to: ", outfiles[[k]],
           call. = FALSE)
    }
  }
  # Both examples use apaquarto, so put the extension beside them --
  # otherwise the documents this writes cannot render, which is the exact
  # failure this whole phase exists to remove. `filename` itself is never
  # written (only the two derived -pdf/-docx files are), so normalizePath()
  # must act on its parent directory, not on `filename`.
  apa.quarto.setup(parent)

  # apa_quarto_notify(), not message(): harmless from the console, but a
  # bare message() called from inside a chunk gets typeset into whatever
  # document is being knitted -- the same trap already closed in
  # apa.quarto.setup().
  apa_quarto_notify("Wrote two example documents ",
          "(the apaquarto extension is already installed alongside them):")
  apa_quarto_notify("  ", normalizePath(outfiles[["pdf"]]),
          "  (renders to PDF; needs a LaTeX engine such as TinyTeX)")
  apa_quarto_notify("  ", normalizePath(outfiles[["docx"]]),
          "  (renders to Word; no LaTeX needed)")
  apa_quarto_notify("The repeated-measures page in each needs the afex and tidyr ",
          "packages: install.packages(c(\"afex\", \"tidyr\"))")
  invisible(outfiles)
}
