#' Start a new APA manuscript that renders on the first try
#'
#' @description
#' One command gives you a working APA manuscript:
#'
#' \preformatted{apa.quarto.new("my-paper")}
#'
#' This creates a \code{my-paper} folder holding \code{my-paper.qmd} -- a
#' Quarto document with a working APA header and one worked table -- and,
#' beside it, an \code{_extensions} folder holding the extension that makes
#' the rendered document follow APA style. Open \code{my-paper.qmd} and
#' click Render (the Preview button in Positron), or run
#' \code{quarto render} in that folder: the very first render produces a
#' complete APA PDF. The starter is set up for PDF; for a Word manuscript,
#' change its \code{format: apaquarto-pdf} line to
#' \code{format: apaquarto-docx}.
#'
#' \strong{What gets installed is not the standard apaquarto release.} It
#' is apaTables' own tested copy: apaquarto plus table fixes that are not
#' yet in the standard release. So always install it with this function (or
#' \code{\link{apa.quarto.setup}} for a folder you already have), never
#' with Quarto's own \code{quarto add}, which gets the standard release
#' instead.
#'
#' The \code{_extensions} folder is the part you cannot write by hand: a
#' document's settings block only names a format, and the folder is what
#' supplies it. Without it the document does not render at all -- Quarto
#' stops with \code{Unable to read the extension 'apaquarto'} before any R
#' code runs, so apaTables never gets the chance to install it for you.
#'
#' @param path Folder to create. It is created if it does not exist.
#' @param filename Name for the document. Defaults to the folder's own name
#'   with a \code{.qmd} extension.
#' @param overwrite Overwrite an existing document of that name?
#'
#' @return Invisibly, a list with \code{path}, \code{document} and
#'   \code{setup} (what \code{\link{apa.quarto.setup}} did).
#'
#' @section Quarto documents:
#' Quarto (\url{https://quarto.org}) is a free system for writing documents
#' that combine text and R code. apaquarto is a Quarto extension that makes
#' such a document follow APA style -- tables get their number, italic
#' title, and \emph{Note.} line automatically, in Word, PDF, or HTML. In
#' the R console, type:
#'
#' \preformatted{library(apaTables)
#' apa.quarto.new("my-paper")}
#'
#' This creates a folder called \code{my-paper} containing \code{my-
#' paper.qmd}. The document's YAML header (the settings block between
#' \code{---} lines at the top) already has everything apaquarto needs to
#' render: a title, an author with
#' \code{corresponding: true} and an affiliation (apaquarto stops with a
#' LaTeX error if the author is missing or has neither), and
#' \code{format: apaquarto-pdf} --
#'
#' \preformatted{---
#' title: "My Paper"
#' shorttitle: "My Paper"
#' author:
#'   - name: Your Name
#'     corresponding: true
#'     email: you@example.org
#'     affiliations:
#'       - name: Your University
#' format: apaquarto-pdf
#' ---}
#'
#' plus one worked apaTables table already in the body. Beside the
#' document, \code{apa.quarto.new()} also installs an
#' \code{_extensions/wjschne/apaquarto} folder (via
#' \code{\link{apa.quarto.setup}}), so opening \code{my-paper/my-paper.qmd}
#' and clicking Render -- or running \code{quarto render} in that folder --
#' produces a complete APA PDF on the first try, with no terminal command
#' to install anything first.
#'
#' Do \strong{not} run \code{quarto add wjschne/apaquarto} or
#' \code{quarto update extension wjschne/apaquarto} in the folder this
#' creates: both commands silently replace the patched extension with the plain,
#' unpatched one, while reporting \code{[No Change]}. That reintroduces bugs
#' in table numbering and in where a long table's note lands. If you have already run one of those commands, running
#' \code{\link{apa.quarto.setup}}\code{()} again puts the tested copy back.
#'
#' Sending a coauthor or reviewer the whole project folder carries
#' \code{_extensions/} with it, so they do nothing extra. Sending only the
#' \code{.qmd} file does not: they must run \code{apa.quarto.setup()} in
#' that folder themselves before rendering, because with the extension
#' folder absent, Quarto fails before any R code runs.
#'
#' @seealso
#' A step-by-step guide to placing apaTables tables in Quarto documents
#' comes installed with the package -- see its "Installing the apaquarto
#' extension" section. To open it, type:
#'
#' \preformatted{vignette("apaquarto-usage", package = "apaTables")}
#'
#' Related functions: \code{\link{apa.quarto.setup}} to install or repair
#' the extension in a project you already have,
#' \code{\link{apa.quarto.example}}.
#' @examples
#' # Start a new manuscript in a temporary folder
#' proj <- file.path(tempdir(), "apaTables-quarto-new-example")
#' apa.quarto.new(proj)
#'
#' # (delete the demo folder)
#' unlink(proj, recursive = TRUE)
#' @export
apa.quarto.new <- function(path, filename = NULL, overwrite = FALSE) {
  if (missing(path) || !is.character(path) || length(path) != 1L || !nzchar(path))
    stop("Give apa.quarto.new() the name of the folder to create, for ",
         "example apa.quarto.new(\"my-paper\").", call. = FALSE)
  # Validate everything before installing or writing anything, so a bad call
  # leaves nothing behind: a `path` that already exists as a FILE would
  # otherwise let dir.create() fail silently and normalizePath(mustWork =
  # TRUE) succeed on the file, surfacing only much later as a cryptic
  # file.copy() error; a `filename` carrying a folder (e.g. "sub/paper.qmd")
  # would install the extension successfully and then fail to open the
  # connection for the document, leaving an extension with no document; and
  # a `filename` that climbs out with "../" would write the document outside
  # the folder that received the extension, so it could never render.
  if (file.exists(path) && !dir.exists(path))
    stop("apa.quarto.new() needs a folder to create or use, but ", path,
         " already exists and is a file, not a folder.", call. = FALSE)
  if (!is.null(filename) &&
      (!is.character(filename) || length(filename) != 1L || !nzchar(filename) ||
       !identical(basename(filename), filename)))
    stop("'filename' must be a plain file name with no folder in it, for ",
         "example \"paper.qmd\", not a path such as \"sub/paper.qmd\" or ",
         "\"../paper.qmd\".", call. = FALSE)

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  root <- normalizePath(path, mustWork = TRUE)
  if (is.null(filename)) filename <- paste0(basename(root), ".qmd")
  if (!grepl("[.]qmd$", filename)) filename <- paste0(filename, ".qmd")
  doc <- file.path(root, filename)

  if (file.exists(doc) && !isTRUE(overwrite))
    stop("There is already a file called ", filename, " in ", root,
         "; pass overwrite = TRUE to replace it.", call. = FALSE)

  setup <- apa.quarto.setup(root)

  tpl <- system.file("templates", "apaquarto-new.qmd", package = "apaTables")
  if (!nzchar(tpl) || !file.exists(tpl))
    tpl <- file.path("inst", "templates", "apaquarto-new.qmd")
  txt <- readLines(tpl, warn = FALSE)
  txt <- gsub("@APATABLES_VERSION@",
              as.character(utils::packageVersion("apaTables")), txt, fixed = TRUE)
  txt <- gsub("@APAQUARTO_VERSION@", apa_bundled_version(), txt, fixed = TRUE)
  writeLines(txt, doc)

  apa_quarto_notify("apaTables created ", doc,
                    "; open it and click Render, or run quarto render in this folder.")
  invisible(list(path = root, document = doc, setup = setup))
}
