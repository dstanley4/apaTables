# apaquarto extension version detection.
#
# apaquarto is a third-party Quarto extension that a user installs PER
# PROJECT ("quarto add wjschne/apaquarto"), which copies the extension's
# files into the project's _extensions folder. That copy is a frozen
# snapshot: nothing in Quarto tells the user it has aged, and a project
# started two years ago still renders happily against a two-year-old copy.
#
# That matters here because apaTables ships its own patched apaquarto and
# apa.quarto.setup() has to say what it found before it replaces it: whether a
# copy is present, what version it reports, and whether it is one of ours (our
# build suffix is the only mark that distinguishes it, and `quarto update`
# strips it silently). Reading the version out of the project's own copy is
# what lets those messages say something true.
#
# The feature-minimum constants that used to head this file
# (APA_APAQUARTO_MIN_LONGTABLE and its siblings) went with the render-time
# warnings they gated: Phase 2's bundled extension decides the longtable and
# the rotation itself, from the LaTeX it is handed, so there is no longer a
# setting a copy can silently ignore.

# Directories to search for an _extensions folder, nearest first.
#
# Quarto resolves an extension by looking beside the document and then in
# each parent directory up to the project root, so the same walk finds the
# copy the render will actually use. The walk starts at the document's
# directory when knitr can name it (during a render the working directory is
# already the document's, but being explicit costs nothing) and stops at the
# filesystem root.
apa_quarto_search_dirs <- function(start = NULL) {
  if (is.null(start)) {
    start <- Sys.getenv("QUARTO_PROJECT_DIR")
    if (!nzchar(start)) start <- getwd()
  }
  start <- tryCatch(normalizePath(start, mustWork = FALSE),
                    error = function(e) start)
  dirs <- character(0)
  d <- start
  repeat {
    dirs <- c(dirs, d)
    up <- dirname(d)
    if (identical(up, d) || !nzchar(up)) break
    d <- up
    if (length(dirs) > 64L) break   # paranoia: never loop forever
  }
  dirs
}

# Candidate _extension.yml paths for apaquarto under one directory.
#
# The published layout is _extensions/<owner>/<name>/_extension.yml
# (_extensions/wjschne/apaquarto/), but a copy taken straight from the
# upstream repository has no owner level (_extensions/apaquarto/), and a
# user may have renamed the folder. So: look for any _extension.yml at
# either depth and keep the ones that are apaquarto's.
apa_quarto_extension_files <- function(dir) {
  root <- file.path(dir, "_extensions")
  if (!dir.exists(root)) return(character(0))
  c(list.files(root, pattern = "^_extension[.]ya?ml$", full.names = TRUE,
               recursive = FALSE),
    list.files(root, pattern = "^_extension[.]ya?ml$", full.names = TRUE,
               recursive = TRUE))
}

# Is this _extension.yml apaquarto's? Its own folder name is the extension
# name Quarto uses, and apaquarto's title line is stable enough to serve as
# a second opinion for a renamed folder.
apa_quarto_is_apaquarto <- function(path, lines) {
  if (grepl("apaquarto", basename(dirname(path)), ignore.case = TRUE)) return(TRUE)
  any(grepl("apaquarto", lines[grepl("^(title|contributes):", lines)],
            ignore.case = TRUE))
}

# The version from an _extension.yml, split into its numeric core and the
# verbatim string.
#
# SemVer build metadata ("+apaTables.1") is what tells our own patched copy
# from stock upstream, and it is invisible everywhere else: Quarto strips it
# from `quarto list extensions`, and a reversion by `quarto add`/`quarto
# update` replaces _extension.yml wholesale, so the suffix simply disappears.
# Retaining it here is what makes a reversion detectable at all.
#
# Read with a targeted regular expression rather than a YAML parser: the key
# is a top-level scalar on its own line, one regex reads it, and apaTables
# gains no new dependency for the sake of one field. Only unindented lines
# are considered, so a "version:" nested under some other key cannot be
# mistaken for the extension's own.
#
# Returns list(version, full): `version` is the dotted numeric core, used for
# every >= comparison; `full` carries any pre-release or build suffix.
apa_quarto_parse_version <- function(lines) {
  none <- list(version = NA_character_, full = NA_character_)
  hit <- grep("^version:[[:space:]]*", lines, value = TRUE)
  if (!length(hit)) return(none)
  m <- regmatches(
    hit[1],
    regexec(paste0("^version:[[:space:]]*[\"']?",
                   "([0-9]+(?:[.][0-9]+)*)",
                   "((?:[-+][0-9A-Za-z.]+)*)"),
            hit[1], perl = TRUE))[[1]]
  if (length(m) < 3L) return(none)
  list(version = m[2], full = paste0(m[2], m[3]))
}

# Is this version string one of ours? Only our own suffix counts: a stock
# copy, and anyone else's build metadata, are both "not ours".
apa_quarto_is_ours <- function(full) {
  !is.na(full) && grepl("+apaTables.", full, fixed = TRUE)
}

# The apaquarto extension this document will render against.
#
# Returns a list with:
#   version  the version string ("5.0.18"), or NA_character_ when no
#            apaquarto extension was found (the normal case for Word, HTML,
#            console and plain-Quarto users -- absence is not a problem)
#   full     the verbatim version string, including any build suffix
#   ours     TRUE when `full` carries our own "+apaTables." build suffix
#   numeric  the same as a numeric_version for comparison, or NA
#   path     the _extension.yml that was read, or NA_character_
apa_quarto_extension_info <- function(start = NULL) {
  none <- list(version = NA_character_, full = NA_character_, ours = FALSE,
               numeric = NA, path = NA_character_)
  for (d in apa_quarto_search_dirs(start)) {
    for (f in unique(apa_quarto_extension_files(d))) {
      lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) NULL)
      if (is.null(lines) || !apa_quarto_is_apaquarto(f, lines)) next
      v <- apa_quarto_parse_version(lines)
      if (is.na(v$version)) next
      num <- tryCatch(numeric_version(v$version), error = function(e) NA)
      return(list(version = v$version, full = v$full,
                  ours = apa_quarto_is_ours(v$full),
                  numeric = num, path = f))
    }
  }
  none
}

# Warn-once registry.
#
# A document with six tables must not carry six copies of the same warning,
# so each message is keyed by its subject AND the document being rendered:
# two documents rendered in one R session each get told once, and a second
# render of the same document in a fresh session (which is what Quarto does)
# starts clean. apa_reset_version_warnings() empties it so one test cannot
# silence the next.
apa_version_warned <- new.env(parent = emptyenv())

apa_reset_version_warnings <- function() {
  rm(list = ls(apa_version_warned, all.names = TRUE), envir = apa_version_warned)
  invisible(NULL)
}

apa_render_id <- function() {
  f <- Sys.getenv("QUARTO_DOCUMENT_FILE")
  if (!nzchar(f)) f <- Sys.getenv("QUARTO_DOCUMENT_PATH")
  if (!nzchar(f) && isTRUE(getOption("knitr.in.progress"))) {
    f <- tryCatch({
      ci <- knitr::current_input(dir = TRUE)
      if (is.null(ci)) "" else ci
    }, error = function(e) "")
  }
  if (!length(f) || is.na(f) || !nzchar(f)) f <- "<no-document>"
  f
}

apa_warn_once <- function(key, msg) {
  k <- paste(key, apa_render_id(), sep = "\r")
  if (!is.null(apa_version_warned[[k]])) return(invisible(FALSE))
  assign(k, TRUE, envir = apa_version_warned)
  warning(msg, call. = FALSE)
  invisible(TRUE)
}
