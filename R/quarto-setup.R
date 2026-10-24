# The extension apaTables ships. system.file() returns "" when the package is
# loaded by pkgload rather than installed, so fall back to the source tree.
apa_bundled_ext_dir <- function() {
  d <- system.file("_extensions", "wjschne", "apaquarto", package = "apaTables")
  if (nzchar(d) && dir.exists(d)) return(d)
  d <- file.path("inst", "_extensions", "wjschne", "apaquarto")
  if (dir.exists(d)) return(normalizePath(d))
  ""
}

apa_bundled_version <- function() {
  f <- file.path(apa_bundled_ext_dir(), "_extension.yml")
  if (!file.exists(f)) return(NA_character_)
  apa_quarto_parse_version(readLines(f, warn = FALSE))$full
}

# The relative path apaTables always installs to, used in messages instead
# of an absolute path: an absolute path is an unbreakable token that
# knitr's line wrapper cannot fold, and this one is the same in every
# message regardless of where the project actually lives.
APA_EXT_REL_PATH <- file.path("_extensions", "wjschne", "apaquarto")

# MD5 of every file in a directory, keyed by path relative to it.
#
# Quarto records no provenance at all -- a `quarto add` copy and a `cp -R`
# copy are byte-identical, there is no lockfile and no manifest -- so the only
# thing a project's copy can be compared against is the copy we ship.
apa_ext_manifest <- function(dir) {
  f <- list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  f <- f[!dir.exists(file.path(dir, f))]
  if (!length(f)) return(character(0))
  stats::setNames(unname(tools::md5sum(file.path(dir, f))), f)
}

# Files that differ between the shipped copy and a project's copy: present in
# ours and either missing or changed in theirs. Extra files on their side are
# not a difference -- a user's own additions are theirs to keep.
apa_ext_diff <- function(src, dest) {
  a <- apa_ext_manifest(src)
  if (!length(a)) return(character(0))
  b <- apa_ext_manifest(dest)
  m <- b[names(a)]
  names(a)[is.na(m) | m != a]
}

# Files present in a project's copy that apaTables does not ship at all: a
# house-style Word reference document, a locally added filter. These are
# never flagged by apa_ext_diff() and must survive every install, or a
# "replaced"/"upgraded" run would silently delete a user's own additions.
apa_ext_extra_files <- function(src, dest) {
  if (!dir.exists(dest)) return(character(0))
  setdiff(names(apa_ext_manifest(dest)), names(apa_ext_manifest(src)))
}

# The N in "+apaTables.N", or NA.
apa_ext_build <- function(full) {
  if (is.na(full)) return(NA_integer_)
  m <- regmatches(full, regexec("[+]apaTables[.]([0-9]+)", full))[[1]]
  if (length(m) < 2L) NA_integer_ else as.integer(m[2])
}

# -1 / 0 / 1 / NA comparing a project's copy of ours against the shipped
# one, numeric core first, then the apaTables build number. NA means the
# numeric core matches but the build number could not be parsed (an
# unparseable suffix such as "+apaTables.x") -- kept distinct from 0 (truly
# equal) so a caller can ask "is this newer" before ever asking "do I
# understand this version", and so a copy with a malformed suffix but a
# newer core (e.g. "9.9.9+apaTables.x") is still protected as kept-newer
# rather than being treated as something to overwrite.
apa_ext_compare <- function(theirs, ours) {
  tv <- numeric_version(apa_quarto_parse_version(paste0("version: ", theirs))$version)
  ov <- numeric_version(apa_quarto_parse_version(paste0("version: ", ours))$version)
  if (tv != ov) return(if (tv < ov) -1L else 1L)
  tb <- apa_ext_build(theirs); ob <- apa_ext_build(ours)
  if (is.na(tb) || is.na(ob)) return(NA_integer_)
  if (tb == ob) return(0L)
  if (tb < ob) -1L else 1L
}

# The version actually in play in `dest`, read directly rather than via
# apa_quarto_extension_info()'s upward, recursive search. That search is
# right for choosing which project a render will use, but wrong here:
# apa.quarto.setup() must judge and act on this exact directory, never an
# ancestor's copy (which it would leave untouched while reporting on it) and
# never a differently-named sibling extension folder that happens to sort
# first in list.files() (which would make every run re-copy and re-message,
# forever, on a project that also has an owner-less apaquarto/ folder).
apa_ext_local_info <- function(dest) {
  f <- file.path(dest, "_extension.yml")
  if (!file.exists(f))
    return(list(version = NA_character_, full = NA_character_, ours = FALSE))
  v <- apa_quarto_parse_version(readLines(f, warn = FALSE))
  list(version = v$version, full = v$full, ours = apa_quarto_is_ours(v$full))
}

# One clause reporting files apaTables preserved during an install, or "" if
# none -- appended to an install message rather than issued as its own
# message, so a run that both replaces files and preserves others still says
# only one thing.
apa_ext_preserved_clause <- function(preserved) {
  if (!length(preserved)) return("")
  paste0("; apaTables kept ", length(preserved), " file",
         if (length(preserved) == 1) "" else "s",
         " this project had added that apaTables does not ship (",
         paste(utils::head(preserved, 3), collapse = ", "),
         if (length(preserved) > 3) ", ..." else "", ")")
}

# Replace a project's copy of the extension, preserving any files the
# project added that we do not ship, via an atomic rename-swap so a failure
# part-way through cannot leave a half-written extension behind: the current
# copy is moved aside, the new copy is moved into place, and only then is
# the old copy deleted -- if the second move fails, the first is undone.
apa_ext_install <- function(src, dest) {
  parent <- dirname(dest)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  tmp <- file.path(parent, paste0(".apaquarto-new-", Sys.getpid()))
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  ok <- all(file.copy(list.files(src, full.names = TRUE, all.files = TRUE,
                                 no.. = TRUE),
                      tmp, recursive = TRUE, copy.date = TRUE))
  if (!ok) {
    unlink(tmp, recursive = TRUE)
    stop("apaTables could not copy the apaquarto extension into ", parent,
         ", so the existing copy in the project was left as it is; check that ",
         "the folder is writable.", call. = FALSE)
  }

  # apa_ext_manifest() (and so apa_ext_extra_files()) already lists
  # individual files, never directories, so each of these is a single file
  # and no `recursive =` is needed to copy it. Record what actually
  # succeeded rather than what was attempted: a file whose name collides
  # with a directory apaTables ships (the only real example today is a
  # project file literally named "typst", colliding with the shipped
  # typst/ folder) cannot be copied to the same relative path, and
  # file.copy() would silently place it somewhere else instead of failing
  # loudly -- so that collision is caught here explicitly and treated as a
  # failed preservation, not a successful one. If any file this project
  # added cannot be preserved, stop before the swap rather than complete it
  # and report a file as kept when it was not.
  extra <- apa_ext_extra_files(src, dest)
  preserved <- character(0)
  failed <- character(0)
  for (f in extra) {
    to <- file.path(tmp, f)
    if (dir.exists(to)) {
      failed <- c(failed, f)
      next
    }
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    from <- file.path(dest, f)
    ok <- isTRUE(file.copy(from, to, copy.date = TRUE)) &&
      file.exists(to) && !dir.exists(to) &&
      identical(unname(tools::md5sum(from)), unname(tools::md5sum(to)))
    if (ok) preserved <- c(preserved, f) else failed <- c(failed, f)
  }
  if (length(failed)) {
    unlink(tmp, recursive = TRUE)
    stop("apaTables could not preserve ", length(failed), " file",
         if (length(failed) == 1) "" else "s",
         " in this project that apaTables does not ship, because ",
         if (length(failed) == 1) "the name collides" else "the names collide",
         " with a file or folder apaTables does ship (",
         paste(utils::head(failed, 3), collapse = ", "),
         if (length(failed) > 3) ", ..." else "",
         "), so nothing was changed; rename ",
         if (length(failed) == 1) "that file" else "those files",
         " and try again.", call. = FALSE)
  }

  if (!dir.exists(dest)) {
    if (!file.rename(tmp, dest)) {
      unlink(tmp, recursive = TRUE)
      stop("apaTables could not put the apaquarto extension in place at ",
           dest, "; check that the folder is writable.", call. = FALSE)
    }
    return(invisible(list(dest = dest, preserved = preserved)))
  }

  old <- file.path(parent, paste0(".apaquarto-old-", Sys.getpid()))
  unlink(old, recursive = TRUE)
  if (!file.rename(dest, old)) {
    unlink(tmp, recursive = TRUE)
    stop("apaTables could not move the current apaquarto extension out of ",
         "the way at ", dest, ", so it was left as it is; check that the ",
         "folder is writable, then try again.", call. = FALSE)
  }
  if (!file.rename(tmp, dest)) {
    restored <- file.rename(old, dest)
    unlink(tmp, recursive = TRUE)
    if (restored)
      stop("apaTables could not put the new apaquarto extension in place at ",
           dest, ", so the previous copy was restored and nothing was ",
           "lost; check that the folder is writable, then try again.",
           call. = FALSE)
    stop("apaTables could not put the new apaquarto extension in place at ",
         dest, " and could not restore the previous copy either, so ", dest,
         " is now missing; run apa.quarto.setup() again once the folder is ",
         "writable.", call. = FALSE)
  }
  unlink(old, recursive = TRUE)
  invisible(list(dest = dest, preserved = preserved))
}

# Report what apa.quarto.setup() did, where the person running it will
# actually see it, and nowhere else. Interactively that is plain message().
# During a knit it cannot be message(): the scaffolded document's setup
# chunk runs with `#| message: false` so that a HEALTHY project renders with
# no stray notice in the manuscript, but message() is captured by knitr's
# condition handling and governed by that same chunk option -- so if a
# repair actually changed something, the report would be dropped before it
# reached the render log at all, leaving no trace anywhere that anything
# happened. Measured empirically (probe documents rendered both ways,
# checking terminal output and the rendered document itself): a direct
# write to the stderr connection bypasses R's condition system entirely, so
# knitr's `message` option never sees it and it always reaches the
# terminal, and knitr never captures a raw stderr write into the rendered
# document, so it never appears there either.
apa_quarto_notify <- function(...) {
  if (isTRUE(getOption("knitr.in.progress"))) {
    cat(..., "\n", sep = "", file = stderr())
  } else {
    message(...)
  }
}

# What has to be said when files were repaired DURING a render, and nothing
# when they were not.
#
# The four patches are consumed at two different times, and only one of them
# can be repaired from inside a render:
#
#   apatemplate.tex, doc-class.tex   read by xelatex AFTER the R chunks run,
#                                    so a mid-render swap DOES take effect --
#                                    which is why the table numbers come out
#                                    right even on a clobbered project
#   _extension.yml, the Lua filters  loaded by Quarto and pandoc BEFORE any R
#                                    executes, so a mid-render swap CANNOT
#                                    take effect for this render
#
# Measured over four consecutive renders of one project: with the extension
# reverted to stock beforehand, every render repaired it, every render numbered
# 1 2 3 4 correctly, and every one of those renders put the long table's note
# four pages away from its table. The output is right in one half and wrong in
# the other, exit status 0, and until now nothing said so -- which is exactly
# the silent kind of defect this package exists to stop shipping.
#
# Erroring instead is ruled out: a repair mid-render must not destroy the
# render. Re-invoking Quarto is ruled out: it would be re-entrant. So the
# honest remaining move is to say it, once, in the receipt that is already
# being written. A net is the right answer only because there is a real
# impossibility behind it.
#
# One flowing sentence, no embedded newline: knitr wraps a message to the page
# width only when it contains no "\n".
apa_midrender_clause <- function() {
  if (!isTRUE(getOption("knitr.in.progress"))) return("")
  paste0(" The repair is complete on disk, but this render had already ",
         "started when it happened and Quarto loads part of the extension ",
         "before any R code runs, so this document should be rendered once ",
         "more before the output is used.")
}

#' Install or repair the apaquarto extension in a project
#'
#' @description
#' You already have a project folder, and you want apaTables tables to come
#' out APA-formatted when the documents in it render. Run this once, in the R
#' console, from that folder:
#'
#' \preformatted{apa.quarto.setup()}
#'
#' It puts the apaquarto extension into an \code{_extensions} folder beside
#' your document. \strong{What gets installed is not the standard apaquarto
#' release}: it is a copy tested with apaTables -- apaquarto plus table fixes
#' that are not yet in the standard release. That is also why this command
#' exists at all. Running \code{quarto add} or
#' \code{quarto update} in a project silently replaces the tested copy with
#' the standard one -- while reporting \code{[No Change]} -- which brings
#' back bugs in table numbering and in where the note of a long table lands.
#' Running \code{apa.quarto.setup()} again puts the tested copy back.
#'
#' It is safe to run at any time: on a project that is already correct it does
#' nothing and says nothing. When it does repair something during a render,
#' the report reaches the render log rather than the manuscript itself, so a
#' repaired document still renders with nothing extra typeset into it.
#'
#' @param path Project folder. Defaults to the current directory. During a
#'   render, if this folder has no copy of the extension, the copy in a
#'   parent folder of the same Quarto project is used instead (Quarto finds
#'   extensions there too). \code{apa.quarto.setup()} never changes a folder
#'   above the project folder (the one with \code{_quarto.yml}), and outside
#'   a Quarto project it changes only the folder given here.
#' @param auto Replace a copy that is not ours (the standard apaquarto, or one
#'   restored by \code{quarto update})? Defaults to \code{TRUE}.
#' @param force Overwrite a copy that IS ours but has been edited locally?
#'   Defaults to \code{FALSE}, so your own edits survive.
#'
#' @return Invisibly, a list with \code{action}, \code{version}, \code{found}
#'   and \code{path}.
#'
#' @section Quarto documents:
#' Quarto (\url{https://quarto.org}) is a free system for writing documents
#' that combine text and R code. apaquarto is a Quarto extension that makes
#' such a document follow APA style: apaTables tables placed in it appear
#' as fully formatted APA tables -- number, title, and note included -- in
#' Word, PDF, or HTML. apaquarto is installed per project folder, not into
#' R itself, which is what \code{apa.quarto.setup()} does. Call it once, in
#' the R console, from the folder that contains -- or will contain -- your
#' \code{.qmd} document:
#'
#' \preformatted{library(apaTables)
#' apa.quarto.setup()}
#'
#' This puts an \code{_extensions/wjschne/apaquarto} folder next to your
#' document, and the YAML header -- the settings block between \code{---}
#' lines at the very top of the document -- can then select an apaquarto
#' format. apaquarto needs an author in that header -- with no author, or
#' an empty one, the render stops with a LaTeX error -- and it reports a
#' problem unless that author has an affiliation and is marked
#' \code{corresponding: true}. This header is enough to render:
#'
#' \preformatted{---
#' title: "My Paper"
#' author:
#'   - name: Jane Doe
#'     corresponding: true
#'     affiliations:
#'       - name: University of Guelph
#' format: apaquarto-pdf
#' ---}
#'
#' \code{apa.quarto.setup()} is safe to run at any time -- when the copy in a
#' project is already correct, it does nothing and says nothing. Do
#' \strong{not} run \code{quarto add wjschne/apaquarto} or
#' \code{quarto update extension wjschne/apaquarto} in one of these
#' folders: both commands silently replace the patched copy with the plain, unpatched
#' extension, while reporting \code{[No Change]}. That reintroduces bugs in
#' table numbering and in where the note of a long table lands. If you have already run one of those commands, running
#' \code{apa.quarto.setup()} again puts the tested copy back.
#'
#' Sending a coauthor or reviewer the whole project folder carries
#' \code{_extensions/} with it, so they do nothing extra. Sending only the
#' \code{.qmd} file does not: they must run \code{apa.quarto.setup()} in
#' that folder themselves before rendering, because with the extension
#' folder absent, Quarto fails before any R code runs, so nothing can
#' repair itself automatically.
#'
#' @seealso
#' A step-by-step guide to placing apaTables tables in Quarto documents
#' comes installed with the package -- see the "Installing the apaquarto
#' extension" section. To open it, type:
#'
#' \preformatted{vignette("apaquarto-usage", package = "apaTables")}
#'
#' Related functions: \code{\link{apa.quarto.new}} to start a whole new
#' manuscript in one command, \code{\link{apa.quarto.template}},
#' \code{\link{apa.quarto.example}}.
#' @examples
#' # Install the apaquarto extension into a temporary project folder
#' proj <- file.path(tempdir(), "apaTables-quarto-setup-example")
#' dir.create(proj, showWarnings = FALSE)
#' apa.quarto.setup(path = proj)
#'
#' # Running it again on the same, already-correct folder does nothing
#' apa.quarto.setup(path = proj)
#'
#' # (delete the demo folder)
#' unlink(proj, recursive = TRUE)
#' @export
apa.quarto.setup <- function(path = ".", auto = TRUE, force = FALSE) {
  src <- apa_bundled_ext_dir()
  if (!nzchar(src))
    stop("This installation of apaTables does not contain the apaquarto ",
         "extension, so it cannot be installed; reinstall apaTables.",
         call. = FALSE)

  root <- apa_setup_root(path)
  dest <- file.path(root, "_extensions", "wjschne", "apaquarto")
  ours <- apa_bundled_version()
  out <- function(action, found = NA_character_)
    invisible(list(action = action, version = ours, found = found, path = dest))

  if (!dir.exists(dest)) {
    apa_ext_install(src, dest)
    apa_quarto_notify("Installed the apaquarto extension apaTables ships (version ",
            ours, ") at ", APA_EXT_REL_PATH, ".", apa_midrender_clause())
    return(out("installed"))
  }

  info <- apa_ext_local_info(dest)
  found <- info$full
  differs <- apa_ext_diff(src, dest)

  if (identical(found, ours) && !length(differs)) return(out("current", found))

  if (isTRUE(info$ours)) {
    # Ask "is this newer" before ever asking "do I understand this version":
    # apa_ext_compare() looks at the numeric core first, so a copy with a
    # malformed build suffix but a newer core (e.g. "9.9.9+apaTables.x") is
    # still protected as kept-newer, rather than an unparseable suffix
    # short-circuiting straight to "replace it" and downgrading a copy that
    # was actually ahead.
    cmp <- apa_ext_compare(found, ours)
    if (!is.na(cmp) && cmp > 0L) {
      apa_quarto_notify("This project has apaquarto ", found, ", which is newer than ",
              "the ", ours, " that this version of apaTables ships, so ",
              "nothing was changed; update apaTables to move it forward.")
      return(out("kept-newer", found))
    }
    if (!is.na(cmp) && cmp < 0L) {
      res <- apa_ext_install(src, dest)
      apa_quarto_notify("apaTables updated the apaquarto extension in this project from ",
              found, " to ", ours, apa_ext_preserved_clause(res$preserved),
              ".", apa_midrender_clause())
      return(out("upgraded", found))
    }
    if (is.na(cmp)) {
      if (!force) {
        apa_quarto_notify("The apaquarto extension in this project reports a version ",
                "apaTables cannot make sense of (", found, "), so nothing ",
                "was changed; run apa.quarto.setup(force = TRUE) to ",
                "replace it with the tested copy.")
        return(out("kept-modified", found))
      }
      res <- apa_ext_install(src, dest)
      apa_quarto_notify("apaTables replaced the apaquarto extension in this project (an ",
              "unrecognized version, ", found, ") with the tested copy it ",
              "ships, version ", ours, apa_ext_preserved_clause(res$preserved),
              ".", apa_midrender_clause())
      return(out("replaced", found))
    }
    if (!force) {
      apa_quarto_notify("The apaquarto extension in this project is version ", found,
              " but ", length(differs), " of the files in it ",
              if (length(differs) == 1) "has" else "have",
              " been changed locally (",
              paste(utils::head(differs, 3), collapse = ", "),
              if (length(differs) > 3) ", ..." else "",
              "), so nothing was changed; run apa.quarto.setup(force = TRUE) ",
              "to restore the files apaTables ships.")
      return(out("kept-modified", found))
    }
    res <- apa_ext_install(src, dest)
    apa_quarto_notify("apaTables restored ", length(differs), " locally changed file",
            if (length(differs) == 1) "" else "s",
            " of the apaquarto extension in this project (version ", ours, ")",
            apa_ext_preserved_clause(res$preserved), ".", apa_midrender_clause())
    return(out("replaced", found))
  }

  if (!auto) {
    apa_quarto_notify("This project has an apaquarto extension that apaTables did not ",
            "install (version ", if (is.na(found)) "unknown" else found,
            "), and auto = FALSE, so nothing was changed; run ",
            "apa.quarto.setup() to replace it with the tested copy.")
    return(out("skipped", found))
  }

  res <- apa_ext_install(src, dest)
  apa_quarto_notify("apaTables replaced the apaquarto extension in this project (version ",
          if (is.na(found)) "unknown" else found,
          ") with the tested copy it ships, version ", ours,
          apa_ext_preserved_clause(res$preserved), ".", apa_midrender_clause())
  out("replaced", found)
}

# Which folder's _extensions are we talking about?
#
# If `path` already holds a copy of the extension, act on it directly --
# this makes the answer depend only on the argument given, never on the
# current working directory. Otherwise, walk up toward a project root only
# when a render is actually in progress (Quarto sets QUARTO_PROJECT_DIR, or
# knitr is running): that is the one situation where Quarto's own extension
# resolution makes an ancestor the right target. A plain interactive call on
# a path with no copy yet -- the common "set up a brand-new project" case --
# must not walk up at all, or a stray _extensions folder anywhere above (as
# far as the filesystem root) would capture it and the new project would
# never get its own copy. And the walk stops at the Quarto project the render
# belongs to (QUARTO_PROJECT_DIR, or the nearest folder with a _quarto.yml):
# a copy above that folder belongs to something else, and with no project at
# all there is no walk.
apa_setup_root <- function(path = ".") {
  path <- normalizePath(path, mustWork = FALSE)
  has_copy <- function(d) dir.exists(file.path(d, "_extensions", "wjschne", "apaquarto"))
  if (has_copy(path)) return(path)
  rendering <- nzchar(Sys.getenv("QUARTO_PROJECT_DIR")) ||
    isTRUE(getOption("knitr.in.progress"))
  if (!rendering) return(path)
  dirs <- apa_quarto_search_dirs(path)
  top <- apa_quarto_project_root(dirs)
  if (is.na(top)) return(path)
  for (d in dirs[seq_len(top)]) {
    if (has_copy(d)) return(d)
  }
  path
}

# Position, in `dirs` (a folder and then each parent), of the root of the
# Quarto project the render belongs to: QUARTO_PROJECT_DIR when Quarto set
# it, otherwise the nearest folder holding a _quarto.yml. NA when there is
# no project.
apa_quarto_project_root <- function(dirs) {
  project <- Sys.getenv("QUARTO_PROJECT_DIR")
  if (nzchar(project)) {
    # one path form on both sides: on Windows dirname() returns "/" where
    # normalizePath() returns "\\"
    same_form <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
    hit <- match(same_form(project), same_form(dirs))
    if (!is.na(hit)) return(hit)
  }
  is_root <- file.exists(file.path(dirs, "_quarto.yml")) |
    file.exists(file.path(dirs, "_quarto.yaml"))
  if (any(is_root)) which(is_root)[1] else NA_integer_
}
