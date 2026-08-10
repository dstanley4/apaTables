# Is the INSTALLED apaTables the package this source tree describes?
#
# Every render test in this suite is run by quarto, which spawns its own R
# session and calls library(apaTables). So a render exercises the INSTALLED
# package -- its R code AND the apaquarto extension it ships -- and never this
# source tree. Running one against a stale library certifies the library, not
# the branch.
#
# The guard used to compare the EXTENSION ONLY. That half is necessary and was
# never sufficient: edit R/render-latex.R, skip the reinstall, and every
# extension file still matched, so four render files certified the OLD
# INSTALLED EMITTER under a message saying the tree was current. C1 -- the
# ladder deciding to rotate where nothing could rotate, and a column leaving
# the page unwarned -- is exactly the kind of defect that hides there. Both
# halves are compared now.
#
# The R half cannot be an md5 of R/*.R: an installed package has no .R files,
# only a lazy-load database. What it does have is the namespace, so both sides
# deparse every object in it and hash the result. Both sides run BYTE-
# IDENTICAL code, held here as source text, because a digest computed two ways
# is a digest that can differ for reasons that are not staleness -- and this
# guard SKIPS on a difference, so a spurious difference would silently switch
# every render test off.

APA_STALE_DIGEST_SRC <- '
function() {
  ns <- asNamespace("apaTables")
  nms <- sort(ls(ns, all.names = TRUE))
  # Dot-names are the loader\'s own bookkeeping (.__DEVTOOLS__, .__NAMESPACE__,
  # .packageName): they differ between a pkgload namespace and an installed
  # one for reasons that have nothing to do with the code.
  nms <- nms[!grepl("^[.]", nms)]
  txt <- unlist(lapply(nms, function(n) {
    v <- tryCatch(get(n, envir = ns), error = function(e) NULL)
    if (is.function(v)) {
      # A closure carries its namespace environment and, under pkgload, a
      # srcref. Neither is code.
      environment(v) <- globalenv()
      attributes(v) <- NULL
    }
    c(n, tryCatch(paste(deparse(v, control = c("keepInteger", "keepNA",
                                               "niceNames")),
                        collapse = "\\n"),
                  error = function(e) "<undeparsable>"))
  }))
  f <- tempfile()
  on.exit(unlink(f))
  writeLines(txt, f)
  unname(tools::md5sum(f))
}
'

# The digest of the namespace loaded in THIS session -- the source tree, under
# devtools::test().
apa_source_r_digest <- function() {
  (eval(parse(text = APA_STALE_DIGEST_SRC)))()
}

# One value printed by a spawned R session that has done library(apaTables):
# the installed package, which is what quarto will load.
apa_installed_probe <- function(expr) {
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote(paste0("suppressMessages(library(apaTables)); ", expr))),
    stdout = TRUE, stderr = FALSE))
  paste(out, collapse = "")
}

apa_installed_r_digest <- function() {
  apa_installed_probe(paste0("cat((", APA_STALE_DIGEST_SRC, ")())"))
}

apa_installed_ext_dir <- function() {
  apa_installed_probe(paste0(
    "cat(system.file('_extensions', 'wjschne', 'apaquarto', ",
    "package = 'apaTables'))"))
}

# Call at the top level of a render test file. Skips the whole file, with a
# message naming what to do about it.
#
# SKIPPING HERE IS NOT A PASS. Everything below a stale guard renders against
# the INSTALLED library, so a stale install cannot test the branch -- and a
# skip prints in the same summary line as a pass, which this project has
# repeatedly mistaken for one. Every message therefore says what is wrong in
# words a human scanning the output cannot read as success, and names the
# command that fixes it.
apa_skip_if_library_stale <- function() {
  loud <- function(what) paste0(
    "THE INSTALLED LIBRARY IS STALE, so these tests did NOT run and this ",
    "branch is UNTESTED -- ", what,
    ". Fix with: R CMD build . && R CMD INSTALL apaTables_*.tar.gz")

  skip_if_not(requireNamespace("apaTables", quietly = TRUE),
              "apaTables is not installed, so quarto cannot load it")
  # apa.quarto.table() is RETIRED (Task 8, D30) -- its ABSENCE is what a
  # current install now has, so it can no longer be the marker. The newest
  # export is one of its three replacements; apa.quarto.docx() marks the
  # same currency the old check did, the cheap way, before the full R-code
  # digest below.
  skip_if_not("apa.quarto.docx" %in% getNamespaceExports("apaTables"),
              loud("the installed apaTables predates apa.quarto.docx()"))

  # The R code.
  inst_r <- apa_installed_r_digest()
  skip_if_not(nzchar(inst_r),
              loud("the installed apaTables namespace could not be read"))
  skip_if_not(identical(apa_source_r_digest(), inst_r),
              loud(paste0("the installed apaTables R code differs from this ",
                          "source tree")))

  # The extension.
  src <- file.path(test_path("..", ".."), "inst", "_extensions", "wjschne",
                   "apaquarto")
  skip_if_not(dir.exists(src), "no source tree to compare the library against")
  inst <- apa_installed_ext_dir()
  skip_if_not(nzchar(inst) && dir.exists(inst),
              loud("the installed apaTables ships no apaquarto extension"))
  a <- apa_ext_manifest(src)
  b <- apa_ext_manifest(inst)
  # The union, not just our side: a file DELETED from this branch but still
  # shipped by a stale library is staleness too, and comparing only our names
  # is blind in exactly that direction.
  all_names <- union(names(a), names(b))
  differ <- all_names[is.na(a[all_names]) | is.na(b[all_names]) |
                        a[all_names] != b[all_names]]
  skip_if_not(!length(differ), loud(paste0(
    "the installed apaquarto extension differs from this source tree in ",
    length(differ), " file(s), e.g. ", differ[1])))
}
