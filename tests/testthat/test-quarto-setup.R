test_that("the bundled extension is present and complete", {
  d <- apa_bundled_ext_dir()
  expect_true(nzchar(d) && dir.exists(d))
  for (f in c("_extension.yml", "apa.csl", "apaquarto.docx",
              "apatemplate.tex", "doc-class.tex", "apafloatlatex.lua",
              "PATCHES.md"))
    expect_true(file.exists(file.path(d, f)), info = f)
})

test_that("the bundled extension declares our own build version", {
  expect_true(apa_quarto_is_ours(apa_bundled_version()))
  expect_true(numeric_version(apa_quarto_parse_version(
    readLines(file.path(apa_bundled_ext_dir(), "_extension.yml"),
              warn = FALSE))$version) >= numeric_version("5.0.17"))
})

# The FLOOR, and why a floor rather than an existence check.
#
# The bundled apaquarto patches have been silently reverted before: a
# `git checkout <branch> -- inst/_extensions` STAGES what it restores, so the
# bundle reverts to an earlier build with nothing in the working tree to show
# for it. That PATCHES.md exists is not enough -- an older bundle also
# satisfies it, every file of it intact and every longtable patch gone.
#
# Two independent readings, because the version string is metadata and can be
# right while the code is not: the declared build must be at or above the one
# the package needs, and the code the package depends on must be IN the files.
APA_REQUIRED_EXT_VERSION <- "5.0.18"
APA_REQUIRED_EXT_BUILD <- 5L

test_that("the bundled apaquarto is at or above the build the package needs", {
  v <- apa_bundled_version()
  expect_false(is.na(v))
  expect_true(apa_quarto_is_ours(v))
  # a plain comparison: expect_gte() on numeric_version objects errors under
  # testthat 3.2.3 ("'-' not defined"), which the package still allows
  expect_true(numeric_version(sub("[+].*$", "", v)) >=
                numeric_version(APA_REQUIRED_EXT_VERSION), info = v)
  expect_false(is.na(apa_ext_build(v)))
  expect_gte(apa_ext_build(v), APA_REQUIRED_EXT_BUILD)
})

test_that("the bundled apaquarto carries the longtable patches, not just a version", {
  # Each of these is load-bearing: without it a rendered table loses rows,
  # its note or its rotation, silently. This is the assertion that catches a
  # reverted bundle on the day.
  lua_lines <- readLines(file.path(apa_bundled_ext_dir(), "apafloatlatex.lua"),
                         warn = FALSE)
  lua <- paste(lua_lines, collapse = "\n")
  # The float wrapper is dropped when the CONTENT is a longtable, with
  # no chunk option required.
  expect_true(grepl("haslongtable", lua, fixed = TRUE))
  # Rotation is a literal landscape block, never sidewaystable.
  expect_true(grepl("landscapewrap", lua, fixed = TRUE))
  # LIVE CODE, not comments: the move away from sidewaystable is explained in
  # comments that (necessarily) name the construct it replaced, which a
  # whole-file grepl() reads as the construct still being emitted.
  lua_code <- lua_lines[!grepl("^\\s*--", lua_lines)]
  expect_false(any(grepl("sidewaystable", lua_code, fixed = TRUE)))
  # A short longtable is kept whole where floats stay in the text.
  #
  # AGAINST LIVE CODE, for the reason stated just above, and it is not
  # hypothetical here: the emitted command is \Needspace*, not \needspace,
  # while the word "needspace" appears all over the comments explaining WHY.
  # A whole-file grepl() for "needspace" would go on passing had the emission
  # been deleted outright.
  expect_true(any(grepl("\\Needspace*{", lua_code, fixed = TRUE)))
  expect_false(any(grepl("\\needspace{", lua_code, fixed = TRUE)))
  tex <- paste(readLines(file.path(apa_bundled_ext_dir(), "apatemplate.tex"),
                         warn = FALSE), collapse = "\n")
  expect_true(grepl("needspace", tex, fixed = TRUE))
  # Without this, a wide AND long table collected at the end does not
  # rotate at all -- endfloat defers the table and leaves the wrapper behind.
  expect_true(grepl("DeclareDelayedFloatFlavor{landscape}{table}", tex,
                    fixed = TRUE))
})

test_that("the bundled apaquarto carries the patches we depend on", {
  tex <- readLines(file.path(apa_bundled_ext_dir(), "apatemplate.tex"),
                   warn = FALSE)
  expect_true(any(grepl("LTpatch@captionfalse", tex, fixed = TRUE)))
  # Check live code, not comments: the patch's own removal is explained in a
  # comment that (necessarily) names the deleted command, which would
  # otherwise false-positive a naive whole-file grepl().
  tex_code <- tex[!grepl("^\\s*%", tex)]
  expect_false(any(grepl("\\patchcmd{\\longtable}", tex_code, fixed = TRUE)))
  dc <- readLines(file.path(apa_bundled_ext_dir(), "doc-class.tex"),
                  warn = FALSE)
  expect_true(any(grepl("$papersize$paper", dc, fixed = TRUE)))
  lua <- readLines(file.path(apa_bundled_ext_dir(), "apafloatlatex.lua"),
                   warn = FALSE)
  expect_true(any(grepl("endlastfoot", lua, fixed = TRUE)))
})

# Build a fake project whose extension copy is whatever we say it is.
fake_project <- function(state) {
  root <- file.path(tempdir(), paste0("proj-", state, "-", sample.int(1e6, 1)))
  dest <- file.path(root, "_extensions", "wjschne", "apaquarto")
  if (state != "empty") {
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    file.copy(list.files(apa_bundled_ext_dir(), full.names = TRUE),
              dest, recursive = TRUE)
    yml <- file.path(dest, "_extension.yml")
    v <- switch(state,
                current = apa_bundled_version(),
                stock   = "5.0.18",
                older   = "5.0.18+apaTables.0",
                newer   = "5.0.18+apaTables.99",
                edited  = apa_bundled_version())
    l <- readLines(yml, warn = FALSE)
    writeLines(sub("^version:.*", paste0("version: ", v), l), yml)
    if (state == "edited")
      cat("-- local edit\n", file = file.path(dest, "apanote.lua"), append = TRUE)
  } else {
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
  }
  root
}

# apa_ext_compare() decides whether apa.quarto.setup() upgrades, keeps or
# repairs a user's copy, and its NA and newer-than branches are otherwise only
# reached through a full install. Tested directly, because getting this
# decision wrong leaves a user on stale Lua filters while the message blames
# them for a local edit.
test_that("extension comparison ranks the numeric core before the build number", {
  # numeric core wins outright, whatever the suffixes say
  expect_identical(apa_ext_compare("5.0.17+apaTables.9", "5.0.18+apaTables.1"), -1L)
  expect_identical(apa_ext_compare("5.0.19+apaTables.1", "5.0.18+apaTables.9"), 1L)
  expect_identical(apa_ext_compare("4.10.2", "5.0.18+apaTables.1"), -1L)

  # same core: the apaTables build number breaks the tie
  expect_identical(apa_ext_compare("5.0.18+apaTables.1", "5.0.18+apaTables.2"), -1L)
  expect_identical(apa_ext_compare("5.0.18+apaTables.3", "5.0.18+apaTables.2"), 1L)
  expect_identical(apa_ext_compare("5.0.18+apaTables.2", "5.0.18+apaTables.2"), 0L)
})

test_that("extension comparison answers NA only when the core matches and the build does not parse", {
  # a stock copy carries no build number at all -- NA, not 0: "equal core" is
  # not "same extension", and treating it as equal would leave a user on stock
  # filters for ever
  expect_true(is.na(apa_ext_compare("5.0.18", "5.0.18+apaTables.2")))
  expect_true(is.na(apa_ext_compare("5.0.18+apaTables.x", "5.0.18+apaTables.2")))
  expect_true(is.na(apa_ext_compare("5.0.18+other.4", "5.0.18+apaTables.2")))

  # but an unparseable suffix on a NEWER core is still newer, so a caller can
  # protect it before ever asking whether it understands the version
  expect_identical(apa_ext_compare("9.9.9+apaTables.x", "5.0.18+apaTables.2"), 1L)
})

test_that("setup installs into a project that has no extension", {
  p <- fake_project("empty"); on.exit(unlink(p, recursive = TRUE))
  expect_message(r <- apa.quarto.setup(p), "Installed")
  expect_equal(r$action, "installed")
  expect_equal(r$version, apa_bundled_version())
  expect_true(file.exists(file.path(p, "_extensions", "wjschne", "apaquarto",
                                    "_extension.yml")))
})

test_that("setup is silent and does nothing on a healthy project", {
  p <- fake_project("current"); on.exit(unlink(p, recursive = TRUE))
  expect_silent(r <- apa.quarto.setup(p))
  expect_equal(r$action, "current")
})

test_that("setup replaces a stock or reverted copy", {
  p <- fake_project("stock"); on.exit(unlink(p, recursive = TRUE))
  expect_message(r <- apa.quarto.setup(p), "5.0.18")
  expect_equal(r$action, "replaced")
  expect_equal(r$found, "5.0.18")
  expect_true(apa_quarto_is_ours(
    apa_quarto_extension_info(p)$full))
})

test_that("auto = FALSE reports a stock copy but does not convert it", {
  p <- fake_project("stock"); on.exit(unlink(p, recursive = TRUE))
  expect_message(r <- apa.quarto.setup(p, auto = FALSE))
  expect_equal(r$action, "skipped")
  expect_false(apa_quarto_is_ours(apa_quarto_extension_info(p)$full))
})

test_that("setup upgrades an older copy of ours", {
  p <- fake_project("older"); on.exit(unlink(p, recursive = TRUE))
  expect_message(r <- apa.quarto.setup(p))
  expect_equal(r$action, "upgraded")
  expect_equal(apa_quarto_extension_info(p)$full, apa_bundled_version())
})

test_that("setup never downgrades a newer copy", {
  p <- fake_project("newer"); on.exit(unlink(p, recursive = TRUE))
  dest <- file.path(p, "_extensions", "wjschne", "apaquarto")
  before <- apa_ext_manifest(dest)
  expect_message(r <- apa.quarto.setup(p))
  expect_equal(r$action, "kept-newer")
  # Byte-for-byte, not just the version string: an overwrite that happened
  # to spare _extension.yml would still be caught this way.
  expect_equal(apa_ext_manifest(dest), before)
})

test_that("setup never clobbers a local edit unless forced", {
  p <- fake_project("edited"); on.exit(unlink(p, recursive = TRUE))
  f <- file.path(p, "_extensions", "wjschne", "apaquarto", "apanote.lua")
  before <- tools::md5sum(f)
  expect_message(r <- apa.quarto.setup(p), "force")
  expect_equal(r$action, "kept-modified")
  expect_equal(unname(tools::md5sum(f)), unname(before))     # unchanged on disk

  expect_message(r2 <- apa.quarto.setup(p, force = TRUE))
  expect_equal(r2$action, "replaced")
  expect_false(identical(unname(tools::md5sum(f)), unname(before)))
})

test_that("setup is idempotent", {
  p <- fake_project("empty"); on.exit(unlink(p, recursive = TRUE))
  apa.quarto.setup(p)
  expect_silent(r <- apa.quarto.setup(p))
  expect_equal(r$action, "current")
})

test_that("setup preserves a project's own files when it replaces the extension", {
  p <- fake_project("stock"); on.exit(unlink(p, recursive = TRUE))
  dest <- file.path(p, "_extensions", "wjschne", "apaquarto")
  writeLines("-- user's own filter", file.path(dest, "mycustom.lua"))
  dir.create(file.path(dest, "mystuff"), showWarnings = FALSE)
  writeLines("notes", file.path(dest, "mystuff", "notes.txt"))

  expect_message(r <- apa.quarto.setup(p))
  expect_equal(r$action, "replaced")
  expect_true(file.exists(file.path(dest, "mycustom.lua")))
  expect_equal(readLines(file.path(dest, "mycustom.lua"), warn = FALSE),
               "-- user's own filter")
  expect_true(file.exists(file.path(dest, "mystuff", "notes.txt")))
  expect_equal(readLines(file.path(dest, "mystuff", "notes.txt"), warn = FALSE),
               "notes")
})

test_that("setup is idempotent even when an owner-less apaquarto copy exists", {
  p <- fake_project("empty"); on.exit(unlink(p, recursive = TRUE))
  # An owner-less copy (as if apaquarto were added straight from upstream,
  # without the wjschne/ owner level) sorts before "wjschne" in list.files()
  # and must not be mistaken for the copy apaTables manages: judging the
  # wrong directory is what made this non-idempotent before. The decoy must
  # be a STOCK copy (no "+apaTables." suffix): a decoy that happened to
  # carry our own build suffix would read as "current" by coincidence even
  # from the wrong directory, masking exactly the bug this test exists to
  # catch.
  alt <- file.path(p, "_extensions", "apaquarto")
  dir.create(alt, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(apa_bundled_ext_dir(), full.names = TRUE),
            alt, recursive = TRUE)
  alt_yml <- file.path(alt, "_extension.yml")
  writeLines(sub("^version:.*", "version: 5.0.18",
                 readLines(alt_yml, warn = FALSE)), alt_yml)

  expect_message(r1 <- apa.quarto.setup(p), "Installed")
  expect_equal(r1$action, "installed")
  expect_silent(r2 <- apa.quarto.setup(p))
  expect_equal(r2$action, "current")
})

test_that("a project file colliding with a shipped folder blocks the install and changes nothing", {
  p <- fake_project("stock"); on.exit(unlink(p, recursive = TRUE))
  dest <- file.path(p, "_extensions", "wjschne", "apaquarto")
  # "typst" is the one name apaTables ships as a folder rather than a file.
  # Replace the shipped typst/ folder with a plain file of the same name,
  # as if a user (or an older apaquarto layout) had put one there: that
  # file cannot be preserved at its own path during an install, and the
  # command must refuse the whole install rather than silently drop it (or
  # worse, complete the install and claim to have kept it).
  unlink(file.path(dest, "typst"), recursive = TRUE)
  writeLines("-- not really typst", file.path(dest, "typst"))
  before <- apa_ext_manifest(dest)

  expect_error(apa.quarto.setup(p), "typst")
  expect_true(file.exists(file.path(dest, "typst")) &&
                !dir.exists(file.path(dest, "typst")))
  expect_equal(apa_ext_manifest(dest), before)  # nothing in dest was touched
})

test_that("a failed rename-in triggers rollback that restores the previous copy byte-for-byte", {
  p <- fake_project("stock"); on.exit(unlink(p, recursive = TRUE))
  dest <- file.path(p, "_extensions", "wjschne", "apaquarto")
  before <- apa_ext_manifest(dest)
  parent <- dirname(dest)

  # Exercise the rollback branch of apa_ext_install() -- "dest -> old
  # succeeds, but tmp -> dest fails" -- without depending on filesystem
  # permissions (not portable to Windows) and without changing production
  # code: give a private copy of apa_ext_install() a lexical environment
  # that shadows only file.rename(), parented on the function's real
  # environment so every other free variable (unlink, dir.create,
  # file.copy, apa_ext_extra_files, Sys.getpid, ...) still resolves through
  # the real package namespace.
  real_rename <- file.rename
  call_n <- 0L
  poisoned_rename <- function(from, to) {
    call_n <<- call_n + 1L
    if (call_n == 2L) return(FALSE)  # the tmp -> dest move
    real_rename(from, to)
  }
  install_with_poisoned_rename <- apa_ext_install
  shadow <- new.env(parent = environment(apa_ext_install))
  shadow$file.rename <- poisoned_rename
  environment(install_with_poisoned_rename) <- shadow

  expect_error(
    install_with_poisoned_rename(apa_bundled_ext_dir(), dest),
    "the previous copy was restored and nothing was lost")

  expect_equal(apa_ext_manifest(dest), before)
  leftovers <- list.files(parent, pattern = "^\\.apaquarto-", all.files = TRUE)
  expect_equal(leftovers, character(0))
})

test_that("a locked file inside the previous copy cannot corrupt the swap", {
  skip_on_os("windows")
  p <- fake_project("stock")
  on.exit({
    system2("chmod", c("-R", "u+rwx", shQuote(p)), stdout = FALSE, stderr = FALSE)
    unlink(p, recursive = TRUE)
  })
  dest <- file.path(p, "_extensions", "wjschne", "apaquarto")
  locked <- file.path(dest, "locked-subdir")
  dir.create(locked, showWarnings = FALSE)
  writeLines("x", file.path(locked, "f.txt"))
  Sys.chmod(locked, mode = "0555")

  # Before the fix, deleting the old copy up front left this subdirectory's
  # file un-removable, the rename-in of the new copy then failed on
  # "Directory not empty", and the handler deleted the staged new copy too
  # -- gutting the extension. The fixed swap must never destroy the old copy
  # until the new one is already fully in place.
  expect_message(r <- apa.quarto.setup(p))
  expect_equal(r$action, "replaced")
  expect_true(file.exists(file.path(dest, "_extension.yml")))
  expect_true(apa_quarto_is_ours(apa_ext_local_info(dest)$full))
  expect_equal(length(apa_ext_diff(apa_bundled_ext_dir(), dest)), 0L)
})

# apa_quarto_notify() is how apa.quarto.setup() reports what it did. It must
# reach the console normally, but during a knit -- where the scaffolded
# document's setup chunk runs with `#| message: false` so a healthy project
# renders with no stray notice in the manuscript -- message() would be
# captured (and dropped) by that very chunk option, so a repair that DID
# change something would happen with no trace anywhere. These two tests pin
# the routing directly; the render test further down pins the full,
# real-render behaviour it exists to produce.
test_that("apa_quarto_notify() behaves like message() outside a knit", {
  expect_message(apa_quarto_notify("interactive receipt"), "interactive receipt",
                 fixed = TRUE)
})

test_that("apa_quarto_notify() bypasses message() during a knit but still reaches the console", {
  old <- getOption("knitr.in.progress")
  options(knitr.in.progress = TRUE)
  on.exit(options(knitr.in.progress = old))

  con <- textConnection("captured", "w", local = TRUE)
  sink(con, type = "message")
  on.exit({ sink(type = "message"); close(con) }, add = TRUE)

  # No "message" condition is raised: this is exactly what a chunk's
  # `message: false` option intercepts, so if apa_quarto_notify() raised one
  # here it would be silently dropped, reproducing the bug this pins.
  expect_no_message(apa_quarto_notify("knitting receipt"))
  expect_true(any(grepl("knitting receipt", captured, fixed = TRUE)))
})

test_that("a mid-render repair's receipt reaches the render log but never the rendered document", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.which("quarto")), "quarto CLI not available")
  # The spawned quarto R session must load THIS source tree, not whatever
  # apaTables happens to be installed -- otherwise this test would exercise
  # (and could pass against) stale code. Same skip guard and env var as
  # test-apaquarto-integration.R, for the same reason. LIMITATION: this means
  # no automated test ever renders the SHIPPED template's actual
  # `library(apaTables)` line -- only pkgload::load_all() is exercised here.
  # That trade-off is deliberate for the property this test checks, but it
  # is worth knowing when relying on this file as coverage for the shipped
  # apaquarto-new.qmd template itself.
  pkg_root <- Sys.getenv("APATABLES_PKG_ROOT",
                         normalizePath(test_path("..", ".."), mustWork = FALSE))
  skip_if_not(file.exists(file.path(pkg_root, "DESCRIPTION")) &&
                dir.exists(file.path(pkg_root, "R")) &&
                length(list.files(file.path(pkg_root, "R"), pattern = "[.]R$")) > 0,
              "apaTables source tree not available (e.g. inside R CMD check or covr)")

  p <- fake_project("stock"); on.exit(unlink(p, recursive = TRUE))
  doc <- file.path(p, "receipt.qmd")
  writeLines(c(
    "---",
    "title: receipt",
    "format: html",
    "---",
    "",
    "```{r}",
    "#| include: false",
    "pkgload::load_all(Sys.getenv(\"APATABLES_PKG_ROOT\"), quiet = TRUE)",
    "```",
    "",
    "```{r}",
    "#| echo: false",
    "#| message: false",
    "apa.quarto.setup(\".\")",
    "```",
    "",
    "Body text follows."
  ), doc)
  withr_env <- Sys.getenv("APATABLES_PKG_ROOT")
  Sys.setenv(APATABLES_PKG_ROOT = pkg_root)
  on.exit(if (nzchar(withr_env)) Sys.setenv(APATABLES_PKG_ROOT = withr_env)
          else Sys.unsetenv("APATABLES_PKG_ROOT"), add = TRUE)

  res <- suppressWarnings(system2("quarto", c("render", doc),
                                  stdout = TRUE, stderr = TRUE))
  html <- file.path(p, "receipt.html")
  expect_true(file.exists(html), info = paste(res, collapse = "\n"))

  expect_true(any(grepl("replaced", res, ignore.case = TRUE)),
             info = paste(res, collapse = "\n"))
  rendered <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_false(grepl("replaced", rendered, ignore.case = TRUE))
})

# During a render, apa.quarto.setup() looks in parent folders for a copy of
# the extension only inside the Quarto project the render belongs to: a copy
# above the project folder, or any copy when there is no project at all,
# belongs to something else and is never changed.
apa_stock_copy_in <- function(dir) {
  ext <- file.path(dir, "_extensions", "wjschne", "apaquarto")
  dir.create(ext, recursive = TRUE)
  writeLines(c("title: apaquarto", "version: 5.0.18"), file.path(ext, "_extension.yml"))
  file.path(ext, "_extension.yml")
}

test_that("mid-render, setup leaves a parent folder's copy alone when there is no Quarto project", {
  withr::local_options(knitr.in.progress = TRUE)
  withr::local_envvar(QUARTO_PROJECT_DIR = "")
  base <- withr::local_tempdir()
  above <- apa_stock_copy_in(base)
  doc_dir <- file.path(base, "paper")
  dir.create(doc_dir)
  utils::capture.output(r <- apa.quarto.setup(path = doc_dir), type = "message")
  expect_equal(r$action, "installed")
  expect_true(any(grepl("^version: 5.0.18$", readLines(above))))
  expect_true(dir.exists(file.path(doc_dir, "_extensions", "wjschne", "apaquarto")))
})

test_that("mid-render, setup uses the project's copy in a parent folder of the same project", {
  withr::local_options(knitr.in.progress = TRUE)
  withr::local_envvar(QUARTO_PROJECT_DIR = "")
  base <- withr::local_tempdir()
  above <- apa_stock_copy_in(base)
  writeLines("project:\n  type: default", file.path(base, "_quarto.yml"))
  doc_dir <- file.path(base, "chapters")
  dir.create(doc_dir)
  utils::capture.output(r <- apa.quarto.setup(path = doc_dir), type = "message")
  expect_equal(r$action, "replaced")
  expect_true(apa_quarto_is_ours(apa_ext_local_info(dirname(above))$full))
  expect_false(dir.exists(file.path(doc_dir, "_extensions")))
})

test_that("mid-render, setup never goes above the project folder", {
  withr::local_options(knitr.in.progress = TRUE)
  withr::local_envvar(QUARTO_PROJECT_DIR = "")
  base <- withr::local_tempdir()
  above <- apa_stock_copy_in(base)
  project <- file.path(base, "project")
  dir.create(project)
  writeLines("project:\n  type: default", file.path(project, "_quarto.yml"))
  utils::capture.output(r <- apa.quarto.setup(path = project), type = "message")
  expect_equal(r$action, "installed")
  expect_true(any(grepl("^version: 5.0.18$", readLines(above))))
})

test_that("mid-render, QUARTO_PROJECT_DIR is the boundary when Quarto sets it", {
  withr::local_options(knitr.in.progress = TRUE)
  base <- withr::local_tempdir()
  above <- apa_stock_copy_in(base)
  # the _quarto.yml is ABOVE the folder Quarto names as the project, so only
  # the QUARTO_PROJECT_DIR boundary keeps the walk from reaching `above`
  writeLines("project:\n  type: default", file.path(base, "_quarto.yml"))
  project <- file.path(base, "project")
  sub <- file.path(project, "sub")
  dir.create(sub, recursive = TRUE)
  withr::local_envvar(QUARTO_PROJECT_DIR = project)
  utils::capture.output(r <- apa.quarto.setup(path = sub), type = "message")
  expect_equal(r$action, "installed")
  expect_true(any(grepl("^version: 5.0.18$", readLines(above))))
})

test_that("mid-render, a _quarto.yaml marks the project folder too", {
  withr::local_options(knitr.in.progress = TRUE)
  withr::local_envvar(QUARTO_PROJECT_DIR = "")
  base <- withr::local_tempdir()
  above <- apa_stock_copy_in(base)
  project <- file.path(base, "project")
  dir.create(project)
  writeLines("project:\n  type: default", file.path(project, "_quarto.yaml"))
  utils::capture.output(r <- apa.quarto.setup(path = project), type = "message")
  expect_equal(r$action, "installed")
  expect_true(any(grepl("^version: 5.0.18$", readLines(above))))
})
