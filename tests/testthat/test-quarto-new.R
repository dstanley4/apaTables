test_that("new() creates a folder, a document and an extension", {
  p <- file.path(tempdir(), paste0("np", sample.int(1e6, 1)))
  on.exit(unlink(p, recursive = TRUE))
  # expect_message() with no regexp returns the captured condition under
  # testthat edition 3 (this package's edition), not the wrapped call's
  # value, so the return value is captured by assigning inside the
  # expression rather than from expect_message()'s own return.
  r <- NULL
  expect_message(r <- apa.quarto.new(p))
  expect_true(dir.exists(p))
  expect_true(file.exists(r$document))
  expect_equal(basename(r$document), paste0(basename(p), ".qmd"))
  expect_true(dir.exists(file.path(p, "_extensions", "wjschne", "apaquarto")))
  expect_equal(r$setup$action, "installed")
})

test_that("the scaffolded document records both versions and repairs itself", {
  p <- file.path(tempdir(), paste0("np", sample.int(1e6, 1)))
  on.exit(unlink(p, recursive = TRUE))
  r <- apa.quarto.new(p)
  txt <- readLines(r$document, warn = FALSE)
  expect_true(any(grepl(as.character(utils::packageVersion("apaTables")),
                        txt, fixed = TRUE)))
  expect_true(any(grepl(apa_bundled_version(), txt, fixed = TRUE)))
  expect_true(any(grepl("apa.quarto.setup()", txt, fixed = TRUE)))
  expect_false(any(grepl("@APATABLES_VERSION@", txt, fixed = TRUE)))
  # the sharing rule keeps BOTH cases, not a blanket instruction
  expect_true(any(grepl("whole folder", txt)))
  expect_true(any(grepl("on its own", txt)))
})

test_that("new() refuses to overwrite an existing document unless told", {
  p <- file.path(tempdir(), paste0("np", sample.int(1e6, 1)))
  on.exit(unlink(p, recursive = TRUE))
  apa.quarto.new(p)
  expect_error(apa.quarto.new(p), "overwrite")
  expect_message(apa.quarto.new(p, overwrite = TRUE))
})

test_that("new() rejects a path that already exists as a file, before touching anything", {
  p <- file.path(tempdir(), paste0("np", sample.int(1e6, 1), ".txt"))
  writeLines("not a folder", p)
  on.exit(unlink(p))
  expect_error(apa.quarto.new(p), "folder")
  # nothing was left behind: still a plain file, no extension installed
  expect_false(dir.exists(p))
  expect_true(file.exists(p))
})

test_that("new() rejects a filename that carries a folder, before installing anything", {
  p <- file.path(tempdir(), paste0("np", sample.int(1e6, 1)))
  on.exit(unlink(p, recursive = TRUE))
  expect_error(apa.quarto.new(p, filename = "sub/paper.qmd"), "filename")
  expect_false(dir.exists(file.path(p, "_extensions")))
})

test_that("new() rejects a filename that escapes the folder, before installing anything", {
  p <- file.path(tempdir(), paste0("np", sample.int(1e6, 1)))
  on.exit(unlink(p, recursive = TRUE))
  expect_error(apa.quarto.new(p, filename = "../escape.qmd"), "filename")
  expect_false(dir.exists(file.path(p, "_extensions")))
})

test_that("example() installs the extension where it writes", {
  p <- file.path(tempdir(), paste0("ex", sample.int(1e6, 1)))
  dir.create(p, recursive = TRUE)
  on.exit(unlink(p, recursive = TRUE))
  f <- file.path(p, "ex.qmd")
  # apa.quarto.example() derives two output files from the stem (documented
  # behaviour, unchanged by this task); it never writes the literal `f`.
  expect_message(apa.quarto.example(f))
  expect_true(file.exists(file.path(p, "ex-pdf.qmd")))
  expect_true(file.exists(file.path(p, "ex-docx.qmd")))
  expect_true(dir.exists(file.path(p, "_extensions", "wjschne", "apaquarto")))
})
