# apaquarto extension version detection (R/apaquarto-version.R).
#
# A project's apaquarto copy is a frozen snapshot, and an old copy ignores a
# setting it does not know instead of complaining, so these tests cover both
# halves of the contract: the version is read from the copy the render would
# actually use, and nothing is said when there is nothing to say.

# Build a throwaway project directory holding an apaquarto extension at
# `version`, in either the published layout (_extensions/<owner>/apaquarto)
# or the bare layout a copy of the upstream repo has (_extensions/apaquarto).
make_ext <- function(version, owner = "wjschne", name = "apaquarto",
                     extra = character(0)) {
  root <- file.path(tempfile("apaqproj"))
  dir <- if (nzchar(owner)) file.path(root, "_extensions", owner, name)
         else file.path(root, "_extensions", name)
  dir.create(dir, recursive = TRUE)
  writeLines(c("title: My Document in APA Style, Seventh Edition",
               "author: W. Joel Schneider",
               paste0("version: ", version),
               "quarto-required: \">=1.4.549\"",
               extra),
             file.path(dir, "_extension.yml"))
  root
}

test_that("the version is read from an extension in the published layout", {
  root <- make_ext("5.0.18")
  info <- apa_quarto_extension_info(root)
  expect_identical(info$version, "5.0.18")
  expect_true(info$numeric >= numeric_version("5.0.17"))
  expect_true(file.exists(info$path))
})

test_that("the version is read from a bare (no owner folder) layout", {
  root <- make_ext("5.0.16", owner = "")
  expect_identical(apa_quarto_extension_info(root)$version, "5.0.16")
})

test_that("the search walks upward the way Quarto resolves extensions", {
  root <- make_ext("5.0.18")
  deep <- file.path(root, "chapters", "results")
  dir.create(deep, recursive = TRUE)
  expect_identical(apa_quarto_extension_info(deep)$version, "5.0.18")
})

test_that("no extension anywhere is reported as NA, not as an error", {
  root <- file.path(tempfile("emptyproj"))
  dir.create(root)
  info <- apa_quarto_extension_info(root)
  expect_true(is.na(info$version))
  expect_true(is.na(info$path))
  expect_true(identical(info$numeric, NA))
})

test_that("a non-apaquarto extension is ignored", {
  root <- file.path(tempfile("otherext"))
  dir <- file.path(root, "_extensions", "quarto-ext", "fancy-text")
  dir.create(dir, recursive = TRUE)
  writeLines(c("title: Fancy Text", "version: 1.2.3"),
             file.path(dir, "_extension.yml"))
  expect_true(is.na(apa_quarto_extension_info(root)$version))
})

test_that("a quoted version parses, and a nested version: key does not", {
  expect_identical(apa_quarto_parse_version(c("version: \"5.0.18\""))$version, "5.0.18")
  expect_identical(apa_quarto_parse_version(c("version: '4.2'"))$version, "4.2")
  expect_true(is.na(apa_quarto_parse_version(c("contributes:", "  version: 9.9.9"))$version))
  expect_true(is.na(apa_quarto_parse_version(character(0))$version))
})

test_that("a warning is emitted once and then suppressed until reset", {
  apa_reset_version_warnings()
  on.exit(apa_reset_version_warnings(), add = TRUE)
  expect_warning(apa_warn_once("some-key", "first time"), "first time")
  expect_silent(apa_warn_once("some-key", "first time"))
  # a different subject still speaks
  expect_warning(apa_warn_once("other-key", "other subject"), "other subject")
  # and the reset restores a clean slate, so one test cannot silence the next
  apa_reset_version_warnings()
  expect_warning(apa_warn_once("some-key", "first time"), "first time")
})

test_that("the warn-once registry starts empty for this test file", {
  # If a previous file had left the flag set, this warning would not fire --
  # which is the leak the reset above exists to prevent.
  apa_reset_version_warnings()
  expect_warning(apa_warn_once("page-geometry", "leak check"),
                 "leak check")
  apa_reset_version_warnings()
})

test_that("parse_version separates the numeric core from build metadata", {
  expect_equal(apa_quarto_parse_version("version: 5.0.18"),
               list(version = "5.0.18", full = "5.0.18"))
  expect_equal(apa_quarto_parse_version("version: 5.0.18+apaTables.1"),
               list(version = "5.0.18", full = "5.0.18+apaTables.1"))
  expect_equal(apa_quarto_parse_version("version: \"5.0.18+apaTables.12\""),
               list(version = "5.0.18", full = "5.0.18+apaTables.12"))
  expect_equal(apa_quarto_parse_version("version: 5.0.18-dev"),
               list(version = "5.0.18", full = "5.0.18-dev"))
})

test_that("parse_version rejects what it cannot understand, cleanly", {
  expect_true(is.na(apa_quarto_parse_version("version: v5.0.18")$version))
  expect_true(is.na(apa_quarto_parse_version("title: apaquarto")$version))
  expect_true(is.na(apa_quarto_parse_version(character(0))$version))
})

test_that("is_ours recognises only our own build suffix", {
  expect_true(apa_quarto_is_ours("5.0.18+apaTables.1"))
  expect_false(apa_quarto_is_ours("5.0.18"))
  expect_false(apa_quarto_is_ours("5.0.18+other.1"))
  expect_false(apa_quarto_is_ours(NA_character_))
})

test_that("extension_info reports full and ours without changing version", {
  d <- file.path(tempdir(), "vinfo", "_extensions", "wjschne", "apaquarto")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(file.path(tempdir(), "vinfo"), recursive = TRUE), add = TRUE)
  writeLines(c("title: apaquarto", "version: 5.0.18+apaTables.3"),
             file.path(d, "_extension.yml"))
  info <- apa_quarto_extension_info(file.path(tempdir(), "vinfo"))
  expect_equal(info$version, "5.0.18")
  expect_equal(info$full, "5.0.18+apaTables.3")
  expect_true(info$ours)
  expect_true(info$numeric >= numeric_version("5.0.17"))
})
