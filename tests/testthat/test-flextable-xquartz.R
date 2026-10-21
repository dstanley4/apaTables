# On a Mac whose gdtools build links the XQuartz X11 libraries (Posit
# Package Manager's does; CRAN's does not), loading flextable fails without
# XQuartz installed. apaTables turns exactly that failure into instructions
# and re-raises every other failure unchanged.
#
# This machine's flextable loads fine, so the failure is simulated by
# standing in for apa_load_flextable() with the error text R actually
# printed on a GitHub Actions macOS runner (2026-10-21).

xquartz_error <- function() {
  stop(paste0(
    "unable to load shared object '/Users/runner/work/_temp/Library/gdtools/",
    "libs/gdtools.so':\n  dlopen(/Users/runner/work/_temp/Library/gdtools/",
    "libs/gdtools.so, 0x0006): Library not loaded: ",
    "/opt/X11/lib/libXrender.1.dylib"), call. = FALSE)
}

test_that("the XQuartz load failure becomes instructions, XQuartz first", {
  local_mocked_bindings(apa_load_flextable = xquartz_error)
  err <- expect_error(apa_require_flextable(), class = "error")
  msg <- conditionMessage(err)
  expect_match(msg, "missing XQuartz", fixed = TRUE)
  expect_match(msg, "https://www.xquartz.org", fixed = TRUE)
  expect_match(msg, 'install.packages("gdtools", repos = "https://cloud.r-project.org")',
               fixed = TRUE)
  # XQuartz is the fix; the CRAN reinstall is the stopgap, so it comes after.
  expect_lt(regexpr("xquartz.org", msg, fixed = TRUE)[[1]],
            regexpr("install.packages", msg, fixed = TRUE)[[1]])
  # One paragraph: a multi-line condition is truncated in a knitted page.
  expect_false(grepl("\n", msg, fixed = TRUE))
})

test_that("any other flextable load failure is re-raised unchanged", {
  local_mocked_bindings(apa_load_flextable = function() {
    stop("there is no package called 'flextable'", call. = FALSE)
  })
  err <- expect_error(apa_require_flextable())
  expect_identical(conditionMessage(err), "there is no package called 'flextable'")
  expect_false(grepl("XQuartz", conditionMessage(err), fixed = TRUE))
})

test_that("a Linux X11 library error is not mistaken for the Mac problem", {
  local_mocked_bindings(apa_load_flextable = function() {
    stop("unable to load shared object 'gdtools.so':\n  libXrender.so.1: ",
         "cannot open shared object file", call. = FALSE)
  })
  err <- expect_error(apa_require_flextable())
  expect_false(grepl("XQuartz", conditionMessage(err), fixed = TRUE))
})

test_that("a working flextable passes straight through", {
  expect_invisible(apa_require_flextable())
  expect_true(isNamespaceLoaded("flextable"))
})

test_that("apa.save() reaches the guard before any flextable call", {
  local_mocked_bindings(apa_load_flextable = xquartz_error)
  tbl <- apa.cor.table(attitude[, 1:3])
  f <- withr::local_tempfile(fileext = ".docx")
  expect_error(apa.save(f, tbl), "missing XQuartz", fixed = TRUE)
  expect_false(file.exists(f))
})

test_that("the Quarto measuring route reaches the guard too", {
  local_mocked_bindings(apa_load_flextable = xquartz_error)
  tbl <- apa.cor.table(attitude[, 1:3])
  expect_error(utils::capture.output(apa.quarto.template(tbl)),
               "missing XQuartz", fixed = TRUE)
})
