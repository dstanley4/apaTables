library(apaTables)

# MBESS::conf.limits.nct() executes options(warn = -1) in its own scope and
# never restores it. apa.d.table() reaches it through MBESS::ci.smd(), so
# before the guard was added a single d-value table left warnings disabled
# for the rest of the session -- including apaTables' own warnings during a
# Quarto render. Verified against MBESS 5.0.0: ci.smd() is the only entry
# point we use that actually leaks (ci.R2()/conf.limits.ncf() do not), but
# DESCRIPTION pins no MBESS version, so all three call sites are guarded.

# Run `body` in a fresh R process against the same copy of apaTables this
# test session is using, returning stdout + stderr as one string. A separate
# process is required for the "a later warning is still delivered" property:
# testthat's calling handlers fire no matter what options(warn) is, so an
# in-process expect_warning() cannot tell warn = 0 from warn = -1. At the top
# level of a script, warn = -1 really does discard the message.
run_in_fresh_r <- function(body) {
     pkg <- find.package("apaTables")
     loader <- if (isTRUE(pkgload::is_dev_package("apaTables"))) {
          sprintf("pkgload::load_all(%s, quiet = TRUE)",
                  encodeString(pkg, quote = '"'))
     } else {
          sprintf("library(apaTables, lib.loc = %s)",
                  encodeString(dirname(pkg), quote = '"'))
     }
     libs <- paste(vapply(.libPaths(), encodeString, character(1), quote = '"'),
                   collapse = ", ")
     script <- tempfile(fileext = ".R")
     on.exit(unlink(script), add = TRUE)
     writeLines(c(sprintf(".libPaths(c(%s))", libs), loader, body), script)
     out <- suppressWarnings(
          system2(file.path(R.home("bin"), "Rscript"), shQuote(script),
                  stdout = TRUE, stderr = TRUE))
     paste(out, collapse = "\n")
}

test_that("a warning raised after apa.d.table is still delivered (fresh R)", {
     skip_on_cran()
     skip_if_not_installed("MBESS")
     skip_if_not_installed("pkgload")
     skip_if(!file.exists(file.path(R.home("bin"), "Rscript")), "no Rscript")

     out <- run_in_fresh_r(c(
          "options(warn = 0)",
          "tbl <- apa.d.table(iv = dose, dv = libido, data = viagra)",
          "cat('WARN_AFTER:', getOption('warn'), '\\n')",
          "warning('still-visible-marker')"))

     # the option itself survived the table
     expect_match(out, "WARN_AFTER: 0", fixed = TRUE)
     # ... and a warning raised afterwards still reaches the user. Under the
     # leaked warn = -1 this line is silently discarded.
     expect_match(out, "still-visible-marker", fixed = TRUE)
})

test_that("apa.d.table leaves getOption('warn') unchanged", {
     skip_if_not_installed("MBESS")
     old_warn <- getOption("warn")
     on.exit(options(warn = old_warn), add = TRUE)

     options(warn = 0)
     t1 <- apa.d.table(iv = dose, dv = libido, data = viagra)
     expect_s3_class(t1, "apa_table")
     expect_equal(getOption("warn"), 0)

     # A non-default setting must also survive the call.
     options(warn = 1)
     t2 <- apa.d.table(iv = dose, dv = libido, data = viagra)
     expect_s3_class(t2, "apa_table")
     expect_equal(getOption("warn"), 1)
})

# The two tests below are DEFENSIVE regression guards, not evidence of a bug:
# with MBESS 5.0.0 these entry points never disturbed options(warn), so they
# pass with or without the guards in R/apaRegressionTable.R and
# R/etaSquaredCI.R. They exist to catch a future MBESS that routes ci.R2() or
# conf.limits.ncf() through conf.limits.nct(), or a careless removal of the
# guard alongside such a change.
test_that("apa.reg.table leaves getOption('warn') unchanged", {
     skip_if_not_installed("MBESS")
     old_warn <- getOption("warn")
     on.exit(options(warn = old_warn), add = TRUE)

     options(warn = 1)
     blk1 <- lm(sales ~ adverts + airplay, data = album)
     r1 <- apa.reg.table(blk1)
     expect_s3_class(r1, "apa_table")
     expect_equal(getOption("warn"), 1)
})

test_that("get.ci.partial.eta.squared leaves getOption('warn') unchanged", {
     skip_if_not_installed("MBESS")
     old_warn <- getOption("warn")
     on.exit(options(warn = old_warn), add = TRUE)

     options(warn = 1)
     ci <- get.ci.partial.eta.squared(F.value = 6.00, df1 = 1, df2 = 42,
                                      conf.level = .90)
     expect_true(is.list(ci))
     expect_equal(getOption("warn"), 1)
})
