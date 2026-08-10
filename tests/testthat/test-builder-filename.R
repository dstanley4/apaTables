library(apaTables)

# The one line this file guards, present in every builder:
#
#      if (make_file_flag == TRUE) { apa.save(filename = filename, tbl) }
#
# Everything DOWNSTREAM of that call -- the docx/html construction, page
# sections, rotation, numbering -- is apa.save()'s own tested territory
# (test-apa-save.R, test-docx-structure.R, and the 2026-08-09 round-2
# real-Word review of every table type). What had no coverage at all was
# the delegation itself: before the docs audit, five of the seven builders'
# `filename` arguments were exercised by no test, on a route the migration
# vignette documents by name. This file tests exactly the wiring and
# nothing deeper: a filename writes the file, the builder still returns
# its table, and no filename writes nothing.
#
# afex is NOT in the loop below: its builder takes afex::aov_ez() output,
# needs skip_if_not_installed, and already has this exact coverage in
# test-apa-afex-table.R (added with the filename = NA normalization, as
# did apa.aov.table's in test-apa-aov-table.R -- kept there because they
# also assert the normalized default).

test_that("every lm/data builder's filename argument writes through apa.save and still returns the table", {
     options(contrasts = c("contr.helmert", "contr.poly"))
     lm_viagra <- lm(libido ~ dose, data = viagra)
     builders <- list(
          cor  = function(fn) apa.cor.table(attitude[, 1:3], filename = fn),
          reg  = function(fn) apa.reg.table(lm(rating ~ complaints, data = attitude),
                                            filename = fn),
          aov  = function(fn) apa.aov.table(lm_viagra, filename = fn),
          oneway = function(fn) apa.1way.table(iv = dose, dv = libido,
                                               data = viagra, filename = fn),
          twoway = function(fn) apa.2way.table(iv1 = gender, iv2 = alcohol,
                                               dv = attractiveness,
                                               data = goggles, filename = fn),
          d    = function(fn) apa.d.table(iv = dose, dv = libido,
                                          data = viagra, filename = fn)
     )
     withr::with_tempdir({
          for (nm in names(builders)) {
               fn <- paste0(nm, ".docx")
               out <- builders[[nm]](fn)
               expect_true(file.exists(fn), info = nm)
               expect_s3_class(out, "apa_table")
          }
          # No filename: same builders write nothing. Run AFTER the writes
          # above so an accidental write would collide with a known name
          # rather than vanish into an empty-dir comparison.
          n_before <- length(list.files())
          for (nm in names(builders)) builders[[nm]](NA)
          expect_equal(length(list.files()), n_before)
     })
})
