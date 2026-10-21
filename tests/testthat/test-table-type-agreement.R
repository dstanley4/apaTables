# The object's `$table.type` and the spec's `meta$table.type` are ONE fact and
# must be spelled ONE way, in every builder.
#
# THE DEFECT THIS EXISTS FOR: if they disagree -- as `apa.aov.table()` once
# did, with "aovstats" on the object and "aov" in the spec -- the type-to-builder
# map in R/quarto.R, which keys on the SPEC field, hands a reader who
# reasonably assumed the two were the same field a value the map has no entry
# for. `R CMD check` then dies running a shipped example while a guard test
# reading the other field passes: the test agrees with itself and not with
# the code.
#
# A per-builder literal cannot catch the next one, because a literal is written
# by the same person who wrote the divergence. This asserts the IDENTITY across
# every builder instead, so a new or renamed builder that gets it wrong fails
# here rather than in a user's document.

apa_type_pairs <- function() {
  options(contrasts = c("contr.sum", "contr.poly"))
  list(
    "apa.cor.table"  = apa.cor.table(attitude[, 1:3]),
    "apa.reg.table"  = apa.reg.table(lm(sales ~ adverts, data = album)),
    "apa.d.table"    = apa.d.table(iv = dose, dv = libido, data = viagra),
    "apa.aov.table"  = apa.aov.table(lm(libido ~ dose, data = viagra)),
    "apa.1way.table" = apa.1way.table(iv = dose, dv = libido, data = viagra),
    "apa.2way.table" = apa.2way.table(iv1 = gender, iv2 = alcohol,
                                      dv = attractiveness, data = goggles)
  )
}

test_that("every builder spells table.type the same way on the object and in the spec", {
  tabs <- suppressWarnings(apa_type_pairs())
  # The loop must not be able to inspect nothing -- an every-builder check that
  # silently iterates an empty list is decorative.
  expect_gt(length(tabs), 0L)
  for (nm in names(tabs)) {
    x <- tabs[[nm]]
    obj  <- x$table.type
    spec <- x$spec$meta$table.type
    expect_true(is.character(obj) && length(obj) == 1L && nzchar(obj),
                info = paste(nm, "-- object $table.type is not a single string"))
    expect_identical(obj, spec,
                     info = paste0(nm, " -- object says '", obj,
                                   "', spec meta says '", spec, "'"))
  }
})

test_that("every builder's table.type resolves in the type-to-builder map", {
  # The other half of the defect: `[[` on a named CHARACTER vector THROWS
  # rather than returning NULL, so an `is.null()` fallback beside it could
  # never run. Assert the lookup RESOLVES, not merely that a key exists.
  skip_if_not(exists("APA_TYPE_BUILDER"), "map not available")
  map <- get("APA_TYPE_BUILDER")
  tabs <- suppressWarnings(apa_type_pairs())
  for (nm in names(tabs)) {
    ty <- tabs[[nm]]$spec$meta$table.type
    got <- tryCatch(map[[ty]], error = function(e) NULL)
    expect_true(!is.null(got) && nzchar(got),
                info = paste0(nm, " -- table.type '", ty,
                              "' does not resolve in the builder map"))
  }
})
