# Phase 1.7 / D32 / D61. The tri-state and its precedence, tested at the
# level where all three destinations share it.

test_that("apa_landscape_value() normalises anything that is not a logical scalar to NA", {
  expect_true(is.na(apa_landscape_value(NULL)))
  expect_true(is.na(apa_landscape_value(character(0))))
  expect_true(is.na(apa_landscape_value(c(TRUE, FALSE))))
  expect_true(is.na(apa_landscape_value("true")))
  expect_true(is.na(apa_landscape_value(1)))
  expect_true(is.na(apa_landscape_value(NA)))
  expect_identical(apa_landscape_value(TRUE),  TRUE)
  expect_identical(apa_landscape_value(FALSE), FALSE)
})

test_that("apa_landscape_request() puts the chunk option above the argument", {
  # chunk wins whenever it has an opinion, in BOTH directions
  expect_identical(apa_landscape_request(TRUE,  FALSE), TRUE)
  expect_identical(apa_landscape_request(FALSE, TRUE),  FALSE)
  # chunk silent -> the argument decides, in BOTH directions
  expect_identical(apa_landscape_request(NA, TRUE),  TRUE)
  expect_identical(apa_landscape_request(NA, FALSE), FALSE)
  # both silent -> measurement decides
  expect_true(is.na(apa_landscape_request(NA, NA)))
  # a non-logical (e.g. YAML-parsed "true") must NOT win the precedence just
  # because it is not NA -- both arguments are normalised through
  # apa_landscape_value() first
  expect_identical(apa_landscape_request("true", FALSE), FALSE)
  expect_true(is.na(apa_landscape_request(NA, "true")))
})

test_that("landscape = FALSE FORBIDS rotation on a table that would otherwise rotate", {
  # apa.cor.table(mtcars) is 8.22 in against a 6.5 in portrait page: the
  # automatic ladder rotates it. An explicit FALSE must shrink it instead,
  # with every column kept.
  plan <- apa_display_plan(as_spec(apa.cor.table(mtcars)))
  auto <- suppressWarnings(apa_layout_decide(plan))
  expect_true(auto$rotate)

  forbidden <- suppressWarnings(apa_layout_decide(plan, landscape = FALSE))
  expect_false(forbidden$rotate)
  expect_lt(forbidden$size, auto$size)     # it shrank instead of rotating
  expect_true(forbidden$fits)              # and it still fits: nothing clipped
})

test_that("landscape = TRUE rotates a table that does not need it", {
  # apa.2way.table() with marginal means is 3.61 in and fits upright.
  plan <- apa_display_plan(as_spec(apa.2way.table(
    iv1 = gender, iv2 = alcohol, dv = attractiveness,
    data = goggles, show.marginal.means = TRUE)))
  expect_false(suppressWarnings(apa_layout_decide(plan))$rotate)
  expect_true(suppressWarnings(apa_layout_decide(plan, landscape = TRUE))$rotate)
})

test_that("the landscape default is NA, so an explicit NA and the default decide identically", {
  # NOT a regression guard on the automatic ladder's BEHAVIOUR: both sides of
  # this comparison pass the same NA value down the same code path (the
  # signature default IS NA), so this can only fail if the DEFAULT itself
  # changes -- it cannot observe any other change to the ladder's logic. The
  # "automatic ladder is unchanged" guarantee is carried by
  # tests/testthat/test-quarto-table.R:90-104, which pins actual size/rotate/
  # fits values and so CAN observe a ladder regression.
  for (tb in list(apa.cor.table(attitude), apa.cor.table(mtcars),
                  apa.1way.table(iv = dose, dv = libido, data = viagra))) {
    plan <- apa_display_plan(as_spec(tb))
    expect_identical(suppressWarnings(apa_layout_decide(plan, landscape = NA)),
                     suppressWarnings(apa_layout_decide(plan)))
  }
})

test_that("neither FORBID nor an inexecutable render rotates", {
  # NOTE what this test can and cannot prove. apa_layout_decide() returns
  # list(size, rotate, fits, width), which is IDENTICAL whether rotation was
  # suppressed by the user's landscape = FALSE or by can_execute_rotation =
  # FALSE -- so no assertion on this return value can ever tell the two
  # apart, and this test does NOT verify that FORBID and CANNOT-EXECUTE are
  # separate code paths. That separation (two independent arguments, neither
  # implemented in terms of the other -- see rotate_allowed's construction in
  # apa_layout_decide()) is a DESIGN INVARIANT enforced by code review, not by
  # this test. What this test legitimately checks is the smoke-test-level
  # fact that both routes, and their combination, do suppress rotation.
  plan <- apa_display_plan(as_spec(apa.cor.table(mtcars)))
  forbidden <- suppressWarnings(apa_layout_decide(plan, landscape = FALSE))
  cannot    <- suppressWarnings(apa_layout_decide(plan, can_execute_rotation = FALSE))
  expect_false(forbidden$rotate)
  expect_false(cannot$rotate)
  both <- suppressWarnings(apa_layout_decide(plan, landscape = TRUE,
                                             can_execute_rotation = FALSE))
  expect_false(both$rotate)   # asked for rotation, render cannot: no rotation
})

test_that("apa_chunk_landscape() is a tri-state outside a knit", {
  expect_true(is.na(apa_chunk_landscape()))
})

test_that("apa_chunk_landscape() reads all three states from the chunk option", {
  # In-process coverage of R/quarto-table.R's isFALSE(v)/"true"/"false"/
  # length-guard branches, which Task 3's render test (skip_on_cran() plus a
  # quarto guard) is the ONLY other coverage for -- on a machine with no
  # quarto that capability would otherwise be untested. knitr::opts_current
  # works this way outside a real knit (verified): $set()/$get() operate on
  # an in-memory list with no rendering context required, so no actual chunk
  # is needed to exercise the parsing.
  withr::local_options(knitr.in.progress = TRUE)
  old <- knitr::opts_current$get()
  withr::defer(knitr::opts_current$restore(old))
  knitr::opts_current$set(`apa-landscape` = FALSE)
  expect_false(apa_chunk_landscape())
  knitr::opts_current$set(`apa-landscape` = "false")
  expect_false(apa_chunk_landscape())
  knitr::opts_current$set(`apa-landscape` = "true")
  expect_true(apa_chunk_landscape())
  knitr::opts_current$set(`apa-landscape` = "yes")
  expect_true(is.na(apa_chunk_landscape()))
})

test_that("six builders default landscape to NA and carry an explicit value through", {
  build <- list(
    cor    = function(l) apa.cor.table(attitude, landscape = l),
    reg    = function(l) apa.reg.table(lm(sales ~ adverts, data = album), landscape = l),
    twoway = function(l) apa.2way.table(iv1 = gender, iv2 = alcohol,
                                        dv = attractiveness, data = goggles, landscape = l),
    dval   = function(l) apa.d.table(iv = dose, dv = libido, data = viagra, landscape = l),
    oneway = function(l) apa.1way.table(iv = dose, dv = libido, data = viagra, landscape = l),
    aov    = function(l) apa.aov.table(lm(libido ~ dose, data = viagra), landscape = l))

  default <- list(
    cor    = apa.cor.table(attitude),
    reg    = apa.reg.table(lm(sales ~ adverts, data = album)),
    twoway = apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles),
    dval   = apa.d.table(iv = dose, dv = libido, data = viagra),
    oneway = apa.1way.table(iv = dose, dv = libido, data = viagra),
    aov    = apa.aov.table(lm(libido ~ dose, data = viagra)))

  for (nm in names(build)) {
    expect_true(is.na(default[[nm]]$landscape),       info = paste(nm, "console default"))
    expect_true(is.na(default[[nm]]$spec$landscape),  info = paste(nm, "spec default"))
    for (l in c(TRUE, FALSE)) {
      tb <- build[[nm]](l)
      expect_identical(tb$landscape,      l, info = paste(nm, "console explicit", l))
      expect_identical(tb$spec$landscape, l, info = paste(nm, "spec explicit", l))
    }
  }
})

test_that("apa.afex.table defaults landscape to NA and carries an explicit value through", {
  # afex is Suggests-only: on a machine without it, this test (and the
  # is.na/isTRUE distinction it exists to guard) has no observer. That gap
  # cannot be closed without moving afex to Imports -- noted, not fixed here.
  skip_if_not_installed("afex")
  r <- suppressMessages(afex::aov_ez(id = "participant", dv = "attractiveness",
                                     data = goggles, between = c("gender", "alcohol")))
  expect_true(is.na(apa.afex.table(r)$landscape))
  expect_true(is.na(apa.afex.table(r)$spec$landscape))
  for (l in c(TRUE, FALSE)) {
    tb <- apa.afex.table(r, landscape = l)
    expect_identical(tb$landscape,      l, info = paste("afex explicit", l))
    expect_identical(tb$spec$landscape, l, info = paste("afex explicit", l))
  }
})

test_that("apa.aov.table() accepts landscape, which it never could before (D63)", {
  # The one builder that could not express a preference. In Quarto the chunk
  # option already covered it; apa.save() had no way to say anything at all.
  expect_true(is.na(apa.aov.table(lm(libido ~ dose, data = viagra))$landscape))
  expect_true(apa.aov.table(lm(libido ~ dose, data = viagra), landscape = TRUE)$landscape)
})

# --- Task 3: the Quarto PDF and Word emitters read the argument -----------
#
# Both Quarto emitters ignored landscape = entirely before 1.7 (D32 required
# that of 1.6/1.6a, so PDF and Word would agree). They must read it now.

test_that("apa_quarto_latex_block() passes the builder argument into the layout decision, which it never did", {
  # SUPERSEDES the plan brief's own sketch for this name, which asserted the
  # substring "landscape" was absent from the rendered LaTeX text. That
  # assertion could not have failed under any mutation: apa_quarto_latex_
  # block() never writes the word "landscape" anywhere in its returned
  # characters -- rotation is signalled ONLY as a side effect on a knitr
  # chunk hook, via apa_request_landscape() (R/quarto-table.R) -- so a text
  # grep on the output is blind to whether the argument was read at all.
  # Rewritten (D66) to watch what apa_layout_decide() is actually called
  # with, which is the one place the wiring this task adds can be observed
  # directly.
  captured <- NULL
  testthat::local_mocked_bindings(
    apa_layout_decide = function(plan, geom = NULL, geom_landscape = NULL,
                                 landscape = NA, can_execute_rotation = TRUE,
                                 size = NULL,
                                 col_overhead_in = 0) {
      captured <<- landscape
      list(size = APA_QUARTO_DEFAULT_SIZE, rotate = FALSE, fits = TRUE,
           width = 1)
    })
  apa_quarto_latex_block(apa.cor.table(mtcars, landscape = FALSE))
  expect_identical(captured, FALSE)

  # Fix round 1 (Important 3): FALSE alone does not prove the tri-state
  # survives the trip -- isTRUE(spec$landscape) also reduces FALSE to FALSE,
  # collapsing every DEFAULT (NA) table into a silent FORBID and losing
  # automatic rotation without any test here noticing. A default-built table
  # must still arrive as NA.
  apa_quarto_latex_block(apa.cor.table(mtcars))
  expect_true(is.na(captured))
})

test_that("apa_quarto_markdown_block() passes the builder argument into the layout decision, which it never did", {
  # The Word-facing sibling of the test above -- both emitters share the same
  # precedence resolver (apa_landscape_request()), but each has its own call
  # site into apa_layout_decide() and each needed its own wiring.
  captured <- NULL
  testthat::local_mocked_bindings(
    apa_layout_decide = function(plan, geom = NULL, geom_landscape = NULL,
                                 landscape = NA, can_execute_rotation = TRUE,
                                 size = NULL,
                                 col_overhead_in = 0) {
      captured <<- landscape
      list(size = APA_QUARTO_DEFAULT_SIZE, rotate = FALSE, fits = TRUE,
           width = 1)
    })
  apa_quarto_markdown_block(apa.cor.table(mtcars, landscape = FALSE))
  expect_identical(captured, FALSE)

  # Fix round 1 (Important 3): see the sibling test above for why a default
  # (NA) table needs its own assertion here, distinct from FALSE.
  apa_quarto_markdown_block(apa.cor.table(mtcars))
  expect_true(is.na(captured))
})

test_that("an explicit landscape = TRUE that cannot be executed warns, like the chunk option does", {
  # The existing warning fired only for the chunk option. A user who asked by
  # ARGUMENT and cannot have it was told nothing at all.
  narrow_forced <- apa.1way.table(iv = dose, dv = libido, data = viagra,
                                  landscape = TRUE)
  expect_warning(apa_quarto_latex_block(narrow_forced), "tbl-")
})
