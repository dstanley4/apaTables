# Phase 1.7 Task 5 (D64): the rotated section's MARGINS, and the branch that
# declines to rotate at all.
#
# WHY THIS IS ITS OWN FILE (fix round 1, Minor 5). These four tests started
# life in test-docx-structure.R, which is guarded FILE-WIDE by
# skip_on_cran(), a `quarto` guard and apa_skip_if_library_stale() -- all
# three correct for that file, every test in which drives a real
# `quarto render` against the INSTALLED library. None of them applies here:
# the three margin tests need `pandoc` and nothing else, and the
# refuse-branch test needs no external tool at all. Left where they were,
# the single assertion proving D64's mapping vanished on any machine without
# quarto or with a stale install -- and D64 is release-relevant, so its
# proof has to be the kind that actually runs on CRAN.
#
# MEASURED IN REAL MICROSOFT WORD, 2026-08-04 (AppleScript-driven, the route
# the "measure Word in Word" rule requires): a portrait section with four
# DISTINCT margins, flipped to landscape by Word itself, went from
#   <w:pgMar w:top="1440" w:right="1440" w:bottom="1080" w:left="2160" .../>
# to
#   <w:pgMar w:top="1440" w:right="1080" w:bottom="2160" w:left="1440" .../>
# -- new top <- old right, new right <- old bottom, new bottom <- old left,
# new left <- old top. `bottom` and `left` were unique values, so that is the
# ONLY mapping consistent with the result; the clockwise alternative would
# have put 2160 in `top` and did not. Fix round 1's review re-ran those exact
# portrait margins through the SHIPPED filter with bare pandoc and got output
# byte-identical to real Word's.
#
# THE MAPPING CANNOT BE OBSERVED FROM ANY RENDERED FIXTURE IN THIS SUITE.
# apaquarto.docx's own margins are symmetric (1440 twips all round), so the
# rotation is a no-op for the bundled reference document and a rendered
# fixture reads identically whether the mapping is right, wrong, or absent.
# The margins have to be CHOSEN, which is what apa_landscape_filter_sections()
# (helper-landscape.R) exists to do: it drives the real filter over a div
# carrying four distinct margins and reads the section properties back out of
# a real .docx.

test_that("the rotated section's margins are rotated the way Word rotates them (D64)", {
  # Four DISTINCT values, so no assertion below can pass by coincidence: a
  # mapping that is backwards, unrotated, or partially applied moves at
  # least one of them somewhere this test names.
  secs <- apa_landscape_filter_sections(mt = 1440, mr = 1000,
                                        mb = 800,  ml = 2000)
  expect_length(secs, 2L)
  land <- Filter(function(s) grepl('w:orient="landscape"', s, fixed = TRUE), secs)
  expect_length(land, 1L)

  expect_true(grepl('w:top="1000"', land[[1]], fixed = TRUE))     # <- old right
  expect_true(grepl('w:right="800"', land[[1]], fixed = TRUE))    # <- old bottom
  expect_true(grepl('w:bottom="2000"', land[[1]], fixed = TRUE))  # <- old left
  expect_true(grepl('w:left="1440"', land[[1]], fixed = TRUE))    # <- old top

  # header/footer/gutter are page-FRAME distances measured from the top and
  # bottom edges of whatever the page now is, not from a rotating side, so
  # Word leaves them alone -- and so must this.
  expect_true(grepl('w:header="720"', land[[1]], fixed = TRUE))
  expect_true(grepl('w:footer="720"', land[[1]], fixed = TRUE))
  expect_true(grepl('w:gutter="0"', land[[1]], fixed = TRUE))
})

test_that("the PORTRAIT closer's margins are NOT rotated -- only the landscape section's are", {
  # The other half of the same defect, and the one a single-sided test would
  # miss: rotating inside sectpr_xml() rather than at the call site would
  # rotate BOTH closers, silently changing the margins of the portrait pages
  # that precede a rotated table -- everything back to the previous closer,
  # i.e. most of the manuscript. Watched failing: moving the swap into
  # sectpr_xml() leaves this test red while the D64 test above stays green.
  secs <- apa_landscape_filter_sections(mt = 1440, mr = 1000,
                                        mb = 800,  ml = 2000)
  port <- Filter(function(s) !grepl('w:orient="landscape"', s, fixed = TRUE), secs)
  expect_length(port, 1L)

  expect_true(grepl('w:top="1440"', port[[1]], fixed = TRUE))
  expect_true(grepl('w:right="1000"', port[[1]], fixed = TRUE))
  expect_true(grepl('w:bottom="800"', port[[1]], fixed = TRUE))
  expect_true(grepl('w:left="2000"', port[[1]], fixed = TRUE))
  # ... and it really is the same page, upright.
  expect_true(grepl('w:w="12240"', port[[1]], fixed = TRUE))
  expect_true(grepl('w:h="15840"', port[[1]], fixed = TRUE))
})

test_that("both closers carry the reference document's own <w:cols>, in schema order", {
  # apaquarto.docx's real trailing sectPr ends `<w:pgMar .../><w:cols
  # w:space="720"/>`; both closers omitted it. Harmless in Word (one column
  # is the default when the element is absent) but a measured divergence
  # from what these closers claim to be -- "the document's real section
  # properties". Real Microsoft Word opened a .18 render with no repair
  # prompt and `pgSz pgMar cols` in every sectPr (fix round 1, coordinator).
  #
  # POSITION IS ASSERTED, NOT ONLY PRESENCE. CT_SectPr is a strict SEQUENCE
  # (... pgSz, pgMar, paperSrc, pgBorders, lnNumType, pgNumType, cols ...),
  # the same schema-order trap D35/D36 hit with <w:rPr> in the docx styles:
  # an element in the wrong place is not ignored, it can make Word reject
  # the part. A bare presence check cannot see that.
  secs <- apa_landscape_filter_sections(mt = 1440, mr = 1000,
                                        mb = 800,  ml = 2000)
  expect_length(secs, 2L)
  for (s in secs) {
    expect_true(grepl('<w:cols w:space="720"/>', s, fixed = TRUE))
    expect_lt(regexpr("<w:pgMar", s, fixed = TRUE),
              regexpr("<w:cols", s, fixed = TRUE))
  }
})

test_that("with numbered lines on, <w:lnNumType> still precedes <w:cols> -- the ordering that can actually be violated", {
  # Fix round 1, Minor 3. The test above asserts `pgMar < cols`, which is
  # true of ANY appending order and so is nearly free -- the pair CT_SectPr's
  # sequence can genuinely be violated on here is lnNumType before cols,
  # because those two are the elements this filter chooses to append.
  # The default probe writes no YAML, so `numbered-lines` is unset,
  # read_meta() leaves numbered_lines FALSE and <w:lnNumType> is simply
  # absent from the string -- the assertion could not see the violation that
  # matters. Driving the probe with `numbered-lines: true` puts the element
  # in the output, where its position can be checked.
  secs <- apa_landscape_filter_sections(mt = 1440, mr = 1000,
                                        mb = 800,  ml = 2000,
                                        numbered_lines = TRUE)
  expect_length(secs, 2L)
  for (s in secs) {
    expect_true(grepl("<w:lnNumType", s, fixed = TRUE))
    expect_lt(regexpr("<w:pgMar", s, fixed = TRUE),
              regexpr("<w:lnNumType", s, fixed = TRUE))
    expect_lt(regexpr("<w:lnNumType", s, fixed = TRUE),
              regexpr("<w:cols", s, fixed = TRUE))
  }
})

test_that("when the reference geometry cannot be read, the table is left UPRIGHT and the user is told WHICH file failed", {
  # Phase 1.7 Task 5, inherited item 3, widened by fix round 1's Important 2.
  # apa_landscape_wrap()'s refuse branch and apa_warn_no_docx_geometry()
  # together decide that a user silently gets an unrotated table -- and ran
  # in NO test at all, while Task 5 makes the branch MORE reachable, not
  # less: apa_docx_reference_sectpr() now prefers a PROJECT copy, so a
  # user's own hand-edited (or truncated, or half-written) apaquarto.docx is
  # newly able to be the file that fails to parse.
  #
  # DRIVEN END TO END WITH A REAL BROKEN PROJECT COPY, no mocking: a file
  # that is not a zip at the exact path a render would resolve. That is the
  # real shape of the failure, and it exercises the path-choosing code as
  # well as the refusal.
  apa_reset_version_warnings()
  on.exit(apa_reset_version_warnings(), add = TRUE)

  proj <- tempfile(); dir.create(proj)
  ext <- file.path(proj, "_extensions", "wjschne", "apaquarto")
  dir.create(ext, recursive = TRUE)
  broken <- file.path(ext, "apaquarto.docx")
  writeLines("this is not a zip archive", broken)

  old <- setwd(proj)
  # after = FALSE so the setwd runs BEFORE the unlink (Minor 8): on.exit
  # APPENDS with add = TRUE, and removing a directory the process is sitting
  # inside is an error on Windows.
  on.exit({ setwd(old); unlink(proj, recursive = TRUE) },
          add = TRUE, after = FALSE)

  expect_null(apa_docx_sectpr_from_file(broken))
  ref <- apa_docx_reference_path()
  expect_true(ref$project)

  body <- "| a | b |\n|---|---|\n| 1 | 2 |"
  expect_warning(out <- apa_landscape_wrap(body), "kept upright")

  # The table is returned UNCHANGED -- no landscape div, so
  # apatablelandscape.lua has nothing to act on and writes no section with
  # invented numbers (D31's own charge against Quarto's filter).
  expect_identical(out, body)
  expect_false(grepl(APA_QUARTO_LANDSCAPE_CLASS, out, fixed = TRUE))

  # ... and it is a once-per-render warning, not one per table: a manuscript
  # with six rotated tables must not print the same paragraph six times.
  expect_silent(out2 <- apa_landscape_wrap(body))
  expect_identical(out2, body)
})

test_that("the no-geometry warning names the file it actually tried and gives advice that fits it", {
  # Fix round 1, Important 2. Before the project's copy was preferred there
  # was exactly one candidate, so "its own bundled apaquarto.docx ...
  # reinstall apaTables" was true by construction. It is not any more, and
  # the refuse-branch test above pinned only "kept upright", leaving the
  # false clause unpinned -- a wrong instruction printed into a rendered
  # manuscript, the category this project treats as release-relevant.
  #
  # Both branches are asserted, each against the advice that does NOT fit
  # it as well as the one that does, so a future edit cannot swap them and
  # stay green.
  apa_reset_version_warnings()
  on.exit(apa_reset_version_warnings(), add = TRUE)

  proj <- list(path = "/some/project/_extensions/wjschne/apaquarto/apaquarto.docx",
               project = TRUE)
  msg <- tryCatch(apa_warn_no_docx_geometry(proj),
                  warning = function(w) conditionMessage(w))
  expect_match(msg, "this project is using", fixed = TRUE)
  expect_match(msg, proj$path, fixed = TRUE)
  expect_match(msg, "apa.quarto.setup()", fixed = TRUE)
  expect_false(grepl("reinstall apaTables", msg, fixed = TRUE))

  apa_reset_version_warnings()
  inst <- list(path = "/some/library/apaTables/_extensions/wjschne/apaquarto/apaquarto.docx",
               project = FALSE)
  msg2 <- tryCatch(apa_warn_no_docx_geometry(inst),
                   warning = function(w) conditionMessage(w))
  expect_match(msg2, "its own bundled apaquarto.docx", fixed = TRUE)
  expect_match(msg2, inst$path, fixed = TRUE)
  expect_match(msg2, "reinstall apaTables", fixed = TRUE)
  expect_false(grepl("apa.quarto.setup()", msg2, fixed = TRUE))

  # NO EMBEDDED NEWLINE, in either branch (auto-memory
  # `knitr-warning-newline-trap`): knitr wraps only single-line warnings, so
  # a "\n" here pushes the rest of the sentence off the right edge of a
  # knitted page, where the reader cannot see it and pdftotext still reports
  # it -- invisible to a human AND to a grep-based check.
  expect_false(grepl("\n", msg, fixed = TRUE))
  expect_false(grepl("\n", msg2, fixed = TRUE))
})
