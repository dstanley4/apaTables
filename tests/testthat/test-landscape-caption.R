# A rotated table's CAPTION belongs on the rotated page with its
# table, and a run of consecutive rotated tables belongs in ONE landscape
# section rather than returning to portrait -- and opening a blank page --
# between each pair.
#
# THESE ASSERTIONS CANNOT SEE A PAGE. Pagination happens in Word, and a
# six-page document and an eight-page one differ here only in which section
# closers are present. Everything below is STRUCTURAL; the pagination itself
# was checked by exporting from real Word and looking. A green run of this
# file proves the closers are where we intend, NOT that the document
# paginates correctly -- the same limit the slide-fitting tests have.

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto not installed")

# The spawned quarto R session runs library(apaTables) against the INSTALLED
# package, not this source tree -- see helper-stale-library.R. A green test
# rendered against a stale library certifies the library, not the branch.
#
# DO NOT REMOVE THIS GUARD. Without it, editing the filter in inst/ and
# re-running gives `PASS 7` here against the unmutated installed copy, while
# test-docx-structure.R -- which has the guard -- correctly refuses to run:
# the file reads green while testing something other than the branch.
apa_skip_if_library_stale()

test_that("a rotated table's caption sits INSIDE its own landscape section", {
  d   <- apa_render_fixture_docx("landscape-caption-run")
  doc <- apa_docx_document_xml(d)

  # Position, not presence. The caption is two paragraphs; the one that
  # carries the title text is styled `Caption`. A `w:pStyle` reference is
  # never split across runs, unlike body text, which Word breaks at
  # formatting boundaries (the false RED that bit the note assertions).
  first_caption  <- regexpr('w:pStyle w:val="Caption"', doc, fixed = TRUE)
  first_portrait <- regexpr("<w:sectPr>", doc, fixed = TRUE)

  expect_gt(first_caption, 0)
  expect_gt(first_portrait, 0)
  # The portrait closer must come FIRST, so the caption falls in the section
  # the closer opens rather than the portrait one it ends.
  expect_lt(first_portrait, first_caption)
})

test_that("consecutive rotated tables share ONE landscape section", {
  d   <- apa_render_fixture_docx("landscape-caption-run")
  doc <- apa_docx_document_xml(d)

  # Two rotated tables and one upright. Merged, the run contributes exactly
  # one closer of each kind; unmerged it contributes two of each. Counting is
  # the whole assertion -- the defect IS an extra pair, and the extra pair is
  # what opens the blank page between the two tables.
  land <- gregexpr('w:orient="landscape"', doc, fixed = TRUE)[[1]]
  expect_equal(length(land), 1L)

  # And the run contributes exactly TWO section closers in total -- the
  # portrait one that opens it and the landscape one that ends it. Asserted
  # separately from the landscape count because a merge that dropped the
  # wrong closer could still leave one landscape closer standing.
  #
  # THE EXPECTED TOTAL IS 3, NOT 2: every apaquarto .docx also carries the
  # reference document's own trailing sectPr, which is not ours and is not
  # removable (measured -- do not "correct" this to 1). Without the merge
  # the same fixture gives 5: two closers per rotated table plus that
  # trailing one.
  expect_equal(length(apa_docx_sections(doc)), 3L)

  # BOTH rotated tables' notes stay inside the merged section, and only the
  # upright table's note follows it. Counts from the fixture's design: two
  # correlation tables with a two-part generated note (body plus the
  # significance-star sentence) and one 1-way table with a one-part note.
  #
  # This pins the property apatablelandscapenote.lua exists for -- a rotated
  # table's note belongs with its table, not on the portrait page after it --
  # restated for a merged section, where per-table closers no longer exist to
  # measure against.
  #
  # IT DOES NOT TEST THE FILTER ORDER, and an earlier version of this comment
  # claimed it did. Reversing the registration of this filter and
  # apatablelandscapenote.lua leaves the entire suite green, and rendering
  # both ways shows why: the two documents differ only in where a zero-width
  # bookmarkEnd sits. A note the note filter fails to relocate is already
  # inside the landscape section, because merging deleted the closer that
  # would have separated them. The order is a real interaction and not a
  # critical one; the filter's own banner records the measurement.
  note_at <- gregexpr("FigureNote", doc, fixed = TRUE)[[1]]
  land_at <- gregexpr('w:orient="landscape"', doc, fixed = TRUE)[[1]]
  expect_equal(sum(note_at < land_at[1]), 4L)
  expect_equal(sum(note_at > land_at[1]), 1L)
})

test_that("an upright table ENDS the run and keeps its own layout", {
  d   <- apa_render_fixture_docx("landscape-caption-run")
  doc <- apa_docx_document_xml(d)

  # The upright table's caption is the LAST one and sits AFTER the run's
  # landscape closer -- i.e. back in the document's portrait default. If the
  # merge ever swallowed a non-rotated float, this is what would move.
  land_at <- gregexpr('w:orient="landscape"', doc, fixed = TRUE)[[1]]
  caps    <- gregexpr('w:pStyle w:val="Caption"', doc, fixed = TRUE)[[1]]

  # Three captions: two rotated tables and the upright one. A weak count on
  # its own -- it holds whatever the sections do -- so it is here only to
  # catch a fixture that has silently stopped emitting a table, which would
  # make the position check below vacuous.
  expect_equal(length(caps), 3L)
  expect_gt(caps[length(caps)], land_at[length(land_at)])
})

# ---------------------------------------------------------------------------
# THIS TEST EXISTS BECAUSE A MUTATION SURVIVED. Deleting merge_runs()'s
# adjacency check -- the loop that refuses to merge two rotated floats with
# anything but page breaks between them -- left BOTH test files completely
# green. Every fixture until now put its upright table LAST, so no rotated
# float ever followed one and the branch never ran.
#
# What the branch prevents is not cosmetic: without it, a rotated float
# separated from the next rotated float by an UPRIGHT one still merges, which
# strips the closers on either side of that upright table and leaves it INSIDE
# the landscape section -- printing an upright table rotated.
test_that("an upright table BETWEEN two rotated ones is not swallowed by the merge", {
  d   <- apa_render_fixture_docx("landscape-caption-interleaved")
  doc <- apa_docx_document_xml(d)

  # The two rotated tables are not consecutive, so they are two runs of one
  # and each keeps its own pair of closers: 2 landscape + 2 portrait, plus
  # the reference document's own trailing sectPr.
  land <- gregexpr('w:orient="landscape"', doc, fixed = TRUE)[[1]]
  expect_equal(length(land), 2L)
  expect_equal(length(apa_docx_sections(doc)), 5L)

  # And the upright table sits BETWEEN the two landscape sections, in the
  # portrait stretch that separates them -- not inside either one. Its
  # caption is the second of the three.
  caps <- gregexpr('w:pStyle w:val="Caption"', doc, fixed = TRUE)[[1]]
  expect_equal(length(caps), 3L)
  expect_gt(caps[2], land[1])
  expect_lt(caps[2], land[2])
})
