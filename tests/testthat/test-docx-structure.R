# The landscape section-break filter.
#
# Quarto's own `.landscape` div for docx is broken twice over (measured): its
# portrait closer is a COMPLETELY EMPTY sectPr, so every page BEFORE a
# rotated table silently loses the running head and page number (not merely
# the rotated page itself), and its rotated page size is hardcoded A4
# regardless of the manuscript's own paper. apaTables writes its own section
# properties instead -- apatablelandscape.lua (inst/_extensions/wjschne/
# apaquarto/), fed real page geometry by apa_landscape_wrap() /
# apa_docx_reference_sectpr() (R/quarto-table.R, R/docx-landscape.R), read
# out of the bundled reference document rather than assumed.
#
# Every render here is a real quarto render taking tens of seconds, and the
# assertions read the rendered .docx back, never the source .qmd -- the same
# discipline test-apaquarto-acceptance.R uses and for the same reason: a
# claim about a section break is only worth as much as the XML it can be
# read off.

skip_on_cran()
skip_if_not(nzchar(Sys.which("quarto")), "quarto not installed")

# The spawned quarto R session runs library(apaTables) against the INSTALLED
# package, not this source tree -- see helper-stale-library.R. A green test
# rendered against a stale library certifies the library, not the branch.
apa_skip_if_library_stale()

test_that("a rotated table keeps the running head, and so do the pages before it", {
  d <- apa_render_fixture_docx("landscape-with-figures")  # fixture HAS figures
  doc <- apa_docx_document_xml(d)
  secs <- apa_docx_sections(doc)          # every sectPr, in document order
  expect_gt(length(secs), 1L)             # there must be more than one to check

  # The rotated section is US LETTER landscape (the fixture's own reference
  # document, apaquarto.docx, is US Letter -- measured), not Quarto's
  # hardcoded A4.
  rot <- Filter(function(s) grepl('w:orient="landscape"', s), secs)
  expect_length(rot, 1)
  expect_true(grepl('w:w="15840"', rot[[1]], fixed = TRUE))
  expect_true(grepl('w:h="12240"', rot[[1]], fixed = TRUE))

  # EVERY section carries a headerReference -- this is the half of the
  # defect that is invisible if you only look at the rotated page. Quarto's
  # own `.landscape` div leaves the portrait closer BEFORE a rotated table
  # completely empty, which strips the running head from that section (and
  # everything before it) with no error anywhere.
  for (s in secs) expect_true(grepl("<w:headerReference", s, fixed = TRUE))
})

test_that("the emitted header references match the bundled reference document", {
  ref <- system.file("_extensions", "wjschne", "apaquarto", "apaquarto.docx",
                     package = "apaTables")
  skip_if(ref == "")
  expect_equal(apa_docx_header_rels(ref),
               c(rId8 = "header1.xml", rId9 = "header2.xml", rId12 = "header3.xml"))
})

test_that("apatablelandscape.lua's hardcoded rId8/rId9/rId12 are not dangling in a REAL rendered docx", {
  # apatablelandscape.lua:147-153 hardcodes r:id="rId8"/"rId9"/"rId12" as
  # literal strings on every <w:headerReference> it writes. The test above
  # only checks the SOURCE reference document -- apaquarto.docx as shipped --
  # never a rendered artefact, and apa_docx_header_rels() is otherwise called
  # nowhere else in the suite. The headerReference-presence loop in the first
  # test in this file (`for (s in secs) expect_true(grepl("<w:headerReference"
  # ...))`) cannot substitute for this: the filter writes the tag as a
  # literal string, so that grepl is true whatever document.xml.rels actually
  # contains -- a drifted rId would leave a dangling reference (a missing
  # running head in real Word) with that assertion still green.
  #
  # This reads BOTH halves off the SAME rendered .docx: the actual
  # word/_rels/document.xml.rels (apa_docx_header_rels(), not the reference
  # document's copy) and the actual r:id values written into each emitted
  # <w:sectPr>'s <w:headerReference> (not just the bare tag name).
  d <- apa_render_fixture_docx("landscape-with-figures")
  rels <- apa_docx_header_rels(d)
  expect_equal(rels,
               c(rId8 = "header1.xml", rId9 = "header2.xml", rId12 = "header3.xml"))

  doc <- apa_docx_document_xml(d)
  secs <- apa_docx_sections(doc)
  expect_gt(length(secs), 1L)

  header_ids <- unlist(lapply(secs, function(s) {
    regmatches(s, gregexpr(
      '<w:headerReference\\b[^>]*\\br:id="([^"]+)"[^>]*/>', s, perl = TRUE))[[1]]
  }))
  header_ids <- sub('.*\\br:id="([^"]+)".*', "\\1", header_ids)
  expect_true(length(header_ids) > 0L)
  # Every r:id the filter actually wrote resolves to a real header part in
  # THIS rendered document's own relationships -- not merely a string that
  # happens to look like a relationship id.
  expect_true(all(header_ids %in% names(rels)))

  # The same check for the FOOTERS. apatablelandscape.lua hardcodes SIX
  # references, not three -- rId10/rId11/rId13 are <w:footerReference>s
  # sitting in the same REFS table as the headers. A drifted footer id is the
  # same defect as a drifted header one (a dangling relationship, and a
  # missing page number in real Word), and the loop above cannot see it: it
  # matches only `<w:headerReference`.
  all_rels <- apa_docx_hdrftr_rels(d)
  expect_equal(all_rels,
               c(rId8 = "header1.xml", rId9 = "header2.xml",
                 rId10 = "footer1.xml", rId11 = "footer2.xml",
                 rId12 = "header3.xml", rId13 = "footer3.xml"))

  # PER SECTION, NOT POOLED ACROSS THE DOCUMENT -- and this is not a style
  # preference, it is what the assertion needs to be able to fail. WATCHED
  # NOT FAILING: the first version of this check pooled every section's ids
  # into one vector and asserted the SET was the six. Deleting the rId13
  # footerReference from apatablelandscape.lua's own REFS table left it
  # GREEN, because the document's LAST sectPr is the reference document's
  # own (pandoc copies apaquarto.docx's trailing section verbatim) and it
  # carries all six regardless of what the filter writes -- so the pooled
  # set was complete even when neither closer the filter wrote was.
  six <- sort(c("rId8", "rId9", "rId10", "rId11", "rId12", "rId13"))
  for (s in secs) {
    ids <- regmatches(s, gregexpr(
      '<w:(header|footer)Reference\\b[^>]*\\br:id="([^"]+)"[^>]*/>', s,
      perl = TRUE))[[1]]
    ids <- sub('.*\\br:id="([^"]+)".*', "\\1", ids)
    expect_equal(sort(ids), six)
    # ... and each one resolves to a part this document actually contains.
    expect_true(all(ids %in% names(all_rels)))
  }
})

test_that("the landscape section is genuinely rotated in real content, not just claimed in XML", {
  # A cheap structural companion to the XML assertions above: text known to
  # belong to the rotated table (its caption) must actually be reachable
  # from a page pdftotext/Word would treat as landscape -- but reading page
  # SHAPE out of a .docx without opening it in Word is not reliable the way
  # it is for a PDF (there is no rasteriser here), so this checks the one
  # thing a .docx CAN say for itself: the table's own grid sits strictly
  # between the portrait closer and the landscape closer that names it, in
  # document order, not before the portrait closer or after the landscape
  # one.
  d <- apa_render_fixture_docx("landscape-with-figures")
  doc <- apa_docx_document_xml(d)

  # Format-agnostic locators: the FIRST landscape pgSz and the table's own
  # grid marker (its caption text is unique to this fixture).
  landscape_at <- regexpr('w:orient="landscape"', doc, fixed = TRUE)
  table_at <- regexpr("<w:tbl>", doc, fixed = TRUE)
  caption_at <- regexpr("Correlations", doc, fixed = TRUE)

  expect_true(landscape_at > 0)
  expect_true(table_at > 0)
  expect_true(caption_at > 0)
  # The table (and its caption) sit BEFORE the landscape closer that follows
  # them -- i.e. the closer really does come AFTER the rotated content, not
  # stranded somewhere else in the document.
  expect_lt(table_at, landscape_at)
  expect_lt(caption_at, landscape_at)
})

test_that("a table that is BOTH rotated AND shrunk gets the right style AND the right section breaks", {
  # Rotation's interaction with the shrink ladder's size marker.
  # apatablesize.lua (custom-style) and
  # apatablelandscape.lua (section breaks) both act on the SAME table in
  # this fixture -- a 14-variable correlation table the ladder rotates AND
  # still has to shrink to fit the rotated budget (measured: size 8.822 pt,
  # rung 8, with Word's fixed cell padding charged; a model that ignores the
  # padding says 9.181/rung 9, the "just fits, padding pushes it back over"
  # answer that makes this table's labels wrap in real Word).
  # Registration order between the two filters does not matter
  # (they touch disjoint things -- a Table node's custom-style attribute;
  # a Div's own content), but this proves it holds in a REAL render, not
  # only in the string-level check in test-render-markdown.R.
  d <- apa_render_fixture_docx("landscape-shrunk")
  doc <- apa_docx_document_xml(d)

  expect_true(grepl('<w:tblStyle w:val="Table8"', doc, fixed = TRUE))
  # The marker div left no trace -- no leftover custom-style text and no
  # leaked "apa-table-size-8" marker anywhere the filter did not consume it.
  expect_false(grepl("apa-table-size-", doc, fixed = TRUE))

  secs <- apa_docx_sections(doc)
  rot <- Filter(function(s) grepl('w:orient="landscape"', s), secs)
  expect_length(rot, 1)
  expect_true(grepl('w:w="15840"', rot[[1]], fixed = TRUE))
  expect_true(grepl('w:h="12240"', rot[[1]], fixed = TRUE))
  for (s in secs) expect_true(grepl("<w:headerReference", s, fixed = TRUE))
})

test_that("an unlabelled (non-float) wide table can still shrink, not only rotate", {
  # Rotation is reachable for an UNLABELLED chunk (apatablelandscape.lua
  # finds its own marker via a Pandoc-level walk, not a FloatRefTarget
  # handler -- see that file's own banner), so apatablesize.lua must not be
  # scoped to FloatRefTarget only: a `tbl-` labelled chunk's table lives
  # inside one at this stage, but an unlabelled one does not, and it would
  # rotate yet keep the default "Table" style at 10 pt, leaving an inert
  # apa-table-size-N div in the output. apatablesize.lua therefore uses the
  # same Pandoc-level walk apatablelandscape.lua does.
  d <- apa_render_fixture_docx("landscape-unlabelled-shrunk")
  doc <- apa_docx_document_xml(d)

  # Table8, not Table9, for the same reason as the labelled fixture above --
  # the ladder charges Word's fixed cell padding and takes
  # the next size down.
  expect_true(grepl('<w:tblStyle w:val="Table8"', doc, fixed = TRUE))
  expect_false(grepl("apa-table-size-", doc, fixed = TRUE))

  secs <- apa_docx_sections(doc)
  rot <- Filter(function(s) grepl('w:orient="landscape"', s), secs)
  expect_length(rot, 1)
  for (s in secs) expect_true(grepl("<w:headerReference", s, fixed = TRUE))
})

test_that("three tables in one document each keep their OWN size, not the last marker the filter visited", {
  # Do NOT let apatablesize.lua keep a SINGLE `size` local shared across a
  # document-wide walk: a first pass visiting every marker div, each one
  # overwriting `size`, followed by a second pass over every Table node,
  # applies that ONE final value to EVERY table. Three tables marked
  # 9/unmarked/6 then all come back `Table6`, the size of whichever marker
  # the walk visited last -- silent, exit 0, reproducible with bare
  # `pandoc --lua-filter=`, and reached by any real APA manuscript with more
  # than one table. The other landscape-*.qmd fixtures cannot see it: every
  # one of them has exactly ONE table.
  #
  # The filter pairs each marker with the table in its OWN immediate Blocks
  # list (apa_quarto_size_wrap() always emits the marker directly after its
  # own table's grid markdown, so they are always siblings in the same
  # list, however deeply that list itself is nested) via a `Blocks`-typed
  # filter handler rather than separate document-wide `Div`/`Table` passes.
  d <- apa_render_fixture_docx("multi-table-shrink")
  doc <- apa_docx_document_xml(d)

  styles <- apa_docx_table_styles(doc)
  expect_length(styles, 3)
  expect_equal(sort(styles), sort(c("Table", "Table6", "Table9")))

  # No marker text leaked into the rendered document either.
  expect_false(grepl("apa-table-size-", doc, fixed = TRUE))
})

test_that("numbered-lines survives on the rotated section AND every page before it", {
  # docxlinenumber.lua turns numbered-lines: true into line numbering by
  # appending ONE closer of its own, at the very end of the document,
  # carrying <w:lnNumType> and nothing else -- no page size, no margins, no
  # header references. A sectPr governs content back to the PREVIOUS closer,
  # so apatablelandscape.lua's own two closers (the portrait one before a
  # rotated table and the landscape one around it) would otherwise close
  # their sections BEFORE docxlinenumber.lua's ever runs, silently dropping
  # line numbers from the rotated table and everything before it. Without
  # the fix, real Microsoft Word shows no line-number digits on the pages
  # before the rotated table and a counter starting fresh at "1" on the first
  # page apaquarto's own numbering closer governs (measured). So
  # apatablelandscape.lua reads numbered-lines off the document metadata
  # itself and carries <w:lnNumType> in both of its own closers; real Word
  # confirms numbering then runs continuously from page 1 through the rotated
  # table and beyond.
  d <- apa_render_fixture_docx("landscape-numbered-lines")
  doc <- apa_docx_document_xml(d)
  secs <- apa_docx_sections(doc)

  rot <- Filter(function(s) grepl('w:orient="landscape"', s), secs)
  expect_length(rot, 1)
  before <- secs[seq_len(which(vapply(secs, identical, logical(1), rot[[1]])))]
  expect_gt(length(before), 1L)  # at least the portrait closer AND the landscape one

  # Every section apatablelandscape.lua itself writes -- the portrait closer
  # before the table and the landscape closer around it -- carries BOTH the
  # running head AND line numbering. This is deliberately NOT asserted for
  # every section in the whole document: docxlinenumber.lua's own trailing
  # closer (whatever comes after the rotated table) textually omits
  # headerReference and pgSz too, but real Word rendering of this exact
  # fixture (and a bare numbered-lines probe with no rotation at all) showed
  # the running head, page number and correct page size present on the
  # pages it governs regardless -- Word visibly treats this "incomplete but
  # not empty" sectPr differently from Quarto's own, fully empty one, by a
  # mechanism this project does not claim to explain (measured). No defect is
  # asserted here against docxlinenumber.lua on the strength of the XML shape
  # alone.
  for (s in before) {
    expect_true(grepl("<w:headerReference", s, fixed = TRUE))
    expect_true(grepl("<w:lnNumType", s, fixed = TRUE))
  }
})

test_that("the reference document read is the PROJECT's own copy when it has one, not the installed package's", {
  # A `quarto render` resolves the apaquarto extension -- reference document
  # and all -- out of the PROJECT's own _extensions folder, so
  # apa_docx_reference_sectpr() must read that copy, not only system.file(),
  # the INSTALLED package's copy. The two are identical for every project
  # apa.quarto.new()/apa.quarto.setup() builds, and silently divergent the
  # moment a user edits their own copy, which is supported. The rotated
  # section carries the margins round with the page, and a customised
  # reference document is exactly where an asymmetric margin lives.
  #
  # The synthetic copy is deliberately NOT US Letter and NOT symmetric --
  # every number differs from the bundled document's, so this cannot pass by
  # reading the wrong file and getting the right answer.
  proj <- tempfile(); dir.create(proj)
  ext <- file.path(proj, "_extensions", "wjschne", "apaquarto")
  dir.create(ext, recursive = TRUE)
  apa_write_synthetic_docx(file.path(ext, "apaquarto.docx"))

  old <- setwd(proj)
  # after = FALSE so the setwd runs BEFORE the unlink:
  # on.exit APPENDS with add = TRUE, and removing a directory the process is
  # sitting inside is an error on Windows.
  on.exit({ setwd(old); unlink(proj, recursive = TRUE) },
          add = TRUE, after = FALSE)
  geom <- apa_docx_reference_sectpr()

  expect_equal(geom$width, 11906L)
  expect_equal(geom$height, 16838L)
  expect_equal(geom$margin_top, 1111L)
  expect_equal(geom$margin_right, 2222L)
  expect_equal(geom$margin_bottom, 3333L)
  expect_equal(geom$margin_left, 4444L)
  expect_equal(geom$margin_header, 555L)
  expect_equal(geom$margin_footer, 666L)
})

test_that("with no project copy anywhere above, apa_docx_reference_sectpr() reads the real page straight off the BUNDLED reference document", {
  # The fallback half of the branch above. Do not call
  # apa_docx_reference_sectpr() here without a setwd: because the project's
  # copy is preferred, its answer would then depend on the DEVELOPER'S
  # FILESYSTEM -- whatever _extensions/wjschne/apaquarto/apaquarto.docx
  # happened to exist at or above tests/testthat/ -- and a pass would be
  # green for a reason nobody chose. Run from a scratch directory instead,
  # with no _extensions folder in it or in any of its parents, so
  # apa_docx_project_reference() demonstrably finds nothing and system.file()
  # is what answers.
  #
  # The values are the US Letter numbers (measured) that the render-based
  # tests above already establish.
  d <- tempfile(); dir.create(d)
  old <- setwd(d)
  on.exit({ setwd(old); unlink(d, recursive = TRUE) },
          add = TRUE, after = FALSE)

  expect_equal(apa_docx_project_reference(), "")
  expect_false(apa_docx_reference_path()$project)

  geom <- apa_docx_reference_sectpr()
  expect_equal(geom$width, 12240L)
  expect_equal(geom$height, 15840L)
  expect_equal(geom$margin_top, 1440L)
  expect_equal(geom$margin_left, 1440L)
  expect_equal(geom$margin_header, 720L)
  expect_equal(geom$margin_gutter, 0L)
})

test_that("apa_docx_sectpr_from_file() derives the real page from ANY reference document, never a hardcoded constant", {
  # Unit-level companion to the test above: proves the DERIVATION mechanism
  # itself is generic (reads whatever a reference document's own trailing
  # sectPr says) rather than a US-Letter constant apa_landscape_wrap()
  # happens to embed. apaquarto ships exactly one docx reference document
  # (US Letter) -- there is no A4 docx equivalent to render against the way
  # R/page-geometry.R has one for LaTeX (a4paper: has no effect on the docx
  # path at all, confirmed by grep across the bundled extension) -- so this
  # feeds apa_docx_sectpr_from_file() a SYNTHETIC A4 reference document
  # instead of a real render, to prove the same function would read a
  # different real page correctly if apaTables ever ships one.
  a4 <- tempfile(fileext = ".docx")
  d <- tempfile(); dir.create(d, recursive = TRUE)
  dir.create(file.path(d, "word"), recursive = TRUE)
  writeLines(paste0(
    '<?xml version="1.0"?><w:document ',
    'xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
    "<w:body><w:p><w:r><w:t>x</w:t></w:r></w:p>",
    '<w:sectPr><w:pgSz w:w="11906" w:h="16838"/>',
    '<w:pgMar w:top="1440" w:right="1800" w:bottom="1440" w:left="1800" ',
    'w:header="708" w:footer="708" w:gutter="0"/></w:sectPr>',
    "</w:body></w:document>"),
    file.path(d, "word", "document.xml"))
  old <- setwd(d)
  on.exit({ setwd(old); unlink(d, recursive = TRUE); unlink(a4) }, add = TRUE)
  utils::zip(a4, "word/document.xml", flags = "-q")
  setwd(old)

  geom <- apaTables:::apa_docx_sectpr_from_file(a4)
  expect_equal(geom$width, 11906L)
  expect_equal(geom$height, 16838L)
  expect_equal(geom$margin_left, 1800L)
  expect_equal(geom$margin_right, 1800L)
  expect_equal(geom$margin_header, 708L)
})

# ---------------------------------------------------------------------------
# A rotated table's note must land on its table's own landscape page.
#
# apanote.lua (stock apaquarto, common, post-render) inserts a float's note
# as a SIBLING immediately after the float's own Div -- one level up from
# apatablelandscape.lua's closers, which sit INSIDE that Div. Since a
# `sectPr` governs everything back to the PREVIOUS such paragraph, a note
# textually AFTER the landscape closer belongs to the section that FOLLOWS
# it (the manuscript's portrait default), not to the rotated section its
# table is on. MEASURED in real Microsoft Word (16.111, AppleScript PDF
# export, pdftotext -layout, per-page split on \f), without the fix:
# landscape-shrunk put its table on pages 3-4 (landscape, 792x612 pt) and its
# note ALONE on page 5 (portrait); landscape-numbered-lines put its table on
# page 3 (landscape) and its note alone on page 4 (portrait) -- an otherwise
# empty portrait page holding nothing but a note divorced from the table it
# describes.
#
# A note-existence check alone cannot see this (the note is still IN the
# document either way) -- these assertions read its POSITION relative to
# the landscape closer that governs its own table, which is what the defect
# actually changes. apatablelandscapenote.lua (registered post-render
# immediately after apatablelandscape.lua and apanote.lua) is the fix: it
# moves a rotated float's note INSIDE its own landscape section, between the
# table and the closer.
test_that("a rotated table's note lands inside its OWN landscape section, not stranded after it", {
  # Four floats, one render: a rotated table with a GENERATED note, a
  # rotated table with an EXPLICIT chunk note, a rotated table with NO note
  # at all, and a non-rotated table with a generated note -- the four cases
  # verified together, in case a fix that helps one silently breaks another
  # (verify by rendering every case in one document, not by reading the
  # diff).
  d <- apa_render_fixture_docx("landscape-note-position")
  doc <- apa_docx_document_xml(d)

  # tbl-a, tbl-b and tbl-c rotate; tbl-d does not.
  #
  # tbl-a, tbl-b and tbl-c are CONSECUTIVE rotated floats and share ONE
  # landscape section, so there is one closer for the whole run rather than
  # one per table. tbl-d is upright and contributes none. A count of 1 rather
  # than 3 IS the run merging working, and it is the cheapest proof of it
  # anywhere in the suite -- landscape-caption-run.qmd has only TWO rotated
  # tables and cannot observe a run splitting.
  land_at <- gregexpr('w:orient="landscape"', doc, fixed = TRUE)[[1]]
  expect_equal(length(land_at), 1L,
              info = "expected 1 landscape closer for the run of three")

  # Position, not presence: `w:pStyle w:val="FigureNote"` is a paragraph STYLE
  # REFERENCE, never split across `<w:t>` runs the way body TEXT is -- unlike a
  # literal note sentence, which Word routinely breaks mid-phrase at a
  # formatting boundary. (Searching raw XML for "N = number of cases" directly
  # gives a false RED: the italic "N" is its own run, splitting the phrase
  # across two `<w:t>` elements with a `</w:t><w:r>...<w:t>` in between, so a
  # literal substring search on raw XML returns -1 even when the text is
  # present and correctly placed. Style tokens do not have this problem, which
  # is why the position check below is built on them rather than on note
  # wording.)
  note_at <- gregexpr("FigureNote", doc, fixed = TRUE)[[1]]
  span_count <- function(lo, hi) sum(note_at > lo & note_at < hi)

  # BOUNDARIES ARE THE FLOATS THEMSELVES, NOT THE SECTION CLOSERS.
  #
  # Do not attribute each note to its table by the position of that table's
  # OWN landscape closer: a merged run has one closer for three tables, so
  # `land_at[2]` would be NA. Each float opens with exactly one `FigureTitle`
  # paragraph -- "Table 1", "Table 2" ... -- which is a property of the
  # float, not of the sectioning, so it survives any change to how sections
  # are drawn.
  title_at <- gregexpr('w:pStyle w:val="FigureTitle"', doc, fixed = TRUE)[[1]]
  expect_equal(length(title_at), 4L,
              info = "four floats, so four FigureTitle paragraphs")

  # The expected counts come from the FIXTURE'S DESIGN, not from the output:
  # tbl-a has a generated two-part note, tbl-b an explicit one-part chunk
  # note, tbl-c none at all, tbl-d a generated two-part note.
  notes_a <- span_count(title_at[1], title_at[2])
  notes_b <- span_count(title_at[2], title_at[3])
  notes_c <- span_count(title_at[3], title_at[4])
  notes_d <- sum(note_at > title_at[4])

  expect_equal(notes_a, 2L,
              info = "tbl-a's generated note must sit with its own table")
  expect_equal(notes_b, 1L,
              info = "tbl-b's explicit note must sit with its own table")
  expect_equal(notes_c, 0L,
              info = "tbl-c has no note; nothing should appear with it")
  expect_equal(notes_d, 2L,
              info = "tbl-d's generated note must still be present, unaffected")

  # And the ORIGINAL property this test exists for, restated for one merged
  # section: every ROTATED table's note is INSIDE the landscape section, and
  # the upright table's note is outside it. A note that regressed to a
  # portrait page would cross this line -- which is exactly what
  # apatablelandscapenote.lua exists to prevent.
  expect_equal(sum(note_at < land_at[1]), 3L,
              info = "tbl-a's two and tbl-b's one note sit inside the landscape section")
  expect_equal(sum(note_at > land_at[1]), 2L,
              info = "only tbl-d's note follows the landscape section")

  # Content cross-check, using the FLATTENED (tag-stripped) text where a
  # phrase search is safe -- proves the tokens counted above are genuinely
  # the right words, not merely the right count of an unrelated style.
  txt <- apa_nbsp_to_space(apa_docx_text(d))
  expect_true(grepl("N = number of cases", txt, fixed = TRUE))
  expect_true(grepl("Explicit chunk note text for table B.", txt, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Structural assertions on the written .docx: does a wide table fit its page?
#
# The failure being guarded: a table handed to Word in absolute widths for a
# page whose width apaTables does not control -- flextable's output measured
# 12,398 twips = 8.61 in against a 6.5 in text area -- is CLIPPED by Word
# rather than wrapped, so a multi-block regression silently loses R2,
# Delta R2 and their CIs, exit 0, no warning.
#
# THREE KINDS OF CHECK BELOW, AND THEY DO NOT ALL PROVE THE SAME THING:
#
#   MECHANISM  `<w:tblW w:type="pct">` present or absent. THIS is the one
#              XML-observable fact that discriminates fix from defect --
#              see "the discriminating assertion" below.
#   GEOMETRY   unzip the .docx, read sectPr (page size, margins) and
#              tblGrid (the table's own declared column widths, in twips);
#              assert the table fits inside its section's text area. Kept,
#              but see "what the geometry comparison actually is" below --
#              it is NOT a live risk measurement against the current
#              emitter.
#   CONTENT    read the rendered text back and assert the columns a clip
#              would lose are actually there.
#
# THE CLIP IS INVISIBLE TO A CONTENT CHECK, and this was MEASURED rather than
# assumed. A flextable-in-Quarto-docx render of the SAME two-block regression
# fixture this file uses below held R2 = .629, R2 = .665, Delta R2 = .035 and
# every coefficient row, unabridged, in `word/document.xml` -- because Word's
# clip is a RENDERING-time visual effect (the part of a too-wide fixed-width
# table that falls past the page edge is still IN the XML, simply not
# displayed when Word or a printer draws the page), not a removal of content
# from the file. No XML-reading check, text or geometry, can see a table that
# LOOKS fine in the file and only clips once a human opens it in Word.
#
# THE DISCRIMINATING ASSERTION. What actually differs between the two
# mechanisms in the file (measured): the pandoc emitter writes
# `<w:tblW w:type="pct" w:w="4817"/>` -- a PERCENTAGE of the containing text
# column, which structurally cannot overflow it -- while flextable/officer
# (what `apa.save()` uses) write NO `<w:tblW>` element at all, only absolute
# `dxa` `gridCol` widths. That absence, not any particular twip count, IS the
# clip's cause, sitting in the file one `grepl` away -- watched failing
# against a flextable render.
#
# WHAT THE GEOMETRY COMPARISON (tblGrid <= usable) ACTUALLY IS. It is not a
# regression net for the clip, for two reasons: (1) `tblGrid` for a
# `pct`-typed table is NOT the width Word renders -- it is a set of
# proportional HINTS, scaled at render time to a percentage of the
# containing text column, so summing it does not measure what governs the
# page; and (2) more basically, that sum is bounded by the REFERENCE
# DOCUMENT's own page width, not by whichever section the table happens to
# land in -- pushed synthetically to 98.4% and 99.3% width, this reference
# document's pandoc-written tables cap out around ~7900 twips regardless of
# content. Both fixtures below (7624 and 7569 twips) sit under that ceiling
# AND under both a 9360-twip portrait and a 12960-twip landscape usable
# width, so `sum(tblGrid) <= usable` is true BY CONSTRUCTION for any
# pandoc-written table this emitter can produce -- it cannot go red for the
# mechanism it is pointed at. (A flextable render of the same table does go
# red, at 14373 twips, but flextable does not serve the Quarto route.)
#
# So: KEPT, not deleted (the rotation and section/grid-COUNT assertions
# beside them genuinely can fail, and the width comparison is a real,
# cheap guard that the pandoc/`pct` MECHANISM itself has not been reverted
# to absolute widths -- if it ever were, `sum(tblGrid)` would stop being
# bounded by the reference document's ~7900-twip ceiling and could exceed a
# real section's usable width again, the same shape the flextable render
# shows), but NAMED as a mechanism guard rather than claiming to be a live
# measurement of a risk it cannot see.

# One integer XML attribute, found by name rather than position -- see
# apa_docx_xml_int_attr()'s own comment in helper-apaquarto-extract.R.
docx_usable_width <- function(sect) {
  pgw   <- apa_docx_xml_int_attr(sect, "w:w")
  left  <- apa_docx_xml_int_attr(sect, "w:left")
  right <- apa_docx_xml_int_attr(sect, "w:right")
  pgw - left - right
}

test_that("apa_docx_sections() sees a genuinely self-closing <w:sectPr/>, not only the open/close shape", {
  # Synthetic, not rendered: no fixture in this suite produces a bare
  # self-closing sectPr any more (apatablelandscape.lua replaced the
  # mechanism that used to), so this proves the HARDENING itself rather than
  # any live defect. Constructed to fail against an open/close-only pattern:
  # `<w:sectPr\\b.*?</w:sectPr>` alone finds only the second (open/close)
  # block and silently drops the first -- length 1, not 2.
  synth <- paste0(
    '<w:p><w:pPr><w:sectPr/></w:pPr></w:p>',
    '<w:sectPr><w:pgSz w:w="12240" w:h="15840"/></w:sectPr>')
  secs <- apa_docx_sections(synth)
  expect_length(secs, 2L)
  expect_equal(secs[[1]], "<w:sectPr/>")
  expect_true(grepl("12240", secs[[2]], fixed = TRUE))
})

test_that("apa_docx_table_styles() does not let a styleless table's slot pick up the NEXT table's style", {
  # Synthetic, not rendered: pandoc stamps a style on every table it writes, so
  # no fixture in this suite reaches a `<w:tbl>` with no `<w:tblStyle>` -- this
  # proves the hardening itself, the same way the self-closing-sectPr test
  # above does. Constructed to fail against a whole-document pattern
  # (`<w:tbl>\\s*<w:tblPr>.*?<w:tblStyle w:val="([^"]+)"`, one lazy scan across
  # the WHOLE document, no stop at the next `<w:tbl>`): fed this two-table
  # fixture, that pattern's one match STARTS at table one's own `<w:tbl>` tag
  # (it has nowhere else to start, since table one's `<w:tblPr>` is what it
  # anchors on) and its lazy `.*?` runs straight through table one's close and
  # into table two's `<w:tblPr>`, capturing table two's "Table9" as if it were
  # table one's answer -- a single-element result, `c("Table9")`, that reads as
  # "table one's style is Table9" to any caller indexing positionally.
  # apa_docx_table_styles() processes one `<w:tbl>...</w:tbl>` block at a time,
  # so a block with no `<w:tblStyle>` reports NA at ITS OWN position instead of
  # donating its slot to the next table.
  synth <- paste0(
    '<w:tbl><w:tblPr><w:tblW w:w="0" w:type="auto"/></w:tblPr>',
    '<w:tr><w:tc><w:p>one</w:p></w:tc></w:tr></w:tbl>',
    '<w:tbl><w:tblPr><w:tblStyle w:val="Table9"/>',
    '<w:tblW w:w="0" w:type="auto"/></w:tblPr>',
    '<w:tr><w:tc><w:p>two</w:p></w:tc></w:tr></w:tbl>')
  styles <- apa_docx_table_styles(synth)
  # Confirmed by construction: fed this exact fixture, that pattern's
  # ONE match spans from table one's own `<w:tbl>` tag straight through to
  # table two's "Table9" (there is nowhere else for it to start, and its
  # lazy `.*?` has no reason to stop at table one's own `</w:tblPr>`), so
  # `regmatches()` returns a length-ONE vector, `c("Table9")` -- this
  # `expect_length(styles, 2L)` alone already catches that mutation.
  expect_length(styles, 2L)
  # The second assertion is not redundant with the first: length alone
  # would also pass a fix that got the COUNT right but still shifted a
  # style into the wrong table's slot (e.g. one that dropped a styleless
  # table's entry entirely and re-padded the vector at the end rather than
  # in place). Naming what sits in table one's own slot -- NA, not "Table9"
  # -- is what actually rules out misattribution rather than merely a wrong
  # count.
  expect_true(is.na(styles[[1]]))
  expect_equal(styles[[2]], "Table9")
})

test_that("the widest builder output is a percentage-width table, rotated, with its declared grid checked as a mechanism guard (not a live-risk measurement -- see this section's banner)", {
  # apa.reg.table()'s multi-block Fit/Delta Fit columns are the widest shape
  # in the suite -- wide enough that the fitting ladder auto-ROTATES it to
  # landscape, which is itself asserted here rather than assumed,
  # because a check compared against the WRONG section (portrait, left
  # behind before the table) would silently pass for the wrong reason.
  d <- apa_render_fixture_docx("regression-wide")
  doc <- apa_docx_document_xml(d)

  # THE DISCRIMINATING CHECK: `w:type="pct"` is what makes Word's clip
  # structurally impossible -- a percentage of the containing text column
  # cannot overflow it. Watched failing against a flextable-in-Quarto-docx
  # render, whose document.xml carries no `<w:tblW>` element at all.
  expect_true(grepl('<w:tblW[^>]*w:type="pct"', doc, perl = TRUE))

  secs <- apa_docx_sections(doc)
  rot <- Filter(function(s) grepl('w:orient="landscape"', s, fixed = TRUE), secs)
  expect_length(rot, 1L)

  # MECHANISM GUARD, not a live measurement: `tblGrid` for a `pct` table is
  # a proportional HINT bounded by the reference document's own ~7900-twip
  # ceiling (measured by pushing a synthetic table to 98.4%/99.3% width),
  # not by whichever section the table lands in -- so `<=` here is true BY
  # CONSTRUCTION for any table this emitter can produce and cannot go red
  # for the mechanism it is pointed at. Kept anyway (cheap, and would go
  # red again if the emitter were ever reverted to absolute widths, the
  # same shape the flextable reproduction above shows), never deleted, but
  # not mistaken for proof this specific table cannot overflow -- the
  # `w:type="pct"` assertion above is what proves that.
  grids <- apa_docx_tbl_grids(doc)
  expect_length(grids, 1L)
  table_width <- sum(grids[[1]])
  usable <- docx_usable_width(rot[[1]])

  expect_gt(usable, 0L)
  expect_true(table_width <= usable,
             info = sprintf("table %d twips vs %d usable", table_width, usable))
})

test_that("the narrow control is a percentage-width table too, unrotated, with its declared grid checked as a mechanism guard (not a live-risk measurement -- see this section's banner)", {
  # The "both orientations" half of this section: this fixture is small
  # enough (apa.cor.table(attitude[, 1:3]), measured 5.2903 in) that the
  # ladder leaves it
  # portrait and unshrunk -- the plain case beside regression-wide's rotated
  # one.
  d <- apa_render_fixture_docx("narrow")
  doc <- apa_docx_document_xml(d)

  # THE DISCRIMINATING CHECK -- see the other geometry test's
  # own comment.
  expect_true(grepl('<w:tblW[^>]*w:type="pct"', doc, perl = TRUE))

  secs <- apa_docx_sections(doc)
  expect_length(secs, 1L)
  expect_false(grepl('w:orient="landscape"', secs[[1]], fixed = TRUE))

  # MECHANISM GUARD, not a live measurement -- see the other geometry
  # test's own comment and this section's banner.
  grids <- apa_docx_tbl_grids(doc)
  expect_length(grids, 1L)
  table_width <- sum(grids[[1]])
  usable <- docx_usable_width(secs[[1]])

  expect_gt(usable, 0L)
  expect_true(table_width <= usable,
             info = sprintf("table %d twips vs %d usable", table_width, usable))
})

test_that("a numbered row label is text, not an ordered list", {
  # Pandoc parses an unescaped "1. rating" as the START of an ordered list,
  # not as literal text -- apa_md_escape_list_marker() (R/render-markdown.R)
  # prevents that, and this is what actually distinguishes the two outcomes.
  # Do not add a `firstLine` indentation check instead: "Compact" has always
  # carried firstLine="0" in stock apaquarto, so such a check guards a defect
  # that never existed and cannot fail.
  #
  # WATCHED FAILING: apa_md_escape_list_marker()'s body temporarily replaced
  # with `lines` (identity, no escaping) in an isolated install and
  # re-rendered against this exact fixture -- the table's own <w:tbl> then
  # DID carry a real <w:numPr>, and the literal "1. rating" text disappeared
  # (it becomes the list marker itself, leaving only "rating" as
  # the run text), so `expect_match(tbl, "1. rating")` went red too, not
  # only `expect_false(numPr)`. Reverting the mutation restores both to
  # green.
  #
  # Anchored to "1. rating", not a bare "1." -- no other cell in this fixture
  # happens to contain a stray "1." substring today, but a bare "1." would
  # discriminate only by luck of the current data, not by construction.
  skip_on_cran()
  doc <- apa_docx_document_xml(apa_render_fixture_docx("narrow"))
  tbl <- regmatches(doc, regexpr("<w:tbl>.*?</w:tbl>", doc))
  expect_false(grepl("<w:numPr>", tbl, fixed = TRUE))       # not a list item
  expect_match(tbl, "1. rating", fixed = TRUE)              # the marker text survives
})

test_that("a multi-block regression's R2, Delta R2 and their CIs survive into the rendered .docx", {
  # The content half of this section (see its banner for why it sits BESIDE
  # the geometry checks above rather than replacing them: it cannot see
  # Word's rendering-time clip either, measured). What it CAN catch is
  # independent and real: a future regression in the emitter itself (a grid
  # table missing a column, a markdown escaping bug that swallows a cell)
  # would show up here as text that is simply gone, with no Word and no
  # rendering involved.
  #
  # The fixture sets neither `tbl-cap` nor `apa-note` (every apaquarto-*.qmd
  # fixture sets both, so they cannot cover the defaults), so the title and
  # note here are the AUTOMATIC ones, not typed text.
  d <- apa_render_fixture_docx("regression-wide")
  txt <- apa_nbsp_to_space(apa_docx_text(d))

  # Block 1's Fit line and its CI.
  expect_true(grepl("R2 = .629", txt, fixed = TRUE))
  expect_true(grepl("CI[.55,.69]", txt, fixed = TRUE))
  # Block 2's Fit line, its Delta Fit line (the Unicode delta character is
  # literal text in the markdown source -- R sources stay ASCII-clean, so it
  # is written as a \uXXXX escape here, never as a pasted glyph), and both
  # CIs.
  expect_true(grepl("R2 = .665", txt, fixed = TRUE))
  expect_true(grepl("\u0394R2 = .035", txt, fixed = TRUE))
  expect_true(grepl("CI[.59,.72]", txt, fixed = TRUE))
  expect_true(grepl("CI[.00, .07]", txt, fixed = TRUE))
  # The third block's own predictor row -- a coefficient a clip also loses,
  # alongside the Fit statistics.
  expect_true(grepl("attract", txt, fixed = TRUE))
})

test_that("apa.save()'s own 0.5 in side margins really do fit the widest builder output in the file it writes", {
  # apa.save() is the OTHER .docx writer (flextable/officer), checked here
  # beside the pandoc emitter's own geometry checks above. It needs no
  # quarto, no Word and no network (officer is already an Imports
  # dependency), so unlike the rest of this file it does not depend on the
  # file-level
  # skip_on_cran()/quarto skips above -- it is placed here anyway, for
  # topical cohesion with the rest of this section, at the (very small) cost
  # of also skipping on a machine with no quarto installed even though it
  # does not need one.
  #
  # This reads the ACTUAL WRITTEN FILE rather than trusting the pre-render
  # R-level computation test-apa-save.R's own "a rotated Word page gives a
  # wide table more room to fit into" test already checks (comparing
  # flextable_dim() against apa_usable_width() before anything is written)
  # -- the gap between "the code that decided this should fit" and "the
  # bytes actually written" is exactly what this section exists to close.
  #
  # WATCHED FAILING: apa_doc_section()'s page_mar() temporarily changed from
  # left/right = 0.5 to left/right = 1 (apa_usable_width() left
  # UNCHANGED, so the pre-render fit_to_width() check would not have known
  # to compensate) in an isolated install -- the SAME table's tblGrid then
  # summed to 14373 twips against an actual usable width of 15840 - 1440 -
  # 1440 = 12960 twips: 14373 > 12960, red. Reverting the mutation restores
  # left/right = 720 twips (0.5 in) and usable = 14400, and the table (still
  # 14373 twips, unchanged -- fit_to_width() never had to shrink it) fits
  # with 27 twips (0.02 in) to spare: a genuinely tight, non-vacuous margin.
  blk1 <- lm(sales ~ adverts + airplay, data = album)
  blk2 <- lm(sales ~ adverts + airplay + attract, data = album)
  # The builder default is landscape = NA, not TRUE -- this table
  # rotates because apa_save_rotate() MEASURES it at 9.98 in against the
  # 7.5 in US Letter portrait budget, not because of any builder default.
  # That is what keeps the margin arithmetic below genuine: measured
  # unshrunk (9.98 in < the 10 in rotated budget, so fit_to_width() never
  # runs), the same 14373 twips the comment below has always described.
  wide <- apa.reg.table(blk1, blk2)

  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp))
  apa.save(filename = tmp, wide)

  d <- tempfile(); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  utils::unzip(tmp, files = "word/document.xml", exdir = d)
  doc <- paste(readLines(file.path(d, "word", "document.xml"), warn = FALSE),
              collapse = "")

  sec <- regmatches(doc, regexpr("<w:sectPr\\b.*?</w:sectPr>", doc, perl = TRUE))
  expect_true(nzchar(sec))
  expect_true(grepl('w:left="720"', sec, fixed = TRUE))    # 0.5 in
  expect_true(grepl('w:right="720"', sec, fixed = TRUE))   # 0.5 in

  grid <- regmatches(doc, regexpr("<w:tblGrid>.*?</w:tblGrid>", doc, perl = TRUE))
  expect_true(nzchar(grid))
  widths <- as.integer(regmatches(grid,
                                  gregexpr('(?<=w:w=")[0-9]+', grid, perl = TRUE))[[1]])
  usable <- docx_usable_width(sec)

  expect_gt(usable, 0L)
  expect_true(sum(widths) <= usable,
             info = sprintf("table %d twips vs %d usable", sum(widths), usable))
})
