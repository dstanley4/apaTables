# apa.quarto.pdf()'s title/note arguments and the empty metadata div that
# carries the generated (or overridden) values to the apatablemeta.lua
# filter. Precedence over a chunk's tbl-cap/apa-note is NOT decided here --
# the filter resolves that later by populating only what the float still
# lacks -- so nothing below constructs a chunk option at all.

test_that("the meta div carries the generated title and note", {
  ct  <- apa.cor.table(attitude)
  div <- apa_meta_div_for(ct, title = NULL, note = NULL)
  expect_match(div, "apa-table-meta", fixed = TRUE)
  expect_match(div, "Descriptive Statistics and Correlations", fixed = TRUE)
  expect_match(div, "*N* = number of cases", fixed = TRUE)   # markup preserved
})

test_that("an argument overrides the generated value", {
  ct  <- apa.cor.table(attitude)
  div <- apa_meta_div_for(ct, title = "My Own Title", note = "My own note.")
  expect_match(div, "My Own Title", fixed = TRUE)
  expect_match(div, "My own note.", fixed = TRUE)
  expect_false(grepl("Descriptive Statistics and Correlations", div, fixed = TRUE))
})

test_that("the div's raw text survives a pandoc round-trip", {
  # NARROW BY DESIGN. This CANNOT observe that attribute values are escaped
  # so the div still parses: replace apa_attr_escape() with the identity and
  # it stays GREEN. Pandoc degrades to a Para on the unescaped quote, and
  # both greps below still match substrings of the mangled text. It is a
  # smoke check -- the literal content round-trips through pandoc without a
  # fatal error -- under a name that claims only that.
  #
  # Escaping itself is covered by two tests that were watched failing under
  # that same mutation: "apa_attr_escape() escapes backslash before quote" and
  # "apa.quarto.pdf() with title/note args still parses as a Div under
  # pandoc".
  #
  # A note legitimately contains double quotes and backslashes. The YAML form
  # of this can break a render (\* is an invalid YAML escape); a pandoc div
  # attribute has different rules, which is why it needs its own coverage.
  div <- apa_meta_div(title_md = 'A "quoted" title',
                      note_md  = 'back\\slash and "quotes"')
  skip_if(Sys.which("pandoc") == "", "pandoc not available")
  parsed <- system2("pandoc", c("-f", "markdown", "-t", "native"),
                    input = div, stdout = TRUE)
  expect_true(any(grepl("apa-table-meta", parsed, fixed = TRUE)))
  expect_true(any(grepl("quoted", parsed, fixed = TRUE)))
})

test_that("richtext is accepted as well as a string", {
  ct  <- apa.cor.table(attitude)
  div <- apa_meta_div_for(ct, title = richtext(list(rt_run("R", italic = TRUE),
                                                    rt_run(" Results"))),
                          note = NULL)
  expect_match(div, "*R* Results", fixed = TRUE)
})

test_that("apa_attr_escape() emits NO backslash-escaped quote, of either kind", {
  # This is the whole contract, and it is a data-loss guard rather than a
  # style rule. A backslash-escaped quote in a div attribute is correct pandoc
  # but does not survive Quarto: measured end to end, when the title holds an
  # ODD number of escaped quotes and the note does too, every ` = ` in the
  # title collapses to `=` on the rendered page, silently, in both placements.
  # `title = "Cronbach's Alpha = ..."` with `note = "Cohen's d ..."` is one
  # apostrophe in each and was measured corrupting. Character references
  # remove the parity that triggers it; pandoc decodes them when it reads the
  # attribute, so nothing is lost.
  escaped <- apa_attr_escape("back\\slash, 'single' and \"double\"")
  expect_identical(escaped,
                   "back\\\\slash, &#39;single&#39; and &#34;double&#34;")
  # Stated as the invariant itself, so a future edit that reintroduces either
  # escape fails here and not on someone's manuscript.
  expect_false(grepl("\\'", escaped, fixed = TRUE))
  expect_false(grepl("\\\"", escaped, fixed = TRUE))

  # The backslash pass is unrelated and still required: a generated note
  # carries the markdown escape \*, and \ is pandoc's escape character inside
  # a quoted attribute value, so it must be doubled to arrive as one.
  expect_identical(apa_attr_escape("\\* star"), "\\\\* star")

  # ...and because an entity holds neither a backslash nor a quote, the three
  # passes do not interact at all. An ordering hazard would be invisible
  # except on an input carrying both characters; this asserts the hazard is
  # absent rather than that the order is right. It calls the real function
  # and pins its output -- comparing hand-written gsub() compositions to each
  # other would exercise only base R's gsub(), never apa_attr_escape().
  both <- "\\ and ' and \""
  expect_identical(apa_attr_escape(both), "\\\\ and &#39; and &#34;")
})

test_that("both quote characters survive the meta div into pandoc's AST", {
  # The round trip that matters: what the filter will be handed. Asserted on
  # the PARSED value, not on the emitted string, because the emitted string is
  # what the previous two versions of this code got right while the parsed
  # value was wrong.
  div <- apa_meta_div(title_md = 'A "quoted" title with an apostrophe: Cohen\'s',
                      note_md  = "x = y")
  expect_false(grepl("\\'", div, fixed = TRUE))
  expect_false(grepl('\\"', div, fixed = TRUE))
  skip_if(Sys.which("pandoc") == "", "pandoc not available")
  json <- paste(system2("pandoc", c("-f", "markdown", "-t", "json"),
                        input = div, stdout = TRUE), collapse = "")
  # pandoc's JSON escapes a double quote as \" -- this is the value coming
  # back decoded, apostrophe and double quote both intact.
  expect_true(grepl('A \\"quoted\\" title with an apostrophe: Cohen\'s', json,
                    fixed = TRUE))
})

test_that("apa_note_attr() emits the separator apaquarto splits on", {
  # One part: wrapped, no separator -- make_note() strips the wrapper and the
  # part comes back whole.
  expect_identical(apa_note_attr("only part"), '["only part"]')
  # Two parts: the literal three characters `","` between them, which is what
  # utilsapa.lua's make_note() splits on. A space-joined value has no
  # separator at all and renders as ONE paragraph -- the defect this fixes.
  expect_identical(apa_note_attr(c("first.", "second.")),
                   '["first.","second."]')
  # No notes at all stays empty, so apatablemeta-decide.lua's M.decide() leaves
  # the float alone rather than setting an empty note on it.
  expect_identical(apa_note_attr(character(0)), "")
})

test_that("a `\",\"` inside a note part is NOT guarded, and this says so", {
  # A KNOWN, DOCUMENTED LIMITATION, asserted so that nobody re-adds a guard
  # believing it works. An earlier version substituted &quot; here and its
  # comment claimed that made the separator safe. It did not: pandoc's
  # attribute reader decodes character references BEFORE make_note() splits,
  # so the entity arrives as a plain double quote and forges a separator
  # exactly as a literal one would.
  #
  # The previous test for this asserted only the R-side string and so could
  # not have observed the failure. This one asks pandoc, which is where the
  # decoding actually happens.
  skip_if(Sys.which("pandoc") == "", "pandoc not available")
  div  <- apa_meta_div(title_md = "T", note_md = apa_note_attr(c('a","b', "c")))
  json <- paste(system2("pandoc", c("-f", "markdown", "-t", "json"),
                        input = div, stdout = TRUE), collapse = "")
  # Two parts went in, so this function wrote ONE separator. The value pandoc
  # hands the filter holds TWO -- the one written plus the one the part forged
  # -- so make_note() emits three paragraphs from two note entries.
  m <- regmatches(json, gregexpr('\\\\",\\\\"', json))[[1]]
  expect_equal(length(m), 2L)
  # The control: with no double quote in the part, the same call yields the
  # one separator that was intended. Without this, the count above would pass
  # just as happily on a value that had lost a separator instead of gaining
  # one.
  clean <- paste(system2("pandoc", c("-f", "markdown", "-t", "json"),
                         input = apa_meta_div("T", apa_note_attr(c("a", "c"))),
                         stdout = TRUE), collapse = "")
  expect_equal(length(regmatches(clean, gregexpr('\\\\",\\\\"', clean))[[1]]), 1L)
})

test_that("no builder puts a double quote in a note", {
  # The limitation in the test above is unreachable from the generated path,
  # which is why it is documented rather than defended. Checked here against
  # the real builders, so a future note that introduces a double quote fails
  # this and gets a decision made about it.
  op <- options(contrasts = c("contr.sum", "contr.poly"))
  on.exit(options(op), add = TRUE)
  viagra <- NULL
  utils::data("viagra", package = "apaTables", envir = environment())
  builders <- list(
    apa.cor.table(attitude),
    apa.reg.table(stats::lm(rating ~ complaints + privileges, data = attitude)),
    apa.d.table(iv = dose, dv = libido, data = viagra),
    apa.aov.table(stats::lm(libido ~ dose, data = viagra)))
  for (tb in builders) {
    notes <- vapply(as_spec(tb)$notes, rt_to_markdown, character(1))
    expect_false(any(grepl('"', notes, fixed = TRUE)), info = paste(notes, collapse = " | "))
  }
})

test_that("apa_meta_div_for() falls back to the spec when args are NULL", {
  ct <- apa.cor.table(attitude)
  div_default <- apa_meta_div_for(ct)
  div_explicit_null <- apa_meta_div_for(ct, title = NULL, note = NULL)
  expect_identical(div_default, div_explicit_null)
})

test_that("apa.quarto.pdf() emits the meta div INSIDE the same asis_output as the table, with a blank line before it", {
  # Hazard: the div must land in the SAME chunk output as the longtable, or
  # the apatablemeta.lua filter -- which walks float.content -- never sees
  # it; and a blank line must separate the raw LaTeX block from the div, or
  # pandoc swallows the ::: fence as part of the raw LaTeX block instead of
  # parsing it as a Div (measured at the post-quarto stage).
  op <- options(knitr.in.progress = TRUE)
  old_fmt <- knitr::opts_knit$get("out.format")
  knitr::opts_knit$set(out.format = "latex")
  on.exit({ options(op); knitr::opts_knit$set(out.format = old_fmt) }, add = TRUE)
  expect_true(knitr::is_latex_output())

  out <- apa.quarto.pdf(apa.cor.table(attitude))
  expect_true(inherits(out, "knit_asis"))
  txt <- as.character(out)

  end_idx <- regexpr("\\end{longtable}", txt, fixed = TRUE)
  expect_gt(end_idx, 0)
  after <- substring(txt, end_idx + nchar("\\end{longtable}"))
  expect_true(startsWith(after, "\n\n"))
  expect_match(after, "apa-table-meta", fixed = TRUE)

  # And the combined string really does parse as a RawBlock followed by a
  # Div carrying the class -- not merely "the substring is present somewhere",
  # which a corrupted, unparseable fence could also satisfy.
  skip_if(Sys.which("pandoc") == "", "pandoc not available")
  parsed <- system2("pandoc", c("-f", "markdown", "-t", "native"),
                    input = txt, stdout = TRUE)
  expect_true(any(grepl("RawBlock", parsed, fixed = TRUE)))
  expect_true(any(grepl('"apa-table-meta"', parsed, fixed = TRUE)))
})

test_that("apa.quarto.pdf() with title/note args still parses as a Div under pandoc", {
  op <- options(knitr.in.progress = TRUE)
  old_fmt <- knitr::opts_knit$get("out.format")
  knitr::opts_knit$set(out.format = "latex")
  on.exit({ options(op); knitr::opts_knit$set(out.format = old_fmt) }, add = TRUE)

  out <- apa.quarto.pdf(apa.cor.table(attitude),
                        title = "A Title With \"Quotes\" and \\backslash",
                        note = "A note.")
  txt <- as.character(out)
  expect_match(txt, "A Title With", fixed = TRUE)
  skip_if(Sys.which("pandoc") == "", "pandoc not available")
  parsed <- system2("pandoc", c("-f", "markdown", "-t", "native"),
                    input = txt, stdout = TRUE)
  expect_true(any(grepl('"apa-table-meta"', parsed, fixed = TRUE)))
})

test_that("apa.quarto.html() off the LaTeX path honours title/note, with no flextable fallback", {
  # apa.quarto.html() dispatches to apa_quarto_markdown_block(), which
  # honours title/note and never returns a flextable at all. A flextable
  # fallback off the LaTeX path would silently IGNORE title/note -- this
  # guards against one returning.
  local_mocked_bindings(apa_latex_target = function() FALSE)
  out <- apa.quarto.html(apa.cor.table(attitude), title = "My Title",
                         note = "My note.")
  expect_false(inherits(out, "flextable"))
  txt <- as.character(out)
  expect_match(txt, "My Title", fixed = TRUE)
  expect_match(txt, "My note.", fixed = TRUE)
})
