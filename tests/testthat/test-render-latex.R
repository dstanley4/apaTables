# OLD_FLEXTABLE_FLOOR is 5 pt: the hard floor the deleted flextable fitter
# (APA_MIN_FONT_SIZE, gone in Task 10) stopped at. It is kept here as a
# LITERAL, not imported, because the assertions below are about walking PAST
# it -- a reference point in the package's history, not a value the package
# still holds.
OLD_FLEXTABLE_FLOOR <- 5

test_that("rt_to_latex styles runs", {
  expect_equal(rt_to_latex(richtext("M")), "M")
  expect_equal(rt_to_latex(richtext(list(rt_run("M", italic = TRUE)))),
               "\\textit{M}")
  expect_equal(rt_to_latex(richtext(list(rt_run("2", script = "super")))),
               "\\textsuperscript{2}")
  expect_equal(rt_to_latex(richtext(list(rt_run("p", script = "sub")))),
               "\\textsubscript{p}")
})

test_that("apa_latex_escape covers every character that has broken a render", {
  # an underscore is math-mode and ABORTS the render (landscape spike)
  expect_equal(apa_latex_escape("a_b"), "a\\_b")
  for (ch in c("&", "%", "$", "#", "{", "}")) {
    expect_match(apa_latex_escape(paste0("x", ch)), "\\\\", fixed = FALSE)
  }
  expect_equal(apa_latex_escape("~"), "\\textasciitilde{}")
  expect_equal(apa_latex_escape("^"), "\\textasciicircum{}")
  expect_equal(apa_latex_escape("\\"), "\\textbackslash{}")
})

# A REAL BUILDER SPEC, carrying each awkward name, all the way through
# apa_display_plan() -> apa_latex_table().
#
# What existed before Phase 2 Task 11 was a unit assertion on
# apa_latex_escape("a_b") and nothing else: no spec containing one of these
# names had ever been emitted, so every layer between the escaper and the
# document was uncovered. The escaper can be perfect and the table still
# wrong -- the emitter interposes the newline collapse, the leading-bracket
# guard, the \multicolumn specs and the row assembly, and only one of those
# calls apa_latex_escape() at all.
#
# `tricky_names` and `tricky_latex` are shared with the spec/.docx sweep and
# the end-to-end render (helper-special-names.R). The expectation is a
# hand-written literal, never apa_latex_escape() applied to the input: an
# expectation computed by the function under test is satisfied by a stub that
# returns its argument unchanged.
test_that("every awkward variable name reaches the LaTeX emitter correctly escaped", {
  for (case_name in names(tricky_names)) {
    nm <- tricky_names[[case_name]]
    want <- tricky_latex[[case_name]]
    tex <- apa_latex_table(
      apa_display_plan(as_spec(apa.cor.table(tricky_cor_df(nm)))), 10)

    expect_true(grepl(want, tex, fixed = TRUE),
                info = paste0(case_name, ": expected '", want,
                              "' in the emitted LaTeX"))

    # And the RAW name must be gone wherever escaping changes it. This is the
    # half that fails when the escaping is stubbed out: the presence check
    # above would still pass on raw output for several characters (an
    # unescaped "v_one" contains "v_one"), so on its own it is decorative.
    if (!identical(want, nm)) {
      expect_false(grepl(nm, tex, fixed = TRUE),
                   info = paste0(case_name, ": the raw name '", nm,
                                 "' survived into the LaTeX unescaped"))
    }
  }
})

# THE SAME NAMES, BUT INSIDE A MACRO ARGUMENT -- which is where a blank line
# actually aborts the render.
#
# Fix round 1. The sweep above uses apa.cor.table(), which produces only
# span-1 cells, so nothing in it ever reached a MOVING ARGUMENT. That matters
# because the hazard is not the cell, it is the argument: a blank line inside a
# plain tabular cell compiles cleanly (measured -- an \halign entry is
# restricted horizontal mode, where \par does nothing), while the same blank
# line inside a non-\long macro argument gives
#
#   ! Paragraph ended before \text@command was complete.
#   ! Extra }, or forgotten \endgroup.
#
# This emitter puts cell text into exactly two such arguments, and both are
# covered below:
#
#   \multicolumn                every spanner label and every one of its
#                               level labels -- REACHABLE BY USER TEXT TODAY,
#                               via apa.2way.table(), whose second factor's
#                               name and levels are spanners.
#   \multicolumn + \textbf      every row-group label. No shipped builder
#                               routes user text here today (apa.reg.table's
#                               labels are the generated "Model 1", "Model 2"),
#                               so it is exercised through a hand-built spec
#                               rather than left uncovered on the strength of
#                               that -- a builder that starts labelling groups
#                               with a variable name must not be the thing that
#                               discovers this.
#
# The blank-line assertion is made on the WHOLE emitted table, not on the
# spanner lines: what aborts is a blank line anywhere in an argument, and an
# emitter that only squeezed runs of newlines would still leave "a\n \n b" --
# a blank line by TeX's rules, since the line between is whitespace.
p2_no_blank_line <- function(tex) {
  !any(trimws(strsplit(tex, "\n", fixed = TRUE)[[1]]) == "")
}

test_that("an awkward name inside a \\multicolumn spanner survives, blank line and all", {
  lv <- unname(tricky_names)
  set.seed(1)
  d <- expand.grid(a = factor(sprintf("Ra%02d", 1:3)),
                   b = factor(lv, levels = lv), rep = 1:3)
  d$y <- stats::rnorm(nrow(d), 10, 2)
  # The second factor's own NAME goes into a spanner too, so it carries an
  # underscore -- the character that aborts a render outright.
  names(d)[2] <- "v_two"
  tex <- apa_latex_table(
    apa_display_plan(as_spec(
      apa.2way.table(iv1 = a, iv2 = `v_two`, dv = y, data = d))), 10)

  mc <- grep("\\multicolumn", strsplit(tex, "\n", fixed = TRUE)[[1]],
             fixed = TRUE, value = TRUE)
  # Without this the loop below could pass on a table that stopped using
  # \multicolumn at all, which is the same shape of mistake as an absence
  # check that can never fire.
  expect_true(length(mc) > 0L)
  expect_true(any(grepl("{v\\_two}", mc, fixed = TRUE)),
              info = "the spanner's own name did not reach a \\multicolumn")

  for (case_name in names(tricky_names)) {
    want <- tricky_latex[[case_name]]
    expect_true(any(grepl(want, mc, fixed = TRUE)),
                info = paste0(case_name, ": expected '", want,
                              "' inside a \\multicolumn cell"))
  }
  # The two-newline name is the crash case, and this is the assertion that
  # sees it: uncollapsed, "two\n\nbreaks" puts a literal blank line inside
  # \multicolumn{2}{...}{...}.
  expect_true(p2_no_blank_line(tex),
              info = "a blank line reached the emitted table")
})

test_that("an awkward name inside a \\textbf row-group label survives", {
  # Hand-built because no shipped builder routes user text into a row-group
  # label yet. \multicolumn{n}{...}{\textbf{label}} is two nested non-\long
  # arguments, which is the harshest position in the emitter.
  spec <- apa_spec_start(2L)
  spec <- apa_col(spec, label = "Variable", align = "l",
                  values = list("row one", "row two"))
  spec <- apa_col(spec, label = "M", align = "r", values = list("1.00", "2.00"))
  for (case_name in names(tricky_names)) {
    s <- apa_row_group(spec, label = tricky_names[[case_name]], rows = 1:2)
    tex <- apa_latex_table(apa_display_plan(apa_spec_finish(s)), 10)
    want <- tricky_latex[[case_name]]
    expect_true(grepl(paste0("\\textbf{", want, "}"), tex, fixed = TRUE),
                info = paste0(case_name, ": expected '\\textbf{", want,
                              "}' in the emitted LaTeX"))
    expect_true(p2_no_blank_line(tex),
                info = paste0(case_name, ": a blank line reached the emitted ",
                              "table through a row-group label"))
  }
})

test_that("every apa_sym() symbol emits a LaTeX macro, not a raw glyph", {
  syms <- c("N","M","SD","b","beta","sr2","r","df","SS","MS","F","p",
            "eta2p","R2","dR2","d","MSE","eta2g","epsilon","df_num","df_den")
  for (s in syms) {
    out <- rt_to_latex(apa_sym(s))
    # A literal UTF-8 beta SILENTLY VANISHED from the spike's PDF.
    expect_false(grepl("[^\x01-\x7f]", out),
                 info = paste("non-ASCII glyph survived in", s))
  }
  expect_match(rt_to_latex(apa_sym("beta")), "beta")
  expect_match(rt_to_latex(apa_sym("dR2")),  "Delta")
})

test_that("every apa_sym() symbol emits its EXACT expected LaTeX (fix round 1)", {
  # Fix round 1: the two checks above (non-ASCII absence, substring match)
  # are both TRUE of "\textit{\textbackslash{}ensuremath\{\textbackslash{}beta\}}"
  # -- the corrupted output produced when symbol substitution and character
  # escaping are applied as two sequential passes over the same text (the
  # second pass re-escapes the backslashes/braces the first one introduced).
  # That string is pure ASCII and contains the substring "beta", so both
  # weaker assertions above pass on garbage. Only an exact match on the
  # fully rendered LaTeX catches it.
  expected <- list(
    N       = "\\textit{N}",
    M       = "\\textit{M}",
    SD      = "\\textit{SD}",
    b       = "\\textit{b}",
    beta    = "\\textit{\\ensuremath{\\beta}}",
    sr2     = "\\textit{sr}\\textsuperscript{2}",
    r       = "\\textit{r}",
    df      = "\\textit{df}",
    SS      = "\\textit{SS}",
    MS      = "\\textit{MS}",
    F       = "\\textit{F}",
    p       = "\\textit{p}",
    eta2p   = "\\textit{\\ensuremath{\\eta}}\\textsuperscript{2}\\textsubscript{p}",
    R2      = "\\textit{R}\\textsuperscript{2}",
    dR2     = "\\ensuremath{\\Delta}\\textit{R}\\textsuperscript{2}",
    d       = "\\textit{d}",
    MSE     = "\\textit{MSE}",
    eta2g   = "\\textit{\\ensuremath{\\eta}}\\textsuperscript{2}\\textsubscript{g}",
    epsilon = "\\textit{\\ensuremath{\\varepsilon}}",
    df_num  = "\\textit{df}\\textsubscript{Num}",
    df_den  = "\\textit{df}\\textsubscript{Den}"
  )
  for (s in names(expected)) {
    expect_equal(rt_to_latex(apa_sym(s)), expected[[s]], info = s)
  }
})

test_that("unsafe C0 control characters and DEL are stripped, not emitted", {
  # Fix round 1, Critical 2 established that a raw "\x01" typed by a user
  # is not reinterpreted as the escaper's own bookkeeping character (the
  # old "\x01" placeholder is gone). Fix round 2 (Important finding) went
  # further: "not misread as a sentinel" and "safe in LaTeX" are different
  # properties, and only the first was established. Compiling a real .tex
  # file containing a raw U+0001 under pdflatex produced a genuine error
  # ("! LaTeX Error: Unicode character ^^A (U+0001) not set up for use with
  # LaTeX") and under xelatex ("! Text line contains an invalid
  # character") -- both engines, real errors, not a silent pass-through.
  # The same is true of every other C0 control except TAB/LF/CR (see the
  # next test) -- confirmed live for this sample of codepoints, spanning
  # the full C0 range and DEL. U+0000 is excluded from this loop: R
  # character strings cannot hold an embedded NUL byte at all
  # (intToUtf8(0x00) is "", not a one-character string containing NUL), so
  # it can never reach apa_latex_escape() as real input in the first
  # place -- it is still in apa_latex_control_strip for completeness (a
  # copy-pasted table, not a per-codepoint decision), the exclusion here is
  # purely because it is untestable through an R string.
  bad <- c(0x01, 0x02, 0x08, 0x0B, 0x0C, 0x0E, 0x1F, 0x7F)
  for (cp in bad) {
    ch <- intToUtf8(cp)
    out <- apa_latex_escape(paste0("a", ch, "b"))
    expect_false(grepl(ch, out, fixed = TRUE),
                 info = sprintf("U+%04X survived to output", cp))
  }
  expect_equal(apa_latex_escape(paste0("a", intToUtf8(0x01), "b")), "ab")
})

test_that("tab, newline and carriage return survive unchanged (deliberate)", {
  # TAB (U+0009), LF (U+000A) and CR (U+000D) are the three C0 controls
  # deliberately NOT stripped. Compiling a real .tex file with each one
  # embedded mid-cell ("START<char>END") rendered as an ordinary
  # inter-word space under BOTH pdflatex and xelatex, with no error and no
  # "Missing character" warning -- TeX's own catcode table already treats
  # all three as whitespace, so there is no render-safety reason to strip
  # or convert them here. LF/CR are additionally reserved for Task 5's
  # longtable emitter, which is the layer that decides how a stacked
  # CI/p line becomes a LaTeX line break inside \makecell -- converting to
  # a space at this layer would destroy that information before Task 5
  # ever sees it.
  expect_equal(apa_latex_escape("a\tb"), "a\tb")
  expect_equal(apa_latex_escape("a\nb"), "a\nb")
  expect_equal(apa_latex_escape("a\rb"), "a\rb")
})

test_that("apa_latex_escape is documented as NOT safe under double application", {
  # Fix round 1, Important 3: pins the documented invariant (see the
  # Roxygen @section on apa_latex_escape()) so a future refactor that
  # silently changes this behaviour is caught here rather than discovered
  # downstream in Task 5.
  once <- apa_latex_escape("a&b\\c")
  twice <- apa_latex_escape(once)
  expect_false(identical(once, twice))
})

# ---------------------------------------------------------------------------
# Task 4: the width model. dev/specs/2026-07-31-phase2-decisions.md D11
# OVERRIDES the plan's Step 1 brief on this task: the brief's formula omits
# flextable's cell padding entirely (a 25-53% over-estimate, measured) and
# gets the \tabcolsep term wrong (4n, not 4n - 4). See R/render-latex.R's
# block comment above apa_latex_natural_width() for the corrected arithmetic.

test_that("width is linear in size and the two directions agree", {
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude)))
  w12 <- apa_latex_natural_width(plan, 12)
  w6  <- apa_latex_natural_width(plan, 6)
  expect_true(w6 < w12)
  # solving for a width must return a size that produces it
  s <- apa_size_for_width(plan, w6)
  expect_equal(s, 6, tolerance = 0.01)
  # and the invariant apa_size_for_width() is DEFINED by: the size it
  # returns must itself still fit the target under the real (rounded)
  # forward function, not just under the closed-form estimate used to seed
  # the search for it.
  expect_lte(apa_latex_natural_width(plan, s), w6)
})

test_that("the estimate BRACKETS measured LaTeX widths from both sides", {
  # Independent origin: these come from real xelatex renders (Task 1's
  # original 4 tables at 10/12 pt, plus fix rounds 1 and 2's sub-10 pt and
  # interior-8 pt additions), read from LaTeX's own \LT@entry longtable
  # column widths, NOT from the model being tested here -- see
  # dev/factcheck/phase2-preflight.md section 3 and
  # dev/factcheck/phase2-preflight/width6/, width8/.
  #
  # The brief's own calibration test was only `expect_gte(pred, actual_in)`,
  # which an absurdly large prediction (1000 in) also satisfies -- it
  # cannot tell a good model from a useless one. As of fix round 2 (the
  # margin raised to 0.10 in, IMPORTANT 1) the 8 measured entries over-
  # estimate by +0.13 to +0.24 in -- see the block comment above
  # apa_latex_natural_width() in R/render-latex.R for the full derivation.
  # An upper bound of +1.0 in leaves comfortable room above every observed
  # cushion while still catching a grossly inflated model -- see the
  # mutation evidence in the Task 4 report
  # (.superpowers/sdd/.../task-4-report.md), which shows this exact bound
  # firing against the pre-D11 (uncorrected) formula's +1.9 to +2.7 in
  # errors.
  UPPER_MARGIN_IN <- 1.0
  measured <- readRDS(test_path("fixtures", "phase2-measured-widths.rds"))
  for (nm in names(measured)) {
    plan <- apa_display_plan(as_spec(measured[[nm]]$spec))
    pred <- apa_latex_natural_width(plan, measured[[nm]]$size)
    actual <- measured[[nm]]$actual_in
    expect_gte(pred, actual, label = paste0(nm, " pred"))
    expect_lte(pred, actual + UPPER_MARGIN_IN, label = paste0(nm, " pred"))
  }
})

# ---------------------------------------------------------------------------
# Task 4 review, fix round 1. IMPORTANT 1: the font-independent constant
# subtracted before scaling was assumed (flextable's documented padding.left
# + padding.right = 10/72 in) rather than measured, and the assumption was
# wrong -- fitting w_j(s) = c_j*(s/12) + K by linear regression over s in
# {6, 8, 10, 12} gives K = 5/27 in (13.333 pt) EXACTLY, identical across
# every column of every real spec tested, not 10/72 in = 0.1389 in. The
# fixture above gained two sub-10 pt entries (cor_attitude_6, twoway_6,
# real LaTeX `\LT@entry` measurements) so the bracketing test actually
# exercises the range Task 9's ladder shrinks INTO after rotating -- the
# range nothing had checked before this fix.

test_that("the font-independent width constant is the MEASURED regression intercept, not assumed padding", {
  # Regression, not a copy of the shipped constant: fits w_j(s) = c*(s/12) +
  # K independently, over four sizes and two different real specs, and
  # checks the shipped constant against what that regression finds -- so a
  # future edit that quietly reverts apa_latex_padding_in to a plausible-
  # looking but wrong value (10/72, or anything else not re-measured) is
  # caught here rather than only showing up as a sub-10 pt under/over-shoot
  # nobody is looking at.
  sizes <- c(6, 8, 10, 12)
  fit_intercept <- function(spec) {
    plan <- apa_display_plan(as_spec(spec))
    W <- sapply(sizes, function(s) {
      ft <- apa_ft_at_size(apa_flextable_from_plan(plan), s)
      dim(ft)$widths
    })
    apply(W, 1, function(w) coef(lm(w ~ sizes))[[1]])
  }
  for (k in fit_intercept(apa.cor.table(attitude))) {
    expect_equal(k, apa_latex_padding_in, tolerance = 1e-6)
  }
  for (k in fit_intercept(apa.2way.table(iv1 = gender, iv2 = alcohol,
                                          dv = attractiveness, data = goggles))) {
    expect_equal(k, apa_latex_padding_in, tolerance = 1e-6)
  }
  expect_equal(apa_latex_padding_in, 5 / 27, tolerance = 1e-9)
})

test_that("IMPORTANT 2: apa_size_for_width() never returns an absurd size", {
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude)))
  # a target far beyond anything a real table needs must not solve to a
  # font size in the thousands of points -- it must clamp to the ceiling.
  expect_equal(apa_size_for_width(plan, 1000), APA_LATEX_MAX_FONT_SIZE)
  expect_lte(apa_size_for_width(plan, 1000), 12)

  # a table that already comfortably fits at 12 pt must not solve to
  # something ABOVE 12 pt either (the un-rounded linear estimate can
  # overshoot past the reference size when there is room to spare).
  one_col_plan <- apa_display_plan(as_spec(apa.cor.table(attitude[, 1:2])))
  s <- apa_size_for_width(one_col_plan, 1.0)
  expect_lte(s, APA_LATEX_MAX_FONT_SIZE)
})

test_that("there is NO legibility floor: the solver walks past 6, 5 and 1 pt (D7b)", {
  # This test used to assert the OPPOSITE -- that a 1.5 in target returned
  # OLD_FLEXTABLE_FLOOR (5 pt) with a predicted width of 3.03 in, i.e. a table
  # that would be CLIPPED. That was Task 5's degeneracy clamp set at a
  # legibility value, and it made D7b false while the help page and the
  # warning both claimed nothing was ever cut off. The clamp is now a
  # degeneracy guard (APA_LATEX_MIN_EMITTABLE_SIZE, measured), and the solver
  # keeps shrinking.
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude)))
  expect_gt(apa_latex_natural_width(plan, OLD_FLEXTABLE_FLOOR), 1.5)

  # Measured on this plan (10 columns): 3.0 in -> 4.86 pt, 2.0 -> 2.88,
  # 1.5 -> 1.80, 1.0 -> 0.72, 0.8 -> 0.36. Every one of them FITS, which is
  # the whole point -- the old clamp answered 5 pt to all five and clipped
  # four of them.
  for (tg in c(3.0, 2.0, 1.5, 1.0, 0.8)) {
    s <- apa_size_for_width(plan, tg)
    expect_lt(s, OLD_FLEXTABLE_FLOOR, label = paste("size for", tg))
    expect_gte(s, APA_LATEX_MIN_EMITTABLE_SIZE)
    expect_lte(apa_latex_natural_width(plan, s), tg)
  }
  # and it does go below one point when that is what fitting takes
  expect_lt(apa_size_for_width(plan, 1.0), 1)
})

test_that("the invariant now holds at every target a table can be made to fit", {
  # The old docs said it held "above the floor" only, which was true of a
  # floor at 5 pt. Swept, so a single lucky target cannot certify it.
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude)))
  fixed <- (4 * plan$n_col - 4) / 72.27 + APA_LATEX_WIDTH_MARGIN_IN
  floor_w <- 0.01 * plan$n_col + fixed          # narrowest at ANY size
  targets <- seq(1.0, 8.0, length.out = 84)
  fitted <- vapply(targets, function(tg) {
    s <- apa_size_for_width(plan, tg)
    apa_latex_natural_width(plan, s) <= tg
  }, logical(1))
  # every target the table can be made to fit is met ...
  expect_true(all(fitted[targets >= floor_w]))
  # ... and the ones that are not met are exactly the impossible ones
  expect_true(all(targets[!fitted] < floor_w))
})

test_that("a table can be too wide for ANY size, and the solver says so by not fitting", {
  # The one honest limit left in D7b: \tabcolsep is (4n - 4) pt whatever the
  # font, and every column rounds UP to 0.01 in, so enough columns cannot fit
  # a page at any size. This is what the ladder's clipping warning reports.
  sp <- apa_spec_start(1L)
  for (j in seq_len(200L)) sp <- apa_col(sp, sprintf("c%d", j), "r", "0")
  plan <- apa_display_plan(apa_spec_finish(sp, title = "T"))
  s <- apa_size_for_width(plan, 9.0)
  expect_equal(s, APA_LATEX_MIN_EMITTABLE_SIZE)
  expect_gt(apa_latex_natural_width(plan, s), 9.0)
})

# ---------------------------------------------------------------------------
# apa_latex_table() -- the emitter (Task 5)

# The column spec is the ONLY place a vertical rule could be introduced, and
# it cannot be extracted with a lazy regex: it contains nested braces
# (">{\fontsize{10}{12}\selectfont}l"), so "up to the first }" returns a
# fragment. This walks the braces.
colspec_of <- function(tex) {
  open <- "\\begin{longtable}{"
  start <- regexpr(open, tex, fixed = TRUE)
  expect_true(start > 0)
  chars <- strsplit(tex, "", fixed = TRUE)[[1]]
  k <- start + nchar(open)
  depth <- 1L
  out <- character(0)
  while (k <= length(chars)) {
    ch <- chars[k]
    if (ch == "{") depth <- depth + 1L
    if (ch == "}") {
      depth <- depth - 1L
      if (depth == 0L) break
    }
    out <- c(out, ch)
    k <- k + 1L
  }
  paste(out, collapse = "")
}

test_that("the emitted longtable has APA structure and survives deferral", {
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude)))
  tex  <- apa_latex_table(plan, 10)

  expect_match(tex, "\\\\begin\\{longtable\\}")
  expect_match(tex, "\\\\endfirsthead")   # headings repeat: APA requires it
  expect_match(tex, "\\\\endhead")
  expect_match(tex, "\\\\endlastfoot")    # apalongtablenote.lua splices here
  expect_match(tex, "\\\\toprule");  expect_match(tex, "\\\\bottomrule")

  # spacing lives INSIDE the column spec, because endfloat carries only the
  # environment to the .ttt and leaves what precedes it behind (this is B3)
  expect_match(tex, "@\\{\\\\hspace")
  expect_false(grepl("\\\\setlength", tex))

  # APA: no vertical rules. The brief's version of this assertion scanned the
  # WHOLE emitted string, which is wrong twice over: a "|" is legal cell data
  # (Task 3 established it needs no escaping under fontenc=TU), so a variable
  # name containing one would fail this test for a reason that has nothing to
  # do with rules; and the rule itself lives in the column spec, nowhere else.
  expect_false(grepl("|", colspec_of(tex), fixed = TRUE))

  # the emitter writes NO furniture -- apaquarto owns it, and that is what
  # keeps @tbl- cross-references resolving
  expect_false(grepl("\\\\caption", tex))
  expect_false(grepl("\\\\label",   tex))

  # D9: the environment must stay UNSTARRED. Quarto's main.lua only leaves it
  # so if apaquarto splices a caption in; the starred form silently lost 15 of
  # 60 rows in Task 1's measurement. The emitter must never write one itself.
  expect_false(grepl("longtable\\*", tex))
})

test_that("a \\bottomrule is NOT drawn at every page break", {
  tex <- apa_latex_table(apa_display_plan(as_spec(apa.cor.table(attitude))), 10)
  # The brief's extraction for this one -- everything between \endhead and
  # \endlastfoot -- cannot express the assertion it names. In longtable the
  # PAGE-BREAK foot is the chunk ending at \endfoot, and the chunk between
  # \endfoot and \endlastfoot is the LAST foot, which is precisely where
  # \bottomrule has to live (the brief's own Step 3 says so). So that span
  # always contains a \bottomrule in a correct table, and the test could only
  # ever pass on a table missing its bottom rule entirely. Cut at \endfoot.
  lines <- strsplit(tex, "\n", fixed = TRUE)[[1]]
  from <- which(lines == "\\endhead")
  to   <- which(lines == "\\endfoot")
  expect_length(from, 1L)
  expect_length(to, 1L)
  expect_true(to > from)
  page_break_foot <- lines[seq(from + 1L, to)]
  expect_false(any(grepl("bottomrule", page_break_foot)))
  # ... and it IS in the last foot, where apalongtablenote.lua splices the note
  last_foot <- lines[seq(to + 1L, which(lines == "\\endlastfoot"))]
  expect_true(any(grepl("bottomrule", last_foot)))
})

test_that("the page-break foot reserves room for the last foot, invisibly", {
  # longtable subtracts \endfoot's height from the page goal on every page and
  # only then substitutes \endlastfoot -- so an EMPTY \endfoot lets the last
  # foot, which carries \bottomrule and the spliced note, fail to fit where the
  # rows just did. Watched, not reasoned: with this reserve absent, the
  # acceptance matrix's 60-row table ended its rows on page 5 and put a
  # repeated column header, the bottom rule and the note alone on page 6
  # (test-apaquarto-acceptance.R's note_page == last_page went red on exactly
  # that cell).
  tex <- apa_latex_table(apa_display_plan(as_spec(apa.cor.table(attitude))), 10)
  lines <- strsplit(tex, "\n", fixed = TRUE)[[1]]
  from <- which(lines == "\\endhead")
  to   <- which(lines == "\\endfoot")
  page_break_foot <- lines[seq(from + 1L, to)]

  # It is there ...
  expect_true(any(grepl("\\rule{0pt}{", page_break_foot, fixed = TRUE)),
              info = "no reserve in the page-break foot")
  # ... it is INVISIBLE (zero width, so it draws nothing and widens no column,
  # which is what keeps the Task 4 width model describing the same table) ...
  expect_false(any(grepl("bottomrule", page_break_foot)))
  expect_false(any(grepl("midrule", page_break_foot)))
  # ... and it is big enough to matter: four lines of the table's own leading.
  expect_true(any(grepl("{48pt}", page_break_foot, fixed = TRUE)),
              info = "the reserve is not 4 lines at 10 pt")
  expect_true(any(grepl("{24pt}", strsplit(
    apa_latex_table(apa_display_plan(as_spec(apa.cor.table(attitude))), 5),
    "\n", fixed = TRUE)[[1]], fixed = TRUE)),
    info = "the reserve does not scale with the type size")
})

test_that("the table declines apaquarto's automatic closing \\hline (F5)", {
  # apaquarto's template appends an \hline to EVERY longtable at
  # \end{longtable} -- load-bearing for a plain markdown table, whose own
  # \bottomrule does not reach the page there (measured), but a DOUBLED
  # bottom rule on every apaTables table, whose \bottomrule lives in
  # \endlastfoot above the spliced note (F5, Fable pre-push review
  # 2026-08-09; two rule bands 2.4 pt apart at 300 dpi, shipped examples
  # included). The emitter's last row arms the bundled template's
  # cooperation flag; the arming is reached via \csname so the same table
  # is a no-op -- probed, exit 0 -- under a template without the patch
  # (plain format: pdf).
  tex <- apa_latex_table(apa_display_plan(as_spec(apa.cor.table(attitude))), 10)
  lines <- strsplit(tex, "\n", fixed = TRUE)[[1]]
  arm <- "\\noalign{\\csname apatablesownbottomrule\\endcsname}"
  expect_true(arm %in% lines)
  # It must be INSIDE the environment (endfloat carries only the environment
  # to the .ttt, so anything outside it is stripped from a deferred table)
  # and it must be the LAST row, after every body line -- \noalign is legal
  # only between rows, and a row after it would still be above the flag's
  # consumer, but the point of "last" is that no future edit can slip a body
  # row below the arming line and make the shape ambiguous.
  expect_identical(lines[length(lines)], "\\end{longtable}")
  expect_identical(lines[length(lines) - 1L], arm)
  # And the bundled template must actually define the consumer -- the two
  # halves of this fix live in different files and drift is the known
  # failure shape (D74).
  # warn = FALSE: the template ships without a trailing newline, and the
  # "incomplete final line" warning would be a suite WARN (D126) about a
  # property of upstream's file, not of anything under test.
  tpl <- readLines(system.file("_extensions", "wjschne", "apaquarto",
                               "apatemplate.tex", package = "apaTables"),
                   warn = FALSE)
  expect_true(any(grepl("\\def\\apatablesownbottomrule", tpl, fixed = TRUE)))
  expect_true(any(grepl("\\ifapatablesownrule", tpl, fixed = TRUE)))
})

test_that("a cell beginning with [ is guarded", {
  # \\ reads a following [ as its optional argument: "Illegal unit of measure"
  spec <- as_spec(apa.cor.table(attitude))   # CI cells start with [
  tex  <- apa_latex_table(apa_display_plan(spec), 10)
  expect_false(grepl("\\\\\\\\\\s*\\[", tex))
  # and the guard is actually there to be seen, not merely absent-by-luck
  expect_match(tex, "{}[", fixed = TRUE)
})

test_that("a vertical bar in the DATA is not mistaken for a vertical rule", {
  # The positive control for the assertion above: "|" is legal cell content,
  # so the no-rules check must look at the column spec and nowhere else.
  spec <- as_spec(apa.cor.table(attitude))
  spec$body[[1]][[1]] <- richtext("a|b")
  tex <- apa_latex_table(apa_display_plan(spec), 10)
  expect_true(grepl("a|b", tex, fixed = TRUE))
  expect_false(grepl("|", colspec_of(tex), fixed = TRUE))
})

test_that("a blank line inside cell text cannot reach the output", {
  # MEASURED landmine, not theory. Task 3 deliberately passes LF/CR through
  # untouched, and cell text really does arrive with blank lines from CSV and
  # Word paste. Reproduced against this emitter's own output with the guard
  # stubbed out: a blank line inside a plain tabular CELL compiles cleanly (an
  # \halign entry is restricted horizontal mode, where \par does nothing), but
  # a blank line inside a MACRO ARGUMENT aborts -- and a row-group label goes
  # into two of them, \multicolumn and \textbf:
  #   ! Paragraph ended before \text@command was complete.
  #   ! Extra }, or forgotten \endgroup.
  # A single newline compiles; two do not. The emitter neutralises the whole
  # class by leaving no newline in a cell payload at all.
  spec <- as_spec(apa.cor.table(attitude))
  spec$body[[1]][[1]] <- richtext("line one\n\n \t \nline two")
  spec$body[[6]][[5]] <- richtext_lines(richtext("top\n\nstill top"),
                                        richtext("[.66, .91]"))
  tex <- apa_latex_table(apa_display_plan(spec), 10)

  # no blank line anywhere: a "paragraph" cannot end inside a cell
  expect_false(grepl("\n[ \t\r]*\n", tex))
  # both halves of each mangled cell stay on ONE emitted line
  one <- grep("line one", strsplit(tex, "\n", fixed = TRUE)[[1]], value = TRUE)
  expect_length(one, 1L)
  expect_match(one, "line two")
  two <- grep("still top", strsplit(tex, "\n", fixed = TRUE)[[1]], value = TRUE)
  expect_length(two, 1L)
  expect_match(two, "top")

  # the case that actually aborts a real render: a blank line in a row-group
  # label, which the emitter wraps in \multicolumn AND \textbf
  gspec <- as_spec(apa.2way.table(iv1 = gender, iv2 = alcohol,
                                  dv = attractiveness, data = goggles,
                                  show.conf.interval = TRUE))
  gspec$row_groups[[1]]$label <- richtext("alcohol: None\n\nsecond para")
  gtex <- apa_latex_table(apa_display_plan(gspec), 10)
  expect_false(grepl("\n[ \t\r]*\n", gtex))
  lab <- grep("second para", strsplit(gtex, "\n", fixed = TRUE)[[1]], value = TRUE)
  expect_length(lab, 1L)
  expect_match(lab, "\\\\textbf\\{alcohol: None +second para\\}")
})

test_that("row groups and spanners emit the shapes the reference solved", {
  # row groups: apa.2way.table(show.conf.interval = TRUE) is the only builder
  # branch that produces them
  gp <- apa_display_plan(as_spec(apa.2way.table(
    iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles,
    show.conf.interval = TRUE)))
  tex <- apa_latex_table(gp, 10)
  expect_match(tex, "\\\\addlinespace")
  # full width, left, bold -- the same treatment the flextable/Word path gives
  expect_match(tex, "\\\\multicolumn\\{4\\}\\{[^\n]*l[^\n]*\\}\\{\\\\textbf\\{alcohol: None\\}\\}")

  # spanners: a \multicolumn plus a trimmed \cmidrule underneath
  sp <- apa_display_plan(as_spec(apa.2way.table(
    iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles)))
  tex2 <- apa_latex_table(sp, 10)
  expect_match(tex2, "\\\\multicolumn\\{6\\}\\{[^\n]*c[^\n]*\\}\\{alcohol\\}")
  expect_match(tex2, "\\\\cmidrule\\(lr\\)\\{2-7\\}")
  expect_match(tex2, "\\\\cmidrule\\(lr\\)\\{2-3\\} \\\\cmidrule\\(lr\\)\\{4-5\\}")

  # EVERY \multicolumn states both of its edges. \multicolumn re-adds
  # \tabcolsep (6 pt here, since the emitter never issues \setlength) on any
  # side it is not told about, which would silently make one row 12 pt wider
  # than every other row in the same table.
  all_mc  <- gregexpr("\\\\multicolumn\\{[0-9]+\\}\\{", tex2)[[1]]
  edged   <- gregexpr("\\\\multicolumn\\{[0-9]+\\}\\{@\\{\\}", tex2)[[1]]
  expect_true(length(all_mc) > 0 && all_mc[1] > 0)
  expect_equal(length(edged), length(all_mc))
  # the right-hand edge too: a span that does not reach the last column has to
  # put back the inter-column gap it swallowed
  expect_match(tex2, "\\\\multicolumn\\{2\\}\\{@\\{\\}[^\n]*@\\{\\\\hspace\\{4pt\\}\\}\\}")
})

test_that("a stacked cell becomes a nested tabular, each line guarded", {
  # the r / CI / p stack in a correlation table
  tex <- apa_latex_table(apa_display_plan(as_spec(apa.cor.table(attitude))), 10)
  expect_match(tex, "\\\\begin\\{tabular\\}\\[t\\]\\{@\\{\\}r@\\{\\}\\}")
  expect_match(tex, "\\\\begin\\{tabular\\}\\[t\\]\\{@\\{\\}r@\\{\\}\\}\\.83\\*\\* \\\\\\\\ \\{\\}\\[\\.66")
  # \makecell is NOT used: neither apaquarto's header.tex nor apa7.cls loads
  # the makecell package, and this function returns a bare string with no way
  # to add a \usepackage. See the comment at apa_latex_cell().
  expect_false(grepl("\\\\makecell", tex))
})

test_that("the requested size is written into the table, not in front of it", {
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude)))
  tex8 <- apa_latex_table(plan, 8)
  # every column carries it, so the size survives endfloat (D10)
  expect_match(tex8, "\\\\fontsize\\{8\\}\\{9.6\\}\\\\selectfont")
  # and nothing precedes \begin{longtable}
  expect_match(tex8, "^\\\\begin\\{longtable\\}")
  expect_equal(lengths(regmatches(colspec_of(tex8),
                                  gregexpr("\\\\fontsize", colspec_of(tex8)))),
               plan$n_col)
  # the row strut is rebuilt at the table's own size, inside the environment
  expect_match(tex8, "\\\\@arstrutbox")
  expect_error(apa_latex_table(plan, "big"), "positive number")
})

test_that("the emitter clamps at DEGENERACY, not at legibility (D7b)", {
  # This test used to assert that 0.5 pt, 1 pt and 4.999 pt were all raised to
  # 5 pt. That clamp is what made D7b false -- a table needing 3 pt was set at
  # 5 pt and CLIPPED. The guard now catches only the real defect it was
  # written for: apa_latex_num()'s four-decimal formatting turns any size
  # under 0.00005 into "\fontsize{0}{0}\selectfont", an invisible table that
  # compiles.
  plan <- apa_display_plan(as_spec(apa.cor.table(attitude)))

  # sizes below 6, below 5 and below 1 are all emitted AS ASKED. Every one of
  # these was measured rendering a complete table through xelatex, at the size
  # requested rather than a substituted one -- dev/factcheck/phase2-subfive/.
  for (small in c(5, 3, 1, 0.5, APA_LATEX_MIN_EMITTABLE_SIZE)) {
    tex <- apa_latex_table(plan, small)
    expect_false(grepl("\\\\fontsize\\{0\\}", tex))
    sizes <- unique(regmatches(tex, gregexpr("\\\\fontsize\\{[0-9.]+\\}\\{[0-9.]+\\}",
                                             tex))[[1]])
    expect_equal(sizes, sprintf("\\fontsize{%s}{%s}",
                                apa_latex_num(small), apa_latex_num(small * 1.2)),
                 info = paste("size", small, "was not emitted as asked"))
  }

  # and the degenerate request is still caught, at the emittable floor
  tex <- apa_latex_table(plan, 0.00001)
  expect_false(grepl("\\\\fontsize\\{0\\}", tex))
  expect_match(tex, sprintf("\\\\fontsize\\{%s\\}",
                            apa_latex_num(APA_LATEX_MIN_EMITTABLE_SIZE)))
})
