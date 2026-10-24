# The LaTeX backend for the richtext run model (R/richtext.R). This is the
# fourth backend over the same rt_run(text, italic, bold, script) shape --
# see rt_to_paragraph() in render-flextable.R (flextable/LaTeX), the officer
# backend rt_run_to_ftext() in save.R (Word), and rt_to_markdown() in
# quarto.R (the apaquarto markdown chunk path). rt_to_markdown() solves the
# same "escape the payload, then add markup" ordering problem this file
# does, and is the closest analogue.
#
# ONE PASS, NEVER TWO. Do not substitute symbols (which injects macros like
# "\ensuremath{\beta}" -- backslashes and braces) and THEN escape the result:
# the escape re-escapes the macro syntax just introduced
# ("\ensuremath{\beta}" -> "\textbackslash{}ensuremath\{\textbackslash{}beta\}").
# Reordering the two calls does not help either (that just moves which stage's
# output gets re-examined by the other). Instead there is no second pass at
# all: apa_latex_run_map below maps EVERY character -- Greek codepoint
# or LaTeX-special character -- directly to its final LaTeX text in ONE
# combined lookup, applied in a SINGLE traversal of the run's original,
# untouched text (apa_latex_map_chars()). Because there is only one pass
# over the source characters, and style macros are wrapped around its
# result afterward without ever being re-scanned, no stage's output is ever
# re-examined by an earlier stage -- there is no earlier stage left to do
# the re-examining.
#
# The same single-pass primitive, restricted to just the escape entries (no
# symbols), is apa_latex_escape() -- also no longer placeholder-based (see
# the note above apa_latex_escape_map for why a placeholder could not be
# made collision-free).

# C0 control bytes. Real .tex files containing each C0 control character
# were compiled (both pdflatex and xelatex, -interaction=nonstopmode) to find
# out which ones actually break a render rather than guessing:
#   - U+0000, U+0001, U+0002, U+0008, U+000B, U+000E..U+001F, and U+007F
#     (DEL) each produced a genuine TeX ERROR under pdflatex ("Unicode
#     character ^^X (U+00XX) not set up for use with LaTeX" or "Text line
#     contains an invalid character") and under xelatex -- exactly the
#     silent-vanishing failure class apa_latex_symbol_map exists to
#     prevent for a literal Greek glyph, just with an error message
#     attached instead of silence.
#   - U+000C (form feed) compiled cleanly on both engines: TeX's own
#     default catcode table already treats it as an ignored character.
#     Included in the strip set anyway for symmetry with the rest of the
#     C0 range -- stripping an already-ignored character changes nothing
#     TeX-visible.
#   - TAB (U+0009), LF (U+000A) and CR (U+000D) all compiled cleanly on
#     both engines EMBEDDED MID-CELL and rendered as an ordinary
#     inter-word space ("START<tab/LF/CR>END" -> "START END", confirmed
#     via pdftotext) -- TeX's own catcode table already treats all three
#     as whitespace, so there is no render-safety reason to touch them.
#     These three are deliberately left OUT of apa_latex_control_strip and
#     so pass through apa_latex_map_chars() unchanged.
#
# LF and CR get one more deliberate decision on top of "TeX won't error on
# them": rt_run()'s text field has no documented multi-line semantics
# (stacking a cell's lines is richtext_lines(), a mechanism one level above
# a single run), and the longtable emitter below is the layer that decides
# what a newline means (see apa_latex_collapse_newlines()). This backend
# therefore leaves LF/CR for the emitter to interpret rather than deciding
# for it -- it only commits to the (empirically confirmed) claim that
# leaving them raw does not break a render.
apa_latex_control_codes <- c(0x00:0x08, 0x0B, 0x0C, 0x0E:0x1F, 0x7F)
apa_latex_control_strip <- stats::setNames(
  rep("", length(apa_latex_control_codes)),
  vapply(apa_latex_control_codes, intToUtf8, character(1))
)

# Escape the LaTeX-special characters -- backslash, &%$#{}, underscore,
# tilde, caret -- so that arbitrary user/data text (variable names, notes)
# can be dropped into a LaTeX document without the characters LaTeX treats
# as syntax aborting or corrupting the render. An underscore starts math
# mode and ABORTS the render outright ("! Missing $ inserted.") -- this has
# been hit for real on a marker string. Also folds in
# apa_latex_control_strip (above) -- the unsafe C0 controls and DEL -- so
# escaping and control-byte stripping happen in the
# same single pass as everything else.
#
# Do not apply this table via a chain of gsub() calls with backslash routed
# through a "\x01" placeholder character (expanded to "\textbackslash{}"
# only after every other substitution has run) to keep the brace-escaping
# step from re-escaping the braces that substitution introduces. That
# placeholder is not safe: cell text arrives from variable
# names, factor levels, and note text that routinely comes from SPSS/SAS/CSV
# imports or copy-paste, any of which can carry a stray "\x01" control byte,
# and that byte would be silently reinterpreted as a backslash the user
# never typed (confirmed: the gsub version turned "a\x01b" into
# "a\textbackslash{}b"). Picking a rarer placeholder only narrows the
# window; it does not close it. apa_latex_map_chars() below closes it by
# construction: it splits the input into individual characters ONCE and
# maps each one through this lookup ONCE, so no substitution's own output
# is ever re-read as input to a later substitution, and no sentinel
# character is needed at all. (U+0001 itself is additionally covered by
# apa_latex_control_strip above, so it is stripped outright rather than
# merely not misread.)
apa_latex_escape_map <- c(
  "\\" = "\\textbackslash{}",
  "&"  = "\\&",
  "%"  = "\\%",
  "$"  = "\\$",
  "#"  = "\\#",
  "{"  = "\\{",
  "}"  = "\\}",
  "_"  = "\\_",
  "~"  = "\\textasciitilde{}",
  "^"  = "\\textasciicircum{}",
  apa_latex_control_strip
)

# INVARIANT: call apa_latex_escape() (and apa_latex_map_chars() generally)
# exactly ONCE, directly on raw, unescaped text -- typically a spec's
# original data. It is NOT safe under double application: its own output
# contains literal backslashes and braces ("\textbackslash{}", "\&", ...),
# and a second call cannot tell those apart from a backslash or brace the
# user actually typed, so it escapes its own output again
# (confirmed: apa_latex_escape(apa_latex_escape("a&b\\c")) produces
# "\textbackslash{}textbackslash\{\}c", not a no-op). The emitter below (or
# any future caller) must escape each piece of raw text exactly once on its
# way into the document, never re-run this function (or apa_latex_map_chars()
# generally) on text that has already been mapped once, or on text that
# already carries a style-macro wrap.
#
#' Escape LaTeX special characters in a plain string
#'
#' @section Invariant:
#' Not safe under double application -- see the source comment above this
#' function for the confirmed counter-example. Call exactly once, on raw
#' text only.
#' @param txt a length-1 (or vectorized) character string.
#' @return `txt` with LaTeX special characters escaped.
#' @keywords internal
apa_latex_escape <- function(txt) {
  apa_latex_map_chars(txt, apa_latex_escape_map)
}

# Map the four Greek codepoints apa_sym() (R/richtext.R) emits as literal
# UTF-8 to LaTeX macros. apa_sym() itself is NOT changed -- the flextable,
# officer and markdown backends all depend on its current literal-glyph
# output; only this LaTeX backend needs the macro form. \ensuremath{} lets
# each macro appear in text mode (the APA table cells this renders into are
# never already inside math mode).
#
# A literal UTF-8 beta SILENTLY VANISHES from a rendered PDF -- no
# error, no warning, just a missing character -- which is why every symbol
# is round-tripped through this table rather than passed through unescaped.
apa_latex_symbol_map <- c(
  "\u03b2" = "\\ensuremath{\\beta}",   # beta
  "\u03b7" = "\\ensuremath{\\eta}",    # eta
  "\u0394" = "\\ensuremath{\\Delta}",  # Delta
  "\u03b5" = "\\ensuremath{\\varepsilon}" # epsilon
)

# The combined per-character map rt_run_to_latex() uses: symbols and plain
# escapes together, so a run's text is mapped in exactly one traversal (see
# apa_latex_map_chars()) rather than two sequential passes that would each
# risk re-scanning the other's output. The two key sets are disjoint (no
# Greek codepoint is also a LaTeX-special character), so combining them
# changes nothing about how either half behaves alone.
apa_latex_run_map <- c(apa_latex_symbol_map, apa_latex_escape_map)

# Map every character of txt through `map`, in a single left-to-right
# traversal: split into individual (Unicode-aware) characters once, look
# each one up once, and concatenate the results. A character with no entry
# in `map` passes through unchanged. Because the traversal is over the
# ORIGINAL input characters only, a replacement's own characters (backslash,
# braces, ...) are never fed back in as something to look up again -- there
# is nothing left to re-scan, so no placeholder/sentinel bookkeeping is
# needed to keep one substitution from colliding with another.
apa_latex_map_chars <- function(txt, map) {
  vapply(txt, function(one) {
    if (is.na(one)) return(NA_character_)
    chars <- strsplit(one, "", fixed = TRUE)[[1]]
    if (!length(chars)) return("")
    # `map` is a plain named character vector, not a list: single-bracket
    # indexing on a name that is not present returns NA (with a dropped
    # name), never an error, unlike `[[`.
    repl <- unname(map[chars])
    mapped <- ifelse(is.na(repl), chars, repl)
    paste0(mapped, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

#' One styled run converted to LaTeX
#' @keywords internal
rt_run_to_latex <- function(r) {
  # One combined pass over the run's raw text (symbols + escapes together --
  # see apa_latex_run_map), then style macros wrapped around the result.
  # The style macros are added AFTER the only pass over the text runs, so
  # they are never themselves re-escaped.
  txt <- apa_latex_map_chars(as.character(r$text), apa_latex_run_map)
  if (identical(r$script, "super")) txt <- paste0("\\textsuperscript{", txt, "}")
  if (identical(r$script, "sub"))   txt <- paste0("\\textsubscript{", txt, "}")
  if (isTRUE(r$bold))   txt <- paste0("\\textbf{", txt, "}")
  if (isTRUE(r$italic)) txt <- paste0("\\textit{", txt, "}")
  txt
}

#' Map a richtext value to a LaTeX string
#'
#' The LaTeX backend for the richtext run model in `R/richtext.R`. Every
#' run's text is mapped through symbol substitution and character escaping
#' TOGETHER, in one pass (see `rt_run_to_latex()`), before any style macro
#' is applied, so macros added by this function are never themselves
#' re-escaped or re-scanned.
#' @param rt an `apa_richtext` value, or a plain character string.
#' @return a length-1 character string of LaTeX.
#' @keywords internal
rt_to_latex <- function(rt) {
  runs <- if (is.character(rt)) list(rt_run(rt)) else unclass(rt)
  paste0(vapply(runs, rt_run_to_latex, character(1)), collapse = "")
}

# ---------------------------------------------------------------------------
# The width model. Its constants come from measuring real LaTeX \LT@entry
# column widths against R's prediction for four tables at two sizes; a
# naive formula that omits flextable's cell padding, or uses the wrong
# \tabcolsep term, mis-predicts those measured widths. This file carries the corrected
# arithmetic and the reasoning for where each correction is applied.
#
# What a flextable's declared column width IS, and WHY the constant below is
# 5/27 in and not the "obvious" 10/72 in (an empirical correction).
#
# The obvious assumption is
#   w_j(s) = content_j(s) + padding.left + padding.right
# with padding.left/right at flextable's documented default, 5 pt EACH (the
# package's theme, apa_flextable_theme(), sets only padding.top/bottom, to
# 1 pt; it never touches left/right) -- 10 pt = 10/72 in total. FIT INSTEAD
# of assumed: `dim(apa_ft_at_size(apa_flextable_from_plan(plan), s))$widths`
# for `s` in `c(6, 8, 10, 12)`, regressed per column as `w_j(s) = c_j * (s /
# 12) + K`, gives an intercept `K` that is IDENTICAL to at least 6 decimal
# places across every column of every one of 4 real specs tested
# (cor_attitude, cor_mtcars, a single-block reg, twoway) -- an exact affine
# relationship, not a noisy fit -- and its value is 0.1851852 in
# (13.33333 pt), i.e. exactly **5/27 in**, not 10/72 in = 0.1389 in. The
# extra ~3.33 pt/column is real and reproducible (flextable/gdtools internal
# margin or minimum-width behavior beyond the documented padding.left/right
# API; not investigated further since the fit already isolates it exactly),
# so `K` below is that MEASURED regression intercept, not the theoretical
# padding sum -- treat it as "the font-independent constant `autofit()`
# actually adds", not as "flextable's declared padding". Using 10/72 in
# instead worked at 10-12 pt (where the two constants differ by little
# enough, relative to the two-decimal column rounding, that no fixture
# comparison caught it) but silently mis-sized every column below 10 pt --
# precisely the range the rotate-then-shrink ladder shrinks INTO after
# rotating (the sub-10 pt fixture entries `cor_attitude_6`/`twoway_6` guard
# it: `\LT@entry` widths from a real longtable, not an R prediction).
#
# WHERE the subtraction happens, and why that point is correct (this is the
# subtlety a linear-scaling formula gets wrong if applied blindly): `K` is a
# PER-COLUMN, FONT-INDEPENDENT constant -- the regression above is what
# establishes that, empirically, not an assumption -- while content DOES scale
# with size (glyph advance widths are proportional to point size for any vector
# font, so a column's content width at size s is content_j(12) * s / 12,
# exactly, once `K` is out of it). So `K` is subtracted from EACH column's
# declared width INDIVIDUALLY, measured once at 12 pt, BEFORE that column's
# content is scaled to the requested size and BEFORE anything is summed across
# columns or rounded. Subtracting it later -- after scaling, or after summing --
# would subtract the wrong quantity: an already-scaled or already-summed `K`
# term is not "5/27 in", it is "5/27 in * s/12" or "5/27 in * n", neither of
# which is what a native longtable actually omits (it omits exactly 5/27 in of
# this constant per column, at every size, because it has no equivalent of its
# own to scale).
#
# The \tabcolsep term. flextable's own LaTeX writer sets \tabcolsep = 2 pt,
# so its grid costs 4 pt PER COLUMN (2 pt each side), a flat 4n. A native
# longtable emitted with "@{}" on BOTH outer edges (apa_latex_table() below)
# removes one gap's worth entirely, so the term is (4n - 4) pt, not 4n -- measured
# directly against LaTeX's own layout, not assumed.
#
# corrected_up(n, s) = sum_j ceil100( w_j(s) - 5/27 )  +  (4n - 4) / 72.27
#
# ceil100() rounds UP to the nearest 0.01 in, per column -- every
# intermediate is rounded up, so any error this model still carries
# costs a needlessly rotated/shrunk page, never a table run off the edge.
# `K` is stated in ordinary points (1/72 in, gdtools'/officer's convention,
# matching how it was measured); \tabcolsep is a LaTeX length and LaTeX's
# own point is 1/72.27 in -- mixing those up by even one column changes a
# measured table's prediction by more than its entire margin.
apa_latex_padding_in <- 5 / 27

# Residual safety margin, added ONCE per table (not per column): a
# gdtools-vs-XeLaTeX font-metric disagreement additive per string (2.7-4.2 pt
# across seven probe strings, NOT proportional -- fitting a per-column
# constant to seven strings would just be new calibration under an old
# name), plus the 0.0415 in worst-case gap between the boxed-tabular proxy
# and LaTeX's own longtable \LT@entry widths (both measured).
#
# Why 0.10 in and not 0.05 in. With `K` at the measured 5/27 in, the model's
# RAW error (cushion minus whatever margin is added) is small enough that
# 0.05 in would be most of the protection rather than a comfortable margin:
# at the tightest measured point (`twoway` at 6 pt) the raw error is only
# +0.026 in, and 0.05 in clears the 0.0415 in worst-case measurement
# uncertainty by just 0.0085 in. The two directions cost differently -- an
# under-estimate is content off the page edge (an overfull \hbox, typically
# an unbreakable token like a CI bound spilling into the margin), an
# over-estimate is only a page rotated or shrunk one step further than it
# needed to be -- so the margin should be comfortable, not marginal.
#
# 0.10 in was checked against every measured case (8) for whether it flips any
# rotate/fit verdict on a 6.5 in portrait page: it does not -- all 8 agree with
# the true (measured) verdict at BOTH 0.05 in and 0.10 in, including
# `apa.cor.table(attitude)` at 10 pt (guarded 5.39 -> 5.44 in, actual 5.29 in,
# comfortably upright both times) and the two closest-to-the-line real cases,
# `apa.reg.table()` 2 blocks at 10 pt (actual 6.64 in) and `cor_mtcars` at 8 pt
# (actual 6.53 in), both correctly called "rotate" under either margin. Raising
# it here is free insurance in the measured set, not a trade against it.
APA_LATEX_WIDTH_MARGIN_IN <- 0.10

# The size the linear-scaling measurement is taken at. Any positive
# reference size would do arithmetically (content scales proportionally
# from any baseline), but 12 pt is what the width measurements used, and
# reusing it means a direct look at
# `apa_ft_at_size(apa_flextable_from_plan(plan), 12)` reproduces exactly
# what this file measures.
APA_LATEX_WIDTH_REFERENCE_SIZE <- 12

# The largest size apa_size_for_width() will ever return. Deliberately the
# SAME value as the reference size above -- not a coincidence, two
# independent reasons land on it: (1) APA's own body type is never set above
# 12 pt, so a "size to use" function has no legitimate reason to answer
# larger than that; (2) the linear-scaling model is measured AT 12 pt and
# only ever verified by extrapolating DOWN from it (nothing was measured
# above 12 pt), so reporting a
# larger size would also be extrapolating the model past where anything has
# checked it. Without this cap, a target wider than the table's own natural
# 12 pt width solves the un-rounded linear formula for an s in the
# thousands (a one-column plan at a merely generous target can already clear
# 12; `target = 1000` on a real spec returns north of 2000 pt) -- answers
# with no meaning as a font size. Capping here, in the one function whose
# whole purpose is choosing a size, means every caller (the rotate-then-shrink
# ladder included) gets a sane answer without having to remember to clamp it
# themselves.
APA_LATEX_MAX_FONT_SIZE <- APA_LATEX_WIDTH_REFERENCE_SIZE

# The smallest size the emitter and the size solver will produce. This is a
# DEGENERACY guard, not a legibility floor, and the distinction matters: the
# design is "shrink past 6 pt as far as the table needs so nothing is ever cut
# off, and warn". Do not reintroduce a legibility floor (e.g. 5 pt): it
# silently CLIPS any table that needs less. A reader can zoom into 3 pt type;
# nobody can recover a column that is not in the file.
#
# WHY 0.1 pt, AND IT IS MEASURED RATHER THAN CHOSEN. Real 12-column tables were
# emitted at 5, 3, 1, 0.5 and 0.1 pt, rendered through the full apaquarto ->
# xelatex path, and read back out of the PDF. All five produced a COMPLETE table
# (12 of 12 markers each) and, by pdftotext -bbox on a marker word, all five
# were HONORED rather than substituted: rendered word width is linear in the
# requested size right down to 0.1 pt (26.007, 15.437, 5.202, 2.573 and 0.503 pt
# -- 5.03 to 5.20 pt of width per requested pt). No "size not available /
# substituted" warning at any of them. Do not raise it to 0.5 pt on the theory
# that xelatex substitutes below that; there is no such substitution, so the
# floor is the smallest size actually verified.
#
# It is close to irrelevant in practice, and that is the point: this floor is
# reachable ONLY when NO size fits (see below), where the ladder is warning
# about clipping anyway. It never constrains a table that could have fitted.
#
# THE HONEST LIMIT THAT REMAINS, stated plainly because the never-cut-off
# promise is absolute and this is the one thing that can defeat it: the width
# model has a term that does NOT scale with the type size. The inter-column
# \tabcolsep is (4n - 4) pt whatever the font, and
# apa_latex_width_from_content() rounds every column UP to the next 0.01 in, so
# a table of n columns can never be narrower than 0.01*n + (4n - 4)/72.27 + the
# margin, at ANY size. Past about 136 columns that alone exceeds a 9 in rotated
# page. apa_size_for_width() detects exactly that case and the ladder warns that
# content WILL be cut off, naming apa.save(). No apaTables builder produces such
# a table from real data.
APA_LATEX_MIN_EMITTABLE_SIZE <- 0.1

# Per-column content width at the reference size (12 pt), padding already
# removed -- the one quantity apa_latex_natural_width() and
# apa_size_for_width() both need, and the only flextable/gdtools
# measurement either of them performs. Kept as its own function (rather
# than inlined twice) so the two public functions cannot silently diverge
# in how they measure.
apa_latex_content_at_12 <- function(plan) {
  ft <- apa_ft_at_size(apa_flextable_from_plan(plan), APA_LATEX_WIDTH_REFERENCE_SIZE)
  dim(ft)$widths - apa_latex_padding_in
}

# The corrected_up(n, s) arithmetic above, applied to an ALREADY-MEASURED
# per-column content vector (so a caller solving for s, which needs the
# same formula evaluated many times while searching, does not re-run
# flextable's font-metric machinery for every candidate -- only the one
# measurement apa_latex_content_at_12() performs).
apa_latex_width_from_content <- function(content_12, size) {
  content_s <- content_12 * (size / APA_LATEX_WIDTH_REFERENCE_SIZE)
  n <- length(content_12)
  sum(ceiling(content_s * 100) / 100) + (4 * n - 4) / 72.27 +
    APA_LATEX_WIDTH_MARGIN_IN
}

#' The width a table will occupy as a native LaTeX longtable
#'
#' Measures the plan's cell strings ONCE, at 12 pt, with the same
#' font metrics flextable's `autofit()` depends on, then scales
#' the CONTENT term (only) to `size` and applies the corrected `K`
#' (empirically fit -- see the block comment above this function in
#' `R/render-latex.R`; NOT the theoretical 10/72 in), the
#' `\\tabcolsep` arithmetic, and a 0.10 in margin. The result
#' is biased HIGH against real LaTeX layout by design: across the 8
#' fixture entries now measured (6, 8, and 10 pt; 4 different real specs),
#' +0.13 to +0.24 in (2.4-7.5\%) -- so an error here costs a needlessly
#' rotated or shrunk page, never a table run off the page edge.
#'
#' @param plan a display plan, from `apa_display_plan()`.
#' @param size numeric point size to lay the table out at.
#' @return width in inches.
#' @keywords internal
apa_latex_natural_width <- function(plan, size) {
  apa_latex_width_from_content(apa_latex_content_at_12(plan), size)
}

#' The largest point size whose predicted width still fits a target
#'
#' The inverse of `apa_latex_natural_width()`. That function contains a
#' round-UP step (every column's content is rounded up to the nearest
#' 0.01 in before summing), so it is not exactly invertible in closed form:
#' two adjacent sizes can round to the same summed width, and the naive
#' algebraic inverse (solving the un-rounded linear formula for `size`) can
#' land fractionally past where the ROUNDED forward function actually still
#' fits. `apa_size_for_width()` is defined as the largest size, no bigger
#' than `APA_LATEX_MAX_FONT_SIZE` (12 pt -- see that constant: neither APA
#' body type nor the range the model was measured over goes above it), whose
#' predicted width -- by `apa_latex_natural_width()`, the exact same
#' function every other caller uses, not the un-rounded approximation used
#' only to seed the search -- still fits `target`. It gets there by solving
#' the un-rounded formula once (closed form, no re-measurement of the
#' table's text), clamping that estimate to the
#' [`APA_LATEX_MIN_EMITTABLE_SIZE`, `APA_LATEX_MAX_FONT_SIZE`] range a font
#' size can be emitted in at all, then walking the clamped estimate down in
#' 0.001 pt steps, re-checking against the real (rounded) formula, until it
#' fits.
#'
#' @section There is no legibility floor:
#' A 5 pt floor here would return a table needing less at 5 pt, CLIPPED.
#' 6 pt is a warning threshold and never a hard stop, so the walk continues
#' below 5 pt -- and below 1 pt -- to whatever size actually fits, bottoming
#' out only at `APA_LATEX_MIN_EMITTABLE_SIZE` (**0.1 pt**, the smallest size a
#' rendered table was measured to be set at rather than substituted; see that
#' constant, and do NOT use a 0.5 pt figure -- 0.5 pt was assumed and the
#' measurement disproved it).
#'
#' @section The invariant, and the one case that genuinely cannot fit:
#' `apa_latex_natural_width(plan, apa_size_for_width(plan, target)) <=
#' target` now holds for every `target` a table can be made to fit at all.
#' It cannot hold when NO size fits, and that case is real rather than
#' theoretical, because part of the width does not scale with the type size:
#' `\\tabcolsep` contributes `(4n - 4)` pt whatever the font, and every
#' column's content is rounded UP to the next 0.01 in, so an `n`-column table
#' is never narrower than `0.01*n + (4n - 4)/72.27 + APA_LATEX_WIDTH_MARGIN_IN`.
#' When `target` is below that, this function returns
#' `APA_LATEX_MIN_EMITTABLE_SIZE` and the predicted width exceeds `target`.
#' A caller that needs to know whether the fit actually succeeded -- the
#' rotate-then-shrink ladder, deciding whether to warn -- must check
#' `apa_latex_natural_width(plan, result) <= target` itself; this function's
#' return value alone does not carry that information.
#'
#' @param plan a display plan, from `apa_display_plan()`.
#' @param target target width in inches.
#' @param content_12 optional per-column content vector already measured by
#'   `apa_latex_content_at_12()`. Supplying it is what lets the ladder
#'   make its whole decision from ONE font measurement: that measurement runs
#'   flextable's `autofit()` and costs about 0.66 s on a 14-column table, and
#'   the ladder needs the same vector for the width at 10 pt, for the solve,
#'   and for the achieved-fit re-check. It must be the vector measured from
#'   `plan` -- it is not a way to price one table against another's columns.
#' @return numeric point size, in
#'   `[APA_LATEX_MIN_EMITTABLE_SIZE, APA_LATEX_MAX_FONT_SIZE]`.
#' @keywords internal
apa_size_for_width <- function(plan, target, content_12 = NULL) {
  if (is.null(content_12)) content_12 <- apa_latex_content_at_12(plan)
  n <- length(content_12)
  fixed <- (4 * n - 4) / 72.27 + APA_LATEX_WIDTH_MARGIN_IN
  budget <- target - fixed
  if (budget <= 0 || sum(content_12) <= 0) {
    return(APA_LATEX_MIN_EMITTABLE_SIZE)
  }
  # The narrowest this table can EVER be, at any positive size. Content is
  # rounded up per column to the next 0.01 in, so a column with any content at
  # all costs 0.01 in however small the type gets; below this width no size
  # fits and the walk-down would run all the way to the floor to discover it.
  # Answering here is both faster and more honest -- it is the same "no size
  # fits" fact the caller has to warn about.
  if (0.01 * sum(content_12 > 0) + fixed > target) {
    return(APA_LATEX_MIN_EMITTABLE_SIZE)
  }

  # Closed-form estimate: ignores only the per-column ceil100 rounding
  # (content scales linearly with size to within measured tolerance), so it
  # lands within a few thousandths of a point of the
  # true crossing before the walk-down below ever runs. Clamped into
  # [APA_LATEX_MIN_EMITTABLE_SIZE, APA_LATEX_MAX_FONT_SIZE] before that
  # walk-down -- never handed an out-of-range starting point to correct from
  # -- so this function can never return a size larger than 12 pt or smaller
  # than the emittable floor, regardless of how far outside a real table's
  # range `target` is.
  s <- APA_LATEX_WIDTH_REFERENCE_SIZE * budget / sum(content_12)
  s <- min(s, APA_LATEX_MAX_FONT_SIZE)

  while (s > APA_LATEX_MIN_EMITTABLE_SIZE &&
         apa_latex_width_from_content(content_12, s) > target) {
    s <- s - 0.001
  }
  max(s, APA_LATEX_MIN_EMITTABLE_SIZE)
}

# ---------------------------------------------------------------------------
# The emitter. apa_latex_table() turns a display plan into ONE
# complete `longtable` environment and nothing else -- no caption, no label,
# no note, no float, no size command in front of it. Everything below that is
# not obvious is here because a render measured it, not because it reads
# well; each such point names its evidence.

# The inter-column gap, in LaTeX points, written into the column spec.
#
# WHY 4 pt and not 2 pt. The gap has to equal what the width model was
# MEASURED against, or the model and the emitter describe different tables.
# That model's \tabcolsep term is (4n - 4) pt (see
# apa_latex_width_from_content() above), and its ground truth is a
# hand-written longtable probe that sets \tabcolsep = 2 pt with "@{}" on both
# outer edges.
# In a tabular, \tabcolsep is applied on BOTH sides of every column, so a
# 2 pt \tabcolsep puts 2 + 2 = 4 pt between two adjacent columns and
# n - 1 gaps make 4n - 4 pt. One "@{\hspace{...}}" replaces a whole gap, both
# halves, so it must carry 4 pt to reproduce the geometry that was measured.
# Emitting 2 pt would make every real table 2(n - 1) pt narrower than
# apa_latex_natural_width() predicts -- for a 10-column regression table
# 0.25 in of phantom width, which is enough to rotate or shrink a page that
# did not need it.
#
# CONFIRMED, not argued: the emitter's output and the probe's hand-written
# equivalent, compiled in the SAME document, give byte-identical total widths
# -- 4.1463 in for cor_attitude at 6 pt and 2.0357 in for twoway at 6 pt, from
# the .aux \LT@entry sums. (Per-column the two differ by 2 pt at each outer
# edge, because the two forms park the edge \tabcolsep in different columns;
# the totals, which are what the fit/rotate decision uses, agree exactly.)
APA_LATEX_COLGAP_PT <- 4

# Format a number for a LaTeX length or a \fontsize argument: no trailing
# zeros, no scientific notation, no locale surprises ("10", "7.2", "9.6").
apa_latex_num <- function(x) {
  s <- formatC(x, format = "f", digits = 4)
  s <- sub("0+$", "", s)
  sub("\\.$", "", s)
}

# "\fontsize{s}{1.2s}\selectfont". 1.2 is the ratio LaTeX's own size files use
# between size and baselineskip, and the ratio kableExtra wrote inside the
# float (\fontsize{10}{12}) on the path this replaces.
apa_latex_size_cmd <- function(size) {
  sprintf("\\fontsize{%s}{%s}\\selectfont",
          apa_latex_num(size), apa_latex_num(size * 1.2))
}

# WHERE THE FONT SIZE GOES, AND WHY IT IS SPELLED THIS WAY.
#
# Measured: `endfloat` defers the ENVIRONMENT ALONE to the
# .ttt file and drops whatever preceded it, so a
# "\bgroup\fontsize{10}{12}\selectfont" written in front of \begin{longtable}
# is silently gone from a table collected at the end of the document -- inside
# the deferred table \the\fontdimen6\font printed 12.0pt against 10.0pt for
# the same document with floatsintext. The size therefore has to
# live INSIDE the environment. kableExtra hides it in \begin{table}; a
# longtable has no float to hide in.
#
# The obvious placement does not compile. Three candidates were built as real
# .tex files and run through xelatex rather than reasoned about:
#
#   1. "\fontsize{8}{9.6}\selectfont" between \begin{longtable}{...} and
#      \toprule -- ABORTS. The size command opens the first cell of the first
#      row, so the \noalign inside \toprule that follows it is misplaced:
#      "! Misplaced \noalign." then 100 errors, no output.
#   2. The size command inside the LEADING "@{}" of the column spec --
#      compiles and does NOTHING. Measured by \LT@entry in the .aux: the
#      table came out 51.00002pt in column 1, byte-identical to the same
#      table with no size command at all, against 41.92802pt for the
#      known-good outside form. An @-expression's font change does not reach
#      the cells.
#   3. ">{\fontsize{8}{9.6}\selectfont}" on EVERY column -- works: column 1
#      measured 43.92802pt, i.e. the 8 pt layout, not the 10 pt one. This is
#      what apa_latex_colspec() emits.
#
# Candidate 3 alone leaves one defect, also measured rather than assumed. The
# per-row strut \@arstrutbox is built ONCE, by \@array, from \strutbox in the
# font current at \begin{longtable} -- which is the document's font, not the
# table's, because the >{} prefixes only run inside cells. At 6 pt in a 12 pt
# document that gives correctly-sized glyphs in double-height rows: baseline
# pitch 14.446 pt against 7.174 pt for the same table with the size set
# outside (pdftotext -bbox). A shrunken table would be nearly twice as tall as
# it should be, which also changes where it breaks across pages.
#
# apa_latex_strut_fix() repairs that from INSIDE the environment, which is the
# only place endfloat leaves available. A \noalign is legal as the very first
# thing after \begin{longtable}{...} (that is exactly how \toprule gets in), and
# rebuilding \@arstrutbox there -- at the table's own size, with array.sty's own
# definition -- gives every row, head rows included, the right height. The
# \setbox is \global because \noalign{...} is a group and a local box would be
# discarded with it. Be precise about what that global write costs, because the
# two obvious statements differ: the BOX ITSELF does persist after the table
# (measured: \ht\@arstrutbox reads 7.2pt after a 6 pt table, against 0pt in a
# control that never ran the fix), but nothing downstream reads that stale
# value, because \@array rebuilds \@arstrutbox at the start of every array and
# tabular. So the leak is real and its EFFECT is nil -- also measured: an
# ordinary 12 pt tabular following the table has a row pitch of 14.446 pt
# whether or not the fix ran. Say "the stale box is never read", not "nothing
# leaks"; a reader who checks the box rather than the pitch would otherwise go
# hunting for a bug that is not there.
#
# Confirmed on a real render -- with the fix the emitted table's row pitch is
# 7.173 pt, matching the outside-the-environment reference to the last
# recorded digit, and xelatex reports 0 errors.
#
# \makeatletter/\makeatother are needed for \@arstrutbox and \z@ and are safe
# here: \noalign's body is processed incrementally, not absorbed as an
# argument, so a catcode change inside it takes effect for the tokens that
# follow it on the same line (the probe compiles clean; a variant using
# \csname @arstrutbox\endcsname but leaving \z@ alone prints a stray "@"
# into the table).
apa_latex_strut_fix <- function(size) {
  paste0("\\noalign{\\makeatletter", apa_latex_size_cmd(size),
         "\\global\\setbox\\@arstrutbox\\hbox{",
         "\\vrule height\\arraystretch\\ht\\strutbox ",
         "depth\\arraystretch\\dp\\strutbox width\\z@}\\makeatother}")
}

# The column spec: "@{}" on both outer edges (that is the geometry the width
# model measured, and it is what removes one gap's worth of \tabcolsep from
# the total), the per-column size prefix, and APA_LATEX_COLGAP_PT between
# columns.
#
# NO VERTICAL RULES, ever -- APA forbids them, and this is the only place a
# vertical rule could enter a table, because a "|" anywhere else is cell data
# ("|" needs no escaping under fontenc=TU, so a
# variable name may legitimately contain one). The test that asserts their
# absence targets THIS string, not the emitted document.
apa_latex_colspec <- function(align, size) {
  sz <- apa_latex_size_cmd(size)
  cols <- paste0(">{", sz, "}", align)
  gap <- sprintf("@{\\hspace{%spt}}", apa_latex_num(APA_LATEX_COLGAP_PT))
  paste0("@{}", paste(cols, collapse = gap), "@{}")
}

# The spec for a \multicolumn covering columns from..to.
#
# \multicolumn replaces the preamble entries of every column it spans, INCLUDING
# the inter-column material that array.sty folded into them, and then puts
# \tabcolsep back on each side of its own spec unless "@{}" says otherwise.
# \tabcolsep is 6 pt here (this emitter never issues \setlength -- see the
# font-size comment above), so a bare "\multicolumn{2}{c}{...}" would silently
# add 12 pt that no other row has. Both edges are therefore always stated:
#   left  -- always "@{}": at column 1 that reproduces the table's own leading
#            "@{}", and past column 1 the slot is empty anyway because the gap
#            before it belongs to the previous column.
#   right -- "@{}" at the last column (the table's trailing edge), otherwise
#            the gap itself, which the span has just eaten.
# Checked against the reference: a twoway table emitted this way and
# the probe's hand-written spanner rows give the same total width to four
# decimal places in the same document.
apa_latex_mc_spec <- function(from, to, align, n_col, size) {
  right <- if (to >= n_col) "@{}" else
    sprintf("@{\\hspace{%spt}}", apa_latex_num(APA_LATEX_COLGAP_PT))
  paste0("@{}>{", apa_latex_size_cmd(size), "}", align, right)
}

# NEWLINES INSIDE CELL TEXT ARE COLLAPSED TO A SPACE. This is a crash guard,
# and it is deliberately absolute.
#
# rt_to_latex() leaves LF and CR raw on purpose (see the block comment at the
# top of this file): TeX treats a single one mid-cell as an ordinary inter-word
# space, and the interpretation is left to this layer. Here it is. A cell that
# STACKS lines does not use newlines at all -- it arrives as an
# apa_richtext_lines value and is emitted as a nested tabular (apa_latex_cell()
# below) -- so a newline inside a single run is never structure. It is stray
# data: variable names and factor levels come from CSV, SPSS/SAS imports and
# Word paste, all of which carry embedded line breaks.
#
# A DOUBLE newline in that position is not merely untidy, it aborts the
# render. The crash was reproduced against THIS emitter's own output, with
# this function stubbed out, and the details make the guard MORE necessary,
# not less:
#
#   - A blank line inside a plain (or nested) tabular CELL compiles cleanly.
#     Measured: a stacked cell built as richtext_lines("top\n\nstill top",
#     "[.66, .91]") emitted a genuine blank line inside
#     "\begin{tabular}[t]{@{}r@{}}" and xelatex reported 0 errors. A cell is
#     an \halign entry, i.e. restricted horizontal mode, where \par does
#     nothing.
#   - A blank line inside a MACRO ARGUMENT aborts. Every non-\long macro
#     qualifies, and this emitter puts cell text inside two of them on
#     purpose: \multicolumn (every spanner label and every row-group label)
#     and \textbf (every row-group label). Measured, with the same stub, on a
#     row-group label containing a blank line:
#       ! Paragraph ended before \text@command was complete.
#       ! Extra }, or forgotten \endgroup.
#     -- the two errors this guard exists to prevent, from a real xelatex run
#     on a real apa.2way.table() spec.
#
# So the hazard is not "stacked cells"; it is any cell text that ends up in an
# argument, which for a spanner or a row group is ALL of it. A single newline
# compiles anywhere; two do not, wherever an argument is involved.
#
# Replacing EVERY newline with a space (rather than only collapsing runs of
# them) is what makes the guard checkable: the emitted cell payload then
# contains no newline character at all, so no combination of them can form a
# blank line, and the test asserts exactly that -- an emitter that only
# squeezed "\n\n+" would still pass a same-shaped test while leaving
# "a\n \n b" (a blank line by TeX's rules -- the line between is whitespace)
# to abort the render. Collapsing to a space also matches what TeX does with
# the single-newline case anyway, so nothing renders differently than before.
#
# This runs on rt_to_latex() OUTPUT, which is safe and is not a second escape
# pass: LF and CR are the two characters apa_latex_map_chars() deliberately
# passes through untouched, so replacing them cannot disturb any escape or any
# style macro. Nothing else in the string is looked at.
apa_latex_collapse_newlines <- function(txt) {
  gsub("[\r\n]", " ", txt)
}

# The leading-bracket guard. "\\" scans for an optional argument, so a cell or
# a stacked line that BEGINS with "[" is eaten as "\\[dimen]": "! Illegal unit
# of measure (pt inserted)", or the cell's content silently vanishes. Every APA confidence
# interval starts with "[", so this is the common case, not the corner case.
# "{}" in front is the same guard kableExtra emitted ("{}[56.48, 64.77]") and
# costs nothing in the output.
apa_latex_bracket_guard <- function(txt) {
  ifelse(substr(txt, 1L, 1L) == "[", paste0("{}", txt), txt)
}

# One cell's payload: guarded, newline-free LaTeX. A stacked cell
# (apa_richtext_lines -- the r/CI/p stack in a correlation table, the d/CI
# stack in a d-value table) becomes a nested tabular of the cell's own
# alignment.
#
# WHY A NESTED TABULAR AND NOT \makecell. \makecell would be fine -- if the
# package were loaded. It is not:
# makecell reached the old kableExtra path only because kableExtra declared it
# as a LaTeX dependency, and this native path has no kableExtra (the package
# no longer depends on it at all -- the LaTeX path it served was removed in
# 4.0, and it is not even in Suggests any more). Checked,
# not assumed: "makecell" appears nowhere in
# inst/_extensions/wjschne/apaquarto/header.tex (which loads booktabs,
# multirow, multicol, colortbl, hhline, caption, array, float, xpatch) and
# nowhere in apa7.cls. An undefined \makecell aborts the render, and this
# emitter returns a bare string -- it has no way to add a \usepackage. The
# nested tabular needs no package at all, and it is exactly the construct the
# width model's ground truth was measured with (the probe uses
# "\begin{tabular}[t]{@{}r@{}}" for every stacked cell), so the emitter and
# the width model continue to describe the same table. "[t]" aligns the
# stack's first line with the row's baseline. If \usepackage{makecell} is
# ever added to the extension header, switching is a one-line
# change here -- the blank-line guard above is what both constructs need.
apa_latex_cell <- function(rt, align) {
  if (inherits(rt, "apa_richtext_lines")) {
    lines <- vapply(unclass(rt), function(line) {
      apa_latex_bracket_guard(apa_latex_collapse_newlines(rt_to_latex(line)))
    }, character(1))
    return(paste0("\\begin{tabular}[t]{@{}", align, "@{}}",
                  paste(lines, collapse = " \\\\ "),
                  "\\end{tabular}"))
  }
  apa_latex_bracket_guard(apa_latex_collapse_newlines(rt_to_latex(rt)))
}

# One header row: its cells, then the \cmidrule line the plan's rules ask for.
# A span-1 cell is written plainly -- no \multicolumn -- because its alignment
# already is the column's own and a \multicolumn would drag \tabcolsep in with
# it (see apa_latex_mc_spec()). Only a real spanner gets one.
apa_latex_header_row <- function(row, n_col, size) {
  col <- 1L
  out <- character(0)
  for (cell in row$cells) {
    txt <- apa_latex_cell(cell$rt, cell$align)
    if (cell$span > 1L) {
      spec <- apa_latex_mc_spec(col, col + cell$span - 1L, cell$align, n_col, size)
      txt <- sprintf("\\multicolumn{%d}{%s}{%s}", cell$span, spec, txt)
    }
    out <- c(out, txt)
    col <- col + cell$span
  }
  lines <- paste0(paste(out, collapse = " & "), " \\\\")
  if (length(row$rules)) {
    # (lr) trims the rule on both sides so adjacent spanners' rules do not
    # run together -- the same shape kableExtra emitted.
    lines <- c(lines, paste(vapply(row$rules, function(r) {
      sprintf("\\cmidrule(lr){%d-%d}", r[1], r[2])
    }, character(1)), collapse = " "))
  }
  lines
}

# The head block, used twice: once before \endfirsthead and once before
# \endhead, so column headings repeat on every continuation page (APA requires
# it for a table that breaks).
apa_latex_head_block <- function(plan, size) {
  c("\\toprule",
    unlist(lapply(plan$header, apa_latex_header_row, n_col = plan$n_col,
                  size = size)),
    "\\midrule")
}

# The invisible strut that reserves room for the last foot (see the comment at
# its emission site). Four lines of the table's own leading: \bottomrule plus a
# note, and an apa-note routinely runs to two or three lines because the
# significance-star sentence is a separate entry. A note longer than that can
# still outgrow the reserve, and then the foot breaks as it does today, with
# every row present.
APA_LATEX_FOOT_LINES <- 4

apa_latex_foot_reserve <- function(n_col, size) {
  sprintf("\\multicolumn{%d}{%s}{\\rule{0pt}{%spt}} \\\\", n_col,
          apa_latex_mc_spec(1L, n_col, "l", n_col, size),
          apa_latex_num(APA_LATEX_FOOT_LINES * 1.2 * size))
}

#' Emit a display plan as a LaTeX longtable
#'
#' Writes the GRID and nothing else: one complete \code{longtable}
#' environment, with no caption, no label, no note and no surrounding float.
#'
#' @section Why there is no caption (measured):
#' Quarto's \code{main.lua} rewrites any raw-LaTeX \code{longtable} that
#' contains no \code{\\caption} into \code{longtable*}, and every later fixup
#' -- those in Quarto and \code{apalongtablenote.lua} in apaquarto alike -- matches
#' the literal UNSTARRED name, so a starred environment becomes invisible to
#' all of them. Measured on two byte-identical documents differing only in that
#' one string: the unstarred form kept 60 of 60 marked rows with the note
#' spliced into the foot of the table; the starred form kept 45, \strong{rows
#' 46-60 vanished with no error}, and the note printed on another page. Writing
#' our own \code{\\caption} is not an escape route either -- the render then
#' dies inside Quarto's filter chain (\code{main.lua:3237}, \code{refType}).
#' apaquarto splices the caption in, which is what keeps the name unstarred and
#' the rows intact, and what keeps \code{@tbl-} cross-references resolving.
#' So the emitter writes the grid; apaquarto supplies the number, title,
#' and note around it.
#'
#' @section Structure:
#' \code{\\begin{longtable}} with the column spec carrying the size and the
#' inter-column spacing (never a \code{\\setlength} in front of the
#' environment -- see \code{apa_latex_colspec()}), then the strut fix,
#' then \code{\\toprule} / header rows / \code{\\midrule} twice, for
#' \code{\\endfirsthead} and \code{\\endhead}. \code{\\endfoot} draws NOTHING --
#' no rule may appear at a page break in the middle of a table -- but it is not
#' EMPTY: it carries an invisible zero-width strut that reserves the room
#' \code{longtable} needs for the last foot, without which that foot can break
#' onto a separate page, carrying the note away from the last row.
#' \code{\\bottomrule} belongs to \code{\\endlastfoot}, which must exist
#' because \code{apalongtablenote.lua} splices the note into it. Row groups are a
#' full-width bold \code{\\multicolumn} preceded by \code{\\addlinespace};
#' spanners are a \code{\\multicolumn} plus \code{\\cmidrule(lr)}; stacked
#' cells are a nested \code{tabular}.
#'
#' @param plan a display plan, from \code{\link{apa_display_plan}()}.
#' @param size numeric point size for the table's text.
#' @return a length-1 character string: one \code{longtable} environment.
#' @keywords internal
apa_latex_table <- function(plan, size = 10) {
  if (!is.numeric(size) || length(size) != 1L || !is.finite(size) || size <= 0) {
    stop("apa_latex_table(): `size` must be a single positive number.",
         call. = FALSE)
  }
  # CLAMP, do not merely validate -- but clamp at DEGENERACY, not at
  # legibility. A legibility floor (e.g. 5 pt) would set a table needing 4 pt
  # to fit at 5 pt and CLIP it, silently. 6 pt is a warning threshold, never a
  # hard stop, and content is never cut off -- so the floor is the smallest
  # size that still produces a real table rather than the smallest legible
  # one. A reader can zoom into 3 pt type; nobody can recover a column that is
  # not in the file.
  #
  # The real degenerate case stays guarded:
  # apa_latex_num() formats to four decimals, so a size below 0.00005 renders
  # as "\fontsize{0}{0}\selectfont" -- not a small table, a broken one.
  size <- max(size, APA_LATEX_MIN_EMITTABLE_SIZE)
  n_col <- plan$n_col
  head_block <- apa_latex_head_block(plan, size)

  body <- unlist(lapply(plan$body, function(p) {
    if (identical(p$type, "group")) {
      # A row-group label spans every column, left aligned and bold -- the
      # same treatment the flextable/Word path gives it (merge_at + align
      # left + bold in apa_flextable_from_plan()), so Word and PDF agree.
      # \addlinespace above it is the kableExtra reference's own separation
      # between groups.
      # Ended with \\* (no page break after it): a label left at the foot of a
      # page with its first row on the next heads nothing.
      spec <- apa_latex_mc_spec(1L, n_col, "l", n_col, size)
      c("\\addlinespace[0.3em]",
        sprintf("\\multicolumn{%d}{%s}{\\textbf{%s}} \\\\*", n_col, spec,
                apa_latex_cell(p$label, "l")))
    } else {
      cells <- vapply(seq_len(n_col), function(j) {
        apa_latex_cell(p$cells[[j]], plan$align[j])
      }, character(1))
      paste0(paste(cells, collapse = " & "), " \\\\")
    }
  }), use.names = FALSE)

  # A row whose first cell is empty continues the row above it -- a
  # regression model's R-squared, its interval and the blank row that closes
  # the block -- so the table must not break between them. Measured on
  # apa.reg.table() at 39-42 predictors: longtable carried the blank row, or
  # the interval and the blank row, to the next page, where they printed
  # under a repeated header with the note and no predictor at all. \\*
  # forbids a page break after a row; nothing on the page changes.
  continues <- grepl("^[[:space:]]*&", body)
  ends_row <- grepl("\\\\\\\\$", body)
  glue <- which(ends_row[-length(body)] & continues[-1L])
  body[glue] <- sub("\\\\\\\\$", "\\\\\\\\*", body[glue])

  paste(c(
    sprintf("\\begin{longtable}{%s}", apa_latex_colspec(plan$align, size)),
    apa_latex_strut_fix(size),
    head_block,
    "\\endfirsthead",
    head_block,
    "\\endhead",
    # NO RULE HERE, ever: anything visible in this chunk is drawn at every page
    # break, and a \bottomrule there would close a table that carries on.
    #
    # But it must not be EMPTY either, and that correction came from a rendered
    # page rather than from reading longtable's source. longtable subtracts the
    # height of \endfoot from the page goal on every page of the table, then
    # substitutes \endlastfoot at the end; if the LAST foot is taller than the
    # page-break foot, it may not fit where the rows just did, and longtable
    # then breaks the page in front of it. With an empty \endfoot that is easy
    # to hit, because the last foot is not empty at all -- it carries
    # \bottomrule AND the note, which apalongtablenote.lua splices in after R
    # has finished. MEASURED on the acceptance matrix's 60-row table, floats at
    # the end: the rows ended on page 5 and page 6 held a repeated column
    # header, the bottom rule and the note, with no data on it. The note was
    # attached in LaTeX's sense and orphaned in the reader's.
    #
    # An invisible strut in the page-break foot reserves that room on every
    # page, so a page that can hold the rows can also hold the last foot.
    # \rule{0pt}{...} has zero width, so nothing is drawn and no column is
    # widened -- the width model is untouched. Like \needspace's argument this
    # is a HINT and its two errors are not equal: too tall breaks the table one
    # row earlier than it needed to (whitespace), too short leaves the foot
    # where it is today. Neither loses a row.
    apa_latex_foot_reserve(plan$n_col, size),
    "\\endfoot",
    "\\bottomrule",
    "\\endlastfoot",
    body,
    # THIS TABLE DRAWS ITS OWN BOTTOM RULE -- tell apaquarto so. apaquarto's
    # template appends an \hline to
    # EVERY longtable at \end{longtable} (upstream code, and load-bearing
    # there: a plain pandoc markdown table was MEASURED to have no closing
    # rule at all without it). This table's \bottomrule lives in
    # \endlastfoot, above the spliced note, so the automatic \hline gave
    # every apaTables PDF table a DOUBLED bottom rule -- two bands 2.4 pt
    # apart at 300 dpi, in the shipped examples too. The bundled template
    # (5.0.18+apaTables.26) defines \apatablesownbottomrule to arm a flag
    # the \hline patch then consumes, standing down for this one table and
    # clearing itself. Reached via \csname, NOT directly: under a template
    # WITHOUT the patch (plain format: pdf, or stock apaquarto) \csname
    # makes the undefined name \relax and this line is a no-op inside its
    # \noalign -- the render survives and merely shows the old doubled rule
    # where the stock \hline patch exists, or the single \bottomrule where
    # it does not. \noalign because an assignment cannot sit bare between
    # longtable rows: a non-expandable token there opens a spurious cell.
    "\\noalign{\\csname apatablesownbottomrule\\endcsname}",
    "\\end{longtable}"
  ), collapse = "\n")
}
