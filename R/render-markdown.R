# The pandoc grid-table backend for the display plan (R/display-plan.R).
# This is the fourth backend over that destination-neutral plan (LaTeX in
# render-latex.R, flextable/Word in render-flextable.R and save.R's officer
# path) and the fifth over the richtext run model (R/richtext.R), reusing
# rt_to_markdown() from R/quarto.R -- the apaquarto markdown chunk path --
# for the per-run escaping and *-/**-/^-/~-markup.
#
# A pandoc grid table is source text: every column's width is the number of
# characters between its "|" separators, and pandoc trusts that count
# exactly. Two consequences drive this file. First, an escaped or marked-up
# cell (e.g. "3.14\\*" or "*Note*") is WIDER as source than it looks when
# rendered, so widths must be sized from the escaped text
# (apa_md_cell_lines()), not the plain text -- apa_md_widths() blends the two
# so columns are proportioned by rendered width but never narrower than the
# markup they must hold. Second, padding must count characters exactly:
# formatC() measures the escaped representation's *print width* under a
# locale-dependent notion of "wide" characters, not nchar(), so a padded
# string can come back short by exactly the number of backslashes it
# contains -- see apa_md_pad() below. Both defects are silent: a mis-sized
# grid table does not error, it collapses into fewer columns or rows than
# pandoc intended.

# A cell is one richtext, or an apa_richtext_lines of several (a stacked
# r / CI / p cell). Multi-line cells become several source lines inside the
# grid cell, joined with a trailing backslash so pandoc emits a hard break.
#
# A literal newline embedded in a cell's own text (e.g. a variable name typed
# with an embedded "\n") must become its own source line too, or it sits
# inside one grid line as a raw LF, which breaks the "|"-delimited row
# structure pandoc expects -- so every element is split on "\n" after
# markdown conversion, before anything else touches it.
#
# A literal tab is EXPANDED HERE to four literal spaces, not left alone. Do
# not pass a raw tab byte through on the theory that pandoc expands tabs to
# stop 4 on input -- measured false: put a one-character list-marker escape
# ("1. " -> "1\\. ") in front of a tab-bearing name and the same tab sits ONE
# COLUMN further right in the source line, pandoc's tab-stop-4 expansion
# produces a DIFFERENT number of spaces, the expanded line's length no longer
# matches the "+---+" boundary this file computed from nchar() (which counts
# a tab as one character, not its expanded width), and the grid loses a "|"
# and merges two columns -- silent, and dependent on the exact column
# position of the tab, so ANY future change to what precedes a tab-bearing
# cell (not only this one) can flip a passing table to a collapsing one.
# Expanding to a FIXED, position-independent run of spaces ourselves removes
# the raw tab byte entirely, so pandoc has nothing left to expand and
# nchar() is exact again.
# A grid-table cell is parsed by pandoc as its own block context, so a line
# beginning "1. " or "2) " -- exactly how apa.cor.table() numbers row/column
# labels (R/apaCorrelationTable.R: paste0(1:n, ". ", names(data))) -- is read
# as the START OF AN ORDERED LIST, not as literal text. Measured directly
# against the bundled reference document: the cell's paragraph style STAYED
# "Compact" (which already carries firstLine=0 in stock apaquarto -- it is
# NOT the source of any indent), but the cell gained a real <w:numPr>, and
# "1." disappeared into the list marker itself, leaving only "rating" as the
# run text; Word then indents the run using the LIST's own indent from
# numbering.xml, which no paragraph-style change can reach or fix. Escaping
# the delimiter is pandoc's own documented technique for suppressing list
# auto-detection and is invisible in the rendered text either way -- the
# escape is stripped back out by apa_md_plain_lines() below for the
# rendered-width measurement. Requires the delimiter to be followed by
# whitespace/end-of-line (matching pandoc's own list-marker rule) so an
# ordinary decimal number like "3.09" is left untouched.
apa_md_escape_list_marker <- function(lines) {
  gsub("^([0-9]+)([.)])(\\s|$)", "\\1\\\\\\2\\3", lines)
}

apa_md_cell_lines <- function(cell) {
  raw <- if (inherits(cell, "apa_richtext_lines")) {
    vapply(unclass(cell), rt_to_markdown, character(1))
  } else {
    rt_to_markdown(cell)
  }
  # strsplit("", "\n") returns character(0), not "" -- so an ordinary blank
  # cell (e.g. the filler cell a spanner row inserts over an uncovered
  # column) would silently vanish from the line vector instead of staying a
  # one-line blank, and every max(nchar(...)) reading it downstream would
  # hit "no non-missing arguments to max" on an empty vector. Restore the
  # blank explicitly.
  parts <- strsplit(raw, "\n", fixed = TRUE)
  parts <- lapply(parts, function(p) if (length(p) == 0L) "" else p)
  lines <- unlist(parts, use.names = FALSE)
  lines <- gsub("\t", "    ", lines, fixed = TRUE)
  apa_md_escape_list_marker(lines)
}

# The rendered (as-printed) width of a cell's lines, with markdown markup
# removed. This drives the PROPORTIONS; the markup width drives the SCALE.
apa_md_plain_lines <- function(cell) {
  s <- apa_md_cell_lines(cell)
  gsub("\\\\(.)", "\\1", gsub("[*^~]", "", s))
}

# A pipe is the grid's own column separator and must be escaped inside a cell.
apa_md_escape <- function(s) gsub("|", "\\|", s, fixed = TRUE)

# A group row (list(type = "group", label = <richtext>)) spans every column,
# so its label is a constraint on the TOTAL width, not on any one column --
# the widest line of its escaped/marked-up markdown must fit inside
# sum(w) + (n_col - 1), the same merged width apa_md_row() computes when it
# actually renders that row (span = n_col, internal "|"s omitted). Returns 0
# when the plan has no row groups.
apa_md_group_requirement <- function(plan, font_size = NULL) {
  req <- 0L
  for (row in plan$body) {
    if (identical(row$type, "group")) {
      md <- apa_md_escape(apa_md_cell_lines(row$label))
      md <- apa_md_style_span(md, font_size)
      req <- max(req, nchar(md))
    }
  }
  req
}

# Spread a width deficit across every column as evenly as possible (never
# narrowing a column to grow another) so the source table scales up as a
# whole rather than any one proportion being sacrificed to make room.
apa_md_widen <- function(w, deficit) {
  if (deficit <= 0L) return(w)
  n <- length(w)
  add <- rep.int(deficit %/% n, n)
  extra <- deficit %% n
  if (extra > 0L) add[seq_len(extra)] <- add[seq_len(extra)] + 1L
  w + add
}

# Spread a width deficit across a CONTIGUOUS range of columns (from:to) as
# evenly as possible -- the scoped counterpart of apa_md_widen() above, used
# when the constraint comes from a spanner rather than a row-group label, so
# only the columns under that spanner grow; an unrelated column is never
# bulked up for a spanner it does not sit under.
apa_md_widen_range <- function(w, from, to, deficit) {
  if (deficit <= 0L) return(w)
  idx <- from:to
  n <- length(idx)
  add <- rep.int(deficit %/% n, n)
  extra <- deficit %% n
  if (extra > 0L) add[seq_len(extra)] <- add[seq_len(extra)] + 1L
  w[idx] <- w[idx] + add
  w
}

# apa_md_widths() measures each column from its own span-1 cells, so a
# spanner label -- a header cell covering several columns with no internal
# "|" -- needs its own check against the width it will actually need.
# Walk every header row (a spanner can appear at any level), and for each
# cell whose span > 1, treat its escaped/marked-up label as a constraint on
# the TOTAL width of the columns it covers: sum(w[i:(i+k-1)]) + (k - 1) must
# be at least as wide as the label plus padding, or the merged cell pandoc
# writes is narrower than its own content and the grid collapses -- silently,
# per the same failure mode as an under-measured ordinary column. A shortfall
# is closed by widening just those columns (apa_md_widen_range()), never by
# trimming the label.
apa_md_span_widen <- function(plan, w, pad = 2L, font_size = NULL, rules = TRUE) {
  for (row in plan$header) {
    at <- 1L
    for (cell in row$cells) {
      span <- as.integer(cell$span)
      # A cell is a spanner LABEL -- and so a candidate for the rule-div
      # wrap apa_markdown_table() applies below -- exactly when its column
      # falls inside this row's own `rules` ranges (apa_md_in_ranges(), the
      # SAME test apa_markdown_table() uses to decide the wrap). `span > 1L`
      # is NOT that test: a spanner covering exactly one column
      # (apa_spanner(spec, cols = <one column>), unreachable through any
      # shipped builder today because apa_spanner() is unexported) still
      # gets a span-1 header cell, still lands in `rules`, and still gets
      # wrapped by apa_markdown_table() -- but a `span > 1L` guard here
      # would skip budgeting its width for that wrap, undershooting the
      # column and desyncing the whole grid the moment the wrap's own
      # "::: {custom-style=\"ApaTableRule\"}" fence line (34 characters)
      # outgrows the unbudgeted column (measured: a 3-column plan came back
      # 8 columns wide after pandoc misread the overflow as literal `|`s).
      if (span > 1L || apa_md_in_ranges(at, row$rules)) {
        md <- apa_md_escape(apa_md_cell_lines(cell$rt))
        md <- apa_md_style_span(md, font_size)
        if (rules) md <- apa_md_rule_div(md)
        need <- max(nchar(md)) + pad
        to <- at + span - 1L
        have <- sum(w[at:to]) + (span - 1L)
        if (need > have) w <- apa_md_widen_range(w, at, to, need - have)
      }
      at <- at + span
    }
  }
  w
}

apa_md_widths <- function(plan, pad = 2L, font_size = NULL, rules = TRUE) {
  n <- plan$n_col
  rendered <- integer(n)
  markup <- integer(n)
  floor_w <- integer(n)
  bump <- function(j, cell) {
    rendered[j] <<- max(rendered[j], max(nchar(apa_md_plain_lines(cell))))
    # `markup` (escaped text only) drives the PROPORTIONAL scale `s` below:
    # escaping overhead (e.g. "|" -> "\|") grows
    # roughly with the content, so a single ratio applied to every column is
    # the right model for it.
    #
    # A font-size wrap ("[...]{custom-style=\"ApaTableN\"}") is NOT
    # proportional -- it is a near-CONSTANT few dozen characters regardless
    # of the cell's own content, so folding it into the same ratio as
    # `markup` explodes it: a one-character column ("N") under a
    # 34-character overhead gives a ratio of ~34, and `s` is the MAX across
    # every column, so that one ratio gets applied to every OTHER column's
    # rendered width too, ballooning the whole table to hundreds of
    # characters wide -- watched directly (a 6-column cor table came out at
    # 200+ characters per column) before this split. `floor_w` carries the
    # STYLED width instead, applied only as a per-column ADDITIVE minimum via
    # pmax() below -- never blended into `s`. Neither the last header row
    # nor an ordinary body cell is ever rule-divided (only a spanner cell
    # is, and that overhead is measured separately by apa_md_span_widen()
    # below), so this needs no rule-div branch.
    esc <- apa_md_escape(apa_md_cell_lines(cell))
    markup[j]  <<- max(markup[j], max(nchar(esc)))
    styled <- apa_md_style_span(esc, font_size)
    floor_w[j] <<- max(floor_w[j], max(nchar(styled)))
  }
  # Every header row, not only the last, and only its `span == 1` cells --
  # a `span > 1` cell is a spanner label, budgeted across its whole range by
  # apa_md_span_widen() below, and must NOT also be measured per-column here
  # or its overhead would be double-counted.
  #
  # Do not narrow this to the LAST header row (the column-label row). The
  # pptx spanner flattening, `apa_slide_flatten_spanners()`, turns a spanner
  # label into an ordinary `span == 1` cell sitting in a NON-final header
  # row, which neither mechanism would otherwise measure -- the flattened
  # label would just inherit whatever width the data columns already need. A
  # label one character wider than its column (measured: "Marginal", 8
  # characters, against a 7-character column) desyncs the row's own `|`
  # positions from the separator line's, and pandoc's grid-table parser
  # reads the overflow as literal text: the shipped cell content came back
  # as `"Marginal|"`, with a phantom empty trailing column on every row.
  #
  # PROVEN, not just argued, to be a no-op for every OTHER destination: on
  # the two-way spanner table, a non-final header row holds ONLY `span > 1`
  # cells and `richtext("")` gap cells in the unflattened pipeline every
  # destination but pptx uses, and an empty cell contributes
  # `nchar("") == 0` to a `max()` -- changing nothing. The eight builders'
  # `apa_md_widths()` output is byte-identical with and without the
  # non-final header rows measured.
  for (row in plan$header) {
    at <- 1L
    for (cell in row$cells) {
      span <- as.integer(cell$span)
      if (span == 1L) bump(at, cell$rt)
      at <- at + span
    }
  }
  for (row in plan$body) {
    if (identical(row$type, "data")) for (j in seq_len(n)) bump(j, row$cells[[j]])
  }
  rendered <- pmax(rendered, 1L)
  markup <- pmax(markup, 1L)
  # Scale so no column is narrower than the SOURCE text it must hold, while
  # the proportions stay those of the rendered text.
  s <- max(markup / rendered)
  w <- pmax(as.integer(ceiling(rendered * s)) + pad, floor_w + pad)

  # A group label the columns can't jointly hold widens every column
  # (distributing the deficit) rather than being trimmed or truncated --
  # see apa_md_group_requirement() and apa_md_widen() above.
  need <- apa_md_group_requirement(plan, font_size) + pad
  have <- sum(w) + (n - 1L)
  if (need > have) w <- apa_md_widen(w, need - have)

  # A spanner (a header cell with span > 1) omits the internal "|"s between
  # the columns it covers, so its label is a constraint on the TOTAL width
  # of just those columns, not on any one of them -- the same merged-width
  # reasoning as the row-group label above, scoped to the span's own range
  # instead of the whole table. Left unmeasured, a spanner label longer than
  # the columns beneath it collapses the grid exactly like the row-group
  # case. See apa_md_span_widen() above.
  w <- apa_md_span_widen(plan, w, pad, font_size, rules)
  w
}

# Pad to an exact character count. NEVER formatC(): it measures the escaped
# representation, so a string holding backslashes -- which markdown escaping
# produces constantly -- is padded short and the whole table collapses.
apa_md_pad <- function(txt, k) paste0(txt, strrep(" ", max(0L, k - nchar(txt))))

apa_md_rule <- function(w, ch) {
  paste0("+", paste(vapply(w, function(k) strrep(ch, k), character(1)),
                    collapse = "+"), "+")
}

# The `+===+` line, carrying alignment. Left "|:===", right "===:|",
# centre both.
apa_md_align_rule <- function(w, align) {
  seg <- vapply(seq_along(w), function(j) {
    k <- w[j]
    switch(align[j],
           r = paste0(strrep("=", k - 1L), ":"),
           c = paste0(":", strrep("=", k - 2L), ":"),
           paste0(":", strrep("=", k - 1L)))
  }, character(1))
  paste0("+", paste(seg, collapse = "+"), "+")
}

# A pandoc fenced-div marker: an opening "::: {...}" or the bare closing
# ":::". apa_md_row() must never glue one of these to its neighbour with a
# trailing "\\" (see below) -- a fence line only opens/closes a BLOCK when it
# sits on its own line; a "\\" hard break keeps the following text in the
# SAME inline paragraph as the marker, so pandoc reads a literal "Str \":::\""
# instead of a Div. Watched: with the glue left in, apa_md_roundtrip() on a
# rule-divided cell reported ColSpan 7 (a total collapse) with ":::" showing
# up as plain text inside the dump -- exactly the silent-collapse failure
# mode this file exists to prevent.
apa_md_is_fence_line <- function(x) grepl('^::: \\{', x) || identical(x, ":::")

# One row. `spans` gives how many columns each cell covers; a span is expressed
# by omitting the internal "|", which is how pandoc detects ColSpan.
apa_md_row <- function(cells_md, spans, w) {
  widths <- integer(length(spans))
  at <- 1L
  for (i in seq_along(spans)) {
    widths[i] <- sum(w[at:(at + spans[i] - 1L)]) + (spans[i] - 1L)
    at <- at + spans[i]
  }
  height <- max(vapply(cells_md, length, integer(1)))
  vapply(seq_len(height), function(ln) {
    pieces <- vapply(seq_along(cells_md), function(i) {
      n <- length(cells_md[[i]])
      txt <- if (ln <= n) cells_md[[i]][ln] else ""
      nxt <- if (ln < n) cells_md[[i]][ln + 1L] else NA_character_
      if (ln < n && !apa_md_is_fence_line(txt) && !apa_md_is_fence_line(nxt)) {
        txt <- paste0(txt, "\\")
      }
      apa_md_pad(txt, widths[i])
    }, character(1))
    paste0("|", paste(pieces, collapse = "|"), "|")
  }, character(1))
}

# A font-size selection: wrap every non-blank cell LINE in a bracketed span
# naming the matching Word character style (see Step 4's ApaTable10..6
# styles below), one span per line so a hard-broken multi-line cell (a
# stacked r / CI / p cell) carries the style on each of its lines rather
# than only the first. A blank line (e.g. a spanner row's uncovered filler
# cell) is left alone: "[]{custom-style=...}" would be an empty, pointless
# span, and apa_md_plain_lines()'s ifelse-free stripping already treats an
# untouched blank line as a blank line.
apa_md_style_span <- function(lines, font_size) {
  if (is.null(font_size)) return(lines)
  ifelse(nzchar(lines),
         paste0("[", lines, "]{custom-style=\"ApaTable", font_size, "\"}"),
         lines)
}

# A cell that needs a rule beneath it carries a fenced div, which sets that
# cell's PARAGRAPH style. Pandoc has no cell borders, so this is the only
# mechanism -- and it is what gives partial (cmidrule) rules under spanners:
# a spanner covers several columns, and only the cells under IT get
# the div, so the rule stops and starts exactly where the spanner does.
apa_md_rule_div <- function(lines) c("::: {custom-style=\"ApaTableRule\"}", lines, ":::")

# TRUE when a 1-based column index falls inside any [from, to] range in a
# list of ranges (plan$header[[i]]$rules -- see R/display-plan.R:99-114,
# one c(min_col, max_col) pair per spanner in that header row). Used to
# decide which header cells at a non-last level get the rule div: exactly
# the spanner cells, never the blank span-1 filler between them.
apa_md_in_ranges <- function(col, ranges) {
  any(vapply(ranges, function(r) col >= r[1] && col <= r[2], logical(1)))
}

apa_markdown_table <- function(plan, font_size = NULL, rules = TRUE) {
  w <- apa_md_widths(plan, font_size = font_size, rules = rules)
  out <- apa_md_rule(w, "-")
  nh <- length(plan$header)
  for (hi in seq_len(nh)) {
    hrow <- plan$header[[hi]]
    is_last <- hi == nh
    ranges <- hrow$rules
    cells <- list()
    at <- 1L
    for (cell in hrow$cells) {
      span <- as.integer(cell$span)
      lines <- apa_md_style_span(apa_md_escape(apa_md_cell_lines(cell$rt)), font_size)
      # apaquarto's Table style already draws a continuous rule under the
      # LAST header row (one-row and multi-row headers alike), so the rule
      # div is applied ONLY to the spanned cells named by this row's own
      # `rules` ranges -- the partial (cmidrule) rule under a spanner,
      # never a blank span-1 filler beside it, and never the last header row,
      # whose `rules` is always empty (R/display-plan.R). A paragraph border
      # cannot span a cell boundary, so a per-cell div could never draw the
      # continuous rule the style already provides -- it would only draw a
      # second, segmented one on top of it.
      if (rules && apa_md_in_ranges(at, ranges)) lines <- apa_md_rule_div(lines)
      cells[[length(cells) + 1L]] <- lines
      at <- at + span
    }
    spans <- vapply(hrow$cells, function(c) as.integer(c$span), integer(1))
    out <- c(out, apa_md_row(cells, spans, w))
    out <- c(out, if (is_last) apa_md_align_rule(w, plan$align) else apa_md_rule(w, "-"))
  }
  for (row in plan$body) {
    if (identical(row$type, "group")) {
      lines <- apa_md_style_span(apa_md_escape(apa_md_cell_lines(row$label)), font_size)
      out <- c(out, apa_md_row(list(lines), plan$n_col, w))
    } else {
      cells <- lapply(row$cells, function(c) {
        apa_md_style_span(apa_md_escape(apa_md_cell_lines(c)), font_size)
      })
      out <- c(out, apa_md_row(cells, rep(1L, plan$n_col), w))
    }
    out <- c(out, apa_md_rule(w, "-"))
  }
  paste(out, collapse = "\n")
}

# Parse an emitted grid table back through `pandoc -t native` and report its
# true shape. A grid table's failure mode is SILENT AND TOTAL -- a column
# too narrow for its content collapses the whole table into one column, with
# exit status 0 and no warning anywhere -- so nothing short of a round trip
# through pandoc's own reader can see it. Internal, not exported: it exists
# so every emitter test (this file and later tasks') can assert the real
# parsed shape instead of trusting the source text's own line widths, which
# is exactly what a collapsed table would still satisfy.
#
# "\\bRow\\b" does not match "RowSpan" -- the character after "Row" there is
# a word character, so the trailing \\b fails to match at that position.
# Checked directly at the console before relying on it:
#   grepl("\\bRow\\b", "RowSpan 2 1") is FALSE; grepl("\\bRow\\b", "Row [") is
#   TRUE -- so counting "\\bRow\\b" occurrences counts real Row nodes and
#   skips every RowSpan attribute pandoc's native dump emits per cell.
apa_md_roundtrip <- function(md) {
  if (!requireNamespace("rmarkdown", quietly = TRUE) ||
      !rmarkdown::pandoc_available()) {
    return(NULL)
  }
  f <- tempfile(fileext = ".md")
  writeLines(md, f)
  out <- tempfile()
  rmarkdown::pandoc_convert(f, to = "native", output = out)
  txt <- paste(readLines(out, warn = FALSE), collapse = "\n")
  list(n_col = apa_md_count_matches("ColWidth", txt),
       n_row = apa_md_count_matches("\\bRow\\b", txt))
}

# gregexpr()'s length is NOT the match count: when a pattern has ZERO
# matches, gregexpr() returns -1L (a length-1 vector), so
# length(gregexpr(p, txt)[[1]]) reports 1, not 0 -- verified directly,
# length(gregexpr("ColWidth", "no match")[[1]]) returns 1. A total grid
# collapse severe enough that pandoc parses no Table node at all would then
# read as n_col == 1, which can match a genuine one-column expectation and
# pass silently -- exactly the failure mode apa_md_roundtrip() exists to
# catch. Both counts in apa_md_roundtrip() go through this one helper so
# there is a single corrected path, not two copies of the same ternary.
apa_md_count_matches <- function(pattern, txt) {
  res <- gregexpr(pattern, txt)[[1]]
  # NOT identical(res, -1L): gregexpr()'s result carries match.length/
  # useBytes attributes even on a no-match sentinel, so it is never
  # identical() to a bare -1L -- verified directly, this comparison silently
  # fell through to the buggy length() branch on every input. Compare the
  # VALUE, which strips attributes automatically.
  if (length(res) == 1L && res == -1L) 0L else length(res)
}
