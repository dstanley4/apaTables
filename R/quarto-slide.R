# The slide destination.
#
# Slides are the one destination with NO apaquarto: revealjs is plain
# Quarto, so there is no float, no automatic table numbering and no
# apa-note mechanism. The `::: {.apa-table-meta}` div every other destination
# relies on is populated by apatablemeta.lua, registered in apaquarto's own
# _extension.yml -- a deck loads no such filter, so the div is genuinely
# inert here and all furniture is emitted as ordinary markdown instead.
# That asymmetry is the design and must not be "unified" away.

# The raw pandoc token a deck reports, MEASURED:
# knitr::pandoc_to() returns exactly "revealjs" for a reveal.js deck,
# distinctly from "html" and "docx". beamer is deliberately absent -- LaTeX
# slides, no apaquarto, untested, and out of scope. It is already
# claimed by apa_quarto_actual_entry() as "pdf", which is where it should
# stay: a beamer document compiles through the LaTeX writer and the LaTeX
# block is the right thing to hand it.
#
# "pptx" is deliberately NOT in this vector. It is not a second spelling of
# the same destination: serving PowerPoint properly through pandoc was
# measured to need three separate mechanisms, one of which must reach into
# the rendered file after Quarto finishes, so PowerPoint is kept off the
# Quarto path entirely rather than served badly. Quarto slide support is
# revealjs only; PowerPoint is planned as a separate route (via officer, not
# pandoc's pptx writer). apa_quarto_dispatch()
# (R/quarto-table.R) checks `actual == "pptx"` directly to tell a caller
# that, and it renders the table anyway rather than refusing -- see
# the comment there. Do not add "pptx" back to this vector as a fix for
# anything.
#
# This vector names the raw pandoc tokens that mean the slide DESTINATION,
# so it takes the destination's name rather than "slide" -- unlike the
# fitting-math helpers below, which are about laying a table out on a slide
# and keep their names.
APA_REVEALJS_FORMATS <- c("revealjs")

apa_quarto_revealjs_target <- function() {
  to <- apa_quarto_actual_format()
  if (is.na(to) || !nzchar(to)) return(NA_character_)
  if (to %in% APA_REVEALJS_FORMATS) return(to)
  NA_character_
}

# --- Furniture as ordinary markdown, opt-in -------------------------------
#
# Every other destination reaches title and note through the
# `::: {.apa-table-meta}` div that apatablemeta.lua populates -- see
# apa_meta_div_for() (R/quarto-table.R). A deck loads no apaquarto extension
# and so runs no such filter, so that div would arrive as inert literal
# text. Slides therefore need the SAME two values reached the SAME way, but
# emitted as plain markdown text a caller places directly on the slide,
# never as a div attribute.
#
# One argument, three accepted types. TRUE means "the value apaTables
# generated"; a string or a richtext means that value; FALSE and NULL both
# mean "emit nothing", which is the DEFAULT -- a slide gets the table alone.
#
# The known, accepted cost: a correlation table prints ".83**" in its
# cells and the sentence defining "**" lives in the note, so the default
# shows a symbol whose definition has been removed. note = TRUE is the
# remedy and the help page says so. The opt-in exists because the
# alternative is that people retype notes by hand, which is exactly how the
# shipped example documents came to print "df-Num" where the package
# produces df with a subscript.
#
# apa_md_value() (R/quarto-table.R:893) already resolves the string and
# richtext cases exactly as the manuscript path does -- reused rather than
# reimplemented so the two destinations cannot drift. The logical case is
# the only thing slides add.
apa_slide_resolve <- function(value, generated) {
  if (is.null(value)) return("")
  if (is.logical(value)) {
    if (length(value) != 1L || is.na(value)) {
      stop("`title` and `note` must be TRUE, FALSE, a string, or NULL.",
           call. = FALSE)
    }
    return(if (isTRUE(value)) generated else "")
  }
  md <- apa_md_value(value)
  if (is.null(md)) "" else md
}

apa_slide_furniture <- function(x, title = NULL, note = NULL) {
  sp <- as_spec(x)

  gen_title <- if (inherits(sp$title, "apa_richtext")) rt_to_markdown(sp$title)
               else as.character(sp$title)

  # THE DIVERGENCE FROM EVERY OTHER DESTINATION, and it is deliberate.
  # sp$notes is a LIST, and apa_meta_div_for() hands it to apa_note_attr()
  # so that apaquarto's utilsapa.lua can split it back apart on the literal
  # '","' and set each part as its own paragraph. A DECK LOADS NO SUCH
  # FILTER: that attribute would arrive as inert text, and a vector returned
  # from here would emit as separate markdown lines, which is how the
  # significance-star sentence silently ends up on its own slide line. So
  # the parts are joined into ONE flowing markdown paragraph here.
  gen_note <- paste(vapply(sp$notes, rt_to_markdown, character(1)),
                    collapse = " ")

  list(title = apa_slide_resolve(title, gen_title),
       note  = apa_slide_resolve(note,  gen_note))
}

# --- The fitting model ----------------------------------------------------
#
# The slide geometry and the fitting model, ALL MEASURED on rendered decks.
# Nothing here is chosen.
#
# Reveal's slide box is 1050 x 700 logical units and its body text is 40px
# (default theme, quarto 1.9.38). A deck may override the box in its own
# YAML; these are the defaults, used because R cannot see the deck's config
# from inside a chunk.
# There is deliberately NO width constant: measured, the table saturates at
# the container (997 of the 1050 units) and stops shrinking, so height is
# the sole binding constraint and there is no width ladder to build here.
# This is the opposite of LaTeX and Word, where width was
# the entire problem -- do not port a width model across.
APA_SLIDE_HEIGHT_UNITS <- 700
APA_SLIDE_BODY_PX      <- 40
APA_SLIDE_BODY_PT      <- APA_SLIDE_BODY_PX * 0.75   # CSS: 1pt = 4/3 px

# CONSTRAINT 1, HEIGHT. A ROW costs line-height (1.3em) plus cell padding
# (0.2em top and bottom); an EXTRA LINE inside a cell costs only
# line-height. That is why one constant cannot serve both, and why a model
# calibrated on correlation matrices (many 2-line cells) mispredicts
# regression tables (all 1-line cells). Measured within 1.5% of the
# predicted height on all five reference tables.
#
# Both are pure multiples of the type size because CSS padding is em-based
# and SCALES with it, unlike Word's fixed cell padding -- so
# there is no padding-aware correction to make here.
APA_SLIDE_ROW_FACTOR  <- 1.60
APA_SLIDE_LINE_FACTOR <- 1.45

# CONSTRAINT 2, THE WRAP CAP -- and it is the one that actually binds for the
# common reference tables (the regression table and the blocks
# regression table). Past this size the table is wider than the slide, gets
# squeezed, and its cells wrap onto extra lines, inflating height by up to
# 56% and invalidating constraint 1 entirely.
#
# WITHOUT THIS, THE HEIGHT MODEL RETURNS 34px FOR A 2-PREDICTOR REGRESSION
# TABLE WHOSE TRUE FITTING SIZE IS 21px -- a slide overflowing by half, with
# no error.
#
# TWO TERMS, NOT ONE. Width is characters PLUS per-column padding, and the
# padding term cannot be folded into the character one: CSS cell padding is
# em-based (0.5em each side), so a table of FEW columns with SHORT content
# is padding-dominated. A single character coefficient fitted on the
# regression tables over-predicts the 2-way spanner table's limit by 60%
# (54.5px against a measured 34px) -- i.e. it would set that table half
# again too large and run it off the side of the slide.
#
# MEASURED IN RENDERED CHARACTERS, NOT THE GRID'S SOURCE WIDTHS. The
# grid width apa_md_widths() returns is scaled so each cell can hold its own
# MARKUP, and that scale varies per table -- the ANOVA table is 142 scaled
# characters for 42 rendered ones, because `*eta*^2^~p~` is 9 source
# characters for 3 drawn ones. Feeding the scaled figure in predicted 1.57x
# that table's real width and set it at 30px where 47px fitted.
#
# Refitted across all seven table types; worst error 6.2%:
#   cor (90 rendered chars, 10 col)   predicted 46.6  measured 43.9
#   reg-2 (85, 9)                     predicted 43.4  measured 44.3
#   reg-blocks (104, 10)              predicted 51.8  measured 53.2
#   aov (42, 8)                       predicted 26.2  measured 25.9
#   1-way (17, 3)                     predicted 10.3  measured 10.7
#   2-way spanner (48, 9)             predicted 29.7  measured 30.5
#   d values (46, 5)                  predicted 23.7  measured 23.3
#
# The container is 1008 units -- the table saturates there and stops
# widening.
APA_SLIDE_CONTAINER_UNITS <- 1008
APA_SLIDE_CHAR_UNITS      <- 0.370
APA_SLIDE_COL_UNITS       <- 1.33

# The share of the slide the table may fill, leaving the rest for the
# speaker's own text -- which R cannot see, because it is written after the
# chunk. 0.78 leaves roughly three lines of body text: 1 - (3 * 1.3 * 40)/700.
#
# Deliberately not a tighter 0.55: for both regression tables the WRAP
# CAP binds and this fraction does not, so 0.55 would make the tall
# tables smaller for no gain while changing the common case not at all. This
# is a backstop for tall-and-narrow shapes (large correlation matrices),
# not the primary control.
APA_SLIDE_BUDGET_FRACTION <- 0.78

# Rows and the EXTRA lines inside their cells, counted separately -- the two
# things that cost height, and they cost different amounts. Returns
# c(rows = , extra = ). This is the same line count apa_markdown_table()
# will actually emit, because both read apa_md_cell_lines().
apa_slide_row_lines <- function(plan) {
  per_row <- function(cells, get) vapply(cells, function(cell)
    length(apa_md_cell_lines(get(cell))), integer(1))

  hdr <- vapply(plan$header, function(r)
    max(per_row(r$cells, function(cell) cell$rt)), integer(1))
  bdy <- vapply(plan$body, function(e) {
    if (identical(e$type, "group")) return(1L)
    max(per_row(e$cells, identity))
  }, integer(1))

  tall <- c(hdr, bdy)
  c(rows = length(tall), extra = sum(tall - 1L))
}

# The emitter's own column widths, in characters -- the input to the wrap
# cap. Read from apa_md_widths() rather than recomputed.
#
# NOT apa_md_widths(). That function answers a DIFFERENT question --
# how wide must the grid SOURCE cell be to hold its own markdown -- and
# scales every column by `max(markup/rendered)` to guarantee it. That factor
# varies per table: the ANOVA table comes out 142 characters wide for 42
# characters of drawn text, because `*eta*^2^~p~` is 9 source characters for
# 3 rendered ones. Using it over-predicts that table's width by 1.57x and
# sets it at 30px where 47px fits.
#
# What the wrap cap needs is the width actually DRAWN, so the markup is
# stripped with apa_md_plain_lines() -- which also disposes of the
# rules = TRUE/FALSE question entirely, since the ApaTableRule fence is
# markup and never appears in a plain line.
apa_slide_char_width <- function(plan) {
  n <- plan$n_col
  w <- integer(n)
  bump <- function(j, cell)
    w[j] <<- max(w[j], max(nchar(apa_md_plain_lines(cell))))

  # Walk EVERY header row, not only the last. A spanner label ("No alcohol
  # whatsoever") sits in a NON-final header row, and reading only
  # `plan$header[[length(plan$header)]]` makes it invisible to the wrap cap
  # entirely -- two structurally identical tables differing only in
  # spanner-label length would return the IDENTICAL char_width and font
  # size. Follow apa_md_span_widen()'s reasoning
  # (R/render-markdown.R), already built for the same problem on the grid's
  # own source widths: a `span == 1` cell adds to its own column as before;
  # a `span > 1` cell is a constraint on the TOTAL width of the columns it
  # covers, closed by widening just those columns when they fall short of
  # the label. NEVER add the label's full length to every column under it --
  # that kind of per-column overhead is what crushes a real table's column
  # ratio from 10:1 to 2:1 (measured in Word), and it is as wrong here.
  for (row in plan$header) {
    at <- 1L
    for (cell in row$cells) {
      span <- as.integer(cell$span)
      if (span == 1L) {
        bump(at, cell$rt)
      } else {
        need <- max(nchar(apa_md_plain_lines(cell$rt)))
        to <- at + span - 1L
        have <- sum(w[at:to])
        if (need > have) w <- apa_md_widen_range(w, at, to, need - have)
      }
      at <- at + span
    }
  }
  for (row in plan$body)
    if (identical(row$type, "data"))
      for (j in seq_len(n)) bump(j, row$cells[[j]])
  sum(pmax(w, 1L))
}

apa_slide_font_pt <- function(plan, font.size = NULL) {
  # An explicit size is honoured EXACTLY, whatever it is. CSS can
  # be exact, so unlike Word (which must snap to the five cloned table
  # styles that exist) nothing is rounded, snapped or refused here.
  #
  # But it is CHECKED: without the check, apa.quarto.revealjs(x,
  # font.size = 2) would put a 2 pt table on a slide with nothing said
  # anywhere. The shared RULE is "honour it
  # exactly, and say so when it is below the size an APA table stops being
  # readable at"; the delivery is not shared, which is why this calls
  # the check directly rather than routing through apa_quarto_font_size().
  if (!is.null(font.size)) {
    size <- as.numeric(font.size)[1]
    apa_quarto_warn_below_floor(size)
    return(size)
  }

  n <- apa_slide_row_lines(plan)
  k <- APA_SLIDE_ROW_FACTOR * n[["rows"]] + APA_SLIDE_LINE_FACTOR * n[["extra"]]

  by_height <- (APA_SLIDE_HEIGHT_UNITS * APA_SLIDE_BUDGET_FRACTION) / max(k, 1)
  by_width  <- APA_SLIDE_CONTAINER_UNITS /
               (APA_SLIDE_CHAR_UNITS * max(apa_slide_char_width(plan), 1) +
                APA_SLIDE_COL_UNITS  * max(plan$n_col, 1))

  # THE SMALLER OF THE TWO, ALWAYS. by_width is what stops the height model
  # returning 34px for a regression table that fits at 21px; by_height
  # is what stops a tall correlation matrix running off the bottom. Either
  # can bind, and for the regression reference tables it is by_width.
  px <- min(by_height, by_width)

  # Never larger than the deck's own body text: a table set bigger than the
  # prose around it is not a thing to ship, and it also keeps the returned
  # value inside the regime the constants were measured in.
  px <- min(px, APA_SLIDE_BODY_PX)
  px * 0.75
}

# --- The size wrapper and the APA furniture CSS ---------------------------

# The fenced div carrying an inline font size. CONTINUOUS, rather than
# reveal's binary `.smaller` class, so a 6-row table and a 20-row table each
# get the size they need.
#
# This is NOT the per-cell span mechanism the Word path rejects, and the
# difference is the whole point: that one puts a custom-style span around
# EVERY CELL, where as a near-constant per-column overhead it crushed a real
# table's column ratio from 10.01:1 to 2.12:1 and made Word break
# `Organizational_Citizenship_Behavior` mid-word (measured). THIS div wraps
# the WHOLE TABLE once, so it cannot act as a per-column floor. Measured on
# this destination: a font-size on the wrapper propagates and
# genuinely reflows the table, and CSS cell padding is em-based so it scales
# with the type. Do not "fix" this by analogy with the Word path.
#
# The blank lines are load-bearing. Without one before it, pandoc swallows a
# `:::` fence into the block above rather than treating it as a fence
# (measured); without the CLOSER, the user's own text below the chunk
# is absorbed into the div and silently inherits the shrunken type
# (constructed and watched).
APA_SLIDE_TABLE_CLASS <- "apa-slide-table"

apa_slide_size_wrap <- function(body, pt) {
  # round(, 2) FIRST, and it is not cosmetic. The automatic size is a
  # continuous quotient -- 1008/(0.370*104+1.33*10)*0.75 is 14.600232... --
  # and format()'s default 7 significant digits would write
  # `font-size: 14.60023pt` into the user's deck. Two decimals is finer than
  # any display can resolve and keeps an exact request exact: 12 stays "12",
  # 8.5 stays "8.5" (exact size wherever the destination allows it).
  size <- format(round(pt, 2), trim = TRUE, drop0trailing = TRUE,
                 scientific = FALSE)
  paste0("::: {.", APA_SLIDE_TABLE_CLASS,
         ' style="font-size: ', size, 'pt;"}\n\n',
         body, "\n\n:::\n")
}

# APA's furniture, which a plain deck does NOT supply. These are
# apaquarto's own rules from apa.css:72-102, re-scoped to the wrapper class
# above -- apaquarto keys on Bootstrap's `.table`, which a reveal table does
# not carry (its class is `caption-top`, measured), and scoping to our own
# class also means this can never restyle a table the user wrote by hand.
#
# Two rules are additions rather than ports, both found by reading a
# rendered slide:
#   vertical-align  -- th defaults to `baseline`, so when ONE label wraps to
#                      two lines (`Unique R2` does, in both regression
#                      tables) every single-line label floats at the top of
#                      the row instead of sitting on the rule.
#   tabular-nums    -- apaquarto sets it; a deck does not.
#
# The ApaTableRule selector is what makes a spanner's cmidrule appear.
# Measured on a deck: the divs are emitted and their computed border is
# `0px none` without this line and `1px solid` with it -- the same fix, on
# the same markup, that the HTML path needs. It is why the slide path keeps
# rules = TRUE.
#
# ```{=html}` is the delivery mechanism and it is load-bearing: measured, a
# raw HTML block reaches the revealjs output and is SILENTLY DROPPED by the
# pptx writer. So there is no format branch here, and no way for a <style>
# to print as visible text on a PowerPoint slide.
apa_slide_css <- function() {
  s <- paste0(".", APA_SLIDE_TABLE_CLASS)
  paste0(
    "```{=html}\n<style>\n",
    s, " table { border-collapse: collapse;\n",
    "  border-top: 1px solid currentColor;\n",
    "  border-bottom: 1px solid currentColor;\n",
    "  font-variant-numeric: tabular-nums; }\n",
    s, " table > thead { border-bottom: 1px solid currentColor; }\n",
    s, " table > :not(caption) > * > * { border-bottom: none; }\n",
    s, " table th { vertical-align: bottom; }\n",
    s, " table [data-custom-style=\"ApaTableRule\"] {",
    " border-bottom: 1px solid currentColor; }\n",
    # Found by reading an exported PDF of a deck. Pandoc turns a
    # grid table's column widths into a <colgroup> of PERCENTAGES, which
    # force the table to fill its container: the ANOVA table's natural width
    # is 784 slide units and it was drawn at 1229, so 36% of the table was
    # slack handed out in proportion -- and the right-aligned `90% CI`
    # column's share of it stranded the value at the far edge, away from the
    # statistic it qualifies.
    #
    # `width: auto` and `fit-content` ON THE TABLE change nothing; measured,
    # all three give an identical layout, because the col percentages are
    # what stretch it. The override must target the col elements themselves.
    s, " table { width: auto !important; }\n",
    s, " col { width: auto !important; }\n",
    "</style>\n```\n")
}

# --- apa.quarto.revealjs() -- assemble the block --------------------------
#
# Assembly order: title (if any), then the table (wrapped and styled), then
# the note (if any). All three are ordinary
# markdown, separated by blank lines -- there is no float and no
# `.apa-table-meta` div on a deck (see the file banner), so this function
# must never compose one.
#
# This function is reached ONLY for revealjs: apa_quarto_revealjs_target()
# never returns "pptx", so the wrap-and-CSS step below runs
# UNCONDITIONALLY. Do not reintroduce a pptx branch here. Pandoc's pptx
# writer silently drops a `Table` nested inside a `Div`, and CSS is inert on
# PowerPoint -- neither fact bears on revealjs. A pptx render falls through
# to apa_quarto_markdown_block() (R/quarto-table.R, shared with docx/html),
# and THAT is where apa_slide_flatten_spanners() (below) is called, not
# here. See that function's own comment for why.
#
# Named after the destination, following the same
# apa_quarto_<format>_block() convention as apa_quarto_latex_block()
# and apa_quarto_markdown_block().
apa_quarto_revealjs_block <- function(x, font.size = NULL,
                                      title = NULL, note = NULL) {
  plan <- apa_display_plan(as_spec(x))

  # rules = TRUE. Do not pass FALSE on the theory that "ApaTableRule is Word
  # vocabulary a deck cannot draw" -- that is backwards: the divs are
  # emitted on a deck with a computed border of `0px none` (the same state
  # HTML is in without its selector), and apa_slide_css() supplies the
  # selector that draws them. With it, spanner cmidrules render correctly
  # on slides.
  #
  # font_size = NULL for the same reason the manuscript path passes NULL:
  # per-cell markup must never reach the emitter; the size is carried by the
  # wrapper div instead.
  body <- apa_markdown_table(plan, font_size = NULL, rules = TRUE)

  pt   <- apa_slide_font_pt(plan, font.size = font.size)
  body <- apa_slide_size_wrap(body, pt)
  # The CSS rides with EVERY table, not once per render. apa_warn_once()'s
  # registry keys on the DOCUMENT, so a second render of the same deck in
  # one R session would emit the style block once and silently skip it
  # afterwards, leaving tables unstyled with nothing reported. CSS is
  # idempotent and this is a few hundred bytes: duplicating it costs
  # nothing and removes that whole class of bug.
  css <- apa_slide_css()

  f <- apa_slide_furniture(x, title = title, note = note)
  parts <- c(if (nzchar(css)) css,
             if (nzchar(f$title)) f$title, body,
             if (nzchar(f$note)) f$note)
  knitr::asis_output(paste0(paste(parts, collapse = "\n\n"), "\n"))
}

# --- Spanner flattening for PowerPoint only -------------------------------
#
# PowerPoint drops column spans outright -- MEASURED:
# gridSpan count 0, hMerge count 0, and a nine-column two-way table came back
# as row0 = ['', 'alcohol', '', '', '', '', '', '', ''], row1 = ['', 'None',
# '', '2 Pints', '', '4 Pints', '', 'Marginal', ''] -- `alcohol` sitting in
# column 2 instead of spanning six, `None` sitting over *M* alone instead of
# over *M* and *SD* together. Exit 0, no warning: a table that reads as if
# the numbers belong to different conditions than they do. So for pptx the
# spanner rows are flattened here, BEFORE the grid table is emitted, rather
# than emitting a span pandoc's pptx writer will silently misplace.
#
# revealjs KEEPS spans (colspan="2" x4, colspan="6" x1, measured on the
# same table), so this is NEVER applied there -- flattening a span that
# already works would throw away correct output. This is a pptx fix, not a
# "slides" fix; do not generalise it.
#
# Do not delete this on the theory that an unsupported PowerPoint table is
# merely ugly-but-correct. Without it a real render is WRONG: `alcohol` sits
# over the `M` of "None" instead of spanning six columns -- the defect
# recorded above. "We do not support PowerPoint" and "we do not emit a lying
# table" are different commitments, and both hold: PowerPoint remains
# not-yet-supported (the warning fires from apa_quarto_render() in
# R/quarto-table.R) but the table it renders anyway must never mislabel its
# own columns.
#
# WHERE IT IS CALLED. A pptx render never reaches
# apa_quarto_revealjs_block() above -- it falls through to
# apa_quarto_markdown_block() (R/quarto-table.R), the shared docx/html
# emitter, because neither the revealjs check nor the LaTeX check claims it.
# apa_quarto_markdown_block() is therefore where this function is called,
# gated on a pptx target the same way that function already gates its
# own docx-only and HTML-only steps.
#
# The label goes in the MIDDLE column of its group, not the first: that is
# the one change that makes the flattened row read correctly instead of
# merely being span-free -- with the label first, a two-column group would
# still read as though it belonged to the first of its two columns, which is
# the exact mislabeling this function exists to remove. For an even-width
# group there is no exact middle, so the left of the two central columns is
# used: offset (n - 1) %/% 2 from the group's start.
apa_slide_flatten_spanners <- function(plan) {
  has_span <- any(vapply(plan$header, function(r)
    any(vapply(r$cells, function(c) c$span > 1L, logical(1))), logical(1)))
  if (!has_span) return(plan)

  empty <- function(align) list(rt = richtext(""), span = 1L, align = align)

  plan$header <- lapply(plan$header, function(row) {
    cells <- list()
    for (cell in row$cells) {
      # as.integer() is defensive, not decorative. The other span readers
      # (apa_md_span_widen() in R/render-markdown.R, apa_slide_char_width()
      # above) coerce cell$span before arithmetic too. With a DOUBLE span,
      # `identical(k, mid)` below compares a double loop index against an
      # integer `k <- seq_len(n) - 1L` (still double if n is double) -- the
      # comparison can silently never match, so every spanner label comes
      # back BLANK. Measured. Same family as the double-vs-integer trap of
      # floor() vs %/%. It cannot fire today (every span this package produces is
      # already integer); make it unable to fire ever.
      n <- as.integer(cell$span)
      if (n <= 1L) {
        cells[[length(cells) + 1L]] <- cell
      } else {
        mid <- (n - 1L) %/% 2L                # 0-based offset of the middle
        for (k in seq_len(n) - 1L) {
          cells[[length(cells) + 1L]] <- if (identical(k, mid)) {
            list(rt = cell$rt, span = 1L, align = cell$align)
          } else {
            empty(cell$align)
          }
        }
      }
    }
    # The cmidrule ranges are dropped HERE ONLY, and only because this
    # function runs for pptx alone. PowerPoint ignores CSS entirely, so
    # apa_slide_css()'s ApaTableRule selector can never reach this markup --
    # the ranges would be dead data. On revealjs the ranges are KEPT and they
    # draw; do not generalise this line into "slides have no
    # cmidrules".
    list(cells = cells, rules = list())
  })
  plan
}

# --- The exported entry point ---------------------------------------------
#
# A thin wrapper over apa_quarto_dispatch(), exactly like its three siblings
# in R/quarto-table.R (apa.quarto.pdf/docx/html) -- the mismatch message and
# the actual-vs-called comparison are already general over "called" and need
# nothing slide-specific.
#
# --- The roxygen ----------------------------------------------------------
#
# Full prose lives in the @section below, not crammed into @param -- the
# @param text for title/note/font.size is the block shared with
# apa.quarto.pdf()/docx()/html() in R/quarto-table.R, and the things that
# make slides different (opt-in furniture, no float, the fitting model) do
# not fit there without
# contradicting what that shared text says about the other three
# destinations. Real limits a user would otherwise meet as a surprise: no
# table number, ever; title/note opt-in rather than automatic, which
# matters specifically for a correlation table's significance stars.
#
# POWERPOINT IS NOT CLAIMED HERE AS A SUPPORTED DESTINATION, and the reason
# is NOT "font.size has no lever there": measured on a real render,
# PowerPoint honours
# an explicit `<a:rPr sz="...">` exactly (monotonic across four declared
# sizes, glyph-extent verified with pdftotext -bbox). The obstacle is that
# pandoc's pptx writer never emits one and gives no per-table hook to add
# one -- the size mechanism would have to reach into the RENDERED .pptx
# file AFTER Quarto finishes, a kind of intervention no other destination
# in this package needs (every other one is served by a Lua filter INSIDE
# the render). Size alone would not be sufficient
# regardless: the narrowest column still breaks its content at any size
# tested (a width problem), and a two-block
# regression table runs off the bottom of a slide at any size (a physical
# constraint, not a bug to fix here). Do NOT write "no per-table size
# control, so font.size is inert" in the @section prose below -- that claim
# is false.
#
# PowerPoint is planned as a separate route written directly with officer,
# with no pandoc pptx writer involved. Until then the documentation's whole
# obligation is one honest sentence: PowerPoint is "not yet supported",
# never "unsupported". The @section below says exactly that.
#
# The other half of the honesty this page owes: a document that
# renders to pptx anyway still gets a table, and its spanning headers are
# no longer left to mislabel their own columns: apa_slide_flatten_spanners()
# (this file) is called from apa_quarto_markdown_block()
# (R/quarto-table.R) for a pptx target, before the not-yet-supported
# warning ever fires. Do NOT say headings are misplaced on a PowerPoint
# render below -- with the flattening restored, they are not; only the
# STYLE (PowerPoint's own default table look, not APA format) is left
# unaddressed.
#' @section Slides are different:
#' \code{apa.quarto.revealjs()} places a table on a slide rather than a
#' manuscript page, and it differs from \code{apa.quarto.pdf()} /
#' \code{apa.quarto.docx()} / \code{apa.quarto.html()} in ways worth knowing
#' before you rely on it.
#'
#' \strong{revealjs is the supported slide format.} Set \code{format:
#' revealjs} in the document's YAML -- everything below describes that
#' format. \strong{PowerPoint is not supported yet.} The obstacle is not something a PowerPoint template or setting can fix.
#' Quarto renders PowerPoint through pandoc, and pandoc's PowerPoint writer gives no way to
#' set a table's text size from the document, so a table placed on a
#' PowerPoint slide comes out at PowerPoint's own type size. Even at a
#' smaller size, a larger table (a two-block regression table, for instance)
#' is taller than a slide. Support for PowerPoint is planned for a future
#' release; apaTables has no working route to a PowerPoint table yet. If a
#' document using \code{apa.quarto.revealjs()} renders to PowerPoint anyway,
#' the table still appears -- styled only by PowerPoint's own default table
#' look, not APA format -- and a warning says PowerPoint is not yet
#' supported and names \code{format: revealjs} as the supported
#' destination.
#'
#' \strong{There is no table number, ever.} A manuscript has a running list
#' of tables to number against -- "Table 1", "Table 2", and so on. A slide
#' deck has no such list, so \code{apa.quarto.revealjs()} never adds a
#' number.
#'
#' \strong{Title and note are opt-in, not automatic.}
#' \code{apa.quarto.revealjs(x)} on its own puts the table alone on the
#' slide, nothing else. Pass \code{title = TRUE} and/or \code{note = TRUE}
#' to add the title and note apaTables generated for the table -- the same
#' arguments work here as on \code{apa.quarto.pdf()} and its siblings. This
#' matters most for a correlation table: its cells mark a significant
#' correlation with \code{**}, and the sentence explaining what \code{**}
#' means (\code{* indicates p < .05. ** indicates p < .01.}) lives in the
#' note. Left at the
#' default, a correlation table on a slide shows the stars with their
#' meaning nowhere on the slide -- \code{note = TRUE} is the remedy:
#'
#' \preformatted{my_table <- apa.cor.table(attitude)
#' apa.quarto.revealjs(my_table, note = TRUE)}
#'
#' \strong{An explicit \code{font.size} is honoured exactly}, the same value
#' you typed, though a size below 6 points -- where an APA table stops being
#' comfortable to read -- warns in the rendered draft. Left at the default,
#' apaTables shrinks the table automatically to fit the slide.
#'
#' Apart from the number, title, note, and sizing, a slide table is the
#' same table every other destination draws: identical values, identical
#' formatting.
#' @rdname apa.quarto.pdf
#' @export
apa.quarto.revealjs <- function(x, title = NULL, note = NULL,
                                font.size = NULL) {
  apa_quarto_dispatch(x, called = "revealjs", title = title, note = note,
                      font.size = font.size)
}
