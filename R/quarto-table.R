# The public chunk function and the layout ladder.
#
# apa_display_plan() gives the structure, R/render-latex.R prices it at a font
# size and emits the grid, and apaquarto -- patched in
# inst/_extensions/wjschne/apaquarto/ -- supplies the caption, the number, the
# note and the page. R decides the size and the rotation; apaquarto executes
# both. Nothing here writes a float, a caption or a landscape environment.

# The default type size for a Quarto PDF table.
#
# Not 12 pt, and the reason is stated because it will be queried: an
# apaTables Quarto manuscript goes to REVIEWERS, who read on screen and can
# zoom losslessly, and 10 pt keeps about 19% more content upright than 12 pt
# would, so fewer tables rotate at all. APA's tables guidance says only "use
# the same font in tables as in the rest of your paper" and gives no type SIZE
# for tables; the widely quoted "8 pt minimum" is a misattribution of the
# FIGURES rule.
APA_QUARTO_DEFAULT_SIZE <- 10

# The size below which the ladder warns. It is a WARNING THRESHOLD, and
# NOTHING ELSE STOPS HERE: the ladder walks straight past it, below 5 pt and
# below 1 pt if that is what fitting takes, down to
# APA_LATEX_MIN_EMITTABLE_SIZE. A reviewer can zoom into 3 pt type but cannot
# recover a column that is not in the file.
#
# Do not reintroduce a hard clamp at this size (or at 5 pt) in
# apa_size_for_width() or apa_latex_table(): a table needing 4 pt would then
# be set at 5 pt and clipped, while the help page promises nothing is ever
# cut off. The only clamp is a degeneracy guard at 0.5 pt, measured (see
# APA_LATEX_MIN_EMITTABLE_SIZE).
#
# Given the ladder's reach (about 17 in of 12-pt-equivalent content on a
# rotated page at 6 pt) this should essentially never fire -- and if it fires
# on ordinary data the width model is wrong, not the data, which is why it is
# a real warning and not a silent adjustment.
APA_QUARTO_READABLE_SIZE <- 6

# The largest cloned table style in the bundled reference document.
# Word's sizes must exist as styles; none can be invented at render time, so
# this is a real ceiling, not a preference. Kept honest against the binary by
# tests/testthat/test-word-size-styles.R.
APA_QUARTO_MAX_SIZE <- 12

# The FIXED padding a Word table cell carries on each side, in inches -- the
# bundled styles' <w:tblCellMar> of 15 twips a side (1440 twips to the inch),
# so one column costs 2 x 15 twips whatever the type size. The ladder charges
# the Word destination for it via apa_layout_decide()'s col_overhead_in.
# Ignore it and the ladder shrinks a dense table until it JUST fits the page,
# then the padding pushes it back over -- measured: every table the ladder
# shrinks lands immediately below budget by construction, and the widest
# column pays, wrapping its labels ("10. Var10" split across two lines while
# "9. Var9" fit). Kept honest against the binary reference document by
# tests/testthat/test-render-markdown.R.
APA_WORD_CELL_PADDING_IN <- (2 * 15) / 1440

# The ONE warning that stays true whatever size a table is set at: a
# table's `\tabcolsep` and per-column rounding cost a fixed amount of width
# no font size changes, so past about 136 columns on a rotated page (fewer
# upright) no size -- automatic OR explicit -- can fit it. Shared by the automatic
# ladder's `!fits` branch and the `size =` override branch so both raise
# byte-identical text instead of two copies that can drift.
#
# ONE FLOWING SENTENCE, no newline anywhere in it -- knitr wraps a
# single-line message and TRUNCATES a multi-line one off the right edge of a
# knitted PDF page, where pdftotext still reports the text that is no longer
# visible, so a newline here is a defect grep cannot catch.
apa_ladder_no_fit_message <- function(content_12, budget, rotate, size) {
  sprintf(paste0(
    "This table has about %d columns, and the space between columns does ",
    "not shrink with the type, so no font size can fit it on the %.1f ",
    "inch %s page: it has been set at %s pt and its right-hand columns ",
    "will still be cut off -- save it as a Word file instead, where ",
    "nothing is cut off: apa.save(filename = \"mytable.docx\", ",
    "your_table)."),
    length(content_12), budget,
    if (rotate) "rotated (landscape)" else "upright",
    apa_latex_num(size))
}

#' Decide the type size and the page orientation for one table
#'
#' The layout ladder, run on an exact width rather than a search: 10 pt
#' upright; if that does not fit, ROTATE and stay at 10 pt; if it still does not fit, shrink by closed-form calculation. If that goes
#' below 6 pt, use it anyway so that nothing is ever cut off, and warn.
#'
#' @section Why the fit is re-checked rather than assumed:
#' `apa_size_for_width()` bottoms out at `APA_LATEX_MIN_EMITTABLE_SIZE` and
#' returns that size even when it does not fit -- which it cannot, for a table
#' whose font-independent `\\tabcolsep` and per-column rounding alone exceed
#' the page. So the returned size is not evidence of a fit. This function
#' computes the width it actually gets and tests that, which is also what makes
#' the warning able to distinguish "small but complete" from "still too wide,
#' and no size can fix it".
#'
#' @section One measurement, not one per candidate:
#' `apa_latex_natural_width()` re-runs flextable's `autofit()` every time it
#' is called (measured: 0.66 s on a 14-column table). The per-column content
#' vector is therefore measured ONCE here, by `apa_latex_content_at_12()`, and
#' every candidate size -- including the one `apa_size_for_width()` solves for
#' -- is computed from that same vector.
#'
#' @param plan a display plan, from `apa_display_plan()`.
#' @param geom optional page geometry, from `apa_page_geometry("portrait")`;
#'   read from the document being rendered when not supplied.
#' @param geom_landscape optional rotated-page geometry, from
#'   `apa_page_geometry("landscape")`.
#' @param landscape Tri-state. `TRUE` forces a rotated page, `FALSE` forbids
#'   one (the table is shrunk to the upright page instead, keeping every
#'   column), `NA` (the default) lets measurement decide. Resolved by
#'   [apa_landscape_request()] from the chunk option and the builder argument
#'   before it reaches here.
#' @param can_execute_rotation `FALSE` when apaquarto cannot carry out a
#'   rotation on this render. A DIFFERENT fact from `landscape = FALSE`: one is
#'   the user declining a rotated page, the other is the render being unable to
#'   produce one, and only the first is anybody's choice.
#' @param size an explicit point size, overriding the whole
#'   rotate-then-shrink ladder -- because the caller already fixed the size,
#'   shrinking is no longer an option, so the ONLY question left
#'   is whether the page needs to rotate to hold it. `NULL` (default) runs
#'   the automatic ladder, and the override branch is not reached at all.
#'   Rotation is decided from the SIZE ACTUALLY EMITTED rather than the
#'   ladder's own 10 pt guess: without this re-check an explicit `font.size`
#'   would reach the LaTeX emitter unchecked -- `apa.cor.table(attitude)` at
#'   20 pt measures 10.26 in against a 6.5 in upright page, and would render
#'   with no warning and no rotation. When the page the table will land on
#'   (rotated, if rotation is allowed) cannot hold it at that size it warns, naming a smaller size that fits, or --
#'   for a table whose `\\tabcolsep` alone exceeds the rotated budget --
#'   saying that no size, explicit or automatic, can fit it.
#' @param col_overhead_in fixed per-column overhead in inches that the
#'   destination adds to every column whatever the type size, and that the
#'   content model therefore cannot see. `0` (the default) for LaTeX, whose
#'   own `\\tabcolsep` term is already in the model; the Word render passes
#'   `APA_WORD_CELL_PADDING_IN`, the bundled
#'   styles' fixed `<w:tblCellMar>`. Charged on the WIDTH side -- it is part
#'   of the space the table really occupies -- so every budget in a warning
#'   is still the true page width.
#' @return a list with `size` (points), `rotate` (logical), `fits` (did the
#'   chosen size actually fit its page) and `width` (the predicted width at
#'   `size`, in inches, `col_overhead_in` included).
#' @keywords internal
apa_layout_decide <- function(plan, geom = NULL, geom_landscape = NULL,
                              landscape = NA,
                              can_execute_rotation = TRUE,
                              size = NULL,
                              col_overhead_in = 0) {
  if (is.null(geom)) geom <- apa_page_geometry("portrait")
  if (is.null(geom_landscape)) geom_landscape <- apa_page_geometry("landscape")

  # The one thing the ladder cannot measure: whether the page it is measuring
  # against is the page the table will land on. apa_page_geometry() falls back
  # to the standard manuscript page when a document's YAML carries something it
  # cannot interpret, and that fallback must not be silent -- a table laid out
  # for 6.5 in on a page that is not 6.5 in wide is exactly the clipping the
  # ladder exists to prevent. Warn-once per document. This is the warning's
  # only caller: without it the fallback would go quiet.
  apa_warn_unknown_geometry(geom)

  # The other thing the ladder cannot measure: whether the table will keep
  # what it is given. In apaquarto's journal mode it may not, and that
  # loss is silent in the PDF, so it is stated here where every table passes.
  apa_warn_journal_mode(geom)

  upright <- geom$width
  # Rotation is worth asking for only where it buys width. In apaquarto's
  # journal mode it does not: apa7 fixes the two-column text block itself, a
  # rotated journal page keeps its two columns, and apafloatlatex.lua's
  # landscapewrap stands down there anyway -- so both geometries report the
  # same column width and this comparison quietly does the right thing
  # without journal mode needing a branch of its own.
  rotated <- geom_landscape$width
  landscape <- apa_landscape_value(landscape)
  # Two independent gates, deliberately not merged. A rotation nobody can
  # execute is worse than no rotation at all: the table is priced against the
  # wider page and then printed on the narrower one, and the columns past the
  # right edge are gone with no error and no warning. A rotation the USER
  # forbade is a different thing entirely -- it is honoured, not worked
  # around, and it is not a problem to be reported.
  rotate_allowed <- rotated > upright && isTRUE(can_execute_rotation) &&
    !identical(landscape, FALSE)

  content_12 <- apa_latex_content_at_12(plan)
  # The destination's fixed per-column cost rides on the WIDTH side of every
  # comparison (and is solved for by handing the size solver a target net of
  # it): padding is space the table really occupies, and it does not shrink
  # with the type, so a ladder that ignores it shrinks a table until it JUST
  # fits and the padding then pushes it back over.
  pad_total <- length(content_12) * col_overhead_in
  width_at <- function(s) apa_latex_width_from_content(content_12, s) + pad_total

  if (!is.null(size)) {
    # EXPLICIT SIZE OVERRIDE. Shrinking is not a lever the caller left
    # available -- the size is fixed -- so the only question is whether the
    # page has to rotate to hold it. Rotate-before-declare-broken, the same
    # policy as the automatic ladder: try the requested placement first
    # (upright, unless landscape asked for rotation up front), then rotate if
    # that alone is not enough, and warn only when even the rotated budget
    # cannot hold it. There is no "below the readable floor" check here:
    # that is apa_quarto_font_size()'s job for an explicit size, kept there
    # so it is not raised twice.
    w <- width_at(size)
    rotate <- isTRUE(landscape) && rotate_allowed
    budget <- if (rotate) rotated else upright
    if (w > budget && !rotate && rotate_allowed) {
      rotate <- TRUE
      budget <- rotated
    }
    fits <- w <= budget
    if (!fits) {
      # TWO DIFFERENT FACTS, and the message must state the right one. "No
      # font size can fit it" is true only in the degenerate \tabcolsep case;
      # for an ordinary wide table at an explicit size it is FALSE --
      # measured, an 11-variable correlation table fits a rotated page at
      # 10 pt, so telling a font.size = 12 caller that no size could fit it
      # would be wrong. Distinguish them by solving for the best size and
      # re-checking the fit at it, the same test the automatic ladder's own
      # warning already rests on.
      best <- apa_size_for_width(plan, budget - pad_total,
                                 content_12 = content_12)
      best_fits <- width_at(best) <= budget
      msg <- if (!best_fits) {
        # Genuinely unfittable at ANY size: the one warning whose text is
        # true whatever size was requested.
        apa_ladder_no_fit_message(content_12, budget, rotate, size)
      } else {
        # The REQUESTED size is the cause, so name it and name the fix. The
        # size offered is floored to one decimal so the value printed is
        # itself a size that fits (width is monotone in size). ONE FLOWING
        # SENTENCE, no newline -- knitr truncates a multi-line warning off
        # the page edge (see apa_ladder_no_fit_message()'s own comment).
        best_named <- floor(best * 10) / 10
        sprintf(paste0(
          "This table is about %.1f inches wide at the %s pt requested, ",
          "wider than the %.1f inch %s page, so its right-hand columns ",
          "will be cut off -- request font.size = %s or smaller, or remove ",
          "font.size so apaTables chooses a size that fits."),
          w, apa_latex_num(size), budget,
          if (rotate) "rotated (landscape)" else "upright",
          apa_latex_num(best_named))
      }
      warning(warningCondition(msg, class = "apaTables_ladder_size"))
    }
    return(list(size = size, rotate = rotate, fits = fits, width = w))
  }

  rotate <- isTRUE(landscape) && rotate_allowed
  budget <- if (rotate) rotated else upright
  w <- width_at(APA_QUARTO_DEFAULT_SIZE)

  if (w <= budget) {
    return(list(size = APA_QUARTO_DEFAULT_SIZE, rotate = rotate,
                fits = TRUE, width = w))
  }

  # ROTATE BEFORE SHRINKING, deliberately, rather than shrinking one notch
  # first. It follows APA's own text -- "if a table is too wide to fit
  # on one page, use landscape orientation on the page with the wide table" --
  # and it maximises type size, at the cost of more rotated pages.
  if (!rotate && rotate_allowed) {
    rotate <- TRUE
    budget <- rotated
    if (w <= budget) {
      return(list(size = APA_QUARTO_DEFAULT_SIZE, rotate = TRUE,
                  fits = TRUE, width = w))
    }
  }

  size <- apa_size_for_width(plan, budget - pad_total, content_12 = content_12)
  achieved <- width_at(size)
  fits <- achieved <= budget

  if (size < APA_QUARTO_READABLE_SIZE || !fits) {
    # ONE FLOWING SENTENCE, no newline anywhere in it. knitr wraps a
    # single-line message and TRUNCATES a multi-line one off the right edge of
    # a knitted PDF page, where pdftotext still reports the text that is no
    # longer visible -- so a newline here is a defect grep cannot catch.
    msg <- if (!fits) {
      apa_ladder_no_fit_message(content_12, budget, rotate, size)
    } else {
      # Every column is present and readable with a zoom -- a trade taken
      # deliberately: a reviewer can magnify small type but cannot
      # recover a column that is not in the file.
      sprintf(paste0(
        "This table has been set at %s pt, below the %s pt size APA table ",
        "text stays readable at, so that the whole of it fits the %.1f inch ",
        "%s page with no column cut off -- for a full-size version save it ",
        "as a Word file instead: apa.save(filename = \"mytable.docx\", ",
        "your_table)."),
        apa_latex_num(size), apa_latex_num(APA_QUARTO_READABLE_SIZE), budget,
        if (rotate) "rotated (landscape)" else "upright")
    }
    # A distinct condition CLASS, not a plain warning(), so a caller that
    # wants to replace THIS specific message (the markdown/docx path, whose
    # wording is wrong here) can muffle it selectively with a class-named
    # handler (withCallingHandlers(..., apaTables_ladder_size = fn)). Never
    # use a bare `warning = fn` handler for that: it catches every warning
    # this function can raise INDISCRIMINATELY -- including
    # apa_warn_unknown_geometry()'s and apa_warn_journal_mode()'s calls just
    # above, which route through apa_warn_once() (R/apaquarto-version.R).
    # That helper assign()s its "already warned" flag BEFORE calling
    # warning(), so muffling it does not just suppress one message -- it
    # permanently burns the once-per-render key, silencing that warning for
    # the REST of the render even on a later, completely unguarded call
    # (measured).
    warning(warningCondition(msg, class = "apaTables_ladder_size"))
  }

  list(size = size, rotate = rotate, fits = fits, width = achieved)
}

# THE TRI-STATE. TRUE forces rotation, FALSE forbids it, NA defers
# to whatever is below it in the precedence -- ultimately to measurement.
#
# Anything that is not a length-1 logical becomes NA rather than an error: the
# value arrives from a builder argument a user typed and from a chunk option
# YAML parsed, and "I have no opinion" is the only safe reading of a value
# this code does not understand. An error here would stop a render over a
# typo in a layout hint.
#
# NEVER isTRUE() A TRI-STATE. isTRUE(NA) is FALSE, so isTRUE() silently turns
# "decide by measurement" into "stay portrait", with nothing raised and
# nothing to notice.
#
# THREE call sites push back against this constraint deliberately, each
# commented in place with why NA and FALSE genuinely behave the same there:
# apa_layout_decide()'s `isTRUE(landscape)` (used at two positions in that
# function, both after landscape has been normalised by apa_landscape_value()
# on the line above) and apa_quarto_latex_block()'s `isTRUE(requested)`. Anywhere
# else, isTRUE() on a tri-state IS the defect this constraint names.
apa_landscape_value <- function(x) {
  if (is.null(x) || length(x) != 1L || !is.logical(x)) return(NA)
  as.logical(x)
}

# The precedence, in ONE place because it has three callers (Quarto PDF,
# Quarto Word, apa.save()), so the rule is not written, reviewed and tested
# three times with copies that can drift.
apa_landscape_request <- function(chunk = NA, argument = NA) {
  chunk <- apa_landscape_value(chunk)
  if (!is.na(chunk)) return(chunk)
  apa_landscape_value(argument)
}

# Has the user expressed a view in this chunk? Like apa-longtable and
# apa-twocolumn this is a CHUNK option, so it is read from knitr rather than
# from the document YAML.
#
# Three values, and `false` FORBIDS. Do not collapse `false` and absent into
# one value, as `isTRUE(v) || identical(as.character(v), "true")` would: it
# returns FALSE for both, leaving "rotate" as the only thing a user could
# express, with no way to keep a wide table upright.
apa_chunk_landscape <- function() {
  if (!isTRUE(getOption("knitr.in.progress"))) return(NA)
  v <- tryCatch(knitr::opts_current$get("apa-landscape"),
                error = function(e) NULL)
  if (is.null(v) || length(v) != 1L) return(NA)
  if (isTRUE(v))  return(TRUE)
  if (isFALSE(v)) return(FALSE)
  s <- tolower(trimws(as.character(v)))
  if (identical(s, "true"))  return(TRUE)
  if (identical(s, "false")) return(FALSE)
  NA
}

# Where the rotation DECISION crosses over into apaquarto.
#
# apafloatlatex.lua rotates a float that carries apa-landscape="true", and
# apalandscapelatex.lua already names apaTables as a source of that attribute
# ("set by a table builder that has decided the table is too wide to stand
# upright"). What was missing is the channel, and it is narrower than it
# looks: Quarto builds the float's attributes from the CELL DIV its knitr
# engine emits, and that div's attributes come from the chunk's options -- so
# the attribute has to be an option on this chunk. R cannot simply set one.
# knitr locks opts_current for the duration of a chunk, and even unlocked it
# would not help: knitr's chunk hook is handed `params`, a separate copy taken
# before the chunk ran, and that copy is what Quarto's hook reads.
#
# The one seam that IS open is the chunk hook itself. knitr looks it up
# (block_exec: `knit_hooks$get("chunk")`) AFTER the chunk's code has been
# evaluated, so a hook installed while the table is being built is the hook
# that runs, and it receives exactly the options object Quarto forwards from.
# Adding "apa-landscape" there puts it in Quarto's `unknown_opts` set, which
# is forwarded verbatim onto the cell div (quarto/share/rmd/hooks.R) -- the
# same route "apa-note" and "apa-longtable" already travel. The wrapper hands
# everything else to the hook it replaced, so Quarto's own behaviour is
# untouched.
#
# Labels, not a flag: a chunk that errors after asking would otherwise leave
# the request armed for whatever chunk ran next, and rotate a table nobody
# asked to rotate. The wrapper marks only the chunks that asked.
apa_landscape_env <- new.env(parent = emptyenv())
apa_landscape_env$labels <- character(0)
apa_landscape_env$hook <- NULL

# The label of the chunk being knitted, or NA outside a knit / for an
# unlabelled chunk. ONE reader, used by both the request and the test of
# whether the request can succeed, so the two cannot drift apart -- and they
# drifting apart is precisely the defect this prevents: the ladder decides to
# rotate, the request quietly returns FALSE, and a column goes off the page.
apa_chunk_label <- function() {
  if (!isTRUE(getOption("knitr.in.progress"))) return(NA_character_)
  if (!requireNamespace("knitr", quietly = TRUE)) return(NA_character_)
  label <- tryCatch(knitr::opts_current$get("label"), error = function(e) NULL)
  if (is.null(label) || !length(label) || !nzchar(label[1])) {
    return(NA_character_)
  }
  label[1]
}

# Can a decision to rotate actually be CARRIED OUT for this chunk?
#
# Two conditions, and both are Quarto's, not ours. There has to be a chunk to
# mark at all -- outside a knit there is none. And its label has to begin
# `tbl-`, because that prefix is what makes Quarto build a table FLOAT out of
# the cell; without a float apafloatlatex.lua's landscapewrap has nothing to
# wrap, so the `apa-landscape` attribute is set on a div that is never turned
# into a rotated page. Measured on `apa.cor.table(mtcars)` in an unlabelled
# chunk: the ladder chose rotate at 10 pt for an 8.22 in width, the render
# exited 0 with no warning, every page came out upright, and the tenth
# correlation column was simply not in the PDF.
apa_landscape_executable <- function() {
  label <- apa_chunk_label()
  !is.na(label) && grepl("^tbl-", label)
}

apa_request_landscape <- function() {
  label <- apa_chunk_label()
  if (is.na(label)) return(invisible(FALSE))

  # Re-install whenever the current hook is not ours. knitr restores its hooks
  # between renders, so a session that renders twice would silently lose the
  # wrapper if this only ever ran once.
  current <- tryCatch(knitr::knit_hooks$get("chunk"), error = function(e) NULL)
  ours <- !is.null(apa_landscape_env$hook) &&
    identical(current, apa_landscape_env$hook)

  # The hook not being ours means knitr has restored its own, which it does
  # BETWEEN renders -- so the labels standing here belong to a document that
  # has finished. Clearing them is what stops a label registered while
  # rendering document A stamping apa-landscape on a same-named chunk in
  # document B later in the same R session, rotating a page nobody asked to
  # rotate.
  if (!ours) apa_landscape_env$labels <- character(0)
  apa_landscape_env$labels <- union(apa_landscape_env$labels, label)
  if (ours) return(invisible(TRUE))

  previous <- current
  wrapper <- function(x, options) {
    if (!is.null(options$label) &&
        options$label %in% apa_landscape_env$labels) {
      options[["apa-landscape"]] <- "true"
    }
    if (is.function(previous)) previous(x, options) else x
  }
  apa_landscape_env$hook <- wrapper
  knitr::knit_hooks$set(chunk = wrapper)
  invisible(TRUE)
}

# The readability floor, shared by every destination. Every destination
# genuinely applies the size it was given, so this warning is TRUE everywhere
# it fires. The RULE is shared; the delivery is not -- each caller decides
# for itself whether an explicit size was even given, and what to return.
# ONE flowing sentence, no newline: an existing test pins the text.
apa_quarto_warn_below_floor <- function(size) {
  if (size >= APA_QUARTO_READABLE_SIZE) return(invisible(NULL))
  warning(paste0(
    "This table is set at ", size, " points, below the ",
    APA_QUARTO_READABLE_SIZE, " points at which an APA table stops being ",
    "readable; the table is complete and nothing has been cut off, and ",
    "apa.save() will write a full-size Word version of it."),
    call. = FALSE)
}

# --- The destination policy and the Word rotation decision ---------------
#
# The size the ladder or the caller chose. Honour any explicit value, warn
# below 6 pt, never refuse and never truncate.
#
# The floor is judged on the size DELIVERED, not the size requested. Word
# cannot deliver below APA_QUARTO_READABLE_SIZE (it rounds the request down
# onto the cloned styles), so checking the raw request there would produce
# two contradictory warnings in the same rendered draft: a font.size = 5 on
# docx would warn "this table is below the readable floor" (false -- Word
# delivers 6) ALONGSIDE apa_quarto_warn_word_size()'s true "Word sets this at
# 6, not the 5 requested". is_docx = TRUE therefore routes the check through
# apa_quarto_delivered_size(), which can never return below the floor, so
# the floor warning correctly never fires there and the Word adjustment
# warning (fired separately by the caller) carries the whole message. PDF,
# HTML and slides deliver the requested size exactly (is_docx = FALSE,
# the default, and slides never call this helper at all -- see
# apa_slide_font_pt()'s own comment), so for them the check is unchanged:
# requested and delivered are the same value.
apa_quarto_font_size <- function(plan, font.size = NULL, decision = NULL,
                                  is_docx = FALSE) {
  if (!is.null(font.size)) {
    # as.numeric(), NOT as.integer(). This helper is
    # shared by BOTH destinations, and only one of them needs a whole
    # number: the markdown/docx path selects a table STYLE by name
    # ("ApaTable8"), a genuinely discrete choice, but that truncation
    # already happens downstream in apa_quarto_size_rung() (its own
    # floor()), not here -- so doing it here too only cost the OTHER
    # caller precision it asked for. apa_latex_table() formats to four
    # decimal places and a caller passing font.size = 9.5 has every right
    # to see 9.5 pt in the PDF, not 9.
    size <- as.numeric(font.size)[1]
    apa_quarto_warn_below_floor(
      if (is_docx) apa_quarto_delivered_size(size) else size)
    return(size)
  }
  if (is.null(decision)) decision <- apa_layout_decide(plan)
  decision$size
}

# The shrink ladder is a set of TABLE STYLES -- the reference document's own
# unmodified "Table" style (10 pt) plus the clones Table6-Table9, Table11 and
# Table12 -- selected by a Lua filter, NOT the per-cell
# "[...]{custom-style=\"ApaTableN\"}" spans apa_markdown_table()'s font_size
# argument still knows how to write. Per-cell spans were measured in real Word
# to crush a real table's column ratio from 10.01:1 to 2.12:1 and break a long
# variable name mid-word, on precisely the tables where columns are already
# tightest. A table style costs zero width: setting a whole table's style
# does not touch how apa_md_widths() sized its columns.
#
# The marker below is a class on an EMPTY sibling div, immediately after the
# table's own grid syntax, in the same block apatablesize.lua later reads
# (mirroring how apa_meta_div() already rides alongside the LaTeX block for
# the title/note filter). An empty div with an unrecognised class is
# inert everywhere nothing reads it -- HTML, a bare pandoc render, and a
# quarto render of any OTHER format all pass it through with no visible
# effect -- so this can be emitted unconditionally without a format check
# here; apatablesize.lua itself checks FORMAT and only acts for docx.
APA_QUARTO_SIZE_CLASS_PREFIX <- "apa-table-size-"

# The size this function is handed can be ANYTHING: a continuous value from
# apa_size_for_width() (measured 4.860447 on a 22-variable apa.cor.table(),
# which the filter's own "^apa%-table%-size%-(%d+)$" pattern cannot match at
# all -- the marker div would be emitted, the filter's Div handler would never
# fire, and the table would silently stay at the DEFAULT 10 pt with no
# warning anywhere), or an explicit font.size the caller chose freely (never
# refused, but naming a style that was never cloned is worse than refusing --
# pandoc still writes custom-style="TableN", and Word's response to an
# UNKNOWN table style is not "fall back to 10 pt": it drops the table style
# assignment entirely, so the table loses the APA rules and cell margins
# along with the requested size, not merely the size). This function is the
# ONE place a size becomes a marker, so it is the one place that must snap to
# a style that is real.
#
# NA means "write NO marker", which delivers the reference document's
# unmodified "Table" style -- already 10 pt. Hence the two edges:
#
#   * exactly 10 returns NA, because there is no Table10 clone and naming a
#     style Word does not know makes it DROP the style assignment entirely,
#     costing the APA rules and cell margins as well as the size.
#   * above 12 returns 12, NOT NA. NA there would write no marker, deliver
#     10 pt, and leave the warning claiming 12.
#
# FLOOR, never round: rounding a ladder size up can put a column back over
# the edge the ladder shrank it to clear.
apa_quarto_size_rung <- function(size) {
  if (size >= APA_QUARTO_MAX_SIZE) return(as.integer(APA_QUARTO_MAX_SIZE))
  rung <- floor(size)
  if (rung == APA_QUARTO_DEFAULT_SIZE) return(NA_integer_)
  as.integer(max(APA_QUARTO_READABLE_SIZE, rung))
}

# What Word will actually put on the page for a requested size: the rung when
# there is one, and the default 10 pt when there is not. The warning below
# compares this with what the caller asked for -- never the rung, which is NA
# in exactly the case where the delivered size is most easily got wrong.
apa_quarto_delivered_size <- function(size) {
  rung <- apa_quarto_size_rung(size)
  if (is.na(rung)) APA_QUARTO_DEFAULT_SIZE else rung
}

# Word adjusts, and says so. One message covers both ends and the
# fractional case, because from the user's side they are one fact: "you asked
# for X, this document has Y". ONE flowing sentence, no newline -- knitr
# truncates a multi-line warning off the page edge in a knitted PDF, where
# pdftotext cannot catch it.
apa_quarto_warn_word_size <- function(requested, delivered) {
  if (isTRUE(all.equal(as.numeric(requested), as.numeric(delivered)))) {
    return(invisible(NULL))
  }
  warning(paste0(
    "This Word document sets the table at ", delivered, " points rather than ",
    "the ", requested, " points requested, because Word takes a table's type ",
    "size from a style in the document template and the sizes available run ",
    "from ", APA_QUARTO_READABLE_SIZE, " to ", APA_QUARTO_MAX_SIZE,
    " points in whole points; the table is complete and nothing has been cut ",
    "off, and apa.save(filename = \"mytable.docx\", your_table) writes the ",
    "table on its own if you need a size outside that range."),
    call. = FALSE)
}

apa_quarto_size_wrap <- function(body, rung) {
  paste0(body, "\n\n::: {.", APA_QUARTO_SIZE_CLASS_PREFIX, rung, "}\n:::\n")
}

# HTML's marker, and deliberately NOT the class form above. The class pattern
# apatablesize.lua matches is integers-only (`^apa%-table%-size%-(%d+)$`),
# while HTML must carry the size EXACTLY, fractions included --
# CSS can honour 8.5 pt and Word cannot. An attribute expresses what a class
# cannot, and keeps the docx marker's input untouched.
#
# format(..., trim = TRUE) rather than paste0(): as.character(8.5) is "8.5"
# but as.character(1e5) is "1e+05", which is not a length CSS understands.
APA_QUARTO_SIZE_PT_ATTR <- "apa-table-size-pt"

# The marker is emitted on EVERY HTML render, not only a sized one.
#
# WHY UNCONDITIONAL: apatablesize.lua turns this marker into a wrapper div, and
# apa.css's table-width rules are scoped to that div's class. A table that
# carries no marker gets no wrapper and no width rules -- which, if the marker
# were emitted only for an explicit font.size, would be almost every
# apaTables table. The scoping is not decoration: rule 1 overrides a table's
# column proportions, and it must not reach a kable() table whose widths the
# user set deliberately.
#
# ONE div, carrying the class always and the size attribute only when a size was
# asked for. NOT two sibling markers: apatablesize.lua pairs a marker with the
# table immediately BEFORE it in the same block list and clears the pairing once
# consumed, so a second marker would find no table and be dropped in silence.
APA_QUARTO_HTML_CLASS <- "apa-table-html"

apa_quarto_html_wrap <- function(body, pt = NULL) {
  attrs <- paste0(".", APA_QUARTO_HTML_CLASS)
  if (!is.null(pt)) {
    attrs <- paste0(attrs, " ", APA_QUARTO_SIZE_PT_ATTR, "=\"",
                    format(pt, trim = TRUE, scientific = FALSE), "\"")
  }
  paste0(body, "\n\n::: {", attrs, "}\n:::\n")
}

# apa_layout_decide()'s own warning is written for the LaTeX ladder: an
# absolute point size and apa.save() offered as an alternative destination.
# Neither is true once it is reached from apa_quarto_markdown_block(): the
# table is never at the raw continuous size (it is floored to
# APA_QUARTO_READABLE_SIZE, the smallest real style), and apa.save() writing
# "a full-size Word version" is a strange thing to advise while THIS document
# already IS a Word document. (The message below never mentions orientation,
# so it stays true whether or not the page was rotated.)
# apa_layout_decide()'s own warning is muffled at the call site and this is
# what fires instead -- only for the AUTOMATIC ladder (no explicit
# font.size; that path has its own, already-correct warning inside
# apa_quarto_font_size() above) when the natural decision would have gone
# below the readable floor. A docx table's width is a PERCENTAGE of the text
# area, so unlike LaTeX it cannot run off the page at any size -- there
# is no "does not fit even at the smallest size" case to report here.
apa_quarto_warn_floored <- function() {
  warning(paste0(
    "This table has been set at ", APA_QUARTO_READABLE_SIZE, " pt, the ",
    "smallest of the table styles its automatic sizing can choose from, ",
    "because a larger size would not have kept its columns at a reasonable ",
    "width; the table is complete and every column is present, and its ",
    "width is a share of the page rather than a fixed size, so it cannot ",
    "run off the page -- for full control over type size instead, save the ",
    "table directly: apa.save(filename = \"mytable.docx\", your_table)."),
    call. = FALSE)
}

# Quarto's OWN built-in `.landscape` div for docx closes its rotated
# section with an EMPTY sectPr, which silently strips the running head and
# page number from itself AND from every page before it, and its rotated
# page size is hardcoded A4 rather than matching the manuscript's own page.
# So this is deliberately a DIFFERENT class from Quarto's own -- using
# ".landscape" here would hand a wide apaTables table straight to that
# measured-broken mechanism. `apatablelandscape.lua` (bundled in the
# apaquarto extension) reads this class and writes the
# section properties itself: a portrait closer and a landscape section that
# is the SAME real page with width and height swapped, both carrying the
# running head.
APA_QUARTO_LANDSCAPE_CLASS <- "apa-table-landscape"

# The div carries the real page geometry as attributes -- read out of the
# bundled reference document by apa_docx_reference_sectpr() (R/docx-
# landscape.R), never hardcoded -- so the Lua filter only has to assemble
# OOXML from numbers it was handed, not compute or guess any of them. This
# is also what keeps a future non-Letter reference document correct with no
# Lua change: whatever apaquarto.docx's own page really is, that is what
# gets threaded through.
#
# When the reference document cannot be read for its geometry, the table is
# NOT wrapped -- it renders upright rather than on a landscape section built
# from guessed numbers. apa_warn_no_docx_geometry() says so once per render.
# This should not happen outside a broken installation: apaTables ships and
# tests the file this reads.
apa_landscape_wrap <- function(body) {
  geom <- apa_docx_reference_sectpr()
  if (is.null(geom)) {
    apa_warn_no_docx_geometry()
    return(body)
  }
  attrs <- paste0(
    'pgw="', geom$width, '" pgh="', geom$height, '" ',
    'mt="', geom$margin_top, '" mb="', geom$margin_bottom, '" ',
    'ml="', geom$margin_left, '" mr="', geom$margin_right, '" ',
    'mh="', geom$margin_header, '" mf="', geom$margin_footer, '" ',
    'mg="', geom$margin_gutter, '"')
  paste0("::: {.", APA_QUARTO_LANDSCAPE_CLASS, " ", attrs, "}\n\n",
        body, "\n\n:::\n")
}

# Is the render CURRENTLY building a docx?
#
# apa_quarto_markdown_block() serves both docx and html, but rotation
# is only EXECUTED for docx (apatablelandscape.lua stands down for every
# other format, HTML included) -- see that function's own comment on
# can_execute_rotation for the mispricing this gate exists to prevent.
#
# A thin wrapper around knitr::pandoc_to("docx") rather than an inline
# call, so it can be MOCKED the way this file's tests already mock
# apa_render_metadata() -- never knitr internals directly.
apa_quarto_docx_target <- function() {
  if (!isTRUE(getOption("knitr.in.progress"))) return(FALSE)
  if (!requireNamespace("knitr", quietly = TRUE)) return(FALSE)
  isTRUE(tryCatch(knitr::pandoc_to("docx"), error = function(e) FALSE))
}

#' Decide the layout and emit the markdown block for one table (docx/html)
#'
#' The non-PDF counterpart of `apa_quarto_latex_block()`: same ladder
#' (`apa_layout_decide()`), same "rotate before shrink" policy, but the
#' output is a pandoc grid table (`apa_markdown_table()`) rather than raw
#' LaTeX, and a font-size choice below the default becomes a STYLE MARKER
#' (one table style for the whole table) rather than per-cell markup.
#'
#' Rotation in Word is DECIDED BY MEASUREMENT by default, but the
#' `landscape =` argument -- carried on `x$landscape` -- is a binding
#' tri-state override. It is resolved against the chunk option by
#' `apa_landscape_request()`: chunk option first, then the argument, then
#' `NA` meaning "measurement decides". The two Quarto destinations agree
#' with each other, and with PDF (`apa_quarto_latex_block()`), because both
#' read the same precedence.
#'
#' Title and note are attached the same way
#' `apa_quarto_latex_block()` relies on its caller to attach them --
#' `apa_meta_div_for()`, an empty `::: \{.apa-table-meta\}` div read by the
#' bundled `apatablemeta.lua`. That filter is registered `at: post-quarto`
#' in `_extension.yml`'s COMMON filter list, so it already runs on this
#' destination too (never reimplemented or branched on precedence here; see
#' that function's own comment for why).
#' Unlike the LaTeX path, where `apa_quarto_render()` combines the two
#' outside `apa_quarto_latex_block()`, here the div is appended INSIDE this
#' function. This is the one function every non-PDF entry point and every
#' fixture in the test suite already calls directly.
#'
#' @param x an apaTables table or spec.
#' @param font.size optional explicit point size, overriding the ladder.
#' @param title optional table title, overriding the one apaTables
#'   generated, as the `title` argument shared by `apa.quarto.pdf()` /
#'   `apa.quarto.docx()` / `apa.quarto.html()` does on the LaTeX path too.
#'   A chunk's `tbl-cap` still wins over this (resolved by the filter, not
#'   here).
#' @param note optional table note, overriding the one apaTables generated,
#'   mirroring `title` above. A chunk's `apa-note` still wins over this.
#' @return a `knit_asis` object.
#' @keywords internal
apa_quarto_markdown_block <- function(x, font.size = NULL,
                                      title = NULL, note = NULL) {
  spec <- as_spec(x)
  plan <- apa_display_plan(spec)
  # A pptx render falls through to THIS function (apa_quarto_render()
  # special-cases revealjs and LaTeX only; neither claims pptx), so this is
  # where PowerPoint's spanner-dropping problem must be handled, even though
  # this function's own title says it serves docx/html. pandoc's pptx writer
  # drops column spans outright -- a spanner label would otherwise sit over
  # the wrong column, MEASURED on a real render: "alcohol" over the "M" of
  # "None" instead of spanning six columns. A destination apaTables does not
  # support may get an unstyled table, never a mislabelled one. Gated on the
  # pptx target alone: docx and HTML both keep their spans (HTML draws them
  # correctly), so flattening must not reach either.
  # apa_slide_flatten_spanners() (R/quarto-slide.R) is a no-op on a plan
  # with no spanner row (`has_span` guard), so this is safe to call
  # unconditionally once the target is known to be pptx.
  if (identical(apa_quarto_actual_format(), "pptx")) {
    plan <- apa_slide_flatten_spanners(plan)
  }
  # THE PRECEDENCE, resolved once: chunk option, then the builder argument,
  # then NA meaning "measurement decides". The LaTeX path reads the same
  # precedence, so PDF and Word agree.
  requested <- apa_landscape_request(apa_chunk_landscape(), spec$landscape)

  # can_execute_rotation = apa_quarto_docx_target().
  #
  # The chunk-label question the LaTeX sibling's apa_landscape_executable()
  # answers does not apply here. LaTeX rotation rides on Quarto's OWN
  # float/crossref machinery (apafloatlatex.lua's landscapewrap fires on a
  # FloatRefTarget's apa-landscape attribute), and only a `tbl-` labelled
  # chunk becomes a float at all. Docx rotation does not use that machinery:
  # apa_landscape_wrap() emits a literal `::: {.apa-table-landscape ...}`
  # div directly in the chunk's own markdown, and apatablelandscape.lua
  # finds and replaces it with a Pandoc-level walk that recurses into a
  # FloatRefTarget's content when the chunk IS a labelled float, and finds
  # the div sitting in the ordinary block list when it is not -- MEASURED
  # both ways: a `tbl-` labelled table (deferred to the document's end by
  # apafloatstoend.lua, as apaquarto does by default) and a plain unlabelled
  # table both came back correctly rotated, with a headerReference on every
  # section, from the same filter with no branch on label at all.
  #
  # A DIFFERENT executability question remains: this function serves BOTH
  # docx and html (one emitter for every non-PDF destination), but
  # apatablelandscape.lua stands down for anything that is not docx
  # (`if FORMAT ~= "docx" then return end`) -- nothing executes rotation on
  # an HTML render. can_execute_rotation must therefore ask WHICH of the two
  # destinations this actually is, not just "is docx rotation built at all":
  # unconditional TRUE would price a wide table against the WIDER rotated
  # budget on an HTML render too, emit it at full size with a dead,
  # unprocessed `.apa-table-landscape` div, and never rotate -- the same
  # shape of mispricing apa_landscape_executable() prevents on the LaTeX
  # path.
  #
  # apa_quarto_docx_target() is a thin, MOCKABLE wrapper around
  # knitr::pandoc_to("docx") -- this file's own established testing
  # convention is to mock a small wrapper rather than knitr internals
  # directly (apa_render_metadata() is mocked the same way; see
  # test-render-markdown.R).
  #
  # apa_layout_decide()'s own ladder-size warning is muffled here and
  # re-raised, if appropriate, by apa_quarto_warn_floored() below -- see that
  # function's comment for why its own wording is wrong on this path. The
  # handler is keyed on the "apaTables_ladder_size" CLASS, not a blanket
  # `warning = fn`: apa_layout_decide() also calls
  # apa_warn_unknown_geometry() and apa_warn_journal_mode() before it ever
  # reaches the ladder, and both of those route through apa_warn_once()
  # (R/apaquarto-version.R), which assign()s its once-per-render flag BEFORE
  # calling warning() -- so a handler that muffled every warning would not
  # just suppress the ladder's message, it would PERMANENTLY BURN the
  # journal-mode and unknown-page-geometry keys for the rest of the render,
  # silencing them even on a later, completely unguarded call (measured).
  # Naming the handler by class means only a warning actually carrying that
  # class is caught; every other warning apa_layout_decide() can raise passes
  # through untouched.
  is_docx <- apa_quarto_docx_target()
  decision <- withCallingHandlers(
    # col_overhead_in for Word only: Word's fixed
    # <w:tblCellMar> is real occupied width the content model cannot see.
    # HTML has no page (nothing here is fitted) and pptx has no Word styles,
    # so both keep the model unchanged.
    apa_layout_decide(plan, landscape = requested,
                      can_execute_rotation = is_docx,
                      col_overhead_in =
                        if (is_docx) APA_WORD_CELL_PADDING_IN else 0),
    apaTables_ladder_size = function(w) invokeRestart("muffleWarning")
  )
  size <- apa_quarto_font_size(plan, font.size = font.size, decision = decision,
                                is_docx = is_docx)
  rung <- apa_quarto_size_rung(size)
  # An explicit size Word cannot deliver is REPORTED, not silently
  # adjusted. Gated on is_docx because it is Word's style mechanism that
  # imposes the range -- PDF, HTML and slides honour any size exactly -- and
  # on !is.null(font.size) because the automatic ladder's own out-of-range
  # case is apa_quarto_warn_floored()'s, just below, with its own wording.
  if (is_docx && !is.null(font.size)) {
    apa_quarto_warn_word_size(size, apa_quarto_delivered_size(size))
  }
  # The floor warning and the `apa-table-size-N` marker div are both
  # consequences of the docx-only shrink ladder: the AUTOMATIC ladder never
  # runs on HTML at all (HTML has no page, so nothing rotates and nothing
  # shrinks automatically there), and no HTML CSS reads the
  # `apa-table-size-*` CLASS markers apatablesize.lua reads for docx (HTML's
  # size travels on the separate `apa-table-size-pt` ATTRIBUTE marker below,
  # set only for an explicit font.size and never by this automatic path;
  # apatablesize.lua itself has no early return for other formats -- see
  # that file's own header -- it consumes every marker it finds and only
  # branches on which one to apply). Gating both on `is_docx` -- the same
  # value that decides above whether rotation can execute -- keeps an HTML
  # render from pricing, warning about, and marking a shrink it never
  # performs; without the gate the reader's manuscript would carry a warning
  # asserting a floor that HTML output never applies.
  if (is_docx && is.null(font.size) && decision$size < APA_QUARTO_READABLE_SIZE) {
    apa_quarto_warn_floored()
  }

  # font_size = NULL, always -- see APA_QUARTO_SIZE_CLASS_PREFIX's comment
  # above for why per-cell markup must never reach apa_markdown_table() on
  # this path, whatever size was chosen.
  body <- apa_markdown_table(plan, font_size = NULL, rules = TRUE)
  if (is_docx) {
    if (!is.na(rung)) body <- apa_quarto_size_wrap(body, rung)
  } else if (!isFALSE(apa_quarto_html_target())) {
    # Emitted on every HTML render, not only a sized one: the marker is also
    # what apa.css's table-width rules are scoped to, and a table without it
    # gets no width rules at all. The size is expressed by what is PASSED
    # (NULL or a size), not by whether the call happens.
    #
    # R never wraps the table in a sized div. A div written HERE, around the
    # table, is wrong twice and silently -- Bootstrap's .small rule takes
    # 0.875 of any inherited size (asked 6 pt, measured 5.25 pt), and a
    # wrapper defeats apaquarto's float collection, MOVING the table in the
    # user's manuscript. Both measured.
    #
    # The size attribute is set only for an EXPLICIT size: nothing shrinks
    # automatically on HTML, because a web page has no page to fit.
    #
    # The size DOES end up delivered by an ancestor div on HTML, but one
    # apatablesize.lua builds at post-quarto, after float collection, which
    # is why it avoids both failures above. Do not "simplify" that filter's
    # div away, and do not move the wrapping into R.
    #
    # Gated on apa_quarto_html_target() being anything but FALSE. HTML
    # consumes this marker (apatablesize.lua, registered for html and docx);
    # a typst render consumes nothing, so the marker would go unread there.
    # Pandoc's typst writer happens to drop an empty attributed Div silently
    # (measured), but do not depend on that downstream accident: do not emit
    # what nothing consumes.
    # !isFALSE(), not isTRUE(): outside a knit the helper answers NA and the
    # marker must still be emitted, which is what every direct unit call in
    # the suite reads.
    body <- apa_quarto_html_wrap(body, if (is.null(font.size)) NULL else size)
  }
  if (isTRUE(decision$rotate)) body <- apa_landscape_wrap(body)

  # apa_meta_div_for(x, ...) is appended AFTER the table -- rung wrap and
  # landscape wrap included -- and separated by a blank line, exactly as
  # apa_quarto_render() already does for the LaTeX block: pandoc swallows the
  # `::: {.apa-table-meta}` fence into whatever came immediately before it
  # without one (measured; the same trap either path can walk into). The div
  # is a sibling of the (possibly wrapped) table, not
  # content inside the landscape div -- apatablelandscape.lua only acts on
  # its own class and leaves any other sibling block alone, and
  # apatablemeta.lua's own walk of float.content finds a Div wherever it
  # sits in that list, not only at a fixed position.
  meta_div <- apa_meta_div_for(x, title = title, note = note)
  knitr::asis_output(paste0(body, "\n\n", meta_div))
}

# The LaTeX half of apa_quarto_render(), split out so it can be exercised
# without a knitr session: decide, ask for the page, emit the grid.
#
# knitr::asis_output() is NOT a raw-LaTeX guarantee. Pandoc treats a
# COMPLETE \begin{env}...\end{env} as raw TeX, but it re-parses neighbouring
# lines as markdown -- measured, it turned a trailing "%" into "\%". What
# apa_latex_table() returns is exactly one self-contained environment with
# nothing before or after it, which is the condition under which asis_output()
# is safe; anything else emitted here would need an explicit ```{=latex}
# fence.
#
# NO \normalem FIX IS NEEDED ON THIS PATH, and that is measured, not
# assumed. flextable's LaTeX dependencies load ulem WITHOUT [normalem], which
# redefines \emph as UNDERLINING document-wide, so apaquarto's italic "Note."
# and any *italics* in an apa-note come out underlined. This path never loads
# flextable, so there is nothing to correct -- confirmed by rendering the
# same document three ways and rasterizing the note at 600 dpi:
#
#   native path, as is        "ulem" appears 0 times in doc.tex; note ITALIC
#   native path + \normalem   ulem + \normalem added; note ITALIC (unchanged)
#   ulem loaded, no normalem  note UPRIGHT and UNDERLINED, 3 rows of the crop
#                             more than 85% dark -- the defect, and proof the
#                             check can see it
#
# No LaTeX destination calls flextable at all, which is what keeps this true;
# a flextable call reaching this path would bring the underlining back.
#
# font.size: optional explicit point size, overriding the ladder.
# Honoured on the LaTeX path exactly as on the markdown path in what it
# PROMISES -- a value is used whatever it is, a value below the readable
# floor warns, nothing is ever refused -- but NOT in how "nothing is cut
# off" is kept true: the markdown/docx path gets that for free from
# percentage-width tables; this path has to actually re-check the fit at
# the size it is about to emit, which is what passing `size = font.size`
# into apa_layout_decide() below does (see that parameter's documentation:
# a 20 pt apa.cor.table(attitude) measures 10.26 in against a 6.5 in page
# and would otherwise render with no warning and no rotation).
apa_quarto_latex_block <- function(x, font.size = NULL) {
  spec <- as_spec(x)
  plan <- apa_display_plan(spec)

  # ASK FIRST. apa_request_landscape() can only mark a chunk Quarto turns into
  # a float, so whether it can succeed is an INPUT to the decision. Discard
  # it and the ladder rotates, the request is refused, and the table is set
  # for a page it never gets; as an input, a table that cannot be rotated is
  # shrunk to the upright page instead and keeps every column.
  #
  # THE PRECEDENCE, resolved once: chunk option, then the builder argument,
  # then NA meaning "measurement decides". The markdown path reads the same
  # precedence, so PDF and Word agree.
  requested <- apa_landscape_request(apa_chunk_landscape(), spec$landscape)
  executable <- apa_landscape_executable()

  # Coerced ONCE, here, rather than inline in the apa_layout_decide() call
  # below -- apa_quarto_font_size() (called again at the end of this
  # function) also coerces font.size on its own, so a caller passing a
  # character "20" or a length-2 vector gets the SAME single value priced
  # for rotation as is later checked against the readable floor and handed
  # to the emitter, instead of two independent coercions that could disagree.
  size_arg <- if (is.null(font.size)) NULL else as.numeric(font.size)[1]

  # size = size_arg (NULL when automatic): apa_layout_decide() prices
  # ROTATION against the size that will actually be emitted, not the
  # ladder's own 10 pt guess, and raises its own warning ONLY for the
  # residual "no size can fix this" case (that warning's own text is true
  # regardless of what size was requested, so it is never muffled here --
  # muffling it would silence the one warning that catches this class of
  # defect). apa_warn_unknown_geometry() / apa_warn_journal_mode(), also
  # called inside apa_layout_decide(), are UNAFFECTED either way: nothing
  # here wraps the call in a warning handler of any kind.
  decision <- apa_layout_decide(plan, landscape = requested,
                                can_execute_rotation = executable,
                                size = size_arg)
  if (isTRUE(decision$rotate)) apa_request_landscape()

  # A rotation the user asked for BY NAME -- whether by chunk option or by
  # the `landscape =` argument, apa_landscape_request() already resolved
  # which -- and cannot have is the one case worth saying out loud, because
  # there is a real fix and it is one word: the chunk needs a label beginning
  # `tbl-`. The automatic case (requested is NA) says nothing -- it silently
  # shrinks, and the table is complete either way. It fires for a request by
  # ARGUMENT as well as by chunk option; otherwise a user who asked by
  # argument and could not have it would be told nothing at all.
  #
  # isTRUE(requested) is correct here, even though `requested` is the
  # tri-state apa_landscape_request() returns (NA/TRUE/FALSE) -- this is a
  # SECOND deliberate exception alongside the one at apa_layout_decide()'s
  # `isTRUE(landscape)` line: NA means nobody asked (nothing to report) and
  # FALSE means the user explicitly declined and got exactly that (also
  # nothing to report), so only TRUE -- an actual request that could not be
  # honoured -- is worth a warning, and isTRUE() gives precisely that.
  if (isTRUE(requested) && !executable) {
    warning(paste0(
      "This table asked for a rotated (landscape) page, but only a chunk ",
      "whose label begins tbl- becomes a table Quarto can rotate, so the ",
      "table has been fitted to the upright page instead with every column ",
      "kept; add a settings line such as #| label: tbl-mytable to the chunk ",
      "to get the rotated page."),
      call. = FALSE)
  }
  size <- apa_quarto_font_size(plan, font.size = font.size, decision = decision)
  knitr::asis_output(apa_latex_table(plan, size))
}

# The metadata div that carries the generated title/note past Quarto's
# starring decision (measured): a `tbl-`labelled chunk becomes a float whose
# caption is empty and whose apa-note attribute is absent when the user
# wrote no tbl-cap/apa-note, and a filter can still fill both in AT THAT
# STAGE, before Quarto decides whether the float is starred. So R's job here
# is only to hand the filter the values it would otherwise have to
# recompute; the filter, not this code, decides precedence by populating
# only whatever the float still lacks.

# Escape a value for a pandoc div attribute: NO BACKSLASH-ESCAPED QUOTE EVER.
# Both quote characters are written as numeric character references instead,
# and pandoc decodes them back when it reads the attribute (MEASURED: the value
# `a&quot;b and &#39;q&#39; and &#34;d&#34;` is handed to a filter as
# `a"b and 'q' and "d"`).
#
# THE REASON IS A SILENT, USER-REACHABLE CORRUPTION, and it is worth stating
# exactly, because two earlier versions of this function each looked correct
# and each lost data on a rendered page.
#
# A backslash-escaped quote in a div attribute is correct pandoc -- `pandoc -f
# markdown -t json` returns the value byte for byte, and a markdown round trip
# is lossless -- but it does not survive Quarto's own pipeline.  The value
# arrives at the FIRST Lua filter (a user filter registered `at: pre-quarto`,
# before anything of ours runs) with every ` = ` in it collapsed to `=`.  On
# the page that is "N=number of cases" instead of "N = number of cases", with
# exit 0 and no warning anywhere.
#
# It is escape PARITY, not a particular quote character.  Writing the values in
# double quotes and escaping `\"` was measured corrupting; switching to single
# quotes and escaping `\'` moved the defect rather than removing it.  MEASURED
# on a matrix of apostrophe counts, twelve cases in one render, reading the
# value back at the filter: the title is corrupted exactly when the title holds
# an ODD number of escaped quotes AND the note does too (1+1 and 3+1 corrupt;
# 0+1, 2+1, 1+2, 2+2, 3+0, 3+2 do not).  So
# `apa.quarto.pdf(title = "Cronbach's Alpha = ...", note = "Cohen's d ...")`
# -- one apostrophe in each, which is ordinary in a real table -- silently
# printed "Alpha=..." on the page.  Two apostrophes in the title alone did NOT
# corrupt, which is why an earlier comment here claiming the single-quote form
# had been "measured intact ... plus an apostrophe" was overstated: that
# measurement used ONE apostrophe, in ONE attribute, and could not have seen
# this.
#
# Entities remove the trigger by construction rather than by dodging it: with
# no backslash-escaped quote emitted at all there is no parity to get wrong,
# for either quote character, in either attribute.  The remaining backslash
# pass is unrelated and still required -- a generated note contains the
# markdown escape `\*`, and `\` is pandoc's escape character inside a quoted
# attribute value, so it has to be doubled to arrive as one.  A doubled
# backslash was measured NOT to participate in the parity trigger.  The order
# of the three passes below no longer matters (an entity contains no backslash
# and no quote), which is itself the point: the old ordering hazard is gone.
apa_attr_escape <- function(s) {
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub('"', "&#34;", s, fixed = TRUE)
  gsub("'", "&#39;", s, fixed = TRUE)
}

# The spec's note entries as ONE apa-note attribute value, in the form
# apaquarto splits back apart.
#
# A note has parts -- a correlation table's significance-star sentence is its
# own entry -- and APA style starts each part on its own line.  apaquarto does
# that in utilsapa.lua's make_note(), which strips a leading `["` and a
# trailing `"]` and then splits what is left on the literal three characters
# `","`, emitting one FigureNote div per piece.  That is the shape a YAML list
# serialises to, because the only way to reach make_note() used to be a
# hand-written `#| apa-note: ["...","..."]` chunk option -- which is exactly
# what apa.quarto.template() printed before this function existed.
#
# So the parts are joined with `","` and wrapped, not run together with a
# space: a space-joined value contains no separator at all and collapses the
# whole note into a single paragraph.  MEASURED against the real make_note()
# under `pandoc lua`: the space-joined value gives 1 div, this one gives 2.
#
# THE SEPARATOR IS STRUCTURAL AND CANNOT BE PROTECTED FROM THIS LAYER.  It is
# only a quote, a comma and a quote in a row, so a note part containing `","`
# would forge one and be split into two paragraphs (and a part merely ENDING in
# `",` would forge one against the join that follows it).  There is no encoding
# of the quote that prevents this: whatever this function emits, the value
# reaching make_note() has already been through pandoc's attribute reader,
# which decodes character references -- so `&quot;` and `&#34;` arrive as a
# plain `"` and split exactly as a literal one would.  MEASURED both ways: with
# `pandoc -t json`, `a&quot;b` comes back as `a"b`; and end to end, a note part
# carrying `","` renders as three paragraphs.
#
# An earlier version of this function substituted `&quot;` here and its comment
# claimed that made the separator safe.  It did not -- the substitution was a
# no-op end to end -- so it has been removed rather than left standing with a
# disclaimer, because a guard that does not guard is worse than none.
#
# It is UNREACHABLE from the generated path: no builder puts a double quote in
# a note (checked across apa.cor.table, apa.reg.table, apa.d.table and
# apa.aov.table -- none contains one), so every `","` in the value this
# function returns is one it put there.  That is a fact about the callers, and
# it is stated here as such rather than dressed up as an invariant this code
# enforces.
#
# A note supplied by the caller as `note =` is deliberately NOT put through
# this: it is a single value, so it needs no separator, and passing it through
# would strip a `["`...`"]` a caller had written on purpose.  A caller note
# containing `","` therefore splits into extra paragraphs too -- the same
# behaviour a hand-written `#| apa-note` chunk option has always had, since
# both end up in the same attribute.  Documented on the `note` argument.
apa_note_attr <- function(parts) {
  if (!length(parts)) return("")
  paste0("[\"", paste(parts, collapse = "\",\""), "\"]")
}

# An empty div, `::: {.apa-table-meta apa-title='...' apa-note='...'}\n:::\n`.
# Empty rather than carrying the title/note as CONTENT because content would
# have to be valid Pandoc AST wherever it landed (raw LaTeX titles included),
# while an attribute is just a string the filter reads back verbatim.
apa_meta_div <- function(title_md, note_md) {
  paste0("::: {.apa-table-meta",
         " apa-title='", apa_attr_escape(title_md), "'",
         " apa-note='",  apa_attr_escape(note_md),  "'}\n:::\n")
}

# A title or note argument, resolved to markdown -- or NULL if none was
# given, so the caller knows to fall back to the spec.
apa_md_value <- function(v) {
  if (is.null(v)) return(NULL)
  if (inherits(v, "apa_richtext")) return(rt_to_markdown(v))
  as.character(v)[1]
}

# Resolve argument-or-spec and build the div. The chunk option (tbl-cap /
# apa-note) is NOT consulted here: the filter resolves that by populating
# only what is empty, so this function never needs to know it exists.
apa_meta_div_for <- function(x, title = NULL, note = NULL) {
  sp <- as_spec(x)
  t_md <- apa_md_value(title)
  if (is.null(t_md)) {
    t_md <- if (inherits(sp$title, "apa_richtext")) rt_to_markdown(sp$title)
            else as.character(sp$title)
  }
  n_md <- apa_md_value(note)
  if (is.null(n_md)) {
    n_md <- apa_note_attr(vapply(sp$notes, rt_to_markdown, character(1)))
  }
  apa_meta_div(t_md, n_md)
}

# --- The four format-named entry points ------------------------------------
#
# WHY FOUR NAMES INSTEAD OF ONE FUNCTION WITH A DESTINATION ARGUMENT. A
# feature should be easy to see exists, not merely "discoverable". A
# function's existence in the package index advertises the capability; an
# argument value inside one function's help page does not. apa.save() already
# names a destination with a function rather than an argument, so this
# extends the package's own existing pattern.
#
# THE NAMES ARE FORGIVING, AND THAT IS A DELIBERATE TRADE. The render
# TARGET, not the function name, decides the mechanism: apa.quarto.pdf() in a
# document whose YAML says apaquarto-docx still produces a correct Word
# table. Quarto's output format is a property of the DOCUMENT, not of a code
# chunk, so a function name can only ever state intent -- and the strict
# alternative (each function errors outside its own format) was rejected
# because it breaks the common real workflow of drafting as PDF and later
# switching the whole document to Word, which would otherwise force every
# table chunk to be edited and edited back.
#
# THE ONE THING A FORGIVING NAME COULD LOSE, RECOVERED. A user who reads
# apa.quarto.docx() as "give me Word" and gets a silently correct PDF instead
# has no way to notice the name was wrong. Because each name matches an
# apaquarto YAML format key exactly (pdf/apaquarto-pdf, docx/apaquarto-docx,
# html/apaquarto-html), the mismatch message can say the one useful thing at
# exactly that moment: which YAML line to change. See
# apa_quarto_target_message() below.

# format key (as these functions and apa.quarto.template()'s target=
# argument spell it) -> the matching apaquarto YAML `format:` value.
#
# The slide key is "revealjs" here, in apa.quarto.template()'s exported
# target= argument and everywhere else the key is used, so
# apa.quarto.revealjs() and apa.quarto.template(target = ...) name the
# destination one way. Do not reintroduce a second spelling such as "slide".
APA_QUARTO_ENTRY_YAML <- c(
  pdf      = "apaquarto-pdf",
  docx     = "apaquarto-docx",
  html     = "apaquarto-html",
  revealjs = "revealjs"
)

# format key -> the word used in prose ("To produce ___, set ...").
#
# "reveal.js slides", not "slides": the word matches the string
# typed on the same line ("set `format: revealjs`"), rather than staying
# vague about which slide format is meant.
APA_QUARTO_ENTRY_PROSE <- c(
  pdf      = "PDF",
  docx     = "Word",
  html     = "HTML",
  revealjs = "reveal.js slides"
)

# The RAW token knitr::pandoc_to() reports (fmt_name() already strips a
# "-variant"/"+extension" suffix, e.g. "docx+lots_of_stuff" -> "docx") ->
# the format key our entry points and apa.quarto.template() use. LaTeX and
# beamer both compile through pandoc's "latex" writer family and both mean
# "pdf" here; "beamer" itself is out of scope (apaquarto does not support it).
# HTML has more than one writer spelling ("html", "html4", "html5"), matched
# by prefix. "revealjs" maps to itself (APA_REVEALJS_FORMATS in
# R/quarto-slide.R). Anything else (gfm, typst, ...) returns NA: this
# function does not know what those destinations want, so it says nothing
# about them rather than guessing.
apa_quarto_actual_entry <- function(to) {
  if (is.null(to) || is.na(to) || !nzchar(to)) return(NA_character_)
  if (to %in% c("latex", "beamer")) return("pdf")
  if (identical(to, "docx")) return("docx")
  if (identical(to, "html") || grepl("^html", to)) return("html")
  if (to %in% APA_REVEALJS_FORMATS) return("revealjs")
  # "pptx" is DELIBERATELY NOT "revealjs", and it has its own explicit arm
  # rather than falling through to the NA_character_ below so a reader sees
  # this is a decision, not an omission. Quarto slide support is revealjs
  # only (APA_REVEALJS_FORMATS does not contain "pptx"), so this function
  # must not classify a pptx render as any known entry, including a "pptx"
  # entry of its own. That does NOT mean a pptx render goes unmentioned:
  # apa_quarto_render() checks for "pptx" directly and raises its own
  # not-yet-supported message via apa_quarto_target_message() below, and
  # this function's NA answer is exactly what tells apa_quarto_dispatch()'s
  # generic mismatch gate it has nothing to say. Do not map pptx back onto
  # "revealjs": that classification hides a PowerPoint render behind the
  # supported slide destination.
  if (identical(to, "pptx")) return(NA_character_)
  NA_character_
}

# TRUE on an HTML writer, FALSE on a knit whose target is anything else, and
# NA when there is no render target to report.
#
# The third state is the point. apa_quarto_actual_format() answers
# NA_character_ on three conditions, not just "no knit in progress" --
# knitr unavailable and an empty pandoc token are the other two (see that
# function's own comment, just below). None of the three must be conflated
# with FALSE: the NA case has no document to leak into, so the marker must
# still be emitted (the direct-unit-call path every test in the suite
# reads).
#
# FALSE covers every non-HTML entry apa_quarto_actual_entry() knows how to
# classify, and it is wider than "typst, gfm, pptx": "latex"/"beamer" map to
# "pdf", "docx" stays "docx", "revealjs" maps to "revealjs" -- so this helper
# withholds the exact-size marker on PDF, Word and the slide destination,
# not just on the unclassified writers (typst, gfm, pptx), all of which also
# produce FALSE here (identical(NA_character_, "html") is FALSE, not NA).
# apatablesize.lua is registered for html and docx only, so a marker emitted
# on, say, a typst render would pass through unconsumed.
apa_quarto_html_target <- function() {
  to <- apa_quarto_actual_format()
  if (is.na(to)) return(NA)
  identical(apa_quarto_actual_entry(to), "html")
}

# The document's ACTUAL render target, as the raw pandoc token (never
# normalised here) -- NA_character_ when there is nothing to report: no knit
# in progress, knitr unavailable, or the pandoc token itself is empty.
#
# A thin, MOCKABLE wrapper around knitr::pandoc_to() rather than an inline
# call -- this file's own established convention (apa_quarto_docx_target()
# is mocked the same way) for a plain testthat call with no real knit session
# behind it to report a pandoc target at all.
apa_quarto_actual_format <- function() {
  if (!isTRUE(getOption("knitr.in.progress"))) return(NA_character_)
  if (!requireNamespace("knitr", quietly = TRUE)) return(NA_character_)
  to <- tryCatch(knitr::pandoc_to(), error = function(e) NULL)
  if (is.null(to) || !nzchar(to)) return(NA_character_)
  to
}

#' The guidance message for a name/target mismatch
#'
#' Used when a chunk calls an entry point that does not match the render
#' target, e.g. \code{apa.quarto.pdf()} in a document rendering to Word. The
#' table renders correctly either way, because the render target decides the
#' mechanism and the function name does not; the message states the one
#' useful fact, which YAML line would make the two agree. A separate internal
#' function so the tests can check the wording directly.
#'
#' @param called the format key of the function that was called
#'   (\code{"pdf"}, \code{"docx"}, \code{"html"}, or \code{"revealjs"}).
#' @param actual the RAW render-target token, e.g. from
#'   \code{apa_quarto_actual_format()} (\code{"latex"}, \code{"docx"},
#'   \code{"html"}, ...) -- not yet normalised to a format key; this function
#'   does that itself via \code{apa_quarto_actual_entry()}.
#' @return A length-1 character string.
#' @keywords internal
apa_quarto_target_message <- function(called, actual) {
  # PowerPoint gets its OWN sentence, checked first and regardless of
  # `called`. apa_quarto_actual_entry("pptx") answers NA_character_ (it is
  # deliberately not classified as any entry, including "revealjs" -- see
  # that function's own comment), so the generic branch below would
  # otherwise fall through to the "unclassified writer" wording ("This
  # document renders to `pptx`. To produce ..."), which is TRUE but not the
  # useful fact here. The useful fact is that PowerPoint is not served
  # through Quarto at all yet, and where the supported slide destination is.
  # RENDER, THEN WARN: the table is never refused -- apa_quarto_render()
  # raises this warning FIRST, then builds the table. knitr captures a
  # warning() and the chunk's own output in the ORDER they occur during
  # evaluation, so the warning text lands ABOVE the table in the rendered
  # document -- hence "the table below" in the message. "not yet
  # supported", never "unsupported": PowerPoint output is planned work, not
  # abandoned.
  if (identical(actual, "pptx")) {
    return(paste0(
      "This document renders to PowerPoint, where apaTables tables are ",
      "not yet supported through Quarto, so the table below is styled ",
      "only by PowerPoint's own default table look, not APA format; set ",
      "`format: revealjs` in the YAML for the supported slide destination."))
  }
  actual_entry <- apa_quarto_actual_entry(actual)
  # No special case is needed for revealjs: the pptx arm above has already
  # returned, and apa_quarto_actual_entry() returns "revealjs" only for
  # `actual == "revealjs"` (APA_REVEALJS_FORMATS contains nothing else), so
  # APA_QUARTO_ENTRY_YAML[["revealjs"]] -- itself "revealjs" -- can never
  # disagree with `actual` here.
  yaml_actual  <- if (!is.na(actual_entry)) {
    APA_QUARTO_ENTRY_YAML[[actual_entry]]
  } else {
    as.character(actual)
  }
  yaml_called  <- APA_QUARTO_ENTRY_YAML[[called]]
  prose_called <- APA_QUARTO_ENTRY_PROSE[[called]]
  paste0("This document renders to `", yaml_actual, "`. To produce ",
        prose_called, ", set `format: ", yaml_called, "` in the YAML.")
}

# The one body shared by all four entry points (the three manuscript entry
# points behave identically; apa.quarto.revealjs() goes through the same
# apa_quarto_dispatch()). `called` is
# the only thing that differs between them, and it is used for nothing but
# the mismatch message -- the dispatch decision itself is read fresh from the
# actual render, never from which of the four names was typed.
#
# DISPATCH reuses apa_latex_target() (R/render-flextable.R) rather than
# re-deriving the LaTeX/not-LaTeX boolean from apa_quarto_actual_format():
# that helper is the package's existing, reviewed and tested source of truth
# for "is this render LaTeX", already correctly excluding apa.save()'s direct
# output and already covering knitr's out.format fallback alongside pandoc_to.
# apa_quarto_actual_format() exists for the FINER question the mismatch
# message needs (which of docx/html/other, not just "not LaTeX") and is used
# for that alone.
#
# THE LANDMINE THIS FUNCTION IS BUILT TO AVOID: apa_quarto_markdown_block()
# already composes its own .apa-table-meta div internally --
# apa_meta_div_for() must NEVER be called again around its result here, or
# TWO divs reach apatablemeta.lua, whose walk has no early exit, so the
# SECOND one silently wins (a wrong title/note, not a crash). Only the LaTeX
# branch composes the div; the markdown branch's return value is passed
# straight through untouched.

# Validate an explicit font.size ONCE, at the one chokepoint every public
# route passes through. Without this, apa.quarto.pdf(t, font.size = "abc")
# dies mid-render with base R's bare "missing value where TRUE/FALSE needed"
# -- a loud failure, which is correct, with a message naming neither the
# argument nor the fix, which is not. A numeric-looking character ("10") is
# accepted: the coercion is the same as.numeric() the downstream code
# applies, performed once and checked.
apa_quarto_font_size_value <- function(font.size) {
  if (is.null(font.size)) return(NULL)
  v <- suppressWarnings(as.numeric(font.size))
  if (length(v) != 1L || !is.finite(v) || v <= 0) {
    stop("font.size must be a single positive number of points (for ",
         "example font.size = 10); got ",
         paste(deparse(font.size), collapse = ""), ".", call. = FALSE)
  }
  v
}

# The render itself, with NO mismatch message -- factored out of
# apa_quarto_dispatch() so that knit_print.apa_table_spec() (a bare `table1`
# on its own line in a chunk, with no function name typed at all) can share
# the identical LaTeX/markdown routing and the identical meta-div-composition
# safety without a "called" name to compare the render against. There is
# nothing to be forgiving ABOUT when no name was ever stated.
apa_quarto_render <- function(x, title = NULL, note = NULL, font.size = NULL) {
  font.size <- apa_quarto_font_size_value(font.size)
  # The pptx check is made FIRST, before any routing decision, and HERE
  # rather than in apa_quarto_dispatch(), because this function has TWO
  # callers: apa_quarto_dispatch() (apa.quarto.pdf/docx/html/revealjs) and
  # apa_quarto_knit_print() (R/quarto.R), which knit_print.apa_table() and
  # knit_print.apa_table_spec() call for the ordinary case of a bare table
  # left as a chunk's last line, with no function name typed at all. A check
  # in only one caller misses the other -- measured: a bare
  # `apa.cor.table(attitude)` as a chunk's last line in a `format: pptx`
  # deck emitted a full table and no warning. `called = "revealjs"` because
  # apa_quarto_target_message()'s pptx arm ignores `called` entirely (its
  # own comment explains why) -- it is passed only to satisfy that
  # function's signature. Keyed on a constant, not on `called`/`actual`
  # (apa_quarto_dispatch()'s generic mismatch key): the message text never
  # varies with `called` for pptx, so one key is the correct dedupe, not an
  # accident of convenience.
  pptx_notice <- NULL
  if (identical(apa_quarto_actual_format(), "pptx")) {
    msg <- apa_quarto_target_message(called = "revealjs", actual = "pptx")
    # Quarto's pptx render suppresses R warnings by default (measured: a
    # plain warning() in a chunk reaches neither the deck nor the
    # render console under `format: pptx`, while the same chunk under
    # HTML shows it; an explicit `execute: warning: true` restores it). So
    # the warning() below is invisible on exactly the render it is about,
    # and the deck's own body must carry the sentence or the deck goes out
    # with nobody told. Once per document, riding the dedupe
    # apa_warn_once() already decided; the warning() itself still fires for
    # tests, programmatic callers, and decks that opt warnings back on.
    if (apa_warn_once("quarto-pptx-not-supported", msg)) {
      pptx_notice <- paste0("Warning: ", msg)
    }
  }

  # Slides first, but ONLY revealjs -- apa_quarto_revealjs_target() answers
  # non-NA for "revealjs" alone (APA_REVEALJS_FORMATS does not contain
  # "pptx"). A revealjs deck loads no apaquarto, so it must not
  # reach apa_quarto_markdown_block(), which composes an .apa-table-meta div
  # no filter will read and which decides the docx-only size marker and
  # landscape wrap. Neither is quiet by construction here -- both are quiet
  # only because apa_quarto_docx_target() is FALSE -- so route around it
  # rather than relying on that.
  #
  # A pptx deck is NOT routed here and DOES fall through to
  # apa_quarto_markdown_block() below, deliberately. apa_latex_target() is
  # also FALSE for it, so it reaches the markdown branch at the bottom of
  # this function -- the same branch docx and HTML share -- which is exactly
  # where its apa_slide_flatten_spanners() call lives. The docx-only markers
  # that function also composes stay inert for pptx the same way they do for
  # HTML: apa_quarto_docx_target() and apa_quarto_html_target() both answer
  # FALSE for a pptx render.
  if (!is.na(apa_quarto_revealjs_target())) {
    return(apa_quarto_revealjs_block(x, font.size = font.size,
                                     title = title, note = note))
  }

  if (apa_latex_target()) {
    latex_block <- apa_quarto_latex_block(x, font.size = font.size)
    meta_div <- apa_meta_div_for(x, title = title, note = note)
    # A blank line is REQUIRED between the raw LaTeX block and the div, or
    # pandoc swallows the `::: {.apa-table-meta}` fence as part of the raw
    # LaTeX and it never becomes a Div node (measured). The div
    # goes inside the SAME asis_output as the longtable so it lands inside
    # the float Quarto builds from this chunk.
    return(knitr::asis_output(
      paste0(as.character(latex_block), "\n\n", meta_div)))
  }

  # apa_quarto_markdown_block() composes its OWN meta div -- do not add
  # another one here. See the landmine comment above this function.
  block <- apa_quarto_markdown_block(x, font.size = font.size, title = title,
                                     note = note)
  # Only a pptx render ever sets pptx_notice, and a pptx render always
  # reaches this branch (revealjs_target is NA for it, latex_target FALSE).
  if (is.null(pptx_notice)) return(block)
  knitr::asis_output(paste0(pptx_notice, "\n\n", as.character(block)))
}

apa_quarto_dispatch <- function(x, called, title = NULL, note = NULL,
                                font.size = NULL) {
  actual <- apa_quarto_actual_format()
  actual_entry <- apa_quarto_actual_entry(actual)
  # No pptx case here: that check lives in apa_quarto_render(), where every
  # route passes -- including apa_quarto_knit_print() (R/quarto.R), which
  # never passes through this function. Only the generic name/target
  # mismatch is handled here.
  if (!is.na(actual_entry) && !identical(actual_entry, called)) {
    # ONCE per (called, actual) pair per document, via the same
    # apa_warn_once() registry page-geometry.R and docx-landscape.R use for a
    # document-wide fact -- not a per-table one. Without it a manuscript with
    # N tables that all call the wrong-named entry point prints N IDENTICAL
    # guidance lines in the rendered draft (measured: 8, in the example
    # document), and a line repeated N times is exactly the noise a rendered
    # manuscript must not carry. Keyed on BOTH `called` and `actual`, not a
    # constant string: a document that mixes apa.quarto.pdf() and
    # apa.quarto.html() calls against one actual render states two
    # genuinely different facts (which call is wrong), so each pattern is
    # still told once, not silenced by the other having already fired. A
    # pptx render never reaches this branch (`actual_entry` is NA for it,
    # see apa_quarto_actual_entry()'s own comment) -- it gets its own key,
    # "quarto-pptx-not-supported", in apa_quarto_render() above.
    apa_warn_once(
      paste("quarto-target-mismatch", called, actual, sep = "\r"),
      apa_quarto_target_message(called = called, actual = actual))
  }
  apa_quarto_render(x, title = title, note = note, font.size = font.size)
}

#' Display an apaTables table in a Quarto document
#'
#' You have made a table, and your manuscript is a Quarto document. Put the
#' table in with a short code chunk, using the function named for the
#' document's \code{format:} line:
#'
#' \preformatted{```{r}
#' #| label: tbl-correlations
#' #| message: false
#' #| warning: true
#' library(apaTables)
#' table1 <- apa.cor.table(attitude)
#' apa.quarto.pdf(table1)
#' ```}
#'
#' Render the document, and the table appears fully APA-formatted -- number,
#' italic title, and \emph{Note.} included -- with nothing else written.
#' (New to Quarto? It is a free system for writing documents that combine
#' text and R code, at \url{https://quarto.org}, and
#' \code{\link{apa.quarto.new}("my-paper")} creates a working APA
#' manuscript with everything already installed. A slide deck works
#' differently; see \strong{Slides are different} below.)
#'
#' There are four of these functions, one named for each destination --
#' \code{apa.quarto.pdf()}, \code{apa.quarto.docx()}, \code{apa.quarto.html()},
#' \code{apa.quarto.revealjs()}. The first three put a table on a manuscript page
#' and behave identically: put one of them on the last line of a table's code
#' chunk and the table appears in APA style, number, title and note included,
#' with nothing else written. \strong{The name states intent; the document's
#' own YAML \code{format:} line decides what is actually produced.} A document
#' rendered with \code{format: apaquarto-docx} gets a correct Word table even from a chunk that calls
#' \code{apa.quarto.pdf()}. Quarto's output format is a property of the
#' document, not of a chunk, so a function name can only
#' ever state what you meant. If the name and the render disagree, the
#' rendered draft carries one line saying exactly which YAML line would make
#' them agree, for example:
#'
#' \preformatted{This document renders to `apaquarto-pdf`. To produce Word,
#' set `format: apaquarto-docx` in the YAML.}
#'
#' The table itself is unaffected either way -- the message is guidance
#' only. \code{\link{apa.quarto.template}} writes
#' the whole chunk for you, and its \code{target} argument picks which of the
#' four names it prints.
#'
#' \code{apa.quarto.revealjs()} goes on a slide deck rather than a manuscript
#' page, and differs from the other three in several ways that matter --
#' see its own \strong{Slides are different} section below.
#'
#' The title and note come from the table itself by default on the three
#' manuscript destinations. Three things can override them, each winning
#' over the one after it: the chunk's own \code{tbl-cap} / \code{apa-note} settings
#' lines, then the \code{title} / \code{note} arguments below, then the
#' generated value. Supplying both a chunk option and the matching argument is
#' not an error, but only the chunk option is used. This is identical across
#' the three manuscript destinations; \code{apa.quarto.revealjs()} does not use
#' \code{tbl-cap} or \code{apa-note} at all, because a slide deck has no float
#' for either to attach to (see \strong{Slides are different} below).
#' \code{\link{apa.quarto.template}} writes the whole chunk for you to copy;
#' the chunk it prints has no \code{tbl-cap} or \code{apa-note} line at all,
#' because both are automatic -- add one only for your own wording.
#'
#' A chunk needs a label to become a table at all. Give it a settings line
#' \code{#| label: tbl-something} -- \code{\link{apa.quarto.template}} always
#' writes one. Without a \code{tbl-} label, Quarto builds no float for the
#' chunk, so there is no number, no title (generated or written by hand), and
#' no note, no matter what \code{tbl-cap}, \code{apa-note}, \code{title}, or
#' \code{note} say. (This does not apply to \code{apa.quarto.revealjs()}: a slide
#' deck builds no float at all, with or without a label, so a table appears
#' regardless -- see \strong{Slides are different} below for how its title and
#' note are requested instead.)
#'
#' Nothing needs to be said about the size of the table or the shape of the
#' page. When a table is too wide for an upright (portrait) page, it is placed
#' on a rotated (landscape) page automatically, at full size. That is what
#' APA style asks for: "if a table is too wide to fit on one page, use
#' landscape orientation on the page with the wide table." A table too wide to
#' fit even at its smallest readable size is printed smaller still instead --
#' as small as it has to be, so that no column is cut off. When that takes the
#' type below 6 points, where an APA table stops being comfortable to read, a
#' warning in the rendered draft says so and points to \code{\link{apa.save}}
#' for a full-size Word version; the table itself is still complete. A table
#' taller than the page continues onto the next one, with the column headings
#' repeated and the note below the last piece. (On an HTML render there is no
#' page at all, so none of this applies: nothing rotates and nothing shrinks
#' -- the table renders at full size regardless of width. On a slide render there is no page either, so nothing rotates there. But
#' a slide's fixed viewport means shrinking IS meaningful, and \code{apa.quarto.revealjs()} uses
#' its own separate automatic sizing to fit the table on the slide rather
#' than this rotate-then-shrink sequence; see \strong{Slides are different}
#' below.)
#'
#' There is one table shape apaTables cannot rescue. The space between columns is a fixed 4 points and does not
#' shrink with the type, so a table of more than roughly 136 columns is wider
#' than a rotated page at every possible font size. Such a table is warned
#' about explicitly. No apaTables function produces one from real data.
#'
#' Automatic measurement can be overruled per table with the \code{landscape}
#' argument every builder accepts, and per chunk with a settings line, which wins over the argument. Add
#' \code{#| apa-landscape: true} to a code chunk
#' to place that table on a rotated page even though it would otherwise stand
#' upright, or \code{#| apa-landscape: false} to forbid rotation and keep it
#' on an upright page instead. A wide table is then shrunk to fit rather
#' than losing a column.
#'
#' Rotating a page needs a label. Give every table chunk a settings line
#' \code{#| label: tbl-something} -- the label a table needs anyway for its
#' number and for references to it. A chunk without one is not a table as far
#' as the document is concerned, so its page cannot be turned. A wide table in such a chunk is printed smaller on the upright page
#' instead, with every column kept. If the chunk asked for a rotated page,
#' the rendered draft says the label is what is missing.
#'
#' @section Journal mode (a PDF manuscript setting) is not supported:
#' This section concerns PDF manuscripts only. A slide deck never uses
#' apaquarto, so it is unaffected; so are Word and HTML output.
#'
#' apaquarto can typeset a manuscript as a printed journal article
#' (\code{documentmode: jou}), in two narrow columns. Do not put apaTables
#' tables in such a document. A two-column page cannot break a table across
#' pages, so rows, columns and whole notes can be missing from the rendered PDF
#' with nothing reported anywhere. A document of four ordinary tables came
#' out with two of the notes and several variable names simply absent.
#' A warning says so during the render, but it is a warning and not a repair:
#' there is no setting that makes journal mode safe. Render the manuscript
#' with \code{documentmode: man} instead, which is what a journal is submitted
#' in, or save the table as a Word file with \code{\link{apa.save}}.
#'
#' @param x An apaTables table (an \code{apa.*.table()} result or an
#'   \code{apa_table_spec}).
#' @param title Optional table title, overriding the one apaTables generated.
#'   A plain character string, or markdown (e.g. \code{"Study *N* by Group"}).
#'   A \code{tbl-cap} settings line in the chunk still takes precedence over
#'   this argument. Default \code{NULL} uses the generated title. \strong{On
#'   \code{apa.quarto.revealjs()} this is different}, because a slide reads no
#'   \code{tbl-cap}: the default \code{NULL} puts no title on the slide at
#'   all, and \code{TRUE} (rather than a string) requests the generated title
#'   -- see \strong{Slides are different} below.
#' @param note Optional table note, overriding the one apaTables generated.
#'   A plain character string, or markdown. An \code{apa-note} settings line
#'   in the chunk still takes precedence over this argument. Default
#'   \code{NULL} uses the generated note. A note given here is one paragraph;
#'   apaquarto starts a new paragraph wherever the text contains a double
#'   quote, a comma and a double quote in a row, so a note written with that
#'   sequence in it is split there. This is long-standing apaquarto
#'   behaviour, and the same is true of a hand-written \code{apa-note}
#'   settings line. A generated note is unaffected. \strong{On
#'   \code{apa.quarto.revealjs()} this is different}, because a slide reads no
#'   \code{apa-note}: the default \code{NULL} puts no note on the slide at
#'   all, and \code{TRUE} (rather than a string) requests the generated note
#'   -- see \strong{Slides are different} below.
#' @param font.size Optional explicit type size in points for the table
#'   body, overriding the automatic rotate-then-shrink sequence. The title
#'   above the table and the note below it keep their usual size. PDF, HTML
#'   and slides use the size exactly, fractions included. \strong{Word is
#'   different}: it takes a table's type size from a style in the document
#'   template, so it uses the requested size rounded down to a whole point,
#'   kept between 6 and 12, and says in
#'   the rendered draft when it had to adjust. A size below 6 points, where
#'   an APA table stops being comfortable to read, warns on every
#'   destination. Nothing is ever refused or cut off. Default \code{NULL}
#'   lets apaTables choose.
#' @return The table, ready for the document (a \code{knit_asis} object).
#' @seealso
#' \code{\link{apa.quarto.new}("my-paper")} starts an APA manuscript with the
#' apaquarto extension already installed; \code{\link{apa.quarto.setup}()}
#' installs the extension into a project you already have. A step-by-step
#' guide to placing apaTables tables in Quarto documents also comes
#' installed with the package. To open it, type:
#'
#' \preformatted{vignette("apaquarto-usage", package = "apaTables")}
#'
#' A table leaves apaTables by one of two routes, and this is one of them.
#' \code{apa.quarto.pdf()} / \code{apa.quarto.docx()} /
#' \code{apa.quarto.html()} / \code{apa.quarto.revealjs()} put a table
#' inside a Quarto document (a manuscript page or a slide deck);
#' \code{\link{apa.save}} writes a table in a Word (\code{.docx}) or HTML file of its own.
#' \code{\link{apa.quarto.template}} writes the whole code chunk for you to
#' copy; \code{\link{apa.quarto.new}} creates a Quarto project that renders an
#' APA manuscript on the first try.
#' @examples
#' # Build the table once; display it with the entry point named for the
#' # document's own format: line
#' my_table <- apa.cor.table(attitude)
#'
#' # In a document whose YAML says format: apaquarto-pdf
#' apa.quarto.pdf(my_table)
#'
#' # format: apaquarto-docx
#' apa.quarto.docx(my_table)
#'
#' # format: apaquarto-html
#' apa.quarto.html(my_table)
#'
#' # format: revealjs -- a slide has no automatic title or note, so pass
#' # note = TRUE to show the note that explains the significance stars
#' apa.quarto.revealjs(my_table)
#' apa.quarto.revealjs(my_table, note = TRUE)
#' @rdname apa.quarto.pdf
#' @export
apa.quarto.pdf <- function(x, title = NULL, note = NULL, font.size = NULL) {
  apa_quarto_dispatch(x, called = "pdf", title = title, note = note,
                      font.size = font.size)
}

#' @rdname apa.quarto.pdf
#' @export
apa.quarto.docx <- function(x, title = NULL, note = NULL, font.size = NULL) {
  apa_quarto_dispatch(x, called = "docx", title = title, note = note,
                      font.size = font.size)
}

#' @rdname apa.quarto.pdf
#' @export
apa.quarto.html <- function(x, title = NULL, note = NULL, font.size = NULL) {
  apa_quarto_dispatch(x, called = "html", title = title, note = note,
                      font.size = font.size)
}
