# apa_direct_output()/apa_direct_output_active() USED TO answer "is the table
# being built right now destined for the document that knitr is rendering?",
# guarding apa.save()'s own file against whatever knit context happened to be
# in progress around it. That mattered for two real mechanisms -- the LaTeX
# width fitter (deleted Phase 2 Task 10) and the leading-bracket escape in
# rt_to_paragraph() plus apa_restore_emph() (both deleted Phase 1.6a Task 9,
# confirmed dead by rendering) -- and once both were gone the flag was
# vestigial: MEASURED (Task 9 fix round 1) at zero calls to either reader
# during apa.save(docx) or apa.save(html) with options(knitr.in.progress =
# TRUE) set. Phase 1.11 re-measured only apa_direct_output_active() on the
# tree three phases later, got zero calls again, and removed
# apa_direct_output(), apa_direct_output_active() and the apa_direct env,
# plus the two call sites in R/save.R.
#
# apa_latex_target() ITSELF SURVIVED the removal: apa_quarto_render()
# (R/quarto-table.R:1327) still uses it to choose the native LaTeX block over
# the native markdown block for a bare table object (D30's dispatch), which
# has nothing to do with flextable. That call site is outside apa.save()'s
# chain entirely -- settled by static reachability, not by the zero-call
# count above, which only ever covered apa_direct_output_active(). Only the
# apa_direct_output_active() early return went with the rest.
apa_latex_target <- function() {
  requireNamespace("knitr", quietly = TRUE) && knitr::is_latex_output()
}

# Re-lay the table out at a given font size.
#
# autofit(add_w = 0) recomputes every column width from the text as it is set
# at that size. That is what makes this a MEASUREMENT of the content rather
# than arithmetic on a 12 pt figure, and it is why Task 4's native-LaTeX width
# model (R/render-latex.R) calls it: apa_latex_natural_width() measures the
# per-column content ONCE at a reference size through this function, then
# prices every candidate size from that vector. Nothing else uses it.
#
# It survived Task 10, which deleted the flextable fitter it was written for
# (apa_fit_by_measure(), apa_latex_table_width()), because the measurement it
# performs is destination-neutral: it reports how wide flextable lays the
# CONTENT out, and both the old flextable fitter and the new LaTeX width model
# start from that.
apa_ft_at_size <- function(ft, size) {
  ft <- flextable::fontsize(ft, size = size, part = "all")
  flextable::autofit(ft, add_w = 0, add_h = 0)
}

# Build the themed flextable body -- data frame, header labels, group-label
# rows, spanner rows, alignment -- straight from a display plan (Task 2's
# apa_display_plan()), with none of apa_flextable()'s x-specific finishing
# (the correlation/d-value row spacing, which depends on x$meta and is a
# vertical-only padding.bottom with no effect on width; set_table_properties
# for margin sharing).
#
# Factored out of apa_flextable() (Task 4, render-latex.R's width model) so
# that apa_latex_natural_width()/apa_size_for_width() -- which receive a
# plan, not a spec -- measure against EXACTLY the same styled flextable
# (font, size, theme) that the real Word/PDF-via-flextable path renders,
# rather than a hand-rolled approximation that could quietly drift from it.
# apa_flextable(x, ...) below is unchanged behaviourally: it now calls this
# helper instead of inlining the same steps.
apa_flextable_from_plan <- function(plan) {
  n_col <- plan$n_col
  keys <- paste0("c", seq_len(n_col))

  # Display rows come straight from the plan: a "group" entry carries a
  # richtext label and prints on its own merged row; a "data" entry carries
  # one richtext cell per column. No groups -> all data rows, which
  # reproduces the flat layout exactly.
  row_text <- function(p) {
    if (identical(p$type, "group")) c(as_plaintext(p$label), rep("", n_col - 1L))
    else vapply(p$cells, as_plaintext, character(1))
  }
  df <- as.data.frame(do.call(rbind, lapply(plan$body, row_text)), stringsAsFactors = FALSE)
  names(df) <- keys

  ft <- flextable::flextable(df)

  # header labels with real richtext styling (the column-label row is always
  # last in plan$header)
  label_row <- plan$header[[length(plan$header)]]
  for (j in seq_len(n_col)) {
    ft <- flextable::compose(ft, j = keys[j], part = "header",
                             value = rt_to_paragraph(label_row$cells[[j]]$rt))
  }

  # body cells: richtext on data rows; merged group label on group rows
  label_rows <- integer(0)
  for (d in seq_along(plan$body)) {
    p <- plan$body[[d]]
    if (identical(p$type, "group")) {
      label_rows <- c(label_rows, d)
      ft <- flextable::compose(ft, i = d, j = keys[1], part = "body",
                               value = rt_to_paragraph(p$label))
      if (n_col > 1) {
        for (j in 2:n_col) {
          ft <- flextable::compose(ft, i = d, j = keys[j], part = "body",
                                   value = flextable::as_paragraph(flextable::as_chunk("")))
        }
      }
    } else {
      for (j in seq_len(n_col)) {
        ft <- flextable::compose(ft, i = d, j = keys[j], part = "body",
                                 value = rt_to_paragraph(p$cells[[j]]))
      }
    }
  }

  # column alignment (header + body), matching the Plan-1 calls so flat specs are unchanged
  align_map <- c(l = "left", r = "right", c = "center")
  for (j in seq_len(n_col)) {
    ft <- flextable::align(ft, j = keys[j], align = align_map[[plan$align[j]]], part = "all")
  }
  # group-label rows override: merge across all columns, left align, bold
  for (d in label_rows) {
    ft <- flextable::merge_at(ft, i = d, j = seq_len(n_col), part = "body")
    ft <- flextable::align(ft, i = d, align = "left", part = "body")
    ft <- flextable::bold(ft, i = d, part = "body")
  }

  # spanners: one header row per LEVEL, already ordered by apa_display_plan()
  # with the highest level first and the column-label row last -- so
  # plan$header[-length(plan$header)] is exactly the spanner rows, top row
  # first. add_header_row(top = TRUE) inserts a new row above whatever is
  # currently topmost, so rows must go in from the BOTTOM spanner row (the
  # level closest to the column labels) up to the TOP -- the reverse of
  # plan$header's own order -- to end with the highest level on top. hline is
  # applied AFTER the theme (theme's border_remove wipes earlier borders),
  # further down. With a single spanner row this reproduces the single-row
  # layout exactly.
  spanner_rows <- plan$header[-length(plan$header)]
  if (length(spanner_rows)) {
    for (k in rev(seq_along(spanner_rows))) {
      row <- spanner_rows[[k]]
      top <- character(0)
      for (cell in row$cells) top <- c(top, rep(as_plaintext(cell$rt), cell$span))
      ft <- flextable::add_header_row(ft, top = TRUE, values = top,
                                      colwidths = rep(1, n_col))
    }
    for (k in seq_along(spanner_rows)) {
      row <- spanner_rows[[k]]
      for (rng in row$rules) {
        ft <- flextable::merge_at(ft, i = k, j = rng[1]:rng[2], part = "header")
      }
      ft <- flextable::align(ft, i = k, align = "center", part = "header")
    }
  }

  apa_flextable_theme(ft)
}

#' Convert an apaTables table to a flextable (internal)
#'
#' INTERNAL as of Phase 2 (D8): the public chunk functions are
#' \code{\link{apa.quarto.pdf}} / \code{\link{apa.quarto.docx}} /
#' \code{\link{apa.quarto.html}}. D14 is complete as of Phase 1.6a (Task 8):
#' this builder is now used for exactly the ONE destination D14 always meant
#' to leave it -- \code{\link{apa.save}} writing a \code{.docx} or
#' \code{.html} file of its own. None of the three Quarto entry points reach
#' it any more; they route through the native pandoc grid-table emitter
#' (\code{apa_quarto_markdown_block()}) on every non-PDF destination.
#' Un-exporting cost nothing only because \code{apa_flextable()} arrived with
#' the unreleased 4.0 work; CRAN carries 3.0.1, which has no such function.
#'
#' \code{apa_flextable()} converts an apaTables table (an
#' \code{apa.*.table()} result) into a "flextable" -- a table format, from
#' the \pkg{flextable} package, that displays correctly when a document is
#' turned into Word, PDF, or HTML. Place the call inside a code chunk of an
#' apaquarto document and the table appears with APA styling (horizontal
#' rules only, Times New Roman 12 point).
#'
#' The result contains only the table body and column headers. The table
#' number, title, and note are added by apaquarto from the \code{tbl-cap}
#' and \code{apa-note} settings lines of the chunk -- see the \emph{Quarto
#' documents} section of any table function's help page (for example,
#' \code{?apa.cor.table}), or run \code{\link{apa.quarto.template}} in the
#' R console, which writes the whole chunk for you to copy into your Quarto
#' document. The helpers \code{\link{apa_caption}} and
#' \code{\link{apa_note_text}} supply the caption and note text.
#'
#' Nothing here is affected by the shape of any page. The width fitting that
#' used to live in this function -- and with it the \code{fit_to_page} and
#' \code{page} arguments -- belonged to the Quarto PDF path, which now emits
#' native LaTeX from \code{\link{apa.quarto.pdf}} and decides size and
#' rotation there (Task 10 removed both arguments). What reaches Word is fitted
#' by \code{\link{apa.save}} against the page it is actually writing.
#'
#' @param x An apaTables table (an \code{apa.*.table()} result or an
#'   \code{apa_table_spec}).
#' @return A \code{flextable} object that displays the table body.
#' @keywords internal
apa_flextable <- function(x) {
  # apa_restore_emph() USED TO be called here (the ulem/normalem fix for
  # flextable's LaTeX dependencies) -- removed in Task 9 (Phase 1.6a) as
  # confirmed dead: apa_flextable() is reachable only from apa.save() (which
  # used to shield its own document with apa_direct_output(), removed as
  # vestigial in Phase 1.11) and from direct unit-test calls with no knit
  # session in progress, so the LaTeX-only defect it corrected can no longer
  # occur. See the comment above apa_latex_target()'s definition in this file.
  x <- as_spec(x)
  plan <- apa_display_plan(x)
  ft <- apa_flextable_from_plan(plan)

  # Needed again below (the final hline-per-spanner pass) --
  # apa_flextable_from_plan() computes the same value internally but does not
  # return it, since a caller building only a width measurement (Task 4) has
  # no use for header borders.
  spanner_rows <- plan$header[-length(plan$header)]

  # correlation / d-value tables: a blank line of separation below each
  # variable row -- the stacked estimate/CI/p lines otherwise read as one block
  table_type <- if (is.list(x$meta)) x$meta$table.type else NULL
  if (identical(table_type, "correlation") || identical(table_type, "dvalue")) {
    n_rows <- length(plan$body)
    if (n_rows > 1L) {
      ft <- flextable::padding(ft, i = seq_len(n_rows - 1L),
                               padding.bottom = 14, part = "body")
    }
  }

  # the table shares the left margin with the title and note around it
  ft <- flextable::set_table_properties(ft, align = "left")

  if (length(spanner_rows)) {
    for (k in seq_along(spanner_rows)) {
      for (rng in spanner_rows[[k]]$rules) {
        ft <- flextable::hline(ft, i = k, j = rng[1]:rng[2],
                               border = officer::fp_border(width = 1), part = "header")
      }
    }
  }
  ft
}

#' One styled run converted to a flextable chunk
#' @keywords internal
rt_run_to_chunk <- function(r) {
  va <- if (identical(r$script, "super")) "superscript"
        else if (identical(r$script, "sub")) "subscript"
        else "baseline"
  flextable::as_chunk(
    r$text,
    props = officer::fp_text(
      font.size = NA_real_, color = "black",
      italic = isTRUE(r$italic), bold = isTRUE(r$bold),
      vertical.align = va,
      font.family = "Times New Roman"))
}

#' Map one richtext (or richtext_lines) to a flextable as_paragraph value
#' @keywords internal
#
# A LaTeX bracket guard used to live here: in PDF/LaTeX output, flextable cell
# content (or an in-cell line) that BEGINS with "[" silently vanished, because
# a leading "[" is parsed as an optional-argument bracket. It was removed in
# Task 9 (Phase 1.6a) as confirmed dead, for the same reason recorded above
# apa_latex_target()'s definition in this file: apa_flextable() can no longer
# reach a LaTeX destination (the apa_direct_output() guard that used to gate
# apa.save()'s call chain was itself removed as vestigial in Phase 1.11), so
# there is no PDF for a leading "[" to vanish from.
rt_to_paragraph <- function(rt) {
  if (inherits(rt, "apa_richtext_lines")) {
    parts <- list()
    for (k in seq_along(rt)) {
      if (k > 1) parts <- c(parts, list(flextable::as_chunk("\n")))
      line <- rt[[k]]
      runs <- if (is.character(line)) list(rt_run(line)) else unclass(line)
      parts <- c(parts, lapply(runs, rt_run_to_chunk))
    }
    return(do.call(flextable::as_paragraph, parts))
  }
  runs <- if (is.character(rt)) list(rt_run(rt)) else unclass(rt)
  do.call(flextable::as_paragraph, lapply(runs, rt_run_to_chunk))
}

#' Apply APA styling to a flextable (horizontal rules only, Times 12pt)
#' @keywords internal
apa_flextable_theme <- function(ft) {
  big_border <- officer::fp_border(width = 1, color = "black")
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_top(ft, border = big_border, part = "header")
  ft <- flextable::hline_bottom(ft, border = big_border, part = "header")
  ft <- flextable::hline_bottom(ft, border = big_border, part = "body")
  ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
  ft <- flextable::fontsize(ft, size = 12, part = "all")
  ft <- flextable::padding(ft, padding.top = 1, padding.bottom = 1, part = "all")
  ft <- flextable::autofit(ft)
  ft
}
