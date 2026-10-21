# TRUE when knitr is rendering LaTeX output. apa_quarto_render()
# (R/quarto-table.R) uses it to choose the native LaTeX block over the native
# markdown block for a bare table object, which has nothing to do with
# flextable and is outside apa.save()'s call chain entirely.
#
# apa.save() deliberately consults no knit context: the file it writes is its
# own, whatever document knitr happens to be rendering around it.
# apa_flextable() writes only .docx and .html, so no LaTeX-specific fix
# belongs on its path (measured: no knit-context check is reached during
# apa.save(docx) or apa.save(html) with options(knitr.in.progress = TRUE)).
apa_latex_target <- function() {
  requireNamespace("knitr", quietly = TRUE) && knitr::is_latex_output()
}

# Loading flextable, with one known failure turned into instructions.
#
# flextable needs gdtools, and some Mac builds of gdtools (Posit Package
# Manager's, for one) link the XQuartz X11 libraries. On a Mac without
# XQuartz, loading flextable then fails with a dyn.load() error naming
# /opt/X11/lib/libXrender.1.dylib -- accurate, but no help to someone who
# just wanted a table. CRAN's own Mac binaries do not have this dependency,
# so whether a user hits it depends on where their packages come from.
#
# Only that failure is translated: the error must name an X11 library on the
# Mac library path. Any other failure is re-raised unchanged, because a
# friendly message for the wrong problem is worse than the raw error.
# The loader is its own function so tests can stand in for a broken install.
apa_load_flextable <- function() {
  if (!isNamespaceLoaded("flextable")) loadNamespace("flextable")
  invisible(TRUE)
}

apa_require_flextable <- function() {
  tryCatch(apa_load_flextable(), error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("/opt/X11/", msg, fixed = TRUE) ||
        grepl("libX[[:alnum:]]+[.0-9]*[.]dylib", msg)) {
      stop("apaTables could not load the flextable package, which it uses ",
           "to measure and save tables, because your Mac is missing ",
           "XQuartz, a free component some R packages need. Install it from ",
           "https://www.xquartz.org, then log out of your Mac, log back in, ",
           "and restart R. To keep working right now instead, run ",
           "install.packages(\"gdtools\", repos = ",
           "\"https://cloud.r-project.org\"); a later package update may ",
           "undo that, which installing XQuartz prevents.", call. = FALSE)
    }
    stop(e)
  })
  invisible(TRUE)
}

# Re-lay the table out at a given font size.
#
# autofit(add_w = 0) recomputes every column width from the text as it is set
# at that size. That is what makes this a MEASUREMENT of the content rather
# than arithmetic on a 12 pt figure, and it is why the native-LaTeX width
# model (R/render-latex.R) calls it: apa_latex_natural_width() measures the
# per-column content ONCE at a reference size through this function, then
# prices every candidate size from that vector. Nothing else uses it.
#
# The measurement is destination-neutral: it reports how wide flextable lays
# the CONTENT out, and the LaTeX width model starts from that.
apa_ft_at_size <- function(ft, size) {
  ft <- flextable::fontsize(ft, size = size, part = "all")
  flextable::autofit(ft, add_w = 0, add_h = 0)
}

# Build the themed flextable body -- data frame, header labels, group-label
# rows, spanner rows, alignment -- straight from a display plan
# (apa_display_plan()), with none of apa_flextable()'s x-specific finishing
# (the correlation/d-value row spacing, which depends on x$meta and is a
# vertical-only padding.bottom with no effect on width; set_table_properties
# for margin sharing).
#
# Separate from apa_flextable() so that apa_latex_natural_width() and
# apa_size_for_width() (R/render-latex.R) -- which receive a plan, not a
# spec -- measure against EXACTLY the same styled flextable (font, size,
# theme) that apa.save() renders, rather than a hand-rolled approximation
# that could quietly drift from it. apa_flextable(x, ...) below calls this
# helper.
apa_flextable_from_plan <- function(plan) {
  apa_require_flextable()
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

  # column alignment (header + body)
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
#' Internal. Its one caller is \code{\link{apa.save}}, when it writes a
#' \code{.docx} or \code{.html} file of its own. The Quarto entry points
#' (\code{\link{apa.quarto.pdf}}, \code{\link{apa.quarto.docx}},
#' \code{\link{apa.quarto.html}}) never reach it: PDF gets native LaTeX and
#' every other destination gets a pandoc grid table
#' (\code{apa_quarto_markdown_block()}).
#'
#' \code{apa_flextable()} converts an apaTables table (an
#' \code{apa.*.table()} result) into a "flextable", a table object from the
#' \pkg{flextable} package, styled for APA: horizontal rules only, Times New
#' Roman 12 point. The result holds the table body and column headers; the
#' table number, title and note are added by \code{\link{apa.save}}.
#'
#' Nothing here depends on the shape of a page. Fitting a table to the page
#' it is written on is \code{\link{apa.save}}'s job.
#'
#' @param x An apaTables table (an \code{apa.*.table()} result or an
#'   \code{apa_table_spec}).
#' @return A \code{flextable} object that displays the table body.
#' @keywords internal
apa_flextable <- function(x) {
  apa_require_flextable()
  # No LaTeX emphasis fix (ulem/normalem) is applied here: apa_flextable() is
  # reachable only from apa.save() and from direct unit-test calls, and never
  # reaches a LaTeX destination. See the comment above apa_latex_target().
  x <- as_spec(x)
  plan <- apa_display_plan(x)
  ft <- apa_flextable_from_plan(plan)

  # Needed again below (the final hline-per-spanner pass) --
  # apa_flextable_from_plan() computes the same value internally but does not
  # return it, since a caller building only a width measurement has no use for
  # header borders.
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
# No LaTeX bracket guard here. In LaTeX output, flextable cell content (or an
# in-cell line) that BEGINS with "[" silently vanishes, because a leading "["
# is parsed as an optional-argument bracket -- but apa_flextable() never
# reaches a LaTeX destination (see the comment above apa_latex_target()), so
# there is no PDF for it to vanish from. If that ever changes, a guard is
# needed again.
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
