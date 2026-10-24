#' Build a destination-neutral display plan from a table spec
#'
#' Walks an \code{apa_table_spec} once and produces a plan describing how the
#' table should be laid out, with every destination-specific decision left to
#' the caller. The plan is consumed by \code{apa_latex_table()} (Quarto PDF,
#' the native LaTeX emitter) and by \code{\link{apa_flextable}()} (which now
#' writes only Word and HTML), and will also be consumed by a
#' pandoc-markdown emitter -- so it carries only
#' structure: column count and alignment codes, header rows (with spans and
#' rule ranges expressed as column indices, never as \code{\\cmidrule} or
#' any other markup), and body rows. Richtext runs (see \code{\link{rt_run}})
#' pass through unconverted so no emitter loses italics, superscripts, or
#' symbols; each destination-specific backend (\code{rt_to_paragraph()} for
#' flextable, \code{rt_run_to_ftext()} for Word via officer, \code{rt_to_markdown()}
#' for pandoc) converts them at the point of use, not here.
#'
#' \code{header} is a list of rows in RENDERING order: the highest spanner
#' level first, descending through any lower levels, with the column-label
#' row always last. Each row is \code{list(cells = <list of cell>, rules =
#' <list of column ranges>)}. Each cell is \code{list(rt = <richtext>, span =
#' <columns it covers>, align = <"l"/"r"/"c">)}; a column not covered by any
#' spanner at that level becomes a separate blank, span-1 cell, so a row's cell
#' spans always sum to \code{n_col}. \code{rules} lists the column range
#' (\code{c(from, to)}) each spanner in that row covers -- the caller decides
#' what to draw there (a \code{\\cmidrule}, a bottom border, nothing).
#'
#' \code{body} is a list of rows: a \code{list(type = "group", label =
#' <richtext>)} entry appears immediately before the first data row of a row
#' group (added by the internal \code{apa_row_group()}, which has no help page
#' to link to), and every spec body row becomes a
#' \code{list(type = "data", cells = <list of richtext, one per column>)}
#' entry. A spec with no row groups produces an all-\code{"data"} body,
#' reproducing a flat table exactly.
#'
#' Every spanner's columns must be contiguous, and no two spanners at the
#' same level may share a column; either shape makes the header-row walk
#' silently drop or misattribute a spanner, so both raise an error instead
#' of returning a wrong-but-silent plan.
#'
#' @param spec An \code{apa_table_spec}, or an \code{apa.*.table()} result
#'   carrying one (see \code{\link{as_spec}}).
#' @return A list with elements \code{n_col} (integer), \code{align}
#'   (character vector of \code{"l"}/\code{"r"}/\code{"c"}, length
#'   \code{n_col}), \code{header}, and \code{body} as described above.
#' @keywords internal
apa_display_plan <- function(spec) {
  x <- as_spec(spec)
  n_col <- length(x$col_labels)
  align <- x$col_align

  # ---- header --------------------------------------------------------
  # One row per spanner level, highest level first, then the column-label
  # row last. A spanner's "level" defaults to 1 (closest to the column
  # labels); higher levels stack above it -- see apa_spanner().
  sp_level <- function(sp) if (is.null(sp$level)) 1L else as.integer(sp$level)

  # The header-row walk below finds a spanner at a given column by testing
  # min(cols) == col, then advances by length(cols) columns. That is only
  # correct when, within one level, every spanner's columns are contiguous
  # (no gaps) and no two spanners share a column -- otherwise the walk
  # silently drops or misreads a spanner instead of failing, and the plan
  # becomes a wrong table with no error anywhere it is consumed. No shipped
  # builder produces either shape today, which is exactly why this needs a
  # loud check rather than a comment: apa_display_plan() is the seam every
  # emitter reads.
  if (length(x$spanners)) {
    for (lev in unique(vapply(x$spanners, sp_level, integer(1)))) {
      sps <- Filter(function(sp) sp_level(sp) == lev, x$spanners)
      for (sp in sps) {
        # as.integer() on the sorted side only. seq.int() already returns
        # integer for whole-number arguments, so this makes the two sides
        # comparable for a legitimate cols = c(2, 3). This is a TIGHTENING,
        # not a preservation: BEFORE this fix, sort(c(2.5, 3.5)) and
        # seq.int(2.5, 3.5) were the same double vector, so identical() was
        # TRUE and a fractional spanner was accepted silently. AFTER,
        # c(2.5, 3.5) truncates to 2:3 on the left while seq.int(2.5, 3.5)
        # stays double on the right, so identical() is FALSE and the call
        # errors -- the as.integer() fix INTRODUCED this rejection. It is
        # unreachable through apa_spec_finish() in practice: R/spec.R's
        # spanner-cols check (`sp$cols %in% seq_len(n_col)`, around :90)
        # already rejects a fractional col one layer earlier.
        if (!identical(sort(as.integer(sp$cols)),
                       seq.int(min(sp$cols), max(sp$cols)))) {
          stop(sprintf(paste0(
            "apa_display_plan: spanner columns must be contiguous (no gaps) ",
            "at level %d; got cols %s."),
            lev, paste(sp$cols, collapse = ", ")), call. = FALSE)
        }
      }
      covered <- unlist(lapply(sps, function(sp) sp$cols))
      dup <- anyDuplicated(covered)
      if (dup) {
        stop(sprintf(paste0(
          "apa_display_plan: spanners at level %d must not overlap; column ",
          "%d is covered by more than one spanner."),
          lev, covered[dup]), call. = FALSE)
      }
    }
  }

  levels_present <- if (length(x$spanners)) {
    sort(unique(vapply(x$spanners, sp_level, integer(1))), decreasing = TRUE)
  } else {
    integer(0)
  }

  header <- list()
  for (lev in levels_present) {
    sps <- Filter(function(sp) sp_level(sp) == lev, x$spanners)
    sps <- sps[order(vapply(sps, function(sp) min(sp$cols), numeric(1)))]
    cells <- list()
    rules <- list()
    col <- 1L
    while (col <= n_col) {
      hit <- Find(function(sp) min(sp$cols) == col, sps)
      if (!is.null(hit)) {
        cells[[length(cells) + 1L]] <- list(rt = hit$label,
                                            span = length(hit$cols),
                                            align = "c")
        # Integer, always: the plan is the seam every emitter reads, and a
        # double range from a caller who wrote cols = c(2, 3) would otherwise
        # travel untyped into each one's cmidrule arithmetic.
        rules[[length(rules) + 1L]] <- as.integer(c(min(hit$cols), max(hit$cols)))
        col <- col + length(hit$cols)
      } else {
        cells[[length(cells) + 1L]] <- list(rt = richtext(""), span = 1L, align = "c")
        col <- col + 1L
      }
    }
    header[[length(header) + 1L]] <- list(cells = cells, rules = rules)
  }
  header[[length(header) + 1L]] <- list(
    cells = lapply(seq_len(n_col), function(j) {
      list(rt = x$col_labels[[j]], span = 1L, align = align[j])
    }),
    rules = list())

  # ---- body ------------------------------------------------------------
  # A "group" entry before the first data row of each row group (identified
  # by the group's smallest row index), then one "data" entry per spec body
  # row, carrying its richtext cells unconverted.
  groups <- x$row_groups
  starts <- if (length(groups)) vapply(groups, function(g) min(g$rows), numeric(1)) else numeric(0)
  body <- list()
  for (i in seq_along(x$body)) {
    gi <- which(starts == i)
    if (length(gi)) {
      body[[length(body) + 1L]] <- list(type = "group", label = groups[[gi[1]]]$label)
    }
    body[[length(body) + 1L]] <- list(type = "data", cells = x$body[[i]])
  }

  list(n_col = n_col, align = align, header = header, body = body)
}
