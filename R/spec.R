#' Construct an apaTables table spec (render-agnostic IR)
#' @keywords internal
apa_table_spec <- function(body, col_labels, col_align,
                           title = NULL, notes = list(),
                           number = NA_integer_, spanners = list(),
                           row_groups = list(), landscape = NA,
                           meta = list()) {
  x <- structure(
    list(number = number, title = title, notes = notes,
         body = body, col_labels = col_labels, col_align = col_align,
         spanners = spanners, row_groups = row_groups,
         landscape = landscape, meta = meta),
    class = "apa_table_spec"
  )
  validate_apa_table_spec(x)
}

#' @keywords internal
apa_spec_start <- function(n_rows) {
  structure(
    list(n_rows = n_rows, col_labels = list(), col_align = character(0),
         columns = list(), spanners = list(), row_groups = list(), notes = list()),
    class = "apa_table_spec_wip"
  )
}

#' @keywords internal
apa_col <- function(spec, label, align = "r", values) {
  stopifnot(inherits(spec, "apa_table_spec_wip"))
  if (length(values) != spec$n_rows) stop("apa_col: values must have n_rows entries.")
  lab <- if (is.character(label)) richtext(label) else label
  cells <- lapply(values, function(v) if (is.character(v)) richtext(v) else v)
  spec$col_labels <- c(spec$col_labels, list(lab))
  spec$col_align  <- c(spec$col_align, align)
  spec$columns    <- c(spec$columns, list(cells))
  spec
}

#' @keywords internal
apa_spanner <- function(spec, label, cols, level = 1) {
  lab <- if (is.character(label)) richtext(label) else label
  spec$spanners <- c(spec$spanners, list(list(label = lab, cols = cols, level = level)))
  spec
}

#' @keywords internal
apa_row_group <- function(spec, label, rows) {
  lab <- if (is.character(label)) richtext(label) else label
  spec$row_groups <- c(spec$row_groups, list(list(label = lab, rows = rows)))
  spec
}

#' @keywords internal
apa_note <- function(spec, ...) {
  parts <- lapply(list(...), function(v) if (is.character(v)) richtext(v) else v)
  spec$notes <- c(spec$notes, parts)
  spec
}

#' @keywords internal
apa_spec_finish <- function(spec, title = NULL, number = NA_integer_,
                            landscape = NA, meta = list()) {
  # transpose columns -> rows
  body <- lapply(seq_len(spec$n_rows), function(i) {
    lapply(spec$columns, function(col) col[[i]])
  })
  ttl <- if (is.character(title)) richtext(title) else title
  apa_table_spec(body = body, col_labels = spec$col_labels,
                 col_align = spec$col_align, title = ttl, notes = spec$notes,
                 number = number, spanners = spec$spanners,
                 row_groups = spec$row_groups, landscape = landscape, meta = meta)
}

#' Validate a table spec
#' @keywords internal
validate_apa_table_spec <- function(x) {
  n_col <- length(x$col_labels)
  if (length(x$col_align) != n_col) {
    stop("apa_table_spec: col_align length must match number of col_labels (alignment).")
  }
  if (!all(x$col_align %in% c("l", "r", "c"))) {
    stop("apa_table_spec: col_align values must each be one of 'l', 'r', 'c' (alignment).")
  }
  widths <- vapply(x$body, length, integer(1))
  if (length(widths) && !all(widths == n_col)) {
    stop("apa_table_spec: every body row must have one cell per column.")
  }
  n_row <- length(x$body)
  for (sp in x$spanners) {
    if (!all(sp$cols %in% seq_len(n_col))) {
      stop("apa_table_spec: spanner cols must index existing columns (1:n_col).")
    }
  }
  for (rg in x$row_groups) {
    if (!all(rg$rows %in% seq_len(n_row))) {
      stop("apa_table_spec: row group rows must index existing rows (1:n_row).")
    }
  }
  invisible(x)
}

#' Interpret a user-supplied table.number
#'
#' Returns the number to print as a positive integer, or `NA_integer_` when the
#' table is to carry no number at all. `0` is the documented spelling for
#' "no number"; `NA` and `NULL` are accepted as synonyms.
#' @keywords internal
apa_table_number_value <- function(table.number) {
  if (is.null(table.number)) return(NA_integer_)
  if (length(table.number) != 1L) {
    stop("table.number must be a single number. Use 0 (or NA, or NULL) for a table with no number.",
         call. = FALSE)
  }
  if (is.na(table.number)) return(NA_integer_)
  if (!is.numeric(table.number)) {
    stop("table.number must be a number. Use 0 (or NA, or NULL) for a table with no number.",
         call. = FALSE)
  }
  n <- as.integer(round(table.number))
  if (n <= 0L) NA_integer_ else n
}

# Record the table number on a finished table object: the value itself, plus
# whether the user asked for it (TRUE) or it came from the default (FALSE).
# apa.save() numbers default-numbered tables by their position in the document,
# so this flag is what keeps a number the user typed from being overwritten.
apa_set_table_number <- function(tbl, number, explicit) {
  tbl$table_number <- number
  tbl$table_number_explicit <- isTRUE(explicit)
  if (!is.null(tbl$spec)) tbl$spec$meta$number_explicit <- isTRUE(explicit)
  tbl
}

#' Coerce a builder result (or a bare spec) to an apa_table_spec
#'
#' Accepts either an `apa_table_spec` or an `apa.*.table()` result that carries
#' a nested `$spec`. This is the bridge that lets a builder's result be passed
#' straight to `apa.quarto.pdf()` while the builder keeps returning its legacy
#' object.
#' @keywords internal
as_spec <- function(x) {
  if (inherits(x, "apa_table_spec")) return(x)
  if (inherits(x, "apa_table") && !is.null(x$spec)) return(x$spec)
  stop("as_spec(): expected an apa_table_spec or an apa.*.table() result carrying a $spec.")
}

# Significance-star sentence as its own note entry: apa.save() starts each
# note entry on a new line in the Note paragraph, while apa_note_text()
# (apaquarto) joins entries with a space, so the chunk-option string is
# unchanged.
stars_spec_note <- function() {
  richtext(list(
    rt_run("* indicates "), rt_run("p", italic = TRUE),
    rt_run(" < .05. ** indicates "), rt_run("p", italic = TRUE),
    rt_run(" < .01.")))
}
