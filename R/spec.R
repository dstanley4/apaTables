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
  if (!is.finite(table.number) || table.number < 0 ||
      table.number != round(table.number) || table.number > .Machine$integer.max) {
    stop("table.number must be a whole number (1, 2, 3, ...). Use 0 (or NA, or NULL) for a table with no number.",
         call. = FALSE)
  }
  n <- as.integer(table.number)
  if (n == 0L) NA_integer_ else n
}

# Stop unless landscape is TRUE, FALSE or NA. Every table function calls
# this first, so a mistyped value is reported rather than read as NA.
apa_check_landscape <- function(landscape) {
  if (!is.logical(landscape) || length(landscape) != 1L) {
    stop("landscape must be TRUE, FALSE or NA. NA, the default, lets apaTables decide by measuring the table.",
         call. = FALSE)
  }
  invisible(landscape)
}

# Stop unless a confidence level is one the table offers (.90 or .95).
apa_check_conf_level <- function(value, arg) {
  ok <- is.numeric(value) && length(value) == 1L && !is.na(value) &&
    any(abs(value - c(.90, .95)) < 1e-8)
  if (!ok) {
    stop(sprintf("%s must be .90 or .95.", arg), call. = FALSE)
  }
  as.numeric(c(.90, .95)[which.min(abs(value - c(.90, .95)))])
}

# Stop unless a show/hide argument is a single TRUE or FALSE (1 and 0, which
# R users often write for them, are accepted too).
apa_check_flag <- function(value, arg) {
  ok <- length(value) == 1L && !is.na(value) &&
    (is.logical(value) || (is.numeric(value) && value %in% c(0, 1)))
  if (!ok) {
    stop(sprintf("%s must be TRUE or FALSE.", arg), call. = FALSE)
  }
  as.logical(value)
}

# The file a builder writes: NA (or NULL) for no file, otherwise one name.
apa_check_filename <- function(filename) {
  if (is.null(filename) || (length(filename) == 1L && is.na(filename))) {
    return(NA)
  }
  if (!is.character(filename) || length(filename) != 1L || !nzchar(filename)) {
    stop("filename must be one file name, such as \"Table1.docx\", or NA for no file.",
         call. = FALSE)
  }
  filename
}

# Stop unless x is a model fitted with lm() (an aov() fit counts). `which`
# names the model in the message: "the model", "block 2", ...
apa_check_lm <- function(fn, x, which = "the model", allow_aov = TRUE) {
  if (inherits(x, "lm") && !inherits(x, c("glm", "mlm")) &&
      (allow_aov || !inherits(x, "aov"))) {
    return(invisible(x))
  }
  got <- if (inherits(x, "aov") && !inherits(x, "aovlist") && !inherits(x, "mlm")) {
    "was fitted with aov(); fit it with lm() instead"
  } else if (inherits(x, "glm")) {
    "was fitted with glm()"
  } else if (inherits(x, "aovlist")) {
    "is an aov() fit with an Error() term (for repeated measures, use apa.afex.table())"
  } else if (inherits(x, "mlm")) {
    stop(sprintf("%s() needs a model with one dependent variable, but %s has more than one (cbind() on the left of ~).",
                 fn, which), call. = FALSE)
  } else {
    paste("is", apa_describe_object(x))
  }
  stop(sprintf("%s() needs models fitted with lm(), but %s %s.", fn, which, got),
       call. = FALSE)
}

# What an object is, for messages: "a data frame", "an object of class x".
apa_describe_object <- function(x) {
  if (inherits(x, "apa.table")) {
    return("a table made by an older version of apaTables (build it again with this version)")
  }
  if (is.data.frame(x)) return("a data frame")
  if (is.null(x)) return("NULL")
  if (is.factor(x)) return("a factor")
  if (is.character(x)) return("text")
  if (is.numeric(x)) return(if (length(x) == 1L) "a number" else "numbers")
  if (is.logical(x)) return("TRUE/FALSE values")
  if (is.list(x) && !is.object(x)) return("a list")
  paste0("an object of class ", class(x)[1])
}

# The exported apaTables function a message raised deep inside it belongs
# to: the innermost frame running one, matched by the function itself, so
# lapply() and do.call() find it too.
apa_user_function <- function() {
  ns <- asNamespace("apaTables")
  exported <- getNamespaceExports("apaTables")
  fns <- mget(exported, envir = ns, mode = "function",
              ifnotfound = list(NULL))
  parents <- sys.parents()
  for (i in rev(seq_len(sys.nframe()))) {
    f <- sys.function(i)
    for (nm in names(fns)) {
      if (!identical(f, fns[[nm]])) next
      # an exported function apaTables itself called (apa.save() calls
      # apa.caption()) is not the one the user called
      p <- parents[i]
      if (p > 0 && identical(environment(sys.function(p)), ns)) next
      return(paste0(nm, "()"))
    }
  }
  "This function"
}

# A short description of what a non-numeric variable holds, for messages.
apa_describe_values <- function(x) {
  if (is.factor(x)) return("a factor")
  if (is.character(x)) return("text")
  if (is.logical(x)) return("TRUE/FALSE values")
  paste0("of class ", class(x)[1])
}

# The dv of a means table as numbers: numeric, TRUE/FALSE (whose mean is a
# proportion) or a time difference. Stops for anything else, or for a dv
# with no values.
apa_check_numeric_dv <- function(fn, dv, dv.name) {
  if (all(is.na(dv))) {
    stop(sprintf("%s() found no values in %s.", fn, dv.name), call. = FALSE)
  }
  if (!(is.numeric(dv) || is.logical(dv) || inherits(dv, "difftime"))) {
    stop(sprintf("%s() needs a numeric dv, but %s is %s.", fn, dv.name,
                 apa_describe_values(dv)), call. = FALSE)
  }
  as.numeric(dv)
}

# A grouping variable as a factor without the levels no row of the data has
# (for example, levels left over after subset()). A level that has rows but
# no dv values stays, so the group-size check names it.
apa_drop_empty_levels <- function(iv) {
  droplevels(as.factor(iv))
}

# Stop when a grouping variable has no values at all.
apa_check_has_groups <- function(fn, iv, iv.name) {
  if (nlevels(iv) == 0) {
    stop(sprintf("%s() found no values in %s.", fn, iv.name), call. = FALSE)
  }
  invisible(iv)
}

# Stop unless every group has at least two cases with a value on the dv: a
# standard deviation needs two. `groups` is a named list of factors, one per
# grouping variable; with two, the groups are the cells of a two-way table.
apa_check_group_sizes <- function(fn, groups, dv, dv.name) {
  # a two-way cell counts cases with values on both factors and the dv
  values_on <- if (length(groups) == 1L) {
    sprintf("a value on %s", dv.name)
  } else {
    sprintf("values on %s and %s", paste(names(groups), collapse = ", "), dv.name)
  }
  n <- table(lapply(groups, function(g) g[!is.na(dv)]))
  small <- which(n < 2, arr.ind = TRUE)
  if (length(small) == 0) return(invisible(NULL))
  small <- matrix(small, ncol = length(groups))
  labels <- apply(small, 1, function(idx) {
    paste(mapply(function(name, g, i) sprintf("%s = %s", name, levels(g)[i]),
                 names(groups), groups, idx), collapse = ", ")
  })
  counts <- n[small]
  what <- if (length(groups) == 1L) "group" else "cell"
  stop(sprintf("%s() needs at least two cases with %s in each %s, to compute a standard deviation. Too few in: %s.",
               fn, values_on, what,
               paste(sprintf("%s (%d %s)", labels, counts, ifelse(counts == 1, "case", "cases")),
                     collapse = "; ")),
       call. = FALSE)
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
  stop(sprintf("%s needs a table made by an apaTables function such as apa.cor.table() or apa.reg.table(), but it was given %s.",
               apa_user_function(), apa_describe_object(x)), call. = FALSE)
}

# Significance-star sentence as its own note entry: apa.save() starts each
# note entry on a new line in the Note paragraph, while apa.note.text()
# (apaquarto) joins entries with a space, so the chunk-option string is
# unchanged.
stars_spec_note <- function() {
  richtext(list(
    rt_run("* indicates "), rt_run("p", italic = TRUE),
    rt_run(" < .05. ** indicates "), rt_run("p", italic = TRUE),
    rt_run(" < .01.")))
}
