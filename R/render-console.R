#' Print an apa_table_spec to the console
#'
#' Renders a render-agnostic table spec as plain text: "Table N" (when a number
#' is set), the flattened title, a fixed-width body (multi-line cells stacked on
#' continuation rows), then notes prefixed with "Note. ".
#'
#' @param x an `apa_table_spec` object.
#' @param ... unused; present for S3 method consistency.
#' @return `x` invisibly.
#' @keywords internal
#' @examples
#' spec <- apa.cor.table(attitude)$spec
#' print(spec)
#' @export
print.apa_table_spec <- function(x, ...) {
  cat("\n")
  if (!is.na(x$number)) cat(sprintf("Table %d\n\n", x$number))
  if (!is.null(x$title)) cat(as_plaintext(x$title), "\n\n", sep = "")

  labels <- vapply(x$col_labels, as_plaintext, character(1))
  # flatten each cell; multi-line cells keep their embedded "\n"
  cells <- lapply(x$body, function(row) vapply(row, as_plaintext, character(1)))
  mat <- do.call(rbind, c(list(labels), cells))
  # column widths from the widest single line in any cell
  width_of <- function(s) {
    parts <- strsplit(s, "\n", fixed = TRUE)[[1]]
    if (!length(parts)) return(0L)  # blank cell (e.g. correlation lower triangle)
    max(nchar(parts))
  }
  widths <- apply(mat, 2, function(col) max(vapply(col, width_of, integer(1))))

  pad <- function(s, w, align) {
    formatC(s, width = w, flag = if (align == "l") "-" else "")
  }
  print_row <- function(vals) {
    # expand multi-line cells into stacked physical rows
    split_vals <- lapply(vals, function(v) strsplit(v, "\n", fixed = TRUE)[[1]])
    n_lines <- max(vapply(split_vals, length, integer(1)))
    for (li in seq_len(n_lines)) {
      pieces <- vapply(seq_along(split_vals), function(j) {
        s <- if (li <= length(split_vals[[j]])) split_vals[[j]][li] else ""
        pad(s, widths[j], x$col_align[j])
      }, character(1))
      cat(paste(pieces, collapse = "  "), "\n", sep = "")
    }
  }
  print_row(labels)
  for (r in seq_along(cells)) print_row(cells[[r]])
  cat("\n")
  for (note in x$notes) cat("Note. ", as_plaintext(note), "\n", sep = "")
  invisible(x)
}
