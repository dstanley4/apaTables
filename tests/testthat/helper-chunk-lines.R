# The R lines of a template chunk: everything that is not a fence (```) and
# not a knitr option line (#| ...).
#
# The chunk is TWO lines, one of which may be a COMMENT.
# Assertions therefore have to distinguish "this text is somewhere in the
# block" from "this is the build line, and it is/is not commented out" -- a
# substring match on the whole block cannot, and would be satisfied by a
# mention in the surrounding prose.
apa_chunk_code_lines <- function(txt) {
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  lines <- lines[!grepl("^```", lines)]
  lines <- lines[!grepl("^#\\|", lines)]
  lines[nzchar(trimws(lines))]
}
