# Words APA sets lowercase in title case: short conjunctions, articles, and
# prepositions of three letters or fewer. Every entry is <= 3 letters, which is
# the same rule as "capitalize words of four letters or more" seen from the
# other side -- so "With", "From" and "Between" capitalize and are absent here.
APA_TITLE_LOWERCASE <- c(
  "and", "as", "but", "for", "if", "nor", "or", "so", "yet",
  "a", "an", "the",
  "at", "by", "in", "of", "off", "on", "per", "to", "up", "via"
)

#' APA title case
#'
#' Capitalization is only ever ADDED, never removed -- except for the one
#' deliberate exception below. The later branch that force-capitalizes a
#' word's first letter (`substr(w,1,1) <- toupper(...)`) can never itself
#' destroy an all-caps acronym: an acronym already starts uppercase, so that
#' branch is a no-op on it regardless of whether the guard below runs. What
#' the guard actually protects is different and easy to miss: an acronym
#' that COLLIDES, case-insensitively, with an entry in
#' `APA_TITLE_LOWERCASE` -- OR (odds ratio), AS (anxiety sensitivity), AN
#' (anorexia nervosa), SO (significant other), VIA (Values in Action) are
#' all real column names that are also minor words. Without the guard,
#' `tolower(w) %in% APA_TITLE_LOWERCASE` matches them and demotes them to
#' "or", "as", "an", "so", "via", silently destroying the acronym. See
#' `test-apa-title-case.R`, "an acronym that collides with a minor word
#' survives", which is the only test in the suite that can observe this
#' guard failing. Function words are still lowercased even when the author
#' capitalized them, because a strict never-downcase rule would leave
#' "Score For Test" and break the very rule being applied -- and a function
#' word is never an acronym, so that is safe.
#' @keywords internal
apa_title_case <- function(x) {
  words <- strsplit(x, " ", fixed = TRUE)[[1]]
  for (i in seq_along(words)) {
    w <- words[i]
    if (!nzchar(w)) next
    # Acronym guard: without this, an acronym that collides case-insensitively
    # with APA_TITLE_LOWERCASE (OR, AS, AN, SO, VIA, ...) would be demoted by
    # the branch below. It does NOT protect GPA/SES/IQ/PTSD -- those never
    # collide with the minor-word list, so the later toupper(substr(w,1,1))
    # branch already leaves them alone with or without this guard.
    if (grepl("^[A-Z]{2,}$", w)) next                       # acronym
    if (i > 1L && tolower(w) %in% APA_TITLE_LOWERCASE) {
      words[i] <- tolower(w)
      next
    }
    substr(w, 1L, 1L) <- toupper(substr(w, 1L, 1L))
    words[i] <- w
  }
  paste(words, collapse = " ")
}

#' A variable name as title text (D21)
#' @keywords internal
apa_name_to_title <- function(x) {
  x <- gsub("[._]+", " ", x)                    # explicit separators
  x <- gsub("([a-z0-9])([A-Z])", "\\1 \\2", x)  # camelCase
  x <- trimws(gsub("\\s+", " ", x))
  apa_title_case(x)
}
