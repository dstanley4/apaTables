# The single list of awkward variable names, shared by every path that has to
# survive one. It lives in a helper rather than in one test file because three
# files now use it -- the spec/.docx sweep
# (test-special-characters-in-names.R), the LaTeX emitter sweep
# (test-render-latex.R) and the end-to-end render (test-render-acceptance.R) --
# and a second copy would drift the moment one of them gained a character.
#
# WHAT A COLUMN NAME CAN ACTUALLY HOLD, MEASURED -- this is what bounds the
# list, rather than taste:
#
#                          default data.frame()   check.names = FALSE   tibble()
#   _                      survives               survives              survives
#   # $ % & ~ ^ \ { }      sanitised to "."       ALL SURVIVE           ALL SURVIVE
#   newline, tab           sanitised              SURVIVE               SURVIVE
#   < > | "                sanitised              survive               survive
#
# check.names = FALSE is what read.csv(check.names = FALSE), haven and readxl
# produce, so EVERY LaTeX special character is reachable in a real variable
# name, and so are newline and tab. The underscore is merely the only one that
# survives a DEFAULT data.frame() -- which is very likely why it, and not one
# of the others, is the one that broke a previous version of apaTables: it is
# the one you hit without trying.
#
# RANKED BY FAILURE MODE, because the loud ones are the safe ones:
#
#   _ and ^        abort the LaTeX render outright ("! Missing $ inserted.")
#   \ { }          undefined control sequence / brace mismatch -- usually loud
#   %              COMMENTS OUT the rest of the line
#   &              SHIFTS every following cell into the wrong column
#
# The last two can complete with exit status 0 and no warning at all, so for
# those a successful render is not evidence: the name itself has to be found in
# the rendered PDF's text. test-render-acceptance.R is where that is done.
#
# A name containing a NEWLINE lands in a first-column cell, which is where the
# emitter's blank-line guard lives (apa_latex_collapse_newlines): a single
# newline is harmless, two consecutive ones make a blank line and a blank line
# inside a LaTeX cell aborts the render. Both are covered.
tricky_names <- c(
  underscore = "v_one", period     = "v.one",     space      = "v one",
  hyphen     = "v-one", percent    = "pct%",      parens     = "score (t1)",
  ampersand  = "a&b",   hash       = "n#1",       dollar     = "cost$",
  colon      = "a:b",   slash      = "a/b",       apostrophe = "it's",
  unicode    = "na\u00efvet\u00e9",
  tilde      = "a~b",   caret      = "a^b",       backslash  = "a\\b",
  braces     = "a{b}c", newline    = "line\nbreak",
  newline2   = "two\n\nbreaks",                   tab        = "col\tsep"
)

# The EXACT LaTeX each name must come out as, written by hand.
#
# Deliberately NOT computed with apa_latex_escape(): a test whose expectation
# comes from the function under test passes when that function is stubbed to
# return its input unchanged, which is precisely the mutation the escaping
# sweep exists to catch (watched: with the "_" entry removed from
# apa_latex_escape_map, a computed expectation stays green and this literal one
# goes red).
#
# "line break" and "two  breaks" are the emitter's newline collapse, not the
# escaper's: apa_latex_escape() passes LF through untouched and
# apa_latex_collapse_newlines() turns every LF into a space one layer later.
# The tab is unchanged by both -- TeX's own catcode table already treats it as
# whitespace.
tricky_latex <- c(
  underscore = "v\\_one", period    = "v.one",      space     = "v one",
  hyphen     = "v-one",   percent   = "pct\\%",     parens    = "score (t1)",
  ampersand  = "a\\&b",   hash      = "n\\#1",      dollar    = "cost\\$",
  colon      = "a:b",     slash     = "a/b",        apostrophe = "it's",
  unicode    = "na\u00efvet\u00e9",
  tilde      = "a\\textasciitilde{}b",
  caret      = "a\\textasciicircum{}b",
  backslash  = "a\\textbackslash{}b",
  braces     = "a\\{b\\}c",
  newline    = "line break",
  newline2   = "two  breaks",
  tab        = "col\tsep"
)

# The four names R's OWN formula machinery cannot carry into a coefficient
# label, so no apa.reg.table() built from a formula can be asked to display
# them. This is a fact about R, not about apaTables, and it is pinned by a test
# in test-special-characters-in-names.R rather than merely asserted here:
# deparse() backquotes a non-syntactic name and escapes the character on the
# way in, so lm()'s coefficient is named `a\\b`, `line\nbreak`, ... with the
# BACKSLASH-N as two characters. The same names go through apa.cor.table()
# unharmed, and through the LaTeX emitter and a real render unharmed, which is
# where they are covered.
tricky_names_no_formula <- c("backslash", "newline", "newline2", "tab")

# A two-variable numeric data frame whose FIRST column carries the tricky name.
# names<-() does not sanitise, so this is the check.names = FALSE case above.
tricky_cor_df <- function(nm) {
  set.seed(1)
  d <- data.frame(a = stats::rnorm(40), y = stats::rnorm(40))
  names(d)[1] <- nm
  d
}
