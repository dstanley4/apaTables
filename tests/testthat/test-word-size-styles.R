# The sizes Word can produce are recorded in THREE places that must agree:
#   1. R          -- apa_quarto_size_rung()'s reachable return values
#   2. Lua        -- apatablesize.lua's `valid_sizes`
#   3. the binary -- the Table* styles actually in the shipped apaquarto.docx
#
# Nothing checked the agreement before this file. A mismatch does not degrade
# gracefully: Word's response to an UNKNOWN table style is to drop the style
# assignment ENTIRELY, so the table loses its APA rules and cell margins along
# with the requested size -- silently, in the user's document.
#
# Reads the SHIPPED extension via system.file(), not the source tree, so it
# certifies what a user actually installs.

apa_shipped_table_styles <- function() {
  docx <- system.file("_extensions", "wjschne", "apaquarto", "apaquarto.docx",
                      package = "apaTables")
  if (!nzchar(docx) || !file.exists(docx)) return(character(0))
  con <- unz(docx, "word/styles.xml", open = "rb")
  on.exit(close(con), add = TRUE)
  # Read to EOF rather than a fixed ceiling. styles.xml is ~60 KB today, so
  # the old n = 2e6 had 33x headroom and no live risk -- but a truncating
  # read fails SILENTLY, and what it would truncate is the style list this
  # whole file exists to certify.
  raw <- raw(0)
  repeat {
    chunk <- readBin(con, "raw", n = 1e6)
    if (!length(chunk)) break
    raw <- c(raw, chunk)
  }
  xml <- rawToChar(raw)
  ids <- regmatches(xml, gregexpr('w:styleId="Table[0-9]*"', xml))[[1]]
  sort(unique(gsub('.*"(Table[0-9]*)".*', "\\1", ids)))
}

apa_lua_valid_sizes <- function() {
  lua <- system.file("_extensions", "wjschne", "apaquarto",
                     "apatablesize.lua", package = "apaTables")
  txt <- paste(readLines(lua, warn = FALSE), collapse = "\n")
  line <- regmatches(txt, regexpr("local valid_sizes = \\{[^}]*\\}", txt))
  # NOTE (found by running this test before it could possibly pass): the
  # obvious regex `regmatches(line, gregexpr('"[0-9]+"', line))` captures the
  # surrounding quote characters as part of each match (`"6"`, not `6`), so
  # comparing that against `from_binary`'s bare digit strings ("6") would
  # fail EVERY TIME, agreement or not -- the quotes alone always differ. The
  # quotes have to be stripped after extraction for this to test what it
  # claims to test.
  sizes <- regmatches(line, gregexpr('"[0-9]+"', line))[[1]]
  sort(unique(gsub('"', "", sizes)))
}

test_that("the shipped reference document carries exactly the clone styles the ladder names", {
  styles <- apa_shipped_table_styles()
  skip_if(length(styles) == 0, "apaquarto.docx not in the installed package")

  # "Table" itself is the unmodified default and already carries 10 pt,
  # which is why there is no Table10 and must never be one.
  expect_true("Table" %in% styles)
  expect_false("Table10" %in% styles)
  expect_setequal(setdiff(styles, "Table"),
                  c("Table6", "Table7", "Table8", "Table9", "Table11", "Table12"))
})

test_that("the Lua filter's valid_sizes matches the clone styles in the binary", {
  styles <- apa_shipped_table_styles()
  skip_if(length(styles) == 0, "apaquarto.docx not in the installed package")
  from_binary <- sort(gsub("^Table", "", setdiff(styles, "Table")))
  expect_setequal(apa_lua_valid_sizes(), from_binary)
})

# The clone-style sizes the shipped binary actually carries, as integers.
# "Table" itself is the unmodified default (10 pt) and names no rung --
# apa_quarto_size_rung() returns NA for 10, meaning "write NO marker and let
# the reference document's own size stand", so it is correctly absent here.
apa_binary_rungs <- function() {
  styles <- apa_shipped_table_styles()
  n <- sub("^Table", "", styles)
  sort(as.integer(n[nzchar(n)]))
}

test_that("every size R can reach is a style the shipped binary actually has", {
  rungs <- apa_binary_rungs()
  skip_if(length(rungs) == 0, "apaquarto.docx not in the installed package")

  rung <- apaTables:::apa_quarto_size_rung
  reachable <- sort(unique(stats::na.omit(
    vapply(seq(1, 40, by = 0.25), function(s) rung(s), integer(1)))))

  # setequal, both directions: a rung R can name that the binary lacks makes
  # Word drop the style assignment ENTIRELY -- losing the APA rules and cell
  # margins along with the size, silently, in the user's document. A style in
  # the binary R can never name is dead weight in a shipped file.
  expect_setequal(as.integer(reachable), rungs)

  # `reachable` is the same sweep test-font-size-contract.R:39 pins to a
  # literal (c(6L, 7L, 8L, 9L, 11L, 12L)), so the assertion above -- checking
  # `reachable` against the binary's own rungs -- certifies that literal
  # transitively; no second copy of the set needs writing down here. The
  # binary's own side is already pinned by name in this file's first test
  # ("the shipped reference document carries exactly the clone styles the
  # ladder names", around :59, `expect_setequal(setdiff(styles, "Table"),
  # c("Table6", ...))`). A redundant `expect_setequal(rungs, c(6L, ...))`
  # here could only fail in a world where that one already had.
})
