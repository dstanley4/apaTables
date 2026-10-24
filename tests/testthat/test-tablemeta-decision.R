# Run the tablemeta filter's precedence decisions through pandoc's
# own Lua interpreter. M.decide lives in apatablemeta-decide.lua, split out
# of apatablemeta.lua specifically so this dofiles the REAL function -- a
# test of a copy would not be a test. It is the only part of the filter a
# plain `pandoc lua` process can exercise -- FloatRefTarget, used by the rest
# of apatablemeta.lua, is a Quarto custom AST node plain pandoc cannot
# construct -- but M.decide IS the whole precedence matrix.

test_that("the tablemeta filter's precedence decisions are correct", {
  skip_if(Sys.which("pandoc") == "", "pandoc not available")
  lua <- system.file("_extensions/wjschne/apaquarto/apatablemeta-decide.lua",
                     package = "apaTables")
  skip_if(lua == "", "extension not installed")
  # The variable is set in the R process, not via system2(env=): on Windows
  # system2() puts env assignments on the command line, where pandoc reads
  # them as input filenames.
  withr::local_envvar(APA_TABLEMETA_LUA = lua)
  out <- suppressWarnings(system2(
    "pandoc", c("lua", test_path("test-tablemeta-decision.lua")),
    stdout = TRUE, stderr = TRUE))
  # system2() sets a status only on a non-zero exit. (No %||% here: base R
  # has it only from 4.4.0, and the package supports R 4.1.)
  status <- attr(out, "status")
  expect_equal(if (is.null(status)) 0L else status, 0L,
               info = paste(out, collapse = "\n"))
})
