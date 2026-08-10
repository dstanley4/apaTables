# Task 5: run the tablemeta filter's precedence decisions through pandoc's
# own Lua interpreter. M.decide lives in apatablemeta-decide.lua, split out
# of apatablemeta.lua specifically so this dofiles the REAL function -- a
# test of a copy would not be a test. It is the only part of the filter a
# plain `pandoc lua` process can exercise -- FloatRefTarget, used by the rest
# of apatablemeta.lua, is a Quarto custom AST node plain pandoc cannot
# construct -- but M.decide IS the whole precedence matrix (D22).

test_that("the tablemeta filter's precedence decisions are correct", {
  skip_if(Sys.which("pandoc") == "", "pandoc not available")
  lua <- system.file("_extensions/wjschne/apaquarto/apatablemeta-decide.lua",
                     package = "apaTables")
  skip_if(lua == "", "extension not installed")
  out <- suppressWarnings(system2(
    "pandoc", c("lua", test_path("test-tablemeta-decision.lua")),
    env = paste0("APA_TABLEMETA_LUA=", lua), stdout = TRUE, stderr = TRUE))
  expect_equal(attr(out, "status") %||% 0L, 0L, info = paste(out, collapse = "\n"))
})
