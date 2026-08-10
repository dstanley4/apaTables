-- Task 5: the tablemeta filter's precedence decisions, exercised as a pure
-- function. `FloatRefTarget` is a Quarto custom AST node that plain
-- `pandoc lua` cannot construct, so this only exercises M.decide -- but
-- M.decide IS the whole precedence matrix; the rest of the filter just
-- reads/writes attributes around it.
local M = dofile(os.getenv("APA_TABLEMETA_LUA"))
local fails = 0
local function eq(got, want, label)
  if got ~= want then
    print("FAIL " .. label .. ": got " .. tostring(got) .. " want " .. tostring(want))
    fails = fails + 1
  end
end

-- the 3 x 3 precedence matrix, as decisions
eq(M.decide(nil,  "gen"), "gen", "absent + candidate -> set")
eq(M.decide("",   "gen"), "gen", "empty  + candidate -> set")
eq(M.decide("mine","gen"), nil,  "existing wins")
eq(M.decide("mine", nil),  nil,  "existing, no candidate")
eq(M.decide(nil,   nil),   nil,  "nothing to do")
eq(M.decide("",    nil),   nil,  "empty, no candidate")

if fails > 0 then os.exit(1) end
print("ok")
