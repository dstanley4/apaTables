-- The precedence matrix for apatablemeta.lua's title/note defaults, split
-- into its own file so a plain `pandoc lua` process can dofile the REAL
-- function directly. FloatRefTarget (used by the rest of the filter, in
-- apatablemeta.lua) is a Quarto custom AST node that plain pandoc cannot
-- construct, so this file holds the only part of that filter testable
-- outside Quarto -- but it IS the whole precedence matrix.
local M = {}

-- Return the value to set, or nil to leave what is there.
function M.decide(existing, candidate)
  if candidate == nil or candidate == "" then return nil end
  if existing ~= nil and existing ~= "" then return nil end
  return candidate
end

return M
