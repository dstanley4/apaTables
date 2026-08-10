-- Companion to apatablelandscape.lua (Task 6): moves a rotated table's note
-- INSIDE its own landscape section.
--
-- MEASURED, twice over -- fix round 1 review of Task 7. apatablelandscape.lua
-- (post-quarto) replaces the `::: {.apa-table-landscape}` div with THREE
-- things, in this order, as direct children of the float's own Div (e.g.
-- `tbl-wide14`): a portrait closer, the table, a landscape closer. apanote.lua
-- (post-render, common) runs LATER and inserts the note as a SIBLING
-- immediately AFTER that whole float Div, one level UP -- not inside it.
-- Read off a real render (fixtures/landscape-shrunk.qmd, a `Blocks` dump at
-- post-render just after apanote.lua):
--
--   15.1 Div id=tbl-wide14         portrait-closer, Table, landscape-closer
--   15.2 Div (unclassed)           Div.FigureNote, Div.FigureNote
--
-- Under OOXML, a `sectPr` embedded in a paragraph governs everything back to
-- the PREVIOUS such paragraph. The note sits textually AFTER the landscape
-- closer, so it belongs to the section that follows -- the manuscript's own
-- portrait default -- not to the rotated section its table is on. MEASURED
-- in real Microsoft Word: landscape-numbered-lines put its table on page 3
-- (landscape) and its note ALONE on page 4 (portrait); landscape-shrunk put
-- its table on pages 3-4 (landscape) and its note on page 5 (portrait).
--
-- The fix moves the note INSIDE the float Div, between the table and the
-- landscape closer, and removes the now-empty sibling wrapper -- so the
-- closer keeps closing the SAME section, just with the note now inside it.
--
-- A NEW FILE rather than a change to apatablelandscape.lua itself (Task 6,
-- already reviewed and signed off): this only reads the note div apanote.lua
-- already built and repositions it; process_div/process_pandoc there are
-- untouched, and this file's own match is narrow -- it fires only when a
-- Div's LAST content block is literally the landscape-closer RawBlock this
-- extension itself writes, immediately followed by a sibling matching
-- apanote.lua's own note-wrapper shape. Every other float -- non-rotated,
-- rotated with no note, rotated with an explicit non-generated note -- has
-- no such trailing RawBlock and is untouched (verified by rendering all
-- four in one document; see the apaTables repo's task-7 report).
if FORMAT ~= "docx" then
  return
end

local function is_landscape_closer(b)
  return b.t == "RawBlock" and b.format == "openxml" and
    b.text:find('w:orient="landscape"', 1, true) ~= nil
end

local function is_note_wrapper(b)
  if not b or b.t ~= "Div" then return false end
  for _, c in ipairs(b.content) do
    if c.t == "Div" and c.classes:includes("FigureNote") then return true end
  end
  return false
end

local function relocate_note(blocks)
  local changed = false
  -- Iterate DOWNWARD, not `for i = 1, #blocks - 1`. Lua's numeric-for
  -- evaluates its limit ONCE, before the loop starts; `table.remove(blocks,
  -- i + 1)` below shrinks `blocks` by one element per relocation, so a
  -- SECOND relocation later in the same `Blocks` list would index past the
  -- new (shorter) end and error on nil -- a hard `quarto render` failure.
  -- Unreachable today (every float/note pair sits in its own `Blocks` list,
  -- verified by rendering multiple rotated+noted floats in one document),
  -- but it is a one-line fix: counting down means every removal happens at
  -- an index ABOVE the current `i`, which this loop has already passed and
  -- will never revisit, so the shrinking list never invalidates `i` itself.
  for i = #blocks - 1, 1, -1 do
    local b = blocks[i]
    if b.t == "Div" and #b.content > 0 and is_landscape_closer(b.content[#b.content])
        and is_note_wrapper(blocks[i + 1]) then
      local closer = table.remove(b.content)      -- pull the landscape closer out
      local note = table.remove(blocks, i + 1)     -- pull the note sibling out
      b.content:extend(note.content)               -- note now inside the section
      b.content:insert(closer)                     -- closer goes back on the end
      changed = true
    end
  end
  if changed then return blocks end
end

return { { Blocks = relocate_note } }
