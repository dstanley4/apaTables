-- apaTables emits an empty ::: {.apa-table-meta} div carrying the title and
-- note it generated. This filter puts them on the float ONLY where the author
-- supplied nothing, then removes the div. Precedence is therefore not coded:
-- a chunk-level tbl-cap means the float already has a caption, and a
-- chunk-level apa-note means the attribute already exists, so both are left
-- alone.
--
-- The decision logic (M.decide) lives in the sibling file
-- apatablemeta-decide.lua so it can be unit-tested with a plain `pandoc lua`
-- process: FloatRefTarget below is a Quarto custom AST node that plain
-- pandoc cannot construct, so that sibling file holds the only part of this
-- filter that is testable outside Quarto.
--
-- IMPORTANT: this script returns EXACTLY the plain, universal apaquarto
-- filter shape, `{ { FloatRefTarget = processfloat } }`, with no extra
-- keys -- matching every sibling filter (apafloatlatex.lua and the rest).
-- An earlier version instead attached `decide` as an extra key on the
-- returned table, so a bare `pandoc lua` test harness could reach it
-- directly without a second file. That was verified to work under bare
-- `pandoc --lua-filter=` and under `pandoc lua`, but NOT under Quarto's own
-- filter loader (main.lua): registered at post-quarto exactly where
-- apafloatlatex.lua expects it and rendered against a real
-- apa.quarto.table() document, `processfloat` was invoked zero times --
-- exit 0, PDF produced, caption and note both silently empty. Loading the
-- decision logic from a sibling file instead, the same way every other
-- apaquarto filter loads utilsapa.lua, avoids the problem entirely and was
-- confirmed end to end with a real Quarto render (see the apaTables repo's
-- task-5 report for this slice).
local scriptdir = PANDOC_SCRIPT_FILE:match("(.*[/\\])") or "./"
local M = dofile(scriptdir .. "apatablemeta-decide.lua")

local metaclass = "apa-table-meta"

local function processfloat(float)
  local title, note, found
  local stripped = pandoc.walk_block(pandoc.Div(float.content), {
    Div = function(d)
      if d.classes:includes(metaclass) then
        found = true
        title = d.attributes["apa-title"]
        note  = d.attributes["apa-note"]
        return {}
      end
    end
  })
  -- Nothing to do: no meta div at all (nothing to strip, nothing to set).
  if not found then return nil end
  float.content = stripped.content

  local setnote = M.decide(float.attributes["apa-note"], note)
  if setnote then float.attributes["apa-note"] = setnote end

  local existing = nil
  if float.caption_long and #float.caption_long.content > 0 then
    existing = pandoc.utils.stringify(float.caption_long)
  end
  local settitle = M.decide(existing, title)
  if settitle then
    local blocks = pandoc.read(settitle, "markdown").blocks
    if #blocks > 0 then
      float.caption_long = pandoc.Plain(blocks[1].content)
    end
  end
  return float
end

return { { FloatRefTarget = processfloat } }
