-- Phase 1.14. A rotated table's CAPTION belonged to the portrait section while
-- its table belonged to the landscape one, so the caption printed alone on a
-- portrait page and the table on the page after it. And between two rotated
-- tables the document returned to portrait for no reason, which opened a BLANK
-- page. Both seen in real Microsoft Word, not inferred: the apaTables repo's
-- dev/factcheck/phase114-caption/VERIFICATION.md.
--
-- A `sectPr` embedded in a paragraph governs everything back to the PREVIOUS
-- such paragraph. apatablelandscape.lua writes the portrait closer immediately
-- before the table, which puts the caption -- emitted earlier -- on the wrong
-- side of it. Moving the closer ABOVE the caption puts both in one section,
-- with no change to what either closer says.
--
-- WHY NOT IN apatablelandscape.lua: that filter runs at post-quarto, and
-- MEASURED there (c1-one-rotated-dump-postquarto.txt) the caption is not a
-- caption yet -- an unclassed Div holding bare text, four levels away from the
-- closers, with no FigureTitle and no Caption class. It is only flat, classed
-- and adjacent at post-render.
--
-- WHY AFTER apatablelandscapenote.lua. That filter finds a rotated float by
-- testing that its LAST content block is the landscape closer, which
-- merge_runs() below removes from every float in a run but the last -- so
-- running first stops it recognising those floats.
--
-- AN EARLIER VERSION OF THIS COMMENT CALLED THAT ORDER LOAD-BEARING and said
-- reversing it strands notes on a portrait page. THAT IS FALSE. It was caught
-- by mutating the registration order and finding the whole suite still green.
-- Rendered both ways, the two documents differ in exactly one respect: the
-- position of a zero-width <w:bookmarkEnd/> relative to a note paragraph.
-- Nothing a reader can see.
--
-- The reason is worth keeping, because it is not obvious. A note the note
-- filter fails to relocate stays a SIBLING immediately after its float -- and
-- merging is precisely what DELETED the closer that used to separate them, so
-- the note is already inside the landscape section. Removing a boundary
-- cannot strand anything past it.
--
-- The order is kept: it is the one verified end to end, and a future change
-- that stops removing closers would revive the dependency. A real
-- interaction, not a critical one.
if FORMAT ~= "docx" then
  return
end

local function is_portrait_closer(b)
  return b and b.t == "RawBlock" and b.format == "openxml"
    and b.text:find("<w:sectPr", 1, true) ~= nil
    and b.text:find('w:orient="landscape"', 1, true) == nil
end

-- Move the portrait closer above the caption paragraphs. Narrow by
-- construction: only a Div that already CONTAINS this extension's own portrait
-- closer is touched, so an upright float -- which has no closers at all --
-- can never match.
local function hoist(div)
  for i, c in ipairs(div.content) do
    if is_portrait_closer(c) and i > 1 then
      local closer = table.remove(div.content, i)
      div.content:insert(1, closer)
      return div
    end
  end
  return nil
end

local function is_landscape_closer(b)
  return b and b.t == "RawBlock" and b.format == "openxml"
    and b.text:find('w:orient="landscape"', 1, true) ~= nil
end

local function is_page_break(b)
  return b and b.t == "RawBlock" and b.format == "openxml"
    and b.text:find('w:type="page"', 1, true) ~= nil
end

-- The Div that DIRECTLY holds the closers. It is nested inside the top-level
-- float wrapper: measured, Div[cell,FigureWithoutNote] > Div#tbl-rot.
local function holder(b)
  if b.t ~= "Div" then return nil end
  for _, c in ipairs(b.content) do
    if is_portrait_closer(c) then return b end
  end
  for _, c in ipairs(b.content) do
    local h = holder(c)
    if h then return h end
  end
  return nil
end

-- Collapse each RUN of consecutive rotated floats into ONE landscape section:
-- drop every float's landscape closer but the last, and every float's portrait
-- closer but the first. The page break between floats stays and becomes an
-- ordinary page break INSIDE the landscape section.
--
-- Without this, the document returns to portrait between two landscape tables
-- for no reason, and that empty portrait stretch IS a blank page in Word --
-- seen in the CONTROL renders, so it predates this filter.
--
-- HOLDERS ARE RESOLVED BEFORE ANY MUTATION, and that is load-bearing --
-- measured twice, by two different failures.
--
-- Replace `holders[i], holders[j]` with `holder(doc.blocks[i]),
-- holder(doc.blocks[j])` and the merge STOPS TAKING EFFECT: a document with
-- two consecutive rotated tables comes back completely unmerged, two landscape
-- closers and five sections. An earlier prototype that resolved lazily in a
-- slightly different way merged a run of THREE only partially -- two landscape
-- sections instead of one.
--
-- The working explanation is pandoc's block-wrapper identity: a Div fetched
-- fresh out of doc.blocks is not guaranteed to be the object whose mutations
-- are written back, so edits applied to a re-fetched wrapper can be silently
-- discarded. Stated as the explanation that fits both measurements, NOT as
-- something established from pandoc's internals -- what is established is that
-- resolving up front works and resolving lazily does not.
local function merge_runs(doc)
  local floats, holders = {}, {}
  for i, b in ipairs(doc.blocks) do
    local h = holder(b)
    if h then
      table.insert(floats, i)
      holders[i] = h
    end
  end
  for k = 1, #floats - 1 do
    local i, j = floats[k], floats[k + 1]
    -- "Consecutive" means nothing between them but page breaks. An upright
    -- float ends the run.
    local adjacent = true
    for m = i + 1, j - 1 do
      if not is_page_break(doc.blocks[m]) then adjacent = false end
    end
    if adjacent then
      local a1, a2 = holders[i], holders[j]
      if a1 and a2 then
        if is_landscape_closer(a1.content[#a1.content]) then
          table.remove(a1.content)            -- stay landscape
        end
        if is_portrait_closer(a2.content[1]) then
          table.remove(a2.content, 1)         -- do not re-open portrait
        end
      end
    end
  end
end

-- Does this block END with the landscape closer -- i.e. is it the last float of
-- a rotated run? The closer is the last content of the float's own Div, which
-- is in turn the last content of the wrapper Quarto puts around it.
local function ends_landscape(b)
  if b.t ~= "Div" or #b.content == 0 then return false end
  local last = b.content[#b.content]
  if is_landscape_closer(last) then return true end
  return ends_landscape(last)
end

-- Drop the page break that apafloatstoend.lua emits between a rotated run and
-- whatever float follows it. The landscape closer ALREADY starts a new page --
-- that is what a section break does -- so the break becomes the first content
-- of the new portrait section and opens a page of its own: a blank one,
-- carrying the running head and nothing else.
--
-- SEEN IN REAL WORD and PRE-EXISTING: page 9 of a three-rotated-tables
-- manuscript, page 5 of one with an upright table between two rotated ones.
-- Identical in the CONTROL renders, so this filter did not cause it; the same
-- family as the blank page D98 folded in, at the other boundary.
--
-- NOT the same as the break BEFORE a rotated float, which is harmless: there
-- the break lands at the END of a section and Word collapses it against the
-- section break's own page (measured, VERIFICATION.md 4.1). Position decides
-- it, so do not "simplify" the two cases together.
--
-- A break BETWEEN two floats of the same run is kept: merging removed the
-- closer that would have preceded it, so it is an ordinary page break inside
-- the landscape section, which is exactly what puts each table on its own page.
local function drop_break_after_run(doc)
  for i = #doc.blocks - 1, 1, -1 do
    if ends_landscape(doc.blocks[i]) and is_page_break(doc.blocks[i + 1]) then
      table.remove(doc.blocks, i + 1)
    end
  end
end

return {
  {
    Pandoc = function(doc)
      -- hoist FIRST: merge_runs reads the hoisted layout, in which the
      -- portrait closer is content[1] of the float's holding Div. And
      -- drop_break_after_run LAST: it asks which float ends a run, which is
      -- only settled once merge_runs has removed the interior closers.
      doc.blocks = doc.blocks:walk({ Div = hoist })
      merge_runs(doc)
      drop_break_after_run(doc)
      return doc
    end
  }
}
