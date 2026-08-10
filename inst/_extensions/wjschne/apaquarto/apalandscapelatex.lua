if FORMAT ~= "latex" then
  return
end

-- Landscape (rotated) floats in latex.
--
-- APA prescribes turning the PAGE for a wide table -- "use landscape
-- orientation on the page with the wide table ... it does not matter if the
-- page header also moves" -- which is what pdflscape's landscape environment
-- does, and what Quarto's own ::: {.landscape} div emits. pdflscape is
-- therefore the SINGLE rotation mechanism here. An earlier version of this
-- filter routed delayed floats through the rotating package's sidewaystable
-- instead, which turns the table on an upright page; that is apa7's LaTeX
-- convention, not APA's instruction, and it is gone.
--
-- What the two placements need is different, and neither is what the div
-- gives on its own:
--
--   floats in the text   Quarto's div already emits \begin{landscape} around
--                        whatever it contains, and it works. Left alone.
--
--   floats at the end    apa7 loads endfloat, which re-typesets each float on
--                        its own page at the end of the manuscript and drops
--                        whatever surrounded it in the body. The div's
--                        landscape environment is left behind holding nothing
--                        -- two \clearpage's in the middle of the prose -- and
--                        the table lands upright. So the div is unwrapped and
--                        each float inside it is marked apa-landscape="true";
--                        apafloatlatex.lua then wraps THAT FLOAT in its own
--                        literal \begin{landscape} block, which the template's
--                        \DeclareDelayedFloatFlavor{landscape}{table} defers
--                        whole, rotation included.
--
-- ONE WRAPPER PER FLOAT, not one around the div. endfloat gives each deferred
-- block its own page, so wrapping the div as a unit would put every table in
-- it on a single landscape page; wrapping each float separately keeps APA's
-- one-table-per-page layout.

local utilsapa = require("utilsapa")

local delayedfloats = true

-- The rule itself lives in utilsapa so that apafloatlatex.lua, which needs the
-- same answer for the longtable reservation, cannot come to a different one.
local getmeta = function(meta)
  delayedfloats = utilsapa.delayedfloats(meta)
end

-- Mark every float in this div, setting `attr` to "true".
local markfloats = function(div, attr)
  return div.content:walk {
    Div = function(d)
      if d.identifier and (d.identifier:find("^tbl%-") or d.identifier:find("^fig%-")) then
        d.attributes[attr] = "true"
        return d
      end
    end,
    Table = function(tbl)
      -- Plain markdown tables: at this stage the {#tbl-...} block is still
      -- literal text in the caption, so the identifier is not set yet.
      -- Every table inside a landscape div wants rotation, so mark it.
      tbl.attributes[attr] = "true"
      return tbl
    end,
    Figure = function(fg)
      if fg.identifier and fg.identifier:find("^fig%-") then
        fg.attributes[attr] = "true"
        return fg
      end
    end
  }
end

local marklandscape = function(div)
  if not div.classes:includes("landscape") then
    return nil
  end
  if not delayedfloats then
    -- Floats stay in text: Quarto's native landscape div works, and the div
    -- IS the wrapper. Say so, so that a float which also carries
    -- apa-landscape="true" -- written as a chunk option, or set by a table
    -- builder that has decided the table is too wide to stand upright -- does
    -- not get a second \begin{landscape} nested inside this one.
    div.content = markfloats(div, "apa-landscape-wrapped")
    return div
  end
  div.content = markfloats(div, "apa-landscape")
  -- Unwrap: the floats are delayed to the end of the manuscript, each now
  -- carrying its own landscape wrapper, so a landscape environment here would
  -- be empty and would only break the surrounding text across pages
  -- (\clearpage at both ends).
  return div.content
end

return {
  { Meta = getmeta },
  { Div = marklandscape }
}
