local M = {}

function M.make_note(s, prefix)
  s = string.gsub(s, '^%[%"', "")
  s = string.gsub(s, '%"%]$', "")

  local includeprefix = true
  local apanotedivs = pandoc.Div(pandoc.Blocks {})
  local cnt = 0

  for v in string.gmatch(s .. '","', '(.-)%",%"') do
    if string.find(v, '^NoNote ') then
      includeprefix = false
      v = string.gsub(v, '^NoNote ', "")
    end
    local apanote = pandoc.Div({})
    apanote.attributes['custom-style'] = 'FigureNote'
    apanote.classes:extend({ "FigureNote" })
    apanote.classes:extend({ "NoIndent" })
    cnt = cnt + 1
    if (cnt == 1 and includeprefix) then
      apanote.content:extend(prefix.content:extend(quarto.utils.string_to_inlines(v:gsub(" ", "\u{00A0}", 1))))
    else
      apanote.content:extend(quarto.utils.string_to_inlines(v))
    end
    apanotedivs.content:extend({ apanote })
  end
  return (apanotedivs)
end

-- make string, if it exists, else return default
function M.stringify(s, default)
  if s then
    s = pandoc.utils.stringify(s)
  else
    if default then
      s = default
    else
      s = ""
    end
  end
  return s
end

-- Are floats collected at the end of the manuscript, or do they stay where
-- they were written?
--
-- apa7 loads endfloat in man and stu modes, which moves every float to its own
-- page at the end; `floatsintext: true` turns that off, and doc and jou modes
-- never had it. Two filters need this answer -- apalandscapelatex.lua, to know
-- whether Quarto's native landscape div will survive, and apafloatlatex.lua,
-- to know whether the reservation in front of a longtable would reach the page or
-- be stripped -- so the rule lives here ONCE. Two copies of it could drift and
-- then disagree about which placement is active, which is a defect no rendered
-- page would announce.
function M.delayedfloats(meta)
  local documentmode = "man"
  if meta.documentmode then
    documentmode = pandoc.utils.stringify(meta.documentmode)
  end
  local delayed = documentmode == "man" or documentmode == "stu"
  if meta.floatsintext and pandoc.utils.stringify(meta.floatsintext) == "true" then
    delayed = false
  end
  return delayed
end

-- if any value in table
function M.containsValue(tbl, value)
  for _, v in pairs(tbl) do
    if v == value then
      return true
    end
  end
  return false
end

return M
