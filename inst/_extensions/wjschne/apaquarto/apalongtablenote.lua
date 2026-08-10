-- Attach a long table's apa-note to the table itself.
--
-- apafloatlatex.lua marks the note of an apa-longtable and leaves it in the
-- AST as a sibling of the table. A sibling block is a separate paragraph as
-- far as LaTeX is concerned: under apaquarto's default placement endfloat
-- defers the longtable to the end of the manuscript and the note, not being a
-- float, stays behind beside the body text it was written next to (measured:
-- 0 of 3 notes stayed with their tables). This filter moves the note into the
-- table's own \endlastfoot, so LaTeX carries it wherever the table goes and
-- prints it once, under the final page of a table that breaks.
--
-- It runs at post-render, AFTER citeprocr.lua, on purpose. The note has to be
-- written out as LaTeX to go inside the raw block, and doing that any earlier
-- freezes [@key] literally and -- worse -- hides the citation from citeproc,
-- so a work cited only in a table note disappears from the reference list.
-- APA 7 section 7.7 requires a copyright attribution note ("Adapted from ...")
-- on any reproduced table, so that is the single most common citation site in
-- an APA table.
if FORMAT ~= "latex" then
  return
end

local notemarker = "apa-longtable-note"
local siblingattr = "apa-longtable-note-sibling-space"

-- Read a longtable's own preamble: how many columns it has, how it is
-- horizontally aligned, and how wide a note spanning it may be.
--
-- Everything is derived, nothing assumed. The note is emitted as a
-- \multicolumn spanning every column, so a wrong count is a malformed row.
-- The parse is deliberately strict: after the constructs below are consumed,
-- anything left over means a preamble this code does not understand (a
-- *{3}{c} repetition, a dcolumn D{.}{.}{2}, an S[table-format=..] option) and
-- it gives up rather than emit a row with the wrong number of cells. An
-- OVERcount is a loud LaTeX error, but an UNDERcount is a short row that
-- LaTeX accepts silently, which is the worse failure of the two.
--
-- Returns ncol, width, align. width is a LaTeX dimen expression for the
-- table's real width -- the declared column widths plus the \tabcolsep either
-- side of each column and the \arrayrulewidth of each | rule -- when every
-- column declares one (p/m/b, which is what flextable emits) and there is no
-- @/! insert of unknowable width; otherwise nil, meaning "use the text
-- block". align is "l", "c", "r" or "" (unknown). Returns nil when there is
-- no longtable preamble to read.
local function apalongtablespec(tex)
  local opt, spec = tex:match("\\begin{longtable}(%b[])%s*(%b{})")
  if not spec then
    opt = ""
    spec = tex:match("\\begin{longtable}%s*(%b{})")
  end
  if not spec then return nil end
  local align = opt:gsub("[%[%]%s]", "")
  if align ~= "l" and align ~= "c" and align ~= "r" then align = "" end
  local inner = spec:sub(2, -2)
  -- @{..} and !{..} put material of unknown width between columns; >{..} and
  -- <{..} only inject declarations and cost nothing.
  local hasinsert = inner:find("@%b{}") ~= nil or inner:find("!%b{}") ~= nil
  inner = inner:gsub("@%b{}", ""):gsub("!%b{}", "")
      :gsub(">%b{}", ""):gsub("<%b{}", "")
  local bars = select(2, inner:gsub("|", ""))
  local n = 0
  local widths = {}
  local rest = inner:gsub("[pmb]%b{}", function(col)
    n = n + 1
    widths[#widths + 1] = col:match("%b{}"):sub(2, -2)
    return ""
  end)
  local sized = #widths > 0
  for _ in rest:gmatch("[lcrXS]") do
    n = n + 1
    sized = false
  end
  if rest:gsub("[lcrXS|%s]", "") ~= "" then return nil end
  if n == 0 then return nil end
  local width = nil
  if sized and not hasinsert then
    -- Declared widths, plus the \tabcolsep between each pair of columns and
    -- the \arrayrulewidth of each | rule. The \tabcolsep OUTSIDE the first
    -- and last column is deliberately left out: measured on the apaTables
    -- probe the full table box is 421.28pt while the rules LaTeX actually
    -- draws span 413.76pt, so counting it would put the note past the visible
    -- edge of its own table. This lands at 409.28pt -- 4.5pt inside the rule,
    -- which is the right side of the APA 7 section 7.14 ceiling to be on.
    width = "\\dimexpr " .. table.concat(widths, "+")
    if n > 1 then
      width = width .. "+" .. (2 * n - 2) .. "\\tabcolsep"
    end
    if bars > 0 then
      width = width .. "+" .. bars .. "\\arrayrulewidth"
    end
    width = width .. "\\relax"
  end
  return n, width, align
end

-- Where a note row may legally be spliced into a longtable's head/foot
-- declarations.
--
-- longtable requires \endfirsthead, \endhead, \endfoot and \endlastfoot in
-- that order, all of them before the first body row, so the note row has to
-- go after the last declaration that is already there. Note that "already
-- there" has to be decided across the WHOLE table: by the time this filter
-- runs Quarto has split the longtable into several raw blocks (preamble,
-- body, \end{longtable}) with the caption sitting between them as ordinary
-- AST, so a per-block decision could see \endhead in one block and miss an
-- \endlastfoot in the next and then declare a second, illegal one.
--
-- \endfirsthead contains neither \endhead nor \endfoot as a substring, so the
-- plain (non-pattern) finds below cannot confuse them.
local anchorkinds = { "\\endlastfoot", "\\endfoot", "\\endhead", "\\endfirsthead" }

-- Which anchor to use, given every latex raw block of the table.
local function apalongtablekind(texts)
  for _, kind in ipairs(anchorkinds) do
    for _, t in ipairs(texts) do
      if t:find(kind, 1, true) then return kind end
    end
  end
  return nil
end

-- Splice the note row into the one raw block that carries the chosen anchor.
-- Returns the rewritten text, or nil if this block is not that one.
local function apalongtableinsert(tex, kind, row)
  local start, stop = tex:find(kind, 1, true)
  if not start then return nil end
  if kind == "\\endlastfoot" then
    -- A second \endlastfoot would be illegal, but giving up is not the only
    -- legal move: rows placed immediately BEFORE the existing token are part
    -- of that same last foot, so splice there. (kableExtra's repeat_header
    -- produces this shape, and so does pandoc.)
    return tex:sub(1, start - 1) .. row .. tex:sub(start)
  end
  if kind == "\\endfoot" then
    -- A running foot but no last foot: longtable reuses \endfoot on the last
    -- page. An \endlastfoot holding only the note would silently drop that
    -- foot from the last page, so copy it in ahead of the note.
    local _, headend = tex:find("\\endhead", 1, true)
    local _, firstend = tex:find("\\endfirsthead", 1, true)
    local prev = 0
    if headend and headend < start then prev = headend end
    if firstend and firstend < start and firstend > prev then prev = firstend end
    if prev == 0 then return nil end
    return tex:sub(1, stop) .. "\n" .. tex:sub(prev + 1, start - 1) ..
        row .. "\\endlastfoot\n" .. tex:sub(stop + 1)
  end
  return tex:sub(1, stop) .. "\n" .. row .. "\\endlastfoot\n" .. tex:sub(stop + 1)
end

-- Build the note row itself.
--
-- It is emitted zero-width, as \makebox[0pt][l]{\parbox[t]{..}{..}}. A
-- \multicolumn WIDER than the columns it spans stretches the whole table to
-- match, which would silently relayout a table that only asked for a note; a
-- zero-width box cannot, whatever width the note ends up with.
--
-- The note is capped at the table's own width (APA 7 section 7.14: a note
-- "should not exceed the width of the table") but floored at half the text
-- block, because a note squeezed into nine hyphenated lines under a narrow
-- table is a new failure, not a lesser one. Its left edge is pulled back to
-- the text block's left margin whichever way the table is aligned, because
-- APA table notes are flush left: the spanning cell is set to the full width
-- of the columns it covers, so aligning its zero-width content l/c/r puts
-- that content at the table's left edge, centre or right edge, and backing
-- off from there by the matching fraction of \linewidth lands on the margin.
local function apalongtablerow(ncol, width, align, notetex)
  local cellalign, shift = "l", ""
  if align == "c" then
    cellalign, shift = "c", "\\hspace*{-0.5\\linewidth}"
  elseif align == "r" then
    cellalign, shift = "r", "\\hspace*{-\\linewidth}"
  end
  local box
  if width then
    -- \dimen0 is scratch and the assignment is confined to a group.
    box = "\\begingroup\\dimen0=" .. width ..
        "\\ifdim\\dimen0<0.5\\linewidth\\dimen0=0.5\\linewidth\\fi" ..
        "\\parbox[t]{\\dimen0}{%\n" .. notetex .. "%\n}\\endgroup"
  else
    box = "\\parbox[t]{\\linewidth}{%\n" .. notetex .. "%\n}"
  end
  return "\\multicolumn{" .. ncol .. "}{@{}" .. cellalign .. "@{}}{" ..
      "\\makebox[0pt][l]{" .. shift .. box .. "}}\\\\\n"
end

-- Find the marked note among a div's children. Returns its index and the div.
local function findnote(blocks)
  for i, b in ipairs(blocks) do
    if b.t == "Div" then
      for _, c in ipairs(b.classes) do
        if c == notemarker then return i, b end
      end
    end
  end
  return nil
end

local function processdiv(div)
  local i, note = findnote(div.content)
  if not i then return nil end
  -- Whatever happens below, the marker and the carried spacing are internal
  -- and must not reach the writer.
  local sibling = note.attributes[siblingattr] or ""
  note.attributes[siblingattr] = nil
  for k = #note.classes, 1, -1 do
    if note.classes[k] == notemarker then note.classes:remove(k) end
  end

  -- Everything except the note; the note itself is going inside the table.
  local rest = pandoc.Blocks({})
  for j, b in ipairs(div.content) do
    if j ~= i then rest:insert(b) end
  end

  -- First pass: read the table. The preamble and the head/foot declarations
  -- are in different raw blocks, so both are collected before anything is
  -- rewritten.
  local spectext = nil
  local ntables = 0
  local texts = {}
  rest:walk {
    RawBlock = function(rb)
      if rb.format ~= "latex" then return nil end
      texts[#texts + 1] = rb.text
      local _, found = rb.text:gsub("\\begin{longtable}", "")
      ntables = ntables + found
      if not spectext and found > 0 then
        spectext = rb.text
      end
      return nil
    end
  }
  -- Exactly one longtable, or nothing is touched. The column count is read
  -- from the FIRST longtable but the anchor is chosen across every raw block,
  -- so with two longtables in one float the first table's column count can be
  -- spliced into the second. Measured: in the overcount direction (first
  -- {cccc}, second {cc}) that is "! Extra alignment tab has been changed to
  -- \cr" and the render fails outright with no PDF. A stranded note is a far
  -- better outcome than a document that will not build, so decline. Nothing
  -- apaTables, flextable, kable, kableExtra or pandoc emits can reach this;
  -- it takes hand-written raw LaTeX with two longtable environments inside
  -- one apa-longtable float.
  local injected = false
  if ntables == 1 and spectext then
    local ncol, width, align = apalongtablespec(spectext)
    local kind = apalongtablekind(texts)
    if ncol and kind then
      local row = apalongtablerow(ncol, width, align,
        pandoc.write(pandoc.Pandoc(note.content), "latex"))
      -- Second pass: rewrite the one block carrying the chosen anchor.
      rest = rest:walk {
        RawBlock = function(rb)
          if injected or rb.format ~= "latex" then return nil end
          local newtext = apalongtableinsert(rb.text, kind, row)
          if not newtext then return nil end
          injected = true
          return pandoc.RawBlock("latex", newtext)
        end
      }
    end
  end

  if injected then
    div.content = rest
    return div
  end
  -- Fallback: the table did not arrive as raw LaTeX (a markdown pipe table
  -- becomes a pandoc AST Table), or its preamble was one this code will not
  -- guess at. Leave the note where upstream put it, with upstream's spacing.
  if sibling ~= "" then
    div.content:insert(i, pandoc.RawBlock("latex", sibling))
  end
  return div
end

return {
  { Div = processdiv }
}
