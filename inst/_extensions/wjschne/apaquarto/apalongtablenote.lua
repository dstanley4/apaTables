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
        "\\parbox[t]{\\dimen0}{\\csname apatablesnotespacing\\endcsname%\n" ..
        notetex .. "%\n}\\endgroup"
  else
    box = "\\parbox[t]{\\linewidth}{\\csname apatablesnotespacing\\endcsname%\n" ..
        notetex .. "%\n}"
  end
  -- The row opens with booktabs' own gap below a rule. Without it the
  -- note's first line sits flush under \bottomrule, and once the table rows
  -- were single-spaced the rule ran through the tops of the note's capitals
  -- (measured at 200 dpi; already so in documentmode: doc). The note itself
  -- takes back the manuscript's line spacing, which the template saved when
  -- the table single-spaced its rows -- a table note is double-spaced like
  -- the text around it, as in Word. \csname keeps both harmless under a
  -- template without them.
  -- It ends with a zero kern so the last foot has no depth: the note is a
  -- top-aligned box, so its lines after the first would otherwise be DEPTH,
  -- which neither longtable's own end-of-table check nor the template's
  -- running-foot reserve measures.
  return "\\noalign{\\vskip\\belowrulesep}" ..
      "\\multicolumn{" .. ncol .. "}{@{}" .. cellalign .. "@{}}{" ..
      "\\makebox[0pt][l]{" .. shift .. box .. "}}\\\\\n\\noalign{\\kern0pt}\n"
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

-- A note is written out as LaTeX here, after citeproc, so anything in it
-- that makes the LaTeX writer add something to the preamble is lost: the
-- writer decides what to set up from what IT writes, and it never writes
-- this note. Highlighted code needs its macros, a language span needs babel
-- -- either would leave an undefined command and no PDF. Such a note is left
-- where the writer will see it, beside its table.
local function needswriter(blocks)
  local found = false
  pandoc.Blocks(blocks):walk {
    Code = function(c)
      if #c.classes > 0 then found = true end
    end,
    CodeBlock = function(c) found = true end,
    Span = function(sp)
      if sp.attributes.lang then found = true end
    end,
    Div = function(d)
      if d.attributes.lang then found = true end
    end
  }
  return found
end

-- Highlighted code is written as \VERB|...|, a verbatim command, which
-- cannot sit inside a macro's argument -- so a note holding it cannot be
-- stored for the template either, and stays beside its table.
local function hascode(blocks)
  local found = false
  pandoc.Blocks(blocks):walk {
    Code = function(c)
      if #c.classes > 0 then found = true end
    end,
    CodeBlock = function(c) found = true end
  }
  return found
end

-- Code in a table note is set plainly, without syntax highlighting: colour
-- has no place in an APA table, and highlighted code is written as \VERB,
-- which cannot be stored in a macro, so the note could not travel with its
-- table.
local function plaincode(blocks)
  return pandoc.Blocks(blocks):walk {
    Code = function(c)
      if #c.classes > 0 then return pandoc.Code(c.text) end
      return nil
    end
  }
end

-- The note as one run of inlines, paragraphs joined by \par -- so that it
-- can sit in a table cell as a single line of source and the writer still
-- writes every inline in it (italics, a language span, a citation).
-- Returns nil for a note holding anything but paragraphs.
local function noteinlines(blocks)
  local out = pandoc.Inlines({})
  local ok = true
  local function add(bs)
    for _, b in ipairs(bs) do
      if b.t == "Div" then
        add(b.content)
      elseif b.t == "Para" or b.t == "Plain" then
        if #out > 0 then out:insert(pandoc.RawInline("latex", "\\par ")) end
        out:extend(b.content)
      else
        ok = false
      end
    end
  end
  add(blocks)
  if not ok then return nil end
  return out
end

local function prependraw(blocks, tex)
  local raw = pandoc.RawInline("latex", tex)
  local first = blocks[1]
  if first and (first.t == "Plain" or first.t == "Para") then
    first.content:insert(1, raw)
    blocks[1] = first
  else
    blocks:insert(1, pandoc.Plain({ raw }))
  end
  return blocks
end

-- Put raw LaTeX at the start of a pandoc Table's first cell: the first
-- header cell when there is a header row, otherwise the first body cell.
-- Returns the table, or nil when it has no cell at all.
--
-- The Table stays a Table: the writer still writes it, and still sees every
-- cell, so whatever a cell needs in the preamble is still set up. (Writing
-- the table out here instead, with pandoc.write, was tried and broke exactly
-- that: highlighted code or a language span in a cell left an undefined
-- command and no PDF.)
local function tablefirstcell(t, tex, headonly)
  local head = t.head
  if #head.rows > 0 then
    local rows = head.rows
    local row = rows[1]
    local cells = row.cells
    if #cells > 0 then
      local cell = cells[1]
      cell.contents = prependraw(cell.contents, tex)
      cells[1] = cell
      row.cells = cells
      rows[1] = row
      head.rows = rows
      t.head = head
      return t
    end
  end
  if headonly then return nil end
  local bodies = t.bodies
  for bi, body in ipairs(bodies) do
    local rows = body.body
    if #rows > 0 and #rows[1].cells > 0 then
      local row = rows[1]
      local cells = row.cells
      local cell = cells[1]
      cell.contents = prependraw(cell.contents, tex)
      cells[1] = cell
      row.cells = cells
      rows[1] = row
      body.body = rows
      bodies[bi] = body
      t.bodies = bodies
      return t
    end
  end
  return nil
end

-- A pandoc Table's closing rule.
--
-- The writer's longtable draws its own \bottomrule in \endlastfoot, and the
-- template appends an \hline at every \end{longtable} unless the table armed
-- \apatablesownbottomrule -- so an unarmed markdown table outside a float
-- printed TWO rules (every uncaptioned one always did; every captioned one
-- would, now that apafloatlatex.lua lifts it out of its float). The marker
-- goes in the first cell: the flag is global and is consumed only at this
-- table's own \end{longtable}, so where inside the table it is set does not
-- matter.
--
-- NOT inside a float. A table still inside \begin{table} (journal mode puts
-- markdown tables in ThreePartTable) or a hand-written \begin{figure} cannot
-- break, its \endlastfoot never reaches the page, and the \hline is its only
-- closing rule; arming it would leave it with none. Floats are tracked per
-- table, in document order, by counting the float environments the raw
-- blocks before it open and close -- a self-contained raw float elsewhere in
-- the same list changes nothing.
-- The trailing {} ends the control-sequence name: without it the cell's
-- own text runs on into it (\endcsnameA is one undefined command).
local armtex = "\\csname apatablesownbottomrule\\endcsname{}"
local floatenvs = {
  table = true, figure = true, ThreePartTable = true, twocolumntable = true,
  sidewaystable = true, sidewaysfigure = true, wraptable = true,
  wrapfigure = true
}

local function floatdelta(tex)
  local d = 0
  for env in tex:gmatch("\\begin{(%a+)%*?}") do
    if floatenvs[env] then d = d + 1 end
  end
  for env in tex:gmatch("\\end{(%a+)%*?}") do
    if floatenvs[env] then d = d - 1 end
  end
  return d
end

local armlist
armlist = function(blocks, depth)
  for k, b in ipairs(blocks) do
    if b.t == "RawBlock" and (b.format == "latex" or b.format == "tex") then
      depth = depth + floatdelta(b.text)
    elseif b.t == "Table" then
      if depth <= 0 then
        local t = tablefirstcell(b, armtex, false)
        if t then blocks[k] = t end
      end
    elseif b.t == "Figure" then
      b.content = armlist(b.content, depth + 1)
      blocks[k] = b
    elseif b.t == "Div" or b.t == "BlockQuote" then
      b.content = armlist(b.content, depth)
      blocks[k] = b
    elseif b.t == "BulletList" or b.t == "OrderedList" then
      local items = b.content
      for ii, item in ipairs(items) do
        items[ii] = armlist(item, depth)
      end
      b.content = items
      blocks[k] = b
    end
  end
  return blocks
end

-- A longtable that Quarto has already written out as raw LaTeX -- a
-- knitr::kable() table in a captioned chunk is one -- and that
-- apafloatlatex.lua lifted out of its float (it marks the Div) draws its own
-- \bottomrule too, so it is armed the way apaTables' own tables are: a last
-- row before \end{longtable}. Only when there is exactly one longtable, it
-- has a \bottomrule, and nothing armed it already.
local liftmarker = "apa-lifted-longtable"

local function armraw(blocks)
  local n, armed, hasrule = 0, false, false
  pandoc.Blocks(blocks):walk {
    RawBlock = function(rb)
      if rb.format ~= "latex" then return nil end
      local _, c = rb.text:gsub("\\begin{longtable}", "")
      n = n + c
      if rb.text:find("apatablesownbottomrule", 1, true) then armed = true end
      if rb.text:find("\\bottomrule", 1, true) then hasrule = true end
      return nil
    end
  }
  if n ~= 1 or armed or not hasrule then return blocks end
  local done = false
  return pandoc.Blocks(blocks):walk {
    RawBlock = function(rb)
      if done or rb.format ~= "latex" then return nil end
      local last, from = nil, 1
      while true do
        local a = rb.text:find("\\end{longtable}", from, true)
        if not a then break end
        last, from = a, a + 1
      end
      if not last then return nil end
      done = true
      return pandoc.RawBlock("latex", rb.text:sub(1, last - 1) ..
        "\\noalign{" .. armtex .. "}\n" .. rb.text:sub(last))
    end
  }
end

-- A markdown table's note, carried inside the table.
--
-- Under endfloat (tables at the end) only the table environment travels, so
-- a note left beside it stays behind in the body text. The note cannot be
-- spliced into a pandoc Table's \endlastfoot as it is into a raw longtable:
-- the writer writes that. Instead the note is defined, numbered, in a block
-- of its own just before the table (the writer writes it, so italics, a
-- citation or a language span are set up as anywhere else; it prints
-- nothing), and the table carries only a marker naming that number --
-- \apatablesnoteid{n}, in its first header cell, or in its caption when it
-- has no header row. The template's marker hands the note to \endlastfoot,
-- which prints it as the table's last foot row. Nothing fragile is ever put
-- in a caption (whose argument cannot hold a paragraph break and is copied
-- into the list of tables) or in a header cell (which the writer repeats,
-- with footnotes stripped, for continuation pages).
local noteseq = 0

local function pendingnote(blocks, note)
  local ntables = 0
  pandoc.Blocks(blocks):walk {
    Table = function(t) ntables = ntables + 1 end
  }
  if ntables ~= 1 then return nil end
  local inl = noteinlines(note.content)
  if not inl then return nil end
  local id = tostring(noteseq + 1)
  local marker = pandoc.RawInline("latex", "\\apatablesnoteid{" .. id .. "}")
  local done = false
  local out = pandoc.Blocks(blocks):walk {
    Table = function(t)
      if done then return nil end
      local head = t.head
      if #head.rows > 0 and #head.rows[1].cells > 0 then
        local r = tablefirstcell(t, "\\apatablesnoteid{" .. id .. "}", true)
        if r then done = true end
        return r
      end
      local cap = t.caption
      local long = cap.long
      local last = long[#long]
      if not last or (last.t ~= "Plain" and last.t ~= "Para") then
        return nil
      end
      last.content:insert(marker)
      long[#long] = last
      cap.long = long
      t.caption = cap
      done = true
      return t
    end
  }
  if not done then return nil end
  noteseq = noteseq + 1
  local def = pandoc.Inlines({ pandoc.RawInline("latex",
    "\\expandafter\\long\\expandafter\\gdef\\csname apatablesnote@" .. id ..
    "\\endcsname{") })
  def:extend(inl)
  def:insert(pandoc.RawInline("latex", "}"))
  out:insert(1, pandoc.Plain(def))
  return out
end

-- A lifted raw longtable whose head carries its caption but has no
-- \endfirsthead (Quarto writes a kable table that way) prints its number and
-- title again at the top of every continuation page. Give it a first head:
-- the rows before \endhead become the first page's head (caption included)
-- and are repeated, without the caption, as the continuation head -- the
-- shape the writer gives a markdown table and apaTables gives its own.
local function splitfirsthead(blocks)
  local hasfirst, hashead = false, false
  pandoc.Blocks(blocks):walk {
    RawBlock = function(rb)
      if rb.format ~= "latex" then return nil end
      if rb.text:find("\\endfirsthead", 1, true) then hasfirst = true end
      if rb.text:find("\\endhead", 1, true) then hashead = true end
      return nil
    end
  }
  if hasfirst or not hashead then return blocks end
  local done = false
  return pandoc.Blocks(blocks):walk {
    RawBlock = function(rb)
      if done or rb.format ~= "latex" then return nil end
      local a, b = rb.text:find("\\endhead", 1, true)
      if not a then return nil end
      local head = rb.text:sub(1, a - 1)
      local again = head
      local c = head:find("\\caption", 1, true)
      if c then
        local _, e = head:find("\\tabularnewline", c, true)
        if not e then return nil end
        again = head:sub(e + 1)
      end
      again = again:gsub("^%s*\\tabularnewline", ""):gsub("^%s+", "")
      if again:find("\\begin{longtable", 1, true) then return nil end
      done = true
      return pandoc.RawBlock("latex", head .. "\\endfirsthead\n" .. again ..
        rb.text:sub(a))
    end
  }
end

-- Store a note from the caption block of a raw longtable that Quarto wrote
-- (a kable table): the caption is written by the writer, inside the table's
-- first head, so the writer sees the note (a language span sets up babel)
-- and the template's \endlastfoot prints it. Only when the table has a last
-- foot to print it in; otherwise the stored note would reach the next table.
-- Returns the blocks, or nil.
local function captionnote(blocks, note, texts)
  local haslast = false
  for _, t in ipairs(texts) do
    if t:find("\\endlastfoot", 1, true) then haslast = true end
  end
  if not haslast then return nil end
  local inl = noteinlines(note.content)
  if not inl then return nil end
  local def = pandoc.Inlines({
    pandoc.RawInline("latex", "\\long\\gdef\\apatablespendingnote{") })
  def:extend(inl)
  def:insert(pandoc.RawInline("latex", "}"))
  local done = false
  -- The caption block: Quarto writes it as a paragraph whose \caption is a
  -- raw inline, inside a span. The stored note goes at its end, so it sits
  -- in the caption's own cell of the first head and prints nothing there.
  local function iscaption(b)
    if b.t ~= "Plain" and b.t ~= "Para" then return false end
    local hit = false
    b:walk {
      RawInline = function(x)
        if x.text:find("^%s*\\caption") then hit = true end
      end
    }
    return hit
  end
  local function visit(bs)
    for k, b in ipairs(bs) do
      if done then return end
      if iscaption(b) then
        b.content:extend(def)
        bs[k] = b
        done = true
        return
      elseif b.t == "Div" then
        visit(b.content)
      end
    end
  end
  local out = pandoc.Blocks(blocks)
  visit(out)
  if done then return out end
  return nil
end

local function processdiv(div)
  local lifted = false
  for k = #div.classes, 1, -1 do
    if div.classes[k] == liftmarker then
      div.classes:remove(k)
      lifted = true
    end
  end
  local i, note = findnote(div.content)
  if not i then
    if lifted then
      div.content = splitfirsthead(armraw(div.content))
      return div
    end
    return nil
  end
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
  if lifted then rest = splitfirsthead(armraw(rest)) end

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
  note.content = plaincode(note.content)
  local safe = not needswriter(note.content)
  local storable = not hascode(note.content)
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
  if safe and ntables == 1 and spectext then
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
  elseif storable and ntables == 0 then
    local out = pendingnote(rest, note)
    if out then
      rest = out
      injected = true
    end
  end

  if not injected and not safe and storable and ntables == 1 then
    local out = captionnote(rest, note, texts)
    if out then
      rest = out
      injected = true
    end
  end

  if injected then
    div.content = rest
    return div
  end
  -- Fallback: the table's preamble was one this code will not guess at, the
  -- float held something other than one table, or the note needs the writer.
  -- Leave the note where upstream put it, with upstream's spacing.
  div.content = rest
  div.content:insert(i, note)
  if sibling ~= "" then
    div.content:insert(i, pandoc.RawBlock("latex", sibling))
  end
  return div
end

return {
  { Div = processdiv },
  { Pandoc = function(doc)
      doc.blocks = armlist(doc.blocks, 0)
      return doc
    end }
}
