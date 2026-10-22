if FORMAT ~= "latex" then
  return
end

local utilsapa = require("utilsapa")

local noteword = "Note"

-- from quarto-cli/src/resources/pandoc/datadir/init.lua
-- global quarto params
local paramsJson = quarto.base64.decode(os.getenv("QUARTO_FILTER_PARAMS"))
local quartoParams = quarto.json.decode(paramsJson)
local function param(name, default)
  -- get name from quartoParams, if possible
  local value = quartoParams[name]
  if value == nil then
    -- get name from quartoParams.language, if possible
    if quartoParams.language then
      value = quartoParams.language[name]
    end
    -- If still nil, then assign default
    if value == nil then
      value = default
    end
  end
  return value
end


-- Is the .pdf in journal mode?
local journalmode = false
local manuscriptmode = true
local docmode = false
-- Are floats collected at the end? Computed by utilsapa.delayedfloats, the
-- same function apalandscapelatex.lua calls, so the two filters cannot
-- disagree about which placement is active.
local delayedfloats = true
local noteprefix = "\\noindent\\textit{Note.}"
local beforenote = ""
local getmode = function(meta)
  local documentmode = pandoc.utils.stringify(meta["documentmode"])
  journalmode = documentmode == "jou"
  manuscriptmode = documentmode == "man"
  docmode = documentmode == "doc"
  delayedfloats = utilsapa.delayedfloats(meta)
  -- Find word for "note"
  if not meta.language["figure-table-note"] then
    if param("callout-note-title") then
      meta.language["figure-table-note"] = param("callout-note-title")
    end
  end
  noteprefix = "\\noindent \\emph{" .. meta.language["figure-table-note"] .. ".} "
end



-- Split string function
--function string:split(delimiter)
--  local result               = {}
--  local from                 = 1
--  local delim_from, delim_to = string.find(self, delimiter, from)
--  while delim_from do
--    from                 = delim_to + 1
--    delim_from, delim_to = string.find(self, delimiter, from)
--  end
--  table.insert(result, string.sub(self, from))
--  return result
--end

-- Does this float's content contain a longtable?
--
-- A longtable CANNOT live inside a LaTeX float: \begin{table} makes its
-- contents unbreakable, so every row past the bottom of the page is dropped
-- SILENTLY -- no error, no warning, a document that compiles. Measured on a
-- 60-row table: 55 rows survive inside the float and the table's note never
-- reaches the page at all; outside it, 60 of 60 rows and the note attached.
-- Detecting a longtable body is therefore not a heuristic about what the
-- author probably wanted; it is reading a fact about what LaTeX can typeset.
-- That is why this replaces the `apa-longtable` chunk option: nothing about
-- the decision needed the author's help.
--
-- WHICH ENVIRONMENT NAME. At this filter's stage (post-quarto) the block is
-- still the unstarred \begin{longtable} -- measured by logging every RawBlock
-- reaching processfloat. Quarto's own main.lua rewrites a captionless raw
-- longtable to longtable* LATER, in the writer, so the starred name never
-- arrives here from a Quarto-produced table. The starred name is matched
-- anyway, and deliberately: it costs one alternation, and if a hand-written
-- or third-party starred longtable ever does arrive, sealing it in a float
-- loses rows invisibly, whereas letting it through loses only the caption
-- Quarto declined to splice -- a visible defect rather than a silent one.
--
-- A MARKDOWN TABLE IS A LONGTABLE TOO. It is still a pandoc Table here and
-- becomes LaTeX only in the writer, which always writes a table as a
-- longtable. Since Pandoc 3.8 (Quarto 1.9) a captioned one arrives wrapped in
-- \begin{table}, and the loss is the one described above: a 90-row table
-- stopped at the foot of its first page after 44 to 48 rows, depending on the
-- document mode, in both placements. Lifted out, the writer puts the caption inside
-- the longtable (the shape apaTables' own tables have), so numbering and
-- cross-references are unchanged. apalongtablenote.lua then makes its own
-- closing rule stand in for the template's and carries its note inside it.
--
-- ONLY A FLOAT WHOSE WHOLE CONTENT IS ONE TABLE. Lifted, a float with two
-- tables (or a panel of subtables) prints each table with the float's
-- caption and its own number, so the cross-reference points at the second
-- and every later table is numbered one too high -- silently; and a float
-- holding a paragraph as well as its table loses the paragraph to the body
-- text when tables go to the end, since only the table environment travels.
-- Such floats are left to the float path, as before.
local function onlytable(blocks)
  local tables, other = 0, 0
  local function scan(bs)
    for _, b in ipairs(bs) do
      if b.t == "Table" then
        tables = tables + 1
      elseif b.t == "Div" then
        scan(b.content)
      elseif b.t == "RawBlock" then
        if b.text:match("%S") then other = other + 1 end
      elseif (b.t == "Plain" or b.t == "Para") then
        if #b.content > 0 then other = other + 1 end
      else
        other = other + 1
      end
    end
  end
  scan(blocks)
  return tables == 1 and other == 0
end

local haslongtable = function(float)
  local found = false
  local ntables = 0
  local node = float.__quarto_custom_node or float.content
  if node == nil then
    return false
  end
  node:walk {
    RawBlock = function(rb)
      if not found and (rb.format == "tex" or rb.format == "latex") then
        if rb.text:find("\\begin{longtable}", 1, true)
            or rb.text:find("\\begin{longtable*}", 1, true) then
          found = true
        end
      end
      return nil
    end,
    Table = function(t)
      ntables = ntables + 1
      return nil
    end
  }
  if found then return true end
  if ntables ~= 1 then return false end
  local content = float.content
  if content and content.t ~= nil and content.t ~= "Blocks" then
    content = pandoc.Blocks({ content })
  end
  if content and not onlytable(content) then return false end
  return true
end

-- How tall a table will be -- single-spaced lines of body text, plus points
-- for rows set at their own size -- so that the in-text reservation (below)
-- can be the table's own height rather than a fixed amount. Calibrated on
-- measured tables (caption top to note bottom, 2026-10-21): the number,
-- title, gap and three rules take 3.4 lines plus the header rows; a markdown
-- table's row is a body line, and a row of a raw table that sets its own
-- size (apaTables' \fontsize{10}{12}) is counted at that size in points; a
-- stacked apaTables cell -- estimate, interval, p -- counts each stacked
-- line; a note line keeps the manuscript's
-- spacing, 1.65 lines outside documentmode: doc, about 95 characters to a
-- full-width line, each note entry starting a new line; and a running foot
-- (apaTables' 48 pt spacer) counts when it is taller than the note. On the
-- calibration sweeps it errs high by one to two lines, on purpose: too high
-- moves a table that would just have fitted, too low lets a short table
-- straddle a page break.
local function rawrows(segment)
  local n = 0
  for line in segment:gmatch("[^\n]+") do
    if line:find("\\\\%*?%s*$") and not line:find("tabularnewline", 1, true) then
      local deepest = 0
      for inner in line:gmatch("\\begin{tabular}(.-)\\end{tabular}") do
        local _, c = inner:gsub("\\\\", "")
        if c > deepest then deepest = c end
      end
      n = n + 1 + deepest
    end
  end
  return n
end

local function tablelines(float)
  local lines = 3.4
  local title = pandoc.utils.stringify(float.caption_long or "")
  lines = lines + math.floor(#title / 90)
  local node = float.__quarto_custom_node or float.content
  local raw = {}
  local rows = 0
  local rowpts = 0
  local footpts = 0
  if node then
    node:walk {
      RawBlock = function(rb)
        if rb.format == "tex" or rb.format == "latex" then
          raw[#raw + 1] = rb.text
        end
      end,
      Table = function(t)
        -- A cell's lines: each paragraph, list item and line break starts
        -- a line, and a paragraph wraps only in a column given a relative
        -- width.
        local function blocklines(blocks, cap)
          local n = 0
          for _, b in ipairs(blocks) do
            if b.t == "Para" or b.t == "Plain" then
              local breaks = 0
              for _, inl in ipairs(b.content) do
                if inl.t == "LineBreak" then breaks = breaks + 1 end
              end
              local chars = #pandoc.utils.stringify(b)
              n = n + math.max(1, math.ceil(chars / cap)) + breaks
            elseif b.t == "BulletList" or b.t == "OrderedList" then
              for _, item in ipairs(b.content) do
                n = n + math.max(1, blocklines(item, cap))
              end
            elseif b.t == "LineBlock" then
              n = n + #b.content
            elseif b.t == "Div" then
              n = n + blocklines(b.content, cap)
            else
              n = n + 1
            end
          end
          return n
        end
        local function rowlines(r)
          local most = 1
          for ci, c in ipairs(r.cells) do
            local spec = t.colspecs[ci]
            local w = spec and spec[2]
            local cap = math.huge
            -- A column holds its share of the line less its padding (about
            -- three characters), and words wrap short of the edge (about
            -- 85% of it is filled): measured, a 70-character cell in a
            -- fifth-of-the-line column printed in 4 lines, not 3.
            if type(w) == "number" and w > 0 then
              cap = math.max(1, (w * 95 - 3) * 0.85)
            end
            local n = blocklines(c.contents, cap)
            if n > most then most = n end
          end
          return most
        end
        for _, r in ipairs(t.head.rows) do rows = rows + rowlines(r) end
        for _, b in ipairs(t.bodies) do
          for _, r in ipairs(b.head) do rows = rows + rowlines(r) end
          for _, r in ipairs(b.body) do rows = rows + rowlines(r) end
        end
        for _, r in ipairs(t.foot.rows) do rows = rows + rowlines(r) end
      end
    }
  end
  if rows == 0 and #raw > 0 then
    local tex = table.concat(raw, "\n")
    local headend = tex:find("\\endfirsthead", 1, true)
        or tex:find("\\endhead", 1, true)
    local bodystart = nil
    for _, anchor in ipairs({ "\\endlastfoot", "\\endfoot", "\\endhead",
                              "\\endfirsthead" }) do
      local from = 1
      while true do
        local _, e = tex:find(anchor, from, true)
        if not e then break end
        if not bodystart or e > bodystart then bodystart = e end
        from = e + 1
      end
    end
    local n = 0
    if headend then n = n + rawrows(tex:sub(1, headend)) end
    n = n + rawrows(tex:sub((bodystart or 0) + 1))
    local skip = tonumber(tex:match("\\fontsize{[%d.]+}{([%d.]+)}"))
    -- Space added between rows (apaTables puts \addlinespace before each
    -- group label), counted at a 12 pt em, rounded high.
    local body = tex:sub((bodystart or 0) + 1)
    for amount in body:gmatch("\\addlinespace%[([%d.]+)em%]") do
      rowpts = rowpts + tonumber(amount) * 12
    end
    for _ in body:gmatch("\\addlinespace%f[^%a%[]") do
      rowpts = rowpts + 6
    end
    if skip then
      -- Rows set at their own size (apaTables' \fontsize{10}{12}) are
      -- counted in points, not in lines of the body: a document set at
      -- 10 pt has 12 pt body lines, and a row counted as a fraction of a
      -- 14.5 pt line came out a sixth short there.
      rowpts = rowpts + n * skip
    else
      rows = n
    end
    -- The running foot. Until its last row a longtable keeps room for its
    -- \endfoot on the page; apaTables' is a 48 pt spacer, taller than the
    -- note that replaces it on the last page, so a table needs that much
    -- more room to fit than it shows (measured: a 6-row table straddled at
    -- an offset where its visible height fitted).
    local fa, fb = tex:find("\\endhead", 1, true)
    local fe = tex:find("\\endfoot", 1, true)
    if fb and fe and fe > fb then
      local foot = 0
      for h in tex:sub(fb + 1, fe):gmatch("\\rule{[^}]*}{([%d.]+)pt}") do
        foot = foot + tonumber(h)
      end
      footpts = math.max(foot, rawrows(tex:sub(fb + 1, fe)) * (skip or 14.5))
    end
  end
  lines = lines + rows
  local lastfoot = 0
  local note = float.attributes["apa-note"]
  if note and note ~= "" then
    local notelines = 0
    for entry in (note .. '","'):gmatch('(.-)","') do
      notelines = notelines + math.max(1, math.ceil(#entry / 95))
    end
    lastfoot = 0.5 + notelines * (docmode and 1 or 1.65)
  end
  -- The last foot is the note or the running foot, whichever is taller.
  local pts = rowpts
  if footpts > lastfoot * 14.5 then
    pts = pts + footpts
  else
    lines = lines + lastfoot
  end
  -- Rounded up: a table that straddles a page break is the defect this
  -- exists to prevent, and one moved a line early is only white space.
  -- (Rounding to the nearest line let a short apaTables table straddle.)
  local n = math.ceil(lines)
  -- The space longtable leaves above itself (\LTpre, 12 pt) is not in the
  -- calibration, which runs from caption to note. A manuscript's note
  -- allowance (1.65 lines a line) covers it; documentmode: doc counts its
  -- note at one line a line, so there it is added (measured: a correlation
  -- table straddled with its visible height 3 pt inside the reservation).
  local pre = docmode and "+\\LTpre" or ""
  if pts > 0 then
    return "\\dimexpr " .. n .. "\\apatablesline+" .. math.ceil(pts) ..
        "pt" .. pre .. "\\relax"
  end
  if docmode then
    return "\\dimexpr " .. n .. "\\apatablesline" .. pre .. "\\relax"
  end
  return n .. "\\apatablesline"
end

-- Wrap a rotated float in a LITERAL \begin{landscape} block.
--
-- Rotation is pdflscape, in both placements and whatever the table's height:
-- APA asks for the PAGE to be turned ("use landscape orientation on the page
-- with the wide table ... it does not matter if the page header also moves"),
-- and turning the page is one mechanism that works for a table of any length.
-- The rotating package's sidewaystable, which this filter used to emit when
-- floats were collected at the end, turns the TABLE on an upright page -- it
-- is apa7's LaTeX convention rather than APA's instruction, and it is a FLOAT,
-- so it cannot hold a longtable at all. Measured on a 60-row table inside a
-- landscape div with floats at the end: sidewaystable gave 51 of 60 rows, a
-- starred longtable*, a note that never reached the PDF, and an upright page,
-- with no error and no warning.
--
-- LITERAL, never macro-constructed. Under endfloat the deferred block is read
-- back line by line and terminated by an exact match on \end{landscape}, and a
-- float built by a macro dies there (\efloat@xfloat, TeX capacity exceeded).
-- Two RawBlocks, each alone on its line, is the whole construction.
--
-- The template declares landscape as a delayed float flavour of table, so
-- endfloat carries this block to the end of the manuscript as a unit --
-- rotation included, and one page per table, because each float gets its own
-- wrapper rather than sharing one around the div.
local landscapewrap = function(blocks, float, injournalmode)
  if float.attributes["apa-landscape"] ~= "true" then return blocks end
  -- Journal mode has no endfloat and a two-column page; rotation there is a
  -- separate problem and was never handled here.
  if injournalmode then return blocks end
  -- Already inside Quarto's own ::: {.landscape} div, which supplies the
  -- environment when floats stay in the text (apalandscapelatex.lua says so).
  -- A second, nested \begin{landscape} would clear the page twice.
  if float.attributes["apa-landscape-wrapped"] == "true" then return blocks end
  quarto.doc.use_latex_package("pdflscape")
  local out = pandoc.Blocks({ pandoc.RawBlock("latex", "\\begin{landscape}") })
  out:extend(blocks)
  out:insert(pandoc.RawBlock("latex", "\\end{landscape}"))
  return out
end

local processfloat = function(float)
  if float.attributes["disable-apaquarto-processing"] then
    if not (float.attributes["disable-apaquarto-processing"] == "false") then
      return float
    end
  end
  -- default float position
  local floatposition = "[!htbp]"
  local p = {}
  local apanotedivs = pandoc.Div(pandoc.Blocks {})
  if float.attributes["fig-pos"] then
    if pandoc.utils.stringify(float.attributes["fig-pos"]) == "false" then
      floatposition = "[!htbp]"
    else
      floatposition = "[" .. float.attributes["fig-pos"] .. "]"
    end
  end

  if float.type == "Table" then
        -- credit to @michaelzehetleitne https://github.com/wjschne/apaquarto/issues/71
        -- Long-table mode: skip float wrapper so longtable can page-break
    --quarto.log.output(float.attributes)
    -- `apa-longtable: true` is still honoured, so a document that carries the
    -- old chunk option renders exactly as it did; it is simply no longer
    -- something the author has to know about.
    --
    -- A ROTATED longtable takes this branch too, and must. The interim guard
    -- that used to stand here -- `and float.attributes["apa-landscape"] ~=
    -- "true"` -- existed only because rotation was then done with
    -- sidewaystable, which IS a float, so dropping the wrapper would have
    -- dropped the rotation with it. Rotation is now a literal
    -- \begin{landscape} block that wraps whatever this branch returns and
    -- needs no float to grip, so the guard is gone. It had to go in the same
    -- change: left behind, it would have sealed a rotated longtable inside
    -- \begin{table} and lost rows in silence.
    if (float.attributes["apa-longtable"] == "true" or haslongtable(float))
        and not journalmode then
      local blocks = pandoc.Blocks({})
      -- Keep a SHORT table whole -- but only where floats stay in the text.
      --
      -- A longtable outside a float is breakable, which is what a tall table
      -- needs and what a short one does not want: measured on a 6-row table at
      -- vertical offsets of 6.8 in and 7.2 in, it straddled the page break 5/1
      -- and 4/2. The reservation asks LaTeX to start a new page when less than
      -- the stated amount of column is left, so a short table moves down whole
      -- while a tall one still breaks with every row present. The argument is a
      -- HINT, not a measurement -- too large moves a table that did not need
      -- moving, too small lets it split; neither loses a row. Starring the rows
      -- (\\*) was the other candidate and is REJECTED: it makes the table
      -- unbreakable, and 10 of 40 rows vanished from a document that compiled
      -- without a murmur.
      --
      -- IT IS \Needspace*, THE EAGER FORM, AND THE LOWER-CASE \needspace IS A
      -- MEASURED DEFECT -- do not "simplify" it back.
      --
      -- \needspace works by GLUE: it lays down `\vskip 0pt plus X`, a
      -- `\penalty-100`, and a matching negative skip, which makes a break in
      -- front of the table cheap and leaves the decision to TeX's page builder.
      -- By the time that builder runs, \LT@start has already contributed the
      -- first head AND installed \LT@output (longtable.sty:253). \LT@output's
      -- normal-break branch ends `\copy\LT@head\nobreak` (longtable.sty:530),
      -- UNCONDITIONALLY -- so it services a break that happened BEFORE any of
      -- the table's own material and stamps a continuation header on the new
      -- page. The reader gets a rule, the header row, a rule, and then the
      -- caption and the real table underneath: the header printed twice, the
      -- first copy attached to nothing.
      --
      -- \Needspace* instead compares \pagegoal-\pagetotal against the argument
      -- and breaks THERE AND THEN, before \begin{longtable} is ever read, so
      -- longtable never sees the break and never repeats its head.
      --
      -- MEASURED, 26 documents, table pushed to a different height on the page
      -- in each -- and the two sizes are scored on DIFFERENT properties,
      -- because for a TALL table straddling is correct behaviour, not a defect:
      --
      --   short table (straddle is the defect)   stranded  straddle  pages
      --     \needspace                              2         0        39
      --     nothing                                 0         2        37
      --     \Needspace*                             0         0        37
      --   tall table (stranded head is the defect)
      --     \needspace                              6         0        43
      --     nothing                                 0         8        43
      --     \Needspace*                             0         6        43
      --
      -- \Needspace* is better on every axis that applies: short tables still
      -- never straddle, tall tables are never stranded, and it costs two pages
      -- FEWER than \needspace, which was pushing tall tables off pages they had
      -- half a column left on.
      --
      -- WHAT IT HAS TO COVER. The reserve must span the WHOLE table --
      -- caption, header, rows, and \endlastfoot, which carries the bottom rule
      -- and the note. Sizing it for the rows alone is a real and measured
      -- defect, not a theoretical one: a 7-row table kept all 7 rows on one
      -- page and pushed its note, alone under a repeated continuation header,
      -- onto the next. Rows intact and note adrift is still a broken table,
      -- and it looks deliberate.
      --
      -- HOW MUCH. It used to be a fixed 12\baselineskip, swept when table
      -- rows were double-spaced (a 7-row table with a two-entry note needed
      -- 11; 13 and 14 wasted pages). Table rows are single-spaced now, so the
      -- fixed amount was about twice what a short table needs, and a one-row
      -- table moved to the next page with half a page to spare. The reserve is
      -- now the table's own estimated height (tablelines, above), in
      -- single-spaced lines (\apatablesline, set by the template). The
      -- template compares it with the page: a table that fits on a page moves
      -- to the next one whole rather than starting low and breaking (as a
      -- captioned markdown table, once a float, always did), and one taller
      -- than the page asks only for room to start and breaks across pages,
      -- as a tall table should, with every row present.
      --
      -- NOT in the default placement, for three separate reasons, any one of
      -- which is sufficient. endfloat re-emits the deferred environment alone
      -- and drops what preceded it, so the instruction is stripped (measured:
      -- 6 occurrences in the body .tex, 0 in the .ttt). It is not needed, since
      -- endfloat already gives each deferred float its own page, leaving a
      -- short table nothing to straddle. And what endfloat leaves behind is a
      -- bare reservation sitting in the author's prose, where it can force a
      -- page break in the text that no table justifies.
      if not delayedfloats then
        blocks:insert(pandoc.RawBlock("latex", "\\Needspace*{" ..
          tablelines(float) .. "}"))
      end
      -- Use Quarto's native longtable output (caption + label included)
      if float.__quarto_custom_node then
        blocks:insert(float.__quarto_custom_node)
      else
        blocks:insert(float.content)
      end
      -- Append apa-note if present.
      --
      -- The note is left in the AST, marked, and spliced into the table's own
      -- \endlastfoot later, by apalongtablenote.lua. It cannot be spliced
      -- here for two measured reasons. First, this filter runs at post-quarto,
      -- BEFORE citeproc: a note frozen to LaTeX now keeps its [@key]
      -- literally, and a work cited only in a table note is never counted, so
      -- it vanishes from the reference list -- and APA 7 section 7.7 requires
      -- a copyright attribution note on any reproduced table, which makes a
      -- citation the most ordinary thing a table note can hold. Second, the
      -- raw block is inside Quarto's custom float node; replacing one block
      -- with three breaks that node, and the table reverts to a \begin{table}
      -- float with the note dropped entirely.
      if float.attributes["apa-note"] then
        local bn = ""
        if manuscriptmode then
          bn = "\\vspace{-12pt}\n"
          if float.attributes["beforenotespace"] then
            bn = "\\vspace{" .. float.attributes["beforenotespace"] .. "}\n"
          end
        end
        -- Inside the table the note is a row, not a paragraph that follows the
        -- table environment, so the -12pt correction that closes the paragraph
        -- gap would drag it up over the bottom rule. The note is therefore
        -- built without it and the sibling-only spacing is carried alongside,
        -- to be put back if the splice cannot happen. An explicitly requested
        -- beforenotespace applies on both paths: dropping a stated option
        -- silently is worse than any spacing it produces.
        local fbn = ""
        if float.attributes["beforenotespace"] then
          fbn = "\\vspace{" .. float.attributes["beforenotespace"] .. "}\n"
        end
        local npfx = pandoc.Span(pandoc.RawInline("latex", fbn .. noteprefix))
        local notedivs = utilsapa.make_note(float.attributes["apa-note"], npfx)
        notedivs.classes:insert("apa-longtable-note")
        notedivs.attributes["apa-longtable-note-sibling-space"] = bn
        blocks:insert(notedivs)
      end
      local lifted = pandoc.Div(landscapewrap(blocks, float, journalmode))
      -- Marked for apalongtablenote.lua, which arms the closing rule of a
      -- table Quarto has already written as raw LaTeX (a kable table).
      lifted.classes:insert("apa-lifted-longtable")
      return lifted
    end
    -- Default table environment
    local latextableenv = "table"
    -- Manuscript spacing before note needs adjustment ment
    if manuscriptmode then
      beforenote = "\\vspace{-12pt}\n"
      if float.attributes["beforenotespace"] then
        beforenote = "\\vspace{" .. float.attributes["beforenotespace"] .. "}\n"
      end
    end
    if journalmode then
      -- No spacing in before note in journalmode
      beforenote = ""
      if float.attributes["beforenotespace"] then
        beforenote = "\\vspace{" .. float.attributes["beforenotespace"] .. "}\n"
      end
      -- Table environment in journal mode
      latextableenv = "ThreePartTable"
    end

    -- Table enironment for apa-twocolumn floats
    if float.attributes then
      if float.attributes["apa-twocolumn"] then
        if float.attributes["apa-twocolumn"] == "true" then
          if journalmode then
            latextableenv = "twocolumntable"
          end
        end
      end
    end

    -- Add note
    if float.attributes["apa-note"] then
      local note_prefix = pandoc.Span(pandoc.RawInline("latex", beforenote .. noteprefix))
      apanotedivs = utilsapa.make_note(float.attributes["apa-note"], note_prefix)
    end

    local captionsubspan = pandoc.Span({
      pandoc.RawInline("latex", "\\label"),
      pandoc.RawInline("latex", "{"),
      pandoc.RawInline("latex", float.identifier),
      pandoc.RawInline("latex", "}")
    })

    -- Adjust space after caption in manuscript mode
    local aftercaption = ""
    if manuscriptmode then
      aftercaption = "\n\\vspace{-20pt}"
      if float.attributes["after-caption-space"] then
        aftercaption = "\\vspace{" .. float.attributes["after-caption-space"] .. "}\n"
      end
    end

    -- Make caption
    local captionspan = pandoc.Span({
      pandoc.RawInline("latex", "\\caption"),
      pandoc.RawInline("latex", "{"),
      pandoc.Span(float.caption_long.content),
      captionsubspan,
      pandoc.RawInline("latex", "}" .. aftercaption)

    })


    -- Make table
    local returnblock = pandoc.Div({
      pandoc.RawBlock("latex", "\\begin{" .. latextableenv .. "}"),
      captionspan,
      float.content

    }
    )
    returnblock.content:extend({ apanotedivs })


    returnblock.content:extend({ pandoc.RawBlock("latex", "\\end{" .. latextableenv .. "}") })

    if journalmode then
      returnblock = pandoc.Div({
        pandoc.RawBlock("latex", "\\begin{" .. latextableenv .. "}"),
        float.__quarto_custom_node,
        apanotedivs,
        pandoc.RawBlock("latex", "\\end{" .. latextableenv .. "}")
      })
    end

    return pandoc.Div(landscapewrap(pandoc.Blocks({ returnblock }), float,
                                    journalmode))
  end

  if float.type == "Figure" then
    local hasnote = false
    local apanote
    local twocolumn = false
    -- A rotated figure is an ordinary \begin{figure} inside a literal
    -- \begin{landscape} block, for the same reason a rotated table is: the
    -- page turns, not the picture. `landscape` is still tracked because it is
    -- what makes this filter build the float itself rather than leave the
    -- figure to Quarto -- there would otherwise be nothing to wrap.
    local landscape = float.attributes["apa-landscape"] == "true"
        and not journalmode
    local latexenv = "figure"
    -- Get apa-note from image, if possible
    float.content:walk {
      Image = function(img)
        if img.attributes["apa-note"] then
          hasnote = true
          apanote = img.attributes["apa-note"]
        end

        if img.attributes["beforenotespace"] then
          beforenote = "\\vspace{" .. img.attributes["beforenotespace"] .. "}\n"
        end
        if img.attributes["apa-twocolumn"] then
          if img.attributes["apa-twocolumn"] == "true" then
            if journalmode then
              twocolumn = true
            end
          end
        end
      end
    }

    if twocolumn and not landscape then
      latexenv = "figure*"
    end

    -- Make note
    if hasnote or twocolumn or landscape then
      if hasnote then
        -- Add note
        if float.attributes["apa-note"] then
          local note_prefix = pandoc.Span(pandoc.RawInline("latex", beforenote .. noteprefix))
          apanotedivs = utilsapa.make_note(float.attributes["apa-note"], note_prefix)
        end
      end

      local captionsubspan = pandoc.Span({
        pandoc.RawInline("latex", "\\label"),
        pandoc.RawInline("latex", "{"),
        pandoc.Str(float.identifier),
        pandoc.RawInline("latex", "}")
      })

      local captionspan = pandoc.Span({
        pandoc.RawInline("latex", "\\caption"),
        pandoc.RawInline("latex", "{"),
        pandoc.Span(float.caption_long.content),
        captionsubspan,
        pandoc.RawInline("latex", "}")
      })

      if float.attributes.prefix ~= "" then
        floatposition = ""
      end

      local returnblock = pandoc.Div({
        pandoc.RawBlock("latex", "\\begin{" .. latexenv .. "}" .. floatposition),
        captionspan,
        float.content,
        apanotedivs,
        pandoc.RawBlock("latex", "\\end{" .. latexenv .. "}")
      })

      return pandoc.Div(landscapewrap(pandoc.Blocks({ returnblock }), float,
                                      journalmode))
    end
  end
end


-- A table placed in the text straight after a heading: its reservation is
-- moved in front of the heading and made three lines larger, so that when
-- the table has to go to the next page the heading goes with it. Left
-- between them, the reservation broke the page after the heading, which was
-- left alone at the foot of the page above its table.
local function stripreserve(b)
  if b.t ~= "Div" then return nil end
  local first = b.content[1]
  if not first then return nil end
  if first.t == "RawBlock" and first.text:find("^\\Needspace%*{") then
    local rest = pandoc.Blocks({})
    for i = 2, #b.content do rest:insert(b.content[i]) end
    b.content = rest
    return b, first
  end
  local inner, rb = stripreserve(first)
  if rb then
    local content = b.content
    content[1] = inner
    b.content = content
    return b, rb
  end
  return nil
end

local function movereserve(blocks)
  local k = 1
  while k < #blocks do
    if blocks[k].t == "Header" then
      local nb, rb = stripreserve(blocks[k + 1])
      if rb then
        local arg = rb.text:match("^\\Needspace%*{(.*)}%s*$")
        blocks[k + 1] = nb
        blocks:insert(k, pandoc.RawBlock("latex", "\\Needspace*{\\dimexpr " ..
          arg .. "+3\\apatablesline\\relax}"))
        k = k + 1
      end
    end
    k = k + 1
  end
  return blocks
end

return {
  { Meta = getmode },
  { FloatRefTarget = processfloat },
  { Blocks = movereserve }
}
