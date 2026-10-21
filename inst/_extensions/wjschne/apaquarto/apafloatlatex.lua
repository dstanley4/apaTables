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
local haslongtable = function(float)
  local found = false
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
    end
  }
  return found
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
      -- WHY 12, AND WHAT IT HAS TO COVER. The reserve must span the WHOLE
      -- table -- caption, header, rows, and \endlastfoot, which carries the
      -- bottom rule and the note. Sizing it for the rows alone is a real and
      -- measured defect, not a theoretical one: at 8\baselineskip a 7-row
      -- table at an offset of 5.6 in kept all 7 rows on one page and pushed
      -- its note, alone under a repeated continuation header, onto the next.
      -- Rows intact and note adrift is still a broken table, and it looks
      -- deliberate.
      --
      -- 12 comes from a sweep, not from arithmetic. Against a no-reservation
      -- control, a 7-row table with a realistic two-entry note at offsets from
      -- 3.6 in to 7.6 in: 8 left 6 of 11 cases defective; 11 and 12 fixed all
      -- 6; 13 wasted 2 pages and 14 wasted 3. So 12 is the largest value that
      -- is still free, and it is taken rather than 11 because the sweep's note
      -- is two lines and a longer one wants the extra line of margin. A wider
      -- table with a three-line note can still outgrow it -- and then it
      -- splits, exactly as it does today, with every row present.
      --
      -- 12 IS RE-CONFIRMED UNDER \Needspace* and was not simply carried over:
      -- 12, 18 and 24 all give short tables 0 straddles, and 18 and 24 cost 4
      -- and 6 extra pages for it. 12 is still the cheapest value that works.
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
        blocks:insert(pandoc.RawBlock("latex", "\\Needspace*{12\\baselineskip}"))
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
      return pandoc.Div(landscapewrap(blocks, float, journalmode))
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


return {
  { Meta = getmode },
  { FloatRefTarget = processfloat }
}
