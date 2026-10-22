-- apaTables' Word table-size ladder. apa_quarto_markdown_block()
-- (R/quarto-table.R) emits every table it sends to Word with zero per-cell
-- markup and an empty sibling marker div immediately after the grid
-- table's own markdown, at every size including the 10 pt default:
--
--   ::: {.apa-table-size-10}
--   :::
--
-- naming the target point size. This filter finds that marker inside the
-- same float, reads the size, selects the matching table style
-- (Table6-Table12, apaTables' own styles: the APA rules, 15-twip cell
-- margins, differing only in <w:sz>/<w:szCs>) as the Table AST node's
-- custom-style, and removes the marker div so it leaves no trace in the
-- rendered document. The reference document's plain "Table" style is left
-- as stock apaquarto has it (document-default 12 pt, 58-twip margins), so a
-- markdown table the user writes -- which pandoc gives "Table" -- keeps
-- the spacing it has without apaTables. apaTables' tight 15-twip margins on
-- "Table" ran a markdown table's columns together ("3.10 0.82" read as
-- "3.100.82").
--
-- MUST be a Lua filter operating on the AST, not the markdown-level
-- `: {custom-style="TableN"}` caption-attribute syntax: that syntax works
-- under bare pandoc but is SILENTLY DEFEATED by Quarto's own crossref/float
-- pipeline under a real `quarto render` (measured).
--
-- Registration mirrors docxstyler.lua (post-quarto, docx only).
--
-- FOUND BY A Pandoc-LEVEL WALK, NOT A FloatRefTarget HANDLER. Only a
-- `tbl-` labelled chunk's table lives inside a FloatRefTarget at this
-- stage, but apa_quarto_markdown_block() emits the size marker whatever the
-- chunk's label (apatablelandscape.lua finds its own marker the same way,
-- for the same reason -- see that file's banner). A FloatRefTarget-only
-- handler here silently does nothing for an unlabelled table: a wide one
-- would rotate but keep the plain "Table" style, leaving an inert
-- `apa-table-size-N` div in the rendered docx. MEASURED: a `Pandoc`-level
-- walk recurses into a `FloatRefTarget`'s content automatically, so ONE
-- handler covers both the labelled and unlabelled cases.
--
-- The walk handles `Blocks` (a whole sibling list at a time), NOT `Div` and
-- `Table` as two separate document-wide passes -- see `process_blocks()`'s
-- own comment below for why.
--
-- IMPORTANT: this script returns EXACTLY the plain, universal apaquarto
-- filter shape, `{ { Pandoc = process_pandoc } }`, with no extra keys. A
-- filter returning a table with a stray non-array key is silently SKIPPED
-- by Quarto's own filter loader (main.lua) -- confirmed more than once
-- (see apatablemeta.lua) -- while bare
-- `pandoc --lua-filter=` and `pandoc lua` both tolerate the same stray key,
-- so verifying against either is not verifying; this was checked only
-- through an end-to-end `quarto render`.

-- This filter serves TWO destinations. Word takes its size from a cloned
-- table STYLE; HTML takes it from a wrapper DIV that this filter builds
-- HERE, at post-quarto, after apaquarto's own float collection has already
-- run, paired with a `.apa-table-size > table { font-size: inherit; }` rule
-- in apa.css -- NOT a style attribute set directly on the Table node, which
-- was MEASURED to be silently discarded by Quarto's HTML pipeline before
-- the page is written (see the comment on the apply step below).
--
-- No early return for other formats: the walk still runs everywhere so that
-- a marker div is always CONSUMED, never left as dead weight in an output
-- this filter does not otherwise serve. Only the APPLY step branches.
local is_docx = FORMAT == "docx"
local is_html = FORMAT:match("^html") ~= nil

local sizeclass = "^apa%-table%-size%-(%d+)$"
local sizeattr = "apa-table-size-pt"
-- Mirrors APA_QUARTO_HTML_CLASS in R/quarto-table.R: R
-- writes this class on the marker div, this filter turns it into the wrapper,
-- and apa.css scopes the table-width rules to it. Three copies of one name,
-- unavoidably -- they are checked against each other by
-- tests/testthat/test-html-structure.R on a real render, which is the only
-- place all three meet.
local htmlclass = "apa-table-html"

-- The only sizes with a table style of apaTables' own in the reference
-- document are Table6-Table12. R's apa_quarto_size_rung() already
-- refuses to write a marker naming anything else, but this filter is a
-- bundled, independently loadable artifact -- another caller, a future R
-- change, or a hand-authored `::: {.apa-table-size-5}` in someone's own
-- document must not be trusted just because today's only caller behaves.
-- Concatenating ANY digit string blindly would write custom-style="Table5"
-- into the docx for a style that was never cloned, and Word's response to
-- an unknown table style is to drop the style assignment ENTIRELY -- losing
-- the APA rules and cell margins along with the requested size. So the
-- check is repeated here, independently.
-- The range is 6-12, 10 included: the default size is named like every
-- other, because the plain "Table" style is no longer apaTables' (see the
-- banner). R's apa_quarto_size_rung() enforces the same range, and
-- tests/testthat/test-word-size-styles.R asserts this list and the styles
-- actually in apaquarto.docx agree.
local valid_sizes = {
  ["6"] = true, ["7"] = true, ["8"] = true, ["9"] = true,
  ["10"] = true, ["11"] = true, ["12"] = true,
}

-- ONE marker div per TABLE, scoped to the Blocks LIST that table and its
-- own marker actually sit in together. Two document-wide passes (a Div
-- walk finding markers into ONE shared `size`, then a Table walk applying
-- it to EVERY Table node) is a LAST-MARKER-WINS bug: three tables marked
-- 9/unmarked/6 all came out `Table6` -- reproduced even with bare
-- `pandoc --lua-filter=`, so it is not about Quarto's pipeline. A table
-- that should stay at its own size would be resized because some
-- OTHER table needed it, and a plain user-authored table with nothing to
-- do with apaTables could be silently restyled.
--
-- So this uses pandoc's `Blocks` filter type (a handler that runs once per
-- BLOCKS LIST anywhere in the tree, not once per node) instead of `Div`/
-- `Table`, because it is the one traversal pandoc offers whose UNIT is a
-- sibling list rather than a single node -- exactly the scope a marker and
-- "the table it belongs to" share. `apa_quarto_size_wrap()`
-- (R/quarto-table.R) always appends the marker directly after the table's
-- own grid markdown with nothing else between them, so table and marker
-- are ALWAYS adjacent elements of the SAME immediate Blocks list, whatever
-- divs surround that list (a FloatRefTarget's content, an unlabelled
-- chunk's own cell div, nested arbitrarily deep) -- MEASURED (a
-- three-table probe: one plain, one marked 9, one marked 6): a
-- `Blocks` handler run via `doc.blocks:walk({ Blocks = fn })` visits the
-- exact two-element [Table, marker Div] list for each marked table
-- separately, and the ONE plain table's own singleton [Table] list on its
-- own, with no crosstalk between them.
local function process_blocks(blocks)
  local out = pandoc.Blocks({})
  local last_table_idx = nil
  local changed = false
  for _, el in ipairs(blocks) do
    if el.t == "Table" then
      out:insert(el)
      last_table_idx = #out
    elseif el.t == "Div" then
      local size
      for _, c in ipairs(el.classes) do
        local n = c:match(sizeclass)
        -- A size this filter does not recognise is IGNORED, not clamped or
        -- guessed at: the marker div is left exactly as it is (an empty
        -- div with an unrecognised class is inert in the rendered docx,
        -- the same as before this filter existed at all), and the table
        -- keeps the plain "Table" style pandoc gave it.
        if n and valid_sizes[n] then size = n end
      end
      -- HTML's exact size, carried as an attribute because a class cannot
      -- express a fraction. Independent of `size` above: the two markers are
      -- never emitted together (R chooses one by destination), and reading
      -- both here keeps this filter correct whichever one it is handed.
      local exact = el.attributes[sizeattr]

      -- The class apaTables puts on EVERY table it
      -- sends to an HTML render, sized or not. It carries no size of its own;
      -- it is what apa.css's table-width rules are scoped to, so that an
      -- apaTables table gets them and a table the user built themselves does
      -- not. A marker carrying only this class must still be recognised --
      -- and still be CONSUMED, or it prints as an empty div.
      local is_html_marker = false
      for _, c in ipairs(el.classes) do
        if c == htmlclass then is_html_marker = true end
      end

      if (size or exact or is_html_marker) and last_table_idx then
        local tbl = out[last_table_idx]
        if is_docx and size then
          tbl.attr.attributes["custom-style"] = "Table" .. size
        end
        -- No `elseif is_html and exact` arm here: Quarto's HTML pipeline
        -- discards a Table node's own attributes -- measured at both
        -- post-quarto and pre-ast, for both a labelled float and an
        -- unlabelled table, so it is neither a stage question nor a float
        -- question. A `style` attribute assigned directly to the Table node
        -- survives inside this filter's own AST but never reaches the
        -- rendered page. Div-level attributes DO survive, so on HTML the
        -- table is wrapped in a div instead of attributed directly. Do not
        -- re-add this arm.
        --
        -- This is NOT a wrapper in the source markdown, which fails for two
        -- independent reasons, both measured:
        --   1. A wrapper in the SOURCE MARKDOWN, around the whole chunk, is
        --      seen by apaquarto's float collector, which then stops
        --      collecting the table -- moving it in the document. This
        --      div is built here, at post-quarto, AFTER collection has
        --      already run, and lives INSIDE the float the table was
        --      already collected into. Measured: the sized table stays
        --      collected at the end, alongside an unsized one.
        --   2. A wrapper setting a plain inherited size loses to Bootstrap's
        --      `.small` rule (specificity 0,1,0), which takes 0.875 of it --
        --      asked 6 pt, delivered 5.25 pt. The `.apa-table-size > table`
        --      rule in apa.css targets the table element directly at
        --      specificity (0,1,1), beating `.small`, and `font-size:
        --      inherit` takes the wrapper's exact size verbatim.
        -- Do not revert this to a plain attribute assignment on `tbl`.
        -- ONE div, whether or not a size was asked for.
        -- The `apa-table-html` class is what apa.css's width rules are scoped
        -- to; the `apa-table-size` class and the style attribute are added
        -- beside it only when a size was asked for.
        --
        -- ONE div, NOT two nested. `.apa-table-size > table` is a CHILD
        -- selector, so a second wrapper between it and the table would break
        -- an explicit font.size SILENTLY -- the size would fall through to
        -- Bootstrap's `.small`, which takes 0.875 of the inherited value
        -- (asked 6 pt, delivered 5.25 pt). Both classes go on the same div
        -- for that reason, and
        -- tests/testthat/test-html-structure.R asserts it on a real render.
        if is_html then
          local classes = {htmlclass}
          local attrs = {}
          if exact then
            table.insert(classes, "apa-table-size")
            attrs.style = "font-size: " .. exact .. "pt;"
          end
          out[last_table_idx] = pandoc.Div({tbl}, pandoc.Attr("", classes, attrs))
        else
          out[last_table_idx] = tbl
        end
        -- This if/else is the ONE place `out[last_table_idx]` is written
        -- for this marker: the `is_html` arm replaces it with the
        -- wrapper div described above, and the `else` arm writes `tbl` back
        -- unwrapped -- still carrying the docx custom-style set in the
        -- is_docx branch above, when that branch fired -- for every other
        -- case, docx included.
        -- Consumed either way, including for a format neither branch serves
        -- (e.g. a docx-only `size` marker walked in a non-docx render, or an
        -- HTML-only `exact` marker walked in a docx render): the marker div
        -- is dropped below regardless, so it never survives as dead weight
        -- in an output this filter does not act on.
        --
        -- Consumed: this marker cannot also apply to a LATER table in the
        -- same list (there is no such case in practice -- one table, one
        -- optional marker, per apa_quarto_size_wrap() -- but not relying
        -- on that from inside this filter, which is an independently
        -- loadable, general-purpose artifact, is the same discipline the
        -- valid_sizes re-check just above already applies).
        last_table_idx = nil
        changed = true
        -- The marker div itself is dropped (not re-inserted into `out`),
        -- leaving no trace in the rendered document.
      else
        -- No table has been seen yet in THIS list, or this div's class
        -- does not name a recognised size: leave the div exactly as it is.
        out:insert(el)
      end
    else
      out:insert(el)
    end
  end
  if changed then return out end
  return nil
end

local function process_pandoc(doc)
  doc.blocks = doc.blocks:walk({ Blocks = process_blocks })
  return doc
end

return { { Pandoc = process_pandoc } }
