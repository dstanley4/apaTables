-- A column spanner is CENTRED over the columns it covers (APA).
--
-- THE DEFECT. In a two-way table the spanner `alcohol` sits over `None`,
-- `2 Pints` and `4 Pints`, and those are numeric columns, so they are
-- right-aligned. Measured in a real render, the spanner printed hard against
-- the RIGHT edge of the columns it spans, in BOTH docx and html:
--
--   html   <th colspan="6" style="text-align: right;">alcohol</th>
--   docx   <w:p><w:pPr><w:pStyle w:val="ApaTableRule"/><w:jc w:val="right"/>
--
-- ONE CAUSE, NOT TWO. Both are pandoc writing from the SAME table, and a
-- pandoc grid table carries alignment PER COLUMN. A cell spanning several
-- columns takes the alignment of the first column it covers, which for a
-- spanner over numeric columns is right. Nothing in the markdown apaTables
-- emits can say otherwise: grid-table syntax has no per-cell alignment.
--
-- WHY THIS RUNS ON THE AST AND NOT IN CSS. A stylesheet rule reaches html
-- only, leaving Word wrong, and it would have to beat an inline `style=`
-- attribute. Pandoc's AST *does* carry per-cell alignment, so setting it here
-- -- before either writer runs -- fixes both destinations with one rule.
--
-- AND THE STYLE ROUTE IS RULED OUT BY MEASUREMENT. That docx paragraph
-- already carries `<w:pStyle w:val="ApaTableRule"/>`, and the `<w:jc>` beside
-- it is DIRECT formatting, which wins over any style. Putting a centred `jc`
-- into the ApaTableRule style would change nothing.
--
-- THE PDF PATH NEEDS NOTHING. apaTables emits raw
-- LaTeX there -- `\multicolumn{n}{c}{...}`, centred explicitly -- so no AST
-- table is ever built and this filter has nothing to match.
--
-- HEAD ROWS ONLY, AND THAT IS LOAD-BEARING. A `col_span > 1` cell in the
-- BODY is a row-group label (`apa_row_group()`), which is flush left and must
-- stay flush left. Centring those would silently restyle every grouped table.
if not (FORMAT == "docx" or FORMAT:match("^html")) then
  return {}
end

local function centre_head_spanners(tbl)
  local changed = false
  -- tbl.head.rows: the header rows. A spanner may sit at any level (a two-way
  -- table has two: the iv2 name over everything, then one per iv2 level).
  if tbl.head and tbl.head.rows then
    for _, row in ipairs(tbl.head.rows) do
      for _, cell in ipairs(row.cells) do
        -- `> 1` and not `~= 1`: col_span is never 0, but a guard that reads
        -- as "spans more than one column" is the requirement itself.
        if cell.col_span and cell.col_span > 1
            and cell.alignment ~= pandoc.AlignCenter then
          cell.alignment = pandoc.AlignCenter
          changed = true
        end
      end
    end
  end
  -- Returning nil leaves the element untouched, which keeps every table
  -- without a spanner byte-identical rather than rebuilt.
  if changed then return tbl end
end

return { { Table = centre_head_spanners } }
