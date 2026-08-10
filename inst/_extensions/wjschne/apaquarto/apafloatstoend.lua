if FORMAT == "latex" then
  return
end
-- Is this block one of the page breaks emitted above, rather than a raw block
-- the document itself contains?
local function is_own_page_break(b)
  if not b or b.t ~= "RawBlock" then return false end
  if b.format == "openxml" then
    return b.text:find('w:type="page"', 1, true) ~= nil
  end
  if b.format == "typst" then
    return b.text:find("pagebreak", 1, true) ~= nil
  end
  return false
end

Pandoc = function(doc)
  local tbl = {}
  local fig = {}
  local appendixword = "Appendix"
  local movefloatstoend = true
  if doc.meta.language and doc.meta.language["crossref-apx-prefix"] then
    appendixword = pandoc.utils.stringify(doc.meta.language["crossref-apx-prefix"])
  end


  if doc.meta.floatsintext and pandoc.utils.stringify(doc.meta.floatsintext) == "true" then
    movefloatstoend = false
  end


  if movefloatstoend then
    for i = #doc.blocks, 1, -1 do
      if doc.blocks[i].identifier then
        if doc.blocks[i].identifier:find("^tbl%-") then
          if doc.blocks[i].attributes and doc.blocks[i].attributes.prefix == "" then
            table.insert(tbl, 1, doc.blocks[i])
            if FORMAT == "docx" then
              table.insert(tbl, 1, pandoc.RawBlock('openxml', '<w:p><w:r><w:br w:type="page"/></w:r></w:p>'))
            end
            if FORMAT == "typst" then
              table.insert(tbl, 1, pandoc.RawBlock('typst', '#pagebreak(weak: true)'))
            end
            doc.blocks:remove(i)
          end
        else
          if doc.blocks[i].identifier:find("^fig%-") then
            if doc.blocks[i].attributes and doc.blocks[i].attributes.prefix == "" then
              table.insert(fig, 1, doc.blocks[i])
              if FORMAT == "docx" then
                table.insert(fig, 1, pandoc.RawBlock('openxml', '<w:p><w:r><w:br w:type="page"/></w:r></w:p>'))
              end
              if FORMAT == "typst" then
                table.insert(fig, 1, pandoc.RawBlock('typst', '#pagebreak(weak: true)'))
              end
              doc.blocks:remove(i)
            end
          else
            local hasfig = false
            doc.blocks[i]:walk {
              Figure = function(fg)
                if fg.identifier then
                  if fg.identifier:find("^fig%-") then
                    if fg.attributes and fg.attributes.prefix == "" then
                      hasfig = true
                    end
                  end
                end
              end
            }
            if hasfig then
              table.insert(fig, 1, doc.blocks[i])
              if FORMAT == "docx" then
                table.insert(fig, 1, pandoc.RawBlock('openxml', '<w:p><w:r><w:br w:type="page"/></w:r></w:p>'))
              end
              if FORMAT == "typst" then
                table.insert(fig, 1, pandoc.RawBlock('typst', '#pagebreak(weak: true)'))
              end
              doc.blocks:remove(i)
              hasfig = false
            else

            end
          end
        end
      end
    end
  end


  -- Insert page breaks for each appendix in docx and typst
  -- html does not need page breaks, and latex inserts pagebreaks automatically
  if FORMAT == "docx" or FORMAT == "typst" then
    for i = #doc.blocks, 1, -1 do
      if doc.blocks[i].tag == "Header" then
        if doc.blocks[i].level == 1 and doc.blocks[i].content[1].text == appendixword then
          if FORMAT == "docx" then
            table.insert(doc.blocks, i, pandoc.RawBlock('openxml', '<w:p><w:r><w:br w:type="page"/></w:r></w:p>'))
          end
          if FORMAT == "typst" then
            table.insert(doc.blocks, i, pandoc.RawBlock('typst', '#pagebreak(weak: true)'))
          end
        end
      end
    end
  end

  if movefloatstoend then
    -- Find block where appendices begin
    local appendixblock = 0
    for i = 1, #doc.blocks, 1 do
      if doc.blocks[i].tag == "Header" then
        if doc.blocks[i].level == 1 and doc.blocks[i].content[1].text == appendixword and appendixblock == 0 then
          appendixblock = i
        end
      end
    end

    -- If there are no appendices, insert figures and tables at the end
    if appendixblock == 0 then
      if #tbl > 0 then
        doc.blocks:extend(tbl)
      end
      if #fig > 0 then
        doc.blocks:extend(fig)
      end
    else
      -- Insert figures and tables before appendices.
      -- Each float now carries its own leading page break, so the floats must
      -- go BEFORE the break that was inserted for the appendix heading,
      -- otherwise that break is doubled and opens a blank page.
      --
      -- Test for THIS filter's own break, not merely for a RawBlock. Only docx
      -- and typst get one, and any raw block a document itself places before
      -- an appendix heading -- a raw HTML block, say -- would otherwise be
      -- mistaken for it and the floats inserted on the wrong side of it.
      if appendixblock > 1 and is_own_page_break(doc.blocks[appendixblock - 1]) then
        appendixblock = appendixblock - 1
      end
      if #fig > 0 then
        for i = #fig, 1, -1 do
          doc.blocks:insert(appendixblock, fig[i])
        end
      end
      if #tbl > 0 then
        for i = #tbl, 1, -1 do
          doc.blocks:insert(appendixblock, tbl[i])
        end
      end
    end
  end

  return doc
end
