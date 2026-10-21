-- apaTables' own docx landscape section break.
--
-- Quarto's OWN built-in `.landscape` div for docx (`/Applications/quarto/
-- share/filters/main.lua`, landscape.lua) closes its rotated section with a
-- COMPLETELY EMPTY sectPr -- no page size, no margins, no header
-- references -- so every page BEFORE a rotated table silently loses the
-- running head and page number, not merely the rotated page itself. Its
-- rotated page size is also hardcoded A4 regardless of the manuscript's own
-- paper. Both measured by rendering.
--
-- apaTables never uses `.landscape`. apa_landscape_wrap() (R/quarto-
-- table.R) emits a DIFFERENT class instead, with the document's real page
-- geometry -- read out of the bundled reference document by
-- apa_docx_reference_sectpr() (R/docx-landscape.R), never hardcoded --
-- carried as the div's own attributes:
--
--   ::: {.apa-table-landscape pgw="12240" pgh="15840" mt="1440" mb="1440"
--                              ml="1440" mr="1440" mh="720" mf="720" mg="0"}
--   <the table's own grid markdown>
--   :::
--
-- This filter finds that div and replaces it with THREE things in order: a
-- portrait closer carrying the real page (not empty), the div's own content
-- unwrapped, and a landscape closer carrying the SAME real page with width
-- and height swapped (not a hardcoded A4). Both closers carry the same
-- three header references as the document's own default section, so the
-- running head and page number survive on every page -- the rotated one and
-- every page before it.
--
-- THE MARGINS ROTATE WITH THE PAGE -- measured in real Microsoft
-- Word, not inferred. See process_div() below for the mapping and the
-- measurement.
--
-- HEADER/FOOTER RELATIONSHIP IDs ARE HARDCODED HERE, DELIBERATELY. They
-- are STABLE under pandoc's own docx writer (measured): it preserves the
-- reference document's own header/footer relationships and allocates every
-- new one ABOVE them -- a probe with twelve new relationships (three
-- images, three hyperlinks, a footnote, two citations with a bibliography,
-- three cross-references, a pipe table and a landscape div) left
-- rId8..rId13 untouched and used rId14..rId32. Since apaTables BUNDLES
-- apaquarto.docx, those IDs are ours to know, and reusing them here creates
-- no new relationship at all -- both closers simply point at the same
-- header/footer parts every other section already uses.
--
-- FOUND BY A Pandoc-LEVEL WALK, NOT A FloatRefTarget HANDLER, AND THAT IS
-- DELIBERATE. apatablesize.lua's sibling marker div is always inside a
-- `tbl-` labelled chunk's float, so a FloatRefTarget handler is enough
-- there. This div is not: apa_landscape_wrap() wraps whatever markdown the
-- chunk emitted regardless of whether that chunk has a crossref-able
-- label, so it can end up nested inside a FloatRefTarget's content (a
-- labelled chunk, later deferred to the document's end by
-- apafloatstoend.lua, as apaquarto does by default) OR sitting directly in
-- the ordinary block list (an unlabelled chunk, never a float at all).
-- MEASURED: `doc.blocks:walk{Div = fn}` from a `Pandoc`
-- handler fires in BOTH cases with no branch needed -- pandoc's own walk
-- recurses into a FloatRefTarget's content automatically. A handler scoped
-- to FloatRefTarget only would silently do nothing for the unlabelled case.
--
-- ALSO MEASURED: this combines cleanly with
-- apatablesize.lua's shrink ladder running in the same render, in
-- either registration order -- the two filters touch disjoint things (a
-- Table's custom-style attribute and its own sibling marker div; this
-- one's own div and nothing else), so a table that is BOTH rotated AND
-- shrunk came out with the right table style AND the right section breaks,
-- verified by rendering to a real .docx and reading word/document.xml back.
--
-- NUMBERED-LINES INTERACTION. apaquarto's docxlinenumber.lua turns
-- `numbered-lines: true` into line numbering by appending ONE closer of its
-- own, at the very end of the document, carrying `<w:lnNumType>` and
-- nothing else -- no page size, no margins, no header references. A
-- `sectPr` governs content back to the PREVIOUS closer, so the two closers
-- this filter inserts put the pre-rotation content and the rotated table
-- in sections docxlinenumber.lua's closer never reaches. MEASURED in real
-- Word (16.111) without the handling below: no line numbers on the pages
-- this filter's two sections govern, and a counter starting fresh at "1"
-- on the first page docxlinenumber.lua's closer governs. So this filter
-- reads `numbered-lines` off the document metadata (the same key and
-- stringify check docxlinenumber.lua itself uses) and carries
-- `<w:lnNumType>` in BOTH of its own closers when set; real Word then
-- numbers continuously from page 1 through the rotated table and beyond.
--
-- NOT A DEFECT, DESPITE APPEARANCES: docxlinenumber.lua's own closer ALSO
-- omits headerReference and pgSz, the same textual shape as Quarto's empty
-- `.landscape` sectPr above -- but MEASURED IN REAL MICROSOFT WORD, the
-- running head, page number and correct page size ALL appear on every page
-- that closer governs, with or without a rotated table elsewhere. Word
-- treats an "incomplete but not entirely empty" sectPr differently from
-- Quarto's `.landscape` sectPr, which has ZERO child elements. The
-- mechanism is not explained here and nothing in this filter depends on
-- it; the XML shape alone is not evidence of a defect.
--
-- IMPORTANT: this script returns EXACTLY the plain, universal apaquarto
-- filter shape -- an ARRAY of filter passes, each with only node-type keys,
-- `{ { Meta = read_meta }, { Pandoc = process_pandoc } }` -- the same
-- two-pass shape apafloatlatex.lua already uses to read `documentmode`
-- before its own FloatRefTarget pass runs. A filter returning a table with
-- a stray non-array key is silently SKIPPED by Quarto's own filter loader
-- (main.lua) -- confirmed more than once -- while
-- bare `pandoc --lua-filter=` and `pandoc lua` both tolerate the same stray
-- key, so verifying against either is not verifying; this was checked only
-- through an end-to-end `quarto render`.

if FORMAT ~= "docx" then
  return
end

local classname = "apa-table-landscape"

-- Set by read_meta() before the Pandoc pass runs (see the file banner's
-- "NUMBERED-LINES INTERACTION" note). Same key and stringify check
-- docxlinenumber.lua itself uses, so the two filters agree on what
-- "numbered-lines: true" means.
local numbered_lines = false

local function read_meta(meta)
  if meta["numbered-lines"] and
      pandoc.utils.stringify(meta["numbered-lines"]) == "true" then
    numbered_lines = true
  end
end

-- rId8/rId9/rId12 (headers) and rId10/rId11/rId13 (footers) -- see the file
-- banner for why these are safe to hardcode.
local REFS = {
  { tag = "headerReference", type = "even",    id = "rId8"  },
  { tag = "headerReference", type = "default", id = "rId9"  },
  { tag = "footerReference", type = "even",    id = "rId10" },
  { tag = "footerReference", type = "default", id = "rId11" },
  { tag = "headerReference", type = "first",   id = "rId12" },
  { tag = "footerReference", type = "first",   id = "rId13" },
}

local function refs_xml()
  local out = {}
  for _, r in ipairs(REFS) do
    out[#out + 1] = string.format('<w:%s w:type="%s" r:id="%s"/>',
                                  r.tag, r.type, r.id)
  end
  return table.concat(out)
end

-- One section-closing paragraph: <w:p><w:pPr><w:sectPr>...</w:sectPr>
-- </w:pPr></w:p>, the OOXML idiom that ends the section everything back to
-- the PREVIOUS closer (or the start of the document) belongs to. `w`/`h`
-- are handed in already in the order this section wants them -- the
-- landscape closer's caller passes them swapped, this function does not
-- swap anything itself.
local function sectpr_xml(a, w, h, landscape)
  local pgsz
  if landscape then
    pgsz = string.format('<w:pgSz w:w="%s" w:h="%s" w:orient="landscape"/>',
                         w, h)
  else
    pgsz = string.format('<w:pgSz w:w="%s" w:h="%s"/>', w, h)
  end
  local pgmar = string.format(
    '<w:pgMar w:top="%s" w:right="%s" w:bottom="%s" w:left="%s" ' ..
    'w:header="%s" w:footer="%s" w:gutter="%s"/>',
    a["mt"], a["mr"], a["mb"], a["ml"], a["mh"], a["mf"], a["mg"])
  -- Same element and attribute values docxlinenumber.lua itself writes,
  -- so numbering is continuous across every section of the document
  -- rather than restarting (or stopping) at this closer.
  local lnnum = numbered_lines
    and '<w:lnNumType w:countBy="1" w:restart="continuous" />' or ""
  -- The reference document's OWN trailing sectPr carries <w:cols
  -- w:space="720"/> (read straight out of apaquarto.docx's
  -- word/document.xml). Word defaults to a single column when the element
  -- is absent, so omitting it changed nothing visible -- but these closers
  -- claim to carry "the document's real section properties", and this is
  -- one of them. Emitted AFTER lnNumType because CT_SectPr is a strict
  -- SEQUENCE (... pgMar, paperSrc, pgBorders, lnNumType, pgNumType, cols
  -- ...), the same schema-order trap as <w:rPr> in the docx styles: an
  -- element in the wrong position is not merely ignored, it can
  -- make Word reject the part.
  local cols = '<w:cols w:space="720"/>'
  return "<w:p><w:pPr><w:sectPr>" .. refs_xml() .. pgsz .. pgmar .. lnnum ..
    cols .. "</w:sectPr></w:pPr></w:p>"
end

local function process_div(div)
  if not div.classes:includes(classname) then
    return nil
  end
  local a = div.attributes
  local w, h = a["pgw"], a["pgh"]
  -- Every attribute apa_landscape_wrap() writes is required. Missing any of
  -- them means the div was not built by that function (or R declined to
  -- rotate because it could not read the reference document's geometry and
  -- left the table unwrapped instead -- in which case this div does not
  -- exist at all). Either way, guessing a page size here would repeat
  -- Quarto's own defect (file banner), so this leaves the div's content
  -- untouched rather than writing a section with invented numbers.
  if not (w and h and a["mt"] and a["mb"] and a["ml"] and a["mr"]
          and a["mh"] and a["mf"] and a["mg"]) then
    return nil
  end
  local closer_portrait  = pandoc.RawBlock("openxml", sectpr_xml(a, w, h, false))
  -- THE MARGINS ROTATE WITH THE PAGE. MEASURED IN REAL MICROSOFT WORD, not
  -- inferred from the numbers: a portrait section with four
  -- DISTINCT margins, flipped to landscape by Word itself, went from
  --   <w:pgMar w:top="1440" w:right="1440" w:bottom="1080" w:left="2160"/>
  -- to
  --   <w:pgMar w:top="1440" w:right="1080" w:bottom="2160" w:left="1440"/>
  -- i.e. new top <- old right, new right <- old bottom, new bottom <- old
  -- left, new left <- old top. `bottom` and `left` were unique values, so
  -- this is the only mapping consistent with the result -- the clockwise
  -- alternative would have put 2160 in `top` and did not.
  --
  -- apaquarto.docx's own margins are symmetric (1440 all round), so this is
  -- a NO-OP for the bundled reference document. It matters for a user who
  -- supplies their own, which is supported -- and an unequal margin is
  -- almost always a physical constraint (a thesis binding edge, a printer's
  -- gripper margin) that a rotation has to carry round with the page.
  -- Leaving them alone is a DIVERGENCE FROM WORD, not neutrality: the same
  -- reference document would give one page to a user who rotates a section
  -- by hand and a different one to a table apaTables rotates.
  --
  -- Swapped HERE, at the call site, exactly as w/h already are: sectpr_xml()
  -- deliberately transforms nothing itself.
  local rotated_margins = {
    mt = a["mr"], mr = a["mb"], mb = a["ml"], ml = a["mt"],
    mh = a["mh"], mf = a["mf"], mg = a["mg"],
  }
  local closer_landscape = pandoc.RawBlock("openxml",
                                           sectpr_xml(rotated_margins, h, w, true))
  local out = pandoc.Blocks({ closer_portrait })
  out:extend(div.content)
  out:insert(closer_landscape)
  return out
end

local function process_pandoc(doc)
  doc.blocks = doc.blocks:walk({ Div = process_div })
  return doc
end

return {
  { Meta = read_meta },
  { Pandoc = process_pandoc }
}
