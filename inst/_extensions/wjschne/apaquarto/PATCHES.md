# Local patches to apaquarto carried by apaTables

GENERATED from apaTables' patch branch of apaquarto -- do not edit by hand.

Upstream base: `main` @ 9a2f392
Bundle branch: `apatables-bundle` @ 342f81e

NOT YET OFFERED UPSTREAM. Every patch below is local to apaTables: none
has been pushed anywhere and no pull request has been opened. This bundle
exists so that apaTables users get correct tables today, not so that
apaTables carries a permanent fork -- the intention is to offer these
changes to the upstream project so the diff below SHRINKS over time.

**Do not run `quarto add` or `quarto update` in a project using these
files** -- both silently revert the extension to stock while printing
`[No Change]`. If you did, run `apa.quarto.setup()` to restore it.

## Summary

```
 _extensions/apaquarto/_extension.yml               |  70 +-
 _extensions/apaquarto/apa.css                      |  96 +++
 _extensions/apaquarto/apafloatlatex.lua            | 751 ++++++++++++++++++++-
 _extensions/apaquarto/apafloatstoend.lua           |  32 +-
 _extensions/apaquarto/apalandscapelatex.lua        |  98 +++
 _extensions/apaquarto/apalongtablenote.lua         | 729 ++++++++++++++++++++
 _extensions/apaquarto/apaquarto.docx               | Bin 33069 -> 30253 bytes
 _extensions/apaquarto/apatablelandscape.lua        | 240 +++++++
 _extensions/apaquarto/apatablelandscapecaption.lua | 190 ++++++
 _extensions/apaquarto/apatablelandscapenote.lua    |  80 +++
 _extensions/apaquarto/apatablemeta-decide.lua      |  16 +
 _extensions/apaquarto/apatablemeta.lua             |  63 ++
 _extensions/apaquarto/apatablesize.lua             | 248 +++++++
 _extensions/apaquarto/apatablespanner.lua          |  60 ++
 _extensions/apaquarto/apatemplate.tex              | 230 ++++++-
 _extensions/apaquarto/doc-class.tex                |   4 +
 _extensions/apaquarto/utilsapa.lua                 |  23 +
 tests/landscape-floats.qmd                         |  52 ++
 tests/longtable-counter.qmd                        |  58 ++
 19 files changed, 3015 insertions(+), 25 deletions(-)
```

## Full diff

```diff
diff --git a/_extensions/apaquarto/_extension.yml b/_extensions/apaquarto/_extension.yml
index 0f58672..2f8bb5a 100644
--- a/_extensions/apaquarto/_extension.yml
+++ b/_extensions/apaquarto/_extension.yml
@@ -1,6 +1,6 @@
 title: My Document in APA Style, Seventh Edition
 author: W. Joel Schneider
-version: 5.0.18
+version: 5.0.18+apaTables.34
 quarto-required: ">=1.4.549"
 contributes:
   formats:
@@ -71,6 +71,9 @@ contributes:
         # Move figures and tables to end of document
         - at: pre-ast
           path: apafloatstoend.lua
+        # Landscape (rotated) floats in latex when floats move to the end
+        - at: pre-ast
+          path: apalandscapelatex.lua
  
         # Set latex document mode
         - at: pre-ast
@@ -114,6 +117,9 @@ contributes:
         # Inserts appendix prefixes to tables and figures
         - at: pre-quarto
           path: apafigtblappendix.lua
+        # Fill in a title and note apaTables generated, where the author gave none
+        - at: post-quarto
+          path: apatablemeta.lua
         # Process tables and figures in latex
         - at: post-quarto
           path: apafloatlatex.lua
@@ -144,6 +150,11 @@ contributes:
         # Process appendices in latex
         - at: post-render
           path: apaappendixlatex.lua
+        # Attach a long table's apa-note to the table itself (latex).
+        # MUST stay after citeprocr.lua: the note is written out as LaTeX
+        # here, so any citation in it has to be resolved first.
+        - at: post-render
+          path: apalongtablenote.lua
     native: default
     html:
       toc: true
@@ -163,6 +174,30 @@ contributes:
       knitr: 
         opts_chunk: 
           dev: svglite
+      filters:
+        # Without this, apatablesize.lua (registered below under
+        # docx: only) never loads on an HTML render, so its is_html branch is
+        # unreachable and the {apa-table-size-pt="N"} marker div leaks into
+        # the rendered output unconsumed instead of being turned into a size
+        # and dropped. Found by a real `quarto render`
+        # (test-font-size-render.R); no unit test or bare-pandoc check could
+        # see it, because both bypass Quarto's own per-format filter
+        # registration entirely.
+        #
+        # What that branch does: setting the size as an attribute on the
+        # <table> node is MEASURED to be silently discarded by Quarto's HTML
+        # pipeline, so the branch wraps the table in a div instead, created here
+        # at post-quarto -- after apaquarto's float collection, so table
+        # placement is untouched -- and apa.css carries the one rule that
+        # makes the table take that size verbatim.
+        - at: post-quarto
+          path: apatablesize.lua
+        # A column spanner is CENTRED over the columns it covers. Pandoc
+        # gives a spanning cell the alignment of the first
+        # column it covers, which over numeric columns is RIGHT. Set on the
+        # AST, so the same one rule corrects docx as well.
+        - at: post-quarto
+          path: apatablespanner.lua
     pdf:
       documentclass: apa7
       pdf-engine: xelatex
@@ -179,10 +214,41 @@ contributes:
       classoption: "colorlinks=true,linkcolor=blue,citecolor=blue,urlcolor=blue"
     docx: 
       reference-doc: apaquarto.docx
-      filters: 
+      filters:
         # Add custom styles to docx
         - at: post-quarto
           path: docxstyler.lua
+        # Select the table-size ladder style (Table6-Table9, Table11, Table12)
+        - at: post-quarto
+          path: apatablesize.lua
+        # A column spanner is CENTRED over the columns it covers -- the
+        # same filter and the same reason as the html block
+        # above. Word receives <w:jc w:val="right"/> as DIRECT formatting on
+        # the spanner paragraph, which beats the ApaTableRule style, so this
+        # cannot be done in the reference document.
+        - at: post-quarto
+          path: apatablespanner.lua
+        # apaTables' own landscape section break, replacing Quarto's
+        # own broken .landscape div for tables the layout ladder rotates
+        - at: post-quarto
+          path: apatablelandscape.lua
+        # Moves a rotated table's note inside its own landscape section --
+        # apanote.lua (common, post-render, above) puts the note AFTER the
+        # closer apatablelandscape.lua already wrote, which strands it on
+        # the following PORTRAIT page (see this file's own banner)
+        - at: post-render
+          path: apatablelandscapenote.lua
+        # Move a rotated table's caption inside its own landscape
+        # section, and keep a RUN of consecutive rotated tables in ONE section.
+        # Kept after apatablelandscapenote.lua, which identifies a rotated
+        # float by its TRAILING landscape closer -- the one this filter
+        # removes when it merges a run. MEASURED: reversing the two changes
+        # only where a zero-width bookmarkEnd lands, because a note the note
+        # filter then fails to relocate is already inside the section that
+        # merging left open. See that filter's banner; the order is retained
+        # because it is what is verified, not because reversing it breaks.
+        - at: post-render
+          path: apatablelandscapecaption.lua
         # Add line numbers and format Latex symbol
         - at: post-render
           path: docxlinenumber.lua
diff --git a/_extensions/apaquarto/apa.css b/_extensions/apaquarto/apa.css
index 1e4caf8..e625718 100644
--- a/_extensions/apaquarto/apa.css
+++ b/_extensions/apaquarto/apa.css
@@ -87,6 +87,34 @@ table {
   border-bottom: none;
 }
 
+/* apaTables: a spanner's cmidrule. apa_markdown_table() wraps ONLY the
+   spanned header cells of a grid table in a
+   "::: {custom-style=\"ApaTableRule\"}" fenced div -- pandoc's HTML
+   writer turns that into a <div data-custom-style="ApaTableRule"> nested
+   inside the <th>, one per spanner, so this rule draws a partial rule under
+   exactly the columns each spanner covers. Measured: without this rule the
+   div carries no visible border at all -- the ordinary table rules
+   above (top of table, bottom of thead, bottom of table, no vertical rules,
+   no interior row rules) already come from apaquarto's own .table CSS with
+   no per-cell markup, so this is the only piece apaTables' HTML output adds. */
+[data-custom-style="ApaTableRule"] {
+  border-bottom: 1px solid black;
+}
+
+/* apaTables: an explicit font.size on an HTML render is
+   delivered by wrapping the <table> in a <div class="apa-table-size"
+   style="font-size: Npt;"> (apatablesize.lua) rather than attributing the
+   <table> node directly -- Quarto's HTML pipeline discards a Table node's
+   own attributes before the page is written, measured at both post-quarto
+   and pre-ast, for both a labelled float and an unlabelled table. Without
+   this rule the table would merely INHERIT the wrapper's size, and
+   Bootstrap's `.small` rule (specificity 0,1,0; apaquarto stamps every
+   table `table-sm ... small`) takes 0.875 of any inherited size -- asked
+   8.5 pt, delivered 7.4375 pt, silently. This rule's selector
+   (specificity 0,1,1) outranks `.small` and `inherit` takes the wrapper's
+   pt value verbatim, so the table gets EXACTLY what was requested. */
+.apa-table-size > table { font-size: inherit; }
+
 .tabwid {padding: 0}
 
 .AbstractFirstParagraph p {text-indent: 0}
@@ -95,3 +123,71 @@ table {
   font-family: Consolas, "Lucida Console", "Courier New", monospace;
   font-size: 10pt;
 }
+
+/* apaTables: without these rules a table wider than the text column is
+   SQUASHED into it, breaking cells mid-value -- a confidence interval printing
+   as `[22.72,` on one line and `59.53]` on the next. Three things combine, and
+   no one of them alone is the fault: apaquarto sets `grid: body-width: "7in"`
+   (_extension.yml), which Quarto turns into a 6.50in text column that does not
+   move with the window; Bootstrap's `.table{width:100%}` stretches every table
+   to it; and the <colgroup> percentages pandoc writes come from the grid table
+   apaTables builds for the PRINTED page, so a column gets less than its
+   contents need and the cell breaks at the only place it can.
+
+   MEASURED on the shipped example at nine window widths, 500-1600px: 92 pieces
+   of broken text at 1440px before these rules, ZERO after, and the page never
+   scrolls sideways at any width.
+
+   SCOPED to `.apa-table-html`, the class apaTables puts on its OWN tables
+   (R/quarto-table.R's APA_QUARTO_HTML_CLASS -> apatablesize.lua's wrapper).
+   Rule 1 overrides column widths, and a table the USER built may carry widths
+   they set deliberately. Do not widen these selectors to `.table`.
+
+   NOT Quarto's own `column-page-right`, which was measured and does work -- it
+   also stretches a table that already FITS from 6.50in to 9.88in, and pinning
+   that back needs apaquarto's body-width written down a second time, here,
+   where the two copies would drift. */
+
+/* 1. Columns size to their own contents, not to the printed page's
+      proportions. */
+.apa-table-html > table.table > colgroup > col { width: auto !important; }
+
+/* 2. The table takes its natural width, and never less than the text column --
+      so the tables that already fit are pixel-identical to before. */
+.apa-table-html > table.table { width: max-content !important; min-width: 100%; }
+
+/* 3. No room to grow into: the table's own box scrolls. Never the page. */
+.apa-table-html { max-width: 100%; overflow-x: auto; }
+
+/* 3b. Room to grow into: the box grows to the RIGHT, into margin that is empty
+       anyway. Right-only, not centred -- a centred overhang was measured to
+       print on top of the table of contents at 1440px.
+       `:has()` is what lets the table overhang the float's own output box,
+       which Quarto gives `overflow-x: auto`. Scoped to a box that CONTAINS one
+       of our tables, deliberately: turning that overflow off for every
+       `.cell-output-display` would reach a user's own wide output and take
+       away Quarto's scroll affordance for it. Where `:has()` is unsupported
+       the page degrades to rule 3 -- the table scrolls in its own box, still
+       whole, still unwrapped, never squashed. */
+@media (min-width: 992px) {
+  .cell-output-display:has(> .apa-table-html) { overflow-x: visible; }
+  .apa-table-html {
+    width: max-content;
+    min-width: 100%;
+    /* HOW FAR A TABLE MAY REACH INTO THE MARGIN, and it must be BOUNDED.
+       An unbounded overhang was measured pushing the whole PAGE sideways for a
+       20-variable correlation table -- natural width 14.42in against a 6.50in
+       column -- which is the one thing the behaviour must never do. The bound
+       is expressed RELATIVE to the text column, so it needs no knowledge of
+       the window or of any table: 1.3in is what still fits in the margin at
+       the narrowest window this branch applies to (measured at 1000, 1050 and
+       1100px, where the room to the right is 8.2in and this box is 7.8in).
+       A table needing MORE than this scrolls in its own box instead -- the
+       same fallback a narrow window gets, reached for the same reason: there
+       is no room. The two tables in the shipped example need 1.09in and
+       1.17in. NOT a table measurement and NOT the width model: one
+       page-layout constant, applied to every table alike. */
+    max-width: calc(100% + 1.3in);
+    overflow-x: auto;
+  }
+}
diff --git a/_extensions/apaquarto/apafloatlatex.lua b/_extensions/apaquarto/apafloatlatex.lua
index 820933f..f33f35a 100644
--- a/_extensions/apaquarto/apafloatlatex.lua
+++ b/_extensions/apaquarto/apafloatlatex.lua
@@ -30,12 +30,19 @@ end
 -- Is the .pdf in journal mode?
 local journalmode = false
 local manuscriptmode = true
+local docmode = false
+-- Are floats collected at the end? Computed by utilsapa.delayedfloats, the
+-- same function apalandscapelatex.lua calls, so the two filters cannot
+-- disagree about which placement is active.
+local delayedfloats = true
 local noteprefix = "\\noindent\\textit{Note.}"
 local beforenote = ""
 local getmode = function(meta)
   local documentmode = pandoc.utils.stringify(meta["documentmode"])
   journalmode = documentmode == "jou"
   manuscriptmode = documentmode == "man"
+  docmode = documentmode == "doc"
+  delayedfloats = utilsapa.delayedfloats(meta)
   -- Find word for "note"
   if not meta.language["figure-table-note"] then
     if param("callout-note-title") then
@@ -60,6 +67,546 @@ end
 --  return result
 --end
 
+-- Does this float's content contain a longtable?
+--
+-- A longtable CANNOT live inside a LaTeX float: \begin{table} makes its
+-- contents unbreakable, so every row past the bottom of the page is dropped
+-- SILENTLY -- no error, no warning, a document that compiles. Measured on a
+-- 60-row table: 55 rows survive inside the float and the table's note never
+-- reaches the page at all; outside it, 60 of 60 rows and the note attached.
+-- Detecting a longtable body is therefore not a heuristic about what the
+-- author probably wanted; it is reading a fact about what LaTeX can typeset.
+-- That is why this replaces the `apa-longtable` chunk option: nothing about
+-- the decision needed the author's help.
+--
+-- WHICH ENVIRONMENT NAME. At this filter's stage (post-quarto) the block is
+-- still the unstarred \begin{longtable} -- measured by logging every RawBlock
+-- reaching processfloat. Quarto's own main.lua rewrites a captionless raw
+-- longtable to longtable* LATER, in the writer, so the starred name never
+-- arrives here from a Quarto-produced table. The starred name is matched
+-- anyway, and deliberately: it costs one alternation, and if a hand-written
+-- or third-party starred longtable ever does arrive, sealing it in a float
+-- loses rows invisibly, whereas letting it through loses only the caption
+-- Quarto declined to splice -- a visible defect rather than a silent one.
+--
+-- A MARKDOWN TABLE IS A LONGTABLE TOO. It is still a pandoc Table here and
+-- becomes LaTeX only in the writer, which always writes a table as a
+-- longtable. Since Pandoc 3.8 (Quarto 1.9) a captioned one arrives wrapped in
+-- \begin{table}, and the loss is the one described above: a 90-row table
+-- stopped at the foot of its first page after 44 to 48 rows, depending on the
+-- document mode, in both placements. Lifted out, the writer puts the caption inside
+-- the longtable (the shape apaTables' own tables have), so numbering and
+-- cross-references are unchanged. apalongtablenote.lua then makes its own
+-- closing rule stand in for the template's and carries its note inside it.
+--
+-- ONLY A FLOAT WHOSE WHOLE CONTENT IS ONE TABLE. Lifted, a float with two
+-- tables (or a panel of subtables) prints each table with the float's
+-- caption and its own number, so the cross-reference points at the second
+-- and every later table is numbered one too high -- silently; and a float
+-- holding a paragraph as well as its table loses the paragraph to the body
+-- text when tables go to the end, since only the table environment travels.
+-- Such floats are left to the float path, as before.
+local function onlytable(blocks)
+  local tables, other = 0, 0
+  local function scan(bs)
+    for _, b in ipairs(bs) do
+      if b.t == "Table" then
+        tables = tables + 1
+      elseif b.t == "Div" then
+        scan(b.content)
+      elseif b.t == "RawBlock" then
+        if b.text:match("%S") then other = other + 1 end
+      elseif (b.t == "Plain" or b.t == "Para") then
+        if #b.content > 0 then other = other + 1 end
+      else
+        other = other + 1
+      end
+    end
+  end
+  scan(blocks)
+  return tables == 1 and other == 0
+end
+
+-- A raw LaTeX tabular (knitr::kable(format = "latex"), kableExtra) as the
+-- float's only content becomes the longtable kable itself writes with
+-- longtable = TRUE: the same environment, rules and rows, so it takes the
+-- longtable path below like every other table. Left as a tabular it would
+-- sit in a float, which drifts away from its place in the text in
+-- documentmode: doc, and whose manuscript-mode spacing is tuned for
+-- double-spaced content: the title is struck through by the top rule and
+-- the note prints over the last row. Only the outermost tabular is renamed;
+-- a tabular nested in a cell (a hand-written multi-line cell) stays as it
+-- is. \centering goes because an APA table is set flush left, and a
+-- position argument such as [t] goes because longtable reads that slot as
+-- alignment. kableExtra's font_size (a \begingroup\fontsize{s}{b}\selectfont
+-- ... \endgroup wrapper) is moved inside the table the way apaTables sets
+-- its own sizes: a group in front of the longtable would shrink the caption
+-- and is dropped when tables are collected at the end of the document, so
+-- the size goes in front of every column and the row strut is rebuilt at
+-- that size (see the R function apa_latex_strut_fix()).
+-- Put ">{size}" in front of every column of a tabular's column spec
+-- (spec includes its outer braces). Returns nil for a spec it cannot read.
+local sizedcolspec = function(spec, size)
+  local s = spec:sub(2, -2)
+  local out, i, prefixed = {}, 1, false
+  local function group(at)
+    local g = s:match("^%b{}", at)
+    return g
+  end
+  while i <= #s do
+    local ch = s:sub(i, i)
+    if ch:match("%s") or ch == "|" then
+      out[#out + 1] = ch
+      i = i + 1
+    elseif ch == "@" or ch == "!" or ch == "<" then
+      local g = group(i + 1)
+      if not g then return nil end
+      out[#out + 1] = ch .. g
+      i = i + 1 + #g
+    elseif ch == ">" then
+      local g = group(i + 1)
+      if not g then return nil end
+      if not prefixed then out[#out + 1] = ">{" .. size .. "}" end
+      prefixed = true
+      out[#out + 1] = ch .. g
+      i = i + 1 + #g
+    elseif ch:match("[lcr]") then
+      if not prefixed then out[#out + 1] = ">{" .. size .. "}" end
+      out[#out + 1] = ch
+      prefixed = false
+      i = i + 1
+    elseif ch:match("[pmb]") then
+      local g = group(i + 1)
+      if not g then return nil end
+      if not prefixed then out[#out + 1] = ">{" .. size .. "}" end
+      out[#out + 1] = ch .. g
+      prefixed = false
+      i = i + 1 + #g
+    else
+      return nil
+    end
+  end
+  return "{" .. table.concat(out) .. "}"
+end
+
+-- The number of columns in a tabular's column spec, or nil if unread.
+local speccolumns = function(spec)
+  local s = spec:sub(2, -2):gsub("[@!<>]%b{}", "")
+  if s:find("%*") then return nil end
+  local n, i = 0, 1
+  while i <= #s do
+    local ch = s:sub(i, i)
+    if ch:match("[lcr]") then
+      n = n + 1
+      i = i + 1
+    elseif ch:match("[pmb]") then
+      local g = s:match("^%b{}", i + 1)
+      if not g then return nil end
+      n = n + 1
+      i = i + 1 + #g
+    elseif ch:match("[%s|]") then
+      i = i + 1
+    else
+      return nil
+    end
+  end
+  return n > 0 and n or nil
+end
+
+local rawtabular_to_longtable = function(text)
+  local body = text:match("^%s*(.-)%s*$")
+  body = body:gsub("^\\centering%s*", "")
+  local size, inbody = body:match(
+    "^\\begingroup%s*(\\fontsize%b{}%b{}\\selectfont)%s*(.-)%s*\\endgroup%s*{?}?$")
+  if size then
+    body = inbody
+  end
+  local spec, inner = body:match("^\\begin{tabular}%[[^%]]*%]%s*(%b{})(.*)\\end{tabular}$")
+  if not spec then
+    spec, inner = body:match("^\\begin{tabular}%s*(%b{})(.*)\\end{tabular}$")
+  end
+  if not spec then
+    return nil
+  end
+  local strut = ""
+  if size then
+    spec = sizedcolspec(spec, size)
+    if not spec then
+      return nil
+    end
+    strut = "\\noalign{\\makeatletter" .. size ..
+        "\\global\\setbox\\@arstrutbox\\hbox{\\vrule height\\arraystretch" ..
+        "\\ht\\strutbox depth\\arraystretch\\dp\\strutbox width\\z@}" ..
+        "\\makeatother}\n"
+  end
+  -- The outer tabular must enclose everything: two tabulars side by side
+  -- also start and end with these words, and would become one broken table.
+  local depth = 0
+  for word in inner:gmatch("\\(%a+){tabular}") do
+    depth = depth + (word == "begin" and 1 or (word == "end" and -1 or 0))
+    if depth < 0 then
+      return nil
+    end
+  end
+  if depth ~= 0 then
+    return nil
+  end
+  -- The header: from the start to the rule under the last header row
+  -- (booktabs' \midrule; for a plain kable table, the first \hline after
+  -- a row). Repeated as the head of every continuation page, as Pandoc
+  -- does for a markdown table; a table whose header cannot be found is
+  -- left without one and still breaks across pages. (A plain kable table
+  -- printed without column names looks the same as one with them, so its
+  -- first row is repeated.)
+  local lines = {}
+  for line in (inner .. "\n"):gmatch("(.-)\n") do
+    lines[#lines + 1] = line
+  end
+  local seenrow, headend = false, nil
+  for i, line in ipairs(lines) do
+    local trimmed = line:match("^%s*(.-)%s*$")
+    if trimmed == "\\midrule" or (seenrow and trimmed == "\\hline") then
+      headend = i
+      break
+    end
+    if trimmed:find("\\\\%s*$") then
+      seenrow = true
+    end
+  end
+  if headend and seenrow and headend < #lines then
+    local head = table.concat(lines, "\n", 1, headend)
+    local rest = table.concat(lines, "\n", headend + 1)
+    inner = head .. "\n\\endfirsthead\n" .. head:gsub("^%s*\n", "") ..
+        "\n\\endhead\n" .. rest
+  end
+  -- A plain table's closing \hline. Longtable's \hline offers a page break
+  -- between the rule and its own copy, so near a page foot the note went to
+  -- the next page alone under a second rule. Drawn instead as one rule the
+  -- way longtable draws each copy (across the table's columns, not the full
+  -- line), with no break on either side, and the template's closing rule
+  -- stood down for it.
+  local stem = inner:match("^(.-)\\hline%s*$")
+  local ncol = speccolumns(spec)
+  if stem and ncol then
+    inner = stem .. "\\noalign{\\penalty10000}\\multispan{" .. ncol ..
+        "}\\unskip\\leaders\\hrule height\\arrayrulewidth\\hfill\\cr\n" ..
+        "\\noalign{\\penalty10000}\n" ..
+        "\\noalign{\\csname apatablesownbottomrule\\endcsname{}}\n"
+  end
+  return "\\begin{longtable}" .. spec .. "\n" .. strut ..
+      inner:gsub("^%s*\n", "") .. "\\end{longtable}"
+end
+
+local liftrawtabular = function(float)
+  local raws, others = 0, 0
+  float.content:walk {
+    RawBlock = function(rb)
+      if rb.format == "tex" or rb.format == "latex" then
+        raws = raws + 1
+      end
+    end,
+    Table = function(t) others = others + 1 end,
+    Image = function(i) others = others + 1 end,
+  }
+  if raws ~= 1 or others ~= 0 then
+    return
+  end
+  float.content = float.content:walk {
+    RawBlock = function(rb)
+      if rb.format == "tex" or rb.format == "latex" then
+        local lt = rawtabular_to_longtable(rb.text)
+        if lt then
+          return pandoc.RawBlock(rb.format, lt)
+        end
+      end
+    end
+  }
+end
+
+local haslongtable = function(float)
+  local found = false
+  local ntables = 0
+  local node = float.__quarto_custom_node or float.content
+  if node == nil then
+    return false
+  end
+  node:walk {
+    RawBlock = function(rb)
+      if not found and (rb.format == "tex" or rb.format == "latex") then
+        if rb.text:find("\\begin{longtable}", 1, true)
+            or rb.text:find("\\begin{longtable*}", 1, true) then
+          found = true
+        end
+      end
+      return nil
+    end,
+    Table = function(t)
+      ntables = ntables + 1
+      return nil
+    end
+  }
+  if found then return true end
+  if ntables ~= 1 then return false end
+  local content = float.content
+  if content and content.t ~= nil and content.t ~= "Blocks" then
+    content = pandoc.Blocks({ content })
+  end
+  if content and not onlytable(content) then return false end
+  return true
+end
+
+-- How tall a table will be -- single-spaced lines of body text, plus points
+-- for rows set at their own size -- so that the in-text reservation (below)
+-- can be the table's own height rather than a fixed amount. Calibrated on
+-- measured tables (caption top to note bottom, 2026-10-21): the number,
+-- title, gap and three rules take 3.4 lines plus the header rows; a markdown
+-- table's row is a body line, and a row of a raw table that sets its own
+-- size (apaTables' \fontsize{10}{12}) is counted at that size in points; a
+-- stacked apaTables cell -- estimate, interval, p -- counts each stacked
+-- line; a note line keeps the manuscript's
+-- spacing, 1.65 lines outside documentmode: doc, about 95 characters to a
+-- full-width line, each note entry starting a new line; and a running foot
+-- (apaTables' 48 pt spacer) counts when it is taller than the note. On the
+-- calibration sweeps it errs high by one to two lines, on purpose: too high
+-- moves a table that would just have fitted, too low lets a short table
+-- straddle a page break.
+local function rawrows(segment)
+  local n = 0
+  for line in segment:gmatch("[^\n]+") do
+    if line:find("\\\\%*?%s*$") and not line:find("tabularnewline", 1, true) then
+      local deepest = 0
+      for inner in line:gmatch("\\begin{tabular}(.-)\\end{tabular}") do
+        local _, c = inner:gsub("\\\\", "")
+        if c > deepest then deepest = c end
+      end
+      n = n + 1 + deepest
+    end
+  end
+  return n
+end
+
+local function tablelines(float)
+  local lines = 3.4
+  local title = pandoc.utils.stringify(float.caption_long or "")
+  lines = lines + math.floor(#title / 90)
+  local node = float.__quarto_custom_node or float.content
+  local raw = {}
+  local rows = 0
+  local rowpts = 0
+  local footpts = 0
+  if node then
+    node:walk {
+      RawBlock = function(rb)
+        if rb.format == "tex" or rb.format == "latex" then
+          raw[#raw + 1] = rb.text
+        end
+      end,
+      Table = function(t)
+        -- A cell's lines: each paragraph, list item and line break starts
+        -- a line, and a paragraph wraps only in a column given a relative
+        -- width.
+        local function blocklines(blocks, cap)
+          local n = 0
+          for _, b in ipairs(blocks) do
+            if b.t == "Para" or b.t == "Plain" then
+              local breaks = 0
+              for _, inl in ipairs(b.content) do
+                if inl.t == "LineBreak" then breaks = breaks + 1 end
+              end
+              local chars = #pandoc.utils.stringify(b)
+              n = n + math.max(1, math.ceil(chars / cap)) + breaks
+            elseif b.t == "BulletList" or b.t == "OrderedList" then
+              for _, item in ipairs(b.content) do
+                n = n + math.max(1, blocklines(item, cap))
+              end
+            elseif b.t == "LineBlock" then
+              n = n + #b.content
+            elseif b.t == "Div" then
+              n = n + blocklines(b.content, cap)
+            else
+              n = n + 1
+            end
+          end
+          return n
+        end
+        local function rowlines(r)
+          local most = 1
+          for ci, c in ipairs(r.cells) do
+            local spec = t.colspecs[ci]
+            local w = spec and spec[2]
+            local cap = math.huge
+            -- A column holds its share of the line less its padding (about
+            -- three characters), and words wrap short of the edge (about
+            -- 85% of it is filled): measured, a 70-character cell in a
+            -- fifth-of-the-line column printed in 4 lines, not 3.
+            if type(w) == "number" and w > 0 then
+              cap = math.max(1, (w * 95 - 3) * 0.85)
+            end
+            local n = blocklines(c.contents, cap)
+            if n > most then most = n end
+          end
+          return most
+        end
+        for _, r in ipairs(t.head.rows) do rows = rows + rowlines(r) end
+        for _, b in ipairs(t.bodies) do
+          for _, r in ipairs(b.head) do rows = rows + rowlines(r) end
+          for _, r in ipairs(b.body) do rows = rows + rowlines(r) end
+        end
+        for _, r in ipairs(t.foot.rows) do rows = rows + rowlines(r) end
+      end
+    }
+  end
+  if rows == 0 and #raw > 0 then
+    local tex = table.concat(raw, "\n")
+    local headend = tex:find("\\endfirsthead", 1, true)
+        or tex:find("\\endhead", 1, true)
+    local bodystart = nil
+    for _, anchor in ipairs({ "\\endlastfoot", "\\endfoot", "\\endhead",
+                              "\\endfirsthead" }) do
+      local from = 1
+      while true do
+        local _, e = tex:find(anchor, from, true)
+        if not e then break end
+        if not bodystart or e > bodystart then bodystart = e end
+        from = e + 1
+      end
+    end
+    local n = 0
+    if headend then n = n + rawrows(tex:sub(1, headend)) end
+    n = n + rawrows(tex:sub((bodystart or 0) + 1))
+    local skip = tonumber(tex:match("\\fontsize{[%d.]+}{([%d.]+)}"))
+    -- Space added between rows (apaTables puts \addlinespace before each
+    -- group label), counted at a 12 pt em, rounded high.
+    local body = tex:sub((bodystart or 0) + 1)
+    for amount in body:gmatch("\\addlinespace%[([%d.]+)em%]") do
+      rowpts = rowpts + tonumber(amount) * 12
+    end
+    for _ in body:gmatch("\\addlinespace%f[^%a%[]") do
+      rowpts = rowpts + 6
+    end
+    -- Rules between rows: a plain table's \hline under every row, and the
+    -- \cmidrule under a spanning header (kableExtra's add_header_above),
+    -- which carries booktabs' space above and below it. Measured: without
+    -- these a six-row plain table, and a table with a spanning header,
+    -- straddled a page break at a few positions.
+    -- The header repeated for continuation pages is not on the first page.
+    local once = tex
+    local ra = tex:find("\\endfirsthead", 1, true)
+    local _, rb = tex:find("\\endhead", 1, true)
+    if ra and rb and rb > ra then
+      once = tex:sub(1, ra - 1) .. tex:sub(rb + 1)
+    end
+    for _ in once:gmatch("\\hline") do
+      rowpts = rowpts + 1
+    end
+    for _ in once:gmatch("\\multispan") do
+      rowpts = rowpts + 1
+    end
+    for line in once:gmatch("[^\n]+") do
+      if line:find("\\cmidrule") then
+        rowpts = rowpts + 6
+      end
+    end
+    if skip then
+      -- Rows set at their own size (apaTables' \fontsize{10}{12}) are
+      -- counted in points, not in lines of the body: a document set at
+      -- 10 pt has 12 pt body lines, and a row counted as a fraction of a
+      -- 14.5 pt line came out a sixth short there.
+      rowpts = rowpts + n * skip
+    else
+      rows = n
+    end
+    -- The running foot. Until its last row a longtable keeps room for its
+    -- \endfoot on the page; apaTables' is a 48 pt spacer, taller than the
+    -- note that replaces it on the last page, so a table needs that much
+    -- more room to fit than it shows (measured: a 6-row table straddled at
+    -- an offset where its visible height fitted).
+    local fa, fb = tex:find("\\endhead", 1, true)
+    local fe = tex:find("\\endfoot", 1, true)
+    if fb and fe and fe > fb then
+      local foot = 0
+      for h in tex:sub(fb + 1, fe):gmatch("\\rule{[^}]*}{([%d.]+)pt}") do
+        foot = foot + tonumber(h)
+      end
+      footpts = math.max(foot, rawrows(tex:sub(fb + 1, fe)) * (skip or 14.5))
+    end
+  end
+  lines = lines + rows
+  local lastfoot = 0
+  local note = float.attributes["apa-note"]
+  if note and note ~= "" then
+    local notelines = 0
+    for entry in (note .. '","'):gmatch('(.-)","') do
+      notelines = notelines + math.max(1, math.ceil(#entry / 95))
+    end
+    lastfoot = 0.5 + notelines * (docmode and 1 or 1.65)
+  end
+  -- The last foot is the note or the running foot, whichever is taller.
+  local pts = rowpts
+  if footpts > lastfoot * 14.5 then
+    pts = pts + footpts
+  else
+    lines = lines + lastfoot
+  end
+  -- Rounded up: a table that straddles a page break is the defect this
+  -- exists to prevent, and one moved a line early is only white space.
+  -- (Rounding to the nearest line let a short apaTables table straddle.)
+  local n = math.ceil(lines)
+  -- The space longtable leaves above itself (\LTpre, 12 pt) is not in the
+  -- calibration, which runs from caption to note. A manuscript's note
+  -- allowance (1.65 lines a line) covers it; documentmode: doc counts its
+  -- note at one line a line, so there it is added (measured: a correlation
+  -- table straddled with its visible height 3 pt inside the reservation).
+  local pre = docmode and "+\\LTpre" or ""
+  if pts > 0 then
+    return "\\dimexpr " .. n .. "\\apatablesline+" .. math.ceil(pts) ..
+        "pt" .. pre .. "\\relax"
+  end
+  if docmode then
+    return "\\dimexpr " .. n .. "\\apatablesline" .. pre .. "\\relax"
+  end
+  return n .. "\\apatablesline"
+end
+
+-- Wrap a rotated float in a LITERAL \begin{landscape} block.
+--
+-- Rotation is pdflscape, in both placements and whatever the table's height:
+-- APA asks for the PAGE to be turned ("use landscape orientation on the page
+-- with the wide table ... it does not matter if the page header also moves"),
+-- and turning the page is one mechanism that works for a table of any length.
+-- The rotating package's sidewaystable, which this filter used to emit when
+-- floats were collected at the end, turns the TABLE on an upright page -- it
+-- is apa7's LaTeX convention rather than APA's instruction, and it is a FLOAT,
+-- so it cannot hold a longtable at all. Measured on a 60-row table inside a
+-- landscape div with floats at the end: sidewaystable gave 51 of 60 rows, a
+-- starred longtable*, a note that never reached the PDF, and an upright page,
+-- with no error and no warning.
+--
+-- LITERAL, never macro-constructed. Under endfloat the deferred block is read
+-- back line by line and terminated by an exact match on \end{landscape}, and a
+-- float built by a macro dies there (\efloat@xfloat, TeX capacity exceeded).
+-- Two RawBlocks, each alone on its line, is the whole construction.
+--
+-- The template declares landscape as a delayed float flavour of table, so
+-- endfloat carries this block to the end of the manuscript as a unit --
+-- rotation included, and one page per table, because each float gets its own
+-- wrapper rather than sharing one around the div.
+local landscapewrap = function(blocks, float, injournalmode)
+  if float.attributes["apa-landscape"] ~= "true" then return blocks end
+  -- Journal mode has no endfloat and a two-column page; rotation there is a
+  -- separate problem and was never handled here.
+  if injournalmode then return blocks end
+  -- Already inside Quarto's own ::: {.landscape} div, which supplies the
+  -- environment when floats stay in the text (apalandscapelatex.lua says so).
+  -- A second, nested \begin{landscape} would clear the page twice.
+  if float.attributes["apa-landscape-wrapped"] == "true" then return blocks end
+  quarto.doc.use_latex_package("pdflscape")
+  local out = pandoc.Blocks({ pandoc.RawBlock("latex", "\\begin{landscape}") })
+  out:extend(blocks)
+  out:insert(pandoc.RawBlock("latex", "\\end{landscape}"))
+  return out
+end
+
 local processfloat = function(float)
   if float.attributes["disable-apaquarto-processing"] then
     if not (float.attributes["disable-apaquarto-processing"] == "false") then
@@ -82,15 +629,128 @@ local processfloat = function(float)
         -- credit to @michaelzehetleitne https://github.com/wjschne/apaquarto/issues/71
         -- Long-table mode: skip float wrapper so longtable can page-break
     --quarto.log.output(float.attributes)
-    if float.attributes["apa-longtable"] == "true" and not journalmode then
+    -- `apa-longtable: true` is still honoured, so a document that carries the
+    -- old chunk option renders exactly as it did; it is simply no longer
+    -- something the author has to know about.
+    --
+    -- A ROTATED longtable takes this branch too, and must. The interim guard
+    -- that used to stand here -- `and float.attributes["apa-landscape"] ~=
+    -- "true"` -- existed only because rotation was then done with
+    -- sidewaystable, which IS a float, so dropping the wrapper would have
+    -- dropped the rotation with it. Rotation is now a literal
+    -- \begin{landscape} block that wraps whatever this branch returns and
+    -- needs no float to grip, so the guard is gone. It had to go in the same
+    -- change: left behind, it would have sealed a rotated longtable inside
+    -- \begin{table} and lost rows in silence.
+    if not journalmode then
+      liftrawtabular(float)
+    end
+    if (float.attributes["apa-longtable"] == "true" or haslongtable(float))
+        and not journalmode then
       local blocks = pandoc.Blocks({})
+      -- Keep a SHORT table whole -- but only where floats stay in the text.
+      --
+      -- A longtable outside a float is breakable, which is what a tall table
+      -- needs and what a short one does not want: measured on a 6-row table at
+      -- vertical offsets of 6.8 in and 7.2 in, it straddled the page break 5/1
+      -- and 4/2. The reservation asks LaTeX to start a new page when less than
+      -- the stated amount of column is left, so a short table moves down whole
+      -- while a tall one still breaks with every row present. The argument is a
+      -- HINT, not a measurement -- too large moves a table that did not need
+      -- moving, too small lets it split; neither loses a row. Starring the rows
+      -- (\\*) was the other candidate and is REJECTED: it makes the table
+      -- unbreakable, and 10 of 40 rows vanished from a document that compiled
+      -- without a murmur.
+      --
+      -- IT IS \Needspace*, THE EAGER FORM, AND THE LOWER-CASE \needspace IS A
+      -- MEASURED DEFECT -- do not "simplify" it back.
+      --
+      -- \needspace works by GLUE: it lays down `\vskip 0pt plus X`, a
+      -- `\penalty-100`, and a matching negative skip, which makes a break in
+      -- front of the table cheap and leaves the decision to TeX's page builder.
+      -- By the time that builder runs, \LT@start has already contributed the
+      -- first head AND installed \LT@output (longtable.sty:253). \LT@output's
+      -- normal-break branch ends `\copy\LT@head\nobreak` (longtable.sty:530),
+      -- UNCONDITIONALLY -- so it services a break that happened BEFORE any of
+      -- the table's own material and stamps a continuation header on the new
+      -- page. The reader gets a rule, the header row, a rule, and then the
+      -- caption and the real table underneath: the header printed twice, the
+      -- first copy attached to nothing.
+      --
+      -- \Needspace* instead compares \pagegoal-\pagetotal against the argument
+      -- and breaks THERE AND THEN, before \begin{longtable} is ever read, so
+      -- longtable never sees the break and never repeats its head.
+      --
+      -- MEASURED, 26 documents, table pushed to a different height on the page
+      -- in each -- and the two sizes are scored on DIFFERENT properties,
+      -- because for a TALL table straddling is correct behaviour, not a defect:
+      --
+      --   short table (straddle is the defect)   stranded  straddle  pages
+      --     \needspace                              2         0        39
+      --     nothing                                 0         2        37
+      --     \Needspace*                             0         0        37
+      --   tall table (stranded head is the defect)
+      --     \needspace                              6         0        43
+      --     nothing                                 0         8        43
+      --     \Needspace*                             0         6        43
+      --
+      -- \Needspace* is better on every axis that applies: short tables still
+      -- never straddle, tall tables are never stranded, and it costs two pages
+      -- FEWER than \needspace, which was pushing tall tables off pages they had
+      -- half a column left on.
+      --
+      -- WHAT IT HAS TO COVER. The reserve must span the WHOLE table --
+      -- caption, header, rows, and \endlastfoot, which carries the bottom rule
+      -- and the note. Sizing it for the rows alone is a real and measured
+      -- defect, not a theoretical one: a 7-row table kept all 7 rows on one
+      -- page and pushed its note, alone under a repeated continuation header,
+      -- onto the next. Rows intact and note adrift is still a broken table,
+      -- and it looks deliberate.
+      --
+      -- HOW MUCH. It used to be a fixed 12\baselineskip, swept when table
+      -- rows were double-spaced (a 7-row table with a two-entry note needed
+      -- 11; 13 and 14 wasted pages). Table rows are single-spaced now, so the
+      -- fixed amount was about twice what a short table needs, and a one-row
+      -- table moved to the next page with half a page to spare. The reserve is
+      -- now the table's own estimated height (tablelines, above), in
+      -- single-spaced lines (\apatablesline, set by the template). The
+      -- template compares it with the page: a table that fits on a page moves
+      -- to the next one whole rather than starting low and breaking (as a
+      -- captioned markdown table, once a float, always did), and one taller
+      -- than the page asks only for room to start and breaks across pages,
+      -- as a tall table should, with every row present.
+      --
+      -- NOT in the default placement, for three separate reasons, any one of
+      -- which is sufficient. endfloat re-emits the deferred environment alone
+      -- and drops what preceded it, so the instruction is stripped (measured:
+      -- 6 occurrences in the body .tex, 0 in the .ttt). It is not needed, since
+      -- endfloat already gives each deferred float its own page, leaving a
+      -- short table nothing to straddle. And what endfloat leaves behind is a
+      -- bare reservation sitting in the author's prose, where it can force a
+      -- page break in the text that no table justifies.
+      if not delayedfloats then
+        blocks:insert(pandoc.RawBlock("latex", "\\Needspace*{" ..
+          tablelines(float) .. "}"))
+      end
       -- Use Quarto's native longtable output (caption + label included)
       if float.__quarto_custom_node then
         blocks:insert(float.__quarto_custom_node)
       else
         blocks:insert(float.content)
       end
-      -- Append apa-note if present
+      -- Append apa-note if present.
+      --
+      -- The note is left in the AST, marked, and spliced into the table's own
+      -- \endlastfoot later, by apalongtablenote.lua. It cannot be spliced
+      -- here for two measured reasons. First, this filter runs at post-quarto,
+      -- BEFORE citeproc: a note frozen to LaTeX now keeps its [@key]
+      -- literally, and a work cited only in a table note is never counted, so
+      -- it vanishes from the reference list -- and APA 7 section 7.7 requires
+      -- a copyright attribution note on any reproduced table, which makes a
+      -- citation the most ordinary thing a table note can hold. Second, the
+      -- raw block is inside Quarto's custom float node; replacing one block
+      -- with three breaks that node, and the table reverts to a \begin{table}
+      -- float with the note dropped entirely.
       if float.attributes["apa-note"] then
         local bn = ""
         if manuscriptmode then
@@ -99,10 +759,28 @@ local processfloat = function(float)
             bn = "\\vspace{" .. float.attributes["beforenotespace"] .. "}\n"
           end
         end
-        local npfx = pandoc.Span(pandoc.RawInline("latex", bn .. noteprefix))
-        blocks:insert(utilsapa.make_note(float.attributes["apa-note"], npfx))
+        -- Inside the table the note is a row, not a paragraph that follows the
+        -- table environment, so the -12pt correction that closes the paragraph
+        -- gap would drag it up over the bottom rule. The note is therefore
+        -- built without it and the sibling-only spacing is carried alongside,
+        -- to be put back if the splice cannot happen. An explicitly requested
+        -- beforenotespace applies on both paths: dropping a stated option
+        -- silently is worse than any spacing it produces.
+        local fbn = ""
+        if float.attributes["beforenotespace"] then
+          fbn = "\\vspace{" .. float.attributes["beforenotespace"] .. "}\n"
+        end
+        local npfx = pandoc.Span(pandoc.RawInline("latex", fbn .. noteprefix))
+        local notedivs = utilsapa.make_note(float.attributes["apa-note"], npfx)
+        notedivs.classes:insert("apa-longtable-note")
+        notedivs.attributes["apa-longtable-note-sibling-space"] = bn
+        blocks:insert(notedivs)
       end
-      return pandoc.Div(blocks)
+      local lifted = pandoc.Div(landscapewrap(blocks, float, journalmode))
+      -- Marked for apalongtablenote.lua, which arms the closing rule of a
+      -- table Quarto has already written as raw LaTeX (a kable table).
+      lifted.classes:insert("apa-lifted-longtable")
+      return lifted
     end
     -- Default table environment
     local latextableenv = "table"
@@ -189,13 +867,21 @@ local processfloat = function(float)
       })
     end
 
-    return returnblock
+    return pandoc.Div(landscapewrap(pandoc.Blocks({ returnblock }), float,
+                                    journalmode))
   end
 
   if float.type == "Figure" then
     local hasnote = false
     local apanote
     local twocolumn = false
+    -- A rotated figure is an ordinary \begin{figure} inside a literal
+    -- \begin{landscape} block, for the same reason a rotated table is: the
+    -- page turns, not the picture. `landscape` is still tracked because it is
+    -- what makes this filter build the float itself rather than leave the
+    -- figure to Quarto -- there would otherwise be nothing to wrap.
+    local landscape = float.attributes["apa-landscape"] == "true"
+        and not journalmode
     local latexenv = "figure"
     -- Get apa-note from image, if possible
     float.content:walk {
@@ -218,12 +904,12 @@ local processfloat = function(float)
       end
     }
 
-    if twocolumn then
+    if twocolumn and not landscape then
       latexenv = "figure*"
     end
 
     -- Make note
-    if hasnote or twocolumn then
+    if hasnote or twocolumn or landscape then
       if hasnote then
         -- Add note
         if float.attributes["apa-note"] then
@@ -259,13 +945,58 @@ local processfloat = function(float)
         pandoc.RawBlock("latex", "\\end{" .. latexenv .. "}")
       })
 
-      return returnblock
+      return pandoc.Div(landscapewrap(pandoc.Blocks({ returnblock }), float,
+                                      journalmode))
     end
   end
 end
 
 
+-- A table placed in the text straight after a heading: its reservation is
+-- moved in front of the heading and made three lines larger, so that when
+-- the table has to go to the next page the heading goes with it. Left
+-- between them, the reservation broke the page after the heading, which was
+-- left alone at the foot of the page above its table.
+local function stripreserve(b)
+  if b.t ~= "Div" then return nil end
+  local first = b.content[1]
+  if not first then return nil end
+  if first.t == "RawBlock" and first.text:find("^\\Needspace%*{") then
+    local rest = pandoc.Blocks({})
+    for i = 2, #b.content do rest:insert(b.content[i]) end
+    b.content = rest
+    return b, first
+  end
+  local inner, rb = stripreserve(first)
+  if rb then
+    local content = b.content
+    content[1] = inner
+    b.content = content
+    return b, rb
+  end
+  return nil
+end
+
+local function movereserve(blocks)
+  local k = 1
+  while k < #blocks do
+    if blocks[k].t == "Header" then
+      local nb, rb = stripreserve(blocks[k + 1])
+      if rb then
+        local arg = rb.text:match("^\\Needspace%*{(.*)}%s*$")
+        blocks[k + 1] = nb
+        blocks:insert(k, pandoc.RawBlock("latex", "\\Needspace*{\\dimexpr " ..
+          arg .. "+3\\apatablesline\\relax}"))
+        k = k + 1
+      end
+    end
+    k = k + 1
+  end
+  return blocks
+end
+
 return {
   { Meta = getmode },
-  { FloatRefTarget = processfloat }
+  { FloatRefTarget = processfloat },
+  { Blocks = movereserve }
 }
diff --git a/_extensions/apaquarto/apafloatstoend.lua b/_extensions/apaquarto/apafloatstoend.lua
index 0f835de..4531a10 100644
--- a/_extensions/apaquarto/apafloatstoend.lua
+++ b/_extensions/apaquarto/apafloatstoend.lua
@@ -1,6 +1,19 @@
 if FORMAT == "latex" then
   return
 end
+-- Is this block one of the page breaks emitted above, rather than a raw block
+-- the document itself contains?
+local function is_own_page_break(b)
+  if not b or b.t ~= "RawBlock" then return false end
+  if b.format == "openxml" then
+    return b.text:find('w:type="page"', 1, true) ~= nil
+  end
+  if b.format == "typst" then
+    return b.text:find("pagebreak", 1, true) ~= nil
+  end
+  return false
+end
+
 Pandoc = function(doc)
   local tbl = {}
   local fig = {}
@@ -21,25 +34,25 @@ Pandoc = function(doc)
       if doc.blocks[i].identifier then
         if doc.blocks[i].identifier:find("^tbl%-") then
           if doc.blocks[i].attributes and doc.blocks[i].attributes.prefix == "" then
+            table.insert(tbl, 1, doc.blocks[i])
             if FORMAT == "docx" then
               table.insert(tbl, 1, pandoc.RawBlock('openxml', '<w:p><w:r><w:br w:type="page"/></w:r></w:p>'))
             end
             if FORMAT == "typst" then
               table.insert(tbl, 1, pandoc.RawBlock('typst', '#pagebreak(weak: true)'))
             end
-            table.insert(tbl, 1, doc.blocks[i])
             doc.blocks:remove(i)
           end
         else
           if doc.blocks[i].identifier:find("^fig%-") then
             if doc.blocks[i].attributes and doc.blocks[i].attributes.prefix == "" then
+              table.insert(fig, 1, doc.blocks[i])
               if FORMAT == "docx" then
                 table.insert(fig, 1, pandoc.RawBlock('openxml', '<w:p><w:r><w:br w:type="page"/></w:r></w:p>'))
               end
               if FORMAT == "typst" then
                 table.insert(fig, 1, pandoc.RawBlock('typst', '#pagebreak(weak: true)'))
               end
-              table.insert(fig, 1, doc.blocks[i])
               doc.blocks:remove(i)
             end
           else
@@ -56,13 +69,13 @@ Pandoc = function(doc)
               end
             }
             if hasfig then
+              table.insert(fig, 1, doc.blocks[i])
               if FORMAT == "docx" then
                 table.insert(fig, 1, pandoc.RawBlock('openxml', '<w:p><w:r><w:br w:type="page"/></w:r></w:p>'))
               end
               if FORMAT == "typst" then
                 table.insert(fig, 1, pandoc.RawBlock('typst', '#pagebreak(weak: true)'))
               end
-              table.insert(fig, 1, doc.blocks[i])
               doc.blocks:remove(i)
               hasfig = false
             else
@@ -112,7 +125,18 @@ Pandoc = function(doc)
         doc.blocks:extend(fig)
       end
     else
-      -- Insert figures and tables before appendices
+      -- Insert figures and tables before appendices.
+      -- Each float now carries its own leading page break, so the floats must
+      -- go BEFORE the break that was inserted for the appendix heading,
+      -- otherwise that break is doubled and opens a blank page.
+      --
+      -- Test for THIS filter's own break, not merely for a RawBlock. Only docx
+      -- and typst get one, and any raw block a document itself places before
+      -- an appendix heading -- a raw HTML block, say -- would otherwise be
+      -- mistaken for it and the floats inserted on the wrong side of it.
+      if appendixblock > 1 and is_own_page_break(doc.blocks[appendixblock - 1]) then
+        appendixblock = appendixblock - 1
+      end
       if #fig > 0 then
         for i = #fig, 1, -1 do
           doc.blocks:insert(appendixblock, fig[i])
diff --git a/_extensions/apaquarto/apalandscapelatex.lua b/_extensions/apaquarto/apalandscapelatex.lua
new file mode 100644
index 0000000..2e012cf
--- /dev/null
+++ b/_extensions/apaquarto/apalandscapelatex.lua
@@ -0,0 +1,98 @@
+if FORMAT ~= "latex" then
+  return
+end
+
+-- Landscape (rotated) floats in latex.
+--
+-- APA prescribes turning the PAGE for a wide table -- "use landscape
+-- orientation on the page with the wide table ... it does not matter if the
+-- page header also moves" -- which is what pdflscape's landscape environment
+-- does, and what Quarto's own ::: {.landscape} div emits. pdflscape is
+-- therefore the SINGLE rotation mechanism here. An earlier version of this
+-- filter routed delayed floats through the rotating package's sidewaystable
+-- instead, which turns the table on an upright page; that is apa7's LaTeX
+-- convention, not APA's instruction, and it is gone.
+--
+-- What the two placements need is different, and neither is what the div
+-- gives on its own:
+--
+--   floats in the text   Quarto's div already emits \begin{landscape} around
+--                        whatever it contains, and it works. Left alone.
+--
+--   floats at the end    apa7 loads endfloat, which re-typesets each float on
+--                        its own page at the end of the manuscript and drops
+--                        whatever surrounded it in the body. The div's
+--                        landscape environment is left behind holding nothing
+--                        -- two \clearpage's in the middle of the prose -- and
+--                        the table lands upright. So the div is unwrapped and
+--                        each float inside it is marked apa-landscape="true";
+--                        apafloatlatex.lua then wraps THAT FLOAT in its own
+--                        literal \begin{landscape} block, which the template's
+--                        \DeclareDelayedFloatFlavor{landscape}{table} defers
+--                        whole, rotation included.
+--
+-- ONE WRAPPER PER FLOAT, not one around the div. endfloat gives each deferred
+-- block its own page, so wrapping the div as a unit would put every table in
+-- it on a single landscape page; wrapping each float separately keeps APA's
+-- one-table-per-page layout.
+
+local utilsapa = require("utilsapa")
+
+local delayedfloats = true
+
+-- The rule itself lives in utilsapa so that apafloatlatex.lua, which needs the
+-- same answer for the longtable reservation, cannot come to a different one.
+local getmeta = function(meta)
+  delayedfloats = utilsapa.delayedfloats(meta)
+end
+
+-- Mark every float in this div, setting `attr` to "true".
+local markfloats = function(div, attr)
+  return div.content:walk {
+    Div = function(d)
+      if d.identifier and (d.identifier:find("^tbl%-") or d.identifier:find("^fig%-")) then
+        d.attributes[attr] = "true"
+        return d
+      end
+    end,
+    Table = function(tbl)
+      -- Plain markdown tables: at this stage the {#tbl-...} block is still
+      -- literal text in the caption, so the identifier is not set yet.
+      -- Every table inside a landscape div wants rotation, so mark it.
+      tbl.attributes[attr] = "true"
+      return tbl
+    end,
+    Figure = function(fg)
+      if fg.identifier and fg.identifier:find("^fig%-") then
+        fg.attributes[attr] = "true"
+        return fg
+      end
+    end
+  }
+end
+
+local marklandscape = function(div)
+  if not div.classes:includes("landscape") then
+    return nil
+  end
+  if not delayedfloats then
+    -- Floats stay in text: Quarto's native landscape div works, and the div
+    -- IS the wrapper. Say so, so that a float which also carries
+    -- apa-landscape="true" -- written as a chunk option, or set by a table
+    -- builder that has decided the table is too wide to stand upright -- does
+    -- not get a second \begin{landscape} nested inside this one.
+    div.content = markfloats(div, "apa-landscape-wrapped")
+    return div
+  end
+  div.content = markfloats(div, "apa-landscape")
+  -- Unwrap: the floats are delayed to the end of the manuscript, each now
+  -- carrying its own landscape wrapper, so a landscape environment here would
+  -- be empty and would only break the surrounding text across pages
+  -- (\clearpage at both ends).
+  return div.content
+end
+
+return {
+  { Meta = getmeta },
+  { Div = marklandscape }
+}
diff --git a/_extensions/apaquarto/apalongtablenote.lua b/_extensions/apaquarto/apalongtablenote.lua
new file mode 100644
index 0000000..d316a91
--- /dev/null
+++ b/_extensions/apaquarto/apalongtablenote.lua
@@ -0,0 +1,729 @@
+-- Attach a long table's apa-note to the table itself.
+--
+-- apafloatlatex.lua marks the note of an apa-longtable and leaves it in the
+-- AST as a sibling of the table. A sibling block is a separate paragraph as
+-- far as LaTeX is concerned: under apaquarto's default placement endfloat
+-- defers the longtable to the end of the manuscript and the note, not being a
+-- float, stays behind beside the body text it was written next to (measured:
+-- 0 of 3 notes stayed with their tables). This filter moves the note into the
+-- table's own \endlastfoot, so LaTeX carries it wherever the table goes and
+-- prints it once, under the final page of a table that breaks.
+--
+-- It runs at post-render, AFTER citeprocr.lua, on purpose. The note has to be
+-- written out as LaTeX to go inside the raw block, and doing that any earlier
+-- freezes [@key] literally and -- worse -- hides the citation from citeproc,
+-- so a work cited only in a table note disappears from the reference list.
+-- APA 7 section 7.7 requires a copyright attribution note ("Adapted from ...")
+-- on any reproduced table, so that is the single most common citation site in
+-- an APA table.
+if FORMAT ~= "latex" then
+  return
+end
+
+local notemarker = "apa-longtable-note"
+local siblingattr = "apa-longtable-note-sibling-space"
+
+-- Read a longtable's own preamble: how many columns it has, how it is
+-- horizontally aligned, and how wide a note spanning it may be.
+--
+-- Everything is derived, nothing assumed. The note is emitted as a
+-- \multicolumn spanning every column, so a wrong count is a malformed row.
+-- The parse is deliberately strict: after the constructs below are consumed,
+-- anything left over means a preamble this code does not understand (a
+-- *{3}{c} repetition, a dcolumn D{.}{.}{2}, an S[table-format=..] option) and
+-- it gives up rather than emit a row with the wrong number of cells. An
+-- OVERcount is a loud LaTeX error, but an UNDERcount is a short row that
+-- LaTeX accepts silently, which is the worse failure of the two.
+--
+-- Returns ncol, width, align. width is a LaTeX dimen expression for the
+-- table's real width -- the declared column widths plus the \tabcolsep either
+-- side of each column and the \arrayrulewidth of each | rule -- when every
+-- column declares one (p/m/b, which is what flextable emits) and there is no
+-- @/! insert of unknowable width; otherwise nil, meaning "use the text
+-- block". align is "l", "c", "r" or "" (unknown). Returns nil when there is
+-- no longtable preamble to read.
+local function apalongtablespec(tex)
+  local opt, spec = tex:match("\\begin{longtable}(%b[])%s*(%b{})")
+  if not spec then
+    opt = ""
+    spec = tex:match("\\begin{longtable}%s*(%b{})")
+  end
+  if not spec then return nil end
+  local align = opt:gsub("[%[%]%s]", "")
+  if align ~= "l" and align ~= "c" and align ~= "r" then align = "" end
+  local inner = spec:sub(2, -2)
+  -- @{..} and !{..} put material of unknown width between columns; >{..} and
+  -- <{..} only inject declarations and cost nothing.
+  local hasinsert = inner:find("@%b{}") ~= nil or inner:find("!%b{}") ~= nil
+  inner = inner:gsub("@%b{}", ""):gsub("!%b{}", "")
+      :gsub(">%b{}", ""):gsub("<%b{}", "")
+  local bars = select(2, inner:gsub("|", ""))
+  local n = 0
+  local widths = {}
+  local rest = inner:gsub("[pmb]%b{}", function(col)
+    n = n + 1
+    widths[#widths + 1] = col:match("%b{}"):sub(2, -2)
+    return ""
+  end)
+  local sized = #widths > 0
+  for _ in rest:gmatch("[lcrXS]") do
+    n = n + 1
+    sized = false
+  end
+  if rest:gsub("[lcrXS|%s]", "") ~= "" then return nil end
+  if n == 0 then return nil end
+  local width = nil
+  if sized and not hasinsert then
+    -- Declared widths, plus the \tabcolsep between each pair of columns and
+    -- the \arrayrulewidth of each | rule. The \tabcolsep OUTSIDE the first
+    -- and last column is deliberately left out: measured on the apaTables
+    -- probe the full table box is 421.28pt while the rules LaTeX actually
+    -- draws span 413.76pt, so counting it would put the note past the visible
+    -- edge of its own table. This lands at 409.28pt -- 4.5pt inside the rule,
+    -- which is the right side of the APA 7 section 7.14 ceiling to be on.
+    width = "\\dimexpr " .. table.concat(widths, "+")
+    if n > 1 then
+      width = width .. "+" .. (2 * n - 2) .. "\\tabcolsep"
+    end
+    if bars > 0 then
+      width = width .. "+" .. bars .. "\\arrayrulewidth"
+    end
+    width = width .. "\\relax"
+  end
+  return n, width, align
+end
+
+-- Where a note row may legally be spliced into a longtable's head/foot
+-- declarations.
+--
+-- longtable requires \endfirsthead, \endhead, \endfoot and \endlastfoot in
+-- that order, all of them before the first body row, so the note row has to
+-- go after the last declaration that is already there. Note that "already
+-- there" has to be decided across the WHOLE table: by the time this filter
+-- runs Quarto has split the longtable into several raw blocks (preamble,
+-- body, \end{longtable}) with the caption sitting between them as ordinary
+-- AST, so a per-block decision could see \endhead in one block and miss an
+-- \endlastfoot in the next and then declare a second, illegal one.
+--
+-- \endfirsthead contains neither \endhead nor \endfoot as a substring, so the
+-- plain (non-pattern) finds below cannot confuse them.
+local anchorkinds = { "\\endlastfoot", "\\endfoot", "\\endhead", "\\endfirsthead" }
+
+-- Which anchor to use, given every latex raw block of the table.
+local function apalongtablekind(texts)
+  for _, kind in ipairs(anchorkinds) do
+    for _, t in ipairs(texts) do
+      if t:find(kind, 1, true) then return kind end
+    end
+  end
+  return nil
+end
+
+-- Splice the note row into the one raw block that carries the chosen anchor.
+-- Returns the rewritten text, or nil if this block is not that one.
+local function apalongtableinsert(tex, kind, row)
+  local start, stop = tex:find(kind, 1, true)
+  if not start then return nil end
+  if kind == "\\endlastfoot" then
+    -- A second \endlastfoot would be illegal, but giving up is not the only
+    -- legal move: rows placed immediately BEFORE the existing token are part
+    -- of that same last foot, so splice there. (kableExtra's repeat_header
+    -- produces this shape, and so does pandoc.)
+    return tex:sub(1, start - 1) .. row .. tex:sub(start)
+  end
+  if kind == "\\endfoot" then
+    -- A running foot but no last foot: longtable reuses \endfoot on the last
+    -- page. An \endlastfoot holding only the note would silently drop that
+    -- foot from the last page, so copy it in ahead of the note.
+    local _, headend = tex:find("\\endhead", 1, true)
+    local _, firstend = tex:find("\\endfirsthead", 1, true)
+    local prev = 0
+    if headend and headend < start then prev = headend end
+    if firstend and firstend < start and firstend > prev then prev = firstend end
+    if prev == 0 then return nil end
+    return tex:sub(1, stop) .. "\n" .. tex:sub(prev + 1, start - 1) ..
+        row .. "\\endlastfoot\n" .. tex:sub(stop + 1)
+  end
+  return tex:sub(1, stop) .. "\n" .. row .. "\\endlastfoot\n" .. tex:sub(stop + 1)
+end
+
+-- Build the note row itself.
+--
+-- It is emitted zero-width, as \makebox[0pt][l]{\parbox[t]{..}{..}}. A
+-- \multicolumn WIDER than the columns it spans stretches the whole table to
+-- match, which would silently relayout a table that only asked for a note; a
+-- zero-width box cannot, whatever width the note ends up with.
+--
+-- The note is capped at the table's own width (APA 7 section 7.14: a note
+-- "should not exceed the width of the table") but floored at half the text
+-- block, because a note squeezed into nine hyphenated lines under a narrow
+-- table is a new failure, not a lesser one. Its left edge is pulled back to
+-- the text block's left margin whichever way the table is aligned, because
+-- APA table notes are flush left: the spanning cell is set to the full width
+-- of the columns it covers, so aligning its zero-width content l/c/r puts
+-- that content at the table's left edge, centre or right edge, and backing
+-- off from there by the matching fraction of \linewidth lands on the margin.
+local function apalongtablerow(ncol, width, align, notetex)
+  local cellalign, shift = "l", ""
+  if align == "c" then
+    cellalign, shift = "c", "\\hspace*{-0.5\\linewidth}"
+  elseif align == "r" then
+    cellalign, shift = "r", "\\hspace*{-\\linewidth}"
+  end
+  local box
+  if width then
+    -- \dimen0 is scratch and the assignment is confined to a group.
+    box = "\\begingroup\\dimen0=" .. width ..
+        "\\ifdim\\dimen0<0.5\\linewidth\\dimen0=0.5\\linewidth\\fi" ..
+        "\\parbox[t]{\\dimen0}{\\csname apatablesnotespacing\\endcsname%\n" ..
+        notetex .. "%\n}\\endgroup"
+  else
+    box = "\\parbox[t]{\\linewidth}{\\csname apatablesnotespacing\\endcsname%\n" ..
+        notetex .. "%\n}"
+  end
+  -- The row opens with booktabs' own gap below a rule. Without it the
+  -- note's first line sits flush under \bottomrule, and once the table rows
+  -- were single-spaced the rule ran through the tops of the note's capitals
+  -- (measured at 200 dpi; already so in documentmode: doc). The note itself
+  -- takes back the manuscript's line spacing, which the template saved when
+  -- the table single-spaced its rows -- a table note is double-spaced like
+  -- the text around it, as in Word. \csname keeps both harmless under a
+  -- template without them.
+  -- It ends with a zero kern so the last foot has no depth: the note is a
+  -- top-aligned box, so its lines after the first would otherwise be DEPTH,
+  -- which neither longtable's own end-of-table check nor the template's
+  -- running-foot reserve measures.
+  return "\\noalign{\\vskip\\belowrulesep}" ..
+      "\\multicolumn{" .. ncol .. "}{@{}" .. cellalign .. "@{}}{" ..
+      "\\makebox[0pt][l]{" .. shift .. box .. "}}\\\\\n\\noalign{\\kern0pt}\n"
+end
+
+-- Find the marked note among a div's children. Returns its index and the div.
+local function findnote(blocks)
+  for i, b in ipairs(blocks) do
+    if b.t == "Div" then
+      for _, c in ipairs(b.classes) do
+        if c == notemarker then return i, b end
+      end
+    end
+  end
+  return nil
+end
+
+-- A note is written out as LaTeX here, after citeproc, so anything in it
+-- that makes the LaTeX writer add something to the preamble is lost: the
+-- writer decides what to set up from what IT writes, and it never writes
+-- this note. Highlighted code needs its macros, a language span needs babel
+-- -- either would leave an undefined command and no PDF. Such a note is left
+-- where the writer will see it, beside its table.
+local function needswriter(blocks)
+  local found = false
+  pandoc.Blocks(blocks):walk {
+    Code = function(c)
+      if #c.classes > 0 then found = true end
+    end,
+    CodeBlock = function(c) found = true end,
+    Span = function(sp)
+      if sp.attributes.lang then found = true end
+    end,
+    Div = function(d)
+      if d.attributes.lang then found = true end
+    end
+  }
+  return found
+end
+
+-- Highlighted code is written as \VERB|...|, a verbatim command, which
+-- cannot sit inside a macro's argument -- so a note holding it cannot be
+-- stored for the template either, and stays beside its table.
+local function hascode(blocks)
+  local found = false
+  pandoc.Blocks(blocks):walk {
+    Code = function(c)
+      if #c.classes > 0 then found = true end
+    end,
+    CodeBlock = function(c) found = true end
+  }
+  return found
+end
+
+-- Code in a table note is set plainly, without syntax highlighting: colour
+-- has no place in an APA table, and highlighted code is written as \VERB,
+-- which cannot be stored in a macro, so the note could not travel with its
+-- table.
+local function plaincode(blocks)
+  return pandoc.Blocks(blocks):walk {
+    Code = function(c)
+      if #c.classes > 0 then return pandoc.Code(c.text) end
+      return nil
+    end
+  }
+end
+
+-- The note as one run of inlines, paragraphs joined by \par -- so that it
+-- can sit in a table cell as a single line of source and the writer still
+-- writes every inline in it (italics, a language span, a citation).
+-- Returns nil for a note holding anything but paragraphs.
+local function noteinlines(blocks)
+  local out = pandoc.Inlines({})
+  local ok = true
+  local function add(bs)
+    for _, b in ipairs(bs) do
+      if b.t == "Div" then
+        add(b.content)
+      elseif b.t == "Para" or b.t == "Plain" then
+        if #out > 0 then out:insert(pandoc.RawInline("latex", "\\par ")) end
+        out:extend(b.content)
+      else
+        ok = false
+      end
+    end
+  end
+  add(blocks)
+  if not ok then return nil end
+  return out
+end
+
+local function prependraw(blocks, tex)
+  local raw = pandoc.RawInline("latex", tex)
+  local first = blocks[1]
+  if first and (first.t == "Plain" or first.t == "Para") then
+    first.content:insert(1, raw)
+    blocks[1] = first
+  else
+    blocks:insert(1, pandoc.Plain({ raw }))
+  end
+  return blocks
+end
+
+-- Put raw LaTeX at the start of a pandoc Table's first cell: the first
+-- header cell when there is a header row, otherwise the first body cell.
+-- Returns the table, or nil when it has no cell at all.
+--
+-- The Table stays a Table: the writer still writes it, and still sees every
+-- cell, so whatever a cell needs in the preamble is still set up. (Writing
+-- the table out here instead, with pandoc.write, was tried and broke exactly
+-- that: highlighted code or a language span in a cell left an undefined
+-- command and no PDF.)
+local function tablefirstcell(t, tex, headonly)
+  local head = t.head
+  if #head.rows > 0 then
+    local rows = head.rows
+    local row = rows[1]
+    local cells = row.cells
+    if #cells > 0 then
+      local cell = cells[1]
+      cell.contents = prependraw(cell.contents, tex)
+      cells[1] = cell
+      row.cells = cells
+      rows[1] = row
+      head.rows = rows
+      t.head = head
+      return t
+    end
+  end
+  if headonly then return nil end
+  local bodies = t.bodies
+  for bi, body in ipairs(bodies) do
+    local rows = body.body
+    if #rows > 0 and #rows[1].cells > 0 then
+      local row = rows[1]
+      local cells = row.cells
+      local cell = cells[1]
+      cell.contents = prependraw(cell.contents, tex)
+      cells[1] = cell
+      row.cells = cells
+      rows[1] = row
+      body.body = rows
+      bodies[bi] = body
+      t.bodies = bodies
+      return t
+    end
+  end
+  return nil
+end
+
+-- A pandoc Table's closing rule.
+--
+-- The writer's longtable draws its own \bottomrule in \endlastfoot, and the
+-- template appends an \hline at every \end{longtable} unless the table armed
+-- \apatablesownbottomrule -- so an unarmed markdown table outside a float
+-- printed TWO rules (every uncaptioned one always did; every captioned one
+-- would, now that apafloatlatex.lua lifts it out of its float). The marker
+-- goes in the first cell: the flag is global and is consumed only at this
+-- table's own \end{longtable}, so where inside the table it is set does not
+-- matter.
+--
+-- NOT inside a float. A table still inside \begin{table} (journal mode puts
+-- markdown tables in ThreePartTable) or a hand-written \begin{figure} cannot
+-- break, its \endlastfoot never reaches the page, and the \hline is its only
+-- closing rule; arming it would leave it with none. Floats are tracked per
+-- table, in document order, by counting the float environments the raw
+-- blocks before it open and close -- a self-contained raw float elsewhere in
+-- the same list changes nothing.
+-- The trailing {} ends the control-sequence name: without it the cell's
+-- own text runs on into it (\endcsnameA is one undefined command).
+local armtex = "\\csname apatablesownbottomrule\\endcsname{}"
+local floatenvs = {
+  table = true, figure = true, ThreePartTable = true, twocolumntable = true,
+  sidewaystable = true, sidewaysfigure = true, wraptable = true,
+  wrapfigure = true
+}
+
+local function floatdelta(tex)
+  local d = 0
+  for env in tex:gmatch("\\begin{(%a+)%*?}") do
+    if floatenvs[env] then d = d + 1 end
+  end
+  for env in tex:gmatch("\\end{(%a+)%*?}") do
+    if floatenvs[env] then d = d - 1 end
+  end
+  return d
+end
+
+local armlist
+armlist = function(blocks, depth)
+  for k, b in ipairs(blocks) do
+    if b.t == "RawBlock" and (b.format == "latex" or b.format == "tex") then
+      depth = depth + floatdelta(b.text)
+    elseif b.t == "Table" then
+      if depth <= 0 then
+        local t = tablefirstcell(b, armtex, false)
+        if t then blocks[k] = t end
+      end
+    elseif b.t == "Figure" then
+      b.content = armlist(b.content, depth + 1)
+      blocks[k] = b
+    elseif b.t == "Div" or b.t == "BlockQuote" then
+      b.content = armlist(b.content, depth)
+      blocks[k] = b
+    elseif b.t == "BulletList" or b.t == "OrderedList" then
+      local items = b.content
+      for ii, item in ipairs(items) do
+        items[ii] = armlist(item, depth)
+      end
+      b.content = items
+      blocks[k] = b
+    end
+  end
+  return blocks
+end
+
+-- A longtable that Quarto has already written out as raw LaTeX -- a
+-- knitr::kable() table in a captioned chunk is one -- and that
+-- apafloatlatex.lua lifted out of its float (it marks the Div) draws its own
+-- \bottomrule too, so it is armed the way apaTables' own tables are: a last
+-- row before \end{longtable}. Only when there is exactly one longtable, it
+-- has a \bottomrule (or, a plain kable table, ends with an \hline), and
+-- nothing armed it already.
+local liftmarker = "apa-lifted-longtable"
+
+local function armraw(blocks)
+  local n, armed, hasrule = 0, false, false
+  local texts = {}
+  pandoc.Blocks(blocks):walk {
+    RawBlock = function(rb)
+      if rb.format ~= "latex" then return nil end
+      local _, c = rb.text:gsub("\\begin{longtable}", "")
+      n = n + c
+      if rb.text:find("apatablesownbottomrule", 1, true) then armed = true end
+      if rb.text:find("\\bottomrule", 1, true) then hasrule = true end
+      texts[#texts + 1] = rb.text
+      return nil
+    end
+  }
+  -- A plain (non-booktabs) kable table closes with its own \hline. The
+  -- table can arrive split across raw blocks, so they are read together.
+  if table.concat(texts, "\n"):find("\\hline%s*\\end{longtable}") then
+    hasrule = true
+  end
+  if n ~= 1 or armed or not hasrule then return blocks end
+  local done = false
+  return pandoc.Blocks(blocks):walk {
+    RawBlock = function(rb)
+      if done or rb.format ~= "latex" then return nil end
+      local last, from = nil, 1
+      while true do
+        local a = rb.text:find("\\end{longtable}", from, true)
+        if not a then break end
+        last, from = a, a + 1
+      end
+      if not last then return nil end
+      done = true
+      return pandoc.RawBlock("latex", rb.text:sub(1, last - 1) ..
+        "\\noalign{" .. armtex .. "}\n" .. rb.text:sub(last))
+    end
+  }
+end
+
+-- A markdown table's note, carried inside the table.
+--
+-- Under endfloat (tables at the end) only the table environment travels, so
+-- a note left beside it stays behind in the body text. The note cannot be
+-- spliced into a pandoc Table's \endlastfoot as it is into a raw longtable:
+-- the writer writes that. Instead the note is defined, numbered, in a block
+-- of its own just before the table (the writer writes it, so italics, a
+-- citation or a language span are set up as anywhere else; it prints
+-- nothing), and the table carries only a marker naming that number --
+-- \apatablesnoteid{n}, in its first header cell, or in its caption when it
+-- has no header row. The template's marker hands the note to \endlastfoot,
+-- which prints it as the table's last foot row. Nothing fragile is ever put
+-- in a caption (whose argument cannot hold a paragraph break and is copied
+-- into the list of tables) or in a header cell (which the writer repeats,
+-- with footnotes stripped, for continuation pages).
+local noteseq = 0
+
+local function pendingnote(blocks, note)
+  local ntables = 0
+  pandoc.Blocks(blocks):walk {
+    Table = function(t) ntables = ntables + 1 end
+  }
+  if ntables ~= 1 then return nil end
+  local inl = noteinlines(note.content)
+  if not inl then return nil end
+  local id = tostring(noteseq + 1)
+  local marker = pandoc.RawInline("latex", "\\apatablesnoteid{" .. id .. "}")
+  local done = false
+  local out = pandoc.Blocks(blocks):walk {
+    Table = function(t)
+      if done then return nil end
+      local head = t.head
+      if #head.rows > 0 and #head.rows[1].cells > 0 then
+        local r = tablefirstcell(t, "\\apatablesnoteid{" .. id .. "}", true)
+        if r then done = true end
+        return r
+      end
+      local cap = t.caption
+      local long = cap.long
+      local last = long[#long]
+      if not last or (last.t ~= "Plain" and last.t ~= "Para") then
+        return nil
+      end
+      last.content:insert(marker)
+      long[#long] = last
+      cap.long = long
+      t.caption = cap
+      done = true
+      return t
+    end
+  }
+  if not done then return nil end
+  noteseq = noteseq + 1
+  local def = pandoc.Inlines({ pandoc.RawInline("latex",
+    "\\expandafter\\long\\expandafter\\gdef\\csname apatablesnote@" .. id ..
+    "\\endcsname{") })
+  def:extend(inl)
+  def:insert(pandoc.RawInline("latex", "}"))
+  out:insert(1, pandoc.Plain(def))
+  return out
+end
+
+-- A lifted raw longtable whose head carries its caption but has no
+-- \endfirsthead (Quarto writes a kable table that way) prints its number and
+-- title again at the top of every continuation page. Give it a first head:
+-- the rows before \endhead become the first page's head (caption included)
+-- and are repeated, without the caption, as the continuation head -- the
+-- shape the writer gives a markdown table and apaTables gives its own.
+local function splitfirsthead(blocks)
+  local hasfirst, hashead = false, false
+  pandoc.Blocks(blocks):walk {
+    RawBlock = function(rb)
+      if rb.format ~= "latex" then return nil end
+      if rb.text:find("\\endfirsthead", 1, true) then hasfirst = true end
+      if rb.text:find("\\endhead", 1, true) then hashead = true end
+      return nil
+    end
+  }
+  if hasfirst or not hashead then return blocks end
+  local done = false
+  return pandoc.Blocks(blocks):walk {
+    RawBlock = function(rb)
+      if done or rb.format ~= "latex" then return nil end
+      local a, b = rb.text:find("\\endhead", 1, true)
+      if not a then return nil end
+      local head = rb.text:sub(1, a - 1)
+      local again = head
+      local c = head:find("\\caption", 1, true)
+      if c then
+        local _, e = head:find("\\tabularnewline", c, true)
+        if not e then return nil end
+        again = head:sub(e + 1)
+      end
+      again = again:gsub("^%s*\\tabularnewline", ""):gsub("^%s+", "")
+      if again:find("\\begin{longtable", 1, true) then return nil end
+      done = true
+      return pandoc.RawBlock("latex", head .. "\\endfirsthead\n" .. again ..
+        rb.text:sub(a))
+    end
+  }
+end
+
+-- Store a note from the caption block of a raw longtable that Quarto wrote
+-- (a kable table): the caption is written by the writer, inside the table's
+-- first head, so the writer sees the note (a language span sets up babel)
+-- and the template's \endlastfoot prints it. Only when the table has a last
+-- foot to print it in; otherwise the stored note would reach the next table.
+-- Returns the blocks, or nil.
+local function captionnote(blocks, note, texts)
+  local haslast = false
+  for _, t in ipairs(texts) do
+    if t:find("\\endlastfoot", 1, true) then haslast = true end
+  end
+  if not haslast then return nil end
+  local inl = noteinlines(note.content)
+  if not inl then return nil end
+  local def = pandoc.Inlines({
+    pandoc.RawInline("latex", "\\long\\gdef\\apatablespendingnote{") })
+  def:extend(inl)
+  def:insert(pandoc.RawInline("latex", "}"))
+  local done = false
+  -- The caption block: Quarto writes it as a paragraph whose \caption is a
+  -- raw inline, inside a span. The stored note goes at its end, so it sits
+  -- in the caption's own cell of the first head and prints nothing there.
+  local function iscaption(b)
+    if b.t ~= "Plain" and b.t ~= "Para" then return false end
+    local hit = false
+    b:walk {
+      RawInline = function(x)
+        if x.text:find("^%s*\\caption") then hit = true end
+      end
+    }
+    return hit
+  end
+  local function visit(bs)
+    for k, b in ipairs(bs) do
+      if done then return end
+      if iscaption(b) then
+        b.content:extend(def)
+        bs[k] = b
+        done = true
+        return
+      elseif b.t == "Div" then
+        visit(b.content)
+      end
+    end
+  end
+  local out = pandoc.Blocks(blocks)
+  visit(out)
+  if done then return out end
+  return nil
+end
+
+local function processdiv(div)
+  local lifted = false
+  for k = #div.classes, 1, -1 do
+    if div.classes[k] == liftmarker then
+      div.classes:remove(k)
+      lifted = true
+    end
+  end
+  local i, note = findnote(div.content)
+  if not i then
+    if lifted then
+      div.content = splitfirsthead(armraw(div.content))
+      return div
+    end
+    return nil
+  end
+  -- Whatever happens below, the marker and the carried spacing are internal
+  -- and must not reach the writer.
+  local sibling = note.attributes[siblingattr] or ""
+  note.attributes[siblingattr] = nil
+  for k = #note.classes, 1, -1 do
+    if note.classes[k] == notemarker then note.classes:remove(k) end
+  end
+
+  -- Everything except the note; the note itself is going inside the table.
+  local rest = pandoc.Blocks({})
+  for j, b in ipairs(div.content) do
+    if j ~= i then rest:insert(b) end
+  end
+  if lifted then rest = splitfirsthead(armraw(rest)) end
+
+  -- First pass: read the table. The preamble and the head/foot declarations
+  -- are in different raw blocks, so both are collected before anything is
+  -- rewritten.
+  local spectext = nil
+  local ntables = 0
+  local texts = {}
+  rest:walk {
+    RawBlock = function(rb)
+      if rb.format ~= "latex" then return nil end
+      texts[#texts + 1] = rb.text
+      local _, found = rb.text:gsub("\\begin{longtable}", "")
+      ntables = ntables + found
+      if not spectext and found > 0 then
+        spectext = rb.text
+      end
+      return nil
+    end
+  }
+  note.content = plaincode(note.content)
+  local safe = not needswriter(note.content)
+  local storable = not hascode(note.content)
+  -- Exactly one longtable, or nothing is touched. The column count is read
+  -- from the FIRST longtable but the anchor is chosen across every raw block,
+  -- so with two longtables in one float the first table's column count can be
+  -- spliced into the second. Measured: in the overcount direction (first
+  -- {cccc}, second {cc}) that is "! Extra alignment tab has been changed to
+  -- \cr" and the render fails outright with no PDF. A stranded note is a far
+  -- better outcome than a document that will not build, so decline. Nothing
+  -- apaTables, flextable, kable, kableExtra or pandoc emits can reach this;
+  -- it takes hand-written raw LaTeX with two longtable environments inside
+  -- one apa-longtable float.
+  local injected = false
+  if safe and ntables == 1 and spectext then
+    local ncol, width, align = apalongtablespec(spectext)
+    local kind = apalongtablekind(texts)
+    if ncol and kind then
+      local row = apalongtablerow(ncol, width, align,
+        pandoc.write(pandoc.Pandoc(note.content), "latex"))
+      -- Second pass: rewrite the one block carrying the chosen anchor.
+      rest = rest:walk {
+        RawBlock = function(rb)
+          if injected or rb.format ~= "latex" then return nil end
+          local newtext = apalongtableinsert(rb.text, kind, row)
+          if not newtext then return nil end
+          injected = true
+          return pandoc.RawBlock("latex", newtext)
+        end
+      }
+    end
+  elseif storable and ntables == 0 then
+    local out = pendingnote(rest, note)
+    if out then
+      rest = out
+      injected = true
+    end
+  end
+
+  if not injected and not safe and storable and ntables == 1 then
+    local out = captionnote(rest, note, texts)
+    if out then
+      rest = out
+      injected = true
+    end
+  end
+
+  if injected then
+    div.content = rest
+    return div
+  end
+  -- Fallback: the table's preamble was one this code will not guess at, the
+  -- float held something other than one table, or the note needs the writer.
+  -- Leave the note where upstream put it, with upstream's spacing.
+  div.content = rest
+  div.content:insert(i, note)
+  if sibling ~= "" then
+    div.content:insert(i, pandoc.RawBlock("latex", sibling))
+  end
+  return div
+end
+
+return {
+  { Div = processdiv },
+  { Pandoc = function(doc)
+      doc.blocks = armlist(doc.blocks, 0)
+      return doc
+    end }
+}
diff --git a/_extensions/apaquarto/apaquarto.docx b/_extensions/apaquarto/apaquarto.docx
index 45c2659..b885f93 100644
Binary files a/_extensions/apaquarto/apaquarto.docx and b/_extensions/apaquarto/apaquarto.docx differ
diff --git a/_extensions/apaquarto/apatablelandscape.lua b/_extensions/apaquarto/apatablelandscape.lua
new file mode 100644
index 0000000..317a70e
--- /dev/null
+++ b/_extensions/apaquarto/apatablelandscape.lua
@@ -0,0 +1,240 @@
+-- apaTables' own docx landscape section break.
+--
+-- Quarto's OWN built-in `.landscape` div for docx (`/Applications/quarto/
+-- share/filters/main.lua`, landscape.lua) closes its rotated section with a
+-- COMPLETELY EMPTY sectPr -- no page size, no margins, no header
+-- references -- so every page BEFORE a rotated table silently loses the
+-- running head and page number, not merely the rotated page itself. Its
+-- rotated page size is also hardcoded A4 regardless of the manuscript's own
+-- paper. Both measured by rendering.
+--
+-- apaTables never uses `.landscape`. apa_landscape_wrap() (R/quarto-
+-- table.R) emits a DIFFERENT class instead, with the document's real page
+-- geometry -- read out of the bundled reference document by
+-- apa_docx_reference_sectpr() (R/docx-landscape.R), never hardcoded --
+-- carried as the div's own attributes:
+--
+--   ::: {.apa-table-landscape pgw="12240" pgh="15840" mt="1440" mb="1440"
+--                              ml="1440" mr="1440" mh="720" mf="720" mg="0"}
+--   <the table's own grid markdown>
+--   :::
+--
+-- This filter finds that div and replaces it with THREE things in order: a
+-- portrait closer carrying the real page (not empty), the div's own content
+-- unwrapped, and a landscape closer carrying the SAME real page with width
+-- and height swapped (not a hardcoded A4). Both closers carry the same
+-- three header references as the document's own default section, so the
+-- running head and page number survive on every page -- the rotated one and
+-- every page before it.
+--
+-- THE MARGINS ROTATE WITH THE PAGE -- measured in real Microsoft
+-- Word, not inferred. See process_div() below for the mapping and the
+-- measurement.
+--
+-- HEADER/FOOTER RELATIONSHIP IDs ARE HARDCODED HERE, DELIBERATELY. They
+-- are STABLE under pandoc's own docx writer (measured): it preserves the
+-- reference document's own header/footer relationships and allocates every
+-- new one ABOVE them -- a probe with twelve new relationships (three
+-- images, three hyperlinks, a footnote, two citations with a bibliography,
+-- three cross-references, a pipe table and a landscape div) left
+-- rId8..rId13 untouched and used rId14..rId32. Since apaTables BUNDLES
+-- apaquarto.docx, those IDs are ours to know, and reusing them here creates
+-- no new relationship at all -- both closers simply point at the same
+-- header/footer parts every other section already uses.
+--
+-- FOUND BY A Pandoc-LEVEL WALK, NOT A FloatRefTarget HANDLER, AND THAT IS
+-- DELIBERATE. apatablesize.lua's sibling marker div is always inside a
+-- `tbl-` labelled chunk's float, so a FloatRefTarget handler is enough
+-- there. This div is not: apa_landscape_wrap() wraps whatever markdown the
+-- chunk emitted regardless of whether that chunk has a crossref-able
+-- label, so it can end up nested inside a FloatRefTarget's content (a
+-- labelled chunk, later deferred to the document's end by
+-- apafloatstoend.lua, as apaquarto does by default) OR sitting directly in
+-- the ordinary block list (an unlabelled chunk, never a float at all).
+-- MEASURED: `doc.blocks:walk{Div = fn}` from a `Pandoc`
+-- handler fires in BOTH cases with no branch needed -- pandoc's own walk
+-- recurses into a FloatRefTarget's content automatically. A handler scoped
+-- to FloatRefTarget only would silently do nothing for the unlabelled case.
+--
+-- ALSO MEASURED: this combines cleanly with
+-- apatablesize.lua's shrink ladder running in the same render, in
+-- either registration order -- the two filters touch disjoint things (a
+-- Table's custom-style attribute and its own sibling marker div; this
+-- one's own div and nothing else), so a table that is BOTH rotated AND
+-- shrunk came out with the right table style AND the right section breaks,
+-- verified by rendering to a real .docx and reading word/document.xml back.
+--
+-- NUMBERED-LINES INTERACTION. apaquarto's docxlinenumber.lua turns
+-- `numbered-lines: true` into line numbering by appending ONE closer of its
+-- own, at the very end of the document, carrying `<w:lnNumType>` and
+-- nothing else -- no page size, no margins, no header references. A
+-- `sectPr` governs content back to the PREVIOUS closer, so the two closers
+-- this filter inserts put the pre-rotation content and the rotated table
+-- in sections docxlinenumber.lua's closer never reaches. MEASURED in real
+-- Word (16.111) without the handling below: no line numbers on the pages
+-- this filter's two sections govern, and a counter starting fresh at "1"
+-- on the first page docxlinenumber.lua's closer governs. So this filter
+-- reads `numbered-lines` off the document metadata (the same key and
+-- stringify check docxlinenumber.lua itself uses) and carries
+-- `<w:lnNumType>` in BOTH of its own closers when set; real Word then
+-- numbers continuously from page 1 through the rotated table and beyond.
+--
+-- NOT A DEFECT, DESPITE APPEARANCES: docxlinenumber.lua's own closer ALSO
+-- omits headerReference and pgSz, the same textual shape as Quarto's empty
+-- `.landscape` sectPr above -- but MEASURED IN REAL MICROSOFT WORD, the
+-- running head, page number and correct page size ALL appear on every page
+-- that closer governs, with or without a rotated table elsewhere. Word
+-- treats an "incomplete but not entirely empty" sectPr differently from
+-- Quarto's `.landscape` sectPr, which has ZERO child elements. The
+-- mechanism is not explained here and nothing in this filter depends on
+-- it; the XML shape alone is not evidence of a defect.
+--
+-- IMPORTANT: this script returns EXACTLY the plain, universal apaquarto
+-- filter shape -- an ARRAY of filter passes, each with only node-type keys,
+-- `{ { Meta = read_meta }, { Pandoc = process_pandoc } }` -- the same
+-- two-pass shape apafloatlatex.lua already uses to read `documentmode`
+-- before its own FloatRefTarget pass runs. A filter returning a table with
+-- a stray non-array key is silently SKIPPED by Quarto's own filter loader
+-- (main.lua) -- confirmed more than once -- while
+-- bare `pandoc --lua-filter=` and `pandoc lua` both tolerate the same stray
+-- key, so verifying against either is not verifying; this was checked only
+-- through an end-to-end `quarto render`.
+
+if FORMAT ~= "docx" then
+  return
+end
+
+local classname = "apa-table-landscape"
+
+-- Set by read_meta() before the Pandoc pass runs (see the file banner's
+-- "NUMBERED-LINES INTERACTION" note). Same key and stringify check
+-- docxlinenumber.lua itself uses, so the two filters agree on what
+-- "numbered-lines: true" means.
+local numbered_lines = false
+
+local function read_meta(meta)
+  if meta["numbered-lines"] and
+      pandoc.utils.stringify(meta["numbered-lines"]) == "true" then
+    numbered_lines = true
+  end
+end
+
+-- rId8/rId9/rId12 (headers) and rId10/rId11/rId13 (footers) -- see the file
+-- banner for why these are safe to hardcode.
+local REFS = {
+  { tag = "headerReference", type = "even",    id = "rId8"  },
+  { tag = "headerReference", type = "default", id = "rId9"  },
+  { tag = "footerReference", type = "even",    id = "rId10" },
+  { tag = "footerReference", type = "default", id = "rId11" },
+  { tag = "headerReference", type = "first",   id = "rId12" },
+  { tag = "footerReference", type = "first",   id = "rId13" },
+}
+
+local function refs_xml()
+  local out = {}
+  for _, r in ipairs(REFS) do
+    out[#out + 1] = string.format('<w:%s w:type="%s" r:id="%s"/>',
+                                  r.tag, r.type, r.id)
+  end
+  return table.concat(out)
+end
+
+-- One section-closing paragraph: <w:p><w:pPr><w:sectPr>...</w:sectPr>
+-- </w:pPr></w:p>, the OOXML idiom that ends the section everything back to
+-- the PREVIOUS closer (or the start of the document) belongs to. `w`/`h`
+-- are handed in already in the order this section wants them -- the
+-- landscape closer's caller passes them swapped, this function does not
+-- swap anything itself.
+local function sectpr_xml(a, w, h, landscape)
+  local pgsz
+  if landscape then
+    pgsz = string.format('<w:pgSz w:w="%s" w:h="%s" w:orient="landscape"/>',
+                         w, h)
+  else
+    pgsz = string.format('<w:pgSz w:w="%s" w:h="%s"/>', w, h)
+  end
+  local pgmar = string.format(
+    '<w:pgMar w:top="%s" w:right="%s" w:bottom="%s" w:left="%s" ' ..
+    'w:header="%s" w:footer="%s" w:gutter="%s"/>',
+    a["mt"], a["mr"], a["mb"], a["ml"], a["mh"], a["mf"], a["mg"])
+  -- Same element and attribute values docxlinenumber.lua itself writes,
+  -- so numbering is continuous across every section of the document
+  -- rather than restarting (or stopping) at this closer.
+  local lnnum = numbered_lines
+    and '<w:lnNumType w:countBy="1" w:restart="continuous" />' or ""
+  -- The reference document's OWN trailing sectPr carries <w:cols
+  -- w:space="720"/> (read straight out of apaquarto.docx's
+  -- word/document.xml). Word defaults to a single column when the element
+  -- is absent, so omitting it changed nothing visible -- but these closers
+  -- claim to carry "the document's real section properties", and this is
+  -- one of them. Emitted AFTER lnNumType because CT_SectPr is a strict
+  -- SEQUENCE (... pgMar, paperSrc, pgBorders, lnNumType, pgNumType, cols
+  -- ...), the same schema-order trap as <w:rPr> in the docx styles: an
+  -- element in the wrong position is not merely ignored, it can
+  -- make Word reject the part.
+  local cols = '<w:cols w:space="720"/>'
+  return "<w:p><w:pPr><w:sectPr>" .. refs_xml() .. pgsz .. pgmar .. lnnum ..
+    cols .. "</w:sectPr></w:pPr></w:p>"
+end
+
+local function process_div(div)
+  if not div.classes:includes(classname) then
+    return nil
+  end
+  local a = div.attributes
+  local w, h = a["pgw"], a["pgh"]
+  -- Every attribute apa_landscape_wrap() writes is required. Missing any of
+  -- them means the div was not built by that function (or R declined to
+  -- rotate because it could not read the reference document's geometry and
+  -- left the table unwrapped instead -- in which case this div does not
+  -- exist at all). Either way, guessing a page size here would repeat
+  -- Quarto's own defect (file banner), so this leaves the div's content
+  -- untouched rather than writing a section with invented numbers.
+  if not (w and h and a["mt"] and a["mb"] and a["ml"] and a["mr"]
+          and a["mh"] and a["mf"] and a["mg"]) then
+    return nil
+  end
+  local closer_portrait  = pandoc.RawBlock("openxml", sectpr_xml(a, w, h, false))
+  -- THE MARGINS ROTATE WITH THE PAGE. MEASURED IN REAL MICROSOFT WORD, not
+  -- inferred from the numbers: a portrait section with four
+  -- DISTINCT margins, flipped to landscape by Word itself, went from
+  --   <w:pgMar w:top="1440" w:right="1440" w:bottom="1080" w:left="2160"/>
+  -- to
+  --   <w:pgMar w:top="1440" w:right="1080" w:bottom="2160" w:left="1440"/>
+  -- i.e. new top <- old right, new right <- old bottom, new bottom <- old
+  -- left, new left <- old top. `bottom` and `left` were unique values, so
+  -- this is the only mapping consistent with the result -- the clockwise
+  -- alternative would have put 2160 in `top` and did not.
+  --
+  -- apaquarto.docx's own margins are symmetric (1440 all round), so this is
+  -- a NO-OP for the bundled reference document. It matters for a user who
+  -- supplies their own, which is supported -- and an unequal margin is
+  -- almost always a physical constraint (a thesis binding edge, a printer's
+  -- gripper margin) that a rotation has to carry round with the page.
+  -- Leaving them alone is a DIVERGENCE FROM WORD, not neutrality: the same
+  -- reference document would give one page to a user who rotates a section
+  -- by hand and a different one to a table apaTables rotates.
+  --
+  -- Swapped HERE, at the call site, exactly as w/h already are: sectpr_xml()
+  -- deliberately transforms nothing itself.
+  local rotated_margins = {
+    mt = a["mr"], mr = a["mb"], mb = a["ml"], ml = a["mt"],
+    mh = a["mh"], mf = a["mf"], mg = a["mg"],
+  }
+  local closer_landscape = pandoc.RawBlock("openxml",
+                                           sectpr_xml(rotated_margins, h, w, true))
+  local out = pandoc.Blocks({ closer_portrait })
+  out:extend(div.content)
+  out:insert(closer_landscape)
+  return out
+end
+
+local function process_pandoc(doc)
+  doc.blocks = doc.blocks:walk({ Div = process_div })
+  return doc
+end
+
+return {
+  { Meta = read_meta },
+  { Pandoc = process_pandoc }
+}
diff --git a/_extensions/apaquarto/apatablelandscapecaption.lua b/_extensions/apaquarto/apatablelandscapecaption.lua
new file mode 100644
index 0000000..104bc96
--- /dev/null
+++ b/_extensions/apaquarto/apatablelandscapecaption.lua
@@ -0,0 +1,190 @@
+-- Without this filter a rotated table's CAPTION belongs to the portrait
+-- section while its table belongs to the landscape one, so the caption prints
+-- alone on a portrait page and the table on the page after it. And between
+-- two rotated tables the document returns to portrait for no reason, which
+-- opens a BLANK page. Both seen in real Microsoft Word, not inferred.
+--
+-- A `sectPr` embedded in a paragraph governs everything back to the PREVIOUS
+-- such paragraph. apatablelandscape.lua writes the portrait closer immediately
+-- before the table, which puts the caption -- emitted earlier -- on the wrong
+-- side of it. Moving the closer ABOVE the caption puts both in one section,
+-- with no change to what either closer says.
+--
+-- WHY NOT IN apatablelandscape.lua: that filter runs at post-quarto, and
+-- MEASURED there the caption is not a
+-- caption yet -- an unclassed Div holding bare text, four levels away from the
+-- closers, with no FigureTitle and no Caption class. It is only flat, classed
+-- and adjacent at post-render.
+--
+-- WHY AFTER apatablelandscapenote.lua. That filter finds a rotated float by
+-- testing that its LAST content block is the landscape closer, which
+-- merge_runs() below removes from every float in a run but the last -- so
+-- running first stops it recognising those floats.
+--
+-- THAT ORDER IS NOT LOAD-BEARING FOR WHAT A READER SEES, though it looks it:
+-- reversing it does NOT strand notes on a portrait page. Rendered both ways,
+-- the two documents differ in exactly one respect: the position of a
+-- zero-width <w:bookmarkEnd/> relative to a note paragraph. The reason is not
+-- obvious. A note the note filter fails to relocate stays a SIBLING
+-- immediately after its float -- and merging is precisely what DELETES the
+-- closer that used to separate them, so the note is already inside the
+-- landscape section. Removing a boundary cannot strand anything past it.
+--
+-- The order is kept: it is the one verified end to end, and a future change
+-- that stops removing closers would revive the dependency.
+if FORMAT ~= "docx" then
+  return
+end
+
+local function is_portrait_closer(b)
+  return b and b.t == "RawBlock" and b.format == "openxml"
+    and b.text:find("<w:sectPr", 1, true) ~= nil
+    and b.text:find('w:orient="landscape"', 1, true) == nil
+end
+
+-- Move the portrait closer above the caption paragraphs. Narrow by
+-- construction: only a Div that already CONTAINS this extension's own portrait
+-- closer is touched, so an upright float -- which has no closers at all --
+-- can never match.
+local function hoist(div)
+  for i, c in ipairs(div.content) do
+    if is_portrait_closer(c) and i > 1 then
+      local closer = table.remove(div.content, i)
+      div.content:insert(1, closer)
+      return div
+    end
+  end
+  return nil
+end
+
+local function is_landscape_closer(b)
+  return b and b.t == "RawBlock" and b.format == "openxml"
+    and b.text:find('w:orient="landscape"', 1, true) ~= nil
+end
+
+local function is_page_break(b)
+  return b and b.t == "RawBlock" and b.format == "openxml"
+    and b.text:find('w:type="page"', 1, true) ~= nil
+end
+
+-- The Div that DIRECTLY holds the closers. It is nested inside the top-level
+-- float wrapper: measured, Div[cell,FigureWithoutNote] > Div#tbl-rot.
+local function holder(b)
+  if b.t ~= "Div" then return nil end
+  for _, c in ipairs(b.content) do
+    if is_portrait_closer(c) then return b end
+  end
+  for _, c in ipairs(b.content) do
+    local h = holder(c)
+    if h then return h end
+  end
+  return nil
+end
+
+-- Collapse each RUN of consecutive rotated floats into ONE landscape section:
+-- drop every float's landscape closer but the last, and every float's portrait
+-- closer but the first. The page break between floats stays and becomes an
+-- ordinary page break INSIDE the landscape section.
+--
+-- Without this, the document returns to portrait between two landscape tables
+-- for no reason, and that empty portrait stretch IS a blank page in Word --
+-- seen in the CONTROL renders, so it predates this filter.
+--
+-- HOLDERS ARE RESOLVED BEFORE ANY MUTATION, and that is load-bearing --
+-- measured twice, by two different failures.
+--
+-- Replace `holders[i], holders[j]` with `holder(doc.blocks[i]),
+-- holder(doc.blocks[j])` and the merge STOPS TAKING EFFECT: a document with
+-- two consecutive rotated tables comes back completely unmerged, two landscape
+-- closers and five sections. An earlier prototype that resolved lazily in a
+-- slightly different way merged a run of THREE only partially -- two landscape
+-- sections instead of one.
+--
+-- The working explanation is pandoc's block-wrapper identity: a Div fetched
+-- fresh out of doc.blocks is not guaranteed to be the object whose mutations
+-- are written back, so edits applied to a re-fetched wrapper can be silently
+-- discarded. Stated as the explanation that fits both measurements, NOT as
+-- something established from pandoc's internals -- what is established is that
+-- resolving up front works and resolving lazily does not.
+local function merge_runs(doc)
+  local floats, holders = {}, {}
+  for i, b in ipairs(doc.blocks) do
+    local h = holder(b)
+    if h then
+      table.insert(floats, i)
+      holders[i] = h
+    end
+  end
+  for k = 1, #floats - 1 do
+    local i, j = floats[k], floats[k + 1]
+    -- "Consecutive" means nothing between them but page breaks. An upright
+    -- float ends the run.
+    local adjacent = true
+    for m = i + 1, j - 1 do
+      if not is_page_break(doc.blocks[m]) then adjacent = false end
+    end
+    if adjacent then
+      local a1, a2 = holders[i], holders[j]
+      if a1 and a2 then
+        if is_landscape_closer(a1.content[#a1.content]) then
+          table.remove(a1.content)            -- stay landscape
+        end
+        if is_portrait_closer(a2.content[1]) then
+          table.remove(a2.content, 1)         -- do not re-open portrait
+        end
+      end
+    end
+  end
+end
+
+-- Does this block END with the landscape closer -- i.e. is it the last float of
+-- a rotated run? The closer is the last content of the float's own Div, which
+-- is in turn the last content of the wrapper Quarto puts around it.
+local function ends_landscape(b)
+  if b.t ~= "Div" or #b.content == 0 then return false end
+  local last = b.content[#b.content]
+  if is_landscape_closer(last) then return true end
+  return ends_landscape(last)
+end
+
+-- Drop the page break that apafloatstoend.lua emits between a rotated run and
+-- whatever float follows it. The landscape closer ALREADY starts a new page --
+-- that is what a section break does -- so the break becomes the first content
+-- of the new portrait section and opens a page of its own: a blank one,
+-- carrying the running head and nothing else.
+--
+-- SEEN IN REAL WORD and PRE-EXISTING: page 9 of a three-rotated-tables
+-- manuscript, page 5 of one with an upright table between two rotated ones.
+-- Identical in the CONTROL renders, so this filter did not cause it; the same
+-- family as the blank page between two rotated tables, at the other boundary.
+--
+-- NOT the same as the break BEFORE a rotated float, which is harmless: there
+-- the break lands at the END of a section and Word collapses it against the
+-- section break's own page (measured). Position decides
+-- it, so do not "simplify" the two cases together.
+--
+-- A break BETWEEN two floats of the same run is kept: merging removed the
+-- closer that would have preceded it, so it is an ordinary page break inside
+-- the landscape section, which is exactly what puts each table on its own page.
+local function drop_break_after_run(doc)
+  for i = #doc.blocks - 1, 1, -1 do
+    if ends_landscape(doc.blocks[i]) and is_page_break(doc.blocks[i + 1]) then
+      table.remove(doc.blocks, i + 1)
+    end
+  end
+end
+
+return {
+  {
+    Pandoc = function(doc)
+      -- hoist FIRST: merge_runs reads the hoisted layout, in which the
+      -- portrait closer is content[1] of the float's holding Div. And
+      -- drop_break_after_run LAST: it asks which float ends a run, which is
+      -- only settled once merge_runs has removed the interior closers.
+      doc.blocks = doc.blocks:walk({ Div = hoist })
+      merge_runs(doc)
+      drop_break_after_run(doc)
+      return doc
+    end
+  }
+}
diff --git a/_extensions/apaquarto/apatablelandscapenote.lua b/_extensions/apaquarto/apatablelandscapenote.lua
new file mode 100644
index 0000000..74351bc
--- /dev/null
+++ b/_extensions/apaquarto/apatablelandscapenote.lua
@@ -0,0 +1,80 @@
+-- Companion to apatablelandscape.lua: moves a rotated table's note
+-- INSIDE its own landscape section.
+--
+-- MEASURED, twice over. apatablelandscape.lua
+-- (post-quarto) replaces the `::: {.apa-table-landscape}` div with THREE
+-- things, in this order, as direct children of the float's own Div (e.g.
+-- `tbl-wide14`): a portrait closer, the table, a landscape closer. apanote.lua
+-- (post-render, common) runs LATER and inserts the note as a SIBLING
+-- immediately AFTER that whole float Div, one level UP -- not inside it.
+-- Read off a real render (fixtures/landscape-shrunk.qmd, a `Blocks` dump at
+-- post-render just after apanote.lua):
+--
+--   15.1 Div id=tbl-wide14         portrait-closer, Table, landscape-closer
+--   15.2 Div (unclassed)           Div.FigureNote, Div.FigureNote
+--
+-- Under OOXML, a `sectPr` embedded in a paragraph governs everything back to
+-- the PREVIOUS such paragraph. The note sits textually AFTER the landscape
+-- closer, so it belongs to the section that follows -- the manuscript's own
+-- portrait default -- not to the rotated section its table is on. MEASURED
+-- in real Microsoft Word: landscape-numbered-lines put its table on page 3
+-- (landscape) and its note ALONE on page 4 (portrait); landscape-shrunk put
+-- its table on pages 3-4 (landscape) and its note on page 5 (portrait).
+--
+-- The fix moves the note INSIDE the float Div, between the table and the
+-- landscape closer, and removes the now-empty sibling wrapper -- so the
+-- closer keeps closing the SAME section, just with the note now inside it.
+--
+-- A SEPARATE FILE, leaving apatablelandscape.lua untouched: this only reads
+-- the note div apanote.lua already built and repositions it, and its match
+-- is narrow -- it fires only when a
+-- Div's LAST content block is literally the landscape-closer RawBlock this
+-- extension itself writes, immediately followed by a sibling matching
+-- apanote.lua's own note-wrapper shape. Every other float -- non-rotated,
+-- rotated with no note, rotated with an explicit non-generated note -- has
+-- no such trailing RawBlock and is untouched (verified by rendering all
+-- four in one document).
+if FORMAT ~= "docx" then
+  return
+end
+
+local function is_landscape_closer(b)
+  return b.t == "RawBlock" and b.format == "openxml" and
+    b.text:find('w:orient="landscape"', 1, true) ~= nil
+end
+
+local function is_note_wrapper(b)
+  if not b or b.t ~= "Div" then return false end
+  for _, c in ipairs(b.content) do
+    if c.t == "Div" and c.classes:includes("FigureNote") then return true end
+  end
+  return false
+end
+
+local function relocate_note(blocks)
+  local changed = false
+  -- Iterate DOWNWARD, not `for i = 1, #blocks - 1`. Lua's numeric-for
+  -- evaluates its limit ONCE, before the loop starts; `table.remove(blocks,
+  -- i + 1)` below shrinks `blocks` by one element per relocation, so a
+  -- SECOND relocation later in the same `Blocks` list would index past the
+  -- new (shorter) end and error on nil -- a hard `quarto render` failure.
+  -- Unreachable today (every float/note pair sits in its own `Blocks` list,
+  -- verified by rendering multiple rotated+noted floats in one document),
+  -- but it is a one-line fix: counting down means every removal happens at
+  -- an index ABOVE the current `i`, which this loop has already passed and
+  -- will never revisit, so the shrinking list never invalidates `i` itself.
+  for i = #blocks - 1, 1, -1 do
+    local b = blocks[i]
+    if b.t == "Div" and #b.content > 0 and is_landscape_closer(b.content[#b.content])
+        and is_note_wrapper(blocks[i + 1]) then
+      local closer = table.remove(b.content)      -- pull the landscape closer out
+      local note = table.remove(blocks, i + 1)     -- pull the note sibling out
+      b.content:extend(note.content)               -- note now inside the section
+      b.content:insert(closer)                     -- closer goes back on the end
+      changed = true
+    end
+  end
+  if changed then return blocks end
+end
+
+return { { Blocks = relocate_note } }
diff --git a/_extensions/apaquarto/apatablemeta-decide.lua b/_extensions/apaquarto/apatablemeta-decide.lua
new file mode 100644
index 0000000..ad37685
--- /dev/null
+++ b/_extensions/apaquarto/apatablemeta-decide.lua
@@ -0,0 +1,16 @@
+-- The precedence matrix for apatablemeta.lua's title/note defaults, split
+-- into its own file so a plain `pandoc lua` process can dofile the REAL
+-- function directly. FloatRefTarget (used by the rest of the filter, in
+-- apatablemeta.lua) is a Quarto custom AST node that plain pandoc cannot
+-- construct, so this file holds the only part of that filter testable
+-- outside Quarto -- but it IS the whole precedence matrix.
+local M = {}
+
+-- Return the value to set, or nil to leave what is there.
+function M.decide(existing, candidate)
+  if candidate == nil or candidate == "" then return nil end
+  if existing ~= nil and existing ~= "" then return nil end
+  return candidate
+end
+
+return M
diff --git a/_extensions/apaquarto/apatablemeta.lua b/_extensions/apaquarto/apatablemeta.lua
new file mode 100644
index 0000000..f487e42
--- /dev/null
+++ b/_extensions/apaquarto/apatablemeta.lua
@@ -0,0 +1,63 @@
+-- apaTables emits an empty ::: {.apa-table-meta} div carrying the title and
+-- note it generated. This filter puts them on the float ONLY where the author
+-- supplied nothing, then removes the div. Precedence is therefore not coded:
+-- a chunk-level tbl-cap means the float already has a caption, and a
+-- chunk-level apa-note means the attribute already exists, so both are left
+-- alone.
+--
+-- The decision logic (M.decide) lives in the sibling file
+-- apatablemeta-decide.lua so it can be unit-tested with a plain `pandoc lua`
+-- process: FloatRefTarget below is a Quarto custom AST node that plain
+-- pandoc cannot construct, so that sibling file holds the only part of this
+-- filter that is testable outside Quarto.
+--
+-- IMPORTANT: this script returns EXACTLY the plain, universal apaquarto
+-- filter shape, `{ { FloatRefTarget = processfloat } }`, with no extra
+-- keys -- matching every sibling filter (apafloatlatex.lua and the rest).
+-- Do NOT attach `decide` as an extra key on the returned table so a bare
+-- `pandoc lua` harness can reach it: bare `pandoc --lua-filter=` and
+-- `pandoc lua` tolerate the stray key, but under Quarto's own filter loader
+-- (main.lua) `processfloat` is then invoked zero times -- exit 0, PDF
+-- produced, caption and note both silently empty (measured). Loading the
+-- decision logic from a sibling file, the same way every other apaquarto
+-- filter loads utilsapa.lua, avoids the problem and is confirmed end to end
+-- with a real Quarto render.
+local scriptdir = PANDOC_SCRIPT_FILE:match("(.*[/\\])") or "./"
+local M = dofile(scriptdir .. "apatablemeta-decide.lua")
+
+local metaclass = "apa-table-meta"
+
+local function processfloat(float)
+  local title, note, found
+  local stripped = pandoc.walk_block(pandoc.Div(float.content), {
+    Div = function(d)
+      if d.classes:includes(metaclass) then
+        found = true
+        title = d.attributes["apa-title"]
+        note  = d.attributes["apa-note"]
+        return {}
+      end
+    end
+  })
+  -- Nothing to do: no meta div at all (nothing to strip, nothing to set).
+  if not found then return nil end
+  float.content = stripped.content
+
+  local setnote = M.decide(float.attributes["apa-note"], note)
+  if setnote then float.attributes["apa-note"] = setnote end
+
+  local existing = nil
+  if float.caption_long and #float.caption_long.content > 0 then
+    existing = pandoc.utils.stringify(float.caption_long)
+  end
+  local settitle = M.decide(existing, title)
+  if settitle then
+    local blocks = pandoc.read(settitle, "markdown").blocks
+    if #blocks > 0 then
+      float.caption_long = pandoc.Plain(blocks[1].content)
+    end
+  end
+  return float
+end
+
+return { { FloatRefTarget = processfloat } }
diff --git a/_extensions/apaquarto/apatablesize.lua b/_extensions/apaquarto/apatablesize.lua
new file mode 100644
index 0000000..88488e2
--- /dev/null
+++ b/_extensions/apaquarto/apatablesize.lua
@@ -0,0 +1,248 @@
+-- apaTables' Word table-size ladder. apa_quarto_markdown_block()
+-- (R/quarto-table.R) emits a table at its DEFAULT 10 pt with zero per-cell
+-- markup -- the reference document's "Table" table style already carries
+-- 10 pt -- and for any other size appends an empty sibling marker
+-- div immediately after the grid table's own markdown:
+--
+--   ::: {.apa-table-size-9}
+--   :::
+--
+-- naming the target point size. This filter finds that marker inside the
+-- same float, reads the size, selects the matching cloned table style
+-- (Table6-Table9, Table11, Table12, each a clone of "Table" differing only in
+-- <w:sz>/<w:szCs>) as the Table AST node's custom-style, and removes the
+-- marker div so it leaves no trace in the rendered document.
+--
+-- MUST be a Lua filter operating on the AST, not the markdown-level
+-- `: {custom-style="TableN"}` caption-attribute syntax: that syntax works
+-- under bare pandoc but is SILENTLY DEFEATED by Quarto's own crossref/float
+-- pipeline under a real `quarto render` (measured).
+--
+-- Registration mirrors docxstyler.lua (post-quarto, docx only).
+--
+-- FOUND BY A Pandoc-LEVEL WALK, NOT A FloatRefTarget HANDLER. Only a
+-- `tbl-` labelled chunk's table lives inside a FloatRefTarget at this
+-- stage, but apa_quarto_markdown_block() emits the size marker whatever the
+-- chunk's label (apatablelandscape.lua finds its own marker the same way,
+-- for the same reason -- see that file's banner). A FloatRefTarget-only
+-- handler here silently does nothing for an unlabelled table: a wide one
+-- would rotate but keep the "Table" style at 10 pt, leaving an inert
+-- `apa-table-size-N` div in the rendered docx. MEASURED: a `Pandoc`-level
+-- walk recurses into a `FloatRefTarget`'s content automatically, so ONE
+-- handler covers both the labelled and unlabelled cases.
+--
+-- The walk handles `Blocks` (a whole sibling list at a time), NOT `Div` and
+-- `Table` as two separate document-wide passes -- see `process_blocks()`'s
+-- own comment below for why.
+--
+-- IMPORTANT: this script returns EXACTLY the plain, universal apaquarto
+-- filter shape, `{ { Pandoc = process_pandoc } }`, with no extra keys. A
+-- filter returning a table with a stray non-array key is silently SKIPPED
+-- by Quarto's own filter loader (main.lua) -- confirmed more than once
+-- (see apatablemeta.lua) -- while bare
+-- `pandoc --lua-filter=` and `pandoc lua` both tolerate the same stray key,
+-- so verifying against either is not verifying; this was checked only
+-- through an end-to-end `quarto render`.
+
+-- This filter serves TWO destinations. Word takes its size from a cloned
+-- table STYLE; HTML takes it from a wrapper DIV that this filter builds
+-- HERE, at post-quarto, after apaquarto's own float collection has already
+-- run, paired with a `.apa-table-size > table { font-size: inherit; }` rule
+-- in apa.css -- NOT a style attribute set directly on the Table node, which
+-- was MEASURED to be silently discarded by Quarto's HTML pipeline before
+-- the page is written (see the comment on the apply step below).
+--
+-- No early return for other formats: the walk still runs everywhere so that
+-- a marker div is always CONSUMED, never left as dead weight in an output
+-- this filter does not otherwise serve. Only the APPLY step branches.
+local is_docx = FORMAT == "docx"
+local is_html = FORMAT:match("^html") ~= nil
+
+local sizeclass = "^apa%-table%-size%-(%d+)$"
+local sizeattr = "apa-table-size-pt"
+-- Mirrors APA_QUARTO_HTML_CLASS in R/quarto-table.R: R
+-- writes this class on the marker div, this filter turns it into the wrapper,
+-- and apa.css scopes the table-width rules to it. Three copies of one name,
+-- unavoidably -- they are checked against each other by
+-- tests/testthat/test-html-structure.R on a real render, which is the only
+-- place all three meet.
+local htmlclass = "apa-table-html"
+
+-- The only sizes with a cloned table style in the reference document are
+-- Table6-Table9, Table11 and Table12. R's apa_quarto_size_rung() already
+-- refuses to write a marker naming anything else, but this filter is a
+-- bundled, independently loadable artifact -- another caller, a future R
+-- change, or a hand-authored `::: {.apa-table-size-5}` in someone's own
+-- document must not be trusted just because today's only caller behaves.
+-- Concatenating ANY digit string blindly would write custom-style="Table5"
+-- into the docx for a style that was never cloned, and Word's response to
+-- an unknown table style is to drop the style assignment ENTIRELY -- losing
+-- the APA rules and cell margins along with the requested size. So the
+-- check is repeated here, independently.
+-- The range is 6-12, and there is deliberately NO "10". The reference
+-- document's unmodified "Table" style already carries 10 pt, so a
+-- table wanting 10 pt gets it by carrying NO marker at all; a "Table10"
+-- style does not exist and must not be named. R's apa_quarto_size_rung()
+-- enforces the same rule, and tests/testthat/test-word-size-styles.R
+-- asserts this list and the styles actually in apaquarto.docx agree.
+local valid_sizes = {
+  ["6"] = true, ["7"] = true, ["8"] = true, ["9"] = true,
+  ["11"] = true, ["12"] = true,
+}
+
+-- ONE marker div per TABLE, scoped to the Blocks LIST that table and its
+-- own marker actually sit in together. Two document-wide passes (a Div
+-- walk finding markers into ONE shared `size`, then a Table walk applying
+-- it to EVERY Table node) is a LAST-MARKER-WINS bug: three tables marked
+-- 9/unmarked/6 all came out `Table6` -- reproduced even with bare
+-- `pandoc --lua-filter=`, so it is not about Quarto's pipeline. A table
+-- that should stay at the 10 pt default would be resized because some
+-- OTHER table needed it, and a plain user-authored table with nothing to
+-- do with apaTables could be silently restyled.
+--
+-- So this uses pandoc's `Blocks` filter type (a handler that runs once per
+-- BLOCKS LIST anywhere in the tree, not once per node) instead of `Div`/
+-- `Table`, because it is the one traversal pandoc offers whose UNIT is a
+-- sibling list rather than a single node -- exactly the scope a marker and
+-- "the table it belongs to" share. `apa_quarto_size_wrap()`
+-- (R/quarto-table.R) always appends the marker directly after the table's
+-- own grid markdown with nothing else between them, so table and marker
+-- are ALWAYS adjacent elements of the SAME immediate Blocks list, whatever
+-- divs surround that list (a FloatRefTarget's content, an unlabelled
+-- chunk's own cell div, nested arbitrarily deep) -- MEASURED (a
+-- three-table probe: one plain, one marked 9, one marked 6): a
+-- `Blocks` handler run via `doc.blocks:walk({ Blocks = fn })` visits the
+-- exact two-element [Table, marker Div] list for each marked table
+-- separately, and the ONE plain table's own singleton [Table] list on its
+-- own, with no crosstalk between them.
+local function process_blocks(blocks)
+  local out = pandoc.Blocks({})
+  local last_table_idx = nil
+  local changed = false
+  for _, el in ipairs(blocks) do
+    if el.t == "Table" then
+      out:insert(el)
+      last_table_idx = #out
+    elseif el.t == "Div" then
+      local size
+      for _, c in ipairs(el.classes) do
+        local n = c:match(sizeclass)
+        -- A size this filter does not recognise is IGNORED, not clamped or
+        -- guessed at: the marker div is left exactly as it is (an empty
+        -- div with an unrecognised class is inert in the rendered docx,
+        -- the same as before this filter existed at all), and the table
+        -- simply keeps its default "Table" style.
+        if n and valid_sizes[n] then size = n end
+      end
+      -- HTML's exact size, carried as an attribute because a class cannot
+      -- express a fraction. Independent of `size` above: the two markers are
+      -- never emitted together (R chooses one by destination), and reading
+      -- both here keeps this filter correct whichever one it is handed.
+      local exact = el.attributes[sizeattr]
+
+      -- The class apaTables puts on EVERY table it
+      -- sends to an HTML render, sized or not. It carries no size of its own;
+      -- it is what apa.css's table-width rules are scoped to, so that an
+      -- apaTables table gets them and a table the user built themselves does
+      -- not. A marker carrying only this class must still be recognised --
+      -- and still be CONSUMED, or it prints as an empty div.
+      local is_html_marker = false
+      for _, c in ipairs(el.classes) do
+        if c == htmlclass then is_html_marker = true end
+      end
+
+      if (size or exact or is_html_marker) and last_table_idx then
+        local tbl = out[last_table_idx]
+        if is_docx and size then
+          tbl.attr.attributes["custom-style"] = "Table" .. size
+        end
+        -- No `elseif is_html and exact` arm here: Quarto's HTML pipeline
+        -- discards a Table node's own attributes -- measured at both
+        -- post-quarto and pre-ast, for both a labelled float and an
+        -- unlabelled table, so it is neither a stage question nor a float
+        -- question. A `style` attribute assigned directly to the Table node
+        -- survives inside this filter's own AST but never reaches the
+        -- rendered page. Div-level attributes DO survive, so on HTML the
+        -- table is wrapped in a div instead of attributed directly. Do not
+        -- re-add this arm.
+        --
+        -- This is NOT a wrapper in the source markdown, which fails for two
+        -- independent reasons, both measured:
+        --   1. A wrapper in the SOURCE MARKDOWN, around the whole chunk, is
+        --      seen by apaquarto's float collector, which then stops
+        --      collecting the table -- moving it in the document. This
+        --      div is built here, at post-quarto, AFTER collection has
+        --      already run, and lives INSIDE the float the table was
+        --      already collected into. Measured: the sized table stays
+        --      collected at the end, alongside an unsized one.
+        --   2. A wrapper setting a plain inherited size loses to Bootstrap's
+        --      `.small` rule (specificity 0,1,0), which takes 0.875 of it --
+        --      asked 6 pt, delivered 5.25 pt. The `.apa-table-size > table`
+        --      rule in apa.css targets the table element directly at
+        --      specificity (0,1,1), beating `.small`, and `font-size:
+        --      inherit` takes the wrapper's exact size verbatim.
+        -- Do not revert this to a plain attribute assignment on `tbl`.
+        -- ONE div, whether or not a size was asked for.
+        -- The `apa-table-html` class is what apa.css's width rules are scoped
+        -- to; the `apa-table-size` class and the style attribute are added
+        -- beside it only when a size was asked for.
+        --
+        -- ONE div, NOT two nested. `.apa-table-size > table` is a CHILD
+        -- selector, so a second wrapper between it and the table would break
+        -- an explicit font.size SILENTLY -- the size would fall through to
+        -- Bootstrap's `.small`, which takes 0.875 of the inherited value
+        -- (asked 6 pt, delivered 5.25 pt). Both classes go on the same div
+        -- for that reason, and
+        -- tests/testthat/test-html-structure.R asserts it on a real render.
+        if is_html then
+          local classes = {htmlclass}
+          local attrs = {}
+          if exact then
+            table.insert(classes, "apa-table-size")
+            attrs.style = "font-size: " .. exact .. "pt;"
+          end
+          out[last_table_idx] = pandoc.Div({tbl}, pandoc.Attr("", classes, attrs))
+        else
+          out[last_table_idx] = tbl
+        end
+        -- This if/else is the ONE place `out[last_table_idx]` is written
+        -- for this marker: the `is_html` arm replaces it with the
+        -- wrapper div described above, and the `else` arm writes `tbl` back
+        -- unwrapped -- still carrying the docx custom-style set in the
+        -- is_docx branch above, when that branch fired -- for every other
+        -- case, docx included.
+        -- Consumed either way, including for a format neither branch serves
+        -- (e.g. a docx-only `size` marker walked in a non-docx render, or an
+        -- HTML-only `exact` marker walked in a docx render): the marker div
+        -- is dropped below regardless, so it never survives as dead weight
+        -- in an output this filter does not act on.
+        --
+        -- Consumed: this marker cannot also apply to a LATER table in the
+        -- same list (there is no such case in practice -- one table, one
+        -- optional marker, per apa_quarto_size_wrap() -- but not relying
+        -- on that from inside this filter, which is an independently
+        -- loadable, general-purpose artifact, is the same discipline the
+        -- valid_sizes re-check just above already applies).
+        last_table_idx = nil
+        changed = true
+        -- The marker div itself is dropped (not re-inserted into `out`),
+        -- leaving no trace in the rendered document.
+      else
+        -- No table has been seen yet in THIS list, or this div's class
+        -- does not name a recognised size: leave the div exactly as it is.
+        out:insert(el)
+      end
+    else
+      out:insert(el)
+    end
+  end
+  if changed then return out end
+  return nil
+end
+
+local function process_pandoc(doc)
+  doc.blocks = doc.blocks:walk({ Blocks = process_blocks })
+  return doc
+end
+
+return { { Pandoc = process_pandoc } }
diff --git a/_extensions/apaquarto/apatablespanner.lua b/_extensions/apaquarto/apatablespanner.lua
new file mode 100644
index 0000000..74814d5
--- /dev/null
+++ b/_extensions/apaquarto/apatablespanner.lua
@@ -0,0 +1,60 @@
+-- A column spanner is CENTRED over the columns it covers (APA).
+--
+-- THE DEFECT. In a two-way table the spanner `alcohol` sits over `None`,
+-- `2 Pints` and `4 Pints`, and those are numeric columns, so they are
+-- right-aligned. Measured in a real render, the spanner printed hard against
+-- the RIGHT edge of the columns it spans, in BOTH docx and html:
+--
+--   html   <th colspan="6" style="text-align: right;">alcohol</th>
+--   docx   <w:p><w:pPr><w:pStyle w:val="ApaTableRule"/><w:jc w:val="right"/>
+--
+-- ONE CAUSE, NOT TWO. Both are pandoc writing from the SAME table, and a
+-- pandoc grid table carries alignment PER COLUMN. A cell spanning several
+-- columns takes the alignment of the first column it covers, which for a
+-- spanner over numeric columns is right. Nothing in the markdown apaTables
+-- emits can say otherwise: grid-table syntax has no per-cell alignment.
+--
+-- WHY THIS RUNS ON THE AST AND NOT IN CSS. A stylesheet rule reaches html
+-- only, leaving Word wrong, and it would have to beat an inline `style=`
+-- attribute. Pandoc's AST *does* carry per-cell alignment, so setting it here
+-- -- before either writer runs -- fixes both destinations with one rule.
+--
+-- AND THE STYLE ROUTE IS RULED OUT BY MEASUREMENT. That docx paragraph
+-- already carries `<w:pStyle w:val="ApaTableRule"/>`, and the `<w:jc>` beside
+-- it is DIRECT formatting, which wins over any style. Putting a centred `jc`
+-- into the ApaTableRule style would change nothing.
+--
+-- THE PDF PATH NEEDS NOTHING. apaTables emits raw
+-- LaTeX there -- `\multicolumn{n}{c}{...}`, centred explicitly -- so no AST
+-- table is ever built and this filter has nothing to match.
+--
+-- HEAD ROWS ONLY, AND THAT IS LOAD-BEARING. A `col_span > 1` cell in the
+-- BODY is a row-group label (`apa_row_group()`), which is flush left and must
+-- stay flush left. Centring those would silently restyle every grouped table.
+if not (FORMAT == "docx" or FORMAT:match("^html")) then
+  return {}
+end
+
+local function centre_head_spanners(tbl)
+  local changed = false
+  -- tbl.head.rows: the header rows. A spanner may sit at any level (a two-way
+  -- table has two: the iv2 name over everything, then one per iv2 level).
+  if tbl.head and tbl.head.rows then
+    for _, row in ipairs(tbl.head.rows) do
+      for _, cell in ipairs(row.cells) do
+        -- `> 1` and not `~= 1`: col_span is never 0, but a guard that reads
+        -- as "spans more than one column" is the requirement itself.
+        if cell.col_span and cell.col_span > 1
+            and cell.alignment ~= pandoc.AlignCenter then
+          cell.alignment = pandoc.AlignCenter
+          changed = true
+        end
+      end
+    end
+  end
+  -- Returning nil leaves the element untouched, which keeps every table
+  -- without a spanner byte-identical rather than rebuilt.
+  if changed then return tbl end
+end
+
+return { { Table = centre_head_spanners } }
diff --git a/_extensions/apaquarto/apatemplate.tex b/_extensions/apaquarto/apatemplate.tex
index 8627834..75b4271 100644
--- a/_extensions/apaquarto/apatemplate.tex
+++ b/_extensions/apaquarto/apatemplate.tex
@@ -40,6 +40,20 @@ $endif$
 \RequirePackage{longtable}
 \RequirePackage{threeparttablex}
 
+% needspace supplies \Needspace*, which apafloatlatex.lua puts in front of a
+% longtable when floats stay in the text so that a short table moves to the
+% next page whole instead of straddling the break. Loaded unconditionally: it
+% defines one macro and adds no output, and loading it only in the modes that
+% use it would make the preamble depend on a mode test that the filter already
+% owns.
+%
+% THE STARRED, EAGER FORM IS DELIBERATE. The lower-case \needspace defers the
+% break decision to TeX's page builder, which by then is inside longtable's
+% output routine -- and that routine repeats the table's header on the new
+% page, above the caption, attached to nothing. The reasoning and the
+% 26-document measurement live in apafloatlatex.lua.
+\usepackage{needspace}
+
 \usepackage{framed}
 
 
@@ -148,9 +162,24 @@ $if(journalmode)$
 
 
 \makeatletter
-\let\endoldlt\endlongtable		
+% apaTables: a longtable that draws its OWN closing \bottomrule (apaTables'
+% native tables do, inside \endlastfoot, so their note prints under it) arms
+% this flag from inside the environment -- \noalign{\csname
+% apatablesownbottomrule\endcsname} as its last row -- and the automatic
+% \hline below stands down for that one table. Without the flag every such
+% table ended with TWO rules: this \hline plus the table's own \bottomrule
+% (rule bands measured 2.4 pt apart at 300 dpi).
+% The flag is cleared, globally, the first time it is consumed, so a
+% following plain markdown table -- whose ONLY bottom rule is this \hline,
+% measured: without it such a table has no closing rule at all -- is
+% unaffected. The arming macro is reached via \csname so the same emitted
+% table is a harmless no-op under a template without this patch (plain
+% format: pdf).
+\newif\ifapatablesownrule
+\def\apatablesownbottomrule{\global\apatablesownruletrue}
+\let\endoldlt\endlongtable
 \def\endlongtable{
-\hline
+\ifapatablesownrule\noalign{\global\apatablesownrulefalse}\else\hline\fi
 \endoldlt}
 \makeatother
 
@@ -165,9 +194,14 @@ $if(journalmode)$
 }% end code
 $else$
 \makeatletter
+% Same flag as the journal-mode branch above (only one branch is emitted per
+% document): a longtable carrying its own \bottomrule arms
+% \apatablesownbottomrule and the automatic \hline stands down for it.
+\newif\ifapatablesownrule
+\def\apatablesownbottomrule{\global\apatablesownruletrue}
 \let\endoldlt\endlongtable
 \def\endlongtable{
-\hline
+\ifapatablesownrule\noalign{\global\apatablesownrulefalse}\else\hline\fi
 \endoldlt
 }
 \makeatother
@@ -176,6 +210,36 @@ $else$
 $if(manuscriptmode)$
 \RequirePackage{longtable}
 \DeclareDelayedFloatFlavor{longtable}{table}
+% Rotation, when floats are collected at the end of the manuscript.
+%
+% APA asks for the PAGE to be turned for a wide table -- "use landscape
+% orientation on the page with the wide table ... it does not matter if the
+% page header also moves" -- which is what pdflscape does. endfloat, though,
+% defers only the float ENVIRONMENT and drops whatever surrounded it, so a
+% naked landscape wrapper is stripped and the table lands upright. Declaring
+% landscape as a delayed flavour of table makes endfloat defer the whole
+% landscape block as a unit, rotation included. Measured against a control
+% without this line: with it, 40 of 40 rows on landscape pages; without it,
+% 40 of 40 rows on PORTRAIT pages.
+%
+% pdflscape MUST be loaded FIRST. \DeclareDelayedFloatFlavor takes over the
+% \landscape macro immediately, so a \usepackage{pdflscape} arriving later --
+% which is what Quarto adds for a ::: {.landscape} div, in header-includes,
+% below this point -- would define \landscape afresh and quietly undo the
+% deferral. Loading it here means Quarto's later \usepackage is the no-op.
+\RequirePackage{pdflscape}
+\DeclareDelayedFloatFlavor{landscape}{table}
+$endif$
+$if(studentmode)$
+% Student mode collects floats at the end exactly as manuscript mode does
+% (apa7 loads endfloat for both, and utilsapa.delayedfloats says so), so it
+% needs the same two declarations. Only the manuscript branch existed before,
+% which left a tall or rotated table in a student paper going through the
+% path measured to lose rows.
+\RequirePackage{longtable}
+\DeclareDelayedFloatFlavor{longtable}{table}
+\RequirePackage{pdflscape}
+\DeclareDelayedFloatFlavor{landscape}{table}
 $endif$
 $endif$
 $endif$
@@ -207,18 +271,166 @@ $endfor$
 \usepackage{etoolbox}
 
 \makeatletter
+% \newif must be declared BEFORE the macros that test the flag.
+\newif\ifLTpatch@caption
 \patchcmd{\LT@caption}
   {\bgroup}
   {\bgroup\global\LTpatch@captiontrue}
   {}{}
-\patchcmd{\longtable}
-  {\par}
-  {\par\global\LTpatch@captionfalse}
-  {}{}
+% longtable (2020 and later) steps the \LTcaptype counter when a longtable
+% begins, whether or not a caption follows, so an uncaptioned longtable would
+% use up a table number; the decrement below gives that number back.
+%
+% Pandoc 3.8 and later (Quarto 1.9) emit a captioned table as a float whose
+% caption sits OUTSIDE the longtable, with \def\LTcaptype{none} inside so the
+% longtable steps nothing. Decrementing there cancels the float's number, and
+% every table in a manuscript-mode PDF prints as "Table 1". So decrement only
+% when the longtable was itself counting as a table (\LTcaptype is "table")
+% and no caption consumed the number.
+%
+% The flag is reset here rather than by patching \longtable: by the time this
+% template runs, \longtable has already been redefined (endfloat in man/stu
+% mode, the footnote wrapper under floatsintext), so a \patchcmd on it reports
+% not-found and never resets the flag; the first captioned longtable then
+% latched it true for the rest of the document.
 \apptocmd{\endlongtable}
-  {\ifLTpatch@caption\else\addtocounter{table}{-1}\fi}
+  {\ifLTpatch@caption\else
+     \edef\LTpatch@type{\LTcaptype}\def\LTpatch@table{table}%
+     \ifx\LTpatch@type\LTpatch@table\addtocounter{table}{-1}\fi
+   \fi
+   \global\LTpatch@captionfalse}
   {}{}
-\newif\ifLTpatch@caption
+\makeatother
+
+% Table rows are single-spaced, as in Word; a table kept out of a float so
+% that it can break across pages would otherwise take the manuscript's double
+% spacing. \linespread{1} with \selectfont keeps whatever type size the table
+% has set and drops only the stretch. The manuscript's stretch is saved
+% first: a table's note keeps it (apalongtablenote.lua opens every note with
+% \apatablesnotespacing), because a note is spaced like the text, as in Word.
+%
+% Whether the table starts inside a box is recorded at the same moment. A
+% table in a callout, a margin note, a minipage or a float cannot break
+% across pages, and longtable never places its last foot (bottom rule and
+% note) there: its end-of-table output routine does not run inside a box.
+% So a boxed table that draws its own closing rule (it armed the flag below)
+% has its last foot set by hand under its last row, with no break allowed
+% between them (a callout can break across pages); one that does not keeps
+% the automatic \hline, its only closing rule.
+\makeatletter
+\newif\ifapatablesinbox
+\newif\ifapatables@armed
+\newif\ifapatables@intable
+\let\apatablesdocstretch\baselinestretch
+\def\apatablesnotespacing{\linespread{\apatablesdocstretch}\selectfont}
+\def\apatables@begintable{%
+  \global\apatables@armedfalse\global\apatables@intabletrue
+  \ifinner\global\apatablesinboxtrue\else\global\apatablesinboxfalse\fi
+  \let\apatablesdocstretch\baselinestretch\linespread{1}\selectfont}
+\AtBeginEnvironment{longtable}{\apatables@begintable}
+\AtBeginEnvironment{longtable*}{\apatables@begintable}
+\def\apatablesownbottomrule{%
+  \global\apatablesownruletrue\global\apatables@armedtrue}
+% The last foot is added inside the table's own alignment, as its final
+% item, when the table ends (before longtable's closing code, which is in a
+% syntax that cannot be patched and which drops the last foot with its group).
+\def\apatables@boxlastfoot{%
+  \ifapatablesinbox\ifapatables@armed\ifvoid\LT@lastfoot\else
+    \noalign{\nobreak\copy\LT@lastfoot}%
+  \fi\fi\fi}
+\pretocmd{\endlongtable}{\apatables@boxlastfoot}{}
+  {\typeout{apaquarto: could not patch the end of a table (boxed last foot)}}
+
+% One single-spaced line of body text: the unit in which apafloatlatex.lua
+% sizes the space it reserves in front of a table placed in the text. (Do not
+% write that reservation command or any table environment name in a comment
+% here: the comment reaches the rendered .tex, and tests count those strings.)
+\newlength{\apatablesline}
+\setlength{\apatablesline}{\f@baselineskip}
+
+% A markdown table's note. The writer writes a markdown table, so its note
+% cannot be spliced into the table's last foot the way a raw table's is;
+% apalongtablenote.lua stores it in \apatablespendingnote from inside the
+% table's first header cell, and here the last foot prints it as a full-width
+% row under the bottom rule and forgets it. Header rows are typeset before the
+% last foot is read, and under endfloat the whole table travels together, so
+% the note reaches only its own table (in a box, the last foot is set by
+% hand, as above).
+%
+% The row ends with a zero kern so that the last foot has no depth: the note
+% is a top-aligned box, so every line after its first would otherwise count
+% as DEPTH, which neither longtable's own end-of-table check nor the running
+% foot below measures -- and a table whose rows filled the page then broke
+% before its last row, which reached the next page without a column header.
+% apalongtablenote.lua defines each note, numbered, just before its table
+% (\apatablesnote@<n>) and puts \apatablesnoteid{<n>} in the table's first
+% header cell or caption. It does nothing outside a table, so the copy of a
+% caption in a list of tables or a cross-reference title is inert.
+\DeclareRobustCommand\apatablesnoteid[1]{%
+  \ifapatables@intable\ifcsname apatablesnote@#1\endcsname
+    \expandafter\global\expandafter\let\expandafter\apatablespendingnote
+      \csname apatablesnote@#1\endcsname
+  \fi\fi}
+\apptocmd{\endlongtable}{\global\apatables@intablefalse}{}
+  {\typeout{apaquarto: could not patch the end of a table (note marker)}}
+\let\apatables@endlastfoot\endlastfoot
+\def\endlastfoot{%
+  \ifdefined\apatablespendingnote\expandafter\apatables@pendingnoterow\fi
+  \apatables@endlastfoot}
+\def\apatables@pendingnoterow{%
+  \noalign{\vskip\belowrulesep}%
+  \multicolumn{\LT@cols}{@{}l@{}}{\makebox[0pt][l]{\parbox[t]{\linewidth}{%
+    \apatablesnotespacing\apatablespendingnote}}}\\
+  \noalign{\kern\z@\global\let\apatablespendingnote\@undefined}}
+
+% Room for the last foot on every page. At the end of a table whose last
+% foot (bottom rule and note) is taller than its running foot, longtable
+% ships the final rows and sets the last foot on a page of its own, under a
+% repeated header with no rows -- the note orphaned. So the running foot is
+% made as tall as the last foot whenever it is shorter: empty for a table
+% that declares none, padded for one that does (apaTables' own 48 pt spacer
+% is shorter than a two- or three-line double-spaced note). Set when the
+% table's body starts, after every head and foot is known. The cost is the
+% same blank space at the foot of each continuation page, so the padding is
+% capped at a quarter of the page: a table with a very long note otherwise
+% spread over many near-empty pages (measured on an apaTables table of 45
+% rows with a 2,000-character note: 3 pages became 8). Only a note taller
+% than that can still be set apart from its rows.
+\newbox\apatables@footbox
+\def\apatables@footroom{%
+  \ifvoid\LT@lastfoot\else
+    \dimen@\ht\LT@lastfoot
+    \ifdim\dimen@>.25\textheight\dimen@.25\textheight\fi
+    \ifdim\ht\LT@foot<\dimen@
+      \setbox\apatables@footbox\box\LT@foot
+      \setbox\LT@foot\vbox to\dimen@{%
+        \ifvoid\apatables@footbox\else\unvbox\apatables@footbox\fi\vss}%
+    \fi
+  \fi}
+\pretocmd{\LT@start}{\apatables@footroom}{}
+  {\typeout{apaquarto: could not patch the start of a table (footroom)}}
+
+% The space reserved in front of a table placed in the text. A table no
+% taller than the page is moved to the next page whole when it does not fit.
+% The estimate errs a little high, so one estimated at up to two lines over
+% the page may still fit on a fresh page: it asks for a page less a line,
+% which moves it to the top of the next one (measured with two-line notes:
+% 37 rows, estimated 46 lines, fit a fresh page; 38 rows, 47 lines, did not). A table taller than
+% that cannot fit anywhere, so it asks only for room to start (its caption,
+% header and a few rows) and breaks across pages where it stands, rather
+% than leaving most of a page empty and breaking anyway. And the page it leaves is ended like \newpage, with its white
+% space at the foot: the starred form ends it with a bare \break, and under
+% the manuscript's \flushbottom TeX then spread that page's text down to the
+% foot, with gaps between the paragraphs.
+\patchcmd{\@sneedsp@}{\advance\dimen@\@tempskipb}
+  {\dimen@ii\textheight\advance\dimen@ii2\apatablesline
+   \ifdim\dimen@>\dimen@ii\dimen@8\apatablesline
+   \else\ifdim\dimen@>\textheight
+     \dimen@\textheight\advance\dimen@-\apatablesline\fi\fi
+   \advance\dimen@\@tempskipb}{}
+  {\typeout{apaquarto: could not patch the reservation (page cap)}}
+\patchcmd{\@sneedsp@}{\break}{\ifdim\dimen@ii>\z@\vfil\fi\break}{}
+  {\typeout{apaquarto: could not patch the reservation (ragged page)}}
 \makeatother
 
 \begin{document}
diff --git a/_extensions/apaquarto/doc-class.tex b/_extensions/apaquarto/doc-class.tex
index 9c639b7..f680c7a 100644
--- a/_extensions/apaquarto/doc-class.tex
+++ b/_extensions/apaquarto/doc-class.tex
@@ -8,6 +8,10 @@ $endif$
   longtable,
 $if(a4paper)$
   a4paper,
+$else$
+$if(papersize)$
+  $papersize$paper,
+$endif$
 $endif$
   nolmodern,
   notxfonts,
diff --git a/_extensions/apaquarto/utilsapa.lua b/_extensions/apaquarto/utilsapa.lua
index 7650bf9..8f9cea8 100644
--- a/_extensions/apaquarto/utilsapa.lua
+++ b/_extensions/apaquarto/utilsapa.lua
@@ -42,6 +42,29 @@ function M.stringify(s, default)
   return s
 end
 
+-- Are floats collected at the end of the manuscript, or do they stay where
+-- they were written?
+--
+-- apa7 loads endfloat in man and stu modes, which moves every float to its own
+-- page at the end; `floatsintext: true` turns that off, and doc and jou modes
+-- never had it. Two filters need this answer -- apalandscapelatex.lua, to know
+-- whether Quarto's native landscape div will survive, and apafloatlatex.lua,
+-- to know whether the reservation in front of a longtable would reach the page or
+-- be stripped -- so the rule lives here ONCE. Two copies of it could drift and
+-- then disagree about which placement is active, which is a defect no rendered
+-- page would announce.
+function M.delayedfloats(meta)
+  local documentmode = "man"
+  if meta.documentmode then
+    documentmode = pandoc.utils.stringify(meta.documentmode)
+  end
+  local delayed = documentmode == "man" or documentmode == "stu"
+  if meta.floatsintext and pandoc.utils.stringify(meta.floatsintext) == "true" then
+    delayed = false
+  end
+  return delayed
+end
+
 -- if any value in table
 function M.containsValue(tbl, value)
   for _, v in pairs(tbl) do
diff --git a/tests/landscape-floats.qmd b/tests/landscape-floats.qmd
new file mode 100644
index 0000000..5cb556c
--- /dev/null
+++ b/tests/landscape-floats.qmd
@@ -0,0 +1,52 @@
+---
+title: "Landscape Floats Reproducer"
+shorttitle: "Landscape Floats"
+author:
+  - name: Test Author
+    corresponding: true
+    email: test@example.com
+    affiliations:
+      - name: Test University
+keep-tex: true
+format:
+  apaquarto-pdf:
+    documentmode: man
+---
+
+This document tests landscape (rotated) floats in manuscript-mode PDF
+output, where apa7's endfloat machinery moves each float to its own page at
+the end of the manuscript.
+
+Expected output: both tables appear at the end of the manuscript on rotated
+(landscape) pages, all twelve columns visible -- APA turns the page, not the
+table, and it is explicit that the running head moving with it does not
+matter. The body text below stays together with no stray blank pages where
+the tables were declared.
+
+A table wrapped in Quarto's native landscape div gets rotated:
+
+::: {.landscape}
+
+| C01 | C02 | C03 | C04 | C05 | C06 | C07 | C08 | C09 | C10 | C11 | C12 |
+|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|
+| 0.11 | 0.22 | 0.33 | 0.44 | 0.55 | 0.66 | 0.77 | 0.88 | 0.99 | 1.10 | 1.21 | 1.32 |
+| 0.21 | 0.32 | 0.43 | 0.54 | 0.65 | 0.76 | 0.87 | 0.98 | 1.09 | 1.20 | 1.31 | 1.42 |
+| 0.31 | 0.42 | 0.53 | 0.64 | 0.75 | 0.86 | 0.97 | 1.08 | 1.19 | 1.30 | 1.41 | 1.52 |
+
+: A wide table inside a landscape div. {#tbl-divlandscape apa-note="This note should appear below the rotated table."}
+
+:::
+
+A table can also request rotation directly with the apa-landscape
+attribute, without a surrounding div:
+
+| C01 | C02 | C03 | C04 | C05 | C06 | C07 | C08 | C09 | C10 | C11 | C12 |
+|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|-----|
+| 0.12 | 0.23 | 0.34 | 0.45 | 0.56 | 0.67 | 0.78 | 0.89 | 0.90 | 1.11 | 1.22 | 1.33 |
+| 0.22 | 0.33 | 0.44 | 0.55 | 0.66 | 0.77 | 0.88 | 0.99 | 1.00 | 1.21 | 1.32 | 1.43 |
+
+: A wide table using the apa-landscape attribute. {#tbl-attrlandscape apa-note="This note should also appear below the rotated table." apa-landscape="true"}
+
+As you can see in @tbl-divlandscape and @tbl-attrlandscape, wide tables are
+rotated. This closing paragraph should sit directly under the preceding
+text with no page break inserted where the tables were declared.
diff --git a/tests/longtable-counter.qmd b/tests/longtable-counter.qmd
new file mode 100644
index 0000000..6986476
--- /dev/null
+++ b/tests/longtable-counter.qmd
@@ -0,0 +1,58 @@
+---
+title: "Table Numbering Reproducer"
+shorttitle: "Table Numbering"
+author:
+  - name: Test Author
+    corresponding: true
+    email: test@example.com
+    affiliations:
+      - name: Test University
+format:
+  apaquarto-pdf:
+    documentmode: man
+---
+
+This document checks that tables are numbered 1, 2, 3 in manuscript-mode PDF.
+The text should read "Table 1", "Table 2" and "Table 3" here: @tbl-one,
+@tbl-two and @tbl-three. A raw captioned longtable takes a number of its own
+(Table 4); a raw uncaptioned longtable takes none, so the final markdown table
+is Table 5 (@tbl-five).
+
+| A | B |
+|---|---|
+| 1 | 2 |
+
+: First table {#tbl-one}
+
+| A | B |
+|---|---|
+| 3 | 4 |
+
+: Second table {#tbl-two}
+
+| A | B |
+|---|---|
+| 5 | 6 |
+
+: Third table {#tbl-three}
+
+```{=latex}
+\begin{longtable}{ll}
+\caption{Raw captioned longtable}\\
+\hline
+a & b \\
+\hline
+\end{longtable}
+
+\begin{longtable}{ll}
+\hline
+c & d \\
+\hline
+\end{longtable}
+```
+
+| A | B |
+|---|---|
+| 7 | 8 |
+
+: Fifth table {#tbl-five}
```
