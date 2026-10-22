if FORMAT == "latex" then
  return
end

-- This filter finds and assigns the table identifier in plain
-- markdown tables so that the crossrefprefix.lua filter can find it.
--
-- Tables inside a table panel (a div labelled tbl-..., holding subtables)
-- keep the earlier reading, which stops at the first hyphen, underscore,
-- dot or colon after "tbl-". It gives subtables labelled {#tbl-pair-a},
-- {#tbl-pair-b} the panel's own identifier, the only arrangement in which
-- a panel of markdown subtables is numbered correctly here; reading their
-- whole labels numbers each as a separate table, and leaving them without
-- identifiers stops the render in apafigtblappendix.lua.

local function set_panel_identifier(tb)
  if tb.caption.long then
    tb.caption.long:walk {
      Str = function(s)
        if s.text:find("%{%#tbl%-%w+") then
          tb.identifier = s.text:match("tbl%-%w+")
        end
      end
    }
    return tb
  end
end

local function set_identifier(tb)
  if tb.caption.long then
    tb.caption.long:walk {
      Str = function(s)
        -- The whole label, up to the closing brace or a space before the
        -- next attribute: %w+ stopped at the first hyphen, underscore, dot
        -- or colon after "tbl-", so {#tbl-md-plain} and {#tbl-md-note}
        -- both became "tbl-md" and the two tables were numbered alike.
        local id = s.text:match("%{%#(tbl%-[^%s%}]+)")
        if id then
          tb.identifier = id
        end
      end
    }
    return tb
  end
end

return {
  {
    traverse = "topdown",
    Div = function(div)
      if div.identifier:find("^tbl%-") then
        return div:walk { Table = set_panel_identifier }, false
      end
    end,
    Table = set_identifier
  }
}
