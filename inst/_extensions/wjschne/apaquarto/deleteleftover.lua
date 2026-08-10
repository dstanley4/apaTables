local directory = "./" -- Change to your directory path

local is_windows = package.config:sub(1,1) == "\\"

if is_windows then
  local pfile = io.popen('dir /B ' .. directory .. ' | grep -v /') 
else
  local pfile = io.popen('ls -p ' .. directory .. ' | grep -v /') 
end

for filename in pfile:lines() do
    -- Check if file ends with ".ttt" using Lua pattern matching
    -- %f matches the escape character, $ anchors to the end
    if filename:match("%.sdfd$") then
        local filepath = directory .. filename
        local success, err = os.remove(filepath)
        
        if success then
            print("Deleted: " .. filename)
        else
            print("Could not delete " .. filename .. ": " .. err)
        end
    end
end
pfile:close()