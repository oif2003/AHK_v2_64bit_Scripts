--invert table for quick lookup
function table_invert(t)
   local s={}
   for k,v in pairs(t) do
     s[v]=k
   end
   return s
end

selstartpos = scite.SendEditor(SCI_GETSELECTIONSTART)
selstartline = scite.SendEditor(SCI_LINEFROMPOSITION, selstartpos)
selendline = scite.SendEditor(SCI_LINEFROMPOSITION, scite.SendEditor(SCI_GETSELECTIONEND))

if selstartline  == selendline then
	text = editor:GetCurLine()
	text = string.gsub(text, "^%s+", "")	--removes leading blank
	headertext = "# Running Line: " .. selstartline +1 ..  "\n"
else
	text = editor:GetSelText()
	
	lines = {}
	i = 1
	for line = selstartline, selendline do
		linestart = scite.SendEditor(SCI_GETLINESELSTARTPOSITION, line) 
		lineend = scite.SendEditor(SCI_GETLINESELENDPOSITION, line)
		strtext = string.sub(text, linestart-selstartpos + 1, lineend - selstartpos)
		if string.match(strtext, "[^%s]")~=nil then
			lines[i] = strtext
			i = i + 1
		end
	end
	
	
	
	--remove extra leading tabs
	spacecount = {}
	spaces = {}
	for i = 1, #lines do
		spaces[i] = string.match(lines[i], "%s*%w")
		if spaces[i]~=Null then
			spacecount[i] = string.len(spaces[i]) - 1
		else
			spacecount[i] = 0
		end
	end

	tablevels = {}
	for k, v in pairs(spacecount) do
		tablevels[k] = v
	end

	table.sort(tablevels)
	j=#tablevels
	while j>1 do
		if tablevels[j]==tablevels[j-1] then
			table.remove(tablevels, j)
		end
		j = j - 1
	end
	
	inv_tablevels = table_invert(tablevels)
	
	for i = 1, #lines do
		if spacecount[i]>0 then
			spacecount[i] = inv_tablevels[spacecount[i]] - 1
			lines[i] = string.gsub(lines[i], string.sub(spaces[i],1,-2), string.rep("\t", spacecount[i]), 1)
		end
	end
	
	--cancel out tabs with backspaces
	for i=1, #lines -1 do
		tabstr = string.match(lines[i], "^\t+")
		lines[i] = lines[i] .. "\n"
		if tabstr ~= nil then
			tabcount = string.len(tabstr)
			lines[i] = lines[i] .. string.rep("\b", tabcount)
			--replace leading each tab with 4 spaces since accidently hitting tab gives you wall of text
			lines[i] = string.gsub(lines[i], "^\t*", string.rep(" ", tabcount*4)) 
		end
	end
	
	text=table.concat(lines)
	headertext = "# Running Lines: " .. selstartline +1 .. " to " .. selendline +1 .. "\n"
end

text = headertext .. text
editor:CopyText(text) 
--print(selendline-selstartline+1 .. " line(s) of Julia code sent to AutoHotkey.")
