--return array index of needle inside haystack array
function inarr(arr, needle)
	for k, v in pairs(arr) do
		if v == needle then
			return k
		end
	end
end
	 
-- Script starts here
text = editor:GetSelText()

--get start/end position of selection
selstartpos = scite.SendEditor(SCI_GETSELECTIONSTART)
selstartline = scite.SendEditor(SCI_LINEFROMPOSITION, selstartpos)
selendline = scite.SendEditor(SCI_LINEFROMPOSITION, scite.SendEditor(SCI_GETSELECTIONEND))

--parse selection into lines array
lines = {}
print(0, "ORIGINAL SELECTED TEXT")
for line = selstartline, selendline do
	linestart = scite.SendEditor(SCI_GETLINESELSTARTPOSITION, line) 
	lineend = scite.SendEditor(SCI_GETLINESELENDPOSITION, line)
	lines[line-selstartline+1] = string.sub(text, linestart-selstartpos + 1, lineend - selstartpos)
	print(line+1, lines[line-selstartline+1])
end

--count spaces(include tabs) leading each line
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

print("Indentation level maping:  orginal level -> min distance level")
for k, v in pairs(tablevels) do
	print(v .. " -> " .. k-1)
end

for i = 1, #lines do
	if spacecount[i]>0 then
		spacecount[i]=inarr(tablevels, spacecount[i]) - 1
		lines[i] = string.gsub(lines[i], string.sub(spaces[i],1,-2), string.rep("\t", spacecount[i]), 1)
	end
end

text=table.concat(lines, "\n")

editor:ReplaceSel(text)
print("Indentation adjustments complete.")

