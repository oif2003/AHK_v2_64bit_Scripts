;==========================================================================
;debug output script
p(x)=>fileappend(x, "*") 	; <=== I use this with SciTE4AutoHotkey
;print to gui and scroll to the bottom of edit control
;~ p(x:="") {
	;~ global gui, eb
	;~ if !isObject(gui) {
		;~ gui := GuiCreate()
		;~ eb := gui.Add("Edit", "r50 w600 +readonly -wrap")
		;~ gui.show()
	;~ }
	;~ eb.value .= x, PostMessage( 0x115, 7, , "Edit1", "ahk_id " gui.hwnd) 
;~ }

;print line
pn(x:="")=>p(x "`n")

;print array
pa(a, sep := ",") {
	for k, v in a
		r .= (k == 1 ? "" : sep) . v
	pn("[" . r . "]")
}

;print array of array
paa(aa, sep := ",") {
	for k, v in aa
		p("[" . k . "] "), pa(v, sep)
}
;End of debug output script
;==========================================================================
