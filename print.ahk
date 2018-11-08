;=========================== Start of print script ============================
/*	for AutoHotkey v2 by oif2003
	Usage:
		p(msg) 	  => prints message without appending newline (`n)
		pn(msg)   => prints message and appends newline (`n)
		pa(arr)   => prints 1D or 2D array; if dim > 2, calls po(arr)
						as 	[Item1,Item2,Item3 ....]
						or 	[1] [Item1,Item2,Item3 ....]
							[2] [Item1,Item2,Item3 ....]
								(...so on and so forth...)
		po(obj)	  => prints object as [key] = value recursively
			
*/
;Example:
a := []
loop 2 {
	i := A_Index, a[i] := []
	loop 2 {
		j := A_Index, a[i][j] := []
		loop 2 {
			k := A_Index
			a[i][j][k] := i * j * k
		}
	}
}

p("Example"), p(" ")
p("o"), p("u"), p("t"), p("p"), p("u"), p("t"), p(":"), p("`n"), p("`n")
pn("Print 1D Array:")
pa(a[1][1])
pn()
pn("Print 2D Array:")
pa(a[1])
pn()
pn("Print as Object:")
po(a)		;equivalent to pa(a) in this case since Dim(a) > 2


;---------------------------------------------------------------------------------------------------------
; p(x), pa(x), po(x)
;---------------------------------------------------------------------------------------------------------
;Use SciTE4AutoHotkey output pane if possible
p(x) => WinExist("SciTE4AutoHotkey") ? fileappend(x, "*") : _p(x)

;GUI EditBox print
_p(x) {
	static _gui, _eb
	if !isObject(_gui)
		_gui := GuiCreate(), _eb := _gui.Add("Edit", "r50 w600 +readonly -wrap"), _gui.show()
	_eb.value .= x, PostMessage(0x115, 7, , "Edit1", "ahk_id " _gui.hwnd) ;scroll to end
}

;Print line
pn(x:="") => p(x "`n")

;helper function for printing 1D array
_pa(a) {
	for k, v in a
		r .= (k == 1 ? "" : ",") v
	pn("[" r "]")
}

;Print 1D or 2D array, if Dim > 2, print as object
pa(a) {
	for _, v in a
		if isObject(v) {
			dim := 2
			for _, s in v
				if isObject(s)
					dim := 3
		}	
	if dim == 3
		po(a)		
	else if dim == 2
		for k, v in a
			p("[" k "] "), _pa(v)
	else
		_pa(a)
}

;Print object recursively
po(o, tablevel := 0) {
	static more := true
	loop tablevel
		tab .= "`t"
	if !isObject(o)
		pn(" = " o), more := false
	else {
		more := true, tablevel && pn()
		for k, v in o
			p(tab "[" k "]"), po(v, tablevel + 1), more && pn()
	}
}