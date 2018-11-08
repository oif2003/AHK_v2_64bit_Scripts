#singleinstance force
/*	ControlGetFont(hwnd:=0)
	if no hwnd is given, gets GUI defaults
*/

;Example:
gui := guicreate()
gui.SetFont(,"Arial")
eb := gui.add("edit")
eb.SetFont("q5 bold italic underline strikeout s20", "Consolas")

pn("Font info for the editbox we added:")
po(ControlGetFont(eb.hwnd))
pn()
pn("Default font info for GUI:")
po(ControlGetFont())

;---------------------------------------------------------------------------------------------------------
; ControlGetFont(hwnd := 0)
;---------------------------------------------------------------------------------------------------------
; by SKAN (modified by just me), modified by oif2003
; https://autohotkey.com/boards/viewtopic.php?t=6750
; If no parameters are give, returns default font for GUI
; for interpretation of the returned values, see:
; https://docs.microsoft.com/en-us/windows/desktop/api/wingdi/ns-wingdi-taglogfonta
ControlGetFont(hwnd := 0) {
	if !hwnd {
		hFont := DllCall("GetStockObject", "Int", 17, "Ptr")
	}
	else {
		hFont := SendMessage(0x31, 0, 0, , "ahk_id" hwnd) ; WM_GETFONT       
		If ErrorLevel
			Return
	}

	VarSetCapacity(LF, szLF := 28 + (A_IsUnicode ? 64 : 32), 0) ; LOGFONT structure
	If DllCall("GetObject", "UInt", hFont, "Int", szLF, "Ptr", &LF) {
		font := {}
		font.Height := NumGet(LF, 0, "Int")
		font.Width := NumGet(LF, 4, "Int")
		font.Escapement := NumGet(LF, 8, "Int")
		font.Orientation := NumGet(LF, 12, "Int")
		font.Weight := NumGet(LF, 16, "Int")
		font.Italic := NumGet(LF, 20, "Char")
		font.Underline := NumGet(LF, 21, "Char")
		font.StrikeOut := NumGet(LF, 22, "Char")
		font.CharSet := NumGet(LF, 23, "Char")
		font.OutPrecision := NumGet(LF, 24, "Char")
		font.ClipPrecision := NumGet(LF, 25, "Char")
		font.Quality := NumGet(LF, 26, "Char")
		font.PitchAndFamily := NumGet(LF, 27, "Char")
		font.FaceName := StrGet(&LF + 28, 32)
		font.Size := Round(Abs(font.Height) * (72 / A_ScreenDPI), 1)
		return font
	}
   Return False
}
;---------------------------------------------------------------------------------------------------------
; End of ControlGetFont(hwnd := 0)
;---------------------------------------------------------------------------------------------------------


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
		
	Example:
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
		
		pa(a[1][1])
		pa(a[1])
		po(a)		;equivalent to pa(a) in this case since Dim(a) > 2
*/

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
;============================ End of print script =============================



