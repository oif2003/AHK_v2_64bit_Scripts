#SingleInstance Force

arr1 := [	  
			  [11, 12, 13, 14, 15]
			, [21, 22, 23, 24, 25]
			, [31, 32, 33, 34, 35]
			, [41, 42, 43, 44, 45]
		]

arr2 := [	  
			  [11, 12]
			, [21, 22]
			, [31, 32]
		]

arr3 := [	  
			  [[111,112], [221,222]]
			, [[211,212], [221,222]]
			, [[311,312], [321,322]]
		]

obj := 	{	  mon:{breakfast:6.5, lunch:11.5, dinner:18}
			, tue:{breakfast:6, lunch:11, dinner:18.5}
			, wed:{breakfast:7, lunch:12.5, dinner:19}
			, thu:{breakfast:6.5, lunch:12, dinner:17.5}
			, fri:{breakfast:6, lunch:11, dinner:18}
			, sat:{breakfast:8, lunch:13.5, dinner:20}
			, sun:{breakfast:9, lunch:13, dinner:19}
		}

pn("Content of arr1:")
p(arr1), pn()

pn("Multiply each element by 10")
test2 := db(arr1, (x) => x * 10)		;when depth is left blank defaults to max
p(test2), pn("`n-----------------------------------------------------------------------------------------------------------------")

pn("Content of arr2:")
pn("(each row corresponds to possible values for x, y, z respectively)")
p(arr2), pn()

pn("Calculate distance from the origin on (x, y, z) sets defined by arr2")
list := ls(arr2)					;generate 1D array with list of points/coordinates
test3 := db(list, (x)=>dist(x*), 1)	;broadcast distance function to list
p(test3, 1), pn("`n-----------------------------------------------------------------------------------------------------------------")

pn("Content of arr3:")
p(arr3), pn()

pn("Variadic function sum test: 2 levels deep")
test4 := db(arr3, (x) => sum(x*), 2)	;goes down two levels
p(test4), pn()

pn("Deep copy of arr3")
test5 := db(arr3)	;when funtion is not provided, returns deep copy of array
p(test5), pn("`n-----------------------------------------------------------------------------------------------------------------")

pn("Content of obj:")
p(obj, 1), pn()

pn("Convert hours to HH:MM format in obj")
test6 := db(obj, (x) => formatHours(x))	;iterate over all elements
p(test6, 1)

;functions used in the above examples
dist(x*) {
	sum := 0
	for _, v in x
		sum += v*v
	return format("{:.2f}", sqrt(sum))
}
sum(x*) {
	s := 0
	for _, v in x
		s += v
	return s
}
formatHours(h) {
	m := (h - floor(h)) * 60
	return format("{1:02d}:{2:02d}", floor(h), m)
}

;------------------------------------------------------------
;deep broadcast for n levels
; a = array/object
; f = function (default is none, which performs a deep copy)
; n = depth (default is max)
; last parameter is for internal use only
db(a, f := "", n := 0x7FFFFFFF, run1 := true) {
	static _n
	if run1
		_n := 1
	
	if _n <= n && isObject(a) {
		_n++, b := {}		
		for k, v in a
			b[k] := db(v, f, n, false)
		return (_n--, b)
	}
	else
		if f != ""
			return f.call(a)
		else return a
}

;------------------------------------------------------------
;list all elements
; a = array/object
; second parameter is for internal use only
ls(a, run1 := true) {		
	static r, t 		
	if run1	
		r := [], t := [] 
	
	b := a.Clone()
	if b.length()		
		for k, v in b.pop()	
			t.InsertAt(1, v), ls(b, false) 	
	else						
		if !r.length()			
			r.push(t), t := []			
		else {					
			_t := r[r.push()].Clone()		
			loop t.length()		
				_t[A_Index] := t[A_Index]	
			r.push(_t), t := []			
		}						
	return r				
}

;----------------------------------------------------------------------
;Just a print functions, nothing interesting here
pn(x:="") => p(x "`n")
p(x, opt := 0) {
	isObject(x) ? (opt ? po(x) 
					   : pa(x))
			    : _p(x) 
	
	;GUI EditBox print
	_p(x) {
		static gui, eb1, eb2, tb

		if !isObject(gui)
			  gui := GuiCreate("+resize", A_ScriptFullPath)
			, tab := gui.Add("Tab3", , "Default|Overflow")
			, tab.UseTab("Default")
			, eb1 := gui.Add("Edit", "r50 w800 +readonly -E0x200")
			, tab.UseTab("Overflow")
			, eb2 := gui.Add("Edit", "r50 w800 +readonly -E0x200")
			, Tab.UseTab()
			, aot := gui.Add("CheckBox", , "Always On Top")
			, gui.onEvent("Size",  ()=>onResize())
			, aot.onEvent("Click", ()=>WinSetAlwaysOnTop(aot.value, "ahk_id" gui.hwnd))
			, gui.show()
			, onResize()
		
		if StrLen(tb) > 1000
			eb1.value .= tb, PostMessage(0x115, 7, , eb1.hwnd, "ahk_id" gui.hwnd), tb := ""
		
		tb .= x, prevtb := tb, SetTimer(()=>update(), -50)
		update() {
			if tb == prevtb {
				if StrLen(eb1.value) > 100000
					  eb2.value .= eb1.value
					, eb1.value := ""
				  eb1.value .= tb, PostMessage(0x115, 7, , eb1.hwnd, "ahk_id" gui.hwnd)
				, tb := ""
			}
		}
		
		onResize() {
			  tab.move("x2 y2 w" (gui.ClientPos.w - 3) " h" (gui.ClientPos.h - 4))
			, eb1.move("x0 y0 w" (gui.ClientPos.w - 8) " h" (gui.ClientPos.h - 24))
			, eb2.move("x0 y0 w" (gui.ClientPos.w - 8) " h" (gui.ClientPos.h - 24))
			, aot.move("x" (gui.ClientPos.w - 100) " y" gui.MarginY - 1)
		}
	}
	
	;Print 1D or 2D array, if Dim > 2, print as object
	pa(a) {
		for _, v in a
			if isObject(v) && dim < 2 && dim := 2
				for _, s in v
					isObject(s) && dim < 3 && dim := 3
		if dim == 3
			po(a)		
		else if dim == 2
			for k, v in a
				p("[" k "] "), _pa(v)
		else
			_pa(a)
		
		;helper function for printing 1D array
		_pa(a) {
			for k, v in a
				r .= (k == 1 ? "" : ",") v
			pn("[" r "]")
		}
	}

	;Print object recursively
	po(o, tablevel := 0) {
		static more := true
		
		loop tablevel
			tab .= "`t"
		if isObject(o) {
			  more := true
			, tablevel && pn()
			for k, v in o
				  p(tab "[" k "]")
				, po(v, tablevel + 1)
				, more && pn()
		}
		else 
			  pn(" = " o)
			, more := false
	}
}
