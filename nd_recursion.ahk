#singleinstance force
/*  Given the axises (as arrays) of an n dimensional array (as an array),
	the function g(array) returns all possible points in coordinate form 
	as an array of lists.  For example:
	Given s := [ ["one", "two"], [1, 2, 3] ]
	g(s) returns:
		[1] [one,1]
		[2] [two,1]
		[3] [one,2]
		[4] [two,2]
		[5] [one,3]
		[6] [two,3]
	This is equivalent to nesting loops, but is better when the required
	number of layers of loops are unknown.
*/

;print
;p(x)=>fileappend(x, "*") 	; <=== I use this with SciTE4AutoHotkey
;print to gui and scroll to the bottom of edit control
p(x:="") {
	global gui, eb
	if !isObject(gui) {
		gui := GuiCreate()
		eb := gui.Add("Edit", "r50 w600 +readonly -wrap")
		gui.show()
	}
	eb.value .= x, PostMessage( 0x115, 7, , "Edit1", "ahk_id " gui.hwnd) 
}

;print line
pn(x:="")=>p(x "`n")

;print array
pa(a) {
	for k, v in a
		r .= (k == 1 ? "" : ",") . v
	pn("[" . r . "]")
}

;print array of array
paa(aa) {
	for k, v in aa
		p("[" . k . "] "), pa(v)
}

testobj := [ [[1,2], [3,4]],  [[5,6], [7,8]],  [[9,10], [11,12]] ]
pn("A test using array of arrays of arrays: [ [[1,2], [3,4]],  [[5,6], [7,8]],  [[9,10], [11,12]] ]")
s := g(testobj)
p("s[1][1] = "), pa(s[1][1])
pn()
pn()

wordList := [	  
				  [1, 2, 3, 4]
				, ["cat", "dog", "fish"]
				, ["desk", "chair"]
				, ["one", "two", "three", "four", "five"]
			]
r := g(wordList)
pn("A test using array of arrays:")
paa(wordList)
pn()
pn("Results:")
paa(r)


/* ==================================
	Function Starts Here
*/ ==================================
g(a, firstRun := true) {		;call this function without using the firstRun parameter
	static r, t 			;stores return array of objects and the current array of objects
	
	if firstRun {			;reset static variables on fresh runs
		r := [], t := [] 	;recursive calls will not reset these variables
	}
	
	b := a.Clone()
	if b.length() {					;if b has anything left
		for k, v in b.pop() {			;work on last array, and reduce size of array of array
			t.InsertAt(1, v), g(b, false) 	;append object at the front of array t
		}					;do this again for b (whose size has been reduced)
	}
	else {							;if b is empty
		if !r.length() {				;if this is the first run	
			r.push(t), t := []			;record result and reset t
		} else {					;if this is not the first run
			_t := r[r.push()].Clone()		;get previous t stored in r
			loop t.length() {			
				_t[A_Index] := t[A_Index]	;replace appropriate spots with new t
			}			
			r.push(_t), t := []			;push this as modified t back in r
		}						;reset t at the end of the branch
	}
	
	return r						;we will only receive return value of the first call
}
