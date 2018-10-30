#singleinstance force

/*
Expression(expr)
	string - start with " (double quote only) => remove quotation marks
	function (include method) => evaluate
	Object => fetch element
	number => number + 0
	variable => %variable%

	whenever something new is encountered, we call f.parse.expr(newStringToExamine)


*/

gui := GuiCreate()
gui.SetFont(, "Consolas")
editBox := gui.Add("Edit", "r1 w600", 'f._if(f.gt(InputBox(),100), MsgBox("Greater than 100"), MsgBox("Less than 100"))')
btn := gui.Add("Button", "Default w0 h0", "OK")
btn.OnEvent("Click", "eval")
gui.Add("Text", , "Output:")
textBox := gui.Add("Edit", "r20 w600 ReadOnly -wrap", "")
gui.Add("Text", , "Debug Output:")
debugBox := gui.Add("Edit", "r20 w600 ReadOnly -wrap", "")
gui.Show()

debugBox.Value := "You can see how functions are evaluated here"
textBox.Value :=    "You can evaluate functions here, and they can be nested (as in the example above)"
				. "`nBuilting functions such as MsgBox/InputBox are supported.  You can even create a gui here."
				. "`nTo perform simple arithmetics, use f.add(a,b), f.mul(a,b), f.div(a,b)... see file for more"
				. "`nf.set(String_Name_of_Variable, value_to_assign, optional_array_indices)"
				. '`n  => for example: f.set("a", 100, 1, 2) performs a[1,2]:=100'
				. '`nTo get object properties/array item use obj["class1"]["prop"] or arr[1][2][3]'
				. '`n"counter" is an increasing counter, you can use f.add(counter, 0) to retrieve'
				. '`nits value or f.set("counter", 0) to reset its value.'
				. '`n"ans" is the variable that holds the last returned value.'
				. '`n`n'

;~ ;you can use f.add(counter, 0) to retrieve its value or f.set("counter", 0) to reset its value
;~ counter := 0
;~ while true {
	;~ counter++
	;~ sleep 1000
;~ }




;debug print to debugBox editbox
dprint(msg) {
	global
	debugBox.Value := debugBox.Value . "`n" . msg
	PostMessage( 0x115, 7, , "Edit3", "ahk_id " gui.hwnd)
}

eval() {
	global
	
	ans := f.parse.expr(editBox.Value)
	textBox.Value := textBox.Value . "`n" . editBox.Value . "`n>" ans
	editBox.Value := ""
	PostMessage( 0x115, 7, , "Edit2", "ahk_id " gui.hwnd)

}


class f {
	static layer := -1 ;used for debug output's indentations
	
	;A few functions that I have defined are listed here.  Scroll way down for the expression parser.
	
	;just binds it back, maybe turn off mapping for this key, too?
	restore(key) {
		Hotkey(key, ()=>send("{" key " down}"))
		Hotkey(key " up", ()=>send("{" key " up}"))
	}
	
	;binds a key to function, such as f.bind("f1", MsgBox("F1 key pressed")
	bind(key1, function) {
		;implemented inside f.parse.function
	}
	
	;binds one key to another: f.bindkey("f1", "shift").  also binds up/down state
	bindKey(key1, key2) {
		Hotkey(key1, ()=>send("{" key2 " down}"))
		Hotkey(key1 " up", ()=>send("{" key2 " up}"))
	}
	
	;will short-circuit
	or(condition*) {
		;implemented inside f.parse.function, only fall through case here
		return false
	}
	
	;will short-circuit
	and(condition*) {
		;implemented inside f.parse.function, only fall through case here
		return true
	}
	
	not(v) {
		return !v
	}
	
	;simple inline if/else statement.  ugly, i know
	_if(condition, if_true_dostuff, if_false_dostuff) {
		;implemented inside f.parse.function
	}
	
	;assignment, can be used to assign array:  set("a", f.add(1+5), 1,2,5) => a[1][2][5] := 6
	set(var, expr, ind*) {	
		global
		
		if ind.Length() {
			if !f.parse.isObject(var) {
				%var% := []
			}
			%var%[ind*] := expr
		} 
		else {
			%var% := expr
		}
		return %var%
	}
	
	free(var) {
		global
		%var% := ""
	}
	
	add(param*) {
		r := 0
		for _, v in param {
			r += v + 0
		}
		return r
	}
	
	;not a cat.  it combines strings
	cat(param*) {
		r := ""
		for _, v in param {
			r .= v
		}
		return r
	}
	
	mul(v*) {
		r := 1
		loop v.count() {
			r *= v[A_Index]
		}
		return r
	}
	
	div(a, b) {
		return a / b
	}
	
	divf(a, b) {
		return a//b
	}
	
	eq(a, b) {
		return a == b
	}
	
	eq2(a, b) {
		return a = b
	}
	
	gt(a, b) {
		return a > b
	}
	
	lt(a, b) {
		return a < b
	}
	
	gteq(a, b) {
		return a >= b
	}
	
	lteq(a, b) {
		return a <= b
	}
	
	;The parser starts here
	class parse {
		;handles generic expressions, and decides what to do with them
		expr(str) {
			global

			local r
			local parenPos := InStr(str, "(")
			local braketPos := InStr(str, "[")
			local nameLast
			
			if f.parse.isString(str) {
				r := f.parse.string(str)
			}
			else if parenPos || braketPos {
				if parenPos && braketPos {
					nameLastPos := Min(parenPos, braketPos) - 1 
				}
				else {
					nameLastPos := Max(parenPos, braketPos) - 1
				}
				
				name := SubStr(str, 1, nameLastPos)
				
				if f.parse.isObject(name) {
					r := f.parse.array(str, name)
				}
				else {
					r := f.parse.function(str, name, parenPos)
				}
				
			} 
			else if f.parse.isNumber(str) {
				r := f.parse.number(str)
			}
			else { ;treat it as a variable
				r := f.parse.variable(str)
			}
			
			return r
		}
		
		;functions are sent here
		function(funcString, funcName, firstParenPos) {
			global
			
			local parenCount := 0
			local lastPos := firstParenPos + 1
			
			;output stuff
			f.layer++

			dprint("Expression: " f.parse.spaces(f.layer) funcString)
			
			local param2 := []
			loop Parse funcString {
				if A_LoopField == "(" {
					parenCount++
				}
				else if A_LoopField == ")" {
					parenCount--
				}
				
				if parenCount==1 && A_LoopField == "," {
					_param := SubStr(funcString, lastPos, A_Index - lastPos)
					_param := Trim(_param, " `t`n`r")
					
					if _param != ""
						param2.push(_param)
					lastPos := A_Index + 1
				}
				
				if A_Index == StrLen(funcString) {
					_param := SubStr(funcString, lastPos, A_Index - lastPos)
					_param := Trim(_param, " `t`n`r")
					
					if _param != ""
						param2.push(_param)
				}
			}
			
			;convert param* into param[1], param[2], param[3] ... (variadic)
			for k, v in param2 {
				if SubStr(v, -1, 1) == "*" {
					if f.parse.isFunc(v) {
						local arrParam := f.parse.expr(SubStr(v, 1, -1))
						for i, j in arrParam {
							if i == 1 {
								param2[k] := j
							}
							else {
								param2.push(j)
							}
						}
					}
					else {
						for i, j in %SubStr(v, 1, InStr(v, "*") - 1)% {
							;msgbox(j ":" (j is "Number"))
							if i == 1 {
								param2[k] := j
							}
							else {
								param2.push(j)
							}
						}
					}
				}	
			}
			
			;exceptions are coded here, ie: short-circuit and/or... etc
			local param := []
			if funcName = "f.or" {
				for _, v in param2 {
					if f.parse.expr(v) {
						return true
					}
				}
			}
			else if funcName = "f.and" {
				for _, v in param2 {
					if !f.parse.expr(v) {
						return false
					}
				}
			}
			else if funcName = "f._if" {
				if f.parse.expr(param2[1]) {
					return f.parse.expr(param2[2])
				}	
				else {
					return f.parse.expr(param2[3])
				}
			}
			else if funcName = "f.bind" {
				Hotkey(StrReplace(param2[1], '"'), ()=>f.parse.expr(param2[2]))
				return true
			}
			else {
				for k, v in param2 {
					param[k] := f.parse.expr(v)
				}
			}
			
			;if calling a method
			local firstDot := InStr(funcName, ".")
			if firstDot {
				local oName := SubStr(funcName, 1, firstDot - 1)
				local str := SubStr(funcName, firstDot + 1)
				local last := 1
				local pArr := []
				
				Loop Parse str {
					if A_LoopField == "."  {
						pArr.push(SubStr(str, last, A_Index - last))
						last := A_Index + 1
					}
					if StrLen(str) == A_Index {
						pArr.push(SubStr(str, last))
					}
				}

				local p := %oName%
				local i := 1
				while i < pArr.Length() {
					p := p[ pArr[i++] ]
				}
				r := p[pArr[i++]](param*)
			}
			;not a method, just a plain ol' function
			else {
				r := Func(funcName).Call(param*)
			}

			;output stuff
			dprint("Return Val: " f.parse.spaces(f.layer) funcString " = " r)
			f.layer--
			
			return r
		}

		;handles arrays/object properties, only supports a[1][2][3] form
		array(str, name) {
			global
			
			local bcount := 0
			local last := 1
			local pArr := []
			str := SubStr(str, StrLen(name) + 1)
			Loop Parse str {
				if A_LoopField == "[" {
					bcount++
				}
				else if A_LoopField == "]" {
					bcount--
				}
				
				if !bcount {
					local tmp := f.parse.expr(SubStr(str, last + 1, A_Index - last - 1))
					;msgbox("tmp:[" tmp "]")
					;~ if StrLen(tmp)>0 {
						pArr.push(tmp)
						last := A_Index + 1
					;~ }
				}
			}
			;~ if !pArr.Length() {
				;~ msgbox("name:" name "  str:" str)
				;~ return %str%
			;~ }
			local r := %name%
			local i := 1
			while IsObject(r) {
				r:=r[ pArr[i++] ]
			}
			
			return r
		}
		
		;handles strings inside quotations, assuming they are always double quotes here
		string(str) {
			str := StrReplace(str, '"')
			str := StrReplace(str, "``n", "`n")
			return str
		}
		
		;handles variables so we can extract their values
		variable(str) {
			global
			
			try {
				r := %str%
			}
			catch {
				r := ""
			}
			
			return r
		}
		
		;handles numbers, convert string number to actual number
		number(str) {
			return str + 0
		}
		
		;isFunc is now used for variadic params
		;[old] test to see if we have a function.  Not used since it doesn't recongize Gui methods
		;[old] instead, we assume object looking things that are not objects (f.isObject) are functions
		isFunc(s) {
			global
			local pPos := InStr(s, "(")	
			local firstDot := InStr(funcName, ".")
			
			if !firstDot {
				return isFunc(SubStr(s, 1, pPos - 1))
			}	
			else {
				local dotMethod := SubStr(s, firstDot)
				return isFunc(%oName%.__Class dotMethod)
			}
		}
		
		;check to see if we have an object.  Placed inside try block because %var% throws an error when empty
		isObject(s) {
			global
			try {
				return IsObject(%s%)
			}	
			catch {
				return false
			}
		}
		
		;if it starts with a double quote.  Single quote not supported
		isString(s) {
			return SubStr(s, 1, 1) == '"'	
		}
		
		isNumber(s) {
			return s is "Number"
		}
		
		;lets us indent output
		spaces(n) {
			loop n {
				str .= "    " ;4 spaces
			}
			
			return str
		}
	
	}

}
