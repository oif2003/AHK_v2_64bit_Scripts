#singleinstance force

/*
The Idea:
1) User input string for evaluation
2) Save all string literals (stuff inside quotation marks) and replace them with tokens
3) Convert Infix operators to Prefix (functions)
4) Parse the functions based on its type:
	a) string - get rid of quotation marks
	b) function / method - evaluate
	c) object / array - get its value
	d) number - convert it from string to value (=> number + 0)
	e) variable - %variable%
	f) keep parsing until we get one of the above
*/

gui := GuiCreate()
gui.SetFont(, "Consolas")
editBox := gui.Add("Edit", "r1 w600", 'f._if(InputBox()>100, MsgBox("Greater than 100"), MsgBox("Less than or equal to 100"))')
btn := gui.Add("Button", "Default w0 h0", "OK")
btn.OnEvent("Click", "eval")
gui.Add("Text", , "Output:")
textBox := gui.Add("Edit", "r20 w600 ReadOnly -wrap", "")
gui.Add("Text", , "Debug Output:")
debugBox := gui.Add("Edit", "r10 w600 ReadOnly -wrap", "")
gui.Show()


debugBox.Value := "You can see how functions are evaluated here"
textBox.Value :=    "You can evaluate functions here, and they can be nested (as in the example above)"
				. "`nBuilting functions such as MsgBox/InputBox are supported.  You can even create a gui here."
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
	str := f.funcnator(editBox.Value)
	ans := f.parse.expr(str)
	textBox.Value := textBox.Value . "`n" . editBox.Value . "`n>" ans
	editBox.Value := ""
	PostMessage( 0x115, 7, , "Edit2", "ahk_id " gui.hwnd)

}


class f {
	static ranOnce := false
	
	;unused Unicode characters
	static basecode := 0x1abf - 1
	
	;Operators are defined here
	;order them from top (high priority) to bottom (low priority)
	;if two operations have the same priority
	;assign it the "priority" property pointing to the first occurence
	;of an operator with the desired priority level.  See below for example.
	static operators   :=[{	
							"operator":"**",
							"function":"f.pow",
							"operands":2  
						},{	
							"operator":"!",
							"function":"f.not",
							"operands":1  
						;~ },{	
							;~ "operator":"~",
							;~ "function":"f.not",
							;~ "operands":"1"  
						},{	
							"operator":"*",
							"function":"f.mul",
							"operands":2  
						},{	
							"operator":"/",
							"function":"f.div",
							"operands":2,
							"priority":"*"
						},{	
							"operator":"//",
							"function":"f.divf",
							"operands":2,
							"priority":"*"
						},{	
							"operator":"+",
							"function":"f.add",
							"operands":2  
						},{	
							"operator":"-",
							"function":"f.sub",
							"operands":2,
							"priority":"+"							
						;~ },{	
							;~ "operator":"<<",
							;~ "function":"f.set",
							;~ "operands":"2"  
						;~ },{	
							;~ "operator":">>",
							;~ "function":"f.not",
							;~ "operands":"1"  
						;~ },{	
							;~ "operator":"&",
							;~ "function":"f.set",
							;~ "operands":"2"  
						;~ },{	
							;~ "operator":"^",
							;~ "function":"f.not",
							;~ "operands":"1"  
						;~ },{	
							;~ "operator":"|",
							;~ "function":"f.set",
							;~ "operands":"2"  
						},{	
							"operator":" . ",
							"function":"f.cat",
							"operands":2  
						},{	
							"operator":"~=",
							"function":"f.regex",
							"operands":2  
						},{	
							"operator":">",
							"function":"f.gt",
							"operands":2  
						},{	
							"operator":"<",
							"function":"f.lt",
							"operands":2,
							"priority":">"								
						},{	
							"operator":">=",
							"function":"f.gteq",
							"operands":2,
							"priority":">" 
						},{	
							"operator":"<=",
							"function":"f.lseq",
							"operands":2,
							"priority":">"  
						},{	
							"operator":"=",
							"function":"f.eqci",
							"operands":2 
						},{	
							"operator":"==",
							"function":"f.eq",
							"operands":2,
							"priority":"="  
						},{	
							"operator":"!=",
							"function":"f.neq",
							"operands":2,
							"priority":"="  
						;~ },{	
							;~ "operator":"!==",
							;~ "function":"f.not",
							;~ "operands":"1"  
						;~ },{	
							;~ "operator":"is",
							;~ "function":"f.set",
							;~ "operands":"2"  
						;~ },{	
							;~ "operator":"not",
							;~ "function":"f.not",
							;~ "operands":"1"  
						},{	
							"operator":"&&",
							"function":"f.and",
							"operands":2  
						},{	
							"operator":"||",
							"function":"f.or",
							"operands":2 
						},{	
							"operator":"?",
							"function":"f._if",
							"operands":3  
						},{	
							"operator":":",
							"function":"f._if",
							"operands":3,
							"priority":"?"
						},{	
							"operator":":=",
							"function":"f.set",
							"operands":2  
						;~ },{	
							;~ "operator":"=>",
							;~ "function":"f.fset",
							;~ "operands":2  
						},{	
							"operator":",",
							"function":",",
							"operands":0  
						},					
					]



	static replace := []
	static op := {}

	funcnator(s) {
		if !this.ranOnce {
			this.init()
			this.ranOnce := true
		}
		;dict := {}
		dprint("Original  : " s)
		dict := f.saveStrings(s)
		s := f.replaceStrings(s, dict)
		dprint("Str Saved : " s)
		s := f.replaceOp(s)
		dprint("Op tokens : " s)
		s := f.functionate(s)
		dprint("Func Form : " s)
		s := f.cleanParen(s)
		dprint("De-paren  : " s)
		s := f.restoreStrings(s, dict)
		dprint("Func Form : " s)
		return s
	}

	init() {
		;setup our lookup arrays
		for k, v in f.operators {
			char := chr(f.basecode + k)
			len := StrLen(v.operator)
			if len == 3 { 	;longer operators to be replaced first
				f.replace[1, v.operator] := char
			}
			else if len == 2 {
				f.replace[2, v.operator] := char
			} 
			else {
				f.replace[3, v.operator] := char
			}
			f.op[char] := {}
			f.op[char].function := v.function
			f.op[char].operands := v.operands
			f.op[char].operator := v.operator
			if v.priority == "" {
				f.op[char].priority := k
			}
			else {
				for i, j in f.operators {
					if j.operator == v.priority {
						f.op[char].priority := i
						break
					}
				}
					
				if f.op[char].priority == "" {
					msgbox "error with shared priority"
				}
			}
		}
	}

	;replace strings with tokens so they don't get parsed
	;Note: this does not escape quotes
	saveStrings(s, quote := '"', token := 0x2DDF) {
		token := chr(token)	;some character hopefully no one else uses
		dict := {}
		
		;find strings
		qc := 0
		startPos := 1
		loop Parse s {
			if A_LoopField == quote {
				qc++
				if Mod(qc, 2) {
					;even number quotation characters mark beginning of string
					startPos := A_Index
				}
				else {
					;build out dictionary so we can swap strings for tokens later
					dict[token . startPos . token] := SubStr(s, startPos, A_Index-startPos + 1)
				}
				
			}
		}
		
		;replace strings with tokens
		for k, v in dict {
			s := StrReplace(s, v, k)
		}
		
		return dict
	}
	
	replaceStrings(s, dict, quote := '"') {
		for k, v in dict {
			s := StrReplace(s, v, k)
		}
		return s
	}
	
	;restore strings
	restoreStrings(s, dict, quote := '"') {
		for k, v in dict {
			s := StrReplace(s, k, v)
		}
		return s
	}
	
	cleanParen(s) {

		pc := 0
		bc := 0
		loop Parse s {
			if A_LoopField == "(" {
				pc++
			}
			else if A_LoopField == ")" {
				pc--
			}	
		}
		if bc || pc {
			msgbox("None matching number of parenthesis or brackets!")
		}

		while true {
			doubleParen := InStr(s, "((")
			if !doubleParen {
				break
			}
			
			loop Parse s {
				if A_Index == doubleParen + 1 {
					pc := 0
				}
				if A_LoopField == "(" {
					pc++
				}
				else if A_LoopField == ")" {
					pc--
				}
				if A_Index > doubleParen && !pc {
					s := SubStr(s, 1, A_Index - 1) . SubStr(s, A_Index + 1)
					s := SubStr(s, 1, doubleParen - 1) . SubStr(s, doubleParen + 1)
					break
				}
			}
		}
		
		while true {
			spaceParen := InStr(s, " (")
			if !spaceParen {
				break
			}
			
			loop Parse s {
				if A_Index == spaceParen + 1 {
					pc := 1
				}
				if A_LoopField == "(" {
					pc++
				}
				else if A_LoopField == ")" {
					pc--
				}
				if A_Index > spaceParen && !pc {
					s := SubStr(s, 1, A_Index - 1) . SubStr(s, A_Index + 1)
					s := SubStr(s, 1, spaceParen) . SubStr(s, spaceParen + 2)
					break
				}
			}
		}
		
		return s
	}

	replaceOp(s) {
		;iterate over operators for replacement
		for k, v in f.replace {
			for i, j in f.replace[k] {
				s := StrReplace(s, i, j)
			}	
		}
		return s
	}
	
	hasOperators(s) {
		loop Parse s {
			if f.op.HasKey(A_LoopField){
				return A_Index
			}
		}
		return 0
	}
	
	functionate(s) {
		
		loop {
			sArr := []
			bc := 0
			pc := 0
			lastPos := 1
			len := StrLen(s)
			highestPriorityLevel := 999
			highestPriorityIndex := 0
			sArr_Index := 0
			loop parse s {
				if A_LoopField == "(" {
					pc++
				}	
				else if A_LoopField == ")" {
					pc--
				}	
				else if A_LoopField == "[" {
					bc++
				}	
				else if A_LoopField == "]" {
					bc--
				}
			
				if !pc && !bc && f.op[A_LoopField] {
					stmp := SubStr(s, lastPos, A_Index - lastPos)
					
					sArr.push(stmp)
					sArr_Index++
					sArr.push(A_LoopField)
					sArr_Index++
					
					if f.op[A_LoopField].priority < highestPriorityLevel {
						highestPriorityLevel := f.op[A_LoopField].priority
						highestPriorityIndex := sArr_Index
					}
					lastPos := A_Index + 1
				}
				if A_Index == len && SubStr(s, lastPos)!="" {
					stmp := SubStr(s, lastPos)
					sArr.push(stmp)
					sArr_Index++
				}
			}
			
			;since operator are substitued for unreadable single width characters
			;we will output its readalbe name here in SciTE => fileappend( message "`n", "*")
			for k, v in sArr {
				sArr[k] := Trim(v, " `t`n`r")
				if f.op[sArr[k]] {	;if we have an operator, get its human readable name
					opNameStr := " = " f.operators[f.op[sArr[k]].priority].operator
				}
				else {
					opNameStr :=  ""
				}
			}
			
			if highestPriorityIndex > 0 {
				
				;if we have something like a comma wwith low precedence and does not need to be wrapped 
				;inside a function, we assign its operands value as zero and it will be replaced
				;with its original form here
				if !f.op[sArr[highestPriorityIndex]].operands {
					sArr[highestPriorityIndex] := f.op[sArr[highestPriorityIndex]].operator
				}
				else if f.op[sArr[highestPriorityIndex]].operands > 1 {
					;if we have a postfix operator like * that behaves differently based on its position
					if sArr[highestPriorityIndex + 1] == "" && f.op[sArr[highestPriorityIndex]].operator == "*" { ;operand2 doesn't exist
						sArr[highestPriorityIndex - 1] :=  sArr[highestPriorityIndex - 1] . f.op[sArr[highestPriorityIndex]].operator
						sArr.RemoveAt(highestPriorityIndex, 1)
					;ternary operators has 3 operands	
					} else if f.op[sArr[highestPriorityIndex]].operands == 3 {
						;this might be redundant but I don't want to fully test it
						if f.op[sArr[highestPriorityIndex]].operator == "?" {
							if sArr[highestPriorityIndex - 1] != "" && sArr[highestPriorityIndex + 1] != "" 
								&& sArr[highestPriorityIndex + 2] != "" && sArr[highestPriorityIndex + 3] != "" {
								sArr[highestPriorityIndex - 1] := f.op[sArr[highestPriorityIndex]].function 
								. "(" sArr[highestPriorityIndex - 1] . ", " . sArr[highestPriorityIndex + 1] 
								. ", " . sArr[highestPriorityIndex + 3] . ")" 
								sArr.RemoveAt(highestPriorityIndex, 4)
							}
							else {
								sArr[highestPriorityIndex] := "?" ;get rid of the token so won't go into infinite loop
								Msgbox("Missing operand(s) or operator : ")
							}
						}	; 1 ? 3 : 5
						else if f.op[sArr[highestPriorityIndex]].operator == ":" {
							if sArr[highestPriorityIndex - 3] != "" && sArr[highestPriorityIndex -2 ] != ""
								&& sArr[highestPriorityIndex -1] != "" && sArr[highestPriorityIndex + 1] != "" {
								sArr[highestPriorityIndex - 3] := f.op[sArr[highestPriorityIndex]].function 
								. "(" sArr[highestPriorityIndex - 3] . ", " . sArr[highestPriorityIndex - 1] 
								. ", " . sArr[highestPriorityIndex + 1] . ")" 
								sArr.RemoveAt(highestPriorityIndex - 2, 4)
							}
							else {
								sArr[highestPriorityIndex] := ":"
								Msgbox("Missing operand(s) or operator : ")
							}
						}
						else {
							msgbox("this is not suppose to happen... something other than ternary with 3 operands?")
						}
					}
					else {
						functionForm := f.op[sArr[highestPriorityIndex]].function 
							. "(" sArr[highestPriorityIndex - 1] 
							. ", " . sArr[highestPriorityIndex + 1] . ")" 
						sArr.RemoveAt(highestPriorityIndex - 1, 3)
						sArr.InsertAt(highestPriorityIndex - 1, functionForm)
					}
				}
				else {
					functionForm := f.op[sArr[highestPriorityIndex]].function 
						. "(" . sArr[highestPriorityIndex + 1] . ")" 	
					sArr.RemoveAt(highestPriorityIndex, 2)	
					sArr.InsertAt(highestPriorityIndex, functionForm)
				}
			}
			
			for k, v in sArr {
				opPos := f.hasOperators(sArr[k])
				if !f.op[sArr[k]] && opPos {
					
					len := StrLen(sArr[k])
					pPos := InStr(sArr[k], "(")
					bPos := InStr(sArr[k], "[")
					if bPos && bPos < pPos || !pPos { ;array
						part1end := bPos
						part2start := InStr(sArr[k], "]", , -1)
					} 
					else if pPos && pPos < bPos || !bPos { ;function
						part1end := pPos
						part2start := InStr(sArr[k], ")", , -1)
					}
					else { ;neither
						msgbox("Houston, we have a problem!")
					}
					part1 := SubStr(sArr[k], 1, part1end)
					mid := SubStr(sArr[k], part1end + 1, part2start - 1 - part1end)
					part2 := SubStr(sArr[k], part2start)
					
					
					sArr[k] := part1 . f.functionate(mid) . part2
				}
			}	
			
			s := f.join(sArr) 
			
		} until (!highestPriorityIndex)
		
		if SubStr(s, 1, 1) == "(" && SubStr(s, -1) == ")"
			s := SubStr(s, 2, -1)
		
		return s
	}

	join(arr, start:=1, end:=-1) {
		if end := -1 {
			end := arr.Length()
		}
		str := ""
		loop end - start + 1 {
			str .= arr[start + A_Index - 1]
		}
		return str
	}

	;debug output to be used with SciTE4AutoHotkey
	dp(msg) {
		fileappend(msg "`n", "*")
	}
	
	
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
	
	;assignment
	set(ByRef var, expr) {	
		;implemented inside f.parse.function
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
	
	sub(param*) {
		;this handles the negative case
		if param.length() == 1 {
			return -param[1]
		}
		else if param.length() == 2 {
			return param[1] - param[2]
		}
		else {
			Msgbox("this function only takes one or two parameters")
			return 0
		}
	}
	
	pow(a, b) {
		return a ** b
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
	
	eqci(a, b) {
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
	
	regex(a, b) {
		return RegExMatch(a, b)
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
			
			local skip := false
			local parenCount := 0
			local lastPos := firstParenPos + 1
			
			;output stuff
			dprint("Expression: " funcString)
			
			local param2 := []
			local quoteCount := 0
			loop Parse funcString {
				if A_LoopField == "(" {
					parenCount++
				}
				else if A_LoopField == ")" {
					parenCount--
				} else if A_LoopField == '"' {
					quoteCount++
				}
				else if A_LoopField == '"' {
					quoteCount--
				}
				
				if !Mod(quoteCount, 2) && parenCount==1 && A_LoopField == "," {
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
						r := true
						skip := true
						break
					}
				}
			}
			else if funcName = "f.and" {
				for _, v in param2 {
					if !f.parse.expr(v) {
						r := false
						skip := true
						break
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
				r := Hotkey(StrReplace(param2[1], '"'), ()=>f.parse.expr(param2[2]))
				skip := true
			} 
			else if funcName = "f.set" {
				if param2[2] == "[]" {
					r := %param2[1]% := []
				}
				else if param2[2] == "{}" {
					r := %param2[1]% := {}
				} 
				else if f.parse.isObject(param2[1]) {
					local arrInfo := f.parse.resolveArr(param2[1])
					
					local ind := []
					for k, v in arrInfo {
						if k == 1 {
							local arrName := v
						}
						else {
							ind.push(v)
						}
					}
					r := %arrName%[ind*] := f.parse.expr(param2[2])
				} 
				else {
					r := %param2[1]% := f.parse.expr(param2[2])
				}
				skip := true
			}
			else if fName == "fset" {
				%param2[1]%(p*)=>%param2[2]%(p*)
			}
			else {
				for k, v in param2 {
					param[k] := f.parse.expr(v)
				}
			}
			
			if !skip {
				
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
			}
			
			;output stuff
			dprint("Return Val: "  funcString " = " r)
			
			return r
		}


		resolveArr(str) {
			bcount := 0
			last := 1
			pArr := []
			bPos := InStr(str, "[")
			name := SubStr(str, 1, bPos - 1)
			str := SubStr(str, bPos)
		
			Loop Parse str {
				if A_LoopField == "[" {
					bcount++
				}
				else if A_LoopField == "]" {
					bcount--
				}
				
				if !bcount {
					
					tmp := f.parse.expr(SubStr(str, last + 1, A_Index - last - 1))
					;msgbox("tmp:[" tmp "]")
					;~ if StrLen(tmp)>0 {
						pArr.push(tmp)
						last := A_Index + 1
					;~ }
				}
			}
			return [name, pArr*]
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
			
			local bPos := InStr(s, "[")
			if bPos {
				s := SubStr(s, 1, bPos - 1)
			}
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
