/*
	Simple Regular Expression Tester for AutoHotkey v2 using RegExMatch()
	Tested with build: AutoHotkey_2.0-a100-52515e2
*/
#singleinstance force

;Default Values
	defaultText := "Your text goes here"
	defaultRegex := "i)(your).(text)"
	defaultStartpos := 1
	width := 600
	font := "Consolas"

;Gui Stuff
	gui := guiCreate()
	gui.SetFont(, font)
	
	;setup regex box
	gui.Add("Text", , "RegEx String:")
	regex := gui.Add("Edit", "-wrap r1 w" width, defaultRegex)
	
	;setup start position box
	gui.Add("Text", , "Start Position:")
	startpos := gui.Add("Edit", "-wrap r1 w" width, defaultStartpos)

	;setup text box
	gui.Add("Text", , "Text:")
	text := gui.Add("Edit", "r20 w" width, defaultText)

	;setup result box
	gui.Add("Text", , "Results:")
	result := gui.Add("Edit", "+readonly r10 w" width)

	;Run doRegEx() whenever changes are detected
	text.OnEvent("Change", ()=>doRegEx())
	regex.OnEvent("Change", ()=>doRegEx())
	startpos.OnEvent("Change", ()=>doRegEx())
	
	gui.show()
	
	;first run
	doRegEx()


;When values in regex, startpos, or text changes this function is triggered
doRegEx() {
	global gui, regex, text, result, startpos
	
	;reset the result box
	result.value := ""
	
	;force use of \ as escape character
	if InStr(regex.value, "``") {
		result.value .= "Must use \ (backslash) as escape character instead of `` (backtick)."
		return
	}
	
	;get startpos value
	if startpos.value == "" {
		spv := 1
	}
	else {
		spv := startpos.value
		if !(spv is "Integer") || spv == 0 {
			result.value .= "Start Position must be a non-zero integer. (Blank = 1)"
			return
		}
	}
	
	;attempt RegExMatch
	try {
		pos := RegExMatch(text.value, regex.value, m, spv)
		;match found
		if pos {
			;use RegExReplace outputVar to count number of matches
			RegExReplace(text.value, regex.value , , matchCount, , spv)

			;get matched text
			matchedText := SubStr(text.Value, pos, m.Len)
			matchedText := "`t" StrReplace(matchedText, "`n", "`n`t")
			
			;print results
			result.value .= "First match found at position: " pos "`n"
			result.value .= "Number of matches: " matchCount "`n"
			result.value .= "Matched: `n"

			result.value .= matchedText "`n`n"
			result.value .= "Number of captured subpatterns: " m.Count() "`n"
			numDigits := floor(log(m.count())) + 1		;get number of digits of m.count()
			Loop m.Count() {
				nameStr := m.Name(A_Index) ? " (" m.Name(A_Index) ")" : ""
				result.value .= "[" format("{:0" numDigits "}", A_Index) "]" nameStr 
								. " pos: " m.Pos(A_Index)
								. ", len: " m.Len(A_Index) " => "
				result.value .=  m.value(A_Index) "`n"
			}
			
			;untested, included for completeness sake
			if m.Mark() {
				result.value .= "Name of last encountered (*MARK:NAME): " m.Mark() "`n"
			}
		}
		;no matches
		else {
			result.value .= "No matches found.`n"
		}
	}
	;RegExMatch exceptions : straight from AutoHotkey documentation
	catch e {
		result.value .= e.message 
		if e.message == "PCRE execution error." {
			result.value .= " (" e.extra ")`n"
			result.value .= '`nLikely errors: "too many possible empty-string matches" (-22), "recursion too deep" (-21), and "reached match limit" (-8). If these happen, try to redesign the pattern to be more restrictive, such as replacing each * with a ?, +, or a limit like {0,3} wherever feasible.'
		}
	}
}
