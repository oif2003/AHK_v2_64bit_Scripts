/* Julia+SciTE for AHK v2
	
*/

#include <classMemory_AHK2_64bit>
#include <print>
#include <sciteDirector>
#singleinstance force

DetectHiddenWindows true
cmdProgram := "ahk_exe conemuC64.exe"
clipsaved := ""
LINE_SEP := "\r\n"
scite := new SciTEdirector(A_ScriptDir "\wscite\sc1.exe")

juliaPath := "C:\Users\Terence\AppData\Local\Julia-1.0.1\bin\julia.exe"

if !WinExist(cmdProgram)
	startJulia()

while true {
	if !WinExist("ahk_pid " scite.pid)
		ExitApp()
	if !WinExist("ahk_pid " julia.ParentPID) {
		response := MsgBox("Julia REPL is not running.  Start it?",, "YesNo")
		if response = "Yes"
			startJulia()
		else
			ExitApp()
	}
	
	exeCount := julia.read(julia.baseAddress + 0x8BB9C, "UShort")
	isExecuting := Mod(exeCount - exeBase, 2)
	newCommand := exeCount != exePrev 

	if  newCommand or isExecuting {
		julia.suspend()
		exePrev := exeCount
		curr1  := julia.read(ptrBaseAdd + 0x18, "UShort", 0xA) 
		curr2  := julia.read(ptrBaseAdd + 0x20, "UShort")
		height := julia.read(ptrBaseAdd + 0x24, "UShort")
		width  := julia.read(ptrBaseAdd + 0x22, "UShort")
		strAdd := julia.read(ptrBaseAdd + 0x08, "Int64")
		curr   := mod(curr1 + curr2, height) 
		if curr != old {   
			line := old
			text := ""
			
			while curr != line {
				str := julia.readstring(strAdd + (line) * width * 2, width * 2, "UTF-16")
				;*2 because char size for UTF-16 is 2 bytes
				str := StrReplace(str, "\", "\\")	;C style escapes
				text := text LINE_SEP str
				line := Mod(line + 1, height) 				
			}
			scite.sendMsg("menucommand", "1107") ;unlock output
			scite.sendMsg("output", text)
			scite.sendMsg("menucommand", "1108") ;lock output
			old := curr
		}
		
		julia.resume()
	}
	sleep 100
}

startJulia() {
	global
	run('C:\Users\' A_UserName '\Desktop\ConEmu\ConEmu64.exe -run "' juliaPath '"')
	WinWait(cmdProgram)
	julia := new _classMemory(cmdProgram, "conhost.exe")	;find conhost.exe child mode
	exeBase := julia.read(julia.baseAddress + 0x8BB9C, "UShort") + 1
	exePrev := exeBase
	ptrBaseAdd := julia.read(julia.baseAddress + 0x8BE80, "Int64", 0x158, 0x570, 0x20)
	curr1  := julia.read(ptrBaseAdd + 0x18, "UShort", 0xA) 
	curr2  := julia.read(ptrBaseAdd + 0x20, "UShort")
	height := julia.read(ptrBaseAdd + 0x24, "UShort")
	curr   := mod(curr1 + curr2, height)  
	old := curr	
}

executeSection() {
	global clipsaved, julia, scite

	clipsaved := ClipboardAll()
	clipboard := ""
	scite.sendMsg("menucommand", "1109")
	while !Clipboard
		sleep 100
	
	if SubStr(ClipBoard, -1) != "`n"
	ClipBoard := ClipBoard "`n"
	MenuSelect("ahk_pid " julia.parentPID,, "0&", "Edit", "Paste")
	
	WinActivate("ahk_pid " scite.pid)
	Clipboard := clipsaved
	clipsaved := ""
}

runScript() {
	global clipsaved, julia, scite
	clipsaved := ClipboardAll()
	clipboard := ""
	scite.sendMsg("menucommand", "1106")
	while !Clipboard
		sleep 100
	
	title := "# Running file: " Clipboard "`n"
	Clipboard := title 'include("' StrReplace(Clipboard, "\", "\\") '")`n'
	MenuSelect("ahk_pid " julia.parentPID,, "0&", "Edit", "Paste")
	
	WinActivate("ahk_pid " scite.pid)
	Clipboard := clipsaved
	clipsaved := ""
}

^e::executeSection()
^r::runScript()