/*	Command Prompt Monitor
	Each command prompt program has its own conhost.exe.  A rolling text buffer can be found within each conhost.exe.  
	The memory offsets follows:
		text buffer starting address: conhost.exe + 0x8BE80, 0x158, 0x570, 0x20, 0x08
		text buffer current row (0 indexed, when greater than max lines): conhost.exe + 0x8BE80, 0x158, 0x570, 0x20, 0x20
		text buffer number of characters per line: conhost.exe + 0x8BE80, 0x158, 0x570, 0x20, 0x22
		text buffer max number of lines (0 indexed): conhost.exe + 0x8BE80, 0x158, 0x570, 0x20, 0x24
		text buffer current row (0 indexed, when less than max lines): conhost.exe + 0x8BE80, 0x158, 0x570, 0x20, 0x18, 0x18
	
	The idea is to break down the text buffer into three parts:
	1) Display Buffer - The most current 1000 lines (so multiline commands and past commands will display and log properly)
	2) Log Page(s) - Every COMPLETE 1000 lines, with no overlap with the Display Buffer
	3) Log Buffer - Everything inbetween the Display Buffer and the last compeleted Log Page
	
	The "Default" tab displays Log Pages + Log Buffer + Display Buffer.  Only Log Buffer and Display Buffer change in realtime.
	Once we have over 10000 lines of Log Pages, they are flushed to the "Overflow" tab or user designated log file.
	This is done to prevent slow down caused by frequently updated large text that are being displayed.
	
	Limitations:
	1) If the current row moves back (reduces) by more than 1000 (exceeds the default Display Buffer size), changes that occur
	   on lines preceding the Display Buffer may or may not be picked up by this program dependant upon whether or not they 
	   fall within the range of Log Buffer.
	2) Time stamps if implemented, will not reflect the exact instance when buffer is read, since they are read many 
	   times per second.
	   
	Alternative implementations:
	  Detection of user input and command execution states can be done for certain programs that run inside the Command Prompt.
	I have yet to find a reliable solution that works for all commands and programs.  For limited case such as using only julia.exe, this might work.
*/
#include classMemory_AHK2_64bit.ahk
;#include print.ahk
#SingleInstance force

iniFile := "cmdMonitor.ini"
iniSection := "cmdMonitor"
if !FileExist(iniFile) {
	IniWrite("", iniFile, iniSection, "RunCommand")
	IniWrite("", iniFile, iniSection, "LogFileName")
}
_runCommand := IniRead(iniFile, iniSection, "RunCommand")
_logFileName := IniRead(iniFile, iniSection, "LogFileName")

;gui stuff
statusBarPart2Width := 160
gui := GuiCreate()
gui.SetFont(, "Consolas")
gui.Opt("+Resize")
gui.OnEvent("Size", "onResize")
gui.OnEvent("Close", "onClose")
tab := gui.Add("Tab3",, "Default|Overflow|Settings")
tab.UseTab("Default")
	mainText := gui.Add("Edit", "r20 w600 ReadOnly -E0x200 -wrap", "") ;-E0x200 = no border for our edit box
tab.UseTab("Overflow")
	overflowText := gui.Add("Edit", "r20 w600 ReadOnly -E0x200 -wrap", "")
tab.UseTab("Settings")
	gui.Add("Text", , "Name of log file: ")
	logFileName := gui.Add("Edit", "yp-2 xp100 w100", _logFileName)
	gui.Add("Text", "xp-100 yp26", "Auto run command: ")
	runCommand := gui.Add("Edit", "xp100 yp-2 w100", _runCommand)
	gui.Add("Text", "xp-100 yp26 +wrap"
		, 'If no log file name is provided, "overflow" tab will be used.`n' 
		. 'If log file name is provided, overflow will be flushed to file `n'
		. 'and display buffer will be saved to log file on exit. `n'
		. '`n'
		. 'Existing log file will be appended (not overwritten). `n'
		. 'Settings here are saved on exit to cmdMonitor.ini')
statusBar := gui.Add("StatusBar")

;run cmd.exe and read its memory
Run(A_ComSpec (runCommand.Value ? " /c" runCommand.Value : ""), , , pid)
cmd := new _classMemory("ahk_pid " pid, "conhost.exe")	;find conhost.exe child mode
ptrBaseAdd := cmd.read(cmd.baseAddress + 0x8BE80, "Int64", 0x158, 0x570, 0x20)
prev := 0 ;start at the beginning of text buffer on load
lineNum := 0
logPages := []
if logFileName.Value {
	log := "`n===============================================`n"
	log .= ">>> Log file started on " FormatTime(,"M/d/yyyy HH:mm:ss") " <<<`n"
	log .= "===============================================`n`n"
}
overflow := ""
width  := cmd.read(ptrBaseAdd + 0x22, "UShort")
gui.Show("w" width*5.6+30 " h300")
while true {
	if !WinExist("ahk_pid " cmd.parentPID) {
		onClose()
	}
	cmd.suspend() ;need appropriate access rights to work, without this there is no guarantee of logging everything
	width  := cmd.read(ptrBaseAdd + 0x22, "UShort")
	strAdd := cmd.read(ptrBaseAdd + 0x08, "Int64")
	height := cmd.read(ptrBaseAdd + 0x24, "UShort")
	curr1  := cmd.read(ptrBaseAdd + 0x18, "UShort", 0xA) 
	curr2  := cmd.read(ptrBaseAdd + 0x20, "UShort")
	curr   := mod(curr1 + curr2, height)
	
	
	;line counter & detect changes for logBuffer
	changeFlag := false
	if curr != prev {
		lineNum += rollDirection(curr, prev, height)
		prev := curr
		changeFlag := true
	} 
	
	;logging each thousand-page
	if lineNum - 1000 >= (logPages.Length() + 1) * 1000 {
		logTemp := ""
		pageStartLine := logPages.Length() * 1000
		loop 1000 {
			offset := Mod(pageStartLine + A_Index - 1, height) * width * 2
			logTemp .= cmd.readstring(strAdd + offset, width * 2, "UTF-16") "`n" 
		}
		logPages.Push(logTemp)
		log .= logTemp
	}
	
	;overflow
	if StrLen(log) >= 10000*width {
		if logFileName.Value
			FileAppend(log, logFileName.Value, "`n")
		else {
			overflowText.Value .= log
			PostMessage( 0x115, 7, , "Edit2", "ahk_id " gui.hwnd) ;scroll to the bottom of edit control
		}
		log := ""
	}
	
	;display buffer
	if curr1 > 999
		cDisplayBuffer := 1000
	else 
		cDisplayBuffer := curr1
	displayBuffer := ""
	loop cDisplayBuffer {
		offset := curr - cDisplayBuffer + A_Index - 1
		if offset < 0
			offset := height + offset
		displayBuffer .= cmd.readstring(strAdd + offset * width * 2, width * 2, "UTF-16") "`n"
	}
	displayBuffer := displayBuffer . cmd.readstring(strAdd + curr * width * 2, width*2, "UTF-16") "`n" ;add current line

	;fetch entries between last log entry and first entry of display buffer
	if changeFlag {
		lastLogEntryPlusOne := Mod(logPages.Length() * 1000, height)
		firstDisplayBufferEntry := curr - cDisplayBuffer
		if firstDisplayBufferEntry < 0
			firstsDisplayBufferEntry := firstDisplayBufferEntry + height
		if firstDisplayBufferEntry {
			logBuffer := ""
			loop Mod(firstDisplayBufferEntry - lastLogEntryPlusOne + height, height) {
				offset := Mod(lastLogEntryPlusOne + A_Index - 1, height) * width * 2 
				logBuffer .= cmd.readstring(strAdd + offset, width * 2, "UTF-16") "`n"
			}	
		}
	}
	
	WinSetTitle(WinGetTitle("ahk_pid " pid), "ahk_id " gui.Hwnd) ;clone Windows Title of the target
	combined := SubStr(log . logBuffer . displayBuffer , 1, -1) ;remove final "`n" (newline)
	if mainText.Value != combined {
		mainText.Value := combined
		PostMessage( 0x115, 7, , "Edit1", "ahk_id " gui.hwnd) ;scroll to the bottom of edit control
	}
	cmd.resume()
	
	statusBar.SetText("Total lines: " lineNum+1 " (" curr "/" height ")")
	sleep 100
}

onResize() {
	global
	tab.move("x2 y2 w" (gui.ClientPos.w-3) " h" (gui.ClientPos.h-22), true)
	mainText.move("x0 y0 w" (gui.ClientPos.w -8) " h" (gui.ClientPos.h-44), true)
	overflowText.move("x0 y0 w" (gui.ClientPos.w -8) " h" (gui.ClientPos.h-44), true)
}

onClose() {
	global
	IniWrite(Trim(runCommand.Value, " `t`n"), iniFile, iniSection, "RunCommand")
	IniWrite(Trim(logFileName.Value, " `t`n"), iniFile, iniSection, "LogFileName")	
	if logFileName.Value 
		FileAppend(log . logBuffer . displayBuffer, logFileName.Value, "`n")
	ExitApp()
}

rollDirection(curr, prev, max) { ;determines shortest distance and direction to go from prev to curr
	posDir := (curr > prev) ? (curr - prev) : (curr + max - prev) ;forward
	negDir := -1 * ((curr < prev) ? (prev - curr) : (prev + max - curr)) ;backward
	return (Abs(posDir) < Abs(negDir)) ? posDir : negDir
}
