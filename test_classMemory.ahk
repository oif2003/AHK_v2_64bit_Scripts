#singleinstance force
#include classMemory_AHK2_64bit.ahk
#include print.ahk

if !WinExist("Command Prompt") {
	Msgbox("Open Command Prompt first!")
	ExitApp()
}

test := new _classMemory("Command Prompt", "conhost.exe") ;getting its child named "conhost.exe"
for k, _ in test.Thread {
	hprint(k ". Thread ID: " test.Thread[k].ThreadID)	
	hprint("     TebBase:     " test.Thread[k].TebBaseAddress)
	hprint("     StackBase:   " test.Thread[k].NT_TIB.StackBase)
	hprint("     StackLimit:  " test.Thread[k].NT_TIB.StackLimit)
	hprint("     ThreadStack" k-1 ":" test.ThreadStack[k-1])
}
print("")
loop test.aModule.count {
	hprint("[" A_Index ".] " test.aModule[A_Index].name " : " test.aModule[A_Index].lpBaseOfDll)
}
