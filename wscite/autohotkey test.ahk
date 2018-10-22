/*
SciTEDirector.ahk

Director for SciTE: pilot a SciTE session.

// by Philippe Lhoste <PhiLho(a)GMX.net> http://Phi.Lho.free.fr
// File/Project history:
 1.00.000 -- 2007/05/14 (PL) -- Creation based on code by toralf
 1.00.001 -- 2007/05/14 (TO) -- Simplified gui and made more use of listbox
 1.00.002 -- 2007/05/23 (TO) -- fix: get the correct data by intializing data with zeros
*/
/* Copyright notice: See the PhiLhoSoftLicence.txt file for details.
This file is distributed under the zlib/libpng license.
Copyright (c) 2007 Philippe Lhoste / PhiLhoSoft
*/

#SingleInstance Force
#NoEnv
DetectHiddenWindows On

; http://msdn2.microsoft.com/en-us/library/ms649011.aspx
WM_COPYDATA = 0x4A
/*
http://msdn2.microsoft.com/en-us/library/ms649010.aspx
typedef struct tagCOPYDATASTRUCT {
    ULONG_PTR dwData; // Data identifier
    DWORD cbData;     // Size of data in lpData
    PVOID lpData;     // Pointer to data, can be NULL
} COPYDATASTRUCT, *PCOPYDATASTRUCT;
*/

commandsToSciTE =
(Join|
askfilename|askproperty
close|closing|currentmacro|cwd|enumproperties
exportashtml|exportasrtf|exportaspdf|exportaslatex|exportasxml
extender|find|goto||identity|insert|loadsession
macrocommand|macroenable|macrolist|menucommand
open|output|property|quit
reloadproperties|replaceall|saveas|savesession
)

EnvGet SciTE_Home, SciTE_Home	; In case of #NoEnv
exeSciTE := exeSciTE := SciTE_Home (SciTE_Home ? "\" : "") "SciTE.exe" 

VarSetCapacity(structCopyData, 3*4, 0)

Gui 1:Add, Button, gBtnStartSciTE vBtnStartSciTE, Start SciTE
Gui 1:Add, Text, xm, Command:
Gui 1:Add, ComboBox, vcommand, %commandsToSciTE%
Gui 1:Add, Edit, vparam w400, 70
Gui 1:Add, Button, gBtnSendData vBtnSendData Disabled, Send Data
Gui 1:Add, ListBox, vmessages w400 r10
Gui 1:Show, y0

Gui 1:+LastFound +AlwaysOnTop
directorID = 0
guiHwnd := WinExist()
guiHwndDec := guiHwnd + 0
OnMessage(WM_COPYDATA, "WM_COPYDATA")
OnExit CloseSciTE
Return

CloseSciTE:
GuiEscape:
GuiClose:
  SendSciTE("closing")
  ExitApp
Return

BtnStartSciTE:
	Run %exeSciTE% -director.hwnd=%guiHwndDec% "%A_ScriptFullPath%", , , pidSciTE
	GuiControl, 1: Disable, BtnStartSciTE
Return

; If the receiving application processes this message, it should return TRUE; otherwise, it should return FALSE.
WM_COPYDATA(hwndSender, pCopyDataStruct, msg, hwnd){
  	local size, pData, data
	
  	size := GetUInt(pCopyDataStruct, 4)
  	pData := GetUInt(pCopyDataStruct, 8)
  	VarSetCapacity(data, size, 0)
  	DllCall("RtlMoveMemory", "Str", data, "UInt", pData, "UInt", size)
  	StringSplit cmd, data, :
  	If (InStr(data,"identity:")=1){
    		directorID := hwndSender
    		GuiControl 1:Enable, BtnSendData
    		GuiControl 1:, messages, directorID = %directorID%
    	}
  	If (data = "closing:"){
        directorID = 0
  		  ExitApp
      }
  	GuiControl 1:, messages, (%size%) %data%
  }

BtnSendData:
	Gui 1:Submit, NoHide
	SendSciTE(command, param)
Return

SendSciTE(command, param=""){
  	local fullCommand
  
  	If (directorID = 0)
  		  Return
  
  	fullCommand = %command%:%param%
  
  	SetUInt(&structCopyData, StrLen(fullCommand), 4)
  	SetUInt(&structCopyData, &fullCommand, 8)
  
  	SendMessage WM_COPYDATA, guiHwnd, &structCopyData, , ahk_id %directorID%
  	GuiControl 1:, messages, send "%fullCommand%" with|(%ErrorLevel%) SendMessage %WM_COPYDATA%, %guiHwnd%, &structCopyData, , ahk_id %directorID%
  }

GetUInt(_sourceAddr, _offset = 0){
  	r := 0
	Loop 4 {
		a := _sourceAddr + _offset + A_Index-1
		
		b := 8*(A_Index-1)
		;result += * (_sourceAddr + _offset + A_Index-1) << b
		
		c := NumGet(a+0, 0, "UChar")

		
		
		
		c := c << b
		
		r := r + c
		
		

		
		
	}
  	Return r
  }
SetUInt(_destAddr, _integer, _offset = 0){
  	Loop 4
  		DllCall("RtlFillMemory"
  				, "UInt", _destAddr + _offset + A_Index-1
  				, "UInt", 1
  				, "UChar", (_integer >> 8*(A_Index-1)) & 0xFF)
  }