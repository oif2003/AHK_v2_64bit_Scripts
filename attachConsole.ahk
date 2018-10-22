#include <print>
#SingleInstance force
GENERIC_READ := 0x80000000
GENERIC_WRITE := 0x40000000
CONSOLE_TEXTMODE_BUFFER := 0x1
FILE_SHARE_READ := 0x00000001	
FILE_SHARE_WRITE := 0x00000002


run("julia", , , pid)
WinWait("julia")
print(DllCall("AttachConsole", "UInt", pid))
;DllCall("AllocConsole")
VarSetCapacity(conTitle, 1000, 0)
DllCall("GetConsoleTitle", "Str", conTitle, "UInt", 1000)
print(conTitle)



/*
CreateConsoleScreenBuffer(
  _In_             DWORD               dwDesiredAccess,
  _In_             DWORD               dwShareMode,
  _In_opt_   const SECURITY_ATTRIBUTES *lpSecurityAttributes,
  _In_             DWORD               dwFlags,
  _Reserved_       LPVOID              lpScreenBufferData
);
*/

cHandle := DllCall("CreateConsoleScreenBuffer", "UInt", GENERIC_READ | GENERIC_WRITE, "UInt", FILE_SHARE_READ | FILE_SHARE_WRITE, "Ptr", , "UInt", CONSOLE_TEXTMODE_BUFFER, "Ptr", "")
hprint(cHandle)

/*
BOOL WINAPI GetCurrentConsoleFont(
  _In_  HANDLE             hConsoleOutput,
  _In_  BOOL               bMaximumWindow,
  _Out_ PCONSOLE_FONT_INFO lpConsoleCurrentFont
);
*/
VarSetCapacity(fontInfo, 16, 0)
r:=DllCall("GetCurrentConsoleFont", "UInt", cHandle, "Int", 0, "Ptr", fontInfo) 
print(r)
eprint(DllCall("GetLastError"))
msgbox()
;~ ;hprint(DllCall("GetStdHandle", "UInt", -11))

/*
BOOL WINAPI ReadConsoleOutput(
  _In_    HANDLE      hConsoleOutput,
  _Out_   PCHAR_INFO  lpBuffer,
  _In_    COORD       dwBufferSize,
  _In_    COORD       dwBufferCoord,
  _Inout_ PSMALL_RECT lpReadRegion
);

typedef struct _SMALL_RECT {
  SHORT Left;
  SHORT Top;
  SHORT Right;
  SHORT Bottom;
} SMALL_RECT;
*/


print("test:" DllCall("SetConsoleMode", "UInt", cHandle, "UInt", 0x1f))
dllcall("SetConsoleTitle", "Str", "test whatever i want")
eprint(DllCall("GetLastError"))



VarSetCapacity(rect, 8, 0)
NumPut(0, &rect, 0*2, "Short")
NumPut(0, &rect, 1*2, "Short")
NumPut(10, &rect, 2*2, "Short")
NumPut(1, &rect, 3*2, "Short")
VarSetCapacity(cBuffer, 1024*1000*2, 0)

VarSetCapacity(bufferSize, 4, 0)
NumPut(1000, &bufferSize, 0*2, "Short")
NumPut(1024, &bufferSize, 0*2, "Short")
r := DllCall("ReadConsoleOutput", "Ptr", cHandle, "Ptr", cBuffer, "Ptr", bufferSize, "Int64", 0,  "Ptr", rect)
print(r)

eprint(DllCall("GetLastError"))