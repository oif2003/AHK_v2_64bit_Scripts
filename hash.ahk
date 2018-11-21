/*
	Basic string and file hash functions for AutoHotkey v2 a100
	by oif2003 (21 Nov 2018)
	Supported hash algorithms: MD2, MD4, MD5, SHA1, SHA256, SHA384, SHA512
*/
#SingleInstance force
;Gui stuff for string and file examples
gui := GuiCreate(, "String and file hash demo")
gui.SetFont("s10", "Consolas")
editbox := gui.Add("Edit", "r19 w1000 readonly -VSCROLL")
gui.Show()

String_To_Hash := "Hash me!"
Hash_Types := ["MD2","MD4","MD5","SHA1","SHA256","SHA384","SHA512","AES"]

editbox.Value .= "string = " String_To_Hash "`n"
for _, v in Hash_Types {
	editbox.Value .= format("{:-6}", v) " = " hashString(String_To_Hash, v) (k != Hash_Types.Length() ? "`n" : "")
}

editbox.Value .= "`n"
editbox.Value .= "file   = " A_ScriptFullPath "`n"
for _, v in Hash_Types {
	editbox.Value .= format("{:-6}", v) " = " hashFile(A_ScriptFullPath, v) (k != Hash_Types.Length() ? "`n" : "")
}


hashFile(file, algo) {	;using CertUtil
	static cPid := 0
	static Supported_Hash_Types := Object("MD2",_,"MD4",_,"MD5",_,"SHA1",_,"SHA256",_,"SHA384",_,"SHA512",_)
	
	;check to see if requested algorithm is supported
	algo := StrUpper(algo)
	if !Supported_Hash_Types.HasKey(algo) {
		return "Unsupported request"
	}
	
	if !cPid {
		_A_DetectHiddenWindows := A_DetectHiddenWindows
		A_DetectHiddenWindows := true
		Run(A_ComSpec " /k ",,"Hide", cPid)
		WinWait("ahk_pid" cPid,, 10)
		DllCall("AttachConsole","uint",cPid)
		A_DetectHiddenWindows := _A_DetectHiddenWindows
	}

	objShell := ComObjCreate("WScript.Shell")
	objExec := objShell.Exec('certutil -hashfile "' file '" ' algo)
	strStdOut:=strStdErr:=""
	while !objExec.StdOut.AtEndOfStream
		 strStdOut := objExec.StdOut.ReadAll()
	while !objExec.StdErr.AtEndOfStream
		 strStdErr := objExec.StdErr.ReadAll()

	r := strStdOut strStdErr
	SplitPath(file, fileName)
	RegExMatch(r, "(?<=" fileName ":)(.|`r|`n)*(?=CertUtil)", match)
	return StrUpper(StrReplace(match.Value(0), "`n"))
}


hashString(string, algo) {
	;These are the supported formats, maybe someone else can get the rest to work?
	static Supported_Hash_Types := Object("MD2",_,"MD4",_,"MD5",_,"SHA1",_,"SHA256",_,"SHA384",_,"SHA512",_)
	
	;check to see if requested algorithm is supported
	algo := StrUpper(algo)
	if !Supported_Hash_Types.HasKey(algo) {
		return "Unsupported request"
	}
	
	;continue if algorithm is supported
	hModule := DllCall("LoadLibrary", "Str", "Bcrypt.dll", "Ptr")
	size := StrPutVar(string, str) - 1	;put string in str variable as UTF-8 for later use
	
	;See link for explaination of steps taken below: 
	;https://docs.microsoft.com/en-us/windows/desktop/SecCNG/creating-a-hash-with-cng
	
		;https://docs.microsoft.com/en-us/windows/desktop/api/Bcrypt/nf-bcrypt-bcryptopenalgorithmprovider
		DllCall("Bcrypt.dll\BCryptOpenAlgorithmProvider"
			,"Ptr*", phandle
			,"Str", algo
			,"Str", 
			,"UInt", 0
			,"UInt"	;returned error code see winnt.h
		)

		;https://docs.microsoft.com/en-us/windows/desktop/api/Bcrypt/nf-bcrypt-bcryptgetproperty
		DllCall("Bcrypt.dll\BCryptGetProperty"
			,"Ptr", phandle
			,"Str", "ObjectLength"
			,"UInt*", pbOutput
			,"UInt", 4
			,"UInt*", pcbResult
			,"UInt", 0
			,"UInt"	;returned error code see winnt.h
		)

		;https://docs.microsoft.com/en-us/windows/desktop/api/Bcrypt/nf-bcrypt-bcryptcreatehash
		cbHashObject := pbOutput
		VarSetCapacity(pbHashObject, cbHashObject, 0)
		DllCall("Bcrypt.dll\BCryptCreateHash"
			,"Ptr", phandle
			,"Ptr*", phHash
			,"Ptr", &pbHashObject
			,"UInt", cbHashObject
			,"Ptr", 0
			,"UInt", 0
			,"UInt", 0
			,"UInt"	;returned error code see winnt.h
		)

		;https://docs.microsoft.com/en-us/windows/desktop/api/Bcrypt/nf-bcrypt-bcrypthashdata
		DllCall("Bcrypt.dll\BCryptHashData"
			,"Ptr", phHash
			,"Ptr", &str ;&pbInput
			,"UInt", size ;cbInput
			,"UInt", 0
			,"UInt"	;returned error code see winnt.h
		)

		DllCall("Bcrypt.dll\BCryptGetProperty"
			,"Ptr", phandle
			,"Str", "HashDigestLength"
			,"UInt*", pbOutput
			,"UInt", 4
			,"UInt*", pcbResult
			,"UInt", 0
			,"UInt"	;returned error code see winnt.h
		)
		hashsize := pbOutput

		;https://docs.microsoft.com/en-us/windows/desktop/api/Bcrypt/nf-bcrypt-bcryptfinishhash
		cbOutput := hashsize
		VarSetCapacity(pbOutput, cbOutput, 0)
		DllCall("Bcrypt.dll\BCryptFinishHash"
			,"Ptr", phHash
			,"Ptr", &pbOutput
			,"UInt", cbOutput
			,"UInt", 0
			,"UInt"	;returned error code see winnt.h
		)

		;read each byte and append its 2 digit hex value to hashstr
		loop hashsize {
			hashstr .= format("{:02x}", NumGet(&pbOutput, A_Index - 1 , "UChar"))
		}

		;clean up
		DllCall("Bcrypt.dll\BCryptDestroyHash", "Ptr", phHash, "UInt")
		DllCall("Bcrypt.dll\BCryptCloseAlgorithmProvider", "Ptr", phandle, "UInt", 0, "UInt")
		DllCall("FreeLibrary", "Ptr", hModule)

	return StrUpper(hashstr)
	
	;helper function
		StrPutVar(string, ByRef var) {	;from AHK v2 doc
			VarSetCapacity(var, StrPut(string, "Utf-8"))
			return StrPut(string, &var, "Utf-8")
		}
}
