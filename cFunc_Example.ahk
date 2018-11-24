;make a copy of the cFunc_Example.tcc.ahk and place it in your script's folder with the same name
;but the .tcc.ahk extention instead of .ahk, then #Include it as below
#Include cFunc_Example.tcc.ahk

hello() => DllCall(cFunc "hello", "AStr")	
/*_C func
	#define DLL __attribute__ ((dllexport)) 
	
	DLL char * hello(void)	
	{					
		return "Hello world!";
	}
*/

MsgBox(hello())