#SingleInstance Force
timeOutInterval := 300	;set the maximum allowed interval between keys in milliseconds

`:: 
keypress() {
	global timeOutInterval
	static pressCount := 0
	
	pressCount++
	currCount := pressCount	;the number of keypresses when we start the timer
	SetTimer(()=>expired(), -timeOutInterval)
	
	expired() {
		; if timer expires without seeing more keypresses
		if currCount == pressCount {
			;Do stuff based on number of keypresses
			ToolTip(pressCount " presses detected.")
			SetTimer(()=>ToolTip(), -1000)
			
			pressCount := 0
		}	
	}
}