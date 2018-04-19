;.486
;.model flat
;.stack 100h

include ..\..\Irvine\Irvine32.inc

getstring	PROTO Near32 stdcall, lpStringToGet:dword, dlength:dword
putstring	PROTO Near32 stdcall, lpStringToPrint:dword
ascint32 	PROTO Near32 stdcall, lpStringOfNumericChars:dword
intasc32	PROTO Near32 stdcall, lpStringToHold:dword, dVal:dword
memoryallocBailey  PROTO Near32 stdcall, dNumBytes:dword 
ExitProcess PROTO, dwExitCode:dword

.data
	strEnter1 byte 10,9,"enter a string: ",0
	strEnter2 byte 10,9,"enter a string: ",0
	strString1 byte 32 dup(0)
	strString2 byte 32 dup(0)
	lowerPrompt byte 10,9,"Enter the number of the string you wish to convert: ",0
	strIndexPrompt byte 10,9,"Enter the character you are looking for: ",0
	strIndexError byte 10,9,"Sorry, that character does not exist in String 1",0
	invalidPrompt1 byte 10,9,"Sorry, string ",0
	invalidPrompt2 byte " does not exist. Please try again.",0
	userInput byte 2 dup(?)
	indexOf1Int byte (0) ;main driver
	indexOf byte "-1",0	 ;main driver
	indexOf2 byte "-1",0 ;main driver
	indexOf3 byte "-1",0 ;main driver
	lastIndexOf byte "-1",0 ;main driver
	lastIndexOf2 byte "-1",0 ;main driver
	lastIndexOf3 byte "-1",0 ;main driver
	indexOF2input byte 3 dup(0)
	indexOf2Prompt byte 10,9,"Enter the index you wish to start the search from (0 to 31): ",0
	strIndexError2 byte 10,9,"Sorry, the character you have chosen is not present after the specified index",0
	toReplacePrompt byte 10,9,"Please pick a character to replace: ",0
	ReplaceWith byte 10,9,"Please pick a replacement: ",0
	strChartoReplace byte 2 dup(0)
	strReplace byte 2 dup (0)
	strConcatAddr dword 0
	intStrAddr	dword (0)
	
.code

main PROC

	invoke putstring, addr strEnter1
	invoke getstring, addr strString1, 31
	invoke putstring, addr strEnter2
	invoke getstring, addr strString2, 31
	push offset strString2
	push offset strString1
	call String_concat
	mov strConcatAddr, EAX
	mov edx, eax
	call WriteString
	
	invoke ExitProcess,0
	
main ENDP

String_toLowerCase PROC

	push EBP											;save ebp
	push eax											;save eax
	push esi											;save esi
	push ebx											;save ebx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into strings
	
_begin:

	invoke putstring, ADDR lowerPrompt					;displays input prompt
	invoke getstring, ADDR userInput,1					;gets user input prompt
	mov AL, userInput
	.IF AL == 31h	
		mov EBX, [EBP + 20]								;move offset address of string1 into EBX
		jmp _lowerconvert								;jump to the conversion
	.ELSEIF AL == 32h
		mov EBX, [EBP + 24]								;move offset address of string2 into EBX
		jmp _lowerconvert								;jump to the conversion
	.ELSE
		invoke putstring, ADDR invalidPrompt1			;first half of error message
		invoke putstring, ADDR userInput				;display the invalid entry
		invoke putstring, ADDR invalidPrompt2			;second half of error message
		jmp _begin
	.ENDIF
	
_lowerconvert:

	cmp byte ptr[EBX+ESI],0 							;reached the end of he string if character == null
	JE _end												;jump to the end once the end of the string is reached
	cmp byte ptr[EBX+ESI],5Ah 							;don't convert characters that aren't letters
	JG _next											;jump to next
	cmp byte ptr[EBX+ESI],41h							;don't convert anything that isn't a uppercase letter
	JL _next											;jump if less to next
	add byte ptr[EBX+ESI],20h							;converts uppercase to lowercase
_next:

	inc ESI												;moves to next byte location 
	jmp _lowerconvert									;jump to beginning of loop
	
_end:

	mov ESP, EBP										;restore stackpointer to original location
	pop EBX												;restore ebx
	pop ESI												;restore esi
	pop EAX												;restore eax
	pop EBP												;restore ebp
	ret													;return 
	
String_toLowerCase ENDP

String_toUpperCase PROC

	push EBP											;save ebp
	push EAX											;save eax
	push ESI											;save esi
	push EBX											;save ebx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into strings
	
_begin:

	invoke putstring, ADDR lowerPrompt					;displays input prompt
	invoke getstring, ADDR userInput,1					;gets user input prompt
	mov AL, userInput
	.IF AL == 31h	
		mov EBX, [EBP + 20]								;move offset address of string1 into EBX
		jmp _upperconvert								;jump to the conversion
	.ELSEIF AL == 32h
		mov EBX, [EBP + 24]								;move offset address of string2 into EBX
		jmp _upperconvert								;jump to the conversion
	.ELSE
		invoke putstring, ADDR invalidPrompt1			;first half of error message
		invoke putstring, ADDR userInput				;display the invalid entry
		invoke putstring, ADDR invalidPrompt2			;second half of error message
		jmp _begin
	.ENDIF
	
_upperconvert:

	cmp byte ptr[EBX+ESI],0 							;reached the end of he string if character == null
	JE _end												;jump to the end once the end of the string is reached
	cmp byte ptr[EBX+ESI],7Ah 							;don't convert characters that aren't letters
	JG _next											;jump to next
	cmp byte ptr[EBX+ESI],61h							;don't convert anything that isn't a uppercase letter
	JL _next											;jump if less to next
	AND byte ptr[EBX+ESI], 0DFh							;converts lowercase to uppercase
_next:

	inc ESI												;moves to next byte location 
	jmp _upperconvert									;jump to beginning of loop
	
_end:

	mov ESP, EBP										;restore stackpointer to original location
	pop EBX												;restore ebx
	pop ESI												;restore esi
	pop EAX												;restore eax
	pop EBP												;restore ebp
	ret													;return 
	
String_toUpperCase ENDP

String_indexOf_1 PROC

	push EBP											;save ebp
	push ESI											;save esi
	push EBX											;save ebx
	push ECX											;save ecx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into string
	mov ECX,0											;clears ECX
	mov EAX, 0											;clears eax
	invoke putstring, ADDR strIndexPrompt				;displays input prompt
	invoke getstring, ADDR userInput,1					;gets user input
	mov CL, userInput									;stores userInput in CL for comparison
	mov EBX, [EBP + 20]									;move offset address of string1 into EBX

_search:

														
														
	mov AL, byte ptr[EBX+ESI]							;moves each byte into AL for comparison
	.IF AL == 0											;reached the end of he string if character == null
		jmp _endstring									;jump to _endstring once the end of the string is reached
	.ELSEIF AL == CL									;checks to see if each byte contains corresponding character
		Jmp _next										;if byte contains character, jump to _next
	.ENDIF
	inc ESI												;increment ESI to move to next byte
	jmp _search											;loop _search
	
_endstring:

	invoke putstring, addr strIndexError				;display error if character not in string
	jmp _end											;jump to the end

_next:
	
	mov EAX, ESI										;stores index in EAX
	
_end:

	mov ESP, EBP										;restore stackpointer to original location
	pop ECX												;restore ecx
	pop EBX												;restore ebx
	pop ESI												;restore esi
	pop EBP												;restore ebp
	ret													;return 

String_indexOf_1 ENDP

String_indexOf_2 PROC

	push EBP											;save ebp
	push ESI											;save esi
	push EBX											;save ebx
	push ECX											;save ecx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into string
	mov ECX,0											;clear ECX
	mov EAX, 0											;clear EAX
	invoke putstring, ADDR strIndexPrompt				;displays input prompt for character
	invoke getstring, ADDR userInput,1					;gets user input
	mov CL, userInput									;stores userInput in CL for comparison
	invoke putstring, ADDR indexOf2Prompt				;displays input prompt for the index
	invoke getstring, ADDR indexOF2input,2				;gets user input
	invoke ascint32, ADDR indexOF2input					;converts user input to an integer for indexing
	mov EBX, [EBP + 20]									;move offset address of string1 into EBX
	mov ESI, EAX										;move specified index into ESI 
	mov EAX, 0											;clear EAX
	
_search:

	mov AL, byte ptr[EBX+ESI]							;moves each byte into AL for comparison
	.IF AL == 0											;reached the end of he string if character == null
		jmp _endstring									;jump to _endstring once the end of the string is reached
	.ELSEIF AL == CL									;checks to see if each byte contains corresponding character
		Jmp _next										;if byte contains character, jump to _next
	.ENDIF
	inc ESI												;increment ESI to move to next byte
	jmp _search											;loop _search
	
_endstring:

	invoke putstring, addr strIndexError2				;display error if character not in string after specified index
	jmp _end											;jump to the end
	
_next:

	mov EAX, ESI

_end:

	mov ESP, EBP										;restore stackpointer to original location
	pop ECX												;restore ecx
	pop EBX												;restore ebx
	pop ESI												;restore esi
	pop EBP												;restore ebp
	ret													;return
	
String_indexOf_2 ENDP

String_indexOf_3 PROC
	
	push EBP											;save ebp
	push EDX											;save EDX
	push EDI											;save EDI
	push ESI											;save esi
	push EBX											;save ebx
	push ECX											;save ecx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into string
	mov EDI,0											;clears EDI for indexing into string
	mov ECX,0											;clears ECX
	mov EAX,0											;clears eax
	mov EBX, [EBP + 28]									;move offset address of string1 into EBX
	mov ECX, [EBP + 32]									;move offset address of string2 into ECX
	

_search:
	
	mov AL, byte ptr[ECX+EDI]							;moves each byte into AL for comparison
	mov AH, byte ptr[EBX+ESI]							;moves each byte into AH for comparison
	.IF AL == 0											;reached the end of string2 if character == null
		jmp _endstring2									;jump to _endstring once the end of the string is reached
	.ELSEIF AH == 0										;checks to see if end of string 1
		Jmp _endstring1
	.ELSEIF AL == AH
		mov EDX, ESI
		inc EDI
	.ENDIF
	inc ESI												;increment ESI to move to next byte
	jmp _search											;loop _search
	
_endstring1:

	invoke putstring, addr strIndexError
	jmp _end											;jump to the end

_endstring2:

	mov EAX, 0
	inc EDX
	sub EDX, EDI
	mov AL, DL
	
_end:

	mov ESP, EBP										;restore stackpointer to original location
	pop ECX												;restore ecx
	pop EBX												;restore ebx
	pop ESI												;restore esi
	pop EDI												;restore EDI
	pop EDX												;restore EDX
	pop EBP												;restore ebp
	ret													;return 

String_indexOf_3 ENDP

String_lastIndexOf_1 PROC

	push EBP											;save ebp
	push EDI
	push ESI											;save esi
	push EBX											;save ebx
	push ECX											;save ecx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into string
	mov ECX,0											;clears ECX
	mov EAX, 0											;clears eax
	invoke putstring, ADDR strIndexPrompt				;displays input prompt
	invoke getstring, ADDR userInput,1					;gets user input
	mov CL, userInput									;stores userInput in CL for comparison
	mov EBX, [EBP + 24]									;move offset address of string1 into EBX

_search:

	mov AL, byte ptr[EBX+ESI]							;moves each byte into AL for comparison
	.IF AL == 0											;reached the end of he string if character == null
		jmp _next									    ;jump to _endstring once the end of the string is reached
	.ELSEIF AL == CL									;checks to see if each byte contains corresponding character
		mov EDI, ESI
	.ENDIF
	inc ESI												;increment ESI to move to next byte
	jmp _search											;loop _search
	

_next:
	
	mov EAX, EDI										;stores index in EAX
	jmp _end
	
_end:

	mov ESP, EBP										;restore stackpointer to original location
	pop ECX												;restore ecx
	pop EBX												;restore ebx
	pop ESI												;restore esi
	Pop EDI
	pop EBP												;restore ebp
	ret			

String_lastIndexOf_1 ENDP

String_lastIndexOf_2 PROC

	push EBP											;save ebp
	push EDI
	push ESI											;save esi
	push EBX											;save ebx
	push ECX											;save ecx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into string
	mov ECX,0											;clear ECX
	mov EAX, 0											;clear EAX
	invoke putstring, ADDR strIndexPrompt				;displays input prompt for character
	invoke getstring, ADDR userInput,1					;gets user input
	mov CL, userInput									;stores userInput in CL for comparison
	invoke putstring, ADDR indexOf2Prompt				;displays input prompt for the index
	invoke getstring, ADDR indexOF2input,2				;gets user input
	invoke ascint32, ADDR indexOF2input					;converts user input to an integer for indexing
	mov EBX, [EBP + 24]									;move offset address of string1 into EBX
	mov ESI, EAX										;move specified index into ESI 
	mov EAX, 0											;clear EAX
	
_search:

	mov AL, byte ptr[EBX+ESI]							;moves each byte into AL for comparison
	.IF AL == 0											;reached the end of he string if character == null
		jmp _next									    ;jump to _endstring once the end of the string is reached
	.ELSEIF AL == CL									;checks to see if each byte contains corresponding character
		mov EDI, ESI
	.ENDIF
	inc ESI												;increment ESI to move to next byte
	jmp _search											;loop _search
	

_next:
	
	mov EAX, EDI										;stores index in EAX
	jmp _end

_end:

	mov ESP, EBP										;restore stackpointer to original location
	pop ECX												;restore ecx
	pop EBX												;restore ebx
	pop ESI												;restore esi
	pop EDI
	pop EBP												;restore ebp
	ret													;return

String_lastIndexOf_2 ENDP

String_lastIndexOf_3 PROC

	push EBP											;save ebp
	push EDX											;save EDX
	push EDI											;save EDI
	push ESI											;save esi
	push EBX											;save ebx
	push ECX											;save ecx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into string
	mov EDI,0											;clears EDI for indexing into string
	mov ECX,0											;clears ECX
	mov EAX,0											;clears eax
	mov EBX, [EBP + 28]									;move offset address of string1 into EBX
	mov ECX, [EBP + 32]									;move offset address of string2 into ECX
	

_search:
	
	mov AL, byte ptr[ECX+EDI]							;moves each byte into AL for comparison
	mov AH, byte ptr[EBX+ESI]							;moves each byte into AH for comparison
	.IF AL == 0											;reached the end of string2 if character == null
		jmp _endstring2									;jump to _endstring once the end of the string is reached
	.ELSEIF AH == 0										;checks to see if end of string 1
		Jmp _endstring1
	.ELSEIF AL == AH
		mov EDX, ESI
		inc EDI
	.ENDIF
	inc ESI												;increment ESI to move to next byte
	jmp _search											;loop _search
	
_endstring2:
	mov EDI, 0
	inc ESI
	jmp _search

_endstring1:

	mov EAX, 0
	dec EDX
	sub EDX, EDI
	mov AL, DL
	
_end:

	mov ESP, EBP										;restore stackpointer to original location
	pop ECX												;restore ecx
	pop EBX												;restore ebx
	pop ESI												;restore esi
	pop EDI												;restore EDI
	pop EDX												;restore EDX
	pop EBP												;restore ebp
	ret													;return 

String_lastIndexOf_3 ENDP

String_replace PROC

	push EBP											;save ebp
	push ESI											;save esi
	push EBX											;save ebx
	push ECX											;save ecx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into string
	mov ECX,0											;clears ECX
	mov EAX, 0											;clears eax
	
_begin:

	invoke putstring, ADDR lowerPrompt					;displays input prompt
	invoke getstring, ADDR userInput,1					;gets user input
	mov AL, userInput									;stores userInput in AL for comparison
	.IF AL == 31h	
		mov EBX, [EBP + 20]								;move offset address of string1 into EBX
	.ELSEIF AL == 32h
		mov EBX, [EBP + 24]								;move offset address of string2 into EBX
	.ELSE
		invoke putstring, ADDR invalidPrompt1			;first half of error message
		invoke putstring, ADDR userInput				;display the invalid entry
		invoke putstring, ADDR invalidPrompt2			;second half of error message
		jmp _begin
	.ENDIF

	invoke putstring, ADDR toReplacePrompt
	invoke getstring, ADDR strChartoReplace,1
	invoke putstring, ADDR ReplaceWith
	invoke getstring, ADDR strReplace,1
	mov CL, strChartoReplace
	mov CH, strReplace
_search:

	mov AL, byte ptr[EBX+ESI]							;moves each byte into AL for comparison
	.IF AL == 0											;reached the end of he string if character == null
		jmp _end										;jump to _endstring once the end of the string is reached
	.ELSEIF AL == CL									;checks to see if each byte contains corresponding character
		mov byte ptr[EBX+ESI],CH						;if byte contains character, jump to _next
	.ENDIF
	inc ESI												;increment ESI to move to next byte
	jmp _search											;loop _search
	
_end:

	mov ESP, EBP										;restore stackpointer to original location
	pop ECX												;restore ecx
	pop EBX												;restore ebx
	pop ESI												;restore esi
	pop EBP												;restore ebp
	ret													;return 

String_replace ENDP

String_concat PROC

	push EBP											;save ebp
	push EDI
	push EDX
	push esi											;save esi
	push ebx											;save ebx
	mov EBP, ESP										;set ebp as the reference for the stackframe
	mov ESI,0											;clear esi for indexing into strings
	mov EAX,0
	mov edi,0
	invoke memoryallocBailey, 64
	.IF EAX == 0
		jmp _error
	.ELSE
		mov EDX, EAX
	.ENDIF
	mov ebx, [EBP+24]
	
	
_start:
	mov AH, byte ptr [EDX+EDI]
	mov AL, byte ptr [EBX+ESI]
	.IF AL != 0
		mov AH, AL
		inc esi
		inc edi
	.ELSE
		jmp _next
	.ENDIF
	jmp _start
	
_next:
	mov esi,0
	mov ebx, [EBP+28]
	
_loop:
	mov AH, byte ptr [EDX+EDI]
	mov AL, byte ptr [EBX+ESI]
	.IF AL != 0
		mov AH, AL
		inc esi
		inc edi
		jmp _loop
	.ENDIF
	
	mov EAX, EDX

_error:
	
	mov ESP, EBP										;restore stackpointer to original location
	pop EBX												;restore ebx
	pop ESI												;restore esi
	pop EDX
	pop EDI
	pop EBP												;restore ebp
	ret													;return 

String_concat ENDP



END main
