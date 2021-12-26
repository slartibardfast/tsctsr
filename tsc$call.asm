[bits 16]

%use masm
ComFile		SEGMENT
		ORG	100h		; Com-type file

Main		PROC	near
		jmp	Main2		; Skip
Main		ENDP

InitialMsg	DB	13,44 DUP(10)
		DB	"Sample program #16 - Demonstrates absolute timestamping in mode 2 from tsc$",13,10
		DB	"Part of the PC Timing FAQ / Application notes",13,10
		DB	"By K. Heidenstrom (kheidens@actrix.gen.nz)",13,10,13,10
		DB	"Press Ctrl-Break to terminate program",13,10,13,10,"$"

BackwardsMsg	DB	13,"Timestamp went backwards: 0x"
Backwards1	DB	"xxxxxxxxxxxx, 0x"
Backwards2	DB	"xxxxxxxxxxxx, then 0x"
Backwards3	DB	"xxxxxxxxxxxx",13,10,13,10,"$"

HexBuffer	DB	"xxxxxxxxxxxx"

TimeL		DW	0		; Time loword (CTC count)
TimeM		DW	0		; Time midword (loword of tick count)
TimeH		DW	0		; Time hiword (hiword of tick count)
Time1L		DW	0		; Old time loword
Time1M		DW	0		; Old time midword
Time1H		DW	0		; Old time rock 'n' roll
Time2L		DW	0		; Old old time loword
Time2M		DW	0		; Old old time midword
Time2H		DW	0		; Old old time hiword
RegenSeg	DW	0B800h		; Regen buffer segment
BotLine		DW	0		; Regen offset of bottom line

Main2		PROC	near
		cld
		xor	ax,ax		; Zero
		mov	es,ax
		cmp	WORD PTR es:[463h],3D4h ; Check for colour mode
		je	GotRegenSeg
		mov	[RegenSeg],word 0B000h
GotRegenSeg:	xchg	ax,bx		; BX = 0 (page number)
		mov	ah,3
		int	10h		; Get cursor position - DH = line
		mov	ah,0Fh
		int	10h		; Get video mode
		mov	al,ah		; Get screen width
		mul	dh		; Calculate offset of bottom line
		shl	ax,1		; Shift for char/attrib
		mov	[BotLine],ax	; Store
		mov	dx,OFFSET InitialMsg
		mov	ah,9
		int	21h		; Display initial message

		nop
		;call	InitCTC0Mode2	; Init CTC chan 0 to mode 2, reload 0

MainLoop:
		mov	ax,[Time1L]	; Copy Time1 to Time2
		mov	[Time2L],ax
		mov	ax,[Time1M]
		mov [Time2M],ax
		mov	ax,[Time1H]
		mov	[Time2H],ax

		mov	ax,[TimeL]	; Copy Time to Time1
		mov	[Time1L],ax
		mov	ax,[TimeM]
		mov	[Time1M],ax
		mov	ax,[TimeH]
		mov	[Time1H],ax

		; call timestamp tsr
		int 0x7D

		mov	[TimeL],ax	; Store it
		mov	[TimeM],dx
		mov	[TimeH],bx

		sub	ax,[Time1L]	; Subtract lowords
		sbb	dx,[Time1M]	; Subtract midwords with borrow
		sbb	bx,[Time1H]	; Subtract hiwords with borrow
		jnb	Alright		; If no borrow, time didn't go backwards

		mov	si,OFFSET Time2L ; Oldest time
		mov	di,OFFSET Backwards1 ; First string position
		call	ToASCII		; Convert to ASCII
		mov	si,OFFSET Time1L ; Second-oldest time
		mov	di,OFFSET Backwards2 ; Second string position
		call	ToASCII		; Convert to ASCII
		mov	si,OFFSET TimeL	; New time
		mov	di,OFFSET Backwards3 ; Third string position
		call	ToASCII		; Convert to ASCII

		mov	dx,OFFSET BackwardsMsg
		mov	ah,9
		int	21h		; Display went-backwards message

Alright:	mov	si,OFFSET TimeL	; New time
		mov	di,OFFSET HexBuffer ; Hex text buffer
		call	ToASCII		; Convert to ASCII
		mov	si,OFFSET HexBuffer ; ASCII hex text
		mov	di,[BotLine]	; Offset of bottom line of screen
		mov	es,[RegenSeg]	; Regen buffer segment
		mov	cx,12		; Characters to copy
ScrLoop:	movsb			; Copy character
		inc	di		; Skip attribute
		loop	ScrLoop		; Loop

		xor	ax,ax
		mov	es,ax
		xchg	al,BYTE PTR es:[471h]
		test	al,al
		js	Finish

		jmp	MainLoop

Finish:		mov	ax,4C00h
		int	21h		; Terminate with errorlevel 0
		int	20h		; In case DOS-1 (!)
Main2		ENDP

InitCTC0Mode2	PROC	near
;				Func:	Initialise CTC channel 0 to operate in
;					mode 2 with reload value of 0 (divisor
;					of 65536, 18.2065 interrupts/second).
;					Wait for a tick to occur before setting
;					mode (should minimise disturbance to
;					system time).
;				In:	None
;				Out:	None
;				Lost:	AX (preserves interrupt flag)
		pushf
		push	ds
		sti			; Ensure interrupts are enabled
		xor	ax,ax
		mov	ds,ax		; Address low memory with DS
		mov	ax,ds:[46Ch]	; Get loword of tick count
WaitTick:	cmp	ax,ds:[46Ch]	; Changed?
		je	WaitTick	; If not, loop
		pop	ds
		mov	al,00110100b	; Channel 0, mode 2
		cli
		out	43h,al		; Set mode
		xor	ax,ax		; Zero
		jmp	SHORT $+2	; Delay
		out	40h,al		; Loword of divisor
		jmp	SHORT $+2	; Delay
		out	40h,al		; Hiword of divisor
		popf			; Restore interrupt flag
		ret
InitCTC0Mode2	ENDP

ToASCII		PROC	near
;				Func:	Convert a three-word time structure to
;					12-digit printable hex representation
;				In:	SI -> Structure
;					DI -> ASCII buffer in this segment
;				Out:	DI -> Past characters stored
;				Lost:	AX DI ES
		push	cs
		pop	es		; ES to ComFile
		mov	ax,ds:[si+4]	; Get hiword
		call	Mach16ToHexAsc	; Convert to hex ASCII representation
		mov	ax,ds:[si+2]	; Get hiword
		call	Mach16ToHexAsc	; Convert to hex ASCII representation
		mov	ax,ds:[si+0]	; Get hiword
Mach16ToHexAsc	PROC	near
		push	ax
		mov	al,ah
		call	Mach8ToHexAsc
		pop	ax
Mach8ToHexAsc	PROC	near
		push	ax
		shr	al,1
		shr	al,1
		shr	al,1
		shr	al,1
		call	Mach4ToHexAsc
		pop	ax
		and	al,0Fh
Mach4ToHexAsc	PROC	near
		add	al,90h
		daa
		adc	al,40h
		daa
		stosb
		ret
Mach4ToHexAsc	ENDP
Mach8ToHexAsc	ENDP
Mach16ToHexAsc	ENDP
ToASCII		ENDP

ComFile		ENDS
		END	Main