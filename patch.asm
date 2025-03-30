;
; Title:	BBC Basic for BSX
; Author:	Dean Belfield
; Created:	16/05/2020
; Last Updated:	08/10/2020
;
; Modinfo:
; 20/05/2020:	Added minimal OSCLI command (*BYE)
;		Uses RST instructions in latest ROM for I/O
; 10/06/2020:	Fixed COLOUR to use EXPRI instead of ITEMI so that calculations work without brackets, i.e. COLOUR 1+128
; 08/10/2020:	Minor mods to support UART
;
			
			.MODULE PATCH

; CLRSCN: clears the screen.
;
CLRSCN:		CALL	TELL
			.BYTE	0x1B
                        .BYTE   0x5B
                        .ASCII  "2J"
                        .BYTE   0x1B
                        .BYTE   0x5B
                        .ASCII  "1;1H"
                        .BYTE   0
			RET



; GETIME: return current time in DE:HL, in centiseconds.
;
GETIME:		LD	DE, #0
 	  		LD	HL, #0
			RET

; PUTCSR: move to cursor to x=DE, y=HL
;
PUTCSR:		RET


; OSRDCH: read a character in from the keyboard (non-blocking)
;
OSRDCH:		JP	SYSNBCIN	; using its return
				

; PROMPT: output the input prompt
;
PROMPT: 		LD	A,#']'

; OSWRCH: write a character out to the serial port
;
OSWRCH:		JP	SYSCOUT

;OSKEY - Read key with time-limit, test for ESCape.
;Main function is carried out in user patch.
;   Inputs: HL = time limit (centiseconds)
;  Outputs: Carry reset if time-out
;           If carry set A = character
; Destroys: A,H,L,F
;
OSKEY:			DEC	HL 
			LD	A,H
			OR	L
			RET	Z 
			CALL	SYSNBCIN              ; get key.
			JR	NC,OSKEY        ; C set if there was a key returned	
			CP	#0x1B		; ESC
			SCF 
			RET	NZ
                        ; if it was escape...
ESCSET: 		PUSH    HL
        		LD      HL,#FLAGS
        		BIT     6,(HL)          ; ESC DISABLED?
        		JR      NZ,ESCDIS
        		SET     7,(HL)          ; SET ESCAPE FLAG
ESCDIS: 		POP     HL
        		RET	
                        
ESCTEST:		CALL	SYSNBCIN               ; get key.
			RET	NC		; if there was no key, return
			CP	#0x1B		; ESC	
			JR	Z,ESCSET        ; escape was pushed.
			RET

TRAP:			CALL ESCTEST
LTRAP:			LD A,(FLAGS)
			OR A
			RET P
			LD HL,#FLAGS 
			RES 7,(HL)         ; clear escape flag
			JP ESCAPE

;OSINIT - Initialise RAM mapping etc.
;If BASIC is entered by BBCBASIC FILENAME then file
;FILENAME.BBC is automatically CHAINed.
;   Outputs: DE = initial value of HIMEM (top of RAM)
;            HL = initial value of PAGE (user program)
;            Z-flag reset indicates AUTO-RUN.
;  Destroys: A,D,E,H,L,F
;
OSINIT: 		XOR	A
			LD	(FLAGS),A	;Clear flags
         		LD 	DE,#0x0000	;DE = HIMEM
         		LD 	E,A             ;PAGE BOUNDARY
         		LD 	HL,#USER
         		RET	

;
;OSLINE - Read/edit a complete line, terminated by CR.
;   Inputs: HL addresses destination buffer.
;           (L=0)
;  Outputs: Buffer filled, terminated by CR.
;           A=0.
; Destroys: A,B,C,D,E,H,L,F
;
OSLINE:		CALL	SYSNBCIN               ; get key.
			OR	A 
			JR	Z,OSLINE
                        
			CP	#0x0D		; CR
			JR	Z,KEYCR		
                        
			CP	#0x7F		; Backspace
			JR	Z,KEYBS
                        
			LD	(HL),A		; Save the character in the buffer
			INC	HL
			CALL	OSWRCH		; Echo character back to terminal
			JR	OSLINE		; Loop

KEYCR:			LD	(HL),A		; Write final CR
			CALL	CRLF		; Print CR
			AND	A
			RET 

KEYBS:			INC	L		; Check for beginning of line
			DEC	L 
			JR	Z,OSLINE
			CALL	OSWRCH          ; write the BS
			DEC	L
			JR	OSLINE


;
;OSCLI - Process an "operating system" command
;
OSCLI: 		CALL    SKIPSP
			CP      #CR
			RET     Z
			CP      #'|'
			RET     Z			
			EX      DE,HL
			LD      HL,#COMDS
OSCLI0:			LD      A,(DE)
			CALL    UPPRC
			CP      (HL)
			JR      Z,OSCLI2
			JR      C,HUH
OSCLI1:			BIT     7,(HL)
			INC     HL
			JR      Z,OSCLI1
			INC     HL
			INC     HL
			JR      OSCLI0
;
OSCLI2:			PUSH    DE
OSCLI3:			INC     DE
			INC     HL
			LD      A,(DE)
			CALL    UPPRC
			CP      #'.'		; ABBREVIATED?
			JR      Z,OSCLI4
			XOR     (HL)
			JR      Z,OSCLI3
			CP      #0x80
			JR      Z,OSCLI4
			POP     DE
			JR      OSCLI1
;
OSCLI4:			POP     AF
		        INC     DE
OSCLI5:			BIT     7,(HL)
			INC     HL
			JR      Z,OSCLI5
			LD      A,(HL)
			INC     HL
			LD      H,(HL)
			LD      L,A
			PUSH    HL
			EX      DE,HL
			JP      SKIPSP

HUH:    		LD      A,#254
        		CALL    EXTERR
        		.ASCII   'Bad command'
        		.BYTE    0			

SKIPSP:			LD      A,(HL)
        		CP      #' '
        		RET     NZ
        		INC     HL
        		JR      SKIPSP	

UPPRC:  		AND     #0x7F
			CP      #'`'
			RET     C
			AND     #0x5F		; CONVERT TO UPPER CASE
			RET					

; OSCLI - *BYE
;
BYE:			JP	0





; Each command has bit 7 of the last character set, and is followed by the address of the handler
;
COMDS:  		
                        .ASCIS    	'BYE'	
                          .WORD          BYE	; JP 0
                          
			.BYTE	0xFF

; COLOUR: change text colour
;
COLOUR:		CALL    EXPRI
			CALL 	TELL
			.BYTE 	0x1B
                        .BYTE   0x5B
                        .BYTE   0			
			EXX
			LD 	A,#'3'
			BIT 	7,L 
			JR 	Z,PCOL1
			INC 	A 
PCOL1:			CALL	OSWRCH
			LD	A,L
			AND	#7
			ADD	A,#'0'
			CALL 	OSWRCH
			LD	A,#"m"
			CALL 	OSWRCH
			JP 	XEQ

; Stuff not implemented yet
;
GETCSR:
PUTIME:
OSBPUT:
OSBGET:
OSSTAT:
OSSHUT:
OSOPEN:
OSCALL:
OSSAVE:
OSLOAD:
GETPTR:
PUTPTR:
GETEXT:
RESET:
			RET

			;ENDMODULE
