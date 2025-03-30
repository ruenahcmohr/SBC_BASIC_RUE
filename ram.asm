;   
;RAM MODULE FOR BBC BASIC INTERPRETER
;FOR USE WITH VERSION 2.0 OF BBC BASIC
;*STANDARD CP/M DISTRIBUTION VERSION*
;(C) COPYRIGHT R.T.RUSSELL 31-12-1983
;

			.MODULE RAM

			.ORG RAM_START
;
;n.b. ACCS, BUFFER & STAVAR must be on page boundaries.
;
ACCS:	                .BLKB    256             ;STRING ACCUMULATOR
BUFFER:	                .BLKB    256             ;STRING INPUT BUFFER
STAVAR:	                .BLKB    27*4            ;STATIC VARIABLES
OC	                .EQU     STAVAR+15*4     ;CODE ORIGIN (O%)
PC	                .EQU     STAVAR+16*4     ;PROGRAM COUNTER (P%)
DYNVAR:                 .BLKB    54*2            ;DYN. VARIABLE POINTERS
FNPTR:                  .BLKB    2               ;DYN. FUNCTION POINTER
PROPTR:                 .BLKB    2               ;DYN. PROCEDURE POINTER
;
PAGE:                   .BLKB    2               ;START OF USER PROGRAM
TOP:                    .BLKB    2               ;FIRST LOCN AFTER PROG.
LOMEM:                  .BLKB    2               ;START OF DYN. STORAGE
FREE:                   .BLKB    2               ;FIRST FREE-SPACE BYTE
HIMEM:                  .BLKB    2               ;FIRST PROTECTED BYTE
;
LINENO:                 .BLKB    2               ;LINE NUMBER
TRACEN:	                .BLKB    2               ;TRACE FLAG
AUTONO:	                .BLKB    2               ;AUTO FLAG
ERRTRP:	                .BLKB    2               ;ERROR TRAP
ERRTXT:	                .BLKB    2               ;ERROR MESSAGE POINTER
DATPTR:	                .BLKB    2               ;DATA POINTER
ERL:	                .BLKB    2               ;ERROR LINE
ERRLIN:	                .BLKB    2               ;"ON ERROR" LINE
RANDOM:	                .BLKB    5               ;RANDOM NUMBER
COUNT:	                .BLKB    1               ;PRINT POSITION
WIDTH:	                .BLKB    1               ;PRINT WIDTH
ERR:	                .BLKB    1               ;ERROR NUMBER
LISTON:	                .BLKB    1               ;LISTO & OPT FLAG
INCREM:	                .BLKB    1               ;AUTO INCREMENT
;
; Added by me
;
FLAGS:			.BLKB	1		; Flags: B7=ESC PRESSED, B6=ESC DISABLED

;			ENDMODULE
