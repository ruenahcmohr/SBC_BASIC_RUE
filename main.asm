;
;BBC BASIC INTERPRETER - Z80 VERSION
;COMMAND, ERROR AND LEXICAL ANALYSIS MODULE - "MAIN"
;(C) COPYRIGHT  R.T.RUSSELL  1984
;VERSION 2.3, 07-05-1984
;VERSION 3.0, 01-03-1987
;
			.MODULE MAIN
;
TERROR			.EQU      0x85
LINE			.EQU      0x86
ELSE			.EQU      0x8B
THEN			.EQU      0x8C
LINO			.EQU      0x8D
FN			.EQU      0xA4
TO			.EQU      0xB8
REN			.EQU      0xCC
DATA			.EQU      0xDC
DIM			.EQU      0xDE
FOR			.EQU      0xE3
GOSUB			.EQU      0xE4
GOTO			.EQU      0xE5
TIF			.EQU      0xE7
LOCAL			.EQU      0xEA
NEXT			.EQU      0xED
ON			.EQU      0xEE
PROC			.EQU      0xF2
REM			.EQU      0xF4
REPEAT			.EQU      0xF5
RESTOR			.EQU      0xF7
TRACE			.EQU      0xFC
UNTIL			.EQU      0xFD
;
TOKLO			.EQU      0x8F
TOKHI			.EQU      0x93
OFFSET			.EQU      0xCF-TOKLO

;START:			JP      COLD
;			JP      WARM
;			JP      ESCAPE
;			JP      EXTERR
;			JP      TELL
;			JP      TEXT
;			JP      SETTOP
;			JP      CLEAR
;			JP      RUN0
;			JP      OSCLI
;			JP      OSBGET
;			JP      OSBPUT
;			JP      OSSTAT
;			JP      OSSHUT

COLD:			LD      HL,#STAVAR       ;COLD START
			LD      SP,HL
			LD      (HL),#10
			INC     L
			LD      (HL),#9
			INC     L
			XOR     A
PURGE:			LD      (HL),A          ;CLEAR SCRATCHPAD
			INC     L
			JR      NZ,PURGE
			LD      A,#0x37           ;V3.0
			LD      (LISTON),A
			LD      HL,#NOTICE
			LD      (ERRTXT),HL
			CALL    OSINIT
			LD      (HIMEM),DE
			LD      (PAGE),HL
			CALL    NEWIT
			JP      NZ,CHAIN0       ;AUTO-RUN
			CALL    TELL
			.ascii  "BBC BASIC (Z80) Version 3.00\n\r"
NOTICE:			.ascii  "(C) Copyright R.T.Russell 1987\n\r"
			.ascii	"\n\r"
			.BYTE    0
WARM:			.BYTE     0xF6
CLOOP:			SCF
			LD      SP,(HIMEM)
			CALL    PROMPT          ;PROMPT USER
			LD      HL,#LISTON
			LD      A,(HL)
			AND     #0x0F             ;LISTO
			OR      #0x30             ;OPT 3
			LD      (HL),A
			SBC     HL,HL           ;HL <- 0 (V3.0)
			LD      (ERRTRP),HL
			LD      (ERRLIN),HL
			LD      HL,(AUTONO)
			LD      (LINENO),HL
			LD      A,H
			OR      L
			JR      Z,NOAUTO
			PUSH    HL
			CALL    PBCD            ;AUTO NUMBER
			POP     HL
			LD      BC,(INCREM)
			LD      B,#0
			ADD     HL,BC
			JP      C,MTOOBIG
			LD      (AUTONO),HL
			LD      A,#' '
			CALL    OUTCHR
NOAUTO:			LD      HL,#ACCS
			CALL    OSLINE          ;GET CONSOLE INPUT
			XOR     A
			LD      (COUNT),A
			LD      IY,#ACCS
			CALL    LINNUM
			CALL    NXT
			LD      A,H
			OR      L
			JR      Z,LNZERO        ;FOR AUTO (NON-MAPPED)
			LD      (LINENO),HL
LNZERO:			LD      DE,#BUFFER
			LD      C,#1             ;LEFT MODE
			CALL    LEXAN2          ;LEXICAL ANALYSIS
			LD      (DE),A          ;TERMINATOR
			XOR     A
			LD      B,A
			LD      C,E             ;BC=LINE LENGTH
			INC     DE
			LD      (DE),A          ;ZERO NEXT
			LD      HL,(LINENO)
			LD      A,H
			OR      L
			LD      IY,#BUFFER       ;FOR XEQ
			JP      Z,XEQ           ;DIRECT MODE
			PUSH    BC
			PUSH    HL
			CALL    SETTOP          ;SET TOP
			POP     HL
			CALL    FINDL
			CALL    Z,DEL
			POP     BC
			LD      A,C
			OR      A
			JP      Z,CLOOP         ;DELETE LINE ONLY
			ADD     A,#4
			LD      C,A             ;LENGTH INCLUSIVE
			PUSH    DE              ;LINE NUMBER
			PUSH    BC              ;SAVE LINE LENGTH
			EX      DE,HL
			LD      HL,(TOP)
			PUSH    HL
			ADD     HL,BC
			PUSH    HL
			INC     H
			XOR     A
			SBC     HL,SP
			POP     HL
			JP      NC,ERROR        ;"No room"
			LD      (TOP),HL
			EX      (SP),HL
			PUSH    HL
			INC     HL
			OR      A
			SBC     HL,DE
			LD      B,H             ;BC=AMOUNT TO MOVE
			LD      C,L
			POP     HL
			POP     DE
			JR      Z,ATEND
			LDDR                    ;MAKE SPACE
ATEND:			POP     BC              ;LINE LENGTH
			POP     DE              ;LINE NUMBER
			INC     HL
			LD      (HL),C          ;STORE LENGTH
			INC     HL
			LD      (HL),E          ;STORE LINE NUMBER
			INC     HL
			LD      (HL),D
			INC     HL
			LD      DE,#BUFFER
			EX      DE,HL
			DEC     C
			DEC     C
			DEC     C
			LDIR                    ;ADD LINE
			CALL    CLEAN
			JP      CLOOP	
;
;LIST OF TOKENS AND KEYWORDS.
;IF A KEYWORD IS FOLLOWED BY NUL THEN IT WILL
; ONLY MATCH WITH THE WORD FOLLOWED IMMEDIATELY
; BY A DELIMITER.
;
KEYWDS:			.BYTE    0x80
			.ASCII  'AND'
			.BYTE     0x94
			.ASCII  'ABS'
			.BYTE     0x95
			.ASCII  'ACS'
			.BYTE     0x96
			.ASCII  'ADVAL'
			.BYTE     0x97
			.ASCII  'ASC'
			.BYTE     0x98
			.ASCII  'ASN'
			.BYTE     0x99
			.ASCII  'ATN'
			.BYTE     0xC6
			.ASCII  'AUTO'
			.BYTE     0x9A
			.ASCII  'BGET'
			.BYTE    0
			.BYTE     0xD5
			.ASCII  'BPUT'
			.BYTE    0
			.BYTE     0xFB
			.ASCII  'COLOUR'
			.BYTE     0xFB
			.ASCII  'COLOR'
			.BYTE     0xD6
			.ASCII  'CALL'
			.BYTE     0xD7
			.ASCII  'CHAIN'
			.BYTE     0xBD
			.ASCII  'CHR$'
			.BYTE     0xD8
			.ASCII  'CLEAR'
			.BYTE    0
			.BYTE     0xD9
			.ASCII  'CLOSE'
			.BYTE    0
			.BYTE     0xDA
			.ASCII  'CLG'
			.BYTE    0
			.BYTE     0xDB
			.ASCII  'CLS'
			.BYTE    0
			.BYTE     0x9B
			.ASCII  'COS'
			.BYTE     0x9C
			.ASCII  'COUNT'
			.BYTE    0
			.BYTE     0xDC
			.ASCII  'DATA'
			.BYTE     0x9D
			.ASCII  'DEG'
			.BYTE     0xDD
			.ASCII  'DEF'
			.BYTE     0xC7
			.ASCII  'DELETE'
			.BYTE     0x81
			.ASCII  'DIV'
			.BYTE     0xDE
			.ASCII  'DIM'
			.BYTE     0xDF
			.ASCII  'DRAW'
			.BYTE     0xE1
			.ASCII  'ENDPROC'
			.BYTE    0
			.BYTE     0xE0
			.ASCII  'END'
			.BYTE    0
			.BYTE     0xE2
			.ASCII  'ENVELOPE'
			.BYTE     0x8B
			.ASCII  'ELSE'
			.BYTE     0xA0
			.ASCII  'EVAL'
			.BYTE     0x9E
			.ASCII  'ERL'
			.BYTE    0
			.BYTE     0x85
			.ASCII  'ERROR'
			.BYTE     0xC5
			.ASCII  'EOF'
			.BYTE    0
			.BYTE     0x82
			.ASCII  'EOR'
			.BYTE     0x9F
			.ASCII  'ERR'
			.BYTE    0
			.BYTE     0xA1
			.ASCII  'EXP'
			.BYTE     0xA2
			.ASCII  'EXT'
			.BYTE    0
			.BYTE     0xE3
			.ASCII  'FOR'
			.BYTE     0xA3
			.ASCII  'FALSE'
			.BYTE    0
			.BYTE     0xA4
			.ASCII  'FN'
			.BYTE     0xE5
			.ASCII  'GOTO'
			.BYTE     0xBE
			.ASCII  'GET$'
			.BYTE     0xA5
			.ASCII  'GET'
			.BYTE     0xE4
			.ASCII  'GOSUB'
			.BYTE     0xE6
			.ASCII  'GCOL'
			.BYTE     0x93
			.ASCII  'HIMEM'
			.BYTE    0
			.BYTE     0xE8
			.ASCII  'INPUT'
			.BYTE     0xE7
			.ASCII  'IF'
			.BYTE     0xBF
			.ASCII  'INKEY$'
			.BYTE     0xA6
			.ASCII  'INKEY'
			.BYTE     0xA8
			.ASCII  'INT'
			.BYTE     0xA7
			.ASCII  'INSTR('
			.BYTE     0xC9
			.ASCII  'LIST'
			.BYTE     0x86
			.ASCII  'LINE'
			.BYTE     0xC8
			.ASCII  'LOAD'
			.BYTE     0x92
			.ASCII  'LOMEM'
			.BYTE    0
			.BYTE     0xEA
			.ASCII  'LOCAL'
			.BYTE     0xC0
			.ASCII  'LEFT$('
			.BYTE     0xA9
			.ASCII  'LEN'
			.BYTE     0xE9
			.ASCII  'LET'
			.BYTE     0xAB
			.ASCII  'LOG'
			.BYTE     0xAA
			.ASCII  'LN'
			.BYTE     0xC1
			.ASCII  'MID$('
			.BYTE     0xEB
			.ASCII  'MODE'
			.BYTE     0x83
			.ASCII  'MOD'
			.BYTE     0xEC
			.ASCII  'MOVE'
			.BYTE     0xED
			.ASCII  'NEXT'
			.BYTE     0xCA
			.ASCII  'NEW'
			.BYTE    0
			.BYTE     0xAC
			.ASCII  'NOT'
			.BYTE     0xCB
			.ASCII  'OLD'
			.BYTE    0
			.BYTE     0xEE
			.ASCII  'ON'
			.BYTE     0x87
			.ASCII  'OFF'
			.BYTE     0x84
			.ASCII  'OR'
			.BYTE     0x8E
			.ASCII  'OPENIN'
			.BYTE     0xAE
			.ASCII  'OPENOUT'
			.BYTE     0xAD
			.ASCII  'OPENUP'
			.BYTE     0xFF
			.ASCII  'OSCLI'
			.BYTE     0xF1
			.ASCII  'PRINT'
			.BYTE     0x90
			.ASCII  'PAGE'
			.BYTE    0
			.BYTE     0x8F
			.ASCII  'PTR'
			.BYTE    0
			.BYTE     0xAF
			.ASCII  'PI'
			.BYTE    0
			.BYTE     0xF0
			.ASCII  'PLOT'
			.BYTE     0xB0
			.ASCII  'POINT('
			.BYTE     0xF2
			.ASCII  'PROC'
			.BYTE     0xB1
			.ASCII  'POS'
			.BYTE    0
			.BYTE     0xCE
			.ASCII  'PUT'
			.BYTE     0xF8
			.ASCII  'RETURN'
			.BYTE    0
			.BYTE     0xF5
			.ASCII  'REPEAT'
			.BYTE     0xF6
			.ASCII  'REPORT'
			.BYTE    0
			.BYTE     0xF3
			.ASCII  'READ'
			.BYTE     0xF4
			.ASCII  'REM'
			.BYTE     0xF9
			.ASCII  'RUN'
			.BYTE    0
			.BYTE     0xB2
			.ASCII  'RAD'
			.BYTE     0xF7
			.ASCII  'RESTORE'
			.BYTE     0xC2
			.ASCII  'RIGHT$('
			.BYTE     0xB3
			.ASCII  'RND'
			.BYTE    0
			.BYTE     0xCC
			.ASCII  'RENUMBER'
			.BYTE     0x88
			.ASCII  'STEP'
			.BYTE     0xCD
			.ASCII  'SAVE'
			.BYTE     0xB4
			.ASCII  'SGN'
			.BYTE     0xB5
			.ASCII  'SIN'
			.BYTE     0xB6
			.ASCII  'SQR'
			.BYTE     0x89
			.ASCII  'SPC'
			.BYTE     0xC3
			.ASCII  'STR$'
			.BYTE     0xC4
			.ASCII  'STRING$('
			.BYTE     0xD4
			.ASCII  'SOUND'
			.BYTE     0xFA
			.ASCII  'STOP'
			.BYTE    0
			.BYTE     0xB7
			.ASCII  'TAN'
			.BYTE     0x8C
			.ASCII  'THEN'
			.BYTE     0xB8
			.ASCII  'TO'
			.BYTE     0x8A
			.ASCII  'TAB('
			.BYTE     0xFC
			.ASCII  'TRACE'
			.BYTE     0x91
			.ASCII  'TIME'
			.BYTE    0
			.BYTE     0xB9
			.ASCII  'TRUE'
			.BYTE    0
			.BYTE     0xFD
			.ASCII  'UNTIL'
			.BYTE     0xBA
			.ASCII  'USR'
			.BYTE     0xEF
			.ASCII  'VDU'
			.BYTE     0xBB
			.ASCII  'VAL'
			.BYTE     0xBC
			.ASCII  'VPOS'
			.BYTE    0
			.BYTE     0xFE
			.ASCII  'WIDTH'
			.BYTE     0xD3
			.ASCII  'HIMEM'
			.BYTE     0xD2
			.ASCII  'LOMEM'
			.BYTE     0xD0
			.ASCII  'PAGE'
			.BYTE     0xCF
			.ASCII  'PTR'
			.BYTE     0xD1
			.ASCII  'TIME'
			.BYTE    1
			.ASCII  'Missing '
			.BYTE    2
			.ASCII  'No such '
			.BYTE    3
			.ASCII  'Bad '
			.BYTE    4
			.ASCII  ' range'
			.BYTE    5
			.ASCII  'variable'
			.BYTE    6
			.ASCII  'Out of'
			.BYTE    7
			.ASCII  'No '
			.BYTE    8
			.ASCII  ' space'
KEYWDL			.EQU     .-KEYWDS
			.WORD    -1
;
;ERROR MESSAGES:
;
ERRWDS:			.BYTE    7
			.ASCII  'room'
			.BYTE    0
			.BYTE    6
			.BYTE    4
			.BYTE    0
			.WORD    0
			.ASCII  'Mistake'
			.BYTE    0
			.BYTE    1
			.ASCII  ','
			.BYTE    0
			.ASCII  'Type mismatch'
			.BYTE    0
			.BYTE    7
			.BYTE    FN
			.WORD    0
			.BYTE    1
			.BYTE   34		;ASCII ""
			.BYTE    0
			.BYTE    3
			.BYTE    DIM
			.BYTE    0
			.BYTE    DIM
			.BYTE    8
			.BYTE    0
			.ASCII  'Not '
			.BYTE    LOCAL
			.BYTE    0
			.BYTE    7
			.BYTE    PROC
			.BYTE    0
			.ASCII  'Array'
			.BYTE    0
			.ASCII  'Subscript'
			.BYTE    0
			.ASCII  'Syntax error'
			.BYTE    0
			.ASCII  'Escape'
			.BYTE    0
			.ASCII  'Division by zero'
			.BYTE    0
			.ASCII  'String too long'
			.BYTE    0
			.ASCII  'Too big'
			.BYTE    0
			.ASCII  '-ve root'
			.BYTE    0
			.ASCII  'Log'
			.BYTE    4
			.BYTE    0
			.ASCII  'Accuracy lost'
			.BYTE    0
			.ASCII  'Exp'
			.BYTE    4
			.WORD    0
			.BYTE    2
			.BYTE    5
			.BYTE    0
			.BYTE    1
			.ASCII  ')'
			.BYTE    0
			.BYTE    3
			.ASCII  'HEX'
			.BYTE    0
			.BYTE    2
			.BYTE    FN
			.ASCII  '/'
			.BYTE    PROC
			.BYTE    0
			.BYTE    3
			.ASCII  'call'
			.BYTE    0
			.ASCII  'Arguments'
			.BYTE    0
			.BYTE    7
			.BYTE    FOR
			.BYTE    0
			.ASCII  "Can't match "
			.BYTE    FOR
			.BYTE    0
			.BYTE    FOR
			.ASCII  ' '
			.BYTE    5
			.WORD    0
			.BYTE    7
			.BYTE    TO
			.WORD    0
			.BYTE    7
			.BYTE    GOSUB
			.BYTE    0
			.BYTE    ON
			.ASCII  ' syntax'
			.BYTE    0
			.BYTE    ON
			.BYTE    4
			.BYTE    0
			.BYTE    2
			.ASCII  'line'
			.BYTE    0
			.BYTE    6
			.ASCII  ' '
			.BYTE    DATA
			.BYTE    0
			.BYTE    7
			.BYTE    REPEAT
			.WORD    0
			.BYTE    1
			.ASCII  '#'
			.BYTE    0
;
;COMMANDS:
;
;DELETE line,line
;
DELETE:		CALL    SETTOP          ;SET TOP
			CALL    DLPAIR
DELET1:			LD      A,(HL)
			OR      A
			JR      Z,WARMNC
			INC     HL
			LD      E,(HL)
			INC     HL
			LD      D,(HL)
			LD      A,D
			OR      E
			JR      Z,CLOOP1        ;LINE NUMBER ZERO
			DEC     HL
			DEC     HL
			EX      DE,HL
			SCF
			SBC     HL,BC
			EX      DE,HL
			JR      NC,WARMNC
			PUSH    BC
			CALL    DEL
			POP     BC
			JR      DELET1
;
;LISTO expr
;
LISTO:			INC     IY              ;SKIP "O"
			CALL    EXPRI
			EXX
			LD      A,L
			LD      (LISTON),A
CLOOP1:			JP      CLOOP
;
;LIST
;LIST line
;LIST line,line [IF string]
;LIST ,line
;LIST line,
;
LIST:			CP      #'O'
			JR      Z,LISTO
			CALL    DLPAIR
			CALL    NXT
			CP      #TIF             ;IF CLAUSE ?
			LD      A,#0             ;INIT IF-CLAUSE LENGTH
			JR      NZ,LISTB
			INC     IY              ;SKIP IF
			CALL    NXT             ;SKIP SPACES (IF ANY)
			EX      DE,HL
			PUSH    IY
			POP     HL              ;HL ADDRESSES IF CLAUSE
			LD      A,#CR
			PUSH    BC
			LD      BC,#256
			CPIR                    ;LOCATE CR
			LD      A,C
			CPL                     ;A = SUBSTRING LENGTH
			POP     BC
			EX      DE,HL
LISTB:			LD      E,A             ;IF-CLAUSE LENGTH
			LD      A,B
			OR      C
			JR      NZ,LISTA
			DEC     BC
LISTA:			EXX
			LD      IX,#LISTON
			LD      BC,#0            ;INDENTATION COUNT
			EXX
			LD      A,#20
;
LISTC:			PUSH    BC              ;SAVE HIGH LINE NUMBER
			PUSH    DE              ;SAVE IF-CLAUSE LENGTH
			PUSH    HL              ;SAVE PROGRAM POINTER
			EX      AF,AF'
			LD      A,(HL)
			OR      A
			JR      Z,WARMNC
;
;CHECK IF PAST TERMINATING LINE NUMBER:
;
			LD      A,E             ;A = IF-CLAUSE LENGTH
			INC     HL
			LD      E,(HL)
			INC     HL
			LD      D,(HL)          ;DE = LINE NUMBER
			DEC     HL
			DEC     HL
			PUSH    DE              ;SAVE LINE NUMBER
			EX      DE,HL
			SCF
			SBC     HL,BC
			EX      DE,HL
			POP     DE              ;RESTORE LINE NUMBER
WARMNC:			JP      NC,WARM
			LD      C,(HL)          ;C = LINE LENGTH + 4
			LD      B,A             ;B = IF-CLAUSE LENGTH
;
;CHECK IF "UNLISTABLE":
;
			LD      A,D
			OR      E
			JP      Z,CLOOP
;
;CHECK FOR IF CLAUSE:
;
			INC     HL
			INC     HL
			INC     HL              ;HL ADDRESSES LINE TEXT
			DEC     C
			DEC     C
			DEC     C
			DEC     C               ;C = LINE LENGTH
			PUSH    DE              ;SAVE LINE NUMBER
			PUSH    HL              ;SAVE LINE ADDRESS
			XOR     A               ;A <- 0
			CP      B               ;WAS THERE AN IF-CLAUSE
			PUSH    IY
			POP     DE              ;DE ADDRESSES IF-CLAUSE
			CALL    NZ,SEARCH       ;SEARCH FOR IF CLAUSE
			POP     HL              ;RESTORE LINE ADDRESS
			POP     DE              ;RESTORE LINE NUMBER
			PUSH    IY
			CALL    Z,LISTIT        ;LIST IF MATCH
			POP     IY
;
			EX      AF,AF'
			DEC     A
			CALL    LTRAP
			POP     HL              ;RESTORE POINTER
			LD      E,(HL)
			LD      D,#0
			ADD     HL,DE           ;ADDRESS NEXT LINE
			POP     DE              ;RESTORE IF-CLAUSE LEN
			POP     BC              ;RESTORE HI LINE NUMBER
			JR      LISTC
;
;RENUMBER
;RENUMBER start
;RENUMBER start,increment
;RENUMBER ,increment
;
RENUM:			CALL    CLEAR           ;USES DYNAMIC AREA
			CALL    PAIR            ;LOAD HL,BC
			EXX
			LD      HL,(PAGE)
			LD      DE,(LOMEM)
RENUM1:			LD      A,(HL)          ;BUILD TABLE
			OR      A
			JR      Z,RENUM2
			INC     HL
			LD      C,(HL)          ;OLD LINE NUMBER
			INC     HL
			LD      B,(HL)
			LD      A,B
			OR      C
			JP      Z,CLOOP         ;LINE NUMBER ZERO
			EX      DE,HL
			LD      (HL),C
			INC     HL
			LD      (HL),B
			INC     HL
			EXX
			PUSH    HL
			ADD     HL,BC           ;ADD INCREMENT
			JP      C,MTOOBIG        ;"Too big"
			EXX
			POP     BC
			LD      (HL),C
			INC     HL
			LD      (HL),B
			INC     HL
			EX      DE,HL
			DEC     HL
			DEC     HL
			XOR     A
			LD      B,A
			LD      C,(HL)
			ADD     HL,BC           ;NEXT LINE
			EX      DE,HL
			PUSH    HL
			INC     H
			SBC     HL,SP
			POP     HL
			EX      DE,HL
			JR      C,RENUM1        ;CONTINUE
			CALL    EXTERR          ;"RENUMBER space'
			.BYTE    REN
			.BYTE    8
			.BYTE    0
;
RENUM2:			EX      DE,HL
			LD      (HL),#-1
			INC     HL
			LD      (HL),#-1
			LD      DE,(LOMEM)
			EXX
			LD      HL,(PAGE)
RENUM3:			LD      C,(HL)
			LD      A,C
			OR      A
			JP      Z,WARM
			EXX
			EX      DE,HL
			INC     HL
			INC     HL
			LD      E,(HL)
			INC     HL
			LD      D,(HL)
			INC     HL
			PUSH    DE
			EX      DE,HL
			LD      (LINENO),HL
			EXX
			POP     DE
			INC     HL
			LD      (HL),E          ;NEW LINE NUMBER
			INC     HL
			LD      (HL),D
			INC     HL
			DEC     C
			DEC     C
			DEC     C
			LD      B,#0
RENUM7:			LD      A,#LINO
			CPIR                    ;SEARCH FOR LINE NUMBER
			JR      NZ,RENUM3
			PUSH    BC
			PUSH    HL
			PUSH    HL
			POP     IY
			EXX
			CALL    DECODE          ;DECODE LINE NUMBER
			EXX
			LD      B,H
			LD      C,L
			LD      HL,(LOMEM)
RENUM4:			LD      E,(HL)          ;CROSS-REFERENCE TABLE
			INC     HL
			LD      D,(HL)
			INC     HL
			EX      DE,HL
			OR      A               ;CLEAR CARRY
			SBC     HL,BC
			EX      DE,HL
			LD      E,(HL)          ;NEW NUMBER
			INC     HL
			LD      D,(HL)
			INC     HL
			JR      C,RENUM4
			EX      DE,HL
			JR      Z,RENUM5        ;FOUND
			CALL    TELL
			.ASCII  'Failed at '
			.BYTE    0
			LD      HL,(LINENO)
			CALL    PBCDL
			CALL    CRLF
			JR      RENUM6
RENUM5:			POP     DE
			PUSH    DE
			DEC     DE
			CALL    ENCODE          ;RE-WRITE NUMBER
RENUM6:			POP     HL
			POP     BC
			JR      RENUM7
;
;AUTO
;AUTO start,increment
;AUTO start
;AUTO ,increment
;
AUTO:			CALL    PAIR
			LD      (AUTONO),HL
			LD      A,C
			LD      (INCREM),A
			JR      CLOOP0
;
;BAD
;NEW
;
BAD:			CALL    TELL            ;"Bad program'
			.BYTE    3
			.ASCII  'program'
			.BYTE    CR
			.BYTE    LF
			.BYTE    0
NEW:			CALL    NEWIT
			JR      CLOOP0
;
;OLD
;
OLD:			LD      HL,(PAGE)
			PUSH    HL
			INC     HL
			INC     HL
			INC     HL
			LD      BC,#252
			LD      A,#CR
			CPIR
			JR      NZ,BAD
			LD      A,L
			POP     HL
			LD      (HL),A
			CALL    CLEAN
CLOOP0:			JP      CLOOP
;
;LOAD filename
;
LOAD:			CALL    EXPRS           ;GET FILENAME
			LD      A,#CR
			LD      (DE),A
			CALL    LOAD0
			CALL    CLEAR
			JR      WARM0
;
;SAVE filename
;
SAVE:			CALL    SETTOP          ;SET TOP
			CALL    EXPRS           ;FILENAME
			LD      A,#CR
			LD      (DE),A
			LD      DE,(PAGE)
			LD      HL,(TOP)
			OR      A
			SBC     HL,DE
			LD      B,H             ;LENGTH OF PROGRAM
			LD      C,L
			LD      HL,#ACCS
			CALL    OSSAVE
WARM0:			JP      WARM
;
;ERROR
;
ERROR:			LD      SP,(HIMEM)
			LD      HL,#ERRWDS
			OR      A
			JR      Z,ERROR1
			LD      B,A             ;ERROR NUMBER
			EX      AF,AF'
			XOR     A
ERROR0:			CP      (HL)
			INC     HL
			JR      NZ,ERROR0
			DJNZ    ERROR0
			EX      AF,AF'
ERROR1:			PUSH    HL
EXTERR:		POP     HL
			LD      (ERRTXT),HL
			LD      SP,(HIMEM)
			LD      (ERR),A
			CALL    SETLIN
			LD      (ERL),HL
			OR      A
			JR      Z,ERROR2
			LD      HL,(ERRTRP)
			LD      A,H
			OR      L
			PUSH    HL
			POP     IY
			JP      NZ,XEQ          ;ERROR TRAPPED
ERROR2:			LD      HL,#0
			LD      (AUTONO),HL
			LD      (TRACEN),HL     ;CANCEL TRACE
			CALL    RESET           ;RESET OPSYS
			CALL    CRLF
			CALL    REPORT          ;MESSAGE
			CALL    SAYLN
			LD      E,#0
			CALL    C,OSSHUT        ;CLOSE ALL FILES
			CALL    CRLF
			JP      CLOOP
;
;SUBROUTINES:
;
;
;LEX - SEARCH FOR KEYWORDS
;   Inputs: HL = start of keyword table
;           IY = start of match text
;  Outputs: If found, Z-flag set, A=token.
;           If not found, Z-flag reset, A=(IY).
;           IY updated (if NZ, IY unchanged).
; Destroys: A,B,H,L,IY,F
;
LEX:			LD      HL,#KEYWDS
LEX0:			LD      A,(IY)
			LD      B,(HL)
			INC     HL
			CP      (HL)
			JR      Z,LEX2
			RET     C               ;FAIL EXIT
LEX1:			INC     HL
			BIT     7,(HL)
			JR      Z,LEX1
			JR      LEX0
LEX2:			PUSH    IY              ;SAVE POINTER
LEX3:			INC     HL
			BIT     7,(HL)
			JR      NZ,LEX6         ;FOUND
			INC     IY
			LD      A,(IY)
			CP      #'.'
			JR      Z,LEX6          ;FOUND (ABBREV.)
			CP      (HL)
			JR      Z,LEX3
			CALL    RANGE1
			JR      C,LEX5
LEX4:			POP     IY              ;RESTORE POINTER
			JR      LEX1
LEX5:			LD      A,(HL)
			OR      A
			JR      NZ,LEX4
			DEC     IY
LEX6:			POP     AF
			XOR     A
			LD      A,B
			RET
;
;DEL - DELETE A PROGRAM LINE.
;   Inputs: HL addresses program line.
; Destroys: B,C,F
;
DEL:			PUSH    DE
			PUSH    HL
			PUSH    HL
			LD      B,#0
			LD      C,(HL)
			ADD     HL,BC
			PUSH    HL
			EX      DE,HL
			LD      HL,(TOP)
			SBC     HL,DE
			LD      B,H
			LD      C,L
			POP     HL
			POP     DE
			LDIR                    ;DELETE LINE
			LD      (TOP),DE
			POP     HL
			POP     DE
			RET
;
;LOAD0 - LOAD A DISK FILE THEN CLEAN.
;   Inputs: Filename in ACCS (term CR)
; Destroys: A,B,C,D,E,H,L,F
;
;CLEAN - CHECK FOR BAD PROGRAM, FIND END OF TEXT
; AND WRITE FF FF, THEN LOAD (TOP).
; Destroys: A,B,C,H,L,F
;
LOAD0: 		LD      DE,(PAGE)
			LD      HL,#-256
			ADD     HL,SP
			SBC     HL,DE           ;FIND AVAILABLE SPACE
			LD      B,H
			LD      C,L
			LD      HL,#ACCS
			CALL    OSLOAD          ;LOAD
			CALL    NC,NEWIT
			LD      A,#0
			JP      NC,ERROR        ;"No room"
CLEAN:			CALL    SETTOP
			DEC     HL
			LD      (HL),#-1         ;WRITE &FFFF
			DEC     HL
			LD      (HL),#-1
			JR      CLEAR
;
SETTOP:			LD      HL,(PAGE)
			LD      B,#0
			LD      A,#CR
SETOP1:			LD      C,(HL)
			INC     C
			DEC     C
			JR      Z,SETOP2
			ADD     HL,BC
			DEC     HL
			CP      (HL)
			INC     HL
			JR      Z,SETOP1
			JP      BAD
SETOP2:			INC     HL              ;N.B. CALLED FROM NEWIT
			INC     HL
			INC     HL
			LD      (TOP),HL
			RET
;
;NEWIT - NEW PROGRAM THEN CLEAR
;   Destroys: H,L
;
;CLEAR - CLEAR ALL DYNAMIC VARIABLES INCLUDING
; FUNCTION AND PROCEDURE POINTERS.
;   Destroys: Nothing
;
NEWIT:			LD      HL,(PAGE)
			LD      (HL),#0
			CALL    SETOP2
CLEAR:			PUSH    HL
			LD      HL,(TOP)
			LD      (LOMEM),HL
			LD      (FREE),HL
			LD      HL,#DYNVAR
			PUSH    BC
			LD      B,#2*(54+2)
CLEAR1:			LD      (HL),#0
			INC     HL
			DJNZ    CLEAR1
			POP     BC
			POP     HL
			RET
;
;LISTIT - LIST A PROGRAM LINE.
;    Inputs: HL addresses line
;            DE = line number (binary)
;            IX addresses LISTON
;  Destroys: A,D,E,B',C',D',E',H',L',IY,F
;
LISTIT:			PUSH    HL
			EX      DE,HL
			PUSH    BC
			CALL    PBCD
			POP     BC
			POP     HL
			LD      A,(HL)
			CP      #NEXT
			CALL    Z,INDENT
			CP      #UNTIL
			CALL    Z,INDENT
			EXX
			LD      A,#' '
			BIT     0,(IX)
			CALL    NZ,OUTCHR
			LD      A,B
			ADD     A,A
			BIT     1,(IX)
			CALL    NZ,FILL
			LD      A,C
			ADD     A,A
			BIT     2,(IX)
			CALL    NZ,FILL
			EXX
			LD      A,(HL)
			CP      #FOR
			CALL    Z,INDENT
			CP      #REPEAT
			CALL    Z,INDENT
			LD      E,#0
LIST8:			LD      A,(HL)
			INC     HL
			CP      #CR
			JR      Z,CRLF
			CP      #34		;ASCII ""
			JR      NZ,LIST7
			INC     E
LIST7:			CALL    LOUT
			JR      LIST8
;
PRLINO:			PUSH    HL
			POP     IY
			PUSH    BC
			CALL    DECODE
			POP     BC
			EXX
			PUSH    BC
			CALL    PBCDL
			POP     BC
			EXX
			PUSH    IY
			POP     HL
			RET
;
LOUT:			BIT     0,E
			JR      NZ,OUTCHR
			CP      #LINO
			JR      Z,PRLINO
			CALL    OUT
			LD      A,(HL)
INDENT:			EXX
			CP      #FOR
			JR      Z,IND1
			CP      #NEXT
			JR      NZ,IND2
			DEC     B
			JP      P,IND2
IND1:			INC     B
IND2:			CP      #REPEAT
			JR      Z,IND3
			CP      #UNTIL
			JR      NZ,IND4
			DEC     C
			JP      P,IND4
IND3:			INC     C
IND4:			EXX
			RET
;
;CRLF - SEND CARRIAGE RETURN, LINE FEED.
;  Destroys: A,F
;OUTCHR - OUTPUT A CHARACTER TO CONSOLE.
;    Inputs: A = character
;  Destroys: A,F
;
CRLF:			LD      A,#CR
			CALL    OUTCHR
			LD      A,#LF
OUTCHR:		CALL    OSWRCH
			SUB     #CR
			JR      Z,CARRET
			RET     C               ;NON-PRINTING
			LD      A,(COUNT)
			INC     A
CARRET:			LD      (COUNT),A
			RET     Z
			PUSH    HL
			LD      HL,(WIDTH)
			CP      L
			POP     HL
			RET     NZ
			JR      CRLF
;
;OUT - SEND CHARACTER OR KEYWORD
;   Inputs: A = character (>=10, <128)
;           A = Token (<10, >=128)
;  Destroys: A,F
;
OUT:			CP      #138
			JP      PE,OUTCHR
			PUSH    BC
			PUSH    HL
			LD      HL,#KEYWDS
			LD      BC,#KEYWDL
			CPIR
TOKEN1:			LD      A,(HL)
			INC     HL
			CP      #138
			PUSH    AF
			CALL    PE,OUTCHR
			POP     AF
			JP      PE,TOKEN1
			POP     HL
			POP     BC
			RET
;
;FINDL - FIND PROGRAM LINE.
;   Inputs: HL = line number (binary)
;  Outputs: HL addresses line (if found)
;           DE = line number
;           Z-flag set if found.
; Destroys: A,B,C,D,E,H,L,F
;
FINDL:			EX      DE,HL
			LD      HL,(PAGE)
			XOR     A               ;A=0
			CP      (HL)
			INC     A
			RET     NC
			XOR     A               ;CLEAR CARRY
			LD      B,A
FINDL1:			LD      C,(HL)
			PUSH    HL
			INC     HL
			LD      A,(HL)
			INC     HL
			LD      H,(HL)
			LD      L,A
			SBC     HL,DE
			POP     HL
			RET     NC              ;FOUND OR PAST
			ADD     HL,BC
			JP      FINDL1
;
;SETLIN - Search program for line containing address.
;         Update (LINENO).
;   Inputs: Address in (ERRLIN)
;  Outputs: Line number in HL and (LINENO)
; Destroys: B,C,D,E,H,L,F
;
SETLIN:		        LD      B,#0
			LD      DE,(ERRLIN)
			LD      HL,(PAGE)
			OR      A
			SBC     HL,DE
			ADD     HL,DE
			JR      NC,SET3
SET1:			LD      C,(HL)
			INC     C
			DEC     C
			JR      Z,SET3
			ADD     HL,BC
			SBC     HL,DE
			ADD     HL,DE
			JR      C,SET1
			SBC     HL,BC
			INC     HL
			LD      E,(HL)          ;LINE NUMBER
			INC     HL
			LD      D,(HL)
			EX      DE,HL
SET2:			LD      (LINENO),HL
			RET
SET3:			LD      HL,#0
			JR      SET2
;
;SAYLN - PRINT " at line nnnn" MESSAGE.
;  Outputs: Carry=0 if line number is zero.
;           Carry=1 if line number is non-zero.
; Destroys: A,B,C,D,E,H,L,F
;
SAYLN:			LD      HL,(LINENO)
			LD      A,H
			OR      L
			RET     Z
			CALL    TELL
			.ASCII  ' at line '
			.BYTE    0
PBCDL:			LD      C,#0
			JR      PBCD0
;
;PBCD - PRINT NUMBER AS DECIMAL INTEGER.
;   Inputs: HL = number (binary).
;  Outputs: Carry = 1
; Destroys: A,B,C,D,E,H,L,F
;
PBCD:			LD      C,#' '
PBCD0:			LD      B,#5
			LD      DE,#10000
PBCD1:			XOR     A
PBCD2:			SBC     HL,DE
			INC     A
			JR      NC,PBCD2
			ADD     HL,DE
			DEC     A
			JR      Z,PBCD3
			SET     4,C
			SET     5,C
PBCD3:			OR      C
			CALL    NZ,OUTCHR
			LD      A,B
			CP      #5
			JR      Z,PBCD4
			ADD     HL,HL
			LD      D,H
			LD      E,L
			ADD     HL,HL
			ADD     HL,HL
			ADD     HL,DE
PBCD4:			LD      DE,#1000
			DJNZ    PBCD1
			SCF
			RET
;
;PUTVAR - CREATE VARIABLE AND INITIALISE TO ZERO.
;   Inputs: HL, IY as returned from GETVAR (NZ).
;  Outputs: As GETVAR.
; Destroys: everything
;
PUTVAR:		CALL    CREATE
			LD      A,(IY)
			CP      #'('
			JR      NZ,GETVZ        ;SET EXIT CONDITIONS
ARRAY:			LD      A,#14            ;'Array'
ERROR3:			JP      ERROR
;
;GETVAR - GET LOCATION OF VARIABLE, RETURN IN HL & IX
;   Inputs: IY addresses first character.
;  Outputs: Carry set and NZ if illegal character.
;           Z-flag set if variable found, then:
;            A = variable type (0,4,5,128 or 129)
;            HL = IX = variable pointer.
;            IY updated
;           If Z-flag & carry reset, then:
;            HL, IY set for subsequent PUTVAR call.
; Destroys: everything
;
GETVAR:		LD      A,(IY)
			CP      #'$'
			JR      Z,GETV4
			CP      #'!'
			JR      Z,GETV5
			CP      #'?'
			JR      Z,GETV6
			CALL    LOCATE
			RET     NZ
			LD      A,(IY)
			CP      #'('             ;ARRAY?
			JR      NZ,GETVX        ;EXIT
			PUSH    DE              ;SAVE TYPE
			LD      A,(HL)          ;NO. OF DIMENSIONS
			OR      A
			JR      Z,ARRAY
			INC     HL
			LD      DE,#0            ;ACCUMULATOR
			PUSH    AF
			INC     IY              ;SKIP (
			JR      GETV3
GETV2:			PUSH    AF
			CALL    COMMA
GETV3:			PUSH    HL
			PUSH    DE
			CALL    EXPRI           ;SUBSCRIPT
			EXX
			POP     DE
			EX      (SP),HL
			LD      C,(HL)
			INC     HL
			LD      B,(HL)
			INC     HL
			EX      (SP),HL
			EX      DE,HL
			PUSH    DE
			CALL    MUL16           ;HL=HL*BC
			POP     DE
			ADD     HL,DE
			EX      DE,HL
			OR      A
			SBC     HL,BC
			LD      A,#15
			JR      NC,ERROR3       ;"Subscript"
			POP     HL
			POP     AF
			DEC     A               ;DIMENSION COUNTER
			JR      NZ,GETV2
			CALL    BRAKET          ;CLOSING BRACKET
			POP     AF              ;RESTORE TYPE
			PUSH    HL
			CALL    X4OR5           ;DE=DE*n
			POP     HL
			ADD     HL,DE
			LD      D,A             ;TYPE
			LD      A,(IY)
GETVX:			CP      #'?'
			JR      Z,GETV9
			CP      #'!'
			JR      Z,GETV8
GETVZ:			PUSH    HL              ;SET EXIT CONDITIONS
			POP     IX
			LD      A,D
			CP      A
			RET
;
;PROCESS UNARY & BINARY INDIRECTION:
;
GETV4:			LD      A,#128           ;STATIC STRING
			JR      GETV7
GETV5:			LD      A,#4             ;UNARY 32-BIT INDIRN.
			JR      GETV7
GETV6:			XOR     A               ;UNARY 8-BIT INDIRECTION
GETV7:			LD      HL,#0
			PUSH    AF
			JR      GETV0
;
GETV8:			LD      B,#4             ;32-BIT BINARY INDIRN.
			JR      GETVA
GETV9:			LD      B,#0             ;8-BIT BINARY INDIRN.
GETVA:			PUSH    HL
			POP     IX
			LD      A,D             ;TYPE
			CP      #129
			RET     Z               ;STRING!
			PUSH    BC
			CALL    LOADN           ;LEFT OPERAND
			CALL    SFIX
			EXX
GETV0:			PUSH    HL
			INC     IY
			CALL    ITEMI
			EXX
			POP     DE
			POP     AF
			ADD     HL,DE
			PUSH    HL
			POP     IX
			CP      A
			RET
;
;GETDEF - Find entry for FN or PROC in dynamic area.
;   Inputs: IY addresses byte following "DEF" token.
;  Outputs: Z flag set if found
;           Carry set if neither FN or PROC first.
;           If Z: HL points to entry
;                 IY addresses delimiter
; Destroys: A,D,E,H,L,IY,F
;
GETDEF:		        LD      A,1(IY)
			CALL    RANGE1
			RET     C
			LD      A,(IY)
			LD      HL,#FNPTR
			CP      #FN
			JR      Z,LOC2
			LD      HL,#PROPTR
			CP      #PROC
			JR      Z,LOC2
			SCF
			RET
;
;LOCATE - Try to locate variable name in static or
;dynamic variables.  If illegal first character return
;carry, non-zero.  If found, return no-carry, zero.
;If not found, return no-carry, non-zero.
;   Inputs: IY addresses first character of name.
;           A=(IY)
;  Outputs: Z-flag set if found, then:
;            IY addresses terminator
;            HL addresses location of variable
;            D=type of variable:  4 = integer
;                                 5 = floating point
;                               129 = string
; Destroys: A,D,E,H,L,IY,F
;
LOCATE:			SUB     #'@'
			RET     C
			LD      H,#0
			CP      #'Z'-'@'+1
			JR      NC,LOC0         ;NOT STATIC
			ADD     A,A
			LD      L,A
			LD      A,1(IY)        ;2nd CHARACTER
			CP      #'%'
			JR      NZ,LOC1         ;NOT STATIC
			LD      A,2(IY)
			CP      #'('
			JR      Z,LOC1          ;NOT STATIC
			ADD     HL,HL
			LD      DE,#STAVAR       ;STATIC VARIABLES
			ADD     HL,DE
			INC     IY
			INC     IY
			LD      D,#4             ;INTEGER TYPE
			XOR     A
			RET
;
LOC0:			CP      #'_'-'@'
			RET     C
			CP      #'z'-'@'+1
			CCF
			DEC     A               ;SET NZ
			RET     C
			SUB     #3
			ADD     A,A
			LD      L,A
LOC1:			LD      DE,#DYNVAR       ;DYNAMIC VARIABLES
			DEC     L
			DEC     L
			SCF
			RET     M
			ADD     HL,DE
LOC2:			LD      E,(HL)
			INC     HL
			LD      D,(HL)
			LD      A,D
			OR      E
			JR      Z,LOC6          ;UNDEFINED VARIABLE
			LD      H,D
			LD      L,E
			INC     HL              ;SKIP LINK
			INC     HL
			PUSH    IY
LOC3:			LD      A,(HL)          ;COMPARE
			INC     HL
			INC     IY
			CP      (IY)
			JR      Z,LOC3
			OR      A               ;0=TERMINATOR
			JR      Z,LOC5          ;FOUND (MAYBE)
LOC4:			POP     IY
			EX      DE,HL
			JP      LOC2            ;TRY NEXT ENTRY
;
LOC5:			DEC     IY
			LD      A,(IY)
			CP      #'('
			JR      Z,LOC5A         ;FOUND
			INC     IY
			CALL    RANGE
			JR      C,LOC5A         ;FOUND
			CP      #'('
			JR      Z,LOC4          ;KEEP LOOKING
			LD      A,-1(IY)
			CALL    RANGE1
			JR      NC,LOC4         ;KEEP LOOKING
LOC5A:			POP     DE
TYPE:			LD      A,-1(IY)
			CP      #'$'
			LD      D,#129
			RET     Z               ;STRING
			CP      #'%'
			LD      D,#4
			RET     Z               ;INTEGER
			INC     D
			CP      A
			RET
;
LOC6:			INC     A               ;SET NZ
			RET
;
;CREATE - CREATE NEW ENTRY, INITIALISE TO ZERO.
;   Inputs: HL, IY as returned from LOCATE (NZ).
;  Outputs: As LOCATE, GETDEF.
; Destroys: As LOCATE, GETDEF.
;
CREATE:		XOR     A
			LD      DE,(FREE)
			LD      (HL),D
			DEC     HL
			LD      (HL),E
			EX      DE,HL
			LD      (HL),A
			INC     HL
			LD      (HL),A
			INC     HL
LOC7:			INC     IY
			CALL    RANGE           ;END OF VARIABLE?
			JR      C,LOC8
			LD      (HL),A
			INC     HL
			CALL    RANGE1
			JR      NC,LOC7
			CP      #'('
			JR      Z,LOC8
			LD      A,1(IY)
			CP      #'('
			JR      Z,LOC7
			INC     IY
LOC8:			LD      (HL),#0          ;TERMINATOR
			INC     HL
			PUSH    HL
			CALL    TYPE
			LD      A,#5
			CP      D
			JR      Z,LOC9
			DEC     A
LOC9:			LD      (HL),#0          ;INITIALISE TO ZERO
			INC     HL
			DEC     A
			JR      NZ,LOC9
			LD      (FREE),HL
			CALL    CHECK
			POP     HL
			XOR     A
			RET
;
;LINNUM - GET LINE NUMBER FROM TEXT STRING
;   Inputs: IY = Text Pointer
;  Outputs: HL = Line number (zero if none)
;           IY updated
; Destroys: A,D,E,H,L,IY,F
;
LINNUM:			CALL    NXT
			LD      HL,#0
LINNM1:			LD      A,(IY)
			SUB     #'0'
			RET     C
			CP      #10
			RET     NC
			INC     IY
			LD      D,H
			LD      E,L
			ADD     HL,HL           ;*2
			JR      C,MTOOBIG
			ADD     HL,HL           ;*4
			JR      C,MTOOBIG
			ADD     HL,DE           ;*5
			JR      C,MTOOBIG
			ADD     HL,HL           ;*10
			JR      C,MTOOBIG
			LD      E,A
			LD      D,#0
			ADD     HL,DE           ;ADD IN DIGIT
			JR      NC,LINNM1       
MTOOBIG:			LD      A,#20
			JP      ERROR           ;"Too big"
;
;PAIR - GET PAIR OF LINE NUMBERS FOR RENUMBER/AUTO.
;   Inputs: IY = text pointer
;  Outputs: HL = first number (10 by default)
;           BC = second number (10 by default)
; Destroys: A,B,C,D,E,H,L,B',C',D',E',H',L',IY,F
;
PAIR:			CALL    LINNUM          ;FIRST
			LD      A,H
			OR      L
			JR      NZ,PAIR1
			LD      L,#10
PAIR1:			CALL    TERM
			INC     IY
			PUSH    HL
			LD      HL,#10
			CALL    NZ,LINNUM       ;SECOND
			EX      (SP),HL
			POP     BC
			LD      A,B
			OR      C
			RET     NZ
			CALL    EXTERR
			.ASCII  'Silly'
			.BYTE    0
;
;DLPAIR - GET PAIR OF LINE NUMBERS FOR DELETE/LIST.
;   Inputs: IY = text pointer
;  Outputs: HL = points to program text
;           BC = second number (0 by default)
; Destroys: A,B,C,D,E,H,L,IY,F
;
DLPAIR:			CALL    LINNUM
			PUSH    HL
			CALL    TERM
			JR      Z,DLP1
			CP      #TIF
			JR      Z,DLP1
			INC     IY
			CALL    LINNUM
DLP1:			EX      (SP),HL
			CALL    FINDL
			POP     BC
			RET
;
;TEST FOR VALID CHARACTER IN VARIABLE NAME:
;   Inputs: IY addresses character
;  Outputs: Carry set if out-of-range.
; Destroys: A,F
;
RANGE:			LD      A,(IY)
			CP      #'$'
			RET     Z
			CP      #'%'
			RET     Z
			CP      #'('
			RET     Z
RANGE1:			CP      #'0'
			RET     C
			CP      #'9'+1
			CCF
			RET     NC
			CP      #'@'             ;V2.4
			RET     Z
RANGE2:			CP      #'A'
			RET     C
			CP      #'Z'+1
			CCF
			RET     NC
			CP      #'_'
			RET     C
			CP      #'z'+1
			CCF
			RET
;
SPACE: 			XOR     A
			CALL    EXTERR          ;"LINE space"
			.BYTE    LINE
			.BYTE    8
			.BYTE    0
;
;LEXAN - LEXICAL ANALYSIS.
;  Bit 0,C: 1=left, 0=right
;  Bit 3,C: 1=in HEX
;  Bit 4,C: 1=accept line number
;  Bit 5,C: 1=in variable, FN, PROC
;  Bit 6,C: 1=in REM, DATA, *
;  Bit 7,C: 1=in quotes
;   Inputs: IY addresses source string
;           DE addresses destination string
;           (must be page boundary)
;           C  sets initial mode
;  Outputs: DE, IY updated
;           A holds carriage return
;
LEXAN1:			LD      (DE),A          ;TRANSFER TO BUFFER
			INC     DE              ;INCREMENT POINTERS
			INC     IY
LEXAN2:		LD      A,E             ;MAIN ENTRY
			CP      #252             ;TEST LENGTH
			JR      NC,SPACE        ;LINE TOO LONG
			LD      A,(IY)
			CP      #CR
			RET     Z               ;END OF LINE
			CALL    RANGE1
			JR      NC,LEXAN3
			RES     5,C             ;NOT IN VARIABLE
			RES     3,C             ;NOT IN HEX
LEXAN3:			CP      #' '
			JR      Z,LEXAN1        ;PASS SPACES
			CP      #','
			JR      Z,LEXAN1        ;PASS COMMAS
			CP      #'G'
			JR      C,LEXAN4
			RES     3,C             ;NOT IN HEX
LEXAN4:			CP      #34		;ASCII ""
			JR      NZ,LEXAN5
			RL      C
			CCF                     ;TOGGLE C7
			RR      C
LEXAN5:			BIT     4,C
			JR      Z,LEXAN6
			RES     4,C
			PUSH    BC
			PUSH    DE
			CALL    LINNUM          ;GET LINE NUMBER
			POP     DE
			POP     BC
			LD      A,H
			OR      L
			CALL    NZ,ENCODE       ;ENCODE LINE NUMBER
			JR      LEXAN2          ;CONTINUE
;
LEXAN6:			DEC     C
			JR      Z,LEXAN7        ;C=1 (LEFT)
			INC     C
			JR      NZ,LEXAN1
			OR      A
			CALL    P,LEX           ;TOKENISE IF POSS.
			JR      LEXAN8
;
LEXAN7:			CP      #'*'
			JR      Z,LEXAN9
			OR      A
			CALL    P,LEX           ;TOKENISE IF POSS.
			CP      #TOKLO
			JR      C,LEXAN8
			CP      #TOKHI+1
			JR      NC,LEXAN8
			ADD     A,#OFFSET        ;LEFT VERSION
LEXAN8:			CP      #REM
			JR      Z,LEXAN9
			CP      #DATA
			JR      NZ,LEXANA
LEXAN9:			SET     6,C             ;QUIT TOKENISING
LEXANA:			CP      #FN
			JR      Z,LEXANB
			CP      #PROC
			JR      Z,LEXANB
			CALL    RANGE2
			JR      C,LEXANC
LEXANB:			SET     5,C             ;IN VARIABLE/FN/PROC
LEXANC:			CP      #'&'
			JR      NZ,LEXAND
			SET     3,C             ;IN HEX
LEXAND:			LD      HL,#LIST1
			PUSH    BC
			LD      BC,#LIST1L
			CPIR
			POP     BC
			JR      NZ,LEXANE
			SET     4,C             ;ACCEPT LINE NUMBER
LEXANE:			LD      HL,#LIST2
			PUSH    BC
			LD      BC,#LIST2L
			CPIR
			POP     BC
			JR      NZ,LEXANF
			SET     0,C             ;ENTER LEFT MODE
LEXANF:			JP      LEXAN1
;
LIST1:			.BYTE    GOTO
			.BYTE    GOSUB
			.BYTE    RESTOR
			.BYTE    TRACE
LIST2:			.BYTE    THEN
			.BYTE    ELSE
LIST1L			.EQU     .-LIST1
			.BYTE    REPEAT
			.BYTE    TERROR
			.BYTE    ':'
LIST2L			.EQU     .-LIST2
;
;ENCODE - ENCODE LINE NUMBER INTO PSEUDO-BINARY FORM.
;   Inputs: HL=line number, DE=string pointer
;  Outputs: DE updated, BIT 4,C set.
; Destroys: A,B,C,D,E,H,L,F
;
ENCODE:			SET     4,C
			EX      DE,HL
			LD      (HL),#LINO
			INC     HL
			LD      A,D
			AND     #0xC0
			RRCA
			RRCA
			LD      B,A
			LD      A,E
			AND     #0xC0
			OR      B
			RRCA
			RRCA
			XOR     #0b01010100
			LD      (HL),A
			INC     HL
			LD      A,E
			AND     #0x3F
			OR      #'@'
			LD      (HL),A
			INC     HL
			LD      A,D
			AND     #0x3F
			OR      #'@'
			LD      (HL),A
			INC     HL
			EX      DE,HL
			RET
;
;TEXT - OUTPUT MESSAGE.
;   Inputs: HL addresses text (terminated by nul)
;  Outputs: HL addresses character following nul.
; Destroys: A,H,L,F
;
REPORT: 		LD      HL,(ERRTXT)
TEXT:			LD      A,(HL)
			INC     HL
			OR      A
			RET     Z
			CALL    OUT
			JR      TEXT
;
;TELL - OUTPUT MESSAGE.
;   Inputs: Text follows subroutine call (term=nul)
; Destroys: A,F
;
TELL:			EX      (SP),HL         ;GET RETURN ADDRESS
			CALL    TEXT
			EX      (SP),HL
			RET
;
CR			.EQU      0x0D
LF			.EQU      0x0A
ESC			.EQU      0x1B
	
	;		ENDMODULE
