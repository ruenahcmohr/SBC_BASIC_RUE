;
;BBC BASIC INTERPRETER - Z80 VERSION
;EXPRESSION EVALUATION & ARITHMETIC MODULE - "EVAL"
;(C) COPYRIGHT  R.T.RUSSELL  1984
;VERSION 2.3, 07-05-1984
;Modified to use external FPP, 01-03-1987
;VERSION 3.0, 08-03-1987
;INSTR bug fixed, 30-09-1992
;
;BINARY FLOATING POINT REPRESENTATION:
;   32 BIT SIGN-MAGNITUDE NORMALIZED MANTISSA
;    8 BIT EXCESS-128 SIGNED EXPONENT
;   SIGN BIT REPLACES MANTISSA MSB (IMPLIED "1")
;   MANTISSA=0 & EXPONENT=0 IMPLIES VALUE IS ZERO.
;
;BINARY INTEGER REPRESENTATION:
;   32 BIT 2'S-COMPLEMENT SIGNED INTEGER
;    "EXPONENT" BYTE = 0 (WHEN PRESENT)
;
;NORMAL REGISTER ALLOCATION: MANTISSA - HLH'L'
;                            EXPONENT - C
;
			.MODULE EVAL
;
;TABLE OF ADDRESSES FOR FUNCTIONS:
;
FUNTOK			.EQU      0x8D             ;1st FUNCTION TOKEN
;
FUNTBL:			.WORD    DECODE          ;Line number
			.WORD    OPENIN          ;OPENIN
			.WORD    VPTR             ;PTR
			.WORD    VPAGEV           ;PAGE
			.WORD    VTIMEV           ;TIME
			.WORD    VLOMEMV          ;LOMEM
			.WORD    VHIMEMV          ;HIMEM
			.WORD    ABS             ;ABS
			.WORD    ACS             ;ACS
			.WORD    ADVAL           ;ADVAL
			.WORD    ASC             ;ASC
			.WORD    ASN             ;ASN
			.WORD    ATN             ;ATN
			.WORD    BGET            ;BGET
			.WORD    COS             ;COS
			.WORD    COUNTV          ;COUNT
			.WORD    DEG             ;DEG
			.WORD    ERLV            ;ERL
			.WORD    ERRV            ;ERR
			.WORD    EVAL            ;EVAL
			.WORD    EXP             ;EXP
			.WORD    VEXT             ;EXT
			.WORD    VZERO            ;FALSE
			.WORD    FN              ;FN
			.WORD    GET             ;GET
			.WORD    INKEY           ;INKEY
			.WORD    INSTR           ;INSTR(
			.WORD    INT             ;INT
			.WORD    LEN             ;LEN
			.WORD    LN              ;LN
			.WORD    LOG             ;LOG
			.WORD    NOTK            ;NOT
			.WORD    OPENUP          ;OPENUP
			.WORD    OPENOT          ;OPENOUT
			.WORD    PI              ;PI
			.WORD    POINT           ;POINT(
			.WORD    POS             ;POS
			.WORD    RAD             ;RAD
			.WORD    RND             ;RND
			.WORD    SGN             ;SGN
			.WORD    SIN             ;SIN
			.WORD    SQR             ;SQR
			.WORD    TAN             ;TAN
			.WORD    TOPV            ;TO(P)
			.WORD    TRUE            ;TRUE
			.WORD    USR             ;USR
			.WORD    VAL             ;VAL
			.WORD    VPOS            ;VPOS
			.WORD    CHRS            ;CHRS
			.WORD    GETS            ;GETS
			.WORD    INKEYS          ;INKEYS
			.WORD    LEFTS           ;LEFTS(
			.WORD    MIDS            ;MIDS(
			.WORD    RIGHTS          ;RIGHTS(
			.WORD    STRS            ;STR$
			.WORD    STRING          ;STRINGS(
			.WORD    EOF             ;EOF
;
TCMD			.EQU     FUNTOK+(.-FUNTBL)/2
;
ANDK			.EQU      0x80
DIVK			.EQU      0x81
EORK			.EQU      0x82
MODK			.EQU      0x83
ORK			.EQU      0x84
;
SOPTBL:			.WORD    SLE             ;<= (STRING)
			.WORD    SNE             ;<>
			.WORD    SGE             ;>=
			.WORD    SLT             ;<
			.WORD    SEQ             ;=
			.WORD    SGT             ;>
;
;EXPR - VARIABLE-TYPE EXPRESSION EVALUATION
;     Expression type is returned in A'F':
;        Numeric - A' bit 7=0, F' sign bit cleared.
;         String - A' bit 7=1, F' sign bit set.
;Floating-point or integer result returned in HLH'L'C
; Integer result denoted by C=0 and HLH'L' non-zero.
;String result returned in string accumulator, DE set.
;
;Hierarchy is: (1) Variables, functions,
;                  constants, bracketed expressions.
;              (2) ^
;              (3) * / MOD DIV
;              (4) + -
;              (5) = <> <= >= > <
;              (6) AND
;              (7) EOR OR
;
EXPR:			CALL    EXPR1           ;GET FIRST OPERAND
EXPR0A:			CP      #EORK            ;CHECK OPERATOR
			JR      Z,EXPR0B
			CP      #ORK
			RET     NZ
EXPR0B:			CALL    SAVE            ;SAVE FIRST OPERAND
			CALL    EXPR1           ;GET SECOND OPERAND
			CALL    DOIT            ;DO OPERATION
			JR      EXPR0A          ;CONTINUE
;
EXPR1:			CALL    EXPR2
EXPR1A:			CP      #ANDK
			RET     NZ
			CALL    SAVE
			CALL    EXPR2
			CALL    DOIT
			JR      EXPR1A
;
EXPR2:			CALL    EXPR3
			CALL    RELOP
			RET     NZ
			LD      B,A
			INC     IY              ;BUMP OVER OPERATOR
			CALL    NXT
			CALL    RELOP          ;COMPOUND OPERATOR?
			JR      NZ,EXPR2B
			INC     IY
			CP      B
			JP      Z,SYNTAX        ;ILLEGAL COMBINATION
			ADD     A,B
			LD      B,A
EXPR2B:			LD      A,B
			EX      AF,AF'
			JP      M,EXPR2S
			EX      AF,AF'
			SUB     #4
			CP      #'>'-4
			JR      NZ,EXPR2C
			ADD     A,#2
EXPR2C:			CALL    SAVE1
			CALL    EXPR3
			CALL    DOIT            ;Must NOT be "JP DOIT"
			RET
;
EXPR2S:			EX      AF,AF'
			DEC     A
			AND     #7
			CALL    PUSHS           ;SAVE STRING ON STACK
			PUSH    AF              ;SAVE OPERATOR
			CALL    EXPR3           ;SECOND STRING
			EX      AF,AF'
			JP      P,VTYPE
			POP     AF
			LD      C,E             ;LENGTH OF STRING #2
			POP     DE
			LD      HL,#0
			ADD     HL,SP
			LD      B,E             ;LENGTH OF STRING #1
			PUSH    DE
			LD      DE,#ACCS
			EX      DE,HL
			CALL    DISPT2
			POP     DE
			EX      DE,HL
			LD      H,#0
			ADD     HL,SP
			LD      SP,HL
			EX      DE,HL
			XOR     A               ;NUMERIC MARKER
			LD      C,A             ;INTEGER MARKER
			EX      AF,AF'
			LD      A,(IY)
			RET
;
EXPR3:			CALL    EXPR4
EXPR3A:			CP      #'-'
			JR      Z,EXPR3B
			CP      #'+'
			RET     NZ
			EX      AF,AF'
			JP      M,EXPR3S
			EX      AF,AF'
EXPR3B:			CALL    SAVE
			CALL    EXPR4
			CALL    DOIT
			JR      EXPR3A
;
EXPR3S:			EX      AF,AF'
			INC     IY              ;BUMP PAST '+'
			CALL    PUSHS           ;SAVE STRING ON STACK
			CALL    EXPR4           ;SECOND STRING
			EX      AF,AF'
			JP      P,VTYPE
			LD      C,E             ;C=LENGTH
			POP     DE
			PUSH    DE
			LD      HL,#ACCS
			LD      D,H
			LD      A,C
			OR      A
			JR      Z,EXP3S3
			LD      B,L
			LD      L,A             ;SOURCE
			ADD     A,E
			LD      E,A             ;DESTINATION
			LD      A,#19
			JP      C,ERROR         ;"String too long"
			PUSH    DE
			DEC     E
			DEC     L
			LDDR                    ;COPY
			POP     DE
EXP3S3:			EXX
			POP     BC
			CALL    POPS            ;RESTORE FROM STACK
			EXX
			OR      #0x80             ;FLAG STRING
			EX      AF,AF'
			LD      A,(IY)
			JR      EXPR3A
;
EXPR4:			CALL    EXPR5
EXPR4A:			CP      #'*'
			JR      Z,EXPR4B
			CP      #'/'
			JR      Z,EXPR4B
			CP      #MODK
			JR      Z,EXPR4B
			CP      #DIVK
			RET     NZ
EXPR4B:			CALL    SAVE
			CALL    EXPR5
			CALL    DOIT
			JR      EXPR4A
;
EXPR5:			CALL    ITEM
			OR      A               ;TEST TYPE
			EX      AF,AF'          ;SAVE TYPE
EXPR5A:			CALL    NXT
			CP      #'^'
			RET     NZ
			CALL    SAVE
			CALL    ITEM
			OR      A
			EX      AF,AF'
			CALL    DOIT
			JR      EXPR5A
;
EXPRN:			CALL    EXPR
			EX      AF,AF'
			RET     P
			JR      VTYPE
;
EXPRI:			CALL    EXPR
			EX      AF,AF'
			JP      P,SFIX
			JR      VTYPE
;
EXPRS:			CALL    EXPR
			EX      AF,AF'
			RET     M
			JR      VTYPE
;
;
ITEMN:			CALL    ITEM
			OR      A
			RET     P
			JR      VTYPE
;
ITEMI:			CALL    ITEM
			OR      A
			JP      P,SFIX
			JR      VTYPE
;
ITEMS:			CALL    ITEM
			OR      A
			RET     M
VTYPE:			LD      A,#6
			JP      ERROR           ;"Type mismatch"
;
ITEM1:			CALL    EXPR            ;BRACKETED EXPR
			CALL    BRAKET
			EX      AF,AF'
			RET
;
;HEX - Get hexadecimal constant.
;   Inputs: ASCII string at (IY)
;  Outputs: Integer result in H'L'HL, C=0, A7=0.
;           IY updated (points to delimiter)
;
VHEX:			CALL    VZERO
			CALL    HEXDIG
			JR      C,BADHEX
HEX1:			INC     IY
			AND     #0x0F
			LD      B,#4
HEX2:			EXX
			ADD     HL,HL
			EXX
			ADC     HL,HL
			DJNZ    HEX2
			EXX
			OR      L
			LD      L,A
			EXX
			CALL    HEXDIG
			JR      NC,HEX1
			XOR     A
			RET
;
BADHEX:			LD      A,#28
			JP      ERROR           ;"Bad HEX"
;
;MINUS - Unary minus.
;   Inputs: IY = text pointer
;  Outputs: Numeric result, same type as argument.
;           Result in H'L'HLC
;
MINUS:			CALL    ITEMN
MINUS0:			DEC     C
			INC     C
			JR      Z,NEGATE        ;ZERO/INTEGER
			LD      A,H
			XOR     #0x80             ;CHANGE SIGN (FP)
			LD      H,A
			XOR     A               ;NUMERIC MARKER
			RET
;
NEGATE:			EXX
			LD      A,H
			CPL
			LD      H,A
			LD      A,L
			CPL
			LD      L,A
			EXX
			LD      A,H
			CPL
			LD      H,A
			LD      A,L
			CPL
			LD      L,A
ADD1:			EXX
			INC     HL
			LD      A,H
			OR      L
			EXX
			LD      A,#0             ;NUMERIC MARKER
			RET     NZ
			INC     HL
			RET
;
;ITEM - VARIABLE TYPE NUMERIC OR STRING ITEM.
;Item type is returned in A:  Bit 7=0 numeric.
;                             Bit 7=1 string.
;Numeric item returned in HLH'L'C.
;String item returned in string accumulator,
;  DE addresses byte after last (E=length).
;
ITEM:			CALL    CHECK
			CALL    NXT
			INC     IY
			CP      #'&'
			JR      Z,VHEX           ;HEX CONSTANT
			CP      #'-'
			JR      Z,MINUS         ;UNARY MINUS
			CP      #'+'
			JR      Z,ITEMN         ;UNARY PLUS
			CP      #'('
			JR      Z,ITEM1         ;EXPRESSION
			CP      #34		;ASCII ""
			JR      Z,CONS          ;STRING CONSTANT
			CP      #TCMD
			JP      NC,SYNTAX       ;SYNTAX ERROR
			CP      #FUNTOK
			JP      NC,DISPAT       ;FUNCTION
			DEC     IY
			CP      #':'
			JR      NC,ITEM2        ;VARIABLE?
			CP      #'0'
			JR      NC,CON          ;NUMERIC CONSTANT
			CP      #'.'
			JR      Z,CON           ;NUMERIC CONSTANT
ITEM2:			CALL    GETVAR          ;VARIABLE
			JR      NZ,NOSUCH
			OR      A
			JP      M,LOADS         ;STRING VARIABLE
LOADN:			OR      A
			JR      Z,LOAD1         ;BYTE VARIABLE
			LD      C,#0
			BIT     0,A
			JR      Z,LOAD4         ;INTEGER VARIABLE
LOAD5:			LD      C,+4(IX)
LOAD4:			EXX
			LD      L,+0(IX)
			LD      H,+1(IX)
			EXX
			LD      L,+2(IX)
			LD      H,+3(IX)
			RET
;
LOAD1:			LD      HL,#0
			EXX
			LD      H,#0
			LD      L,+0(IX)
			EXX
			LD      C,H
			RET
;
NOSUCH:			JP      C,SYNTAX
			LD      A,(LISTON)
			BIT     5,A
			LD      A,#26
			JR      NZ,VERROR0       ;"No such variable"
NOS1:			INC     IY
			CALL    RANGE
			JR      NC,NOS1
			LD      IX,#PC
			XOR     A
			LD      C,A
			JR      LOAD4
;
;CONS - Get string constant from ASCII string.
;   Inputs: ASCII string at (IY)
;  Outputs: Result in string accumulator.
;           D = MS byte of ACCS, E = string length
;           A7 = 1 (string marker)
;           IY updated
;
CONS:			LD      DE,#ACCS
CONS3:			LD      A,(IY)
			INC     IY
			CP      #34		;ASCII ""
			JR      Z,CONS2
CONS1:			LD      (DE),A
			INC     E
			CP      #CR
			JR      NZ,CONS3
			LD      A,#9
VERROR0:			JP      ERROR           ;"Missing """
;
CONS2:			LD      A,(IY)
			CP      #0x34		;ASCII "
			INC     IY
			JR      Z,CONS1
			DEC     IY
			LD      A,#0x80           ;STRING MARKER
			RET
;
;CON - Get unsigned numeric constant from ASCII string.
;   Inputs: ASCII string at (IY).
;  Outputs: Variable-type result in HLH'L'C
;           IY updated (points to delimiter)
;           A7 = 0 (numeric marker)
;
CON:			PUSH    IY
			POP     IX
			LD      A,#36
			CALL    FPP
			JR      C,VERROR0
			PUSH    IX
			POP     IY
			XOR     A
			RET
;
DLOAD5:		LD      B,4(IX)
			EXX
			LD      E,0(IX)
			LD      D,1(IX)
			EXX
			LD      E,2(IX)
			LD      D,3(IX)
			RET
;
LOADS:			LD      DE,#ACCS
			RRA
			JR      NC,LOADS2       ;FIXED STRING
			CALL    LOAD4
			EXX
			LD      A,L
			EXX
			OR      A
			LD      C,A
			LD      A,#0x80           ;STRING MARKER
			RET     Z
			LD      B,#0
			LDIR
			RET
LOADS2:			LD      A,(HL)
			LD      (DE),A
			INC     HL
			CP      #CR
			LD      A,#0x80           ;STRING MARKER
			RET     Z
			INC     E
			JR      NZ,LOADS2
			RET                     ;RETURN NULL STRING
;
;VARIABLE-TYPE FUNCTIONS:
;
;Result returned in HLH'L'C (floating point)
;Result returned in HLH'L' (C=0) (integer)
;Result returned in string accumulator & DE (string)
;All registers destroyed.
;IY (text pointer) updated.
;Bit 7 of A indicates type: 0 = numeric, 1 = string.
;
;
;POS - horizontal cursor position.
;VPOS - vertical cursor position.
;EOF - return status of file.
;BGET - read byte from file.
;INKEY - as GET but wait only n centiseconds.
;GET - wait for keypress and return ASCII value.
;GET(n) - input from Z80 port n.
;ASC - ASCII value of string.
;LEN - length of string.
;LOMEM - location of dynamic variables.
;HIMEM - top of available RAM.
;PAGE - start of current text page.
;TOP - address of first free byte after program.
;ERL - line number where last error occurred.
;ERR - number of last error.
;COUNT - number of printing characters since CR.
;Results are integer numeric.
;
POS:			CALL    GETCSR
			EX      DE,HL
			JR      COUNT1
VPOS:			CALL    GETCSR
			JR      COUNT1
EOF:			CALL    CHANEL
			CALL    OSSTAT
			JP      Z,TRUE
			JP      VZERO
BGET:			CALL    CHANEL          ;CHANNEL NUMBER
			CALL    OSBGET
			LD      L,A
			JR      COUNT0
INKEY:			CALL    INKEYS
			JR      ASC0
GET:			CALL    NXT
			CP      #'('
			JR      NZ,GET0
			CALL    ITEMI           ;PORT ADDRESS
			EXX
			LD      B,H
			LD      C,L
			IN      L,(C)           ;INPUT FROM PORT BC
			JR      COUNT0
GET0:			CALL    GETS
			JR      ASC1
ASC:			CALL    ITEMS
ASC0:			XOR     A
			CP      E
			JP      Z,TRUE          ;NULL STRING
ASC1:			LD      HL,(ACCS)
			JR      COUNT0
LEN:			CALL    ITEMS
			EX      DE,HL
			JR      COUNT0
VLOMEMV:			LD      HL,(LOMEM)
			JR      COUNT1
VHIMEMV:			LD      HL,(HIMEM)
			JR      COUNT1
VPAGEV:			LD      HL,(PAGE)
			JR      COUNT1
TOPV:			LD      A,(IY)
			INC     IY              ;SKIP "P"
			CP      #'P'
			JP      NZ,SYNTAX       ;"Syntax Error"
			LD      HL,(TOP)
			JR      COUNT1
ERLV:			LD      HL,(ERL)
			JR      COUNT1
ERRV:			LD      HL,(ERR)
			JR      COUNT0
COUNTV:			LD      HL,(COUNT)
COUNT0:			LD      H,#0
COUNT1:			EXX
			XOR     A
			LD      C,A             ;INTEGER MARKER
			LD      H,A
			LD      L,A
			RET
;
;OPENIN - Open a file for reading.
;OPENOUT - Open a file for writing.
;OPENUP - Open a file for reading or writing.
;Result is integer channel number (0 if error)
;
OPENOT:			XOR     A
			.BYTE    0x21             ;SKIP NEXT 2 BYTES
OPENUP:			LD      A,#2
			.BYTE    0x21             ;SKIP NEXT 2 BYTES
OPENIN:			LD      A,#1
			PUSH    AF              ;SAVE OPEN TYPE
			CALL    ITEMS           ;FILENAME
			LD      A,#CR
			LD      (DE),A
			POP     AF              ;RESTORE OPEN TYPE
			ADD     A,#-1            ;AFFECT FLAGS
			LD      HL,#ACCS
			CALL    OSOPEN
			LD      L,A
			JR      COUNT0
;
;EXT - Return length of file.
;PTR - Return current file pointer.
;Results are integer numeric.
;
VEXT:			CALL    CHANEL
			CALL    GETEXT
			JR      TIME0
;
VPTR:			CALL    CHANEL
			CALL    GETPTR
			JR      TIME0
;
;TIME - Return current value of elapsed time.
;Result is integer numeric.
;
VTIMEV:			LD      A,(IY)
			CP      #'$'
			JR      Z,VTIMEVS
			CALL    GETIME
TIME0:			PUSH    DE
			EXX
			POP     HL
			XOR     A
			LD      C,A
			RET
;
;TIME$ - Return date/time string.
;Result is string
;
VTIMEVS:			INC     IY              ;SKIP $
			CALL    GETIMS
			LD      A,#0x80           ;MARK STRING
			RET
;
;String comparison:
;
SLT:			CALL    SCP
			RET     NC
			JR      TRUE
;
SGT:			CALL    SCP
			RET     Z
			RET     C
			JR      TRUE
;
SGE:			CALL    SCP
			RET     C
			JR      TRUE
;
SLE:			CALL    SCP
			JR      Z,TRUE
			RET     NC
			JR      TRUE
;
SNE:			CALL    SCP
			RET     Z
			JR      TRUE
;
SEQ:			CALL    SCP
			RET     NZ
TRUE:			LD      A,#-1
			EXX
			LD      H,A
			LD      L,A
			EXX
			LD      H,A
			LD      L,A
			INC     A
			LD      C,A
			RET
;
;PI - Return PI (3.141592654)
;Result is floating-point numeric.
;
PI:			LD      A,#35
			JR      FPP1
;
;ABS - Absolute value
;Result is numeric, variable type.
;
ABS:			LD      A,#16
			JR      FPPN
;
;NOT - Complement integer.
;Result is integer numeric.
;
NOTK:			LD      A,#26
			JR      FPPN
;
;DEG - Convert radians to degrees
;Result is floating-point numeric.
;
DEG:			LD      A,#21
			JR      FPPN
;
;RAD - Convert degrees to radians
;Result is floating-point numeric.
;
RAD:			LD      A,#27
			JR      FPPN
;
;SGN - Return -1, 0 or +1
;Result is integer numeric.
;
SGN:			LD      A,#28
			JR      FPPN
;
;INT - Floor function
;Result is integer numeric.
;
INT:			LD      A,#23
			JR      FPPN
;
;SQR - square root
;Result is floating-point numeric.
;
SQR:			LD      A,#30
			JR      FPPN
;
;TAN - Tangent function
;Result is floating-point numeric.
;
TAN:			LD      A,#31
			JR      FPPN
;
;COS - Cosine function
;Result is floating-point numeric.
;
COS:			LD      A,#20
			JR      FPPN
;
;SIN - Sine function
;Result is floating-point numeric.
;
SIN:			LD      A,#29
			JR      FPPN
;
;EXP - Exponential function
;Result is floating-point numeric.
;
EXP:			LD      A,#22
			JR      FPPN
;
;LN - Natural log.
;Result is floating-point numeric.
;
LN:			LD      A,#24
			JR      FPPN
;
;LOG - base-10 logarithm.
;Result is floating-point numeric.
;
LOG:			LD      A,#25
			JR      FPPN
;
;ASN - Arc-sine
;Result is floating-point numeric.
;
ASN:			LD      A,#18
			JR      FPPN
;
;ATN - arc-tangent
;Result is floating-point numeric.
;
ATN:			LD      A,#19
			JR      FPPN
;
;ACS - arc-cosine
;Result is floating point numeric.
;
ACS:			LD      A,#17
FPPN:			PUSH    AF
			CALL    ITEMN
			POP     AF
FPP1:			CALL    FPP
			JP      C,ERROR
			XOR     A
			RET
;
;SFIX - Convert to fixed-point notation
;
SFIX:			LD      A,#38
			JR      FPP1
;
;SFLOAT - Convert to floating-point notation
;
SFLOAT:			LD      A,#39
			JR      FPP1
;
;VAL - Return numeric value of string.
;Result is variable type numeric.
;
VAL:			CALL    ITEMS
VAL0:			XOR     A
			LD      (DE),A
			LD      IX,#ACCS
			LD      A,#36
			JR      FPP1
;
;EVAL - Pass string to expression evaluator.
;Result is variable type (numeric or string).
;
EVAL:			CALL    ITEMS
			LD      A,#CR
			LD      (DE),A
			PUSH    IY
			LD      DE,#ACCS
			LD      IY,#ACCS
			LD      C,#0
			CALL    LEXAN2          ;TOKENISE
			LD      (DE),A
			INC     DE
			XOR     A
			CALL    PUSHS           ;PUT ON STACK
			LD      IY,#2
			ADD     IY,SP
			CALL    EXPR
			POP     IY
			ADD     IY,SP
			LD      SP,IY           ;ADJUST STACK POINTER
			POP     IY
			EX      AF,AF'
			RET
;
;RND - Random number function.
; RND gives random integer 0-&FFFFFFFF
; RND(-n) seeds random number & returns -n.
; RND(0) returns last value in RND(1) form.
; RND(1) returns floating-point 0-0.99999999.
; RND(n) returns random integer 1-n.
;
RND:			LD      IX,#RANDOM
			CALL    NXT
			CP      #'('
			JR      Z,RND5          ;ARGUMENT FOLLOWS
			CALL    LOAD5
RND1:			RR      C
			LD      B,#32
RND2:			EXX                     ;CALCULATE NEXT
			ADC     HL,HL
			EXX
			ADC     HL,HL
			BIT     3,L
			JR      Z,RND3
			CCF
RND3:			DJNZ    RND2
RND4:			RL      C               ;SAVE CARRY
			CALL    STORE5          ;STORE NEW NUMBER
			XOR     A
			LD      C,A
			RET
RND5:			CALL    ITEMI
			LD      IX,#RANDOM
			BIT     7,H             ;NEGATIVE?
			SCF
			JR      NZ,RND4         ;SEED
			CALL    TEST
			PUSH    AF
			CALL    SWAP
			EXX
			CALL    LOAD5
			CALL    NZ,RND1         ;NEXT IF NON-ZERO
			EXX                     ;SCRAMBLE (CARE!)
			LD      C,#0x7F
RND6:			BIT     7,H             ;FLOAT
			JR      NZ,RND7
			EXX
			ADD     HL,HL
			EXX
			ADC     HL,HL
			DEC     C
			JR      NZ,RND6
RND7:			RES     7,H             ;POSITIVE 0-0.999999
			POP     AF
			RET     Z               ;ZERO ARGUMENT
			EXX
			LD      A,E
			DEC     A
			OR      D
			EXX
			OR      E
			OR      D
			RET     Z               ;ARGUMENT=1
			LD      B,#0             ;INTEGER MARKER
			LD      A,#10
			CALL    FPP             ;MULTIPLY
			JP      C,ERROR
			CALL    SFIX
			JP      ADD1
;
;INSTR - String search.
;Result is integer numeric.
;
INSTR:			CALL    EXPRSC          ;STRING TO SEARCH
			CALL    PUSHS           ;SAVE STRING ON STACK
			CALL    EXPRS           ;SUB-STRING
			POP     BC
			LD      HL,#0
			ADD     HL,SP           ;HL ADDRESSES MAIN
			PUSH    BC              ;C = MAIN STRING LENGTH
			LD      B,E             ;B = SUB-STRING LENGTH
			CALL    NXT
			CP      #','
			LD      A,#0
			JR      NZ,INSTR1
			INC     IY              ;SKIP COMMA
			PUSH    BC              ;SAVE LENGTHS
			PUSH    HL              ;SAVE MAIN ADDRESS
			CALL    PUSHS
			CALL    EXPRI
			POP     BC
			CALL    POPS
			POP     HL              ;RESTORE MAIN ADDRESS
			POP     BC              ;RESTORE LENGTHS
			EXX
			LD      A,L
			EXX
			OR      A
			JR      Z,INSTR1
			DEC     A
INSTR1:			LD      DE,#ACCS         ;DE ADDRESSES SUB
			CALL    VSEARCH
			POP     DE
			JR      Z,INSTR2        ;N.B. CARRY CLEARED
			SBC     HL,HL
			ADD     HL,SP
INSTR2:			SBC     HL,SP
			EX      DE,HL
			LD      H,#0
			ADD     HL,SP
			LD      SP,HL
			EX      DE,HL
			CALL    BRAKET
			JP      COUNT1
;
;SEARCH - Search string for sub-string
;   Inputs: Main string at HL length C
;           Sub-string  at DE length B
;           Starting offset A
;  Outputs: NZ - not found
;           Z - found at location HL-1
;           Carry always cleared
;
VSEARCH:		PUSH    BC
			LD      B,#0
			LD      C,A
			ADD     HL,BC           ;NEW START ADDRESS
			POP     BC
			SUB     C
			JR      NC,SRCH4
			NEG
			LD      C,A             ;REMAINING LENGTH
VSRCH1:			LD      A,(DE)
			PUSH    BC
			LD      B,#0
			CPIR                    ;FIND FIRST CHARACTER
			LD      A,C
			POP     BC
			JR      NZ,SRCH4
			LD      C,A
			DEC     B               ;Bug fix
			CP      B               ;Bug fix
			INC     B               ;Bug fix
			JR      C,SRCH4         ;Bug fix
			PUSH    BC
			PUSH    DE
			PUSH    HL
			DEC     B
			JR      Z,SRCH3         ;FOUND !
VSRCH2:			INC     DE
			LD      A,(DE)
			CP      (HL)
			JR      NZ,SRCH3
			INC     HL
			DJNZ    VSRCH2
SRCH3:			POP     HL
			POP     DE
			POP     BC
			JR      NZ,VSRCH1
			XOR     A               ;Z, NC
			RET                     ;FOUND
;
SRCH4:			OR       #0xFF            ;NZ, NC
			RET                     ;NOT FOUND
;
;CHRS - Return character with given ASCII value.
;Result is string.
;
CHRS:			CALL    ITEMI
			EXX
			LD      A,L
			JR      GET1
;
;GETS - Return key pressed as string.
;Result is string.
;
GETS:			CALL    OSRDCH
GET1:			SCF
			JR      INKEY1
;
;INKEYS - Wait up to n centiseconds for keypress.
;         Return key pressed as string or null
;         string if time elapsed.
;Result is string.
;
INKEYS:			CALL    ITEMI
			EXX
			CALL    OSKEY
INKEY1:			LD      DE,#ACCS
			LD      (DE),A
			LD      A,#0x80
			RET     NC
			INC     E
			RET
;
;MID$ - Return sub-string.
;Result is string.
;
MIDS:			CALL    EXPRSC
			CALL    PUSHS           ;SAVE STRING ON STACK
			CALL    EXPRI
			POP     BC
			CALL    POPS
			EXX
			LD      A,L
			EXX
			OR      A
			JR      Z,MIDS1
			DEC     A
			LD      L,A
			SUB     E
			LD      E,#0
			JR      NC,MIDS1
			NEG
			LD      C,A
			CALL    RIGHT1
MIDS1:			CALL    NXT
			CP      #','
			INC     IY
			JR      Z,LEFT1
			DEC     IY
			CALL    BRAKET
			LD      A,#0x80
			RET
;
;LEFT$ - Return left part of string.
;Carry cleared if entire string returned.
;Result is string.
;
LEFTS:			CALL    EXPRSC
LEFT1:			CALL    PUSHS           ;SAVE STRING ON STACK
			CALL    EXPRI
			POP     BC
			CALL    POPS
			CALL    BRAKET
			EXX
			LD      A,L
			EXX
			CP      E
			JR      NC,LEFT3
			LD      L,E             ;FOR RIGHTS
LEFT2:			LD      E,A
LEFT3:			LD      A,#0x80           ;STRING MARKER
			RET
;
;RIGHT$ - Return right part of string.
;Result is string.
;
RIGHTS:			CALL    LEFTS
			RET     NC
			INC     E
			DEC     E
			RET     Z
			LD      C,E
			LD      A,L
			SUB     E
			LD      L,A
RIGHT1:			LD      B,#0
			LD      H,D
			LD      E,B
			LDIR                    ;MOVE
			LD      A,#0x80
			RET
;
;STRINGS - Return n concatenations of a string.
;Result is string.
;
STRING:			CALL    EXPRI
			CALL    COMMA
			EXX
			LD      A,L
			EXX
			PUSH    AF
			CALL    EXPRS
			CALL    BRAKET
			POP     AF
			OR      A
			JR      Z,LEFT2         ;N=0
			DEC     A
			LD      C,A
			LD      A,#0x80           ;STRING MARKER
			RET     Z
			INC     E
			DEC     E
			RET     Z               ;NULL STRING
			LD      B,E
			LD      H,D
			LD      L,#0
STRIN1:			PUSH    BC
STRIN2:			LD      A,(HL)
			INC     HL
			LD      (DE),A
			INC     E
			LD      A,#19
			JP      Z,ERROR         ;"String too long"
			DJNZ    STRIN2
			POP     BC
			DEC     C
			JR      NZ,STRIN1
			LD      A,#0x80
			RET
;
;SUBROUTINES
;
;SWAP - Swap arguments
;Exchanges DE,HL D'E',H'L' and B,C
;Destroys: A,B,C,D,E,H,L,D',E',H',L'
;
SWAP:			LD      A,C
			LD      C,B
			LD      B,A
			EX      DE,HL
			EXX
			EX      DE,HL
			EXX
			RET
;
;TEST - Test HLH'L' for zero
;Outputs: Z-flag set & A=0 if zero
;Destroys: A,F
;
TEST:			LD      A,H
			OR      L
			EXX
			OR      H
			OR      L
			EXX
			RET
;
;DECODE - Decode line number in pseudo-binary.
;   Inputs: IY = Text pointer.
;   Outputs: HL=0, H'L'=line number, C=0.
;   Destroys: A,C,H,L,H',L',IY,F
;
DECODE:		EXX
			LD      A,(IY)
			INC     IY
			RLA
			RLA
			LD      H,A
			AND      #0xC0
			XOR     (IY)
			INC     IY
			LD      L,A
			LD      A,H
			RLA
			RLA
			AND      #0xC0
			XOR     (IY)
			INC     IY
			LD      H,A
			EXX
			XOR     A
			LD      C,A
			LD      H,A
			LD      L,A
			RET
;
;HEXSTR - convert numeric value to HEX string.
;   Inputs: HLH'L'C = integer or floating-point number
;  Outputs: String in string accumulator.
;           E = string length.  D = ACCS/256
;
HEXSTS:			INC     IY              ;SKIP TILDE
			CALL    ITEMN
			CALL    HEXSTR
			LD      A,#0x80
			RET
;
HEXSTR:		CALL    SFIX
			LD      BC,#8
			LD      DE,#ACCS
HEXST1:			PUSH    BC
			LD      B,#4
			XOR     A
HEXST2:			EXX
			ADD     HL,HL
			EXX
			ADC     HL,HL
			RLA
			DJNZ    HEXST2
			POP     BC
			DEC     C
			RET     M
			JR      Z,HEXST3
			OR      A
			JR      NZ,HEXST3
			CP      B
			JR      Z,HEXST1
HEXST3:			ADD     A,#0x90
			DAA
			ADC     A,#0x40
			DAA
			LD      (DE),A
			INC     DE
			LD      B,A
			JR      HEXST1
;
;Function STR - convert numeric value to ASCII string.
;   Inputs: HLH'L'C = integer or floating-point number.
;  Outputs: String in string accumulator.
;           E = length, D = ACCS/256
;           A = 80H (type=string)
;
;First normalise for decimal output:
;
STRS:			CALL    NXT
			CP      #'~'
			JR      Z,HEXSTS
			CALL    ITEMN
			LD      IX,#STAVAR
			LD      A,3(IX)
			OR      A
			LD      IX,#G9-1         ;G9 FORMAT
			JR      Z,STR0
STR:			LD      IX,#STAVAR
STR0:			LD      DE,#ACCS
			LD      A,#37
			CALL    FPP
			JP      C,ERROR
			BIT     0,2(IX)
STR1:			LD      A,#0x80           ;STRING MARKER
			RET     Z
			LD      A,C
			ADD     A,#4
STR2:			CP      E
			JR      Z,STR1
			EX      DE,HL	
			LD      (HL),#' '        ;TRAILING SPACE
			INC     HL
			EX      DE,HL
			JR      STR2
;
G9:			.WORD    9
;
;STRING COMPARE
;Compare string (DE) length B with string (HL) length C.
;Result preset to false.
;
SCP:			CALL    SCP0
VZERO:			LD      A,#0
			EXX
			LD      H,A
			LD      L,A
			EXX
			LD      H,A
			LD      L,A
			LD      C,A
			RET
;
SCP0:			INC     B
			INC     C
SCP1:			DEC     B
			JR      Z,SCP2
			DEC     C
			JR      Z,SCP3
			LD      A,(DE)
			CP      (HL)
			RET     NZ
			INC     DE
			INC     HL
			JR      SCP1
SCP2:			OR      A
				DEC     C
			RET     Z
			SCF
			RET
SCP3:			OR      A
			INC     C
			RET
;
;PUSHS - SAVE STRING ON STACK.
;    Inputs: String in string accumulator.
;            E = string length.
;            A - saved on stack.
;  Destroys: B,C,D,E,H,L,IX,SP,F
;
PUSHS:			CALL    CHECK
			POP     IX              ;RETURN ADDRESS
			OR      A               ;CLEAR CARRY
			LD      HL,#ACCS
			LD      D,H
			LD      B,L             ;B=0
			SBC     HL,DE
			ADD     HL,SP
			LD      SP,HL
			LD      D,A
			PUSH    DE
			JR      Z,PUSHS1        ;ZERO LENGTH
			LD      C,E
			LD      DE,#ACCS
			EX      DE,HL
			LDIR                    ;COPY TO STACK
			CALL    CHECK
PUSHS1:			JP      (IX)            ;"RETURN"
;
;POPS - RESTORE STRING FROM STACK.
;    Inputs: C = string length.
;   Outputs: String in string accumulator.
;            E = string length.
;  Destroys: B,C,D,E,H,L,IX,SP,F
;
POPS:			POP     IX              ;RETURN ADDRESS
			LD      HL,#0
			LD      B,H             ;B=0
			ADD     HL,SP
			LD      DE,#ACCS
			INC     C
			DEC     C
			JR      Z,POPS1         ;ZERO LENGTH
			LDIR                    ;COPY FROM STACK
POPS1:			LD      SP,HL
			JP      (IX)            ;"RETURN"
;
HEXDIG:			LD      A,(IY)
			CP      #'0'
			RET     C
			CP      #'9'+1
			CCF
			RET     NC
			CP      #'A'
			RET     C
			SUB     #'A'-10
			CP      #16
			CCF
			RET
;
RELOP:			CP      #'>'
			RET     NC
			CP      #'='
			RET     NC
			CP      #'<'
			RET
;
EXPRSC:			CALL    EXPRS
COMMA:			CALL    NXT
			INC     IY
			CP      #','
			RET     Z
			LD      A,#5
			JR      VERROR1          ;"Missing ,"
;
BRAKET:		CALL    NXT
			INC     IY
			CP      #')'
			RET     Z
			LD      A,#27
VERROR1:			JP      ERROR           ;"Missing )"
;
ESAVE:			INC     IY
SAVE1:			EX      AF,AF'
			JP      M,VTYPE
			EX      AF,AF'
			EX      (SP),HL
			EXX
			PUSH    HL
			EXX
			PUSH    AF
			PUSH    BC
			JP      (HL)
;
DOIT:			EX      AF,AF'
			JP      M,VTYPE
			EXX
			POP     BC              ;RETURN ADDRESS
			EXX
			LD      A,C
			POP     BC
			LD      B,A
			POP     AF              ;OPERATOR
			EXX
			EX      DE,HL
			POP     HL
			EXX
			EX      DE,HL
			POP     HL
			EXX
			PUSH    BC
			EXX
			AND     #0x0F
			CALL    FPP
			JR      C,VERROR1
			XOR     A
			EX      AF,AF'          ;TYPE
			LD      A,(IY)
			RET
;
NXT:			LD      A,(IY)
			CP      #' '
			RET     NZ
			INC     IY
			JP      NXT
;
DISPT2:			PUSH    HL
			LD      HL,#SOPTBL
			JR      DISPT0
;
DISPAT:			PUSH    HL
			SUB     #FUNTOK
			LD      HL,#FUNTBL
DISPT0:			PUSH    BC
			ADD     A,A
			LD      C,A
			LD      B,#0
			ADD     HL,BC
			LD      A,(HL)
			INC     HL
			LD      H,(HL)
			LD      L,A
			POP     BC
			EX      (SP),HL
			RET                     ;OFF TO ROUTINE
;
CR			.EQU      0x0D

			;ENDMODULE
