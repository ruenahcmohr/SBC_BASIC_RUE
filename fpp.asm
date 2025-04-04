;
;Z80 FLOATING POINT PACKAGE
;(C) COPYRIGHT  R.T.RUSSELL  1986
;VERSION 0.0, 26-10-1986
;VERSION 0.1, 14-12-1988 (BUG FIX)
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
;ALTERNATE REGISTER ALLOCATION: MANTISSA - DED'E'
;                               EXPONENT - B
;
;Error codes:
;
			.MODULE FPP

BADOP			.EQU     1               ;Bad operation code
DIVBY0			.EQU     18              ;Division by zero
TOOBIG			.EQU     20              ;Too big
NGROOT			.EQU     21              ;Negative root
LOGRNG			.EQU     22              ;Log range
ACLOST			.EQU     23              ;Accuracy lost
EXPRNG			.EQU     24              ;Exp range
;
;Call entry and despatch code:
;
FPP:			PUSH    IY              ;Save IY
        		LD      IY,#0
        		ADD     IY,SP           ;Save SP in IY
        		CALL    OP              ;Perform operation
        		CP      A               ;Good return (Z, NC)
FPEXIT:			POP     IY              ;Restore IY
        		RET                     ;Return to caller
;
;Error exit:
;
FPBAD:			LD      A,#BADOP         ;"Bad operation code"
FPERROR:			LD      SP,IY           ;Restore SP from IY
        		OR      A               ;Set NZ
        		SCF                     ;Set C
        		JR      FPEXIT
;
;Perform operation or function:
;
OP:			CP      #(RTABLE-DTABLE)/2
        		JR      NC,FPBAD
        		CP      #(FTABLE-DTABLE)/2
        		JR      NC,FPDISTAT
        		EX      AF,AF'
        		LD      A,B
        		OR      C               ;Both integer?
        		CALL    NZ,FLOATA       ;No, so float both
        		EX      AF,AF'
FPDISTAT:			PUSH    HL
        		LD      HL,#DTABLE
        		PUSH    BC
        		ADD     A,A             ;A = op-code * 2
        		LD      C,A
        		LD      B,#0             ;BC = op-code * 2
        		ADD     HL,BC
        		LD      A,(HL)          ;Get low byte
        		INC     HL
        		LD      H,(HL)          ;Get high byte
        		LD      L,A
        		POP     BC
        		EX      (SP),HL
        		RET                     ;Off to routine
;
;Despatch table:
;
DTABLE:			.WORD    IAND            ;AND (INTEGER)
        		.WORD    IBDIV           ;DIV
        		.WORD    IEOR            ;EOR
        		.WORD    IMOD            ;MOD
        		.WORD    IOR             ;OR
        		.WORD    ILE             ;<=
        		.WORD    INE             ;<>
        		.WORD    IGE             ;>=
        		.WORD    ILT             ;<
        		.WORD    IEQ             ;=
        		.WORD    IMUL            ;*
        		.WORD    IADD            ;+
        		.WORD    IGT             ;>
        		.WORD    ISUB            ;-
        		.WORD    IPOW            ;^
        		.WORD    IDIV            ;/
;
FTABLE:			.WORD    FPABS             ;ABS
        		.WORD    FPACS             ;ACS
        		.WORD    FPASN             ;ASN
        		.WORD    FPATN             ;ATN
        		.WORD    FPCOS             ;COS
        		.WORD    FPDEG             ;DEG
        		.WORD    FPEXP             ;EXP
        		.WORD    FPINT             ;INT
        		.WORD    FPLN              ;LN
        		.WORD    FPLOG             ;LOG
        		.WORD    FPNOTK            ;NOT
        		.WORD    FPRAD             ;RAD
        		.WORD    FPSGN             ;SGN
        		.WORD    FPSIN             ;SIN
        		.WORD    FPSQR             ;SQR
        		.WORD    FPTAN             ;TAN
;
		        .WORD    ZERO            ;ZERO
        		.WORD    FONE            ;FONE
        		.WORD    FPTRUE            ;TRUE
        		.WORD    FPPI              ;PI
;
		        .WORD    FPVAL             ;VAL
        		.WORD    FPSTR            ;STR$
;
        		.WORD    FPSFIX            ;FIX
        		.WORD    FPSFLOAT          ;FLOAT
;
		        .WORD    FTEST           ;TEST
        		.WORD    FCOMP           ;COMPARE
;
RTABLE:			.WORD    FAND            ;AND (FLOATING-POINT)
        		.WORD    FBDIV           ;DIV
        		.WORD    FEOR            ;EOR
        		.WORD    FMOD            ;MOD
        		.WORD    FPFOR             ;OR
        		.WORD    FLE             ;<= 
        		.WORD    FNE             ;<>
        		.WORD    FGE             ;>=
        		.WORD    FLT             ;<
        		.WORD    FEQ             ;=
        		.WORD    FMUL            ;*
        		.WORD    FADD            ;+
        		.WORD    FGT             ;>
        		.WORD    FSUB            ;-
        		.WORD    FPOW            ;^
        		.WORD    FDIV            ;/
;
;       PAGE
;
;ARITHMETIC AND LOGICAL OPERATORS:
;All take two arguments, in HLH'L'C & DED'E'B.
;Output in HLH'L'C
;All registers except IX, IY destroyed.
; (N.B. FPOW destroys IX).
;
;FAND - Floating-point AND.
;IAND - Integer AND.
;
FAND:			CALL    FIX2
IAND:			LD      A,H
        		AND     D
        		LD      H,A
        		LD      A,L
        		AND     E
        		LD      L,A
        		EXX
        		LD      A,H
        		AND     D
        		LD      H,A
        		LD      A,L
        		AND     E
        		LD      L,A
        		EXX
        		RET
;
;FEOR - Floating-point exclusive-OR.
;IEOR - Integer exclusive-OR.
;
FEOR:			CALL    FIX2
IEOR:			LD      A,H
        		XOR     D
        		LD      H,A
        		LD      A,L
        		XOR     E
        		LD      L,A
        		EXX
        		LD      A,H
        		XOR     D
        		LD      H,A
        		LD      A,L
        		XOR     E
        		LD      L,A
        		EXX
        		RET
;
;FOR - Floating-point OR.
;IOR - Integer OR.
;
FPFOR:			CALL    FIX2
IOR:			LD      A,H
        		OR      D
        		LD      H,A
        		LD      A,L
        		OR      E
        		LD      L,A
        		EXX
        		LD      A,H
        		OR      D
        		LD      H,A
        		LD      A,L
        		OR      E
        		LD      L,A
        		EXX
        		RET
;
;FMOD - Floating-point remainder.
;IMOD - Integer remainder.
;
FMOD:			CALL    FIX2
IMOD:			LD      A,H
        		XOR     D               ;DIV RESULT SIGN
        		BIT     7,H
        		EX      AF,AF'
        		BIT     7,H
        		CALL    NZ,FPNEGATE       ;MAKE ARGUMENTS +VE
        		CALL    FPSWAP
        		BIT     7,H
        		CALL    NZ,FPNEGATE
        		LD      B,H
        		LD      C,L
        		LD      HL,#0
        		EXX
        		LD      B,H
        		LD      C,L
        		LD      HL,#0
        		LD      A,#-33
        		CALL    DIVA            ;DIVIDE
        		EXX
        		LD      C,#0             ;INTEGER MARKER
        		EX      AF,AF'
        		RET     Z
        		JP      FPNEGATE
;
;BDIV - Integer division.
;
FBDIV:			CALL    FIX2
IBDIV:			CALL    IMOD
        		OR      A
        		CALL    FPSWAP
        		LD      C,#0
        		RET     P
        		JP      FPNEGATE
;
;ISUB - Integer subtraction.
;FSUB - Floating point subtraction with rounding.
;
ISUB:			CALL    SUB
        		RET     PO
        		CALL    ADD
        		CALL    FLOAT2
FSUB:			LD      A,D
        		XOR     #0x80             ;CHANGE SIGN THEN ADD
        		LD      D,A
        		JR      FADD
;
;Reverse subtract.
;
RSUB:			LD      A,H
        		XOR     #0x80
        		LD      H,A
        		JR      FADD
;
;IADD - Integer addition.
;FADD - Floating point addition with rounding.
;
IADD:			CALL    ADD
        		RET     PO
        		CALL    SUB
        		CALL    FLOAT2
FADD:			DEC     B
        		INC     B
        		RET     Z               ;ARG 2 ZERO
        		DEC     C
        		INC     C
        		JP      Z,FPSWAP          ;ARG 1 ZERO
        		EXX
        		LD      BC,#0            ;INITIALISE
        		EXX
        		LD      A,H
        		XOR     D               ;XOR SIGNS
        		PUSH    AF
        		LD      A,B
        		CP      C               ;COMPARE EXPONENTS
        		CALL    C,FPSWAP          ;MAKE DED'E'B LARGEST
        		LD      A,B
        		SET     7,H             ;IMPLIED 1
        		CALL    NZ,FIX          ;ALIGN
        		POP     AF
        		LD      A,D             ;SIGN OF LARGER
        		SET     7,D             ;IMPLIED 1
        		JP      M,FADD3         ;SIGNS DIFFERENT
        		CALL    ADD             ;HLH'L'=HLH'L'+DED'E'
        		CALL    C,DIV2          ;NORMALISE
        		SET     7,H
        		JR      FADD4
;
FADD3:			CALL    SUB             ;HLH'L'=HLH'L'-DED'E'
        		CALL    C,NEG           ;NEGATE HLH'L'B'C'
        		CALL    FLO48
        		CPL                     ;CHANGE RESULT SIGN
FADD4:			EXX
        		EX      DE,HL
        		LD      HL,#0x8000
        		OR      A               ;CLEAR CARRY
        		SBC     HL,BC
        		EX      DE,HL
        		EXX
        		CALL    Z,ODD           ;ROUND UNBIASSED
        		CALL    C,FPADD1          ;ROUND UP
        		CALL    C,INCC
        		RES     7,H
        		DEC     C
        		INC     C
        		JP      Z,ZERO
        		OR      A               ;RESULT SIGNQ
        		RET     P               ;POSITIVE
        		SET     7,H             ;NEGATIVE
        		RET
;
;IDIV - Integer division.
;FDIV - Floating point division with rounding.
;
IDIV:			CALL    FLOAT2
FDIV:			DEC     B               ;TEST FOR ZERO
        		INC     B
        		LD      A,#DIVBY0
        		JP      Z,FPERROR         ;"Division by zero"
        		DEC     C               ;TEST FOR ZERO
        		INC     C
        		RET     Z
        		LD      A,H
        		XOR     D               ;CALC. RESULT SIGN
        		EX      AF,AF'          ;SAVE SIGN
        		SET     7,D             ;REPLACE IMPLIED 1's
        		SET     7,H
        		PUSH    BC              ;SAVE EXPONENTS
        		LD      B,D             ;LOAD REGISTERS
        		LD      C,E
        		LD      DE,#0
        		EXX
        		LD      B,D
        		LD      C,E
        		LD      DE,#0
        		LD      A,#-32           ;LOOP COUNTER
        		CALL    DIVA            ;DIVIDE
        		EXX
        		BIT     7,D
        		EXX
        		CALL    Z,DIVB          ;NORMALISE & INC A
        		EX      DE,HL
        		EXX
        		SRL     B               ;DIVISOR/2
        		RR      C
        		OR      A               ;CLEAR CARRY
        		SBC     HL,BC           ;REMAINDER-DIVISOR/2
        		CCF
        		EX      DE,HL           ;RESULT IN HLH'L'
        		CALL    Z,ODD           ;ROUND UNBIASSED
        		CALL    C,FPADD1          ;ROUND UP
        		POP     BC              ;RESTORE EXPONENTS
        		CALL    C,INCC
        		RRA                     ;LSB OF A TO CARRY
        		LD      A,C             ;COMPUTE NEW EXPONENT
        		SBC     A,B
        		CCF
        		JP      CHKOVF
;
;IMUL - Integer multiplication.
;
IMUL:			LD      A,H
        		XOR     D
        		EX      AF,AF'          ;SAVE RESULT SIGN
        		BIT     7,H
        		CALL    NZ,FPNEGATE
        		CALL    FPSWAP
        		BIT     7,H
        		CALL    NZ,FPNEGATE
        		LD      B,H
        		LD      C,L
        		LD      HL,#0
        		EXX
        		LD      B,H
        		LD      C,L
        		LD      HL,#0
        		LD      A,#-33
        		CALL    MULA            ;MULTIPLY
        		EXX
        		LD      C,#191           ;PRESET EXPONENT
        		CALL    FPTEST            ;TEST RANGE
        		JR      NZ,IMUL1        ;TOO BIG
        		BIT     7,D
        		JR      NZ,IMUL1
        		CALL    FPSWAP
        		LD      C,D             ;INTEGER MARKER
        		EX      AF,AF'
        		RET     P
        		JP      FPNEGATE
;
IMUL1:			DEC     C
        		EXX
        		SLA     E
        		RL      D
        		EXX
        		RL      E
        		RL      D
        		EXX
        		ADC     HL,HL
        		EXX
        		ADC     HL,HL
        		JP      P,IMUL1         ;NORMALISE
        		EX      AF,AF'
        		RET     M
        		RES     7,H             ;POSITIVE
        		RET
;
;FMUL - Floating point multiplication with rounding.
;
FMUL:			DEC     B               ;TEST FOR ZERO
        		INC     B
        		JP      Z,ZERO
        		DEC     C               ;TEST FOR ZERO
        		INC     C
        		RET     Z
        		LD      A,H
        		XOR     D               ;CALC. RESULT SIGN
        		EX      AF,AF'
        		SET     7,D             ;REPLACE IMPLIED 1's
        		SET     7,H
        		PUSH    BC              ;SAVE EXPONENTS
        		LD      B,H             ;LOAD REGISTERS
        		LD      C,L
        		LD      HL,#0
        		EXX
        		LD      B,H
        		LD      C,L
        		LD      HL,#0
        		LD      A,#-32           ;LOOP COUNTER
        		CALL    MULA            ;MULTIPLY
        		CALL    C,MULB          ;NORMALISE & INC A
        		EXX
        		PUSH    HL
        		LD      HL,#0x8000
        		OR      A               ;CLEAR CARRY
        		SBC     HL,DE
        		POP     HL
        		CALL    Z,ODD           ;ROUND UNBIASSED
        		CALL    C,FPADD1          ;ROUND UP
        		POP     BC              ;RESTORE EXPONENTS
        		CALL    C,INCC
        		RRA                     ;LSB OF A TO CARRY
        		LD      A,C             ;COMPUTE NEW EXPONENT
        		ADC     A,B
CHKOVF:			JR      C,CHKO1
        		JP      P,ZERO          ;UNDERFLOW
        		JR      CHKO2
CHKO1:			JP      M,OFLOW         ;OVERFLOW
CHKO2:			ADD     A,#0x80
        		LD      C,A
        		JP      Z,ZERO
        		EX      AF,AF'          ;RESTORE SIGN BIT
        		RES     7,H
        		RET     P
        		SET     7,H
        		RET
;
;IPOW - Integer involution.
;
IPOW:			CALL    FPSWAP
        		BIT     7,H
        		PUSH    AF              ;SAVE SIGN
        		CALL    NZ,FPNEGATE
IPOW0:			LD      C,B
        		LD      B,#32            ;LOOP COUNTER
IPOW1:			CALL    X2
        		JR      C,IPOW2
        		DJNZ    IPOW1
        		POP     AF
        		EXX
        		INC     L               ;RESULT=1
        		EXX
        		LD      C,H
        		RET
;
IPOW2:			POP     AF
        		PUSH    BC
        		EX      DE,HL
        		PUSH    HL
        		EXX
        		EX      DE,HL
        		PUSH    HL
        		EXX
        		LD      IX,#0
        		ADD     IX,SP
        		JR      Z,IPOW4
        		PUSH    BC
        		EXX
        		PUSH    DE
        		EXX
        		PUSH    DE
        		CALL    FPSFLOAT
        		CALL    RECIP
        		LD      +4(IX),C
        		EXX
        		LD      +0(IX),L
        		LD      +1(IX),H
        		EXX
        		LD      +2(IX),L
        		LD      +3(IX),H
        		JR      IPOW5
;
IPOW3:			PUSH    BC
        		EXX
        		SLA     E
        		RL      D
        		PUSH    DE
        		EXX
        		RL      E
        		RL      D
        		PUSH    DE
        		LD      A,#'*' & 0x0F
        		PUSH    AF
        		CALL    COPY
        		CALL    OP              ;SQUARE
        		POP     AF
        		CALL    FPDLOAD5
        		CALL    C,OP            ;MULTIPLY BY X
IPOW5:			POP     DE
        		EXX
        		POP     DE
        		EXX
        		LD      A,C
        		POP     BC
        		LD      C,A
IPOW4:			DJNZ    IPOW3
        		POP     AF
        		POP     AF
        		POP     AF
        		RET
;
FPOW0:			POP     AF
        		POP     AF
        		POP     AF
        		JR      IPOW0
;
;FPOW - Floating-point involution.
;
FPOW:			BIT     7,D
        		PUSH    AF
        		CALL    FPSWAP
        		CALL    PUSH5
        		DEC     C
        		INC     C
        		JR      Z,FPOW0
        		LD      A,#158
        		CP      C
        		JR      C,FPOW1
        		INC     A
        		CALL    FIX
        		EX      AF,AF'
        		JP      P,FPOW0
FPOW1:			CALL    FPSWAP
        		CALL    LN0
        		CALL    POP5
        		POP     AF
        		CALL    FMUL
        		JP      EXP0
;
;Integer and floating-point compare.
;Result is TRUE (-1) or FALSE (0).
;
FLT:			CALL    FCP
        		JR      ILT1
ILT:			CALL    ICP
ILT1:			RET     NC
        		JR      FPTRUE
;
FGT:			CALL    FCP
        		JR      IGT1
IGT:			CALL    ICP
IGT1:			RET     Z
        		RET     C
        		JR      FPTRUE
;
FGE:			CALL    FCP
        		JR      IGE1
IGE:			CALL    ICP
IGE1:			RET     C
        		JR      FPTRUE
;
FLE:			CALL    FCP
        		JR      ILE1
ILE:			CALL    ICP
ILE1:			JR      Z,FPTRUE
        		RET     NC
        		JR      FPTRUE
;
FNE:			CALL    FCP
        		JR      INE1
INE:			CALL    ICP
INE1:			RET     Z
        		JR      FPTRUE
;
FEQ:			CALL    FCP
        		JR      IEQ1
IEQ:			CALL    ICP
IEQ1:			RET     NZ
FPTRUE:			LD      HL,#-1
        		EXX
        		LD      HL,#-1
        		EXX
        		XOR     A
        		LD      C,A
        		RET
;
;FUNCTIONS:
;
;Result returned in HLH'L'C (floating point)
;Result returned in HLH'L' (C=0) (integer)
;All registers except IY destroyed.
;
;ABS - Absolute value
;Result is numeric, variable type.
;
FPABS:			BIT     7,H
        		RET     Z               ;POSITIVE/ZERO
        		DEC     C
        		INC     C
        		JP      Z,FPNEGATE        ;INTEGER
        		RES     7,H
        		RET
;
;NOT - Complement integer.
;Result is integer numeric.
;
FPNOTK:			CALL    FPSFIX
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
        		EXX
        		XOR     A               ;NUMERIC MARKER
        		RET
;
;PI - Return PI (3.141592654)
;Result is floating-point numeric.
;
FPPI:			LD      HL,#0x490F
        		EXX
        		LD      HL,#0xDAA2
        		EXX
        		LD      C,#0x81
        		XOR     A               ;NUMERIC MARKER
        		RET
;
;DEG - Convert radians to degrees
;Result is floating-point numeric.
;
FPDEG:			CALL    FPI180
        		CALL    FMUL
        		XOR     A
        		RET
;
;RAD - Convert degrees to radians
;Result is floating-point numeric.
;
FPRAD:			CALL    FPI180
        		CALL    FDIV
        		XOR     A
        		RET
;
;180/PI
;
FPI180:			CALL    FPSFLOAT
        		LD      DE,#0x652E
        		EXX
        		LD      DE,#0xE0D3
        		EXX
        		LD      B,#0x85
        		RET
;
;SGN - Return -1, 0 or +1
;Result is integer numeric.
;
FPSGN:			CALL    FPTEST
        		OR      C
        		RET     Z               ;ZERO
        		BIT     7,H
        		JP      NZ,FPTRUE         ;-1
        		CALL    ZERO
        		JP      FPADD1            ;1
;
;VAL - Return numeric value of string.
;Input: ASCII string at IX
;Result is variable type numeric.
;
FPVAL:			CALL    SIGNQ
        		PUSH    AF
        		CALL    FPCON
        		POP     AF
        		CP      #'-'
        		LD      A,#0             ;NUMERIC MARKER
        		RET     NZ
        		DEC     C
        		INC     C
        		JP      Z,FPNEGATE        ;ZERO/INTEGER
        		LD      A,H
        		XOR     #0x80             ;CHANGE SIGN (FP)
        		LD      H,A
        		XOR     A
        		RET
;
;INT - Floor function
;Result is integer numeric.
;
FPINT:			DEC     C
        		INC     C
        		RET     Z               ;ZERO/INTEGER
        		LD      A,#159
        		LD      B,H             ;B7=SIGN BIT
        		CALL    FIX
        		EX      AF,AF'
        		AND     B
        		CALL    M,FPADD1          ;NEGATIVE NON-INTEGER
        		LD      A,B
        		OR      A
        		CALL    M,FPNEGATE
        		XOR     A
        		LD      C,A
        		RET
;
;SQR - square root
;Result is floating-point numeric.
;
FPSQR:			CALL    FPSFLOAT
SQR0:			BIT     7,H
        		LD      A,#NGROOT
        		JP      NZ,FPERROR        ;"-ve root"
        		DEC     C
        		INC     C
        		RET     Z               ;ZERO
        		SET     7,H             ;IMPLIED 1
        		BIT     0,C
        		CALL    Z,DIV2          ;MAKE EXPONENT ODD
        		LD      A,C
        		SUB     #0x80
        		SRA     A               ;HALVE EXPONENT
        		ADD     A,#0x80
        		LD      C,A
        		PUSH    BC              ;SAVE EXPONENT
        		EX      DE,HL
        		LD      HL,#0
        		LD      B,H
        		LD      C,L
        		EXX
        		EX      DE,HL
        		LD      HL,#0
        		LD      B,H
        		LD      C,L
        		LD      A,#-31
        		CALL    SQRA            ;ROOT
        		EXX
        		BIT     7,B
        		EXX
        		CALL    Z,SQRA          ;NORMALISE & INC A
        		CALL    SQRB
        		OR      A               ;CLEAR CARRY
        		CALL    DIVB
        		RR      E               ;LSB TO CARRY
        		LD      H,B
        		LD      L,C
        		EXX
        		LD      H,B
        		LD      L,C
        		CALL    C,FPADD1          ;ROUND UP
        		POP     BC              ;RESTORE EXPONENT
        		CALL    C,INCC
        		RRA
        		SBC     A,A
        		ADD     A,C
        		LD      C,A
        		RES     7,H             ;POSITIVE
        		XOR     A
        		RET
;
;TAN - Tangent function
;Result is floating-point numeric.
;
FPTAN:			CALL    FPSFLOAT
        		CALL    PUSH5
        		CALL    COS0
        		CALL    POP5
        		CALL    PUSH5
        		CALL    FPSWAP
        		CALL    SIN0
        		CALL    POP5
        		CALL    FDIV
        		XOR     A               ;NUMERIC MARKER
        		RET
;
;COS - Cosine function
;Result is floating-point numeric.
;
FPCOS:			CALL    FPSFLOAT
COS0:			CALL    SCALE
        		INC     E
        		INC     E
        		LD      A,E
        		JR      SIN1
;
;SIN - Sine function
;Result is floating-point numeric.
;
FPSIN:			CALL    FPSFLOAT
SIN0:			PUSH    HL              ;H7=SIGN
        		CALL    SCALE
        		POP     AF
        		RLCA
        		RLCA
        		RLCA
        		AND     #4
        		XOR     E
SIN1:			PUSH    AF              ;OCTANT
        		RES     7,H
        		RRA
        		CALL    PIBY4
        		CALL    C,RSUB          ;X=(PI/4)-X
        		POP     AF
        		PUSH    AF
        		AND     #3
        		JP      PO,SIN2         ;USE COSINE APPROX.
        		CALL    PUSH5           ;SAVE X
        		CALL    SQUARE          ;PUSH X*X
        		CALL    POLY
        		.WORD    0xA8B7          ;a(8)
        		.WORD    0x3611
        		.BYTE    0x6D
        		.WORD    0xDE26          ;a(6)
        		.WORD    0xD005
        		.BYTE    0x73
        		.WORD    0x80C0           ;a(4)
        		.WORD    0x888
        		.BYTE    0x79
        		.WORD    0xAA9D          ;a(2)
        		.WORD    0xAAAA
        		.BYTE    0x7D
        		.WORD    0               ;a(0)
        		.WORD    0
        		.BYTE    0x80
        		CALL    POP5
        		CALL    POP5
        		CALL    FMUL
        		JP      SIN3
;
SIN2:			CALL    SQUARE          ;PUSH X*X
        		CALL    POLY
        		.WORD    0xD571          ;b(8)
        		.WORD    0x4C78
        		.BYTE    0x70
        		.WORD    0x94AF           ;b(6)
        		.WORD    0xB603
        		.BYTE    0x76
        		.WORD    0x9CC8           ;b(4)
        		.WORD    0x2AAA
        		.BYTE    0x7B
        		.WORD    0xFFDD          ;b(2)
        		.WORD    0xFFFF
        		.BYTE    0x7E
        		.WORD    0               ;b(0)
        		.WORD    0
        		.BYTE    0x80
        		CALL    POP5
SIN3:			POP     AF
        		AND     #4
        		RET     Z
        		DEC     C
        		INC     C
        		RET     Z               ;ZERO
        		SET     7,H             ;MAKE NEGATIVE
        		RET
;
;Floating-point one:
;
FONE:			LD      HL,#0
        		EXX
        		LD      HL,#0
        		EXX
        		LD      C,#0x80
        		RET
;
DONE:			LD      DE,#0
        		EXX
        		LD      DE,#0
        		EXX
        		LD      B,#0x80
        		RET
;
PIBY4:			LD      DE,#0x490F
        		EXX
        		LD      DE,#0xDAA2
        		EXX
        		LD      B,#0x7F
        		RET
;
;EXP - Exponential function
;Result is floating-point numeric.
;
FPEXP:			CALL    FPSFLOAT
EXP0:			CALL    LN2             ;LN(2)
        		EXX
	        	DEC     E
		        LD      BC,#0xD1CF       ;0.6931471805599453
        		EXX
        		PUSH    HL              ;H7=SIGN
        		CALL    MOD48           ;"MODULUS"
        		POP     AF
        		BIT     7,E
        		JR      Z,EXP1
        		RLA
        		JP      C,ZERO
        		LD      A,#EXPRNG
        		JP      FPERROR           ;"Exp range"
;
EXP1:			AND     #0x80
        		OR      E
        		PUSH    AF              ;INTEGER PART
        		RES     7,H
        		CALL    PUSH5           ;PUSH X*LN(2)
        		CALL    POLY
        		.WORD    0x4072           ;a(7)
        		.WORD    0x942E
        		.BYTE    0x73
        		.WORD    0x6F65           ;a(6)
        		.WORD    0x2E4F
        		.BYTE    0x76
        		.WORD    0x6D37           ;a(5)
        		.WORD    0x8802
        		.BYTE    0x79
        		.WORD    0xE512          ;a(4)
        		.WORD    0x2AA0
        		.BYTE    0x7B
        		.WORD    0x4F14           ;a(3)
        		.WORD    0xAAAA
        		.BYTE    0x7D
        		.WORD    0xFD56          ;a(2)
        		.WORD    0x7FFF
        		.BYTE    0x7E
        		.WORD    0xFFFE          ;a(1)
        		.WORD    0xFFFF
        		.BYTE    0x7F
        		.WORD    0               ;a(0)
        		.WORD    0
        		.BYTE    0x80
        		CALL    POP5
        		POP     AF
        		PUSH    AF
        		CALL    P,RECIP         ;X=1/X
        		POP     AF
        		JP      P,EXP4
        		AND     #0x7F
        		NEG
EXP4:			ADD     A,#0x80
        		ADD     A,C
        		JR      C,EXP2
        		JP      P,ZERO          ;UNDERFLOW
        		JR      EXP3
EXP2:			JP      M,OFLOW         ;OVERFLOW
EXP3:			ADD     A,#0x80
        		JP      Z,ZERO
        		LD      C,A
        		XOR     A               ;NUMERIC MARKER
        		RET
;
RECIP:			CALL    DONE
RDIV:			CALL    FPSWAP
        		JP      FDIV            ;RECIPROCAL
;
LN2:			LD      DE,#0x3172        ;LN(2)
        		EXX
        		LD      DE,#0x17F8
        		EXX
        		LD      B,#0x7F
        		RET
;
;LN - Natural log.
;Result is floating-point numeric.
;
FPLN:			CALL    FPSFLOAT
LN0:			LD      A,#LOGRNG
        		BIT     7,H
        		JP      NZ,FPERROR        ;"Log range"
        		INC     C
        		DEC     C
        		JP      Z,FPERROR
        		LD      DE,#0x3504        ;SQR(2)
        		EXX
        		LD      DE,#0xF333       ;1.41421356237
        		EXX
        		CALL    ICP0            ;MANTISSA>SQR(2)?
        		LD      A,C             ;EXPONENT
        		LD      C,#0x80           ;1 <= X < 2
        		JR      C,LN4
        		DEC     C
        		INC     A
LN4:			PUSH    AF              ;SAVE EXPONENT
        		CALL    RATIO           ;X=(X-1)/(X+1)
        		CALL    PUSH5
		        CALL    SQUARE          ;PUSH X*X
        		CALL    POLY
        		.WORD    0xCC48          ;a(9)
        		.WORD    0x74FB
        		.BYTE    0x7D
        		.WORD    0xAEAF          ;a(7)
        		.WORD    0x11FF
        		.BYTE    0x7E
        		.WORD    0xD98C          ;a(5)
        		.WORD    0x4CCD
        		.BYTE    0x7E
        		.WORD    0xA9E3          ;a(3)
        		.WORD    0x2AAA
        		.BYTE    0x7F
        		.WORD    0               ;a(1)
        		.WORD    0
        		.BYTE    0x81
        		CALL    POP5
        		CALL    POP5
        		CALL    FMUL
        		POP     AF              ;EXPONENT
        		CALL    PUSH5
        		EX      AF,AF'
        		CALL    ZERO
        		EX      AF,AF'
        		SUB     #0x80
        		JR      Z,LN3
        		JR      NC,LN1
        		CPL
        		INC     A
LN1:			LD      H,A
        		LD      C,#0x87
        		PUSH    AF
        		CALL    FLOAT
        		RES     7,H
        		CALL    LN2
        		CALL    FMUL
        		POP     AF
        		JR      NC,LN3
        		JP      M,LN3
        		SET     7,H
LN3:			CALL    POP5
        		CALL    FADD
        		XOR     A
        		RET
;
;LOG - base-10 logarithm.
;Result is floating-point numeric.
;
FPLOG:			CALL    FPLN
        		LD      DE,#0x5E5B        ;LOG(e)
        		EXX
        		LD      DE,#0xD8A9
        		EXX
        		LD      B,#0x7E
        		CALL    FMUL
        		XOR     A
        		RET
;
;ASN - Arc-sine
;Result is floating-point numeric.
;
FPASN:			CALL    FPSFLOAT
        		CALL    PUSH5
        		CALL    COPY
        		CALL    FMUL
        		CALL    DONE
        		CALL    RSUB
        		CALL    SQR0
        		CALL    POP5
        		INC     C
        		DEC     C
        		LD      A,#2
        		PUSH    DE
        		JR      Z,ACS1
        		POP     DE
        		CALL    RDIV
        		JR      ATN0
;
;ATN - arc-tangent
;Result is floating-point numeric.
;
FPATN:			CALL    FPSFLOAT
ATN0:			PUSH    HL              ;SAVE SIGN
        		RES     7,H
        		LD      DE,#0x5413        ;TAN(PI/8)=SQR(2)-1
        		EXX
        		LD      DE,#0xCCD0
        		EXX
        		LD      B,#0x7E
        		CALL    FCP0            ;COMPARE
        		LD      B,#0
        		JR      C,ATN2
        		LD      DE,#0x1A82        ;TAN(3*PI/8)=SQR(2)+1
        		EXX
        		LD      DE,#0x799A
        		EXX
        		LD      B,#0x81
        		CALL    FCP0            ;COMPARE
        		JR      C,ATN1
        		CALL    RECIP           ;X=1/X
        		LD      B,#2
        		JP      ATN2
ATN1:			CALL    RATIO           ;X=(X-1)/(X+1)
        		LD      B,#1
ATN2:			PUSH    BC              ;SAVE FLAG
        		CALL    PUSH5
        		CALL    SQUARE          ;PUSH X*X
        		CALL    POLY
        		.WORD    0xF335          ;a(13)
        		.WORD    0x37D8
        		.BYTE    0x7B
        		.WORD    0x6B91           ;a(11)
        		.WORD    0xAAB9
        		.BYTE    0x7C
        		.WORD    0x41DE           ;a(9)
        		.WORD    0x6197
        		.BYTE    0x7C
        		.WORD    0x9D7B           ;a(7)
        		.WORD    0x9237
        		.BYTE    0x7D
        		.WORD    0x2A5A           ;a(5)
        		.WORD    0x4CCC
        		.BYTE    0x7D
        		.WORD    0xA95C          ;a(3)
        		.WORD    0xAAAA
        		.BYTE    0x7E
        		.WORD    0               ;a(1)
        		.WORD    0
        		.BYTE    0x80
        		CALL    POP5
        		CALL    POP5
        		CALL    FMUL
        		POP     AF
ACS1:			CALL    PIBY4           ;PI/4
        		RRA
        		PUSH    AF
        		CALL    C,FADD
        		POP     AF
        		INC     B
        		RRA
        		CALL    C,RSUB
        		POP     AF
        		OR      A
        		RET     P
        		SET     7,H             ;MAKE NEGATIVE
        		XOR     A
        		RET
;
;ACS - Arc cosine=PI/2-ASN.
;Result is floating point numeric.
;
FPACS:			CALL    FPASN
        		LD      A,#2
        		PUSH    AF
        		JR      ACS1
;
;Function STR - convert numeric value to ASCII string.
;   Inputs: HLH'L'C = integer or floating-point number
;           DE = address at which to store string
;           IX = address of @% format control
;  Outputs: String stored, with NUL terminator
;
;First normalise for decimal output:
;
FPSTR:			CALL    FPSFLOAT
        		LD      B,#0             ;DEFAULT PT. POSITION
        		BIT     7,H             ;NEGATIVE?
        		JR      Z,STR10
        		RES     7,H
        		LD      A,#'-'
        		LD      (DE),A          ;STORE SIGN
        		INC     DE
STR10:			XOR     A               ;CLEAR A
        		CP      C
        		JR      Z,FPSTR2          ;ZERO
        		PUSH    DE              ;SAVE TEXT POINTER
        		LD      A,B
STR11:			PUSH    AF              ;SAVE DECIMAL COUNTER
        		LD      A,C             ;BINARY EXPONENT
        		CP      #161
        		JR      NC,STR14
        		CP      #155
        		JR      NC,STR15
        		CPL
        		CP      #225
        		JR      C,STR13
        		LD      A,#-8
STR13:			ADD     A,#28
        		CALL    POWR10
        		PUSH    AF
        		CALL    FMUL
        		POP     AF
        		LD      B,A
        		POP     AF
        		SUB     B
        		JR      STR11
STR14:			SUB     #32
        		CALL    POWR10
        		PUSH    AF
        		CALL    FDIV
        		POP     AF
        		LD      B,A
        		POP     AF
        		ADD     A,B
        		JR      STR11
STR15:			LD      A,#9
        		CALL    POWR10          ;10^9
        		CALL    FCP0
        		LD      A,C
        		POP     BC
        		LD      C,A
        		SET     7,H             ;IMPLIED 1
        		CALL    C,X10B          ;X10, DEC B
        		POP     DE              ;RESTORE TEXT POINTER
        		RES     7,C
        		LD      A,#0
        		RLA                     ;PUT CARRY IN LSB
;
;At this point decimal normalisation has been done,
;now convert to decimal digits:
;      AHLH'L' = number in normalised integer form
;            B = decimal place adjustment
;            C = binary place adjustment (29-33)
;
FPSTR2:			INC     C
        		EX      AF,AF'          ;SAVE A
        		LD      A,B
        		BIT     1,+2(IX)
        		JR      NZ,STR20
        		XOR     A
        		CP      +1(IX)
        		JR      Z,STR21
        		LD      A,#-10
STR20:			ADD     A,+1(IX)        ;SIG. FIG. COUNT
        		OR      A               ;CLEAR CARRY
        		JP      M,STR21
        		XOR     A
STR21:			PUSH    AF
        		EX      AF,AF'          ;RESTORE A
STR22:			CALL    X2              ;RL AHLH'L'
        		ADC     A,A
        		CP      #10
        		JR      C,STR23
        		SUB     #10
        		EXX
        		INC     L               ;SET RESULT BIT
        		EXX
STR23:			DEC     C
        		JR      NZ,STR22        ;32 TIMES
        		LD      C,A             ;REMAINDER
        		LD      A,H
        		AND     #0x3F             ;CLEAR OUT JUNK
        		LD      H,A
        		POP     AF
        		JP      P,STR24
        		INC     A
        		JR      NZ,STR26
        		LD      A,#4
        		CP      C               ;ROUND UP?
        		LD      A,#0
        		JR      STR26
STR24:			PUSH    AF
        		LD      A,C
        		ADC     A,#'0'           ;ADD CARRY
        		CP      #'0'
        		JR      Z,STR25         ;SUPPRESS ZERO
        		CP      #'9'+1
        		CCF
        		JR      NC,STR26
STR25:			EX      (SP),HL
        		BIT     6,L             ;ZERO FLAG
		        EX      (SP),HL
        		JR      NZ,STR27
        		LD      A,#'0'
STR26:			INC     A               ;SET +VE
        		DEC     A
        		PUSH    AF              ;PUT ON STACK + CARRY
STR27:			INC     B
        		CALL    FPTEST            ;IS HLH'L' ZERO?
        		LD      C,#32
        		LD      A,#0
        		JR      NZ,STR22
        		POP     AF
        		PUSH    AF
        		LD      A,#0
        		JR      C,STR22
;
;At this point, the decimal character string is stored
; on the stack. Trailing zeroes are suppressed and may
; need to be replaced.
;B register holds decimal point position.
;Now format number and store as ASCII string:
;
STR3:			EX      DE,HL           ;STRING POINTER
        		LD      C,#-1            ;FLAG "E"
        		LD      D,#1
        		LD      E,+1(IX)        ;f2
        		BIT     0,+2(IX)
        		JR      NZ,STR34        ;E MODE
        		BIT     1,+2(IX)
        		JR      Z,STR31
        		LD      A,B             ;F MODE
        		OR      A
        		JR      Z,STR30
        		JP      M,STR30
        		LD      D,B
STR30:			LD      A,D
        		ADD     A,+1(IX)
        		LD      E,A
        		CP      #11
        		JR      C,STR32
STR31:			LD      A,B             ;G MODE
        		LD      DE,#0x0101
        		OR      A
        		JP      M,STR34
        		JR      Z,STR32
        		LD      A,+1(IX)
        		OR      A
        		JR      NZ,STR3A
        		LD      A,#10
STR3A:			CP      B
        		JR      C,STR34
        		LD      D,B
        		LD      E,B
STR32:			LD      A,B
        		ADD     A,#129
        		LD      C,A
STR34:			SET     7,D
        		DEC     E
STR35:			LD      A,D
        		CP      C
        		JR      NC,STR33
STR36:			POP     AF
        		JR      Z,STR37
        		JP      P,STR38
STR37:			PUSH    AF
        		INC     E
        		DEC     E
        		JP      M,STR4
STR33:			LD      A,#'0'
STR38:			DEC     D
        		JP      PO,STR39
        		LD      (HL),#'.'
        		INC     HL
STR39:			LD      (HL),A
        		INC     HL
        		DEC     E
        		JP      P,STR35
        		JR      STR36
;
STR4:			POP     AF
STR40:			INC     C
        		LD      C,L
        		JR      NZ,STR44
        		LD      (HL),#'E'        ;EXPONENT
        		INC     HL
        		LD      A,B
        		DEC     A
        		JP      P,STR41
        		LD      (HL),#'-'
        		INC     HL
        		NEG
STR41:			LD      (HL),#'0'
        		JR      Z,STR47
        		CP      #10
        		LD      B,A
        		LD      A,#':'
        		JR      C,STR42
        		INC     HL
        		LD      (HL),#'0'
STR42:			INC     (HL)
        		CP      (HL)
        		JR      NZ,STR43
        		LD      (HL),#'0'
        		DEC     HL
        		INC     (HL)
        		INC     HL
STR43:			DJNZ    STR42
STR47:			INC     HL
STR44:			EX      DE,HL
      			RET
;
;Support subroutines:
;
FPDLOAD5:			LD      B,+4(IX)
        		EXX
        		LD      E,+0(IX)
        		LD      D,+1(IX)
        		EXX
        		LD      E,+2(IX)
        		LD      D,+3(IX)
        		RET
;
;CON - Get unsigned numeric constant from ASCII string.
;   Inputs: ASCII string at (IX).
;  Outputs: Variable-type result in HLH'L'C
;           IX updated (points to delimiter)
;           A7 = 0 (numeric marker)
;
FPCON:			CALL    ZERO            ;INITIALISE TO ZERO
        		LD      C,#0             ;TRUNCATION COUNTER
        		CALL    FPNUMBER          ;GET INTEGER PART
        		CP      #'.'
        		LD      B,#0             ;DECL. PLACE COUNTER
        		CALL    Z,NUMBIX        ;GET FRACTION PART
        		CP      #'E'
        		LD      A,#0             ;INITIALISE EXPONENT
        		CALL    Z,GETEXP        ;GET EXPONENT
        		BIT     7,H
        		JR      NZ,CON0         ;INTEGER OVERFLOW
        		OR      A
        		JR      NZ,CON0         ;EXPONENT NON-ZERO
        		CP      B
        		JR      NZ,CON0         ;DECIMAL POINT
        		CP      C
        		RET     Z               ;INTEGER
CON0:			SUB     B
        		ADD     A,C
        		LD      C,#159
        		CALL    FLOAT
        		RES     7,H             ;DITCH IMPLIED 1
        		OR      A
        		RET     Z               ;DONE
        		JP      M,FPCON2          ;NEGATIVE EXPONENT
        		CALL    POWR10
        		CALL    FMUL            ;SCALE
        		XOR     A
        		RET
FPCON2:			CP      #-38
        		JR      C,CON3          ;CAN'T SCALE IN ONE GO
        		NEG
        		CALL    POWR10
        		CALL    FDIV            ;SCALE
        		XOR     A
        		RET
CON3:			PUSH    AF
        		LD      A,#38
        		CALL    POWR10
        		CALL    FDIV
        		POP     AF
        		ADD     A,#38
        		JR      FPCON2
;
;GETEXP - Get decimal exponent from string
;     Inputs: ASCII string at (IX)
;             (IX points at 'E')
;             A = initial value
;    Outputs: A = new exponent
;             IX updated.
;   Destroys: A,A',IX,F,F'
;
GETEXP:			PUSH    BC              ;SAVE REGISTERS
        		LD      B,A             ;INITIAL VALUE
        		LD      C,#2             ;2 DIGITS MAX
        		INC     IX              ;BUMP PAST 'E'
        		CALL    SIGNQ
        		EX      AF,AF'          ;SAVE EXPONENT SIGN
GETEX1:			CALL    DIGITQ
        		JR      C,GETEX2
        		LD      A,B             ;B=B*10
        		ADD     A,A
        		ADD     A,A
        		ADD     A,B
        		ADD     A,A
        		LD      B,A
        		LD      A,(IX)          ;GET BACK DIGIT
        		INC     IX
        		AND     #0x0F             ;MASK UNWANTED BITS
        		ADD     A,B             ;ADD IN DIGIT
        		LD      B,A
        		DEC     C
        		JP      P,GETEX1
        		LD      B,#100           ;FORCE OVERFLOW
        		JR      GETEX1
GETEX2:			EX      AF,AF'          ;RESTORE SIGN
        		CP      #'-'
        		LD      A,B
        		POP     BC              ;RESTORE
        		RET     NZ
        		NEG                     ;NEGATE EXPONENT
        		RET
;
;NUMBER: Get unsigned integer from string.
;    Inputs: string at (IX)
;            C = truncated digit count
;                (initially zero)
;            B = total digit count
;            HLH'L' = initial value
;   Outputs: HLH'L' = number (binary integer)
;            A = delimiter.
;            B, C & IX updated
;  Destroys: A,B,C,D,E,H,L,B',C',D',E',H',L',IX,F
;
NUMBIX:			INC     IX
FPNUMBER:			CALL    DIGITQ
        		RET     C
        		INC     B               ;INCREMENT DIGIT COUNT
        		INC     IX
        		CALL    X10             ;*10 & COPY OLD VALUE
        		JR      C,NUMB1         ;OVERFLOW
        		DEC     C               ;SEE IF TRUNCATED
        		INC     C
        		JR      NZ,NUMB1        ;IMPORTANT!
        		AND     #0x0F
        		EXX
        		LD      B,#0
        		LD      C,A
        		ADD     HL,BC           ;ADD IN DIGIT
        		EXX
        		JR      NC,FPNUMBER
        		INC     HL              ;CARRY
        		LD      A,H
        		OR      L
        		JR      NZ,FPNUMBER
NUMB1:			INC     C               ;TRUNCATION COUNTER
        		CALL    SWAP1           ;RESTORE PREVIOUS VALUE
        		JR      FPNUMBER
;
;FIX - Fix number to specified exponent value.
;    Inputs: HLH'L'C = +ve non-zero number (floated)
;            A = desired exponent (A>C)
;   Outputs: HLH'L'C = fixed number (unsigned)
;            fraction shifted into B'C'
;            A'F' positive if integer input
;  Destroys: C,H,L,A',B',C',H',L',F,F'
;
FIX:			EX      AF,AF'
        		XOR     A
        		EX      AF,AF'
        		SET     7,H             ;IMPLIED 1
FIX1:			CALL    DIV2
        		CP      C
        		RET     Z
        		JP      NC,FIX1
        		JP      OFLOW
;
;SFIX - Convert to integer if necessary.
;    Input: Variable-type number in HLH'L'C
;   Output: Integer in HLH'L', C=0
; Destroys: A,C,H,L,A',B',C',H',L',F,F'
;
;NEGATE - Negate HLH'L'
;    Destroys: H,L,H',L',F
;
FIX2:			CALL    FPSWAP
        		CALL    FPSFIX
        		CALL    FPSWAP
FPSFIX:			DEC     C
        		INC     C
        		RET     Z               ;INTEGER/ZERO
        		BIT     7,H             ;SIGN
        		PUSH    AF
        		LD      A,#159
        		CALL    FIX
        		POP     AF
        		LD      C,#0
        		RET     Z
FPNEGATE:			OR      A               ;CLEAR CARRY
        		EXX
NEG0:			PUSH    DE
        		EX      DE,HL
        		LD      HL,#0
        		SBC     HL,DE
        		POP     DE
        		EXX
        		PUSH    DE
        		EX      DE,HL
        		LD      HL,#0
        		SBC     HL,DE
        		POP     DE
        		RET
;
;NEG - Negate HLH'L'B'C'
;    Also complements A (used in FADD)
;    Destroys: A,H,L,B',C',H',L',F
;
NEG:			EXX
        		CPL
        		PUSH    HL
        		OR      A               ;CLEAR CARRY
        		LD      HL,#0
        		SBC     HL,BC
        		LD      B,H
        		LD      C,L
        		POP     HL
        		JR      NEG0
;
;SCALE - Trig scaling.
;MOD48 - 48-bit floating-point "modulus" (remainder).
;   Inputs: HLH'L'C unsigned floating-point dividend
;           DED'E'B'C'B unsigned 48-bit FP divisor
;  Outputs: HLH'L'C floating point remainder (H7=1)
;           E = quotient (bit 7 is sticky)
; Destroys: A,B,C,D,E,H,L,B',C',D',E',H',L',IX,F
;FLO48 - Float unsigned number (48 bits)
;    Input/output in HLH'L'B'C'C
;   Destroys: C,H,L,B',C',H',L',F
;
SCALE:			LD      A,#150
        		CP      C
        		LD      A,#ACLOST
        		JP      C,FPERROR         ;"Accuracy lost"
        		CALL    PIBY4
        		EXX
        		LD      BC,#0x2169        ;3.141592653589793238
        		EXX
MOD48:			SET     7,D             ;IMPLIED 1
        		SET     7,H
        		LD      A,C
        		LD      C,#0             ;INIT QUOTIENT
        		LD      IX,#0
        		PUSH    IX              ;PUT ZERO ON STACK
        		CP      B
        		JR      C,MOD485        ;DIVIDEND<DIVISOR
MOD481:			EXX                     ;CARRY=0 HERE
        		EX      (SP),HL
        		SBC     HL,BC
        		EX      (SP),HL
        		SBC     HL,DE
        		EXX
        		SBC     HL,DE
        		JR      NC,MOD482       ;DIVIDEND>=DIVISOR
        		EXX
        		EX      (SP),HL
        		ADD     HL,BC
        		EX      (SP),HL
        		ADC     HL,DE
        		EXX
        		ADC     HL,DE
MOD482:			CCF
        		RL      C               ;QUOTIENT
        		JR      NC,MOD483
        		SET     7,C             ;STICKY BIT
MOD483:			DEC     A
        		CP      B
        		JR      C,MOD484        ;DIVIDEND<DIVISOR
        		EX      (SP),HL
        		ADD     HL,HL           ;DIVIDEND * 2
        		EX      (SP),HL
        		EXX
        		ADC     HL,HL
        		EXX
        		ADC     HL,HL
        		JR      NC,MOD481       ;AGAIN
        		OR      A
        		EXX
        		EX      (SP),HL
        		SBC     HL,BC           ;OVERFLOW, SO SUBTRACT
        		EX      (SP),HL
        		SBC     HL,DE
        		EXX
        		SBC     HL,DE
        		OR      A
        		JR      MOD482
;
MOD484:			INC     A
MOD485:			LD      E,C             ;QUOTIENT
        		LD      C,A             ;REMAINDER EXPONENT
        		EXX
        		POP     BC
        		EXX
FLO48:			BIT     7,H
        		RET     NZ
        		EXX
        		SLA     C
        		RL      B
        		ADC     HL,HL
        		EXX
        		ADC     HL,HL
        		DEC     C
        		JP      NZ,FLO48
        		RET
;
;Float unsigned number
;    Input/output in HLH'L'C
;   Destroys: C,H,L,H',L',F
;
FLOAT:			BIT     7,H
        		RET     NZ
        		EXX                     ;SAME AS "X2"
        		ADD     HL,HL           ;TIME-CRITICAL
        		EXX                     ;REGION
        		ADC     HL,HL           ;(BENCHMARKS)
        		DEC     C
        		JP      NZ,FLOAT
        		RET
;
;SFLOAT - Convert to floating-point if necessary.
;    Input: Variable-type number in HLH'L'C
;    Output: Floating-point in HLH'L'C
;    Destroys: A,C,H,L,H',L',F
;
FLOATA:			EX      AF,AF'
        		ADD     A,#(RTABLE-DTABLE)/2
        		EX      AF,AF'
FLOAT2:			CALL    FPSWAP
        		CALL    FPSFLOAT
        		CALL    FPSWAP
FPSFLOAT:			DEC     C
        		INC     C
        		RET     NZ              ;ALREADY FLOATING-POINT
        		CALL    FPTEST
        		RET     Z               ;ZERO
        		LD      A,H
        		OR      A
        		CALL    M,FPNEGATE
        		LD      C,#159
        		CALL    FLOAT
        		OR      A
        		RET     M               ;NEGATIVE
        		RES     7,H
        		RET
;
;ROUND UP
;Return with carry set if 32-bit overflow
;   Destroys: H,L,B',C',H',L',F
;
FPADD1:			EXX
        		LD      BC,#1
        		ADD     HL,BC
        		EXX
        		RET     NC
        		PUSH    BC
        		LD      BC,#1
        		ADD     HL,BC
        		POP     BC
        		RET
;
;ODD - Add one if even, leave alone if odd.
; (Used to perform unbiassed rounding, i.e.
;  number is rounded up half the time)
;    Destroys: L',F (carry cleared)
;
ODD:			OR      A               ;CLEAR CARRY
        		EXX
        		SET     0,L             ;MAKE ODD
        		EXX
        		RET
;
;SWAP - Swap arguments.
;    Exchanges DE,HL D'E',H'L' and B,C
;    Destroys: A,B,C,D,E,H,L,D',E',H',L'
;SWAP1 - Swap DEHL with D'E'H'L'
;    Destroys: D,E,H,L,D',E',H',L'
;
FPSWAP:			LD      A,C
        		LD      C,B
        		LD      B,A
SWAP1:			EX      DE,HL
        		EXX
        		EX      DE,HL
        		EXX
        		RET
;
;DIV2 - destroys C,H,L,A',B',C',H',L',F,F'
;INCC - destroys C,F
;OFLOW
;
DIV2:			CALL    D2
        		EXX
        		RR      B
        		RR      C
        		EX      AF,AF'
        		OR      B
        		EX      AF,AF'
        		EXX
INCC:			INC     C
        		RET     NZ
OFLOW:			LD      A,#TOOBIG
        		JP      FPERROR           ;"Too big"
;
;FTEST - Test for zero & sign
;    Output: A=0 if zero, A=&40 if +ve, A=&C0 if -ve
;
FTEST:			CALL    FPTEST
        		RET     Z
        		LD      A,H
        		AND     #0b10000000
        		OR      #0b01000000
        		RET
;
;TEST - Test HLH'L' for zero.
;    Output: Z-flag set & A=0 if HLH'L'=0
;    Destroys: A,F
;
FPTEST:			LD      A,H
        		OR      L
        		EXX
        		OR      H
        		OR      L
        		EXX
        		RET
;
;FCOMP - Compare two numbers
;    Output: A=0 if equal, A=&40 if L>R, A=&C0 if L<R
;
FCOMP:			LD      A,B
        		OR      C               ;Both integer?
        		JR      NZ,FCOMP1
        		CALL    ICP
FCOMP0:			LD      A,#0
        		RET     Z               ;Equal
        		LD      A,#0x80
        		RRA
        		RET
;
FCOMP1:			CALL    FLOAT2          ;Float both
        		CALL    FCP
        		JR      FCOMP0
;
;Integer and floating point compare.
;Sets carry & zero flags according to HLH'L'C-DED'E'B
;Result pre-set to FALSE
;ICP1, FCP1 destroy A,F
;
;ZERO - Return zero.
; Destroys: A,C,H,L,H',L'
;
ICP:			CALL    ICP1
ZERO:			LD      A,#0
        		EXX
        		LD      H,A
        		LD      L,A
        		EXX
        		LD      H,A
        		LD      L,A
        		LD      C,A
        		RET
;
FCP:			CALL    FCP1
        		JR      ZERO            ;PRESET FALSE
;
FCP0:			LD      A,C
        		CP      B               ;COMPARE EXPONENTS
        		RET     NZ
ICP0:			SBC     HL,DE           ;COMP MANTISSA MSB
        		ADD     HL,DE
        		RET     NZ
        		EXX
        		SBC     HL,DE           ;COMP MANTISSA LSB
        		ADD     HL,DE
        		EXX
        		RET
;
FCP1:			LD      A,H
        		XOR     D
        		LD      A,H
        		RLA
        		RET     M
        		JR      NC,FCP0
        		CALL    FCP0
        		RET     Z               ;** V0.1 BUG FIX
        		CCF
        		RET
;
ICP1:			LD      A,H
        		XOR     D
        		JP      P,ICP0
        		LD      A,H
        		RLA
        		RET
;
;ADD - Integer add.
;Carry, sign & zero flags valid on exit
;    Destroys: H,L,H',L',F
;
X10B:			DEC     B
        		INC     C
X5:			CALL    COPY0
        		CALL    D2C
        		CALL    D2C
        		EX      AF,AF'          ;SAVE CARRY
ADD:			EXX
        		ADD     HL,DE
        		EXX
        		ADC     HL,DE
        		RET
;
;SUB - Integer subtract.
;Carry, sign & zero flags valid on exit
;    Destroys: H,L,H',L',F
;
SUB:			EXX
        		OR      A
        		SBC     HL,DE
        		EXX
        		SBC     HL,DE
        		RET
;
;X10 - unsigned integer * 10
;   Inputs: HLH'L' initial value
;  Outputs: DED'E' = initial HLH'L'
;           Carry bit set if overflow
;           If carry not set HLH'L'=result
; Destroys: D,E,H,L,D',E',H',L',F
;X2 - Multiply HLH'L' by 2 as 32-bit integer.
;    Carry set if MSB=1 before shift.
;    Sign set if MSB=1 after shift.
;    Destroys: H,L,H',L',F
;
X10:			CALL    COPY0           ;DED'E'=HLH'L'
        		CALL    X2
        		RET     C               ;TOO BIG
        		CALL    X2
        		RET     C
        		CALL    ADD
        		RET     C
X2:			EXX
        		ADD     HL,HL
        		EXX
        		ADC     HL,HL
        		RET
;
;D2 - Divide HLH'L' by 2 as 32-bit integer.
;    Carry set if LSB=1 before shift.
;    Destroys: H,L,H',L',F
;
D2C:			INC     C
D2:			SRL     H
        		RR      L
        		EXX
        		RR      H
        		RR      L
        		EXX
        		RET
;
;COPY - COPY HLH'L'C INTO DED'E'B
;  Destroys: B,C,D,E,H,L,D',E',H',L'
;
COPY:			LD      B,C
COPY0:			LD      D,H
        		LD      E,L
        		EXX
        		LD      D,H
        		LD      E,L
        		EXX
        		RET
;
;SQUARE - PUSH X*X
;PUSH5 - PUSH HLH'L'C ONTO STACK.
;  Destroys: SP,IX
;
SQUARE:			CALL    COPY
        		CALL    FMUL
PUSH5:			POP     IX              ;RETURN ADDRESS
        		PUSH    BC
        		PUSH    HL
        		EXX
        		PUSH    HL
        		EXX
        		JP      (IX)            ;"RETURN"
;
;POP5 - POP DED'E'B OFF STACK.
;  Destroys: A,B,D,E,D',E',SP,IX
;
POP5:			POP     IX              ;RETURN ADDRESS
        		EXX
        		POP     DE
        		EXX
        		POP     DE
        		LD      A,C
        		POP     BC
        		LD      B,C
        		LD      C,A
        		JP      (IX)            ;"RETURN"
;
;RATIO - Calculate (X-1)/(X+1)
;    Inputs: X in HLH'L'C
;   Outputs: (X-1)/(X+1) in HLH'L'C
;  Destroys: Everything except IY,SP,I
;
RATIO:			CALL    PUSH5           ;SAVE X
        		CALL    DONE
        		CALL    FADD
        		CALL    POP5            ;RESTORE X
        		CALL    PUSH5           ;SAVE X+1
        		CALL    FPSWAP
        		CALL    DONE
        		CALL    FSUB
        		CALL    POP5            ;RESTORE X+1
        		JP      FDIV
;
;POLY - Evaluate a polynomial.
;    Inputs: X in HLH'L'C and also stored at (SP+2)
;            Polynomial coefficients follow call.
;   Outputs: Result in HLH'L'C
;  Destroys: Everything except IY,SP,I
;Routine terminates on finding a coefficient >=1.
;Note: The last coefficient is EXECUTED on return
;      so must contain only innocuous bytes!
;
POLY:			LD      IX,#2
        		ADD     IX,SP
        		EX      (SP),IX
        		CALL    FPDLOAD5          ;FIRST COEFFICIENT
POLY1:			CALL    FMUL
        		LD      DE,#5
        		ADD     IX,DE
        		CALL    FPDLOAD5          ;NEXT COEFFICIENT
        		EX      (SP),IX
        		INC     B
        		DEC     B               ;TEST
        		JP      M,FADD
        		CALL    FADD
        		CALL    FPDLOAD5          ;X
        		EX      (SP),IX
        		JR      POLY1
;
;POWR10 - Calculate power of ten.
;    Inputs: A=power of 10 required (A<128)
;            A=binary exponent to be exceeded (A>=128)
;   Outputs: DED'E'B = result
;            A = actual power of ten returned
;  Destroys: A,B,D,E,A',D',E',F,F'
;
POWR10:			INC     A
        		EX      AF,AF'
        		PUSH    HL
        		EXX
        		PUSH    HL
        		EXX
        		CALL    DONE
        		CALL    FPSWAP
        		XOR     A
POWR11:			EX      AF,AF'
        		DEC     A
        		JR      Z,POWR14        ;EXIT TYPE 1
        		JP      P,POWR13
        		CP      C
        		JR      C,POWR14        ;EXIT TYPE 2
        		INC     A
POWR13:			EX      AF,AF'
        		INC     A
        		SET     7,H
        		CALL    X5
        		JR      NC,POWR12
        		EX      AF,AF'
        		CALL    D2C
        		EX      AF,AF'
POWR12:			EX      AF,AF'
        		CALL    C,FPADD1          ;ROUND UP
        		INC     C
        		JP      M,POWR11
        		JP      OFLOW
POWR14:			CALL    FPSWAP
        		RES     7,D
        		EXX
        		POP     HL
        		EXX
        		POP     HL
        		EX      AF,AF'
        		RET
;
;DIVA, DIVB - DIVISION PRIMITIVE.
;    Function: D'E'DE = H'L'HLD'E'DE / B'C'BC
;              Remainder in H'L'HL
;    Inputs: A = loop counter (normally -32)
;    Destroys: A,D,E,H,L,D',E',H',L',F
;
DIVA:			OR      A               ;CLEAR CARRY
DIV0:			SBC     HL,BC           ;DIVIDEND-DIVISOR
        		EXX
        		SBC     HL,BC
        		EXX
        		JR      NC,DIV1
        		ADD     HL,BC           ;DIVIDEND+DIVISOR
        		EXX
        		ADC     HL,BC
        		EXX
DIV1:			CCF
DIVC:			RL      E               ;SHIFT RESULT INTO DE
        		RL      D
        		EXX
        		RL      E
        		RL      D
        		EXX
        		INC     A
        		RET     P
DIVB:			ADC     HL,HL           ;DIVIDEND*2
        		EXX
        		ADC     HL,HL
        		EXX
        		JR      NC,DIV0
        		OR      A
        		SBC     HL,BC           ;DIVIDEND-DIVISOR
        		EXX
        		SBC     HL,BC
        		EXX
        		SCF
        		JP      DIVC
;
;MULA, MULB - MULTIPLICATION PRIMITIVE.
;    Function: H'L'HLD'E'DE = B'C'BC * D'E'DE
;    Inputs: A = loop counter (usually -32)
;            H'L'HL = 0
;    Destroys: D,E,H,L,D',E',H',L',A,F
;
MULA:			OR      A               ;CLEAR CARRY
MUL0:			EXX
        		RR      D               ;MULTIPLIER/2
        		RR      E
        		EXX
        		RR      D
        		RR      E
        		JR      NC,MUL1
        		ADD     HL,BC           ;ADD IN MULTIPLICAND
        		EXX
        		ADC     HL,BC
        		EXX
MUL1:			INC     A
        		RET     P
MULB:			EXX
        		RR      H               ;PRODUCT/2
        		RR      L
        		EXX
        		RR      H
        		RR      L
        		JP      MUL0
;
;SQRA, SQRB - SQUARE ROOT PRIMITIVES
;    Function: B'C'BC = SQR (D'E'DE)
;    Inputs: A = loop counter (normally -31)
;            B'C'BCH'L'HL initialised to 0
;  Destroys: A,B,C,D,E,H,L,B',C',D',E',H',L',F
;
SQR1:		SBC     HL,BC
        		EXX
        		SBC     HL,BC
        		EXX
        		INC     C
        		JR      NC,SQR2
        		DEC     C
        		ADD     HL,BC
        		EXX
        		ADC     HL,BC
        		EXX
        		DEC     C
SQR2:			INC     A
        		RET     P
SQRA:			SLA     C
        		RL      B
        		EXX
        		RL      C
        		RL      B
        		EXX
        		INC     C
        		SLA     E
        		RL      D
        		EXX
        		RL      E
        		RL      D
        		EXX
        		ADC     HL,HL
        		EXX
        		ADC     HL,HL
        		EXX
        		SLA     E
        		RL      D
        		EXX
        		RL      E
        		RL      D
        		EXX
        		ADC     HL,HL
        		EXX
        		ADC     HL,HL
        		EXX
        		JP      NC,SQR1
SQR3:			OR      A
        		SBC     HL,BC
        		EXX
        		SBC     HL,BC
        		EXX
        		INC     C
        		JP      SQR2
;
SQRB:			ADD     HL,HL
        		EXX
        		ADC     HL,HL
        		EXX
        		JR      C,SQR3
        		INC     A
        		INC     C
        		SBC     HL,BC
        		EXX
        		SBC     HL,BC
        		EXX
        		RET     NC
        		ADD     HL,BC
        		EXX
        		ADC     HL,BC
        		EXX
        		DEC     C
        		RET
;
DIGITQ:			LD      A,(IX)
        		CP      #'9'+1
        		CCF
        		RET     C
        		CP      #'0'
        		RET
;
SIGNQ:			LD      A,(IX)
        		INC     IX
        		CP      #' '
        		JR      Z,SIGNQ
        		CP      #'+'
        		RET     Z
        		CP      #'-'
        		RET     Z
        		DEC     IX
        		RET

			;ENDMODULE
