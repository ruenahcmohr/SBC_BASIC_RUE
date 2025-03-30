			.MODULE SYSTEM
   
BASECOM1 = 0x03F8
     MCR = 4
     LSR = 5
     LCR = 3
     DLL = 0
     DLM = 1
     
FORMAT8N1 = 0x03  
 
        ;; Reset vector
        .org    0
        jp      SYSCOLD

        .org    0x08
        ret
        .org    0x10
        ret
        .org    0x18
        ret
        .org    0x20
        ret
        .org    0x28
        ret
        .org    0x30
        ret
        .org    0x38
        ret
        
        .org    0x66
        ret

        .org    0x100
        jp      SYSCOLD
        
        
                        
SYSCOLD:
        LD      SP,#0xFFFF
        CALL    SYSUARTINIT
        JP      SYSDONE

   
; get UART going for 115k
SYSUARTINIT:
        ; set DLAB on the 8250, if its there...
        ld      bc,#(BASECOM1+LCR)
        ld      d,#0x80
        out     (c),d
       
        ; set 115200 baud
        xor     a
        ld      bc,#(BASECOM1+DLM)
        out     (c),a
        inc     a
        dec     bc
        out     (c),a
         
        ; set N81
        ld      bc,#(BASECOM1+LCR)
        ld      d,#FORMAT8N1
        out     (c),d
        ret


   
; will return with carry flag set if there is a character ready

SYSCINCHECK:
        push    BC
        ld      BC,#(BASECOM1+LSR)
        OR      A   ; CLEAR carry.
        in      a,(c)
        pop     BC
        bit     0,a
        ret     nz
        SCF         ; set carry        
        RET   
                        
       
       
; returns with character in A , blocking                      
SYSCIN:        
        push    BC
        ld      BC,#(BASECOM1+LSR)
SYSRxWait:
        in      a,(c)
        bit     0,a
        jr      z, SYSRxWait
        
        ld      BC,#(BASECOM1)
        in      a,(c)
        SCF
        pop     BC
        ret        

   
; non blocking CIN wrapper   
SYSNBCIN:
   CALL SYSCINCHECK
   RET  NC
   JP   SYSCIN
   
   
   
; character to send in A   
SYSCOUT:
        push    BC
        push    AF
        
SYSTxWait:
        ; check holding register
        ld      bc,#(BASECOM1+LSR)
        in      a,(c)
        bit     5,a
        jr      Z,SYSTxWait
        
        ; write character to transmitter
        pop     AF
        ld      bc,#(BASECOM1)
        out     (c),a
        pop     BC
        ret

    
    
    

SYSDONE:
 ; to the next source file I suppose....                        
