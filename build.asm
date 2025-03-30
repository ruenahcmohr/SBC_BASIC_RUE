;
; Title:	BBC Basic for BSX - Main build file
; Author:	Dean Belfield
; Created:	16/05/2020
; Last Updated:	08/10/2020
;
; Modinfo:
;
; 08/10/2020:	Minor mods to support UART
			.MODULE BUILD
        .optsdcc -mz80
        .area   _EVERYTHING (ABS)        

ROM_START		.EQU 	0x0000			
RAM_START		.EQU 	0x8000


			.ORG ROM_START

			.include "system.asm"
			CALL    TELL
			.ASCII	"BSX Rue ISA Motherboard\n\r"
			.ASCIZ	"\n\r"

			.include "main.asm"
			.include "exec.asm"
			.include "eval.asm"
			.include "fpp.asm"
			.include "sorry.asm"
			.include "patch.asm"
			.include "ram.asm"

USER:  		
