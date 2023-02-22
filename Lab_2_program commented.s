; lab 2 program, edited by Yoana Yun (y23yun) and Javeria Siddiqui (j4siddiq)
; (section 201, group 5)

;*----------------------------------------------------------------------------
;* Name:    Lab_2_program.s 
;* Purpose: This code template is for Lab 2
;* Author: Eric Praetzel and Rasoul Keshavarzi 
;*----------------------------------------------------------------------------*/
		THUMB 		; Declare THUMB instruction set 
                AREA 		My_code, CODE, READONLY 	; 
                EXPORT 		__MAIN 		; Label __MAIN is used externally q
		ENTRY 
__MAIN
; The following lines are similar to Lab-1 but use an address, in r4, to make it easier.
; Note that one still needs to use the offsets of 0x20 and 0x40 to access the ports
;
; Turn off all LEDs:
; given in template file:
		MOV 		R2, #0xC000
		MOV 		R3, #0xB0000000	
		MOV 		R4, #0x0
		MOVT 		R4, #0x2009	; move 2009 into top, ignore bottom halfword	
		ADD 		R4, R4, R2 	; 0x2009C000 - the base address for dealing with the ports
		STR 		R3, [R4, #0x20]	; Turn off the three LEDs on port 1
		MOV 		R3, #0x0000007C
		STR 		R3, [R4, #0x40] ; Turn off five LEDs on port 2 

ResetLUT
		LDR         	R5, =InputLUT	; assign R5 to the address at label LUT

; Start processing the characters


NextChar ; would only executed between letters and at the end of the word (line 35-41)
		CMP 		R1, #0 		; compare R1 and 0 (is equal, done displaying that letter )
		MOV 		R3, #0xB0000000 ; we need to turn the LED off for delays between letters
		MOV32 		R4, #0x2009C000 ; to make sure R4 is ready to set for P1.28 LED
						; (Base address of the memory that controls I/O like LEDs)
		STR 		R3, [R4, #0x20] ; R4 = 0x2009C000, R3 is now 0x2009C020 = LED P1.28
		MOVEQ 		R0, #3		; when CMP above =0, move 3 into R0 (for next letter 3s delay)
		BEQ 		Next		; when CMP above =0, branch to Next (for DELAY) 


Charnext
        LDRB        R0, [R5]	; Read a character to convert to Morse Code
       	ADD        	R5, #1		; point to next value for number of delays, jump by 1 byte
		TEQ         R0, #0		; If we hit 0 (null at end of the string) then reset to the start of lookup table
		BNE			ProcessChar	; If we have a character process it
		
		BL 			LED_OFF		; "branch and link" to turn off led
								; this is for the end of the Word
								; there will be additional #3 delay in addition to the above BEQ Next's #3 delay at line 41
		
		
		MOV			R0, #1		; additional #1 delay in addition to the #6 delay above
		BL			DELAY		; branch & link to display delay 
		BEQ         ResetLUT	; reset to start of lookup table (to A)


ProcessChar	BL		CHAR2MORSE	; convert ASCII to Morse pattern in R1		
		
		;LR <- addr, BL SP - R13, LR - R14, R15- PC
		
		CLZ 		R7, R1		; count leading number of 0s in R1 and store in R7
		LSL			R1, R7		; logical shift left by # of spaces stored in R7 (to remove leading 0s in R1)

; if C is 1 or set, LED_ON
; if C is 0 or clear, LED_OFF


CharP	
		CMP 		R1, #0 		; compare R1 and 0, but dont store results in any register
		BEQ 		NextChar	; if R1 is = 0, then branch to Nextchar subroutine

		LSLS 		R1, R1, #1 	; R1 = morse pattern converted from ASCII
					; logical left shift R1 by 1, and set condition flag S

		BCC 		LED_OFF		; "branch if carry is clear"
					; turn off led if there is no carry (=0)
		BCS 		LED_ON		; "branch if carry is set"
					; turn on led if theres a carry (=1)
		B 			CharP		; branch to CharP (continue loop until R1=0 (which is when the letter is done))



;*************************************************************************************************************************************************
;*****************  These are alternate methods to read the bits in the Morse code LUT. You can use them or not **********************************
;************************************************************************************************************************************************* 

;	This is a different way to read the bits in the Morse Code LUT than is in the lab manual.
; 	Choose whichever one you like.
; 
;	First - loop until we have a 1 bit to send  (no code provided)
;
;	This is confusing as we're shifting a 32-bit value left, but the data is ONLY in the lowest 16 bits, so test starting at bit 15 for 1 or 0
;	Then loop thru all of the data bits:
;
;		MOV		R6, #0x8000	; Init R6 with the value for the bit, 15th, which we wish to test
;		LSL		R1, R1, #1	; shift R1 left by 1, store in R1
;		ANDS		R7, R1, R6	; R7 gets R1 AND R6, Zero bit gets set telling us if the bit is 0 or 1
;		BEQ		; branch somewhere it's zero
;		BNE		; branch somewhere - it's not zero
;
;		....  lots of code
;		B 		somewhere in your code! 	; This is the end of the main program 
;
;	Alternate Method #2
; Shifting the data left - makes you walk thru it to the right.  You may find this confusing!
; Instead of shifting data - shift the masking pattern.  Consider this and you may find that
;   there is a much easier way to detect that all data has been dealt with.
;
;		LSR		R6, #1		; shift the mask 1 bit to the right
;		ANDS		R7, R1, R6	; R7 gets R1 AND R6, Zero bit gets set telling us if the bit is 0 or 1
;
;
;	Alternate Method #3
; All of the above methods do not use the shift operation properly.
; In the shift operation the bit which is being lost, or pushed off of the register,
; "falls" into the C flag - then one can BCC (Branch Carry Clear) or BCS (Branch Carry Set)
; This method works very well when coupled with an instruction which counts the number 
;  of leading zeros (CLZ) and a shift left operation to remove those leading zeros.

;*************************************************************************************************************************************************
;
;
; Subroutines
;
;		convert ASCII character to Morse pattern
;		pass ASCII character in R0, output in R1
;		index into MorseLuT must be by steps of 2 bytes


CHAR2MORSE	STMFD		R13!,{R14}	; given: push Link Register (return address) on stack
		;
		;... add code here to convert the ASCII to an index (subtract 41) and lookup the Morse patter in the Lookup Table
		SUB 		R0, #0x00000041	; subtract 41 from R0 to get the index number (will need to be multiplied by 2 as it's halfword)
		MOV 		R10, #2			; mov 2 into R10
		MUL 		R0, R10			; multiply value in R0 by R10 (2)
									; need to multiply by 2 because of letter being stored in halfword
		LDR 		R7, =MorseLUT	; load appropriate lookup table into R7
		LDRH 		R1, [R7, R0] 	; load register halfword into R1
        	
		LDMFD		R13!,{R15}	; given: restore LR to R15 the Program Counter to return


; Turn the LED on, but deal with the stack in a simpler way
; NOTE: This method of returning from subroutine (BX  LR) does NOT work if subroutines are nested!!
;

	
LED_ON 		push 		{r3-r4}		; given: preserve R3 and R4 on the R13 stack
		;... insert your code here
		MOV 		R3, #0xA0000000 ; turning LED on
		MOV32 		R4, #0x2009C000 ; Base address of the memory that controls I/O like LEDs
		STR 		R3, [R4, #0x20] ; R4 = ; 0x2009C000
		MOV 		R0, #1			; mov 1 into R0 (turn the LED on for 1 dot)
		BL 			DELAY			; branch and link to 0.5s delay
		pop 		{r3-r4}			; given: pop onto stack
		B CharP; branch back to CharP to see if the current bit is the end of the letter

; Turn the LED off, but deal with the stack in the proper way
; the Link register gets pushed onto the stack so that subroutines can be nested


LED_OFF	  	STMFD		R13!,{R12, R14}	; given: push R3 and Link Register (return address) on stack
		; turn off P.1.28 ... insert your code here - 0x2009C020
		MOV 		R3, #0xB0000000 ; turn off LED
		MOV32 		R4, #0x2009C000 ; Base address of the memory that controls I/O like LEDs
		STR 		R3, [R4, #0x20] ; R4 = ; 0x2009C000
		CMP 		R1, #0			; compare R1 and 0, but dont store results in any register
		MOVEQ 		R0, #3			; if R0 is 0, mov 3 into it, used for the end of the word (called by Charnext line 50)
		MOVNE 		R0, #1			; if R0 isn't 0, mov 1 into it
									; (just a regular led off, no long delay)
		BL 			DELAY			; 
		LDMFD		R13!,{R12, R15} ;given, go back to where this subroutine is called
	


DELAY		STMFD		R13!,{R12, R14} ;
MultipleDelay	TEQ		R0, #0		; test R0 to see if it's 0 - will never be 0 at the beginning of MultipleDelay
		MOV		R2, #0xC7000	; ~ 0.5s delay
DELAY2
		SUBS		R2, R2, #0x0001 ; acts as counter, subtract 1 at each iteration
		BNE 		DELAY2 			; if R2 isn't =0, then continue looping in Delay2
		SUBS 		R0, #0x0001 	; -1 to number of loops required
		BEQ 		exitDelay 		; break loop if R0 is =0
		BNE 		MultipleDelay 	; if not, loop until R0 = 0 (no more loop)
	
exitDelay

		LDMFD		R13!,{R12, R15} ; restore R12 and LR to R15 the Program Counter to return

;
; Data used in the program
; DCB is Define Constant Byte size
; DCW is Define Constant Word (16-bit) size
; EQU is EQUate or assign a value.  This takes no memory but instead of typing the same address in many places one can just use an EQU
;
		ALIGN						; make sure things fall on word addresses

; One way to provide a data to convert to Morse code is to use a string in memory.
; Simply read bytes of the string until the NULL or "0" is hit.  This makes it very easy to loop until done.
;

Next 
		BL 			DELAY ; branch to delay 
		B 			Charnext; when done, branch to Charnext for the next char


InputLUT	DCB		"JSYYE", 0		; strings must be stored, and read, as BYTES
									; our initials are J.S. and Y.Y. and E is the letter of our choice

		ALIGN						; make sure things fall on word addresses
; given:
MorseLUT 
		DCW 	0x17, 0x1D5, 0x75D, 0x75 	; A, B, C, D
		DCW 	0x1, 0x15D, 0x1DD, 0x55 	; E, F, G, H
		DCW 	0x5, 0x1777, 0x1D7, 0x175 	; I, J, K, L
		DCW 	0x77, 0x1D, 0x777, 0x5DD 	; M, N, O, P
		DCW 	0x1DD7, 0x5D, 0x15, 0x7 	; Q, R, S, T
		DCW 	0x57, 0x157, 0x177, 0x757 	; U, V, W, X
		DCW 	0x1D77, 0x775 				; Y, Z

; One can also define an address using the EQUate directive
;

LED_PORT_ADR	EQU	0x2009c000				; Base address of the memory that controls I/O like LEDs

		END 
