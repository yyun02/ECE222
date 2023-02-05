;*----------------------------------------------------------------------------
;* Name:    Lab_1_program.s 
;* Purpose: This code flashes one LED at approximately 1 Hz frequency 
;* Author: 	Rasoul Keshavarzi 
;*----------------------------------------------------------------------------*/
	THUMB		; Declare THUMB instruction set 
	AREA		My_code, CODE, READONLY 	; 
	EXPORT		__MAIN 		; Label __MAIN is used externally q
	ENTRY 
__MAIN
; The following operations can be done in simpler methods. They are done in this 
; way to practice different memory addressing methods. 
; MOV moves into the lower word (16 bits) and clears the upper word
; MOVT moves into the upper word
; show several ways to create an address using a fixed offset and register as offset
;   and several examples are used below
; NOTE MOV can move ANY 16-bit, and only SOME >16-bit, constants into a register
; BNE and BEQ can be used to branch on the last operation being Not Equal or EQual to zero
;
	MOV 		R2, #0xC000		; move 0xC000 into R2
	MOV 		R4, #0x0		; init R4 register to 0 to build address
	MOVT 		R4, #0x2009		; assign 0x20090000 into R4
	ADD 		R4, R4, R2 		; add 0xC000 to R4 to get 0x2009C000 

	MOV 		R3, #0x0000007C	; move initial value for port P2 into R3 by writing 0x0000007C
								
	STR 		R3, [R4, #0x40] ; Turn off five LEDs on port 2 

	MOV 		R3, #0xB0000000	; move initial value for port P1 into R3 (turned off)
	STR 		R3, [R4, #0x20]	; Turn off three LEDs on Port 1 using an offset

	MOV 		R2, #0x20		; put Port 1 offset into R2 for user later

	MOV 		R0, #0xFFFF ; initialize r0 lower word for countdown
	MOVT 		R0, #0x000C	; Initialize R0 upper word for countdown (0x000C results in a delay of ~500 ms)

loop
	SUBS 		R0, #1 ; Decrement r0 and set the N,Z,C status bits
	
	BNE 		loop ; dec. the counter until counter isnt = 0
					 ; when counter = 0, perform the lines below:
		
	EOR			R3, #0x10000000 ; XOR of 1011 (hex B) and 0001 (hex 1) results in 1010 (hex A), which is exactly the value we want for toggle
								; B is turning off and A is turning on 
	
	STR 		R3, [R4, R2] ; write R3 port 1, YOU NEED to toggle bit 28 first

							; set counter back to initial value (the upper and lower word):
	MOV 		R0, #0xFFFF ; writes 0xFFFF into the lower word of R0 (and clears the upper word)
	MOVT 		R0, #0x000C ; writes 0x000D into the upper word of R0
	
	B 			loop ; continue the loop forever

 	END 


; lab report: 
; hand assemble the following instruction: ADD R4, R4, R2

; solution: 1110 00 0 0100 0 0100 0100 00000000 0010

	; bits 31-28 are 1110 (Always [default])	
	; bits 27-26 are 00 (given)
	; bit 25 is 0 (bc its register)
 	; bits 24-21 are 0100 (ADD)
	; bit 20 is 0 (S flag, bc status bits are not being set by the ADD operation)
	; bits 19-16 are 0100 (R4 (2 in binary) is the operand reg.)	 
	; bits 15-12 are 0100 (R4 (4 in binary) is the dest. reg. )
	; bits 11-0 are 0010 (operand 2 (R2))



