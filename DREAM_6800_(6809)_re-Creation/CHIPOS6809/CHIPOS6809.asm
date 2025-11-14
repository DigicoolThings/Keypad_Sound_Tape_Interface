; CHIPOS for the 6809 MECB based re-Creation
; ------------------------------------------
;
; Original CHIPOS source file header below.
;
; Original source has been modified for:
; - 6809 assembly by the asm6809 Assembler
; - DigicoolThings MECB based DREAM re-Creation, comprising of:
;	- 2MHz 6809 CPU Card configured as 48K RAM / 16K ROM / $C0 IO (MECB)
;	- Motorola I/O Card for PIA + PTM (MECB)
;	- 128x64 OLED Display (MECB)
;	- 4x5 matrix Keypad (with 4x Function Keys)
;	- Keypad / Tape / Sound interface
; - Keypad routines updated for 4x5 Matrix and removal of CA1 flag use
; - Alternate FN key exit from the FN0 MEMOD mode (reset unnecessary)
; - Tape Load / Dump display of Start / End address as completion feedback
; - Removal of DMA-ENAB (as no longer using DMA driven memory-mapped display)
; - Support for 128x64 HiRes display mode and original 64x32 resolution.
; - Addition of new CHIP-8 instruction (Fx95) to switch display mode (OLEDRES)
; - Removal of DDPAT, as now replaced with pre-expanded 3x5 & 6x10 fonts
; - Removal of PATNH & PATNL, no longer needed with removal of DDPAT
; - Removal of BLOC, now replaced with new OLEDRES
; - Removal of DISBUF & ENDBUF, as display buffer not required for OLED display
; - CHIP-8 Interpreter entry address changed to C800 (instead of original C000)
; - CHIPOS subroutines all relocated to re-direct jumps located from $F700
;
; Assembled binary intended for ROM bootable location from $C800 - $FFFF
;	(with plenty of room for future expansions / monitor additions etc.)
;
; Note: Comments in UPPER CASE are the original source comments (mjbauer),
;	my added / ammended comments are all in Mixed Case.
;
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;                           C H I P O S
;
;  COMPACT HEXADECIMAL INTERPRETIVE PROGRAMMING AND OPERATING SYSTEM
;
;     DREAM-6800 OPERATING SYSTEM WITH CHIP8 LANGUAGE INTERPRETER 
;
;       ORIGINATED BY MICHAEL J BAUER, DEAKIN UNIVERSITY, 1978
;
;                  www.mjbauer.biz/DREAM6800.htm
;
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;
; (1) Upon Relocation, the data at ZRANDOM must be changed accordingly.
;
ENTRY		EQU	$F000		; Entry (Reset) Address
CHIP8		EQU	$C800		; CHIP-8 Interpreter entry point
OLED		EQU	$C088		; OLED Panel base address
OLED_CMD	EQU	OLED		; OLED Command address
OLED_DTA	EQU	OLED+1		; OLED Data address
PIA		EQU	$C010		; MC6821 PIA base address
PIA_PRTB	EQU	PIA+2		; MC6821 PIA Port B & DDR B address
PIA_CTLB	EQU	PIA+3		; MC6821 PIA Control Register B address
PTM		EQU	$C000		; MC6840 PTM address
PTMSTA		EQU	PTM+1		; PTM Read Status Register
PTMC13		EQU	PTM		; PTM Control Registers 1 and 3
PTMC2		EQU	PTM+1		; PTM Control Register 2
PTMTM3		EQU	PTM+6		; PTM Latch 3 (MSB)
;
; SCRATCHPAD RAM ASSIGNMENTS (PAGE 0)
;
IRQV		EQU	$0000		; INTERRUPT VECTOR
BEGA 		EQU	$0002		; BEGIN ADRS FOR LOAD/DUMP
ENDA 		EQU	$0004		; ENDING ADRS FOR LOAD/DUMP
ADRS 		EQU	$0006		; ADRS FOR GO AND MEMOD
;DDPAT 		EQU	$0008		; DIGIT PATTERN TEMP (5 BYTES)
RND		EQU	$000D		; RANDOM BYTE (SEED)
N		EQU 	$000E		; TEMP
ATEMP		EQU 	$000F		; TEMP
XTEMP 		EQU 	$0012		; 2-BYTE SAVE FOR X, SP
ZHI 		EQU 	$0014		; TEMP ADRS
ZLO		EQU	$0015		; 
KEYCOD		EQU	$0017		; KEYCODE TEMP
BADRED		EQU	$0018		; KEY BAD-READ FLAG
OLEDRES		EQU	$001C		; Oled Resolution (was BLOC) 0=Off / 1=128x64 / 2=64x32
;PATNH 		EQU	$001E		; PATTERN TEMP
;PATNL 		EQU	$001F		;
TIME 		EQU	$0020		; RTC TIMER VALUE
TONE 		EQU	$0021		; DURATION COUNT FOR TONE
PPC 		EQU	$0022		; PSEUDO PRGM-COUNTER
PSP 		EQU	$0024		; PSEUDO STACK-PTR
I		EQU	$0026		; CHIP8 MEMORY POINTER
PIR 		EQU	$0028		; PSEUDO INST-REG
VXLOC 		EQU	$002A		; POINTS TO VX
RNDX 		EQU	$002C		; RANDOM POINTER
VX		EQU	$002E		; VARIABLE X (ALSO X-COORD)
VY		EQU	$002F		; VARIABLE Y (ALSO Y-COORD)
;
; CHIP8 VARIABLES (TABLE)
;
VO		EQU 	$0030
VF		EQU	$003F
;
; CHIP8 SUBROUTINE STACK
;
STACK 		EQU 	$005F
;
; OPERATING-SYSTEM STACK
;
STOP 		EQU 	$007F		; STACK TOP (MONITOR)
;
; CHIP-8 GRAPHIC DISPLAY AREA
; ( 1/4K RAM BLOCK MAPPED ONTO T.V. SCREEN BY DMA.
; IN FORMAT 64X32 DOTS
;
;DISBUF		EQU	$0100		; DISPLAY BUFFER AREA
;ENDBUF		EQU	$0200
PIAA		EQU 	$C010		; PORT-A FOR KEYPAD
PIAB		EQU 	$C012		; PORT-B FOR TAPE, RTC, TONE
;
; CHIP-8 INTERPRETER MAINLINE
;
		ORG 	CHIP8
;
		LBSR	ZERASE		; NORMAL ENTRY POINT
		LDX	#$0200		; RESET PSEUDO-PC
		STX	PPC
		LDX	#STACK		; RESET   STACK PTR
		STX	PSP
FETCH		LDX	PPC		; POINT TO NEXT INSTR
		LDX	0,X		; COPY TO PIR
		STX	PIR
		STX	ZHI		; SAVE ADRS (MMM)
		JSR	SKIP2		; BUMP PRGM-CTR
		LDA	ZHI		; MASK OFF ADRS
		ANDA	#$0F
		STA	ZHI
		BSR	FINDV		; EXTRACT VX ALSO
		STA	VX		; STASH VX
		STX	VXLOC		; SAVE LOCATION OF VX
		LDA	PIR+1		; FIND Y
		LSRA
		LSRA
		LSRA
		LSRA
		BSR	FINDV		; EXTRACT VY
		STA 	VY		; STASH VY
EXEC		LDX	#JUMTAB-2	; POINT TO JUMP TABLE
		LDA	PIR		; EXTRACT MSD
		ANDA	#$F0
EXEl		LEAX	2,X		; FlND ROUTINE ADRS
		SUBA	#$10
		BCC	EXEl		; BRANCH IF HIGHER OR SAME
		LDX	0,X		; LOAD ROUTINE ADRS
		JSR	0,X		; PERFORM ROUTINE
		BRA	FETCH		; NEXT INSTR...
FINDV		LDX	#VO-1		; POINT TO VARIABLES TABLE
FIND1		LEAX	1,X		; FIND LOCN VX
		DECA
		BPL	FIND1
		LDA	0,X		; FETCH VX FROM TABLE
		RTS
;
; JUMP TABLE(ROUTINE ADDRESSES)
; 
JUMTAB		FDB	EXCALL		; ERASE, RET, CALL, NOP
		FDB	GOTO		; GOTO MMM
		FDB	DOSUB		; DO MMM
		FDB	SKFEK		; SKF VX=KK
		FDB	SKFNK		; SKF VX#KK
		FDB	SKFEV		; SKF VX=VY
		FDB	LETK		; Vx=KK
		FDB	LETVK		; VX=VX+KK
		FDB	LETVV		; VX=[VX][+-&!]VY
		FDB	SKFNV		; SKF  VX#VY
		FDB	LETI		; I=MMM
		FDB	GOTOV		; GOTO MMM+VO
		FDB	RANDV		; VX-RND.KK
		FDB	SHOW		; SHOW  N@VX, VY
		FDB	SKFKEY		; SKF VX[=#]KEY
		FDB	MISC		; (MINOR JUMP TABL)
;
; ERASE, RETURN, CALL (MLS), OR NOP INTRN:
;
EXCALL		LDB	PIR		; GET INSTR REG
		BNE	CALL
		LDA	PIR+1
		CMPA	#$E0
		BEQ	ZERASE
		CMPA	#$EE
		BEQ	RETDO
		RTS			; NOP, FETCH
;
; ERASE routine	
ZERASE		CLRA			; WRITE ZEROS TO SCREEN
		CLRB			;
ZFILL		JMP	OledFill
RETDO		TFR	S,X		; SAVE REAL SP
		LDS	PSP
		PULS	A
		STA	PPC		; PULL PPC
		PULS	A
		STA	PPC+1
		STS	PSP		; SAVE CHIP8 SP
		TFR	X,S		; RESTORE SP
		RTS
CALL		LDX	ZHI		; GET OPRND ADRS(MMM)
		JMP	0,X		; PERFORM MLS
GOTOV		LDA	VO		; 16-BIT ADD VO TO ADRS
		CLRB
		ADDA	ZLO
		STA	ZLO
		ADCB	ZHI
		STB	ZHI
GOTO		LDX	ZHI		; MOVE ADRS TO PPC
		STX	PPC
		RTS			; FETCH
LETI		LDX	ZHI		; MOVE ADRS TO MI PTR
		STX	I
		RTS			; FETCH
DOSUB		TFR	S,X		; SAVE SP
		LDS	PSP
		LDA	PPC+1		; PUSH PPC
		PSHS	A
		LDA	PPC
		PSHS	A
		STS	PSP		; SAVE CHIP SP
		TFR	X,S		; RESTORE REAL SP
		BRA	GOTO		; JUMP TO ADRS(MMM)
;
; CONDITIONAL SKIP ROUTINES
;
SKFEK		LDA	PIR+1		; GET KK
SKFEQ		CMPA	VX
		BEQ	SKIP2
		RTS
SKFNK		LDA	PIR+1		; GET KK
SKFNE		CMPA	VX
		BNE	SKIP2
		RTS
SKFEV		LDA	VY		; GET VY
		BRA	SKFEQ
SKFNV		LDA	VY
		BRA	SKFNE
SKIP2		LDX	PPC		; ADD 2 TO PPC
		LEAX	2,X
		STX	PPC
		RTS
SKFKEY		JSR	ZKEYINP		; INTERROGATE KEYBOARD
		TST	BADRED		; KEY DOWN?
		BEQ	SKFK1
		LDB	#$A1		; WHAT INSTRN?
		CMPB	PIR+1		; SKF VX#KEY
		BEQ	SKIP2
		RTS			; NO KEY GO FETCH
SKFK1		LDB	#$9E
		CMPB	PIR+1		; WHAT INSTRN?
		BEQ	SKFEQ
		BRA	SKFNE
;
; ARITHMETIC/LOGIC ROUTINES
;
LETK		LDA	PIR+1		; GET KK
		BRA	PUTVX
LETVK		LDA	PIR+1
		ADDA	VX
		BRA	PUTVX
RANDV		BSR	ZRANDOM		; GET RANDOM BYTE
		ANDA	PIR+1
		BRA	PUTVX
LETVV		LDA	VX
		LDB	PIR+1
		ANDB	#$0F		; EXTRACT N
		BNE	LETV1
		LDA	VY		; VX=VY
LETV1		DECB
		BNE	LETV2
		ORA	VY		; VX=VX!VY (OR)
LETV2		DECB
		BNE	LETV4
		ANDA	VY		; VX=VX.VY
LETV4		DECB
		DECB
		BNE	LETV5
		CLR	VF		; VF=0
		ADDA	VY		; VX=VX+VY
		BCC	LETV5		; RESULT < 256
		INC	VF		; VF=1(OVERFLOW)
LETV5		DECB
		BNE	PUTVX
		CLR	VF		; VF=0
		SUBA	VY		; VX=VX-VY
		BCS	PUTVX		; VX<VY? (UNSIGNED)
		INC	VF		; NO PUT VF=l
PUTVX		LDX	VXLOC		; REPLACE VX
		STA	0,X
		RTS
;
; RANDOM BYTE GENERATOR
;
; RANDOM routine
;
ZRANDOM		LDA	#$C8		; HIGH-ORDER BYTE OF RNDX =
		STA	RNDX		; =MSB OF CHIP8 START ADRS
		INC	RNDX+1
		LDX	RNDX		; POINT TO NEXT PROGRAM BYTE
		LDA	RND		; GET SEED (LAST VALUE)
		ADDA	0,X		; MANGLE IT
		EORA	$FF,X
		STA	RND		; STASH IT
		RTS
;
; JUMP TABLE FOR MISCELLANEOUS INSTRNS [FXZZ]
;
MINJMP		FCB	$07		; VX=TIME
		FDB	VTIME
		FCB	$0A		; VX=KEY
		FDB	VKEY
		FCB	$15		; TIME=VX
		FDB     TIMEV
		FCB	$18		; TONE=VX
		FDB 	TONEV
		FCB	$1E		; I=I+VX
		FDB	LETIV
		FCB	$29		; I=DSPL,VX
		FDB	ZLETDSP
		FCB	$33		; MI=DEQ,VX
		FDB	LETDEQ
		FCB	$55		; MI=VO:VX
		FDB	STORV
		FCB	$65		; VO:VX=MI
		FDB	LOADV
		FCB	$95		; Set Graphics Mode
		FDB	GRAPHM
;
MISC		LDX	#MINJMP		; POINT TO TABLE
		LDB	#10		; DO 10 TIMES		
MIS1		LDA	0,X		; GET TABLE OPCODE
		CMPA	PIR+1
		BEQ	MIS2
		LEAX	3,X
		DECB
		BNE	MIS1
		JMP	ZSTART		; BAD OPCODE, RETURN TO MON.
MIS2		LDX	1,X		; GET ROUTINE ADRS FROM TABLE
		LDA	VX		; GET VX
		JMP	0,X		; GO TO ROUTINE
GRAPHM		LDA	ZHI
		STA	OLEDRES
		RTS
VTIME		LDA	TIME
		BRA	PUTVX
VKEY		JSR	ZGETKEY
		BRA	PUTVX
TIMEV		STA	TIME
		RTS
TONEV		TFR	A,B		; SET DURATION=VX
		JMP	ZBTONE
LETIV		CLRB			; 16-BIT ADD VX TO I
		ADDA	I+1
		STA	I+1
		ADCB	I
		STB	I
		RTS
;
; Determine Font & Character (A) to use, & set I for 'SHOW'
;
; LETDSP routine
;
ZLETDSP		LDB	OLEDRES		; Set X to the correct Font Table
		LDX	#FONTH-5	; Initialise for Half res Font Table
		LSRB			; Test for Full res mode
		BNE	LETDSP1		; Assume Half res mode
		LDX	#FONTF-10	; We want Full res Font Table
LETDSP1		PSHS	A		; Save character
		LDA	OLEDRES		; Select the correct Font character
		LDB	#5		; 5 pixel high font characters
		LSRA			; Test for Full res mode
		BNE	LETDSP2		; Assume Half res mode
		LSLB			; Double (10) for Full Res mode
LETDSP2		PULS	A		; Retrieve character
		ANDA	#$0F		; Isolate LS digit
LETDSP3		LEAX	B,X		; Next Character
		DECA			; (A=VX)
		BPL	LETDSP3
		STX	I		; SET MI POINTER
		RTS
;
; Hexadecimal Font Patterns (3x5 matrix)
;
FONTH		FCB	$E0,$A0,$A0,$A0,$E0	; 0
		FCB	$40,$40,$40,$40,$40	; 1
		FCB	$E0,$20,$E0,$80,$E0	; 2
		FCB	$E0,$20,$E0,$20,$E0	; 3
		FCB	$80,$A0,$A0,$E0,$20	; 4
		FCB	$E0,$80,$E0,$20,$E0	; 5
		FCB	$E0,$80,$E0,$A0,$E0	; 6
		FCB	$E0,$20,$20,$20,$20	; 7
		FCB	$E0,$A0,$E0,$A0,$E0	; 8
		FCB	$E0,$A0,$E0,$20,$E0	; 9
		FCB	$E0,$A0,$E0,$A0,$A0	; A
		FCB	$C0,$A0,$E0,$A0,$C0	; B
		FCB	$E0,$80,$80,$80,$E0	; C
		FCB	$C0,$A0,$A0,$A0,$C0	; D
		FCB	$E0,$80,$E0,$80,$E0	; E
		FCB	$E0,$80,$E0,$80,$80	; F
;
; Hexadecimal Font Patterns (6x10 matrix)
;
FONTF		FCB	$78,$FC,$CC,$CC,$CC,$CC,$CC,$CC,$FC,$78	; 0
		FCB	$10,$30,$70,$30,$30,$30,$30,$30,$78,$78	; 1
		FCB	$78,$FC,$CC,$0C,$18,$30,$60,$C0,$FC,$FC	; 2
		FCB	$78,$FC,$CC,$0C,$38,$38,$0C,$CC,$FC,$78	; 3
;		FCB	$0C,$1C,$3C,$6C,$CC,$FC,$FC,$0C,$0C,$0C	; 4
;		FCB	$18,$38,$78,$F8,$D8,$FC,$FC,$18,$18,$18	; 4
		FCB	$CC,$CC,$CC,$CC,$FC,$7C,$0C,$0C,$0C,$0C	; 4
		FCB	$FC,$FC,$C0,$C0,$F8,$FC,$0C,$CC,$FC,$78	; 5
		FCB	$78,$FC,$CC,$C0,$F8,$FC,$CC,$CC,$FC,$78	; 6
		FCB	$FC,$FC,$0C,$18,$18,$30,$30,$60,$60,$60	; 7
		FCB	$78,$FC,$CC,$CC,$78,$FC,$CC,$CC,$FC,$78	; 8
		FCB	$78,$FC,$CC,$CC,$FC,$7C,$0C,$CC,$FC,$78 ; 9
		FCB	$30,$78,$CC,$CC,$FC,$FC,$CC,$CC,$CC,$CC	; A
		FCB	$F8,$FC,$CC,$CC,$F8,$FC,$CC,$CC,$FC,$F8	; B
		FCB	$78,$FC,$CC,$C0,$C0,$C0,$C0,$CC,$FC,$78	; C
		FCB	$F0,$F8,$DC,$CC,$CC,$CC,$CC,$DC,$F8,$F0	; D
		FCB	$FC,$FC,$C0,$C0,$F8,$F8,$C0,$C0,$FC,$FC	; E
		FCB	$FC,$FC,$C0,$C0,$F8,$F8,$C0,$C0,$C0,$C0	; F
;
LETDEQ		LDX	I		; GET MI POINTER
ZDECEQ		LDB	#100		; N=100
		BSR	DECI		; CALC 100'S DIGIT
		LDB	#10		; N=10
		BSR	DECI		; CALC l0'S DIGIT
		LDB	#1
DECI		STB	N
		CLRB
LDEQ1		CMPA	N		; DO UNTIL A<N  ...
		BCS	LDEQ2		; BRANCH IF LOWER NOT SAME.
		INCB
		SUBA	N
		BRA	LDEQ1		; END-DO...
LDEQ2		STB	0,X		; STASH
		LEAX	1,X		; FOR NEXT DIGIT
		RTS
STORV		ORCC	#$10		; KILL IRQ FOR DATA STACK
		STS	XTEMP		; SAVE SP
		LDS	#VO-1		; POINT TO VARIABLES TABLE
		LDX	I		; FOINT MI
		BRA	MOVX		; TRANSFER NB BYTES
LOADV		ORCC	#$10		; KILL IRQ
		STS	XTEMP
		LDS	I		; POINT MI
;		LEAS	-1,S
		LDX	#VO		; POINT TO VO
MOVX		LDB	VXLOC+1		; CALC. X  (AS IN VX)
		ANDB	#$0F		; LOOP (X+l) TIMES.....
MOVX1		PULS	A		; GET NEXT V
		STA	0,X		; COPY IT
		LEAX	1,X
		INC 	I+1		; I=I+X+1(ASSUMES SAME PAGE)
		DECB
		BPL	MOVX1		; CONTINUE...
		LDS	XTEMP		; RESTORE SP
		ANDCC	#$EF		; RESTORE IRQ
		RTS
;
; DISPLAY ROUTINES 
;   
SHOW		LDA	OLEDRES		; Get OLED Display Enable / Resolution flag
		BEQ	OLEDOFF		; OLED is Off
		LDB	PIR+1		; GET N (OPCODE LSB)
		CLR	VF		; CLEAR OVERLAP FLAG
;
; SHOWI routine
;
ZSHOWI		LDX	I		; POINT TO PATTERN BYTES
;
; SHOWX routine
;
ZSHOWX		ANDB	#$0F		; COMPUTE NO. OF BYTES (N)
		BNE	SHOW2		; IF N=0, MAKE N=16
		LDB	#16
SHOW2		PSHS	B		; DO N TIMES,...,.
		STX	ZHI		; SAVE MI POINTER

		LDA	0,X		; FETCH NEW PATTERN BYTE
		LDB	#8		; Do all 8 bits of pattern byte

SHOW3		ASLA			; Next bit to display into Carry
		BCC	SHOW4		; Skip if bit not set
		LBSR	TglPxl		; If bit was set then toggle pixel
SHOW4		INC	VX		; Move VX to next bit pixel
		DECB			; Decrement byte bit count
		BNE	SHOW3		; Do all 8 bits
		LDA	VX		; Finished, so restore VX
		SUBA	#8		;
		STA	VX		;
		INC	VY
		LDX	ZHI		; POINT NEXT PATTERN BYTE
		LEAX	1,X
		PULS	B
		DECB
		LBNE	SHOW2		; CONT.....
OLEDOFF		RTS
;
; KEYPAD ROUTINES 
;
; PAINZ & PAINV routines
;
ZPAINZ		LDB	#$F0		; INITIALIZE PORT
ZPAINV		LDX	#PIAA		; (ENTRY PT FOR INV. DDR)
		CLR	1,X		; RESET & SELECT DDR
		STB	0,X		; SET DATA DIRECTION
		LDB	#$3E		; Setup Ctrl with CA2 High
		STB	1,X
		CLR	0,X		; Output PA4-7 Low
		RTS
;
; KEYPAD INPUT SERVICE ROUTINE
;
; KEYINP routine
;
ZKEYINP		BSR	ZPAINZ		; RESET KEYPAD PORT
		CLR	BADRED		; RESET BAD-READ FLAG
		LBSR	ZDEL333		; DELAY FOR DEBOUNCE
		LDB	0,X		; INPUT ROW DATA
		BSR	KBILD		; FORM CODE BITS 0,1
		STA	KEYCOD
		LDB	#$0F		; SET DDR FOR...
		BSR	ZPAINV		; INVERSE ROW/COL  DIR N
		LDB	0,X		; INPUT COLUM DATA
		LSRB			; RIGHT JUSTIFY
		LSRB
		LSRB
		LSRB
		BSR	KBILD		; FORM CODE BITS 2,3
		ASLA
		ASLA
		ADDA	KEYCOD
		STA	KEYCOD		; BUILD COMPLETE KEYCODE
		RTS
KBILD		CMPB	#$0F		; CHECK KEY STATUS
		BNE	KBILD0		; KEY IS DOWN, GO DECODE IT
		STB	BADRED		; NO KEY, SET BAD-READ FLAG
KBILD0		LDA	#-1
KBILD1		INCA			; (A=RESULT)
		LSRB			; SHIFT DATA BIT TO CARRY
		BCS	KBILD1		; FOUND ZERO BIT ?
		RTS
;
; GETKEY routine - WAIT FOR KEYDOWN, THEN INPUTS
; As we no longer use the CA1 low to high transition flag to signal a key down,
; we first ensure that no key is still down, then await a new key down.
;
ZGETKEY		STX	XTEMP		; SAVE X FOR CALLING ROUTINE
		BSR	ZPAINZ		; RESET PORT, CLEAR FLAGS
		LDB	#$36		; Set CA2 Low (to include FN keys)
		STB	1,X
GETK1		LDA	0,X		; Get PortA Data
		EORA	#$0F		; Any Key Down?
		BNE	GETK1		; Yes, Wait for Key Release
GETK2		BSR	ZPAINZ		; Re-establish PortA default
		LDA	0,X		; Get PortA Data
		EORA	#$0F		; Any Hex Key Down?
		BNE	HEXKEY		; Yes Fetch it in
		CLRB
		BSR	ZPAINV		; Output PA4-7 High (All inputs)
		LDB	#$36		; Set CA2 Low (to check FN keys)
		STB	1,X
		LDA	0,X		; Get PortA Data
		EORA	#$FF		; Any Function Key Down?
		BEQ	GETK2		; No, Loop for Keydown
		ORA	#$80		; Set MSb to indicate FN Key
		BRA	HEXK1		; Return Without Hex Code
HEXKEY		BSR	ZKEYINP		; DECODE THE KEYPAD
		TST	BADRED		; WAS IT A BAD READ?
		BNE	GETK2		; YES, TRY  AGAIN
HEXK1		BSR	ZBLEEP		; Acknowledge Key-down
		BSR	ZPAINZ		; Re-establish PortA default
		LDX	XTEMP		; RESTORE CALLER'S X-REG
		RTS			; RETURN (WITH A<O FOR FN KEY)
;
; TONE GENERATING ROUTINES
;
; BLEEP routine
;
ZBLEEP		LDB	#4		; 80ms 2400Hz
ZBTONE		STB	TONE		; Set Duration (RTC Cycles)
		CLRB			; Ensure Tone Duration doesn't affect Freq
ZBTON		PSHS	A		; Entry point for Variable Duration/Freq 
		LDA	#$7F		; Sound On - DDR
		CMPB	#$40		; Do we want 1200Hz?
		BEQ	BTON1
		JSR	ZPBINZ
		LDB	#$41		; Sound On 2400Hz - PB6 Output High
		BRA	BTON2
BTON1		JSR	ZPBINZ
		LDB	#$40		; Sound On 1200Hz - PB6 Output High

BTON2		STB	0,X		; PB6 High / 1200Hz or 2400Hz 
BTON3		TST	TONE		; WAIT FOR RTC TIME-OUT
		BNE	BTON3
		LDA	#$3F		; Sound Off - DDR
		JSR	ZPBINZ
		PULS	A
		RTS
;
; SOFTWARE DELAY ROUTINE FOR SERIAL I/O:
;
; DEL333 routine
;
ZDEL333		BSR	ZDEL167		; DELAY FOR 3.33 MILLISEC
;
; DEL167 routine
;
ZDEL167		PSHS	X		; Save X
		LDX	#412		; Delay for 1.67 Millisec (@ 2Mhz)
DEL		LEAX	-1,X		; Dec X
		BNE	DEL
		PULS	X		; Restore X
		RTS
;
; TAPE INPUT/OUTPUT ROUTINES
; Initialize Port B for Tape I/O and Sound
; A=DDR - $7F for PB7 input, $3F for PB6 & PB7 input (Sound Off)
;
; PBINZ routine
;
ZPBINZ		LDX	#PIAB
		LDB	#$32		; SELECT DDR
		STB	1,X
		STA	0,X		; Write DDR
		LDB	#$36		; Select Output Reg
		STB	1,X		; WRITE CTRL REG
		LDB	#01		; OUTPUT FOR...
		STB	0,X		; TAPE DATA-OUT HIGH (MARKING)
		RTS
;
; INBYT routine - INPUT ONE BYTE FROM TAPE PORT
;
ZINBYT		BSR	XCHG		; EXCHANGE X FOR PIA ADRS
IN1		LDA	0,X
		BMI	IN1		; LOOK FOR START BIT
		BSR	ZDEL167		; DELAY HALF BIT-TIME (300BD)
		LDB	#9		; DO 9 TIMES....
IN2		ORCC	#$01		; ENSURE PB0 MARKING
		ROL	0,X		; INPUT & SHIFT NEXT BIT
		RORA			; INTO ACC-A
		BSR	ZDEL333		; WAIT 1 BIT-TIME
		DECB
		BNE	IN2		; CONT....
		BRA	OUTX		; RESTORE X AND RETURN
XCHG		STX	XTEMP		; SAVE X-REG
		LDX	#PIAB
		RTS
;
; OUTBYT routine - OUTPUT ONE BYTE TO TAPE PORT 
;
ZOUTBYT		BSR	XCHG
;		PSHS	A
		DEC	0,X		; RESET START BIT
		LDB	#10		; DO 10 TIMES....
OUT1		BSR	ZDEL333		; DELAY 1 BIT-TIME
		PSHS	A
		ANDA	#$01		; Mask Bit to send (PB0)
		STA	0,X		; NEXT BIT TO OUT LINE (PB0)
		ORCC	#$01
		PULS	A		; RESTORE A
		RORA
		DECB
		BNE	OUT1		; CONT....
;		PULS	A		; RESTORE A
OUTX		LDX	XTEMP		; RESTORE X
		RTS
;
; TAPE LOAD AND DUMP ROUTINES
;
LODUMX		LDA	#$3F		; Sound Off
		BSR	ZPBINZ
		LDX	BEGA		; POINT TO FIRST LOAD/DUMP ADR
		RTS
DUMP		BSR	LODUMX
		STX	ADRS
		LBSR	SHOADR		; Display Begin Address
		LDX	BEGA		; Restore Begin Address
DUMP1		LDA	0,X		; FETCH RAM BYTE
		BSR	ZOUTBYT
		LEAX	1,X
		CMPX	ENDA		; (ENDA = LAST ADRS+1)
		BNE	DUMP1
		STX	ADRS
		LBSR	SHOADR		; Display End Address
		BRA	ZSTART
LOAD		BSR	LODUMX
		STX	ADRS
		LBSR	SHOADR		; Display Begin Address
		LDX	BEGA		; Restore Begin Address
LOAD1		BSR	ZINBYT
		STA	0,X		; STASH BYTE IN RAM
		LEAX	1,X
		CMPX	ENDA		; DONE?
		BNE	LOAD1		; CONT....
		STX	ADRS
		BSR	SHOADR		; Display End Address
;		(BRA ZSTART)
;
; START routine - MONITOR ENTRY POINT
;
ZSTART		LDS	#STOP		; RESET SP TO TOP
		LDX	#RTC		; SETUP IRQ VECTOR FOR RTC
		STX	IRQV
		LDA	#$3F		; Sound Off
		LBSR	ZPBINZ
		BSR	SHOADR		; PROMPT
		ANDCC	#$EF		; Clear CC IRQ Flag - Enable IRQ Interrupts
COMAND		LBSR	ZGETKEY		; INPUT SOMETHING
		TSTA
		BPL	INADRS		; IF HEX, GET AN ADDRESS
		BITA	#$01
		BNE	MEMOD		; FN0 = MEM0RY MODIFY
		BITA	#$02
		BNE	LOAD		; FN1 = TAPE LOAD
		BITA	#$04		
		BNE	DUMP		; FN2 = TAPE DUMP
GO		LDX	ADRS		; FN3, so FETCH ADRS FOR GO
		JMP	0,X
INADRS		BSR	BYT1		; BUILD ADRS MS BYTE
		STA	ADRS
		BSR	ZBYTIN		; INPUT & BUILD LSB
		STA	ADRS+1
		BSR	SHOADR		; DISPLAY RESULTANT ADRS
		BRA	COMAND
;
; BYTIN routine
;
ZBYTIN		LBSR	ZGETKEY		; INPUT 2 HEX DIGITS
BYT1		ASLA			; LEFT JUSTIFY FIRST DIGIT
		ASLA
		ASLA
		ASLA
		STA	ATEMP		; HOLD IT
		LBSR	ZGETKEY		; INPUT ANOTHER DIGIT
		ADDA	ATEMP		; BUILD A BYTE
		RTS
;
; MEMORY MODIFY ROUTINE
;
MEMOD		BSR	SHOADR		; SHOW CURRENT ADRS
		LDX	ADRS		; SHOW DATA AT ADRS
		BSR	ZSHODAT		;
		LBSR	ZGETKEY		; WAIT FOR INPUT
		TSTA
		BPL	MEM1		; Hex Key; Get New Data Byte
		BITA	#$01
		BNE	MEM2		; FN0 Key; Next Adrs
		BRA	ZSTART		; Any other FN key; Exit Memod
MEM1		BSR	BYT1		; HEX KEY; NEW DATA BYTE
		STA	0,X		; DEPOSIT IT
MEM2		LEAX	1,X
		STX	ADRS		; BUMP ADRS
		BRA	MEMOD
SHOADR		LDB	OLEDRES		;
		LDA	#$10		; Set Cursor for Half Res mode
		LSRB			; Test for Full res mode
		BNE	SHOWADRC	; Assume Half res mode
		LSLA			; Double for Full Res mode
SHOWADRC	BSR	ZCURS1
		LDA	#$FF		; Illuminate last 14 / 7 rows
		LDB	#50
		JSR	OledFill
		LDX	#ADRS		; POINT TO ADRS MS BYTE
		BSR	ZSHODAT
		LEAX	1,X		; POINT TO ADRS LS BYTE
		BSR	ZSHODAT
		BSR	ZCURSR		; MOVE CURSOR RIGHT
		RTS
;
; SHODAT routine
;
ZSHODAT		LDA	0,X		; FETCH DATA @ X
;
; SHOBYT routine
;
ZSHOBYT		PSHS	A
		LSRA			; ISOLATE MS DIGIT
		LSRA
		LSRA
		LSRA
		BSR	ZDIGOUT		; SHOW ONE DIGIT
		PULS	A
;
; DIGOUT routine
;
ZDIGOUT		STX	XTEMP		; SAVE X
		JSR	ZLETDSP		; POINT TO DIGIT PATTERN
		LDA	OLEDRES		;
		LDB	#5		; Show 5 byte Pattern
		LSRA			; Test for Full res mode
		BNE	DIGOUTC		; Assume Half res mode
		LSLB			; Double (10 byte) for Full res mode
DIGOUTC		JSR	ZSHOWI
;
; CURSR routine
;
ZCURSR		LDB	OLEDRES		;
		LDA	#4		; SHIFT CURSOR RIGHT 4 DOTS
		LSRB			; Test for Full res mode
		BNE	CURSRC		; Assume Half res mode
		LSLA			; Double (8 dots) for Full res mode
CURSRC		ADDA	VX
;
; CURS1 routine
;
ZCURS1		STA	VX		; SET X COORD
		LDB	OLEDRES		;
		LDA	#26		; SET Y COORD
		LSRB			; Test for Full res mode
		BNE	CURSR1C		; Assume Half res mode
		LSLA			; Double (52) for Full Res mode
CURSR1C		STA	VY
		LDX	XTEMP		; RESTORE X_REG
		RTS
;
; REAL TIME CLOCK INTERRUPT SERVICE ROUTINE
;
RTC		DEC	TIME
		DEC	TONE
					; Clear PTM Counter3 IRQ Flag
		LDA	PTMC2		; Read PTM Status Register
		LDD	PTMTM3		; Read PTM Counter3
		RTI
;
; ---------------------------------
; Hardware Reset - Code Entry Point
; ---------------------------------
		ORG	ENTRY
; Initialise Direct Page Register for Zero page
		CLRA
		TFR	A,DP	
; Tell asm6809 (Assembler) what page the DP register has been set to
		SETDP	#$00
; Set Stack to Stack Top
		LDS	#STOP
;
; Initialise our OLED Display Panel
; There are many settings for the SSD1327, but most are correctly initialised
; following a Reset.  Therefore, I only update the settings here that I wish to
; change from their default Reset value (as per the SSD1327 datasheet). 
;
		LDX	#OledInitCmds	; Load X as pointer to Initialise Command table
		LDB	#16		; Number of Command bytes in table
LoadCmdLoop	LDA	,X+		; Load register data pointed to by X and increment X
		STA	OLED_CMD	; Store Command byte
		DECB			; Point to next register
		BNE	LoadCmdLoop	; Have we done all Command bytes?
; Clear the Display Buffer (VRAM)
		CLRA			; Zero byte to Clear Display buffer (VRAM)
		CLRB			; Full Display (Start row = 0)
		LBSR	OledFill	; Fill OLED Display
; Turn ON the Display		
		LDA	#$AF		; Turn Display ON (after clearing buffer)
		STA	OLED_CMD	;
;
; Initialise PTM (Counter 3 for 20ms periodic IRQ)
		LDX	#PTM		; LOAD PTM ADDRESS
		LDD	#$0100		; Select Timer 1 / Clear Reset
		STA	PTMC2-PTM,X	; Setup for Control Register 1
		STB	PTMC13-PTM,X	; Clear Reset
		LDD	#$0043		; Select Timer 3 / Timer 3 IRQ E /8 mode
		STA	PTMC2-PTM,X	; Setup to write Control Register3
		STB	PTMC13-PTM,X	; Set Output Disabled / IRQ Enabled
					; Continous / 16 bit / E div 8 prescaled
		LDD	#5000		; Latch for 20ms (with 2MHz /8 prescale)
		STA	PTMTM3-PTM,X	; Write Counter 3 Latch MSB
		STB	PTMTM3+1-PTM,X	; Write Counter 3 Latch LSB
;
; Setup IRQ Handler - Note: We now do this in the CHIPOS START
;		LDX	#RTC		; SETUP IRQ VECTOR FOR RTC
;		STX	IRQV
;		ANDCC	#$EF		; Clear CC IRQ Flag - Enable IRQ Interrupts
;
; Initially Setup PIA Port B for Sound output (Silence SN76489)
		LDA	#$22		; Select DDR Register B
		STA	PIA_CTLB	; CB2 goes low following data write, returned high by IRQB1 set by low to high transition on CB1
		LDA	#$FF		; Set Port B as all outputs
		STA	PIA_PRTB	; DDR B register write
		LDA	#$26		; Select Port B Data Register (rest as above) 
		STA	PIA_CTLB
; Silence Sound output
		LBSR	silenceSound
;
		LDA	#$01		; Set OLED Display enabled,
		STA	OLEDRES		;  at OLED Resolution 128x64
;
		JMP	ZSTART		; Jump to CHIPOS Entry point
;
;
; Subroutines
; -----------
UpdPxlStpF
; Function:	Update Pixel - Setup for Set, Clr, Tgl subroutines (128x64 res)
;		Note: As SSD1327 stores 2 pixels in each byte, it's
;		necessary to get the VRAM byte first, to avoid overwriting
;		the neighbouring pixel (hence call to GetPxlBytF).
; Parameters:	VX - X coord (0 - 127)
;		VY - Y coord (0 - 63)
; Returns:	B - Pixel value at X,Y (appropriate nibble)
; Destroys:	A,B
		BSR	GetPxlBytF	; Get curent pixel byte values
		PSHS	B		; Save current pixel byte
		LDB	VY		; Retrieve Y coord
		TFR	B,A		; 
		LBSR	RowSetF		; Set Row of pixel
		LDA	VX		; Retrieve X coord
		TFR	A,B		; 
		LBSR	ColSetF		; Set Column of pixel
		PULS	B		; Retrieve current pixel
		RTS
;
UpdPxlStpH
; Function:	Update Pixel - Setup for SetH, ClrH, TglH subroutines (64x32 res)
;		Note: As SSD1327 stores 2 pixels in each byte, half Resolution
;		is easily achieved as we're updating 2 pixels for every "pixel"
; Parameters:	VX - X coord (0 - 63)
;		VY - Y coord (0 - 31)
; Returns:	B - Pixel value at A,B (dual even pixel byte)
; Destroys:	A,B
		BSR	GetPxlBytH	; Get curent pixel byte value
		PSHS	B		; Save current pixel byte
		LDB	VY		; Retrieve Y coord
		TFR	B,A		; 
		LBSR	RowSetH		; Set Row of pixel
		LDA	VX		; Retrieve X coord
		TFR	A,B		; 
		LBSR	ColSetH		; Set Column of pixel
		PULS	B		; Retrieve current pixel
		RTS
;
GetPxlBytF
; Function:	Get the Pixel Byte at VX,VY
; Parameters:	VX - X coord (0 - 127)
;		VY - Y coord (0 - 63)
; Returns:	B - Pixel value at A,B (appropriate nibble)
; Destroys:	A,B
		LDB	VY		; Retrieve Y coord
		TFR	B,A		; 
		LBSR	RowSetF		; Set Row of pixel
		LDA	VX		; Retrieve X coord
		TFR	A,B		; 
		LBSR	ColSetF		; Set Column of pixel
		LDB	OLED_DTA 	; Dummy Read
		LDB	OLED_DTA 	; Actual Read
		RTS
;
GetPxlBytH
; Function:	Get the Pixel Byte at VX,VY
; Parameters:	VX - X coord (0 - 63)
;		VY - Y coord (0 - 31)
; Returns:	B - Pixel value at A,B (dual even pixel byte)
; Destroys:	A,B
		LDB	VY		; Retrieve V coord
		TFR	B,A		; 
		LBSR	RowSetH		; Set Row of pixel
		LDA	VX		; Retrieve X coord
		TFR	A,B		; 
		LBSR	ColSetH		; Set Column of pixel
		LDB	OLED_DTA 	; Dummy Read
		LDB	OLED_DTA 	; Actual Read
		RTS
;
SetPxlF
; Function:	Set the Pixel at VX,VY (128x64 Res)
;		Note: As SSD1327 stores 2 pixels in each byte, it's
;		necessary to get the VRAM byte first, to avoid overwriting
;		the neighbouring pixel.
; Parameters:	VX - X coord (0 - 127)
;		VY - Y coord (0 - 63)
; Returns:	-
; Destroys:	A,B
		BSR	UpdPxlStpF	; Setup for updating the pixel
		LDA	VX
		BITA	#$01		; Test if we're updating odd column?
		BEQ	WasEvnSet	;
		ORB	#$0F		; Set for odd column pixel
		BRA	StrPxlSet	;
WasEvnSet	ORB	#$F0		; Set for even column pixel
StrPxlSet	STB	OLED_DTA	;
		RTS
;
SetPxlH
; Function:	Set the Pixel at VX,VY (64x32 Res)
;		Note: As SSD1327 stores 2 pixels in each byte, half resolution
;		is easily achieved as we're updating 2 pixels for every "pixel"
; Parameters:	VX - X coord (0 - 63)
;		VY - Y coord (0 - 31)
; Returns:	-
; Destroys:	A,B
		LBSR	UpdPxlStpH	; Setup for updating the pixel
		LDB	#$FF		; Set double pixel
		STB	OLED_DTA	;
		STB	OLED_DTA	;
		RTS
;
SetPxl
; Function:	Set the Pixel at VX,VY (Res as per OLEDRES)
; Parameters:	VX - X coord (0 - 63 / 127)
;		VY - Y coord (0 - 31 / 63)
; Returns:	-
; Destroys:	-
		PSHS	A,B
		LDA	OLEDRES		; Get OLED resolution flag
		BEQ	SetPxlRts	; Nothing to do
		LSRA			; Test for Full res mode
		BEQ	SetPxl2		;
		BSR	SetPxlH		; Assume Half Res Mode
		BRA	SetPxlRts	;
SetPxl2		BSR	SetPxlF		; Full Res Mode
SetPxlRts	PULS	A,B
		RTS
;
ClrPxlF
; Function:	Clear the Pixel at VX,VY (128x64 Res)
;		Note: As SSD1327 stores 2 pixels in each byte, it's
;		necessary to get the VRAM byte first, to avoid overwriting
;		the neighbouring pixel.
; Parameters:	VX - X coord (0 - 127)
;		VY - Y coord (0 - 63)
; Returns:	-
; Destroys:	A,B
		LBSR	UpdPxlStpF	; Setup for updating the pixel
		LDA	VX
		BITA	#$01		; Test if we're updating odd column?
		BEQ	WasEvnClr	;
		ANDB	#$F0		; Clear odd column pixel
		BRA	StrPxlClr	;
WasEvnClr	ANDB	#$0F		; Clear even column pixel
StrPxlClr	STB	OLED_DTA	;
		RTS
;
ClrPxlH
; Function:	Clear the Pixel at VX,VY (64x32 Res)
;		Note: As SSD1327 stores 2 pixels in each byte, half resolution
;		is easily achieved as we're updating 2 pixels for every "pixel"
; Parameters:	VX - X coord (0 - 63)
;		VY - Y coord (0 - 31)
; Returns:	-
; Destroys:	A,B
		LBSR	UpdPxlStpH	; Setup for updating the pixel
		CLRB			; Clear double pixel
		STB	OLED_DTA	;
		STB	OLED_DTA	;
		RTS
;
ClrPxl
; Function:	Clear the Pixel at VX,VY (Res as per OLEDRES)
; Parameters:	VX - X coord (0 - 63 / 127)
;		VY - Y coord (0 - 31 / 63)
; Returns:	-
; Destroys:	-
		PSHS	A,B
		LDA	OLEDRES		; Get OLED resolution flag
		BEQ	ClrPxlRts	; Nothing to do
		LSRA			; Test for Full res mode
		BEQ	ClrPxl2		;
		BSR	ClrPxlH		; Assume Half Res Mode
		BRA	ClrPxlRts	;
ClrPxl2		BSR	ClrPxlF		; Full Res Mode
ClrPxlRts	PULS	A,B
		RTS
;
TglPxlF
; Function:	Toggle (invert) the Pixel value at VX,VY (128x64 res)
;		Note: As SSD1327 stores 2 pixels in each byte, it's
;		necessary to get the VRAM byte first, to avoid overwriting
;		the neighbouring pixel.
; Parameters:	VX - X coord (0 - 127)
;		VY - Y coord (0 - 63)
; Returns:	-
; Destroys:	A,B
		LBSR	UpdPxlStpF	; Setup for updating the pixel
		LDA	VX
		BITA	#$01		; Test if we're updating odd column?
		BEQ	WasEvnTgl	;
		EORB	#$0F		; Invert odd column pixel
		BITB	#$0F		; Was Pixel toggled OFF?
		BNE	StrPxlTglF	; No, pixel was originally OFF
		LDA	#1		; SET CHIPOS OVERLAP FLAG (VF)
		STA 	VF
		BRA	StrPxlTglF	;
WasEvnTgl	EORB	#$F0		; Invert even column pixel
		BITB	#$F0		; Was Pixel toggled OFF?
		BNE	StrPxlTglF	; No, pixel was originally OFF
		LDA	#1		; SET CHIPOS OVERLAP FLAG (VF)
		STA 	VF
StrPxlTglF	STB	OLED_DTA	;
		RTS
;
TglPxlH
; Function:	Toggle (invert) the Pixel value at VX,VY (64x32 res)
;		Note: As SSD1327 stores 2 pixels in each byte, half resolution
;		is easily achieved as we're updating 2 pixels for every "pixel"
; Parameters:	VX - X coord (0 - 63)
;		VY - Y coord (0 - 31)
; Returns:	-
; Destroys:	A,B
		LBSR	UpdPxlStpH	; Setup for updating the pixel
		EORB	#$FF		; Invert column dual pixel
		BITB	#$FF		; Was Pixel toggled OFF?
		BNE	StrPxlTglH	; No, pixel was originally OFF
		LDA	#1		; SET CHIPOS OVERLAP FLAG (VF)
		STA 	VF
StrPxlTglH	STB	OLED_DTA	;
		STB	OLED_DTA	;
		RTS
;
TglPxl
; Function:	Toggle (invert) the Pixel value at VX,VY (Res as per OLEDRES)
; Parameters:	VX - X coord (0 - 63 / 127)
;		VY - Y coord (0 - 31 / 63)
; Returns:	-
; Destroys:	-
		PSHS	A,B
		LDA	OLEDRES		; Get OLED resolution flag
		BEQ	TglPxlRts	; Nothing to do
		LSRA			; Test for Full res mode
		BEQ	TglPxl2		;
		BSR	TglPxlH		; Assume Half Res Mode
		BRA	TglPxlRts	;
TglPxl2		BSR	TglPxlF		; Full Res Mode
TglPxlRts	PULS	A,B
		RTS
;
ColSetF
; Function:	Set the Display buffer Column Start and End addresses (128x64 res)
; Parameters:	A - Start column (0 - 127)
;		B - End column  (0 - 127)
; Returns:	-
; Destroys:	A,B
		PSHS	A		;
		LDA	#$15		; Set Column Address Command
		STA	OLED_CMD	;
		PULS	A		; Start column (left)
		LSRA			; Div A by 2 (2 pixels per byte)
		STA	OLED_CMD	;
		LSRB			; Div B by 2 (2 pixels per byte)
		STB	OLED_CMD	; End column address (right)
		RTS
;
ColSetH
; Function:	Set the Display buffer Column Start and End addresses (64x32 res)
; Parameters:	A - Start column (0 - 63)
;		B - End column  (0 - 63)
; Returns:	-
; Destroys:	-
		PSHS	A		;
		LDA	#$15		; Set Column Address Command
		STA	OLED_CMD	;
		PULS	A		; Start column (left)
		STA	OLED_CMD	;
		STB	OLED_CMD	; End column address (right)
		RTS
;
RowSetF
; Function:	Set the Display buffer Row Start and End addresses (128x64 res)
; Parameters:	A - Start row (0 - 63)
;		B - End row (0 - 63) 
; Returns:	-
; Destroys:	-
		PSHS	A		; Save A
		LDA	#$75		; Set Row Address Command
		STA	OLED_CMD	;
		PULS	A		; Start row (top)
		STA	OLED_CMD	;
		STB	OLED_CMD	; End row (bottom)
		RTS
;
RowSetH
; Function:	Set the Display buffer Row Start and End addresses (64x32 res)
; Parameters:	A - Start row (0 - 31)
;		B - End row (0 - 31) 
; Returns:	-
; Destroys:	A,B
		PSHS	A		; Save A
		LDA	#$75		; Set Row Address Command
		STA	OLED_CMD	;
		PULS	A		; Start row (top)
		LSLA			;
		STA	OLED_CMD	;
		LSLB			;
		ADDB	#$01		;
		STB	OLED_CMD	; End row (bottom)
		RTS
;
OledFill
; Function:	Fill OLED display VRAM with byte, from a specified start row
; Parameters:	A - Byte to fill OLED buffer with
;		B - Start Row (i.e. 0 for full panel fill)
; Returns:	-
; Destroys:	B,Y
; Turn ON the Display		
		TFR	D,Y		; Save Parameters
		LDA	#$A4		; Normal Display
		STA	OLED_CMD	;
;
		CLRA			; Set Column Address range
		LDB	#127		; Start =0, End = 127
		BSR	ColSetF		;
;
		TFR	Y,D		; Restore Parameters
		PSHS	A		; Save Byte to fill
		TFR	B,A		; Set Row Address range

		LDY	#0		; Establish Count of Bytes to Write
WrtDtaLp1	LEAY	128,Y		; Add 128 to Y
		INCB
		CMPB	#64
		BNE	WrtDtaLp1
;
		LDB	#63		; Start = A, End = 63
		BSR	RowSetF		;
		PULS	A		; Restore Byte to fill
WrtDtaLp2	STA	OLED_DTA	; Write Byte to curent buffer location
		LEAY	-1,Y		; Dec Y
		BNE	WrtDtaLp2	; Done?
		RTS
;
OledFillAll
; Function:	Fill OLED display VRAM with byte (note 1 byte = 2 pixels)
; Parameters:	A - Byte to fill OLED buffer with
; Returns:	-
; Destroys:	Y
		LDY	#4096		; 64 x 64 bytes (128 x 64 pixels)
		PSHS	A		; Save Byte we want to fill with
		LDA	#$A4		; Normal Display
		STA	OLED_CMD	;
;
		LDA	#$00		; Set Column Address range
		LDB	#$7F		; Start =0, End = 128
		LBSR	ColSetF	;
;
		LDA	#$00		; Set Row Address range
		LDB	#$3F		; Start = 0, End = 64
		BSR	RowSetF	;
;
		PULS	A		; Restore Byte we want to fill with
WrtDtaLp	STA	OLED_DTA	; Write Byte to curent buffer location
;		PSHS	Y
;		BSR	delay1MS
;		PULS	Y
		LEAY	-1,Y		; Dec Y
		BNE	WrtDtaLp	; Done?
		RTS
;
writeSoundByte
; Function:	Write Sound Byte (A) to SN76489 and wait for not busy
; Parameters:	A - Sound Byte to write
; Returns:	-
; Destroys:	A
		STA PIA_PRTB
busyCheck	LDA PIA_CTLB		; Read control Register
		BPL busyCheck		; Wait for CB1 transition (IRQB1 flag)	
		LDA PIA_PRTB		; Reset the IRQ flag by reading the data register
		RTS
;
silenceSound	
; Function:	Silence all SN76489 Sound Channels
; Parameters:	-
; Returns:	-
; Destroys:	A
		LDA	#$9F		; Turn Off Channel 0
		BSR	writeSoundByte
		LDA	#$BF		; Turn Off Channel 1
		BSR	writeSoundByte
		LDA	#$CF		; Turn Off Channel 2
		BSR	writeSoundByte
		LDA	#$FF		; Turn Off Noise Channel
		BSR	writeSoundByte
		RTS
;
OledInitCmds	FCB	$B3,$71		; Set Clk Divider / Osc Fequency
		FCB	$A0,$51		; Set appropriate Display re-map
		FCB	$D5,$62		; Enable second pre-charge
		FCB	$81,$7F		; Set contrast (0 - $FF)
		FCB	$B1,$74		; Set phase length - Phase 1 = 4 DCLK / Phase 2 = 7 DCLK
		FCB	$B6,$0F		; Set second pre-charge period
		FCB	$BC,$07		; Set pre-charge voltage - 0.613 x Vcc
		FCB	$BE,$07		; Set VCOMH - 0.86 x Vcc
;
;
; Default Interrupt Handlers (Redirects)
;
RSRVD		RTI			; Reserved Vector / Handler
SWI3		RTI			; SWI3 Vector / Handler
SWI2		RTI			; SWI2 Vector / Handler
FIRQ		RTI			; FIRQ Vector / Handler
IRQ		LDX	IRQV		; IRQ Vector - Redirect to IRQV
		JMP	0,X
SWI		RTI			; SWI Vextor / Handler
NMI		RTI			; NMI Vector / Handler

;
; Standard CHIPOS functions (subroutines) - Redirects
;
		ORG	$F700		; Establish Standard base address for Redirects
ERASE		JMP	ZERASE		; Clear the display buffer.
FILL		JMP	ZFILL		; Fill part or all of display buffer with constant byte.
RANDOM		JMP	ZRANDOM		; Generate a pseudorandom byte.
LETDSP		JMP	ZLETDSP		; Called prior to SHOWI to display a hex digit.
DECEQ		JMP	ZDECEQ		; Store 3-digit BCD equivalent of A at X, X+1, X+2.
SHOWI		JMP	ZSHOWI		; Displays an N-byte symbol in memory pointed at by I.
SHOWX		JMP	ZSHOWX		; Displays an N-byte symbol in memory pointed at by X.
PAINZ		JMP	ZPAINZ		; Initialises the keypad port.
PAINV		JMP	ZPAINV		; Inverts DDR for Keypad port.
KEYINP		JMP	ZKEYINP		; Decodes the Hex keypad. after a 3.33msec debounce delay.
GETKEY		JMP	ZGETKEY		; Waits for a Key to be pressed, ackowledges with a BLEEP.
BLEEP		JMP	ZBLEEP		; Generates a 1200Hz tone in the speaker for approx 80ms.
BTONE		JMP	ZBTONE		; Generates a variable length tone (B).
BTON		JMP	ZBTON		; Generates a variable length tone (TONE) at either 1200Hz or 600Hz (B).
DEL333		JMP	ZDEL333		; Delay for 3.33ms.
DEL167		JMP	ZDEL167		; Delay for 1.67ms.
PBINZ		JMP	ZPBINZ		; Initialise Tape and Sound port.
INBYT		JMP	ZINBYT		; Inputs a serial byte at 300 baud.
OUTBYT		JMP	ZOUTBYT		; Outputs a serial byte at 300 baud.
BYTIN		JMP	ZBYTIN		; Accepts 2 Hex digits from Keypad and builds a byte.
START		JMP	ZSTART		; CHIPOS Monitor entry point.
SHODAT		JMP	ZSHODAT		; Display a byte (2 Hex digits) pointed at by X.
SHOBYT		JMP	ZSHOBYT		; Display a byte (2 Hex digits) in A reg.
DIGOUT		JMP	ZDIGOUT		; Display lesast significant digit in A reg.
CURSR		JMP	ZCURSR		; Moves cursor position to the right.
CURS1		JMP	ZCURS1		; Reset cursor horizontal position (as per A). 
;
;
; Hardware Vector Table
;
		ORG	$FFF0		; Setup 6809 Hardware Vectors
;		
		FDB	RSRVD		; Reserved
		FDB	SWI3		; Software Interrupt 3
		FDB	SWI2		; Software Interrupt 2
		FDB	FIRQ		; Fast Interrupt Request
		FDB	IRQ		; Interrupt Request
		FDB	SWI		; Software Interrupt
		FDB	NMI		; Non-Maskable Interrupt
		FDB	ENTRY		; Reset
;
		END
