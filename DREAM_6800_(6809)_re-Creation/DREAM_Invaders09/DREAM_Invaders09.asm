; DREAM INVADERS - 6809 Conversion
;
; Original DREAM INVADERS source file header below.
;
; Original source has been modified for:
; - 6809 assembly by the asm6809 Assembler
; - DigicoolThings MECB based DREAM re-Creation, comprising of:
;	- 6809 CPU Card configured as 48K RAM / 16K ROM / $C0 IO (MECB)
;	- Motorola I/O Card for PIA + PTM (MECB)
;	- 128x64 OLED Display (MECB)
;	- 4x5 matrix Keypad (with 4x Function Keys)
;	- Keypad / Tape / Sound interface
; - CHIPOS subroutines all relocated to re-direct jumps located from $F700
;
; Note: Comments in UPPER CASE are the original source comments (mjbauer),
;	my added / ammended comments are all in Mixed Case.
;
;
; TITLE   DREAM INVADERS (C) 1980; M.J. Bauer
;
;
; OLED Display references
;
OLED		EQU	$C088		; OLED Panel base address
OLED_CMD	EQU	OLED		; OLED Command address
OLED_DTA	EQU	OLED+1		; OLED Data address
OLEDRES		EQU	$1C		; Oled Resolution 0=Off / 1=128x64 / 2=64x32
;
; SCRATCHPAD RAM ASSIGNMENTS (PAGE ZERO):
; ARRAY OF ALIEN X-COORDS;  5 ROWS BY 8 COLS:
;
ALNARR		EQU	$C0		; ALIEN ARRAY (40 BYTES)
;
; ARRAY OF ALIEN MISSILE COORDINATES;  4 X-Y PAIRS:
;
MISARR		EQU	$88		; ALIENS' MISSILE ARRAY
;
; POINTERS:
;
		ORG	$0090
MVAPTR		RMB	2		; POINTS TO NEXT ALIEN TO MOVE
ALAPTR		RMB	2		; GENERAL--PURPOSE PTR TO ALNARR
MISPTR		RMB	2		; POINTS TO MISARR
ROW		RMB	1		; TEMP ROW COUNT
COL		RMB	1		; TEMP COL COUNT
;
; FLAGS.  (NON-ZERO IS 'TRUE' CONDITION)
;
NOALFG		RMB	1		; NO ALIENS LEFT
LFLAG		RMB	1		; ALIEN MISSILE HAS BEEN LAUNCHED
DROPFG		RMB	1		; ALIEN DROPPED TO LOWER ROW
NGFLG		RMB	1		; NO GUN TURRETS LEFT
ALNDFG		RMB	1		; ALIEN LANDED
KMOVE		RMB	1		; ALIEN MOVE COMPLETED
;
; VARIABLES:
;
		ORG	$00A0
ALIENX		RMB	1		; GENERAL ALIEN COORDS
ALIENY		RMB	1
AMISX		RMB	1		; GENERAL MISSILE COORDS
AMISY		RMB	1
GMISX		RMB	1		; GUN MISSILE COORDS
GMISY		RMB	1
GUNPOS		RMB	1		; GUN-TURRET POSITION (X)
SCORE		RMB	1		; NO, OF ALIENS EXTERMINATED
KSTEP		RMB	1		; ALIEN STEPS BEFORE DIR'N CHANGER
NMOVE		RMB	1		; MAX. )(MOVE PER CYCLE
ALSTEP  	RMB	1		; ALIEN STEP (+1 RIGHT;  -1 LEFT)
KALIEN		RMB	1		; ALIENS ON SCREEN COUNTER
KGUNS		RMB	1		; GUN-TURRETS LEFT COUNT
QUOT		RMB	1		; DIV QUOTIENT (TEMP)
KMISS		RMB	1		; ALIEN'S ACTIVE-MISSILE COUNT
NMISS		RMB	1		; MAX. SIMULTANEOUS ALIEN MISSILES
MDELAY		RMB	1		; MAIN CYCLE DELAY (GAME SPEED)
CLOCK1		RMB	1		; CONTROL-CYCLE TIMERS
CLOCK2		RMB	1
KSHOT		RMB	1		; SHOT COUNTER (used for DROP)
NDROP		RMB	1		; * SHOTS BEFORE ALIEN CAN DROP
ROUND		RMB	1		; ROUND ( 24 aliens per round)
DECIM		RMB	4		; DECIMAL EQUIV. WORKSPACE
;
; EXTERNAL (CHIPOS) REFERENCES:
;

;
; Standard CHIPOS functions (subroutines) - Redirects
;
ERASE		EQU	$F700		; Clear the display buffer.
FILL		EQU	$F703		; Fill part or all of display buffer with constant byte.
RANDOM		EQU	$F706		; Generate a pseudorandom byte.
LETDSP		EQU	$F709		; Called prior to SHOWI to display a hex digit.
DECEQ		EQU	$F70C		; Store 3-digit BCD equivalent of A at X, X+1, X+2.
SHOWI		EQU	$F70F		; Displays an N-byte symbol in memory pointed at by I.
SHOWX		EQU	$F712		; Displays an N-byte symbol in memory pointed at by X.
PAINZ		EQU	$F715		; Initialises the keypad port.
PAINV		EQU	$F718		; Inverts DDR for Keypad port.
KEYINP		EQU	$F71B		; Decodes the Hex keypad. after a 3.33msec debounce delay.
GETKEY		EQU	$F71E		; Waits for a Key to be pressed, ackowledges with a BLEEP.
BLEEP		EQU	$F721		; Generates a 1200Hz tone in the speaker for approx 80ms.
BTONE		EQU	$F724		; Generates a variable length tome at either 1200Hz or 600Hz.
BTON		EQU	$F727		; Generates a variable length tome at either 1200Hz or 600Hz.
DEL333		EQU	$F72A		; Delay for 3.33ms.
DEL167		EQU	$F72D		; Delay for 1.67ms.
PBINZ		EQU	$F730		; Initialise Tape and Sound port.
INBYT		EQU	$F733		; Inputs a serial byte at 300 baud.
OUTBYT		EQU	$F736		; Outputs a serial byte at 300 baud.
BYTIN		EQU	$F739		; Accepts 2 Hex digits from Keypad and builds a byte.
START		EQU	$F73C		; CHIPOS Monitor entry point.
SHODAT		EQU	$F73F		; Display a byte (2 Hex digits) pointed at by X.
SHOBYT		EQU	$F742		; Display a byte (2 Hex digits) in A reg.
DIGOUT		EQU	$F745		; Display lesast significant digit in A reg.
CURSR		EQU	$F748		; Moves cursor position to the right.
CURS1		EQU	$F74B		; Reset cursor horizontal position (as per A). 
;
;  Old (commented-out) CHIPOS hard-wire address subroutine references
;
;SHOWX   EQU     $C226   Show symbol  @ X, @(VX,VY),  B byte
;ERASE   EQU     $C079   Clear screen
;RANDOM  EQU     $C132   Get random byte (A)
;BTON    EQU     $C2E5   Bleeper   (variable)
;PAINZ   EQU     $C287   Initialize keypad port
;FILL    EQU     $C07D   Fill screen memory with constant
;BTONE   EQU     $C2E1   Bleep for (B)*20 mSec.
;GETKEY  EQU     $C2C4   Wait for input from keypad
;DIGOUT  EQU     $C3D2   Display digit  @ X
;DECEQ   EQU     $C1E0   Store 3-digit decimal equiv @ X
;CURS1   EQU     $C3E0   Set display  'cursor' Pos'n (A)
;
VX		EQU	$2E
VY		EQU	$2F
VF		EQU	$3F
HITFLG		EQU	$3F		; 'Objects collided'  flag - VF
TIME		EQU	$20
TONE		EQU	$21
;
;
PIAA		EQU	$C010		; MC6821 PIA Keypad Port
PIAB		EQU	PIAA+2		; MC6821 PIA Speaker Port
;
;
;******************************************************
;*           DREAM INVADERS - - - COPYRIGHT NOTICE     *
;*                                                     *
;* No part of this Program may be reproduced in any    *
;* form or used in other software product for Purpose  *
;* of distribution or commercial gain, except with     *
;* Permission from the author.                         *
;*                                                     *
;*******************************************************
;
; MAINLINE (INITIALIZATION AND CONTROL CYCLE):
;
		ORG	$0200
;
		LDA	#2		; Set for original 64x32 Resolution
		STA	OLEDRES
MAINGO		JSR	PAINZ		; Reset keypad port.
		LDA	#0		; CLEAR VARIABLES 
		LDX	#$90
MAIN1		STA	0,X
		LEAX	1,X
		CMPX	#$00C0
		BNE	MAIN1
;
; INITIALIZE FOR NEW GAME:
;
MAIN2		JSR	ADJ2		; Set initial game parameters
		LDA	#4		; Start with 4 guns
		STA	KGUNS
;
; INZ. FOR NEW ROUND:
;
MAIN3		INC	ROUND		; Begin next round
		LDA	#24		; Alien count = 24
		STA	KALIEN
		CLR	NOALFG
		CLR	KSTEP
		JSR	INZALA		; SETUP ALIEN ARRAY
		JSR	ERASE
		JSR	SHOWAA		; SHOW ALIEN ARRAY
		LDA	#1
		STA	ALSTEP		; Start with aliens stepping right
		LDA	#$1C		; Start with gun centred
		STA	GUNPOS
		BSR	INZMIS		; Clear the missile array
;
MAIN4		CLR	CLOCK2
		CLR	CLOCK1
;
; START WITH JUST ALIENS MOVING (Insideous, isn't it?)
;
MAIN5		JSR	MOVALN
		LDA	KSTEP		; Move ALL aliens thru 8 steps
		CMPA	#8
		BEQ	MAIN6
		BSR	DELAY
		BRA	MAIN5
;
MAIN6		JSR	DSPGUN		; Show gun turret
;
; A-C-T-T-O-N !
;
CONTRL		LDA	CLOCK1
		ANDA	#$01
		BNE	CTRL1
		JSR	MOVGM		; Move/fire qun-missile
		LDA	NOALFG		; Aliens depleted?
		BNE	MAIN3		; If so, new round.
;
CTRL1		LDA	CLOCK2
		BNE	CTRL2
		JSR	MOVALN		; Move next alien in turn
		LDA	ALNDFG		; Alien landed?
		BNE	ENDGAM		; If so. end game
		JSR	MOVGUN		; Move gun
;
CTRL2		LDA	CLOCK1
		ANDA	#$03
		BNE	CTRL3
		JSR	MVAMIS		; Move (/launch) alien-missiles
		LDA	NGFLG		; Guns depleted?
		BNE	ENDGAM
;
CTRL3		BSR	DELAY
		LDA	CLOCK1		; Adjust clocks
		INCA
		CMPA	#12
		BNE	*+3
		CLRA
		STA	CLOCK1
		LDA	CLOCK2
		INCA
		CMPA	#3
		BNE	*+3
		CLRA
		STA	CLOCK2
		BRA	CONTRL
;
; VARIABLE DELAY TO SET SAME SPFED (MDELAY*100 uSec.):
;
DELAY		LDB	MDELAY
DEL1		LDA	#19
		NOP
DEL2		NOP
		DECA
		BNE	DEL2
		DECB
		BNE	DEL1
		RTS

ENDGAM		JSR	STATUS		; Show round, scrore, guns
		JSR	GETKEY		; Wait for key to restart game
		JMP	MAINGO
;
; INITIALIZE MISSILE ARRAY:
;
INZMIS		LDX	#MISARR
		LDA	#$FF
		STA	GMISY
INZM1		STA	0,X
		LEAX	1,X
		CMPX	#MISARR+8
		BNE	INZM1
		RTS
;
; INITIALIZE ALIEN ARRAY FOR NEW ROUND:
;
INZALA		LDX	#ALNARR		; point to alien array
		STX	MVAPTR		; Reset 'Move Alien' pointer
		CLRA
		LDB	#40		; Do 40 times......
INZA1		CMPB	#16		; Done first 24 (ie 3 rows)?
		BGT	*+4
		LDA	#$FF		; Last 2 rows = $FF
		STA	0,X
		ADDA	#8		; Next col.
		ANDA	#$3F		; For 64 dot wide screen.
		LEAX	1,X		; )
		DECB			; ) Next element
		BNE	INZA1		; )
		RTS
;
; SHOW ALIENS STORED IN ARRAY:
;
SHOWAA		LDX	#ALNARR
		CLR	ALIENY		; First row; alien y = 0
		LDB	#5		; For 5 rows....... 
SHAA1		PSHS	B
		LDB	#8		; For 8 cols 
SHAA2		PSHS	B
		STX	ALAPTR
		LDA	0,X		; Get x-coord
		BMI	SHAA3		; Null. forget it
		STA	ALIENX
		JSR	DSPAL		; Show alien
SHAA3		LDX	ALAPTR		; )
		LEAX	1,X		; )
		PULS	B		; )   Next col.
		DECB			; )
		BNE	SHAA2
		LDA	ALIENY		; )
		ADDA	#5		; )
		STA	ALIENY		; )   Next row.
		PULS	B		; )
		DECB			; )
		BNE	SHAA1		; )
		RTS
;
;  MOVE NEXT ALIEN IN SEQUENCE:
;
MOVALN		CLR	KMOVE
MVA2		LDX	MVAPTR
		LDA	0,X		; Fetch alien-x
		STA	ALIENX
		BMI	MVA7		; Skip if  null.
		LDA	KSHOT		; Check if OK to drop down
		CMPA	NDROP
		BLT	MVA4		; NO
		BSR	DROP		; Attempt to drop down 1 row
		LDA	DROPFG		; Success?
		BNE	MVA6		; YES
MVA4		BSR	CALCY		; Compute alien y-coord
		JSR	DSPAL		; Erase alien at old coords
		LDX	MVAPTR		; Point to alien array
		LDA	0,X
		ADDA	ALSTEP		; Step x-coord
		ANDA	#$3F
		STA	0,X
		STA	ALIENX
		JSR	DSPAL		; Show alien at new coords
MVA6		INC	KMOVE		; Bump counter
MVA7		LDX	MVAPTR		; Bump Pointer
		LEAX	1,X
		CMPX	#ALNARR+40	; Done?
		BNE	MVA8
		BSR	DIRECT		; Set direction of alien movement
		LDX	#ALNARR		; Reset pointer
MVA8		STX	MVAPTR
		LDA	KMOVE		; Move commleted ?
		BEQ	MVA2
		RTS
;
; CALCULATE ROW (B) AND Y-COORD (A) FROM POINTER:
;
CALCY		LDA	MVAPTR+1
		LSRA
		LSRA
		LSRA			; A = row count
		ANDA	#$7
		TFR	A,B		; B = ROW
		ASLA
		ASLA
		PSHS	B
		ADDA	,S+		; Y = ROW*5 (=A)
		STA	ALIENY
		RTS
;
; SET DIRCTION OF ALIEN MOVEMENT:
;
DIRECT		LDA	KSTEP		; Incr. step counter
		INCA			; -.
		STA	KSTEP
		CMPA	#96		; Reverse if all aliens done 96 steps
		BNE	DIR1
		CLR	KSTEP
		NEG	ALSTEP
DIR1		RTS
;
; ATTEMPT TO DROP ALIEN DOWN TO LOWER ROW:
;
DROP		CLR	DROPFG		; Clear 'alien dropped' flag
		BSR	CALCY		; Compute ROW (= B)
		CMPB	#4		; This alien on row 4 ?
		BEQ	DROP6		; Yes; alien just landed! (End)
		LDA	MVAPTR+1	; NO, check for clear below
		ADDA	#8
		STA	ALAPTR+1
		LDX	ALAPTR		; Look at next row down
		LDA	0,X
		CMPA	#$FF		; Is there a vacant slot ?
		BNE	DROP4		; No; forget it.
DROP2		LDX	MVAPTR		; Make null entry in old row
		LDA	#$FF
		STA	0,X
		JSR	DSPAL		; Remove alien from old row
		LDX	ALAPTR		; Store x-coord in new row.
		LDA	ALIENX
		STA	0,X
DROP3		LDA	ALIENY		; calc. y-coord in new row
		ADDA	#5
		STA	ALIENY
		JSR	DSPAL		; Show alien in new row
		CLR	KSHOT		; Reset shot counter
		INC	DROPFG		; Set 'alien dropped' flag
DROP4		RTS
;
; ALIEN LANDED; FLAG END OF GAME:
;
DROP6		INC	ALNDFG		; Set 'alien landed' flag
		JSR	DSPAL		; Remove old alien
		BSR	DROP3		; Show it in new row
		JSR	DSBLOT		; Blot it
		LDB	#100		; Bleep 2 sec
		JSR	BTONE
		RTS
;
; MOVE GUN (If left or right key closed):
;
MOVGUN		LDA	#$01		; Check for LEFT key closed.
		BITA	PIAA
		BNE	MVG2
		JSR	DSPGUN		; Erase gun at old x.
		LDA	GUNPOS
		CMPA	#$02		; (Don't want GUNPOS = 0 or 1)
		BEQ	MVG1		; Skip if hard left
		DECA			; Move left
		STA	GUNPOS
MVG1		JSR	DSPGUN		; Show gun at new x.
		RTS
;
MVG2		LDA	#$02
		BITA	PIAA		; Check for RIGHT key closed.
		BNE	MVG4
		JSR	DSPGUN
		LDA	GUNPOS
		CMPA	#$3B		; Skip if gun is hard right
		BGE	MVG3
		INCA			; Move right
		STA	GUNPOS
MVG3		JSR	DSPGUN
MVG4		RTS
;
; MOVE GUN MISSILE; TEST FOR HIT:
;
MOVGM		LDA	GMISY		; See if missile active
		BMI	MGM2
		BEQ	DISAGM		; Disable if top of screen.
		JSR	DSPGM		; Move UP 1 unit.
		DEC	GMISY
		JSR	DSPGM
		LDA	HITFLG		; Hit anything?
		BNE	MGM4		; Yes.
		RTS
MGM2		LDA	#$08		; FIRE button pressed?
		BITA	PIAA
		BEQ	FIREGM
		RTS
;
; DISABLE GUN MISSILE:
;
DISAGM		JSR	DSPGM		; Erase missile
DGM1		LDA	#$FF
		STA	GMISY		; Store null code.
		RTS
;
; FIRE GUN MISSILE:
;
FIREGM		LDA	GUNPOS
		ADDA	#2		; = centre of gun
		STA	GMISX
		LDA	#$1B
		STA	GMISY
		JSR	DSPGM		; Show missile
		INC	KSHOT		; Bump shot counter
		RTS
;
; DETERMINE WHAT GUN MISSILE INTERCEPTED:
;
MGM4		LDX	#MISARR		; Was it an alien missile?
MGM5		LDA	0,X
		CMPA	GMISX		; Compare missile coords.
		BNE	MGM6
		LDA	1,X
		CMPA	GMISY
		BEQ	MGM8		; Yes, hit missile.
MGM6		LEAX	2,X		; No, try next missile.
		CMPX	#MISARR+8 
		BNE	MGM5
		BSR	HITALN		; Must have hit alien
		RTS
;
; GUN MISSILE HITS ALIEN MISSILE:
;
MGM8		LDA	#$FF		; Kill alien missile
		STA	0,X
		STA	1,X
		JSR	DISMIS		; Display missile collision
		BSR	DGM1		; Disable gun missile 
		RTS
;
; SEARCH ALIEN ARRAY FOR X-COORD OF HIT ALIEN: 
;
HITALN		LDA	GMISY		; Get gun missile y-coord. 
		LDB	#5
		JSR	DIV		; A=A/5 = row of #
		TFR	A,B		; Alien-Y-coord = row x 5
		ASLA
		ASLA
		PSHS	B
		ADDA	,S+
		STA	ALIENY
		TFR	B,A		; Compute array pointer
		ASLA
		ASLA
		ASLA
		ORA	#$C0
		STA	ALAPTR+1	; Search this row....
		LDB	#8		; For 8 columns............
HIT2		LDX	ALAPTR 
		LDA	0,X		; Get alien-x 
		BMI	HIT3		; Skip if null
		LDA	GMISX		; Get gun missile x-coord. 
		SUBA	0,X		; Subtract alien missile x.
		BPL	*+3		; Convert to absolute value
		NEGA
		CMPA	#4		; Diff <= 4?
		BLE	HIT4		; Yes. found the sucker! 
HIT3		INC	ALAPTR+1	; No, try next col.
		DECB
		BNE	HIT2
		RTS			; Search failed; forget it
;
; EXTERMINATE HIT ALIEN:
;
HIT4		LDA	0,X		; Get its x-coord.
		STA	ALIENX
		LDA	#$FF
		STA	0,X		; Deposit null code in array.
		JSR	DISAGM		; Disable gun missile.
		LDB	#1
		JSR	PAUSE		; Wait  for RTC tick
		JSR	DSBLOT		; Blot alien
		LDB	#3
		JSR	BTONE		; Bleep for 60 mSec
		JSR	DSBLOT		; Remove blot
		JSR	DSPAL		; Remove alien
;
; ADJUST SCORER DIFFICULTY LEVEL, ETC:
;
HIT5		JSR	ADJUST
		DEC	KALIEN
		BEQ	HIT6
		RTS
;
; ALL ALIENS DEPLETED; END OF ROUND:
;
HIT6		INC	NOALFG		; Set 'aliens depleted' flag
		LDA	ROUND		; Add bonus 2 guns at....
		CMPA	#10		; ....end of round 10
		BNE	*+8
		INC	KGUNS
		INC	KGUNS
		JSR	STATUS		; Show round, score, guns.
		RTS
;
; ALIEN MISSILE MANAGEMENT; MOVE ALIEN MISSILES:
;
MVAMIS		CLR	LFLAG		; Reset 'launch' flag.
		LDX	#MISARR
		LDB	NMISS		; For (N) missiles.........
MVM1		STB	KMISS 
		STX	MISPTR
		LDA	0,X		; Get missile(I) x-coord.
		STA	AMISX
		BMI	MVM4		; Null; try a launch.
		LDA	1,X		; Get missile(I) Y-coord.
		STA	AMISY
		CMPA	#$1F		; Is it at bottom of screen?
		BEQ	MVM8
MVM2		JSR	DSPALM		; Erase from old Pos'n.
		LDX	MISPTR
		INC	AMISY		; Show in new pos'n.
		LDA	AMISY
		STA	1,X
		JSR	DSPALM
		LDA	HITFLG		; Hit anything?
		BEQ	MVM3		; if not.
		JSR	DSTROY		; if so: destroy it.
		LDA	NGFLG		; Guns depleted?
		BNE	MVMR		; If so, return
MVM3		LDX	MISPTR		; )
		LEAX	2,X		; ) next missile
		LDB	KMISS		; )
		DECB			; )
		BNE	MVM1		; )
MVMR		RTS 
;
; TEST FOR 'CLEAR-TO-LAUNCH' CONDITION:
;
MVM4		LDA	LFLAG		; Already launched 1 this cycle?
		BNE	MVM3		; Yes: next I.
		LDA	CLOCK1		; Launch every 12th control cycle
		BNE	MVM3
		BSR	LAUNCH
		BRA	MVM3		; Next missile.
;
; DE-ACTIVATE ALIEN MISSILE:
;
MVM8		BSR	KILALM 
		BRA	MVM3
;
KILALM		JSR	DSPALM		; Erase it 
KIL1		LDX	MISPTR
		LDA	#$FF
		STA	0,X		; Store null code.
		STA	1,X
		RTS
;
; DESTROY OBJECT HIT BY ALIEN MISSILE:
;
DSTROY		LDA	GMISX		; Check for missile/missile hit
		CMPA	AMISX
		BNE	DST1
		LDA	GMISY
		CMPA	AMISY
		BNE	DST1		; No; try gun
		BSR	KIL1		; Yes; kill missiles.
		JSR	DISMIS		; Show missiles exploding.
		JSR	DGM1		; Disable gun missile.
		RTS
DST1		LDA	AMISY		; Check for gun-turret hit
		CMPA	#$1C
		BGE	KILGUN
		RTS
;
; DESTROY GUN-TURRET;
;
KILGUN		BSR	KILALM		; First remove alien missile
		JSR	DSPCLD		; Show 'cloud' on dead gun-turret
;		JSR	INVERT		; FLASH, ETC
		LDB	#100
		STB	TONE
		LDB	#$40
		JSR	BTON		; Make sound
		DEC	KGUNS		; Decrement gun count
		BGT	KILG1
		INC	NGFLG		; If no guns, set flag.
		RTS
;
KILG1		JSR	STATUS		; Show score, pause.
		JSR	ERASE		; Clear screen
		JSR	INZMIS		; Remove missiles.
		JSR	SHOWAA		; Replace aliens
		JSR	DSPGUN		; Replace gun
		RTS
;
; LAUNCH ALIEN MISSILE;  CH0OSE AT RANDOM:
;
LAUNCH		JSR	RANDOM		; Select random col.
		LDB	#8		; For 8 columns............
AML1		PSHS	B
		ANDA	#7
		STA    COL
		LDA	#$20
		LDB	#5		; For 5 rows (max)....,....
AML2		PSHS	B
		STA	ROW		; Compute pointer from ROW,COL.
		ORA	COL
		ORA	#$C0
		STA	ALAPTR+1
		LDX	ALAPTR
		LDA	0,X		; Fetch alien x (or null)
		BPL	AML3		; If not $ff, we have alien to find
		LDA	ROW		; )
		SUBA	#8		; )
		PULS	B		; ) next ROW up...
		DECB			; )
		BNE	AML2
		LDA	COL
		LDB	KSHOT		; Try next col left or right....
		ANDB	#$01		; ...depending on KSHOT! ...
		BEQ	*+4		; ...(flgure that one out !!)
		DECA
		DECA
		INCA			; )
		PULS	B		; )   next column
		DECB			; )
		BNE	AML1		; )
		RTS			; Search failed; forget it.
;
AML3		PULS	B		; Re-adjust stack
		PULS	B
		ADDA	#2		; Missile-x is centre of alien
		STA	AMISX
		LDX	MISPTR
		STA	0,X		; Store new missile in array
		LDA	ROW
		LSRA
		LSRA
		LSRA
		TFR	A,B		; Mult. A by 5 giving y-coord.
		ASLA
		ASLA
		PSHS	B
		ADDA	,S+
		ADDA	#3		; ..... + 3
		STA	AMISY
		STA	1,X
		JSR	DSPALM		; Show alien missile here
		INC	LFLAG		; Set 'launch' flag
		RTS
;
; SHOW STATUS --- ROUND, SCORE, GUNS:
;
STATUS		CLRA			; Make display 'window'
		LDB	#48
		JSR	FILL
		COMA
		LDB	#50
		JSR	FILL
		LDA	#$10		; Put gaps in window (3 fields)
		JSR	DSPGAP
		LDA	#$2C
		JSR	DSPGAP
;
		LDA	#4
		JSR	CURS1		; Set  'invisible cursor' posn
		LDA	ROUND		; Convert ROUND to decimal
		LDX	#DECIM		; Point  to workspace
		JSR	DECEQ
		LDX	#DECIM+1
		BSR	DISDIG		; Show 'tens'
		LEAX	1,X
		BSR	DISDIG		; Show  'units'
		LDA	#$18		; Convert & show SCORE x 10
		JSR	CURS1
		LDA	SCORE
		LDX	#DECIM
		JSR	DECEQ
		CLRA
		STA	0,X
		LDX	#DECIM
		LDB	#4
STAT2		PSHS	B
		BSR	DISDIG
		LEAX	1,X
		PULS	B
		DECB
		BNE	STAT2
		LDA	#$33		; Show guns remaining
		JSR	CURS1
		LDX	#KGUNS
		BSR	DISDIG
		LDA	#$39		; Show gun symbol
		STA	VX
		LDX	#GUN
		LDB	#4
		JSR	SHOWX
		LDB	#200		; Pause for 4 seconds...
;
; PAUSE:  Wait for the Nth RTC interrupt (N = B-reg):
;
PAUSE		STB	TIME
PSE1		TST	TIME
		BNE	PSE1
		RTS
;
; DISPLAY BCD DIGIT (LSD) OF BYTE 0 X:
;
DISDIG		LDA	0,X
		JMP	DIGOUT		; Use monitor display routine
;
; ADJUST DIFFICULTY LEVEL OF PLAY: 
;
ADJUST		LDA	SCORE		; BUMP score
		CMPA	#250		; Stop at 250 !
		BEQ	ADJ2
		INC	SCORE
;
; COMPUTE MDELAY = MDMAX - SCORE/(255/(MDMAX-MDMIN))
;
ADJ2		LDB	#$28		; [ Maximum-Minimum delay ]
		LDA	#255
		JSR	DIV
		TFR	A,B
		LDA	SCORE
		JSR	DIV
		TFR	A,B
		LDA	#$40		; [ Maximum de1ay ]
		PSHS	B
		SUBA	,S+
		STA	MDELAY
;
; COMPUTE DROP RATE; NDROP = (250 - SCORE)/64 + 1:
;
		LDA	#250
		SUBA	SCORE
		LDB	#64
		JSR	DIV
		ADDA	#1
		STA	NDROP
;
; COMPUTE NMISS = 2, 3 OR 4; depending on which round:
;
		LDA	#4
		LDB	ROUND
		CMPB	#6
		BGE	ADJ3
		DECA
		CMPB	#3
		BGE	ADJ3
		DECA
ADJ3		STA	NMISS
		RTS
;
; DIVIDE A BY B;   8-BITS UNSIGNED; (SLOW);
;
DIV		CLR	QUOT
		TSTB
		BEQ	DIV2		; Dividing by 0 !!!?
DIV1		PSHS	B
		SUBA	,S+		; Compare A with B.   (A-B)
		BCS	DIV2		; Branch if A was LOWER (unsigned)
		INC	QUOT
		BRA	DIV1
DIV2		LDA	QUOT
		RTS
;
; DISPLAY/ERASE GUN MISSILE:
;
DSPGM		LDA	GMISX
		STA	VX
		LDA	GMISY
DSPM1		STA	VY
		LDB	#1
		LDX	#MISILE
		CLR	HITFLG		; Reset  'overlap' flag.
		JMP	SHOWX		; Jump to CHIPOS show routine
;
; DISPLAY/ERASE  ALIEN MISILE:
;
DSPALM		LDA	AMISX
		STA	VX
		LDA	AMISY
		BRA	DSPM1
;
; DISPLAY/ERASE  GUN-TURRET:
;
DSPGUN		LDA	GUNPOS
		STA	VX
		LDA	#$1C
		STA	VY
		LDB	#4
		LDX	#GUN
		JMP	SHOWX
;
; DISPLAY/ERASE ALIEN:
;
DSPAL		LDB	#4
		LDA	ALIENY
		STA	VY
		LDA	ALIENX
		STA	VX
		ANDA	#1		; Test for odd or even x coord
		BEQ	DSPAL1
		LDX	#ALIEN2
		JMP	SHOWX		; Show alien type I (odd x)
DSPAL1   LDX	#ALIEN1
		JMP	SHOWX		; Show alien type 2 (even x)
;
; DISPLAY MISSILE/MISSILE COLLISION:
;
DISMIS		BSR	DISM1		; Show fragments
		LDB	#2
		JSR	PAUSE		; Delay 20 - 40 mSec.
DISM1		LDA	GMISX		; Use gun missile coords
		DECA
		STA	VX
		LDA	GMISY
		DECA
		STA	VY
		LDB	#3
		LDX	#FRAGM
		JMP	SHOWX
;
; DISPLAY/ERASE 'BLOT' ON DECEASED ALIEN:
;
DSBLOT		LDA	ALIENX
		DECA
		STA	VX
		LDA	ALIENY
		DECA
		STA	VY
		LDB	#6
		LDX	#BLOT
		JMP	SHOWX
;
; DISPLAY/ERASE 'CLOUD' OVER HIT GUN-TURRET:
;
DSPCLD		LDA	GUNPOS
		DECA
		STA	VX
		LDA	#$1B
		STA	VY
		LDB	#5
		LDX	#CLOUD 
		JMP	SHOWX
;
; SHOW  GAP IN DISPLAY WINDOW:
;
DSPGAP		STA	VX
		LDA	#$19
		STA	VY
		LDB	#7
		LDX	#GAP
		JMP	SHOWX
;
; INVERT VIDEO,    FULL SCREEN:
;
INVERT		LDA	#$A7		; Inverse the OLED Display
		STA	OLED_CMD	;
		RTS
;
; SYMBOL PATTERNS:
;
MISILE		FCB	$80
GUN		FDB	$2070
		FDB	$F888
		 
ALIEN1		FDB	$F8A8
		FDB	$F850
		 
ALIEN2		FDB	$F8A8
		FDB	$F888
		 
BLOT		FDB	$7CFE
		FDB	$FEFE
		FDB	$FE6C
		 
FRAGM		FDB	$A040
		FCB	$A0
CLOUD		FDB	$387C
		FDB	$FEFE
		FCB	$FE
GAP		FDB	$F0F0
		FDB	$F0F0
		FDB	$F0F0
		FDB	$F0F0
;
;
; CHECKSUM VERIFY ROUTINE: ($7E)
;
		ORG	$07C0
VERIFY		LDX	#$0200
		CLRA
VER1		LDB	0,X
		PSHS	B
		ADDA	,S+
		LEAX	1,X
		CMPX	#$0800
		BNE	VER1
		STA	$00FF
		JMP	START
;
		END
