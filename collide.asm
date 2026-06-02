	org $BFF0					; Header - see other tutorials
	db "NES", $1A				; for more info
	db $1
	db $1
	db %00000000
	db %00000000
	db 0
	db 0,0,0,0,0,0,0


; labels/variables
button_array equ $00 			; Storage of buttons being pressed.
player_x equ $01 				; x-position of player sprite
player_y equ $02 				; y-position of player sprite
player_inner_x equ $03			; x-position within one tile
player_inner_y equ $04			; y-position within one tile
player_abs_x equ $05
player_abs_y equ $06
dx equ $07 						; delta x (movement)
dy equ $08 						; delta y (movement)

n equ $EF 						; NMI-only "register"

nmi_flags equ $F0 				; keeping track of whether waiting/ready (1 yes, 0 no):
								; -----LVN (L-Logic, V-video, N-NMI)

; Flags for collision_flags: 
; 765
; 4X3 <-- X - YOU ARE HERE
; 210
collision_flags equ $F1 		

start_collision_high equ $F2 	; MSB for collision logic (player_abs_y - 1)
start_collision_low equ $F3		; Storage for LSB for collision logic (player_abs_y - 1)


;; REGISTERS ;;
z3 equ $FD
z2 equ $FE
z equ $FF 						; "z" register (for those times when x and y aren't enough abc's)


; nmi/irq/reset
nmi:
	pha
		php
			txa
			pha
			tya
			pha

	; check last nmi was completed
	lda nmi_flags
	and #1 					; check bit 1, did nmi finish?
	bne finish_nmi  		; if it didn't, get to the end pls

	; we're in an NMI, turn the "NMI" bit on:
	lda nmi_flags
	ora #1
	sta nmi_flags

	lda nmi_flags
	and #%00000100 			; Are we ready to check collision?
	beq gfx_update 			; If not, just update the sprites
	jsr check_bg_collision_in_nmi
	lda nmi_flags
	and #%11111011  		; Turn bit 2 off (logic) since
	sta nmi_flags			; we finished checking bkgd

gfx_update:
	lda #$02
	sta $4014
	lda nmi_flags
	and #%11111101 			; Turn bit 1 off (video flag)
	sta nmi_flags

finish_nmi:
	lda nmi_flags
	and #$FE  				; turn off bit 1 - we've just finished
	sta nmi_flags 			; the most recent unfinished NMI

			pla
			tay
			pla
			tax
		plp
	pla
	rti

; Subroutine for checking collisions against BG
check_bg_collision_in_nmi:

check_bg_collision_upper_left:
	jsr check_bg_collision_update_2006_lda_2007
	lda #%10000000
	jsr check_bg_collision_chk_flag

check_bg_collision_upper:
	lda #1
	sta n
	jsr check_bg_collision_add_to_find_next_spot
	jsr check_bg_collision_update_2006_lda_2007
	lda #%01000000
	jsr check_bg_collision_chk_flag

check_bg_collision_upper_right:
	lda #1
	sta n
	jsr check_bg_collision_add_to_find_next_spot
	jsr check_bg_collision_update_2006_lda_2007
	lda #%00100000
	jsr check_bg_collision_chk_flag

check_bg_collision_left:
	lda #30
	sta n
	jsr check_bg_collision_add_to_find_next_spot
	jsr check_bg_collision_update_2006_lda_2007
	lda #%00010000
	jsr check_bg_collision_chk_flag


check_bg_collision_right:
	lda #2
	sta n
	jsr check_bg_collision_add_to_find_next_spot
	jsr check_bg_collision_update_2006_lda_2007
	lda #%00001000
	jsr check_bg_collision_chk_flag

check_bg_collision_lower_left:
	lda #30
	sta n
	jsr check_bg_collision_add_to_find_next_spot
	jsr check_bg_collision_update_2006_lda_2007
	lda #%00000100
	jsr check_bg_collision_chk_flag

check_bg_collision_lower:
	lda #1
	sta n
	jsr check_bg_collision_add_to_find_next_spot
	jsr check_bg_collision_update_2006_lda_2007	
	lda #%00000010
	jsr check_bg_collision_chk_flag

check_bg_collision_lower_right:
	lda #1
	sta n
	jsr check_bg_collision_add_to_find_next_spot
	jsr check_bg_collision_update_2006_lda_2007
	lda #%00000001
	jsr check_bg_collision_chk_flag

done_check_bg_collision:

	lda nmi_flags
	and #%11111011 			; Turn bit 2 off (logic)
	sta nmi_flags
	
	lda #0
	sta $2005
	lda #255
	sta $2005
	lda #$88
	sta $2000
	rts

check_bg_collision_update_2006_lda_2007:
	lda start_collision_high
	sta $2006
	lda start_collision_low
	sta $2006

	; TODO (later): modify for scrolling logic

	
	; prime read buffer, ignoring result
	; see: https://www.nesdev.org/wiki/PPU_registers#PPUDATA_-_VRAM_data_($2007_read/write)
	lda $2007 						
	lda $2007    					; Checking BG at that spot
	tax
	rts

check_bg_collision_chk_flag:
	cpx #1
	bne finished_check_bg_collision_chk_flag
	ora collision_flags
	sta collision_flags
finished_check_bg_collision_chk_flag:
	rts

check_bg_collision_add_to_find_next_spot:
    lda start_collision_low
    clc
    adc n
    sta start_collision_low
    lda start_collision_high
    adc #0
    sta start_collision_high
    rts

irq:
	rti

reset:
	cld
	sei

	ldx #$FF
	txs
	
	inx
	sta $2000
	sta $2001
	sta $4010
	sta $4015
	
	lda #40
	sta $4017 			; Disable frame interrupt (APU frame counter)

wait_vblank:
	bit $2002
	bpl wait_vblank

	
	; x is already 0
clear_ram:
	lda #0
	sta $0000,x
	sta $0100,x
	sta $0300,x
	sta $0400,x
	sta $0500,x
	sta $0600,x
	sta $0700,x

	lda #255
	sta $0200,x  		; Put all the sprites offscreen

	inx
	bne clear_ram

	;; set collision address to $2000 (00 already set in start_collision_low)
	lda #$20
	sta start_collision_high


	;; DISPLAY CODE: this is where the code starts to initialize
	;; (place) the sprites used to track the position of the player's
	;; sprite. 

	lda #0
	sta $02D2
	sta $02DA
	sta $02D6
	sta $02DE
	sta $02E2
	sta $02EA
	sta $02E6
	sta $02EE
	sta $02F2
	sta $02FA
	sta $02F6
	sta $02FE

	; set y-value on screen for debug to $A0 (160 pixels down)
	lda #$A0 
	sta $02E0	; high-digit (inner x)
	sta $02E4	; low-digit
	sta $02E8	; high-digit (inner y)
	sta $02EC	; low-digit
	
	; set y-value on screen for debug to $A8 (168 pixels down)
	lda #$A8
	sta $02F0	; high-digit (x)
	sta $02F4
	sta $02F8	; high-digit (y)
	sta $02FC

	; set y-value on screen for debug to $B0 (176 pixels down)
	lda #$B0	
	sta $02D0
	sta $02D4
	sta $02D8
	sta $02DC

	; set x-values for two-digit numbers for testing
	; (See "extracting_tens" function below)
	; highx lowx highy lowy
	lda #$60
	sta $02D3
	sta $02E3
	sta $02F3
	lda #$68
	sta $02D7
	sta $02E7
	sta $02F7
	lda #$70
	sta $02DB
	sta $02EB
	sta $02FB
	lda #$78
	sta $02DF
	sta $02EF
	sta $02FF
	;; END DISPLAY CODE


wait_vblank_2:
	bit $2002
	bpl wait_vblank_2


load_palete:
	lda $2002
	lda #$3F
	sta $2006
	lda #$00
	sta $2006
	ldx #0
palette_loop:
	lda palette_table,x
	sta $2007
	inx
	cpx #$20
	bne palette_loop

load_map:
	lda $2002
	lda #$20
	sta $2006
	lda #$00
	sta $2006

	; NOTE: This loop breaks if the second table
	; value is zero, i.e. there are "zero" tiles
	; of the first tile!
	; SECOND NOTE: Using two entries of 255 ($FF) 
	; to indicate the end of table data, so we 
	; know when to kill the loop.
	
	ldx #1
	lda map_data_table_one,x
	tay
	dex
map_load_loop:
	lda map_data_table_one,x
inner_map_loop:
	sta $2007 			; write one tile
	dey					; decrease y-count cause we've done one...
	cpy #0
	bne inner_map_loop
	inx
	inx
	inx
	lda map_data_table_one,x ; load value two-higher
	tay
	cpy #255			; last two values assumed to be 255,255
	beq done_map_loop	; so if we hit that, we're done
	dex					; at any rate, y is loaded properly...
	jmp map_load_loop	; so let's start the loop over again
done_map_loop:
	lda #0
	sta $2005			; reset the scrolling
	lda #255
	sta $2005

	; TODO:
	; store magic numbers for player
	; position for proof of concept
	lda #0
	sta player_inner_x
	sta player_inner_y

	lda #80  		;not lda #80, that's 80/8 = 10
	sta player_x
	lda #16			;not #16, that's 16/8 = 2
	sta player_y
	lda #10
	sta player_abs_x
	lda #2
	sta player_abs_y

	;; SPRITE LOGIC - BEGIN ;;
	ldx #0
load_sprite:
	lda sprite_data,x
	sta $0200,x
	inx
	cpx #4
	bne load_sprite

	lda #$02
	sta $4014


	lda #$1E 		;; PPUMASK 0001 1110 - rgb and monochrome untouched
	sta $2001		;; sprite and bkgd on, don't hide sprite/bg clip
	lda #$88 		;; PPUCTRL 10001000 - Nmi on, (unused/extpins), sprites8x8, bg$0000, 
	sta $2000 		;; spr$1000, inc+1, AA=$2000 (namespace start)


main_loop:

check_ctrl_start:
	lda #1
	sta $4016
	lda #0
	sta $4016
	ldx #8
read_ctrl_loop:
	pha
	lda $4016
	and #%00000011
	cmp #1
	pla
	ror
	dex
	bne read_ctrl_loop
	sta button_array

	lda nmi_flags
	and #%00000100
	bne main_loop_end

	
	jsr check_controller
	jsr update_xy
	jsr update_tiles
	jsr check_collision

main_loop_end:
	jmp main_loop

check_collision:
	;TODO: still not sure whether to use this or EOR the DONKY


	; This code assumes the screen doesn't scroll;
	; you would have to compensate for scrolling otherwise.
	; For now, assume a starting point of $2000 in our nametable:
	lda #$20
	sta start_collision_high
	lda #0
	sta start_collision_low


	; Start by finding where we are on BG nametable
	ldy #0
	lda #0
	sta z3

	lda player_abs_y
	sec
	sbc #1
	tax
	beq done_bg_collision_find_y

	clc
	lda #0
bg_collision_find_y:
	adc #32
	sta z3
	bcc bg_collision_find_y_dex
	iny 							; Store number of carries in y
	clc
bg_collision_find_y_dex:
	dex
	bne bg_collision_find_y
done_bg_collision_find_y:

	; We need to subtract 1 to find our "starting" point for grabbing
	; the correct nametable address:
	; X--
	; -P-
	; ---
	; (X marks the spot)
	dec z3

	; Also, we don't add 1 more than the x offset (yes, there is 
	; zero-indexing but $2000 is the first spot, so we're "adding and 
	; subtracting" one, i.e., not doing anything):
	clc
	lda z3
	adc player_abs_x
	sta z3


	lda z3
	sta start_collision_low  		; LSB for nametable

	tya
	clc
	adc #$20 						; MSB for nametable
	sta start_collision_high

	lda nmi_flags
	ora #%00000100					; turn collision flag on
	sta nmi_flags

	rts

check_controller:
	
check_right:
	lda button_array
	and #%10000000
	beq checkleft
	; logic for moving right
	moveright:
		; set dx to positive
		lda #1
		sta dx
checkleft:
	lda button_array
	and #%01000000 
	beq checkup
	; logic for moving left
	moveleft:
		; set dx to negative
		lda #255
		sta dx
checkup:
	lda button_array
	and #%00010000 
	beq checkdown
	moveup:
		; set dy to negative
		lda #255
		sta dy
		lda #1
checkdown:
	lda button_array
	and #%00100000 
	beq move_now
	movedown:
		; set dy to negative
		lda #1
		sta dy
move_now:		
	rts

;; BEFORE WE DO ANY UPDATE_XY, just do the following:
;; Check if there's a collision using player_abs_x or player_abs_y
;; if there is, just zero out dx and/or dy, then movement won't happen anyways
;; NOTE: this would still "trigger" update_xy (i.e. the code will still run)
;; so this is probably wasted cycles (we might want a quick dx/dy check and just skip update_xy if necessary)


update_xy:
	; x coordinate relative to one screen
	; (zero-indexed) 32x30
	lda nmi_flags
	and #%00000010
	beq continue_update_xy
	rts

continue_update_xy:
	; 4 Cases:
	; 1) player_inner_x and player_inner_y are both non-zero:
	; For both to be non-zero, the sprite is currently "occupying"
	; all adjacent tiles, so collisions are not possible
	; 2) Only player_inner_x is non-zero:
	; In this case we need only check UD collisions (four tiles in 
	; total, since sprite is "in-between tiles" horizontally):
	; [bits: -65---10 I believe?]
	; 3) Only player_inner_y is non-zero:
	; Similar to 2), but checking LR collisions (four tiles total)
	; [bits: ---432-0 I believe?]
	; 4) player_inner_x and player_inner_y are both zero:
	; The four collisions we check are exactly one tile each
	; direction (ULDR)
	; [bits: 76543210]

	; Note: use of 'rts' throughout function saves time/space
	; rather than using something like:
	; lda #0
	; sta dx
	; sta dy
	; ; (and then continuing the function...)

	;; There are 36 cases in total:
	;; inner_x: zero or non-zero
	;; inner_y: zero or non-zero
	;; dx: 1, 0, -1
	;; dy: 1, 0, -1
	;; => 2 x 2 x 3 x 3 = 36 cases
	;; Make sure you catch'em all!!!

	; Easiest cases: dx = dy = 0:
	; (4 cases, 32 remaining)
	lda dx
	bne at_least_one_nonzero_d
	lda dy
	bne at_least_one_nonzero_d
	rts

at_least_one_nonzero_d:

	; Next easiest cases: player_inner_x = player_inner_y != 0
	; (4 cases, 28 remaining)
	lda player_inner_x
	beq at_least_one_inner_zero
	lda player_inner_y
	beq at_least_one_inner_zero
	; Don't need collisions while you're "in between" blocks:
	jmp add_dx_dy

	
at_least_one_inner_zero:

update_xy_check_bit_7:
	; 33) dx: -1, dy: -1, inner_x: 0, inner_y: 0
	
	lda collision_flags
	and #%10000000
	beq update_xy_check_bit_6
	lda player_inner_y
	bne update_xy_check_bit_6
	lda player_inner_x
	bne update_xy_check_bit_6
	lda dx
	bpl update_xy_check_bit_6
	lda dy
	bpl update_xy_check_bit_6
	lda collision_flags
	and #%00010000
	bne update_xy_check_bit_7_zero_dx
	lda collision_flags
	and #%01000000
	bne update_xy_check_bit_7_zero_dy
	rts
update_xy_check_bit_7_zero_dx:
	lda #0
	sta dx
	jmp update_xy_check_bit_6
update_xy_check_bit_7_zero_dy:
	lda #0
	sta dy

update_xy_check_bit_6:
	; bit 6 can be reached in 6 cases:
	; 9) dx: 0, dy: -1, inner_x: 0, inner_y: 0
	; 11) dx: 0, dy: -1, inner_x: 1, inner_y: 0
	; 21) dx: 1, dy: -1, inner_x: 0, inner_y: 0
	; 23) dx: 1, dy: -1, inner_x: 1, inner_y: 0
	; 33) dx: -1, dy: -1, inner_x: 0, inner_y: 0
	; 35) dx: -1, dy: -1, inner_x: 1, inner_y: 0
	lda collision_flags
	and #%01000000
	beq update_xy_check_bit_5
	lda player_inner_y
	bne update_xy_check_bit_5
	lda dy
	bpl update_xy_check_bit_5
	lda #0
	sta dy

update_xy_check_bit_5:
	; 11) dx: 0, dy: -1, inner_x: 1, inner_y: 0
	; 21) dx: 1, dy: -1, inner_x: 0, inner_y: 0
	; 23) dx: 1, dy: -1, inner_x: 1, inner_y: 0
	; 35) dx: -1, dy: -1, inner_x: 1, inner_y: 0

	lda collision_flags
	and #%00100000
	beq update_xy_check_bit_4
	lda dx
	bmi update_xy_check_bit_5_case_35
	lda dy 
	bpl update_xy_check_bit_4
	lda player_inner_y
	bne update_xy_check_bit_4

	; dx != -1, dy = -1, inner_y = 0
	lda dx
	bne update_xy_check_bit_5_cases_21_23
update_xy_check_bit_5_case_11:
	lda player_inner_x
	beq update_xy_check_bit_4
	; dx = 0, we're moving up and there's a block there
	rts
update_xy_check_bit_5_case_35:
	lda dy
	bpl update_xy_check_bit_4
	lda player_inner_y
	bne update_xy_check_bit_4
	lda player_inner_x
	beq update_xy_check_bit_4
	lda #0
	sta dy
	jmp update_xy_check_bit_4
update_xy_check_bit_5_cases_21_23:
	; dx = 1, dy = -1, inner_y = 0
	; We're moving up-right and there's a block there
	lda #0
	sta dx
	lda player_inner_x
	beq update_xy_check_bit_4
	lda #0
	sta dy

update_xy_check_bit_4:
	; 25) dx: -1, dy: 0, inner_x: 0, inner_y: 0
	; 26) dx: -1, dy: 0, inner_x: 0, inner_y: 1
	; 29) dx: -1, dy: 1, inner_x: 0, inner_y: 0
	; 30) dx: -1, dy: 1, inner_x: 0, inner_y: 1
	; 33) dx: -1, dy: -1, inner_x: 0, inner_y: 0
	; 34) dx: -1, dy: -1, inner_x: 0, inner_y: 1

	lda collision_flags
	and #%00010000
	beq update_xy_check_bit_3
	lda dx
	bpl update_xy_check_bit_3
	lda player_inner_x
	bne update_xy_check_bit_3
	lda #0
	sta dx

update_xy_check_bit_3:
	; 13) dx: 1, dy: 0, inner_x: 0, inner_y: 0
	; 14) dx: 1, dy: 0, inner_x: 0, inner_y: 1
	; 17) dx: 1, dy: 1, inner_x: 0, inner_y: 0
	; 18) dx: 1, dy: 1, inner_x: 0, inner_y: 1
	; 21) dx: 1, dy: -1, inner_x: 0, inner_y: 0
	; 22) dx: 1, dy: -1, inner_x: 0, inner_y: 1
	; 35) dx: -1, dy: -1, inner_x: 1, inner_y: 0
		
	lda collision_flags
	and #%00001000
	beq update_xy_check_bit_2
	lda dx
	bmi update_xy_check_bit_3_case_35
	lda player_inner_x
	bne update_xy_check_bit_2
	beq update_xy_check_bit_3_zero_dx
update_xy_check_bit_3_case_35:
	lda dx
	bpl update_xy_check_bit_2
	lda player_inner_x
	beq update_xy_check_bit_2
	lda player_inner_y
	bne update_xy_check_bit_2
	lda #0
	sta dy

update_xy_check_bit_3_zero_dx:
	lda #0
	sta dx

update_xy_check_bit_2:
	; 26) dx: -1, dy: 0, inner_x: 0, inner_y: 1
	; 29) dx: -1, dy: 1, inner_x: 0, inner_y: 0
	; 30) dx: -1, dy: 1, inner_x: 0, inner_y: 1
	; 34) dx: -1, dy: -1, inner_x: 0, inner_y: 1

	lda collision_flags
	and #%00000100
	beq update_xy_check_bit_1

	lda dx
	bpl update_xy_check_bit_1
	lda player_inner_x
	bne update_xy_check_bit_1
	lda dy
	bne update_xy_check_bit_2_cases_29_30_34
update_xy_check_bit_2_case_26:	
	lda player_inner_y
	beq update_xy_check_bit_1
	lda #0
	sta dx
	jmp update_xy_check_bit_1

update_xy_check_bit_2_cases_29_30_34:
	lda dy
	bpl update_xy_check_bit_2_cases_29_30
	lda player_inner_y
	beq update_xy_check_bit_1
	lda #0
	sta dx
	jmp	update_xy_check_bit_1
update_xy_check_bit_2_cases_29_30:
	lda player_inner_y
	beq update_xy_check_bit_2_zero_dx_dy
update_xy_check_bit_2_case_30:
	lda #0
	sta dx
	jmp update_xy_check_bit_1
update_xy_check_bit_2_zero_dx_dy:
	lda #0
	sta dy
	
update_xy_check_bit_1:
	; 5) dx: 0, dy: 1, inner_x: 0, inner_y: 0
	; 7) dx: 0, dy: 1, inner_x: 1, inner_y: 0
	; 17) dx: 1, dy: 1, inner_x: 0, inner_y: 0
	; 19) dx: 1, dy: 1, inner_x: 1, inner_y: 0
	; 29) dx: -1, dy: 1, inner_x: 0, inner_y: 0
	; 31) dx: -1, dy: 1, inner_x: 1, inner_y: 0
	lda collision_flags
	and #%00000010
	beq update_xy_check_bit_0

	lda dy
	beq update_xy_check_bit_0
	bmi update_xy_check_bit_0
	lda player_inner_y
	bne update_xy_check_bit_0
	lda #0
	sta dy
	
update_xy_check_bit_0:
	; 7) dx: 0, dy: 1, inner_x: 1, inner_y: 0
	; 14) dx: 1, dy: 0, inner_x: 0, inner_y: 1
	; 17) dx: 1, dy: 1, inner_x: 0, inner_y: 0
	; 18) dx: 1, dy: 1, inner_x: 0, inner_y: 1
	; 19) dx: 1, dy: 1, inner_x: 1, inner_y: 0
	; 22) dx: 1, dy: -1, inner_x: 0, inner_y: 1
	; 31) dx: -1, dy: 1, inner_x: 1, inner_y: 0
	
	lda collision_flags
	and #%00000001
	beq add_dx_dy

	lda dx
	bmi update_xy_check_bit_0_case_31
	beq update_xy_check_bit_0_case_7
	; dx = 1; check cases 14, 17, 19, and 22:
	lda dy
	bmi update_xy_check_bit_0_case_22
	beq update_xy_check_case_14
	; dy = 1, check cases 17, 18, 19:
update_xy_check_cases_17_19:
	lda player_inner_y
	beq update_xy_check_bit_0_clear_dx_dy
	; inner_y = 1
	lda player_inner_x
	bne add_dx_dy
	lda #0
	sta dx
	jmp add_dx_dy
	

update_xy_check_case_14:
	; dy = 0
	lda player_inner_x
	bne add_dx_dy
	lda player_inner_y
	bne update_xy_check_bit_0_clear_dx_dy
	beq add_dx_dy

update_xy_check_bit_0_case_22:
	; dx = 1, dy = -1
	; 22) dx: 1, dy: -1, inner_x: 0, inner_y: 1
	lda player_inner_x
	bne add_dx_dy
	lda player_inner_y
	lda #0
	sta dx


update_xy_check_bit_0_case_7:
	; dx = 0, check case 7:
	lda dy
	beq add_dx_dy
	bmi add_dx_dy
	lda player_inner_x
	beq add_dx_dy
	lda player_inner_y
	bne add_dx_dy
	beq update_xy_check_bit_0_clear_dx_dy

update_xy_check_bit_0_case_31:
	; dx = -1
	; 31) dx: -1, dy: 1, inner_x: 1, inner_y: 0
	lda dy
	beq add_dx_dy
	bmi add_dx_dy
	lda player_inner_x
	beq add_dx_dy
	lda player_inner_y
	bne add_dx_dy

update_xy_check_bit_0_clear_dx_dy:
	lda #0
	
	sta dy
	lda player_inner_x
	bne add_dx_dy
	lda #0
	sta dx

add_dx_dy:
	lda player_x
	clc
	adc dx
	sta player_x

	lda player_inner_x
	clc
	adc dx
	sta player_inner_x
	
	; check if > 7 (new abs_x)
	cmp #8
	beq change_abs_x

	; check if < 0 (new_abs_x)
	cmp #255
	beq change_abs_x
	jmp done_inner_x

change_abs_x:
	; player_inner_x is -1 or 8, reset it by doing lsr 5 times:
	;  8 = 00001000 => 00000100 => 00000010 => 00000001 => 00000000 => 00000000
	; -1 = 11111111 => 01111111 => 00111111 => 00011111 => 00001111 => 00000111
	lsr player_inner_x
	lsr player_inner_x
	lsr player_inner_x
	lsr player_inner_x
	lsr player_inner_x
	
	lda player_abs_x
	clc
	adc dx
	sta player_abs_x
done_inner_x:
	; check y now
start_y:

	lda player_y
	clc
	adc dy
	sta player_y

	lda player_inner_y
	clc
	adc dy
	sta player_inner_y
		

	cmp #8
	beq change_abs_y

	cmp #255
	beq change_abs_y
	jmp done_inner_y
	
change_abs_y:
	; player_inner_y is -1 or 8, reset it by doing lsr 5 times:
	;  8 = 00001000 => 00000100 => 00000010 => 00000001 => 00000000 => 00000000
	; -1 = 11111111 => 01111111 => 00111111 => 00011111 => 00001111 => 00000111
	lsr player_inner_y
	lsr player_inner_y
	lsr player_inner_y
	lsr player_inner_y
	lsr player_inner_y

	lda player_abs_y
	clc
	adc dy
	sta player_abs_y

done_inner_y:

	rts
update_tiles:

	;; TO DO DEL PLACEHOLDERS
	; e7 e6 e5 e4 (inner x low) e3 e2 e1 e0 (inner x high)
	; ef ee ed ec (inner y low) eb ea e9 e8 (inner y high)
	; f7 f6 f5 f4 (abs x low)  f3 f2 f1 f0 (abs x high)
	; ff fe fd fc (abs y low)  fb fa f9 f8 (abs y high)

	;; inner_x tile number: $02E5 (high)	

	lda #0
	sta z
	lda player_abs_x
	ldy #$D1
	jsr extracting_tens
	lda player_abs_y
	ldy #$D9
	jsr extracting_tens

	;lda player_inner_x
	;lda dx 			; test dx if you want :)
	lda collision_flags
	ldy #$E1
	jsr extracting_tens
	lda #0
	lda player_inner_y
	;lda dy  		; test dy if you want :)
	;lda z3
	ldy #$E9
	jsr extracting_tens
	lda player_x
	ldy #$F1
	jsr extracting_tens
	lda player_y
	ldy #$F9
	jsr extracting_tens
	
	
	lda nmi_flags
	ora #%00000010  	; turn on update gfx flag
	sta nmi_flags

	lda player_x
	sta $0203
	lda player_y
	sta $0200

	lda #0
	sta dx
	sta dy

	rts

extracting_tens:
	sta z

	ldx #0  			; Use this to keep track of tens column
begin_ten_extraction:

	lda z
	and #$F0

	;; loop:
	;; - is it less than 10? then just take the number and that's your right digit; exit loop
	beq store_high_digit
	;; - is it more than 10? then just subtract 10, add that to your left digit, and repeat loop
	lda z
	sec
	sbc #$10

	; x is where we keep track of "tens" (hexes?) column:
	sta z
	inx
	jmp begin_ten_extraction

store_high_digit:
	; store the new tens digit before moving to the units digit
	txa
	sta $0200,y
	
store_low_digit:

	ldx #0
loop_low_digit:
	lda z
store_value:
	iny
	iny
	iny
	iny
	sta $0200,y
	lda #0
	sta z
	rts

palette_table:
	db $0F, $18, $16, $32
	db $0F, $18, $16, $32
	db $0F, $18, $16, $32
	db $0F, $18, $16, $32
	db $0F, $11, $12, $11
	db $0F, $11, $14, $15
	db $0F, $16, $17, $19
	db $0F, $19, $1A, $1B


map_data_table_one:
	; let's build a simple map so we can see if it works
	db 1,33, 0,30, 1,2, 0,15, 1,5, 0,10, 1,2, 0,30 
	db 1,10, 0,8, 1,19, 0,15, 1,1, 0,10, 1,3, 0,30
	db 1,30, 0,2, 1,2, 0,5, 1,5, 0,5, 1,5, 0,5, 1,4, 0,1, 1,5
    db 0,10, 1,7, 0,10, 1,2
    db 0,30, 1,2, 0,30, 1,2, 0,30, 1,2, 0,30, 1,2, 0,30, 1,2
    db 0,30, 1,2, 0,30, 1,2, 0,30, 1,2, 0,30, 1,2, 0,30, 1,2
    db 0,30, 1,2, 0,30, 1,2, 0,30, 1,2, 0,30, 1,2, 0,30, 1,2
    db 0,30, 1,2, 0,30, 1,2, 0,30, 1,2, 0,30, 1,33
	db 255,255


sprite_data:
	db 16, $10, $01, 80

	org $FFFA
	dw nmi
	dw reset
	dw irq

chr_rom_start:

bg_start:

	; empty bkg tile
    db 0,0,0,0,0,0,0,0
    db 0,0,0,0,0,0,0,0

    ; test block - tile 1
    db %11111111
    db %10000001
    db %10000001
    db %10000001
    db %10000001
    db %10000001
    db %10000001
    db %11111111
    db $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF

bg_end:

	ds 4096 - (bg_end - bg_start)	; Ensure correct size of background tiles (4096 bytes)


sprite_start:

	; TEST TILE ZERO - solid block after digits (tile "A")
	

	; 0
	db %11111111
	db %10000001
	db %10111101
	db %10111101
	db %10111101
	db %10111101
	db %10000001
	db %11111111
	db 0,0,0,0,0,0,0,0 ; bitplane 2

	; 1
	db %11111111
	db %11100111
	db %11100111
	db %11100111
	db %11100111
	db %11100111
	db %11100111
	db %11111111
	db 0,0,0,0,0,0,0,0 ; bitplane 2

	; 2
	db %11111111
	db %11100111
	db %10000011
	db %00110011
	db %11001111
	db %10111111
	db %10000111
	db %11111111
	db 0,0,0,0,0,0,0,0 ; bitplane 2

	; 3
	db %11111111
	db %11100111
	db %10000011
	db %11111011
	db %11000111
	db %11111011
	db %10000111
	db %11111111
	db 0,0,0,0,0,0,0,0 ; bitplane 2

	; 4
	db %11111111
	db %11100111
	db %11000111
	db %10100111
	db %00000111
	db %11100111
	db %11100111
	db %11111111
	db 0,0,0,0,0,0,0,0 ; bitplane 2

	; 5
	db %11111111
	db %00000001
	db %00111111
	db %00000111
	db %11111011
	db %11111011
	db %10000111
	db %11111111
	db 0,0,0,0,0,0,0,0 ; bitplane 2

	; 6
	db %11011111
	db %10111111
	db %01111111
	db %00000111
	db %01111011
	db %01111011
	db %10000111
	db %11111111
	db 0,0,0,0,0,0,0,0 ; bitplane 2

	; 7
	db %11111111
	db %10000001
	db %11111101
	db %11111011
	db %11110111
	db %11101111
	db %11011111
	db %11111111
	db 0,0,0,0,0,0,0,0 ; bitplane 2

	; 8
	db %10000111
	db %01111011
	db %01111011
	db %10000111
	db %10000111
	db %01111011
	db %10000111
	db %11111111
	db 0,0,0,0,0,0,0,0 ; bitplane 2

	; 9
	db %10000111
	db %01111011
	db %01111011
	db %10000011
	db %11110111
	db %11101111
	db %10111111
	db %11111111
	db 0,0,0,0,0,0,0,0 ; bitplane 2

	db %00000000    
    db %01100000 ; A
    db %10010000
    db %10010000
    db %11110000
    db %10010000 ; A5
    db %10010000
    db %10010000
    db 0,0,0,0,0,0,0,0

    db %00000000 ; B
    db %11100000 
    db %10010000
    db %10010000
    db %11100000 ; B5
    db %10010000
    db %10010000
    db %11100000
    db 0,0,0,0,0,0,0,0

    db %11110000 ; C
    db %10000000 ; 
    db %10000000 ; 
    db %10000000 ; 
    db %10000000 ; 
    db %10000000 ; 
    db %10000000 ; 
    db %11110000 ; 
    db 0,0,0,0,0,0,0,0

    db %00000000
    db %11100000 ; D 
    db %10010000
    db %10010000
    db %10100000
    db %11000000
    db %00000000
    db %00000000
    db 0,0,0,0,0,0,0,0

    db %00000000
    db %01110000 ; E
    db %01000000
    db %01000000
    db %01110000
    db %01000000
    db %01000000
    db %01110000
    db 0,0,0,0,0,0,0,0

    db %00000000
    db %01110000 ; F (moved top row of 1s over one)
    db %01000000
    db %01000000
    db %01110000
    db %01000000
    db %01000000
    db %00000000
    db 0,0,0,0,0,0,0,0

	; test block (solid square)
	db $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
	db $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF

sprite_end:
	ds 4096 - (sprite_end - sprite_start)	; Ensure correct size of sprite tiles (4096 bytes)


