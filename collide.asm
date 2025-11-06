	org $BFF0
	db "NES", $1A
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

clock_cycle equ $09 			; These two labels are for delaying/slowing down
clock_cycle_end equ $0A			; movement


nmi_flags equ $F0 				; keeping track of whether waiting/ready (1 yes, 0 no): -----LVN (L-Logic, V-video, N-NMI)
collision_flags equ $F0 		; Flags for collisions: ULRD???? (up, left, right, down, last four for damage :D )

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

	; 1) check nmi was completed
	lda nmi_flags
	and #1 			; check bit 1, did nmi finish?
	bne finish_nmi  ; if it didn't, get to the end pls

	; we're in an NMI, turn the bit on:
	lda nmi_flags
	ora #1
	sta nmi_flags

	; 2) for now I'm not sure we need to 
	; use gfx flag - we're only updating sprites?

gfx_update:
	lda #$02
	sta $4014
	lda nmi_flags
	and #%11111101 			;We're turning bit 1 off ("2")
	sta nmi_flags
	

finish_nmi:
	lda clock_cycle
	clc
	adc #1
	sta clock_cycle

	cmp clock_cycle_end
	bne continue_after_cycle_check
	lda #0
	sta clock_cycle
continue_after_cycle_check:
	;TODO: theoretically other stuff would happen here...

	lda nmi_flags
	and #$FE  		; turn off bit 1 - we've now "completed" previous NMI
	sta nmi_flags

			pla
			tay
			pla
			tax
		plp
	pla
	rti

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

	; set y-value on screen for debug to 40 (inner)
	lda #40 
	sta $02E0	; high-digit (inner x)
	sta $02E4	; low-digit
	sta $02E8	; high-digit (inner y)
	sta $02EC	; low-digit
	
	; set y-value on screen for debug to 48 (x/y)
	lda #48
	sta $02F0	; high-digit (x)
	sta $02F4
	sta $02F8	; high-digit (y)
	sta $02FC

	; set y-value on screen for abs val debug to 56 (abs x/y)
	lda #56	
	sta $02D0
	sta $02D4
	sta $02D8
	sta $02DC

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
	sta clock_cycle
	lda #1
	sta clock_cycle_end

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
	cpx #44
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

	lda clock_cycle  			; check that clock_cycle is appropriate (for now we're assuming
	bne main_loop_end 			; 0 means "you can update x/y positions")
	lda nmi_flags				; Now we check that we're not already waiting for a gfx update...
	and #2  
	bne main_loop_end   		; go to end of loop if bit 1 is turned on....

	jsr check_collision
	jsr check_controller
	jsr update_xy

main_loop_end:
	jmp main_loop

check_collision:
	lda collision_flags
	and #%00001111				; turn off collisions to start check
	sta collision_flags
	; We need to perform two checks:
	; Check vertical collision if dy isn't zero
	; Check horizontal collision if dx isn't zero
	; I think the trick here is:
	
	; 1) Figure out which tile is directly above our hero, zero out dy if moving up
	lda player_abs_y
	; multiply by 32 (number of tiles in each row):
	asl
	asl
	asl
	asl
	asl
	sec 
	sbc #32 					; subtract a row since we want the tile above our hero...
	sta z 						; put this value in "z" - overflow not possible (must be <= 240) 

	; now add the x value of our hero to find the tile exactly above...
	lda player_abs_x
	clc
	adc z
	sta z
	
	lda z
	clc
	adc #1 						; add one more due to zero-indexing (but we DON't add 1 to y
	sta z 						; above because we want the row above our hero)


	sta z3

	ldy #1
find_first_tile_loop:
	lda map_data_table_one,y
	sta z2

	lda z
	sec
	sbc z2
	bmi found_tile_minus  		; problematic for values > 127? (after subtracting)
	sta z
	lda #0
	sta z2
	lda z  				; lda for beq
	beq found_tile
	iny
	iny
	jmp find_first_tile_loop
found_tile_minus:
	lda z2
	sec
	sbc z
	sta z2
	lda #0
	sta z
found_tile:
	dey 						; Decrease y by one to check what type of tile
	lda map_data_table_one,y
	cmp #1
	bne check_collision_left
	lda collision_flags
	ora #%10000000  			; There's a collision above
	sta collision_flags
check_collision_left:
	iny
	; 2) 31 higher gives to the left, zero out dx if moving left
	lda #31						
	sta z
	sec
	sbc z2
	bmi found_left_tile_minus
	sta z
	lda #0
	sta z2
	lda z
	beq found_left_tile
	coll_left_loop:
		iny
		iny 						; we had to do it again, jump up an index
		lda map_data_table_one,y
		sta z2
		lda z
		sec
		sbc z2		
		bmi found_left_tile_minus
		sta z
		lda #0
		sta z2
		lda z
		beq found_left_tile
		jmp coll_left_loop
found_left_tile_minus:
	lda z2
	sec
	sbc z
	sta z2	
	lda #0
	sta z
found_left_tile:
	dey 						; Decrease y by one to check what type of tile
	lda map_data_table_one,y
	cmp #1
	bne check_collision_right
	lda collision_flags
	ora #%01000000  			; There's a collision above
	sta collision_flags
check_collision_right:
	iny
	; 3) 2 higher than that gives to the right, zero out dx if moving right
	lda #2			
	sta z
	sec
	sbc z2
	bmi found_right_tile_minus
	sta z
	lda #0
	sta z2
	lda z
	beq found_right_tile
	coll_right_loop:
		iny
		iny
		lda map_data_table_one,y
		sta z2
		lda z
		sec
		sbc z2
		bmi found_right_tile_minus
		sta z
		lda #0
		sta z2
		lda z
		beq found_right_tile		
		jmp coll_right_loop
found_right_tile_minus:
	lda z2
	sec
	sbc z
	sta z2
	lda #0
	sta z
found_right_tile:
	dey 						; Decrease y by one to check what type of tile
	lda map_data_table_one,y
	cmp #1
	bne check_collision_down
	lda collision_flags
	ora #%00100000  			; There's a collision above
	sta collision_flags
check_collision_down:
	iny 
	; 4) 31 higher gives below, zero out dy if moving down
	lda #31						
	sta z
	sec
	sbc z2
	bmi found_down_tile
	sta z
	lda #0
	sta z2
	lda z
	beq found_down_tile
	coll_down_loop:
		iny
		iny
		lda map_data_table_one,y
		sta z2
		lda z
		sec
		sbc z2
		bmi found_down_tile
		sta z
		lda #0
		sta z2
		lda z
		beq found_down_tile
		jmp coll_down_loop
found_down_tile:
	dey 						; Decrease y by one to check what type of tile
	lda map_data_table_one,y
	cmp #1
	bne done_checking_collision
	lda collision_flags
	ora #%00010000  			; There's a collision above
	sta collision_flags
	lda #0
	sta z
	sta z2
done_checking_collision:
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
	
	lda player_x
	clc
	adc dx
	sta player_x

	lda player_inner_x
	clc
	adc dx
	sta player_inner_x

	;; check if adjacent direction is wall;
	;; if it is, we don't move!
	cmp #1
	beq check_wall_right
	
	; check if > 7 (new abs_x)
	cmp #8
	beq change_abs_x

	cmp #255
	;; check if adjacent direction is wall;
	;; if it is, we don't move!
	beq check_wall_left
	jmp done_inner_x

check_wall_left:
	lda collision_flags
	and #%01000000
	
	beq change_abs_x
	inc player_x
	inc player_inner_x
	inc dx
	jmp done_inner_x

check_wall_right:
	lda collision_flags
	and #%00100000
	beq done_inner_x
	dec player_x
	dec player_inner_x
	dec dx 
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
	
	; check if > 7 (new abs_y)
	cmp #1
	;; check if adjacent direction is wall;
	;; if it is, we don't move!
	beq check_wall_down

	cmp #8
	beq change_abs_y

	cmp #255
	;; check if adjacent direction is wall;
	;; if it is, we don't move!
	beq check_wall_up
	jmp done_inner_y

check_wall_up:
	lda collision_flags
	and #%10000000
	beq change_abs_y
	inc player_y
	inc player_inner_y
	inc dy
	jmp done_inner_y

check_wall_down:
	lda collision_flags
	and #%00010000
	beq done_inner_y
	dec player_y
	dec player_inner_y
	dec dy 
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
	;lda clock_cycle
	lda collision_flags
	ldy #$E1
	jsr extracting_tens
	lda #0
	lda player_inner_y
	;lda dy  		; test dy if you want :)
	;lda clock_cycle_end
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
	db 1,33, 0,30, 1,2, 0,30, 1,2, 0,30, 1,33, 255,255


sprite_data:
	db 16, $10, $01, 80

	db $36, $0, $03, $40 			
	db $46, $1, $03, $40 			
	db $56, $2, $03, $40 			
	db $66, $3, $03, $40 			

	db $76, $4, $03, $40 			
	db $86, $5, $03, $40 			
	db $96, $6, $03, $40 			
	db $A6, $7, $03, $40 			

	db $B6, $8, $03, $40 			
	db $C6, $9, $03, $40 			

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
