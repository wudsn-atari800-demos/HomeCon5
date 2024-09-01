;
;	HomeCon 5 Invitation
;
;	(c) 2009-12-29 JAC!
;	(r) 2010-01-02 JAC! NTSC fix
;
;	Created for and released with the Atari New Year's Disk 2010.
;
;	Created using WUDSN IDE and ATASM.
;	WUDSN IDE - the free integrated Atari 8-bit development plugin for Eclipse
;	Visit https://www.wudsn.com for details.
;
;	Thanks to Nils Feske for arranging the music.
;	Thanks to FOX for the inflate routine.
;	Thanks to Stephen Anderson for testing the NTSC fix on real hardware.
;	Vote on Pouet:  http://www.pouet.net/prod.php?which=54268.
;
;	The original "Rainbow on Mars" was ported from the 1991 Amiga release "Joyride" from Phenomena in 1992.
;	I love that one even today.
;	Visit on Pouet: https://www.pouet.net/prod.php?which=1621.
;
;	For some strange reason, I wanted to keep this one smaller than 16k.
;	... Just in case you wonder about the certain lines in the code ;-) 
;

; Zero page pointers
p1	= $80
p2	= $82
p3	= $84

; Zero page variables
x1	= $e0
x2	= $e1
x3	= $e2

; Zero page counters
dlicnt	= $fe
cnt	= $ff

; CMC replay routine
sound		= $5000
sound_init	= $5003
sound_replay	= $5006

; Original and shifted char set
chr		= $5800	;$400 bytes
chr2		= $5c00	;$400 bytes

tune		= $8400	;up to $8bff

; Inflated rainbow pictures
rainbowpic1	= $3000
rainbowpic2	= $4000
rainbowpic3	= $b000
rainbowpic4	= $6000
rainbowpic5	= $7000
rainbowpic6	= $c000
rainbowpic7	= $9000
rainbowpic8	= $a000

; Base for working area
base	= $e000

; Working areas for top effect
toplines = 64

topsm	= base
toplum1 = base+$200
topcol1 = base+$240
toplum2 = base+$280
topcol2 = base+$2c0
topmix1 = base+$300
topmix2 = base+$400
topmix3 = base+$500

; Working areas for rainbow effect
rainbowctab1	= base+$1000
rainbowctab2	= base+$1100
rainbowctab3	= base+$1200

;==========================================================
; Include standard macros
	.include "Macros.inc"

;==========================================================
; Pre-loader to fade the screen off

.bank
*	= $2000	;Begin of loader 
init_loader
	jsr init_system
	jsr init_fade
	rts
	

; Check if this system has 64k. 
; If not wait for a key and cold start.
init_system
	sei
	lda #0
	sta $d40e
	ldy #0
	lda $d301
	pha
	lda #$fe	;Disable OS ROM
	sta $d301
	ldx $e000	;Check if writeable
	inc $e000
	cpx $e000
	bne init_system1
	iny		;No
init_system1
	pla
	ora #2		;Disable BASIC ROM
	sta $d301
	lda #$40
	sta $d40e
	cli
	cpy #0
	bne init_fail
	rts

init_fail
	lda #14
	sta 708
	set $230,init_fail_dl
init_fail1
	lda $d40b
	clc
	adc 20
	sta $d40a
	sta $d01a
	lda $d20f
	and #12
	cmp #12
	beq init_fail1
	jmp $e474

	
init_fail_dl
	.byte $70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70,$70
	.byte $46
	.word init_fail_sm
	.byte $70,$06
	.byte $41
	.word init_fail_dl

init_fail_sm
	.sbyte "  REQUIRES 64K RAM. "
	.sbyte "PRESS KEY TO REBOOT."

init_fade
	lda #0
	sta x2
	ldx #8
init_fade2
	lda 704,x
	pha
	and #$f0
	sta x1
	pla
	and #15
	beq init_fade3
	sec
	sbc #1
	ora x1
	sta x2
init_fade3
	sta 704,x
	dex
	bpl init_fade2
	lda 20
	clc
	adc #2
init_fade4
	cmp 20
	bne init_fade4
	lda x2
	bne init_fade
	sta 559
	sta $d400
	rts

	.byte 0,155,10,13
	.byte "HomeCon 5 Intro by JAC!/Peter Dell. Original version for New Year's Disk 2010 compiled on 2009-12-31. NTSC fix on 2010-01-02." 
	.byte 0,155,10,13
.bank
*	= $2e2
	.word init_loader

;==========================================================
; Pre-loader to copy compressed animation under the OS
.bank

inflate_zp = $90
inflate_data = $d800

*	= $2000
init_copy
	sei
	lda #0
	sta $d40e
	sta inflate_zp
	sta inflate_zp+2

	lda $d301
	pha
	lda #$fe
	sta $d301

;	Copy compresses pic under OS ROM
	set p1,rainbowpic
	set p2,$e000
	ldx #$20
	jsr init_copy_pages

	lda #1
	sta $d00d

;	Uncompress to $3000-$afff
	lda #$e0
	sta inflate_zp+1
	lda #$30
	sta inflate_zp+3
	jsr inflate

;	Copy $5000-$5fff to $b000-$bfff
	lda #$50
	ldy #$b0
	ldx #$10
	stx $d000
	jsr init_copy_full_page


;	Copy $8000-$8fff to $c000-$cfff
	lda #$80
	ldy #$c0
	ldx #$10
	jsr init_copy_full_page

	pla
	sta $d301
	lda #$40
	sta $d40e
	cli
	rts

init_copy_full_page
	sta p1+1
	sty p2+1
	lda #0
	sta p1
	sta p2
init_copy_pages
	ldy #0
init_copy_pages1
	lda (p1),y
	sta (p2),y
	iny
	bne init_copy_pages1
	inc p1+1
	inc p2+1
	dex
	bne init_copy_pages1
	rts

inflate
	.include "HomeCon5-Inflate.asm"

rainbowpic
	.incbin "HomeCon5-Rainbow.pic.z"

*	= inflate_data
	.include "HomeCon5-Inflate-Data.asm"

.bank
*	= $2e2
	.word init_copy

;==========================================================
; Main routine start
.bank


*	= $2000
start	jsr init_screen
	jsr init_topsm
	jsr sound_reset
	jsr sound_start

	lda $d40b
	bne *-3
	lda #$40
	sta $d40e
	lda #250
	jsr wait

	ldx #0
scrollup
	stx dljump
	inx	
	stx rainbow_dli2+1

	lda #4
	jsr wait

	cpx #102
	bne scrollup

	lda #250
	jsr wait

loop	lda #2
	jsr wait
	jsr top_effect
	jmp loop

;==========================================================
init_screen
	sei
	lda #0
	sta $d40e
	sta $d20e
	sta cnt

	lda #$fe
	sta $d301
	set $fffa, nmi

	ldx #0
init_clear
	lda #0
	sta topmix1,x
	sta topmix2,x
	sta topmix3,x
	
	sta chr,x		;Not filled from file to save some space
	sta chr2,x
	
	cpx #48
	bcs init_clear1
	sta textline1,x		;Not filled from file to save same space
	sta textline2,x
	sta textline2end,x
init_clear1

	cpx #101
	bcs init_clear2
	sta rainbowdl,x		;Not filled from file to save same space
	lda #$0e
	sta rainbowdlc,x
init_clear2
	inx
	bne init_clear

init_chr
	lda chr+$100,x
	lsr
	sta chr2+$100,x
	lda chr+$200,x
	lsr
	sta chr2+$200,x
	lda chr+$300,x
	lsr
	sta chr2+$300,x
	inx
	bne init_chr

init_ctab
	txa
	lsr
	and #$f0
	sta x1
	txa
	and #63
	cmp #32
	bcc *+4
	eor #63
	lsr
	tay
	ora x1
	sta rainbowctab1,x
	tya
	sec
	sbc #3
	bpl *+6
	lda #0
	beq *+4
	ora x1
	sta rainbowctab2,x
	tya
	sec
	sbc #6
	bpl *+6
	lda #0
	beq *+4
	ora x1
	sta rainbowctab3,x

	inx
	bne init_ctab
	rts

;==========================================================
init_topsm
	set p1,topsm
	set p2,tophomepattern
	set p3,topconpattern
	jsr init_topsm_pattern
	rts

init_topsm_pattern
	ldy #0
	lda #40
	sta x3
init_topsm_pattern1
	lda (p3),y
	sta x1
	lda (p2),y
	sta x2
	jsr init_topsm_bytes
	dic p2
	dic p3
	dec x3
	bne init_topsm_pattern1

	ldy #0
init_col
	lda (p2),y
	pha
	and #$f0
	sta topcol1,y
	pla
	and #15
	sta toplum1,y
	
	lda (p3),y
	pha
	and #$f0
	sta topcol2,y
	pla
	and #15
	sta toplum2,y

	iny
	cpy #toplines
	bne init_col
	rts


init_topsm_bytes
	jsr init_topsm_byte
init_topsm_byte
	lda #0
	jsr init_topsm_4bit
	sta (p1),y
	dic p1
	rts

init_topsm_4bit
	jsr init_topsm_2bit
init_topsm_2bit
	jsr init_topsm_1bit
init_topsm_1bit
	asl x1
	rol
	asl x2
	rol
	rts

; Every pattern is 5*8 = 40 bytes plus toplines color bytes
tophomepattern	
	.byte ~01100001,~10111111,~11011111,~11111011,~11111100
	.byte ~01100001,~10111111,~11011111,~11111011,~11111100
	.byte ~01100001,~10110000,~11011001,~10011011,~00000000
	.byte ~01111111,~10110000,~11011001,~10011011,~11111100
	.byte ~01111111,~10110000,~11011001,~10011011,~11111100
	.byte ~01100001,~10110000,~11011001,~10011011,~00000000
	.byte ~01100001,~10111111,~11011001,~10011011,~11111100
	.byte ~01100001,~10111111,~11011001,~10011011,~11111100

	.byte $33,$35,$33,$33,$35,$37,$35,$35
	.byte $37,$39,$37,$37,$39,$3b,$39,$39
	.byte $3b,$3d,$3b,$3b,$3d,$3f,$3d,$3d
	.byte $3f,$3f,$3f,$3f,$0f,$0f,$0f,$0f
	.byte $0f,$0f,$0f,$0f,$7f,$7f,$7f,$7f
	.byte $7d,$7f,$7d,$7d,$7b,$7d,$7b,$7b
	.byte $79,$7b,$79,$79,$77,$79,$77,$77
	.byte $75,$77,$75,$75,$73,$75,$73,$73

topconpattern
	.byte ~00000001,~11111110,~11111111,~01111111,~10000000
	.byte ~00000001,~11111110,~11111111,~01111111,~10000000
	.byte ~00000001,~10000000,~11000011,~01100001,~10000000
	.byte ~00000001,~10000000,~11000011,~01100001,~10000000
	.byte ~00000001,~10000000,~11000011,~01100001,~10000000
	.byte ~00000001,~10000000,~11000011,~01100001,~10000000
	.byte ~00000001,~11111110,~11111111,~01100001,~10000000
	.byte ~00000001,~11111110,~11111111,~01100001,~10000000

	.byte $43,$45,$43,$43,$45,$47,$45,$45
	.byte $47,$49,$47,$47,$49,$4b,$49,$49
	.byte $4b,$4d,$4b,$4b,$4d,$4f,$4d,$4d
	.byte $4f,$4f,$4f,$4f,$0f,$0f,$0f,$0f
	.byte $0f,$0f,$0f,$0f,$af,$af,$af,$af
	.byte $ad,$af,$ad,$ad,$ab,$ad,$ab,$ab
	.byte $a9,$ab,$a9,$a9,$a7,$a9,$a7,$a7
	.byte $a5,$a7,$a5,$a5,$a3,$a5,$a3,$a3

;==========================================================
; CMC replayer entry points

sound_reset
	ldx #8
	lda #0
sound_reset1
	sta $d200,x
	dex
	bpl sound_reset1
	lda #3
	sta $d20f
	rts

sound_start
	lda #$70	;Set tune address
	ldx #<tune	;Tune address low byte
	ldy #>tune	;Tune address high byte
	jsr sound_init
	ldx #0		;Default tune #0
sound_stop
	lda #0		;Set default tune
	jsr sound_init
	rts

;==========================================================
; Wait <A> frames

wait	clc
	adc cnt
wait1	cmp cnt
	bne wait1
	rts

;==========================================================
; Main NMI routine

nmi	pha
	txa
	pha
	tya
	pha

	bit $d40f
	bpl vbi

	lda dlicnt
	beq top_dli
	cmp #1
	beq text_dli
	cmp #2
	bne dli1
	lda scrollfooterchr
	sta $d409
	jmp text_dli
dli1	cmp #3
	bne *+5
	jmp rainbow_dli
dlix	inc dlicnt

nmix	pla
	tay
	pla
	tax
	pla
	rti

;==========================================================
; Main VBI routine

vbi	set $d402,dl
	lda #$22
	sta $d400
	lda #$c0
	sta $d40e
	lda #>chr
	sta $d409
	lda #0
	sta dlicnt

	jsr sound_replay
	jsr bounce_texts
	jsr scroll_texts
	jsr animate_rainbow
	jsr check_exit
	inc cnt
	jmp nmix

;==========================================================
; Top effect DLI

top_dli
	ldx #0
top_dli1
	sta $d40a
	lda topmix1,x
	sta $d016
	lda topmix2,x
	sta $d017
	lda topmix3,x
	sta $d018
	inx
	cpx #toplines
	bne top_dli1
	jmp dlix

;==========================================================
; Text scroll lines DLI

textdlicol	.byte 4,6,8,10,12,14,14,14

text_dli
	lda #0
	sta $d018
	lda #$23	;Wide playfield
	sta $d400
	ldx #0
text_dli1
	sta $d40a
	lda textdlicol,x
	sta $d017
	inx
	cpx #8
	bne text_dli1
	jmp dlix

;==========================================================
; Rainbow effect DLI

rainbow_dli
	lda #$22	;Normal playfield
	sta $d400
	ldx #0
	ldy rainbowcolup
rainbow_dli1
	lda rainbowctab1,y
	sta $d40a
	sta $d016
	lda rainbowctab2,y
	sta $d017
	lda rainbowctab3,y
	sta $d018
	iny
	inx
rainbow_dli2
	cpx #1	;up to 102
	bcc rainbow_dli1
	lda #0
	sta $d40a
	sta $d01a
	sta $d400
	jmp dlix

;==========================================================
; Top effect main routine

topcount1	.byte 15
topcountflag2	.byte 0
topcount2	.byte 16

top_effect
	lda topcount1
	and #31
	cmp #16
	bcc *+4
	eor #31
	sta topblend1
	inc topcount1

	lda topcount2		;Initially 63
	and #31
	cmp #15
	bcc *+4
	eor #31
	sta topblend2		;Initially 15
	lda topcountflag2	;Initially 0
	beq top_effect1
	inc topcount2
top_effect1
	jsr top_blend

	lda topcount1
	and #31
	bne top_effect2
	lda #1			;Start "CON" fading
	sta topcountflag2

top_effect2
	lda topcount1
	and #15
	bne top_effect3

	lda #100
	jsr wait
top_effect3
	rts

;==========================================================
; Blend first and second overlay picture based on
; the values in topblend1 (0..15 and topblend2 (0..15)

topblend1	.byte 0
topblend2	.byte 0
topcurlum1	.byte 0
topcurlum2	.byte 0

top_blend
	ldx #toplines-1
top_blend1
	lda toplum1,x
	sec
	sbc topblend1
	bcs *+4
	lda #0
	sta topcurlum1
	beq *+5
	ora topcol1,x

top_blendcol1
	sta topmix1,x
	
	lda toplum2,x
	sec
	sbc topblend2
	bcs *+4
	lda #0
	sta topcurlum2
	beq *+5
	ora topcol2,x

top_blendcol2
	sta topmix2,x

	lda topcurlum1
	cmp topcurlum2
	bcc top_blendcol3
	cmp #0
	beq *+5
	ora topcol1,x
	jmp top_blendcol4
top_blendcol3
	lda topcurlum2
	cmp #0
	beq *+5
	ora topcol2,x
top_blendcol4
	sta topmix3,x
 
	dex
	bpl top_blend1
	rts

;==========================================================
bouncesinmax	= 28
bouncecnt	.byte 0
bouncey1	.byte 0
bouncey2	.byte 0

bounce_texts
	lda cnt
	and #1
	beq bounce_text0
	inc bouncecnt
bounce_text0
	lda bouncecnt
	cmp #bouncesinmax
	bne bounce_text1
	lda #0
	sta bouncecnt
bounce_text1
	tax
	lda bouncesintab,x
	sta bouncey1
	
	txa
	clc
	adc #3
	cmp #bouncesinmax
	bcc *+4
	sbc #bouncesinmax
	tax
	lda bouncesintab,x
	sta bouncey2

	ldx bouncey1
	lda bouncetab1,x
	sta dlbounce1
	lda bouncetab2,x
	sta dlbounce1+1
	
	ldx bouncey2
	lda bouncetab1,x
	sta dlbounce2
	lda bouncetab2,x
	sta dlbounce2+1
	
	lda #14
	sec
	sbc bouncey1
	tax
	lda bouncetab1,x
	sta dlbounce3
	lda bouncetab2,x
	sta dlbounce3+1
	
	lda #14
	sec
	sbc bouncey2
	tax
	lda bouncetab1,x
	sta dlbounce3+2
	lda bouncetab2,x
	sta dlbounce3+3
	rts

	; 15 steps
bouncetab1
	.byte $00,$10,$20,$30,$40,$50,$60,$70,$10,$20,$30,$40,$50,$60,$70
bouncetab2
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$70,$70,$70,$70,$70,$70,$70

bouncesintab
	.byte 0,1,2,3,4,5,6,7,8,9,10,11,12,13
	.byte 14,13,12,11,10,9,8,7,6,5,4,3,2,1

;==========================================================

scrollheaderdelay	.byte 10
scrollheaderchar	.byte 0
scrollheaderline	.byte $ff

scrollfooterflag	.byte 0
scrollfootersoft	.byte 3
scrollfooterchr		.byte >chr

scroll_texts
	jsr scroll_header
	jsr scroll_footer
	rts

scroll_header
	lda scrollheaderchar
	bne scroll_texts_by_char
	dec scrollheaderdelay
	bne scroll_header_delay
	lda #48
	sta scrollheaderchar
	inc scrollheaderline
	lda scrollheaderline
	cmp #5
	bne scroll_header_delay
	lda #0
	sta scrollheaderline
	set textlms1, textline1
	lda #1
	sta scrollfooterflag
scroll_header_delay
	rts

scroll_texts_by_char
	add textlms1,1
	dec scrollheaderchar
	bne scroll_texts_by_char1
	lda #250
	sta scrollheaderdelay
scroll_texts_by_char1
	rts

scroll_footer
	lda scrollfooterflag
	bne scroll_footer1
	rts
scroll_footer1
	ldx #>chr
	lda cnt
	and #1
	bne *+4
	ldx #>chr2
	stx scrollfooterchr
	cmp #0
	bne scroll_footer2
	dec scrollfootersoft
	bpl scroll_footer2
	lda #3
	sta scrollfootersoft
	add textlms2,1
	lda textlms2
	cmp #<textline2end
	bne scroll_footer2
	lda textlms2+1
	cmp #>textline2end
	bne scroll_footer2
	set textlms2,textline2
scroll_footer2
	lda scrollfootersoft
	sta $d404
	rts

;==========================================================
; Animate the rainbow bitmap and colors
rainbowpicup	.byte 0
rainbowpicadr	.byte >rainbowpic1, >rainbowpic2, >rainbowpic3, >rainbowpic4
		.byte >rainbowpic5, >rainbowpic6, >rainbowpic7, >rainbowpic8
rainbowcolup	.byte 0

animate_rainbow
;	Animate rainbow bitmap
	ldx rainbowpicup
	lda rainbowpicadr,x
	sta rainbowlms+1
	inx
	cpx #8
	bne *+4
	ldx #0
	stx rainbowpicup

;	Animate rainbow color
	lda cnt
	and #1
	bne animate_rainbow1
	inc rainbowcolup
animate_rainbow1
	rts

;==========================================================
; Check for any trigger or key pressed

check_exit
	lda $d010
	and $d011
	beq check_exit1
	lda $d01f
	and #7
	cmp #7
	bne check_exit1
	lda $d20f
	and #12
	cmp #12
	bne check_exit1
	rts

check_exit1
	sei
	lda #0
	sta $d20e
	sta $d40e
	sta $d400
	jsr sound_stop
	jsr sound_reset
	lda #>rainbowpic1
	sta p1+1
	lda #0
	tay
check_exit2
	sta (p1),y
	iny
	bne check_exit2
	inc p1+1
	ldx p1+1
	cpx #$d0
	bne check_exit2
	lda #$ff
	sta $d301
	jmp $e474

;==========================================================

dl	.byte $70,$70+$80
	.byte $48
	.word topsm
	.byte $08,$08,$08,$08,$08,$08,$08
	.byte $10
dlbounce1
	.byte $00,$00
	.byte $80,$42
textlms1
	.word textline1
dlbounce2
	.byte $00,$00
	.byte $80,$52
textlms2
	.word textline2
dlbounce3
	.byte $70,$70,$70,$70
	.byte $01
dljump	.word rainbowdl

*	= [[*+255]/256]*256
rainbowdl
	.ds 101
	.byte $80
	.byte $4e
rainbowlms
	.word $0000
rainbowdlc
	.ds 101 		;Must be $0e
	.byte $41
	.word dl

textline1
	.ds 48
	.byte "    HomeCon 5 on 2010-01-23 in Hanau/Germany    "
 	.byte "       Taubengasse 3 in 63457 Grossauheim       "
	.byte "    Games and fun with 8/16 bit home systems    "
	.byte "     Find out more on http://www.homecon.net    "
textline1end

textline2
	.ds 48
 	.byte "Welcome to the HomeCon 5 invitation coded by JAC/WUDSN for the official Atari New Year's Disk 2010. "
 	.byte "Music arranged by 505/Checkpoint. "
 	.byte "The nice 'Rainbow on Mars' below was ported from the great Amiga demo Joyride by Phenomena back in 1992. "
 	.byte "And just 17 years later I managed to release it - that's what I call retro spirit ;-) "
	.byte "Code, sound and effects packed into 16k using WUDSN IDE - Visit https://www.wudsn.com for details. "
 	.byte "Greetings and thanks go out to: PPS - the New Year's Disk is a great idea. "
 	.byte "505 - waiting for your Falcon release and of course new tunes in 2010. "
 	.byte "FOX - for the inflate routine, it is excellent. "
 	.byte "MICDUNE - for pointing me to Joyride. "
 	.byte "Tigerduck - for beating me in Rally Speedway all the time. "
 	.byte "Hessi and Quarkbeutel - for organizing the HomeCon. "
 	.byte "My family - for sparing me the time to do these crazy things. "
 	.byte "And of course to all the other HomeCon, ABBUC, Atari and Pouet people all over the globe. "
 	.byte "Let's make 2010 a year of fun, games and new releases."
textline2end
 	.ds 48
end
;===============================================

*	= sound
	.incbin "HomeCon5-CmcReplay$5000.snd"

*	= chr+$100
	.incbin "HomeCon5-AscII.chr"

*	= tune
	.incbin "HomeCon5-Murks$8400.cmc"

*	= $2e0
	.word start
