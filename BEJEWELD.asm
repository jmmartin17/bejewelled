#include ti83plus.inc
#include iondef.inc
.org $9d95
    ret
    jr nc,start_program ;ion
.db "Bejewelled 1.0",0
#define numgems 7
cursorx equ    saferam1
cursory equ    saferam1+1
score   equ    saferam1+2
array   equ    saferam1+4  ;will be indexed through ix, 64 bytes (8x8 grid of gems) add 8 to get to next row
start_program:
    res doneprgm,(iy+doneflags)
    set textwrite,(iy+sgrflags)
    bcall(_grbufclr)
    res     indicrun,(iy+indicflags)
    ld      hl,splash
    ld      de,plotsscreen+192
    ld      bc,384
    ldir
    call ionfastcopy
enter_loop:
    ei          ;getkey system routines dont work in di =/
    bcall(_getcsc)
    cp      skenter
    jr      z,enter_press
    jr      enter_loop
enter_press:
    bcall(_grbufclr)
    ld hl,side_splash
    ld de,plotsscreen+48
    ld bc,60*3
ss_loop:
    ldi
    ldi
    ldi
    jp po,ss_done
    push hl
    ex de,hl
    ld de,9
    add hl,de
    ex de,hl
    pop hl
    jr ss_loop
ss_done:
    ld hl,plotsscreen+11
    ld de,12
    ld a,127
    ld b,64
    ld (hl),a
    add hl,de
    djnz $-2
    ld hl,0
    ld      (cursorx),hl
    ld (score),hl
    ld      hl,array
    ld      b,64
setup_loop:
    push        bc
    ld      b,numgems     ;6 different 
    call        ionrandom
    inc a
    ld      (hl),a
    inc     hl
    pop     bc
    djnz        setup_loop
    call draw_gems
init_loop:
    res 1,(iy+asm_flag1)
    call check_for_trips
    call check_for_blanks
    call draw_gems
    call ionfastcopy
    ld b,50
    call haltb
    bit 1,(iy+asm_flag1)
    jr nz,init_loop
    call draw_cursor
game_loop:
    ei
    in a,(4)
    and 8
    jr nz,skip_g
    ld a,$fe
    out (1),a
    ld a,(de)
    in a,(1)
    cp $fe
    call z,gemdown
    cp $f7
    call z,gemup
    cp $fd
    call z,gemleft
    cp $fb
    call z,gemright
    ld hl,(score)
    jr game_loop
skip_g:
    bcall(_getcsc)
    cp skclear
    res textwrite,(iy+sgrflags)
    ret z
    set textwrite,(iy+sgrflags)
    cp skdown
    call z,curdown
    cp skup
    call z,curup
    cp skleft
    call z,curleft
    cp skright
    call z,curright
    call ionfastcopy
    jr game_loop
curup:
    call draw_cursor
    ld a,(cursory)
    dec a
    ld (cursory),a
    jp p,draw_cursor
    ld a,7
    ld (cursory),a
    jp draw_cursor
curdown:
    call draw_cursor
    ld a,(cursory)
    inc a
    cp 8
    ld (cursory),a
    jp c,draw_cursor
    xor a
    ld (cursory),a
    jp draw_cursor
curleft:
    call draw_cursor
    ld a,(cursorx)
    dec a
    ld (cursorx),a
    jp p,draw_cursor
    ld a,7
    ld (cursorx),a
    jp draw_cursor
curright:
    call draw_cursor
    ld a,(cursorx)
    inc a
    cp 8
    ld (cursorx),a
    jp c,draw_cursor
    xor a
    ld (cursorx),a
    jp draw_cursor
gemup:
    ld a,(cursory)
    or a
    ret z
    add a,a
    add a,a
    add a,a
    ld l,a
    ld h,0
    ld a,(cursorx)
    ld d,0
    ld e,a
    add hl,de
    ld de,array
    add hl,de
    ld d,h
    ld e,l
    ld bc,8
    sbc hl,bc
    jp check_sit
gemdown:
    ld a,(cursory)
    cp 7
    ret z
    add a,a
    add a,a
    add a,a
    ld l,a
    ld h,0
    ld a,(cursorx)
    ld d,0
    ld e,a
    add hl,de
    ld de,array
    add hl,de
    ld d,h
    ld e,l
    ld bc,8
    add hl,bc
    jp check_sit
gemleft:
    ld a,(cursory)
    add a,a
    add a,a
    add a,a
    ld l,a
    ld h,0
    ld a,(cursorx)
    or a
    ret z
    ld d,0
    ld e,a
    add hl,de
    ld de,array
    add hl,de
    ld d,h
    ld e,l
    dec hl
    jp check_sit
gemright:
    ld a,(cursory)
    add a,a
    add a,a
    add a,a
    ld l,a
    ld h,0
    ld a,(cursorx)
    cp 7
    ret z
    ld d,0
    ld e,a
    add hl,de
    ld de,array
    add hl,de
    ld d,h
    ld e,l
    inc hl
check_sit:
    ld b,(hl)
    ld a,(de)
    ex de,hl
    ld (hl),b
    ld (de),a
    push hl
    push de
    res 0,(iy+asm_flag1)
    call check_for_trips
    pop de
    pop hl
    bit 0,(iy+asm_flag1)
    jr nz,flip_success
    ld b,(hl)
    ld a,(de)
    ex de,hl
    ld (hl),b
    ld (de),a
    xor a
    ret
flip_success:
    res 1,(iy+asm_flag1)
    call check_for_trips
    call check_for_blanks
    call draw_gems
    ld b,20
    call haltb
    bit 1,(iy+asm_flag1)
    jr nz,flip_success
    call draw_cursor
    xor a
    ret
check_for_trips:
    ld hl,array
    ld b,8
rtrips_loop_row:
    ld c,6
rtrips_loop_col:
    ld a,(hl)
    push hl
    push bc
    call check_a
    pop bc
    pop hl
    inc hl
    dec c
    jr nz,rtrips_loop_col
    inc hl
    inc hl
    djnz rtrips_loop_row
    ld hl,array
    ld b,6
ctrips_loop_row:
    ld c,8
ctrips_loop_col:
    ld a,(hl)
    push hl
    push bc
    call check_b
    pop bc
    pop hl
    inc hl
    dec c
    jr nz,ctrips_loop_col
    djnz ctrips_loop_row
    ret
check_a:
    ld bc,3
    cpi
    ret nz
    jp pe,$-3
    ld de,0
    cp (hl)
    jr nz,cad_3
    inc de
    inc de
    ld (hl),0
    inc hl
    cp (hl)
    dec hl
    jr nz,cad_3
    inc de
    inc de
    inc de
    inc hl
    ld (hl),0
    dec hl
cad_3:
    xor a
    dec hl
    ld (hl),a
    dec hl
    ld (hl),a
    dec hl
    ld (hl),a
    inc de
    inc de
    inc de
    push hl
    push bc
    ld hl,0
    ld (pencol),hl
    ld hl,(score)
    add hl,de
    ld (score),hl
    ld b,5
    call disp_hl
    pop bc
    pop hl
    set 0,(iy+asm_flag1)
    set 1,(iy+asm_flag1)
    ret
disp_hl:
    ld de,(pencol)
    ld a,b
    add a,a
    add a,a
dhlsl:
    inc e
    dec a
    jr nz,dhlsl
    ld (pencol),de
dhll:
    ld de,(pencol)
    push de
    push bc
    bcall(_divhlby10)
    add a,'0'
    bcall(_vputmap)
    pop bc
    pop de
    dec e
    dec e
    dec e
    dec e
    ld (pencol),de
    djnz dhll
    ret
check_b:
    ld ix,0
    ld bc,3
    ld de,7
cbl:
    inc (hl)
    dec (hl)
    jr z,eq_0
    cpi
    ret nz
    add hl,de
    jp pe,cbl
cbd:
    cpi
    dec hl
    jr nz,cpd_3
    inc ix
    inc ix
    ld (hl),0
    add hl,de
    cpi
    dec hl
    push af
    or a
    sbc hl,de
    pop af
    jr nz,cpd_3
    inc ix
    inc ix
    inc ix
    ld (hl),0
cpd_3:
    xor a
    inc de
    sbc hl,de
    ld (hl),a
    sbc hl,de
    ld (hl),a
    sbc hl,de
    ld (hl),a
    push ix
    pop de
    inc de
    inc de
    inc de
    ld hl,(score)
    add hl,de
    ld (score),hl
    ld b,5
    call disp_hl
    set 0,(iy+asm_flag1)
    set 1,(iy+asm_flag1)
    ret
eq_0:
    inc hl
    add hl,de
    dec c
    jr nz,cbl
    jr cbd
    
check_for_blanks:
    call draw_gems
    ld b,8
    ld hl,array
    ld de,0
cfblr:
    ld c,8
cfblc:
    ld a,(hl)
    push bc
    push hl
    push de
    or a
    jr nz,$+16
    call drop_gems
    call draw_gems
    call ionfastcopy
    ld b,20
    call haltb
    pop de
    pop hl
    pop bc
    inc hl
    inc d
    dec c
    jr nz,cfblc
    ld d,0
    inc e
    djnz cfblr
    call ionfastcopy
    ret
drop_gems:  ;d=column,e=row. fills in blank square by dropping gems
    ld a,e
    add a,a
    add a,a
    add a,a
    ld l,a
    ld h,0
    ld c,e
    ld e,d
    ld d,0
    add hl,de
    ld de,array
    add hl,de
    ld d,h
    ld e,l
    ld a,c
    or a
    jr z,rand_gem
    push bc
    ld bc,8
    sbc hl,bc
    pop bc
    ld b,0
drop_loop:
    ldd
    dec de \ dec de \ dec de
    dec de \ dec de \ dec de
    dec de
    dec hl \ dec hl \ dec hl
    dec hl \ dec hl \ dec hl
    dec hl
    jp pe,drop_loop
rand_gem:
    ld b,numgems
    call ionrandom
    inc a
    ld (de),a
    ret
draw_gems:
    ld hl,plotsscreen+3
    ld de,4
    ld b,64
clrl1:
    ld c,8
clrl2:
    ld (hl),0
    inc hl
    dec c
    jr nz,clrl2
    add hl,de
    djnz clrl1
    ld ix,array
    ld a,24
    ld e,0
    ld b,64
drawloop:
    push bc
    ld      l,(ix+0)
    ld      h,0
    add     hl,hl
    add     hl,hl
    add     hl,hl
    ld      bc,gems
    add     hl,bc
    push hl
    ex (sp),ix
    push        af
    push        de
    ld l,e
    ld b,8
    call        ionputsprite
    pop     de
    pop     af
    pop ix
    cp      80
    call        z,next_row
    add     a,8
    inc     ix
    pop     bc
    djnz        drawloop
    ret
next_row:
    ld      a,8
    add     a,e
    ld      e,a
    ld      a,16
    ret
draw_cursor:
    ld a,(cursory)
    ld h,0
    ld l,a
    ld d,h
    ld e,l
    add hl,de
    add hl,de
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    ld a,(cursorx)
    add a,3
    ld e,a
    add hl,de
    ld de,plotsscreen
    add hl,de
    ld b,9
    ld de,12
curloop:
    ld a,(hl)
    cpl
    xor 128
    ld (hl),a
    add hl,de
    djnz curloop
    ret
haltb:
    ei
    halt
    djnz $-1
    ret
gems:
.dw 0,0,0,0
.db %00000000
.db %01111111
.db %01100111
.db %01001111
.db %01011111
.db %01111111
.db %01111111
.db %01111111

.db %00000000
.db %00001000
.db %00010100
.db %00100110
.db %01001111
.db %00111110
.db %00011100
.db %00001000

.db %00000000
.db %00111110
.db %01100111
.db %01101111
.db %00111110
.db %00111110
.db %00011100
.db %00001000

.db %00000000
.db %00001000
.db %00011100
.db %00011100
.db %00110110
.db %00100110
.db %01001111
.db %01111111

.db %00000000
.db %01111111
.db %00110010
.db %00010100
.db %00001000
.db %00010100
.db %00100110
.db %01111111

.db %00000000
.db %00111110
.db %01011101
.db %01101011
.db %01110111
.db %01101011
.db %01011101
.db %00111110

.db %00000000
.db %00001000
.db %00010100
.db %00111110
.db %01000001
.db %00111110
.db %00010100
.db %00001000

#include splash.z80
.end
