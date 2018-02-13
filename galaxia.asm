
; Galaxia GBC v1.1
; (c) 2003-2018 Tomasz Slanina
;
; Minigame compo entry
; 1024 bytes size limit (including header and N logo)
; up/down/left/right to move
; A/B to shot
;
; 3 difficulty levels

_SCRN0                      equ $9800
_HRAM                       equ $ff80

rP1                         equ $ff00
rDIV                        equ $ff04
rIF                         equ $ff0f
rLCDC                       equ $ff40
rSTAT                       equ $ff41
rSCX                        equ $ff43
rLY                         equ $ff44
rDMA                        equ $ff46
rVBK                        equ $ff4f
rBCPS                       equ $ff68
rIE                         equ $ffff

;limits
MIN_PLAYER_Y                equ 24
MAX_PLAYER_Y                equ 144
MIN_PLAYER_X                equ 16
MAX_PLAYER_X                equ 96

;player starting position
START_Y                     equ 84 

;inactivity flag
SPRITE_INACTIVE             equ -16

;collision rectangles
PLAYER_Y                    equ 8
PLAYER_Y2                   equ 16
PLAYER_X                    equ 16
PLAYER_X2                   equ 16+8

BULLET_Y                    equ 8
BULLET_Y2                   equ 16
BULLET_X                    equ 8
BULLET_X2                   equ 16

;tiles and colors
TILE_BULLET                 equ 4
TILE_ENEMY                  equ 5

COLOR_ENEMY                 equ 1
SHIP_COLOR                  equ 0

;speed of enemies
START_SPEED                 equ 2
HARD_SPEED                  equ 4

;difficulty increase after MAX_HIT hits
MAX_HIT                     equ 5

;varaiables in HiRAM (for faster access)
scrollPosition              equ $ffda
oldKeys                     equ $ffdb
enemySpeed                  equ $ffdc
hitCounter                  equ $ffdd
increaseDifficulty          equ $ffde
autoMove                    equ $ffdf


SECTION "start",ROM0[0]

restartzero:   ;label must be set before any local labels    
       ld [hl],a ; 2nd part of keyread
       ld a,[hl]
       ld a,[hl]
       and $0f
       swap a
       ld b,a
       ld a,16
       ld [hl],a
       ld a,[hl] ;4 times (should be 6 times)
       ld a,[hl]
       ld a,[hl]
       ld a,[hl]
       and $0f
       or b
       cpl
       ld b,a
       bit 6,a  ;Up
       jr z,.down
       ld a,[de] ;y
       cp MIN_PLAYER_Y
       ret c
       sub 4   ;4 = offset
       jr .setY
.down:
       bit 7,a ;down
       ret z
       ld a,[de];y
       cp MAX_PLAYER_Y
       ret nc
       add 4
.setY:
       ld [de],a
       ld [shadowOAM+4],a
       ret

spritePalette: ; unused space

db %00000001,%01111101
db %00000000,%00000000
db %00000000,%01000100
db %00010000,%00000010
db %00000000,%00000000
db %00011111,%00001101
db %11100000,%00000011     

SECTION "vblank",ROM0[$40]  ;VBL interrupt vector
       push af
       ld a,[scrollPosition] ;scroll offset
       inc a
       jp _HRAM ; jump to HRAM


SECTION "hblank",ROM0[$48]     ;LCDC interrupt vector (hblank mode)
hblank:
       push af
       push bc
       ld a,[scrollPosition] ;pseudo paralax scrolling
       ld b,a
       ld a,[rLY] ;current scanline
       cp 1
       jr z,.set
       cp 121
       jr z,.set
       srl b
       cp 17
       jr z,.set
       cp 105
       jr z,.set
       srl b
       cp 33
       jr z,.set
       cp 89
       jr z,.set
       srl b
       cp 49
       jr nz,.exit

.set:
       ld a,b
       ld [rSCX],a ; BG scroll X offset

.exit:
       pop bc
       pop af
       reti

; collisions ship - obstacles

collisions:
       ld hl,$c000 ;ship coords
       ld a,[hl+]
       ld b,a   ;y
       ld c,[hl] ;x
       ld d,SPRITE_INACTIVE
       ld l,2*4+4*4 ;skip bullets

.collLoop:
       ld a,[hl]
       cp d ;SPRITE_INACTIVE
       jr z,.notused
       cp 255
       jp z,collisionscd  ;bullets collisions
       push hl
       inc hl
       sub PLAYER_Y  ;calculations of collision rectangles
       cp b
       jr nc,.skip
       add PLAYER_Y2
       cp b
       jr c,.skip
       ld a,[hl]
       sub PLAYER_X
       cp c
       jr nc,.skip
       add PLAYER_X2
       cp c
       jr c,.skip

.wait:               ;game over
       dec hl
       ld a,h
       or l
       jr nz,.wait
       jp $100  ; restart

.skip:
       pop hl
.notused:
       ld a,4 ; next sprite
       add l
       ld l,a
       jr .collLoop

;bullets collisions

bulletCollision:
       ld a,[hl]
       cp d      ; SPRITE_INACTIVE
       ret z
       push hl
       inc hl
       ld b,a     ;y
       ld c,[hl]  ;x
       ld l,2*4+4*4 ; enemies
.bulletLoop:
       ld a,[hl]
       cp d
       jr z,.notused
       cp 255
       jr z,.exitCheck
       push hl
       inc hl
       sub BULLET_Y
       cp b
       jr nc,.skip
       add BULLET_Y2
       cp b
       jr c,.skip
       ld a,[hl]
       sub BULLET_X
       cp c
       jr nc,.skip
       add BULLET_X2
       cp c
       jr c,.skip
       pop hl
       inc hl ;x
       inc hl ;tile
       ld a,[hl-]
       cp TILE_ENEMY
       jr nz,.notEnemy
       dec hl
       ld [hl],d ;remove enemy

       ld a,[hitCounter]
       inc a
       cp MAX_HIT
       jr c,.spo
       ld a,$ff
       ld [increaseDifficulty],a
       cpl
.spo:
       ld [hitCounter],a
.notEnemy:
       pop hl
       ld [hl],d ;remove bullet
       ret

.exitCheck:
       pop hl
       ret
.skip:
       pop hl

.notused:
       ld a,4
       add l
       ld l,a
       jr .bulletLoop

SECTION "boot",ROM0[$100]
       nop
       jp main

SECTION "header",ROM0[$134]

       db "Galaxia        ",$c0
       db 0,0,0,0,0,0,0,0,0,0,0,0
main:
       di
       xor a
       ld [rLCDC],a ;LCD off
       ld sp,$cfff
       ld [rVBK],a  ;vram bank 0
       ld h,$80
       ld l,a
       ld de,tiles

.loopFill:
       cpl
       ld [hl+],a
       bit 4,l ; 16 bytes fill
       jr z,.loopFill

.copyTileLoop:
       ld a,[de]
       inc de
       ld [hl+],a
       bit 1,h
       jr z,.copyTileLoop

       ld hl,_SCRN0 ; tilemap
       push hl
       ld e,32
       ld b,4

.bigLoop:
       ld c,e
       xor a

.fillSolid:
       ld [hl+],a ; solid color
       dec c
       jr nz,.fillSolid

       ld c,e
       ld a,1

.fill3:
       ld [hl+],a
       dec c
       jr nz,.fill3

       dec b
       jr nz,.bigLoop
       ld c,32*2 ;middle of the screen
       xor a

.fill4:
       ld [hl+],a
       dec c
       jr nz,.fill4

       ld b,4 ; bottom part of bg screen

.bLoop:
       ld c,e
       ld a,1
.l3x:
       ld [hl+],a
       dec c
       jr nz,.l3x

       ld c,e
       xor a

.fillSolid2:
       ld [hl+],a ; solid color
       dec c
       jr nz,.fillSolid2
       dec b
       jr nz,.bLoop
       pop hl
       ld a,1
       ld [rVBK],a ; VRAM bank 1
       ld d,b   ;0
       ld b,4

.bigLoop2:
       ld c,e
       ld a,d
.lx1:
       ld [hl+],a ;solid
       dec c
       jr nz,.lx1
       inc d
       ld c,16 ;only 16 !
.lx2:
       ld a,d
       ld [hl+],a
       set 5,a    ;flip h on
       ld [hl+],a
       dec c
       jr nz,.lx2
       dec b
       jr nz,.bigLoop2

       ld c,32*2
       ld a,4
.fillD2:
       ld [hl+],a
       dec c
       jr nz,.fillD2
       ld b,4

.bigLoop3:
       ld c,16
       ld a,d
       set 6,a  ; flip v always on
.lx2x:
       ld [hl+],a
       set 5,a    ;flip h on
       ld [hl+],a
       res 5,a    ;flip h off
       dec c
       jr nz,.lx2x
       dec d
       ld c,e
       ld a,d
.fillD1:
       ld [hl+],a ; solid line
       dec c
       jr nz,.fillD1

       dec b
       jr nz,.bigLoop3

       ld hl,rBCPS ; BG palette
       ld a,128
       ld [hl+],a
       ld c,8
       ld d,h
       xor a

.pall:
       ld [hl],a   ;palette generator
       ld [hl],d
       add 5
       ld [hl],a
       ld [hl],d
       ld [hl],d
       ld [hl],d
       ld [hl],d
       ld [hl],d
       dec c
       jr nz,.pall
       inc hl  ;rBCPD
       ld a,128+2
       ld [hl+],a
       ld de,spritePalette
       ld c,64

.copy:
       ld a,[de]
       ld [hl],a
       inc de
       dec c
       jr nz,.copy
       ld a,8   ;LCDC interrupt = hblank
       ld [rSTAT],a   ;LCDC set

       ld hl,hiramCode ;OAM DMA code MUST be executed from HiRAM
       ld de,_HRAM

.copy1:
       ld a,[hl+]
       ld [de],a
       inc de
       bit 5,e
       jr z,.copy1

       ld hl,shadowOAM ; shadow OAM clear
       ld a,SPRITE_INACTIVE

.clear:
       ld [hl+],a
       bit 0,h
       jr z,.clear
       
       xor a    ; varaibles set
       ld [oldKeys],a ;tutaj
       ld [hitCounter],a
       ld [increaseDifficulty],a
       ld [autoMove],a
       ld l,a
       cpl
       ld [shadowEnd],a ; set end marker for sprites 
       
       ld a,START_SPEED
       ld [enemySpeed],a

       ld a,START_Y ;ship sprites
       ld h,$c0
       ld [hl+],a
       ld a,10
       ld [hl+],a
       ld a,2     ;tile
       ld [hl+],a
       ld a,SHIP_COLOR
       ld [hl+],a
       ld a,START_Y
       ld [hl+],a
       ld a,10+8
       ld [hl+],a
       ld a,2+1     ;tile
       ld [hl+],a
       ld a,SHIP_COLOR
       ld [hl],a

       ld a,128+16+2 ;on , chr 8000 , bg 9800 , 8x16 , obj on
       ld [rLCDC],a

       xor a
       ld [rIF],a  ;clear interrupts flag
       ld a,3
       ld [rIE],a  ;set interrupts = VBL+LCDC
       ei

mainloop:
       ld hl,scrollPosition
       ld a,[hl]

.waitframe:
       cp [hl]
       jr z,.waitframe ;wait for next frame

       ld a,[increaseDifficulty] ;difficulty increase
       or a
       jr z,.noinc
       ld a,[enemySpeed] ; speed of enemies
       cp START_SPEED
       jr nz,.extraDifficulty
       ld a,HARD_SPEED
       ld [enemySpeed],a
       jr .noinc

.extraDifficulty:
       ld [autoMove],a

.noinc:
       xor a
       ld [increaseDifficulty],a
       bit 1,[hl]

       ld a,32     ;keypad read
       ld hl,rP1
       ld de,shadowOAM
       
       rst 0  ; keypad read and test up/down

       bit 5,b
       jr z,.left
       ld a,[shadowOAM+1];x
       cp MIN_PLAYER_X
       jr c,.skipmove
       sub 4
       ld [shadowOAM+1],a
       add 8
       ld [shadowOAM+5],a
       jr .skipmove

.left:
       bit 4,b
       jr z,.skipmove
       ld  a,[shadowOAM+1];x
       cp  MAX_PLAYER_X
       jr nc,.skipmove
       add 4
       ld [shadowOAM+1],a
       add 8
       ld [shadowOAM+5],a
.skipmove:
       ld a,[oldKeys] ; old buttons status
       xor b
       and b
       ld c,a
       ld a,b
       ld [oldKeys],a
       ld a,c
       and 3
       jr z,.noshot

;shot

       ld hl,shadowOAM+4*2
       ld c,4
.loz:
       ld a,[hl]
       cp SPRITE_INACTIVE
       jr nz,.nextbullet

       ld a,[shadowOAM] ;free slot was found
       ld [hl+],a
       ld a,[shadowOAM+1]
       add 9
       ld [hl+],a
       ld a,TILE_BULLET
       ld [hl+],a
       ld a,1
       ld [hl],a
       jr .noshot

.nextbullet:
       ld a,4
       add a,l
       ld l,a
       dec c
       jr nz,.loz

.noshot:

;new enemies

       ld a,[scrollPosition] ; pseudo "timer"
       and %11100
       jr nz,.endenemyadd

       ld hl,shadowOAM+4*2+4*4 ; skiping player/bullets
.l1:
       ld a,[hl]
       cp SPRITE_INACTIVE ; is active ?
       jr z,.insert
       cp 255   ; end of sprites
       jr z,.endenemyadd
       inc hl
       inc hl
       inc hl
       inc hl
       jr .l1

.insert:  ;free slot
       ld a,[rDIV] ; "random" number 
       ld b,a
       ld a,[scrollPosition]
       cpl
       rla
       rla
       xor b
       swap a
       and 127 ;limit
       add 20 ; move a bit down

       ld [hl+],a  ; y
       ld a,170 ; x
       ld [hl+],a

       ld a,[rDIV]
       bit 0,a      ;red or green ?
       ld a,TILE_ENEMY
       ld b,COLOR_ENEMY
       jr z,.storeData
       inc a ;second enemy tile

.storeData:
       ld [hl+],a
       ld [hl],b

.endenemyadd:

;sprite autorun

       ld hl,shadowOAM+4*2 ;starting form "bullets"
       ld c,SPRITE_INACTIVE
.loopauto:

       ld a,[enemySpeed]
       ld e,a
       ld a,[hl+] ;x
       cp c  ;is active?
       jr z,.skipme
       cp 255 ; end marker
       jr z,.end
       inc hl ;tile
       ld a,[hl]
       cp TILE_BULLET
       jr z,.bulletAuto

       cp TILE_ENEMY
       jr z,.enemyAuto

       ld a,[autoMove]
       srl e
       or a
       jr z,.enemyAuto  ;skip in easy mode

       dec hl
       ld a,[hl];x
       and 15
       sub 7
       ld b,a
       dec hl
       ld a,[hl]
       add b
       ld [hl+],a
       ld a,[hl]
       sub START_SPEED/2
       jr .cont

.enemyAuto:

       dec hl ;x
       ld a,[hl]
       sub e
.cont:
       ld [hl],a ;x
       cp 8
       jr nc,.skipme
       dec hl   ;y
       ld a,c
       ld [hl+],a

.skipme:
       inc hl ;tile
       inc hl ;atrib
       inc hl ;next
       jr .loopauto

.bulletAuto:
       dec hl
       ld a,[hl]
       add 5
       ld [hl],a
       cp 160 ;check if went out of screen 
       jr c,.skipme
       dec hl
       ld a,c
       ld [hl+],a
       jr .skipme
.end:
       call collisions
       jp mainloop

collisionscd:
       ld l,8
       call bulletCollision
       ld l,12
       call bulletCollision
       ld l,16
       call bulletCollision
       ld l,20
       jp bulletCollision

hiramCode:
       inc a ; second part of the scroll update code
       ld [scrollPosition],a
       ld a,$c0    ; DMA Shadow OAM -> OAM
       ld [rDMA],a
.wait:
       dec a
       jr nz,.wait
       pop af
       reti
tiles:

;BG tile
db %00000000,%10000000
db %10000000,%01000000
db %11000000,%00100000
db %11100000,%00010000
db %11110000,%00001100
db %11111100,%00000011
db %11111111,%00000000
db %11111111,%00000000

;Player tiles
db %00000000,%00011110
db %00000000,%00001111
db %00000111,%00000111
db %00000000,%00011111
db %00000000,%00011111
db %00000111,%00000111
db %00000000,%00001111
db %00000000,%00011110

db %00000000,%00000000
db %00000000,%10000000
db %11111110,%11111110
db %01111111,%10000001
db %01111111,%10000001
db %11111110,%11111110
db %00000000,%10000000
db %00000000,%00000000

;Bullet
db %00000000,%00000000
db %00000000,%00000000
db %00000000,%00000000
db %00111100,%00000000
db %00111100,%00000000
db %00000000,%00000000
db %00000000,%00000000
db %00000000,%00000000

;Enemies
db %00011000,%00000000
db %00111100,%00011000
db %01111110,%00111100
db %11111111,%01111110
db %11111111,%01111110
db %01111110,%00111100
db %00111100,%00011000
db %00011000,%00000000

db %11111111,%00000000
db %10000001,%01111110
db %10000001,%01111110
db %10000001,%01111110
db %10000001,%01111110
db %10000001,%01111110
db %10000001,%01111110
db %11111111,%00000000


SECTION "RAM",WRAM0[$c000]

shadowOAM:                 ds 40*4  ;Shadow OAM
shadowEnd:                 ds 1     ;0xff = end marker of sprite table
