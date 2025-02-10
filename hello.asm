;==============================================================
; SDSC tag and SMS rom header
;==============================================================
.sdsctag 0.1,"Hello World!","SMS programming test program","Alex West"

; based on Maxim's "Hello World!" tutorial, found here:
; https://www.smspower.org/maxim/HowToProgram/Index

;.sdsctag 1.2,"Hello World!","SMS programming tutorial program - enhanced version","Maxim"

;==============================================================
; WLA-DX banking setup
;==============================================================
.memorymap
defaultslot 0
slotsize $8000
slot 0 $0000
.endme

.rombankmap
bankstotal 1
banksize $8000
banks 1
.endro

;==============================================================
; SMS defines
;==============================================================
.define VDPControl $bf
.define VDPData $be
.define VRAMWrite $4000
.define CRAMWrite $c000

; RAM variables

.enum $C000 export
    tempVRAMAddr dw


.ende



; ROM starts here
.bank 0 slot 0
.org $0000
;==============================================================
; Boot section
;==============================================================
    di              ; disable interrupts
    im 1            ; Interrupt mode 1
    jp main         ; jump to main program


;==============================================================
; Interrupt Handler
;==============================================================
.org $0038
interruptHandler: ;{ Handles VBlank and HBlank
    push af
    push bc
    push de
    push hl
    ; Distinguish between HBlank and VBlank
    in a,(VDPControl) ; Read status, clear interrupt flag
    bit 7, a
    jr z, @branch_HBlank
        ; VBlank always:
        ; Updates music
        ; Resets scroll value
        ; Preps HBlank interrupts (if applicable)
        ; Only if VBlank is ready:
            ; Transfer data lists
            ; Transfer sprite lists
            ; Update scroll value
            ; Updates VDP registers (if applicable)
            ; Sets "VBlank is over" flag
        @branch_HBlank:
        ; HBlank
            ; Does whatever
            ; Sets up next HBlank interrupt (if applicable)

@exitInterrupt            
    pop hl
    pop de
    pop bc
    pop af
    ei ; ?
    reti
;}

.org $0066
;==============================================================
; Pause button handler
;==============================================================
    ; Do nothing
    retn

;==============================================================
; Main program
;==============================================================
main:
    ld sp, $dff0

    ;==============================================================
    ; Set up VDP registers
    ;==============================================================
    ld hl,VDPInitData
    ld b,VDPInitDataEnd-VDPInitData
    ld c,VDPControl
    otir

    ;==============================================================
    ; Clear VRAM
    ;==============================================================
    ; 1. Set VRAM write address to $0000
    ld hl,$0000 | VRAMWrite
    call SetVDPAddress
    ; 2. Output 16KB of zeroes
    ld bc,$4000     ; Counter for 16KB of VRAM
-:  xor a
    out (VDPData),a ; Output to VRAM address, which is auto-incremented after each write
    dec bc
    ld a,b
    or c
    jr nz,-

    ;==============================================================
    ; Load palette
    ;==============================================================
loadPalette:
    ld hl, CRAMWrite|$0000
    call SetVDPAddress
    
    ld hl, PortraitPal
    ld bc, PortraitPalSize
    call CopyToVDP
    
    
    ;; 1. Set VRAM write address to CRAM (palette) address 0
    ;ld hl,$0000 | CRAMWrite
    ;call SetVDPAddress
    ;; 2. Output colour data
    ;ld hl,PaletteData
    ;ld bc,PaletteDataEnd-PaletteData
    ;call CopyToVDP

    ;==============================================================
    ; Load tiles (font)
    ;==============================================================
loadTiles:
    ; 1. Set VRAM write address to tile index 0
    ld hl,$0000 | VRAMWrite
    call SetVDPAddress
    ; 2. Output tile data
    ld hl,FontData              ; Location of tile data
    ld bc,FontDataSize          ; Counter for number of bytes to write
    call CopyToVDP

    ; Load portrait tiles
    ld hl, $0C00 | VRAMWrite
    call SetVDPAddress
    ld hl, PortraitChr
    ld bc, PortraitChrSize
    call CopyToVDP

    ;==============================================================
    ; Write text to name table
    ;==============================================================
writeText:
    ; 1. Set VRAM write address to tilemap index 0
    ld hl,$3800 + $40*2 + $2*2 | VRAMWrite
    call SetVDPAddress
    ; 2. Output tilemap data
    ld hl,Message
    @loop:
        ld a,(hl)
        cp $ff
        jr z,@loopExit
        out (VDPData),a
        xor a
        out (VDPData),a
        inc hl
        jr @loop
    @loopExit:

writePortrait:
    ld hl, $3800 + $40*10 + $2*17 | VRAMWrite
    call SetVDPAddress
    ld (tempVRAMAddr), hl
;    ld a, h
;    ld tempVRAMAddr+1, a
    
    ld hl, PortraitMap
    ld bc, PortraitMapSize
    ld d, 15*$2 ; (portrait tilemap width in bytes)
    
    @loop:
        ld a, (hl)
        out (VDPData), a
        inc hl
        dec d
        jr nz, @endIf
            push hl
                ld hl, (tempVRAMAddr)
                ld de, $0040 ; Add one row
                add hl, de
                ld (tempVRAMAddr), hl
                call SetVDPAddress
            pop hl
            ld d, 15*$2
        @endIf:
        dec bc
        ld a, b
        or c
        jr nz, @loop
    @loopExit:
    
clearSprites:
    ld hl, VRAMWrite | $3F00 ; Sprite attribute table
    call SetVDPAddress
    
    ld a, $D0 ; Ypos
    ld b, $40 ; number of sprites
    @loop:
        out (VDPData), a
        dec b
    jr nz, @loop


    ; Turn screen on
    ld a,%01000000
;          ||||||`- Zoomed sprites -> 16x16 pixels
;          |||||`-- Doubled sprites -> 2 tiles per sprite, 8x16
;          ||||`--- Mega Drive mode 5 enable
;          |||`---- 30 row/240 line mode
;          ||`----- 28 row/224 line mode
;          |`------ VBlank interrupts
;          `------- Enable display
    out (VDPControl),a
    ld a,$81
    out (VDPControl),a

    ; Infinite loop to stop program
-:  jr -

;==============================================================
; Helper functions
;==============================================================

SetVDPAddress:
; Sets the VDP address
; Parameters: hl = address
    push af
        ld a,l
        out (VDPControl),a
        ld a,h
        out (VDPControl),a
    pop af
    ret

CopyToVDP:
; Copies data to the VDP
; Parameters: hl = data address, bc = data length
; Affects: a, hl, bc
-:  ld a,(hl)    ; Get data byte
    out (VDPData),a
    inc hl       ; Point to next letter
    dec bc
    ld a,b
    or c
    jr nz,-
    ret

;==============================================================
; Data
;==============================================================

.asciitable
map " " to "~" = 0
.enda

Message:
.asc "Hello world!"
.db $ff

PaletteData:
.db $00,$3f ; Black, white
PaletteDataEnd:

; VDP initialisation data
VDPInitData:
.db $04,$80 ; Rendering properties 1
.db $00,$81 ; Rendering properties 2
.dw $ff,$82 ; Nametable address
.dw $ff,$85 ; Sprite attribute table address
.dw $fd,$86 ; Sprite tileset address
.dw $ff,$87 ; Overscan color
.dw $00,$88 ; X Scroll
.dw $00,$89 ; Y Scroll
.dw $ff,$8a ; Line Counter
VDPInitDataEnd:

FontData:
.incbin "font.bin" fsize FontDataSize


PortraitMap:
.incbin "portrait_map.bin" fsize PortraitMapSize
PortraitChr:
.incbin "portrait_chr.bin" fsize PortraitChrSize
PortraitPal:
.incbin "portrait_pal.bin" fsize PortraitPalSize

;.db $00 $00 $FF $0F $00 $00 $AF $00 $5F $00 $5A $00 $FF $00 $FF $0A $AF $0A
;    $50 $0A $A0 $0A $00 $05 $AA $0F $FA $0F $00 $0F $50 $0F
;    
;.db $00,$00,$FF,$0F,$00,$00,$AF,$00,$5F,$00,$5A,$00,$FF,$00,$FF,$0A,
;    $AF,$0A,$50,$0A,$A0,$0A,$00,$05,$AA,$0F,$FA,$0F,$00,$0F,$50,$0F
