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
; TODO: See if there's a standard .h file for this
.define VDPControl $bf
.define VDPData $be
.define JoyPortA $dc
.define JoyPortB $dd
.define Button1_bit 4
.define Button2_bit 5
.define ButtonR_bit 3
.define ButtonL_bit 2
.define ButtonD_bit 1
.define ButtonU_bit 0
.define VRAMWrite $4000
.define CRAMWrite $c000


;==============================================================
; RAM variables
;==============================================================
.enum $C000 export

; Sprite attribute table buffer
    SATBuffer_YPos dsb 64  ; Y positions
    SATBuffer_XPos dsb 128 ; X positions and tile indeces
; VDP Mirrors
    VDPMirror .dw
    VDPMirror_ModeA dw
    VDPMirror_ModeB dw
    VDPMirror_NametableAddr dw
    VDPMirror_SATAddr dw
    VDPMirror_SpriteChrAddr dw
    VDPMirror_OverscanColor dw
    VDPMirror_XScroll dw
    VDPMirror_YScroll dw 
    VDPMirror_LineCounter dw
    VDPMirror_End .dw

; VDP Related Variables
    VBlank_ReadyFlag db ; 1 = Ready for VBlank, 0 = VBlank is done

    Input_PlayerA db
    Input_PlayerARising db
    Input_PlayerB db
    Input_PlayerBRising db

    FrameCount db ; Counts frames

    tempVRAMAddr dw

.ende



; ROM starts here
.bank 0 slot 0

;==============================================================
; Boot section
;==============================================================
.org $0000
    di              ; disable interrupts
    im 1            ; Interrupt mode 1
    jp main         ; jump to main program


.org $0038
interruptHandler_stub: ;{ Handles VBlank and HBlank
    jp interruptHandler
;}

.org $0066
;==============================================================
; Pause button handler
;==============================================================
    ; Do nothing
    retn


;==============================================================
; Interrupt Handler
;==============================================================
interruptHandler: ;{
    push af
    push bc
    push de
    push hl
    ; Distinguish between HBlank and VBlank
    in a,(VDPControl) ; Read status, clear interrupt flag
    bit 7, a
    jr z, @branch_HBlank
        ; Check VBlank flag
        ld a, (VBlank_ReadyFlag)
        cp a, 1
            jr nz, @exitInterrupt
        ; Clear it
        xor a
        ld (VBlank_ReadyFlag), a
        
        ; VBlank always:

        ; Updates music
        ; Resets scroll value
        ; Preps HBlank interrupts (if applicable)
        ; Only if VBlank is ready:
            ; Write VBlank registers
            ld hl,VDPMirror
            ld b,VDPMirror_End-VDPMirror
            ld c,VDPControl
            otir
            ; Transfer data lists
            ; Transfer sprite lists
            ;ld hl, VRAMWrite | $3F00
            ld a, <VRAMWrite | <$3F00 ; low byte
            out (VDPControl),a
            ld a, >VRAMWrite | >$3F00 ; high byte
            out (VDPControl),a
            ld hl,SATBuffer_YPos
            ld b,64
            ld c,VDPData
            otir
            
            ld a, < VRAMWrite | <$3F80 ; low byte
            out (VDPControl),a
            ld a, > VRAMWrite | >$3F80 ; high byte
            out (VDPControl),a
            ld hl,SATBuffer_XPos
            ld b,128
            ld c,VDPData
            otir
            
            ; Update scroll value
            ; Sets "VBlank is over" flag
            ; Read input
            call readInput
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

readInput: ;{
    ; Read registers
    in a, (JoyPortA)
    ld d, a
    in a, (JoyPortB)
    ld e, a
    
    ; Read Player 1 inputs
    ld a, d
    xor a, $FF ; Invert so 1 is pressed, 0 is not pressed
    and a, %00111111
    ld b, a ; Temp save new input
    ld a, (Input_PlayerA) ; Get the old input
    xor b
    and b
    ld (Input_PlayerARising), a
    ld a, b
    ld (Input_PlayerA), a
    
    ; Read Player 2 inputs
    ; Bit twiddling
    ld a, d
    xor $FF
    and %11000000
    ld b, a
    ld a, e
    xor $FF
    and %00001111
    or b
    rlca
    rlca
    ; Repeat of the above
    ld b, a
    ld a, (Input_PlayerB) ; Get the old input
    xor b
    and b
    ld (Input_PlayerBRising), a
    ld a, b
    ld (Input_PlayerB), a
ret ;}

;==============================================================
; Init program
;==============================================================
main:
    ld sp, $dff0
    
    ; Clear RAM
    ld hl, $C000
    ld bc, $2000 ; Clear all 8K
-   xor a
    ld (hl), a
    inc hl
    dec bc
    ld a, b
    or c
    jr nz, -

    ;==============================================================
    ; Set up VDP registers
    ;==============================================================
    ld hl,VDPInitData
    ld bc,VDPInitData_End-VDPInitData
    ld de,VDPMirror 
    ldir
    
    ld hl,VDPMirror
    ld b,VDPMirror_End-VDPMirror
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

clearSprites:
    ld hl, SATBuffer_YPos
    ld a, $D0 ; Ypos
    ld b, $40 ; number of sprites
    @loop:
        ld (hl), a
        inc hl
        dec b
    jr nz, @loop

    ;==============================================================
    ; Load palette
    ;==============================================================
loadPalette:
    ; 1. Set VRAM write address to CRAM (palette) address 0
    ld hl, CRAMWrite|$0000
    call SetVDPAddress
    ; 2. Output colour data
    ld hl, PortraitPal
    ld bc, PortraitPalSize
    call CopyToVDP
    
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
    ld hl, $3800 + $40*9 | VRAMWrite ;+ $2*17 | VRAMWrite
    call SetVDPAddress
    ld (tempVRAMAddr), hl
;    ld a, h
;    ld tempVRAMAddr+1, a
    
    ld hl, PortraitMap
    ld bc, PortraitMapSize
    ld d, 32*2;15*$2 ; (portrait tilemap width in bytes)
    
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
            ld d, 32*2;15*$2
        @endIf:
        dec bc
        ld a, b
        or c
        jr nz, @loop
    @loopExit:
    
    ; Turn screen on
    ld a,%01100000
;          ||||||`- Zoomed sprites -> 16x16 pixels
;          |||||`-- Doubled sprites -> 2 tiles per sprite, 8x16
;          ||||`--- Mega Drive mode 5 enable
;          |||`---- 30 row/240 line mode
;          ||`----- 28 row/224 line mode
;          |`------ VBlank interrupts
;          `------- Enable display
    ld (VDPMirror_ModeB), a
    out (VDPControl),a
    ld a,$81
    out (VDPControl),a
    
    ei

;==============================================================
; Main program
;==============================================================
MainLoop:
    ; TODO: Logic goes here
    ld a, (FrameCount)
    inc a
    ld (FrameCount), a
    
    ld a, (Input_PlayerA)
    bit ButtonR_bit, a
    jr z, @endIfA
        ld a, (VDPMirror_XScroll)
        inc a
        inc a
        ld (VDPMirror_XScroll), a
    @endIfA:

    ld a, (Input_PlayerA)
    bit ButtonL_bit, a
    jr z, @endIfB
        ld a, (VDPMirror_XScroll)
        dec a
        dec a
        ld (VDPMirror_XScroll), a
    @endIfB:
    
    ld a, $01
    ld (VBlank_ReadyFlag), a
    @waitLoop:
        halt
        ld a, (VBlank_ReadyFlag)
        cp a, $00
    jr nz, @waitLoop
    
jr MainLoop

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

; VDP initialisation data
VDPInitData:
.db %00100100,$80 ; Rendering properties 1
.db $00,$81 ; Rendering properties 2
.db $ff,$82 ; Nametable address
.db $ff,$85 ; Sprite attribute table address
.db $fd,$86 ; Sprite tileset address
.db $00,$87 ; Overscan color
.db $00,$88 ; X Scroll
.db $00,$89 ; Y Scroll
.db $ff,$8a ; Line Counter
VDPInitData_End:

FontData:
.incbin "gfx/font.chr" fsize FontDataSize


PortraitMap:
.incbin "gfx/samantha.map" fsize PortraitMapSize
PortraitChr:
.incbin "gfx/samantha.chr" fsize PortraitChrSize
PortraitPal:
.incbin "gfx/samantha.pal" fsize PortraitPalSize

;.db $00 $00 $FF $0F $00 $00 $AF $00 $5F $00 $5A $00 $FF $00 $FF $0A $AF $0A
;    $50 $0A $A0 $0A $00 $05 $AA $0F $FA $0F $00 $0F $50 $0F
;    
;.db $00,$00,$FF,$0F,$00,$00,$AF,$00,$5F,$00,$5A,$00,$FF,$00,$FF,$0A,
;    $AF,$0A,$50,$0A,$A0,$0A,$00,$05,$AA,$0F,$FA,$0F,$00,$0F,$50,$0F
