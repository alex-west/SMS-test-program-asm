;==============================================================
; SDSC tag and SMS rom header
;==============================================================
.sdsctag 0.1,"Hello World!","SMS test program","Alex West"

; based on Maxim's "Hello World!" tutorial, found here:
; https://www.smspower.org/maxim/HowToProgram/Index

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

; VDP Data Transfer Buffer
    VDPTransferPointer dw ; Current index into the transfer buffer (two bytes because I might expand it)
    ; dw VRAM Dest Addr
    ; db Length
    ; dsb Data (source implied is here)
    VDPTransferBuffer dsb 256

; Inputs
    Input_PlayerA db
    Input_PlayerARising db
    Input_PlayerB db
    Input_PlayerBRising db
    Input_PauseFlag db

    FrameCount db ; Counts frames

    tempVRAMAddr dw
    tempVRAMWidth db

; Object Test Variables
    objTimer db
    objCharPointer dw
    objX db
    objY db

; Raster Effect Variables
    ; TODO: Use a tabular approach
    Raster_LineCounterNext db
    Raster_XScroll db
    RasterMirror_LineCounterNext db
    RasterMirror_XScroll db
    
.ende



; ROM starts here
.bank 0 slot 0

;==============================================================
; Boot section
;==============================================================
.org $0000
Boot:
    di              ; disable interrupts
    im 1            ; Interrupt mode 1
    ld sp, $dff0
    jp MainInit     ; jump to main program

.org $0038
interruptHandler_stub: ;{ Handles VBlank and HBlank
    jp interruptHandler
;}

.org $0066 ; Pause button handler
pauseButtonHandler: ;{
    push af
        ld a,$01
        ld (Input_PauseFlag),a
    pop af
retn ;}

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
        jr nz, @branch_EveryVBlank
            ; Only if VBlank is ready:
            ; Clear VBlank flag
            xor a
            ld (VBlank_ReadyFlag), a
            
            ; Write VBlank registers (incl. scroll values)
            call VBlank_WriteVDPRegisters
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
            
            ; Update raster tables
            ld a,(RasterMirror_LineCounterNext)
            ld (Raster_LineCounterNext),a
            ld a,(RasterMirror_XScroll)
            ld (Raster_XScroll),a
            
            ; Transfer color lists (must happen during actual VBlank?)
            
            ; Transfer data lists
            ld hl, VDPTransferBuffer
            @DataTransferLoop:
                ; Load destination address
                ld e,(hl)
                inc hl
                ld d,(hl)
                inc hl
                ld a,e
                or d
                    jr z, @ExitDataTransferLoop
                ld c,VDPControl
                out (c),e
                out (c),d
                ; Load length
                ld b,(hl)
                ; Set hl to source
                inc hl
                ; Set port
                ld c,VDPData
                otir
                jr @DataTransferLoop
            @ExitDataTransferLoop:
            xor a
            ld (VDPTransferBuffer),a
            ld (VDPTransferBuffer+1),a
            ld a,<VDPTransferBuffer
            ld (VDPTransferPointer),a
            ld a,>VDPTransferBuffer
            ld (VDPTransferPointer+1),a
            ; Read input
            call readInput
            
        @branch_EveryVBlank:
            ; Updates music
            ; Resets scroll value (in case of raster effect)
            ; Preps HBlank interrupts (if applicable)
        jr @exitInterrupt
        
        @branch_HBlank:
        ; HBlank
            ; Write value for next line and x scroll
            ld a, (Raster_LineCounterNext)
            out (VDPControl),a
            ld a, $8A
            out (VDPControl),a
            
            ld a, (Raster_XScroll) 
            out (VDPControl),a
            ld a, $88
            out (VDPControl),a
            ; Set up next interrupt
@exitInterrupt            
    pop hl
    pop de
    pop bc
    pop af
    ei ; ?
    reti
;}

VBlank_WriteVDPRegisters: ;{
    ld hl,VDPMirror
    ld b,VDPMirror_End-VDPMirror
    ld c,VDPControl
    otir
ret ;}

readInput: ;{
    ; Read registers
    in a, (JoyPortA)
    ld d, a
    in a, (JoyPortB)
    ld e, a
    
    ; Read Player 1 inputs
    ld a, d
    cpl
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
    cpl
    and %11000000
    ld b, a
    ld a, e
    cpl
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
; Init System
;==============================================================
VDPInitData:;{ ; VDP initialisation data
    .db %01110100,$80 ; Rendering properties 1
    .db %10000000,$81 ; Rendering properties 2
    .db $ff,$82 ; Nametable address
    .db $ff,$85 ; Sprite attribute table address
    .db $fd,$86 ; Sprite tileset address
    .db $00,$87 ; Overscan color
    .db $00,$88 ; X Scroll
    .db $00,$89 ; Y Scroll
    .db 72,$8a;$ff,$8a ; Line Counter
VDPInitData_End: ;}

MainInit:;{
; Clear RAM
    ld hl, $C000
    ld bc, $2000 ; Clear all 8K (note: this clobbers the stack)
    @LoopRAM:
        xor a
        ld (hl), a
        inc hl
        dec bc
        ld a, b
        or c
    jr nz, @LoopRAM

; Initialize VDP
    ; Set up mirrors
    ld hl,VDPInitData
    ld bc,VDPInitData_End-VDPInitData
    ld de,VDPMirror 
    ldir
    ; Copy mirrors
    call VBlank_WriteVDPRegisters
    ; Init this silly variable
    ld a,<VDPTransferBuffer
    ld (VDPTransferPointer),a
    ld a,>VDPTransferBuffer
    ld (VDPTransferPointer+1),a

; Clear VRAM
    ld hl, VRAMWrite|$0000
    call SetVDPAddress
    ld bc,$4000     ; Counter for 16KB of VRAM
    @LoopVRAM:
        xor a
        out (VDPData),a ; Output to VRAM address, which is auto-incremented after each write
        dec bc
        ld a,b
        or c
    jr nz,@LoopVRAM

; Clear CRAM
    ld hl, CRAMWrite|$0000
    call SetVDPAddress
    ld b,32
    xor a
    @LoopCRAM:
        out (VDPData),a
    djnz @LoopCRAM

; Clear Sprites
    ld hl, SATBuffer_YPos
    ld a, $D0 ; Ypos
    ld b, $40 ; number of sprites
    @LoopSAT:
        ld (hl), a
        inc hl
        dec b
    jr nz, @LoopSAT
;}

;==============================================================
; Prep Before Main Loop
;==============================================================
PrepGame: ;{
; Load Palette
    ; 1. Set VRAM write address to CRAM (palette) address 0
    ld hl, CRAMWrite|$0000
    call SetVDPAddress
    ; 2. Output colour data
    ld hl, gfx_SamanthaPal ; PortraitPal
    ld bc, SamanthaPalSize ; PortraitPalSize
    call CopyToVDP
    
; Load Font
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
    ld hl, gfx_SamanthaChr
    ld bc, SamanthaChrSize
    call CopyToVDP

; Write portrait tilemap
    ld bc, $0009
    call GetTilemapAddress
    ld de, gfxInfo_SamanthaMap
    ;ld de, gfxInfo_GadflyMap
    call unsafe_WritePartialTilemap
    
; Write text to name table in status bar area
    ld bc,$0201
    ld hl,Message
    call unsafe_WriteString
    
; Prepare text writer object
    ld hl, Message
    ld (objCharPointer),hl
    ld a,5
    ld (objX),a
    ld a,4
    ld (objY),a
    
; Set up raster variables
    ld a,$FF
    ld (RasterMirror_LineCounterNext),a
    ld (Raster_LineCounterNext),a
    ld a,$00
    ld (RasterMirror_XScroll),a
    ld (Raster_XScroll),a
    
MainScreenTurnOn: ; Jump target in case I want to test skipping anything above here
; Turn screen on
    ld a,%11100000
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
;}

;==============================================================
; Main program
;==============================================================
MainLoop: ;{
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
    
    call WriteString
    
    call WaitForVBlank
    
jr MainLoop ;}

WaitForVBlank: ;{
    ; Clear the last two bytes of VDPTransferBuffer here
    ld hl,(VDPTransferPointer)
    xor a
    ld (hl),a
    inc hl
    ld (hl),a
    
    ld a, $01
    ld (VBlank_ReadyFlag), a
    @waitLoop:
        halt
        ld a, (VBlank_ReadyFlag)
        cp a, $00
    jr nz, @waitLoop
ret ;}

WriteString: ;{
    ld a,(objTimer)
    inc a
    ld (objTimer),a
    cp 5
    jr nz, @End
        ; Reset timer
        xor a
        ld (objTimer),a
        ; Get char, set char and attr
        ld hl,(objCharPointer)
        ld a,(hl)
        cp $ff
            jr z, @End
        ; Set char
        ld d,a
        xor a
        ld e,a
        ; Increment pointer
        inc hl
        ld (objCharPointer),hl
        ; Get x,y
        ld a,(objX)
        ld b,a
        ld a,(objY)
        ld c,a
        call safe_WriteChar
        ; Increment cursor
        ld a,(objX)
        inc a
        ld (objX),a
    @End:
ret ;}

;==============================================================
; Helper functions
;==============================================================

; Given an xy pair in BC
;  Returns a VRAM address in HL
GetTilemapAddress:;{
    ; VRAMWrite | ($3800 + $40*y + $2*x)
    push de
        ld de,$0000
        ; x*2
        ld a,b
        sla a
        ld e,a
        ; Clear b
        xor a
        ld b,a
        ; y*64
        sla c
        rl b
        sla c
        rl b
        sla c
        rl b
        sla c
        rl b
        sla c
        rl b
        sla c
        rl b
        ld hl,VRAMWrite | $3800
        add hl,bc
        add hl,de
    pop de
ret ;}

; Sets the VDP address
; Parameters: hl = address
SetVDPAddress: ;{
    push af
        ld a,l
        out (VDPControl),a
        ld a,h
        out (VDPControl),a
    pop af
ret ;}

; Copies data to the VDP
; Parameters: hl = data address, bc = data length
; Affects: a, hl, bc
CopyToVDP: ;{
    @Loop:
        ld a,(hl)    ; Get data byte
        out (VDPData),a
        inc hl       ; Point to next byte
        dec bc
        ld a,b
        or c
    jr nz,@Loop
ret ;}

; Arguments
;  HL - VRAM Destination Addr
;  DE - Pointer to Tilemap info (data pointer, data size, width)
unsafe_WritePartialTilemap: ;{
    call SetVDPAddress
    ld (tempVRAMAddr), hl
    
    ; Juggle registers
    ld l,e
    ld h,d    
    ; Load pointer to data
    ld e,(hl)
    inc hl
    ld d,(hl)
    inc hl
    ; Load length of data
    ld c,(hl)
    inc hl
    ld b,(hl)
    inc hl
    ; Load width of tilemap
    ld a,(hl)
    add a ; Don't forget to convert width from tiles to bytes :)
    ld (tempVRAMWidth),a
    ld l,e ; Finally finish juggling registers
    ld h,d
    ld d,a
    
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
            ld a,(tempVRAMWidth)
            ld d, a
        @endIf:
        dec bc
        ld a, b
        or c
        jr nz, @loop
    @loopExit:
ret ;}

; Arguments
; BC - xy
; HL - String pointer
unsafe_WriteString: ;{
    push hl
        call GetTilemapAddress
        call SetVDPAddress
    pop hl
    ; 2. Output tilemap data
    @Loop:
        ld a,(hl)
        cp $ff
            jr z,@Exit
        out (VDPData),a
        xor a
        out (VDPData),a
        inc hl
        jr @Loop
    @Exit:
ret ;}

; Arguments
; BC - xy
; HL - String pointer
safe_WriteString: ;{
    ; Write VRAM destination address
    push hl
        call GetTilemapAddress
        ld e,l
        ld d,h
        ld hl,(VDPTransferPointer)
        ld (hl),e
        inc hl
        ld (hl),d
        inc hl
        ; Save address to write length to
        ld e,l
        ld d,h
        inc hl
        ld (VDPTransferPointer),hl
        ld ix,(VDPTransferPointer)
    pop hl
        
    ; 2. Output tilemap data
    xor a
    ld b,a ; B will keep track of the string length
    @Loop:
        ; Read letter
        ld a,(hl)
        cp $ff
            jr z,@Exit
        ld (ix+0),a
        inc ix
        xor a
        ld (ix+0),a
        inc ix
        inc hl
        inc b
        inc b
        jr @Loop
    @Exit:
    ;Save IX to VDPTransferPointer
    push ix
    pop hl
    ld (VDPTransferPointer),hl
    ;Save length
    ex de,hl
    ld (hl),b
ret ;}

; Arguments
; BC - xy
; DE - character, attributes
safe_WriteChar: ;{
    ; Write VRAM destination address
    push de
        call GetTilemapAddress
        ld e,l
        ld d,h
        ld hl,(VDPTransferPointer)
        ld (hl),e
        inc hl
        ld (hl),d
        inc hl
    pop de
    ; Write length
    ld a,2
    ld (hl),a
    inc hl
    ; Write char, attribute
    ld (hl),d
    inc hl
    ld (hl),e
    inc hl
    ; Save transfer pointer
    ld (VDPTransferPointer),hl
ret ;}

; Args:
;  DE - VRAM Dest
;  C  - Transfer Length
;  HL - Data Source Pointer
safe_WriteDataToVRAM: ;{
    push hl
        ; Write things in this order
        ;  dw VRAM Dest Addr
        ;  db Length
        ;  dsb Data (WRAM source implied is here)
        ; Write VRAM destination address
        ld hl,(VDPTransferPointer)
        ld (hl),e
        inc hl
        ld (hl),d
        inc hl
        ; Write transfer length
        ld (hl),c
        inc hl
        ex de,hl
    pop hl
    xor a
    ld b,a
    ldir
    ex de,hl
    ld (VDPTransferPointer),hl
ret ;}

;==============================================================
; Data
;==============================================================

.asciitable
map " " to "~" = 0
.enda

Message:
.asc "Hello world!!!"
.db $ff

TilemapMessage:
.asc "H e l l o   w o r l d ! ! " ; Note: Don't do this.
TilemapMessage_End:

FontData:
.incbin "gfx/font.chr" fsize FontDataSize

gfxInfo_SamanthaMap:
    .dw gfx_SamanthaMap, SamanthaMapSize
    .db 32

gfx_SamanthaMap:
.incbin "gfx/samantha.map" fsize SamanthaMapSize
gfx_SamanthaChr:
.incbin "gfx/samantha.chr" fsize SamanthaChrSize
gfx_SamanthaPal:
.incbin "gfx/samantha.pal" fsize SamanthaPalSize


gfxInfo_GadflyMap:
    .dw gfx_GadflyMap, GadflyMapSize
    .db 32

gfx_GadflyMap:
.incbin "gfx/gadfly.map" fsize GadflyMapSize
gfx_GadflyChr:
.incbin "gfx/gadfly.chr" fsize GadflyChrSize
gfx_GadflyPal:
.incbin "gfx/gadfly.pal" fsize GadflyPalSize



; EoF