mach_init0:
    rts

mach_init1:
    rts

mach_reset:
    ldx #$42
    ldy #$02
    lda #$00
    jsr $FEC9

mach_dbg:
    ;; A has low order of new IP
    rts

mach_hex_char:
    tsx
    lda $101,x
    cmp #10
    bcc _is_digit
    sec
    sbc #9
    bra _done
_is_digit:
    clc
    adc #$30
_done:
    sta $101,x
    jmp do_next

con_init:
    jsr $ff81                   ; CINT
    lda #$0f                    ; ISO mode
    jsr $FFD2
    ldx #0
    ldy #0
    jsr gotoxy
    rts                         ; Using C64-compatible kernal chrin/chrout

con_tx:
    jsr $FFD2                   ; C64 CHROUT
    rts

con_rx:
    jsr $FFCF                   ; C64 CHRIN
    rts

raw_bs:
    lda #20
    rts

ll_mult:
    tsx
    lda $103,x
    stz $103,x
    sta mac
    lda $104,x
    stz $104,x
    sta mac+1
    ldy #16
again
    ;; Shift multiplier 1 bit right and check for carry
    lda $102,x
    lsr
    sta $102,x
    lda $101,x
    ror
    sta $101,x
    bcc skip_add
    ;; Carry set. Add multiplicant to accumulator
    clc
    lda mac
    adc $103,x
    sta $103,x
    lda mac+1
    adc $104,x
    sta $104,x
skip_add
    lda mac
    asl
    sta mac
    lda mac+1
    rol
    sta mac+1
    dey
    bne again
    pla
    pla
    jmp do_next

ll_slash_mod:
    tsx
    lda $101,x
    sta divisor
    lda $102,x
    sta divisor+1
    lda $103,x
    sta dividend
    lda $104,x
    sta dividend+1
    ;; Taken almost verbatim from the WDC 65816 6502 Programming Manual
    lda #0
    tax
    pha
    ldy #1
    lda divisor
    bmi div2
div1:
    iny
    asl divisor
    rol divisor+1
    bmi div2
    cpy #17
    bne div1
div2:
    sec
    lda dividend
    sbc divisor
    pha
    lda dividend+1
    sbc divisor+1
    bcc div3
    sta dividend+1
    pla
    sta dividend
    pha
div3:
    pla
    pla
    rol
    pha
    txa
    rol
    tax
    lsr divisor+1
    ror divisor
    dey
    bne div2
done:
    pla
    tay
    txa
    tsx
    sta $102,x
    tya
    sta $101,x
    lda dividend
    sta $103,x
    lda dividend+1
    sta $104,x
    jmp do_next

    vera_addrx_l = $9f20
    vera_addrx_m = $9f21
    vera_addrx_h = $9f22
    vera_data0 = $9f23
    vera_data1 = $9f24
    vera_ctrl = $9f25
    vera_text_base = $1b000

    vera_incr_1_h = $21         ; 1 for high-order addr bit
    vera_addr0 = $00            ; for ADDRSEL in vera_ctrl
    vera_addr1 = $01

screen_x: .byte ?
screen_y: .byte ?
scrptr: .addr ?

gotoxy:
    clc
    jsr $fff0

    ;; A has the number of lines.  >=60 is "clear screen"
    ;; mac0,1 == dest
    ;; mac2,3 == src
    ;; mac4 == scroll count
scroll_up:
    cmp #60
    bcc _in_bounds
    lda #60
_in_bounds:
    sta mac+4                   ; Save line count
    lda #vera_addr0             ; Init dest line address
    sta vera_ctrl
    stz vera_addrx_l
    lda #>vera_text_base
    sta vera_addrx_m
    lda #vera_incr_1_h
    sta vera_addrx_h
    lda #vera_addr1             ; Init  src line address
    sta vera_ctrl
    stz vera_addrx_l
    clc
    lda #>vera_text_base
    adc mac+4
    sta vera_addrx_m
    lda #vera_incr_1_h
    sta vera_addrx_h
    sec
    lda #60
    sbc mac+4
    tax                         ; Count of line moves
_line:
    ldy #80
    cpx #0
    beq _do_blanks
_char:
    lda vera_data1
    sta vera_data0
    dey
    bne _char
    lda #vera_addr0             ; Update dest
    sta vera_ctrl
    stz vera_addrx_l
    inc vera_addrx_m
    lda #vera_addr1             ; Update src
    sta vera_ctrl
    stz vera_addrx_l
    inc vera_addrx_m
    dex
    bne _line
_do_blanks:
    lda #vera_addr0
    sta vera_ctrl
    lda #vera_incr_1_h
    sta vera_addrx_h
    sec
    lda #60
    sbc mac+4
    tax                         ; X now has current blanking line
_blank_line:
    txa
    clc
    adc #>vera_text_base
    sta vera_addrx_m
    stz vera_addrx_l
    ldy #80
    lda #32
_blank_char:
    sta vera_data0
    dey
    bne _blank_char
    inx
    cpx #60
    bne _blank_line
    rts

pagesize:
    rts
