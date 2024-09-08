mach_init0:
    rts

mach_init1:
    rts

mach_reset:
    .ax8
    ldx #$42
    ldy #$02
    lda #$00
    jsr $FEC9

mach_dbg:
    ;; A has low order of new IP
    rts

mach_hex_char:
    .a8
    lda 1,s
    cmp #10
    bcc _is_digit
    sec
    sbc #9
    bra _done
_is_digit:
    clc
    adc #$30
_done:
    sta 1,s
    jmp do_next

con_init:
    .ax8
    jsr $ff81                   ; CINT
    lda #$0f                    ; ISO mode
    jsr $FFD2
    ldx #0
    ldy #0
    jsr gotoxy
    rts                         ; Using C64-compatible kernal chrin/chrout

con_tx:
    .ax8
    jsr $FFD2                   ; C64 CHROUT
    rts

con_rx:
    .ax8
    jsr $FFCF
    rts

raw_bs:
    .a8
    lda #20
    rts

ll_mult:
    .ax16
    lda 3,s
    sta mac
    lda 1,s
    sta mac+2
    lda #0
_again
    ldx mac
    beq _done
    lsr mac
    bcc _skip_add
    clc
    adc mac+2
_skip_add
    asl mac+2
    bra _again
_done
    sta 3,s
    pla
    jmp do_next

ll_slash_mod:
    .ax16
    ;; Taken almost verbatim from the WDC 65816 6502 Programming Manual
    lda 3,s
    tax
    lda 1,s
    stz quotient
    ldy #1
_div1:
    asl a
    bcs _div2
    iny
    cpy #17
    bne _div1
_div2:
    ror a
_div4:
    pha
    txa
    sec
    sbc 1,s
    bcc _div3
    tax
_div3:
    rol quotient
    pla
    lsr a
    dey
    bne _div4
_done:
    txa
    sta 3,s
    lda quotient
    sta 1,s
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
    .ax8
    clc
    jsr $FFF0
    rts

    ;; A has the number of lines.  >=60 is "clear screen"
    ;; mac0,1 == dest
    ;; mac2,3 == src
    ;; mac4 == scroll count
scroll_up:
    .ax8
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
