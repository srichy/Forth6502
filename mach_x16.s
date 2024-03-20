mach_init0:
    rts

mach_init1:
    rts

mach_dbg:
    ;; A has low order of new IP
    rts

con_init:
    jsr $ff81                   ; CINT
    lda #$0f                    ; ISO mode
    sta screen_y                ; force recompute in gotoxy
    jsr $FFD2
    ldx #0
    ldy #0
    jsr gotoxy
    rts                         ; Using C64-compatible kernal chrin/chrout

con_tx:
    ;jsr $FFD2                   ; C64 CHROUT
    jsr dispchar
    rts

con_rx:
-   jsr $FFE4                   ; C64 GETIN
    cmp #0
    beq -
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

    vera_incr_1_h = $11         ; increment by 2; 1 for high-order addr bit

screen_x: .byte ?
screen_y: .byte ?
scrptr: .addr ?

dispchar:
    sta vera_data0
    ldy screen_y
    inc screen_x
    ldx screen_x
    cpx #80
    bcc _done
    ldx #0
    iny
_done:
    jsr gotoxy
    rts

gotoxy:
    pha
    stx screen_x
    cpy screen_y
    beq _add_col
    sty screen_y
    lda #<vera_text_base
    sta scrptr
    lda #>vera_text_base
    sta scrptr+1
_add_row:
    cpy #0
    beq _add_col
    dey
    clc
    lda #160
    adc scrptr
    sta scrptr
    bcc _add_row
    inc scrptr+1
    bra _add_row
_add_col:
    clc
    txa
    asl                         ; x2; Thanks, VERA. :(
    adc scrptr
    sta vera_addrx_l
    lda #0
    adc scrptr+1
    sta vera_addrx_m
    lda #vera_incr_1_h
    sta vera_addrx_h
    pla
    rts

scroll_up:
    brk
    nop
