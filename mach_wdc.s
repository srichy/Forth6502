mach_init0:
    sei
    ldx #80
    ldy #24
    jsr pagesize
    rts

mach_init1:
    lda #1
    jsr set_led
    jsr con_init
    rts

mach_reset:
    jmp start

mach_hex_char:
    tsx
    lda $101,x
    and #$0f
    cmp #10
    bcc _is_digit
    clc
    adc #55
    bra _done
_is_digit:
    clc
    adc #$30
_done:
    sta $101,x
    jmp do_next

mach_dbg:
    ;; A has low order of new IP
    jsr set_led
    rts

set_led:
    pha                         ; Save selected LED(s)
    lda #00                     ; DDRA access
    sta PIA_A_CTRL
    lda #$ff                    ; All bits as "output"
    sta PIA_A_DATA
    lda #04                     ; ORA access
    sta PIA_A_CTRL
    pla                         ; Fetch select LED(s)
    sta PIA_A_DATA
    rts

con_init:
    ;; Set USB_CTRL_WR and USB_CTRL_RDb pins as OUTPUT; others as input
    lda #USB_CTRL_WR|USB_CTRL_RDb
    sta USB_CTRL_DDR
    rts

con_tx:
    pha
    lda #0
    sta USB_DATA_DDR
    pla
    sta USB_DATA_OR
    lda #USB_CTRL_TXEb
    ;; Wait for transmitter availability
-   bit USB_CTRL_IR
    bne -
    lda #$ff
    sta USB_DATA_DDR
    lda #USB_CTRL_WRSTR
    sta USB_CTRL_OR
    nop
    nop
    lda #USB_CTRL_DEF
    sta USB_CTRL_OR
    rts

con_rx:
    lda #0
    sta USB_DATA_DDR
    lda #USB_CTRL_RXFb
-   bit USB_CTRL_IR
    bne -
    lda #USB_CTRL_RDSTR
    sta USB_CTRL_OR
    nop
    nop
    nop
    nop
    lda USB_DATA_IR
    pha
    lda #USB_CTRL_DEF
    sta USB_CTRL_OR
    pla
    rts

raw_bs:
    lda #8
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

pagesize:
    sty size_y
    stx size_x
    rts

    ;; cup=\E[%i%p1%d;%p2%dH
    ;; cursor position by sending
    ;; ESC[row;colH
    ;; where row = y, col = x
gotoxy:
    phy
    phx
    lda #27                     ;ESC
    jsr con_tx
    lda #"["
    jsr con_tx
    pla
    jsr prt_dec_num
    lda #";"
    jsr con_tx
    pla
    jsr prt_dec_num
    lda #"H"
    jsr con_tx
    rts

clear_seq:  .byte 27,"[","H",27,"[","J",0
scroll_up:
    cmp #255
    bne _done
    ldx #0
_another:
    lda clear_seq,x
    beq _done
    jsr con_tx
    inx
    bra _another
_done:
    rts
