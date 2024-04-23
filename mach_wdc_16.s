mach_init0:
    .ax8
    sei
    ldx #80
    ldy #24
    jsr pagesize
    rts

mach_init1:
    .ax8
    lda #1
    jsr set_led
    jsr con_init
    rts

mach_hex_char:
    .ax8
    lda 1,s
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
    sta 1,s
    jmp do_next

mach_dbg:
    ;; A has low order of new IP
    ;jsr set_led
    rts

set_led:
    .ax8
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
    .ax8
    ;; Set USB_CTRL_WR and USB_CTRL_RDb pins as OUTPUT; others as input
    lda #USB_CTRL_WR|USB_CTRL_RDb
    sta USB_CTRL_DDR
    rts

con_tx:
    .ax8
    stz USB_DATA_DDR
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
    .ax8
    stz USB_DATA_DDR
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
    .ax8
    lda #8
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

pagesize:
    .ax8
    sty size_y
    stx size_x
    rts

    ;; cup=\E[%i%p1%d;%p2%dH
    ;; cursor position by sending
    ;; ESC[row;colH
    ;; where row = y, col = x
gotoxy:
    .ax8
    phx
    phy
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
    .ax8
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
