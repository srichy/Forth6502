mach_init0:
    sei
    rts

mach_init1:
    lda #1
    jsr set_led
    jsr con_init
    rts

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
