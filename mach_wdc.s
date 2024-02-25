mach_init0:
    sei
    rts

mach_init1:
    lda #1
    jsr set_led
    jsr usb_init
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

usb_init:
    ;; Set USB_CTRL_WR and USB_CTRL_RDb pins as OUTPUT; others as input
    lda #USB_CTRL_WR|USB_CTRL_RDb
    sta USB_CTRL_DDR
    rts

usb_tx:
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

usb_rx:
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
