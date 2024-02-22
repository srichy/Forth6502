    .cpu "65c02"
    .include "wdc65c816sxb.inc"

    STACK_SIZE = 48

    ;; Set up symbols for the zero elements
    * = $24
rsp:    .byte ?
wjmp:   .byte ?
w:      .addr ?
ip:     .addr ?
rstk:   .fill (STACK_SIZE * 2)
mac:    .fill 4
    * = $200

start:
    sei
    lda #$6c               ; jmp (a) opcode
    sta wjmp               ; Now we can do 'jmp wjmp' to get 'jmp (w)'
    ;; Init parameter stack
    lda #(STACK_SIZE << 1) - 1
    sta rsp
    ;; Init return stack
    ldx #$ff
    txs
    ;; Init hardware.  FIXME to make this per-platform
    lda #1
    jsr set_led
    jsr usb_init
    ;; Load IP with "cold"
    ;; Then fall through to "next"
    lda #<w_cold.cfa
    sta ip
    lda #>w_cold.cfa
    sta ip+1
    ;; fall through to do_next

do_next:
    ldy #1
    lda (ip)
    sta w
    jsr set_led                 ; DEBUG blinky activity
    lda (ip),y
    sta w+1
    clc
    lda ip
    adc #2
    sta ip
    bcc +
    inc ip+1
+   jmp wjmp

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

    .include "fth_main.s"
