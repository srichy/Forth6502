    .cpu "65c02"

    .if targ=="wdc"
    .include "wdc65c816sxb.inc"
    .endif

    STACK_MEM = $80 - $22 - $10

    ;; Set up symbols for the zero elements
    * = $22
rsp:    .byte ?
wjmp:   .byte ?
w:      .addr ?
ip:     .addr ?
rstk:   .fill STACK_MEM
mac:    .fill 6
here_store: .addr ?

    .if targ=="wdc"
    * = $200
    .elsif targ=="x16"

    *       = $0801
    .word (+), 2005  ;pointer, line number
    .null $9e, format("%4d", start);will be sys 4096
+   .word 0          ;basic line end

    *   = $0820

    .elsif targ=="f256"

    * = $1000

    .endif

start:
    .if targ=="wdc"
    sei
    .endif
    lda #$4c               ; jmp a opcode
    sta wjmp               ; Now we can do 'jmp wjmp' to get 'jmp (w)'
    ;; Init parameter stack
    lda #STACK_MEM
    sta rsp
    ;; Init return stack
    ldx #$ff
    txs
    ;; Init hardware.  FIXME to make this per-platform
    .if targ=="wdc"
    lda #1
    jsr set_led
    jsr usb_init
    .endif
    ;; Load IP with "cold"
    ;; Then fall through to "next"
    lda #<w_cold.cfa
    sta w
    lda #>w_cold.cfa
    sta w+1
    jmp wjmp
    ;; The following should never be reached
    bra start

do_next:
    ldy #1
    lda (ip)
    sta w
    ;jsr set_led                 ; DEBUG blinky activity
    lda (ip),y
    sta w+1
    clc
    lda ip
    adc #2
    sta ip
    lda ip+1
    adc #0
    sta ip+1
    jmp wjmp

    .if targ=="wdc"
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

    .endif                      ; targ=="wdc"

    .if targ=="x16"

usb_init:
    rts                         ; Using C64-compatible kernal chrin/chrout

usb_tx:
    jsr $FFD2                   ; C64 CHROUT
    rts

usb_rx:
-   jsr $FFE4                   ; C64 GETIN
    cmp #0
    beq -
    rts

    .endif                      ; targ=="x16"

    .if targ=="f256"

usb_init:
    ;jsr $FF81
    rts

usb_tx:
    jsr $FFD2                   ; C64 CHROUT
    rts

usb_rx:
-   jsr $FFE4                   ; C64 GETIN
    cmp #0
    beq -
    rts

    .endif                      ; targ=="x16"

    .include "fth_main.s"

edata: .addr *
