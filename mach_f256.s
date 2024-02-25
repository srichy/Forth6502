mach_init0:
    rts

mach_init1:
    rts

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
