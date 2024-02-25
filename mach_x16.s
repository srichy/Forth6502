mach_init0:
    rts

mach_init1:
    rts

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

