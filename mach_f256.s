mach_init0:
    rts

mach_init1:
    rts

mach_dbg:
    ;; A has low order of new IP
    rts

con_init:
    ;jsr $FF81
    rts

con_tx:
    jsr $FFD2                   ; C64 CHROUT
    rts

con_rx:
-   jsr $FFE4                   ; C64 GETIN
    cmp #0
    beq -
    rts
