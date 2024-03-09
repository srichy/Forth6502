mach_init0:
    lda 1
    pha
    stz 1
    lda #$00
    sta $d001

    lda #32
    sta $d012                   ; space for cursor?

    pla
    sta 1
    rts

mach_init1:
    rts

hex_digit .text "0123456789ABCDEF"
    ip_disp_addr = $c000 + (1 * 80) + 72
ip_disp_label .null "IP:"
    word_disp_addr = $c000 + (0 * 80) + 72
    rsp_disp_addr = $c000 + (2 * 80) + 72
    psp_disp_addr = $c000 + (3 * 80) + 72
    here_disp_addr = $c000 + (4 * 80) + 72
    pstk_disp_addr = $c000 + (5 * 80) + 72
here_disp_label .null "H :"

hon:
    ldy #4
-   lsr
    dey
    bne -
    and #$0f
    tax
    lda hex_digit,x
    rts

lon:
    and #$0f
    tax
    lda hex_digit,x
    rts

show_ip:
    ldx #0
-   lda ip_disp_label,x
    beq +
    sta ip_disp_addr,x
    inx
    bra -
+   lda w+1
    pha
    jsr hon
    sta ip_disp_addr+3
    pla
    jsr lon
    sta ip_disp_addr+4
+   lda w
    pha
    jsr hon
    sta ip_disp_addr+5
    pla
    jsr lon
    sta ip_disp_addr+6
    rts

show_here:
    ldx #0
-   lda here_disp_label,x
    beq +
    sta here_disp_addr,x
    inx
    bra -
+   lda here_store+1
    pha
    jsr hon
    sta here_disp_addr+3
    pla
    jsr lon
    sta here_disp_addr+4
+   lda here_store
    pha
    jsr hon
    sta here_disp_addr+5
    pla
    jsr lon
    sta here_disp_addr+6
    rts

show_word:
    sec
    lda w
    sbc #7                      ; Just use the space after-padding, not len
    sta mac
    lda w+1
    sbc #0
    sta mac+1
    ldy #0
-   lda (mac),y
    sta word_disp_addr,y
    iny
    cpy #5
    bne -

    rts

show_rsp:
    lda #'R'
    sta rsp_disp_addr
    lda #':'
    sta rsp_disp_addr+1
    clc
    lda #rstk
    adc rsp
    pha
    jsr hon
    sta rsp_disp_addr+2
    pla
    jsr lon
    sta rsp_disp_addr+3
    lda #','
    sta rsp_disp_addr+4
    sec
    lda #STACK_MEM
    sbc rsp
    pha
    jsr hon
    sta rsp_disp_addr+5
    pla
    jsr lon
    sta rsp_disp_addr+6

    rts

show_psp:
    lda #'P'
    sta psp_disp_addr
    lda #':'
    sta psp_disp_addr+1
    tsx
    txa
    pha
    jsr hon
    sta psp_disp_addr+2
    pla
    jsr lon
    sta psp_disp_addr+3
    lda #','
    sta psp_disp_addr+4
    tsx
    txa
    eor #$ff
    lsr
    pha
    jsr hon
    sta psp_disp_addr+5
    pla
    jsr lon
    sta psp_disp_addr+6

    rts

mach_delay:
    ldx #0
x   ldy #0
y   dey
    bne y
    dex
    bne x
    rts

mach_dbg:
    rts
    ;; A has low order of new IP
    lda $01
    pha
    lda #2
    sta $01

    jsr show_word
    jsr show_ip
    jsr show_rsp
    jsr show_psp
    jsr show_here

    pla
    sta $01
    ;jsr mach_delay
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

raw_bs:
    lda #8
    rts
