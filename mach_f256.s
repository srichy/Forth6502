    MULU_A_L = $de00
    MULU_A_H = $de01
    MULU_B_L = $de02
    MULU_B_H = $de03
    MULU_LL = $de10
    MULU_LH = $de11
    MULU_HL = $de12
    MULU_HH = $de13

    DIVU_D_L = $de04
    DIVU_D_H = $de05
    DIVU_N_L = $de06
    DIVU_N_H = $de07
    QUOT_L = $de14
    QUOT_H = $de15
    REMU_L = $de16
    REMU_H = $de17

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

    ;; The following is not working; fixme
    tsx
    lda $101,x
    sta DIVU_D_L
    lda $102,x
    sta DIVU_D_H
    lda $103,x
    sta DIVU_N_L
    lda $104,x
    sta DIVU_N_H

    lda QUOT_L
    sta $101,x
    lda QUOT_H
    sta $102,x
    lda REMU_L
    sta $103,x
    lda REMU_H
    sta $104,x
    jmp do_next


    ;; This lovely routine is from the Foenix F256_MicroKernel:
    ;; https://github.com/ghackwrench/F256_MicroKernel.git
    ;; If I get the integer coprocessor working, I may switch to that.
gotoxy:
    pha
    stx screen_x
    sty screen_y
    stz scrptr+1
    tya
    asl a
    asl a
    rol scrptr+1
    adc screen_y
    asl a
    rol scrptr+1
    asl a
    rol scrptr+1
    asl a
    rol scrptr+1
    asl a
    rol scrptr+1
    sta scrptr+0

    lda scrptr+1
    adc #$c0
    sta scrptr+1
    lda 1
    ldx #0
    stx 1
    ldx screen_x
    stx $d014
    ldx screen_y
    stx $d016
    sta 1
    pla
    rts

dispchar:
    pha
    ldx 1
    ldy #2
    sty 1
    ldy screen_x
    sta (scrptr),y
    stx 1
    iny
    sty screen_x
    cpy #80
    bne _done
    stz screen_x
    inc screen_y
    clc
    lda scrptr
    adc #80
    sta scrptr
    bcc _done
    inc scrptr+1
_done:
    lda 1
    ldx #0
    stx 1
    ldx screen_x
    stx $d014
    ldx screen_y
    stx $d016
    sta 1
    pla
    rts

mach_init0:
    sei
    lda #<event
    sta kernel.args.events+0
    lda #>event
    sta kernel.args.events+1
    cli
    rts

mach_init1:
    lda 1
    pha
    stz 1
    lda #1                      ;text mode only
    sta $d000                   ; Vicky master control
    lda #$00                    ; 80x60, single x, single y, font set 0
    sta $d001                   ; Vicky master control

    lda #3                      ; Cursor enable, rate = 1/2s
    sta $d010                   ; Cursor control
    lda #64                     ; Cursor character
    sta $d012
    stz $d014                   ; Cursor to 0,0
    stz $d015
    stz $d016
    stz $d017

    pla
    sta 1
    rts

screenfill:
    ldx #0
    ldy #0
    jsr gotoxy
_d:
    jsr dispchar
    ldy screen_y
    cpy #60
    bne _d
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
    lda #32
    jsr screenfill
    ldx #0
    ldy #0
    jsr gotoxy
    rts

con_tx:
    cmp #8                      ; backspace
    bne _maybe_13
    jmp do_bs
_maybe_13
    cmp #13
    bne _maybe_10
    ldx #0
    ldy screen_y
    jmp gotoxy
_maybe_10
    cmp #10
    bne _doout
    ldx screen_x
    ldy screen_y
    iny
    jmp gotoxy
_doout
    jmp dispchar

con_rx:
    lda kernel.args.events.pending
    bpl con_rx
    jsr kernel.NextEvent
    bcs con_rx
    lda event.type
    cmp #kernel.event.key.PRESSED
    bne con_rx                  ; only keys for now
    lda event.key.flags
    bmi con_rx                  ; Negative flags == non-ASCII
    lda event.key.ascii
    rts

raw_bs:
    lda #8
    rts

do_bs:
    dec screen_x
    bpl _do_erase
    inc screen_x
_do_erase:
    jsr gotoxy
    ldx screen_x
    ldy screen_y
    lda #32
    jsr dispchar
    dec screen_x
    ldx screen_x
    ldy screen_y
    jsr gotoxy
    rts
