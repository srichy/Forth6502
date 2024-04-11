( -*- forth-asm -*- )

HEADLESSCODE

HIGH_W .macro name_len, name, act=w_enter, flgs=0, prev
dict:
    .byte (\flgs << 7) | len(\name)
    .text format("%-7s", \name[:7])
    .addr \prev
cfa:
    jmp \act                  ; CFA
    .endmacro                   ; PFA implicit, follows macro

CODE_W .macro name_len, name, flgs=0, prev
dict:
    .byte (\flgs << 7) | len(\name)
    .text format("%-7s", \name[:7])
    .addr \prev
cfa:
    .al
    .endmacro                   ; PFA implicit, follows macro

NEXT .macro
    jmp do_next
    .endmacro

w_core_dict    .HIGH_W 9, "core_dict", w_const, , "0"
    .addr dict_head

source_id_stk:   .fill 16      ;8 addresses
source_id_sp:    .word 0

    ;; Byte is in A.  Put at here and advance here
puthere8:
    .a8
    sta (here_store)
    .a16
    inc here_store
    rts

puthere16:
    .a16
    sta (here_store)
    inc here_store
    inc here_store
    rts

rpush1
    .x8
    ldx rsp
    dex
    dex
    stx rsp
    rts

rpop1
    .x8
    ldx rsp
    inx
    inx
    stx rsp
    rts

rpush2
    php
    .a8
    lda rsp
    sec
    sbc #4
    sta rsp
    plp
    rts

rpop2
    .a8
    lda rsp
    clc
    adc #4
    sta rsp
    rts

w_enter .block
cfa
    ;jsr    debug_dump
    jsr rpush1
    .a16
    lda ip
    sta rstk+1,x
    clc
    lda w
    adc #3
    sta ip
    .NEXT
  .endblock

w_var .block
cfa
    .a16
    lda w
    clc
    adc #3
    pha
    .NEXT
  .endblock

w_const .block
cfa
    .a16
    .x8
    ldy #3
    lda (w),y
    pha
    .NEXT
  .endblock

END-CODE

next_immediate
CODE brk
    brk
END-CODE

CODE init_serial
    jsr con_init
END-CODE

    ( Low-level Forth engine support )
CODE lit
    lda (ip)
    pha
    clc
    lda ip
    adc #2
    sta ip
END-CODE

CODE exit
    .x8
    ldx rsp
    lda rstk+1,x
    sta ip
    jsr rpop1
END-CODE

CODE p0
    .x16
    ldx #$1ff
    txs
END-CODE

CODE r0
    .a8
    lda #STACK_MEM
    sta rsp
END-CODE

CODE emit
    pla
    jsr     con_tx
END-CODE

CODE at-xy
    .ax16
    pla
    tay
    pla
    tax
    jsr     gotoxy
END-CODE

CODE pagesize
    .ax16
    pla
    tay
    pla
    tax
    jsr     pagesize
END-CODE

CODE page
    .ax8
    lda #255                    ; Hopefully worst-case terminal size
    jsr scroll_up
    ldx #0
    ldy #0
    jsr gotoxy
END-CODE

    ( c-addr u char -- )
CODE fill
    .ax16
    plx                         ; char
    ply                         ; count
    pla                         ; base addr
    sta mac
    txa
    .a8
_again:
    dey
    bmi _done
    sta (mac),y
    bra _again
_done:
END-CODE

( -- c )
CODE key
    .a8
    lda #0
    pha
    jsr con_rx
    pha
END-CODE

CODE unloop
    jsr rpop2
END-CODE

CODE depth
    .ax8                        ; fixme
    tsx
    lda #0
    pha
    txa
    eor #$ff                    ; one's comp because zero stack == #$ff
    lsr                         ; now div by two since cells are two bytes
    pha
END-CODE

CODE dup
    lda 1,s
    pha
END-CODE

CODE ?dup
    lda 1,s
    beq _done
    pha
_done:
END-CODE

CODE drop
    pla
END-CODE

CODE swap
    .ax16
    lda 1,s
    tax
    lda 3,s
    sta 1,s
    txa
    sta 3,s
END-CODE

CODE over
    lda 3,s
    pha
END-CODE

CODE nip
    pla
    sta 1,s
END-CODE

CODE tuck
    lda 1,s
    tax
    lda 3,s
    sta 1,s
    txa
    sta 3,s
    pha
END-CODE

CODE pick
    .ax16
    tsx
    txa
    asl 1,x
    clc
    adc 1,x
    tay
    lda 3,y
    sta 1,x
END-CODE

CODE 2drop
    pla
    pla
END-CODE

CODE 2dup
    lda 3,s
    pha
    lda 3,s
    pha
END-CODE

CODE 2over
    lda 7,s
    pha
    lda 7,s
    pha
END-CODE

CODE 2swap
    .ax16
    lda 1,s
    tax
    lda 5,s
    sta 1,s
    txa
    sta 5,s
    lda 3,s
    tax
    lda 7,s
    sta 3,s
    txa
    sta 7,s
END-CODE

CODE rot
    .ax16
    lda 5,s
    tax
    lda 3,s
    sta 5,s
    lda 1,s
    sta 3,s
    txa
    sta 1,s
END-CODE

CODE and
    pla
    and 1,s
    sta 1,s
END-CODE

CODE or
    pla
    ora 1,s
    sta 1,s
END-CODE

CODE lshift
    .ax16
    ply
    pla
_again:
    asl a
    dey
    bne _again
    pha
END-CODE

CODE rshift
    .ax16
    ply
    pla
_again:
    lsr a
    dey
    bne _again
    pha
END-CODE

CODE =
    pla
    cmp 1,s
    bne _notequal
    lda #$ffff
    bra _finished
_notequal:
    lda #0
_finished:
    sta 1,s
END-CODE

CODE <>
    pla
    cmp 1,s
    bne _notequal
    lda #0
    bra _finished
_notequal:
    lda #$ffff
_finished:
    sta 1,s
END-CODE

CODE 0=
    pla
    beq _iszero
    lda #0
    bra _done
_iszero:
    lda #$ffff
_done:
    pha
END-CODE

CODE invert
    lda #$ffff
    eor 1,s
END-CODE

CODE 0<
    pla
    bmi _set_true
    lda #0
    bra _save_flag
_set_true:
    lda #$ffff
_save_flag:
    pha
END-CODE

CODE 0>
    pla
    bmi _set_false
    beq _set_false
    lda #$ffff
    bra _save_flag
_set_false:
    lda #0
_save_flag:
    pha
END-CODE

CODE <
    pla
    cmp 1,s
    beq _set_false
    bcc _set_false
    lda #$ffff
    bra _set_flag
_set_false:
    lda #0
_set_flag:
    sta 1,s
END-CODE

CODE >
    pla
    cmp 1,s
    bcc _set_true
    lda #0
    bra _set_flag
_set_true:
    lda #$ffff
_set_flag:
    sta 1,s
END-CODE

CODE +
    pla
    clc
    adc 1,s
    sta 1,s
END-CODE

CODE -
    lda 3,s
    sec
    sbc 1,s
    sta 3,s
    pla
END-CODE

CODE 1+
    .x16
    tsx
    inc 1,x
END-CODE

CODE 1-
    .x16
    tsx
    dec 1,x
END-CODE

CODE *
    jmp ll_mult
END-CODE

CODE /mod
    jmp ll_slash_mod
END-CODE

CODE 2@
    .x8
    pla
    sta mac
    ldy #1
-   lda (mac),y
    pha
    dey
    bpl -
END-CODE

CODE @
    .x8
    ldy #0
    lda (1,s),y
    sta 1,s
END-CODE

CODE c@
    .ax8
    ldy #0
    lda (1,s),y
    .a16
    and #$00ff
    sta 1,s
END-CODE

CODE 2!
    .x8
    ldy #1
    pla
    sta mac
    pla
    sta (mac)
    pla
    sta (mac),y
END-CODE

CODE !
    pla
    sta mac
    pla
    sta (mac)
END-CODE

CODE +!
    pla
    sta mac
    clc
    pla
    adc (mac)
    sta (mac)
END-CODE

CODE c!
    pla
    sta mac
    pla
    .a8
    sta (mac)
END-CODE

CODE branch
    lda (ip)
    sta ip
END-CODE

CODE qbranch
    pla
    bne _no_branch
    lda (ip)
    sta ip
    bra _done
_no_branch:
    clc
    lda ip
    adc #2
    sta ip
_done:
END-CODE

CODE bye
    jmp start
END-CODE

CODE >r
    jsr rpush1
    pla
    sta rstk+1,x
END-CODE

CODE r>
    .x8
    ldx rsp
    lda rstk+1,x
    pha
    jsr rpop1
END-CODE

CODE r@
    .x8
    ldx rsp
    lda rstk+1,x
    pha
END-CODE

CODE 2>r
    jsr rpush2
    tax
    .x8
    pla
    sta rstk+1,x
    pla
    sta rstk+3,x
END-CODE

CODE 2r>
    .x8
    ldx rsp
    lda rstk+3,x
    pha
    lda rstk+1,x
    pha
    jsr rpop2
END-CODE

CODE 2rdrop
    jsr rpop2
END-CODE

CODE >cf
    .x8
    ldx cfp
    dex
    dex
    stx cfp
    pla
    sta CORE_MEM_END+1,x
END-CODE

CODE cf>
    .x8
    ldx cfp
    lda CORE_MEM_END+1,x
    pha
    inx
    inx
    stx cfp
END-CODE

CODE cf@
    .x8
    ldx cfp
    lda CORE_MEM_END+1,x
    pha
END-CODE

CODE cfnip
    ;; fixme: not yet tested
    .x8
    ldx cfp
    lda CORE_MEM_END+1,x
    sta CORE_MEM_END+3,x
    inx
    inx
    stx cfp
END-CODE

CODE i
    .x8
    ldx rsp
    lda rstk+1,x
    pha
END-CODE

CODE j
    .x8
    ldx rsp
    lda rstk+5,x
    pha
END-CODE

CODE k
    .x8
    ldx rsp
    lda rstk+9,x
    pha
END-CODE

( full loop terminator include dropping loop counters from rstk )
CODE do_loop
    ;; Increment loop counter
    .x8
    ldx rsp
    inc rstk+1,x

    ;; Compare counter with limit
    lda rstk+3,x
    cmp rstk+1,x
    bne _loop_again

    ;; Loop finished. Remove loop context. Skip backpointer.
    jsr rpop2
    inc ip
    inc ip
    bra _finished
_loop_again:
    lda (ip)
    sta ip
_finished:
END-CODE

( full loop terminator include dropping loop counters from rstk )
CODE do_plus_loop
    ;; Increment loop counter
    .x8
    ldx rsp
    clc
    pla
    adc rstk+1,x
    sta rstk+1,x

    ;; Compare counter with limit, (count - limit here though)
    lda rstk+1,x
    cmp rstk+3,x
    bcc loop_again              ; If carry is clear, counter < limit

    jsr rpop2
    inc ip
    inc ip
    bra finished
loop_again:
    lda (ip)
    sta ip
finished:
END-CODE

( loop terminator that does not clear the rstk )
CODE do_loop1
    .x8
    ldx rsp
    inc rstk+1,x
    lda rstk+3,x
    cmp rstk+1,x
    bne loop_again
    ; jsr rpop2                 ; This is the only difference compared to do_loop (currently)
    inc ip
    inc ip
    bra finished
loop_again:
    lda (ip)
    sta ip
finished:   
END-CODE

CODE here0
    lda edata
    sta here_store
END-CODE

CODE here
    lda here_store
    pha
END-CODE

CODE ,
    pla
    jsr puthere16
END-CODE

CODE c,
    pla
    jsr puthere8
END-CODE

CODE allot
    clc
    pla
    adc here_store
    sta here_store
END-CODE

CODE cells
    lda 1,s
    asl
    sta 1,s
END-CODE

CODE chars
    ;; nop
END-CODE

CODE halt
    brk
END-CODE

CODE execute
    pla
    sta w
    jmp wjmp
END-CODE

CODE move
    .x16
    pla
    dec a                       ; mvn, mvp need count-1
    sta mac
    pla
    cmp 1,s
    beq _done
    bcs _move_positive
_move_negative:
    ;; uses beginning addresses
    tay
    plx
    lda mac
    mvn 0,0
    bra _done
_move_positive:
    clc
    adc mac
    tay
    pla
    clc
    adc mac
    tax
    lda mac
    mvp 0,0
_done:
END-CODE

CODE get_bs
    .a8
    lda #0
    pha
    jsr raw_bs
    pha
END-CODE

CODE hex_char
    jmp mach_hex_char
END-CODE

CODE dec_num
    pla
    jsr prt_dec_num
END-CODE

8 CONSTANT HDR_SIZE
7 CONSTANT MAX_NM_LEN

: dict_to_cfa ( entry-addr -- cfa-addr )
    HDR_SIZE + 1 cells +
;
