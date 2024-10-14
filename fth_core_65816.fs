( -*- forth-asm -*- )

include word_hdr.fs

HEADLESSCODE

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
    .x16
    ldx rsp
    dex
    dex
    stx rsp
    rts

rpop1
    .x16
    ldx rsp
    inx
    inx
    stx rsp
    rts

rpush2
    .a16
    lda rsp
    sec
    sbc #4
    sta rsp
    rts

rpop2
    .a16
    lda rsp
    clc
    adc #4
    sta rsp
    rts

w_enter .block
cfa
    ;jsr    debug_dump
    .ax16
    jsr rpush1
    lda ip
    sta 1,x
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
    .byte $55
END-CODE

next_unlisted
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
    ldx rsp
    lda 1,x
    sta ip
    jsr rpop1
END-CODE

next_unlisted
CODE p0
    ldx #$1ff
    txs
END-CODE

CODE r0
    lda #rstk_top
    sta rsp
END-CODE

CODE emit
    pla
    jsr     con_tx
END-CODE

CODE at-xy
    pla
    tay
    pla
    tax
    jsr     gotoxy
END-CODE

CODE pagesize
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
    .a8
    pha
END-CODE

next_unlisted
CODE unloop
    jsr rpop2
END-CODE

CODE depth
    tsc
    eor #$1ff                   ; one's comp because zero stack == #$1ff
    lsr                         ; now div by two since cells are two bytes
    pha
END-CODE

CODE rdepth
    lda #rstk_top
    sec
    sbc rsp
    lsr
    pha
END-CODE

next_unlisted
CODE rbase
    lda #rstk_top
    dec a
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

CODE xor
    pla
    eor 1,s
    sta 1,s
END-CODE

CODE lshift
    ply
    pla
    cpy #0
    beq done
again:
    asl a
    dey
    bne again
done:
    pha
END-CODE

CODE rshift
    ply
    pla
    cpy #0
    beq done
again:
    lsr a
    dey
    bne again
done:
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

CODE 0<>
    pla
    beq _iszero
    lda #$ffff
    bra _done
_iszero:
    lda #0
_done:
    pha
END-CODE

CODE invert
    lda #$ffff
    eor 1,s
    sta 1,s
END-CODE

CODE abs
    lda 1,s
    bpl _done
    eor #$ffff
    inc a
    sta 1,s
_done:
END-CODE

CODE negate
    lda 1,s
    eor #$ffff
    inc a
    sta 1,s
END-CODE

CODE ALIGN
END-CODE

CODE ALIGNED
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
    bmi _set_false
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
    tsx
    inc 1,x
END-CODE

CODE 1-
    tsx
    dec 1,x
END-CODE

CODE *
    jmp ll_mult
END-CODE

CODE 2*
    tsx
    asl 1,x
END-CODE

CODE 2/
    tsx
    clc
    lda 1,x
    bpl _is_positive
    sec
_is_positive:
    ror 1,x
END-CODE

CODE /mod
    jmp ll_slash_mod
END-CODE

CODE 2@
    .x8
    pla
    sta mac
    ldy #2
    lda (mac),y
    pha
    lda (mac)
    pha
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
    and #$ff
    sta 1,s
END-CODE

CODE 2!
    .x8
    ldy #2
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

next_unlisted
CODE branch
    lda (ip)
    sta ip
END-CODE

next_unlisted
CODE qbranch
    pla
    bne _no_branch
    lda (ip)
    sta ip
    bra _done
_no_branch:
    inc ip
    inc ip
_done:
END-CODE

CODE bye
    jmp start
END-CODE

CODE >r
    jsr rpush1
    pla
    sta 1,x
END-CODE

CODE r>
    ldx rsp
    lda 1,x
    pha
    jsr rpop1
END-CODE

CODE r@
    ldx rsp
    lda 1,x
    pha
END-CODE

CODE 2>r
    jsr rpush2
    tax
    pla
    sta 1,x
    pla
    sta 3,x
END-CODE

CODE 2r>
    ldx rsp
    lda 3,x
    pha
    lda 1,x
    pha
    jsr rpop2
END-CODE

CODE 2r@
    ldx rsp
    lda 3,x
    pha
    lda 1,x
    pha
END-CODE

CODE 2rdrop
    jsr rpop2
END-CODE

CODE >cf
    ldx cfp
    dex
    dex
    stx cfp
    pla
    sta 1,x
END-CODE

CODE cf>
    ldx cfp
    lda 1,x
    pha
    inx
    inx
    stx cfp
END-CODE

CODE cf@
    ldx cfp
    lda 1,x
    pha
END-CODE

CODE cfnip
    ;; fixme: not yet tested
    ldx cfp
    lda 1,x
    sta 3,x
    inx
    inx
    stx cfp
END-CODE

CODE i
    ldx rsp
    lda 1,x
    pha
END-CODE

CODE j
    ldx rsp
    lda 5,x
    pha
END-CODE

CODE k
    ldx rsp
    lda 9,x
    pha
END-CODE

( full loop terminator include dropping loop counters from rstk )
next_unlisted
CODE do_loop
    ;; Increment loop counter
    ldx rsp
    inc 1,x

    ;; Compare counter with limit
    lda 3,x
    cmp 1,x
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
next_unlisted
CODE do_plus_loop
    ;; Increment loop counter
    ldx rsp
    clc
    pla
    adc 1,x
    sta 1,x

    ;; Compare counter with limit, (count - limit here though)
    lda 1,x
    cmp 3,x
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
next_unlisted
CODE do_loop1
    ldx rsp
    inc 1,x
    lda 3,x
    cmp 1,x
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

next_unlisted
CODE do_plus_loop1
    ;; Increment loop counter
    ldx rsp
    clc
    pla
    adc 1,x
    sta 1,x

    ;; Compare counter with limit, (count - limit here though)
    lda 1,x
    cmp 3,x
    bcc loop_again              ; If carry is clear, counter < limit

    ;jsr rpop2                  ; This is the only difference compared to do_plus_loop (currently)
    inc ip
    inc ip
    bra finished
loop_again:
    lda (ip)
    sta ip
finished:
END-CODE

next_unlisted
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

CODE cell+
    tsx
    inc 1,x
    inc 1,x
END-CODE

CODE cells
    lda 1,s
    asl
    sta 1,s
END-CODE

CODE char+
    tsx
    inc 1,x
END-CODE

CODE chars
    ;; nop
END-CODE

CODE halt
    brk
    .byte $66
END-CODE

CODE execute
    pla
    sta w
    jmp (w)
END-CODE

CODE move
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

( c-addr1 u1 c-addr2 u2 -- f )
CODE compare
    lda 5,s
    cmp 1,s                     ; U1 cmp U2
    bcc do_cmp0                 ; U1 is < U2
    lda 1,s
do_cmp0:                        ; A has count
    tax                         ; X has count
    ldy #0                      ; Y has string offset (for (d,s),y addr mode)
do_cmp1:
    lda (7,s),y
    cmp (5,s),y
    bne is_different
    iny
    dex
    bne do_cmp1
    ; We're same to here.  Check lengths
    lda 5,s
    cmp 1,s
    bcc return_less
    beq return_eq
    bra return_greater
is_different:
    bcc return_less
    bra return_greater
return_less:
    lda #-1
    bra done
return_eq:
    lda #0
    bra done
return_greater:
    lda #1
done:
    sta 7,s                     ; store comparison result in soon-to-be TOS
    clc
    tsc
    adc #6                      ; remove top three args
    tcs
END-CODE

next_unlisted
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

next_unlisted
CODE dec_num
    pla
    jsr prt_dec_num
END-CODE
