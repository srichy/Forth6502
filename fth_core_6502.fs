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
    .endmacro                   ; PFA implicit, follows macro

NEXT .macro
    jmp do_next
    .endmacro

w_core_dict    .HIGH_W 9, "core_dict", w_const, , "0"
    .addr dict_head

source_id_stk:   .fill 16      ;8 addresses
source_id_sp:    .word 0

    ;; Byte is in A.  Put at here and advance here
puthere:
    sta (here_store)
    inc here_store
    bne +
    inc here_store+1
+   rts

rpush1
    ldx rsp
    dex
    dex
    stx rsp
    rts

rpop1
    ldx rsp
    inx
    inx
    stx rsp
    rts

rpush2
    php
    lda rsp
    sec
    sbc #4
    sta rsp
    plp
    rts

rpop2
    lda rsp
    clc
    adc #4
    sta rsp
    rts

w_enter .block
cfa
    ;jsr    debug_dump
    jsr rpush1
    lda ip
    sta rstk+1,x
    lda ip+1
    sta rstk+2,x
    clc
    lda w
    adc #3
    sta ip
    lda w+1
    adc #0
    sta ip+1
    .NEXT
  .endblock

w_var .block
cfa
    lda w
    clc
    adc #3
    tax
    lda w+1
    adc #0
    pha
    txa
    pha
    .NEXT
  .endblock

w_const .block
cfa
    ldy #4
    lda (w),y
    pha
    dey
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
    ldy #1
    lda (ip),y
    pha
    lda (ip)
    pha
    clc
    lda ip
    adc #2
    sta ip
    lda ip+1
    adc #0
    sta ip+1
END-CODE

CODE exit
    ldx rsp
    lda rstk+1,x
    sta ip
    lda rstk+2,x
    sta ip+1
    jsr rpop1
END-CODE

CODE p0
    ldx #$ff
    txs
END-CODE

CODE r0
    lda #STACK_MEM
    sta rsp
END-CODE

CODE emit
    pla
    jsr     con_tx
    pla
END-CODE

CODE at-xy
    pla
    tay
    pla
    pla
    tax
    pla
    jsr     gotoxy
END-CODE

CODE pagesize
    pla
    tay
    pla
    pla
    tax
    pla
    jsr     pagesize
END-CODE

CODE page
    lda #255                    ; Hopefully worst-case terminal size
    jsr scroll_up
    ldx #0
    ldy #0
    jsr gotoxy
END-CODE

    ( c-addr u char -- )
CODE fill
    pla
    sta mac                     ; char
    pla
    pla
    tay                         ; low byte of count
    pla
    tax                         ; high byte of count
    pla
    sta mac+1                   ; base dest addr (l)
    pla
    sta mac+2                   ; base dest addr (h)
    txa
    clc
    adc mac+2                   ; pre-increment high order
    sta mac+2
    lda mac
-   sta (mac+1),y
    dey
    bpl -
    dec mac+2                   ; high order dec on low-order wrap
    dex
    bpl -
END-CODE

( -- c )
CODE key
    lda #0
    pha
    jsr con_rx
    pha
END-CODE

CODE unloop
    jsr rpop2
END-CODE

CODE depth
    tsx
    lda #0
    pha
    txa
    eor #$ff                    ; one's comp because zero stack == #$ff
    lsr                         ; now div by two since cells are two bytes
    pha
END-CODE

CODE dup
    pla
    tax
    pla
    tay
    pha
    txa
    pha
    tya
    pha
    txa
    pha
END-CODE

CODE ?dup
    tsx
    lda $101,x
    bne nonzero
    lda $102,x
    bne nonzero
    bra iszero
nonzero
    jmp w_dup.cfa
iszero
END-CODE

CODE drop
    pla
    pla
END-CODE

CODE swap
    pla
    sta mac
    pla
    sta mac+1
    pla
    tax
    pla
    tay
    lda mac+1
    pha
    lda mac
    pha
    tya
    pha
    txa
    pha
END-CODE

CODE over
    tsx
    lda $104,x
    pha
    lda $103,x
    pha
END-CODE

CODE nip
    tsx
    pla
    sta $103,x
    pla
    sta $104,x
END-CODE

CODE tuck
    pha
    pha
    tsx
    ldy #4
-
    lda $103,x
    sta $101,x
    inx
    dey
    bne -
    tsx                         ; Avoiding z-page crossing
    lda $101,x
    sta $105,x
    lda $102,x
    sta $106,x
END-CODE

CODE pick
    tsx
    txa
    asl $101,x
    clc
    adc $101,x
    tay
    lda $103,y
    sta $101,x
    lda $104,y
    sta $102,x
END-CODE

CODE 2drop
    pla
    pla
    pla
    pla
END-CODE

CODE 2dup
    pha
    pha
    pha
    pha
    tsx
    ldy #4
-   lda $105,x
    sta $101,x
    inx
    dey
    bne -
END-CODE

CODE 2over
    pha
    pha
    pha
    pha
    tsx
    ldy #4
-   lda $109,x
    sta $101,x
    inx
    dey
    bne -
END-CODE

CODE 2swap
    tsx
    ldy #4
-   lda $101,x
    sta mac
    lda $105,x
    sta $101,x
    lda mac
    sta $105,x
    inx
    dey
    bne -
END-CODE

CODE rot
    tsx
    lda $101,x
    sta mac
    lda $102,x
    sta mac+1
    lda $105,x
    sta $101,x
    lda $106,x
    sta $102,x
    lda $103,x
    sta $105,x
    lda $104,x
    sta $106,x
    lda mac+1
    sta $104,x
    lda mac
    sta $103,x
END-CODE

CODE and
    tsx
    lda $101,x
    and $103,x
    sta $103,x
    lda $102,x
    and $104,x
    sta $104,x
    pla
    pla
END-CODE

CODE or
    tsx
    lda $101,x
    ora $103,x
    sta $103,x
    lda $102,x
    ora $104,x
    sta $104,x
    pla
    pla
END-CODE

CODE lshift
    pla
    tay
    pla
    tsx
_again:
    asl $101,x
    rol $102,x
    dey
    bne _again
END-CODE

CODE rshift
    pla
    tay
    pla
    tsx
_again:
    lsr $102,x
    ror $101,x
    dey
    bne _again
END-CODE

CODE =
    tsx
    lda $101,x
    cmp $103,x
    bne notequal
    lda $102,x
    cmp $104,x
    bne notequal
    lda #$ff
    bra finished
notequal
    lda #00
finished
    sta $103,x
    sta $104,x
    pla
    pla
END-CODE

CODE <>
    tsx
    lda $101,x
    cmp $103,x
    bne notequal
    lda $102,x
    cmp $104,x
    bne notequal

    lda #00
    bra finished
notequal
    lda #$ff
finished
    sta $103,x
    sta $104,x
    pla
    pla
END-CODE

CODE 0=
    tsx
    lda $101,x
    bne nonzero
    lda $102,x
    bne nonzero
    lda #$ff
    bra finished
nonzero
    lda #0
finished
    sta $101,x
    sta $102,x
END-CODE

CODE invert
    tsx
    lda $101,x
    eor #$ff
    sta $101,x
    lda $102,x
    eor #$ff
    sta $102,x
END-CODE

CODE 0<
    tsx
    lda $102,x
    bmi set_true
    lda #0
    bra save_flag
set_true
    lda #$ff
save_flag
    sta $101,x
    sta $102,x
END-CODE

CODE 0>
    tsx
    lda $102,x
    bmi set_false
    bne set_true
    lda $101,x
    cmp #0
    bne set_true
set_false
    lda #0
    bra save_flag
set_true
    lda #$ff
save_flag
    sta $101,x
    sta $102,x
END-CODE

CODE <
    tsx
    sec
    lda $103,x
    sbc $101,x
    lda $104,x
    sbc $102,x
    bmi set_true
    lda #0
    bra set_flag
set_true
    lda #$ff
set_flag
    sta $103,x
    sta $104,x
    pla
    pla
END-CODE

CODE >
    tsx
    sec
    lda $101,x
    sbc $103,x
    lda $102,x
    sbc $104,x
    bmi set_true
    lda #0
    bra set_flag
set_true
    lda #$ff
set_flag
    sta $103,x
    sta $104,x
    pla
    pla
END-CODE

CODE +
    tsx
    clc
    lda $103,x
    adc $101,x
    sta $103,x
    lda $104,x
    adc $102,x
    sta $104,x
    pla
    pla
END-CODE

CODE -
    tsx
    sec
    lda $103,x
    sbc $101,x
    sta $103,x
    lda $104,x
    sbc $102,x
    sta $104,x
    pla
    pla
END-CODE

CODE 1+
    tsx
    inc $101,x
    bne +
    inc $102,x
+
END-CODE

CODE 1-
    tsx
    sec
    lda $101,x
    sbc #1
    sta $101,x
    lda $102,x
    sbc #0
    sta $102,x
END-CODE

CODE *
    jmp ll_mult
END-CODE

CODE /mod
    jmp ll_slash_mod
END-CODE

CODE 2@
    pla
    sta mac
    pla
    sta mac+1
    ldy #3
-   lda (mac),y
    pha
    dey
    bpl -
END-CODE

CODE @
    pla
    sta mac
    pla
    sta mac+1
    ldy #1
    lda (mac),y
    pha
    lda (mac)
    pha
END-CODE

CODE c@
    pla
    sta mac
    pla
    sta mac+1
    lda #0
    pha
    lda (mac)
    pha
END-CODE

CODE 2!
    pla
    sta mac
    pla
    sta mac+1
    ldy #0
-   pla
    sta (mac),y
    iny
    cpy #4
    bne -
END-CODE

CODE !
    pla
    sta mac
    pla
    sta mac+1
    pla
    sta (mac)
    pla
    ldy #1
    sta (mac),y
END-CODE

CODE +!
    pla
    sta mac
    pla
    sta mac+1
    ldy #1
    clc
    pla
    adc (mac)
    sta (mac)
    pla
    adc (mac),y
    sta (mac),y
END-CODE

CODE c!
    pla
    sta mac
    pla
    sta mac+1
    pla
    sta (mac)
    pla
END-CODE

CODE branch
    ldy #1
    lda (ip)
    tax
    lda (ip),y
    sta ip+1
    stx ip
END-CODE

CODE qbranch
    pla
    bne no_branch1
    pla
    bne no_branch2
    ldy #1
    lda (ip)
    tax
    lda (ip),y
    sta ip+1
    stx ip
    bra finished
no_branch1
    pla
no_branch2
    clc
    lda ip
    adc #2
    sta ip
    bcc finished
    inc ip+1
finished
END-CODE

CODE bye
    jmp start
END-CODE

CODE >r
    jsr rpush1
    pla
    sta rstk+1,x
    pla
    sta rstk+2,x
END-CODE

CODE r>
    ldx rsp
    lda rstk+2,x
    pha
    lda rstk+1,x
    pha
    jsr rpop1
END-CODE

CODE r@
    ldx rsp
    lda rstk+2,x
    pha
    lda rstk+1,x
    pha
END-CODE

CODE 2>r
    jsr rpush2
    tax
    ldy #4
again
    pla
    sta rstk+1,x
    inx
    dey
    bne again
END-CODE

CODE 2r>
    ldx rsp
    ldy #4
again:
    lda $104,x
    pha
    dex
    dey
    bne again
    jsr rpop2
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
    sta CORE_MEM_END+1,x
    pla
    sta CORE_MEM_END+2,x
END-CODE

CODE cf>
    ldx cfp
    lda CORE_MEM_END+2,x
    pha
    lda CORE_MEM_END+1,x
    pha
    inx
    inx
    stx cfp
END-CODE

CODE cf@
    ldx cfp
    lda CORE_MEM_END+2,x
    pha
    lda CORE_MEM_END+1,x
    pha
END-CODE

CODE cfnip
    ;; fixme: not yet tested
    ldx cfp
    lda CORE_MEM_END+2,x
    sta CORE_MEM_END+4,x
    lda CORE_MEM_END+1,x
    sta CORE_MEM_END+3,x
    inx
    inx
    stx cfp
END-CODE

CODE i
    ldx rsp
    lda rstk+2,x
    pha
    lda rstk+1,x
    pha
END-CODE

CODE j
    ldx rsp
    lda rstk+6,x
    pha
    lda rstk+5,x
    pha
END-CODE

CODE k
    ldx rsp
    lda rstk+10,x
    pha
    lda rstk+9,x
    pha
END-CODE

( full loop terminator include dropping loop counters from rstk )
CODE do_loop
    ;; Increment loop counter
    ldx rsp
    inc rstk+1,x
    bne +
    inc rstk+2,x

    ;; Compare counter with limit
+    sec
    lda rstk+3,x
    sbc rstk+1,x
    bne loop_again
    lda rstk+4,x
    sbc rstk+2,x
    bne loop_again

    ;; Loop finished. Remove loop context. Skip backpointer.
    jsr rpop2
    clc
    lda ip
    adc #2
    sta ip
    bcc +
    inc ip+1
+   bra finished
loop_again:
    ldy #1
    lda (ip)
    tax
    lda (ip),y
    sta ip+1
    stx ip
finished:
END-CODE

( full loop terminator include dropping loop counters from rstk )
CODE do_plus_loop
    ;; Increment loop counter
    ldx rsp
    clc
    pla
    adc rstk+1,x
    sta rstk+1,x
    pla
    adc rstk+2,x
    sta rstk+2,x

    ;; Compare counter with limit, (count - limit here though)
    sec
    lda rstk+1,x
    sbc rstk+3,x
    lda rstk+2,x
    sbc rstk+4,x
    bcc loop_again              ; If carry is clear, counter < limit

    jsr rpop2
    clc
    lda ip
    adc #2
    sta ip
    bcc +
    inc ip+1
+   bra finished
loop_again:
    ldy #1
    lda (ip)
    tax
    lda (ip),y
    sta ip+1
    stx ip
finished:
END-CODE

( loop terminator that does not clear the rstk )
CODE do_loop1
    ldx rsp
    inc rstk+1,x
    bne +
    inc rstk+2,x
+   sec
    lda rstk+3,x
    sbc rstk+1,x
    bne loop_again
    lda rstk+4,x
    sbc rstk+2,x
    bne loop_again
    ; jsr rpop2                 ; This is the only difference compared to do_loop (currently)
    clc
    lda ip
    adc #2
    sta ip
    bcc +
    inc ip+1
+   bra finished
loop_again:
    ldy #1
    lda (ip)
    tax
    lda (ip),y
    sta ip+1
    stx ip
finished:   
END-CODE

CODE here0
    lda edata
    sta here_store
    lda edata+1
    sta here_store+1
END-CODE

CODE here
    lda here_store+1
    pha
    lda here_store
    pha
END-CODE

CODE ,
    pla
    jsr puthere
    pla
    jsr puthere
END-CODE

CODE c,
    pla
    jsr puthere
    pla
END-CODE

CODE allot
    clc
    pla
    adc here_store
    sta here_store
    pla
    adc here_store+1
    sta here_store+1
END-CODE

CODE cells
    tsx
    lda $101,x
    asl
    sta $101,x
    lda $102,x
    rol
    sta $102,x
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
    pla
    sta w+1
    jmp wjmp
END-CODE

CODE move
    pla
    sta mac                     ; u
    pla
    sta mac+1
    pla
    sta mac+2                   ; a2, dest
    pla
    sta mac+3
    pla
    sta mac+4                   ; a1, source
    pla
    sta mac+5
-   sec
    lda mac
    sbc #1
    sta mac
    lda mac+1
    sbc #0
    sta mac+1
    bmi finished
    lda (mac+4)
    sta (mac+2)
    inc mac+4
    bne incdest
    inc mac+5
incdest
    inc mac+2
    bne -
    inc mac+3
    bra -
finished
END-CODE

CODE get_bs
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
    pla
END-CODE

8 CONSTANT HDR_SIZE
7 CONSTANT MAX_NM_LEN

: dict_to_cfa ( entry-addr -- cfa-addr )
    HDR_SIZE + 1 cells +
;
