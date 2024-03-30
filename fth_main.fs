( -*- forth-asm -*- )

HEADLESSCODE

HIGH_W .macro name_len, name, act=w_enter, flgs=0, prev
dict:
    .byte (\flgs << 7) | len(\name)
    .text format("%-5s", \name[:5])
    .addr \prev
cfa:
    jmp \act                  ; CFA
    .endmacro                   ; PFA implicit, follows macro

CODE_W .macro name_len, name, flgs=0, prev
dict:
    .byte (\flgs << 7) | len(\name)
    .text format("%-5s", \name[:5])
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

132 CONSTANT tiblen

CODE p0
    ldx #$ff
    txs
END-CODE

CODE r0
    lda #STACK_MEM
    sta rsp
END-CODE

: . ( x -- )
    <# #s #> type
;

: <# ( -- )
    pic_num_size pic_num_off !
;

: hold ( n -- )
    -1 pic_num_off +!
    pic_num pic_num_off @ + c!
;

: # ( ud1 -- ud2 )
    base @ /mod swap ( -- ud2 rem )
    dup 10 < if
        0x30 + hold
    else
        55 + hold
    then
;

: #s ( ud1 -- ud2 )
    begin
        #
        dup 0=
    until
;

: #> ( xd -- c-addr u )
    drop
    pic_num pic_num_off @ +
    pic_num_size pic_num_off @ -
;

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

: type ( c-addr u -- )
    0 do
        dup i + c@ emit
    loop
    drop
;

: count ( c-addr1 -- c-addr2 u )
    dup 1+ swap c@
;

: cstr_cmp ( c-addr1 dict_start -- f )
    dup c@ 0x7f and rot dup c@ rot over = if
        ( Limit to 5 significant chars )
        5 min
        0 do
            1+ swap 1+
            ( case-insensitive search for ASCII only! )
            dup c@ 0xdf and rot dup c@ 0xdf and rot <> if
                2drop unloop 0 exit
            then
        loop
        2drop
        -1
    else
        2drop drop
        0
    then
;

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

: >=
  < invert
;

: <=
  > invert
;

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

: /
    /mod nip
;

: mod
    /mod drop
;

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

: do_bs ( a-end a-cur a-beg -- a-end a-cur 0 ; R: +n1 a-beg -- )
    over < if
        get_bs emit
        1- bl over c!
    then
    0 ( continue taking keys )
;

: do_insert ( a-end a-cur k -- a-end a-cur n 0|-1 )
    dup emit
    over c!
    2dup > if ( do we have room for more chars ? )
        1+ 0
    else
        -1 ( No more room; exit key taking loop )
    then
;

: accept ( a-beg +n1 -- +n2 )
    dup >r
    over dup >r + swap ( a-end a-beg -- )

    begin ( a-end a-cur -- a-end a-cur )
        key case
            10 of cr -1 endof
            13 of cr -1 endof
            8 of r@ do_bs endof
            127 of r@ do_bs endof
            20 of r@ do_bs endof
            do_insert 0
        endcase
    until

    -
    r> drop r> swap -
;

: is_delim ( c delim -- f )
    dup bl = if ( if delim is the bl, then match any space )
        over =
        over 10 = or
        over 13 = or
        over 0  = or ( F256k files are padded with NULs )
        swap  9 = or
    else
        =
    then
;

: append_counted ( c c-addr -- )
    dup >r ( c c-addr; r: c-addr -- )
    dup c@ + 1+ c!
    r> dup c@ 1+ swap c!
;

: word ( char "<chars>ccc<char>" -- c-addr )
    0 word_space c!

    begin
        source >in @ dup rot ( delim src-addr >in-v >in-v src-len -- )
        < >r                 ( delim src-addr >in-v -- ; r: chars-avail -- )
        + c@ over is_delim   ( delim c delim -- delim f )
        r> and
    while
            1 >in +!
    repeat

    ( delim -- )
    source >in @ <= if ( >in is maxed out; return )
      2drop word_space
      exit
    then
    drop

    ( delim -- )
    begin
        source >in @ dup rot
        < >r
        + c@ over is_delim invert
        r> and
    while
            source drop >in @ + c@ ( grab character again )
            word_space append_counted
            1 >in +!
    repeat
    1 >in +! ( Skip at least one closing delim )

    ( delim -- )
    drop
    word_space
;

VARIABLE tib 132 XALLOT
VARIABLE word_space 128 XALLOT
128 CONSTANT squote_size
VARIABLE squote_store 128 XALLOT

12 CONSTANT pic_num_size
VARIABLE pic_num_off
VARIABLE pic_num 12 XALLOT

VARIABLE base
VARIABLE >in
VARIABLE state
VARIABLE dict_start
32 CONSTANT bl

2VARIABLE ticksource

: source
    ticksource 2@
;

next_immediate
: s" ( -- c-addr u )
    34 word count ( get the next input until a quote char )
    state @ 0= if
        squote_size min dup >r
        squote_store swap move
        squote_store r>
    else
        ( Behavior is analogous to LITERAL )
        ['] branch , dup here + 1 cells + ,
        swap here 2 pick move here over allot
        ['] lit , ,
        ['] lit , ,
    then
;

next_immediate
: ."
    34 word count
    state @ 0= if
        type
    else
        ['] branch , dup here + 1 cells + ,
        swap here 2 pick move here over allot
        ['] lit , ,
        ['] lit , ,
        ['] type ,
    then
;

: cr ( -- )
    13 emit
    10 emit
;

: splat ( -- )
    42 emit
;

: find ( c-addr -- c-addr 0 | xt 1 | xt -1 )
    dict_start @

    begin
        ( c-addr dict_next -- )
        ?dup
    while ( c-addr dict_start -- )
            2dup >r >r ( c-addr dict_start -- ) ( R: dict_start c-addr -- )
            cstr_cmp if
                r> drop r> dup 8 + swap c@ 128 and if
                    1
                else
                    -1
                then
                exit
            then
            r> r> 6 + @
    repeat
    0
;

: '
    bl word find 0= if
        drop 0
    then
;

: check_sign ( c-addr n -- 1|-1 c-addr n )
    dup 1 > if
        over c@ case
            45 of -1 rot 1+ rot 1 - endof
            43 of  1 rot 1+ rot 1 - endof
            >r 1 rot rot r>
        endcase
    else
        1 rot rot
    then
;

: check_base ( c-addr n -- c-addr n -- )
    dup 2 > if
        over c@ 0x30 <> if
            exit
        then
        over 1+ c@ case
             88 of 16 base ! swap 2 + swap 2 - endof
            120 of 16 base ! swap 2 + swap 2 - endof
             66 of  2 base ! swap 2 + swap 2 - endof
             98 of  2 base ! swap 2 + swap 2 - endof
             79 of  8 base ! swap 2 + swap 2 - endof
            111 of  8 base ! swap 2 + swap 2 - endof
            >r swap 1 + swap 1 - r>
        endcase
    then
;

: c_to_i ( c -- n )
    dup 97 >= if
        87 -
        exit
    then

    dup 65 >= if
        55 -
        exit
    then

    48 -
    dup 0 < over 15 > or
    abort" WORD NOT FOUND"
;

: str_to_int ( c-addr n -- n )
    0 swap 0 do
        ( c-addr accum -- )
        base @ *
        over i + c@ c_to_i
        +
    loop
    swap drop
;

: chk_num_start ( c-addr -- )
    dup c@ 0= if
      drop exit
    then
    1+ c@
    dup 43 =
    over 45 = or
    swap dup 47 > swap 58 < and or
    0= abort" WORD NOT FOUND"
;

(
1. Check for sign
2. Check for leading zero
2.1. Check for 'x', 'b', or 'o'
2.2. Set base to to 10, 16, 2, or 8
3. Multiply accumulator by base then add current digit
4. Check for more digits, loop to 3
)
: try_number ( c-addr -- n )
    base @ >r

    dup chk_num_start

    count
    check_sign
    check_base
    str_to_int
    *

    r> base !
;

: space
    32 emit
;

: min
    2dup < if
      drop
    else
      nip
    then
;

: max
    2dup > if
      drop
    else
      nip
    then
;

: .s
    depth ?dup if
      dup ." <" . ." >"
      0 do
        space
        0x1fe i cells - @ .
      loop
    else
      ." <0> "
    then
    cr
;

: cstr_to_here ( c-str maxlen -- )
    here over bl fill
    dup >r
    over c@ 1+ min ( c-str min -- )
    here swap move
    r> allot
;

: create ( "word" -- here )
    here
    bl word 6 cstr_to_here
    dict_start @ ,
    0x4c c, lit var ,
    dict_start !
;

: :
    create
    lit enter here 2 - !
    1 state !
;

next_immediate
: ;
    lit exit ,
    0 state !
;

next_immediate
: [
    0 state !
;

: ]
    1 state !
;

next_immediate
: ['] ( "word" -- x-addr )
    bl word ( fixme; add error handling )
    find drop
;

: interpret ( -- )
    begin
        bl word
        dup count nip 0= if
            drop
            exit
        then
        find ?dup if
            1 = state @ 0= or if ( execute it immediately? )
                execute
            else
              ,
            then
        else ( try as number )
          try_number
          state @ if ( compile the literal into the definition )
            ['] lit , ,
          then
        then
        0
    until
;

: quit ( -- ) ( R: i*x -- )
    r0

    0 state !
    begin
        tib dup tiblen accept ticksource 2!
        0 >in !
        interpret
        state @ 0= if
            bl emit ." OK "
        then
        0
    until
;

: casetest
    3 0 do
        i case
            1 of 0x31 emit endof
            2 of 0x32 emit endof
            0x39 emit
        endcase
        bl emit
    loop
    cr
;

: abort
    p0 quit
;

: immediate
    dict_start @ dup c@ 0x80 or swap c!
;

next_immediate
: if ( C: -- dest )
    ['] qbranch ,
    here >cf 0xf0f1 ,
;

next_immediate
: else ( C: orig1 -- orig2 )
    cf>
    ['] branch ,
    here >cf 0xf0f2 ,
    here swap !
;

next_immediate
: then ( C: orig -- )
    here cf> !
;

next_immediate
: begin ( C: -- dest )
    here >cf
;

next_immediate
: until ( C: dest -- )
    ['] qbranch ,
    cf> ,
;

next_immediate
: while ( C: dest -- dest orig ) ( I think I implemented dest orig instead; fixme )
    ['] qbranch ,
    here >cf 0xf1f1 ,
;

next_immediate
: repeat ( C: dest orig -- )
    cf> cf>
    ['] branch , ,
    here swap !
;

next_immediate
: again ( C: dest -- )
    ['] branch ,
    cf> ,
;

next_immediate
: do ( C: -- do-sys )
    ['] 2>r ,
    here >cf
;

next_immediate
: leave
    ['] branch , 0xf2f1 , ( These 0xf2f1s will get replaced by resolve_leaves )
;

: resolve_leaves ( C: loop-orig here/unloop-dest -- )
    dup rot
    do
        i @ 0xf2f1 = if
            ['] branch i 2 - @ = if
                dup i !
            then
        then
    2 +loop
    drop
;

next_immediate
: loop ( C: do-sys -- )
    ['] do_loop1 , cf> dup , ( loop target was top-of-C: stack )
    here resolve_leaves
    ['] unloop ,
;

: hello
    s" HELLO, WORLD!"
    type
;

: abort_test
    1 abort" This should print then abort."
    s" This should not print." type
;

: disp_char ( c -- )
    dup 32 < over 126 > or if
        drop
        46 emit
    else
        emit
    then
;

: dump ( addr u -- )
    dup 0= if
        drop
        exit
    then
    base @ >r
    16 base !
    over + dup rot do
        i <# # # # # 32 hold #> type
        dup i 8 + min i do
            i c@ <# # # 32 hold #> type
        loop
        bl emit
        dup i 8 + min i do
            i c@ disp_char
        loop
        cr
        8
    +loop
    drop
    r> base !
;

: print_name ( name-addr -- )
    dup 1+ swap c@ 0x7f and dup 6 < if
        type
    else
        ( Name is longer than 5 chars; show length )
        <# 62 hold #s 60 hold #> type
        5 type
    then
;

: words ( -- )
    dict_start @

    begin
        ( dict_next -- )
        ?dup
    while ( dict_next -- )
        dup >r ( dict_start -- ) ( R: dict_start -- )
        print_name bl emit
        r> 6 + @
    repeat
;

: plt ( plus loop test )
    ." Testing +LOOP..." cr

    ." [1] 0-4 +2" cr
    4 0 do
        i . bl emit
        2
    +loop
    cr ." [1] Done." cr

    ." [2] 1-4 +2" cr
    4 1 do
        i . bl emit
        i 5 = if
            brk
        then
        2
    +loop
    cr ." [2] Done. " cr
;

: hex
    0x10 base !
;

: decimal
    0x0a base !
;

: octal
    0x08 base !
;

CODE hex_char
    jmp mach_hex_char
END-CODE

CODE dec_num
    pla
    jsr prt_dec_num
    pla
END-CODE

: char_filt ( c -- c )
    dup 32 < over 126 > or if
        drop 46
    then
;

: char_block
    16 0 do
        16 0 do
            i 0= if
                i j 2 + at-xy
                j hex_char emit
            then

            j 0= if
                i 2 + 0 at-xy
                i hex_char emit
            then
            i 2 + j 2 + at-xy
            i j 4 lshift + emit
        loop
    loop
;

: cold
    here0
    10 base !
    0 >in !
    init_serial
    core_dict @ dict_start !

    char_block 0 20 at-xy

    ." HOLDFORTH 0.1" cr
    quit
    depth
    bye
;

[DEFINED] ARCH_F256 [IF]

include f256_files.fs

[THEN]
