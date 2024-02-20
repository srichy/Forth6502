( -*- forth-asm -*- )

HEADLESSCODE

    previous_entry := 0


link_entry .macro new_entry
    .addr previous_entry
    previous_entry := \new_entry
    .endmacro

HIGH_W .macro name_len, name, act=w_enter, flgs=0
dict:
    .byte (\flgs << 7) | len(\name)
    .text format("%-5s", \name[:5])
    .link_entry dict
cfa:
    jmp \act                  ; CFA
    .endmacro                   ; PFA implicit, follows macro

CODE_W .macro name_len, name, flgs=0
dict:
    .byte (\flgs << 7) | len(\name)
    .text format("%-5s", \name[:5])
    .link_entry dict
cfa:
    .endmacro                   ; PFA implicit, follows macro

NEXT .macro
    jmp do_next
    .endmacro

banner_str: .text "Holdfire 0.0"
banner_len: .word (* - banner_str)

w_tib    .HIGH_W 3, "tib", w_var
    .fill 132

w_pic_num    .HIGH_W 7, "pic_num", w_var
    .fill 12

w_word_space    .HIGH_W 10, "word_space", w_var
    .fill 80

w_core_dict    .HIGH_W 9, "core_dict", w_const
    .addr dict_head

here_store
    .addr   0

push1
    ldx psp
    dex
    dex
    stx psp
    rts

pop1
    ldx psp
    inx
    inx
    stx psp
    rts

push2
    php
    lda psp
    sec
    sbc #4
    sta psp
    plp
    rts

pop2
    lda psp
    clc
    adc #4
    sta psp
    rts

w_enter .block
cfa
    ;jsr    debug_dump
    lda ip+1
    pha
    lda ip
    pha
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
    jsr push1
    lda w
    clc
    adc #3
    sta pstk+1,x
    lda w+1
    adc #0
    sta pstk+2,x
    .NEXT
  .endblock

w_const .block
cfa
    jsr push1
    ldy #3
    lda (w),y
    sta pstk+1,x
    iny
    lda (w),y
    sta pstk+2,x
    .NEXT
  .endblock

END-CODE

( Low-level Forth engine support )
CODE lit
    jsr push1
    ldy #0
    lda (ip),y
    sta pstk+1,x
    iny
    lda (ip),y
    sta pstk+2,x
    clc
    lda ip
    adc #2
    sta ip
    bcc +
    inc ip+1
+
END-CODE

CODE banner
    jsr push2
    tax
    lda #<banner_str
    sta pstk+3,x
    lda #>banner_str
    sta pstk+4,x
    lda #<banner_len
    sta pstk+1,x
    lda #>banner_len
    sta pstk+2,x
END-CODE

CODE exit
    pla
    sta ip
    pla
    sta ip+1
END-CODE

132 CONSTANT tiblen

CODE p0
    lda #(PSTACK_SIZE << 1) - 1
    sta psp
END-CODE

CODE r0
    ldx #$ff
    txs
END-CODE

( Brute-force until pictured numeric support added )
: . ( x -- )
    pic_num_size pic_num_off !
    begin
      -1 pic_num_off +!
      dup 10 / swap 10 mod   ( x r -- )
      0x30 + pic_num pic_num_off @ + c!
      dup 0=
    until
    drop
    pic_num pic_num_off @ +
    pic_num_size pic_num_off @ -
    type
;

CODE emit
    ldx psp
    lda pstk+1,x
    jsr     usb_tx
    jsr pop1
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

: cstr_cmp ( c-addr1 c-addr2 -- f )
    dup c@ rot dup c@ rot over = if
        0 do
            1+ swap 1+
            dup c@ rot dup c@ rot <> if
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
    jsr push1
    stz pstk+2,x
    jsr usb_rx
    sta pstk+1,x
END-CODE

CODE unloop
    pla
    pla
    pla
    pla
END-CODE

CODE depth
    sec
    lda #(PSTACK_SIZE << 1) - 1
    sbc psp
    jsr push1
    stz pstk+2,x
    sta pstk+1,x
END-CODE

CODE pstackptr
    clc
    lda #pstk
    adc psp
    jsr push1
    sta pstk+1,x
    adc #0
    sta pstk+2,x
END-CODE

CODE dup
    jsr push1
    lda pstk+3,x
    sta pstk+1,x
    lda pstk+4,x
    sta pstk+2,x
END-CODE

CODE ?dup
    ldx psp
    lda pstk+1,x
    bne nonzero
    lda pstk+2,x
    beq iszero
nonzero
    jsr push1
    lda pstk+4,x
    sta pstk+2,x
    lda pstk+3,x
    sta pstk+1,x
iszero
END-CODE

CODE drop
    inc psp
    inc psp
END-CODE

CODE swap
    ldx psp
    ldy pstk+1,x
    lda pstk+3,x
    sty pstk+3,x
    sta pstk+1,x
    ldy pstk+2,x
    lda pstk+4,x
    sty pstk+4,x
    sta pstk+2,x
END-CODE

CODE over
    jsr push1
    lda pstk+5,x
    sta pstk+1,x
    lda pstk+6,x
    sta pstk+2,x
END-CODE

CODE nip
    ldx psp
    lda pstk+1,x
    sta pstk+3,x
    lda pstk+2,x
    sta pstk+4,x
    jsr pop1
END-CODE

CODE tuck
    jsr push1
    ldy #4
-
    lda pstk+3,x
    sta pstk+1,x
    inx
    dey
    bne -
    ldx psp
    lda pstk+1,x
    sta pstk+5,x
    lda pstk+2,x
    sta pstk+6,x
END-CODE

CODE pick
    ldy psp
    ldx psp
    lda pstk+1,x
    asl
    clc
    adc psp
    tax
    lda pstk+1,x
    pha
    lda pstk+2,x
    tax
    stx pstk+2,y
    plx
    stx pstk+1,y
END-CODE

CODE 2drop
    jsr pop2
END-CODE

CODE 2dup
    jsr push2
    ldy #4
    ldx psp
-   lda pstk+5,x
    sta pstk+1,x
    inx
    dey
    bne -
END-CODE

CODE 2over
    jsr push2
    ldy #4
    ldx psp
-   lda pstk+9,x
    sta pstk+1,x
    inx
    dey
    bne -
END-CODE

CODE 2swap
    ldx psp
    ldy #4
-   lda pstk+1,x
    pha
    lda pstk+5,x
    sta pstk+1,x
    pla
    sta pstk+5,x
    inx
    dey
    bne -
END-CODE

CODE rot
    ldx psp
    lda pstk+1,x
    pha
    lda pstk+2,x
    pha
    lda pstk+5,x
    sta pstk+1,x
    lda pstk+6,x
    sta pstk+2,x
    lda pstk+3,x
    sta pstk+5,x
    lda pstk+4,x
    sta pstk+6,x
    pla
    sta pstk+4,x
    pla
    sta pstk+3,x
END-CODE

CODE and
    ldx psp
    ldy #2
-   lda pstk+1,x
    and pstk+3,x
    sta pstk+3,x
    inx
    dey
    bne -
    jsr pop1
END-CODE

CODE or
    ldx psp
    ldy #2
-   lda pstk+1,x
    and pstk+3,x
    sta pstk+3,x
    inx
    dey
    bne -
    jsr pop1
END-CODE

CODE =
    ldx psp
    lda pstk+1,x
    cmp pstk+3,x
    bne notequal
    lda pstk+2,x
    cmp pstk+4,x
    bne notequal

    lda #$ff
    bra finished
notequal
    lda #00
finished
    sta pstk+3,x
    sta pstk+4,x
    jsr pop1
END-CODE

CODE <>
    ldx psp
    lda pstk+1,x
    cmp pstk+3,x
    bne notequal
    lda pstk+2,x
    cmp pstk+4,x
    bne notequal

    lda #00
    bra finished
notequal
    lda #$ff
finished
    sta pstk+3,x
    sta pstk+4,x
    jsr pop1
END-CODE

CODE 0=
    ldx psp
    lda pstk+1,x
    bne nonzero
    lda pstk+2,x
    bne nonzero
    lda #$ff
    bra finished
nonzero
    lda #0
finished
    sta pstk+1,x
    sta pstk+2,x
END-CODE

CODE invert
    ldx psp
    lda pstk+1,x
    eor #$ff
    sta pstk+1,x
    lda pstk+2,x
    eor #$ff
    sta pstk+2,x
END-CODE

CODE 0<
    ldx psp
    lda pstk+2,x
    bmi set_true
set_false
    lda #0
    bra save_flag
set_true
    lda #$ff
save_flag
    sta pstk+1,x
    sta pstk+2,x
END-CODE

CODE 0>
    ldx psp
    lda pstk+2,x
    bmi set_false
    bne set_true
    lda pstk+1,x
    cmp #0
    bne set_true
set_false
    lda #0
    bra save_flag
set_true
    lda #$ff
save_flag
    sta pstk+1,x
    sta pstk+2,x
END-CODE

CODE <
    ldx psp
    sec
    lda pstk+3,x
    sbc pstk+1,x
    lda pstk+4,x
    sbc pstk+2,x
    bmi set_true
    lda #0
    bra set_flag
set_true
    lda #$ff
set_flag
    sta pstk+3,x
    sta pstk+4,x
    jsr pop1
END-CODE

CODE >
    ldx psp
    sec
    lda pstk+1,x
    sbc pstk+3,x
    lda pstk+2,x
    sbc pstk+4,x
    bmi set_true
    lda #0
    bra set_flag
set_true
    lda #$ff
set_flag
    sta pstk+3,x
    sta pstk+4,x
    jsr pop1
END-CODE

: >=
  < invert
;

: <=
  > invert
;

CODE +
    ldx psp
    clc
    lda pstk+3,x
    adc pstk+1,x
    sta pstk+3,x
    lda pstk+4,x
    adc pstk+2,x
    sta pstk+4,x
    jsr pop1
END-CODE

CODE -
    ldx psp
    sec
    lda pstk+3,x
    sbc pstk+1,x
    sta pstk+3,x
    lda pstk+4,x
    sbc pstk+2,x
    sta pstk+4,x
    jsr pop1
END-CODE

CODE 1+
    ldx psp
    clc
    inc pstk+1,x
    bcc +
    inc pstk+2,x
+
END-CODE

CODE 1-
    ldx psp
    sec
    lda pstk+1,x
    sbc #1
    sta pstk+1,x
    lda pstk+2,x
    sbc #0
    sta pstk+2,x
END-CODE

CODE *
    ldx psp
    lda pstk+3,x
    stz pstk+3
    sta mac
    lda pstk+4,x
    stz pstk+4
    sta mac+1
    ldy #16
again
    ;; Shift multiplier 1 bit right and check for carry
    lda pstk+2,x
    lsr
    sta pstk+2,x
    lda pstk+1,x
    ror
    sta pstk+1,x
    bcc skip_add
    ;; Carry set. Add multiplicant to accumulator
    clc
    lda mac
    adc pstk+3,x
    sta pstk+3,x
    lda mac+1
    adc pstk+4,x
    sta pstk+4,x
skip_add
    lda mac
    asl
    sta mac
    lda mac+1
    rol
    sta mac+1
    dey
    bne again
    jsr pop1
END-CODE

CODE /
    ;; fixme
END-CODE

CODE mod
    ;; fixme
END-CODE

CODE /mod
    ;; fixme
END-CODE

CODE 2@
    ldx psp
    lda pstk+1,x
    sta mac
    lda pstk+2,x
    sta mac+1
    jsr push1
    ldy #3
    lda (mac),y
    sta pstk+4,x
    dey
    lda (mac),y
    sta pstk+3,x
    dey
    lda (mac),y
    sta pstk+2,x
    dey
    lda (mac),y
    sta pstk+1,x
END-CODE

CODE @
    ldx psp
    lda pstk+1,x
    sta mac
    lda pstk+2,x
    sta mac+1
    ldy #1
    lda (mac)
    sta pstk+1,x
    lda (mac),y
    sta pstk+2,x
END-CODE

CODE c@
    ldx psp
    lda (pstk+1,x)
    sta pstk+1,x
    stz pstk+2,x
END-CODE

CODE 2!
    ;; fixme
END-CODE

CODE !
    ldx psp
    ;; fixme
END-CODE

CODE +!
    ;; fixme
END-CODE

CODE c!
    ldx psp
    lda pstk+1,x
    sta (pstk+3,x)
    jsr pop2
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
    ldx psp
    lda pstk+1,x
    bne no_branch
    lda pstk+2,x
    bne no_branch
    ldy #1
    lda (ip)
    sta mac
    lda (ip),y
    sta mac+1
    lda mac
    sta ip
    lda mac+1
    sta ip+1
    bra finished
no_branch
    clc
    lda ip
    adc #2
    sta ip
    bcc finished
    inc ip+1
finished
    jsr pop1
END-CODE

CODE bye
    jmp start
END-CODE

CODE >r
    ldx psp
    lda pstk+2,x
    pha
    lda pstk+1,x
    pha
    jsr pop1
END-CODE

CODE r>
    jsr push1
    pla
    sta pstk+1,x
    pla
    sta pstk+2,x
END-CODE

CODE r@
    jsr push1
    pla
    sta pstk+1,x
    pla
    sta pstk+2,x
    pha
    lda pstk+1,x
    pha
END-CODE

CODE 2>r
    jsr pop2
    tax
    ldy #4
again
    lda pstk,x
    pha
    dex
    dey
    bne again
END-CODE

CODE 2r>
    jsr push2
    tax
again
    pla
    sta pstk+1,x
    inx
    dey
    bne again
END-CODE

CODE 2rdrop
    pla
    pla
    pla
    pla
END-CODE

CODE >cf
    ;; fixme
END-CODE

CODE cf>
    ;; fixme
END-CODE

CODE cf@
    ;; fixme
END-CODE

CODE cfnip
    ;; fixme
END-CODE

CODE cftuck
    ;; fixme
END-CODE

CODE i
    jsr push1
    pla
    sta pstk+1,x
    pla
    sta pstk+2,x
    pha
    lda pstk+1,x
    pha
END-CODE

CODE j
    ;; fixme
    movl TOS, -(PSP)
    movl 8(RSP), TOS
END-CODE

CODE k
    ;; fixme
    movl TOS, -(PSP)
    movl 16(RSP), TOS
END-CODE

CODE do_loop
    ;; fixme
    movl  (IP)+, TA0       | TA0 holds addr of loop head
    movl 4(RSP), %d0
    movl  (RSP), %d1
    addq.l #1, %d1
    movl  %d1, (RSP)       | Save incremented loop counter
    cmpl   %d0, %d1
    bne  1f                | Loop still active
    addql  #8, RSP
    bra  2f
1:  movl TA0, IP
2:
END-CODE

CODE do_loop1
    ;; fixme
    movl  (IP)+, TA0       | TA0 holds addr of loop head
    movl 4(RSP), %d0
    movl  (RSP), %d1
    addq.l #1, %d1
    movl  %d1, (RSP)       | Save incremented loop counter
    cmpl   %d0, %d1
    beq  1f                | Loop still active
    movl TA0, IP
1:
END-CODE

CODE here0
    ;; fixme
    lea _edata, TA0
    movl TA0, here_store
END-CODE

CODE here
    ;; fixme
    movl TOS, -(PSP)
    movl (here_store), TOS
END-CODE

CODE ,
    ;; fixme
    movl here_store, TA0
    movl TOS, (TA0)+
    movl TA0, here_store
    movl (PSP)+, TOS
END-CODE

CODE c,
    ;; fixme
    movl here_store, TA0
    movb TOS, (TA0)+
    movl TA0, here_store
    movl (PSP)+, TOS
END-CODE

CODE allot
    ;; fixme
    movl here_store, TA0
    addl TOS, TA0
    movl TA0, here_store
    movl (PSP)+, TOS
END-CODE

CODE cells
    ;; fixme
    lsll #2, TOS
END-CODE

CODE chars
END-CODE

CODE halt
    ;; fixme
END-CODE

CODE execute
    ;; fixme
    movl TOS, W
    DPOP TOS
    movl (W), TA0
    jmp (TA0)
END-CODE

CODE move
    ;; fixme
    movl (PSP)+, TA0
    movl (PSP)+, TA1
1:  cmpl #0, TOS
    beq  3f
2:  movb (TA1)+, (TA0)+
    subql #1, TOS
    bne   2b
3:  movl (PSP)+, TOS
END-CODE

: is_eol ( c -- n )
    dup
    10 =
    swap
    13 =
    or
;

: accept ( c-addr +n1 -- +n2 )
    dup >r
    over + swap ( end-addr base-addr -- )

    begin
        2dup - if
            key dup is_eol if
                drop cr
                bl over c!
                0 ( e b 0 -- )
            else
                dup
            then
        else
            0
        then
    while ( e b k -- )
            dup emit
            over c!
            1+
    repeat
    -
    r> swap -
;

: is_delim ( c delim -- f )
    dup bl = if ( if delim is the bl, then match any space )
        over =
        over 10 = or
        over 13 = or
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

    ( delim -- )
    drop
    word_space
;

12 CONSTANT pic_num_size
VARIABLE pic_num_off

VARIABLE base
VARIABLE >in
VARIABLE state
VARIABLE dict_start
32 CONSTANT bl

2VARIABLE ticksource

: source
    ticksource 2@
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
            1+ cstr_cmp if
                r> drop r> dup 20 + swap c@ if
                    1
                else
                    -1
                then
                exit
            then
            r> r> 16 + @
    repeat
    0
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
    abort" Word not found"
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
    0= abort" Word not found"
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

: .s
    depth ?dup if
      dup 60 emit . 62 emit ( hack until dot-quote implemented; fixme )
      pstackptr
      swap 0 do
        space
        dup i 1+ cells - @ . ( 1+ because TOS in reg; *psp0 is garbage )
      loop
      drop
    else
      60 emit 48 emit 62 emit space ( hack until dot-quote implemented; fixme )
    then
    cr
;

: cstr_to_here ( c-str maxlen -- )
    >r
    dup c@ 1+ r@ min dup >r ( c-str copy-len -- )
    here swap move
    r> r> dup rot do
      0 i here + c!
    loop
    allot
;

: create ( "word" -- here )
    here
    0 c, ( make non-immediate )
    bl word 15 cstr_to_here
    dict_start @ ,
    lit var ,
    dict_start !
;

: :
    create
    lit enter here 4 - !
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
            bl emit 111 emit 107 emit bl emit
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
    dict_start @
    1 c!
;

next_immediate
: if
    ['] qbranch ,
    here >cf 0xf0f1f1f1 ,
;

next_immediate
: else
    cf>
    ['] branch ,
    here >cf 0xf0f2f2f2 ,
    here swap !
;

next_immediate
: then
    here cf> !
;

next_immediate
: begin
    here >cf
;

next_immediate
: until
    ['] qbranch ,
    cf> ,
;

next_immediate
: while
    ['] qbranch ,
    here >cf 0xf1f1f1f1 ,
;

next_immediate
: repeat
    cf> cf>
    ['] branch , ,
    here swap !
;

next_immediate
: again
    ['] branch ,
    cf> ,
;

next_immediate
: do
    ['] 2>r ,
    here >cf 0 >cf
;

next_immediate
: leave
  ['] branch , here 0xf2f1f1f1 ,
  cf> 1+ cf> rot >cf >cf >cf
;

next_immediate
: loop
  ['] do_loop1 , cf> ( count of leaves )
  cf> , ( do target )
  ?dup if
    0 do
      here cf> !
    loop
  then
  ['] unloop ,
;

: hello
    s" Hello, world!"
    type
;

: abort_test
    1 abort" This should print then abort."
    s" This should not print." type
;

: cold
    here0
    10 base !
    0 >in !
    cf_init_uart
    core_dict @ dict_start !

    banner type cr
    quit
    depth
    bye
;
