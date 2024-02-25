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

banner_str: .text "HOLDFIRE 0.0"
banner_len: .byte (* - banner_str)

    ;; FIXME: Augment rfc so handle allocated VARs, etc
w_tib    .HIGH_W 3, "tib", w_var, , "0"
    .fill 132

w_pic_num    .HIGH_W 7, "pic_num", w_var, , "0"
    .fill 12

w_word_space    .HIGH_W 10, "word_space", w_var, , "0"
    .fill 80

w_core_dict    .HIGH_W 9, "core_dict", w_const, , "0"
    .addr dict_head

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

CODE brk
    brk
END-CODE

CODE init_serial
    jsr usb_init
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

CODE banner
    lda #>banner_str
    pha
    lda #<banner_str
    pha
    lda #0
    pha
    lda banner_len
    pha
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
    pla
    jsr     usb_tx
    pla
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
    lda #0
    pha
    jsr usb_rx
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

CODE pstackptr
    lda #1
    pha
    tsx
    inx                         ; this should be the actual address, not 1-
    phx
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
    pla
    bne nonzero1
    pla
    bne nonzero2
    bra iszero
nonzero1
    pla
nonzero2
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
    pla
    asl
    pha
    tsx
    txa
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
    tsx
    lda $103,x
    stz $103
    sta mac
    lda $104,x
    stz $104
    sta mac+1
    ldy #16
again
    ;; Shift multiplier 1 bit right and check for carry
    lda $102,x
    lsr
    sta $102,x
    lda $101,x
    ror
    sta $101,x
    bcc skip_add
    ;; Carry set. Add multiplicant to accumulator
    clc
    lda mac
    adc $103,x
    sta $103,x
    lda mac+1
    adc $104,x
    sta $104,x
skip_add
    lda mac
    asl
    sta mac
    lda mac+1
    rol
    sta mac+1
    dey
    bne again
    pla
    pla
END-CODE

: /
    /mod nip
;

: mod
    /mod drop
;

CODE /mod
    ;; fixme
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
    brk                         ;fixme ; the below seems broken
    jsr rpop2
    tax
    ldy #4
again
    lda $101,x
    pha
    dex
    dey
    bne again
END-CODE

CODE 2rdrop
    jsr rpop2
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

( loop terminator that does not clear the rstk )
CODE do_loop1
    ldx rsp
    inc rstk+1,x
    bne +
    inc rstk+2,x
+   sec
    lda rstk+3,x
    sbc rstk+1,x
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
    tsx
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
-   bra -
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
    init_serial
    core_dict @ dict_start !

    key drop
    banner type cr
    quit
    depth
    bye
;
