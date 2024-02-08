( -*- forth-asm -*- )

HEADLESSCODE

    previous_entry := 0


link_entry .macro new_entry
    .addr previous_entry
    previous_entry := \new_entry
    .endmacro

HIGH_W .macro sym name_len name act=do_enter flgs=0
dict:
    .byte (flgs << 7) | len(\name)
    .text format("%-5s", \name[:5])
    .link_entry \{sym}.dict
cfa:
    .addr \act                  ; CFA
    .endmacro                   ; PFA implicit, follows macro

CODE_W .macro sym name_len name flgs=0
dict:
    .byte (flgs << 7) | len(\name)
    .text format("%-5s", \name[:5])
    .link_entry \{sym}.dict
cfa:
    .addr (* + 2)               ; CFA
    .endmacro                   ; PFA implicit, follows macro

NEXT .macro
    jmp do_next
    .endmacro

RPUSH .macro reg
    movl \reg, -(RSP)
    .endmacro

RPOP .macro reg
    jsr check_rstack_underflow
    movl (RSP)+, \reg
    .endmacro

DPUSH .macro reg
    movl \reg, -(PSP)
    .endmacro

DPOP .macro reg
    jsr check_pstack_underflow
    movl (PSP)+, \reg
    .endmacro

    .data
    .align 4

banner_str: .text "Holdfire 0.0"
banner_len: .word (* - banner_str)

cold0:  .int cold
ssp_bottom:
    .space (64 * 4)
ssp0:    .int
rsp_bottom:
        .space (64 * 4)
rsp0:   .int                    | dummy
csp_bottom:
        .space (64 * 4)
csp0:   .int                    | dummy
psp_bottom:
        .space (64 * 4)
psp0:   .int                    | dummy

        HIGH_W tib 3 "tib" act=do_var
        .space 132

        HIGH_W pic_num 7 "pic_num" act=do_var
        .space 12

        HIGH_W word_space 10 "word_space" act=do_var
        .space 256

        HIGH_W core_dict 9 "core_dict" act=do_const
        .int dict_head

here_store:
    .int    0

    .text
    .globl  _start
_start:
    lea ssp0, SSP
    lea rsp0, RSP
    lea psp0, PSP
    lea csp0, CSP
    lea cold0, IP
    movea.l    #0x10000140,TA0
    move.l    #0x13, %d0
    move.b    %d0, (TA0)
    move.l    #0x07, %d0
    move.b    %d0, (TA0)
    movea.l    #0x10000148,TA0
    move.l    #0x05, %d0
    move.b    %d0, (TA0)
    || Fall through to "next"
do_next:
    movl    (IP), W
    addq.l  #4, IP
    movl    (W), TA0
    jmp     (TA0)

check_pstack_underflow:
    lea   psp0, TA0
    cmpal TA0, PSP
    beq   1f
    rts
1:  movel #0xe1e1e1e1, %d0
    bra   1b

check_rstack_underflow:
    lea   rsp0, TA0
    cmpal TA0, RSP
    beq   1f
    rts
1:  movel #0xd1d1d1d1, %d0
    bra   1b

await_char:
    moveal  #0x10000144, TA0
1:  btst.b  #1, (TA0)
    beq     1b
    rts

output_ready:
    moveal  #0x10000144, TA0
1:  btst.b  #3, (TA0)
    beq     1b
    rts

outchar:
    move.l    TA0, -(SSP)
    movea.l    #0x1000014c,TA0
    move.b    %d0, (TA0)
    move.l    (SSP)+, TA0
    rts

dump_long:
    move.l    %d3, -(SSP)
    move.l    %d2, -(SSP)
    move.l    %d0, %d3
    move.l    #28, %d1
1:  move.l    %d3, %d0
    lsr.l    %d1, %d0
    andi.l    #0x0f, %d0
    cmp.l    #0x0a, %d0
    blt    2f
    addi.l    #(65 - 0x30 - 10), %d0
2:  addi.l    #0x30, %d0
    jsr    outchar
    subq.l    #4, %d1
    cmp.l    #0, %d1
    bge    1b
    move.l    %d2, (SSP)+
    move.l    %d3, (SSP)+
    rts

debug_dump:
    move.b    #0x52, %d0
    jsr    outchar
    move.b    #0x3a, %d0
    jsr    outchar
    movea.l    RSP, TA1
    lea    rsp0, TA2
1:  cmp.l    TA1, TA2
    beq    2f
    move.l    (TA1)+, %d0
    jsr    dump_long
    move.b    #0x20, %d0
    jsr    outchar
    bra    1b
2:  move.b    #10, %d0
    jsr    outchar
    move.b    #13, %d0
    jsr    outchar
    rts

debug_trace:
    movb    #0x40, %d0
    jsr     outchar
    movb    #0x3a, %d0
    jsr     outchar
    lea rsp0, TA2
    subal RSP, TA2
    movl    TA2, %d0
    jsr     dump_long
    move.b    #0x20, %d0
    jsr    outchar
    movl    W, %d0
    jsr     dump_long
    move.b    #10, %d0
    jsr    outchar
    move.b    #13, %d0
    jsr    outchar
    jmp     do_enter

do_enter:
    |jsr    debug_dump
    RPUSH   IP
    movl    W, IP
    addq.l  #4, IP
    .NEXT

do_var:
    DPUSH   TOS
    movl    W, TOS
    addq.l  #4, TOS
    .NEXT

do_const:
    DPUSH   TOS
    movl    4(W), TOS
    .NEXT

END-CODE

CODE does_var
    DPUSH   TOS
    movl    do_var, TOS
END-CODE

( Low-level Forth engine support )
CODE lit
    DPUSH TOS
    movl  (IP)+, TOS
END-CODE

CODE banner
    movl TOS, -(PSP)
    movl  #banner_str, -(PSP)
    lea   banner_len, TA0
    movl  (TA0), TOS
END-CODE

CODE debugdump
    jsr debug_dump
END-CODE

CODE exit
    RPOP IP
END-CODE

( Coldfire processor-specific stuff )

0x10000000 CONSTANT mbar
0x140 CONSTANT uart_umr
0x144 CONSTANT uart_usr
0x148 CONSTANT uart_ucr
0x14c CONSTANT uart_urb
0x150 CONSTANT uart_uacr
0x154 CONSTANT uart_uisr
0x158 CONSTANT uart_ubg1
0x15c CONSTANT uart_ubg2
0x170 CONSTANT uart_uivr
0x174 CONSTANT uart_uip
0x178 CONSTANT uart_uop1
0x17c CONSTANT uart_uop0

132 CONSTANT tiblen

: to_mbar ( x -- x )
    mbar +
;

: cf_init_uart ( -- )
    0x13 uart_umr to_mbar c!
    0x07 uart_umr to_mbar c!
    0x05 uart_ucr to_mbar c!
;

CODE p0
    lea psp0, PSP
END-CODE

CODE r0
    lea rsp0, RSP
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
    jsr     output_ready
    movb    TOS, %d0
    jsr     outchar
    movl    (PSP)+, TOS
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

: key ( -- c )
    uart_usr to_mbar
    begin
        dup c@
        1 and
    until
    drop
    uart_urb to_mbar c@
;

CODE unloop
    addql #8, RSP
END-CODE

CODE depth
    movl TOS, -(PSP)
    lea  psp0, TA0
    movl TA0, TOS
    subl PSP, TOS
    lsrl #2, TOS
    subl #1, TOS
END-CODE

CODE pstackptr
    movl TOS, -(PSP)
    lea psp0, TA0
    movl TA0, TOS
    subql #4, TOS
END-CODE

CODE dup
    movl TOS, -(PSP)
END-CODE

CODE ?dup
    cmpl #0, TOS
    beq 1f
    movl TOS, -(PSP)
1:
END-CODE

CODE drop
    movl (PSP)+, TOS
END-CODE

CODE swap
    movl (PSP), %d0
    movl TOS, (PSP)
    movl %d0, TOS
END-CODE

CODE over
    movl TOS, -(PSP)
    movl 4(PSP), TOS
END-CODE

CODE nip
    addql #4, PSP
END-CODE

CODE tuck
    movl (PSP), -(PSP)
    movl TOS, 4(PSP)
END-CODE

CODE pick
    movl (PSP,TOS:l:4), TOS
END-CODE

CODE 2drop
    movl 4(PSP), TOS
    addql #8, PSP
END-CODE

CODE 2dup
    movl TOS, -(PSP)
    movl 4(PSP), -(PSP)
END-CODE

CODE 2over
    movl TOS, -(PSP)
    movl 12(PSP), -(PSP)
    movl 12(PSP), TOS
END-CODE

CODE 2swap
    movl 4(PSP), %d0  | x2 -> %d0
    movl TOS, 4(PSP)  | x4 -> old x2
    movl %d0, TOS     | x2 -> old x4
    movl 8(PSP), %d0  | x1 -> %d0
    movl (PSP), 8(PSP)| x3 -> old x1
    movl %d0, (PSP)   | x1 -> old x3
END-CODE

CODE rot
    movl 4(PSP), %d0
    movl (PSP), 4(PSP)
    movl TOS, (PSP)
    movl %d0, TOS
END-CODE

CODE and
    andl (PSP)+, TOS
END-CODE

CODE or
    orl  (PSP)+, TOS
END-CODE

CODE =
    cmpl  (PSP)+, TOS
    seq   TOS
    extbl TOS
END-CODE

CODE <>
    cmpl  (PSP)+, TOS
    sne   TOS
    extbl TOS
END-CODE

CODE 0=
    cmpil #0, TOS
    seq   TOS
    extbl TOS
END-CODE

CODE invert
    notl  TOS
END-CODE

CODE 0<
    cmpil #0, TOS
    slt   TOS
    extbl TOS
END-CODE

CODE 0>
    cmpil #0, TOS
    sgt   TOS
    extbl TOS
END-CODE

CODE <
    cmpl (PSP)+, TOS
    sgt  TOS
    extbl TOS
END-CODE

CODE >
    cmpl (PSP)+, TOS
    slt  TOS
    extbl TOS
END-CODE

CODE >=
    cmpl (PSP)+, TOS
    sle  TOS
    extbl TOS
END-CODE

CODE <=
    cmpl (PSP)+, TOS
    sge  TOS
    extbl TOS
END-CODE

CODE +
    movl  (PSP)+, %d0
    addl  %d0, TOS
END-CODE

CODE -
    movl  (PSP)+, %d0
    subl  TOS, %d0
    movl  %d0, TOS
END-CODE

CODE 1+
    addql #1, TOS
END-CODE

CODE 1-
    subql #1, TOS
END-CODE

CODE *
    movl (PSP)+, %d0
    mulul %d0, TOS
END-CODE

CODE /
    movl (PSP)+, %d0
    divul TOS, %d0
    movl %d0, TOS
END-CODE

CODE mod
    movl (PSP)+, %d0
    remul TOS, %d1:%d0
    movl %d1, TOS
END-CODE

CODE /mod
    movl (PSP), %d0  | n1
    movl %d0, %d1    | n1
    movl TOS, %d2    | n2
    divsl TOS, %d0   | %d0 = n1 / n2
    movl %d0, TOS    | TOS = n4 (n1 / n2)
    remsl %d2, %d3:%d1
    movl %d3, (PSP)
END-CODE

CODE 2@
    movl  TOS, TA0
    movl  (4,TA0), -(PSP)
    movl  (TA0), TOS
END-CODE

CODE @
    movl  TOS, TA0
    movl  (TA0), TOS
END-CODE

CODE c@
    movl  TOS, TA0
    movb  (TA0), TOS
    andil #0xff, TOS
END-CODE

CODE 2!
    movl  TOS, TA0
    movl  (PSP)+, (TA0)
    movl  (PSP)+, (4,TA0)
    movl  (PSP)+, TOS
END-CODE

CODE !
    movl  TOS, TA0
    movl  (PSP)+, (TA0)
    movl  (PSP)+, TOS
END-CODE

CODE +!
    movl  TOS, TA0
    movl  (PSP)+, %d0
    movl  (PSP)+, TOS
    addl  %d0, (TA0)
END-CODE

CODE c!
    movl  TOS, TA0
    movl  (PSP)+, TOS
    movb  TOS, (TA0)
    movl  (PSP)+, TOS
END-CODE

CODE branch
    movl  (IP), IP
END-CODE

CODE qbranch
    movl  (IP)+, TA0
    movl  TOS, %d0
    movl  (PSP)+, TOS
    cmpl  #0, %d0
    bne   1f
    movl  TA0, IP
1:
END-CODE

CODE bye
    movel #0xdeadbead, %d6
    halt
END-CODE

CODE ddd
    movl  TOS, %d5
    movel #0xdeadbeef, %d6
    halt
END-CODE

CODE >r
    movl  TOS, -(RSP)
    movl  (PSP)+, TOS
END-CODE

CODE r>
    movl TOS, -(PSP)
    movl (RSP)+, TOS
END-CODE

CODE r@
    movl TOS, -(PSP)
    movl (RSP), TOS
END-CODE

CODE 2>r
    movl  (PSP)+, -(RSP)
    movl  TOS, -(RSP)
    movl  (PSP)+, TOS
END-CODE

CODE 2r>
    movl  TOS, -(PSP)
    movl  (RSP)+, TOS
    movl  (RSP)+, -(PSP)
END-CODE

CODE 2rdrop
    addql #8, RSP
END-CODE

CODE >cf
    movl  TOS, -(CSP)
    movl  (PSP)+, TOS
END-CODE

CODE cf>
    movl TOS, -(PSP)
    movl (CSP)+, TOS
END-CODE

CODE cf@
    movl TOS, -(PSP)
    movl (CSP), TOS
END-CODE

CODE cfnip
    movl TOS, -(PSP)
    movl 4(CSP), TOS
    movl (CSP), 4(CSP)
    addql #4, CSP
END-CODE

CODE cftuck
    movl (CSP), -(CSP)
    movl TOS, 4(CSP)
    movl (PSP)+, TOS
END-CODE

CODE i
    movl TOS, -(PSP)
    movl (RSP), TOS
END-CODE

CODE j
    movl TOS, -(PSP)
    movl 8(RSP), TOS
END-CODE

CODE k
    movl TOS, -(PSP)
    movl 16(RSP), TOS
END-CODE

CODE do_loop
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
    lea _edata, TA0
    movl TA0, here_store
END-CODE

CODE here
    movl TOS, -(PSP)
    movl (here_store), TOS
END-CODE

CODE ,
    movl here_store, TA0
    movl TOS, (TA0)+
    movl TA0, here_store
    movl (PSP)+, TOS
END-CODE

CODE c,
    movl here_store, TA0
    movb TOS, (TA0)+
    movl TA0, here_store
    movl (PSP)+, TOS
END-CODE

CODE allot
    movl here_store, TA0
    addl TOS, TA0
    movl TA0, here_store
    movl (PSP)+, TOS
END-CODE

CODE cells
    lsll #2, TOS
END-CODE

CODE chars
END-CODE

CODE halt
    halt
END-CODE

CODE execute
    movl TOS, W
    DPOP TOS
    movl (W), TA0
    jmp (TA0)
END-CODE

CODE move
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

: star ( -- )
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
    lit do_var ,
    dict_start !
;

: :
    create
    lit do_enter here 4 - !
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
