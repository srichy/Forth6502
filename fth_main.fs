( -*- forth-asm -*- )

[DEFINED] ARCH_6502 [IF]

include fth_core_6502.fs

[THEN]

[DEFINED] ARCH_65816 [IF]

include fth_core_65816.fs

[THEN]

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
        ( Limit to MAX_NM_LEN significant chars )
        MAX_NM_LEN min
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

: >=
  < invert
;

: <=
  > invert
;

: /
    /mod nip
;

: mod
    /mod drop
;

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

132 CONSTANT tiblen
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
                r> drop r> dup dict_to_cfa swap c@ 128 and if
                    1
                else
                    -1
                then
                exit
            then
            r> r> HDR_SIZE + @
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
    bl word HDR_SIZE cstr_to_here
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

: evaluate ( i*x c-addr u -- j*x )
    ( save-input )
    ( store -1 in source-id )
    ticksource 2!
    0 >in !
    interpret
    ( restore-input )
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
    dup 1+ swap c@ 0x7f and dup MAX_NM_LEN 1+ < if
        type
    else
        ( Name is longer than MAX_NM_LEN chars; show length )
        <# 62 hold #s 60 hold #> type
        MAX_NM_LEN type
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
        r> HDR_SIZE + @
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

: restore-input ( xn ... x1 n -- flag )
    abort" RESTORE-INPUT not yet supported"
;

: save-input ( -- xn ... x1 n )
    abort" SAVE-INPUT not yet supported"
;

: source-id ( -- 0 | -1 | fileid )
    abort" SOURCE-ID not yet supported"
;

[DEFINED] ARCH_F256 [IF]

include f256_files.fs

[THEN]
