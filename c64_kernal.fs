( -*- forth-asm -*- )

( c64_kernal; was x16_files; but this is almost 100% KERNAL )

: x16_init ( FIXME: this needs to go into a different file )
    0 K_FD_BITS ! ( No files open at start )
;

: bin ( fam1 -- fam2 )
    ( This does nothing for now )
;

0 CONSTANT R/O
1 CONSTANT R/W
2 CONSTANT W/O

0xffb7 CONSTANT K_READST
0x01 CONSTANT K_ST_WRTIMEOUT
0x02 CONSTANT K_ST_RDTIMEOUT
0x04 CONSTANT K_ST_SHORTBLK
0x08 CONSTANT K_ST_LONGBLK
0x10 CONSTANT K_ST_UNRECRD
0x20 CONSTANT K_ST_CHKSUM
0x40 CONSTANT K_ST_EOF
0x80 CONSTANT K_ST_EOTAPE

0xffba CONSTANT K_SETLFS
0xffbd CONSTANT K_SETNAM
0xffc0 CONSTANT K_OPEN
0xffc3 CONSTANT K_CLOSE
0xffc6 CONSTANT K_CHKIN
0xffc9 CONSTANT K_CHKOUT
0xffcc CONSTANT K_CLRCHN
0xffcf CONSTANT K_CHRIN
0xffd2 CONSTANT K_CHROUT
0xffdb CONSTANT K_SETTIM
0xffde CONSTANT K_RDTIM
0xffe7 CONSTANT K_CLALL

VARIABLE K_FD_BITS

( -- )
: k_reset_io
    0 0 0 K_CLRCHN call_kernal 2drop
;

( fileid -- ior )
: close-file
    0 0 K_CLOSE call_kernal nip
    k_reset_io
;

( A X Y KERNAL_FUNC -- A 0|-1 )
CODE call_kernal
    .if proc=="65816"
    pla
    sta mac+1
    ply
    plx
    .ax8
    .else
    pla
    sta mac+1
    pla
    sta mac+2
    ply
    plx                         ; top half of Y (discarded)
    plx                         ; bot half of X
    pla                         ; top half of X (discarded)
    .endif

    lda #$4c
    sta mac
    pla
    jsr mac

    plx                         ; top half of A (discarded)

    ldx #0
    phx
    pha
    bcc success
    lda #$ff
    bra done
success:
    lda #0
done:
    pha
    pha
END-CODE

( c-addr u -- )
: k_file_name
    swap dup
    0xff and swap
    8 rshift 0xff and
    K_SETNAM call_kernal 2drop
;

: init_file_name ( c-addr u -- drive-n )
    dup 2 > if
        over 1+ c@ 58 = if ( do we have something like "8:filename"? )
            over c@ 0x30 - >r
            2 - swap 2 + swap r>
        else
            8
        then
    else
        8
    then
    ( c-addr u drive -- )
    rot rot
    k_file_name
;

: assign_fileid ( drive-n -- fileid ior )
    14 dup 2 do
        1 i lshift K_FD_BITS @ and 0= if
            drop i leave
        then
    loop
    dup 14 = if
        drop ( drop "14"; drive-n becomes the pseudo-fileid )
        -1
    else
        dup >r swap over K_SETLFS call_kernal 2drop r>
        1 over lshift K_FD_BITS @ or K_FD_BITS !
        0
    then
;

: unassign_fileid ( fileid -- )
    1- 1 swap lshift invert
    K_FD_BITS @ and
    K_FD_BITS !
;

: create-file ( c-addr u fam -- fileid ior )
    >r
    init_file_name
    assign_fileid abort" Out of file descriptors"
    0 0 0 K_OPEN call_kernal if
        k_reset_io
        1 abort" Cannot open file"
    then
    drop ( unused call_kernal rval )
    dup 0 swap 0 ( use and return fileid; prepare for call_kernal )
    r> R/O = if
        K_CHKIN call_kernal if
            k_reset_io
            1 abort" Cannot CHKIN fileid"
        then
        drop ( unused call_kernal rval )
    else
        K_CHKOUT call_kernal if
            k_reset_io
            1 abort" Cannot CHKOUT fileid"
        then
        drop ( unused call_kernal rval )
    then
    0
;

: delete-file ( c-addr u -- ior )
    1 abort" DELETE-FILE not yet supported"
;

: file-position ( fileid - ud ior )
    1 abort" FILE-POSITION not yet supported"
;

: file-size ( fileid - ud ior )
    1 abort" FILE-SIZE not yet supported"
;

: include-file ( i*x fileid -- j*x )
    push-source-id
    begin
        tib dup tiblen 2 - source-id read-line if
            pop-source-id drop
            1 abort" File read failed.  include-file aborted."
        then
    while ( flag is true; we have not reached EOF yet )
        ?dup if ( We have a non-zero count of characters on this line )
            ticksource 2!
            0 >in !
            interpret
        else
            2drop
        then
    repeat
    2drop
    pop-source-id close-file drop
;

: included ( i*x c-addr u -- j*x )
    R/O open-file if
        k_reset_io
        1 abort" File open failed"
    then
    include-file
;

: open-file ( c-addr u fam -- fileid ior )
    create-file if
        k_reset_io
        1 abort" Cannot open file"
    then
    0
;

( c-addr u1 fileid -- u2 ior )
: read-file
;

( A -1 means EOF )
: read-byte ( fileid -- b )
    drop ( FIXME )
    0 0 0 K_CHRIN call_kernal drop
    0 0 0 K_READST call_kernal drop
    ?dup if
        K_ST_EOF = if
            drop
            k_reset_io
            -1
        else
            ." Got unexpected status code: " . cr
            abort
        then
    then
;

VARIABLE xin 128 XALLOT
128 CONSTANT xinlen

( First version is going to be sloooooow. )
: read-line ( c-addr u1 fileid -- u2 flag ior )
    2 pick >r ( save original buffer addr )
    >r over + swap ( end-addr cur-addr -- ; R: start-addr fileid )
    begin
        2dup > ( e c f -- ; enough room for another? )
        r@ read-byte swap over -1 <> and ( e c b f -- ; not EOF? )
        over 10 <> and ( not LF? )
        over 13 <> and ( not CR? )
    while
        over c!
        1+
    repeat
    r> drop ( fileid no longer needed )
    ( end-addr cur-addr c )
    -1 = if ( We hit EOF )
        dup r@ = if ( At EOF, and we read nothing )
            r> drop
            2drop 0 0 0
            exit
        then
    then
    r> - nip -1 0
;

( ud fileid -- ior )
CODE reposition-file
END-CODE

: resize-file ( ud fileid -- ior )
    1 abort" RESIZE-FILE not yet supported"
;

( c-addr u fileid -- ior )
CODE write-file
END-CODE

: write-line ( c-addr u fileid -- ior )
    1 abort" READ-LINE not yet supported"
;

( File-Access extension words )

: file-status ( c-addr u -- x ior )
    1 abort" FILE-STATUS not yet supported"
;

: flush-file ( fileid -- ior )
    1 abort" FLUSH-FILE not yet supported"
;

: include ( i*x "name" -- j*x )
    bl word count included
;

: refill ( -- flag )
    1 abort" REFILL not yet supported"
;

: rename-file ( c-addr1 u1 c-addr2 u2 -- ior )
    1 abort" RENAME-FILE not yet supported"
;

: require ( i*x "name" -- i*x )
    1 abort" REQUIRE not yet supported"
;

: required ( i*x c-addr u -- i*x )
    1 abort" REQUIRED not yet supported"
;
