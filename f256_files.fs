( -*- forth-asm -*- )

: bin ( fam1 -- fam2 )
    ( This does nothing for now )
;

0 CONSTANT R/O
1 CONSTANT R/W
2 CONSTANT W/O

( fileid -- ior )
CODE close-file
    pla
    sta kernel.args.file.close.stream
    pla
    jsr kernel.File.Close
    bcc _success
    lda #$ff
    bra _done
_success:
    lda #0
_done:
    pha
    pha
END-CODE

( len c-addr drive fam -- )
CODE save_file_name
    pla
    sta kernel.args.file.open.mode
    pla
    pla
    sta kernel.args.file.open.drive
    pla
    pla
    sta kernel.args.file.open.fname_len
    pla
    pla
    sta kernel.args.file.open.fname
    pla
    sta kernel.args.file.open.fname+1
END-CODE

: init_file_name ( c-addr u fam -- ) ( Initialize file name struct )
    >r
    dup 2 > if
        over 1+ c@ 58 = if ( do we have something like "0:filename"? )
            over c@ 0x30 - >r
            2 - swap 2 + swap r>
        else
            0
        then
    else
        0
    then
    ( c-addr u drive -- )
    r> save_file_name
;

( init_file_name must be called before this! )
( -- fileid ior )
CODE do_create_file
    jsr kernel.File.Open
    bcc _success
    lda #$ff
    pha
    pha
    bra _done
_success:
    tax
    lda #0
    pha
    phx
_done:
    pha
    pha
END-CODE

CODE do_delete_file
    jsr kernel.File.Delete
    bcc _success
    lda #$ff
    bra _done
_success:
    lda #0
_done:
    pha
    pha
END-CODE

: create-file ( c-addr u fam -- fileid ior )
    init_file_name
    do_create_file
;

: delete-file ( c-addr u -- ior )
    1 init_file_name
    do_delete_file
;

: file-position ( fileid - ud ior )
    1 abort" FILE-POSITION not yet supported"
;

: file-size ( fileid - ud ior )
    1 abort" FILE-SIZE not yet supported"
;

: include-file ( i*x fileid -- j*x ) ( FIXME: refactor into QUIT and friends )
    dup push-source-id
    begin
        tib dup tiblen 2 - source-id read-line if
            pop-source-id
            1 abort" File read failed.  include-file aborted."
        then
    while ( flag is true; we have not reached EOF yet )
        ?dup if ( We have a non-zero count of characters on this line )
            ticksource 2!
            0 >in !
            interpret
        then
    repeat
    pop-source-id
;

: included ( i*x c-addr u -- j*x )
    R/O open-file abort" File open failed"
    include-file
;

: open-file ( c-addr u fam -- fileid ior )
    create-file
;

( c-addr u1 fileid -- u2 ior )
CODE read-file
    pla                         ; fileid:L
    sta kernel.args.file.read.stream
    pla                         ; fileid:H
    pla                         ; u1:L
    sta kernel.args.file.read.buflen
    pla                         ; u1:H
    jsr kernel.File.Read
    bcc _success
    pla                         ; c-addr:L
    pla                         ; c-addr:H
    lda #0
    pha                         ; u2:H
    pha                         ; u2:L
    lda #$ff
    pha                         ; ior:H
    pha                         ; ior:L
    bra _done
_success:                       ; Wait for file read event
    lda kernel.args.events.pending
    bpl _success
    jsr kernel.NextEvent
    bcs _success
    lda event.type
    cmp #kernel.event.file.DATA
    beq _copy_read_data
    cmp #kernel.event.file.EOF
    bne _success                ; Only read responses for now
    ;; We have hit EOF
    pla                         ; c-addr:L
    pla                         ; c-addr:H
    lda #0
    pha                         ; u2:H
    pha                         ; u2:L
    pha                         ; ior:H
    pha                         ; ior:L
    bra _done
_copy_read_data:
    pla                         ; c-addr:L
    sta kernel.args.buf
    pla                         ; c-addr:H
    sta kernel.args.buf+1
    lda #0
    pha                         ; u2:H
    lda event.file.data.read
    pha                         ; u2:L
    sta kernel.args.buflen
    jsr kernel.ReadData
    lda #0
    pha                         ; ior:H
    pha                         ; ior:L
_done:
END-CODE

( A -1 means EOF )
: read-byte ( fileid -- b )
    >r here 1 r> read-file abort" READ-BYTE: Error reading file"
    0= if
        -1
    else
        here c@
    then
;

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
        dup r> = if ( At EOF, and we read nothing )
            2drop 0 0 0
            exit
        then
    then
    r> - nip -1 0
;

( ud fileid -- ior )
CODE reposition-file
    pla
    sta kernel.args.file.seek.stream
    pla
    pla
    sta kernel.args.file.seek.position+2
    pla
    sta kernel.args.file.seek.position+3
    pla
    sta kernel.args.file.seek.position+0
    pla
    sta kernel.args.file.seek.position+1
    jsr kernel.File.Seek
    bcc _success
    lda #$ff
    bra _done
_success:
    lda #0
_done:
    pha
    pha
END-CODE

: resize-file ( ud fileid -- ior )
    1 abort" RESIZE-FILE not yet supported"
;

( c-addr u fileid -- ior )
CODE write-file
    pla
    sta kernel.args.file.write.stream
    pla
    pla
    sta kernel.args.file.write.buflen
    pla
    pla
    sta kernel.args.file.write.buf
    pla
    sta kernel.args.file.write.buf+1
    jsr kernel.File.Write
    bcc _success
    lda #$ff
    bra _done
_success:
    lda #0
_done:
    pha
    pha
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
