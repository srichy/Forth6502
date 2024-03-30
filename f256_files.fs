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
    0x0 0x0 0xffff ( not supported )
;

: file-size ( fileid - ud ior )
    0x0 0x0 0xffff ( not supported )
;

: include-file ( i*x fileid -- j*x )
    abort" INCLUDE-FILE not yet supported"
;

: included ( i*x c-addr u -- j*x )
    abort" INCLUDED not yet supported"
;

: open-file ( c-addr u fam -- fileid ior )
    create-file
;

( c-addr u1 fileid -- u2 ior )
CODE read-file
    pla
    sta kernel.args.file.read.stream
    pla
    pla
    sta kernel.args.file.read.buflen
    pla
    jsr kernel.File.Read
    bcc _success
    lda #0
    pha
    pha
    lda #$ff
    pha
    pha
    jsr _done
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
    pla
    pla
    lda #0
    pha
    pha
    pha
    pha
    bra _done
_copy_read_data:
    pla
    sta kernel.args.buf
    pla
    sta kernel.args.buf+1
    lda #0
    pha
    lda event.file.data.read
    pha
    sta kernel.args.buflen
    jsr kernel.ReadData
    lda #0
    pha
    pha
_done:
END-CODE

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
