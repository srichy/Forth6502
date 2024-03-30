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

: delete-file ( c-addr u fam -- ior )
    init_file_name
    do_delete_file
;

: file-position ( fileid - ud ior )
    0xffff ( not supported )
;

: file-size ( fileid - ud ior )
    0xffff ( not supported )
;

: open-file ( c-addr u fam -- fileid ior )
    create-file
;

: read-file ( c-addr u1 fileid -- u2 ior )
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
