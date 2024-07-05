    .cpu "65c02"

    .if targ=="wdc"
    .include "wdc65c816sxb.inc"
    .elsif targ=="x16"
    .include "x16.inc"
    .elsif targ=="f256"
    .include "f256.inc"
    .endif

    STACK_MEM = $80 - $22 - $10

    ;; Set up symbols for the zero elements
    * = $22
rsp:    .byte ?
wjmp:   .byte ?
w:      .addr ?
ip:     .addr ?
rstk:   .fill STACK_MEM+1
mac:    .fill 6
here_store: .addr ?
cfp:    .byte ?

    divisor = mac
    dividend = mac+2

    .if targ=="wdc"
    ;; Still in zero page
size_x: .byte ?
size_y: .byte ?
screen_x: .byte ?
screen_y: .byte ?
    * = $200
    .elsif targ=="x16"

    *       = $0801
    .word (+), 2005  ;pointer, line number
    .null $9e, format("%4d", start);will be sys 4096
+   .word 0          ;basic line end

    *   = $0820

    .elsif targ=="f256"
    .include "api.asm"

    ;; Still in zero page
event .dstruct kernel.event.event_t
screen_x: .byte ?
screen_y: .byte ?
scrptr: .addr ?
    * = $200

    .endif

start:
    ldx #$ff
    txs
    stx cfp
    ldx #7
    stx source_id_sp
    stz source_id_stk,x         ; Set input to keyboard by default
    jsr mach_init0
    lda #$4c               ; jmp a opcode
    sta wjmp               ; Now we can do 'jmp wjmp' to get 'jmp (w)'
    ;; Init parameter stack
    lda #STACK_MEM
    sta rsp
    ;; Init return stack
    ;; Init hardware.  FIXME to make this per-platform
    jsr mach_init1
    ;; Load IP with "cold"
    ;; Then fall through to "next"
    lda #<w_cold.cfa
    sta w
    lda #>w_cold.cfa
    sta w+1
    jmp wjmp
    ;; The following should never be reached
    bra start

do_next:
    ldy #1
    lda (ip)
    sta w
    lda (ip),y
    sta w+1
    jsr mach_dbg
    clc
    lda ip
    adc #2
    sta ip
    lda ip+1
    adc #0
    sta ip+1
    jmp wjmp

    ;; Turn A into three ASCII digit bytes
    ;; print each in order.
prt_dec_num:
    pha                         ;Save the number for repeated ops
    ldx #0
    stx mac+4
_do_100:
    sec
    sbc #100
    bcc _done_100
    pha
    clc
    lda mac+4
    adc #100
    sta mac+4
    inx
    pla
    bra _do_100
_done_100:
    cpx #0
    beq _do_10s
    clc
    txa
    adc #$30
    jsr con_tx
_do_10s:
    pla
    sec
    sbc mac+4
    pha
    ldx #0
    stx mac+4
_do_10:
    sec
    sbc #10
    bcc _done_10
    clc
    pha
    lda mac+4
    adc #10
    sta mac+4
    inx
    pla
    bra _do_10
_done_10:
    clc
    txa
    adc #$30
    jsr con_tx
_do_1s:
    pla
    sec
    sbc mac+4
    clc
    adc #$30
    jsr con_tx
    rts

    .if targ=="wdc"
    .include "mach_wdc.s"
    .include "fth_main_wdc.s"
    .endif                      ; targ=="wdc"

    .if targ=="x16"
    .include "mach_x16.s"
    .include "fth_main_x16.s"
    .endif                      ; targ=="x16"

    .if targ=="f256"
    .include "mach_f256.s"
    .include "fth_main_f256.s"
    .endif                      ; targ=="x16"


edata: .addr *
