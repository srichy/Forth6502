    .cpu "65816"

a16 .macro
    .al
    rep #$20
    .endmacro
a8  .macro
    .as
    sep #$20
    .endmacro

x16 .macro
    .xl
    rep #$10
    .endmacro

x8  .macro
    .xs
    sep #$10
    .endmacro

ax16 .macro
    .al
    .xl
    rep #$30
    .endmacro

ax8 .macro
    .as
    .xs
    sep #$30
    .endmacro

    .if targ=="wdc"
    .include "wdc65c816sxb.inc"
    .elsif targ=="x16"
    .include "x16.inc"
    .elsif targ=="f256"
    .include "f256.inc"
    .endif

    STACK_MEM = $80 - $22 - $12

    ;; Set up symbols for the zero elements
    * = $22
wjmp:   .byte ?
w:      .addr ?
ip:     .addr ?
rstk:   .fill STACK_MEM+1
    rstk_top = * - 1
mac:    .fill 6
here_store: .addr ?
rsp:    .addr ?
cfp:    .addr ?

    divisor = mac
    dividend = mac+2
    quotient = mac+4

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
    clc
    xce
    .ax8                        ; 8-bit A,X for init
    ldx #7
    stx source_id_sp
    stz source_id_stk,x         ; Set input to keyboard by default
    .x16
    ldx #$01ff
    txs
    ldx #CORE_MEM_END
    stx cfp
    jsr mach_init0
    ;; Init parameter stack
    .a16
    lda #rstk_top
    sta rsp
    .ax8
    lda #$4c               ; jmp a opcode
    sta wjmp               ; Now we can do 'jmp wjmp' to get 'jmp (w)'
    ;; Init hardware.
    jsr mach_init1
    ;; Load IP with "cold"
    ;; Then fall through to "next"
    .ax16
    lda #w_cold.cfa
    sta w
    jmp wjmp
    ;; The following should never be reached
    bra start

do_next:
    .ax16
    lda (ip)
    sta w
    jsr mach_dbg
    inc ip
    inc ip
    jmp wjmp

    ;; Turn A into three ASCII digit bytes
    ;; print each in order.
prt_dec_num:
    php
    .ax8
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
    plp
    rts

    .if targ=="wdc"
    .include "mach_wdc_16.s"
    .include "fth_main_wdc_16.s"
    .endif                      ; targ=="wdc"

    .if targ=="x16"
    .include "mach_x16_16.s"
    .include "fth_main_x16_16.s"
    .endif                      ; targ=="x16"

    .if targ=="f256"
    .include "mach_f256_16.s"
    .include "fth_main_f256_16.s"
    .endif                      ; targ=="x16"


edata: .addr *
