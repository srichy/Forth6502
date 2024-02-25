    .cpu "65c02"

    .if targ=="wdc"
    .include "wdc65c816sxb.inc"
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

    .if targ=="wdc"
    * = $200
    .elsif targ=="x16"

    *       = $0801
    .word (+), 2005  ;pointer, line number
    .null $9e, format("%4d", start);will be sys 4096
+   .word 0          ;basic line end

    *   = $0820

    .elsif targ=="f256"

    * = $1000

    .endif

start:
    jsr mach_init0
    lda #$4c               ; jmp a opcode
    sta wjmp               ; Now we can do 'jmp wjmp' to get 'jmp (w)'
    ;; Init parameter stack
    lda #STACK_MEM
    sta rsp
    ;; Init return stack
    ldx #$ff
    txs
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

    .if targ=="wdc"
    .include "mach_wdc.s"
    .endif                      ; targ=="wdc"

    .if targ=="x16"
    .include "mach_x16.s"
    .endif                      ; targ=="x16"

    .if targ=="f256"
    .include "mach_f256.s"
    .endif                      ; targ=="x16"

    .include "fth_main.s"

edata: .addr *
