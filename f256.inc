;;; -*- asm -*-
    CORE_MEM_END = $9fff

    DMA_CTRL = $DF00

    DMA_CTRL_START = $80
    DMA_CTRL_FILL = $04
    CMA_CTRL_ENABLE = $01

    DMA_STATUS = $DF01
    DMA_STAT_BUSY = $80

    DMA_FILL_VAL = $DF01

    DMA_DST_ADDR = $DF08
    DMA_COUNT = $DF0C

    ;; Integer Coprocessor
    MULU_A_L = $de00
    MULU_A_H = $de01
    MULU_B_L = $de02
    MULU_B_H = $de03
    MULU_LL = $de10
    MULU_LH = $de11
    MULU_HL = $de12
    MULU_HH = $de13

    DIVU_D_L = $de04
    DIVU_D_H = $de05
    DIVU_N_L = $de06
    DIVU_N_H = $de07
    QUOT_L = $de14
    QUOT_H = $de15
    REMU_L = $de16
    REMU_H = $de17
