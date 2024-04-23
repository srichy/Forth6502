;;; -*- asm -*-
    CORE_MEM_END = $9eff

    ;; DMA

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

    ;; VIAs

    VIA0_IORB = $dc00
    VIA0_IORA = $dc01
    VIA0_DDRB = $dc02
    VIA0_DDRA = $dc03
    VIA0_T1C_L = $dc04
    VIA0_T1C_H = $dc05
    VIA0_T1L_L = $dc06
    VIA0_T1L_H = $dc07
    VIA0_T2C_L = $dc08
    VIA0_T2C_H = $dc09
    VIA0_SDR = $dc0a
    VIA0_ACR = $dc0b
    VIA0_PCR = $dc0c
    VIA0_IFR = $dc0d
    VIA0_IER = $dc0e
    VIA0_IORA2 = $dc0f

    VIA1_IORB = $db00
    VIA1_IORA = $db01
    VIA1_DDRB = $db02
    VIA1_DDRA = $db03
    VIA1_T1C_L = $db04
    VIA1_T1C_H = $db05
    VIA1_T1L_L = $db06
    VIA1_T1L_H = $db07
    VIA1_T2C_L = $db08
    VIA1_T2C_H = $db09
    VIA1_SDR = $db0a
    VIA1_ACR = $db0b
    VIA1_PCR = $db0c
    VIA1_IFR = $db0d
    VIA1_IER = $db0e
    VIA1_IORA2 = $db0f

    ;; PS/2
    PS2_CTRL = $d640
    PS2_CTRL_MCLR = %00100000
    PS2_CTRL_KCLR = %00010000
    PS2_CTRL_M_WR = %00001000
    PS2_CTRL_K_WR = %00000010

    PS2_OUT = $d641
    PS2_KBD_IN = $d642
    PS2_MOUSE_IN = $d643

    PS2_STAT = $d644
    PS2_STAT_K_AK = %10000000
    PS2_STAT_K_NK = %01000000
    PS2_STAT_M_AK = %00100000
    PS2_STAT_M_NK = %00010000
    PS2_STAT_MEMP = %00000010
    PS2_STAT_KEMP = %00000001
