AS = 64tass
ASFLAGS = -C --m65c02 -D 'proc="65c02"'
ASFLAGS16 = -C --m65816 -D 'proc="65816"'
RFC = ../rfc/target/debug/rfc

TARGETS = wdcForth x16Forth.prg f256Forth.pgz wdcForth16 x16Forth16.prg f256Forth16.pgz

all: $(TARGETS)

SRC_6502 = fth_main.fs word_hdr.fs fth_core_6502.fs
SRC_65816 = fth_main.fs word_hdr.fs fth_core_65816.fs
SRC_WDC = start.s fth_main_wdc.s mach_wdc.s
SRC_X16 = start.s fth_main_x16.s mach_x16.s x16.inc
SRC_F256 = start.s fth_main_f256.s mach_f256.s
SRC_WDC_16 = start_65816.s fth_main_wdc_16.s mach_wdc_16.s
SRC_X16_16 = start_65816.s fth_main_x16_16.s mach_x16_16.s x16.inc
SRC_F256_16 = start_65816.s fth_main_f256_16.s mach_f256_16.s

clean:
	rm -f $(TARGETS) *.o *.map *.lbl *.lis fth_main_*.s

wdcForth: $(SRC_WDC)
	$(AS) $(ASFLAGS) -D 'targ="wdc"' --nostart -o $@ -l $@.lbl -L $@.lis --map $@.map $<

x16Forth.prg: $(SRC_X16)
	$(AS) $(ASFLAGS) -D 'targ="x16"' --cbm-prg -o $@ -l $@.lbl -L $@.lis --map $@.map $<

f256Forth.pgz: $(SRC_F256)
	$(AS) $(ASFLAGS) -D 'targ="f256"' --c256-pgz --output-exec=start -o $@ -l $@.lbl -L $@.lis --map $@.map $<

fth_main_wdc.s: $(SRC_6502) $(RFC)
	$(RFC) --arch ca6502 -d ARCH_6502,ARCH_WDC fth_main.fs > $@ || rm fth_main_wdc.s

fth_main_x16.s: $(SRC_6502) c64_kernal.fs $(RFC)
	$(RFC) --arch ca6502 -d ARCH_6502,ARCH_X16,ARCH_KERNAL fth_main.fs > $@ || rm fth_main_x16.s

fth_main_f256.s: $(SRC_6502) f256_files.fs $(RFC)
	$(RFC) --arch ca6502 -d ARCH_6502,ARCH_F256 fth_main.fs > $@ || rm fth_main_f256.s

# 65816 follows

wdcForth16: $(SRC_WDC_16)
	$(AS) $(ASFLAGS16) -D 'targ="wdc"' --nostart -o $@ -l $@.lbl -L $@.lis --map $@.map $<

x16Forth16.prg: $(SRC_X16_16)
	$(AS) $(ASFLAGS16) -D 'targ="x16"' --cbm-prg -o $@ -l $@.lbl -L $@.lis --map $@.map $<

f256Forth16.pgz: $(SRC_F256_16)
	$(AS) $(ASFLAGS16) -D 'targ="f256"' --c256-pgz --output-exec=start -o $@ -l $@.lbl -L $@.lis --map $@.map $<

fth_main_wdc_16.s: $(SRC_65816) $(RFC)
	$(RFC) --arch ca6502 -d ARCH_65816,ARCH_WDC fth_main.fs > $@ || rm fth_main_wdc_16.s

fth_main_x16_16.s: $(SRC_65816) c64_kernal.fs $(RFC)
	$(RFC) --arch ca6502 -d ARCH_65816,ARCH_X16,ARCH_KERNAL fth_main.fs > $@ || rm fth_main_x16_16.s

fth_main_f256_16.s: $(SRC_65816) f256_files.fs $(RFC)
	$(RFC) --arch ca6502 -d ARCH_65816,ARCH_F256 fth_main.fs > $@ || rm fth_main_f256_16.s
