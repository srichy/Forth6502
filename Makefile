AS = 64tass
ASFLAGS = -C --m65c02
RFC = ../rfc/target/debug/rfc

TARGETS = wdcForth x16Forth.prg f256Forth.pgz

all: $(TARGETS)

SRC_6502 = fth_main.fs fth_core_6502.fs
SRC_WDC = start.s fth_main_wdc.s mach_wdc.s
SRC_X16 = start.s fth_main_x16.s mach_x16.s
SRC_F256 = start.s fth_main_f256.s mach_f256.s

clean:
	rm -f $(TARGETS) *.o *.map *.lis fth_main_*.s

wdcForth: $(SRC_WDC)
	$(AS) $(ASFLAGS) -D 'targ="wdc"' --nostart -o $@ -L $@.lis --map $@.map $<

fth_main_wdc.s: $(SRC_6502) $(RFC)
	$(RFC) --arch ca6502 -d ARCH_6502,ARCH_WDC fth_main.fs > $@ || rm fth_main_wdc.s

fth_main_x16.s: $(SRC_6502) $(RFC)
	$(RFC) --arch ca6502 -d ARCH_6502,ARCH_X16 fth_main.fs > $@ || rm fth_main_x16.s

fth_main_f256.s: $(SRC_6502) f256_files.fs $(RFC)
	$(RFC) --arch ca6502 -d ARCH_6502,ARCH_F256 fth_main.fs > $@ || rm fth_main_f256.s

x16Forth.prg: $(SRC_X16)
	$(AS) $(ASFLAGS) -D 'targ="x16"' --cbm-prg -o $@ -L $@.lis --map $@.map $<

f256Forth.pgz: $(SRC_F256)
	$(AS) $(ASFLAGS) -D 'targ="f256"' --c256-pgz --output-exec=start -o $@ -L $@.lis --map $@.map $<
