AS = 64tass
ASFLAGS = -C --m65c02
RFC = ../rfc/target/debug/rfc

TARGETS = wdcForth x16Forth.prg f256Forth.pgx f256Forth.bin f256Forth.pgz

all: $(TARGETS)

SRC_WDC = start.s fth_main.fs mach_wdc.s
SRC_X16 = start.s fth_main.fs mach_x16.s
SRC_F256 = start.s fth_main.fs mach_f256.s f256_files.fs

clean:
	rm -f $(TARGETS) *.o *.map *.lis fth_main_*.s

wdcForth: $(SRC_WDC)
	$(AS) $(ASFLAGS) -D 'targ="wdc"' --nostart -o $@ -L $@.lis --map $@.map $<

fth_main_wdc.s: $(SRC_WDC) $(RFC)
	$(RFC) --arch ca6502 -d ARCH_WDC fth_main.fs > $@ || rm fth_main_wdc.s

fth_main_x16.s: $(SRC_X16) $(RFC)
	$(RFC) --arch ca6502 -d ARCH_X16 fth_main.fs > $@ || rm fth_main_x16.s

fth_main_f256.s: $(SRC_F256) $(RFC)
	$(RFC) --arch ca6502 -d ARCH_F256 fth_main.fs > $@ || rm fth_main_f256.s

x16Forth.prg: $(SRC_X16)
	$(AS) $(ASFLAGS) -D 'targ="x16"' --cbm-prg -o $@ -L $@.lis --map $@.map $<

f256Forth.pgx: $(SRC_F256)
	$(AS) $(ASFLAGS) -D 'targ="f256"' --c256-pgx -o $@ -L $@.lis --map $@.map $<

f256Forth.pgz: $(SRC_F256)
	$(AS) $(ASFLAGS) -D 'targ="f256"' --c256-pgz --output-exec=start -o $@ -L $@.lis --map $@.map $<

f256Forth.bin: $(SRC_F256)
	$(AS) $(ASFLAGS) -D 'targ="f256"' --nostart -o $@ -L $@.lis --map $@.map $<
