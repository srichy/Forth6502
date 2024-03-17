AS = 64tass
ASFLAGS = -C --m65c02
RFC = ../rfc/target/debug/rfc

TARGETS = wdcForth x16Forth.prg f256Forth.pgx f256Forth.bin f256Forth.pgz

all: $(TARGETS)

SRCS = start.s fth_main.s $(wildcard mach_*.s)

clean:
	rm -f $(TARGETS) *.o *.map *.lis

wdcForth: $(SRCS)
	$(AS) $(ASFLAGS) -D 'targ="wdc"' --nostart -o $@ -L $@.lis --map $@.map $<

fth_main.s: fth_main.fs $(RFC)
	$(RFC) --arch ca6502 fth_main.fs > $@ || rm fth_main.s

x16Forth.prg: $(SRCS)
	$(AS) $(ASFLAGS) -D 'targ="x16"' --cbm-prg -o $@ -L $@.lis --map $@.map $<

f256Forth.pgx: $(SRCS)
	$(AS) $(ASFLAGS) -D 'targ="f256"' --c256-pgx -o $@ -L $@.lis --map $@.map $<

f256Forth.pgz: $(SRCS)
	$(AS) $(ASFLAGS) -D 'targ="f256"' --c256-pgz --output-exec=start -o $@ -L $@.lis --map $@.map $<

f256Forth.bin: $(SRCS)
	$(AS) $(ASFLAGS) -D 'targ="f256"' --nostart -o $@ -L $@.lis --map $@.map $<
