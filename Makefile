AS = 64tass
ASFLAGS = -C --m65c02
RFC = ../rfc/target/debug/rfc

all: wdcForth x16Forth.prg f256Forth.pgx

TARGETS = wdcForth x16Forth.prg
SRCS = start.s fth_main.s

clean:
	rm -f $(TARGETS) *.o *.map *.lis

wdcForth: $(SRCS)
	$(AS) $(ASFLAGS) -D 'targ="wdc"' --nostart -o $@ -L $@.lis --map $@.map $<

fth_main.s: fth_main.fs $(RFC)
	$(RFC) --arch ca6502 fth_main.fs > $@

x16Forth.prg: $(SRCS)
	$(AS) $(ASFLAGS) -D 'targ="x16"' --cbm-prg -o $@ -L $@.lis --map $@.map $<

f256Forth.pgx: $(SRCS)
	$(AS) $(ASFLAGS) -D 'targ="f256"' --c256-pgx -o $@ -L $@.lis --map $@.map $<
