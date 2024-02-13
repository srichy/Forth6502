AS = 64tass -a
ASFLAGS = -C --m65c02
RFC = ../rfc/target/debug/rfc

all: wdcForth

TARGETS = wdcForth
SRCS = start.s forth_main.s

clean:
	rm -f $(TARGETS) *.o *.map *.lis

wdcForth: $(SRCS)
	$(AS) $(ASFLAGS) --s-record -o $@ -L $@.lis --map $@.map $<

fth_main.s: fth_main.fs
	$(RFC) --arch ca6502 fth_main.fs > $@
