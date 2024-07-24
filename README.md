# Forth6502

THIS IS A WORK IN PROGRESS.  Currently, the 65816 version will _not_
work on the Foenix F256K.  It will run a loop showing useless debug
about the key scans.

Filesystem support is functional (at least for reading files) on the
F256K 6502 version.

# Building

You need https://github.com/srichy/rfc to build this, as well as
64tass.  rfc should be checked out in a directory at the same level as
this repository.

All The Things need to be cleaned up and better organized.  TODO.

## Design Notes: TBD

I removed the old ones.  They were hugely out-of-date.

## Design Notes: WDC65C816 (6502 mode)

### Addresses Reserved by WDCMON

  - Page Zero $00 - $22 unavailable
  - Page Zero $f0 - $ff may be unavailable
  - $7e00 - $7fff unavailable

# Credits and References

I can't recommend enough
[Brad Rodriguez'](http://www.bradrodriguez.com/papers/) writings on Forth
internals.

Also, I have to chase down where I got the number parsing code.  If
you recognize it, let me know and I'll add the credit or remove it and
rewrite it or whatever.
