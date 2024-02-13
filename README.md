# Forth6502

First attempt will be an ITC (Indirect-Threaded Code) Forth model

## Design Notes: WDC65C816 (6502 mode)

### Addresses Reserved by WDCMON

  - Page Zero $00 - $22 unavailable
  - Page Zero $f0 - $ff may be unavailable
  - $7e00 - $7fff unavailable

### Forth Locations

#### Parameter Stack
  - First attempt will be a 32- or 48- depth zero-page stack

#### Return Stack
  - This will be the normal stack

#### PSP (Parameter Stack Pointer)
  - Zero-page byte

#### RSP (Return Stack Pointer)
  - Should just _the_ stack pointer

#### Control-flow Stack?
  - TBD

#### IP (Instruction Pointer)
  - Zero-page addr (two bytes)

#### W (Word/working "Register")
  - Zero-page addr (two bytes)
  - This will hold the "currently-executing" word and allow
    for a `jmp (W)` (sort-of; not literally)
