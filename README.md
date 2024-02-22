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
  - Switching to using system stack

#### Return Stack
  - This will be the normal stack
  - Switching to using a z-page stack

#### PSP (Parameter Stack Pointer)
  - Zero-page byte
  - Will be just _the_ stack pointer

#### RSP (Return Stack Pointer)
  - Should just _the_ stack pointer
  - Switching to a z-page

#### Control-flow Stack?
  - TBD

#### IP (Instruction Pointer)
  - Zero-page addr (two bytes)

#### W (Word/working "Register")
  - Zero-page addr (two bytes)
  - This will hold the "currently-executing" word and allow
    for a `jmp (W)` (sort-of; not literally)

## Design Doodles

### Rot

|---|--------|-------|
| i | before | after |
|---|--------|-------|
| 3 | x1     | x2    |
| 2 | x2     | x3    |
| 1 | x3     | x1    |
|---|--------|-------|

|---|--------|-------|
| i | before | after |
|---|--------|-------|
| 6 | x1h    | x2h   |
| 5 | x1l    | x2l   |
| 4 | x2h    | x3h   |
| 3 | x2l    | x3l   |
| 2 | x3h    | x1h   |
| 1 | x3l    | x1l   |
|---|--------|-------|

