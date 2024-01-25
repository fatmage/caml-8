# caml-8

CHIP-8 interpreter and rom debugger written in Ocaml for Functional Programming course at University of Wrocław.

## Installation and running

Dependencies: `tsdl` and `tsdl-mixer`

To run the program with `dune` run ``dune exec caml-8 -- ROM_PATH``

CHIP-8 delay and sound timers run at 60 Hz, default execution speed of caml-8 is set at 5 times that, which is 300 Hz.

To pause/resume execution of the ROM press space, to move backwards/forwards use left and right arrows.

## Sources

ROM collections:

[](https://github.com/kripod/chip8-roms)

CHIP-8 specification based on:

[Timendus' Chip Test Suite](https://github.com/Timendus/chip8-test-suite) and various sources referenced there.

[CHIP‐8 Technical Reference (Matthew Mikolay)](https://github.com/mattmikolay/chip-8/wiki/CHIP%E2%80%908-Technical-Reference)


## TODO

- custom timer/frame ratio passed in argv
