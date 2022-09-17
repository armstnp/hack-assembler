# hack-assembler

A Rust-based assembler for the Hack language of ['From Nand to Tetris'](https://www.nand2tetris.org/).

Uses [nom](https://github.com/Geal/nom) for parsing instructions-proper. While I had issues getting it to parse the entire input as a single parser, the instruction parsing worked great.

For this kind of task, Rust ends up feeling mostly perfect. A great balance between functional and systems concerns. And I quite appreciated being able to use the precise types I wanted, including a proper unsigned integer; looking at you, JVM.
