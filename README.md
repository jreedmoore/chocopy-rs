# Overview

This is a Rust implementation of a compiler implementing the student language [ChocoPy](https://chocopy.org). This is implementation is independently built and not connected to any particular institution or course.

This implementation is for personal study and is _not_ intended to be maintained.

# Project Layout

The project is laid out across several crates:

`entrypoint` is a binary that compiles an ChocoPy source file to some target.
`front` contains the frontend of the compiler: lexer, parser, and semantic analysis
`middle` contains an intermediate representation and target-independent optimizations 
`back_wasm` contains a backend emitting [WebAssembly](webassembly.org) intended to run in [WASI](https://wasi.dev/)
`ir_interp` contains an interpreter for the intermediate representation