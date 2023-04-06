# Overview

This is a Rust implementation of a compiler implementing the student language [ChocoPy](https://chocopy.org). This is implementation is independently built and not connected to any particular institution or course.

This implementation is for personal study and is _not_ intended to be maintained.

# Project Layout

The project is laid out across several crates:

- `bin` is a binary that compiles an ChocoPy source file to some target.
- `cc` coordinates all the following steps
- `front` contains the frontend of the compiler: lexer, parser, and semantic analysis
- `middle` contains an intermediate representation and target-independent optimizations 
- `back_wasm` contains a backend emitting [WebAssembly](webassembly.org) intended to run in [WASI](https://wasi.dev/)

# Important Data Structures

The compiler has several important data structures built for the life a compilation unit to some target.

- An iterator of `front::lexer::Token` representing the lexical structure of a file
- A `front::ast::Program` representing a fully parsed program
- A `front::annotated_ast::Program` representing a type checked and slightly simplified program
- A `middle::stack::Program` representing a program as a sequence of operations on a stack machine

In the WASM target this is directly lowered into a WebAssembly text string which is fed into a WASM runtime.