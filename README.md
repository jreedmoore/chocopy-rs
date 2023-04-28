# Overview

This is a Rust implementation of a compiler implementing the student language [ChocoPy](https://chocopy.org). This implementation is independently built and not connected to any particular institution or course.

This implementation is for personal study and is _not_ intended to be maintained.

# Project Layout

The project is laid out across several crates:

- `bin` is a binary that compiles an ChocoPy source file to some target.
- `cc` coordinates all the following steps
- `front` contains the frontend of the compiler: lexer, parser, and semantic analysis
- `middle` contains an intermediate representation and target-independent transformations

- `stack_vm` directly interprets the stack IR in `middle::stack`

# Important Data Structures

The compiler has several important data structures built for the life a compilation unit to some target.

- An iterator of `front::lexer::Token` represents the lexical structure of a file
- A `front::ast::Program` represents a fully parsed program
- A `front::annotated_ast::Program` represents a type checked and slightly simplified program
- A `middle::stack::Program` represents a program as a sequence of operations on a stack machine