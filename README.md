# C7 - toy programming language compiler

C7 (compiler #7) - hobby project to learn compiler development end-to-end. Previous 6 compilers are unfinished and were abandoned.

Compiler features:
* There are two Linux backends: nasm and own x64.
* Produced binaries are staticly linked and do not depend on libc.
* At the moment depends on external ld linker.

Language features:
* Language is functional-ish(inspired by erlang/elixir), staticly typed and compiled.
* There is support library written in C to implement things that are hard to do in intermediate representation, like conversion of floating point number to string (Thank you Sean Barret for stb libraries).
* Syscall directive to interact with Linux kernel.
* 64 bit integers, double precision floating point numbers, booleans, strings, lists, tuples, atoms.
* Passing functions as value.
* Tail call optimization.
* Closures.
* Polymorphic functions.
* Stacktrace with file, line numbers and function name.
