# OAT-Compiler
A complete compiler for OAT, a C-like language.

Included is a typechecker, parser and lexer, compiler from OAT to LLVMlite (a simplified version of LLVM), compiler from LLVMlite to X86lite (a simplified version of X86), assembler for X86lite, and a simulator for X86lite.

Currently the X86lite assembler and simulator is separate (X86-Simulator) and the actual compiler (Compiler) uses clang's builtin backend to assemble and simulate X86.

Made with the assistance of [Dominick DiMaggio](https://github.com/njdom24).
