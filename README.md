# LLVM backend for Ivo

This is just an implementation of simple functional high-level IR.

The implementation is based on a simple compiler I wrote for teaching compilers at USI.
This was in turn based on Jens Palsberg's MiniJava compiler.

The compiler consists of three IRs:

- HIR is a simple functional language
- MIR is an expression tree IR
- LIR is a flattened expression tree IR

We perform closure conversion and lambda lifting on the HIR, then generate MIR, then generate LIR, then generate LLVM.
The structure could be simplified by eliminating LIR and generating LLVM directly from MIR, but the current architecture works well enough and I have more confidence in its correctness.

Porting to a different backend should just require changing the translation from LIR to LLVM. The rest should be portable.

The compiler performs minimal optimization, leaving the heavy lifting to LLVM.

This crate just handles compiling to LLVM.
The LLVM code for implementing a JIT for HIR is in another crate (to be written).

