// MIR is the mid-level IR.
// MIR makes control flow between statements explicit.
// MIR makes memory layout explicit (more-or-less...we defer some decisions to LLVM).
// MIR does no include boxing and unboxing operations. We assume a 64-bit architecture.

pub mod trees;

pub mod ops {
    pub use crate::hir::ops::*;
}

pub mod runtime_api;
