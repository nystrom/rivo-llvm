#![feature(custom_attribute, plugin)]
#![plugin(trace)]
#![cfg_attr(test, plugin(quickcheck_macros))]

#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unreachable_code)]

#![feature(nll)]
#![feature(box_patterns)]

extern crate num;
extern crate pretty;
extern crate rpds;
#[macro_use]
extern crate failure;

extern crate string_interner;
#[macro_use]
extern crate lazy_static;

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

#[cfg(test)]
#[macro_use]
extern crate quickcheck;

extern crate llvm_sys;
extern crate libc;
extern crate immix_rust;

#[macro_use]
pub mod macros {
    macro_rules! unsafe_llvm {
        ($e: expr) => {
            {
                let mut guard = crate::llvm::wrappers::LLVM_LOCK.lock().unwrap();
                println!("locked");
                let v = unsafe {
                    $e
                };
                println!("unlocked");
                *guard = ();
                v
            }
        };
    }
}

pub mod common;
pub mod llvm;
pub mod lir;
pub mod mir;
pub mod hir;
pub mod funky;
pub mod gen;
pub mod jit;
