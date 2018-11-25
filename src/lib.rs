#![feature(custom_attribute, plugin)]
#![plugin(trace)]
#![cfg_attr(test, plugin(quickcheck_macros))]

#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unreachable_code)]
// #![allow(unused_must_use)]

#![feature(duration_as_u128)]
#![feature(vec_resize_default)]
#![feature(vec_remove_item)]
#![feature(nll)]

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




pub mod hir;
pub mod mir;
pub mod lir;
pub mod llvm;
pub mod gen;
pub mod common;
