use std;
use std::cell::RefCell;
use std::rc::Rc;

use std::collections::HashMap;
use std::collections::HashSet;

use crate::common::names::*;
use crate::hir::trees as hir;
use crate::llvm;
use crate::gen;

// TODO
// Here we use llvm_sys directly rather than using the wrappers.
// We'll correct this later.
use llvm_sys;
use llvm_sys::{core, execution_engine, target};
use std::ffi::CString;
use std::ptr;
use std::mem;
use ::libc::{c_uint, c_ulonglong, c_double, c_void, c_char};

// Built-in functions.
pub extern fn panic() {
    panic!("panic");
}

pub extern fn print_int(x: i64) {
    println!("{}", x);
}

pub extern fn print_char(x: i32) {
    print!("{}", x as u8 as char);
}

// TODO: add missing intrinsics, better I/O, etc.
// pub fn init() {
//     unsafe {
//         // add_symbol("panic", panic as *const ());
//         // add_symbol("print_int", print_int as *const ());
//         // add_symbol("print_char", print_char as *const ());
//     }
// }

pub fn run_main(name: &str, h: &hir::Root) -> Result<i32, String> {
    // Use MCJIT.
    // Would prefer to use Orc, but Rust bindings aren't there yet.
    unsafe { execution_engine::LLVMLinkInMCJIT(); }
    llvm::init();

    let context = llvm::Context::new();
    let module = gen::translate_in_context("main", h, context);

    let pm = unsafe { core::LLVMCreatePassManager() };
    unsafe { crate::llvm_sys::transforms::ipo::LLVMAddFunctionInliningPass(pm); }
    unsafe { crate::llvm_sys::transforms::scalar::LLVMAddInstructionCombiningPass(pm); }
    unsafe { crate::llvm_sys::transforms::scalar::LLVMAddGVNPass(pm); }
    unsafe { crate::llvm_sys::transforms::scalar::LLVMAddTailCallEliminationPass(pm); }
    unsafe { crate::llvm_sys::transforms::scalar::LLVMAddInstructionCombiningPass(pm); }
    // unsafe { crate::llvm_sys::transforms::scalar::LLVMAddCFGSimplificationPass(pm); }

    module.dump();

    unsafe { core::LLVMRunPassManager(pm, module.0); }

    module.dump();

    let mut ee = unsafe { mem::uninitialized() };
    let mut out = unsafe { mem::zeroed() };

    // hand off the module to the EE.
    unsafe {
        execution_engine::LLVMCreateExecutionEngineForModule(&mut ee, module.0, &mut out);
    }

    let cstr = CString::new(name).unwrap();
    let addr = unsafe { execution_engine::LLVMGetFunctionAddress(ee, cstr.as_ptr()) };

    if addr == 0 {
        return Err("main not found".to_string());
    }

    let f: extern "C" fn() -> i32 = unsafe { mem::transmute(addr) };
    let res = f();

    unsafe {
        execution_engine::LLVMDisposeExecutionEngine(ee);
    }

    // Don't dispose of the module. The EE owns it now!
    // module.dispose();

    context.dispose();

    Ok(res)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gen::*;
    use crate::hir::trees as hir;
    use crate::hir::ops::*;
    use crate::common::names::*;

    #[test]
    fn test_fn_returns_0i64() {
        let h = hir::Root {
            defs: vec![
                hir::Def::FunDef {
                    ret_type: hir::Type::I64,
                    name: Name::new("zero"),
                    params: vec![],
                    body: Box::new(
                        hir::Exp::Lit { lit: hir::Lit::I64 { value: 0 } }
                    )
                },

                // main = (i32) fact(10)
                hir::Def::FunDef {
                    ret_type: hir::Type::I32,
                    name: Name::new("main"),
                    params: vec![],
                    body: Box::new(
                        hir::Exp::Unary {
                            op: Uop::Wrap_i64_i32,
                            exp: Box::new(
                                hir::Exp::Call {
                                    fun_type: hir::Type::Fun { ret: Box::new(hir::Type::I64), args: vec![hir::Type::I64] },
                                    name: Name::new("zero"),
                                    args: vec![]
                                }
                            )
                        }
                    )
                },
            ]
        };

        let r = run_main("main", &h);
        assert_eq!(r, Ok(0));
    }

    #[test]
    fn test_fact_i64() {
        let h = hir::Root {
            defs: vec![
                hir::Def::FunDef {
                    ret_type: hir::Type::I64,
                    name: Name::new("fact"),
                    params: vec![
                        hir::Param { ty: hir::Type::I64, name: Name::new("n") }
                    ],
                    body: Box::new(
                        hir::Exp::Seq {
                            body: Box::new(
                                hir::Stm::IfThen {
                                    cond: Box::new(
                                        hir::Exp::Binary {
                                            op: Bop::Eq_i64,
                                            e1: Box::new(
                                                hir::Exp::Var { ty: hir::Type::I64, name: Name::new("n") }
                                            ),
                                            e2: Box::new(
                                                hir::Exp::Lit { lit: hir::Lit::I64 { value: 0 } }
                                            )
                                        }
                                    ),
                                    if_true: Box::new(
                                        hir::Stm::Return {
                                            exp: Box::new(
                                                hir::Exp::Lit { lit: hir::Lit::I64 { value: 1 } }
                                            )
                                        }
                                    ),
                                }
                            ),
                            exp: Box::new(
                                hir::Exp::Binary {
                                    op: Bop::Mul_i64,
                                    e1: Box::new(
                                        hir::Exp::Var { ty: hir::Type::I64, name: Name::new("n") }
                                    ),
                                    e2: Box::new(
                                        hir::Exp::Call {
                                            fun_type: hir::Type::Fun { ret: Box::new(hir::Type::I64), args: vec![hir::Type::I64] },
                                            name: Name::new("fact"),
                                            args: vec![
                                                hir::Exp::Binary {
                                                    op: Bop::Sub_i64,
                                                    e1: Box::new(
                                                        hir::Exp::Var { ty: hir::Type::I64, name: Name::new("n") }
                                                    ),
                                                    e2: Box::new(
                                                        hir::Exp::Lit { lit: hir::Lit::I64 { value: 1 } }
                                                    )
                                                }
                                            ]
                                        }
                                    ),
                                }
                            )
                        }
                    )
                },

                // main = (i32) fact(10)
                hir::Def::FunDef {
                    ret_type: hir::Type::I32,
                    name: Name::new("main"),
                    params: vec![],
                    body: Box::new(
                        hir::Exp::Unary {
                            op: Uop::Wrap_i64_i32,
                            exp: Box::new(
                                hir::Exp::Call {
                                    fun_type: hir::Type::Fun { ret: Box::new(hir::Type::I64), args: vec![hir::Type::I64] },
                                    name: Name::new("fact"),
                                    args: vec![
                                        hir::Exp::Lit { lit: hir::Lit::I64 { value: 10 } }
                                    ]
                                }
                            )
                        }
                    )
                },
            ]
        };

        let r = run_main("main", &h);
        assert_eq!(r, Ok(10*9*8*7*6*5*4*3*2*1));
    }

    #[test]
    fn test_identity_i64() {
        let h = hir::Root {
            defs: vec![
                hir::Def::FunDef {
                    ret_type: hir::Type::I64,
                    name: Name::new("id_i64"),
                    params: vec![
                        hir::Param { ty: hir::Type::I64, name: Name::new("x") }
                    ],
                    body: Box::new(
                        hir::Exp::Var { ty: hir::Type::I64, name: Name::new("x") }
                    )
                },

                // FIXME: functions (and globals) must be declared and added to the module before
                // they can be referenced.

                // main = (i32) id_i64(99)
                hir::Def::FunDef {
                    ret_type: hir::Type::I32,
                    name: Name::new("main"),
                    params: vec![],
                    body: Box::new(
                        hir::Exp::Unary {
                            op: Uop::Wrap_i64_i32,
                            exp: Box::new(
                                hir::Exp::Call {
                                    fun_type: hir::Type::Fun { ret: Box::new(hir::Type::I64), args: vec![hir::Type::I64] },
                                    name: Name::new("id_i64"),
                                    args: vec![
                                        hir::Exp::Lit { lit: hir::Lit::I64 { value: 99 } }
                                    ]
                                }
                            )
                        }
                    )
                },
            ]
        };

        let r = run_main("main", &h);
        assert_eq!(r, Ok(99));
    }
}
