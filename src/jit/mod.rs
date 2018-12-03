use std;
use std::cell::RefCell;
use std::rc::Rc;

use std::collections::HashMap;
use std::collections::HashSet;

use crate::common::names::*;
use crate::gen;
use crate::hir::trees as hir;
use crate::llvm;

// TODO
// Here we use llvm_sys directly rather than using the wrappers.
// We'll correct this later.
use ::libc::{c_char, c_double, c_uint, c_ulonglong, c_void};
use llvm_sys;
use llvm_sys::{core, execution_engine, target, support};
use std::ffi::CString;
use std::mem;
use std::ptr;

// TODO: add missing intrinsics, better I/O, etc.
// pub fn init() {
//     unsafe {
//         // add_symbol("panic", panic as *const ());
//         // add_symbol("print_int", print_int as *const ());
//         // add_symbol("print_char", print_char as *const ());
//     }
// }

// TODO: support multiple modules and lazy loading.

pub struct JITManager {
    context: llvm::Context,
    pm: crate::llvm_sys::prelude::LLVMPassManagerRef,
    roots: Vec<hir::Root>,
    execution_engines: Vec<execution_engine::LLVMExecutionEngineRef>,
}

impl JITManager {
    pub fn new() -> JITManager {
        llvm::init();

        let pm = unsafe { core::LLVMCreatePassManager() };

        unsafe {
            crate::llvm_sys::transforms::ipo::LLVMAddFunctionInliningPass(pm);
            crate::llvm_sys::transforms::scalar::LLVMAddBasicAliasAnalysisPass(pm);
            crate::llvm_sys::transforms::scalar::LLVMAddInstructionCombiningPass(pm);
            crate::llvm_sys::transforms::scalar::LLVMAddReassociatePass(pm);
            crate::llvm_sys::transforms::scalar::LLVMAddGVNPass(pm);
            crate::llvm_sys::transforms::scalar::LLVMAddTailCallEliminationPass(pm);
            crate::llvm_sys::transforms::scalar::LLVMAddInstructionCombiningPass(pm);
        }

        JITManager {
            context: llvm::Context::new(),
            pm: pm,
            roots: vec![],
            execution_engines: vec![],
        }
    }

    pub fn add_module(&mut self, name: &str, h: &hir::Root) {
        // TODO: just add to roots.
        // Compile the module when the function is requested.
        let module = gen::translate_in_context(name, h, self.context);

        // self.modules.push(module);

        unsafe {
            core::LLVMRunPassManager(self.pm, module.0);
        }

        let mut ee = unsafe { mem::zeroed() };
        let mut out = unsafe { mem::zeroed() };

        unsafe {
            execution_engine::LLVMCreateExecutionEngineForModule(&mut ee, module.0, &mut out);
        }

        self.execution_engines.push(ee);
    }

    fn get_function(&self, name: &str) -> Option<extern "C" fn() -> i32> {
        let cstr = CString::new(name).unwrap();

        for ee in &self.execution_engines {
            let addr = unsafe { execution_engine::LLVMGetFunctionAddress(*ee, cstr.as_ptr()) };

            if addr != 0 {
                let f: extern "C" fn() -> i32 = unsafe { mem::transmute(addr) };
                return Some(f);
            }
        }

        None
    }

    pub fn run(&mut self, name: &str) -> Result<i32, String> {
        if let Some(f) = self.get_function(name) {
            let res = f();
            Ok(res)
        } else {
            Err(format!("could not find function {}", name))
        }
    }
}

// Memory management hooks for the JIT.
// Just call libc's malloc.
pub extern "C" fn malloc(bytes: i64) -> *const c_void {
    eprintln!("allocating {} bytes", bytes);
    unsafe {
        ::libc::malloc(bytes as usize) as *const c_void
    }
}

pub extern "C" fn panic() {
    panic!("JIT panicked!");
}

extern "C" {
    fn LLVMAddSymbol(symbolName: *const c_char, symbolValue: *const c_void);
}

pub fn run_main(name: &str, h: &hir::Root) -> Result<i32, String> {
    llvm::init();

    let context = unsafe { llvm::Context(core::LLVMGetGlobalContext()) };
    // let context = llvm::Context::new();

    // // Add the runtime functions.
    // unsafe {
    //     let name = std::ffi::CString::new("malloc").unwrap();
    //     let addr = malloc as *const c_void;
    //     LLVMAddSymbol(name.as_ptr() as *const c_char, addr);
    // }

    let module = gen::translate_in_context("main", h, context);

    let pm = unsafe { core::LLVMCreatePassManager() };

    unsafe {
        crate::llvm_sys::transforms::ipo::LLVMAddFunctionInliningPass(pm);
        crate::llvm_sys::transforms::scalar::LLVMAddBasicAliasAnalysisPass(pm);
        crate::llvm_sys::transforms::scalar::LLVMAddInstructionCombiningPass(pm);
        crate::llvm_sys::transforms::scalar::LLVMAddReassociatePass(pm);
        crate::llvm_sys::transforms::scalar::LLVMAddGVNPass(pm);
        crate::llvm_sys::transforms::scalar::LLVMAddTailCallEliminationPass(pm);
        crate::llvm_sys::transforms::scalar::LLVMAddInstructionCombiningPass(pm);
    }

    // This breaks the control flow. Dunno why.
    // crate::llvm_sys::transforms::scalar::LLVMAddCFGSimplificationPass(pm);

    unsafe {
        core::LLVMRunPassManager(pm, module.0);
        core::LLVMDisposePassManager(pm);
    }

    module.dump();

    let mut ee = unsafe { mem::zeroed() };
    let mut out = unsafe { mem::zeroed() };

    // hand off the module to the EE.
    unsafe {
        execution_engine::LLVMCreateExecutionEngineForModule(&mut ee, module.0, &mut out);
        // target::LLVMSetModuleDataLayout(module.0, execution_engine::LLVMGetExecutionEngineTargetData(ee));
    }

    // Register the runtime functions.
    unsafe {
        support::LLVMAddSymbol(CString::new("malloc").unwrap().as_ptr(), malloc as *mut c_void);
        support::LLVMAddSymbol(CString::new("panic").unwrap().as_ptr(), panic as *mut c_void);
        // execution_engine::LLVMAddGlobalMapping(ee, module.get_named_function("malloc").0, malloc as *mut c_void);
        // execution_engine::LLVMAddGlobalMapping(ee, module.get_named_function("panic").0, panic as *mut c_void);
    }

    // Check if malloc works.
    // {
    //     let cstr = CString::new("malloc").unwrap();
    //     let addr = unsafe { execution_engine::LLVMGetFunctionAddress(ee, cstr.as_ptr()) };
    //     assert_ne!(addr, 0, "address of malloc not found in EE");
    //     assert_eq!(addr as u64, malloc as u64, "address of malloc not as expected, got {:x}, expected {:x}", addr as u64, malloc as u64);
    //     eprintln!("malloc linked");
    //     let f: extern "C" fn(i32) -> u64 = unsafe { mem::transmute(addr) };
    //     f(8);
    // }

    // Check if panic works.
    // {
    //     let cstr = CString::new("panic").unwrap();
    //     let addr = unsafe { execution_engine::LLVMGetGlobalValueAddress(ee, cstr.as_ptr()) };
    //     assert_ne!(addr, 0, "address of panic not found in EE");
    //     unsafe {
    //         assert_eq!(addr as u64, malloc as u64, "address of panic not as expected, got {:x}, expected {:x}", addr as u64, panic as u64);
    //     }
    //     eprintln!("panic linked");
    //     let f: extern "C" fn() -> ! = unsafe { mem::transmute(addr) };
    //     f();
    // }

    // Initialize the module if there's an init_module function.
    {
        let cstr = CString::new("init_module").unwrap();
        let addr = unsafe { execution_engine::LLVMGetFunctionAddress(ee, cstr.as_ptr()) };

        if addr != 0 {
            let f: extern "C" fn() -> i32 = unsafe { mem::transmute(addr) };
            f();
        }
    }

    let cstr = CString::new(name).unwrap();
    let addr = unsafe { execution_engine::LLVMGetFunctionAddress(ee, cstr.as_ptr()) };

    if addr == 0 {
        return Err("main not found".to_string());
    }

    let f: extern "C" fn() -> i32 = unsafe { mem::transmute(addr) };

    // TODO: initialize the heap as an arena.
    // Add hooks for allocation and GC of the heap.
    // Simplest thing is to use BDW collector and just tell it to run within
    // this arena with the stack bottom here.
    // For concurrency, we should support spawning threads and tracing those stacks too.

    // Or: can we integrate with Rust's memory management from the generated code?
    // For instance, all Box are translated to Rc<Box> or Arc<Box>.
    // Add explicit calls to clone and drop (inlined) to increment/decrement the ref count.

    // We can use Gc refs too, writing a custom Trace implementation for Box, allocating
    // in an arena of Box and writing a custom Trace implementation for the stack
    // using generated stack maps.

    let res = f();

    // We're done. Dispose of the EE and the context.
    // Don't dispose of the module. The EE owns it!
    unsafe {
        execution_engine::LLVMDisposeExecutionEngine(ee);
    }

    // context.dispose();

    Ok(res)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::names::*;
    use crate::gen::*;
    use crate::hir::ops::*;
    use crate::hir::trees as hir;

    #[test]
    #[should_panic]
    fn panic() {
        let h = hir::Root {
            defs: vec![
                hir::Def::FunDef {
                    ret_type: hir::Type::Void,
                    name: Name::new("main"),
                    params: vec![],
                    body: Box::new(hir::Exp::Call {
                        fun_type: hir::Type::Fun { ret: Box::new(hir::Type::Void), args: vec![] },
                        name: Name::new("panic"),
                        args: vec![]
                    })
                }
            ]
        };

        let r = run_main("main", &h);
        match r {
            Ok(_) => {},
            Err(_) => assert!(false),
        }
    }

    #[test]
    fn struct_lit_and_load() {
        let h = hir::Root {
            defs: vec![
                hir::Def::FunDef {
                    ret_type: hir::Type::I32,
                    name: Name::new("main"),
                    params: vec![],
                    body: Box::new(
                        hir::Exp::StructLoad {
                            ty: hir::Type::Struct {
                                fields: vec![
                                    hir::Param {
                                        ty: hir::Type::I32,
                                        name: Name::new("f"),
                                    }
                                ]
                            },
                            base: Box::new(hir::Exp::StructLit {
                                fields: vec![
                                    hir::Field {
                                        param: hir::Param {
                                            ty: hir::Type::I32,
                                            name: Name::new("f"),
                                        },
                                        exp: Box::new(hir::Exp::Lit { lit: hir::Lit::I32 { value: 3 }}),
                                    }
                                ]
                            }),
                            field: Name::new("f"),
                        }
                    ),
                }
            ]
        };

        let r = run_main("main", &h);
        assert_eq!(r, Ok(3));
    }

    #[test]
    fn fn_returns_0i64() {
        let h = hir::Root {
            defs: vec![
                hir::Def::FunDef {
                    ret_type: hir::Type::I64,
                    name: Name::new("zero"),
                    params: vec![],
                    body: Box::new(hir::Exp::Lit {
                        lit: hir::Lit::I64 { value: 0 },
                    }),
                },
                // main = (i32) fact(10)
                hir::Def::FunDef {
                    ret_type: hir::Type::I32,
                    name: Name::new("main"),
                    params: vec![],
                    body: Box::new(hir::Exp::Unary {
                        op: Uop::Wrap_i64_i32,
                        exp: Box::new(hir::Exp::Call {
                            fun_type: hir::Type::Fun {
                                ret: Box::new(hir::Type::I64),
                                args: vec![],
                            },
                            name: Name::new("zero"),
                            args: vec![],
                        }),
                    }),
                },
            ],
        };

        let r = run_main("main", &h);
        assert_eq!(r, Ok(0));
    }

    #[test]
    fn fact_i64() {
        let h = hir::Root {
            defs: vec![
                hir::Def::FunDef {
                    ret_type: hir::Type::I64,
                    name: Name::new("fact"),
                    params: vec![hir::Param {
                        ty: hir::Type::I64,
                        name: Name::new("n"),
                    }],
                    body: Box::new(hir::Exp::Seq {
                        body: Box::new(hir::Stm::IfThen {
                            cond: Box::new(hir::Exp::Binary {
                                op: Bop::Eq_i64,
                                e1: Box::new(hir::Exp::Var {
                                    ty: hir::Type::I64,
                                    name: Name::new("n"),
                                }),
                                e2: Box::new(hir::Exp::Lit {
                                    lit: hir::Lit::I64 { value: 0 },
                                }),
                            }),
                            if_true: Box::new(hir::Stm::Return {
                                exp: Box::new(hir::Exp::Lit {
                                    lit: hir::Lit::I64 { value: 1 },
                                }),
                            }),
                        }),
                        exp: Box::new(hir::Exp::Binary {
                            op: Bop::Mul_i64,
                            e1: Box::new(hir::Exp::Var {
                                ty: hir::Type::I64,
                                name: Name::new("n"),
                            }),
                            e2: Box::new(hir::Exp::Call {
                                fun_type: hir::Type::Fun {
                                    ret: Box::new(hir::Type::I64),
                                    args: vec![hir::Type::I64],
                                },
                                name: Name::new("fact"),
                                args: vec![hir::Exp::Binary {
                                    op: Bop::Sub_i64,
                                    e1: Box::new(hir::Exp::Var {
                                        ty: hir::Type::I64,
                                        name: Name::new("n"),
                                    }),
                                    e2: Box::new(hir::Exp::Lit {
                                        lit: hir::Lit::I64 { value: 1 },
                                    }),
                                }],
                            }),
                        }),
                    }),
                },
                // main = (i32) fact(10)
                hir::Def::FunDef {
                    ret_type: hir::Type::I32,
                    name: Name::new("main"),
                    params: vec![],
                    body: Box::new(hir::Exp::Unary {
                        op: Uop::Wrap_i64_i32,
                        exp: Box::new(hir::Exp::Call {
                            fun_type: hir::Type::Fun {
                                ret: Box::new(hir::Type::I64),
                                args: vec![hir::Type::I64],
                            },
                            name: Name::new("fact"),
                            args: vec![hir::Exp::Lit {
                                lit: hir::Lit::I64 { value: 10 },
                            }],
                        }),
                    }),
                },
            ],
        };

        let mut jit = JITManager::new();
        jit.add_module("main", &h);

        if let Some(f) = jit.get_function("main") {
            let r = f();
            assert_eq!(r, 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1);
        }

        // let r = run_main("main", &h);
        // assert_eq!(r, Ok(10*9*8*7*6*5*4*3*2*1));
    }

    #[test]
    fn fact_i64_with_forward_reference() {
        let h = hir::Root {
            defs: vec![
                // main = (i32) fact(10)
                hir::Def::FunDef {
                    ret_type: hir::Type::I32,
                    name: Name::new("main"),
                    params: vec![],
                    body: Box::new(hir::Exp::Unary {
                        op: Uop::Wrap_i64_i32,
                        exp: Box::new(hir::Exp::Call {
                            fun_type: hir::Type::Fun {
                                ret: Box::new(hir::Type::I64),
                                args: vec![hir::Type::I64],
                            },
                            name: Name::new("fact"),
                            args: vec![hir::Exp::Lit {
                                lit: hir::Lit::I64 { value: 10 },
                            }],
                        }),
                    }),
                },
                hir::Def::FunDef {
                    ret_type: hir::Type::I64,
                    name: Name::new("fact"),
                    params: vec![hir::Param {
                        ty: hir::Type::I64,
                        name: Name::new("n"),
                    }],
                    body: Box::new(hir::Exp::Seq {
                        body: Box::new(hir::Stm::IfThen {
                            cond: Box::new(hir::Exp::Binary {
                                op: Bop::Eq_i64,
                                e1: Box::new(hir::Exp::Var {
                                    ty: hir::Type::I64,
                                    name: Name::new("n"),
                                }),
                                e2: Box::new(hir::Exp::Lit {
                                    lit: hir::Lit::I64 { value: 0 },
                                }),
                            }),
                            if_true: Box::new(hir::Stm::Return {
                                exp: Box::new(hir::Exp::Lit {
                                    lit: hir::Lit::I64 { value: 1 },
                                }),
                            }),
                        }),
                        exp: Box::new(hir::Exp::Binary {
                            op: Bop::Mul_i64,
                            e1: Box::new(hir::Exp::Var {
                                ty: hir::Type::I64,
                                name: Name::new("n"),
                            }),
                            e2: Box::new(hir::Exp::Call {
                                fun_type: hir::Type::Fun {
                                    ret: Box::new(hir::Type::I64),
                                    args: vec![hir::Type::I64],
                                },
                                name: Name::new("fact"),
                                args: vec![hir::Exp::Binary {
                                    op: Bop::Sub_i64,
                                    e1: Box::new(hir::Exp::Var {
                                        ty: hir::Type::I64,
                                        name: Name::new("n"),
                                    }),
                                    e2: Box::new(hir::Exp::Lit {
                                        lit: hir::Lit::I64 { value: 1 },
                                    }),
                                }],
                            }),
                        }),
                    }),
                },
            ],
        };

        let r = run_main("main", &h);
        assert_eq!(r, Ok(10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1));
    }

    #[test]
    fn identity_i64() {
        let h = hir::Root {
            defs: vec![
                hir::Def::FunDef {
                    ret_type: hir::Type::I64,
                    name: Name::new("id_i64"),
                    params: vec![hir::Param {
                        ty: hir::Type::I64,
                        name: Name::new("x"),
                    }],
                    body: Box::new(hir::Exp::Var {
                        ty: hir::Type::I64,
                        name: Name::new("x"),
                    }),
                },
                // main = (i32) id_i64(99)
                hir::Def::FunDef {
                    ret_type: hir::Type::I32,
                    name: Name::new("main"),
                    params: vec![],
                    body: Box::new(hir::Exp::Unary {
                        op: Uop::Wrap_i64_i32,
                        exp: Box::new(hir::Exp::Call {
                            fun_type: hir::Type::Fun {
                                ret: Box::new(hir::Type::I64),
                                args: vec![hir::Type::I64],
                            },
                            name: Name::new("id_i64"),
                            args: vec![hir::Exp::Lit {
                                lit: hir::Lit::I64 { value: 99 },
                            }],
                        }),
                    }),
                },
            ],
        };

        let r = run_main("main", &h);
        assert_eq!(r, Ok(99));
    }

    #[test]
    fn church_true() {
        let h = hir::Root {
            defs: vec![
                // var true = \x -> \y -> x
                hir::Def::VarDef {
                    ty: hir::Type::Fun {
                        ret: Box::new(hir::Type::Fun {
                            ret: Box::new(hir::Type::I32),
                            args: vec![hir::Type::I32],
                        }),
                        args: vec![hir::Type::I32],
                    },
                    name: Name::new("true"),
                    exp: Box::new(hir::Exp::Lambda {
                        ret_type: hir::Type::Fun {
                            ret: Box::new(hir::Type::I32),
                            args: vec![hir::Type::I32],
                        },
                        params: vec![hir::Param {
                            name: Name::new("x"),
                            ty: hir::Type::I32,
                        }],
                        body: Box::new(hir::Exp::Lambda {
                            ret_type: hir::Type::I32,
                            params: vec![hir::Param {
                                name: Name::new("y"),
                                ty: hir::Type::I32,
                            }],
                            body: Box::new(hir::Exp::Var {
                                name: Name::new("x"),
                                ty: hir::Type::I32,
                            }),
                        }),
                    }),
                },
                hir::Def::FunDef {
                    ret_type: hir::Type::I32,
                    name: Name::new("main"),
                    params: vec![],
                    body: Box::new(hir::Exp::Apply {
                        fun_type: hir::Type::Fun {
                            ret: Box::new(hir::Type::I32),
                            args: vec![hir::Type::I32],
                        },
                        fun: Box::new(hir::Exp::Apply {
                            fun_type: hir::Type::Fun {
                                ret: Box::new(hir::Type::Fun {
                                    ret: Box::new(hir::Type::I32),
                                    args: vec![hir::Type::I32],
                                }),
                                args: vec![hir::Type::I32],
                            },
                            fun: Box::new(hir::Exp::Global {
                                name: Name::new("true"),
                                ty: hir::Type::Fun {
                                    ret: Box::new(hir::Type::Fun {
                                        ret: Box::new(hir::Type::I32),
                                        args: vec![hir::Type::I32],
                                    }),
                                    args: vec![hir::Type::I32],
                                },
                            }),
                            args: vec![hir::Exp::Lit {
                                lit: hir::Lit::I32 { value: 99 },
                            }],
                        }),
                        args: vec![hir::Exp::Lit {
                            lit: hir::Lit::I32 { value: 77 },
                        }],
                    }),
                },
            ],
        };

        let r = run_main("main", &h);
        assert_eq!(r, Ok(99));
    }

    #[test]
    fn church_false() {
        let h = hir::Root {
            defs: vec![
                // var false = \x -> \y -> y
                hir::Def::VarDef {
                    ty: hir::Type::Fun {
                        ret: Box::new(hir::Type::Fun {
                            ret: Box::new(hir::Type::I32),
                            args: vec![hir::Type::I32],
                        }),
                        args: vec![hir::Type::I32],
                    },
                    name: Name::new("false"),
                    exp: Box::new(hir::Exp::Lambda {
                        ret_type: hir::Type::Fun {
                            ret: Box::new(hir::Type::I32),
                            args: vec![hir::Type::I32],
                        },
                        params: vec![hir::Param {
                            name: Name::new("x"),
                            ty: hir::Type::I32,
                        }],
                        body: Box::new(hir::Exp::Lambda {
                            ret_type: hir::Type::I32,
                            params: vec![hir::Param {
                                name: Name::new("y"),
                                ty: hir::Type::I32,
                            }],
                            body: Box::new(hir::Exp::Var {
                                name: Name::new("y"),
                                ty: hir::Type::I32,
                            }),
                        }),
                    }),
                },
                // main = false(77)(99)
                hir::Def::FunDef {
                    ret_type: hir::Type::I32,
                    name: Name::new("main"),
                    params: vec![],
                    body: Box::new(hir::Exp::Apply {
                        fun_type: hir::Type::Fun {
                            ret: Box::new(hir::Type::I32),
                            args: vec![hir::Type::I32],
                        },
                        fun: Box::new(hir::Exp::Apply {
                            fun_type: hir::Type::Fun {
                                ret: Box::new(hir::Type::Fun {
                                    ret: Box::new(hir::Type::I32),
                                    args: vec![hir::Type::I32],
                                }),
                                args: vec![hir::Type::I32],
                            },
                            fun: Box::new(hir::Exp::Global {
                                name: Name::new("false"),
                                ty: hir::Type::Fun {
                                    ret: Box::new(hir::Type::Fun {
                                        ret: Box::new(hir::Type::I32),
                                        args: vec![hir::Type::I32],
                                    }),
                                    args: vec![hir::Type::I32],
                                },
                            }),
                            args: vec![hir::Exp::Lit {
                                lit: hir::Lit::I32 { value: 77 },
                            }],
                        }),
                        args: vec![hir::Exp::Lit {
                            lit: hir::Lit::I32 { value: 99 },
                        }],
                    }),
                },
            ],
        };

        let r = run_main("main", &h);
        assert_eq!(r, Ok(99));
    }
}
