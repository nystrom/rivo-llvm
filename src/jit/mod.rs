use crate::macros::*;

// TODO
// Here we use llvm_sys directly rather than using the wrappers.
// We'll correct this later.
use ::libc::{c_char, c_double, c_uint, c_ulonglong, c_void};
use llvm_sys;
use llvm_sys::{core, execution_engine, target, support};
use std::ffi::CString;
use std::mem;
use std::ptr;

use std::collections::HashMap;
use std::collections::HashSet;

use crate::common::names::*;
use crate::gen;
use crate::hir::trees as hir;
use crate::llvm;

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
    pm: llvm_sys::prelude::LLVMPassManagerRef,
    roots: Vec<hir::Root>,
    execution_engines: Vec<execution_engine::LLVMExecutionEngineRef>,
}

impl JITManager {
    pub fn new() -> JITManager {
        llvm::init();

        let pm = unsafe_llvm!( core::LLVMCreatePassManager() );

        unsafe_llvm!( {
            llvm_sys::transforms::ipo::LLVMAddFunctionInliningPass(pm);
            llvm_sys::transforms::scalar::LLVMAddBasicAliasAnalysisPass(pm);
            llvm_sys::transforms::scalar::LLVMAddInstructionCombiningPass(pm);
            llvm_sys::transforms::scalar::LLVMAddReassociatePass(pm);
            llvm_sys::transforms::scalar::LLVMAddGVNPass(pm);
            llvm_sys::transforms::scalar::LLVMAddTailCallEliminationPass(pm);
            llvm_sys::transforms::scalar::LLVMAddInstructionCombiningPass(pm);
        });

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

        unsafe_llvm!(
            core::LLVMRunPassManager(self.pm, module.0)
        );

        let mut ee = unsafe { mem::zeroed() };
        let mut out = unsafe { mem::zeroed() };

        unsafe_llvm!( execution_engine::LLVMCreateExecutionEngineForModule(&mut ee, module.0, &mut out) );

        self.execution_engines.push(ee);
    }

    fn get_function(&self, name: &str) -> Option<extern "C" fn() -> i32> {
        let cstr = CString::new(name).unwrap();

        for ee in &self.execution_engines {
            let addr = unsafe_llvm!( execution_engine::LLVMGetFunctionAddress(*ee, cstr.as_ptr()) );

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

// Memory management hooks.
#[cfg(not(feature = "immix"))]
mod gc {
    use ::libc::c_void;

    pub fn init() { }

    // Just call libc's malloc.
    pub extern "C" fn malloc(bytes: i64) -> *const c_void {
        eprintln!("old_malloc: allocating {} bytes", bytes);
        unsafe {
            ::libc::malloc(bytes as usize) as *const c_void
        }
    }

    pub extern "C" fn yieldpoint() { }
}

#[cfg(feature = "immix")]
mod gc {
    use immix_rust as immix;
    use ::libc::c_void;
    use std::sync::Once;

    static INIT: Once = Once::new();

    pub struct Mutator(Box<immix::Mutator>);

    impl Mutator {
        pub fn new() -> Mutator {
            Mutator(immix::new_mutator())
        }

        pub fn alloc(&mut self, size: usize, align: usize) -> immix::common::ObjectReference {
            if size <= 256 {
                self.alloc_small(size, align)
            }
            else {
                self.alloc_large(size)
            }
        }

        pub fn alloc_small(&mut self, size: usize, align: usize) -> immix::common::ObjectReference {
            assert!(size <= 256);
            immix::alloc(&mut self.0, size, align)
        }

        pub fn alloc_large(&mut self, size: usize) -> immix::common::ObjectReference {
            immix::alloc_large(&mut self.0, size)
        }

        pub fn yieldpoint(&mut self) {
            immix::yieldpoint(&mut self.0)
        }
    }

    #[cfg(test)]
    const HEAP_SIZE: usize = 1024*256;
    #[cfg(not(test))]
    const HEAP_SIZE: usize = 1024*1024*256;
    const LARGE_OBJECT_SIZE: usize = 1024*1024*256;

    pub fn init() {
        INIT.call_once(|| {
            immix::gc_init(HEAP_SIZE, LARGE_OBJECT_SIZE, 1);
            println!("Immix initialized!");
        });

        self::MUTATOR.with(|m| {
            let mut r = m.borrow_mut();
            if let None = *r {
                *r = Some(self::Mutator::new());
                println!("Mutator initialized!");
            }
        });
    }

    pub extern "C" fn malloc(bytes: i64) -> *const c_void {
        eprintln!("malloc: allocating {} bytes", bytes);

        let oref = self::MUTATOR.with(|m| {
            if let Some(s) = m.borrow_mut().as_mut() {
                s.alloc(bytes as usize, 16)
            }
            else {
                panic!("GC mutator not set for this thread")
            }
        });

        // eprintln!("ptr = {:x}", oref.value());
        oref.value() as *const c_void
    }

    pub extern "C" fn yieldpoint() {
        // eprintln!("yieldpoint");
        self::MUTATOR.with(|m| {
            if let Some(s) = m.borrow_mut().as_mut() {
                s.yieldpoint()
            }
        });
    }


    // Create a thread-local cell for the mutator.
    use std::cell::RefCell;
    thread_local! {
        pub static MUTATOR: RefCell<Option<Mutator>> = RefCell::new(None);
    }
}

pub extern "C" fn panic() {
    panic!("JIT panicked!");
}

extern "C" {
    fn LLVMAddSymbol(symbolName: *const c_char, symbolValue: *const c_void);
}

pub fn run_main(name: &str, h: &hir::Root) -> Result<i32, String> {
    // let context = llvm::Context::global();
    let context = llvm::Context::new();

    // // Add the runtime functions.
    // unsafe {
    //     let name = std::ffi::CString::new("malloc").unwrap();
    //     let addr = malloc as *const c_void;
    //     LLVMAddSymbol(name.as_ptr() as *const c_char, addr);
    // }

    eprintln!("{}: about to translate", name);
    let module = gen::translate_in_context(name, h, context);
    eprintln!("{}: translated", name);

    if cfg!(feature = "optimize") {
        unsafe_llvm!( {
            let pm = core::LLVMCreatePassManager();

            llvm_sys::transforms::ipo::LLVMAddFunctionInliningPass(pm);
            llvm_sys::transforms::scalar::LLVMAddBasicAliasAnalysisPass(pm);
            llvm_sys::transforms::scalar::LLVMAddInstructionCombiningPass(pm);
            llvm_sys::transforms::scalar::LLVMAddReassociatePass(pm);
            llvm_sys::transforms::scalar::LLVMAddGVNPass(pm);
            llvm_sys::transforms::scalar::LLVMAddTailCallEliminationPass(pm);
            llvm_sys::transforms::scalar::LLVMAddInstructionCombiningPass(pm);

            // This breaks the control flow. Dunno why.
            // llvm_sys::transforms::scalar::LLVMAddCFGSimplificationPass(pm);

            core::LLVMRunPassManager(pm, module.0);
            core::LLVMDisposePassManager(pm);
        });

        eprintln!("{}: optimized", name);
    }

    module.dump();

    let mut ee = unsafe { mem::zeroed() };
    let mut out = unsafe { mem::zeroed() };

    // hand off the module to the EE.
    unsafe_llvm!( execution_engine::LLVMCreateExecutionEngineForModule(&mut ee, module.0, &mut out) );

    // Register the runtime functions.
    unsafe_llvm!({
        support::LLVMAddSymbol(CString::new("panic").unwrap().as_ptr(), panic as *mut c_void);
        support::LLVMAddSymbol(CString::new("malloc").unwrap().as_ptr(), gc::malloc as *mut c_void);
        support::LLVMAddSymbol(CString::new("yieldpoint").unwrap().as_ptr(), gc::yieldpoint as *mut c_void);
    });

    // Initialize the module if there's an init_module function.
    let init = {
        let cstr = CString::new("init_module").unwrap();
        let addr = unsafe_llvm!( execution_engine::LLVMGetFunctionAddress(ee, cstr.as_ptr()) );

        if addr != 0 {
            let f: extern "C" fn() -> c_void = unsafe { mem::transmute(addr) };
            Some(f)
        }
        else {
            None
        }
    };

    let main = {
        let cstr = CString::new(name).unwrap();
        let addr = unsafe_llvm!( execution_engine::LLVMGetFunctionAddress(ee, cstr.as_ptr()) );

        if addr == 0 {
            return Err("main not found".to_string());
        }

        let f: extern "C" fn() -> i32 = unsafe { mem::transmute(addr) };
        f
    };

    use self::gc;

    // Run the module initializer and main.
    // Put in another function so we can set the low water mark for the GC.
    fn init_and_run(init: Option<extern "C" fn() -> c_void>, main: extern "C" fn() -> i32) -> i32 {
        use immix_rust as immix;

        if cfg!(feature = "immix") {
            unsafe {
                immix::set_low_water_mark();
            }
        }

        match init {
            Some(f) => { f(); },
            None => {},
        }

        main()
    }

    eprintln!("{}: about to init gc", name);
    gc::init();
    eprintln!("{}: about to run", name);

    let res = init_and_run(init, main);
    eprintln!("{}: finished run", name);

    gc::yieldpoint();

    unsafe_llvm!( execution_engine::LLVMDisposeExecutionEngine(ee) );

    context.dispose();

    Ok(res)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::names::*;
    use crate::gen::*;
    use crate::hir::ops::*;
    use crate::hir::trees as hir;

    // #[test]
    // fn print_env() {
    //     use std::env;
    //
    //     for (key, value) in env::vars() {
    //         println!("{}='{}'", key, value);
    //     }
    // }

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
                // main = (i32) zero()
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
    fn while_loop() {
        let h = hir::Root {
            defs: vec![
                hir::Def::FunDef {
                    ret_type: hir::Type::I32,
                    name: Name::new("loop"),
                    params: vec![],
                    body: Box::new(hir::Exp::Seq {
                        body: Box::new(
                            hir::Stm::Block {
                                body: vec![
                                    hir::Stm::Assign {
                                        ty: hir::Type::I32,
                                        lhs: Name::new("i"),
                                        rhs: Box::new(hir::Exp::Lit {
                                            lit: hir::Lit::I32 { value: 0 },
                                        }),
                                    },
                                    hir::Stm::While {
                                        cond: Box::new(hir::Exp::Binary {
                                                op: Bop::Lt_u_i32,
                                                e1: Box::new(hir::Exp::Var {
                                                    name: Name::new("i"),
                                                    ty: hir::Type::I32,
                                                }),
                                                e2: Box::new(hir::Exp::Lit { lit: hir::Lit::I32 { value: 1000000 }, }),
                                            }
                                        ),
                                        body: Box::new(
                                            hir::Stm::Assign {
                                                ty: hir::Type::I32,
                                                lhs: Name::new("i"),
                                                rhs: Box::new(hir::Exp::Binary {
                                                        op: Bop::Add_i32,
                                                        e1: Box::new(hir::Exp::Var {
                                                            name: Name::new("i"),
                                                            ty: hir::Type::I32,
                                                        }),
                                                        e2: Box::new(hir::Exp::Lit { lit: hir::Lit::I32 { value: 1 }, }),
                                                    }
                                                ),
                                            }
                                        ),
                                    }
                                ]
                            }
                        ),
                        exp: Box::new(hir::Exp::Var {
                            name: Name::new("i"),
                            ty: hir::Type::I32,
                        }),
                    }),
                },
                // main = loop()
                hir::Def::FunDef {
                    ret_type: hir::Type::I32,
                    name: Name::new("main"),
                    params: vec![],
                    body: Box::new(hir::Exp::Call {
                        fun_type: hir::Type::Fun {
                            ret: Box::new(hir::Type::I32),
                            args: vec![],
                        },
                        name: Name::new("loop"),
                        args: vec![],
                    }),
                },
            ],
        };

        let r = run_main("main", &h);
        assert_eq!(r, Ok(1000000));
    }

    #[test]
    fn alloc_in_loop() {
        let h = hir::Root {
            defs: vec![
                hir::Def::FunDef {
                    ret_type: hir::Type::I32,
                    name: Name::new("loop"),
                    params: vec![],
                    body: Box::new(hir::Exp::Seq {
                        body: Box::new(
                            hir::Stm::Block {
                                body: vec![
                                    hir::Stm::Assign {
                                        ty: hir::Type::I32,
                                        lhs: Name::new("i"),
                                        rhs: Box::new(hir::Exp::Lit {
                                            lit: hir::Lit::I32 { value: 0 },
                                        }),
                                    },
                                    hir::Stm::While {
                                        cond: Box::new(hir::Exp::Binary {
                                                op: Bop::Lt_u_i32,
                                                e1: Box::new(hir::Exp::Var {
                                                    name: Name::new("i"),
                                                    ty: hir::Type::I32,
                                                }),
                                                e2: Box::new(hir::Exp::Lit { lit: hir::Lit::I32 { value: 1000000 }, }),
                                            }
                                        ),
                                        body: Box::new(
                                            hir::Stm::Assign {
                                                ty: hir::Type::I32,
                                                lhs: Name::new("i"),
                                                rhs: Box::new(hir::Exp::Binary {
                                                        op: Bop::Add_i32,
                                                        e1: Box::new(hir::Exp::Var {
                                                            name: Name::new("i"),
                                                            ty: hir::Type::I32,
                                                        }),
                                                        e2: Box::new(hir::Exp::Call {
                                                            fun_type: hir::Type::Fun {
                                                                ret: Box::new(hir::Type::I32),
                                                                args: vec![],
                                                            },
                                                            name: Name::new("one"),
                                                            args: vec![],
                                                        }),
                                                    }
                                                ),
                                            }
                                        ),
                                    }
                                ]
                            }
                        ),
                        exp: Box::new(hir::Exp::Var {
                            name: Name::new("i"),
                            ty: hir::Type::I32,
                        }),
                    }),
                },
                // returns 1, but allocating a struct.
                // { f: 1 }.f
                hir::Def::FunDef {
                    ret_type: hir::Type::I32,
                    name: Name::new("one"),
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
                                        exp: Box::new(hir::Exp::Lit { lit: hir::Lit::I32 { value: 1 }}),
                                    }
                                ]
                            }),
                            field: Name::new("f"),
                        }
                    ),
                },

                // main = loop()
                hir::Def::FunDef {
                    ret_type: hir::Type::I32,
                    name: Name::new("main"),
                    params: vec![],
                    body: Box::new(hir::Exp::Call {
                        fun_type: hir::Type::Fun {
                            ret: Box::new(hir::Type::I32),
                            args: vec![],
                        },
                        name: Name::new("loop"),
                        args: vec![],
                    }),
                },
            ],
        };

        let r = run_main("main", &h);
        assert_eq!(r, Ok(1000000));
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
