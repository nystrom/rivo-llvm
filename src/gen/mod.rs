mod mir_gen;
mod lir_gen;
mod llvm_gen;
mod runtime_api;

use crate::hir::trees as hir;
use crate::lir::trees as lir;
use crate::llvm;

pub fn translate_in_context(name: &str, h: &hir::Root, context: llvm::Context) -> llvm::Module {
    let l = translate_lir(name, h);

    let t = llvm_gen::Translate::new_in_context(context);
    let m = t.translate(name, &l);
    m.dump();

    m
}

pub fn translate_lir(name: &str, h: &hir::Root) -> lir::Root {
    println!("HIR {:#?}", h);

    let m = mir_gen::Translate::translate(h);
    println!("MIR {:#?}", m);

    let l = lir_gen::Translate::translate(&m);
    println!("LIR {:#?}", l);

    l
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hir::trees as hir;
    use crate::hir::ops::*;
    use crate::common::names::*;

    #[test]
    fn fn_returns_0i64() {
        let h = hir::Root {
            defs: vec![
                hir::Def::FunDef {
                    ret_type: hir::Type::I64,
                    name: Name::new("zero"),
                    params: vec![],
                    body: Box::new(
                        hir::Exp::Lit { lit: hir::Lit::I64 { value: 0 } }
                    )
                }
            ]
        };

        let context = unsafe { llvm::Context(crate::llvm_sys::core::LLVMGetGlobalContext()) };
        translate_in_context("test_fn_returns_0i64", &h, context);
        context.dispose();
    }

    #[test]
    fn fact_i64() {
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
                }
            ]
        };

        let context = unsafe { llvm::Context(crate::llvm_sys::core::LLVMGetGlobalContext()) };
        translate_in_context("test_fact_i64", &h, context);
        context.dispose();
    }

    #[test]
    fn identity_i64() {
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
                }
            ]
        };

        let context = unsafe { llvm::Context(crate::llvm_sys::core::LLVMGetGlobalContext()) };
        translate_in_context("test_identity_i64", &h, context);
        context.dispose();
    }

    #[test]
    fn struct_lit_and_load() {
        let h = hir::Root {
            defs: vec![
                hir::Def::FunDef {
                    ret_type: hir::Type::I64,
                    name: Name::new("three_from_struct"),
                    params: vec![],
                    body: Box::new(
                        hir::Exp::StructLoad {
                            ty: hir::Type::Struct {
                                fields: vec![
                                    hir::Param {
                                        ty: hir::Type::I64,
                                        name: Name::new("f"),
                                    }
                                ]
                            },
                            base: Box::new(hir::Exp::StructLit {
                                fields: vec![
                                    hir::Field {
                                        param: hir::Param {
                                            ty: hir::Type::I64,
                                            name: Name::new("f"),
                                        },
                                        exp: Box::new(hir::Exp::Lit { lit: hir::Lit::I64 { value: 3 }}),
                                    }
                                ]
                            }),
                            field: Name::new("f"),
                        }
                    ),
                }
            ]
        };

        let context = unsafe { llvm::Context(crate::llvm_sys::core::LLVMGetGlobalContext()) };
        translate_in_context("test_struct_load", &h, context);
        context.dispose();
    }
}
