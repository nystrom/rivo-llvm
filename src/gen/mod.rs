mod mir_gen;
mod lir_gen;
mod llvm_gen;

use crate::hir::trees as hir;
use crate::lir::trees as lir;
use crate::llvm;

pub fn translate(name: &str, h: &hir::Root) -> llvm::Module {
    println!("HIR {:#?}", h);

    let m = mir_gen::Translate::translate(h);
    println!("MIR {:#?}", m);

    let l = lir_gen::Translate::translate(&m);
    println!("LIR {:#?}", l);

    let v = llvm_gen::Translate::new().translate(name, &l);
    v.dump();

    v
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
    use crate::common::names::*;

    #[test]
    fn test_fn_returns_0i64() {
        let h = hir::Root {
            defs: vec![
                hir::Def::FunDef {
                    ty: hir::Type::I64,
                    name: Name::new("zero"),
                    params: vec![],
                    body: Box::new(
                        hir::Exp::Lit { lit: hir::Lit::I64 { value: 0 } }
                    )
                }
            ]
        };

        let m = translate("test_fn_returns_0i64", &h);
        m.dump();
    }

    #[test]
    fn test_identity_i64() {
        let h = hir::Root {
            defs: vec![
                hir::Def::FunDef {
                    ty: hir::Type::I64,
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

        let m = translate("test_identity_i64", &h);
        m.dump();
    }
}
