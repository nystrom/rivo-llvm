/// Closure conversion for HIR
/// We translate into HIR/CC, then lambda lift, producing HIR again (but without lambdas).

use rpds::HashTrieSet;
use std::collections::HashMap;
use super::trees::{Stm, Exp, Type, Def, Param, Field, Lit, Root};
use crate::common::names::*;
use crate::hir::ops::*;

// Closure converted expressions and statements.
// This is just a duplicate of Exp, but Lambda and Apply are different.
// The purpose of this is to ensure all the tree is rewritten. We transform from
// HIR to HIR/CC, then back to HIR (without Lambda).
mod hircc {
    use crate::hir::trees::Type;
    use crate::hir::trees::Param;
    use crate::hir::trees::Lit;
    use crate::common::names::Name;
    use crate::hir::ops::*;

    #[derive(Clone, Debug)]
    pub enum Exp {
        NewArray { ty: Type, length: Box<Exp> },
        ArrayLit { ty: Type, exps: Vec<Exp> },
        ArrayLoad { ty: Type, array: Box<Exp>, index: Box<Exp> },
        ArrayLength { array: Box<Exp> },

        Lit { lit: Lit },
        Call { name: Name, args: Vec<Exp> },
        Var { name: Name, ty: Type },

        Binary { op: Bop, e1: Box<Exp>, e2: Box<Exp> },
        Unary { op: Uop, exp: Box<Exp> },

        Seq { body: Box<Stm>, exp: Box<Exp> },

        Let { param: Param, init: Box<Exp>, body: Box<Exp> },
        LambdaCC { env: Param, params: Vec<Param>, body: Box<Exp> },
        ApplyCC { fun: Box<Exp>, args: Vec<Exp> },

        StructLit { ty: Type, fields: Vec<Field> },
        StructLoad { ty: Type, base: Box<Exp>, field: Name },

        Box { ty: Type, exp: Box<Exp> },
        Unbox { ty: Type, exp: Box<Exp> },
        Cast { ty: Type, exp: Box<Exp> },
    }

    #[derive(Clone, Debug)]
    pub enum Stm {
        IfElse { cond: Box<Exp>, if_true: Box<Stm>, if_false: Box<Stm> },
        IfThen { cond: Box<Exp>, if_true: Box<Stm> },
        While { cond: Box<Exp>, body: Box<Stm> },
        Return { exp: Box<Exp> },
        Block { body: Vec<Stm> },
        Eval { exp: Box<Exp> },
        Assign { ty: Type, lhs: Name, rhs: Box<Exp> },
        ArrayAssign { ty: Type, array: Box<Exp>, index: Box<Exp>, value: Box<Exp> },
        StructAssign { ty: Type, base: Box<Exp>, field: Name, value: Box<Exp> },
    }

    #[derive(Clone, Debug)]
    pub struct Field {
        pub param: Param,
        pub exp: Box<Exp>,
    }
}

macro_rules! union {
    ($e: expr) => { $e };

    ($e1: expr, $e2: expr) => {
        {
            let mut s1: HashTrieSet<Name> = $e1;
            let s2: HashTrieSet<Name> = $e2;
            for x in s2.iter() {
                s1 = s1.insert(*x);
            }
            s1
        }
    };

    ($e: expr, $($es: expr),+) => {
        union!($e, union!($($es),+))
    };
}

trait FV {
    fn fv(&self) -> HashTrieSet<Name>;
}

impl<A: FV> FV for Vec<A> {
    fn fv(&self) -> HashTrieSet<Name> {
        let mut s = HashTrieSet::new();
        for e in self {
            s = union!(s, e.fv());
        }
        s
    }
}

impl FV for Stm {
    fn fv(&self) -> HashTrieSet<Name> {
        match self {
            Stm::IfElse { cond, if_true, if_false } => {
                union!(cond.fv(), if_true.fv(), if_false.fv())
            },
            Stm::IfThen { cond, if_true } => {
                union!(cond.fv(), if_true.fv())
            },
            Stm::While { cond, body } => {
                union!(cond.fv(), body.fv())
            },
            Stm::Return { exp } => {
                exp.fv()
            },
            Stm::Block { body } => {
                body.fv()
            },
            Stm::Eval { exp } => {
                exp.fv()
            },
            Stm::Assign { ty, lhs, rhs } => {
                rhs.fv().insert(*lhs)
            },
            Stm::ArrayAssign { ty, array, index, value } => {
                union!(array.fv(), index.fv(), value.fv())
            },
            Stm::StructAssign { ty, base, field, value } => {
                union!(base.fv(), value.fv())
            },
        }
    }
}

impl FV for Exp {
    fn fv(&self) -> HashTrieSet<Name> {
        match self {
            Exp::NewArray { ty, length } => length.fv(),
            Exp::ArrayLit { ty, exps } => {
                exps.fv()
            },
            Exp::ArrayLoad { ty, array, index } => {
                union!(array.fv(), index.fv())
            },
            Exp::ArrayLength { array } => array.fv(),

            Exp::Lit { lit } => {
                HashTrieSet::new()
            }
            Exp::Call { name, args } => {
                args.fv()
            },
            Exp::Var { name, ty } => {
                HashTrieSet::new().insert(*name)
            },
            Exp::Binary { op, e1, e2 } => {
                union!(e1.fv(), e2.fv())
            },
            Exp::Unary { op, exp } => exp.fv(),
            Exp::Box { ty, exp } => exp.fv(),
            Exp::Unbox { ty, exp } => exp.fv(),
            Exp::Cast { ty, exp } => exp.fv(),

            Exp::Seq { body, exp } => {
                union!(body.fv(), exp.fv())
            },
            Exp::Let { param, init, body } => {
                union!(init.fv(), body.fv().remove(&param.name))
            },
            Exp::Lambda { params, body } => {
                let mut p = HashTrieSet::new();
                for param in params {
                    p = p.insert(param.name);
                }
                let mut s = HashTrieSet::new();
                for x in body.fv().iter() {
                    if ! p.contains(&x) {
                        s = s.insert(*x);
                    }
                }
                s
            },
            Exp::Apply { fun, args } => {
                union!(fun.fv(), args.fv())
            }
            Exp::StructLit { ty, fields } => {
                let mut s = HashTrieSet::new();
                for field in fields {
                    s = union!(s, field.exp.fv())
                }
                s
            },
            Exp::StructLoad { ty, base, field } => base.fv(),
        }
    }
}

type Subst = HashMap<Name, hircc::Exp>;

trait Substitute {
    fn subst(&self, s: &Subst) -> Self;
}

impl<A: Substitute + Clone> Substitute for Box<A> {
    fn subst(&self, s: &Subst) -> Box<A> {
        Box::new((*self.clone()).subst(s))
    }
}

impl<A: Substitute> Substitute for Vec<A> {
    fn subst(&self, s: &Subst) -> Vec<A> {
        self.iter().map(|e| e.subst(s)).collect()
    }
}

impl Substitute for hircc::Exp {
    fn subst(&self, s: &Subst) -> hircc::Exp {
        match self {
            hircc::Exp::NewArray { ty, length } => {
                hircc::Exp::NewArray { ty: ty.clone(), length: length.subst(s) }
            },
            hircc::Exp::ArrayLit { ty, exps } => {
                hircc::Exp::ArrayLit { ty: ty.clone(), exps: exps.subst(s) }
            },
            hircc::Exp::ArrayLoad { ty, array, index } => {
                hircc::Exp::ArrayLoad { ty: ty.clone(), array: array.subst(s), index: index.subst(s) }
            },
            hircc::Exp::ArrayLength { array } => {
                hircc::Exp::ArrayLength { array: array.subst(s) }
            },
            hircc::Exp::Lit { lit } => {
                hircc::Exp::Lit { lit: lit.clone() }
            },
            hircc::Exp::Call { name, args } => {
                hircc::Exp::Call { name: *name, args: args.subst(s) }
            },
            hircc::Exp::Var { name, ty } => {
                match s.get(&name) {
                    Some(e) => e.clone(),
                    None => hircc::Exp::Var { name: *name, ty: ty.clone() }
                }
            },
            hircc::Exp::Binary { op, e1, e2 } => {
                hircc::Exp::Binary { op: *op, e1: e1.subst(s), e2: e2.subst(s) }
            },
            hircc::Exp::Unary { op, exp } => {
                hircc::Exp::Unary { op: *op, exp: exp.subst(s) }
            },
            hircc::Exp::Box { ty, exp } => {
                hircc::Exp::Box { ty: ty.clone(), exp: exp.subst(s) }
            },
            hircc::Exp::Unbox { ty, exp } => {
                hircc::Exp::Unbox { ty: ty.clone(), exp: exp.subst(s) }
            },
            hircc::Exp::Cast { ty, exp } => {
                hircc::Exp::Cast { ty: ty.clone(), exp: exp.subst(s) }
            },

            hircc::Exp::Seq { body, exp } => {
                hircc::Exp::Seq { body: body.subst(s), exp: exp.subst(s) }
            },

            hircc::Exp::Let { param, init, body } => {
                let mut s2: Subst = s.clone();
                s2.remove(&param.name);
                hircc::Exp::Let { param: param.clone(), init: init.subst(s), body: body.subst(&s2) }
            },
            hircc::Exp::LambdaCC { env, params, body } => {
                let mut s2: Subst = s.clone();
                s2.remove(&env.name);
                for param in params {
                    s2.remove(&param.name);
                }
                hircc::Exp::LambdaCC { env: env.clone(), params: params.clone(), body: body.subst(&s2) }
            },
            hircc::Exp::ApplyCC { fun, args } => {
                hircc::Exp::ApplyCC { fun: fun.subst(s), args: args.subst(s) }
            },

            hircc::Exp::StructLit { ty, fields } => {
                hircc::Exp::StructLit { ty: ty.clone(), fields: fields.iter().map(|f| hircc::Field { param: f.param.clone(), exp: f.exp.subst(s) }).collect() }
            },
            hircc::Exp::StructLoad { ty, base, field } => {
                hircc::Exp::StructLoad { ty: ty.clone(), base: base.subst(s), field: *field }
            },
        }
    }
}

impl Substitute for hircc::Stm {
    fn subst(&self, s: &Subst) -> hircc::Stm {
        match self {
            hircc::Stm::IfElse { cond, if_true, if_false } => {
                hircc::Stm::IfElse { cond: cond.subst(s), if_true: if_true.subst(s), if_false: if_false.subst(s) }
            },
            hircc::Stm::IfThen { cond, if_true } => {
                hircc::Stm::IfThen { cond: cond.subst(s), if_true: if_true.subst(s) }
            },
            hircc::Stm::While { cond, body } => {
                hircc::Stm::While { cond: cond.subst(s), body: body.subst(s) }
            },
            hircc::Stm::Return { exp } => {
                hircc::Stm::Return { exp: exp.subst(s) }
            },
            hircc::Stm::Block { body } => {
                hircc::Stm::Block { body: body.subst(s) }
            },
            hircc::Stm::Eval { exp } => {
                hircc::Stm::Eval { exp: exp.subst(s) }
            },
            hircc::Stm::Assign { ty, lhs, rhs } => {
                hircc::Stm::Assign { ty: ty.clone(), lhs: *lhs, rhs: rhs.subst(s) }
            },
            hircc::Stm::ArrayAssign { ty, array, index, value } => {
                hircc::Stm::ArrayAssign { ty: ty.clone(), array: array.subst(s), index: index.subst(s), value: value.subst(s) }
            },
            hircc::Stm::StructAssign { ty, base, field, value } => {
                hircc::Stm::StructAssign { ty: ty.clone(), base: base.subst(s), field: *field, value: value.subst(s) }
            },
        }
    }
}

pub trait CC<T> {
    fn convert(&self) -> T;
}

impl CC<hircc::Exp> for Exp {
    fn convert(&self) -> hircc::Exp {
        match self {
            Exp::NewArray { ty, length } => {
                hircc::Exp::NewArray { ty: ty.clone(), length: Box::new(length.convert()) }
            },
            Exp::ArrayLit { ty, exps } => {
                hircc::Exp::ArrayLit { ty: ty.clone(), exps: exps.iter().map(|e| e.convert()).collect() }
            },
            Exp::ArrayLoad { ty, array, index } => {
                hircc::Exp::ArrayLoad { ty: ty.clone(), array: Box::new(array.convert()), index: Box::new(index.convert()) }
            },
            Exp::ArrayLength { array } => {
                hircc::Exp::ArrayLength { array: Box::new(array.convert()) }
            },
            Exp::Lit { lit } => {
                hircc::Exp::Lit { lit: lit.clone() }
            },
            Exp::Call { name, args } => {
                hircc::Exp::Call { name: *name, args: args.iter().map(|e| e.convert()).collect() }
            },
            Exp::Var { name, ty } => {
                hircc::Exp::Var { name: *name, ty: ty.clone() }
            },

            Exp::Binary { op, e1, e2 } => {
                hircc::Exp::Binary { op: *op, e1: Box::new(e1.convert()), e2: Box::new(e2.convert()) }
            },
            Exp::Unary { op, exp } => {
                hircc::Exp::Unary { op: *op, exp: Box::new(exp.convert()) }
            },
            Exp::Box { ty, exp } => {
                hircc::Exp::Box { ty: ty.clone(), exp: Box::new(exp.convert()) }
            },
            Exp::Unbox { ty, exp } => {
                hircc::Exp::Unbox { ty: ty.clone(), exp: Box::new(exp.convert()) }
            },
            Exp::Cast { ty, exp } => {
                hircc::Exp::Cast { ty: ty.clone(), exp: Box::new(exp.convert()) }
            },

            Exp::Seq { body, exp } => {
                hircc::Exp::Seq { body: Box::new(body.convert()), exp: Box::new(exp.convert()) }
            },

            Exp::Let { param, init, body } => {
                hircc::Exp::Let { param: param.clone(), init: Box::new(init.convert()), body: Box::new(body.convert()) }
            },
            Exp::Lambda { params, body } => {
                // The only interesting case is lambda.

                // Create a new name for the environment parameter.
                let env = Name::fresh("env");

                // Get the free variables of the lambda.
                let vars = self.fv();

                // Create a struct to represent the environment.
                // Each var in vars is mapped to a lookup into the environment.
                let mut env_fields = Vec::new();
                let mut env_params = Vec::new();

                for (i, x) in vars.iter().enumerate() {
                    // Make sure the indices agree.
                    assert_eq!(env_fields.len(), i);
                    let param = Param {
                        ty: Type::Box,
                        name: *x
                    };
                    env_params.push(param.clone());
                    env_fields.push(hircc::Field {
                        param: param,
                        exp: Box::new(hircc::Exp::Var { name: *x, ty: Type::Box }),
                    });
                }

                let env_type = Type::Struct { fields: env_params };

                // Build a substitution.
                // Map x to env.x
                let mut s = HashMap::new();
                for (i, x) in vars.iter().enumerate() {
                    s.insert(*x, hircc::Exp::StructLoad {
                        ty: env_type.clone(),
                        base: Box::new(hircc::Exp::Var { name: env, ty: env_type.clone() }),
                        field: *x
                    });
                }

                let cc_body = body.convert().subst(&s);

                let fun_param = Param { name: Name::new("fun"), ty: Type::FunPtr };
                let env_param = Param { name: Name::new("env"), ty: env_type.clone() };

                hircc::Exp::StructLit {
                    ty: Type::Struct { fields: vec![fun_param.clone(), env_param.clone()] },
                    fields: vec![
                        hircc::Field {
                            param: fun_param,
                            exp: Box::new(
                                hircc::Exp::LambdaCC {
                                    env: Param { name: env, ty: Type::EnvPtr },
                                    params: params.clone(),
                                    body: Box::new(cc_body),
                                }
                            ),
                        },
                        hircc::Field {
                            param: env_param,
                            exp: Box::new(
                                hircc::Exp::StructLit {
                                    ty: env_type.clone(),
                                    fields: env_fields
                                }
                            ),
                        }
                    ]
                }
            },
            Exp::Apply { fun, args } => {
                hircc::Exp::ApplyCC { fun: Box::new(fun.convert()), args: args.iter().map(|e| e.convert()).collect() }
            },

            Exp::StructLit { ty, fields } => {
                hircc::Exp::StructLit {
                    ty: ty.clone(),
                    fields: fields.iter().map(|f| hircc::Field { param: f.param.clone(), exp: Box::new(f.exp.convert()) }).collect()
                }
            },
            Exp::StructLoad { ty, base, field } => {
                hircc::Exp::StructLoad { ty: ty.clone(), base: Box::new(base.convert()), field: *field }
            },
        }
    }
}

impl CC<hircc::Stm> for Stm {
    fn convert(&self) -> hircc::Stm {
        match self {
            Stm::IfElse { cond, if_true, if_false } => {
                hircc::Stm::IfElse { cond: Box::new(cond.convert()), if_true: Box::new(if_true.convert()), if_false: Box::new(if_false.convert()) }
            },
            Stm::IfThen { cond, if_true } => {
                hircc::Stm::IfThen { cond: Box::new(cond.convert()), if_true: Box::new(if_true.convert()) }
            },
            Stm::While { cond, body } => {
                hircc::Stm::While { cond: Box::new(cond.convert()), body: Box::new(body.convert()) }
            },
            Stm::Return { exp } => {
                hircc::Stm::Return { exp: Box::new(exp.convert()) }
            },
            Stm::Block { body } => {
                hircc::Stm::Block { body: body.iter().map(|e| e.convert()).collect() }
            },
            Stm::Eval { exp } => {
                hircc::Stm::Eval { exp: Box::new(exp.convert()) }
            },
            Stm::Assign { ty, lhs, rhs } => {
                hircc::Stm::Assign { ty: ty.clone(), lhs: *lhs, rhs: Box::new(rhs.convert()) }
            },
            Stm::ArrayAssign { ty, array, index, value } => {
                hircc::Stm::ArrayAssign { ty: ty.clone(), array: Box::new(array.convert()), index: Box::new(index.convert()), value: Box::new(value.convert()) }
            },
            Stm::StructAssign { ty, base, field, value } => {
                hircc::Stm::StructAssign { ty: ty.clone(), base: Box::new(base.convert()), field: *field, value: Box::new(value.convert()) }
            },
        }
    }
}

pub trait LL<T> {
    fn lift(&self, decls: &mut Vec<Def>) -> T;
}

pub struct Lift;

impl Lift {
    pub fn lift(root: &Root) -> Root {
        let mut defs = Vec::new();
        let mut decls = Vec::new();

        for def in &root.defs {
            defs.push(def.lift(&mut decls));
        }

        defs.append(&mut decls);

        Root {
            defs
        }
    }
}

impl LL<Def> for Def {
    fn lift(&self, decls: &mut Vec<Def>) -> Def {
        match self {
            Def::VarDef { ty, name, exp } => {
                Def::VarDef { ty: ty.clone(), name: *name, exp: Box::new(exp.convert().lift(decls)) }
            },
            Def::FunDef { ty, name, params, body } => {
                Def::FunDef { ty: ty.clone(), name: *name, params: params.clone(), body: Box::new(body.convert().lift(decls)) }
            },
            Def::ExternDef { ty, name, params } => {
                Def::ExternDef { ty: ty.clone(), name: *name, params: params.clone() }
            }
        }
    }
}

impl LL<Exp> for hircc::Exp {
    fn lift(&self, decls: &mut Vec<Def>) -> Exp {
        match self {
            hircc::Exp::NewArray { ty, length } => {
                Exp::NewArray { ty: ty.clone(), length: Box::new(length.lift(decls)) }
            },
            hircc::Exp::ArrayLit { ty, exps } => {
                Exp::ArrayLit { ty: ty.clone(), exps: exps.iter().map(|e| e.lift(decls)).collect() }
            },
            hircc::Exp::ArrayLoad { ty, array, index } => {
                Exp::ArrayLoad { ty: ty.clone(), array: Box::new(array.lift(decls)), index: Box::new(index.lift(decls)) }
            },
            hircc::Exp::ArrayLength { array } => {
                Exp::ArrayLength { array: Box::new(array.lift(decls)) }
            },
            hircc::Exp::Lit { lit } => {
                Exp::Lit { lit: lit.clone() }
            },
            hircc::Exp::Call { name, args } => {
                Exp::Call { name: *name, args: args.iter().map(|e| e.lift(decls)).collect() }
            },
            hircc::Exp::Var { name, ty } => {
                Exp::Var { name: *name, ty: ty.clone() }
            },

            hircc::Exp::Binary { op, e1, e2 } => {
                Exp::Binary { op: *op, e1: Box::new(e1.lift(decls)), e2: Box::new(e2.lift(decls)) }
            },
            hircc::Exp::Unary { op, exp } => {
                Exp::Unary { op: *op, exp: Box::new(exp.lift(decls)) }
            },
            hircc::Exp::Box { ty, exp } => {
                Exp::Box { ty: ty.clone(), exp: Box::new(exp.lift(decls)) }
            },
            hircc::Exp::Unbox { ty, exp } => {
                Exp::Unbox { ty: ty.clone(), exp: Box::new(exp.lift(decls)) }
            },
            hircc::Exp::Cast { ty, exp } => {
                Exp::Cast { ty: ty.clone(), exp: Box::new(exp.lift(decls)) }
            },
            hircc::Exp::Seq { body, exp } => {
                Exp::Seq { body: Box::new(body.lift(decls)), exp: Box::new(exp.lift(decls)) }
            },
            hircc::Exp::Let { param, init, body } => {
                Exp::Let { param: param.clone(), init: Box::new(init.lift(decls)), body: Box::new(body.lift(decls)) }
            },
            hircc::Exp::LambdaCC { env, params, body } => {
                let lifted_body = body.lift(decls);

                let mut def_params = params.clone();
                def_params.push(env.clone());

                let f = Name::fresh("lifted");

                // Declare the function
                decls.push(Def::FunDef {
                    ty: Type::Box,  // TODO
                    name: f,
                    params: def_params,
                    body: Box::new(lifted_body),
                });

                Exp::Var { name: f, ty: Type::FunPtr }
            },
            hircc::Exp::ApplyCC { fun, args } => {
                let closure = Name::fresh("closure");
                let mut closure_args: Vec<Exp> = args.iter().map(|e| e.lift(decls)).collect();
                let closure_type = Type::Struct {
                    fields: vec![
                        Param { name: Name::new("fun"), ty: Type::FunPtr },
                        Param { name: Name::new("env"), ty: Type::EnvPtr } // TODO
                    ]
                };
                // Add environment at the end of the arguments.
                closure_args.push(
                    Exp::StructLoad {
                        ty: closure_type.clone(),
                        base: Box::new(Exp::Var { name: closure, ty: closure_type.clone() }),
                        field: Name::new("env"),
                    },
                );

                Exp::Let {
                    param: Param { name: closure, ty: closure_type.clone() },
                    init: Box::new(fun.lift(decls)),
                    body: Box::new(
                        Exp::Apply {
                            fun: Box::new(
                                Exp::StructLoad {
                                    ty: closure_type.clone(),
                                    base: Box::new(Exp::Var { name: closure, ty: closure_type.clone() }),
                                    field: Name::new("fun"),
                                }
                            ),
                            args: closure_args
                        }
                    )
                }
            },
            hircc::Exp::StructLit { ty, fields } => {
                Exp::StructLit {
                    ty: ty.clone(),
                    fields: fields.iter().map(|f| Field { param: f.param.clone(), exp: Box::new(f.exp.lift(decls)) }).collect()
                 }
            },
            hircc::Exp::StructLoad { ty, base, field } => {
                Exp::StructLoad { ty: ty.clone(), base: Box::new(base.lift(decls)), field: *field }
            },
        }
    }
}

impl LL<Stm> for hircc::Stm {
    fn lift(&self, decls: &mut Vec<Def>) -> Stm {
        match self {
            hircc::Stm::IfElse { cond, if_true, if_false } => {
                Stm::IfElse { cond: Box::new(cond.lift(decls)), if_true: Box::new(if_true.lift(decls)), if_false: Box::new(if_false.lift(decls)) }
            },
            hircc::Stm::IfThen { cond, if_true } => {
                Stm::IfThen { cond: Box::new(cond.lift(decls)), if_true: Box::new(if_true.lift(decls)) }
            },
            hircc::Stm::While { cond, body } => {
                Stm::While { cond: Box::new(cond.lift(decls)), body: Box::new(body.lift(decls)) }
            },
            hircc::Stm::Return { exp } => {
                Stm::Return { exp: Box::new(exp.lift(decls)) }
            },
            hircc::Stm::Block { body } => {
                Stm::Block { body: body.iter().map(|e| e.lift(decls)).collect() }
            },
            hircc::Stm::Eval { exp } => {
                Stm::Eval { exp: Box::new(exp.lift(decls)) }
            },
            hircc::Stm::Assign { ty, lhs, rhs } => {
                Stm::Assign { ty: ty.clone(), lhs: *lhs, rhs: Box::new(rhs.lift(decls)) }
            },
            hircc::Stm::ArrayAssign { ty, array, index, value } => {
                Stm::ArrayAssign { ty: ty.clone(), array: Box::new(array.lift(decls)), index: Box::new(index.lift(decls)), value: Box::new(value.lift(decls)) }
            },
            hircc::Stm::StructAssign { ty, base, field, value } => {
                Stm::StructAssign { ty: ty.clone(), base: Box::new(base.lift(decls)), field: *field, value: Box::new(value.lift(decls)) }
            },
        }
    }
}
