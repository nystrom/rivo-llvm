// Closure conversion for HIR
// We translate into HIR/CC, then lambda lift, producing HIR again (but without lambdas).

use std::collections::HashMap;
use std::collections::HashSet;

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
        ArrayLoad { bounds_check: bool, ty: Type, array: Box<Exp>, index: Box<Exp> },
        ArrayLength { array: Box<Exp> },

        Lit { lit: Lit },
        Call { fun_type: Type, name: Name, args: Vec<Exp> },
        Var { name: Name, ty: Type },
        Global { name: Name, ty: Type },
        Function { name: Name, ty: Type },

        Binary { op: Bop, e1: Box<Exp>, e2: Box<Exp> },
        Unary { op: Uop, exp: Box<Exp> },

        Seq { body: Box<Stm>, exp: Box<Exp> },

        Let { inits: Vec<Field>, body: Box<Exp> },
        LambdaCC { ret_type: Type, env_param: Param, params: Vec<Param>, body: Box<Exp> },
        ApplyCC { fun_type: Type, fun: Box<Exp>, args: Vec<Exp> },

        StructLit { fields: Vec<Field> },
        ClosureLit { fun_type: Type, fun: Box<Exp>, env: Box<Exp> },
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
        ArrayAssign { bounds_check: bool, ty: Type, array: Box<Exp>, index: Box<Exp>, value: Box<Exp> },
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
            let mut s: HashMap<Name, Type> = $e1;
            for (x, ty) in $e2 {
                s.insert(x, ty.clone());
            }
            s
        }
    };

    ($e: expr, $($es: expr),+) => {
        union!($e, union!($($es),+))
    };
}

trait FV {
    fn fv(&self) -> HashMap<Name, Type>;
}

impl<A: FV> FV for Vec<A> {
    fn fv(&self) -> HashMap<Name, Type> {
        let mut s = HashMap::new();
        for e in self {
            s = union!(s, e.fv());
        }
        s
    }
}

impl FV for Stm {
    fn fv(&self) -> HashMap<Name, Type> {
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
                let mut s = rhs.fv();
                s.insert(*lhs, ty.clone());
                s
            },
            Stm::ArrayAssign { bounds_check, ty, array, index, value } => {
                union!(array.fv(), index.fv(), value.fv())
            },
            Stm::StructAssign { ty, base, field, value } => {
                union!(base.fv(), value.fv())
            },
        }
    }
}

impl FV for Exp {
    fn fv(&self) -> HashMap<Name, Type> {
        match self {
            Exp::NewArray { ty, length } => length.fv(),
            Exp::ArrayLit { ty, exps } => {
                exps.fv()
            },
            Exp::ArrayLoad { bounds_check, ty, array, index } => {
                union!(array.fv(), index.fv())
            },
            Exp::ArrayLength { array } => array.fv(),

            Exp::Lit { lit } => {
                HashMap::new()
            }
            Exp::Call { fun_type, name, args } => {
                args.fv()
            },
            Exp::Var { name, ty } => {
                let mut s = HashMap::new();
                s.insert(*name, ty.clone());
                s
            },
            Exp::Global { name, ty } => {
                HashMap::new()
            },
            Exp::Function { name, ty } => {
                HashMap::new()
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
            Exp::Let { inits, body } => {
                let mut p = HashSet::new();
                for init in inits {
                    p.insert(init.param.name);
                }
                let mut s = HashMap::new();
                for (x, ty) in body.fv() {
                    if ! p.contains(&x) {
                        s.insert(x, ty.clone());
                    }
                }
                for init in inits {
                    s = union!(s, init.exp.fv());
                }
                s
            },
            Exp::Lambda { ret_type, params, body } => {
                let mut p = HashSet::new();
                for param in params {
                    p.insert(param.name);
                }
                let mut s = HashMap::new();
                for (x, ty) in body.fv() {
                    if ! p.contains(&x) {
                        s.insert(x, ty.clone());
                    }
                }
                s
            },
            Exp::Apply { fun_type, fun, args } => {
                union!(fun.fv(), args.fv())
            }
            Exp::StructLit { fields } => {
                let mut s = HashMap::new();
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

impl Substitute for hircc::Field {
    fn subst(&self, s: &Subst) -> hircc::Field {
        hircc::Field {
            param: self.param.clone(),
            exp: self.exp.subst(s)
        }
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
            hircc::Exp::ArrayLoad { bounds_check, ty, array, index } => {
                hircc::Exp::ArrayLoad { bounds_check: *bounds_check, ty: ty.clone(), array: array.subst(s), index: index.subst(s) }
            },
            hircc::Exp::ArrayLength { array } => {
                hircc::Exp::ArrayLength { array: array.subst(s) }
            },
            hircc::Exp::Lit { lit } => {
                hircc::Exp::Lit { lit: lit.clone() }
            },
            hircc::Exp::Call { fun_type, name, args } => {
                hircc::Exp::Call { fun_type: fun_type.clone(), name: *name, args: args.subst(s) }
            },
            hircc::Exp::Var { name, ty } => {
                match s.get(&name) {
                    Some(e) => e.clone(),
                    None => hircc::Exp::Var { name: *name, ty: ty.clone() }
                }
            },
            hircc::Exp::Global { ty, name } => {
                hircc::Exp::Global { name: *name, ty: ty.clone() }
            },
            hircc::Exp::Function { ty, name } => {
                hircc::Exp::Function { name: *name, ty: ty.clone() }
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

            hircc::Exp::Let { inits, body } => {
                let mut s2: Subst = s.clone();
                for f in inits {
                    let name = f.param.name;
                    s2.remove(&name);
                }
                hircc::Exp::Let { inits: inits.subst(s), body: body.subst(&s2) }
            },
            hircc::Exp::LambdaCC { ret_type, env_param, params, body } => {
                let mut s2: Subst = s.clone();
                s2.remove(&env_param.name);
                for param in params {
                    s2.remove(&param.name);
                }
                hircc::Exp::LambdaCC { ret_type: ret_type.clone(), env_param: env_param.clone(), params: params.clone(), body: body.subst(&s2) }
            },
            hircc::Exp::ApplyCC { fun_type, fun, args } => {
                hircc::Exp::ApplyCC { fun_type: fun_type.clone(), fun: fun.subst(s), args: args.subst(s) }
            },

            hircc::Exp::StructLit { fields } => {
                hircc::Exp::StructLit { fields: fields.subst(s) }
            },
            hircc::Exp::ClosureLit { fun_type, fun, env } => {
                hircc::Exp::ClosureLit { fun_type: fun_type.clone(), fun: fun.subst(s), env: env.subst(s) }
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
            hircc::Stm::ArrayAssign { bounds_check, ty, array, index, value } => {
                hircc::Stm::ArrayAssign { bounds_check: *bounds_check, ty: ty.clone(), array: array.subst(s), index: index.subst(s), value: value.subst(s) }
            },
            hircc::Stm::StructAssign { ty, base, field, value } => {
                hircc::Stm::StructAssign { ty: ty.clone(), base: base.subst(s), field: *field, value: value.subst(s) }
            },
        }
    }
}

pub trait CC<T> {
    fn convert(&self, fresh_name_generator: &mut FreshNameGenerator, globals: &HashSet<Name>) -> T;
}

impl CC<hircc::Field> for Field {
    fn convert(&self, fresh_name_generator: &mut FreshNameGenerator, globals: &HashSet<Name>) -> hircc::Field {
        hircc::Field {
            param: self.param.clone(),
            exp: Box::new(self.exp.convert(fresh_name_generator, globals))
        }
    }
}

impl CC<hircc::Exp> for Exp {
    fn convert(&self, fresh_name_generator: &mut FreshNameGenerator, globals: &HashSet<Name>) -> hircc::Exp {
        match self {
            Exp::NewArray { ty, length } => {
                hircc::Exp::NewArray { ty: ty.clone(), length: Box::new(length.convert(fresh_name_generator, globals)) }
            },
            Exp::ArrayLit { ty, exps } => {
                hircc::Exp::ArrayLit { ty: ty.clone(), exps: exps.iter().map(|e| e.convert(fresh_name_generator, globals)).collect() }
            },
            Exp::ArrayLoad { bounds_check, ty, array, index } => {
                hircc::Exp::ArrayLoad { bounds_check: *bounds_check, ty: ty.clone(), array: Box::new(array.convert(fresh_name_generator, globals)), index: Box::new(index.convert(fresh_name_generator, globals)) }
            },
            Exp::ArrayLength { array } => {
                hircc::Exp::ArrayLength { array: Box::new(array.convert(fresh_name_generator, globals)) }
            },
            Exp::Lit { lit } => {
                hircc::Exp::Lit { lit: lit.clone() }
            },
            Exp::Call { fun_type, name, args } => {
                hircc::Exp::Call { fun_type: fun_type.clone(), name: *name, args: args.iter().map(|e| e.convert(fresh_name_generator, globals)).collect() }
            },
            Exp::Var { name, ty } => {
                hircc::Exp::Var { name: *name, ty: ty.clone() }
            },
            Exp::Global { name, ty } => {
                hircc::Exp::Global { name: *name, ty: ty.clone() }
            },
            Exp::Function { name, ty } => {
                hircc::Exp::Function { name: *name, ty: ty.clone() }
            },

            Exp::Binary { op, e1, e2 } => {
                hircc::Exp::Binary { op: *op, e1: Box::new(e1.convert(fresh_name_generator, globals)), e2: Box::new(e2.convert(fresh_name_generator, globals)) }
            },
            Exp::Unary { op, exp } => {
                hircc::Exp::Unary { op: *op, exp: Box::new(exp.convert(fresh_name_generator, globals)) }
            },
            Exp::Box { ty, exp } => {
                hircc::Exp::Box { ty: ty.clone(), exp: Box::new(exp.convert(fresh_name_generator, globals)) }
            },
            Exp::Unbox { ty, exp } => {
                hircc::Exp::Unbox { ty: ty.clone(), exp: Box::new(exp.convert(fresh_name_generator, globals)) }
            },
            Exp::Cast { ty, exp } => {
                hircc::Exp::Cast { ty: ty.clone(), exp: Box::new(exp.convert(fresh_name_generator, globals)) }
            },

            Exp::Seq { body, exp } => {
                hircc::Exp::Seq { body: Box::new(body.convert(fresh_name_generator, globals)), exp: Box::new(exp.convert(fresh_name_generator, globals)) }
            },

            Exp::Let { inits, body } => {
                hircc::Exp::Let { inits: inits.iter().map(|f| f.convert(fresh_name_generator, globals)).collect(), body: Box::new(body.convert(fresh_name_generator, globals)) }
            },
            Exp::Lambda { ret_type, params, body } => {
                // The only interesting case is lambda.

                // Create a new name for the environment parameter.
                let env = fresh_name_generator.fresh("env");

                // Get the free variables of the lambda.
                // TODO: get the types of the variables!
                let mut vars = self.fv();

                // Remove the globals (they don't need to be passed in the environment).
                for name in globals {
                    vars.remove(name);
                }

                // Create a struct to represent the environment.
                // Each var in vars is mapped to a lookup into the environment.
                let mut env_fields = Vec::new();
                let mut env_params = Vec::new();

                for (i, (x, ty)) in vars.iter().enumerate() {
                    let param = Param {
                        ty: ty.clone(),
                        name: *x
                    };
                    env_params.push(param.clone());
                    env_fields.push(hircc::Field {
                        param: param,
                        exp: Box::new(hircc::Exp::Var { name: *x, ty: ty.clone() }),
                    });
                }

                let internal_env_type = Type::Struct { fields: env_params };
                let external_env_type = Type::Struct { fields: vec![] };   // the environment type as seen by the caller (basically a void*)

                let fun_type = Type::Fun {
                    ret: Box::new(ret_type.clone()),
                    args: params.iter().map(|p| p.ty.clone()).collect(),
                };

                // Build a substitution.
                // Map x to env.x
                let mut s = HashMap::new();
                for (i, (x, ty)) in vars.iter().enumerate() {
                    s.insert(*x, hircc::Exp::StructLoad {
                        ty: internal_env_type.clone(),
                        base: Box::new(hircc::Exp::Var { name: env, ty: internal_env_type.clone() }),
                        field: *x
                    });
                }

                let cc_body = body.convert(fresh_name_generator, globals).subst(&s);

                hircc::Exp::ClosureLit {
                    fun_type: fun_type.clone(),
                    fun: Box::new(
                        hircc::Exp::LambdaCC {
                            ret_type: ret_type.clone(),
                            env_param: Param {
                                name: env,
                                ty: internal_env_type.clone(),
                            },
                            params: params.clone(),
                            body: Box::new(cc_body),
                        }
                    ),
                    env: Box::new(
                        hircc::Exp::Cast {
                            ty: external_env_type.clone(),
                            exp: Box::new(
                                hircc::Exp::StructLit {
                                    fields: env_fields
                                }
                            )
                        }
                    ),
                }
            },
            Exp::Apply { fun_type, fun, args } => {
                hircc::Exp::ApplyCC { fun_type: fun_type.clone(), fun: Box::new(fun.convert(fresh_name_generator, globals)), args: args.iter().map(|e| e.convert(fresh_name_generator, globals)).collect() }
            },

            Exp::StructLit { fields } => {
                hircc::Exp::StructLit {
                    fields: fields.iter().map(|f| hircc::Field { param: f.param.clone(), exp: Box::new(f.exp.convert(fresh_name_generator, globals)) }).collect()
                }
            },
            Exp::StructLoad { ty, base, field } => {
                hircc::Exp::StructLoad { ty: ty.clone(), base: Box::new(base.convert(fresh_name_generator, globals)), field: *field }
            },
        }
    }
}

impl CC<hircc::Stm> for Stm {
    fn convert(&self, fresh_name_generator: &mut FreshNameGenerator, globals: &HashSet<Name>) -> hircc::Stm {
        match self {
            Stm::IfElse { cond, if_true, if_false } => {
                hircc::Stm::IfElse { cond: Box::new(cond.convert(fresh_name_generator, globals)), if_true: Box::new(if_true.convert(fresh_name_generator, globals)), if_false: Box::new(if_false.convert(fresh_name_generator, globals)) }
            },
            Stm::IfThen { cond, if_true } => {
                hircc::Stm::IfThen { cond: Box::new(cond.convert(fresh_name_generator, globals)), if_true: Box::new(if_true.convert(fresh_name_generator, globals)) }
            },
            Stm::While { cond, body } => {
                hircc::Stm::While { cond: Box::new(cond.convert(fresh_name_generator, globals)), body: Box::new(body.convert(fresh_name_generator, globals)) }
            },
            Stm::Return { exp } => {
                hircc::Stm::Return { exp: Box::new(exp.convert(fresh_name_generator, globals)) }
            },
            Stm::Block { body } => {
                hircc::Stm::Block { body: body.iter().map(|e| e.convert(fresh_name_generator, globals)).collect() }
            },
            Stm::Eval { exp } => {
                hircc::Stm::Eval { exp: Box::new(exp.convert(fresh_name_generator, globals)) }
            },
            Stm::Assign { ty, lhs, rhs } => {
                hircc::Stm::Assign { ty: ty.clone(), lhs: *lhs, rhs: Box::new(rhs.convert(fresh_name_generator, globals)) }
            },
            Stm::ArrayAssign { bounds_check, ty, array, index, value } => {
                hircc::Stm::ArrayAssign { bounds_check: *bounds_check, ty: ty.clone(), array: Box::new(array.convert(fresh_name_generator, globals)), index: Box::new(index.convert(fresh_name_generator, globals)), value: Box::new(value.convert(fresh_name_generator, globals)) }
            },
            Stm::StructAssign { ty, base, field, value } => {
                hircc::Stm::StructAssign { ty: ty.clone(), base: Box::new(base.convert(fresh_name_generator, globals)), field: *field, value: Box::new(value.convert(fresh_name_generator, globals)) }
            },
        }
    }
}

pub struct LambdaLift;

impl LambdaLift {
    pub fn lambda_lift(root: &Root) -> Root {
        let mut defs = Vec::new();
        let mut decls = Vec::new();

        let globals: HashSet<Name> = root.defs.iter().map(|def| match def {
            Def::VarDef { ty, name, exp } => *name,
            Def::FunDef { ret_type, name, params, body } => *name,
            Def::ExternDef { ty, name } => *name,
        }).collect();

        let mut fresh_name_generator = FreshNameGenerator::new("cc");

        for def in &root.defs {
            defs.push(LambdaLift::lambda_lift_def(def, &mut fresh_name_generator, &globals, &mut decls));
        }

        defs.append(&mut decls);

        Root {
            defs
        }
    }

    fn lambda_lift_def(def: &Def, fresh_name_generator: &mut FreshNameGenerator, globals: &HashSet<Name>, decls: &mut Vec<Def>) -> Def {
        match def {
            Def::VarDef { ty, name, exp } => {
                Def::VarDef { ty: ty.lift_type(), name: *name, exp: Box::new(exp.convert(fresh_name_generator, globals).lambda_lift(fresh_name_generator, decls)) }
            },
            Def::FunDef { ret_type, name, params, body } => {
                Def::FunDef { ret_type: ret_type.lift_type(), name: *name, params: params.lift_type(), body: Box::new(body.convert(fresh_name_generator, globals).lambda_lift(fresh_name_generator, decls)) }
            },
            Def::ExternDef { ty, name } => {
                Def::ExternDef { ty: ty.clone(), name: *name }
            }
        }
    }
}


pub trait LL<T> {
    fn lambda_lift(&self, fresh_name_generator: &mut FreshNameGenerator, decls: &mut Vec<Def>) -> T;
}

trait TypeLifter {
    fn lift_type(&self) -> Self;
}

impl<A: TypeLifter> TypeLifter for Vec<A> {
    fn lift_type(&self) -> Vec<A> {
        self.iter().map(|t| t.lift_type()).collect()
    }
}

impl TypeLifter for Param {
    fn lift_type(&self) -> Param {
        Param { ty: self.ty.lift_type(), name: self.name }
    }
}

impl TypeLifter for Type {
    fn lift_type(&self) -> Type {
        match self {
            Type::Fun { ret, args } => {
                // Function types turn into closure types with the code pointer taking an extract env pointer argument.
                let external_env_type = Type::Struct { fields: vec![] };

                let mut new_args = Vec::new();
                for a in args {
                    new_args.push(a.lift_type());
                }
                new_args.push(external_env_type.clone());

                Type::Struct {
                    fields: vec![
                        Param { name: Name::new("fun"), ty: Type::Fun { ret: Box::new(ret.lift_type()), args: new_args } },
                        Param { name: Name::new("env"), ty: external_env_type.clone() }
                    ]
                }
            },
            Type::Struct { fields } => {
                Type::Struct { fields: fields.lift_type() }
            },
            Type::Array { ty } => {
                Type::Array { ty: Box::new(ty.lift_type()) }
            },
            Type::I8 => Type::I8,
            Type::I16 => Type::I16,
            Type::I32 => Type::I32,
            Type::I64 => Type::I64,
            Type::F32 => Type::F32,
            Type::F64 => Type::F64,
            Type::Bool => Type::Bool,
            Type::Void => Type::Void,
            Type::Box => Type::Box,
        }
    }
}

impl LL<Exp> for hircc::Exp {
    fn lambda_lift(&self, fresh_name_generator: &mut FreshNameGenerator, decls: &mut Vec<Def>) -> Exp {
        match self {
            hircc::Exp::NewArray { ty, length } => {
                Exp::NewArray { ty: ty.lift_type(), length: Box::new(length.lambda_lift(fresh_name_generator, decls)) }
            },
            hircc::Exp::ArrayLit { ty, exps } => {
                Exp::ArrayLit { ty: ty.lift_type(), exps: exps.iter().map(|e| e.lambda_lift(fresh_name_generator, decls)).collect() }
            },
            hircc::Exp::ArrayLoad { bounds_check, ty, array, index } => {
                Exp::ArrayLoad { bounds_check: *bounds_check, ty: ty.lift_type(), array: Box::new(array.lambda_lift(fresh_name_generator, decls)), index: Box::new(index.lambda_lift(fresh_name_generator, decls)) }
            },
            hircc::Exp::ArrayLength { array } => {
                Exp::ArrayLength { array: Box::new(array.lambda_lift(fresh_name_generator, decls)) }
            },
            hircc::Exp::Lit { lit } => {
                Exp::Lit { lit: lit.clone() }
            },
            hircc::Exp::Call { fun_type, name, args } => {
                Exp::Call { fun_type: fun_type.clone(), name: *name, args: args.iter().map(|e| e.lambda_lift(fresh_name_generator, decls)).collect() }
            },
            hircc::Exp::Var { name, ty } => {
                Exp::Var { name: *name, ty: ty.lift_type() }
            },
            hircc::Exp::Global { name, ty } => {
                Exp::Global { name: *name, ty: ty.lift_type() }
            },
            hircc::Exp::Function { name, ty } => {
                Exp::Function { name: *name, ty: ty.lift_type() }
            },

            hircc::Exp::Binary { op, e1, e2 } => {
                Exp::Binary { op: *op, e1: Box::new(e1.lambda_lift(fresh_name_generator, decls)), e2: Box::new(e2.lambda_lift(fresh_name_generator, decls)) }
            },
            hircc::Exp::Unary { op, exp } => {
                Exp::Unary { op: *op, exp: Box::new(exp.lambda_lift(fresh_name_generator, decls)) }
            },
            hircc::Exp::Box { ty, exp } => {
                Exp::Box { ty: ty.lift_type(), exp: Box::new(exp.lambda_lift(fresh_name_generator, decls)) }
            },
            hircc::Exp::Unbox { ty, exp } => {
                Exp::Unbox { ty: ty.lift_type(), exp: Box::new(exp.lambda_lift(fresh_name_generator, decls)) }
            },
            hircc::Exp::Cast { ty, exp } => {
                Exp::Cast { ty: ty.lift_type(), exp: Box::new(exp.lambda_lift(fresh_name_generator, decls)) }
            },
            hircc::Exp::Seq { body, exp } => {
                Exp::Seq { body: Box::new(body.lambda_lift(fresh_name_generator, decls)), exp: Box::new(exp.lambda_lift(fresh_name_generator, decls)) }
            },
            hircc::Exp::Let { inits, body } => {
                Exp::Let { inits: inits.iter().map(|f| Field { param: f.param.lift_type(), exp: Box::new(f.exp.lambda_lift(fresh_name_generator, decls)) }).collect(), body: Box::new(body.lambda_lift(fresh_name_generator, decls)) }
            },
            hircc::Exp::LambdaCC { ret_type, env_param, params, body } => {
                let f = fresh_name_generator.fresh("lifted");

                // Add a parameter for the environment pointer.
                // The parameter type is just a void* (an empty struct pointer).
                let env_param_name = fresh_name_generator.fresh("env");
                let external_env_type = Type::Struct { fields: vec![] };

                let mut def_params = params.clone();
                def_params.push(Param {
                    ty: external_env_type.clone(),
                    name: env_param_name,
                });

                // Create the function type, using the opaque env pointer type.
                let mut args: Vec<Type> = params.iter().map(|p| p.ty.lift_type()).collect();
                args.push(external_env_type.clone());

                let fun_type = Type::Fun {
                    ret: Box::new(ret_type.lift_type()),
                    args: args
                };

                // Lift the body.
                let lifted_body = body.lambda_lift(fresh_name_generator, decls);

                // Cast the env parameter to the more specific type, using the name
                // that was used for the env parameter during closure conversion.
                let env_ptr = Exp::Var { ty: external_env_type.clone(), name: env_param_name };
                let cast = Exp::Cast { ty: env_param.ty.lift_type(), exp: Box::new(env_ptr) };
                let exp = Exp::Let {
                    inits: vec![
                        Field {
                            param: env_param.lift_type(),
                            exp: Box::new(cast)
                        }
                    ],
                    body: Box::new(lifted_body),
                };

                // Declare the function using the new lifted body with cast.
                decls.push(Def::FunDef {
                    ret_type: ret_type.lift_type(),
                    name: f,
                    params: def_params.clone(),
                    body: Box::new(exp),
                });

                // Return a variable with the external function type.
                Exp::Function { name: f, ty: fun_type }
            },
            hircc::Exp::ApplyCC { fun_type, fun, args } => {
                // The caller doesn't know the environment type, just that it's a struct.
                let env_type = Type::Struct { fields: vec![] };

                let closure = fresh_name_generator.fresh("closure");
                let mut closure_args: Vec<Exp> = args.iter().map(|e| e.lambda_lift(fresh_name_generator, decls)).collect();
                let closure_type = fun_type.lift_type();

                // Add environment at the end of the arguments.
                closure_args.push(
                    Exp::StructLoad {
                        ty: closure_type.clone(),
                        base: Box::new(Exp::Var { name: closure, ty: closure_type.clone() }),
                        field: Name::new("env"),
                    },
                );

                let cc_fun_type = match fun_type {
                    Type::Fun { ret, args } => {
                        let mut new_args = Vec::new();
                        for a in args {
                            new_args.push(a.lift_type());
                        }
                        new_args.push(env_type.clone());
                        Type::Fun { ret: Box::new(ret.lift_type()), args: new_args }
                    },
                    _ => panic!("ApplyCC type should be a function type")
                };

                Exp::Let {
                    inits: vec![
                        Field {
                            param: Param { name: closure, ty: closure_type.clone() },
                            exp: Box::new(fun.lambda_lift(fresh_name_generator, decls)),
                        }
                    ],
                    body: Box::new(
                        Exp::Apply {
                            fun_type: cc_fun_type,
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
            hircc::Exp::StructLit { fields } => {
                Exp::StructLit {
                    fields: fields.iter().map(|f| Field { param: f.param.lift_type(), exp: Box::new(f.exp.lambda_lift(fresh_name_generator, decls)) }).collect()
                 }
            },
            hircc::Exp::ClosureLit { fun_type, fun, env } => {
                let env_type = Type::Struct { fields: vec![] };

                // distinguish between closure lits and other struct literals
                // in that we don't convert the fun_type into a closure type, but we do lift the function type itself.
                let cc_fun_type = match fun_type {
                    Type::Fun { ret, args } => {
                        let mut new_args = Vec::new();
                        for a in args {
                            new_args.push(a.lift_type());
                        }
                        new_args.push(env_type.clone());
                        Type::Fun { ret: Box::new(ret.lift_type()), args: new_args }
                    },
                    _ => panic!("ApplyCC type should be a function type")
                };


                Exp::StructLit {
                    fields: vec![
                        Field {
                            param: Param {
                                ty: cc_fun_type.clone(),
                                name: Name::new("fun"),
                            },
                            exp: Box::new(fun.lambda_lift(fresh_name_generator, decls)),
                        },
                        Field {
                            param: Param {
                                ty: env_type.lift_type(),
                                name: Name::new("env"),
                            },
                            exp: Box::new(env.lambda_lift(fresh_name_generator, decls)),
                        },
                    ]
                }
            },
            hircc::Exp::StructLoad { ty, base, field } => {
                Exp::StructLoad { ty: ty.lift_type(), base: Box::new(base.lambda_lift(fresh_name_generator, decls)), field: *field }
            },
        }
    }
}

impl LL<Stm> for hircc::Stm {
    fn lambda_lift(&self, fresh_name_generator: &mut FreshNameGenerator, decls: &mut Vec<Def>) -> Stm {
        match self {
            hircc::Stm::IfElse { cond, if_true, if_false } => {
                Stm::IfElse { cond: Box::new(cond.lambda_lift(fresh_name_generator, decls)), if_true: Box::new(if_true.lambda_lift(fresh_name_generator, decls)), if_false: Box::new(if_false.lambda_lift(fresh_name_generator, decls)) }
            },
            hircc::Stm::IfThen { cond, if_true } => {
                Stm::IfThen { cond: Box::new(cond.lambda_lift(fresh_name_generator, decls)), if_true: Box::new(if_true.lambda_lift(fresh_name_generator, decls)) }
            },
            hircc::Stm::While { cond, body } => {
                Stm::While { cond: Box::new(cond.lambda_lift(fresh_name_generator, decls)), body: Box::new(body.lambda_lift(fresh_name_generator, decls)) }
            },
            hircc::Stm::Return { exp } => {
                Stm::Return { exp: Box::new(exp.lambda_lift(fresh_name_generator, decls)) }
            },
            hircc::Stm::Block { body } => {
                Stm::Block { body: body.iter().map(|e| e.lambda_lift(fresh_name_generator, decls)).collect() }
            },
            hircc::Stm::Eval { exp } => {
                Stm::Eval { exp: Box::new(exp.lambda_lift(fresh_name_generator, decls)) }
            },
            hircc::Stm::Assign { ty, lhs, rhs } => {
                Stm::Assign { ty: ty.clone(), lhs: *lhs, rhs: Box::new(rhs.lambda_lift(fresh_name_generator, decls)) }
            },
            hircc::Stm::ArrayAssign { bounds_check, ty, array, index, value } => {
                Stm::ArrayAssign { bounds_check: *bounds_check, ty: ty.clone(), array: Box::new(array.lambda_lift(fresh_name_generator, decls)), index: Box::new(index.lambda_lift(fresh_name_generator, decls)), value: Box::new(value.lambda_lift(fresh_name_generator, decls)) }
            },
            hircc::Stm::StructAssign { ty, base, field, value } => {
                Stm::StructAssign { ty: ty.clone(), base: Box::new(base.lambda_lift(fresh_name_generator, decls)), field: *field, value: Box::new(value.lambda_lift(fresh_name_generator, decls)) }
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hir::trees::*;
    use crate::hir::ops::*;
    use crate::common::names::*;

    #[test]
    fn test_church_true() {
        let h = Root {
            defs: vec![
                // var true = \x -> \y -> x
                Def::VarDef {
                    ty: Type::Fun { ret: Box::new(Type::Fun { ret: Box::new(Type::I32), args: vec![Type::I32] }), args: vec![Type::I32] },
                    name: Name::new("true"),
                    exp: Box::new(
                        Exp::Lambda {
                            ret_type: Type::Fun { ret: Box::new(Type::I32), args: vec![Type::I32] },
                            params: vec![
                                Param {
                                    name: Name::new("x"), ty: Type::I32
                                }
                            ],
                            body: Box::new(
                                Exp::Lambda {
                                    ret_type: Type::I32,
                                    params: vec![
                                        Param {
                                            name: Name::new("y"), ty: Type::I32
                                        }
                                    ],
                                    body: Box::new(Exp::Var { name: Name::new("x"), ty: Type::I32 })
                                }
                            )
                        }
                    )
                }
             ]
        };

        let expected = Root {
            defs: vec![
                Def::VarDef {
                    ty: Type::Struct {
                        fields: vec![
                            Param {
                                ty: Type::Fun {
                                    ret: Box::new(Type::Struct {
                                        fields: vec![
                                            Param {
                                                ty: Type::Fun {
                                                    ret: Box::new(Type::I32),
                                                    args: vec![
                                                        Type::I32,
                                                        Type::Struct {
                                                            fields: vec![]
                                                        }
                                                    ]
                                                },
                                                name: Name::new("fun")
                                            },
                                            Param {
                                                ty: Type::Struct {
                                                    fields: vec![]
                                                },
                                                name: Name::new("env")
                                            }
                                        ]
                                    }),
                                    args: vec![
                                        Type::I32,
                                        Type::Struct {
                                            fields: vec![]
                                        }
                                    ]
                                },
                                name: Name::new("fun")
                            },
                            Param {
                                ty: Type::Struct {
                                    fields: vec![]
                                },
                                name: Name::new("env")
                            }
                        ]
                    },
                    name: Name::new("true"),
                    exp: Box::new(Exp::StructLit {
                        fields: vec![
                            Field {
                                param: Param {
                                    ty: Type::Fun {
                                        ret: Box::new(Type::Struct {
                                            fields: vec![
                                                Param {
                                                    ty: Type::Fun {
                                                        ret: Box::new(Type::I32),
                                                        args: vec![
                                                            Type::I32,
                                                            Type::Struct {
                                                                fields: vec![]
                                                            }
                                                        ]
                                                    },
                                                    name: Name::new("fun")
                                                },
                                                Param {
                                                    ty: Type::Struct {
                                                        fields: vec![]
                                                    },
                                                    name: Name::new("env")
                                                }
                                            ]
                                        }),
                                        args: vec![
                                            Type::I32,
                                            Type::Struct {
                                                fields: vec![]
                                            }
                                        ]
                                    },
                                    name: Name::new("fun")
                                },
                                exp: Box::new(Exp::Function {
                                    name: Name::new("lifted.cc.2"),
                                    ty: Type::Fun {
                                        ret: Box::new(Type::Struct {
                                            fields: vec![
                                                Param {
                                                    ty: Type::Fun {
                                                        ret: Box::new(Type::I32),
                                                        args: vec![
                                                            Type::I32,
                                                            Type::Struct {
                                                                fields: vec![]
                                                            }
                                                        ]
                                                    },
                                                    name: Name::new("fun")
                                                },
                                                Param {
                                                    ty: Type::Struct {
                                                        fields: vec![]
                                                    },
                                                    name: Name::new("env")
                                                }
                                            ]
                                        }),
                                        args: vec![
                                            Type::I32,
                                            Type::Struct {
                                                fields: vec![]
                                            }
                                        ]
                                    }
                                })
                            },
                            Field {
                                param: Param {
                                    ty: Type::Struct {
                                        fields: vec![]
                                    },
                                    name: Name::new("env")
                                },
                                exp: Box::new(Exp::Cast {
                                    ty: Type::Struct {
                                        fields: vec![]
                                    },
                                    exp: Box::new(Exp::StructLit {
                                        fields: vec![]
                                    })
                                })
                            }
                        ]
                    })
                },
                Def::FunDef {
                    ret_type: Type::I32,
                    name: Name::new("lifted.cc.4"),
                    params: vec![
                        Param {
                            ty: Type::I32,
                            name: Name::new("y")
                        },
                        Param {
                            ty: Type::Struct {
                                fields: vec![]
                            },
                            name: Name::new("env.cc.5")
                        }
                    ],
                    body: Box::new(Exp::Let {
                        inits: vec![
                            Field {
                                param: Param {
                                    ty: Type::Struct {
                                        fields: vec![
                                            Param {
                                                ty: Type::I32,
                                                name: Name::new("x")
                                            }
                                        ]
                                    },
                                    name: Name::new("env.cc.1")
                                },
                                exp: Box::new(Exp::Cast {
                                    ty: Type::Struct {
                                        fields: vec![
                                            Param {
                                                ty: Type::I32,
                                                name: Name::new("x")
                                            }
                                        ]
                                    },
                                    exp: Box::new(Exp::Var {
                                        name: Name::new("env.cc.5"),
                                        ty: Type::Struct {
                                            fields: vec![]
                                        }
                                    })
                                })
                            }
                        ],
                        body: Box::new(Exp::StructLoad {
                            ty: Type::Struct {
                                fields: vec![
                                    Param {
                                        ty: Type::I32,
                                        name: Name::new("x")
                                    }
                                ]
                            },
                            base: Box::new(Exp::Var {
                                name: Name::new("env.cc.1"),
                                ty: Type::Struct {
                                    fields: vec![
                                        Param {
                                            ty: Type::I32,
                                            name: Name::new("x")
                                        }
                                    ]
                                }
                            }),
                            field: Name::new("x")
                        })
                    })
                },
                Def::FunDef {
                    ret_type: Type::Struct {
                        fields: vec![
                            Param {
                                ty: Type::Fun {
                                    ret: Box::new(Type::I32),
                                    args: vec![
                                        Type::I32,
                                        Type::Struct {
                                            fields: vec![]
                                        }
                                    ]
                                },
                                name: Name::new("fun")
                            },
                            Param {
                                ty: Type::Struct {
                                    fields: vec![]
                                },
                                name: Name::new("env")
                            },
                        ]
                    },
                    name: Name::new("lifted.cc.2"),
                    params: vec![
                        Param {
                            ty: Type::I32,
                            name: Name::new("x")
                        },
                        Param {
                            ty: Type::Struct {
                                fields: vec![]
                            },
                            name: Name::new("env.cc.3")
                        }
                    ],
                    body: Box::new(Exp::Let {
                        inits: vec![
                            Field {
                                param: Param {
                                    ty: Type::Struct {
                                        fields: vec![]
                                    },
                                    name: Name::new("env.cc.0")
                                },
                                exp: Box::new(Exp::Cast {
                                    ty: Type::Struct {
                                        fields: vec![]
                                    },
                                    exp: Box::new(Exp::Var {
                                        name: Name::new("env.cc.3"),
                                        ty: Type::Struct {
                                            fields: vec![]
                                        }
                                    })
                                })
                            }
                        ],
                        body: Box::new(Exp::StructLit {
                            fields: vec![
                                Field {
                                    param: Param {
                                        ty: Type::Fun {
                                            ret: Box::new(Type::I32),
                                            args: vec![
                                                Type::I32,
                                                Type::Struct {
                                                    fields: vec![]
                                                }
                                            ]
                                        },
                                        name: Name::new("fun")
                                    },
                                    exp: Box::new(Exp::Function {
                                        name: Name::new("lifted.cc.4"),
                                        ty: Type::Fun {
                                            ret: Box::new(Type::I32),
                                            args: vec![
                                                Type::I32,
                                                Type::Struct {
                                                    fields: vec![]
                                                }
                                            ]
                                        }
                                    })
                                },
                                Field {
                                    param: Param {
                                        ty: Type::Struct {
                                            fields: vec![]
                                        },
                                        name: Name::new("env")
                                    },
                                    exp: Box::new(Exp::Cast {
                                        ty: Type::Struct {
                                            fields: vec![]
                                        },
                                        exp: Box::new(Exp::StructLit {
                                            fields: vec![
                                                Field {
                                                    param: Param {
                                                        ty: Type::I32,
                                                        name: Name::new("x")
                                                    },
                                                    exp: Box::new(Exp::Var {
                                                        name: Name::new("x"),
                                                        ty: Type::I32
                                                    })
                                                }
                                            ]
                                        })
                                    })
                                }
                            ]
                        })
                    })
                }
            ]
        };

        let lifted = LambdaLift::lambda_lift(&h);
        assert_eq!(lifted, expected);
    }

    #[test]
    fn test_church_false() {
        let h = Root {
            defs: vec![
                // var false = \x -> \y -> y
                Def::VarDef {
                    ty: Type::Fun { ret: Box::new(Type::Fun { ret: Box::new(Type::I32), args: vec![Type::I32] }), args: vec![Type::I32] },
                    name: Name::new("false"),
                    exp: Box::new(
                        Exp::Lambda {
                            ret_type: Type::Fun { ret: Box::new(Type::I32), args: vec![Type::I32] },
                            params: vec![
                                Param {
                                    name: Name::new("x"), ty: Type::I32
                                }
                            ],
                            body: Box::new(
                                Exp::Lambda {
                                    ret_type: Type::I32,
                                    params: vec![
                                        Param {
                                            name: Name::new("y"), ty: Type::I32
                                        }
                                    ],
                                    body: Box::new(Exp::Var { name: Name::new("y"), ty: Type::I32 })
                                }
                            )
                        }
                    )
                }
             ]
        };

        let expected = Root {
            defs: vec![
                Def::VarDef {
                    ty: Type::Struct {
                        fields: vec![
                            Param {
                                ty: Type::Fun {
                                    ret: Box::new(Type::Struct {
                                        fields: vec![
                                            Param {
                                                ty: Type::Fun {
                                                    ret: Box::new(Type::I32),
                                                    args: vec![
                                                        Type::I32,
                                                        Type::Struct {
                                                            fields: vec![]
                                                        }
                                                    ]
                                                },
                                                name: Name::new("fun")
                                            },
                                            Param {
                                                ty: Type::Struct {
                                                    fields: vec![]
                                                },
                                                name: Name::new("env")
                                            }
                                        ]
                                    }),
                                    args: vec![
                                        Type::I32,
                                        Type::Struct {
                                            fields: vec![]
                                        }
                                    ]
                                },
                                name: Name::new("fun")
                            },
                            Param {
                                ty: Type::Struct {
                                    fields: vec![]
                                },
                                name: Name::new("env")
                            }
                        ]
                    },
                    name: Name::new("false"),
                    exp: Box::new(Exp::StructLit {
                        fields: vec![
                            Field {
                                param: Param {
                                    ty: Type::Fun {
                                        ret: Box::new(Type::Struct {
                                            fields: vec![
                                                Param {
                                                    ty: Type::Fun {
                                                        ret: Box::new(Type::I32),
                                                        args: vec![
                                                            Type::I32,
                                                            Type::Struct {
                                                                fields: vec![]
                                                            }
                                                        ]
                                                    },
                                                    name: Name::new("fun")
                                                },
                                                Param {
                                                    ty: Type::Struct {
                                                        fields: vec![]
                                                    },
                                                    name: Name::new("env")
                                                }
                                            ]
                                        }),
                                        args: vec![
                                            Type::I32,
                                            Type::Struct {
                                                fields: vec![]
                                            }
                                        ]
                                    },
                                    name: Name::new("fun")
                                },
                                exp: Box::new(Exp::Function {
                                    name: Name::new("lifted.cc.2"),
                                    ty: Type::Fun {
                                        ret: Box::new(Type::Struct {
                                            fields: vec![
                                                Param {
                                                    ty: Type::Fun {
                                                        ret: Box::new(Type::I32),
                                                        args: vec![
                                                            Type::I32,
                                                            Type::Struct {
                                                                fields: vec![]
                                                            }
                                                        ]
                                                    },
                                                    name: Name::new("fun")
                                                },
                                                Param {
                                                    ty: Type::Struct {
                                                        fields: vec![]
                                                    },
                                                    name: Name::new("env")
                                                }
                                            ]
                                        }),
                                        args: vec![
                                            Type::I32,
                                            Type::Struct {
                                                fields: vec![]
                                            }
                                        ]
                                    }
                                })
                            },
                            Field {
                                param: Param {
                                    ty: Type::Struct {
                                        fields: vec![]
                                    },
                                    name: Name::new("env")
                                },
                                exp: Box::new(Exp::Cast {
                                    ty: Type::Struct {
                                        fields: vec![]
                                    },
                                    exp: Box::new(Exp::StructLit {
                                        fields: vec![]
                                    })
                                })
                            }
                        ]
                    })
                },
                Def::FunDef {
                    ret_type: Type::I32,
                    name: Name::new("lifted.cc.4"),
                    params: vec![
                        Param {
                            ty: Type::I32,
                            name: Name::new("y")
                        },
                        Param {
                            ty: Type::Struct {
                                fields: vec![]
                            },
                            name: Name::new("env.cc.5")
                        }
                    ],
                    body: Box::new(Exp::Let {
                        inits: vec![
                            Field {
                                param: Param {
                                    ty: Type::Struct {
                                        fields: vec![]
                                    },
                                    name: Name::new("env.cc.1")
                                },
                                exp: Box::new(Exp::Cast {
                                    ty: Type::Struct {
                                        fields: vec![]
                                    },
                                    exp: Box::new(Exp::Var {
                                        name: Name::new("env.cc.5"),
                                        ty: Type::Struct {
                                            fields: vec![]
                                        }
                                    })
                                })
                            }
                        ],
                        body: Box::new(Exp::Var {
                            ty: Type::I32,
                            name: Name::new("y")
                        })
                    })
                },
                Def::FunDef {
                    ret_type: Type::Struct {
                        fields: vec![
                            Param {
                                ty: Type::Fun {
                                    ret: Box::new(Type::I32),
                                    args: vec![
                                        Type::I32,
                                        Type::Struct {
                                            fields: vec![]
                                        }
                                    ]
                                },
                                name: Name::new("fun")
                            },
                            Param {
                                ty: Type::Struct {
                                    fields: vec![]
                                },
                                name: Name::new("env")
                            },
                        ]
                    },
                    name: Name::new("lifted.cc.2"),
                    params: vec![
                        Param {
                            ty: Type::I32,
                            name: Name::new("x")
                        },
                        Param {
                            ty: Type::Struct {
                                fields: vec![]
                            },
                            name: Name::new("env.cc.3")
                        }
                    ],
                    body: Box::new(Exp::Let {
                        inits: vec![
                            Field {
                                param: Param {
                                    ty: Type::Struct {
                                        fields: vec![]
                                    },
                                    name: Name::new("env.cc.0")
                                },
                                exp: Box::new(Exp::Cast {
                                    ty: Type::Struct {
                                        fields: vec![]
                                    },
                                    exp: Box::new(Exp::Var {
                                        name: Name::new("env.cc.3"),
                                        ty: Type::Struct {
                                            fields: vec![]
                                        }
                                    })
                                })
                            }
                        ],
                        body: Box::new(Exp::StructLit {
                            fields: vec![
                                Field {
                                    param: Param {
                                        ty: Type::Fun {
                                            ret: Box::new(Type::I32),
                                            args: vec![
                                                Type::I32,
                                                Type::Struct {
                                                    fields: vec![]
                                                }
                                            ]
                                        },
                                        name: Name::new("fun")
                                    },
                                    exp: Box::new(Exp::Function {
                                        name: Name::new("lifted.cc.4"),
                                        ty: Type::Fun {
                                            ret: Box::new(Type::I32),
                                            args: vec![
                                                Type::I32,
                                                Type::Struct {
                                                    fields: vec![]
                                                }
                                            ]
                                        }
                                    })
                                },
                                Field {
                                    param: Param {
                                        ty: Type::Struct {
                                            fields: vec![]
                                        },
                                        name: Name::new("env")
                                    },
                                    exp: Box::new(Exp::Cast {
                                        ty: Type::Struct {
                                            fields: vec![]
                                        },
                                        exp: Box::new(Exp::StructLit {
                                            fields: vec![]
                                        })
                                    })
                                }
                            ]
                        })
                    })
                }
            ]
        };

        let lifted = LambdaLift::lambda_lift(&h);
        assert_eq!(lifted, expected);
    }

    #[test]
    fn test_id() {
        let h = Root {
            defs: vec![
                // var id = \x -> x
                Def::VarDef {
                    ty: Type::Fun { ret: Box::new(Type::I32), args: vec![Type::I32] },
                    name: Name::new("id"),
                    exp: Box::new(
                        Exp::Lambda {
                            ret_type: Type::I32,
                            params: vec![
                                Param {
                                    name: Name::new("x"), ty: Type::I32
                                }
                            ],
                            body: Box::new(Exp::Var { name: Name::new("x"), ty: Type::I32 })
                        }
                    )
                }
             ]
        };

        let expected = Root {
            defs: vec![
                Def::VarDef {
                    ty: Type::Struct {
                        fields: vec![
                            Param {
                                ty: Type::Fun {
                                    ret: Box::new(Type::I32),
                                    args: vec![
                                        Type::I32,
                                        Type::Struct {
                                            fields: vec![]
                                        }
                                    ]
                                },
                                name: Name::new("fun")
                            },
                            Param {
                                ty: Type::Struct {
                                    fields: vec![]
                                },
                                name: Name::new("env")
                            }
                        ]
                    },
                    exp: Box::new(
                        Exp::StructLit {
                            fields: vec![
                                Field {
                                    param: Param {
                                        ty: Type::Fun {
                                            ret: Box::new(Type::I32),
                                            args: vec![
                                                Type::I32,
                                                Type::Struct {
                                                    fields: vec![]
                                                }
                                            ]
                                        },
                                        name: Name::new("fun")
                                    },
                                    exp: Box::new(Exp::Function {
                                        name: Name::new("lifted.cc.1"),
                                        ty: Type::Fun {
                                            ret: Box::new(Type::I32),
                                            args: vec![
                                                Type::I32,
                                                Type::Struct {
                                                    fields: vec![]
                                                }
                                            ]
                                        }
                                    })
                                },
                                Field {
                                    param: Param {
                                        ty: Type::Struct {
                                            fields: vec![]
                                        },
                                        name: Name::new("env")
                                    },
                                    exp: Box::new(Exp::Cast {
                                        ty: Type::Struct {
                                            fields: vec![]
                                        },
                                        exp: Box::new(Exp::StructLit {
                                            fields: vec![]
                                        })
                                    })
                                }
                            ]
                        }
                    ),
                    name: Name::new("id"),
                },
                Def::FunDef {
                    ret_type: Type::I32,
                    params: vec![
                        Param {
                            name: Name::new("x"), ty: Type::I32,
                        },
                        Param {
                            name: Name::new("env.cc.2"), ty: Type::Struct { fields: vec![] },
                        }
                    ],
                    name: Name::new("lifted.cc.1"),
                    body: Box::new(Exp::Let {
                        inits: vec![
                            Field {
                                param: Param {
                                    ty: Type::Struct {
                                        fields: vec![]
                                    },
                                    name: Name::new("env.cc.0")
                                },
                                exp: Box::new(Exp::Cast {
                                    ty: Type::Struct {
                                        fields: vec![]
                                    },
                                    exp: Box::new(Exp::Var {
                                        name: Name::new("env.cc.2"),
                                        ty: Type::Struct {
                                            fields: vec![]
                                        }
                                    })
                                })
                            }
                        ],
                        body: Box::new(Exp::Var {
                            name: Name::new("x"),
                            ty: Type::I32
                        })
                    })
                 }
            ]
        };

        let lifted = LambdaLift::lambda_lift(&h);
        assert_eq!(lifted, expected);
    }

    // TODO: add tests where we CALL the function!
}
