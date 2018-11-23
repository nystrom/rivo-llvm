use crate::common::names::Name;

use crate::hir::ops::*;

#[derive(Clone, Debug)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    Bool,
    Void,
    Array { ty: Box<Type> },
    Struct { fields: Vec<Param> },
    Fun { ty: Box<Type>, params: Vec<Type> },
    Box,
    FunPtr,
    EnvPtr,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub ty: Type,
    pub name: Name,
}

#[derive(Copy, Clone, Debug)]
pub enum Lit {
    I32 { value: i32 },
    I64 { value: i64 },
    F32 { value: f32 },
    F64 { value: f64 },
    Bool { value: bool },
    Void,
}

#[derive(Clone, Debug)]
pub struct Root {
    pub defs: Vec<Def>
}

#[derive(Clone, Debug)]
pub enum Def {
    VarDef { ty: Type, name: Name, exp: Box<Exp> },
    FunDef { ty: Type, name: Name, params: Vec<Param>, body: Box<Stm> },
    ExternDef { ty: Type, name: Name, params: Vec<Param> },
}

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

    // Used for let too.
    Seq { body: Box<Stm>, exp: Box<Exp> },

    // Before lambda lifting.
    Let { param: Param, init: Box<Exp>, body: Box<Exp> },
    Lambda { params: Vec<Param>, body: Box<Exp> },
    Apply { fun: Box<Exp>, args: Vec<Exp> },

    // Structs
    // These are tagged in Ivo, but we make the tag an explicit field in HIR.
    StructLit { ty: Type, fields: Vec<Field> },
    StructLoad { ty: Type, base: Box<Exp>, field: Name },
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
