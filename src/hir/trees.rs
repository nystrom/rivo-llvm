use crate::common::names::Name;

use crate::hir::ops::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    Bool,
    Void,

    // Pointer types
    Array { ty: Box<Type> },
    Struct { fields: Vec<Param> },
    Fun { ret: Box<Type>, args: Vec<Type> },

    // Boxed/tagged values.
    // To use a boxed value, one must explicitly unbox.
    Box,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Param {
    pub ty: Type,
    pub name: Name,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Lit {
    I32 { value: i32 },
    I64 { value: i64 },
    F32 { value: f32 },
    F64 { value: f64 },
    Bool { value: bool },
}

#[derive(Clone, Debug)]
pub struct Root {
    pub defs: Vec<Def>
}

#[derive(Clone, Debug)]
pub enum Def {
    VarDef { ty: Type, name: Name, exp: Box<Exp> },
    FunDef { ret_type: Type, name: Name, params: Vec<Param>, body: Box<Exp> },
    ExternDef { ret_type: Type, name: Name, params: Vec<Param> },
}

#[derive(Clone, Debug)]
pub enum Exp {
    NewArray { ty: Type, length: Box<Exp> },
    ArrayLit { ty: Type, exps: Vec<Exp> },
    ArrayLoad { bounds_check: bool, ty: Type, array: Box<Exp>, index: Box<Exp> },
    ArrayLength { array: Box<Exp> },

    Lit { lit: Lit },
    Call { fun_type: Type, name: Name, args: Vec<Exp> },
    Var { name: Name, ty: Type },

    Binary { op: Bop, e1: Box<Exp>, e2: Box<Exp> },
    Unary { op: Uop, exp: Box<Exp> },

    Seq { body: Box<Stm>, exp: Box<Exp> },

    // Before lambda lifting.
    Let { param: Param, init: Box<Exp>, body: Box<Exp> },
    Lambda { ret_type: Type, params: Vec<Param>, body: Box<Exp> },
    Apply { fun_type: Type, fun: Box<Exp>, args: Vec<Exp> },

    // Structs
    // These are tagged in Ivo, but we make the tag an explicit field in HIR.
    StructLit { fields: Vec<Field> },
    StructLoad { ty: Type, base: Box<Exp>, field: Name },

    // Convert to and from boxed values.
    Box { ty: Type, exp: Box<Exp> },
    Unbox { ty: Type, exp: Box<Exp> },

    // Unchecked cast from one type to another.
    // Should only be used for pointer types.
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
