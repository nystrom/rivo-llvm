use crate::common::names::Name;

use crate::mir::ops::*;

#[derive(Clone, Debug)]
pub struct Root {
    pub data: Vec<Data>,
    pub procs: Vec<Proc>
}

#[derive(Clone, Debug)]
pub struct Data {
    pub ty: Type,
    pub name: Name,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub ty: Type,
    pub name: Name,
}

#[derive(Clone, Debug)]
pub struct Proc {
    pub ty: Type,
    pub name: Name,
    pub params: Vec<Param>,
    pub body: Box<Exp>
}

#[derive(Clone, Debug)]
pub enum Stm {
    Nop,
    Error { message: String },

    // Following LLVM, we don't fall-through to the next instruction, but have two branch targets.
    CJump { cond: Box<Exp>, if_true: Name, if_false: Name },
    Jump { label: Name },
    Label { label: Name },

    Move { ty: Type, lhs: Name, rhs: Box<Exp> },
    Store { ty: Type, ptr: Box<Exp>, value: Box<Exp> }

    // StructStore { struct_ty: Type, ptr: Box<Exp>, field: usize, value: Box<Exp> },
    // ArrayStore { base_ty: Type, ptr: Box<Exp>, index: Box<Exp>, value: Box<Exp> },
}

#[derive(Clone, Debug)]
pub struct Address {
    pub base: Box<Exp>,
    pub offset: i32,
}

#[derive(Clone, Debug)]
pub enum Exp {
    Block { body: Vec<Stm>, exp: Box<Exp> },

    // FIXME: typing
    Call { fun: Box<Exp>, args: Vec<Exp> },

    // Allocate a struct
    StructAlloc { ty: Type },
    ArrayAlloc { base_ty: Type, length: Box<Exp> },

    // Address of a struct field entry.
    StructAddr { struct_ty: Type, ptr: Box<Exp>, field: usize },
    // Address of an array entry.
    ArrayAddr { base_ty: Type, ptr: Box<Exp>, index: Box<Exp> },
    // Address of an array entry.
    ArrayLengthAddr { ptr: Box<Exp> },

    Load { ty: Type, ptr: Box<Exp> },

    // StructLoad { struct_ty: Type, ptr: Box<Exp>, field: usize },
    // ArrayLoad { base_ty: Type, ptr: Box<Exp>, index: Box<Exp> },

    Binary { op: Bop, e1: Box<Exp>, e2: Box<Exp> },
    Unary { op: Uop, exp: Box<Exp> },

    Lit { lit: Lit },

    Global { label: Name, ty: Name },
    Temp { name: Name, ty: Type },
}

#[derive(Clone, Debug)]
pub enum Type {
    I1, // bool
    I32,
    I64,
    F32,
    F64,
    Ptr { ty: Box<Type> },        // LLVM: ty*
    Array { ty: Box<Type> },      // LLVM: { i32, [0 x ty] }
    Struct { fields: Vec<Type> }, // LLVM: { .. }
    Void,
    EnvPtr, // LLVM: void*
    FunPtr, // LLVM: void*
}

#[derive(Copy, Clone, Debug)]
pub enum Lit {
    I1 { value: bool },
    I32 { value: i32 },
    I64 { value: i64 },
    F32 { value: f32 },
    F64 { value: f64 },
}
