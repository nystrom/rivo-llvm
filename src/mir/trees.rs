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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Param {
    pub ty: Type,
    pub name: Name,
}

#[derive(Clone, Debug)]
pub struct Proc {
    pub ret_type: Type,
    pub name: Name,
    pub params: Vec<Param>,
    pub body: Box<Exp>
}

#[derive(Clone, Debug)]
pub enum Stm {
    Nop,

    // Following LLVM, we don't fall-through to the next instruction, but have two branch targets.
    CJump { cond: Box<Exp>, if_true: Name, if_false: Name },
    Jump { label: Name },
    Label { label: Name },

    Move { ty: Type, lhs: Name, rhs: Box<Exp> },
    Store { ty: Type, ptr: Box<Exp>, value: Box<Exp> },

    // Early return
    Return { exp: Box<Exp> },

    // StructStore { struct_ty: Type, ptr: Box<Exp>, field: usize, value: Box<Exp> },
    // ArrayStore { base_ty: Type, ptr: Box<Exp>, index: Box<Exp>, value: Box<Exp> },
}

#[derive(Clone, Debug)]
pub enum Exp {
    Block { body: Vec<Stm>, exp: Box<Exp> },

    Call { fun_type: Type, fun: Box<Exp>, args: Vec<Exp> },

    Load { ty: Type, ptr: Box<Exp> },

    Binary { op: Bop, e1: Box<Exp>, e2: Box<Exp> },
    Unary { op: Uop, exp: Box<Exp> },
    Cast { ty: Type, exp: Box<Exp> },

    Lit { lit: Lit },

    Function { name: Name, ty: Type },
    Global { name: Name, ty: Type },
    Temp { name: Name, ty: Type },

    // Address of a struct field entry.
    GetStructElementAddr { struct_ty: Type, ptr: Box<Exp>, field: usize },

    // Address of an array entry.
    GetArrayElementAddr { base_ty: Type, ptr: Box<Exp>, index: Box<Exp> },

    // Address of the array length field.
    GetArrayLengthAddr { ptr: Box<Exp> },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    I1, // bool
    I32,
    I64,
    F32,
    F64,
    Ptr { ty: Box<Type> },        // LLVM: ty*
    Array { ty: Box<Type> },      // LLVM: { i32, [0 x ty] }
    Struct { fields: Vec<Type> }, // LLVM: { .. }
    Fun { ret: Box<Type>, args: Vec<Type> },
    Void,
    Word,  // i32 or i64 depending on array index size
}

#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
    I1 { value: bool },
    I32 { value: i32 },
    I64 { value: i64 },
    F32 { value: f32 },
    F64 { value: f64 },
    Sizeof { ty: Type },
    ArrayBaseOffset,      // i32 or i64 depending on array index size (should = 0)
    ArrayLengthOffset,    // i32 or i64 depending on array index size (should = sizeof(Word))
    StructFieldOffset { ty: Type, field: usize }, // word size
}
