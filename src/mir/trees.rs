use crate::common::names::Name;

use crate::mir::ops::*;

#[derive(Clone, Debug)]
pub struct Root {
    pub externs: Vec<Param>,
    pub data: Vec<Data>,
    pub procs: Vec<Proc>
}

#[derive(Clone, Debug)]
pub struct Data {
    pub ty: Type,
    pub name: Name,
    pub init: Lit,
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

    // ty should be the type of rhs
    Move { ty: Type, lhs: Name, rhs: Box<Exp> },
    // ty should be the type of value
    // ptr should have type Ptr { ty }
    Store { ty: Type, ptr: Box<Exp>, value: Box<Exp> },

    // Early return
    Return { exp: Box<Exp> },

    // StructStore { struct_ty: Type, ptr: Box<Exp>, field: usize, value: Box<Exp> },
    // ArrayStore { base_ty: Type, ptr: Box<Exp>, index: Box<Exp>, value: Box<Exp> },
}

#[derive(Clone, Debug)]
pub enum Exp {
    Block { body: Vec<Stm>, exp: Box<Exp> },

    // fun_type should be a Fun type (not a Ptr { Fun })
    Call { fun_type: Type, fun: Box<Exp>, args: Vec<Exp> },

    // ptr should have type Ptr { ty }
    Load { ty: Type, ptr: Box<Exp> },

    Binary { op: Bop, e1: Box<Exp>, e2: Box<Exp> },
    Unary { op: Uop, exp: Box<Exp> },
    Cast { ty: Type, exp: Box<Exp> },

    Lit { lit: Lit },

    // ty should be a ptr to fun type
    FunctionAddr { name: Name, ty: Type },
    // ty should be a ptr to a non-fun type (but might be a closure struct)
    GlobalAddr { name: Name, ty: Type },
    Temp { name: Name, ty: Type },

    // Address of a struct field entry.
    // ptr has type Ptr { struct_ty }
    GetStructElementAddr { struct_ty: Type, ptr: Box<Exp>, field: usize },

    // Address of an array entry.
    // ptr has type Ptr { Array { base_ty } }
    GetArrayElementAddr { base_ty: Type, ptr: Box<Exp>, index: Box<Exp> },

    // Address of the array length field.
    // ptr has type Ptr { Array { ?? } }
    GetArrayLengthAddr { ptr: Box<Exp> },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    I1, // bool
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,

    Void,
    Word,  // i32 or i64 depending on array index size

    Ptr { ty: Box<Type> },        // LLVM: ty*

    // Struct types (usually wrapped in Ptr)
    Array { ty: Box<Type> },      // LLVM: { i32, [0 x ty] }
    Struct { fields: Vec<Type> }, // LLVM: { .. }

    // Function types (usually wrapped in Ptr)
    Fun { ret: Box<Type>, args: Vec<Type> },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
    Void,
    Null { ty: Type },
    I1 { value: bool },
    I8 { value: i8 },
    I16 { value: i16 },
    I32 { value: i32 },
    I64 { value: i64 },
    F32 { value: f32 },
    F64 { value: f64 },
    Sizeof { ty: Type },
    ArrayBaseOffset,      // i32 or i64 depending on array index size (should = 0)
    ArrayLengthOffset,    // i32 or i64 depending on array index size (should = sizeof(Word))
    StructFieldOffset { ty: Type, field: usize }, // word size
}
