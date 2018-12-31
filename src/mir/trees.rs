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
    // ptr should have type IRef { ty }
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

    // ptr should have type IRef { ty }
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
    // ptr has type Ref { struct_ty }, result is type IRef
    GetStructElementAddr { struct_ty: Type, ptr: Box<Exp>, field: usize },

    // Address of an array entry.
    // We've already used GetStructElementAddr to get an IRef to the internal array.
    // ptr has type [I]Ref { Array { base_ty } }, result is type IRef
    GetArrayElementAddr { base_ty: Type, ptr: Box<Exp>, index: Box<Exp> },

    New { ty: Type },
    NewHybrid { ty: Type, length: Box<Exp> },
}

// MIR types are based on MuVM types, but simplified
// int<n> Fixed-size integer type of n bits
// float IEEE754 single-precision (32-bit) floating-point type
// double IEEE754 double-precision (32-bit) floating-point type
// uptr<T> Untraced pointer to a memory location of type T
// ufuncptr<sig> Untraced pointer to a native function with signature sig
// struct<T1 T2 . . .> Structure with fields T1 T2 . . .
// hybrid<F1 F2 . . . V> A hybrid with fixed-part fields F1 F2 . . . and variable part type V
// array<T n> Fixed-size array of element type T and length n
// vector<T n> Vector type of element type T and length n
// ref<T> Object reference to a heap object of type T
// iref<T> Internal reference to a memory location of type T
// weakref<T> Weak object reference to a heap object of type T
// funcref<sig> Function reference to a Mu function with signature sig
// stackref Opaque reference to a Mu stack
// threadref Opaque reference to a Mu thread
// framecursorref Opaque reference to a Mu frame cursor (see Section 6.3.3)
// irbuilderref Opaque reference to a Mu IR builder
// tagref64 64-bit tagged reference
// void Void type
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

    // internal reference to a field
    IRef { ty: Box<Type> },       // LLVM: ty*
    // object reference
    Ref { ty: Box<Type> },        // LLVM: ty*
    // untraced pointer
    Ptr { ty: Box<Type> },        // LLVM: ty*

    // Fixed fields, then 0 or more of variant type
    // Hybrid is the only dynamically sized type.
    Hybrid { fields: Vec<Type>, variant: Box<Type> },
    Struct { fields: Vec<Type> }, // LLVM: { .. }
    // like a struct, but with overlapping fields... translates into the a struct
    Union { variants: Vec<Type> }, // LLVM: { .. }

    // Function types (usually wrapped in Ptr)
    Fun { ret: Box<Type>, args: Vec<Type> },
}

impl Type {
    /// This is the word size of the target architecture.
    /// It should be either I32 or I64. Other code may panic if this is not true.
    /// The word size gives the array length and array index size and the field offset size.
    /// For simplicity, structs on the heap are allocated to be word size * number of fields.
    pub fn word() -> Type {
        Type::I64
    }
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
}
