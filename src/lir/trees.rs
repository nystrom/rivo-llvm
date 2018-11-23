use crate::mir::ops::*;
use crate::mir::trees::Lit;
use crate::mir::trees::Param;
use crate::mir::trees::Type;
use crate::common::names::*;

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
pub struct Proc {
    pub ty: Type,
    pub name: Name,
    pub params: Vec<Param>,
    pub body: Vec<Stm>
}

#[derive(Clone, Debug)]
pub enum Stm {
    Nop,

    CJump { cmp: Cmp, e1: Exp, e2: Exp, if_true: Name, if_false: Name },
    Jump { label: Name },
    Ret { exp: Exp },

    Store { dst_addr: Exp, src: Exp },
    Load { dst: Name, src_addr: Exp },
    Move { dst: Name, src: Exp },

    Call { dst: Name, fun: Exp, args: Vec<Exp> },

    Binary { dst: Name, op: Bop, e1: Exp, e2: Exp },
    Unary { dst: Name, op: Uop, exp: Exp },

    Label { label: Name },

    // These should be calls, but we don't know the sizes before LLVM generation,
    // so just leave as is.
    StructAlloc { dst: Name, ty: Type },
    ArrayAlloc { dst: Name, base_ty: Type, length: Exp },

    // Address of a struct field entry.
    GetStructElementAddr { dst: Name, struct_ty: Type, ptr: Exp, field: usize },

    // Address of an array entry.
    GetArrayElementAddr { dst: Name, base_ty: Type, ptr: Exp, index: Exp },

    // Address of the array length field.
    GetArrayLengthAddr { dst: Name, ptr: Exp },
}

#[derive(Clone, Debug)]
pub enum Exp {
    Global { name: Name },
    Temp { name: Name },
    Lit { lit: Lit },
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug)]
pub enum Cmp {
    Eq_i32, Ne_i32, Lt_i32, Gt_i32, Le_i32, Ge_i32,
    Eq_i64, Ne_i64, Lt_i64, Gt_i64, Le_i64, Ge_i64,
    Eq_f32, Ne_f32, Lt_f32, Gt_f32, Le_f32, Ge_f32,
    Eq_f64, Ne_f64, Lt_f64, Gt_f64, Le_f64, Ge_f64,
}
