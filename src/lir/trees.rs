use crate::mir::ops::*;
use crate::common::names::*;

// These are the same as mir trees, so just use them.
pub use crate::mir::trees::Lit;
pub use crate::mir::trees::Param;
pub use crate::mir::trees::Type;

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

    CJump { cmp: Exp, if_true: Name, if_false: Name },
    Jump { label: Name },
    Ret { exp: Exp },

    Store { dst_addr: Exp, src: Exp },
    Load { dst: Name, src_addr: Exp },
    Move { dst: Name, src: Exp },

    Call { dst: Name, fun: Exp, args: Vec<Exp> },

    Binary { dst: Name, op: Bop, e1: Exp, e2: Exp },
    Unary { dst: Name, op: Uop, exp: Exp },

    // Bitcast
    Cast { dst: Name, ty: Type, exp: Exp },

    Label { label: Name },

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
