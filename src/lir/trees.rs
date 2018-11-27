// TODO
// Change to be as close as possible to LLVM.
// Basically we're just defining a builder-less API for LLVM.
// Procs are a vec of BB.
// Each BB has a label.
// Add SSA and support all LLVM types.

use crate::mir::ops::*;
use crate::common::names::*;

// These are the same as mir trees, so just use them.
pub use crate::mir::trees::Lit;
pub use crate::mir::trees::Param;
pub use crate::mir::trees::Type;

pub use crate::mir::trees::Typed;

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
    pub ret_type: Type,
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
    Load { dst: Exp, src_addr: Exp },
    Move { dst: Exp, src: Exp },

    Call { dst: Exp, fun: Exp, args: Vec<Exp> },

    Binary { dst: Exp, op: Bop, e1: Exp, e2: Exp },
    Unary { dst: Exp, op: Uop, exp: Exp },

    // Bitcast
    Cast { dst: Exp, ty: Type, exp: Exp },

    Label { label: Name },

    // Address of a struct field entry.
    GetStructElementAddr { dst: Exp, struct_ty: Type, ptr: Exp, field: usize },

    // Address of an array entry.
    GetArrayElementAddr { dst: Exp, base_ty: Type, ptr: Exp, index: Exp },

    // Address of the array length field.
    GetArrayLengthAddr { dst: Exp, ptr: Exp },
}

#[derive(Clone, Debug)]
pub enum Exp {
    Function { ty: Type, name: Name },
    Global { ty: Type, name: Name },
    Temp { ty: Type, name: Name },
    Lit { lit: Lit },
}

impl Typed for Exp {
    fn get_type(&self) -> Type {
        match self {
            Exp::Function { ty, name } => ty.clone(),
            Exp::Global { ty, name } => ty.clone(),
            Exp::Temp { ty, name } => ty.clone(),
            Exp::Lit { lit } => lit.get_type(),
        }
    }
}
