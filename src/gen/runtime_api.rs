use crate::common::names::*;
use crate::mir::trees as mir;
use super::mir_gen;

pub fn alloc() -> mir::Exp {
    let byte_ptr = mir::Type::Ptr { ty: Box::new(mir::Type::I8) };
    mir::Exp::FunctionAddr {
        name: Name::new("malloc"),
        ty: mir::Type::Ptr { ty: Box::new(mir::Type::Fun { ret: Box::new(byte_ptr), args: vec![mir::Type::Word] }) },
    }
}

pub fn panic() -> mir::Exp {
    mir::Exp::FunctionAddr {
        name: Name::new("panic"),
        ty: mir::Type::Ptr { ty: Box::new(mir::Type::Fun { ret: Box::new(mir::Type::Void), args: vec![] }) },
    }
}

pub fn boxer(ty: &mir::Type) -> mir::Exp {
    let name = match ty {
        mir::Type::I1  => "box_bool",
        mir::Type::I32 => "box_i32",
        mir::Type::I64 => "box_i64",
        mir::Type::F32 => "box_f32",
        mir::Type::F64 => "box_f64",
        _ => unimplemented!()
    };

    let byte_ptr = mir::Type::Ptr { ty: Box::new(mir::Type::I8) };

    mir::Exp::FunctionAddr {
        name: Name::new(name),
        ty: mir::Type::Ptr { ty: Box::new(mir::Type::Fun { ret: Box::new(byte_ptr), args: vec![ty.clone()] }) },
    }
}

pub fn unboxer(ty: &mir::Type) -> mir::Exp {
    let name = match ty {
        mir::Type::I1  => "unbox_bool",
        mir::Type::I32 => "unbox_i32",
        mir::Type::I64 => "unbox_i64",
        mir::Type::F32 => "unbox_f32",
        mir::Type::F64 => "unbox_f64",
        _ => unimplemented!()
    };

    let byte_ptr = mir::Type::Ptr { ty: Box::new(mir::Type::I8) };

    mir::Exp::FunctionAddr {
        name: Name::new(name),
        ty: mir::Type::Ptr { ty: Box::new(mir::Type::Fun { ret: Box::new(ty.clone()), args: vec![byte_ptr] }) },
    }
}
