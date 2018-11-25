use crate::common::names::*;
use crate::mir::trees as mir;

pub fn alloc() -> mir::Exp {
    mir::Exp::Global {
        name: Name::new("malloc"),
        ty: mir::Type::FunPtr,
    }
}

pub fn panic() -> mir::Exp {
    mir::Exp::Global {
        name: Name::new("panic"),
        ty: mir::Type::FunPtr,
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

    mir::Exp::Global {
        name: Name::new(name),
        ty: mir::Type::FunPtr,
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

    mir::Exp::Global {
        name: Name::new(name),
        ty: mir::Type::FunPtr,
    }
}
