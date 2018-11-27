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
    EnvPtr, // LLVM: void*
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




pub trait Typed {
    fn get_type(&self) -> Type;
}

impl Typed for Stm {
    fn get_type(&self) -> Type { Type::Void }
}

impl Typed for Exp {
    fn get_type(&self) -> Type {
        match self {
            Exp::Block { body, exp } => exp.get_type(),
            Exp::Call { fun_type: Type::Fun { box ret, .. }, fun, args } => ret.clone(),
            Exp::Load { ty, ptr } => ty.clone(),
            Exp::Binary { op, e1, e2 } => op.get_type(),
            Exp::Unary { op, exp } => op.get_type(),
            Exp::Cast { ty, exp } => ty.clone(),
            Exp::Lit { lit } => lit.get_type(),
            Exp::Function { name, ty } => ty.clone(),
            Exp::Global { name, ty } => ty.clone(),
            Exp::Temp { name, ty } => ty.clone(),
            Exp::GetStructElementAddr { struct_ty: Type::Struct { fields }, ptr, field } => {
                match fields.get(*field) {
                    Some(ty) => Type::Ptr { ty: Box::new(ty.clone()) },
                    _ => panic!("ill-typed expression {:?}", self)
                }
            },
            Exp::GetArrayElementAddr { base_ty, ptr, index } => Type::Ptr { ty: Box::new(base_ty.clone()) },
            Exp::GetArrayLengthAddr { ptr } => Type::Ptr { ty: Box::new(Type::Word) },
            _ => panic!("ill-typed expression {:?}", self)
        }
    }
}
impl Typed for Lit {
    fn get_type(&self) -> Type {
        match self {
            Lit::I1 { value } => Type::I1,
            Lit::I32 { value } => Type::I32,
            Lit::I64 { value } => Type::I64,
            Lit::F32 { value } => Type::F32,
            Lit::F64 { value } => Type::F64,
            Lit::Sizeof { ty } => Type::Word,
            Lit::ArrayBaseOffset => Type::Word,
            Lit::ArrayLengthOffset => Type::Word,
            Lit::StructFieldOffset { ty, field } => Type::Word,
        }
    }
}

impl Typed for Uop {
    fn get_type(&self) -> Type {
        match self {
            Uop::Not_z => Type::I1,

            Uop::Ctz_i32 => Type::I32,
            Uop::Clz_i32 => Type::I32,
            Uop::Popcount_i32 => Type::I32,
            Uop::Eqz_i32 => Type::I32,
            Uop::Complement_i32 => Type::I32,

            Uop::Ctz_i64 => Type::I64,
            Uop::Clz_i64 => Type::I64,
            Uop::Popcount_i64 => Type::I64,
            Uop::Eqz_i64 => Type::I64,
            Uop::Complement_i64 => Type::I64,

            Uop::Neg_f32 => Type::F32,
            Uop::Abs_f32 => Type::F32,
            Uop::Ceil_f32 => Type::F32,    // round toward pos
            Uop::Floor_f32 => Type::F32,   // round toward neg
            Uop::Trunc_f32 => Type::F32,   // round toward zero
            Uop::Nearest_f32 => Type::F32, // round to nearest, ties to even

            Uop::Exp_f32 => Type::F32,
            Uop::Log_f32 => Type::F32,
            Uop::Sqrt_f32 => Type::F32,
            Uop::Pow_f32 => Type::F32,
            Uop::Logb_f32 => Type::F32,
            Uop::Sin_f32 => Type::F32,
            Uop::Cos_f32 => Type::F32,
            Uop::Tan_f32 => Type::F32,
            Uop::Asin_f32 => Type::F32,
            Uop::Acos_f32 => Type::F32,
            Uop::Atan_f32 => Type::F32,
            Uop::Sinh_f32 => Type::F32,
            Uop::Cosh_f32 => Type::F32,
            Uop::Tanh_f32 => Type::F32,
            Uop::Asinh_f32 => Type::F32,
            Uop::Acosh_f32 => Type::F32,
            Uop::Atanh_f32 => Type::F32,

            Uop::IsNan_f32 => Type::I1,
            Uop::IsInf_f32 => Type::I1,
            Uop::IsDenormalized_f32 => Type::I1,
            Uop::IsNegativeZero_f32 => Type::I1,
            Uop::IsIEEE_f32 => Type::I1,

            Uop::Neg_f64 => Type::F64,
            Uop::Abs_f64 => Type::F64,
            Uop::Ceil_f64 => Type::F64,    // round toward pos
            Uop::Floor_f64 => Type::F64,   // round toward neg
            Uop::Trunc_f64 => Type::F64,   // round toward zero
            Uop::Nearest_f64 => Type::F64, // round to nearest, ties to even
            Uop::Exp_f64 => Type::F64,
            Uop::Log_f64 => Type::F64,
            Uop::Sqrt_f64 => Type::F64,
            Uop::Pow_f64 => Type::F64,
            Uop::Logb_f64 => Type::F64,
            Uop::Sin_f64 => Type::F64,
            Uop::Cos_f64 => Type::F64,
            Uop::Tan_f64 => Type::F64,
            Uop::Asin_f64 => Type::F64,
            Uop::Acos_f64 => Type::F64,
            Uop::Atan_f64 => Type::F64,
            Uop::Sinh_f64 => Type::F64,
            Uop::Cosh_f64 => Type::F64,
            Uop::Tanh_f64 => Type::F64,
            Uop::Asinh_f64 => Type::F64,
            Uop::Acosh_f64 => Type::F64,
            Uop::Atanh_f64 => Type::F64,

            Uop::IsNan_f64 => Type::I1,
            Uop::IsInf_f64 => Type::I1,
            Uop::IsDenormalized_f64 => Type::I1,
            Uop::IsNegativeZero_f64 => Type::I1,
            Uop::IsIEEE_f64 => Type::I1,

            Uop::Wrap_i64_i32 => Type::I32,
            Uop::Trunc_s_f32_i32 => Type::I32,
            Uop::Trunc_s_f64_i32 => Type::I32,
            Uop::Trunc_u_f32_i32 => Type::I32,
            Uop::Trunc_u_f64_i32 => Type::I32,
            Uop::Trunc_s_f32_i64 => Type::I64,
            Uop::Trunc_s_f64_i64 => Type::I64,
            Uop::Trunc_u_f32_i64 => Type::I64,
            Uop::Trunc_u_f64_i64 => Type::I64,
            Uop::Extend_s_i32_i64 => Type::I64,
            Uop::Extend_u_i32_i64 => Type::I64,
            Uop::Reinterpret_i32_f32 => Type::F32,
            Uop::Reinterpret_f32_i32 => Type::I32,
            Uop::Reinterpret_f64_i64 => Type::I64,
            Uop::Reinterpret_i64_f64 => Type::F64,
            Uop::Convert_s_i32_f32 => Type::F32,
            Uop::Convert_u_i32_f32 => Type::F32,
            Uop::Convert_s_i64_f32 => Type::F32,
            Uop::Convert_u_i64_f32 => Type::F32,
            Uop::Convert_s_i32_f64 => Type::F64,
            Uop::Convert_u_i32_f64 => Type::F64,
            Uop::Convert_s_i64_f64 => Type::F64,
            Uop::Convert_u_i64_f64 => Type::F64,
            Uop::Demote_f64_f32 => Type::F32,
            Uop::Promote_f32_f64 => Type::F64,
        }
    }
}

impl Typed for Bop {
    fn get_type(&self) -> Type {
        match self {
            Bop::Add_word => Type::Word,
            Bop::Mul_word => Type::Word,

            Bop::And_z => Type::I1,
            Bop::Or_z => Type::I1,
            Bop::Eq_z => Type::I1,
            Bop::Ne_z => Type::I1,

            Bop::Add_i32 => Type::I32,
            Bop::Sub_i32 => Type::I32,
            Bop::Mul_i32 => Type::I32,
            Bop::Div_s_i32 => Type::I32,
            Bop::Div_u_i32 => Type::I32,
            Bop::Rem_s_i32 => Type::I32,
            Bop::Rem_u_i32 => Type::I32,

            Bop::And_i32 => Type::I32,
            Bop::Or_i32 => Type::I32,
            Bop::Xor_i32 => Type::I32,

            Bop::Eq_i32 => Type::I1,
            Bop::Ne_i32 => Type::I1,
            Bop::Lt_s_i32 => Type::I1,
            Bop::Lt_u_i32 => Type::I1,
            Bop::Le_s_i32 => Type::I1,
            Bop::Le_u_i32 => Type::I1,
            Bop::Gt_s_i32 => Type::I1,
            Bop::Gt_u_i32 => Type::I1,
            Bop::Ge_s_i32 => Type::I1,
            Bop::Ge_u_i32 => Type::I1,

            Bop::Shl_i32 => Type::I32,
            Bop::Shr_i32 => Type::I32,
            Bop::Shr_u_i32 => Type::I32,
            Bop::Rotl_i32 => Type::I32,
            Bop::Rotr_i32 => Type::I32,

            Bop::Add_i64 => Type::I64,
            Bop::Sub_i64 => Type::I64,
            Bop::Mul_i64 => Type::I64,
            Bop::Div_s_i64 => Type::I64,
            Bop::Div_u_i64 => Type::I64,
            Bop::Rem_s_i64 => Type::I64,
            Bop::Rem_u_i64 => Type::I64,

            Bop::And_i64 => Type::I64,
            Bop::Or_i64 => Type::I64,
            Bop::Xor_i64 => Type::I64,

            Bop::Eq_i64 => Type::I1,
            Bop::Ne_i64 => Type::I1,
            Bop::Lt_s_i64 => Type::I1,
            Bop::Lt_u_i64 => Type::I1,
            Bop::Le_s_i64 => Type::I1,
            Bop::Le_u_i64 => Type::I1,
            Bop::Gt_s_i64 => Type::I1,
            Bop::Gt_u_i64 => Type::I1,
            Bop::Ge_s_i64 => Type::I1,
            Bop::Ge_u_i64 => Type::I1,

            Bop::Shl_i64 => Type::I64,
            Bop::Shr_i64 => Type::I64,
            Bop::Shr_u_i64 => Type::I64,
            Bop::Rotl_i64 => Type::I64,
            Bop::Rotr_i64 => Type::I64,

            Bop::Min_f32 => Type::F32,
            Bop::Max_f32 => Type::F32,
            Bop::Copysign_f32 => Type::F32,

            Bop::Atan2_f32 => Type::F32,

            Bop::Add_f32 => Type::F32,
            Bop::Sub_f32 => Type::F32,
            Bop::Mul_f32 => Type::F32,
            Bop::Div_f32 => Type::F32,
            Bop::Rem_f32 => Type::F32,

            Bop::Eq_f32 => Type::I1,
            Bop::Ne_f32 => Type::I1,
            Bop::Lt_f32 => Type::I1,
            Bop::Le_f32 => Type::I1,
            Bop::Gt_f32 => Type::I1,
            Bop::Ge_f32 => Type::I1,

            Bop::Min_f64 => Type::F64,
            Bop::Max_f64 => Type::F64,
            Bop::Copysign_f64 => Type::F64,

            Bop::Atan2_f64 => Type::F64,

            Bop::Add_f64 => Type::F64,
            Bop::Sub_f64 => Type::F64,
            Bop::Mul_f64 => Type::F64,
            Bop::Div_f64 => Type::F64,
            Bop::Rem_f64 => Type::F64,

            Bop::Eq_f64 => Type::I1,
            Bop::Ne_f64 => Type::I1,
            Bop::Lt_f64 => Type::I1,
            Bop::Le_f64 => Type::I1,
            Bop::Gt_f64 => Type::I1,
            Bop::Ge_f64 => Type::I1,
        }
    }
}
