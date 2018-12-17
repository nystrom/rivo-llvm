use super::trees::*;
use super::ops::*;

pub trait Typed {
    fn get_type(&self) -> Type;
}

impl Typed for Stm {
    fn get_type(&self) -> Type {
        // Check the types.
        match self {
            Stm::Nop => {},
            Stm::CJump { cond, if_true, if_false } => {},
            Stm::Jump { label } => {},
            Stm::Label { label } => {},
            Stm::Move { ty, lhs, rhs } => {
                // ty should be the type of rhs
                assert_eq!(ty, &rhs.get_type())
            },
            Stm::Store { ty, ptr, value } => {
                // ty should be the type of value
                // ptr should have type Ptr { ty }
                let vty = value.get_type();
                let pty = ptr.get_type();
                assert_eq!(ty, &vty);
                assert_eq!(Type::Ptr { ty: Box::new(ty.clone()) }, pty);
            },
            Stm::Return { exp } => {},
        }

        Type::Void
    }
}

// Compute the type of each expression.
// Also asserts that the types are consistent.
impl Typed for Exp {
    fn get_type(&self) -> Type {
        let ty = match self {
            Exp::Block { body, exp } => {
                for s in body {
                    assert_eq!(s.get_type(), Type::Void);
                }
                exp.get_type()
            },
            Exp::Call { fun_type: Type::Fun { box ret, args: arg_types }, fun, args } => {
                assert_eq!(fun.get_type(), Type::Ptr { ty: Box::new(Type::Fun { ret: Box::new(ret.clone()), args: arg_types.clone() }) });
                let actual_types: Vec<Type> = args.iter().map(|a| a.get_type()).collect();
                assert_eq!(arg_types, &actual_types);
                ret.clone()
            },
            Exp::Call { fun_type, fun, args } => {
                panic!("call function type must be a function type, got {:?}", fun_type)
            },
            Exp::Load { ty, ptr } => {
                assert_eq!(ptr.get_type(), Type::Ptr { ty: Box::new(ty.clone()) });
                ty.clone()
            }
            Exp::Binary { op, e1, e2 } => op.get_type(),
            Exp::Unary { op, exp } => op.get_type(),
            Exp::Cast { ty, exp } => {
                // Both should be pointer types.
                assert!(match exp.get_type() { Type::Ptr { .. } => true, _ => false }, "can only cast from ptr types, got {:?}", exp.get_type());
                assert!(match ty { Type::Ptr { .. } => true, _ => false }, "can only cast to ptr types, got {:?}", exp.get_type());
                ty.clone()
            },
            Exp::Lit { lit } => lit.get_type(),
            Exp::FunctionAddr { name, ty } => {
                // Functions should not have function type.
                assert!(match ty { Type::Ptr { ty: box Type::Fun { .. } } => true, _ => false }, "function variables should have fun ptr type, got {:?}", ty);
                ty.clone()
            },
            Exp::GlobalAddr { name, ty } => {
                // Globals should not have function type.
                assert!(match ty { Type::Ptr { ty: box Type::Fun { .. } } => false, _ => true }, "global variables cannot be fun ptr type, got {:?}", ty);
                assert!(match ty { Type::Fun { .. } => false, _ => true }, "global variables cannot be fun type, got {:?}", ty);
                ty.clone()
            },
            Exp::Temp { name, ty } => {
                assert!(match ty { Type::Fun { .. } => false, _ => true }, "temporary variables cannot be fun type, got {:?}", ty);
                ty.clone()
            },
            Exp::GetStructElementAddr { struct_ty: Type::Struct { fields }, ptr, field } => {
                assert_eq!(Type::Ptr { ty: Box::new(Type::Struct { fields: fields.clone() }) }, ptr.get_type());
                match fields.get(*field) {
                    Some(ty) => Type::Ptr { ty: Box::new(ty.clone()) },
                    _ => panic!("ill-typed expression {:#?}, field {} does not exist", self, field)
                }
            },
            Exp::GetStructElementAddr { struct_ty, ptr, field } => {
                panic!("struct accessor must have struct type, got {:?}", struct_ty)
            },
            Exp::GetArrayElementAddr { base_ty, ptr, index } => {
                assert_eq!(Type::Ptr { ty: Box::new(Type::Array { ty: Box::new(base_ty.clone()) }) }, ptr.get_type());
                Type::Ptr { ty: Box::new(base_ty.clone()) }
            },
            Exp::GetArrayLengthAddr { ptr } => Type::Ptr { ty: Box::new(Type::word()) },
        };

        // match ty {
        //     Type::Ptr { ty: box Type::Ptr { .. } } => panic!("found pointer to pointer {:#?} in {:#?}", ty, self),
        //     _ => {},
        // }

        ty
    }
}
impl Typed for Lit {
    fn get_type(&self) -> Type {
        match self {
            Lit::Null { ty } => {
                assert!(match ty { Type::Ptr { .. } => true, _ => false }, "null literals must have pointer type, got {:?}", ty);
                ty.clone()
            },
            Lit::Void => Type::Void,
            Lit::I1 { value } => Type::I1,
            Lit::I8 { value } => Type::I8,
            Lit::I16 { value } => Type::I16,
            Lit::I32 { value } => Type::I32,
            Lit::I64 { value } => Type::I64,
            Lit::F32 { value } => Type::F32,
            Lit::F64 { value } => Type::F64,
            Lit::Sizeof { ty } => Type::word(),
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
            Bop::And_z => Type::I1,
            Bop::Or_z => Type::I1,
            Bop::Eq_z => Type::I1,
            Bop::Ne_z => Type::I1,

            Bop::Eq_ptr => Type::I1,
            Bop::Ne_ptr => Type::I1,

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
