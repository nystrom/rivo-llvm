#![allow(non_camel_case_types)]

#[derive(Clone, Copy, Debug)]
pub enum Uop {
    // Boolean ops
    Not_z,

    // Ops that eval to i32
    Ctz_i32,
    Clz_i32,
    Popcount_i32,
    Eqz_i32,
    Complement_i32,

    // Ops that eval to i64
    Ctz_i64,
    Clz_i64,
    Popcount_i64,
    Eqz_i64,
    Complement_i64,

    // Ops that eval to f32
    Neg_f32,
    Abs_f32,

    Ceil_f32,    // round toward pos
    Floor_f32,   // round toward neg
    Trunc_f32,   // round toward zero
    Nearest_f32, // round to nearest, ties to even

    Exp_f32,
    Log_f32,
    Sqrt_f32,
    Pow_f32,
    Logb_f32,
    Sin_f32,
    Cos_f32,
    Tan_f32,
    Asin_f32,
    Acos_f32,
    Atan_f32,
    Sinh_f32,
    Cosh_f32,
    Tanh_f32,
    Asinh_f32,
    Acosh_f32,
    Atanh_f32,

    IsNan_f32,
    IsInf_f32,
    IsDenormalized_f32,
    IsNegativeZero_f32,
    IsIEEE_f32,

    // Ops that eval to f64
    Neg_f64,
    Abs_f64,

    Ceil_f64,    // round toward pos
    Floor_f64,   // round toward neg
    Trunc_f64,   // round toward zero
    Nearest_f64, // round to nearest, ties to even

    Exp_f64,
    Log_f64,
    Sqrt_f64,
    Pow_f64,
    Logb_f64,
    Sin_f64,
    Cos_f64,
    Tan_f64,
    Asin_f64,
    Acos_f64,
    Atan_f64,
    Sinh_f64,
    Cosh_f64,
    Tanh_f64,
    Asinh_f64,
    Acosh_f64,
    Atanh_f64,

    IsNan_f64,
    IsInf_f64,
    IsDenormalized_f64,
    IsNegativeZero_f64,
    IsIEEE_f64,

    // Conversions
    Wrap_i64_i32,

    Trunc_s_f32_i32,
    Trunc_s_f64_i32,
    Trunc_u_f32_i32,
    Trunc_u_f64_i32,
    Trunc_s_f32_i64,
    Trunc_s_f64_i64,
    Trunc_u_f32_i64,
    Trunc_u_f64_i64,

    Extend_s_i32_i64,
    Extend_u_i32_i64,

    Reinterpret_i32_f32,
    Reinterpret_f32_i32,
    Reinterpret_f64_i64,
    Reinterpret_i64_f64,

    Convert_s_i32_f32,
    Convert_u_i32_f32,
    Convert_s_i64_f32,
    Convert_u_i64_f32,
    Convert_s_i32_f64,
    Convert_u_i32_f64,
    Convert_s_i64_f64,
    Convert_u_i64_f64,

    Demote_f64_f32,
    Promote_f32_f64,
}

#[derive(Clone, Copy, Debug)]
pub enum Bop {
    // These are either equivalent to Add/Mul_i32 or Add/Mul_i64, but left abstract here.
    Add_word,
    Mul_word,

    // Boolean ops
    And_z,
    Or_z,
    Eq_z,
    Ne_z,

    // Pointer ops
    Eq_ptr,
    Ne_ptr,

    Add_i32,
    Sub_i32,
    Mul_i32,
    Div_s_i32,
    Div_u_i32,
    Rem_s_i32,
    Rem_u_i32,

    And_i32,
    Or_i32,
    Xor_i32,

    Eq_i32,
    Ne_i32,
    Lt_s_i32,
    Lt_u_i32,
    Le_s_i32,
    Le_u_i32,
    Gt_s_i32,
    Gt_u_i32,
    Ge_s_i32,
    Ge_u_i32,

    Shl_i32,
    Shr_i32,
    Shr_u_i32,
    Rotl_i32,
    Rotr_i32,

    Add_i64,
    Sub_i64,
    Mul_i64,
    Div_s_i64,
    Div_u_i64,
    Rem_s_i64,
    Rem_u_i64,

    And_i64,
    Or_i64,
    Xor_i64,

    Eq_i64,
    Ne_i64,
    Lt_s_i64,
    Lt_u_i64,
    Le_s_i64,
    Le_u_i64,
    Gt_s_i64,
    Gt_u_i64,
    Ge_s_i64,
    Ge_u_i64,

    Shl_i64,
    Shr_i64,
    Shr_u_i64,
    Rotl_i64,
    Rotr_i64,

    Min_f32,
    Max_f32,
    // magnitude of left, sign of right (abs x == copysign x 1)
    Copysign_f32,

    Atan2_f32,

    Add_f32,
    Sub_f32,
    Mul_f32,
    Div_f32,
    Rem_f32,

    // Compare ops always eval to an i32
    Eq_f32,
    Ne_f32,
    Lt_f32,
    Le_f32,
    Gt_f32,
    Ge_f32,

    Min_f64,
    Max_f64,
    // magnitude of left, sign of right (abs x == copysign x 1)
    Copysign_f64,

    Atan2_f64,

    Add_f64,
    Sub_f64,
    Mul_f64,
    Div_f64,
    Rem_f64,

    // Compare ops always eval to an i32
    Eq_f64,
    Ne_f64,
    Lt_f64,
    Le_f64,
    Gt_f64,
    Ge_f64,
}
